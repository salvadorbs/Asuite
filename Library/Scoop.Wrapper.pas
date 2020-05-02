{
Copyright (C) 2006-2020 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Scoop.Wrapper;

interface

uses
  classes, PJPipe, PJConsoleApp, PJPipeFilters, SysUtils, Vcl.Forms,
  Scoop.Bucket, Scoop.App, System.StrUtils,
  System.Generics.Defaults, System.Generics.Collections, Winapi.Windows,
  MPcommonObjects, JsonDataObjects, MPCommonUtilities, Scoop.List.Buckets,
  Scoop.List.Apps;

//TODO: Move const in another unit
const
  SCOOP_CMD = 'cmd.exe /c scoop';
  SCOOP_ARGS_STATUS = ' status';
  SCOOP_ARGS_APP_LIST = ' list';
  SCOOP_ARGS_BUCKET_LIST = ' bucket list';
  SCOOP_ARGS_BUCKET_KNOWN = ' bucket known';
  SCOOP_ARGS_BUCKET_ADD = ' bucket add ';
  SCOOP_ARGS_BUCKET_REMOVE = ' bucket rm ';
  SCOOP_ARGS_INSTALL = ' install ';
  SCOOP_ARGS_UNINSTALL = ' uninstall ';
  SCOOP_ARGS_UPDATE = ' update ';

type

  TOnEndLoadingBucketApps = procedure(Sender: TObject; const Apps: TScoopApps)
    of object;

  TScoopWrapper = class
  private
    { private declarations }
    FConsoleApp: TPJConsoleApp;
    FOutput: TStringList;
    FOutPipe: TPJPipe;
    FErrPipe: TPJPipe;
    FOutFilter: TPJAnsiSBCSPipeFilter;
    FOnEndLoadingBucketApps: TOnEndLoadingBucketApps;
    FOnOutputLineEnd: TPJAnsiTextReadEvent;

    FBuckets: TScoopBuckets;
    FBucketsKnown: TScoopBuckets;

    procedure LineEndHandler(Sender: TObject; const Text: AnsiString);
    procedure WorkHandler(Sender: TObject);
    procedure CompleteHandler(Sender: TObject);
    function InternalExecute(Args: string; OutputZero: boolean = False): boolean; overload;
    function InternalExecute(): boolean; overload;
    procedure OnEndLoadingBucketApps(Sender: TObject; const Apps: TScoopApps);

    procedure LoadListAppsInstalled();
    //TODO: Create a class-wrapper to parse install.json
    function GetBucketNameFromInstallJson(AppName: string): string;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create();
    destructor Destroy(); override;

    property Output: TStringList read FOutput;
    property Buckets: TScoopBuckets read FBuckets;
    property BucketsKnown: TScoopBuckets read FBucketsKnown;
    property OnEndLoadingApps: TOnEndLoadingBucketApps read FOnEndLoadingBucketApps
      write FOnEndLoadingBucketApps;
    property OnOutputLineEnd: TPJAnsiTextReadEvent read FOnOutputLineEnd write FOnOutputLineEnd;

    function IsScoopExists(): boolean;
    function UpdateScoop(): boolean;
    function UpdateApp(AName: string): boolean;
    function InstallApp(AName: string): Boolean;
    function UninstallApp(AName: string): Boolean;
    procedure GetStatus();
    function AddBucket(AName: string): Boolean;
    function RemoveBucket(AName: string): Boolean;

    function LoadListBuckets(): TList<TScoopBucket>;
    function LoadListBucketsKnown(): TList<TScoopBucket>;
    procedure LoadAllApps();
  end;

const
  STATUS_MSG_UPDATE = 'Updates are available for:';
  STATUS_MSG_OUTDATED = 'These apps are outdated and on hold:';
  STATUS_MSG_MISSMANIFEST = 'These app manifests have been removed:';
  STATUS_MSG_FAILED = 'These apps failed to install:';
  STATUS_MSG_MISSDEPENDECIES = 'Missing runtime dependencies:';

  arrScoopStatus: array of string = [STATUS_MSG_UPDATE, STATUS_MSG_OUTDATED,
    STATUS_MSG_MISSMANIFEST, STATUS_MSG_FAILED, STATUS_MSG_MISSDEPENDECIES];

implementation

uses
  System.RegularExpressions, Path.Utils;

{ TScoopWrapper }

constructor TScoopWrapper.Create();
begin
  inherited;
  FConsoleApp := TPJConsoleApp.Create;
  FOutPipe := TPJPipe.Create;
  FErrPipe := TPJPipe.Create;
  FOutput := TStringList.Create;
  FOutFilter := TPJAnsiSBCSPipeFilter.Create(FOutPipe, True);

  FOutFilter.OnLineEnd := LineEndHandler;

  FConsoleApp.MaxExecTime := INFINITE;
  FConsoleApp.TimeSlice := 5;
  FConsoleApp.Visible := false;
  FConsoleApp.StdOut := FOutPipe.WriteHandle;
  FConsoleApp.StdErr := FErrPipe.WriteHandle;
  FConsoleApp.OnWork := WorkHandler;
  FConsoleApp.OnComplete := CompleteHandler;

  FBuckets := TScoopBuckets.Create();
  FBucketsKnown := TScoopBuckets.Create();
end;

destructor TScoopWrapper.Destroy();
begin
  FConsoleApp.Free;
  FOutFilter.Free;
  FErrPipe.Free;
  FOutput.Free;

  FBuckets.Free;
  FBucketsKnown.Free;
end;

procedure TScoopWrapper.LoadListAppsInstalled;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
  App: TScoopApp;
  AppName: string;
  BucketName: string;
begin
  try
    FBuckets.ClearInstalledApps;

    FOutFilter.EOLMarker := #10;
    isExecuted := Self.InternalExecute(SCOOP_ARGS_APP_LIST);
    if (isExecuted) then
    begin
      //Regexp to get info (name, version and bucket name) from query "Scoop list"
      //We can have two types of output line:
      //  App 9.0
      //  AnotherApp 1.2.123 [bucket]
      Regexp := TRegEx.Create('(\S+) (\S+)(?:\s\[(.*)\])?$');

      //Parse FOutput to find app name
      for I := 1 to FOutput.Count - 1 do
      begin
        AppName := '';
        BucketName := '';
        Match := Regexp.Match(FOutput[I]);

        //Get app info
        if (Match.Success) and (Match.Groups.Count > 1) then
        begin
          //Get app and bucket name
          AppName := Match.Groups[1].Value;

          //If regexp found 3 groups, we have bucket name else it is bucket main's app
          if Match.Groups.Count > 3 then
            BucketName := Match.Groups[3].Value
          else
            BucketName := 'main';

          App := FBuckets.AddApp(Match.Groups[1].Value, BucketName);

          if Assigned(App) then
          begin
            App.Installed := True;
            App.VersionInstalled := Match.Groups[2].Value;

            //TODO: Parse app's manifest.json to get App.InstalledVersion
            //TODO: Parse install.json to get other app info (arch?)
          end;
        end;
      end;
    end;

  except
    raise;
  end;
end;

procedure TScoopWrapper.LoadAllApps;
begin
  //First get buckets from scoop executable
  Self.LoadListBuckets;

  //After get only apps installed from scoop executable
  Self.LoadListAppsInstalled;

  //Now we can get other apps, searching using TScoopBuckets
  FBuckets.LoadAllApps;
end;

function TScoopWrapper.GetBucketNameFromInstallJson(AppName: string): string;
var
  Obj: TJsonObject;
  Path: string;
begin
  Path := ExpandEnvVars('%SCOOP%\apps\' + AppName + '\current\install.json');
  if FileExists(Path) then
  begin
    Obj := TJsonObject.ParseFromFile(Path) as TJsonObject;
    try
      Result := Obj.S['bucket'];
    finally
      Obj.Free;
    end;
  end;
end;

function TScoopWrapper.LoadListBuckets(): TList<TScoopBucket>;
var
  isExecuted: boolean;
  I: Integer;
  Bucket: TScoopBucket;
begin
  try

    FOutFilter.EOLMarker := #13#10;
    isExecuted := Self.InternalExecute(SCOOP_ARGS_BUCKET_LIST);
    if (isExecuted) then
    begin
      FBuckets.Clear;

      for I := 0 to FOutput.Count - 1 do
      begin
        Bucket := FBuckets.AddBucket(FOutput[I]);
        Bucket.OnEndLoadingApps := OnEndLoadingBucketApps;
      end;
    end;

    Result := FBuckets;
  except
    raise;
  end;
end;

function TScoopWrapper.LoadListBucketsKnown: TList<TScoopBucket>;
var
  isExecuted: boolean;
  I: Integer;
begin
  try

    FOutFilter.EOLMarker := #13#10;

    isExecuted := Self.InternalExecute(SCOOP_ARGS_BUCKET_KNOWN);
    if (isExecuted) then
    begin
      FBucketsKnown.Clear;

      for I := 0 to FOutput.Count - 1 do
        FBucketsKnown.Add(TScoopBucket.Create(FOutput[I]))
    end;

    Result := FBucketsKnown;
  except
    raise;
  end;
end;

procedure TScoopWrapper.OnEndLoadingbucketApps(Sender: TObject;
  const Apps: TScoopApps);
begin
  if Assigned(FOnEndLoadingBucketApps) then
    FOnEndLoadingBucketApps(Sender, Apps);
end;

function TScoopWrapper.InstallApp(AName: string): Boolean;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
begin
  Result := false;

  FOutFilter.EOLMarker := #10;

  try
    isExecuted := Self.InternalExecute(SCOOP_ARGS_INSTALL + AName);
    if (isExecuted) then
    begin

      //TODO: Check scoop updating, too?
      //TODO: Manage warning error 'WARN  'appname' (x.y.z) is already installed.'
      Regexp := TRegEx.Create('.* was installed successfully!');

      for I := 1 to FOutput.Count - 1 do
      begin

        Match := Regexp.Match(FOutput[I]);

        if (Match.Success) then
        begin
          Result := True;
          break
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TScoopWrapper.InternalExecute: boolean;
begin
  Result := Self.InternalExecute('');
end;

function TScoopWrapper.InternalExecute(Args: string; OutputZero: boolean = False): boolean;
var
  isExecuted: boolean;
begin
  FOutput.Clear;

  isExecuted := FConsoleApp.Execute(SCOOP_CMD + Args);

  Result := (isExecuted) and (FConsoleApp.ExitCode = 0) and (OutputZero or (FOutput.Count > 0));
end;

function TScoopWrapper.IsScoopExists(): boolean;
begin
  try

    Result := Self.InternalExecute();

  except
    raise;
  end;
end;

procedure TScoopWrapper.WorkHandler(Sender: TObject);
begin
  FOutFilter.ReadPipe;

  if Assigned(FOnOutputLineEnd) then
    Application.ProcessMessages;
end;

procedure TScoopWrapper.LineEndHandler(Sender: TObject; const Text: AnsiString);
begin
  if Text <> '' then
  begin
    FOutput.Add(string(Text));

    if Assigned(FOnOutputLineEnd) then
      FOnOutputLineEnd(Sender, Text);
  end;
end;

function TScoopWrapper.RemoveBucket(AName: string): Boolean;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
begin
  Result := False;

  FOutFilter.EOLMarker := #10;

  try
    isExecuted := Self.InternalExecute(SCOOP_ARGS_BUCKET_REMOVE + AName, true);
    if (isExecuted) then
    begin

      Regexp := TRegEx.Create('''.*'' bucket not found\.');

      if FOutput.Count = 0 then
        Result := True
      else
        for I := 1 to FOutput.Count - 1 do
        begin

          Match := Regexp.Match(FOutput[I]);

          if (Match.Success) then
          begin
            Result := False;
            break
          end;
        end;
    end;
  except
    raise;
  end;
end;

procedure TScoopWrapper.GetStatus;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
  TrackNextLines: boolean;
  App: TScoopApp;
begin
  try

    FOutFilter.EOLMarker := #10;
    isExecuted := Self.InternalExecute(SCOOP_ARGS_STATUS);
    if (isExecuted) then
    begin
      // Regexp for "appname: cur_version -> new_version"
      Regexp := TRegEx.Create('(\S+). (.+) -> (.+)');

      // Get new app version
      for I := 1 to FOutput.Count - 1 do
      begin
        // If current line is update status, we can track next lines
        TrackNextLines := (IndexStr(FOutput[I], arrScoopStatus) = 0);

        if TrackNextLines then
        begin

          Match := Regexp.Match(FOutput[I]);

          if (Match.Success) and (Match.Groups.Count > 1) then
          begin
            App := FBuckets.SearchAppByName(Match.Groups[1].Value);
            App.VersionInstalled := Match.Groups[2].Value;
            App.VersionLatest := Match.Groups[3].Value;
          end;
        end;
      end;
    end;

  except
    raise;
  end;
end;

function TScoopWrapper.UninstallApp(AName: string): Boolean;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
begin
  Result := false;

  try
    isExecuted := Self.InternalExecute(SCOOP_ARGS_UNINSTALL + AName);
    if (isExecuted) then
    begin
      //TODO: Manage error 'ERROR 'appname' isn't installed.'

      Regexp := TRegEx.Create('.* was uninstalled\.');

      for I := 1 to FOutput.Count - 1 do
      begin

        Match := Regexp.Match(FOutput[I]);

        if (Match.Success) then
        begin
          Result := True;
          break
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TScoopWrapper.UpdateApp(AName: string): boolean;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
begin
  Result := false;

  FOutFilter.EOLMarker := #10;

  try
    isExecuted := Self.InternalExecute(SCOOP_ARGS_UPDATE + AName);
    if (isExecuted) then
    begin

      Regexp := TRegEx.Create('.* was installed successfully!');

      for I := 1 to FOutput.Count - 1 do
      begin

        Match := Regexp.Match(FOutput[I]);

        if (Match.Success) then
        begin
          Result := True;
          break
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TScoopWrapper.UpdateScoop: boolean;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
begin
  Result := false;

  FOutFilter.EOLMarker := #10;

  try
    isExecuted := Self.InternalExecute(SCOOP_ARGS_UPDATE);
    if (isExecuted) then
    begin

      Regexp := TRegEx.Create('Scoop was updated successfully!');

      for I := 1 to FOutput.Count - 1 do
      begin

        Match := Regexp.Match(FOutput[I]);

        if (Match.Success) then
        begin
          Result := True;
          break
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TScoopWrapper.AddBucket(AName: string): Boolean;
var
  isExecuted: boolean;
  I: Integer;
  Regexp: TRegEx;
  Match: TMatch;
begin
  Result := false;

  FOutFilter.EOLMarker := #10;

  try
    isExecuted := Self.InternalExecute(SCOOP_ARGS_BUCKET_ADD + AName);
    if (isExecuted) then
    begin

      Regexp := TRegEx.Create('The .* bucket was added successfully\.');

      for I := 1 to FOutput.Count - 1 do
      begin

        Match := Regexp.Match(FOutput[I]);

        if (Match.Success) then
        begin
          Result := True;
          break
        end;
      end;
    end;
  except
    raise;
  end;
end;

procedure TScoopWrapper.CompleteHandler(Sender: TObject);
begin
  FOutFilter.Flush;
end;

end.
