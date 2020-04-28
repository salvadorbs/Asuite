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
  classes, PJPipe, PJConsoleApp, PJPipeFilters, PJFileHandle, SysUtils,
  Scoop.Bucket, Scoop.App, System.StrUtils,
  System.Generics.Defaults, System.Generics.Collections;

const
  SCOOP_CMD = 'cmd.exe /c scoop';
  SCOOP_ARGS_STATUS = ' status';
  SCOOP_ARGS_APP_LIST = ' list';
  SCOOP_ARGS_BUCKET_LIST = ' bucket list';
  SCOOP_ARGS_BUCKET_KNOWN = ' bucket known';
  SCOOP_ARGS_INSTALL = ' install';
  SCOOP_ARGS_UNINSTALL = ' uninstall';

type

  TScoopWrapper = class
  private
    { private declarations }
    FConsoleApp: TPJConsoleApp;
    FOutput: TStringList;
    FOutPipe: TPJPipe;
    FErrPipe: TPJPipe;
    FOutFilter: TPJAnsiSBCSPipeFilter;

    FVersion: string;
    FBuckets: TScoopBuckets;
    FBucketsKnown: TScoopBuckets;
    FAppsInstalled: TScoopApps;

    procedure LineEndHandler(Sender: TObject; const Text: AnsiString);
    procedure WorkHandler(Sender: TObject);
    procedure CompleteHandler(Sender: TObject);
    function InternalExecute(Args: string): boolean; overload;
    function InternalExecute(): boolean; overload;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create();
    destructor Destroy(); override;

    property Output: TStringList read FOutput;
    property Buckets: TScoopBuckets read FBuckets;
    property BucketsKnown: TScoopBuckets read FBucketsKnown;
    property Apps: TScoopApps read FAppsInstalled;

    function IsScoopExists(): boolean;
    procedure Update();
    procedure InstallApp(AName: string);
    procedure UninstallApp(AName: string);
    procedure Status();
    procedure AddBucket(AName: string);
    procedure RemoveBucket(AName: string);

    function ListBuckets(): TList<TScoopBucket>;
    function ListBucketsKnown(): TList<TScoopBucket>;
    function ListApps(): TScoopApps;
  end;

const
  STATUS_MSG_UPDATE = 'Updates are available for:';
  STATUS_MSG_OUTDATED = 'These apps are outdated and on hold:';
  STATUS_MSG_MISSMANIFEST = 'These app manifests have been removed:';
  STATUS_MSG_FAILED = 'These apps failed to install:';
  STATUS_MSG_MISSDEPENDECIES = 'Missing runtime dependencies:';

  arrScoopStatus: array of string = [STATUS_MSG_UPDATE, STATUS_MSG_OUTDATED, STATUS_MSG_MISSMANIFEST, STATUS_MSG_FAILED, STATUS_MSG_MISSDEPENDECIES];

implementation

uses
  System.RegularExpressions;

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
  FAppsInstalled := TScoopApps.Create();
end;

destructor TScoopWrapper.Destroy();
begin
  FConsoleApp.Free;
  FOutFilter.Free;
  FErrPipe.Free;
  FOutput.Free;

  FAppsInstalled.Free;
  FBuckets.Free;
  FBucketsKnown.Free;
end;

function TScoopWrapper.ListApps: TScoopApps;
var
  isExecuted: boolean;
  I: Integer;
  Bucket: TScoopBucket;
  Regexp: TRegEx;
  Match: TMatch;
begin
  try

    FOutFilter.EOLMarker := #10;
    isExecuted := Self.InternalExecute(SCOOP_ARGS_APP_LIST);
    if (isExecuted) then
    begin
      FAppsInstalled.Clear;

      Regexp := TRegEx.Create('(\S+) (.+)');
      for I := 1 to FOutput.Count - 1 do
      begin
        Match := Regexp.Match(FOutput[I]);

        if (Match.Success) and (Match.Groups.Count > 1) then
        begin
          FAppsInstalled.Add(TScoopApp.Create(Match.Groups[1].Value));
        end;
      end;
    end;

    Result := FAppsInstalled;
  except
    raise;
  end;
end;

function TScoopWrapper.ListBuckets(): TList<TScoopBucket>;
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
        FBuckets.Add(TScoopBucket.Create(FOutput[I]))
    end;

    Result := FBuckets;
  except
    raise;
  end;
end;

function TScoopWrapper.ListBucketsKnown: TList<TScoopBucket>;
var
  isExecuted: boolean;
  I: Integer;
  Bucket: TScoopBucket;
begin
  try

    FOutFilter.EOLMarker := #13#10;
    if FBucketsKnown.Count = 0 then
    begin
      isExecuted := Self.InternalExecute(SCOOP_ARGS_BUCKET_KNOWN);
      if (isExecuted) then
      begin
        FBucketsKnown.Clear;

        for I := 0 to FOutput.Count - 1 do
          FBucketsKnown.Add(TScoopBucket.Create(FOutput[I]))
      end;
    end;

    Result := FBucketsKnown;
  except
    raise;
  end;
end;

procedure TScoopWrapper.InstallApp(AName: string);
begin
  // TODO: Run scoop --install param and
end;

function TScoopWrapper.InternalExecute: boolean;
begin
  Self.InternalExecute('');
end;

function TScoopWrapper.InternalExecute(Args: string): boolean;
var
  isExecuted: boolean;
begin
  FOutput.Clear;

  isExecuted := FConsoleApp.Execute(SCOOP_CMD + Args);

  Result := (isExecuted) and (FConsoleApp.ExitCode = 0) and (FOutput.Count > 0);
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
end;

procedure TScoopWrapper.LineEndHandler(Sender: TObject; const Text: AnsiString);
begin
  if Text <> '' then
    FOutput.Add(Text);
end;

procedure TScoopWrapper.RemoveBucket(AName: string);
begin

end;

procedure TScoopWrapper.Status;
var
  isExecuted: boolean;
  I: Integer;
  Bucket: TScoopBucket;
  Regexp: TRegEx;
  Match: TMatch;
  TrackNextLines: boolean;
  intApp: integer;
begin
  try

    Self.ListApps;

    FOutFilter.EOLMarker := #10;
    isExecuted := Self.InternalExecute(SCOOP_ARGS_STATUS);
    if (isExecuted) then
    begin
      TrackNextLines := False;
      //Regexp for "appname: cur_version -> new_version"
      Regexp := TRegEx.Create('(\S+). (.+) -> (.+)');

      //Get new app version
      for I := 1 to FOutput.Count - 1 do
      begin
        //If current line is update status, we can track next lines
        TrackNextLines := (IndexStr(FOutput[I], arrScoopStatus) = 0);

        if TrackNextLines then
        begin

          Match := Regexp.Match(FOutput[I]);

          if (Match.Success) and (Match.Groups.Count > 1) then
          begin
            intApp := FAppsInstalled.Find(Match.Groups[1].Value);
            FAppsInstalled[intApp].Version := Match.Groups[2].Value;
            FAppsInstalled[intApp].LatestVersion := Match.Groups[3].Value;
          end;
        end;
      end;
    end;

  except
    raise;
  end;
end;

procedure TScoopWrapper.UninstallApp(AName: string);
begin
  // TODO: Run scoop --uninstall param
end;

procedure TScoopWrapper.Update;
begin
  // TODO: Run scoop --update only scoop
end;

procedure TScoopWrapper.AddBucket(AName: string);
begin

end;

procedure TScoopWrapper.CompleteHandler(Sender: TObject);
begin
  FOutFilter.Flush;
end;

end.
