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

unit Scoop.Bucket;

interface

uses
  classes, SysUtils, Scoop.App, VirtualFileSearch, System.IOUtils, Scoop.List.Apps,
  System.Generics.Defaults, System.Generics.Collections, Winapi.Windows,
  MPcommonObjects, JsonDataObjects, MPCommonUtilities;

type

  TOnEndLoadingApps = procedure(Sender: TObject; const Apps: TScoopApps)
    of object;

  TScoopBucket = class
  private
    { private declarations }
    FName: string;
    FVirtualFileSearch: TVirtualFileSearch;
    FExcludeJson: TStringList;
    FApps: TScoopApps;
    FOnEndLoadingApps: TOnEndLoadingApps;

    procedure OnSearchCompareManifest(Sender: TObject; const FilePath: string;
      FindFileData: TWIN32FindDataW; var UseFile: boolean);
    procedure OnEndSearchAppsManifest(Sender: TObject;
      Results: TCommonPIDLList);

    function GetPath(): string;
    function FindMatchText(Strings: TStrings; const Str: string): Integer;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AName: string);
    destructor Destroy(); override;

    property Name: string read FName;
    property Path: string read GetPath;
    property Apps: TScoopApps read FApps;

    property OnEndLoadingApps: TOnEndLoadingApps read FOnEndLoadingApps
      write FOnEndLoadingApps;

    procedure ClearApps();
    procedure LoadApps();
  end;

implementation

uses
  Path.Utils, MPShellUtilities, StrUtils;

{ TScoopBucket }

procedure TScoopBucket.ClearApps;
begin
  FApps.Clear;
end;

constructor TScoopBucket.Create(AName: string);
begin
  Self.FName := AName;

  FVirtualFileSearch := TVirtualFileSearch.Create(nil);
  FVirtualFileSearch.ThreadPriority := tpIdle;
  FVirtualFileSearch.UpdateRate := 50;
  FVirtualFileSearch.SearchCriteriaFilename.Add('*.json');
  FVirtualFileSearch.CaseSensitive := false;
  FVirtualFileSearch.SubFolders := True;
  FVirtualFileSearch.OnSearchEnd := OnEndSearchAppsManifest;
  FVirtualFileSearch.OnSearchCompare := OnSearchCompareManifest;

  FApps := TScoopApps.Create;

  FExcludeJson := TStringList.Create;
  FExcludeJson.Add('.vscode');
end;

destructor TScoopBucket.Destroy;
begin
  FVirtualFileSearch.Free;
  FApps.Free;
  FExcludeJson.Free;
end;

function TScoopBucket.FindMatchText(Strings: TStrings;
  const Str: string): Integer;
begin
  for Result := 0 to Strings.Count - 1 do
    if ContainsText(Str, Strings[Result]) then
      exit;

  Result := -1;
end;

function TScoopBucket.GetPath: string;
begin
  Result := TPath.Combine(ExpandEnvVars('%SCOOP%\buckets\'), Self.Name);
end;

procedure TScoopBucket.LoadApps;
begin
  FVirtualFileSearch.SearchPaths.Clear;
  FVirtualFileSearch.SearchPaths.Add(Self.Path);
  FVirtualFileSearch.Run;
end;

procedure TScoopBucket.OnEndSearchAppsManifest(Sender: TObject;
  Results: TCommonPIDLList);
var
  I: Integer;
  Obj: TJsonObject;
  App: TScoopApp;
begin
  for I := 0 to Results.Count - 1 do
  begin
    try
      //TODO: Create a class-wrapper to parse manifest.json
      Obj := TJsonObject.ParseFromFile(PIDLToPath(Results[I])) as TJsonObject;

      if Assigned(Obj) and (Obj.IndexOf('version') <> -1) then
      begin
        App := FApps.AddItem(TPath.GetFileNameWithoutExtension(PIDLToPath(Results[I])), Self);
        App.VersionLatest := Obj.S['version'];

      end;
    finally
      FreeAndNil(Obj);
    end;
  end;

  if Assigned(FOnEndLoadingApps) then
    FOnEndLoadingApps(Self, FApps);
end;

procedure TScoopBucket.OnSearchCompareManifest(Sender: TObject;
  const FilePath: string; FindFileData: TWIN32FindDataW; var UseFile: boolean);
begin
  UseFile := FindMatchText(FExcludeJson, FilePath) = -1;
end;

end.
