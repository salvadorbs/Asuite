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
  classes, SysUtils, MPcommonObjects, Scoop.App, VirtualFileSearch,
  Winapi.Windows,
  System.Generics.Collections, variants, Vcl.Dialogs, JsonDataObjects;

type

  TOnEndSearchManifest = procedure(Sender: TObject; const Apps: TScoopApps)
    of object;

  TScoopBucket = class
  private
    { private declarations }
    FName: string;
    FApps: TScoopApps; // TODO: Populate it
    FVirtualFileSearch: TVirtualFileSearch;
    FExcludeJson: TStringList;
    FOnEndSearchManifest: TOnEndSearchManifest;

    function GetPath(): string;
    procedure OnEndSearchAppsManifest(Sender: TObject;
      Results: TCommonPIDLList);
    procedure OnSearchCompareManifest(Sender: TObject; const FilePath: string;
      FindFileData: TWIN32FindDataW; var UseFile: Boolean);
    function FindMatchText(Strings: TStrings; const Str: string): Integer;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AName: string);
    destructor Destroy(); override;

    procedure StartSearchAppsManifest();

    property Name: string read FName;
    property Path: string read GetPath;
    property OnEndSearchManifest: TOnEndSearchManifest read FOnEndSearchManifest
      write FOnEndSearchManifest;
  end;

  TScoopBuckets = class(TObjectList<TScoopBucket>);

implementation

uses
  Path.Utils, MPShellUtilities, StrUtils;

{ TScoopBucket }

constructor TScoopBucket.Create(AName: string);
begin
  FApps := TScoopApps.Create;
  FExcludeJson := TStringList.Create;
  FExcludeJson.Add('.vscode');

  FVirtualFileSearch := TVirtualFileSearch.Create(nil);
  FVirtualFileSearch.ThreadPriority := tpIdle;
  FVirtualFileSearch.UpdateRate := 50;
  FVirtualFileSearch.SearchCriteriaFilename.Add('*.json');
  FVirtualFileSearch.CaseSensitive := false;
  FVirtualFileSearch.SubFolders := true;
  FVirtualFileSearch.OnSearchEnd := OnEndSearchAppsManifest;
  FVirtualFileSearch.OnSearchCompare := OnSearchCompareManifest;

  Self.FName := AName;
end;

destructor TScoopBucket.Destroy;
begin
  FApps.Free;
  FVirtualFileSearch.Free;
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

procedure TScoopBucket.StartSearchAppsManifest;
begin
  FVirtualFileSearch.SearchPaths.Clear;
  FVirtualFileSearch.SearchPaths.Add(Self.Path);
  FVirtualFileSearch.Run;
end;

function TScoopBucket.GetPath: string;
begin
  // TODO: Use TPath.combine!
  Result := IncludeTrailingPathDelimiter
    (ExpandEnvVars('%SCOOP%\buckets\' + Self.Name));
end;

function IsEmptyOrNull(const Value: Variant): Boolean;
begin
  Result := VarIsClear(Value) or VarIsEmpty(Value) or VarIsNull(Value) or
    (VarCompareValue(Value, Unassigned) = vrEqual);
  if (not Result) and VarIsStr(Value) then
    Result := Value = '';
end;

procedure TScoopBucket.OnEndSearchAppsManifest(Sender: TObject;
  Results: TCommonPIDLList);
var
  I: Integer;
  Obj: TJsonObject;
  App: TScoopApp;
begin
  FApps.Clear;
  for I := 0 to Results.Count - 1 do
  begin
    try
      Obj := TJsonObject.ParseFromFile(PIDLToPath(Results[I])) as TJsonObject;

      if Assigned(Obj) and (Obj.IndexOf('version') <> -1) then
      begin
        App := TScoopApp.Create(ExtractFileName(PIDLToPath(Results[I])));
        App.Version := Obj.S['version'];
        FApps.Add(App);
      end;
    finally
      FreeAndNil(Obj);
    end;

  end;

  if Assigned(FOnEndSearchManifest) then
    FOnEndSearchManifest(Self, FApps);
end;

procedure TScoopBucket.OnSearchCompareManifest(Sender: TObject;
  const FilePath: string; FindFileData: TWIN32FindDataW; var UseFile: Boolean);
begin
  UseFile := FindMatchText(FExcludeJson, FilePath) = -1;
end;

end.
