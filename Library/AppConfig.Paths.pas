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

unit AppConfig.Paths;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Graphics, Forms, Controls, Classes, LazFileUtils, Dialogs,
  Generics.Defaults, Generics.Collections, LazUTF8, AppConfig.PathVars;

type
  { TConfigPaths }

  TConfigPaths = class
  private
    FSuitePathList       : String;
    FSuiteFullFileName   : String;
    FSuiteFileName       : String;
    FSuiteDrive          : String;
    FSuitePathData       : String;
    FSuitePathSettings: String;
    FSuitePathWorking    : String;
    FSuitePathLocale     : String;
    FSuitePathCache      : String;
    FSuitePathBackup     : String;
    FSuitePathMenuThemes     : String;
    FSuitePathCurrentTheme   : String;

    FEnvironmentVars : TPathVars;
    FASuiteVars      : TPathVars;

    function DeQuotedStr(AVar: AnsiString): AnsiString;
    procedure SetSuitePathCurrentTheme(AValue: String);
    procedure UpdateEnvironmentVars;
    procedure UpdateASuiteVars;
  public
    constructor Create;
    destructor Destroy; override;

    function AbsoluteToRelative(const APath: String): string;
    function RelativeToAbsolute(const APath: String): string;

    procedure CheckBackupFolder;
    procedure CheckCacheFolders;
    function  GetNumberSubFolders(const FolderPath: String): Integer;
    procedure RemoveCacheFolders;
    procedure UpdatePathVariables;

    property SuitePathList: String read FSuitePathList write FSuitePathList;     
    property SuitePathSettings: String read FSuitePathSettings write FSuitePathSettings;
    property SuiteFullFileName: String read FSuiteFullFileName write FSuiteFullFileName;
    property SuiteFileName: String read FSuiteFileName write FSuiteFileName;
    property SuiteDrive: String read FSuiteDrive write FSuiteDrive;
    property SuitePathData: String read FSuitePathData write FSuitePathData;
    property SuitePathWorking: String read FSuitePathWorking write FSuitePathWorking;
    property SuitePathLocale: String read FSuitePathLocale write FSuitePathLocale;
    property SuitePathCache: String read FSuitePathCache write FSuitePathCache;
    property SuitePathBackup: String read FSuitePathBackup write FSuitePathBackup;
    property SuitePathMenuThemes: String read FSuitePathMenuThemes write FSuitePathMenuThemes;
    property SuitePathCurrentTheme: String read FSuitePathCurrentTheme write SetSuitePathCurrentTheme;

    property EnvironmentVars: TPathVars read FEnvironmentVars;
    property ASuiteVars: TPathVars read FASuiteVars;
  end;

implementation

uses
  Kernel.Consts, Utility.FileFolder, SynCommons;

{ TConfigPaths }

function TConfigPaths.AbsoluteToRelative(const APath: String): string;
var
  sPath: string;
begin
  sPath := APath;

  //Const %FolderIcon%
  sPath := FASuiteVars.DeExpandVars(sPath, DeQuotedStr(CONST_PATH_FOLDERICON));

  //Const %UrlIcon%
  sPath := FASuiteVars.DeExpandVars(sPath, DeQuotedStr(CONST_PATH_URLICON));

  //Const $ASuite
  sPath := FASuiteVars.DeExpandVars(sPath, DeQuotedStr(CONST_PATH_ASuite));

  //Const $Drive
  sPath := FASuiteVars.DeExpandVars(sPath, DeQuotedStr(CONST_PATH_DRIVE));

  Result := sPath;
end;

procedure TConfigPaths.CheckBackupFolder;
begin
  //Check if folder backup exists, else create it
  SysUtils.ForceDirectories(FSuitePathBackup);
end;

procedure TConfigPaths.CheckCacheFolders;
begin
  //Check if folder cache exists, else create it
  SysUtils.ForceDirectories(FSuitePathCache);
end;

procedure TConfigPaths.UpdateEnvironmentVars;
var
  str: AnsiString;
  arrString: TStringArray;
  I: Integer;
begin
  FEnvironmentVars.Clear;

  for I := 0 to GetEnvironmentVariableCountUTF8 - 1 do
  begin
    str := GetEnvironmentStringUTF8(I);
    if str[1] <> '=' then
    begin
      arrString := str.Split('=', 2);
      if Length(arrString) = 2 then
        //Key in UpperCase, value don't care if up or lower case
        FEnvironmentVars.Add(UpperCase(arrString[0]), arrString[1]);
    end;
  end;
end;

procedure TConfigPaths.SetSuitePathCurrentTheme(AValue: String);
begin
  if FSuitePathCurrentTheme = AValue then Exit;
    FSuitePathCurrentTheme := AValue;

  UpdatePathVariables;
end;

//Workaround for const string
function TConfigPaths.DeQuotedStr(AVar: AnsiString): AnsiString;
begin
  Result := UpperCase(AVar);
  Result := AnsiDequotedStr(Result, '%');
end;

procedure TConfigPaths.UpdateASuiteVars;
var
  strFolderIcon: AnsiString;
begin
  FASuiteVars.Clear;

  strFolderIcon := AppendPathDelim(FSuitePathCurrentTheme + ICONS_DIR);

  //CONST_PATH_ASuite = Launcher's path
  FASuiteVars.Add(DeQuotedStr(CONST_PATH_ASUITE), SuitePathWorking);

  //CONST_PATH_DRIVE = Launcher's Drive (ex. ASuite in H:\Software\ASuite.exe, CONST_PATH_DRIVE is H: )
  FASuiteVars.Add(DeQuotedStr(CONST_PATH_DRIVE), SUITEDRIVE);

  //CONST_PATH_FOLDERICON = Folder Icon's path
  FASuiteVars.Add(DeQuotedStr(CONST_PATH_FOLDERICON), strFolderIcon + FILEICON_Folder + EXT_ICO);

  //CONST_PATH_URLICON = Url Icon's path
  FASuiteVars.Add(DeQuotedStr(CONST_PATH_URLICON), strFolderIcon + FILEICON_Url + EXT_ICO);
end;

constructor TConfigPaths.Create;
begin
  //Default paths
  FSuiteFullFileName := Application.ExeName;
  FSuiteFileName     := ExtractFileName(FSuiteFullFileName);
  FSuiteDrive        := LowerCase(ExtractFileDrive(FSuiteFullFileName));
  FSuitePathWorking  := ExtractFilePath(FSuiteFullFileName);
  SetCurrentDir(FSuitePathWorking);
  //TODO: Review structure folder - SetCurrentDir(FSuitePathWorking) is correct?
  if Not(IsDirectoryWritable(FSuitePathWorking)) then
  begin
    FSuitePathData := GetAppConfigDir(True);
    SysUtils.ForceDirectories(FSuitePathData);
  end
  else
    FSuitePathData := FSuitePathWorking;
  FSuitePathLocale     := AppendPathDelim(FSuitePathWorking + LOCALE_DIR);
  FSuitePathCache      := AppendPathDelim(FSuitePathData + CACHE_DIR);
  FSuitePathBackup     := AppendPathDelim(FSuitePathData + BACKUP_DIR);
  FSuitePathMenuThemes := AppendPathDelim(FSuitePathWorking + MENUTHEMES_DIR);
  //List
  //Check if xml list exists, else get sqlite list
  FSuitePathList := FSuitePathData + 'asuite.xml';
  if not FileExists(FSuitePathList) then
    FSuitePathList := FSuitePathData + ChangeFileExt(FSuiteFileName, EXT_SQL);

  FSuitePathSettings := FSuitePathData + SETTINGS_FILENAME;

  //Path variables
  {$IFDEF MSWINDOWS}
  FEnvironmentVars := TPathVars.Create('\%([^%]+)\%');
  {$ELSE}
  FEnvironmentVars := TPathVars.Create('\$\{([^}]+)\}');
  {$ENDIF}
  FASuiteVars := TPathVars.Create('\%([^%]+)\%');

  UpdatePathVariables;
end;

destructor TConfigPaths.Destroy;
begin
  FEnvironmentVars.Free;
  FASuiteVars.Free;
end;

function TConfigPaths.GetNumberSubFolders(const FolderPath: String): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  //Count subfolders in FolderPath
  if FindFirst(FolderPath + '*.*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) and
         ((SearchRec.Attr and faDirectory) = (faDirectory)) then
      begin
        //Increment result
        Inc(Result);
        Result := Result + GetNumberSubFolders(AppendPathDelim(FolderPath + SearchRec.Name));
      end;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function TConfigPaths.RelativeToAbsolute(const APath: String): string;
var
  sPath: string;
begin
  Result := '';
  if APath <> '' then
  begin
    sPath := APath;

    //Replace old ASuite variables
    //Note: Unfortunately old asuite vars is not quoted, but in format $var.
    //      So these two vars are deprecated. This code remain for only backwards compatibility
    //CONST_PATH_ASuite_old = Launcher's path
    sPath := StringReplace(sPath, CONST_PATH_ASuite_old, SuitePathWorking, [rfIgnoreCase,rfReplaceAll]);
    //CONST_PATH_DRIVE_old = Launcher's Drive (ex. ASuite in H:\Software\ASuite.exe, CONST_PATH_DRIVE is H: )
    sPath := StringReplace(sPath, CONST_PATH_DRIVE_old, SUITEDRIVE, [rfIgnoreCase,rfReplaceAll]);

    //Replace ASuite variables
    sPath := FASuiteVars.ExpandVars(sPath);

    //Replace environment variable
    sPath := FEnvironmentVars.ExpandVars(sPath);
                                                                                
    //Remove double path delimiter and resolve dots
    sPath := TrimFilename(sPath);

    //If sPath exists, expand it in absolute path (to avoid the "..")
    if (FileExists(sPath) or SysUtils.DirectoryExists(sPath)) and (Length(sPath) <> 2) then
      Result := ExpandFileName(sPath)
    else
      Result := sPath;
  end;
end;

procedure TConfigPaths.RemoveCacheFolders;
begin
  //Delete all file icon-cache and folder cache
  if (SysUtils.DirectoryExists(FSuitePathCache)) then
  begin
    DeleteFiles(FSuitePathCache,'*.*');
    RemoveDir(FSuitePathCache);
  end;
end;

procedure TConfigPaths.UpdatePathVariables;
begin
  UpdateEnvironmentVars;
  UpdateASuiteVars;
end;

end.
