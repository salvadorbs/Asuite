{
Copyright (C) 2006-2015 Matteo Salvi

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

interface

uses
  Windows, SysUtils, Graphics, Forms, Controls, Vcl.Imaging.pngimage, Classes,
  ShlObj;

type
  TConfigPaths = class
  private
    FSuitePathList       : String;
    FSuiteFullFileName   : String;
    FSuiteFileName       : String;
    FSuiteDrive          : String;
    FSuitePathData       : String;
    FSuitePathWorking    : String;
    FSuitePathLocale     : String;
    FSuitePathCache      : String;
    FSuitePathBackup     : String;
    FSuitePathMenuThemes     : String;
    FSuitePathCurrentTheme   : String;
  public
    constructor Create; overload;

    function AbsoluteToRelative(const APath: String): string;
    function RelativeToAbsolute(const APath: String): string;
    function ExpandEnvVars(const Str: string): string;

    procedure CheckBackupFolder;
    procedure CheckCacheFolders;
    function  GetNumberSubFolders(const FolderPath: String): Integer;
    procedure RemoveCacheFolders;

    property SuitePathList: String read FSuitePathList write FSuitePathList;
    property SuiteFullFileName: String read FSuiteFullFileName write FSuiteFullFileName;
    property SuiteFileName: String read FSuiteFileName write FSuiteFileName;
    property SuiteDrive: String read FSuiteDrive write FSuiteDrive;
    property SuitePathData: String read FSuitePathData write FSuitePathData;
    property SuitePathWorking: String read FSuitePathWorking write FSuitePathWorking;
    property SuitePathLocale: String read FSuitePathLocale write FSuitePathLocale;
    property SuitePathCache: String read FSuitePathCache write FSuitePathCache;
    property SuitePathBackup: String read FSuitePathBackup write FSuitePathBackup;
    property SuitePathMenuThemes: String read FSuitePathMenuThemes write FSuitePathMenuThemes;
    property SuitePathCurrentTheme: String read FSuitePathCurrentTheme write FSuitePathCurrentTheme;
  end;

implementation

uses
  Utility.System, Kernel.Consts, Utility.FileFolder;

{ TConfigPaths }

function TConfigPaths.AbsoluteToRelative(const APath: String): string;
var
  sPath: string;
begin
  sPath := APath;
  //Const %FolderIcon%
  sPath  := StringReplace(sPath, FSuitePathCurrentTheme + ICONS_DIR + FILEICON_Folder + EXT_ICO, CONST_PATH_FOLDERICON, [rfIgnoreCase,rfReplaceAll]);
  //Const %UrlIcon%
  sPath  := StringReplace(sPath, FSuitePathCurrentTheme + ICONS_DIR + FILEICON_Url + EXT_ICO, CONST_PATH_URLICON, [rfIgnoreCase,rfReplaceAll]);
  //Const $ASuite
  sPath  := StringReplace(sPath, ExcludeTrailingPathDelimiter(SuitePathWorking), CONST_PATH_ASuite, [rfIgnoreCase,rfReplaceAll]);
  //Const $Drive
  sPath  := StringReplace(sPath, SUITEDRIVE, CONST_PATH_DRIVE, [rfIgnoreCase,rfReplaceAll]);
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

constructor TConfigPaths.Create;
begin
  //Default paths
  FSuiteFullFileName := Application.ExeName;
  FSuiteFileName     := ExtractFileName(FSuiteFullFileName);
  FSuiteDrive        := LowerCase(ExtractFileDrive(FSuiteFullFileName));
  FSuitePathWorking  := ExtractFilePath(FSuiteFullFileName);
  SetCurrentDir(FSuitePathWorking);
  if Not(IsDirectoryWriteable(FSuitePathWorking)) then
  begin
    FSuitePathData := IncludeTrailingBackslash(GetSpecialFolder(CSIDL_LOCAL_APPDATA) + APP_NAME);
    SysUtils.ForceDirectories(FSuitePathData);
  end
  else
    FSuitePathData := FSuitePathWorking;
  FSuitePathLocale     := FSuitePathWorking + LOCALE_DIR;
  FSuitePathCache      := FSuitePathData + CACHE_DIR;
  FSuitePathBackup     := FSuitePathData + BACKUP_DIR;
  FSuitePathMenuThemes := FSuitePathWorking + MENUTHEMES_DIR;
  //List
  //Check if xml list exists, else get sqlite list
  FSuitePathList := FSuitePathData + 'asuite.xml';
  if not FileExists(FSuitePathList) then
    FSuitePathList := FSuitePathData + ChangeFileExt(FSuiteFileName, EXT_SQL);
end;

function TConfigPaths.ExpandEnvVars(const Str: string): string;
var
  BufSize: Integer; // size of expanded string
begin
  // Get required buffer size
  BufSize := ExpandEnvironmentStrings(PChar(Str), nil, 0);
  if BufSize > 0 then
  begin
    // Read expanded string into result string
    SetLength(Result, BufSize - 1);
    ExpandEnvironmentStrings(PChar(Str), PChar(Result), BufSize);
  end
  else
    // Trying to expand empty string
    Result := '';
end;

function TConfigPaths.GetNumberSubFolders(const FolderPath: String): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  //Count subfolders in FolderPath
  if FindFirst(FolderPath + '*.*', faAnyFile, SearchRec) = 0 then
  repeat
    if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) and
       ((SearchRec.Attr and faDirectory) = (faDirectory)) then
    begin
      //Increment result
      Inc(Result);
      Result := Result + GetNumberSubFolders(IncludeTrailingBackslash(FolderPath + SearchRec.Name));
    end;
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

function TConfigPaths.RelativeToAbsolute(const APath: String): string;
var
  sPath: string;
begin
  Result := '';
  if APath <> '' then
  begin
    sPath := APath;
    //CONST_PATH_FOLDERICON = Folder Icon's path
    sPath := StringReplace(sPath, CONST_PATH_FOLDERICON, FSuitePathCurrentTheme +
                           ICONS_DIR + FILEICON_Folder + EXT_ICO, [rfIgnoreCase,rfReplaceAll]);
    //CONST_PATH_URLICON = Url Icon's path
    sPath := StringReplace(sPath, CONST_PATH_URLICON, FSuitePathCurrentTheme +
                           ICONS_DIR + FILEICON_Url + EXT_ICO, [rfIgnoreCase,rfReplaceAll]);
    //CONST_PATH_ASuite = Launcher's path
    sPath := StringReplace(sPath, CONST_PATH_ASuite, SuitePathWorking, [rfIgnoreCase,rfReplaceAll]);
    //CONST_PATH_DRIVE = Launcher's Drive (ex. ASuite in H:\Software\ASuite.exe, CONST_PATH_DRIVE is H: )
    sPath := StringReplace(sPath, CONST_PATH_DRIVE, SUITEDRIVE, [rfIgnoreCase,rfReplaceAll]);
    //Remove double slash (\)
    if Pos('\\', sPath) <> 1 then
      sPath := StringReplace(sPath, '\\', PathDelim, [rfIgnoreCase,rfReplaceAll]);
    //Replace environment variable
    sPath := ExpandEnvVars(sPath);
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

end.
