{
Copyright (C) 2006-2021 Matteo Salvi

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
  LCLIntf, LCLType, SysUtils, Forms, Controls, Classes, LazFileUtils, Dialogs,
  LazUTF8, AppConfig.PathList;

type
  { TConfigPaths }

  TConfigPaths = class
  private
    FSuitePathCurrentTheme: String;
    FSuitePathList       : String;
    FSuiteDrive          : String;
    FSuitePathData       : String;
    FSuitePathSettings: String;
    FSuitePathWorking    : String;

    FEnvironmentVars : TPathList;
    FASuiteVars      : TPathList;

    function DeQuotedStr(AVar: AnsiString): AnsiString;
    function GetSuitePathBackup: String;
    function GetSuitePathCache: String;
    function GetSuitePathLocale: String;
    function GetSuitePathMenuThemes: String;
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
    property SuiteDrive: String read FSuiteDrive write FSuiteDrive;
    property SuitePathData: String read FSuitePathData write FSuitePathData;
    property SuitePathWorking: String read FSuitePathWorking write FSuitePathWorking;
    property SuitePathLocale: String read GetSuitePathLocale;
    property SuitePathCache: String read GetSuitePathCache;
    property SuitePathBackup: String read GetSuitePathBackup;
    property SuitePathMenuThemes: String read GetSuitePathMenuThemes;
    property SuitePathCurrentTheme: String read FSuitePathCurrentTheme write SetSuitePathCurrentTheme;

    property EnvironmentVars: TPathList read FEnvironmentVars;
    property ASuiteVars: TPathList read FASuiteVars;
  end;

implementation

uses
  Kernel.Consts, Utility.FileFolder, Utility.System, mormot.core.os;

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
  SysUtils.ForceDirectories(Self.SuitePathBackup);
end;

procedure TConfigPaths.CheckCacheFolders;
begin
  //Check if folder cache exists, else create it
  SysUtils.ForceDirectories(Self.SuitePathCache);
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

function TConfigPaths.GetSuitePathBackup: String;
begin
  Result := AppendPathDelim(FSuitePathData + BACKUP_DIR);
end;

function TConfigPaths.GetSuitePathCache: String;
begin
  Result := AppendPathDelim(FSuitePathData + CACHE_DIR);
end;

function TConfigPaths.GetSuitePathLocale: String;
begin
  Result := AppendPathDelim(FSuitePathWorking + LOCALE_DIR);
end;

function TConfigPaths.GetSuitePathMenuThemes: String;
begin
  Result := AppendPathDelim(FSuitePathWorking + MENUTHEMES_DIR);
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
var
  strPathExe: String;
begin
  //Default paths
  strPathExe := Application.ExeName;
  FSuitePathWorking  := ExtractFilePath(strPathExe);

  {$IFDEF MSWINDOWS}
  FSuiteDrive        := LowerCase(ExtractFileDrive(strPathExe));
  {$ELSE}
  //In Linux, use the folder path of asuite
  FSuiteDrive        := FSuitePathWorking;
  {$ENDIF}
  SetCurrentDir(FSuitePathWorking);

  if Not(IsDirectoryWritable(FSuitePathWorking)) then
  begin
    //FSuitePathWorking = ASuite.exe folder (ex C:\path\to\asuite_folder\)
    //FSuitePathData    = ASuite config folder (ex. C:\Users\user\AppData\Roaming\asuite\)
    FSuitePathData := GetAppConfigDir(True);
    SysUtils.ForceDirectories(FSuitePathData);
  end
  else
    FSuitePathData := FSuitePathWorking;

  //Check if xml list exists, else get sqlite list
  FSuitePathList := FSuitePathData + LIST_XML_FILENAME;
  if not FileExists(FSuitePathList) then
    FSuitePathList := FSuitePathData + LIST_SQLITE_FILENAME;

  FSuitePathSettings := FSuitePathData + SETTINGS_FILENAME;

  //Path variables
  {$IFDEF MSWINDOWS}
  FEnvironmentVars := TPathList.Create('\%([^%]+)\%');
  {$ELSE}
  FEnvironmentVars := TPathList.Create('\$\{([^}]+)\}');
  {$ENDIF}
  FASuiteVars := TPathList.Create('\%([^%]+)\%');

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
begin
  Result := '';
  if APath <> '' then
  begin
    Result := APath;

    //Replace old ASuite variables
    //Note: Unfortunately old asuite vars is not quoted, but in format $var.
    //      So these two vars are deprecated. This code remain for only backwards compatibility
    //CONST_PATH_ASuite_old = Launcher's path
    Result := StringReplace(Result, CONST_PATH_ASuite_old, SuitePathWorking, [rfIgnoreCase,rfReplaceAll]);
    //CONST_PATH_DRIVE_old = Launcher's Drive (ex. ASuite in H:\Software\ASuite.exe, CONST_PATH_DRIVE is H: )
    Result := StringReplace(Result, CONST_PATH_DRIVE_old, SUITEDRIVE, [rfIgnoreCase,rfReplaceAll]);

    //TODO: Expand only if string has %
    //Replace ASuite variables
    Result := FASuiteVars.ExpandVars(Result);

    //Replace environment variable
    Result := FEnvironmentVars.ExpandVars(Result);
                                                                                
    //Remove double path delimiter, resolve dots and expand path
    if not IsValidURLProtocol(Result) and not FilenameIsAbsolute(Result) then
      Result := CleanAndExpandFilename(Result);
  end;
end;

procedure TConfigPaths.RemoveCacheFolders;
var
  strPath: String;
begin
  strPath := Self.SuitePathCache;
  //Delete all file icon-cache and folder cache
  if (strPath <> '') and (SysUtils.DirectoryExists(strPath)) then
  begin
    DeleteFiles(strPath, '*.*');
    RemoveDir(strPath);
  end;
end;

procedure TConfigPaths.UpdatePathVariables;
begin
  UpdateEnvironmentVars;
  UpdateASuiteVars;
end;

end.
