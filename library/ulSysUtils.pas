{
Copyright (C) 2006-2013 Matteo Salvi

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

unit ulSysUtils;

interface

uses
  AppConfig, Windows, ShellApi, SysUtils, Classes, ulEnumerations, Registry,
  ShlObj, ActiveX, ComObj, Forms, Dialogs, FileCtrl;

{ Browse }
function  BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
function  BrowseForFolder(const Caption, InitialDir: String): String;

{ Check functions }
function HasDriveLetter(const Path: String): Boolean;
function IsAbsolutePath(const Path: String): Boolean;
function IsDirectory(const Path: String): Boolean;
function IsDriveRoot(const Path: String): Boolean;
function IsUrl(Path: String): Boolean;
function FileFolderPageWebExists(Path: String): Boolean;

{ Files }
procedure DeleteFiles(PathDir, FileName: String);
procedure DeleteOldBackups(MaxNumber: Integer);

{ Desktop shortcut }
procedure CreateShortcutOnDesktop(FileName: String;TargetFilePath, Params, WorkingDir: String);
procedure DeleteShortcutOnDesktop(FileName: String);
function  GetShortcutTarget(LinkFileName:String;ShortcutType: TShortcutField):String;
procedure RenameShortcutOnDesktop(OldFileName, FileName: String);

{ Relative & Absolute path }
function AbsoluteToRelative(APath: String): string;
function RelativeToAbsolute(APath: String): string;

{ Registry }
procedure SetASuiteAtWindowsStartup;
procedure DeleteASuiteAtWindowsStartup;

{ Misc }
procedure EjectDialog(Sender: TObject);
function ExtractDirectoryName(const Filename: string): string;
function GetCorrectWorkingDir(Default: string): string;

implementation

uses
  ulStringUtils, Main;

function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
begin
  //Set initial directory
  if uMsg = BFFM_INITIALIZED then
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
  Result := 0;
end;

function BrowseForFolder(const Caption, InitialDir: String): String;
var
  Path: string;
begin
  Result := '';
  //Get Path and delete \ in last char. Example c:\xyz\ to c:\xyz
  Path   := ExcludeTrailingPathDelimiter(InitialDir);
  //Call Browse for folder dialog and get new path
  if SelectDirectory('','',Path) then
    Result := Path;
end;

function HasDriveLetter(const Path: String): Boolean;
var P: PChar;
begin
  if Length(Path) < 2 then
    Exit(False);
  P := Pointer(Path);
  if not CharInSet(P^, DriveLetters) then
    Exit(False);
  Inc(P);
  if not CharInSet(P^, [':']) then
    Exit(False);
  Result := True;
end;

function IsAbsolutePath(const Path: String): Boolean;
begin
  if Path = '' then
    Result := False
  else if HasDriveLetter(Path) then
    Result := True
  else if CharInSet(PChar(Pointer(Path))^, ['\', '/']) then
    Result := True else
  Result := False;
end;

function IsDirectory(const Path: String): Boolean;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Path);
  if L = 0 then
    Result := False
  else if (L = 2) and HasDriveLetter(Path) then
    Result := True
  else
    begin
      P := Pointer(Path);
      Inc(P, L - 1);
      Result := CharInSet(P^, SLASHES);
    end;
end;

function IsDriveRoot(const Path: String): Boolean;
begin
  Result := (Length(Path) = 3) and HasDriveLetter(Path) and (Path[3] = PathDelim);
end;

function IsUrl(Path: String): Boolean;
begin
  if (pos('http://',Path) = 1) or (pos('https://',Path) = 1) or
     (pos('ftp://',Path) = 1) or (pos('www.',Path) = 1) or
     (pos('%',Path) = 1) then
    Result := True
  else
    Result := False;
end;

function FileFolderPageWebExists(Path: String): Boolean;
begin
  Result := ((FileExists(Path)) or (SysUtils.DirectoryExists(Path)) or
             (IsUrl(Path)));
end;

procedure DeleteFiles(PathDir, FileName: String);
var
  Search : TSearchRec;
begin
  //Delete file with FileName in folder PathDir (path relative)
  if FindFirst(PathDir + FileName,faAnyFile,Search) = 0 then
  begin
    repeat
      DeleteFile(PathDir + Search.Name);
    until
      FindNext(Search) <> 0;
    FindClose(Search);
  end;
end;

procedure DeleteOldBackups(MaxNumber: Integer);
var
  BackupList   : TStringList;
  BackupSearch : TSearchRec;
  I            : Integer;
begin
  BackupList := TStringList.Create;
  if FindFirst(SUITE_BACKUP_PATH + APP_NAME + '_*' + EXT_SQLBCK,faAnyFile,BackupSearch) = 0 then
  begin
    repeat
      BackupList.Add(BackupSearch.Name);
    until
      FindNext(BackupSearch) <> 0;
    FindClose(BackupSearch);
  end;
  BackupList.Sort;
  for I := 1 to BackupList.Count - MaxNumber do
    DeleteFile(SUITE_BACKUP_PATH + BackupList[I - 1]);
  BackupList.Free;
end;

procedure CreateShortcutOnDesktop(FileName: String;TargetFilePath, Params, WorkingDir: String);
var
  IObject  : IUnknown;
  ISLink   : IShellLink;
  IPFile   : IPersistFile;
  PIDL     : PItemIDList;
  InFolder : array[0..MAX_PATH] of Char;
  LinkName : String;
begin
  //Relative path to Absolute path
  if pos(':',TargetFilePath) = 0 then
    TargetFilePath := SUITE_WORKING_PATH + TargetFilePath;
  //Create objects
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink  := IObject as IShellLink;
  IPFile  := IObject as IPersistFile;
  //Create link
  ISLink.SetPath(pChar(TargetFilePath));
  ISLink.SetArguments(pChar(Params));
  if WorkingDir = '' then
    ISLink.SetWorkingDirectory(pChar(ExtractFilePath(TargetFilePath)))
  else
    ISLink.SetWorkingDirectory(pChar(RelativeToAbsolute(WorkingDir)));
  //DesktopPath
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder);
  //Save link
  FileName := PathDelim + FileName;
  LinkName := PWChar(InFolder + FileName);
  IPFile.Save(PWChar(LinkName), false);
end;

procedure DeleteShortcutOnDesktop(FileName: String);
var
  PIDL        : PItemIDList;
  DesktopPath : array[0..MAX_PATH] of Char;
  LinkName    : String;
begin
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, DesktopPath);
  FileName := PathDelim + FileName;
  LinkName := PWChar(DesktopPath + FileName);
  if (FileExists(LinkName)) then
    DeleteFile(LinkName);
end;

function GetShortcutTarget(LinkFileName:String;ShortcutType: TShortcutField):String;
var
  ISLink    : IShellLink;
  IPFile    : IPersistFile;
  WidePath  : PWideChar;
  Info      : Array[0..MAX_PATH] of Char;
  wfs       : TWin32FindData;
begin
  CoCreateInstance(CLSID_ShellLink,nil,CLSCTX_INPROC_SERVER,IShellLink,ISLink);
  if ISLink.QueryInterface(IPersistFile, IPFile) = 0 then
  begin
    {$IFDEF UNICODE}
    WidePath := PWideChar(LinkFileName);
    {$ELSE}
    MultiByteToWideChar(CP_ACP,MB_PRECOMPOSED,PChar(LinkFileName),-1,@WidePath,MAX_PATH);
    {$ENDIF}
    //Get pathexe, parameters or working directory from shortcut
    IPFile.Load(WidePath, STGM_READ);
    case ShortcutType of
     sfPathExe    : ISLink.GetPath(@info,MAX_PATH,wfs,SLGP_UNCPRIORITY);
     sfParameter  : ISLink.GetArguments(@info,MAX_PATH);
     sfWorkingDir : ISLink.GetWorkingDirectory(@info,MAX_PATH);
    end;
    Result := info
  end
  else
    Result := LinkFileName;
end;

procedure RenameShortcutOnDesktop(OldFileName, FileName: String);
var
  PIDL        : PItemIDList;
  DesktopPath : array[0..MAX_PATH] of Char;
  sDesktopPath : string;
begin
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, DesktopPath);
  sDesktopPath := DesktopPath;
  RenameFile(sDesktopPath + PathDelim + OldFileName,sDesktopPath + PathDelim + FileName);
end;

function AbsoluteToRelative(APath: String): string;
var
  TempPath: string;
begin
  TempPath := LowerCase(APath);
  if (pos(ExcludeTrailingPathDelimiter(SUITE_WORKING_PATH),TempPath) <> 0) then
    APath := StringReplace(APath, ExcludeTrailingPathDelimiter(SUITE_WORKING_PATH), CONST_PATH_ASUITE, [rfIgnoreCase,rfReplaceAll])
  else
    if pos(SUITE_DRIVE,TempPath) <> 0 then
      APath := StringReplace(APath, SUITE_DRIVE, CONST_PATH_DRIVE, [rfIgnoreCase,rfReplaceAll]);
  Result := APath;
end;

function RelativeToAbsolute(APath: String): string;
var
  EnvVar: String;
begin
  //CONST_PATH_ASuite = Launcher's path
  APath := StringReplace(APath, CONST_PATH_ASUITE, SUITE_WORKING_PATH, [rfIgnoreCase,rfReplaceAll]);
  //CONST_PATH_DRIVE = Launcher's Drive (ex. ASuite in H:\Software\asuite.exe, CONST_PATH_DRIVE is H: )
  APath := StringReplace(APath, CONST_PATH_DRIVE, SUITE_DRIVE, [rfIgnoreCase,rfReplaceAll]);
  //Remove double slash (\)
  if Pos('\\', APath) <> 1 then
    APath := StringReplace(APath, '\\', PathDelim, [rfReplaceAll]);
  //Replace environment variable
  if (pos('%',APath) <> 0) then
  begin
    EnvVar := APath;
    Delete(EnvVar,1,pos('%',EnvVar));
    EnvVar := Copy(EnvVar,1,pos('%',EnvVar) - 1);
    APath := StringReplace(APath, '%' + EnvVar + '%', GetEnvironmentVariable(EnvVar), [rfIgnoreCase,rfReplaceAll]);
  end;
  //If APath exists, expand it in absolute path (to avoid the "..")
  if (FileExists(APath) or SysUtils.DirectoryExists(APath)) and (Length(APath) <> 2) then
    Result := ExpandFileName(APath)
  else
    Result := APath;
end;

procedure EjectDialog(Sender: TObject);
var
  WindowsPath : string;
begin
  //Call "Safe Remove hardware" Dialog
  WindowsPath := GetEnvironmentVariable('WinDir');
  if FileExists(PChar(WindowsPath + '\System32\Rundll32.exe')) then
  begin
    ShellExecute(0,'open',
                 PChar(WindowsPath + '\System32\Rundll32.exe'),
                 PChar('Shell32,Control_RunDLL hotplug.dll'),
                 PChar(WindowsPath + '\System32'),SW_SHOWNORMAL);
  end;
  //Close ASuite
  frmMain.miExitClick(Sender);
end;

function ExtractDirectoryName(const Filename: string): string;
var
  AList : TStringList;
begin
  AList := TStringList.create;
  try
    StrToStrings(Filename,PathDelim,AList);
    if AList.Count > 1 then
      Result := AList[AList.Count - 1]
    else
      Result := '';
  finally
    AList.Free;
  end;
end;

procedure SetASuiteAtWindowsStartup;
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run',False) then
        if Not(ValueExists(APP_NAME)) then
          WriteString(APP_NAME,(Application.ExeName));
    end
  finally
    Registry.Free;
  end;
end;

procedure DeleteASuiteAtWindowsStartup;
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run',False) then
        DeleteValue(APP_NAME)
    end
  finally
    Registry.Free;
  end;
end;

function GetCorrectWorkingDir(Default: string): string;
var
  sPath: String;
begin
  Result := Default;
  sPath := IncludeTrailingBackslash(SUITE_PATH + sPath);
  if SysUtils.DirectoryExists(sPath) then
    Result := sPath;
end;

end.
