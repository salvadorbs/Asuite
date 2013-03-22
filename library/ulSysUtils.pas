{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit ulSysUtils;

{$MODE Delphi}

interface

uses
  AppConfig, Windows, ShellApi, SysUtils, Classes, ulEnumerations, Registry,
  ShlObj, ActiveX, ComObj, Forms, Dialogs, FileUtil;

{ Browse }
function  BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
function  BrowseForFolder(const Caption, InitialDir: String): String;

{ Check functions }
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
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

{ Relative & Absolute path }
function AbsoluteToRelative(APath: String): string;
function RelativeToAbsolute(APath: String): string;

{ Registry }
procedure SetASuiteAtWindowsStartup;
procedure DeleteASuiteAtWindowsStartup;

{ Misc }
function ExtractDirectoryName(const Filename: string): string;
function GetCorrectWorkingDir(Default: string): string;

implementation

uses
  ulStringUtils;

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
  SelectDirectoryDialog: TSelectDirectoryDialog;
begin
  Result := '';
  //Get Path and delete \ in last char. Example c:\xyz\ to c:\xyz
  Path   := ExcludeTrailingPathDelimiter(InitialDir);
  //Create TSelectDirectoryDialog and execute it
  SelectDirectoryDialog := TSelectDirectoryDialog.Create(nil);
  try
    SelectDirectoryDialog.InitialDir := Path;
    SelectDirectoryDialog.Execute;
    Result := SelectDirectoryDialog.FileName;
  finally
    SelectDirectoryDialog.Free;
  end;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
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
  Result := ((FileExistsUTF8(Path)) or (DirectoryExistsUTF8(Path)) or
             (IsUrl(Path)));
end;

procedure DeleteFiles(PathDir, FileName: String);
var
  Search : TSearchRec;
begin
  //Delete file with FileName in folder PathDir (path relative)
  if FindFirstUTF8(SUITE_WORKING_PATH + PathDir + FileName,faAnyFile,Search) = 0 then
  begin
    repeat
      DeleteFileUTF8(PathDir + Search.Name); 
    until
      FindNextUTF8(Search) <> 0;
    FindCloseUTF8(Search); 
  end;
end;

procedure DeleteOldBackups(MaxNumber: Integer);
var
  BackupList   : TStringList;
  BackupSearch : TSearchRec;
  I            : Integer;
begin
  BackupList := TStringList.Create;
  if FindFirstUTF8(SUITE_BACKUP_PATH + APP_NAME + '_*' + EXT_SQLBCK,faAnyFile,BackupSearch) = 0 then
  begin
    repeat
      BackupList.Add(BackupSearch.Name);
    until
      FindNextUTF8(BackupSearch) <> 0;
    FindCloseUTF8(BackupSearch); 
  end;
  BackupList.Sort;
  for I := 1 to BackupList.Count - MaxNumber do
    DeleteFileUTF8(SUITE_BACKUP_PATH + BackupList[I - 1]);
  BackupList.Free;
end;

procedure CreateShortcutOnDesktop(FileName: String;TargetFilePath, Params, WorkingDir: String);
var
  IObject  : IUnknown;
  ISLink   : IShellLink;
  IPFile   : IPersistFile;
  PIDL     : PItemIDList;
  InFolder : array[0..MAX_PATH] of Char;
  LinkName : WideString;
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
  LinkName := InFolder + PathDelim + FileName;
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
  LinkName := DesktopPath + PathDelim + FileName;
  if (FileExistsUTF8(LinkName)) then
    DeleteFileUTF8(LinkName);
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
     //AnsiString -> WideChar
     WidePath := PWideChar(UTF8Decode(LinkFileName));
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

function AbsoluteToRelative(APath: String): string;
begin
  APath := LowerCase(APath);
  if (pos(ExcludeTrailingPathDelimiter(SUITE_WORKING_PATH),APath) <> 0) then
    APath := StringReplace(APath, ExcludeTrailingPathDelimiter(SUITE_WORKING_PATH), CONST_PATH_ASUITE, [rfReplaceAll])
  else
    if pos(SUITE_DRIVE,APath) <> 0 then
      APath := StringReplace(APath, SUITE_DRIVE, CONST_PATH_DRIVE, [rfReplaceAll]);
  Result := APath;
end;

function RelativeToAbsolute(APath: String): string;
var
  EnvVar: String;
begin
  APath := LowerCase(APath);
  //CONST_PATH_ASuite = Launcher's path
  APath := StringReplace(APath, CONST_PATH_ASUITE, SUITE_WORKING_PATH, [rfReplaceAll]);
  //CONST_PATH_DRIVE = Launcher's Drive (ex. ASuite in H:\Software\asuite.exe, CONST_PATH_DRIVE is H: )
  APath := StringReplace(APath, CONST_PATH_DRIVE, SUITE_DRIVE, [rfReplaceAll]);
  //Remove double slash (\)
  APath := StringReplace(APath, '\\', PathDelim, [rfReplaceAll]);
  //Replace environment variable
  if (pos('%',APath) <> 0) then
  begin
    EnvVar := APath;
    Delete(EnvVar,1,pos('%',EnvVar));
    EnvVar := Copy(EnvVar,1,pos('%',EnvVar) - 1);
    APath := StringReplace(APath, '%' + EnvVar + '%', GetEnvironmentVariable(EnvVar), [rfReplaceAll]);
  end;
  //If APath exists, expand it in absolute path (to avoid the "..")
  if (FileExistsUTF8(APath) or DirectoryExistsUTF8(APath)) and (Length(APath) <> 2) then
    Result := ExpandFileNameUTF8(APath)
  else
    Result := APath;
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
  if DirectoryExistsUTF8(sPath) then
    Result := sPath;
end;


end.
