unit Utility.FileFolder;

{$MODE DelphiUnicode}

interface

uses
  Kernel.Consts, LCLIntf, LCLType, SysUtils, Classes, Kernel.Enumerations,
  FileUtil, {$IFDEF Windows}ComObj, ActiveX, ShlObj, Windows,{$ELSE} FakeActiveX, {$ENDIF} Dialogs,
  LazFileUtils;

{ Folders }
function BrowseForFolder(const InitialDir: String; const Caption: String = ''): String;
function DirToPath(const Dir: string): string;
function IsDirectory(const DirName: string): Boolean;
function IsFlagSet(const Flags, Mask: Integer): Boolean;

{ Files }
procedure DeleteOldBackups(const MaxNumber: Integer);
function DeleteFiles(const Dir, Wildcard: string): Integer;
function ListFiles(const Dir, Wildcard: string; const List: Classes.TStrings): Boolean;
function GetFileCRC32(const FileName: String): Integer;
function ExtractFileNameEx(const AFileName: String): string;

{ Desktop shortcut }
procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
procedure DeleteShortcutOnDesktop(const FileName: String);
function  GetShortcutTarget(const LinkFileName: String; ShortcutType: TShortcutField):String;
function  GetUrlTarget(const AFileName: String; ShortcutType: TShortcutField): String;

implementation

uses
  AppConfig.Main, IniFiles, FCRC32, FileInfo;

function BrowseForFolder(const InitialDir: String; const Caption: String): String;
var
  Path: string;
  Dialog: TSelectDirectoryDialog;
begin
  Result := '';
  //Call Browse for folder dialog and get new path
  Dialog := TSelectDirectoryDialog.Create(nil);
  try
    Dialog.InitialDir := ExcludeTrailingPathDelimiter(InitialDir);
    if Dialog.Execute then
      Result := Dialog.FileName;
  finally
    Dialog.Free;
  end;
end;

function DirToPath(const Dir: string): string;
begin
  if (Dir <> '') and (Dir[Length(Dir)] <> '\') then
    Result := Dir + '\'
  else
    Result := Dir;
end;

function IsDirectory(const DirName: string): Boolean;
var
  Attr: Integer;  // directory's file attributes
begin
  Attr := SysUtils.FileGetAttr(DirName);
  Result := (Attr <> -1) and IsFlagSet(Attr, SysUtils.faDirectory);
end;

function IsFlagSet(const Flags, Mask: Integer): Boolean;
begin
  Result := Mask = (Flags and Mask);
end;

function DeleteFiles(const Dir, Wildcard: string): Integer;
var
  Files: Classes.TStringList; // stores files to be deleted
  I: Integer;                 // loops thru files in folder
  AFile: string;              // a file to be deleted
  Path: string;               // path to directory
  Attr: Integer;              // attributes of a file
begin
  Result := 0;
  // Create list to stores files to be deleted
  Files := Classes.TStringList.Create;
  try
    // List files per file spec into string list
    if not ListFiles(Dir, Wildcard, Files) then
      Exit;
    // Get path of directory containing files
    Path := DirToPath(Dir);
    // Loop through all files
    for I := 0 to Pred(Files.Count) do
    begin
      // Get name and attributes of file to be deleted
      AFile := Path + Files[I];
      Attr := SysUtils.FileGetAttr(AFile);
      // Delete file if it is not a directory
      if (Attr and SysUtils.faDirectory = 0) then
      begin
        if SysUtils.DeleteFile(AFile) then
          // File deleted: count it
          Inc(Result);
      end;
    end;
  finally
    // Tidy up
    Files.Free;
  end;
end;

function ListFiles(const Dir, Wildcard: string; const List: Classes.TStrings): Boolean;
var
  FileSpec: string;         // search file specification
  SR: SysUtils.TSearchRec;  // file search result
  Success: Integer;         // success code for FindXXX routines
begin
  Assert(Assigned(List));
  // Check if true directory and exit if not
  Result := IsDirectory(Dir);
  if not Result then
    Exit;
  // Build file spec from directory and wildcard
  FileSpec := DirToPath(Dir);
  if Wildcard = '' then
    FileSpec := FileSpec + '*.*'
  else
    FileSpec := FileSpec + Wildcard;
  // Initialise search for matching files
  Success := SysUtils.FindFirst(FileSpec, SysUtils.faAnyFile, SR);
  try
    // Loop for all files in directory
    while Success = 0 do
    begin
      // only add true files or directories to list
      if (SR.Name <> '.') and (SR.Name <> '..') then
        List.Add(SR.Name);
      // get next file
      Success := SysUtils.FindNext(SR);
    end;
  finally
    // Tidy up
    SysUtils.FindClose(SR);
  end;
end;

function GetFileCRC32(const FileName: String): Integer;
var
  ErrCode: Word;
  Buffer: Array[1..65521] of byte;
begin
  ErrCode := 0;
  Result := 0;

  if FileName <> '' then
    FCRC32File(FileName, Result, Buffer, SizeOf(Buffer), ErrCode);
end;

function ExtractFileNameEx(const AFileName: String): string;
var
  VersionInfo : TFileVersionInfo;
  sPath, strFileDescription, strProductName : String;
begin
  sPath := Config.Paths.RelativeToAbsolute(AFileName);
  Result := ExtractFileName(sPath);

  VersionInfo := TFileVersionInfo.Create(nil);
  try
    VersionInfo.FileName := sPath;
    strFileDescription := VersionInfo.VersionStrings.Values['FileDescription'];
    if (strFileDescription <> '') then
      Result := strFileDescription
    else begin
      strProductName := VersionInfo.VersionStrings.Values['ProductName'];
      if (strProductName <> '') then
        Result := strProductName;
    end;
  finally
    VersionInfo.Free;
  end;
end;

procedure DeleteOldBackups(const MaxNumber: Integer);
var
  BackupList   : TStringList;
  BackupSearch : TSearchRec;
  I            : Integer;
begin
  BackupList := TStringList.Create;
  if FindFirst(Config.Paths.SuitePathBackup + APP_NAME + '_*' + EXT_SQLBCK, faAnyFile, BackupSearch) = 0 then
  begin
    repeat
      BackupList.Add(BackupSearch.Name);
    until
      SysUtils.FindNext(BackupSearch) <> 0;
    SysUtils.FindClose(BackupSearch);
  end;
  BackupList.Sort;
  for I := 1 to BackupList.Count - MaxNumber do
    SysUtils.DeleteFile(Config.Paths.SuitePathBackup + BackupList[I - 1]);
  BackupList.Free;
end;

{$IFDEF MSWINDOWS}
procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
var
  IObject  : IUnknown;
  ISLink   : IShellLinkW;
  IPFile   : IPersistFile;
  PIDL     : PItemIDList;
  InFolder : array[0..MAX_PATH] of Char;
begin
  //Create objects
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink  := IObject as IShellLinkW;
  IPFile  := IObject as IPersistFile;
  //Create link
  ISLink.SetPath(pChar(TargetFilePath));
  ISLink.SetArguments(pChar(Params));
  if WorkingDir = '' then
    ISLink.SetWorkingDirectory(pChar(ExtractFilePath(TargetFilePath)))
  else
    ISLink.SetWorkingDirectory(pChar(WorkingDir));
  //DesktopPath
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDListW(PIDL, InFolder);
  //Save link
  IPFile.Save(PWChar(AppendPathDelim(InFolder) + FileName), false);
end;
{$ELSE}
{$IFDEF UNIX}
procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
begin
  fpSymlink(TargetFilePath, AppendPathDelim(GetUserDir + 'Desktop') + Filename);
end;
{$ENDIF UNIX}
{$ENDIF MSWINDOWS}

procedure DeleteShortcutOnDesktop(const FileName: String);
var
  {$IFDEF MSWINDOWS}
  PIDL        : PItemIDList;
  {$ENDIF}
  DesktopPath : array[0..MAX_PATH] of Char;
  LinkName    : String;
begin
  {$IFDEF MSWINDOWS}
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDListW(PIDL, DesktopPath);
  {$ELSE}
  DesktopPath := AppendPathDelim(GetUserDir + 'Desktop');
  {$ENDIF}
  LinkName := PChar(AppendPathDelim(DesktopPath) + FileName);
  if (FileExists(LinkName)) then
    SysUtils.DeleteFile(LinkName);
end;

{$IFDEF MSWINDOWS}
function GetShortcutTarget(const LinkFileName: String; ShortcutType: TShortcutField):String;
var
  ISLink    : IShellLinkW;
  IPFile    : IPersistFile;
  WidePath  : PChar;
  Info      : Array[0..MAX_PATH] of Char;
  wfs       : WIN32_FIND_DATAW;
begin
CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLinkW, ISLink);
  if ISLink.QueryInterface(IPersistFile, IPFile) = 0 then
  begin
    WidePath := PChar(LinkFileName);
    //Get pathexe, parameters or working directory from shortcut
    IPFile.Load(WidePath, STGM_READ);
    case ShortcutType of
     sfPathFile   : ISLink.GetPath(@info,MAX_PATH,@wfs,SLGP_UNCPRIORITY);
     sfParameter  : ISLink.GetArguments(@info,MAX_PATH);
     sfWorkingDir : ISLink.GetWorkingDirectory(@info,MAX_PATH);
    end;
    Result := info
  end
  else
    Result := LinkFileName;
end;
{$ELSE}
{$IFDEF UNIX}
function GetShortcutTarget(const LinkFileName: String; ShortcutType: TShortcutField):String;
begin
  Result := ReadAllLinks(LinkFileName, False);
end;
{$ENDIF UNIX}
{$ENDIF}

function GetUrlTarget(const AFileName: String; ShortcutType: TShortcutField): String;
var
  IniFile: TIniFile;
begin
  //TODO: Check in linux
  IniFile := TIniFile.Create(AFileName);
  try
    case ShortcutType of
      sfPathFile   : Result := IniFile.ReadString('InternetShortcut','URL', AFileName);
      sfWorkingDir : Result := IniFile.ReadString('InternetShortcut','WorkingDirectory', '');
      sfPathIcon   : Result := IniFile.ReadString('InternetShortcut','IconFile', '');
    end;
  finally
    IniFile.Free;
  end;
end;

end.
