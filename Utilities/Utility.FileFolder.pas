unit Utility.FileFolder;

interface

uses
  Kernel.Consts, Windows, SysUtils, Classes, Kernel.Enumerations, ShlObj, ActiveX,
  ComObj, FileCtrl;

{ Folders }
function  BrowseForFolder(const InitialDir: String; const Caption: String = ''): String;
function DirToPath(const Dir: string): string;
function IsDirectory(const DirName: string): Boolean;
function IsFlagSet(const Flags, Mask: Integer): Boolean;

{ Files }
procedure DeleteOldBackups(const MaxNumber: Integer);
function DeleteFiles(const Dir, Wildcard: string): Integer;
function ListFiles(const Dir, Wildcard: string; const List: Classes.TStrings): Boolean;

{ Desktop shortcut }
procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
procedure DeleteShortcutOnDesktop(const FileName: String);
function  GetShortcutTarget(const LinkFileName:String;ShortcutType: TShortcutField):String;
procedure RenameShortcutOnDesktop(const OldFileName, FileName: String);

implementation

uses
  AppConfig.Main;

function BrowseForFolder(const InitialDir: String; const Caption: String): String;
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

procedure DeleteOldBackups(const MaxNumber: Integer);
begin
  DeleteFiles(Config.Paths.SuitePathBackup, APP_NAME + '_*' + EXT_SQLBCK);
end;

procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
var
  IObject  : IUnknown;
  ISLink   : IShellLink;
  IPFile   : IPersistFile;
  PIDL     : PItemIDList;
  InFolder : array[0..MAX_PATH] of Char;
  sPath    : String;
begin
  //Relative path to Absolute path
  sPath := TargetFilePath;
  if pos(':',sPath) = 0 then
    sPath := Config.Paths.SuitePathWorking + sPath;
  //Create objects
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink  := IObject as IShellLink;
  IPFile  := IObject as IPersistFile;
  //Create link
  ISLink.SetPath(pChar(sPath));
  ISLink.SetArguments(pChar(Params));
  if WorkingDir = '' then
    ISLink.SetWorkingDirectory(pChar(ExtractFilePath(sPath)))
  else
    ISLink.SetWorkingDirectory(pChar(WorkingDir));
  //DesktopPath
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder);
  //Save link
  IPFile.Save(PWChar(IncludeTrailingPathDelimiter(InFolder) + FileName), false);
end;

procedure DeleteShortcutOnDesktop(const FileName: String);
var
  PIDL        : PItemIDList;
  DesktopPath : array[0..MAX_PATH] of Char;
  LinkName    : String;
begin
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, DesktopPath);
  LinkName := PWChar(IncludeTrailingPathDelimiter(DesktopPath) + FileName);
  if (FileExists(LinkName)) then
    DeleteFile(LinkName);
end;

function GetShortcutTarget(const LinkFileName:String; ShortcutType: TShortcutField):String;
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

procedure RenameShortcutOnDesktop(const OldFileName, FileName: String);
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

end.
