unit Utility.FileFolder;

interface

uses
  Kernel.Consts, Windows, SysUtils, Classes, Kernel.Enumerations, ShlObj, ActiveX, ComObj,
  FileCtrl;

{ Browse }
function  BrowseForFolder(const InitialDir: String; const Caption: String = ''): String;

{ Files }
procedure DeleteFiles(const PathDir, FileName: String);
procedure DeleteOldBackups(const MaxNumber: Integer);

{ Folders }                 
procedure CheckBackupFolder;
procedure CheckCacheFolders;
function  GetNumberSubFolders(const FolderPath: String): Integer;
procedure RemoveCacheFolders;

{ Desktop shortcut }
procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
procedure DeleteShortcutOnDesktop(const FileName: String);
function  GetShortcutTarget(const LinkFileName:String;ShortcutType: TShortcutField):String;
procedure RenameShortcutOnDesktop(const OldFileName, FileName: String);

implementation

uses
  Utility.System, AppConfig.Main;

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

procedure DeleteFiles(const PathDir, FileName: String);
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

procedure DeleteOldBackups(const MaxNumber: Integer);
var
  BackupList   : TStringList;
  BackupSearch : TSearchRec;
  I            : Integer;
begin
  BackupList := TStringList.Create;
  if FindFirst(Config.Paths.SuitePathBackup + APP_NAME + '_*' + EXT_SQLBCK,faAnyFile,BackupSearch) = 0 then
  begin
    repeat
      BackupList.Add(BackupSearch.Name);
    until
      FindNext(BackupSearch) <> 0;
    FindClose(BackupSearch);
  end;
  BackupList.Sort;
  for I := 1 to BackupList.Count - MaxNumber do
    DeleteFile(Config.Paths.SuitePathBackup + BackupList[I - 1]);
  BackupList.Free;
end;

procedure CheckBackupFolder;
begin
  //Check if folder backup exists, else create it
  SysUtils.ForceDirectories(Config.Paths.SuitePathBackup);
end;         

procedure CheckCacheFolders;
begin
  //Check if folder cache exists, else create it
  SysUtils.ForceDirectories(Config.Paths.SuitePathCache);
  SysUtils.ForceDirectories(Config.Paths.SuitePathCacheLarge);
end;

function GetNumberSubFolders(const FolderPath: String): Integer;
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

procedure RemoveCacheFolders;
begin
  //Delete all file icon-cache and folder cache
  if (SysUtils.DirectoryExists(Config.Paths.SuitePathCache)) then
  begin
    if (SysUtils.DirectoryExists(Config.Paths.SuitePathCacheLarge)) then
    begin
      DeleteFiles(Config.Paths.SuitePathCacheLarge,'*.*');
      RemoveDir(Config.Paths.SuitePathCacheLarge);
    end;
    DeleteFiles(Config.Paths.SuitePathCache,'*.*');
    RemoveDir(Config.Paths.SuitePathCache);
  end;
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
