unit Utility.FileFolder;

{$MODE DelphiUnicode}

interface

uses
  Kernel.Consts, LCLIntf, LCLType, SysUtils, Classes, Kernel.Enumerations,
  FileUtil, {$IFDEF Windows}ComObj, ShlObj, {$ELSE} FakeActiveX, {$ENDIF} Dialogs,
  LazFileUtils, HlpHashFactory;

{ Folders }
function BrowseForFolder(const InitialDir: String = ''; const Caption: String = ''): String;
function DirToPath(const Dir: string): string;
function IsDirectory(const DirName: string): Boolean;
function IsFlagSet(const Flags, Mask: Integer): Boolean;

{ Files }
procedure DeleteOldBackups(const MaxNumber: Integer);
function DeleteFiles(const Dir, Wildcard: string): Integer;
function ListFiles(const Dir, Wildcard: string; const List: Classes.TStrings): Boolean;
function GetFileXXHash32(const FileName: String): Integer;
function ExtractFileNameEx(const AFileName: String): string;
function ExtractFileExtEx(const AFileName: String): string;

{ Desktop shortcut }
function  GetUrlTarget(const AFileName: String; ShortcutType: TShortcutField): String;

implementation

uses
  IniFiles, FileInfo, Kernel.Instance, Kernel.Logger, Kernel.ResourceStrings
  {$IFDEF UNIX}, BaseUnix{$ENDIF}, Kernel.ShellLink;

function BrowseForFolder(const InitialDir: String; const Caption: String): String;
var
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

function GetFileXXHash32(const FileName: String): Integer;
begin
  Result := 0;

  try
    if (FileName <> '') and not IsUNCPath(FileName) and FileExists(FileName) and (FileSize(FileName) > 0) then
      Result := THashFactory.THash32.CreateXXHash32().ComputeFile(FileName).GetInt32();
  except
    on E : Exception do
      TASuiteLogger.Exception(E, Format(msgGenerateFileHashError, [FileName]));
  end;
end;

function ExtractFileNameEx(const AFileName: String): string;
var
  VersionInfo : TFileVersionInfo;
  sPath, strFileDescription, strProductName : String;
begin
  sPath := ASuiteInstance.Paths.RelativeToAbsolute(AFileName);
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

function ExtractFileExtEx(const AFileName: String): string;
begin
  Result := LowerCase(ExtractFileExt(AFileName));
end;

procedure DeleteOldBackups(const MaxNumber: Integer);
var
  BackupList   : TStringList;
  BackupSearch : TSearchRec;
  I            : Integer;
begin
  BackupList := TStringList.Create;
  if FindFirst(ASuiteInstance.Paths.SuitePathBackup + APP_NAME + '_*' + EXT_SQLBCK, faAnyFile, BackupSearch) = 0 then
  begin
    repeat
      BackupList.Add(BackupSearch.Name);
    until
      SysUtils.FindNext(BackupSearch) <> 0;
    SysUtils.FindClose(BackupSearch);
  end;
  BackupList.Sort;
  for I := 1 to BackupList.Count - MaxNumber do
    SysUtils.DeleteFile(ASuiteInstance.Paths.SuitePathBackup + BackupList[I - 1]);
  BackupList.Free;
end;

function GetUrlTarget(const AFileName: String; ShortcutType: TShortcutField): String;
var
  IniFile: TIniFile;
begin
  //.url files exists only in Windows
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
