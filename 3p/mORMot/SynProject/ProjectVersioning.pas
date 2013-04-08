/// File Versioning system core
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectVersioning;

(*
    This file is part of SynProject.

    Synopse SynProject. Copyright (C) 2008-2011 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SynProject is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or (at
    your option) any later version.

    SynProject is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SynProject. If not, see <http://www.gnu.org/licenses/>.

*)

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  ProjectCommons,
  SysUtils,
  Classes,
  SynZipFiles,
  ProjectSections,
  ProjectDiffUnit,
  ProjectDiff,
  SynZip;

type
  TOnCommitEvent = procedure(const aLog: string) of object;
  TOnCommitFileUpdate = procedure(aFile: integer; const action: string) of object;

  TModifiedDynArray = array of packed record
    FileName: integer;
    Age: integer;
  end;

  PVersion = ^TVersion;
  TVersion = object
    FileName: integer;    // index of 'EIA\Main.pas' in TVersions.FileNames[]
    Age: integer;         // FileAge(), 1 if file was deleted (data='deleted')
    Commit: integer;
    // first appairing Commit is stored in plain text format (not diff)
    // following Commits are incremental diffs (1=diff from 0, 2=diff from 1, etc..)
    ReaderIndex: integer; // >=0 -> Reader.Entry[ReaderIndex] exists; -1 -> new
    Data: string;         // <>'' -> plain (Commmit=0) or diff (1..9999)
  end;

  TVersions = class
  private
    fSCR: TSectionsStorage;
    fCommitPVCSAddDiff: boolean; // true -> PVCS note will contain text diff
    fLog: TOnCommitEvent;
    procedure WriteIfModified(forceFileAge: integer = 0);
    function FillStrings(SourceDir: string; const Filters1, Filters2: TStringDynArray;
      var Deleted: TIntegerDynArray;
      var Modified: TModifiedDynArray): boolean; // true if some file was modified
    function GetVersionDiffOrPlain(Index: integer): string;
    procedure VersToFile(var Vers, Files: TIntegerDynArray; n: integer);
    function GetSCR: TSectionsStorage;
    function SameParams(V2: TVersions): boolean;
    procedure CopyParams(Dest: TSection);
  public
    // filled from Commits [Params] section
    ReleaseName,  // 'Product New Version'
    DefaultPath,  // 'D:\DEV\'
    IgnorePath,   // 'CVS,BACKUP,PASDOC,GG'
    IgnoreFile: string; // '.~'  -> add file if pos(IgnoreFile,FileName)=0
    PROFileName,            // D:\Dev\Synopse\Documents\Product New Version\Product New Version.pro
    MANFileName,            // D:\Dev\Synopse\Product System.man
    SCRFileName: string;    // D:\Dev\Synopse\Product System.scr
    FilterDefault: TStringDynArray; // ['*.pas','*.php','*.dfm','*.bpg'..]
    Count: integer;
    FileNames: TAnsiStrings; // used internaly for TVersion.FileName
    Values: array of TVersion;
    Reader: TZipReader;
    Commits: TSectionsStorage; // 'commits' ZipName contains params and commits info
    Params: TSection; // Commits [Params] section
    constructor Create(const aFileName: string);
    constructor CreateFrom(Vers: TVersions; const aTitle, aFileName: string);
    destructor Destroy; override;
    procedure ReadParamsFromCommits;
    function FileName: string;
    procedure SCRForceCreate;
    procedure MANForceCreate;
    function GetVersionIndex(aFileName: integer; aVersion: integer): integer;
    function GetVersion(aFileName: integer; aVersion: integer): PVersion;
    function GetLastVersionIndex(aFileName: integer): integer;
    function CreateNewVersion(aFileName: integer; aVersion: integer): integer;
    function UpdateVersion(aFileName: integer; aDate, aVersion: integer;
      const aDiff: string): boolean; // true if created a new one
    function DoUpdate(const UpdateNumbers: array of integer;
      var Deleted: TIntegerDynArray; var Modified: TModifiedDynArray): boolean;
    function DoCommit(const UpdateNumbers: array of integer;
      const Description: string;
      const Comment: string = ''; SCR: integer=0;
      Log: TOnCommitEvent=nil; OnFile: TOnCommitFileUpdate=nil): integer; // return Commit number
    function DoBackup(const BackupNumbers: array of integer; Log: TOnCommitEvent): boolean; overload;
    function DoBackup(BackupNumber: integer; Log: TOnCommitEvent): boolean; overload; // get Backup*
    function GetBackupFileName(const com: string; out path: string): string;
    function GetVersionData(aFileName: integer; aVersion: integer): string; // undiff if necessary
    function ZipName(Index: integer): string;
    function GetVersions(const aVersion: integer; var Vers: TIntegerDynArray;
      withoutDel: boolean=true): integer;
    function GetVersionsModified(const aVersion: integer; var Vers: TIntegerDynArray): integer;
    procedure GetVersionValues(const aVersion: integer; var Files: TIntegerDynArray; onlyModified: boolean);
    procedure GetVersionValuesDiff(var aVersion1, aVersion2: integer; var Files: TIntegerDynArray);
    function LastCommit: integer;
    function CommitDescription(Commit: integer): string;
    function CommitToString(Commit: integer): string;
    function PreviousCommit(aFileName, aCommit: integer): integer;
    function PreviousVersion(Vers: PVersion): PVersion;
    function ExtractDefaultPath(const aFileName: string): string;
    property SCR: TSectionsStorage read GetSCR;
      // content of 'D:\DEV\Synopse\FileName.scr'
  end;

{ Commits format:
   [Params]
   ReleaseName=Product New Version
   LastCommit=C       -> stored in Hex
   DefaultPath=D:\DEV
   IgnorePath=CVS,BACKUP,PASDOC,GG
   IgnoreFile=.~
   SCR=Synopse\Documents\FileName.scr
   PRO=Synopse\Documents\Product New Version\Product New Version.pro
   FilterDefault=.pas.php.dfm.bpg.cfg.dpr.inc.dof...
   LastUpdate=034
   Update0=Documentation;Synopse\Documents;*.*
   Update1=Global Library;Lib;FilterDefault
   Update2=Synopse Library;Synopse\Common;FilterDefault,BalloonHint.dcu,BHWndProcHook.dcu,*.pro,*.sad
   Update3=Test Software;Synopse\Test;FilterDefault,*.dll
   LastBackup=01
   BackupDir0=Local backup,012345,D:\-=- Backup -=-\Synopse\
   CommitPVCSAddDiff=True -> add full text diff in Note

   [People]
   AB=Arnaud Bouchez,arnaud.bouchez@synopse.info

   [0]     -> commits begin with 0, stored in Hex
   Description=First modification with new version
   SCR=11  -> justified by [11] section in FileName.scr (set in [Params].SCR)

   [1]
   ....
   [C]
   Description=Last Commit

  SCR format (used during investigation, then can be easily pasted in ProjectEdit):
  [11]     <- SCR # in BRS Tracker
  Description=EIA Highlight samples in list is different
  ShortName=
  Request=SCR #55
  Risk=1,1,3,Franck Grandemange+Arnaud Bouchez,UI only change, affects report

}


// get 'My documents' path for current user
function GetMyDocuments: string;

// generic function to retrieve directory value from OS
function GetShellFolderPath(const FolderID: Integer): AnsiString;

// per-user settings directory
// Vista: C:\Users\username\AppData\Roaming
// XP:    C:\Documents and Settings\username\Application Data
function GetAppDataPath: AnsiString;

// common data directory
// Vista: C:\ProgramData
// XP:    C:\Documents and Settings\All Users\Application Data
function GetCommonAppDataPath: AnsiString;

// current User "Start Menu\Programs" directory
// Vista: C:\Users\User\AppData\Roaming\Microsoft\Windows\Start Menu\Programs
// XP:    C:\Documents and Settings\username\Start Menu\Programs
function GetStartMenuPath: AnsiString;

// common "Start Menu\Programs" directory
// Vista: C:\ProgramData\Microsoft\Windows\Start Menu\Programs
// XP:    C:\Documents and Settings\All Users\Start Menu\Programs
function GetCommonStartMenuPath: AnsiString;

// common exe directory
// Vista+XP: C:\Program Files (ReadOnly on Vista)
function GetProgramFilesPath: AnsiString;

// select a directory using Windows default dialog
function SelectDirectory(Handle: integer; const Caption: string; var Directory: string): Boolean;


resourcestring
  sChangeKindStr = 'none,added,deleted,modified';
  sChangeTimeNN = '%s, on %s';


implementation

uses
  ShlObj, ActiveX, // for SelectDirectory()
  ProjectRTF,
  Contnrs;

procedure FreePidl(pidl: PItemIDList);
var allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;

function GetShellFolderPath(const FolderID: Integer): AnsiString;
var pidl: PItemIDList;
    Buffer: array[0..MAX_PATH] of AnsiChar;
begin
  Result := '';
  if Win32MajorVersion<4 then Exit;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, pidl)) then begin
    if SHGetPathFromIDListA(pidl, Buffer) then begin
      Result := Buffer;
      if Result[length(Result)]<>'\' then
        Result := Result+'\';
    end;
    FreePidl(pidl);
  end;
end;

function GetMyDocuments: string;
// get 'My documents' path for current user
begin
  Result := GetShellFolderPath(CSIDL_PERSONAL);
end;

function GetAppDataPath: AnsiString; // %USERPROFILE%\Application Data (roaming)
const CSIDL_APPDATA = $001a;
begin
  result := GetShellFolderPath(CSIDL_APPDATA);
end;

function GetStartMenuPath: AnsiString;
const CSIDL_PROGRAMS = $0002;
begin
  result := GetShellFolderPath(CSIDL_PROGRAMS);
end;

function GetCommonStartMenuPath: AnsiString;
const CSIDL_COMMON_PROGRAMS = $0017;
begin
  result := GetShellFolderPath(CSIDL_COMMON_PROGRAMS);
end;

function ReadRegString(Key: DWORD; const Path, Value: AnsiString): AnsiString;
var
  l, t: DWORD;
  z: array[0..250] of char;
  k: HKey;
begin
  Result := '';
  if RegOpenKeyEx(Key, pAnsiChar(Path), 0, KEY_QUERY_VALUE, k)=ERROR_SUCCESS then
  try
    l := 250;
    t := REG_SZ;
    if RegQueryValueEx(K, pAnsiChar(Value), nil, @t, @z, @l)=ERROR_SUCCESS then
      Result := z;
  finally
    RegCloseKey(k);
  end;
end;

const
  { Do NOT localize any of these }
 {} NEWREGSTR_PATH_SETUP = 'Software\Microsoft\Windows\CurrentVersion';

function GetPathFromRegistry (const Name: pAnsiChar): AnsiString;
begin
  if Win32MajorVersion>=4 then
    Result := ReadRegString(HKEY_LOCAL_MACHINE,NEWREGSTR_PATH_SETUP,Name)
    else Result := 'C:\';
end;

function GetProgramFilesPath: AnsiString;
const CSIDL_PROGRAM_FILES = $0026;
{ Gets path of Program Files.
  Returns blank AnsiString if not found in registry. }
begin
  result := GetShellFolderPath(CSIDL_PROGRAM_FILES);
  if Result='' then
    Result := GetPathFromRegistry('ProgramFilesDir');
  if (result<>'') and (result[length(result)]<>'\') then result := result+'\';
end;

function GetCommonAppDataPath: AnsiString;
const CSIDL_COMMON_APPDATA = $0023;
begin
  result := GetShellFolderPath(CSIDL_COMMON_APPDATA); // Vista: c:\ProgramData
  if result='' then
    result := GetProgramFilesPath;
end;

function FolderDialogCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage (Wnd, BFFM_SETSELECTION, 1, lpData); // set the current directory
  result := 0;
end;

function SelectDirectory(Handle: integer; const Caption: string; var Directory: string): Boolean;
// select a directory using Windows default dialog
var bi: TBrowseInfo;
    id: PItemIDList;
    nm: array[0..MAX_PATH] of Char;
begin
  FillChar(bi, sizeof(bi), 0);
  bi.hwndOwner := Handle;
  bi.lpszTitle := pointer(Caption);
  bi.lParam := integer(pointer(Directory)); // current directory value
  bi.lpfn := FolderDialogCallBack; // set the current directory
  bi.ulFlags := BIF_RETURNONLYFSDIRS;
  id := SHBrowseForFolder(bi);
  result := id<>nil;
  if not result then
    exit;
  SHGetPathFromIDList(id, nm);
  Directory := ExcludeTrailingPathDelimiter(nm);
  FreePidl(id);
end;


{ TVersions }

function TVersions.CreateNewVersion(aFileName: integer; aVersion: integer): integer;
begin
  result := Count;
  if Count=length(Values) then
    SetLength(Values,Count+32);
  with Values[result] do begin
    FileName := aFileName;
    Commit := aVersion;
    ReaderIndex := -1; // -1 -> new entry, nothing to delete in Reader
  end;
  inc(Count);
end;

procedure TVersions.ReadParamsFromCommits;
var n: integer;
    dir, s, v: string;
    P: PChar;
begin
  Params := Commits['Params'];
  ReleaseName := Params['ReleaseName'];
  DefaultPath := IncludeTrailingPathDelimiter(Params['DefaultPath']);
  dir := GetCurrentDir;
  if DefaultPath='\' then
    DefaultPath := '' else
    SetCurrentDir(DefaultPath);
  IgnorePath := Params['IgnorePath'];
  IgnoreFile := Params['IgnoreFile'];
  SCRFileName := ExpandFileName(Params['SCR']);
  MANFileName := ExpandFileName(Params['MAN']);
  PROFileName := ExpandFileName(Params['PRO']);
  SetCurrentDir(dir);
  fCommitPVCSAddDiff := isTrue(Params['CommitPVCSAddDiff']); // text diff in PVCS note
  s := Params['FilterDefault'];
  Setlength(FilterDefault,length(s) div 4+5);
  n := 0;
  P := pointer(s); // '.pas.php.dfm.....'
  while P<>nil do begin
    v := GetNextItem(P,'.'); // v='pas'
    if v='' then continue;
    if n=length(FilterDefault) then
      Setlength(FilterDefault,n+10);
    FilterDefault[n] := '*.'+v;
    inc(n);
  end;
  Setlength(FilterDefault,n);
end;

constructor TVersions.CreateFrom(Vers: TVersions; const aTitle, aFileName: string);
var i: integer;
begin
  Commits := TSectionsStorage.Create;
  Reader := TZipReader.Create(aFileName); // we need Reader.FileName
  for i := 0 to Reader.Count-1 do // this constructor erase old file content
    Reader.Entry[i].Header.signature := 0; // signature = 0 to delete all
  if Vers=nil then
    exit;
  Vers.CopyParams(Commits.GetOrCreateSection('Params',true));
  ReadParamsFromCommits;
  Params.Lines.Insert(0,'; parameters below were imported from '+Vers.FileName);
  Params.Lines.Insert(0,format('; %s from "%s"',[aTitle,ReleaseName]));
end;

constructor TVersions.Create(const aFileName: string);
var max,i,j,k,com,L: integer;
    ok: boolean;
    s: string;
begin
  Commits := TSectionsStorage.Create;
  Reader := TZipReader.Create(aFileName);
  max := Reader.Count;
  SetLength(Values,max+10);
  if max=0 then begin // new file -> minimum [Params] section
    Commits.WriteString('Params','ReleaseName','New Release');
    with Commits['Params'] do begin
      Value['DefaultPath'] := '';
      Value['FilterDefault'] :=
        '.pas.php.dfm.bpg.cfg.dpr.inc.dof.msg.bat.lng.res.rc.ini.bmp.png.css'+
        '.emf.jpg.gif.ico.obj.dpk.dcr.nsi.so.xml.dat';
      Value['IgnoreFile'] := '.~';
      Value['IgnorePath'] := 'CSV,BACKUP,PASDOC,GG';
    end;
    max := -1;
  end else begin
    if (max>1) and (Reader.Entry[max-1].ZipName<>'commits') then
      raise Exception.CreateFmt('Incorrect last entry in %s',[aFileName]);
    dec(max);
    Commits.LoadFromMemory(pointer(Reader.GetString(max)));
  end;
  ReadParamsFromCommits;
  if max<0 then
    exit;
  Reader.Entry[max].Header.signature := 0; // mark delete commits
  for i := 0 to max-1 do // very fast Values[] init from Reader.Entry[]
  with Reader.Entry[i] do begin
    ok := false;
    L := length(ZipName);
    for j := L downto 1 do
      if ZipName[j]='.' then begin // search file name extension
        com := 0;
        ok := true;
        for k := j to L-1 do // convert Hex to integer = Commit
        case ZipName[k+1] of
          '0'..'9': com := com*16+Ord(ZipName[k+1])-Ord('0');
          'a'..'f': com := com*16+Ord(ZipName[k+1])+(-Ord('a')+10);
          'A'..'F': com := com*16+Ord(ZipName[k+1])+(-Ord('A')+10);
          else ok := false;
        end;
        s := copy(ZipName,1,j-1);
        with Values[i] do begin
          Commit := com;
          FileName := FileNames.Add(s,true);
          Age := Header.fileInfo.zlastMod;
          // Age=1 if file was deleted -> valid (01/01/1980) file entry
          ReaderIndex := i;
        end;
        break;
      end;
    if not ok then
      raise Exception.CreateFmt('Incorrect entry %s in %s',[ZipName,aFileName]);
  end;
  Count := max;
end;

destructor TVersions.Destroy;
begin
  WriteIfModified;
  FreeAndNil(Reader); // if not already in WriteIfModified
  FreeAndNil(Commits);
  FreeAndNil(fSCR);
  inherited;
end;

function TVersions.FileName: string;
begin
  if (self=nil) or (Reader=nil) then
    result := '' else
    result := Reader.FileName;
end;

function TVersions.GetVersionIndex(aFileName: integer; aVersion: integer): integer;
begin
  if aFileName>=0 then
  for result := 0 to Count-1 do
  with Values[result] do
    if (Commit=aVersion) and (FileName=aFileName) then
      exit;
  result := -1;
end;

function TVersions.UpdateVersion(aFileName: integer; aDate, aVersion: integer;
  const aDiff: string): boolean; // true if created a new one
var i: integer;
  begin
  i := GetVersionIndex(aFileName,aVersion);
  if i<0 then begin
    i := CreateNewVersion(aFileName,aVersion);
    result := true;
  end else
    result := false;
  Values[i].Age := aDate;
  Values[i].Data := aDiff;
end;

procedure TVersions.WriteIfModified(forceFileAge: integer = 0);
var Z: TZipWriter;
    i: integer;
    modified: boolean;
    W: TStringWriter;
begin
  if Reader=nil then
    exit; // WriteIfModified is to be called once (in Destroy or DoBackup e.g.)
  // mark entries to be deleted in Reader
  modified := Commits.Modified;
  for i := 0 to Count-1 do
    with Values[i] do
    if Data<>'' then begin // new or updated data
      modified := true;
      if ReaderIndex>=0 then // does this Data update a Reader entry?
        Reader.Entry[ReaderIndex].Header.signature := 0; // mark to be deleted
    end;
  if not modified then // nothing to write so far
    exit;
  // update file
  Z := TZipWriter.Create(Reader); // delete all Reader.Entry[] with signature=0
  for i := 0 to Count-1 do
  with Values[i] do
    if Data<>'' then // save new or updated Data
      if length(Data)<128 then // no compression for very small blocks
        Z.Add(ZipName(i),pointer(Data),length(Data),-1,nil,Age) else
        Z.Add(ZipName(i),pointer(Data),length(Data),6,nil,Age);
  Commits.SaveText(W);
  Z.Add('commits',W.DataPointer,W.len,6);
  Z.forceFileAge := forceFileAge; // TZipWriter.Destroy will call FileSetDate()
  Z.Free;
  FreeAndNil(Reader); // Reader was already unmapped -> free it
end;

function TVersions.DoCommit(const UpdateNumbers: array of integer;
  const Description, Comment: string; SCR: integer;
  Log: TOnCommitEvent; OnFile: TOnCommitFileUpdate): integer;
var last, updated, diff: string;
procedure DoLog(index: integer; Kind: TChangeKind; Age: integer);
var suffix: string;
begin
  suffix := ValAt(sChangeKindStr,integer(Kind));
  if Age<>0 then
    suffix := format(sChangeTimeNN,[suffix,DateTimeToStr(FileDateToDateTime(Age))]);
  if Assigned(Log) then
    Log(' '+FileNames.Value[index]+' '+suffix);
  if not Assigned(OnFile) then exit;
  if (Kind=ckModify) and (diff<>'=') and fCommitPVCSAddDiff then
    // if content changed -> write human-readable diff
    suffix := suffix+#13#10+DiffText(last,updated,false)+#13#10; // withLineNumber=false
  OnFile(index,suffix);
end;
var Deleted: TIntegerDynArray;  // deleted files, as FileNames[] index
    Modified: TModifiedDynArray; // new or updated files Strings[]=FileName, Objects[]=FileAge
    sec, test: string;
    i: integer;
    Tick: cardinal;
begin
  fLog := Log;
  try
  Tick := GetTickCount;
  result := -1; // no modification found -> return -1
  SetLength(Deleted,0);
  SetLength(Modified,0);
  if DoUpdate(UpdateNumbers,Deleted,Modified) then begin
    // 1. update Commits values
    result := LastCommit;
    if result<0 then
      result := 0 else
      inc(result);
    sec := IntToHex(result,0);
    Params['LastCommit'] := Sec;
    Commits.WriteString(sec,'Description',Description); // force create [Sec]
    Commits.WriteString(sec,'Date',FloatToStr(Now));
    if SCR<>0 then
      Commits[sec]['SCR'] := IntToStr(SCR);
    if Comment<>'' then
      Commits.WriteBody(sec,Comment);
    // 2. update Values[] from Deleted[]+Modified[]
    if Assigned(Log) then
      Log(format(#13#10'%s'#13#10'%s'#13#10'%d file(s) deleted',
        [Description,StringOfChar('-',length(Description)),length(Deleted)]));
    for i := 0 to high(Deleted) do begin
      DoLog(Deleted[i],ckDelete,0);
      UpdateVersion(Deleted[i],1,result,'deleted'); // Age=1 for deleted item
    end;
      // data='deleted' ensure that a zip entry is written to disk
    if Assigned(Log) then
      Log(format(#13#10'%d file(s) modified/added',[length(Modified)]));
    for i := 0 to high(Modified) do
    with Modified[i] do begin
      last := GetVersionData(FileName,maxInt); // maxInt -> will get last data
      if FileToString(DefaultPath+FileNames.Value[FileName],updated) then begin
        if last='' then begin // first time this file is Modified
          UpdateVersion(FileName,Age,result,updated);
          DoLog(FileName,ckAdd,Age);
        end else begin // -> add initial plain
          diff := DiffStreamCompress(updated, last); // otherwize, calc diff
          if diff<>'=' then
            // always check compressed diff buffer now
            if (DiffStreamExtract(diff,last,test)<>'') or
               (test<>updated) then begin
              if Assigned(Log) then
                Log('! Error getting diff for '+FileNames.Value[FileName]+
                  ' -> storing plain content');
              diff := DiffStreamCompress(updated,''); // diff error -> plain store
            end;
          UpdateVersion(FileName,Age,result,diff); // store data diff
          DoLog(FileName,ckModify,Age);
        end;
      end;
    end;
    if Assigned(Log) then begin
      Tick := GetTickCount-Tick;
      Log(format(#13#10'"%s" Commit performed in %d.%d s.',[Description,
        Tick div 1024,(Tick mod 1024)div 100])); // 1024 faster than 1000
    end;
  end;
  finally
    fLog := nil;
  end;
end;

function TVersions.DoBackup(const BackupNumbers: array of integer; Log: TOnCommitEvent): boolean;
var i: integer;
    Tick: cardinal;
begin
  Tick := GetTickCount;
  result := false;
  for i := 0 to high(BackupNumbers) do
    result := DoBackup(BackupNumbers[i], Log) or result;
  Tick := GetTickCount-Tick;
  if Assigned(Log) and result then
    Log(format(#13#10'  Backup finished in %d.%d s.',[
      Tick div 1024,(Tick mod 1024)div 100])); // 1024 faster than 1000
end;

function TVersions.SameParams(V2: TVersions): boolean;
var i: integer;
    n: string;
begin
  result := false;
  if (ReleaseName<>V2.ReleaseName) or
     (DefaultPath<>V2.DefaultPath) or
     (IgnorePath<>V2.IgnorePath) or
     (IgnoreFile<>V2.IgnoreFile) or
     (SCRFileName<>V2.SCRFileName) or
     (MANFileName<>V2.MANFileName) or
     (PROFileName<>V2.PROFileName) or
     (Params['FilterDefault']<>V2.Params['FilterDefault']) then
    exit;
  for i := 0 to 9 do begin
    n := 'Update'+IntToStr(i);
    if Params[n]<>V2.Params[n] then
      exit;
  end;
  result := true;
end;

procedure TVersions.CopyParams(Dest: TSection);
var i: integer;
    n: string;
begin // all params are added in backwards order -> written in order
  for i := 9 downto 0 do begin
    n := 'Update'+IntToStr(i);
    Dest[n] := Params[n];
  end;
  Dest['IgnoreFile'] := IgnoreFile;
  Dest['FilterDefault'] := Params['FilterDefault'];
  Dest['SCR'] := SCRFileName;
  Dest['MAN'] := MANFileName;
  Dest['PRO'] := PROFileName;
  Dest['IgnorePath'] := IgnorePath;
  Dest['DefaultPath'] := DefaultPath;
  Dest['ReleaseName'] := ReleaseName;
end;

function TVersions.DoBackup(BackupNumber: integer; Log: TOnCommitEvent): boolean;
// backup two files:
// 1. *- backup.dvs (commits by date):
//  diffs are created for each backup, but it's quick on practice,
//  and necessary since not all the backups are always updated on the same time
// 2. backup of associated .dvs (with SCR-related commits) - file is updated,
//  only newly added commits are added into disk or network: backup is very fast 
var com, origFN, FN, path, desc, commitText: string;
    Back: TVersions;
    UpdateNumbers: TIntegerDynArray;
    i, Age, ToBeUpdated: integer;
    V: PVersion;
    FHandle: integer;
    ForceFullCopy: boolean; 
begin
  result := false;
  com := trim(Params['BackupDir'+IntToStr(BackupNumber)]);
  if com='' then // BackupDir0=Local backup,012345,D:\-=- Backup -=-\Synopse\
    exit;
  fLog := Log;
  try
  // 1. Create an incremental backup (commits by date)
  desc := ValAt(com,0); // 'Local backup'
  origFN := ExtractFileName(FileName);
  FN := GetBackupFileName(com,path);
  if FN='' then
    exit; // invalid ValAt(com,2)=directory -> ignore this entry
  if FileExists(FN) then begin // existing -> update if necessary
    Back := TVersions.Create(FN);
    if not SameParams(Back) then begin // synchronize Params
      CopyParams(Back.Params);
      Back.ReadParamsFromCommits;
      Back.Commits.Modified := true;
    end;
  end else begin // not existing -> create a new copy from present
    if not DirectoryExists(path) then
      if not ForceDirectories(path) then
        exit; // impossible to create target directory -> ignore this BackupDir?=
    FHandle := FileCreate(FN);
    if FHandle=-1 then
      exit; // impossible to create this file -> ignore this BackupDir?=
    FileClose(FHandle);
    DeleteFile(FN);
    Back := TVersions.CreateFrom(self,desc,FN);
  end;
  try // commit changes
    com := ValAt(com,1); // '012345'
    SetLength(UpdateNumbers,length(com));
    for i := 0 to length(com)-1 do
      if com[i+1] in ['0'..'9'] then
        UpdateNumbers[i] := ord(com[i+1])-48 else
        UpdateNumbers[i] := -1;
    if Back.DoCommit(UpdateNumbers,desc,'',0,Log)>=0 then
      result := true; // mark something added
  finally
    Back.Free;
  end;
  // 2. Update a copy of real .dvs (with SCR-related commits)
  FN := path+origFN;
  Age := FileAge(FileName);
  ForceFullCopy := true;
  if Age<>FileAge(FN) then // update if needed
  if FileExists(FN) then begin // existing -> add new commits (faster on network)
    Back := TVersions.Create(FN); // open existing
    try
      commitText := Commits.Text;
      ToBeUpdated := Back.LastCommit;
      if (ToBeUpdated<>LastCommit) or
         (Back.Commits.Text<>commitText) then begin  // update only if changed
        for i := 0 to Count-1 do begin
          V := @Values[i];
          if V^.Commit>ToBeUpdated then // add updated commits
            with Back.Values[Back.CreateNewVersion(V^.FileName,V^.Commit)] do begin
              Age := V^.Age;
              Data := GetVersionDiffOrPlain(i);
            end;
        end;
        Back.Commits.Text := commitText;
        if Count=Back.Count then begin // count mismatch -> update failed
          Back.WriteIfModified(Age); // force FileSetDate(FN,Age)
          ForceFullCopy := false;
        end;
      end;
    finally
      Back.Free;
    end;
  end;
  if ForceFullCopy then
    CopyFile(FileName,FN); // new or update failed -> full file content copy
  finally
    fLog := nil;
  end;
end;

function TVersions.FillStrings(SourceDir: string;
  const Filters1, Filters2: TStringDynArray;
  var Deleted: TIntegerDynArray;
  var Modified: TModifiedDynArray): boolean; // true if some file was modified
// Deleted: deleted files, Strings[]=FileName
// Modified: new or updated files Strings[]=FileName, Objects[]=FileAge
var Files: TStringList; // files on disk, Strings[]=FileName, Objects[]=FileAge
procedure AddFiles(RelativePath: string);
var SR: TSearchRec;
procedure AddFilesFilter(const Filter: TStringDynArray);
var SR: TSearchRec;
    f: integer;
begin
  for f := 0 to high(Filter) do
  if FindFirst(DefaultPath+SourceDir+RelativePath+Filter[f],
    faAnyFile-faDirectory{$ifndef DELPHI6OROLDER}-faSymLink{$endif}-faHidden,SR)=0 then begin
    repeat
      if SR.Size<>0 then
        if (IgnoreFile='') or
           (pos(IgnoreFile,SR.Name)=0) then // always ignore backup.~pas file
          Files.AddObject(SourceDir+RelativePath+SR.Name,pointer(SR.Time));
    until FindNext(SR)<>0;
    FindClose(SR);
  end;
end;
begin
  // 1. recursively get directories
  if FindFirst(DefaultPath+SourceDir+RelativePath+'*.*',faDirectory,SR)=0 then begin
    repeat
      if (SR.Attr and faDirectory<>0) and (SR.Name<>'') then
        if not CSVContains(IgnorePath,SR.Name) and // skip CVS,BACKUP.. directories
           (SR.Name[1]<>'.') then // skip relatives '.'&'..' directories
          AddFiles(RelativePath+SR.Name+'\');
    until FindNext(SR)<>0;
    FindClose(SR);
  end;
  // 2. get Files from Filters1[] and Filters2[] masks
  AddFilesFilter(Filters1);
  AddFilesFilter(Filters2);
end;
function SameFileDateWindows(FileDate1,FileDate2: integer): boolean;
// we allow an exact one Hour round (NTFS bug on summer time zone change)
begin
  dec(FileDate1,FileDate2);
  result := (FileDate1=0) or (FileDate1=1 shl 11) or (FileDate1=-(1 shl 11));
end;
var f, i, j: integer;
    wasdeleted: boolean;
    ModifiedCount, DeletedCount: integer;
    Name: string;
begin
  result := false;
  // 0. put files on disk in SourceDir in Files[]
  Files   := TStringList.Create;
  SourceDir := IncludeTrailingPathDelimiter(SourceDir);
  AddFiles(''); // recursively get directories, starting from SourceDir+''
  Files.CaseSensitive := false;
  Files.Sorted := true; // faster file find
  // 1. check for deleted files, and add them in Deleted[]
  SourceDir := ProjectCommons.UpperCase(SourceDir); // for IdemPChar()
  DeletedCount := length(Deleted);
  for i := 0 to FileNames.Count-1 do begin // was any file deleted on disk ?
    Name := FileNames.Value[i];
    if IdemPChar(pointer(Name),pointer(SourceDir)) and (Files.IndexOf(Name)<0) then begin
      wasdeleted := false; // search if not already marked as deleted
      for j := Count-1 downto 0 do // search last version of this FileName
        if Values[j].FileName=i then begin
          if Values[j].Age=1 then // marked deleted?
            wasdeleted := true;
          break; // if last version was Age<>1 -> not deleted
        end;
      if wasdeleted then continue;
      AddInteger(Deleted,DeletedCount,i,true);
      result := true;
    end;
  end;
  SetLength(Deleted,DeletedCount);
  // 2. add new or updated files in Modified[] (object=Age)
  ModifiedCount := length(Modified);
  for f := 0 to Files.Count-1 do begin
    j := FileNames.FindIndex(Files[f]);
    i := GetLastVersionIndex(j); // search last version
    if (i<0) or
      not SameFileDateWindows(Values[i].Age,integer(Files.Objects[f])) then begin
      if j<0 then
        j := FileNames.Add(Files[f]);
      if ModifiedCount=length(Modified) then
        SetLength(Modified,ModifiedCount+100);
      with Modified[ModifiedCount] do begin
        FileName := j;
        Age := integer(Files.Objects[f]);
      end;
      inc(ModifiedCount);
      result := true;
    end;
  end;
  SetLength(Modified,ModifiedCount);
  Files.Free;
end;

function TVersions.DoUpdate(const UpdateNumbers: array of integer;
  var Deleted: TIntegerDynArray; var Modified: TModifiedDynArray): boolean;
procedure DoUpdate(P: PChar);
// 'Synopse Library;Common;FilterDefault,BalloonHint.dcu,BHWndProcHook.dcu,*.pro,*.sad'
var s, SourceDir: string;
    isFilterDefault: boolean;
    Filter2: TAnsiStrings;
begin
  if P=nil then exit;
  GetNextItem(P,';'); // ignore description: 'Synopse Library'
  SourceDir := GetNextItem(P,';'); // 'Common'
  if (P=nil) or not DirectoryExists(DefaultPath+SourceDir) then
    exit;
  isFilterDefault := false;
  Filter2.Init;
  while P<>nil do begin
    s := GetNextItem(P);
    if s='' then continue;
    if SameText(s,'FILTERDEFAULT') then
      isFilterDefault := true else
      Filter2.Add(s,true);
  end;
  Filter2.SetLength;
  if (Filter2.Count=0) and not isFilterDefault then
    exit;
  if (isFilterDefault and FillStrings(SourceDir,FilterDefault,Filter2.Value,Deleted,Modified)) or
     (not isFilterDefault and FillStrings(SourceDir,Filter2.Value,nil,Deleted,Modified)) then
    result := true;
end;
var i: integer;
begin
  result := false;
  for i := 0 to high(UpdateNumbers) do
    if UpdateNumbers[i]>=0 then
      DoUpdate(pointer(Params['Update'+IntToStr(UpdateNumbers[i])]));
end;

function TVersions.GetVersionData(aFileName: integer; aVersion: integer): string;
var first, i: integer;
    old, err: string;
begin
  result := '';
  if aFileName<0 then
    exit;
  for first := 0 to Count-1 do
  if Values[first].FileName=aFileName then begin // search initial, plain data
    result := GetVersionDiffOrPlain(first); // 0 = initial (plain)
    for i := first+1 to Count-1 do // get the plain data from all diffs
    with Values[i] do
    if (Age<>1) and (FileName=aFileName) then
      if Commit>aVersion then // commits are stored in increasing order
        exit else begin // exit if we reached aVersion -> result contains data
        old := result; // update value from diff stored in 1..9999
        err := DiffStreamExtract(GetVersionDiffOrPlain(i),old,result);
        if err<>'' then begin // diff error (should never occur):
          err := format('Error getting diff %s.%x: %s'#13+
            'Previous version used instead',
            [FileNames.Value[aFileName],Commit,err]);
          if Assigned(fLog) then
            fLog(err) else
            MessageBox(0,pointer(err),nil,MB_OK);
          result := old;
        end;
{          raise Exception.CreateFmt('Error getting diff %d for %s: %s',
            [Commit,FileNames.Value[aFileName],err]); }
      end;
    exit; // [first] if done once
  end;
end;

function TVersions.GetVersionDiffOrPlain(Index: integer): string;
begin
  result := '';
  if (Index>=0) and (Index<Count) then
  with Values[Index] do
    if Age<>1 then // Age=1 if deleted -> ignore 'deleted' bulk text
      if Data<>'' then
        result := Data else
        if ReaderIndex>=0 then
          result := Reader.GetString(ReaderIndex);
end;

function TVersions.GetLastVersionIndex(aFileName: integer): integer;
begin
  if aFileName>=0 then
  for result := Count-1 downto 0 do
    if Values[result].FileName=aFileName then
      exit;
  result := -1;
end;

function TVersions.GetVersions(const aVersion: integer; var Vers: TIntegerDynArray;
   withoutDel: boolean=true): integer;
// fill Vers[] with Values[] index of every files
var i: integer;
begin
  SetLength(Vers,FileNames.Count);
  fillchar(Vers[0],FileNames.Count*4,-1); // set all Vers[] to -1
  result := 0; // file count
  for i := 0 to Count-1 do
  with Values[i] do
  if Commit<=aVersion then
  if withoutDel and (Age=1) then begin // deleted -> delete in Vers[]
    if Vers[FileName]>=0 then begin
      Vers[FileName] := -1;
      dec(result); // decrease file count
    end;
  end else begin // updated -> update in Vers[]
    if Vers[FileName]<0 then
      inc(result); // add one to file count
    Vers[FileName] := i;
  end;
end;

function TVersions.GetVersionsModified(const aVersion: integer;
  var Vers: TIntegerDynArray): integer;
var i: integer;
begin
  SetLength(Vers,FileNames.Count);
  fillchar(Vers[0],FileNames.Count*4,-1); // set all Vers[] to -1
  result := 0; // file count
  for i := 0 to Count-1 do
  with Values[i] do
    if Commit=aVersion then begin
      Vers[FileName] := i;
      inc(result); // add one to file count
    end;
end;

procedure TVersions.VersToFile(var Vers, Files: TIntegerDynArray; n: integer);
var i: integer;
begin
  Setlength(Files,n);
  n := 0;
  for i := 0 to FileNames.Count-1 do
  if Vers[i]>=0 then begin
    Files[n] := Vers[i];
    inc(n);
  end;
  assert(n=length(Files));
end;

procedure TVersions.GetVersionValues(const aVersion: integer;
  var Files: TIntegerDynArray; onlyModified: boolean);
// return all Values[] index of the files in a particular version
var n: integer;
    Vers: TIntegerDynArray;
begin
  // 1. fill Vers[] with Values[] index of every files
  if onlyModified then
    n := GetVersionsModified(aVersion,Vers) else
    n := GetVersions(aVersion,Vers);
  // 2. copy Vers[] to Files[]
  VersToFile(Vers, Files, n);
end;

procedure TVersions.GetVersionValuesDiff(var aVersion1, aVersion2: integer;
  var Files: TIntegerDynArray);
var Vers1, Vers2: TIntegerDynArray;
    n, n1, n2, i: integer;
begin
  if aVersion1>aVersion2 then begin // always from old (1) to new (2)
    i := aVersion1;
    aVersion1 := aVersion2;
    aVersion2 := i;
  end;
  n1 := GetVersions(aVersion1,Vers1,false); // false = include Deleted files
  n2 := GetVersions(aVersion2,Vers2,false);
  SetLength(Files,n1+n2);
  n := 0;
  for i := 0 to FileNames.Count-1 do
  if Vers1[i]<>Vers2[i] then begin
    Files[n] := Vers2[i]; // store new (2) index
    inc(n);
  end;
  SetLength(Files,n);
end;

function TVersions.ZipName(Index: integer): string;
// 'EIA\Main.pas.00000' e.g. with Commit=0
begin
  with Values[index] do
    if FileName<FileNames.Count then
      result := format('%s.%x',[FileNames.Value[FileName],Commit]) else
      result := '';
end;

function TVersions.LastCommit: integer;
begin
  if not TryStrToInt('x'+Params['LastCommit'],result) then
    LastCommit := -1;
end;


function TVersions.CommitToString(Commit: integer): string;
begin
  if Commit=-1 then
    result := 'previous' else begin
    result := format('#%d %s',[Commit,CommitDescription(Commit)]);
  end;
end;

function TVersions.PreviousCommit(aFileName, aCommit: integer): integer;
var i: integer;
begin
  result := -1;
  for i := Count-1 downto 0 do
  with Values[i] do
  if FileName=aFileName then begin
    result := Commit;
    if Commit<aCommit then
      exit;
  end;
end;

function TVersions.PreviousVersion(Vers: PVersion): PVersion;
var i: integer;
begin
  result := nil;
  if Vers<>nil then
  for i := Count-1 downto 0 do
  with Values[i] do
  if FileName=Vers^.FileName then begin
    result := @Values[i];
    if Commit<Vers^.Commit then
      exit;
  end;
end;

function TVersions.GetVersion(aFileName, aVersion: integer): PVersion;
// get aVersion or the first occurence before
var i: integer;
begin
  result := nil;
  for i := 0 to Count-1 do // a bit slower, but same result as GetVersionData()
  with Values[i] do
  if FileName=aFileName then
    if Commit>aVersion then // reach a smaller? -> ok
      exit else
      result := @Values[i];
end;

function TVersions.CommitDescription(Commit: integer): string;
var Sec: TSection;
begin
  Sec := Commits[IntToHex(Commit,0)];
  if Sec<>nil then
    result := Sec['Description'] else
    result := '';
end;

procedure TVersions.SCRForceCreate;
begin
  if (SCRFileName='') or (fSCR<>nil) or FileExists(SCRFileName) then
    exit;
  fSCR := TSectionsStorage.Create(SCRFileName);
  // we need a [Pictures] section here, otherwize a file with size=0 is written
  fSCR.LoadFromMemory('[Pictures]'#13'; pictures will be put here'#13#13+
   '; put tracker sections below, [11] for SCR #11, e.g.'#13+
   '; with following values (use "Add a new Tracker entry" button in toolbar):'#13+
   ';  Description=EIA Highlight samples in list is different'#13+
   ';  ShortName='#13';  Request=SCR #55'#13+
   ';  Risk=Risk=1,1,3,Franck Grandemange+Arnaud Bouchez,UI only change, affects report'#13#13);
  fSCR.SaveToFile;
end;

procedure TVersions.MANForceCreate;
var Company, CompanyUp, Product, Author: string;
begin
  if (MANFileName='') or FileExists(MANFileName) then
    exit;
  Company := Params['COMPANY'];
  CompanyUp := ProjectCommons.UpperCase(Company);
  Product := Params['Product'];
  Author := Params['DefaultAuthor'];
  StringToFile(MANFileName,
   '[People]'#13+Author+'=Documentation Manager'#13#13+
   '[Pictures]'#13'; pictures will be put here'#13#13+
   '[Project]'#13'Name='+Product+#13'Company='+Company+
   #13'ReleaseVersion='#13'ReleaseDate='#13'Manager='+Author+
   #13'DestinationDir='+GetMyDocuments+
   #13'; path to store all created .doc (not to be inside versioning tree)'+
   #13'OldWordOpen=No'#13'; OldWordOpen=Yes for some Word 2000 installations'+
   #13'DefLang=1033'#13'Confidential='+ValAt(Company,0,' ')+
   #13#13'{\b WARNING:} THE ATTACHED DOCUMENTS DESCRIBE CONFIDENTIAL INFORMATION OF '+
   CompanyUp+'.'#13'ACCESS SHOULD BE RESTRICTED TO ONLY THOSE INDIVIDUALS WHO ARE '+
   'FULL-TIME EMPLOYEES OF '+CompanyUp+' OR COMPANIES WHICH HAVE SIGNED A CONFIDENTIALITY '+
   'AGREEMENT WITH '+CompanyUp+' AND HAVE A LEGITIMATE REASON TO KNOW THE CONTENTS OF '+
   'THESE DOCUMENTS.'#13#13+
   '[Manual]'#13'Owner=Manual'#13'Name='+Product+' Manual'#13'PreparedBy='+Author+
   #13'Revision=0.1'#13'RevisionDate='#13'RevisionDescription=Initial Version'+
   #13'DocumentFrontPage=ProjectDetails,PeopleDetails,RevisionDetails,NoHeader'#13+
   'DocumentIndex=Pictures'#13'WriteTableOfContent=Yes'#13#13+
   ':Introduction'#13': Document content'#13'This document is the '+
   Product+' Manual, corresponding to the '+Params['ReleaseName']+' release.'#13': '+
   Product+' Overview'#13'The '+Product+' was designed in order to...'#13+
   ':'+Product+#13'...');
end;

function TVersions.GetSCR: TSectionsStorage;
begin
  result := fSCR;
  if (result=nil) and (SCRFileName<>'') and FileExists(SCRFileName) then begin
    fSCR := TSectionsStorage.Create(SCRFileName);
    result := fSCR;
  end;
end;

function TVersions.ExtractDefaultPath(const aFileName: string): string;
begin
  if (DefaultPath<>'') and
     SameText(copy(aFileName,1,length(DefaultPath)),DefaultPath) then
    result := copy(aFileName,length(DefaultPath)+1,maxInt) else
    result := aFileName;
end;

function TVersions.GetBackupFileName(const com: string; out path: string): string;
begin
  result := trim(ValAt(com,2));
  if result='' then exit;
  path := IncludeTrailingPathDelimiter(result);
  result := path+ChangeFileExt(ExtractFileName(FileName),
    ' - backup'+ExtractFileExt(FileName));
end;

end.

