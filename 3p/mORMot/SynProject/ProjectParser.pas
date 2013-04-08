/// Software Architecture documentation generation from Delphi source code
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectParser;

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

interface

{$define WITH_GRAPHVIZ}
// if defined, the WinGraphviz COM server will be used to generated diagrams
// (must be defined in ProjectParser, ProjectEditor and ProjectTypes)
// - the WinGraphviz.dll is embedded into the main executable, and will be
// uncompressed and installed on the computer if necessary


uses
  Windows,
  ProjectCommons,
  Classes,
  SysUtils,
  SynZipFiles,
{$ifdef WITH_GRAPHVIZ}
  Graphics,
  Variants,
{$endif}
  ProjectTypes,
  ProjectSections,
  ProjectRTF,
  PasDoc_Types,
  PasDoc_Items,
  PasDoc_SortSettings,
  PasDoc_HierarchyTree,
  PasDoc_Light;

type
  /// Delphi project parser (PasDoc based), for creating source documentation
  TProjectBrowser = class(TPasDocLight)
  private
    FProject: TProject;
    CurrentSection,
    SAD: TSection;
    LogFile: Text;
    SADUpperDefaultPath,
    LogBuf: string; // 8kb memory buffer for log, autoreleased in Destroy
    FClassHierarchy: TStringCardinalTree;
  private
{$ifdef WITH_GRAPHVIZ}
    function GraphFileNameOk(const name: string; addLastMinus: boolean): string;
{$endif}
    procedure LogMessage(const MessageType: TMessageType;
      const AMessage: string; const AVerbosity: Cardinal);
    function FindGlobal(const NameParts: TNameParts): TBaseItem;
  public
    // list of already written units filenames
    UnitsDescription: TStringList;
{$ifdef WITH_GRAPHVIZ}
    // path to GraphViz generated *.emf files
    GraphValues: TSection;
    GraphValuesModified: boolean;
    // '.emf' or '.png'
    GraphExt: string;
    procedure LoadGraphValues;
{$endif}
    //
    constructor Create(aProject: TProject); reintroduce;
    destructor Destroy; override;

    // fill ClassHierarchy with all classes of all units
    procedure CreateClassHierarchy;
    // fill ClassHierarchy with all classes of this unit
    procedure CreateClassHierarchyFor(PU: TPasUnit; NoClear: boolean = false);
    // [SAD-LIS].SourceFile -> Units[]
    // - if WithViz is true, all diagrams are recreated
    procedure FillUnits(Section: TSection; WithViz: boolean);
    // write description of this Item, RTF-formated
    function RtfDescription(Item: TPasItem; W: TProjectWriter): boolean;
    // write used unit table
    procedure RtfUsesUnits(WR: TProjectWriter);
{$ifdef WITH_GRAPHVIZ}
    // create graphics with diagrams
    function GraphVizEmf(aSection: TSection): boolean;
    // add the the corresponding graph to the output RTF buffer, from its filename 
    procedure AddGraph(graph: string; WR: TProjectWriter);
{$endif}
    // write the description of this unit
    procedure RtfUnitDescription(aUnit: TPasUnit; WR: TProjectWriter; TitleLevel: integer; withFields: boolean);
    // write all used unit detailled description if not already done
    procedure RtfUsesUnitsDescription(WR: TProjectWriter; TitleLevel: integer;
      const CSVUnitsWithFields: string);
    // the associated project
    property Project: TProject read FProject;
    // the current class hierarchy
    property ClassHierarchy: TStringCardinalTree read FClassHierarchy;
  end;

{$ifdef WITH_GRAPHVIZ}
/// get Wingraphviz.dot COM object into a Variant
// - return false on error, true on success
// - display an error message if failed
function GetWingraphviz(out Dot: Variant): boolean;

/// create a picture from a .dot content
// - the picture will be created in DestDir+FileName+GraphExt
// - will set GraphValues[GraphPath+FileName+GraphExt]='230x123 90% GraphTitle'
function WingraphvizCreateFile(Dot: Variant; const FileName, DestDir, GraphExt, Source: string;
  const GraphPath: string=''; const GraphTitle: string=''; GraphValues: TSection=nil;
  Perc: integer=0): boolean;
{$endif}

/// convert a \graph content (from lines) into a .dot compatible
function WingraphvizFromText(const UniqueImageName: string; Buffer: TStrings;
  BufferStartIndex: integer=0): string;

{/// retrieve common data directory
// - Vista: C:\ProgramData
// - XP:    C:\Documents and Settings\All Users\Application Data
function GetCommonAppDataPath: AnsiString;}

/// retrieve common data directory for Synopse applications
// - Vista: C:\ProgramData\Synopse
// - XP:    C:\Documents and Settings\All Users\Application Data\Synopse
function GetSynopseCommonAppDataPath: AnsiString;

/// convert a supplied SVG textual content into an .emf file
// - will only handle SVG content as created by GraphViz
// - if Dest='', will return the created TMetaFile instance (caller must free it)
function SVGToEMF(const Source: string; const Dest: TFileName;
  WhiteBackground: boolean=false): TMetaFile;



implementation

uses
  ShlObj, ComObj, ActiveX, SynZip,
  ProjectDiff; // for Alder32Asm

resourcestring
  sUsedFor = 'Used for';
  sImplementedInN = '%s implemented in the {\i %s} unit';
  sPurpose = 'Purpose';
  sQuotedInN = 'The {\i %s} unit is quoted in the following items';
  sUnitsUsedInN = 'Units used in the {\i %s} unit';
  sUnitName = 'Unit Name';
  sUnitsLocatedInN = 'Units located in the "%s" directory';
  sSourceFileName = 'Source File Name';

{ TProjectBrowser }

{$ifdef WITH_GRAPHVIZ}
procedure TProjectBrowser.AddGraph(graph: string; WR: TProjectWriter);
var caption: string;
begin
  LoadGraphValues;
  graph := GraphDirName+graph;
  if FileExists(Project.FileNameDir+graph+GraphExt) then
    graph := graph+GraphExt else
  if FileExists(Project.FileNameDir+graph+'.emf') then
    graph := graph+'.emf' else
  if FileExists(Project.FileNameDir+graph+'.png') then
    graph := graph+'.png' else
    graph := graph+'.gif';
  if FileExists(Project.FileNameDir+graph) then begin
    WR.RtfFont(50);
    WR.RtfImage(
        Project.PictureFullLine(graph,caption),caption).RtfFont(100);
  end;
end;
{$endif}

{$I-}
constructor TProjectBrowser.Create(aProject: TProject);
begin
  inherited Create(nil);
  OnMessage := LogMessage;
  FProject := aProject;
  // init default parsing values from Project+SAD
  CacheName := Project.FileNameDir+Project.Project['Name']+'.sad'; // zip file name for caching
  CacheNameSAE := ChangeFileExt(CacheName,'.sae');
  AssignFile(LogFile,Project.DestinationDir+'SAD.log'); // 'D:\Documents\Product New Version\'
  SetLength(LogBuf,8192); // autoreleased in TProjectBrowser.Destroy
  SetTextBuf(LogFile,LogBuf[1],length(LogBuf)); // 8kb memory buffer for log
  Rewrite(LogFile); // new file each time
  WriteLn(LogFile,DateTimeToStr(Now));
  DoMessage(0,mtInformation,'Cache file = %s',[CacheName]);
  ioresult;
  SAD := Project.ParseSAD.Params;
  if SAD=nil then exit;
  if SAD['DefaultPath']<>'' then // D:\DEV\
    SADUpperDefaultPath := SysUtils.UpperCase(
      IncludeTrailingPathDelimiter(SAD['DefaultPath']));
  DoMessage(0,mtInformation,'[%s].Source = %s',[SAD.SectionName,SAD['Source']]);
  Directives := 'WIN32;VER150;MSWINDOWS'; // Borland's format -> default is Delphi 7
  CommentMarkers := '/{*'; // parsed comments are: /// {{ (** e.g.
  MarkerOptional := false; // if true, CommentMarkers are ignored
  ShowVisibilities := [viPublic, viPublished, viAutomated];
  ImplicitVisibility := ivPublic;
  OutdatedCacheAutoRecreate := false;
  ForceCacheRecreateAll := false;
  HandleMacros := false;
  UnitUsesAlsoInImplementation := true;
  UnitsDescription := TStringList.Create;
  UnitsDescription.Sorted := true;
  UnitsDescription.Duplicates := dupIgnore;
{$ifdef WITH_GRAPHVIZ}
  if not DirectoryExists(Project.FileNameDir+GraphDirName) then
    mkDir(Project.FileNameDir+GraphDirName);
  GraphExt := '.emf'; // by default
{$endif}
//  Verbosity := 5; // used for debug only
end;
{$I+}

procedure TProjectBrowser.CreateClassHierarchy;
var unitLoop: Integer;
begin
  FClassHierarchy.Free;
  FClassHierarchy := TStringCardinalTree.Create;
  for unitLoop := 0 to Units.Count - 1 do
    CreateClassHierarchyFor(TPasUnit(Units[unitLoop]),true);
  FClassHierarchy.Sort;
end;

procedure TProjectBrowser.CreateClassHierarchyFor(PU: TPasUnit; NoClear: boolean = false);
  function FindGlobalPasItem(const NameParts: TNameParts): TPasItem;
  var BaseResult: TBaseItem;
  begin
    BaseResult := FindGlobal(NameParts);
    if (BaseResult <> nil) and (BaseResult is TPasItem) then
      Result := TPasItem(BaseResult) else
      Result := nil;
  end;
var
  classLoop: Integer;
  ACIO: TPasCio;
  ParentItem: TPasItem;
  Parent, Child: TPasItemNode;
begin
  if not NoClear then begin
    FClassHierarchy.Free;
    FClassHierarchy := TStringCardinalTree.Create;
  end;
  if PU.CIOs = nil then exit;
  for classLoop := 0 to PU.CIOs.Count - 1 do begin
    ACIO := TPasCio(PU.CIOs.PasItemAt[classLoop]);
    if ACIO.MyType in CIONonHierarchy then continue;
    if Assigned(ACIO.Ancestors) and (ACIO.Ancestors.Count > 0) then begin
      ParentItem := FindGlobalPasItem(OneNamePart(ACIO.Ancestors.FirstName));
      if Assigned(ParentItem) then begin
        Parent := FClassHierarchy.ItemOfName(ParentItem.Name);
        // Add parent if not already there
        if Parent = nil then
            Parent := FClassHierarchy.InsertItem(ParentItem);
      end else begin
        Parent := FClassHierarchy.ItemOfName(ACIO.Ancestors.FirstName);
        if Parent = nil then
            Parent := FClassHierarchy.InsertName(ACIO.Ancestors.FirstName);
      end;
    end else
      Parent := nil;
    Child := FClassHierarchy.ItemOfName(ACIO.Name);
    if Child = nil then
      FClassHierarchy.InsertItemParented(Parent, ACIO)
    else
      if Parent <> nil then
        FClassHierarchy.MoveChildLast(Child, Parent);
  end;
  if not NoClear then
    FClassHierarchy.Sort;
end;

{$I-}
destructor TProjectBrowser.Destroy;
begin
  WriteLn(LogFile);
  DoMessage(2,mtInformation,'Files not found during process:'#13#10'%s',[FilesNotFoundList]);
  WriteLn(LogFile);
  Close(LogFile);
  ioresult;
  UnitsDescription.Free;
{$ifdef WITH_GRAPHVIZ}
  if GraphValues<>nil then begin
    if GraphValuesModified then
      GraphValues.SaveToFile(Project.FileNameDir+GraphDirName+GraphValues.SectionName+'.ini');
    GraphValues.Free;
  end;
{$endif}
  inherited;
end;
{$I+}

procedure TProjectBrowser.FillUnits(Section: TSection; WithViz: boolean);
procedure ParseFiles;
var i, j: integer;
    U: TPasUnit;
begin
  // add all the depending units to SourceFileNames[]
  SourceFileNames.Clear;
  for i := 0 to Units.Count-1 do begin
    U := TPasUnit(Units[i]); // add all units in uses clause
    for j := 0 to U.UsesUnits.Count-1 do
      if Units.FindName(U.UsesUnits[j])=nil then // add once
        AddOneSourceUnit(U.UsesUnits[j]);
  end;
  // parse the SourceFileNames[] files -> Units[]
  for i := 0 to SourceFileNames.Count-1 do
    ParseFile(SourceFileNames[i]);
end;
var D: TSectionsStorage;
procedure Update(Items: TPasItems; const SectionName: string);
var Sec: TSection;
    i: Integer;
    s: string;
begin
  Sec := D.Section[SectionName];
  if Sec=nil then
    Exit;
  for i := 0 to Items.Count-1 do
  with TPasItem(Items[i]) do begin
    s := trim(Sec[Name]);
    if s<>'' then // leave any existing description, if no newer available
      RawDescription := s; // override existing item description
  end;
end;
var i, j, k: integer;
    Dir, data, txt: string;
begin // Section = [SAD-EIA] e.g.
  Units.Clear;
  if Section=nil then
    exit;
  CurrentSection := Section;
  CloseCacheSAE(False,False); // FreeAndNil(FCacheReaderSAE)
  // default subpath for ZipName Entry -> 'EIA\'
  CacheZipPath := IncludeTrailingPathDelimiter(Section.SectionNameValue);
  if Section['DefaultPath']<>'' then // D:\DEV\
    UpperDefaultPath := SysUtils.UpperCase(
      IncludeTrailingPathDelimiter(Section['DefaultPath'])) else
    UpperDefaultPath := SADUpperDefaultPath;
  IncludeDirectories := Section['IncludePath']; // D:\DEV\LIB;D:\DEV\TMS
  SourceDefaultPath := Section['SourcePath']; // D:\Dev\Synopse\EIA
  if SourceDefaultPath<>'' then begin
    if not DirectoryExists(SourceDefaultPath) then
      SourceDefaultPath := UpperDefaultPath+Section['SourcePath']; // D:\Dev\Synopse\EIA
    SourceDefaultPath := IncludeTrailingPathDelimiter(SourceDefaultPath);
  end;
  SourceFileNames.Clear;
  AddSourceFileName(Section['SourceFile']); // fill SourceFileNames[]
  if SourceFileNames.Count=0 then
    exit; // nothing to parse
{$I-}
  WriteLn(LogFile,#13#10' FillUnits(',Section.SectionName,') -> ',
    Section['SourceFile']);
{$I+}
  DoMessage(0,mtInformation,'DefaultPath = %s',[UpperDefaultPath]);
  DoMessage(0,mtInformation,'IncludePath = %s',[Section['IncludePath']]);
  Dir := Section['Directives'];
  if Dir<>'' then begin
    if pos('VER70',Dir)=0 then // allow Turbo Pascal only parsing
      Dir := 'WIN32;VER150;'+Dir;
    Directives := Dir;
    DoMessage(0,mtInformation,'Directives = %s',[Dir]);
  end else
    Directives := 'WIN32;VER150';
  if ForceCacheRecreateAll then
    DoMessage(0,mtInformation,'ForceCacheRecreateAll is ON',[]) else
    if OutdatedCacheAutoRecreate then
      DoMessage(0,mtInformation,'OutdatedCacheAutoRecreate is ON',[]);
  if isTrue(Section['ExternalDescription']) then begin
    if FileExists(CacheNameSAE) then begin
      FCacheReaderSAE := TZipReader.Create(CacheNameSAE);
      DoMessage(0,mtInformation,'ExternalDescription=Yes from %s',[CacheNameSAE]);
    end else
      DoMessage(0,mtError,'ExternalDescription=Yes but no %s file yet',[CacheNameSAE]);
  end;
  try
    // Parse the main files -> Units[]
    for i := 0 to SourceFileNames.Count-1 do
      ParseFile(SourceFileNames[i]);
    // Units[].Use->Units[]
    ParseFiles;
    // Units[].Use->Units[] twice
    ParseFiles;
    // init OutputFileName = SourceFileName for display
    if UpperDefaultPath<>'' then
    for i := 0 to Units.Count-1 do
    with TPasUnit(Units[i]) do begin
      if IdemPChar(pointer(SourceFileName),pointer(UpperDefaultPath)) then
        OutputFileName := copy(SourceFileName,length(UpperDefaultPath)+1,maxInt) else
        OutputFileName := SourceFileName;
      if FCacheReaderSAE<>nil then begin // override by hand-made description
        j := FCacheReaderSAE.ZipNameIndexOf(OutputFileName);
        if j<0 then
          j := FCacheReaderSAE.ZipNameIndexOf(SourceFileName);
        if j>=0 then begin
          data := FCacheReaderSAE.GetString(j);
          if data<>'' then begin
            D := TSectionsStorage.Create;
            try
              D.LoadFromMemory(Pointer(data));
              txt := D.ReadHeader('Description','');
              if txt<>'' then // leave any existing description, if no newer 
                RawDescription := txt;
              Update(Types,'Types');
              Update(Constants,'Constants');
              Update(FuncsProcs,'Functions');
              Update(Variables,'Variables');
              Update(CIOs,'Objects');
              for k := 0 to CIOs.Count-1 do
                if CIOs[k].InheritsFrom(TPasCIO) then
                with TPasCio(CIOs[k]) do begin
                  Update(Fields,Name);
                  Update(Methods,Name);
                  Update(Properties,Name);
                end;
            finally
              D.Free;
            end;
          end;
        end;
      end;
    end;
    // update cache file
    ZipCacheUpdate;
    Units.SortByDirectory;
    Units.SortOnlyInsideItems([
      ssConstants, ssFuncsProcs, ssTypes, ssVariables, ssUsesClauses,
      ssRecordFields, ssNonRecordFields, ssMethods, ssProperties]);
{$ifdef WITH_GRAPHVIZ}
    if WithViz then
      GraphVizEmf(Section);
{$endif}
  except
    on E: Exception do
      DoMessage(0,mtError,E.Message,[]);
  end;
end;

function TProjectBrowser.FindGlobal(const NameParts: TNameParts): TBaseItem;
var i: Integer;
    Item: TBaseItem;
    U: TPasUnit;
begin
  Result := nil;
  if (Units=nil) or (Units.Count=0) then Exit;
  case Length(NameParts) of
    1: { field_method_property }
        for i := 0 to Units.Count - 1 do begin
           U := TPasUnit(Units[i]);
           if SameText(U.Name, NameParts[0]) then begin
             Result := U;
             Exit;
           end;
           Result := U.FindItem(NameParts[0]);
           if Result <> nil then Exit;
         end;
    2: begin  { object.field_method_property }
         for i := 0 to Units.Count - 1 do begin
           Result := TPasUnit(Units[i]).FindFieldMethodProperty(NameParts[0], NameParts[1]);
           if Assigned(Result) then Exit;
         end;
         { unit.cio_var_const_type }
         U := TPasUnit(Units.FindName(NameParts[0]));
         if Assigned(U) then
           Result := U.FindItem(NameParts[1]);
       end;
    3: begin  { unit.objectorclassorinterface.fieldormethodorproperty }
         U := TPasUnit(Units.FindName(NameParts[0]));
         if (not Assigned(U)) then Exit;
         Item := U.FindItem(NameParts[1]);
         if (not Assigned(Item)) then Exit;
         Item := Item.FindItem(NameParts[2]);
         if (not Assigned(Item)) then Exit;
         Result := Item;
         Exit;
       end;
  end;
end;


function GetShellFolderPath(const FolderID: Integer): AnsiString;
var pidl: PItemIDList;
    Buffer: array[0..MAX_PATH-1] of AnsiChar;
    Malloc: IMalloc;
begin
  Result := '';
  if Win32MajorVersion<4 then Exit;
  if FAILED(SHGetMalloc(Malloc)) then
    Malloc := nil;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, pidl)) then begin
    if SHGetPathFromIDListA(pidl, Buffer) then begin
      Result := Buffer;
      if Result[length(Result)]<>'\' then
        Result := Result+'\';
    end;
    if Assigned(Malloc) then
      Malloc.Free(pidl);
  end;
end;

/// retrieve common data directory
// - Vista: C:\ProgramData
// - XP:    C:\Documents and Settings\All Users\Application Data
function GetCommonAppDataPath: AnsiString;
const CSIDL_COMMON_APPDATA = $0023;
begin
  result := GetShellFolderPath(CSIDL_COMMON_APPDATA); // Vista: c:\ProgramData
  if result='' then
    result := ExtractFilePath(paramstr(0));
end;

function GetSynopseCommonAppDataPath: AnsiString;
const CSIDL_LOCAL_APPDATA = $001C;
begin
{  result := GetCommonAppDataPath+'Synopse\';
  if not DirectoryExists(result) then begin
    CreateDirectory(pointer(result),nil);
    if not DirectoryExists(result) then begin // read-only problem (Vista UAC?) }
      result := GetShellFolderPath(CSIDL_LOCAL_APPDATA)+'Synopse\';
      if not DirectoryExists(result) then
        CreateDirectory(pointer(result),nil);
{    end;
  end; }
end;

{$ifdef WITH_GRAPHVIZ}

(*
function WinExecAndWait(const FileName: String; Visibility: integer=SW_SHOWNORMAL): cardinal;
var StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    Options: cardinal;
begin
  FillChar(StartupInfo,Sizeof(StartupInfo),0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if Visibility=0 then
    Options := NORMAL_PRIORITY_CLASS else
    Options := CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS;
  if not CreateProcess(nil,
     pointer(FileName),             { pointer to command line string }
     nil,                           { pointer to process security attributes }
     nil,                           { pointer to thread security attributes }
     false,                         { handle inheritance flag }
     Options,                       { creation flags }
     nil,                           { pointer to new environment block }
     nil,                           { pointer to current directory name }
     StartupInfo,                   { pointer to STARTUPINFO }
     ProcessInfo) then              { pointer to PROCESS_INF }
    Result := cardinal(-1) else begin
    WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess,Result);
  end;
end;
*)


function WinExecAndWait32(Path: PChar; Visibility: Word; Timeout : DWORD): integer;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := visibility;
  end;
  if CreateProcess(nil,path,nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil,
		StartupInfo, ProcessInfo) then
    { timeout is in miliseconds or INFINITE if you want to wait forever }
    result := Integer(WaitForSingleObject(ProcessInfo.hProcess, timeout)) else
    result := GetLastError;
end;

var
  GetWingraphvizDlg: boolean = False;

function GetWingraphviz(out Dot: Variant): boolean;
procedure Retry;
var TempDll, TempReg, TempPath: TFileName;
    Reg: function: HResult; stdcall;
    H: THandle;
begin
  // library not available -> register now
  TempPath := GetSynopseCommonAppDataPath;
  TempDll := TempPath+'WinGraphviz.dll';
  TempReg := TempPath+'WinGraphviz.reg';
  if not FileExists(TempDll) or not FileExists(TempReg) then
    with TZipRead.Create(HInstance,'Zip','ZIP') do
    try // get embedded WinGraphviz server
      StringToFile(TempDll,UnZip(NameToIndex('WinGraphviz.dll')));
      StringToFile(TempReg,StringReplace(UnZip(NameToIndex('WinGraphviz.reg')),
        'WinGraphviz.dll',StringReplace(TempDll,'\','\\',[rfReplaceAll]),
        [rfReplaceAll,rfIgnoreCase]));
    finally
      Free;
    end;
  result := false;
  // first try to register as normal COM library
  H := SafeLoadLibrary(TempDll);
  if H<>0 then
  try
    @Reg := GetProcAddress(H,'DllRegisterServer');
    if Assigned(@Reg) then
      result := (Reg=S_OK);
  finally
    FreeLibrary(H);
  end;
  // on failure, register COM library for the current user
  if not result then
    result := WinExecAndWait32(pointer(format('reg.exe import "%s"',[TempReg])),
      SW_SHOWMINIMIZED,INFINITE)=0;
  if result then
  try
    Dot := CreateOleObject('Wingraphviz.DOT');
  except
    on E: Exception do
      result := false; // WinGraphviz was not successfully registered
  end;
  if not result then
    if not GetWingraphvizDlg then begin
      MessageBox(0,pointer('Please run as Administrator:'#13#13+
        'either SynProject.exe and this button'#13#13+
        'either the following command line:'#13' regsvr32.exe "'+
        TempDll+'"'),nil,MB_ICONERROR);
      GetWingraphvizDlg := true;
    end;
end;
begin
  try
    Dot := CreateOleObject('Wingraphviz.DOT');
    result := true;
  except // library not available -> register now
    on E: Exception do
      Retry;
  end;
end;


function SVGToEMF(const Source: string; const Dest: TFileName;
  WhiteBackground: boolean=false): TMetaFile;

  function GetInt(var S: PChar): integer;
  begin
    result := 0;
    if S=nil then
      exit;
    while S^=' ' do inc(S);
    if (S=nil) or not (ord(S^) in [ord('0')..ord('9')]) then
      exit;
    repeat
      result := result*10+ord(S^)-48;
      inc(S);
    until not (ord(S^) in [ord('0')..ord('9')]);
    while S^ in [',',' '] do inc(S);
    if S^=#0 then
      S := nil;
  end;

  function GetTag(var S: PChar): PChar;
  begin  // '<?xml  >next' -> result='?xml' S='next'
    if S<>nil then begin
      while (S^<>'<') and (S^<>#0) do inc(S);
      if S^=#0 then
        S := nil;
    end;
    result := S;
    if result<>nil then begin
      inc(result);
      repeat
        inc(S);
      until (S^='>') or (S^=#0);
      if S^=#0 then
        S := nil else
        inc(S);
    end;
  end;

  function GetAttr(var S: PChar; Names: PChar; IntValues: array of Pinteger;
    out StrValue: string): integer;
  var L, i, err: integer;
      Beg: PChar;
  begin
    result := 0;
    if (Names=nil) or (S=nil) or (S^=#0) then
      exit;
    while S^=' ' do inc(S);
    if not (S^ in ['a'..'z']) then
      exit;
    i := 0;
    repeat
      inc(i);
      while Names^=' ' do inc(Names);
      L := 0;
      while not (Names[L] in ['=',#0]) do inc(L);
      if StrLComp(S,Names,L)=0 then begin
        result := i;
        break;
      end;
      inc(Names,L);
      if Names^='=' then
        inc(Names);
    until Names^=#0;
    if cardinal(result-1)>cardinal(high(IntValues)) then
      exit;
    inc(S,L);
    while S^ in [' ','='] do inc(S);
    if S^<>'"' then begin
      result := 0;
      exit;
    end;
    inc(S);
    Beg := S;
    repeat
      inc(S);
    until S^ in [#0,'"'];
    SetString(StrValue,Beg,S-Beg);
    if IntValues[result-1]<>nil then
      val(StrValue,IntValues[result-1]^,err);
    if S^<>#0 then
      inc(S);
  end;

  procedure FillStroke(Style: PChar; Canvas: TCanvas);
  function HtmlToTColor(Hexa: PChar): TColor;
  var err, color: integer;
      col: string;
  begin
    result := 0;
    if Hexa^='#' then begin
      SetString(col,Hexa,7);
      Hexa[0] := '$';
      val(Hexa,color,err); // hexa $RRGGBB
      with TRGBQuad(color) do
        result := RGB(rgbRed,rgbGreen,rgbBlue);
    end;
  end;
  begin
    repeat
      if StrLComp(Style,'fill:',5)=0 then begin
        inc(Style,5);
        if StrLComp(Style,'none',4)=0 then begin
          Canvas.Brush.Style := bsClear;
        end else begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := HtmlToTColor(Style);
        end;
      end else
      if StrLComp(Style,'stroke:',7)=0 then begin
        inc(Style,7);
        Canvas.Pen.Color := HtmlToTColor(Style);
      end;
      while not (Style^ in [';',' ',#0]) do inc(Style);
      while Style^ in [';',' '] do inc(Style);
    until Style^=#0;
  end;

var S: PChar;
    Cmd: char;
    style, path: string;
    Width, Height, X, Y, n: integer;
    DC: HDC;
    Canvas: TMetaFileCanvas;
    R: TRect;
    Tag, P: PChar;
    Points: array of TPoint;
    Str: string;

  function GetStylePoints: boolean;
  var n: integer;
      P: PChar;
      Str: string;
  begin
    n := 0;
    while true do
      case GetAttr(Tag,'style=points=',[nil,nil],Str) of
        0: break;
        1: style := Str;
        2: begin
          P := pointer(Str);
          while P<>nil do begin
            SetLength(Points,n+1);
            with Points[n] do begin
              X := GetInt(P);
              Y := GetInt(P);
            end;
            inc(n);
          end;
        end;
      end;
    result := (n>0);
    if result then
      FillStroke(pointer(style),Canvas);
  end;

begin
  result := nil;
  S := pointer(Source);
  repeat
    Tag := GetTag(S);
    if S=nil then
      exit;
  until StrLComp(Tag,'svg ',4)=0;
  inc(Tag,4);
  width := 0;
  height := 0;
  repeat until GetAttr(Tag,'width=height=',[@width,@height],Str)=0;
  result := TMetaFile.Create;
  try
    result.Width := Width;
    result.Height := Height;
    DC := GetDC(0);
    Canvas := TMetaFileCanvas.Create(result,DC);
    try
      if WhiteBackground then begin
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(Rect(0,0,Width,Height));
      end;
      Canvas.Font.Name := 'Calibri';
      Canvas.Font.Size := 14;
      SetTextAlign(Canvas.Handle,TA_NOUPDATECP or TA_LEFT or TA_BASELINE);
      Canvas.Pen.Width := 2;
      repeat
        Tag := GetTag(S);
        if S=nil then break;
        case Tag[0] of
        'p':
        if StrLComp(Tag,'polyline ',9)=0 then begin
          inc(Tag,9);
          if GetStylePoints then
            Canvas.Polyline(Points);
        end else
        if StrLComp(Tag,'polygon ',8)=0 then begin
          inc(Tag,8);
          if GetStylePoints then begin
            if pos('fill:none',style)>0 then
              Canvas.Polyline(Points) else
              Canvas.Polygon(Points);
          end;
        end else
        if StrLComp(Tag,'path ',5)=0 then begin
          inc(Tag,5);
          path := '';
          while true do
            case GetAttr(Tag,'style=d=',[nil,nil],Str) of
              0: break;
              1: style := Str;
              2: path := Str;
            end;
          if path<>'' then begin
            FillStroke(pointer(style),Canvas);
            P := pointer(path);
            n := 0;
            while P<>nil do begin
              while P^=' ' do inc(P);
              cmd := P^;
              case cmd of
              'M': begin
                inc(P);
                n := 1;
                SetLength(Points,1);
                Points[0].X := GetInt(P);
                Points[0].Y := GetInt(P);
              end;
              'C', 'L': if n<>1 then break else begin
                inc(P);
                repeat
                  SetLength(Points,n+1);
                  with Points[n] do begin
                    X := GetInt(P);
                    Y := GetInt(P);
                  end;
                  inc(n);
                until (P=nil) or not (P^ in ['0'..'9']);
                case cmd of
                  'C': Canvas.PolyBezier(Points);
                  'L': Canvas.Polyline(Points);
                end;
                n := 0;
              end;
              end;
            end;
          end;
        end;
        'e': if StrLComp(Tag,'ellipse ',8)=0 then begin
          inc(Tag,8);
          while true do
            case GetAttr(Tag,'style=cx=cy=rx=ry=',[nil,@R.Left,@R.Top,@R.Right,@R.Bottom],Str) of
              0: break;
              1: style := Str;
            end;
          dec(R.Left,R.Right);
          dec(R.Top,R.Bottom);
          R.Right := R.Right*2+R.Left;
          R.Bottom := R.Bottom*2+R.Top;
          FillStroke(pointer(style),Canvas);
          Canvas.Ellipse(R);
        end;
        't': if StrLComp(Tag,'text ',5)=0 then begin
          inc(Tag,5);
          X := -1;
          Y := -1;
          while true do
            case GetAttr(Tag,'text-anchor=x=y=style=',[nil,@X,@Y,nil],Str) of
              0: break;
              1: style := Str;
            end;
          Tag := GetTag(S);
          if (X>=0) and (Y>=0) and (StrLComp(Tag,'![CDATA[',8)=0) then begin
            inc(Tag,8);
            SetString(Str,Tag,S-Tag-3);
            Str := StringReplaceAll(Str,'&lt;','<');
            Str := StringReplaceAll(Str,'&gt;','>');
            Str := StringReplaceAll(Str,'&amp;','&');
            if style='middle' then
              dec(X,Canvas.TextWidth(Str) shr 1);
            Canvas.Brush.Style := bsClear;
            Canvas.TextOut(X,Y,Str);
          end;
        end;
        end;
      until false;
    finally
      Canvas.Free;
      ReleaseDC(0,DC);
    end;
    if Dest<>'' then
      result.SaveToFile(Dest);
  finally
    if Dest<>'' then
      FreeAndNil(result);
  end;
end;

/// create a picture from a .dot content
// - the picture will be created in DestDir+FileName+GraphExt
// - GraphValues[GraphPath+FileName+GraphExt]='230x123 90% GraphTitle'
function WingraphvizCreateFile(Dot: Variant; const FileName, DestDir, GraphExt, Source,
  GraphPath, GraphTitle: string; GraphValues: TSection; Perc: integer): boolean;
var max: integer;
    Img: Variant;
    Pic: TPicture;
    FN, SVG: string;
    Kind: integer;
begin
  result := false;
  try
    if Dot.Validate(Source) then begin
      if pos('AuditTrail',FileName)>0 then
        Max := 850 else
      Max := 850;
      Kind := GetStringIndex(VALID_PICTURES_EXT,GraphExt);
      case Kind of
      0,1: Img := Dot.ToJPEG(Source);
      2:   Img := Dot.ToPNG(Source);
      3: begin
        Img := Dot.ToSVG(Source);
        Max := 1000;
      end;
      else exit;
      end;
      FN := FileName+VALID_PICTURES_EXT[Kind];
      if Kind=3 then begin
        SVG := Img;
        StringToFile(DestDir+FileName+'.dot',Source);
        //StringToFile(DestDir+FileName+'.svg',SVG);
        SVGToEMF(SVG,DestDir+FN);
      end else
        Img.Save(DestDir+FN);
      Pic := TPicture.Create;
      try
        Pic.LoadFromFile(DestDir+FN);
        if Perc=0 then
          if Pic.Width>Max then
            Perc := 100 else
            Perc := (Pic.Width*100) div Max; // resize small pictures
        if Assigned(GraphValues) then
          GraphValues[GraphPath+FN] :=
            format('%dx%d %d%%,%s',[Pic.Width,Pic.Height,Perc,GraphTitle]);
        result := true;
      finally
        Img := NULL; // release memory
        Pic.Free;
      end;
    end;
  except
    on Exception do
      result := false; // GraphViz can failed sometimes, even on correct input
  end;
end;

function TProjectBrowser.GraphFileNameOk(const name: string; addLastMinus: boolean): string;
var i: integer;
begin
  result := trim(name);
  for  i := length(result) downto 1 do
    if result[i] in ['\','/',' ',':'] then // 'd:\dev\lib' -> 'D-DEV-LIB'
      if (result[i+1]='-') or (i=1) then
        delete(result,i,1) else
        result[i] := '-';
  if Result='' then
    exit; // avoid GPF
  if result[length(result)]='-' then begin
    if not addLastMinus then
      SetLength(result,length(result)-1);
  end else
    if addLastMinus then
      result := result+'-';
end;


function GetValueFromIndex(List: TStrings; Index: Integer): string;
begin // not defined before Delphi 7
  if Index >= 0 then
    Result := Copy(List[Index], Length(List.Names[Index]) + 2, MaxInt) else
    Result := '';
end;

function TProjectBrowser.GraphVizEmf(aSection: TSection): boolean;
var Dot: Variant;
    Source, DestDir, dir, item, Ignore, aName,
    GraphHead, GraphTitle: string;
    W: TStringWriter;
    u, i: integer;
    p: TPasItem;
    Dep: TSection;
    pc: PChar;
    un: TPasUnit;
procedure WInit;
begin
  W.Len := 0;
  W.AddShort('digraph uses{'#13#10); // '  graph [fontname="Arial"];'#13#10);
  if graphHead<>'' then
    W.Add(graphHead); // '  graph [shape="box"];'#13#10);
  W.AddShort('  graph[rankdir="LR"];'#13#10);
end;
procedure WAdd(const aFrom,aTo: string);
begin
  W.AddShort('  ').Add(aFrom).AddShort(' -> ').Add(aTo).AddShort(';'#13#10);
end;
function WGet: string;
begin
  W.Add('}');
  result := W.Data;
end;
procedure CreateEmf(FileName: string);
var i: integer;
begin
  if Dep.Lines.Count=0 then
    exit;
  // 1. create .dot contents from Dep.Lines values
  WInit;
  for i := 0 to Dep.Lines.Count-1 do begin
    PC := pointer(GetValueFromIndex(Dep.Lines,i));
    aName := Dep.Lines.Names[i];
    while PC<>nil do
      WAdd(GetNextItem(PC),aName);
  end;
  Dep.Lines.Clear;
  // 2. create picture from .dot content
  Source := WGet;
  if (FileName<>'') and (DestDir[length(DestDir)]<>'\') then
    FileName := '-'+FileName;
  result := WingraphvizCreateFile(Dot,FileName,DestDir,GraphExt,Source,
    copy(DestDir,length(Project.FileNameDir)+1,maxInt),GraphTitle,GraphValues);
  if result then
    GraphValuesModified := true;
end;
procedure CreateClassEmf(const UnitName: string);
var LNode: TPasItemNode;
begin
  LNode := ClassHierarchy.FirstItem;
  if LNode=nil then exit;
  GraphHead := '';
  GraphTitle := '{\i '+GraphTitle+'} class hierarchy';
  Dep.Lines.Clear;
  while LNode<>nil do begin
    if (LNode.Parent<>nil) and (LNode.Parent.Name <> '') then
      Dep.AddCSVValue(LNode.Name,LNode.Parent.Name,true);
    LNode := ClassHierarchy.NextItem(LNode);
  end;
  CreateEmf(UnitName);
end;
begin
  result := false;
  if not GetWingraphviz(Dot) then exit;
  Dep := TSection.Create('Dep'); // will contain all units dependencies
  try
  try
    LoadGraphValues;
    W.Init;
    Ignore := aSection['GraphIgnore'];
    DestDir := Project.FileNameDir+GraphDirName+aSection.SectionName;
    dir := '';
    GraphHead := '  node [shape="box"];'#13#10;
    // 1. graphs of Unit dependencies
    for u := 0 to Units.Count-1 do
    with TPasUnit(Units[u]) do
    if isUnit and not CSVContains(Ignore,Name) then begin
      item := ExtractFilePath(OutputFileName);
      if item<>dir then begin // directory change -> create graph
        if dir<>'' then
          CreateEmf(GraphFileNameOK(dir,false));
        dir := item;
      end;
      GraphTitle := 'Unit dependencies in the "'+
        RtfBackSlash(ExcludeTrailingPathDelimiter(dir))+'" directory';
      for i := 0 to InterfaceUsesUnits.Count-1 do begin
        p := Units.FindName(InterfaceUsesUnits[i]);
        if (p=nil) or (p.ClassType<>TPasUnit) or CSVContains(Ignore,p.Name) then continue;
        Dep.AddCSVValue(Name,p.Name,true);
      end;
    end;
    if dir<>'' then // create graph if not already done
      CreateEmf(GraphFileNameOK(dir,false));
(*    DestDir := GraphDir;
    for u := 0 to Units.Count-1 do
    with TPasUnit(Units[u]) do
    if isUnit and not CSVContains(GraphVizUnits,Name) then begin
      CSVAdd(GraphVizUnits,Name);
      Dep.Lines.Clear;
      for i := 0 to InterfaceUsesUnits.Count-1 do begin
        p := Units.FindName(InterfaceUsesUnits[i]);
        if (p=nil) or (p.ClassType<>TPasUnit) then continue;
        Dep.AddCSVValue(Name,p.Name,true);
        for j := 0 to TPasUnit(p).InterfaceUsesUnits.Count-1 do begin
          p2 := Units.FindName(TPasUnit(p).InterfaceUsesUnits[j]);
          if (p2=nil) or (p2.ClassType<>TPasUnit) then continue;
          Dep.AddCSVValue(p.Name,p2.Name,true);
        end;
      end;
      CreateEmf(FileNameOk(OutputFileName,false));
    end; *)
    // 2. graphs of full class hierarchy
    CreateClassHierarchy;
    GraphTitle := aSection.DisplayName(nil);
    CreateClassEmf('');
    // 3. graphs of class hierarchy of all units
    for u := 0 to Units.Count-1 do begin
      un := TPasUnit(Units[u]);
      CreateClassHierarchyFor(un);
      GraphTitle := un.Name;
      DestDir := Project.FileNameDir+GraphDirName;
      CreateClassEmf(GraphFileNameOk(ValAt(un.OutputFileName,0,'.'),false));
    end;
  except
    on E: Exception do
      MessageBox(0,pChar(E.Message),'Error creating graphs',MB_ICONERROR);
  end;
  finally
    Dot := Null; // release memory
    Dep.Free;
  end;
end;

procedure TProjectBrowser.LoadGraphValues;
begin
  if GraphValues<>nil then exit;
  GraphValues := TSection.Create('GraphValues');
  GraphValues.LoadFromFile(Project.FileNameDir+GraphDirName+GraphValues.SectionName+'.ini');
  if GraphValues.Lines.Count>0 then begin
    GraphExt := ExtractFileExt(ValAt(GraphValues.Lines[0],0,'='));
    Project.Pictures.Lines.AddStrings(GraphValues.Lines);
  end;
end;
{$endif}

/// convert a \graph content (from lines) into a .dot compatible
function WingraphvizFromText(const UniqueImageName: string; Buffer: TStrings; BufferStartIndex: integer=0): string;
var line: string;
    PC: PChar;
    DefaultNode: boolean;
function CR(const Source: string): string;
begin
  result := StringReplaceAll(Source,#164,'\n')
end;
function CRSep(Sep: char): string;
begin
  result := CR(GetNextItem(PC,Sep));
end;
begin
  DefaultNode := true;
  result := '';
  while BufferStartIndex<Buffer.Count do begin
    line := trim(Buffer[BufferStartIndex]);
    inc(BufferStartIndex);
    PC := pointer(line);
    if PC=nil then break; // process until void line
    if PC^='=' then begin
      // e.g. "=1=OSI 1¤Physical layer"
      inc(PC);
      result := result+'"'+CRSep('=');
      line := CR(PC);
      result := result+'" [label="'+line+'"];';
    end else
    if PC^='\' then begin
      // e.g. "\From Text\To Text[\Label between both]"
      // or "\One Text=Other text at same rank=3rd text at same rank"
      inc(PC);
      if PC^=#0 then break; // process until "\" line
      if (StrScan(PC,'\')=nil) and (StrScan(PC,'=')<>nil) then begin
        result := result+'{ rank=same; ';
        repeat
          result := result+'"'+CRSep('=')+'"; ';
        until PC=nil;
        result := result+'}'#13#10;
      end else begin
        result := result+'"'+CRSep('\')+'" -> "'+CRSep('\')+'"';
        if PC<>nil then
          result := result+' [label="'+CR(PC)+'"];'#13#10 else
          result := result+';'#13#10;
      end;
    end else begin
      // a .result normal line
      if pos('node [',line)>0 then
        DefaultNode := false;
      result := result+line+#13#10;
    end;
  end;
  if DefaultNode then
    result := 'node [color=lightblue1, style=filled];'#13#10+result;
  result := 'digraph '+UniqueImageName+' {'#13#10+
    result+'}'#13#10;
end;

{$I-}
procedure TProjectBrowser.LogMessage(const MessageType: TMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  case MessageType of
    mtInformation: WriteLn(LogFile,' Info[', AVerbosity, ']:    ', AMessage);
    mtWarning: WriteLn(LogFile,' Warning[', AVerbosity, ']: ', AMessage);
    mtError: WriteLn(LogFile,' Error[', AVerbosity, ']:   ', AMessage);
  else
    WriteLn(LogFile,AMessage);
  end;
  ioresult;
end;
{$I+}

function MainDescription(const Raw: string): string;
var P: PChar;
    line: string;
    i: integer;
begin
  result := '';
  P := pointer(Raw);
  if P=nil then
    exit;
  repeat
    while P^=' ' do inc(P); // trim left of line
    line := GetNextLineRightTrim(P,P);
    if (line='') or (line[1] in ['!','&','#','µ','$','-']) then
      break;
    for i := length(line) downto 1 do
      if line[i] in ['{','}'] then insert('\',line,i) else
      if (line[i]='\') and not IdemPChar(@line[i+1],'LINE') then
        insert('\',line,i);
    if result='' then begin
      line[1] := upcase(line[1]);
      result := line;
    end else
      result := result+' '+line;
  until P^=#0;
  if Result<>'' then
    result := '{\fs20 '+result+'}';
end;
                     
function TProjectBrowser.RtfDescription(Item: TPasItem; W: TProjectWriter): boolean;
var P: PChar;
    line: string;
    i,j: integer;
    ToFind: string;
    Find: TBaseItem;
    WLen: integer;
    inMain: boolean;
begin
  result := false;
  P := pointer(Item.RawDescriptionInfo.Content);
  if P=nil then
    exit;
  WLen := W.Len;
  inMain := False;
  repeat
    while P^=' ' do inc(P); // trim left of line
    line := GetNextLineRightTrim(P,P);
    if line='' then begin
      if P^=#0 then break;
      if W.len<>WLen then
        W.RtfPar; // void line -> force paragraph
      continue;
    end;
    // make rtf happy
    if not (line[1] in ['!','&','#','µ','$']) then begin
      for i := length(line) downto 1 do
        if line[i] in ['{','}'] then insert('\',line,i) else
        if (line[i]='\') and not IdemPChar(@line[i+1],'LINE') then
          insert('\',line,i);
      if W.len=WLen then begin
        line[1] := UpCase(line[1]);
        line := '{\i '+line;
        inMain := true;
      end;
      // find any component name used in this line -> Fixed Font
      j := 1;
      if Item.MyUnit<>nil then
      repeat
        while (j<=length(line)) and (line[j] in [' ','(','[']) do
          inc(j);
        i := j;
        while (i<=length(line)) and not (line[i] in [' ','(','[']) do
          inc(i);
        ToFind := copy(line,j,i-j);
        Find := Item.FindName(OneNamePart(ToFind));
        if Find<>nil then begin
          insert('}',line,i);
          insert('{\f1 ',line,j);
        end;
        j := i;
      until j>=length(line);
    end;
    // find @TComponent and write it as Fixed Font
    j := 1;
    repeat
      i := posEx('@',line,j);
      if i=0 then break;
      j := i+1;
      while line[j] in ['A'..'Z','0'..'9','a'..'z','_'] do
        inc(j);
      ToFind := copy(line,i+1,j-i-1);
      if ToFind='' then continue;
      Find := FindGlobal(OneNamePart(ToFind));
      if Find=nil then continue;
      insert('}',line,j);
      line[i] := ' ';
      insert('{\f1',line,i);
    until false;
    // write line
    case line[1] of
    '!','&','#','µ','$','-': begin
      if inMain then begin
        inMain := false;
        W.AddRtfContent('}');
      end;
      if W.Last=lastRtfText then
        W.RtfLine;
      if not W.RtfCode(line) then
        W.RtfText.AddRtfContent(line); // handle '-' line
    end;
    else begin
      if W.Len<>WLen then // put comments lines on the same paragraph
        if W.Last=lastRtfText then
          W.AddRtfContent(' ') else
          W.RtfText else
        W.RtfText;
      W.AddRtfContent(line);
    end;
    end;
  until P^=#0;
  if inMain then
    W.AddRtfContent('}');
  W.RtfText; // close any pending list
  result := WLen<>W.Len;
end;


procedure TProjectBrowser.RtfUnitDescription(aUnit: TPasUnit; WR: TProjectWriter; TitleLevel: integer;
  withFields: boolean);
var procnames: string; // CSV TClass.Funct,globalproc to be highlighted
    unitName: string;
procedure Details(Items: TPasItems; const ItemName: string;
  withVoid, insideTable, ConstsResourceOnly, HeadLines: boolean);
procedure FullDeclCorrect(var decl: string);
var j: integer;
begin
  for j := length(decl) downto 1 do
    case decl[j] of
      #10: if decl[j+1]=' ' then delete(decl,j,1) else decl[j] := ' ';
      ' ': if decl[j+1]=' ' then delete(decl,j,1);
      #13: delete(decl,j,1);
    end;
end;
var i, j: integer;
    p: TPasItem;
    ok: boolean;
    decl, anc: string;
    line: TProjectWriter;
    isCio, hasFields: boolean;
procedure LinePage(const SectionNameValue: string);
begin
  line.RtfLinkTo(SectionNameValue,SectionNameValue).AddRtfContent(' (page ');
  line.RtfPageRefTo(SectionNameValue,true,false).AddRtfContent(')');
end;
procedure Field(Fields: TPasItems; const FieldName: string);
var f, i: integer;
    m: TPasItem;
    decl, fullName: string;
    ok, highlight: boolean;
begin
  if (Fields=nil) or (Fields.Count=0) then exit;
  for f := 0 to Fields.Count-1 do begin
    m := TPasItem(Fields[f]);
    if m.MyObject=nil then
      m.MyObject := TPasCio(p);
    if m.MyUnit=nil then
      m.MyUnit := aUnit;
    fullName := m.MyObject.Name+'.'+m.Name;
    highlight := CSVContains(procnames,fullName);
    if not highlight and not withVoid and (m.RawDescriptionInfo.Content='') then
      continue;
    line.Clear;
    decl := m.FullDeclaration;
    if decl='' then
      decl := m.Name else
      FullDeclCorrect(decl);
    line.AddRtfContent('\li160 ');
    if highlight then
      decl := '!'+decl;
    line.RtfPascal(decl,92);
    line.AddRtfContent('\sb40\li320 ');
    ok := RtfDescription(m,line);
    if highlight then begin
      for i := 0 to high(Project.ParseSAD.List) do
      with Project.ParseSAD.List[i] do
        if CSVContains(Value[unitName],fullName) then begin
          if highlight then begin
            if ok then
              line.RtfPar;
            line.AddRtfContent('{');
            line.RtfBookMark('\i '+sUsedFor+' ',
              BookMarkHash(m.MyUnit.OutputFileName+'.'+fullName));
            highlight := false;
          end else
            line.AddRtfContent(', ');
          LinePage(SectionNameValue);
        end;
      if not highlight then
        line.AddRtfContent('}.');
    end;
   { if not line.RtfValid then
      MessageBox(0,pointer(line.Data),pointer(aUnit.Name),0); }
    if insideTable then
      WR.RtfRow([line.Data]) else begin
      line.RtfPar;
      line.SaveToWriter(WR);
    end;
  end;
end;
function hasFied(Fields: TPasItems): boolean;
var f: integer;
    fullName: string;
begin
  result := true;
  if withVoid and (Fields.Count>0) then
    exit;
  fullName := p.Name+'.';
  for f := 0 to Fields.Count-1 do
  with TPasItem(Fields[f]) do
    if (RawDescriptionInfo.Content<>'') or CSVContains(procnames,fullName+Name) then
      exit;
  result := false;
end;
var highlight: boolean;
    OKs: array of (okNone, okHighlighted, okNormal);
    SL: TStringList;
begin
  if (Items=nil) or (Items.Count=0) then exit;
  ok := false;
  line := TProjectWriter.CreateFrom(WR);
  SetLength(OKs,Items.Count);
  SL := TStringList.Create;
  try
    for i := 0 to Items.Count-1 do begin
      p := TPasItem(Items[i]);
      if p.MyUnit=nil then
        p.MyUnit := aUnit;
      highlight := CSVContains(procnames,p.Name);
      if not highlight and not withVoid and (p.RawDescriptionInfo.Content='') then
        continue;
      if p.InheritsFrom(TPasConstant) then
        if (ConstsResourceOnly<>TPasConstant(p).IsResourceString) then
          continue;
      if highlight then
        OKs[i] := okHighlighted else
        OKs[i] := okNormal;
      if HeadLines then
        SL.AddObject(p.Name,pointer(i));
      ok := true;
    end;
    if not ok then
      exit;
    WR.AddRtfContent('{\sb220\b ').
      AddRtfContent(sImplementedInN,[ItemName,aUnit.Name]).
      AddRtfContent(':\b0\par}');
    if HeadLines then begin
      WR.RtfColsPercent([26,66,8],false,true,true,'\trhdr');
      WR.RtfRow(['\b\ql '+ItemName,sDescription,'\qc '+sPage+'\b0'],true);
      WR.RtfColsPercent([26,66,8],false,true,true,'\trkeep');
      SL.Sort;
      for i := 0 to SL.Count-1 do
      if OKs[Integer(SL.Objects[i])]<>okNone then begin
        p := TPasItem(Items[Integer(SL.Objects[i])]);
        WR.RtfRow(['\ql{\f1\fs18 '+p.Name+'}',
          MainDescription(p.RawDescriptionInfo.Content),
          '\qc '+RtfPageRefTo(p.QualifiedName,True)]);
      end;
      WR.RtfColsEnd.RtfPar;
    end;
    if insideTable then
      WR.RtfColsPercent([100],false,false,false,'\trkeep');
    ok := false;
    for i := 0 to Items.Count-1 do
    if OKs[i]<>okNone then begin
      p := TPasItem(Items[i]);
      isCio := p.InheritsFrom(TPasCio);
      highlight := OKs[i]=okHighlighted;
      hasFields := isCio and withFields and (hasFied(TPasCio(p).Fields) or
        hasFied(TPasCio(p).Methods) or hasFied(TPasCio(p).Properties));
      if p.InheritsFrom(TPasEnum) then
      with TPasEnum(p) do begin
        decl := Name+' = ';
        if Members.Count<>0 then begin
          if Members.Count>5 then
            decl := decl+'\line( ' else
            decl := decl+'( ';
          for j := 0 to Members.Count-1 do
          with TPasItem(Members[j]) do begin
            decl := decl+TPasItem(Members[j]).Name;
            if j<Members.Count-1 then
              decl := decl+', ';
  {          if RawDescriptionInfo.Content<>'' then
              decl := decl+' // '+RawDescriptionInfo.Content+'\line'; }
          end;
          decl := decl+' );'
        end else
          decl := Name+' = ();';
      end else begin
        decl := p.FullDeclaration;
        if decl='' then begin
          decl := p.Name;
          if isCio then begin // field
            decl := decl+' = '+CIO_NAMES[TPasCio(p).MyType];
            anc := TPasCio(p).FirstAncestorName;
            if anc<>'' then
              decl := decl+'('+anc+')';
          end;
        end else
          FullDeclCorrect(decl);
      end;
      line.Clear;
      line.AddRtfContent('\li0 ');
      if highlight then
        decl := '!'+decl; // highlight object modified
      if hasFields then begin
  {      j := pos('\cellx',WR.fCols);
        if j>0 then
          insert('\clbrdrl\brdrs',WR.fCols,j); // manual border left }
        if ok then
          line.RtfPar; // gap between objects, not before the first item
        line.RtfPascal(decl,100);
      end else
        line.RtfPascal(decl,92);
      line.AddRtfContent('\sb40\li160 ');
      RtfDescription(p,line);
      if highlight then begin
        for j := 0 to high(Project.ParseSAD.List) do
        with Project.ParseSAD.List[j] do
          if CSVContains(Value[unitName],p.Name) then begin
            if highlight then begin
              line.RtfPar.AddRtfContent('{\i '+sUsedFor+' ');
              highlight := false;
            end else
              line.AddRtfContent(', ');
            LinePage(SectionNameValue);
          end;
        if not highlight then
          line.AddRtfContent('}.');
      end;
      WR.RtfBookMark('',p.QualifiedName);
      if insideTable then
        WR.RtfRow([line.Data]) else
        line.RtfPar.SaveToWriter(WR);
      if hasFields then begin
  //      WR.fCols := StringReplaceAll(WR.fCols,'\clbrdrl\brdrs',''); // reset border
        Field(TPasCio(p).Fields,'fields');
        Field(TPasCio(p).Methods,'methods');
        Field(TPasCio(p).Properties,'properties');
      end;
      ok := true;
    end;
    if ok then
      WR.RtfColsEnd.RtfParDefault;
  finally
    SL.Free;
  end;
end;
var i,j: integer;
    p: TPasItem;
    ok: boolean;
begin
  unitName := aUnit.Name+'.pas';
  WR.RtfTitle(aUnit.Name+' unit',TitleLevel,(TitleLevel>=0),aUnit.OutputFileName);
  WR.AddRtfContent('{\i '+sPurpose+'}: '+aUnit.UnitDescription(false));
  WR.RtfPar;
  procnames := '';
  if withFields then
  for j := 0 to high(Project.ParseSAD.List) do
  if CSVContains(Project.ParseSAD.List[j].Value['UnitsUsed'],aUnit.OutputFileName) then begin
    WR.AddRtfContent('{\sb220\b %s:\b0\par}',[format(sQuotedInN,[aUnit.Name])]);
    WR.RtfColsPercent([17,75,9],true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b '+Project.ParseSAD.Owner.ItemName+' #',
      '\ql '+sDescription,'\qc '+sPage+'\b0'],true);
    WR.RtfColsPercent([17,75,9],true,true,false,'\trkeep');
    for i := j to high(Project.ParseSAD.List) do
    with Project.ParseSAD.List[i] do
      if CSVContains(Value['UnitsUsed'],aUnit.OutputFileName) then begin
       WR.RtfRow(['\qc '+WR.RtfGoodSized(SectionNameValue),
        '\ql '+TrimLastPeriod(Owner.ShortDescription('')),
        '\qc '+RtfPageRefTo(SectionNameValue,true)]);
        CSVAddOnceCSV(procnames,Value[aUnit.Name+'.pas']);
      end;
    WR.RtfColsEnd;
    break;
  end;
  ok := false;
  if not isTrue(Project.ParseSAD^.Params['NoUsesUnits']) then
  for i := 0 to aUnit.UsesUnits.Count-1 do begin
    p := Units.FindName(aUnit.UsesUnits[i]);
    if (p=nil) or (p.ClassType<>TPasUnit) then continue;
    if not ok then begin
      WR.AddRtfContent('{\sb220\b ').
        AddRtfContent(sUnitsUsedInN,[aUnit.Name]).
        AddRtfContent(':\b0\par}');
      WR.RtfFont(92);
      WR.RtfColsPercent([26,66,8],false,true,true,'\trhdr');
      WR.RtfRow(['\b\ql '+sUnitName,sDescription,'\qc '+sPage+'\b0'],true);
      WR.RtfColsPercent([26,66,8],false,true,true,'\trkeep');
      ok := true;
    end;
    WR.RtfRow(['\ql{\i '+p.Name+'}',TPasUnit(p).UnitDescription(false),
        '\qc '+RtfPageRefTo(TPasUnit(p).OutputFileName,true)]);
  end;
  if ok then
    WR.RtfColsEnd;
  WR.RtfFont(100);
{$ifdef WITH_GRAPHVIZ}
  AddGraph(GraphFileNameOk(ValAt(aUnit.OutputFileName,0,'.'),false),WR);
{$endif}
  Details(aUnit.CIOs,'Objects',false,true,false,true); // need table for properties
  Details(aUnit.Types,'Types',false,false,false,false);
  Details(aUnit.Constants,'Constants',false,false,false,false);
  Details(aUnit.FuncsProcs,'Functions or procedures',false,true,false,true); // need table
  Details(aUnit.Variables,'Variables',false,true,false,false); // need table
  // Details(aUnit.Constants,'Resource Strings',false,true,true); pointless
end;

procedure TProjectBrowser.RtfUsesUnits(WR: TProjectWriter);
var item,dir,desc: string;
    u: integer;
procedure EndDir;
begin
  if dir<>'' then begin
    WR.RtfColsEnd;
{$ifdef WITH_GRAPHVIZ}
    WR.AddRtfContent('\line');
    AddGraph(CurrentSection.SectionName+'-'+GraphFileNameOk(dir,false),WR);
{$endif}
  end;
end;
begin
  dir := '';
  for u := 0 to Units.Count-1 do
  with TPasUnit(Units[u]) do
  if isUnit then begin
    item := ExtractFilePath(OutputFileName);
    if item<>dir then begin
      EndDir;
      dir := item;
      WR.AddRtfContent('\b ').
        AddRtfContent(sUnitsLocatedInN,[RtfBackSlash(dir)]).
        AddRtfContent(':\b0\par'#13);
      WR.RtfColsPercent([31,60,9],false,true,true,'\trhdr');
      WR.RtfRow(['\b\ql '+sSourceFileName,sDescription,'\qc '+sPage+'\b0'],true);
      WR.RtfColsPercent([31,60,9],false,true,true,'\trkeep');
    end;
    desc := UnitDescription(true);
    Project.ParseSAD.Params[OutputFileName] := desc; // write description in [SAD].EIA\Name.pas=...
    WR.RtfRow(['\ql{\i '+Name+'}',desc,'\qc '+RtfPageRefTo(OutputFileName,true)]);
  end;
  EndDir;
end;

procedure TProjectBrowser.RtfUsesUnitsDescription(WR: TProjectWriter; TitleLevel: integer;
  const CSVUnitsWithFields: string);
var u: integer;
    aUnit: TPasUnit;
    withFields: boolean;
begin
  for u := 0 to Units.Count-1 do begin
    aUnit := TPasUnit(Units[u]);
    if not aUnit.isUnit or (UnitsDescription.IndexOf(aUnit.OutputFileName)>=0) then
      continue;
    UnitsDescription.Add(aUnit.OutputFileName);
    if CSVUnitsWithFields='*' then
      withFields := true else
      withFields := CSVContains(CSVUnitsWithFields,aUnit.OutputFileName);
    RtfUnitDescription(aUnit,WR,TitleLevel,withFields);
  end;
end;

{$ifdef WITH_GRAPHVIZ}
initialization
  CoInitialize(nil);
finalization
  CoUninitialize;
{$endif}
end.

