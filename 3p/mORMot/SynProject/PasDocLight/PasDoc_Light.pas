/// Delphi source code parser, based on PasDoc units
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit PasDoc_Light;

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

{$I pasdoc_defines.inc}

// must have been set in global compiler directives or {$I pasdoc_defines.inc}:
{$define USEZIPCACHE}
{$define DONTUSETAGS}

uses
  ProjectCommons,
  Windows,
  SysUtils,
  Classes,
{$ifdef USEZIPCACHE}
  SynZipFiles,
{$endif}
  PasDoc_Parser,
  PasDoc_Utils,
  PasDoc_Items,
  PasDoc_Types,
  PasDoc_StringVector,
  PasDoc_SortSettings;

const
  DEFAULT_VERBOSITY_LEVEL = 2;

type
  TPasDocLight = class(TComponent)
  protected
    FDirectives: TStringVector;
    FIncludeDirectories: TStringVector;
    FSourceFileNames: TStringVector;
    FUnits: TPasUnits;
    FOnMessage: TPasDocMessageEvent;
    FVerbosity: Cardinal;
    FShowVisibilities: TVisibilities;
    FImplicitVisibility: TImplicitVisibility;
    FCommentMarkers: string;
    FMarkerOptional: boolean;
    FHandleMacros: boolean;
    FSortSettings: TSortSettings;
    FFileNotFound: TStringList;

{$ifdef USEZIPCACHE}
    // fast read unit from cache
    FCacheReader: TZipReader;
    FCacheReaderSAE: TZipReader;
    // 'ZipName' + Objects[]=TMemoryStream of Data
    // so all new cache values will be written at once in Execute
    FCacheWriter: TStringList;
    FCacheZipPath: string;
{$endif}
    FCacheName: string;
    FCacheNameSAE: string;
    FOutdatedCacheAutoRecreate: boolean;
    FForceCacheRecreateAll: boolean;

    procedure SetSourceFileNames(const ASourceFileNames: TStringVector);
    procedure SetDirectives(const Value: string);
    procedure SetIncludeDirectories(const Value: string);
  public
    // 'D:\DEV\', e.g.
    UpperDefaultPath: string;
    // added as prefix to SourceFileName if not 'D:\...'
    SourceDefaultPath: string;
    // if true, the Uses clause in implementation will be added to UsesUnits[]
    UnitUsesAlsoInImplementation: boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { add an individual Source file (.pas, .dpr) to Units[] (if not already)
      from cache or parsing with params already Set in object property }
    procedure ParseFile(const SourceFileName: string);
{$ifdef USEZIPCACHE}
    // write FCacheWriter to cache
    procedure ZipCacheUpdate;
    // close any opened FCacheReaderSAE
    procedure CloseCacheSAE(RenameForBackup, CopyBackup: boolean);
{$endif}
    { Starts filling Units[] with SourceFileNames[] from cache or parsing
      with params already Set in object property }
    procedure Execute; overload;

    { Starts filling Units[] with corresponding parameters
      (easier to use) }
    procedure Execute(const aProjectName,
      aSourceDefaultPath,
      aSourceFileNames,
      aCacheName,
      aDirectives,
      aIncludeDirectories,
      aCommentMarkers: string;
      aMarkersOptional: boolean = false;
      aSortSettings: TSortSettings = [];
      aShowVisibilities: TVisibilities = [viProtected, viPublic, viPublished, viAutomated];
      aImplicitVisibility: TImplicitVisibility = ivPublic;
      aOutdatedCacheAutoRecreate: boolean = true;
      aForceCacheRecreateAll: boolean = false;
      aHandleMacros: boolean = false); overload;

    { Raises an exception. }
    procedure DoError(const AMessage: string; const AArguments: array of
      const; const AExitCode: Word);

    { Forwards a message to the @link(OnMessage) event. }
    procedure DoMessage(const AVerbosity: Cardinal; const AMessageType:
      TMessageType; const AMessage: string; const AArguments: array of const);

    { for Generator messages }
    procedure GenMessage(const MessageType: TMessageType; const
      AMessage: string; const AVerbosity: Cardinal);

    { Adds one source filename ('*.pas' e.g.), searching also from IncludeDirectories }
    procedure AddOneSourceFileName(const FileMask: string);

    { Adds one source from unit name ('FastMM4' e.g.) }
    procedure AddOneSourceUnit(UnitName: string);

    { Adds source filenames, may be delimted by Commas }
    procedure AddSourceFileName(const FileMaskCSV: string);

    { Adds source filenames from a stringlist }
    procedure AddSourceFileNames(const AFileNames: TStringList);

    function FilesNotFoundList: string;

    property SourceFileNames: TStringVector read FSourceFileNames write
      SetSourceFileNames;

    { Sets Directives in Borland's format: 'WIN32;NOVCL' }
    property Directives: string write SetDirectives;

    { Sets IncludeDirectories in Borland's format: 'D:\DEV\LIB;D:\DEV\TMS'}
    property IncludeDirectories: string write SetIncludeDirectories;

    property Verbosity: Cardinal read FVerbosity write FVerbosity
      default DEFAULT_VERBOSITY_LEVEL;

    property ShowVisibilities: TVisibilities read FShowVisibilities
      write FShowVisibilities;

    { See command-line option @--implicit-visibility documentation at
      [http://pasdoc.sipsolutions.net/ImplicitVisibilityOption].
      This will be passed to parser instance. }
    property ImplicitVisibility: TImplicitVisibility
      read FImplicitVisibility write FImplicitVisibility default ivPublic;

    { Sets CommentMarkers with every char (':.') }
    property CommentMarkers: string read FCommentMarkers write FCommentMarkers;

    { if true, CommentMarkers are ignored }
    property MarkerOptional: boolean read FMarkerOptional write FMarkerOptional
      default false;

    { @name points to a directory which will contain the cached units declaration,
      or a .zip filename containing all the cache data
      (if global USEZIPCACHE if defined in pasdoc_defines.inc include file) }
    property CacheName: string read FCacheName write FCacheName;

    { @name points to the .zip filename containing the external
      hand-made description of every code item }  
    property CacheNameSAE: string read FCacheNameSAE write FCacheNameSAE;

{$ifdef USEZIPCACHE}
    { @name points to a "virtual" directory name which will be added
      at the begining of every cache entry in Zip. It is therefore possible
      to use an unique zip cache file with multiple projects
      (if global USEZIPCACHE if defined in pasdoc_defines.inc include file) }
    property CacheZipPath: string read FCacheZipPath write FCacheZipPath;
{$endif}

    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
    
    { This determines how items inside will be sorted.
      See [http://pasdoc.sipsolutions.net/SortOption]. }
    property SortSettings: TSortSettings 
      read FSortSettings write FSortSettings default [];

    property HandleMacros: boolean
      read FHandleMacros write FHandleMacros default false;

    { This forces cache to be updated (i.e. parse source file again) if
      its contents is outdated }
    property OutdatedCacheAutoRecreate: boolean
      read FOutdatedCacheAutoRecreate write FOutdatedCacheAutoRecreate default true;

    property ForceCacheRecreateAll: boolean
      read FForceCacheRecreateAll write FForceCacheRecreateAll default false;

    { All TPasUnit objects which have been created from the list of file names
      during the parsing }
    property Units: TPasUnits read FUnits;
  end;



implementation

uses SynZip;

{ TPasDocLight }

procedure TPasDocLight.AddOneSourceFileName(const FileMask: string);
var
  SR: TSearchRec;
  Path, s: string;
  SearchResult: Integer;
begin
    { Just ignore empty FileMask }
    if FileMask = '' then exit;

    Path := ExtractFilePath(FileMask);
    if (Path='') or not DirectoryExists(Path) then
      Path := SourceDefaultPath+Path; // add prefix if not 'D:\...'

    SearchResult := SysUtils.FindFirst(Path+ExtractFileName(FileMask), 63, SR);
    if SearchResult <> 0 then
      FFileNotFound.Add(FileMask) else
      repeat
        if (SR.Attr and 24) = 0 then begin
          s := Path + SR.Name;
          if not FSourceFileNames.ExistsNameCI(s) then  // add once
            FSourceFileNames.Add(s)
        end;
        SearchResult := FindNext(SR);
      until SearchResult <> 0;
    SysUtils.FindClose(SR);
end;

{ Adds one source filename ('*.pas' e.g.), searching also from IncludeDirectories }
procedure TPasDocLight.AddOneSourceUnit(UnitName: string);
function OK(Path: string): boolean;
begin
  Path := Path+UnitName;
  result := FileExists(Path);
  if result and not FSourceFileNames.ExistsNameCI(Path) then  // add once
    FSourceFileNames.Add(Path);
end;
var i: integer;
begin
  if ExtractFileExt(UnitName)='' then
    UnitName := UnitName+'.pas';
  if OK(ExtractFilePath(UnitName)) then
    exit;
  if OK(SourceDefaultPath) then
    exit;
  for i := 0 to FIncludeDirectories.Count-1 do
    if OK(FIncludeDirectories[i]) then
      exit;
  FFileNotFound.Add(UnitName);
end;

procedure TPasDocLight.AddSourceFileName(const FileMaskCSV: string);
var Tmp: TStringList;
begin
  Tmp := TStringList.Create;
  Tmp.Delimiter := ';'; // Borland's format
  Tmp.DelimitedText := FileMaskCSV;
  AddSourceFileNames(Tmp);
  Tmp.Free;
end;

procedure TPasDocLight.AddSourceFileNames(const AFileNames: TStringList);
var i: Integer;
begin
  for i := 0 to AFileNames.Count - 1 do
    if SameText(ExtractFileExt(AFileNames[i]),'.pas') then
      AddOneSourceUnit(AFileNames[i]) else
      AddOneSourceFileName(AFileNames[i]);
end;

constructor TPasDocLight.Create(AOwner: TComponent);
begin
  inherited;
  FDirectives := NewStringVector;
  FDirectives.Delimiter := ';'; // Borland's format
  FIncludeDirectories := NewStringVector;
  FIncludeDirectories.Delimiter := ';'; // Borland's format
  FSourceFileNames := NewStringVector;
  FUnits := TPasUnits.Create(True);
  FImplicitVisibility := ivPublic;
  FVerbosity := DEFAULT_VERBOSITY_LEVEL;
  FFileNotFound := TStringList.Create;
  FFileNotFound.Sorted := true;
  FFileNotFound.Duplicates := dupIgnore;
end;

destructor TPasDocLight.Destroy;
begin
  FDirectives.Free;
  FIncludeDirectories.Free;
  FSourceFileNames.Free;
  FUnits.Free;
  FFileNotFound.Free;
  FCacheReader.Free;
  FCacheReaderSAE.Free;
  inherited;
end;

procedure TPasDocLight.DoError(const AMessage: string;
  const AArguments: array of const; const AExitCode: Word);
begin
  raise EPasDoc.Create(AMessage, AArguments, AExitCode);
end;

procedure TPasDocLight.DoMessage(const AVerbosity: Cardinal;
  const AMessageType: TMessageType; const AMessage: string;
  const AArguments: array of const);
begin
  if (AVerbosity <= FVerbosity) and Assigned(FOnMessage) then
    FOnMessage(AMessageType, Format(AMessage, AArguments), AVerbosity);
end;

procedure TPasDocLight.Execute;
var i: integer;
begin
  Units.Clear;
  for i := 0 to FSourceFileNames.Count-1 do
    ParseFile(FSourceFileNames[i]);
{$ifdef USEZIPCACHE}
  ZipCacheUpdate;
{$endif}
end;

{$ifdef USEZIPCACHE}
procedure TPasDocLight.ZipCacheUpdate;
var i: integer;
begin
  FreeAndNil(FCacheReader);
  if FCacheWriter<>nil then
  with TZip.Create(CacheName) do
  try
    DoMessage(2, mtInformation, 'Write new data to cache', []);
    for i := 0 to FCacheWriter.Count-1 do // first delete outdated data
      MarkDeleted(Reader.ZipNameIndexOf(FCacheWriter[i])); // must be before any Write
    for i := 0 to FCacheWriter.Count-1 do // write new data at once
      with THeapMemoryStream(FCacheWriter.Objects[i]) do
      try
        AddBuf(FCacheWriter[i],6,Memory,Size); // add corresponding Data
      finally
        Free; // free MemoryStream Data memory in TStringList.Objects[]
      end;
  finally
    Free; // TZip.Free
    FreeAndNil(FCacheWriter);
  end;
end;
{$endif}

procedure TPasDocLight.Execute(const aProjectName, aSourceDefaultPath,
  aSourceFileNames, aCacheName, aDirectives, aIncludeDirectories,
  aCommentMarkers: string; aMarkersOptional: boolean;
  aSortSettings: TSortSettings;
  aShowVisibilities: TVisibilities; aImplicitVisibility: TImplicitVisibility;
  aOutdatedCacheAutoRecreate, aForceCacheRecreateAll, aHandleMacros: boolean);
begin
{$ifdef USEZIPCACHE}
  if aProjectName<>'' then  // subpath for ZipName Entry
    CacheZipPath := IncludeTrailingPathDelimiter(aProjectName);
  CacheName := aCacheName; // zip file name for caching 
{$else}
  // cache in files -> CacheName=directory
  CacheName := IncludeTrailingPathDelimiter(aCacheName);
{$endif}
  Directives := aDirectives;
  IncludeDirectories := aIncludeDirectories;
  CommentMarkers := aCommentMarkers;
  MarkerOptional := aMarkersOptional;
  ShowVisibilities := aShowVisibilities;
  ImplicitVisibility := aImplicitVisibility;
  OutdatedCacheAutoRecreate := aOutdatedCacheAutoRecreate;
  ForceCacheRecreateAll := aForceCacheRecreateAll;
  HandleMacros := aHandleMacros;
  if aSourceDefaultPath<>'' then
    SourceDefaultPath := IncludeTrailingPathDelimiter(aSourceDefaultPath);
  SourceFileNames.Clear;
  SortSettings := aSortSettings;
  AddSourceFileName(aSourceFileNames);
  try
    Execute;
  except
    on E: Exception do
      DoMessage(0,mtError,E.Message,[]);
  end;
end;

procedure TPasDocLight.GenMessage(const MessageType: TMessageType;
  const AMessage: string; const AVerbosity: Cardinal);
begin
  DoMessage(AVerbosity, MessageType, AMessage, []);
end;

procedure TPasDocLight.ParseFile(const SourceFileName: string);
var
  p: TParser;
  U, Old: TPasUnit;
  LLoaded: boolean;
  SourceAge: integer;
  InputStream: THeapMemoryStream; // open file only if need to be parsed
  TrimSourceFileName,
  LCacheFileName: string;
{$ifdef USEZIPCACHE}
  MS: THeapMemoryStream;
  i: integer;
  IndexFromFCacheWriter: integer;
label ok;
{$endif}
begin
  Old := TPasUnit(Units.FindName(ExtractFileName(SourceFileName)));
  if (Old<>nil) and SameText(Old.SourceFileName,SourceFileName) then
    exit; // already parsed with the same exact source file name -> fast exit
  LLoaded := false;
  SourceAge := FileAge(SourceFileName);
  if (UpperDefaultPath<>'') and IdemPChar(pointer(SourceFileName),pointer(UpperDefaultPath)) then
    TrimSourceFileName := copy(SourceFileName,length(UpperDefaultPath)+1,maxInt) else
    TrimSourceFileName := SourceFileName;
{$ifdef USEZIPCACHE}
  // inner Zip name path from UpperDefaultPath or default CacheZipPath
  if (TrimSourceFileName<>SourceFileName) then
    LCacheFileName := TrimSourceFileName else
    LCacheFileName := CacheZipPath+ExtractFileName(SourceFileName);
  if FCacheWriter<>nil then begin
    IndexFromFCacheWriter := FCacheWriter.IndexOf(LCacheFileName);
    if IndexFromFCacheWriter>=0 then begin // we just already parsed it, but not in Units[] (should never happen)
      U := TPasUnit(TPasUnit.DeserializeFromStream(THeapMemoryStream(
        FCacheWriter.Objects[IndexFromFCacheWriter]))); // read unit from Stream stored in FCacheWriter
      U.CacheAge := SourceAge; // -> already up to date
      goto ok;
    end;
  end else
    IndexFromFCacheWriter := -1;
  if not ForceCacheRecreateAll then
  if (FCacheReader<>nil) or ((CacheName<>'') and FileExists(CacheName)) then
    begin
      if FCacheReader=nil then
        FCacheReader := TZipReader.Create(CacheName);
      i := FCacheReader.ZipNameIndexOf(LCacheFileName);
      if i>=0 then begin
        MS := THeapMemoryStream.Create;
        if FCacheReader.GetData(i,MS)<>nil then begin
          MS.Seek(0,soFromBeginning);
          DoMessage(2, mtInformation, '%s from cache', [TrimSourceFileName]);
          U := TPasUnit(TPasUnit.DeserializeFromStream(MS));
          U.CacheAge := FCacheReader.Entry[i].Header.fileInfo.zlastMod;
{$else}
  LCacheFileName := CacheName+ChangeFileExt(ExtractFileName(SourceFileName), '.pduc');
  if (CacheName <> '') and FileExists(LCacheFileName) then
    begin
      DoMessage(2, mtInformation, '%s from cache', [TrimSourceFileName]);
      U := TPasUnit(TPasUnit.DeserializeFromFile(LCacheFileName));
      U.CacheAge := FileAge(LCacheFileName);
{$endif}
      if U.CacheAge < SourceAge then
      begin
        if OutdatedCacheAutoRecreate then begin
          DoMessage(2, mtInformation, 'outdated %s cache -> recreate',
            [TrimSourceFileName]);
          FreeAndNil(U);
        end else begin
          DoMessage(2, mtInformation, 'Cache data for %s is outdated but kept',
            [TrimSourceFileName]);
          LLoaded := True;
        end;
      end else
        LLoaded := True;
{$ifdef USEZIPCACHE}
        end; // if FCacheReader.GetData(i,MS)<>nil
      end; // if i>=0
    end; // if (FCacheReader<>nil)
{$else}
    end; // if (CacheName <> '')
{$endif}

    if not LLoaded then
    begin
      if not FileExists(SourceFileName) then begin
        FFileNotFound.Add(ValAt(ExtractFileName(SourceFileName),0,'.'));
        exit;
      end;
      DoMessage(2, mtInformation, 'Now parsing file %s...', [TrimSourceFileName]);
      InputStream := THeapMemoryStream.Create;
      InputStream.LoadFromFile(SourceFileName);
      p := TParser.Create(InputStream, FDirectives, FIncludeDirectories,
        {$IFDEF FPC}@{$ENDIF} GenMessage, FVerbosity,
        SourceFileName, ExtractFilePath(SourceFileName), HandleMacros);
      try
      try
        p.ShowVisibilities := ShowVisibilities;
        p.ImplicitVisibility := ImplicitVisibility;
        p.CommentMarkers := FCommentMarkers;
        p.MarkersOptional := MarkerOptional;
        p.ParseUnitOrProgram(U,UnitUsesAlsoInImplementation);
      except
        on e: Exception do begin
          DoMessage(2, mtWarning,
            'Error %s: %s while parsing unit %s, continuing...',
            [e.ClassName, e.Message, ExtractFileName(SourceFileName)]);
          exit;
        end;
     end;
     finally
       p.Free;
     end;
    end;

ok: if FUnits.ExistsUnit(U) then begin
      DoMessage(2, mtWarning,
        'Duplicate unit name "%s" in files "%s" and "%s" (discarded)', [U.Name,
        TPasUnit(FUnits.FindName(U.Name)).SourceFileName, SourceFileName]);
      U.Free;
    end else
    begin
      U.SourceFileName := SourceFileName;
      U.SourceFileAge := SourceAge;
      FUnits.Add(U);

      { Now we know that unit was 100% successfully parsed.

        So now we save it to the cache. The current approach to cache
        stores in cache the exact state of unit as it was generated by
        parser (that why we can use deserialization as an equivalent
        of parsing), so we want to save the unit to cache *now*,
        in case some later processing would change some things.
        E.g. processing @deprecated tag will change item's
        IsDeprecated, processing @member and @value will change
        some item's RawDescription. We want to write the cache
        *before* such changes occur. }

      if (CacheName <> '') and not U.WasDeserialized then
{$ifdef USEZIPCACHE}
      if IndexFromFCacheWriter<0 then // not taken from FCacheWriter -> add
      begin
        MS := THeapMemoryStream.Create;
        U.SerializeObject(U,MS);
        if FCacheWriter=nil then begin
          FCacheWriter := TStringList.Create;
          FCacheWriter.CaseSensitive := false;
        end;
        FCacheWriter.AddObject(LCacheFileName,MS);
      end;
{$else}
        U.SerializeToFile(LCacheFileName);
{$endif}
    end;
end;

procedure TPasDocLight.SetDirectives(const Value: string);
begin
  FDirectives.DelimitedText := Value;
end;

procedure TPasDocLight.SetIncludeDirectories(const Value: string);
var i: integer;
    s: string;
begin
  FIncludeDirectories.DelimitedText := Value;
  for i := FIncludeDirectories.Count-1 downto 0 do begin // delete() -> downto 
    s := FIncludeDirectories[i];
    if s='' then 
      FIncludeDirectories.Delete(i) else begin
      s := IncludeTrailingPathDelimiter(s);
      if DirectoryExists(s) then
        FIncludeDirectories[i] := s else
      if UpperDefaultPath='' then
        FIncludeDirectories.Delete(i) else begin
        s := UpperDefaultPath+s;
        if not DirectoryExists(s) then
          FIncludeDirectories.Delete(i) else
          FIncludeDirectories[i] := s;
      end;
    end;
  end;
end;

procedure TPasDocLight.SetSourceFileNames(const ASourceFileNames: TStringVector);
begin
  AddSourceFileNames(ASourceFileNames);
end;


function TPasDocLight.FilesNotFoundList: string;
begin
  result := FFileNotFound.CommaText;
end;

procedure TPasDocLight.CloseCacheSAE(RenameForBackup, CopyBackup: boolean);
var bak: TFileName;
begin
  FreeAndNil(FCacheReaderSAE);
  if not RenameForBackup then
    exit;
  bak := ChangeFileExt(CacheNameSAE,'.~sae');
  DeleteFile(bak);
  if CopyBackup then
    CopyFile(pointer(CacheNameSAE),pointer(bak),false) else
    MoveFile(pointer(CacheNameSAE),pointer(bak));
end;

end.
