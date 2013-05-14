/// main classes, implementing document and project types
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectTypes;

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

{$ifndef DONTUSEPARSER} // for ProjectVersion, e.g.
{$define USEPARSER}
// attempt to get information from source code directly

{$define WITH_GRAPHVIZ}
// if defined, the WinGraphviz COM server will be used to generated diagrams
// (must be defined in ProjectParser, ProjectEditor and ProjectTypes)
{$endif}

uses
  Windows, SysUtils, Classes, Contnrs,
  ProjectRTF, ProjectCommons, ProjectSections, ProjectVersioning;

const
  /// all on-the-fly generated diagrams (from WinGraphviz) are stored in
  // this subfolder 
  GraphDirName = 'GG\';

type
  TRiskOneDef = record
    Name: string;
    Description: string;
    Level: array[1..3] of string;
  end;
  TRiskDef = array[0..2] of TRiskOneDef;

resourcestring
  sAbstract = 'Abstract';
  sAbstractN = '%s Abstract';
  sSpecification = 'Specification';
  sRiskDef = 'Severity,identify the severity of incorrect implementation,'+
    'Cosmetic or no effect to intended operation,'+
    'Potentially effects one or multiple features for intended operation,'+
    'Potentially affects a result or safety,'+
    'Probability,identify the probability of incorrect or incomplete implementation,'+
    'Documentation and familiar with the code,'+
    'Documentation but not familiar/familiar but no documentation,'+
    'No documentation and not familiar with the code area,'+
    'Occurrence,identify the reproducibility of the defect before correction,'+
    'One time: relatively few or remote likelihood of failure (<5% failure rate),'+
    'Intermittent or recurring: occasional failures (5-25% failure rate),'+
    'Reproducible: failure inevitable or repeated (>25% failure rate)';
  sRiskShort = 'Sev.,Pro.,Occ.,Management approval,Approved';
  sDocumentPurposeSentence =
    'The {\i %s} document purpose is to %s for the {\i %s} project.\par '+
    'The current revision of this document is %s.\par';
  sProjectDocumentation = 'PROJECT DOCUMENTATION';
  sTestProcedure = 'TEST PROCEDURE';
  sDocumentName = 'Document Name';
  sDocumentRevision = 'Document Revision';
  sDocumentPurpose = 'Document Purpose';
  sDisplayName = 'Display Name';
  sPreparedBy = 'Prepared by';
  sReviewedBy ='Reviewed by';
  sApprovedBy = 'Approved by';
  sCompany = 'Company Name';
  sProjectName = 'Project Name';
  sProtocolName = 'Protocol Name';
  sProtocolDescription = 'Protocol Description';
  sProtocolRevision = 'Protocol Revision';
  sProtocolDate = 'Protocol Date';
  sDateTested = 'Date Tested';
  sTestedBy = 'Tested by';
  sTestDate = 'Test Date';
  sPassFail = 'Pass / Fail';
  sYesNo = 'Yes / No';
  sTestFeatures = 'Test features';
  sActions = 'Actions';
  sProcedureRequires = 'This procedure requires:';
  sExpectedResults = 'Expected results';
  sSpecialRequirements = 'Special Requirements';
  sDescriptionRtfN = '{\i %s description}';
  sProcedureCriteria = 'Procedure Pass/Fail criteria';
  sProcedureCriteriaText = 'The procedure will pass if all actions are completed correctly '+
      'and no major problem is found';
  sSummarySheet = 'Summary sheet';
  sExpected = 'expected';
  sProcedureSteps = 'Procedure steps';
  sValidation = 'Validation';
  sObservation = 'Observation';
  sRequirement = 'Requirement';
  sReference = 'Reference';
  sTitle = 'Title';
  sSignature = 'Signature';
  sDate = 'Date';
  sRiskAssessment = 'Risk Assessment';
  sRiskAssessmentScale = 'Risk Assessment Scale';
  sHighMedLow = 'High,Med,Low';
  sDocumentRevisionTable = 'Document Revision Table';
  sRev = 'Rev.';
  sRevisionSharp = 'Revision #';
  sWrittenBy = 'Written by';
  sRef = 'Ref.';
  sAuthor = 'Author';
  sVersion = 'Version';
  sDescription = 'Description';
  sDescriptionN = '%s Description';
  sRequest = 'Request';
  sPage = 'Page';
  sBelow = 'below';
  sPageN = '(page %s)';
  sPageNN = 'Page %s of %s';
  sItemSharp = 'Item #';
  sGlobalArchitecture = 'Global Architecture';
  sSourceCodeImplementation = 'source';
  sUsedUnitsN = '%s used Units';
  sUsedUnitsTextN = 'The %s makes use of the following units.\par';
  sImplicationsN = '%s implications';
  sOverviewN = '%s overview';
  sDocumentNameN = '%s document name';
  sAssociatedItem = 'Associated Item';
  sAssociatedItems = 'Associated Items';
  sUnitTestingProtocol = 'Unit Testing Protocol';
  sNotImplemented = 'Not implemented';
  sRiskEvaluatedFromSub = 'Risk evaluated from sub-items';
  sRiskNotEvaluatedNoModif = 'Since there''s no code modification, no Risk has to be evaluated.';
  sRiskEvaluationTeam = 'Risk evaluation team';
  sDesignInputSplitDefault = 'This Design Input shall be traced to the following %s items';
  sQuickReferenceTableNN = 'The following table is a quick-reference guide to '+
     'all the software @%s@ items and their corresponding @%s@ items.\par';
  sQuickReferenceTableN = 'The following table is a quick-reference guide to '+
     'all the @%s@ items.\par';
  sQuickReferenceTableN2 = 'The following table is a quick-reference guide to '+
     'all the %s referenced in this @%s@.\par';
  sImplementation = 'Implementation';
  sTestExtractFromNN = 'Test case below is extracted from the "{\i%s "} document, which purpose is to %s';
  sSameAsN = 'Same as @%s@';
  sItemModifiedUnits = 'This specification is implemented by the following units';
  sItemQuotedUnits = ' The {\i Software Design Document} also quoted the following units';
  sUnitName = 'Unit Name';
  sSeeInParticular = 'See in particular ';
  sAnd = 'and';
  sDocNameParenthNN = '{\i %s} (%s) document';
  sCONFIDENTIAL = 'CONFIDENTIAL';
  sSourceReferenceTable = 'Source Reference Table';
  sSourceFileNames = 'Source code File Names';
  sReferenceTableN = '%s Reference Table';
  sOthers = 'Others';
  sName = 'Name';
  sRelatedDocuments = 'Related Documents';
  sPictures = 'Pictures';
  sIndex = 'Keywords';
  sTableOfContents = 'Table of Contents';
  sFoundIn = 'Found in';
  
const
  VALID_PROGRAM_EXT: array[0..13] of string =
    ('.PAS','.MOD','.C','.CPP','.H','.CS','.NSI', '.DEF', '.CXX', '.INC',
     '.XML','.HTML','.HTM', '.DFM');
  VALID_PROGRAM_CHAR: array[-1..high(VALID_PROGRAM_EXT)] of string =
    ('$', '!', 'µ', '&', '&', '&', '#', '$', 'µ', '&', '$', '$$', '$$', '$$', '!$');
  FRONTPAGE_OPTIONS: array[0..12] of string =
    ('ProjectDetails','Warning','PeopleDetails','RevisionDetails','AddPurpose',
     'RiskTable','NoHeader','TestDetails','NoConfidential','NoHeaderBorder',
     'HeaderWithLogo','FullTitleInTableOfContent','NoProjectDetailsLogo');
var
  RISKDEFS: TRiskDef;

procedure InitLang;
// init RISKDEFS[] with proper sRiskDef resourcestring value

type
  TSectionDynArray = array of TSection;
  PRisk = ^TRisk;
  TRisk = object
    // 1,1,3,Claude Mench+Arnaud Bouchez,Service SW is safe
    Risk: array[0..2] of integer; // (1,1,3)
    EvaluatedBy: string; // 'Claude Mench+Arnaud Bouchez'
    Comment: string;     // 'Service SW is safe'
    procedure FromString(const aString: string);
    function ToString: string;
    procedure SetWorse(aRisk: PRisk);
    function EvaluatedByDisplay: string; // 'Claude Mench, Arnaud Bouchez'
  end;

  PDocument = ^TDocument;
  TDocument = object // DI, SRS, SDD, SAD, Test... (has Revision=..)
    Params,
    Order: TSection;
    Owner: TSection; // from Owner=...
    // ex: DI->DI, SRS->DI, SDD->SRS, SAD->SRS, Test->SRS
    List: array of TSection; // from Order=...
    // ex: DI->DI-4.1,DI-4.2... SRS->SRS-DI-4.1,SRS-MENU01...
    TestDoc: PDocument; // [Test].BodyIsTest=Yes -> [Tests] DocByDescription=Test
    function GetParentIndex(Index: integer): integer;
    function GetPropertyFromParent(Index: integer; const aName: string;
      const aDefault: string = ''): string;
    function GetSectionIndex(aSection: TSection): integer; overload;
    function GetSectionIndex(const aSectionName: string): integer; overload;
    function GetSectionNameValueIndex(const aSectionNameValue: string): integer;
    function GetSection(const aSectionName: string): TSection;
  end;

  TProject = class
  // Document vars:
  public
    People,
    Project,
    Pictures,
    DILayout: TSection;
    Document: array of TDocument; // DI, SRS, SDD, SAD, Test... (has Owner=..)
    DI: PDocument; // points to TDocument for 'DI'
    Parse: array of TSection; // [SAD]:Source=EIA,IFA2 -> [SAD-EIA],[SAD-IFA2]
    ParseSAD: PDocument; // [SAD]
    ParseSDD: PDocument; // [SDD] section to be parsed for modified files as @!EIA\bidule.pas@
    ParseTest: PDocument; // [Test] section as DocByDescription=Test was called
    FileName, // 'D:\Dev\Synopse\Documents\Product New Version\Product New Version.pro'
    FileNameDir, // 'D:\Dev\Synopse\Documents\Product New Version\'
    DestinationDir: string; // 'D:\Documents\Product New Version\'
    OldWord2k: boolean; // OldWord2k := isTrue(Project['OldWordOpen'])
    // functions to search in Document[]:
    function DocumentIndex(aSection: TSection): integer; overload;
    function DocumentIndex(const aSectionName: string): integer; overload;
    function DocumentFind(aSection: TSection): PDocument; overload;
    function DocumentFind(const aSectionName: string): PDocument; overload;
    procedure DocumentAddSection(Doc: PDocument; aSection: TSection);
  private
    FData: TSectionsStorage;
  // used for document generation { TODO : sub class it? }:
  public
    Doc: PDocument; // current Document generated
    WRClass: TProjectWriterClass;
    WR: TProjectWriter;
    CreatedDocuments: string; // comma separated list of .doc/.rtf filenames
    DestroyOpensCreatedDocuments: boolean; // if true -> Destroy will launch docs
  private
    Header: record
      visible: boolean;
      withborder: boolean;
      withlogo: boolean;
      confidential: boolean;
      ProjectName,
      DocumentTitle,
      HeaderName,
      HeaderFunction,
      Rev,
      RevDate: string;
      LastFooterTitle: string;
      ColWidth: array[0..3] of integer;
    end;
    Layout: TProjectLayout;
    ForcedOnlySection: TSection; // for CreateRTFDetails() -> ignore all other
    TitleBookmark: TStringList;
    fGraphValues: TSection;
    // cross-references:
    ReferencePictures, // Picture Caption=Bookmark0,Bookmark1...
    ReferenceIndex,    // KeyWord 1=bookmark0,bookmark1...
    ReferenceProgram: TSection;  // something.pas=Bookmark0,Bookmark1...
    ReferenceImplements: TSection; // IEC 5.7 blabla=Bookmark0,Bookmark1...
    ReferencePicturesIndex,
    ReferenceProgramIndex,
    ReferenceImplementsIndex,
    ReferenceIndexIndex: integer;
    ReferenceDocuments: TList; // mapped to TSection
    ReferenceTablesPos, // used to write DocumentIndex=... tables after parsing
    ReferenceDocumentsPos: integer;
    ReferenceTablesTitleLevel: TTitleLevel;
    procedure TestGetDescr(Test, DIDetails: TSection; TestDoc: PDocument; out docname, title: string);
    procedure HeaderOnly;
    procedure HeaderAndFooter;
    procedure Footer(WR: TProjectWriter; const FooterTitle: string);
    procedure AddReferenceDocument(aSec: TSection);
    function PictureCaption(var FileName: string; Coords: PString=nil;
      aGraphValues: TSection=nil): string;
    function GetSourceOutputFileName(const ButtonFileName: string;
      mustExists: boolean): string; // 'IFA\Main.pas' -> 'IFA2\Main.pas'
    procedure SetData(aData: TSectionsStorage);
  public
    constructor Create(aClass: TProjectWriterClass; const aFileName: string = ''); reintroduce;
    destructor Destroy; override;
    procedure SaveToFile(const aFileName: string);
    procedure CreateRTF(ProjectDetails, Warning, PeopleDetails, RevisionDetails,
      AddPurpose, RiskTable, TestDetails, NoProjectDetailsLogo: boolean;
      Landscape: boolean; RevSection: TSection = nil;
      aDocumentTitle: string = ''; aRev: string = ''; aHeaderName: string = '';
      aPurpose: string = '');
    procedure CloseRtf(aFormat: TSaveFormat=fDoc);
    procedure ForceLandscape;
    procedure ForceFooter(const FooterTitle: string; forcePortrait: boolean = false);
    procedure InitPageSize;
    procedure CreateRTFBody(aSection: TSection; aTitleOffset: integer = 0;
      withNumbers: boolean = true; FirstTitleBookMark: boolean = false; isTest: boolean = false);
    procedure CreateRTFTable(const TableKind: string);
    procedure CreateRTFDetails(Level: integer; AutoFooter: boolean);
    procedure CreateDefaultDocument(const SectionName: string;
      SubSection: TSection = nil; TestSummarySheet: boolean = false;
      SaveFormat: TSaveFormat=fDoc);
    procedure CreateSectionDocument(const SectionName: string);
    procedure CreateTestDocument(const SectionName: string);
    procedure CreateSummarySheets(Tests: TSection); // manually add all Test Summary Sheets
    procedure CreateRiskAssessmentTable;
    function ExpandDocumentNames(text: string): string; // '@DI@' -> ...
      // 'DI-4.2.1' -> 'Design Input 4.2.1 (SCR#23)':
    function GetDIDisplayName(aDI: TSection): string;
    function GetPeopleFunction(const PeopleName: string; const Default: string = ''): string;
    function GetPeopleFunctionDescription(const PeopleName: string; const Default: string = ''): string;
    procedure WriteRiskDescription;
{$ifdef USEPARSER}
    procedure UpdateSADFileFromSource(RecreateAll: boolean);
    procedure CreateExternalSAE;
{$endif}
{$ifdef WITH_GRAPHVIZ}
    procedure UpdateSADGraphViz(const Ext: string; OnlyGraphs: boolean);
{$endif}
    procedure ExportAsHtml;
    procedure NeedGraphValues;
    function PercFromTitle(var GraphTitle: string; const UniqueImageName: string): integer;
    function PictureFullLine(const Line: string; out Caption: string;
      aGraphValues: TSection=nil): string;
    function PictureInlined(const ButtonPicture: string; Percent: integer;
      WriteBinary: boolean): AnsiString;
    class function GetProgramSection(const Button: string): string; // 'EIA\one.pas' -> 'SAD-EIA'
    class function GetProgramName(const Button: string): string; // 'EIA\one.pas' -> 'one.pas'
    property Data: TSectionsStorage read FData write SetData;
    property GraphValues: TSection read fGraphValues;
  end;



implementation

uses
  ShellApi,
{$ifdef WITH_GRAPHVIZ}
  Variants,
{$endif}
{$ifdef USEPARSER}
  ProjectParser,
  PasDoc_Items,
  SynZipFiles,
{$endif}
  ProjectDiff; // for TMemoryMap

{ TProject }

constructor TProject.Create(aClass: TProjectWriterClass; const aFileName: string);
begin
  WRClass := aClass;
  InitPageSize; // A4 default paper size
  if not FileExists(aFileName) then
    exit;
  // Init Data + various TSection
  Data := TSectionsStorage.Create(aFileName); // SetData() will init all TSection
end;

procedure TProject.SetData(aData: TSectionsStorage);
// Init various TSection from Data
procedure Adjust(var Doc: TDocument; const Prop: string);
var i: integer;
    Default: string;
begin
  Default := Doc.Params['Default'+Prop];
  if Default='' then exit;
  for i := 0 to length(Doc.List)-1 do
    if Doc.List[i][Prop]='' then
      Doc.List[i][Prop] := Default;
end;
var i, j, k, n, nsub: integer;
    OwnerName, OrderName, s,
    DocName, procname, Ext, SubName, procCSV: string;
    Sec, Sub, LastOwner, SadSDD: TSection;
    D: PDocument;
    P: PChar;
    MinLevel: integer;
    modif: boolean;
begin
  if Data<>nil then
    exit; // must be called only once
  // 1. init cache to common Sections
  FData := aData;
  ReferencePictures := Data.GetOrCreateSection('ReferencePictures',true);
  ReferencePictures.Clear;
  ReferenceProgram := Data.GetOrCreateSection('ReferenceProgram',true);
  ReferenceProgram.Clear;
  ReferenceImplements := Data.GetOrCreateSection('ReferenceImplements',true);
  ReferenceImplements.Clear;
  ReferenceDocuments := TList.Create;
  ReferenceIndex := Data.GetOrCreateSection('ReferenceIndex',true);
  ReferenceIndex.Clear;
  TitleBookmark := TStringList.Create;
  People := Data.GetOrCreateSection('People', true);
  Project := Data.GetOrCreateSection('Project', true);
  Pictures := Data.GetOrCreateSection('Pictures', true);
  Header.ProjectName := Project['Name'];
  OldWord2k := isTrue(Project['OldWordOpen']);
  // 2. get documents -> Document[]
  SetLength(Document,Data.Sections.Count); // max possible length
  n := 0;
  for i := 0 to Data.Sections.Count-1 do begin
    Sec := Data.Sections[i];
    if Sec.SectionName<>Sec.SectionNameValue then // name must be 'DI','SRS'..
      continue; // not a true document
    OwnerName := Sec['Owner'];
    Sec.Owner := Data[OwnerName];
    if Sec.Owner=nil then // document must have a valid Owner= param
      continue; // not a true document
    D := @Document[n];
    D.Params := Sec;
    D.Owner := Sec.Owner;
    OrderName := Sec['Order'];
    D.Order := Data[OrderName];
    SetLength(D.List,Data.Sections.Count); // max possible length
    nsub := 0;
    LastOwner := nil;
    if OrderName='' then begin // no Order= specified => unique Document
      D.List[0] := Sec;
      nsub := 1;
    end else
    if OrderName=Sec.SectionName then
      // self order (may have sub items)
      for j := 0 to Data.Sections.Count-1 do begin
        Sub := Data.Sections[j]; // extract the list directly from [Sec-*]
        if (Sub.SectionNameKind=Sec.SectionName) and (Sub<>Sec) then begin
          if D.Owner<>D.Params then begin // if not DI -> set Owner
            Sub.Owner := Data[Sub.SectionNameValue]; // SRS-DI-4.1 -> DI-4.1
            if Sub.Owner=nil then begin // SRS-MENU01->DI-4.1
              Sub.Owner := Data[Sub['Parent']];
              if Sub.Owner=nil then // no Parent
                if LastOwner=nil then // lastParent=nil -> [SRS-Service Software] -> not in List[]
                  continue else
                  Sub.Owner := LastOwner else // no Parent= -> use last DI
                LastOwner := Sub.Owner;     // Parent=DI-4.2 -> force this DI
            end else
              LastOwner := Sub.Owner;      // SRS-DI-4.8->DI-4.8
          end; // leave Sub.Owner=nil
          D.List[nsub] := Sub;
          inc(nsub);
        end;
      end else
      // not self ordered -> search by Order List[] values
      for j := 0 to n-1 do
      with Document[j] do
      if Params.SectionName=OrderName then // SRS for SDD, DI for SRS (if no subitem)
        for k := 0 to length(List)-1 do begin // get every [Sec-Order*]
          Sub := Data.Section[Sec.SectionName+'-'+List[k].SectionNameValueWithDI];
          if Sub=nil then continue;
          Sub.Owner := List[k]; // SDD-MENU-01.Owner = SRS-MENU-01
          D.List[nsub] := Sub;
          inc(nsub);
        end;
    Setlength(D.List,nsub); // adjust List[] count
    inc(n); // good document -> store it
  end;
  SetLength(Document,n); // adjust documents count
  // 3. adjust DI params
  DI := DocumentFind(Project.ReadString('MainSection','DI'));
  if DI<>nil then begin
//    raise Exception.Create('At least a [DI] section is necessary');
  DILayout := Data.GetOrCreateSection(DI.Params.SectionName+'Layout', true);
  MinLevel := maxInt;
  for i := 0 to high(DI.List) do
  with DI.List[i] do begin
    for j := 1 to length(SectionNameValue) do
      if SectionNameValue[j]='.' then
        inc(Level);
    if Level<MinLevel then
      MinLevel := Level;
    if Value['InputLevel']='' then
      Value['InputLevel'] := 'Must Have';
    if Value['Request']='' then // get Request from parent
      Value['Request'] := DI.GetPropertyFromParent(i,'Request');
  end;
  if MinLevel<>1 then // case some DI number with no '.' inside
    for i := 0 to high(DI.List) do
      inc(DI.List[i].Level,1-MinLevel); // -> force level start with 1
  // 4. adjust default parameters
  for i := 0 to high(Document) do
    Adjust(Document[i],'PreparedBy');
  // 5. special SAD-like documents:
  for i := 0 to high(Document) do
  with Document[i] do
  if Params['Source']<>'' then begin
    // [SAD]:Source=EIA,IFA2,Firmware -> [SAD-EIA],[SAD-IFA2],[SAD-Firmware]
    // [SAD-EIA] will get files from @EIA\name.pas@
    n := length(Parse);
    SetLength(Parse,n+Data.Sections.Count); // max possible length
    P := pointer(Params['Source']);
    if ParseSAD=nil then // first Source=.. document is the project main SAD
      ParseSAD := @Document[i];
    repeat
      s := GetNextItem(P); // 'EIA'
      if s<>'' then begin
        Parse[n] := Data[Params.SectionName+'-'+s];
        if Parse[n]<>nil then
          inc(n);
      end;
    until P=nil;
    SetLength(Parse,n);
    if n>0 then begin
      D := DocumentFind(Params['SourceSDD']);
      if D<>nil then begin
        ParseSDD := D; // [SDD] section to be parsed for modified files as @!EIA\bidule.pas@
        for j := 0 to high(ParseSDD.List) do begin // read body
          Data.ReadOpen(ParseSDD.List[j].SectionName,false);
          SubName := Params.SectionName+'-'+ParseSDD.List[j].SectionNameValue;
          while not Data.ReadEof do begin
            s := Data.ReadLine;
            if s='' then continue;
            repeat
              // find @something.pas@
              k := pos('@',s);
              if k=0 then
                break; // no valid @..
              if (k>1) and (NormToUpper[s[k-1]] in ['A'..'Z','0'..'9']) then begin
                delete(s,1,k); // bidule@mlds -> email adress -> ignore
                continue;
              end;
              delete(s,1,k);
              k := pos('@',s);
              if k=0 then
                break; // no valid ..@
              DocName := copy(s,1,k-1);
              delete(s,1,k);
              Ext := ExtractFileExt(DocName);
              if GetStringIndex(VALID_PROGRAM_EXT, Ext)<0 then
                continue;
              if Data.Section[DocName]<>nil then
                continue; // this is @SDD-DI-6.3.2.6.C@ -> ignore
              // valid @something.pas@ -> add to [SAD-*] Section
              modif := DocName[1]='!';
              procname := '';
              if modif then begin
                delete(DocName,1,1);
                k := pos('!',DocName);
                if k>0 then begin // '@!procname!EIA\unit.pas@
                  procname := copy(DocName,1,k-1); // get procname
                  delete(DocName,1,k);  // trim procname
                end;
              end;
              // real filename ('IFA\Main.pas'->'IFA2\Main.pas')
              DocName := GetSourceOutputFileName(DocName,true);  // mustExists=true
              if DocName='' then
                continue; // file must exists to be indexed
              // update [SAD] and [SAD-DI-4.5]: UnitsUsed=.. + UnitsModified=..
              Params.AddCSVValue('UnitsUsed',DocName,true);
              Data.AddCSVValue(SubName,'UnitsUsed',DocName,true);
              if ParseSAD.GetSectionIndex(SubName)<0 then // add [SAD-DI-4.5] to ParseSAD if necessary
                DocumentAddSection(ParseSAD,Data[SubName]);
              if modif then begin
                Params.AddCSVValue('UnitsModified',DocName,true);
                Data.AddCSVValue(SubName,'UnitsModified',DocName,true);
                if procname<>'' then begin // [SAD-DI-4.5].unit.pas=TClass.Funct,globalproc
                  DocName := ExtractFileName(DocName);
                  P := pointer(procName);
                  repeat
                    // allow adding multi procs at once: @!WriteLCD,Warning!Firmware\Main\Hardware.pas@
                    procCSV := GetNextItemTrimed(P);
                    Data.AddCSVValue(SubName,DocName,procCSV,true);
                    // unit.pasexact=.. TClass.Funct only (not TClass)
                    Data.AddCSVValue(SubName,DocName+'exact',procCSV,true);
                    k := pos('.',procCSV);
                    if k>0 then // highlight also 'TClass' in 'TClass.Funct'
                      Data.AddCSVValue(SubName,DocName,copy(procCSV,1,k-1),true);
                  until P=nil;
                end;
              end;
            until false;
          end; // while not Data.ReadEof
        end;
      end;
    end;
  end;
  // 5. special Test-like documents:
  for i := 0 to high(Document) do
  with Document[i] do // Document[i] = [Tests]
  if Params['DocByDescription']<>'' then begin
    // [Test].BodyIsTest=Yes -> Test.TestDoc=[Tests] where DocByDescription=Test
    D := DocumentFind(Params['DocByDescription']); // D = [Test]
    if (D<>nil) and isTrue(D.Params['BodyIsTest']) then begin
      D.TestDoc := @Document[i]; // .TestDoc=[Tests] where DocByDescription=Test
      if ParseTest=nil then
        ParseTest := D; // [Test], as DocByDescription=Test was called
    end;
  end;
  end; // if DI<>nil 
  // 6. Project DestinationDir init as default directory
  DestinationDir := Project['DestinationDir'];
  if DestinationDir<>'' then begin // <>'' -> DestinationDir\Name
    if not DirectoryExists(DestinationDir) then begin
      CreateDir(DestinationDir);
      if not DirectoryExists(DestinationDir) then
        DestinationDir := GetMyDocuments;
    end;
    DestinationDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(
      DestinationDir)+Project['Name']);
    if not DirectoryExists(DestinationDir) then // 'D:\Documents\Product New Version\'
      if not CreateDir(DestinationDir) then 
        DestinationDir := IncludeTrailingPathDelimiter(GetMyDocuments);
  end;
  FileName := Data.FileName; // 'D:\Dev\Synopse\Documents\Product New Version\Product New Version.pro'
  FileNameDir := ExtractFilePath(FileName);  // 'D:\Dev\Synopse\Documents\Product New Version\'
end;

destructor TProject.Destroy;
procedure OpenDocuments;
var P: PChar;
    doc: string;
begin
  P := pointer(CreatedDocuments);
  if P=nil then exit;
  if DestroyOpensCreatedDocuments then
    repeat
      doc := GetNextItem(P);
      if doc='' then break;
      if FileExists(doc) then
        ShellExecute(0,nil,pChar(doc),nil,nil,SW_SHOWMAXIMIZED);
    until false;
end;
begin
  Data.Free;
  ReferenceDocuments.Free;
  TitleBookmark.Free;
  OpenDocuments;
  inherited;
end;

procedure TProject.SaveToFile(const aFileName: string);
var W: TStringWriter;
begin
  Data.SaveText(W);
  W.SaveToFile(aFileName);
end;

procedure TProject.CreateRTF(ProjectDetails, Warning, PeopleDetails,
  RevisionDetails, AddPurpose, RiskTable, TestDetails, NoProjectDetailsLogo: boolean;
  Landscape: boolean; RevSection: TSection = nil;
  aDocumentTitle: string = ''; aRev: string = ''; aHeaderName: string = '';
  aPurpose: string = '');
procedure Details(Title,Name: string);
var P: PChar;
begin
  P := pointer(Doc.Params[Name]);
  if P=nil then exit; // PreparedBy='' -> no table to add
  WR.RtfPar;
  WR.RtfColsPercent([30,30,20,20],true,true,false,'\trkeep');
  WR.RtfRow(['\b '+Title+':',sTitle+':',sSignature+':',sDate+'\b0 ']);
  repeat
    Name := GetNextItem(P);
    if Name='' then break;
    WR.RtfRow([Name,GetPeopleFunction(Name),'','']);
  until false;
  WR.RtfColsEnd;
end;
var Name, Value, Logo, PreparedBy, Purpose, ColW, ProjMan: string;
    RevTableCount: integer;
    RevTable: array[0..3] of string;
    ValueKind: integer;
    P: PAnsiChar;
procedure AddRevisionEntry;
// RevTable[] must have been set appropriatly
begin
  inc(RevTableCount);
  if RevTableCount<>1 then begin
    if RevisionDetails then begin
      RevisionDetails := false;
      if not TestDetails then
        WR.RtfPage;
      WR.RtfPar.RtfBig(sDocumentRevisionTable);
      WR.RtfColsPercent([10,28,24,38],true,true);
      WR.RtfRow(['\b '+sRev,sDate,sAuthor,sDescription+' \b0']);
      WR.RtfRow([' '+Header.Rev,Header.RevDate,PreparedBy,
        RevSection['RevisionDescription']]);
    end;
    RevTable[0] := ' '+RevTable[0];
    WR.RtfRow(RevTable);
  end;
  RevTable[0] := ''; // mark written
end;
begin
  // 1. RTF header
  // 1.1 Init
  ReferenceTablesPos := 0;  // used to write DocumentIndex=... tables after parsing
  ReferenceDocumentsPos := 0;
  WR := WRClass.Create(Layout,11,1252,
    Project.ReadInteger('DefLang',1033),Landscape,true,isTrue(Doc.Params['TitleFlat']));
  WR.PicturePath := FileNameDir;  // 'D:\Dev\Synopse\Documents\Product New Version\'
  // 1.2 Header and Footer
  if aDocumentTitle='' then
    Header.DocumentTitle := Doc.Params['Name'] else
    Header.DocumentTitle := aDocumentTitle;
  ProjMan := Project['Manager'];
  if aHeaderName='' then begin
    Header.HeaderName := Project['Writer'];
    if Header.HeaderName='' then begin
      Header.HeaderName := ProjMan;
      Header.HeaderFunction := GetPeopleFunction(ProjMan,'Project Manager');
    end else
      Header.HeaderFunction := sWrittenBy;
    ColW := Project['HeaderColWidth'];
    P := pointer(ColW);
    if (P=nil) or not TryStrToInt(GetNextItem(P),Header.ColWidth[0]) or
      not TryStrToInt(GetNextItem(P),Header.ColWidth[1]) or
      not TryStrToInt(GetNextItem(P),Header.ColWidth[2]) or
      not TryStrToInt(GetNextItem(P),Header.ColWidth[3]) then begin
      Header.ColWidth[0] := 22;
      Header.ColWidth[1] := 48;
      Header.ColWidth[2] := 15;
      Header.ColWidth[3] := 15;
    end;
  end else begin
    Header.HeaderName := aHeaderName;
    Header.HeaderFunction := sWrittenBy;
    Header.ColWidth[0] := 22;
    Header.ColWidth[1] := 38; // smaller width for DocumentTitle
    Header.ColWidth[2] := 15;
    Header.ColWidth[3] := 25; // bigger width for HeaderName
  end;
  if RevSection=nil then
    RevSection := Doc.Params;
  if aRev='' then
    Header.Rev := RevSection['Revision'] else
    Header.Rev := aRev;
  Header.RevDate := RevSection.RevisionDate;
  HeaderAndFooter;
  // 1.3 Document Properties
  PreparedBy := ValAt(Doc.Params['PreparedBy'],0);
   if aPurpose='' then
    Purpose := Doc.Params['Purpose'] else
    Purpose := aPurpose;
  WR.SetInfo(Header.DocumentTitle+' '+Header.Rev,PreparedBy,
     Header.ProjectName+': '+Purpose,
     Project.ReadString('Manager',Header.HeaderName),Project['Company']);
  // 1.4 End rtf header
  WR.InitClose;
  // 2. First page
  if ProjectDetails then begin
    Logo := Project['Logo'];
    if (Logo<>'') and (Pictures[Logo]<>'') and not NoProjectDetailsLogo then begin
      Logo := Logo+' '+ValAt(Pictures[Logo],0);
      WR.RtfImage(Logo,'',true,'\ql').AddRtfContent('\line'#13);
    end;
    WR.AddRtfContent('{\b\cf9 '+sProjectDocumentation);
    if not Header.Visible then // no header -> write Company name in ProjectDetails
      WR.AddRtfContent(' - '+Project['Company']);
    WR.AddRtfContent('\par}');
    WR.RtfColsPercent([30,70],true,true);
    WR.RtfRow([sProjectName+':','{\b '+Header.ProjectName+'}']);
    WR.RtfRow([sDocumentName+':','{\b '+Header.DocumentTitle+'}']);
    WR.RtfRow([sDocumentRevision+':','{\b '+Header.Rev+'}']);
    WR.RtfRow([sDate+':','{\b '+Header.RevDate+'}']);
    if ProjMan='' then
      WR.RtfRow([Header.HeaderFunction+':','{\b '+Header.HeaderName+'}'],true) else
      WR.RtfRow([GetPeopleFunction(ProjMan,'Project Manager')+':','{\b '+ProjMan+'}'],true);
    WR.RtfPar;
  end;
  if TestDetails then begin
    WR.AddRtfContent('{\b\cf9 '+sTestProcedure+'\par}');
    WR.RtfColsPercent([30,70],true,true);
    WR.RtfRow([sProjectName+':','{\b '+Header.ProjectName+'}']);
    WR.RtfRow([sProtocolName+':','{\b '+Header.DocumentTitle+'}']);
    WR.RtfRow([sProtocolDescription+':','{\b '+TrimLastPeriod(Purpose)+'}']);
    WR.RtfRow([sProtocolRevision+':','{\b '+Header.Rev+'}']);
    WR.RtfRow([sProtocolDate+':','{\b '+Header.RevDate+'}']);
    WR.RtfRow([sPreparedBy+':','{\b '+PreparedBy+'}'],true);
    WR.RtfPar;
    WR.RtfColsPercent([30,70],true,true);
    WR.RtfRow([sTestedBy+':','']);
    WR.RtfRow([sTestDate+':',''],true);
  end;
  if Warning then
    CreateRTFBody(Project); // get WARNING message from [Project] body
  if PeopleDetails then begin
    Details(sPreparedBy,'PreparedBy');
    Details(sReviewedBy,'ReviewedBy');
    Details(sApprovedBy,'ApprovedBy');
  end;
  if RevisionDetails then begin
    RevTableCount := 0;
    Data.ReadOpen(RevSection.SectionName,true);
    while Data.ReadNextNameValue(Name,Value) do begin
      ValueKind := GetStringIndex(
        ['Revision','RevisionDate','PreparedBy','RevisionDescription'],Name);
      if ValueKind<0 then continue;
      if (ValueKind=0) and (RevTable[0]<>'') then // Revision=... ?
        AddRevisionEntry; // mark previous revision if any
      RevTable[ValueKind] := Value;
    end;
    if RevTable[0]<>'' then
      AddRevisionEntry; // will only make the table revision if more than one revision
    WR.RtfColsEnd;
  end else // force RtfPage if something written
    RevisionDetails := ProjectDetails or Warning or PeopleDetails or RevisionDetails;
  if AddPurpose then begin
    if Purpose<>'' then begin
      if RevisionDetails then begin
        RevisionDetails := false; // new page now if not already done
        if not TestDetails then
          WR.RtfPage;
      end;
      WR.RtfPar.RtfBig(sDocumentPurpose).
        AddRtfContent(sDocumentPurposeSentence,[Header.DocumentTitle,
          TrimLastPeriod(Purpose,true),Header.ProjectName,Header.Rev]).
          AddRtfContent(#13);
    end;
    WR.RtfText; // close any pending {
    ReferenceDocumentsPos := WR.len;
    if RiskTable then
      WriteRiskDescription;
  end;
  if RevisionDetails and not TestDetails and AddPurpose then
    WR.RtfPage; // change page if not already done
  // 3. set FileName
  if aDocumentTitle='' then begin
    aDocumentTitle := Doc.Params.DocName;
    WR.FileName := Project.ReadString('DocName',Header.ProjectName)+' '+
      aDocumentTitle+' '+Header.Rev+'.rtf';
  end else
    WR.FileName := Header.DocumentTitle+'.rtf';
  if DestinationDir<>'' then  // <>'' -> 'D:\Documents\Product New Version\'
    WR.FileName := DestinationDir+WR.FileName;
end;

procedure TProject.WriteRiskDescription;
var i: integer;
    HighMedLow: string;
    P: PChar;
begin
  WR.RtfPar.RtfBig(sRiskAssessmentScale);
  HighMedLow := sHighMedLow;
  for i := 0 to 2 do
  with RiskDefs[i] do begin
    WR.AddRtfContent('{\b\cf9 '+Name+'}: '+Description+'\par');
    WR.RtfColsPercent([15,85],true,true,false,'\trkeep');
    P := pointer(HighMedLow);
    WR.RtfRow(['\qc 3 - '+GetNextItem(P),'\qj '+Level[3]]);
    WR.RtfRow(['\qc 2 - '+GetNextItem(P),'\qj '+Level[2]]);
    WR.RtfRow(['\qc 1 - '+GetNextItem(P),'\qj '+Level[1]],true);
  end;
end;

procedure TProject.CreateRTFBody(aSection: TSection;
  aTitleOffset: integer = 0; withNumbers: boolean = true; FirstTitleBookMark: boolean = false;
  isTest: boolean = false);
(* special lines begin with
     :  for titles (no Name=Value pairs after a :title)
     -  for a list item
     !  for pascal source
     !! for modified pascal source line
     &  for c c++ source
     &! for modified c c++ source line
     #  for c# source
     #! for modified c# source line
     $  for text file (fixed-width font)
     $! for modified text file line (fixed-width font)
     %filename.jpg [640x480 85%] for images jpg/bmp/png/emf - see [Pictures]
     |=-%30%40%30   then  |Col 1|Col 2|Col 3   then   |%  for columns
        (=:no indent -:no border)
     =[SectionName] to inline the [SectionName] content at this place

 text can be formated as rtf (with \b \i { } e.g.) - each new text paragraph
   will be ended with \par
 {} can be used for a \par alone (void lines are just ignored)

 you can link to another item with @SectionName@ (@DI-4.1@ e.g) or @DocName@ (@SRS@)

 in the [SDD-*] sections, specify @Module\filename.pas@ for each file name,
   @!Module\filename.pas@ for filename modified or
   @!procedurename,classname,class.methodname!Module\filename.pas@ in order to
     specify the procedure name.
   The corresponding units (and procedures) will be highlited in the [SAD] document.

 some special lines commands can be entered:
  \page        to force a new page
  \landscape   to change the page orientation to landscape
  \portrait    to change the page orientation to portrait
  \footer blabla  to change the footer text
  \Layout      to add a list with all DILayout titles
  \LayoutPage  idem + associated pages in the document
  \Refers      to add a list with all Refers= titles
  \RefersPage  idem + associated pages in the document
  \risk        to add the Risk Assessment Scale table
  \Source      (for [SAD] section) to add the list of the Source=.. modules
  \SourcePage  idem + associated pages in the document
  \graph UniqueImageName [Title] then following lines either .dot normal text,
    either "\From Text\To Text[\Label between both]"
  \include filename.ext    ext will be used to append !&#$ left
  =[SectionName]   to include this section content at the current place
  \Implements ISO 5.1 [blabla][\DocumentName] will be processed by \TableImplements=ISO
  \TableSoftwareChanges or \TableTraceabilityMatrix for SCRS
  \TableNewFeatures or \TableBugFixes or \TableTests[=October 16, 2008] for Release Notes
  \TableDI=6.3.1.2,6.3.1.3  for a table with all the supplied Design Inputs
  \TableDocuments[=DI,SRS,SDD,SAD] for a table with the supplied document details

 in the [Test-*] sections, special auto-defined columns can be used with
   |Actions[|Expected Results]
   manual tables can be used as usual (with |%..)
*)
var s, line, caption, UniqueImageName, BookMarkName, IEC, BookMarks: string;
    i, j, BookMark: integer;
    withPage: boolean;
    ColType: (noCol, testCol, manualCol);
    TestNumber: integer;
    Sec: TSection;
    RefDoc: PDocument;
    PC: PChar;
procedure WRtestCol;
var P: PChar;
    n, i: integer;
    Colss: TStringDynArray;
begin
   SetLength(Colss,20);
   Colss[0] := '\qc '+IntToStr(TestNumber);
   inc(TestNumber);
   n := 1;
   P := pointer(line);
   inc(P);
   repeat
     ProjectCommons.AddString(Colss,n,GetNextItem(P,'|'));
   until P=nil;
   Colss[1] := '\ql '+Colss[1];
   n := WR.ColsCount;
   SetLength(Colss,n); // force good columns number
   dec(n);
   if (Colss[n]='') and (Colss[2]<>'') then
     Colss[n] := '\qc '+sPassFail; // Validation only if Expected results
   WR.RtfColVertAlign(1,taLeftJustify); // description text -> vert align top
   for i := 2 to n do
     WR.RtfColVertAlign(i,taRightJustify); // Pass/Fail -> vert align bottom
   WR.RtfRow(Colss);
end;
procedure WRtestEnd;
begin
  colType := noCol;
  WR.RtfColsEnd; // close any pending test table
end;
begin
  if (aSection=nil) or (WR=nil) or not aSection.HasBody then exit;
  WR.ListLine := true;
  TestNumber := 1;
  ColType := noCol;
  aTitleOffset := aSection.ReadInteger('TitleOffset',aTitleOffset);
  Data.ReadOpen(aSection.SectionName,false);
  while not Data.ReadEof do begin
    line := Data.ReadLine;
    if line='' then
      continue;
    // special [Test] | behavior
    if isTest then
      if line[1]='|' then begin
        if pos('@',line)<>0 then
          line := ExpandDocumentNames(line);
        case ColType of
        noCol: begin
          if line[2]='%' then begin // force manual table
            WR.RtfColLine(copy(line,2,maxInt));
            ColType := manualCol;
          end else begin
            ColType := testCol;
            WR.RtfColsPercent([5,33,33,16,13],true,true,false,'\trhdr');
            WR.RtfRow(['\qc\b #'+WR.RtfFontString(90),
              sActions,sExpectedResults,sObservation,sValidation+'\b0'],true);
            WR.RtfColsPercent([5,33,33,16,13],true,true,false,'\trkeep');
            WR.RtfFont(90);
            WRtestCol; // force good columns number
          end;
        end;
        testCol:
          if line[2]='%' then begin // force manual table
            WR.RtfColsEnd; // close any pending test table
            WR.RtfPar;
            WR.RtfColLine(copy(line,2,maxInt));
            ColType := manualCol;
          end else
          if line='|' then begin // | alone on a line ->
            WR.RtfColsEnd; // close any pending test table
            WR.RtfPar;
            ColType := noCol;
          end else
            WRtestCol; // force good columns number
        manualCol: begin
          if line='|%' then begin // end of manual col
            colType := noCol;
            WR.RtfColsEnd; // close any pending test table
          end;
          WR.RtfColLine(copy(line,2,maxInt));
        end;
        end;
        continue;
      end else
      if ColType=testCol then begin // normal line -> end of manual col
        colType := noCol;
        WR.RtfColsEnd; // close any pending test table
      end;
    // normal line
    case line[1] of
    ':': begin
      i := 2;
      BookMark := 0; // ':12 Title' -> Bookmark number 12 -> @12@ will jump to it
      while line[i] in ['0'..'9'] do begin
        BookMark := (BookMark*10)+ord(line[i])-48;
        inc(i);
      end;
      if FirstTitleBookMark then begin
        WR.RtfTitle(copy(line,i,maxInt),aTitleOffset,withNumbers,aSection.SectionNameValue);
        FirstTitleBookMark := false;
      end else begin
        if BookMark<>0 then
          BookMarkName := 'TITL_'+IntToStr(BookMark) else
          BookMarkName := '';
        TitleBookmark.AddObject(trim(copy(line,i,maxInt)),pointer(BookMark));
        WR.RtfTitle(copy(line,i,maxInt),aTitleOffset,withNumbers,BookMarkName);
      end;
    end;
    '!','&','#','µ','$':
      WR.RtfCode(line);
    '-': begin                   
      if pos('@',line)<>0 then
        line := ExpandDocumentNames(line);
      WR.RtfList(line);
    end;
    '%': begin
      if line[2]='%' then begin // '%%FirmwareBoot' for \graph FirmwareBoot ...
        NeedGraphValues;
        delete(line,1,2);
        WR.RtfFont(50).RtfImage(PictureFullLine(
          GraphDirName+line,caption,fGraphValues),caption).RtfFont(100);
      end else
        WR.RtfFont(50).RtfImage(PictureFullLine(line,caption),caption).RtfFont(100);
    end;
    '|': begin
      if pos('@',line)<>0 then
        line := ExpandDocumentNames(line);
      WR.RtfColLine(copy(line,2,maxInt));
    end;
    else
    if IdemPChar(pointer(line),'\GRAPH') then begin
      { - first line has format: \Graph UniqueImageName [Title]
        - then following lines are expected to be (until void line):
           either a .dot normal line
             (e.g. "rankdir=LR; node [shape = circle];")
           either "\From Text\To Text[\Label between both]" }
      NeedGraphValues;
      PC := @line[7]; while PC^=' ' do inc(PC);
      UniqueImageName := GetNextItemTrimed(PC,' ');
      if UniqueImageName<>'' then
        WR.RtfFont(50).RtfImage(PictureFullLine(
          GraphDirName+UniqueImageName,caption,fGraphValues),caption).RtfFont(100);
      while not Data.ReadEof do begin
        line := Data.ReadLine;
        if (line='') or (line='\') then break; // ignore \graph content
      end;
    end else
    if IdemPChar(pointer(line),'\SOURCE') then begin
      withpage := IdemPChar(@line[8],'PAGE');
      for i := 0 to high(Parse) do
      with Parse[i] do
        if SameText(SectionNameKind,aSection.SectionNameKind) then begin
          line := '- '+DisplayName(nil);
          if withpage then
            WR.RtfList(RtfLinkTo(SectionNameValue,
              Line+' '+format(sPageN,[RtfPageRefTo(SectionNameValue,false)]))) else
            WR.RtfList(line);
        end;
    end else
    if IdemPChar(pointer(line),'\LAYOUT') then begin
      withpage := IdemPChar(@line[8],'PAGE');
      if DILayout<>nil then
      for i := 0 to DILayout.Lines.Count-1 do begin
        line := DILayout.Lines[i];
        if (line<>'') and (line[1]=':') then begin
          line := trim(copy(line,2,maxInt));
          if withpage then
            WR.RtfList(RtfLinkTo('Layout_'+line,'- '+Line+
              ' '+format(sPageN,[RtfPageRefTo('Layout_'+line,false)]))) else
            WR.RtfList('- '+line);
        end;
      end;
    end else
    if IdemPChar(pointer(line),'\REFERS') then begin
      // list of referal to other documents (e.g. RK)
      withpage := IdemPChar(@line[8],'PAGE');
      PC := pointer(Doc.Params['Refers']);
      while PC<>nil do begin
        line := GetNextItem(PC);
        RefDoc := DocumentFind(line);
        if RefDoc=nil then continue;
        caption := RefDoc.Params.DisplayName(nil);
        if withpage then
          WR.RtfList(RtfLinkTo('Layout_'+line,'- '+caption+
            ' '+format(sPageN,[RtfPageRefTo('Layout_'+line,false)]))) else
          WR.RtfList('- '+caption);
      end;
    end else
    if IdemPChar(pointer(line),'\INDEX ') then begin
      // '\Index KeyWord 1,KeyWord 2' to be processed by DocumentIndex=...,Index
      // just as @*keyword@ inlined in text
      PC := @line[8];
      while PC^=' ' do inc(PC);
      if PC^=#0 then
        continue;
      repeat
        s := GetNextItemTrimed(PC);
        if s<>'' then begin
          // KeyWord 1=bookmark0,bookmark1...
          inc(ReferenceIndexIndex);
          BookMarkName := WR.RtfBookMark('','NDX_'+IntToStr(ReferenceIndexIndex));
          ReferenceIndex.AddCSVValue(s,BookMarkName);
        end;
      until PC=nil;
    end else
    if IdemPChar(pointer(line),'\IMPLEMENTS ') then begin
      // '\Implements ISO 5.1 [blabla bloblo][\DocumentName]'
      // (will also be processed by '\TableImplements=ISO')
      if PosEx('\',line,12)>0 then
        continue; // don't index \DocumentName
      // ReferenceImplements[] := 'IEC 5.7 blabla=Bookmark0,Bookmark1...'
      Delete(line,1,12);
      j := Pos(' ',line);
      if j>0 then
        j := PosEx(' ',line,j+1) else
        continue; // need at least 'IEC 5.7'
      if j=0 then begin
        line := line+' ';
        j := length(line); // 'IEC 5.7 '
      end;
      caption := UpperCase(copy(line,1,j));
      inc(ReferenceImplementsIndex);
      BookMarkName := WR.RtfBookMark('','IMPLEM_'+IntToStr(ReferenceImplementsIndex));
      s := '';
      with ReferenceImplements.Lines do
        for i := 0 to Count-1 do
          if IdemPChar(Pointer(Strings[i]),Pointer(caption)) then begin // 'IEC 5.7 ' found
            s := Strings[i];
            TSection.SplitNameValue(s,IEC,BookMarks);
            if length(IEC)=j then // no description yet -> put line
              s := line+copy(Strings[i],j+1,maxInt);
            Strings[i] := s+','+BookMarkName;
            break;
          end;
      if s='' then
        ReferenceImplements.Lines.Add(line+'='+BookMarkName);
    end else
    if IdemPChar(pointer(line),'\RISK') then
      WriteRiskDescription else
    if IdemPChar(pointer(line),'\PAGE') then
      WR.RtfPage else
    if IdemPChar(pointer(line),'\LANDSCAPE') then
      ForceLandscape else
    if IdemPChar(pointer(line),'\PORTRAIT') then
      ForceFooter(Header.LastFooterTitle,true) else
    if IdemPChar(pointer(line),'\FOOTER') then
      ForceFooter(trim(copy(line,8,maxInt))) else
    if IdemPChar(pointer(line),'\TABLE') then
      CreateRTFTable(copy(line,7,maxint)) else  // \TableDocuments e.g.
    if IdemPChar(pointer(line),'\INCLUDE ') then begin
      Delete(line,1,9);
      if not FileExists(line) then
        line := FileNameDir+line;
      if FileExists(line) then
        with TStringList.Create do
        try
          WR.RtfText;
          LoadFromFile(line);
          line := ExtractFileExt(line);
          if IdemPChar(pointer(line),'.PRO')
             {or IdemPChar(pointer(line),'.TXT')} then begin
            for i := 0 to Count-1 do
              WR.AddRtfContent(Strings[i]); // include as a .pro = rtf content
          end else begin
            line := VALID_PROGRAM_CHAR[
              GetStringIndex(VALID_PROGRAM_EXT,line)]; // !&#µ$
            for i := 0 to Count-1 do
              WR.RtfCode(line+Strings[i]); // include as a fixed text
          end;
        finally
          Free;
        end;
    end else
    if PWord(pointer(line))^=ord('=')+ord('[')shl 8 then begin // =[SoftwareHistory]
      delete(line,1,2);
      line := trim(line);
      if line<>'' then begin
        if line[length(line)]=']' then
          SetLength(line,length(line)-1);
        Sec := Data[line];
        if Sec<>nil then begin
          Data.ReadPush;
          CreateRTFBody(Sec,WR.TitleLevelCurrent);
          Data.ReadPop;
        end;
      end;
    end else begin
      // normal line = text paragrah, rtf-like formated
      WR.RtfText; // set Last -> update any pending {}
      if pos('@',line)<>0 then
        WR.AddRtfContent(ExpandDocumentNames(line)) else
        WR.AddRtfContent(line);
      WR.RtfPar;
    end;
    end;
  end;
  if ColType=testCol then
    WR.RtfColsEnd; // close any pending test table
  WR.RtfText; // close any pending list
end;

procedure TProject.CreateDefaultDocument(const SectionName: string;
  SubSection: TSection = nil; TestSummarySheet: boolean = false; SaveFormat: TSaveFormat=fDoc);
var Ok: array[0..high(FRONTPAGE_OPTIONS)] of boolean;
    layout, title, desc, testdocname, Requirement,
    HeaderName, Purpose, WriteSummaryOf, item, par: string;
{$ifdef USEPARSER}
    SAD: TProjectBrowser; // contains all [SAD-Parse].SourceFile= details
{$endif}
    i, j, d, n, ParseTitleOffset: integer;
    Source, DIDetails, Sec, RevSection: TSection;
    Test, TestDoc: PDocument; // [Test]
    Tests: array of TSection;
    Required: array of boolean; // [SAD-MENU-01].Source=Firmware -> Required[ParseFirmwareIndex] := true
procedure RequiredSetOne(TestSection: TSection);
function RequiredGetSource(TestSection: TSection): PChar;
var Sec: TSection;
begin
  result := pointer(TestSection['Source']);
  if result<>nil then exit;
  Sec := ParseSAD.GetSection(ParseSAD.Params.SectionName+'-'+TestSection.SectionNameValue);
  if Sec<>nil then
    result := pointer(Sec['Source']); // [SAD-MENU-01].Source=Firmware
end;
// [SAD-MENU-01].Source=Firmware -> Required[ParseFirmwareIndex] := true
var P: PChar;
    s: string;
    j: integer;
begin
  P := RequiredGetSource(TestSection);
  if P<>nil then
  repeat
    s := GetNextItem(P); // 'Firmware'
    if s<>'' then
      for j := 0 to high(Required) do
        if SameText(Parse[j].SectionNameValue,s) then
          Required[j] := true;
  until P=nil;
end;
procedure RequiredSet(TestSection: TSection = nil);
var i: integer;
begin
  if Length(Required)=0 then
    SetLength(Required,length(Parse)); // Parse=[SAD-Firmware],[SAD-EIA],[SAD-IFA2]..
  fillchar(Required[0],length(Required),0); // all Required[] := false
  if TestSection<>nil then
    RequiredSetOne(TestSection) else
    for i := 0 to n do // normally should be set only for the first, but how knows?
      RequiredSetOne(Tests[i]);
end;
procedure RequiredWrite;
var i: integer;
    Requirement: string;
begin
  WR.AddRtfContent(sProcedureRequires);
  WR.RtfPar;
  WR.RtfColsPercent([80,20],false,true,true,'\trkeep');
  WR.RtfRow(['\b\ql '+sRequirement,'\qc '+sVersion+'\b0']);
  if (length(Parse)=0) and (n>=0) then begin
    Requirement := Tests[0].Value['Requirement'];
    if Requirement<>'' then
      WR.RtfRow(['\ql '+ValAt(Requirement,0),'\qc '+ValAt(Requirement,1)]);
  end else
  for i := 0 to high(Required) do
    if Required[i] then
      with Parse[i] do
        WR.RtfRow(['\ql  '+ReadString('Requirements',Value['DisplayName']),
          '\qc '+Value['Version']]);
  WR.RtfColsEnd;
end;
var P: PChar;
function GetNextRisk: string;
begin
  result := GetNextItem(P);
  if result='0' then
    result := '';
end;
begin
  Doc := DocumentFind(SectionName);
  if Doc=nil then exit;
  TestDoc := DocumentFind(Doc.Params['DocByDescription']);
  if (TestDoc<>nil) and (SubSection=nil) then
    exit; // don't generate [Tests] as one document, just as SubSection
  RevSection := nil;
  fillchar(Ok,sizeof(OK),0);
  OK[8] := IsTrue(Project[FRONTPAGE_OPTIONS[8]]);   // global NoConfidential
  OK[9] := IsTrue(Project[FRONTPAGE_OPTIONS[9]]);   // global NoHeaderBorder
  OK[10] := IsTrue(Project[FRONTPAGE_OPTIONS[10]]); // global HeaderWithLogo
  OK[11] := IsTrue(Project[FRONTPAGE_OPTIONS[11]]); // global FullTitleInTableOfContent
  OK[12] := IsTrue(Project[FRONTPAGE_OPTIONS[12]]); // global NoProjectDetailsLogo
  if SubSection=nil then begin // full document
    ForcedOnlySection := nil;
    SetStringIndexCSV(Doc.Params['DocumentFrontPage'],FRONTPAGE_OPTIONS,Ok);
    title := '';
    HeaderName := '';
    Purpose := '';
  end else begin // section document
    ForcedOnlySection := SubSection;
    SetStringIndexCSV(Doc.Params['SubDocFrontPage'],FRONTPAGE_OPTIONS,Ok);
    if TestDoc=nil then begin
      title := SubSection.DisplayName(Doc.Params);
      HeaderName := SubSection.PreparedBy;
    end else begin
      TestGetDescr(SubSection,nil,TestDoc,testdocname,title);
      title := TestDoc.Params.DocName+' '+title;
      if SubSection['Revision']<>'' then
        RevSection := SubSection else
        RevSection := TestDoc.Params;
    end;
    Purpose := SubSection.Description;
    if Purpose<>'' then
      Purpose[1] := NormToUpper[Purpose[1]];
  end;
  Header.visible := not OK[6];
  Header.confidential := not OK[8];
  Header.withborder :=not OK[9];
  Header.withlogo := OK[10];
  if TestSummarySheet then begin
    Header.DocumentTitle := title;
    Header.Rev := RevSection['Revision'];
  end else begin
    CreateRTF(OK[0],OK[1],OK[2],OK[3],OK[4],OK[5],OK[7],OK[12],false,RevSection,
      title,'',HeaderName,Purpose);
    WR.FullTitleInTableOfContent := OK[11];
  end;
  // Test protocol document
  if TestDoc<>nil then begin // TestDoc=[Test] Doc=[Tests] SubSection=[Test-DI-4.4]
    // fill Tests[] with all Tests in this document
    SetLength(Tests,length(TestDoc.List));
    n := 0;
    i := TestDoc.GetSectionIndex(SubSection);
    Sec := SubSection;
    if i>=0 then
    repeat
      Tests[n] := Sec;
      inc(n);
      inc(i);
      if i=length(TestDoc.List) then break;
      Sec := TestDoc.List[i];
    until (Sec['Description']<>'') or isTrue(Sec['ExcludeFromSummary']);
    dec(n);
    if TestSummarySheet then
      RequiredSet else begin
      // set all Test protocols Required[]
      WR.FileName := DestinationDir+ChangeFileExt(testdocname,'.rtf');
      // write Test features table
      if n>1 then
        WR.RtfPage else
        WR.RtfPar;
      WR.RTFBig(sTestFeatures);
      WR.RtfColsPercent([30,70],false,true,true,'\trkeep');
      WR.RtfRow(['\b\qc '+TestDoc.Owner.ItemName+' '+sReference,'\ql '+sDescription+'\b0']);
      for i := 0 to n do
      with Tests[i] do
        WR.RtfRow(['\qc{\i '+RtfLinkTo(SectionNameValue,DisplayName(TestDoc.Owner))+'}',
          '\ql '+TrimLastPeriod(Owner.ShortDescription(''))]);
      WR.RtfColsEnd;
      // write Special Requirements table
      WR.RtfLine;
      WR.RTFBig(sSpecialRequirements);
      RequiredSet; // set all Test protocols Required[]
      RequiredWrite;
      WR.RtfPage;
      // write all Test protocols
      WR.RTFTitle(sProcedureSteps,1,false);
      for i := 0 to n do begin
        Sec := Tests[i];
        WR.RTFTitle(Sec.DisplayName(TestDoc.Owner),2,false,Sec.SectionNameValue);
        if (Sec.Root.SectionName=Sec.SectionNameValue) then // if not Sub-Item: write DI
          WR.AddRtfContent(sDescriptionRtfN,[TestDoc.Params.Root.DisplayName(nil)]).
            AddRtfContent(': ').
            AddWithoutPeriod(Sec.Root.Description).AddRtfContent('.\par'#13);
        desc := Sec.Owner['Description'];
        if (desc<>'') then begin
          if desc<>Sec.Root.Description then
            WR.AddRtfContent(sDescriptionRtfN,[TestDoc.Owner.ItemName]).
              AddRtfContent(': ').
              AddWithoutPeriod(desc).AddRtfContent('.\par'#13);
        end else begin
          WR.AddRtfContent(sDescriptionRtfN,[TestDoc.Owner.ItemName]).
            AddRtfContent(': ');
          WR.RtfPar;
          CreateRTFBody(Sec.Owner,3,false,false,false);
        end;
        WR.AddRtfContent('{\i '+sActions+'}:');
        WR.RtfPar;
        if Sec['SameAs']<>'' then
          Sec := Data[Sec['SameAs']]; // SameAs=.. -> get test from identical
        CreateRTFBody(Sec,3,false,false,true);
      end;
      WR.RtfTitle(sProcedureCriteria,1,false);
      WR.AddRtfContent(sProcedureCriteriaText).AddRtfContent('.\par'#13);
    end;
    // write Summary Sheet
    WR.RtfEndSection;
    WR.RtfHeaderBegin(0);
    WR.RtfParDefault.RtfFont(130).AddRtfContent('\qc\b ').
      AddRtfContent(Header.ProjectName).RtfPar.AddRtfContent(Header.DocumentTitle).
      AddRtfContent('\line{').RtfFont(100).AddRtfContent('\i Rev. ').
      AddRtfContent(Header.Rev).AddRtfContent('}').
      RtfPar.RtfFont(130).AddRtfContent(UpperCase(sSummarySheet)).
      AddRtfContent('\par\par').RtfHeaderEnd;
    WR.RtfFooterBegin(0);
    WR.RtfFooterEnd;
    WR.RtfParDefault.RTFBig('{\ul '+AnsiUpperCase(sTestFeatures)+'}').
      AddRtfContent(Purpose).RtfPar.RtfFont(110); // all page = bigger text
    if length(Required)=0 then begin
      Requirement := Tests[0].Value['Requirement'];
      if Requirement<>'' then
        WR.AddRtfContent('\ql{\ul\b ').AddRtfContent(ValAt(Requirement,0)).
          AddRtfContent(' ').AddRtfContent(sVersion).AddRtfContent(' :}{\f1  ________  }{').
          RtfFont(100).AddRtfContent('(').AddRtfContent(sExpected).
          AddRtfContent(': ').AddRtfContent(ValAt(Requirement,1)).
          AddRtfContent(')}\par'#13);
    end else
    for i := 0 to high(Required) do
      if Required[i] then begin
        WR.AddRtfContent('\ql{\ul\b ').AddRtfContent(Parse[i]['DisplayName']).
        AddRtfContent(' ').AddRtfContent(sVersion).
        AddRtfContent(' :}{\f1  ________  }{').RtfFont(100).
        AddRtfContent('(').AddRtfContent(sExpected).AddRtfContent(': ').
        AddRtfContent(Parse[i]['Version']).AddRtfContent(')}\par'#13);
      end;
    if OldWord2k then
      WR.AddRtfContent('\ql'#13#10);
    CreateRTFBody(Doc.Params,3,false); // [Tests] body = Summary Sheet text
    if TestSummarySheet then
      exit;
    CloseRTF(SaveFormat);
    exit; // special document format
  end;
  if isTrue(Doc.Params['Landscape']) then
    ForceLandscape else // contain a page jump inside the landscape setting
    if OK[0] or OK[1] or OK[2] or OK[3] or OK[4] or OK[6] then
      WR.RtfPage;
{$ifdef USEPARSER}
  SAD := nil;
{$endif}
  if ReferenceTablesPos=0 then begin
    // used to write [TOC and] DocumentIndex=... tables after parsing
    ReferenceTablesPos := WR.len;
    ReferenceTablesTitleLevel := WR.TitleLevel;
  end;
  if ForcedOnlySection=nil then begin // multiple section document:
    WriteSummaryOf := Doc.Params['WriteSummaryOf'];
    if (Doc=DI) or (WriteSummaryOf<>'') or (Doc.Params.Root<>DI.Params) then
      WR.TitleLevel[0] := 0; // first paragraph must be numeroted with 1.
    // 0. init W.TitlesList[] for TableOfContent if necessary
    if isTrue(Doc.Params['WriteTableOfContent']) then
      WR.TitlesList := TStringList.Create;
    // 1. always write body of the main section (as Introduction)
    CreateRTFBody(Doc.Params);
{$ifdef USEPARSER}
    // 2. deal with SAD-like documents (two parts)
    if Doc.Params['Source']<>'' then begin
      // use [Project] Name=.. since can be use with Data.FileName='truc.~tmp'
      // 2.1. first part: follow \SOURCE order
      // 2.1.1. Introduction
      if ReferenceTablesPos=0 then begin
        ReferenceTablesPos := WR.RtfText.len;  // used to write DocumentIndex=... tables after parsing
        ReferenceTablesTitleLevel := WR.TitleLevel;
      end;
      ForceFooter(format('%s - %s',[Doc.Params.ItemName,sGlobalArchitecture]));
      Source := Data[Doc.Params.SectionName+'-Source'];
      if Source.ReadInteger('TitleOffset',1)>0 then
        WR.RtfTitle(sGlobalArchitecture);
      CreateRTFBody(Source,1,true,true);
      // 2.1.2. get body+units from \SOURCE sections ([SAD-LIS],[SAD-EIA]...)
      SAD := TProjectBrowser.Create(self);
      for i := 0 to high(Parse) do
      with Parse[i] do // 'SAD-LIS'
        if SameText(SectionNameKind,Doc.Params.SectionName) then begin
          // write general description text (from [SAD-LIS] body)
          title := DisplayName(Doc.Params);
          ForceFooter(format('%s - %s',[Doc.Params.ItemName,title]));
          ParseTitleOffset := Parse[i].ReadInteger('TitleOffset',1);
          if ParseTitleOffset>0 then
            WR.RtfTitle(title,1,true,SectionNameValue);
          CreateRTFBody(Parse[i],2);
          // auto create units descriptions from source files
          SAD.FillUnits(Parse[i],false);
          // write unit table
          if SAD.Units.Count>0 then begin
            WR.RtfTitle(title+' '+sSourceCodeImplementation,ParseTitleOffset);
            WR.RtfTitle(format(sUsedUnitsN,[title]),ParseTitleOffset+1);
            WR.AddRtfContent(sUsedUnitsTextN,[title]);
            SAD.RtfUsesUnits(WR);
            // write all units descriptions
            if isTrue(ParseSAD.Params['WithAllfields']) then
              par := '*' else
              par := ParseSAD.Params['UnitsUsed'];
            SAD.RtfUsesUnitsDescription(WR,ParseTitleOffset+1,par,Footer);
          end;
        end;
      SAD.Free;
      // 2.2. second part: follow \LAYOUT order
      title := format(sImplicationsN,[Doc.Owner.ItemName]);
      ForceFooter(format('%s - %s',[Doc.Params.ItemName,title]));
      WR.RtfTitle(title);
    end;
{$endif}
    // 3. deal with VV-like documents (just write list of [WriteSummaryOf] documents)
    if WriteSummaryOf<>'' then begin
      Test := DocumentFind(WriteSummaryOf); // [VV].WriteSummaryOf=Test, e.g.
      if Test<>nil then begin
        layout := '';
        for i := 0 to DILayout.Lines.Count-1 do begin // always follow the DILayout
          title := DILayout.Lines[i];
          if (title='') or (title[1]=';') then continue else
          if title[1]=':' then begin // 'Service Software' = big section
            layout := trim(copy(title,2,maxInt));
            WR.RtfTitle(layout,1,true,'Layout_'+layout);
          end else begin // '4.2.1' in [DILayout]
            d := DI.GetSectionNameValueIndex(title);
            if d<0 then continue;
            DIDetails := DI.List[d];
            item := '';
            for j := 0 to high(Test.List) do
            with Test.List[j] do begin
              if (Root<>DIDetails) or isTrue(Value['ExcludeFromSummary']) then continue;
              desc := Value['Description'];
              if desc<>'' then begin // Description= for every new document
                if item<>'' then // write remaining item
                  WR.AddRtfContent('}:\line'#13+item+'.\par'#13);
                item := '';
                TestGetDescr(Test.List[j],DIDetails,Test,testdocname,title);
                WR.RtfTitle(title,2,true,SectionNameValue);
                if OldWord2k then
                  WR.AddRtfContent('\ql'#13#10);
                WR.AddRtfContent('{\b ').
                  AddRtfContent(sOverViewN,[Test.Params.ItemName]).
                  AddRtfContent(' }:\line ').AddWithoutPeriod(desc).
                  AddRtfContent('.\par'#13'{\b ').
                  AddRtfContent(sDocumentNameN,[Test.Params.ItemName]).
                  AddRtfContent(' }:\line  "{\i '+testdocname+'}"\par'#13'{\b ');
                Sec := Data[Doc.Owner.SectionName+'-'+SectionNameValue];
                if Sec<>nil then
                  item := Sec.DisplayName(Doc.Owner)+': '+
                    TrimLastPeriod(Sec.ShortDescription(layout));
              end else begin
                Sec := Data[Doc.Owner.SectionName+'-'+SectionNameValue];
                if (Sec<>nil) then begin
                  if item<>'' then begin
                    WR.AddRtfContent(sAssociatedItems+'}:\par'#13);
                    WR.RtfList('- '+item);
                    item := '';
                  end;
                  WR.RtfList('- '+Sec.DisplayName(Doc.Owner)+': '+
                    TrimLastPeriod(Sec.ShortDescription(layout)));
                end;
              end;
            end;
            if item<>'' then begin // write remaining item
              WR.AddRtfContent(sAssociatedItem+'}:\line'#13+
                item+'.\par'#13);
              item := '';
            end;
          end;
        end;
        CloseRtf(fDoc);
        exit; // no secund part of VV document
      end;
    end;
  end;
  if {$ifdef USEPARSER}(SAD=nil) and{$endif} (Doc<>DI) then
    CreateRTFDetails(0,true) else // write main parts, following DILayout
    CreateRTFDetails(1,false); // write DI or SAD-like documents second part
  CloseRtf(SaveFormat);
end;

procedure TProject.CreateSectionDocument(const SectionName: string);
var Sec: TSection;
begin
  Sec := Data[SectionName];
  if Sec=nil then exit;
  CreateDefaultDocument(Sec.SectionNameKind,Sec);
end;

procedure TProject.CreateTestDocument(const SectionName: string);
var Test: TSection;
    TestDoc, TestsDoc: PDocument;
begin
  Test := Data[SectionName];
  if Test=nil then exit;
  TestDoc := DocumentFind(Test.SectionNameKind);
  if TestDoc=nil then exit;
  TestsDoc := TestDoc.TestDoc;
  if TestsDoc=nil then exit;
  CreateDefaultDocument(TestsDoc.Params.SectionName,Test);
end;

procedure TProject.CreateSummarySheets(Tests: TSection);
// manually add all Test Summary Sheets
var t: integer;
    Test: PDocument;
begin
  if Tests=nil then exit;
  Test := DocumentFind(Tests['DocByDescription']);
  if Test=nil then exit;
  // A4 paper, Font Size 11, WinAnsi=1252, English=1033
  WR := WRClass.Create(Layout,11,1252,
    Project.ReadInteger('DefLang',1033),false,false);
  for t := 0 to high(Test.List) do
  if Test.List[t]['Description']<>'' then
    CreateDefaultDocument(Tests.SectionName,Test.List[t],true);
  WR.FileName := Header.ProjectName+' Tests Summary Sheets '+Test.Params['Revision']+'.rtf';
  CloseRTF;
end;

procedure TProject.CreateRTFDetails(Level: integer; AutoFooter: boolean);
procedure WriteReferenceTable(OwnerDoc: PDocument);
var i,j: integer;
    Own: TSection;
    ident: string;
begin
  if OwnerDoc=nil then exit;
  // this document has sub items -> show reference Table (DI->SRS, e.g.)
  if ReferenceTablesPos=0 then begin
    ReferenceTablesPos := WR.RtfText.len;  // used to write DocumentIndex=... tables after parsing
    ReferenceTablesTitleLevel := WR.TitleLevel;
  end;
  if not WR.TitleFlat then
    WR.RtfPage;
  WR.RtfTitle(format(sReferenceTableN,[OwnerDoc.Params.DisplayName(nil)]),Level+1);
  WR.AddRtfContent(ExpandDocumentNames(format(sQuickReferenceTableNN,
    [OwnerDoc.Params.SectionName,Doc.Params.SectionName])));
  WR.RtfColsPercent([12,17,62,9],true,true,false,'\trhdr');
  WR.RtfRow(['\qc\b '+OwnerDoc.Params.ItemName+' #',Doc.Params.ItemName+' #',
    '\ql '+sDescription,'\qc '+sPage+'\b0'],true);
  WR.RtfColsPercent([12,17,62,9],true,true,false,'\trkeep');
  for i := 0 to length(OwnerDoc.List)-1 do begin // for each [DI-*]
    Own := OwnerDoc.List[i];
    ident := '';
    for j := 0 to length(Doc.List)-1 do // put SRS-DI-4.1, SRS-MENU01..
    with Doc.List[j] do
    if (Owner=Own) and (SectionNameValue<>Own.SectionName) then begin
      if ident='' then
        ident := SectionNameValue else
        ident := ident+'\line '+SectionNameValue;
    end;
    if ident='' then // not split into sub items -> add owner (DI)
      ident := Own.SectionName else
    if pos('\line',ident)>0 then
      ident := '{'+WR.RtfFontString(91)+ident+'}';
    WR.RtfRow(['\qc '+Own.SectionNameValue, ident,
      '\ql{'+WR.RtfFontString(91)+TrimLastPeriod(Own.Description)+'}',
      '\qc '+RtfPageRefTo(Own.SectionName,true)]);
  end;
  WR.RtfColsEnd;
end;
var i, j, aLevel, DIDetailsIndex, LastDIDetailsIndex: integer;
    DIDetails, Current: TSection;
    OwnerDoc, ReferDoc: PDocument;
    ident, aLayout, aValue, testdesc, testdocname, DesignInputSplit,
    title1stpart, title2ndpart, un, desc, sec, procname, quoted: string;
    isSAD, WriteRisk: boolean;
procedure DoWriteRisk;
var aRisk: TRisk;
    aRiskValue, aComment, aTeam: string;
begin
  aRiskValue := DIDetails['Risk'];
  if aRiskValue='' then exit;
  aRisk.FromString(aRiskValue);
  if aRisk.Comment=sRiskEvaluatedFromSub then exit;
  WR.RtfColsPercent([10,90],true,false,true,'\trkeep');
  if aRisk.Comment='' then
    aComment := '' else // no comment
    aComment := '\line '+aRisk.Comment;
  if aRisk.EvaluatedBy='' then
    aTeam := '' else // no name -> no team
    aTeam := '\line{'+WR.RtfFontString(91)+
      sRiskEvaluationTeam+': '+aRisk.EvaluatedByDisplay+'}';
  if aRisk.Risk[0]=0 then // not implemented -> risk not evaluated
    WR.RtfRow(['\qr RISK','\ql '+sRiskNotEvaluatedNoModif],true) else begin
    WR.RtfRow(['\qr RISK','\ql '+RiskDefs[0].Name+': '+IntToStr(aRisk.Risk[0])+
      ', '+RiskDefs[1].Name+': '+IntToStr(aRisk.Risk[1])+
      ', '+RiskDefs[2].Name+': '+IntToStr(aRisk.Risk[2])+
          aComment+aTeam],true);
  end;
end;
function WriteDetail(DIIndex: integer; Parent: PDocument): boolean;
begin
  result := false;
  if LastDIDetailsIndex>=DIIndex then exit; // write once
  result := true;
  WR.RtfText.AddRtfContent('{\i '+GetDIDisplayName(Parent.List[DIIndex])+': ').
    AddWithoutPeriod(Parent.List[DIIndex]['Ident']).AddRtfContent('.}');
  WR.RtfPar;
  LastDIDetailsIndex := DIIndex;
end;
procedure WriteOwnerDetails(aLevel: integer; forceNotVoid: boolean; Parent: PDocument);
var k: integer;
begin
//  if (Parent=DI) or (Doc.Owner=DI.Params) then // write DI details only if not in a "private" order
  if WriteDetail(DIDetailsIndex,Parent) then begin // DI description in italics
    forceNotVoid := false; // Description='' -> DI details would be written twice
    if (j<high(Doc.List)) and (Doc.List[j+1].Root=DIDetails) then begin // sub items
      WR.AddRtfContent(DesignInputSplit,[Doc.Params.ItemName]).
        AddRtfContent(':\par'#13);
      WR.RtfColsPercent([30,70],false,false,true,'\trkeep');
      for k := j+1 to high(Doc.List) do
      with Doc.List[k] do // for all sub-items:
      if Root=DIDetails then
        WR.RtfRow(['\qr{\i '+RtfLinkTo(SectionNameValue,DisplayName(Doc.Owner))+'}',
          '\ql '+TrimLastPeriod(Owner.ShortDescription(''))+'.']);
      WR.RtfColsEnd;
    end;
    WriteRisk := isTrue(Doc.Params['WriteRisk']);
  end;
  if Current.Owner.HasBody then
    CreateRTFBody(Current.Owner,aLevel) else
    if forceNotVoid or
      (Current.Owner['Description']<>'') then // '' -> already written in WriteDetails()
      WR.RtfText.AddWithoutPeriod(Current.Owner.Description).
       AddRtfContent('.').RtfPar;
  if WriteRisk then begin
    DoWriteRisk;
    WriteRisk := false;
  end;
end;
function GetDIDetailsTitle(Parent: PDocument): string;
begin
  if Parent=DI then
    result := GetDIDisplayName(DIDetails) else
    result := Parent.Params.ItemName;
  result := result+'\line{';
  if Doc=DI then
    result := result+'{\ul '+DIDetails['InputLevel']+'} - ' else
    result := result+WR.RtfFontString(120);
  result := result+TrimLastPeriod(DIDetails['Ident'])+'}';
end;
procedure AppendBlock(aValue: string; Parent: PDocument);
// aValue='4.2.1' from DILayout (Parent=DI), or '1' for 'RK-1' (Parent=RK)
var j,k, DetailLevel: integer;
    U, P: PChar;
    aItemName, aItemTitle: string;
begin
  aValue := Parent.Params.SectionName+'-'+aValue; // 'Parent-4.2.1'
  DIDetailsIndex := Parent.GetSectionIndex(aValue);
  if DIDetailsIndex<0 then exit;
  DIDetails := Parent.List[DIDetailsIndex]; // DIDetails = [Parent-4.2.1]
  if Doc.Owner=Parent.Params then begin // Parent or SRS: DIDetails in title
    aLevel := Level+1; // DIDetails.Level; all titles at the same level
    WR.RtfTitle(GetDIDetailsTitle(Parent),aLevel,Doc<>Parent,
      DIDetails.SectionName); // BookMark='DI-4.2.1' or 'RK-1'
    for j := 0 to length(Doc.List)-1 do begin // for all items
      Current := Doc.List[j];
      if Current.Owner<>DIDetails then continue;
      if Current.SectionNameValue=DIDetails.SectionName then begin // no sub item
        CreateRTFBody(Current,aLevel,true); // put it one by one
      end else begin // sub items: put as a table
        if Current.hasBody then begin
          CreateRTFBody(Current,aLevel+1,true);
          WR.RTFTitle(sImplementation,aLevel+1,true,
            Current.SectionNameValue); // 'SRS-Parent-4.2.1' -> BookMark='Parent-4.2.1'
        end;
        WR.AddRtfContent(DesignInputSplit,[Doc.Params.ItemName]).
          AddRtfContent(':\par'#13).
          RtfColsPercent([20,80],false,false,true,'\trkeep');
        for k := j to length(Doc.List)-1 do
        with Doc.List[k] do
        if Owner=DIDetails then
          WR.RtfRow([RtfBookMark('\qr{\b '+SectionNameValue+'}',
            SectionNameValue),
            '\ql '+TrimLastPeriod(Description)+'.']);
        WR.RtfColsEnd;
        break;
      end;
    end;
    if isTrue(Doc.Params['WriteRisk']) then
      DoWriteRisk;
  end else begin // SAD,SDD,Test: write every item
     if Parent<>DI then
      aItemName := Parent.Params.ItemName else
      aItemName := Doc.Owner.ItemName;
    for j := 0 to length(Doc.List)-1 do begin // for all items
      Current := Doc.List[j];
      if Current.Root<>DIDetails then continue;
      aItemTitle := Current.Description;
      if aItemTitle<>'' then
        aItemTitle := '\line '+NormToUpper[aItemTitle[1]]+copy(aItemTitle,2,1000);
      WR.RtfTitle(aItemName+' # '+Current.SectionNameValue+aItemTitle, // SWRS # Parent-4.4
        Level+1,true,Current.SectionNameValue); // BookMark=SectionNameValue='DI-4.3'
      // .TestDoc=[Tests] where DocByDescription=Test
      if Doc.TestDoc<>nil then begin
        if Current['Description']<>'' then begin
          testdesc := Current['Description'];
          TestGetDescr(Current,DIDetails,Doc,testdocname,title1stpart);
        end;
        if Current.Owner['ShortName']<>'' then
          title1stpart := TrimLastPeriod(Current.Owner.ShortDescription(aLayout)) else
          title1stpart := format(sAbstractN,[aItemName]); // 'SWRS Abstract'
        title2ndpart := Doc.TestDoc.Params['Name'];
      end else begin // [Tests]->'Test report'
        title1stpart := sAbstract;
        title2ndpart := sImplementation;
      end;
      if Current.HasBody then begin
        DetailLevel := Level+2;
        if Doc.TestDoc<>nil then begin // special [Test]
          WR.RtfTitle(title1stpart,DetailLevel,true);
          WriteOwnerDetails(DetailLevel+1,true,Parent); // rewrite SRS description
          if Current.HasTitle then
            WR.RtfPar else
            WR.RtfTitle(title2ndpart,DetailLevel,true);
          WR.AddRtfContent(sTestExtractFromNN,[testdocname,TrimLastPeriod(testdesc,true)]).
             AddRtfContent('.\par{\sb0\sa0\par}\qj'#13);
          CreateRTFBody(Current,DetailLevel,true,false,true); // write Test details
        end else begin
          if Current.HasTitle then begin
            WR.RtfTitle(title1stpart,DetailLevel,true);
            WriteOwnerDetails(DetailLevel+1,true,Parent);
          end else
            WriteRisk := true;
          CreateRTFBody(Current,DetailLevel); // write SDD details
        end;
      end else // no body available
      if Data[Current['SameAs']]<>nil then begin // SameAs=
        WR.RtfTitle(title1stpart,Level+2,true);
        WriteOwnerDetails(Level+3,true,Parent); // rewrite SRS description
        CreateRTFBody(Current,Level+2); // write SDD details
        WR.RtfTitle(title2ndpart,Level+2,true);
        WR.AddRtfContent(ExpandDocumentNames(format(
          sSameAsN,[Current['SameAs']])+'.\par'#13));
        continue;
      end else
        WriteOwnerDetails(Level+2,false,Parent); // just rewrite SRS description
      if isSAD then begin
        // [SAD] document -> write UnitsUsed=.. and UnitsModified=..
        ident := Current['UnitsModified'];
        if ident<>'' then begin
          WR.AddRtfContent('{\sb220\b '+sItemModifiedUnits+':\b0\par}');
          WR.RtfFont(92);
          WR.RtfColsPercent([26,66,8],true,true,true,'\trhdr');
          WR.RtfRow(['\b\qc '+sUnitName,'\ql '+sDescription,'\qc '+sPage+'\b0'],true);
          WR.RtfColsPercent([26,66,8],true,true,true,'\trkeep');
          U := pointer(ident);
          while U<>nil do begin
            un := GetNextItem(U); // = TPasUnit(p).OutputFileName
            desc := '\ql '+Doc.Params[un]; // description is [SAD].EIA\Name.pas=..
            // unit.pasexact=.. TClass.Funct only (not TClass)
            P := pointer(Current[ExtractFileName(un)+'exact']);
            if P<>nil then begin // proc/object name
              procname := GetNextItem(P);
              desc := desc+'\par{\i '+sSeeInParticular+' ';
              repeat
                //desc := desc+RtfLinkTo(BookMarkHash(un+'.'+procname),procname);
                desc := desc+'{\f1\i0 '+procname+'}';
                if P=nil then break;
                if StrScan(P,',')<>nil then
                  desc := desc+', ' else
                  desc := desc+' '+sAnd+' ';
                procname := GetNextItem(P);
              until false;
              desc := desc+'}.';
            end;
            WR.RtfRow(['\qc{\i '+ValAt(ExtractFileName(un),0,'.')+'}',
              desc, '\qc '+RtfPageRefTo(un,true)]);
          end;
          WR.RtfColsEnd.RtfFont(100);
        end;
        quoted := Current['UnitsUsed'];
        if (ident<>quoted) and (quoted<>'') then begin
          WR.AddRtfContent('{\sb220\b '+sItemQuotedUnits+':\b0\par}');
          WR.RtfFont(92);
          WR.RtfColsPercent([26,66,8],true,true,true,'\trhdr');
          WR.RtfRow(['\b\qc '+sUnitName,'\ql '+sDescription,'\qc '+sPage+'\b0'],true);
          WR.RtfColsPercent([26,66,8],true,true,true,'\trkeep');
          U := pointer(quoted);
          while U<>nil do begin
            un := GetNextItem(U); // = TPasUnit(p).OutputFileName
            if CSVContains(ident,un) then continue; // already written
            WR.RtfRow(['\qc{\i '+ValAt(ExtractFileName(un),0,'.')+'}',
              '\ql '+Doc.Params[un], '\qc '+RtfPageRefTo(un,true)]);
          end;
          WR.RtfColsEnd.RtfFont(100);
        end;
      end;
    end;
    if WriteRisk then
      DoWriteRisk;
  end;
end;
var P: PChar;
begin
  if (Doc=nil) or (Doc.Params.SectionNameValue<>Doc.Params.SectionName) then
    exit; // no details existing for [*-*]
  if (Doc.Order=nil) then exit; // no details if not order to get from
  if (DILayout=nil) then exit;
  LastDIDetailsIndex := -1; // write DI only once
  DesignInputSplit := Doc.Params.ReadString('DesignInputSplit',sDesignInputSplitDefault);
  // 0. special case: only one section to write
  if ForcedOnlySection<>nil then begin
    WR.TitleLevel[0] := 1;
    DIDetails := ForcedOnlySection.Root;
    DIDetailsIndex := DI.GetSectionIndex(DIDetails);
    if Doc.Owner=DI.Params then begin // DI or SRS: DIDetails in title
      WR.RtfTitle(GetDIDetailsTitle(DI),1,false);
      CreateRTFBody(ForcedOnlySection,1,false);
    end else begin // SAD,SDD,Test: write item
      WR.RtfTitle(Header.DocumentTitle,1,false);
      WriteDetail(DIDetailsIndex,DI); // DI description in italics
      if (ForcedOnlySection.Owner<>nil) and
        ForcedOnlySection.Owner.HasBody then // write Owner (=SRS) description
        CreateRTFBody(ForcedOnlySection.Owner,1,false) else
        WR.RtfText.AddWithoutPeriod(ForcedOnlySection.Description).
          AddRtfContent('.').RtfPar;
      CreateRTFBody(ForcedOnlySection,1,false,false,Doc.TestDoc<>nil); // write body
    end;
    if isTrue(Doc.Params['WriteRisk']) then
      DoWriteRisk;
    exit;
  end;
  // 1. reference tables:
  OwnerDoc := DocumentFind(Doc.Owner);
  if OwnerDoc<>nil then
  if Doc.Owner<>Doc.Order then begin
    // this document has sub items -> show reference Table (DI->SRS, e.g.)
    WriteReferenceTable(OwnerDoc);
  end else
  if Doc.Owner<>Doc.Params then begin
    // SDD,SAD: show the reference Table to SRS (Owner) - no sub items
    ident := Doc.Params['Name']+' Reference Table';
    if WR.TitleLevel[Level+1]=0 then
      WR.RtfTitle(ident,Level+1,false) else
      WR.RtfTitle(ident,Level+1);
    WR.AddRtfContent(ExpandDocumentNames(format(sQuickReferenceTableN,[Doc.Owner.SectionName])));
    WR.RtfColsPercent([17,75,9],true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b '+Doc.Owner.ItemName+' #','\ql '+sDescription,'\qc '+sPage+'\b0'],true);
    WR.RtfColsPercent([17,75,9],true,true,false,'\trkeep');
    for i := 0 to length(OwnerDoc.List)-1 do begin
      if OwnerDoc=DI then
         sec := OwnerDoc.List[i].SectionName else // special case if Owner=DI
         sec := OwnerDoc.List[i].SectionNameValue; // 'SRS-DI-4.1'->'DI-4.1'
      for j := 0 to length(Doc.List)-1 do // search corresponding item
      if Doc.List[j].SectionNameValue=sec then begin
        // 'SRS-DI-4.1'->'SDD-DI-4.1'
        WR.RtfRow(['\qc '+WR.RtfGoodSized(sec),
          '\ql '+TrimLastPeriod(OwnerDoc.List[i].Description),
          '\qc '+RtfPageRefTo(sec,true)]);
        break;
      end;
    end;
    WR.RtfColsEnd;
  end;
  P := pointer(Doc.Params['Refers']);
  while P<>nil do // this document refers to other documents (e.g. RK)
    WriteReferenceTable(DocumentFind(GetNextItem(P)));
  if ReferenceTablesPos=0 then begin
    ReferenceTablesPos := WR.RtfText.len;  // used to write DocumentIndex=... tables after parsing
    ReferenceTablesTitleLevel := WR.TitleLevel;
  end;
  // 2. main loop:
  isSAD := Doc.Params['Source']<>''; // [SAD] document
  WriteRisk := false;
  testdesc := '';
  aLayout := '';
  if Doc.Params.Root<>DI.Params then begin
    // doesn't inherit from DI -> follow its own layout ([RK-1],[RK-2]...)
    for j := 0 to high(Doc.List) do begin // for all items
      Current := Doc.List[j];
      DIDetails := Current; // for Risk= access
      ident := Current['Ident'];
      if ident<>'' then
        ident := '\line '+ident;
      WR.RtfTitle(Doc.Owner.ItemName+' # '+Current.SectionName+ident,
        Level+1,true,Current.SectionName); // BookMark=SectionName='RK-1'
      if Current.HasBody then
        CreateRTFBody(Current,Level+1,true);
      if isTrue(Doc.Params['WriteRisk']) then
        DoWriteRisk;
    end;
  end else begin
    // inherits from DI -> follow the DILayout
    for i := 0 to DILayout.Lines.Count-1 do begin
      aValue := DILayout.Lines[i];
      if (aValue='') or (aValue[1]=';') then continue else
      if aValue[1]=':' then begin // 'Service Software' = big section
        aLayout := trim(copy(aValue,2,maxInt));
        // new page and footer for each big part:
        if AutoFooter and (Doc<>DI) then
          ForceFooter(format('%s - %s',[Doc.Params.ItemName,aLayout]));
        WR.RtfTitle(aLayout,Level,true,'Layout_'+aLayout); // just write the big section title
        Current := Data[Doc.Params.SectionNameKind+'-'+aLayout]; // 'SRS-ServiceSoftware'
        if (Current<>nil) and Current.HasBody then
          CreateRTFBody(Current,Level+1,true); // get introduction text of this section
      end else begin // '4.2.1' in [DILayout]
        AppendBlock(aValue,DI);
      end;
    end;
    P := pointer(Doc.Params['Refers']);
    while P<>nil do begin
      un := GetNextItem(P); // e.g. 'RK' -> list [SRS-RK-*]
      ReferDoc := DocumentFind(un);
      if ReferDoc=nil then
        continue;
      ident := ReferDoc.Params.DisplayName(nil);
      if AutoFooter and (Doc<>DI) then
        ForceFooter(format('%s - %s',[Doc.Params.ItemName,ident]));
      WR.RtfTitle(ident,Level,true,'Layout_'+un); // just write the big section title
      for i := 0 to high(ReferDoc.List) do
        AppendBlock(ReferDoc.List[i].SectionNameValue,ReferDoc);
{      for j := 0 to high(Doc.List) do begin
        Current := Doc.List[j];
          DIDetails := Current.Owner; // for Risk= access
          if not SameText(Current.Owner.SectionNameKind,un) then continue;
          WR.RtfTitle(ReferDoc.Params.ItemName+' # '+Current.SectionNameValue,
            Level+1,true,Current.SectionNameValue); // BookMark='RK-1'
          if Current.HasBody then
            CreateRTFBody(Current,Level+2);
          if isTrue(Doc.Params['WriteRisk']) then
            DoWriteRisk;
        end;}
      end;
  end;
end;

function TProject.ExpandDocumentNames(text: string): string;
// '@DI@' -> '{\i Design Input Product Specifications} (Design Input) document'
var i,j,k,aTitleBookmark: integer;
    DocName, Ext, Caption, BookMark: string;
    aSec, aDoc, GraphV: TSection;
    ForcePicturePage: boolean;
    HTTP: string;
begin
  repeat
    i := Pos('@http://',text);
    if i=0 then
      break;
    for j := i to length(text)+1 do
      if text[j]<=' ' then begin
        HTTP := copy(text,i+1,j-i-1);
        delete(text,i,j-i);
        insert('{\field{\*\fldinst{HYPERLINK'#13'"'+HTTP+
          '"'#13'}}{\fldrslt{\ul'#13+HTTP+'..'+'}}}',text,i);
        break;
      end;
  until false;
  j := 1;
  repeat
    i := PosEx('@',text,j);
    if i=0 then
      break; // no more '@'
    if (i>1) and (NormToUpper[text[i-1]] in ['A'..'Z','0'..'9']) then begin
      j := i+1;
      continue; // bidule@mlds -> email adress -> ignore
    end;
    if text[i+1]='@' then begin
      j := i+2;
      continue; // @@ -> ignore
    end;
    j := PosEx('@',text,i+1);
    if j=0 then
      break; // no valid '@...@' name
    DocName := copy(text,i+1,j-i-1);
    delete(text,i,j-i+1);
    dec(j,length(DocName)); // inc(j,length(DocName)) below with modified DocName
    aSec := Data[DocName];
    if aSec=nil then begin
      if People[DocName]<>'' then // test if @Arnaud Bouchez@
        DocName := DocName+' ('+GetPeopleFunction(DocName)+')' else begin
        if DocName[1]='*' then begin
          // '@*keyword@'-> inlined keyword for Index in KeyWord 1=bookmark0,...
          delete(DocName,1,1);
          inc(ReferenceIndexIndex);
          BookMark := 'NDX_'+IntToStr(ReferenceIndexIndex);
          if DocName[1]='*' then begin // @**keyword@ for main entry reference
            delete(DocName,1,1);
            BookMark := 'M'+BookMark;
          end;
          ReferenceIndex.AddCSVValue(DocName,BookMark);
          DocName := '{\*\bkmkstart '+BookMark+'}'+DocName+'{\*\bkmkend '+BookMark+'}';
        end else
        if DocName[1]='=' then begin
          // '@=30%bidule.png@' or '@=30%%FirmwareBoot@' -> inlined picture
          delete(DocName,1,1);
          DocName := PictureInlined(DocName,0,true);
        end else begin
          Ext := ExtractFileExt(DocName);
          if (DocName[1]='%') or (GetStringIndex(VALID_PICTURES_EXT, Ext)>=0) then begin
            // @picture.emf@ -> use caption name and page if previously added
            GraphV := nil;
            ForcePicturePage := false;
            if DocName[1]='%' then begin
              delete(DocName,1,1);
              if DocName[1]='%' then begin // '%%FirmwareBoot' for \graph FirmwareBoot ...
                NeedGraphValues;
                GraphV := fGraphValues;
                delete(DocName,1,1);
                DocName := GraphDirName+DocName;
              end else
                ForcePicturePage := true;
            end;
            Caption := PictureCaption(DocName,nil,GraphV);
            if Caption='' then
              DocName := '{\i '+DocName+'}' else begin
              BookMark := ReferencePictures[DocName];
              if BookMark='' then begin // not yet written
                if ForcePicturePage then // %name.png assure OK -> use default name
                  BookMark := RtfBookMarkName(DocName);
              end else
                BookMark := LastCSV(BookMark); // already written -> jump to closer = last
              DocName := '{\i '+Caption+'}';
              if BookMark<>'' then
                DocName := RtfLinkTo(BookMark,DocName+' '+format(
                  sPageN,[RtfPageRefTo(BookMark,false)]));
            end;
          end else
          if GetStringIndex(VALID_PROGRAM_EXT, Ext)>=0 then begin
            // @something.pas@ -> add to ReferenceProgram
            if DocName[1]='!' then begin // modified source file
              delete(DocName,1,1);
              k := pos('!',DocName);
              if k>0 then // '@!procname!EIA\unit.pas@
                delete(DocName,1,k); // trim procname
            end;
            if ReferenceProgram['LastTitleBookMark_'+DocName]<>WR.LastTitleBookmark then begin
              // add bookmark once by title for each file name
              ReferenceProgram['LastTitleBookMark_'+DocName] := WR.LastTitleBookmark;
              inc(ReferenceProgramIndex);
              BookMark := 'PROGRAM_'+IntToStr(ReferenceProgramIndex);
              ReferenceProgram.AddCSVValue(DocName,Bookmark);
              DocName := '{\*\bkmkstart '+BookMark+'}{\f1\fs20 '+ExtractFileName(DocName)+
                '}{\*\bkmkend '+BookMark+'}';
            end else // already bookmarked -> just add string as Courrier New font
              DocName := '{\f1\fs20 '+ExtractFileName(DocName)+'}';
          end else
          if TryStrToInt(DocName,aTitleBookmark) and (aTitleBookmark<>0) then begin
            // @12@ -> ':12 Title'
            BookMark := 'TITL_'+IntToStr(aTitleBookmark);
            k := TitleBookmark.IndexOfObject(pointer(aTitleBookmark));
            if k>=0 then // we got the 'Title' -> put this text
              Caption := '{\i '+TitleBookmark[k]+'}' else
              // title is below -> put only page number
              Caption := sBelow;
            DocName := RtfLinkTo(BookMark, Caption+' '+format(
                sPageN,[RtfPageRefTo(BookMark,false)]));
          end;
        end;
      end;
    end else // aSec<>nil -> @..@ is a Section Name
    if aSec.SectionNameValue<>aSec.SectionName then begin // @SRS-4.7.1@
      aDoc := Data[aSec.SectionNameKind]; // aDoc = SRS
      if aDoc<>nil then begin  // @SRS-DI-4.7.1@ -> 'SWRS # DI-4.7.1'
        AddReferenceDocument(aDoc);
        DocName := '{\i '+aSec.DisplayName(aDoc)+'}';
        if (Doc<>nil) and (aDoc=Doc.Params) and (ForcedOnlySection=nil) then
          // in the same doc -> add hyperlink and page No
          DocName := RtfLinkTo(aSec.SectionNameValue,
            DocName+' '+format(
                sPageN,[RtfPageRefTo(aSec.SectionNameValue,false)]));
      end;
    end else begin
      // @SRS@ -> 'Software Requirements Specifications (SWRS) document'
      AddReferenceDocument(aSec);
      DocName := format(sDocNameParenthNN,[aSec.ReadString('Name',DocName),
        aSec.ItemName]);
    end;
    insert(DocName,text,i);
    inc(j,length(DocName));
  until false;
  result := text;
end;

function TProject.GetDIDisplayName(aDI: TSection): string;
var SCR: string;
    DIDoc: TSection;
begin // 'Design Input 4.2.1 (SCR#23)'
  if aDI=nil then begin
    result := '';
    exit;
  end;
  if aDI.Root.SectionNameKind='DI' then
    DIDoc := DI.Params else
    DIDoc := aDI;
  result := DIDoc.DisplayName(nil)+' '+aDI.SectionNameValue;
  SCR := aDI['Request'];
  if SCR<>'' then begin
    SCR := ValAt(SCR,0); // Request=SCR #65,EIA 2.0+IFA 2.0,Low
    result := result+' ('+SCR+')';
  end;
end;

procedure TProject.CreateRiskAssessmentTable;
var S: integer;
    P: PChar;
    NotImplemented, desc, Request: string;
function GetNextRisk: string;
begin
  result := GetNextItem(P);
  if result='0' then
    result := ''; // Risk=0 : not implemented -> display nothing
end;
function GetComment: string;
begin
  GetNextItem(P); // ignore EvaluatedBy
  result := P; // returns Comment
end;
begin
  Doc := DocumentFind('Risk');
  Header.visible := true;
  Header.confidential := true;
  AddReferenceDocument(Doc.Params.Owner); // add SRS
  CreateRTF(true,true,true,true,true,true,false,false,false); // first pages
  ForceLandscape;
  Doc := DocumentFind(Doc.Params.Owner.SectionName); // Doc = SRS
  WR.RtfColsPercent([12,43,9,5,5,5,23],true,true,false,'\trhdr');
  desc := sRiskShort;
  P := pointer(desc); // 'Sev.,Pro.,Occ.,Management approval,Approved';
  WR.RtfRow(['\qc\b '+Doc.Params.ItemName+' #',
    '\ql '+DI.Params.DisplayName(nil)+' '+sDescription,
    sRequest,'\qc '+GetNextItem(P),GetNextItem(P),GetNextItem(P),
    '\ql '+sRiskAssessment+' \b0'],true);
  WR.RtfColsPercent([12,43,9,5,5,5,23],true,true,false,'\trkeep');
  NotImplemented := '0,0,0,,'+Project.ReadString('NoRiskDisplay',sNotImplemented);
  for S := 0 to length(Doc.List)-1 do
  with Doc.List[S] do begin
    P := pointer(Risk);
    if P=nil then
      P := pointer(NotImplemented);
    Request := ValAt(ReadString('Request',Owner['Request']),0);
    // Request=SCR #65,EIA 2.0+IFA 2.0,Low
    WR.RtfRow(['\qc '+WR.RtfGoodSized(SectionNameValue),
      '\ql '+TrimLastPeriod(Description),
      '\qc{'+WR.RtfFontString(91)+Request+'}',
      GetNextRisk, GetNextRisk, GetNextRisk,'\ql '+GetComment]);
  end;
  WR.RtfColsEnd;
  // calculate Cols[] with portrait width
  WR.Width := Layout.Page.Width-Layout.Margin.Left-Layout.Margin.Right;
  // write Related Document Table
  CloseRtf;
end;

procedure TProject.ForceLandscape;
begin
  WR.Landscape := true;
  HeaderAndFooter; // recreate full width for both Header+Footer
  WR.RtfParDefault;
end;

procedure TProject.ForceFooter(const FooterTitle: string; forcePortrait: boolean);
begin
  if forcePortrait and WR.Landscape then begin
    WR.Landscape := false;
    HeaderOnly;
  end else
    WR.RtfEndSection;
  Footer(WR,FooterTitle);
  WR.RtfParDefault;
end;

procedure TProject.InitPageSize;
begin
  // A4 default paper size
  Layout.Page.Width := MM2Inch(210);
  Layout.Page.Height := MM2Inch(297);
  // margins to 1.2 or 2.4 cm
  Layout.Margin.Left := MM2Inch(24);
  Layout.Margin.Right := MM2Inch(24);
  Layout.Margin.Top := MM2Inch(12);
  Layout.Margin.Bottom := MM2Inch(12);
end;

procedure TProject.HeaderAndFooter;
begin
  HeaderOnly;
  Footer(WR,Header.DocumentTitle);
end;

procedure TProject.HeaderOnly;
var logo: string;
begin
  with Header do
  if visible then begin
    WR.RtfHeaderBegin(88);
    if withlogo then begin
      logo := Project['Logo'];
      if (logo='') or (Pictures[logo]='') then
        logo := '' else
        logo := PictureInlined(logo,100,true);
    end;
    WR.AddRtfContent('\sa80\sb80 ');
    if logo='' then begin
      if withborder then
        WR.AddRtfContent(Project['Company']).RtfPar;
      WR.RtfColsPercent(ColWidth,true,withborder);
      WR.RtfRow([sProjectName+':',ProjectName,sRevisionSharp+':',Rev]);
      WR.RtfRow([sDocumentName+':',DocumentTitle,HeaderFunction+':',HeaderName],true);
    end else begin
      WR.RtfColsPercent([55,45],true,false); // just one row with logo on right
      WR.RtfRow([format('%s \line %s %s\line %s: %s',
        [ProjectName,DocumentTitle,Rev,sDate,RevDate]),logo],true);
    end;
    WR.RtfLine;
    WR.RtfHeaderEnd;
  end;
end;

procedure TProject.Footer(WR: TProjectWriter; const FooterTitle: string);
var conf: string;
begin
  with Header do begin
    LastFooterTitle := FooterTitle;
    WR.RtfFooterBegin(88);
    WR.RtfPar.
       RtfColsPercent([18,63,19],false,false);
    if confidential then
      conf := '\ql '+Project.ReadString('Confidential',sCONFIDENTIAL) else
      conf := '';
    WR.RtfRow([conf,'\qc '+FooterTitle+' - '+sRev+' '+Rev{+', '+DateToStr(Now)},
      '\qr '+format(sPageNN,[RtfField('PAGE'),RtfField('NUMPAGES')])],true);
    WR.RtfFooterEnd;
  end;
end;

procedure TProject.AddReferenceDocument(aSec: TSection);
begin
  if (aSec<>nil) and (aSec<>Doc.Params) and // no self reference
     (ReferenceDocuments.IndexOf(aSec)<0) then
    ReferenceDocuments.Add(aSec);
end;

procedure TProject.CloseRtf(aFormat: TSaveFormat=fDoc);
procedure CreateIndex(IndexName: string; References: TSection);
procedure WRTitle(const Title: string);
begin
  if not WR.TitleFlat then
    WR.RtfPage;
  WR.RtfTitle(title,1,false,'-'); // '-' -> force no bookmark -> otherwize,
    // will appear at the end of the Table of Contents
  WR.AddRtfContent(ExpandDocumentNames(format(sQuickReferenceTableN2,
    [IndexName,Doc.Params.SectionName])));
end;
procedure CreateOne(Lines: TStrings; const Title, MatchFirstChars: string);
var i: integer;
    Line, Name, BookMark, aBookMark, BookMarks: string;
    FirstChar: char;
    P: PChar;
    Init: boolean;
begin
  Init := false;
  FirstChar := ' ';
  for i := 0 to Lines.Count-1 do begin
    Line := Lines[i];
    if Line='' then continue;
    if (MatchFirstChars<>'') and
       not IdemPChar(pointer(Line),pointer(MatchFirstChars)) then
      continue;
    References.SplitNameValue(Line,Name,BookMark);
    P := pointer(BookMark);
    BookMarks := '';
    while P<>nil do begin
      aBookMark := GetNextItem(P);
      if aBookMark<>'' then begin
        if (References=ReferenceIndex) and (aBookMark[1]='M') then // main ref
          aBookMark := '{\b\ul\fs24 '+RtfPageRefTo(aBookMark,true)+'}' else
          aBookMark := RtfPageRefTo(aBookMark,true);
        if BookMarks='' then
          BookMarks := '\qc '+aBookMark else
          BookMarks := BookMarks+', '+aBookMark;
      end;
    end;
    if BookMarks='' then
      continue;
    if References=ReferencePictures then begin
      Line := Name;
      Name := PictureCaption(Line);
      if Name='' then begin
        NeedGraphValues;
        Name := PictureCaption(Line,nil,fGraphValues);
      end;
    end else
    if References=ReferenceProgram then begin
      Delete(Name,1,length(MatchFirstChars));
      Name := StringReplaceAll(Name,'\','\\');
      if length(Name)>35 then
        Name := '\fs20  '+Name else
        Name := '   '+Name;
      Name := '{\f1 '+Name+'}';
    end;
    if not Init then begin
      Init := true;
      if References=ReferenceProgram then begin
        WR.RtfBig(format('%s - %s',[title,sSourceReferenceTable]));
        WR.RtfColsPercent([70,30],true,true,false,'\trhdr');
        WR.RtfRow(['\ql{\f1    }\b '+IndexName,'\qc '+sPage+'\b0'],true);
        WR.RtfColsPercent([70,30],true,true,false,'\trkeep');
      end else
      if References=ReferenceIndex then begin
        WRTitle(title);
      end else begin
        WRTitle(title);
        WR.RtfColsPercent([80,20],true,true,false,'\trhdr');
        WR.RtfRow(['\ql\b '+IndexName,'\qc '+sPage+'\b0'],true);
        WR.RtfColsPercent([80,20],true,true,false,'\trkeep');
      end;
    end;
    if References=ReferenceIndex then begin
      if NormToUpper[Name[1]]<>FirstChar then begin
        if FirstChar<>' ' then
          WR.RtfColsEnd;
        FirstChar := NormToUpper[Name[1]];
        WR.RtfColsPercent([100],true,false,false,'\trkeep');
        WR.RtfRow(['\ql\line{\b\fs30 '+FirstChar+'}'],true);
        WR.RtfColsPercent([30,70],true,true,false,'\trkeep');
      end;
      Name[1] := FirstChar; // force uppercase for 1st char in keyword list
    end;
    WR.RtfRow(['\ql '+Name,BookMarks]);
    if MatchFirstChars<>'' then
      Lines[i] := ''; // erase done item -> write it once
  end;
  if Init then begin
    WR.RtfColsEnd;
    WR.RtfPar;
  end;
end;
var i: integer;
    title: string;
    Lines: TStringList;
begin
  if (References=nil) or (References.Lines.Count=0) then
    exit;
  References.Lines.CaseSensitive := false;
  if References=ReferenceImplements then begin
    // IndexName='IEC=IEC 62304'
    i := pos('=',IndexName);
    if i=0 then // IndexName='IEC'
      title := IndexName else begin
      title := copy(IndexName,1,i-1); // title='IEC'
      Delete(IndexName,1,i); // IndexName='IEC 62304'
    end;
    title := UpperCase(title)+' ';
    // ReferenceImplements[]='IEC 5.7 blabla=Bookmark0,Bookmark1...'
    Lines := TStringList.Create;
    for i := 0 to ReferenceImplements.Lines.Count-1 do
      if IdemPChar(pointer(ReferenceImplements.Lines[i]),pointer(title)) then
        // keep only IEC= items, renaming them as '5.7 blabla=Bookmark0..'
        Lines.Add(copy(ReferenceImplements.Lines[i],length(title)+1,maxInt));
    if Lines.Count=0 then begin
      Lines.Free;
      exit;
    end;
    Lines.CustomSort(SectionNumbersCompare); // sort by '5.1'
  end else begin
    Lines := References.Lines;
    Lines.Sort; // alphabetical sort
  end;
  title := format(sReferenceTableN,[IndexName]);
  if References=ReferenceProgram then begin
    for i := 0 to Lines.Count-1 do
      if IdemPChar(pointer(Lines[i]),'LASTTITLEBOOKMARK_') then
        Lines[i] := ''; // 'LastTitleBookMark_'+DocName trick is not yet used
    WRTitle(title);
    for i := 0 to high(Parse) do
    with Parse[i] do begin // extract references from every [SAD-*] instances
      CreateOne(ReferenceProgram.Lines,ReadString('DisplayName',SectionNameValue),
        UpperCase(SectionNameValue+'\')); // match 'EIA\*' names
    end;
    title := sOthers;
  end;
  CreateOne(Lines,title,'');
  if References=ReferenceImplements then
    Lines.Free;
end;
var tmpPos, tmpSize, i, j, Level, TitleMax: integer;
    P: PChar;
    index, Title, BookMark: string;
    TOCatBOF: boolean;
begin
  if (ReferenceDocuments.Count<>0) and (ReferenceDocumentsPos<>0) then begin
    WR.RtfText; // close any pending {
    tmpPos := WR.Len;
    WR.RtfPar.RtfBig(sRelatedDocuments);
    WR.RtfColsPercent([13,47,10,30],true,true,false,'\trkeep');
    WR.RtfRow(['\qc\b '+sName,'\ql '+sDescription,' '+sRev,'\ql '+sDate+'\b0']);
    for i := 0 to ReferenceDocuments.Count-1 do
    with TSection(ReferenceDocuments[i]) do
      WR.RtfRow(['\qc '+ItemName,'\ql '+Value['Name'],
        ' '+Value['Revision'],Header.RevDate]);
    WR.RtfColsEnd;
    tmpSize := WR.Len-tmpPos;
    WR.MovePortion(tmpPos,ReferenceDocumentsPos,tmpSize);
    if ReferenceTablesPos>ReferenceDocumentsPos then
      inc(ReferenceTablesPos,tmpSize);
    ReferenceDocuments.Clear;
  end;
  tmpPos := 0;
  if (WR.TitlesList<>nil) and (WR.TitlesList.Count>0) then begin
    // Write Table of contents
    WR.RtfText.RtfParDefault; // close any pending {
    TOCatBOF := isTrue(Doc.Params['TableOfContentsAtTheBeginning']);
    if TOCatBOF then begin
      WR.TitleLevel := ReferenceTablesTitleLevel; // back to Introduction's TitleLevels
      tmpPos := WR.Len;
    end else
      ForceFooter(Header.DocumentTitle,true);
    TitleMax := WR.TitlesList.Count-1;
    if TOCatBOF then
      WR.RtfTitle(sTableOfContents,0,False) else
    if WR.TitleFlat then begin
      WR.AddRtfContent('{\sb800\fi80\b\shad').RtfFont(160).
        AddRtfContent(sTableofContents+'\par}');
    end else
      WR.RtfTitle(sTableOfContents,0,false);
    WR.TitleWidth := WR.IndentWidth;
    WR.TitleLevelCurrent := 3; // force indent the RtfCols
    WR.RtfColsPercent([70,15],true,false,true,'\trkeep');
    with WR.TitlesList do
    for i := 0 to TitleMax do begin
      Title := Strings[i]; // 'Title|BOOKMARK'
      BookMark := ValAt(Title,1,'|');
      if BookMark<>'' then
        BookMark := '\qc\li0\fi0'+RtfPageRefTo(BookMark,true);
      Title := ValAt(Title,0,'|');
      Level := pred(integer(Objects[i]));
      if Level<2 then
        Title := WR.RtfFontString(130-Level*15)+Title;
      if Level<3 then
        Title := '{\b '+Title+'}';
      if Level=0 then begin
        if (i<Count-2) and (integer(Objects[i+1])<>1) then
          BookMark := ''; // no page index only if sub items
        for j := 0 to WR.ColsCount-1 do
          if (Level=0) and (BookMark='') then begin
            inc(Level); // bottom line if any sub items
            WR.RtfColVertAlign(j,taRightJustify,true); // DrawBottomLine=true
          end else
            WR.RtfColVertAlign(j,taRightJustify,false);
        WR.RtfRow(['\li0\fi0\ql'+Title,BookMark]);
        for j := 0 to WR.ColsCount-1 do
          WR.RtfColVertAlign(j,taCenter); // restore Vertical center alignment
      end else begin
        WR.RtfRow([format('\ql\fi-%d\li%d %s',
          [WR.IndentWidth,Level*WR.IndentWidth,Title]), BookMark]);
      end;
    end;
    WR.TitleWidth := 0;
    WR.RtfColsEnd.RtfParDefault;
  end;
  if (ForcedOnlySection=nil) and (ReferenceTablesPos>0) then begin // create indexes
    P := pointer(Doc.Params['DocumentIndex']);
    if P<>nil then begin
      // DocumentIndex=Pictures,Source,Implements IEC=IEC 62304,Index
      WR.RtfText; // close any pending {
      if tmpPos=0 then // if not already moving the TOC to the BOF
        tmpPos := WR.Len;
      WR.TitleLevel := ReferenceTablesTitleLevel; // TOC could have overwritten
      while P<>nil do begin
        index := GetNextItem(P);
        if SameText('Pictures',Index) then
          CreateIndex(sPictures,ReferencePictures) else
        if SameText('Index',Index) or SameText(sIndex,Index) then
          CreateIndex(sIndex,ReferenceIndex) else
        if SameText(sSourceFileNames,Index) or SameText('Source',Index) then
          CreateIndex(sSourceFileNames,ReferenceProgram) else
        if IdemPChar(Pointer(index),'IMPLEMENTS ') then
          CreateIndex(Copy(index,12,MaxInt),ReferenceImplements);
      end;
    end;
  end;
  if tmpPos<>0 then begin
    WR.RtfPage; // last moved block to the beginning must end with \page
    WR.MovePortion(tmpPos,ReferenceTablesPos,WR.Len-tmpPos);
  end;
  WR.SaveToFile(aFormat,OldWord2k); // Save=true
  if CreatedDocuments='' then
    CreatedDocuments := WR.FileName else
    CreatedDocuments := CreatedDocuments+','+WR.FileName;
end;

function TProject.PictureCaption(var FileName: string; Coords: PString;
  aGraphValues: TSection): string;
var i,j: integer;
    FN,Up: string;
begin
  if aGraphValues=nil then
    result := Pictures[FileName] else begin
    result := aGraphValues[FileName];
    if (result='') and (pos('.',FileName)=0) then begin
      Up := UpperCase(FileName)+'.'; // search without extension
      for i := 0 to aGraphValues.Lines.Count-1 do begin
        FN := aGraphValues.Lines[i];
        if IdemPChar(pointer(FN),pointer(Up)) then begin
          j := pos('=',FN);
          FileName := copy(FN,1,j-1); // retrieve filename with .EMF or .PNG
          result := copy(FN,j+1,maxInt);
          break;
        end;
      end;
    end;
  end;
  i := pos(',',result);
  if i>0 then begin
    if Coords<>nil then
      Coords^ := copy(result,1,i-1);
    delete(result,1,i);
  end else // 'Service software: LaunchAction, Internal Spawn'
    result := '';
end;

function TProject.PictureFullLine(const Line: string; out Caption: string;
  aGraphValues: TSection): string;
// SDD-DI-4.1-Menu3.emf=4574x6521 95%,Service software: LaunchAction, Internal Spawn
var BookMark, Coords: string;
begin
  result := Line; // result=FileName
  caption := '';
  if result='' then exit;
  if pos(' ',result)=0 then begin // no '796x729 100%' -> 'DI-4.1-interface.png'
    if result[1]='%' then
      delete(result,1,1);
    caption := PictureCaption(result,@Coords,aGraphValues);
    if caption<>'' then begin
      if ReferencePictures[result]='' then
        BookMark := RtfBookMarkName(result) else begin
        inc(ReferencePicturesIndex);
        BookMark := 'PICTURE_'+IntToStr(ReferencePicturesIndex);
      end;
      ReferencePictures.AddCSVValue(result,BookMark);
      caption := '{\*\bkmkstart '+BookMark+'}\line'+
        WR.RtfFontString(88)+'\i '+caption+'{\*\bkmkend '+BookMark+'}';
    end;
    result := result+' '+Coords; // 796x729 100%
  end;
end;

const
  PICEXTTORTF: array[0..high(VALID_PICTURES_EXT)] of string =
    ('jpeg','jpeg','png','emf');

function TProject.PictureInlined(const ButtonPicture: string; Percent: integer;
  WriteBinary: boolean): AnsiString;
var P: PChar;
    Code: integer;
    Image, caption, Coords: string;
    Map: TMemoryMap;
    w,h, Ext: integer;
    aFileName, iWidth, iHeight, content: string;
    Logo: boolean;
begin // '30%bidule.png' or '30%%FirmwareBoot' -> inlined picture
  result := '';
  P := pointer(ButtonPicture);
  if Percent=0 then begin
    Logo := false;
    val(GetNextItem(P,'%'),Percent,Code);;
    if (Code<>0) or (cardinal(Percent)>100) then
      exit;
    if P^='%' then begin
      // '%FirmwareBoot' for \graph FirmwareBoot ...
      inc(P);
      NeedGraphValues;
      Image := PictureFullLine(GraphDirName+P,caption,fGraphValues);
    end else
      Image := PictureFullLine(P,caption);
  end else begin
    logo := true;
    Image := ButtonPicture;
    PictureCaption(Image,@Coords);
    Image := Image+' '+Coords;
  end;
  if ImageSplit(Image, aFileName, iWidth, iHeight, w,h,Code,ext) then
  if Map.DoMap(aFileName) or Map.DoMap(WR.PicturePath+aFileName) then
  try
    if logo then
      percent := Code; // TProject.HeaderOnly will use default width %
    percent := (WR.Width*percent) div 100;
    h := (h*percent) div w;
    if WriteBinary then begin
      SetString(content,PAnsiChar(Map.buf),Map._size);
      content := '\bin'+IntToStr(Map._size)+' '+content;
    end else begin
      SetLength(content,Map._size*2);
      Map.ToHex(pointer(content));
    end;
    if not logo then
      result := '\qc' else
      result := '\qr';
    result := result+'{\pict\'+PICEXTTORTF[ext]+'blip\picw'+iWidth+'\pich'+iHeight+
      '\picwgoal'+IntToStr(percent)+'\pichgoal'+IntToStr(h)+
      ' '+content+'}';
    if not logo and (caption<>'') then
      result := WR.RtfFontString(50)+result+'\line '+caption+WR.RtfFontString(100);
  finally
    Map.UnMap;
  end;
end;

function TProject.GetPeopleFunction(const PeopleName, Default: string): string;
begin
  result := ValAt(People[PeopleName],0);
  if result='' then
    result := Default;
end;

function TProject.GetPeopleFunctionDescription(const PeopleName, Default: string): string;
var i: integer;
begin
  result := People[PeopleName];
  i := pos(result,',');
  if i=0 then
    result := Default else
    delete(result,1,i);
end;

{ TRisk }

{ '1,1,3,Claude Mench+Arnaud Bouchez,Service SW is safe'
  Risk: array[0..2] of integer; // (1,1,3)
  EvaluatedBy: string; // 'Claude Mench+Arnaud Bouchez'
  Comment: string;     // 'Service SW is safe' }

function TRisk.EvaluatedByDisplay: string;
// format for UI: 'Claude Mench, Arnaud Bouchez'
begin
  result := StringReplaceAll(EvaluatedBy,'+',', ');
end;

procedure TRisk.FromString(const aString: string);
var P: PChar;
    i: integer;
begin
  if aString='' then
    exit; // no update if ''
  P := pointer(aString);
  for i := 0 to high(Risk) do
    Risk[i] := StrToIntDef(GetNextItem(P),0);
  if P<>nil then begin
    EvaluatedBy := GetNextItem(P);
    if P<>nil then
      Comment := P;
  end;
end;

procedure TRisk.SetWorse(aRisk: PRisk);
var i: integer;
begin
  if aRisk=nil then begin
    fillchar(Risk,sizeof(Risk),0);
    Comment := sNotImplemented;
  end else begin
    for i := 0 to high(Risk) do
      if aRisk^.Risk[i]>Risk[i] then
        Risk[i] := aRisk^.Risk[i];
    Comment := sRiskEvaluatedFromSub;
  end;
end;

function TRisk.ToString: string;
var i: integer;
begin
  result := '';
  for i := 0 to high(Risk) do
    result := result+IntToStr(Risk[i])+',';
  result := result+EvaluatedBy+','+Comment;
end;

class function TProject.GetProgramName(const Button: string): string;
begin // 'EIA\one.pas' -> 'one.pas'
  result := ValAt(Button,1,'\');
end;

class function TProject.GetProgramSection(const Button: string): string;
begin // 'EIA\one.pas' -> 'SAD-EIA'
  result := ValAt(Button,0,'\');
  if result<>'' then
    result := 'SAD-'+result;
end;

function TProject.DocumentIndex(aSection: TSection): integer;
begin
  if aSection<>nil then
  for result := 0 to length(Document)-1 do
    if Document[result].Params=aSection then
      exit; // found
  result := -1; // not found
end;

function TProject.DocumentIndex(const aSectionName: string): integer;
begin
  if aSectionName<>'' then
  for result := 0 to length(Document)-1 do
    if SameText(Document[result].Params.SectionName,aSectionName) then
      exit; // found
  result := -1; // not found
end;

function TProject.DocumentFind(const aSectionName: string): PDocument;
var i: integer;
begin
  i := DocumentIndex(aSectionName);
  if i<0 then
    result := nil else
    result := @Document[i];
end;

function TProject.DocumentFind(aSection: TSection): PDocument;
var i: integer;
begin
  i := DocumentIndex(aSection);
  if i<0 then
    result := nil else
    result := @Document[i];
end;

{$ifdef USEPARSER}
procedure TProject.UpdateSADFileFromSource(RecreateAll: boolean);
var SAD: TProjectBrowser; // contains all [SAD-Parse].SourceFile= details
    i: integer;
begin
  SAD := TProjectBrowser.Create(self);
  try
    if RecreateAll then
      DeleteFile(SAD.CacheName); // recreate only once -> better than SAD.ForceCacheRecreateAll
    SAD.OutdatedCacheAutoRecreate := true;
    for i := 0 to high(Parse) do
      SAD.FillUnits(Parse[i],false); // false = no recreate GraphViz emf
  finally
    SAD.Free;
  end;
end;

procedure TProject.CreateExternalSAE;
var SAD: TProjectBrowser; // contains all [SAD-Parse].SourceFile= details
    i,j,k: integer;
    U,D: TStringList;
    s: string;
procedure AddItems(Items: TPasItems; const SectionName: string);
var i: integer;
begin
  if SectionName<>'' then
    D.Add(#13#10'['+SectionName+']');
  for i := 0 to Items.Count-1 do
    with TPasItem(Items[i]) do
      D.Add(Name+'='+
        StringReplaceAll(trim(RawDescription),#13#10,'|'));
end;
begin
  SAD := TProjectBrowser.Create(self);
  U := TStringList.Create;
  try
    for i := 0 to high(Parse) do
      if isTrue(Parse[i]['ExternalDescription']) then begin
        SAD.FillUnits(Parse[i],false); // false = no recreate GraphViz emf
        for j := 0 to SAD.Units.Count-1 do
          with TPasUnit(SAD.Units[j]) do
          if U.IndexOf(OutputFileName)<0 then begin
            D := TStringList.Create;
            D.Add('OutputFileName='+OutputFileName);
            D.Add('Description='+
              StringReplaceAll(trim(RawDescriptionInfo.Content),#13#10,'|'));
            AddItems(Types,'Types');
            AddItems(Constants,'Constants');
            AddItems(FuncsProcs,'Functions');
            AddItems(Variables,'Variables');
            AddItems(CIOs,'Objects');
            for k := 0 to CIOs.Count-1 do
              if CIOs[k].InheritsFrom(TPasCIO) then
              with TPasCio(CIOs[k]) do
              if (Fields.Count>0) or (Methods.Count>0) or
                 (Properties.Count>0) then begin
                AddItems(Fields,Name);
                AddItems(Methods,'');
                AddItems(Properties,'');
              end;
            U.AddObject(OutputFileName,D);
          end;
      end;
    SAD.CloseCacheSAE(true,false); // we will overwrite now the .sae file
    if U.Count=0 then
      exit; // no unit to be described in the external .sae file
    U.Sort;
    with TZipWriter.Create(SAD.CacheNameSAE) do
    try
      for i := 0 to U.Count-1 do begin
        s := TStringList(U.Objects[i]).Text;
        Add(U[i],Pointer(s),length(s),6);
      end;
    finally
      Free;
    end;
  finally
    SAD.Free;
    for i := 0 to U.Count-1 do
      U.Objects[i].Free;
    U.Free;
  end;
end;
{$endif}

procedure TProject.TestGetDescr(Test, DIDetails: TSection; TestDoc: PDocument; out docname, title: string);
begin
  docname := Test['DocName']; // 'Firmware'  -> don't use Test.DocName (<>'')
  if docname='' then begin
    docname := Test.SectionNameValue; // 'DI-4.1'
    if DIDetails=nil then
      title := docname else
      title := GetDIDisplayName(DIDetails);
  end else begin
    docname[1] := NormToUpper[docname[1]];
    title := docname;
  end;
  docname := Project['Name']+ // Header.ProjectName may have not yet been set
    ' '+TestDoc.Params.docName+' '+docname+'.doc';
end;

function TProject.GetSourceOutputFileName(const ButtonFileName: string;
  mustExists: boolean): string;
// 'IFA\Main.pas' -> 'IFA2\Main.pas'
var SADname, path, SADpath: string;
    i: integer;
    Sec: TSection;
begin
  result := ButtonFileName;
  SADname := ValAt(ButtonFileName,0,'\');
  if SADname='' then
    SADname := ValAt(ButtonFileName,0,'/');
  if SADname='' then
     SADname := ExcludeTrailingPathDelimiter(ExtractFilePath(ButtonFileName));
  if SADname='' then exit; // ='EIA','IFA','LIS'
  SADpath := IncludeTrailingPathDelimiter(ParseSAD.Params['DefaultPath']);
  if not DirectoryExists(SADPath) then
    if mustExists then
      raise Exception.CreateFmt('Impossible to access %s directory.'#13#13+
        'Please correct the following line in [SAD] header:'#13' DefaultPath=%s'#13#13'Then retry',
         [SADpath,ParseSAD.Params['DefaultPath']]) else begin
      result := '';
      exit;
     end;
  for i := 0 to high(Parse) do begin
    Sec := Parse[i]; // [SAD-IFA] ?
    if SameText(Sec.SectionNameValue,SADName) then begin
      path := Sec.ReadString('SourcePath',SADName);  // 'IFA2'
      result := IncludeTrailingPathDelimiter(path)+copy(result,length(SADName)+2,maxInt);
      // 'IFA2\Main.pas'
      if mustExists then begin
        if Sec['SourceFile']<>'' then // we must have something to parse
        if SameText(ExtractFileExt(result),'.pas') then // parse only .pas for now
          if FileExists(result) or FileExists(SADpath+result) then
            exit else begin
            result := ButtonFileName;
            if FileExists(SADpath+result) then
              exit;
          end;
        result := '';
      end;
      exit;
    end;
  end;
end;

procedure TProject.DocumentAddSection(Doc: PDocument; aSection: TSection);
var i,n: integer;
    OwnDoc: PDocument;
begin
  if Doc=nil then exit;
  i := Doc.GetSectionIndex(aSection);
  if i>=0 then exit;
  n := length(Doc.List);
  Setlength(Doc.List,n+1);
  if (Doc.Owner<>nil) and (aSection.Owner=nil) then begin
    OwnDoc := DocumentFind(Doc.Owner.SectionName);
    if OwnDoc<>nil then
      aSection.Owner := OwnDoc.GetSection(Doc.Owner.SectionName+'-'+aSection.SectionNameValue);
  end;
  Doc.List[n] := aSection;
end;

{$ifdef WITH_GRAPHVIZ}
procedure DeleteFiles(Path: string);
var SR: TSearchRec;
begin
  if Path='' then exit;
  if Path[length(Path)]<>'\' then
    Path := Path+'\';
  if FindFirst(Path+'*.*',faAnyFile-faDirectory,SR)=0 then begin
    repeat
      if SR.Name[1]<>'.' then
        DeleteFile(Path+SR.Name);
    until FindNext(SR)<>0;
    FindClose(SR);
  end;
end;

function TProject.PercFromTitle(var GraphTitle: string; const UniqueImageName: string): integer;
var k,m: integer;
begin
  result := 0;
  if (GraphTitle<>'') and (GraphTitle[length(GraphTitle)]='%') then begin
    // title can end with 90% e.g. to force this width
    k := length(GraphTitle)-1;
    m := 1;
    while (k>0) and (GraphTitle[k] in ['0'..'9']) do begin
      inc(result,(ord(GraphTitle[k])-48)*m);
      m := m*10;
      dec(k);
    end;
    while (k>0) and (GraphTitle[k]=' ') do dec(k);
    SetLength(GraphTitle,k);
  end;
  if GraphTitle='' then
    GraphTitle := UniqueImageName+' Graph';
end;

procedure TProject.UpdateSADGraphViz(const Ext: string; OnlyGraphs: boolean);
var i, j, perc: integer;
    DotV: variant;
    DestDir, GraphTitle, dot, UniqueImageName: string;
    PC: PChar;
begin
  with TProjectBrowser.Create(self) do
  try
    GraphExt := Ext;
    DestDir := FileNameDir+GraphDirName;
    if not OnlyGraphs then begin
      // create all emf from parsed SAD sections
      if DirectoryExists(DestDir) then
        DeleteFiles(DestDir);
      for i := 0 to high(Parse) do
        FillUnits(Parse[i],true); // true = force recreate GraphViz emf files
    end;
    if GetWingraphviz(DotV) then
    try
      LoadGraphValues;
      // recreate all graphs corresponding to \Graph commands
      for i := 0 to FData.Sections.Count-1 do
      with FData.Sections[i] do begin
        j := 0;
        while j<Lines.Count do begin
          PC := pointer(Lines[j]);
          inc(j);
          if PC=nil then continue;
          if not IdemPChar(PC,'\GRAPH') then continue;
          { - first line has format: \Graph UniqueImageName [Title]
            - then following lines are expected to be (until void line):
               either a .dot normal line
                 (e.g. "rankdir=LR; node [shape = circle];")
               either "\From Text\To Text[\Label between both]" }
          inc(PC,6); while PC^=' ' do inc(PC);
          UniqueImageName := GetNextItemTrimed(PC,' ');
          if UniqueImageName='' then continue; // we need an UniqueImageName
          GraphTitle := trim(PC);
          perc := PercFromTitle(GraphTitle,UniqueImageName);
          Dot := WingraphvizFromText(UniqueImageName,Lines,j);
          if WingraphvizCreateFile(DotV, UniqueImageName, DestDir, '.emf',
             dot, GraphDirName, GraphTitle, GraphValues, perc) then
            GraphValuesModified := true;
        end;
      end;
    finally
      DotV := null; // release memory
    end;
  finally
    Free;
  end;
end;
{$endif}

procedure TProject.CreateRTFTable(const TableKind: string);
var SWRS: PDocument;
    layout: string;
    Descri,  // if <> '' -> must be added to SCRS doc
    Request: array of string; // Request=SCR #65,EIA 2.0+IFA 2.0,Low
    DescriCount: integer;
function InitSWRS: boolean;
begin
  if Doc.Params.Owner=nil then
    SWRS := nil else
    SWRS := DocumentFind(Doc.Params.Owner.SectionName);
  if SWRS=nil then begin
    WR.RtfBig('No Owner (DI/SRS) section found -> impossible to produce '+TableKind);
    result := false;
    end else begin
    layout := '\ql '+format(sDescriptionN,[sSpecification]); // 'SWRS Description'
    result := true
  end;
end;
function InitDescri: boolean;
var i: integer;
    item, desc: string;
begin
  result := InitSWRS;
  if not result then exit;
  SetLength(Descri,length(SWRS.List));
  SetLength(Request,length(SWRS.List));
  desc := '';
  DescriCount := 0;
  for i := 0 to high(SWRS.List) do
  with SWRS.List[i] do begin
    if Risk='' then continue;
{    if Owner<>nil then
    if (Root.SectionName=SectionNameValueWithDI) and (i<high(SWRS.List))
      and (SWRS.List[i+1].Owner=Owner) then continue; }
    Request[i] := ReadString('Request',Owner['Request']);
    if Request[i]='' then continue;
    if ParseTest<>nil then begin
      item := Data[ParseTest.Params.SectionName+'-'+SectionNameValueWithDI]['Source'];
      if item<>'' then
        desc := StringReplaceAll(item,',',', ');
    end;
    Descri[i] := TrimLastPeriod(ShortDescription(desc));
    inc(DescriCount);
  end;
end;
var i,j, n: integer;
    param, YesNo, desc, testdocname, pass: string;
    P: PChar;
function testdocnameGet(const SectionNameValue: string): integer;
begin
  result := ParseTest.GetSectionNameValueIndex(SectionNameValue);
  if result<0 then exit;
  desc := ParseTest.List[result]['Description'];
  if desc<>'' then // new test document begin with Description=..
    TestGetDescr(ParseTest.List[result],nil,ParseTest,testdocname,layout) else
    result := -1;
end;
function GetNextRisk: string;
begin
  result := GetNextItem(P);
  if result='0' then
    result := '';
end;
var aSec: TSection;
    aDoc: PDocument;
    line, isoName, search, CSVSections, currentName: string;
    TNum: TSection;
    TCaption: TSection;
    MainDone: boolean;
    ColWidths: TIntegerDynArray;
procedure AddImplements(aSec: TSection);
var P: PChar;
    line, isoName, text: string;
begin
  Data.ReadOpen(aSec.SectionName,false);
  while not Data.ReadEof do begin
    line := Data.ReadLine;
    if not IdemPChar(pointer(line),pointer(search)) then
      continue;
    P := @line[length(search)];  // '5.1 [blabla bloblo][\DocumentName]'
    while P^=' ' do inc(P);
    isoName := GetNextItem(P,' '); // '5.1'
    if Pos('\',isoName)>0 then
      continue; // ignore '5.1\Document' -> should be '5.1 \Document'
    text := GetNextItemTrimed(P,'\');
    if P=nil then begin // '5.1 [blabla bloblo]'
      AddReferenceDocument(aDoc.Params);
      TNum.AddCSVValue(isoName,aSec.SectionName);
    end else // '5.1 blabla bloblo\DocumentName'
      TNum.AddCSVValue(isoName,'\'+StringReplaceAll(trim(P),',','|')); // CSV ready
    if text<>'' then
      TCaption.AddCSVValue(isoName,text,true);
  end;
end;
begin
  i := pos('=',TableKind);
  if i=0 then begin
    param := '';
    desc := TableKind;
  end else begin
    desc := copy(TableKind,1,i-1);
    param := copy(TableKind,i+1,maxInt);
  end;
  case CSVIndexOf('SoftwareChanges,TraceabilityMatrix,'+
    'NewFeatures,BugFixes,Tests,Documents,DI,Implements',desc) of
  7: begin // \TableImplements=ISO-LIS Simulator
    // -> add '\implements ISO 5.1 [blabla][\DocName]' inside any document section
    i := pos('-',param);
    if i>0 then begin
      desc := copy(param,i+1,maxInt); // desc='LIS Simulator' will be ignored
      param := copy(param,1,i-1);
    end else
      desc := '';
    search := '\IMPLEMENTS '+UpperCase(trim(param))+' ';
    Data.ReadPush;
    TNum := TSection.Create('TableImplementsNum');
    TCaption := TSection.Create('TableImplementsCaption');
    try
      for i := 0 to high(Document) do begin
        // 1. create a list of all '\implements' items, and corresponding sections
        aDoc := @Document[i];
        if (desc<>'') and (CSVIndexOf(desc,aDoc.Params.SectionName)>=0) then
          Continue; // \TableImplements=ISO-LIS Simulator -> ignore [LIS Simulator]
        MainDone := False;
        for j := 0 to Data.Sections.Count-1 do
        if Data.Sections[j].SectionNameKind=aDoc.Params.SectionName then begin
          AddImplements(Data.Sections[j]); // sub paragraphs
          if Data.Sections[j]=aDoc.Params then
            MainDone := True;
        end;
        if not MainDone then // add body only once (in List[] if doc has no Order=)
          AddImplements(aDoc.Params); // main body
      end;
      if TNum.Lines.Count<>0 then begin
        // 2. create table, sorted by numbers (not by section/document)
        TNum.Lines.CustomSort(SectionNumbersCompare); // will sort by '5.1'
        WR.RtfColsPercent([12,34,54],true,true,false,'\trhdr');
        WR.RtfRow(['\qc\b '+sItemSharp,'\qj '+sDescription,sRelatedDocuments+'\b0'],true);
        WR.RtfColsPercent([12,34,54],true,true,false,'\trkeep');
        for i := 0 to TNum.Lines.Count-1 do begin
          line := TNum.Lines[i];
          TNum.SplitNameValue(line,isoName,CSVSections);
          P := pointer(CSVSections);
          if P=nil then continue;
          line := '';
          repeat
            currentName := GetNextItem(P);
            if currentName='' then continue;
            if currentName[1]='\' then begin
              Delete(currentName,1,1); // '5.1 blabla bloblo\DocumentName'
              currentName := StringReplaceAll(currentName,'|',','); // CSV ready
              if line<>'' then
                line := line+'\line '+currentName else
                line := currentName;
              continue;
            end;
            aSec := Data[currentName]; // '5.1 [blabla bloblo]' -> from section
            if aSec=nil then continue;
            aDoc := DocumentFind(aSec.SectionNameKind);
            if aDoc=nil then continue;
            if line<>'' then
              line := line+'\line ';
            currentName := aDoc.Params.ReadString('Name','');
            if currentName<>'' then
              line := line+currentName+' ('+aDoc.Params.ItemName+')' else
              line := line+aDoc.Params.ItemName;
            if aSec.SectionNameValue<>aSec.SectionName then // not in aDoc.Params
              line := line+' # '+aSec.SectionNameValue;
          until P=nil;
          WR.RtfRow(['\qc{\b '+isoName+'}','\ql{\fs20 '+TCaption[isoName]+'}',
            '{\fs20 '+line+'}']);
        end;
        WR.RtfColsEnd.RtfPar;
      end;
    finally
      Data.ReadPop;
      TNum.Free;
      TCaption.Free;
    end;
  end;
  6: begin // \TableDI=6.3.1.2,6.3.1.3
    P := pointer(param);
    WR.RtfColsPercent([18,82],true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b '+sItemSharp, '\qc '+sDescription+'\b0'],true);
    WR.RtfColsPercent([18,82],true,true,false,'\trkeep');
    while P<>nil do begin
      i := DI.GetSectionIndex('DI-'+GetNextItem(P));
      if i>=0 then
        with DI.List[i] do
          WR.RtfRow(['\qc '+SectionNameValueWithDI,'\ql '+Value['Ident']]);
    end;
    WR.RtfColsEnd.RtfPar;
  end;
  0: begin // \TableSoftwareChanges (for SCRS)
    if not InitDescri then exit;
    WR.RtfColsPercent([7,40,15,5,5,5,12,10],true,true,false,'\trhdr');
    desc := sRiskShort;
    P := pointer(desc); // 'Sev.,Pro.,Occ.,Management approval,Approved';
    WR.RtfRow(['\qc\b '+sItemSharp, layout,'\qc '+sRequest,
      GetNextItem(P),GetNextItem(P),GetNextItem(P),GetNextItem(P),
      GetNextItem(P)+'\b0'],true);
    WR.RtfColsPercent([7,40,15,5,5,5,12,10],true,true,false,'\trkeep');
    n := 1;
    YesNo := Doc.Params.ReadString('YesNo',sYesNo);
    for i := 0 to high(SWRS.List) do
    with SWRS.List[i] do
    if Descri[i]<>'' then begin
      P := pointer(Risk);
      WR.RtfRow(['\qc\b '+IntToStr(n)+'\b0', //WR.RtfGoodSized(SectionNameValue)
        '\ql '+Descri[i],'\qc '+ValAt(Request[i],0),
        GetNextRisk, GetNextRisk, GetNextRisk,YesNo,YesNo]);
      inc(n);
    end;
    WR.RtfColsEnd.RtfPar;
  end;
  1: begin // \TableTraceabilityMatrix (for SCRS)
    if not InitDescri then exit;
    if ParseSDD=nil then
      desc := 'SDD' else
      desc := ParseSDD.Params.ItemName;
    if DescriCount>99 then
      i := 1 else
      i := 0;
    WR.RtfColsPercent([4+i,35-i,11,11,40],true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b #', layout, '\qc '+SWRS.Params.ItemName+' #',
      desc+' #', sUnitTestingProtocol+'\b0'],true);
    WR.RtfColsPercent([4+i,35-i,11,11,40],true,true,false,'\trkeep');
    testdocname := '';
    n := 1;
    for i := 0 to high(Descri) do
    if Descri[i]<>'' then
    with SWRS.List[i] do begin
      if ParseTest<>nil then
        testdocnameGet(SectionNameValueWithDI);
      WR.RtfRow(['\qc{\b '+IntToStr(n)+'}'+WR.RtfFontString(91),
        '\ql '+Descri[i],'\qc '+SectionNameValueWithDI, SectionNameValueWithDI,
        testdocname+WR.RtfFontString(100)]);
      inc(n);
    end;
    WR.RtfColsEnd.RtfPar;
  end;
  2: begin // \TableNewFeatures (for Release Notes)
    if not InitDescri then exit;
    CSVToIntegers(param,ColWidths);
    if length(ColWidths)<>3 then begin
      SetLength(ColWidths,3);
      ColWidths[0] := 18;
      ColWidths[1] := 62;
      ColWidths[2] := 20;
    end;
    WR.RtfColsPercent(ColWidths,true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b '+sItemSharp, layout,'\qc '+sRequest+'\b0'],true);
    WR.RtfColsPercent(ColWidths,true,true,false,'\trkeep');
    // Request=SCR #65
    for i := 0 to high(Descri) do
    if (Descri[i]<>'') and
       (pos(',',Request[i])=0) then
      WR.RtfRow(['\qc '+SWRS.List[i].SectionNameValueWithDI,
        '\ql '+Descri[i],'\qc '+ValAt(Request[i],0)]);
    WR.RtfColsEnd.RtfPar;
  end;
  3: begin // \TableBugFixes (for Release Notes)
    if not InitDescri then exit;
    CSVToIntegers(param,ColWidths);
    if length(ColWidths)<>5 then begin
      SetLength(ColWidths,5);
      ColWidths[0] := 17;
      ColWidths[1] := 10;
      ColWidths[2] := 12;
      ColWidths[3] := 12;
      ColWidths[4] := 49;
    end;
    WR.RtfColsPercent(ColWidths,true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b '+sRequest,sFoundIn,ValAt(sRiskDef,0),
      sItemSharp,layout+'\b0'], true);
    WR.RtfColsPercent(ColWidths,true,true,false,'\trkeep');
    WR.RtfFont(91);
    for i := 0 to high(Descri) do
    if (Descri[i]<>'') and
       (pos(',',Request[i])>0) then // Request=SCR #65,EIA 2.0+IFA 2.0,Low
      WR.RtfRow(['\qc '+ValAt(Request[i],0), // Request
        StringReplaceAll(ValAt(Request[i],1),'+','\line '), // Found in
        ValAt(Request[i],2), // Severity
        SWRS.List[i].SectionNameValueWithDI, // Item #
        '\ql '+Descri[i]]); // Description
    WR.RtfFont(100);
    WR.RtfColsEnd.RtfPar;
  end;
  4: begin // \TableTests[=October 16, 2008] (for Release Notes)
    if ParseTest=nil then exit;
    SWRS := DocumentFind(ParseTest.Order.SectionName);
    if SWRS=nil then exit;
    WR.RtfColsPercent([8,58,22,12],true,true,false,'\trhdr');
    pass := sPassFail;
    WR.RtfRow(['\qc\b #', sUnitTestingProtocol, sDateTested, pass+'\b0'],true);
    pass := ValAt(pass,0,' '); // 'Pass' by default ;)
    WR.RtfColsPercent([8,58,22,12],true,true,false,'\trkeep');
    n := 1;
    for i := 0 to high(SWRS.List) do
    with SWRS.List[i] do
    if testdocnameGet(SectionNameValueWithDI)>=0 then begin // new test protocol
      WR.RtfRow(['\qc{\b '+IntToStr(n)+'}',
        WR.RtfFontString(91)+copy(testdocname,1,LastDelimiter('.',testdocname)-1),
        param+WR.RtfFontString(100), pass]);
      inc(n);
    end;
    WR.RtfColsEnd.RtfPar;
  end;
  5: begin // \TableDocuments[=DI,SRS,SDD,SAD]
    WR.RtfColsPercent([34,5,15,12,34],true,true,false,'\trhdr');
    WR.RtfRow(['\qc\b '+sTitle,sRev,sDate,sRef,sDocumentName+'\b0'],true);
    WR.RtfColsPercent([34,5,15,12,34],true,true,false,'\trkeep');
    for i := 0 to high(Document) do
    with Document[i] do
    if (Params<>Doc.Params) and (Params['DocByDescription']='') then
    if (param='') or CSVContains(param,Params.SectionName) then
      WR.RtfRow(['\ql '+Params['Name'],'\qc '+Params['Revision'],
        '{'+WR.RtfFontString(91)+Params.RevisionDate+'}',Params.ItemName,
        '\ql{'+WR.RtfFontString(91)+Project.ReadString('DocName',Project['Name'])+
        ' '+Params.DocName+'.doc}']);
    WR.RtfColsEnd.RtfPar;
  end;
  -1: begin
    aDoc := DocumentFind(desc); // try \TableRK=1,2,3,4,5,6
    if (aDoc=nil) or (param='') then
      WR.RtfText.AddRtfContent('Unkwnown command: \\Table'+TableKind).RtfPar else begin
      P := pointer(param);
      WR.RtfColsPercent([18,82],true,true,false,'\trhdr');
      WR.RtfRow(['\qc\b '+sItemSharp, '\qc '+sDescription+'\b0'],true);
      WR.RtfColsPercent([18,82],true,true,false,'\trkeep');
      while P<>nil do begin
        i := aDoc.GetSectionIndex(desc+'-'+GetNextItem(P)); // get e.g. [RK-1]
        if i>=0 then
          with aDoc.List[i] do
            WR.RtfRow(['\qc '+SectionNameValueWithDI,'\ql '+Description]);
      end;
      WR.RtfColsEnd.RtfPar;
    end;
  end;
  end;
end;

procedure TProject.NeedGraphValues;
begin
  if fGraphValues=nil then begin // we need our copy of the GraphValues file
    fGraphValues := TSection.Create('GraphValues');
    fGraphValues.LoadFromFile(FileNameDir+GraphDirName+'GraphValues.ini');
  end;
end;

procedure TProject.ExportAsHtml;
begin

end;


{ TDocument }

function TDocument.GetParentIndex(Index: integer): integer;
var Level: integer;
begin
  if Index<length(List) then begin
    Level := List[Index].Level;
    for result := Index-1 downto 0 do
      if List[result].Level<Level then
        exit;
  end;
  result := -1;
end;

function TDocument.GetPropertyFromParent(Index: integer;
  const aName, aDefault: string): string;
begin
  if Index<length(List) then
    repeat
      result := List[Index][aName];
      if result<>'' then
        exit; // property found
      if Index=0 then
        break; // return default
      Index := GetParentIndex(Index);
    until Index<0;
  result := aDefault;
end;

function TDocument.GetSectionIndex(aSection: TSection): integer;
begin
  for result := 0 to length(List)-1 do
    if List[result]=aSection then
      exit;
  result := -1;
end;

function TDocument.GetSection(const aSectionName: string): TSection;
var i: integer;
begin
  for i := 0 to length(List)-1 do
    if SameText(List[i].SectionName,aSectionName) then begin
      result := List[i];
      exit;
    end;
  result := nil;
end;

function TDocument.GetSectionIndex(const aSectionName: string): integer;
begin
  for result := 0 to length(List)-1 do
    if SameText(List[result].SectionName,aSectionName) then
      exit;
  result := -1;
end;

function TDocument.GetSectionNameValueIndex(const aSectionNameValue: string): integer;
begin
  result := GetSectionIndex(Params.SectionName+'-'+aSectionNameValue);
end;

const
  hexChars: array[0..15] of Char = '0123456789ABCDEF';

function IntToHex(Value: cardinal): string;
begin
  if Value=0 then
    result := '0' else begin
    result := '';
    while (Value>0) do begin
      result := HexChars[(Value shr 4) and $F]+HexChars[Value and $F]+result;
      Value := Value shr 8;
    end;
    if result[1]='0' then
      delete(result,1,1);
  end;
end;

procedure InitLang;
// init RISKDEFS[] with proper sRiskDef resourcestring value
{.$define TESTSTRVAL}
var P: PChar;
    s: string;
    i: integer;
{$ifdef TESTSTRVAL}
    j,err: integer; k,l: Int64; t: string; d,e: double;
function FloatToStr(Value: Extended): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0));
end;
function SameValue(const A, B: Double; DoublePrec: double = 1E-12): Boolean;
var AbsA,AbsB: double;
begin // faster than the Math unit version
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA<AbsB then
    AbsA := AbsA*DoublePrec else
    AbsA := AbsB*DoublePrec; // AbsA := Min(Abs(A),Abs(B))*DoublePrec
  // AbsA is the allowed Epsilon value
  if AbsA<DoublePrec then
    Result := Abs(A-B)<=DoublePrec else
    Result := Abs(A-B)<=AbsA;
end;
{$endif}
begin
  s := sRiskDef;
  P := pointer(s);
  for i := 0 to high(TRiskDef) do
  with RISKDEFS[i] do begin
    Name := GetNextItem(P);
    Description := GetNextItem(P);
    Level[1] := GetNextItem(P);
    Level[2] := GetNextItem(P);
    Level[3] := GetNextItem(P);
  end;
{$ifdef TESTSTRVAL}
  Randomize;
  for i := 1 to 1000000 do begin
    j := Random(maxInt)-Random(maxInt);
    k := Int64(j)*Random(MaxInt);
    str(k,s);
    assert(IntToStr(k)=s);
    assert(format('%d',[k])=s);
    val(s,l,err);
    assert((err=0)and(l=k));
    str(j,s);
    assert(IntToStr(j)=s);
    assert(format('%d',[j])=s);
    s := format('%x',[j]);
    t := IntToHex(j);
    assert(t=s);
    d := Random*1E-17-Random*1E-9;
    str(d,s);
    val(s,e,j);
    assert(SameValue(e,d)); // test str()
    t := FloatToStr(d);
    val(t,e,err);
    assert(SameValue(e,d)); // test FloatToStr()
  end;
{$endif}
end;


initialization
  InitLang; // init RISKDEFS[] with default english

finalization

end.
