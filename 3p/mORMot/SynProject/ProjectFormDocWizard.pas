/// Documentation editor full Wizard form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectFormDocWizard;

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

{$define USEGDIPLUSFORIMAGES}
// if defined, GDI+ library will be used for reading jpeg and png images
// (requires Windows XP and later - or GdiPlus.dll in program folder)

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls, SynMemoEx,
  ProjectMemoExSyntax, ProjectEditor, ProjectTypes,
{$ifdef USEGDIPLUSFORIMAGES}
  SynGdiPlus,
{$endif}
  Menus, ProjectSections, ProjectRTF, ProjectFrameRisk;

type
  TProjectDocWizard = class(TForm)
    BtnPrev: TBitBtn;
    BtnNext: TBitBtn;
    BtnCancel: TBitBtn;
    PanelTop: TPanel;
    Pages: TPanel;
    BtnWelcome: TBitBtn;
    BtnSave: TBitBtn;
    PopupMenu: TPopupMenu;
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnWelcomeClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CreateEditX, CreateEditY, CreateEditW, CreateEditTop, CreateEditH,
    LastCreateEditY,
    CreateComponentFirstIndex,
    CreateListComponentIndex: integer;
    CreateEditSec: TSection;
    SubDirDefPath: string;
    fData: TSectionsStorage; // =nil -> new release wizard
    fPeopleText: string;
    fPageCount: integer;
    fActivePageIndex: integer;
    fPageTitle: string;
    LabelVersion: TLabel;
    fDefault: TSection;
    FTitle: string; // contains [Default] of default.ini (from ProjectRes.RES)
    FDefPROLines: string;
    procedure SetActivePageIndex(const Value: integer);
    procedure SetData(const Value: TSectionsStorage);
    procedure SetfPeopleText;
    procedure CreateEnoughPlace(Height: integer=48);
    procedure WelcomeButtonClick(Sender: TObject);
    procedure ImportClick(Sender: TObject);
    procedure VersionMgmentClick(Sender: TObject);
    function VersionLabel(Sec: TSection): string;
    procedure CreateListClick(Sender: TObject);
    procedure CreatePeoplesMenu(aEdit: TLabeledEdit; P: PChar);
    procedure CreateDirectoryClick(Sender: TObject);
    procedure CreatePeoplesClick(Sender: TObject);
    procedure CreateCommitClick(Sender: TObject);
    procedure MemoSetText(Memo: TMemoEx; const aValueName: string);
    function UpdateDataFromActivePage(dontAsk: boolean): boolean;
    procedure SetTitle(const Value: string);
    procedure CreateComponentsMessage(var Message: TMessage); message WM_USER;
    procedure CreateDocFromResource;
  public
    DVSFileName: string; // <>'' if BtnSaveClick() has created a new project
    DVSParams: TSection; // <>nil before Show with Data=nil -> Commits [Params] section
    procedure CreateEditInit(const SecName: string; AddDocumentFields: boolean=false;
      NoItem: boolean=false; NoRevision: boolean=false; NoReviewOrApprov: boolean=false);
    function CreateEdit(const aValueName, aCaption: string): TLabeledEdit;
    function CreateCombo(const aValueName, aCaption, aValues: string): TComboBox;
    function CreateMemo(const aValueName, aCaption: string; BodySyntax: boolean;
       MemoHeight: integer = 240): TMemoEx;
    function CreateList: TListBox;
    function CreateOption(const aValueName, aCaption: string): TCheckBox;
    function CreateButton(const aCaption: string; OnClick: TNotifyEvent; Width: integer = 0): TButton;
    function CreatePeoples(const aValueName, aCaption: string): TLabeledEdit;
    function CreateDirectory(const aValueName, aCaption: string; SubDir: boolean=false): TLabeledEdit;
    function CreateLabel(const aCaption: string): TLabel;
    function CreateRisk(const aValueName, aCaption: string): TFrameRisk;
    procedure CreateVersion;
    function DoCreateReleaseNew: boolean;
    property Data: TSectionsStorage read fData write SetData; // Data=nil -> new Release
    property ActivePageIndex: integer read fActivePageIndex write SetActivePageIndex;
    property PageCount: integer read fPageCount;
    property Title: string read FTitle write SetTitle;
  end;

var
  ProjectDocWizard: TProjectDocWizard;


resourcestring
  sDocumentPurpose = 'Document purpose (must begin with a verb - "Create description..")';
  sDocDisplayName = 'Document Name for display (same as Document name if left blank)';
  sItemName = 'Short Item Name for display';
  sDocNameForDoc = 'Document Name for .doc creation';
  sDocNameForDocNotItem = 'Document Name for .doc (used instead of Item Name)';
  sClickToVersionN = 'Click to manage version for this "%s" document';
  sRightClickToAddName = 'Right click on the field to display add menu';
  sReleaseVersion = 'Release Version number (2.1 e.g.)';
  sNoRiskDisplay = 'Text to display in case of no Risk assessment';
  sOldWordOpen = 'Word 97/2000 compatibility Hack';
  sDestDir = 'Destination directory for all generated documents';
  sWarningMessage = 'Warning message';
  sWarningOvewriteExisting = 'Warning:'#13#10+
    'This will rewrite the file content according to the current source code '+
    'state'#13#10'(your hand-made descriptions should remain, '+
    'if the item names didn''t change).'#13#10#13#10+
    'Do you want to continue?'; 
  sPeopleList = 'People list (Name=Function,Detailled function)';
  sDefaultPreparedBy = '"Prepared by" default value';
  sDILayout = 'Design Input order (lines beginning with : are main titles)';
  sPreparedByDefault = 'Prepared by - blank if default "%s"';
  sRequest = 'Associated Request ("SCR #23") - with version and severity for bug fix ("SCR #52,EIA 2.0+IFA 2.0,Low")';
  sInputLevel = 'Prority Level ("Must have" if left blank)';
  sDescriptionIfNoBody = 'Item Description (if no text in body below)';
  sShortItemName = 'Short Item Name for display';
  sItemBody = 'Item Body (long description)';
  sSourceModules = 'Source SAD-* project executable modules, separated by ","';
  sSourceModulesHint = 'The SAD document will look for [SAD-*] sections and for @*\filename.ext@';
  sDefaultPath = 'Global default directory (trimmed in the text)';
  sRequirementsForTest = 'Requirements for Test (blank if same as Display Name)';
  sSourcePathN = 'Source Path to search in (default parent is "%s")';
  sSourceFiles = 'Delphi Source Files to be parsed (separated by ";")';
  sIncludePath = 'Include Path for Delphi parser (separated by ";")';
  sDirectives = 'Compilation Directives for Delphi parser';
  sGraphIgnore = 'Units names without graphs (separated by ",")';
  sExternalDescription = 'External code description in separate .sae file'; 
  sModuleDescription = 'Module description';
  sExcludeFromSummary = 'If checked, the V&&V plan won''t show this entry';
  sTestDescription = 'Test Description (must begin with a verb), leave blank if no new document';
  sTestDocName = '"Test *" document name to use instead of section name';
  sTestProtocol = 'Test protocol';
  sIntroductionTextN = '%s introduction text';
  sContentN = '%s content';
  sSoftwareVersion = 'Software version for this release';
  sSoftwareHistory = 'Product Software History';
  sKnownIssues = 'Known Issues';
  sSOUP = 'SOUP items';
  sDocFirstPart = 'Document first part';
  sRightClickToAdd = '(right click to add)';
  sStepNumber = '(step %d/%d)';
  sFieldChangeUpdateQuery = 'At least one field has been updated.'#13+
      'Do you want to save the changes?';
  sSectionNewCreateQuery = 'The [%s] section doesn''t exist yet.'#13+
      'Do you want to create it now?';
  sChangeDirectory = 'Browse for folder';
  sImportValues = 'Import values from another release';
  sGlobalCompany = 'Main Company Name (local Company Name will be provided later)';
  sProduct = 'Full Product Name, as inserted into the documentation';
  sProductPath = 'Shorter Product Name for path/file creation';
  sReleaseName = 'Release Name / Commits Name for this Product';
  sDefaultAuthor = 'Default Author name of this documentation';
  sDefaultAuthorFunction = 'Default Author responsability ("function,function details")';
  sSRSORDER = 'SWRS has its own sub items (not follow strictly the DI order)';
  sDefaultPathRelease = 'Root directory for all files (source and documentation)';
  sNoDocumentation = 'Release has NO Design Input: don''t create .pro documentation';
  sDocumentsPath = '(Sub) Directory containing all documentation related files';
//  sSourcePathPossible = 'Sub directory containing the source files (can be blank)';
  sBackupPathLocal = 'Local directory for backups';
  sBackupPathDistant = 'Distant directory for backups (can be blank)';
  sCommitIgnorePath = 'Directory names to ignore (separated by ",")';
  sCommitIgnoreFile = 'File extension begining to ignore (".~" will ignore *.~* files)';
  sCommitFilterDefault = 'File extensions to scan for commit (.pas.php..)';
  sCommitFileNameNN = '%s file name (root directory is "%s") - can be blank';
  sCommitUpdate = 'Commit module ("Display Name;Path;Filter,Filter,..") #%d';
  sCommitEditionN = 'Commit module edition #%d'; 
  sDoCreateReleaseNewQueryN = 'Do you want to create a new "%s" release,'#13+
    'with all associated files and directories?';
  sCreateDirectoryQueryN = 'The "%s" directory doesn''t exist yet.'#13#13+
    'Do you want to force its creation?';
  sOverwriteFileQueryN = 'The "%s" file already exists.'#13#13+
    'Do you want to overwrite it (and loose its contents)?';
  sFileCreateNewQueryN = 'The file "%s" doesn''t exist yet.'#13#13+
    'Do you want to create a new one now?';
  sProjectDocWizardTitles =
    'Direct access to a page,General,Design Input,Design Input details,'+
    'Design FMEA,Design FMEA details,SWRS,SWRS details,'+
    'Risk Assessment,SDD,SDD details,Software Architecture,Software Architecture details,'+
    'Software Architecture modules,V&V plan,Test protocols,Test protocols report,'+
    'Tests details,Versions,Issues,SCRS,Release Notes';
  sProjectNewReleaseWizardTitles =
    'Creating a new project,Software Commits Settings,Creating a new documentation';



function LoadTextFromResource(const ResName: string): string;
// load from embedded resource (not zip but as normal text resource)
// default := LoadTextFromResource('default');

function MessageDlgFmt(const Msg: string; const Args: array of const;
  DlgType: TMsgDlgType = mtConfirmation; Buttons: TMsgDlgButtons = mbYesNoCancel;
  HelpCtx: Longint = 0): Integer;
// result := MessageDlg(Format(Msg,Args), mtConfirmation, mbYesNoCancel, 0);

procedure MessageErrFmt(const Format, Value: string);


implementation

uses
  SysConst,
  ProjectCommons,
  ProjectVersionSCR, // for notVoid()
  ProjectVersioning,
  ProjectDiff,
  SynZip, // TZipRead to read embedded ProjectRes.zip
  ProjectEditorRelease, ProjectVersionMain, ProjectTrackerLogin,
  ProjectEditorCommit;

{$R *.dfm}

function MessageDlgFmt(const Msg: string; const Args: array of const;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  result := MessageDlg(Format(Msg,Args), DlgType, Buttons, HelpCtx);
end;

procedure MessageErrFmt(const Format, Value: string);
begin
  MessageDlgFmt(Format, [Value], mtError, [mbOK], 0);
end;

{$R ProjectRes.RES}
// contains wizard2.png, default.ini, and the embedded ProjectRes.zip

function LoadTextFromResource(const ResName: string): string;
// load from embedded resource (not zip but as normal text resource)
// default := LoadTextFromResource('default');
begin
  with TResourceStream.Create(HInstance,ResName,'TXT') do
  try
    SetString(result,PAnsiChar(Memory),Size);
  finally
    Free;
  end;
end;

function NameValidate(const aName: string): string;
// make aName compatible with TComponent.Name (['0'..'9','A'..'Z','_'])
var i: integer;
begin
  result := aName;
  for i := length(result) downto 1 do begin
    case result[i] of
    '_': insert('_',result,i+1);
    '-': insert('A',result,i+1);
    '.': insert('B',result,i+1);
    ' ': insert('C',result,i+1);
    'a'..'z','A'..'Z','0'..'9': continue;
    end;
    result[i] := '_';
  end;
end;

function NameUnValidate(const aNameValidated: string): string;
// make aName compatible with TComponent.Name (['0'..'9','A'..'Z','_'])
var i: integer;
begin
  result := aNameValidated;
  for i := length(result)-1 downto 1 do
    if result[i]='_' then begin
      delete(result,i,1);
      case result[i] of
      'A': result[i] := '-';
      'B': result[i] := '.';
      'C': result[i] := ' ';
      end;
    end;
end;

procedure TProjectDocWizard.BtnNextClick(Sender: TObject);
begin
  ActivePageIndex := ActivePageIndex+1;
end;

procedure TProjectDocWizard.BtnPrevClick(Sender: TObject);
begin
  ActivePageIndex := ActivePageIndex-1;
end;

procedure TProjectDocWizard.FormShow(Sender: TObject);
var i: integer;
begin
  if CreateComponentFirstIndex>0 then
    for i := ComponentCount-1 downto CreateComponentFirstIndex do
      Components[i].Free;
  CreateComponentFirstIndex := 0; // force recreate
  DVSFileName := '';
  BtnSave.Enabled := DVSParams<>nil;
  CreateEditSec := nil;
  BtnWelcome.Visible := Data<>nil;
  fActivePageIndex := 0; // UpdateDataFromActivePage will accept following line
  ActivePageIndex := 0;
end;

function TProjectDocWizard.CreateEdit(const aValueName, aCaption: string): TLabeledEdit;
begin
  CreateEnoughPlace;
  result := TLabeledEdit.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY;
  result.Width := CreateEditW;
  result.LabelPosition := lpAbove;
  result.EditLabel.Caption := aCaption+':';
  if result.EditLabel.Width>CreateEditW then begin
    result.EditLabel.Hint := aCaption;
    result.EditLabel.ShowHint := true;
  end;
  result.Name := NameValidate(aValueName); // Data[Name] := .. to update modified value
  result.Text := CreateEditSec[aValueName];
  inc(CreateEditY,48);
end;

function TProjectDocWizard.CreateRisk(const aValueName, aCaption: string): TFrameRisk;
begin
  result := TFrameRisk.Create(self);
  result.Parent := Pages;
  CreateEnoughPlace(result.GroupBoxRisk.Height);
  result.Left := CreateEditX;
  result.Top := CreateEditY-16;
  if CreateEditW>result.GroupBoxRisk.Width then
    result.Width := CreateEditW;
  result.GroupBoxRisk.Width := CreateEditW;
  result.Name := NameValidate(aValueName); // Data[Name] := .. to update modified value
  CreatePeoplesMenu(result.LabeledEditEvaluatedBy,pointer(fPeopleText));
  inc(CreateEditY,result.GroupBoxRisk.Height);
end;

function TProjectDocWizard.CreateCombo(const aValueName, aCaption, aValues: string): TComboBox;
var i: integer;
    s: string;
begin
  CreateEnoughPlace;
  with TLabel.Create(self) do begin
    Parent := Pages;
    Left := CreateEditX;
    Top := CreateEditY-16;
    Caption := aCaption+':';
  end;
  result := TComboBox.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY;
  result.Width := CreateEditW;
  result.Style := csDropDownList; // user may not enter manually a value
  result.Items.Text := aValues;
  s := CreateEditSec[aValueName];
  i := result.Items.IndexOf(s);
  if i>=0 then
    result.ItemIndex := i else
    result.Text := s;
  result.Name := NameValidate(aValueName); // Data[Name] := .. to update modified value
  inc(CreateEditY,48);
end;

function TProjectDocWizard.CreatePeoples(const aValueName, aCaption: string): TLabeledEdit;
begin
  result := CreateEdit(aValueName,aCaption+' '+sRightClickToAdd);
  CreatePeoplesMenu(result,pointer(fPeopleText));
end;

function TProjectDocWizard.CreateDirectory(const aValueName, aCaption: string; SubDir: boolean): TLabeledEdit;
begin
  result := CreateEdit(aValueName,aCaption);
  result.Width := result.Width-48;
  with TButton.Create(self) do begin
    Parent := Pages;
    Left := result.Left+CreateEditW-(48-8);
    Top := result.Top;
    Height := result.Height;
    Width := 48-8;
    Caption := '...';
    Hint := sChangeDirectory;
    ShowHint := true;
    OnClick := CreateDirectoryClick;
    Tag := integer(SubDir);
    Name := result.Name+'2'; // TLabeledEdit.Name=copy(Name,1,length(Name)-1)
  end;
end;

procedure TProjectDocWizard.CreateDirectoryClick(Sender: TObject);
var Dir, DefPath: string;
    Btn: TButton absolute Sender;
    Edit: TLabeledEdit;
begin
  if not Sender.InheritsFrom(TButton) then exit;
  DefPath := '';
  if SubDirDefPath='' then begin
    Edit := FindComponent('DefaultPath') as TLabeledEdit;
    if (Edit<>nil) and not IdemPChar(pointer(Btn.Name),'DEFAULTPATH') then
      DefPath := Trim(Edit.Text);
  end else
    DefPath := SubDirDefPath;
  if DefPath<>'' then
    DefPath := IncludeTrailingPathDelimiter(UpperCase(DefPath));
  Edit := FindComponent(copy(Btn.Name,1,length(Btn.Name)-1)) as TLabeledEdit;
  if Edit=nil then exit;
  Dir := Edit.Text;
  if (DefPath<>'') and not DirectoryExists(Dir) then
    if DirectoryExists(DefPath+Dir) then
      Dir := DefPath+Dir else
      Dir := DefPath; // if not a valid directory, start with DefPath
  if SelectDirectory(Handle,Edit.EditLabel.Caption,Dir) then begin
    if (DefPath<>'') and IdemPChar(pointer(Dir),pointer(DefPath)) then
      Delete(Dir,1,length(DefPath));
    Edit.Text := Dir;
  end;
end;

procedure TProjectDocWizard.CreatePeoplesMenu(aEdit: TLabeledEdit; P: PChar);
var Menu: TPopupMenu;
    Item: TMenuItem;
begin
  if aEdit=nil then exit;
  Menu := TPopupMenu.Create(self);
  if aEdit.Hint<>'' then
    aEdit.Hint := aEdit.Hint+' ('+sRightClickToAddName+')' else
    aEdit.Hint := sRightClickToAddName;
  aEdit.ShowHint := true;
  aEdit.PopupMenu := Menu;
  if P<>nil then
  while P^<>#0 do begin
    Item := TMenuItem.Create(self);
    Item.Caption := GetNextLine(P,P);
    Item.Hint := NameUnValidate(aEdit.Name); // Hint = ComponentName = aValueName
    Item.OnClick := CreatePeoplesClick;
    Menu.Items.Add(Item);
  end;
end;

procedure TProjectDocWizard.CreatePeoplesClick(Sender: TObject);
var Menu: TMenuItem absolute Sender;
    C: TComponent;
    L: TLabeledEdit absolute C;
    text, name: string;
    Plus: char;
begin
  if not Sender.InheritsFrom(TMenuItem) then exit;
  if Menu.Hint='LabeledEditEvaluatedBy' then begin
    Plus := '+';
    C := FindComponent('Risk');
    if (C<>nil) and C.InheritsFrom(TFrameRisk) then
      C := TFrameRisk(C).LabeledEditEvaluatedBy;
  end else begin
    Plus := ',';
    C := FindComponent(Menu.Hint);
  end;
  if (C=nil) or not C.InheritsFrom(TLabeledEdit) then exit;
  name := StringReplaceAll(Menu.Caption,'&','');
  text := L.Text;
  if not CSVContains(text,name,Plus) then
    if text='' then
      L.Text := name else
        L.Text := text+Plus+name;
end;

procedure TProjectDocWizard.CreateEnoughPlace(Height: integer);
begin
  if (Height>=0) and (CreateEditY+Height<=Pages.ClientHeight) then exit;
  LastCreateEditY := CreateEditY;
  CreateEditY := CreateEditTop;
  inc(CreateEditX,CreateEditW+32);
end;

function TProjectDocWizard.CreateMemo(const aValueName, aCaption: string;
  BodySyntax: boolean; MemoHeight: integer = 240): TMemoEx;
begin
  if MemoHeight=-1 then // Height = -1 -> force full next column
      MemoHeight := CreateEditH else
  if MemoHeight=0 then // Height = 0 -> force till end of column
    if CreateEditY>=Pages.ClientHeight then
      MemoHeight := CreateEditH else
      MemoHeight := Pages.ClientHeight-CreateEditY;
  CreateEnoughPlace(MemoHeight);
  inc(MemoHeight,16);
  with TLabel.Create(self) do begin
    Parent := Pages;
    Left := CreateEditX;
    Top := CreateEditY-16;
    if aCaption='' then
      Caption := format(sIntroductionTextN,[fPageTitle])+' :' else
      Caption := aCaption+':';
  end;
  result := TMemoEx.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY;
  result.Width := CreateEditW;
  result.Height := MemoHeight-32;
  if Screen.Fonts.IndexOf('Consolas')>=0 then
    result.Font.Name := 'Consolas';
  result.Font.Size := 9;
  if BodySyntax then begin
    result.RightMargin := (CreateEditW div result.CellRect.Width)-4;
    result.OnGetLineAttr := TProjectSyntax.BodyGetLineAttr;
  end else
    result.OnGetLineAttr := TProjectSyntax.IniGetLineAttr;
  result.WordWrap := BodySyntax;
  MemoSetText(result,aValueName); // set result.Name
  inc(CreateEditY,MemoHeight);
end;

procedure TProjectDocWizard.MemoSetText(Memo: TMemoEx; const aValueName: string);
var s, L: string;
    BodySyntax: boolean;
begin
  BodySyntax := Memo.WordWrap;
  Data.ReadOpen(aValueName,not BodySyntax);
  s := '';
  if BodySyntax then begin
    while not Data.ReadEof do
      s := s+Data.ReadLine(true)+#13#10;
  end else
    while not Data.ReadEof do begin
      L := Data.ReadLine(true);
      if L='' then
        break else
        s := s+L+#13#10;
    end;
  Memo.Lines.Text := trim(s);
  Memo.Modified := false;
  Memo.Name := NameValidate(aValueName); // Data[Name] := ..
end;

function TProjectDocWizard.CreateOption(const aValueName, aCaption: string): TCheckBox;
begin
  CreateEnoughPlace(48-24);
  result := TCheckBox.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY-16;
  result.Width := CreateEditW;
  result.Caption := aCaption;
  result.Checked := isTrue(CreateEditSec[aValueName]);
  result.Name := NameValidate(aValueName); // Data[Name] := .. to update modified value
  inc(CreateEditY,48-24);
end;


function TProjectDocWizard.CreateButton(const aCaption: string; OnClick: TNotifyEvent;
  Width: integer = 0): TButton;
begin
  CreateEnoughPlace(48-24);
  result := TButton.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY-16;
  result.Height := 48-24;
  if width=0 then
    result.Width := CreateEditW div 2 else
    result.Width := width;
  result.Caption := aCaption;
  result.OnClick := OnClick;
  inc(CreateEditY,48-16);
end;

function TProjectDocWizard.CreateLabel(const aCaption: string): TLabel;
begin
  CreateEnoughPlace(48-32);
  result := TLabel.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY-16;
  result.AutoSize := false;
  result.Width := CreateEditW;
  result.Caption := aCaption;
  inc(CreateEditY,48-32);
end;

function TProjectDocWizard.CreateList: TListBox;
var H, i: integer;
    order: string; // Order=.. document value
    par: string;   // Parent=.. value for each item
    source: string; // Source=EIA,IFA,LIS in [SAD] section
    P: PChar;
begin
  if (CreateEditSec=nil) or // section must be a true document
     (CreateEditSec.SectionNameKind<>CreateEditSec.SectionNameValue) then begin
    result := nil;
    exit;
  end;
  H := Pages.ClientHeight-CreateEditY;
  if H<0 then
    H := CreateEditH;
  CreateEnoughPlace(H);
  with TLabel.Create(self) do begin
    Parent := Pages;
    Left := CreateEditX;
    Top := CreateEditY-16;
    Caption := CreateEditSec.ItemName+' items:';
  end;
  result := TListBox.Create(self);
  result.Parent := Pages;
  result.Left := CreateEditX;
  result.Top := CreateEditY;
  result.Width := 128;
  result.Height := H-16;
  result.Items.BeginUpdate;
  source := CreateEditSec['Source']; // Source=Firmware,EIA,IFA,LIS,Builder
  if source='' then begin // normal section: DI,SRS,SDD,Test... by Order=
    order := CreateEditSec['Order'];
    if order='' then // default self-ordered
      order := CreateEditSec.SectionName;
    par := '';
    if order='DI' then begin
      for i := 0 to Data.Sections.Count-1 do
      with Data.Sections.Items[i] do
      if (SectionNameKind<>SectionNameValue) and SameText(SectionNameKind,'DI') then
        if CreateEditSec.SectionName='DI' then
          result.Items.Add(SectionName) else // 'DI-4.1'
          result.Items.Add(CreateEditSec.SectionName+'-'+SectionName); // 'SRS-DI-4.1'
    end else
    for i := 0 to Data.Sections.Count-1 do
      with Data.Sections.Items[i] do
      if (SectionNameKind<>SectionNameValue) and SameText(SectionNameKind,order) then begin
        if Data[SectionNameValue]<>nil then // SRS-DI-4.1 -> DI-4.1
          par := SectionNameValue else // default Parent is last DI
        if Value['Parent']<>'' then // SRS-MENU01->DI-4.1
          par := Value['Parent'];
        if par<>'' then
          if order=CreateEditSec.SectionName then // self-ordered
            result.Items.Add(SectionName) else
            result.Items.Add(CreateEditSec.SectionName+'-'+SectionNameValue) else
          result.Items.Add(SectionName);
      end;
  end else begin // Source<>'' -> SAD document
    P := pointer(source);
    repeat // Source=Firmware,EIA,IFA,LIS,Builder
      result.Items.Add(CreateEditSec.SectionName+'-'+GetNextItem(P));
    until P=nil;
  end;
  result.Items.EndUpdate;
  result.OnClick := CreateListClick;
  CreateListComponentIndex := ComponentCount;
  CreateEditY := CreateEditTop;
  CreateEditW := (Pages.ClientWidth-result.Width-result.Left)div 2-32;
  CreateEditX := result.Left+result.Width+16;
  CreateEditSec := nil; // by default, all detail fields are empty
end;
  
procedure TProjectDocWizard.CreateEditInit(const SecName: string;
  AddDocumentFields, NoItem, NoRevision, NoReviewOrApprov: boolean);
begin
  LabelVersion := nil;
  CreateListComponentIndex := 0;
  CreateComponentFirstIndex := ComponentCount;
  CreateEditSec := Data[SecName]; // Data=nil (new Release) -> CreateEditSec=nil
  CreateEditTop := 24;
  CreateEditX := 48;
  CreateEditY := CreateEditTop;
  CreateEditW := Pages.ClientWidth div 2-(48+12);
  CreateEditH := Pages.ClientHeight-CreateEditTop-8;
  SubDirDefPath := '';
  if not AddDocumentFields then
    exit; // code below are common document properties edit
  CreateEdit('Name',sDocumentName);
  CreateEdit('Purpose',sDocumentPurpose);
  CreateEdit('DisplayName',sDocDisplayName);
  if not NoItem then
    CreateEdit('ItemName',sItemName);
  if NoItem then
    CreateEdit('DocName',sDocNameForDoc) else
    CreateEdit('DocName',sDocNameForDocNotItem);
  if not NoRevision then
    CreateVersion;
  CreatePeoples('PreparedBy',sPreparedBy);
  if not NoReviewOrApprov then begin
    CreatePeoples('ReviewedBy',sReviewedBy);
    CreatePeoples('ApprovedBy',sApprovedBy);
  end;
end;

procedure TProjectDocWizard.CreateVersion;
begin
  LabelVersion := CreateLabel(VersionLabel(CreateEditSec));
  LabelVersion.Font.Style := [fsItalic];
  LabelVersion.ShowHint := true;
  LabelVersion.Hint := format(sClickToVersionN,[fPageTitle]);
  LabelVersion.OnClick := VersionMgmentClick;
  inc(CreateEditY,8);
end;

procedure TProjectDocWizard.SetData(const Value: TSectionsStorage);
var def: string;
    i: integer;
begin
  // init properties
  FDefPROLines := '';
  CreateEditSec := nil;
  if not visible and (fDefault<>nil) then begin // clear memory from closed New Release
    FreeAndNil(fDefault);
    if Data<>nil then // temp fData has been created in SetActivePageIndex(1)
      FreeAndNil(fData);
  end;
  fData := Value;
  SetfPeopleText;
  Title := '';
  if Value=nil then begin // Data=nil -> new release wizard
    def := LoadTextFromResource('default');
    fDefault := TSection.Create('Default');
    i := pos('Company=',def);
    if i=0 then
      Raise Exception.Create(fDefault.SectionName);
    fDefault.ReadSection(@def[i]);
    if DVSParams<>nil then begin
      fDefault['Company'] := DVSParams['COMPANY'];
      fDefault['Product'] := DVSParams['Product'];
      fDefault['ProductPath'] := DVSParams['ProductPath'];
      fDefault['ReleaseName'] := DVSParams['ReleaseName'];
      fDefault['DefaultAuthor'] := DVSParams['DefaultAuthor'];
      fDefault['DefaultPath'] := DVSParams['DefaultPath'];
      fPageCount := 1;
    end else
      fPageCount := 2;
  end else
    if not Visible then
      fPageCount := 22;
end;

procedure TProjectDocWizard.SetfPeopleText;
var N, V: string;
begin
  fPeopleText := '';
  Data.ReadOpen('People',true);
  while Data.ReadNextNameValue(N,V) do
    if fPeopleText='' then
      fPeopleText := N else
      fPeopleText := fPeopleText+#13#10+N;
end;

procedure TProjectDocWizard.SetActivePageIndex(const Value: integer);
var i: integer;
begin
  // 0. save updated values
  if (Value<0) or (Value>=PageCount) then exit;
  if not UpdateDataFromActivePage(false) then exit;
  fActivePageIndex := Value;

  // 1. erase existing data-aware components
  if CreateComponentFirstIndex>0 then
    for i := ComponentCount-1 downto CreateComponentFirstIndex do
      Components[i].Free;
  CreateComponentFirstIndex := 0;
  Application.ProcessMessages;

  // 2. need a message after erasing components (Windows hangs otherwize)
  PostMessage(Handle,WM_USER,Value,0);
end;

procedure TProjectDocWizard.CreateComponentsMessage(var Message: TMessage);
var i, j: integer;
    s, Titles: string;
    P: PChar;
    B: TButton;
    withDoc: boolean;
begin
  assert(Message.wParam=fActivePageIndex);

  // 1. init buttons and property value
  BtnPrev.Enabled := fActivePageIndex>0;
  BtnNext.Enabled := fActivePageIndex<PageCount-1;
  if DVSParams=nil then
    if fDefault=nil then
      Titles := sProjectDocWizardTitles else
      Titles := sProjectNewReleaseWizardTitles else
    Titles := ValAt(sProjectNewReleaseWizardTitles,2);
  fPageTitle := ValAt(Titles,ActivePageIndex);
  BtnPrev.Hint := ValAt(Titles,fActivePageIndex-1);
  BtnNext.Hint := ValAt(Titles,fActivePageIndex+1);

  // 2. add components
  if fDefault<>nil then // new release wizard:
  case fActivePageIndex of
  0: begin
    CreateEditInit('');
    CreateEditSec := fDefault;
    inc(CreateEditY,8);
    CreateButton(sImportValues,ImportClick,CreateEditW);
    inc(CreateEditY,8);
    CreateEdit('Company',sGlobalCompany);
    CreateEdit('Product',sProduct);
    CreateEdit('ProductPath',sProductPath);
    CreateEdit('ReleaseName',sReleaseName);
    CreateEdit('DefaultAuthor',sDefaultAuthor);
    CreateEdit('DefaultAuthorFunction',sDefaultAuthorFunction);
    if DVSParams=nil then begin
      CreateDirectory('BackupPathLocal',sBackupPathLocal);
      CreateDirectory('BackupPathDistant',sBackupPathDistant);
      CreateDirectory('DefaultPath',sDefaultPathRelease);
      // commits will be stored in $DefaultPath$\$ReleaseName$.dvs
      CreateOption('NoDoc',sNoDocumentation);
    end;
    CreateDirectory('DocumentsPath',sDocumentsPath,true);
    with CreateEdit('SADModules',sSourceModules) do begin
      ShowHint := true;
      Hint := sSourceModulesHint;
    end;
    if DVSParams=nil then begin
      CreateOption('SRSOwnORDER',sSRSORDER);
      CreateMemo('DILayout',sDILayout,false,0,).Lines.Text :=
        StringReplaceAll(fDefault['DILayout'],'\',#13#10);
    end else begin
      if fDefault['PRO']='' then begin
        s := fDefault['ReleaseName'];
        fDefault['PRO'] := fDefault['DocumentsPath']+'\'+fDefault['ProductPath']+
          '\'+s+'\'+s+'.pro';
      end;
      CreateEdit('PRO',format(sCommitFileNameNN,['PRO',fDefault['DefaultPath']]));
      CreateEnoughPlace(CreateEditH);
      CreateMemo('DILayout',sDILayout,false,CreateEditH div 2).Lines.Text :=
        StringReplaceAll(fDefault['DILayout'],'\',#13#10);
      CreateOption('SRSOwnORDER',sSRSORDER);
      CreateMemo('SRSORDER',sSRSORDER,false,0);
     end;
  end;
  1: begin
    withDoc := not isTrue(fDefault['NoDoc']);
    if Data=nil then begin
      CreateDocFromResource;
      if not withDoc then
        Data['Params']['Update2'] := ''; // Update2=$ReleaseName$ Documentation;...
      if DVSParams<>nil then
      with Data['Params'] do begin
        Value['SCR'] := DVSParams['SCR'];
        Value['MAN'] := DVSParams['MAN'];
      end;
    end;
    CreateEditInit('Params');
    s := fDefault['DefaultPath'];
    CreateEdit('IgnorePath',sCommitIgnorePath);
    CreateEdit('IgnoreFile',sCommitIgnoreFile);
    CreateEdit('FilterDefault',sCommitFilterDefault);
    CreateEdit('SCR',format(sCommitFileNameNN,['SCR',s]));
    CreateEdit('MAN',format(sCommitFileNameNN,['MAN',s]));
    if withDoc then
      CreateEdit('PRO',format(sCommitFileNameNN,['PRO',s]));
    for i := 0 to 9 do // Display Name;Path;Filter,Filter,..
    with CreateEdit('Update'+IntToStr(i),format(sCommitUpdate,[i])) do begin
      Width := Width-48;
      B := TButton.Create(self);
      B.Parent := Pages;
      B.SetBounds(Left+Width+8,Top,40,Height);
      B.Caption := '...';
      B.OnClick := CreateCommitClick;
      B.Tag := i;
    end;
    if withDoc then
    if isTrue(fDefault['SRSOwnORDER']) then begin
      if Data['SRSORDER']=nil then
        Data.GetOrCreateSection('SRSORDER',true).ReadSection(pointer(
          'DI-'+StringReplaceAll(fDefault['DILayout'],'\',#13#10'DI-')));
      CreateMemo('SRSORDER',sSRSORDER,false,0);
    end;
  end;
  end else
  // fDefault=nil -> document full wizard:
  case fActivePageIndex of
  0: begin
    CreateEditInit('');
    with TImage.Create(self) do begin
      Parent := Pages;
      AutoSize := true;
      Picture.Graphic := TPngImage.Create;
      TPngImage(Picture.Graphic).LoadFromResourceName(HInstance,'wizard2');
      Left := CreateEditX+(CreateEditW-Picture.Width)div 2;
      Top := CreateEditY-8;
      CreateEditY := Top+Height+32;
    end;
    P := pointer(Titles);
    BtnWelcome.Hint := GetNextItem(P);
    i := 1;
    while P<>nil do begin
      B := CreateButton(IntToStr(i)+' - '+StringReplaceAll(GetNextItem(P),'&','&&'),
        WelcomeButtonClick,CreateEditW);
      inc(i);
      if CreateEditX>CreateEditW then begin // we flipped to next page
        CreateEditY := LastCreateEditY-(fPageCount-i+1)*(48-16);
        B.Top := CreateEditY-16;
        inc(CreateEditY,48-16);
      end;
    end;
  end;
  1: begin
    CreateEditInit('Project');
    CreateEdit('Company',sCompany);
    CreateEdit('Name',sProjectName);
    CreateEdit('ReleaseVersion',sReleaseVersion);
    CreateEdit('ReleaseDate','Release Date');
    CreateCombo('Manager','Project Manager',fPeopleText);
    CreateEdit('NoRiskDisplay',sNoRiskDisplay);
    CreateOption('OldWordOpen',sOldWordOpen);
    CreateDirectory('DestinationDir',sDestDir);
    CreateMemo('Project',sWarningMessage,true,0);
    CreateMemo('People',sPeopleList,false,-1);
  end;
  2: begin
    CreateEditInit('DI',true);
    CreateCombo('DefaultPreparedBy',sDefaultPreparedBy,fPeopleText);
    CreateMemo('DILayout',sDILayout,false,-1);
  end;
  3: begin
    CreateEditInit('DI');
    CreateList;
    CreateEditW := CreateEditW*2;
    CreateEdit('Ident',sDescription);
    CreatePeoples('PreparedBy',format(sPreparedByDefault,
      [Data['DI']['DefaultPreparedBy']]));
    CreateEdit('InputLevel',sInputLevel);
    CreateEdit('Request',sRequest);
    CreateRisk('Risk',sRiskAssessment);
  end;
  4: begin
    CreateEditInit('RK',true);
    CreateMemo('RK','',true,-1);
  end;
  5: begin
    CreateEditInit('RK');
    CreateList;
    CreateEditW := CreateEditW*2;
    CreateEdit('Ident',sDescription);
    CreateRisk('Risk',sRiskAssessment);
    CreateMemo('',sItemBody,true,0);
  end;
  6: begin
    CreateEditInit('SRS',true);
    CreateMemo('SRS','',true,-1);
  end;
  7: begin
    CreateEditInit('SRS');
    CreateList;
    CreateEditW := CreateEditW*2;
    CreateEdit('Description',sDescriptionIfNoBody);
    CreateEdit('ShortName',sShortItemName);
    CreateMemo('',sItemBody,true,0);
  end;
  8: begin
    CreateEditInit('Risk',true,true);
  end;
  9: begin
    CreateEditInit('SDD',true,true);
    CreateMemo('SDD','',true,-1);
  end;
  10: begin
    CreateEditInit('SDD');
    CreateList;
    CreateEditW := CreateEditW*2;
    CreateMemo('',sItemBody,true,0);
  end;
  11: begin
    CreateEditInit('SAD',true);
    CreateMemo('SAD','',true,-1);
  end;
  12: begin
    CreateEditInit('SAD');
    with CreateEdit('Source',sSourceModules) do begin
      ShowHint := true;
      Hint := sSourceModulesHint;
    end;
    CreateDirectory('DefaultPath',sDefaultPath);
    CreateEditW := CreateEditW*2; // full width
    CreateMemo('SAD-Source',format(sIntroductionTextN,[sDocFirstPart]),true,0);
  end;
  13: begin
    CreateEditInit('SAD');
    SubDirDefPath := CreateEditSec['DefaultPath'];
    CreateList;
    CreateEdit('DisplayName',sDisplayName);
    CreateEdit('Version',sVersion);
    CreateEdit('Requirements',sRequirementsForTest);
    CreateDirectory('SourcePath',format(sSourcePathN,[SubDirDefPath]),true);
    CreateEdit('SourceFile',sSourceFiles);
    CreateEdit('IncludePath',sIncludePath);
    CreateEdit('Directives',sDirectives);
    CreateEdit('GraphIgnore',sGraphIgnore);
    CreateOption('ExternalDescription',sExternalDescription);
    CreateMemo('',sModuleDescription,true,-1);
  end;
  14: begin
    CreateEditInit('VV',true,true);
    CreateMemo('VV','',true,-1);
  end;
  15: begin
    CreateEditInit('Test',true);
    CreateMemo('Test','',true,-1);
  end;
  16: begin
    CreateEditInit('Tests');
    CreateEdit('Name',sDocumentName);
    CreatePeoples('PreparedBy',sDefaultPreparedBy);
    CreateEditW := CreateEditW*2;
    CreateMemo('Tests',format(sContentN,[sSummarySheet]),true,0);
  end;
  17: begin
    CreateEditInit('Test');
    CreateList;
    CreateEditW := CreateEditW*2;
    CreateOption('ExcludeFromSummary',sExcludeFromSummary);
    CreateEdit('Description',sTestDescription);
    CreateEdit('DocName',sTestDocName);
    CreateVersion;
    LabelVersion.Hint := format(sClickToVersionN,[sTestProtocol]);
    CreatePeoplesMenu(CreateEdit(
      'Source',sSourceModules+' '+sRightClickToAdd),
      pointer(StringReplaceAll(Data['SAD']['Source'],',',#13#10)));
    CreateMemo('',format(sContentN,[sTestProtocol]),true,0);
  end;
  18: begin
    CreateEditInit('');
    CreateMemo('SoftwareVersion',sSoftwareVersion,true,0);
    CreateMemo('SoftwareHistory',sSoftwareHistory,true,-1);
  end;
  19: begin
    CreateEditInit('');
    CreateMemo('KnownIssues',sKnownIssues,true,0);
    CreateMemo('SOUP',sSOUP,true,-1);
  end;
  20: begin
    CreateEditInit('SCRS',true);
    CreateMemo('SCRS',format(sContentN,[fPageTitle]),true,-1);
  end;
  21: begin
    CreateEditInit('Release',true);
    CreateMemo('Release',format(sContentN,[fPageTitle]),true,-1);
  end;
  else begin
    Caption := '';
    PanelTop.Caption := '';
    exit;
  end;
  end; // case of

  // 3. update caption and focus
  Caption := Title+fPageTitle;
  s := '    '+StringReplaceAll(fPageTitle,'&','&&');
  i := ActivePageIndex;
  j := PageCount;
  if fDefault=nil then
    dec(j) else
    inc(i);
  if i=0 then
    PanelTop.Caption := s else
    PanelTop.Caption := s+' '+format(sStepNumber,[i,j]);
  if Visible then
  for i := CreateComponentFirstIndex to ComponentCount-1 do
    with TWinControl(Components[i]) do
    if InheritsFrom(TWinControl) then begin
      SetFocus;
      break;
    end;
end;

procedure TProjectDocWizard.VersionMgmentClick(Sender: TObject);
begin
  if (CreateEditSec<>nil) and (LabelVersion<>nil) and
     Sender.InheritsFrom(TLabel) then
  with TProjectEditorReleaseForm.Create(Application) do
  try
    Init(CreateEditSec);
    if ShowModal=mrOk then
      LabelVersion.Caption := VersionLabel(CreateEditSec);
  finally
    Free; // TProjectEditorReleaseForm.Free
  end;
end;

function TProjectDocWizard.VersionLabel(Sec: TSection): string;
begin
  result := format('%s %s - %s - %s',[sRev,Sec['Revision'],
    Sec['RevisionDate'],Sec['RevisionDescription']])
end;

procedure TProjectDocWizard.CreateListClick(Sender: TObject);
var index, i,j: integer;
    List: TListBox absolute Sender;
    C: TComponent;
    CName, SecName, Value: string;
    TestDetails, B: boolean;
begin
  if (CreateListComponentIndex=0) or
     not Sender.InheritsFrom(TListBox) then exit;
  UpdateDataFromActivePage(false);
  index := List.ItemIndex;
  if index<0 then
    SecName := '' else
    SecName := List.Items[index];
  TestDetails := false;
  CreateEditSec := Data[SecName]; // current section is set from the list
  if CreateEditSec=nil then begin // not existing -> propose to create section
    if MessageDlgFmt(sSectionNewCreateQuery,[SecName])=mrYes then
      CreateEditSec := Data.GetOrCreateSection(SecName,true);
    if CreateEditSec=nil then exit;
  end;
  for i := CreateListComponentIndex to ComponentCount-1 do begin
    C := Components[i];
    CName := NameUnValidate(C.Name);
    Value := CreateEditSec[CName];
    if C.InheritsFrom(TLabeledEdit) then begin
      TLabeledEdit(C).Text := Value;
      if TLabeledEdit(C).Enabled then
      if SameText(CName,'SourceFile') or
         (TestDetails and SameText(CName,'Description')) then begin
        B := Value<>'';
        for j := i+1 to ComponentCount-1 do begin
          C := Components[j];
          if not C.InheritsFrom(TControl) or
             (C.InheritsFrom(TLabel) and (C<>LabelVersion)) then
            break else
            TControl(C).Enabled := B;
        end;
      end;
    end else
    if C.InheritsFrom(TFrameRisk) then
      TFrameRisk(C).Init(Value) else
    if C.InheritsFrom(TComboBox) then
      TComboBox(C).Text := Value else
    if C.InheritsFrom(TMemoEx) then
      MemoSetText(TMemoEx(C),SecName) else
    if C.InheritsFrom(TCheckBox) then begin
      B := isTrue(Value);
      TCheckBox(C).Checked := B;
      if SameText(CName,'ExcludeFromSummary') then
        TestDetails := true;
      if TestDetails then
        for j := i+1 to ComponentCount-1 do begin
          C := Components[j];
          if C.InheritsFrom(TControl) then
            TControl(C).Enabled := not B;
        end;
    end else
    if C=LabelVersion then
      LabelVersion.Caption := VersionLabel(CreateEditSec);
  end;
end;

procedure TProjectDocWizard.WelcomeButtonClick(Sender: TObject);
var i: integer;
begin
  if not Sender.InheritsFrom(TButton) then exit;
  i := StrToIntDef(ValAt(TButton(Sender).Caption,0,' '),-1);
  if i>=0 then
    ActivePageIndex := i;
end;

procedure TProjectDocWizard.BtnWelcomeClick(Sender: TObject);
begin
  ActivePageIndex := 0;
end;

function TProjectDocWizard.UpdateDataFromActivePage(dontAsk: boolean): boolean;
function Void(const aName: string): boolean;
begin
  result := not notVoid(FindComponent(aName));
end;
var i: integer;
    C: TComponent;
    M: TMemoEx absolute C;
    CB: TCheckBox absolute C;
    SecName, Value: string;
    asked: boolean;
    P: PChar;
function ask: boolean;
begin
  if not asked then
    if MessageDlg(sFieldChangeUpdateQuery,mtConfirmation,[mbYes,mbNo],0)<>
      mrYes then begin
      result := false;
      exit;
    end else begin
      BtnSave.Enabled := true;
      asked := true;
    end;
  result := true;
end;
function Save(newValue: string): boolean;
begin
  result := false;
  if CreateEditSec=nil then exit;
  newValue := trim(newValue);
  if newValue=Value then exit; // nothing changed
  if not ask then exit;
  result := true;
  CreateEditSec[SecName] := newValue;
end;
begin
  result := false;
  if (ActivePageIndex<0) or (ActivePageIndex>=PageCount) then exit;
  asked := dontAsk;
  if CreateComponentFirstIndex>0 then
  for i := CreateComponentFirstIndex to ComponentCount-1 do begin
    C := Components[i];
    if not C.InheritsFrom(TControl) or not TControl(C).Enabled then
      continue;
    SecName := NameUnValidate(C.Name);
    Value := CreateEditSec[SecName];
    if C.InheritsFrom(TLabeledEdit) then begin
      if Save(TLabeledEdit(C).Text) then begin
        case ActivePageIndex of
        12: // Software Architecture details -> Source=Firmware,EIA,IFA,LIS,Builder
          if SameText(SecName,'Source') then begin
            P := pointer(CreateEditSec[SecName]);
            while P<>nil do begin
              Value := GetNextItem(P);
              if Value='' then continue;
              Value := CreateEditSec.SectionName+'-'+Value;
              if Data[Value]=nil then
                if MessageDlgFmt(sSectionNewCreateQuery,[Value])=mrYes then
                  Data.GetOrCreateSection(Value,true);
            end;
          end;
        end; // case
      end;
    end else
    if C.InheritsFrom(TFrameRisk) then
      Save(TFrameRisk(C).Risk) else
    if C.InheritsFrom(TComboBox) then
      Save(TComboBox(C).Text) else
    if C.InheritsFrom(TMemoEx) then begin
      if M.Modified then
        if ask then
        if (fDefault<>nil) and SameText(SecName,'DILayout') then
          // DILayout is fDefault['DILayout'] with \ for New project
          fDefault[SecName] := StringReplaceAll(trim(M.Lines.Text),#13#10,'\')
        else
        if Data<>nil then begin
          if M.WordWrap then
            Data.WriteBody(SecName,M.Lines.Text) else
            Data.WriteParams(SecName,M.Lines.Text);
          M.Modified := false;
          if fDefault=nil then
          case ActivePageIndex of
          1: // General page -> [People] Section changed -> fPeopleText update
            if SameText(SecName,'People') then
              SetfPeopleText;
          end; // case
        end;
    end else
    if C.InheritsFrom(TCheckBox) then begin
      if isTrue(Value) then // normalize boolean text representation
        Value := 'Yes' else
        Value := 'No';
      if TCheckBox(C).Checked then
        Save('Yes') else
        Save('No');
    end;
    // LabelVersion were already saved in VersionMgmentClick
  end;
  if fDefault<>nil then // new release wizard: assure no void fields
    case ActivePageIndex of
    0: if Void('COMPANY') or Void('Product')  or Void('ReleaseName') or
          Void('DefaultPath') then exit else begin
         C := FindComponent('NoDoc');
         if C=nil then begin
           if Void('DILayout') or
            (isTrue(fDefault['SRSOwnORDER']) and Void('SRSORDER')) then exit;
         end else
         if not CB.Checked and Void('DILayout') then
           exit;
       end;
    1: if Void('Update0') then exit else
       if isTrue(fDefault['SRSOwnORDER']) then
         if Void('SRSORDER') then exit;
    end
  else
    case ActivePageIndex of
    1: if Void('COMPANY') or Void('Name') or Void('DestinationDir') or
        Void('People') then exit;
    2: if Void('DILayout') then exit;
    end;
  result := true;
end;

procedure TProjectDocWizard.BtnSaveClick(Sender: TObject);
procedure ForceDirectoriesQuery(dir: string; ask: boolean=true);
begin
  dir := trim(dir);
  if (dir<>'') and not DirectoryExists(dir) then
    if ask and (MessageDlgFmt(sCreateDirectoryQueryN,[dir])<>mrYes) then
      exit else
      if not ForceDirectories(dir) then
        raise EInOutError.CreateFmt('%s:'#13'%s',[SCannotCreateDir,dir]);
end;
procedure CreatePRO;
var i: integer;
    P: PChar;
    SRSOrder, DILayout: TSection;
    s: string;
begin
  if isTrue(fDefault['SRSOwnORDER']) then
    SRSOrder := Data['SRSORDER'] else
    SRSOrder := nil; // SRSOrder=nil if order by DILayout
  // update [DILayout] and create [DI-*] sections
  DILayout := Data.GetOrCreateSection('DILayout',true);
  P := pointer(fDefault['DILayout']); // '1\2\3'
  while P<>nil do begin
    s := trim(GetNextItem(P,'\'));
    if s='' then continue;
    DILayout.Lines.Add(s);
    if not (s[1] in [':',';']) then
      with Data.GetOrCreateSection('DI-'+s,true) do
        Value['Ident'] := SectionName;
  end;
  // create [SRS-*] section
  if SRSOrder=nil then begin // order by DILayout
    P := pointer(fDefault['DILayout']); // '1\2\3'
    while P<>nil do begin
      s := trim(GetNextItem(P,'\'));
      if (s<>'') and not (s[1] in [':',';']) then
        Data.GetOrCreateSection('SRS-DI-'+s,true)['ShortName'] := 'SRS of DI-'+s;
    end;
  end else // order by [SRSORDER] memo lines
  for i := 0 to SRSOrder.Lines.Count-1 do begin
    s := trim(SRSOrder.Lines[i]);
    if s<>'' then
      Data.GetOrCreateSection('SRS-'+s,true)['ShortName'] := 'SRS '+s;
  end;
  Data.DeleteSection('SRSORDER');
end;
var CurrentPath,
    DefPath, ReleaseName, ProductPath, PROFileName,
    TempDVS: string;
    Commit: TSection;
    Versions: TVersions;
begin
  if not UpdateDataFromActivePage(DVSParams<>nil) then // save any pending updated field values
    ModalResult := mrNone else
    if fDefault=nil then exit;
  // new project: create proper DI and SRS sections
  if (Data=nil) and (DVSParams<>nil) then
  if ModalResult=mrNone then exit else begin
    ModalResult := mrNone;
    CreateDocFromResource;
    CreatePRO;
    if Data<>nil then begin
      PROFileName := fDefault['PRO'];
      Data.DeleteSection('Params'); // Data.Text without [Params] = .pro content
      DefPath := IncludeTrailingPathDelimiter(fDefault['DefaultPath']);
      ForceDirectoriesQuery(DefPath); // raise an exception if impossible to create
      CurrentPath := GetCurrentDir;
      SetCurrentDir(DefPath);
      PROFileName := ExpandFileName(PROFileName);
      SetCurrentDir(CurrentPath);
      ForceDirectoriesQuery(ExtractFilePath(PROFileName));
      if (FileExists(PROFileName) and
          (MessageDlgFmt(sOverwriteFileQueryN,[PROFileName])=mrYes)) or
        (MessageDlgFmt(sFileCreateNewQueryN,[PROFileName])=mrYes) then
        if not StringToFile(PROFileName,Data.Text) then
          raise EInOutError.Create(SAccessDenied) else begin
          DVSParams['PRO'] := fDefault['PRO'];
          ModalResult := mrOk;
        end;
    end;
    exit;
  end;
  Commit := Data['Params']; // will be used to create new .dvs params
  if (Data=nil) or (Commit=nil) then begin
    ModalResult := mrNone;
    exit;
  end;
  if isTrue(fDefault['NoDoc']) then // no documentation -> Version.PROFileName := ''
    Commit['PRO'] := '' else
    CreatePRO;
  case MessageDlgFmt(sDoCreateReleaseNewQueryN,[fDefault['ReleaseName']]) of
  mrYes: begin
    ReleaseName := ExcludeTrailingPathDelimiter(trim(fDefault['ReleaseName']));
    DefPath := IncludeTrailingPathDelimiter(fDefault['DefaultPath']);
    ProductPath := trim(fDefault['ProductPath']);
    if ProductPath='' then
      ProductPath := trim(fDefault['Product']);
    ProductPath := ExcludeTrailingPathDelimiter(ProductPath);
    // commits are stored in $DefaultPath$\$ReleaseName$.dvs
    DVSFileName := DefPath+ReleaseName+'.dvs'; // 'D:\Dev\Synopse\Product New Version.dvs'
    if not FileExists(DVSFileName) or
      (MessageDlgFmt(sOverwriteFileQueryN,[DVSFileName])=mrYes) then
    try
      CurrentPath := GetCurrentDir;
    try
      // [Default] = fDefault directories initialization
      ForceDirectoriesQuery(DefPath); // raise an exception if impossible to create
      SetCurrentDir(DefPath);
{      if Versions.PROFileName<>'' then NO: creation will be done with PROFileName below
        ForceDirectoriesQuery(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(
          fDefault['DocumentsPath'])+ProductPath)+ReleaseName); // 'D:\Dev\Documents\Product New Version'}
      ForceDirectoriesQuery(fDefault['BackupPathLocal']);
      // [Params] = Commit Versions initialization
      TempDVS := ChangeFileExt(DVSFileName,'.tmp');
      DeleteFile(TempDVS);
      Versions := TVersions.Create(TempDVS);
      try
        Versions.Commits.LoadFromMemory(pointer('[Params]'#13#10+Commit.Lines.Text));
        Versions.ReadParamsFromCommits;
        ForceDirectoriesQuery(ExtractFilePath(Versions.SCRFileName));
        ForceDirectoriesQuery(ExtractFilePath(Versions.MANFileName));
        if (Versions.SCRFileName<>'') and not FileExists(Versions.SCRFileName) and
           (MessageDlgFmt(sFileCreateNewQueryN,[Versions.SCRFileName])=mrYes) then begin
          Versions.SCRForceCreate;
          if SCRImport(Versions.SCR,true) then // forcelog=true (allow project change)
            Versions.SCR.SaveToFile;
        end;
        if (Versions.MANFileName<>'') and not FileExists(Versions.MANFileName) and
           (MessageDlgFmt(sFileCreateNewQueryN,[Versions.MANFileName])=mrYes) then
          Versions.MANForceCreate;
        if Versions.PROFileName<>'' then begin
          Data.DeleteSection('Params'); // Data.Text without [Params] = .pro content
          ForceDirectoriesQuery(ExtractFilePath(Versions.PROFileName));
          if not FileExists(Versions.PROFileName) or
            (MessageDlgFmt(sOverwriteFileQueryN,[Versions.PROFileName])=mrYes) then
            if not StringToFile(Versions.PROFileName,Data.Text) then
              raise EInOutError.Create(SAccessDenied);
        end;
        Versions.Commits.Modified := true; // force save Versions to file
        FreeAndNil(MainForm.Version); // close main dvs -> allows update
      finally
        Versions.Free;
        if (MainForm.Version=nil) and FileExists(TempDVS) then begin
          DeleteFile(DVSFileName);
          if not RenameFile(TempDVS,DVSFileName) then
            DVSFileName := '';
        end;
      end;
    except
      on E: Exception do begin
        DVSFileName := '';
        ModalResult := mrNone;
        MessageDlg(E.Message,mtError,[mbOk],0);
      end;
    end;
    finally
      SetCurrentDir(CurrentPath);
    end else
      DVSFileName := '';
  end;
  mrCancel: ModalResult := mrNone;
  mrNo: exit;
  end;
end;

procedure TProjectDocWizard.FormDestroy(Sender: TObject);
begin
  if fDefault<>nil then begin
    FreeAndNil(fDefault);
    if Data<>nil then // temp fData has been created in SetActivePageIndex(1)
      FreeAndNil(fData);
  end;
end;

function TProjectDocWizard.DoCreateReleaseNew: boolean;
begin
  result := false; // error by default
  if (fDefault=nil) or (Data=nil) or (ModalResult<>mrOk) then exit;
  if (Data['DI']=nil) or (Data['DILayout']=nil) or (Data['Project']=nil) then exit;
end;

procedure TProjectDocWizard.SetTitle(const Value: string);
begin
  FTitle := Value;
  if Value<>'' then
    FTitle := FTitle +' - ';
end;

procedure TProjectDocWizard.CreateCommitClick(Sender: TObject);
var Btn: TButton absolute Sender;
    Edit,
    FilterDefault: TLabeledEdit;
    Value: string;
begin
  if (CreateEditSec=nil) or not Sender.InheritsFrom(TButton) then exit;
  FilterDefault := FindComponent('FilterDefault') as TLabeledEdit;
  Edit := FindComponent('Update'+IntToStr(Btn.Tag)) as TLabeledEdit;
  Value := Edit.Text;
  if EditorCommitForm(format(sCommitEditionN,[Btn.Tag]),
    fDefault['DefaultPath'], FilterDefault.Text,Value) then
    Edit.Text := Value;
end;

procedure TProjectDocWizard.ImportClick(Sender: TObject);
var DVS: string;
    V: TVersions;
    PRO: TSectionsStorage;
procedure EditSet(const Name, Text: string);
var F: TLabeledEdit;
begin
  F := FindComponent(Name) as TLabeledEdit;
  if F<>nil then
    F.Text := Text;
end;
procedure Edit(const Name: string);
begin
  EditSet(Name,V.Params[Name]);
end;
begin
  if Sender.InheritsFrom(TButton) then begin
    MainForm.UpdatePopupMenuOpen(PopupMenu.Items,ImportClick);
    case PopupMenu.Items.Count of
     0: exit;
     1: ImportClick(PopupMenu.Items[0]); // no history -> direct browse
     else with Pages.ClientToScreen(TButton(Sender).BoundsRect.TopLeft) do
            PopupMenu.Popup(X,Y+TButton(Sender).Height);
     end;
  end else begin
    DVS := MainForm.GetPopupMenuOpenFileName(Sender as TMenuItem);
    if DVS='' then exit;
    if SameText(MainForm.Version.FileName,DVS) then
      V := MainForm.Version else
      V := TVersions.Create(DVS);
    PRO := nil;
    try
      if (V.PROFileName<>'') and FileExists(V.PROFileName) then
        PRO := TSectionsStorage.Create(V.PROFileName);
      Edit('COMPANY');
      Edit('Product');
      EditSet('ProductPath',V.Params.ReadString('ProductPath',ValAt(V.Params['Product'],0,' ')));
      Edit('DefaultPath');
      Edit('DocumentsPath');
      EditSet('BackupPathLocal',ValAt(V.Params['BackupDir0'],2));
      EditSet('BackupPathDistant',ValAt(V.Params['BackupDir1'],2));
      Edit('DefaultAuthor');
      if PRO<>nil then begin
        EditSet('DefaultAuthorFunction',PRO['People'][V.Params['DefaultAuthor']]);
        EditSet('SADModules',PRO['SAD']['Source']);
        FDefPROLines := PRO['Project'].Lines.Text;
      end;
      { TODO : import Update?=.. }
    finally
      PRO.Free;
      if V<>MainForm.Version then
        V.Free;
    end;
  end;
end;

procedure TProjectDocWizard.CreateDocFromResource;
var text,s,v: string;
    i,j: integer;
    PRO: TSection;
begin
  if Data<>nil then exit;
  text := LoadTextFromResource('default');
  i := pos('[Params]',text);
  if i=0 then
    exit; // wrong resource format
  delete(text,1,i-1); // erase [Default] section
  for i := 0 to fDefault.Lines.Count-1 do begin
    s := fDefault.Lines[i];
    if (s='') or (s[1]=';') then continue;
    j := pos('=',s);
    if j>0 then // '$Product$' -> 'Product'
      if IdemPChar(pointer(s),'SRSOWNORDER=') then
        if IdemPChar(pointer(s),'SRSOWNORDER=YES') then
           text := StringReplaceAll(text,'$SRSORDER$','SRS') else
           text := StringReplaceAll(text,'$SRSORDER$','DI') else begin
        v := copy(s,j+1,maxInt);
        text := StringReplaceAll(text,'$'+copy(s,1,j-1)+'$',v);
        if IdemPChar(pointer(s),'COMPANY=') then // $Company=lower $COMPANY$=upper
          text := StringReplaceAll(text,'$COMPANY$',UpperCase(v));
      end;
  end;
  Data := TSectionsStorage.Create;
  Data.LoadFromMemory(pointer(text),length(text));
  PRO := Data['Project'];
  PRO['DestinationDir'] := GetMyDocuments;
  if FDefPROLines<>'' then // restore params from old release
  with TStringList.Create do
  try
    Text := FDefPROLines;
    j := IndexOf('Logo'); if j>=0 then PRO['Logo'] := Strings[j];
    j := IndexOf('OldWordOpen'); if j>=0 then PRO['OldWordOpen'] := Strings[j];
    j := IndexOf('DefLang'); if j>=0 then PRO['DefLang'] := Strings[j];
    j := IndexOf('DestinationDir'); if j>=0 then PRO['DestinationDir'] := Strings[j];
    i := pos(#13#10#13#10,FDefPROLines);
    s := PRO.Lines.Text;
    j := pos(#13#10#13#10,s); assert(j>0);
    if i>0 then // copy Copyright
      PRO.Lines.Text := copy(s,1,j)+copy(FDefPROLines,i,maxInt);
  finally
    Free;
  end;
end;

end.
