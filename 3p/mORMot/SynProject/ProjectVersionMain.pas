/// main SynProject screen, managing all project versioning
// - this unit is part of SynProject, under GPL 3.0 license; version 1.18
unit ProjectVersionMain;

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

 Version 1.9
  - handle more than one recursivity level for =[section] includes in document
  - fix error displaying the Refers (RK) items without the description in
    the paragraph title
  - "\TableImplements=ISO-To be ignored" syntax to ignore any "\Implements ISO ..."
    in [To be ignored] document
  - new extended e.g. \TableRK=1,2,3 syntax, similar to \TableDI=...

 Version 1.10
  - source code description can be loaded from outside .sae files:
    this allow to document an existing code tree, without touching its content;
    the .sae files are .zip files containing a .ini-like text file for storing
    the description text for all types/objects/methods of the source code:
    the ExternalDescription=Yes parameter must be set in any [SAD-module], then
    the user uses the "SAD/Create external .sae file" menu from the "Word" button;
    edition of the .sae file must be done externaly, with WinZip or Total Commander

 Version 1.12
  - direct insertion of a picture from the clipboard
  - fix issue with People Name with accents
  - added "Writer=" optional parameter for header in [Project] section
  - added "NoUsesUnits=Yes" parameter in [SAD] in order to avoid any adding of
    "Units used in..." table in SAD document
  - added custom column widths for \TableNewFeatures[=18,62,20] and
    \TableBugFixes[=17,10,12,12,49]
  - new internal Graphviz EMF creation written in Delphi, directly from SVG
    (the WinGraphViz EMF writer was buggy)

  Version 1.13
  - added new source code formats: $$ for XML/HTML/SGML, !$ for DFM
  - format included .XML/.HTML/.HTM/.DFM files as expected
  - fixed GPF errors in the internal object browser
  - fixed issue when formating code in RTF content (avoid prematurated doc end)
  - SynProject application will display the project name in the Windows task bar 
  - new NoConfidential global option in [Project]
  - new 'All Titles' option in right click on the section list to browse then
    go to corresponding content (also accessible via F10 function key)
  - new 'Titles #' sub menu to browse ":# title" items in # order for adding @#@
  - now handle '\Index KeyWord 1,KeyWord 2' line or @*keyword@ inlined text, to
    be processed by DocumentIndex=...,Index  (@**keyword@ for main entry)
  - Graph window can now create custom hierarchy diagrams for any classes, or
    TSQLRecord structure layout, per SAD-module (SAD default graphs are limited
    to one unit content, but those graphs can be as wide as any SAD-module)
  - added F2 key in the editor to mark a block as Delphi / highlight (i.e. will
    put an ! at every line beginning)
  - added F10 key to browse the .pro titles (with incremental search)
  - new right-click popup menu accessible for editor, with fast access to all
    tool functions
  - new "Copy as HTML/BBCODE" functions - to be used e.g. for easy Blogging :)

  Version 1.15
  - inlined pictures (e.g. in a grid) are now exported as binary in rtf
  - new HeaderWithLogo option for DocumentFrontPage=/SubDocFrontPage= setting,
    creating an header with the Company logo instead of the default text header
    (can be set globaly also for all documents in the [Project] section,
    same as the NoHeaderBorder= setting)
  - new FullTitleInTableOfContent option for DocumentFrontPage=/SubDocFrontPage=
    setting, adding the will title in the table of contents (e.g. not only the DI
    number, but also its textual description)
  - more modern table layout, with no vertical lines, and doubled first row

  Version 1.16
  - new template (including fonts and colors) for the generated documents
  - SAD now contains a quick reference table for objects and functions lists
  - now the Wingraphviz COM library can be registered for the current user, if
    this one does not have local administrator rights
  - made versioning screen faster in case of huge number of files (lazy loading)
  - use "My Documents" folder if DestinationDir is not properly set
  - change main editor font to 'Consolas' (if available), and with word wrap

  Version 1.18
  - added optional ParseTitleOffset=.. integer param for the [SAD-*] sections
  - enhanced SAD layout, when working with non flat titles (e.g. for mORMot)
  - added NoProjectDetailsLogo option in DocumentFrontPage/SubDocFrontPage
  - added optional EditorRightMargin=... parameter in [Project] section
  - a new section with dedicated footer is created in SAD for each unit
    (implements feature request [f33db9cd0e])


*)

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ProjectVersioning, ProjectVersionPages, ProjectEditor, SynMemoEx,
  ProjectCommons, Menus, ProjectEditMain,
  StdCtrls, ComCtrls, ToolWin, ExtCtrls, Dialogs, ImgList;

type
  TMainForm = class(TForm)
    ToolBar: TToolBar;
    PanelLeft: TPanel;
    SplitterPanel: TSplitter;
    PanelRight: TPanel;
    ImageList: TImageList;
    BtnReleaseSettings: TToolButton;
    BtnSCR: TToolButton;
    BtnCommit: TToolButton;
    BtnBackup: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    BtnPRO: TToolButton;
    BtnPROWizard: TToolButton;
    BtnSCRAdd: TToolButton;
    ToolButton5: TToolButton;
    BtnReleaseNew: TToolButton;
    BtnReleaseOpen: TToolButton;
    BtnManual: TToolButton;
    ToolButton3: TToolButton;
    BtnBackupOpen: TToolButton;
    PopupMenuBackup: TPopupMenu;
    PopupMenuOpen: TPopupMenu;
    ToolButton4: TToolButton;
    BtnAbout: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnReleaseSettingsClick(Sender: TObject);
    procedure BtnSCRClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnBackupClick(Sender: TObject);
    procedure BtnCommitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateCommitMemo(LastCommit: boolean=true);
    procedure BtnPROClick(Sender: TObject);
    procedure BtnSCRAddClick(Sender: TObject);
    procedure BtnPROWizardClick(Sender: TObject);
    procedure BtnReleaseNewClick(Sender: TObject);
    procedure BtnManualClick(Sender: TObject);
    procedure BtnBackupOpenClick(Sender: TObject);
    procedure BtnReleaseOpenClick(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
  private
    SCRForm: TForm;
    SCREditor: TFrameEditor; // init in BtnSCRClick(), used in BtnSCRAddClick()
    CommitMemo: TMemo;
    CommitMemoText: string;
    CommitFiles: TStringList;
    procedure OnCommit(const Text: string);
    procedure OnCommitFileUpdate(aFile: integer; const action: string);
    function CommitLogForm(const Title: string): TForm;
    procedure InitBtnHistory(Editor: TFrameEditor; const aFileName: string);
    procedure EditSCR;
    procedure OnCommitFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateVersions;
  public
    /// where to store last opened files
    // 'C:\Documents and Settings\username\Application Data\SynProject.ini'
    IniFileName: string;
    Version: TVersions;
    PagesLeft,
    PagesRight: TFramePages;
    MANEditor: TProMainForm;
    procedure UpdatePopupMenuOpen(Menu: TMenuItem; OnClick: TNotifyEvent);
    function GetPopupMenuOpenFileName(Menu: TMenuItem): string;
    procedure AddToIniFileName(aDVSFileName: string; MakeDefault: boolean);
  end;

var
  MainForm: TMainForm;

resourcestring
  sDone = 'Done';
  sNothingNew = 'Nothing new in the files: commit not done';
  sVersionMain = 'SynProject Versioning system - %s - %s';
  sErrorTrackNumberDupplicate = 'The "%d" Track Number is already in the SCR file';
  sErrorEditorReadOnly = 'Editor is Read-Only';
  sReleaseFiles = 'Release files';
  sNoReleaseCreateQuery = 'No release file (*.dvs) has been found.'#13#13+
      'Do you want to create a new Release?';
//  sNewReleaseWillOpenNextStartup = 'The new release will be opened as default at next program startup.';
  sExportToPVCS = 'Do you really want to add a Note to the PVCS Tracker?';
  sExportToPVCSNNN = '%s (%s commit n.%d)';
  sExportedToPVCSN = 'Note created successfully in tracker entry # %d';
  sPressEchapToCloseForm =
    '-=- Process complete: press Echap or Enter to close the form -=-';
  sBrowseToOpenAFile = 'Browse to open a file';
  sNoDocumentCreateQuery = 'No documentation file (*.pro) has been found.'#13#13+
      'Do you want to create a new documentation file?';


implementation

uses
  ShellApi,
  ProjectRTF,
  ProjectVersionSCR, ProjectVersionBackup, ProjectFormViewOne,
  ProjectVersionCommit, ProjectSections,
  ProjectVersionCompare, ProjectFormDocWizard,
  ProjectTrackerLogin, ProjectTrkTool,
  ProjectDiff;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var Ext: string;
    Param: string; // command line or last opened file name
    n, i: integer;
    F: TForm;
function GetCurrentDVS: string;
begin
  FileToString(IniFileName,result);
  result := ValAt(result,0,#13); // get first line only
end;
procedure OneBackup(const DVSName: string);
var b: integer;
    s: string;
    something: boolean;
    Backup: TVersions;
begin
  Ext := ExtractFileExt(DVSName);
  if SameText(Ext,'.DVS') and FileExists(DVSName) then
  try
    Backup := TVersions.Create(DVSName);
    F.Caption := format(SVersionMain,[btnBackup.Caption,DVSName]);
    s := #13#10' -'+StringOfChar('=',length(DVSName)+14)+'-';
    OnCommit(s+#13#10' -====-  '+DVSName+'  -====-'+s);
    something := false;
    for b := 0 to 9 do
      if Backup.doBackup(b,OnCommit) then
        something := true;
    if not something then
      OnCommit(#13#10'  '+sNothingNew);
    OnCommit('');
    UpdateCommitMemo; // refresh last logs
  finally
    FreeAndNil(Backup);
  end;
end;
begin
  LoadFromEmbedded(ImageList,'MainForm.bmp');
  // 'C:\Documents and Settings\username\Application Data\SynProject.ini'
  IniFileName := GetAppDataPath+ChangeFileExt(ExtractFileName(Application.ExeName),'.ini');
  UpdatePopupMenuOpen(PopupMenuOpen.Items,BtnReleaseOpenClick);
  Param := '';
  n := ParamCount;
  if n>0 then begin
    Param := paramstr(1);
    if SameText(Param,'--backup') or SameText(Param,'-b') then begin
      F := CommitLogForm(format(SVersionMain,[btnBackup.Caption,'']));
      try
        F.OnClose := nil; // we will close it below
        if n=1 then
          OneBackup(GetCurrentDVS) else
        for i := 2 to n do // all other params are .dvs file names
          OneBackup(paramstr(i));
        F.Caption := format(SVersionMain,[btnBackup.Caption,sDone]);
        Screen.Cursor := crDefault;
        repeat
          Application.ProcessMessages;
          Sleep(10);
        until not F.Visible;
        exit; // Version is left = nil -> FormShow will close application
      finally
        F.Free; // will be called, even by exit
      end;
    end;
    if FileExists(Param) then begin
      Ext := ExtractFileExt(Param);
      if SameText(Ext,'.PRO') then begin
        with TProMainForm.Create(nil) do
        try
          InitEditor(Param,Param);
          Application.Title := ' SynProject - '+ExtractFileName(Param);
          ShowModal;
        finally
          Free;
        end;
        exit; // Version is left = nil -> FormShow will close application
      end;
    end else
      Param := '';
  end;
  if Param='' then begin // no .dvs file name specified -> try from ProjectVersion.ini
    Param := GetCurrentDVS;
    if (Param<>'') and not FileExists(Param) then
      Param := '';
  end;                                   
  if (Param='') or not SameText(ExtractFileExt(Param),'.DVS') then
    case MessageDlg(sNoReleaseCreateQuery,mtConfirmation,mbYesNoCancel,0) of
    mrNo,mrCancel: exit; // Version is left = nil -> FormShow will close application
    mrYes:
    with TProjectDocWizard.Create(Application) do
    try
      Data := nil; // will force new release wizard
      if (ShowModal=mrOK) and (DVSFileName<>'') and FileExists(DVSFileName) then begin
         // a new Release has been created -> open by default
        AddToIniFileName(DVSFileName,true);
        Param := DVSFileName;
      end else
        exit; // Version is left = nil -> FormShow will close application
    finally
      Free;
    end;
  end;
  Version := TVersions.Create(Param);
  if (Version.SCR=nil) and (Version.SCRFileName<>'') and
     (MessageDlgFmt(sFileCreateNewQueryN,[Version.SCRFileName])=mrYes) then begin
    Version.SCRForceCreate;
    SCRImport(Version.SCR);
  end;
  PagesLeft := TFramePages.Create(self,Version);
  PagesLeft.Parent := PanelLeft;
  PagesLeft.Align := alClient;
  PagesLeft.Name := 'PagesLeft';
  PagesRight := TFramePages.Create(self,Version);
  PagesRight.Parent := PanelRight;
  PagesRight.Align := alClient;
  PagesRight.Name := 'PagesRight';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
//  if Version=nil then exit;
//  Version.DoCommit([1,2,{3,4,}5],'Release '+IntToStr(Random(1000)));
  FreeAndNil(Version);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if Version=nil then exit;
  PanelLeft.Width := ClientWidth div 2;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if Version=nil then
    Close else begin
    WindowState := wsMaximized;
    UpdateVersions;
  end;
end;

procedure TMainForm.BtnReleaseSettingsClick(Sender: TObject);
var E: TFrameEditor;
begin
  if EditText(CreateFormEditor(Version.Commits,E,true,
    Version.ReleaseName+' - '+BtnReleaseSettings.Hint),E,true) then begin
    Version.ReadParamsFromCommits;
    UpdateVersions;
  end;
end;

procedure TMainForm.BtnSCRClick(Sender: TObject); // Sender=nil -> init, not edit
begin
  if Version.SCR=nil then exit; // we need a valid .scr file name
  if SCRForm=nil then begin
    SCRForm := CreateFormEditor(Version.SCR,SCREditor,false,
    format('%s - %s - %s',[Version.ReleaseName,BtnSCR.Hint,Version.SCR.FileName]));
    SCREditor.BtnAddTracker.OnClick := BtnSCRAddClick;
    SCREditor.BtnAddTracker.Show;
    SCREditor.BtnImportTracker.OnClick := BtnSCRAddClick;
    SCREditor.BtnImportTracker.Show;
  end else
    if SCREditor.Memo.Modified then // modified but not saved?
      SCREditor.Memo.Lines.Text := Version.SCR.Text; // -> reload from file
  SCREditor.Memo.Modified := false;
  InitBtnHistory(SCREditor,Version.SCR.FileName);
  if Sender<>nil then
    EditSCR;
end;

procedure TMainForm.BtnSCRAddClick(Sender: TObject);
var Btn: TToolButton absolute Sender;
begin
  if not Sender.InheritsFrom(TToolButton) then exit;
  if Version.SCR=nil then exit;
  if Btn.Owner=self then // direct add from main form -> create SCREditor if necessary
    BtnSCRClick(nil); // Sender=nil -> init, not edit
  if (SCREditor<>nil) and SCREditor.ReadOnly then begin
    MessageDlg(SErrorEditorReadOnly,mtError,[mbYes],0);
    exit;
  end;
  if Sender=SCREditor.BtnImportTracker then begin
    SCREditor.UpdateDataFromTextAllIfNecessary;
    SCRImport(Version.SCR);
    SCREditor.Memo.Lines.Text := Version.SCR.Text; // -> reload from file
    SCREditor.Memo.Modified := true; // force save modifications to .scr file 
  end else begin
    ProjectVersionSCRForm.Init(SCREditor.BtnAddTracker.Hint);
    repeat
      if ProjectVersionSCRForm.ShowModal=mrCancel then exit else
      if Version.SCR.Section[IntToStr(ProjectVersionSCRForm.TrackNumber)]<>nil then begin
        MessageDlgFmt(SErrorTrackNumberDupplicate, [ProjectVersionSCRForm.TrackNumber],
          mtError,[mbOK]);
        ProjectVersionSCRForm.TrackNumber := -1; // force Ident.SetFocus
      end else
        break;
    until false;
    with SCREditor.Memo do begin
      Command(ecEndDoc);
      with ProjectVersionSCRForm do
      InsertTextAtCurrentPos(format(#13#10'[%d]'#13#10'Description=%s'#13#10+
        'ShortName=%s'#13#10'Request=%s'#13#10'Risk=%s'#13#10#13#10,
        [TrackNumber, Description.Text, ShortName.Text, Request.Text, Risk])+
        Body);
    end;
  end;
  if Btn.Owner=self then // direct add from main form -> create SCREditor if necessary
    EditSCR;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if byte(Shift)=0 then
   case Key of
     vk_F1: BtnSCR.Click;
     vk_F2: BtnPRO.Click;
     vk_F3: BtnCommit.Click;
     vk_F4: BtnBackup.Click;
     vk_F9: BtnReleaseSettings.Click;
   end;
end;

{$WARN SYMBOL_PLATFORM OFF}
function TMainForm.CommitLogForm(const Title: string): TForm;
begin
  Screen.Cursor := crHourGlass;
  result := TForm.Create(Application);
  result.Position := poScreenCenter;
  if DebugHook=0 then
    result.FormStyle := fsStayOnTop;
  result.OnClose := FormClose; // close=Free
  with Monitor.WorkareaRect do begin
    result.Width := ((Right-Left)*2)div 3;
    result.Height := ((Bottom-Top)*2)div 3;
  end;
  result.BorderStyle := bsDialog;
  result.Caption := Title;
  result.KeyPreview := true; // Escape key will close form
  result.OnKeyDown := OnCommitFormKeyDown;
  if Screen.Fonts.IndexOf('Consolas')>=0 then
    result.Font.Name := 'Consolas' else
    result.Font.Name := 'Courrier';
  result.Font.Size := 10;
  CommitMemo := TMemo.Create(result);
  CommitMemo.Parent := result;
  CommitMemo.Align := alClient;
  CommitMemo.ScrollBars := ssVertical;
  CommitMemo.ReadOnly := true;
  CommitMemo.Font.Style := [fsBold];
  CommitMemo.Font.Color := clYellow;
  CommitMemo.Color := clNavy;
  CommitMemoText := '';
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TMainForm.BtnBackupClick(Sender: TObject);
procedure Backup;
var F: TForm;
begin
  F := CommitLogForm(ProjectVersionBackupForm.Caption);
  try
    Screen.Cursor := crHourGlass;
    if Version.DoBackup(ProjectVersionBackupForm.Selected,OnCommit) then begin
      UpdateCommitMemo; // refresh last logs
      F.Show;
    end else // close will call F.Free -> idem as ShowModal
      F.Free;
  finally
    Screen.Cursor := crDefault;
  end;
end;
var i, s: integer;
    sel, b: string;
begin
  sel := Version.Params['LastBackup']; // LastBackup=01
  with ProjectVersionBackupForm.CheckListBox.Items do begin
    Clear;
    for i := 0 to 9 do begin
      b := Version.Params['BackupDir'+IntToStr(i)];
      if b='' then continue;
      AddObject(format('%s - %s',[ValAt(b,0),ValAt(b,2)]),pointer(i));
      if pos(chr(i+48),sel)>0 then
        ProjectVersionBackupForm.CheckListBox.Checked[Count-1] := true;
    end;
  end;
  ProjectVersionBackupForm.Caption :=
    format(' %s - %s',[Version.ReleaseName,BtnBackup.Hint]);
  case ProjectVersionBackupForm.ShowModal of
  mrCancel, mrNone:
    exit;
  mrAll:
    Backup;
  mrOk: begin
    sel := '';
    for i := 0 to high(ProjectVersionBackupForm.Selected) do begin
      s := ProjectVersionBackupForm.Selected[i];
      sel := sel+chr(s+48);
    end;
    if sel<>Version.Params['LastBackup'] then // LastBackup=01
      Version.Commits.WriteString('Params','LastBackup',sel); // WriteString->modified=true
    Backup;
  end;
  end;
end;

procedure TMainForm.BtnCommitClick(Sender: TObject);
var i, s: integer;
    sel, temp: string;
procedure Commit;
var F, FT: TForm;
    aSCR: integer;
    OnFile: TOnCommitFileUpdate;
    i, v: integer;
    desc, comment: string;
    Note: TStringWriter;
begin
  OnFile := nil;
  with ProjectVersionCommitForm.SCR do
    if ItemIndex<=0 then
      aSCR := 0 else begin
      aSCR := integer(Items.Objects[ItemIndex]);
      if ProjectVersionCommitForm.PVCS.Checked then begin
        OnFile := OnCommitFileUpdate; // will fill CommitFiles
        Version.Params['CommitToPVCS'] := 'True';
      end else
        Version.Params['CommitToPVCS'] := 'False';
    end;
  F := CommitLogForm(ProjectVersionCommitForm.Caption);
  try
    Screen.Cursor := crHourGlass;
    desc := trim(ProjectVersionCommitForm.Description.Text);
    comment := trim(ProjectVersionCommitForm.Comments.Text);
    v := Version.DoCommit(ProjectVersionCommitForm.Selected,
        desc, comment, aSCR, OnCommit, OnFile);
    if v>=0 then begin
      temp := Version.FileName;
      Version.Free; // will force write commits data on disk
      Version := TVersions.Create(temp);
      UpdateCommitMemo; // refresh last logs
      F.Show; // close will call F.Free -> idem as ShowModal
      UpdateVersions;
      if (CommitFiles<>nil) and // add files to PVCS as modules
         (MessageDlgFmt(sExportToPVCS,[])=mrYes) then begin
        FT := TrackerLogin(false);
        if FT<>nil then
          if FT=ProjectTrackerLoginForm then
          with ProjectTrackerLoginForm.Tracker do // create the note
          try
            RecordUpdate(aSCR,false,false); // select and open record for update
            NoteCreate;                     // create a note
            NoteTitle := format(sExportToPVCSNNN,[desc, // set note title
              ValAt(ExtractFileName(Version.FileName),0,'.'),v]);
            Note.Init; // get note content
            if comment<>'' then
              Note.Add(comment).AddCRLF;
            for i := 0 to CommitFiles.Count-1 do
              Note.Add(Version.FileNames.Value[integer(CommitFiles.Objects[i])]).
                Add(' ').Add(CommitFiles[i]).AddCRLF;
            dec(Note.len,2); // ignore last AddCRLF (we have CommitFiles.Count>=1)
            NoteData := Note.Data; // set note content
            RecordCommit;          // save record changes
            OnCommit(#13#10'  '+format(sExportedToPVCSN,[aSCR]));          
          except
            on E: TPVCSTrackerException do begin // error for this entry
              OnCommit(#13#10'  !!! '+E.Message);
            end;
          end else
            DeleteTempForm(FT);  // should never happen
      end;
    end else begin
      F.Free;
      MessageDlg(SNothingNew,mtInformation,[mbOk],0);
    end;
  finally
    Screen.Cursor := crDefault;
    if (@OnFile<>nil) and (ProjectTrackerLoginForm.Tracker<>nil) then
    try
      ProjectTrackerLoginForm.Tracker.RecordCancel; // cancel changes (=unlock)
      ProjectTrackerLoginForm.Tracker.RecordOpened := false; // free handles
    finally
      FreeAndNil(CommitFiles);
    end;
  end;
end;
begin
  ProjectVersionCommitForm.Init(
    format('%s - %s',[Version.ReleaseName,BtnCommit.Hint]),Version);
  case ProjectVersionCommitForm.ShowModal of
  mrCancel, mrNone:
    exit;
  mrAll:
    Commit;
  mrOk: begin
    sel := '';
    for i := 0 to high(ProjectVersionCommitForm.Selected) do begin
      s := ProjectVersionCommitForm.Selected[i];
      sel := sel+chr(s+48);
    end;
    if sel<>Version.Params['LastUpdate'] then // LastUpdate=01
      Version.Commits.WriteString('Params','LastUpdate',sel);
    Commit;
  end;
  end;
end;

procedure TMainForm.OnCommit(const Text: string);
var Clock: cardinal;
begin
  if Assigned(CommitMemo) then begin
    if CommitMemoText='' then
      CommitMemoText := Text else
      CommitMemoText := CommitMemoText+#13#10+Text;
    Clock := GetTickCount; // refresh screen every 0.5 s
    if Clock-cardinal(CommitMemo.Tag)>500 then begin
      if not TForm(CommitMemo.Parent).Visible then
        TForm(CommitMemo.Parent).Show;
      UpdateCommitMemo(false);
      Application.ProcessMessages; // will do refresh
      CommitMemo.Tag := Clock;
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // used also in BtnBackupClick() and TProMainForm.SectionsMouseDown
  Action := caFree; // close=Free
end;

procedure TMainForm.UpdateCommitMemo(LastCommit: boolean);
var P: PChar;
begin
  with CommitMemo.Lines do begin
    BeginUpdate;
    P := pointer(CommitMemoText);
    if P<>nil then begin
      repeat
        Add(GetNextLine(P,P));
      until P^=#0;
      CommitMemoText := '';
    end;
    if LastCommit then
      CommitMemo.Lines.Add(#13#10+sPressEchapToCloseForm);
    EndUpdate;
    SendMessage(CommitMemo.Handle, EM_SCROLLCARET, 0,0); // not done before
  end;
end;

procedure TMainForm.BtnPROClick(Sender: TObject); // Sender=nil -> no ShowModal
begin
  if (Version.PROFileName='') or not FileExists(Version.PROFileName) then
    exit; { TODO : new .pro -> call documentation Wizard }
  if (ProMainForm.Data=nil) or (ProMainForm.Data.FileName<>Version.PROFileName) then begin
    InitBtnHistory(ProMainForm.Editor,Version.PROFileName);
    ProMainForm.InitEditor(
      BtnPRO.Hint+' - '+Version.ExtractDefaultPath(Version.PROFileName),
      Version.PROFileName);
  end;
  if (Sender<>nil) and not ProMainForm.Visible then
    ProMainForm.ShowModal;
end;

procedure TMainForm.BtnManualClick(Sender: TObject);
begin
  if (Version.MANFileName='') or not FileExists(Version.MANFileName) then
    exit; { TODO : new .man -> call Version.MANForceCreate }
  if MANEditor=nil then
    MANEditor := TProMainForm.Create(Application);
  if (MANEditor.Data=nil) or (MANEditor.Data.FileName<>Version.MANFileName) then begin
    InitBtnHistory(MANEditor.Editor,Version.MANFileName);
    MANEditor.InitEditor(
      BtnManual.Hint+' - '+Version.ExtractDefaultPath(Version.MANFileName),
      Version.MANFileName);
  end;
  if Sender<>nil then
    MANEditor.ShowModal;
end;

procedure TMainForm.InitBtnHistory(Editor: TFrameEditor; const aFileName: string);
var Menu: TMenuItem;
function NewMenu(const Name, Hint: string; Kind: integer): TMenuItem;
begin
  result := TMenuItem.Create(Owner);
  result.Caption := Name;
  result.Hint := Hint;
  result.Tag := Kind;
  result.OnClick := ProjectVersionCompareForm.OnEditorBtnHistoryClick;
  Menu.Add(result);
end;
var i: integer;
    FN, b: string;
begin
  FN := Version.ExtractDefaultPath(aFileName);
  if not Version.FileNames.Find(FN) then begin
    Editor.BtnHistory.Visible := false;
    exit;
  end;
  Editor.BtnHistory.OnClick := ProjectVersionCompareForm.OnEditorBtnHistoryClick;
  Editor.BtnHistory.DropdownMenu.Items.Clear;
  Menu := Editor.BtnHistory.DropdownMenu.Items;
  NewMenu('Commits',Version.FileName,-1); // Tag=-1 -> commits = Version
  for i := 0 to 9 do begin
    b := Version.Params['BackupDir'+IntToStr(i)];
    if b='' then continue;
    NewMenu(ValAt(b,0),ValAt(b,2),i);
  end;
  Editor.BtnHistory.Visible := true;
  ProjectVersionCompareForm.Init(Version,Editor,FN);
end;

procedure TMainForm.EditSCR;
begin
  if EditText(SCRForm,SCREditor,false) then begin // returns true if ok to save
    Version.SCR.SaveToFile;
    UpdateVersions;
  end;
end;

procedure TMainForm.BtnPROWizardClick(Sender: TObject);
begin
  if (Version.PROFileName='') or not FileExists(Version.PROFileName) then begin
    // new .pro -> init documentation Wizard
    if MessageDlg(sNoDocumentCreateQuery,mtConfirmation,mbYesNoCancel,0)<>mrYes then
      exit;
    ProjectDocWizard.DVSParams := Version.Params; // <>nil will create .pro only
    ProjectDocWizard.Data := nil; // will force new release wizard
    if (ProjectDocWizard.ShowModal=mrOK) and (Version.Params['PRO']<>'') then begin
       // a new .pro has been created -> PRO=... must be saved
      Version.Commits.Modified := true;
      Version.ReadParamsFromCommits; // force update PROFileName
    end;
    // ProjectDocWizard.Data will be automaticaly freed in TProjectDocWizard next time
    ProjectDocWizard.DVSParams := nil;
  end;
  BtnPROClick(nil); // Sender=nil -> no ShowModal -> just update ProMainForm.Editor+Data
  if (ProMainForm.Editor.Memo.Lines.Text='') and
     (ProMainForm.Data<>nil) then begin // we have to force TextAll+Memo.Lines
    ProMainForm.DataChangedInEditorSection(nil,ProMainForm.Data.Text);
    ProMainForm.Data.Modified := false;
  end;
  ProMainForm.Editor.BtnWizardClick(nil);
end;

procedure TMainForm.BtnReleaseNewClick(Sender: TObject);
begin
  ProjectDocWizard.Data := nil; // will force new release wizard
  if (ProjectDocWizard.ShowModal=mrOK) and (ProjectDocWizard.DVSFileName<>'')
    and FileExists(ProjectDocWizard.DVSFileName) then begin
     // a new Release has been created -> open by default
    AddToIniFileName(ProjectDocWizard.DVSFileName,true);
    FreeAndNil(Version);
    Version := TVersions.Create(ProjectDocWizard.DVSFileName);
    UpdateVersions; // refresh screen
  end;
  // ProjectDocWizard.Data will be automaticaly freed in TProjectDocWizard next time
end;

procedure TMainForm.OnCommitFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin // Escape or Return will close form
  if byte(Shift)=0 then
  case Key of
  VK_ESCAPE, VK_RETURN:
    if Sender.InheritsFrom(TCustomForm) then
      TCustomForm(Sender).Close;
  end;
end;

procedure TMainForm.UpdateVersions;
var i: integer;
    back: string;
    M: TMenuItem;
begin
  PagesLeft.UpdateVersions(Version); // force update Pages*.fVersions
  PagesRight.UpdateVersions(Version);
  PopupMenuBackup.Items.Clear;
  if Version=nil then exit;
  for i := 0 to 9 do begin
    back := Version.Params['BackupDir'+IntToStr(i)];
    if back='' then continue;
    M := TMenuItem.Create(Owner);
    M.Caption := ValAt(back,0)+#9+ValAt(back,2);
    M.Tag := i;
    M.OnClick := BtnBackupOpenClick;
    PopupMenuBackup.Items.Add(M);
  end;
  Caption := ' '+format(SVersionMain,[Version.ReleaseName,Version.FileName]);
  Application.Title := 'SynProject - '+Version.ReleaseName;
end;

procedure TMainForm.BtnBackupOpenClick(Sender: TObject);
var FN, path: string;
    Back: TVersions;
begin
  if not Sender.InheritsFrom(TMenuItem) then exit;
  FN := Version.GetBackupFileName(
    Version.Params['BackupDir'+IntToStr(TMenuItem(Sender).Tag)],path);
  if not FileExists(FN) then
    exit;
  Back := TVersions.Create(FN);
  try
    ShowVersions(Back);
  finally
    Back.Free;
  end;
end;

procedure TMainForm.OnCommitFileUpdate(aFile: integer; const action: string);
begin
  if CommitFiles=nil then
    CommitFiles := TStringList.Create;
  CommitFiles.AddObject(action,pointer(aFile));
end;

procedure TMainForm.AddToIniFileName(aDVSFileName: string; MakeDefault: boolean);
var i: integer;
begin
  aDVSFileName := ExpandFileName(aDVSFileName);
  if not FileExists(IniFileName) then
    StringToFile(IniFileName,aDVSFileName) else
  with TStringList.Create do
  try
    LoadFromFile(IniFileName);
    CaseSensitive := false;
    i := IndexOf(aDVSFileName);
    if i>=0 then
      Delete(i) else
      while Count>15 do // max 15 entries in history
        Delete(15);
    if MakeDefault then
      Insert(0,aDVSFileName) else // put at first place = default openened file
      Add(aDVSFileName);
    SaveToFile(IniFileName);
  finally
    Free;
  end;
  UpdatePopupMenuOpen(PopupMenuOpen.Items,BtnReleaseOpenClick);
end;

function TMainForm.GetPopupMenuOpenFileName(Menu: TMenuItem): string;
begin
  result := StringReplaceAll(Menu.Caption,'&','');
  if result=sBrowseToOpenAFile then
  with TOpenDialog.Create(self) do
  try
    if Version<>nil then
      InitialDir := ExtractFilePath(Version.FileName);
    DefaultExt := 'dvs';
    Filter := sReleaseFiles+' (*.dvs)|*.dvs';
    FilterIndex := 1;
    Title := result;
    Options := [ofFileMustExist, ofHideReadOnly, ofEnableSizing];
    if Execute then
      result := FileName else
      result := '';
  finally
    Free;
  end;
  if not FileExists(result) then
    result := '';
end;

procedure TMainForm.BtnReleaseOpenClick(Sender: TObject);
var DVS: string;
begin
  DVS := GetPopupMenuOpenFileName(Sender as TMenuItem);
  if (DVS='') or SameText(Version.FileName,DVS) then exit;
  FreeAndNil(Version);
  Version := TVersions.Create(DVS);
  AddToIniFileName(DVS,true);
  UpdateVersions; // refresh screen
end;

procedure TMainForm.UpdatePopupMenuOpen(Menu: TMenuItem; OnClick: TNotifyEvent);
procedure Add(const name: string);
var M: TMenuItem;
begin
  M := TMenuItem.Create(Menu.Owner);
  M.Caption := name;
  M.OnClick := OnClick;
  Menu.Add(M);
end;
var P: PAnsiChar;
    v, DVS: string;
begin
  Menu.Clear;
  FileToString(IniFileName,v);
  P := pointer(v);
  if P<>nil then
  repeat
    DVS := GetNextLine(P,P);
    if FileExists(DVS) then
      Add(DVS);
  until P^=#0;
  if Menu.Count>0 then
    Add('-');
  Add(sBrowseToOpenAFile);
end;

{$ifdef DELPHI6OROLDER}
function GetFileVersion(const AFileName: string): Cardinal;
var FileName: string;
    InfoSize, Wnd: DWORD;
    VerBuf: Pointer;
    FI: PVSFixedFileInfo;
    VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$endif}

procedure TMainForm.BtnAboutClick(Sender: TObject);
begin
  with LongRec(GetFileVersion(paramstr(0))) do
    if MessageDlg(format('SynProject v.%d.%d'#13+
      'Under GPL License'#13'Copyright 2013 Synopse'+
      #13#13'Visit http://synopse.info ?',
      [Hi,Lo]),mtInformation,[mbYes,mbNo],0)=mrYes then
      ShellExecute(Handle,nil,'http://synopse.info',nil,nil,SW_SHOWNORMAL);
end;


initialization
  if SetThreadLocale($0409) then
    GetFormatSettings; // resets all locale-specific variables to LCID_US
  assert(DecimalSeparator='.');

//  ExtractAllResources;
end.

