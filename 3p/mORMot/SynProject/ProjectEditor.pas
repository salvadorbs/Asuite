/// Documentation Editor editor visual frame
// - this unit is part of SynProject, under GPL 3.0 license; version 1.13
unit ProjectEditor;

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

{$ifndef DONTUSEPARSER} // for ProjectVersion, e.g. (must be set globally)
{$define USEPARSER}
// attempt to get information from source code directly

{$define USEGDIPLUSFORIMAGES}
// if defined, GDI+ library will be used for reading jpeg and png images
// (requires Windows XP and later - or GdiPlus.dll in program folder)

{$define WITH_GRAPHVIZ}
// if defined, the WinGraphviz COM server will be used to generated diagrams
// (must be defined in ProjectParser, ProjectEditor and ProjectTypes)
{$endif}

{$define USEGDIPLUSFORIMAGES}
// must be defined by default

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SynMemoEx, ExtCtrls, ImgList, ComCtrls, StdCtrls,
{$ifdef USEGDIPLUSFORIMAGES}
  SynGdiPlus,
{$endif}
  ProjectTypes, ProjectSections, ProjectMemoExSyntax, ProjectSpellCheck,
  ProjectFormSelection, ToolWin, Menus, ExtDlgs;

type
  TOnDataChange = procedure(Sender: TObject; const newData: string) of object;

  TFrameEditor = class(TFrame)
    Memo: TMemoEx;
    ToolBar: TToolBar;
    BtnReadOnly: TToolButton;
    ImageListEnabled: TImageList;
    ImageListDisabled: TImageList;
    BtnWordWrap: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    BtnSave: TToolButton;
    Panel: TPanel;
    Sections: TListBox;
    BtnHistoryBack: TToolButton;
    BtnHistoryNext: TToolButton;
    ToolButton3: TToolButton;
    BtnBold: TToolButton;
    BtnItalic: TToolButton;
    BtnUnderline: TToolButton;
    BtnUndo: TToolButton;
    BtnTextAll: TToolButton;
    BtnLinkSection: TToolButton;
    BtnLinkPeople: TToolButton;
    PopupMenuLink: TPopupMenu;
    BtnLinkPicture: TToolButton;
    FindDialog: TFindDialog;
    BtnDocument: TToolButton;
    BtnFixedFont: TToolButton;
    BtnMarkProgram: TToolButton;
    PopupMenuProgram: TPopupMenu;
    PopupMenuProgramDelphi: TMenuItem;
    PopupMenuProgramC: TMenuItem;
    PopupMenuProgramCSharp: TMenuItem;
    PopupMenuProgramINI: TMenuItem;
    BtnAutoSections: TToolButton;
    BtnAddPicture: TToolButton;
    ToolButton4: TToolButton;
    BtnAddTracker: TToolButton;
    BtnHistory: TToolButton;
    PopupMenuBtnHistory: TPopupMenu;
    BtnReleaseDocument: TToolButton;
    PopupMenuProgramComment: TMenuItem;
    BtnWizard: TToolButton;
    BtnImportTracker: TToolButton;
    PopupMenuProgramModula2: TMenuItem;
    BtnAddGraph: TToolButton;
    BtnSpellCheck: TToolButton;
    BtnAbout: TToolButton;
    ToolButton5: TToolButton;
    BtnLinkProgram: TToolButton;
    PopupMenuProgramXML: TMenuItem;
    PopupMenuProgramDFM: TMenuItem;
    EditorPopup: TPopupMenu;
    EditorPopupCopy: TMenuItem;
    EditorPopupPaste: TMenuItem;
    EditorPopupCut: TMenuItem;
    EditorPopupCopyAs: TMenuItem;
    EditorPopupCopyAsHtml: TMenuItem;
    EditorPopupCopyAsBBCode: TMenuItem;
    N1: TMenuItem;
    EditorPopupSpellCheck: TMenuItem;
    EditorPopupUndo: TMenuItem;
    EditorPopupWordWrap: TMenuItem;
    ImageList16: TImageList;
    procedure BtnReadOnlyClick(Sender: TObject);
    procedure BtnWordWrapClick(Sender: TObject);
    procedure MemoSetCaretPos(Sender: TObject; CaretX, CaretY: Integer);
    procedure SectionsClick(Sender: TObject);
    procedure MemoMouseOver(Sender: TObject; WordStyle: Word;
      var _Cursor: TCursor);
    procedure BtnHistoryBackClick(Sender: TObject);
    procedure BtnHistoryNextClick(Sender: TObject);
    procedure BtnBoldItalicUnderlineClick(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure BtnLinkSectionClick(Sender: TObject);
    procedure SectionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FindDialogFind(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnMarkProgramClick(Sender: TObject);
    procedure BtnAutoSectionsClick(Sender: TObject);
    procedure BtnReleaseDocumentClick(Sender: TObject);
    procedure BtnWizardClick(Sender: TObject);
    procedure BtnAddGraphClick(Sender: TObject);
    procedure BtnSpellCheckClick(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
    procedure BtnLinkProgramClick(Sender: TObject);
    procedure EditorPopupCopyClick(Sender: TObject);
    procedure EditorPopupPasteClick(Sender: TObject);
    procedure EditorPopupCutClick(Sender: TObject);
    procedure EditorPopupCopyAsHtmlClick(Sender: TObject);
    procedure EditorPopupCopyAsBBCodeClick(Sender: TObject);
    procedure EditorPopupPopup(Sender: TObject);
  private
    FParams,
    FTextAll: boolean;
    FLinkClickMenus: TList;
    procedure UpdateSections(KeepSelected: boolean);
    procedure SetTextAll(const Value: boolean);
    procedure MemoWordClick(Sender: TObject; const Clicked: TWordUnderCursor);
    function HistoryAddFromCurrent: boolean;
    function HistoryAdd(const Clicked: TWordUnderCursor): boolean;
    procedure SetParams(const Value: boolean);
    procedure LinkMenuClick(Sender: TObject);
    function CreateTempProject: TProject;
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    procedure UpdateSectionValues(Sec: TSection);
    function TitleLinkParaIndex(TitleID: integer): integer;
    function OnClipboardPaste(Sender: TObject): boolean;
    function InsertPicture(Pic: TPicture; const Title, PicFileName: string): boolean;
    procedure AllTitles(Sender: TObject);
  public
    Data: TSectionsStorage;
    MemoWordClickText: string;
    HistoryMax,
    HistoryCurrent: integer;
    History: array[0..20] of TWordUnderCursor;
    OnDataChange: TOnDataChange;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; 
    function UpdateDataFromTextAllIfNecessary: boolean;
    procedure OnEscKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    property TextAll: boolean read FTextAll write SetTextAll;
    property Params: boolean read FParams write SetParams;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;


procedure EnsureSingleInstance;
{ Application.Initialize;
  EnsureSingleInstance;
  Application.CreateForm(TMainForm, MainForm);
 .... }

function CreateFormEditor(aFile: TSectionsStorage; out E: TFrameEditor;
  NotPro: boolean; const Title: string): TForm;

function EditText(F: TForm; E: TFrameEditor; FreeAtClose: boolean): boolean; // true is modified

/// uncompress an image list from a .zip embedded as a .res to the executable
procedure LoadFromEmbedded(ImgList: TImageList; const ZipName: string);

resourcestring
  sPictureFromFile = 'Picture from file';
  sPictureFromClipboad = 'Picture from Clipboard';
  sMenusParser = ',Refresh from source code,,Parse all source code again,'+
    'Initialize external source code descriptions .sae file,Create external .sae file,'+
    ',Edit external .sae file';
  sMenusGraph = 'Diagrams,Recreate all diagrams';
  sMenuAllSummaryOnly = 'All Summary Sheets only';
  sMenuGraphs = 'Recreate \graph diagrams';
  sAllDocumentsHtml = 'All documents,Web Site Export';
  sEnterPictureFileName = 'Enter Picture internal File Name (with no .png extension):';
  sErrorFileExists = 'This file name already exists. Please enter a genuine one';
  sEnterPictureWidth = 'Enter Picture Width percent (100%):';
  sEnterPictureDescription = 'Enter Picture description:';
  sCompleteSections = 'This will complete sections for the document';
  sNoNewSectionFound = 'No new section found';
  sAddSectionsQuery = 'Do you want to add the following sections';
  sErrorDocModifiedAskSave = 'Document has been modified.'#13#13'Do you want to save changes?';
  sPopupMenuText = 'Format,Mark,Link,Insert,Go to,Create Doc';
  sTitlesDot = 'Titles...';
  sTitlesSharp = 'Titles #';
  sTitlesAll = 'All titles';
  

implementation

{$R *.dfm}

uses
  Clipbrd,
  ProjectCommons, // for UpperPCharCpy
{$ifdef WITH_GRAPHVIZ}
  ProjectGraphEdit,
  ProjectParser,
{$endif}
  ProjectRTF, SynZip,
  ProjectVersionSCR, ProjectEditorRelease, ProjectFormDocWizard,
{$ifdef USEPARSER}
  ProjectEditorProgram,
{$endif}
  ProjectVersionMain; // for keywords

procedure TFrameEditor.BtnReadOnlyClick(Sender: TObject);
begin
  BtnReadOnly.ImageIndex := BtnReadOnly.ImageIndex xor 1;
  Memo.ReadOnly := boolean(BtnReadOnly.ImageIndex);
end;

procedure TFrameEditor.BtnWordWrapClick(Sender: TObject);
var _P, _PI: integer;
    Down: boolean;
begin
  Down := not Memo.WordWrap;
  BtnWordWrap.Down := Down; // manual tbsCheck style for Delphi 5
  Memo.Lines.Index2ParaIndex(Memo.CaretY, _P, _PI);
  Memo.WordWrap := Down;
  EditorPopupWordWrap.Checked := Down;
  if _P>=0 then // set top screen to middle of screen
    Memo.SetLeftTop(0,Memo.Lines.Paragraphs[_P].FPreCount-3);
end;

procedure LoadFromEmbedded(ImgList: TImageList; const ZipName: string);
var i: integer;
    Bmp: TBitmap;
    Stream: TStringStream;
    BW,BH,W,H: integer;
begin
  with TZipRead.Create(HInstance,'Zip','ZIP') do
  try
    i := NameToIndex(ZipName);
    if i<0 then exit;
    Stream := TStringStream.Create(UnZip(i)); // uncompress
    try
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromStream(Stream);
        // from multi-line (i.e. IDE export) into one-line (for AddMasked)
        BW := Bmp.Width; 
        BH := Bmp.Height;
        W := (BW div ImgList.Width);
        H := (BH div ImgList.Height);
        Bmp.Width := W*H*ImgList.Width;
        BH := ImgList.Height;
        for i := 2 to H do
          Bmp.Canvas.CopyRect(Rect((i-1)*BW,0,i*BW,BH),
            Bmp.Canvas,Rect(0,(i-1)*BH,BW,i*BH));
        Bmp.Height := BH;
        // add these images to the image list
        ImgList.AddMasked(Bmp,clFuchsia);
      finally
        Bmp.Free;
      end;
    finally
      Stream.Free;
    end;
  finally
    Free;
  end;
end;

procedure ImageListStretch(ImgListSource, ImgListDest: TImageList;
   BkColor: TColor=clSilver);
var BmpSource, BmpDest: TBitmap;
    i: integer;
    Pic: TSynPicture;
    RS,RD: TRect;
begin
  ImgListDest.Clear;
  if Gdip=nil then
    Gdip := TGDIPlusFull.Create;
  Pic := TSynPicture.Create;
  BmpSource := TBitmap.Create;
  BmpDest := TBitmap.Create;
  try
    RS.Left := 0;
    RS.Top := 0;
    RS.Right := ImgListSource.Width;
    RS.Bottom := ImgListSource.Height;
    BmpSource.Width := RS.Right;
    BmpSource.Height := RS.Bottom;
    RD.Left := 0;
    RD.Top := 0;
    RD.Right := ImgListDest.Width;
    RD.Bottom := ImgListDest.Height;
    BmpDest.Width := RD.Right;
    ImgListDest.Masked := false;
    BmpDest.Height := RD.Bottom;
    for i := 0 to ImgListSource.Count-1 do begin
      BmpSource.Canvas.Brush.Color := BkColor;
      BmpSource.Canvas.Brush.Style := bsSolid;
      BmpSource.Canvas.FillRect(RS);
      ImgListSource.Draw(BmpSource.Canvas,0,0,i);
      Pic.Assign(BmpSource);
      Pic.Draw(BmpDest.Canvas,RD); // GDI+ smooth draw
      ImgListDest.Add(BmpDest,nil);
    end;
  finally
    BmpDest.Free;
    BmpSource.Free;
    Pic.Free;
  end;
end;

destructor TFrameEditor.Destroy;
begin
  FLinkClickMenus.Free;
  inherited;
end;

constructor TFrameEditor.Create(AOwner: TComponent);
function New(Button: TToolButton; const Caption: string='';
  Source: TMenuItem=nil; Tag: integer=0): TMenuItem;
begin
  if Source=nil then
    Source := EditorPopup.Items;
  result := TMenuItem.Create(self);
  result.Caption := Caption;
  if Button<>nil then begin
    if Button.ImageIndex=0 then
      result.ImageIndex := Source.ImageIndex else
      result.ImageIndex := Button.ImageIndex;
    if Caption='' then
      result.Caption := Button.Hint;
    if Tag<>0 then begin
      result.Tag := Tag;
      FLinkClickMenus.Add(result);
    end else begin
      result.Tag := Button.Tag;
      result.OnClick := Button.OnClick;
    end;
  end;
  Source.Add(result);
end;
var Mark, M: TMenuItem;
    i: integer;
    P: PChar;
begin
  inherited;
  LoadFromEmbedded(ImageListEnabled,'FrameEditorEnabled.bmp');
  LoadFromEmbedded(ImageListDisabled,'FrameEditorDisabled.bmp');
  ImageListStretch(ImageListEnabled,ImageList16,clWhite);
  BtnWordWrapClick(nil); // update Memo.WordWrap according to button
  Memo.ClipPasteRtfBackSlashConvert := true; // '\' -> '\\' for paste
  Memo.OnClipboardPaste := OnClipboardPaste;
  if Screen.Fonts.IndexOf('Consolas')>=0 then
    Memo.Font.Name := 'Consolas';
{$ifdef WITH_GRAPHVIZ}
  BtnAddGraph.Visible := true;
{$endif}
  FLinkClickMenus := TList.Create;
  P := pointer(string(sPopupMenuText));
  Mark := New(nil,GetNextItem(P));
  Mark.ImageIndex := BtnItalic.ImageIndex;
  New(BtnBold,'',Mark);
  New(BtnItalic,'',Mark);
  New(BtnUnderline,'',Mark);
  New(BtnFixedFont,'',Mark);
  Mark := New(nil,GetNextItem(P));
  Mark.ImageIndex := BtnMarkProgram.ImageIndex;
  for i := 0 to PopupMenuProgram.Items.Count-1 do
    with PopupMenuProgram.Items[i] do begin
      M := New(BtnMarkProgram,Caption,Mark);
      M.Hint := Hint;
      M.ShortCut := ShortCut;
    end;
  Mark := New(nil,GetNextItem(P));
  Mark.ImageIndex := BtnLinkPeople.ImageIndex;
  New(BtnLinkSection,'',Mark,1);
  New(BtnLinkPeople,'',Mark,2);
  New(BtnLinkPicture,'',Mark,3);
  New(BtnLinkProgram,'',Mark);
  Mark := New(nil,GetNextItem(P));
  Mark.ImageIndex := BtnAddPicture.ImageIndex;
  New(BtnAddPicture,'',Mark,10);
  New(BtnAddGraph,'',Mark);
  New(BtnLinkSection,GetNextItem(P),nil,20).ImageIndex := BtnLinkSection.ImageIndex;
  New(BtnDocument,GetNextItem(P),nil,21);
end;

procedure TFrameEditor.SetTextAll(const Value: boolean);
begin
  FTextAll := Value;
  Sections.Visible := Value;
  BtnHistoryBack.Visible := Value;
  BtnHistoryNext.Visible := Value;
  BtnAutoSections.Visible := Value;
  BtnReleaseDocument.Visible := Value;
  BtnWizard.Visible := Value;
  BtnTextAll.Visible := not Value;
  BtnSave.Visible := Value and Assigned(BtnSave.OnClick);
  UpdateSections(false);
  if Value then
    Memo.OnWordClick := MemoWordClick;
end;

procedure TFrameEditor.SetParams(const Value: boolean);
begin
  TextAll := false;
  FParams := Value;
  BtnBold.Visible := not Value;
  BtnItalic.Visible := not Value;
  BtnUnderline.Visible := not Value;
  BtnDocument.Visible := not Value;
  if Value then
    Memo.OnGetLineAttr := TProjectSyntax.IniGetLineAttr else
    Memo.OnGetLineAttr := TProjectSyntax.BodyGetLineAttr;
end;

procedure TFrameEditor.UpdateSections(KeepSelected: boolean);
var Select, s: string;
    i: integer;
begin
  if KeepSelected then begin
    i := Sections.ItemIndex;
    if i>=0 then
      Select := Sections.Items[i] else
      Select := '';
  end;
  Sections.Items.BeginUpdate;
  Sections.Items.Clear;
  with Memo.Lines do
    for i := 0 to Count-1 do
    with Paragraphs[i]^ do
      if FCount>0 then begin
        s := FStrings[0];
        if (s<>'') and (s[1]='[') then begin
          s := TSectionsStorage.TrimBrackets(s);
          if s<>'' then
            Sections.Items.Add(s);
        end;
      end;
  if KeepSelected then
    Sections.ItemIndex := Sections.Items.IndexOf(Select);
  Sections.Items.EndUpdate;
end;

procedure TFrameEditor.MemoSetCaretPos(Sender: TObject; CaretX, CaretY: Integer);
var Para, ParaIndex, i: integer;
    s: string;
begin // update Sections selected item from current position (fast code)
  Memo.Lines.Index2ParaIndex(CaretY,Para,ParaIndex);
//  TForm(Owner).Caption := format('%d,%d,Line[%d]=%s',
//    [CaretX,CaretY,Para,Memo.Lines.Paragraphs[Para].FStrings[0]]);
  with Memo.Lines do // find [Section] just before Caret Pos
  while Para>=0 do begin
    s := Paragraphs[Para].FStrings[0]; // first string of the paragraph
    if (length(s)>1) and (s[1]='[') then begin
      s := TSectionsStorage.TrimBrackets(s);
      if s<>'' then begin
        i := Sections.ItemIndex;
        if (i<0) or (Sections.Items[i]<>s) then begin
          i := Sections.Items.IndexOf(s);
          if i<0 then begin // new or modified section name -> update list
            UpdateSections(false);
            i := Sections.Items.IndexOf(s);
          end;
          Sections.ItemIndex := i;
        end;
      end;
      exit;
    end;
    dec(Para);
  end;
end;

procedure TFrameEditor.SectionsClick(Sender: TObject);
// if Sender=nil -> search of MemoWordClickText+'='
var index, i, j, y: integer;
    s: string;
begin
  index := Sections.ItemIndex;
  if index<0 then exit;
  if Sender=nil then // Click from Code -> add to History
    HistoryAddFromCurrent;
  s := '['+UpperCase(Sections.Items[index])+']';
  with Memo.Lines do
  for i := 0 to Count-1 do
    with Paragraphs[i]^ do // find [Section] line
      if (FCount>0) and IdemPChar(pointer(FStrings[0]),pointer(s)) then begin
        Memo.SetLeftTop(0,FPreCount); // top screen to [Section] beginning
        Y := FPreCount;
        if Sender=nil then begin // -> search of MemoWordClickText+'=' line
          MemoWordClickText := UpperCase(MemoWordClickText)+'=';
          for j := i+1 to Count-1 do begin
            s := Paragraphs[j].FStrings[0];
            if (s='') or (s[1] in ['[',':']) then break else
            if IdemPChar(pointer(s),pointer(MemoWordClickText)) then begin
              Y := Paragraphs[j].FPreCount;
              break;
            end;
          end;
        end;
        Memo.SetCaret(0,Y); // focus this line
        Memo.SetFocus;
        exit;
      end;
end;

procedure TFrameEditor.MemoMouseOver(Sender: TObject; WordStyle: Word;
  var _Cursor: TCursor);
begin
 if (WordStyle>0) and Assigned(Memo.OnWordClick) then
    _Cursor := crHandPoint;
end;

function TFrameEditor.TitleLinkParaIndex(TitleID: integer): integer;
var j, value: integer;
    line: string;
begin // fast find ':1 title' where TitleID=1
  with Memo.Lines do
  for result := 0 to Count-1 do begin
    line := Paragraphs[result].FStrings[0]; // fast retrieval of beginning of line
    if (line='') or (line[1]<>':') or
       not (line[2] in ['1'..'9']) then continue;
    value := 0;
    j := 2;
    while line[j] in ['0'..'9'] do begin
      value := ord(line[j])-48+value*10;
      inc(j);
    end;
    if value=TitleID then
      exit;
  end;
  result := -1;
end;

function TFrameEditor.OnClipboardPaste(Sender: TObject): boolean;
var Dest: TPicture;
    Value, Enter, Path: string;
begin
  result := false;
  if Clipboard.HasFormat(CF_PICTURE) then begin
    Dest := TPicture.Create;
    try
      Dest.Assign(Clipboard);
      if (Dest.Height<>0) and (Dest.Width<>0) then begin
        Path := ExtractFilePath(Data.FileName);
        if Path='' then
          exit; // need a valid local path to store picture in
        repeat
          if not InputQuery(sPictureFromClipboad,sEnterPictureFileName, Enter) then
            exit;
          Value := ExtractFileName(ChangeFileExt(Enter,'.png'));
          if FileExists(Path+Value) or (Data['Pictures'][Value]<>'') then
            MessageDlg(sErrorFileExists,mtError,[mbOk],0) else
            break;
        until false;
        if InsertPicture(Dest,sPictureFromClipboad,Value) then begin
          SaveAs(Dest,Path+Value,gptPNG);
          result := true; // mark don't get as text
        end;
      end;
    finally
      Dest.Free;
    end;
  end;
end;

procedure TFrameEditor.AllTitles(Sender: TObject);
var i,Para,ParaIndex: integer;
    Value: string;
begin
  with TSelectionForm.Create(self) do // "All titles" window
  try
    Caption := '   '+ExtractFileName(Data.FileName);
    Memo.Lines.Index2ParaIndex(Memo.CaretY,Para,ParaIndex);
    with Memo.Lines do
     for i := 0 to Count-1 do begin
       Value := Paragraphs[i].FStrings[0];
       if Value='' then
         continue;
       case Value[1] of
         '[': Lines.AddObject(Value,TObject(i));
         ':': Lines.AddObject(copy(Value,2,maxInt),TObject(i));
         else continue;
       end;
       if Para>=i then
         Selected := Lines.Count-1;
     end;
     ShowModal;
     if Selected>=0 then begin
       HistoryAddFromCurrent;
       Memo.SetCaretAtParaPos(integer(Lines.Objects[Selected]),0);
       Memo.SetFocus;
     end;
  finally
    Free;
  end;
end;

function TFrameEditor.InsertPicture(Pic: TPicture; const Title, PicFileName: string): boolean;
var Def, Enter: string;
    i, Y: integer;
    Pictures: TSection;
begin
  result := false;
  repeat
    Enter := '100';
    if not InputQuery(Title,sEnterPictureWidth, Enter) then
      exit;
  until StrToIntDef(Enter,0)>0;
  Def := Enter+'%';
  Enter := '';
  if not InputQuery(Title,sEnterPictureDescription, Enter) then
    exit;
  result := true;
  Def := format('%dx%d %s,%s',[Pic.Width,Pic.Height,Def,Enter]);
  Memo.Command(ecBeginLine);
  Memo.InsertTextAtCurrentPos('%'+PicFileName+#13#10);
  if TextAll then begin
    Y := Memo.CaretY;
    i := Sections.Items.IndexOf('Pictures');
    Pictures := Data['Pictures'];
    if Pictures=nil then begin
      Pictures := Data.GetOrCreateSection('Pictures',true);
      Memo.SetCaret(0,0);
      Memo.InsertTextAtCurrentPos('[Pictures]'#13#10#13#10);
      inc(Y,2); // #13#10#13#10 -> 2 lines down
      UpdateSections(false);
      Memo.Command(ecUp);
      Memo.Command(ecUp);
    end else begin
      Sections.ItemIndex := i;
      SectionsClick(nil);
    end;
    Memo.Command(ecDown);
    Pictures[PicFileName] := Def;
    Memo.InsertTextAtCurrentPos(PicFileName+'='+Def+#13#10);
    Memo.SetCaret(0,Y);
  end else
    Data['Pictures'][PicFileName] := Def; // thats enough
end;

procedure TFrameEditor.MemoWordClick(Sender: TObject; const Clicked: TWordUnderCursor);
var DestSection: integer;
    DI, SectionName, SectionNameKind, SectionNameValue, Ext: string;
    i, TitleID, x,y: integer;
begin
  DestSection := -1;
  case Clicked.Style of
  edStyleButton: begin // '@SRS@' '=[KnownIssues]'
    MemoWordClickText := Clicked.Text;
    while (MemoWordClickText<>'') and (MemoWordClickText[1] in ['@','=','[']) do
      delete(MemoWordClickText,1,1);
    while (MemoWordClickText<>'') and (MemoWordClickText[length(MemoWordClickText)] in ['@',']']) do
      SetLength(MemoWordClickText,length(MemoWordClickText)-1);
    if MemoWordClickText='' then exit;
    DestSection := Sections.Items.IndexOf(MemoWordClickText);
    if DestSection<0 then begin // if not @SRS@
      Ext := ExtractFileExt(MemoWordClickText);
      if PWord(MemoWordClickText)^=ord('%')+ord('%')shl 8 then
        exit else // @%%graphPicture@ is ignored (not referenced in [Pictures])
      if GetStringIndex(VALID_PROGRAM_EXT, Ext)>=0 then begin
{$ifdef USEPARSER}
        if not SameText(Ext,'.PAS') or
           (MemoWordClickText[1]<>'!') or (ssCtrl in Clicked.Shift) then begin
{$endif}  // @PC\EIA\main.pas@ or Ctrl + @!PC\EIA\main.pas@ -> go to [SAD-PC] section
          if MemoWordClickText[1]='!' then begin
            delete(MemoWordClickText,1,1);
            i := pos('!',MemoWordClickText);
            if i>0 then // @!TObject!PC\EIA\main.pas@
              delete(MemoWordClickText,1,i);
          end;
          DestSection := Sections.Items.IndexOf(TProject.GetProgramSection(MemoWordClickText));
          MemoWordClickText := TProject.GetProgramName(MemoWordClickText);
{$ifdef USEPARSER}
        end else begin
          // left click on @!PC\EIA\main.pas@ -> edit program link
          if EditProgramForm(CreateTempProject,MemoWordClickText) then begin
            Memo.SelStart := Clicked.TextStart;
            Memo.SelLength := length(Clicked.Text);
            Memo.SelText := '@'+MemoWordClickText+'@'; // replace
          end;
          exit;
        end;
{$endif}
      end else
      if GetStringIndex(VALID_PICTURES_EXT, Ext)>=0 then begin
        // @picture.png@ or @%picture.png@
        if MemoWordClickText[1]='%' then
          delete(MemoWordClickText,1,1);
        DestSection := Sections.Items.IndexOf('Pictures');
      end else
      if Data['People'].Lines.IndexOfName(MemoWordClickText)>=0 then
        // @Michael Jackson@
        DestSection := Sections.Items.IndexOf('People') else
        // @1@ -> ':1 title'
        if TryStrToInt(MemoWordClickText,TitleID) then begin
          i := TitleLinkParaIndex(TitleID);
          if i>=0 then begin
            Memo.Lines.Paragraph2Caret(i,0,x,y);
            HistoryAddFromCurrent;
            Memo.SetCaret(x,y);
            exit;
          end;
        end;
    end;
  end;
  edStylePicture: begin // '%picturename.png'
    MemoWordClickText := copy(Clicked.Text,2,maxInt); // search for 'picturename.png'
    if (MemoWordClickText<>'') and (MemoWordClickText[1]='%') then
      exit; // ignore %%GraphGenerated
    DestSection := Sections.Items.IndexOf('Pictures');
  end;
  edStyleSection: begin // '[SDD-DI-4.1]' -> [SRS-DI-4.1]
    SectionName := UpperCase(TSectionsStorage.TrimBrackets(Clicked.Text));
    TSection.SplitSectionName(SectionName, SectionNameKind, SectionNameValue);
     // 'SRS-DI-4.2' -> 'SRS', 'DI-4.2'
    DI := Data['Project']['MainSection'];
    if SectionName=DI then
      SectionNameValue := 'Project' else
    if SectionNameKind=DI then
      SectionNameValue := DI else begin
      SectionNameKind := Data[SectionNameKind]['Owner'];
      if (SectionNameKind<>'') and (SectionNameKind<>DI) then
        SectionNameValue := SectionNameKind+'-'+SectionNameValue;
    end;
    DestSection := Sections.Items.IndexOf(SectionNameValue);
  end;
  end;
  if DestSection<0 then exit;
  HistoryAdd(Clicked);
  Sections.ItemIndex := DestSection;
  SectionsClick(nil); // Sender=nil -> search of MemoWordClickText+'='
end;

function TFrameEditor.HistoryAdd(const Clicked: TWordUnderCursor): boolean;
begin
  if HistoryCurrent>=high(History) then
    result := false else begin
    result := true;
    if HistoryMax<>HistoryCurrent then
      HistoryCurrent := 0;
    inc(HistoryCurrent);
    HistoryMax := HistoryCurrent;
    History[HistoryCurrent] := Clicked;
    BtnHistoryBack.Enabled := true;
    BtnHistoryNext.Enabled := false;
  end;
end;

function TFrameEditor.HistoryAddFromCurrent: boolean;
var Clicked: TWordUnderCursor;
begin
  fillchar(Clicked,sizeof(Clicked),0);
  Clicked.CaretX := Memo.CaretX;
  Clicked.CaretY := Memo.CaretY;
  result := HistoryAdd(Clicked);
end;

procedure TFrameEditor.BtnHistoryBackClick(Sender: TObject);
var X,Y: integer;
begin
  if HistoryCurrent=0 then exit;
  X := Memo.CaretX;
  Y := Memo.CaretY;
  with History[HistoryCurrent] do
    Memo.SetCaret(CaretX,CaretY);
  Memo.SetFocus;
  dec(HistoryCurrent);
  with History[HistoryCurrent] do begin
    CaretX := X;
    CaretY := Y;
  end;
  BtnHistoryBack.Enabled := HistoryCurrent>0;
  BtnHistoryNext.Enabled := true;
end;

procedure TFrameEditor.BtnHistoryNextClick(Sender: TObject);
begin
  if HistoryCurrent>=HistoryMax then exit;
  with History[HistoryCurrent] do
    Memo.SetCaret(CaretX,CaretY);
  Memo.SetFocus;
  inc(HistoryCurrent);
  BtnHistoryBack.Enabled := true;
  BtnHistoryNext.Enabled := HistoryCurrent<HistoryMax;
end;

procedure TFrameEditor.BtnBoldItalicUnderlineClick(Sender: TObject);
var s: string;
const Cmd: array[0..3] of string = ('{\b ','{\i ','{\ul ','{\f1\fs20 ');
begin
  if Memo.ReadOnly then exit; 
  s := Memo.SelText;
  if s='' then exit;
  Memo.SelText := Cmd[TComponent(Sender).Tag]+s+'}';
end;

procedure TFrameEditor.BtnUndoClick(Sender: TObject);
begin
  if Sender=EditorPopupUndo then
  with Memo.UndoBuffer do
    if (LastUndo<>nil) and (LastUndo.ClassName='TCaretUndo') then
        Memo.Command(ecUndo); // popup right click did change the caret pos
  Memo.Command(ecUndo);
end;

procedure TFrameEditor.MemoChange(Sender: TObject);
begin
  BtnUndo.Enabled := not Memo.IsUndoEmpty;
  EditorPopupUndo.Enabled := BtnUndo.Enabled;
end;

procedure TFrameEditor.BtnLinkSectionClick(Sender: TObject);
var Kind: integer;
function NewMenu(const Name, Hint: string; Menu: TMenuItem): TMenuItem;
function IsChild(const aParent: string; NewMenu,ParentMenu: TMenuItem): boolean;
var i: integer;
    NewFirstMenu: TMenuItem;
begin
  if ParentMenu<>nil then begin
    result := true;
    // test if present in parent
    for i := 0 to ParentMenu.Count-1 do
    with ParentMenu[i] do
    if Caption=aParent then begin
      if Count=0 then begin
        // Menu[i] will be unclickable now -> clone as first child
        NewFirstMenu := TMenuItem.Create(Owner);
        NewFirstMenu.Caption := aParent;
        NewFirstMenu.Hint := Hint;
        NewFirstMenu.Tag := Tag;
        NewFirstMenu.OnClick := OnClick;
        NewFirstMenu.ImageIndex := ImageIndex;
        Add(NewFirstMenu);
      end;
      Add(NewMenu);
      exit;
    end;
    // recursively test if present in children
    for i := 0 to ParentMenu.Count-1 do
      if ParentMenu[i].Count>0 then
        if IsChild(aParent,NewMenu,ParentMenu[i]) then
          exit; // found in children
  end;
  result := false;
end;
var i: integer;
begin
  result := TMenuItem.Create(Owner);
  result.Caption := Name;
  result.Hint := Hint;
  result.Tag := Kind;
  result.OnClick := LinkMenuClick;
  result.ImageIndex := Menu.ImageIndex;
  if Kind<1000 then begin
    i := length(Name);
    while (i>1) and (Name[i]<>'.') do dec(i);
    if i>1 then
      if IsChild(copy(Name,1,i-1),result,Menu) then
        exit; // avoid insert same menu twice
  end;
  Menu.Add(result); // insert menu at default level
end;
var i,j,k: integer;
    s, aName, aValue: string;
    Menu, aMenu, PopupMenuLink: TMenuItem;
    Str: TStringList;
    Sec, Test: TSection;
    P: PChar;
begin
  if (Data=nil) or Memo.ReadOnly then exit;
  if Sender is TMenuItem then begin // from Editor popup menu
    PopupMenuLink := TMenuItem(Sender);
    case PopupMenuLink.Tag of
      // link
      1: Sender := BtnLinkSection;
      2: Sender := BtnLinkPeople;
      3: Sender := BtnLinkPicture;
      // insert
      10: Sender := BtnAddPicture;
      // jump
      20: Sender := Sections;
      21: Sender := BtnDocument;
    end;
  end else begin
    PopupMenuLink := self.PopupMenuLink.Items;
    if Sender is TToolButton then
      PopupMenuLink.ImageIndex := TToolButton(Sender).ImageIndex;
  end;
  PopupMenuLink.Clear;

  // Sections -> menu
  if (Sender=BtnLinkSection) or (Sender=Sections) then begin
    if Sender=Sections then
      Kind := 5 else // MenuItem.Tag=5 -> jump Section
      Kind := 1; // MenuItem.Tag=1 -> @Section@
    with Data.Sections do
    for i := 0 to Count-1 do
    with Items[i] do
      if SectionName=SectionNameValue then begin
        Menu := NewMenu(SectionName,Hint,PopupMenuLink);
        for j := 0 to Count-1 do begin
          Sec := Items[j];
          if Sec.SectionNameKind=SectionName then
            NewMenu(Sec.SectionName,Sec.Hint,Menu);
        end;
        if Menu.Count=1 then // no Child?
          if Data[SectionName]['Revision']<>'' then // true doc has Revision=..
            Menu.Clear else // Revision=document
            with PopupMenuLink do
              Delete(Count-1); // nothing to add -> delete this item
      end;
    Menu := nil;  // ':1 Title' -> create "Titles..." submenu
    Str := TStringList.Create;
    aName := '';
    with Memo.Lines do
    for i := 0 to Count-1 do begin
      s := Paragraphs[i].FStrings[0]; // fast retrieval of beginning of line
      if s='' then continue;
      if s[1]='[' then // remember section name
        aName := s else
      if s[1]=':' then begin
        if s[2] in ['1'..'9'] then begin
          if Menu=nil then begin // create "Titles..." submenu only if necessary
            NewMenu('-','',PopupMenuLink);
            Menu := NewMenu(sTitlesDot,'',PopupMenuLink);
            Menu.ImageIndex := BtnTextAll.ImageIndex;
          end;
          s[1] := ' '; // erase left ':'
          kind := 0;
          j := 2;
          while s[j] in ['0'..'9'] do begin
            kind := ord(s[j])-48+kind*10;
            inc(j);
          end;
          s := aName+' '+IntToStr(kind)+' '+trim(copy(s,j,maxInt));
          k := Str.Count;
          for j := 0 to Str.Count-1 do
            if integer(Str.Objects[j])shr 16>kind then begin
              k := j;
              break;
            end;
          Str.InsertObject(k,s,TObject(kind shl 16+i));
          if Sender=Sections then
             // jump paraIndex Tag-10000
            kind := 10000+i else 
             // insert @Tag-1000@
            inc(kind,1000);
          NewMenu(s,'',Menu);
        end;
      end;
    end;
    if Str.Count>0 then begin
      Menu := NewMenu(sTitlesSharp,'',PopupMenuLink);
      Menu.ImageIndex := BtnTextAll.ImageIndex;
      for i := 0 to Str.Count-1 do begin
        kind := integer(Str.Objects[i]); // insert @Tag-1000@
        if Sender=Sections then
          kind := 10000+kind and $ffff else
          kind := (kind shr 16)+1000;
        NewMenu(Str[i],'',Menu);
      end;
    end;
    Str.Free;
    if Sender=Sections then begin
      Kind := 20; // MenuItem.Tag=20 -> "All titles" window
      Menu := NewMenu(sTitlesAll,'',PopupMenuLink);
      Menu.ImageIndex := BtnTextAll.ImageIndex;
      Menu.ShortCut := VK_F10;
      exit; // no auto popup from Sections TListbox
    end;
  end else

  // People -> menu
  if Sender=BtnLinkPeople then begin
    Kind := 2; // MenuItem.Tag=2 -> People
    Data.ReadOpen('People',true);
    while Data.ReadNextNameValue(aName,aValue) do
      NewMenu(aName,ValAt(aValue,0),PopupMenuLink);
  end else

  // Picture -> menu
  if Sender=BtnAddPicture then begin
    Menu := PopupMenuLink;
    Kind := 4; // MenuItem.Tag=4 -> New Picture
    NewMenu(sPictureFromFile,'',Menu);
    NewMenu('-','',Menu);
    Kind := 3; // MenuItem.Tag=3 -> Picture
    Data.ReadOpen('Pictures',true);
    while Data.ReadNextNameValue(aName,aValue) do
      NewMenu(aName,ValAt(aValue,1),Menu).ImageIndex := BtnLinkPicture.ImageIndex;
  end else

  // Document -> menu
  if Sender=BtnDocument then begin
    for i := 0 to Data.Sections.Count-1 do
    with Data.Sections[i] do
    if (SectionName=SectionNameValue) and (Value['Owner']<>'') then begin
      // every document has a value Owner= param
      aName := Value['Name'];
      if aName='' then continue;
      aValue := Value['Purpose'];
      if aValue<>'' then
        aValue := aName+': '+aValue else
        aValue := aName;
      Kind := 6; // MenuItem.Tag=6 -> Document
      Menu := NewMenu(SectionName,aValue,PopupMenuLink);
      if Value['Source']<>'' then begin // [SAD] document
        NewMenu(SectionName,aValue,Menu);
        NewMenu(SectionName+' pdf',aValue,Menu);
{$ifdef USEPARSER}
        NewMenu('-','',Menu);
        Kind := 13; // MenuItem.Tag=13 -> Refresh
        s := sMenusParser;
        P := pointer(s);
        NewMenu(GetNextItem(P),GetNextItem(P),Menu).ImageIndex := BtnLinkProgram.ImageIndex;
        Kind := 14; // MenuItem.Tag=14 -> Rebuild
        NewMenu(GetNextItem(P),GetNextItem(P),Menu).ImageIndex := BtnLinkProgram.ImageIndex;
        for j := 0 to Data.Sections.Count-1 do
          if Data.Sections[j].SectionNameKind=SectionName then
          if isTrue(Data.Sections[j]['ExternalDescription']) then begin
            Kind := 18; // MenuItem.Tag=18 -> .sae recreate
            NewMenu(GetNextItem(P),GetNextItem(P),Menu).ImageIndex := BtnLinkProgram.ImageIndex;
            Kind := 19; // MenuItem.Tag=19 -> .sae edit
            NewMenu(GetNextItem(P),GetNextItem(P),Menu).ImageIndex := BtnLinkProgram.ImageIndex;
            break;
          end;
{$endif}
{$ifdef WITH_GRAPHVIZ}
        Kind := 15; // MenuItem.Tag=15 -> Graphs refresh
        s := sMenusGraph;
        P := pointer(s);
        aMenu := NewMenu(GetNextItem(P),GetNextItem(P),Menu);
        aMenu.ImageIndex := BtnAddGraph.ImageIndex;
        for j := high(VALID_PICTURES_EXT) downto 1 do
          NewMenu(VALID_PICTURES_EXT[j], '', aMenu);
{$endif}
      end else
      if Value['DocByDescription']<>'' then  begin // [Tests] document
        Test := Data.GetOrCreateSection(Value['DocByDescription'],false); // [Test]
        // GetOrCreateSection() instead of Section[] -> obfuscate GPF with caching
        Kind := 12; // MenuItem.Tag=12 -> Test Document
        for j := 0 to Data.Sections.Count-1 do begin // manually add all Test documents
          Sec := Data.Sections[j];
          if (Sec.SectionNameKind=Test.SectionName) and (Sec['Description']<>'') then begin
            aName := Sec['DocName'];
            if aName='' then
              aName := Sec.SectionName else
              aName := Test.SectionName+' '+aName;
            NewMenu(aName,Sec.SectionName+': '+Sec['Description'],Menu);
          end;
        end;
        NewMenu('-','',Menu);
        Kind := 11; // MenuItem.Tag=11 -> All Test Documents
        NewMenu(Value['Purpose'],Test.SectionName,Menu); // Hint=Test
        Kind := 16; // MenuItem.Tag=16 -> Summary Sheets only
        NewMenu(sMenuAllSummaryOnly,SectionName,Menu); // Hint=Tests
      end else
      if SameText(SectionName,'Risk') then begin
        Kind := 7; // MenuItem.Tag=7 -> Risk Assessment Document
        NewMenu(sRiskAssessment,'',PopupMenuLink);
      end;
    end;
    Menu := PopupMenuLink;
{$ifdef WITH_GRAPHVIZ}
    Kind := 17; // MenuItem.Tag=17 -> \graph refresh
    NewMenu(sMenuGraphs,'',Menu).ImageIndex := BtnAddGraph.ImageIndex;
{$endif}
    if Menu.Count>0 then begin // All documents only if any document ;)
      NewMenu('-','',Menu);
      Kind := 8; // MenuItem.Tag=8 -> All Documents
      P := pointer(string(sAllDocumentsHtml));
      NewMenu(GetNextItem(P),'',Menu);
      Kind := 21; // MenuItem.Tag=21 -> Web Site Export
      NewMenu(GetNextItem(P),'',Menu).ImageIndex := BtnWizard.ImageIndex;
    end;
    i := Sections.ItemIndex; // preview doc of the current subsection
    if i>=0 then begin
      Sec := Data[Sections.Items[i]];
      if (Sec<>nil) and (Sec.SectionNameKind<>Sec.SectionNameValue) and
         (Data[Sec.SectionNameKind]['Owner']<>'') then begin
        Kind := 9; // MenuItem.Tag=9 -> Current Section
        NewMenu(Sec.DisplayName(Data[Sec.SectionNameKind]),Sec.SectionName,Menu).ShortCut := vk_F9;
      end;
    end;
  end else

  // Picture link -> menu
  if Sender=BtnLinkPicture then begin
    Kind := 10; // MenuItem.Tag=10 -> Picture link
    Data.ReadOpen('Pictures',true);
    while Data.ReadNextNameValue(aName,aValue) do
      NewMenu(aName,ValAt(aValue,1),PopupMenuLink);
  end else
    // invalid call
    exit;
    
  // display the popup menu from the corresponding button position
  if PopupMenuLink.Count>0 then
    if (PopupMenuLink=self.PopupMenuLink.Items) and (Sender is TToolButton) then
      with ClientToScreen(TToolButton(Sender).BoundsRect.TopLeft) do
        Self.PopupMenuLink.Popup(X,Y+BtnLinkSection.Height);
end;

procedure TFrameEditor.LinkMenuClick(Sender: TObject);
var Menu: TMenuItem absolute Sender;
    Value: string;
    i: integer;
    Pic: TPicture;
    Project: TProject;
    Test: PDocument;
    Pictures: TSection;
begin
  if not Sender.InheritsFrom(TMenuItem) then exit;
  if Menu.Count>0 then exit; // parent menu item
  Value := StringReplaceAll(Menu.Caption,'&','');
  case Menu.Tag of
  1,2: Memo.InsertTextAtCurrentPos('@'+Value+'@'); // people + sections
  3:   begin
    Memo.Command(ecBeginLine);
    Memo.InsertTextAtCurrentPos('%'+Value+#13#10);
  end;
  4: with TOpenPictureDialog.Create(Application) do
  try // New Picture
    Title := Value;
    Filter := 'Images files|*';
    for i := 0 to high(VALID_PICTURES_EXT)-1 do
      Filter := Filter+VALID_PICTURES_EXT[i]+';*';
    Filter := Filter+VALID_PICTURES_EXT[high(VALID_PICTURES_EXT)];
    Value := ExtractFilePath(Data.FileName);
    if Value='' then
      InitialDir := GetCurrentDir else
      InitialDir := Value;
    Options := Options+[ofFileMustExist,ofNoChangeDir];
    if Execute and FileExists(FileName) then begin
      Value := ExtractFileName(FileName);
      Pictures := Data['Pictures'];
      if Pictures[Value]<>'' then begin // already in [Pictures] ?
        Memo.Command(ecBeginLine);
        Memo.InsertTextAtCurrentPos('%'+Value+#13#10);
        exit;
      end;
      Pic := TPicture.Create;
      try
        Pic.LoadFromFile(FileName);
        InsertPicture(Pic,Title,Value);
      finally
        Pic.Free;
      end;
    end;
  finally
    Free;
  end;
  5: begin
    i := Sections.Items.IndexOf(Value);
    if i<0 then exit;
    Sections.ItemIndex := i;
    SectionsClick(nil);
  end;
  6,7,8,9,11,12,13,14,15,16,17,18,19,21,22: begin
    Project := CreateTempProject;
    try
      Screen.Cursor := crHourGlass;
      case Menu.Tag of
      6: begin
        if copy(Value,length(Value)-3,10)=' pdf' then begin
          Project.CreateDefaultDocument(Copy(Value,1,length(Value)-4),nil,false,fPdf);
        end else
          Project.CreateDefaultDocument(Value,nil,false,fDoc);
        Project.DestroyOpensCreatedDocuments := true; //  Destroy will launch doc
      end;
      7: begin
        Project.CreateRiskAssessmentTable;
        Project.DestroyOpensCreatedDocuments := true; //  Destroy will launch doc
      end;
      8: begin // all documents
        Project.CreateRiskAssessmentTable;
        for i := 0 to Data.Sections.Count-1 do
          with Data.Sections[i] do
          if (SectionName=SectionNameValue) and (Value['Name']<>'') then
            Project.CreateDefaultDocument(SectionName);
      end;
      9: begin // Current Section
        Project.CreateSectionDocument(StringReplaceAll(Menu.Hint,'&',''));
        Project.DestroyOpensCreatedDocuments := true; //  Destroy will launch doc
      end;
      11: begin // All Test Documents
        Test := Project.DocumentFind(Menu.Hint); // Hint='Test'
        if Test<>nil then
        for i := 0 to high(Test.List) do begin // manually add all Test documents
          if (Test.List[i].Value['Description']<>'') then
            Project.CreateTestDocument(Test.List[i].SectionName);
        end;
      end;
      12: begin // Test Document
        Project.CreateTestDocument(ValAt(Menu.Hint,0,':'));
        Project.DestroyOpensCreatedDocuments := true; //  Destroy will launch doc
      end;
{$ifdef USEPARSER}
      13:  // Refresh
        Project.UpdateSADFileFromSource(false);
      14:  // Rebuild
        Project.UpdateSADFileFromSource(true);
      18: // Create external .sae file
        if MessageDlg(Menu.Hint+#13#10#13#10+sWarningOvewriteExisting,
           mtWarning,[mbYes,mbNo],0)=mrYes then
          Project.CreateExternalSAE;
      19: // Edit external .sae file
        EditSAEForm(Project); 
{$endif}
{$ifdef WITH_GRAPHVIZ}
      15:  // Diagrams
        Project.UpdateSADGraphViz(Value,false);
      17:  // \graph refresh
        Project.UpdateSADGraphViz(Value,true);
{$endif}
      16: begin // Summary Sheets only (Hint='Tests')
        // manually add all Test Summary Sheets
        Project.CreateSummarySheets(Data[Menu.Hint]);
        //  Destroy will launch doc
        Project.DestroyOpensCreatedDocuments := true;
      end;
      21:
        Project.ExportAsHtml;
      end;
    finally
      Screen.Cursor := crDefault;
      Project.Free; // auto-open Doc if Menu.Tag=6,7
    end;
  end;
  10:  // link to picture
    Memo.InsertTextAtCurrentPos('@'+Value+'@');
  20:
    AllTitles(nil);
  else
    if Menu.Tag>10000 then begin
      // ':1 Title' -> jump from Tag=10000+paraIndex
      HistoryAddFromCurrent;
      Memo.SetCaretAtParaPos(Menu.Tag-10000,0);
      Memo.SetFocus;
    end else
    if Menu.Tag>1000 then
      // ':1 Title' -> insert @TitleID@ from Tag=1000+TitleID 
      Memo.InsertTextAtCurrentPos('@'+IntToStr(Menu.Tag-1000)+'@');
  end; // case Menu.Tag of
end;

procedure TFrameEditor.SectionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MouseY: integer;
begin
  if Button=mbRight then begin
    PopupMenuLink.Items.ImageIndex := BtnLinkSection.ImageIndex;
    BtnLinkSectionClick(Sender);
    MouseY := Y;
    with ClientToScreen(Sections.BoundsRect.TopLeft) do
      PopupMenuLink.Popup(X,Y+MouseY+5);
  end;
end;

procedure TFrameEditor.MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var i: integer;
    Sec,SecDoc: TSection;
begin
  if byte(Shift)=0 then // no Shift nor Alt
  case Key of
  VK_F1: if BtnAddTracker.Visible then   // F1 = add tracker entry for .scr editor
           BtnAddTracker.Click;
  VK_F2: BtnMarkProgramClick(
           PopupMenuProgramDelphi);      // F2 = block as Delphi/highlight (!) 
  VK_F3: if FindDialog.FindText<>'' then // F3 = Find Next
           FindDialog.OnFind(nil);
  VK_F6: if Memo.SelLength<>0 then       // F6 = change all \\ into single \
           Memo.SelText := StringReplaceAll(Memo.SelText,'\\','\'); 
  VK_F7: BtnSpellCheckClick(nil);        // F7 = Spell Check
  VK_F8: BtnAddGraphClick(nil);          // F8 = Add/Edit Graph
  VK_F9: begin                           // F9 = Create Word document
    i := Sections.ItemIndex;
    if i>=0 then begin
      Sec := Data[Sections.Items[i]]; // Sec = current Section
      if (Sec<>nil)  then begin
        if Sec.SectionNameKind<>Sec.SectionNameValue then
          SecDoc := Data[Sec.SectionNameKind] else
          SecDoc := Sec;
        if (SecDoc<>nil) and (SecDoc['Name']<>'') then
        with CreateTempProject do
        try
          if SecDoc=Sec then
            CreateDefaultDocument(Sec.SectionName) else // full Doc
            CreateSectionDocument(Sec.SectionName); // section Doc
          DestroyOpensCreatedDocuments := true; //  Destroy will launch doc
        finally
          Free; // auto-open Doc
        end;
      end;
    end;
  end;
  VK_F10:                                // F10 = browse document titles
    AllTitles(nil);
  end else
  if ssAlt in Shift then // Alt Key
  case Key of
  VK_LEFT:  BtnHistoryBackClick(nil);              // Alt + Left = history back
  VK_RIGHT: BtnHistoryNextClick(nil);              // Alt + Right = history next
  end else
  if ssCtrl in Shift then // Ctrl Key
  case Key of             // Ctrl + 0,B,I,U = Fixed,Bold,Italic,Underline font
  ord('0'): BtnBoldItalicUnderlineClick(BtnFixedFont);
  ord('B'): BtnBoldItalicUnderlineClick(BtnBold);
  ord('I'): BtnBoldItalicUnderlineClick(BtnItalic);
  ord('U'): BtnBoldItalicUnderlineClick(BtnUnderline);
  ord('F'): FindDialog.Execute;                       // Ctrl + F = find
  ord('K'):  if not Memo.ReadOnly then                // Ctrl + K = \line
               Memo.InsertTextAtCurrentPos('\line ');
  ord('L'):  if FindDialog.FindText<>'' then          // Ctrl + L = find next
               FindDialog.OnFind(nil);
  ord('S'): if Assigned(BtnSave.OnClick) then         // Ctrl + S = Save
              BtnSave.OnClick(nil);
  end;
end;

procedure TFrameEditor.FindDialogFind(Sender: TObject);
begin
  if not Memo.FindNext(FindDialog.FindText,not(frMatchCase in FindDialog.Options)) then
    FindDialog.CloseDialog;
end;

function TFrameEditor.UpdateDataFromTextAllIfNecessary: boolean;
var Text: string;
begin
  result := false;
  if TextAll and Memo.Modified then begin
    Text := Memo.Lines.Text;
    Data.LoadFromMemory(pointer(Text),length(Text));
    Data.Modified := true;
    result := true;
  end;
end;

procedure TFrameEditor.BtnMarkProgramClick(Sender: TObject);
var Cmd: string;
    Offs, yBegin, yEnd, y: integer;
begin  
  if (Data=nil) or Memo.ReadOnly or (Memo.SelLength=0) or
     not Sender.InheritsFrom(TMenuItem) then exit;
  Cmd := TMenuItem(Sender).Hint;
  Memo.Lines.Caret2Paragraph(0,Memo.SelBegY,yBegin,Offs);
  Memo.Lines.Caret2Paragraph(0,Memo.SelEndY,yEnd,Offs);
  if not(length(Cmd) in [1..2]) or (yBegin>yEnd) or
     (yBegin<0) or (yBegin>=Memo.Lines.Count) then exit;
  Memo.BeginUpdate;
  Memo.SelLength := 0; // avoid deleting some text
  for y := yBegin to yEnd do begin
    Memo.SetCaretAtParaPos(y,0);
    Memo.InsertTextAtCurrentPos(Cmd); // insert program char
  end;
  Memo.EndUpdate;
  Memo.SetFocus;
end;

procedure TFrameEditor.BtnAutoSectionsClick(Sender: TObject);
var i: integer;
    s, IDs: string;
    Sec: TSection;
    Add: TStringList;
    Project: TProject;
    Doc, Owner: PDocument;
begin
  if Memo.ReadOnly then exit;
  if not TextAll and not Assigned(OnDataChange) then exit;
  i := Sections.ItemIndex;
  if i<0 then exit;
  Project := CreateTempProject;
  add := TStringList.Create;
  try
    Sec := Project.Data[Sections.Items[i]];
    if Sec=nil then exit;
    Doc := Project.DocumentFind(Sec.SectionNameKind);
    if Doc=nil then exit;
    if MessageDlg(BtnAutoSections.Hint+'?'#13#13+sCompleteSections+':'#13' '+
      Doc.Params.DisplayName(nil)+' - '+Doc.Params['Name'],
      mtConfirmation,mbYesNoCancel,0)<>mrYes then exit;
    if Doc=Project.DI then begin // DI -> complete from DILayout
      with Project.DILayout do
      for i := 0 to Lines.Count-1 do begin
        s := Lines[i];
        if (s='') or (s[1] in [':',';']) then continue; // '4.1' e.g.
        s := Project.DI.Params.SectionName+'-'+s;
        if Project.Data[s]=nil then
          add.Add(s);
      end;
      IDs := add.Text;
    end else begin // other sections -> complete from Owner
      Owner := Project.DocumentFind(Doc.Owner);
      if Owner<>nil then
      for i := 0 to high(Owner.List) do begin
        if Owner=Project.DI then
          s :=  Sec.SectionNameKind+'-'+Owner.List[i].SectionName else
          s :=  Sec.SectionNameKind+'-'+Owner.List[i].SectionNameValue;
        if Project.Data[s]=nil then begin
          add.AddObject(s,Owner.List[i]);
          IDs := IDs+s+'  ';
          if add.Count mod 5=0 then
            IDs := IDs+#13#10;
        end;
      end;
    end;
    if add.Count=0 then
      MessageDlg(sNoNewSectionFound,mtInformation,[mbOk],0) else
      if MessageDlg(sAddSectionsQuery+':'#13#13+
        IDs,mtConfirmation,mbYesNoCancel,0)=mrYes then begin
        // add new sections:
        s := #13#10;
        for i := 0 to add.Count-1 do begin
          s := s+'['+add[i]+']'#13#10;
          if add.Objects[i]<>nil then
            with TSection(add.Objects[i]) do
              s := s+'; '+SectionName+' - '+Description+#13#10#13#10 else
            s := s+#13#10;
        end;
        if TextAll then begin
          Memo.SelLength := 0; // avoid any selection deletion
          Memo.Command(ecEndDoc);
          Memo.InsertTextAtCurrentPos(s);
          UpdateDataFromTextAllIfNecessary;
          UpdateSections(true);
        end else
          if Assigned(OnDataChange) then
            OnDataChange(Self, Data.Text+s);
      end;
  finally
    add.Free;
    Project.Free;
  end;
end;

function TFrameEditor.CreateTempProject: TProject;
var Name: string;
begin
  Name := ChangeFileExt(Data.FileName,'~.tmp');
  UpdateDataFromTextAllIfNecessary;
  Data.SaveToFile(Name);
  result := TProject.Create(TRTF,Name);
  DeleteFile(Name);
end;

procedure EnsureSingleInstance;
var Wnd: HWnd;
    WndClass, WndText: array[0..255] of char;
begin
  { Try and create a semaphore. If we succeed, then check }
  { if the semaphore was already present. If it was }
  { then a previous instance is floating around. }
  { Note the OS will free the returned semaphore handle }
  { when the app shuts so we can forget about it }
  if (CreateSemaphore(nil, 0, 1,
        PChar(ExtractFileName(Application.ExeName))) <> 0) and
     (GetLastError = Error_Already_Exists) then  begin
    Wnd := GetWindow(Application.Handle, gw_HWndFirst);
    while Wnd <> 0 do begin
      { Look for the other TApplication window out there }
      if Wnd <> Application.Handle then begin
        { Check it's definitely got the same class and caption }
        GetClassName(Wnd, WndClass, Pred(SizeOf(WndClass)));
        GetWindowText(Wnd, WndText, Succ(Length(Application.Title)));
        if (string(WndClass) = Application.ClassName) and
           (WndText = Application.Title) then begin
          { This technique is used by the VCL: post }
          { a message then bring the window to the }
          { top, before the message gets processed }
          PostMessage(Wnd, wm_SysCommand, sc_Restore, 0);
          SetForegroundWindow(Wnd);
          Halt
        end
      end;
      Wnd := GetWindow(Wnd, gw_HWndNext)
    end
  end
end;

function TFrameEditor.GetReadOnly: boolean;
begin
  result := boolean(BtnReadOnly.ImageIndex);
  assert(result=Memo.ReadOnly);
end;

procedure TFrameEditor.SetReadOnly(const Value: boolean);
begin
  BtnReadOnly.ImageIndex := integer(Value);
  Memo.ReadOnly := Value;
  BtnAddPicture.Enabled := not Value;
  BtnAutoSections.Enabled := not Value;
  BtnBold.Enabled := not Value;
  BtnDocument.Enabled := not Value;
  BtnFixedFont.Enabled := not Value;
  BtnItalic.Enabled := not Value;
  BtnLinkPeople.Enabled := not Value;
  BtnLinkPicture.Enabled := not Value;
  BtnLinkSection.Enabled := not Value;
  BtnReleaseDocument.Enabled := not Value;
  BtnUnderline.Enabled := not Value;
end;

function CreateFormEditor(aFile: TSectionsStorage; out E: TFrameEditor;
  NotPro: boolean; const Title: string): TForm;
begin
  result := TForm.Create(Application);
  result.Caption := ' '+Title;
  result.Font.Name := 'Tahoma';
  result.Position := poScreenCenter;
  result.Width := (result.Monitor.Width*2)div 3;
  result.Height := (result.Monitor.Height*2)div 3;
  E := TFrameEditor.Create(result);
  E.Parent := result;
  E.Align := alClient;
  E.BtnSave.Hide;
  if aFile<>nil then begin
    E.Data := aFile;
    E.Memo.Lines.Text := aFile.Text;
    E.Memo.RightMargin := StrToIntDef(
      E.Data.Section['Project'].Value['EditorRightMargin'],100);
  end;
  E.Params := false;
  E.TextAll := true; // must be called after having set E.Memo.Lines
  E.BtnWizard.Hide;
  E.BtnDocument.Hide;
  E.BtnLinkSection.Hide;
  E.BtnLinkPeople.Hide;
  E.BtnAutoSections.Hide;
  E.BtnReleaseDocument.Hide;
  if NotPro then begin
    E.BtnLinkPicture.Hide;
    E.BtnAddPicture.Hide;
    E.BtnDocument.Hide;
  end;
{$ifndef USEPARSER}
  BtnLinkProgramClick.Hide;
{$endif}
  result.KeyPreview := true;
  result.OnKeyDown := E.OnEscKeyDown; // Escape key will close form
end;

function EditText(F: TForm; E: TFrameEditor; FreeAtClose: boolean): boolean; // true is modified
begin
  result := false;
  try
    repeat
      F.ShowModal;
      if E.Memo.Modified then
      case MessageDlg(SErrorDocModifiedAskSave,mtConfirmation,mbYesNoCancel,0) of
        mrYes:
          if E.UpdateDataFromTextAllIfNecessary then
            result := true;
        mrCancel:
          continue;
      end;
      break;
    until false;
  finally
    if FreeAtClose then
      F.Free;
  end;
end;

procedure TFrameEditor.OnEscKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin // Escape key will close form
  if byte(Shift)=0 then
  case Key of
  VK_ESCAPE:
    if Sender.InheritsFrom(TCustomForm) then
      TCustomForm(Sender).Close;
  end;
end;

procedure TFrameEditor.BtnReleaseDocumentClick(Sender: TObject);
var i: integer;
    Sec: TSection;
    Project: TProject;
begin
  if Memo.ReadOnly then exit;
  if not TextAll and not Assigned(OnDataChange) then exit;
  i := Sections.ItemIndex;
  if i<0 then exit;
  Project := CreateTempProject;
  try
    Sec := Project.Data[Sections.Items[i]];
    if Sec=nil then exit;
    if Sec['Revision']='' then begin // some [Test-DI-4.6] have Revision=..
      Sec := Project.Data[Sec.SectionNameKind]; // otherwize -> document edit
      if (Sec=nil) or (Sec['Name']='') then exit; // real documents have Name=..
    end;
    with TProjectEditorReleaseForm.Create(Application) do
    try
      Init(Sec);
      case ShowModal of
        mrOk: UpdateSectionValues(Sec);
      end; // case of
    finally
      Free; // TProjectEditorReleaseForm.Free
    end;
  finally
    Project.Free;
  end;
end;

procedure TFrameEditor.UpdateSectionValues(Sec: TSection);
// change all the [Sec] lines with the values in Sec.Lines
var i,j: integer;
    s: string;
begin
  if TextAll then begin
    s := '['+UpperCase(Sec.SectionName)+']';
    with Memo.Lines do
    for i := 0 to Count-1 do
    with Paragraphs[i]^ do // find [Section] line
    if (FCount>0) and IdemPChar(pointer(FStrings[0]),pointer(s)) then begin
      Memo.SetCaret(0,FPreCount+1); // caret to [Section] beginning
      for j := i+1 to Count-1 do
        with Paragraphs[j]^ do
          if (FCount>0) and IdemPChar(pointer(FStrings[0]),'[') then
            break else
            Memo.Command(ecSelDown); // select every line of this section
      Memo.InsertTextAtCurrentPos(Sec.Lines.Text); // change section lines
      Memo.Command(ecInsertPara); // add blank line
      Memo.SetCaret(0,FPreCount);    // caret to [Section] beginning
      Memo.SetLeftTop(0,FPreCount);  // screen top to [Section] beginning
      break;
    end;
  end else
   OnDataChange(Self,Data.Text);
end;

procedure TFrameEditor.BtnWizardClick(Sender: TObject);
var tmp: TSectionsStorage;
    d: string;
begin
  if not TextAll or Memo.ReadOnly then exit;
  tmp := TSectionsStorage.Create;
  try
    d := Memo.Lines.Text; // very fast
    tmp.LoadFromMemory(pointer(d),length(d));
    if (tmp['Project']=nil) or (tmp['DI']=nil) then
      exit; // must be a true document with [Project] and [DI] sections
    if ProjectDocWizard=nil then // if doc editor is called in TMainForm.FormCreate
      ProjectDocWizard := TProjectDocWizard.Create(Application);
    if Data.FileName='' then
      ProjectDocWizard.Title := BtnWizard.Hint else
      ProjectDocWizard.Title := BtnWizard.Hint+' - '+ExtractFileName(Data.FileName);
    ProjectDocWizard.Data := tmp;
    if ProjectDocWizard.ShowModal=mrOK then begin
      Memo.Lines.Text := tmp.Text;
      Memo.Modified := true;
      UpdateDataFromTextAllIfNecessary;
      UpdateSections(true);
      if Owner.InheritsFrom(TCustomForm) and not TCustomForm(Owner).Visible then
        TCustomForm(Owner).ShowModal;
    end;
  finally
    tmp.Free;
  end;
end;

procedure TFrameEditor.BtnAddGraphClick(Sender: TObject);
{$ifdef WITH_GRAPHVIZ}
var aName, aTitle, Dot, Line, ImageName: string;
    perc, StartIndex, Index, Offs: integer;
    PC: PChar;
    Proj: TProject;
begin
  Memo.Lines.Caret2Paragraph(0,Memo.CaretY,StartIndex,Offs);
  Line := Memo.Lines[StartIndex];
  if IdemPChar(pointer(Line),'\GRAPH') then begin
    // retrieve graph source to be edited
    PC := @Line[7];
    while PC^=' ' do inc(PC);
    aName := GetNextItemTrimed(PC,' ');
    aTitle := PC;
    Index := StartIndex;
    while (Index<Memo.Lines.Count) do begin
      inc(Index);
      Line := Memo.Lines[Index];
      if (Line='') or (Line='\') then
        break;
      Dot := Dot+#13#10+Line;
    end;
    Offs := Memo.Lines.GetParaOffs(StartIndex);
    Memo.SelStart := Offs;
    Memo.SelLength := Memo.Lines.GetParaOffs(Index+1)-Offs;
  end else
    Index := -1;
  with TGraphEditForm.Create(Application) do
  try
    Proj := CreateTempProject;
    try
      SAD := TProjectBrowser.Create(Proj);
      Caption := BtnAddGraph.Hint;
      EditName.Text := aName;
      EditTitle.Text := aTitle;
      Source.Text := trim(Dot);
      if Memo.ReadOnly then
        BtnSave.Enabled := false;
      if (ShowModal<>mrOk) or Memo.ReadOnly then
        exit;
      // Save pressed -> update content
      ImageName := trim(EditName.Text);
      Dot := '\graph '+ImageName;
      aTitle := trim(EditTitle.Text);
      if aTitle<>'' then
        Dot := Dot+' '+aTitle;
      Dot := Dot+#13#10+trim(Source.Text)+#13#10'\'#13#10;
      if Index>=0 then begin
        Memo.SelText := Dot;
        Memo.SelLength := 0;
      end else
        Memo.InsertTextAtCurrentPos(#13#10+Dot);
      // update .emf file on disk and reference in GraphValues.ini
      Dot := WingraphvizFromText(ImageName,Source.Lines,0);
      if DotEngine.Validate(Dot) then begin
          Proj.NeedGraphValues;
          perc := Proj.PercFromTitle(aTitle,ImageName);
          if WingraphvizCreateFile(DotEngine,ImageName,Proj.FileNameDir+GraphDirName,
            '.emf',Dot,GraphDirName,aTitle,Proj.GraphValues,perc) then
            Proj.GraphValues.SaveToFile;
        end;
    finally
      Proj.Free;
    end;
  finally
    Free;
  end;

{$else}
begin // nothing to do
{$endif}
end;

procedure TFrameEditor.BtnSpellCheckClick(Sender: TObject);
begin
  if SpellCheckForm=nil then
    // Application.CreateForm() is buggy if .exe if called with a .pro
    SpellCheckForm := TSpellCheckForm.Create(MainForm);
  SpellCheckForm.SpellCheck(Memo);
end;

procedure TFrameEditor.BtnAboutClick(Sender: TObject);
begin
  MainForm.BtnAboutClick(Sender);
end;

procedure TFrameEditor.BtnLinkProgramClick(Sender: TObject);
{$ifdef USEPARSER}
var Button: string;
{$endif}
begin
{$ifdef USEPARSER}
   if EditProgramForm(CreateTempProject,Button) then
     Memo.InsertTextAtCurrentPos('@'+Button+'@');
{$endif}
end;


const
  BBCODE_TAGS: THtmlTagsSet = (
    ('[b]','[i]','[u]','[em]',#13#10,'','[color=navy]','[color=navy][i]',
     ' ','[quote]','[url=%s]','[quote]','   ','',#13#10,#13#10#13#10'[h]','[ins]',
     '<','>'),
    ('[/b]','[/i]','[/u]','[/em]','','','[/color]','[/i][/color]',
     '','[/quote]','[/url]','[/quote]','','','','[/h]','[/ins]','',''));

function FormatProAs(P: PAnsiChar; const Tags: THtmlTagsSet): AnsiString;
// convert .pro format into html or [bbcode] format
var token: AnsiString;
    B: PAnsiChar;
    Level, L: integer;
    Current: THtmlTags;
    InTable, HasLT, HasGT: boolean;
    InListing: AnsiChar;
    Stack: array[0..20] of THtmlTags; // stack to handle { }
procedure AddListing(const KeyWords: array of string);
label com, str, str2;
var B: PAnsiChar;
    Highlight, CString, PasString, IgnoreKeyWord: boolean;
procedure SetToken;
begin
  SetString(token,B,P-B);
  if HasLT and (Tags[false,hLT][1]<>'<') then
    token := StringReplaceAll(token,'<',Tags[false,hLT]);
  if HasGT and (Tags[false,hGT][1]<>'>') then
    token := StringReplaceAll(token,'>',Tags[false,hGT]);
end;
begin
  if InListing=' ' then
    result := result+Tags[false,hPre];
  InListing := P^;
  inc(P); // always ignore first line char (already handled by caller)
  CString := (@KeyWords=@MODULA2KEYWORDS) or (@KeyWords=@CKEYWORDS)
          or (@KeyWords=@CSHARPKEYWORDS);
  PasString := (@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@DFMKEYWORDS);
  IgnoreKeyWord := (length(KeyWords)=0) or (@KeyWords=@XMLKEYWORDS);
  Highlight := (P^='!');
  if Highlight then
    inc(P);
  B := P;
  while B^=' ' do inc(B);
  if B^<' ' then begin
    result := result+Tags[false,hBR];
    P := B; // avoid '<pre></pre>'
    exit;
  end;
  if IgnoreKeyWord then begin
    if HighLight then
      result := result+Tags[false,hHighlight];
    HasLT := False;
    HasGT := False;
    B := P;
    while P^>=' ' do begin
      if P^='<' then HasLT := True else
      if P^='>' then HasGT := True;
      inc(P);
    end;
    SetToken;
    result := result+token;
    if HighLight then
      result := result+Tags[true,hHighlight];
    exit;
  end;
  while P^=' ' do begin result := result+Tags[false,hNbsp]; inc(P); end;
  if HighLight then
    result := result+Tags[false,hHighlight];
  while P^>=' ' do begin
    HasLT := false;
    HasGT := false;
    while P^=' ' do begin result := result+' '; inc(P); end;
    if (PWord(P)^=ord('(')+ord('*')shl 8) and
       ((@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@MODULA2KEYWORDS)) then begin
      B := P;
      inc(P,2);
      while (PWord(P)^<>ord('*')+ord(')')) and (P^>=' ') do begin
        if P^='<' then HasLT := True else
        if P^='>' then HasGT := True;
        inc(P);
      end;
      if P^='*' then inc(P,2);
com:  SetToken;
      result := result+Tags[false,hNavyItalic]+token+Tags[true,hNavyItalic];
      continue;
    end else
    if (P^='{') and (@KeyWords=@PASCALKEYWORDS) then begin
      B := P;
      repeat
        if P^='<' then HasLT := True else
        if P^='>' then HasGT := True;
        inc(P)
      until (P^='}') or (P^<' ');
      if P^='}' then inc(P);
      goto com;
    end; 
    B := P;
    if PWord(P)^=ord('/')+ord('/')shl 8 then begin
      while P^>=' ' do begin
        if P^='<' then HasLT := True else
        if P^='>' then HasGT := True;
        inc(P);
      end;
      goto com;
    end;
    repeat
      if CString and (P^='"') then begin
        if (P[1]='"') and (B=P) then begin
          result := result+'""';
          inc(P,2);
          B := P;
        end else
          break;
      end else
      if P^='''' then begin
        if PasString and (P[1]='''') then begin
          result := result+'''''';
          inc(P,2);
          B := P;
        end else
          break;
      end else
      if not (P^ in ['0'..'9','a'..'z','A'..'Z','_']) then
        break else
        inc(P);
    until P^<' ';
    SetString(token,B,P-B);
    HasLT := false;
    HasGT := false;
    B := P;
    if token<>'' then
      if (CString or PasString) and IsNumber(pointer(token)) then
        goto str2 else
      if ((@KeyWords=@MODULA2KEYWORDS) and
            IsKeyWord(KeyWords,token)) or // .MOD keys are always uppercase
         ((@KeyWords<>@MODULA2KEYWORDS) and IsKeyWord(KeyWords,UpperCase(token))) then
        result := result+Tags[false,hBold]+token+Tags[true,hBold] else
        result := result+token;
    if CString and (P^='"') and (P[1]<>'"') then begin
      repeat
        if P^='<' then HasLT := True else
        if P^='>' then HasGT := True;
        inc(P);
      until (P^='"') or (P^<' ');
      if P^='"' then inc(P);
str:  SetToken;
str2: result := result+Tags[false,hNavy]+token+Tags[true,hNavy];
    end else
    if PasString and (P^='''') and (P[1]<>'''') then begin
      repeat
        if P^='<' then HasLT := True else
        if P^='>' then HasGT := True;
        inc(P);
      until (P^='''') or (P^<' ');
      if P^='''' then inc(P);
      goto str;
    end else
    if token='' then begin
      if P^='<' then
        result := result+Tags[false,hLT] else
      if P^='>' then
        result := result+Tags[false,hGT] else
        AddCharLS(result,P^);
      inc(P);
    end;
  end;
  if HighLight then
    result := result+Tags[true,hHighlight];
end;
procedure SetCurrent;
var Old, New: THtmlTags;
    Tag: THtmlTag;
begin
  New := Stack[Level];
  Old := Current;
  if New=Old then
    exit;
  for Tag := low(Tag) to high(Tag) do
    if (Tag in Old) and not (Tag in New) then
      result := result+Tags[true,Tag] else
    if not (Tag in Old) and (Tag in New) then
      result := result+Tags[false,Tag];
  Current := New;
end;
begin
  result := Tags[false,hP];
  Level := 0;
  Stack[0] := [];
  Current := [];
  InTable := false;
  InListing := ' ';
  if P<>nil then
    while P^<>#0 do begin
      case P^ of
        '{': begin
          B := P;
          repeat inc(B) until B^ in [#0,'}'];
          if B^<>#0 then begin
            if Level<high(Stack) then begin
              inc(Level);
              Stack[Level] := Stack[Level-1];
            end;
          end;
        end;
        '}': if Level>0 then dec(Level);
        '<': begin
          SetCurrent;
          result := result+Tags[false,hLT];
        end;
        '>': begin
          SetCurrent;
          result := result+Tags[false,hGT];
        end;
        '\': begin
          B := P;
          repeat inc(B) until B^ in RTFEndToken;
          L := B-P-1;
          if L<=7 then begin
            SetString(Token,p+1,L);
            if token='b' then
              include(Stack[Level],hBold) else
            if token='b0' then
              exclude(Stack[Level],hBold) else
            if token='i' then
              include(Stack[Level],hItalic) else
            if token='i0' then
              exclude(Stack[Level],hItalic) else
            if token='ul' then
              include(Stack[Level],hUnderline) else
            if token='ul0' then
              exclude(Stack[Level],hUnderline) else
            if token='strike' then
              include(Stack[Level],hItalic) else
            if token='f1' then
              include(Stack[Level],hCode) else
            if token='line' then
              result := result+Tags[false,hBR];
            inc(P,L+1);
            if P^<>'\' then inc(P);
            continue;
          end;
        end;
        '@': begin
          inc(P);
          if P^='*' then begin
            inc(P); // ignore @*keyword@
            if P^='*' then inc(P);
          end else
          if IdemPChar(P,'HTTP:') then begin
            B := P;
            repeat inc(P) until (P^<=' ') or (P^ in [')',',',';']);
            SetString(token,B,P-B);
            result := result+format(Tags[false,hAHRef],[token])+token+Tags[true,hAHRef];
          end;
          continue;
        end;
        '|': if InTable then begin
          SetCurrent; // force close any <b> tag before <td>
          result := result+Tags[false,hTD];
          if @Tags=@HTML_TAGS then begin
            Current := [];
            SetCurrent; // force rewrite any <b> tag after <td>
          end;
          inc(P);
          continue;
        end;
        #13,#10: begin
          Level := 0;
          SetCurrent; // force flush format at end of paragraph
          result := result+#13#10;
          repeat inc(P) until (P^=#0) or (P^>=' ');
          if InTable then begin
            if (P^<>'|') then begin
              InTable := false;
              result := result+Tags[true,hTable];
            end;
          end else
          if (InListing<>' ') and (P^<>InListing) then begin
            InListing := ' ';
            result := result+Tags[true,hPre];
          end;
          case P^ of
            #0: break;
            '[',';','%': repeat inc(P) until P^<' ';
            '=': if P[1]='[' then repeat inc(P) until P^<' ';
            '|': if P[1]='%' then
                   repeat inc(P) until P^<' ' else begin
                   if not InTable then begin
                     InTable := true;
                     result := result+Tags[false,hTable];
                   end;
                   result := result+Tags[false,hTR]+Tags[false,hTD];
                 end;  
            ':': begin
              repeat inc(P) until not (P^ in ['0'..'9',' ']); // ':0123 title'
              B := P; while P^>=' ' do inc(P);
              SetString(token,B,P-B);
              result := result+Tags[false,hTitle]+token+Tags[true,hTitle];
            end;
            '-': begin
              result := result+Tags[false,hBRList];
              continue;
            end;
            '!': if P[1]='$' then begin // !$=DFM
               inc(P);
               AddListing([]);
             end else
               AddListing(PASCALKEYWORDS);
            '&': AddListing(CKEYWORDS);
            '#': AddListing(CSHARPKEYWORDS);
            'µ': AddListing(MODULA2KEYWORDS);
            '$': begin
              if P[1]='$' then inc(P); // $$ = XML/HTML
              AddListing([]);
            end;
            else begin
              result := result+Tags[false,hP]; // new paragraph
              continue;
            end;
          end;
          if P^=#0 then break;
        end;
        else begin
          SetCurrent;
          AddCharLS(result,P^);
        end;
      end;
      inc(P);
    end;
  if InTable then
    result := result+Tags[true,hTable] else
  if InListing<>' ' then
    result := result+Tags[true,hPre];
end;

procedure TFrameEditor.EditorPopupCopyClick(Sender: TObject);
begin
  Memo.ClipBoardCopy;
end;

procedure TFrameEditor.EditorPopupPasteClick(Sender: TObject);
begin
  Memo.ClipBoardPaste;
end;

procedure TFrameEditor.EditorPopupCutClick(Sender: TObject);
begin
  Memo.ClipBoardCut;
end;

procedure TFrameEditor.EditorPopupCopyAsHtmlClick(Sender: TObject);
begin
  Clipboard.AsText := FormatProAs(pointer(Memo.GetSelText),HTML_TAGS);
end;

procedure TFrameEditor.EditorPopupCopyAsBBCodeClick(Sender: TObject);
begin
  Clipboard.AsText := FormatProAs(pointer(Memo.GetSelText),BBCODE_TAGS);
end;

procedure TFrameEditor.EditorPopupPopup(Sender: TObject);
var i, j: integer;
begin
  if not BtnLinkSection.Visible then begin
    // delete .pro popup menus for .scr editor 
    j := EditorPopup.Items.IndexOf(EditorPopupSpellCheck);
    if j>0 then
      for i := EditorPopup.Items.Count-1 downto j do
        EditorPopup.Items.Delete(i);
  end else
    // recreate refreshed .pro popup menu entries 
    for i := 0 to FLinkClickMenus.Count-1 do
      BtnLinkSectionClick(FLinkClickMenus[i]);
end;

initialization
{$ifdef USEGDIPLUSFORIMAGES}
  Gdip := TGDIPlus.Create('gdiplus.dll');
  Gdip.RegisterPictures;
{$endif}
end.
