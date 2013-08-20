//**********************************************************************************************************************
//  $Id: Main.pas,v 1.3 2006/08/22 13:17:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  DKLang Localization Package
//  Copyright 2002-2006 DK Software, http://www.dk-soft.org
//**********************************************************************************************************************
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, DKLang, Dialogs, ActnList, Menus, ComCtrls;

type
  TfMain = class(TForm)
    aEditCopy: TAction;
    aEditCut: TAction;
    aEditDateAndTime: TAction;
    aEditFind: TAction;
    aEditFindNext: TAction;
    aEditGoToLine: TAction;
    aEditPaste: TAction;
    aEditReplace: TAction;
    aEditSelectAll: TAction;
    aEditUndo: TAction;
    aFileExit: TAction;
    aFileNew: TAction;
    aFileOpen: TAction;
    aFileSave: TAction;
    aFileSaveAs: TAction;
    aFormatFont: TAction;
    aFormatWordWrap: TAction;
    aHelpAbout: TAction;
    alMain: TActionList;
    aViewStatusBar: TAction;
    dklcMain: TDKLanguageController;
    fdMain: TFontDialog;
    iEditCopy: TMenuItem;
    iEditCut: TMenuItem;
    iEditDateAndTime: TMenuItem;
    iEditFind: TMenuItem;
    iEditFindNext: TMenuItem;
    iEditGoToLine: TMenuItem;
    iEditPaste: TMenuItem;
    iEditReplace: TMenuItem;
    iEditSelectAll: TMenuItem;
    iEditUndo: TMenuItem;
    iFileExit: TMenuItem;
    iFileNew: TMenuItem;
    iFileOpen: TMenuItem;
    iFileSave: TMenuItem;
    iFileSaveAs: TMenuItem;
    iFormatFont: TMenuItem;
    iFormatWordWrap: TMenuItem;
    iHelpAbout: TMenuItem;
    iSepEditCut: TMenuItem;
    iSepEditFind: TMenuItem;
    iSepEditSelectAll: TMenuItem;
    iSepFileExit: TMenuItem;
    iSepViewLanguage: TMenuItem;
    iViewStatusBar: TMenuItem;
    mMain: TMemo;
    mmMain: TMainMenu;
    odMain: TOpenDialog;
    sdMain: TSaveDialog;
    smEdit: TMenuItem;
    smFile: TMenuItem;
    smFormat: TMenuItem;
    smHelp: TMenuItem;
    smView: TMenuItem;
    smViewLanguage: TMenuItem;
    TheStatusBar: TStatusBar;
    procedure aEditCopyExecute(Sender: TObject);
    procedure aEditCutExecute(Sender: TObject);
    procedure aEditDateAndTimeExecute(Sender: TObject);
    procedure aEditFindExecute(Sender: TObject);
    procedure aEditFindNextExecute(Sender: TObject);
    procedure aEditGoToLineExecute(Sender: TObject);
    procedure aEditPasteExecute(Sender: TObject);
    procedure aEditReplaceExecute(Sender: TObject);
    procedure aEditSelectAllExecute(Sender: TObject);
    procedure aEditUndoExecute(Sender: TObject);
    procedure aFileExitExecute(Sender: TObject);
    procedure aFileNewExecute(Sender: TObject);
    procedure aFileOpenExecute(Sender: TObject);
    procedure aFileSaveAsExecute(Sender: TObject);
    procedure aFileSaveExecute(Sender: TObject);
    procedure aFormatFontExecute(Sender: TObject);
    procedure aFormatWordWrapExecute(Sender: TObject);
    procedure aHelpAboutExecute(Sender: TObject);
    procedure aViewStatusBarExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure UpdateStateNotify(Sender: TObject);
  private
     // Prop storage
    FFileName: UnicodeString;
     // Updates form interface
    procedure UpdateState;
     // Language item click handler
    procedure LanguageItemClick(Sender: TObject);
     // Loads the specified file
    procedure DoLoadFile(const wsFileName: UnicodeString);
     // Saves the text into the specified file
    procedure DoSaveFile(const wsFileName: UnicodeString);
     // Returns True if text can be discarded
    function  CanDiscardText: Boolean;
     // Prop handlers
    function  GetDisplayFileName: UnicodeString;
  public
     // Props
     // -- Name of the file being edited, always not empty
    property DisplayFileName: UnicodeString read GetDisplayFileName;
  end;

var
  fMain: TfMain;

implementation
{$R *.dfm}
uses
  XPMan;

  procedure TfMain.aEditCopyExecute(Sender: TObject);
  begin
    mMain.CopyToClipboard;
  end;

  procedure TfMain.aEditCutExecute(Sender: TObject);
  begin
    mMain.CutToClipboard;
  end;

  procedure TfMain.aEditDateAndTimeExecute(Sender: TObject);
  begin
    mMain.SelText := DateTimeToStr(Now);
  end;

  procedure TfMain.aEditFindExecute(Sender: TObject);
  begin
    { Find not implemented }
  end;

  procedure TfMain.aEditFindNextExecute(Sender: TObject);
  begin
    { Find not implemented }
  end;

  procedure TfMain.aEditGoToLineExecute(Sender: TObject);
  var ws: UnicodeString;
  begin
    ws := IntToStr(mMain.CaretPos.y+1);
    if InputQuery(DKLangConstW('SDlgTitle_GoToLine'), DKLangConstW('SGoToLinePrompt'), ws) then begin
      mMain.CaretPos := Point(0, StrToInt(ws)-1);
      mMain.Perform(EM_SCROLLCARET, 0, 0);
    end;
  end;

  procedure TfMain.aEditPasteExecute(Sender: TObject);
  begin
    mMain.PasteFromClipboard;
  end;

  procedure TfMain.aEditReplaceExecute(Sender: TObject);
  begin
    { Replace not implemented }
  end;

  procedure TfMain.aEditSelectAllExecute(Sender: TObject);
  begin
    mMain.SelectAll;
  end;

  procedure TfMain.aEditUndoExecute(Sender: TObject);
  begin
    mMain.Undo;
  end;

  procedure TfMain.aFileExitExecute(Sender: TObject);
  begin
    Close;
  end;

  procedure TfMain.aFileNewExecute(Sender: TObject);
  begin
    if CanDiscardText then begin
      mMain.Clear;
      mMain.Modified := False;
      FFileName := '';
      UpdateState;
    end;
  end;

  procedure TfMain.aFileOpenExecute(Sender: TObject);
  begin
    odMain.FileName := FFileName;
    if odMain.Execute and CanDiscardText then DoLoadFile(odMain.FileName);
  end;

  procedure TfMain.aFileSaveAsExecute(Sender: TObject);
  begin
    sdMain.FileName := FFileName;
    if sdMain.Execute then DoSaveFile(sdMain.FileName);
  end;

  procedure TfMain.aFileSaveExecute(Sender: TObject);
  begin
    if FFileName='' then aFileSaveAs.Execute else DoSaveFile(FFileName);
  end;

  procedure TfMain.aFormatFontExecute(Sender: TObject);
  begin
    fdMain.Font.Assign(mMain.Font);
    if fdMain.Execute then mMain.Font.Assign(fdMain.Font);
  end;

  procedure TfMain.aFormatWordWrapExecute(Sender: TObject);
  begin
    mMain.WordWrap := not mMain.WordWrap;
    UpdateState;
  end;

  procedure TfMain.aHelpAboutExecute(Sender: TObject);
  begin
    MessageBoxW(
      Application.Handle,
      PWideChar(Format(
        '%s v1.00'#13#10'%s', [DKLangConstW('SApplicationName'), DKLangConstW('SCopyright')])),
      PWideChar(DKLangConstW('SDlgTitle_About')),
      MB_ICONINFORMATION or MB_OK);
  end;

  procedure TfMain.aViewStatusBarExecute(Sender: TObject);
  begin
    TheStatusBar.Visible := not TheStatusBar.Visible;
    UpdateState;
  end;

  function TfMain.CanDiscardText: Boolean;
  begin
    Result := True;
    if mMain.Modified then
      case MessageBoxW(
          Application.Handle,
          PWideChar(DKLangConstW('SMsg_ConfirmFileDiscard', [DisplayFileName])),
          PWideChar(DKLangConstW('SDlgTitle_Warning')),
          MB_ICONWARNING or MB_YESNOCANCEL) of
        IDYES: Result := aFileSave.Execute and not mMain.Modified;
        IDNO:  { nothing };
        else   Result := False;
      end;
  end;

  procedure TfMain.DoLoadFile(const wsFileName: UnicodeString);
  begin
    mMain.Lines.LoadFromFile(wsFileName);
    mMain.Modified := False;
    FFileName := wsFileName;
    UpdateState;
  end;

  procedure TfMain.DoSaveFile(const wsFileName: UnicodeString);
  begin
    mMain.Lines.SaveToFile(wsFileName);
    mMain.Modified := False;
    FFileName := wsFileName;
    UpdateState;
  end;

  function TfMain.GetDisplayFileName: UnicodeString;
  begin
    Result := FFileName;
    if Result='' then Result := DKLangConstW('SDefaultFileName');
  end;

  procedure TfMain.LanguageItemClick(Sender: TObject);
  begin
     // We stored language ID in Tag of each menu item (which is Sender here)
    LangManager.LanguageID := (Sender as TComponent).Tag;
    UpdateState;
  end;

  procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    CanClose := CanDiscardText;
  end;

  procedure TfMain.FormCreate(Sender: TObject);

    procedure CreateLanguageMenu;
    var
      i: Integer;
      mi: TMenuItem;
    begin
      for i := 0 to LangManager.LanguageCount-1 do begin
        mi := NewItem(LangManager.LanguageNames[i], 0, False, True, LanguageItemClick, 0, '');
        mi.Tag := LangManager.LanguageIDs[i];
        smViewLanguage.Add(mi);
      end;
    end;

  begin
     // Scan for language files in the app directory and register them in the LangManager object
    LangManager.ScanForLangFiles(ExtractFileDir(ParamStr(0)), '*.lng', False);
     // Create available languages menu
    CreateLanguageMenu;
     // Update interface elements
    UpdateState;
  end;

  procedure TfMain.UpdateState;
  const awsModified: Array[Boolean] of UnicodeString = ('', '*');

    procedure UpdateLanguageMark;
    var
      i: Integer;
      CurLang: LANGID; // To avoid excess synch calls
    begin
      CurLang := LangManager.LanguageID; 
      for i := 0 to smViewLanguage.Count-1 do
        with smViewLanguage[i] do Checked := Tag=CurLang;
    end;

  begin
     // Update form caption
    Caption := Format(
      '[%s%s] - %s', [DisplayFileName, awsModified[mMain.Modified], DKLangConstW('SApplicationName')]);
    Application.Title := Caption;
     // Update language menu
    UpdateLanguageMark;
     // Update misc
    aFormatWordWrap.Checked := mMain.WordWrap;
    if mMain.WordWrap then mMain.ScrollBars := ssVertical else mMain.ScrollBars := ssBoth;
    aViewStatusBar.Checked := TheStatusBar.Visible;
  end;

  procedure TfMain.UpdateStateNotify(Sender: TObject);
  begin
    UpdateState;
  end;

end.
