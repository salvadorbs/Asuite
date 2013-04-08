// -------------------------------------------------------------------------------------------------
// Demo program for Html2Pdf.pas.
// You must have HTMLVIEW components by L. David Baldwin installed to open this in IDE.
// This demo was written by Pawel Stroinski on 2010/07/07 and is public domain.
// -------------------------------------------------------------------------------------------------

{ AB enhancements:
  - drawing clipping, since THtmlView sometime writes outside the margins :(
  - optional page number print }

unit Main_Form;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Html2Pdf, StdCtrls, Htmlview, IniFiles, ShellApi, Printers;

type
  TMainForm = class(TForm)
    btnOpen: TButton;
    lbHTML: TLabel;
    OpenDialog: TOpenDialog;
    Viewer: THTMLViewer;
    btnSave: TButton;
    btnSaveAs: TButton;
    lbPDF: TLabel;
    SaveDialog: TSaveDialog;
    cxOpenAfterSave: TCheckBox;
    lbMargins: TLabel;
    edMarginLeft: TEdit;
    edMarginTop: TEdit;
    edMarginRight: TEdit;
    edMarginBottom: TEdit;
    cxScaleToFit: TCheckBox;
    cbOrientation: TComboBox;
    cbPaperSize: TComboBox;
    btnSynopse: TButton;
    cxPrintPage: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
  procedure btnSynopseClick(Sender: TObject);
  private
    { Private declarations }
    FSettings: TMemIniFile;
  public
    { Public declarations }
  end;

var
    MainForm: TMainForm;

implementation

uses SynPdf;

{$R *.dfm}

{$R Vista.res}

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(lbHTML.Caption);
  OpenDialog.FileName := ExtractFileName(lbHTML.Caption);
  if OpenDialog.Execute then
  begin
    lbHTML.Caption := OpenDialog.FileName;
    Viewer.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.btnSaveAsClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFileDir(lbPDF.Caption);
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(lbHTML.Caption), '');
  if SaveDialog.Execute then
  begin
    lbPDF.Caption := SaveDialog.FileName;
    btnSaveClick(nil);
  end;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var FN: TFileName;
begin
  if lbPDF.Caption = '' then
    btnSaveAsClick(nil)
  else
  begin
    with THtml2Pdf.Create do
    begin
      FN := lbPDF.Caption;
      Viewer := Self.Viewer;
      MarginLeft := StrToFloat(edMarginLeft.Text);
      MarginTop := StrToFloat(edMarginTop.Text);
      MarginRight := StrToFloat(edMarginRight.Text);
      MarginBottom := StrToFloat(edMarginBottom.Text);
      ScaleToFit := cxScaleToFit.Checked;
      Orientation := TPrinterOrientation(cbOrientation.ItemIndex);
      DefaultPaperSize := TPDFPaperSize(cbPaperSize.ItemIndex);
      DrawPageNumber := cxPrintPage.Checked;
      DrawPageNumberText := ExtractFileName(lbHTML.Caption)+' - Page %d/%d';
      Execute;
      SaveToFile(FN);
      Free;
    end;
    if cxOpenAfterSave.Checked then
      ShellExecute(Handle, 'open', pointer(FN), nil, nil, SW_SHOWNORMAL)
      //ShellExecute(Handle, 'open', 'cmd.exe', PChar('/c start ' + lbPDF.Caption), nil, SW_SHOWNORMAL)
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var FN, Ext: TFileName;
begin
  FSettings := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  lbHTML.Caption := FSettings.ReadString('Settings', 'HTML', '');
  Viewer.LoadFromFile(lbHTML.Caption);
  lbPDF.Caption := FSettings.ReadString('Settings', 'PDF', '');
  cxOpenAfterSave.Checked := FSettings.ReadBool('Settings', 'OpenAfterSave', True);
  edMarginLeft.Text := FloatToStr(FSettings.ReadFloat('Margins', 'Left', 1));
  edMarginTop.Text := FloatToStr(FSettings.ReadFloat('Margins', 'Top', 1));
  edMarginRight.Text := FloatToStr(FSettings.ReadFloat('Margins', 'Right', 1));
  edMarginBottom.Text := FloatToStr(FSettings.ReadFloat('Margins', 'Bottom', 1));
  cxPrintPage.Checked := FSettings.ReadBool('Margins','PrintPage',false);
  cxScaleToFit.Checked := FSettings.ReadBool('Settings', 'ScaleToFit', False);
  cbOrientation.ItemIndex := FSettings.ReadInteger('Settings', 'Orientation', 0);
  cbPaperSize.ItemIndex := FSettings.ReadInteger('Settings', 'PaperSize', 0);
  if ParamCount=0 then
    exit;
  FN := ParamStr(1);
  if FileExists(FN) then begin
    // parameters can be
    //  /silent
    // or   FileNameToConvert.htm [DestFileName.pdf]
    Ext := ExtractFileExt(FN);
    if SameText(Ext,'.HTML') or SameText(Ext,'.HTM') then begin
      lbHTML.Caption := FN;
      if ParamCount=2 then
        lbPDF.Caption := ParamStr(2) else
        lbPDF.Caption := ChangeFileExt(FN,'.pdf');
      FN := '/silent'; 
    end;
  end;
  if SameText(FN,'/silent') then
  begin
    btnSaveClick(nil);
    Halt;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSettings.WriteString('Settings', 'HTML', lbHTML.Caption);
  FSettings.WriteString('Settings', 'PDF', lbPDF.Caption);
  FSettings.WriteBool('Settings', 'OpenAfterSave', cxOpenAfterSave.Checked);
  FSettings.WriteFloat('Margins', 'Left', StrToFloat(edMarginLeft.Text));
  FSettings.WriteFloat('Margins', 'Top', StrToFloat(edMarginTop.Text));
  FSettings.WriteFloat('Margins', 'Right', StrToFloat(edMarginRight.Text));
  FSettings.WriteFloat('Margins', 'Bottom', StrToFloat(edMarginBottom.Text));
  FSettings.WriteBool('Margins','PrintPage', cxPrintPage.Checked);
  FSettings.WriteBool('Settings', 'ScaleToFit', cxScaleToFit.Checked);
  FSettings.WriteInteger('Settings', 'Orientation', cbOrientation.ItemIndex);
  FSettings.WriteInteger('Settings', 'PaperSize', cbPaperSize.ItemIndex);
  FSettings.UpdateFile;
  FreeAndNil(FSettings);
end;

procedure TMainForm.btnSynopseClick(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://synopse.info',nil,nil,SW_SHOWNORMAL);
end;

end.

