{ @abstract(This unit contains page setup dialog.)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(18 Sep 2009)
  @lastmod(15 Oct 2009)

  Copyright © 2009 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KPrintSetup;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources, PrintersDlgs,
{$ELSE}
  Windows, Messages, Dialogs,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, KControls, KPrintPreview;

resourcestring
  sPrinterSetup = 'Printer setup';
  sAllPages = 'All pages (%d)';
  sErrPrintSetup = 'Print setup error';
  sErrNoPrinterInstalled = 'No printer is installed on this computer.';

type

  { TKPrintSetupForm }

  TKPrintSetupForm = class(TForm)
    BUConfigure: TButton;
    CoBPrinterName: TComboBox;
    EDTitle: TEdit;
    GBFileToPrint: TGroupBox;
    GBPrinter: TGroupBox;
    GBPrintOptions: TGroupBox;
    LBPrinterName: TLabel;
    BUPrint: TButton;
    BUCancel: TButton;
    CBFitToPage: TCheckBox;
    CBPageNumbers: TCheckBox;
    CBUseColor: TCheckBox;
    GBMargins: TGroupBox;
    CoBMarginUnits: TComboBox;
    LBMarginUnits: TLabel;
    CBMirrorMargins: TCheckBox;
    GBPageSelection: TGroupBox;
    RBAll: TRadioButton;
    RBRange: TRadioButton;
    RBSelectedOnly: TRadioButton;
    LBRangeTo: TLabel;
    LBCopies: TLabel;
    EDLeft: TEdit;
    LBLeft: TLabel;
    LBRight: TLabel;
    EDRight: TEdit;
    EDTop: TEdit;
    LBTop: TLabel;
    EDBottom: TEdit;
    LBBottom: TLabel;
    EDRangeFrom: TEdit;
    EDRangeTo: TEdit;
    EDCopies: TEdit;
    Label1: TLabel;
    EDPrintScale: TEdit;
    LBUnitsLeft: TLabel;
    LBUnitsTop: TLabel;
    LBUnitsRight: TLabel;
    LBUnitsBottom: TLabel;
    BUPreview: TButton;
    CBPaintSelection: TCheckBox;
    BUOk: TButton;
    CBPrintTitle: TCheckBox;
    CBCollate: TCheckBox;
    PSDMain: TPrinterSetupDialog;
    procedure BUConfigureClick(Sender: TObject);
    procedure CoBMarginUnitsChange(Sender: TObject);
    procedure RBAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BUPreviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EDTopExit(Sender: TObject);
    procedure CBPageNumbersClick(Sender: TObject);
    procedure BUPrintClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FPrevSetup: TKPrintPageSetup;
    FPageSetup: TKPrintPageSetup;
    FPreviewForm: TKPrintPreviewForm;
    FPreviewCreated: Boolean;
    FSelAvail: Boolean;
    FUpdateLock: Boolean;
    procedure SetPageSetup(const Value: TKPrintPageSetup);
    procedure SetPreviewForm(const Value: TKPrintPreviewForm);
  protected
    procedure PageSetupToForm; virtual;
    procedure FormToPageSetup; virtual;
    procedure ValidateForm;
  public
    { Public declarations }
    property PageSetup: TKPrintPageSetup read FPageSetup write SetPageSetup;
    property PreviewForm: TKPrintPreviewForm read FPreviewForm write SetPreviewForm;
    property SelAvail: Boolean read FSelAvail write FSelAvail;
  end;

implementation

uses
  Printers, KFunctions;

procedure TKPrintSetupForm.FormCreate(Sender: TObject);
begin
  FPageSetup := nil;
  FPrevSetup := TKPrintPageSetup.Create(nil);
  FPreviewForm := nil;
  FPreviewCreated := False;
{$IFDEF FPC}
  PSDMain.Title := sPrinterSetup;
{$ENDIF}
end;

procedure TKPrintSetupForm.FormDestroy(Sender: TObject);
begin
  if FPreviewCreated then
  begin
    FPreviewForm.Free;
    FPreviewCreated := False;
  end;
  FPrevSetup.Free;
end;

procedure TKPrintSetupForm.FormShow(Sender: TObject);
begin
  PageSetupToForm;
end;

procedure TKPrintSetupForm.PageSetupToForm;
  function FmtMargin(Value: Double): string;
  const
    Fmt = '%.*f';
  var
    Precision: Integer;
  begin
    case FPageSetup.Units of
      puCM: Precision := 1;
      puMM: Precision := 0;
      puInch: Precision := 2;
    else
      Precision := 0;
    end;
    Result := Format(Fmt, [Precision, Value]);
  end;

  function FmtUnit: string;
  begin
    case FPageSetup.Units of
      puMM: Result := 'mm';
      puInch: Result := '"';
      puHundredthInch: Result := '".100';
    else
      Result := 'cm';
    end;
  end;
var
  S: string;
begin
  if Assigned(FPageSetup) then
  begin
    FUpdateLock := True;
    try
      CBCollate.Checked := poCollate in FPageSetup.Options;
      CBFitToPage.Checked := poFitToPage in FPageSetup.Options;
      CBPageNumbers.Checked := poPageNumbers in FPageSetup.Options;
      CBUseColor.Checked := poUseColor in FPageSetup.Options;
      CBPaintSelection.Checked := poPaintSelection in FPageSetup.Options;
      CBPrintTitle.Checked := poTitle in FPageSetup.Options;
      CBMirrorMargins.Checked := poMirrorMargins in FPageSetup.Options;
      CoBPrinterName.Items.Assign(Printer.Printers);
      CoBPrinterName.ItemIndex := CoBPrinterName.Items.IndexOf(FPageSetup.PrinterName);
      if CoBPrinterName.ItemIndex < 0 then CoBPrinterName.ItemIndex := Printer.PrinterIndex;
      RBSelectedOnly.Enabled := FPageSetup.SelAvail;
      if RBSelectedOnly.Enabled and FSelAvail then
        RBSelectedOnly.Checked := True
      else if FPageSetup.Range = prRange then
        RBRange.Checked := True
      else
        RBAll.Checked := True;
      RBAll.Caption := Format(sAllPages, [FPageSetup.PageCount]);
      EDRangeFrom.Enabled := RBRange.Checked;
      EDRangeFrom.Text := IntToStr(FPageSetup.StartPage);
      EDRangeTo.Enabled := RBRange.Checked;
      EDRangeTo.Text := IntToStr(FPageSetup.EndPage);
      EDCopies.Text := IntToStr(FPageSetup.Copies);
      EDPrintScale.Enabled := not CBFitTopage.Checked;
      EDPrintScale.Text := IntToStr(FPageSetup.Scale);
      EDTitle.Text := FPageSetup.Title;
      CoBMarginUnits.ItemIndex := Integer(FPageSetup.Units);
      S := FmtUnit;
      EDBottom.Text := FmtMargin(FPageSetup.MarginBottom); LBUnitsBottom.Caption := S;
      EDLeft.Text := FmtMargin(FPageSetup.MarginLeft); LBUnitsLeft.Caption := S;
      EDRight.Text := FmtMargin(FPageSetup.MarginRight); LBUnitsRight.Caption := S;
      EDTop.Text := FmtMargin(FPageSetup.MarginTop); LBUnitsTop.Caption := S;
    finally
      FUpdateLock := False;
    end;
  end;
end;

procedure TKPrintSetupForm.FormToPageSetup;
var
  Options: TKPrintOptions;
begin
  if Assigned(FPageSetup) and not FUpdateLock then
  begin
    FPageSetup.LockUpdate;
    try
      Options := [];
      if CBCollate.Checked then Include(Options, poCollate);
      if CBFitToPage.Checked then Include(Options, poFitToPage);
      if CBPageNumbers.Checked then Include(Options, poPageNumbers);
      if CBUseColor.Checked then Include(Options, poUseColor);
      if CBPaintSelection.Checked then Include(Options, poPaintSelection);
      if CBPrintTitle.Checked then Include(Options, poTitle);
      if CBMirrorMargins.Checked then Include(Options, poMirrorMargins);
      FPageSetup.PrinterName := CoBPrinterName.Text;
      FPageSetup.Options := Options;
      if RBSelectedOnly.Checked then FPageSetup.Range := prSelectedOnly
      else if RBRange.Checked then FPageSetup.Range := prRange
      else FPageSetup.Range := prAll;
      FPageSetup.StartPage := StrToIntDef(EDRangeFrom.Text, FPageSetup.StartPage);
      FPageSetup.EndPage := StrToIntDef(EDRangeTo.Text, FPageSetup.EndPage);
      FPageSetup.Copies := StrToIntDef(EDCopies.Text, FPageSetup.Copies);
      FPageSetup.Scale := StrToIntDef(EDPrintScale.Text, FPageSetup.Scale);
      FPageSetup.Title := EDTitle.Text;
      FPageSetup.Units := TKPrintUnits(CoBMarginUnits.ItemIndex);
      FPageSetup.MarginBottom := StrToFloatDef(AdjustDecimalSeparator(EDBottom.Text), FPageSetup.MarginBottom);
      FPageSetup.MarginLeft := StrToFloatDef(AdjustDecimalSeparator(EDLeft.Text), FPageSetup.MarginLeft);
      FPageSetup.MarginRight := StrToFloatDef(AdjustDecimalSeparator(EDRight.Text), FPageSetup.MarginRight);
      FPageSetup.MarginTop := StrToFloatDef(AdjustDecimalSeparator(EDTop.Text), FPageSetup.MarginTop);
    finally
      FPageSetup.UnlockUpdate;
    end;
  end;
end;

procedure TKPrintSetupForm.BUPrintClick(Sender: TObject);
begin
  FormToPageSetup;
  FPageSetup.PrintOut;
end;

procedure TKPrintSetupForm.BUConfigureClick(Sender: TObject);
begin
  FormToPageSetup;
  try
    if PSDMain.Execute then
    begin
      FPageSetup.LockUpdate;
      try
        FPageSetup.PrinterName := '';
      finally
        FPageSetup.UnlockUpdate;
      end;
      PageSetupToForm;
    end;
  except
    MessageBox(Handle, PChar(sErrNoPrinterInstalled), PChar(sErrPrintSetup), MB_OK);
  end;
end;

procedure TKPrintSetupForm.EDTopExit(Sender: TObject);
begin
  ValidateForm;
end;

procedure TKPrintSetupForm.CoBMarginUnitsChange(Sender: TObject);
begin
  if Assigned(FPageSetup) then
  begin
    FPageSetup.Units := TKPrintUnits(CoBMarginUnits.ItemIndex);
    PageSetupToForm;
  end;
end;

procedure TKPrintSetupForm.CBPageNumbersClick(Sender: TObject);
begin
  FormToPageSetup;
end;

procedure TKPrintSetupForm.RBAllClick(Sender: TObject);
begin
  FSelAvail := RBSelectedOnly.Checked;
  ValidateForm;
end;

procedure TKPrintSetupForm.SetPageSetup(const Value: TKPrintPageSetup);
begin
  if Value <> FPageSetup then
  begin
    FPrevSetup.Assign(Value);
    FPageSetup := Value;
    PageSetupToForm;
  end;
end;

procedure TKPrintSetupForm.SetPreviewForm(const Value: TKPrintPreviewForm);
begin
  if Value <> FPreviewForm then
  begin
    if FPreviewCreated then
    begin
      FPreviewForm.Free;
      FPreviewCreated := False;
    end;
    FPreviewForm := Value;
  end;
end;

procedure TKPrintSetupForm.ValidateForm;
begin
  FormToPageSetup;
  PageSetupToForm;
end;

procedure TKPrintSetupForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FPreviewCreated then
    FPreviewForm.Hide;
  if ModalResult = mrOk then
    FormToPageSetup
  else if Assigned(FPageSetup) then
    FPageSetup.Assign(FPrevSetup);
end;

procedure TKPrintSetupForm.BUPreviewClick(Sender: TObject);
begin
  ValidateForm;
  if FPreviewForm = nil then
  begin
    FPreviewForm := TKPrintPreviewForm.Create(nil);
    FPreviewCreated := True;
  end;
  FPreviewForm.Preview.Control := FPageSetup.Control;
  FPreviewForm.Show;
end;

{$IFDEF FPC}
initialization
  {$i kprintsetup.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
