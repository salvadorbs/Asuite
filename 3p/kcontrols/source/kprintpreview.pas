{ @abstract(This unit contains print preview form.)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(18 Sep 2009)
  @lastmod(20 Jun 2010)

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

unit KPrintPreview;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LResources,
{$ELSE}
  Windows, Messages, ToolWin, ImgList,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnList, Buttons, StdCtrls,
  ExtCtrls, KControls;

type

  { TKPrintPreviewForm }

  TKPrintPreviewForm = class(TForm)
    ILMain: TImageList;
    ALMain: TActionList;
    ACPageFirst: TAction;
    ACPageLast: TAction;
    ACPageNext: TAction;
    ACPagePrevious: TAction;
    ACClose: TAction;
    ILMainDis: TImageList;
    ToBMain: TToolBar;
    TBPageFirst: TToolButton;
    TBPagePrevious: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    TBPageNext: TToolButton;
    TBPageLast: TToolButton;
    PNPage: TPanel;
    EDPage: TEdit;
    UDPage: TUpDown;
    ToolButton6: TToolButton;
    PNScale: TPanel;
    CoBScale: TComboBox;
    TBClose: TToolButton;
    Preview: TKPrintPreview;
    TBPrint: TToolButton;
    ToolButton4: TToolButton;
    ACPrint: TAction;
    procedure CoBScaleExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ACPageFirstExecute(Sender: TObject);
    procedure ACPageFirstUpdate(Sender: TObject);
    procedure ACPagePreviousExecute(Sender: TObject);
    procedure ACPageNextExecute(Sender: TObject);
    procedure ACPageNextUpdate(Sender: TObject);
    procedure ACPageLastExecute(Sender: TObject);
    procedure ACCloseExecute(Sender: TObject);
    procedure ACCloseUpdate(Sender: TObject);
    procedure EDPageExit(Sender: TObject);
    procedure UDPageClick(Sender: TObject; Button: TUDBtnType);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PreviewChanged(Sender: TObject);
    procedure ACPrintExecute(Sender: TObject);
    procedure ACPrintUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ScaleChanged;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  KFunctions;

procedure TKPrintPreviewForm.FormCreate(Sender: TObject);
begin
  CoBScale.ItemIndex := 9; // page width
  Preview.DoubleBuffered := True;
end;

procedure TKPrintPreviewForm.FormShow(Sender: TObject);
begin
  UDPage.Min := Preview.StartPage;
  UDPage.Max := Preview.EndPage;
end;

procedure TKPrintPreviewForm.CoBScaleExit(Sender: TObject);
begin
  ScaleChanged;
end;

procedure TKPrintPreviewForm.ACPageFirstExecute(Sender: TObject);
begin
  Preview.FirstPage;
end;

procedure TKPrintPreviewForm.ACPageFirstUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Preview.Page > Preview.StartPage;
end;

procedure TKPrintPreviewForm.ACPagePreviousExecute(Sender: TObject);
begin
  Preview.PreviousPage;
end;

procedure TKPrintPreviewForm.ACPageNextExecute(Sender: TObject);
begin
  Preview.NextPage;
end;

procedure TKPrintPreviewForm.ACPageNextUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Preview.Page < Preview.EndPage;
end;

procedure TKPrintPreviewForm.ACPageLastExecute(Sender: TObject);
begin
  Preview.LastPage;
end;

procedure TKPrintPreviewForm.ACPrintExecute(Sender: TObject);
begin
  Preview.Control.PrintOut;
end;

procedure TKPrintPreviewForm.ACPrintUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Preview.Control) and Preview.Control.CanPrint;
end;

procedure TKPrintPreviewForm.ACCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TKPrintPreviewForm.ACCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TKPrintPreviewForm.EDPageExit(Sender: TObject);
begin
  Preview.Page := MinMax(StrToIntDef(EDPage.Text, Preview.Page), Preview.StartPage, Preview.EndPage);
  EDPage.Text := IntToStr(Preview.Page);
end;

procedure TKPrintPreviewForm.UDPageClick(Sender: TObject; Button: TUDBtnType);
begin
  EDPageExit(nil);
end;

procedure TKPrintPreviewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Close;
    Key := 0;
  end;
end;

procedure TKPrintPreviewForm.PreviewChanged(Sender: TObject);
begin
  EDPage.Text := IntToStr(Preview.Page);
end;

procedure TKPrintPreviewForm.ScaleChanged;
var
  S: string;
begin
  S := CoBScale.Text;
  if CoBScale.Items.IndexOf(S) < 0 then
    CoBScale.ItemIndex := -1;
  case CoBScale.ItemIndex of
   -1:
    begin
      while (S <> '') and not CharInSetEx(S[Length(S)], ['0'..'9']) do Delete(S, Length(S), 1);
      Preview.Scale := StrToIntDef(S, 100);
    end;
    0: Preview.Scale := 25;
    1: Preview.Scale := 50;
    2: Preview.Scale := 75;
    3: Preview.Scale := 100;
    4: Preview.Scale := 125;
    5: Preview.Scale := 150;
    6: Preview.Scale := 200;
    7: Preview.Scale := 500;
  end;
  case CoBScale.ItemIndex of
    -1:
    begin
      Preview.ScaleMode := smScale;
      CobScale.Text := Format('%d %%', [Preview.Scale]);
    end;
    0..7: Preview.ScaleMode := smScale;
    8: Preview.ScaleMode := smWholePage;
    9: Preview.ScaleMode := smPageWidth;
  end;
end;

{$IFDEF FPC}
initialization
  {$i kprintpreview.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
