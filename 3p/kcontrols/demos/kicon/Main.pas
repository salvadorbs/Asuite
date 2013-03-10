unit Main;

interface

{$include KControls.inc}

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, KIcon, ComCtrls, Buttons, KGraphics;

type
  TMainForm = class(TForm)
    ACExtractBitmap: TAction;
    BUExtract: TButton;
    BULoad: TButton;
    BUExtractBitmap: TButton;
    BUSave: TButton;
    BUAdd: TButton;
    BURemove: TButton;
    BUBackground: TButton;
    ODMain: TOpenDialog;
    PBAlpha: TPaintBox;
    PNAlpha: TPanel;
    SDMain: TSaveDialog;
    ALMain: TActionList;
    ACSave: TAction;
    ACAddImage: TAction;
    CBStretch: TCheckBox;
    RGDrawStyle: TRadioGroup;
    PNMain: TPanel;
    IMMain: TImage;
    CBDisplayAll: TCheckBox;
    CDMain: TColorDialog;
    LVMain: TListView;
    ACRemoveImage: TAction;
    CBDisplayHorz: TCheckBox;
    ACDisplayHorz: TAction;
    BULoadMain: TButton;
    SBIcon: TSpeedButton;
    procedure BULoadClick(Sender: TObject);
    procedure ACSaveUpdate(Sender: TObject);
    procedure ACSaveExecute(Sender: TObject);
    procedure CBStretchClick(Sender: TObject);
    procedure RGDrawStyleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ACAddImageExecute(Sender: TObject);
    procedure CBDisplayAllClick(Sender: TObject);
    procedure BUBackgroundClick(Sender: TObject);
    procedure LVMainSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ACRemoveImageExecute(Sender: TObject);
    procedure ACRemoveImageUpdate(Sender: TObject);
    procedure ACDisplayHorzExecute(Sender: TObject);
    procedure ACDisplayHorzUpdate(Sender: TObject);
    procedure BULoadMainClick(Sender: TObject);
    procedure ACExtractBitmapExecute(Sender: TObject);
    procedure BUExtractClick(Sender: TObject);
    procedure PBAlphaPaint(Sender: TObject);
  private
    { Private declarations }
    KIcon: TKIcon;
    KAlphaBitmap: TKAlphaBitmap;
    procedure UpdateIcon;
    procedure UpdateSpeedButton;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF USE_PNG_SUPPORT}
uses
 {$IFDEF FPC}
  fpImage, IntfGraphics
 {$ELSE}
  PngImage
 {$ENDIF}
  ;
{$ENDIF}

const
  cFilterExecutables = 'Executables (*.exe,*.dll,*.ocx)|*.exe;*.dll;*.ocx';
  cFilterIcons = 'Icons (*.ico)|*.ico';
  cFilterImages = 'Images (*.bmp,*.ico,*.jpg,*.png,*.wmf,*.emf)|*.bmp;*.ico;*.jpg;*.png;*.wmf;*.emf';
  cFilterBitmaps = 'Bitmap (*.bmp)|*.bmp';
  cFilterPngsBitmaps = 'Portable network graphic(*.png)|*.png|Bitmap (*.bmp)|*.bmp';

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  KAlphaBitmap := TKAlphaBitmap.Create;
end;

destructor TMainForm.Destroy;
begin
  inherited;
  KAlphaBitmap.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterKIcon;
  KIcon := nil;
end;

procedure TMainForm.UpdateIcon;
var
  I: Integer;
begin
  LVMain.Clear;
  if KIcon <> nil then
  begin
    KIcon.IconDrawStyle := TKIconDrawStyle(RGDrawStyle.ItemIndex);
    KIcon.DisplayAll := CBDisplayAll.Checked;
    KIcon.DisplayHorz := CBDisplayHorz.Checked;
    for I := 0 to KIcon.IconCount - 1 do
    begin
      LVMain.Items.Add;
      with LVMain.Items[I] do
      begin
        Caption := IntToStr(I);
        SubItems.Add(IntToStr(KIcon.IconData[I].Width));
        SubItems.Add(IntToStr(KIcon.IconData[I].Height));
        SubItems.Add(IntToStr(KIcon.IconData[I].Bpp));
        if KIcon.IconData[I].IsPNG then SubItems.Add('yes') else SubItems.Add('no');
      end;
    end;
  {$IFDEF FPC}
    LVMain.ItemFocused := LVMain.Items[KIcon.CurrentIndex];
  {$ELSE}
    LVMain.ItemIndex := KIcon.CurrentIndex;
  {$ENDIF}
    UpdateSpeedButton;
  end;
end;

procedure TMainForm.UpdateSpeedButton;
var
  BM: TBitmap;
begin
  BM := TBitmap.Create;
  try
    KIcon.CopyToBitmap(KIcon.CurrentIndex, BM);
    SBIcon.Glyph := BM;
  finally
    BM.Free;
  end;
  KIcon.CopyToAlphaBitmap(KIcon.CurrentIndex, KAlphaBitmap);
  PBAlpha.Invalidate;
end;

procedure TMainForm.BULoadClick(Sender: TObject);
begin
  ODMain.Filter := cFilterIcons;
  if ODMain.Execute then
  begin
    IMMain.Picture.LoadFromFile(ODMain.FileName);
    KIcon := TKIcon(IMMain.Picture.Graphic);
    KIcon.OptimalIcon := True;
    UpdateIcon;
  end;
end;

procedure TMainForm.BULoadMainClick(Sender: TObject);
var
  KIcon1: TKIcon;
begin
  KIcon1 := TKIcon.Create;
  try
    KIcon1.LoadFromHandle(Application.Icon.Handle);
    IMMain.Picture.Assign(KIcon1);
    KIcon := TKIcon(IMMain.Picture.Graphic);
    KIcon.OptimalIcon := True;
    UpdateIcon;
  finally
    KIcon1.Free;
  end;
end;

procedure TMainForm.BUExtractClick(Sender: TObject);
var
  KIcon1: TKIcon;
begin
  //ODMain.Filter := cFilterExecutables;
  if ODMain.Execute then
  begin
    KIcon1 := TKIcon.Create;
    try
      KIcon1.LoadFromModuleByIndex(ODMain.FileName,0);
      IMMain.Picture.Assign(KIcon1);
      //KIcon := TKIcon(IMMain.Picture.Graphic);
      //KIcon.OptimalIcon := True;
      //UpdateIcon;
    finally
      KIcon1.Free;
    end;
  end;
end;

procedure TMainForm.ACSaveExecute(Sender: TObject);
begin
  SDMain.Filter := cFilterIcons;
  SDMain.DefaultExt := 'ico';
  if SDMain.Execute then
    IMMain.Picture.SaveToFile(SDMain.FileName);
end;

procedure TMainForm.ACSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := KIcon <> nil;
end;

procedure TMainForm.CBStretchClick(Sender: TObject);
begin
  IMMain.Stretch := not IMMain.Stretch;
end;

procedure TMainForm.RGDrawStyleClick(Sender: TObject);
begin
  UpdateIcon;
end;

procedure TMainForm.ACAddImageExecute(Sender: TObject);
var
  Picture:TPicture;
  BM: TBitmap;
begin
  ODMain.Filter := cFilterImages;
  if ODMain.Execute then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(ODMain.FileName);
      if Picture.Graphic is TBitmap then
      begin
        KIcon.Add(MakeHandles(
          TBitmap(Picture.Graphic).Handle,
          TBitmap(Picture.Graphic).MaskHandle));
      end
      else if Picture.Graphic is TKIcon then
        KIcon.Add(TKIcon(Picture.Graphic).Handles[TKIcon(Picture.Graphic).CurrentIndex])
      else if Picture.Graphic is TKPngImage then
        KIcon.AddFromPng(TKPngImage(Picture.Graphic))
      else
      begin
        BM := TBitmap.Create;
        try
          BM.Width := Picture.Width;
          BM.Height := Picture.Height;
          BM.Canvas.Draw(0, 0, Picture.Graphic);
          KIcon.Add(MakeHandles(
            BM.Handle,
            BM.MaskHandle));
        finally
          BM.Free;
        end;
      end;
      KIcon.OptimalIcon := True;
      UpdateIcon;
    finally
      Picture.Free;
    end;
  end;
end;

procedure TMainForm.ACRemoveImageExecute(Sender: TObject);
begin
  KIcon.Delete(KIcon.CurrentIndex);
  KIcon.OptimalIcon := True;
  UpdateIcon;
end;

procedure TMainForm.ACRemoveImageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (KIcon <> nil) and (KIcon.IconCount > 1);
end;

procedure TMainForm.CBDisplayAllClick(Sender: TObject);
begin
  UpdateIcon;
end;

procedure TMainForm.BUBackgroundClick(Sender: TObject);
begin
  CDMain.Color := PNMain.Color;
  if CDMain.Execute then
    PNMain.Color := CDMain.Color;
end;

procedure TMainForm.LVMainSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if KIcon <> nil then
  begin
    KIcon.OptimalIcon := False;
  {$IFDEF FPC}
    KIcon.CurrentIndex := Item.Index;
  {$ELSE}
    KIcon.CurrentIndex := LVMain.ItemIndex;
  {$ENDIF}
    UpdateSpeedButton;
  end;
end;

procedure TMainForm.PBAlphaPaint(Sender: TObject);
begin
  PBAlpha.Canvas.Draw(
    (PBAlpha.ClientWidth - KAlphaBitmap.Width) div 2,
    (PBAlpha.ClientHeight - KAlphaBitmap.Height) div 2,
    KAlphaBitmap);
end;

procedure TMainForm.ACDisplayHorzExecute(Sender: TObject);
begin
  UpdateIcon;
end;

procedure TMainForm.ACDisplayHorzUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := CBDisplayAll.Checked;
end;

procedure TMainForm.ACExtractBitmapExecute(Sender: TObject);
var
  BM: TBitmap;
  Png: TKPngImage;
begin
{$IFDEF USE_PNG_SUPPORT}
  SDMain.Filter := cFilterPngsBitmaps;
  SDMain.DefaultExt := 'png';
{$ELSE}
  SDMain.Filter := cFilterBitmaps;
  SDMain.DefaultExt := 'bmp';
{$ENDIF}
  if SDMain.Execute then
  begin
  {$IFDEF USE_PNG_SUPPORT}
    if ExtractFileExt(SDMain.FileName) = '.png' then
    begin
      Png := TKPngImage.Create;
      try
        KIcon.CopyToPng(KIcon.CurrentIndex, Png);
        Png.SaveToFile(SDMain.FileName);
      finally
        Png.Free;
      end;
    end else
  {$ENDIF}
    begin
      BM := TBitmap.Create;
      try
        KIcon.CopyToBitmap(KIcon.CurrentIndex, BM);
        BM.SaveToFile(SDMain.FileName);
      finally
        BM.Free;
      end;
    end;
  end;
end;

{$IFDEF FPC}
initialization
  {$I Main.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
