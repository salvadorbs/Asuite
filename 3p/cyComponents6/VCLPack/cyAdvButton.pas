{   Component(s):
    tcyAdvButton

    Description:
    Advanced Button with color/border and wallpaper features ...

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyAdvButton;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Themes, Types, Controls, Graphics, Buttons, {$IFDEF DELPHI2009_OR_ABOVE} pngimage, {$ENDIF}
      VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics, cyBaseButton;

type
  TcyAdvButton = class;

  TRenderProp=class(TPersistent)
  private
    FOwner: TcyAdvButton;
    FOnChange: TNotifyEvent;
    FDegrade: TcyGradient;
    FWallpaper: TcyBgPicture;
    FUseDefaultFont: Boolean;
    FUseDefaultGlyph: Boolean;
    FUseDefaultWallpaper: Boolean;
    FFont: TFont;
    FGlyph: TPicture;
    FImageListIndex: Integer;
    procedure SetDegrade(const Value: TcyGradient);
    procedure SetWallpaper(const Value: TcyBgPicture);
    procedure SetUseDefaultFont(const Value: Boolean);
    procedure SetUseDefaultWallpaper(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetUseDefaultGlyph(const Value: Boolean);
    procedure SetGlyph(const Value: TPicture);
    procedure SetImageListIndex(const Value: Integer);
  protected
    procedure PropertiesChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property Font: TFont read FFont write SetFont;
    property Glyph: TPicture read FGlyph write SetGlyph;
    property ImageListIndex: Integer read FImageListIndex write SetImageListIndex default -1;
    property UseDefaultFont: Boolean read FUseDefaultFont write SetUseDefaultFont default true;
    property UseDefaultGlyph: Boolean read FUseDefaultGlyph write SetUseDefaultGlyph default true;
    property UseDefaultWallpaper: Boolean read FUseDefaultWallpaper write SetUseDefaultWallpaper default true;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcyAdvButton = class(TcyBaseButton)
  private
    FButtonHot: TRenderProp;
    FButtonDown: TRenderProp;
    FButtonNormal: TRenderProp;
    FButtonDisabled: TRenderProp;
    FWallpaper: TcyBgPicture;
    FBordersDisabled: TcyBevels;
    FBordersHot: TcyBevels;
    FBordersDown: TcyBevels;
    FBordersNormal: TcyBevels;
    FImageList: TImageList;
    procedure SetButtonDisabled(const Value: TRenderProp);
    procedure SetButtonDown(const Value: TRenderProp);
    procedure SetButtonHot(const Value: TRenderProp);
    procedure SetButtonNormal(const Value: TRenderProp);
    procedure SetWallpaper(const Value: TcyBgPicture);
    procedure SetBordersDisabled(const Value: TcyBevels);
    procedure SetBordersDown(const Value: TcyBevels);
    procedure SetBordersHot(const Value: TcyBevels);
    procedure SetBordersNormal(const Value: TcyBevels);
    procedure SetImageList(const Value: TImageList);
  protected
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); override;
    procedure DrawButton(aState: TButtonState; Hot: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefaultWallpaperChanged(Sender: TObject);
    procedure ButtonNormalChanged(Sender: TObject);
    procedure ButtonHotChanged(Sender: TObject);
    procedure ButtonDownChanged(Sender: TObject);
    procedure ButtonDisabledChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetButtonState: TButtonState;
    // Herited from TcyBaseButton :
    property MouseOver;
    property MouseLeftDown;
  published
    property BordersNormal: TcyBevels read FBordersNormal write SetBordersNormal;
    property BordersHot: TcyBevels read FBordersHot write SetBordersHot;
    property BordersDown: TcyBevels read FBordersDown write SetBordersDown;
    property BordersDisabled: TcyBevels read FBordersDisabled write SetBordersDisabled;
    property ButtonNormal: TRenderProp read FButtonNormal write SetButtonNormal;
    property ButtonHot: TRenderProp read FButtonHot write SetButtonHot;
    property ButtonDown: TRenderProp read FButtonDown write SetButtonDown;
    property ButtonDisabled: TRenderProp read FButtonDisabled write SetButtonDisabled;
    property ImageList: TImageList read FImageList write SetImageList;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    // Herited from TcyBaseButton :
    property CaptionOrientation;
    property GlyphX;
    property OnPaint;
  end;

implementation

{ TRenderProp }
procedure TRenderProp.Assign(Source: TPersistent);
begin
//  inherited;

  if Source is TRenderProp
  then begin
    FDegrade.Assign(TRenderProp(Source).FDegrade);
    FFont.Assign(TRenderProp(Source).FFont);
    FGlyph.Assign(TRenderProp(Source).FGlyph);
    FImageListIndex := TRenderProp(Source).FImageListIndex;
    FUseDefaultFont := TRenderProp(Source).FUseDefaultFont;
    FUseDefaultGlyph := TRenderProp(Source).FUseDefaultGlyph;
    FUseDefaultWallpaper := TRenderProp(Source).FUseDefaultWallpaper;
    FWallpaper.Assign(TRenderProp(Source).FWallpaper);
    PropertiesChanged(self);
  end;
end;

constructor TRenderProp.Create(AOwner: TComponent);
begin
  FOwner := TcyAdvButton(AOwner);
  FImageListIndex := -1;
  FUseDefaultFont := true;
  FUseDefaultGlyph := true;
  FUseDefaultWallpaper := true;
  FGlyph := TPicture.Create;
  FWallpaper := TcyBgPicture.Create(FOwner);
  FWallpaper.OnChange := PropertiesChanged;
  FDegrade := TcyGradient.Create(FOwner);
  FDegrade.OnChange := PropertiesChanged;
  FFont := TFont.Create;
  FFont.OnChange := PropertiesChanged;
end;

destructor TRenderProp.Destroy;
begin
  FFont.Free;
  FDegrade.Free;
  FWallpaper.Free;
  FGlyph.Free;
  inherited;
end;

procedure TRenderProp.PropertiesChanged(Sender: TObject);
begin
  if not (csLoading in FOwner.ComponentState)
  then
    if Assigned(FOnChange)
    then FOnChange(Self);
end;

procedure TRenderProp.SetDegrade(const Value: TcyGradient);
begin
  FDegrade := Value;
end;

procedure TRenderProp.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  PropertiesChanged(nil);
end;

procedure TRenderProp.SetGlyph(const Value: TPicture);
begin
  try
    FGlyph.Assign(Value);

    if not UseDefaultGlyph
    then PropertiesChanged(nil);
  except
  end;
end;

procedure TRenderProp.SetImageListIndex(const Value: Integer);
begin
  FImageListIndex := Value;
  if Assigned(FOwner.FImageList)
  then PropertiesChanged(nil);
end;

procedure TRenderProp.SetUseDefaultFont(const Value: Boolean);
begin
  FUseDefaultFont := Value;
  PropertiesChanged(nil);
end;

procedure TRenderProp.SetUseDefaultGlyph(const Value: Boolean);
begin
  FUseDefaultGlyph := Value;
  PropertiesChanged(nil);
end;

procedure TRenderProp.SetUseDefaultWallpaper(const Value: Boolean);
begin
  FUseDefaultWallpaper := Value;
  PropertiesChanged(nil);
end;

procedure TRenderProp.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
  PropertiesChanged(nil);
end;

{ TcyAdvButton }
constructor TcyAdvButton.Create(AOwner: TComponent);
begin
  inherited;

  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := DefaultWallpaperChanged;

  // Borders :
  FBordersNormal := TcyBevels.Create(self, TcyBevel);
  FBordersHot := TcyBevels.Create(self, TcyBevel);
  FBordersDown := TcyBevels.Create(self, TcyBevel);
  FBordersDisabled := TcyBevels.Create(self, TcyBevel);

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then begin
        with FBordersNormal.Add do
        begin
          HighlightColor := clNavy;
          ShadowColor := clNavy;
        end;

        with FBordersHot.Add do
        begin
          HighlightColor := clAqua;
          ShadowColor := clBlue;
        end;

        with FBordersDown.Add do
        begin
          HighlightColor := clBlue;
          ShadowColor := clAqua;
        end;

        with FBordersDisabled.Add do
        begin
          HighlightColor := clMaroon;
          ShadowColor := clMaroon;
        end;
      end;

  FBordersNormal.OnChange := ButtonNormalChanged;
  FBordersHot.OnChange := ButtonHotChanged;
  FBordersDown.OnChange := ButtonDownChanged;
  FBordersDisabled.OnChange := ButtonDisabledChanged;

  // Body color :
  FButtonNormal := TRenderProp.Create(Self);
  FButtonNormal.FDegrade.FromColor := clAqua;
  FButtonNormal.FDegrade.ToColor := clBlue;
  FButtonNormal.FDegrade.SpeedPercent := 90;
  FButtonNormal.FOnChange := ButtonNormalChanged;

  FButtonHot := TRenderProp.Create(Self);
  FButtonHot.FDegrade.FromColor := clAqua;
  FButtonHot.FDegrade.ToColor := clBlue;
  FButtonHot.FDegrade.BalanceMode := bmReverseFromColor;
  FButtonHot.FOnChange := ButtonHotChanged;

  FButtonDown := TRenderProp.Create(Self);
  FButtonDown.FDegrade.FromColor := clAqua;
  FButtonDown.FDegrade.ToColor := clAqua;
  FButtonDown.FOnChange := ButtonDownChanged;

  FButtonDisabled := TRenderProp.Create(Self);
  FButtonDisabled.FDegrade.FromColor := clRed;
  FButtonDisabled.FDegrade.ToColor := clMaroon;
  FButtonDisabled.FDegrade.SpeedPercent := 90;
  FButtonDisabled.FOnChange := ButtonDisabledChanged;
end;

destructor TcyAdvButton.Destroy;
begin
  FWallpaper.Free;

  FButtonNormal.Free;
  FButtonHot.Free;
  FButtonDown.Free;
  FButtonDisabled.Free;

  FBordersNormal.Free; FBordersNormal := Nil;
  FBordersHot.Free; FBordersHot := Nil;
  FBordersDown.Free; FBordersDown := Nil;
  FBordersDisabled.Free; FBordersDisabled := Nil;
  inherited;
end;

procedure TcyAdvButton.SetBordersDisabled(const Value: TcyBevels);
begin
  FBordersDisabled := Value;
end;

procedure TcyAdvButton.SetBordersDown(const Value: TcyBevels);
begin
  FBordersDown := Value;
end;

procedure TcyAdvButton.SetBordersHot(const Value: TcyBevels);
begin
  FBordersHot := Value;
end;

procedure TcyAdvButton.SetBordersNormal(const Value: TcyBevels);
begin
  FBordersNormal := Value;
end;

procedure TcyAdvButton.SetButtonDisabled(const Value: TRenderProp);
begin
  FButtonDisabled := Value;
end;

procedure TcyAdvButton.SetButtonDown(const Value: TRenderProp);
begin
  FButtonDown := Value;
end;

procedure TcyAdvButton.SetButtonHot(const Value: TRenderProp);
begin
  FButtonHot := Value;
end;

procedure TcyAdvButton.SetButtonNormal(const Value: TRenderProp);
begin
  FButtonNormal := Value;
end;

procedure TcyAdvButton.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  Invalidate;

  if Value <> nil
  then Value.FreeNotification(Self);
end;

procedure TcyAdvButton.DefaultWallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

function TcyAdvButton.GetButtonState: TButtonState;
begin
  if Enabled
  then begin
    if MouseLeftDown
    then RESULT := bsDown
    else RESULT := bsUp;
  end
  else
    RESULT := bsDisabled;
end;

procedure TcyAdvButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove
  then
    if FImagelist <> nil
    then
      if AComponent = FImageList
      then begin
        FImageList := nil;
        Invalidate;
      end;
end;

procedure TcyAdvButton.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;

  case GetButtonState of
    bsUp:
      if MouseOver
      then begin
        if FButtonHot.FUseDefaultWallpaper then Invalidate;
      end
      else
       if FButtonNormal.FUseDefaultWallpaper then Invalidate;

    bsDisabled:
      if FButtonDisabled.FUseDefaultWallpaper then Invalidate;

    bsDown, bsExclusive:
      if FButtonDown.FUseDefaultWallpaper then Invalidate;
  end;
end;

procedure TcyAdvButton.ButtonNormalChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then begin
    FDesignDraw  := true;
    FDesignState := bsUp;
    FMouseOver := false;
    Invalidate;
  end
  else
    if GetButtonState = bsUp then Invalidate;
end;

procedure TcyAdvButton.ButtonHotChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then begin
    FDesignDraw  := true;
    FDesignState := bsUp;
    FMouseOver := true;
    Invalidate;
  end
  else
    if FMouseOver then Invalidate;
end;

procedure TcyAdvButton.ButtonDownChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then begin
    FDesignDraw  := true;
    FDesignState := bsDown;
    FMouseOver := false;
    Invalidate;
  end
  else
    if GetButtonState in [bsDown, bsExclusive] then Invalidate;
end;

procedure TcyAdvButton.ButtonDisabledChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then begin
    FDesignDraw  := true;
    FDesignState := bsDisabled;
    FMouseOver := false;
    Invalidate;
  end
  else
    if not Enabled then Invalidate;
end;

procedure TcyAdvButton.DrawButton(aState: TButtonState; Hot: Boolean);
var
  Rect, TextRect: TRect;
  ImageHandled: Boolean;
  RenderProp: TRenderProp;
  aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY: Integer;
  ImageListIndex: Integer;
  ImageListEnabled: Boolean;
begin
//  inherited;
  Rect := ClientRect;
  ImageHandled := false;
  DrawBackground(Rect, aState, Hot);
  aGraphicWidth := 0;
  aGraphicHeight := 0;

  // Draw caption/glyph :
  case aState of
    bsUp:
      if Hot
      then RenderProp := FButtonHot
      else RenderProp := FButtonNormal;

    bsDisabled:
      RenderProp := FButtonDisabled;

    bsDown, bsExclusive:
      RenderProp := FButtonDown;
  end;

  // Font to use :
  if RenderProp.FUseDefaultFont
  then FCanvas.Font.Assign(Self.Font)
  else FCanvas.Font.Assign(RenderProp.FFont);

  if Assigned(FImageList)
  then begin
    ImageListIndex := RenderProp.ImageListIndex;
    ImageListEnabled := (ImageListIndex > -1) or (aState <> bsDisabled);
    if ImageListIndex = -1
    then ImageListIndex := FButtonNormal.ImageListIndex;

    if ImageListIndex > -1
    then begin
      aGraphicWidth := FImageList.Width;
      aGraphicHeight := FImageList.Height;
      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      FImageList.Draw(Canvas, aGraphicX, aGraphicY, ImageListIndex, ImageListEnabled);
      ImageHandled := true;
    end;
  end
  else
    if RenderProp.UseDefaultGlyph
    then begin
      if ValidGraphic(GlyphX.Graphic)
      then begin
        if GlyphX.Graphic is TBitmap
        then aGraphicWidth := GlyphX.Bitmap.Width div NumGlyphs
        else aGraphicWidth := GlyphX.Width;

        aGraphicHeight := GlyphX.Height;
      end;

      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      DrawGlyph(aGraphicX, aGraphicY, aState);
      ImageHandled := true;
    end
    else
      if ValidGraphic(RenderProp.Glyph.Graphic)
      then begin
        aGraphicWidth := RenderProp.Glyph.Width;
        aGraphicHeight := RenderProp.Glyph.Height;
        CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
        RenderProp.FGlyph.Graphic.Transparent := True;
        FCanvas.Draw(aGraphicX, aGraphicY, RenderProp.FGlyph.Graphic);
        ImageHandled := true;
      end;

  if not ImageHandled
  then begin
    aGraphicWidth := 0;
    aGraphicHeight := 0;
    CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
  end;

  Canvas.Brush.Style := bsClear;
  DrawCaption(TextRect, aState, Hot, aGraphicWidth <> 0);
end;

procedure TcyAdvButton.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
var
  PaintButton: TRenderProp;
  PaintWallPaper: TcyBgPicture;
  PaintBorders: TcyBevels;
//  Details: TThemedElementDetails;
  TopLeftColor, TopRightColor, BottomRightColor, BottomLeftColor: TColor;
begin
  // inherited;

  case aState of
    bsUp:
      if Hot
      then begin
        PaintButton := FButtonHot;
        PaintBorders := FBordersHot;
      end
      else begin
        PaintButton := FButtonNormal;
        PaintBorders := FBordersNormal;
      end;

    bsDisabled:
      begin
        PaintButton := FButtonDisabled;
        PaintBorders := FBordersDisabled;
      end;

    bsDown, bsExclusive:
      begin
        PaintButton := FButtonDown;
        PaintBorders := FBordersDown;
      end;
  end;

  // Save bounds pixel color :
  TopLeftColor := FCanvas.Pixels[0, 0];
  TopRightColor := FCanvas.Pixels[Rect.Right-1, 0];
  BottomRightColor := FCanvas.Pixels[Rect.Right-1, Rect.Bottom-1];
  BottomLeftColor := FCanvas.Pixels[0, Rect.Bottom-1];

  // Draw button body :
  PaintButton.FDegrade.Draw(FCanvas, Rect);

  // Draw Wallpaper :
  if PaintButton.UseDefaultWallpaper
  then PaintWallPaper := FWallpaper
  else PaintWallPaper := PaintButton.FWallpaper;

  cyDrawBgPicture(FCanvas, Rect, PaintWallPaper);

  // Restore bounds pixel color :
  FCanvas.Pixels[0, 0]                        := TopLeftColor;
  FCanvas.Pixels[Rect.Right-1, 0]             := TopRightColor;
  FCanvas.Pixels[Rect.Right-1, Rect.Bottom-1] := BottomRightColor;
  FCanvas.Pixels[0, Rect.Bottom-1]            := BottomLeftColor;

  // Draw borders :
  PaintBorders.DrawBevels(FCanvas, Rect, true);
  DrawInnerBorders(Rect, ColorSetPercentBrightness(PaintButton.FDegrade.FromColor, 80),
                    MediumColor(PaintButton.FDegrade.FromColor, PaintButton.FDegrade.ToColor));

  // Draw focus rect :
  if Focused
  then DrawFocusRect(FCanvas.Handle, Rect);
end;

end.
