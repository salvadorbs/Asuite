{   Component(s):
    tcyCustomBitBtn

    Description:
    Simple BitBtn with color feature

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

unit cyBitBtn;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Controls, Graphics, Buttons, VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics, cyBaseButton;

type
  TcyCustomBitBtn = class;

  TProcBeforePaint = procedure (Sender: TObject; var DrawBorders, DrawBackground, DrawWallPaper, DrawCaption, DrawGlyph: Boolean) of object;

  TcyCustomBitBtn = class(TcyBaseButton)
  private
    bDrawBorders, bDrawBackground, bDrawWallpaper, bDrawCaption, bDrawGlyph: Boolean;
    FDegrade: TcyGradient;
    FDisabledFromColor: TColor;
    FDisabledToColor: TColor;
    FDownFromColor: TColor;
    FDownToColor: TColor;
    FHotFromColor: TColor;
    FHotToColor: TColor;
    FHotBalanceMode: TDgradBalanceMode;
    FImagelistOptions: TcyImagelistOptions;
    FWallpaper: TcyBgPicture;
    FBeforePaint: TProcBeforePaint;
    procedure SetDegrade(const Value: TcyGradient);
    procedure SetImagelistOptions(const Value: TcyImagelistOptions);
    procedure SetWallpaper(const Value: TcyBgPicture);
  protected
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); override;
    procedure DrawButton(aState: TButtonState; Hot: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DegradeChanged(Sender: TObject);
    procedure WallpaperChanged(Sender: TObject);
    procedure ImagelistOptionsChanged(Sender: TObject);
    // To be Public properties : (properties defined after selecting Degrade colors, can be set by code)
    property DefaultHotFromColor: TColor read FHotFromColor write FHotFromColor;
    property DefaultHotToColor: TColor read FHotToColor write FHotToColor;
    property DefaultDownFromColor: TColor read FDownFromColor write FDownFromColor;
    property DefaultDownToColor: TColor read FDownToColor write FDownToColor;
    property DefaultDisabledFromColor: TColor read FDisabledFromColor write FDisabledFromColor;
    property DefaultDisabledToColor: TColor read FDisabledToColor write FDisabledToColor;
    // To be Published properties :
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property ImagelistOptions: TcyImagelistOptions read FImagelistOptions write SetImagelistOptions;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property BeforePaint: TProcBeforePaint read FBeforePaint write FBeforePaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyBitBtn = class(TcyCustomBitBtn)
  private
  protected
  public
    // Properties defined after selecting Degrade colors, can be set by code :
    property DefaultHotFromColor;
    property DefaultHotToColor;
    property DefaultDownFromColor;
    property DefaultDownToColor;
    property DefaultDisabledFromColor;
    property DefaultDisabledToColor;
    // Herited from TcyBaseButton :
    property MouseOver;
    property MouseLeftDown;
  published
    // Herited from TcyBaseButton :
    property CaptionOrientation;
    // Herited from TcyCustomBitBtn :
    property Degrade;
    property GlyphX;
    property ImagelistOptions;
    property Wallpaper;
    property BeforePaint;
    property OnPaint;
  end;

implementation

{ TcyCustomBitBtn }
constructor TcyCustomBitBtn.Create(AOwner: TComponent);
begin
  inherited;
  FImagelistOptions := TcyImagelistOptions.Create(Self);
  FImagelistOptions.OnChange := ImagelistOptionsChanged;
  FDegrade := TcyGradient.Create(Self);
  FDegrade.OnChange := DegradeChanged;
  FDegrade.FromColor := clYellow;
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := WallpaperChanged;
  FDegrade.ToColor := $000080FF;  // Orange color ...
  FDegrade.SpeedPercent := 90;
  FHotBalanceMode := bmReverseFromColor;
  DegradeChanged(Self);
end;

destructor TcyCustomBitBtn.Destroy;
begin
  FImagelistOptions.Free;
  FWallpaper.Free;
  FDegrade.Free;
  inherited;
end;

procedure TcyCustomBitBtn.DegradeChanged(Sender: TObject);
begin
  if FDegrade.FromColor = FDegrade.ToColor
  then begin
    FHotFromColor := ColorSetPercentBrightness(FDegrade.FromColor, 20);
    FHotToColor := FHotFromColor;
  end
  else begin
    FHotFromColor := FDegrade.FromColor;
    FHotToColor := FDegrade.ToColor;
  end;

  FDownFromColor := ColorSetPercentBrightness(FDegrade.FromColor, -20);
  FDownToColor := ColorSetPercentBrightness(FDegrade.ToColor, -20);
  FDisabledFromColor := ColorSetPercentBrightness(FDegrade.FromColor, 30);
  FDisabledToColor := ColorSetPercentBrightness(FDegrade.ToColor, 30);
  Invalidate;
end;

procedure TcyCustomBitBtn.ImagelistOptionsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomBitBtn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  // Careful, FImageListOptions may be not created already, check operation first!
  if Operation = opRemove
  then
    if FImagelistOptions.ImageList <> nil
    then
      if AComponent = FImagelistOptions.ImageList
      then begin
        FImagelistOptions.ImageList := nil;
        Invalidate;
      end;
end;

procedure TcyCustomBitBtn.SetDegrade(const Value: TcyGradient);
begin
  FDegrade := Value;
end;

procedure TcyCustomBitBtn.SetImagelistOptions(const Value: TcyImagelistOptions);
begin
  FImagelistOptions.Assign(Value);
end;

procedure TcyCustomBitBtn.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

procedure TcyCustomBitBtn.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomBitBtn.DrawButton(aState: TButtonState; Hot: Boolean);
var
  ImageHandled: Boolean;
  Rect, TextRect: TRect;
  aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY: Integer;
  ImageListIndex: Integer;
  ImageListEnabled: Boolean;
begin
  bDrawBorders := true;
  bDrawBackground := true;
  bDrawWallpaper := true;
  bDrawCaption := true;
  bDrawGlyph := true;
  if Assigned(FBeforePaint) then
    FBeforePaint(Self, bDrawBorders, bDrawBackground, bDrawWallpaper, bDrawCaption, bDrawGlyph);

  Rect := ClientRect;
  ImageHandled := false;
  DrawBackground(Rect, aState, Hot);

  // Draw caption/glyph :
  FCanvas.Font := Self.Font;

  if Assigned(FImagelistOptions.ImageList)
  then begin
    FImagelistOptions.GetDrawingParams(aState, ImageListIndex, ImageListEnabled);

    if ImageListIndex > -1
    then begin
      aGraphicWidth := FImagelistOptions.ImageList.Width;
      aGraphicHeight := FImagelistOptions.ImageList.Height;
      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      if bDrawGlyph then
        FImagelistOptions.ImageList.Draw(FCanvas, aGraphicX, aGraphicY, ImageListIndex, ImageListEnabled);
      ImageHandled := true;
    end;
  end
  else
    if ValidGraphic(GlyphX.Graphic)
    then begin
      if GlyphX.Graphic is graphics.TBitmap
      then aGraphicWidth := GlyphX.Bitmap.Width div NumGlyphs
      else aGraphicWidth := GlyphX.Width;

      aGraphicHeight := GlyphX.Height;
      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      if bDrawGlyph then
        DrawGlyph(aGraphicX, aGraphicY, aState);
      ImageHandled := true;
    end;

  if not ImageHandled
  then begin
    aGraphicWidth := 0;
    aGraphicHeight := 0;
    CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
  end;

  if bDrawCaption then
  begin
    FCanvas.Brush.Style := bsClear;
    DrawCaption(TextRect, aState, Hot, aGraphicWidth <> 0);
  end;
end;

procedure TcyCustomBitBtn.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
var
  fromColor, toColor, FrameColor, BorderLeftTopColor, BorderRightBottomColor: TColor;
  balanceMode: TDgradBalanceMode;
  TopLeftColor, TopRightColor, BottomRightColor, BottomLeftColor: TColor;
begin
  // inherited;
  case aState of
    bsUp:
      begin
        if Hot
        then begin
          fromColor := FHotFromColor;
          toColor := FHotToColor;
          balanceMode := FHotBalanceMode;
        end
        else begin
          fromColor := FDegrade.FromColor;
          toColor := FDegrade.ToColor;
          balanceMode := FDegrade.BalanceMode;
        end;

        BorderLeftTopColor := ColorSetPercentBrightness(fromColor, 40);
        BorderRightBottomColor := MediumColor(fromColor, ToColor);
        FrameColor := ColorSetPercentBrightness(toColor, FFramePercent);
      end;

    bsDisabled:
      begin
        fromColor := FDisabledFromColor;
        toColor := FDisabledToColor;
        balanceMode := FDegrade.BalanceMode;
        BorderLeftTopColor := ColorSetPercentBrightness(fromColor, 40);
        BorderRightBottomColor := MediumColor(fromColor, ToColor);
        FrameColor := ColorSetPercentBrightness(FDisabledToColor, FFramePercent);
      end;

    bsDown:
      begin
        fromColor := FDownFromColor;
        toColor := FDownToColor;
        balanceMode := bmMirror;
        BorderLeftTopColor := MediumColor(fromColor, ToColor);
        BorderRightBottomColor := ColorSetPercentBrightness(fromColor, 40);
        FrameColor := ColorSetPercentBrightness(FDownToColor, FFramePercent);
      end;
  end;

  // Save bounds pixel color :
  TopLeftColor := FCanvas.Pixels[0, 0];
  TopRightColor := FCanvas.Pixels[Rect.Right-1, 0];
  BottomRightColor := FCanvas.Pixels[Rect.Right-1, Rect.Bottom-1];
  BottomLeftColor := FCanvas.Pixels[0, Rect.Bottom-1];

  // Draw button body :
  if bDrawBackground then
    cyGradientFill(FCanvas, Rect, fromColor, toColor,  FDegrade.Orientation, FDegrade.Balance,
                    balanceMode, FDegrade.MaxDegrade, FDegrade.SpeedPercent);

  // Draw Wallpaper :
  if bDrawWallpaper then
    cyDrawBgPicture(FCanvas, Rect, FWallpaper);

  // Restore bounds pixel color :
  FCanvas.Pixels[0, 0]                        := TopLeftColor;
  FCanvas.Pixels[Rect.Right-1, 0]             := TopRightColor;
  FCanvas.Pixels[Rect.Right-1, Rect.Bottom-1] := BottomRightColor;
  FCanvas.Pixels[0, Rect.Bottom-1]            := BottomLeftColor;

  // Draw frame :
  if bDrawBorders then
    DrawBorders(Rect, FrameColor, FrameColor);

  if bDrawBorders then
    if aState = bsDown
    then DrawBorders(Rect, BorderLeftTopColor, BorderRightBottomColor)
    else DrawInnerBorders(Rect, BorderLeftTopColor, BorderRightBottomColor);

  // Draw focus rect :
  if Focused and bDrawBackground then
    DrawFocusRect(FCanvas.Handle, Rect);
end;

end.
