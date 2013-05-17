{   Component(s):
    tcyStaticText

    Description:
    A StaticText with gradient background, wallpaper, colored border, caption orientation, ellipsis mode, caption layout, wordwrap and glyph/imageList

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

unit cyAdvStaticText;

interface

uses Classes, Types, Buttons, graphics, VCL.cyClasses, VCL.cyGraphics, cyBaseStaticText;

type
  TcyCustomAdvStaticText = class(TcyBaseStaticText)
  private
    FDegrade: TcyGradient;
    FImagelistOptions: TcyImagelistOptions;
    FWallpaper: TcyBgPicture;
    procedure SetDegrade(const Value: TcyGradient);
    procedure DegradeChanged(Sender: TObject);
    procedure SetImagelistOptions(const Value: TcyImagelistOptions);
    procedure SetWallpaper(const Value: TcyBgPicture);
  protected
    procedure DrawStaticText(aState: TButtonState; Hot: Boolean); override;
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); override;
    procedure ImagelistOptionsChanged(Sender: TObject);
    procedure WallpaperChanged(Sender: TObject);
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property ImagelistOptions: TcyImagelistOptions read FImagelistOptions write SetImagelistOptions;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyAdvStaticText = class(TcyCustomAdvStaticText)
  private
  protected
  public
    property MouseOver: Boolean read FMouseOver;
    property MouseLeftDown: Boolean read FMouseLeftDown;
  published
    // TWinControl properties:
    property Align;
    property Anchors;
    // Not used ... property AutoSize;
    property BiDiMode;
    property Caption;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DELPHI2009_OR_ABOVE} property ParentDoubleBuffered; {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseActivate; {$ENDIF}
    property OnMouseDown;
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseEnter; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseLeave; {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TcyBaseStaticText properties :
    property CaptionAlignment;
    property Bevels;
    property CaptionOrientation;
    property CaptionRender;
    property CaptionLayout;
    property FocusControl;
    property GlyphLayout;
    property GlyphAlignment;
    property GlyphX;
    property NumGlyphs;
    property Layout;
    property Margin;
    property Spacing;
    property Transparent;
    property WordWrap;
    property OnPaint;
    // TcyCustomAdvStaticText properties :
    property Degrade;
    property ImagelistOptions;
    property ShowAccelChar;
    property Wallpaper;
  end;

implementation

{ TcyCustomAdvStaticText }
constructor TcyCustomAdvStaticText.Create(AOwner: TComponent);
begin
  inherited;
  FDegrade := TcyGradient.Create(Self);
  FDegrade.OnChange := DegradeChanged;
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := WallpaperChanged;
  FImagelistOptions := TcyImagelistOptions.Create(Self);
  FImagelistOptions.OnChange := ImagelistOptionsChanged;
end;

destructor TcyCustomAdvStaticText.Destroy;
begin
  FWallpaper.Free;
  FDegrade.Free;
  FImagelistOptions.Free;
  inherited;
end;

procedure TcyCustomAdvStaticText.DegradeChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomAdvStaticText.SetDegrade(const Value: TcyGradient);
begin
  FDegrade := Value;
end;

procedure TcyCustomAdvStaticText.SetImagelistOptions(const Value: TcyImagelistOptions);
begin
  FImagelistOptions.Assign(Value);
end;

procedure TcyCustomAdvStaticText.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

procedure TcyCustomAdvStaticText.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomAdvStaticText.ImagelistOptionsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomAdvStaticText.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
begin
  // Draw background :
  if not Transparent then
    FDegrade.Draw(Canvas, Rect);

  // Draw Wallpaper :
  cyDrawBgPicture(Canvas, Rect, FWallpaper);

  // Draw Bevels :
  Bevels.DrawBevels(Canvas, Rect, false);
end;

procedure TcyCustomAdvStaticText.DrawStaticText(aState: TButtonState; Hot: Boolean);
var
  ImageHandled: Boolean;
  Rect, TextRect: TRect;
  aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY: Integer;
  ImageListIndex: Integer;
  ImageListEnabled: Boolean;
begin
  Rect := ClientRect;
  ImageHandled := false;
  DrawBackground(Rect, aState, Hot);

  // Draw caption/glyph :
  Canvas.Font := Self.Font;

  if Assigned(FImagelistOptions.ImageList)
  then begin
    FImagelistOptions.GetDrawingParams(aState, ImageListIndex, ImageListEnabled);

    if ImageListIndex > -1
    then begin
      aGraphicWidth := FImagelistOptions.ImageList.Width;
      aGraphicHeight := FImagelistOptions.ImageList.Height;
      CalcLayout(Rect, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
      FImagelistOptions.ImageList.Draw(Canvas, aGraphicX, aGraphicY, ImageListIndex, ImageListEnabled);
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
      DrawGlyph(aGraphicX, aGraphicY, aState);
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
  Canvas.Brush.Style := bsSolid;
end;

end.
