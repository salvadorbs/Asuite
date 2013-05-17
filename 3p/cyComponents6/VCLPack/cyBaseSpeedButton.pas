{   Component(s):
    tcyBaseSpeedButton

    Description:
    Component base for speedbuttons ...

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

unit cyBaseSpeedButton;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Graphics, Windows, Messages, StdCtrls, Controls, Buttons, Dialogs, VCL.cyTypes, VCL.cyGraphics;

{ *** Glyph and NumGlyph properies handling ***
  Glyph 1 : normal state
  Glyph 2 : disabled state
  Glyph 3 : Mouse down state
  Glyph 4 : Exclusive state }

type
  TFlatHotStyle = (hsWindowsTheme, hsClassic, hsHidden, hsMetro);
  TFlatDownStyle = (dsWindowsTheme, dsClassic, dsHidden, dsMetro);

  TcyBaseSpeedButton = class(TSpeedButton)
  private
    FCaptionOrientation: TCaptionOrientation;
    FFlatHotStyle: TFlatHotStyle;
    FFlatDownStyle: TFlatDownStyle;
    FMouseOver: Boolean;
    FWordWrap: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FPicGlyph: TPicture;
    procedure SetCaptionOrientation(const Value: TCaptionOrientation);
    procedure SetWordWrap(const Value: Boolean);
    // Handle my own MouseOver variable :
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetFlatDownStyle(const Value: TFlatDownStyle);
    procedure SetFlatHotStyle(const Value: TFlatHotStyle);
    procedure SetPicGlyph(const Value: TPicture);
    procedure GlyphChanged(Sender: TObject);
  protected
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CalcLayout(Rect: TRect; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); virtual;
    procedure DrawButton(aState: TButtonState; Hot: Boolean); virtual;
    procedure DrawCaption(aRect: TRect; aState: TButtonState; Hot: Boolean; GlyphExists: Boolean);
    procedure DrawGlyph(X, Y: Integer; aState: TButtonState);
    procedure DrawBorders(var Rect: TRect; LeftTopColor, RightBottomColor: TColor);
    procedure DrawInnerBorders(var Rect: TRect; TopColor, BottomColor: TColor);
    // Public properties :
    property MouseOver: Boolean read FMouseOver;
    // Published properties :
    property CaptionOrientation: TCaptionOrientation read FCaptionOrientation write SetCaptionOrientation default coHorizontal;
    property FlatDownStyle: TFlatDownStyle read FFlatDownStyle write SetFlatDownStyle default dsClassic;
    property FlatHotStyle: TFlatHotStyle read FFlatHotStyle write SetFlatHotStyle default hsClassic;
    property GlyphX: TPicture read FPicGlyph write SetPicGlyph;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property ButtonState: TButtonState read FState;
  published
  end;

implementation

{TcyBaseSpeedButton}
constructor TcyBaseSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FCaptionOrientation := coHorizontal;
  FFlatDownStyle := dsClassic;
  FFlatHotStyle := hsClassic;
  FMouseOver := false;
  FWordWrap := false;
  FPicGlyph := TPicture.Create;
  Glyph.OnChange := GlyphChanged;
end;

destructor TcyBaseSpeedButton.Destroy;
begin
  FPicGlyph.Free;
  inherited;
end;

procedure TcyBaseSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  Inherited;
  if (not FMouseOver) and Enabled
  then
    if Flat
    then begin
      if FFlatHotStyle <> hsHidden
      then Invalidate;
    end
    else
      Invalidate;

  FMouseOver := true;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TcyBaseSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  Inherited;
  if (FMouseOver) and Enabled
  then
    if Flat
    then begin
      if FFlatHotStyle <> hsHidden
      then Invalidate;
    end
    else
      Invalidate;

  FMouseOver := false;
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure TcyBaseSpeedButton.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

procedure TcyBaseSpeedButton.SetCaptionOrientation(const Value: TCaptionOrientation);
begin
  if FCaptionOrientation = Value then Exit;
  FCaptionOrientation := Value;
  Invalidate;

  if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
  then
    if (FCaptionOrientation <> coHorizontal) and CaptionOrientationWarning
    then begin
      CaptionOrientationWarning := false;
      ShowMessage(cCaptionOrientationWarning);
    end;
end;

procedure TcyBaseSpeedButton.SetFlatDownStyle(const Value: TFlatDownStyle);
begin
  FFlatDownStyle := Value;

  if Flat and ((FState = bsDown) or (FState = bsExclusive))
  then Invalidate;
end;

procedure TcyBaseSpeedButton.SetFlatHotStyle(const Value: TFlatHotStyle);
begin
  FFlatHotStyle := Value;

  if Flat and (MouseOver)
  then Invalidate;
end;

procedure TcyBaseSpeedButton.SetPicGlyph(const Value: TPicture);
begin
  FPicGlyph.Assign(Value);
  Invalidate;
end;

procedure TcyBaseSpeedButton.SetEnabled(Value: Boolean);
begin
  if Value = Enabled then Exit;

  inherited;

  // Adjust FState:
  if Enabled then
  begin
    if FState = bsDisabled
    then
      if Down and (GroupIndex <> 0)
      then FState := bsExclusive
      else FState := bsUp;
  end
  else
    FState := bsDisabled;

  Invalidate;
end;

procedure TcyBaseSpeedButton.Paint;
var
  Rect: TRect;
  TopLeftColor, TopRightColor, BottomRightColor, BottomLeftColor: TColor;
begin
  // inherited;

  // Save bounds pixel color :
  if not Flat
  then begin
    Rect := ClientRect;
    TopLeftColor := Canvas.Pixels[0, 0];
    TopRightColor := Canvas.Pixels[Rect.Right-1, 0];
    BottomRightColor := Canvas.Pixels[Rect.Right-1, Rect.Bottom-1];
    BottomLeftColor := Canvas.Pixels[0, Rect.Bottom-1];
  end;

  DrawButton(FState, MouseOver);

  // Restore bounds pixel color :
  if not Flat
  then begin
    Canvas.Pixels[0, 0]                        := TopLeftColor;
    Canvas.Pixels[Rect.Right-1, 0]             := TopRightColor;
    Canvas.Pixels[Rect.Right-1, Rect.Bottom-1] := BottomRightColor;
    Canvas.Pixels[0, Rect.Bottom-1]            := BottomLeftColor;
  end;

  if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TcyBaseSpeedButton.DrawButton(aState: TButtonState; Hot: Boolean);
begin
  //
end;

procedure TcyBaseSpeedButton.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
begin
  //
end;

procedure TcyBaseSpeedButton.DrawCaption(aRect: TRect; aState: TButtonState; Hot: Boolean; GlyphExists: Boolean);
var
  aAlignment: TAlignment;
  aTextLayout: TTextLayout;
  DrawStyle: LongInt;
  TmpFont: TFont;
begin
  if Caption = '' then EXIT;

  aAlignment := taCenter;
  aTextLayout := tlCenter;

  if GlyphExists
  then
    case Layout of
      blGlyphLeft:   aAlignment := taLeftJustify;
      blGlyphRight:  aAlignment := taRightJustify;
      blGlyphTop:    aTextLayout := tlTop;
      blGlyphBottom: aTextLayout := tlBottom;
    end;

  DrawStyle := VCL.cyGraphics.DrawTextFormatFlags(0, aAlignment, aTextLayout, FWordWrap);
  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);

  if FCaptionOrientation = coHorizontal
  then begin
    cyDrawText(Canvas.Handle, Caption, aRect, DrawStyle);
  end
  else begin
    TmpFont := cyCreateFontIndirect(Font, FCaptionOrientation);
    try
      Canvas.Font.Assign(TmpFont);
      cyDrawVerticalText(Canvas, Caption, aRect, DrawStyle, FCaptionOrientation, aAlignment, aTextLayout);
    finally
      TmpFont.Free;
    end;
  end;
end;

procedure TcyBaseSpeedButton.DrawGlyph(X, Y: Integer; aState: TButtonState);
var
  aBmp: Graphics.TBitmap;
  GlyphState: TButtonState;
  fromRect, toRect: TRect;
begin
  if not ValidGraphic(FPicGlyph.Graphic) then EXIT;
  GlyphState := aState;
  if Ord(GlyphState) >= NumGlyphs
  then GlyphState := bsUp;

  if not (FPicGlyph.Graphic is graphics.TBitmap)
  then begin
    if not FPicGlyph.Graphic.Transparent
    then FPicGlyph.Graphic.Transparent := true;

    Canvas.Draw(X, Y, FPicGlyph.Graphic);
  end
  else
    try
      aBmp := Graphics.TBitmap.Create;
      aBmp.Width := FPicGlyph.Bitmap.Width div NumGlyphs;
      aBmp.Height := FPicGlyph.Bitmap.Height;
      aBmp.Palette := CopyPalette(FPicGlyph.Bitmap.Palette);
      fromRect := Rect(Ord(GlyphState) * aBmp.Width, 0, (Ord(GlyphState) + 1) * aBmp.Width, aBmp.Height);
      toRect := Rect(0, 0, aBmp.Width, aBmp.Height);
      aBmp.Canvas.CopyRect(toRect, FPicGlyph.Bitmap.Canvas, fromRect);
      aBmp.Transparent := True;
      Canvas.Draw(X, Y, aBmp);
    finally
      aBmp.Free;
    end;
end;

procedure TcyBaseSpeedButton.CalcLayout(Rect: TRect; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
var SizeNeed, TextSize: Integer;

      function CalcTextSize: Integer;
      var
        CalcRect: TRect;
        CalcFlags: LongInt;
        WordWrapText: Boolean;
      begin
        Result := 0;
        if Caption = '' then Exit;

        CalcRect := Rect;
        // Wordwrap only works for FCaptionOrientation = coHorizontal !
        WordWrapText := WordWrap and (FCaptionOrientation = coHorizontal);

        // WordWrap mode (horizontal text) : we need to set space avaible for text :
        if WordWrapText and (Layout in [blGlyphLeft, blGlyphRight]) then
          CalcRect.Right := CalcRect.Right - Spacing - GraphicWidth;

        CalcFlags := VCL.cyGraphics.DrawTextFormatFlags(0, taLeftJustify, tlTop, WordWrapText);
        CalcFlags := CalcFlags or DT_CALCRECT;
        CalcFlags := DrawTextBiDiModeFlags(CalcFlags);

        {$IFDEF DELPHI2009_OR_ABOVE}
          Windows.DrawText(Canvas.Handle, Caption, -1, CalcRect, CalcFlags);
        {$ELSE}
          Windows.DrawText(Canvas.Handle, PChar(Caption), -1, CalcRect, CalcFlags);
        {$ENDIF}

        // Get CalcRect :
        if FCaptionOrientation in [coHorizontal, coHorizontalReversed]
        then begin
          // Horizontal Text :
          if Layout in [blGlyphLeft, blGlyphRight]
          then Result := CalcRect.Right - CalcRect.Left
          else Result := CalcRect.Bottom - CalcRect.Top;
        end
        else begin
          // Vertical Text :
          if Layout in [blGlyphLeft, blGlyphRight]
          then Result := CalcRect.Bottom - CalcRect.Top
          else Result := CalcRect.Right - CalcRect.Left;
        end;
      end;

begin
  if GraphicWidth = 0
  then begin
    TextRect := Rect;

    if Margin <> -1
    then
      case Layout of
        blGlyphLeft:   Inc(TextRect.Left, Margin);
        blGlyphRight:  Dec(TextRect.Right, Margin);
        blGlyphTop:    Inc(TextRect.Top, Margin);
        blGlyphBottom: Dec(TextRect.Bottom, Margin);
      end;
    Exit;
  end;

  // Calc Graphic position :
  if Margin <> -1
  then begin
    // TextRect will be set after define the position of Graphic :
    case Layout of
      blGlyphLeft, blGlyphRight:
        begin
          GraphicY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - GraphicHeight div 2;

          if Layout = blGlyphLeft
          then begin
            GraphicX := Rect.Left;
            if Margin <> -1 then Inc(GraphicX, Margin);
          end
          else begin
            GraphicX := Rect.Right - GraphicWidth;
            if Margin <> -1 then Dec(GraphicX, Margin);
          end;
        end;

      blGlyphTop, blGlyphBottom:
        begin
          GraphicX := Rect.Left + (Rect.Right-Rect.Left) div 2 - GraphicWidth div 2;

          if Layout = blGlyphTop
          then begin
            GraphicY := Rect.Top;
            if Margin <> -1 then Inc(GraphicY, Margin);
          end
          else begin
            GraphicY := Rect.Bottom - GraphicHeight;
            if Margin <> -1 then Dec(GraphicY, Margin);
          end;
        end;
    end;
  end
  else begin
    // Margin = -1, so center Text and Graphic on Rect parameter : Glyph + Spacing + Text
    TextSize := CalcTextSize;

    case Layout of
      blGlyphLeft, blGlyphRight:
        begin
          SizeNeed := GraphicWidth;
          if TextSize <> 0 then Inc(SizeNeed, Spacing + TextSize);
          GraphicY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - GraphicHeight div 2;

          if Layout = blGlyphLeft
          then GraphicX := Rect.Left + (Rect.Right - Rect.Left) div 2 - SizeNeed div 2
          else GraphicX := Rect.Left + (Rect.Right - Rect.Left) div 2 + SizeNeed div 2 - GraphicWidth;
        end;

      blGlyphTop, blGlyphBottom:
        begin
          SizeNeed := GraphicHeight;
          if TextSize <> 0 then
            Inc(SizeNeed, Spacing + TextSize);
          GraphicX := Rect.Left + (Rect.Right-Rect.Left) div 2 - GraphicWidth div 2;

          if Layout = blGlyphTop
          then GraphicY := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - SizeNeed div 2
          else GraphicY := Rect.Top + (Rect.Bottom - Rect.Top) div 2 + SizeNeed div 2 - GraphicHeight;
        end;
    end;
  end;

  // Define TextRect :
  case Layout of
    blGlyphLeft:   TextRect := classes.Rect(GraphicX + GraphicWidth + Spacing, Rect.Top, Rect.Right, Rect.Bottom);
    blGlyphRight:  TextRect := classes.Rect(Rect.Left, Rect.Top, GraphicX - Spacing, Rect.Bottom);
    blGlyphTop:    TextRect := classes.Rect(Rect.Left, GraphicY + GraphicHeight + Spacing, Rect.Right, Rect.Bottom);
    blGlyphBottom: TextRect := classes.Rect(Rect.Left, Rect.Top, Rect.Right, GraphicY - Spacing);
  end;
end;

procedure TcyBaseSpeedButton.DrawBorders(var Rect: TRect; LeftTopColor, RightBottomColor: TColor);
begin
  with Canvas do
  begin
    // Left and top:
    Pen.Color := LeftTopColor;
    MoveTo(Rect.Left, Rect.Bottom - 2);
    LineTo(Rect.Left, Rect.Top);
    MoveTo(Rect.Left+1, Rect.Top);
    LineTo(Rect.Right-1, Rect.Top);

    // Right and Bottom:
    Pen.Color := RightBottomColor;
    MoveTo(Rect.Right-1, Rect.Top + 1);
    LineTo(Rect.Right-1, Rect.Bottom-1);
    MoveTo(Rect.Right-2, Rect.Bottom-1);
    LineTo(Rect.Left, Rect.Bottom-1);

    if not Flat then
      InflateRect(Rect, -1, -1);
  end;
end;

procedure TcyBaseSpeedButton.DrawInnerBorders(var Rect: TRect; TopColor, BottomColor: TColor);
var
  aHeight, Y16, Y26, Y46, Y56: Integer;
  CenterColor, InterMedColor: TColor;
begin
  aHeight := Rect.Bottom - Rect.Top;
  Y16     := Rect.Top + aHeight div 6;
  Y26     := Rect.Top + MulDiv(aHeight, 2, 6);
  Y46     := Rect.Top + MulDiv(aHeight, 4, 6);
  Y56     := Rect.Top + MulDiv(aHeight, 5, 6);

  CenterColor := MediumColor(TopColor, BottomColor);

  // LeftCenter to TopLeft to TopRight to RightCenter:
  Canvas.Pen.Color := CenterColor;
  Canvas.MoveTo(Rect.Left, Y46);
  Canvas.LineTo(Rect.Left, Y26);
  InterMedColor := MediumColor(TopColor, CenterColor);
  Canvas.Pen.Color := InterMedColor;
  Canvas.LineTo(Rect.Left, Y16);
  Canvas.Pen.Color := TopColor;
  Canvas.LineTo(Rect.Left, Rect.Top);
  Canvas.LineTo(Rect.Right-1, Rect.Top);
  Canvas.LineTo(Rect.Right-1, Y16);
  Canvas.Pen.Color := InterMedColor;
  Canvas.LineTo(Rect.Right-1, Y26);
  Canvas.Pen.Color := CenterColor;
  Canvas.LineTo(Rect.Right-1, Y46);
  // RightCenter to RightBottom to LeftBottom to LeftCenter:
  InterMedColor := MediumColor(BottomColor, CenterColor);
  Canvas.Pen.Color := InterMedColor;
  Canvas.LineTo(Rect.Right-1, Y56);
  Canvas.Pen.Color := BottomColor;
  Canvas.LineTo(Rect.Right-1, Rect.Bottom-1);
  Canvas.LineTo(Rect.Left, Rect.Bottom-1);
  Canvas.LineTo(Rect.Left, Y56);
  Canvas.Pen.Color := InterMedColor;
  Canvas.LineTo(Rect.Left, Y46);
  InflateRect(Rect, -1, -1);
end;

procedure TcyBaseSpeedButton.GlyphChanged(Sender: TObject);
begin
  // Import image from Glyph property :
  if not Glyph.Empty
  then begin
    GlyphX.Assign(Glyph);
    Glyph := Nil;

    if not (csLoading in ComponentState) and (csDesigning in ComponentState)
    then ShowMessage('Property Glyph: TBitmap deprecated, use property GlyphX.');
  end;
end;

end.
