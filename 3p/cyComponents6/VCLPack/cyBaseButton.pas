{   Component(s):
    tcyBaseSpeedButton

    Description:
    Component base for Buttons ...

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
unit cyBaseButton;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, StdCtrls, Messages, Graphics, Controls, {$IFDEF DELPHI2009_OR_ABOVE} pngimage, {$ENDIF}
       Buttons, Dialogs, VCL.cyTypes, VCL.cyGraphics;

{ *** Glyph and NumGlyph properies handling ***
  Glyph 1 : normal state
  Glyph 2 : disabled state
  Glyph 3 : Mouse down state
  Glyph 4 : Exclusive state }

type
  TcyBaseButton = class(TBitBtn)
  private
    FCaptionOrientation: TCaptionOrientation;
    FOnPaint: TNotifyEvent;
    FPicGlyph: TPicture;
    procedure SetCaptionOrientation(const Value: TCaptionOrientation);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    // Handle my own MouseOver variable :
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetPicGlyph(const Value: TPicture);
    procedure GlyphChanged(Sender: TObject);
  protected
    // FDesignDraw and FDesignState allows to view any button state rendering at Design time :
    FCanvas: TCanvas;
    FDesignDraw: boolean;
    FDesignState: TButtonState;
    FMouseOver: Boolean;
    FMouseLeftDown: Boolean;
    procedure CalcLayout(Rect: TRect; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
    procedure DrawButton(aState: TButtonState; Hot: Boolean); virtual;
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); virtual;
    procedure DrawBorders(var Rect: TRect; LeftTopColor, RightBottomColor: TColor);
    procedure DrawCaption(aRect: TRect; aState: TButtonState; Hot: Boolean; GlyphExists: Boolean);
    procedure DrawGlyph(X, Y: Integer; aState: TButtonState);
    procedure DrawInnerBorders(var Rect: TRect; TopColor, BottomColor: TColor);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // Public properties :
    property MouseOver: Boolean read FMouseOver;
    property MouseLeftDown: Boolean read FMouseLeftDown;
    // Published properties :
    property CaptionOrientation: TCaptionOrientation read FCaptionOrientation write SetCaptionOrientation default coHorizontal;
    property GlyphX: TPicture read FPicGlyph write SetPicGlyph;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  published
  end;

const
  FFramePercent = -20;

implementation

{TcyBaseButton}
constructor TcyBaseButton.Create(AOwner: TComponent);
begin
  inherited;
  FDesignDraw := false;
  FCaptionOrientation := coHorizontal;
  FMouseOver := false;
  FMouseLeftDown := false;
  FCanvas := TCanvas.Create;
  FPicGlyph := TPicture.Create;
  Glyph.OnChange := GlyphChanged;
end;

destructor TcyBaseButton.Destroy;
begin
  FCanvas.Free;
  FPicGlyph.Free;
  inherited;
end;

procedure TcyBaseButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not FMouseLeftDown) and Enabled
  then Invalidate;
  FMouseLeftDown := true;
  inherited;
end;

procedure TcyBaseButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FMouseLeftDown) and Enabled
  then Invalidate;
  FMouseLeftDown := false;
  inherited;
end;

procedure TcyBaseButton.CMMouseEnter(var Message: TMessage);
begin
  if (not FMouseOver) and Enabled
  then Invalidate;
  FMouseOver := true;
  Inherited;
end;

procedure TcyBaseButton.CMMouseLeave(var Message: TMessage);
begin
  if (FMouseOver) and Enabled
  then Invalidate;
  FMouseOver := false;
  Inherited;
end;

procedure TcyBaseButton.SetCaptionOrientation(const Value: TCaptionOrientation);
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

procedure TcyBaseButton.SetPicGlyph(const Value: TPicture);
begin
  FPicGlyph.Assign(Value);
  Invalidate;
end;

procedure TcyBaseButton.CNDrawItem(var Message: TWMDrawItem);
var
  DrawItemStruct: TDrawItemStruct;
  State: TButtonState;
  IsDown: Boolean;
begin
  DrawItemStruct := Message.DrawItemStruct{$IFNDEF CLR}^{$ENDIF};

  // Adjust FState:
  if FDesignDraw
  then begin
    FDesignDraw := false;
    State := FDesignState;
  end
  else begin
    IsDown := DrawItemStruct.itemState and ODS_SELECTED <> 0;

    if Enabled
    then begin
      if IsDown
      then State := bsDown
      else State := bsUp;
    end
    else
      State := bsDisabled;
  end;

  FCanvas.Handle := DrawItemStruct.hDC;
  DrawButton(State, MouseOver);
  if Assigned(FOnPaint)
  then FOnPaint(Self);
  FCanvas.Handle := 0;
end;

procedure TcyBaseButton.DrawButton(aState: TButtonState; Hot: Boolean);
begin
  //
end;

procedure TcyBaseButton.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
begin
  //
end;

procedure TcyBaseButton.DrawGlyph(X, Y: Integer; aState: TButtonState);
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
      // aBmp.TransparentColor := CorTransparente;
      aBmp.Transparent := True;
      FCanvas.Draw(X, Y, aBmp);
    finally
      aBmp.Free;
    end;
end;

procedure TcyBaseButton.CalcLayout(Rect: TRect; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
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
          if TextSize <> 0 then
            Inc(SizeNeed, Spacing + TextSize);
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

procedure TcyBaseButton.DrawCaption(aRect: TRect; aState: TButtonState; Hot: Boolean; GlyphExists: Boolean);
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

  DrawStyle := VCL.cyGraphics.DrawTextFormatFlags(0, aAlignment, aTextLayout, WordWrap);
  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);

  if FCaptionOrientation = coHorizontal
  then begin
    cyDrawText(FCanvas.Handle, Caption, aRect, DrawStyle);
  end
  else begin
    TmpFont := cyCreateFontIndirect(Font, FCaptionOrientation);
    try
      FCanvas.Font.Assign(TmpFont);
      cyDrawVerticalText(FCanvas, Caption, aRect, DrawStyle, FCaptionOrientation, aAlignment, aTextLayout);
    finally
      TmpFont.Free;
    end;
  end;
end;

procedure TcyBaseButton.DrawBorders(var Rect: TRect; LeftTopColor, RightBottomColor: TColor);
begin
  with FCanvas do
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

    InflateRect(Rect, -1, -1);
  end;
end;

procedure TcyBaseButton.DrawInnerBorders(var Rect: TRect; TopColor, BottomColor: TColor);
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
  FCanvas.Pen.Color := CenterColor;
  FCanvas.MoveTo(Rect.Left, Y46);
  FCanvas.LineTo(Rect.Left, Y26);
  InterMedColor := MediumColor(TopColor, CenterColor);
  FCanvas.Pen.Color := InterMedColor;
  FCanvas.LineTo(Rect.Left, Y16);
  FCanvas.Pen.Color := TopColor;
  FCanvas.LineTo(Rect.Left, Rect.Top);
  FCanvas.LineTo(Rect.Right-1, Rect.Top);
  FCanvas.LineTo(Rect.Right-1, Y16);
  FCanvas.Pen.Color := InterMedColor;
  FCanvas.LineTo(Rect.Right-1, Y26);
  FCanvas.Pen.Color := CenterColor;
  FCanvas.LineTo(Rect.Right-1, Y46);
  // RightCenter to RightBottom to LeftBottom to LeftCenter:
  InterMedColor := MediumColor(BottomColor, CenterColor);
  FCanvas.Pen.Color := InterMedColor;
  FCanvas.LineTo(Rect.Right-1, Y56);
  FCanvas.Pen.Color := BottomColor;
  FCanvas.LineTo(Rect.Right-1, Rect.Bottom-1);
  FCanvas.LineTo(Rect.Left, Rect.Bottom-1);
  FCanvas.LineTo(Rect.Left, Y56);
  FCanvas.Pen.Color := InterMedColor;
  FCanvas.LineTo(Rect.Left, Y46);
  InflateRect(Rect, -1, -1);
end;

procedure TcyBaseButton.GlyphChanged(Sender: TObject);
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
