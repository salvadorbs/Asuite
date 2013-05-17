{   Component(s):
    tcyBaseStaticText

    Description:
    Component base for StaticText ...

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
unit cyBaseStaticText;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Themes, Forms, StdCtrls, Messages, Graphics, Controls, {$IFDEF DELPHI2009_OR_ABOVE} pngimage, {$ENDIF}
       Buttons, Dialogs, VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics;

{ *** Glyph and NumGlyph properies handling ***
  Glyph 1 : normal state
  Glyph 2 : disabled state
  Glyph 3 : Mouse down state
  Glyph 4 : Exclusive state }

type
  TStaticTextLayout = (slGlyphLeft, slGlyphRight, slGlyphTop, slGlyphBottom);

  TcyBaseStaticText = class(TWinControl)
  private
    FCaptionOrientation: TCaptionOrientation;
    FOnPaint: TNotifyEvent;
    FPicGlyph: TPicture;
    FNumGlyphs: Integer;
    FWordWrap: Boolean;
    FSpacing: Integer;
    FLayout: TStaticTextLayout;
    FMargin: Integer;
    FCaptionRender: TCaptionRender;
    FBevels: TcyBevels;
    FCaptionLayout: TTextLayout;
    FShowAccelChar: Boolean;
    FFocusControl: TWinControl;
    FCaptionAlignment: TAlignment;
    FGlyphAlignment: TGlyphAlignment;
    FGlyphLayout: TGlyphLayout;
    procedure BevelsChange(Sender: TObject);
    procedure SetCaptionOrientation(const Value: TCaptionOrientation);
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    // Handle my own MouseOver variable :
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetPicGlyph(const Value: TPicture);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetSpacing(const Value: Integer);
    procedure SetLayout(const Value: TStaticTextLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetCaptionRender(const Value: TCaptionRender);
    procedure SetBevels(const Value: TcyBevels);
    procedure SetCaptionLayout(const Value: TTextLayout);
    procedure SetShowAccelChar(const Value: Boolean);
    procedure SetFocusControl(const Value: TWinControl);
    function GetTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetGlyphAlignment(const Value: TGlyphAlignment);
    procedure SetGlyphLayout(const Value: TGlyphLayout);
  protected
    // FDesignDraw and FDesignState allows to view any component state rendering at Design time :
    FCanvas: TCanvas;
    FDesignDraw: boolean;
    FDesignState: TButtonState;
    FMouseOver: Boolean;
    FMouseLeftDown: Boolean;
    procedure CalcLayout(Rect: TRect; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
    procedure DrawStaticText(aState: TButtonState; Hot: Boolean); virtual;
    procedure DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean); virtual;
    procedure DrawCaption(aRect: TRect; aState: TButtonState; Hot: Boolean; GlyphExists: Boolean); virtual;
    procedure DrawGlyph(X, Y: Integer; aState: TButtonState); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Public properties :
    property MouseOver: Boolean read FMouseOver;
    property MouseLeftDown: Boolean read FMouseLeftDown;
    // Published properties :
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property Bevels: TcyBevels read FBevels write SetBevels;
    property CaptionOrientation: TCaptionOrientation read FCaptionOrientation write SetCaptionOrientation default coHorizontal;
    property CaptionRender: TCaptionRender read FCaptionRender write SetCaptionRender default crNormal;
    property CaptionLayout: TTextLayout read FCaptionLayout write SetCaptionLayout default tlTop;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property GlyphAlignment: TGlyphAlignment read FGlyphAlignment write SetGlyphAlignment default gaLeft;
    property GlyphLayout: TGlyphLayout read FGlyphLayout write SetGlyphLayout default glTop;
    property GlyphX: TPicture read FPicGlyph write SetPicGlyph;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property Layout: TStaticTextLayout read FLayout write SetLayout default slGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default 0;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default true;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
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

{TcyBaseStaticText}
constructor TcyBaseStaticText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csReplicatable, csDoubleClicks];

  FCaptionAlignment := taLeftJustify;
  FDesignDraw := false;
  FCaptionOrientation := coHorizontal;
  FCaptionRender := crNormal;
  FCaptionLayout := tlTop;
  FMouseOver := false;
  FMouseLeftDown := false;
  FLayout := slGlyphLeft;
  FNumGlyphs := 1;
  FGlyphAlignment := gaLeft;
  FGlyphLayout := glTop;

  FShowAccelChar := true;
  FMargin := 0;
  FSpacing := 4;
  FWordWrap := false;
  FCanvas := TCanvas.Create;
  FBevels := TcyBevels.Create(self, TcyBevel);
  FBevels.OnChange := BevelsChange;
  FPicGlyph := TPicture.Create;
end;

procedure TcyBaseStaticText.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  with Params do
    Style := Style or SS_NOTIFY or SS_OWNERDRAW;
end;

destructor TcyBaseStaticText.Destroy;
begin
  FCanvas.Free;
  FPicGlyph.Free;
  FBevels.Free;
  FBevels := Nil;
  inherited;
end;

procedure TcyBaseStaticText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseLeftDown := true;
  Invalidate;
  inherited;
end;

procedure TcyBaseStaticText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseLeftDown := false;
  Invalidate;
  inherited;
end;

procedure TcyBaseStaticText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TcyBaseStaticText.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure TcyBaseStaticText.CMEnabledChanged(var Message: TMessage);
begin
  Inherited;
  Invalidate;
end;

procedure TcyBaseStaticText.CMMouseEnter(var Message: TMessage);
begin
  FMouseOver := true;
  Inherited;
end;

procedure TcyBaseStaticText.CMMouseLeave(var Message: TMessage);
begin
  FMouseOver := false;
  Inherited;
end;

procedure TcyBaseStaticText.SetCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment = Value then Exit;

  FCaptionAlignment := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

procedure TcyBaseStaticText.SetCaptionLayout(const Value: TTextLayout);
begin
  if FCaptionLayout = Value then Exit;

  FCaptionLayout := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetCaptionOrientation(const Value: TCaptionOrientation);
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

procedure TcyBaseStaticText.SetCaptionRender(const Value: TCaptionRender);
begin
  if FCaptionRender = Value then Exit;

  FCaptionRender := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetFocusControl(const Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TcyBaseStaticText.SetGlyphAlignment(const Value: TGlyphAlignment);
begin
  if FGlyphAlignment = Value then Exit;

  FGlyphAlignment := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetGlyphLayout(const Value: TGlyphLayout);
begin
  if FGlyphLayout = Value then Exit;

  FGlyphLayout := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetLayout(const Value: TStaticTextLayout);
begin
  if FLayout = Value then Exit;

  FLayout := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetMargin(const Value: Integer);
begin
  if FMargin = Value then Exit;

  FMargin := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetNumGlyphs(const Value: Integer);
begin
  if FNumGlyphs = Value then Exit;

  FNumGlyphs := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap = Value then Exit;

  FWordWrap := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.BevelsChange(Sender: TObject);
begin
  if FBevels.NeedOwnerRealign then
    Realign;

  Invalidate;
end;

procedure TcyBaseStaticText.SetPicGlyph(const Value: TPicture);
begin
  FPicGlyph.Assign(Value);
  Invalidate;
end;

procedure TcyBaseStaticText.SetShowAccelChar(const Value: Boolean);
begin
  if FShowAccelChar = Value then Exit;

  FShowAccelChar := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.SetSpacing(const Value: Integer);
begin
  if FSpacing = Value then Exit;

  FSpacing := Value;
  Invalidate;
end;

procedure TcyBaseStaticText.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  with ThemeServices do
    if ThemesEnabled and Transparent then
    begin
      SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
      DrawParentBackground(Handle, Message.ChildDC, nil, False);
      { Return an empty brush to prevent Windows from overpainting what we just have created. }
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
end;

function TcyBaseStaticText.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TcyBaseStaticText.SetTransparent(const Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value
    then ControlStyle := ControlStyle - [csOpaque]
    else ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TcyBaseStaticText.CNDrawItem(var Message: TWMDrawItem);
var
  DrawItemStruct: TDrawItemStruct;
  State: TButtonState;
begin
  DrawItemStruct := Message.DrawItemStruct{$IFNDEF CLR}^{$ENDIF};

  // Adjust FState:
  if FDesignDraw then
  begin
    FDesignDraw := false;
    State := FDesignState;
  end
  else
    if Enabled then
    begin
      if FMouseLeftDown
      then State := bsDown
      else State := bsUp;
    end
    else
      State := bsDisabled;

  FCanvas.Handle := DrawItemStruct.hDC;
  DrawStaticText(State, MouseOver);
  if Assigned(FOnPaint)
  then FOnPaint(Self);
  FCanvas.Handle := 0;
end;

procedure TcyBaseStaticText.DrawStaticText(aState: TButtonState; Hot: Boolean);
begin
  //
end;


procedure TcyBaseStaticText.DrawBackground(var Rect: TRect; aState: TButtonState; Hot: Boolean);
begin
  //
end;

procedure TcyBaseStaticText.DrawGlyph(X, Y: Integer; aState: TButtonState);
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

procedure TcyBaseStaticText.DrawCaption(aRect: TRect; aState: TButtonState; Hot: Boolean; GlyphExists: Boolean);
var
  DrawStyle: LongInt;
  TmpFont: TFont;
begin
  if Caption = '' then EXIT;

  DrawStyle := VCL.cyGraphics.DrawTextFormatFlags(0, FCaptionAlignment, FCaptionLayout, FWordWrap, FCaptionRender);
  if not FShowAccelChar then DrawStyle := DrawStyle or DT_NOPREFIX;
  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);

  if FCaptionOrientation = coHorizontal
  then begin
    cyDrawText(FCanvas.Handle, Caption, aRect, DrawStyle);
  end
  else begin
    TmpFont := cyCreateFontIndirect(Font, FCaptionOrientation);
    try
      FCanvas.Font.Assign(TmpFont);
      cyDrawVerticalText(FCanvas, Caption, aRect, DrawStyle, FCaptionOrientation, FCaptionAlignment, FCaptionLayout);
    finally
      TmpFont.Free;
    end;
  end;
end;

procedure TcyBaseStaticText.CalcLayout(Rect: TRect; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
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
        if WordWrapText and (Layout in [slGlyphLeft, slGlyphRight]) then
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
          if Layout in [slGlyphLeft, slGlyphRight]
          then Result := CalcRect.Right - CalcRect.Left
          else Result := CalcRect.Bottom - CalcRect.Top;
        end
        else begin
          // Vertical Text :
          if Layout in [slGlyphLeft, slGlyphRight]
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
      case FLayout of
        slGlyphLeft:
          Inc(TextRect.Left, Margin);

        slGlyphRight:
          Dec(TextRect.Right, Margin);

        slGlyphTop:
          Inc(TextRect.Top, Margin);

        slGlyphBottom:
          Dec(TextRect.Bottom, Margin);
      end;
    Exit;
  end;

  // Calc Graphic position :
  if Margin <> -1
  then begin
    // TextRect will be set after define the position of Graphic :
    case FLayout of
      slGlyphLeft, slGlyphRight:
        begin
          // Vertical position (not affected by Margin) :
          case FGlyphLayout of
            glTop:
              GraphicY := Rect.Top;

            glCenter:
              GraphicY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - GraphicHeight div 2;

            glBottom:
              GraphicY := Rect.Bottom - GraphicHeight;
          end;

          if FLayout = slGlyphLeft then
          begin
            GraphicX := Rect.Left;
            Inc(GraphicX, Margin);
          end
          else begin
            GraphicX := Rect.Right - GraphicWidth;
            Dec(GraphicX, Margin);
          end;
        end;

      slGlyphTop, slGlyphBottom:
        begin
          // Horizontal position (not affected by Margin) :
          case FGlyphAlignment of
            gaLeft:   GraphicX := Rect.Left;
            gaCenter: GraphicX := Rect.Left + (Rect.Right-Rect.Left) div 2 - GraphicWidth div 2;
            gaRight:  GraphicX := Rect.Right - GraphicWidth;
          end;

          if Layout = slGlyphTop
          then begin
            GraphicY := Rect.Top;
            Inc(GraphicY, Margin);
          end
          else begin
            GraphicY := Rect.Bottom - GraphicHeight;
            Dec(GraphicY, Margin);
          end;
        end;
    end;
  end
  else begin
    // Margin = -1, so center Text and Graphic on Rect parameter : Glyph + Spacing + Text
    TextSize := CalcTextSize;

    case FLayout of
      slGlyphLeft, slGlyphRight:
        begin
          SizeNeed := GraphicWidth;
          if TextSize <> 0 then Inc(SizeNeed, Spacing + TextSize);

          // Vertical position :
          case FGlyphLayout of
            glTop:
              GraphicY := Rect.Top;

            glCenter:
              GraphicY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - GraphicHeight div 2;

            glBottom:
              GraphicY := Rect.Bottom - GraphicHeight;
          end;

          if Layout = slGlyphLeft
          then GraphicX := Rect.Left + (Rect.Right - Rect.Left) div 2 - SizeNeed div 2
          else GraphicX := Rect.Left + (Rect.Right - Rect.Left) div 2 + SizeNeed div 2 - GraphicWidth;
        end;

      slGlyphTop, slGlyphBottom:
        begin
          SizeNeed := GraphicHeight;
          if TextSize <> 0 then Inc(SizeNeed, Spacing + TextSize);

          case FGlyphAlignment of
            gaLeft:   GraphicX := Rect.Left;
            gaCenter: GraphicX := Rect.Left + (Rect.Right-Rect.Left) div 2 - GraphicWidth div 2;
            gaRight:  GraphicX := Rect.Right - GraphicWidth;
          end;

          if FLayout = slGlyphTop
          then GraphicY := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - SizeNeed div 2
          else GraphicY := Rect.Top + (Rect.Bottom - Rect.Top) div 2 + SizeNeed div 2 - GraphicHeight;
        end;
    end;
  end;

  // Define TextRect :
  case FLayout of
    slGlyphLeft:
      TextRect := classes.Rect(GraphicX + GraphicWidth + Spacing, Rect.Top, Rect.Right, Rect.Bottom);

    slGlyphRight:
      TextRect := classes.Rect(Rect.Left, Rect.Top, GraphicX - Spacing, Rect.Bottom);

    slGlyphTop:
      TextRect := classes.Rect(Rect.Left, GraphicY + GraphicHeight + Spacing, Rect.Right, Rect.Bottom);

    slGlyphBottom:
      TextRect := classes.Rect(Rect.Left, Rect.Top, Rect.Right, GraphicY - Spacing);
  end;
end;

end.
