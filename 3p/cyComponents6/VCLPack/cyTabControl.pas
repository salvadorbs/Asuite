{   Component(s):
    tcyTabControl

    Description:
    A TabControl with color feature.

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
unit cyTabControl;

interface

uses cyBaseTabControl, Classes, Windows, Buttons, StdCtrls, Controls, CommCtrl, ComCtrls, Graphics, Messages,
      VCL.cyClasses, VCL.cyGraphics;

type
  TcyTabControl = class(TTabControl)
  private
    TmpColors: TcyGradient;
    FThinBorder: Boolean;
    FStyle: TcyTabStyle;
    FActiveTabColors: TcyGradient;
    FInactiveTabColors: TcyGradient;
    FActiveTabFont: TFont;
    FInactiveTabFont: TFont;
    FOnAdvancedDrawTabBackground: TProcAdvancedDrawTabBackground;
    FOnAdvancedDrawTab: TProcAdvancedDrawTab;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FWordWrap: Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
//    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
//    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure SetThinBorder(const Value: Boolean);
    procedure SetStyle(const Value: TcyTabStyle);
    procedure SetActiveTabColors(const Value: TcyGradient);
    procedure SetInactiveTabColors(const Value: TcyGradient);
    procedure SetActiveTabFont(const Value: TFont);
    procedure SetInactiveTabFont(const Value: TFont);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    // Only called if OwnerDraw is True, purpous is to call DrawTab():
    // procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure VisualChanged(Sender: TObject);
    procedure DoDrawTabStyleBackground(OnCanvas: TCanvas; const Tab: Integer; const TabStyle: TcyTabStyle; const TabPosition: TTabPosition; const aRect: TRect; const Colors: TcyGradient; const Flags: TcyTabParameters);
    procedure DoDrawTabStyleCaption(OnCanvas: TCanvas; const Tab: Integer; aCaption: String; const aRect: TRect; const aFont: TFont);
    procedure cyDrawTab(Tab: Integer; aRect: TRect);
    procedure CalcLayout(Rect: TRect; Caption: String; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabBoundsRect(Tab: Integer): TRect;
  published
    property ThinBorder: Boolean read FThinBorder write SetThinBorder default true;
    property Style: TcyTabStyle read FStyle write SetStyle default tsTab;
    property ActiveTabColors: TcyGradient read FActiveTabColors write SetActiveTabColors;
    property InactiveTabColors: TcyGradient read FInactiveTabColors write SetInactiveTabColors;
    property ActiveTabFont: TFont read FActiveTabFont write SetActiveTabFont;
    property InactiveTabFont: TFont read FInactiveTabFont write SetInactiveTabFont;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
    property OnAdvancedDrawTabBackground: TProcAdvancedDrawTabBackground read FOnAdvancedDrawTabBackground write FOnAdvancedDrawTabBackground;
    property OnAdvancedDrawTab: TProcAdvancedDrawTab read FOnAdvancedDrawTab write FOnAdvancedDrawTab;
  end;

implementation

{ TcyTabControl }
constructor TcyTabControl.Create(AOwner: TComponent);
begin
  inherited;
  TmpColors := TcyGradient.Create(self);

  FThinBorder := true;
  FStyle := tsTab;
  FActiveTabColors := TcyGradient.Create(self);
  FActiveTabColors.OnChange := VisualChanged;
  FInactiveTabColors := TcyGradient.Create(self);
  FInactiveTabColors.OnChange := VisualChanged;
  FActiveTabFont := TFont.Create;
  FActiveTabFont.OnChange := VisualChanged;
  FInactiveTabFont := TFont.Create;
  FInactiveTabFont.OnChange := VisualChanged;
  FMargin := -1;
  FSpacing := 4;
  FLayout := blGlyphLeft;
  FWordWrap := false;
end;

destructor TcyTabControl.Destroy;
begin
  FActiveTabColors.Free;
  FInactiveTabColors.Free;
  FActiveTabFont.Free;
  FInactiveTabFont.Free;
  inherited;
end;

procedure TcyTabControl.WndProc(var Msg: TMessage);
begin
  inherited WndProc(Msg);

  if Msg.Msg = TCM_ADJUSTRECT then
    with PRect(Msg.LParam)^ do
      if ThinBorder then
        case TabPosition of
          tpTop:
          begin
            Left := 1;
            Right := Width-3;
            Bottom := Height-2;

            if TabHeight = 0
            then Top := top - 3
            else Top := top - 3; // Top := TabHeight + 1;

            if Inherited Style = tsButtons then
              Dec(Top, 3);
          end;

          tpBottom:
          begin
            Left := 1;
            Right := Width-3;
            Top := 1
          end;

          tpLeft, tpRight:
          begin
            dec(Left, 2);
            inc(Right, 2);
            dec(Top, 2);
            inc(Bottom, 2);
          end;
        end;
end;

// Windows Paint rules : active tab frame are painted by inactive tab on each side (unless there' s no one)
// My approach is to paint with exact same height to avoid complex paints
function TcyTabControl.GetTabBoundsRect(Tab: Integer): TRect;
begin
  Result := TabRect(Tab);

  // Correct TabIndex Rect:
  case Inherited Style of
    tsTabs:         // *** Tabs ****
    begin
      case TabPosition of
        tpTop:
        begin
          if Tab = TabIndex then
          begin
            dec(Result.Top, 2);
            inc(Result.Bottom);

            if Tab = 0 then dec(Result.Left, 2);
            if Tab = Tabs.Count-1 then inc(Result.Right, 2);
          end
          else begin
//            dec(Result.Top, 2);
            inc(Result.Bottom);
          end;
        end;

        tpBottom:
        begin
          if Tab = TabIndex then
          begin
            dec(Result.Top, 2);
            inc(Result.Bottom, 2);
            if Tab = 0 then dec(Result.Left, 2);
            if Tab = Tabs.Count-1 then inc(Result.Right, 2);
          end
          else begin
//            dec(Result.Top, 2);
            inc(Result.Bottom, 2);
          end;
        end;

        tpLeft:
        begin
          if Tab = TabIndex then
          begin

          end
          else begin

          end;
        end;

        tpRight:
        begin

        end;
      end;
    end;

    tsButtons:      // *** Buttons (TabPosition = tpTop) ***
    begin
//      Dec(Result.Left);
//      Inc(Result.Right);
    end;

    tsFlatButtons:  // *** Flat buttons (TabPosition = tpTop) *** //
    begin
      if Tab = TabIndex then
      begin
        Dec(Result.Left);
        Inc(Result.Right);
        Result.Bottom := Result.Bottom + 3;
      end
      else begin
        Dec(Result.Left);
        Inc(Result.Right);
        Result.Bottom := Result.Bottom + 3;
      end;
    end;
  end;
end;

procedure TcyTabControl.VisualChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyTabControl.SetActiveTabColors(const Value: TcyGradient);
begin
  FActiveTabColors.Assign(Value);
end;

procedure TcyTabControl.SetActiveTabFont(const Value: TFont);
begin
  FActiveTabFont.Assign(Value);
end;

procedure TcyTabControl.SetInactiveTabColors(const Value: TcyGradient);
begin
  FInactiveTabColors.Assign(Value);
end;

procedure TcyTabControl.SetInactiveTabFont(const Value: TFont);
begin
  FInactiveTabFont.Assign(Value);
end;

procedure TcyTabControl.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
  Invalidate;
end;

procedure TcyTabControl.SetMargin(const Value: Integer);
begin
  FMargin := Value;
  Invalidate;
end;

procedure TcyTabControl.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  Invalidate;
end;

procedure TcyTabControl.SetStyle(const Value: TcyTabStyle);
begin
  if FStyle = Value then Exit;

  case Value of
    tsTab:
    begin
      Inherited Style := tsTabs;
    end;

    tsButton, tsFlatButton:
    begin
      TabPosition := tpTop;
      Inherited Style := tsButtons;
    end;
  end;

  FStyle := Value;
  Invalidate;
end;

procedure TcyTabControl.SetThinBorder(const Value: Boolean);
begin
  if FThinBorder = Value then Exit;

  FThinBorder := Value;
  RecreateWnd;
end;

procedure TcyTabControl.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

procedure TcyTabControl.DoDrawTabStyleBackground(OnCanvas: TCanvas; const Tab: Integer; const TabStyle: TcyTabStyle; const TabPosition: TTabPosition; const aRect: TRect; const Colors: TcyGradient; const Flags: TcyTabParameters);
var
  TmpTabStyle: TcyTabStyle;
  Draw: Boolean;
begin
  if Assigned(FOnAdvancedDrawTabBackground) then
  begin
    Draw := true;
    TmpColors.Assign(Colors);
    TmpTabStyle := TabStyle;
    FOnAdvancedDrawTabBackground(Self, Tab, TmpTabStyle, aRect, TmpColors, Draw);

    if Draw then
      DrawTabStyleBackground(OnCanvas, TmpTabStyle, TabPosition, aRect, TmpColors, Flags);
  end
  else
    DrawTabStyleBackground(OnCanvas, TabStyle, TabPosition, aRect, Colors, Flags);
end;

procedure TcyTabControl.DoDrawTabStyleCaption(OnCanvas: TCanvas; const Tab: Integer; aCaption: String; const aRect: TRect; const aFont: TFont);
var Draw: Boolean;
begin
  OnCanvas.Font.Assign(aFont);

  if Assigned(FOnAdvancedDrawTab) then
  begin
    Draw := true;
    FOnAdvancedDrawTab(Self, Tab, aRect, Draw);

    if Draw then
      DrawTabStyleCaption(Canvas, aCaption, TabPosition, aRect);
  end
  else
    DrawTabStyleCaption(Canvas, aCaption, TabPosition, aRect);
end;

procedure TcyTabControl.cyDrawTab(Tab: Integer; aRect: TRect);
var
  TextRect: TRect;
  Parameters: TcyTabParameters;
  aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY: Integer;
begin
  Parameters := [];

  if Tab = TabIndex then
    include(Parameters, tpActive);

  if (Tab-1 = TabIndex) or ((Tab = TabIndex) and (Tab = Tabs.Count-1)) then
    include(Parameters, tpDrawActiveRightFrame);

  if (Tab+1 = TabIndex) or ((Tab = TabIndex) and (Tab = 0)) then
    include(Parameters, tpDrawActiveLeftFrame);

  if Tab = TabIndex
  then DoDrawTabStyleBackground(Canvas, Tab, FStyle, TabPosition, aRect, FActiveTabColors, Parameters)
  else DoDrawTabStyleBackground(Canvas, Tab, FStyle, TabPosition, aRect, FInactiveTabColors, Parameters);

  if Assigned(Images) then
  begin
    aGraphicWidth := Images.Width;
    aGraphicHeight := Images.Height;
    CalcLayout(aRect, tabs[Tab], aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
    Images.Draw(Canvas, aGraphicX, aGraphicY, Tab);
  end
  else
    TextRect := aRect;

  if Tab = TabIndex
  then DoDrawTabStyleCaption(Canvas, Tab, tabs[Tab], TextRect, FActiveTabFont)
  else DoDrawTabStyleCaption(Canvas, Tab, tabs[Tab], TextRect, FInactiveTabFont);
end;

procedure TcyTabControl.CalcLayout(Rect: TRect; Caption: String; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
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
        WordWrapText := WordWrap and (TabPosition in [tpTop, tpBottom]);

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
        if TabPosition in [tpTop, tpBottom]
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

procedure TcyTabControl.WMPaint(var Message: TWMPaint);
var
  t: Integer;
  TabRect: TRect;
begin
  inherited;

  for t := Tabs.Count-1 downto 0 do
  begin
    TabRect := GetTabBoundsRect(t);

    if TabRect.Right > 0 then
      cyDrawTab(t, TabRect)
  end;
end;

end.
