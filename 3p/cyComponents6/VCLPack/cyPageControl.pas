{   Component(s):
    tcyPageControl

    Description:
    A PageControl with color feature.

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

unit cyPageControl;

interface

uses cyBaseTabControl, Classes, Windows, Buttons, StdCtrls, Controls, commctrl, ComCtrls, Graphics, Messages,
      VCL.cyClasses, VCL.cyGraphics;

type
{  TTabSheet = class(ComCtrls.TTabSheet)
  private
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    constructor Create(aOwner: TComponent); override;
  end; }

  TcyPageControl = class(TPageControl)
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
    procedure SetThinBorder(const Value: Boolean);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetStyle(const Value: TcyTabStyle);
    procedure SetActiveTabColors(const Value: TcyGradient);
    procedure SetInactiveTabColors(const Value: TcyGradient);
    procedure SetActiveTabFont(const Value: TFont);
    procedure SetInactiveTabFont(const Value: TFont);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure VisualChanged(Sender: TObject);
    procedure DoDrawTabStyleBackground(OnCanvas: TCanvas; const Tab: Integer; const TabStyle: TcyTabStyle; const TabPosition: TTabPosition; const aRect: TRect; const Colors: TcyGradient; const Flags: TcyTabParameters);
    procedure DoDrawTabStyleCaption(OnCanvas: TCanvas; const Tab: Integer; aCaption: String; const aRect: TRect; const aFont: TFont);
    procedure cyDrawPage(aPage: Integer; aRect: TRect);
    procedure CalcLayout(Rect: TRect; Caption: String; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowTabs;
    procedure HideTabs;
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

{ constructor TTabSheet.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure TTabSheet.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
begin
  Brush.Color := clLime;
  Windows.FillRect(Msg.dc, ClientRect, Brush.Handle);
  Msg.Result := 1;
end;             }

{ TcyPageControl }
constructor TcyPageControl.Create(AOwner: TComponent);
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

destructor TcyPageControl.Destroy;
begin
  FActiveTabColors.Free;
  FInactiveTabColors.Free;
  FActiveTabFont.Free;
  FInactiveTabFont.Free;
  inherited;
end;

procedure TcyPageControl.WndProc(var Msg: TMessage);
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
            else Top := top - 3; // TabHeight + 1;

            if Inherited Style = tsButtons then
              Dec(Top, 3);
          end;

          tpBottom:
          begin
            Left := 1;
            Right := Width-3;
            Top := 1;
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

procedure TcyPageControl.ShowTabs;
var
  i: Integer;
  sActive: TTabSheet;
begin
  sActive := nil;

  for i:= 0 to PageCount-1 do
  begin
    if Pages[i].Visible then
      sActive := Pages[i];
    Pages[i].TabVisible := true;
  end;


  if sActive <> nil then
    ActivePage := sActive;
end;

procedure TcyPageControl.HideTabs;
var s, i: Integer;
begin
  s := ActivePageIndex;

  for i:= 0 to PageCount-1 do
    Pages[i].TabVisible := false;


  if s <> -1 then
    Pages[s].Visible := true;
end;

function TcyPageControl.GetTabBoundsRect(Tab: Integer): TRect;
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
//            dec(Result.Top);
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
//      if Tab <> 0 then
//        Dec(Result.Left);
//      Inc(Result.Right);
    end;

    tsFlatButtons:  // *** Flat buttons (TabPosition = tpTop) *** //
    begin
      if Tab = TabIndex then
      begin
//        Inc(Result.Right);
        Result.Bottom := Result.Bottom + 3;
      end
      else begin
//        Inc(Result.Right);
        Result.Bottom := Result.Bottom + 3;
      end;
    end;
  end;
end;

procedure TcyPageControl.VisualChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyPageControl.SetActiveTabColors(const Value: TcyGradient);
begin
  FActiveTabColors.Assign(Value);
end;

procedure TcyPageControl.SetActiveTabFont(const Value: TFont);
begin
  FActiveTabFont.Assign(Value);
end;

procedure TcyPageControl.SetInactiveTabColors(const Value: TcyGradient);
begin
  FInactiveTabColors.Assign(Value);
end;

procedure TcyPageControl.SetInactiveTabFont(const Value: TFont);
begin
  FInactiveTabFont.Assign(Value);
end;

procedure TcyPageControl.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
  Invalidate;
end;

procedure TcyPageControl.SetMargin(const Value: Integer);
begin
  FMargin := Value;
  Invalidate;
end;

procedure TcyPageControl.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  Invalidate;
end;

procedure TcyPageControl.SetStyle(const Value: TcyTabStyle);
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

procedure TcyPageControl.SetThinBorder(const Value: Boolean);
begin
  if FThinBorder = Value then Exit;

  FThinBorder := Value;
  RecreateWnd;
end;

procedure TcyPageControl.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

procedure TcyPageControl.DoDrawTabStyleBackground(OnCanvas: TCanvas; const Tab: Integer; const TabStyle: TcyTabStyle; const TabPosition: TTabPosition; const aRect: TRect; const Colors: TcyGradient; const Flags: TcyTabParameters);
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

procedure TcyPageControl.DoDrawTabStyleCaption(OnCanvas: TCanvas; const Tab: Integer; aCaption: String; const aRect: TRect; const aFont: TFont);
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

procedure TcyPageControl.cyDrawPage(aPage: Integer; aRect: TRect);
var
  Tab: Integer;
  TextRect: TRect;
  Parameters: TcyTabParameters;
  aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY: Integer;
begin
  Parameters := [];
  Tab := Pages[aPage].TabIndex;

  if Tab = TabIndex then
    include(Parameters, tpActive);

  if (Tab-1 = TabIndex) or ((Tab = TabIndex) and (Tab = Tabs.Count-1)) then
    include(Parameters, tpDrawActiveRightFrame);

  if (Tab+1 = TabIndex) or ((Tab = TabIndex) and (Tab = 0)) then
    include(Parameters, tpDrawActiveLeftFrame);

  if Tab = TabIndex
  then DoDrawTabStyleBackground(Canvas, Tab, FStyle, TabPosition, aRect, FActiveTabColors, Parameters)
  else DoDrawTabStyleBackground(Canvas, Tab, FStyle, TabPosition, aRect, FInactiveTabColors, Parameters);

  if (Assigned(Images)) and (Pages[Tab].ImageIndex <> -1) then
  begin
    aGraphicWidth := Images.Width;
    aGraphicHeight := Images.Height;
    CalcLayout(aRect, Pages[aPage].Caption, aGraphicWidth, aGraphicHeight, aGraphicX, aGraphicY, TextRect);
    Images.Draw(Canvas, aGraphicX, aGraphicY, Pages[aPage].ImageIndex);
  end
  else
    TextRect := aRect;

  if Tab = TabIndex
  then DoDrawTabStyleCaption(Canvas, Tab, Pages[aPage].Caption, TextRect, FActiveTabFont)
  else DoDrawTabStyleCaption(Canvas, Tab, Pages[aPage].Caption, TextRect, FInactiveTabFont);
end;

procedure TcyPageControl.CalcLayout(Rect: TRect; Caption: String; GraphicWidth, GraphicHeight: Integer; var GraphicX, GraphicY: Integer; var TextRect: TRect);
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

procedure TcyPageControl.WMPaint(var Message: TWMPaint);
var
  p: Integer;
  TabRect: TRect;
begin
  inherited;

  // Carefull: Tabs.Count only counts visible tabs !!!
  for p := PageCount-1 downto 0 do
  begin
    TabRect := GetTabBoundsRect(Pages[p].TabIndex);

    if TabRect.Right > 0 then
      cyDrawPage(p, TabRect);
  end;
end;

end.
