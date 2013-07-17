{   Component(s):
    tcyColorGrid

    Description:
    It's a selection palette color control
    with selected color and hot selection !!!
    
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

unit cyColorGrid;

interface

uses Classes, Types, Controls, Graphics, Messages, Windows;

type
  TcyCursorRender=class(TPersistent)
  private
    FFrameWidth: Integer;
    FColor: TColor;
    FFrameColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameWidth(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property FrameColor: TColor read FFrameColor write SetFrameColor default clNavy;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 1;
    property Color: TColor read FColor write SetColor default $00FF8E8E;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TColorState = set of (csHot, csSelected);
  TProcOnPaintBox = procedure (Sender: TObject; aRect: TRect; aState: TColorState; aRow: integer; aCol: integer; aColor: TColor) of object;
  TProcOnBoxClick = procedure (Sender: TObject; aRow: integer; aCol: integer; aColor: TColor) of object;

  TcyColorGrid = class(TGraphicControl)
    FColorList: TStrings;
  private
    FValidHotColor: Boolean;
    FNeedFullRepaint: Boolean;
    FRepaintColor: TColor;
    FptMouseDown: TPoint;
    FHot: Boolean;
    FHideSelection: Boolean;
    FSelection: TColor;
    FBackground: TColor;
    FBoxWidth: integer;
    FBoxHeight: integer;
    FBoxRows: integer;
    FBoxCols: integer;
    FBoxSpacingWidth: integer;
    FBoxSpacingHeight: integer;
    FBoxFrameWidth: integer;
    FBoxFrameColor: TColor;
    FOnCustomDrawBox: TProcOnPaintBox;
    FOnAfterPaint: TNotifyEvent;
    FTransparent: Boolean;
    FOnCustomDrawBkgnd: TNotifyEvent;
    FOnBoxClick: TProcOnBoxClick;
    FHotColor: TColor;
    FReadOnly: Boolean;
    FSelectionRender: TcyCursorRender;
    FHotRender: TcyCursorRender;
    procedure ColorListChange(Sender: TObject);
    procedure SetHot(const Value: Boolean);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetSelection(const Value: TColor);
    procedure SetBackground(const Value: TColor);
    procedure SetBoxWidth(const Value: integer);
    procedure SetBoxHeight(const Value: integer);
    procedure SetBoxRows(const Value: integer);
    procedure SetBoxCols(const Value: integer);
    procedure SetBoxSpacingWidth(const Value: integer);
    procedure SetBoxSpacingHeight(const Value: integer);
    procedure SetColorList(const Value: Tstrings);
    procedure SetBoxFrameWidth(const Value: integer);
    procedure SetBoxFrameColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetHotColor(const Value: TColor);
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetHotRender(const Value: TcyCursorRender);
    procedure SetSelectionRender(const Value: TcyCursorRender);
    procedure SubPropertiesChanged(Sender: TObject);
  protected
    procedure Click; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    function  CanPartialPaint: Boolean; virtual;
    procedure DoDrawBkgnd;
    procedure DoDrawBoxes;
    procedure DoDrawBox(BoxColor: TColor; BoxRect: TRect; Row, Column: Integer);
    procedure RedefineSize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property  HotColor: TColor read FHotColor write SetHotColor;
    procedure AddColor(Color: TColor);
    procedure DefaultDrawBkgnd;
    procedure DefaultDrawBox(aRect: TRect; aState: TColorState; aRow: integer; aCol: integer; aColor: TColor);
    function  GetColorState(Color: TColor): TColorState;
    function  GetBoxAtPos(aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;
    function  GetColorFromIndex(aIndex: Integer; var aColor: TColor): Boolean;
    function  GetColorListIndex(aRow: Integer; aCol: Integer): integer;
    function  GetColorGrid(aRow: Integer; aCol: integer; var aColor: TColor): Boolean;
    function  SetColorGrid(aRow: Integer; aCol: integer; aColor: TColor): Boolean;
  published
    property Align;
    property Autosize default true;
    property Anchors;
    property Constraints;
    property Enabled;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Background: TColor read FBackground write SetBackground default clWindow;
    property BoxHeight: integer read FBoxHeight write SetBoxHeight default 12;
    property BoxWidth: integer read FBoxWidth write SetBoxWidth default 12;
    property BoxRows: integer read FBoxRows write SetBoxRows default 5;
    property BoxCols: integer read FBoxCols write SetBoxCols default 8;
    property BoxFrameColor: TColor read FBoxFrameColor write SetBoxFrameColor default clGray;
    property BoxFrameWidth: integer read FBoxFrameWidth write SetBoxFrameWidth default 1;
    property BoxSpacingWidth: integer read FBoxSpacingWidth write SetBoxSpacingWidth default 6;
    property BoxSpacingHeight: integer read FBoxSpacingHeight write SetBoxSpacingHeight default 6;
    property ColorList: Tstrings read FColorList write SetColorList;
    property Hot: Boolean read FHot write SetHot default false;
    property HotRender: TcyCursorRender read FHotRender write SetHotRender;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default false;
    property Selection: TColor read FSelection write SetSelection default clBlack;
    property SelectionRender: TcyCursorRender read FSelectionRender write SetSelectionRender;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property OnCustomDrawBox: TProcOnPaintBox read FOnCustomDrawBox write FOnCustomDrawBox;
    property OnCustomDrawBkgnd: TNotifyEvent read FOnCustomDrawBkgnd write FOnCustomDrawBkgnd;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnBoxClick: TProcOnBoxClick read FOnBoxClick write FOnBoxClick;
  end;

implementation

{ TcyCursorRender }
constructor TcyCursorRender.Create(AOwner: TComponent);
begin
  FFrameColor := clNavy;
  FFrameWidth := 1;
  FColor      := $00FF8E8E;
end;

procedure TcyCursorRender.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCursorRender.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCursorRender.SetFrameWidth(const Value: Integer);
begin
  FFrameWidth := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TcyColorGrid }
constructor TcyColorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorList := TStringList.Create;
  FColorList.BeginUpdate;
  FColorList.Add('clBlack');
  FColorList.Add('clMaroon');
  FColorList.Add('clGreen');
  FColorList.Add('clOlive');
  FColorList.Add('clNavy');
  FColorList.Add('clPurple');
  FColorList.Add('clTeal');
  FColorList.Add('clGray');
  FColorList.Add('clSilver');
  FColorList.Add('clRed');
  FColorList.Add('clLime');
  FColorList.Add('clYellow');
  FColorList.Add('clBlue');
  FColorList.Add('clFuchsia');
  FColorList.Add('clAqua');
  FColorList.Add('clWhite');
  FColorList.Add('clMoneyGreen');
  FColorList.Add('clSkyBlue');
  FColorList.Add('clCream');
  FColorList.Add('clMedGray');
  FColorList.Add('clActiveBorder');
  FColorList.Add('clActiveCaption');
  FColorList.Add('clAppWorkSpace');
  FColorList.Add('clBackground');
  FColorList.Add('clBtnFace');
  FColorList.Add('clBtnHighlight');
  FColorList.Add('clBtnShadow');
  FColorList.Add('clBtnText');
  FColorList.Add('clCaptionText');
  FColorList.Add('clDefault');
  FColorList.Add('clGrayText');
  FColorList.Add('clHighlight');
  FColorList.Add('clHighlightText');
  FColorList.Add('clInactiveBorder');
  FColorList.Add('clInactiveCaption');
  FColorList.Add('clInactiveCaptionText');
  FColorList.Add('clInfoBk');
  FColorList.Add('clInfoText');
  FColorList.Add('clMenu');
  FColorList.Add('clMenuText');
  FColorList.Add('clNone');
  FColorList.Add('clScrollBar');
  FColorList.Add('cl3DDkShadow');
  FColorList.Add('cl3DLight');
  FColorList.Add('clWindow');
  FColorList.Add('clWindowFrame');
  FColorList.Add('clWindowText');
  FColorList.EndUpdate;
  TStringList(FColorList).OnChange := ColorListChange;
  FHot := false;
  FHideSelection := false;
  FSelection := clBlack;
  FBackground := clWindow;
  FBoxHeight := 12;
  FBoxWidth := 12;
  FBoxRows := 5;
  FBoxCols := 8;
  FBoxSpacingWidth := 6;
  FBoxSpacingHeight := 6;
  FBoxFrameWidth := 1;
  FBoxFrameColor := clGray;
  FReadOnly := false;
  FTransparent := false;
  FSelectionRender := TcyCursorRender.Create(self);
  FSelectionRender.OnChange := SubPropertiesChanged;
  FHotRender := TcyCursorRender.Create(self);
  FHotRender.OnChange := SubPropertiesChanged;
  Autosize:= true;
  RedefineSize;
end;

destructor TcyColorGrid.Destroy;
begin
  FSelectionRender.Free;
  FHotRender.Free;
  FColorList.Free;
  inherited Destroy;
end;

procedure TcyColorGrid.Loaded;
begin
  Inherited;
  FValidHotColor := false;
end;

procedure TcyColorGrid.SubPropertiesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyColorGrid.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value
  then begin
    inherited SetAutoSize(Value);  // Change Autosize value ...
    RedefineSize;
  end;
end;

procedure TcyColorGrid.RedefineSize;
begin
  if AutoSize
  then begin
    Width  := FBoxSpacingWidth + FBoxCols * (FBoxWidth + FBoxSpacingWidth);
    Height := FBoxSpacingHeight + FBoxRows * (FBoxHeight + FBoxSpacingHeight);
  end;
end;

procedure TcyColorGrid.SetBackground(const Value: TColor);
begin
  FBackground := Value;
  if not FTransparent
  then Invalidate;
end;

procedure TcyColorGrid.SetHot(const Value: Boolean);
begin
  FHot := Value;
  Invalidate;
end;

procedure TcyColorGrid.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxCols(const Value: integer);
begin
  FBoxCols := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxFrameColor(const Value: TColor);
begin
  FBoxFrameColor := Value;
  if FBoxFrameWidth <> 0
  then Invalidate;
end;

procedure TcyColorGrid.SetBoxFrameWidth(const Value: integer);
begin
  FBoxFrameWidth := Value;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxHeight(const Value: integer);
begin
  FBoxHeight := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxSpacingWidth(const Value: integer);
begin
  FBoxSpacingWidth := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxSpacingHeight(const Value: integer);
begin
  FBoxSpacingHeight := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxRows(const Value: integer);
begin
  FBoxRows := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetBoxWidth(const Value: integer);
begin
  FBoxWidth := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorGrid.SetColorList(const Value: Tstrings);
begin
  if Assigned(FColorList) then
    FColorList.Assign(Value)
  else
    FColorList := Value;
end;

procedure TcyColorGrid.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  Invalidate;
end;

procedure TcyColorGrid.ColorListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyColorGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FptMouseDown := Point(X, Y);
  inherited;
end;

procedure TcyColorGrid.Click;
var
  aRow, aCol: integer;
  aColor: TColor;
begin
  if GetBoxAtPos(FptMouseDown, aRow, aCol, true)
  then
    if GetColorGrid(aRow, aCol, aColor)
    then begin
      if not FReadOnly
      then Selection := aColor;

      if Assigned(FOnBoxClick)
      then FOnBoxClick(Self, aRow, aCol, aColor);
    end;

  Inherited;
end;

procedure TcyColorGrid.AddColor(Color: TColor);
begin
  FColorList.Add(ColorToString(Color));
end;

procedure TcyColorGrid.Paint;
begin
  FNeedFullRepaint := true;
  DoDrawBkgnd;
  DoDrawBoxes;
  if Assigned(FOnAfterPaint) then FOnAfterPaint(Self);
end;

procedure TcyColorGrid.DoDrawBkgnd;
begin
  if Assigned(FOnCustomDrawBkgnd)
  then FOnCustomDrawBkgnd(Self)
  else DefaultDrawBkgnd;
end;

procedure TcyColorGrid.DefaultDrawBkgnd;
begin
  if not FTransparent
  then begin
    Canvas.Brush.Color := FBackground;
    Canvas.FillRect(ClientRect);
  end;
end;

procedure TcyColorGrid.DoDrawBoxes;
var
  curIndex, r, c, xPos, yPos: integer;
  BoxRect: TRect;
  aColor: TColor;
  StrColor: ShortString;
begin
  curIndex := -1;
  yPos := FBoxSpacingHeight;

  for r := 0 to FBoxRows -1 do
  begin
    xPos := FBoxSpacingWidth;

    for c := 0 to FBoxCols -1 do
    begin
      inc(curIndex, 1);

      if curIndex <= FColorList.Count-1
      then begin
        StrColor := FColorList[CurIndex];

        if StrColor <> ''
        then begin
          aColor  := StringToColor(StrColor);

          if (FNeedFullRepaint) or (FRepaintColor = aColor)
          then begin
            BoxRect := classes.Rect(xPos, yPos, xPos + FBoxWidth, yPos + FBoxHeight);
            DoDrawBox(aColor, BoxRect, r, c);
          end;
        end;
      end;

      Inc(xPos, FBoxWidth + FBoxSpacingWidth);
    end;

    Inc(yPos, FBoxHeight + FBoxSpacingHeight);
  end;
end;

procedure TcyColorGrid.DoDrawBox(BoxColor: TColor; BoxRect: TRect; Row, Column: Integer);
var ColorState: TColorState;

        procedure DrawCellBkgnd;
        var
          BgndRect: TRect;
          ShowSelection, ShowHot: Boolean;
          cursorRender: TcyCursorRender;
        begin
          BgndRect := BoxRect;
          InflateRect(BgndRect, FBoxSpacingWidth div 2, FBoxSpacingHeight div 2);
          ShowSelection := (csSelected in ColorState) and (not FHideSelection);
          ShowHot := (csHot in ColorState) and (FHot);

          if ShowSelection or ShowHot
          then begin
            if ShowSelection
            then cursorRender := FSelectionRender
            else cursorRender := FHotRender;

            if cursorRender.FFrameWidth > 0
            then begin
              Canvas.Brush.Color := cursorRender.FrameColor;
              Canvas.FillRect(BgndRect);
              InflateRect(BgndRect, -cursorRender.FFrameWidth, -cursorRender.FFrameWidth);
            end;

            Canvas.Brush.Color := cursorRender.Color;
            Canvas.FillRect(BgndRect);

            // Show Selection/Hot despite of no margin avaible :
            if (FBoxSpacingWidth = 0) and (FBoxSpacingHeight = 0)
            then
              if not Assigned(FOnCustomDrawBox)
              then InflateRect(BoxRect, -1, -1);
          end
          else begin
            Canvas.Brush.Color := FBackground;
            Canvas.FillRect(BgndRect);
          end;
        end;

begin
  ColorState := GetColorState(BoxColor);

  // Draw cell background:
  if not FNeedFullRepaint
  then
    DrawCellBkgnd  // ColorState changed, always to be done!
  else
    if ((csSelected in ColorState) and (not FHideSelection))
      or ((csHot in ColorState) and (FHot))
    then DrawCellBkgnd;

  // Draw box color :
  if Assigned(FOnCustomDrawBox)
  then FOnCustomDrawBox(self, BoxRect, ColorState, Row, Column, BoxColor)
  else DefaultDrawBox(BoxRect, ColorState, Row, Column, BoxColor);
end;

procedure TcyColorGrid.DefaultDrawBox(aRect: TRect; aState: TColorState; aRow: integer; aCol: integer; aColor: TColor);
begin
  if FBoxHeight = 1
  then begin
    if FBoxWidth = 1
    then begin
      // Draw a point :
      Canvas.Pixels[aRect.Left, aRect.Top] := aColor;
    end
    else begin
      // Draw horizontal line :
      Canvas.Brush.Color := aColor;
      Canvas.FillRect(aRect);
    end;
  end
  else begin
    if FBoxWidth = 1
    then begin
      // Draw vertical line :
      Canvas.Brush.Color := aColor;
      Canvas.FillRect(aRect);
    end
    else
      case FBoxFrameWidth of
        0 : begin
              Canvas.Brush.Color := aColor;
              Canvas.FillRect(aRect);
            end;

        1 : begin
              Canvas.Brush.Color := aColor;
              Canvas.Pen.Width := FBoxFrameWidth;
              Canvas.Pen.Color := FBoxFrameColor;
              Canvas.Rectangle(aRect);
            end;

            else begin
              // Draw outer Rect :
              Canvas.Brush.Color := FBoxFrameColor;
              Canvas.FillRect(aRect);

              // Draw inner Rect :
              InflateRect(aRect, -FBoxFrameWidth, -FBoxFrameWidth);
              Canvas.Brush.Color := aColor;
              Canvas.FillRect(aRect);
            end;
      end;
  end;
end;

function TcyColorGrid.CanPartialPaint: Boolean;
begin
  RESULT := false;

  if not FTransparent
  then
    if not Assigned(FOnCustomDrawBkgnd)
    then
      if not Assigned(FOnAfterPaint)
      then RESULT := true;
end;

function TcyColorGrid.GetColorState(Color: TColor): TColorState;
begin
  RESULT := [];

  if Color = FSelection
  then Include(RESULT, csSelected);

  if (Color = FHotColor) and (FValidHotColor)
  then Include(RESULT, csHot);
end;

function  TcyColorGrid.GetBoxAtPos(aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;

    function VerifyIntervals: Boolean;
    begin
      RESULT := false;

      if (aCol >= 0) and (aCol < FBoxCols)
      then
        if (aRow >= 0) and (aRow < FBoxRows)
        then RESULT := true;
    end;

begin
  RESULT := false;
  aCol := (aPoint.X - FBoxSpacingWidth div 2) div (FBoxWidth + FBoxSpacingWidth);
  aRow := (aPoint.Y - FBoxSpacingHeight div 2) div (FBoxHeight + FBoxSpacingHeight);

  if ExactPos
  then begin
    if aPoint.X >= (FBoxWidth + FBoxSpacingWidth) * aCol + FBoxSpacingWidth
    then
      if aPoint.X <= (FBoxWidth + FBoxSpacingWidth) * (aCol + 1)
      then
        if aPoint.Y >= (FBoxHeight + FBoxSpacingHeight) * aRow + FBoxSpacingHeight
        then
          if aPoint.Y <= (FBoxHeight + FBoxSpacingHeight) * (aRow + 1)
          then RESULT := VerifyIntervals;
  end
  else
    RESULT := VerifyIntervals;
end;

function  TcyColorGrid.GetColorListIndex(aRow: Integer; aCol: Integer): integer;
begin
  RESULT := -1;

  if (ARow <= FBoxRows-1) and (ACol <= FBoxCols-1)
  then RESULT := (ARow * FBoxCols) + ACol;
end;

function  TcyColorGrid.GetColorFromIndex(aIndex: Integer; var aColor: TColor): Boolean;
begin
  RESULT := false;

  if aIndex >= 0
  then
    if aIndex < FColorList.Count
    then
      if FColorList[aIndex] <> ''
      then
        try
          aColor := StringToColor(FColorList[aIndex]);
        finally
          RESULT := true;
        end;
end;

function  TcyColorGrid.GetColorGrid(aRow: Integer; aCol: integer; var aColor: TColor): Boolean;
begin
  RESULT := GetColorFromIndex(GetColorListIndex(aRow, aCol), aColor);
end;

function  TcyColorGrid.SetColorGrid(aRow: Integer; aCol: integer; aColor: TColor): Boolean;
var
  ColorListIndex, i, x, y: integer;
  BoxRect: TRect;
begin
  RESULT := false;
  ColorListIndex := GetColorListIndex(aRow, aCol);

  if ColorListIndex <> -1
  then begin
    TStringList(FColorList).OnChange := nil;

    for i := (FColorList.Count-1) to ColorListIndex do
      FColorList.Add('');

    if CanPartialPaint
    then begin
      FColorList[ColorListIndex] := ColorToString(aColor);
      x := FBoxSpacingWidth + (FBoxWidth + FBoxSpacingWidth) * aCol;
      y := FBoxSpacingHeight + (FBoxHeight + FBoxSpacingHeight) * aRow;
      BoxRect := classes.Rect(x, y, x + FBoxWidth, y + FBoxHeight);
      FNeedFullRepaint := false;
      DoDrawBox(aColor, BoxRect, aRow, aCol);
    end
    else begin
      FColorList[ColorListIndex] := ColorToString(aColor);
      Invalidate;
    end;

    TStringList(FColorList).OnChange := ColorListChange;
    RESULT := true;
  end;
end;

procedure TcyColorGrid.SetSelection(const Value: TColor);
var Buf: TColor;
begin
  if Value <> FSelection
  then begin
    Buf := FSelection;
    FSelection := Value;

    if not FHideSelection
    then
      if CanPartialPaint   // Avoid full repaint with blinking effect ...
      then begin
        FNeedFullRepaint := false;
        FRepaintColor := Buf;
        DoDrawBoxes;                             // Repaint old selection color ...
        FRepaintColor := FSelection;
        DoDrawBoxes;                             // Paint new selection color ...
      end
      else
        Invalidate;
  end;
end;

procedure TcyColorGrid.SetSelectionRender(const Value: TcyCursorRender);
begin
  FSelectionRender := Value;
end;

procedure TcyColorGrid.SetHotColor(const Value: TColor);
var Buf: TColor;
begin
  if (Value <> FHotColor) or (not FValidHotColor)
  then begin
    Buf := FHotColor;
    FHotColor := Value;

    if CanPartialPaint       // Avoid full repaint with blinking effect ...
    then begin
      FNeedFullRepaint := false;

      if FValidHotColor
      then begin
        FRepaintColor := Buf;
        DoDrawBoxes;
      end
      else
        FValidHotColor := true;

      FRepaintColor := FHotColor;
      DoDrawBoxes;
    end
    else begin
      FValidHotColor := true;
      Invalidate;
    end;
  end;
end;

procedure TcyColorGrid.SetHotRender(const Value: TcyCursorRender);
begin
  FHotRender := Value;
end;

procedure TcyColorGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aRow, aCol: integer;
  aColor: TColor;
begin
  if FHot
  then
    if GetBoxAtPos(Point(X, Y), aRow, aCol, true)
    then begin
      if GetColorGrid(aRow, aCol, aColor)
      then HotColor := aColor;
    end
    else
      // Deactivate old hot color !!!
      if FValidHotColor
      then begin
        FValidHotColor := false;

        if CanPartialPaint
        then begin
          FNeedFullRepaint := false;
          FRepaintColor := FHotColor;
          DoDrawBoxes;
        end
        else
          Invalidate;
      end;

  inherited;
end;

procedure TcyColorGrid.CmMouseLeave(var Msg: TMessage);
begin
  if FValidHotColor
  then begin
    FValidHotColor := false;

    if CanPartialPaint
    then begin
      FNeedFullRepaint := false;
      FRepaintColor := FHotColor;
      DoDrawBoxes;
    end
    else
      Invalidate;
  end;

  inherited;
end;

end.
