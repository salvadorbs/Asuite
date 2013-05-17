{   Component(s):
    tcySimpleGauge

    Description:
    It' s a simple gauge that allow user defined visualisation and custom paint.
    The orientation can be left-to-right, right-to-left, top-to-bottom and bottom-to-top.

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

unit cySimpleGauge;

interface

uses classes, Windows, Graphics, VCL.cyTypes, VCL.cyGraphics, cyCustomGauge;

type
  TGaugeOrientation = (goLeftRight, goRightLeft, goTopBottom, goBottomTop);
  TItemStyle = (isGradient, isRectangle, isRoundRect, isEllipsis, isNone);

  TcySimpleGauge = class(TcyCustomGauge)
  private
    FOrientation: TGaugeOrientation;
    FItemOnBrush: TBrush;
    FItemOffBrush: TBrush;
    FItemOnPen: TPen;
    FItemOffPen: TPen;
    FItemOnStyle: TItemStyle;
    FItemOffStyle: TItemStyle;
    Resizing: Boolean;
    FDegradeBalance: Integer;
    FDegradeBalanceMode: TDgradBalanceMode;
    FDegradeBalanceSpeedPercent: Integer;
    procedure ChangedProperty(Sender: TObject);
    procedure SetOrientation(const Value: TGaugeOrientation);
    procedure SetItemOffBrush(const Value: TBrush);
    procedure SetItemOffPen(const Value: TPen);
    procedure SetItemOnBrush(const Value: TBrush);
    procedure SetItemOnPen(const Value: TPen);
    procedure SetItemOffStyle(const Value: TItemStyle);
    procedure SetItemOnStyle(const Value: TItemStyle);
    procedure SetDegradeBalance(const Value: Integer);
    procedure SetDegradeBalanceMode(const Value: TDgradBalanceMode);
    procedure SetDegradeBalanceSpeedPercent(const Value: Integer);
  protected
    procedure AdjustSize; override;
    procedure Draw(Rect: TRect; FullRepaint: Boolean); override;
    procedure MeasureBoundsChanged; override;
    procedure Resize; override;
    procedure DoDrawLeftRightGauge(Rect: TRect; FullRepaint: Boolean);
    procedure DoDrawRightLeftGauge(Rect: TRect; FullRepaint: Boolean);
    procedure DoDrawTopBottomGauge(Rect: TRect; FullRepaint: Boolean);
    procedure DoDrawBottomTopGauge(Rect: TRect; FullRepaint: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetValueFromPos(fromPoint: TPoint; var Value: Double): boolean; override;
    procedure DefaultDrawItem(aRect: TRect; ItemValue: Double);
    function ItemsRect: TRect;
  published
    property DegradeBalance: Integer read FDegradeBalance write SetDegradeBalance default 60;
    property DegradeBalanceMode: TDgradBalanceMode read FDegradeBalanceMode write SetDegradeBalanceMode default bmReverse;
    property DegradeBalanceSpeedPercent: Integer read FDegradeBalanceSpeedPercent write SetDegradeBalanceSpeedPercent default 100;
    property ItemOffBrush: TBrush read FItemOffBrush write SetItemOffBrush;
    property ItemOffPen: TPen read FItemOffPen write SetItemOffPen;
    property ItemOffStyle: TItemStyle read FItemOffStyle write SetItemOffStyle default isGradient;
    property ItemOnBrush: TBrush read FItemOnBrush write SetItemOnBrush;
    property ItemOnPen: TPen read FItemOnPen write SetItemOnPen;
    property ItemOnStyle: TItemStyle read FItemOnStyle write SetItemOnStyle default isGradient;
    property Orientation: TGaugeOrientation read FOrientation write SetOrientation default goLeftRight;
    property Align;
    property Anchors;
    property Constraints;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // TcyBaseMeasure properties :
    property MarginBottom;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    property Color;
    property ItemsCount;
    property ItemsSpacing;
    property ItemsWidth;
    property ItemsHeight;
    property Min;
    property Max;
    property ReadOnly;
    property Smooth;
    property Transparent;
    property OnBeforePaint;
    property OnAfterPaint;
    // TcyCustomGauge properties :
    property Bevels;
    property PartialPaint;  
    property Position;
    property Precision;
    property Step;
    property OnChange;
    property OnCustomDrawItem;
  end;

implementation

uses Controls;

{ TcySimpleGauge }

constructor TcySimpleGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Resizing := false;
  FOrientation := goLeftRight;
  FDegradeBalance := 50;
  FDegradeBalanceMode := bmReverse;
  FDegradeBalanceSpeedPercent := 100;
  FItemOnStyle := isGradient;
  FItemOffStyle := isGradient;
  FItemOnPen := TPen.Create;
  FItemOnPen.Color := $00FF8000;
  FItemOnPen.Width := 1;
  FItemOnPen.OnChange := ChangedProperty;
  FItemOnBrush := TBrush.Create;
  FItemOnBrush.Color := clAqua;
  FItemOnBrush.OnChange := ChangedProperty;
  FItemOffPen := TPen.Create;
  FItemOffPen.Color := clBlack;
  FItemOffPen.Width := 1;
  FItemOffPen.OnChange := ChangedProperty;
  FItemOffBrush := TBrush.Create;
  FItemOffBrush.Color := clGray;
  FItemOffBrush.OnChange := ChangedProperty;
end;

destructor TcySimpleGauge.Destroy;
begin
  FItemOnPen.Free;
  FItemOnBrush.Free;
  FItemOffPen.Free;
  FItemOffBrush.Free;
  inherited Destroy;
end;

procedure TcySimpleGauge.ChangedProperty(Sender: TObject);
begin
  Invalidate;
end;

procedure TcySimpleGauge.SetOrientation(const Value: TGaugeOrientation);
var Rotate: Boolean;
begin
  if Value <> FOrientation
  then begin
    if FOrientation in [goLeftRight, goRightLeft]
    then Rotate := Value in [goTopBottom, goBottomTop]
    else Rotate := Value in [goLeftRight, goRightLeft];

    FOrientation := Value;
    Invalidate;

    if Rotate and not (csLoading in ComponentState)
    then SetBounds(Left, Top, Height, Width);
  end;
end;

procedure TcySimpleGauge.SetDegradeBalance(const Value: Integer);
begin
  if (Value <> FDegradeBalance)
    and (Value in [0..100])
  then begin
    FDegradeBalance := Value;
    Invalidate;
  end;
end;

procedure TcySimpleGauge.SetDegradeBalanceMode(const Value: TDgradBalanceMode);
begin
  FDegradeBalanceMode := Value;
  Invalidate;
end;

procedure TcySimpleGauge.SetDegradeBalanceSpeedPercent(
  const Value: Integer);
begin
  FDegradeBalanceSpeedPercent := Value;
  Invalidate;
end;

procedure TcySimpleGauge.SetItemOffBrush(const Value: TBrush);
begin
  FItemOffBrush := Value;
end;

procedure TcySimpleGauge.SetItemOffPen(const Value: TPen);
begin
  FItemOffPen := Value;
end;

procedure TcySimpleGauge.SetItemOnBrush(const Value: TBrush);
begin
  FItemOnBrush := Value;
end;

procedure TcySimpleGauge.SetItemOnPen(const Value: TPen);
begin
  FItemOnPen := Value;
end;

procedure TcySimpleGauge.SetItemOffStyle(const Value: TItemStyle);
begin
  FItemOffStyle := Value;
  Invalidate;
end;

procedure TcySimpleGauge.SetItemOnStyle(const Value: TItemStyle);
begin
  FItemOnStyle := Value;
  Invalidate;
end;

procedure TcySimpleGauge.MeasureBoundsChanged;
begin
  AdjustSize;
end;

procedure TcySimpleGauge.AdjustSize;   
var w, h: integer;
begin
  if (not (csLoading in ComponentState)) and (not Resizing)
  then begin
    Resizing := true;
    w := Bevels.BevelsWidth * 2;
    h := w + ItemsHeight;
    w := w + ItemsWidth * ItemsCount + ItemsSpacing * (ItemsCount-1);

    if FOrientation in [goLeftRight, goRightLeft]
    then SetBounds(Left, Top, w + MarginLeft + MarginRight, h + MarginTop + MarginBottom)
    else SetBounds(Left, Top, h + MarginLeft + MarginRight, w + MarginTop + MarginBottom);

    Resizing := false;
  end;
end;

procedure TcySimpleGauge.Resize;
var BvW, AvaibleHeight, AvaibleWidth: Integer;
begin
  if not Resizing
  then begin
    Resizing := true;
    BvW := Bevels.BevelsWidth * 2;
    AvaibleHeight := Height - MarginTop - MarginBottom - BvW;
    AvaibleWidth := Width - MarginLeft - MarginRight - BvW;

    if FOrientation in [goLeftRight, goRightLeft]
    then begin
      ItemsHeight := AvaibleHeight;
      ItemsCount := (AvaibleWidth + ItemsSpacing) div (ItemsWidth + ItemsSpacing);
    end
    else begin
      ItemsHeight := AvaibleWidth;
      ItemsCount := (AvaibleHeight + ItemsSpacing) div (ItemsWidth + ItemsSpacing);
    end;
    Resizing := false;
  end;

  Inherited;
end;

procedure TcySimpleGauge.Draw(Rect: TRect; FullRepaint: Boolean);
var
  DefaultDrawing: Boolean;
  w: Integer;
begin
  DefaultDrawing := not Assigned(OnCustomDrawItem);

  if DefaultDrawing and (not FullRepaint)  // Partial paint to avoid clipping :
  then begin
    if OldPosition > Position
    then FullRepaint := FItemOffStyle = isNone
    else FullRepaint := FItemOnStyle = isNone;

    if FullRepaint and Transparent
    then begin
      Invalidate;
      Exit;
    end;
  end;

  if FullRepaint
  then begin
    if not Transparent
    then begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect);
    end;

    Bevels.DrawBevels(Canvas, Rect, false);
  end
  else begin
    w := Bevels.BevelsWidth;
    InflateRect(Rect, -w, -w);
  end;

  Rect := classes.Rect(Rect.Left + MarginLeft, Rect.Top + MarginTop,
                        Rect.Right - MarginRight, Rect.Bottom - MarginBottom);

  if (ItemsCount > 0) and (Max <> Min) and (Rect.Right <> Rect.Left) and (Rect.Top <> Rect.Bottom)  // Avoid division by zero ...
  then begin
    case FOrientation of
      goLeftRight: DoDrawLeftRightGauge(Rect, FullRepaint);
      goRightLeft: DoDrawRightLeftGauge(Rect, FullRepaint);
      goTopBottom: DoDrawTopBottomGauge(Rect, FullRepaint);
      goBottomTop: DoDrawBottomTopGauge(Rect, FullRepaint);
    end;
  end;
end;

procedure TcySimpleGauge.DoDrawLeftRightGauge(Rect: TRect; FullRepaint: Boolean);
var
  ItemRect: TRect;
  ItemValue: Double;
  I, XCoord: integer;
  DefaultDrawing: Boolean;

    function NeedPaintItem: boolean;
    begin
      RESULT := FullRepaint;

      if not RESULT
      then
        if OldPosition > Position
        then RESULT := (ItemValue >= Position) and (ItemValue <= OldPosition)
        else RESULT := (ItemValue >= OldPosition) and (ItemValue <= Position);
    end;

begin
  DefaultDrawing := not Assigned(OnCustomDrawItem);

  if Smooth
  then begin
    // Draw ItemOn :
    XCoord := Rect.Left + round((Rect.Right-Rect.Left) * ((position-Min)/(max-Min)));
    ItemRect := classes.Rect(Rect.Left, Rect.Top, XCoord, Rect.Bottom);

    if (ItemRect.Right > Rect.Left)
    then begin
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Position)
      else OnCustomDrawItem(self, ItemRect, Position);
    end
    else
      ItemRect := classes.Rect(Rect.Left, Rect.Top, Rect.Left, Rect.Bottom);

    // Draw ItemOff :
    ItemRect := classes.Rect(ItemRect.Right, Rect.Top, Rect.Right, Rect.Bottom);

    if ItemRect.Right > ItemRect.Left
    then
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Max)
      else OnCustomDrawItem(self, ItemRect, Max);
  end
  else begin
    XCoord := Rect.Left;

    for I := 1 to ItemsCount do
    begin
      ItemValue := Min + (I * (Max-Min) / ItemsCount);
      ItemRect := classes.Rect(XCoord, Rect.Top, XCoord + ItemsWidth, Rect.Bottom);
      inc(XCoord, ItemsWidth + ItemsSpacing);

      if NeedPaintItem
      then
        if DefaultDrawing
        then DefaultDrawItem(ItemRect, ItemValue)
        else OnCustomDrawItem(self, ItemRect, ItemValue);
    end;
  end;
end;

procedure TcySimpleGauge.DoDrawRightLeftGauge(Rect: TRect; FullRepaint: Boolean);
var
  ItemRect: TRect;
  ItemValue: Double;
  I, XCoord: integer;
  DefaultDrawing: Boolean;

    function NeedPaintItem: boolean;
    begin
      RESULT := FullRepaint;

      if not RESULT
      then
        if OldPosition > Position
        then RESULT := (ItemValue >= Position) and (ItemValue <= OldPosition)
        else RESULT := (ItemValue >= OldPosition) and (ItemValue <= Position);
    end;

begin
  DefaultDrawing := not Assigned(OnCustomDrawItem);

  if Smooth
  then begin
    // Draw ItemOn :
    XCoord := Rect.Right - round((Rect.Right-Rect.Left) * ((position-Min)/(max-Min)));
    ItemRect := classes.Rect(XCoord, Rect.Top, Rect.Right, Rect.Bottom);

    if (ItemRect.Right > Rect.Left)
    then begin
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Position)
      else OnCustomDrawItem(self, ItemRect, Position);
    end
    else
      ItemRect := classes.Rect(Rect.Right, Rect.Top, Rect.Right, Rect.Bottom);

    // Draw ItemOff :
    ItemRect := classes.Rect(Rect.Left, Rect.Top, ItemRect.Left, Rect.Bottom);

    if ItemRect.Right > ItemRect.Left
    then
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Max)
      else OnCustomDrawItem(self, ItemRect, Max);
  end
  else begin
    XCoord := Rect.Right;

    for I := 1 to ItemsCount do
    begin
      ItemValue := Min + (I * (Max-Min) / ItemsCount);
      ItemRect := classes.Rect(XCoord - ItemsWidth, Rect.Top, XCoord, Rect.Bottom);
      inc(XCoord, -(ItemsWidth + ItemsSpacing));

      if NeedPaintItem
      then
        if DefaultDrawing
        then DefaultDrawItem(ItemRect, ItemValue)
        else OnCustomDrawItem(self, ItemRect, ItemValue);
    end;
  end;
end;

procedure TcySimpleGauge.DoDrawTopBottomGauge(Rect: TRect; FullRepaint: Boolean);
var
  ItemRect: TRect;
  ItemValue: Double;
  I, YCoord: integer;
  DefaultDrawing: Boolean;

    function NeedPaintItem: boolean;
    begin
      RESULT := FullRepaint;

      if not RESULT
      then
        if OldPosition > Position
        then RESULT := (ItemValue >= Position) and (ItemValue <= OldPosition)
        else RESULT := (ItemValue >= OldPosition) and (ItemValue <= Position);
    end;

begin
  DefaultDrawing := not Assigned(OnCustomDrawItem);

  if Smooth
  then begin
    // Draw ItemOn :
    YCoord := Rect.Top + round((Rect.Bottom-Rect.Top) * ((position-Min)/(max-Min)));
    ItemRect := classes.Rect(Rect.Left, Rect.Top, Rect.Right, YCoord);

    if (ItemRect.Bottom > Rect.Top)
    then begin
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Position)
      else OnCustomDrawItem(self, ItemRect, Position);
    end
    else
      ItemRect := classes.Rect(Rect.Left, Rect.Top, Rect.Right, Rect.Top);

    // Draw ItemOff :
    ItemRect := classes.Rect(Rect.Left, ItemRect.Bottom, Rect.Right, Rect.Bottom);

    if ItemRect.Bottom > ItemRect.Top
    then
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Max)
      else OnCustomDrawItem(self, ItemRect, Max);
  end
  else begin
    YCoord := Rect.Top;

    for I := 1 to ItemsCount do
    begin
      ItemValue := Min + (I * (Max-Min) / ItemsCount);
      ItemRect := classes.Rect(Rect.Left, YCoord, Rect.Right, YCoord + ItemsWidth);
      inc(YCoord, ItemsWidth + ItemsSpacing);

      if NeedPaintItem
      then
        if DefaultDrawing
        then DefaultDrawItem(ItemRect, ItemValue)
        else OnCustomDrawItem(self, ItemRect, ItemValue);
    end;
  end;
end;

procedure TcySimpleGauge.DoDrawBottomTopGauge(Rect: TRect; FullRepaint: Boolean);
var
  ItemRect: TRect;
  ItemValue: Double;
  I, YCoord: integer;
  DefaultDrawing: Boolean;

    function NeedPaintItem: boolean;
    begin
      RESULT := FullRepaint;

      if not RESULT
      then
        if OldPosition > Position
        then RESULT := (ItemValue >= Position) and (ItemValue <= OldPosition)
        else RESULT := (ItemValue >= OldPosition) and (ItemValue <= Position);
    end;
  
begin
  DefaultDrawing := not Assigned(OnCustomDrawItem);

  if Smooth
  then begin
    // Draw ItemOn :
    YCoord := Rect.Bottom - round((Rect.Bottom-Rect.Top) * ((position-Min)/(max-Min)));
    ItemRect := classes.Rect(Rect.Left, YCoord, Rect.Right, Rect.Bottom);


    if (ItemRect.Bottom > Rect.Top)
    then begin
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Position)
      else OnCustomDrawItem(self, ItemRect, Position);
    end
    else
      ItemRect := classes.Rect(Rect.Left, Rect.Bottom, Rect.Right, Rect.Bottom);

    // Draw ItemOff :
    ItemRect := classes.Rect(Rect.Left, Rect.Top, Rect.Right, YCoord);

    if ItemRect.Bottom > ItemRect.Top
    then
      if DefaultDrawing
      then DefaultDrawItem(ItemRect, Max)
      else OnCustomDrawItem(self, ItemRect, Max);
  end
  else begin
    YCoord := Rect.Bottom;

    for I := 1 to ItemsCount do
    begin
      ItemValue := Min + (I * (Max-Min) / ItemsCount);
      ItemRect := classes.Rect(Rect.Left, YCoord - ItemsWidth, Rect.Right, YCoord);
      inc(YCoord, -(ItemsWidth + ItemsSpacing));

      if NeedPaintItem
      then
        if DefaultDrawing
        then DefaultDrawItem(ItemRect, ItemValue)
        else OnCustomDrawItem(self, ItemRect, ItemValue);
    end;
  end;
end;

procedure TcySimpleGauge.DefaultDrawItem(aRect: TRect; ItemValue: Double);
var
  ItemStyle: TItemStyle;
  aDragdOrientation: TDgradOrientation;
begin
  if ItemValue > Position
  then begin
    ItemStyle := FItemOffStyle;
    Canvas.Pen.Assign(FItemOffPen);
    Canvas.Brush.Assign(FItemOffBrush);
  end
  else begin
    ItemStyle := FItemOnStyle;
    Canvas.Pen.Assign(FItemOnPen);
    Canvas.Brush.Assign(FItemOnBrush);
  end;

  if ItemStyle = isNone
  then EXIT;

  // Adjust rect because of pen.width :
  if Canvas.Pen.Width <> 0
  then begin
    aRect := classes.Rect(aRect.Left + Canvas.Pen.Width div 2,
                           aRect.Top + Canvas.Pen.Width div 2,
                            aRect.Right - (Canvas.Pen.Width-1) div 2,
                             aRect.Bottom - (Canvas.Pen.Width-1) div 2);
  end
  else
    Canvas.Pen.Color := Canvas.Brush.Color;

  case ItemStyle of
    isGradient:
      begin
        if FOrientation in [goLeftRight, goRightLeft]
        then aDragdOrientation := dgdVertical
        else aDragdOrientation := dgdHorizontal;

        if Canvas.Brush.Color = Canvas.Pen.Color
        then begin
          Canvas.Brush.Color := Canvas.Brush.Color;
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(aRect);
        end
        else
          cyGradientFill(Canvas, aRect, Canvas.Brush.Color, Canvas.Pen.Color, aDragdOrientation,
                          FDegradeBalance, FDegradeBalanceMode, 255, FDegradeBalanceSpeedPercent);

        if Canvas.Pen.Width <> 0
        then begin
          Canvas.Brush.Style := bsClear;    
          Canvas.Rectangle(aRect);
        end;
      end;

    isRectangle:
      Canvas.Rectangle(aRect);

    isRoundRect:
      Canvas.RoundRect(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom,
        (aRect.Right-aRect.Left) div 2, (aRect.Bottom-aRect.Top) div 2);

    isEllipsis:
      Canvas.Ellipse(aRect);
  end;
end;

function TcySimpleGauge.GetValueFromPos(fromPoint: TPoint; var Value: Double): boolean;
var
  ValuesRect: TRect;
  percPoint: Double;
begin
  RESULT := false;
  ValuesRect := ItemsRect;

  percPoint := 0;
  case FOrientation of
    goLeftRight:
    begin
      if ValuesRect.Right - ValuesRect.Left <> 0  // Avoid division by zero ...
      then begin
        percPoint := (fromPoint.X - ValuesRect.Left) / (ValuesRect.Right - ValuesRect.Left);
        RESULT := true;
      end;
    end;

    goRightLeft:
    begin
      if ValuesRect.Right - ValuesRect.Left <> 0  // Avoid division by zero ...
      then begin
        percPoint := (ValuesRect.Right - fromPoint.X) / (ValuesRect.Right - ValuesRect.Left);
        RESULT := true;
      end;
    end;

    goTopBottom:
    begin
      if ValuesRect.Bottom - ValuesRect.Top <> 0  // Avoid division by zero ...
      then begin
        percPoint := (fromPoint.Y - ValuesRect.Top) / (ValuesRect.Bottom - ValuesRect.Top);
        RESULT := true;
      end;
    end;

    goBottomTop:
    begin
      if ValuesRect.Bottom - ValuesRect.Top <> 0  // Avoid division by zero ...
      then begin
        percPoint := (ValuesRect.Bottom - fromPoint.Y) / (ValuesRect.Bottom - ValuesRect.Top);
        RESULT := true;
      end;
    end;
  end;

  if RESULT
  then begin
    // Value := Max * percPoint;
    Value := Min + (Max-Min) * percPoint;

    if Value > Max then Value := Max;
    if Value < Min then Value := Min;
  end;
end;

function TcySimpleGauge.ItemsRect: TRect;
begin
  RESULT := ClientRect;
  InflateRect(RESULT, -Bevels.BevelsWidth, -Bevels.BevelsWidth);
  RESULT := Rect(RESULT.Left + MarginLeft, RESULT.Top + MarginTop,
                  RESULT.Right - MarginRight, RESULT.Bottom - MarginBottom);
end;

end.
