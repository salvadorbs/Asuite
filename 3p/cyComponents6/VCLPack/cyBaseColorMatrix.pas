{   Component(s):
    tcyBaseMatrix

    Description:
    This is a 2 dimensions array of TColor representation base component.
    This component allow best performance than TcyColorGrid.

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

unit cyBaseColorMatrix;

interface

uses Classes, Windows, Types, Controls, Graphics, Messages, ExtCtrls, VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics;

type
  TRGBQuadArray = Array[0..0] of TRGBQuad;
  pRGBQuadArray = ^TRGBQuadArray;

  TProcOnDrawCell = procedure (Sender: TObject; aRect: TRect; aRow: integer; aCol: integer; aColor: TColor) of object;
  TProcOnCellClick = procedure (Sender: TObject; aRow: integer; aCol: integer; aColor: TColor) of object;

  TcyBaseColorMatrix = class(TGraphicControl)
  private
    FUpdateCount: Integer;
    FDisplay: TBitmap;
    FColorArray: Array of Array of TColor;
    FptMouseDown: TPoint;
    //
    FCellWidth: integer;
    FCellHeight: integer;
    FRowCount: integer;
    FColCount: integer;
    FCellSpacingWidth: integer;
    FCellSpacingHeight: integer;
    FCellFrameWidth: integer;
    FCellFrameColor: TColor;
    FOnCustomDrawCell: TProcOnDrawCell;
    FOnCellClick: TProcOnCellClick;
    FDefaultColor: TColor;
    FBackground: TcyGradient;
    FBorderWidth: Integer;
    FLeftColumnValue: Double;
    FTopRowValue: Double;
    FRightColumnValue: Double;
    FBottomRowValue: Double;
    FOnPaint: TNotifyEvent;
    procedure SetCellWidth(const Value: integer);
    procedure SetCellHeight(const Value: integer);
    procedure SetRowCount(const Value: integer);
    procedure SetColCount(const Value: integer);
    procedure SetCellSpacingWidth(const Value: integer);
    procedure SetCellSpacingHeight(const Value: integer);
    procedure SetCellFrameWidth(const Value: integer);
    procedure SetCellFrameColor(const Value: TColor);
    procedure SetDefaultColor(const Value: TColor);
    procedure SetBackground(const Value: TcyGradient);
    procedure SetBorderWidth(const Value: Integer);
    function GetCanvas: TCanvas;
    procedure BitmapChanged(Sender: TObject);
    function CalcWidth: Integer;
    function CalcHeight: Integer;
  protected
    property Autosize default true;
    procedure AdjustSize; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;   // Don' t let resize component ...
    procedure Click; override;
    function GetClientRect: TRect; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure Paint; override;
    procedure BackgroundChanged(Sender: TObject);
    procedure DrawMatrix; virtual;
    procedure DrawBackground(Rect: TRect); virtual;
    function GetBorderSize: Integer; virtual;
    procedure SetColorArrayItem(aRow: Integer; aCol: integer; withColor: TColor);
    procedure SetColorArrayItemsRange(fromRow, toRow, fromCol, toCol: Integer; withColor: TColor);
    procedure SetColorArrayWithDefaultColor(fromRow, fromCol: Integer);
    property Background: TcyGradient read FBackground write SetBackground;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Canvas: TCanvas read GetCanvas;
    property CellHeight: integer read FCellHeight write SetCellHeight;
    property CellWidth: integer read FCellWidth write SetCellWidth;
    property ColCount: integer read FColCount write SetColCount default 30;
    property CellFrameColor: TColor read FCellFrameColor write SetCellFrameColor;
    property CellFrameWidth: integer read FCellFrameWidth write SetCellFrameWidth default 0;
    property CellSpacingWidth: integer read FCellSpacingWidth write SetCellSpacingWidth default 1;
    property CellSpacingHeight: integer read FCellSpacingHeight write SetCellSpacingHeight default 1;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor;
    property TopRowValue: Double read FTopRowValue write FTopRowValue;
    property LeftColumnValue: Double read FLeftColumnValue write FLeftColumnValue;
    property BottomRowValue: Double read FBottomRowValue write FBottomRowValue;
    property RightColumnValue: Double read FRightColumnValue write FRightColumnValue;
    property RowCount: integer read FRowCount write SetRowCount default 30;
    property OnCustomDrawCell: TProcOnDrawCell read FOnCustomDrawCell write FOnCustomDrawCell;
    property OnCellClick: TProcOnCellClick read FOnCellClick write FOnCellClick;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DrawText(aText: String; fromRow, fromCol, toRow, toCol: Integer; TextFormat: LongInt);
    procedure DrawCanvas(StartRow, StartColumn: Integer; Canvas: TCanvas; CanvasRect: TRect; TransparentColor: TColor; Transparent: Boolean);
    procedure DrawGraphic(StartRow, StartColumn: Integer; Graphic: TGraphic; TransparentColor: TColor; Transparent: Boolean);
    procedure LoadFromBitmap(Bitmap: TBitmap);
    procedure SaveToBitmap(Bitmap: TBitmap);
    function  FindCellColor(fromRow, toRow, fromCol, toCol: Integer; aColor: TColor; var aRow: Integer; var aCol: Integer): Boolean;
    function  GetCellAtPos(aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;
    function  GetColumnPosition(aCol: Integer): Integer;
    function  GetRowPosition(aRow: Integer): Integer;
    function  GetColorGrid(aRow: Integer; aCol: integer): TColor; virtual;
    procedure DrawCell(CellRect: TRect; CellColor: TColor; Row, Column: Integer; CustomDrawCell: Boolean);
    procedure DefaultDrawCell(aRect: TRect; aRow, aCol: integer; aColor: TColor);
    procedure ReplaceColor(old, New: TColor); virtual;
    procedure SetColorGrid(aRow: Integer; aCol: integer; withColor: TColor);
    procedure SetColorGridRange(fromRow, toRow, fromCol, toCol: Integer; withColor: TColor);
    function RowToValue(Row: Integer): Double;
    function ValueToRow(Value: Double): Integer;
    function ColumnToValue(Column: Integer): Double;
    function ValueToColumn(Value: Double): Integer;
  published
  end;

implementation

{ TcyBaseColorMatrix }

function TcyBaseColorMatrix.GetBorderSize: Integer;
begin
  RESULT := FBorderWidth;
end;

function TcyBaseColorMatrix.CalcWidth: Integer;
begin
  RESULT := GetBorderSize * 2 + FColCount * FCellWidth + ((FColCount - 1) * FCellSpacingWidth);
end;

function TcyBaseColorMatrix.CalcHeight: Integer;
begin
  RESULT := GetBorderSize * 2 + FRowCount * FCellHeight + ((FRowCount - 1) * FCellSpacingHeight);
end;

procedure TcyBaseColorMatrix.SetColorArrayWithDefaultColor(fromRow, fromCol: Integer);
var r, c: Integer;
begin
  for c := fromCol to FColCount - 1 do
    for r := fromRow to FRowCount - 1 do
      FColorArray[c, r] := FDefaultColor;
end;

procedure TcyBaseColorMatrix.SetColorArrayItem(aRow: Integer; aCol: integer; withColor: TColor);
begin
  if FColorArray[aCol, aRow] = withColor then Exit;
  FColorArray[aCol, aRow] := withColor;
end;

procedure TcyBaseColorMatrix.SetColorArrayItemsRange(fromRow, toRow, fromCol, toCol: Integer; withColor: TColor);
var r, c: Integer;
begin
  for c := fromCol to toCol do
    for r := fromRow to toRow do
      if FColorArray[c, r] <> withColor
      then FColorArray[c, r] := withColor;
end;

constructor TcyBaseColorMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];  // Reduce flickering when we update lot of time per second ...
  FUpdateCount := 0;
  FDisplay := TBitmap.Create;
  FDisplay.OnChange := BitmapChanged;
  FDisplay.PixelFormat := pf32bit;
  FBackground := TcyGradient.Create(self);
  FBackground.FromColor := clBlack;
  FBackground.ToColor := clBlack;
  FBackground.OnChange := BackgroundChanged;
  FTopRowValue := 0;
  FLeftColumnValue := 0;
  FBottomRowValue := 0;
  FRightColumnValue := 0;
  FDefaultColor := clGreen;
  FBorderWidth := 1;
  FCellHeight := 5;
  FCellWidth := 5;
  FRowCount := 30;
  FColCount := 30;
  FCellSpacingWidth := 1;
  FCellSpacingHeight := 1;
  FCellFrameWidth := 0;
  FCellFrameColor := clGray;
  SetLength(FColorArray, FColCount, FRowCount);
  SetColorArrayWithDefaultColor(0, 0);
  Autosize:= true;
end;

procedure TcyBaseColorMatrix.Loaded;
begin
  inherited;
  FDisplay.Width := Width;
  FDisplay.Height := Height;
  DrawMatrix;
end;

procedure TcyBaseColorMatrix.BitmapChanged(Sender: TObject);
begin
  if FUpdateCount <> 0 then Exit;
  Invalidate;
end;

procedure TcyBaseColorMatrix.Paint;
begin
   Inherited Canvas.Draw(0, 0, FDisplay);

  if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

destructor TcyBaseColorMatrix.Destroy;
begin
  FBackground.Free;
  FDisplay.Free;
  inherited Destroy;
end;

procedure TcyBaseColorMatrix.AdjustSize;
begin
  if csLoading in ComponentState then Exit;

  SetBounds(Left, Top, CalcWidth, CalcHeight);
  FDisplay.Width := Width;
  FDisplay.Height := Height;
  DrawMatrix;
end;

function TcyBaseColorMatrix.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;

  if not (csLoading in ComponentState)
  then begin
    NewWidth := CalcWidth;
    NewHeight := CalcHeight;
  end;
end;

procedure TcyBaseColorMatrix.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FptMouseDown := Point(X, Y);
  inherited;
end;

procedure TcyBaseColorMatrix.Click;
var
  aRow, aCol: integer;
begin
  if Assigned(FOnCellClick)
  then
    if GetCellAtPos(FptMouseDown, aRow, aCol, true)
    then FOnCellClick(Self, aRow, aCol, FColorArray[aCol, aRow]);

  Inherited;
end;

procedure TcyBaseColorMatrix.SetBackground(const Value: TcyGradient);
begin
  FBackground.Assign(Value);
end;

procedure TcyBaseColorMatrix.BackgroundChanged(Sender: TObject);
begin
  DrawMatrix;
end;

procedure TcyBaseColorMatrix.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth = Value then Exit;
  FBorderWidth := Value;
  AdjustSize;
end;

procedure TcyBaseColorMatrix.BeginUpdate;
begin
  Inc(FUpdateCount, 1);
end;

procedure TcyBaseColorMatrix.SetColCount(const Value: integer);
var
  oldCols: Integer;
begin
  if FColCount = Value then Exit;
  oldCols := FColCount;
  FColCount := Value;
  SetLength(FColorArray, FColCount, FRowCount);
  SetColorArrayWithDefaultColor(0, oldCols);
  AdjustSize;
end;

procedure TcyBaseColorMatrix.SetRowCount(const Value: integer);
var oldRows: Integer;
begin
  if FRowCount = Value then Exit;
  oldRows := FRowCount;
  FRowCount := Value;
  SetLength(FColorArray, FColCount, FRowCount);
  SetColorArrayWithDefaultColor(oldRows, 0);
  AdjustSize;
end;

procedure TcyBaseColorMatrix.SetCellFrameColor(const Value: TColor);
begin
  if FCellFrameColor = Value then Exit;
  FCellFrameColor := Value;
  if FCellFrameWidth <> 0 then
    DrawMatrix;
end;

procedure TcyBaseColorMatrix.SetCellFrameWidth(const Value: integer);
begin
  if FCellFrameWidth = Value then Exit;
  FCellFrameWidth := Value;
  DrawMatrix;
end;

procedure TcyBaseColorMatrix.SetCellHeight(const Value: integer);
begin
  if FCellHeight = Value then Exit;
  FCellHeight := Value;
  AdjustSize;
end;

procedure TcyBaseColorMatrix.SetCellSpacingWidth(const Value: integer);
begin
  if FCellSpacingWidth = Value then Exit;
  FCellSpacingWidth := Value;
  AdjustSize;
end;

procedure TcyBaseColorMatrix.SetCellSpacingHeight(const Value: integer);
begin
  if FCellSpacingHeight = Value then Exit;
  FCellSpacingHeight := Value;
  AdjustSize;
end;

procedure TcyBaseColorMatrix.SetCellWidth(const Value: integer);
begin
  if FCellWidth = Value then Exit;
  FCellWidth := Value;
  AdjustSize;
end;

procedure TcyBaseColorMatrix.SetDefaultColor(const Value: TColor);
begin
  if FDefaultColor = Value then Exit;
  FDefaultColor := Value;
  SetColorArrayItemsRange(0, RowCount-1, 0, ColCount-1, Value);
  DrawMatrix;
end;

function TcyBaseColorMatrix.GetColumnPosition(aCol: Integer): Integer;
begin
  RESULT := GetBorderSize + (FCellWidth + FCellSpacingWidth) * aCol;
end;

function TcyBaseColorMatrix.GetRowPosition(aRow: Integer): Integer;
begin
  RESULT := GetBorderSize + (FCellHeight + FCellSpacingHeight) * aRow;
end;

function TcyBaseColorMatrix.GetClientRect: TRect;
var BorderSize: Integer;
begin
  BorderSize := GetBorderSize;
  RESULT := classes.Rect(BorderSize, BorderSize, Width - BorderSize, Height - BorderSize);
end;

function TcyBaseColorMatrix.GetCanvas: TCanvas;
begin
  // Result := FDisplay.Canvas;
  Result := Inherited Canvas;
end;

function  TcyBaseColorMatrix.GetCellAtPos(aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;
var
  CellLeft, CellTop: Integer;

    function CheckBounds: Boolean;
    begin
      RESULT := false;

      if (aCol >= 0) and (aCol < FColCount)
      then
        if (aRow >= 0) and (aRow < FRowCount)
        then RESULT := true;
    end;

begin
  RESULT := false;

  // Determine ACol and ARow :
  aCol := (aPoint.X - GetBorderSize) div (FCellWidth + FCellSpacingWidth);
  aRow := (aPoint.Y - GetBorderSize) div (FCellHeight + FCellSpacingHeight);

  if ExactPos
  then begin
    CellLeft := GetColumnPosition(aCol);
    CellTop := GetRowPosition(aRow);

    if aPoint.X >= CellLeft
    then
      if aPoint.X <= CellLeft + FCellWidth
      then
        if aPoint.Y >= CellTop
        then
          if aPoint.Y <= CellTop + FCellHeight
          then RESULT := CheckBounds;
  end
  else
    RESULT := CheckBounds;
end;

function  TcyBaseColorMatrix.GetColorGrid(aRow: Integer; aCol: integer): TColor;
begin
  RESULT := FColorArray[aCol, aRow];
end;

procedure TcyBaseColorMatrix.SetColorGrid(aRow: Integer; aCol: integer; withColor: TColor);
var
  CellRect: TRect;
  aLeft, aTop: Integer;
begin
  if FColorArray[aCol, aRow] = withColor then Exit;
  FColorArray[aCol, aRow] := withColor;

  aLeft := GetColumnPosition(aCol);
  aTop := GetRowPosition(aRow);
  CellRect := classes.Rect(aLeft, aTop, aLeft + FCellWidth, aTop + FCellHeight);
  DrawCell(CellRect, withColor, aRow, aCol, Assigned(FOnCustomDrawCell));
end;

procedure TcyBaseColorMatrix.SetColorGridRange(fromRow, toRow, fromCol, toCol: Integer; withColor: TColor);
var r, c: Integer;
begin
  for c := fromCol to toCol do
    for r := fromRow to toRow do
      SetColorGrid(r, c, withColor);
end;

procedure TcyBaseColorMatrix.ReplaceColor(old, New: TColor);
var
  CellRect: TRect;
  r, c: Integer;
  aLeft, aTop: Integer;
  CustomDrawAssigned: Boolean;
begin
  CustomDrawAssigned := Assigned(FOnCustomDrawCell);
  for r := 0 to FRowCount - 1 do
  begin
    aTop := -1;      // Need to be recalculated

    for c := 0 to FColCount - 1 do
      if FColorArray[c, r] = Old
      then begin
        if aTop = -1
        then aTop := GetRowPosition(r);
        aLeft := GetColumnPosition(c);

        FColorArray[c, r] := New;
        CellRect := classes.Rect(aLeft, aTop, aLeft + FCellWidth, aTop + FCellHeight);
        DrawCell(CellRect, New, r, c, CustomDrawAssigned);
      end;
  end;
end;

procedure TcyBaseColorMatrix.EndUpdate;
begin
  Dec(FUpdateCount, 1);
  if FUpdateCount = 0 then
    BitmapChanged(FDisplay);
end;

function TcyBaseColorMatrix.FindCellColor(fromRow, toRow, fromCol, toCol: Integer; aColor: TColor; var aRow, aCol: Integer): Boolean;
var r, c: Integer;
label found;
begin
  RESULT := false;

  for r := fromRow to toRow do
    for c := fromCol to toCol do
      if FColorArray[c, r] = aColor
      then begin
        RESULT := true;
        aRow := r;
        aCol := c;
        goto Found;
      end;

  found:
end;

function TcyBaseColorMatrix.RowToValue(Row: Integer): Double;
var StepValue: Double;
begin
  StepValue := (BottomRowValue - TopRowValue) / (RowCount-1);
  RESULT := TopRowValue + StepValue * Row;
end;

function TcyBaseColorMatrix.ValueToRow(Value: Double): Integer;
var StepValue: Double;
begin
  StepValue := (BottomRowValue - TopRowValue) / (RowCount - 1);
  RESULT := Round( (Value - TopRowValue) / Stepvalue );
end;

function TcyBaseColorMatrix.ColumnToValue(Column: Integer): Double;
var StepValue: Double;
begin
  StepValue := (RightColumnValue - LeftColumnValue) / (ColCount-1);
  RESULT := LeftColumnValue + StepValue * Column;
end;

function TcyBaseColorMatrix.ValueToColumn(Value: Double): Integer;
var StepValue: Double;
begin
  StepValue := (RightColumnValue - LeftColumnValue) / (ColCount - 1);
  RESULT := Round( (Value - LeftColumnValue) / Stepvalue );
end;

procedure TcyBaseColorMatrix.DrawBackground(Rect: TRect);
begin
  // Draw background:
  FBackground.Draw(FDisplay.Canvas, Rect);
end;

procedure TcyBaseColorMatrix.DrawMatrix;
var
  r, c, xPos, yPos: integer;
  CurColor: TColor;
  CellRect: TRect;
  CustomDrawCell: Boolean;
begin
  if csLoading in ComponentState then Exit;

  BeginUpdate;
  DrawBackground(classes.Rect(0, 0, Width, Height));
  CustomDrawCell := Assigned(FOnCustomDrawCell);
  yPos := GetBorderSize;

  for r := 0 to FRowCount -1 do
  begin
    xPos := GetBorderSize;

    for c := 0 to FColCount -1 do
    begin
      // Draw cell:
      CurColor := FColorArray[c, r];
      CellRect := classes.Rect(xPos, yPos, xPos + FCellWidth, yPos + FCellHeight);

      // Draw frame cell:
      if not CustomDrawCell
      then begin
        FDisplay.Canvas.Brush.Color := FCellFrameColor;
        FDisplay.Canvas.FillRect(CellRect);
      end;

      DrawCell(CellRect, CurColor, r, c, CustomDrawCell);
      inc(xPos, FCellWidth + FCellSpacingWidth);
    end;

    inc(yPos, FCellHeight + FCellSpacingHeight);
  end;
  EndUpdate;
end;

procedure TcyBaseColorMatrix.DrawCell(CellRect: TRect; CellColor: TColor; Row, Column: Integer; CustomDrawCell: Boolean);
begin
  if CustomDrawCell
  then FOnCustomDrawCell(self, CellRect, Row, Column, CellColor)
  else DefaultDrawCell(CellRect, Row, Column, CellColor);
end;

procedure TcyBaseColorMatrix.DefaultDrawCell(aRect: TRect; aRow, aCol: integer; aColor: TColor);
begin
  // Frame has already been painted by DrawMatrix procedure :
  with FDisplay.Canvas do
  begin
    if FCellFrameWidth > 0
    then InflateRect(aRect, -FCellFrameWidth, -FCellFrameWidth);

    if aRect.Bottom-aRect.Top = 1
    then begin
      if aRect.Right-aRect.Left = 1
      then begin
        // Draw a point :
        Pixels[aRect.Left, aRect.Top] := aColor;
      end
      else begin
        // Draw horizontal line :
        Pen.Color := aColor;
        MoveTo(aRect.Left, aRect.Top);
        LineTo(aRect.Right, aRect.Top);
      end;
    end
    else begin
      if aRect.Right-aRect.Left = 1
      then begin
        // Draw vertical line :
        Pen.Color := aColor;
        MoveTo(aRect.Left, aRect.Top);
        LineTo(aRect.Left, aRect.Bottom);
      end
      else begin
        Brush.Color := aColor;
        FillRect(aRect);
      end;
    end;
  end;
end;

procedure TcyBaseColorMatrix.DrawCanvas(StartRow, StartColumn: Integer; Canvas: TCanvas; CanvasRect: TRect; TransparentColor: TColor; Transparent: Boolean);
var
  r, c, BmpR, BmpC, EndRow, EndColumn: Integer;
  Bmp: TBitmap;
  BmpColor: TColor;
  RowBytes: pRGBQuadArray;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.Width := CanvasRect.Right - CanvasRect.Left;
  Bmp.Height := CanvasRect.Bottom - CanvasRect.Top;
  Bmp.Canvas.CopyRect(classes.Rect(0, 0, Bmp.Width, Bmp.Height), Canvas, CanvasRect);

  EndColumn := StartColumn + Bmp.Width - 1;
  if EndColumn > ColCount-1
  then EndColumn := ColCount-1;
  EndRow := StartRow + Bmp.Height - 1;
  if EndRow > RowCount-1
  then EndRow := RowCount-1;

  BeginUpdate;
  for r := StartRow to EndRow do
  begin
    BmpR := r-StartRow;
    RowBytes := Bmp.ScanLine[BmpR];

    for c := StartColumn to EndColumn do
    begin
      BmpC := c-StartColumn;
      BmpColor := RGB(RowBytes[BmpC].rgbRed, RowBytes[BmpC].rgbGreen, RowBytes[BmpC].rgbBlue);
      if (not Transparent) or (BmpColor <> TransparentColor) then
        SetColorGrid( r, c, BmpColor);
    end;
  end;
  EndUpdate;

  Bmp.Free;
end;

procedure TcyBaseColorMatrix.DrawGraphic(StartRow, StartColumn: Integer; Graphic: TGraphic; TransparentColor: TColor; Transparent: Boolean);
var
  r, c, BmpR, BmpC, EndRow, EndColumn: Integer;
  Bmp: TBitmap;
  BmpColor: TColor;
  RowBytes: pRGBQuadArray;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.Width := Graphic.Width;
  Bmp.Height := Graphic.Height;
  Bmp.Canvas.Draw(0, 0, Graphic);

  EndColumn := StartColumn + Bmp.Width - 1;
  if EndColumn > ColCount - 1
  then EndColumn := ColCount - 1;
  EndRow := StartRow + Bmp.Height - 1;
  if EndRow > RowCount-1
  then EndRow := RowCount-1;

  BeginUpdate;
  for r := StartRow to EndRow do
  begin
    BmpR := r-StartRow;
    RowBytes := Bmp.ScanLine[BmpR];

    for c := StartColumn to EndColumn do
    begin
      BmpC := c-StartColumn;
      BmpColor := RGB(RowBytes[BmpC].rgbRed, RowBytes[BmpC].rgbGreen, RowBytes[BmpC].rgbBlue);
      if (not Transparent) or (BmpColor <> TransparentColor) then
        SetColorGrid( r, c, BmpColor);
    end;
  end;
  EndUpdate;

  Bmp.Free;
end;

procedure TcyBaseColorMatrix.LoadFromBitmap(Bitmap: TBitmap);
begin
  DrawGraphic(0, 0, Bitmap, clNone, false);
end;

procedure TcyBaseColorMatrix.SaveToBitmap(Bitmap: TBitmap);
var
  r, c: Integer;
  RowBytes: pRGBQuadArray;
  RGBValue: Integer;
begin
  if Bitmap.PixelFormat <> pf32bit then
    Bitmap.PixelFormat := pf32bit;
  if Bitmap.Width <> ColCount then
    Bitmap.Width := ColCount;
  if Bitmap.Height <> RowCount then
    Bitmap.Height := RowCount;

  for r := 0 to FRowCount - 1 do
  begin
    RowBytes := Bitmap.ScanLine[r];

    for c := 0 to FColCount - 1 do
    begin
      RGBValue := ColorToRGB(FColorArray[c, r]);
      RowBytes[c].rgbRed   := GetRValue(RGBValue);
      RowBytes[c].rgbGreen := GetGValue(RGBValue);
      RowBytes[c].rgbBlue  := GetBValue(RGBValue);
    end;
  end;

  Bitmap.Modified := true;
end;

procedure TcyBaseColorMatrix.DrawText(aText: String; fromRow, fromCol, toRow, toCol: Integer; TextFormat: LongInt);
var
  aBitmap: TBitmap;
  aRect: TRect;
begin
  aBitmap := TBitmap.Create;
  SaveToBitmap(aBitmap);
  aRect := classes.Rect(fromRow, fromCol, toCol, toRow); // One row and one column is one pixel ...

  with aBitmap.Canvas do
  begin
    Brush.Assign(Self.Canvas.Brush);
    Font.Assign(Self.Canvas.Font);
    cyDrawText(Handle, aText, aRect, TextFormat);
  end;

  DrawGraphic(0, 0, aBitmap, clNone, false);
  aBitmap.Free;
end;

end.
