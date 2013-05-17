{   Unit VCL.cyDmmCanvas

    Description:
    Unit with Canvas functions.

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

unit VCL.cyDmmCanvas;

interface

uses windows, Graphics;

type
  TcyDMMCanvas=class(Tcanvas)
  private
    function  DMMGetPixel(X, Y: Integer): TColor;
    procedure DMMSetPixel(X, Y: Integer; Value: TColor);
    function  DMMGetFontHeight: Integer;
    procedure DMMSetFontHeight(Value: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    // Fonctions de conversion en 10ème de milimètres :
    function  PxToDmm_X(x:integer): integer;
    function  PxToDmm_Y(y:integer): integer;
    
    // Fonctions de conversion en pixels :
    function  DmmToPx_X(x:integer): integer;
    function  DmmToPx_Y(y:integer): integer;
    function  DmmToPx_Point(pt:TPoint): TPoint;
    function  DmmToPx_Rect(rect:trect): TRect;

    // Fonctions de dessin en 10ème de milimètres :
    procedure DMMArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure DMMBrushCopy(Const Dest: TRect; Bitmap: TBitmap; Const Source: TRect; Color: TColor);
    procedure DMMChord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure DMMCopyRect(Const Dest: TRect; Canvas: TCanvas;Const Source: TRect);
    procedure DMMDraw(X, Y: Integer; Graphic: TGraphic);
    procedure DMMDrawFocusRect(const Rect: TRect);
    procedure DMMEllipse(X1, Y1, X2, Y2: Integer); overload;
    procedure DMMEllipse(const Rect: TRect); overload;
    procedure DMMFillRect(const Rect: TRect);
    procedure DMMFloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle);
    procedure DMMFrameRect(const Rect: TRect);
    procedure DMMLineTo(X, Y: Integer);
    procedure DMMMoveTo(X, Y: Integer);
    procedure DMMPie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure DMMPolygon(const Points: array of TPoint);
    procedure DMMPolyline(const Points: array of TPoint);
    procedure DMMPolyBezier(const Points: array of TPoint);
    procedure DMMPolyBezierTo(const Points: array of TPoint);
    procedure DMMRectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure DMMRectangle(const Rect: TRect); overload;
    procedure DMMRoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
    procedure DMMStretchDraw(const Rect: TRect; Graphic: TGraphic);
    function  DMMTextExtent(const Text: string): TSize;    // Largeur et hauteur en DMM du texte dans la police actuelle
    function  DMMTextHeight(const Text: string): Integer;
    procedure DMMTextOut(X, Y: Integer; const Text: string);
    procedure DMMTextRect(Rect: TRect; X, Y: Integer; const Text: string);
    function  DMMTextWidth(const Text: string): Integer;
    property  DMMFontHeight: Integer read DMMGetFontHeight write DMMSetFontHeight;
    property  DMMPixels[X, Y: Integer]: TColor read DMMGetPixel write DMMSetPixel;
  published

  end;



implementation


constructor TcyDMMCanvas.Create;
begin
  inherited Create;
end;

destructor TcyDMMCanvas.Destroy;
begin
  inherited Destroy;
end;

// Convertir en 10ème de milimètres ...
function TcyDMMCanvas.PxToDmm_X(x: integer):integer;
begin
  result := GetDeviceCaps(Handle, LOGPIXELSX);
  result := x * 254 div result;
end;

// Convertir en 10ème de milimètres ...
function TcyDMMCanvas.PxToDmm_Y(y: integer):integer;
begin
  result := GetDeviceCaps(Handle, LOGPIXELSY);
  result := y * 254 div result;
end;

// Convertir en pixels ...
function TcyDMMCanvas.DmmToPx_X(x: integer):integer;
begin
  result := GetDeviceCaps(Handle, LOGPIXELSX);  // Récupérer le nombre de points par pouce du Canvas ...
  result := x * result div 254;
end;

// Convertir en pixels ...
function TcyDMMCanvas.DmmToPx_Y(y: integer):integer;
begin
  result := GetDeviceCaps(Handle, LOGPIXELSY);
  result := y * result div 254;
end;

// Convertir en pixels :
function TcyDMMCanvas.DmmToPx_Point(pt: TPoint):TPoint;
begin
  result.x := GetDeviceCaps(Handle, LOGPIXELSX);
  result.y := GetDeviceCaps(Handle, LOGPIXELSY);
  result.x := pt.x * result.x div 254;
  result.y := pt.y * result.y div 254;
end;

function TcyDMMCanvas.DmmToPx_Rect(rect: TRect):TRect;
var lpx, lpy:integer;
begin
  lpx := GetDeviceCaps(Handle, LOGPIXELSX);
  lpy := GetDeviceCaps(Handle, LOGPIXELSY);

  result.Left := rect.Left * lpx div 254;
  result.Top := rect.Top * lpy div 254;
  result.right := rect.right * lpx div 254;
  result.bottom := rect.bottom * lpy div 254;
end;

procedure TcyDMMCanvas.DMMArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  x1 := DmmToPx_X(x1);  y1 := DmmToPx_Y(y1);
  x2 := DmmToPx_X(x2);  y2 := DmmToPx_Y(y2);
  x3 := DmmToPx_X(x3);  y3 := DmmToPx_Y(y3);
  x4 := DmmToPx_X(x4);  y4 := DmmToPx_Y(y4);
  Arc( X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TcyDMMCanvas.DMMBrushCopy(Const Dest: TRect; Bitmap: TBitmap; Const Source: TRect; Color: TColor);
begin
  BrushCopy(DmmToPx_Rect(dest), Bitmap, DmmToPx_Rect(source), Color);
end;

procedure TcyDMMCanvas.DMMChord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  x1 := DmmToPx_X(x1);  y1 := DmmToPx_Y(y1);
  x2 := DmmToPx_X(x2);  y2 := DmmToPx_Y(y2);
  x3 := DmmToPx_X(x3);  y3 := DmmToPx_Y(y3);
  x4 := DmmToPx_X(x4);  y4 := DmmToPx_Y(y4);
  Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TcyDMMCanvas.DMMCopyRect(Const Dest: TRect; Canvas: TCanvas;Const Source: TRect);
begin
  CopyRect(DmmToPx_Rect(dest), Canvas, DmmToPx_Rect(source));
end;

procedure TcyDMMCanvas.DMMDraw(X, Y: Integer; Graphic: TGraphic);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  Draw(X, Y, Graphic);
end;

procedure TcyDMMCanvas.DMMDrawFocusRect(const Rect: TRect);
begin
  DrawFocusRect(DmmToPx_Rect(Rect));
end;

procedure TcyDMMCanvas.DMMEllipse(X1, Y1, X2, Y2: Integer);
begin
  x1 := DmmToPx_X(x1);  y1 := DmmToPx_Y(y1);
  x2 := DmmToPx_X(x2);  y2 := DmmToPx_Y(y2);
  Ellipse( X1, Y1, X2, Y2);
end;

procedure TcyDMMCanvas.DMMEllipse(const Rect: TRect);
begin
  DMMEllipse(rect.Left, rect.Top, rect.Right, rect.Bottom);
end;

procedure TcyDMMCanvas.DMMFillRect(const Rect: TRect);
begin
  FillRect(DmmToPx_Rect(Rect));
end;

procedure TcyDMMCanvas.DMMFloodFill(X, Y: Integer; Color: TColor;
  FillStyle: TFillStyle);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  FloodFill(X, Y, Color, FillStyle);
end;

procedure TcyDMMCanvas.DMMFrameRect(const Rect: TRect);
begin
  FrameRect(DmmToPx_Rect(Rect));
end;


procedure TcyDMMCanvas.DMMLineTo(X, Y: Integer);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  LineTo(X, Y);
end;


procedure TcyDMMCanvas.DMMMoveTo(X, Y: Integer);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  MoveTo(X, Y);
end;

procedure TcyDMMCanvas.DMMPie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  x1 := DmmToPx_X(x1);  y1 := DmmToPx_Y(y1);
  x2 := DmmToPx_X(x2);  y2 := DmmToPx_Y(y2);
  x3 := DmmToPx_X(x3);  y3 := DmmToPx_Y(y3);
  x4 := DmmToPx_X(x4);  y4 := DmmToPx_Y(y4);
  Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;


procedure TcyDMMCanvas.DMMPolygon(const Points: array of TPoint);
var
  i:integer;
  pt:array of tpoint;
begin
  setlength(pt, high(points) + 1);

  for i := 0 to high(points) do
    pt[i] := DmmToPx_Point(points[i]);

  Polygon(pt);
end;

procedure TcyDMMCanvas.DMMPolyline(const Points: array of TPoint);
var
  i:integer;
  pt:array of tpoint;
begin
  setlength(pt, high(points)+1);

  for i := 0 to high(points) do
    pt[i] := DmmToPx_Point(points[i]);

  Polyline(pt);
end;

procedure TcyDMMCanvas.DMMPolyBezier(const Points: array of TPoint);
var
  i:integer;
  pt:array of tpoint;
begin
  setlength(pt, high(points) + 1);

  for i := 0 to high(points) do
    pt[i] := DmmToPx_Point(points[i]);
    
  PolyBezier(pt);
end;

procedure TcyDMMCanvas.DMMPolyBezierTo(const Points: array of TPoint);
var
  i:integer;
  pt:array of tpoint;
begin
  setlength(pt, high(points) + 1);

  for i := 0 to high(points) do
    pt[i] := DmmToPx_Point(points[i]);

  PolyBezierTo(pt);
end;

procedure TcyDMMCanvas.DMMRectangle(X1, Y1, X2, Y2: Integer);
begin
  x1 := DmmToPx_X(x1);  y1 := DmmToPx_Y(y1);
  x2 := DmmToPx_X(x2);  y2 := DmmToPx_Y(y2);
  Rectangle(X1, Y1, X2, Y2);
end;

procedure TcyDMMCanvas.DMMRectangle(const Rect: TRect);
begin
  DMMRectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TcyDMMCanvas.DMMRoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  x1 := DmmToPx_X(x1);  y1 := DmmToPx_Y(y1);
  x2 := DmmToPx_X(x2);  y2 := DmmToPx_Y(y2);
  x3 := DmmToPx_X(x3);  y3 := DmmToPx_Y(y3);
  RoundRect(X1, Y1, X2, Y2, X3, Y3);
end;

procedure TcyDMMCanvas.DMMStretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
  StretchDraw(DmmToPx_Rect(Rect), Graphic);
end;

procedure TcyDMMCanvas.DMMTextOut(X, Y: Integer; const Text: String);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  TextOut(X, Y, Text);
end;

procedure TcyDMMCanvas.DMMTextRect(Rect: TRect; X, Y: Integer; const Text: string);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  TextRect(DmmToPx_Rect(Rect), X, Y, Text);
end;

function TcyDMMCanvas.DMMTextExtent(const Text: string): TSize;
begin
  result := TextExtent(Text);
  result.cx := PxToDmm_X(result.cx);
  result.cy := PxToDmm_Y(result.cy);
end;

function TcyDMMCanvas.DMMTextWidth(const Text: string): Integer;
begin
  result := TextExtent(Text).cX;
  result := PxToDmm_X(result);
end;

function TcyDMMCanvas.DMMTextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
  Result := PxToDmm_Y(result);
end;

function TcyDMMCanvas.DMMGetPixel(X, Y: Integer): TColor;
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  result := pixels[x,y];
end;

procedure TcyDMMCanvas.DMMSetPixel(X, Y: Integer; Value: TColor);
begin
  x := DmmToPx_X(x);  y := DmmToPx_Y(y);
  pixels[x,y] := value;
end;

function TcyDMMCanvas.DMMGetFontHeight: Integer;
begin
  result := Font.Height * 254 div Font.PixelsPerInch;
end;

procedure TcyDMMCanvas.DMMSetFontHeight(Value: Integer);
begin
  Font.Height := Value * Font.PixelsPerInch div 254;
end;

end.
