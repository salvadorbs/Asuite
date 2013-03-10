{ @abstract(This unit contains advanced graphic functions used by KControls suite.)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(5 May 2004)
  @lastmod(20 Jun 2010)

  Copyright © 2004 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KGraphics;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
 // use the LCL interface support whenever possible
 {$IFDEF USE_WINAPI}
  Windows,
 {$ENDIF}
  GraphType, IntfGraphics, LCLType, LCLIntf, LMessages, LResources,
{$ELSE}
  Windows, Messages,
 {$IFDEF USE_PNG_SUPPORT}
  PngImage,
 {$ENDIF}
{$ENDIF}
  Classes, Forms, Graphics, Controls, KFunctions;

resourcestring
  { @exclude }
  SGDIError = 'GDI object could not be created.';

const
  { PNG Support }
  PNGHeader = #137'PNG'#13#10#26#10;
  MNGHeader = #138'MNG'#13#10#26#10;

type
  { Declares possible values for the Style parameter of the @link(BrightColor) function. }
  TKBrightMode = (
    { The Color will be brightened with Percent of its entire luminosity range. }
    bsAbsolute,
    { The Color will be brightened with Percent of its current luminosity value. }
    bsOfBottom,
    { The Color will be brightened with Percent of the difference of its entire
      luminosity range and current luminosity value. }
    bsOfTop
  );

  { Declares RGB + Alpha channel color description allowing both to
    access single channels and the whole color item. }
  TKColorRec = packed record
    case Integer of
      0: (R, G, B, A: Byte);
      1: (Value: Cardinal);
  end;

  { Pointer to TKColorRec. }
  PKColorRec = ^TKColorRec;

  { Dynamic array for TKColorRec. }
  TKColorRecs = array[0..MaxInt div SizeOf(TKColorRec) - 1] of TKColorRec;
  { Dynamic array for TKColorRecs. }
  PKColorRecs = ^TKColorRecs;
  { Dynamic array for TKColorRec. }
  TKDynColorRecs = array of TKColorRec;

  { String type for @link(ImageByType) function. }
  TKImageHeaderString = string[10];

{$IFDEF USE_PNG_SUPPORT}
 {$IFDEF FPC}
   { @exclude }
  TKPngImage = TPortableNetworkGraphic;
 {$ELSE}
  {$IFDEF COMPILER12_UP}
   { @exclude }
  TKPngImage = TPngImage;
  {$ELSE}
   { @exclude }
  TKPngImage = TPngObject;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

  { Declares possible values for the Attributes parameter in the @link(DrawAlignedText) function. }
  TKTextAttribute = (
    { Bounding rectangle is calculated. No text is drawn. }
    taCalcRect,
    { Text will be clipped within the given rectangle. }
    taClip,
    { Text will be drawn with end ellipsis if it does not fit within given width. }
    taEndEllipsis,
    { Given rectangle will be filled. }
    taFillRect,
    { Only yhe text within given rectangle will be filled. }
    taFillText,
    { Text will be drawn as multi-line text if it contains carriage returns and line feeds. }
    taLineBreak,
    { Text will be drawn with path ellipsis if it does not fit within given width. }
    taPathEllipsis,
    { Text line(s) will be broken between words if they don't fit within given width. }
    taWordBreak,
    { Text line(s) will be broken if they don't fit within col width. }
    taWrapText, //JR:20091229
    { No white spaces will be trimmed at the beginning or end of text lines. }
    taTrimWhiteSpaces
  );

  { Set type for @link(TKTextAttribute) enumeration. }
  TKTextAttributes = set of TKTextAttribute;

  { Declares possible values for the HAlign parameter in the @link(DrawAlignedText) function. }
  TKHAlign = (
    { Text is aligned to the left border of a cell rectangle. }
    halLeft,
    { Text is horizontally centered within the cell rectangle. }
    halCenter,
    { Text is aligned to the right border of a cell rectangle. }
    halRight
  );

  { Declares possible values for the StretchMode parameter in the @link(ExcludeShapeFromBaseRect) function. }
  TKStretchMode = (
    { Shape is not stretched. }
    stmNone,
    { Shape is zoomed out. }
    stmZoomOutOnly,
    { Shape is zoomed in. }
    stmZoomInOnly,
    { Shape is zoomed arbitrary. }
    stmZoom
  );

  { For backward compatibility. }
  TKTextHAlign = TKHAlign;

  { Declares possible values for the VAlign parameter in the @link(DrawAlignedText) function. }
  TKVAlign = (
    { Text is aligned to the upper border of a cell rectangle. }
    valTop,
    { Text is vertically centered within the cell rectangle. }
    valCenter,
    { Text is aligned to the lower border of a cell rectangle. }
    valBottom
  );

  { For backward compatibility. }
  TKTextVAlign = TKVAlign;

  { A simple platform independent encapsulation for a 32bpp bitmap with
    alpha channel with the ability to modify it's pixels directly. }
  TKAlphaBitmap = class(TGraphic)
  private
    FCanvas: TCanvas;
    FDirectCopy: Boolean;
    FHandle: HBITMAP;
    FHeight: Integer;
  {$IFNDEF USE_WINAPI}
    FImage: TLazIntfImage; // Lazarus only
    FMaskHandle: HBITMAP;
  {$ENDIF}
    FOldBitmap: HBITMAP;
    FPixels: PKColorRecs;
    FPixelsChanged: Boolean;
    FWidth: Integer;
    function GetScanLine(Index: Integer): PKColorRecs;
    function GetHandle: HBITMAP;
    function GetPixel(X, Y: Integer): TKColorRec;
    procedure SetPixel(X, Y: Integer; Value: TKColorRec);
  protected
    { Paints itself to ACanvas at location ARect. }
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    { Returns True if bitmap is empty. }
    function GetEmpty: Boolean; override;
    { Returns the bitmap height. }
    function GetHeight: Integer; override;
    { Returns True. Treat alpha bitmap as transparent because of the
      possible alpha channel. }
    function GetTransparent: Boolean; override;
    { Returns the bitmap width. }
    function GetWidth: Integer; override;
    { Specifies new bitmap height. }
    procedure SetHeight(Value: Integer); override;
    { Specifies new bitmap width. }
    procedure SetWidth(Value: Integer); override;
    { Does nothing. Bitmap is never transparent. }
    procedure SetTransparent(Value: Boolean); override;
    { Updates the bitmap handle from bitmap pixels. }
    procedure UpdateHandle; dynamic;
    { Updates the pixels from bitmap handle. }
    procedure UpdatePixels; dynamic;
  public
    { Creates the instance. }
    constructor Create; override;
    { Creates the instance from application resources. For Lazarus 'BMP' type is
      taken, for Delphi RT_RCDATA is taken. }
    constructor CreateFromRes(const ResName: string);
    { Destroys the instance. }
    destructor Destroy; override;
    { Paints alpha bitmap onto Canvas at position given by X, Y. The alpha bitmap
      is combined with the background already drawn on Canvas using alpha channel
      stored in the alpha bitmap. }
    procedure AlphaDrawTo(ACanvas: TCanvas; X, Y: Integer);
    { Paints alpha bitmap onto Canvas at position given by ARect. The alpha bitmap
      is combined with the background already drawn on Canvas using alpha channel
      stored in the alpha bitmap. }
    procedure AlphaStretchDrawTo(ACanvas: TCanvas; const ARect: TRect);
    { Fills the alpha channel with Alpha. If the optional IfEmpty parameter is True,
      the alpha channel won't be modified unless it has zero value for all pixels. }
    procedure AlphaFill(Alpha: Byte; IfEmpty: Boolean = False); overload;
    { Fills the alpha channel according to given parameters. Currently it is used
      internally by @link(TKDragWindow). }
    procedure AlphaFill(Alpha: Byte; BlendColor: TColor; Gradient, Translucent: Boolean); overload;
    { Combines the pixel at given location with the given color. }
    procedure CombinePixel(X, Y: Integer; Color: TKColorRec);
    { Takes dimensions and pixels from ABitmap. }
    procedure CopyFrom(ABitmap: TKAlphaBitmap);
    { Takes 90°-rotated dimensions and pixels from ABitmap. }
    procedure CopyFromRotated(ABitmap: TKAlphaBitmap);
    { Copies a location specified by ARect from ACanvas to bitmap. }
    procedure DrawFrom(ACanvas: TCanvas; const ARect: TRect);
    { Calls @link(TKAlphaBitmap.Draw). }
    procedure DrawTo(ACanvas: TCanvas; const ARect: TRect);
  {$IFNDEF FPC}
    { Does nothing. }
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
  {$ENDIF}
    { Loads the bitmap from a stream. }
    procedure LoadFromStream(Stream: TStream); override;
    { Mirrors the bitmap pixels horizontally. }
    procedure MirrorHorz;
    { Mirrors the bitmap pixels vertically. }
    procedure MirrorVert;
  {$IFNDEF FPC}
    { Does nothing. }
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
  {$ENDIF}
    { Saves the bitmap to a stream. }
    procedure SaveToStream(Stream: TStream); override;
    { Specifies the bitmap size. }
    procedure SetSize(AWidth, AHeight: Integer); {$IFNDEF FPC} reintroduce;{$ENDIF}
    { Returns the bitmap memory canvas. }
    property Canvas: TCanvas read FCanvas;
    { Temporary flag. Use when copying data directly from another TGraphic to TKAlphaBitmap. }
    property DirectCopy: Boolean read FDirectCopy write FDirectCopy;
    { Returns the bitmap handle. }
    property Handle: HBITMAP read GetHandle;
    { Specifies the pixel color. Does range checking. }
    property Pixel[X, Y: Integer]: TKColorRec read GetPixel write SetPixel;
    { Returns the pointer to bitmap pixels. }
    property Pixels: PKColorRecs read FPixels;
    { Set this property to True if you have modified the bitmap pixels. }
    property PixelsChanged: Boolean read FPixelsChanged write FPixelsChanged;
    { Returns the pointer to a bitmap scan line. }
    property ScanLine[Index: Integer]: PKColorRecs read GetScanLine;
  end;

{$IFDEF USE_WINAPI}
  TUpdateLayeredWindowProc = function(Handle: THandle; hdcDest: HDC; pptDst: PPoint;
    _psize: PSize; hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBLENDFUNCTION;
    dwFlags: DWORD): Boolean; stdcall;
{$ENDIF}

  { @abstract(Encapsulates the drag window)
    Drag window is top level window used for dragging with mouse. It displays
    some portion of associated control. It can be translucent under Windows. }
  TKDragWindow = class(TObject)
  private
    FActive: Boolean;
    FAlphaEffects: Boolean;
    FBitmap: TKAlphaBitmap;
    FBitmapFilled: Boolean;
    FControl: TCustomControl;
    FGradient: Boolean;
    FInitialPos: TPoint;
    FLayered: Boolean;
    FMasterAlpha: Byte;
  {$IFDEF USE_WINAPI}
    FBlend: TBlendFunction;
    FUpdateLayeredWindow: TUpdateLayeredWindowProc;
    FWindow: HWND;
  {$ELSE}
    FDragForm: TCustomForm;
  {$ENDIF}
  public
    { Creates the instance. }
    constructor Create;
    { Destroys the instance. }
    destructor Destroy; override;
    { Shows the drag window on screen. Takes a rectangular part as set by ARect from
      IniCtrl's Canvas and displays it at position InitialPos. MasterAlpha and
      Gradient are used to premaster the copied image with a specific fading effect. }
    procedure Show(IniCtrl: TCustomControl; const ARect: TRect; const InitialPos,
      CurrentPos: TPoint; MasterAlpha: Byte; Gradient: Boolean);
    { Moves the drag window to a new location. }
    procedure Move(const NewPos: TPoint);
    { Hides the drag window. }
    procedure Hide;
    { Returns True if the drag window is shown. }
    property Active: Boolean read FActive;
    { Returns the pointer to the bitmap that holds the copied control image. }
    property Bitmap: TKAlphaBitmap read FBitmap;
    { Returns True if the control already copied itself to the bitmap. }
    property BitmapFilled: Boolean read FBitmapFilled;
  end;

  { @abstract(Base class for KControls hints)
    This class extends the standard THintWindow class. It adds functionality
    common to all hints used in KControls. }
  TKHintWindow = class(THintWindow)
  private
    FExtent: TPoint;
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
  public
    { Creates the instance. }
    constructor Create(AOwner: TComponent); override;
    { Shows the hint at given position. This is an IDE independent implementation. }
    procedure ShowAt(const Origin: TPoint);
    { Returns the extent of the hint. }
    property Extent: TPoint read FExtent;
  end;

  { @abstract(Hint window to display formatted text)
    This class implements the textual hint window. The text is displayed . }
  TKTextHint = class(TKHintWindow)
  private
    FText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
    procedure SetText(const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
  protected
    { Overriden method. Paints the hint. }
    procedure Paint; override;
  public
    { Creates the instance. }
    constructor Create(AOwner: TComponent); override;
    { }
    property Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF} read FText write SetText;
  end;

  TKGraphicHint = class(TKHintWindow)
  private
    FGraphic: TGraphic;
    procedure SetGraphic(const Value: TGraphic);
  protected
    { Overriden method. Paints the hint. }
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Graphic: TGraphic read FGraphic write SetGraphic;
  end;

{ Draws Src to Dest with per pixel weighting by alpha channel saved in Src. }
procedure BlendLine(Src, Dest: PKColorRecs; Count: Integer);

{ Calculates a brighter color of given color based on the HSL color space.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Color</I> - input color.</LI>
  <LI><I>Percent</I> - percentage of luminosity to bright the color (0 to 1).</LI>
  <LI><I>Mode</I> - identifies how the Percent parameter should be interpreted.</LI>
  </UL> }
function BrightColor(Color: TColor; Percent: Single; Mode: TKBrightMode = bsAbsolute): TColor;

{ Returns current canvas window/wiewport scaling. }
procedure CanvasGetScale(ACanvas: TCanvas; out MulX, MulY, DivX, DivY: Integer);

{ Selects the default window/wiewport scaling to given canvas for both axes. }
procedure CanvasResetScale(ACanvas: TCanvas);

{ Returns True if the ACanvas's device context has been mapped to anything else
  than MM_TEXT. }
function CanvasScaled(ACanvas: TCanvas): Boolean;

{ Selects the window/wiewport scaling to given canvas for both axes. }
procedure CanvasSetScale(ACanvas: TCanvas; MulX, MulY, DivX, DivY: Integer);

{ Selects the wiewport offset to given canvas for both axes. }
procedure CanvasSetOffset(ACanvas: TCanvas; OfsX, OfsY: Integer);

{ Makes a grayscale representation of the given color. }
function ColorToGrayScale(Color: TColor): TColor;

{ Calls BitBlt. }
procedure CopyBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcX, SrcY: Integer);

{ Creates an empty rectangular region. }
function CreateEmptyRgn: HRGN;

{ Draws Text to the Canvas at location given by ARect.
  HAlign and VAlign specify horizontal resp. vertical alignment of the text
  within ARect. HPadding and VPadding specify horizontal (both on left and right side)
  and vertical (both on top and bottom side) padding of the Text from ARect.
  BackColor specifies the fill color for brush gaps if a non solid Brush
  is defined in Canvas. Attributes specift various text output attributes. }
procedure DrawAlignedText(Canvas: TCanvas; var ARect: TRect;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  BackColor: TColor = clWhite; Attributes: TKTextAttributes = []);

{ Simulates WinAPI DrawEdge with customizable colors. }
procedure DrawEdges(Canvas: TCanvas; const R: TRect; HighlightColor,
  ShadowColor: TColor; Flags: Cardinal);

{ Draws a rectangle to Canvas. The rectangle coordinates are given by Rect.
  The rectangle is filled by Brush. If Brush is not solid, its gaps are filled
  with BackColor. If BackColor is clNone these gaps are not filled and the Brush
  appears transparent. }
procedure DrawFilledRectangle(Canvas: TCanvas; const ARect: TRect;
  BackColor: TColor);

{ This helper function excludes a rectangular area occupied by a shape from
  BaseRect and calculates the shape area rectangles Bounds and Interior.
  The shape area is specified by the shape extent (ShapeWidth and ShapeHeight),
  padding (HPadding and VPadding) and stretching mode (StretchMode).
  The returned Bounds includes (possibly stretched) shape + padding,
  and Interior includes only the (possibly stretched) shape.
  HAlign specifies the horizontal alignment of shape area within BaseRect.
  VAlign specifies the vertical alignment of shape area within BaseRect.
  The shape area is always excluded horizontally from BaseRect, as needed by cell
  data calculations in KGrid. }
procedure ExcludeShapeFromBaseRect(var BaseRect: TRect; ShapeWidth, ShapeHeight: Integer;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  StretchMode: TKStretchMode; out Bounds, Interior: TRect);

{ Selects ARect into device context. Returns previous clipping region. }
function ExtSelectClipRect(DC: HDC; ARect: TRect; Mode: Integer; out PrevRgn: HRGN): Boolean;

{ Selects ARect into device context. Combines with CurRgn and
  returns previous clipping region. Both regions have to be created first. }
function ExtSelectClipRectEx(DC: HDC; ARect: TRect; Mode: Integer; CurRgn, PrevRgn: HRGN): Boolean;

{ Fills the area specified by the difference Boundary - Interior on ACanvas with current Brush.
  If Brush is not solid, its gaps are filled with BackColor. If BackColor is
  clNone these gaps are not filled and the Brush appears transparent. }
procedure FillAroundRect(ACanvas: TCanvas; const Boundary, Interior: TRect; BackColor: TColor);

{ Selects the region into given device context and deletes the region. }
procedure FinalizePrevRgn(DC: HDC; ARgn: HRGN);

{ Determine the height (ascent + descent) of the font currently selected into given DC. }
function GetFontHeight(DC: HDC): Integer;

{ Raises an exception if GDI resource has not been created. }
function GDICheck(Value: Integer): Integer;

{ Creates a TGraphic instance according to the image file header.
  Currently supported images are BMP, PNG, MNG, JPG, ICO. }
function ImageByType(const Header: TKImageHeaderString): TGraphic;

{ Calls the IntersectClipRect function. }
function IntersectClipRectIndirect(DC: HDC; ARect: TRect): Boolean;

{ Determines if given color has lightness > 0.5. }
function IsBrightColor(Color: TColor): Boolean;

{ Loads a custom mouse cursor. }
procedure LoadCustomCursor(Cursor: TCursor; const ResName: string);

{ Builds a TKColorRec structure. }
function MakeColorRec(R, G, B, A: Byte): TKColorRec;

{ Returns a pixel format that matches Bpp. }
function PixelFormatFromBpp(Bpp: Cardinal): TPixelFormat;

{ In Lazarus this WinAPI function is missing. }
function RectInRegion(Rgn: HRGN; ARect: TRect): Boolean;

{ Paints an image so that it fits in ARect. Performs double buffering and fills
  the background with current brush for mapped device contexts. }
procedure SafeStretchDraw(ACanvas: TCanvas; ARect: TRect; AGraphic: TGraphic; ABackColor: TColor = clWhite);

{ Selects ARect as new clipping region into the device context. }
procedure SelectClipRect(DC: HDC; const ARect: TRect);

{ Calls StretchBlt. }
procedure StretchBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcRect: TRect);

{ Swaps the color format from RGB to BGR and vice versa. }
function SwitchRGBToBGR(Value: TColor): TColor;

{ Subtracts the current device context offset to ARect. }
procedure TranslateRectToDevice(DC: HDC; var ARect: TRect);

implementation

uses
  Math, SysUtils, Types, KControls
{$IFDEF FPC}
  , FPImage
{$ELSE}
  , JPeg
{$ENDIF}
  ;

procedure BlendLine(Src, Dest: PKColorRecs; Count: Integer);
var
  I: Integer;
  R, G, B, A1, A2: Integer;
begin
  // without assembler
  for I := 0 to Count - 1 do
  begin
    A1 := Src[I].A;
    A2 := 255 - A1;
    Inc(A1);
    Inc(A2);
    R := Src[I].R * A1 + Dest[I].R * A2;
    G := Src[I].G * A1 + Dest[I].G * A2;
    B := Src[I].B * A1 + Dest[I].B * A2;
    Dest[I].R := R shr 8;
    Dest[I].G := G shr 8;
    Dest[I].B := B shr 8;
  end;
end;

function CalcLightness(Color: TColor): Single;
var
  X: TKColorRec;
begin
  X.Value := ColorToRGB(Color);
  Result := (X.R + X.G + X.B) / (3 * 256);
end;

function BrightColor(Color: TColor; Percent: Single; Mode: TKBrightMode): TColor;
var
  L, Tmp: Single;

  function Func1(Value: Single): Single;
  begin
    Result := Value * (L + Percent) / L;
  end;

  function Func2(Value: Single): Single;
  begin
    Result := 1 - (0.5 - Tmp) * (1 - Value) / (1 - L);
    { this is the shorter form of
      Value := 1 - 0.5 * (1 - Value) / (1 - L) ; // get color with L = 0.5
      Result := 1 - (0.5 - Tmp) * (1 - Value) / 0.5; // get corresponding color
    }
  end;

  function Rd(Value: Single): Byte;
  begin
    Result := Min(Integer(Round(Value * 255)), 512);
  end;

var
  R, G, B, Cmax, Cmin: Single;
  X: TKColorRec;
begin
  X.Value := ColorToRGB(Color);
  R := X.R / 255;
  G := X.G / 255;
  B := X.B / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  if L < 1 then
  begin
    case Mode of
      bsOfBottom: Percent := L * Percent;
      bsOfTop: Percent := (1 - L) * Percent;
    end;
    Percent := Min(Percent, 1 - L);
    if L = 0 then
    begin
      // zero length singularity
      R := R + Percent; G := G + Percent; B := B + Percent;
    end else
    begin
      Tmp := L + Percent - 0.5;
      // lumination below 0.5
      if L < 0.5 then
      begin
        // if L + Percent is >= 0.5, get color with L = 0.5
        Percent := Min(Percent, 0.5 - L);
        R := Func1(R); G := Func1(G); B := Func1(B);
        L := 0.5;
      end;
      // lumination above 0.5
      if Tmp > 0 then
      begin
        R := Func2(R); G := Func2(G); B := Func2(B);
      end;
    end;
    X.R := Rd(R);
    X.G := Rd(G);
    X.B := Rd(B);
  end;
  Result := X.Value;
end;

procedure CanvasGetScale(ACanvas: TCanvas; out MulX, MulY, DivX, DivY: Integer);
{$IFDEF USE_DC_MAPPING}
var
  WindowExt, ViewPortExt: TSize;
{$ENDIF}
begin
{$IFDEF USE_DC_MAPPING}
  if Boolean(GetWindowExtEx(ACanvas.Handle, {$IFDEF FPC}@{$ENDIF}WindowExt)) and
    Boolean(GetViewPortExtEx(ACanvas.Handle, {$IFDEF FPC}@{$ENDIF}ViewPortExt)) then
  begin
    DivX := WindowExt.cx; DivY := WindowExt.cy;
    MulX := ViewPortExt.cx; MulY := ViewPortExt.cy;
  end else
{$ENDIF}
  begin
    MulX := 1; DivX := 1;
    MulY := 1; DivY := 1;
  end;
end;

procedure CanvasResetScale(ACanvas: TCanvas);
begin
{$IFDEF USE_DC_MAPPING}
  SetMapMode(ACanvas.Handle, MM_TEXT);
{$ENDIF}
end;

function CanvasScaled(ACanvas: TCanvas): Boolean;
begin
{$IFDEF USE_DC_MAPPING}
  Result := not (GetMapMode(ACanvas.Handle) in [0, MM_TEXT]);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure CanvasSetScale(ACanvas: TCanvas; MulX, MulY, DivX, DivY: Integer);
begin
{$IFDEF USE_DC_MAPPING}
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, DivX, DivY, nil);
  SetViewPortExtEx(ACanvas.Handle, MulX, MulY, nil);
{$ELSE}
  {$WARNING 'Device context window/viewport transformations not working!'}
{$ENDIF}
end;

procedure CanvasSetOffset(ACanvas: TCanvas; OfsX, OfsY: Integer);
begin
{$IFDEF USE_DC_MAPPING}
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  SetViewPortOrgEx(ACanvas.Handle, OfsX, OfsY, nil);
{$ENDIF}  
end;

function ColorToGrayScale(Color: TColor): TColor;
var
  GreyValue: Integer;
  X: TKColorRec;
begin
  X.Value := ColorToRGB(Color);
  GreyValue := (X.R + X.G + X.B) div 3;
  X.R := GreyValue;
  X.G := GreyValue;
  X.B := GreyValue;
  Result := X.Value;
end;

procedure CopyBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcX, SrcY: Integer);
begin
  {$IFDEF USE_WINAPI}Windows.{$ENDIF}BitBlt(DestDC,
    DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    SrcDC, 0, 0, SRCCOPY);
end;

function CreateEmptyRgn: HRGN;
begin
  Result := CreateRectRgn(0,0,0,0);
end;

procedure DrawAlignedText(Canvas: TCanvas; var ARect: TRect;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  BackColor: TColor; Attributes: TKTextAttributes);
var
  DC: HDC;
  FontHeight: Integer;
  ClipRect: TRect;

  function MeasureOrOutput(Y: Integer; Output: Boolean): TSize;
  var
    EndEllipsis, PathEllipsis: Boolean;
    Width, EllipsisWidth: Integer;

    function TextExtent(AText: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}; ALen: Integer; Trim: Boolean = False): TSize;
    begin
      if Trim then
      begin
        if taLineBreak in Attributes then
          TrimWhiteSpaces(AText, ALen, cLineBreaks);
        if taTrimWhiteSpaces in Attributes then
          TrimWhiteSpaces(AText, ALen, cWordBreaks);
      end;
    {$IFDEF STRING_IS_UNICODE}
     {$IFDEF FPC}
      {$IFDEF USE_CANVAS_METHODS}
      Result := Canvas.TextExtent(Copy(AText, 0, ALen)); // little slower but more secure in Lazarus
      {$ELSE}
      GetTextExtentPoint32(DC, AText, ALen, Result);
      {$ENDIF}
     {$ELSE}
      GetTextExtentPoint32(DC, AText, ALen, Result);
     {$ENDIF}
    {$ELSE}
      GetTextExtentPoint32W(DC, AText, ALen, Result);
    {$ENDIF}
    end;

    procedure FmtTextOut(Y: Integer; AText: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}; ALen: Integer);
    var
      DrawEllipsis, DrawFileName: Boolean;
      AWidth, Index, NewIndex,SlashPos, FileNameLen, EllipsisMaxX, X: Integer;
      S: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
    begin
      DrawEllipsis := False;
      DrawFileName := False;
      SlashPos := 0;
      FileNameLen := 0;
      if taLineBreak in Attributes then
        TrimWhiteSpaces(AText, ALen, cLineBreaks);
      if taTrimWhiteSpaces in Attributes then
        TrimWhiteSpaces(AText, ALen, cWordBreaks);
      if (EndEllipsis or PathEllipsis) and (ALen > 1) then
      begin
        AWidth := TextExtent(AText, ALen).cx;
        if AWidth > Width then
        begin
          AWidth := 0;
          Index := 0;
          if EndEllipsis then
          begin
            EllipsisMaxX := Width - EllipsisWidth;
            while (Index < ALen) do
            begin
              NewIndex := StrNextCharIndex(AText, Index);
              Inc(AWidth, TextExtent(@AText[Index], NewIndex - Index).cx);
              if (AWidth > EllipsisMaxX) and (Index > 0) then
                Break
              else
                Index := NewIndex;
            end;
            ALen := Index;
            DrawEllipsis := True;
          end
          else if PathEllipsis then
          begin
            SlashPos := ALen;
            while (SlashPos > 0) and not CharInSetEx(AText[SlashPos], ['/', '\']) do
              Dec(SlashPos);
            if SlashPos > 0 then
            begin
              DrawEllipsis := True;
              DrawFileName := True;
              FileNameLen := ALen - SlashPos;
              EllipsisMaxX := Width - TextExtent(@AText[SlashPos], FileNameLen).cx - EllipsisWidth;
              while (Index < SlashPos) do
              begin
                NewIndex := StrNextCharIndex(AText, Index);
                Inc(AWidth, TextExtent(@AText[Index], NewIndex - Index).cx);
                if AWidth > EllipsisMaxX then
                  Break
                else
                  Index := NewIndex;
              end;
              ALen := Index;
            end;
          end;
        end;
      end;
      if DrawEllipsis then
      begin
        if DrawFileName then
        begin
          S := Copy(AText, 0, ALen) + cEllipsis + Copy(AText, SlashPos + 1, FileNameLen);
        end else
          S := Copy(AText, 0, ALen) + cEllipsis;
        AText := {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}(S);
        ALen := Length(S);
      end;
      case HAlign of
        halCenter:
          X := Max(ClipRect.Left, (ClipRect.Left + ClipRect.Right - TextExtent(AText, ALen).cx) div 2);
        halRight:
          X := ClipRect.Right - TextExtent(AText, ALen).cx;
      else
        X := ClipRect.Left;
      end;
    {$IFDEF STRING_IS_UNICODE}
     {$IFDEF FPC}
      {$IFDEF USE_CANVAS_METHODS}
      Canvas.TextOut(X, Y, Copy(AText, 0, ALen)); // little slower but more secure in Lazarus
      {$ELSE}
      TextOut(DC, X, Y, AText, ALen);
      {$ENDIF}
     {$ELSE}
      TextOut(DC, X, Y, AText, ALen);
     {$ENDIF}
    {$ELSE}
      TextOutW(DC, X, Y, AText, ALen);
    {$ENDIF}
    end;

  var
    I, Index, TextLen, LineBegin, LineBreaks, Vert: Integer;
    CalcRect, WordBreak, LineBreak, WhiteSpace, PrevWhiteSpace, FirstWord,
    WrapText: Boolean;
    Size: TSize;
  begin
    Result.cx := 0;
    Vert := Y;
    if AText <> '' then
    begin
      LineBegin := 1;
      LineBreaks := 0;
      TextLen := Length(AText);
      Width := ClipRect.Right - ClipRect.Left;
      CalcRect := taCalcRect in Attributes;
      WordBreak := taWordBreak in Attributes;
      LineBreak := taLineBreak in Attributes;
      WrapText := taWrapText in Attributes; //JR:20091229
      if Output then
      begin
        EndEllipsis := taEndEllipsis in Attributes;
        PathEllipsis := taPathEllipsis in Attributes;
        EllipsisWidth := TextExtent(cEllipsis, Length(cEllipsis)).cx;
      end;
      if WordBreak or LineBreak then
      begin
        I := LineBegin;
        Index := LineBegin;
        WhiteSpace := True;
        FirstWord := True;
        while I <= TextLen + 1 do
        begin
          PrevWhiteSpace := WhiteSpace;
          WhiteSpace := CharInSetEx(AText[I], cWordBreaks + cLineBreaks);
          if (not PrevWhiteSpace and WhiteSpace and (I > LineBegin))
            or (not PrevWhiteSpace and WrapText and (I > LineBegin)) then //JR:20091229
          begin
            if (WordBreak or WrapText) and (LineBreaks = 0) and not FirstWord then
            begin
              Size := TextExtent(@AText[LineBegin], I - LineBegin, True);
              if Size.cx > Width then
                Inc(LineBreaks);
            end;
            if LineBreaks > 0 then
            begin
              if Index > LineBegin then
              begin
                if Output and (Vert >= ClipRect.Top - FontHeight) and (Vert <= ClipRect.Bottom) then
                  FmtTextOut(Vert, @AText[LineBegin], Index - LineBegin)
                else if CalcRect then
                  Result.cx := Max(Result.cx, TextExtent(@AText[LineBegin], Index - LineBegin, True).cx);
                LineBegin := Index;
              end;
              Inc(Vert, FontHeight * LineBreaks);
              LineBreaks := 0;
            end;
            Index := I;
            FirstWord := False;
          end;
          if LineBreak and (AText[I] = cCR) then
            Inc(LineBreaks);
          Inc(I);
        end;
      end;
      if LineBegin <= TextLen then
      begin
        if Output and (Vert >= ClipRect.Top - FontHeight) and (Vert <= ClipRect.Bottom) then
          FmtTextOut(Vert, @AText[LineBegin], TextLen - LineBegin + 1)
        else if CalcRect then
          Result.cx := Max(Result.cx, TextExtent(@AText[LineBegin], TextLen - LineBegin + 1, True).cx);
        Inc(Vert, FontHeight * (1 + LineBreaks));
      end;
    end;
    Result.cy := Vert - Y;
  end;

  procedure Initialize;
  begin
    ClipRect := ARect;
    InflateRect(ClipRect, -HPadding, -VPadding);
    DC := Canvas.Handle;
    FontHeight := GetFontHeight(DC);
  end;

var
  Y: Integer;
  TmpRect: TRect;
  Extent: TSize;
  PrevRgn: HRGN;
begin
  if taCalcRect in Attributes then
  begin
    Initialize;
    Extent := MeasureOrOutput(0, False);
    ARect.Right := ARect.Left + Extent.cx;
    ARect.Bottom := ARect.Top + Extent.cy;
  end
  else if not IsRectEmpty(ARect) then
  begin
    if taFillRect in Attributes then
      DrawFilledRectangle(Canvas, ARect, BackColor);
    if AText <> '' then
    begin
      Initialize;
      if not IsRectEmpty(ClipRect) then
      begin
        case VAlign of
          valCenter:
            Y := Max(ClipRect.Top, (ClipRect.Bottom + ClipRect.Top - MeasureOrOutput(0, False).cy) div 2);
          valBottom:
            Y := ClipRect.Bottom - MeasureOrOutput(0, False).cy;
        else
          Y := ClipRect.Top;
        end;
        TmpRect := ClipRect;
        if taClip in Attributes then
        begin
          TranslateRectToDevice(DC, TmpRect);
          if ExtSelectClipRect(DC, TmpRect, RGN_AND, PrevRgn) then
          try
            if not (taFillText in Attributes) then
              SetBkMode(DC, TRANSPARENT);
            MeasureOrOutput(Y, True);
          finally
            FinalizePrevRgn(DC, PrevRgn);
          end;
        end else
        begin
          if not (taFillText in Attributes) then
            SetBkMode(DC, TRANSPARENT);
          MeasureOrOutput(Y, True);
        end;
      end;
    end;
  end;
end;

procedure DrawEdges(Canvas: TCanvas; const R: TRect; HighlightColor,
  ShadowColor: TColor; Flags: Cardinal);
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := HighlightColor;
    if Flags and BF_LEFT <> 0 then
      FillRect(Rect(R.Left, R.Top + 1, R.Left + 1, R.Bottom));
    if Flags and BF_TOP <> 0 then
      FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    Brush.Color := ShadowColor;
    if Flags and BF_RIGHT <> 0 then
      FillRect(Rect(R.Right - 1, R.Top + 1, R.Right, R.Bottom));
    if Flags and BF_BOTTOM <> 0 then
      FillRect(Rect(R.Left + 1, R.Bottom - 1, R.Right - 1, R.Bottom));
  end;
end;

procedure DrawFilledRectangle(Canvas: TCanvas; const ARect: TRect; BackColor: TColor);
var
  DC: HDC;
begin
  DC := Canvas.Handle;
  SetBkMode(DC, OPAQUE);
  SetBkColor(DC, ColorToRGB(BackColor));
  FillRect(DC, ARect, Canvas.Brush.Handle);
end;

procedure ExcludeShapeFromBaseRect(var BaseRect: TRect; ShapeWidth, ShapeHeight: Integer;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  StretchMode: TKStretchMode; out Bounds, Interior: TRect);
var
  MaxHeight, MaxWidth, StretchHeight, StretchWidth: Integer;
  RatioX, RatioY: Single;
begin
  MaxHeight := BaseRect.Bottom - BaseRect.Top - 2 * VPadding;
  MaxWidth := BaseRect.Right - BaseRect.Left - HPadding;
  if ((MaxWidth <> ShapeWidth) or (MaxHeight <> ShapeHeight)) and (
    (StretchMode = stmZoom) or
    (StretchMode = stmZoomInOnly) and (MaxWidth >= ShapeWidth) and (MaxHeight >= ShapeHeight) or
    (StretchMode = stmZoomOutOnly) and ((MaxWidth < ShapeWidth) or (MaxHeight < ShapeHeight))
    ) then
  begin
    RatioX := MaxWidth / ShapeWidth;
    RatioY := MaxHeight / ShapeHeight;
    if RatioY >= RatioX then
    begin
      StretchWidth := MaxWidth;
      StretchHeight := ShapeHeight * StretchWidth div ShapeWidth;
    end else
    begin
      StretchHeight := MaxHeight;
      StretchWidth := ShapeWidth * StretchHeight div ShapeHeight;
    end;
  end else
  begin
    StretchHeight := ShapeHeight;
    StretchWidth := ShapeWidth;
  end;
  Bounds := BaseRect;
  Interior := BaseRect;
  case HAlign of
    halLeft:
    begin
      Inc(BaseRect.Left, StretchWidth + HPadding);
      // Bounds.Left remains unchanged
      Bounds.Right := BaseRect.Left;
      Inc(Interior.Left, HPadding);
    end;
    halCenter:
    begin
      BaseRect.Right := BaseRect.Left; // BaseRect empty, no space for next item!
      // Bounds remains unchanged
      Inc(Interior.Left, HPadding + (MaxWidth - StretchWidth) div 2);
    end;
    halRight:
    begin
      Dec(BaseRect.Right, StretchWidth + HPadding);
      Bounds.Left := BaseRect.Right;
      // Bounds.Right remains unchanged
      Interior.Left := BaseRect.Right;
    end;
  end;
  Interior.Right := Interior.Left + StretchWidth;
  case VAlign of
    valTop: Inc(Interior.Top, VPadding);
    valCenter: Inc(Interior.Top, VPadding + (MaxHeight - StretchHeight) div 2);
    valBottom: Interior.Top := BaseRect.Bottom - VPadding - StretchHeight;
  end;
  Interior.Bottom := Interior.Top + StretchHeight;
end;

function ExtSelectClipRect(DC: HDC; ARect: TRect; Mode: Integer; out PrevRgn: HRGN): Boolean;
var
  TmpRgn: HRGN;
begin
  PrevRgn := CreateEmptyRgn;
  GetClipRgn(DC, PrevRgn);
  TmpRgn := CreateEmptyRgn;
  try
    Result := ExtSelectClipRectEx(DC, ARect, Mode, TmpRgn, PrevRgn)
  finally
    DeleteObject(TmpRgn);
  end;
end;

function ExtSelectClipRectEx(DC: HDC; ARect: TRect; Mode: Integer; CurRgn, PrevRgn: HRGN): Boolean;
var
  RectRgn: HRGN;
begin
  RectRgn := CreateRectRgnIndirect(ARect);
  try
    Result := CombineRgn(CurRgn, PrevRgn, RectRgn, Mode) <> NULLREGION;
    if Result then
      SelectClipRgn(DC, CurRgn);
  finally
    DeleteObject(RectRgn);
  end;
end;

procedure FillAroundRect(ACanvas: TCanvas; const Boundary, Interior: TRect; BackColor: TColor);
var
  R: TRect;
begin
  R := Rect(Boundary.Left, Boundary.Top, Boundary.Right, Interior.Top);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
  R := Rect(Boundary.Left, Interior.Top, Interior.Left, Interior.Bottom);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
  R := Rect(Interior.Right, Interior.Top, Boundary.Right, Interior.Bottom);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
  R := Rect(Boundary.Left, Interior.Bottom, Boundary.Right, Boundary.Bottom);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
end;

procedure FinalizePrevRgn(DC: HDC; ARgn: HRGN);
begin
  SelectClipRgn(DC, ARgn);
  DeleteObject(ARgn);
end;

function GetFontHeight(DC: HDC): Integer;
var
  TM: TTextMetric;
begin
  FillChar(TM, SizeOf(TTextMetric), 0);
  GetTextMetrics(DC, TM);
  Result := TM.tmHeight;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then
    raise EOutOfResources.Create(SGDIError);
  Result := Value;
end;

function ImageByType(const Header: TKImageHeaderString): TGraphic;
begin
  if Pos('BM', {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1 then
    Result := TBitmap.Create
{$IFDEF USE_PNG_SUPPORT }
  else if (Pos(#137'PNG', {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) or
    (Pos(#138'MNG', {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) then
    Result := TKPngImage.Create
{$ENDIF }
  else if (Pos(#$FF#$D8, {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) then
    Result := TJPegImage.Create
  else if (Pos(#$FF#$D8, {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) then
    Result := TIcon.Create
  else
    Result := nil;
end;

function IntersectClipRectIndirect(DC: HDC; ARect: TRect): Boolean;
begin
  with ARect do
    Result := IntersectClipRect(DC, Left, Top, Right, Bottom) <> NULLREGION;
end;

function IsBrightColor(Color: TColor): Boolean;
begin
  Result := CalcLightness(Color) > 0.5;
end;

function MakeColorRec(R, G, B, A: Byte): TKColorRec;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

procedure LoadCustomCursor(Cursor: TCursor; const ResName: string);
begin
  Screen.Cursors[Cursor] :=
  {$IFDEF FPC}
    LoadCursorFromLazarusResource(ResName);
  {$ELSE}
    LoadCursor(HInstance, PChar(ResName));
  {$ENDIF}
end;

function PixelFormatFromBpp(Bpp: Cardinal): TPixelFormat;
begin
  case Bpp of
    1: Result := pf1bit;
    2..4: Result := pf4bit;
    5..8: Result := pf8bit;
    9..16: Result := pf16bit;
  else
    Result := pf32bit;
  end;
end;

function RectInRegion(Rgn: HRGN; ARect: TRect): Boolean;
{$IFDEF FPC}
var
  RectRgn, TmpRgn: HRGN;
{$ENDIF}
begin
{$IFDEF FPC}
  RectRgn := CreateRectRgnIndirect(ARect);
  try
    TmpRgn := CreateEmptyRgn;
    try
      Result := CombineRgn(TmpRgn, RectRgn, Rgn, RGN_AND) <> NULLREGION;
    finally
      DeleteObject(TmpRgn);
    end;
  finally
    DeleteObject(RectRgn);
  end;
{$ELSE}
  Result := Windows.RectInRegion(Rgn, ARect);
{$ENDIF}
end;

procedure SafeStretchDraw(ACanvas: TCanvas; ARect: TRect; AGraphic: TGraphic; ABackColor: TColor);
{$IFDEF USE_WINAPI}
var
  BM: TBitmap;
  W, H, MulX, MulY, DivX, DivY: Integer;
  R: TRect;
{$ENDIF}
begin
{$IFDEF USE_WINAPI}
  if AGraphic.Transparent then
  begin
    // WinAPI StretchBlt function does not read properly from screen buffer
    // so we have to append double buffering
    CanvasGetScale(ACanvas, MulX, MulY, DivX, DivY);
    W := MulDiv(ARect.Right - ARect.Left, MulX, DivX);
    H := MulDiv(ARect.Bottom - ARect.Top, MulY, DivY);
    BM := TBitmap.Create;
    try
      BM.Width := W;
      BM.Height := H;
      BM.Canvas.Brush := ACanvas.Brush;
      R := Rect(0, 0, W, H);
      DrawFilledRectangle(BM.Canvas, R, ABackColor);
      BM.Canvas.StretchDraw(R, AGraphic);
      ACanvas.StretchDraw(ARect, BM);
    finally
      BM.Free;
    end;
  end else
{$ENDIF}
    ACanvas.StretchDraw(ARect, AGraphic);
end;

procedure SelectClipRect(DC: HDC; const ARect: TRect);
var
  Rgn: HRGN;
begin
  Rgn := CreateRectRgnIndirect(ARect);
  try
    SelectClipRgn(DC, Rgn);
  finally
    DeleteObject(Rgn);
  end;
end;

procedure StretchBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcRect: TRect);
begin
  {$IFDEF USE_WINAPI}Windows.{$ENDIF}StretchBlt(DestDC,
    DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    SrcDC, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
    SRCCOPY);
end;

procedure SwapBR(var ColorRec: TKColorRec);
var
  Tmp: Byte;
begin
  Tmp := ColorRec.R;
  ColorRec.R := ColorRec.B;
  ColorRec.B := Tmp;
end;

function SwitchRGBToBGR(Value: TColor): TColor;
var
  B: Byte;
begin
  Result := Value;
  B := PKColorRec(@Value).B;
  PKColorRec(@Result).B := PKColorRec(@Result).R;
  PKColorRec(@Result).R := B;
end;

procedure TranslateRectToDevice(DC: HDC; var ARect: TRect);
var
  P: TPoint;
{$IFDEF USE_DC_MAPPING}
 {$IFNDEF LCLQT}
  WindowExt, ViewportExt: TSize;
 {$ENDIF}
{$ENDIF}
begin
{$IFDEF USE_DC_MAPPING}
  {$IFNDEF LCLQT}
  if not (GetMapMode(DC) in [0, MM_TEXT]) and
    Boolean(GetWindowExtEx(DC, {$IFDEF FPC}@{$ENDIF}WindowExt)) and
    Boolean(GetViewportExtEx(DC, {$IFDEF FPC}@{$ENDIF}ViewportExt)) then
  begin
    ARect.Left := MulDiv(ARect.Left, ViewportExt.cx, WindowExt.cx);
    ARect.Right := MulDiv(ARect.Right, ViewportExt.cx, WindowExt.cx);
    ARect.Top := MulDiv(ARect.Top, ViewportExt.cy, WindowExt.cy);
    ARect.Bottom := MulDiv(ARect.Bottom, ViewportExt.cy, WindowExt.cy);
  end;
  if Boolean(GetViewPortOrgEx(DC, {$IFDEF FPC}@{$ENDIF}P)) then
    OffsetRect(ARect, P.X, P.Y);
  {$ENDIF}
{$ENDIF}
  if Boolean(GetWindowOrgEx(DC, {$IFDEF FPC}@{$ENDIF}P)) then
    OffsetRect(ARect, -P.X, -P.Y);
end;

{ TKAlphaBitmap }

constructor TKAlphaBitmap.Create;
begin
  inherited;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := CreateCompatibleDC(0);
  FDirectCopy := False;
  FHandle := 0;
{$IFNDEF USE_WINAPI}
  FImage := TLazIntfImage.Create(0, 0);
{$ENDIF}
  FHeight := 0;
  FOldBitmap := 0;
  FPixels := nil;
  FWidth := 0;
end;

constructor TKAlphaBitmap.CreateFromRes(const ResName: string);
var
  Stream: {$IFDEF FPC}TLazarusResourceStream{$ELSE}TResourceStream{$ENDIF};
begin
  Create;
  try
  {$IFDEF FPC}
    Stream := TLazarusResourceStream.Create(LowerCase(ResName), 'BMP');
  {$ELSE}
    Stream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  {$ENDIF}
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
  end;  
end;

destructor TKAlphaBitmap.Destroy;
var
  DC: HDC;
begin
  inherited;
  SetSize(0, 0);
{$IFNDEF USE_WINAPI}
  FImage.Free;
{$ENDIF}
  DC := FCanvas.Handle;
  FCanvas.Handle := 0;
  DeleteDC(DC);
  FCanvas.Free;
end;

procedure TKAlphaBitmap.AlphaDrawTo(ACanvas: TCanvas; X, Y: Integer);
begin
  AlphaStretchDrawTo(ACanvas, Rect(X, Y, X + FWidth, Y + FHeight));
end;

procedure TKAlphaBitmap.AlphaFill(Alpha: Byte; IfEmpty: Boolean);
var
  I: Integer;
  HasAlpha: Boolean;
begin
  HasAlpha := False;
  if IfEmpty then
    for I := 0 to FWidth * FHeight - 1 do
      if FPixels[I].A <> 0 then
      begin
        HasAlpha := True;
        Break;
      end;
  if not HasAlpha then
    for I := 0 to FWidth * FHeight - 1 do
      FPixels[I].A := Alpha;
end;

procedure TKAlphaBitmap.AlphaFill(Alpha: Byte; BlendColor: TColor; Gradient, Translucent: Boolean);
var
  I, J, A1, A2, AR, AG, AB, HAlpha: Integer;
  HStep, HSum, VStep, VSum: Single;
  Scan: PKColorRecs;
  CS: TKColorRec;
begin
  VSum := 0; VStep := 0;
  HSum := 0; HStep := 0;
  if Gradient then
  begin
    VStep := Alpha / FHeight;
    VSum := Alpha;
  end;
  CS.Value := ColorToRGB(BlendColor);
{$IFNDEF USE_WINAPI}
  for I := 0 to FHeight - 1 do
{$ELSE}
  for I := FHeight - 1 downto 0 do
{$ENDIF}
  begin
    Scan := ScanLine[I];
    HAlpha := Alpha;
    if Gradient then
    begin
      HStep := HAlpha / FWidth;
      HSum := HAlpha;
    end;
    for J := 0 to FWidth - 1 do with Scan[J] do
    begin
      A1 := HAlpha;
      A2 := 255 - HAlpha;
      AR := R * A1 + CS.R * A2;
      AG := G * A1 + CS.G * A2;
      AB := B * A1 + CS.B * A2;
      R := AR shr 8;
      G := AG shr 8;
      B := AB shr 8;
      if Translucent then
        A := HAlpha
      else
        A := 255;
      if Gradient then
      begin
        HAlpha := Round(HSum);
        HSum := HSum - HStep;
      end;
    end;
    if Gradient then
    begin
      Alpha := Round(VSum);
      VSum := VSum - VStep;
    end;
  end;
  FPixelsChanged := True;
end;

procedure TKAlphaBitmap.AlphaStretchDrawTo(ACanvas: TCanvas;
  const ARect: TRect);
{$IFDEF USE_WINAPI}
var
  I: Integer;
  Tmp: TKAlphaBitmap;
  Ps, Pd: PKColorRecs;
{$ENDIF}
begin
{$IFNDEF USE_WINAPI}
  DrawTo(ACanvas, ARect);
{$ELSE}
  Tmp := TKAlphaBitmap.Create;
  try
    Tmp.SetSize(FWidth, FHeight);
    Tmp.DrawFrom(ACanvas, ARect);
    for I := 0 to FHeight - 1 do
    begin
      Ps := ScanLine[I];
      Pd := Tmp.ScanLine[I];
      BlendLine(Ps, Pd, FWidth);
    end;
    Tmp.PixelsChanged := True;
    Tmp.DrawTo(ACanvas, ARect);
  finally
    Tmp.Free;
  end;
{$ENDIF}
end;

procedure TKAlphaBitmap.CombinePixel(X, Y: Integer; Color: TKColorRec);
var
  Index, A1, A2, AR, AG, AB: Integer;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    SwapBR(Color);
  {$IFDEF USE_WINAPI}
    Index := (FHeight - Y - 1) * FWidth + X;
  {$ELSE}
    Index := Y * FWidth + X;
  {$ENDIF}
    A2 := Color.A;
    if A2 = 255 then
      FPixels[Index] := Color
    else if A2 <> 0 then
    begin
      A1 := 255 - Color.A;
      AR := FPixels[Index].R * A1 + Color.R * A2;
      AG := FPixels[Index].G * A1 + Color.G * A2;
      AB := FPixels[Index].B * A1 + Color.B * A2;
      FPixels[Index].R := AR shr 8;
      FPixels[Index].G := AG shr 8;
      FPixels[Index].B := AB shr 8;
      FPixels[Index].A := 255;
    end;
    FPixelsChanged := True;
  end;
end;

procedure TKAlphaBitmap.CopyFrom(ABitmap: TKAlphaBitmap);
var
  I, Size: Integer;
begin
  SetSize(ABitmap.Width, ABitmap.Height);
  Size := FWidth * SizeOf(TKColorRec);
  for I := 0 to FHeight - 1 do
    Move(ABitmap.ScanLine[I]^, ScanLine[I]^, Size);
  FPixelsChanged := True;
end;

procedure TKAlphaBitmap.CopyFromRotated(ABitmap: TKAlphaBitmap);
var
  I, J: Integer;
  SrcScan, DstScan: PKColorRecs;
begin
  SetSize(ABitmap.Height, ABitmap.Width);
  for J := 0 to ABitmap.Height - 1 do
  begin
    SrcScan := ABitmap.ScanLine[J];
    for I := 0 to ABitmap.Width - 1 do
    begin
      DstScan := ScanLine[ABitmap.Width - I - 1];
      DstScan[J] := SrcScan[I];
    end;
  end;
  FPixelsChanged := True;
end;

procedure TKAlphaBitmap.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FDirectCopy then
    DrawTo(ACanvas, ARect)
  else
    AlphaStretchDrawTo(ACanvas, ARect);
end;

procedure TKAlphaBitmap.DrawFrom(ACanvas: TCanvas; const ARect: TRect);
begin
  if not Empty then
  begin
    if not CanvasScaled(ACanvas) then
      StretchBitmap(FCanvas.Handle, Rect(0, 0, FWidth, FHeight), ACanvas.Handle, ARect)
    else
    begin
      FCanvas.Brush := ACanvas.Brush;
      DrawFilledRectangle(FCanvas, Rect(0, 0, FWidth, FHeight),
        {$IFDEF USE_WINAPI}GetBkColor(ACanvas.Handle){$ELSE}clWindow{$ENDIF});
    end;
    UpdatePixels;
  end;
end;

procedure TKAlphaBitmap.DrawTo(ACanvas: TCanvas; const ARect: TRect);
begin
  if not Empty then
  begin
    UpdateHandle;
    StretchBitmap(ACanvas.Handle, ARect, FCanvas.Handle, Rect(0, 0, FWidth, FHeight))
  end;
end;

function TKAlphaBitmap.GetEmpty: Boolean;
begin
  Result := (FWidth = 0) and (FHeight = 0);
end;

function TKAlphaBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TKAlphaBitmap.GetPixel(X, Y: Integer): TKColorRec;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
  {$IFDEF USE_WINAPI}
    Result := FPixels[(FHeight - Y - 1) * FWidth + X];
  {$ELSE}
    Result := FPixels[Y * FWidth + X];
  {$ENDIF}
    SwapBR(Result);
  end else
    Result := MakeColorRec(0,0,0,0);
end;

function TKAlphaBitmap.GetTransparent: Boolean;
begin
  Result := True;
end;

function TKAlphaBitmap.GetScanLine(Index: Integer): PKColorRecs;
begin
  // no checks here
  Result := @FPixels[Index * FWidth];
end;

function TKAlphaBitmap.GetHandle: HBITMAP;
begin
  Result := FHandle;
end;

function TKAlphaBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

{$IFNDEF FPC}
procedure TKAlphaBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  // does nothing
end;
{$ENDIF}

procedure TKAlphaBitmap.LoadFromStream(Stream: TStream);
var
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
begin
  SetSize(0, 0);
  Stream.Read(BF, SizeOf(TBitmapFileHeader));
  Stream.Read(BI, SizeOf(TBitmapInfoHeader));
  if BI.biBitCount = 32 then
  begin
    SetSize(BI.biWidth, BI.biHeight);
    Stream.Read(FPixels^, BI.biSizeImage);
    // if bitmap has no alpha channel, create full opacity
    AlphaFill($FF, True);
  end;
  FPixelsChanged := True;
end;

procedure TKAlphaBitmap.MirrorHorz;
var
  I, J, Index: Integer;
  SrcScan: PKColorRecs;
  Buf: TKColorRec;
begin
  for I := 0 to FHeight - 1 do
  begin
    SrcScan := ScanLine[I];
    Index := FWidth - 1;
    for J := 0 to (FWidth shr 1) - 1 do
    begin
      Buf := SrcScan[Index];
      SrcScan[Index] := SrcScan[J];
      SrcScan[J] := Buf;
      Dec(Index);
    end;
  end;
  FPixelsChanged := True;
end;

procedure TKAlphaBitmap.MirrorVert;
var
  I, Size, Index: Integer;
  SrcScan, DstScan: PKColorRecs;
  Buf: PKColorRec;
begin
  Size:= FWidth * SizeOf(TKColorRec);
  Index := FHeight - 1;
  GetMem(Buf, Size);
  try
    for I := 0 to (FHeight shr 1) - 1 do
    begin
      SrcScan := ScanLine[I];
      DstScan := ScanLine[Index];
      Move(SrcScan^, Buf^, Size);
      Move(DstScan^, SrcScan^, Size);
      Move(Buf^, DstScan^, Size);
      Dec(Index);
    end;
  finally
    FreeMem(Buf);
  end;
  FPixelsChanged := True;
end;

{$IFNDEF FPC}
procedure TKAlphaBitmap.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  // does nothing
end;
{$ENDIF}

procedure TKAlphaBitmap.SaveToStream(Stream: TStream);
var
  Size: Integer;
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
begin
  Size := FWidth * FHeight * 4;
  FillChar(BF, SizeOf(TBitmapFileHeader), 0);
  BF.bfType := $4D42;
  BF.bfSize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + Size;
  BF.bfOffBits := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);
  Stream.Write(BF, SizeOf(TBitmapFileHeader));
  FillChar(BI, SizeOf(TBitmapInfoHeader), 0);
  BI.biSize := SizeOf(TBitmapInfoHeader);
  BI.biWidth := FWidth;
  BI.biHeight := FHeight;
  BI.biPlanes := 1;
  BI.biBitCount := 32;
  BI.biCompression := BI_RGB;
  BI.biSizeImage := Size;
  Stream.Write(BI, SizeOf(TBitmapInfoHeader));
  Stream.Write(FPixels^, Size);
end;

procedure TKAlphaBitmap.SetHeight(Value: Integer);
begin
  SetSize(FWidth, Value);
end;

procedure TKAlphaBitmap.SetPixel(X, Y: Integer; Value: TKColorRec);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    SwapBR(Value);
  {$IFDEF USE_WINAPI}
    FPixels[(FHeight - Y - 1) * FWidth + X] := Value;
  {$ELSE}
    FPixels[Y * FWidth + X] := Value;
  {$ENDIF}
    FPixelsChanged := True;
  end;
end;

procedure TKAlphaBitmap.SetSize(AWidth, AHeight: Integer);
var
{$IFNDEF USE_WINAPI}
  ImgFormatDescription: TRawImageDescription;
{$ELSE}
  BI: TBitmapInfoHeader;
{$ENDIF}
begin
  AWidth := Max(AWidth, 0);
  AHeight := Max(AHeight, 0);
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    if FHandle <> 0 then
    begin
      SelectObject(FCanvas.Handle, FOldBitmap);
      DeleteObject(FHandle);
      FHandle := 0;
    {$IFNDEF USE_WINAPI}
      DeleteObject(FMaskHandle);
      FMaskHandle := 0;
    {$ENDIF}
    end;
  {$IFNDEF USE_WINAPI}
    FImage.SetSize(0, 0);
  {$ENDIF}
    FPixels := nil;
    if (FWidth <> 0) and (FHeight <> 0) then
    begin
    {$IFNDEF USE_WINAPI}
      ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth,FHeight);
      FImage.DataDescription := ImgFormatDescription;
      FPixelsChanged := True;
      UpdateHandle;
    {$ELSE}
      FillChar(BI, SizeOf(TBitmapInfoHeader), 0);
      BI.biSize := SizeOf(TBitmapInfoHeader);
      BI.biWidth := FWidth;
      BI.biHeight := FHeight;
      BI.biPlanes := 1;
      BI.biBitCount := 32;
      BI.biCompression := BI_RGB;
      FHandle := GDICheck(CreateDIBSection(FCanvas.Handle, PBitmapInfo(@BI)^, DIB_RGB_COLORS, Pointer(FPixels), 0, 0));
      FOldBitmap := SelectObject(FCanvas.Handle, FHandle);
    {$ENDIF}
    end;
  end;
end;

procedure TKAlphaBitmap.SetWidth(Value: Integer);
begin
  SetSize(Value, FWidth);
end;

procedure TKAlphaBitmap.SetTransparent(Value: Boolean);
begin
  // does nothing
end;

procedure TKAlphaBitmap.UpdateHandle;
begin
{$IFNDEF USE_WINAPI}
  if FPixelsChanged then
  begin
    PixelsChanged := False;
    if FHandle <> 0 then
    begin
      DeleteObject(FMaskHandle);
      DeleteObject(SelectObject(FCanvas.Handle, FOldBitmap));
    end;
    FImage.CreateBitmaps(FHandle, FMaskHandle, False);
    FOldBitmap := SelectObject(FCanvas.Handle, FHandle);
    FPixels := PKColorRecs(FImage.PixelData);
  end;
{$ENDIF}
end;

procedure TKAlphaBitmap.UpdatePixels;
begin
{$IFNDEF USE_WINAPI}
  FImage.LoadFromDevice(FCanvas.Handle);
  FPixelsChanged := True;
  UpdateHandle;
{$ENDIF}
end;

{$IFDEF USE_WINAPI}
const
  cLayeredWndClass = 'KControls drag window';

function DragWndProc(Window: HWnd; Msg, WParam, LParam: Longint): Longint; stdcall;
var
  DC: HDC;
  PS: TPaintStruct;
  AWindow: TKDragWindow;
begin
  case Msg of
    WM_PAINT:
    begin
      AWindow := TKDragWindow(GetWindowLong(Window, GWL_USERDATA));
      if (AWindow <> nil) and AWindow.BitmapFilled then
      begin
        if wParam = 0 then
          DC := BeginPaint(Window, PS)
        else
          DC := wParam;
        try
          BitBlt(DC, 0, 0, AWindow.Bitmap.Width, AWindow.Bitmap.Height,
            AWindow.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          if wParam = 0 then EndPaint(Window, PS);
        end;
      end;
      Result := 1;
    end;
  else
    Result := DefWindowProc(Window, Msg, WParam, LParam);
  end;
end;

{$ELSE}

type

  { TKDragForm }

  TKDragForm = class(THintWindow)
  private
    FWindow: TKDragWindow;
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
  protected
    procedure Paint; override;
  public
    constructor CreateDragForm(AWindow: TKDragWindow);
  end;

{ TKDragForm }

constructor TKDragForm.CreateDragForm(AWindow: TKDragWindow);
begin
  inherited Create(nil);
  FWindow := AWindow;
  ShowInTaskBar := stNever;
end;

procedure TKDragForm.Paint;
begin
  if FWindow.Active and FWindow.BitmapFilled then
    Canvas.Draw(0, 0, FWindow.FBitmap);
end;

procedure TKDragForm.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

{$ENDIF}

constructor TKDragWindow.Create;
{$IFDEF USE_WINAPI}
var
  Cls: Windows.TWndClass;
  ExStyle: Cardinal;
{$ENDIF}
begin
  inherited;
  FActive := False;
  FBitmap := TKAlphaBitmap.Create;
  FInitialPos := Point(0, 0);
{$IFDEF USE_WINAPI}
  FUpdateLayeredWindow := GetProcAddress(GetModuleHandle('user32.dll'), 'UpdateLayeredWindow');
  FLayered := Assigned(FUpdateLayeredWindow);
  Cls.style := CS_SAVEBITS;
  Cls.lpfnWndProc := @DragWndProc;
  Cls.cbClsExtra := 0;
  Cls.cbWndExtra := 0;
  Cls.hInstance := HInstance;
  Cls.hIcon := 0;
  Cls.hCursor := 0;
  Cls.hbrBackground := 0;
  Cls.lpszMenuName := nil;
  Cls.lpszClassName := cLayeredWndClass;
  Windows.RegisterClass(Cls);
  ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  if FLayered then
    ExStyle := ExStyle or WS_EX_LAYERED or WS_EX_TRANSPARENT;
  FWindow := CreateWindowEx(ExStyle, cLayeredWndClass, '', WS_POPUP,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
    Integer(CW_USEDEFAULT), 0, 0, HInstance, nil);
  Windows.SetWindowLong(FWindow, GWL_USERDATA, Integer(Self));
{$ELSE}
  FDragForm := TKDragForm.CreateDragForm(Self);
  FLayered := False;
{$ENDIF}
end;

destructor TKDragWindow.Destroy;
begin
  inherited;
  Hide;
{$IFDEF USE_WINAPI}
  DestroyWindow(FWindow);
  Windows.UnregisterClass(cLayeredWndClass, HInstance);
{$ELSE}
  FDragForm.Free;
{$ENDIF}
  FBitmap.Free;
end;

procedure TKDragWindow.Hide;
begin
  if FActive then
  begin
  {$IFDEF USE_WINAPI}
    ShowWindow(FWindow, SW_HIDE);
  {$ELSE}
    FDragForm.Hide;
  {$ENDIF}
    FActive := False;
  end;
end;

procedure TKDragWindow.Show(IniCtrl: TCustomControl; const ARect: TRect;
  const InitialPos, CurrentPos: TPoint; MasterAlpha: Byte; Gradient: Boolean);
var
  Org: TPoint;
  W, H: Integer;
  ScreenDC: HDC;
begin
  if not (IniCtrl is TKCustomControl) then Exit;
  if not FActive then
  begin
    FActive := True;
    FBitmapFilled := False;
    FControl := IniCtrl;
    FMasterAlpha := MasterAlpha;
    FGradient := Gradient;
    FInitialPos := InitialPos;
    W := ARect.Right - ARect.Left;
    H := ARect.Bottom - ARect.Top;
    FBitmap.SetSize(W, H);
    Org := IniCtrl.ClientToScreen(ARect.TopLeft);
    ScreenDC := GetDC(0);
    try
      FAlphaEffects := GetDeviceCaps(ScreenDC, BITSPIXEL) >= 15;
      // because alpha blending is not nice elsewhere
    finally
      ReleaseDC(0, ScreenDC);
    end;
    // to be compatible with all LCL widgetsets we must copy the control's part
    // while painting in TKCustomControl.Paint!
    TKCustomControl(FControl).MemoryCanvas := FBitmap.Canvas;
    TKCustomControl(FControl).MemoryCanvasRect := ARect;
    TKCustomControl(FControl).Repaint;
  {$IFDEF USE_WINAPI}
    if FLayered then with FBlend do
    begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := 255;
      if FAlphaEffects then
        AlphaFormat := AC_SRC_ALPHA
      else
        AlphaFormat := 0;
    end;
    SetWindowPos(FWindow, 0, Org.X, Org.Y, W, H,
      SWP_NOACTIVATE or SWP_NOZORDER);
  {$ELSE}
    FDragForm.SetBounds(Org.X, Org.Y, W, H);
  {$ENDIF}
    Move(CurrentPos);
  end;
end;

procedure TKDragWindow.Move(const NewPos: TPoint);
var
  R: TRect;
  DX, DY: Integer;
  BlendColor: TColor;
{$IFDEF USE_WINAPI}
  ScreenDC: HDC;
  CanvasOrigin: TPoint;
{$ENDIF}
begin
  if FActive then
  begin
    if (TKCustomControl(FControl).MemoryCanvas = nil) and not FBitmapFilled then
    begin
      FBitmapFilled := True;
      FBitmap.UpdatePixels;
      if FAlphaEffects then
      begin
        if FLayered then
          BlendColor := clBlack
        else
          BlendColor := clWhite;
        FBitmap.AlphaFill(FMasterAlpha, BlendColor, FGradient, FLayered);
        FBitmap.UpdateHandle;
      end;
    end;
    DX := NewPos.X - FInitialPos.X;
    DY := NewPos.Y - FInitialPos.Y;
    if (DX <> 0) or (DY <> 0) then
    begin
      FInitialPos := NewPos;
    {$IFDEF USE_WINAPI}
      GetWindowRect(FWindow, R);
      OffsetRect(R, DX, DY);
      if FLayered then
      begin
        R.Right := FBitmap.Width;
        R.Bottom := FBitmap.Height;
        CanvasOrigin := Point(0, 0);
        ScreenDC := GetDC(0);
        try
          if FUpdateLayeredWindow(FWindow, ScreenDC, @R.TopLeft, PSize(@R.BottomRight),
            FBitmap.Canvas.Handle, @CanvasOrigin, clNone, @FBlend, ULW_ALPHA) then
            if FBitmapFilled then
              ShowWindow(FWindow, SW_SHOWNOACTIVATE);
        finally
          ReleaseDC(0, ScreenDC);
        end;
      end
      else if FBitmapFilled then
        SetWindowPos(FWindow, 0, R.Left, R.Top, 0, 0,
          SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER or SWP_SHOWWINDOW);
    {$ELSE}
      R := FDragForm.BoundsRect;
      OffsetRect(R, DX, DY);
      FDragForm.BoundsRect := R;
      if FBitmapFilled then
      begin
        FDragForm.Visible := True;
        SetCaptureControl(FControl);
      end;
    {$ENDIF}
    end;
  end;
end;

{ TKHintWindow }

constructor TKHintWindow.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF FPC}
  ShowInTaskBar := stNever;
{$ENDIF}
  DoubleBuffered := True;
end;

procedure TKHintWindow.ShowAt(const Origin: TPoint);
begin
  ActivateHint(Rect(Origin.X, Origin.Y, Origin.X + FExtent.X + 10, Origin.Y + FExtent.Y + 10), '');
end;

procedure TKHintWindow.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

{ TKTextHint }

constructor TKTextHint.Create(AOwner: TComponent);
begin
  inherited;
  FText := '';
end;

procedure TKTextHint.Paint;
var
  R: TRect;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clInfoBk;
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Style := bsClear;
  R := Rect(0, 0, FExtent.X + 10, FExtent.Y + 10);
  DrawAlignedText(Canvas, R, halLeft, valCenter,
    5, 5, FText, clInfoBk, [taEndEllipsis, taWordBreak, taLineBreak])
end;

procedure TKTextHint.SetText(const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
var
  R: TRect;
begin
  if Value <> FText then
  begin
    FText := Value;
    R := Rect(0, 0, 300, 0);
    DrawAlignedText(Canvas, R, halLeft, valCenter,
      0, 0, FText, clInfoBk, [taCalcRect, taWordBreak, taLineBreak]);
    FExtent.X := R.Right - R.Left;
    FExtent.Y := R.Bottom - R.Top;
  end;
end;

{ TKGraphicHint }

constructor TKGraphicHint.Create(AOwner: TComponent);
begin
  inherited;
  FGraphic := nil;
{$IFDEF FPC}
  ShowInTaskBar := stNever;
{$ENDIF}
  DoubleBuffered := True;
end;

procedure TKGraphicHint.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clInfoBk;
  Canvas.FillRect(ClientRect);
  if Assigned(FGraphic) then
    Canvas.Draw(5, 5, FGraphic)
end;

procedure TKGraphicHint.SetGraphic(const Value: TGraphic);
begin
  if Value <> FGraphic then
  begin
    FGraphic := Value;
    FExtent.X := FGraphic.Width;
    FExtent.Y := FGraphic.Height;
  end;
end;

end.
