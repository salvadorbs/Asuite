unit Graphics;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL Graphics.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TBrush+TCanvas+TFont+TPen+TPicture
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup
   - TFont: only default CharSet is available
   - TPicture is only for bitmap (advice: use UPX to shrink your EXE)

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are Copyright (C) 2001 Paul Toth. http://tothpaul.free.fr
  All Rights Reserved.

}


interface

uses
 Windows, Classes, SysUtils;

type
 TColor = -$7FFFFFFF-1..$7FFFFFFF;

 TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
 TFontStyles = set of TFontStyle;

 TFont = class(TPersistent)
 private
   fHandle: THandle;
   fColor: integer;
   fHeight: integer;
   fName: string;
   fStyle: TFontStyles;
   function GetHandle: THandle;
 public
   destructor Destroy; override;
   procedure ReadProperty(const Name: string; Reader: TReader); override;
   procedure Assign(AFont: TFont);
   property Handle: THandle read GetHandle;
   property Color: integer read fColor write fColor;
   property Style: TFontStyles read fStyle write fStyle;
   property Name: string read fName;
 end;

 TStaticHandle = object
 // static object since we don't need to free any memory or read any property
 private
   fHandle: THandle;
   fColor: integer;
   procedure SetValue(var Value: integer; NewValue: integer);
   procedure SetColor(const Value: integer);
 public
   property Color: integer read fColor write SetColor;
 end;

 TPen = object(TStaticHandle)
 private
   fWidth: integer;
   procedure SetWidth(const Value: integer);
 public
   procedure Select(Canvas: HDC);
   property Width: integer read fWidth write SetWidth;
 end;

 TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
   bsBDiagonal, bsCross, bsDiagCross);

 TBrush = object(TStaticHandle)
 private
   fStyle: TBrushStyle;
   procedure SetStyle(const Value: TBrushStyle);
 public
   function GetHandle: THandle;
   property Style: TBrushStyle read fStyle write SetStyle;
 end;

 TCanvas = class
 private
   fFont: TFont;
   fHandle: THandle;
   procedure SetFont(Value: TFont);
   function GetFont: TFont;
 public
   Pen: TPen;
   Brush: TBrush;
   destructor Destroy; override;
   procedure FillRect(const R: TRect);
   procedure MoveTo(x,y: integer);
   procedure LineTo(x,y: integer);
   procedure Rectangle(x1,y1,x2,y2: integer);
   procedure FrameRect(const Rect: TRect; cl1,cl2: integer);
   procedure PrepareText;
   procedure TextOut(x,y: integer; const s: string);
   procedure TextRect(const Rect: TRect; x,y: integer; const s:string);
   function TextWidth(const s: string): integer;
   function TextHeight(const s: string): integer;
   Property Handle: THandle read fHandle write fHandle;
   Property Font: TFont read GetFont write SetFont;
 end;

  /// this TImage component only handle a bitmap
 TPicture = class(TPersistent)
 private
   fSize: integer;
   fData: pointer;
 public
   destructor Destroy; override;
   procedure ReadProperty(const Name: string; Reader: TReader); override;
   procedure LoadFromMemory(BufferBitmap: pointer; BufferSize: integer);
   procedure LoadFromResourceName(Instance: THandle; const ResName: string);
   procedure DrawRect(const R: TRect; Canvas: TCanvas);
 end;


implementation

uses
  Controls;

 
{ TFont }

destructor TFont.Destroy;
begin
  DeleteObject(fHandle);
  inherited;
end;

procedure TFont.ReadProperty(const Name: string; Reader: TReader);
const
  TFontProperties: array[0..3] of PChar=(
   'Color','Height','Name','Style'
  );
begin
  case StringIndex(Name,TFontProperties) of
    0 : fColor := Reader.ColorProperty;
    1 : fHeight := Reader.IntegerProperty;
    2 : fName := Reader.StringProperty;
    3 : Reader.SetProperty(fStyle,TypeInfo(TFontStyle));
   else inherited;
  end;
end;

procedure TFont.Assign(AFont: TFont);
begin
  DeleteObject(fHandle);
  fHandle  := 0;
  fColor   := AFont.fColor;
  fHeight  := AFont.fHeight;
  fName    := AFont.fName;
  fStyle   := AFont.fStyle;
end;

function TFont.GetHandle: THandle;
var LogFont: TLogFont;
begin
  if fHandle=0 then begin
    FillChar(LogFont,SizeOf(LogFont),0);
    with LogFont do begin
      lfHeight := fHeight;
      if fsBold in fStyle then
        lfWeight := FW_BOLD else
        lfWeight := FW_NORMAL;
      lfItalic := byte(fsItalic in fStyle);
      lfUnderline := byte(fsUnderline in fStyle);
      lfStrikeOut := byte(fsStrikeOut in fStyle);
      lfCharSet := DEFAULT_CHARSET; 
      StrLCopy(lfFaceName,pointer(fName),high(lfFaceName));
    end;
    fHandle := CreateFontIndirect(LogFont);
  end;
  result := fHandle;
end;


{ TStaticHandle }

procedure TStaticHandle.SetColor(const Value: integer);
begin
  SetValue(fColor,Value);
end;

procedure TStaticHandle.SetValue(var Value: integer; NewValue: integer);
begin // tricky procedure for shorter code
  if Value=NewValue then exit;
  if fHandle<>0 then begin
    DeleteObject(fHandle);
    fHandle := 0;
  end;
  Value := NewValue;
end;

{ TPen }

procedure TPen.Select(Canvas: HDC);
begin
  if fHandle=0 then begin // create object once
    if Width=0 then
      fWidth := 1;
    fHandle := CreatePen(PS_SOLID,Width,Color);
  end;
  SelectObject(Canvas,fHandle);
end;

procedure TPen.SetWidth(const Value: integer);
begin
  SetValue(fWidth,Value);
end;


{ TBrush }

function TBrush.GetHandle: THandle;
begin
  if fHandle=0 then // create object once
    case fStyle of
      bsClear : ;
      bsSolid : fHandle := CreateSolidBrush(fColor);
    end;
  result := fHandle;
end;

procedure TBrush.SetStyle(const Value: TBrushStyle);
begin // tricky conversion of Value into integer for shorter code
  SetValue(PInteger(@fStyle)^,integer(Value));
end;


{ TCanvas }

destructor TCanvas.Destroy;
begin
  DeleteObject(Brush.fHandle);
  DeleteObject(Pen.fHandle);
  fFont.Free;
  inherited;
end;

function TCanvas.GetFont: TFont;
begin
  if fFont=nil then
    fFont := TFont.Create;
  result := fFont;
end;

procedure TCanvas.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TCanvas.FillRect(const R: TRect);
begin
  Windows.FillRect(fHandle,R,Brush.GetHandle);
end;

procedure TCanvas.Rectangle(x1,y1,x2,y2: integer);
begin
  Pen.Select(fHandle);
  Windows.Rectangle(fHandle,x1,y1,x2,y2);
end;

procedure TCanvas.FrameRect(const Rect: TRect; cl1,cl2: integer);
begin
  Pen.Color := cl1;
  MoveTo(Rect.left,Rect.Bottom);
  LineTo(Rect.Left,Rect.Top);
  LineTo(Rect.Right,Rect.Top);
  Pen.Color := cl2;
  LineTo(Rect.Right,Rect.Bottom);
  LineTo(Rect.Left,Rect.Bottom);
end;

procedure TCanvas.MoveTo(x,y: integer);
begin
  Windows.MoveToEx(fHandle,x,y,nil);
end;

procedure TCanvas.LineTo(x,y: integer);
begin
  Pen.Select(fHandle);
  Windows.LineTo(fHandle,x,y);
end;

procedure TCanvas.PrepareText;
begin
  SelectObject(fHandle,Font.Handle);
  if Brush.Style=bsClear then
    SetBkMode(fHandle,TRANSPARENT) else
    SetBkColor(fHandle,Brush.fColor);
  SetTextColor(fHandle,Font.fColor);
end;

procedure TCanvas.TextOut(x,y: integer; const s: string);
begin
  if s='' then exit;
  PrepareText;
  Windows.TextOut(fHandle,x,y,pointer(s),length(s));
end;

procedure TCanvas.TextRect(const Rect: TRect; x, y: integer; const s: string);
begin
  if s='' then exit;
  PrepareText;
  // DrawText handles line breaks, not ExtTextOut (clip)
//  DrawText(fHandle, pointer(s), length(s), R, DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  Windows.ExtTextOut(fHandle,x,y,ETO_CLIPPED,@Rect,pointer(s),length(s),nil);
end;

function TCanvas.TextWidth(const s: string): integer;
var Size: TSize;
begin
  Windows.GetTextExtentPoint32(FHandle, pointer(s), Length(s), Size);
  result := Size.cX;
end;

function TCanvas.TextHeight(const s: string): integer;
var Size: TSize;
begin
  Windows.GetTextExtentPoint32(FHandle, pointer(s), Length(s), Size);
  result := Size.cY;
end;


{ TPicture }

destructor TPicture.Destroy;
begin
  FreeMem(fData);
  inherited;
end;

procedure TPicture.ReadProperty(const Name: string; Reader:TReader);
const
  TFontProperties:array[0..0] of PChar=(
   'Data'
  );
begin
  case StringIndex(Name,TFontProperties) of
    0 : fData := Reader.BinaryProperty(fSize);
   else inherited;
  end;
end;

type
  PBMP = ^TBMP;
  // match  DFM binary content
  TBMP = record
    ClassName: string[7]; // "TBitmap"
    Size: integer;
    FileHeader: TBitmapFileHeader;
    InfoHeader: TBitmapInfo;
  end;

procedure TPicture.DrawRect(const R: TRect; Canvas: TCanvas);
begin
  if fSize>=SizeOf(TBitmapFileHeader) then
  with PBMP(fData)^ do
  if (ClassName='TBitmap') and (FileHeader.bfType=$4D42) then
    SetDIBitsToDevice(
     Canvas.Handle, // handle of device context
     R.Left,        // x-coordinate of upper-left corner of dest. rect.
     R.Top,	        // y-coordinate of upper-left corner of dest. rect.
     InfoHeader.bmiHeader.biWidth,	// source rectangle width
     InfoHeader.bmiHeader.biHeight,	// source rectangle height
     0,	// x-coordinate of lower-left corner of source rect.
     0,	// y-coordinate of lower-left corner of source rect.
     0,	// first scan line in array
     InfoHeader.bmiHeader.biHeight,	// number of scan lines
     pointer(cardinal(@FileHeader)+FileHeader.bfOffBits),	// address of array with DIB bits
     InfoHeader,	// address of structure with bitmap info.
     DIB_RGB_COLORS 	// RGB or palette indices
    );
end;

procedure TPicture.LoadFromMemory(BufferBitmap: pointer; BufferSize: integer);
begin
  fSize := BufferSize;
  Getmem(fData,BufferSize+12);
  with PBMP(fData)^ do begin // mimic .dfm binary stream
    ClassName := 'TBitmap';
    Size := BufferSize;
    Move(BufferBitmap^,FileHeader,BufferSize-12);
  end;
end;

procedure TPicture.LoadFromResourceName(Instance: THandle; const ResName: string);
var HResInfo: THandle;
    HGlobal: THandle;
    HRes: pChar;
begin
  HResInfo := FindResource(Instance, pointer(ResName), 'BMP');
  if HResInfo = 0 then exit;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then exit;
  HRes := LockResource(HGlobal);
  fSize := SizeOfResource(Instance, HResInfo);
  Getmem(fData,fSize+12);
  with PBMP(fData)^ do begin // mimic .dfm binary stream
    ClassName := 'TBitmap';
    Size := fSize;
    Move(HRes^,FileHeader,fSize-12);
  end;
end;

end.
