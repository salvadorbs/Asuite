{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit DataModules.Icons;

{$MODE DelphiUnicode}

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, LCLIntf, LCLType, Graphics, Dialogs, SyncObjs,
  BGRAImageList{$IFDEF Windows}, CommCtrl, Windows{$ENDIF};

type

  {$IFDEF MSWINDOWS}
  THANDLE = Windows.THANDLE;
  {$ENDIF}

  { TdmImages }

  TdmImages = class(TDataModule)
    ilIcons: TBGRAImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }                          
    FLock: SyncObjs.TCriticalSection;

    procedure DrawTransparentBitmap(DC: HDC; hBmp: HBITMAP; xStart: integer;
                                    yStart : integer; cTransparentColor : COLORREF);
  public
    procedure GetAlphaBitmapFromImageList(ABMP: Graphics.TBitmap; const AImageIndex: Integer);
    procedure DrawIconInBitmap(const AGlyph: Graphics.TBitmap; const AImageIndex: Integer);

    function AddIcon(Images: Graphics.TBitmap): Integer;
    function AddMultipleResolutions(Images: array of TCustomBitmap): Integer;

    property Lock: SyncObjs.TCriticalSection read FLock;
  end;

var
  dmImages: TdmImages;

implementation

uses
  ImgList, Kernel.Consts;

{$R *.lfm}

{ TdmImages }

procedure TdmImages.DrawIconInBitmap(const AGlyph: Graphics.TBitmap;
  const AImageIndex: Integer);
var
  BMP: Graphics.TBitmap;
begin
  AGlyph.Assign(nil);

  if AImageIndex <> -1 then
  begin
    //Buttons' image
    BMP := Graphics.TBitmap.Create;
    try
      ilIcons.GetBitmap(AImageIndex, BMP);
      AGlyph.Assign(BMP);
      DrawTransparentBitmap(AGlyph.Canvas.Handle, BMP.Handle, 0, 0, clWhite);
    finally
      BMP.Free;
    end;
  end;
end;

function TdmImages.AddIcon(Images: Graphics.TBitmap): Integer;
begin
  Result := -1;

  if Assigned(Images) then
    Result := ilIcons.Add(Images, nil);
end;

function TdmImages.AddMultipleResolutions(Images: array of TCustomBitmap): Integer;
begin                    
  FLock.Acquire;
  try
    Result := ilIcons.AddMultipleResolutions(Images);
  finally     
    FLock.Release;
  end;
end;

procedure TdmImages.GetAlphaBitmapFromImageList(ABMP: Graphics.TBitmap;
  const AImageIndex: Integer);
{$IFDEF MSWINDOWS}
var
  Images: TCustomImageListResolution;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if AImageIndex <> -1 then
  begin
    //Button's image
    ilIcons.FindResolution(ICON_SIZE_SMALL, Images);

    Images.GetBitmap(AImageIndex, ABMP);
  end;
  {$ENDIF}
end;

procedure TdmImages.DataModuleCreate(Sender: TObject);
begin
  //Small and large icons in a single ImageList
  ilIcons.RegisterResolutions([ICON_SIZE_SMALL, ICON_SIZE_LARGE]);
  ilIcons.Scaled := True;

  FLock := SyncObjs.TCriticalSection.Create;
end;

procedure TdmImages.DataModuleDestroy(Sender: TObject);
begin
  FLock.Free;
end;

procedure TdmImages.DrawTransparentBitmap(DC: HDC; hBmp: HBITMAP;
  xStart: integer; yStart: integer; cTransparentColor: COLORREF);
var
      bm:                                                  BITMAP;
      cColor:                                              COLORREF;
      bmAndBack, bmAndObject, bmAndMem, bmSave:            HBITMAP;
      bmBackOld, bmObjectOld, bmMemOld, bmSaveOld:         HBITMAP;
      hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave:        HDC;
      ptSize:                                              TPOINT;

begin
   hdcTemp := CreateCompatibleDC(dc);
   SelectObject(hdcTemp, hBmp);   // Select the bitmap

   GetObject(hBmp, sizeof(BITMAP), @bm);
   ptSize.x := bm.bmWidth;            // Get width of bitmap
   ptSize.y := bm.bmHeight;           // Get height of bitmap
   DPtoLP(hdcTemp, ptSize, 1);        // Convert from device
                                      // to logical points

   // Create some DCs to hold temporary data.
   hdcBack   := CreateCompatibleDC(dc);
   hdcObject := CreateCompatibleDC(dc);
   hdcMem    := CreateCompatibleDC(dc);
   hdcSave   := CreateCompatibleDC(dc);

   // Create a bitmap for each DC. DCs are required for a number of
   // GDI functions.

   // Monochrome DC
   bmAndBack   := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

   // Monochrome DC
   bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

   bmAndMem    := CreateCompatibleBitmap(dc, ptSize.x, ptSize.y);
   bmSave      := CreateCompatibleBitmap(dc, ptSize.x, ptSize.y);

   // Each DC must select a bitmap object to store pixel data.
   bmBackOld   := SelectObject(hdcBack, bmAndBack);
   bmObjectOld := SelectObject(hdcObject, bmAndObject);
   bmMemOld    := SelectObject(hdcMem, bmAndMem);
   bmSaveOld   := SelectObject(hdcSave, bmSave);

   // Set proper mapping mode.
   SetMapMode(hdcTemp, GetMapMode(dc));

   // Save the bitmap sent here, because it will be overwritten.
   BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY);

   // Set the background color of the source DC to the color.
   // contained in the parts of the bitmap that should be transparent
   cColor := SetBkColor(hdcTemp, cTransparentColor);

   // Create the object mask for the bitmap by performing a BitBlt
   // from the source bitmap to a monochrome bitmap.
   BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0,
          SRCCOPY);

   // Set the background color of the source DC back to the original
   // color.
   SetBkColor(hdcTemp, cColor);

   // Create the inverse of the object mask.
   BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
          NOTSRCCOPY);

   // Copy the background of the main DC to the destination.
   BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, dc, xStart, yStart,
          SRCCOPY);

   // Mask out the places where the bitmap will be placed.
   BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

   // Mask out the transparent colored pixels on the bitmap.
   BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);

   // XOR the bitmap with the background on the destination DC.
   BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCPAINT);

   // Copy the destination to the screen.
   BitBlt(dc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0,
          SRCCOPY);

   // Place the original bitmap back into the bitmap sent here.
   BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

   // Delete the memory bitmaps.
   DeleteObject(SelectObject(hdcBack, bmBackOld));
   DeleteObject(SelectObject(hdcObject, bmObjectOld));
   DeleteObject(SelectObject(hdcMem, bmMemOld));
   DeleteObject(SelectObject(hdcSave, bmSaveOld));

   // Delete the memory DCs.
   DeleteDC(hdcMem);
   DeleteDC(hdcBack);
   DeleteDC(hdcObject);
   DeleteDC(hdcSave);
   DeleteDC(hdcTemp);
end;

end.

