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

unit Icons.Custom;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Controls, LCLIntf, LCLType, Graphics, BGRABitmap, Icons.Base, Types,
  gdk2pixbuf;

type

  { TGTK2CustomIcon }

  TGTK2CustomIcon = class(TBaseIcon)
  private
    function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
    function ExtractIcon(const sIconName: AnsiString; ALargeImage: Boolean
      ): TBitmap;
  protected
    function GetIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
  public
  end;

  TCustomIcon = TGTK2CustomIcon;

implementation

uses
   Kernel.Consts, BGRABitmapTypes, Kernel.Manager, GraphType,
   gtk2, glib2, gdk2, IntfGraphics;

{ TGTK2CustomIcon }

function TGTK2CustomIcon.PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
var
  width, height, rowstride, n_channels, i, j: Integer;
  pixels: Pguchar;
  pSrc: PByte;
  pDst: PLongWord;
  BmpData: TLazIntfImage;
  hasAlphaChannel: Boolean;
  QueryFlags: TRawImageQueryFlags = [riqfRGB];
  Description: TRawImageDescription;
  ARawImage: TRawImage;
begin
  Result := nil;

  n_channels:= gdk_pixbuf_get_n_channels(Pixbuf);

  if ((n_channels <> 3) and (n_channels <> 4)) or  // RGB or RGBA
     (gdk_pixbuf_get_colorspace(pixbuf) <> GDK_COLORSPACE_RGB) or
     (gdk_pixbuf_get_bits_per_sample(pixbuf) <> 8) then Exit;

  width:= gdk_pixbuf_get_width(Pixbuf);
  height:= gdk_pixbuf_get_height(Pixbuf);
  rowstride:= gdk_pixbuf_get_rowstride(Pixbuf);
  pixels:= gdk_pixbuf_get_pixels(Pixbuf);
  hasAlphaChannel:= gdk_pixbuf_get_has_alpha(Pixbuf);

  if hasAlphaChannel then
    Include(QueryFlags, riqfAlpha);

  BmpData := TLazIntfImage.Create(width, height, QueryFlags);
  try
    BmpData.CreateData;
    Description := BmpData.DataDescription;

    pDst := PLongWord(BmpData.PixelData);
    for j:= 0 to Height - 1 do
    begin
      pSrc := PByte(pixels) + j * rowstride;
      for i:= 0 to Width - 1 do
      begin
        pDst^ := pSrc[0] shl Description.RedShift +
                 pSrc[1] shl Description.GreenShift +
                 pSrc[2] shl Description.BlueShift;

        if hasAlphaChannel then
          pDst^ := pDst^ + pSrc[3] shl Description.AlphaShift;

        Inc(pSrc, n_channels);
        Inc(pDst);
      end;
    end;

    Result := TBitmap.Create;

    BmpData.GetRawImage(ARawImage, True);
    // Simply change raw image owner without data copy
    Result.LoadFromRawImage(ARawImage, True);

    if not hasAlphaChannel then
      Result.Transparent := True;

  finally
    BmpData.Free;
  end;
end;

function TGTK2CustomIcon.ExtractIcon(const sIconName: AnsiString;
  ALargeImage: Boolean): TBitmap;
var
  pbPicture: PGdkPixbuf = nil;
  Size: Integer;
begin
  Result := nil;

  if ALargeImage then
    Size := ICON_SIZE_LARGE
  else
    Size := ICON_SIZE_SMALL;

  try
    if ASuiteManager.IconsManager.CheckSystemIconName(sIconName) then
    begin
      if FileExists(sIconName) then
        pbPicture := gdk_pixbuf_new_from_file_at_size(PAnsiChar(sIconName), Size, Size, nil)
      else
        pbPicture := gtk_icon_theme_load_icon(gtk_icon_theme_get_for_screen(gdk_screen_get_default), Pgchar(sIconName), Size, GTK_ICON_LOOKUP_USE_BUILTIN, nil);
    end;

    if Assigned(pbPicture) then
      Result := PixBufToBitmap(pbPicture)
    else
      Result := TBitmap.Create;
  finally
    g_object_unref(pbPicture);
  end;
end;

function TGTK2CustomIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  iconName: AnsiString;
  bmp: TBitmap;
begin
  iconName := ASuiteManager.IconsManager.GetSystemIconName(APathFile);

  bmp := ExtractIcon(iconName, AWantLargeIcon);
  try
    Result := TBGRABitmap.Create(bmp);
  finally
    bmp.Free;
  end;
end;

end.
