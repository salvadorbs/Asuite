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
  lazgdkpixbuf2;

type

  { TGtkCustomIcon }

  TGtkCustomIcon = class(TBaseIcon)
  private
    function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
    function ExtractIcon(const sIconName: AnsiString; ALargeImage: Boolean
      ): TBitmap;
  protected
    function GetIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
  public
  end;

  TCustomIcon = TGtkCustomIcon;

implementation

uses
   Kernel.Consts, BGRABitmapTypes, Kernel.Manager, GraphType, IntfGraphics,
   lazgtk3, LazGLib2, LazGdk3, LazGObject2;

{$I ../Icons.Pixbuf.inc}

function TGtkCustomIcon.ExtractIcon(const sIconName: AnsiString;
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
        pbPicture := gtk_icon_theme_load_icon(gtk_icon_theme_get_for_screen(gdk_screen_get_default), Pgchar(sIconName), Size, [GTK_ICON_LOOKUP_USE_BUILTIN], nil);
    end;

    if Assigned(pbPicture) then
      Result := PixBufToBitmap(pbPicture)
    else
      Result := TBitmap.Create;
  finally
    g_object_unref(pbPicture);
  end;
end;

function TGtkCustomIcon.GetIconFromFile(const APathFile: string;
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
