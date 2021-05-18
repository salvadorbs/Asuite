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
  qt5, qtobjects;

type

  { TQT5CustomIcon }

  TQT5CustomIcon = class(TBaseIcon)
  private
    function QIconToHBitmap(AIcon: QIconH; ASize: Types.TSize): HBITMAP;
    function ExtractIcon(const sIconName: WideString; ALargeImage: Boolean): TBitmap;
  protected
    function GetIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
  public
  end;

  TCustomIcon = TQT5CustomIcon;

implementation

uses
   Kernel.Consts, BGRABitmapTypes, Utility.System, Kernel.Manager;

{ TQT5CustomIcon }

function TQT5CustomIcon.QIconToHBitmap(AIcon: QIconH; ASize: Types.TSize
  ): HBITMAP;
var
  AImage: QImageH;
  APixmap: QPixmapH;
begin
  APixmap := QPixmap_create();
  QIcon_pixmap(AIcon, APixmap, Types.PSize(@ASize));

  AImage := QImage_create();
  QPixmap_toImage(APixmap, AImage);
  QPixmap_destroy(APixmap);

  Result := HBitmap(TQtImage.Create(AImage));
end;

function TQT5CustomIcon.ExtractIcon(const sIconName: WideString;
  ALargeImage: Boolean): TBitmap;
var
  QIcon: QIconH;
  Size: Types.TSize;
begin
  Result := TBitmap.Create;

  if ALargeImage then
    Size := Types.Size(ICON_SIZE_LARGE, ICON_SIZE_LARGE)
  else
    Size := Types.Size(ICON_SIZE_SMALL, ICON_SIZE_SMALL);

  QIcon := QIcon_Create();
  try
    if ASuiteManager.IconsManager.CheckSystemIconName(sIconName) then
    begin
      QIcon_fromTheme(QIcon, @sIconName);
      Result.Handle := QIconToHBitmap(QIcon, Size);
    end;
  finally
    QIcon_destroy(QIcon);
  end;
end;

function TQT5CustomIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  iconName: String;
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
