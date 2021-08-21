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
  SysUtils, Controls, LCLIntf, LCLType, Graphics, BGRABitmap, ShellApi,
  CommCtrl, uBitmap, Icons.Base;

type

  { TWin32CustomIcon }

  TWin32CustomIcon = class(TBaseIcon)
  private
    function BGRABitmapCreateFromHICON(AHIcon: HICON): TBGRABitmap;
    function ExtractIconFromSysImageList(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap;
  protected
    function GetIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
  public
  end;

  TCustomIcon = TWin32CustomIcon;

implementation

uses
   DataModules.Icons, Kernel.Consts, BGRABitmapTypes, Utility.FileFolder;

{ TWin32CustomIcon }

function TWin32CustomIcon.ExtractIconFromSysImageList(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  FileInfo: TSHFileInfoW;
  Flags: Integer;
begin
  Result := nil;

  Assert(Assigned(dmImages));

  if AWantLargeIcon then
    Flags := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;

  try
    if SHGetFileInfoW(PChar(APathFile), 0, FileInfo{%H-}, SizeOf(TSHFileInfo), Flags) <> 0 then
      Result := BGRABitmapCreateFromHICON(FileInfo.hIcon);
  finally
    DestroyIcon(FileInfo.hIcon);
  end;
end;

function TWin32CustomIcon.BGRABitmapCreateFromHICON(AHIcon: HICON): TBGRABitmap;
var
  bmpTemp: Graphics.TBitmap;
begin
  bmpTemp := BitmapCreateFromHICON(AHIcon);
  try
    Result := TBGRABitmap.Create(bmpTemp);
  finally
    FreeAndNil(bmpTemp);
  end;
end;

function TWin32CustomIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
begin
  Result := nil;

  if ExtractFileExtEx(APathFile) = EXT_ICO then
    Result := LoadFromFileIcon(APathFile, AWantLargeIcon)
  else
    Result := ExtractIconFromSysImageList(APathFile, AWantLargeIcon);

  if Result = nil then
    Result := TBGRABitmap.Create;
end;

end.
