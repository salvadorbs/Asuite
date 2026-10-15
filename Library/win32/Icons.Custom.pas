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
  SysUtils, Controls, LCLIntf, LCLType, Graphics, BGRABitmap, Icons.Base;

type

  { TWin32CustomIcon }

  TWin32CustomIcon = class(TBaseIcon)
  protected
    function GetIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
  public
  end;

  TCustomIcon = TWin32CustomIcon;

implementation

uses
   Kernel.Consts, Kernel.Manager;

{ TWin32CustomIcon }

function TWin32CustomIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  Size: Integer;
begin
  if AWantLargeIcon then
    Size := ICON_SIZE_LARGE
  else
    Size := ICON_SIZE_SMALL;

  Result := ASuiteManager.IconsManager.GetSystemIcon(APathFile, Size);
  if Result = nil then
    Result := TBGRABitmap.Create;
end;

end.
