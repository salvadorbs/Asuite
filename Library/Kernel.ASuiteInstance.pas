{
Copyright (C) 2006-2020 Matteo Salvi

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

unit Kernel.ASuiteInstance;

interface

uses
  Windows, USingleInst, Messages, xxHash32, Forms, SysUtils;

type
  TASuiteSingleInst = class(TSingleInst)
  protected
    function WdwClassName: string; override;
    function WaterMark: DWORD; override;
  end;

implementation

{ TASuiteSingleInst }

function TASuiteSingleInst.WaterMark: DWORD;
begin
  Result := $DE1F1DAB;
end;

function TASuiteSingleInst.WdwClassName: string;
var
  str: string;
begin
  str := Application.ExeName;
  Result := 'ASuite.SingleInstance.' + IntToStr(TxxHash32.CalculateHash32(str[low(str)],
            Length(str) * SizeOf(Char)));
end;

initialization

RegisterSingleInstClass(TASuiteSingleInst);

end.
