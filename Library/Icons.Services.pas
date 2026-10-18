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

unit Icons.Services;

{$MODE DelphiUnicode}

interface

type
  ICachePolicy = interface
    ['{8A3F1C2D-4B5E-4C7A-9D8E-1F2A3B4C5D6E}']
    function UseCache: Boolean;
    function IsImporting: Boolean;
  end;

  IFileOps = interface
    ['{1B2C3D4E-5F60-47A8-9B0C-1234567890AB}']
    function FileExists(const APath: string): Boolean;
    function PathExists(const APath: string): Boolean;
  end;

implementation

end.
