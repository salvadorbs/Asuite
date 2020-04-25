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

unit Scoop.Bucket;

interface

uses
  classes, SysUtils;

type

  TScoopBucket = class
  private
    { private declarations }
    FName: string;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AName: string);
    destructor Destroy(); override;

    property Name: string read FName;
  end;

implementation

{ TScoopBucket }

constructor TScoopBucket.Create(AName: string);
begin
  Self.FName := AName;
end;

destructor TScoopBucket.Destroy;
begin

end;

end.
