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

unit Scoop.App;

interface

uses
  classes, SysUtils, System.Generics.Collections;

type

  TScoopApp = class
  private
    { private declarations }
    FName: string;
    FVersion: string;
    FLatestVersion: string;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AName: string);
    destructor Destroy(); override;

    property Name: string read FName;
    property Version: string read FVersion write FVersion;
    property LatestVersion: string read FLatestVersion write FLatestVersion;
  end;

  //TODO: Move in another unit
  TScoopApps = class(TList<TScoopApp>)
  public
    function Find(const AName: string): Integer;
  end;

implementation

{ TScoopApp }

constructor TScoopApp.Create(AName: string);
begin
  FName := AName;
end;

destructor TScoopApp.Destroy;
begin

end;

{ TScoopApps }

function TScoopApps.Find(const AName: string): Integer;
begin
  for Result := 0 to Count-1 do
    if Self[Result].Name = AName then
      exit;
  Result := -1;
end;

end.
