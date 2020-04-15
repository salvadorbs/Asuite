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

unit Icons.Application;

interface

uses
  SysUtils, Classes, Icons.Base;

type
  TApplicationIcon = class(TBaseIcon)
  private
    FPathFile: string;
    function GetName: string;
  public
    constructor Create(APathFile: string);

    function LoadIcon: Integer; override;

    property Name: string read GetName;
    property PathFile: string read FPathFile write FPathFile;
  end;

implementation

constructor TApplicationIcon.Create(APathFile: string);
begin
  inherited Create;
  FPathFile := APathFile;
end;

function TApplicationIcon.GetName: string;
begin
  Result := '';
  if FPathFile <> '' then
  begin
    Result := ExtractFileName(FPathFile);
    Result := ChangeFileExt(Result, '');
  end;
end;

function TApplicationIcon.LoadIcon: Integer;
begin
  Result := -1;
  if FileExists(FPathFile) then
    Result := InternalGetImageIndex(FPathFile);
end;

end.
