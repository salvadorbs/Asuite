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

unit Icons.Files;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Icons.Base;

type

  { TFileIcon }

  TFileIcon = class(TBaseIcon)
  private
    FPathFile: string;
  protected
    function GetName: string; override;
    function LoadIcon: Integer; override;
    function GetDefaultPathIcon: string; override;
  public
    constructor Create(APathFile: string; AStatic: Boolean = False);

    property PathFile: string read FPathFile write FPathFile;
  end;

implementation

uses
  LazFileUtils;

constructor TFileIcon.Create(APathFile: string; AStatic: Boolean);
begin
  inherited Create(AStatic);
  FPathFile := APathFile;
end;

function TFileIcon.GetName: string;
begin
  Result := '';
  if FPathFile <> '' then
    Result := LowerCase(ExtractFileNameOnly(FPathFile));
end;

function TFileIcon.LoadIcon: Integer;
begin
  Result := -1;

  if FileExists(FPathFile) then
    Result := inherited;
end;

function TFileIcon.GetDefaultPathIcon: string;
begin
  Result := FPathFile;
end;

end.
