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

unit Icons.ExtFile;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Icons.Base, Icons.Custom;

type

  { TExtFileIcon }

  TExtFileIcon = class(TCustomIcon)
  private
    FExtFile: string;
  protected
    function GetName: string; override;
    function LoadIcon: Integer; override;
    // Inherits default GetDefaultPathIcon = '' (no cache file for ext icons)
  public
    constructor Create(AExtFile: string);

    property ExtFile: string read FExtFile write FExtFile;
  end;

implementation

uses
  SystemIcons.Ext;

constructor TExtFileIcon.Create(AExtFile: string);
begin
  inherited Create;
  FExtFile := SystemIcons.Ext.NormalizeExtKey(AExtFile);
end;

function TExtFileIcon.GetName: string;
begin
  Result := FExtFile;
end;

function TExtFileIcon.LoadIcon: Integer;
begin
  Result := -1;
  if (FExtFile <> '') then
    Result := InternalGetImageIndex(FExtFile);
end;

end.
