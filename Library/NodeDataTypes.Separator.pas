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

unit NodeDataTypes.Separator;

{$MODE DelphiUnicode}

interface

uses
  Kernel.Enumerations, NodeDataTypes.Base;

type
  TvSeparatorNodeData = class(TvBaseNodeData)
  public
    //Specific properties
    constructor Create; overload;
  end;
  PvSeparatorNodeData = ^TvSeparatorNodeData;

implementation

constructor TvSeparatorNodeData.Create;
begin
  inherited Create(vtdtSeparator);
  Name := '';
end;

end.
