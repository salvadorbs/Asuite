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

unit Kernel.PopupMenu;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Menus, VirtualTrees, NodeDataTypes.Base,
  LCLIntf, LCLType, LMessages, Messages;

type

  //ASuite TrayIcon Menu
  TASMenuItem = class(TMenuItem)
  private
    FData: TvBaseNodeData;
    FpNode: PVirtualNode;
    FPath: string;

    procedure SetData(const Value: TvBaseNodeData);
    procedure SetpNode(const Value: PVirtualNode);
  public
    constructor Create(AOwner: TComponent); override;

    property Data  : TvBaseNodeData read FData write SetData;
    property pNode : PVirtualNode read FpNode write SetpNode;
    property Path  : string read FPath write FPath;
  end;

implementation

{ TASMenuItem }

constructor TASMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData  := nil;
  FpNode := nil;
end;

procedure TASMenuItem.SetData(const Value: TvBaseNodeData);
begin
  FData := Value;
end;

procedure TASMenuItem.SetpNode(const Value: PVirtualNode);
begin
  FpNode := Value;
end;

end.
