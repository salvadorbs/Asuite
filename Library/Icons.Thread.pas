{
Copyright (C) 2006-2015 Matteo Salvi

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

unit Icons.Thread;

interface

uses
  Classes, VirtualTrees;

type
  TTreeIconsThread = class(TThread)
  private
    FSenderTree : TBaseVirtualTree;
    procedure GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
  public
    constructor Create(ASenderTree: TBaseVirtualTree);
    procedure Execute; override;
  end;

implementation

uses
  VirtualTree.Methods, NodeDataTypes.Base;

{ TTreeIconsThread }

constructor TTreeIconsThread.Create(ASenderTree: TBaseVirtualTree);
begin
  inherited Create(True);
  //Init thread with base properties
  FSenderTree := ASenderTree;

  FreeOnTerminate := True;
end;

procedure TTreeIconsThread.Execute;
begin
  FSenderTree.IterateSubtree(nil, GetImageIndex, nil)
end;

procedure TTreeIconsThread.GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    NodeData.Icon.ImageIndex;
end;

end.
