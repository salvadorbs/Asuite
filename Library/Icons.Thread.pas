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

unit Icons.Thread;

{$MODE DelphiUnicode}

interface

uses
  Classes, VirtualTrees;

type
  TTreeIconsThread = class(TThread)
  private
    FSenderTree : TBaseVirtualTree;
    FParentNode : PVirtualNode;
    procedure GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
  public
    constructor Create(ASenderTree: TBaseVirtualTree; AParentNode : PVirtualNode);
    procedure Execute; override;
  end;

implementation

uses
  VirtualTree.Methods, NodeDataTypes.Base, AppConfig.Main, Kernel.Enumerations,
  Kernel.Logger, mormot.core.log;

{ TTreeIconsThread }

constructor TTreeIconsThread.Create(ASenderTree: TBaseVirtualTree; AParentNode : PVirtualNode);
begin
  inherited Create(True);
  TASuiteLogger.Info('Start thread to get all icons', []);
  //Init thread with base properties
  FSenderTree := ASenderTree;
  FParentNode := AParentNode;

  FreeOnTerminate := True;
end;

procedure TTreeIconsThread.Execute;
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TTreeIconsThread.Execute', Self);
  FSenderTree.IterateSubtree(FParentNode, GetImageIndex, nil);
end;

procedure TTreeIconsThread.GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  if Config.ASuiteState = lsNormal then
  begin
    NodeData := TVirtualTreeMethods.GetNodeItemData(Node, Sender);
    if Assigned(NodeData) then
      NodeData.Icon.ImageIndex;
  end
  else
    Abort := True;
end;

end.
