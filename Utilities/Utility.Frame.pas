{
Copyright (C) 2006-2013 Matteo Salvi

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

unit Utility.Frame;

interface

uses
  Windows, SysUtils, ActiveX, VirtualTrees, Controls, Kernel.Consts,
  Classes, ShellApi, comobj, extctrls, Frame.BaseEntity;

function AddFrameNode(Tree: TBaseVirtualTree; Parent: PVirtualNode;
                      FramePage: TPageFrameClass): PVirtualNode;
function GetNodeByFrameClass(Tree: TBaseVirtualTree; AFramePage: TPageFrameClass; Node: PVirtualNode = nil):PVirtualNode;
procedure LoadPage(var CurrentPage: TfrmBaseEntityPage; NewPage: TPageFrameClass; ParentPanel: TPanel);

implementation

uses
  Kernel.Types;

function AddFrameNode(Tree: TBaseVirtualTree;Parent: PVirtualNode; FramePage: TPageFrameClass): PVirtualNode;
var
  NodeData: PFramesNodeData;
begin
  Result   := Tree.AddChild(Parent);
  NodeData := Tree.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    NodeData.Frame := FramePage;
    NodeData.Title := TfrmBaseEntityPage(FramePage).Title;
    NodeData.ImageIndex := TfrmBaseEntityPage(FramePage).ImageIndex;
  end;
end;

function GetNodeByFrameClass(Tree: TBaseVirtualTree; AFramePage: TPageFrameClass; Node: PVirtualNode): PVirtualNode;
var
  nodeData:PFramesNodeData;
begin
  Result := nil;
  if Node = nil then
    Node := Tree.GetFirst;
  while Assigned(Node) do begin
    nodeData := Tree.GetNodeData(Node);
    if TfrmBaseEntityPage(nodeData.Frame).ClassName = AFramePage.ClassName then
      Exit(Node);
    if Node.ChildCount > 0 then
      Result := GetNodeByFrameClass(Tree, AFramePage, Node.FirstChild);
    Node := Tree.GetNextSibling(Node);
  end;
end;

procedure LoadPage(var CurrentPage: TfrmBaseEntityPage; NewPage: TPageFrameClass; ParentPanel: TPanel);
begin
  if NewPage <> nil then
  begin
    if CurrentPage <> nil then
    begin
      if CurrentPage.ClassType = NewPage then
        Exit
      else
       CurrentPage.Visible := False;
    end;
    CurrentPage := TfrmBaseEntityPage(NewPage);
    CurrentPage.Parent  := ParentPanel;
    CurrentPage.Align   := alClient;
    CurrentPage.Visible := True;
  end;
end;

end.
