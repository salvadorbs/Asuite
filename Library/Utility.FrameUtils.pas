unit Utility.FrameUtils;

interface

uses
  Windows, SysUtils, ActiveX, VirtualTrees, Controls, ulCommonClasses, AppConfig,
  ulNodeDataTypes, ulEnumerations, Classes, ShellApi, comobj, BaseEntityPage, extctrls;

function AddFrameNode(Tree: TBaseVirtualTree; Parent: PVirtualNode;
                      FramePage: TPageFrameClass): PVirtualNode;
function GetNodeByFrameClass(Tree: TBaseVirtualTree; AFramePage: TPageFrameClass; Node: PVirtualNode = nil):PVirtualNode;
procedure LoadPage(var CurrentPage: TfrmBaseEntityPage; NewPage: TPageFrameClass; ParentPanel: TPanel);

implementation

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
