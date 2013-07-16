unit ulFrameUtils;

interface

uses
  Windows, SysUtils, ActiveX, VirtualTrees, Controls, ulCommonClasses, AppConfig,
  ulNodeDataTypes, ulEnumerations, Classes, ShellApi, comobj, BaseEntityPage, extctrls;

function AddFrameNode(Tree: TBaseVirtualTree; Parent: PVirtualNode;
                      FramePage: TPageFrameClass; ImageIndex: Integer): PVirtualNode;
function GetNodeByFrameClass(Tree: TBaseVirtualTree; AFramePage: TPageFrameClass):PVirtualNode;
procedure LoadPage(var CurrentPage: TfrmBaseEntityPage; NewPage: TPageFrameClass; ParentPanel: TPanel);

implementation

function AddFrameNode(Tree: TBaseVirtualTree;Parent: PVirtualNode; FramePage: TPageFrameClass; ImageIndex: Integer): PVirtualNode;
var
  NodeData: PFramesNodeData;
begin
  Result   := Tree.AddChild(Parent);
  NodeData := Tree.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    NodeData.Frame := FramePage;
    NodeData.Title := TfrmBaseEntityPage(FramePage).Title;
    NodeData.ImageIndex := ImageIndex;
  end;
end;

function GetNodeByFrameClass(Tree: TBaseVirtualTree; AFramePage: TPageFrameClass): PVirtualNode;
var
  node:PVirtualNode;
  nodeData:PFramesNodeData;
begin
  Result := nil;
  node   := Tree.GetFirst();
  while Assigned(node) do begin
    nodeData := Tree.GetNodeData(node);
    if TfrmBaseEntityPage(nodeData.Frame).ClassName = AFramePage.ClassName then
      Exit(node);
    node := Tree.GetNextSibling(node);
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
