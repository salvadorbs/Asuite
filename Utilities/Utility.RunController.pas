unit Utility.RunController;

{$MODE DelphiUnicode}

interface

uses
  Classes, ActnList, VirtualTrees, Kernel.Enumerations;

type
  TRunController = class
  public
    procedure ExecuteRun(const Tree: TBaseVirtualTree; const RunMode: TRunMode);
    procedure UpdateRunAction(Action: TAction; const Tree: TBaseVirtualTree);
  end;

implementation

uses
  Utility.System, VirtualTree.Methods, NodeDataTypes.Base, NodeDataTypes.Files;

procedure TRunController.ExecuteRun(const Tree: TBaseVirtualTree; const RunMode: TRunMode);
begin
  if Assigned(Tree) then
    TVirtualTreeMethods.ExecuteSelectedNodes(Tree, RunMode, False);
end;

procedure TRunController.UpdateRunAction(Action: TAction; const Tree: TBaseVirtualTree);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
  I: Integer;
begin
  if not Assigned(Action) or not Assigned(Tree) then Exit;
  Nodes := Tree.GetSortedSelection(True);
  Action.Enabled := False;

  for I := Low(Nodes) to High(Nodes) do
  begin
    NodeData := TVirtualTreeMethods.GetNodeItemData(Nodes[I], Tree);
    if not Assigned(NodeData) then
      Continue;

    if not (NodeData.IsSeparatorItem) then
      Action.Enabled := True;

    if ((Action.Tag = 1) or (Action.Tag = 2)) and (NodeData.IsFileItem) then
      Action.Enabled := IsExecutableFile(TvFileNodeData(NodeData).PathAbsoluteFile);

    if (Action.Tag = 3) then
    begin
      if NodeData.IsFileItem then
      begin
        if IsValidURLProtocol(TvFileNodeData(NodeData).PathAbsoluteFile) then
          Action.Enabled := False
      end
      else
        Action.Enabled := False;
    end;
  end;
end;

end.
