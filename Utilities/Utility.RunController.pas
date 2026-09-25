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
  Utility.System, Utility.RunActionLogic, VirtualTree.Methods,
  NodeDataTypes.Base, NodeDataTypes.Files;

procedure TRunController.ExecuteRun(const Tree: TBaseVirtualTree; const RunMode: TRunMode);
begin
  if Assigned(Tree) then
    TVirtualTreeMethods.ExecuteSelectedNodes(Tree, RunMode, False);
end;

procedure TRunController.UpdateRunAction(Action: TAction; const Tree: TBaseVirtualTree);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
  Items: TRunActionItems;
  I: Integer;
begin
  if not Assigned(Action) or not Assigned(Tree) then Exit;

  Nodes := Tree.GetSortedSelection(True);
  SetLength(Items, Length(Nodes));
  for I := Low(Nodes) to High(Nodes) do
  begin
    // A node without data is treated as neutral so it never enables the action.
    Items[I].IsSeparator := True;
    NodeData := TVirtualTreeMethods.GetNodeItemData(Nodes[I], Tree);
    if Assigned(NodeData) then
    begin
      Items[I].IsSeparator  := NodeData.IsSeparatorItem;
      Items[I].IsFileItem   := NodeData.IsFileItem;
      if NodeData.IsFileItem then
      begin
        Items[I].IsExecutable  := IsExecutableFile(TvFileNodeData(NodeData).PathAbsoluteFile);
        Items[I].IsUrlProtocol := IsValidURLProtocol(TvFileNodeData(NodeData).PathAbsoluteFile);
      end;
    end;
  end;

  Action.Enabled := IsRunActionEnabled(TRunMode(Action.Tag), Items);
end;

end.
