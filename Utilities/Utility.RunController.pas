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
  RunMode: TRunMode;
begin
  if not Assigned(Action) or not Assigned(Tree) then Exit;
  Nodes := Tree.GetSortedSelection(True);
  Action.Enabled := False;
  RunMode := TRunMode(Action.Tag);

  // The action is enabled when at least one selected node can handle it.
  // The result does not depend on the order of the selection.
  for I := Low(Nodes) to High(Nodes) do
  begin
    NodeData := TVirtualTreeMethods.GetNodeItemData(Nodes[I], Tree);
    if not Assigned(NodeData) then
      Continue;

    case RunMode of
      rmNormal:
        // Run: any node but a separator (categories run their children).
        if not NodeData.IsSeparatorItem then
        begin
          Action.Enabled := True;
          Break;
        end;

      rmAsUser, rmAsAdmin:
        // Run as/as admin: categories are allowed, files must be executable.
        if (not NodeData.IsSeparatorItem) and
           ((not NodeData.IsFileItem) or
            IsExecutableFile(TvFileNodeData(NodeData).PathAbsoluteFile)) then
        begin
          Action.Enabled := True;
          Break;
        end;

      rmExplorePath:
        // Open file location: only real file paths, not URL protocols.
        if NodeData.IsFileItem and
           (not IsValidURLProtocol(TvFileNodeData(NodeData).PathAbsoluteFile)) then
        begin
          Action.Enabled := True;
          Break;
        end;
    end;
  end;
end;

end.
