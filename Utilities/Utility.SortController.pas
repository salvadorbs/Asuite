unit Utility.SortController;

{$MODE DelphiUnicode}

interface

uses
  VirtualTrees, ActnList;

type
  TSortController = class
  public
    procedure SortSelectedCategories(const Tree: TBaseVirtualTree; ListTree: TVirtualStringTree);
    procedure UpdateSortCategoriesAction(Action: TAction; const Tree: TBaseVirtualTree);
    procedure SortListTree(ListTree: TVirtualStringTree);
    procedure UpdateSortListAction(Action: TAction; ListTree: TVirtualStringTree; const ActiveTree: TBaseVirtualTree);
  end;

implementation

uses
  NodeDataTypes.Base, VirtualTree.Methods;

procedure TSortController.SortSelectedCategories(const Tree: TBaseVirtualTree; ListTree: TVirtualStringTree);
var
  Nodes: TNodeArray;
  I: Integer;
begin
  if not Assigned(Tree) then Exit;
  Nodes := Tree.GetSortedSelection(True);
  if Length(Nodes) > 0 then
  begin
    for I := Low(Nodes) to High(Nodes) do
      ListTree.Sort(Nodes[I], 0, sdAscending);
  end;
  TVirtualTreeMethods.RefreshList(ListTree);
end;

procedure TSortController.UpdateSortCategoriesAction(Action: TAction; const Tree: TBaseVirtualTree);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
  I: Integer;
begin
  if not Assigned(Action) or not Assigned(Tree) then Exit;
  Nodes := Tree.GetSortedSelection(True);
  Action.Visible := True;
  Action.Enabled := False;
  if Length(Nodes) > 0 then
  begin
    for I := Low(Nodes) to High(Nodes) do
    begin
      NodeData := TVirtualTreeMethods.GetNodeItemData(Nodes[I], Tree);
      if Assigned(NodeData) and NodeData.IsCategoryItem then
      begin
        Action.Enabled := True;
        Break;
      end;
    end;
  end;
end;

procedure TSortController.SortListTree(ListTree: TVirtualStringTree);
begin
  if Assigned(ListTree) then
  begin
    ListTree.SortTree(-1, sdAscending);
    TVirtualTreeMethods.RefreshList(ListTree);
  end;
end;

procedure TSortController.UpdateSortListAction(Action: TAction; ListTree: TVirtualStringTree; const ActiveTree: TBaseVirtualTree);
begin
  if not Assigned(Action) or not Assigned(ListTree) then Exit;
  Action.Visible := (ActiveTree = ListTree);
  Action.Enabled := (ListTree.RootNode.ChildCount > 1) and (ActiveTree = ListTree);
end;

end.
