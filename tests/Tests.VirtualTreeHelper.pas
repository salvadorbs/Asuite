unit Tests.VirtualTreeHelper;

{$mode objfpc}{$H+}

{ Behaviour tests for VirtualTree.Helper and the TVirtualTreeEvents wiring.

  The helper tests run on naked trees (default node data size, no node
  objects, no events): copying and moving then only shuffles raw node
  memory, so nothing can leak. The SetupVST* tests only check that every
  flavour wires the handlers it promises, on throwaway trees; invoking the
  handlers themselves is covered in Tests.VirtualTreeEvents. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  VirtualTree.Helper, VirtualTree.Events,
  Kernel.Types,
  DataModules.Icons;

type
  TTestVirtualTreeHelper = class(TTestCase)
  private
    function NewTreeWith(const ACount: Integer): TVirtualStringTree;
  published
    procedure TestFakeCopyMarksCopyPending;
    procedure TestFakeCutMarksCutPending;
    procedure TestMarkWithoutSelectionDoesNothing;
    procedure TestMarkReadOnlyDoesNothing;
    procedure TestFakePasteReadOnlyReturnsFalse;
    procedure TestPasteCopiesNodes;
    procedure TestPasteCutMovesNodes;
    procedure TestPasteIntoDescendantSkipped;
    procedure TestSecondMarkReplacesFirst;
    procedure TestPasteClearsPendingFlags;
    procedure TestPasteWithoutPendingCopiesNothing;
  end;

  TTestSetupWiring = class(TTestCase)
  private
    FEvents: TVirtualTreeEvents;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetupVSTListWiring;
    procedure TestSetupVSTListNodeDataSize;
    procedure TestSetupVSTSimpleWiring;
    procedure TestSetupVSTSearchWiring;
    procedure TestSetupVSTImportListWiring;
    procedure TestSetupVSTHotkeyWiring;
    procedure TestSetupVSTAutorunWiring;
    procedure TestSetupVSTDialogFrameWiring;
    procedure TestSetupVSTGraphicMenuWiring;
    procedure TestSetupsKeepTheirOwnHandlers;
  end;

implementation

{ TTestVirtualTreeHelper }

function TTestVirtualTreeHelper.NewTreeWith(
  const ACount: Integer): TVirtualStringTree;
var
  I: Integer;
begin
  Result := TVirtualStringTree.Create(nil);
  for I := 1 to ACount do
    Result.AddChild(nil);
end;

procedure TTestVirtualTreeHelper.TestFakeCopyMarksCopyPending;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.Selected[Tree.GetFirst] := True;
    Tree.FakeCopyToClipBoard;
    AssertTrue('copy pending set', tsCopyPending in Tree.TreeStates);
    AssertFalse('cut pending not set', tsCutPending in Tree.TreeStates);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestFakeCutMarksCutPending;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.Selected[Tree.GetFirst] := True;
    Tree.FakeCutToClipBoard;
    AssertTrue('cut pending set', tsCutPending in Tree.TreeStates);
    AssertFalse('copy pending not set', tsCopyPending in Tree.TreeStates);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestMarkWithoutSelectionDoesNothing;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.FakeCopyToClipBoard;
    Tree.FakeCutToClipBoard;
    AssertFalse('no copy without selection', tsCopyPending in Tree.TreeStates);
    AssertFalse('no cut without selection', tsCutPending in Tree.TreeStates);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestMarkReadOnlyDoesNothing;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.TreeOptions.MiscOptions :=
      Tree.TreeOptions.MiscOptions + [toReadOnly];
    Tree.Selected[Tree.GetFirst] := True;
    Tree.FakeCopyToClipBoard;
    Tree.FakeCutToClipBoard;
    AssertFalse('readonly never marks', tsCopyPending in Tree.TreeStates);
    AssertFalse('readonly never marks', tsCutPending in Tree.TreeStates);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestFakePasteReadOnlyReturnsFalse;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.TreeOptions.MiscOptions :=
      Tree.TreeOptions.MiscOptions + [toReadOnly];
    AssertFalse('readonly never pastes', Tree.FakePasteFromClipboard);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestPasteCopiesNodes;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(2);
  try
    // Multi-select, otherwise the second selection drops the first one
    Tree.TreeOptions.SelectionOptions :=
      Tree.TreeOptions.SelectionOptions + [toMultiSelect];
    Tree.Selected[Tree.GetFirst] := True;
    Tree.Selected[Tree.GetNextSibling(Tree.GetFirst)] := True;
    AssertEquals('both nodes selected', 2, Tree.SelectedCount);
    Tree.FakeCopyToClipBoard;
    AssertEquals('cutset holds both nodes', 2,
      Length(Tree.GetSortedCutCopySet(True)));
    // Paste at the root: selecting focuses, and the focused node would
    // otherwise become the paste target
    Tree.FocusedNode := nil;
    AssertTrue('paste succeeds', Tree.FakePasteFromClipboard);
    AssertEquals('copies are added', 4, Tree.RootNodeCount);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestPasteCutMovesNodes;
var
  Tree: TVirtualStringTree;
  FirstNode, SecondNode: PVirtualNode;
begin
  Tree := NewTreeWith(2);
  try
    FirstNode := Tree.GetFirst;
    SecondNode := Tree.GetNextSibling(FirstNode);
    Tree.Selected[FirstNode] := True;
    Tree.FakeCutToClipBoard;
    Tree.FocusedNode := nil;
    AssertTrue('paste succeeds', Tree.FakePasteFromClipboard);
    AssertEquals('cut nodes move instead of duplicating', 2,
      Tree.RootNodeCount);
    AssertTrue('first node moved to the end', Tree.GetFirst = SecondNode);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestPasteIntoDescendantSkipped;
var
  Tree: TVirtualStringTree;
  ParentNode, ChildNode: PVirtualNode;
begin
  Tree := NewTreeWith(0);
  try
    ParentNode := Tree.AddChild(nil);
    ChildNode := Tree.AddChild(ParentNode);
    Tree.Selected[ParentNode] := True;
    Tree.FakeCopyToClipBoard;
    // Pasting a node into its own descendant must not duplicate it
    Tree.FocusedNode := ChildNode;
    AssertTrue('paste succeeds', Tree.FakePasteFromClipboard);
    AssertEquals('ancestor skipped', 1, Tree.RootNodeCount);
    AssertTrue('nothing pasted under the child',
      Tree.GetFirstChild(ChildNode) = nil);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestSecondMarkReplacesFirst;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.Selected[Tree.GetFirst] := True;
    Tree.FakeCopyToClipBoard;
    Tree.FakeCutToClipBoard;
    AssertFalse('copy replaced by cut', tsCopyPending in Tree.TreeStates);
    AssertTrue('cut pending set', tsCutPending in Tree.TreeStates);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestPasteClearsPendingFlags;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(1);
  try
    Tree.Selected[Tree.GetFirst] := True;
    Tree.FakeCopyToClipBoard;
    Tree.FocusedNode := nil;
    Tree.FakePasteFromClipboard;
    AssertFalse('copy cleared', tsCopyPending in Tree.TreeStates);
    AssertFalse('cut cleared', tsCutPending in Tree.TreeStates);
    AssertEquals('copy landed at the root', 2, Tree.RootNodeCount);
  finally
    Tree.Free;
  end;
end;

procedure TTestVirtualTreeHelper.TestPasteWithoutPendingCopiesNothing;
var
  Tree: TVirtualStringTree;
begin
  Tree := NewTreeWith(2);
  try
    AssertTrue('vacuous paste succeeds', Tree.FakePasteFromClipboard);
    AssertEquals('nothing added', 2, Tree.RootNodeCount);
  finally
    Tree.Free;
  end;
end;

{ TTestSetupWiring }

procedure TTestSetupWiring.SetUp;
begin
  inherited SetUp;
  // Bare instance: the setups only need the global to exist for the
  // Images assignment (ilIcons stays nil without .lfm streaming)
  dmImages := TdmImages.CreateNew(nil);
  FEvents := TVirtualTreeEvents.Create;
end;

procedure TTestSetupWiring.TearDown;
begin
  FreeAndNil(dmImages);
  FreeAndNil(FEvents);
  inherited TearDown;
end;

procedure TTestSetupWiring.TestSetupVSTListWiring;
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTList(Tree);
    AssertTrue('OnGetText', Assigned(Tree.OnGetText));
    AssertTrue('OnCompareNodes', Assigned(Tree.OnCompareNodes));
    AssertTrue('OnFreeNode', Assigned(Tree.OnFreeNode));
    AssertTrue('OnSaveNode', Assigned(Tree.OnSaveNode));
    AssertTrue('OnLoadNode', Assigned(Tree.OnLoadNode));
    AssertTrue('OnNewText', Assigned(Tree.OnNewText));
    AssertTrue('OnDragDrop', Assigned(Tree.OnDragDrop));
    AssertTrue('OnEditing', Assigned(Tree.OnEditing));
    AssertTrue('OnMeasureItem', Assigned(Tree.OnMeasureItem));
    AssertTrue('OnGetHint', Assigned(Tree.OnGetHint));
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTListNodeDataSize;
var
  Tree: TVirtualStringTree;
  Size: Integer;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTList(Tree);
    Size := 0;
    Tree.OnGetNodeDataSize(Tree, Size);
    AssertEquals('list nodes hold rBaseData', SizeOf(rBaseData), Size);
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTSimpleWiring;
var
  Tree: TVirtualStringTree;
  Size: Integer;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTSimple(Tree);
    AssertTrue('OnGetText', Assigned(Tree.OnGetText));
    AssertTrue('OnGetImageIndex', Assigned(Tree.OnGetImageIndex));
    Size := 0;
    Tree.OnGetNodeDataSize(Tree, Size);
    AssertEquals('simple nodes hold rTreeDataX', SizeOf(rTreeDataX), Size);
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTSearchWiring;
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTSearch(Tree);
    AssertTrue('OnCompareNodes', Assigned(Tree.OnCompareNodes));
    AssertTrue('OnKeyPress', Assigned(Tree.OnKeyPress));
    AssertTrue('OnColumnResize', Assigned(Tree.OnColumnResize));
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTImportListWiring;
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTImportList(Tree);
    AssertTrue('OnFreeNode', Assigned(Tree.OnFreeNode));
    AssertTrue('OnDrawText', Assigned(Tree.OnDrawText));
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTHotkeyWiring;
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTHotkey(Tree);
    AssertTrue('OnCompareNodes', Assigned(Tree.OnCompareNodes));
    AssertTrue('OnBeforeCellPaint', Assigned(Tree.OnBeforeCellPaint));
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTAutorunWiring;
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTAutorun(Tree);
    AssertTrue('OnGetText', Assigned(Tree.OnGetText));
    AssertTrue('OnBeforeCellPaint', Assigned(Tree.OnBeforeCellPaint));
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTDialogFrameWiring;
var
  Tree: TVirtualStringTree;
  Size: Integer;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTDialogFrame(Tree);
    AssertTrue('OnAddToSelection', Assigned(Tree.OnAddToSelection));
    AssertTrue('OnFreeNode', Assigned(Tree.OnFreeNode));
    Size := 0;
    Tree.OnGetNodeDataSize(Tree, Size);
    AssertEquals('frame nodes hold rFramesNodeData', SizeOf(rFramesNodeData),
      Size);
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupVSTGraphicMenuWiring;
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    // A nil menu is fine: the setup only stores the reference, and no
    // graphic-menu handler is invoked here
    FEvents.SetupVSTGraphicMenu(Tree, nil);
    AssertTrue('OnClick', Assigned(Tree.OnClick));
    AssertTrue('OnCompareNodes', Assigned(Tree.OnCompareNodes));
    AssertTrue('OnScroll', Assigned(Tree.OnScroll));
    AssertTrue('OnResize', Assigned(Tree.OnResize));
  finally
    Tree.Free;
  end;
end;

procedure TTestSetupWiring.TestSetupsKeepTheirOwnHandlers;
var
  ListTree, HotkeyTree: TVirtualStringTree;
begin
  ListTree := TVirtualStringTree.Create(nil);
  HotkeyTree := TVirtualStringTree.Create(nil);
  try
    FEvents.SetupVSTList(ListTree);
    FEvents.SetupVSTHotkey(HotkeyTree);
    AssertTrue('flavours wire different text handlers',
      TMethod(ListTree.OnGetText).Code <> TMethod(HotkeyTree.OnGetText).Code);
  finally
    ListTree.Free;
    HotkeyTree.Free;
  end;
end;

initialization
  RegisterTest(TTestVirtualTreeHelper);
  RegisterTest(TTestSetupWiring);

end.
