unit Tests.VirtualTreeMethods;

{$mode objfpc}{$H+}

{ Behaviour tests for TVirtualTreeMethods (VirtualTree.Methods).

  Creation, lookup and the pure iterator callbacks run on the shared tree
  fixture. Sub-tree tests (search/menu trees holding rTreeDataX records that
  point back at main-tree nodes) use a second tree per test class.

  Deliberately excluded: anything that shows a dialog (AddChildNodeByGUI,
  ShowItemProperty), launches processes (ExecuteNode, ExecuteSelectedNodes)
  or saves the list (RefreshList), plus the icon thread (GetAllIcons). }

interface

uses
  Classes, SysUtils, LCLProc, Graphics, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  NodeDataTypes.Category, NodeDataTypes.Separator,
  VirtualTree.Methods,
  Kernel.Enumerations, Kernel.Types, Kernel.Consts,
  Lists.Base,
  AppConfig.Main, Kernel.Instance,
  Tests.Helpers.Tree;

type
  TTestCreateNodeData = class(TTestCase)
  published
    procedure TestCategoryBranch;
    procedure TestFileBranch;
    procedure TestFolderBranch;
    procedure TestSeparatorBranch;
    procedure TestUnknownTypeReturnsNil;
  end;

  TTestAddChildNodeEx = class(TNodeTreeTestCase)
  published
    procedure TestNilParentAddsRoot;
    procedure TestCategoryParentAddsChildAndExpands;
    procedure TestCategoryParentWithoutAutoExpand;
    procedure TestCategoryParentWithoutDoExpand;
    procedure TestFileParentInsertsSiblingAfter;
    procedure TestFileParentInsertsSiblingBefore;
    procedure TestPointerNodeAlwaysSet;
  end;

  TTestNodeDataResolution = class(TNodeTreeTestCase)
  private
    FSubTree: TVirtualStringTree;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMainTreeNodeResolves;
    procedure TestSubTreeNodeResolvesThroughMainTree;
    procedure TestSubTreeNodeWithoutLinkReturnsNil;
    procedure TestGetListNodeFromSubTree;
    procedure TestGetNodeDataExMainTree;
    procedure TestHasSelectedNodesFalse;
    procedure TestHasSelectedNodesTrue;
  end;

  TTestIterateCallbacks = class(TNodeTreeTestCase)
  published
    procedure TestIncNumberNodeChecked;
    procedure TestIncNumberNodeMixed;
    procedure TestIncNumberNodeUnchecked;
    procedure TestUpdateListItemCount;
    procedure TestActionsOnShutdownFileWithoutShortcut;
    procedure TestActionsOnShutdownCategory;
  end;

  TTestFindNode = class(TNodeTreeTestCase)
  private
    FSearch: TVirtualStringTree;
    function SearchFor(ANode: PVirtualNode; const AKeyword: string;
      AType: TSearchType): PVirtualNode;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindByName;
    procedure TestFindByPathFile;
    procedure TestFindByPathIcon;
    procedure TestFindByWorkingDir;
    procedure TestFindByParameters;
    procedure TestFindSkipsCategoriesAndSeparators;
    procedure TestFindIsCaseInsensitive;
  end;

  TTestFindHotkey = class(TNodeTreeTestCase)
  published
    procedure TestMatchingHotkeyAborts;
    procedure TestNonMatchingHotkeyDoesNotAbort;
  end;

  TTestAddNode = class(TNodeTreeTestCase)
  published
    procedure TestAddNodeByText;
    procedure TestAddNodeByTextEmpty;
    procedure TestAddNodeByPathFile;
    procedure TestAddNodeByPathFileExtractName;
    procedure TestAddNodeByPathFileFolder;
    procedure TestAddNodeByPathFileMissingDoesNotRaise;
  end;

  TTestTreeVisuals = class(TNodeTreeTestCase)
  published
    procedure TestChangeAllNodeHeight;
    procedure TestUpdateItemColorLeavesCustomBackground;
    procedure TestCheckVisibleNodePathExe;
    procedure TestPopulateVSTItemList;
    procedure TestAddNodeInTreeFromMainTreeRoot;
    procedure TestAddNodeInTreeFromMainTreeNestedHidden;
  end;

implementation

{ TTestCreateNodeData }

procedure TTestCreateNodeData.TestCategoryBranch;
var
  Data: TvBaseNodeData;
begin
  Data := TVirtualTreeMethods.CreateNodeData(vtdtCategory);
  try
    AssertTrue('category branch yields a category node',
      Data is TvCategoryNodeData);
  finally
    Data.Free;
  end;
end;

procedure TTestCreateNodeData.TestFileBranch;
var
  Data: TvBaseNodeData;
begin
  Data := TVirtualTreeMethods.CreateNodeData(vtdtFile);
  try
    AssertTrue('file branch yields a file node', Data is TvFileNodeData);
    AssertTrue('file data type', Data.DataType = vtdtFile);
  finally
    Data.Free;
  end;
end;

procedure TTestCreateNodeData.TestFolderBranch;
var
  Data: TvBaseNodeData;
begin
  Data := TVirtualTreeMethods.CreateNodeData(vtdtFolder);
  try
    AssertTrue('folder branch yields a file node', Data is TvFileNodeData);
    AssertEquals('folder nodes get the folder icon', CONST_PATH_FOLDERICON,
      UTF8Encode(TvCustomRealNodeData(Data).PathIcon));
    // Characterization: the constructor always passes vtdtFile up, so even
    // folder nodes report the file data type
    AssertTrue('folder nodes report the file data type',
      Data.DataType = vtdtFile);
  finally
    Data.Free;
  end;
end;

procedure TTestCreateNodeData.TestSeparatorBranch;
var
  Data: TvBaseNodeData;
begin
  Data := TVirtualTreeMethods.CreateNodeData(vtdtSeparator);
  try
    AssertTrue('separator branch yields a separator node',
      Data is TvSeparatorNodeData);
  finally
    Data.Free;
  end;
end;

procedure TTestCreateNodeData.TestUnknownTypeReturnsNil;
begin
  AssertTrue('unknown data type yields nil',
    TVirtualTreeMethods.CreateNodeData(TvTreeDataType(99)) = nil);
end;

{ TTestAddChildNodeEx }

procedure TTestAddChildNodeEx.TestNilParentAddsRoot;
begin
  TVirtualTreeMethods.AddChildNodeEx(Tree, nil, amInsertAfter, vtdtFile);
  TVirtualTreeMethods.AddChildNodeEx(Tree, nil, amInsertAfter, vtdtCategory);
  AssertEquals('two root nodes', 2, Tree.RootNodeCount);
end;

procedure TTestAddChildNodeEx.TestCategoryParentAddsChildAndExpands;
var
  CatNode, ChildNode: PVirtualNode;
  OldDrag: Boolean;
begin
  OldDrag := Config.TVAutoOpCatsDrag;
  Config.TVAutoOpCatsDrag := True;
  try
    AddCategory('Cat');
    CatNode := Tree.GetFirst;
    ChildNode := TVirtualTreeMethods.AddChildNodeEx(Tree, CatNode,
      amInsertAfter, vtdtFile, True);
    AssertTrue('child lands under the category', ChildNode^.Parent = CatNode);
    AssertTrue('category auto-expands on drop', Tree.Expanded[CatNode]);
  finally
    Config.TVAutoOpCatsDrag := OldDrag;
  end;
end;

procedure TTestAddChildNodeEx.TestCategoryParentWithoutAutoExpand;
var
  CatNode: PVirtualNode;
  OldDrag: Boolean;
begin
  OldDrag := Config.TVAutoOpCatsDrag;
  Config.TVAutoOpCatsDrag := False;
  try
    AddCategory('Cat');
    CatNode := Tree.GetFirst;
    TVirtualTreeMethods.AddChildNodeEx(Tree, CatNode, amInsertAfter,
      vtdtFile, True);
    AssertFalse('category stays collapsed', Tree.Expanded[CatNode]);
  finally
    Config.TVAutoOpCatsDrag := OldDrag;
  end;
end;

procedure TTestAddChildNodeEx.TestCategoryParentWithoutDoExpand;
var
  CatNode: PVirtualNode;
  OldDrag: Boolean;
begin
  OldDrag := Config.TVAutoOpCatsDrag;
  Config.TVAutoOpCatsDrag := True;
  try
    AddCategory('Cat');
    CatNode := Tree.GetFirst;
    TVirtualTreeMethods.AddChildNodeEx(Tree, CatNode, amInsertAfter,
      vtdtFile, False);
    AssertFalse('no expansion without the flag', Tree.Expanded[CatNode]);
  finally
    Config.TVAutoOpCatsDrag := OldDrag;
  end;
end;

procedure TTestAddChildNodeEx.TestFileParentInsertsSiblingAfter;
var
  ParentNode, NewNode: PVirtualNode;
begin
  AddFile('Parent', '/bin/parent');
  ParentNode := Tree.GetFirst;
  NewNode := TVirtualTreeMethods.AddChildNodeEx(Tree, ParentNode,
    amInsertAfter, vtdtFile);
  AssertTrue('node inserted after a file parent',
    Tree.GetNextSibling(ParentNode) = NewNode);
end;

procedure TTestAddChildNodeEx.TestFileParentInsertsSiblingBefore;
var
  ParentNode, NewNode: PVirtualNode;
begin
  AddFile('Parent', '/bin/parent');
  ParentNode := Tree.GetFirst;
  NewNode := TVirtualTreeMethods.AddChildNodeEx(Tree, ParentNode,
    amInsertBefore, vtdtFile);
  AssertTrue('node inserted before a file parent',
    Tree.GetPreviousSibling(ParentNode) = NewNode);
end;

procedure TTestAddChildNodeEx.TestPointerNodeAlwaysSet;
var
  NewNode: PVirtualNode;
  Data: TvBaseNodeData;
begin
  NewNode := TVirtualTreeMethods.AddChildNodeEx(Tree, nil, amInsertAfter,
    vtdtSeparator);
  Data := TVirtualTreeMethods.GetNodeItemData(NewNode, Tree);
  AssertNotNull('data attached', Data);
  AssertTrue('data points back at its node', Data.PNode = NewNode);
end;

{ TTestNodeDataResolution }

procedure TTestNodeDataResolution.SetUp;
begin
  inherited SetUp;
  FSubTree := TVirtualStringTree.Create(nil);
  // Search/menu trees hold plain links, no owned objects: no OnFreeNode
  FSubTree.NodeDataSize := SizeOf(rTreeDataX);
end;

procedure TTestNodeDataResolution.TearDown;
begin
  FSubTree.Free;
  inherited TearDown;
end;

procedure TTestNodeDataResolution.TestMainTreeNodeResolves;
var
  Node: PVirtualNode;
  Data: TvBaseNodeData;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  Data := TVirtualTreeMethods.GetNodeItemData(Node, Tree);
  AssertNotNull('main tree node resolves', Data);
  AssertEquals('it is the right node', 'File', UTF8Encode(Data.Name));
end;

procedure TTestNodeDataResolution.TestSubTreeNodeResolvesThroughMainTree;
var
  MainNode, SubNode: PVirtualNode;
  MainData, Resolved: TvBaseNodeData;
begin
  AddFile('File', '/bin/sh');
  MainNode := Tree.GetFirst;
  MainData := TVirtualTreeMethods.GetNodeItemData(MainNode, Tree);

  SubNode := FSubTree.AddChild(nil);
  PTreeDataX(FSubTree.GetNodeData(SubNode))^.pNodeList := MainNode;

  Resolved := TVirtualTreeMethods.GetNodeItemData(SubNode, FSubTree);
  AssertTrue('sub tree node resolves to the main node data',
    Resolved = MainData);
end;

procedure TTestNodeDataResolution.TestSubTreeNodeWithoutLinkReturnsNil;
var
  SubNode: PVirtualNode;
begin
  // Fresh nodes are zeroed: pNodeList starts nil
  SubNode := FSubTree.AddChild(nil);
  AssertTrue('unlinked sub tree node resolves to nil',
    TVirtualTreeMethods.GetNodeItemData(SubNode, FSubTree) = nil);
end;

procedure TTestNodeDataResolution.TestGetListNodeFromSubTree;
var
  MainNode, SubNode: PVirtualNode;
begin
  AddFile('File', '/bin/sh');
  MainNode := Tree.GetFirst;
  SubNode := FSubTree.AddChild(nil);
  PTreeDataX(FSubTree.GetNodeData(SubNode))^.pNodeList := MainNode;

  AssertTrue('link read back',
    TVirtualTreeMethods.GetListNodeFromSubTree(SubNode, FSubTree) = MainNode);

  SubNode := FSubTree.AddChild(nil);
  AssertTrue('missing link reads nil',
    TVirtualTreeMethods.GetListNodeFromSubTree(SubNode, FSubTree) = nil);
end;

procedure TTestNodeDataResolution.TestGetNodeDataExMainTree;
var
  Node: PVirtualNode;
  BaseData: PBaseData;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  BaseData := TVirtualTreeMethods.GetNodeDataEx(Node, Tree);
  AssertTrue('record found', Assigned(BaseData));
  AssertTrue('object attached', Assigned(BaseData^.Data));
  AssertEquals('it is the right node', 'File',
    UTF8Encode(BaseData^.Data.Name));
end;

procedure TTestNodeDataResolution.TestHasSelectedNodesFalse;
begin
  AddFile('File', '/bin/sh');
  AssertFalse('nothing selected', TVirtualTreeMethods.HasSelectedNodes(Tree));
end;

procedure TTestNodeDataResolution.TestHasSelectedNodesTrue;
var
  Node: PVirtualNode;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  Tree.Selected[Node] := True;
  AssertTrue('selection detected', TVirtualTreeMethods.HasSelectedNodes(Tree));
end;

{ TTestIterateCallbacks }

procedure TTestIterateCallbacks.TestIncNumberNodeChecked;
var
  Node: PVirtualNode;
  Count: Integer;
  Abort: Boolean;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  Tree.CheckState[Node] := csCheckedNormal;
  Count := 0;
  Abort := False;
  TVirtualTreeMethods.IncNumberNode(Tree, Node, @Count, Abort);
  AssertEquals('checked node counted', 1, Count);
  AssertFalse('iteration continues', Abort);
end;

procedure TTestIterateCallbacks.TestIncNumberNodeMixed;
var
  Node: PVirtualNode;
  Count: Integer;
  Abort: Boolean;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  Tree.CheckState[Node] := csMixedNormal;
  Count := 0;
  Abort := False;
  TVirtualTreeMethods.IncNumberNode(Tree, Node, @Count, Abort);
  AssertEquals('mixed node counted', 1, Count);
end;

procedure TTestIterateCallbacks.TestIncNumberNodeUnchecked;
var
  Node: PVirtualNode;
  Count: Integer;
  Abort: Boolean;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  Count := 0;
  Abort := False;
  TVirtualTreeMethods.IncNumberNode(Tree, Node, @Count, Abort);
  AssertEquals('unchecked node skipped', 0, Count);
end;

procedure TTestIterateCallbacks.TestUpdateListItemCount;
var
  Stats: rListStats;
  Abort: Boolean;
  Node: PVirtualNode;
begin
  AddCategory('Cat');
  AddFile('File A', '/bin/a', Tree.GetFirst);
  AddFile('File B', '/bin/b', Tree.GetFirst);
  AddSeparator('---');
  Stats.SwCount := 0;
  Stats.CatCount := 0;
  Abort := False;

  Node := Tree.GetFirst;
  while Assigned(Node) do
  begin
    TVirtualTreeMethods.UpdateListItemCount(Tree, Node, @Stats, Abort);
    Node := Tree.GetNext(Node);
  end;

  AssertEquals('one category', 1, Stats.CatCount);
  AssertEquals('files counted, separator skipped', 2, Stats.SwCount);
end;

procedure TTestIterateCallbacks.TestActionsOnShutdownFileWithoutShortcut;
var
  Node: PVirtualNode;
  Abort: Boolean;
begin
  AddFile('File', '/bin/sh');
  Node := Tree.GetFirst;
  Abort := False;
  // ShortcutDesktop is off: nothing happens and nothing raises
  TVirtualTreeMethods.ActionsOnShutdown(Tree, Node, nil, Abort);
  AssertFalse('iteration continues', Abort);
end;

procedure TTestIterateCallbacks.TestActionsOnShutdownCategory;
var
  Node: PVirtualNode;
  Abort: Boolean;
begin
  AddCategory('Cat');
  Node := Tree.GetFirst;
  Abort := False;
  TVirtualTreeMethods.ActionsOnShutdown(Tree, Node, nil, Abort);
  AssertFalse('iteration continues', Abort);
end;

{ TTestFindNode }

procedure TTestFindNode.SetUp;
begin
  inherited SetUp;
  FSearch := TVirtualStringTree.Create(nil);
  FSearch.NodeDataSize := SizeOf(rTreeDataX);
end;

procedure TTestFindNode.TearDown;
begin
  FSearch.Free;
  inherited TearDown;
end;

function TTestFindNode.SearchFor(ANode: PVirtualNode; const AKeyword: string;
  AType: TSearchType): PVirtualNode;
var
  Search: TLauncherSearch;
  Abort: Boolean;
begin
  // The application lowercases the keyword on input; the callback only
  // lowercases the node side
  Search.Tree := FSearch;
  Search.Keyword := UTF8Decode(AKeyword);
  Search.SearchType := AType;
  Abort := False;
  TVirtualTreeMethods.FindNode(Tree, ANode, @Search, Abort);
  Result := FSearch.GetFirst;
end;

procedure TTestFindNode.TestFindByName;
var
  Node, Found: PVirtualNode;
begin
  AddFile('MyApp Launcher', '/bin/other');
  Node := Tree.GetFirst;
  Found := SearchFor(Node, 'launcher', stName);
  AssertNotNull('name match found', Found);
  AssertTrue('search node links the main node',
    PTreeDataX(FSearch.GetNodeData(Found))^.pNodeList = Node);
end;

procedure TTestFindNode.TestFindByPathFile;
var
  Node, Found: PVirtualNode;
begin
  AddFile('App', '/opt/myapp-portable/run.sh');
  Node := Tree.GetFirst;
  Found := SearchFor(Node, 'portable', stPathFile);
  AssertNotNull('path match found', Found);
end;

procedure TTestFindNode.TestFindByPathIcon;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Found: PVirtualNode;
begin
  Data := AddFile('App', '/bin/app');
  Data.PathIcon := 'myapp-icon.ico';
  Node := Tree.GetFirst;
  Found := SearchFor(Node, 'icon', stPathIcon);
  AssertNotNull('icon path match found', Found);
end;

procedure TTestFindNode.TestFindByWorkingDir;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Found: PVirtualNode;
begin
  Data := AddFile('App', '/bin/app');
  Data.WorkingDir := '/tmp/myapp-work';
  Node := Tree.GetFirst;
  Found := SearchFor(Node, 'work', stWorkingDir);
  AssertNotNull('working dir match found', Found);
end;

procedure TTestFindNode.TestFindByParameters;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Found: PVirtualNode;
begin
  Data := AddFile('App', '/bin/app');
  Data.Parameters := '--myapp-flag';
  Node := Tree.GetFirst;
  Found := SearchFor(Node, 'flag', stParameters);
  AssertNotNull('parameters match found', Found);
end;

procedure TTestFindNode.TestFindSkipsCategoriesAndSeparators;
var
  CatNode, SepNode: PVirtualNode;
begin
  AddCategory('CatOnly');
  CatNode := Tree.GetFirst;
  AddSeparator('SepOnly');
  SepNode := Tree.GetNextSibling(CatNode);

  AssertTrue('category skipped even on name match',
    SearchFor(CatNode, 'catonly', stName) = nil);
  AssertTrue('separator skipped', SearchFor(SepNode, 'seponly', stName) = nil);
  AssertTrue('search tree stays empty', FSearch.GetFirst = nil);
end;

procedure TTestFindNode.TestFindIsCaseInsensitive;
var
  Node, Found: PVirtualNode;
begin
  AddFile('MiXeD Case', '/bin/app');
  Node := Tree.GetFirst;
  Found := SearchFor(Node, 'mixed case', stName);
  AssertNotNull('mixed case name found with lowercase keyword', Found);
end;

{ TTestFindHotkey }

procedure TTestFindHotkey.TestMatchingHotkeyAborts;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Abort: Boolean;
begin
  Data := AddFile('File', '/bin/sh');
  Data.Hotkey := TextToShortCut('Ctrl+H');
  Node := Tree.GetFirst;
  Abort := False;
  // The callback overlays the Data pointer itself (Cardinal absolute Data),
  // so callers pass the hotkey value as the pointer
  TVirtualTreeMethods.FindHotkey(Tree, Node, Pointer(PtrUInt(Data.Hotkey)), Abort);
  AssertTrue('matching hotkey stops the iteration', Abort);
end;

procedure TTestFindHotkey.TestNonMatchingHotkeyDoesNotAbort;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Abort: Boolean;
begin
  Data := AddFile('File', '/bin/sh');
  Data.Hotkey := TextToShortCut('Ctrl+H');
  Node := Tree.GetFirst;
  Abort := False;
  TVirtualTreeMethods.FindHotkey(Tree, Node,
    Pointer(PtrUInt(Data.Hotkey) + 1), Abort);
  AssertFalse('other hotkey keeps iterating', Abort);
end;

{ TTestAddNode }

procedure TTestAddNode.TestAddNodeByText;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
begin
  AssertTrue('text node added',
    TVirtualTreeMethods.AddNodeByText(Tree, nil, 'https://example.com/x',
      amInsertAfter));
  Node := Tree.GetFirst;
  AssertNotNull('node exists', Node);
  Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
  AssertEquals('link name', 'Link', UTF8Encode(Data.Name));
  AssertEquals('link target', 'https://example.com/x',
    UTF8Encode(Data.PathFile));
  AssertEquals('link icon', CONST_PATH_URLICON, UTF8Encode(Data.PathIcon));
end;

procedure TTestAddNode.TestAddNodeByTextEmpty;
begin
  AssertFalse('empty text adds nothing',
    TVirtualTreeMethods.AddNodeByText(Tree, nil, '', amInsertAfter));
  AssertEquals('tree stays empty', 0, Tree.RootNodeCount);
end;

procedure TTestAddNode.TestAddNodeByPathFile;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Tmp: string;
begin
  Tmp := WriteTempFile('asuitebynode.tmp');
  try
    TVirtualTreeMethods.AddNodeByPathFile(Tree, nil, Tmp, amInsertAfter, False);
    Node := Tree.GetFirst;
    AssertNotNull('node exists', Node);
    Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
    AssertEquals('name without extension', 'asuitebynode',
      UTF8Encode(Data.Name));
    AssertEquals('path stored relative-aware',
      UTF8Encode(ASuiteInstance.Paths.AbsoluteToRelative(UTF8Decode(Tmp))),
      UTF8Encode(Data.PathFile));
  finally
    DeleteFile(Tmp);
  end;
end;

procedure TTestAddNode.TestAddNodeByPathFileExtractName;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Tmp: string;
begin
  Tmp := WriteTempFile('asuitebyname.tmp');
  try
    TVirtualTreeMethods.AddNodeByPathFile(Tree, nil, Tmp, amInsertAfter, True);
    Node := Tree.GetFirst;
    Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
    // A plain temp file carries no version info: the file name survives
    AssertEquals('name extracted from file', 'asuitebyname.tmp',
      UTF8Encode(Data.Name));
  finally
    DeleteFile(Tmp);
  end;
end;

procedure TTestAddNode.TestAddNodeByPathFileFolder;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Dir: string;
begin
  Dir := GetTempDir(False);
  TVirtualTreeMethods.AddNodeByPathFile(Tree, nil, Dir, amInsertAfter, False);
  Node := Tree.GetFirst;
  AssertNotNull('node exists', Node);
  Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
  AssertEquals('folders get the folder icon', CONST_PATH_FOLDERICON,
    UTF8Encode(Data.PathIcon));
end;

procedure TTestAddNode.TestAddNodeByPathFileMissingDoesNotRaise;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
begin
  TVirtualTreeMethods.AddNodeByPathFile(Tree, nil, '/definitely/missing/xyz.tmp',
    amInsertAfter, False);
  Node := Tree.GetFirst;
  AssertNotNull('node created anyway', Node);
  Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
  AssertFalse('missing path flagged', Data.IsPathFileExists);
end;

{ TTestTreeVisuals }

procedure TTestTreeVisuals.TestChangeAllNodeHeight;
var
  FileNode, SepNode: PVirtualNode;
begin
  AddFile('File', '/bin/sh');
  FileNode := Tree.GetFirst;
  AddSeparator('---');
  SepNode := Tree.GetNextSibling(FileNode);

  TVirtualTreeMethods.ChangeAllNodeHeight(Tree, 42);

  AssertEquals('regular node resized', 42, Tree.NodeHeight[FileNode]);
  AssertEquals('separator keeps the small height',
    ASuiteInstance.SmallHeightNode, Tree.NodeHeight[SepNode]);
end;

procedure TTestTreeVisuals.TestUpdateItemColorLeavesCustomBackground;
var
  OldBackground: Boolean;
begin
  OldBackground := Config.TVBackground;
  Config.TVBackground := True;
  try
    Tree.Font.Color := clRed;
    TVirtualTreeMethods.UpdateItemColor(Tree);
    AssertTrue('custom background keeps its font color',
      Tree.Font.Color = clRed);
  finally
    Config.TVBackground := OldBackground;
  end;
end;

procedure TTestTreeVisuals.TestCheckVisibleNodePathExe;
var
  Data: TvFileNodeData;
  Tmp: string;
begin
  Tmp := WriteTempFile('asuite_test_visible.tmp');
  try
    Data := AddFile('File', '/bin/sh');
    Data.PathFile := UTF8Decode(Tmp);
    AssertTrue('path exists before the check', Data.IsPathFileExists);

    TVirtualTreeMethods.CheckVisibleNodePathExe(Tree);

    AssertTrue('existing path still flagged', Data.IsPathFileExists);
  finally
    DeleteFile(Tmp);
  end;

  Data := AddFile('Missing', '/definitely/missing/xyz.tmp');
  TVirtualTreeMethods.CheckVisibleNodePathExe(Tree);
  AssertFalse('missing path flagged', Data.IsPathFileExists);
end;

procedure TTestTreeVisuals.TestPopulateVSTItemList;
var
  ListTree: TVirtualStringTree;
  Items: TBaseItemsList;
  First, Second: PVirtualNode;
begin
  AddFile('File A', '/bin/a');
  AddFile('File B', '/bin/b');

  Items := TBaseItemsList.Create;
  try
    Items.AddItem(TvCustomRealNodeData(FindNodeDataByName('File A')));
    Items.AddItem(TvCustomRealNodeData(FindNodeDataByName('File B')));

    ListTree := TVirtualStringTree.Create(nil);
    try
      ListTree.NodeDataSize := SizeOf(rTreeDataX);
      TVirtualTreeMethods.PopulateVSTItemList(ListTree, Items);

      AssertEquals('one node per item', 2, ListTree.RootNodeCount);
      First := ListTree.GetFirst;
      Second := ListTree.GetNextSibling(First);
      AssertTrue('first node links the first item',
        PTreeDataX(ListTree.GetNodeData(First))^.pNodeList =
          TvCustomRealNodeData(FindNodeDataByName('File A')).PNode);
      AssertTrue('second node links the second item',
        PTreeDataX(ListTree.GetNodeData(Second))^.pNodeList =
          TvCustomRealNodeData(FindNodeDataByName('File B')).PNode);
    finally
      ListTree.Free;
    end;
  finally
    Items.Free;
  end;
end;

procedure TTestTreeVisuals.TestAddNodeInTreeFromMainTreeRoot;
var
  Sec: TVirtualStringTree;
  CatNode, NewNode: PVirtualNode;
  MainData: PBaseData;
  Abort: Boolean;
begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;

  Sec := TVirtualStringTree.Create(nil);
  try
    Sec.NodeDataSize := SizeOf(rTreeDataX);
    Abort := False;
    TVirtualTreeMethods.AddNodeInTreeFromMainTree(Tree, CatNode, @Sec, Abort);

    NewNode := Sec.GetFirst;
    AssertTrue('node mirrored', Assigned(NewNode));
    AssertTrue('mirror links the main node',
      PTreeDataX(Sec.GetNodeData(NewNode))^.pNodeList = CatNode);
    MainData := Tree.GetNodeData(CatNode);
    AssertTrue('main node remembers its mirror',
      MainData^.MenuNode = NewNode);
    AssertFalse('iteration continues', Abort);
  finally
    Sec.Free;
  end;
end;

procedure TTestTreeVisuals.TestAddNodeInTreeFromMainTreeNestedHidden;
var
  Sec: TVirtualStringTree;
  CatNode, FileNode, NewNode: PVirtualNode;
  FileData: TvFileNodeData;
  Abort: Boolean;
begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;
  FileData := AddFile('File', '/bin/sh', CatNode);
  FileNode := Tree.GetFirstChild(CatNode);
  FileData.HideFromMenu := True;

  Sec := TVirtualStringTree.Create(nil);
  try
    Sec.NodeDataSize := SizeOf(rTreeDataX);
    Abort := False;
    // The parent mirror must exist first, like IterateSubtree provides it
    TVirtualTreeMethods.AddNodeInTreeFromMainTree(Tree, CatNode, @Sec, Abort);
    TVirtualTreeMethods.AddNodeInTreeFromMainTree(Tree, FileNode, @Sec, Abort);

    NewNode := Sec.GetFirstChild(Sec.GetFirst);
    AssertTrue('child mirror lands under the parent mirror', Assigned(NewNode));
    AssertTrue('mirror links the main node',
      PTreeDataX(Sec.GetNodeData(NewNode))^.pNodeList = FileNode);
    AssertFalse('hidden nodes stay invisible', Sec.IsVisible[NewNode]);
  finally
    Sec.Free;
  end;
end;

initialization
  RegisterTest(TTestCreateNodeData);
  RegisterTest(TTestAddChildNodeEx);
  RegisterTest(TTestNodeDataResolution);
  RegisterTest(TTestIterateCallbacks);
  RegisterTest(TTestFindNode);
  RegisterTest(TTestFindHotkey);
  RegisterTest(TTestAddNode);
  RegisterTest(TTestTreeVisuals);

end.
