unit Tests.DatabaseList;

{$mode objfpc}{$H+}

{ Round-trip and incremental-save tests for TSQLtbl_list through the singleton
  TDBManager and the MainTree registered by the shared fixture.

  These exercise the real persistence contract: what SaveData writes must be
  exactly what LoadData reads back, an already-stored node must only be
  rewritten when Changed, Position or ParentID actually diverge, and the
  import-mode flag must mark every loaded node as a tri-state checkbox. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types, Kernel.Enumerations, Kernel.Manager,
  Database.Manager, Database.Version, Database.List,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Category,
  NodeDataTypes.Files,
  VirtualTree.Methods,
  Tests.Helpers.Database;

type
  TTestDatabaseList = class(TDatabaseTestCase)
  published
    procedure TestSaveReloadKeepsEveryField;
    procedure TestLoadOrdersSiblingsByPosition;
    procedure TestSaveUpdatesOnlyChangedOrMovedNodes;
    procedure TestSaveUpdatesReorderedSiblings;
    procedure TestSaveUpdatesNodeMovedToAnotherCategory;
    procedure TestImportModeMarksNodesAsTriState;
    procedure TestUnicodeNamesAndPathsSurviveRoundTrip;
  end;

implementation

// Snapshot of every stored row as 'id=lastModified', to prove which rows a
// second SaveData actually rewrote.
function SnapshotRows: TStringList;
var
  Rec: TSQLtbl_list;
begin
  Result := TStringList.Create;
  Rec := TSQLtbl_list.CreateAndFillPrepare(ASuiteManager.DBManager.Database.Orm,
    'ORDER BY ID');
  try
    while Rec.FillOne do
      Result.Add(IntToStr(Rec.ID) + '=' + IntToStr(Rec.lastModified));
  finally
    Rec.Free;
  end;
end;

procedure TTestDatabaseList.TestSaveReloadKeepsEveryField;
var
  CatNode: PVirtualNode;
  Cat, SubCat: TvCategoryNodeData;
  FileNode: TvFileNodeData;
  Expected: string;
begin
  Cat := AddCategory('Cat');
  Cat.Description := UTF8Decode('Main category');
  Cat.HideFromMenu := True;
  Cat.PathIcon := UTF8Decode('%FolderIcons%\folder.ico');
  CatNode := Tree.GetFirst;
  AddSeparator('---');
  SubCat := AddCategory('Sub', CatNode);
  SubCat.Description := UTF8Decode('Nested');
  FileNode := AddFile('Deep file', '/usr/bin/deep');
  FileNode.WorkingDir := UTF8Decode('/tmp');
  FileNode.Parameters := UTF8Decode('--verbose');
  FileNode.EnvironmentVars.Text := 'A=1' + LineEnding + 'B=2';
  FileNode.ShortcutDesktop := True;
  FileNode.NoMRU := True;
  AddFile('Loose', '/bin/loose');

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  AssertEquals('one list row per tree node', 5, CountRows(TSQLtbl_list));
  AssertEquals('version row written by SaveData', 1, CountRows(TSQLtbl_version));
  // Save stamps Now into every in-memory edit date (see RestoreEditDates);
  // re-stamp the build-time value so the snapshot matches the reloaded tree
  RestoreEditDates;
  Expected := DescribeTree(Tree);

  ReloadTree;

  AssertEquals('round trip preserves every field', Expected, DescribeTree(Tree));
end;

procedure TTestDatabaseList.TestLoadOrdersSiblingsByPosition;
var
  Names: string;
  Node: PVirtualNode;
  Data: TvBaseNodeData;

  procedure AddRawRow(const ATitle: string; APosition: Integer);
  var
    Rec: TSQLtbl_list;
  begin
    Rec := TSQLtbl_list.Create;
    try
      Rec.itemtype := Ord(vtdtFile);
      Rec.parent := 0;
      Rec.position := APosition;
      Rec.title := ATitle;
      DB.Database.Add(Rec, True);
    finally
      Rec.Free;
    end;
  end;

begin
  // Insert out of order on purpose: LoadItemsByParentID runs
  // 'parent=? ORDER BY parent, position', so the tree must come back sorted
  AddRawRow('Third', 2);
  AddRawRow('First', 0);
  AddRawRow('Second', 1);

  ReloadTree;

  Names := '';
  Node := Tree.GetFirst;
  while Assigned(Node) do
  begin
    Data := TVirtualTreeMethods.GetNodeItemData(Node, Tree);
    if Names <> '' then
      Names := Names + ',';
    Names := Names + UTF8Encode(Data.Name);
    Node := Tree.GetNextSibling(Node);
  end;
  AssertEquals('siblings loaded in position order', 'First,Second,Third', Names);
end;

procedure TTestDatabaseList.TestSaveUpdatesOnlyChangedOrMovedNodes;
var
  Before, After: TStringList;
  I: Integer;
  RowName: string;
  StoredID, StoredModified: Integer;
begin
  AddCategory('Cat');
  AddFile('File A', '/bin/a');
  AddFile('File B', '/bin/b');
  AddFile('File C', '/bin/c');

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  AssertEquals('four rows saved (category + 3 files)', 4, CountRows(TSQLtbl_list));

  Before := SnapshotRows;
  try
    // Mark only File B as changed. Changed := True stamps Now into the edit
    // date, so fix the stamp explicitly to keep the assertion deterministic.
    StoredID := FindNodeDataByName('File B').ID;
    StoredModified := StrToInt(Before.Values[IntToStr(StoredID)]);
    with FindNodeDataByName('File B') do
    begin
      Name := UTF8Decode('File B renamed');
      Changed := True;
      UnixEditDate := StoredModified + 1000;
    end;

    AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));

    AssertEquals('no row inserted or deleted', 4, CountRows(TSQLtbl_list));

    After := SnapshotRows;
    try
      AssertEquals('same number of rows after second save', Before.Count, After.Count);
      for I := 0 to Before.Count - 1 do
      begin
        RowName := Before.Names[I];
        if RowName = IntToStr(StoredID) then
          AssertEquals('changed row got the new edit date',
            IntToStr(StoredModified + 1000), After.Values[RowName])
        else
          AssertEquals('row ' + RowName + ' untouched',
            Before.Values[RowName], After.Values[RowName]);
      end;
    finally
      After.Free;
    end;
  finally
    Before.Free;
  end;

  ReloadTree;
  AssertEquals('renamed node survives reload',
    'File B renamed', UTF8Encode(FindNodeDataByName('File B renamed').Name));
end;

procedure TTestDatabaseList.TestSaveUpdatesReorderedSiblings;
var
  CatNode: PVirtualNode;

  function SiblingNames(AParent: PVirtualNode): string;
  var
    Child: PVirtualNode;
    ChildData: TvBaseNodeData;
  begin
    Result := '';
    Child := Tree.GetFirstChild(AParent);
    while Assigned(Child) do
    begin
      ChildData := TVirtualTreeMethods.GetNodeItemData(Child, Tree);
      if Result <> '' then
        Result := Result + ',';
      Result := Result + UTF8Encode(ChildData.Name);
      Child := Tree.GetNextSibling(Child);
    end;
  end;

begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;
  AddFile('A', '/bin/a', CatNode);
  AddFile('B', '/bin/b', CatNode);
  AddFile('C', '/bin/c', CatNode);

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));

  // Reorder within the same parent: Node.Index changes but Data.Position
  // does not, so Save must take the Update branch for the moved nodes
  Tree.MoveTo(FindNodeByName('C'), FindNodeByName('A'), amInsertBefore, False);

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  ReloadTree;

  CatNode := FindNodeByName('Cat');
  AssertEquals('reordered siblings reload in the new order',
    'C,A,B', SiblingNames(CatNode));
end;

procedure TTestDatabaseList.TestSaveUpdatesNodeMovedToAnotherCategory;
var
  CatNode1, CatNode2: PVirtualNode;
  FileNode: TvFileNodeData;
begin
  AddCategory('Cat1');
  CatNode1 := Tree.GetFirst;
  AddFile('File', '/bin/file', CatNode1);
  AddCategory('Cat2');
  CatNode2 := Tree.GetNextSibling(CatNode1);

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));

  // Move from Cat1 to Cat2: Data.ParentID stays stale until Save runs
  Tree.MoveTo(FindNodeByName('File'), CatNode2, amAddChildLast, False);

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  ReloadTree;

  CatNode2 := FindNodeByName('Cat2');
  FileNode := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(
    Tree.GetFirstChild(CatNode2), Tree));
  AssertEquals('moved node reloads under the new parent',
    'File', UTF8Encode(FileNode.Name));
  AssertEquals('moved node has the new ParentID',
    FindNodeDataByName('Cat2').ID, FileNode.ParentID);
end;

procedure TTestDatabaseList.TestImportModeMarksNodesAsTriState;

  procedure Walk(AParent: PVirtualNode; const AExpectTriState: Boolean);
  var
    Child: PVirtualNode;
    ChildData: TvBaseNodeData;
  begin
    if AParent = nil then
      Child := Tree.GetFirst
    else
      Child := Tree.GetFirstChild(AParent);
    while Assigned(Child) do
    begin
      ChildData := TVirtualTreeMethods.GetNodeItemData(Child, Tree);
      if AExpectTriState then
        AssertEquals('imported node uses tri-state check type',
          Ord(ctTriStateCheckBox), Ord(Tree.CheckType[Child]))
      else
        AssertFalse('normally loaded node is not tri-state',
          Tree.CheckType[Child] = ctTriStateCheckBox);
      if Assigned(ChildData) and ChildData.IsCategoryItem then
        Walk(Child, AExpectTriState);
      Child := Tree.GetNextSibling(Child);
    end;
  end;

begin
  AddCategory('Cat');
  AddFile('File A', '/bin/a', Tree.GetFirst);
  AddFile('Loose', '/bin/loose');

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));

  // Import mode: every loaded node becomes a tri-state checkbox
  ClearTree;
  DB.ImportData(Tree);
  Walk(nil, True);

  // A normal load must not use tri-state
  ClearTree;
  DB.LoadData(Tree);
  Walk(nil, False);
end;

procedure TTestDatabaseList.TestUnicodeNamesAndPathsSurviveRoundTrip;
var
  Cat: TvCategoryNodeData;
  FileNode: TvFileNodeData;
begin
  Cat := AddCategory('Catégories');
  Cat.Description := UTF8Decode('Descrizione àèìòù');
  FileNode := AddFile('Café', '/opt/caffè/eseguibile');
  FileNode.WorkingDir := UTF8Decode('/tmp/données');
  FileNode.Parameters := UTF8Decode('--titolo=Présentation');

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  ReloadTree;

  FileNode := TvFileNodeData(FindNodeDataByName('Café'));
  AssertNotNull('file found after reload', FileNode);
  AssertEquals('unicode name survives', 'Café', UTF8Encode(FileNode.Name));
  AssertEquals('unicode path survives',
    '/opt/caffè/eseguibile', UTF8Encode(FileNode.PathFile));
  AssertEquals('unicode working dir survives',
    '/tmp/données', UTF8Encode(FileNode.WorkingDir));
  AssertEquals('unicode parameters survive',
    '--titolo=Présentation', UTF8Encode(FileNode.Parameters));
  Cat := TvCategoryNodeData(FindNodeDataByName('Catégories'));
  AssertEquals('unicode description survives',
    'Descrizione àèìòù', UTF8Encode(Cat.Description));
end;

initialization
  RegisterTest(TTestDatabaseList);

end.
