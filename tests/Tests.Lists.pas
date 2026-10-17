unit Tests.Lists;

{$mode objfpc}{$H+}

{ Behaviour tests for the item lists (Lists.Base, Lists.Special,
  Lists.Manager, Lists.HotKey).

  TBaseItemsList is pure object bookkeeping. The special and manager lists
  register nodes in response to node setters, so those tests run on the
  shared tree fixture, which drains every singleton list in teardown.

  THotkeyItemsList.Clear is deliberately not covered: unlike RemoveItem it
  unregisters every shortcut with the desktop unconditionally, even when
  Config.HotKey is off. }

interface

uses
  Classes, SysUtils, LCLProc, fpcunit, testregistry,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  NodeDataTypes.Category,
  Lists.Base, Lists.Special, Lists.Manager, Lists.HotKey,
  Kernel.Enumerations, Kernel.Manager,
  AppConfig.Main,
  Tests.Helpers.Tree;

type
  TTestBaseItemsList = class(TTestCase)
  private
    FList: TBaseItemsList;
    FFirst, FSecond, FThird: TvCategoryNodeData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddItemAppends;
    procedure TestAddItemRemovesDuplicatesFirst;
    procedure TestInsertItemClampsToCount;
    procedure TestInsertItemAtZero;
    procedure TestRemoveItemPresent;
    procedure TestRemoveItemAbsent;
    procedure TestDelete;
    procedure TestClear;
  end;

  TTestSpecialItemsList = class(TNodeTreeTestCase)
  published
    procedure TestMRUOrdersByLastAccessDesc;
    procedure TestMFUOrdersByClickCountDesc;
    procedure TestAddItemRejectsNonFile;
    procedure TestSortRestoresMRUOrder;
    procedure TestMRUComparer;
    procedure TestMFUComparer;
    procedure TestComparersIgnoreNonFile;
  end;

  TTestListManager = class(TNodeTreeTestCase)
  published
    procedure TestRemoveItemFromLists;
  end;

  TTestHotkeyItemsList = class(TNodeTreeTestCase)
  published
    procedure TestAddItemTransientIDReturnsMinusOne;
    procedure TestAddItemRegistersWithoutDesktop;
    procedure TestIndexOfID;
  end;

implementation

{ TTestBaseItemsList }

procedure TTestBaseItemsList.SetUp;
begin
  inherited SetUp;
  FList := TBaseItemsList.Create;
  FFirst := TvCategoryNodeData.Create;
  FSecond := TvCategoryNodeData.Create;
  FThird := TvCategoryNodeData.Create;
end;

procedure TTestBaseItemsList.TearDown;
begin
  // The list only holds references: nodes are freed here
  FList.Free;
  FFirst.Free;
  FSecond.Free;
  FThird.Free;
  inherited TearDown;
end;

procedure TTestBaseItemsList.TestAddItemAppends;
begin
  AssertEquals('first index', 0, FList.AddItem(FFirst));
  AssertEquals('second index', 1, FList.AddItem(FSecond));
  AssertEquals('two items', 2, FList.Count);
  AssertTrue('first stored', FList[0] = FFirst);
  AssertTrue('second stored', FList[1] = FSecond);
end;

procedure TTestBaseItemsList.TestAddItemRemovesDuplicatesFirst;
begin
  FList.AddItem(FFirst);
  FList.AddItem(FSecond);
  FList.AddItem(FFirst);
  AssertEquals('no duplicates', 2, FList.Count);
  AssertTrue('re-added item moves to the end', FList[1] = FFirst);
end;

procedure TTestBaseItemsList.TestInsertItemClampsToCount;
begin
  FList.AddItem(FFirst);
  AssertEquals('index clamped to the end', 1, FList.InsertItem(99, FSecond));
  AssertEquals('two items', 2, FList.Count);
end;

procedure TTestBaseItemsList.TestInsertItemAtZero;
begin
  FList.AddItem(FFirst);
  AssertEquals('index zero', 0, FList.InsertItem(0, FSecond));
  AssertTrue('inserted first', FList[0] = FSecond);
end;

procedure TTestBaseItemsList.TestRemoveItemPresent;
begin
  FList.AddItem(FFirst);
  FList.AddItem(FSecond);
  AssertEquals('removal returns the index', 0, FList.RemoveItem(FFirst));
  AssertEquals('one item left', 1, FList.Count);
end;

procedure TTestBaseItemsList.TestRemoveItemAbsent;
begin
  FList.AddItem(FFirst);
  AssertEquals('missing removal returns -1', -1, FList.RemoveItem(FSecond));
  AssertEquals('list untouched', 1, FList.Count);
end;

procedure TTestBaseItemsList.TestDelete;
begin
  FList.AddItem(FFirst);
  FList.AddItem(FSecond);
  FList.Delete(0);
  AssertEquals('one item left', 1, FList.Count);
  AssertTrue('second survived', FList[0] = FSecond);
end;

procedure TTestBaseItemsList.TestClear;
begin
  FList.AddItem(FFirst);
  FList.AddItem(FSecond);
  FList.Clear;
  AssertEquals('list emptied', 0, FList.Count);
end;

{ TTestSpecialItemsList }

procedure TTestSpecialItemsList.TestMRUOrdersByLastAccessDesc;
var
  List: TSpecialItemsList;
  Older, Newer: TvFileNodeData;
begin
  List := TSpecialItemsList.Create(lmMRU);
  try
    Older := TvFileNodeData.Create(vtdtFile);
    Newer := TvFileNodeData.Create(vtdtFile);
    try
      Older.LastAccess := 100;
      Newer.LastAccess := 200;
      List.AddItem(Older);
      List.AddItem(Newer);
      AssertTrue('newest first', List[0] = Newer);
      AssertTrue('oldest last', List[1] = Older);
    finally
      Older.Free;
      Newer.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestSpecialItemsList.TestMFUOrdersByClickCountDesc;
var
  List: TSpecialItemsList;
  Cold, Hot: TvFileNodeData;
begin
  List := TSpecialItemsList.Create(lmMFU);
  try
    Cold := TvFileNodeData.Create(vtdtFile);
    Hot := TvFileNodeData.Create(vtdtFile);
    try
      Cold.ClickCount := 1;
      Hot.ClickCount := 9;
      List.AddItem(Cold);
      List.AddItem(Hot);
      AssertTrue('hottest first', List[0] = Hot);
      AssertTrue('coldest last', List[1] = Cold);
    finally
      Cold.Free;
      Hot.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestSpecialItemsList.TestAddItemRejectsNonFile;
var
  List: TSpecialItemsList;
  Cat: TvCategoryNodeData;
begin
  List := TSpecialItemsList.Create(lmMRU);
  Cat := TvCategoryNodeData.Create;
  try
    AssertEquals('non-file refused', -1, List.AddItem(Cat));
    AssertEquals('list stays empty', 0, List.Count);
  finally
    Cat.Free;
    List.Free;
  end;
end;

procedure TTestSpecialItemsList.TestSortRestoresMRUOrder;
var
  List: TSpecialItemsList;
  Older, Newer: TvFileNodeData;
begin
  List := TSpecialItemsList.Create(lmMRU);
  try
    Older := TvFileNodeData.Create(vtdtFile);
    Newer := TvFileNodeData.Create(vtdtFile);
    try
      Older.LastAccess := 100;
      Newer.LastAccess := 200;
      List.AddItem(Older);
      List.AddItem(Newer);
      // Bypass the list: the order goes stale until Sort runs
      Older.LastAccess := 300;
      List.Sort;
      AssertTrue('newest first again', List[0] = Older);
    finally
      Older.Free;
      Newer.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TTestSpecialItemsList.TestMRUComparer;
var
  Comparer: TMRUItemsComparer;
  Older, Newer: TvFileNodeData;
begin
  Comparer := TMRUItemsComparer.Create;
  Older := TvFileNodeData.Create(vtdtFile);
  Newer := TvFileNodeData.Create(vtdtFile);
  try
    Older.LastAccess := 100;
    Newer.LastAccess := 200;
    AssertTrue('older sorts after newer', Comparer.Compare(Older, Newer) > 0);
    AssertTrue('newer sorts before older', Comparer.Compare(Newer, Older) < 0);
    AssertEquals('same access compares equal', 0,
      Comparer.Compare(Older, Older));
  finally
    Older.Free;
    Newer.Free;
    Comparer.Free;
  end;
end;

procedure TTestSpecialItemsList.TestMFUComparer;
var
  Comparer: TMFUItemsComparer;
  Cold, Hot: TvFileNodeData;
begin
  Comparer := TMFUItemsComparer.Create;
  Cold := TvFileNodeData.Create(vtdtFile);
  Hot := TvFileNodeData.Create(vtdtFile);
  try
    Cold.ClickCount := 1;
    Hot.ClickCount := 9;
    AssertTrue('colder sorts after hotter', Comparer.Compare(Cold, Hot) > 0);
    AssertTrue('hotter sorts before colder', Comparer.Compare(Hot, Cold) < 0);
  finally
    Cold.Free;
    Hot.Free;
    Comparer.Free;
  end;
end;

procedure TTestSpecialItemsList.TestComparersIgnoreNonFile;
var
  MRUComparer: TMRUItemsComparer;
  MFUComparer: TMFUItemsComparer;
  Cat, FileNode: TvBaseNodeData;
begin
  MRUComparer := TMRUItemsComparer.Create;
  MFUComparer := TMFUItemsComparer.Create;
  Cat := TvCategoryNodeData.Create;
  FileNode := TvFileNodeData.Create(vtdtFile);
  try
    AssertEquals('mru ignores categories', 0, MRUComparer.Compare(Cat, FileNode));
    AssertEquals('mfu ignores categories', 0, MFUComparer.Compare(Cat, FileNode));
  finally
    Cat.Free;
    FileNode.Free;
    MRUComparer.Free;
    MFUComparer.Free;
  end;
end;

{ TTestListManager }

procedure TTestListManager.TestRemoveItemFromLists;
var
  Node: TvFileNodeData;
  OldHotKey: Boolean;
  Manager: TListManager;
begin
  OldHotKey := Config.HotKey;
  Config.HotKey := False; // never register with the desktop portal
  try
    Manager := ASuiteManager.ListManager;
    Node := AddFile('File', '/bin/sh');
    Node.ID := 42;
    Node.LastAccess := 100;
    Node.ClickCount := 3;
    Node.SchMode := smOnce;
    Node.Autorun := atAlwaysOnStart;
    Node.Hotkey := TextToShortCut('Ctrl+M');

    AssertEquals('mru holds it', 1, Manager.MRUList.Count);
    AssertEquals('mfu holds it', 1, Manager.MFUList.Count);
    AssertEquals('scheduler holds it', 1, Manager.SchedulerItemList.Count);
    AssertEquals('startup holds it', 1, Manager.StartupItemList.Count);
    AssertEquals('hotkeys hold it', 1, Manager.HotKeyItemList.Count);

    Manager.RemoveItemFromLists(Node);

    AssertEquals('mru drained', 0, Manager.MRUList.Count);
    AssertEquals('mfu drained', 0, Manager.MFUList.Count);
    AssertEquals('scheduler drained', 0, Manager.SchedulerItemList.Count);
    AssertEquals('startup drained', 0, Manager.StartupItemList.Count);
    AssertEquals('hotkeys drained', 0, Manager.HotKeyItemList.Count);
  finally
    Config.HotKey := OldHotKey;
  end;
end;

{ TTestHotkeyItemsList }

procedure TTestHotkeyItemsList.TestAddItemTransientIDReturnsMinusOne;
var
  Node: TvFileNodeData;
begin
  // ID is -1 until the database assigns one: nothing is stored
  Node := AddFile('File', '/bin/sh');
  AssertEquals('transient refused', -1,
    ASuiteManager.ListManager.HotKeyItemList.AddItem(Node));
  AssertEquals('list stays empty', 0,
    ASuiteManager.ListManager.HotKeyItemList.Count);
end;

procedure TTestHotkeyItemsList.TestAddItemRegistersWithoutDesktop;
var
  Node: TvFileNodeData;
  OldHotKey: Boolean;
  List: THotkeyItemsList;
begin
  OldHotKey := Config.HotKey;
  Config.HotKey := False; // never register with the desktop portal
  try
    List := ASuiteManager.ListManager.HotKeyItemList;
    Node := AddFile('File', '/bin/sh');
    Node.ID := 42;
    Node.Hotkey := TextToShortCut('Ctrl+M');

    AssertEquals('node stored', 1, List.Count);
    List.RemoveItem(Node);
    AssertEquals('node removed', 0, List.Count);
  finally
    Config.HotKey := OldHotKey;
  end;
end;

procedure TTestHotkeyItemsList.TestIndexOfID;
var
  First, Second: TvFileNodeData;
  OldHotKey: Boolean;
  List: THotkeyItemsList;
begin
  OldHotKey := Config.HotKey;
  Config.HotKey := False; // never register with the desktop portal
  try
    List := ASuiteManager.ListManager.HotKeyItemList;
    First := AddFile('First', '/bin/a');
    First.ID := 1;
    First.Hotkey := TextToShortCut('Ctrl+1');
    Second := AddFile('Second', '/bin/b');
    Second.ID := 2;
    Second.Hotkey := TextToShortCut('Ctrl+2');

    AssertTrue('found by ID', List.IndexOfID(2) = Second);
    AssertTrue('missing ID returns nil', List.IndexOfID(99) = nil);
  finally
    Config.HotKey := OldHotKey;
  end;
end;

initialization
  RegisterTest(TTestBaseItemsList);
  RegisterTest(TTestSpecialItemsList);
  RegisterTest(TTestListManager);
  RegisterTest(TTestHotkeyItemsList);

end.
