unit Tests.NodeDataTypes;

{$mode objfpc}{$H+}

{ Behaviour tests for the node data classes (NodeDataTypes.*).

  TvBaseNodeData and TvSeparatorNodeData are covered with plain test cases:
  they never touch Config or the singleton lists. TvCustomRealNodeData
  setters (Hotkey/SchMode/Autorun) do, so those tests run on the shared tree
  fixture, which drains every list in teardown; Config.HotKey is switched
  off around the hotkey tests so nothing is ever registered with the
  desktop.

  TvCustomRealNodeData.Execute is covered through a test double: the real
  InternalExecute of file nodes would launch processes. }

interface

uses
  Classes, SysUtils, DateUtils, LCLProc, fpcunit, testregistry,
  VirtualTrees,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Category,
  NodeDataTypes.Separator, NodeDataTypes.Files,
  VirtualTree.Methods,
  Kernel.Enumerations, Kernel.Types, Kernel.Manager, Lists.Manager, Lists.HotKey,
  AppConfig.Main,
  Tests.Helpers.Tree;

type
  TTestBaseNodeData = class(TTestCase)
  private
    FData: TvBaseNodeData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultsIDAndParent;
    procedure TestDefaultsNameAndDescription;
    procedure TestDefaultsDatesAreNow;
    procedure TestDefaultsHideFromMenuFalse;
    procedure TestDefaultsIconAssigned;
    procedure TestNameSetsMenuCaptionWithDoubledAmpersands;
    procedure TestDataTypePredicates;
    procedure TestCopyPrefixesName;
    procedure TestCopyDuplicatesFields;
    procedure TestChangedStampsEditDate;
    procedure TestChangedFalseStillStamps;
    procedure TestUnixDateRoundTrip;
    procedure TestSetPointerNodeNil;
  end;

  { Drives the Execute template without launching anything. }
  TTestableCustomNode = class(TvCustomRealNodeData)
  private
    FTestResult: Boolean;
    FCalls: string;
  protected
    function InternalExecute(ARunFromCategory: Boolean;
      ACheckSingleInstance: Boolean): Boolean; override;
    function InternalExecuteAsUser(ARunFromCategory: Boolean;
      AUserData: TUserData): Boolean; override;
    function InternalExecuteAsAdmin(ARunFromCategory: Boolean): Boolean; override;
  public
    property TestResult: Boolean read FTestResult write FTestResult;
    property Calls: string read FCalls;
  end;

  TTestCustomExecute = class(TTestCase)
  private
    FNode: TTestableCustomNode;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteRoutesToInternalExecute;
    procedure TestExecuteFailureSkipsAfterExecute;
    procedure TestExecuteAsUserRoutes;
    procedure TestExecuteAsAdminRoutes;
  end;

  TTestCustomNodeLists = class(TNodeTreeTestCase)
  published
    procedure TestCustomDefaults;
    procedure TestSchDateTimeZeroBecomesNow;
    procedure TestIsHotkeyActive;
    procedure TestSetHotkeyRegistersAndUnregisters;
    procedure TestSetSchModeRegistersScheduler;
    procedure TestSetAutorunRegistersLists;
    procedure TestListSettersAreIdleWhenImporting;
    procedure TestCopyCopiesCustomFields;
    procedure TestUpdateHotkeyFromDesktopStoresWithoutRegistering;
    procedure TestParentNodeFollowsTree;
  end;

  TTestSeparatorNodeData = class(TTestCase)
  published
    procedure TestSeparatorDefaults;
    procedure TestSeparatorCopy;
  end;

  TTestCategoryNodeData = class(TNodeTreeTestCase)
  published
    procedure TestCategoryDefaults;
    procedure TestExecuteRunsOnlyFlaggedChildren;
    procedure TestExecuteWithConfirmationAndNoRunnableChildrenReturnsFalse;
    procedure TestExecuteEmptyCategory;
  end;

implementation

{ TTestBaseNodeData }

procedure TTestBaseNodeData.SetUp;
begin
  inherited SetUp;
  FData := TvBaseNodeData.Create(vtdtFile);
end;

procedure TTestBaseNodeData.TearDown;
begin
  FData.Free;
  inherited TearDown;
end;

procedure TTestBaseNodeData.TestDefaultsIDAndParent;
begin
  AssertEquals('ID starts unassigned', -1, FData.ID);
  AssertEquals('ParentID starts unassigned', -1, FData.ParentID);
end;

procedure TTestBaseNodeData.TestDefaultsNameAndDescription;
begin
  AssertEquals('Name starts empty', '', UTF8Encode(FData.Name));
  AssertEquals('Description starts empty', '', UTF8Encode(FData.Description));
end;

procedure TTestBaseNodeData.TestDefaultsDatesAreNow;
begin
  AssertTrue('AddDate is now',
    Abs(DateTimeToUnix(Now) - FData.UnixAddDate) <= 120);
  AssertTrue('EditDate is now',
    Abs(DateTimeToUnix(Now) - FData.UnixEditDate) <= 120);
end;

procedure TTestBaseNodeData.TestDefaultsHideFromMenuFalse;
begin
  AssertFalse('visible in menu by default', FData.HideFromMenu);
end;

procedure TTestBaseNodeData.TestDefaultsIconAssigned;
begin
  AssertNotNull('every node owns its icon', FData.Icon);
end;

procedure TTestBaseNodeData.TestNameSetsMenuCaptionWithDoubledAmpersands;
begin
  FData.Name := 'A&B';
  AssertEquals('Name keeps single ampersand', 'A&B', UTF8Encode(FData.Name));
  AssertEquals('MenuCaption escapes ampersands for menus', 'A&&B',
    UTF8Encode(FData.MenuCaption));

  FData.Name := 'plain';
  AssertEquals('MenuCaption without ampersands', 'plain',
    UTF8Encode(FData.MenuCaption));
end;

procedure TTestBaseNodeData.TestDataTypePredicates;
var
  FileData, FolderData, CatData, SepData: TvBaseNodeData;
begin
  FileData := TvBaseNodeData.Create(vtdtFile);
  FolderData := TvBaseNodeData.Create(vtdtFolder);
  CatData := TvBaseNodeData.Create(vtdtCategory);
  SepData := TvBaseNodeData.Create(vtdtSeparator);
  try
    AssertTrue('file is a file item', FileData.IsFileItem);
    AssertTrue('folder is a file item', FolderData.IsFileItem);
    AssertFalse('category is not a file item', CatData.IsFileItem);
    AssertFalse('separator is not a file item', SepData.IsFileItem);

    AssertFalse('file is not a category', FileData.IsCategoryItem);
    AssertFalse('folder is not a category', FolderData.IsCategoryItem);
    AssertTrue('category is a category', CatData.IsCategoryItem);
    AssertFalse('separator is not a category', SepData.IsCategoryItem);

    AssertFalse('file is not a separator', FileData.IsSeparatorItem);
    AssertFalse('folder is not a separator', FolderData.IsSeparatorItem);
    AssertFalse('category is not a separator', CatData.IsSeparatorItem);
    AssertTrue('separator is a separator', SepData.IsSeparatorItem);
  finally
    FileData.Free;
    FolderData.Free;
    CatData.Free;
    SepData.Free;
  end;
end;

procedure TTestBaseNodeData.TestCopyPrefixesName;
var
  Dest: TvBaseNodeData;
begin
  FData.Name := 'Item';
  Dest := TvBaseNodeData.Create(vtdtFile);
  try
    Dest.Copy(FData);
    AssertEquals('copied name gets the copy prefix', 'Copy_Item',
      UTF8Encode(Dest.Name));
  finally
    Dest.Free;
  end;
end;

procedure TTestBaseNodeData.TestCopyDuplicatesFields;
var
  Dest: TvBaseNodeData;
begin
  FData.Name := 'Item';
  FData.HideFromMenu := True;
  FData.Description := 'desc';
  Dest := TvBaseNodeData.Create(vtdtCategory);
  try
    Dest.Copy(FData);
    AssertTrue('data type follows the source', Dest.DataType = vtdtFile);
    AssertTrue('HideFromMenu follows the source', Dest.HideFromMenu);
    AssertEquals('Description follows the source', 'desc',
      UTF8Encode(Dest.Description));
  finally
    Dest.Free;
  end;
end;

procedure TTestBaseNodeData.TestChangedStampsEditDate;
begin
  FData.UnixEditDate := SAMPLE_UNIX_DATE;
  FData.Changed := True;
  AssertTrue('node is marked changed', FData.Changed);
  AssertTrue('EditDate moves to now', FData.UnixEditDate > SAMPLE_UNIX_DATE);
end;

procedure TTestBaseNodeData.TestChangedFalseStillStamps;
begin
  // Quirk: assigning Changed always restamps EditDate, even with False
  FData.UnixEditDate := SAMPLE_UNIX_DATE;
  FData.Changed := False;
  AssertFalse('node is not marked changed', FData.Changed);
  AssertTrue('EditDate still moves to now', FData.UnixEditDate > SAMPLE_UNIX_DATE);
end;

procedure TTestBaseNodeData.TestUnixDateRoundTrip;
begin
  FData.UnixAddDate := SAMPLE_UNIX_DATE;
  AssertEquals('AddDate reads back the unix date', SAMPLE_UNIX_DATE,
    DateTimeToUnix(FData.AddDate));
  FData.UnixEditDate := SAMPLE_UNIX_DATE;
  AssertEquals('EditDate reads back the unix date', SAMPLE_UNIX_DATE,
    DateTimeToUnix(FData.EditDate));
end;

procedure TTestBaseNodeData.TestSetPointerNodeNil;
begin
  FData.SetPointerNode(nil);
  AssertTrue('PNode is nil', FData.PNode = nil);
end;

{ TTestableCustomNode }

function TTestableCustomNode.InternalExecute(ARunFromCategory: Boolean;
  ACheckSingleInstance: Boolean): Boolean;
begin
  FCalls := FCalls + 'E';
  Result := FTestResult;
end;

function TTestableCustomNode.InternalExecuteAsUser(ARunFromCategory: Boolean;
  AUserData: TUserData): Boolean;
begin
  FCalls := FCalls + 'U';
  Result := FTestResult;
end;

function TTestableCustomNode.InternalExecuteAsAdmin(
  ARunFromCategory: Boolean): Boolean;
begin
  FCalls := FCalls + 'A';
  Result := FTestResult;
end;

{ TTestCustomExecute }

procedure TTestCustomExecute.SetUp;
begin
  inherited SetUp;
  FNode := TTestableCustomNode.Create(vtdtFile);
end;

procedure TTestCustomExecute.TearDown;
begin
  FNode.Free;
  inherited TearDown;
end;

procedure TTestCustomExecute.TestExecuteRoutesToInternalExecute;
begin
  FNode.TestResult := True;
  // ADoActionOnExe stays False: running the post-execute action would hide
  // frmMain, which does not exist in the test binary
  AssertTrue('success propagates', FNode.Execute(False, False, False));
  AssertEquals('InternalExecute ran once', 'E', FNode.Calls);
  AssertTrue('AfterExecute marks the node changed', FNode.Changed);
end;

procedure TTestCustomExecute.TestExecuteFailureSkipsAfterExecute;
begin
  FNode.TestResult := False;
  AssertFalse('failure propagates', FNode.Execute(False, False, False));
  AssertEquals('InternalExecute ran once', 'E', FNode.Calls);
  AssertFalse('AfterExecute is skipped on failure', FNode.Changed);
end;

procedure TTestCustomExecute.TestExecuteAsUserRoutes;
var
  UserData: TUserData;
begin
  UserData.UserName := 'u';
  UserData.Password := 'p';
  FNode.TestResult := True;
  AssertTrue('success propagates', FNode.ExecuteAsUser(False, False, UserData));
  AssertEquals('InternalExecuteAsUser ran once', 'U', FNode.Calls);
  AssertTrue('AfterExecute marks the node changed', FNode.Changed);
end;

procedure TTestCustomExecute.TestExecuteAsAdminRoutes;
begin
  FNode.TestResult := True;
  AssertTrue('success propagates', FNode.ExecuteAsAdmin(False, False));
  AssertEquals('InternalExecuteAsAdmin ran once', 'A', FNode.Calls);
  AssertTrue('AfterExecute marks the node changed', FNode.Changed);
end;

{ TTestCustomNodeLists }

procedure TTestCustomNodeLists.TestCustomDefaults;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  AssertEquals('no custom icon by default', '', UTF8Encode(Node.PathIcon));
  AssertTrue('scheduler off by default', Node.SchMode = smDisabled);
  AssertEquals('no hotkey by default', 0, Node.Hotkey);
  AssertFalse('hotkey inactive by default', Node.IsHotkeyActive);
  AssertTrue('never in autorun by default', Node.Autorun = atNever);
  AssertTrue('default post-execute action', Node.ActionOnExe = aeDefault);
  AssertEquals('default window state', 0, Node.WindowState);
  AssertEquals('never accessed by default', -1, Node.LastAccess);
end;

procedure TTestCustomNodeLists.TestSchDateTimeZeroBecomesNow;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  Node.SchDateTime := 0;
  AssertTrue('zero scheduler date becomes now',
    Abs(Now - Node.SchDateTime) < (1 / 24 / 60));
end;

procedure TTestCustomNodeLists.TestIsHotkeyActive;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  AssertFalse('no hotkey, not active', Node.IsHotkeyActive);
  Node.Hotkey := TextToShortCut('Ctrl+A');
  AssertTrue('hotkey set, active', Node.IsHotkeyActive);
  Node.Hotkey := 0;
  AssertFalse('hotkey cleared, inactive', Node.IsHotkeyActive);
end;

procedure TTestCustomNodeLists.TestSetHotkeyRegistersAndUnregisters;
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

    Node.Hotkey := TextToShortCut('Ctrl+A');
    AssertEquals('node enters the hotkey list', 1, List.Count);
    AssertTrue('it is the same node', List[0] = Node);

    Node.Hotkey := 0;
    AssertEquals('node leaves the hotkey list', 0, List.Count);
  finally
    Config.HotKey := OldHotKey;
  end;
end;

procedure TTestCustomNodeLists.TestSetSchModeRegistersScheduler;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');

  Node.SchMode := smOnce;
  AssertEquals('node enters the scheduler list', 1,
    ASuiteManager.ListManager.SchedulerItemList.Count);

  Node.SchMode := smOnce;
  AssertEquals('setting the same mode twice does not duplicate', 1,
    ASuiteManager.ListManager.SchedulerItemList.Count);

  Node.SchMode := smDisabled;
  AssertEquals('node leaves the scheduler list', 0,
    ASuiteManager.ListManager.SchedulerItemList.Count);
end;

procedure TTestCustomNodeLists.TestSetAutorunRegistersLists;
var
  Node: TvFileNodeData;
  Manager: TListManager;
begin
  Node := AddFile('File', '/bin/sh');
  Manager := ASuiteManager.ListManager;

  Node.Autorun := atAlwaysOnStart;
  AssertEquals('startup list gains the node', 1, Manager.StartupItemList.Count);
  AssertEquals('shutdown list untouched', 0, Manager.ShutdownItemList.Count);

  Node.Autorun := atAlwaysOnClose;
  AssertEquals('startup list loses the node', 0, Manager.StartupItemList.Count);
  AssertEquals('shutdown list gains the node', 1, Manager.ShutdownItemList.Count);

  Node.Autorun := atNever;
  AssertEquals('shutdown list loses the node', 0, Manager.ShutdownItemList.Count);

  Node.Autorun := atSingleInstance;
  AssertEquals('single instance items run at startup', 1,
    Manager.StartupItemList.Count);
end;

procedure TTestCustomNodeLists.TestListSettersAreIdleWhenImporting;
var
  Node: TvFileNodeData;
  OldState: TLauncherState;
  Manager: TListManager;
begin
  OldState := Config.ASuiteState;
  Config.ASuiteState := lsImporting;
  try
    Manager := ASuiteManager.ListManager;
    Node := AddFile('File', '/bin/sh');
    Node.ID := 42;
    Node.Hotkey := TextToShortCut('Ctrl+A');
    Node.SchMode := smOnce;
    Node.Autorun := atAlwaysOnStart;
    Node.ClickCount := 3;
    Node.LastAccess := SAMPLE_UNIX_DATE;
    Node.NoMFU := True;
    Node.NoMRU := True;

    AssertEquals('hotkey list untouched', 0, Manager.HotKeyItemList.Count);
    AssertEquals('scheduler list untouched', 0, Manager.SchedulerItemList.Count);
    AssertEquals('startup list untouched', 0, Manager.StartupItemList.Count);
    AssertEquals('MFU list untouched', 0, Manager.MFUList.Count);
    AssertEquals('MRU list untouched', 0, Manager.MRUList.Count);
  finally
    Config.ASuiteState := OldState;
  end;
end;

procedure TTestCustomNodeLists.TestCopyCopiesCustomFields;
var
  Source, Dest: TvFileNodeData;
  OldState: TLauncherState;
begin
  // Importing state keeps every setter away from the singleton lists, so
  // this test only checks the copied field values
  OldState := Config.ASuiteState;
  Config.ASuiteState := lsImporting;
  try
    Source := AddFile('Source', '/bin/src');
    Source.PathIcon := 'icons\app.ico';
    Source.WindowState := 2;
    Source.ActionOnExe := aeRunAndHide;
    Source.Autorun := atAlwaysOnStart;
    Source.SchMode := smOnce;
    Source.Hotkey := TextToShortCut('Ctrl+A');

    Dest := AddFile('Dest', '/bin/dest');
    Dest.Copy(Source);

    AssertEquals('PathIcon copied', 'icons\app.ico', UTF8Encode(Dest.PathIcon));
    AssertEquals('WindowState copied', 2, Dest.WindowState);
    AssertTrue('ActionOnExe copied', Dest.ActionOnExe = aeRunAndHide);
    AssertTrue('Autorun copied', Dest.Autorun = atAlwaysOnStart);
    AssertTrue('SchMode copied', Dest.SchMode = smOnce);
    AssertEquals('Hotkey copied', TextToShortCut('Ctrl+A'), Dest.Hotkey);
  finally
    Config.ASuiteState := OldState;
  end;
end;

procedure TTestCustomNodeLists.TestUpdateHotkeyFromDesktopStoresWithoutRegistering;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  Node.UpdateHotkeyFromDesktop(TextToShortCut('Ctrl+B'));
  AssertEquals('desktop trigger stored', TextToShortCut('Ctrl+B'), Node.Hotkey);
  AssertEquals('nothing registered', 0,
    ASuiteManager.ListManager.HotKeyItemList.Count);
end;

procedure TTestCustomNodeLists.TestParentNodeFollowsTree;
var
  CatNode, FileNode: PVirtualNode;
  CatData, FileData: TvBaseNodeData;
begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;
  AddFile('File', '/bin/sh', CatNode);
  FileNode := Tree.GetFirstChild(CatNode);

  CatData := TVirtualTreeMethods.GetNodeItemData(CatNode, Tree);
  FileData := TVirtualTreeMethods.GetNodeItemData(FileNode, Tree);
  AssertTrue('top level parent is the root', CatData.ParentNode = Tree.RootNode);
  AssertTrue('child parent is the category', FileData.ParentNode = CatNode);
end;

{ TTestSeparatorNodeData }

procedure TTestSeparatorNodeData.TestSeparatorDefaults;
var
  Sep: TvSeparatorNodeData;
begin
  Sep := TvSeparatorNodeData.Create;
  try
    AssertTrue('separator data type', Sep.DataType = vtdtSeparator);
    AssertTrue('separator flag', Sep.IsSeparatorItem);
    AssertFalse('not a file item', Sep.IsFileItem);
    AssertFalse('not a category item', Sep.IsCategoryItem);
    AssertEquals('separators start unnamed', '', UTF8Encode(Sep.Name));
  finally
    Sep.Free;
  end;
end;

procedure TTestSeparatorNodeData.TestSeparatorCopy;
var
  Source: TvSeparatorNodeData;
  Dest: TvBaseNodeData;
begin
  Source := TvSeparatorNodeData.Create;
  Dest := TvBaseNodeData.Create(vtdtFile);
  try
    Dest.Copy(Source);
    AssertTrue('data type follows the source', Dest.DataType = vtdtSeparator);
    AssertEquals('even an empty name gets the copy prefix', 'Copy_',
      UTF8Encode(Dest.Name));
  finally
    Source.Free;
    Dest.Free;
  end;
end;

{ TTestCategoryNodeData }

procedure TTestCategoryNodeData.TestCategoryDefaults;
var
  Cat: TvCategoryNodeData;
begin
  Cat := AddCategory('Cat');
  AssertTrue('category data type', Cat.DataType = vtdtCategory);
  AssertTrue('category flag', Cat.IsCategoryItem);
  AssertFalse('not a file item', Cat.IsFileItem);
end;

procedure TTestCategoryNodeData.TestExecuteRunsOnlyFlaggedChildren;
var
  Cat: TvCategoryNodeData;
  OldConfirm: Boolean;
begin
  OldConfirm := Config.ConfirmRunCat;
  Config.ConfirmRunCat := False;
  try
    Cat := AddCategory('Cat');
    AddFile('Skipped', '/bin/skip', Tree.GetFirst);
    AddSeparator('---', Tree.GetFirst);
    // No child has RunFromCategory: the callback skips everything and
    // nothing is ever launched
    AssertTrue('category with idle children runs clean', Cat.Execute(False, False, False));
  finally
    Config.ConfirmRunCat := OldConfirm;
  end;
end;

procedure TTestCategoryNodeData.TestExecuteWithConfirmationAndNoRunnableChildrenReturnsFalse;
var
  Cat: TvCategoryNodeData;
  OldConfirm: Boolean;
begin
  OldConfirm := Config.ConfirmRunCat;
  Config.ConfirmRunCat := True;
  try
    Cat := AddCategory('Cat');
    AddFile('Skipped', '/bin/skip', Tree.GetFirst);
    // No runnable children: the confirmation dialog is never asked and the
    // whole execution is refused
    AssertFalse('category without runnable children refuses',
      Cat.Execute(False, False, False));
  finally
    Config.ConfirmRunCat := OldConfirm;
  end;
end;

procedure TTestCategoryNodeData.TestExecuteEmptyCategory;
var
  Cat: TvCategoryNodeData;
  OldConfirm: Boolean;
begin
  OldConfirm := Config.ConfirmRunCat;
  Config.ConfirmRunCat := False;
  try
    Cat := AddCategory('Cat');
    AssertTrue('empty category runs clean', Cat.Execute(False, False, False));
  finally
    Config.ConfirmRunCat := OldConfirm;
  end;
end;

initialization
  RegisterTest(TTestBaseNodeData);
  RegisterTest(TTestCustomExecute);
  RegisterTest(TTestCustomNodeLists);
  RegisterTest(TTestSeparatorNodeData);
  RegisterTest(TTestCategoryNodeData);

end.
