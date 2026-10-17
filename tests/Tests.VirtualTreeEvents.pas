unit Tests.VirtualTreeEvents;

{$mode objfpc}{$H+}

{ Behaviour tests for TVirtualTreeEvents (VirtualTree.Events).

  Every handler is exercised through the tree event property it is wired to
  (for example Tree.OnGetText), after the matching SetupVST* call: this
  covers the private handlers without any RTTI tricks. The data modules the
  setups expect (dmImages, dmTrayMenu) are bare CreateNew instances, so no
  form file is ever streamed and no tray icon is ever created.

  Deliberately excluded: every path that saves the list (DoNewText,
  DoDragDrop, DoKeyPress on Enter, DoNodeDblClick on a node, ShowItemProperty)
  because RefreshList dereferences the running application's dmTrayMenu;
  DoSingleClickGM (needs the graphic menu form and the mouse cursor);
  DoBeforeCellPaintHotkey (paints through Tree.Images, which is nil here). }

interface

uses
  Classes, SysUtils, Types, Graphics, Forms, ExtCtrls, Controls, LCLProc,
  ImgList,
  fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  VirtualTree.Events, VirtualTree.Methods,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  NodeDataTypes.Category, NodeDataTypes.Separator,
  Kernel.Types, Kernel.Enumerations,
  AppConfig.Main, Kernel.Instance, DataModules.Icons, DataModules.TrayMenu,
  Tests.Helpers.Tree;

type
  { Creates the event source and the bare data modules; subclasses wire one
    SetupVST* flavour each, because wiring a second flavour would overwrite
    the first one's handlers. }
  TEventsTestCase = class(TNodeTreeTestCase)
  protected
    FEvents: TVirtualTreeEvents;
    procedure SetUp; override;
    procedure TearDown; override;
    // The default tree options include toAutoSort, which re-sorts on every
    // insert once OnCompareNodes is wired. Test trees keep insertion order
    // instead: every comparison below passes explicit node pairs, and the
    // production sort would also run on half-initialized nodes during the
    // insert itself (their PNode is set only after AddChild returns).
    procedure NoAutoSort(ATree: TVirtualStringTree);
  end;

  TTestEventsListText = class(TEventsTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestGetTextNameColumn;
    procedure TestGetTextParentColumn;
    procedure TestGetTextBlankSeparator;
    procedure TestGetImageIndexOtherColumn;
    procedure TestGetImageIndexSeparatorColumn;
    procedure TestGetImageIndexOtherKind;
    procedure TestEditingAllowed;
    procedure TestEditingSeparatorDenied;
    procedure TestEditingSingleClickDenied;
    procedure TestMeasureSeparator;
    procedure TestMeasureRegular;
    procedure TestPaintMissingPathRed;
    procedure TestPaintExistingPathKeepsColor;
    procedure TestGetHintWithDescription;
    procedure TestGetHintWithoutDescription;
    procedure TestDrawTextRegularKeepsDefault;
    procedure TestDrawTextSeparatorDisablesDefault;
    procedure TestExpandedIsSafe;
    procedure TestDragOverAccepts;
    procedure TestKeyPressOtherKey;
    procedure TestSingleClickWithoutRunOnClick;
    procedure TestDoubleClickOnButton;
    procedure TestCompareListCategoryFirst;
    procedure TestCompareListAlphabetical;
    procedure TestCompareListSeparatorNeutral;
    procedure TestCompareListDifferentBlocksNeutral;
    procedure TestNodeDataSizeList;
  end;

  TTestEventsHotkey = class(TEventsTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestGetTextHotkeyColumns;
    procedure TestGetTextHotkeyTypeColumnEmpty;
    procedure TestCompareHotkeyByName;
    procedure TestCompareHotkeyCategoryFirst;
    procedure TestCompareHotkeyByParent;
    procedure TestCompareHotkeyByShortcut;
    procedure TestGetImageIndexHotkeyOtherColumnUntouched;
  end;

  TTestEventsAutorun = class(TEventsTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestGetTextAutorunColumns;
    procedure TestGetTextAutorunCategoryPathEmpty;
    procedure TestGetTextAutorunMiddleColumnEmpty;
  end;

  TTestEventsSearch = class(TEventsTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestCompareSearchByName;
    procedure TestCompareSearchByCategory;
    procedure TestNodeDataSizeSearch;
  end;

  TTestEventsFrames = class(TEventsTestCase)
  private
    FFrameTree: TVirtualStringTree;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetTextFrameDefault;
    procedure TestGetTextFrameCustom;
    procedure TestGetImageIndexFrame;
    procedure TestGetImageIndexFrameOtherKind;
    procedure TestNodeDataSizeFrame;
    procedure TestFreeNodeFrameClearsTitle;
    procedure TestAddToSelectionOutsideDialogIsSafe;
  end;

  TTestEventsSaveLoad = class(TEventsTestCase)
  private
    FTarget: TVirtualStringTree;
    procedure FreeTargetNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure BuildSource;
    procedure StreamToTarget;
    function TargetData(ANode: PVirtualNode): TvBaseNodeData;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripPreservesStructure;
    procedure TestRoundTripCopiesFields;
    procedure TestRoundTripResetsHotkey;
    procedure TestRoundTripCreatesDistinctInstances;
  end;

  TTestEventsMisc = class(TEventsTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestColumnResizeNilIsSafe;
    procedure TestResizeBigNodes;
    procedure TestResizeSmallNodes;
    procedure TestScrollAlignsOffset;
  end;

implementation

{ TEventsTestCase }

procedure TEventsTestCase.SetUp;
begin
  inherited SetUp;
  // Bare instances: no .lfm is streamed, no tray icon is created. Only the
  // globals' existence matters to the SetupVST* calls.
  dmImages := TdmImages.CreateNew(nil);
  dmTrayMenu := TdmTrayMenu.CreateNew(nil);
  FEvents := TVirtualTreeEvents.Create;
end;

procedure TEventsTestCase.TearDown;
begin
  FreeAndNil(dmTrayMenu);
  FreeAndNil(dmImages);
  FreeAndNil(FEvents);
  inherited TearDown;
end;

procedure TEventsTestCase.NoAutoSort(ATree: TVirtualStringTree);
begin
  ATree.TreeOptions.AutoOptions :=
    ATree.TreeOptions.AutoOptions - [toAutoSort];
end;

{ TTestEventsListText }

procedure TTestEventsListText.SetUp;
begin
  inherited SetUp;
  FEvents.SetupVSTList(Tree);
  NoAutoSort(Tree);
end;

procedure TTestEventsListText.TestGetTextNameColumn;
var
  CellText: string;
begin
  AddFile('File', '/bin/sh');
  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirst, 0, ttNormal, CellText);
  AssertEquals('name column shows the name', 'File', UTF8Encode(CellText));
end;

procedure TTestEventsListText.TestGetTextParentColumn;
var
  CellText: string;
begin
  AddCategory('Cat');
  AddFile('File', '/bin/sh', Tree.GetFirst);
  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirstChild(Tree.GetFirst), 1, ttNormal, CellText);
  AssertEquals('parent column shows the category', 'Cat', UTF8Encode(CellText));

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirst, 1, ttNormal, CellText);
  AssertEquals('root items report <Root>', '<Root>', UTF8Encode(CellText));
end;

procedure TTestEventsListText.TestGetTextBlankSeparator;
var
  CellText: string;
begin
  AddSeparator('');
  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirst, 0, ttNormal, CellText);
  AssertEquals('blank separator shows a space', ' ', UTF8Encode(CellText));
end;

procedure TTestEventsListText.TestGetImageIndexOtherColumn;
var
  ImageIndex: TImageIndex;
  Ghosted: Boolean;
begin
  AddFile('File', '/bin/sh');
  ImageIndex := 7;
  Ghosted := False;
  Tree.OnGetImageIndex(Tree, Tree.GetFirst, ikNormal, 1, Ghosted, ImageIndex);
  AssertEquals('no icon outside column zero', -1, ImageIndex);
end;

procedure TTestEventsListText.TestGetImageIndexSeparatorColumn;
var
  ImageIndex: TImageIndex;
  Ghosted: Boolean;
begin
  // TNodeIcon.LoadIcon bails out immediately for separators, so this never
  // touches the icon manager
  AddSeparator('---');
  ImageIndex := 7;
  Ghosted := False;
  Tree.OnGetImageIndex(Tree, Tree.GetFirst, ikNormal, 0, Ghosted, ImageIndex);
  AssertEquals('separator has no icon', -1, ImageIndex);
end;

procedure TTestEventsListText.TestGetImageIndexOtherKind;
var
  ImageIndex: TImageIndex;
  Ghosted: Boolean;
begin
  AddFile('File', '/bin/sh');
  ImageIndex := 7;
  Ghosted := False;
  Tree.OnGetImageIndex(Tree, Tree.GetFirst, ikState, 0, Ghosted, ImageIndex);
  AssertEquals('state icons untouched', 7, ImageIndex);
end;

procedure TTestEventsListText.TestEditingAllowed;
var
  Allowed: Boolean;
begin
  AddFile('File', '/bin/sh');
  Allowed := False;
  Tree.OnEditing(Tree, Tree.GetFirst, 0, Allowed);
  AssertTrue('regular nodes are editable', Allowed);
end;

procedure TTestEventsListText.TestEditingSeparatorDenied;
var
  Allowed: Boolean;
begin
  AddSeparator('---');
  Allowed := True;
  Tree.OnEditing(Tree, Tree.GetFirst, 0, Allowed);
  AssertFalse('separators are not editable', Allowed);
end;

procedure TTestEventsListText.TestEditingSingleClickDenied;
var
  Allowed: Boolean;
  OldSingleClick: Boolean;
begin
  OldSingleClick := Config.RunSingleClick;
  Config.RunSingleClick := True;
  try
    AddFile('File', '/bin/sh');
    Allowed := True;
    Tree.OnEditing(Tree, Tree.GetFirst, 0, Allowed);
    AssertFalse('no editing when a single click runs', Allowed);
  finally
    Config.RunSingleClick := OldSingleClick;
  end;
end;

procedure TTestEventsListText.TestMeasureSeparator;
var
  Height: Integer;
begin
  AddSeparator('---');
  Height := 0;
  Tree.OnMeasureItem(Tree, nil, Tree.GetFirst, Height);
  AssertEquals('separator uses the small height',
    ASuiteInstance.SmallHeightNode, Height);
end;

procedure TTestEventsListText.TestMeasureRegular;
var
  Height: Integer;
begin
  AddFile('File', '/bin/sh');
  Height := 33;
  Tree.OnMeasureItem(Tree, nil, Tree.GetFirst, Height);
  AssertEquals('regular nodes keep their height', 33, Height);
end;

procedure TTestEventsListText.TestPaintMissingPathRed;
var
  Bmp: Graphics.TBitmap;
begin
  AddFile('Missing', '/definitely/missing/xyz.tmp');
  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.SetSize(100, 30);
    Tree.OnPaintText(Tree, Bmp.Canvas, Tree.GetFirst, 0, ttNormal);
    AssertTrue('missing paths paint red', Bmp.Canvas.Font.Color = clRed);
  finally
    Bmp.Free;
  end;
end;

procedure TTestEventsListText.TestPaintExistingPathKeepsColor;
var
  Bmp: Graphics.TBitmap;
  Tmp: string;
  Data: TvFileNodeData;
begin
  Tmp := WriteTempFile('asuite_test_paint.tmp');
  try
    Data := AddFile('File', '/bin/sh');
    Data.PathFile := UTF8Decode(Tmp);
    Tree.Font.Color := clBlue;
    Bmp := Graphics.TBitmap.Create;
    try
      Bmp.SetSize(100, 30);
      Tree.OnPaintText(Tree, Bmp.Canvas, Tree.GetFirst, 0, ttNormal);
      AssertTrue('existing paths keep the tree color',
        Bmp.Canvas.Font.Color = clBlue);
    finally
      Bmp.Free;
    end;
  finally
    DeleteFile(Tmp);
  end;
end;

procedure TTestEventsListText.TestGetHintWithDescription;
var
  HintText: string;
  BreakStyle: TVTTooltipLineBreakStyle;
  Data: TvFileNodeData;
begin
  Data := AddFile('File', '/bin/sh');
  Data.Description := 'hint text';
  HintText := '';
  Tree.OnGetHint(Tree, Tree.GetFirst, 0, BreakStyle, HintText);
  AssertEquals('description becomes the hint', 'hint text', UTF8Encode(HintText));
end;

procedure TTestEventsListText.TestGetHintWithoutDescription;
var
  HintText: string;
  BreakStyle: TVTTooltipLineBreakStyle;
begin
  AddFile('File', '/bin/sh');
  HintText := 'keep me';
  Tree.OnGetHint(Tree, Tree.GetFirst, 0, BreakStyle, HintText);
  AssertEquals('no description leaves the hint alone', 'keep me',
    UTF8Encode(HintText));
end;

procedure TTestEventsListText.TestDrawTextRegularKeepsDefault;
var
  DefaultDraw: Boolean;
  R: TRect;
begin
  AddFile('File', '/bin/sh');
  R := Rect(0, 0, 200, 30);
  DefaultDraw := True;
  Tree.OnDrawText(Tree, nil, Tree.GetFirst, 0, 'File', R, DefaultDraw);
  AssertTrue('regular nodes use the default draw', DefaultDraw);
end;

procedure TTestEventsListText.TestDrawTextSeparatorDisablesDefault;
var
  DefaultDraw: Boolean;
  R: TRect;
  Bmp: Graphics.TBitmap;
begin
  AddSeparator('Caption');
  R := Rect(0, 0, 200, 30);
  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.SetSize(200, 30);
    DefaultDraw := True;
    Tree.OnDrawText(Tree, Bmp.Canvas, Tree.GetFirst, 0, 'Caption', R,
      DefaultDraw);
    AssertFalse('separators draw themselves', DefaultDraw);
  finally
    Bmp.Free;
  end;
end;

procedure TTestEventsListText.TestExpandedIsSafe;
begin
  AddCategory('Cat');
  // The handler body is intentionally empty; reaching it is the test
  Tree.OnExpanded(Tree, Tree.GetFirst);
end;

procedure TTestEventsListText.TestDragOverAccepts;
var
  Accept: Boolean;
  Effect: Integer;
begin
  Accept := False;
  Effect := 0;
  Tree.OnDragOver(Tree, Tree, [], dsDragEnter, Point(0, 0), dmOnNode, Effect,
    Accept);
  AssertTrue('drops are always accepted', Accept);
end;

procedure TTestEventsListText.TestKeyPressOtherKey;
var
  Key: Char;
begin
  AddFile('File', '/bin/sh');
  Key := 'x';
  Tree.OnKeyPress(Tree, Key);
  AssertEquals('other keys do nothing', 'x', Key);
end;

procedure TTestEventsListText.TestSingleClickWithoutRunOnClick;
var
  HitInfo: THitInfo;
  OldSingleClick: Boolean;
begin
  OldSingleClick := Config.RunSingleClick;
  Config.RunSingleClick := False;
  try
    AddFile('File', '/bin/sh');
    FillChar(HitInfo, SizeOf(HitInfo), 0);
    HitInfo.HitNode := Tree.GetFirst;
    // No button hit and single-click-run off: nothing happens
    Tree.OnNodeClick(Tree, HitInfo);
  finally
    Config.RunSingleClick := OldSingleClick;
  end;
end;

procedure TTestEventsListText.TestDoubleClickOnButton;
var
  HitInfo: THitInfo;
begin
  AddFile('File', '/bin/sh');
  FillChar(HitInfo, SizeOf(HitInfo), 0);
  HitInfo.HitNode := Tree.GetFirst;
  HitInfo.HitPositions := [hiOnItemButton];
  // Clicks on the expand button never execute
  Tree.OnNodeDblClick(Tree, HitInfo);
end;

procedure TTestEventsListText.TestCompareListCategoryFirst;
var
  CatNode, FileNode: PVirtualNode;
  Result: Integer;
begin
  AddCategory('Zebra');
  CatNode := Tree.GetFirst;
  AddFile('Apple', '/bin/a', CatNode);
  FileNode := Tree.GetFirstChild(CatNode);
  // Siblings under the same block (no separator between them)
  Result := 0;
  Tree.OnCompareNodes(Tree, CatNode, FileNode, 0, Result);
  AssertTrue('category sorts before files', Result < 0);
  Tree.OnCompareNodes(Tree, FileNode, CatNode, 0, Result);
  AssertTrue('files sort after categories', Result > 0);
end;

procedure TTestEventsListText.TestCompareListAlphabetical;
var
  First, Second: PVirtualNode;
  Result: Integer;
begin
  AddFile('Apple', '/bin/a');
  First := Tree.GetFirst;
  AddFile('Zebra', '/bin/z');
  Second := Tree.GetNextSibling(First);
  Result := 0;
  Tree.OnCompareNodes(Tree, First, Second, 0, Result);
  AssertTrue('names compare alphabetically', Result < 0);
end;

procedure TTestEventsListText.TestCompareListSeparatorNeutral;
var
  FileNode, SepNode: PVirtualNode;
  Result: Integer;
begin
  AddFile('File', '/bin/sh');
  FileNode := Tree.GetFirst;
  AddSeparator('---');
  SepNode := Tree.GetNextSibling(FileNode);
  Result := 99;
  Tree.OnCompareNodes(Tree, FileNode, SepNode, 0, Result);
  AssertEquals('separators never move', 0, Result);
end;

procedure TTestEventsListText.TestCompareListDifferentBlocksNeutral;
var
  First, Second: PVirtualNode;
  Result: Integer;
begin
  // A separator between two files puts them in different sort blocks
  AddFile('Zebra', '/bin/z');
  First := Tree.GetFirst;
  AddSeparator('---');
  AddFile('Apple', '/bin/a');
  Second := Tree.GetNextSibling(Tree.GetNextSibling(First));
  Result := 99;
  Tree.OnCompareNodes(Tree, First, Second, 0, Result);
  AssertEquals('different blocks never compare', 0, Result);
end;

procedure TTestEventsListText.TestNodeDataSizeList;
var
  Size: Integer;
begin
  Size := 0;
  Tree.OnGetNodeDataSize(Tree, Size);
  AssertEquals('list nodes hold rBaseData', SizeOf(rBaseData), Size);
end;

{ TTestEventsHotkey }

procedure TTestEventsHotkey.SetUp;
begin
  inherited SetUp;
  FEvents.SetupVSTHotkey(Tree);
  NoAutoSort(Tree);
end;

procedure TTestEventsHotkey.TestGetTextHotkeyColumns;
var
  CellText: string;
  Data: TvFileNodeData;
begin
  Data := AddFile('File', '/bin/sh');
  Data.Hotkey := TextToShortCut('Ctrl+H');

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirst, 0, ttNormal, CellText);
  AssertEquals('name column', 'File', UTF8Encode(CellText));

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirst, 2, ttNormal, CellText);
  AssertEquals('root items report <Root>', '<Root>', UTF8Encode(CellText));

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirst, 3, ttNormal, CellText);
  AssertEquals('shortcut column', UTF8Encode(ShortCutToText(TextToShortCut('Ctrl+H'))),
    UTF8Encode(CellText));
end;

procedure TTestEventsHotkey.TestGetTextHotkeyTypeColumnEmpty;
var
  CellText: string;
begin
  // Column 1 is painted by OnBeforeCellPaint; the text handler clears it
  AddFile('File', '/bin/sh');
  CellText := 'keep me';
  Tree.OnGetText(Tree, Tree.GetFirst, 1, ttNormal, CellText);
  AssertEquals('type column has no text', '', UTF8Encode(CellText));
end;

procedure TTestEventsHotkey.TestCompareHotkeyByName;
var
  First, Second: PVirtualNode;
  Result: Integer;
begin
  AddFile('Apple', '/bin/a');
  First := Tree.GetFirst;
  AddFile('Zebra', '/bin/z');
  Second := Tree.GetNextSibling(First);
  Result := 0;
  Tree.OnCompareNodes(Tree, First, Second, 0, Result);
  AssertTrue('names compare alphabetically', Result < 0);
end;

procedure TTestEventsHotkey.TestCompareHotkeyCategoryFirst;
var
  CatNode, FileNode: PVirtualNode;
  Result: Integer;
begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;
  AddFile('File', '/bin/f', CatNode);
  FileNode := Tree.GetFirstChild(CatNode);
  Result := 99;
  Tree.OnCompareNodes(Tree, CatNode, FileNode, 1, Result);
  AssertTrue('categories sort before files', Result < 0);
  Tree.OnCompareNodes(Tree, FileNode, CatNode, 1, Result);
  AssertTrue('files sort after categories', Result > 0);
end;

procedure TTestEventsHotkey.TestCompareHotkeyByParent;
var
  FileNode: PVirtualNode;
  Result: Integer;
begin
  AddCategory('Cat');
  AddFile('File', '/bin/sh', Tree.GetFirst);
  FileNode := Tree.GetFirstChild(Tree.GetFirst);
  Result := 0;
  Tree.OnCompareNodes(Tree, FileNode, FileNode, 2, Result);
  AssertEquals('same parent compares equal', 0, Result);
end;

procedure TTestEventsHotkey.TestCompareHotkeyByShortcut;
var
  First, Second: PVirtualNode;
  FirstData, SecondData: TvFileNodeData;
  Result: Integer;
begin
  FirstData := AddFile('A', '/bin/a');
  FirstData.Hotkey := TextToShortCut('Ctrl+A');
  First := Tree.GetFirst;
  SecondData := AddFile('B', '/bin/b');
  SecondData.Hotkey := TextToShortCut('Ctrl+B');
  Second := Tree.GetNextSibling(First);
  Result := 0;
  Tree.OnCompareNodes(Tree, First, Second, 3, Result);
  AssertTrue('shortcuts compare as text', Result < 0);
end;

procedure TTestEventsHotkey.TestGetImageIndexHotkeyOtherColumnUntouched;
var
  ImageIndex: TImageIndex;
  Ghosted: Boolean;
begin
  AddFile('File', '/bin/sh');
  ImageIndex := 7;
  Ghosted := False;
  Tree.OnGetImageIndex(Tree, Tree.GetFirst, ikNormal, 1, Ghosted, ImageIndex);
  AssertEquals('hotkey icons only load on column zero', 7, ImageIndex);
end;

{ TTestEventsAutorun }

procedure TTestEventsAutorun.SetUp;
begin
  inherited SetUp;
  FEvents.SetupVSTAutorun(Tree);
  NoAutoSort(Tree);
end;

procedure TTestEventsAutorun.TestGetTextAutorunColumns;
var
  CellText: string;
begin
  AddCategory('Cat');
  AddFile('File', '/bin/sh', Tree.GetFirst);

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirstChild(Tree.GetFirst), 0, ttNormal, CellText);
  AssertEquals('name column', 'File', UTF8Encode(CellText));

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirstChild(Tree.GetFirst), 2, ttNormal, CellText);
  AssertEquals('parent column', 'Cat', UTF8Encode(CellText));

  CellText := '';
  Tree.OnGetText(Tree, Tree.GetFirstChild(Tree.GetFirst), 3, ttNormal, CellText);
  AssertEquals('path column', '/bin/sh', UTF8Encode(CellText));
end;

procedure TTestEventsAutorun.TestGetTextAutorunCategoryPathEmpty;
var
  CellText: string;
begin
  AddCategory('Cat');
  CellText := 'keep me';
  Tree.OnGetText(Tree, Tree.GetFirst, 3, ttNormal, CellText);
  AssertEquals('categories have no path', '', UTF8Encode(CellText));
end;

procedure TTestEventsAutorun.TestGetTextAutorunMiddleColumnEmpty;
var
  CellText: string;
begin
  AddFile('File', '/bin/sh');
  CellText := 'keep me';
  Tree.OnGetText(Tree, Tree.GetFirst, 1, ttNormal, CellText);
  AssertEquals('column one stays empty', '', UTF8Encode(CellText));
end;

{ TTestEventsSearch }

procedure TTestEventsSearch.SetUp;
begin
  inherited SetUp;
  FEvents.SetupVSTSearch(Tree);
  NoAutoSort(Tree);
end;

procedure TTestEventsSearch.TestCompareSearchByName;
var
  First, Second: PVirtualNode;
  Result: Integer;
begin
  AddFile('Apple', '/bin/a');
  First := Tree.GetFirst;
  AddFile('Zebra', '/bin/z');
  Second := Tree.GetNextSibling(First);
  Result := 0;
  Tree.OnCompareNodes(Tree, First, Second, 0, Result);
  AssertTrue('names compare alphabetically', Result < 0);
end;

procedure TTestEventsSearch.TestCompareSearchByCategory;
var
  First, Second: PVirtualNode;
  Result: Integer;
begin
  AddCategory('CatB');
  AddFile('Same', '/bin/a', Tree.GetFirst);
  First := Tree.GetFirstChild(Tree.GetFirst);
  AddCategory('CatA');
  AddFile('Same', '/bin/b', Tree.GetNextSibling(Tree.GetFirst));
  Second := Tree.GetFirstChild(Tree.GetNextSibling(Tree.GetFirst));

  Result := 0;
  Tree.OnCompareNodes(Tree, First, Second, 1, Result);
  AssertTrue('categories compare alphabetically', Result > 0);
end;

procedure TTestEventsSearch.TestNodeDataSizeSearch;
var
  Size: Integer;
begin
  Size := 0;
  Tree.OnGetNodeDataSize(Tree, Size);
  AssertEquals('search nodes hold rTreeDataX', SizeOf(rTreeDataX), Size);
end;

{ TTestEventsFrames }

procedure TTestEventsFrames.SetUp;
begin
  inherited SetUp;
  FFrameTree := TVirtualStringTree.Create(nil);
  FFrameTree.NodeDataSize := SizeOf(rFramesNodeData);
  FEvents.SetupVSTDialogFrame(FFrameTree);
  NoAutoSort(FFrameTree);
end;

procedure TTestEventsFrames.TearDown;
begin
  FFrameTree.Free;
  inherited TearDown;
end;

procedure TTestEventsFrames.TestGetTextFrameDefault;
var
  CellText: string;
begin
  FFrameTree.AddChild(nil);
  CellText := '';
  FFrameTree.OnGetText(FFrameTree, FFrameTree.GetFirst, 0, ttNormal, CellText);
  AssertEquals('nameless pages show a placeholder', 'Options Page',
    UTF8Encode(CellText));
end;

procedure TTestEventsFrames.TestGetTextFrameCustom;
var
  CellText: string;
  Node: PVirtualNode;
begin
  Node := FFrameTree.AddChild(nil);
  PFramesNodeData(FFrameTree.GetNodeData(Node))^.Title := 'Custom';
  CellText := '';
  FFrameTree.OnGetText(FFrameTree, Node, 0, ttNormal, CellText);
  AssertEquals('titled pages show their title', 'Custom', UTF8Encode(CellText));
end;

procedure TTestEventsFrames.TestGetImageIndexFrame;
var
  ImageIndex: TImageIndex;
  Ghosted: Boolean;
  Node: PVirtualNode;
begin
  Node := FFrameTree.AddChild(nil);
  PFramesNodeData(FFrameTree.GetNodeData(Node))^.ImageIndex := 5;
  ImageIndex := -1;
  Ghosted := False;
  FFrameTree.OnGetImageIndex(FFrameTree, Node, ikNormal, 0, Ghosted, ImageIndex);
  AssertEquals('frame icon read back', 5, ImageIndex);
end;

procedure TTestEventsFrames.TestGetImageIndexFrameOtherKind;
var
  ImageIndex: TImageIndex;
  Ghosted: Boolean;
  Node: PVirtualNode;
begin
  Node := FFrameTree.AddChild(nil);
  ImageIndex := 7;
  Ghosted := False;
  FFrameTree.OnGetImageIndex(FFrameTree, Node, ikState, 0, Ghosted, ImageIndex);
  AssertEquals('state icons untouched', 7, ImageIndex);
end;

procedure TTestEventsFrames.TestNodeDataSizeFrame;
var
  Size: Integer;
begin
  Size := 0;
  FFrameTree.OnGetNodeDataSize(FFrameTree, Size);
  AssertEquals('frame nodes hold rFramesNodeData', SizeOf(rFramesNodeData), Size);
end;

procedure TTestEventsFrames.TestFreeNodeFrameClearsTitle;
var
  Node: PVirtualNode;
begin
  Node := FFrameTree.AddChild(nil);
  PFramesNodeData(FFrameTree.GetNodeData(Node))^.Title := 'Custom';
  FFrameTree.OnFreeNode(FFrameTree, Node);
  AssertEquals('title released',
    '', UTF8Encode(PFramesNodeData(FFrameTree.GetNodeData(Node))^.Title));
end;

procedure TTestEventsFrames.TestAddToSelectionOutsideDialogIsSafe;
var
  Frm: TForm;
  Pnl: TPanel;
  Node: PVirtualNode;
begin
  // The tree is parented two levels deep in a plain form, not in a
  // TfrmDialogBase: selecting a page must be a safe no-op
  Frm := TForm.CreateNew(nil);
  try
    Pnl := TPanel.Create(Frm);
    Pnl.Parent := Frm;
    FFrameTree.Parent := Pnl;
    try
      Node := FFrameTree.AddChild(nil);
      PFramesNodeData(FFrameTree.GetNodeData(Node))^.Title := 'Custom';
      FFrameTree.OnAddToSelection(FFrameTree, Node);
    finally
      FFrameTree.Parent := nil;
    end;
  finally
    Frm.Free;
  end;
end;

{ TTestEventsSaveLoad }

procedure TTestEventsSaveLoad.SetUp;
begin
  inherited SetUp;
  FEvents.SetupVSTList(Tree);
  FTarget := TVirtualStringTree.Create(nil);
  FTarget.NodeDataSize := SizeOf(rBaseData);
  // Same wiring for OnLoadNode/OnSaveNode, but freeing goes through a
  // direct cast: the production OnFreeNode would resolve FTarget against
  // MainTree and read its rBaseData memory as an rTreeDataX link
  FEvents.SetupVSTList(FTarget);
  FTarget.OnFreeNode := @FreeTargetNode;
  // Loading fires SetText per node: the production OnNewText would resolve
  // the half-loaded nodes against MainTree and refresh the whole list
  // (which needs the running tray menu), so the load target stays silent
  FTarget.OnNewText := nil;
  NoAutoSort(Tree);
  NoAutoSort(FTarget);
end;

procedure TTestEventsSaveLoad.TearDown;
begin
  FTarget.Free;
  inherited TearDown;
end;

procedure TTestEventsSaveLoad.FreeTargetNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PBaseData;
begin
  Data := PBaseData(Sender.GetNodeData(Node));
  if Assigned(Data) and Assigned(Data^.Data) then
    FreeAndNil(Data^.Data);
end;

procedure TTestEventsSaveLoad.BuildSource;
begin
  AddCategory('Cat');
  AddFile('File', '/bin/sh', Tree.GetFirst);
  AddSeparator('---');
end;

procedure TTestEventsSaveLoad.StreamToTarget;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Tree.SaveToStream(Stream);
    Stream.Position := 0;
    FTarget.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TTestEventsSaveLoad.TargetData(ANode: PVirtualNode): TvBaseNodeData;
var
  Data: PBaseData;
begin
  // Direct cast on purpose: GetNodeItemData would resolve FTarget nodes
  // against MainTree and misread them as subtree links
  Data := PBaseData(FTarget.GetNodeData(ANode));
  if Assigned(Data) then
    Result := Data^.Data
  else
    Result := nil;
end;

procedure TTestEventsSaveLoad.TestRoundTripPreservesStructure;
var
  CatNode, FileNode, SepNode: PVirtualNode;
begin
  BuildSource;
  StreamToTarget;

  AssertEquals('two root nodes', 2, FTarget.RootNodeCount);
  CatNode := FTarget.GetFirst;
  SepNode := FTarget.GetNextSibling(CatNode);
  AssertNotNull('category loaded', CatNode);
  AssertNotNull('separator loaded', SepNode);
  AssertTrue('category data type survives',
    TargetData(CatNode).DataType = vtdtCategory);
  AssertTrue('separator data type survives',
    TargetData(SepNode).DataType = vtdtSeparator);

  FileNode := FTarget.GetFirstChild(CatNode);
  AssertNotNull('child lands under the category', FileNode);
  AssertTrue('file data type survives',
    TargetData(FileNode).DataType = vtdtFile);
end;

procedure TTestEventsSaveLoad.TestRoundTripCopiesFields;
var
  FileNode: PVirtualNode;
  Data: TvFileNodeData;
begin
  BuildSource;
  StreamToTarget;

  FileNode := FTarget.GetFirstChild(FTarget.GetFirst);
  Data := TvFileNodeData(TargetData(FileNode));
  // Copies are renamed with the copy prefix, like clipboard copies
  AssertEquals('name copied with prefix', 'Copy_File', UTF8Encode(Data.Name));
  AssertEquals('path copied', '/bin/sh', UTF8Encode(Data.PathFile));
end;

procedure TTestEventsSaveLoad.TestRoundTripResetsHotkey;
var
  Loaded: TvBaseNodeData;
begin
  BuildSource;
  TvFileNodeData(FindNodeDataByName('File')).Hotkey :=
    TextToShortCut('Ctrl+R');
  StreamToTarget;

  Loaded := TargetData(FTarget.GetFirstChild(FTarget.GetFirst));
  AssertNotNull('file loaded', Loaded);
  AssertEquals('loaded nodes never reuse the hotkey', 0,
    TvCustomRealNodeData(Loaded).Hotkey);
end;

procedure TTestEventsSaveLoad.TestRoundTripCreatesDistinctInstances;
var
  Source, Loaded: TvBaseNodeData;
  LoadedNode: PVirtualNode;
begin
  BuildSource;
  StreamToTarget;

  Source := FindNodeDataByName('File');
  LoadedNode := FTarget.GetFirstChild(FTarget.GetFirst);
  Loaded := TargetData(LoadedNode);
  AssertNotNull('source data', Source);
  AssertNotNull('file loaded', Loaded);
  AssertTrue('loaded data is a new instance', Loaded <> Source);
  AssertTrue('loaded data points at its own node',
    Loaded.PNode = LoadedNode);
end;

{ TTestEventsMisc }

procedure TTestEventsMisc.SetUp;
begin
  inherited SetUp;
  FEvents.SetupVSTSearch(Tree);
  FEvents.SetupVSTGraphicMenu(Tree, nil);
  NoAutoSort(Tree);
end;

procedure TTestEventsMisc.TestColumnResizeNilIsSafe;
var
  OldNameWidth: Integer;
begin
  // A search tree without columns must not touch Config: the handler guards
  // the column count
  OldNameWidth := Config.SearchNameColWidth;
  Tree.OnColumnResize(nil, 0);
  AssertEquals('column widths untouched', OldNameWidth,
    Config.SearchNameColWidth);
end;

procedure TTestEventsMisc.TestResizeBigNodes;
begin
  Tree.DefaultNodeHeight := ASuiteInstance.BigHeightNode;
  Tree.OnResize(Tree);
  AssertEquals('big nodes leave no bottom space', 1, Tree.BottomSpace);
end;

procedure TTestEventsMisc.TestResizeSmallNodes;
var
  Aligned: Integer;
begin
  Tree.DefaultNodeHeight := 40;
  Tree.OnResize(Tree);
  Aligned := Round(Tree.OffsetY / 40) * 40;
  AssertEquals('offset stays aligned', Aligned, Tree.OffsetY);
end;

procedure TTestEventsMisc.TestScrollAlignsOffset;
begin
  Tree.DefaultNodeHeight := 40;
  Tree.OnScroll(Tree, 0, 5);
  AssertEquals('scroll keeps the offset aligned', 0, Tree.OffsetY);
  Tree.OnScroll(Tree, 0, 0);
  AssertEquals('zero scroll is a no-op', 0, Tree.OffsetY);
end;

initialization
  RegisterTest(TTestEventsListText);
  RegisterTest(TTestEventsHotkey);
  RegisterTest(TTestEventsAutorun);
  RegisterTest(TTestEventsSearch);
  RegisterTest(TTestEventsFrames);
  RegisterTest(TTestEventsSaveLoad);
  RegisterTest(TTestEventsMisc);

end.
