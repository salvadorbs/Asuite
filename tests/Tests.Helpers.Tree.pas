unit Tests.Helpers.Tree;

{$mode objfpc}{$H+}

{ Shared base fixture for the ASuite VirtualTree test suites.

  It wires a TVirtualStringTree the way the running application does, minus
  everything that would talk to the desktop or write outside a temp folder:

  * the tree is registered as ASuiteInstance.MainTree:
    TVirtualTreeMethods.GetNodeDataEx only resolves nodes of the main/import
    tree, a foreign tree would return nil and crash in AddChildNodeEx;
  * node data is owned by its node (rBaseData.Data) and freed in OnFreeNode,
    the way the private TVirtualTreeEvents.DoFreeNode does it;
  * Config values touched by the fixture (Backup, HotKey, ASuiteState) are
    snapshotted and restored, and TASuiteLogger.ShowDialogs is off: a logged
    exception raises a modal MessageDlg nobody can answer, which would hang
    the run;
  * every singleton list of TListManager is drained in teardown, so node
    data freed with the tree never dangles in a list. The hotkey list is
    drained with RemoveItem (which skips the desktop unregistration when
    Config.HotKey is off) instead of Clear (which unregisters
    unconditionally).

  This fixture intentionally leaves Backup, HotKey and ASuiteState alone:
  suites that need non-default values set and restore them around each test
  (the database suites force lsImporting in their own SetUp, which is what
  Forms.ImportList does around its own Load: the node setters
  Autorun/SchMode/Hotkey/NoMRU/ClickCount/LastAccess would otherwise
  register the node in the singleton lists).

  Quirk encoded here: TvCustomRealNodeData.SetSchDateTime(0) replaces 0 with
  Now, so every fixture node is given a real (whole second) scheduler
  datetime, otherwise a save/load round trip could not compare equal. }

interface

uses
  Classes, SysUtils, TypInfo, fpcunit,
  VirtualTrees, VirtualTrees.Types,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  NodeDataTypes.Category, NodeDataTypes.Separator,
  VirtualTree.Methods, Kernel.Types, Kernel.Enumerations, Icons.Node,
  Kernel.Logger, Kernel.Manager, Kernel.Instance, AppConfig.Main;

type
  TNodeTreeTestCase = class(TTestCase)
  private
    FTree: TVirtualStringTree;
    FOldBackup, FOldHotKey, FOldDialogs: Boolean;
    FOldBackupNumber: Integer;
    FOldState: TLauncherState;
    procedure FreeNodeData(Sender: TBaseVirtualTree; Node: PVirtualNode);
  protected
    // Must be idempotent: SetUp calls it back when it fails halfway, and
    // FPCUnit does not run TearDown after a failed SetUp.
    procedure ReleaseTree;
    procedure SetUp; override;
    procedure TearDown; override;

    { node builders - always create a node of the requested type and fill in
      the fields the database layer persists. Names and paths arrive as UTF-8
      (test literals are AnsiString) and are decoded explicitly: the node
      properties are UnicodeString, and an implicit conversion would depend on
      DefaultSystemCodePage }
    function AddCategory(const AName: string;
      AParent: PVirtualNode = nil): TvCategoryNodeData;
    function AddFile(const AName, APath: string;
      AParent: PVirtualNode = nil): TvFileNodeData;
    function AddSeparator(const AName: string;
      AParent: PVirtualNode = nil): TvSeparatorNodeData;

    procedure ClearTree;

    { Creates an empty file in the temp folder and returns its full path.
      Files created this way must be deleted by the caller. }
    function WriteTempFile(const AName: string): string;

    // Canonical multi line dump of every field the database layer persists:
    // comparing two dumps shows the first differing node and field
    function DescribeTree(ATree: TVirtualStringTree): string;
    // First node whose Name matches, in tree order (nil when absent)
    function FindNodeByName(const AName: string): PVirtualNode;
    function FindNodeDataByName(const AName: string): TvBaseNodeData;
  public
    property Tree: TVirtualStringTree read FTree;
  end;

const
  // Fixed unix timestamp used for node dates: makes DescribeTree dumps
  // deterministic and free of clock boundaries
  SAMPLE_UNIX_DATE = 1760000000;

implementation

function DefaultSchDateTime: TDateTime;
begin
  // Whole second on purpose: any TDateTime storage precision round-trips it
  Result := EncodeDate(2026, 10, 14) + EncodeTime(9, 30, 0, 0);
end;

{ TNodeTreeTestCase }

procedure TNodeTreeTestCase.FreeNodeData(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PBaseData;
begin
  Data := PBaseData(Sender.GetNodeData(Node));
  if Assigned(Data) and Assigned(Data^.Data) then
    FreeAndNil(Data^.Data);
end;

procedure TNodeTreeTestCase.ReleaseTree;
begin
  // No singleton list may keep a pointer to node data freed with the tree
  if Assigned(ASuiteManager) then
  begin
    ASuiteManager.ListManager.MRUList.Clear;
    ASuiteManager.ListManager.MFUList.Clear;
    ASuiteManager.ListManager.SchedulerItemList.Clear;
    ASuiteManager.ListManager.StartupItemList.Clear;
    ASuiteManager.ListManager.ShutdownItemList.Clear;
    while ASuiteManager.ListManager.HotKeyItemList.Count > 0 do
      ASuiteManager.ListManager.HotKeyItemList.RemoveItem(
        ASuiteManager.ListManager.HotKeyItemList[0]);
  end;

  FreeAndNil(FTree);
  // Baseline of the whole test suite: no main tree is registered
  ASuiteInstance.MainTree := nil;

  Config.Backup := FOldBackup;
  Config.BackupNumber := FOldBackupNumber;
  Config.HotKey := FOldHotKey;
  Config.ASuiteState := FOldState;
  TASuiteLogger.ShowDialogs := FOldDialogs;
end;

procedure TNodeTreeTestCase.SetUp;
begin
  inherited SetUp;

  FOldDialogs := TASuiteLogger.ShowDialogs;
  TASuiteLogger.ShowDialogs := False;
  FOldBackup := Config.Backup;
  FOldBackupNumber := Config.BackupNumber;
  FOldHotKey := Config.HotKey;
  FOldState := Config.ASuiteState;

  try
    FTree := TVirtualStringTree.Create(nil);
    FTree.NodeDataSize := SizeOf(rBaseData);
    FTree.OnFreeNode := @FreeNodeData;
    ASuiteInstance.MainTree := FTree;
  except
    ReleaseTree;
    raise;
  end;
end;

procedure TNodeTreeTestCase.TearDown;
begin
  try
    ReleaseTree;
  finally
    inherited TearDown;
  end;
end;

function TNodeTreeTestCase.AddCategory(const AName: string;
  AParent: PVirtualNode): TvCategoryNodeData;
var
  Node: PVirtualNode;
begin
  Node := TVirtualTreeMethods.AddChildNodeEx(Tree, AParent, amInsertAfter,
    vtdtCategory, False);
  Result := TvCategoryNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
  Result.Name := UTF8Decode(AName);
  // SetSchDateTime(0) would store Now instead of "no date"
  Result.SchDateTime := DefaultSchDateTime;
  Result.UnixAddDate := SAMPLE_UNIX_DATE;
  Result.UnixEditDate := SAMPLE_UNIX_DATE;
end;

function TNodeTreeTestCase.AddFile(const AName, APath: string;
  AParent: PVirtualNode): TvFileNodeData;
var
  Node: PVirtualNode;
begin
  Node := TVirtualTreeMethods.AddChildNodeEx(Tree, AParent, amInsertAfter,
    vtdtFile, False);
  Result := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
  Result.Name := UTF8Decode(AName);
  Result.PathFile := UTF8Decode(APath);
  Result.SchDateTime := DefaultSchDateTime;
  Result.UnixAddDate := SAMPLE_UNIX_DATE;
  Result.UnixEditDate := SAMPLE_UNIX_DATE;
end;

function TNodeTreeTestCase.AddSeparator(const AName: string;
  AParent: PVirtualNode): TvSeparatorNodeData;
var
  Node: PVirtualNode;
begin
  Node := TVirtualTreeMethods.AddChildNodeEx(Tree, AParent, amInsertAfter,
    vtdtSeparator, False);
  Result := TvSeparatorNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Tree));
  Result.Name := UTF8Decode(AName);
  // TSQLtbl_list.LoadDataFromNode skips the date fields of separator items,
  // so a stored separator row always reads back 0/0
  Result.UnixAddDate := 0;
  Result.UnixEditDate := 0;
end;

procedure TNodeTreeTestCase.ClearTree;
begin
  Tree.BeginUpdate;
  try
    Tree.Clear;
  finally
    Tree.EndUpdate;
  end;
end;

function TNodeTreeTestCase.WriteTempFile(const AName: string): string;
var
  FS: TFileStream;
begin
  Result := GetTempDir(False) + AName;
  FS := TFileStream.Create(Result, fmCreate);
  FS.Free;
end;

function TNodeTreeTestCase.DescribeTree(ATree: TVirtualStringTree): string;
var
  Lines: TStringList;

  function EnumName(AValue: Integer; ATypeInfo: PTypeInfo): string;
  begin
    Result := GetEnumName(ATypeInfo, AValue);
  end;

  procedure Walk(AParent: PVirtualNode; ADepth: Integer);
  var
    Node: PVirtualNode;
    Data: TvBaseNodeData;
    RealData: TvCustomRealNodeData;
    FileData: TvFileNodeData;
    Line: string;
  begin
    if AParent = nil then
      Node := ATree.RootNode^.FirstChild
    else
      Node := AParent^.FirstChild;
    while Assigned(Node) do
    begin
      Data := TVirtualTreeMethods.GetNodeItemData(Node, ATree);
      Line := StringOfChar(' ', ADepth * 2);
      if Data = nil then
        Line := Line + '<nil node data>'
      else begin
        // NB: node data strings are UnicodeString ({$MODE DelphiUnicode} in
        // NodeDataTypes.*); UTF8Encode keeps the snapshot ASCII-safe and
        // deterministic, per the README idiom (UTF8Encode for assertions).
        Line := Line + Format('%s "%s"',
          [EnumName(Ord(Data.DataType), TypeInfo(TvTreeDataType)),
           UTF8Encode(Data.Name)]);
        Line := Line + Format(
          ' id=%d parent=%d pos=%d desc="%s" add=%d edit=%d hide=%s',
          [Data.ID, Data.ParentID, Data.Position, UTF8Encode(Data.Description),
           Data.UnixAddDate, Data.UnixEditDate,
           BoolToStr(Data.HideFromMenu, True)]);

        if not Data.IsSeparatorItem then
        begin
          RealData := TvCustomRealNodeData(Data);
          Line := Line + Format(
            ' icon="%s" crc=%d autorun=%s autorunpos=%d onlaunch=%s ' +
            'schmode=%s schdate=%s windowstate=%d hotkey=%d',
            [UTF8Encode(RealData.PathIcon), TNodeIcon(RealData.Icon).CacheIconCRC,
             EnumName(Ord(RealData.Autorun), TypeInfo(TAutorunType)),
             RealData.AutorunPos,
             EnumName(Ord(RealData.ActionOnExe), TypeInfo(TActionOnExecute)),
             EnumName(Ord(RealData.SchMode), TypeInfo(TSchedulerMode)),
             FormatDateTime('yyyy-mm-dd hh:nn:ss', RealData.SchDateTime),
             RealData.WindowState, RealData.Hotkey]);
        end;

        if Data.IsFileItem then
        begin
          FileData := TvFileNodeData(Data);
          Line := Line + Format(
            ' path="%s" work="%s" params="%s" dsk=%s nomru=%s nomfu=%s ' +
            'lastaccess=%d clicks=%d runfromcat=%s env="%s"',
            [UTF8Encode(FileData.PathFile), UTF8Encode(FileData.WorkingDir),
             UTF8Encode(FileData.Parameters),
             BoolToStr(FileData.ShortcutDesktop, True),
             BoolToStr(FileData.NoMRU, True),
             BoolToStr(FileData.NoMFU, True),
             FileData.LastAccess, FileData.ClickCount,
             BoolToStr(FileData.RunFromCategory, True),
             FileData.EnvironmentVars.Text]);
        end;
      end;
      Lines.Add(Line);

      if Assigned(Data) and Data.IsCategoryItem then
        Walk(Node, ADepth + 1);
      Node := Node^.NextSibling;
    end;
  end;

begin
  Lines := TStringList.Create;
  try
    Walk(nil, 0);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TNodeTreeTestCase.FindNodeByName(const AName: string): PVirtualNode;

  procedure Walk(AParent: PVirtualNode);
  var
    Node: PVirtualNode;
  begin
    if AParent = nil then
      Node := Tree.GetFirst
    else
      Node := Tree.GetFirstChild(AParent);
    while Assigned(Node) do
    begin
      if TVirtualTreeMethods.GetNodeItemData(Node, Tree).Name = UTF8Decode(AName) then
      begin
        Result := Node;
        Exit;
      end;
      Walk(Node);
      Node := Tree.GetNextSibling(Node);
    end;
  end;

begin
  Result := nil;
  Walk(nil);
end;

function TNodeTreeTestCase.FindNodeDataByName(const AName: string): TvBaseNodeData;
var
  Node: PVirtualNode;
begin
  Node := FindNodeByName(AName);
  if Assigned(Node) then
    Result := TVirtualTreeMethods.GetNodeItemData(Node, Tree)
  else
    Result := nil;
end;

initialization
  Randomize;

end.
