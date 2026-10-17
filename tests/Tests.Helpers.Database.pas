unit Tests.Helpers.Database;

{$mode objfpc}{$H+}

{ Shared fixture for the ASuite SQLite/mORMot test suites.

  It wires the database layer the way the running application does, minus
  everything that would talk to the desktop or write outside a temp folder:

  * the system under test is the singleton ASuiteManager.DBManager, because
    TVirtualTreeMethods.BeforeDeleteNode (run by TDBManager.DeleteItems)
    deletes rows through ASuiteManager.DBManager, never through a local
    instance;
  * every test gets its own temporary sqlite file; TDBManager.Setup closes the
    database opened by a previous Setup, and TearDown moves the singleton to
    ':memory:' first, so the file can be deleted on every platform;
  * the TVirtualStringTree is registered as ASuiteInstance.MainTree:
    TVirtualTreeMethods.GetNodeDataEx only resolves nodes of the main/import
    tree, a foreign tree would return nil and crash in AddChildNodeEx;
  * node data is owned by its node (rBaseData.Data) and freed in OnFreeNode,
    the way the private TVirtualTreeEvents.DoFreeNode does it;
  * Config.ASuiteState is lsImporting for the whole test: the node setters
    (Autorun/SchMode/Hotkey/NoMRU/ClickCount/LastAccess) register the node in
    the singleton lists of TListManager, and those pointers must not outlive
    the tree. This is what Forms.ImportList does around its own Load;
  * Config.HotKey is off (registering a shortcut talks to the desktop portal)
    and Config.Backup is off (DoBackupList would copy the database outside
    the temp folder);
  * TASuiteLogger.ShowDialogs is off: a logged exception raises a modal
    MessageDlg nobody can answer, which would hang the run under Xvfb.

  Quirk encoded here: TvCustomRealNodeData.SetSchDateTime(0) replaces 0 with
  Now, so every fixture node is given a real (whole second) scheduler
  datetime, otherwise a save/load round trip could not compare equal. }

interface

uses
  Classes, SysUtils, TypInfo, fpcunit,
  VirtualTrees, VirtualTrees.Types,
  mormot.core.base, mormot.orm.core,
  Database.Manager,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  NodeDataTypes.Category, NodeDataTypes.Separator,
  VirtualTree.Methods, Kernel.Types, Kernel.Enumerations, Icons.Node,
  Kernel.Logger, Kernel.Manager, Kernel.Instance, AppConfig.Main;

type
  TDatabaseTestCase = class(TTestCase)
  private
    FTree: TVirtualStringTree;
    FTempFile: string;
    FDBAttached: Boolean;
    FOldBackup, FOldHotKey, FOldDialogs: Boolean;
    FOldBackupNumber: Integer;
    FOldState: TLauncherState;
    procedure FreeNodeData(Sender: TBaseVirtualTree; Node: PVirtualNode);
    function GetDB: TDBManager;
    function NewTempFile(const APrefix: string): string;
  protected
    // Must be idempotent: SetUp calls it back when it fails halfway, and
    // FPCUnit does not run TearDown after a failed SetUp.
    procedure ReleaseFixture;
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
    // ClearTree + TDBManager.LoadData: what TASuiteInstance.LoadList does
    procedure ReloadTree;

    function CountRows(AClass: TOrmClass): Integer;
    function CountRowsWhere(AClass: TOrmClass; const AWhere: RawUtf8;
      const AParams: array of const): Integer;
    // Canonical multi line dump of every field the database layer persists:
    // comparing two dumps shows the first differing node and field
    function DescribeTree(ATree: TVirtualStringTree): string;
    // First node whose Name matches, in tree order (nil when absent)
    function FindNodeByName(const AName: string): PVirtualNode;
    function FindNodeDataByName(const AName: string): TvBaseNodeData;
    // TvBaseNodeData.SetChanged always stamps FEditDate := Now, even when
    // assigning False: TSQLtbl_list.LoadDataFromNode writes lastModified before
    // resetting the flag, so after a save the in-memory edit date diverges
    // from the row. Re-stamp the build-time value so a post-save snapshot
    // matches the reloaded tree field for field. Separator rows store no
    // dates at all (LoadDataFromNode skips their date fields), hence 0.
    procedure RestoreEditDates;
  public
    property DB: TDBManager read GetDB;
    property Tree: TVirtualStringTree read FTree;
    property TempFile: string read FTempFile;
  end;

const
  // TDBManager.Setup() on this name opens an in-memory database and, thanks to
  // Setup being idempotent, closes the handle held on the previous temp file
  MEMORY_DB_FILE = ':memory:';
  // Fixed unix timestamp used for node dates: makes DescribeTree dumps
  // deterministic and free of clock boundaries
  SAMPLE_UNIX_DATE = 1760000000;

implementation

function DefaultSchDateTime: TDateTime;
begin
  // Whole second on purpose: any TDateTime storage precision round-trips it
  Result := EncodeDate(2026, 10, 14) + EncodeTime(9, 30, 0, 0);
end;

{ TDatabaseTestCase }

function TDatabaseTestCase.GetDB: TDBManager;
begin
  Result := ASuiteManager.DBManager;
end;

function TDatabaseTestCase.NewTempFile(const APrefix: string): string;
begin
  Result := GetTempDir(False) +
    Format('asuite_tests_%s_%s_%d.sqlite',
      [APrefix, FormatDateTime('yyyymmddhhnnsszzz', Now), Random(MaxInt)]);
end;

procedure TDatabaseTestCase.FreeNodeData(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PBaseData;
begin
  Data := PBaseData(Sender.GetNodeData(Node));
  if Assigned(Data) and Assigned(Data^.Data) then
    FreeAndNil(Data^.Data);
end;

procedure TDatabaseTestCase.ReleaseFixture;
begin
  if FDBAttached then
  begin
    FDBAttached := False;
    // Close the handle on the test database before deleting its file
    DB.Setup(MEMORY_DB_FILE);
  end;
  if FTempFile <> '' then
  begin
    DeleteFile(FTempFile);
    DeleteFile(FTempFile + '-journal');
    DeleteFile(FTempFile + '-wal');
    DeleteFile(FTempFile + '-shm');
    FTempFile := '';
  end;

  // No singleton list may keep a pointer to node data freed with the tree
  if Assigned(ASuiteManager) then
  begin
    ASuiteManager.ListManager.MRUList.Clear;
    ASuiteManager.ListManager.MFUList.Clear;
    ASuiteManager.ListManager.SchedulerItemList.Clear;
    ASuiteManager.ListManager.StartupItemList.Clear;
    ASuiteManager.ListManager.ShutdownItemList.Clear;
    ASuiteManager.ListManager.HotKeyItemList.Clear;
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

procedure TDatabaseTestCase.SetUp;
begin
  inherited SetUp;

  FOldDialogs := TASuiteLogger.ShowDialogs;
  TASuiteLogger.ShowDialogs := False;
  FOldBackup := Config.Backup;
  FOldBackupNumber := Config.BackupNumber;
  FOldHotKey := Config.HotKey;
  FOldState := Config.ASuiteState;
  Config.Backup := False;
  Config.HotKey := False;
  Config.ASuiteState := lsImporting;

  try
    FTempFile := NewTempFile('db');
    DB.Setup(FTempFile);
    FDBAttached := True;

    FTree := TVirtualStringTree.Create(nil);
    FTree.NodeDataSize := SizeOf(rBaseData);
    FTree.OnFreeNode := @FreeNodeData;
    ASuiteInstance.MainTree := FTree;
  except
    ReleaseFixture;
    raise;
  end;
end;

procedure TDatabaseTestCase.TearDown;
begin
  try
    ReleaseFixture;
  finally
    inherited TearDown;
  end;
end;

function TDatabaseTestCase.AddCategory(const AName: string;
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

function TDatabaseTestCase.AddFile(const AName, APath: string;
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

function TDatabaseTestCase.AddSeparator(const AName: string;
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

procedure TDatabaseTestCase.ClearTree;
begin
  Tree.BeginUpdate;
  try
    Tree.Clear;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TDatabaseTestCase.ReloadTree;
begin
  ClearTree;
  DB.LoadData(Tree);
end;

function TDatabaseTestCase.CountRows(AClass: TOrmClass): Integer;
begin
  Result := CountRowsWhere(AClass, '', []);
end;

function TDatabaseTestCase.CountRowsWhere(AClass: TOrmClass;
  const AWhere: RawUtf8; const AParams: array of const): Integer;
var
  Rec: TOrm;
begin
  Result := 0;
  if AWhere = '' then
    Rec := AClass.CreateAndFillPrepare(DB.Database.Orm, '')
  else
    Rec := AClass.CreateAndFillPrepare(DB.Database.Orm, AWhere, AParams);
  try
    while Rec.FillOne do
      Inc(Result);
  finally
    Rec.Free;
  end;
end;

function TDatabaseTestCase.DescribeTree(ATree: TVirtualStringTree): string;
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

function TDatabaseTestCase.FindNodeByName(const AName: string): PVirtualNode;

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

function TDatabaseTestCase.FindNodeDataByName(const AName: string): TvBaseNodeData;
var
  Node: PVirtualNode;
begin
  Node := FindNodeByName(AName);
  if Assigned(Node) then
    Result := TVirtualTreeMethods.GetNodeItemData(Node, Tree)
  else
    Result := nil;
end;

procedure TDatabaseTestCase.RestoreEditDates;

  procedure Walk(AParent: PVirtualNode);
  var
    Node: PVirtualNode;
    Data: TvBaseNodeData;
  begin
    if AParent = nil then
      Node := Tree.GetFirst
    else
      Node := Tree.GetFirstChild(AParent);
    while Assigned(Node) do
    begin
      Data := TVirtualTreeMethods.GetNodeItemData(Node, Tree);
      if Assigned(Data) then
      begin
        if Data.IsSeparatorItem then
          Data.UnixEditDate := 0
        else
          Data.UnixEditDate := SAMPLE_UNIX_DATE;
      end;
      if Assigned(Data) and Data.IsCategoryItem then
        Walk(Node);
      Node := Tree.GetNextSibling(Node);
    end;
  end;

begin
  Walk(nil);
end;

initialization
  Randomize;

end.
