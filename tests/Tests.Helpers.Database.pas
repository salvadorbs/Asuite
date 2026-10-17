unit Tests.Helpers.Database;

{$mode objfpc}{$H+}

{ Shared fixture for the ASuite SQLite/mORMot test suites.

  It wires the database layer the way the running application does, minus
  everything that would talk to the desktop or write outside a temp folder.
  The tree side (MainTree registration, node ownership, list cleanup, Config
  snapshot) lives in the base class TNodeTreeTestCase
  (Tests.Helpers.Tree); this unit only adds the database side:

  * the system under test is the singleton ASuiteManager.DBManager, because
    TVirtualTreeMethods.BeforeDeleteNode (run by TDBManager.DeleteItems)
    deletes rows through ASuiteManager.DBManager, never through a local
    instance;
  * every test gets its own temporary sqlite file; TDBManager.Setup closes the
    database opened by a previous Setup, and TearDown moves the singleton to
    ':memory:' first, so the file can be deleted on every platform;
  * Config.ASuiteState is lsImporting for the whole test: the node setters
    (Autorun/SchMode/Hotkey/NoMRU/ClickCount/LastAccess) register the node in
    the singleton lists of TListManager, and those pointers must not outlive
    the tree. This is what Forms.ImportList does around its own Load;
  * Config.HotKey is off (registering a shortcut talks to the desktop portal)
    and Config.Backup is off (DoBackupList would copy the database outside
    the temp folder). }

interface

uses
  Classes, SysUtils, fpcunit,
  VirtualTrees,
  mormot.core.base, mormot.orm.core,
  Database.Manager,
  NodeDataTypes.Base,
  VirtualTree.Methods, Kernel.Enumerations, Kernel.Manager, AppConfig.Main,
  Tests.Helpers.Tree;

type
  TDatabaseTestCase = class(TNodeTreeTestCase)
  private
    FTempFile: string;
    FDBAttached: Boolean;
    function GetDB: TDBManager;
    function NewTempFile(const APrefix: string): string;
  protected
    procedure ReleaseDatabase;
    procedure SetUp; override;
    procedure TearDown; override;

    // ClearTree + TDBManager.LoadData: what TASuiteInstance.LoadList does
    procedure ReloadTree;

    function CountRows(AClass: TOrmClass): Integer;
    function CountRowsWhere(AClass: TOrmClass; const AWhere: RawUtf8;
      const AParams: array of const): Integer;
    // TvBaseNodeData.SetChanged always stamps FEditDate := Now, even when
    // assigning False: TSQLtbl_list.LoadDataFromNode writes lastModified before
    // resetting the flag, so after a save the in-memory edit date diverges
    // from the row. Re-stamp the build-time value so a post-save snapshot
    // matches the reloaded tree field for field. Separator rows store no
    // dates at all (LoadDataFromNode skips their date fields), hence 0.
    procedure RestoreEditDates;
  public
    property DB: TDBManager read GetDB;
    property TempFile: string read FTempFile;
  end;

const
  // TDBManager.Setup() on this name opens an in-memory database and, thanks to
  // Setup being idempotent, closes the handle held on the previous temp file
  MEMORY_DB_FILE = ':memory:';

implementation

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

procedure TDatabaseTestCase.ReleaseDatabase;
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
end;

procedure TDatabaseTestCase.SetUp;
begin
  inherited SetUp;

  Config.Backup := False;
  Config.HotKey := False;
  Config.ASuiteState := lsImporting;

  try
    FTempFile := NewTempFile('db');
    DB.Setup(FTempFile);
    FDBAttached := True;
  except
    ReleaseDatabase;
    ReleaseTree;
    raise;
  end;
end;

procedure TDatabaseTestCase.TearDown;
begin
  ReleaseDatabase;
  inherited TearDown;
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

end.
