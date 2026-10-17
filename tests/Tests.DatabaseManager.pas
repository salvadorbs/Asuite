unit Tests.DatabaseManager;

{$mode objfpc}{$H+}

{ Behaviour tests for TDBManager itself: idempotent Setup, the SaveData
  result contract, backup creation and pruning through Config, row deletion
  (RemoveItem, DeleteItems) and ClearTable.

  Backup files land in ASuiteInstance.Paths.SuitePathBackup (tests/backup,
  gitignored): the test binary cannot redirect that path, so every test that
  touches the folder cleans up after itself. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  VirtualTrees, Kernel.Instance, Kernel.Consts,
  AppConfig.Main,
  Database.Manager, Database.Version, Database.List,
  NodeDataTypes.Files,
  Tests.Helpers.Database;

type
  TTestDatabaseManager = class(TDatabaseTestCase)
  private
    procedure CollectBackups(var ANames: TStringList);
    procedure DeleteBackups(const ANames: TStringList);
    procedure WriteBackupFile(const AName: string);
  published
    procedure TestSetupIsIdempotent;
    procedure TestSaveDataReturnsTrueAndPersistsRows;
    procedure TestSaveDataCreatesBackupWhenEnabled;
    procedure TestBackupNumberSetterPrunesOldBackups;
    procedure TestRemoveItemDeletesSingleRow;
    procedure TestDeleteItemsRemovesRowsForWholeSubtree;
    procedure TestClearTableEmptiesRequestedTable;
  end;

  { Covers the Setup-never-ran case: a TDBManager whose database handle is
    still nil must fail closed instead of raising an access violation. A
    local instance is used so the singleton database is left untouched. }
  TTestDBManagerWithoutDatabase = class(TTestCase)
  private
    FManager: TDBManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveDataWithoutDatabaseReturnsFalse;
    procedure TestRemoveItemWithoutDatabaseIsSafe;
    procedure TestDeleteItemsWithoutDatabaseReturnsFalse;
  end;

implementation

procedure TTestDatabaseManager.CollectBackups(var ANames: TStringList);
var
  SR: TSearchRec;
begin
  if FindFirst(ASuiteInstance.Paths.SuitePathBackup + APP_NAME + '_*' +
    EXT_SQLBCK, faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Attr and faDirectory) = 0 then
        ANames.Add(SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure TTestDatabaseManager.DeleteBackups(const ANames: TStringList);
var
  I: Integer;
begin
  for I := 0 to ANames.Count - 1 do
    DeleteFile(ASuiteInstance.Paths.SuitePathBackup + ANames[I]);
end;

procedure TTestDatabaseManager.WriteBackupFile(const AName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(ASuiteInstance.Paths.SuitePathBackup + AName,
    fmCreate);
  FS.Free;
end;

procedure TTestDatabaseManager.TestSetupIsIdempotent;
begin
  AddFile('File', '/bin/sh');
  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  AssertEquals('row before second Setup', 1, CountRows(TSQLtbl_list));

  // SQLite opens the file in exclusive mode: a second Setup must close the
  // previous handle first (Fase 1 refactor) or opening would fail
  DB.Setup(TempFile);

  AssertNotNull('database still assigned after second Setup', DB.Database);
  AssertEquals('rows survive a second Setup', 1, CountRows(TSQLtbl_list));
  AssertTrue('database file still exists', FileExists(TempFile));
end;

procedure TTestDatabaseManager.TestSaveDataReturnsTrueAndPersistsRows;
begin
  AddCategory('Cat');
  AddFile('File', '/bin/sh');

  AssertTrue('SaveData returns True on success', DB.SaveData(Tree, False));

  AssertEquals('list rows persisted', 2, CountRows(TSQLtbl_list));
  AssertEquals('version row persisted', 1, CountRows(TSQLtbl_version));
  AssertTrue('database file exists on disk', FileExists(TempFile));
end;

procedure TTestDatabaseManager.TestSaveDataCreatesBackupWhenEnabled;
var
  Names: TStringList;
begin
  AddFile('File', '/bin/sh');

  // Start from an empty backup folder: DeleteOldBackups (BackupNumber = 5)
  // would otherwise prune older files and mask the creation
  Names := TStringList.Create;
  try
    CollectBackups(Names);
    DeleteBackups(Names);

    Config.Backup := True; // the setter also creates the backup folder
    try
      AssertTrue('SaveData with backup succeeds', DB.SaveData(Tree, True));

      CollectBackups(Names);
      AssertTrue('a backup file was created', Names.Count >= 1);
    finally
      Config.Backup := False;
    end;
  finally
    // Clean up every backup this test created
    DeleteBackups(Names);
    Names.Free;
  end;
end;

procedure TTestDatabaseManager.TestBackupNumberSetterPrunesOldBackups;
const
  FAKE_FORMAT = 'ASuite_2020-01-%02d-00-00-00.sqbck';
var
  Names: TStringList;
  I: Integer;
begin
  // Start from an empty backup folder so the assertion is deterministic
  Names := TStringList.Create;
  try
    CollectBackups(Names);
    DeleteBackups(Names);
  finally
    Names.Free;
  end;

  // Five fake backups with increasing timestamps: name order == chronology
  for I := 1 to 5 do
    WriteBackupFile(Format(FAKE_FORMAT, [I]));

  try
    // Lowering BackupNumber prunes immediately through DeleteOldBackups
    Config.BackupNumber := 2;

    Names := TStringList.Create;
    try
      CollectBackups(Names);
      AssertEquals('only the two newest backups survive', 2, Names.Count);
      AssertTrue('newest backup kept',
        Names.IndexOf(Format(FAKE_FORMAT, [5])) >= 0);
      AssertTrue('second newest backup kept',
        Names.IndexOf(Format(FAKE_FORMAT, [4])) >= 0);
    finally
      Names.Free;
    end;
  finally
    for I := 1 to 5 do
      DeleteFile(ASuiteInstance.Paths.SuitePathBackup + Format(FAKE_FORMAT, [I]));
  end;
end;

procedure TTestDatabaseManager.TestRemoveItemDeletesSingleRow;
var
  FileNode: TvFileNodeData;
  ID: Int64;
begin
  AddCategory('Cat');
  FileNode := AddFile('File', '/bin/sh');
  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));

  ID := FileNode.ID;
  AssertTrue('file has an ID', ID > 0);

  DB.RemoveItem(ID);
  AssertEquals('only the target row is deleted', 1, CountRows(TSQLtbl_list));

  DB.RemoveItem(0);
  DB.RemoveItem(-1);
  AssertEquals('non-positive IDs are ignored', 1, CountRows(TSQLtbl_list));
end;

procedure TTestDatabaseManager.TestDeleteItemsRemovesRowsForWholeSubtree;
var
  CatNode: PVirtualNode;
  Nodes: TNodeArray;
begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;
  AddFile('File A', '/bin/a', CatNode);
  AddFile('File B', '/bin/b', CatNode);
  AddFile('Loose', '/bin/loose');

  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));
  AssertEquals('four rows saved', 4, CountRows(TSQLtbl_list));

  // DeleteItems removes the DB rows of the whole subtree (frmMain then
  // removes the nodes themselves); the root file must survive
  SetLength(Nodes, 1);
  Nodes[0] := CatNode;

  AssertTrue('DeleteItems succeeds', DB.DeleteItems(Tree, Nodes));
  AssertEquals('category and its children removed, root file survives',
    1, CountRows(TSQLtbl_list));
end;

procedure TTestDatabaseManager.TestClearTableEmptiesRequestedTable;
begin
  AddFile('File', '/bin/sh');
  AssertTrue('SaveData succeeds', DB.SaveData(Tree, False));

  DB.ClearTable(TSQLtbl_list);

  AssertEquals('tbl_list emptied', 0, CountRows(TSQLtbl_list));
  AssertEquals('tbl_version untouched', 1, CountRows(TSQLtbl_version));
end;

{ TTestDBManagerWithoutDatabase }

procedure TTestDBManagerWithoutDatabase.SetUp;
begin
  inherited SetUp;
  //FDatabase is nil until the first Setup: this is the state under test
  FManager := TDBManager.Create;
end;

procedure TTestDBManagerWithoutDatabase.TearDown;
begin
  FManager.Free;
  inherited TearDown;
end;

procedure TTestDBManagerWithoutDatabase.TestSaveDataWithoutDatabaseReturnsFalse;
begin
  AssertFalse('SaveData without Setup returns False', FManager.SaveData(nil, False));
  //With DoBackup the early exit must also skip DoBackupList entirely
  AssertFalse('SaveData without Setup never backs up', FManager.SaveData(nil, True));
end;

procedure TTestDBManagerWithoutDatabase.TestRemoveItemWithoutDatabaseIsSafe;
begin
  //Must not raise, whatever the ID looks like
  FManager.RemoveItem(42);
  FManager.RemoveItem(0);
  FManager.RemoveItem(-1);
end;

procedure TTestDBManagerWithoutDatabase.TestDeleteItemsWithoutDatabaseReturnsFalse;
begin
  AssertFalse('DeleteItems without Setup returns False',
    FManager.DeleteItems(nil, nil));
end;

initialization
  RegisterTest(TTestDatabaseManager);
  RegisterTest(TTestDBManagerWithoutDatabase);

end.
