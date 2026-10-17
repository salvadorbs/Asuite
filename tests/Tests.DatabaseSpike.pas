unit Tests.DatabaseSpike;

{$mode objfpc}{$H+}

{ Fase 0 (spike) della suite SQLite/mORMot: valida che il layer database sia
  davvero testabile dalla suite GUI prima di investire nei test completi.

  Copre: leggibilità della risorsa versione del binario di test (serve a
  TSQLtbl_version.Save, che altrimenti raise-erebbe dentro un try/except che
  mostra una dialog modale), TDBManager.Setup su file temporaneo, creazione
  delle tabelle, CRUD di base su tbl_version e round-trip della versione. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Database.Manager, Database.Version, Database.List, AppConfig.Main,
  Kernel.Instance, FileInfo, mormot.core.base;

type
  TTestDatabaseSpike = class(TTestCase)
  private
    FDB: TDBManager;
    FTempFile: string;
    FOldBackup: Boolean;
    function NewTempDBFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAppVersionIsReadable;
    procedure TestSetupCreatesFileAndTables;
    procedure TestAddRetrieveDeleteRecord;
    procedure TestVersionSaveLoadRoundTrip;
  end;

implementation

function TTestDatabaseSpike.NewTempDBFile: string;
begin
  // Unique per run: tests must never share (or depend on) a database file
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('asuite_spike_%s_%d.sqlite',
      [FormatDateTime('yyyymmddhhnnsszzz', Now), Random(MaxInt)]);
end;

procedure TTestDatabaseSpike.SetUp;
begin
  inherited SetUp;
  // DoBackupList copies the database into Paths.SuitePathBackup when enabled:
  // keep every write inside the temp folder
  FOldBackup := Config.Backup;
  Config.Backup := False;

  FTempFile := NewTempDBFile;
  FDB := TDBManager.Create;
  FDB.Setup(FTempFile);
end;

procedure TTestDatabaseSpike.TearDown;
begin
  FDB.Free;
  FDB := nil;
  DeleteFile(FTempFile);
  DeleteFile(FTempFile + '-journal');
  DeleteFile(FTempFile + '-wal');
  DeleteFile(FTempFile + '-shm');
  Config.Backup := FOldBackup;
  inherited TearDown;
end;

procedure TTestDatabaseSpike.TestAppVersionIsReadable;
var
  Version: TProgramVersion;
begin
  // TSQLtbl_version.Save calls this inside a try/except that logs through
  // TASuiteLogger.Exception (modal dialog): it must not raise here.
  Version := TASuiteInstance.GetASuiteVersion;
  AssertEquals('Major', 2, Version.Major);
  AssertEquals('Minor', 1, Version.Minor);
end;

procedure TTestDatabaseSpike.TestSetupCreatesFileAndTables;
begin
  AssertEquals('DB file name stored', FTempFile, FDB.DBFileName);
  AssertTrue('sqlite file created on disk', FileExists(FTempFile));
  AssertNotNull('rest server assigned', FDB.Database);
  AssertFalse('tbl_version starts empty', FDB.Database.TableHasRows(TSQLtbl_version));
  AssertFalse('tbl_list starts empty', FDB.Database.TableHasRows(TSQLtbl_list));
end;

procedure TTestDatabaseSpike.TestAddRetrieveDeleteRecord;
var
  Rec, Loaded: TSQLtbl_version;
  ID: TID;
begin
  Rec := TSQLtbl_version.Create;
  try
    Rec.Major := 9;
    Rec.Minor := 8;
    Rec.Revision := 7;
    Rec.Build := 6;
    ID := FDB.Database.Add(Rec, True);
  finally
    Rec.Free;
  end;
  AssertTrue('Add returns a valid ID', ID > 0);
  AssertTrue('tbl_version has a row now', FDB.Database.TableHasRows(TSQLtbl_version));

  Loaded := TSQLtbl_version.CreateAndFillPrepare(FDB.Database.Orm, 'id=?', [ID]);
  try
    AssertTrue('record retrieved by id', Loaded.FillOne);
    AssertEquals('Major', 9, Loaded.Major);
    AssertEquals('Minor', 8, Loaded.Minor);
    AssertEquals('Revision', 7, Loaded.Revision);
    AssertEquals('Build', 6, Loaded.Build);
    AssertFalse('only one row', Loaded.FillOne);
  finally
    Loaded.Free;
  end;

  AssertTrue('delete by id', FDB.Database.Delete(TSQLtbl_version, ID));
  AssertFalse('tbl_version empty again', FDB.Database.TableHasRows(TSQLtbl_version));
end;

procedure TTestDatabaseSpike.TestVersionSaveLoadRoundTrip;
var
  Expected, Zeroed: TProgramVersion;
  Rec: TSQLtbl_version;
begin
  Expected := TASuiteInstance.GetASuiteVersion;

  TSQLtbl_version.Save(FDB);
  AssertEquals('DBVersion.Major after Save', Expected.Major, FDB.DBVersion.Major);
  AssertEquals('DBVersion.Build after Save', Expected.Build, FDB.DBVersion.Build);

  Rec := TSQLtbl_version.CreateAndFillPrepare(FDB.Database.Orm, '');
  try
    AssertTrue('a version row exists', Rec.FillOne);
    AssertEquals('persisted Major', Expected.Major, Rec.Major);
    AssertEquals('persisted Minor', Expected.Minor, Rec.Minor);
    AssertEquals('persisted Revision', Expected.Revision, Rec.Revision);
    AssertEquals('persisted Build', Expected.Build, Rec.Build);
    AssertFalse('still a single row', Rec.FillOne);
  finally
    Rec.Free;
  end;

  // Forget the in-memory value, then reload it from disk
  FillChar(Zeroed, SizeOf(Zeroed), 0);
  FDB.DBVersion := Zeroed;
  AssertEquals('DBVersion cleared', 0, FDB.DBVersion.Major);

  TSQLtbl_version.Load(FDB);
  AssertEquals('DBVersion.Major after Load', Expected.Major, FDB.DBVersion.Major);
  AssertEquals('DBVersion.Minor after Load', Expected.Minor, FDB.DBVersion.Minor);
  AssertEquals('DBVersion.Revision after Load', Expected.Revision, FDB.DBVersion.Revision);
  AssertEquals('DBVersion.Build after Load', Expected.Build, FDB.DBVersion.Build);
end;

initialization
  RegisterTest(TTestDatabaseSpike);

end.
