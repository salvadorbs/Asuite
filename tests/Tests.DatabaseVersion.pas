unit Tests.DatabaseVersion;

{$mode objfpc}{$H+}

{ Round-trip tests for TSQLtbl_version through the singleton TDBManager.

  The spike (Tests.DatabaseSpike) proved the layer is testable with a local
  TDBManager; these tests cover the branches the spike could not reach:
  the Update path of TSQLtbl_version.Save (a second Save must not insert a
  second row) and the empty-table guard of TSQLtbl_version.Load. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Kernel.Instance, FileInfo, Database.Version,
  Tests.Helpers.Database;

type
  TTestDatabaseVersion = class(TDatabaseTestCase)
  published
    procedure TestVersionSaveInsertsSingleRow;
    procedure TestVersionSaveTwiceUpdatesSameRow;
    procedure TestVersionLoadOnEmptyTableKeepsVersion;
    procedure TestVersionLoadRestoresPersistedValues;
  end;

implementation

procedure TTestDatabaseVersion.TestVersionSaveInsertsSingleRow;
var
  Expected: TProgramVersion;
begin
  Expected := TASuiteInstance.GetASuiteVersion;

  TSQLtbl_version.Save(DB);

  AssertEquals('one version row after first Save', 1, CountRows(TSQLtbl_version));
  AssertEquals('DBVersion.Major after Save', Expected.Major, DB.DBVersion.Major);
  AssertEquals('DBVersion.Minor after Save', Expected.Minor, DB.DBVersion.Minor);
  AssertEquals('DBVersion.Revision after Save', Expected.Revision, DB.DBVersion.Revision);
  AssertEquals('DBVersion.Build after Save', Expected.Build, DB.DBVersion.Build);
end;

procedure TTestDatabaseVersion.TestVersionSaveTwiceUpdatesSameRow;
var
  Expected: TProgramVersion;
begin
  Expected := TASuiteInstance.GetASuiteVersion;

  TSQLtbl_version.Save(DB);
  TSQLtbl_version.Save(DB);

  // The second Save must take the Update branch (IsDataExists), not Add
  AssertEquals('Save twice keeps a single row', 1, CountRows(TSQLtbl_version));
  AssertEquals('DBVersion still correct after second Save',
    Expected.Build, DB.DBVersion.Build);
end;

procedure TTestDatabaseVersion.TestVersionLoadOnEmptyTableKeepsVersion;
var
  Zeroed: TProgramVersion;
begin
  FillChar(Zeroed, SizeOf(Zeroed), 0);
  DB.DBVersion := Zeroed;

  // Load is a no-op when tbl_version has no rows
  TSQLtbl_version.Load(DB);

  AssertEquals('Load on empty table keeps DBVersion zeroed', 0, DB.DBVersion.Major);
end;

procedure TTestDatabaseVersion.TestVersionLoadRestoresPersistedValues;
var
  Expected: TProgramVersion;
  Zeroed: TProgramVersion;
begin
  Expected := TASuiteInstance.GetASuiteVersion;

  TSQLtbl_version.Save(DB);

  // Forget the in-memory value, then reload it from disk
  FillChar(Zeroed, SizeOf(Zeroed), 0);
  DB.DBVersion := Zeroed;
  AssertEquals('DBVersion cleared', 0, DB.DBVersion.Major);

  TSQLtbl_version.Load(DB);

  AssertEquals('DBVersion.Major restored by Load', Expected.Major, DB.DBVersion.Major);
  AssertEquals('DBVersion.Minor restored by Load', Expected.Minor, DB.DBVersion.Minor);
  AssertEquals('DBVersion.Revision restored by Load', Expected.Revision, DB.DBVersion.Revision);
  AssertEquals('DBVersion.Build restored by Load', Expected.Build, DB.DBVersion.Build);
end;

initialization
  RegisterTest(TTestDatabaseVersion);

end.
