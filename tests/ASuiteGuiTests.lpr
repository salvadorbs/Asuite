{ ASuite GUI tests (FPCUnit console runner + LCL).

  These tests build real LCL controls and need a display server on Linux
  (run under xvfb-run, or with DISPLAY set).

  Build: lazbuild --widgetset=<gtk2|gtk3|qt5|qt6> ASuiteGuiTests.lpi
  Run:   ./ASuiteGuiTests --all --format=plain   (exit code <> 0 on failure)
}
program ASuiteGuiTests;

{$mode objfpc}{$H+}

uses
  heaptrc,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  // Static SQLite3 engine, exactly like ASuite.lpr: without it every
  // TSQLRestServerDB raises "No SQLite3 libray available"
  mormot.db.raw.sqlite3.static,
  consoletestrunner,
  Tests.MenuUtils,
  Tests.SortController,
  Tests.LanguageCombo,
  Tests.ConfigPaths,
  Tests.Conversions,
  Tests.FileFolder,
  Tests.Scheduler,
  Tests.IconsManager,
  Tests.IconsBaseLogic,
  Tests.Helpers.Database,
  Tests.DatabaseVersion,
  Tests.DatabaseList,
  Tests.DatabaseManager,
  Tests.DatabaseSpike;

{$R *.res}

var
  Application: TTestRunner;
begin
  Application := TTestRunner.Create(nil);
  try
    Application.Run;
  finally
    Application.Free;
  end;
end.
