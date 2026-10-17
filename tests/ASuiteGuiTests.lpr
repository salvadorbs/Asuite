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
  Tests.NodeDataTypes,
  Tests.NodeDataTypesFiles,
  Tests.VirtualTreeMethods,
  Tests.VirtualTreeEvents,
  Tests.VirtualTreeHelper,
  Tests.Lists,
  Tests.SystemUtils,
  Tests.MiscUtils,
  Tests.RunController,
  Tests.ClipboardController,
  Tests.SearchController,
  Tests.XmlImport,
  Tests.Configuration,
  Tests.ShellLink,
  Tests.FindFiles,
  Tests.ASMenuItem,
  Tests.Frames,
  Tests.ImportList,
  Tests.TrayMenu,
  Tests.Execute,
  Tests.Settings,
  Tests.Instance,
  Utility.Misc,
  Tests.Helpers.Tree,
  Tests.Helpers.Database,
  Tests.DatabaseVersion,
  Tests.DatabaseList,
  Tests.DatabaseManager;

{$R *.res}

var
  Application: TTestRunner;
begin
  // Failure paths must never open a modal dialog: on Windows ExecWithShell
  // reports a missing launcher that way and the test would block forever.
  ShowMessageDialogs := False;

  Application := TTestRunner.Create(nil);
  try
    Application.Run;
  finally
    Application.Free;
  end;
end.
