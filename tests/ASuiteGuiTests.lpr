{ ASuite GUI tests (FPCUnit console runner + LCL).

  These tests build real LCL controls and need a display server on Linux
  (run under xvfb-run, or with DISPLAY set).

  Build: lazbuild --widgetset=<gtk2|gtk3|qt5|qt6> ASuiteGuiTests.lpi
  Run:   ./ASuiteGuiTests --all --format=plain   (exit code <> 0 on failure)
}
program ASuiteGuiTests;

{$mode objfpc}{$H+}

uses
  // NOTE: unit order matters with FPC 3.2.2 heaptrc. This GUI runner hangs on
  // shutdown with cthreads first; the console runner (ASuiteTests.lpr) needs
  // the opposite order or it prints no dump. Do not "normalize".
  heaptrc,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  consoletestrunner,
  Tests.MenuUtils,
  Tests.SortController,
  Tests.LanguageCombo,
  Tests.ConfigPaths,
  Tests.Conversions,
  Tests.FileFolder,
  Tests.Scheduler,
  Tests.IconsManager,
  Tests.IconsBaseLogic;

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
