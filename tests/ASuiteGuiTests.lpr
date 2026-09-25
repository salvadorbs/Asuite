{ ASuite GUI tests (FPCUnit console runner + LCL).

  These tests build real LCL controls and need a display server on Linux
  (run under xvfb-run, or with DISPLAY set).

  Build: lazbuild --widgetset=<gtk2|gtk3|qt5|qt6> ASuiteGuiTests.lpi
  Run:   ./ASuiteGuiTests --all --format=plain   (exit code <> 0 on failure)
}
program ASuiteGuiTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  consoletestrunner,
  Tests.MenuUtils,
  Tests.SortController;

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
