{ ASuite automated tests (FPCUnit console runner).

  Build: lazbuild ASuiteTests.lpi
  Run:   ./ASuiteTests --all --format=plain   (exit code <> 0 on failure)
}
program ASuiteTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  consoletestrunner,
  Tests.AppConfigObserver;

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
