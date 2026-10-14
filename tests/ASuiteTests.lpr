{ ASuite automated tests (FPCUnit console runner).

  Build: lazbuild ASuiteTests.lpi
  Run:   ./ASuiteTests --all --format=plain   (exit code <> 0 on failure)
}
program ASuiteTests;

{$mode objfpc}{$H+}

uses
  // NOTE: unit order matters with FPC 3.2.2 heaptrc. This console runner only
  // dumps leaks with cthreads first; the GUI runner (ASuiteGuiTests.lpr)
  // needs the opposite order or it hangs on shutdown. Do not "normalize".
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  heaptrc,
  consoletestrunner,
  Tests.AppConfigObserver,
  Tests.RunActionLogic,
  Tests.LanguageManager,
  Tests.IconsCacheKey,
  Tests.IconsMime;

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
