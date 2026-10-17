unit Tests.Execute;

{$mode objfpc}{$H+}

{ Behaviour tests for real execution paths (NodeDataTypes.Files,
  Lists.Manager, Utility.System).

  The "true" executable exits immediately, so launching it has zero side
  effects: /bin/true on Unix and %SystemRoot%\System32\whoami.exe on
  Windows. A background sleep(30) provides a running process for the
  single-instance branch (Unix only). Missing-file failures are exercised
  too: the test runner sets ShowMessageDialogs := False, so the modal error
  that ExecWithShell would show on Windows cannot block the run. Anything
  else that prompts (pkexec/admin/user), opens documents in external apps
  (ExplorePath) or needs a display server dialog is deliberately excluded. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  process,
  NodeDataTypes.Files,
  Utility.System,
  Kernel.Enumerations, Kernel.Manager, Lists.Manager, Lists.Special,
  AppConfig.Main,
  Tests.Helpers.Tree;

type
  TTestExecute = class(TNodeTreeTestCase)
  published
    procedure TestExecuteLaunchesTrue;
    procedure TestExecuteWithActionOnExe;
    procedure TestExecuteMissingSkipsAfterExecute;
    procedure TestSingleInstanceSkipsRunning;
    procedure TestExecuteAutorunListRunsStartup;
    procedure TestExecuteAutorunListShutdownRespectsFlag;
    procedure TestRunFileTrue;
    procedure TestRunFileMissing;
  end;

implementation

{ A harmless executable that exits immediately on every platform. On Windows
  whoami.exe is always present and does not open a window or wait for input. }
function TrueExecutable: string;
{$IFDEF MSWINDOWS}
var
  Root: string;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Root := GetEnvironmentVariable('SystemRoot');
  if Root = '' then
    Root := 'C:\Windows';
  Result := IncludeTrailingPathDelimiter(Root) + 'System32' + PathDelim +
    'whoami.exe';
  {$ELSE}
  Result := '/bin/true';
  {$ENDIF}
end;

{ A path that certainly does not exist, inside the writable temp folder. }
function MissingExecutable: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'asuite_test_missing_xyz.tmp';
  DeleteFile(Result);
end;

procedure TTestExecute.TestExecuteLaunchesTrue;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', TrueExecutable);
  AssertTrue('launch succeeds', Node.Execute(False, False, False));
  AssertEquals('click counted', 1, Node.ClickCount);
  AssertTrue('node marked changed', Node.Changed);
  AssertEquals('mfu registered', 1,
    ASuiteManager.ListManager.MFUList.Count);
  AssertEquals('mru registered', 1,
    ASuiteManager.ListManager.MRUList.Count);
end;

procedure TTestExecute.TestExecuteWithActionOnExe;
var
  Node: TvFileNodeData;
begin
  // aeDefault with the default config resolves to just-run: no windows move
  Node := AddFile('Tool', TrueExecutable);
  AssertTrue('launch succeeds', Node.Execute(True, False, False));
  AssertTrue('post action ran', Node.Changed);
end;

procedure TTestExecute.TestExecuteMissingSkipsAfterExecute;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('Missing', MissingExecutable);
  AssertFalse('launch fails', Node.Execute(False, False, False));
  AssertFalse('nothing recorded', Node.Changed);
  AssertEquals('no clicks', 0, Node.ClickCount);
end;

procedure TTestExecute.TestSingleInstanceSkipsRunning;
{$IFDEF UNIX}
var
  Sleeper: TProcess;
  Node: TvFileNodeData;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Sleeper := TProcess.Create(nil);
  try
    Sleeper.Executable := '/bin/sleep';
    Sleeper.Parameters.Add('30');
    Sleeper.Options := [];
    Sleeper.Execute;
    Sleep(1000);

    Node := AddFile('Sleeper', '/bin/sleep');
    Node.Autorun := atSingleInstance;
    AssertFalse('running instance skips launch',
      Node.Execute(False, False, True));
    AssertEquals('no clicks recorded', 0, Node.ClickCount);
  finally
    Sleeper.Terminate(0);
    Sleeper.WaitOnExit;
    Sleeper.Free;
  end;
  {$ELSE}
  // Process probing is covered on the unix runners (see above)
  AssertTrue('documented skip', True);
  {$ENDIF}
end;

procedure TTestExecute.TestExecuteAutorunListRunsStartup;
var
  First, Second: TvFileNodeData;
begin
  First := AddFile('First', TrueExecutable);
  First.Autorun := atAlwaysOnStart;
  Second := AddFile('Second', TrueExecutable);
  Second.Autorun := atAlwaysOnStart;

  ASuiteManager.ListManager.ExecuteAutorunList(amStartup);

  AssertEquals('first ran', 1, First.ClickCount);
  AssertEquals('second ran', 1, Second.ClickCount);
end;

procedure TTestExecute.TestExecuteAutorunListShutdownRespectsFlag;
var
  Node: TvFileNodeData;
  OldFlag: Boolean;
begin
  OldFlag := Config.AutorunShutdown;
  Config.AutorunShutdown := False;
  try
    Node := AddFile('Tool', TrueExecutable);
    Node.Autorun := atAlwaysOnClose;
    ASuiteManager.ListManager.ExecuteAutorunList(amShutdown);
    AssertEquals('disabled list never runs', 0, Node.ClickCount);
  finally
    Config.AutorunShutdown := OldFlag;
  end;
end;

procedure TTestExecute.TestRunFileTrue;
begin
  AssertTrue('true executable runs', RunFile(TrueExecutable));
end;

procedure TTestExecute.TestRunFileMissing;
begin
  // On Unix this falls through to OpenDocument, which fails fast on missing
  // paths; on Windows ExecWithShell fails and only logs (dialogs suppressed).
  AssertFalse('missing file fails', RunFile(MissingExecutable));
end;

initialization
  RegisterTest(TTestExecute);

end.
