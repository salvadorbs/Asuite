unit Tests.Execute;

{$mode objfpc}{$H+}

{ Behaviour tests for real execution paths (NodeDataTypes.Files,
  Lists.Manager, Utility.System).

  /bin/true exits immediately, so launching it has zero side effects; a
  background sleep(30) provides a running process for the single-instance
  branch (Unix only). Anything that prompts (pkexec/admin/user), opens
  documents in external apps (ExplorePath) or needs a display server dialog
  is deliberately excluded. }

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

procedure TTestExecute.TestExecuteLaunchesTrue;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/true');
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
  Node := AddFile('Tool', '/bin/true');
  AssertTrue('launch succeeds', Node.Execute(True, False, False));
  AssertTrue('post action ran', Node.Changed);
end;

procedure TTestExecute.TestExecuteMissingSkipsAfterExecute;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('Missing', '/definitely/missing/xyz.tmp');
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
  First := AddFile('First', '/bin/true');
  First.Autorun := atAlwaysOnStart;
  Second := AddFile('Second', '/bin/true');
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
    Node := AddFile('Tool', '/bin/true');
    Node.Autorun := atAlwaysOnClose;
    ASuiteManager.ListManager.ExecuteAutorunList(amShutdown);
    AssertEquals('disabled list never runs', 0, Node.ClickCount);
  finally
    Config.AutorunShutdown := OldFlag;
  end;
end;

procedure TTestExecute.TestRunFileTrue;
begin
  AssertTrue('/bin/true runs', RunFile('/bin/true'));
end;

procedure TTestExecute.TestRunFileMissing;
begin
  // Falls through to OpenDocument, which fails fast on missing paths
  AssertFalse('missing file fails', RunFile('/definitely/missing/xyz.tmp'));
end;

initialization
  RegisterTest(TTestExecute);

end.
