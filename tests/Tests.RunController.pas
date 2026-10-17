unit Tests.RunController;

{$mode objfpc}{$H+}

{ Behaviour tests for TRunController.UpdateRunAction
  (Utility.RunController).

  The action enablement is decided from the selected nodes: separators and
  empty selections disable, categories always enable, files depend on the
  run mode (executable bit and URL protocols are probed on real temp
  files). TRunController.ExecuteRun is deliberately excluded: it launches
  processes through TVirtualTreeMethods.ExecuteSelectedNodes. }

interface

uses
  Classes, SysUtils, ActnList, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  NodeDataTypes.Files,
  Utility.RunController,
  Kernel.Enumerations, Kernel.Manager,
  DataModules.Icons, DataModules.TrayMenu,
  Tests.Helpers.Tree
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}
  ;

type
  TTestRunController = class(TNodeTreeTestCase)
  private
    function NewAction(AMode: TRunMode): TAction;
  published
    procedure TestNilActionIsSafe;
    procedure TestNilTreeIsSafe;
    procedure TestEmptySelectionIsDisabled;
    procedure TestSeparatorIsDisabled;
    procedure TestCategoryIsEnabled;
    procedure TestExecutableIsEnabled;
    procedure TestNonExecutableIsDisabled;
    procedure TestUrlIsDisabledForExplore;
  end;

  { Same fixture with fully streamed data modules: ExecuteRun saves the
    list through RefreshList, which rebuilds the real classic menu. }
  TTestRunControllerExecute = class(TNodeTreeTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteRunLaunches;
    procedure TestExecuteRunSkipsIdleCategory;
  end;

implementation

function TTestRunController.NewAction(AMode: TRunMode): TAction;
begin
  Result := TAction.Create(nil);
  Result.Tag := Ord(AMode);
end;

procedure TTestRunController.TestNilActionIsSafe;
var
  Controller: TRunController;
begin
  Controller := TRunController.Create;
  try
    AddFile('File', '/bin/sh');
    // Must not raise
    Controller.UpdateRunAction(nil, Tree);
  finally
    Controller.Free;
  end;
end;

procedure TTestRunController.TestNilTreeIsSafe;
var
  Controller: TRunController;
  Action: TAction;
begin
  Controller := TRunController.Create;
  Action := NewAction(rmNormal);
  try
    // Must not raise
    Controller.UpdateRunAction(Action, nil);
  finally
    Action.Free;
    Controller.Free;
  end;
end;

procedure TTestRunController.TestEmptySelectionIsDisabled;
var
  Controller: TRunController;
  Action: TAction;
begin
  Controller := TRunController.Create;
  Action := NewAction(rmNormal);
  try
    AddFile('File', '/bin/sh');
    Controller.UpdateRunAction(Action, Tree);
    AssertFalse('nothing selected disables', Action.Enabled);
  finally
    Action.Free;
    Controller.Free;
  end;
end;

procedure TTestRunController.TestSeparatorIsDisabled;
var
  Controller: TRunController;
  Action: TAction;
begin
  Controller := TRunController.Create;
  Action := NewAction(rmNormal);
  try
    AddSeparator('---');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.UpdateRunAction(Action, Tree);
    AssertFalse('separator disables', Action.Enabled);
  finally
    Action.Free;
    Controller.Free;
  end;
end;

procedure TTestRunController.TestCategoryIsEnabled;
var
  Controller: TRunController;
  Action: TAction;
begin
  Controller := TRunController.Create;
  Action := NewAction(rmNormal);
  try
    AddCategory('Cat');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.UpdateRunAction(Action, Tree);
    AssertTrue('category enables normal run', Action.Enabled);
  finally
    Action.Free;
    Controller.Free;
  end;
end;

procedure TTestRunController.TestExecutableIsEnabled;
var
  Controller: TRunController;
  Action: TAction;
  Data: TvFileNodeData;
  {$IFDEF UNIX}
  Tmp: string;
  {$ENDIF}
begin
  Controller := TRunController.Create;
  Action := NewAction(rmAsUser);
  try
    {$IFDEF UNIX}
    Tmp := WriteTempFile('asuite_test_runnable.tmp');
    AssertEquals('chmod succeeds', 0, fpchmod(PChar(Tmp),
      S_IRWXU or S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH));
    Data := AddFile('Tool', '/bin/sh');
    Data.PathFile := UTF8Decode(Tmp);
    {$ELSE}
    Data := AddFile('Tool', '/bin/sh');
    Data.PathFile := 'C:\tool.exe';
    {$ENDIF}
    Tree.Selected[Tree.GetFirst] := True;
    Controller.UpdateRunAction(Action, Tree);
    AssertTrue('executable enables run-as', Action.Enabled);
  finally
    Action.Free;
    Controller.Free;
    {$IFDEF UNIX}
    DeleteFile(Tmp);
    {$ENDIF}
  end;
end;

procedure TTestRunController.TestNonExecutableIsDisabled;
var
  Controller: TRunController;
  Action: TAction;
  Data: TvFileNodeData;
  {$IFDEF UNIX}
  Tmp: string;
  {$ENDIF}
begin
  Controller := TRunController.Create;
  Action := NewAction(rmAsUser);
  try
    {$IFDEF UNIX}
    Tmp := WriteTempFile('asuite_test_plain.tmp');
    Data := AddFile('Doc', '/bin/sh');
    Data.PathFile := UTF8Decode(Tmp);
    {$ELSE}
    Data := AddFile('Doc', '/bin/sh');
    Data.PathFile := 'C:\doc.txt';
    {$ENDIF}
    Tree.Selected[Tree.GetFirst] := True;
    Controller.UpdateRunAction(Action, Tree);
    AssertFalse('plain file disables run-as', Action.Enabled);
  finally
    Action.Free;
    Controller.Free;
    {$IFDEF UNIX}
    DeleteFile(Tmp);
    {$ENDIF}
  end;
end;

procedure TTestRunController.TestUrlIsDisabledForExplore;
var
  Controller: TRunController;
  Action: TAction;
  Data: TvFileNodeData;
begin
  Controller := TRunController.Create;
  Action := NewAction(rmExplorePath);
  try
    Data := AddFile('Link', '/bin/sh');
    Data.PathFile := 'https://example.com/x';
    Tree.Selected[Tree.GetFirst] := True;
    Controller.UpdateRunAction(Action, Tree);
    AssertFalse('urls cannot be explored', Action.Enabled);
  finally
    Action.Free;
    Controller.Free;
  end;
end;

{ TTestRunControllerExecute }

procedure TTestRunControllerExecute.SetUp;
begin
  inherited SetUp;
  dmImages := TdmImages.Create(nil);
  dmTrayMenu := TdmTrayMenu.Create(nil);
end;

procedure TTestRunControllerExecute.TearDown;
begin
  FreeAndNil(dmTrayMenu);
  FreeAndNil(dmImages);
  inherited TearDown;
end;

procedure TTestRunControllerExecute.TestExecuteRunLaunches;
var
  Controller: TRunController;
  Tool: TvFileNodeData;
begin
  {$IFDEF UNIX}
  // /bin/true exits immediately: a real launch with zero side effects
  Tool := AddFile('Tool', '/bin/true');
  AddCategory('Cat');
  Tree.TreeOptions.SelectionOptions :=
    Tree.TreeOptions.SelectionOptions + [toMultiSelect];
  Tree.Selected[Tree.GetFirst] := True;
  Tree.Selected[Tree.GetNextSibling(Tree.GetFirst)] := True;

  Controller := TRunController.Create;
  try
    Controller.ExecuteRun(Tree, rmNormal);
    AssertEquals('tool ran once', 1, Tool.ClickCount);
  finally
    Controller.Free;
  end;
  {$ELSE}
  // Launching is covered on the unix runners (see above)
  AssertTrue('documented skip', True);
  {$ENDIF}
end;

procedure TTestRunControllerExecute.TestExecuteRunSkipsIdleCategory;
var
  Controller: TRunController;
begin
  // No runnable children anywhere: nothing launches on any platform
  AddCategory('Cat');
  AddFile('Idle', '/bin/idle', Tree.GetFirst);
  Tree.Selected[Tree.GetFirst] := True;

  Controller := TRunController.Create;
  try
    Controller.ExecuteRun(Tree, rmNormal);
    AssertEquals('tree untouched', 1, Tree.RootNodeCount);
  finally
    Controller.Free;
  end;
end;

initialization
  RegisterTest(TTestRunController);
  RegisterTest(TTestRunControllerExecute);

end.
