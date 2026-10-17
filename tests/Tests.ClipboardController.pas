unit Tests.ClipboardController;

{$mode objfpc}{$H+}

{ Behaviour tests for TClipboardController (Utility.ClipboardController).

  Copy/cut only mark nodes (through the Fake* helpers on non-Windows) and
  UpdatePasteEnabled only inspects the cut/copy set, so all of them run
  headless on the shared tree fixture. DoPaste is deliberately excluded: on
  success it saves the whole list through RefreshList, which needs the
  running tray menu. EmptyClipboard is excluded too: it clears the real
  system clipboard. Pasting itself is covered in Tests.VirtualTreeHelper. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  Utility.ClipboardController,
  Tests.Helpers.Tree;

type
  TTestClipboardController = class(TNodeTreeTestCase)
  published
    procedure TestNilTreeCopyIsSafe;
    procedure TestNilTreeCutIsSafe;
    procedure TestNilTreePasteReturnsFalse;
    procedure TestDoCopyMarksCopyPending;
    procedure TestDoCutMarksCutPending;
    procedure TestUpdatePasteEnabledWithoutSelection;
    procedure TestUpdatePasteEnabledAfterCopy;
    procedure TestUpdatePasteEnabledInactiveTree;
  end;

implementation

procedure TTestClipboardController.TestNilTreeCopyIsSafe;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(nil);
  try
    // Must not raise
    Controller.DoCopy;
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestNilTreeCutIsSafe;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(nil);
  try
    // Must not raise
    Controller.DoCut;
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestNilTreePasteReturnsFalse;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(nil);
  try
    AssertFalse('nil tree pastes nothing', Controller.DoPaste);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestDoCopyMarksCopyPending;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.DoCopy;
    AssertTrue('copy pending set', tsCopyPending in Tree.TreeStates);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestDoCutMarksCutPending;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.DoCut;
    AssertTrue('cut pending set', tsCutPending in Tree.TreeStates);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestUpdatePasteEnabledWithoutSelection;
var
  Controller: TClipboardController;
  Enabled: Boolean;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    Enabled := True;
    Controller.UpdatePasteEnabled(Enabled, True);
    AssertFalse('nothing pending disables paste', Enabled);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestUpdatePasteEnabledAfterCopy;
var
  Controller: TClipboardController;
  Enabled: Boolean;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.DoCopy;
    Enabled := False;
    Controller.UpdatePasteEnabled(Enabled, True);
    AssertTrue('pending copy enables paste', Enabled);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardController.TestUpdatePasteEnabledInactiveTree;
var
  Controller: TClipboardController;
  Enabled: Boolean;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.DoCopy;
    Enabled := True;
    Controller.UpdatePasteEnabled(Enabled, False);
    AssertFalse('inactive tree disables paste', Enabled);
  finally
    Controller.Free;
  end;
end;

initialization
  RegisterTest(TTestClipboardController);

end.
