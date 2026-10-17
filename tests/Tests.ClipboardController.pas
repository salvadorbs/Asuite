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
  DataModules.Icons, DataModules.TrayMenu,
  Tests.Helpers.Tree;

type
  TTestClipboardController = class(TNodeTreeTestCase)
  published
    procedure TestNilTreeCopyCutIsSafe;
    procedure TestNilTreePasteReturnsFalse;
    procedure TestUpdatePasteEnabledWithoutSelection;
    procedure TestUpdatePasteEnabledAfterCopy;
    procedure TestUpdatePasteEnabledInactiveTree;
  end;

  { Same fixture with fully streamed data modules: DoPaste saves the list
    through RefreshList, which rebuilds the real classic menu. }
  TTestClipboardPaste = class(TNodeTreeTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDoPasteAfterFile;
    procedure TestDoPasteUnderCategory;
    procedure TestDoPasteWithoutSelection;
  end;

implementation

procedure TTestClipboardController.TestNilTreeCopyCutIsSafe;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(nil);
  try
    // Must not raise
    Controller.DoCopy;
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

{ TTestClipboardPaste }

procedure TTestClipboardPaste.SetUp;
begin
  inherited SetUp;
  // Streamed modules: DoPaste saves and rebuilds the classic menu
  dmImages := TdmImages.Create(nil);
  dmTrayMenu := TdmTrayMenu.Create(nil);
end;

procedure TTestClipboardPaste.TearDown;
begin
  FreeAndNil(dmTrayMenu);
  FreeAndNil(dmImages);
  inherited TearDown;
end;

procedure TTestClipboardPaste.TestDoPasteAfterFile;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.DoCopy;
    // First-selected is a file: paste mode is insert-after
    AssertTrue('paste succeeds', Controller.DoPaste);
    AssertTrue('insert mode used',
      Tree.DefaultPasteMode = amInsertAfter);
    AssertEquals('sibling added', 2, Tree.RootNodeCount);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardPaste.TestDoPasteUnderCategory;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddCategory('Cat');
    Tree.Selected[Tree.GetFirst] := True;
    Controller.DoCopy;
    // First-selected is a category: paste mode is add-child; pasting at
    // the root duplicates it (pasting into itself is skipped)
    Tree.FocusedNode := nil;
    AssertTrue('paste succeeds', Controller.DoPaste);
    AssertTrue('child mode used',
      Tree.DefaultPasteMode = amAddChildLast);
    AssertEquals('category duplicated', 2, Tree.RootNodeCount);
  finally
    Controller.Free;
  end;
end;

procedure TTestClipboardPaste.TestDoPasteWithoutSelection;
var
  Controller: TClipboardController;
begin
  Controller := TClipboardController.Create(Tree);
  try
    AddFile('File', '/bin/sh');
    AssertTrue('vacuous paste succeeds', Controller.DoPaste);
    AssertTrue('mode falls back to add-child',
      Tree.DefaultPasteMode = amAddChildLast);
    AssertEquals('nothing added', 1, Tree.RootNodeCount);
  finally
    Controller.Free;
  end;
end;

initialization
  RegisterTest(TTestClipboardController);
  RegisterTest(TTestClipboardPaste);

end.
