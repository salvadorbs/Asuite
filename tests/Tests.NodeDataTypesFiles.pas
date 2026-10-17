unit Tests.NodeDataTypesFiles;

{$mode objfpc}{$H+}

{ Behaviour tests for TvFileNodeData (NodeDataTypes.Files).

  Only side-effect-free behaviour is covered: executing a file node would
  launch processes (InternalExecute/ExplorePath) and managing the desktop
  shortcut writes outside the test folders, so those paths are deliberately
  excluded. List registration (MRU/MFU) runs on the shared tree fixture,
  which drains every list in teardown. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  VirtualTree.Methods,
  Kernel.Enumerations, Kernel.Consts, Kernel.Manager, Kernel.Instance,
  Lists.Manager, Lists.Special,
  AppConfig.Main,
  Tests.Helpers.Tree;

type
  TTestFileNodeData = class(TNodeTreeTestCase)
  published
    procedure TestFileDefaults;
    procedure TestFolderCreationUsesFolderIcon;
    procedure TestPathAbsoluteFileEmpty;
    procedure TestPathAbsoluteFileKeepsAbsolutePaths;
    procedure TestWorkingDirAbsoluteFallsBackToFileDir;
    procedure TestWorkingDirAbsoluteFallsBackToDirectoryItself;
    procedure TestWorkingDirAbsoluteExplicit;
    procedure TestSetPathFileUpdatesExistence;
    procedure TestCopyDuplicatesFileFields;
    procedure TestClickCountRegistersMFU;
    procedure TestLastAccessRegistersMRU;
    procedure TestCountersAreIdleWhenImporting;
    procedure TestSetShortcutDesktopFalseToFalseIsNoOp;
    procedure TestDeleteShortcutFileWhenNotOnDesktopIsNoOp;
  end;

implementation

procedure TTestFileNodeData.TestFileDefaults;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  AssertEquals('no path by default after overwrite', '/bin/sh', UTF8Encode(Node.PathFile));
  AssertEquals('no parameters by default', '', UTF8Encode(Node.Parameters));
  AssertEquals('no working dir by default', '', UTF8Encode(Node.WorkingDir));
  AssertEquals('no clicks by default', 0, Node.ClickCount);
  AssertFalse('in MRU by default', Node.NoMRU);
  AssertFalse('in MFU by default', Node.NoMFU);
  AssertFalse('no desktop shortcut by default', Node.ShortcutDesktop);
  AssertFalse('not run from category by default', Node.RunFromCategory);
  AssertNotNull('environment list always assigned', Node.EnvironmentVars);
  AssertEquals('environment list starts empty', 0, Node.EnvironmentVars.Count);
  AssertTrue('path assumed to exist until checked', Node.IsPathFileExists);
end;

procedure TTestFileNodeData.TestFolderCreationUsesFolderIcon;
var
  Data: TvBaseNodeData;
begin
  Data := TVirtualTreeMethods.CreateNodeData(vtdtFolder);
  try
    AssertTrue('folder creation yields a file node',
      Data is TvFileNodeData);
    AssertEquals('folder nodes get the folder icon', CONST_PATH_FOLDERICON,
      UTF8Encode(TvCustomRealNodeData(Data).PathIcon));
    // Characterization: the constructor always passes vtdtFile up, so even
    // folder nodes report the file data type
    AssertTrue('folder nodes report the file data type',
      Data.DataType = vtdtFile);
  finally
    Data.Free;
  end;
end;

procedure TTestFileNodeData.TestPathAbsoluteFileEmpty;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  Node.PathFile := '';
  AssertEquals('empty path stays empty', '', UTF8Encode(Node.PathAbsoluteFile));
end;

procedure TTestFileNodeData.TestPathAbsoluteFileKeepsAbsolutePaths;
var
  Node: TvFileNodeData;
  Tmp: string;
begin
  Tmp := WriteTempFile('asuite_test_abs.tmp');
  try
    Node := AddFile('File', '/bin/sh');
    Node.PathFile := UTF8Decode(Tmp);
    AssertEquals('absolute paths pass through', Tmp,
      UTF8Encode(Node.PathAbsoluteFile));
  finally
    DeleteFile(Tmp);
  end;
end;

procedure TTestFileNodeData.TestWorkingDirAbsoluteFallsBackToFileDir;
var
  Node: TvFileNodeData;
  Tmp: string;
begin
  Tmp := WriteTempFile('asuite_test_workdir.tmp');
  try
    Node := AddFile('File', '/bin/sh');
    Node.PathFile := UTF8Decode(Tmp);
    AssertEquals('working dir falls back to the file folder',
      ExtractFileDir(Tmp), UTF8Encode(Node.WorkingDirAbsolute));
  finally
    DeleteFile(Tmp);
  end;
end;

procedure TTestFileNodeData.TestWorkingDirAbsoluteFallsBackToDirectoryItself;
var
  Node: TvFileNodeData;
  Dir: string;
begin
  Dir := GetTempDir(False);
  Node := AddFile('File', '/bin/sh');
  Node.PathFile := UTF8Decode(Dir);
  AssertEquals('a directory path is its own working dir', Dir,
    UTF8Encode(Node.WorkingDirAbsolute));
end;

procedure TTestFileNodeData.TestWorkingDirAbsoluteExplicit;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  Node.WorkingDir := 'somewhere';
  AssertEquals('explicit working dir is converted',
    UTF8Encode(ASuiteInstance.Paths.RelativeToAbsolute('somewhere')),
    UTF8Encode(Node.WorkingDirAbsolute));
end;

procedure TTestFileNodeData.TestSetPathFileUpdatesExistence;
var
  Node: TvFileNodeData;
  Tmp: string;
begin
  Tmp := WriteTempFile('asuite_test_exists.tmp');
  try
    Node := AddFile('File', '/bin/sh');
    Node.PathFile := UTF8Decode(Tmp);
    AssertTrue('existing path flagged as existing', Node.IsPathFileExists);

    Node.PathFile := UTF8Decode(Tmp + '.missing');
    AssertFalse('missing path flagged as missing', Node.IsPathFileExists);
  finally
    DeleteFile(Tmp);
  end;
end;

procedure TTestFileNodeData.TestCopyDuplicatesFileFields;
var
  Source, Dest: TvFileNodeData;
begin
  Source := AddFile('Source', '/bin/src');
  Source.Parameters := '--flag';
  Source.WorkingDir := 'work';
  Source.RunFromCategory := True;
  Source.EnvironmentVars.Add('A=1');

  Dest := AddFile('Dest', '/bin/dest');
  Dest.Copy(Source);

  AssertEquals('PathFile copied', '/bin/src', UTF8Encode(Dest.PathFile));
  AssertEquals('Parameters copied', '--flag', UTF8Encode(Dest.Parameters));
  AssertEquals('WorkingDir copied', 'work', UTF8Encode(Dest.WorkingDir));
  AssertTrue('RunFromCategory copied', Dest.RunFromCategory);
  AssertEquals('environment copied', 'A=1',
    Trim(UTF8Encode(Dest.EnvironmentVars.Text)));
  AssertTrue('environment list copied by value',
    Dest.EnvironmentVars <> Source.EnvironmentVars);
end;

procedure TTestFileNodeData.TestClickCountRegistersMFU;
var
  Node: TvFileNodeData;
  List: TSpecialItemsList;
begin
  List := ASuiteManager.ListManager.MFUList;
  Node := AddFile('File', '/bin/sh');

  Node.ClickCount := 3;
  AssertEquals('clicked node enters the MFU list', 1, List.Count);

  Node.NoMFU := True;
  AssertEquals('excluded node leaves the MFU list', 0, List.Count);

  Node.NoMFU := False;
  AssertEquals('re-included node with clicks rejoins', 1, List.Count);
end;

procedure TTestFileNodeData.TestLastAccessRegistersMRU;
var
  Node: TvFileNodeData;
  List: TSpecialItemsList;
begin
  List := ASuiteManager.ListManager.MRUList;
  Node := AddFile('File', '/bin/sh');

  Node.LastAccess := SAMPLE_UNIX_DATE;
  AssertEquals('accessed node enters the MRU list', 1, List.Count);

  Node.NoMRU := True;
  AssertEquals('excluded node leaves the MRU list', 0, List.Count);

  Node.NoMRU := False;
  AssertEquals('re-included node with access rejoins', 1, List.Count);
end;

procedure TTestFileNodeData.TestCountersAreIdleWhenImporting;
var
  Node: TvFileNodeData;
  OldState: TLauncherState;
  Manager: TListManager;
begin
  OldState := Config.ASuiteState;
  Config.ASuiteState := lsImporting;
  try
    Manager := ASuiteManager.ListManager;
    Node := AddFile('File', '/bin/sh');
    Node.ClickCount := 3;
    Node.LastAccess := SAMPLE_UNIX_DATE;
    Node.NoMFU := True;
    Node.NoMRU := True;

    AssertEquals('MFU list untouched', 0, Manager.MFUList.Count);
    AssertEquals('MRU list untouched', 0, Manager.MRUList.Count);
  finally
    Config.ASuiteState := OldState;
  end;
end;

procedure TTestFileNodeData.TestSetShortcutDesktopFalseToFalseIsNoOp;
var
  Node: TvFileNodeData;
begin
  // Setting the flag to its current value must not touch the desktop
  Node := AddFile('File', '/bin/sh');
  Node.ShortcutDesktop := False;
  AssertFalse('flag stays off', Node.ShortcutDesktop);
end;

procedure TTestFileNodeData.TestDeleteShortcutFileWhenNotOnDesktopIsNoOp;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('File', '/bin/sh');
  // Must not raise and must not touch the desktop
  Node.DeleteShortcutFile;
end;

initialization
  RegisterTest(TTestFileNodeData);

end.
