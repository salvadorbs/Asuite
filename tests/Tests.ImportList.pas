unit Tests.ImportList;

{$mode objfpc}{$H+}

{ Behaviour tests for TfrmImportList (Forms.ImportList).

  The form is created per test and published as the frmImportList global,
  which is what ASuiteInstance.ImportTree reads: node lookups inside the
  import tree then resolve exactly like in the running application. The
  import tree is wired with SetupVSTImportList (bare dmImages), so its
  OnFreeNode frees node data on teardown.

  Deliberately excluded: the SQL branch of PopulateTree (database
  round-trips are covered in Tests.Database*) and the wizard buttons. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  Forms.ImportList,
  DataModules.Icons,
  NodeDataTypes.Base, NodeDataTypes.Files,
  VirtualTree.Methods,
  Kernel.Enumerations, Kernel.Types, Kernel.Instance,
  AppConfig.Main, AppConfig.Paths,
  Tests.Helpers.Tree;

type
  TTestImportList = class(TNodeTreeTestCase)
  private
    FForm: TfrmImportList;
    function WriteXmlFile(const AName, AContent: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetNumberNodeImp;
    procedure TestCheckedEnablesNext;
    procedure TestUncheckedDisablesNext;
    procedure TestPopulateTreeXml;
    procedure TestPopulateTreeUnknownXml;
    procedure TestMassRelativeAbsoluteRoundTrip;
    procedure TestTreeImpToTreeChecked;
    procedure TestTreeImpToTreeUncheckedSkipped;
  end;

implementation

procedure TTestImportList.SetUp;
begin
  inherited SetUp;
  dmImages := TdmImages.CreateNew(nil);
  FForm := nil;
  try
    FForm := TfrmImportList.Create(nil);
    ASuiteInstance.VSTEvents.SetupVSTImportList(FForm.vstListImp);
    frmImportList := FForm;
  except
    FForm.Free;
    FForm := nil;
    FreeAndNil(dmImages);
    ReleaseTree;
    raise;
  end;
end;

procedure TTestImportList.TearDown;
begin
  // The form must be freed while still published: its OnFreeNode resolves
  // nodes through ASuiteInstance.ImportTree
  FreeAndNil(FForm);
  frmImportList := nil;
  FreeAndNil(dmImages);
  inherited TearDown;
end;

function TTestImportList.WriteXmlFile(const AName, AContent: string): string;
var
  FS: TFileStream;
  Raw: RawByteString;
begin
  Result := GetTempDir(False) + AName;
  Raw := RawByteString(AContent);
  FS := TFileStream.Create(Result, fmCreate);
  try
    if Length(Raw) > 0 then
      FS.WriteBuffer(Raw[1], Length(Raw));
  finally
    FS.Free;
  end;
end;

procedure TTestImportList.TestGetNumberNodeImp;
var
  First, Second: PVirtualNode;
begin
  AddFile('File A', '/bin/a');
  First := Tree.GetFirst;
  AddFile('File B', '/bin/b');
  Second := Tree.GetNextSibling(First);

  Tree.CheckState[First] := csCheckedNormal;
  Tree.CheckState[Second] := csCheckedNormal;
  AssertEquals('two checked', 2, FForm.GetNumberNodeImp(Tree));

  Tree.CheckState[Second] := csUncheckedNormal;
  AssertEquals('one checked', 1, FForm.GetNumberNodeImp(Tree));
end;

procedure TTestImportList.TestCheckedEnablesNext;
begin
  AddFile('File', '/bin/sh');
  Tree.CheckState[Tree.GetFirst] := csCheckedNormal;
  FForm.btnNext.Enabled := False;
  FForm.vstListImpChecked(Tree, Tree.GetFirst);
  AssertTrue('next enabled', FForm.btnNext.Enabled);
end;

procedure TTestImportList.TestUncheckedDisablesNext;
begin
  AddFile('File', '/bin/sh');
  FForm.btnNext.Enabled := True;
  FForm.vstListImpChecked(Tree, Tree.GetFirst);
  AssertFalse('next disabled', FForm.btnNext.Enabled);
end;

procedure TTestImportList.TestPopulateTreeXml;
var
  Path: string;
  CatNode, FileNode: PVirtualNode;
  Data: TvFileNodeData;
begin
  Path := WriteXmlFile('asuite_test_import.xml',
    '<ASuite>' +
    '<Category Name="Cat">' +
    '<Software Name="App">' +
    '<PathExe>/bin/app</PathExe>' +
    '<Parameters>--x</Parameters>' +
    '</Software>' +
    '</Category>' +
    '</ASuite>');
  try
    FForm.PopulateTree(FForm.vstListImp, Path);
    AssertEquals('one root', 1, FForm.vstListImp.RootNodeCount);
    CatNode := FForm.vstListImp.GetFirst;
    FileNode := FForm.vstListImp.GetFirstChild(CatNode);
    AssertNotNull('software imported', FileNode);
    Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(FileNode,
      FForm.vstListImp));
    AssertEquals('name', 'App', UTF8Encode(Data.Name));
    AssertEquals('path', '/bin/app', UTF8Encode(Data.PathFile));
  finally
    DeleteFile(Path);
  end;
end;

procedure TTestImportList.TestPopulateTreeUnknownXml;
var
  Path: string;
begin
  Path := WriteXmlFile('asuite_test_unknown.xml',
    '<SomethingElse><Item Name="x"/></SomethingElse>');
  try
    FForm.PopulateTree(FForm.vstListImp, Path);
    AssertEquals('unknown format imports nothing', 0,
      FForm.vstListImp.RootNodeCount);
  finally
    DeleteFile(Path);
  end;
end;

procedure TTestImportList.TestMassRelativeAbsoluteRoundTrip;
var
  Node: PVirtualNode;
  Data: TvFileNodeData;
  Paths: TConfigPaths;
  Abs, Back: UnicodeString;
  Abort: Boolean;
begin
  Data := AddFile('Tool', '/bin/tool');
  Node := Tree.GetFirst;
  Abs := ASuiteInstance.Paths.RelativeToAbsolute('%asuite%/rel/app.exe');
  Data.PathFile := Abs;
  Data.PathIcon := Abs;

  // Absolute inside the suite folder goes relative and back losslessly
  Paths := ASuiteInstance.Paths;
  Abort := False;
  FForm.MassAbsoluteToRelativePath(Tree, Node, @Paths, Abort);
  AssertEquals('relative form', '%asuite%/rel/app.exe',
    UTF8Encode(Data.PathFile));
  AssertFalse('iteration continues', Abort);

  FForm.MassRelativeToAbsolutePath(Tree, Node, @Paths, Abort);
  Back := ASuiteInstance.Paths.RelativeToAbsolute('%asuite%/rel/app.exe');
  AssertEquals('absolute form restored', UTF8Encode(Back),
    UTF8Encode(Data.PathFile));
  AssertEquals('icon follows', UTF8Encode(Back),
    UTF8Encode(Data.PathIcon));
end;

procedure TTestImportList.TestTreeImpToTreeChecked;
var
  CatNode: PVirtualNode;
  NewNode: PVirtualNode;
  Data: TvBaseNodeData;
begin
  AddCategory('Cat');
  CatNode := Tree.GetFirst;
  AddFile('File', '/bin/sh', CatNode);
  Tree.CheckState[CatNode] := csCheckedNormal;
  Tree.CheckState[Tree.GetFirstChild(CatNode)] := csCheckedNormal;

  AssertTrue('import succeeds', FForm.TreeImpToTree(Tree, FForm.vstListImp));

  NewNode := FForm.vstListImp.GetFirst;
  AssertNotNull('category copied', NewNode);
  Data := TVirtualTreeMethods.GetNodeItemData(NewNode, FForm.vstListImp);
  AssertEquals('name copied', 'Cat', UTF8Encode(Data.Name));
  AssertNotNull('child copied',
    FForm.vstListImp.GetFirstChild(NewNode));
end;

procedure TTestImportList.TestTreeImpToTreeUncheckedSkipped;
begin
  AddCategory('Cat');
  AddFile('File', '/bin/sh', Tree.GetFirst);

  AssertTrue('import succeeds', FForm.TreeImpToTree(Tree, FForm.vstListImp));
  AssertEquals('unchecked nodes skipped', 0, FForm.vstListImp.RootNodeCount);
end;

initialization
  RegisterTest(TTestImportList);

end.
