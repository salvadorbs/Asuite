unit Tests.XmlImport;

{$mode objfpc}{$H+}

{ Behaviour tests for the XML helpers and legacy importers (Utility.XML).

  The property getters and XMLToShortcut run on in-memory DOM documents.
  The three old-format converters (ASuite 1.x, PStart, winPenPack) run
  through XMLToTree on the shared tree fixture with compact documents.

  Deliberately excluded: LoadDatabaseFromXML/LoadXMLSettings (they rewrite
  large parts of Config, including the real icon-cache folders) and the
  scheduler date parsing (StrToDateTime follows the locale). XMLToTree with
  ltASuite2 is not covered either: the dispatch has no branch for it, so
  the converter procvar stays nil. }

interface

uses
  Classes, SysUtils, DOM, XMLRead, Menus, LCLProc, LCLType, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  NodeDataTypes.Base, NodeDataTypes.Custom, NodeDataTypes.Files,
  VirtualTree.Methods,
  Kernel.Enumerations, Kernel.Types, Kernel.Manager, Lists.Manager,
  AppConfig.Main, Utility.XML,
  Tests.Helpers.Tree;

type
  TTestXmlGetters = class(TTestCase)
  private
    function LoadDoc(const AXml: string): TXMLDocument;
  published
    procedure TestGetStrPresent;
    procedure TestGetStrMissing;
    procedure TestGetStrEmpty;
    procedure TestGetIntPresent;
    procedure TestGetIntMissing;
    procedure TestGetBoolTrue;
    procedure TestGetBoolFalse;
    procedure TestGetBoolMissing;
    procedure TestXMLToShortcutFull;
    procedure TestXMLToShortcutMissing;
    procedure TestXMLToShortcutZeroKey;
  end;

  TTestOldListImport = class(TNodeTreeTestCase)
  private
    function LoadDoc(const AXml: string): TXMLDocument;
  published
    procedure TestASuite1UnknownNodeReturnsNil;
    procedure TestASuite1CategoryAndSoftware;
    procedure TestPStartDispatch;
    procedure TestPStartUnknownNodeReturnsNil;
    procedure TestWppDispatch;
  end;

implementation

function DocFromString(const AXml: string): TXMLDocument;
var
  Path: string;
  FS: TFileStream;
  Raw: RawByteString;
begin
  Path := GetTempDir(False) + 'asuite_test_doc.xml';
  Raw := RawByteString(AXml);
  FS := TFileStream.Create(Path, fmCreate);
  try
    if Length(Raw) > 0 then
      FS.WriteBuffer(Raw[1], Length(Raw));
  finally
    FS.Free;
  end;
  ReadXMLFile(Result, Path);
  DeleteFile(Path);
end;

{ TTestXmlGetters }

function TTestXmlGetters.LoadDoc(const AXml: string): TXMLDocument;
begin
  Result := DocFromString(AXml);
end;

procedure TTestXmlGetters.TestGetStrPresent;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Name>App</Name></item>');
  try
    AssertEquals('value read', 'App',
      UTF8Encode(GetStrPropertyXML(Doc.DocumentElement, 'Name', 'def')));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetStrMissing;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Name>App</Name></item>');
  try
    AssertEquals('default kept', 'def',
      UTF8Encode(GetStrPropertyXML(Doc.DocumentElement, 'Other', 'def')));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetStrEmpty;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Name></Name></item>');
  try
    AssertEquals('empty element keeps default', 'def',
      UTF8Encode(GetStrPropertyXML(Doc.DocumentElement, 'Name', 'def')));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetIntPresent;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Count>42</Count></item>');
  try
    AssertEquals('value read', 42,
      GetIntPropertyXML(Doc.DocumentElement, 'Count', 0));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetIntMissing;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Count>42</Count></item>');
  try
    AssertEquals('default kept', 7,
      GetIntPropertyXML(Doc.DocumentElement, 'Other', 7));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetBoolTrue;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Flag>1</Flag></item>');
  try
    AssertTrue('1 is true',
      GetBoolPropertyXML(Doc.DocumentElement, 'Flag', False));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetBoolFalse;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Flag>0</Flag></item>');
  try
    AssertFalse('0 is false',
      GetBoolPropertyXML(Doc.DocumentElement, 'Flag', True));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestGetBoolMissing;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Flag>1</Flag></item>');
  try
    AssertTrue('default kept',
      GetBoolPropertyXML(Doc.DocumentElement, 'Other', True));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestXMLToShortcutFull;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><HotKeyCode>0</HotKeyCode>' +
    '<HotKeyModifier>1</HotKeyModifier></item>');
  try
    AssertEquals('code 0 + ctrl',
      ShortCut(VK_A, [ssCtrl]),
      XMLToShortcut(Doc.DocumentElement, 'HotKeyCode', 'HotKeyModifier'));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestXMLToShortcutMissing;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><Name>App</Name></item>');
  try
    AssertEquals('missing fields give no shortcut', 0,
      XMLToShortcut(Doc.DocumentElement, 'HotKeyCode', 'HotKeyModifier'));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlGetters.TestXMLToShortcutZeroKey;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<item><HotKeyCode>99</HotKeyCode>' +
    '<HotKeyModifier>1</HotKeyModifier></item>');
  try
    AssertEquals('unknown code gives no shortcut', 0,
      XMLToShortcut(Doc.DocumentElement, 'HotKeyCode', 'HotKeyModifier'));
  finally
    Doc.Free;
  end;
end;

{ TTestOldListImport }

function TTestOldListImport.LoadDoc(const AXml: string): TXMLDocument;
begin
  Result := DocFromString(AXml);
end;

procedure TTestOldListImport.TestASuite1UnknownNodeReturnsNil;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<ASuite><Bogus Name="x"/></ASuite>');
  try
    AssertTrue('unknown elements are skipped',
      TImportOldListProcs.ASuite1NodeToTree(Tree,
        Doc.DocumentElement.FirstChild, nil) = nil);
    AssertEquals('nothing added', 0, Tree.RootNodeCount);
  finally
    Doc.Free;
  end;
end;

procedure TTestOldListImport.TestASuite1CategoryAndSoftware;
var
  Doc: TXMLDocument;
  CatNode, FileNode, SepNode: PVirtualNode;
  Data: TvFileNodeData;
  Manager: TListManager;
begin
  Doc := LoadDoc(
    '<ASuite>' +
    '<Category Name="Cat">' +
    '<Software Name="App">' +
    '<PathExe>/bin/app</PathExe>' +
    '<Parameters>--x</Parameters>' +
    '<PathIcon>icon.ico</PathIcon>' +
    '<HideSoftwareMenu>1</HideSoftwareMenu>' +
    '<SchedulerMode>1</SchedulerMode>' +
    '<Autorun>1</Autorun>' +
    '<AutorunPosition>2</AutorunPosition>' +
    '<WindowState>2</WindowState>' +
    '<ActionOnExe>2</ActionOnExe>' +
    '<HotKeyCode>0</HotKeyCode>' +
    '<HotKeyModifier>1</HotKeyModifier>' +
    '<DontInsertMRU>1</DontInsertMRU>' +
    '<WorkingDir>/tmp</WorkingDir>' +
    '<ShortcutDesktop>0</ShortcutDesktop>' +
    '</Software>' +
    '<Separator/>' +
    '</Category>' +
    '</ASuite>');
  try
    XMLToTree(Tree, ltASuite1, Doc);

    AssertEquals('one root', 1, Tree.RootNodeCount);
    CatNode := Tree.GetFirst;
    AssertEquals('category name', 'Cat',
      UTF8Encode(TVirtualTreeMethods.GetNodeItemData(CatNode, Tree).Name));

    FileNode := Tree.GetFirstChild(CatNode);
    AssertNotNull('software imported', FileNode);
    Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(FileNode, Tree));
    AssertEquals('name', 'App', UTF8Encode(Data.Name));
    AssertEquals('path', '/bin/app', UTF8Encode(Data.PathFile));
    AssertEquals('parameters', '--x', UTF8Encode(Data.Parameters));
    AssertEquals('icon', 'icon.ico', UTF8Encode(Data.PathIcon));
    AssertTrue('hidden flag', Data.HideFromMenu);
    AssertTrue('scheduler mode', Data.SchMode = smOnce);
    AssertTrue('autorun', Data.Autorun = atAlwaysOnStart);
    AssertEquals('autorun position', 2, Data.AutorunPos);
    AssertEquals('window state', 2, Data.WindowState);
    AssertTrue('action on execute', Data.ActionOnExe = aeRunAndHide);
    AssertEquals('hotkey', ShortCut(VK_A, [ssCtrl]), Data.Hotkey);
    AssertTrue('no mru', Data.NoMRU);
    AssertEquals('working dir', '/tmp', UTF8Encode(Data.WorkingDir));

    SepNode := Tree.GetNextSibling(FileNode);
    AssertNotNull('separator imported', SepNode);
    AssertTrue('separator type',
      TVirtualTreeMethods.GetNodeItemData(SepNode, Tree).IsSeparatorItem);

    Manager := ASuiteManager.ListManager;
    AssertEquals('scheduler list fed', 1, Manager.SchedulerItemList.Count);
    AssertEquals('startup list fed', 1, Manager.StartupItemList.Count);
  finally
    Doc.Free;
  end;
end;

procedure TTestOldListImport.TestPStartDispatch;
var
  Doc: TXMLDocument;
  CatNode, FileNode, SepNode: PVirtualNode;
  Data: TvFileNodeData;
begin
  Doc := LoadDoc(
    '<PStart>' +
    '<files name="Cats">' +
    '<file name="App">' +
    '<path>/bin/app</path>' +
    '<parameters>--x</parameters>' +
    '<directory>/tmp</directory>' +
    '<windowstate>2</windowstate>' +
    '<onstartup>1</onstartup>' +
    '</file>' +
    '<separator/>' +
    '</files>' +
    '</PStart>');
  try
    XMLToTree(Tree, ltPStart1, Doc);

    CatNode := Tree.GetFirst;
    AssertEquals('group name', 'Cats',
      UTF8Encode(TVirtualTreeMethods.GetNodeItemData(CatNode, Tree).Name));

    FileNode := Tree.GetFirstChild(CatNode);
    AssertNotNull('file imported', FileNode);
    Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(FileNode, Tree));
    AssertEquals('name', 'App', UTF8Encode(Data.Name));
    AssertEquals('path', '/bin/app', UTF8Encode(Data.PathFile));
    AssertEquals('parameters', '--x', UTF8Encode(Data.Parameters));
    AssertEquals('directory', '/tmp', UTF8Encode(Data.WorkingDir));
    AssertEquals('window state', 2, Data.WindowState);
    AssertTrue('startup flag', Data.Autorun = atAlwaysOnStart);

    SepNode := Tree.GetNextSibling(FileNode);
    AssertNotNull('separator imported', SepNode);
  finally
    Doc.Free;
  end;
end;

procedure TTestOldListImport.TestPStartUnknownNodeReturnsNil;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('<PStart><bogus name="x"/></PStart>');
  try
    AssertTrue('unknown elements are skipped',
      TImportOldListProcs.PStartNodeToTree(Tree,
        Doc.DocumentElement.FirstChild, nil) = nil);
  finally
    Doc.Free;
  end;
end;

procedure TTestOldListImport.TestWppDispatch;
var
  Doc: TXMLDocument;
  CatNode, FileNode, SepNode: PVirtualNode;
  Data: TvFileNodeData;
begin
  Doc := LoadDoc(
    '<wpp>' +
    '<files name="Cats">' +
    '<file name="App">' +
    '<path>/bin/app</path>' +
    '<parameters>--x</parameters>' +
    '<icon>icon.ico</icon>' +
    '<HideSoftwareMenu>1</HideSoftwareMenu>' +
    '<WorkingDir>/tmp</WorkingDir>' +
    '<ActionOnExe>2</ActionOnExe>' +
    '</file>' +
    '<separator/>' +
    '</files>' +
    '</wpp>');
  try
    XMLToTree(Tree, ltwppLauncher1, Doc);

    CatNode := Tree.GetFirst;
    AssertEquals('group name', 'Cats',
      UTF8Encode(TVirtualTreeMethods.GetNodeItemData(CatNode, Tree).Name));

    FileNode := Tree.GetFirstChild(CatNode);
    AssertNotNull('file imported', FileNode);
    Data := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(FileNode, Tree));
    AssertEquals('name', 'App', UTF8Encode(Data.Name));
    AssertEquals('path', '/bin/app', UTF8Encode(Data.PathFile));
    AssertEquals('icon', 'icon.ico', UTF8Encode(Data.PathIcon));
    AssertTrue('hidden flag', Data.HideFromMenu);
    AssertEquals('working dir', '/tmp', UTF8Encode(Data.WorkingDir));
    AssertTrue('action on execute', Data.ActionOnExe = aeRunAndHide);

    SepNode := Tree.GetNextSibling(FileNode);
    AssertNotNull('separator imported', SepNode);
  finally
    Doc.Free;
  end;
end;

initialization
  RegisterTest(TTestXmlGetters);
  RegisterTest(TTestOldListImport);

end.
