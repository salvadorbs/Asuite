unit Tests.TrayMenu;

{$mode objfpc}{$H+}

{ Behaviour tests for TdmTrayMenu menu building (DataModules.TrayMenu).

  Both data modules are fully streamed, like the running application: the
  classic menu is rebuilt from the main tree, including icons (missing
  paths fall back to the file-error icon). RefreshClassicMenu is what
  TVirtualTreeMethods.RefreshList calls on Linux, so this suite also
  unlocks every list-saving handler (DoNewText, drag and drop, Enter key,
  ExecuteSelectedNodes, clipboard paste, run controller). }

interface

uses
  Classes, SysUtils, Menus, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  DataModules.Icons, DataModules.TrayMenu,
  NodeDataTypes.Base, NodeDataTypes.Files,
  VirtualTree.Methods,
  Kernel.Enumerations, Kernel.Manager,
  AppConfig.Main,
  Tests.Helpers.Tree;

type
  TTestTrayMenu = class(TNodeTreeTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRefreshBuildsMenu;
    procedure TestRefreshEmptyTreeBuildsFrame;
    procedure TestRefreshListsSpecialItems;
  end;

implementation

procedure TTestTrayMenu.SetUp;
begin
  inherited SetUp;
  // Streamed like Application.CreateForm does: real popup menu, tray icon
  // and image list, but nothing is ever shown
  dmImages := TdmImages.Create(nil);
  dmTrayMenu := TdmTrayMenu.Create(nil);
end;

procedure TTestTrayMenu.TearDown;
begin
  // BeforeDestruction unregisters the Config observer
  FreeAndNil(dmTrayMenu);
  FreeAndNil(dmImages);
  inherited TearDown;
end;

procedure TTestTrayMenu.TestRefreshBuildsMenu;
var
  Menu: TPopupMenu;
begin
  AddCategory('Cat');
  AddFile('Tool', '/bin/tool', Tree.GetFirst);

  dmTrayMenu.RefreshClassicMenu;

  Menu := dmTrayMenu.pmTrayicon;
  AssertNotNull('popup assigned', Menu);
  AssertTrue('menu populated', Menu.Items.Count > 2);
end;

procedure TTestTrayMenu.TestRefreshEmptyTreeBuildsFrame;
var
  Menu: TPopupMenu;
begin
  dmTrayMenu.RefreshClassicMenu;

  Menu := dmTrayMenu.pmTrayicon;
  AssertTrue('header/footer still built', Menu.Items.Count > 0);
end;

procedure TTestTrayMenu.TestRefreshListsSpecialItems;
var
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Node.ClickCount := 5;
  Node.LastAccess := 2000000000;

  dmTrayMenu.RefreshClassicMenu;

  AssertTrue('menu populated', dmTrayMenu.pmTrayicon.Items.Count > 2);
end;

initialization
  RegisterTest(TTestTrayMenu);

end.
