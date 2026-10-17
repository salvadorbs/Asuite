unit Tests.SearchController;

{$mode objfpc}{$H+}

{ Behaviour tests for TSearchController (Utility.SearchController).

  ExecuteOn searches any tree without rebinding the controller's own tree
  and needs no widgets at all. The event handlers are covered with a real
  TButtonedEdit and TPopupMenu: nil controls are always safe no-ops, and
  the checked menu item selects the search field. OnLeftButtonClick is only
  covered with a nil menu: a real PopUp would block waiting for input. }

interface

uses
  Classes, SysUtils, Menus, ButtonedEdit, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  Kernel.Enumerations, Kernel.Types,
  Utility.SearchController,
  AppConfig.Main,
  Tests.Helpers.Tree;

type
  TTestSearchController = class(TNodeTreeTestCase)
  private
    FSearchA, FSearchB: TVirtualStringTree;
    function NewController(ASearch: TVirtualStringTree; AEdit: TButtonedEdit;
      AMenu: TPopupMenu): TSearchController;
    function NewMenu(AChecked: TSearchType): TPopupMenu;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteOnFindsByName;
    procedure TestExecuteOnEmptyKeywordClears;
    procedure TestExecuteOnNilTreeIsSafe;
    procedure TestExecuteOnNoMatchLeavesEmpty;
    procedure TestExecuteOnSearchesGivenTree;
    procedure TestExecuteSearchesOwnedTree;
    procedure TestExecuteOnLowercasesKeyword;
    procedure TestOnTextChangeNilEditIsSafe;
    procedure TestOnTextChangeSearchesWhenTyping;
    procedure TestOnTextChangeIdleWhenNotTyping;
    procedure TestOnRightButtonClickSearchesWhenNotTyping;
    procedure TestOnRightButtonClickClearsWhenTyping;
    procedure TestOnLeftButtonClickNilMenuIsSafe;
    procedure TestOnKeyPressOtherKeyIsSafe;
    procedure TestOnKeyPressEnterDelegates;
    procedure TestInitIconsNilEditIsSafe;
    procedure TestInitIconsRealEditIsSafe;
  end;

implementation

function TTestSearchController.NewController(ASearch: TVirtualStringTree;
  AEdit: TButtonedEdit; AMenu: TPopupMenu): TSearchController;
begin
  Result := TSearchController.Create(ASearch, AEdit, AMenu);
end;

function TTestSearchController.NewMenu(AChecked: TSearchType): TPopupMenu;
var
  Kind: TSearchType;
  Item: TMenuItem;
begin
  Result := TPopupMenu.Create(nil);
  for Kind := Low(TSearchType) to High(TSearchType) do
  begin
    Item := TMenuItem.Create(Result);
    Item.Caption := 'field';
    Item.Tag := Ord(Kind);
    Item.Checked := Kind = AChecked;
    Result.Items.Add(Item);
  end;
end;

procedure TTestSearchController.SetUp;
begin
  inherited SetUp;
  // Search trees hold plain links, no owned objects: no OnFreeNode
  FSearchA := TVirtualStringTree.Create(nil);
  FSearchA.NodeDataSize := SizeOf(rTreeDataX);
  FSearchB := TVirtualStringTree.Create(nil);
  FSearchB.NodeDataSize := SizeOf(rTreeDataX);
end;

procedure TTestSearchController.TearDown;
begin
  FSearchA.Free;
  FSearchB.Free;
  inherited TearDown;
end;

procedure TTestSearchController.TestExecuteOnFindsByName;
var
  Controller: TSearchController;
  Found: PVirtualNode;
begin
  AddFile('MyApp', '/bin/other');
  Controller := NewController(FSearchA, nil, nil);
  try
    Controller.ExecuteOn(FSearchB, 'myapp', stName);
    Found := FSearchB.GetFirst;
    AssertNotNull('match found', Found);
    AssertTrue('search node links the main node',
      PTreeDataX(FSearchB.GetNodeData(Found))^.pNodeList = Tree.GetFirst);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestExecuteOnEmptyKeywordClears;
var
  Controller: TSearchController;
begin
  AddFile('MyApp', '/bin/other');
  Controller := NewController(FSearchA, nil, nil);
  try
    Controller.ExecuteOn(FSearchA, 'myapp', stName);
    AssertNotNull('populated first', FSearchA.GetFirst);
    Controller.ExecuteOn(FSearchA, '', stName);
    AssertTrue('empty keyword clears', FSearchA.GetFirst = nil);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestExecuteOnNilTreeIsSafe;
var
  Controller: TSearchController;
begin
  Controller := NewController(FSearchA, nil, nil);
  try
    // Must not raise
    Controller.ExecuteOn(nil, 'myapp', stName);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestExecuteOnNoMatchLeavesEmpty;
var
  Controller: TSearchController;
begin
  AddFile('MyApp', '/bin/other');
  Controller := NewController(FSearchA, nil, nil);
  try
    Controller.ExecuteOn(FSearchA, 'zzz-no-match', stName);
    AssertTrue('no match, no nodes', FSearchA.GetFirst = nil);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestExecuteOnSearchesGivenTree;
var
  Controller: TSearchController;
begin
  AddFile('MyApp', '/bin/other');
  Controller := NewController(FSearchA, nil, nil);
  try
    Controller.ExecuteOn(FSearchB, 'myapp', stName);
    AssertNotNull('given tree populated', FSearchB.GetFirst);
    AssertTrue('owned tree untouched', FSearchA.GetFirst = nil);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestExecuteSearchesOwnedTree;
var
  Controller: TSearchController;
begin
  AddFile('MyApp', '/bin/other');
  Controller := NewController(FSearchA, nil, nil);
  try
    Controller.Execute('myapp', stName);
    AssertNotNull('owned tree populated', FSearchA.GetFirst);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestExecuteOnLowercasesKeyword;
var
  Controller: TSearchController;
begin
  // The controller lowercases; the raw FindNode callback does not
  AddFile('MyApp', '/bin/other');
  Controller := NewController(FSearchA, nil, nil);
  try
    Controller.ExecuteOn(FSearchA, 'MYAPP', stName);
    AssertNotNull('uppercase keyword still matches', FSearchA.GetFirst);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestOnTextChangeNilEditIsSafe;
var
  Controller: TSearchController;
  Key: WideChar;
begin
  Controller := NewController(FSearchA, nil, nil);
  try
    // Must not raise
    Controller.OnTextChange(nil);
    Key := 'x';
    Controller.OnKeyPress(nil, Key);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestOnTextChangeSearchesWhenTyping;
var
  Controller: TSearchController;
  Edit: TButtonedEdit;
  Menu: TPopupMenu;
  OldTyping: Boolean;
begin
  OldTyping := Config.SearchAsYouType;
  Config.SearchAsYouType := True;
  Edit := TButtonedEdit.Create(nil);
  Menu := NewMenu(stName);
  Controller := NewController(FSearchA, Edit, Menu);
  try
    AddFile('MyApp', '/bin/other');
    Edit.Text := 'myapp';
    Controller.OnTextChange(Edit);
    AssertNotNull('typing searches the owned tree', FSearchA.GetFirst);
  finally
    Controller.Free;
    Menu.Free;
    Edit.Free;
    Config.SearchAsYouType := OldTyping;
  end;
end;

procedure TTestSearchController.TestOnTextChangeIdleWhenNotTyping;
var
  Controller: TSearchController;
  Edit: TButtonedEdit;
  Menu: TPopupMenu;
  OldTyping: Boolean;
begin
  OldTyping := Config.SearchAsYouType;
  Config.SearchAsYouType := False;
  Edit := TButtonedEdit.Create(nil);
  Menu := NewMenu(stName);
  Controller := NewController(FSearchA, Edit, Menu);
  try
    AddFile('MyApp', '/bin/other');
    Edit.Text := 'myapp';
    Controller.OnTextChange(Edit);
    AssertTrue('no search without search-as-you-type',
      FSearchA.GetFirst = nil);
  finally
    Controller.Free;
    Menu.Free;
    Edit.Free;
    Config.SearchAsYouType := OldTyping;
  end;
end;

procedure TTestSearchController.TestOnRightButtonClickSearchesWhenNotTyping;
var
  Controller: TSearchController;
  Edit: TButtonedEdit;
  Menu: TPopupMenu;
  OldTyping: Boolean;
begin
  OldTyping := Config.SearchAsYouType;
  Config.SearchAsYouType := False;
  Edit := TButtonedEdit.Create(nil);
  Menu := NewMenu(stPathFile);
  Controller := NewController(FSearchA, Edit, Menu);
  try
    AddFile('App', '/opt/myapp-portable/run.sh');
    Edit.Text := 'portable';
    Controller.OnRightButtonClick(Edit);
    AssertNotNull('button searches the checked field', FSearchA.GetFirst);
  finally
    Controller.Free;
    Menu.Free;
    Edit.Free;
    Config.SearchAsYouType := OldTyping;
  end;
end;

procedure TTestSearchController.TestOnRightButtonClickClearsWhenTyping;
var
  Controller: TSearchController;
  Edit: TButtonedEdit;
  OldTyping: Boolean;
begin
  OldTyping := Config.SearchAsYouType;
  Config.SearchAsYouType := True;
  Edit := TButtonedEdit.Create(nil);
  Controller := NewController(FSearchA, Edit, nil);
  try
    Edit.Text := 'myapp';
    Controller.OnRightButtonClick(Edit);
    AssertEquals('button clears the text', '', Edit.Text);
  finally
    Controller.Free;
    Edit.Free;
    Config.SearchAsYouType := OldTyping;
  end;
end;

procedure TTestSearchController.TestOnLeftButtonClickNilMenuIsSafe;
var
  Controller: TSearchController;
begin
  Controller := NewController(FSearchA, nil, nil);
  try
    // Must not raise; a real menu would pop up modally
    Controller.OnLeftButtonClick(nil);
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestOnKeyPressOtherKeyIsSafe;
var
  Controller: TSearchController;
  Key: WideChar;
begin
  Controller := NewController(FSearchA, nil, nil);
  try
    Key := 'x';
    Controller.OnKeyPress(nil, Key);
    AssertTrue('other keys pass through', Key = 'x');
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestOnKeyPressEnterDelegates;
var
  Controller: TSearchController;
  Edit: TButtonedEdit;
  OldTyping: Boolean;
  Key: WideChar;
begin
  OldTyping := Config.SearchAsYouType;
  Config.SearchAsYouType := False;
  Edit := TButtonedEdit.Create(nil);
  Controller := NewController(FSearchA, Edit, nil);
  try
    // No menu, so no checked item: Enter safely searches nothing
    Key := #13;
    Controller.OnKeyPress(Edit, Key);
    AssertTrue('nothing searched without a field', FSearchA.GetFirst = nil);
  finally
    Controller.Free;
    Edit.Free;
    Config.SearchAsYouType := OldTyping;
  end;
end;

procedure TTestSearchController.TestInitIconsNilEditIsSafe;
var
  Controller: TSearchController;
begin
  Controller := NewController(FSearchA, nil, nil);
  try
    // Must not raise
    Controller.InitIcons;
  finally
    Controller.Free;
  end;
end;

procedure TTestSearchController.TestInitIconsRealEditIsSafe;
var
  Controller: TSearchController;
  Edit: TButtonedEdit;
begin
  Edit := TButtonedEdit.Create(nil);
  Controller := NewController(FSearchA, Edit, nil);
  try
    // Icon lookups may miss without a loaded theme; assigning is safe
    Controller.InitIcons;
  finally
    Controller.Free;
    Edit.Free;
  end;
end;

initialization
  RegisterTest(TTestSearchController);

end.
