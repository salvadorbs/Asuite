unit Utility.SearchController;

{$MODE DelphiUnicode}

interface

uses
  Classes, Controls, ButtonedEdit, Menus, VirtualTrees, Kernel.Enumerations;

type
  TSearchController = class
  private
    FTreeSearch: TBaseVirtualTree;
    FSearchEdit: TButtonedEdit;
    FSearchMenu: TPopupMenu;
    function GetCheckedMenuItem(AMenu: TPopupMenu): TMenuItem;
    procedure DoSearchItem(const Keyword: string; const SearchType: TSearchType);
  public
    constructor Create(ATreeSearch: TBaseVirtualTree; ASearchEdit: TButtonedEdit; ASearchMenu: TPopupMenu);
    procedure InitIcons;
    procedure OnTextChange(Sender: TObject);
    procedure OnRightButtonClick(Sender: TObject);
    procedure OnLeftButtonClick(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    // Public API to execute a search
    procedure Execute(const Keyword: string; const SearchType: TSearchType);
    procedure ExecuteOn(ATree: TBaseVirtualTree; const Keyword: string; const SearchType: TSearchType);
  end;

implementation

uses
  SysUtils, LCLType, AppConfig.Main, Kernel.Manager, Kernel.Instance,
  VirtualTree.Methods, Kernel.Types;

constructor TSearchController.Create(ATreeSearch: TBaseVirtualTree; ASearchEdit: TButtonedEdit; ASearchMenu: TPopupMenu);
begin
  inherited Create;
  FTreeSearch := ATreeSearch;
  FSearchEdit := ASearchEdit;
  FSearchMenu := ASearchMenu;
end;

procedure TSearchController.InitIcons;
begin
  if Assigned(FSearchEdit) then
  begin
    FSearchEdit.LeftButton.ImageIndex  := ASuiteManager.IconsManager.GetIconIndex('search_type');
    FSearchEdit.RightButton.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('search');
  end;
end;

function TSearchController.GetCheckedMenuItem(AMenu: TPopupMenu): TMenuItem;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(AMenu) then
    Exit;
  for I := 0 to AMenu.Items.Count - 1 do
    if AMenu.Items[I].Checked then
      Exit(AMenu.Items[I]);
  if AMenu.Items.Count > 0 then
    Result := AMenu.Items[0];
end;

procedure TSearchController.DoSearchItem(const Keyword: string; const SearchType: TSearchType);
var
  LauncherSearch: TLauncherSearch;
begin
  if not Assigned(FTreeSearch) then
    Exit;

  FTreeSearch.Clear;
  if Length(Keyword) > 0 then
  begin
    FTreeSearch.BeginUpdate;
    try
      LauncherSearch.Tree       := FTreeSearch;
      LauncherSearch.Keyword    := LowerCase(Keyword);
      LauncherSearch.SearchType := SearchType;
      ASuiteInstance.MainTree.IterateSubtree(nil, TVirtualTreeMethods.FindNode, @LauncherSearch, [], True);
    finally
      FTreeSearch.EndUpdate;
      TVirtualTreeMethods.CheckVisibleNodePathExe(FTreeSearch);
    end;
  end;
end;

procedure TSearchController.OnTextChange(Sender: TObject);
var
  mi: TMenuItem;
begin
  if not Assigned(FSearchEdit) then Exit;
  if Config.SearchAsYouType then
  begin
    if FSearchEdit.Text <> '' then
      FSearchEdit.RightButton.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('cancel')
    else
      FSearchEdit.RightButton.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('search');

    mi := GetCheckedMenuItem(FSearchMenu);
    if Assigned(mi) then
      DoSearchItem(FSearchEdit.Text, TSearchType(mi.Tag));
  end;
end;

procedure TSearchController.OnRightButtonClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  if not Assigned(FSearchEdit) then Exit;
  if Config.SearchAsYouType then
    FSearchEdit.Text := ''
  else
  begin
    mi := GetCheckedMenuItem(FSearchMenu);
    if Assigned(mi) then
      DoSearchItem(FSearchEdit.Text, TSearchType(mi.Tag));
  end;
end;

procedure TSearchController.OnLeftButtonClick(Sender: TObject);
begin
  if Assigned(FSearchMenu) then
    FSearchMenu.PopUp;
end;

procedure TSearchController.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    OnRightButtonClick(Sender);
end;

procedure TSearchController.Execute(const Keyword: string; const SearchType: TSearchType);
begin
  DoSearchItem(Keyword, SearchType);
end;

procedure TSearchController.ExecuteOn(ATree: TBaseVirtualTree; const Keyword: string; const SearchType: TSearchType);
begin
  FTreeSearch := ATree;
  DoSearchItem(Keyword, SearchType);
end;

end.
