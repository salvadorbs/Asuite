unit Utility.MenuUtils;

{$MODE DelphiUnicode}

interface

uses Menus;

procedure PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);

implementation

procedure PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);
var
  I: Integer;
  MenuItem: TMenuItem;
const
  cLineCaption = '-';
begin
  if not Assigned(APopupMenu) or not Assigned(AParentMenuItem) then Exit;

  APopupMenu.Clear;
  for I := 0 to AParentMenuItem.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(APopupMenu);
    if Assigned(AParentMenuItem.Items[I].Action) then
      MenuItem.Action := AParentMenuItem.Items[I].Action
    else
      MenuItem.Caption := cLineCaption;
    // TODO: add nested levels if needed
    APopupMenu.Add(MenuItem);
  end;
end;

end.
