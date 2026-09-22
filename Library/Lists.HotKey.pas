{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Lists.HotKey;

{$MODE DelphiUnicode}

interface

uses
  NodeDataTypes.Custom, SysUtils, Classes, Hotkeys.Manager.Platform,
  Hotkeys.Manager, LCLIntf, LCLType, Lists.Base;

type
  THotkeyItemsList = class(TBaseItemsList)
  private
    { Called when the backend (Wayland portal) reports the trigger it actually
      assigned to an action. ASuite uses IntToStr(Tag) as the stable ActionId,
      so the action can be identified even when the desktop reassigned the
      shortcut. The value is stored without re-registering anything.
      ActionId is AnsiString because the component is compiled in Delphi mode
      (AnsiString), while ASuite uses DelphiUnicode. }
    procedure HotkeyTriggerChangedEx(Sender: TObject; const ActionId: AnsiString;
      Tag: Integer; Trigger: TShortCut);
  public
    constructor Create;
    destructor Destroy; override;

    function AddItem(AItem: TvCustomRealNodeData): Integer; override;
    function RemoveItem(AItem: TvCustomRealNodeData): Integer; override;

    procedure Clear; override;

    function IndexOfID(ID: Integer): TvCustomRealNodeData;
    procedure RefreshRegs;

    { Groups every registration/removal between BeginUpdate and EndUpdate into
      a single backend update. On Wayland the portal binds the whole set only
      once instead of once per item. }
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

implementation

uses
  AppConfig.Main, VirtualTree.Methods, Kernel.Consts, Kernel.Logger;

constructor THotkeyItemsList.Create;
begin
  inherited Create;
  HotkeyManager.OnTriggerChangedEx := HotkeyTriggerChangedEx;
end;

destructor THotkeyItemsList.Destroy;
begin
  if Assigned(InternalManager) then
    InternalManager.OnTriggerChangedEx := nil;
  inherited Destroy;
end;

procedure THotkeyItemsList.HotkeyTriggerChangedEx(Sender: TObject;
  const ActionId: AnsiString; Tag: Integer; Trigger: TShortCut);
var
  NodeData: TvCustomRealNodeData;
  Changed: Boolean;
  ActionTag: Integer;
begin
  // 0 means the desktop reported a trigger we cannot represent: keep the
  // value ASuite had configured.
  if (not Config.HotKey) or (Trigger = 0) then
    Exit;

  // ASuite registers each action with ActionId = IntToStr(Tag), so the
  // ActionId is authoritative and lets the action be resolved even if the
  // callback Tag is not meaningful for every backend.
  ActionTag := StrToIntDef(ActionId, Tag);

  Changed := False;
  case ActionTag of
    frmMainID, frmGMenuID, frmCMenuID:
      Changed := Config.UpdateLauncherHotkeyFromDesktop(ActionTag, Trigger);
  else
    NodeData := IndexOfID(ActionTag);
    if (NodeData <> nil) and (NodeData.Hotkey <> Trigger) then
    begin
      NodeData.UpdateHotkeyFromDesktop(Trigger);
      Changed := True;
    end;
  end;

  if Changed then
    Config.Changed := True;
end;

function THotkeyItemsList.AddItem(AItem: TvCustomRealNodeData): Integer;
begin
  Result := -1;
  if (AItem.ID = -1) then
    Exit;

  Result := inherited;
  if Config.HotKey then
  begin
    // The item ID is stable in the database, so it is the natural ActionId:
    // changing the shortcut keeps the same desktop action and the user's
    // configuration. The result is the bind verdict: log it when the backend
    // refused the registration instead of silently dropping it.
    if not HotkeyManager.RegisterNotifyEx(AItem.Hotkey,
      TVirtualTreeMethods.HotKeyNotify, AItem.ID, IntToStr(AItem.ID)) then
      TASuiteLogger.Error('Failed to register hotkey for item "%s" (id %d)',
        [AItem.Name, AItem.ID]);
  end;
end;

procedure THotkeyItemsList.Clear;
var
  I: Integer;
begin 
  for I := 0 to FItems.Count - 1 do
    HotkeyManager.UnregisterNotify(TvCustomRealNodeData(FItems[I]).Hotkey);

  inherited;
end;

function THotkeyItemsList.IndexOfID(ID: Integer): TvCustomRealNodeData;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[I].ID = ID then
    begin
      Result := TvCustomRealNodeData(FItems[I]);
      Exit;
    end;
  end;
end;

procedure THotkeyItemsList.BeginUpdate;
begin
  HotkeyManager.BeginHotkeyUpdate;
end;

procedure THotkeyItemsList.EndUpdate;
begin
  HotkeyManager.EndHotkeyUpdate;
end;

procedure THotkeyItemsList.RefreshRegs;
var
  I: Integer;
  NodeData: TvCustomRealNodeData;
begin
  //This method re-applies the hotkey of every item as a single backend
  //update. On X11/Windows RefreshNotify re-registers each shortcut; on the
  //Wayland portal it is a no-op and EndUpdate does not rebind.
  BeginUpdate;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      if not(FItems[I].IsSeparatorItem) then
      begin
        NodeData := TvCustomRealNodeData(FItems[I]);

        HotkeyManager.RefreshNotify(NodeData.Hotkey);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function THotkeyItemsList.RemoveItem(AItem: TvCustomRealNodeData): Integer;
begin
  Result := inherited;
  if (Config.HotKey) and (AItem.ID <> -1) then
    HotkeyManager.UnregisterNotify(AItem.Hotkey);
end;

end.
