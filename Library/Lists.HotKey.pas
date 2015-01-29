{
Copyright (C) 2006-2015 Matteo Salvi

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

interface

uses
  Classes, NodeDataTypes.Custom, Menus, SysUtils,
  Windows, Kernel.Enumerations, NodeDataTypes.Base,
  DateUtils, Dialogs, UITypes, Lists.Base;

type
  THotkeyItemsList = class(TBaseItemsList)
  public
    function AddItem(AItem: TvCustomRealNodeData): Integer; override;
    function RemoveItem(AItem: TvCustomRealNodeData): Integer; override;

    procedure Clear; override;

    function IndexOfID(ID: Integer): TvCustomRealNodeData;
    procedure RefreshRegs;
  end;

implementation

uses
  AppConfig.Main, Utility.System;

function THotkeyItemsList.AddItem(AItem: TvCustomRealNodeData): Integer;
begin
  Result := -1;
  if (AItem.ID = -1) then
    Exit;

  Result := inherited;
  if Config.HotKey then
    RegisterHotKeyEx(AItem.ID, AItem.Hotkey);
end;

procedure THotkeyItemsList.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    UnregisterHotKeyEx(FItems[I].ID);
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

procedure THotkeyItemsList.RefreshRegs;
var
  I: Integer;
  NodeData: TvCustomRealNodeData;
begin
  //This method unregister and register hotkey again for every item
  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[I].DataType <> vtdtSeparator then
    begin
      NodeData := TvCustomRealNodeData(FItems[I]);
      UnregisterHotKeyEx(NodeData.ID);
      if Config.HotKey then
        RegisterHotKeyEx(NodeData.ID, NodeData.Hotkey);
    end;
  end;
end;

function THotkeyItemsList.RemoveItem(AItem: TvCustomRealNodeData): Integer;
begin
  Result := inherited;
  if (Config.HotKey) and (AItem.ID <> -1) then
    UnregisterHotKeyEx(AItem.ID);
end;

end.
