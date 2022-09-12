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

unit Lists.Manager;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Lists.Special, NodeDataTypes.Custom, Lists.Base, Lists.HotKey,
  Kernel.Enumerations;

type

  { TListManager }

  TListManager = class
  private
    FMRUList : TSpecialItemsList;
    FMFUList : TSpecialItemsList;
    FStartupItemList   : TBaseItemsList; //Software in StartUp list
    FShutdownItemList  : TBaseItemsList; //Software in Shutdown list
    FSchedulerItemList : TBaseItemsList;
    FHotKeyItemList    : THotkeyItemsList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveItemFromLists(AItemData: TvCustomRealNodeData);
    procedure ExecuteAutorunList(AutorunListMode: TAutorunListMode);

    property MRUList: TSpecialItemsList read FMRUList write FMRUList;
    property MFUList: TSpecialItemsList read FMFUList write FMFUList;
    property StartupItemList: TBaseItemsList read FStartupItemList write FStartupItemList;
    property ShutdownItemList: TBaseItemsList read FShutdownItemList write FShutdownItemList;
    property SchedulerItemList: TBaseItemsList read FSchedulerItemList write FSchedulerItemList;
    property HotKeyItemList: THotkeyItemsList read FHotKeyItemList write FHotKeyItemList;
  end;

implementation

uses
  LCLIntf, LCLType, AppConfig.Main, VirtualTree.Methods, Kernel.Logger, Kernel.Instance;

{ TLauncherLists }

constructor TListManager.Create;
begin
  //Create special list
  FMRUList := TSpecialItemsList.Create(lmMRU);
  FMFUList := TSpecialItemsList.Create(lmMFU);

  //Create TNodeLists for autorun
  FStartupItemList   := TBaseItemsList.Create;
  FShutdownItemList  := TBaseItemsList.Create;
  FSchedulerItemList := TBaseItemsList.Create;
  FHotKeyItemList    := THotkeyItemsList.Create;
end;

destructor TListManager.Destroy;
begin
  FreeAndNil(FMRUList);
  FreeAndNil(FMFUList);
  FreeAndNil(FStartupItemList);
  FreeAndNil(FShutdownItemList);
  FreeAndNil(FSchedulerItemList);
  FreeAndNil(FHotKeyItemList);

  inherited;
end;

procedure TListManager.RemoveItemFromLists(AItemData: TvCustomRealNodeData);
begin
  //Remove item from special lists
  FMRUList.RemoveItem(AItemData);
  FMFUList.RemoveItem(AItemData);
  //Remove item from hotkey list
  if AItemData.ActiveHotkey then
    FHotKeyItemList.RemoveItem(AItemData);
  //Remove item from scheduler list
  if AItemData.SchMode <> smDisabled then
    FSchedulerItemList.RemoveItem(AItemData);
  //Remove item from autorun lists
  if (AItemData.Autorun in [atAlwaysOnStart, atSingleInstance]) then
    FStartupItemList.RemoveItem(AItemData);
  if (AItemData.Autorun in [atAlwaysOnClose]) then
    FShutdownItemList.RemoveItem(AItemData);
end;

procedure TListManager.ExecuteAutorunList(AutorunListMode: TAutorunListMode);
var
  List : TBaseItemsList;
  I    : Integer;
  StartTime: Cardinal;
begin
  StartTime := TASuiteLogger.EnterMethod('TListManager.ExecuteAutorunList', Self);
  try
    case AutorunListMode of
      amStartup: TASuiteLogger.Info('Execute Autorun List (Startup)', []);
      amShutdown: TASuiteLogger.Info('Execute Autorun List (Shutdown)', []);
    end;
    List := nil;
    if (Config.AutorunStartup) or (Config.AutorunShutdown) then
    begin
      case AutorunListMode of
        amStartup:
          begin
            if Config.AutorunStartup then
              List := FStartupItemList;
          end;
        amShutdown:
          begin
            if Config.AutorunShutdown then
              List := FShutdownItemList;
          end;
      end;

      if Assigned(List) then
      begin
        for I := 0 to List.Count - 1 do
          TVirtualTreeMethods.ExecuteNode(ASuiteInstance.MainTree, List[I].pNode, rmNormal, True);
      end;
    end;
  finally
    TASuiteLogger.ExitMethod('TListManager.ExecuteAutorunList', Self, StartTime);
  end;
end;

end.
