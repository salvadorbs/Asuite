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

unit Lists.Manager;

interface

uses
  Classes, Menus, SysUtils, Kernel.Singleton, Lists.Special,
  Lists.Base, Lists.Scheduler, Lists.HotKey;

type
  TListManager = class(TSingleton)
  private
    FMRUList : TSpecialItemsList;
    FMFUList : TSpecialItemsList;
    FStartupItemList   : TBaseItemsList; //Software in StartUp list
    FShutdownItemList  : TBaseItemsList; //Software in Shutdown list
    FSchedulerItemList : TSchedulerItemsList;
    FHotKeyItemList    : THotkeyItemsList;
  public
    procedure Initialize; override;
    procedure Finalize; override;

    property MRUList: TSpecialItemsList read FMRUList write FMRUList;
    property MFUList: TSpecialItemsList read FMFUList write FMFUList;
    property StartupItemList: TBaseItemsList read FStartupItemList write FStartupItemList;
    property ShutdownItemList: TBaseItemsList read FShutdownItemList write FShutdownItemList;
    property SchedulerItemList: TSchedulerItemsList read FSchedulerItemList write FSchedulerItemList;
    property HotKeyItemList: THotkeyItemsList read FHotKeyItemList write FHotKeyItemList;
  end;

implementation

uses
  Windows, Kernel.Enumerations;

{ TLauncherLists }

procedure TListManager.Initialize;
begin
  //Create special list
  FMRUList := TSpecialItemsList.Create(lmMRU);
  FMFUList := TSpecialItemsList.Create(lmMFU);
  //Create TNodeLists for autorun
  FStartupItemList   := TBaseItemsList.Create;
  FShutdownItemList  := TBaseItemsList.Create;
  FSchedulerItemList := TSchedulerItemsList.Create;
  FHotKeyItemList    := THotkeyItemsList.Create;
end;

procedure TListManager.Finalize;
begin
  FreeAndNil(FMRUList);
  FreeAndNil(FMFUList);
  FreeAndNil(FStartupItemList);
  FreeAndNil(FShutdownItemList);
  FreeAndNil(FSchedulerItemList);
  FreeAndNil(FHotKeyItemList);
  inherited;
end;

end.