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

unit NodeDataTypes.Custom;

interface

uses
  VirtualTrees, SysUtils, Kernel.Enumerations, NodeDataTypes.Base,
  DateUtils, Menus, Classes;

type
  TvCustomRealNodeData = class(TvBaseNodeData)
  private
    FLastAccess  : Int64;
    FPathIcon    : String;
    FCacheID     : Integer;
    FCacheLargeID  : Integer;
    FWindowState : Integer;
    FActionOnExe : TActionOnExecute;
    FAutorun     : TAutorunType;
    FAutorunPos  : Integer; //Position for ASuiteStartUpApp and ASuiteShutdownApp
    FSchMode     : TSchedulerMode; //0 Disabled, 1 Once, 2 Hourly, 3 Daily, 4 Weekly
    FSchDateTime : TDateTime;
    FActiveHotkey  : Boolean;
    FHotkey        : TShortcut;
    procedure SetPathIcon(value:string);
    procedure SetAutorun(value: TAutorunType);
    procedure SetSchMode(value: TSchedulerMode);
    procedure SetSchDateTime(value: TDateTime);
    procedure SetActiveHotkey(const Value: Boolean);
    function GetPathCacheIcon: string;
    function GetPathCacheLargeIcon: string;
    function GetPathAbsoluteIcon: String;
    procedure SetLastAccess(const Value: Int64);
  public
    constructor Create(AType: TvTreeDataType); // virtual;
    procedure Copy(source:TvBaseNodeData); override;
    function Execute(Tree: TBaseVirtualTree): boolean; virtual;

    property LastAccess: Int64 read FLastAccess write SetLastAccess;
    property PathIcon: string read FPathIcon write SetPathIcon;
    property PathAbsoluteIcon: String read GetPathAbsoluteIcon;
    property CacheID: Integer read FCacheID write FCacheID;
    property CacheLargeID: Integer read FCacheLargeID write FCacheLargeID;
    property PathCacheIcon: string read GetPathCacheIcon;
    property PathCacheLargeIcon: string read GetPathCacheLargeIcon;
    property WindowState: Integer read FWindowState write FWindowState;
    property ActionOnExe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property Autorun: TAutorunType read FAutorun write SetAutorun;
    property AutorunPos: Integer read FAutorunPos write FAutorunPos;
    property SchMode: TSchedulerMode read FSchMode write SetSchMode;
    property SchDateTime: TDateTime read FSchDateTime write SetSchDateTime;
    property ActiveHotkey: Boolean read FActiveHotkey write SetActiveHotkey;
    property Hotkey: TShortcut read FHotkey write FHotkey;
  end;
  PvCustomRealNodeData = ^TvCustomRealNodeData;

implementation

uses
  AppConfig.Main, Lists.Manager, Kernel.Consts, VirtualTree.Methods, NodeDataTypes.Files;

procedure TvCustomRealNodeData.Copy(source: TvBaseNodeData);
var
  SourceNodeData : TvCustomRealNodeData;
begin
  inherited;
  if source is TvCustomRealNodeData then
  begin
    SourceNodeData := TvCustomRealNodeData(source);
    //Copy from source
    SetPathIcon(SourceNodeData.PathIcon);
    FWindowState  := SourceNodeData.WindowState;
    FActionOnExe  := SourceNodeData.ActionOnExe;
    FHotkey       := SourceNodeData.Hotkey;
    SetActiveHotkey(SourceNodeData.ActiveHotkey);
    SetAutorun(SourceNodeData.Autorun);
    SetSchMode(SourceNodeData.SchMode);
    SetSchDateTime(SourceNodeData.SchDateTime);
  end;
end;

constructor TvCustomRealNodeData.Create(AType: TvTreeDataType);
begin
  inherited;
  FLastAccess  := -1;
  FPathIcon    := '';
  FSchMode     := smDisabled;
  FSchDateTime := Now;
  FActiveHotkey := False;
  FHotkey      := 0;
  FWindowState := 0;
  FActionOnExe := aeDefault;
  FAutorun     := atNever;
  FAutorunPos  := 0;
end;

procedure TvCustomRealNodeData.SetActiveHotkey(const Value: Boolean);
begin
  //Old value is true, remove it in HotKeyApp
  if (FActiveHotkey) then
    Config.ListManager.HotKeyItemList.RemoveItem(Self);
  //New value is true, add it in HotKeyApp
  if (value) then
    Config.ListManager.HotKeyItemList.AddItem(Self);
  FActiveHotkey := Value;
end;

procedure TvCustomRealNodeData.SetLastAccess(const Value: Int64);
begin
  FLastAccess := Value;
  if (Config.ASuiteState <> lsImporting) then
    if Self is TvFileNodeData then
      if (FLastAccess > -1) and (not TvFileNodeData(Self).NoMRU) then
        Config.ListManager.MRUList.AddItem(Self);
end;

procedure TvCustomRealNodeData.SetSchDateTime(value: TDateTime);
begin
  //If value is not a empty TDateTime, set it in FSchDateTime
  if value <> 0 then
    FSchDateTime := value
  else //Else use function Now
    FSchDateTime := Now;
end;

procedure TvCustomRealNodeData.SetPathIcon(value:string);
begin
  FPathIcon := value;
end;

procedure TvCustomRealNodeData.SetSchMode(value: TSchedulerMode);
begin
  if (Config.ASuiteState <> lsImporting) then
    if (FSchMode <> value) then
    begin
      if (FSchMode <> smDisabled) and (value = smDisabled) then
        Config.ListManager.SchedulerItemList.RemoveItem(Self);
      if (FSchMode = smDisabled) and (value <> smDisabled) then
        Config.ListManager.SchedulerItemList.AddItem(Self);
    end;
  FSchMode := value;
end;

function TvCustomRealNodeData.Execute(Tree: TBaseVirtualTree): boolean;
begin
  Assert(Assigned(Tree), 'Tree is not assigned!');

  LastAccess := DateTimeToUnix(Now);
  Self.Changed := True;
  TVirtualTreeMethods.Create.RefreshList(Tree);
  Result := True;
end;

function TvCustomRealNodeData.GetPathAbsoluteIcon: String;
begin
  Result := Config.Paths.RelativeToAbsolute(FPathIcon);
end;

function TvCustomRealNodeData.GetPathCacheIcon: string;
begin
  if FCacheID <> -1 then
    Result := Config.Paths.SuitePathCache + IntToStr(FCacheID) + EXT_ICO
  else
    Result := '';
end;

function TvCustomRealNodeData.GetPathCacheLargeIcon: string;
begin
  if FCacheLargeID <> -1 then
    Result := Config.Paths.SuitePathCacheLarge + IntToStr(FCacheLargeID) + EXT_ICO
  else
    Result := '';
end;

procedure TvCustomRealNodeData.SetAutorun(value:TAutorunType);
begin
  if (Config.ASuiteState <> lsImporting) then
  begin
    //If it is changed, remove from old list and insert in new list
    if (value > atNever) and ((FAutorun) <> (value)) then
    begin
      if (FAutorun in [atAlwaysOnStart, atSingleInstance, atNever]) and (value in [atAlwaysOnClose]) then
      begin
        Config.ListManager.StartupItemList.RemoveItem(Self);
        Config.ListManager.ShutdownItemList.InsertItem(Self.FAutorunPos, Self)
      end
      else
        if (FAutorun in [atAlwaysOnClose, atNever]) and (value in [atAlwaysOnStart, atSingleInstance]) then
        begin
          Config.ListManager.ShutdownItemList.RemoveItem(Self);
          Config.ListManager.StartupItemList.InsertItem(Self.FAutorunPos, Self);
        end;
    end
    else begin
      //If it is changed, RemoveItem from old list
      if (FAutorun in [atAlwaysOnStart, atSingleInstance]) and (value in [atNever]) then
        Config.ListManager.StartupItemList.RemoveItem(Self)
      else
        if (FAutorun in [atAlwaysOnClose]) and (value in [atNever]) then
          Config.ListManager.ShutdownItemList.RemoveItem(Self);
    end;
  end;
  //Set new value
  FAutorun := value;
end;

end.
