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
  SysUtils, Kernel.Enumerations, NodeDataTypes.Base,
  DateUtils, Menus, Classes, Kernel.Types;

type
  TvCustomRealNodeData = class(TvBaseNodeData)
  private
    FPathIcon    : String;
    FWindowState : Integer;
    FActionOnExe : TActionOnExecute;
    FAutorun     : TAutorunType;
    FAutorunPos  : Integer; //Position for ASuiteStartUpApp and ASuiteShutdownApp
    FSchMode     : TSchedulerMode; //0 Disabled, 1 Once, 2 Hourly, 3 Daily, 4 Weekly
    FSchDateTime : TDateTime;
    FActiveHotkey : Boolean;
    FHotkey      : TShortcut;
    FLastAccess  : Int64;

    procedure SetAutorun(value: TAutorunType);
    procedure SetSchMode(value: TSchedulerMode);
    procedure SetSchDateTime(value: TDateTime);
    procedure SetActiveHotkey(const Value: Boolean);
    function GetPathAbsoluteIcon: String;
  protected
    procedure SetLastAccess(const Value: Int64); virtual;
    procedure AfterExecute(ADoActionOnExe: Boolean); virtual;

    function InternalExecute(ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean; virtual; abstract;
    function InternalExecuteAsUser(ARunFromCategory: Boolean; AUserData: TUserData): boolean; virtual; abstract;
    function InternalExecuteAsAdmin(ARunFromCategory: Boolean): boolean; virtual; abstract;
  public
    constructor Create(AType: TvTreeDataType);
    procedure Copy(source:TvBaseNodeData); override;

    function Execute(ADoActionOnExe: Boolean; ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean;
    function ExecuteAsUser(ADoActionOnExe: Boolean; ARunFromCategory: Boolean;
      AUserData: TUserData): boolean;
    function ExecuteAsAdmin(ADoActionOnExe: Boolean; ARunFromCategory: Boolean): boolean;

    property LastAccess: Int64 read FLastAccess write SetLastAccess;
    property PathIcon: string read FPathIcon write FPathIcon;
    property PathAbsoluteIcon: String read GetPathAbsoluteIcon;
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
  AppConfig.Main, Lists.Manager, VirtualTree.Methods, Kernel.Logger,
  Utility.Process;

procedure TvCustomRealNodeData.Copy(source: TvBaseNodeData);
var
  SourceNodeData : TvCustomRealNodeData;
begin
  inherited;
  if source is TvCustomRealNodeData then
  begin
    SourceNodeData := TvCustomRealNodeData(source);
    //Copy from source
    FPathIcon     := SourceNodeData.PathIcon;
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
  FPathIcon    := '';
  FSchMode     := smDisabled;
  FSchDateTime := Now;
  FActiveHotkey := False;
  FHotkey      := 0;
  FWindowState := 0;
  FActionOnExe := aeDefault;
  FAutorun     := atNever;
  FAutorunPos  := 0;
  FLastAccess  := -1;
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

procedure TvCustomRealNodeData.SetSchDateTime(value: TDateTime);
begin
  //If value is not a empty TDateTime, set it in FSchDateTime
  if value <> 0 then
    FSchDateTime := value
  else //Else use function Now
    FSchDateTime := Now;
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

procedure TvCustomRealNodeData.AfterExecute(ADoActionOnExe: Boolean);
begin
  //Run action after execution
  if ADoActionOnExe then
    RunActionOnExe(Self.ActionOnExe);
  //Save changes
  Self.Changed := True;
end;

function TvCustomRealNodeData.GetPathAbsoluteIcon: String;
begin
  Result := Config.Paths.RelativeToAbsolute(FPathIcon);
end;

function TvCustomRealNodeData.Execute(ADoActionOnExe: Boolean;
  ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean;
begin
  TASuiteLogger.Info('Execute Node (%s)', [Self.Name]);
  Result := False;
  try
    Result := InternalExecute(ARunFromCategory, ACheckSingleInstance);
  finally
    if Result then
      AfterExecute(ADoActionOnExe)
  end;
end;

function TvCustomRealNodeData.ExecuteAsAdmin(ADoActionOnExe: Boolean;
  ARunFromCategory: Boolean): boolean;
begin
  TASuiteLogger.Info('Execute Node (%s) as Admin', [Self.Name]);
  Result := False;
  try
    Result := InternalExecuteAsAdmin(ARunFromCategory);
  finally
    if Result then
      AfterExecute(ADoActionOnExe)
  end;
end;

function TvCustomRealNodeData.ExecuteAsUser(ADoActionOnExe: Boolean;
  ARunFromCategory: Boolean; AUserData: TUserData): boolean;
begin
  TASuiteLogger.Info('Execute Node (%s) as user', [Self.Name]);
  Result := False;
  try
    Result := InternalExecuteAsUser(ARunFromCategory, AUserData);
  finally
    if Result then
      AfterExecute(ADoActionOnExe)
  end;
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

procedure TvCustomRealNodeData.SetLastAccess(const Value: Int64);
begin
  FLastAccess := Value;
end;

end.
