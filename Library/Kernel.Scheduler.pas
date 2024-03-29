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

unit Kernel.Scheduler;

{$MODE DelphiUnicode}

interface

uses
  Classes, ExtCtrls, DateUtils, SysUtils, NodeDataTypes.Custom;

type

  { TScheduler }

  TScheduler = class
  private
    FTimer: TTimer;

    procedure DoTimer(Sender: TObject);
    function CompareSchDates(ANodeData: TvCustomRealNodeData; ANow: TDateTime): Boolean;
    function GetSchedulerTime(ANodeData: TvCustomRealNodeData; ANow: TDateTime; ADecDay: Boolean): TDateTime;
    function CheckOnce(ANow, ASchedDateTime: TDateTime): Boolean;
    function CheckDaily(ANow, ASchedTime: TDateTime): Boolean;
    function CheckHourly(ANow: TDateTime): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CheckMissedTasks;

    property Timer: TTimer read FTimer write FTimer;
  end;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Kernel.ResourceStrings, Kernel.Manager,
  Utility.Misc, Kernel.Logger;

function ResetHourMinute(var ANow: TDateTime): TDateTime;
begin
  Result := RecodeSecond(RecodeMinute(ANow, 0), 0);
end;

{ TScheduler }

procedure TScheduler.CheckMissedTasks;
var
  I: Integer;
  NodeData: TvCustomRealNodeData;
  dtNowDateTime, dtLastAccess: TDateTime;
begin
  dtNowDateTime := RecodeMilliSecond(Now,0);
  dtNowDateTime := RecodeSecond(dtNowDateTime, 0);
  if (Config.Scheduler) then
  begin
    for I := 0 to ASuiteManager.ListManager.SchedulerItemList.Count - 1 do
    begin
      NodeData := ASuiteManager.ListManager.SchedulerItemList[I];
      if Assigned(NodeData) then
      begin
        dtLastAccess := UnixToDateTime(NodeData.LastAccess);
        //Check if last task is missed
        if (CompareDateTime(GetSchedulerTime(NodeData, dtNowDateTime, True), dtLastAccess) = 1) and (NodeData.SchMode <> smDisabled) then
        begin
          //Start process
          if AskUserWarningMessage(msgMissedTask, [NodeData.Name]) then
            NodeData.Execute(True, NodeData.IsCategoryItem, False);
        end;
      end;
    end;
  end;
end;

procedure TScheduler.DoTimer(Sender: TObject);
var
  NodeData : TvCustomRealNodeData;
  I        : Integer;
  dtNowDateTime: TDateTime;
begin
  if (Config.ASuiteState = lsStartUp) or (Config.ASuiteState = lsShutdown) then
    Exit;
  dtNowDateTime := RecodeMilliSecond(Now,0);
  //Check scheduler list to know which items to run
  for I := 0 to ASuiteManager.ListManager.SchedulerItemList.Count - 1 do
  begin
    NodeData := ASuiteManager.ListManager.SchedulerItemList[I];
    if Assigned(NodeData) then
    begin
      //Compare time and/or date based of scheduler mode and run node
      if CompareSchDates(NodeData, dtNowDateTime) and (NodeData.SchMode <> smDisabled) then
        NodeData.Execute(True, NodeData.IsCategoryItem, False);
    end;
  end;
end;

constructor TScheduler.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
end;

destructor TScheduler.Destroy;
begin
  FTimer.Free;

  inherited;
end;

function TScheduler.CompareSchDates(ANodeData: TvCustomRealNodeData; ANow: TDateTime): Boolean;
begin
  Result := False;
  if Assigned(ANodeData) then
  begin
    case ANodeData.SchMode of
      smOnce:
        Result := CheckOnce(ANow, ANodeData.SchDateTime);
      smHourly:
        Result := CheckHourly(ANow);
      smDaily:
        Result := CheckDaily(ANow, ANodeData.SchDateTime);
    end;
  end;
end;

function TScheduler.GetSchedulerTime(ANodeData: TvCustomRealNodeData; ANow: TDateTime; ADecDay: Boolean): TDateTime;
begin
  Result := 0;
  if Assigned(ANodeData) then
  begin
    case ANodeData.SchMode of
      smOnce: Result := ANodeData.SchDateTime;
      smHourly:
        Result := ResetHourMinute(ANow);
      smDaily:
      begin
        //Run software every day (user choose time, hour and minute)
        Result := RecodeYear(ANodeData.SchDateTime, YearOf(ANow));
        Result := RecodeMonth(Result, MonthOf(ANow));
        Result := RecodeDay(Result, DayOf(ANow));
        //Check if dtScheduler is already passed
        if (CompareDateTime(ANow, Result) = -1) and ADecDay then
          Result := IncDay(Result, -1);
      end;
    end;
  end;
end;

function TScheduler.CheckOnce(ANow, ASchedDateTime: TDateTime): Boolean;
begin
  Result := SameDate(ANow, ASchedDateTime) and CheckDaily(ANow, ASchedDateTime);

  if Result then
    TASuiteLogger.Info('Scheduler - Check Once is true', []);
end;

function TScheduler.CheckDaily(ANow, ASchedTime: TDateTime): Boolean;
begin
  Result := (HourOf(ANow) = HourOf(ASchedTime)) and (MinuteOf(ANow) = MinuteOf(ASchedTime))
            and (SecondOf(ANow) = 0);

  if Result then
    TASuiteLogger.Info('Scheduler - Check Daily is true', []);
end;

function TScheduler.CheckHourly(ANow: TDateTime): Boolean;
begin
  Result := (MinuteOf(ANow) = 0) and (SecondOf(ANow) = 0);

  if Result then
    TASuiteLogger.Info('Scheduler - Check Hourly is true', []);
end;

end.
