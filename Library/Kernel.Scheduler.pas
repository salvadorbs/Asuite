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

unit Kernel.Scheduler;

interface

uses
  Classes, Kernel.Singleton, ExtCtrls, System.UITypes, DateUtils, SysUtils,
  Dialogs, DKLang, NodeDataTypes.Custom;

type
  TScheduler = class(TSingleton)
  private
    FTimer: TTimer;

    procedure DoTimer(Sender: TObject);
    function GetSchedulerTime(ANodeData: TvCustomRealNodeData; ANow: TDateTime; ADecDay: Boolean): TDateTime;
  public
    procedure Initialize; override;
    procedure Finalize; override;

    procedure CheckMissedTasks;

    property Timer: TTimer read FTimer write FTimer;
  end;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, VirtualTree.Methods;

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
    for I := 0 to Config.ListManager.SchedulerItemList.Count - 1 do
    begin
      NodeData := Config.ListManager.SchedulerItemList[I];
      if Assigned(NodeData) then
      begin
        dtLastAccess := UnixToDateTime(NodeData.LastAccess);
        //Check if last task is missed
        if (CompareDateTime(GetSchedulerTime(NodeData, dtNowDateTime, True), dtLastAccess) = 1) and (NodeData.SchMode <> smDisabled) then
        begin
          //Start process
          if (MessageDlg(Format(DKLangConstW('msgMissedTask'), [NodeData.Name]), mtWarning, [mbYes, mbNo], 0) = mrYes) then
            NodeData.Execute(True, NodeData.DataType = vtdtCategory, False);
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
  for I := 0 to Config.ListManager.SchedulerItemList.Count - 1 do
  begin
    NodeData := Config.ListManager.SchedulerItemList[I];
    if Assigned(NodeData) then
    begin
      //Compare time and/or date based of scheduler mode and run node
      if (CompareDateTime(GetSchedulerTime(NodeData, dtNowDateTime, False), dtNowDateTime) = 0) and (NodeData.SchMode <> smDisabled) then
        NodeData.Execute(True, NodeData.DataType = vtdtCategory, False);
    end;
  end;
end;

procedure TScheduler.Finalize;
begin
  inherited;
  FTimer.Free;
end;

function TScheduler.GetSchedulerTime(ANodeData: TvCustomRealNodeData; ANow: TDateTime; ADecDay: Boolean): TDateTime;
begin
  Result := 0;
  if Assigned(ANodeData) then
  begin
    case ANodeData.SchMode of
      smDisabled: Result := 0;
      smOnce: Result := ANodeData.SchDateTime;
      smHourly:
      begin
        //Run software every hour
        Result := RecodeMinute(ANow, 0);
        Result := RecodeSecond(Result, 0);
      end;
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

procedure TScheduler.Initialize;
begin
  inherited;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
end;

end.
