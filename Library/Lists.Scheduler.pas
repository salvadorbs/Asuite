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

unit Lists.Scheduler;

interface

uses
  Classes, NodeDataTypes.Custom, Menus, SysUtils,
  Windows, Kernel.Enumerations,
  DateUtils, Dialogs, UITypes, Lists.Base;

type
  TSchedulerItemsList = class(TBaseItemsList)
  public
    procedure CheckMissedTasks;
  end;

implementation

uses
  AppConfig.Main, Forms.Main;

procedure TSchedulerItemsList.CheckMissedTasks;
var
  I: Integer;
  NodeData: TvCustomRealNodeData;
  dtScheduler, dtNowDateTime, dtLastAccess: TDateTime;
begin
  dtNowDateTime := RecodeMilliSecond(Now,0);
  dtNowDateTime := RecodeSecond(dtNowDateTime, 0);
  dtScheduler   := 0;
  if (Config.Scheduler) then
  begin
    for I := 0 to Self.Count - 1 do
    begin
      NodeData := ListManager.SchedulerItemList[I];
      dtLastAccess := UnixToDateTime(NodeData.LastAccess);
      case NodeData.SchMode of
        smDisabled: dtScheduler := 0;
        smOnce:
        begin
          //Check if dtScheduler is already passed
          dtScheduler := NodeData.SchDateTime;
          if CompareDateTime(dtNowDateTime, NodeData.SchDateTime) = -1 then
            dtScheduler := 0;
        end;
        smHourly:
        begin
          //Run software every hour
          dtScheduler := RecodeMinute(dtNowDateTime, 0);
          dtScheduler := RecodeSecond(dtScheduler, 0);
        end;
        smDaily:
        begin
          //Run software every day (user choose time, hour and minute)
          dtScheduler := RecodeYear(NodeData.SchDateTime, YearOf(dtNowDateTime));
          dtScheduler := RecodeMonth(dtScheduler, MonthOf(dtNowDateTime));
          dtScheduler := RecodeDay(dtScheduler, DayOf(dtNowDateTime));
          //Check if dtScheduler is already passed
          if CompareDateTime(dtNowDateTime, dtScheduler) = -1 then
            dtScheduler := IncDay(dtScheduler, -1);
        end;
      end;
    end;
  end;
end;

end.
