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

unit Kernel.Logger;

interface

uses
  SysUtils, SynLog, SynCommons;

type
  TASuiteLogger = Class
  public
    constructor Create;

    class procedure Info(const AText: string; ASynLog: ISynLog = nil);
    class procedure Debug(const AText: string; ASynLog: ISynLog = nil);
    class procedure Error(const AText: string; ASynLog: ISynLog = nil);
    class procedure LastError(const AText: string; ASynLog: ISynLog = nil);

    class function Enter(const AMethodName: PUTF8Char; AInstance: TObject): ISynLog;
  end;

implementation

{ TASuiteLogger }

constructor TASuiteLogger.Create;
begin
  with TSynLog.Family do
  begin
    Level := LOG_VERBOSE;
    RotateFileCount := 1;
    RotateFileDailyAtHour := 0;
  end;
end;

class procedure TASuiteLogger.Debug(const AText: string; ASynLog: ISynLog);
begin
  if Assigned(ASynLog) then
    ASynLog.Log(sllDebug, AText)
  else
    TSynLog.Add.Log(sllDebug, AText);
end;

class function TASuiteLogger.Enter(const AMethodName: PUTF8Char; AInstance: TObject): ISynLog;
begin
  Result := TSynLog.Enter(AInstance, AMethodName);
end;

class procedure TASuiteLogger.Error(const AText: string; ASynLog: ISynLog);
begin
  if Assigned(ASynLog) then
    ASynLog.Log(sllError, AText)
  else
    TSynLog.Add.Log(sllError, AText);
end;

class procedure TASuiteLogger.Info(const AText: string; ASynLog: ISynLog);
begin
  if Assigned(ASynLog) then
    ASynLog.Log(sllInfo, AText)
  else
    TSynLog.Add.Log(sllInfo, AText);
end;

class procedure TASuiteLogger.LastError(const AText: string; ASynLog: ISynLog);
begin
  if Assigned(ASynLog) then
    ASynLog.Log(sllLastError, AText)
  else
    TSynLog.Add.Log(sllLastError, AText);
end;

end.
