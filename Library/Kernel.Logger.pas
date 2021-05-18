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

unit Kernel.Logger;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, SynLog, SynCommons;

type
  TASuiteLogger = Class
  public
    class procedure Info(const AText: string; AParams: Array of const);
    class procedure Debug(const AText: string; AParams: Array of const);
    class procedure Error(const AText: string; AParams: Array of const);
    class procedure LastError(const AText: string; AParams: Array of const);

    class function Enter(const AMethodName: PUTF8Char; AInstance: TObject): ISynLog;
  end;

implementation

uses
  AppConfig.Main, Kernel.Instance, Kernel.Manager;

{ TASuiteLogger }

class procedure TASuiteLogger.Debug(const AText: string; AParams: Array of const);
begin
  TSynLog.Add.Log(sllDebug, Format(AText, AParams));
end;

class function TASuiteLogger.Enter(const AMethodName: PUTF8Char; AInstance: TObject): ISynLog;
begin
  Result := TSynLog.Enter(AInstance, AMethodName);
end;

class procedure TASuiteLogger.Error(const AText: string; AParams: Array of const);
begin
  TSynLog.Add.Log(sllError, Format(AText, AParams));
end;

class procedure TASuiteLogger.Info(const AText: string; AParams: Array of const);
begin
  TSynLog.Add.Log(sllInfo, Format(AText, AParams));
end;

class procedure TASuiteLogger.LastError(const AText: string; AParams: Array of const);
begin
  TSynLog.Add.Log(sllLastError, Format(AText, AParams));
end;

end.
