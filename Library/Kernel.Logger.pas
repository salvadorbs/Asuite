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
  SysUtils, mormot.core.log, Forms, mormot.core.base;

type

  { TASuiteLogger }

  TASuiteLogger = Class
  public
    class procedure Info(const AText: string; AParams: Array of const);
    class procedure Debug(const AText: string; AParams: Array of const);
    class procedure Error(const AText: string; AParams: Array of const);
    class procedure LastError(const AText: string; AParams: Array of const);
    class procedure Exception(E: SysUtils.Exception; const AText: string = '');

    class function Enter(const AMethodName: PUTF8Char; AInstance: TObject): ISynLog;
  end;

  { TEventContainer }

  TEventContainer = class
  public
    procedure HandleApplicationException(Sender: TObject; E: Exception);
  end;

implementation

uses
  Kernel.Instance, Kernel.Manager, Kernel.ResourceStrings,
  Utility.Misc;

{ TEventContainer }

procedure TEventContainer.HandleApplicationException(Sender: TObject;
  E: Exception);
begin
  if not (E is EAbort) then
    TASuiteLogger.Exception(E);
end;

{ TASuiteLogger }

class procedure TASuiteLogger.Debug(const AText: string;
  AParams: array of const);
begin
  TSynLog.Add.Log(sllDebug, Format(AText, AParams));
end;

class function TASuiteLogger.Enter(const AMethodName: PUTF8Char; AInstance: TObject): ISynLog;
begin
  Result := TSynLog.Enter(AInstance, AMethodName);
end;

class procedure TASuiteLogger.Error(const AText: string;
  AParams: array of const);
begin
  TSynLog.Add.Log(sllError, Format(AText, AParams));
end;

class procedure TASuiteLogger.Info(const AText: string;
  AParams: array of const);
begin
  TSynLog.Add.Log(sllInfo, Format(AText, AParams));
end;

class procedure TASuiteLogger.LastError(const AText: string;
  AParams: array of const);
begin
  TSynLog.Add.Log(sllLastError, Format(AText, AParams));
end;

class procedure TASuiteLogger.Exception(E: SysUtils.Exception;
  const AText: string);
begin
  if AText = '' then
    ShowMessageFmtEx(msgErrGeneric, [E.ClassName, E.Message], True)
  else
    ShowMessageEx(AText, True);

  TSynLog.Add.Log(sllStackTrace, E.Message, E);
end;

procedure HandleOnShowException(Msg: ShortString);
begin
  TSynLog.Add.Log(sllStackTrace, Msg);
end;

initialization
  SysUtils.OnShowException := HandleOnShowException;
  Application.OnException := TEventContainer(nil).HandleApplicationException;

end.
