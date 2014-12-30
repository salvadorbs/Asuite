{
Copyright (C) 2006-2013 Matteo Salvi

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

unit Utility.Process;

interface

uses
  Windows, SysUtils, Kernel.Enumerations, Forms.Main, AppConfig.Main, TlHelp32;

{ Processes, execution }
procedure ActionOnExe(Action: TActionOnExecute);
function  IsProcessExists(exeFileName: string): Boolean;
procedure RunActionOnExe(Action: TActionOnExecute);

{ Windows Api }
function CreateProcessWithLogonW(
  Username          : PWideChar;
  Domain            : PWideChar;
  Password          : PWideChar;
  LogonFlags        : DWORD;
  ApplicationName   : PWideChar;
  CommandLine       : LPWSTR;
  CreationFlags     : DWORD;
  Environment       : Pointer;
  CurrentDirectory  : PWideChar;
  const StartupInfo : TStartupInfoW;
  var ProcessInfo   : TProcessInformation): BOOL; stdcall;
  external 'advapi32.dll' name 'CreateProcessWithLogonW';

const
  LOGON_WITH_PROFILE = $00000001;

implementation

procedure ActionOnExe(Action: TActionOnExecute);
begin
  case Action of
    aeRunAndHide:
    begin
      //Hide frmMain
      frmMain.Close;
    end;
    aeRunAndClose:
    begin
      //Close application
      Config.ASuiteState := lsShutdown;
      frmMain.Close;
    end;
  end;
end;

function IsProcessExists(exeFileName: string): Boolean;
var
  hSnapShot : THandle;
  ProcInfo  : TProcessEntry32;
begin
  Result      := False;
  exeFileName := UpperCase(ExeFileName);
  hSnapShot   := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  //Check processes
  if (hSnapShot <> THandle(-1)) then
  begin
    ProcInfo.dwSize := SizeOf(ProcInfo);
    //First process
    if (Process32First(hSnapshot, ProcInfo)) then
    begin
      //Compare first process with ExeFileName
      if (UpperCase(ExtractFileName(ProcInfo.szExeFile)) = ExeFileName) then
        Result := True;
      while (Process32Next(hSnapShot, ProcInfo)) do
        if (UpperCase(ExtractFileName(ProcInfo.szExeFile)) = ExeFileName) then
          Result := True;
    end;
  end;
  FileClose(hSnapShot);
end;

procedure RunActionOnExe(Action: TActionOnExecute);
begin
  //If Action is aeDefault, using launcher options
  if Action = aeDefault then
    ActionOnExe(TActionOnExecute(Ord(Config.ActionOnExe) + 1))
  else //Else software options
    ActionOnExe(Action);
end;

end.
