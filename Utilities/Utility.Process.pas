{
Copyright (C) 2006-2020 Matteo Salvi

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

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Kernel.Enumerations, Forms.Main, AppConfig.Main,
  SynLog{$IFDEF MSWINDOWS} , JwaWinBase, jwatlhelp32 {$ENDIF};

{ Processes, execution }
procedure ActionOnExe(Action: TActionOnExecute);
function  IsProcessExists(exeFileName: string): Boolean;
procedure CloseProcessOpenByASuite;
procedure RunActionOnExe(Action: TActionOnExecute);

implementation

uses
  Kernel.Logger;

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
{$IFDEF MSWINDOWS}
var
  hSnapShot : THandle;
  ProcInfo  : TProcessEntry32;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
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
  {$ENDIF}
end;

procedure CloseProcessOpenByASuite;
{$IFDEF MSWINDOWS}
var
  hSnapShot, hProcess : THandle;
  ProcInfo  : TProcessEntry32;
  ContinueLoop: Boolean;
const
  PROCESS_TERMINATE = $0001;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TASuiteLogger.Info('Close processes opened by ASuite', []);
  hSnapShot   := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  //Check processes
  if (hSnapShot <> THandle(-1)) then
  begin
    ProcInfo.dwSize := SizeOf(ProcInfo);
    ContinueLoop := Process32First(hSnapshot, ProcInfo);
    while ContinueLoop do
    begin
      //Close process with ParentID same as ASuite PID
      if (ProcInfo.th32ParentProcessID = GetCurrentProcessId) and (ProcInfo.szExeFile <> LowerCase('Rundll32.exe')) then
      begin
        hProcess := OpenProcess(PROCESS_TERMINATE, False, ProcInfo.th32ProcessID);
        TerminateProcess(hProcess, 0);
        FileClose(hProcess);
      end;

      ContinueLoop := Process32Next(hSnapShot, ProcInfo);
    end;
  end;
  FileClose(hSnapShot);
  {$ENDIF}
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
