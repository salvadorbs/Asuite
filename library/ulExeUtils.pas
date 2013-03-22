{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit ulExeUtils;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, ulEnumerations, ulNodeDataTypes, Main, ulAppConfig, jwatlhelp32;

{ Processes, execution }
procedure ActionOnExe(Action: TActionOnExecution);
function  IsProcessExists(exeFileName: string): Boolean;
procedure RunActionOnExe(NodeData: TvFileNodeData);

implementation

procedure ActionOnExe(Action: TActionOnExecution);
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
      frmMain.ShutdownTime := True;
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

procedure RunActionOnExe(NodeData: TvFileNodeData);
var
  ActionTemp : TActionOnExecution;
begin
  if NodeData.ActionOnExe = aeDefault then
  begin
    ActionTemp := TActionOnExecution(Ord(Config.ActionOnExe) + 1);
    ActionOnExe(ActionTemp);
  end
  else
    ActionOnExe(NodeData.ActionOnExe);
end;

end.
