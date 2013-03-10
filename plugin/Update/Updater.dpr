{
Copyright (C) 2006-2008 Matteo Salvi of SalvadorSoftware

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

program Updater;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes, IniFiles, ShellApi;

type
  TUpdateRec = record
    InstallFileName : String;
    CurrentVersion  : String;
    ReleaseNotes    : String;
  end;

  TNodeData = record
    //Properties generic
    Name       : WideString;
    DataType   : Integer; //0 Category, 1 Software, 2 Software group,
                          //3 Folder (in reality it's a software type), 4 Separator
    //Advanced
    PathExe    : WideString;
    PathIcon   : WideString;
    Parameters : WideString;
    WorkingDir : WideString;
    ActionOnExe     : Integer;
    DontInsertMRU   : Boolean;
    ShortcutDesktop : Boolean;
    HideSoftwareMenu : Boolean;
    //Misc
    Autorun    : Integer; //0 Never, 1 always on startup, 2 if no previous instances are running
                          //3 Always on shutdown
    AutorunPos : Integer; //Position for ASuiteStartUpApp and ASuiteShutdownApp
    WindowState : Integer;
  end;

var
  IniFile     : TIniFile;
  WorkingPath : String;
  ErrorCode   : Integer;
  UpdateRec   : TUpdateRec;

const
  // Application's informations
  LAUNCHER_NAME  = 'ASuite';
  APP_NAME       = 'ASuite Updater';
  ReleaseVersion = '2.0';
  UpdateFile     = 'Update.ini';

resourcestring

  msgReading  = 'Reading update.ini in progess';
  msgUpdating = 'Updating ASuite in progess';
  msgRunLauncher = 'Running ' + LAUNCHER_NAME;
  msgComplete = ' - Complete';
  msgExit     = 'Press execute key to exit';

  //Errors
  msgNotExecute = ' - Could not execute ';
  msgNotFound   = 'Error - %s file don''t found';

Function RunAndWaitProcess(PathExe, PathDir: String): Boolean;
var
  ProcInstaller  : TProcessInformation;
  StartInstaller : TStartupInfo;
begin
  Result := False;
  FillChar(ProcInstaller, sizeof(TProcessInformation), 0);
  FillChar(StartInstaller, sizeof(TStartupInfo), 0);
  StartInstaller.cb := sizeof(TStartupInfo);
  if CreateProcess(nil, PChar('"' + PathExe + '"' + '/S /D=' + PathDir +''), nil,
                   nil, false, NORMAL_PRIORITY_CLASS, nil, nil, StartInstaller,
                   ProcInstaller) then
  begin
    WaitForSingleObject(ProcInstaller.hProcess, INFINITE);
    CloseHandle(ProcInstaller.hThread);
    CloseHandle(ProcInstaller.hProcess);
    Result := True;
  end;
end;

procedure GetErrorAndReadln(ErrMessage: String);
begin
  Writeln(ErrMessage);
  Writeln(msgExit);
  Readln(Input);
  exit;
end;

procedure DeleteOldFiles;
var
  FileDelete : TStringList;
  SearchRec  : TSearchRec;
begin
  FileDelete := TStringList.Create;
  try
    FileDelete.Delimiter := ' ';
    FileDelete.QuoteChar := '|';
    FileDelete.DelimitedText := IniFile.ReadString('ForASuite' + UpdateRec.CurrentVersion,'FileDelete','');
    if FindFirst(WorkingPath + '*.*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr <> faDirectory) and
           (FileDelete.IndexOf(SearchRec.Name) <> -1) then
        begin
          DeleteFile(WorkingPath + SearchRec.Name);
        end;
      until
        FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    FileDelete.Free;
  end;
end;

begin
  if ParamStr(1) = '-version' then
  begin
    Write(ReleaseVersion);
    Exit;
  end
  else begin
    Writeln(Format('%s %s',[APP_NAME, ReleaseVersion]));
    Writeln;
    WorkingPath := GetCurrentDir + '\';
    //Load and read update.ini
    Write(msgReading);
    IniFile := TIniFile.Create(WorkingPath + UpdateFile);
    try
      UpdateRec.InstallFileName := IniFile.ReadString(LAUNCHER_NAME,'UpdateFileName','');
      UpdateRec.CurrentVersion  := IniFile.ReadString(LAUNCHER_NAME,'CurrentVersion','');
      UpdateRec.ReleaseNotes    := IniFile.ReadString(LAUNCHER_NAME,'ReleaseNotesUrl','');
      Writeln(msgComplete);
      //Check
      if (FileExists(UpdateRec.InstallFileName)) then
      begin
        Write(msgUpdating);
        //Delete ASuite's old files
        DeleteOldFiles;
        //Run ASuite install in silent mode
        if RunAndWaitProcess(WorkingPath + UpdateRec.InstallFileName,WorkingPath) then
          Writeln(msgComplete)
        else
          GetErrorAndReadln(msgNotExecute + UpdateRec.InstallFileName);
        //Run ASuite
        Write(msgRunLauncher);
        ShellExecute(GetDesktopWindow, 'open', PChar(UpdateRec.ReleaseNotes),
                                  nil, nil, SW_NORMAL);
        ErrorCode := ShellExecute(GetDesktopWindow, 'open', PChar(LAUNCHER_NAME + '.exe'),
                                  nil, nil, SW_NORMAL);
        if ErrorCode <= 32 then //Error
          GetErrorAndReadln(msgNotExecute + UpdateRec.InstallFileName);
        //Delete update.ini and InstallFileName
        DeleteFile(WorkingPath + UpdateFile);
        DeleteFile(WorkingPath + UpdateRec.InstallFileName);
      end
      else //Error
        GetErrorAndReadln(Format(msgNotFound, [UpdateRec.InstallFileName]));
    finally
      IniFile.Free;
    end;
  end;
end.
