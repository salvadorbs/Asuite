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

unit Kernel.CheckUpdates;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, IniFiles,
  System.Classes, Vcl.Graphics, Vcl.ComCtrls, IdHTTP, IdComponent, Dialogs,
  URLMon, IdBaseComponent, DKLang, ShellApi, IdIOHandlerStack,
  IdTCPConnection, Controls, PJVersionInfo;

type
  TCheckUpdatesThread = Class(TThread)
  private
    FSilentMode: Boolean;
    procedure DownloadUpdateFile(AFileStream: TMemoryStream);
    procedure HttpWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: int64);
    procedure HttpWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure ProcessUpdateFile;
    function StrToVersionNumber(Str: string): TPJVersionNumber;
  protected
    Procedure Execute; Override;
  public
    Constructor Create(ASilentMode: Boolean); overload;
  End;

const
  UPDATE_URL = 'http://www.salvadorsoftware.com/update/';
  UPDATE_FILE = 'update.ini';

  UPDATEFILE_SECTION_ASUITE = 'ASuite';
  UPDATEFILE_KEY_LASTVERSION = 'LastVersion';
  UPDATEFILE_KEY_RELEASENOTESURL = 'ReleaseNotesUrl';

implementation

uses
  Kernel.Logger, AppConfig.Main, Utility.Conversions, Utility.Misc;

{ TCheckUpdatesThread }

constructor TCheckUpdatesThread.Create(ASilentMode: Boolean);
begin
  Inherited Create(False);

  FSilentMode := ASilentMode;
  FreeOnTerminate := True;
end;

procedure TCheckUpdatesThread.DownloadUpdateFile(AFileStream: TMemoryStream);
var
  IdHTTP: TIdHTTP;
begin
  IdHTTP := TIdHTTP.Create(nil);
  try
    //Set properties
    IdHTTP.HandleRedirects := True;
    //Events
    IdHTTP.OnWorkBegin := HttpWorkBegin;
    IdHTTP.OnWorkEnd   := HttpWorkEnd;
    //Download file
    try
      IdHTTP.Get(UPDATE_URL + UPDATE_FILE, AFileStream);
    except
      on E: EIdHTTPProtocolException do
        ShowMessageEx(E.ErrorMessage, True);
    end;
  finally
    IdHTTP.Free;
  end;
end;

procedure TCheckUpdatesThread.Execute;
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    //Download file from FUrl
    TASuiteLogger.Info('Download %s file from %s', [UPDATE_FILE, UPDATE_URL + UPDATE_FILE]);
    DownloadUpdateFile(MemoryStream);
    //Save file in disk
    if MemoryStream.Size <> 0 then
    begin
      MemoryStream.SaveToFile(Config.Paths.SuitePathData + UPDATE_FILE);

      ProcessUpdateFile;
    end;
  finally
    MemoryStream.Free;
  end;
end;

procedure TCheckUpdatesThread.HttpWorkBegin(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: int64);
begin

end;

procedure TCheckUpdatesThread.HttpWorkEnd(ASender: TObject;
  AWorkMode: TWorkMode);
begin

end;

procedure TCheckUpdatesThread.ProcessUpdateFile;
var
  IniFile: TIniFile;
  LatestVersion: TPJVersionNumber;
  VersionInfo: TPJVersionInfo;
begin
  if FileExists(Config.Paths.SuitePathData + UPDATE_FILE) then
  begin
    IniFile := TIniFile.Create(Config.Paths.SuitePathData + UPDATE_FILE);
    VersionInfo := TPJVersionInfo.Create(nil);
    try
      VersionInfo.FileName := Config.Paths.SuiteFullFileName;
      LatestVersion := StrToVersionNumber(IniFile.ReadString(UPDATEFILE_SECTION_ASUITE, UPDATEFILE_KEY_LASTVERSION, ''));

      if VersionInfo.FileVersionNumber < LatestVersion then
      begin
        if (MessageDlg((DKLangConstW('msgAvailableVersion')),mtWarning, [mbYes,mbNo], 0) = mrYes) then
        begin
          TASuiteLogger.Info('Found new version', []);
          ShellExecute(handle, 'open', PChar(IniFile.ReadString(UPDATEFILE_SECTION_ASUITE, UPDATEFILE_KEY_RELEASENOTESURL,
                       'http://www.salvadorsoftware.com')), nil, nil, SW_SHOWNORMAL);
        end;
      end
      else begin
        TASuiteLogger.Info('No new version available', []);
        if Not (FSilentMode) then
          ShowMessageEx(DKLangConstW('msgNoAvailableVersion'));
      end;
    finally
      IniFile.Free;
      VersionInfo.Free;
    end;
  end;
end;

function TCheckUpdatesThread.StrToVersionNumber(Str: string): TPJVersionNumber;
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  StrToStrings(Str, '.', Parts);
  try
    while Parts.Count < 4 do
      Parts.Add('0');
    Result.V1 := StrToInt(Parts[0]);
    Result.V2 := StrToInt(Parts[1]);
    Result.V3 := StrToInt(Parts[2]);
    Result.V4 := StrToInt(Parts[3]);
  finally
    Parts.Free;
  end;
end;

end.
