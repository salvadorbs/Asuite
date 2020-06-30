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

unit Database.Version;

{$MODE DelphiUnicode}

interface

uses
  mORMot, Database.Manager, PJVersionInfo, SynLog, SysUtils;

type
  TSQLtbl_version = class(TSQLRecord) //Table tbl_version
  private
    FMajor   : Integer;
    FMinor   : Integer;
    FRelease : Integer;
    FBuild   : Integer;

    function ToVersionNumber: TPJVersionNumber;
  public
    class procedure Load(ADBManager: TDBManager);
    class procedure Save(ADBManager: TDBManager);
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    property Major   : Integer read FMajor write FMajor;
    property Minor   : Integer read FMinor write FMinor;
    property Release : Integer read FRelease write FRelease;
    property Build   : Integer read FBuild write FBuild;
  end;

implementation

uses
  AppConfig.Main, Kernel.Logger, Utility.Misc;

{ TSQLtbl_version }

class procedure TSQLtbl_version.Load(ADBManager: TDBManager);
var
  SQLVersionData: TSQLtbl_version;
begin
  if ADBManager.Database.TableHasRows(TSQLtbl_version) then
  begin
    //Get sql data
    SQLVersionData := TSQLtbl_version.CreateAndFillPrepare(ADBManager.Database,'');
    try
      SQLVersionData.FillOne;

      ADBManager.DBVersion := SQLVersionData.ToVersionNumber;
      TASuiteLogger.Info('Load Database Version (%s)', [GetASuiteVersion(False)]);
    finally
      SQLVersionData.Free;
    end;
  end;
end;

class procedure TSQLtbl_version.Save(ADBManager: TDBManager);
var
  SQLVersionData: TSQLtbl_version;
  VersionInfo: TPJVersionInfo;
  IsDataExists: Boolean;
begin
  TASuiteLogger.Info('Saving ASuite Version', []);
  VersionInfo := TPJVersionInfo.Create(nil);
  try
    VersionInfo.FileName := Config.Paths.SuiteFullFileName;
    //Select only file record by ID
    SQLVersionData := TSQLtbl_version.CreateAndFillPrepare(ADBManager.Database, '');
    try
      IsDataExists := SQLVersionData.FillOne;

      SQLVersionData.Major   := VersionInfo.FileVersionNumber.V1;
      SQLVersionData.Minor   := VersionInfo.FileVersionNumber.V2;
      SQLVersionData.Release := VersionInfo.FileVersionNumber.V3;
      SQLVersionData.Build   := VersionInfo.FileVersionNumber.V4;

      if IsDataExists then
        ADBManager.Database.Update(SQLVersionData)
      else
        ADBManager.Database.Add(SQLVersionData, True);

      ADBManager.DBVersion := SQLVersionData.ToVersionNumber;
    finally
      SQLVersionData.Free;
    end;
  finally
    VersionInfo.Free;
  end;
end;

function TSQLtbl_version.ToVersionNumber: TPJVersionNumber;
begin
  Result.V1 := Major;
  Result.V2 := Minor;
  Result.V3 := Release;
  Result.V4 := Build;
end;

end.
