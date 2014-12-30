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

unit Database.Version;

interface

uses
  mORMot, Database.Manager, AppConfig.Main;

type
  TSQLtbl_version = class(TSQLRecord) //Table tbl_version
  private
    FMajor   : Integer;
    FMinor   : Integer;
    FRelease : Integer;
    FBuild   : Integer;
  public
    class procedure Load(ADBManager: TDBManager);
    class procedure Save(ADBManager: TDBManager; AConfig: TConfiguration);
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    property Major   : Integer read FMajor write FMajor;
    property Minor   : Integer read FMinor write FMinor;
    property Release : Integer read FRelease write FRelease;
    property Build   : Integer read FBuild write FBuild;
  end;

implementation



{ TSQLtbl_version }

class procedure TSQLtbl_version.Load(ADBManager: TDBManager);
var
  SQLVersionData: TSQLtbl_version;
begin
  if ADBManager.Database.TableHasRows(TSQLtbl_version) then
  begin
    //Get sql data and get version info
    SQLVersionData := TSQLtbl_version.CreateAndFillPrepare(ADBManager.Database,'');
    try
      SQLVersionData.FillOne;
      //Create Result with db version info
      //Review this code
//      ADBManager.DBVersion := TVersionInfo.Create(SQLVersionData.Major,
//                                    SQLVersionData.Minor,
//                                    SQLVersionData.Release,
//                                    SQLVersionData.Build);
    finally
      SQLVersionData.Free;
    end;
  end
  else begin
    //Create Result with actual ASuite version info
//    ADBManager.DBVersion := TVersionInfo.Create;
  end;
end;

class procedure TSQLtbl_version.Save(ADBManager: TDBManager; AConfig: TConfiguration);
var
  SQLVersionData: TSQLtbl_version;
begin
  //If necessary, clear version table
  if ADBManager.Database.TableHasRows(TSQLtbl_version) then
  begin
//    if CompareVersionInfo(ADBManager.DBVersion, AConfig.ASuiteVersion) <> 0 then
//      ADBManager.ClearTable(TSQLtbl_version)
//    else
//      Exit;
  end;
  //Insert ASuite version info
  SQLVersionData := TSQLtbl_version.Create;
  try
//    SQLVersionData.Major   := AConfig.ASuiteVersion.Major;
//    SQLVersionData.Minor   := AConfig.ASuiteVersion.Minor;
//    SQLVersionData.Release := AConfig.ASuiteVersion.Release;
//    SQLVersionData.Build   := AConfig.ASuiteVersion.Build;
    ADBManager.Database.Add(SQLVersionData,true);
  finally
    SQLVersionData.Free;
  end;
end;

end.
