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

unit Database.Version;

{$MODE DelphiUnicode}

interface

uses
  Database.Manager, mormot.core.log, SysUtils, FileInfo, mormot.orm.core;

type

  { TSQLtbl_version }

  TSQLtbl_version = class(TOrm) //Table tbl_version
  private
    FMajor   : Integer;
    FMinor   : Integer;
    FRevision : Integer;
    FBuild   : Integer;

    function ToVersionNumber: TProgramVersion;
  public
    constructor Create; override;

    class procedure Load(ADBManager: TDBManager);
    class procedure Save(ADBManager: TDBManager);
  published
    property Major   : Integer read FMajor write FMajor;
    property Minor   : Integer read FMinor write FMinor;
    property Revision : Integer read FRevision write FRevision;
    property Build   : Integer read FBuild write FBuild;
  end;

implementation

uses
  Kernel.Logger, Kernel.Instance;

{ TSQLtbl_version }

class procedure TSQLtbl_version.Load(ADBManager: TDBManager);
var
  SQLVersionData: TSQLtbl_version;
begin
  if ADBManager.Database.TableHasRows(TSQLtbl_version) then
  begin
    //Get sql data
    SQLVersionData := TSQLtbl_version.CreateAndFillPrepare(ADBManager.Database.Orm,'');
    try
      SQLVersionData.FillOne;

      ADBManager.DBVersion := SQLVersionData.ToVersionNumber;
      TASuiteLogger.Info('Load Database Version (%s)', [TASuiteInstance.GetASuiteVersion(False)]);
    finally
      SQLVersionData.Free;
    end;
  end;
end;

class procedure TSQLtbl_version.Save(ADBManager: TDBManager);
var
  SQLVersionData: TSQLtbl_version;
  ASuiteVersion  : TProgramVersion;
  IsDataExists: Boolean;
begin
  TASuiteLogger.Info('Saving ASuite Version', []);
  ASuiteVersion := TASuiteInstance.GetASuiteVersion;
  try
    //Select only file record by ID
    SQLVersionData := TSQLtbl_version.CreateAndFillPrepare(ADBManager.Database.Orm, '');
    try
      IsDataExists := SQLVersionData.FillOne;

      SQLVersionData.Major    := ASuiteVersion.Major;
      SQLVersionData.Minor    := ASuiteVersion.Minor;
      SQLVersionData.Revision := ASuiteVersion.Revision;
      SQLVersionData.Build    := ASuiteVersion.Build;

      if IsDataExists then
        ADBManager.Database.Update(SQLVersionData)
      else
        ADBManager.Database.Add(SQLVersionData, True);

      ADBManager.DBVersion := SQLVersionData.ToVersionNumber;
    finally
      SQLVersionData.Free;
    end;
  except
    on E : Exception do
      TASuiteLogger.Exception(E);
  end;
end;

function TSQLtbl_version.ToVersionNumber: TProgramVersion;
begin
  Result.Major := Major;
  Result.Minor := Minor;
  Result.Revision := Revision;
  Result.Build := Build;
end;

constructor TSQLtbl_version.Create;
begin
  inherited Create;

  Major := 0;
  Minor := 0;
  Revision := 0;
  Build := 0;
end;

end.
