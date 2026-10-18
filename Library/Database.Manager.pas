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

unit Database.Manager;

{$MODE DelphiUnicode}

interface

uses
  LCLType, SysUtils, Dialogs, VirtualTrees, mormot.rest.sqlite3, mormot.orm.core,
  Controls, FileInfo;

type
  TDBManager = class
  private
    FDBFileName : string;
    FDBVersion  : TProgramVersion;
    FDatabase   : TSQLRestServerDB;
    FSQLModel   : TOrmModel;

    procedure DoBackupList;
    function GetDateTimeAsString: AnsiString;
    //Share-friendly copy: FileUtil.CopyFile opens the source with
    //fmShareDenyWrite, which fails on Windows while SQLite holds the
    //database open, so SaveData silently produced no backup there
    function CopyFileShared(const ASource, ADest: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property DBFileName: string read FDBFileName write FDBFileName;
    property DBVersion: TProgramVersion read FDBVersion write FDBVersion;
    property Database: TSQLRestServerDB read FDatabase;

    procedure Setup(const ADBFilePath: string);

    procedure LoadData(ATree: TBaseVirtualTree);
    function  SaveData(ATree: TBaseVirtualTree; DoBackup: Boolean = True): Boolean;

    procedure RemoveItem(aID: Integer);
    function DeleteItems(ATree: TBaseVirtualTree; ANodes: TNodeArray): Boolean;

    procedure ClearTable(SQLRecordClass:TSQLRecordClass);

    procedure ImportData(ATree: TBaseVirtualTree); //For frmImportList
  end;

implementation

uses
  Classes,
  Kernel.Consts, AppConfig.Main, Utility.FileFolder,
  Database.Version, Database.List, Kernel.Logger,
  VirtualTree.Methods, mormot.core.log, Kernel.Instance;

constructor TDBManager.Create;
begin
  FSQLModel := TSQLModel.Create([TSQLtbl_version, TSQLtbl_list]);
end;

procedure TDBManager.RemoveItem(aID: Integer);
begin
  //Setup has not run yet: there is no database handle to delete through
  if not Assigned(FDatabase) then
    Exit;
  if (aID > 0) then
    FDatabase.Delete(TSQLtbl_list, aID);
end;

function TDBManager.DeleteItems(ATree: TBaseVirtualTree; ANodes: TNodeArray): Boolean;
var
  I: Integer;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TDBManager.DeleteItems', Self);
  Result := False;
  //Setup has not run yet: there is no database handle to delete through
  if not Assigned(FDatabase) then
    Exit;
  Result := FDatabase.TransactionBegin(TSQLtbl_list, 1);
  //Begin transaction for remove data from sqlite database
  if Result then
  begin
    try
      //Run actions (ex. remove node from MRU list) before delete nodes and
      //remove each selected items from sqlite database
      for I := High(ANodes) downto 0 do
        ATree.IterateSubtree(ANodes[I], TVirtualTreeMethods.BeforeDeleteNode, nil, [], False);
      //Commit database's updates
      FDatabase.Commit(1);
    except                              
      on E: Exception do
      begin
        //Or in case of error, rollback and log
        TASuiteLogger.Exception(E);

        FDatabase.RollBack(1);
        Result := False;
      end;
    end;
  end;
end;

destructor TDBManager.Destroy;
begin
  inherited;
  FDatabase.Free;
  FSQLModel.Free;
end;

procedure TDBManager.DoBackupList;
var
  BackupFileName: String;
begin
  //Backup list and old delete backup
  if FileExists(FDBFileName) and (Config.Backup) then
  begin
    BackupFileName := Format(ASuiteInstance.Paths.SuitePathBackup + BACKUP_FILE,
                             [GetDateTimeAsString]);
    if not CopyFileShared(FDBFileName, BackupFileName) then
      TASuiteLogger.Info('Unable to create backup "%s"', [BackupFileName]);
    DeleteOldBackups(Config.BackupNumber);
  end;
end;

function TDBManager.CopyFileShared(const ASource, ADest: String): Boolean;
var
  Source, Dest: TFileStream;
begin
  Result := False;
  if not FileExists(ASource) then
    Exit;

  //fmShareDenyNone lets the copy succeed while SQLite has the database open:
  //fmShareDenyWrite (used by FileUtil.CopyFile) is rejected by Windows there
  Source := TFileStream.Create(ASource, fmOpenRead or fmShareDenyNone);
  try
    Dest := TFileStream.Create(ADest, fmCreate);
    try
      Dest.CopyFrom(Source, 0);
      Result := True;
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;
end;

function TDBManager.GetDateTimeAsString: AnsiString;
begin
  DateTimeToString(Result, 'yyyy-mm-dd-hh-mm-ss', now);
end;

procedure TDBManager.ImportData(ATree: TBaseVirtualTree);
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TDBManager.ImportData', Self);
  try
    TSQLtbl_list.Load(Self, ATree, True);
  except
    on E : Exception do
      TASuiteLogger.Exception(E);
  end;
end;

procedure TDBManager.ClearTable(SQLRecordClass: TSQLRecordClass);
var
  SQLData: TSQLRecord;
begin
  SQLData := SQLRecordClass.CreateAndFillPrepare(FDatabase.Orm, '');
  try
    while SQLData.FillOne do
      FDatabase.Delete(SQLRecordClass, SQLData.ID);
  finally
    SQLData.Free;
  end;
end;

procedure TDBManager.LoadData(ATree: TBaseVirtualTree);
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TDBManager.LoadData', Self);
  TASuiteLogger.Info('Found SQLite Database - Loading it', []);
  //List & Options
  ATree.BeginUpdate;
  try
    try
      //Load Database version
      TSQLtbl_version.Load(Self);
      //Load list
      TSQLtbl_list.Load(Self, ATree, False);
    except
      on E : Exception do
        TASuiteLogger.Exception(E);
    end;
  finally
    ATree.EndUpdate;
  end;
end;

function TDBManager.SaveData(ATree: TBaseVirtualTree; DoBackup: Boolean): Boolean;
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TDBManager.SaveData', Self);
  TASuiteLogger.Info('Saving ASuite SQLite Database', []);

  //Result stays False when the transaction cannot run or the save raises:
  //frmMain already branches on it to show msgErrSave instead of msgSaveCompleted
  Result := False;

  //Setup has not run yet: there is no database handle to save through,
  //and without a save there is nothing to back up either
  if not Assigned(FDatabase) then
    Exit;

  //List & Options
  try
    //Save data and do backup
    try
      //Create and open Sqlite3Dataset
      if FDatabase.TransactionBegin(TSQLtbl_list, 1) then
      begin
        TSQLtbl_list.Save(Self, ATree);
        //Save new version info
        TSQLtbl_version.Save(Self);
        //Commit data in sqlite database
        FDatabase.Commit(1);
        Result := True;
      end;
    except
      on E : Exception do begin
        TASuiteLogger.Exception(E);
        FDatabase.Rollback(1);
      end;
    end;
  finally
    //Never back up a database whose save just failed
    if Result and DoBackup then
      DoBackupList;
  end;
end;

procedure TDBManager.Setup(const ADBFilePath: string);
begin
  TASuiteLogger.Info('Setup DBManager using sqlite file "%s"', [ADBFilePath]);
  FDBFileName := ADBFilePath;

  //Close a database opened by a previous Setup: SQLite is opened in exclusive
  //mode, so a second handle on the same file would fail to open it
  FreeAndNil(FDatabase);

  DoBackupList;

  //Load sqlite3 database and create missing tables
  FDatabase := TSQLRestServerDB.Create(FSQLModel, FDBFileName);
  fDatabase.CreateMissingTables(0);
end;

end.
