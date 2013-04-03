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

unit ulDatabase;

interface

uses
  Windows, SysUtils, Forms, Dialogs, VirtualTrees, ulNodeDataTypes, ulEnumerations,
  Sqlite3DS, db, FileUtil, ulCommonClasses, Classes;

type

  { TDBUpdateTable }

  TDBUpdateTable = class
  private
    FActualVersion: TVersionInfo;
    FUpdatesVersion: array of TVersionInfo;
    procedure GetNewFields20Alpha3(TableName: String;FieldsList: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    property ActualVersion: TVersionInfo read FActualVersion write FActualVersion;
  end;

  { TDBManager }

  TDBManager = class
  private
    FDBFileName : String;
    FdsRemoveItems : TSqlite3Dataset; //Used for remove items
    FDBUpdate  : TDBUpdateTable;
    procedure CheckDatabaseFiles;
    procedure CheckDatabaseOptions;
    procedure CheckDatabaseVersion;
    procedure CreateDBTableFiles(Dataset: TSqlite3Dataset);
    procedure CreateDBTableOptions(Dataset: TSqlite3Dataset);
    procedure CreateDBTableVersion(Dataset: TSqlite3Dataset);
    function CreateSQLiteDataset(TableName: String): TSqlite3Dataset;
    procedure InsertVersion(Dataset: TSqlite3Dataset);
    procedure InternalLoadFiles(Tree: TBaseVirtualTree; id: int64; ParentNode:
                                PVirtualNode; IsImport: Boolean);
    procedure InternalSaveFiles(dsTable: TSqlite3Dataset;Tree: TBaseVirtualTree;
                                ANode: PVirtualNode;AParentID: Int64);
    procedure InternalSaveOptions;
    procedure InternalSaveVersion;
    procedure UpdateFileRecord(dsTable: TSqlite3Dataset;AData: TvBaseNodeData;
                               AIndex, AParentID: Integer);
    procedure InsertFileRecord(dsTable: TSqlite3Dataset;AData: TvBaseNodeData;
                               AIndex, AParentID: Integer);
    procedure InsertOptions(Dataset: TSqlite3Dataset);
    function  LoadVersion(Dataset: TSqlite3Dataset): TDBUpdateTable;
    procedure UpdateTableField(Dataset: TSqlite3Dataset);
  public
    constructor Create(DBFilePath: String);
    destructor Destroy; override;
    property DBFileName: String read FDBFileName write FDBFileName;
    procedure LoadOptions;
    procedure SaveData(Tree:TBaseVirtualTree; ANode: PVirtualNode;AParentID: Int64);
    procedure LoadData(Tree: TBaseVirtualTree; IsImport: Boolean);
    procedure DoBackupList(DBFile: String);
    procedure CreateDataSetRemoveItems;
    procedure DeleteItem(aID: Integer);
    procedure ApplyUpdatesRemoveItems;
  end;

var
  DBManager: TDBManager;

const
  //Tables
  DBTable_files   = 'tbl_files';
  DBTable_version = 'tbl_version';
  DBTable_options = 'tbl_options';

  //tblFiles's fields
  DBField_files_id           = 'id';
  DBField_files_type         = 'type';
  DBField_files_parent       = 'parent';
  DBField_files_position     = 'position';
  DBField_files_title        = 'title';
  DBField_files_path         = 'path';
  DBField_files_workpath     = 'work_path';
  DBField_files_parameters   = 'parameters';
  DBField_files_dateAdded    = 'dateAdded';
  DBField_files_lastModified = 'lastModified';
  DBField_files_lastAccess   = 'lastAccess';
  DBField_files_clicks       = 'clicks';
  DBField_files_nomru        = 'no_mru';
  DBField_files_nomfu        = 'no_mfu';
  DBField_files_hidefrommenu = 'hide_from_menu';
  DBField_files_dskshortcut  = 'dsk_shortcut';
  DBField_files_icon         = 'icon';
  DBField_files_cacheiconid  = 'cacheicon_id';
  DBField_files_onlaunch     = 'onlaunch';
  DBField_files_windowstate  = 'window_state';
  DBField_files_filesautorun = 'autorun';
  DBField_files_autorunpos   = 'autorun_position';
  DBField_files_keywordid    = 'keyword_id';
  DBField_files_foldertype   = 'folder_type';

  //tblVersion's fields
  DBField_version_major   = 'major';
  DBField_version_minor   = 'minor';
  DBField_version_release = 'release';
  DBField_version_build   = 'build';

  //tblOptions's fields
  DBField_options_startwithwindows   = 'startwithwindows';
  DBField_options_showpanelatstartup = 'showpanelatstartup';
  DBField_options_showmenuatstartup  = 'showmenuatstartup';
  DBField_options_language           = 'language';
  DBField_options_usecustomtitle     = 'usecustomtitle';
  DBField_options_customtitlestring  = 'customtitlestring';
  DBField_options_hidetabsearch      = 'hidetabsearch';
  DBField_options_holdsize           = 'holdsize';
  DBField_options_alwaysontop        = 'alwaysontop';
  DBField_options_listformtop        = 'listformtop';
  DBField_options_listformleft       = 'listformleft';
  DBField_options_listformwidth      = 'listformwidth';
  DBField_options_listformheight     = 'listformheight';
  DBField_options_tvbackground       = 'tvbackground';
  DBField_options_tvbackgroundpath   = 'tvbackgroundpath';
  DBField_options_tvautoopclcats     = 'tvautoopclcats';
  DBField_options_tvfont             = 'tvfont';
  DBField_options_mru                = 'mru';
  DBField_options_submenumru         = 'submenumru';
  DBField_options_mrunumber          = 'mrunumber';
  DBField_options_mfu                = 'mfu';
  DBField_options_submenumfu         = 'submenumfu';
  DBField_options_mfunumber          = 'mfunumber';
  DBField_options_backup             = 'backup';
  DBField_options_backupnumber       = 'backupnumber';
  DBField_options_autorun            = 'autorun';
  DBField_options_cache              = 'cache';
  DBField_options_actiononexe        = 'actiononexe';
  DBField_options_runsingleclick     = 'runsingleclick';
  DBField_options_trayicon           = 'trayicon';
  DBField_options_trayusecustomicon  = 'trayusecustomicon';
  DBField_options_traycustomiconpath = 'traycustomiconpath';
  DBField_options_actionclickleft    = 'actionclickleft';
  DBField_options_actionclickright   = 'actionclickright';
  //Mouse Sensors
  DBField_options_mousesensorleft    = 'mousesensorleft%d';
  DBField_options_mousesensorright   = 'mousesensorright%d';

implementation

uses
  AppConfig, ulCommonUtils, ulSysUtils, ulExeUtils, ulAppConfig, ulSQLite,
  ulTreeView, Main;

{ TDBUpdateTable }

procedure TDBUpdateTable.GetNewFields20Alpha3(TableName: String;
  FieldsList: TStringList);
var
  I : Integer;
  Query: String;
begin
  //SQlite can add ONE column at a time
  Query := Format('ALTER TABLE %s add ',[TableName]);
  //Files
  if TableName = DBTable_files then
  begin

  end
  else //Options
    if TableName = DBTable_options then
    begin
      //Mouse sensors
      for I := Low(Config.SensorLeftClick) to High(Config.SensorLeftClick) do
      begin
        FieldsList.Append(Query + Format(DBField_options_mousesensorleft, [I]) + ' INTEGER');
        FieldsList.Append(Query + Format(DBField_options_mousesensorright,[I]) + ' INTEGER');
      end;
    end;
end;

constructor TDBUpdateTable.Create;
begin
  //Why these numbers? Simple, they are ASuite 2.0 Alpha 2 and old ASuite version
  //haven't database version
  FActualVersion := TVersionInfo.Create(2,0,0,1159);
  //Add updates versions
  SetLength(FUpdatesVersion,1);
  //ASuite 2.0 Alpha 3
  { TODO : Insert right version info of Alpha 3, before release }
  FUpdatesVersion[0] := TVersionInfo.Create(2,0,0,1163);
  FUpdatesVersion[0].GetNewFields := GetNewFields20Alpha3;
end;

destructor TDBUpdateTable.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FActualVersion);
  for I := 0 to Length(FUpdatesVersion) - 1 do
    FreeAndNil(FUpdatesVersion[I]);
end;

{ TDBManager }

constructor TDBManager.Create(DBFilePath: String);
begin
  //Set FDBFileName - Database file
  FDBFileName    := DBFilePath;
  FdsRemoveItems := nil;
  //Check tables
  //If they doesn't exists, create them else update them (if necessary)
  CheckDatabaseVersion;
  CheckDatabaseFiles;
  CheckDatabaseOptions;
end;

destructor TDBManager.Destroy;
begin
  inherited;
  FreeAndNil(FDBUpdate);
end;

procedure TDBManager.CheckDatabaseVersion;
var
  dsTable: TSqlite3Dataset;
begin
  //Create SQLite Version Dataset and set Filename/Tablename
  dsTable := CreateSQLiteDataset(DBTable_version);
  try
    if (dsTable.TableExists(DBTable_version)) then
    begin
      dsTable.Open;
      if (dsTable.IsEmpty) then
        FDBUpdate := TDBUpdateTable.Create
      else //Load Database version
        FDBUpdate := LoadVersion(dsTable);
    end
    else
      CreateDBTableVersion(dsTable);
  finally
    dsTable.Destroy;
  end;
end;

procedure TDBManager.CheckDatabaseFiles;
var
  dsTable: TSqlite3Dataset;
begin
  //Create SQLite Files Dataset and set Filename/Tablename
  dsTable := CreateSQLiteDataset(DBTable_files);
  try
    if (dsTable.TableExists(DBTable_files)) then
      UpdateTableField(dsTable)
    else
      CreateDBTableFiles(dsTable);
  finally
    dsTable.Destroy;
  end;
end;

procedure TDBManager.CheckDatabaseOptions;
var
  dsTable: TSqlite3Dataset;
begin
  //Create SQLite Options Dataset and set Filename/Tablename
  dsTable := CreateSQLiteDataset(DBTable_options);
  try
    if (dsTable.TableExists(DBTable_options)) then
      UpdateTableField(dsTable)
    else
      CreateDBTableOptions(dsTable);
  finally
    dsTable.Destroy;
  end;
end;

function TDBManager.LoadVersion(Dataset: TSqlite3Dataset): TDBUpdateTable;
begin
  Result := TDBUpdateTable.Create;
  Result.ActualVersion.Major   := ReadIntegerSQLite(Dataset,DBField_version_major);
  Result.ActualVersion.Minor   := ReadIntegerSQLite(Dataset,DBField_version_minor);
  Result.ActualVersion.Release := ReadIntegerSQLite(Dataset,DBField_version_release);
  Result.ActualVersion.Build   := ReadIntegerSQLite(Dataset,DBField_version_build);
end;

procedure TDBManager.UpdateTableField(Dataset: TSqlite3Dataset);
var
  I : Integer;
  ActualVersion : TVersionInfo;
  SQLQueryList: TStringList;
  TableName: String;
begin
  SQLQueryList:=tstringlist.create;
  try
    TableName     := Dataset.TableName;
    ActualVersion := FDBUpdate.ActualVersion;
    //Get query to add new fields
    for I := 0 to Length(FDBUpdate.FUpdatesVersion) - 1 do
    begin
      if CompareVersionInfo(ActualVersion,FDBUpdate.FUpdatesVersion[I]) = -1 then
      begin
        ActualVersion := FDBUpdate.FUpdatesVersion[I];
        if Assigned(ActualVersion.GetNewFields) then
          ActualVersion.GetNewFields(TableName,SQLQueryList);
      end;
    end;
  finally
    //Execute every query to add new fields
    for I := 0 to SQLQueryList.Count - 1 do
      Dataset.ExecSQL(SQLQueryList[I]);
    SQLQueryList.Free;
  end;
end;

procedure TDBManager.CreateDataSetRemoveItems;
begin
  FdsRemoveItems := CreateSQLiteDataset(DBTable_files);
end;

procedure TDBManager.CreateDBTableVersion(Dataset: TSqlite3Dataset);
begin
  with Dataset do
  begin
    //Add fields to table
    FieldDefs.Add(DBField_version_major, ftInteger);
    FieldDefs.Add(DBField_version_minor, ftInteger);
    FieldDefs.Add(DBField_version_release, ftInteger);
    FieldDefs.Add(DBField_version_build, ftInteger);
    //Create table in database
    CreateTable;
  end;
end;

function TDBManager.CreateSQLiteDataset(TableName: String): TSqlite3Dataset;
begin
  Result := TSqlite3Dataset.Create(nil);
  Result.FileName  := FDBFileName;
  Result.TableName := TableName;
end;

procedure TDBManager.DoBackupList(DBFile: String);
begin
  //Backup list and old delete backup
  if (Config.Backup) then
  begin
    if not (DirectoryExists(SUITE_BACKUP_PATH)) then
      CreateDir(SUITE_BACKUP_PATH);
    Windows.CopyFile(PChar(DBFile), PChar(SUITE_BACKUP_PATH + APP_NAME +
      '_' + GetDateTime + EXT_SQLBCK), false);
    DeleteOldBackups(Config.BackupNumber);
  end;
end;

procedure TDBManager.CreateDBTableFiles(Dataset: TSqlite3Dataset);
begin
  with Dataset do
  begin
    //Add fields to table
    FieldDefs.Add(DBField_files_id, ftAutoInc);
    FieldDefs.Add(DBField_files_type, ftInteger);
    FieldDefs.Add(DBField_files_parent, ftInteger);
    FieldDefs.Add(DBField_files_position, ftInteger);
    FieldDefs.Add(DBField_files_title, ftString);
    FieldDefs.Add(DBField_files_path, ftString);
    FieldDefs.Add(DBField_files_workpath, ftString);
    FieldDefs.Add(DBField_files_parameters, ftString);
    FieldDefs.Add(DBField_files_dateAdded, ftInteger);
    FieldDefs.Add(DBField_files_lastModified, ftInteger);
    FieldDefs.Add(DBField_files_lastAccess, ftInteger);
    FieldDefs.Add(DBField_files_clicks, ftInteger);
    FieldDefs.Add(DBField_files_nomru, ftInteger);
    FieldDefs.Add(DBField_files_nomfu, ftInteger);
    FieldDefs.Add(DBField_files_hidefrommenu, ftInteger);
    FieldDefs.Add(DBField_files_dskshortcut, ftInteger);
    FieldDefs.Add(DBField_files_icon, ftString);
    FieldDefs.Add(DBField_files_cacheiconid, ftInteger);
    FieldDefs.Add(DBField_files_onlaunch, ftInteger);
    FieldDefs.Add(DBField_files_windowstate, ftInteger);
    FieldDefs.Add(DBField_files_filesautorun, ftInteger);
    FieldDefs.Add(DBField_files_autorunpos, ftInteger);
    FieldDefs.Add(DBField_files_keywordid, ftInteger);
    FieldDefs.Add(DBField_files_foldertype, ftString);
    //Create table in database
    CreateTable;
  end;
end;

procedure TDBManager.InternalLoadFiles(Tree: TBaseVirtualTree; id: int64;
                            ParentNode: PVirtualNode; IsImport: Boolean);
var
  dsTable : TSqlite3Dataset;
  nType   : TvTreeDataType;
  vData   : TvBaseNodeData;
  Node    : PVirtualNode;
begin
  dsTable := CreateSQLiteDataset(DBTable_files);
  try
    //Get files from DBTable and order them by parent, position
    dsTable.SQL := Format('SELECT * FROM %s WHERE parent = %d ORDER BY parent, position',[DBTable_files,id]);
    dsTable.Open;
    //Get files and its properties
    while not dsTable.EOF do
    begin
      nType := TvTreeDataType(dsTable.FieldByName(DBField_files_type).AsInteger);
      Node  := Tree.AddChild(ParentNode, CreateNodeData(nType));
      vData := PBaseData(Tree.GetNodeData(Node)).Data;
      PBaseData(Tree.GetNodeData(Node)).pNode := Node;
      if IsImport then
        Tree.CheckType[Node] := ctTriStateCheckBox
      else
        vData.CacheID       := ReadIntegerSQLite(dsTable,DBField_files_cacheiconid);
      // generic fields
      vData.Name          := ReadStringSQLite(dsTable,DBField_files_title);
      vData.id            := ReadIntegerSQLite(dsTable,DBField_files_id);
      vData.ParentID      := id;
      vData.Position      := Node.Index;
      vData.UnixAddDate   := ReadIntegerSQLite(dsTable,DBField_files_dateAdded);
      vData.UnixEditDate  := ReadIntegerSQLite(dsTable,DBField_files_lastModified);
      vData.ParentNode    := ParentNode;
      vData.PathIcon      := ReadStringSQLite(dsTable,DBField_files_icon);
      vData.HideFromMenu  := ReadBooleanSQLite(dsTable,DBField_files_hidefrommenu);
      if (nType = vtdtFile) then
      begin
        with TvFileNodeData(vData) do
        begin
          PathExe          := ReadStringSQLite(dsTable,DBField_files_path);
          Parameters       := ReadStringSQLite(dsTable,DBField_files_parameters);
          WorkingDir       := ReadStringSQLite(dsTable,DBField_files_workpath);
          ClickCount       := ReadIntegerSQLite(dsTable,DBField_files_clicks);
          ShortcutDesktop  := ReadBooleanSQLite(dsTable,DBField_files_dskshortcut);
          AutorunPos       := ReadIntegerSQLite(dsTable,DBField_files_autorunpos);
          Autorun          := TAutorunType(ReadIntegerSQLite(dsTable,DBField_files_filesautorun,0));
          WindowState      := ReadIntegerSQLite(dsTable,DBField_files_windowstate,0);
          ActionOnExe      := TActionOnExecution(ReadIntegerSQLite(dsTable,DBField_files_onlaunch,0));
          NoMRU            := ReadBooleanSQLite(dsTable,DBField_files_nomru);
          NoMFU            := ReadBooleanSQLite(dsTable,DBField_files_nomfu);
          MRUPosition      := ReadIntegerSQLite(dsTable,DBField_files_lastAccess);
        end;
      end;
      if (nType = vtdtCategory) then
        InternalLoadFiles(Tree, vData.ID, Node, IsImport);
      dsTable.Next;
    end;
  finally
    dsTable.Destroy;
  end;
end;

procedure TDBManager.InternalSaveFiles(dsTable: TSqlite3Dataset;Tree:TBaseVirtualTree; ANode: PVirtualNode; AParentID: Int64);
var
  Node    : PVirtualNode;
  vData   : TvBaseNodeData;
begin
  Node    := ANode;
  while (Node <> nil) do
  begin
    vData := PBaseData(Tree.GetNodeData(Node)).Data;
    try
      //Insert or update record
      if (vData.ID < 0) then
        InsertFileRecord(dsTable, vData, Node.Index, AParentID)
      else
        if ((vData.Changed) or (vData.Position <> Node.Index) or (vData.ParentID <> AParentID)) then
          UpdateFileRecord(dsTable, vData, Node.Index, AParentID);
      //If type is category then process sub-nodes
      if (vData.DataType = vtdtCategory) then
        InternalSaveFiles(dsTable, Tree, Node.FirstChild, vData.ID);
    except
      on E : Exception do
        ShowMessageFmt(msgErrGeneric,[E.ClassName,E.Message]);
    end;
    Node := Node.NextSibling;
  end;
end;

procedure TDBManager.InternalSaveOptions;
var
  dsTable : TSqlite3Dataset;
begin
  dsTable := CreateSQLiteDataset(DBTable_options);
  try
    //Set primarykey, if doesn't set it, ApplyUpdates doesn't work
    dsTable.PrimaryKey := DBField_options_startwithwindows;
    dsTable.Open;
    //Remove options from dbTable_Options
    while not dsTable.IsEmpty do
      dsTable.Delete;
    //ReInsert options in dbTable_Options
    dsTable.Append;
    InsertOptions(dsTable);
  finally
    //Write in sqlite database
    dsTable.Post;
    dsTable.ApplyUpdates;
    dsTable.Destroy;
  end;
end;

procedure TDBManager.InternalSaveVersion;
var
  dsTable : TSqlite3Dataset;
begin
  dsTable := CreateSQLiteDataset(DBTable_version);
  try
    //Set primarykey, if doesn't set it, ApplyUpdates doesn't work
    dsTable.PrimaryKey := DBField_version_major;
    dsTable.Open;
    //Remove old version info from dbTable_Version
    while not dsTable.IsEmpty do
      dsTable.Delete;
    //Reinsert version info in dbTable_Version
    dsTable.Append;
    InsertVersion(dsTable);
  finally
    //Write in sqlite database
    dsTable.Post;
    dsTable.ApplyUpdates;
    dsTable.Destroy;
  end;
end;

procedure TDBManager.InsertVersion(Dataset: TSqlite3Dataset);
begin
  WriteIntegerSQLite(Dataset,DBField_version_major,  StrToInt(VERSION_MAJOR));
  WriteIntegerSQLite(Dataset,DBField_version_minor,  StrToInt(VERSION_MINOR));
  WriteIntegerSQLite(Dataset,DBField_version_release,StrToInt(VERSION_RELEASE));
  WriteIntegerSQLite(Dataset,DBField_version_build,  StrToInt(VERSION_BUILD));
end;

procedure TDBManager.InsertFileRecord(dsTable: TSqlite3Dataset;AData: TvBaseNodeData; AIndex, AParentID: Integer);
begin
  try
    dsTable.Append;
    //Add base fields
    WriteIntegerSQLite(dsTable,DBField_files_type,Ord(AData.DataType));
    WriteIntegerSQLite(dsTable,DBField_files_parent,AParentID);
    WriteIntegerSQLite(dsTable,DBField_files_position,AIndex);
    WriteStringSQLite (dsTable,DBField_files_title,AData.Name);
    WriteIntegerSQLite(dsTable,DBField_files_cacheiconid,AData.CacheID);
    //Add specific category and file fields
    if AData.DataType <> vtdtSeparator then
    begin
      WriteStringSQLite (dsTable,DBField_files_icon,AData.PathIcon);
      WriteBooleanSQLite(dsTable,DBField_files_hidefrommenu,AData.HideFromMenu);
      //Add file fields
      if (AData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(AData) do
        begin
          WriteStringSQLite (dsTable,DBField_files_path,PathExe);
          WriteStringSQLite (dsTable,DBField_files_workpath,WorkingDir);
          WriteStringSQLite (dsTable,DBField_files_parameters,Parameters);
          WriteIntegerSQLite(dsTable,DBField_files_clicks,ClickCount);
          WriteIntegerSQLite(dsTable,DBField_files_windowstate,WindowState);
          WriteBooleanSQLite(dsTable,DBField_files_dskshortcut,ShortcutDesktop);
          WriteIntegerSQLite(dsTable,DBField_files_filesautorun,Ord(Autorun));
          WriteIntegerSQLite(dsTable,DBField_files_autorunpos,AutorunPos);
          WriteIntegerSQLite(dsTable,DBField_files_onlaunch,Ord(ActionOnExe));
          WriteBooleanSQLite(dsTable,DBField_files_nomru,NoMRU);
          WriteBooleanSQLite(dsTable,DBField_files_nomfu,NoMFU);
          WriteIntegerSQLite(dsTable,DBField_files_lastAccess,MRUPosition);
        end;
      end;
      //Add time fields
      WriteIntegerSQLite(dsTable,DBField_files_dateAdded,AData.UnixAddDate);
      WriteIntegerSQLite(dsTable,DBField_files_lastModified,AData.UnixEditDate);
    end;
  finally
    //Update node
    dsTable.Post;
    //Set ID, ParentID and position
    AData.ID       := ReadIntegerSQLite(dsTable,DBField_files_id);
    AData.ParentID := AParentID;
    AData.Position := AIndex;
  end;
end;

procedure TDBManager.InsertOptions(Dataset: TSqlite3Dataset);
var
   i:Integer;
begin
  //general
  WriteBooleanSQLite(Dataset,DBField_options_startwithwindows,Config.StartWithWindows);
  WriteBooleanSQLite(Dataset,DBField_options_showpanelatstartup,Config.ShowPanelAtStartUp);
  WriteBooleanSQLite(Dataset,DBField_options_showmenuatstartup,Config.ShowMenuAtStartUp);
  //main form
  WriteStringSQLite (Dataset,DBField_options_language,Config.Language);
  WriteBooleanSQLite(Dataset,DBField_options_usecustomtitle,Config.UseCustomTitle);
  WriteStringSQLite (Dataset,DBField_options_customtitlestring,Config.CustomTitleString);
  WriteBooleanSQLite(Dataset,DBField_options_hidetabsearch,Config.HideTabSearch);
  //main form - position and size
  WriteBooleanSQLite(Dataset,DBField_options_holdsize,Config.HoldSize);
  WriteBooleanSQLite(Dataset,DBField_options_alwaysontop,Config.AlwaysOnTop);
  WriteIntegerSQLite(Dataset,DBField_options_listformtop,frmMain.Top);
  WriteIntegerSQLite(Dataset,DBField_options_listformleft,frmMain.Left);
  WriteIntegerSQLite(Dataset,DBField_options_listformwidth,frmMain.Width);
  WriteIntegerSQLite(Dataset,DBField_options_listformheight,frmMain.Height);
  //main form - treevew
  WriteBooleanSQLite(Dataset,DBField_options_tvbackground,Config.TVBackground);
  WriteStringSQLite (Dataset,DBField_options_tvbackgroundpath,Config.TVBackgroundPath);
  WriteBooleanSQLite(Dataset,DBField_options_tvautoopclcats,Config.TVAutoOpClCats);
  WriteStringSQLite (Dataset,DBField_options_tvfont,FontToStr(Config.TVFont));
  //mru
  WriteBooleanSQLite(Dataset,DBField_options_mru,Config.MRU);
  WriteBooleanSQLite(Dataset,DBField_options_submenumru,Config.SubMenuMRU);
  WriteIntegerSQLite(Dataset,DBField_options_mrunumber,Config.MRUNumber);
  //mfu
  WriteBooleanSQLite(Dataset,DBField_options_mfu,Config.MFU);
  WriteBooleanSQLite(Dataset,DBField_options_submenumfu,Config.SubMenuMFU);
  WriteIntegerSQLite(Dataset,DBField_options_mfunumber,Config.MFUNumber);
  //backup
  WriteBooleanSQLite(Dataset,DBField_options_backup,Config.Backup);
  WriteIntegerSQLite(Dataset,DBField_options_backupnumber,Config.BackupNumber);
  //other functions
  WriteBooleanSQLite(Dataset,DBField_options_autorun,Config.Autorun);
  WriteBooleanSQLite(Dataset,DBField_options_cache,Config.Cache);
  //execution
  WriteIntegerSQLite(Dataset,DBField_options_actiononexe,Ord(Config.ActionOnExe));
  WriteBooleanSQLite(Dataset,DBField_options_runsingleclick,Config.RunSingleClick);
  //trayicon
  WriteBooleanSQLite(Dataset,DBField_options_trayicon,Config.TrayIcon);
  WriteBooleanSQLite(Dataset,DBField_options_trayusecustomicon,Config.TrayUseCustomIcon);
  WriteStringSQLite (Dataset,DBField_options_traycustomiconpath,Config.TrayCustomIconPath);
  WriteIntegerSQLite(Dataset,DBField_options_actionclickleft,Config.ActionClickLeft);
  WriteIntegerSQLite(Dataset,DBField_options_actionclickright,Config.ActionClickRight);
  //Mouse Sensor
  for i:=Low(Config.SensorLeftClick) to High(Config.SensorLeftClick) do begin
    WriteIntegerSQLite(Dataset,format(DBField_options_mousesensorleft,[i]),Config.SensorLeftClick[i]);
    WriteIntegerSQLite(Dataset,format(DBField_options_mousesensorright,[i]),Config.SensorRightClick[i]);
  end;
end;

procedure TDBManager.UpdateFileRecord(dsTable: TSqlite3Dataset;AData: TvBaseNodeData; AIndex, AParentID: Integer);
begin
  try
    //Select only file record by ID
    dsTable.Locate(DBField_files_id,AData.ID,[]);
    dsTable.Edit;
    //Update base fields
    UpdateIntegerSQLite(dsTable,DBField_files_parent,AParentID);
    UpdateIntegerSQLite(dsTable,DBField_files_position,AIndex);
    UpdateStringSQLite (dsTable,DBField_files_title,AData.Name);
    UpdateIntegerSQLite(dsTable,DBField_files_cacheiconid,AData.CacheID);
    //Update specific fields
    if AData.DataType <> vtdtSeparator then
    begin
      UpdateIntegerSQLite(dsTable,DBField_files_dateAdded,AData.UnixAddDate);
      UpdateIntegerSQLite(dsTable,DBField_files_lastModified,AData.UnixEditDate);
      UpdateIntegerSQLite(dsTable,DBField_files_lastAccess,AData.MRUPosition);
      UpdateIntegerSQLite(dsTable,DBField_files_clicks,AData.ClickCount);
      UpdateStringSQLite (dsTable,DBField_files_icon,AData.PathIcon);
      UpdateBooleanSQLite(dsTable,DBField_files_hidefrommenu,AData.HideFromMenu);
      //Update file fields
      if (AData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(AData) do
        begin
          UpdateStringSQLite (dsTable,DBField_files_path,PathExe);
          UpdateStringSQLite (dsTable,DBField_files_workpath,WorkingDir);
          UpdateStringSQLite (dsTable,DBField_files_parameters,Parameters);
          UpdateBooleanSQLite(dsTable,DBField_files_nomru,NoMRU);
          UpdateBooleanSQLite(dsTable,DBField_files_nomfu,NoMFU);
          UpdateBooleanSQLite(dsTable,DBField_files_dskshortcut,ShortcutDesktop);
          UpdateIntegerSQLite(dsTable,DBField_files_onlaunch,Ord(ActionOnExe));
          UpdateIntegerSQLite(dsTable,DBField_files_windowstate,WindowState);
          UpdateIntegerSQLite(dsTable,DBField_files_filesautorun,Ord(Autorun));
          UpdateIntegerSQLite(dsTable,DBField_files_autorunpos,AutorunPos);
        end;
      end;
    end;
  finally
    //Insert data
    dsTable.Post;
    // update node
    AData.ParentID := AParentID;
    AData.Position := AIndex;
  end;
end;

procedure TDBManager.CreateDBTableOptions(Dataset: TSqlite3Dataset);
var
   i:Integer;
begin
  with Dataset do
  begin
    //Add fields to table
    //general
    FieldDefs.Add(DBField_options_startwithwindows, ftInteger);
    FieldDefs.Add(DBField_options_showpanelatstartup, ftInteger);
    FieldDefs.Add(DBField_options_showmenuatstartup, ftInteger);
    //main form
    FieldDefs.Add(DBField_options_language, ftString);
    FieldDefs.Add(DBField_options_usecustomtitle, ftInteger);
    FieldDefs.Add(DBField_options_customtitlestring, ftString);
    FieldDefs.Add(DBField_options_hidetabsearch, ftInteger);
    //main form - position and size
    FieldDefs.Add(DBField_options_holdsize, ftInteger);
    FieldDefs.Add(DBField_options_alwaysontop, ftInteger);
    FieldDefs.Add(DBField_options_listformtop, ftInteger);
    FieldDefs.Add(DBField_options_listformleft, ftInteger);
    FieldDefs.Add(DBField_options_listformwidth, ftInteger);
    FieldDefs.Add(DBField_options_listformheight, ftInteger);
    //main form - treevew
    FieldDefs.Add(DBField_options_tvbackground, ftInteger);
    FieldDefs.Add(DBField_options_tvbackgroundpath, ftString);
    FieldDefs.Add(DBField_options_tvautoopclcats, ftInteger);
    FieldDefs.Add(DBField_options_tvfont, ftString);
    //mru
    FieldDefs.Add(DBField_options_mru, ftInteger);
    FieldDefs.Add(DBField_options_submenumru, ftInteger);
    FieldDefs.Add(DBField_options_mrunumber, ftInteger);
    //mfu
    FieldDefs.Add(DBField_options_mfu, ftInteger);
    FieldDefs.Add(DBField_options_submenumfu, ftInteger);
    FieldDefs.Add(DBField_options_mfunumber, ftInteger);
    //backup
    FieldDefs.Add(DBField_options_backup, ftInteger);
    FieldDefs.Add(DBField_options_backupnumber, ftInteger);
    //other functions
    FieldDefs.Add(DBField_options_autorun, ftInteger);
    FieldDefs.Add(DBField_options_cache, ftInteger);
    //execution
    FieldDefs.Add(DBField_options_actiononexe, ftInteger);
    FieldDefs.Add(DBField_options_runsingleclick, ftInteger);
    //trayicon
    FieldDefs.Add(DBField_options_trayicon, ftInteger);
    FieldDefs.Add(DBField_options_trayusecustomicon, ftInteger);
    FieldDefs.Add(DBField_options_traycustomiconpath, ftString);
    FieldDefs.Add(DBField_options_actionclickleft, ftInteger);
    FieldDefs.Add(DBField_options_actionclickright, ftInteger);
    //mouse sensors
    for i:=Low(Config.SensorLeftClick) to High(Config.SensorLeftClick) do begin
      FieldDefs.Add(format(DBField_options_mousesensorleft,[i]), ftInteger);
      FieldDefs.Add(format(DBField_options_mousesensorright,[i]), ftInteger);
    end;
    //Create table in database
    CreateTable;
  end;
end;

procedure TDBManager.LoadOptions;
var
  dsTable : TSqlite3Dataset;
  i:Integer;
begin
  dsTable := CreateSQLiteDataset(DBTable_options);
  try
    //Get options from DBTable
    dsTable.Open;
    if not dsTable.EOF then
    begin
      //General
      Config.StartWithWindows   := ReadBooleanSQLite(dsTable, DBField_options_startwithwindows);
      Config.ShowPanelAtStartUp := ReadBooleanSQLite(dsTable, DBField_options_showpanelatstartup,true);
      Config.ShowMenuAtStartUp  := ReadBooleanSQLite(dsTable, DBField_options_showmenuatstartup);
      //Main Form
      { TODO -oMatteo -c : Insert code for language 26/11/2009 22:21:05 }
//      FLanguage           := '';
      Config.CustomTitleString := ReadStringSQLite(dsTable, DBField_options_customtitlestring,APP_TITLE);
      Config.UseCustomTitle    := ReadBooleanSQLite(dsTable, DBField_options_usecustomtitle);
      Config.HideTabSearch     := ReadBooleanSQLite(dsTable, DBField_options_hidetabsearch);
      //Main Form - Position and size
      Config.HoldSize    := ReadBooleanSQLite(dsTable, DBField_options_holdsize);
      Config.AlwaysOnTop := ReadBooleanSQLite(dsTable, DBField_options_alwaysontop);
      //frmMain's size
      frmMain.Width      := ReadIntegerSQLite(dsTable,DBField_options_ListFormWidth,frmMainWidth);
      frmMain.Height     := ReadIntegerSQLite(dsTable,DBField_options_ListFormHeight,frmMainHeight);
      //FrmMain's position
      if Not(FileExists(SUITE_LIST_PATH)) then
        frmMain.Position := poDesigned
      else
        frmMain.Position := poDesktopCenter;
      SetFormPosition(frmMain, ReadIntegerSQLite(dsTable,DBField_options_ListFormLeft,frmMain.Left),
                               ReadIntegerSQLite(dsTable,DBField_options_ListFormTop,frmMain.Top));
      //Main Form - Treevew
      Config.TVBackgroundPath   := ReadStringSQLite(dsTable, DBField_options_tvbackgroundpath);
      Config.TVBackground       := ReadBooleanSQLite(dsTable, DBField_options_tvbackground);
      Config.TVAutoOpClCats     := ReadBooleanSQLite(dsTable, DBField_options_tvautoopclcats);
      //Treeview Font
      StrToFont(ReadStringSQLite(dsTable,DBField_options_tvfont),Config.TVFont);
      //MRU
      Config.MRU            := ReadBooleanSQLite(dsTable, DBField_options_mru,true);
      Config.SubMenuMRU     := ReadBooleanSQLite(dsTable, DBField_options_submenumru);
      Config.MRUNumber      := ReadIntegerSQLite(dsTable, DBField_options_mrunumber,5);
      //MFU
      Config.MFU            := ReadBooleanSQLite(dsTable, DBField_options_mfu);
      Config.SubMenuMFU     := ReadBooleanSQLite(dsTable, DBField_options_submenumfu);
      Config.MFUNumber      := ReadIntegerSQLite(dsTable, DBField_options_mfunumber);
      //Backup
      Config.Backup         := ReadBooleanSQLite(dsTable, DBField_options_backup,true);
      Config.BackupNumber   := ReadIntegerSQLite(dsTable, DBField_options_backupnumber,5);
      //Other functions
      Config.Autorun        := ReadBooleanSQLite(dsTable, DBField_options_autorun,true);
      Config.Cache          := ReadBooleanSQLite(dsTable, DBField_options_cache,true);
      //Execution
      Config.ActionOnExe    := TActionOnExecution(ReadIntegerSQLite(dsTable, DBField_options_actiononexe,0));
      Config.RunSingleClick := ReadBooleanSQLite(dsTable, DBField_options_runsingleclick);
      //Trayicon
      Config.TrayIcon           := ReadBooleanSQLite(dsTable, DBField_options_trayicon,true);
      Config.TrayCustomIconPath := ReadStringSQLite(dsTable, DBField_options_traycustomiconpath);
      Config.TrayUseCustomIcon  := ReadBooleanSQLite(dsTable, DBField_options_trayusecustomicon);
      Config.ActionClickLeft    := ReadIntegerSQLite(dsTable, DBField_options_actionclickleft,0);
      Config.ActionClickRight   := ReadIntegerSQLite(dsTable, DBField_options_actionclickright,2);
      //Mouse Sensor
      for i:=Low(Config.SensorLeftClick) to High(Config.SensorLeftClick) do begin
        Config.SensorLeftClick[i]  := ReadIntegerSQLite(dsTable,format(DBField_options_mousesensorleft,[i]));
        Config.SensorRightClick[i] := ReadIntegerSQLite(dsTable,format(DBField_options_mousesensorright,[i]));
      end;
    end
    else begin
      Config.Changed := True;
      //Create folder cache, if it doesn't exist
      if (not DirectoryExists(SUITE_CACHE_PATH)) then
        CreateDir(SUITE_CACHE_PATH);
    end;
  finally
    dsTable.Destroy;
    Config.UpdateSensors;
  end;
end;

procedure TDBManager.SaveData(Tree: TBaseVirtualTree;ANode: PVirtualNode;
  AParentID: Int64);
var
  dsTable : TSqlite3Dataset;
begin
  try
    //Create and open Sqlite3Dataset
    dsTable := CreateSQLiteDataset(DBTable_files);
    dsTable.Open;
    //Save list in Sqlite3Dataset
    InternalSaveFiles(dsTable, Tree, Anode, AParentID);
    //Apply updates (write updates in sqlite database)
    dsTable.ApplyUpdates;
    dsTable.Destroy;
    //If settings is changed, insert it else (if it exists) update it
    if Config.Changed then
      InternalSaveOptions;
    //Save version info
    InternalSaveVersion;
  except
    on E : Exception do
      ShowMessageFmt(msgErrGeneric,[E.ClassName,E.Message]);
  end;
end;

procedure TDBManager.LoadData(Tree: TBaseVirtualTree; IsImport: Boolean);
begin
  try
    if not IsImport then
    begin
      LoadOptions;
      MRUList := TMRUList.Create(Config.MRUNumber);
      MFUList := TMFUList.Create(Config.MFUNumber);
    end;
    InternalLoadFiles(Tree, 0, nil, IsImport);
  except
    on E : Exception do
      ShowMessageFmt(msgErrGeneric,[E.ClassName,E.Message]);
  end;
end;

procedure TDBManager.DeleteItem(aID: Integer);
begin
  try
    FdsRemoveItems.Open;
    //Get node from db by its ID and delete it
    FdsRemoveItems.Locate(DBField_files_id,aID,[]);
    FdsRemoveItems.Delete;
  except
    on E : Exception do
      ShowMessageFmt(msgErrGeneric,[E.ClassName,E.Message]);
  end;
end;

procedure TDBManager.ApplyUpdatesRemoveItems;
begin
  FdsRemoveItems.ApplyUpdates;
  FdsRemoveItems.Destroy;
end;

initialization

finalization
  DBManager.Free

end.
