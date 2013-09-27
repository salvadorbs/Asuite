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

unit ulDatabase;

interface

uses
  Windows, SysUtils, Forms, Dialogs, VirtualTrees, ulNodeDataTypes, ulEnumerations,
  ulCommonClasses, Classes, mORMot, SynCommons, mORMotSQLite3, DKLang,
  System.UITypes, Vcl.Controls;

type

  { TSQLVersion }

  TSQLtbl_version = class(TSQLRecord) //Table tbl_version
  private
    FMajor   : Integer;
    FMinor   : Integer;
    FRelease : Integer;
    FBuild   : Integer;
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    property Major   : Integer read FMajor write FMajor;
    property Minor   : Integer read FMinor write FMinor;
    property Release : Integer read FRelease write FRelease;
    property Build   : Integer read FBuild write FBuild;
  end;

  { TSQLFiles }

  TSQLtbl_files = class(TSQLRecord) //Table tbl_files
  private
    Ftype             : Integer;
    Fparent           : Integer;
    Fposition         : Integer;
    Ftitle            : RawUTF8;
    Fpath             : RawUTF8;
    Fwork_path        : RawUTF8;
    Fparameters       : RawUTF8;
    FdateAdded        : Integer;
    FlastModified     : Integer;
    FlastAccess       : Integer;
    Fclicks           : Integer;
    Fno_mru           : Boolean;
    Fno_mfu           : Boolean;
    Fhide_from_menu   : Boolean;
    Fdsk_shortcut     : Boolean;
    Ficon             : RawUTF8;
    Fcacheicon_id     : Integer;
    Fcachelargeicon_id : Integer;
    Fonlaunch         : Byte;
    Fwindow_state     : Integer;
    Fautorun          : Byte;
    Fautorun_position : Integer;
    Fscheduler_mode   : Byte;
    Fscheduler_datetime : TDateTime;
    Frun_from_category  : Boolean;
    FHotkey           : Boolean;
    FHotkeyMod        : integer;
    FHotkeyCode       : integer;
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    property itemtype: Integer read Ftype write Ftype;
    property parent: Integer read Fparent write Fparent;
    property position: Integer read Fposition write Fposition;
    property title: RawUTF8 read Ftitle write Ftitle;
    property path: RawUTF8 read Fpath write Fpath;
    property work_path: RawUTF8 read Fwork_path write Fwork_path;
    property parameters: RawUTF8 read Fparameters write Fparameters;
    property dateAdded: Integer read FdateAdded write FdateAdded;
    property lastModified: Integer read FlastModified write FlastModified;
    property lastAccess: Integer read FlastAccess write FlastAccess;
    property clicks: Integer read Fclicks write Fclicks;
    property no_mru: Boolean read Fno_mru write Fno_mru;
    property no_mfu: Boolean read Fno_mfu write Fno_mfu;
    property hide_from_menu: Boolean read Fhide_from_menu write Fhide_from_menu;
    property dsk_shortcut: Boolean read Fdsk_shortcut write Fdsk_shortcut;
    property icon_path: RawUTF8 read Ficon write Ficon;
    property cacheicon_id: Integer read Fcacheicon_id write Fcacheicon_id;
    property cachelargeicon_id: Integer read Fcachelargeicon_id write Fcachelargeicon_id;
    property onlaunch: Byte read Fonlaunch write Fonlaunch;
    property window_state: Integer read Fwindow_state write Fwindow_state;
    property autorun: Byte read Fautorun write Fautorun;
    property autorun_position: Integer read Fautorun_position write Fautorun_position;
    property scheduler_mode: Byte read Fscheduler_mode write Fscheduler_mode;
    property scheduler_datetime: TDateTime read Fscheduler_datetime write Fscheduler_datetime;
    property hotkey: Boolean read FHotkey write FHotkey;
    property hotkeymod: integer read FHotkeyMod write FHotkeyMod;
    property hotkeycode: integer read FHotkeyCode write FHotkeyCode;
    property run_from_category: Boolean read Frun_from_category write Frun_from_category default True;
  end;

  TSQLtbl_options = class(TSQLRecord)
  private
    //General
    FStartWithWindows   : Boolean;
    FShowPanelAtStartUp : Boolean;
    FShowMenuAtStartUp  : Boolean;
    //Main Form
    FLangID             : Word;
    FUseCustomTitle     : Boolean;
    FCustomTitleString  : RawUTF8;
    FHideTabSearch      : Boolean;
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    FListFormTop        : Integer;
    FListFormLeft       : Integer;
    FListFormWidth      : Integer;
    FListFormHeight     : Integer;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : RawUTF8;
    FTVAutoOpClCats     : Boolean; //Automatic Opening/closing categories
    FTVFont             : RawUTF8;
    //MRU
    FMRU                : Boolean;
    FSubMenuMRU         : Boolean;
    FMRUNumber          : Integer;
    //MFU
    FMFU                : Boolean;
    FSubMenuMFU         : Boolean;
    FMFUNumber          : Integer;
    //Backup
    FBackup             : Boolean;
    FBackupNumber       : Integer;
    //Other functions
    FAutorun            : Boolean;
    FCache              : Boolean;
    FScheduler          : Boolean;
    //Execution
    FActionOnExe        : TActionOnExecute;
    FRunSingleClick     : Boolean;
    //Trayicon
    FTrayIcon           : Boolean;
    FTrayUseCustomIcon  : Boolean;
    FTrayCustomIconPath : RawUTF8;
    FActionClickLeft    : Integer;
    FActionClickRight   : Integer;
    FUseClassicMenu     : Boolean;
    //Graphic Menu
    FGMTheme            : string;
    FGMFade             : Boolean;
    FGMPersonalPicture  : string;
    //Right buttons
    FGMBtnDocuments     : string;
    FGMBtnMusic         : string;
    FGMBtnPictures      : string;
    FGMBtnVideos        : string;
    FGMBtnExplore       : string;
    //HotKeys
    FHotKey             : Boolean;
    FWindowHotKey       : Boolean;
    FWindowHotKeyCode   : Integer;
    FWindowHotKeyMod    : Integer;
    FMenuHotKey         : Boolean;
    FMenuHotKeyCode     : Integer;
    FMenuHotKeyMod      : Integer;
    //Mouse Sensors
    FUseMouseSensors    : Boolean;
    FSensorLeftClick    : RawUTF8; //0 Top, 1 Left, 2 Right, 3 Bottom
    FSensorRightClick   : RawUTF8;
    Fscanfolderlastpath : RawUTF8;
    Fscanfoldersubfolders   : Boolean;
    Fscanfolderfiletypes    : RawUTF8;
    Fscanfolderexcludenames : RawUTF8;
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    //General
    property startwithwindows: Boolean read FStartWithWindows write FStartWithWindows;
    property showpanelatstartup: Boolean read FShowPanelAtStartUp write FShowPanelAtStartUp;
    property showmenuatstartup: Boolean read FShowMenuAtStartUp write FShowMenuAtStartUp;
    // Main Form
    property langid: Word read FLangID write FLangID;
    property usecustomtitle: Boolean read FUseCustomTitle write FUseCustomTitle;
    property customtitlestring : RawUTF8 read FCustomTitleString write FCustomTitleString;
    property hidetabsearch: Boolean read FHideTabSearch write FHideTabSearch;
    // Main Form - Position and size
    property holdsize: Boolean read FHoldSize write FHoldSize;
    property alwaysontop: Boolean read FAlwaysOnTop write FAlwaysOnTop;
    property listformtop: Integer read FListFormTop write FListFormTop;
    property listformleft: Integer read FListFormLeft write FListFormLeft;
    property listformwidth: Integer read FListFormWidth write FListFormWidth;
    property listformheight: Integer read FListFormHeight write FListFormHeight;
    // Main Form - Treevew
    property tvbackground: Boolean read FTVBackground write FTVBackground;
    property tvbackgroundpath: RawUTF8 read FTVBackgroundPath write FTVBackgroundPath;
    property tvautoopclcats: Boolean read FTVAutoOpClCats write FTVAutoOpClCats;
    property tvfont: RawUTF8 read FTVFont write FTVFont;
    // MRU
    property mru: Boolean read FMRU write FMRU;
    property submenumru: Boolean read FSubMenuMRU write FSubMenuMRU;
    property mrunumber: Integer read FMRUNumber write FMRUNumber;
    // MFU
    property mfu: Boolean read FMFU write FMFU;
    property submenumfu: Boolean read FSubMenuMFU write FSubMenuMFU;
    property mfunumber: Integer read FMFUNumber write FMFUNumber;
    // Backup
    property backup: Boolean read FBackup write FBackup;
    property backupnumber: Integer read FBackupNumber write FBackupNumber;
    // Other functions
    property autorun: Boolean read FAutorun write FAutorun;
    property cache: Boolean read FCache write FCache;
    property scheduler: Boolean read FScheduler write FScheduler;
    // Execution
    property actiononexe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property runsingleclick: Boolean read FRunSingleClick write FRunSingleClick;
    // Trayicon
    property trayicon: Boolean read FTrayIcon write FTrayIcon;
    property trayusecustomicon: Boolean read FTrayUseCustomIcon write FTrayUseCustomIcon;
    property traycustomiconpath: RawUTF8 read FTrayCustomIconPath write FTrayCustomIconPath;
    property actionclickleft: Integer read FActionClickLeft write FActionClickLeft;
    property actionclickright: Integer read FActionClickRight write FActionClickRight;
    property useclassicmenu: Boolean read Fuseclassicmenu write Fuseclassicmenu;
    //Graphic Menu
    property gmtheme: string read FGMTheme write FGMTheme;
    property gmfade: Boolean read FGMFade write FGMFade;
    property gmpersonalpicture: string read FGMPersonalPicture write FGMPersonalPicture;
    //Right buttons
    property gmbtndocuments: string read FGMBtnDocuments write FGMBtnDocuments;
    property gmbtnpictures: string read FGMBtnPictures write FGMBtnPictures;
    property gmbtnmusic: string read FGMBtnMusic write FGMBtnMusic;
    property gmbtnvideos: string read FGMBtnVideos write FGMBtnVideos;
    property gmbtnexplore: string read FGMBtnExplore write FGMBtnExplore;
    //Hot Keys
    property hotkey: Boolean read FHotKey write FHotKey;
    property windowhotkey: Boolean read FWindowHotKey write FWindowHotKey;
    property windowhotkeycode: Integer read FWindowHotKeyCode write FWindowHotKeyCode;
    property windowhotkeymod: Integer read FWindowHotKeyMod write FWindowHotKeyMod;
    property menuhotkey: Boolean read FMenuHotKey write FMenuHotKey;
    property menuhotkeycode: Integer read FMenuHotKeyCode write FMenuHotKeyCode;
    property menuhotkeymod: Integer read FMenuHotKeyMod write FMenuHotKeyMod;
    //Mouse Sensor
    property usemousesensors: Boolean read FUseMouseSensors write FUseMouseSensors;
    property mousesensorleft:RawUTF8 read FSensorLeftClick write FSensorLeftClick;
    property mousesensorright:RawUTF8 read FSensorRightClick write FSensorRightClick;
    //Scan Folder
    property scanfolderlastpath: RawUTF8 read Fscanfolderlastpath write Fscanfolderlastpath;
    property scanfoldersubfolders: Boolean read Fscanfoldersubfolders write Fscanfoldersubfolders;
    property scanfolderfiletypes: RawUTF8 read Fscanfolderfiletypes write Fscanfolderfiletypes;
    property scanfolderexcludenames: RawUTF8 read Fscanfolderexcludenames write Fscanfolderexcludenames;
  end;

  { TDBManager }

  TDBManager = class
  private
    FDBFileName : string;
    FDatabase   : TSQLRest;
    FSQLModel   : TSQLModel;
    FDBVersion  : TVersionInfo;
    FASuiteVersion : TVersionInfo;
    procedure InternalLoadVersion;
    procedure InternalLoadData(Tree: TBaseVirtualTree);
    procedure InternalLoadListItems(Tree: TBaseVirtualTree; ID: Integer;
                            ParentNode: PVirtualNode; IsImport: Boolean = false);
    procedure InternalLoadOptions;
    procedure InternalSaveListItems(Tree:TBaseVirtualTree; ANode: PVirtualNode;AParentID: Int64);
    procedure InternalSaveData(Tree: TBaseVirtualTree; ANode: PVirtualNode;
      AParentID: Int64);
    procedure UpdateFileRecord(AData: TvBaseNodeData;AIndex, AParentID: Integer);
    procedure InsertFileRecord(AData: TvBaseNodeData;AIndex, AParentID: Integer);
    procedure InternalSaveVersion;
    procedure InternalSaveOptions;
    procedure ClearTable(SQLRecordClass:TSQLRecordClass);
    procedure UTF8ToMouseSensors(StringSensors: RawUTF8; MouseButton: TMouseButton);
    function  MouseSensorsToUTF8(MouseButton: TMouseButton): RawUTF8;
  public
    constructor Create(const DBFilePath: string);
    destructor Destroy; override;
    property DBFileName: string read FDBFileName write FDBFileName;
    property Database: TSQLRest read FDatabase write FDatabase;
    procedure DoBackupList;
    procedure LoadData(Tree: TBaseVirtualTree);
    function  SaveData(Tree: TBaseVirtualTree): Boolean;
    procedure DeleteItem(aID: Integer);
    procedure ImportData(Tree: TBaseVirtualTree); //For frmImportList
    procedure ImportOptions; //For frmImportList
  end;

var
  DBManager: TDBManager;

implementation

uses
  AppConfig, ulAppConfig, ulFileFolder, ulCommonUtils, ulTreeView, Main, udImages,
  ulStringUtils;

{ TDBManager }

constructor TDBManager.Create(const DBFilePath: String);
begin
  FDBFileName := DBFilePath;
  //Load sqlite3 database and create missing tables
  FSQLModel := TSQLModel.Create([TSQLtbl_version, TSQLtbl_files, TSQLtbl_options]);
  FDatabase := TSQLRestServerDB.Create(FSQLModel,FDBFileName);
  TSQLRestServerDB(fDatabase).CreateMissingTables(0);
  FASuiteVersion := TVersionInfo.Create;
end;

procedure TDBManager.DeleteItem(aID: Integer);
begin
  FDatabase.Delete(TSQLtbl_files,aID);
end;

destructor TDBManager.Destroy;
begin
  inherited;
  FDatabase.Free;
  FSQLModel.Free;
  FDBVersion.Free;
  FASuiteVersion.Free;
end;

procedure TDBManager.DoBackupList;
begin
  //Backup list and old delete backup
  if (Config.Backup) then
  begin
    CopyFile(PChar(FDBFileName),
             PChar(Format(SUITE_BACKUP_PATH + BACKUP_FILE,[GetDateTime])),false);
    DeleteOldBackups(Config.BackupNumber);
  end;
end;

procedure TDBManager.ImportData(Tree: TBaseVirtualTree);
begin
  try
    InternalLoadListItems(Tree, 0, nil, true);
  except
    on E : Exception do
      ShowMessageFmt(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message],True);
  end;
end;

procedure TDBManager.ImportOptions;
begin
  try
    InternalLoadOptions;
  except
    on E : Exception do
      ShowMessageFmt(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message],True);
  end;
end;

procedure TDBManager.UTF8ToMouseSensors(StringSensors: RawUTF8; MouseButton: TMouseButton);
var
  Strs: TStringList;
  I: Integer;
begin
  //Mouse Sensor
  Strs := TStringList.Create;
  try
    Strs.Text := StringReplace(UTF8ToString(StringSensors), '.', ''#10'', [rfReplaceAll]);
    if MouseButton = mbLeft then
    begin
      for I := 0 to 3 do
        Config.SensorLeftClick[i] := StrToInt(Strs[I]);
    end
    else begin
      for I := 0 to 3 do
        Config.SensorRightClick[i] := StrToInt(Strs[I]);
    end;
  finally
    Strs.Free;
  end;
end;

procedure TDBManager.ClearTable(SQLRecordClass:TSQLRecordClass);
var
  SQLData: TSQLRecord;
begin
  SQLData := SQLRecordClass.CreateAndFillPrepare(FDatabase, '');
  try
    while SQLData.FillOne do
      FDatabase.Delete(SQLRecordClass, SQLData.ID);
  finally
    SQLData.Free;
  end;
end;

procedure TDBManager.InsertFileRecord(AData: TvBaseNodeData; AIndex,
  AParentID: Integer);
var
  SQLFilesData : TSQLtbl_files;
begin
  SQLFilesData := TSQLtbl_files.Create;
  try
    //Add base fields
    SQLFilesData.itemtype := Ord(AData.DataType);
    SQLFilesData.parent   := AParentID;
    SQLFilesData.position := AIndex;
    SQLFilesData.title    := StringToUTF8(AData.Name);
    //Add specific category and file fields
    if AData.DataType <> vtdtSeparator then
    begin
      //Add time fields
      SQLFilesData.dateAdded      := AData.UnixAddDate;
      SQLFilesData.lastModified   := AData.UnixEditDate;
      SQLFilesData.hide_from_menu := AData.HideFromMenu;
      //Add category and file fields
      with TvCustomRealNodeData(AData) do
      begin
        SQLFilesData.cacheicon_id := CacheID;
        SQLFilesData.cachelargeicon_id := CacheLargeID;
        SQLFilesData.icon_path    := StringToUTF8(PathIcon);
        SQLFilesData.clicks       := ClickCount;
        SQLFilesData.window_state := WindowState;
        SQLFilesData.autorun      := Ord(Autorun);
        SQLFilesData.autorun_position := AutorunPos;
        SQLFilesData.onlaunch     := Ord(ActionOnExe);
        SQLFilesData.scheduler_mode := Ord(SchMode);
        SQLFilesData.scheduler_datetime := SchDateTime;
        SQLFilesData.hotkey       := Hotkey;
        SQLFilesData.hotkeymod    := HotkeyMod;
        SQLFilesData.hotkeycode   := HotkeyCode;
      end;
      //Add file fields
      if (AData.DataType = vtdtFile) then
      begin
        with TvFileNodeData(AData) do
        begin
          SQLFilesData.path       := StringToUTF8(PathExe);
          SQLFilesData.work_path  := StringToUTF8(WorkingDir);
          SQLFilesData.parameters := StringToUTF8(Parameters);
          SQLFilesData.dsk_shortcut := ShortcutDesktop;
          SQLFilesData.no_mru     := NoMRU;
          SQLFilesData.no_mfu     := NoMFU;
          SQLFilesData.lastAccess := MRUPosition;
          SQLFilesData.run_from_category := RunFromCategory;
        end;
      end;
    end;
  finally
    //Set ID, ParentID and position
    AData.ID       := FDatabase.Add(SQLFilesData,true);
    AData.ParentID := AParentID;
    AData.Position := AIndex;
    SQLFilesData.Free;
  end;
end;

procedure TDBManager.InternalLoadData(Tree: TBaseVirtualTree);
begin
  try
    //Load Database version
    InternalLoadVersion;
    //Load Options
    InternalLoadOptions;
    //Load list
    InternalLoadListItems(Tree, 0, nil, false);
  except
    on E : Exception do
      ShowMessageFmt(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message],True);
  end;
end;

procedure TDBManager.InternalLoadListItems(Tree: TBaseVirtualTree; ID: Integer;
  ParentNode: PVirtualNode; IsImport: Boolean = false);
var
  SQLFilesData : TSQLtbl_files;
  nType    : TvTreeDataType;
  vData    : TvBaseNodeData;
  Node     : PVirtualNode;
begin
  //Get files from DBTable and order them by parent, position
  SQLFilesData := TSQLtbl_files.CreateAndFillPrepare(FDatabase,'parent=? ORDER BY parent, position',[ID]);
  try
    //Get files and its properties
    while SQLFilesData.FillOne do
    begin
      nType := TvTreeDataType(SQLFilesData.itemtype);
      Node  := Tree.AddChild(ParentNode, CreateNodeData(nType));
      vData := PBaseData(Tree.GetNodeData(Node)).Data;
      PBaseData(Tree.GetNodeData(Node)).Data.pNode := Node;
      if IsImport then
        Tree.CheckType[Node] := ctTriStateCheckBox
      else
        if (nType <> vtdtSeparator) then
        begin
          TvCustomRealNodeData(vData).CacheID := SQLFilesData.cacheicon_id;
          TvCustomRealNodeData(vData).CacheLargeID := SQLFilesData.cacheLargeicon_id;
        end;
      // generic fields
      vData.Name          := UTF8ToString(SQLFilesData.title);
      vData.id            := SQLFilesData.ID;
      vData.ParentID      := id;
      vData.Position      := Node.Index;
      vData.UnixAddDate   := SQLFilesData.dateAdded;
      vData.UnixEditDate  := SQLFilesData.lastModified;
      vData.HideFromMenu  := SQLFilesData.hide_from_menu;
      if (nType <> vtdtSeparator) then
      begin
        with TvCustomRealNodeData(vData) do
        begin
          PathIcon    := UTF8ToString(SQLFilesData.icon_path);
          AutorunPos  := SQLFilesData.autorun_position;
          Autorun     := TAutorunType(SQLFilesData.autorun);
          SchMode     := TSchedulerMode(SQLFilesData.scheduler_mode);
          SchDateTime := SQLFilesData.scheduler_datetime;
          HotkeyMod   := SQLFilesData.hotkeymod;
          HotkeyCode  := SQLFilesData.hotkeycode;
          Hotkey      := SQLFilesData.hotkey;
          WindowState := SQLFilesData.window_state;
          ActionOnExe := TActionOnExecute(SQLFilesData.onlaunch);
          ClickCount  := SQLFilesData.clicks;
        end;
        if (nType = vtdtFile) then
        begin
          with TvFileNodeData(vData) do
          begin
            PathExe          := UTF8ToString(SQLFilesData.path);
            Parameters       := UTF8ToString(SQLFilesData.parameters);
            WorkingDir       := UTF8ToString(SQLFilesData.work_path);
            ShortcutDesktop  := SQLFilesData.dsk_shortcut;
            NoMRU            := SQLFilesData.no_mru;
            NoMFU            := SQLFilesData.no_mfu;
            MRUPosition      := SQLFilesData.lastAccess;
            RunFromCategory  := SQLFilesData.run_from_category;
          end;
        end;
        if (nType = vtdtCategory) then
          InternalLoadListItems(Tree, vData.ID, Node, IsImport);
      end;
    end;
  finally
    SQLFilesData.Free;
  end;
end;

procedure TDBManager.InternalLoadOptions;
var
  SQLOptionsData : TSQLtbl_options;
begin
  if FDatabase.TableHasRows(TSQLtbl_version) then
  begin
    SQLOptionsData := TSQLtbl_options.CreateAndFillPrepare(FDatabase,'');
    try
      //Get options from DBTable
      while SQLOptionsData.FillOne do
      begin
        //Get GMTheme before everything (so ASuite know where icons folder)
        Config.GMTheme            := SQLOptionsData.gmtheme;
        //General
        Config.StartWithWindows   := SQLOptionsData.startwithwindows;
        Config.ShowPanelAtStartUp := SQLOptionsData.showpanelatstartup;
        Config.ShowMenuAtStartUp  := SQLOptionsData.showmenuatstartup;
        //Main Form
        Config.LangID             := SQLOptionsData.langid;
        LangManager.LanguageID    := Config.LangID;
        Config.CustomTitleString  := UTF8ToString(SQLOptionsData.customtitlestring);
        Config.UseCustomTitle     := SQLOptionsData.usecustomtitle;
        Config.HideTabSearch      := SQLOptionsData.hidetabsearch;
        //Main Form - Position and size
        Config.HoldSize    := SQLOptionsData.holdsize;
        Config.AlwaysOnTop := SQLOptionsData.alwaysontop;
        //frmMain's size
        frmMain.Width      := SQLOptionsData.listformwidth;
        frmMain.Height     := SQLOptionsData.listformheight;
        //FrmMain's position
        if Not(FileExists(SUITE_LIST_PATH)) then
          frmMain.Position := poDesigned
        else
          frmMain.Position := poDesktopCenter;
        SetFormPosition(frmMain, SQLOptionsData.listformleft, SQLOptionsData.listformtop);
        //Main Form - Treevew
        Config.TVBackgroundPath   := UTF8ToString(SQLOptionsData.tvbackgroundpath);
        Config.TVBackground       := SQLOptionsData.tvbackground;
        Config.TVAutoOpClCats     := SQLOptionsData.tvautoopclcats;
        //Treeview Font
        Config.TVFont.Free;
        Config.TVFont         := StrToFont(UTF8ToString(SQLOptionsData.tvfont));
        //MRU
        Config.MRU            := SQLOptionsData.mru;
        Config.SubMenuMRU     := SQLOptionsData.submenumru;
        Config.MRUNumber      := SQLOptionsData.mrunumber;
        //MFU
        Config.MFU            := SQLOptionsData.mfu;
        Config.SubMenuMFU     := SQLOptionsData.submenumfu;
        Config.MFUNumber      := SQLOptionsData.mfunumber;
        //Backup
        Config.Backup         := SQLOptionsData.backup;
        Config.BackupNumber   := SQLOptionsData.backupnumber;
        //Other functions
        Config.Autorun        := SQLOptionsData.autorun;
        Config.Cache          := SQLOptionsData.cache;
        Config.Scheduler      := SQLOptionsData.scheduler;
        //Execution
        Config.ActionOnExe    := TActionOnExecute(SQLOptionsData.actiononexe);
        Config.RunSingleClick := SQLOptionsData.runsingleclick;
        //Trayicon
        Config.TrayIcon           := SQLOptionsData.trayicon;
        Config.TrayCustomIconPath := UTF8ToString(SQLOptionsData.traycustomiconpath);
        Config.TrayUseCustomIcon  := SQLOptionsData.trayusecustomicon;
        Config.ActionClickLeft    := SQLOptionsData.actionclickleft;
        Config.ActionClickRight   := SQLOptionsData.actionclickright;
        Config.UseClassicMenu     := SQLOptionsData.useclassicmenu;
        //Graphic Menu
        Config.GMFade             := SQLOptionsData.gmfade;
        Config.GMPersonalPicture  := SQLOptionsData.gmpersonalpicture;
        //Right buttons
        Config.GMBtnDocuments     := SQLOptionsData.gmbtndocuments;
        Config.GMBtnPictures      := SQLOptionsData.gmbtnpictures;
        Config.GMBtnMusic         := SQLOptionsData.gmbtnmusic;
        Config.GMBtnVideos        := SQLOptionsData.gmbtnvideos;
        Config.GMBtnExplore       := SQLOptionsData.gmbtnexplore;
        //Hot Keys
        Config.HotKey             := SQLOptionsData.HotKey;
        Config.WindowHotKeyCode   := SQLOptionsData.WindowHotKeyCode;
        Config.WindowHotKeyMod    := SQLOptionsData.WindowHotKeyMod;
        Config.WindowHotKey       := SQLOptionsData.WindowHotKey;
        Config.MenuHotKeyCode     := SQLOptionsData.MenuHotKeyCode;
        Config.MenuHotKeyMod      := SQLOptionsData.MenuHotKeyMod;
        Config.MenuHotKey         := SQLOptionsData.MenuHotKey;
        //Mouse sensors
        Config.UseMouseSensors    := SQLOptionsData.UseMouseSensors;
        UTF8ToMouseSensors(SQLOptionsData.mousesensorleft,mbLeft);
        UTF8ToMouseSensors(SQLOptionsData.mousesensorright,mbRight);
        //Scan Folder
        Config.ScanFolderLastPath   := UTF8ToString(SQLOptionsData.scanfolderlastpath);
        Config.ScanFolderSubFolders := SQLOptionsData.scanfoldersubfolders;
        Config.ScanFolderFileTypes.Text    := UTF8ToString(SQLOptionsData.scanfolderfiletypes);
        Config.ScanFolderExcludeNames.Text := UTF8ToString(SQLOptionsData.scanfolderexcludenames);
      end
    finally
      SQLOptionsData.Free;
      Config.UpdateSensors;
    end;
  end
  else begin
    Config.LangID  := 1033;
    LangManager.LanguageID := Config.LangID;
    Config.GMTheme := 'Default';
    Config.Changed := True;
  end;
end;

procedure TDBManager.LoadData(Tree: TBaseVirtualTree);
begin
  //List & Options
  DBManager.InternalLoadData(Tree);
end;

function TDBManager.MouseSensorsToUTF8(MouseButton: TMouseButton): RawUTF8;
var
  I: Integer;
  SensorString: String;
begin
  SensorString := '';
  for I := 0 to 3 do
  begin
    if MouseButton = mbLeft then
      SensorString := SensorString + IntToStr(Config.SensorLeftClick[I])
    else
      if MouseButton = mbRight then
        SensorString := SensorString + IntToStr(Config.SensorRightClick[I]);
    if I <> 3 then
      SensorString := SensorString + '.'
  end;
  Result := StringToUTF8(SensorString);
end;

procedure TDBManager.InternalLoadVersion;
var
  SQLVersionData: TSQLtbl_version;
begin
  if FDatabase.TableHasRows(TSQLtbl_version) then
  begin
    //Get sql data and get version info
    SQLVersionData := TSQLtbl_version.CreateAndFillPrepare(FDatabase,'');
    try
      SQLVersionData.FillOne;
      //Create FDBVersion with db version info
      FDBVersion := TVersionInfo.Create(SQLVersionData.Major,
                                        SQLVersionData.Minor,
                                        SQLVersionData.Release,
                                        SQLVersionData.Build);
    finally
      SQLVersionData.Free;
    end;
  end
  else begin
    //Create FDBVersion with actual ASuite version info
    FDBVersion := TVersionInfo.Create;
  end;
end;

procedure TDBManager.InternalSaveData(Tree: TBaseVirtualTree;
  ANode: PVirtualNode; AParentID: Int64);
begin
  try
    //Create and open Sqlite3Dataset
    if FDatabase.TransactionBegin(TSQLtbl_files,1) then
    begin
      InternalSaveListItems(Tree, Anode, AParentID);
      //If settings is changed, insert it else (if it exists) update it
      if Config.Changed then
        InternalSaveOptions;
      //Save new version info
      InternalSaveVersion;
      //Commit data in sqlite database
      FDatabase.Commit(1);
    end;
  except
    on E : Exception do begin
      ShowMessageFmt(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message],True);
      FDatabase.Rollback(1);
    end;
  end;
end;

procedure TDBManager.InternalSaveListItems(Tree: TBaseVirtualTree;
  ANode: PVirtualNode; AParentID: Int64);
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
        InsertFileRecord(vData, Node.Index, AParentID)
      else
        if ((vData.Changed) or (vData.Position <> Node.Index) or (vData.ParentID <> AParentID)) then
          UpdateFileRecord(vData, Node.Index, AParentID);
      //If type is category then process sub-nodes
      if (vData.DataType = vtdtCategory) then
        InternalSaveListItems(Tree, Node.FirstChild, vData.ID);
    except
      on E : Exception do
        ShowMessageFmt(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message],True);
    end;
    Node := Node.NextSibling;
  end;
end;

procedure TDBManager.InternalSaveOptions;
var
  SQLOptionsData : TSQLtbl_options;
begin
  //Clear options table
  if FDatabase.TableHasRows(TSQLtbl_options) then
    ClearTable(TSQLtbl_options);
  //Save ASuite options
  SQLOptionsData := TSQLtbl_options.Create;
  try
    //general
    SQLOptionsData.startwithwindows   := Config.StartWithWindows;
    SQLOptionsData.showpanelatstartup := Config.ShowPanelAtStartUp;
    SQLOptionsData.showmenuatstartup  := Config.ShowMenuAtStartUp;
    //main form
    SQLOptionsData.langid            := Config.LangID;
    SQLOptionsData.usecustomtitle    := Config.UseCustomTitle;
    SQLOptionsData.customtitlestring := StringToUTF8(Config.CustomTitleString);
    SQLOptionsData.hidetabsearch     := Config.HideTabSearch;
    //main form - position and size
    SQLOptionsData.holdsize          := Config.HoldSize;
    SQLOptionsData.alwaysontop       := Config.AlwaysOnTop;
    SQLOptionsData.listformtop       := frmMain.Top;
    SQLOptionsData.listformleft      := frmMain.Left;
    SQLOptionsData.listformwidth     := frmMain.Width;
    SQLOptionsData.listformheight    := frmMain.Height;
    //main form - treevew
    SQLOptionsData.tvbackground      := Config.TVBackground;
    SQLOptionsData.tvbackgroundpath  := StringToUTF8(Config.TVBackgroundPath);
    SQLOptionsData.tvautoopclcats    := Config.TVAutoOpClCats;
    SQLOptionsData.tvfont            := StringToUTF8(FontToStr(Config.TVFont));
    //mru
    SQLOptionsData.mru          := Config.MRU;
    SQLOptionsData.submenumru   := Config.SubMenuMRU;
    SQLOptionsData.mrunumber    := Config.MRUNumber;
    //mfu
    SQLOptionsData.mfu          := Config.MFU;
    SQLOptionsData.submenumfu   := Config.SubMenuMFU;
    SQLOptionsData.mfunumber    := Config.MFUNumber;
    //backup
    SQLOptionsData.backup       := Config.Backup;
    SQLOptionsData.backupnumber := Config.BackupNumber;
    //other functions
    SQLOptionsData.autorun      := Config.Autorun;
    SQLOptionsData.cache        := Config.Cache;
    SQLOptionsData.scheduler    := Config.Scheduler;
    //execution
    SQLOptionsData.actiononexe    := TActionOnExecute(Config.ActionOnExe);
    SQLOptionsData.runsingleclick := Config.RunSingleClick;
    //trayicon
    SQLOptionsData.trayicon := Config.TrayIcon;
    SQLOptionsData.trayusecustomicon  := Config.TrayUseCustomIcon;
    SQLOptionsData.traycustomiconpath := StringToUTF8(Config.TrayCustomIconPath);

    //Debug code
    Assert((Config.ActionClickLeft <> -1),  'Config.ActionClickLeft is -1');
    Assert((Config.ActionClickRight <> -1), 'Config.ActionClickRight is -1');

    SQLOptionsData.actionclickleft    := Config.ActionClickLeft;
    SQLOptionsData.actionclickright   := Config.ActionClickRight;
    SQLOptionsData.useclassicmenu     := Config.UseClassicMenu;    
    //Graphic Menu
    SQLOptionsData.gmtheme            := Config.GMTheme;
    SQLOptionsData.gmfade             := Config.GMFade;
    SQLOptionsData.gmpersonalpicture  := Config.GMPersonalPicture;
    //Right buttons
    SQLOptionsData.gmbtndocuments     := Config.GMBtnDocuments;
    SQLOptionsData.gmbtnpictures      := Config.GMBtnPictures;
    SQLOptionsData.gmbtnmusic         := Config.GMBtnMusic;
    SQLOptionsData.gmbtnvideos        := Config.GMBtnVideos;
    SQLOptionsData.gmbtnexplore       := Config.GMBtnExplore;
    //Hot Keys
    SQLOptionsData.HotKey             := Config.HotKey;
    SQLOptionsData.WindowHotKey       := Config.WindowHotKey;
    SQLOptionsData.WindowHotKeyCode   := Config.WindowHotKeyCode;
    SQLOptionsData.WindowHotKeyMod    := Config.WindowHotKeyMod;
    SQLOptionsData.MenuHotKey         := Config.MenuHotKey;
    SQLOptionsData.MenuHotKeyCode     := Config.MenuHotKeyCode;
    SQLOptionsData.MenuHotKeyMod      := Config.MenuHotKeyMod;
    //Mouse Sensor
    SQLOptionsData.usemousesensors    := Config.UseMouseSensors;
    SQLOptionsData.mousesensorleft    := MouseSensorsToUTF8(mbLeft);
    SQLOptionsData.mousesensorright   := MouseSensorsToUTF8(mbRight);
    //Scan Folder
    SQLOptionsData.scanfolderlastpath     := StringToUTF8(Config.ScanFolderLastPath);
    SQLOptionsData.scanfoldersubfolders   := Config.ScanFolderSubFolders;
    SQLOptionsData.scanfolderfiletypes    := StringToUTF8(Config.ScanFolderFileTypes.Text);
    SQLOptionsData.scanfolderexcludenames := StringToUTF8(Config.ScanFolderExcludeNames.Text);

    FDatabase.Add(SQLOptionsData,true);
  finally
    SQLOptionsData.Free;
  end;
end;

procedure TDBManager.InternalSaveVersion;
var
  SQLVersionData: TSQLtbl_version;
begin
  //If necessary, clear version table
  if FDatabase.TableHasRows(TSQLtbl_version) then
  begin
    if CompareVersionInfo(FDBVersion,FASuiteVersion) <> 0 then
      ClearTable(TSQLtbl_version)
    else
      Exit;
  end;
  //Insert ASuite version info
  SQLVersionData := TSQLtbl_version.Create;
  try
    SQLVersionData.Major   := FASuiteVersion.Major;
    SQLVersionData.Minor   := FASuiteVersion.Minor;
    SQLVersionData.Release := FASuiteVersion.Release;
    SQLVersionData.Build   := FASuiteVersion.Build;
    FDatabase.Add(SQLVersionData,true);
  finally
    SQLVersionData.Free;
  end;
end;

function TDBManager.SaveData(Tree: TBaseVirtualTree): Boolean;
begin
  Result := True;
  //If launcher is in ReadOnlyMode, exit from this function
  if (Config.ReadOnlyMode) then
    Exit;
  //List & Options
  try
    DBManager.InternalSaveData(Tree,Tree.GetFirst,0);
  except
    Result := False;
  end;
end;

procedure TDBManager.UpdateFileRecord(AData: TvBaseNodeData; AIndex,
  AParentID: Integer);
var
  SQLFilesData : TSQLtbl_files;
begin
  //Select only file record by ID
  SQLFilesData := TSQLtbl_files.CreateAndFillPrepare(FDatabase,'id=?',[AData.ID]);
  try
    if SQLFilesData.FillOne then
    begin
      //Update base fields
      SQLFilesData.parent   := AParentID;
      SQLFilesData.position := AIndex;
      SQLFilesData.title    := StringToUTF8(AData.Name);
      SQLFilesData.hide_from_menu := AData.HideFromMenu;
      //Update specific fields
      if AData.DataType <> vtdtSeparator then
      begin
        //Update time fields
        SQLFilesData.dateAdded    := AData.UnixAddDate;
        SQLFilesData.lastModified := AData.UnixEditDate;
        //Update category and file fields
        with TvCustomRealNodeData(AData) do
        begin
          SQLFilesData.cacheicon_id := CacheID;
          SQLFilesData.cachelargeicon_id := CacheLargeID;
          SQLFilesData.icon_path    := StringToUTF8(PathIcon);
          SQLFilesData.clicks       := ClickCount;
          SQLFilesData.window_state := WindowState;
          SQLFilesData.autorun      := Ord(Autorun);
          SQLFilesData.autorun_position := AutorunPos;
          SQLFilesData.onlaunch     := Ord(ActionOnExe);
          SQLFilesData.scheduler_mode := Ord(SchMode);
          SQLFilesData.scheduler_datetime := SchDateTime;
          SQLFilesData.hotkey       := Hotkey;
          SQLFilesData.hotkeymod    := HotkeyMod;
          SQLFilesData.hotkeycode   := HotkeyCode;
        end;
        //Update file specific fields
        if (AData.DataType = vtdtFile) then
        begin
          with TvFileNodeData(AData) do
          begin
            SQLFilesData.path       := StringToUTF8(PathExe);
            SQLFilesData.work_path  := StringToUTF8(WorkingDir);
            SQLFilesData.parameters := StringToUTF8(Parameters);
            SQLFilesData.dsk_shortcut := ShortcutDesktop;
            SQLFilesData.no_mru     := NoMRU;
            SQLFilesData.no_mfu     := NoMFU;
            SQLFilesData.lastAccess := MRUPosition;
            SQLFilesData.run_from_category := RunFromCategory;
          end;
        end;
      end;
    end;
  finally
    //Update data
    FDatabase.Update(SQLFilesData);
    //Update node
    AData.ParentID := AParentID;
    AData.Position := AIndex;
    SQLFilesData.Free;
  end;
end;

initialization

finalization
  DBManager.Free

end.
