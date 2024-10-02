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

unit Database.List;

{$MODE DelphiUnicode}

interface

uses
  mormot.orm.core, Database.Manager, VirtualTrees, SysUtils,
  Dialogs, Classes, NodeDataTypes.Base, mormot.core.log, mormot.core.base;

type

  { TSQLtbl_list }

  TSQLtbl_list = class(TOrm) //Table tbl_list
  private
    Fdescription: RawUTF8;
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
    Ficon_path        : RawUTF8;
    Fcache_icon_crc   : Integer;
    Fonlaunch         : Byte;
    Fwindow_state     : Integer;
    Fautorun          : Byte;
    Fautorun_position : Integer;
    Fscheduler_mode   : Byte;
    Fscheduler_datetime : TDateTime;
    Frun_from_category  : Boolean;
    Fhotkey             : Word;
    Fenvironment_vars   : RawUTF8;

    procedure LoadDataFromNode(AData: TvBaseNodeData; AIndex, AParentID: Integer);

    class procedure LoadItemsByParentID(Tree: TBaseVirtualTree; ADBManager: TDBManager;
                                        ID: Integer; ParentNode: PVirtualNode;
                                        IsImport: Boolean = False);
    class procedure SaveItemsByParentID(Tree:TBaseVirtualTree; ADBManager: TDBManager;
                                        ANode: PVirtualNode; AParentID: Int64);

    class procedure UpdateFileRecord(AData: TvBaseNodeData; ADBManager: TDBManager;
                                     AIndex, AParentID: Integer);
    class procedure InsertFileRecord(AData: TvBaseNodeData; ADBManager: TDBManager;
                                     AIndex, AParentID: Integer);
  public
    class procedure Load(ADBManager: TDBManager; ATree: TBaseVirtualTree; IsImport: Boolean);
    class procedure Save(ADBManager: TDBManager; ATree: TBaseVirtualTree);
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    property itemtype: Integer read Ftype write Ftype;
    property parent: Integer read Fparent write Fparent;
    property position: Integer read Fposition write Fposition;
    property title: RawUTF8 read Ftitle write Ftitle;
    property description: RawUTF8 read Fdescription write Fdescription;
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
    property icon_path: RawUTF8 read Ficon_path write Ficon_path;
    property cache_icon_crc: Integer read Fcache_icon_crc write Fcache_icon_crc;
    property onlaunch: Byte read Fonlaunch write Fonlaunch;
    property window_state: Integer read Fwindow_state write Fwindow_state;
    property autorun: Byte read Fautorun write Fautorun;
    property autorun_position: Integer read Fautorun_position write Fautorun_position;
    property scheduler_mode: Byte read Fscheduler_mode write Fscheduler_mode;
    property scheduler_datetime: TDateTime read Fscheduler_datetime write Fscheduler_datetime;
    property hotkey: Word read Fhotkey write Fhotkey;
    property run_from_category: Boolean read Frun_from_category write Frun_from_category;
    property environment_vars: RawUTF8 read Fenvironment_vars write Fenvironment_vars;
  end;

implementation

uses
  Kernel.Enumerations, VirtualTree.Methods, NodeDataTypes.Custom, VirtualTrees.Types,
  NodeDataTypes.Files, Icons.Node, Kernel.Logger, mormot.core.unicode,
  Kernel.Manager;

{ TSQLtbl_files }

class procedure TSQLtbl_list.InsertFileRecord(AData: TvBaseNodeData; ADBManager: TDBManager;
                                               AIndex, AParentID: Integer);
var
  SQLFilesData: TSQLtbl_list;
begin
  SQLFilesData := TSQLtbl_list.Create;
  try
    SQLFilesData.LoadDataFromNode(AData, AIndex, AParentID);
  finally
    //Set ID, ParentID and position
    AData.ID := ADBManager.Database.Add(SQLFilesData,true);

    //If enabled, register item's hotkey with new ID
    if AData is TvCustomRealNodeData then
      if TvCustomRealNodeData(AData).Hotkey <> 0 then
        ASuiteManager.ListManager.HotKeyItemList.AddItem(TvCustomRealNodeData(AData));

    SQLFilesData.Free;
  end;
end;

class procedure TSQLtbl_list.LoadItemsByParentID(Tree: TBaseVirtualTree; ADBManager: TDBManager;
  ID: Integer; ParentNode: PVirtualNode; IsImport: Boolean);
var
  SQLItemsData : TSQLtbl_list;
  nType    : TvTreeDataType;
  vData    : TvBaseNodeData;
  Node     : PVirtualNode;
begin
  //Get files from DBTable and order them by parent, position
  SQLItemsData := TSQLtbl_list.CreateAndFillPrepare(ADBManager.Database.orm, 'parent=? ORDER BY parent, position',[ID]);
  try
    //Get files and its properties
    while SQLItemsData.FillOne do
    begin
      nType := TvTreeDataType(SQLItemsData.itemtype);
      Node  := TVirtualTreeMethods.AddChildNodeEx(Tree, ParentNode, amInsertAfter, nType, False);
      vData := TVirtualTreeMethods.GetNodeItemData(Node, Tree);
      if IsImport then
        Tree.CheckType[Node] := ctTriStateCheckBox;
      // generic fields
      vData.Name          := UTF8DecodeToUnicodeString(SQLItemsData.title);
      vData.Description   := UTF8DecodeToUnicodeString(SQLItemsData.description);
      vData.id            := SQLItemsData.ID;
      vData.ParentID      := id;
      vData.Position      := Node.Index;
      vData.UnixAddDate   := SQLItemsData.dateAdded;
      vData.UnixEditDate  := SQLItemsData.lastModified;
      vData.HideFromMenu  := SQLItemsData.hide_from_menu;
      if (nType <> vtdtSeparator) then
      begin
        with TvCustomRealNodeData(vData) do
        begin
          PathIcon    := UTF8DecodeToUnicodeString(SQLItemsData.icon_path);
          Icon.CacheIconCRC := SQLItemsData.cache_icon_crc;
          AutorunPos  := SQLItemsData.autorun_position;
          Autorun     := TAutorunType(SQLItemsData.autorun);
          SchMode     := TSchedulerMode(SQLItemsData.scheduler_mode);
          SchDateTime := SQLItemsData.scheduler_datetime;
          Hotkey      := SQLItemsData.hotkey;
          WindowState := SQLItemsData.window_state;
          ActionOnExe := TActionOnExecute(SQLItemsData.onlaunch);
        end;
        if (nType = vtdtFile) then
        begin
          with TvFileNodeData(vData) do
          begin
            PathFile         := UTF8DecodeToUnicodeString(SQLItemsData.path);
            Parameters       := UTF8DecodeToUnicodeString(SQLItemsData.parameters);
            WorkingDir       := UTF8DecodeToUnicodeString(SQLItemsData.work_path);
            ShortcutDesktop  := SQLItemsData.dsk_shortcut;
            NoMRU            := SQLItemsData.no_mru;
            NoMFU            := SQLItemsData.no_mfu;
            LastAccess       := SQLItemsData.lastAccess;
            ClickCount       := SQLItemsData.clicks;
            RunFromCategory  := SQLItemsData.run_from_category;
            EnvironmentVars.Text := UTF8DecodeToUnicodeString(SQLItemsData.environment_vars);
          end;
        end;
        if (nType = vtdtCategory) then
          TSQLtbl_list.LoadItemsByParentID(Tree, ADBManager, vData.ID, Node, IsImport);
      end;
    end;
  finally
    SQLItemsData.Free;
  end;
end;

class procedure TSQLtbl_list.SaveItemsByParentID(Tree: TBaseVirtualTree; ADBManager: TDBManager;
  ANode: PVirtualNode; AParentID: Int64);
var
  Node  : PVirtualNode;
  vData : TvBaseNodeData;
begin
  Node  := ANode;
  while (Node <> nil) do
  begin
    vData := TVirtualTreeMethods.GetNodeItemData(Node, Tree);
    try
      //Insert or update record
      if (vData.ID < 0) then
        TSQLtbl_list.InsertFileRecord(vData, ADBManager, Node.Index, AParentID)
      else
        if ((vData.Changed) or (vData.Position <> Node.Index) or (vData.ParentID <> AParentID)) then
          TSQLtbl_list.UpdateFileRecord(vData, ADBManager, Node.Index, AParentID);
      //If type is category then process sub-nodes
      if (vData.IsCategoryItem) then
        TSQLtbl_list.SaveItemsByParentID(Tree, ADBManager, Node.FirstChild, vData.ID);
    except
      on E : Exception do
        TASuiteLogger.Exception(E);
    end;
    Node := Node.NextSibling;
  end;
end;

class procedure TSQLtbl_list.Load(ADBManager: TDBManager; ATree: TBaseVirtualTree; IsImport: Boolean);
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TSQLtbl_list.Load', nil);

  if IsImport then
    TASuiteLogger.Info('Load ASuite List from Database in VirtualTree (Import mode)', [])
  else
    TASuiteLogger.Info('Load ASuite List from Database in VirtualTree', []);

  TSQLtbl_list.LoadItemsByParentID(ATree, ADBManager, 0, nil, IsImport);
end;

procedure TSQLtbl_list.LoadDataFromNode(AData: TvBaseNodeData; AIndex, AParentID: Integer);
begin
  //Add base fields
  if Ftype <> Ord(AData.DataType) then
    Ftype := Ord(AData.DataType);
  Fparent   := AParentID;
  Fposition := AIndex;
  Ftitle    := UnicodeStringToUtf8(AData.Name);
  Fdescription := UnicodeStringToUtf8(AData.Description);
  //Add specific category and file fields
  if not(AData.IsSeparatorItem) then
  begin
    //Add time fields
    FdateAdded      := AData.UnixAddDate;
    FlastModified   := AData.UnixEditDate;
    Fhide_from_menu := AData.HideFromMenu;
    //Add category and file fields
    with TvCustomRealNodeData(AData) do
    begin
      Ficon_path      := UnicodeStringToUtf8(PathIcon);
      Fcache_icon_crc := TNodeIcon(Icon).CacheIconCRC;
      Fwindow_state   := WindowState;
      Fautorun        := Ord(Autorun);
      Fautorun_position := AutorunPos;
      Fonlaunch       := Ord(ActionOnExe);
      Fscheduler_mode := Ord(SchMode);
      Fscheduler_datetime := SchDateTime;
      Fhotkey         := Hotkey;
    end;
    //Add file fields
    if (AData.IsFileItem) then
    begin
      with TvFileNodeData(AData) do
      begin
        Fpath       := UnicodeStringToUtf8(PathFile);
        Fwork_path  := UnicodeStringToUtf8(WorkingDir);
        Fparameters := UnicodeStringToUtf8(Parameters);
        Fdsk_shortcut := ShortcutDesktop;
        Fno_mru     := NoMRU;
        Fno_mfu     := NoMFU;
        FlastAccess := LastAccess;
        Fclicks     := ClickCount;
        Frun_from_category := RunFromCategory;
        Fenvironment_vars := UnicodeStringToUtf8(EnvironmentVars.Text);
      end;
    end;
  end;
  AData.ParentID := AParentID;
  AData.Position := AIndex;
  AData.Changed  := False;
end;

class procedure TSQLtbl_list.Save(ADBManager: TDBManager; ATree: TBaseVirtualTree);
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TSQLtbl_list.Save', nil);
  TSQLtbl_list.SaveItemsByParentID(ATree, ADBManager, ATree.GetFirst, 0);
end;

class procedure TSQLtbl_list.UpdateFileRecord(AData: TvBaseNodeData; ADBManager: TDBManager;
                                               AIndex, AParentID: Integer);
var
  SQLFilesData : TSQLtbl_list;
begin
  //Select only file record by ID
  SQLFilesData := TSQLtbl_list.CreateAndFillPrepare(ADBManager.Database.orm,'id=?',[AData.ID]);
  try
    if SQLFilesData.FillOne then
      SQLFilesData.LoadDataFromNode(AData, AIndex, AParentID);
  finally
    //Update data
    ADBManager.Database.Update(SQLFilesData);
    SQLFilesData.Free;
  end;
end;

end.
