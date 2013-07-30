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

unit Main;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ComCtrls, VirtualTrees, ActiveX, AppConfig, ulNodeDataTypes, ulCommonClasses,
  UDImages, ASuiteForm, StdCtrls, Buttons, Sensor, System.UITypes, mORMotUILogin,
  ulEnumerations, Vcl.ExtCtrls, System.DateUtils;

type

  { TfrmMain }

  TfrmMain = class(TASuiteForm)
    miCheckUpdates: TMenuItem;
    miStatistics: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    sbtnSearch: TSpeedButton;
    vstList: TVirtualStringTree;
    pcList: TPageControl;
    tbList: TTabSheet;
    tbSearch: TTabSheet;
    vstSearch: TVirtualStringTree;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miHelp: TMenuItem;
    N2: TMenuItem;
    miOptions1: TMenuItem;
    miImportList: TMenuItem;
    miExit1: TMenuItem;
    miInfoASuite: TMenuItem;
    miEdit: TMenuItem;
    miAddCat1: TMenuItem;
    miAddSw1: TMenuItem;
    miDelete1: TMenuItem;
    N8: TMenuItem;
    miProperty1: TMenuItem;
    pmWindow: TPopupMenu;
    miAddCat2: TMenuItem;
    miAddSw2: TMenuItem;
    miDelete2: TMenuItem;
    N6: TMenuItem;
    miProperty2: TMenuItem;
    miAddFolder2: TMenuItem;
    miAddFolder1: TMenuItem;
    miSaveList1: TMenuItem;
    miRunSelectedSw: TMenuItem;
    miOpenFolderSw: TMenuItem;
    N9: TMenuItem;
    miAddSeparator2: TMenuItem;
    miAddSeparator1: TMenuItem;
    N3: TMenuItem;
    miSortList: TMenuItem;
    miExportList: TMenuItem;
    N4: TMenuItem;
    N1: TMenuItem;
    SaveDialog1: TSaveDialog;
    miSortItems: TMenuItem;
    N5: TMenuItem;
    miCopy2: TMenuItem;
    miCut2: TMenuItem;
    miPaste2: TMenuItem;
    N10: TMenuItem;
    miN11: TMenuItem;
    miCut1: TMenuItem;
    miCopy1: TMenuItem;
    miPaste1: TMenuItem;
    pmSearch: TPopupMenu;
    miSearchName: TMenuItem;
    miSearchExePath: TMenuItem;
    miSearchIconPath: TMenuItem;
    miSearchWorkingDirPath: TMenuItem;
    miSearchParameters: TMenuItem;
    edtSearch: TEdit;
    miRunAs: TMenuItem;
    miRunAsAdmin: TMenuItem;
    tmScheduler: TTimer;
    procedure miOptionsClick(Sender: TObject);
    procedure miStatisticsClick(Sender: TObject);
    procedure pcListChange(Sender: TObject);
    procedure miImportListClick(Sender: TObject);
    procedure miSaveListClick(Sender: TObject);
    procedure vstListDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure miPropertyClick(Sender: TObject);
    procedure DeleteSwCat(Sender: TObject);
    procedure AddFolder(Sender: TObject);
    procedure AddSoftware(Sender: TObject);
    procedure AddCategory(Sender: TObject);
    procedure vstSearchGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RunDoubleClick(Sender: TObject);
    procedure RunSingleClick(Sender: TObject);
    procedure pmWindowPopup(Sender: TObject);
    procedure miRunSelectedSwClick(Sender: TObject);
    procedure miAddSeparator2Click(Sender: TObject);
    procedure vstSearchGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstSearchCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure miSortListClick(Sender: TObject);
    procedure miExportListClick(Sender: TObject);
    procedure vstListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure FormDestroy(Sender: TObject);
    procedure miSortItemsClick(Sender: TObject);
    procedure vstListKeyPress(Sender: TObject; var Key: Char);
    procedure miCopy2Click(Sender: TObject);
    procedure miPaste2Click(Sender: TObject);
    procedure miCut2Click(Sender: TObject);
    procedure vstListSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure vstListLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure miEditClick(Sender: TObject);
    procedure miInfoASuiteClick(Sender: TObject);
    procedure btnedtSearchRightButtonClick(Sender: TObject);
    procedure ChangeSearchTextHint(Sender: TObject);
    procedure vstListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure btnedtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure vstListExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure vstSearchHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstListDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure miRunAsClick(Sender: TObject);
    procedure miRunAsAdminClick(Sender: TObject);
    procedure tmSchedulerTimer(Sender: TObject);
    procedure vstListEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure miOpenFolderSwClick(Sender: TObject);
    procedure vstListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
  private
    { Private declarations }
    function  GetActiveTree: TBaseVirtualTree;
    procedure RunStartupProcess;
    procedure RunShutdownProcess;
    procedure ExecuteSelectedNode(TreeView: TBaseVirtualTree;ProcessInfo: TProcessInfo);
  public
    { Public declarations }
    procedure ShowMainForm(Sender: TObject);
    procedure HideMainForm;
    procedure RunNormalSw(TreeView: TBaseVirtualTree);
    procedure RunAs(TreeView: TBaseVirtualTree);
    procedure RunAsAdmin(TreeView: TBaseVirtualTree);
    procedure OpenFolder(TreeView: TBaseVirtualTree);
    function  ShowItemProperty(TreeView: TBaseVirtualTree; Node: PVirtualNode = nil): Integer;
    procedure DoSearchItem(TreeSearch: TBaseVirtualTree; Keyword: string;
                           Callback: TVTGetNodeProc);
    procedure LoadGlyphs;
  end;

var
  frmMain     : TfrmMain;
  FormSensors : Array[0..3] of TfrmSensor;

implementation

uses
  Options, About, ulCommonUtils, udClassicMenu, ulExeUtils, ImportList,
  ulAppConfig, ulTreeView, ulDatabase, notifications, GraphicMenu, StatsOptionsPage,
  PropertyItem, PropertySeparator;

{$R *.dfm}

procedure TfrmMain.btnedtSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    btnedtSearchRightButtonClick(Sender);
end;

procedure TfrmMain.btnedtSearchRightButtonClick(Sender: TObject);
begin
  DoSearchItem(vstSearch,edtSearch.Text,IterateSubtreeProcs.FindNode);
end;

procedure TfrmMain.DeleteSwCat(Sender: TObject);
var
  Nodes: TNodeArray;
  I: Integer;
begin
  if (vstList.GetFirstSelected <> nil) and (MessageDlg((msgConfirm),mtWarning, [mbYes,mbNo], 0) = mrYes) then
  begin
    //Run actions (ex. remove node from MRU list) before delete nodes
    Nodes := vstList.GetSortedSelection(true);
    //Begin transaction for remove data from sqlite database
    if DBManager.Database.TransactionBegin(TSQLtbl_files,1) then
    begin
      try
        //Remove each selected items from sqlite database
        for I := High(Nodes) downto 0 do
          vstList.IterateSubtree(Nodes[I],IterateSubtreeProcs.BeforeDeleteNode,nil,[],False);
        //Commit database's updates
        DBManager.Database.Commit(1);
      except
        //Or in case of error, rollback
        DBManager.Database.RollBack(1);
      end;
    end;
    //Delete nodes and refresh list
    vstList.DeleteSelectedNodes;
    RefreshList(vstList);
  end;
end;

procedure TfrmMain.miSaveListClick(Sender: TObject);
begin;
  if DBManager.SaveData(vstList) then
    showmessage(msgSaveCompleted)
  else
    showmessage(msgErrSave,true);
end;

procedure TfrmMain.ChangeSearchTextHint(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    //Set new placeholder and SearchType
    edtSearch.TextHint := StringReplace((Sender as TMenuItem).Caption, '&', '', []);
    (Sender as TMenuItem).Checked := True;
    SearchType := TSearchType((Sender as TMenuItem).Tag);
  end;
end;

procedure TfrmMain.AddCategory(Sender: TObject);
begin
  AddNode(vstList,vtdtCategory);
end;

procedure TfrmMain.AddFolder(Sender: TObject);
begin
  AddNode(vstList,vtdtFolder);
end;

procedure TfrmMain.AddSoftware(Sender: TObject);
begin
  AddNode(vstList,vtdtFile);
end;

procedure TfrmMain.miAddSeparator2Click(Sender: TObject);
begin
  AddNode(vstList,vtdtSeparator);
end;

procedure TfrmMain.miCopy2Click(Sender: TObject);
begin
  vstList.CopyToClipBoard;
end;

procedure TfrmMain.miCut2Click(Sender: TObject);
begin
  vstList.CutToClipBoard;
end;

procedure TfrmMain.RunDoubleClick(Sender: TObject);
begin
  //Check if user click on node or expand button (+/-)
  if (Sender is TBaseVirtualTree) then
    if Not(ClickOnButtonTree((Sender as TBaseVirtualTree))) then
      RunNormalSw((Sender as TBaseVirtualTree));
end;

procedure TfrmMain.miImportListClick(Sender: TObject);
begin
  try
    Application.CreateForm(TfrmImportList, frmImportList);
    frmImportList.FormStyle := Self.FormStyle;
    frmImportList.ShowModal;
  finally
    frmImportList.Free;
    RefreshList(vstList);
  end;
end;

procedure TfrmMain.miInfoASuiteClick(Sender: TObject);
begin
  if not IsFormOpen('frmAbout') then
    Application.CreateForm(TfrmAbout, frmAbout);
  frmAbout.show;
end;

procedure TfrmMain.miRunAsAdminClick(Sender: TObject);
begin
  RunAsAdmin(GetActiveTree);
end;

procedure TfrmMain.miRunAsClick(Sender: TObject);
begin
  RunAs(GetActiveTree);
end;

procedure TfrmMain.miRunSelectedSwClick(Sender: TObject);
begin
  RunNormalSw(GetActiveTree);
end;

procedure TfrmMain.miOpenFolderSwClick(Sender: TObject);
begin
  OpenFolder(GetActiveTree);
end;

procedure TfrmMain.miOptionsClick(Sender: TObject);
begin
  try
    Application.CreateForm(TfrmOptions, frmOptions);
    frmOptions.FormStyle := Self.FormStyle;
    frmOptions.Execute();
  finally
    frmOptions.Free;
  end;
end;

procedure TfrmMain.miStatisticsClick(Sender: TObject);
begin
  try
    Application.CreateForm(TfrmOptions, frmOptions);
    frmOptions.FormStyle := Self.FormStyle;
    frmOptions.Execute(TfrmStatsOptionsPage);
  finally
    frmOptions.Free;
  end;
end;

procedure TfrmMain.miPaste2Click(Sender: TObject);
var
  NodeData : PBaseData;
begin
  NodeData := vstList.GetNodeData(vstList.FocusedNode);
  if Assigned(NodeData) then
  begin
    if NodeData.Data.DataType = vtdtCategory then
      vstList.DefaultPasteMode := amAddChildLast
    else
      vstList.DefaultPasteMode := amInsertAfter;
    end
  else
    vstList.DefaultPasteMode := amAddChildLast;
  vstList.PasteFromClipboard;
  vstList.Expanded[vstList.FocusedNode] := True;
  RefreshList(vstList);
end;

procedure TfrmMain.miSortItemsClick(Sender: TObject);
begin
  vstList.Sort(vstList.GetFirstSelected,0,sdAscending,True);
  RefreshList(vstList);
end;

procedure TfrmMain.miSortListClick(Sender: TObject);
begin
  vstList.SortTree(0,sdAscending,True);
  RefreshList(vstList);
end;

procedure TfrmMain.pcListChange(Sender: TObject);
var
  ActiveTab : Boolean;
begin
  ActiveTab := pcList.ActivePageIndex = PG_LIST;
  //Set property Visible for some components
  miEdit.Visible          := ActiveTab;
  miSortItems.Visible     := ActiveTab;
  miAddCat2.Visible       := ActiveTab;
  miAddSw2.Visible        := ActiveTab;
  miAddFolder2.Visible    := ActiveTab;
  miAddSeparator2.Visible := ActiveTab;
  miDelete2.Visible       := ActiveTab;
  miCut2.Visible          := ActiveTab;
  miCopy2.Visible         := ActiveTab;
  miPaste2.Visible        := ActiveTab;
end;

procedure TfrmMain.pmWindowPopup(Sender: TObject);
var
  NodeData : PBaseData;
  Point    : TPoint;
  HitInfo  : ThitInfo;
  TreeView : TBaseVirtualTree;
begin
  GetCursorPos(Point);
  Point    := vstList.ScreenToClient(Point);
  vstList.GetHitTestInfoAt(Point.X,Point.Y,true,HitInfo);
  //If user clicks on empty area of vstList
  if Not(hiOnItem in hitinfo.HitPositions) then
  begin
    //Set nil vstList.FocusedNode
    vstList.Selected[vstList.FocusedNode] := False;
    vstList.FocusedNode := nil;
  end;
  //Enable/disable item window menu
  if (Assigned(vstList.FocusedNode) and (pcList.ActivePageIndex = PG_LIST)) or
     (Assigned(vstSearch.FocusedNode) and (pcList.ActivePageIndex = PG_Search)) then
  begin
    TreeView := GetActiveTree;
    NodeData := GetNodeDataEx(TreeView.FocusedNode, TreeView, vstSearch, vstList);
    miProperty2.Enabled     := True;
    miRunSelectedSw.Enabled := (NodeData.Data.DataType <> vtdtSeparator);
    miRunAs.Enabled         := (NodeData.Data.DataType <> vtdtSeparator);
    miRunAsAdmin.Enabled    := (NodeData.Data.DataType <> vtdtSeparator);
    miOpenFolderSw.Enabled  := (NodeData.Data.DataType in [vtdtFile,vtdtFolder]);
  end
  else begin
    miRunSelectedSw.Enabled := False;
    miRunAs.Enabled         := False;
    miRunAsAdmin.Enabled    := False;
    miOpenFolderSw.Enabled  := False;
    miProperty2.Enabled     := False;
  end;
  miPaste2.Enabled := IsFormatInClipBoard(CF_VIRTUALTREE);
end;

procedure TfrmMain.vstSearchCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2, CatData1, CatData2: PBaseData;
  CatName1, CatName2 : String;
begin
  Data1 := GetNodeDataSearch(Node1,vstSearch,vstList);
  Data2 := GetNodeDataSearch(Node2,vstSearch,vstList);
  if (Not Assigned(Data1)) or (Not Assigned(Data2)) then
    Result := 0
  else
    if Column = 0 then
      Result := CompareText(Data1.Data.Name, Data2.Data.Name)
    else begin
      CatData1 := vstList.GetNodeData(Data1.Data.pNode.Parent);
      CatData2 := vstList.GetNodeData(Data2.Data.pNode.Parent);
      if Assigned(CatData1) then
        CatName1 := CatData1.Data.Name
      else
        CatName1 := '';
      if Assigned(CatData2) then
        CatName2 := CatData2.Data.Name
      else
        CatName2 := '';
      Result  := CompareText(CatName1, CatName2)
    end;
end;

procedure TfrmMain.vstSearchGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : TvBaseNodeData;
begin
  NodeData   := GetNodeDataSearch(Node,vstSearch,vstList).Data;
  ImageIndex := NodeData.ImageIndex;
  if Column = 1 then
    ImageIndex := -1;
end;

procedure TfrmMain.vstSearchGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData, CatData : PBaseData;
begin
  NodeData := GetNodeDataSearch(Node,vstSearch,vstList);
  if Column = 0 then
    CellText   := NodeData.Data.Name;
  if Column = 1 then
    if (NodeData.Data.pNode.Parent <> vstList.RootNode) then
    begin
      CatData  := vstList.GetNodeData(NodeData.Data.pNode.Parent);
      CellText := CatData.Data.Name;
    end
    else
      CellText := '';
end;

procedure TfrmMain.vstSearchHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  vstSearch.SortTree(HitInfo.Column,Sender.SortDirection,True);
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending
end;

procedure TfrmMain.ShowMainForm(Sender: TObject);
begin
  //From CoolTrayicon source
  if Application.MainForm <> nil then
  begin
    // Restore the app, but don't automatically show its taskbar icon
    // Show application's TASKBAR icon (not the tray icon)
    ShowWindow(Application.Handle, SW_RESTORE);
    Application.Restore;
    // Show the form itself
    if Application.MainForm.WindowState = wsMinimized then
      Application.MainForm.WindowState := wsNormal;    // Override minimized state
    Application.MainForm.Visible := True;
    // Bring the main form (or its modal dialog) to the foreground
    SetForegroundWindow(Application.Handle);
  end;
end;

procedure TfrmMain.RunNormalSw(TreeView: TBaseVirtualTree);
var
  ProcessInfo: TProcessInfo;
begin
  ProcessInfo.RunMode := rmNormal;
  ExecuteSelectedNode(TreeView,ProcessInfo);
end;

procedure TfrmMain.HideMainForm;
begin
  if Application.MainForm <> nil then
  begin
    // Hide the form itself (and thus any child windows)
    Application.MainForm.Visible := False;
    { Hide application's TASKBAR icon (not the tray icon). Do this AFTER
        the main form is hidden, or any child windows will redisplay the
        taskbar icon if they are visible. }
    if IsWindowVisible(Application.Handle) then
      ShowWindow(Application.Handle, SW_HIDE);
  end;
end;

procedure TfrmMain.OpenFolder(TreeView: TBaseVirtualTree);
var
  ProcessInfo: TProcessInfo;
begin
  ProcessInfo.RunFromCat := False;
  ProcessInfo.RunMode := rmOpenFolder;
  ExecuteSelectedNode(TreeView, ProcessInfo);
end;

function TfrmMain.ShowItemProperty(TreeView: TBaseVirtualTree; Node: PVirtualNode): Integer;
var
  BaseNode: PBaseData;
begin
  Result := mrCancel;
  if Assigned(Node) then
    BaseNode := GetNodeDataEx(Node, TreeView, vstSearch, vstList)
  else
    BaseNode := GetNodeDataEx(TreeView.FocusedNode, TreeView, vstSearch, vstList);
  if Assigned(BaseNode) then
    begin
      if BaseNode.Data.DataType in [vtdtFile, vtdtCategory, vtdtFolder] then
      begin
        try
          Application.CreateForm(TfrmPropertyItem, frmPropertyItem);
          frmPropertyItem.FormStyle := Self.FormStyle;
          Result := frmPropertyItem.Execute(TvCustomRealNodeData((BaseNode).Data));
        finally
          frmPropertyItem.Free;
        end;
      end
      else
        Result := TfrmPropertySeparator.Edit(Self, BaseNode);
    RefreshList(vstList);
  end;
end;

procedure TfrmMain.DoSearchItem(TreeSearch: TBaseVirtualTree; Keyword: string;
                                Callback: TVTGetNodeProc);
var
  NodeData: PBaseData;
begin
  TreeSearch.Clear;
  if Length(Keyword) > 0 then
  begin
    New(NodeData);
    NodeData.Data := CreateNodeData(vtdtFile);
    with TvFileNodeData(NodeData.Data) do
    begin
      case SearchType of
        stName       : Name       := Keyword;
        stPathExe    : PathExe    := Keyword;
        stPathIcon   : PathIcon   := Keyword;
        stWorkingDir : WorkingDir := Keyword;
        stParameters : Parameters := Keyword;
      end;
    end;
    vstList.IterateSubtree(nil, CallBack, NodeData.Data, [], True);
    FreeAndNil(NodeData.Data);
    Dispose(NodeData);
  end;
end;

procedure TfrmMain.RunAsAdmin(TreeView: TBaseVirtualTree);
var
  ProcessInfo: TProcessInfo;
begin
  ProcessInfo.RunMode := rmRunAsAdmin;
  ExecuteSelectedNode(TreeView, ProcessInfo);
end;

procedure TfrmMain.RunAs(TreeView: TBaseVirtualTree);
var
  ProcessInfo: TProcessInfo;
begin
  ProcessInfo.RunMode := rmRunAs;
  //Call login dialog for Windows username and password
  TLoginForm.Login(msgRunAsTitle, msgInsertWinUserInfo, ProcessInfo.UserName, ProcessInfo.Password, true, '');
  if ProcessInfo.UserName <> '' then
    ExecuteSelectedNode(TreeView, ProcessInfo)
  else
    ShowMessage(msgErrEmptyUserName, true);
end;

procedure TfrmMain.ExecuteSelectedNode(TreeView: TBaseVirtualTree;ProcessInfo: TProcessInfo);
var
  Node         : PVirtualNode;
  BaseNodeData : TvBaseNodeData;
  BaseNode     : PBaseData;
begin
  //First selected node
  Node := TreeView.GetFirstSelected;
  while Assigned(Node) do
  begin
    //Get Node data
    BaseNode     := PBaseData(GetNodeDataEx(Node, TreeView, vstSearch, vstList));
    BaseNodeData := BaseNode.Data;
    //Execute action
    if ProcessInfo.RunMode <> rmOpenFolder then
    begin
      ProcessInfo.RunFromCat := (BaseNodeData.DataType = vtdtCategory);
      if (BaseNodeData.DataType in [vtdtFile,vtdtFolder]) then
        TvFileNodeData(BaseNodeData).Execute(vstList, ProcessInfo)
      else
        if (BaseNodeData.DataType = vtdtCategory) then
          TvCategoryNodeData(BaseNodeData).Execute(vstList, ProcessInfo);
    end
    else begin
      if (BaseNodeData.DataType in [vtdtFile,vtdtFolder]) then
        TvFileNodeData(BaseNodeData).OpenExtractedFolder;
    end;
    //Next selected node
    Node := TreeView.GetNextSelected(node);
  end;
end;

procedure TfrmMain.vstListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : PvBaseNodeData;
begin
  NodeData   := Sender.GetNodeData(Node);
  ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmMain.vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData : PBaseData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
  begin
    if (NodeData.Data.DataType = vtdtSeparator) and (NodeData.Data.Name = '') then
      CellText := ' '
    else
      CellText := StringReplace(NodeData.Data.Name, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
  end;
end;

procedure TfrmMain.vstListKeyPress(Sender: TObject; var Key: Char);
begin
  if (Sender is TBaseVirtualTree) then
    if Ord(Key) = VK_RETURN then
        RunNormalSw((Sender as TBaseVirtualTree));
end;

procedure TfrmMain.vstListLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Stream: TStream);
var
  DataDest,DataSource: PBaseData;
begin
  //Create a new PBaseData as source
  New(DataSource);
  Stream.ReadBuffer(DataSource^,SizeOf(rBaseData));
  //Copy source's properties in DataDest
  DataDest := vstList.GetNodeData(Node);
  DataDest.Data := CreateNodeData(DataSource.Data.DataType);
  //Copy DataSource in DataDest
  case DataSource.Data.DataType of
    vtdtCategory  : TvCategoryNodeData(DataDest.Data).Copy(DataSource.Data);
    vtdtFile      : TvFileNodeData(DataDest.Data).Copy(DataSource.Data);
    vtdtFolder    : TvFileNodeData(DataDest.Data).Copy(DataSource.Data);
    vtdtSeparator : TvSeparatorNodeData(DataDest.Data).Copy(DataSource.Data);
  end;
  //Set some personal record fields
  DataDest.Data.pNode := Node;
  DataDest.Data.ParentNode := Node.Parent;
  //Icon
  DataDest.Data.ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(DataDest.Data));
  FreeMem(DataSource);
end;

procedure TfrmMain.vstListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var
  NodeData : PBaseData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    NodeData.Data.Name := NewText;
end;

procedure TfrmMain.vstListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  with TargetCanvas do
  begin
    Font.Name  := Config.TVFont.Name;
    Font.Style := Config.TVFont.Style;
    Font.Size  := Config.TVFont.Size;
    if Not(Sender.Selected[Node]) then
      Font.Color := Config.TVFont.Color;
  end;
end;

procedure TfrmMain.vstListSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Stream: TStream);
var
  Data: PBaseData;
begin
  Data := vstList.GetNodeData(Node);
  Stream.WriteBuffer(Data^,SizeOf(rBaseData));
end;

procedure TfrmMain.RunSingleClick(Sender: TObject);
begin
  //Check if user click on node or expand button (+/-)
  if (Sender is TBaseVirtualTree) then
    if Not(ClickOnButtonTree((Sender as TBaseVirtualTree))) then
      if (Config.RunSingleClick) then
        RunNormalSw((Sender as TBaseVirtualTree));
end;

procedure TfrmMain.vstListCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PBaseData;
begin
  Data1 := vstList.GetNodeData(Node1);
  Data2 := vstList.GetNodeData(Node2);
  if (Not Assigned(Data1)) or (Not Assigned(Data2)) then
    Result := 0
  else
    if (Data1.Data.DataType = vtdtCategory) <> (Data2.Data.DataType = vtdtCategory) then
    begin
      if Data1.Data.DataType = vtdtCategory then
        Result := -1
      else
        Result := 1
    end
    else
      Result := CompareText(Data1.Data.Name, Data2.Data.Name);
end;

procedure TfrmMain.vstListDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  I          : integer;
  NodeData   : PBaseData;
  AttachMode : TVTNodeAttachMode;
begin
  Sender.BeginUpdate;
  case Mode of
    dmAbove  : AttachMode := amInsertBefore;
    dmOnNode : AttachMode := amAddChildLast;
    dmBelow  : AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;
  if Assigned(DataObject) then
  begin
    NodeData := Sender.GetNodeData(Sender.DropTargetNode);
    if Mode = dmOnNode then
    begin
      //Check if DropMode is in a vtdtCategory (so expand it, before drop item)
      //or another item type (change Mode and AttachMode for insert after new nodes)
      if NodeData.Data.DataType <> vtdtCategory then
      begin
        Mode := dmBelow;
        AttachMode := amInsertAfter;
      end
      else
        vstList.Expanded[Sender.DropTargetNode] := True;
    end;
    try
      for I := 0 to High(Formats) - 1 do
      begin
        //Files
        if Formats[I] = CF_HDROP then
          DragDropFiles(Sender,DataObject, AttachMode, Mode)
        else //VirtualTree Nodes
          if Formats[I] = CF_VIRTUALTREE then
            Sender.ProcessDrop(DataObject, Sender.DropTargetNode, Effect, AttachMode)
          else //Text
            if Formats[I] = CF_TEXT then
              DragDropText(Sender,DataObject, AttachMode, Mode);
      end;
    except
      on E : Exception do
        ShowMessageFmt(msgErrGeneric,[E.ClassName,E.Message], true);
    end;
    vstList.Repaint;
    RefreshList(vstList);
  end;
  Sender.EndUpdate;
end;

procedure TfrmMain.vstListDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
begin
  accept := true;
end;

procedure TfrmMain.vstListDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := PBaseData(Sender.GetNodeData(Node)).Data;
  if NodeData.DataType = vtdtSeparator then
  begin
    ClassicMenu.DoDrawCaptionedSeparator(Sender,TargetCanvas,CellRect,NodeData.Name);
    DefaultDraw := False;
  end;
end;

procedure TfrmMain.vstListEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData : PBaseData;
begin
  NodeData := Sender.GetNodeData(Node);
  Allowed  := (NodeData.Data.DataType <> vtdtSeparator) and Not(Config.RunSingleClick);
end;

procedure TfrmMain.vstListExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := PBaseData(Sender.GetNodeData(Node)).Data;
  if NodeData.DataType = vtdtCategory then
    ImagesDM.GetChildNodesIcons(Sender, nil, Node);
end;

procedure TfrmMain.vstListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : PBaseData;
begin
  NodeData := Sender.GetNodeData(Node);
  FreeAndNil(NodeData.Data);
end;

function TfrmMain.GetActiveTree: TBaseVirtualTree;
begin
  case pcList.ActivePageIndex of
    PG_LIST   : Result := vstList;
    PG_SEARCH : Result := vstSearch;
    else        Result := nil;
  end;
end;

procedure TfrmMain.LoadGlyphs;
begin
  //Set IcoImages
  //Set submenuimages to three MainMenu's subitems
  miFile.SubMenuImages := ImagesDM.IcoImages;
  miEdit.SubMenuImages := ImagesDM.IcoImages;
  miHelp.SubMenuImages := ImagesDM.IcoImages;
  vstList.Images       := ImagesDM.IcoImages;
  vstSearch.Images     := ImagesDM.IcoImages;
  pmSearch.Images      := ImagesDM.IcoImages;
  pmWindow.Images      := ImagesDM.IcoImages;
  //Set MainMenu's ImageIndexes
  miSaveList1.ImageIndex   := IMAGE_INDEX_Save;
  miOptions1.ImageIndex    := IMAGE_INDEX_Options;
  miAddCat1.ImageIndex     := IMAGE_INDEX_AddCat;
  miAddSw1.ImageIndex      := IMAGE_INDEX_AddFile;
  miAddFolder1.ImageIndex  := IMAGE_INDEX_AddFolder;
  miCut1.ImageIndex        := IMAGE_INDEX_Cut;
  miCopy1.ImageIndex       := IMAGE_INDEX_Copy;
  miPaste1.ImageIndex      := IMAGE_INDEX_Paste;
  miDelete1.ImageIndex     := IMAGE_INDEX_Delete;
  miProperty1.ImageIndex   := IMAGE_INDEX_Property;
  miInfoASuite.ImageIndex  := IMAGE_INDEX_Help;
  //TODO ...image for stats
//  miStatistics.ImageIndex  := IMAGE_INDEX_sHelp;


  //Set PopUpMenu's ImageIndexes
  miRunSelectedSw.ImageIndex := IMAGE_INDEX_Run;
  miAddCat2.ImageIndex     := IMAGE_INDEX_AddCat;
  miAddSw2.ImageIndex      := IMAGE_INDEX_AddFile;
  miAddFolder2.ImageIndex  := IMAGE_INDEX_AddFolder;
  miCut2.ImageIndex        := IMAGE_INDEX_Cut;
  miCopy2.ImageIndex       := IMAGE_INDEX_Copy;
  miPaste2.ImageIndex      := IMAGE_INDEX_Paste;
  miDelete2.ImageIndex     := IMAGE_INDEX_Delete;
  miProperty2.ImageIndex   := IMAGE_INDEX_Property;
  //Set Search's ImageIndexes
  ImagesDM.IcoImages.GetBitmap(IMAGE_INDEX_Search,sbtnSearch.Glyph);
end;

procedure TfrmMain.RunStartupProcess;
var
  NodeData    : TvCustomRealNodeData;
  ProcessInfo : TProcessInfo;
  I : Integer;
begin
  //Autorun - Execute software
  if (Config.Autorun) then
  begin
    for I := 0 to StartupItemList.Count - 1 do
    begin
      NodeData := StartupItemList[I];
      ProcessInfo.RunFromCat := (NodeData.DataType = vtdtCategory);
      //Set RunMode
      if (NodeData.Autorun = atSingleInstance) then
        ProcessInfo.RunMode := rmAutorunSingleInstance
      else
        if (NodeData.Autorun = atAlwaysOnStart) then
          ProcessInfo.RunMode := rmAutorun;
      //Start process
      if NodeData.DataType in [vtdtFile,vtdtFolder] then
        TvFileNodeData(NodeData).Execute(vstList, ProcessInfo)
      else
        if NodeData.DataType = vtdtCategory then
          TvCategoryNodeData(NodeData).Execute(vstList, ProcessInfo);
    end;
  end;
end;

procedure TfrmMain.RunShutdownProcess;
var
  NodeData    : TvCustomRealNodeData;
  ProcessInfo : TProcessInfo;
  I : Integer;
begin
  //Autorun - Execute software
  if (Config.Autorun) then
  begin
    for I := 0 to ShutdownItemList.Count - 1 do
    begin
      NodeData := ShutdownItemList[I];
      ProcessInfo.RunFromCat := (NodeData.DataType = vtdtCategory);
      ProcessInfo.RunMode := rmAutorun;
      //Start process
      if NodeData.DataType in [vtdtFile,vtdtFolder] then
        TvFileNodeData(NodeData).Execute(vstList, ProcessInfo)
      else
        if NodeData.DataType = vtdtCategory then
          TvCategoryNodeData(NodeData).Execute(vstList, ProcessInfo);
    end;
  end;
end;

procedure TfrmMain.miPropertyClick(Sender: TObject);
begin
  ShowItemProperty(GetActiveTree);
end;

procedure TfrmMain.tmSchedulerTimer(Sender: TObject);
var
  NodeData    : TvCustomRealNodeData;
  NowDateTime : TDateTime;
  ProcessInfo : TProcessInfo;
  I           : Integer;
  schTime     : TDateTime;
begin
  if (Config.ASuiteState = asStartUp) or (Config.ASuiteState = asShutdown) then
    Exit;
  NowDateTime := RecodeMilliSecond(Now,0);
  schTime     := NowDateTime;
  //Check scheduler list to know which items to run
  for I := 0 to SchedulerItemList.Count - 1 do
  begin
    if Assigned(SchedulerItemList[I]) then
    begin
      NodeData := SchedulerItemList[I];
      //Compare time and/or date based of scheduler mode
      case NodeData.SchMode of
        smDisabled: schTime := 0;
        smOnce:     schTime := NodeData.SchDateTime;
        smHourly:
        begin
          //Run software every hour
          schTime := RecodeMinute(NowDateTime,0);
          schTime := RecodeSecond(schTime,0);
        end;
        smDaily:
        begin
          //Run software every day (user choose time, hour and minute)
          schTime := RecodeYear(NodeData.SchDateTime, YearOf(NowDateTime));
          schTime := RecodeMonth(schTime, MonthOf(NowDateTime));
          schTime := RecodeDay(schTime, DayOf(NowDateTime));
        end;
      end;
      //If is its turn, run item
      if (CompareDateTime(NowDateTime, schTime) = 0) and (NodeData.SchMode <> smDisabled) then
      begin
        ProcessInfo.RunFromCat := (NodeData.DataType = vtdtCategory);
        ProcessInfo.RunMode := rmNormal;
        //Start process
        if NodeData.DataType in [vtdtFile,vtdtFolder] then
          TvFileNodeData(NodeData).Execute(vstList, ProcessInfo)
        else
          if NodeData.DataType = vtdtCategory then
            TvCategoryNodeData(NodeData).Execute(vstList, ProcessInfo);
      end;
    end;
  end;
end;

procedure TfrmMain.miEditClick(Sender: TObject);
begin
  miPaste1.Enabled := IsFormatInClipBoard(CF_VIRTUALTREE);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Config.ASuiteState := asShutdown;
  Close;
end;

procedure TfrmMain.miExportListClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) then
  begin
    RefreshList(vstList);
    CopyFile(PChar(DBManager.DBFileName),PChar(SaveDialog1.FileName),false)
  end;
  SetCurrentDir(SUITE_WORKING_PATH);
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if IsFormOpen('frmGraphicMenu') then
    frmGraphicMenu.CloseMenu;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RunShutdownProcess;
  //Execute actions on asuite's shutdown (inside vstList)
  vstList.IterateSubtree(nil, IterateSubtreeProcs.ActionsOnShutdown, nil, [], True);
  RefreshList(vstList);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Not(Config.TrayIcon) then
    CanClose := True
  else
    CanClose := ((Config.ASuiteState = asShutdown) or (SessionEnding));
  //If user close window (not ASuite), hide form and taskbar icon
  if not (CanClose) then
    HideMainForm;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.CreateForm(TImagesDM, ImagesDM);
  Application.CreateForm(TClassicMenu, ClassicMenu);
  pcList.ActivePageIndex := PG_LIST;
  //Create TNodeLists for autorun
  StartupItemList   := TAutorunItemList.Create;
  ShutdownItemList  := TAutorunItemList.Create;
  SchedulerItemList := TNodeDataList.Create;
  HotKeyApp := TNodeDataList.Create;
  //Set NodeDataSize for trees
  vstList.NodeDataSize   := SizeOf(rBaseData);
  vstSearch.NodeDataSize := SizeOf(TTreeDataX);
  //Read Only Mode
  with Config do
  begin
    ReadOnlyMode := GetDriveType(PChar(SUITE_DRIVE)) = DRIVE_CDROM;
    if (ReadOnlyMode) then
    begin
      Cache  := False;
      Backup := False;
      MRU    := False;
      miOptions1.Enabled  := False;
      miSaveList1.Enabled := False;
    end;
  end;
  //List & Options
  DBManager.LoadData(vstList);
  RunStartupProcess;
  //Get placeholder for edtSearch
  edtSearch.TextHint := StringReplace(miSearchName.Caption, '&', '', []);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  TfrmNotification.Stop;
  //Fix memory leaks
  FreeAndNil(MRUList);
  FreeAndNil(MFUList);
  FreeAndNil(StartupItemList);
  FreeAndNil(ShutdownItemList);
  FreeAndNil(SchedulerItemList);
  FreeAndNil(HotKeyApp);
  Config.Destroy;
end;

end.
