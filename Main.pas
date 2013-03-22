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

unit Main;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ComCtrls, VirtualTrees, ActiveX, AppConfig, ulNodeDataTypes, ulCommonClasses,
  UDImages, ASuiteForm, LCLIntf, FileUtil, win32int, InterfaceBase,
  StdCtrls, EditBtn, Buttons, ASuiteControls;

type

  { TfrmMain }

  TfrmMain = class(TASuiteForm)
    edtSearch: TASuiteEdit;
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
    procedure FormChangeBounds(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miStatisticsClick(Sender: TObject);
    procedure OpenFolderSw(Sender: TObject);
    procedure pcListChange(Sender: TObject);
    procedure miImportListClick(Sender: TObject);
    procedure miSaveListClick(Sender: TObject);
    procedure vstListDragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; const Pt: TPoint; var Effect: LongWord;
      Mode: TDropMode);
    procedure vstListDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure ShowProperty(Sender: TObject);
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
    procedure RunExe(Sender: TObject);
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
    procedure vstSearchHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    procedure ExecuteFileOrOpenFolder(TreeView: TBaseVirtualTree;ExecuteFile: Boolean);
    function  GetActiveTree: TBaseVirtualTree;
    procedure LoadGlyphs;
    procedure RunAutorun;
  public
    { Public declarations }
  end;

var
  frmMain : TfrmMain;

implementation

uses
  Option, PropertyFile, PropertyCat, About, ulCommonUtils, ulEnumerations,
  udClassicMenu, PropertySeparator, ulExeUtils, ImportList, Stats, ulAppConfig,
  ulTreeView, ulSQLite, ulDatabase;

{$R *.lfm}

procedure TfrmMain.btnedtSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    btnedtSearchRightButtonClick(Sender);
end;

procedure TfrmMain.btnedtSearchRightButtonClick(Sender: TObject);
var
  NodeData: PBaseData;
begin
  vstSearch.Clear;
  if Length(edtSearch.Text) > 0 then
  begin
    New(NodeData);
    NodeData.Data := CreateNodeData(vtdtFile);
    with TvFileNodeData(NodeData.Data) do
    begin
      case SearchType of
        stName       : Name       := edtSearch.Text;
        stPathExe    : PathExe    := edtSearch.Text;
        stPathIcon   : PathIcon   := edtSearch.Text;
        stWorkingDir : WorkingDir := edtSearch.Text;
        stParameters : Parameters := edtSearch.Text;
      end;
    end;
    vstList.IterateSubtree(nil, IterateSubtreeProcs.FindNode, NodeData.Data, [], True);
    FreeAndNil(NodeData.Data);
    Dispose(NodeData);
  end;
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
    for I := High(Nodes) downto 0 do
      vstList.IterateSubtree(Nodes[I],IterateSubtreeProcs.BeforeDeleteNode,nil,[],False);
    //Delete nodes and refresh list
    vstList.DeleteSelectedNodes;
    RefreshList(vstList);
  end;
end;

procedure TfrmMain.miSaveListClick(Sender: TObject);
var
  Result : Boolean;
begin
  Result := SaveASuiteSQLite(vstList, true);
  if Result then
    showmessage(msgSaveCompleted)
  else
    showmessage(msgErrSave);
end;

procedure TfrmMain.ChangeSearchTextHint(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    //Set new placeholder and SearchType
    edtSearch.PlaceHolder := StringReplace((Sender as TMenuItem).Caption, '&', '', []);
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

procedure TfrmMain.OpenFolderSw(Sender: TObject);
var
  TreeView : TBaseVirtualTree;
begin
  TreeView := GetActiveTree;
  //Show file's folder
  ExecuteFileOrOpenFolder(TreeView, false);
end;

procedure TfrmMain.RunExe(Sender: TObject);
var
  NodeData : TvBaseNodeData;
  Node     : PVirtualNode;
begin
  if (Sender is TBaseVirtualTree) then
  begin
    //Check if user click on node or expand button (+/-)
    if Not(ClickOnButtonTree((Sender as TBaseVirtualTree))) then
      ExecuteFileOrOpenFolder((Sender as TBaseVirtualTree), true);
  end;
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

procedure TfrmMain.miRunSelectedSwClick(Sender: TObject);
begin
  if pcList.ActivePageIndex = PG_LIST then
    RunExe(vstList)
  else
    RunExe(vstSearch);
end;

procedure TfrmMain.miOptionsClick(Sender: TObject);
begin
  if not IsFormOpen('frmOption') then
    try
      Application.CreateForm(TfrmOption, frmOption);
      frmOption.FormStyle := Self.FormStyle;
      frmOption.showmodal;
    finally
      frmOption.Free;
    end
  else
    frmOption.show;
  RefreshList(vstList);
end;

procedure TfrmMain.miStatisticsClick(Sender: TObject);
var
   frmStats:TfrmStats;
begin
   try
     frmStats:=TfrmStats.Create(self);
     frmStats.ShowModal();
   finally
     frmStats.Free;
   end;
end;

procedure TfrmMain.FormChangeBounds(Sender: TObject);
begin
  if not(StartUpTime) then
    Config.Changed := true;
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    //Cancel minimize frmMain
    WindowState := wsNormal;
    //Hide frmMain and taskbar icon
    Hide;
    ShowInTaskBar := stNever;
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
  //Separator
  N9.Visible              := ActiveTab;
  N5.Visible              := ActiveTab;
  N10.Visible             := ActiveTab;
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
    miProperty2.Enabled     := true;
    if (NodeData.Data.DataType <> vtdtCategory) then
      miRunSelectedSw.Enabled := true;
    if (NodeData.Data.DataType <> vtdtFile) then
      miOpenFolderSw.Enabled := false;
    if (NodeData.Data.DataType <> vtdtSeparator) then
      miRunSelectedSw.Enabled := true;
    case NodeData.Data.DataType of
      vtdtCategory  : miRunSelectedSw.Enabled := false;
      vtdtFile      : miOpenFolderSw.Enabled  := true;
      vtdtSeparator : miRunSelectedSw.Enabled := false;
    end;
  end
  else begin
    miRunSelectedSw.Enabled := false;
    miOpenFolderSw.Enabled  := false;
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
      CatData1 := vstList.GetNodeData(Data1.pNode.Parent);
      CatData2 := vstList.GetNodeData(Data2.pNode.Parent);
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
    if (NodeData.pNode.Parent <> vstList.RootNode) then
    begin
      CatData  := vstList.GetNodeData(NodeData.pNode.Parent);
      CellText := CatData.Data.Name;
    end
    else
      CellText := '';
end;

procedure TfrmMain.vstSearchHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  );
begin
  vstSearch.SortTree(Column,Sender.SortDirection,True);
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending
end;

procedure TfrmMain.ExecuteFileOrOpenFolder(TreeView: TBaseVirtualTree;ExecuteFile: Boolean);
var
  Node     : PVirtualNode;
  NodeData : TvBaseNodeData;
begin
  //First selected node
  Node := TreeView.GetFirstSelected;
  while Assigned(Node) do
  begin
    //Get Node data
    NodeData := PBaseData(GetNodeDataEx(Node, TreeView, vstSearch, vstList)).Data;
    //Check if node type is a file
    if (NodeData is TvFileNodeData) then
    begin
      //If ExecuteFile = true, execute it
      if ExecuteFile then
        TvFileNodeData(NodeData).Execute(vstList, false)
      else //Else open file's folder
        TvFileNodeData(NodeData).OpenExtractedFolder;
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
  if Column = 1 then
    ImageIndex := -1;
end;

procedure TfrmMain.vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData : PBaseData;
  I        : Byte;
  str      : string;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData.Data.DataType = vtdtSeparator then
  begin
    I := 40 - (Length(NodeData.Data.Name));
    str := '----------------------';
    if I >= 10 then
      SetLength(str, I div 2)
    else
      SetLength(str, 5);
    CellText := str + ' ' + NodeData.Data.Name + ' ' + str;
  end
  else
    CellText := StringReplace(NodeData.Data.Name, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
end;

procedure TfrmMain.vstListKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    RunExe(Sender);
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
  DataDest.pNode := Node;
  DataDest.Data.ParentNode := Node.Parent;
  //Icon
  DataDest.Data.ImageIndex := ImagesDM.GetIconIndex(DataDest.Data);
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
  if (Sender is TBaseVirtualTree) then
    if Not(ClickOnButtonTree((Sender as TBaseVirtualTree))) then
      if (Config.RunSingleClick) then
        RunExe(Sender);
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

procedure TfrmMain.vstListDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  I          : integer;
  NodeData   : PBaseData;
  AttachMode : TVTNodeAttachMode;
begin
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
      //or another item type (set amNowhere as AttachMode)
      if NodeData.Data.DataType <> vtdtCategory then
        AttachMode := amNowhere
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
        ShowMessageFmt(msgErrGeneric,[E.ClassName,E.Message]);
    end;
    RefreshList(vstList);
  end;
end;

procedure TfrmMain.vstListDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
begin
  accept := true;
end;

procedure TfrmMain.vstListExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
  GetChildNodesIcons(Sender, Node);
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
  miSaveList1.ImageIndex   := IMG_Save;
  miOptions1.ImageIndex    := IMG_Options;
  miAddCat1.ImageIndex     := IMG_AddCat;
  miAddSw1.ImageIndex      := IMG_AddFile;
  miAddFolder1.ImageIndex  := IMG_AddFolder;
  miCut1.ImageIndex        := IMG_Cut;
  miCopy1.ImageIndex       := IMG_Copy;
  miPaste1.ImageIndex      := IMG_Paste;
  miDelete1.ImageIndex     := IMG_Delete;
  miProperty1.ImageIndex   := IMG_Property;
  miInfoASuite.ImageIndex  := IMG_Help;
  //Set PopUpMenu's ImageIndexes
  miRunSelectedSw.ImageIndex := IMG_Run;
  miAddCat2.ImageIndex     := IMG_AddCat;
  miAddSw2.ImageIndex      := IMG_AddFile;
  miAddFolder2.ImageIndex  := IMG_AddFolder;
  miCut2.ImageIndex        := IMG_Cut;
  miCopy2.ImageIndex       := IMG_Copy;
  miPaste2.ImageIndex      := IMG_Paste;
  miDelete2.ImageIndex     := IMG_Delete;
  miProperty2.ImageIndex   := IMG_Property;
  //Set Search's ImageIndexes
  ImagesDM.IcoImages.GetBitmap(IMG_Search,sbtnSearch.Glyph);
end;

procedure TfrmMain.RunAutorun;
var
  NodeData : TvFileNodeData;
  I        : Integer;
begin
  //Autorun - Execute software
  for I := 0 to ASuiteStartUpApp.Count - 1 do
  begin
    NodeData := ASuiteStartUpApp[I];
    if (Config.Autorun) then
    begin
      if (NodeData.Autorun = atSingleInstance) then
      begin
        if not IsProcessExists(ExtractFileName(NodeData.PathAbsoluteExe)) then
          NodeData.Execute(vstList,true);
      end
      else if (NodeData.Autorun = atAlwaysOnStart) then
        NodeData.Execute(vstList,true);
    end;
  end;
end;

procedure TfrmMain.ShowProperty(Sender: TObject);
var
  Node     : PVirtualNode;
  NodeData : PBaseData;
  OK       : Boolean;
  TreeView : TBaseVirtualTree;
begin
  TreeView := GetActiveTree;
  NodeData := GetNodeDataEx(TreeView.FocusedNode, TreeView, vstSearch, vstList);
  if Assigned(NodeData) then
  begin
    case NodeData.Data.DataType of
      vtdtFile      : OK := (TfrmPropertyFile.Edit(Self, NodeData) = mrOK);
      vtdtCategory  : OK := (TfrmPropertyCat.Edit(Self, NodeData) = mrOK);
      vtdtSeparator : OK := (TfrmPropertySeparator.Edit(Self, NodeData) = mrOK);
      else            OK := False;
    end;
    if Ok then
      RefreshList(vstList);
  end;
end;

procedure TfrmMain.miEditClick(Sender: TObject);
begin
  miPaste1.Enabled := IsFormatInClipBoard(CF_VIRTUALTREE);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  ShutdownTime := True;
  Close;
end;

procedure TfrmMain.miExportListClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) then
  begin
    SaveASuiteSQLite(vstList,true);
    Windows.CopyFile(PChar(DBManager.DBFileName),PChar(SaveDialog1.FileName),false)
  end;
  SetCurrentDirUTF8(SUITE_WORKING_PATH); 
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I        : Integer;
  NodeData : TvFileNodeData;
begin
  //Autorun - Execute software
  for I := 0 to ASuiteShutdownApp.Count - 1 do
  begin
    NodeData := TvFileNodeData(ASuiteShutdownApp[I]);
    if (Config.Autorun) then
      NodeData.Execute(vstList,true);
  end;
  //Execute actions on asuite's shutdown (inside vstList)
  vstList.IterateSubtree(nil, IterateSubtreeProcs.ActionsOnShutdown, nil, [], True);
  SaveASuiteSQLite(vstList,true);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Not(Config.TrayIcon) then
    CanClose := True
  else
    CanClose := (ShutdownTime or SessionEnding);
  //If user close window (not ASuite), hide form and taskbar icon
  if not (CanClose) then
  begin
    //Hide frmMain and taskbar icon
    Hide;
    ShowInTaskBar := stNever;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pcList.ActivePageIndex := PG_LIST;
  //Create TNodeLists for autorun
  ASuiteStartUpApp  := TAutorunItemList.Create;
  ASuiteShutdownApp := TAutorunItemList.Create;
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
  //Loading icons
  LoadGlyphs;
  //List & Options
  LoadASuiteSQLite(vstList, false);
  RunAutorun;
  StartUpTime := false;
  //Get placeholder for edtSearch
  edtSearch.PlaceHolder := StringReplace(miSearchName.Caption, '&', '', []);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  //Fix memory leaks
  FreeAndNil(MRUList);
  FreeAndNil(MFUList);
  FreeAndNil(ASuiteStartUpApp);
  FreeAndNil(ASuiteShutdownApp);
  Config.Destroy;
end;

end.
