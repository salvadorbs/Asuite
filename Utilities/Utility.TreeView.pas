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

unit Utility.TreeView;

interface

uses
  Windows, SysUtils, ActiveX, VirtualTrees, Controls, Kernel.Consts, Graphics,
  NodeDataTypes.Base, Kernel.Enumerations, Classes, ShellApi, comobj, DKLang,
  Lists.Manager, Lists.Base, Kernel.Types, NodeDataTypes.Custom;

{ List, Menu, MRU }
function  AddNodeInVST(Sender: TBaseVirtualTree;ParentNode: PVirtualNode;AType: TvTreeDataType): PVirtualNode;
function  CreateListItem(Sender: TBaseVirtualTree;AType: TvTreeDataType): TvBaseNodeData;
function  CreateNodeData(AType: TvTreeDataType): TvBaseNodeData;
function  ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;
function GetNodeParentName(const ASender: TBaseVirtualTree; const ANode: PVirtualNode): string;
procedure RefreshList(const ATree: TBaseVirtualTree);
function ShowItemProperty(const AOwner: TComponent; const ATreeView: TBaseVirtualTree;
                          const ANode: PVirtualNode; ANewNode: Boolean): Integer;

{ Drag&Drop }
procedure DragDropFiles(Sender: TBaseVirtualTree; DataObject: IDataObject;
                        AttachMode: TVTNodeAttachMode);
procedure GetDropFileProperty(Sender: TBaseVirtualTree; Node: pVirtualNode;
                       PathTemp: string);
function GetTextFromDataObject(DataObject: IDataObject): string;
function DragDropText(Sender: TBaseVirtualTree;DataObject: IDataObject;
                       AttachMode: TVTNodeAttachMode; Mode: TDropMode): Boolean;

{ Get Data from Virtual TreeView }
procedure GetFileListFromObj(const DataObj: IDataObject; FileList: TStringList);
function  GetNodeDataEx(const ANode: PVirtualNode; const ATree: TBaseVirtualTree): PBaseData;
function GetNodeItemData(const ANode: PVirtualNode; const ATree: TBaseVirtualTree): TvBaseNodeData;
function  GetListNodeFromSubTree(const ANodeX: PVirtualNode;const ATree: TBaseVirtualTree): PVirtualNode;

{ Populate methods }
procedure PopulateListTree(Tree: TVirtualStringTree);
procedure PopulateSpecialTree(Tree: TVirtualStringTree;SList: TBaseItemsList;MaxItems: Integer);
procedure PopulateVSTItemList(const ATree: TBaseVirtualTree;const ABaseItemsList: TBaseItemsList);

{ Visual }
procedure ChangeAllNodeHeight(const ASender: TBaseVirtualTree; const ANewNodeHeight: Integer);
procedure ChangeTreeIconSize(const ASender: TVirtualStringTree; const ASmallIcon: Boolean);
procedure CheckVisibleNodePathExe(const ASender: TBaseVirtualTree);
procedure DrawSeparatorItem(const ASender: TBaseVirtualTree; const ANode: PVirtualNode;
                            TargetCanvas: TCanvas; const CellRect: TRect; var DefaultDraw: Boolean);

type

  { TIterateSubtreeProcs }

  TIterateSubtreeProcs = class //Class for IterateSubtree
    class procedure BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);
    class procedure ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                Data: Pointer; var Abort: Boolean);
    class procedure AddNodeInTreeFromMainTree(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                        Data: Pointer; var Abort: Boolean);
    class procedure FindNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    class procedure IncNumberNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
    class procedure UpdateListItemCount(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                  Data: Pointer; var Abort: Boolean);
    class procedure UpdateNodeHeight(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);
  end;

implementation

uses
  Utility.System, DataModules.Images, Forms.Main, AppConfig.Main, NodeDataTypes.Files,
  Utility.FileFolder, Forms.PropertySeparator, DataModules.TrayMenu,
  NodeDataTypes.Category, NodeDataTypes.Separator,
  Forms.PropertyItem;

function AddNodeInVST(Sender: TBaseVirtualTree;ParentNode: PVirtualNode;AType: TvTreeDataType): PVirtualNode;
var
  NodeData: TvBaseNodeData;
begin
  Result := nil;
  if Assigned(ParentNode) then
  begin
    NodeData := GetNodeItemData(ParentNode, Sender);
    case NodeData.DataType of
      //if category then expand and add subnode
      vtdtCategory :
      begin
        Result := Sender.AddChild(ParentNode, CreateNodeData(AType));
        Sender.Expanded[ParentNode] := True;
      end;
      //if separator, file or folder then always insert after
      vtdtSeparator,
      vtdtFile,
      vtdtFolder   : Result := Sender.InsertNode(ParentNode, amInsertAfter, CreateNodeData(AType));
    end;
  end
  else
    Result := Sender.AddChild(Sender.RootNode, CreateNodeData(AType));
end;

function CreateListItem(Sender: TBaseVirtualTree;AType: TvTreeDataType): TvBaseNodeData;
var
  CurrNode, ChildNode  : PVirtualNode;
  NodeData             : TvBaseNodeData;
  FolderPath, tempName : String;
begin
  FolderPath := '';
  Result     := nil;
  NodeData   := nil;
  try
    CurrNode   := Sender.FocusedNode;
    ChildNode  := AddNodeInVST(Sender, CurrNode, AType);
    //Set ChildNode's pNode and name (temporary)
    NodeData       := GetNodeItemData(ChildNode, Sender);
    NodeData.pNode := ChildNode;
    NodeData.Name  := DKLangConstW('msgNoName') + IntToStr(Sender.TotalCount);
    //ShowPropertyItem
    //Separator
    if AType = vtdtSeparator then
    begin
      if (TfrmPropertySeparator.Execute(Sender, NodeData) <> mrOK) then
        Sender.DeleteNode(ChildNode);
    end //Other types
    else begin
      if AType = vtdtFolder then
      begin
        FolderPath := BrowseForFolder(Config.Paths.SuitePathWorking);
        if FolderPath <> '' then
        begin
          tempName := ExtractDirectoryName(FolderPath + PathDelim);
          if tempName <> '' then
            NodeData.Name := tempName;
          TvFileNodeData(NodeData).PathExe := Config.Paths.AbsoluteToRelative(FolderPath + PathDelim);
        end
        else begin
          Sender.DeleteNode(ChildNode);
          Exit;
        end;
      end;
      if (ShowItemProperty(nil, Sender, ChildNode, True) <> mrOK) then
        Sender.DeleteNode(ChildNode);
    end;
  finally
    if Assigned(NodeData) then
      Result := NodeData;
    RefreshList(Sender);
  end;
end;

function CreateNodeData(AType: TvTreeDataType): TvBaseNodeData;
begin
  case AType of
    vtdtCategory  : Result := TvCategoryNodeData.Create;
    vtdtFile      : Result := TvFileNodeData.Create(vtdtFile);
    vtdtFolder    : Result := TvFileNodeData.Create(vtdtFolder);
    vtdtSeparator : Result := TvSeparatorNodeData.Create;
  else
    Result := nil;
  end;
end;

function ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;
var
  Point   : TPoint;
  HitInfo : ThitInfo;
begin
  Result   := false;
  GetCursorPos(Point);
  Point    := Sender.ScreenToClient(Point);
  Sender.GetHitTestInfoAt(Point.X,Point.Y,true,HitInfo);
  if hiOnItemButton in hitinfo.HitPositions then
    Result := True;
end;

procedure ChangeAllNodeHeight(const ASender: TBaseVirtualTree; const ANewNodeHeight: Integer);
begin
  ASender.IterateSubtree(nil, TIterateSubTreeProcs.UpdateNodeHeight, @ANewNodeHeight);
end;

procedure ChangeTreeIconSize(const ASender: TVirtualStringTree; const ASmallIcon: Boolean);
begin
  //Change default node height and imagelist based of IconSize
  if ASmallIcon then
  begin
    ASender.DefaultNodeHeight := 18;
    ASender.Images := dmImages.IcoImages;
  end
  else begin
    ASender.DefaultNodeHeight := 36;
    ASender.Images := dmImages.LargeIcoImages;
  end;
  ASender.ScrollBarOptions.VerticalIncrement := ASender.DefaultNodeHeight;
end;

procedure CheckVisibleNodePathExe(const ASender: TBaseVirtualTree);
var
  Node: PVirtualNode;
  NodeData: TvBaseNodeData;
begin
  Node := ASender.GetFirstVisible;
  while Assigned(Node) do
  begin
    //Get data and check if AbsoluteExe path exists
    NodeData := GetNodeItemData(Node, ASender);
//    if Assigned(NodeData) then
//      if NodeData.DataType = vtdtFile then
//        TvFileNodeData(NodeData).CheckPathExe;
    //Next visible node
    Node := ASender.GetNextVisible(Node);
  end;
end;

procedure DrawSeparatorItem(const ASender: TBaseVirtualTree; const ANode: PVirtualNode;
                            TargetCanvas: TCanvas; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := GetNodeDataEx(ANode, ASender).Data;
  if Assigned(NodeData) then
  begin
    if NodeData.DataType = vtdtSeparator then
    begin
      //Resize CellRect.Width, if necessary
      if ASender.ClientWidth < CellRect.Width then
        CellRect.Width := ASender.ClientWidth - 12;
      //Draw captioned separator and disable Tree's Draw
      dmTrayMenu.DoDrawCaptionedSeparator(ASender,TargetCanvas,CellRect,NodeData.Name);
      DefaultDraw := False;
    end;
  end;
end;

function GetNodeParentName(const ASender: TBaseVirtualTree; const ANode: PVirtualNode): string;
var
  CatData: TvBaseNodeData;
begin
  Result := '';
  if (ANode.Parent <> Config.MainTree.RootNode) then
  begin
    CatData := GetNodeItemData(ANode.Parent, Config.MainTree);
    if Assigned(CatData) then
      Result  := CatData.Name;
  end
  else
    Result := '<Root>';
end;

procedure PopulateListTree(Tree: TVirtualStringTree);
begin
  Tree.Clear;
  Tree.BeginUpdate;
  try
    //Populate and get icons from first level
    Config.MainTree.IterateSubtree(nil, TIterateSubtreeProcs.AddNodeInTreeFromMainTree, @Tree);
  finally
    Tree.EndUpdate;
    //Check nodes path
    CheckVisibleNodePathExe(Tree);
  end;
end;

procedure PopulateSpecialTree(Tree: TVirtualStringTree;
  SList: TBaseItemsList; MaxItems: Integer);
var
  NewNodeData  : PTreeDataX;
  NewNode      : PVirtualNode;
  I, ItemCount : Integer;
begin
  Tree.Clear;
  Tree.BeginUpdate;
  try
    //Change node height and imagelist
    ChangeTreeIconSize(Tree, False);
    //Set limit based on MaxItems or SList.Count
    if MaxItems < SList.Count then
      ItemCount := MaxItems
    else
      ItemCount := SList.Count;
    for I := 0 to ItemCount - 1 do
    begin
      if Assigned(SList[I]) then
      begin
        //Create MenuItem
        if Assigned(SList[I]) then
        begin
          NewNode     := Tree.AddChild(nil);
          NewNodeData := Tree.GetNodeData(NewNode);
          //References
          NewNodeData.pNodeList := TvCustomRealNodeData(SList[I]).pNode;
        end
        else
          SList.Delete(I);
      end;
    end;
  finally
    Tree.EndUpdate;
    Tree.ValidateNode(Tree.RootNode, True);
    //Check nodes path
    //TODO: Fix it
//ImagesDM.GetChildNodesIcons(Tree, Tree.RootNode, isAny);
    CheckVisibleNodePathExe(Tree);
  end;
end;

procedure PopulateVSTItemList(const ATree: TBaseVirtualTree;const ABaseItemsList: TBaseItemsList);
var
  I: Integer;
  CurrentFileData : TvCustomRealNodeData;
  NewNode         : PVirtualNode;
  NewNodeData     : PTreeDataX;
begin
  ATree.BeginUpdate;
  try
    for I := 0 to ABaseItemsList.Count - 1 do
    begin
      CurrentFileData := ABaseItemsList[I];
      if Assigned(CurrentFileData) then
      begin
        NewNode := ATree.AddChild(ATree.RootNode);
        NewNodeData := ATree.GetNodeData(NewNode);
        //Set pointers
        NewNodeData.pNodeList := CurrentFileData.PNode;
      end;
    end;
    //Check nodes path and get icons
    CheckVisibleNodePathExe(ATree);
    //TODO: Fix it
//ImagesDM.GetChildNodesIcons(ATree, ATree.RootNode, isAny);
    //Auto columns width
    TVirtualStringTree(ATree).Header.AutoFitColumns;
  finally
    ATree.EndUpdate;
  end;
end;

procedure RefreshList(const ATree: TBaseVirtualTree);
begin
  Config.DBManager.SaveData(Config.MainTree, Config.ASuiteState = lsStartUp);
  //Check paths of only visible nodes
  if Assigned(ATree) then
    CheckVisibleNodePathExe(ATree);
end;

function ShowItemProperty(const AOwner: TComponent; const ATreeView: TBaseVirtualTree;
                          const ANode: PVirtualNode; ANewNode: Boolean): Integer;
var
  BaseNode: TvBaseNodeData;
begin
  Result := mrCancel;
  if Assigned(ANode) then
  begin
    BaseNode := GetNodeItemData(ANode, ATreeView);
    if Assigned(BaseNode) then
    begin
      if BaseNode.DataType <> vtdtSeparator then
        Result := TfrmPropertyItem.Execute(AOwner, TvCustomRealNodeData(BaseNode))
      else
        Result := TfrmPropertySeparator.Execute(AOwner, BaseNode);
      ATreeView.InvalidateNode(ANode);

      if Not(ANewNode) then
        RefreshList(ATreeView);
    end;
  end;
end;

procedure DragDropFiles(Sender: TBaseVirtualTree; DataObject: IDataObject;
                        AttachMode: TVTNodeAttachMode);
var
  FileNames : TStringList;
  I         : Integer;
  Node      : PVirtualNode;
begin
  FileNames := TStringList.Create;
  try
    GetFileListFromObj(DataObject,FileNames);
    //Iterate drag&drop file list
    for I := 0 to FileNames.Count - 1 do
    begin
      //Add new node
      if Assigned(Sender.DropTargetNode) then
        Node := Sender.InsertNode(Sender.DropTargetNode, AttachMode, TvFileNodeData.Create(vtdtFile))
      else
        Node := Sender.AddChild(nil,TvFileNodeData.Create(vtdtFile));
      //Set node properties
      GetDropFileProperty(Sender,Node,FileNames[I]);
    end;
  finally
    FileNames.Free;
  end;
end;

procedure GetDropFileProperty(Sender: TBaseVirtualTree;Node: pVirtualNode;PathTemp: string);
var
  NodeData: PBaseData;
  FileNodeData: TvFileNodeData;
begin
  //TODO: Rename this function and params
  NodeData := GetNodeDataEx(Node, Sender);
  FileNodeData := TvFileNodeData(NodeData.Data);
  //Set some node record's variables
  FileNodeData.Name := ChangeFileExt(ExtractFileName(PathTemp), '');
  if LowerCase(ExtractFileExt(PathTemp)) = EXT_LNK then
  begin
    //Shortcut
    FileNodeData.PathExe    := Config.Paths.AbsoluteToRelative(GetShortcutTarget(PathTemp, sfPathExe));
    FileNodeData.Parameters := Config.Paths.AbsoluteToRelative(GetShortcutTarget(PathTemp, sfParameter));
    FileNodeData.WorkingDir := Config.Paths.AbsoluteToRelative(GetShortcutTarget(PathTemp, sfWorkingDir));
  end
  else //Normal file
    FileNodeData.PathExe := Config.Paths.AbsoluteToRelative(PathTemp);
  //If it is a directory, use folder icon
  if DirectoryExists(FileNodeData.PathAbsoluteExe) then
    FileNodeData.PathIcon := Config.Paths.AbsoluteToRelative(Config.Paths.SuitePathIconsTree + FILEICON_Folder);
  FileNodeData.DataType   := vtdtFile;
//  FileNodeData.CheckPathExe;
//  ImagesDM.GetNodeImageIndex(FileNodeData, isAny);
  NodeData.Data.pNode     := Node;
end;

function GetTextFromDataObject(DataObject: IDataObject): string;
var
  Medium : TStgMedium;
  PText  : PChar;

  function MakeFormatEtc(const Fmt: TClipFormat): TFormatEtc;
  begin
    Result.cfFormat := Fmt;
    Result.ptd := nil;
    Result.dwAspect := DVASPECT_CONTENT;
    Result.lindex := -1;
    Result.tymed := TYMED_HGLOBAL;
  end;

begin
  Result := '';
  if DataObject.GetData(MakeFormatEtc(CF_UNICODETEXT), Medium) = S_OK then
  begin
    Assert(Medium.tymed = MakeFormatEtc(CF_UNICODETEXT).tymed);
    try
      PText := GlobalLock(Medium.hGlobal);
      try
        Result := string(PText);
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
    finally
      ReleaseStgMedium(Medium);
    end;
  end;
end;

function DragDropText(Sender: TBaseVirtualTree;DataObject: IDataObject;
                       AttachMode: TVTNodeAttachMode; Mode: TDropMode): Boolean;
var
  Node     : PVirtualNode;
  NodeData : TvFileNodeData;
  sPath    : string;
begin
  sPath  := GetTextFromDataObject(DataObject);
  Result := sPath <> '';
  if Result then
  begin
    //Add node
    if Assigned(Sender.DropTargetNode) then
      Node := Sender.InsertNode(Sender.DropTargetNode, AttachMode, TvFileNodeData.Create(vtdtFile))
    else
      Node := Sender.AddChild(nil,TvFileNodeData.Create(vtdtFile));
    NodeData := TvFileNodeData(GetNodeItemData(Node, Sender));
    //Set node properties
    NodeData.pNode      := Node;
    NodeData.DataType   := vtdtFile;
    NodeData.Name       := 'Link';
    //Get text from DataObject
    NodeData.PathExe    := sPath;
    //Icon
    NodeData.PathIcon   := Config.Paths.AbsoluteToRelative(Config.Paths.SuitePathIconsTree + FILEICON_Url);
//    ImagesDM.GetNodeImageIndex(NodeData, isAny);
  end;
end;

procedure GetFileListFromObj(const DataObj: IDataObject; FileList: TStringList);
var
  FmtEtc: TFormatEtc;                   // specifies required data format
  Medium: TStgMedium;                   // storage medium containing file list
  DroppedFileCount: Integer;            // number of dropped files
  I: Integer;                           // loops thru dropped files
  FileNameLength: Integer;              // length of a dropped file name
  FileName: string;                 // name of a dropped file
begin
  // Get required storage medium from data object
  FmtEtc.cfFormat := CF_HDROP;
  FmtEtc.ptd := nil;
  FmtEtc.dwAspect := DVASPECT_CONTENT;
  FmtEtc.lindex := -1;
  FmtEtc.tymed := TYMED_HGLOBAL;
  OleCheck(DataObj.GetData(FmtEtc, Medium));
  try
    // Get count of files dropped
    DroppedFileCount := DragQueryFile(Medium.hGlobal, $FFFFFFFF, nil, 0);
    // Get name of each file dropped and process it
    for I := 0 to Pred(DroppedFileCount) do
    begin
      // get length of file name, then name itself
      FileNameLength := DragQueryFile(Medium.hGlobal, I, nil, 0);
      SetLength(FileName, FileNameLength);
      DragQueryFile(Medium.hGlobal, I, PChar(FileName), FileNameLength + 1);
      // add file name to list
      FileList.Append(FileName);
    end;
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(Medium.hGlobal);
    ReleaseStgMedium(Medium);
  end;
end;

function GetNodeDataEx(const ANode: PVirtualNode; const ATree: TBaseVirtualTree): PBaseData;
var
  ListNode: PVirtualNode;
begin
  //Check if ATree is MainTree (frmMain.vstList), to get nodedata from the right Tree
  Result := nil;
  if ATree <> Config.MainTree then
  begin
    //If node is from another Tree, we must find the mainnode from MainTree
    ListNode := GetListNodeFromSubTree(ANode, ATree);
    if Assigned(ListNode) then
      Result := Config.MainTree.GetNodeData(ListNode);
  end
  else
    Result := Config.MainTree.GetNodeData(ANode);

//  Assert(Assigned(Result), 'Result is not assigned');
end;

function GetNodeItemData(const ANode: PVirtualNode; const ATree: TBaseVirtualTree): TvBaseNodeData;
var
  BaseData: PBaseData;
begin
  Result := nil;
  BaseData := GetNodeDataEx(ANode, ATree);
  if Assigned(BaseData) then
    Result := BaseData.Data;
end;

function GetListNodeFromSubTree(const ANodeX: PVirtualNode;const ATree: TBaseVirtualTree): PVirtualNode;
var
  NodeDataX : PTreeDataX;
begin
  Result := nil;
  NodeDataX := ATree.GetNodeData(ANodeX);
  if Assigned(NodeDataX) then
    Result := NodeDataX.pNodeList;
end;

class procedure TIterateSubtreeProcs.AddNodeInTreeFromMainTree(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
  var Abort: Boolean);
var
  NodeData, ParentNodeData : PBaseData;
  NewNode     : PVirtualNode;
  Tree: TVirtualStringTree;

  function CreateData(ANode: PVirtualNode): rTreeDataX;
  var
    NodeData: rTreeDataX;
  begin
    NodeData.pNodeList := ANode;
    Result := NodeData;
  end;

begin
  Tree := TVirtualStringTree(Data^);
  if Assigned(Tree) then
  begin
    //Get nodadata from sender (it is frmMain.vstList)
    NodeData := Sender.GetNodeData(Node);
    if Assigned(Node) then
    begin
      //Add new child
      if (Node.Parent <> Sender.RootNode) then
      begin
        ParentNodeData  := Sender.GetNodeData(Node.Parent);
        NewNode         := Tree.AddChild(ParentNodeData.MenuNode, PTreeDataX(CreateData(Node)));
      end
      else
        NewNode         := Tree.AddChild(nil, PTreeDataX(CreateData(Node)));
      NodeData.MenuNode := NewNode;
      if NodeData.Data.HideFromMenu then
        Tree.IsVisible[NewNode] := False;
    end;
  end;
end;

class procedure TIterateSubtreeProcs.BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
var
  NodeData : TvCustomRealNodeData;
begin
  NodeData := TvCustomRealNodeData(GetNodeItemData(Node, Sender));
  if (NodeData.DataType <> vtdtSeparator) then
  begin
    //Delete desktop's shortcut
    if NodeData is TvFileNodeData then
      TvFileNodeData(NodeData).DeleteShortcutFile;
    //Remove item from special lists
    Config.ListManager.MRUList.RemoveItem(NodeData);
    Config.ListManager.MFUList.RemoveItem(NodeData);
    //Remove item from hotkey list
    if NodeData.ActiveHotkey then
      Config.ListManager.HotKeyItemList.RemoveItem(NodeData);
    //Remove item from scheduler list
    if NodeData.SchMode <> smDisabled then
      Config.ListManager.SchedulerItemList.RemoveItem(NodeData);
    if (NodeData.Autorun in [atAlwaysOnStart, atSingleInstance]) then
      Config.ListManager.StartupItemList.RemoveItem(NodeData);
    if (NodeData.Autorun in [atAlwaysOnClose]) then
      Config.ListManager.ShutdownItemList.RemoveItem(NodeData);
  end;
end;

class procedure TIterateSubtreeProcs.FindNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  LauncherSearch  : ^TLauncherSearch;
  CurrentFileData : TvFileNodeData;
  SearchNodeData  : PTreeDataX;
  SearchNode      : PVirtualNode;
  Found           : Boolean;
begin
  LauncherSearch  := Data;
  CurrentFileData := TvFileNodeData(GetNodeItemData(Node, Sender));
  if Assigned(CurrentFileData) then
    if (CurrentFileData.DataType in [vtdtFile,vtdtFolder]) then
    begin
      Found := False;
      //Search Keyword in user specified field
      case LauncherSearch.SearchType of
        stName       : Found := Pos(LauncherSearch.Keyword,LowerCase(CurrentFileData.Name)) <> 0;
        stPathExe    : Found := Pos(LauncherSearch.Keyword,LowerCase(CurrentFileData.PathExe)) <> 0;
        stPathIcon   : Found := Pos(LauncherSearch.Keyword,LowerCase(CurrentFileData.PathIcon)) <> 0;
        stWorkingDir : Found := Pos(LauncherSearch.Keyword,LowerCase(CurrentFileData.WorkingDir)) <> 0;
        stParameters : Found := Pos(LauncherSearch.Keyword,LowerCase(CurrentFileData.Parameters)) <> 0;
      end;
      //If found, add new node in LauncherSearch.Tree
      if Found then
      begin
        SearchNode     := LauncherSearch.Tree.AddChild(nil);
        SearchNodeData := LauncherSearch.Tree.GetNodeData(SearchNode);
        SearchNodeData.pNodeList := Node;
      end;
    end;
end;

class procedure TIterateSubtreeProcs.ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
  if NodeData.DataType = vtdtFile then
  begin
    //Delete shortcut on shutdown
    if TvFileNodeData(NodeData).ShortcutDesktop then
      DeleteShortcutOnDesktop(NodeData.Name + EXT_LNK);
  end;
end;

class procedure TIterateSubtreeProcs.IncNumberNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                             Data: Pointer; var Abort: Boolean);
begin
  if (Node.CheckState = csCheckedNormal) or (Node.CheckState = csMixedNormal) then
    Inc(Integer(Data^));
end;

class procedure TIterateSubtreeProcs.UpdateListItemCount(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData  : TvBaseNodeData;
  ListStats : PListStats;
begin
  ListStats := Data;
  if Assigned(Node) then
  begin
    NodeData := GetNodeItemData(Node, Config.MainTree);
    //Count Softwares and Categories
    case NodeData.DataType of
      vtdtCategory : Inc(ListStats.CatCount);
      vtdtFile, vtdtFolder : Inc(ListStats.SwCount);
      //vtdtSeparator
    end;
  end;
end;

class procedure TIterateSubtreeProcs.UpdateNodeHeight(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
  SubNode: PVirtualNode;
begin
  //Get NodeData (need it because we must know DataType)
  if Sender = Config.MainTree then
    SubNode := Node
  else //Else get right node from MainTree
    SubNode := GetListNodeFromSubTree(Node, Sender);
  NodeData  := GetNodeItemData(SubNode, Config.MainTree);
  if Assigned(NodeData) then
  begin
    //Change node height
    if NodeData.DataType = vtdtSeparator then
      Sender.NodeHeight[Node] := 18
    else
      Sender.NodeHeight[Node] := Integer(Data^);
  end;
end;

end.
