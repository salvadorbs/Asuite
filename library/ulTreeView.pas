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

unit ulTreeView;

interface

uses
  Windows, SysUtils, ActiveX, VirtualTrees, Controls, ulCommonClasses, AppConfig,
  ulNodeDataTypes, ulEnumerations, Classes, ShellApi, comobj, ulXMLUtils, DKLang;

{ List, Menu, MRU }
function  AddNodeInVST(Sender: TBaseVirtualTree;ParentNode: PVirtualNode;AType: TvTreeDataType): PVirtualNode;
function  CreateListItem(Sender: TBaseVirtualTree;AType: TvTreeDataType): PBaseData;
function  ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;
procedure DragDropFiles(Sender: TBaseVirtualTree; DataObject: IDataObject;
                        AttachMode: TVTNodeAttachMode; Mode: TDropMode);
procedure GetDropFileProperty(Sender: TBaseVirtualTree; Node: pVirtualNode;
                       PathTemp: string);
procedure DragDropText(Sender: TBaseVirtualTree;DataObject: IDataObject;
                       AttachMode: TVTNodeAttachMode; Mode: TDropMode);
procedure GetFileListFromObj(const DataObj: IDataObject; FileList: TStringList);
function  GetNodeDataEx(Node: PVirtualNode; TreeView, SearchTree, ListTree: TBaseVirtualTree): PBaseData;
function  GetNodeDataSearch(NodeX: PVirtualNode; SearchTree, ListTree: TBaseVirtualTree): PBaseData;
procedure RefreshList(Tree: TBaseVirtualTree);

type

  { TIterateSubtreeProcs }

  TIterateSubtreeProcs = class //Class for IterateSubtree
    procedure FindNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    procedure GMFindNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    procedure BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);
    procedure ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                Data: Pointer; var Abort: Boolean);
    procedure IncNumberNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
    procedure UpdateListItemCount(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                  Data: Pointer; var Abort: Boolean);
  end;


  TListStats = record
      SwCount      : Integer;
      CatCount     : Integer;
    end;

var
  MRUList : TMRUList;
  MFUList : TMFUList;
  SearchType : TSearchType;
  IterateSubTreeProcs : TIterateSubtreeProcs;
  ImportOldListProcs  : TImportOldListProcs;
  StartupItemList,                        //Software in StartUp list
  ShutdownItemList    : TAutorunItemList; //Software in Shutdown list
  SchedulerItemList   : TNodeDataList;
  HotKeyApp : THotkeyList;
  ListStats : TListStats;     //Stats

implementation

uses
  Menus, ulSysUtils, udImages, Main, ulDatabase, ulAppConfig, ulFileFolder,
  GraphicMenu, PropertyItem, PropertySeparator;

function AddNodeInVST(Sender: TBaseVirtualTree;ParentNode: PVirtualNode;AType: TvTreeDataType): PVirtualNode;
var
  NodeData: PBaseData;
begin
  Result := nil;
  if Assigned(ParentNode) then
  begin
    NodeData := Sender.GetNodeData(ParentNode);
    case NodeData.Data.DataType of
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

function CreateListItem(Sender: TBaseVirtualTree;AType: TvTreeDataType): PBaseData;
var
  CurrNode, ChildNode  : PVirtualNode;
  NodeData             : PBaseData;
  FolderPath, tempName : String;
begin
  Result     := nil;
  FolderPath := '';
  CurrNode   := Sender.FocusedNode;
  ChildNode  := AddNodeInVST(Sender, CurrNode, AType);
  //Set ChildNode's pNode and name (temporary)
  NodeData            := Sender.GetNodeData(ChildNode);
  NodeData.Data.pNode := ChildNode;
  NodeData.Data.Name := DKLangConstW('msgNoName') + IntToStr(Sender.TotalCount);
  //Refresh List
  RefreshList(frmMain.vstList);
  //ShowPropertyItem
  //Separator
  if AType = vtdtSeparator then
  begin
    if (TfrmPropertySeparator.Edit(Sender, NodeData) <> mrOK) then
      Sender.DeleteNode(ChildNode);
  end //Other types
  else begin
    if AType = vtdtFolder then
    begin
      FolderPath := BrowseForFolder('',SUITE_WORKING_PATH);
      if FolderPath <> '' then
      begin
        tempName := ExtractDirectoryName(FolderPath + PathDelim);
        if tempName <> '' then
          NodeData.Data.Name := tempName;
        TvFileNodeData(NodeData.Data).PathExe := AbsoluteToRelative(FolderPath + PathDelim);
      end
      else begin
        Sender.DeleteNode(ChildNode);
        Exit;
      end;
    end;
    if (frmMain.ShowItemProperty(Sender, ChildNode) <> mrOK) then
      Sender.DeleteNode(ChildNode);
  end;
  if Assigned(NodeData) then
    Result := NodeData;
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

procedure DragDropFiles(Sender: TBaseVirtualTree; DataObject: IDataObject;
                        AttachMode: TVTNodeAttachMode; Mode: TDropMode);
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
        Node := Sender.InsertNode(Sender.DropTargetNode, AttachMode,TvFileNodeData.Create(vtdtFile))
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
  NodeData := PBaseData(Sender.GetNodeData(Node));
  FileNodeData := TvFileNodeData(NodeData.Data);
  //Set some node record's variables
  FileNodeData.Name := ChangeFileExt(ExtractFileName(PathTemp), '');
  if LowerCase(ExtractFileExt(PathTemp)) = EXT_LNK then
  begin
    //Shortcut
    FileNodeData.PathExe    := AbsoluteToRelative(GetShortcutTarget(PathTemp, sfPathExe));
    FileNodeData.Parameters := AbsoluteToRelative(GetShortcutTarget(PathTemp, sfParameter));
    FileNodeData.WorkingDir := AbsoluteToRelative(GetShortcutTarget(PathTemp, sfWorkingDir));
  end
  else //Normal file
    FileNodeData.PathExe := AbsoluteToRelative(PathTemp);
  //If it is a directory, use folder icon
  if DirectoryExists(FileNodeData.PathAbsoluteExe) then
    FileNodeData.PathIcon := AbsoluteToRelative(SUITE_SMALLICONS_PATH + FILEICON_Folder);
  FileNodeData.DataType   := vtdtFile;
  FileNodeData.ImageIndex := ImagesDM.GetIconIndex(FileNodeData);
  NodeData.Data.pNode     := Node;
end;

procedure DragDropText(Sender: TBaseVirtualTree;DataObject: IDataObject;
                       AttachMode: TVTNodeAttachMode; Mode: TDropMode);
var
  Node     : PVirtualNode;
  NodeData : PBaseData;
  Medium   : TStgMedium;
  PText    : PAnsiChar;

  function MakeFormatEtc(const Fmt: TClipFormat): TFormatEtc;
  begin
    Result.cfFormat := Fmt;
    Result.ptd := nil;
    Result.dwAspect := DVASPECT_CONTENT;
    Result.lindex := -1;
    Result.tymed := TYMED_HGLOBAL;
  end;

begin
  //Add node
  if Assigned(Sender.DropTargetNode) then
    Node := Sender.InsertNode(Sender.DropTargetNode, AttachMode, TvFileNodeData.Create(vtdtFile))
  else
    Node := Sender.AddChild(nil,TvFileNodeData.Create(vtdtFile));
  NodeData := Sender.GetNodeData(Node);
  //Set node properties
  NodeData.Data.pNode := Node;
  with TvFileNodeData(NodeData.Data) do
  begin
    DataType   := vtdtFile;
    Name       := 'Link';
    //Get text from DataObject
    if DataObject.GetData(MakeFormatEtc(CF_TEXT), Medium) = S_OK then
    begin
      Assert(Medium.tymed = MakeFormatEtc(CF_TEXT).tymed);
      try
        PText := GlobalLock(Medium.hGlobal);
        try
          PathExe := string(PText);
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      finally
        ReleaseStgMedium(Medium);
      end;
    end;
    //Icon
    PathIcon   := AbsoluteToRelative(SUITE_SMALLICONS_PATH + FILEICON_Url);
    ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(NodeData.Data));
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

function GetNodeDataEx(Node: PVirtualNode; TreeView, SearchTree, ListTree: TBaseVirtualTree): PBaseData;
begin
  if (TreeView = ListTree) then //List Treeview
    Result := TreeView.GetNodeData(Node)
  else //Search Treeview
    Result := GetNodeDataSearch(Node, SearchTree, ListTree);
end;

function GetNodeDataSearch(NodeX: PVirtualNode; SearchTree, ListTree: TBaseVirtualTree): PBaseData;
var
  NodeDataX : PTreeDataX;
begin
  Result := nil;
  NodeDataX := SearchTree.GetNodeData(NodeX);
  if Assigned(NodeDataX) then
    Result  := ListTree.GetNodeData(NodeDataX.pNodeList);
end;

procedure RefreshList(Tree: TBaseVirtualTree);
begin
  DBManager.SaveData(Tree);
end;

//------------------------------------------------------------------------------

procedure TIterateSubtreeProcs.FindNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  FilterData, CurrentFileData : TvFileNodeData;
  FoundNodeData : PTreeDataX;
  FoundNode     : PVirtualNode;
  Found         : Boolean;
begin
  FilterData      := Data;
  CurrentFileData := TvFileNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  if (CurrentFileData.DataType in [vtdtFile,vtdtFolder]) then
  begin
    Found := False;
    case SearchType of
      stName       : Found := Pos(LowerCase(FilterData.Name),LowerCase(CurrentFileData.Name)) <> 0;
      stPathExe    : Found := Pos(LowerCase(FilterData.PathExe),LowerCase(CurrentFileData.PathExe)) <> 0;
      stPathIcon   : Found := Pos(LowerCase(FilterData.PathIcon),LowerCase(CurrentFileData.PathIcon)) <> 0;
      stWorkingDir : Found := Pos(LowerCase(FilterData.WorkingDir),LowerCase(CurrentFileData.WorkingDir)) <> 0;
      stParameters : Found := Pos(LowerCase(FilterData.Parameters),LowerCase(CurrentFileData.Parameters)) <> 0;
    end;
    if Found then
    begin
      FoundNode               := frmMain.vstSearch.AddChild(nil);
      FoundNodeData           := frmMain.vstSearch.GetNodeData(FoundNode);
      //Get node's image, if it hasn't
      if CurrentFileData.ImageIndex = -1 then
        CurrentFileData.ImageIndex := ImagesDM.GetIconIndex(CurrentFileData);
      FoundNodeData.pNodeList := Node;
    end;
  end;
end;

procedure TIterateSubtreeProcs.GMFindNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  FilterData, CurrentFileData : TvFileNodeData;
  FoundNodeData : PTreeDataX;
  FoundNode     : PVirtualNode;
  Found         : Boolean;
begin
  FilterData      := Data;
  CurrentFileData := TvFileNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  if (CurrentFileData.DataType in [vtdtFile,vtdtFolder]) then
  begin
    Found := False;
    case SearchType of
      stName       : Found := Pos(LowerCase(FilterData.Name),LowerCase(CurrentFileData.Name)) <> 0;
      stPathExe    : Found := Pos(LowerCase(FilterData.PathExe),LowerCase(CurrentFileData.PathExe)) <> 0;
      stPathIcon   : Found := Pos(LowerCase(FilterData.PathIcon),LowerCase(CurrentFileData.PathIcon)) <> 0;
      stWorkingDir : Found := Pos(LowerCase(FilterData.WorkingDir),LowerCase(CurrentFileData.WorkingDir)) <> 0;
      stParameters : Found := Pos(LowerCase(FilterData.Parameters),LowerCase(CurrentFileData.Parameters)) <> 0;
    end;
    if Found then
    begin
      FoundNode               := frmGraphicMenu.vstSearch.AddChild(nil);
      FoundNodeData           := frmGraphicMenu.vstSearch.GetNodeData(FoundNode);
      //Get node's image, if it hasn't
      if CurrentFileData.ImageLargeIndex = -1 then
        CurrentFileData.ImageLargeIndex := ImagesDM.GetIconIndex(CurrentFileData, True);
      FoundNodeData.pNodeList := Node;
    end;
  end;
end;

procedure TIterateSubtreeProcs.BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
var
  NodeData : TvCustomRealNodeData;
begin
  NodeData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  if (NodeData.DataType <> vtdtSeparator) then
  begin
    //Delete cache icon
    NodeData.DeleteCacheIcon;
    //Delete desktop's shortcut
    if NodeData is TvFileNodeData then
      TvFileNodeData(NodeData).DeleteShortcutFile;
    //Remove item from special lists
    MRUList.Remove(NodeData);
    MFUList.Remove(NodeData);
    //Remove item from hotkey list
    if NodeData.Hotkey then
      HotKeyApp.Remove(NodeData);
    //Remove item from scheduler list
    if NodeData.SchMode <> smDisabled then
      SchedulerItemList.Remove(NodeData);
    if (NodeData.Autorun in [atAlwaysOnStart, atSingleInstance]) then
      StartupItemList.Remove(NodeData);
    if (NodeData.Autorun in [atAlwaysOnClose]) then
      ShutdownItemList.Remove(NodeData);
  end;
  //Remove item from sqlite database
  DBManager.DeleteItem(NodeData.ID);
end;

procedure TIterateSubtreeProcs.ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
var
  NodeData: PBaseData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData.Data.DataType = vtdtFile then
  begin
    //Delete shortcut on shutdown
    if TvFileNodeData(NodeData.Data).ShortcutDesktop then
      DeleteShortcutOnDesktop(NodeData.Data.Name + EXT_LNK);
  end;
end;

procedure TIterateSubtreeProcs.IncNumberNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                             Data: Pointer; var Abort: Boolean);
begin
  if (Node.CheckState = csCheckedNormal) or (Node.CheckState = csMixedNormal) then
    Inc(Integer(Data^));
end;

procedure TIterateSubtreeProcs.UpdateListItemCount(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData :PBaseData;
begin
  if Assigned(Node) then
  begin
    NodeData := frmMain.vstList.GetNodeData(Node);
    //Count Softwares and Categories
    case NodeData^.Data.DataType of
      vtdtCategory : Inc(ListStats.CatCount);
      vtdtFile, vtdtFolder : Inc(ListStats.SwCount);
      //vtdtSeparator
    end;
  end;
end;

end.
