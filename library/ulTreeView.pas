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

unit ulTreeView;

interface

uses
  Windows, SysUtils, ActiveX, VirtualTrees, Controls, ulCommonClasses, AppConfig,
  ulNodeDataTypes, ulEnumerations, ulSQLite, FileUtil, Classes, ShellApi, comobj;

{ List, Menu, MRU }
function  AddNode(Sender: TBaseVirtualTree;AType: TvTreeDataType): PBaseData;
function  ClickOnButtonTree(Sender: TBaseVirtualTree): Boolean;
procedure DragDropFiles(Sender: TBaseVirtualTree; DataObject: IDataObject;
                        AttachMode: TVTNodeAttachMode; Mode: TDropMode);
procedure GetDropFileProperty(Sender: TBaseVirtualTree; Node: pVirtualNode;
                       PathTemp: string);
procedure DragDropText(Sender: TBaseVirtualTree;DataObject: IDataObject;
                       AttachMode: TVTNodeAttachMode; Mode: TDropMode);
procedure GetChildNodesIcons(Sender: TBaseVirtualTree; Node: PVirtualNode);
procedure GetFileListFromObj(const DataObj: IDataObject; FileList: TStringList);
function  GetNodeDataEx(Node: PVirtualNode; TreeView, SearchTree, ListTree: TBaseVirtualTree): PBaseData;
function  GetNodeDataSearch(NodeX: PVirtualNode; SearchTree, ListTree: TBaseVirtualTree): PBaseData;
procedure RefreshList(Tree: TBaseVirtualTree);

type

  { TIterateSubtreeProcs }

  TIterateSubtreeProcs = class //Class for IterateSubtree
    procedure FindNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
      SwGroupCount : Integer;
      CatCount     : Integer;
    end;

var
  MRUList : TMRUList;
  MFUList : TMFUList;
  SearchType : TSearchType;
  IterateSubTreeProcs : TIterateSubtreeProcs;
  ASuiteStartUpApp,                     //Software in StartUp list
  ASuiteShutdownApp : TAutorunItemList; //Software in Shutdown list
  ListStats: TListStats;     //Stats

implementation

uses
  Menus, PropertyFile, PropertyCat, ulAppConfig, ulSysUtils, udImages, PropertySeparator,
  Main, ulDatabase;

function AddNode(Sender: TBaseVirtualTree;AType: TvTreeDataType): PBaseData;
var
  CurrNode, ChildNode  : PVirtualNode;
  NodeData             : PBaseData;
  FolderPath, tempName : String;
begin
  FolderPath := '';
  CurrNode := Sender.FocusedNode;
  if (CurrNode <> nil) then
    begin
      NodeData := Sender.GetNodeData(CurrNode);
      // Separator: always insert after
      if (AType = vtdtSeparator) then
        ChildNode := Sender.InsertNode(CurrNode, amInsertAfter, CreateNodeData(AType))
      else
        if (NodeData.Data.DataType = vtdtCategory) then
        begin
          // if category then expand and add subnode
          Sender.Expanded[CurrNode] := true;
          ChildNode := Sender.AddChild(CurrNode, CreateNodeData(AType));
        end
        else // else get parent and add to it
          ChildNode := Sender.InsertNode(CurrNode, amInsertAfter, CreateNodeData(AType));
    end
  else
    ChildNode := Sender.AddChild(Sender.RootNode, CreateNodeData(AType));
  //Set ChildNode's pNode and name (temporary)
  NodeData           := Sender.GetNodeData(ChildNode);
  NodeData.pNode     := ChildNode;
  NodeData.Data.ParentNode := ChildNode.Parent;
  NodeData.Data.Name := msgNoName + IntToStr(Sender.TotalCount);
  //Set some its variables depending on its type
  //Don't necessary for separator
  case AType of
    vtdtCategory:
    begin
      //Category
      NodeData.Data.ImageIndex := IMG_Cat;
      if (TfrmPropertyCat.Edit(Sender, NodeData) <> mrOK) then
        Sender.DeleteNode(ChildNode);
    end;
    vtdtFile:
    begin
      //File
      if (TfrmPropertyFile.Edit(Sender, NodeData) <> mrOK) then
        Sender.DeleteNode(ChildNode);
    end;
    vtdtFolder:
    begin
      //Folder
      NodeData.Data.PathIcon := AbsoluteToRelative(SUITE_ICONS_PATH + IntToStr(IMG_Folder) + EXT_ICO);
      FolderPath             := BrowseForFolder('',SUITE_WORKING_PATH);
      if FolderPath <> '' then
      begin
        tempName := ExtractDirectoryName(FolderPath + PathDelim);
        if tempName <> '' then
          NodeData.Data.Name   := tempName;
        TvFileNodeData(NodeData.Data).PathExe := AbsoluteToRelative(FolderPath + PathDelim);
        if (TfrmPropertyFile.Edit(Sender, NodeData) <> mrOK) then
          Sender.DeleteNode(ChildNode);
      end
      else
        Sender.DeleteNode(ChildNode);
    end;
    vtdtSeparator:
    begin
      //Separator
      if (TfrmPropertySeparator.Edit(Sender, NodeData) <> mrOK) then
        Sender.DeleteNode(ChildNode);
    end;
  end;
  RefreshList(Sender);
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
  NodeData  : PBaseData;
begin
  //If dropMode is on node and this node isn't a category type item, then exit
  if (Mode = dmOnNode) and (AttachMode = amNoWhere) then
    exit;
  FileNames := TStringList.Create;
  try
    GetFileListFromObj(DataObject,FileNames);
    //Iterate drag&drop file list
    for I := 0 to FileNames.Count - 1 do
    begin
      //Add new node
      if Assigned(Sender.DropTargetNode) then
        Node := Sender.InsertNode(Sender.DropTargetNode, AttachMode,TvFileNodeData.Create)
      else
        Node := Sender.AddChild(nil,TvFileNodeData.Create);
      //Set node properties
      GetDropFileProperty(Sender,Node,FileNames[I]);
    end;
  finally
    FileNames.Free;
  end;
end;

procedure GetDropFileProperty(Sender: TBaseVirtualTree;Node: pVirtualNode;PathTemp: string);
var
  NodeData : PBaseData;
  Name     : string;
begin
  NodeData := Sender.GetNodeData(Node);
  Name     := ExtractFileName(PathTemp);
  //If it is a directory, use folder icon else get its icon
  if DirectoryExistsUTF8(PathTemp) then
    NodeData.Data.PathIcon := AbsoluteToRelative(SUITE_ICONS_PATH + IntToStr(IMG_Folder) + EXT_ICO)
  else
    Delete(Name,pos(ExtractFileExt(PathTemp),name),Length(name));
  //Set some node record's variables
  NodeData.Data.Name := Name;
  if LowerCase(ExtractFileExt(PathTemp)) = EXT_LNK then
  begin
    //Shortcut
    with TvFileNodeData(NodeData.Data) do
    begin
      PathExe    := AbsoluteToRelative(GetShortcutTarget(PathTemp,sfPathExe));
      Parameters := AbsoluteToRelative(GetShortcutTarget(PathTemp,sfParameter));
      WorkingDir := AbsoluteToRelative(GetShortcutTarget(PathTemp,sfWorkingDir));
    end;
  end
  else //Normal file
    TvFileNodeData(NodeData.Data).PathExe := AbsoluteToRelative(PathTemp);
  NodeData.Data.DataType   := vtdtFile;
  NodeData.Data.ImageIndex := ImagesDM.GetIconIndex(NodeData.Data);
  NodeData.Data.ParentNode := Node.Parent;
  NodeData.pNode           := Node;
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
  //If dropMode is on node and this node isn't a category type item, then exit
  if (Mode = dmOnNode) and (AttachMode = amNoWhere) then
    exit;
  //Add node
  if Assigned(Sender.DropTargetNode) then
    Node := Sender.InsertNode(Sender.DropTargetNode, AttachMode, TvFileNodeData.Create)
  else
    Node := Sender.AddChild(nil,TvFileNodeData.Create);
  NodeData := Sender.GetNodeData(Node);
  //Set node properties
  NodeData.pNode := Node;
  with NodeData.Data do
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
          TvFileNodeData(NodeData.Data).PathExe := PText;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      finally
        ReleaseStgMedium(Medium);
      end;
    end;
    //Icon
    PathIcon   := AbsoluteToRelative(SUITE_ICONS_PATH + IntToStr(IMG_Url) + EXT_ICO);
    ImageIndex := ImagesDM.GetIconIndex(NodeData.Data);
    ParentNode := Node.Parent;
  end;
end;

procedure GetChildNodesIcons(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ChildNode : PVirtualNode;
  NodeData  : TvBaseNodeData;
begin
  ChildNode := Sender.GetFirstChild(Node);
  while Assigned(ChildNode) do
  begin
    NodeData := PBaseData(Sender.GetNodeData(ChildNode)).Data;
    if Assigned(NodeData) and (NodeData.ImageIndex = -1) then
      NodeData.ImageIndex := ImagesDM.GetIconIndex(NodeData);
    ChildNode := Sender.GetNextSibling(ChildNode);
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
  SaveASuiteSQLite(Tree, true);
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
      FoundNodeData.pNodeX    := FoundNode;
    end;
  end;
end;

procedure TIterateSubtreeProcs.BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
var
  NodeData : PBaseData;
begin
  NodeData := Sender.GetNodeData(Node);
  //Delete cache icon
  if (NodeData.Data.DataType = vtdtFile) and FileExistsUTF8(NodeData.Data.PathCacheIcon) then
    DeleteFileUTF8(NodeData.Data.PathCacheIcon);
  //Delete desktop's shortcut, if exists
  if (TvFileNodeData(NodeData.Data).ShortcutDesktop) then
    DeleteShortcutOnDesktop(TvFileNodeData(NodeData.Data).Name + EXT_LNK);
  //Remove item from special menu
  MRUList.Remove(NodeData.Data);
  MFUList.Remove(NodeData.Data);
  //Remove item from sqlite database
  DBManager.DeleteItem(NodeData.Data.ID);
end;

procedure TIterateSubtreeProcs.ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
var
  NodeData : PBaseData;
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
      vtdtFile : Inc(ListStats.SwCount);
      vtdtFolder : Inc(ListStats.SwGroupCount);
      //vtdtSeparator
    end;
  end;
end;

end.
