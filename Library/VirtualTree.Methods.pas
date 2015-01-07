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

unit VirtualTree.Methods;

interface

uses
  Windows, SysUtils, Classes, Graphics, VirtualTrees, ActiveX, UITypes, DKLang,
  Kernel.Singleton, Kernel.Enumerations, NodeDataTypes.Base, Kernel.Types, Lists.Base;

type
  TVirtualTreeMethods = class(TSingleton)
  private
  public
    //Add child methods
    function AddChildNodeEx(const ASender: TBaseVirtualTree; AParentNode: PVirtualNode;
                            AAttachMode: TVTNodeAttachMode; AType: TvTreeDataType;
                            ADoExpand: Boolean = True): PVirtualNode;
    function AddChildNodeByGUI(const ASender: TBaseVirtualTree; AParentNode: PVirtualNode;
                                AType: TvTreeDataType): TvBaseNodeData;
    procedure AddNodeByPathFile(const ASender: TBaseVirtualTree; AParentNode: PVirtualNode;
                                const APathFile: string; AAttachMode: TVTNodeAttachMode);
    function AddNodeByText(const ASender: TBaseVirtualTree; AParentNode: PVirtualNode;
                           const AText: string; AAttachMode: TVTNodeAttachMode): Boolean;

    //Get Data from Virtual TreeView
    function GetNodeDataEx(const ANode: PVirtualNode; const ATree: TBaseVirtualTree): PBaseData;
    function GetNodeItemData(const ANode: PVirtualNode; const ATree: TBaseVirtualTree): TvBaseNodeData;
    function GetListNodeFromSubTree(const ANodeX: PVirtualNode;const ATree: TBaseVirtualTree): PVirtualNode;

    //Visual
    procedure ChangeAllNodeHeight(const ASender: TBaseVirtualTree; const ANewNodeHeight: Integer);
    procedure ChangeTreeIconSize(const ASender: TVirtualStringTree; const ASmallIcon: Boolean);
    procedure CheckVisibleNodePathExe(const ASender: TBaseVirtualTree);

    //Iterate methods
    procedure BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);
    procedure ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                Data: Pointer; var Abort: Boolean);
    procedure AddNodeInTreeFromMainTree(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                        Data: Pointer; var Abort: Boolean);
    procedure FindNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    procedure IncNumberNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
    procedure UpdateListItemCount(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                  Data: Pointer; var Abort: Boolean);
    procedure UpdateNodeHeight(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);

    //Misc
    function CreateNodeData(AType: TvTreeDataType): TvBaseNodeData;
    function ShowItemProperty(const AOwner: TComponent; const ATreeView: TBaseVirtualTree;
                              const ANode: PVirtualNode; ANewNode: Boolean = False): Integer;
    procedure RefreshList(const ATree: TBaseVirtualTree);
    procedure PopulateVSTItemList(const ATree: TBaseVirtualTree; const ABaseItemsList: TBaseItemsList);
  end;

implementation

uses
  Utility.System, DataModules.Images, AppConfig.Main, NodeDataTypes.Files,
  Utility.FileFolder, Forms.PropertySeparator,
  NodeDataTypes.Category, NodeDataTypes.Separator, Forms.PropertyItem,
  NodeDataTypes.Custom, Kernel.Consts;

{ TVirtualTreeMethods }

function TVirtualTreeMethods.AddChildNodeByGUI(const ASender: TBaseVirtualTree;
  AParentNode: PVirtualNode; AType: TvTreeDataType): TvBaseNodeData;
var
  ChildNode : PVirtualNode;
  NodeData  : TvBaseNodeData;
  FolderPath, sName : String;
begin
  FolderPath := '';
  Result     := nil;
  NodeData   := nil;
  try
    ChildNode  := AddChildNodeEx(ASender, AParentNode, amInsertAfter, AType);
    //Set ChildNode's pNode and name (temporary)
    NodeData       := GetNodeItemData(ChildNode, ASender);
    NodeData.Name  := DKLangConstW('msgNoName') + IntToStr(ASender.TotalCount);
    //If AType is a vtdtFolder, asuite must ask to user the folder
    if AType = vtdtFolder then
    begin
      FolderPath := BrowseForFolder(Config.Paths.SuitePathWorking);
      if FolderPath <> '' then
      begin
        sName := ExtractDirectoryName(FolderPath + PathDelim);
        if sName <> '' then
          NodeData.Name := sName;
        TvFileNodeData(NodeData).PathExe := Config.Paths.AbsoluteToRelative(FolderPath + PathDelim);
      end
      else begin
        ASender.DeleteNode(ChildNode);
        Exit;
      end;
    end;
    //ShowPropertyItem
    if (ShowItemProperty(nil, ASender, ChildNode, True) <> mrOK) then
      ASender.DeleteNode(ChildNode);
  finally
    if Assigned(NodeData) then
      Result := NodeData;
    RefreshList(ASender);
  end;
end;

function TVirtualTreeMethods.AddChildNodeEx(const ASender: TBaseVirtualTree;
  AParentNode: PVirtualNode; AAttachMode: TVTNodeAttachMode;
  AType: TvTreeDataType; ADoExpand: Boolean): PVirtualNode;
var
  ParentData, NodeData: TvBaseNodeData;
begin
  Result := nil;
  try
    if Assigned(AParentNode) then
    begin
      ParentData := GetNodeItemData(AParentNode, ASender);
      //If parent node is a category then expand
      if ParentData.DataType = vtdtCategory then
      begin
        Result := ASender.AddChild(AParentNode, CreateNodeData(AType));
        if ADoExpand then
          ASender.Expanded[AParentNode] := True;
      end
      else
        Result := ASender.InsertNode(AParentNode, AAttachMode, CreateNodeData(AType));
    end
    else
      Result := ASender.AddChild(nil, CreateNodeData(AType));
  finally
    //Set pointer node in TvBaseNodeData
    NodeData := GetNodeItemData(Result, ASender);
    NodeData.SetPointerNode(Result);
  end;
end;

procedure TVirtualTreeMethods.AddNodeByPathFile(const ASender: TBaseVirtualTree;
  AParentNode: PVirtualNode; const APathFile: string;
  AAttachMode: TVTNodeAttachMode);
var
  NodeData: TvFileNodeData;
  Node: PVirtualNode;
begin
  Node := AddChildNodeEx(ASender, AParentNode, AAttachMode, vtdtFile);
  NodeData := TvFileNodeData(GetNodeItemData(Node, ASender));
  //Set some node record's variables
  NodeData.Name := ChangeFileExt(ExtractFileName(APathFile), '');
  if LowerCase(ExtractFileExt(APathFile)) = EXT_LNK then
  begin
    //Shortcut
    NodeData.PathExe    := Config.Paths.AbsoluteToRelative(GetShortcutTarget(APathFile, sfPathExe));
    NodeData.Parameters := Config.Paths.AbsoluteToRelative(GetShortcutTarget(APathFile, sfParameter));
    NodeData.WorkingDir := Config.Paths.AbsoluteToRelative(GetShortcutTarget(APathFile, sfWorkingDir));
  end
  else begin
    if LowerCase(ExtractFileExt(APathFile)) = EXT_URL then
    begin
      //Shortcut
      NodeData.PathExe    := Config.Paths.AbsoluteToRelative(GetUrlTarget(APathFile, sfPathExe));
      NodeData.Parameters := Config.Paths.AbsoluteToRelative(GetUrlTarget(APathFile, sfParameter));
      NodeData.WorkingDir := Config.Paths.AbsoluteToRelative(GetUrlTarget(APathFile, sfWorkingDir));
    end
    else //Normal file
      NodeData.PathExe := Config.Paths.AbsoluteToRelative(APathFile);
  end;
  //If it is a directory, use folder icon
  if DirectoryExists(NodeData.PathAbsoluteExe) then
    NodeData.PathIcon := Config.Paths.AbsoluteToRelative(Config.Paths.SuitePathIconsTree + FILEICON_Folder);
//  NodeData.CheckPathExe;
//  ImagesDM.GetNodeImageIndex(NodeData, isAny);
end;

function TVirtualTreeMethods.AddNodeByText(const ASender: TBaseVirtualTree;
  AParentNode: PVirtualNode; const AText: string;
  AAttachMode: TVTNodeAttachMode): Boolean;
var
  Node     : PVirtualNode;
  NodeData : TvFileNodeData;
begin
  Result := AText <> '';
  if Result then
  begin
    //Add node and set node properties
    Node := AddChildNodeEx(ASender, AParentNode, AAttachMode, vtdtFile);
    NodeData := TvFileNodeData(GetNodeItemData(Node, ASender));
    NodeData.Name     := 'Link';
    NodeData.PathExe  := AText;
    NodeData.PathIcon := Config.Paths.AbsoluteToRelative(Config.Paths.SuitePathIconsTree + FILEICON_Url);
//    ImagesDM.GetNodeImageIndex(NodeData, isAny);
  end;
end;

procedure TVirtualTreeMethods.ChangeAllNodeHeight(
  const ASender: TBaseVirtualTree; const ANewNodeHeight: Integer);
begin
  ASender.IterateSubtree(nil, UpdateNodeHeight, @ANewNodeHeight);
end;

procedure TVirtualTreeMethods.ChangeTreeIconSize(
  const ASender: TVirtualStringTree; const ASmallIcon: Boolean);
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

procedure TVirtualTreeMethods.CheckVisibleNodePathExe(
  const ASender: TBaseVirtualTree);
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

function TVirtualTreeMethods.CreateNodeData(
  AType: TvTreeDataType): TvBaseNodeData;
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

function TVirtualTreeMethods.GetListNodeFromSubTree(const ANodeX: PVirtualNode;
  const ATree: TBaseVirtualTree): PVirtualNode;
var
  NodeDataX : PTreeDataX;
begin
  Result := nil;
  NodeDataX := ATree.GetNodeData(ANodeX);
  if Assigned(NodeDataX) then
    Result := NodeDataX.pNodeList;
end;

function TVirtualTreeMethods.GetNodeDataEx(const ANode: PVirtualNode;
  const ATree: TBaseVirtualTree): PBaseData;
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

function TVirtualTreeMethods.GetNodeItemData(const ANode: PVirtualNode;
  const ATree: TBaseVirtualTree): TvBaseNodeData;
var
  BaseData: PBaseData;
begin
  Result := nil;
  BaseData := GetNodeDataEx(ANode, ATree);
  if Assigned(BaseData) then
    Result := BaseData.Data;
end;

procedure TVirtualTreeMethods.RefreshList(const ATree: TBaseVirtualTree);
begin
  Config.SaveList(Config.ASuiteState = lsStartUp);
  //Check paths of only visible nodes
  if Assigned(ATree) then
    CheckVisibleNodePathExe(ATree);
end;

function TVirtualTreeMethods.ShowItemProperty(const AOwner: TComponent;
  const ATreeView: TBaseVirtualTree; const ANode: PVirtualNode;
  ANewNode: Boolean): Integer;
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

procedure TVirtualTreeMethods.AddNodeInTreeFromMainTree(
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

procedure TVirtualTreeMethods.BeforeDeleteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
    Config.ListManager.RemoveItemFromLists(NodeData);
  end;
  //Remove item from sqlite database
  Config.DBManager.RemoveItem(NodeData.ID);
end;

procedure TVirtualTreeMethods.FindNode(Sender: TBaseVirtualTree;
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

procedure TVirtualTreeMethods.ActionsOnShutdown(Sender: TBaseVirtualTree; Node: PVirtualNode;
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

procedure TVirtualTreeMethods.IncNumberNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                             Data: Pointer; var Abort: Boolean);
begin
  if (Node.CheckState = csCheckedNormal) or (Node.CheckState = csMixedNormal) then
    Inc(Integer(Data^));
end;

procedure TVirtualTreeMethods.PopulateVSTItemList(const ATree: TBaseVirtualTree;
  const ABaseItemsList: TBaseItemsList);
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
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(ATree);
    //TODO: Fix it
//ImagesDM.GetChildNodesIcons(ATree, ATree.RootNode, isAny);
    //Auto columns width
    TVirtualStringTree(ATree).Header.AutoFitColumns;
  finally
    ATree.EndUpdate;
  end;
end;

procedure TVirtualTreeMethods.UpdateListItemCount(Sender: TBaseVirtualTree;
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

procedure TVirtualTreeMethods.UpdateNodeHeight(Sender: TBaseVirtualTree;
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
