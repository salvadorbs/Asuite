{
Copyright (C) 2006-2020 Matteo Salvi

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

unit VirtualTree.Events;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, VirtualTrees, ActiveX,
  Kernel.Singleton, Forms.GraphicMenu, Forms.Dialog.BaseEntity, Menus, Forms, Controls;

type
  TVirtualTreeEvents = class(TSingleton)
  private
    FGraphicMenu: TfrmGraphicMenu;

    function ClickOnButtonTree(Sender: TBaseVirtualTree; const HitInfo: THitInfo): Boolean;
    function GetNodeParentName(const ASender: TBaseVirtualTree; const ANode: PVirtualNode): string;
    procedure DrawSeparatorItem(const ASender: TBaseVirtualTree; const ANode: PVirtualNode;
                                TargetCanvas: TCanvas; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure DragDropFiles(const ASender: TBaseVirtualTree; ADataObject: IDataObject;
                            AttachMode: TVTNodeAttachMode);
    function GetTextFromDataObject(DataObject: IDataObject): string;
    procedure GetFileListFromDataObject(const DataObj: IDataObject; FileList: TStringList);
  public
    //Methods to set events in vsts
    procedure SetupVSTList(ATree: TVirtualStringTree);
    procedure SetupVSTSimple(ATree: TVirtualStringTree);
    procedure SetupVSTSearch(ATree: TVirtualStringTree);
    procedure SetupVSTGraphicMenu(ATree: TVirtualStringTree; AGraphicMenu: TfrmGraphicMenu);
    procedure SetupVSTImportList(ATree: TVirtualStringTree);
    procedure SetupVSTDialogFrame(ATree: TVirtualStringTree);
    procedure SetupVSTHotkey(ATree: TVirtualStringTree);
    procedure SetupVSTAutorun(ATree: TVirtualStringTree);

    //Generic events
    procedure DoDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
      State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord;
      var Accept: Boolean);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DoGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DoPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure DoSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure DoLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure DoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      const NewText: String);
    procedure DoDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure DoEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DoDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure DoNodeSingleClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure DoExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; var NodeHeight: Integer);

    //Specific events
    //List events
    procedure DoCompareNodesList(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DoGetNodeDataSizeList(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);

    //Search events
    procedure DoCompareNodesSearch(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DoGetNodeDataSizeSearch(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);

    //Graphic Menu Events
    procedure DoResizeGM(Sender: TObject);
    procedure DoScrollGM(Sender: TBaseVirtualTree; DeltaX,
      DeltaY: Integer);
    procedure DoSingleClickGM(Sender: TObject);

    //Frame events
    procedure DoGetNodeDataSizeFrame(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure DoGetTextFrame(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DoGetImageIndexFrame(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DoFreeNodeFrame(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoAddToSelectionFrame(Sender: TBaseVirtualTree;
      Node: PVirtualNode);

    //Hotkey events
    procedure DoCompareNodesHotkey(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DoGetTextHotkey(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DoGetImageIndexHotkey(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);

    //Autorun events
    procedure DoGetTextAutorun(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  end;

implementation

uses
  AppConfig.Main, NodeDataTypes.Base, NodeDataTypes.Category,
  NodeDataTypes.Files, NodeDataTypes.Custom, NodeDataTypes.Separator, Kernel.Types,
  Kernel.Enumerations, Frame.BaseEntity, VirtualTree.Methods, DataModules.TrayMenu,
  comobj, DataModules.Icons, Kernel.Logger, SynLog, Utility.Hotkey, ShellApi,
  Windows;

{ TVirtualTreeEvents }

procedure TVirtualTreeEvents.DoNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  //Check if user click on node or expand button (+/-)
  if Not(ClickOnButtonTree(Sender, HitInfo)) then
    TVirtualTreeMethods.Create.ExecuteSelectedNodes(Sender, rmNormal, False);
end;

procedure TVirtualTreeEvents.DoNodeSingleClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  //Check if user click on node or expand button (+/-)
  if Not(ClickOnButtonTree(Sender, HitInfo)) then
    if (Config.RunSingleClick) then
      TVirtualTreeMethods.Create.ExecuteSelectedNodes(Sender, rmNormal, False)
end;

procedure TVirtualTreeEvents.DoSingleClickGM(Sender: TObject);
var
  Tree     : TBaseVirtualTree;
  NodeData : TvBaseNodeData;
  Point    : TPoint;
  HitInfo  : ThitInfo;
begin
  Tree  := TBaseVirtualTree(Sender);
  Point := Tree.ScreenToClient(Mouse.CursorPos);
  Tree.GetHitTestInfoAt(Point.X, Point.Y, True, HitInfo);
  if Assigned(HitInfo.HitNode) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Tree.GetFirstSelected, Tree);
    if NodeData.DataType = vtdtCategory then
    begin
      Tree.Expanded[Tree.GetFirstSelected] := Not(Tree.Expanded[Tree.GetFirstSelected]);
      FGraphicMenu.FocusControl(FGraphicMenu.edtSearch);
    end
    else
      if NodeData.DataType = vtdtFile then
      begin
        DoNodeDblClick(Tree, HitInfo);
        FGraphicMenu.CloseMenu;
      end;
  end;
end;

procedure TVirtualTreeEvents.SetupVSTGraphicMenu(ATree: TVirtualStringTree; AGraphicMenu: TfrmGraphicMenu);
begin
  FGraphicMenu := AGraphicMenu;

  ATree.OnClick         := DoSingleClickGM;
  ATree.OnCompareNodes  := DoCompareNodesList;
  ATree.OnDrawText      := DoDrawText;
  ATree.OnExpanded      := DoExpanded;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
  ATree.OnGetText       := DoGetText;
  ATree.OnGetImageIndex := DoGetImageIndex;
  ATree.OnMeasureItem   := DoMeasureItem;
  ATree.OnResize        := DoResizeGM;
  ATree.OnScroll        := DoScrollGM;
end;

procedure TVirtualTreeEvents.SetupVSTHotkey(ATree: TVirtualStringTree);
begin
  ATree.Images := dmImages.ilLargeIcons;
  ATree.DefaultNodeHeight := Config.BigHeightNode;

  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
  ATree.OnCompareNodes    := DoCompareNodesHotkey;
  ATree.OnGetText         := DoGetTextHotkey;
  ATree.OnGetImageIndex   := DoGetImageIndexHotkey;
end;

procedure TVirtualTreeEvents.SetupVSTImportList(ATree: TVirtualStringTree);
begin
  ATree.Images := dmImages.ilSmallIcons;

  ATree.OnDrawText  := DoDrawText;
  ATree.OnFreeNode  := DoFreeNode;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeList;
  ATree.OnGetText   := DoGetText;
  ATree.OnGetImageIndex := DoGetImageIndex;
end;

procedure TVirtualTreeEvents.SetupVSTList(ATree: TVirtualStringTree);
begin
  ATree.Images := dmImages.ilSmallIcons;

  ATree.OnNodeClick    := DoNodeSingleClick;
  ATree.OnCompareNodes := DoCompareNodesList;
  ATree.OnNodeDblClick := DoNodeDblClick;
  ATree.OnDragOver     := DoDragOver;
  ATree.OnDragDrop     := DoDragDrop;
  ATree.OnDrawText     := DoDrawText;
  ATree.OnEditing      := DoEditing;
  ATree.OnExpanded     := DoExpanded;
  ATree.OnFreeNode     := DoFreeNode;
  ATree.OnGetText      := DoGetText;
  ATree.OnPaintText    := DoPaintText;
  ATree.OnGetImageIndex   := DoGetImageIndex;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeList;
  ATree.OnKeyPress     := DoKeyPress;
  ATree.OnLoadNode     := DoLoadNode;
  ATree.OnNewText      := DoNewText;
  ATree.OnSaveNode     := DoSaveNode;
  ATree.OnMeasureItem  := DoMeasureItem;
end;

procedure TVirtualTreeEvents.SetupVSTAutorun(ATree: TVirtualStringTree);
begin
  ATree.Images := dmImages.ilLargeIcons;
  ATree.DefaultNodeHeight := Config.BigHeightNode;

  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
  ATree.OnGetText         := DoGetTextAutorun;
  ATree.OnGetImageIndex   := DoGetImageIndexHotkey;
end;

procedure TVirtualTreeEvents.SetupVSTDialogFrame(ATree: TVirtualStringTree);
begin
  ATree.Clear;
  ATree.Images := dmImages.ilLargeIcons;
  ATree.DefaultNodeHeight := Config.BigHeightNode;

  ATree.OnAddToSelection  := DoAddToSelectionFrame;
  ATree.OnFreeNode        := DoFreeNodeFrame;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeFrame;
  ATree.OnGetText         := DoGetTextFrame;
  ATree.OnGetImageIndex   := DoGetImageIndexFrame;
end;

procedure TVirtualTreeEvents.SetupVSTSearch(ATree: TVirtualStringTree);
begin
  ATree.Images := dmImages.ilSmallIcons;

  ATree.OnNodeClick       := DoNodeSingleClick;
  ATree.OnCompareNodes    := DoCompareNodesSearch;
  ATree.OnNodeDblClick    := DoNodeDblClick;
  ATree.OnGetText         := DoGetText;
  ATree.OnGetImageIndex   := DoGetImageIndex;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
  ATree.OnKeyPress        := DoKeyPress;
end;

procedure TVirtualTreeEvents.SetupVSTSimple(ATree: TVirtualStringTree);
begin
  ATree.Images := dmImages.ilSmallIcons;

  ATree.OnGetText         := DoGetText;
  ATree.OnGetImageIndex   := DoGetImageIndex;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
end;

function TVirtualTreeEvents.ClickOnButtonTree(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo): Boolean;
begin
  Result := hiOnItemButton in HitInfo.HitPositions;
end;

procedure TVirtualTreeEvents.DoAddToSelectionFrame(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) and (Sender.Parent is TfrmDialogBase) then
    TfrmDialogBase(Sender.Parent).ChangePage(NodeData.Frame);
end;

procedure TVirtualTreeEvents.DoCompareNodesHotkey(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: TvCustomRealNodeData;
begin
  Data1 := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(Node1, Sender));
  Data2 := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(Node2, Sender));
  if (Not Assigned(Data1)) or (Not Assigned(Data2)) then
    Result := 0
  else
    case Column of
      0: Result := CompareText(Data1.Name, Data2.Name);
      1:
      begin
        if (Data1.DataType) <> (Data2.DataType) then
        begin
          if Data1.DataType = vtdtCategory then
            Result := -1
          else
            Result := 1
        end;
      end;
      2: Result := CompareText(GetNodeParentName(Sender, Data1.pNode), GetNodeParentName(Sender, Data2.pNode));
      3: Result := CompareText(HotKeyToText(Data1.Hotkey, False), HotKeyToText(Data2.Hotkey, False));
    end;
end;

procedure TVirtualTreeEvents.DoCompareNodesList(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: TvBaseNodeData;
begin
  Data1 := TVirtualTreeMethods.Create.GetNodeItemData(Node1, Sender);
  Data2 := TVirtualTreeMethods.Create.GetNodeItemData(Node2, Sender);
  if (Not Assigned(Data1)) or (Not Assigned(Data2)) then
    Result := 0
  else
    if (Data1.DataType = vtdtCategory) <> (Data2.DataType = vtdtCategory) then
    begin
      if Data1.DataType = vtdtCategory then
        Result := -1
      else
        Result := 1
    end
    else
      Result := CompareText(Data1.Name, Data2.Name);
end;

procedure TVirtualTreeEvents.DoDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  I          : integer;
  NodeData   : TvBaseNodeData;
  AttachMode : TVTNodeAttachMode;
  NodeCreated : Boolean;
begin
  TASuiteLogger.Enter('DoDragDrop', Self);
  NodeCreated := False;
  case Mode of
    dmAbove  : AttachMode := amInsertBefore;
    dmOnNode : AttachMode := amAddChildLast;
    dmBelow  : AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;
  if Assigned(DataObject) then
  begin
    Sender.BeginUpdate;
    try
      if (Mode = dmOnNode) and Assigned(Sender.DropTargetNode) then
      begin
        NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Sender.DropTargetNode, Sender);
        //Check if DropMode is in a vtdtCategory (so expand it, before drop item)
        //or another item type (change Mode and AttachMode for insert after new nodes)
        if NodeData.DataType <> vtdtCategory then
          AttachMode := amInsertAfter
        else
          if Config.TVAutoOpCatsDrag then
            Sender.Expanded[Sender.DropTargetNode] := True;
      end;
      try
        for I := 0 to High(Formats) do
        begin
          //Files
          if Formats[I] = CF_HDROP then
            DragDropFiles(Sender, DataObject, AttachMode)
          else //VirtualTree Nodes
            if Formats[I] = CF_VIRTUALTREE then
            begin
              TASuiteLogger.Info('Moves VirtualTree nodes', []);
              Sender.ProcessDrop(DataObject, Sender.DropTargetNode, Effect, AttachMode);
            end
            else //Text
              if (Formats[I] = CF_UNICODETEXT) and Not(NodeCreated) then
                NodeCreated := TVirtualTreeMethods.Create.AddNodeByText(Sender, Sender.DropTargetNode, GetTextFromDataObject(DataObject), AttachMode);
        end;
      except
        on E : Exception do
          ;//ShowMessageFmtEx(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message], True);
      end;
    finally
      TVirtualTreeMethods.Create.RefreshList(Sender);
      Sender.EndUpdate;
    end;
  end;
end;

procedure TVirtualTreeEvents.DoDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
  var Effect: LongWord; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TVirtualTreeEvents.DoDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DrawSeparatorItem(Sender, Node, TargetCanvas, CellRect, DefaultDraw);
end;

procedure TVirtualTreeEvents.DoEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  Allowed  := (NodeData.DataType <> vtdtSeparator) and Not(Config.RunSingleClick);
end;

procedure TVirtualTreeEvents.DoExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
//  TVirtualTreeMethods.Create.CheckVisibleNodePathExe(Sender);
end;

procedure TVirtualTreeEvents.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    FreeAndNil(NodeData);
end;

procedure TVirtualTreeEvents.DoFreeNodeFrame(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
  begin
    TfrmBaseEntityPage(NodeData.Frame).Free;
    NodeData.Title := '';
  end;
end;

procedure TVirtualTreeEvents.DoGetNodeDataSizeFrame(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rFramesNodeData);
end;

procedure TVirtualTreeEvents.DoGetNodeDataSizeList(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rBaseData);
end;

procedure TVirtualTreeEvents.DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
  begin
    if Column = 1 then
      CellText := GetNodeParentName(Sender, NodeData.pNode)
    else begin
      if (NodeData.DataType = vtdtSeparator) and (NodeData.Name = '') then
        CellText := ' '
      else
        CellText := NodeData.Name;
    end;
  end;
end;

procedure TVirtualTreeEvents.DoGetTextAutorun(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData : TvCustomRealNodeData;
begin
  CellText := '';
  NodeData := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender));
  if Assigned(NodeData) then
  begin
    case Column of
      0: CellText := NodeData.Name;
      2: CellText := GetNodeParentName(Sender, NodeData.pNode);
      3:
      begin
        if NodeData.DataType = vtdtFile then
          CellText := TvFileNodeData(NodeData).PathFile;
      end;
    end;
  end;
end;

procedure TVirtualTreeEvents.DoGetTextFrame(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData : PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  CellText := 'Options Page';
  if Assigned(NodeData) then
    if NodeData.Title <> '' then
      CellText := NodeData.Title;
end;

procedure TVirtualTreeEvents.DoGetTextHotkey(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData : TvCustomRealNodeData;
begin
  CellText := '';
  NodeData := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender));
  if Assigned(NodeData) then
  begin
    case Column of
      0: CellText := NodeData.Name;
      2: CellText := GetNodeParentName(Sender, NodeData.pNode);
      3: CellText := HotKeyToText(NodeData.Hotkey, False)
    end;
  end;
end;

procedure TVirtualTreeEvents.DoKeyPress(Sender: TObject; var Key: Char);
begin
  if (Sender is TBaseVirtualTree) then
    if Ord(Key) = VK_RETURN then
      TVirtualTreeMethods.Create.ExecuteSelectedNodes((Sender as TBaseVirtualTree), rmNormal, False)
end;

procedure TVirtualTreeEvents.DoMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    if NodeData.DataType = vtdtSeparator then
      NodeHeight := Config.SmallHeightNode;
end;

procedure TVirtualTreeEvents.DoLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Stream: TStream);
var
  DataDest, DataSource: PBaseData;
begin
  //Create a new PBaseData as source
  New(DataSource);
  Stream.ReadBuffer(DataSource^,SizeOf(rBaseData));
  //Copy source's properties in DataDest
  DataDest := Sender.GetNodeData(Node);
  DataDest.Data := TVirtualTreeMethods.Create.CreateNodeData(DataSource.Data.DataType);
  //Copy DataSource in DataDest
  case DataSource.Data.DataType of
    vtdtCategory  : TvCategoryNodeData(DataDest.Data).Copy(DataSource.Data);
    vtdtFile      : TvFileNodeData(DataDest.Data).Copy(DataSource.Data);
    vtdtFolder    : TvFileNodeData(DataDest.Data).Copy(DataSource.Data);
    vtdtSeparator : TvSeparatorNodeData(DataDest.Data).Copy(DataSource.Data);
  end;
  //New node can't use same hotkey of old node
  if DataDest.Data is TvCustomRealNodeData then
  begin
    TvCustomRealNodeData(DataDest.Data).ActiveHotkey := False;
    TvCustomRealNodeData(DataDest.Data).Hotkey := 0;
  end;
  //Set some personal record fields
  DataDest.Data.SetPointerNode(Node);

  FreeMem(DataSource);
end;

procedure TVirtualTreeEvents.DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    const NewText: String);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    NodeData.Name := NewText;
  TVirtualTreeMethods.Create.RefreshList(Sender);
end;

procedure TVirtualTreeEvents.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  NodeData: TvBaseNodeData;
begin
  //Get data and check if AbsoluteExe path exists
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    if NodeData.DataType = vtdtFile then
      if Not(TvFileNodeData(NodeData).IsPathFileExists) then
        TargetCanvas.Font.Color := clRed;
  //TODO: change clred in case of black theme
end;

procedure TVirtualTreeEvents.DoResizeGM(Sender: TObject);
var
  DY: integer;
begin
  if Sender is TVirtualStringTree then
  begin
    DY := TVirtualStringTree(Sender).DefaultNodeHeight;
    if TVirtualStringTree(Sender).DefaultNodeHeight = Config.BigHeightNode then
      TVirtualStringTree(Sender).BottomSpace := 1
    else
      TVirtualStringTree(Sender).BottomSpace := TVirtualStringTree(Sender).ClientHeight mod DY;
    TVirtualStringTree(Sender).OffsetY := Round(TVirtualStringTree(Sender).OffsetY / DY) * DY;
  end;
end;

procedure TVirtualTreeEvents.DoSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Stream: TStream);
var
  Data: PBaseData;
begin
  Data := TVirtualTreeMethods.Create.GetNodeDataEx(Node, Sender);
  Stream.WriteBuffer(Data^,SizeOf(rBaseData));
end;

procedure TVirtualTreeEvents.DoScrollGM(Sender: TBaseVirtualTree; DeltaX,
  DeltaY: Integer);
var
  DY: integer;
begin
  if DeltaY <> 0 then
  begin
    DY := TVirtualStringTree(Sender).DefaultNodeHeight;
    Sender.OffsetY := Round(Sender.OffsetY / DY) * DY;
  end;
end;

procedure TVirtualTreeEvents.DragDropFiles(const ASender: TBaseVirtualTree;
  ADataObject: IDataObject; AttachMode: TVTNodeAttachMode);
var
  FileNames : TStringList;
  I         : Integer;
begin
  TASuiteLogger.Info('Drag&Drop files in ASuite', []);
  FileNames := TStringList.Create;
  try
    GetFileListFromDataObject(ADataObject, FileNames);
    //Iterate file list to add nodes
    for I := 0 to FileNames.Count - 1 do
      TVirtualTreeMethods.Create.AddNodeByPathFile(ASender, ASender.DropTargetNode, FileNames[I], AttachMode);
  finally
    FileNames.Free;
  end;
end;

procedure TVirtualTreeEvents.DrawSeparatorItem(const ASender: TBaseVirtualTree;
  const ANode: PVirtualNode; TargetCanvas: TCanvas; const CellRect: TRect;
  var DefaultDraw: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(ANode, ASender);
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

procedure TVirtualTreeEvents.GetFileListFromDataObject(
  const DataObj: IDataObject; FileList: TStringList);
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

function TVirtualTreeEvents.GetNodeParentName(const ASender: TBaseVirtualTree;
  const ANode: PVirtualNode): string;
var
  CatData: TvBaseNodeData;
begin
  Result := '';
  if (ANode.Parent <> Config.MainTree.RootNode) then
  begin
    CatData := TVirtualTreeMethods.Create.GetNodeItemData(ANode.Parent, Config.MainTree);
    if Assigned(CatData) then
      Result  := CatData.Name;
  end
  else
    Result := '<Root>';
end;

function TVirtualTreeEvents.GetTextFromDataObject(
  DataObject: IDataObject): string;
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

procedure TVirtualTreeEvents.DoCompareNodesSearch(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2, CatData1, CatData2: TvBaseNodeData;
  CatName1, CatName2 : String;
begin
  Data1 := TVirtualTreeMethods.Create.GetNodeItemData(Node1, Sender);
  Data2 := TVirtualTreeMethods.Create.GetNodeItemData(Node2, Sender);
  if (Not Assigned(Data1)) or (Not Assigned(Data2)) then
    Result := 0
  else
    if Column = 0 then
      Result := CompareText(Data1.Name, Data2.Name)
    else begin
      CatData1 := TVirtualTreeMethods.Create.GetNodeItemData(Data1.pNode.Parent, Config.MainTree);
      CatData2 := TVirtualTreeMethods.Create.GetNodeItemData(Data2.pNode.Parent, Config.MainTree);
      if Assigned(CatData1) then
        CatName1 := CatData1.Name
      else
        CatName1 := '';
      if Assigned(CatData2) then
        CatName2 := CatData2.Name
      else
        CatName2 := '';
      Result  := CompareText(CatName1, CatName2)
    end;
end;

procedure TVirtualTreeEvents.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: TvBaseNodeData;
begin
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
    if (Column = 0) or (Column = -1) then
      ImageIndex := NodeData.Icon.ImageIndex
    else
      ImageIndex := -1;
  end;
end;

procedure TVirtualTreeEvents.DoGetImageIndexFrame(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : PFramesNodeData;
begin
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    NodeData := Sender.GetNodeData(Node);
    if Assigned(NodeData) then
      ImageIndex := NodeData.ImageIndex;
  end;
end;

procedure TVirtualTreeEvents.DoGetImageIndexHotkey(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: TvBaseNodeData;
begin
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
    if Assigned(NodeData) then
    begin
      case Column of
        0: ImageIndex := NodeData.Icon.ImageIndex;
        1:
        begin
          if NodeData.DataType = vtdtCategory then
            ImageIndex := Config.IconsManager.GetIconIndex('category')
          else
            ImageIndex := Config.IconsManager.GetIconIndex('file');
        end;
      end;
    end;
  end;
end;

procedure TVirtualTreeEvents.DoGetNodeDataSizeSearch(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rTreeDataX);
end;

end.
