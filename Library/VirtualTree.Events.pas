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

unit VirtualTree.Events;

interface

uses
  Windows, SysUtils, Classes, Graphics, VirtualTrees, ActiveX, UITypes, DKLang,
  Kernel.Singleton, Forms.GraphicMenu, Forms.Dialog.BaseEntity;

type
  TVirtualTreeEvents = class(TSingleton)
  private
    FGraphicMenu: TfrmGraphicMenu;
    FDialogForm: TfrmDialogBase;
  public
    //Methods to set events in vsts
    procedure SetupVSTList(ATree: TVirtualStringTree);
    procedure SetupVSTSearch(ATree: TVirtualStringTree);
    procedure SetupVSTGraphicMenu(ATree: TVirtualStringTree; AGraphicMenu: TfrmGraphicMenu);
    procedure SetupVSTImportList(ATree: TVirtualStringTree);
    procedure SetupVSTDialogFrame(ATree: TVirtualStringTree; ADialogForm: TfrmDialogBase);
    procedure ResetDialogFrame;

    //Generic events
    procedure DoDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
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
    procedure DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure DoExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure DoDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
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
    procedure DoNodeSingleClickGM(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

    //Frame events
    procedure DoGetNodeDataSizeFrame(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure DoGetTextFrame(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure DoGetImageIndexFrame(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DoFreeNodeFrame(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoInitNodeFrame(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoAddToSelectionFrame(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  end;

implementation

uses
  Utility.Misc, AppConfig.Main, Utility.TreeView, NodeDataTypes.Base, NodeDataTypes.Category,
  NodeDataTypes.Files, NodeDataTypes.Custom, NodeDataTypes.Separator, Kernel.Types,
  Kernel.Enumerations, Frame.BaseEntity, Controls;

{ TVirtualTreeEvents }

procedure TVirtualTreeEvents.DoNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  //Check if user click on node or expand button (+/-)
  if Not(ClickOnButtonTree(Sender, HitInfo)) then
//    RunNormalSw(Sender);
end;

procedure TVirtualTreeEvents.DoNodeSingleClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  //Check if user click on node or expand button (+/-)
  if Not(ClickOnButtonTree(Sender, HitInfo)) then
    if (Config.RunSingleClick) then
//      RunNormalSw(Sender);
end;

procedure TVirtualTreeEvents.DoNodeSingleClickGM(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  NodeData : TvBaseNodeData;
begin
  if Assigned(Sender.FocusedNode) then
  begin
    NodeData := GetNodeItemData(Sender.FocusedNode, Sender);
    if NodeData.DataType = vtdtCategory then
    begin
      Sender.Expanded[Sender.FocusedNode] := Not(Sender.Expanded[Sender.FocusedNode]);
      FGraphicMenu.FocusControl(FGraphicMenu.edtSearch);
    end
    else
      if NodeData.DataType = vtdtFile then
      begin
        DoNodeSingleClick(Sender, HitInfo);
        FGraphicMenu.CloseMenu;
      end;
  end;
end;

procedure TVirtualTreeEvents.SetupVSTGraphicMenu(ATree: TVirtualStringTree; AGraphicMenu: TfrmGraphicMenu);
begin
  FGraphicMenu := AGraphicMenu;

  ATree.OnNodeClick     := DoNodeSingleClickGM;
  ATree.OnCompareNodes  := DoCompareNodesList;
  ATree.OnDrawText      := DoDrawText;
  ATree.OnExpanded      := DoExpanded;
  ATree.OnExpanding     := DoExpanding;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
  ATree.OnGetText       := DoGetText;
  ATree.OnGetImageIndex := DoGetImageIndex;
  ATree.OnMeasureItem   := DoMeasureItem;
  ATree.OnResize        := DoResizeGM;
  ATree.OnScroll        := DoScrollGM;
end;

procedure TVirtualTreeEvents.SetupVSTImportList(ATree: TVirtualStringTree);
begin
  ATree.OnDrawText  := DoDrawText;
  ATree.OnExpanding := DoExpanding;
  ATree.OnFreeNode  := DoFreeNode;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeList;
  ATree.OnGetText   := DoGetText;
  ATree.OnGetImageIndex := DoGetImageIndex;
end;

procedure TVirtualTreeEvents.SetupVSTList(ATree: TVirtualStringTree);
begin
  ATree.OnNodeClick    := DoNodeSingleClick;
  ATree.OnCompareNodes := DoCompareNodesList;
  ATree.OnNodeDblClick := DoNodeDblClick;
  ATree.OnDragOver     := DoDragOver;
  ATree.OnDragDrop     := DoDragDrop;
  ATree.OnDrawText     := DoDrawText;
  ATree.OnEditing      := DoEditing;
  ATree.OnExpanded     := DoExpanded;
  ATree.OnExpanding    := DoExpanding;
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

procedure TVirtualTreeEvents.SetupVSTDialogFrame(ATree: TVirtualStringTree; ADialogForm: TfrmDialogBase);
begin
  ATree.Clear;

  FDialogForm := ADialogForm;
  ATree.OnAddToSelection  := DoAddToSelectionFrame;
  ATree.OnFreeNode        := DoFreeNodeFrame;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeFrame;
  ATree.OnGetText         := DoGetTextFrame;
  ATree.OnGetImageIndex   := DoGetImageIndexFrame;
  ATree.OnInitNode        := DoInitNodeFrame;
end;

procedure TVirtualTreeEvents.SetupVSTSearch(ATree: TVirtualStringTree);
begin
  ATree.OnNodeClick       := DoNodeSingleClick;
  ATree.OnCompareNodes    := DoCompareNodesSearch;
  ATree.OnNodeDblClick    := DoNodeDblClick;
  ATree.OnGetText         := DoGetText;
  ATree.OnGetImageIndex   := DoGetImageIndex;
  ATree.OnGetNodeDataSize := DoGetNodeDataSizeSearch;
  ATree.OnKeyPress        := DoKeyPress;
end;

procedure TVirtualTreeEvents.DoAddToSelectionFrame(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) and Assigned(FDialogForm) then
  begin
    if Assigned(FDialogForm.CurrentPage) then
    begin
      if FDialogForm.CurrentPage.ClassType = NodeData.Frame then
        Exit
      else
       FDialogForm.CurrentPage.Visible := False;
    end;
    FDialogForm.CurrentPage := TfrmBaseEntityPage(NodeData.Frame);
    FDialogForm.CurrentPage.Parent  := FDialogForm.pnlDialogPage;
    FDialogForm.CurrentPage.Align   := alClient;
    FDialogForm.CurrentPage.Visible := True;
  end;
end;

procedure TVirtualTreeEvents.DoCompareNodesList(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: TvBaseNodeData;
begin
  Data1 := GetNodeItemData(Node1, Sender);
  Data2 := GetNodeItemData(Node2, Sender);
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

procedure TVirtualTreeEvents.DoDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  I          : integer;
  NodeData   : TvBaseNodeData;
  AttachMode : TVTNodeAttachMode;
  NodeCreated : Boolean;
begin
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
      NodeData := GetNodeItemData(Sender.DropTargetNode, Sender);
      if Mode = dmOnNode then
      begin
        //Check if DropMode is in a vtdtCategory (so expand it, before drop item)
        //or another item type (change Mode and AttachMode for insert after new nodes)
        if NodeData.DataType <> vtdtCategory then
        begin
          Mode := dmBelow;
          AttachMode := amInsertAfter;
        end
        else
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
              Sender.ProcessDrop(DataObject, Sender.DropTargetNode, Effect, AttachMode)
            else //Text
              if (Formats[I] = CF_UNICODETEXT) and Not(NodeCreated) then
                NodeCreated := DragDropText(Sender, DataObject, AttachMode, Mode);
        end;
      except
        on E : Exception do
          ShowMessageFmtEx(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message], true);
      end;
    finally
      RefreshList(Sender);
      Sender.EndUpdate;
    end;
  end;
end;

procedure TVirtualTreeEvents.DoDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
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
  NodeData := GetNodeItemData(Node, Sender);
  Allowed  := (NodeData.DataType <> vtdtSeparator) and Not(Config.RunSingleClick);
end;

procedure TVirtualTreeEvents.DoExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  CheckVisibleNodePathExe(Sender);
end;

procedure TVirtualTreeEvents.DoExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
  //TODO: Fix it
//  if NodeData.DataType = vtdtCategory then
//ImagesDM.GetChildNodesIcons(Sender, nil, Node);
end;

procedure TVirtualTreeEvents.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
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
  NodeData := GetNodeItemData(Node, Sender);
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

procedure TVirtualTreeEvents.DoInitNodeFrame(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.NodeHeight := 36;
end;

procedure TVirtualTreeEvents.DoKeyPress(Sender: TObject; var Key: Char);
begin
//  if (Sender is TBaseVirtualTree) then
//    if Ord(Key) = VK_RETURN then
//        RunNormalSw((Sender as TBaseVirtualTree));
end;

procedure TVirtualTreeEvents.DoMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    if NodeData.DataType = vtdtSeparator then
      NodeHeight := 18;
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
  DataDest.Data := CreateNodeData(DataSource.Data.DataType);
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
  DataDest.Data.pNode := Node;
  //Icon
//  ImagesDM.GetNodeImageIndex(TvCustomRealNodeData(DataDest.Data), isAny);
  FreeMem(DataSource);
end;

procedure TVirtualTreeEvents.DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    NodeData.Name := NewText;
  RefreshList(Sender);
end;

procedure TVirtualTreeEvents.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  //TODO: Paint node color, if path exe doesn't exists
end;

procedure TVirtualTreeEvents.DoResizeGM(Sender: TObject);
var
  DY: integer;
begin
  if Sender is TVirtualStringTree then
  begin
    DY := TVirtualStringTree(Sender).DefaultNodeHeight;
    if TVirtualStringTree(Sender).DefaultNodeHeight = 36 then
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
  Data := GetNodeDataEx(Node, Sender);
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

procedure TVirtualTreeEvents.ResetDialogFrame;
begin
  FDialogForm := nil;
end;

procedure TVirtualTreeEvents.DoCompareNodesSearch(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2, CatData1, CatData2: TvBaseNodeData;
  CatName1, CatName2 : String;
begin
  Data1 := GetNodeItemData(Node1, Sender);
  Data2 := GetNodeItemData(Node2, Sender);
  if (Not Assigned(Data1)) or (Not Assigned(Data2)) then
    Result := 0
  else
    if Column = 0 then
      Result := CompareText(Data1.Name, Data2.Name)
    else begin
      CatData1 := GetNodeItemData(Data1.pNode.Parent, Config.MainTree);
      CatData2 := GetNodeItemData(Data2.pNode.Parent, Config.MainTree);
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
    NodeData   := GetNodeDataEx(Node, Sender).Data;
    if Column = 0 then
    begin
      if TVirtualStringTree(Sender).DefaultNodeHeight = 18 then
        ImageIndex := NodeData.ImageIndex
      else
        ImageIndex := NodeData.ImageLargeIndex;
    end
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

procedure TVirtualTreeEvents.DoGetNodeDataSizeSearch(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rTreeDataX);
end;

end.
