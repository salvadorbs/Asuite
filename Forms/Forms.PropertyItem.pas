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

unit Forms.PropertyItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls,
  Vcl.StdCtrls, Frame.BaseEntity, NodeDataTypes.Custom, DKLang;

type
  TfrmPropertyItem = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    pnlPropertyPage: TPanel;
    vstCategory: TVirtualStringTree;
    DKLanguageController1: TDKLanguageController;
    procedure vstCategoryFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstCategoryGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstCategoryGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstCategoryAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstCategoryInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure SaveNodeData(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
  strict private
    FCurrentPage: TfrmBaseEntityPage;
    FFrameGeneral, FFrameAdvanced: PVirtualNode;
    FListNodeData: TvCustomRealNodeData;
  public
    { Public declarations }
    class function Execute(AOwner: TComponent; ANodeData:TvCustomRealNodeData): Integer;
  end;

var
  frmPropertyItem: TfrmPropertyItem;

implementation

uses
  Utility.Frame, Frame.Properties.Advanced, Kernel.Enumerations,
  Frame.Properties.Behavior, Frame.Properties.General.Category, Frame.Properties.General.Software,
  Kernel.Types;

{$R *.dfm}

procedure TfrmPropertyItem.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmPropertyItem.btnOkClick(Sender: TObject);
var
  ResultNode: PVirtualNode;
begin
  //If IterateSubtree returns a value, something is wrong
  ResultNode := vstCategory.IterateSubtree(nil, SaveNodeData, nil);
  if Not Assigned(ResultNode) then
  begin
    FListNodeData.Changed := True;
    //Reset icon and get it again
    //TODO: Fix it
//    FListNodeData.ResetIcon(isAny);
//    ImagesDM.GetNodeImageIndex(FListNodeData, isAny);
    ModalResult := mrOk;
  end;
end;

class function TfrmPropertyItem.Execute(AOwner: TComponent; ANodeData: TvCustomRealNodeData): Integer;
var
  frm: TfrmPropertyItem;
begin
  Result := mrCancel;
  frm := TfrmPropertyItem.Create(nil);
  try
    frm.FListNodeData := ANodeData;
    //General
    if (ANodeData.DataType = vtdtFile) or (ANodeData.DataType = vtdtFolder) then
      frm.FFrameGeneral := AddFrameNode(frm.vstCategory, nil, TPageFrameClass(TfrmSWGeneralPropertyPage.Create(frm, ANodeData)))
    else
      if ANodeData.DataType = vtdtCategory then
        frm.FFrameGeneral := AddFrameNode(frm.vstCategory, nil, TPageFrameClass(TfrmCatGeneralPropertyPage.Create(frm, ANodeData)));
    frm.FFrameAdvanced := AddFrameNode(frm.vstCategory, nil, TPageFrameClass(TfrmAdvancedPropertyPage.Create(frm, ANodeData)));
    AddFrameNode(frm.vstCategory, nil, TPageFrameClass(TfrmBehaviorPropertyPage.Create(frm, ANodeData)));
    //Select node (automatically open frame using vst's AddToSelection event)
    frm.vstCategory.FocusedNode := frm.FFrameGeneral;
    frm.vstCategory.Selected[frm.FFrameGeneral] := True;
    frm.vstCategory.FullExpand;
    Result := frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmPropertyItem.FormCreate(Sender: TObject);
begin
  vstCategory.NodeDataSize := SizeOf(rFramesNodeData);
  vstCategory.Clear;
end;

procedure TfrmPropertyItem.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Ord(Key) of
    VK_RETURN: btnOkClick(Sender);
  end;
end;

procedure TfrmPropertyItem.SaveNodeData(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData : PFramesNodeData;
begin
  //Call Frame's function SaveData
  NodeData := vstCategory.GetNodeData(Node);
  Abort := Not(TfrmBaseEntityPage(NodeData.Frame).SaveData);
end;

procedure TfrmPropertyItem.vstCategoryAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    LoadPage(FCurrentPage, NodeData.Frame, pnlPropertyPage);
end;

procedure TfrmPropertyItem.vstCategoryFreeNode(Sender: TBaseVirtualTree;
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

procedure TfrmPropertyItem.vstCategoryGetImageIndex(Sender: TBaseVirtualTree;
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

procedure TfrmPropertyItem.vstCategoryGetText(Sender: TBaseVirtualTree;
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

procedure TfrmPropertyItem.vstCategoryInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.NodeHeight := 36;
end;

end.
