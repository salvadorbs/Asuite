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

unit Frame.Properties.General.Category;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees, DKLang,
  Frame.Properties.General;

type
  TfrmCatGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    grpSubItems: TGroupBox;
    vstCategoryItems: TVirtualStringTree;
    lblNote: TLabel;
    DKLanguageController1: TDKLanguageController;
    procedure vstCategoryItemsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstCategoryItemsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    { Private declarations }
    procedure GetCategoryItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure SetCategoryItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmCatGeneralPropertyPage: TfrmCatGeneralPropertyPage;

implementation

uses
  NodeDataTypes.Custom, NodeDataTypes.Files, NodeDataTypes.Base,
  DataModules.Images, Kernel.Types, Kernel.Enumerations, Forms.Main, Utility.Treeview;

{$R *.dfm}

{ TfrmCatGeneralPropertyPage }

procedure TfrmCatGeneralPropertyPage.GetCategoryItems(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  CurrentFileData : TvCustomRealNodeData;
  NewNode         : PVirtualNode;
  NewNodeData     : PTreeDataX;
begin
  CurrentFileData := TvCustomRealNodeData(GetNodeItemData(Node, Sender));
  if (CurrentFileData.DataType in [vtdtFile,vtdtFolder]) and
     (Node.Parent = PVirtualNode(Data)) then
  begin
    //Add new checked node in vstCategoryItems
    NewNode     := vstCategoryItems.AddChild(vstCategoryItems.RootNode);
    vstCategoryItems.CheckType[NewNode]  := ctTriStateCheckBox;
    //Check or uncheck new node
    if TvFileNodeData(CurrentFileData).RunFromCategory then
      vstCategoryItems.CheckState[NewNode] := csCheckedNormal
    else
      vstCategoryItems.CheckState[NewNode] := csUncheckedNormal;
    NewNodeData := vstCategoryItems.GetNodeData(NewNode);
    //Set pointers
    NewNodeData.pNodeList := Node;
  end;
end;

function TfrmCatGeneralPropertyPage.InternalLoadData: Boolean;
var
  Node: PVirtualNode;
begin
  Result := inherited;
  vstCategoryItems.NodeDataSize := SizeOf(rTreeDataX);
  vstCategoryItems.Images       := dmImages.IcoImages;
  if Assigned(CurrentNodeData) then
  begin
    Node := CurrentNodeData.pNode;
    //Get items list
    frmMain.vstList.IterateSubtree(Node,GetCategoryItems,Pointer(Node));
  end;
end;

function TfrmCatGeneralPropertyPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
    vstCategoryItems.IterateSubtree(nil,SetCategoryItems,nil,[],False);
end;

procedure TfrmCatGeneralPropertyPage.SetCategoryItems(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  FileNodeData : TvFileNodeData;
begin
  FileNodeData := TvFileNodeData(GetNodeItemData(Node, Sender));
  FileNodeData.RunFromCategory := (Node.CheckState = csCheckedNormal);
  FileNodeData.Changed := True;
end;

procedure TfrmCatGeneralPropertyPage.vstCategoryItemsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmCatGeneralPropertyPage.vstCategoryItemsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    CellText := NodeData.Name;
end;

end.
