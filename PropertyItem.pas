unit PropertyItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, BaseEntityPage;

type
  TfrmPropertyItem = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    pnlPropertyPage: TPanel;
    vstCategory: TVirtualStringTree;
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
  private
    { Private declarations }
  strict private
    FCurrentPage: TfrmBaseEntityPage;
    FFrameGeneral, FFrameAdvanced: PVirtualNode;
  public
    { Public declarations }
    function Execute(APage:TPageFrameClass = nil):Integer;
  end;

var
  frmPropertyItem: TfrmPropertyItem;

implementation

uses
  ulNodeDataTypes, ulFrameUtils, BaseGeneralPropertyPage, AdvancedPropertyPage,
  BehaviorPropertyPage, AppConfig, CatGeneralPropertyPage, SWGeneralPropertyPage;

{$R *.dfm}

procedure TfrmPropertyItem.btnCancelClick(Sender: TObject);
begin
  Close;
end;

function TfrmPropertyItem.Execute(APage: TPageFrameClass): Integer;
var
  selNode :PVirtualNode;
begin
  if not Assigned(APage) then
    selNode := FFrameGeneral
  else
    selNode := GetNodeByFrameClass(vstCategory, APage);
  //Select node (automatically open frame using vst's AddToSelection event)
  vstCategory.Selected[selNode] := True;
  vstCategory.FullExpand;
  result := ShowModal;
end;

procedure TfrmPropertyItem.FormCreate(Sender: TObject);
begin
  vstCategory.NodeDataSize := SizeOf(rFramesNodeData);
  vstCategory.Clear;
  //General
  FFrameGeneral  := AddFrameNode(vstCategory, nil,TPageFrameClass(TfrmSWGeneralPropertyPage.Create(Self)));
  FFrameGeneral  := AddFrameNode(vstCategory, nil,TPageFrameClass(TfrmCatGeneralPropertyPage.Create(Self)));
  FFrameAdvanced := AddFrameNode(vstCategory, nil,TPageFrameClass(TfrmAdvancedPropertyPage.Create(Self)));
  AddFrameNode(vstCategory, nil,TPageFrameClass(TfrmBehaviorPropertyPage.Create(Self)));
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
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
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
