unit CatGeneralPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseGeneralPropertyPage, Vcl.StdCtrls,
  VirtualTrees;

type
  TfrmCatGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    grpSubItems: TGroupBox;
    vstCategoryItems: TVirtualStringTree;
    lblNote: TLabel;
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
  AppConfig, PropertyItem, ulNodeDataTypes, udImages, ulEnumerations, Main,
  ulTreeView;

{$R *.dfm}

{ TfrmCatGeneralPropertyPage }

procedure TfrmCatGeneralPropertyPage.GetCategoryItems(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  CurrentFileData : TvCustomRealNodeData;
  NewNode         : PVirtualNode;
  NewNodeData     : PTreeDataX;
begin
  CurrentFileData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
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
    //Get imageindex
    CurrentFileData.ImageIndex := ImagesDM.GetIconIndex(CurrentFileData);
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
  vstCategoryItems.Images       := ImagesDM.IcoImages;
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
  FileNodeData := TvFileNodeData(GetNodeDataSearch(Node,vstCategoryItems,frmMain.vstList).Data);
  FileNodeData.RunFromCategory := (Node.CheckState = csCheckedNormal);
  FileNodeData.Changed := True;
end;

procedure TfrmCatGeneralPropertyPage.vstCategoryItemsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : TvBaseNodeData;
begin
  NodeData   := GetNodeDataSearch(Node,vstCategoryItems,frmMain.vstList).Data;
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmCatGeneralPropertyPage.vstCategoryItemsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeDataSearch(Node,vstCategoryItems,frmMain.vstList).Data;
  if Assigned(NodeData) then
    CellText := NodeData.Name;
end;

end.
