unit PropertyItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, BaseEntityPage,
  ulNodeDataTypes, DKLang;

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
    function Execute(ANodeData:TvCustomRealNodeData):Integer;
  end;

var
  frmPropertyItem: TfrmPropertyItem;

implementation

uses
  ulFrameUtils, BaseGeneralPropertyPage, AdvancedPropertyPage, ulEnumerations,
  BehaviorPropertyPage, AppConfig, CatGeneralPropertyPage, SWGeneralPropertyPage,
  Main, udImages;

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
    //If changed, refresh cache icon
    FListNodeData.ResetIcon;
    FListNodeData.ImageIndex := ImagesDM.GetIconIndex(FListNodeData);
    if frmMain.Visible then
      frmMain.FocusControl(frmMain.vstList);
    ModalResult := mrOk;
  end;
end;

function TfrmPropertyItem.Execute(ANodeData: TvCustomRealNodeData): Integer;
begin
  FListNodeData := ANodeData;
  //General
  if (ANodeData.DataType = vtdtFile) or (ANodeData.DataType = vtdtFolder) then
    FFrameGeneral := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmSWGeneralPropertyPage.Create(Self, ANodeData)))
  else
    if ANodeData.DataType = vtdtCategory then
      FFrameGeneral := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmCatGeneralPropertyPage.Create(Self, ANodeData)));
  FFrameAdvanced := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmAdvancedPropertyPage.Create(Self, ANodeData)));
  AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmBehaviorPropertyPage.Create(Self, ANodeData)));
  //Select node (automatically open frame using vst's AddToSelection event)
  vstCategory.Selected[FFrameGeneral] := True;
  vstCategory.FullExpand;
  Result := ShowModal;
end;

procedure TfrmPropertyItem.FormCreate(Sender: TObject);
begin
  vstCategory.NodeDataSize := SizeOf(rFramesNodeData);
  vstCategory.Clear;
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
