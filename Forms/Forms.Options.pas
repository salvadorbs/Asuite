unit Forms.Options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, BaseEntityPage, VirtualTrees, DKLang;

type
  TfrmOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    pnlOptionsPage: TPanel;
    vstListCategory: TVirtualStringTree;
    DKLanguageController1: TDKLanguageController;
    procedure FormCreate(Sender: TObject);
    procedure vstListCategoryGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstListCategoryFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure btnCancelClick(Sender: TObject);
    procedure vstListCategoryInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure btnOkClick(Sender: TObject);
    procedure vstListCategoryGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstListCategoryAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    { Private declarations }
    procedure SaveOptions(Sender: TBaseVirtualTree; Node: PVirtualNode;
                          Data: Pointer; var Abort: Boolean);
  strict private
    FCurrentPage: TfrmBaseEntityPage;
    FFrameGeneral, FFrameAdvanced: PVirtualNode;
  public
    { Public declarations }
    function Execute(APage:TPageFrameClass = nil):Integer;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  GeneralOptionsPage, AdvancedOptionsPage, TrayIconOptionsPage,
  StatsOptionsPage, HotkeyOptionsPage, ItemsOptionsPage, ulAppConfig, Main, AppConfig,
  ulNodeDataTypes, ulFrameUtils;

{$R *.dfm}

{ TfrmOptions }

procedure TfrmOptions.SaveOptions(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  NodeData : PFramesNodeData;
begin
  //Call Frame's function SaveData
  NodeData := vstListCategory.GetNodeData(Node);
  TfrmBaseEntityPage(NodeData.Frame).SaveData;
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmOptions.btnOkClick(Sender: TObject);
begin
  vstListCategory.IterateSubtree(nil, SaveOptions, nil);
  Config.Changed := True;
  if frmMain.Visible then
    frmMain.FocusControl(frmMain.vstList);
  LangManager.LanguageID := Config.LangID;
  ModalResult := mrOk;
end;

function TfrmOptions.Execute(APage: TPageFrameClass): Integer;
var
  selNode :PVirtualNode;
begin
  if not Assigned(APage) then
    selNode := FFrameGeneral
  else
    selNode := GetNodeByFrameClass(vstListCategory, APage);
  //Select node (automatically open frame using vst's AddToSelection event)
  vstListCategory.FocusedNode := selNode;
  vstListCategory.Selected[selNode] := True;
  vstListCategory.FullExpand;
  result := ShowModal;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  vstListCategory.NodeDataSize := SizeOf(rFramesNodeData);
  vstListCategory.Clear;
  //General
  FFrameGeneral  := AddFrameNode(vstListCategory, nil,TPageFrameClass(TfrmGeneralOptionsPage.Create(Self)));
  //Advanced
  FFrameAdvanced := AddFrameNode(vstListCategory, nil,TPageFrameClass(TfrmAdvancedOptionsPage.Create(Self)));
  AddFrameNode(vstListCategory, FFrameAdvanced,TPageFrameClass(TfrmItemsOptionsPage.Create(Self)));
  AddFrameNode(vstListCategory, FFrameAdvanced,TPageFrameClass(TfrmHotkeyOptionsPage.Create(Self)));
  //TrayIcon
  AddFrameNode(vstListCategory, nil,TPageFrameClass(TfrmTrayiconOptionsPage.Create(Self)));
  //Stats
  AddFrameNode(vstListCategory, nil,TPageFrameClass(TfrmStatsOptionsPage.Create(Self)));
end;

procedure TfrmOptions.vstListCategoryAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    LoadPage(FCurrentPage, NodeData.Frame, pnlOptionsPage);
end;

procedure TfrmOptions.vstListCategoryFreeNode(Sender: TBaseVirtualTree;
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

procedure TfrmOptions.vstListCategoryGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : PFramesNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmOptions.vstListCategoryGetText(Sender: TBaseVirtualTree;
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

procedure TfrmOptions.vstListCategoryInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.NodeHeight := 36;
end;

end.
