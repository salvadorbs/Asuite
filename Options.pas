unit Options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, BaseOptionsPage, VirtualTrees;

type
  TfrmOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    pnlOptionsPage: TPanel;
    vstListCategory: TVirtualStringTree;
    OpenDialog1: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstListCategoryGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstListCategoryFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstListCategoryNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure btnCancelClick(Sender: TObject);
    procedure vstListCategoryInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    { Private declarations }
    function AddFrameNode(Parent: PVirtualNode; FramePage: TPageFrameClass): PVirtualNode;
  strict private
    FCurrentPage: TfrmBaseOptionsPage;
  public
    { Public declarations }
    procedure LoadPage(Page: TPageFrameClass);
    procedure LoadPageByNode(Tree: TBaseVirtualTree;Node: PVirtualNode);
  end;

  rOptionsNodeData = record
    Title : string;
    Frame : TPageFrameClass;
  end;
  POptionsNodeData = ^rOptionsNodeData;

var
  frmOptions: TfrmOptions;

implementation

{$R *.dfm}

uses
  GeneralOptionsPage, AdvancedOptionsPage, TrayIconOptionsPage, SensorsOptionsPage,
  StatsOptionsPage, HotkeyOptionsPage, ItemsOptionsPage;

{ TfrmOptions }

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  vstListCategory.NodeDataSize := SizeOf(rOptionsNodeData);
end;

procedure TfrmOptions.FormShow(Sender: TObject);
var
  FrameGeneral, FrameAdvanced: PVirtualNode;
begin
  vstListCategory.Clear;
  //General
  FrameGeneral  := AddFrameNode(nil,TPageFrameClass(TfrmGeneralOptionsPage.Create(Self)));
  //Advanced
  FrameAdvanced := AddFrameNode(nil,TPageFrameClass(TfrmAdvancedOptionsPage.Create(Self)));
  AddFrameNode(FrameAdvanced,TPageFrameClass(TfrmItemsOptionsPage.Create(Self)));
  AddFrameNode(FrameAdvanced,TPageFrameClass(TfrmHotkeyOptionsPage.Create(Self)));
  AddFrameNode(FrameAdvanced,TPageFrameClass(TfrmSensorsOptionsPage.Create(Self)));
  //TrayIcon
  AddFrameNode(nil,TPageFrameClass(TfrmTrayiconOptionsPage.Create(Self)));
  //Stats
  AddFrameNode(nil,TPageFrameClass(TfrmStatsOptionsPage.Create(Self)));

  LoadPageByNode(vstListCategory, FrameGeneral);
  vstListCategory.FullExpand;
end;

procedure TfrmOptions.LoadPage(Page: TPageFrameClass);
begin
  if Page <> nil then
  begin
    if FCurrentPage <> nil then
    begin
      if FCurrentPage.ClassType = Page then
        Exit
      else
       FCurrentPage.Visible := False;
    end;
    FCurrentPage := TfrmBaseOptionsPage(Page);
    FCurrentPage.Parent  := pnlOptionsPage;
    FCurrentPage.Align   := alClient;
    FCurrentPage.Visible := True;
  end;
end;

procedure TfrmOptions.LoadPageByNode(Tree: TBaseVirtualTree;Node: PVirtualNode);
var
  NodeData : POptionsNodeData;
begin
  NodeData := Tree.GetNodeData(Node);
  if Assigned(NodeData) then
    LoadPage(NodeData.Frame);
end;

function TfrmOptions.AddFrameNode(Parent: PVirtualNode; FramePage: TPageFrameClass): PVirtualNode;
var
  NodeData: POptionsNodeData;
begin
  Result   := vstListCategory.AddChild(Parent);
  NodeData := vstListCategory.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    NodeData.Frame := FramePage;
    NodeData.Title := TfrmBaseOptionsPage(FramePage).Title;
  end;
end;

procedure TfrmOptions.vstListCategoryFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData : POptionsNodeData;
begin
  NodeData := vstListCategory.GetNodeData(Node);
  if Assigned(NodeData) then
  begin
    TfrmBaseOptionsPage(NodeData.Frame).Free;
    NodeData.Title := '';
  end;
end;

procedure TfrmOptions.vstListCategoryGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData : POptionsNodeData;
begin
  NodeData := vstListCategory.GetNodeData(Node);
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

procedure TfrmOptions.vstListCategoryNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  if hiOnItemLabel in HitInfo.HitPositions then
    LoadPageByNode(vstListCategory, HitInfo.HitNode);
end;

end.
