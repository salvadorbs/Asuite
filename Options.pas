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
    function GetNodeByFrameClass(AFramePage: TPageFrameClass):PVirtualNode;
    function AddFrameNode(Parent: PVirtualNode; FramePage: TPageFrameClass;
                          ImageIndex: Integer): PVirtualNode;
    procedure SaveOptions(Sender: TBaseVirtualTree; Node: PVirtualNode;
                          Data: Pointer; var Abort: Boolean);
  strict private
    FCurrentPage: TfrmBaseOptionsPage;
    FFrameGeneral, FFrameAdvanced: PVirtualNode;
  public
    { Public declarations }
    function Execute(APage:TPageFrameClass = nil):Integer;
    procedure LoadPage(Page: TPageFrameClass);
    procedure LoadPageByNode(Tree: TBaseVirtualTree;Node: PVirtualNode);
  end;

  rOptionsNodeData = record
    Title : string;
    Frame : TPageFrameClass;
    ImageIndex: Integer;
  end;
  POptionsNodeData = ^rOptionsNodeData;

var
  frmOptions: TfrmOptions;

implementation

uses
  GeneralOptionsPage, AdvancedOptionsPage, TrayIconOptionsPage, SensorsOptionsPage,
  StatsOptionsPage, HotkeyOptionsPage, ItemsOptionsPage, ulAppConfig, Main, AppConfig;

{$R *.dfm}

{ TfrmOptions }

procedure TfrmOptions.SaveOptions(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);

var
  NodeData : POptionsNodeData;
begin
  //Call Frame's function SaveData
  NodeData := vstListCategory.GetNodeData(Node);
  TfrmBaseOptionsPage(NodeData.Frame).SaveData;
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.btnOkClick(Sender: TObject);
begin
  vstListCategory.IterateSubtree(nil, SaveOptions, nil, [], True);
  Config.Changed := True;
  if frmMain.Visible then
    frmMain.FocusControl(frmMain.vstList);
  Close;
end;

function TfrmOptions.Execute(APage: TPageFrameClass): Integer;
var
  selNode :PVirtualNode;
begin
  if not Assigned(APage) then
    selNode := FFrameGeneral
  else
    selNode := GetNodeByFrameClass(APage);
  LoadPageByNode(vstListCategory, selNode);
  vstListCategory.FullExpand;
  result := ShowModal;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  vstListCategory.NodeDataSize := SizeOf(rOptionsNodeData);
  vstListCategory.Clear;
  //General
  FFrameGeneral  := AddFrameNode(nil,TPageFrameClass(TfrmGeneralOptionsPage.Create(Self)),IMAGELARGE_INDEX_General);
  //Advanced
  FFrameAdvanced := AddFrameNode(nil,TPageFrameClass(TfrmAdvancedOptionsPage.Create(Self)),IMAGELARGE_INDEX_Advanced);
  AddFrameNode(FFrameAdvanced,TPageFrameClass(TfrmItemsOptionsPage.Create(Self)),IMAGELARGE_INDEX_Items);
  AddFrameNode(FFrameAdvanced,TPageFrameClass(TfrmHotkeyOptionsPage.Create(Self)),IMAGELARGE_INDEX_Hotkey);
  AddFrameNode(FFrameAdvanced,TPageFrameClass(TfrmSensorsOptionsPage.Create(Self)),IMAGELARGE_INDEX_Mouse);
  //TrayIcon
  AddFrameNode(nil,TPageFrameClass(TfrmTrayiconOptionsPage.Create(Self)),IMAGELARGE_INDEX_Trayicon);
  //Stats
  AddFrameNode(nil,TPageFrameClass(TfrmStatsOptionsPage.Create(Self)),IMAGELARGE_INDEX_Stats);
end;

function TfrmOptions.GetNodeByFrameClass(
  AFramePage: TPageFrameClass): PVirtualNode;
var
  node:PVirtualNode;
  nodeData:POptionsNodeData;
begin
  Result := nil;
  node := vstListCategory.GetFirst();
  while Assigned(node) do begin
    nodeData := vstListCategory.GetNodeData(node);
    if TfrmBaseOptionsPage(nodeData.Frame).ClassName = AFramePage.ClassName then begin
      Exit(node);
    end;
    node := vstListCategory.GetNextSibling(node);
  end;

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
  if Assigned(NodeData) then begin
    Tree.Selected[Node] := True;
    LoadPage(NodeData.Frame);
  end;
end;

function TfrmOptions.AddFrameNode(Parent: PVirtualNode; FramePage: TPageFrameClass; ImageIndex: Integer): PVirtualNode;
var
  NodeData: POptionsNodeData;
begin
  Result   := vstListCategory.AddChild(Parent);
  NodeData := vstListCategory.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    NodeData.Frame := FramePage;
    NodeData.Title := TfrmBaseOptionsPage(FramePage).Title;
    NodeData.ImageIndex := ImageIndex;
  end;
end;

procedure TfrmOptions.vstListCategoryAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  LoadPageByNode(vstListCategory, Node);
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

procedure TfrmOptions.vstListCategoryGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : POptionsNodeData;
begin
  NodeData := vstListCategory.GetNodeData(Node);
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
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

end.
