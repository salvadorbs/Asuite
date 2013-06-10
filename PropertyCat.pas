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

unit PropertyCat;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, GTForm,
  ulNodeDataTypes, Graphics, Vcl.CheckLst, Vcl.ComCtrls, VirtualTrees;

type

  { TfrmPropertyCat }

  TfrmPropertyCat = class(TGTForm)
    OpenDialog1: TOpenDialog;
    btnCancel: TButton;
    btnOk: TButton;
    PageControl1: TPageControl;
    tsInfo1: TTabSheet;
    lbName: TLabel;
    edtName: TEdit;
    tsInfo2: TTabSheet;
    lbPathIcon: TLabel;
    lbAutoExecute: TLabel;
    lbWindowState: TLabel;
    lbActionOnExe: TLabel;
    edtPathIcon: TEdit;
    btnBrowseIcon: TButton;
    cxAutoExecute: TComboBox;
    cxWindowState: TComboBox;
    cxActionOnExe: TComboBox;
    cbHideSoftware: TCheckBox;
    btnChangeOrder: TButton;
    lblListItems: TLabel;
    lblNote: TLabel;
    vstCategoryItems: TVirtualStringTree;
    dtpSchTime: TDateTimePicker;
    dtpSchDate: TDateTimePicker;
    cxScheduler: TComboBox;
    lbScheduler: TLabel;
    procedure btnBrowseIconClick(Sender: TObject);
    procedure edtNameEnter(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btnChangeOrderClick(Sender: TObject);
    procedure vstCategoryItemsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstCategoryItemsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure cxSchedulerChange(Sender: TObject);
  private
    { Private declarations }
    FNodeData: PBaseData;
    procedure LoadNodeData(AData: TvCategoryNodeData);
    procedure SaveNodeData(AData: TvCategoryNodeData);
    procedure GetCategoryItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure SetCategoryItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
  public
    { Public declarations }
    class function Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
  end;

var
  frmPropertyCat : TfrmPropertyCat;

implementation

uses
  appConfig, udImages, ulSysUtils, Main, ulCommonUtils, ulEnumerations,
  OrderSoftware, ulAppConfig, ulTreeView;

{$R *.dfm}

class function TfrmPropertyCat.Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
begin
  Result := mrCancel;
  if not Assigned(NodeData) then
    ShowMessage(msgErrGeneric,true)
  else
    with TfrmPropertyCat.Create(AOwner) do
      try
        FNodeData := NodeData;
        LoadNodeData(TvCategoryNodeData(NodeData.Data));
        FormStyle := frmMain.FormStyle;
        ShowModal;
        if ModalResult = mrOK then
          SaveNodeData(TvCategoryNodeData(NodeData.Data));
        Result := ModalResult;
      finally
        Free;
      end;
end;

procedure TfrmPropertyCat.btnBrowseIconClick(Sender: TObject);
begin
  OpenDialog1.Filter     := 'Files supported (*.ico;*.exe)|*.ico;*.exe|All files|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtPathIcon.Text));
  if (OpenDialog1.Execute) then
    edtPathIcon.text := AbsoluteToRelative(OpenDialog1.FileName);
  SetCurrentDir(SUITE_WORKING_PATH);
end;

procedure TfrmPropertyCat.edtNameEnter(Sender: TObject);
begin
  TEdit(Sender).Color := clWindow;
end;

procedure TfrmPropertyCat.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPropertyCat.btnChangeOrderClick(Sender: TObject);
begin
  //Autorun
  with TvCustomRealNodeData(FNodeData.Data) do
  begin
    Autorun := TAutorunType(cxAutoExecute.ItemIndex);
    try
      Application.CreateForm(TfrmOrderSoftware, frmOrderSoftware);
      frmOrderSoftware.FormStyle   := Self.FormStyle;
      frmOrderSoftware.AutorunType := Autorun;
      frmOrderSoftware.ShowModal;
    finally
      frmOrderSoftware.Free;
    end;
  end;
end;

procedure TfrmPropertyCat.btnOkClick(Sender: TObject);
begin
  CheckPropertyName(edtName);
end;

procedure TfrmPropertyCat.cxSchedulerChange(Sender: TObject);
begin
  dtpSchDate.Enabled := cxScheduler.ItemIndex = Ord(smOnce);
  dtpSchTime.Enabled := (cxScheduler.ItemIndex = Ord(smOnce)) or
                        (cxScheduler.ItemIndex = Ord(smDaily));
end;

procedure TfrmPropertyCat.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := ((ModalResult = mrOK) and (Trim(edtName.Text) <> ''))
           or (ModalResult = mrCancel);
end;

procedure TfrmPropertyCat.FormCreate(Sender: TObject);
begin
  vstCategoryItems.NodeDataSize := SizeOf(TTreeDataX);
  vstCategoryItems.Images       := ImagesDM.IcoImages;
  PageControl1.ActivePageIndex  := 0;
end;

procedure TfrmPropertyCat.GetCategoryItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  CurrentFileData : TvCustomRealNodeData;
  NewNode         : PVirtualNode;
  NewNodeData     : PTreeDataX;
begin
  CurrentFileData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  if (CurrentFileData.DataType in [vtdtFile,vtdtFolder]) and
     (Node.Parent = FNodeData.Data.pNode) then
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
    //If necessary, get imageindex
    if CurrentFileData.ImageIndex = -1 then
      CurrentFileData.ImageIndex := ImagesDM.GetIconIndex(CurrentFileData);
    //Set pointers
    NewNodeData.pNodeList := Node;
  end;
end;

procedure TfrmPropertyCat.LoadNodeData(AData: TvCategoryNodeData);
begin
  edtName.Text     := AData.name;
  edtPathIcon.Text := AData.PathIcon;
  //Get items list
  frmMain.vstList.IterateSubtree(FNodeData.Data.pNode,GetCategoryItems,nil,[],False);
  cxActionOnExe.ItemIndex := Ord(AData.ActionOnExe);
  cxAutoExecute.ItemIndex := Ord(AData.Autorun);
  btnChangeOrder.Enabled  := (cxAutoExecute.ItemIndex <> 0);
  //Scheduler
  cxScheduler.ItemIndex    := Ord(AData.SchMode);
  dtpSchDate.Date          := AData.SchDateTime;
  dtpSchTime.Time          := AData.SchDateTime;
  //Window State
  if (AData.WindowState <> -1) and Not(AData.WindowState >= 4) then
    cxWindowState.ItemIndex := AData.WindowState
  else
    cxWindowState.ItemIndex := 0;
  cbHideSoftware.Checked  := AData.HideFromMenu;
  cxSchedulerChange(Self);
end;

procedure TfrmPropertyCat.SaveNodeData(AData: TvCategoryNodeData);
begin
  AData.Name := StringReplace(edtName.Text, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
  AData.Name := StringReplace(AData.Name, '&', '&&', [rfIgnoreCase,rfReplaceAll]);
  vstCategoryItems.IterateSubtree(nil,SetCategoryItems,nil,[],False);
  //If changed, refresh cache icon
  if AData.PathIcon <> edtPathIcon.Text then
  begin
    AData.PathIcon   := edtPathIcon.Text;
    AData.CacheID    := -1;
    AData.CacheLargeID := -1;
    AData.ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(AData));
  end;
  AData.ActionOnExe  := TActionOnExecute(cxActionOnExe.ItemIndex);
  AData.Autorun      := TAutorunType(cxAutoExecute.ItemIndex);
  //Scheduler
  AData.SchMode      := TSchedulerMode(cxScheduler.ItemIndex);
  AData.SchDateTime  := Int(dtpSchDate.Date) + Frac(dtpSchTime.Time);
  AData.WindowState  := cxWindowState.ItemIndex;
  AData.HideFromMenu := cbHideSoftware.Checked;
  AData.Changed := True;
end;

procedure TfrmPropertyCat.SetCategoryItems(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  FileNodeData : TvFileNodeData;
begin
  FileNodeData := TvFileNodeData(GetNodeDataSearch(Node,vstCategoryItems,frmMain.vstList).Data);
  FileNodeData.RunFromCategory := (Node.CheckState = csCheckedNormal);
  FileNodeData.Changed := True;
end;

procedure TfrmPropertyCat.vstCategoryItemsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : TvBaseNodeData;
begin
  NodeData   := GetNodeDataSearch(Node,vstCategoryItems,frmMain.vstList).Data;
  ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmPropertyCat.vstCategoryItemsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeDataSearch(Node,vstCategoryItems,frmMain.vstList).Data;
  CellText := NodeData.Name;
end;

end.
