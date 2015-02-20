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

unit Forms.ScanFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, ComCtrls, DKLang,
  JvExMask, JvToolEdit, VirtualExplorerTree, MPShellUtilities, MPCommonObjects,
  EasyListview, ShellApi, Vcl.ImgList, Generics.Collections, MPCommonUtilities,
  Generics.Defaults, Scanner.Thread;

type
  TfrmScanFolder = class(TForm)
    btnScan: TButton;
    btnCancel: TButton;
    pnlFilters: TPanel;
    DKLanguageController1: TDKLanguageController;
    vstShell: TVirtualExplorerTree;
    grpFileTypes: TGroupBox;
    btnTypesDelete: TButton;
    btnTypesAdd: TButton;
    edtTypes: TEdit;
    grpExclude: TGroupBox;
    edtExclude: TEdit;
    btnExcludeAdd: TButton;
    btnExcludeDelete: TButton;
    vstTypes: TVirtualStringTree;
    vstExclude: TVirtualStringTree;
    ilExtIcons: TImageList;
    grpGeneralSettings: TGroupBox;
    chkFlat: TCheckBox;
    chkExtractName: TCheckBox;
    procedure vstShellEnumFolder(Sender: TCustomVirtualExplorerTree;
      Namespace: TNamespace; var AllowAsChild: Boolean);
    procedure vstShellInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormCreate(Sender: TObject);
    procedure vstGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure btnTypesAddClick(Sender: TObject);
    procedure btnTypesDeleteClick(Sender: TObject);
    procedure btnExcludeDeleteClick(Sender: TObject);
    procedure btnExcludeAddClick(Sender: TObject);
    procedure edtTypesChange(Sender: TObject);
    procedure edtExcludeChange(Sender: TObject);
    procedure vstTypesRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstTypesAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstExcludeAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstExcludeRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure btnScanClick(Sender: TObject);
    procedure vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FScanThread: TScanThread;
    procedure PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
    procedure PopulateVSTListView(AListView: TVirtualStringTree; AStringList: TStringList; AIsExtension: Boolean);
    function GetExtImage(AExtension: string): Integer;
    procedure AddItem(AListView: TVirtualStringTree; AText: string; AIsExtension: Boolean);

    procedure thTerminate(Sender: TObject);

    procedure DoScanThread(ATree: TVirtualStringTree; AParentNode: PVirtualNode);
  public
    { Public declarations }
    class procedure Execute(AOwner: TComponent);
  end;

var
  frmScanFolder: TfrmScanFolder;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Kernel.Types, VirtualTree.Methods, NodeDataTypes.Base;

{$R *.dfm}

procedure TfrmScanFolder.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScanFolder.btnExcludeAddClick(Sender: TObject);
begin
  if edtExclude.Text <> '' then
  begin
    AddItem(vstExclude, LowerCase(edtExclude.Text), False);
    edtExclude.Clear;
  end;
end;

procedure TfrmScanFolder.btnExcludeDeleteClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstExclude.GetFirstSelected;
  if Assigned(Node) then
  begin
    vstExclude.DeleteNode(Node);
    edtExclude.Clear;
  end;
end;

procedure TfrmScanFolder.btnScanClick(Sender: TObject);
var
  ListNode: PVirtualNode;
  ListNodeData : TvBaseNodeData;
begin
  //Add parent node named as Form's caption
  ListNode := TVirtualTreeMethods.Create.AddChildNodeEx(Config.MainTree, Config.MainTree.GetFirstSelected, amInsertAfter, vtdtCategory);
  ListNodeData := TVirtualTreeMethods.Create.GetNodeItemData(ListNode, Config.MainTree);
  ListNodeData.Name := Self.Caption;
  //Create and start Scanner thread
  DoScanThread(Config.MainTree, ListNode);
end;

procedure TfrmScanFolder.btnTypesAddClick(Sender: TObject);
var
  str: string;
begin
  if edtTypes.Text <> '' then
  begin
    str := LowerCase(edtTypes.Text);
    AddItem(vstTypes, str, True);
    edtTypes.Clear;
  end;
end;

procedure TfrmScanFolder.btnTypesDeleteClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstTypes.GetFirstSelected;
  if Assigned(Node) then
  begin
    vstTypes.DeleteNode(Node);
    edtTypes.Clear;
  end;
end;

procedure TfrmScanFolder.edtExcludeChange(Sender: TObject);
begin
  btnExcludeAdd.Enabled := edtExclude.Text <> '';
end;

procedure TfrmScanFolder.edtTypesChange(Sender: TObject);
begin
  btnTypesAdd.Enabled := edtTypes.Text <> '';
end;

class procedure TfrmScanFolder.Execute(AOwner: TComponent);
var
  frm: TfrmScanFolder;
begin
  frm := TfrmScanFolder.Create(AOwner);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmScanFolder.AddItem(AListView: TVirtualStringTree; AText: string; AIsExtension: Boolean);
var
  Node: PVirtualNode;
  NodeData: PScanFolderData;
begin
  if AIsExtension then
  begin
    //Add *., if user forget it
    if pos('*.', AText) = 0 then
      AText := '*.' + AText;
  end;
  //Add item in ListView
  Node := AListView.AddChild(nil);
  NodeData := AListView.GetNodeData(Node);
  NodeData.Text := AText;
  NodeData.ImageIndex := GetExtImage(AText);
end;

procedure TfrmScanFolder.FormCreate(Sender: TObject);
begin
  //TODO: Load checkboxes states
  PopulateVSTListView(vstTypes, Config.ScanFolderFileTypes, True);
  PopulateVSTListView(vstExclude, Config.ScanFolderExcludeNames, False);
end;

function TfrmScanFolder.GetExtImage(AExtension: string): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
  Icon: TIcon;
begin
  Result := -1;
  Flags := -1;
  Icon := TIcon.Create;
  try
    Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;
    //Get index
    if SHGetFileInfo(PChar(AExtension), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
    begin
      Icon.Handle := FileInfo.hIcon;
      Result := ilExtIcons.AddIcon(Icon);
    end;
  finally
    Icon.Free;
  end;
end;

procedure TfrmScanFolder.PopulateVSTListView(AListView: TVirtualStringTree; AStringList: TStringList; AIsExtension: Boolean);
var
  I: Integer;
begin
  AListView.BeginUpdate;
  try
    AListView.Clear;
    for I := 0 to AStringList.Count - 1 do
      AddItem(AListView, LowerCase(AStringList[I]), AIsExtension);
  finally
    AListView.EndUpdate;
  end;
end;

procedure TfrmScanFolder.DoScanThread(ATree: TVirtualStringTree;
  AParentNode: PVirtualNode);
begin
  FScanThread := TScanThread.Create(True, ATree, vstShell);
  FScanThread.ParentNode      := AParentNode;
  FScanThread.Flat            := chkFlat.Checked;
  FScanThread.AutoExtractName := chkExtractName.Checked;
  FScanThread.Start;
  FScanThread.OnTerminate := thTerminate;
end;

procedure TfrmScanFolder.thTerminate(Sender: TObject);
begin
  FScanThread.CloseAndFreeDialog;
  Close;
end;

procedure TfrmScanFolder.PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
var
  I: Integer;
  Node: PVirtualNode;
  NodeData: pScanFolderData;
begin
  AStringList.Clear;
  //From VST to StringList
  Node := AListView.GetFirst;
  while Assigned(Node) do
  begin
    NodeData := AListView.GetNodeData(Node);
    AStringList.Add(LowerCase(NodeData.Text));

    Node := AListView.GetNext(Node);
  end;
end;

procedure TfrmScanFolder.vstShellEnumFolder(
  Sender: TCustomVirtualExplorerTree; Namespace: TNamespace;
  var AllowAsChild: Boolean);
begin
  AllowAsChild := (NameSpace.FileSystem or NameSpace.IsMyComputer) and Not(Namespace.Stream);
end;

procedure TfrmScanFolder.vstShellInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
   Node.CheckType := ctTriStateCheckBox;
end;

procedure TfrmScanFolder.vstTypesAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnTypesDelete.Enabled := True;
end;

procedure TfrmScanFolder.vstFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: pScanFolderData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    NodeData.Text := '';
end;

procedure TfrmScanFolder.vstTypesRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnTypesDelete.Enabled := False;
end;

procedure TfrmScanFolder.vstExcludeAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnExcludeDelete.Enabled := True;
end;

procedure TfrmScanFolder.vstExcludeRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnExcludeDelete.Enabled := False;
end;

procedure TfrmScanFolder.vstGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: pScanFolderData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmScanFolder.vstGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: pScanFolderData;
begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
    CellText := NodeData.Text;
end;

procedure TfrmScanFolder.vstGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rScanFolderData);
end;

end.
