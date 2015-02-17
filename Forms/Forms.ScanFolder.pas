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
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, ComCtrls, DKLang, Vcl.Mask,
  JvExMask, JvToolEdit, VirtualExplorerTree, MPShellUtilities, MPCommonObjects,
  EasyListview, ShellApi, Vcl.ImgList;

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
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
  private
    { Private declarations }
    procedure BuildTree;
    procedure PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
    procedure PopulateListView(AListView: TVirtualStringTree; AStringList: TStringList);
    function GetExtImage(AExtension: string): Integer;
  public
    { Public declarations }
    class procedure Execute(AOwner: TComponent);
  end;

var
  frmScanFolder: TfrmScanFolder;

implementation

uses
  Utility.Misc, NodeDataTypes.Custom, AppConfig.Main, Kernel.Enumerations, Kernel.Types,
  VirtualTree.Methods, Utility.System, NodeDataTypes.Files, NodeDataTypes.Base;

{$R *.dfm}

procedure TfrmScanFolder.BuildTree;
var
  MyComputer: PVirtualNode;
begin
  vstShell.BeginUpdate;
  try
    vstShell.Active := False;
    vstShell.Active := True;
    MyComputer := vstShell.FindNodeByPIDL(DrivesFolder.AbsolutePIDL);
    if MyComputer <> nil then
      vstShell.Expanded[MyComputer] := True;
  finally
    vstShell.EndUpdate;
  end;
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

procedure TfrmScanFolder.FormCreate(Sender: TObject);
begin
  //TODO: Load checkboxes states
  PopulateListView(vstTypes, Config.ScanFolderFileTypes);
  PopulateListView(vstExclude, Config.ScanFolderExcludeNames);
end;

procedure TfrmScanFolder.FormShow(Sender: TObject);
begin
  BuildTree;
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
    Flags := SHGFI_ICON or SHGFI_SMALLICON or
             SHGFI_USEFILEATTRIBUTES;
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

procedure TfrmScanFolder.PopulateListView(AListView: TVirtualStringTree; AStringList: TStringList);
var
  I: Integer;
  Node: PVirtualNode;
  NodeData: pScanFolderData;
begin
  AListView.BeginUpdate;
  try
    AListView.Clear;
    for I := 0 to AStringList.Count - 1 do
    begin
      Node := AListView.AddChild(nil);
      NodeData := AListView.GetNodeData(Node);
      NodeData.Text       := AStringList[I];
      NodeData.ImageIndex := GetExtImage(AStringList[I]);
    end;
  finally
    AListView.EndUpdate();
  end;
end;

procedure TfrmScanFolder.PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
var
  I: Integer;
begin
//  AStringList.Clear;
//  for I := 0 to AListView.Count - 1 do
//    AStringList.Add(LowerCase(AListView.Items[I].Caption));
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
