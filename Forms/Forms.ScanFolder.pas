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
  VirtualExplorerTree, MPShellUtilities,
  ShellApi, Vcl.ImgList, MPCommonUtilities,
  Scanner.Thread, Scanner.Folder;

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
    FScannerFolder: TScannerFolder;
    procedure PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
    procedure PopulateVSTListView(AListView: TVirtualStringTree; AStringList: TStringList; AIsExtension: Boolean);
    function GetExtImage(AExtension: string): Integer;
    procedure AddItem(AListView: TVirtualStringTree; AText: string; AIsExtension: Boolean);

    procedure thTerminate(Sender: TObject);

    procedure LoadSettings;
    procedure SaveSettings;
    procedure DoScanThread(ATree: TVirtualStringTree; AParentNode: PVirtualNode; AFolder: TScannerFolder);
    function GetAllCheckedFolders(ASender: TVirtualExplorerTree): TScannerFolder;
  public
    { Public declarations }
    class procedure Execute(AOwner: TComponent);
  end;

var
  frmScanFolder: TfrmScanFolder;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Kernel.Types, VirtualTree.Methods,
  NodeDataTypes.Base, Utility.Misc;

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
  //Check if user add at least one file extension
  if vstTypes.HasChildren[vstTypes.RootNode] then
  begin
    SaveSettings;
    //Get checked folders from vstShell
    FScannerFolder := GetAllCheckedFolders(vstShell);
    if FScannerFolder.Count > 0 then
    begin
      //Add parent node named as Form's caption
      ListNode := TVirtualTreeMethods.Create.AddChildNodeEx(Config.MainTree, nil, amInsertAfter, vtdtCategory);
      ListNodeData := TVirtualTreeMethods.Create.GetNodeItemData(ListNode, Config.MainTree);
      ListNodeData.Name := Self.Caption;
      //Create and start Scanner thread
      DoScanThread(Config.MainTree, ListNode, FScannerFolder);
    end
    else
      ShowMessageEx(DKLangConstW('msgErrScanFolderEmptyPath'), True);
  end
  else
    ShowMessageEx(DKLangConstW('msgErrScanFolderMissingTypes'), True);
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
    //Add ., if user forget it
    if AText[1] <> '.' then
      AText := '.' + AText;
  end;
  //Add item in ListView
  Node := AListView.AddChild(nil);
  NodeData := AListView.GetNodeData(Node);
  NodeData.Text := AText;
  NodeData.ImageIndex := GetExtImage(AText);
end;

procedure TfrmScanFolder.FormCreate(Sender: TObject);
begin
  LoadSettings;
end;

function TfrmScanFolder.GetAllCheckedFolders(
  ASender: TVirtualExplorerTree): TScannerFolder;

  procedure RecurseStorage(S: TNodeStorage; CheckedFolderList: TScannerFolder);
  var
    NS: TNamespace;
    i: integer;
    Str: string;
    CheckType: TCheckType;
  begin
    NS := TNamespace.Create(S.AbsolutePIDL, nil);
    NS.FreePIDLOnDestroy := False;
    // Need to do this to get the real path to special folders
    Str := NS.NameForParsing;
    { The items must - be in the file system, a valid file or directory, have a }
    { full check     }
    if NS.FileSystem and (FileExistsW(Str) or WideDirectoryExists(Str) or WideIsDrive(Str)) then
    begin
      //Get proper check
      case S.Storage.Check.CheckState of
        csUncheckedNormal,
        csUncheckedPressed: CheckType := fctUnchecked;
        csCheckedNormal,
        csCheckedPressed: CheckType := fctChecked;
        csMixedNormal,
        csMixedPressed: CheckType := fctMixed;
      end;
      //Add in FolderList
      if CheckType <> fctUnchecked then
        CheckedFolderList := CheckedFolderList.AddItem(Str, CheckType)
    end;

    if Assigned(S.ChildNodeList) and (CheckType <> fctChecked) then
      for i := 0 to S.ChildNodeList.Count - 1 do
        RecurseStorage(S.ChildNodeList[i], CheckedFolderList);

    NS.Free;
  end;

begin
  Result := TScannerFolder.Create('', fctUnChecked);
  //Add mixed checked folders
  RecurseStorage(ASender.Storage, Result);
end;

function TfrmScanFolder.GetExtImage(AExtension: string): Integer;
var
  FileInfo: TSHFileInfo;
  Icon: TIcon;
begin
  Result := -1;
  Icon := TIcon.Create;
  try
    //Get index
    if SHGetFileInfo(PChar('*' + AExtension), 0, FileInfo, SizeOf(TSHFileInfo), SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES) <> 0 then
    begin
      Icon.Handle := FileInfo.hIcon;
      Result := ilExtIcons.AddIcon(Icon);
    end;
  finally
    Icon.Free;
  end;
end;

procedure TfrmScanFolder.LoadSettings;
begin
  chkFlat.Checked := Config.ScanFolderFlatStructure;
  chkExtractName.Checked := Config.ScanFolderAutoExtractName;
  PopulateVSTListView(vstTypes, Config.ScanFolderFileTypes, True);
  PopulateVSTListView(vstExclude, Config.ScanFolderExcludeNames, False);
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

procedure TfrmScanFolder.SaveSettings;
begin
  Config.ScanFolderFlatStructure   := chkFlat.Checked;
  Config.ScanFolderAutoExtractName := chkExtractName.Checked;
  PopulateStringList(vstTypes, Config.ScanFolderFileTypes);
  PopulateStringList(vstExclude, Config.ScanFolderExcludeNames);
  Config.Changed := True;
end;

procedure TfrmScanFolder.DoScanThread(ATree: TVirtualStringTree;
  AParentNode: PVirtualNode; AFolder: TScannerFolder);
begin
  FScanThread := TScanThread.Create(True, ATree, AFolder);
  FScanThread.ParentNode      := AParentNode;
  FScanThread.Flat            := chkFlat.Checked;
  FScanThread.AutoExtractName := chkExtractName.Checked;
  FScanThread.Start;
  FScanThread.OnTerminate := thTerminate;
end;

procedure TfrmScanFolder.thTerminate(Sender: TObject);
begin
  FreeAndNil(FScannerFolder);
  //Select and expanded ScanFolder node
  Config.MainTree.ClearSelection;
  Config.MainTree.Selected[FScanThread.ParentNode] := True;
  Config.MainTree.Expanded[FScanThread.ParentNode] := True;
  Config.MainTree.FocusedNode := FScanThread.ParentNode;
  //Close Dialog and frmScanFolder
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
