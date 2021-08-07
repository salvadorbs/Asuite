{
Copyright (C) 2006-2021 Matteo Salvi

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

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, VirtualTrees, ComCtrls, DefaultTranslator, ShellCtrls,
  ImgList, ButtonPanel, FileUtil, Thread.FindFiles, SynLog, StrUtils,
  LazStringUtils;

type

  { TfrmScanFolder }

  TfrmScanFolder = class(TForm)
    btnExcludeAdd: TButton;
    btnExcludeDelete: TButton;
    btnTypesAdd: TButton;
    btnTypesDelete: TButton;
    pnlButtons: TButtonPanel;
    chkExtractName: TCheckBox;
    edtExclude: TEdit;
    edtTypes: TEdit;
    grpExclude: TGroupBox;
    grpFileTypes: TGroupBox;
    grpGeneralSettings: TGroupBox;
    pnlGroups: TPanel;
    pnlClient: TPanel;
    pbScan: TProgressBar;
    pnlFilters: TPanel;
    vstExclude: TVirtualStringTree;
    vstShell: TShellTreeView;
    vstTypes: TVirtualStringTree;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure vstGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: AnsiString);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure btnTypesAddClick(Sender: TObject);
    procedure btnTypesDeleteClick(Sender: TObject);
    procedure btnExcludeDeleteClick(Sender: TObject);
    procedure btnExcludeAddClick(Sender: TObject);
    procedure edtTypesChange(Sender: TObject);
    procedure edtExcludeChange(Sender: TObject);
    procedure vstShellGetImageIndex(Sender: TObject; Node: TTreeNode);
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
    FListNode: PVirtualNode;
    FThreadFindFiles: TFindFiles;

    procedure FileFound(AFilePath: String);
    procedure PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
    procedure PopulateVSTListView(AListView: TVirtualStringTree; AStringList: TStringList; AIsExtension: Boolean);
    procedure AddItem(AListView: TVirtualStringTree; AText: string);

    procedure LoadSettings;
    procedure SaveSettings;
    procedure SearchEnd(ATotalFiles: Integer; ATime: Cardinal);
    procedure SetupThreadFinder;
    procedure ThreadTerminate(Sender: TObject);
    function FindNodeByText(AListView: TVirtualStringTree; AText: String): PVirtualNode;
  public
    { Public declarations }
    class procedure Execute(AOwner: TComponent);
  end;

var
  frmScanFolder: TfrmScanFolder;

implementation

uses
  AppConfig.Main, Kernel.Types, Kernel.Logger, Kernel.Consts,
  DataModules.Icons, NodeDataTypes.Base, VirtualTree.Methods, Kernel.Enumerations,
  Utility.FileFolder, NodeDataTypes.Files, Utility.Misc, Kernel.ResourceStrings,
  RegExpr, Kernel.Instance, Kernel.Manager;

{$R *.lfm}

procedure TfrmScanFolder.btnCancelClick(Sender: TObject);
begin
  if Assigned(FThreadFindFiles) then
  begin
    if AskUserWarningMessage(msgCancelScanFolder, []) then
    begin
      FThreadFindFiles.Stop;
      pnlButtons.CancelButton.Caption := msgCancel;
    end;
  end
  else
    Close;
end;

procedure TfrmScanFolder.btnExcludeAddClick(Sender: TObject);
begin
  if edtExclude.Text <> '' then
  begin
    AddItem(vstExclude, LowerCase(edtExclude.Text));
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
  ListNodeData : TvBaseNodeData;
begin
  SaveSettings;

  if vstTypes.HasChildren[vstTypes.RootNode] then
  begin
    if Assigned(vstShell.Selected) then
    begin
      pbScan.Style := pbstMarquee;
      pbScan.Position := 0;

      pnlButtons.OKButton.Enabled := False;
      pnlButtons.CancelButton.Caption := msgStop;

      FThreadFindFiles := TFindFiles.Create;
      SetupThreadFinder;

      ASuiteInstance.MainTree.BeginUpdate;

      //Add parent node named as Form's caption
      FListNode := TVirtualTreeMethods.AddChildNodeEx(ASuiteInstance.MainTree, nil, amInsertAfter, vtdtCategory);
      ListNodeData := TVirtualTreeMethods.GetNodeItemData(FListNode, ASuiteInstance.MainTree);
      ListNodeData.Name := Self.Caption + ' - ' + vstShell.Selected.GetTextPath;

      FThreadFindFiles.Start;
    end
    else begin
      ShowMessageEx(msgErrScanFolderEmptyPath, True);
    end;
  end
  else
    ShowMessageEx(msgErrScanFolderMissingTypes, True);
end;

procedure TfrmScanFolder.btnTypesAddClick(Sender: TObject);
var
  str: string;
  RegexObj: TRegExpr;
begin
  RegexObj := TRegExpr.Create('^[\*]?[\.]?([a-zA-Z0-9.]+)$');
  try
    str := LowerCase(edtTypes.Text);

    if RegexObj.Exec(str) then
    begin
      AddItem(vstTypes, EXT_PATH_MASK + EXT_PATH_DOT + RegexObj.Match[1]);
      edtTypes.Clear;
    end
    else
      ShowMessageEx(msgScanFolderExtNotValid, True);
  finally
    RegexObj.Free;
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

procedure TfrmScanFolder.vstShellGetImageIndex(Sender: TObject; Node: TTreeNode
  );
begin
  if Node.Level = 0 then
    Node.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('disk')
  else
    Node.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('folder');

  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TfrmScanFolder.edtTypesChange(Sender: TObject);
begin
  btnTypesAdd.Enabled := edtTypes.Text <> '';
end;

class procedure TfrmScanFolder.Execute(AOwner: TComponent);
var
  frm: TfrmScanFolder;
begin
  TASuiteLogger.Info('Opening form ScanFolder', []);

  frm := TfrmScanFolder.Create(AOwner);
  try
    SetFormPositionFromConfig(frm);

    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmScanFolder.AddItem(AListView: TVirtualStringTree; AText: string);
var
  Node: PVirtualNode;
  NodeData: PScanFolderData;
begin
  //Exit if found a node with same text
  if Assigned(FindNodeByText(AListView, AText)) then
    Exit;
                    
  //Add item in ListView
  Node := AListView.AddChild(nil);
  NodeData := AListView.GetNodeData(Node);
  NodeData.Text := AText;
  NodeData.ImageIndex := ASuiteManager.IconsManager.GetExtIconIndex(AText);
end;

procedure TfrmScanFolder.FormCreate(Sender: TObject);
begin
  FThreadFindFiles := nil;

  LoadSettings;

  vstShell.Images := dmImages.ilLargeIcons;
  vstShell.ImagesWidth := ICON_SIZE_SMALL;

  vstTypes.Images := dmImages.ilLargeIcons;
  vstTypes.ImagesWidth := ICON_SIZE_SMALL;
end;

procedure TfrmScanFolder.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_RETURN then
    btnScanClick(Sender)
  else
    if Ord(Key) = VK_ESCAPE then
      btnCancelClick(Sender);
end;

procedure TfrmScanFolder.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Assigned(FThreadFindFiles);
end;

procedure TfrmScanFolder.LoadSettings;
begin
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
      AddItem(AListView, LowerCase(AStringList[I]));
  finally
    AListView.EndUpdate;
  end;
end;

procedure TfrmScanFolder.SaveSettings;
begin
  TASuiteLogger.Info('Save ScanFolder settings in TConfig', []);

  Config.ScanFolderAutoExtractName := chkExtractName.Checked;
  PopulateStringList(vstTypes, Config.ScanFolderFileTypes);
  PopulateStringList(vstExclude, Config.ScanFolderExcludeNames);
  Config.Changed := True;
end;

procedure TfrmScanFolder.SearchEnd(ATotalFiles: Integer; ATime: Cardinal);
begin
  TASuiteLogger.Info('Added new %d nodes! Elapsed Search Time = %d ms', [ATotalFiles, ATime]);

  pbScan.Style := pbstNormal;
  pbScan.Position := pbScan.Max;
  ShowMessageFmtEx(msgFoundNumFiles, [ATotalFiles]);

  //Select and expanded ScanFolder node
  ASuiteInstance.MainTree.ClearSelection;
  if Assigned(FListNode) then
  begin
    ASuiteInstance.MainTree.Selected[FListNode] := True;
    ASuiteInstance.MainTree.FocusedNode := FListNode;
  end;

  pnlButtons.OKButton.Enabled := True;
  pnlButtons.CancelButton.Caption := msgCancel;
  ASuiteInstance.MainTree.EndUpdate;

  Close;
end;

procedure TfrmScanFolder.SetupThreadFinder;
begin
  FThreadFindFiles.Directory := vstShell.Selected.GetTextPath;
  FThreadFindFiles.SearchCriteriaFilename.Assign(Config.ScanFolderFileTypes);
  FThreadFindFiles.SearchExcludeFilename.Assign(Config.ScanFolderExcludeNames);
  FThreadFindFiles.OnFileFound := FileFound;
  FThreadFindFiles.OnSearchEnd := SearchEnd;
  FThreadFindFiles.OnTerminate := ThreadTerminate;
end;

procedure TfrmScanFolder.ThreadTerminate(Sender: TObject);
begin
  FThreadFindFiles := nil;
end;

function TfrmScanFolder.FindNodeByText(AListView: TVirtualStringTree;
  AText: String): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: pScanFolderData;
begin
  Result := nil;

  Node := AListView.GetFirst;
  while Assigned(Node) and not Assigned(Result) do
  begin
    NodeData := AListView.GetNodeData(Node);
    if LowerCase(NodeData.Text) = LowerCase(AText) then
      Result := Node;

    Node := AListView.GetNext(Node);
  end;
end;

procedure TfrmScanFolder.PopulateStringList(AListView: TVirtualStringTree; AStringList: TStringList);
var
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

procedure TfrmScanFolder.FileFound(AFilePath: String);
var
  sShortName: String;
  Node: PVirtualNode;
  NodeData: TvBaseNodeData;
begin
  sShortName := ExtractFileName(AFilePath);

  if Assigned(FListNode) then
    TVirtualTreeMethods.AddNodeByPathFile(ASuiteInstance.MainTree, FListNode, AFilePath, amInsertAfter, True);
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
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  NodeData: pScanFolderData;
begin
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    NodeData := Sender.GetNodeData(Node);
    if Assigned(NodeData) then
      ImageIndex := NodeData.ImageIndex;
  end;
end;

procedure TfrmScanFolder.vstGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: AnsiString);
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
