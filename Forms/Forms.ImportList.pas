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

unit Forms.ImportList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, VirtualTrees, Kernel.Consts, xmldom,
  XMLIntf, msxmldom, XMLDoc, Kernel.Enumerations, DateUtils, DKLang, JvExMask,
  Vcl.Mask, JvToolEdit;

type
  TfrmImportList = class(TForm)
    bvl1: TBevel;
    bvl2: TBevel;
    pgcImport: TPageControl;
    tsLaunchers: TTabSheet;
    tsSettings: TTabSheet;
    gbElements: TGroupBox;
    cbImportList: TCheckBox;
    cbImportSettings: TCheckBox;
    gbFile: TGroupBox;
    rgrpLauncher: TRadioGroup;
    lblFile: TLabel;
    tsList: TTabSheet;
    vstListImp: TVirtualStringTree;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    XMLDocument1: TXMLDocument;
    tsProgress: TTabSheet;
    pbImport: TProgressBar;
    lblItems: TLabel;
    imgList: TImage;
    lblList: TLabel;
    lblLauncher: TLabel;
    imgSettings: TImage;
    lblSettings: TLabel;
    DKLanguageController1: TDKLanguageController;
    edtPathList: TJvFilenameEdit;
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure edtPathListExit(Sender: TObject);
    procedure cbImportListClick(Sender: TObject);
    procedure tsLaunchersShow(Sender: TObject);
    procedure tsSettingsShow(Sender: TObject);

    procedure tsListShow(Sender: TObject);
    procedure tsProgressShow(Sender: TObject);
    procedure edtPathListAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtPathListChange(Sender: TObject);
  private
    { Private declarations }
    procedure ImportSettingsInASuite(XMLDoc: TXMLDocument);
    procedure ImportListInASuite;
    function  TreeImp2Tree(TreeImp, Tree: TVirtualStringTree): Boolean;
    function  GetNumberNodeImp(Sender: TBaseVirtualTree): Integer;
    procedure PopulateTree(Tree: TVirtualStringTree;FilePath: String);
    procedure CheckAllItems(State: TCheckState);
    function InternalImportOptions(XMLDoc: TXMLDocument): Boolean;
  public
    { Public declarations }
    class procedure Execute(AOwner: TComponent);
  end;

var
  frmImportList : TfrmImportList;

implementation

{$R *.dfm}

uses
  Forms.Main, Utility.Misc, AppConfig.Main, VirtualTree.Events, VirtualTree.Methods,
  Utility.FileFolder, Utility.XML, Database.Manager, Kernel.Types, NodeDataTypes.Base;

procedure TfrmImportList.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmImportList.btnBackClick(Sender: TObject);
begin
  pgcImport.SelectNextPage(false,false);
  btnBack.Enabled := pgcImport.ActivePageIndex <> 0;
end;

procedure TfrmImportList.btnSelectAllClick(Sender: TObject);
begin
  CheckAllItems(csCheckedNormal);
end;

procedure TfrmImportList.cbImportListClick(Sender: TObject);
begin
  btnNext.Enabled := ((cbImportList.Checked) or (cbImportSettings.Checked)) and
                     ((edtPathList.Text <> '') and FileExists(edtPathList.Text));
end;

procedure TfrmImportList.ImportSettingsInASuite(XMLDoc: TXMLDocument);
var
  FileExt: string;
begin
  //Import settings
  if (cbImportSettings.Checked) then
  begin
    if Not(XMLDocument1.Active) then
    begin
      FileExt  := ExtractFileExt(edtPathList.Text);
      //ASuite or ASuite or wppLauncher
      if (FileExt = EXT_XML) or (FileExt = EXT_XMLBCK) then
      begin
        XMLDocument1.FileName := edtPathList.Text;
        XMLDocument1.Active   := True;
      end;
    end;
//    if InternalImportOptions(XMLDoc) then
//      ImagesDM.IcoImages.GetIcon(Config.ASuiteIcons.PopupMenu.Accept, imgSettings.Picture.Icon)
//    else
//      ImagesDM.IcoImages.GetIcon(Config.ASuiteIcons.PopupMenu.Cancel,imgList.Picture.Icon);
  end;
end;

procedure TfrmImportList.ImportListInASuite;
begin
  if (cbImportList.Checked) then
  begin
    //Set progressbar's max
    pbImport.Max := GetNumberNodeImp(vstListImp);
    try
//      if TreeImp2Tree(vstListImp, Config.MainTree) then
//        ImagesDM.IcoImages.GetIcon(Config.ASuiteIcons.PopupMenu.Accept,imgList.Picture.Icon)
//      else
//        ImagesDM.IcoImages.GetIcon(Config.ASuiteIcons.PopupMenu.Cancel,imgList.Picture.Icon);
      lblItems.Caption := Format(DKLangConstW('msgItemsImported'),[pbImport.Max]);
    except
      on E : Exception do
      begin
        ShowMessageFmtEx(DKLangConstW('msgErrGeneric'),[E.ClassName,E.Message],True);
        lblItems.Caption := DKLangConstW('msgImportFailed');
      end;
    end;
  end;
end;

procedure TfrmImportList.PopulateTree(Tree: TVirtualStringTree;FilePath: String);
var
  DBImp : TDBManager;
  FileName : String;
  FileExt  : String;
begin
  vstListImp.BeginUpdate;
  vstListImp.Clear;
  FileName := LowerCase(ExtractFileName(FilePath));
  FileExt  := ExtractFileExt(FileName);
  //ASuite or ASuite or wppLauncher
  if (FileExt = EXT_XML) or (FileExt = EXT_XMLBCK) then
  begin
    XMLDocument1.FileName := FilePath;
    XMLDocument1.Active   := True;
    //Identify launcher xml from first node
    //ASuite 1.x
    if (XMLDocument1.DocumentElement.NodeName = 'ASuite') then
      XMLToTree(vstListImp,TImportOldListProcs.ASuite1NodeToTree,XMLDocument1)
    else //winPenPack Launcher 1.x
      if ChangeFileExt(FileName,'') = 'winpenpack' then
        XMLToTree(vstListImp,TImportOldListProcs.wppLauncherNodeToTree,XMLDocument1);
  end
  else //ASuite 2.x
    if (FileExt = EXT_SQL) or (FileExt = EXT_SQLBCK) then
    begin
      DBImp := TDBManager.Create();
      DBImp.Setup(FilePath);
      DBImp.ImportData(Tree);
      DBImp.Destroy;
    end;
  vstListImp.EndUpdate;
//  ImagesDM.GetChildNodesIcons(Tree, Tree.RootNode, isSmall);
end;

procedure TfrmImportList.btnDeselectAllClick(Sender: TObject);
begin
  CheckAllItems(csUncheckedNormal);
end;

procedure TfrmImportList.btnNextClick(Sender: TObject);
begin
  //If PageIndex is not last page, show next page
  if pgcImport.ActivePageIndex <> (pgcImport.PageCount - 1) then
  begin
    if (pgcImport.ActivePage = tsSettings) and Not (cbImportList.Checked) then
      pgcImport.ActivePage := tsProgress
    else
      pgcImport.SelectNextPage(True, false);
    btnBack.Enabled := pgcImport.ActivePageIndex <> 0;
  end
  else //Else close import form
    Close;
end;

procedure TfrmImportList.FormCreate(Sender: TObject);
begin
  TVirtualTreeEvents.Create.SetupVSTImportList(vstListImp);
  pgcImport.ActivePageIndex := 0;
  //Set imgList and imgSettings's icon
//  ImagesDM.IcoImages.GetBitmap(Config.ASuiteIcons.PopupMenu.Cancel,imgList.Picture.Bitmap);
//  ImagesDM.IcoImages.GetBitmap(Config.ASuiteIcons.PopupMenu.Cancel,imgSettings.Picture.Bitmap);
end;

procedure TfrmImportList.CheckAllItems(State: TCheckState);
var
  tnImp: PVirtualNode;
begin
  tnImp := vstListImp.GetFirst;
  while Assigned(tnImp) do
  begin
    vstListImp.CheckState[tnImp] := State;
    tnImp := tnImp.NextSibling;
  end;
end;

procedure TfrmImportList.edtPathListAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  btnNext.Enabled  := True;
end;

procedure TfrmImportList.edtPathListChange(Sender: TObject);
begin
  btnNext.Enabled  := edtPathList.Text <> '';
end;

procedure TfrmImportList.edtPathListExit(Sender: TObject);
begin
  Assert((Sender is TJvFilenameEdit), 'Sender is not TJvFilenameEdit!');

  btnNext.Enabled := ((Sender as TJvFilenameEdit).Text <> '') and FileExists((Sender as TJvFilenameEdit).Text);
end;

class procedure TfrmImportList.Execute(AOwner: TComponent);
var
  frm: TfrmImportList;
begin
  frm := TfrmImportList.Create(AOwner);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

function TfrmImportList.TreeImp2Tree(TreeImp, Tree: TVirtualStringTree): Boolean;
var
  tnImp : PVirtualNode;
  procedure ProcessTreeItem(tn, tnImp: PVirtualNode);
  var
    NodeData, NodeDataImp : PBaseData;
  begin
    if (tnImp = nil) then Exit;
    NodeDataImp := TreeImp.GetNodeData(tnImp);
    //Import checked item in main list
    if (tnImp.CheckState = csCheckedNormal) or (tnImp.CheckState = csMixedNormal) then
    begin
      //Update progress bar
      pbImport.Position := pbImport.Position + 1;
      lblItems.Caption  := Format(DKLangConstW('msgProcessingItems'), [((pbImport.Position / pbImport.Max) * 100), pbImport.Max]);
      Self.Update;
      //Create new node in vstList
      tn             := Tree.AddChild(tn, TVirtualTreeMethods.Create.CreateNodeData(NodeDataImp.Data.DataType));
      NodeData       := Tree.GetNodeData(tn);
      //Copy from NodeDataImp
      NodeData.Data.Copy(NodeDataImp.Data);
      //Set some properties
      RenameShortcutOnDesktop(NodeData.Data.Name + EXT_LNK,NodeDataImp.Data.Name + EXT_LNK);
      NodeData.Data.Name     := NodeDataImp.Data.Name;
      NodeData.Data.Position := tn.Index;
      NodeData.Data.SetPointerNode(tn);
      //Get icon item, only if tn is in first level
//      if (NodeData.Data.DataType <> vtdtSeparator) and (Tree.GetNodeLevel(tn) = 0) then
//        ImagesDM.GetNodeImageIndex(TvCustomRealNodeData(NodeData.Data), isAny);
    end;
    tnImp := tnImp.FirstChild;
    while Assigned(tnImp) do
    begin
      ProcessTreeItem(tn, tnImp);
      tnImp := tnImp.NextSibling;
    end;
  end;
begin
  Tree.BeginUpdate;
  Result := True;
  try
    tnImp := TreeImp.GetFirst;
    while Assigned(tnImp) do
    begin
      ProcessTreeItem(nil, tnImp);
      tnImp := tnImp.NextSibling;
    end;
  except
    Result := False;
  end;
  Tree.EndUpdate;
end;

procedure TfrmImportList.tsLaunchersShow(Sender: TObject);
begin
  lblTitle.Caption := DKLangConstW('msgImportTitle1');
  btnNext.Caption  := DKLangConstW('msgNext');
  btnNext.Enabled  := True;
end;

procedure TfrmImportList.tsListShow(Sender: TObject);
begin
  //If cbImportList is checked, import selected list in VirtualTree
  if (cbImportList.Checked) then
  begin
    lblTitle.Caption := DKLangConstW('msgImportTitle3');
    btnNext.Caption  := DKLangConstW('msgImport');
    //Import list in temporary vst
    Config.ASuiteState := lsImporting;
    PopulateTree(vstListImp, edtPathList.Text);
  end
  else //Else next page
    pgcImport.SelectNextPage(true,false)
end;

procedure TfrmImportList.tsSettingsShow(Sender: TObject);
begin
  vstListImp.Clear;
  lblTitle.Caption := DKLangConstW('msgImportTitle2');
  btnNext.Enabled  := (edtPathList.Text <> '') and FileExists(edtPathList.Text);
  btnNext.Caption  := DKLangConstW('msgNext');
  //Change opendialog's filter depending on chosen launcher
  case rgrpLauncher.ItemIndex of
    0: edtPathList.Filter:= Format(DKLangConstW('msgFilterASuite2'),   [EXT_SQL,EXT_SQLBCK,EXT_SQL,EXT_SQLBCK]);
    1: edtPathList.Filter:= Format(DKLangConstW('msgFilterASuite1'),   [EXT_XML,EXT_XMLBCK,EXT_XML,EXT_XMLBCK]);
    2: edtPathList.Filter:= Format(DKLangConstW('msgFilterWinPenPack'),[EXT_XML,EXT_XMLBCK,EXT_XML,EXT_XMLBCK]);
  end;
end;

function TfrmImportList.GetNumberNodeImp(Sender: TBaseVirtualTree): Integer;
var
  NumberNode : Integer;
begin
  NumberNode := 0;
  Sender.IterateSubtree(nil, TVirtualTreeMethods.Create.IncNumberNode, @NumberNode, [], True);
  Result := NumberNode;
end;

function TfrmImportList.InternalImportOptions(XMLDoc: TXMLDocument): Boolean;
var
  DBImp : TDBManager;
  ImportFileExt : String;
begin
  try
    ImportFileExt := LowerCase(ExtractFileExt(edtPathList.Text));
    if (ImportFileExt = EXT_XML) or (ImportFileExt = EXT_XMLBCK) then
      LoadXMLSettings(XMLDoc)
    else
      if (ImportFileExt = EXT_SQL) or (ImportFileExt = EXT_SQLBCK)then
      begin
        //ASuite 2.x
        DBImp := TDBManager.Create();
        DBImp.Setup(edtPathList.Text);
        DBImp.ImportOptions;
        DBImp.Destroy;
      end;
  finally
    Result := True;
    //Config changed and focus vstList (so it repaint)
    Config.Changed := True;
    frmMain.FocusControl(Config.MainTree);
  end;
end;

procedure TfrmImportList.tsProgressShow(Sender: TObject);
begin
  Config.ASuiteState := lsNormal;
  btnBack.Enabled  := False;
  btnNext.Enabled  := False;
  try
    lblTitle.Caption := DKLangConstW('msgImportProgress');
    //Which launcher?
    case rgrpLauncher.ItemIndex of
      0,1: lblLauncher.Caption := Format(lblLauncher.Caption,['ASuite']);
      2:   lblLauncher.Caption := Format(lblLauncher.Caption,['winPenPack Launcher']);
    end;
    //Set some label and progress bar visibile
    lblList.Enabled  := cbImportList.Checked;
    lblItems.Visible := cbImportList.Checked;
    pbImport.Visible := cbImportList.Checked;
    lblSettings.Enabled := cbImportSettings.Checked;
    //Import list
    ImportListInASuite;
    ImportSettingsInASuite(XMLDocument1);
  finally
    btnNext.Enabled  := True;
    btnNext.Caption  := DKLangConstW('msgClose');
    Self.Show;
  end;
end;

end.
