{
Copyright (C) 2006-2010 Matteo Salvi of SalvadorSoftware

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit ImportList;

interface

uses
  Windows, SysUtils, Graphics, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  VirtualTrees, AppConfig, DOM, XMLRead, ulEnumerations, ulDatabase, DateUtils, Classes;

type
  TImportListToTree = function(Tree: TVirtualStringTree;Node: TDOMNode;Parent: PVirtualNode): PVirtualNode of object;

  { TfrmImportList }

  TfrmImportList = class(TForm)
    bvl1: TBevel;
    bvl2: TBevel;
    nbImport: TNotebook;
    pgLaunchers: TPage;
    pgSettings: TPage;
    pgItems: TPage;
    pgProgress: TPage;
    rbASuite2: TRadioButton;
    rgrpLauncher: TRadioGroup;
    rbASuite1: TRadioButton;
    rbwppLauncher: TRadioButton;
    rbPStart: TRadioButton;
    gbElements: TGroupBox;
    cbImportList: TCheckBox;
    cbImportSettings: TCheckBox;
    gbFile: TGroupBox;
    lblFile: TLabel;
    btnBrowse: TButton;
    edtPathList: TEdit;
    vstListImp: TVirtualStringTree;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    OpenDialog1: TOpenDialog;
    pbImport: TProgressBar;
    lblItems: TLabel;
    imgList: TImage;
    lblList: TLabel;
    lblLauncher: TLabel;
    imgSettings: TImage;
    lblSettings: TLabel;
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure pgItemsBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgLaunchersBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgProgressBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure pgSettingsBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure vstListImpGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure vstListImpGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstListImpExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure rbASuite1Click(Sender: TObject);
    procedure rbPStartClick(Sender: TObject);
    procedure edtPathListEnter(Sender: TObject);
    procedure cbImportListClick(Sender: TObject);
  private
    { Private declarations }
    procedure SelectNextPage(Notebook: TNotebook; PageIndex: Integer);
    procedure SelectPrevPage(Notebook: TNotebook;PageIndex: Integer);
    function  TreeImp2Tree(TreeImp, Tree: TVirtualStringTree): Boolean;
    function  GetNumberNodeImp(Sender: TBaseVirtualTree): Integer;
    procedure PopulateTree(Tree: TVirtualStringTree;FilePath: String);
    function ASuite1NodeToTree(Tree: TVirtualStringTree;Node: TDOMNode;
                               Parent: PVirtualNode): PVirtualNode;
    function PStart2NodeToTree(Tree: TVirtualStringTree;Node: TDOMNode;
                               Parent: PVirtualNode): PVirtualNode;
    function wppLauncherNodeToTree(Tree: TVirtualStringTree;Node: TDOMNode;
                                    Parent: PVirtualNode): PVirtualNode;
    procedure XMLToTree(Tree: TVirtualStringTree;CallBack: TImportListToTree;
  XMLDoc: TXMLDocument);
    procedure CheckAllItems(State: TCheckState);
    procedure InternalImportOptions;
    function DoImport: Boolean;
  public
    { Public declarations }
  end;

var
  frmImportList : TfrmImportList;
  XMLDocument1: TXMLDocument;

implementation

{$R *.lfm}

uses
  Main, ulNodeDataTypes, ulCommonUtils, ulTreeView, udImages, ulAppConfig;

procedure TfrmImportList.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmImportList.pgItemsBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  //If cbImportList is checked, import selected list in VirtualTree
  if (cbImportList.Checked) then
  begin
    lblTitle.Caption := msgImportTitle3;
    btnNext.Caption  := msgImport;
    //Import list in temporary vst
    //PopulateTree(vstListImp, edtPathList.Text);
  end
  else //Else next page
    nbImport.PageIndex := 2;
end;

procedure TfrmImportList.pgLaunchersBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  lblTitle.Caption := msgImportTitle1;
  btnNext.Caption  := msgNext;
  btnNext.Enabled  := True;
end;

procedure TfrmImportList.pgProgressBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  btnNext.Caption := msgClose;
  lblTitle.Caption := msgImportProgress;
  //Which launcher?
  if rbASuite1.Checked then
    lblLauncher.Caption := Format(lblLauncher.Caption,['ASuite'])
  else
    if rbwppLauncher.Checked then
      lblLauncher.Caption := Format(lblLauncher.Caption,['wppLauncher'])
    else
      if rbPStart.Checked then
        lblLauncher.Caption := Format(lblLauncher.Caption,['PStart']);
  //Set progressbar's max
  pbImport.Max := GetNumberNodeImp(vstListImp);
  //Set some label and progress bar visibile
  lblList.Enabled  := cbImportList.Checked;
  lblItems.Visible := cbImportList.Checked;
  pbImport.Visible := cbImportList.Checked;
  lblSettings.Enabled := cbImportSettings.Checked;
  //Import in main
  //if DoImport then
  //  lblTitle.Caption := msgImportTitle4
  //else
  //  lblTitle.Caption := msgImportFailed;
  btnBack.Enabled  := False;
  Self.Show;
end;

procedure TfrmImportList.pgSettingsBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin
  lblTitle.Caption := msgImportTitle2;
  btnNext.Enabled  := (edtPathList.Text <> '') and FileExists(edtPathList.Text);
  btnNext.Caption  := msgNext;
end;

function TfrmImportList.ASuite1NodeToTree(Tree: TVirtualStringTree;Node: TDOMNode;
                                          Parent: PVirtualNode): PVirtualNode;
var
  NodeData     : PBaseData;
  FileNodeData : TvFileNodeData;
begin
  Result := nil;
  with Node do
  begin
    { TODO : Check this code *Lazarus Porting* }
    //(Attributes[LongWord('name')] <> nil
    if ((Node.HasAttributes) or (NodeName = 'Separator')) and
       ((NodeName = 'Software') or (NodeName = 'Category') or (NodeName = 'Separator')) then
    begin
      //Create a new node
      if (Node.NodeName = 'Category') then
        Result := Tree.AddChild(Parent, CreateNodeData(vtdtCategory))
      else
        if (Node.NodeName = 'Software') then
          Result := Tree.AddChild(Parent, CreateNodeData(vtdtFile))
        else
          if NodeName = 'Separator' then
            Result := Tree.AddChild(Parent, CreateNodeData(vtdtSeparator));
      Tree.CheckType[Result] := ctTriStateCheckBox;
      NodeData       := Tree.GetNodeData(Result);
      NodeData.pNode := Result;
      FileNodeData   := TvFileNodeData(NodeData.Data);
      //Get Property
      if (FileNodeData.DataType <> vtdtSeparator) then
      begin
        FileNodeData.Name     := String(Attributes[LongWord('name')]);
        FileNodeData.PathIcon := GetStrPropertyXML(Node, 'PathIcon', '');
        FileNodeData.NoMRU    := GetBoolPropertyXML(Node, 'DontInsertMRU',false);
        if (FileNodeData.DataType = vtdtFile) then
        begin
          FileNodeData.PathExe     := GetStrPropertyXML(Node, 'PathExe', '');
          FileNodeData.Parameters  := GetStrPropertyXML(Node, 'Parameters', '');
          FileNodeData.WorkingDir  := GetStrPropertyXML(Node, 'WorkingDir', '');
          FileNodeData.WindowState := GetIntPropertyXML(Node, 'WindowState',0);
          FileNodeData.ShortcutDesktop := GetBoolPropertyXML(Node, 'ShortcutDesktop',false);
          FileNodeData.ActionOnExe := TActionOnExecution(GetIntPropertyXML(Node, 'ActionOnExe',0));
          FileNodeData.AutorunPos  := GetIntPropertyXML(Node, 'AutorunPosition',0);
          FileNodeData.Autorun     := TAutorunType(GetIntPropertyXML(Node, 'Autorun',0));
        end;
      end;
    end;
  end;
end;

procedure TfrmImportList.btnBackClick(Sender: TObject);
begin
  SelectPrevPage(nbImport, nbImport.PageIndex);
  btnBack.Enabled := nbImport.PageIndex <> 0;
end;

procedure TfrmImportList.btnBrowseClick(Sender: TObject);
begin
  vstListImp.Clear;
  if rbASuite1.Checked then
    OpenDialog1.Filter:= 'All list|*.xml;*.sqlite;*.bck;*.sqbck|ASuite 2.x List (*.sqlite, *.sqbck)|*.sqlite;*.sqbck|ASuite 1.x List (*.xml, *.bck)|*.xml;*.bck'
  else
    if rbwppLauncher.Checked then
      OpenDialog1.Filter:= 'winPenPack 1.x List (*.xml, *.bck)|*.xml;*.bck'
    else
      if rbPStart.Checked then
        OpenDialog1.Filter:= 'PStart 2.x List (*.xml)|*.xml';
  OpenDialog1.InitialDir := ExtractFileDir(edtPathList.text);
  if (OpenDialog1.Execute) then
  begin
    edtPathList.text := OpenDialog1.FileName;
    btnNext.Enabled  := True;
  end;
  SetCurrentDir(SUITE_WORKING_PATH);
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

procedure TfrmImportList.SelectNextPage(Notebook: TNotebook; PageIndex: Integer);
begin
  Notebook.PageIndex := PageIndex + 1;
end;

procedure TfrmImportList.SelectPrevPage(Notebook: TNotebook;PageIndex: Integer);
begin
  Notebook.PageIndex := PageIndex - 1;
end;

procedure TfrmImportList.PopulateTree(Tree: TVirtualStringTree;FilePath: String);
var
  DBImp : TDBManager;
begin
  vstListImp.BeginUpdate;
  if LowerCase(ExtractFileExt(FilePath)) = EXT_XML then
  begin
    ReadXMLFile(XMLDocument1,FilePath);
    if LowerCase(ChangeFileExt(ExtractFileName(FilePath),'')) = 'asuite' then
      XMLToTree(vstListImp,ASuite1NodeToTree,XMLDocument1)
    else
      if LowerCase(ChangeFileExt(ExtractFileName(FilePath),'')) = 'pstart' then
        XMLToTree(vstListImp,PStart2NodeToTree,XMLDocument1)
      else
        if LowerCase(ChangeFileExt(ExtractFileName(FilePath),'')) = 'winpenpack' then
          XMLToTree(vstListImp,wppLauncherNodeToTree,XMLDocument1);
  end
  else
    if LowerCase(ExtractFileExt(FilePath)) = EXT_SQL then
    begin
      //ASuite 2.x
      DBImp := TDBManager.Create(True, FilePath);
      DBImp.LoadData(Tree, True);
      DBImp.Destroy;
    end;
  GetChildNodesIcons(vstListImp, vstListImp.RootNode);
  vstListImp.EndUpdate;
end;

function TfrmImportList.PStart2NodeToTree(Tree: TVirtualStringTree;Node: TDomNode;
                                          Parent: PVirtualNode): PVirtualNode;
var
  NodeData     : PBaseData;
  FileNodeData : TvFileNodeData;
begin
  Result := nil;
  with Node do
  begin
    if ((Node.HasAttributes) or (NodeName = 'separator')) and
       ((NodeName = 'file') or (NodeName = 'files') or (NodeName = 'separator')) then
    begin
      //Create a new node
      if (NodeName = 'files') then
        Result := tree.AddChild(Parent, CreateNodeData(vtdtCategory))
      else
        if (NodeName = 'file') then
          Result := tree.AddChild(Parent, CreateNodeData(vtdtFile))
        else
          if NodeName = 'separator' then
            Result := Tree.AddChild(Parent, CreateNodeData(vtdtSeparator));
      Tree.CheckType[Result] := ctTriStateCheckBox;
      NodeData       := Tree.GetNodeData(Result);
      NodeData.pNode := Result;
      FileNodeData   := TvFileNodeData(NodeData.Data);
      //Get Property
      if FileNodeData.DataType <> vtdtSeparator then
      begin
        FileNodeData.Name     := String(Attributes[LongWord('name')]);
        FileNodeData.PathIcon := GetStrPropertyXML(Node,'icon','');
        FileNodeData.PathExe  := StringReplace(FileNodeData.PathExe, '%pdrive%', '$ASuite',[rfIgnoreCase]);
        FileNodeData.NoMRU    := (GetStrPropertyXML(Node, 'hidefromtray','false') = 'True');
        if (FileNodeData.DataType = vtdtFile) then
        begin
          FileNodeData.PathExe     := GetStrPropertyXML(Node,'path','');
          FileNodeData.Parameters  := GetStrPropertyXML(Node,'parameters','');
          FileNodeData.WorkingDir  := GetStrPropertyXML(Node,'directory','');
          FileNodeData.WindowState := GetIntPropertyXML(Node,'windowstate',0);
          FileNodeData.ActionOnExe := TActionOnExecution(GetIntPropertyXML(Node, 'executionaction',0));
          FileNodeData.AutorunPos  := GetIntPropertyXML(Node,'AutorunPosition',0);
          if GetStrPropertyXML(Node, 'autorun', '') = 'onstartup' then
            TvFileNodeData(NodeData.Data).Autorun    := atAlwaysOnStart
          else
            if GetStrPropertyXML(Node, 'autorun', '') = 'onexit' then
              FileNodeData.Autorun := atAlwaysOnClose;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportList.rbASuite1Click(Sender: TObject);
begin
  cbImportSettings.Enabled := (Sender as TRadioButton).Checked;
  cbImportSettings.Checked := (Sender as TRadioButton).Checked;
end;

procedure TfrmImportList.rbPStartClick(Sender: TObject);
begin
  cbImportSettings.Enabled := Not(Sender as TRadioButton).Checked;
  cbImportSettings.Checked := Not(Sender as TRadioButton).Checked;
end;

function TfrmImportList.wppLauncherNodeToTree(Tree: TVirtualStringTree;Node: TDomNode;
                                               Parent: PVirtualNode): PVirtualNode;
var
  NodeData     : PBaseData;
  FileNodeData : TvFileNodeData;
begin
  Result := nil;
  with Node do
  begin
    if ((Node.HasAttributes) or (NodeName = 'separator')) and
       ((NodeName = 'file') or (NodeName = 'files') or (NodeName = 'separator')) then
    begin
      //Create a new node
      if (NodeName = 'files') then
        Result := tree.AddChild(Parent, CreateNodeData(vtdtCategory))
      else
        if (NodeName = 'file') then
          Result := tree.AddChild(Parent, CreateNodeData(vtdtFile))
        else
          if NodeName = 'separator' then
            Result := Tree.AddChild(Parent, CreateNodeData(vtdtSeparator));
      Tree.CheckType[Result] := ctTriStateCheckBox;
      NodeData       := Tree.GetNodeData(Result);
      NodeData.pNode := Result;
      FileNodeData   := TvFileNodeData(NodeData.Data);
      if FileNodeData.DataType <> vtdtSeparator then
      begin
        FileNodeData.Name     := String(Attributes[LongWord('name')]);
        FileNodeData.PathIcon := GetStrPropertyXML(Node,'icon','');
        FileNodeData.NoMRU    := GetBoolPropertyXML(Node, 'DontInsertMRU',false);
        if (FileNodeData.DataType = vtdtFile) then
        begin
          FileNodeData.PathExe     := GetStrPropertyXML(Node,'path','');
          FileNodeData.Parameters  := GetStrPropertyXML(Node,'parameters','');
          FileNodeData.WorkingDir  := GetStrPropertyXML(Node,'WorkingDir','');
          FileNodeData.WindowState := GetIntPropertyXML(Node,'WindowState',0);
          FileNodeData.ShortcutDesktop := GetBoolPropertyXML(Node, 'ShortcutDesktop',false);
          FileNodeData.ActionOnExe := TActionOnExecution(GetIntPropertyXML(Node, 'ActionOnExe',0));
          FileNodeData.AutorunPos  := GetIntPropertyXML(Node,'AutorunPosition',0);
          FileNodeData.Autorun     := TAutorunType(GetIntPropertyXML(Node, 'Autorun',0));
        end;
      end;
    end;
  end;
end;

procedure TfrmImportList.btnDeselectAllClick(Sender: TObject);
begin
  CheckAllItems(csUncheckedNormal);
end;

procedure TfrmImportList.btnNextClick(Sender: TObject);
begin
  //If PageIndex is not last page, show next page
  if nbImport.PageIndex <> (nbImport.PageCount - 1) then
    SelectNextPage(nbImport, nbImport.PageIndex)
  else //Else close import form
    Close;
  btnBack.Enabled := nbImport.PageIndex <> 0;
end;

procedure TfrmImportList.FormCreate(Sender: TObject);
var
  Icon : TIcon;
begin
  vstListImp.NodeDataSize := SizeOf(rBaseData);
  nbImport.PageIndex      := 0;
  //Set imgList and imgSettings's icon
  Icon := TIcon.Create;
  try
    ImagesDM.IcoImages.GetIcon(IMG_Cancel,Icon);
    imgList.Picture.Icon     := Icon;
    imgSettings.Picture.Icon := Icon;
  finally
    Icon.Free;
  end;
end;

procedure TfrmImportList.vstListImpExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
  GetChildNodesIcons(Sender, Node);
end;

procedure TfrmImportList.vstListImpGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : PvBaseNodeData;
begin
  NodeData   := Sender.GetNodeData(Node);
  ImageIndex := NodeData.ImageIndex;
  if Column = 1 then
    ImageIndex := -1;
end;

procedure TfrmImportList.vstListImpGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData : PBaseData;
  I        : Byte;
  str      : string;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData.Data.DataType = vtdtSeparator then
  begin
    I := 40 - (Length(NodeData.Data.Name));
    str := '----------------------';
    if I >= 10 then
      SetLength(str, I div 2)
    else
      SetLength(str, 5);
    CellText := str + ' ' + NodeData.Data.Name + ' ' + str;
  end
  else
    CellText := StringReplace(NodeData.Data.Name, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
end;

procedure TfrmImportList.XMLToTree(Tree: TVirtualStringTree;CallBack: TImportListToTree;
  XMLDoc: TXMLDocument);
var
  cXMLNode : TDOMNode;
  procedure ProcessNode(XMLNode : TDOMNode;TreeNode : PVirtualNode);
  var
    cNode : TDOMNode;
  begin
    if XMLNode = nil then Exit;
    //Import xml node in vstListImp
    TreeNode := CallBack(vstListImp, XMLNode, TreeNode);
    //Next nodes
    cNode := XMLNode.FirstChild;
    while Assigned(cNode) do
    begin
      ProcessNode(cNode,TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;
begin
  Tree.Clear;
  Tree.BeginUpdate;
  cXMLNode := XMLDoc.DocumentElement.FirstChild;
  while Assigned(cXMLNode) do
  begin
    ProcessNode(cXMLNode,nil);
    cXMLNode := cXMLNode.NextSibling;
  end;
  Tree.EndUpdate;
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

procedure TfrmImportList.edtPathListEnter(Sender: TObject);
begin
  btnNext.Enabled := ((Sender as TEdit).Text <> '') and FileExists((Sender as TEdit).Text);
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
    if (tnImp.CheckState = csCheckedNormal) or (tnImp.CheckState = csMixedNormal) then
    begin
      pbImport.Position := pbImport.Position + 1;
      lblItems.Caption  := Format(msgProcessingItems, [((pbImport.Position / pbImport.Max) * 100), pbImport.Max]);
      Self.Update;
      tn             := Tree.AddChild(tn);
      NodeData       := Tree.GetNodeData(tn);
      NodeData^      := NodeDataImp^;
      with NodeData.Data do
      begin
        ID           := -1;
        ParentID     := -1;
        Position     := tn.Index;
        UnixAddDate  := DateTimeToUnix(Now);
        UnixEditDate := NodeData.Data.UnixAddDate;
      end;
      NodeData.pNode := tn;
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

function TfrmImportList.GetNumberNodeImp(Sender: TBaseVirtualTree): Integer;
var
  NumberNode : Integer;
begin
  NumberNode := 0;
  Sender.IterateSubtree(nil, IterateSubTreeProcs.IncNumberNode, @NumberNode, [], True);
  Result := NumberNode;
end;

function TfrmImportList.DoImport: Boolean;
var
  Icon : TIcon;
begin
  Result := False;
  Icon   := TIcon.Create;
  ImagesDM.IcoImages.GetIcon(IMG_Accept,Icon);
  if (cbImportSettings.Checked) or (cbImportList.Checked) then
  begin
    //Import list in main vst
    if cbImportList.Checked then
    begin
      Result := TreeImp2Tree(vstListImp,frmMain.vstList);
      if Result then
      begin
        imgList.Picture.Icon := Icon;
        lblItems.Caption := Format(msgItemsImported,[pbImport.Max]);
      end
      else begin
        showmessage(msgImportFailed);
        lblItems.Caption := msgImportFailed;
      end;
    end;
    //Import settings
    if cbImportSettings.Checked then
    begin
      InternalImportOptions;
      Result := True;
      imgSettings.Picture.Icon := Icon;
    end;
  end;
  Icon.Free;
end;

procedure TfrmImportList.InternalImportOptions;
var
  DBImp : TDBManager;
  Node  : TDOMNode;
begin
  if (LowerCase(ExtractFileExt(edtPathList.Text)) = EXT_XML) and
     (LowerCase(ChangeFileExt(ExtractFileName(edtPathList.Text),'')) = 'asuite') then
  begin
    ReadXMLFile(XMLDocument1, edtPathList.Text);
    //ASuite 1.x
    Node := XMLDocument1.DocumentElement.FindNode('Option');
    with Config do
    begin
      //General
      StartWithWindows   := GetBoolPropertyXML(Node, 'StartOnWindowsStartup', false);
      Config.ShowPanelAtStartUp := GetBoolPropertyXML(Node, 'StartUpShowPanel', true);
      Config.ShowMenuAtStartUp  := GetBoolPropertyXML(Node, 'StartUpShowMenu', false);
      //Main Form
  { TODO -oMatteo -c : Insert code for language 26/11/2009 22:21:05 }
//      FLanguage           := '';
      Config.CustomTitleString := GetStrPropertyXML(Node, 'CustomTitleString', APP_NAME);
      Config.UseCustomTitle    := GetBoolPropertyXML(Node, 'CustomTitle', false);
      Config.HideTabSearch     := GetBoolPropertyXML(Node, 'HideSearch', false);
      //Main Form - Position and size
      Config.HoldSize    := GetBoolPropertyXML(Node, 'HoldSize', false);
      Config.AlwaysOnTop := GetBoolPropertyXML(Node, 'MainOnTop', false);
      //frmMain's size
      frmMain.Width      := GetIntPropertyXML(Node,'ListFormWidth',frmMainWidth);
      frmMain.Height     := GetIntPropertyXML(Node,'ListFormHeight',frmMainHeight);
      //frmMain position
      if ((GetIntPropertyXML(Node,'ListFormTop',frmMain.Top) <> -1) and
         (GetIntPropertyXML(Node,'ListFormLeft',frmMain.Left) <> -1)) then
      begin
        if ((GetIntPropertyXML(Node,'ListFormTop',frmMain.Top) + frmMainHeight) <= GetDeviceCaps(GetDC(frmMain.Handle), VERTRES)) then
          frmMain.Top  := GetIntPropertyXML(Node,'ListFormTop',frmMain.Top)
        else
          frmMain.Top  := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES) - frmMain.Height - 30;
        if ((GetIntPropertyXML(Node,'ListFormLeft',frmMain.Left) + frmMainWidth) <= GetDeviceCaps(GetDC(frmMain.Handle), HORZRES)) then
          frmMain.Left := GetIntPropertyXML(Node,'ListFormLeft',frmMain.Left)
        else
          frmMain.Left := GetDeviceCaps(GetDC(frmMain.Handle), HORZRES) - frmMain.Width;
        frmMain.Position := poDesigned;
      end;
      //Main Form - Treevew
      Config.TVBackgroundPath := GetStrPropertyXML(Node, 'BackgroundPath','');
      Config.TVBackground     := GetBoolPropertyXML(Node, 'Background',False);
      Config.TVAutoOpClCats   := GetBoolPropertyXML(Node, 'AutoOpClCategories',False);
      //Treeview Font
      Config.TVFont.Name   := GetStrPropertyXML(Node, 'TreeViewFontName','MS Sans Serif');
      with Node.FindNode('TreeViewFontStyle') do
      begin
        if GetBoolPropertyXML(Node.FindNode('TreeViewFontStyle'),'fsBold',false) then
          Config.TVFont.Style := Config.TVFont.Style + [fsBold];
        if GetBoolPropertyXML(Node.FindNode('TreeViewFontStyle'),'fsItalic',false) then
          Config.TVFont.Style := Config.TVFont.Style + [fsItalic];
        if GetBoolPropertyXML(Node.FindNode('TreeViewFontStyle'),'fsUnderline',false) then
          Config.TVFont.Style := Config.TVFont.Style + [fsUnderline];
        if GetBoolPropertyXML(Node.FindNode('TreeViewFontStyle'),'fsStrikeOut',false) then
          Config.TVFont.Style := Config.TVFont.Style + [fsStrikeOut];
      end;
      Config.TVFont.Size    := GetIntPropertyXML(Node,'TreeViewFontSize',8);
      Config.TVFont.Color   := GetIntPropertyXML(Node,'TreeViewFontColor',clWindowText);
      Config.TVFontChanged  := True;
      //MRU
      Config.MRU            := GetBoolPropertyXML(Node, 'ActiveMRU',true);
      Config.SubMenuMRU     := GetBoolPropertyXML(Node, 'ActiveSubMenuMRU',false);
      Config.MRUNumber      := GetIntPropertyXML(Node, 'MRUNumber',5);
      //Backup
      Config.Backup         := GetBoolPropertyXML(Node, 'ActiveBackup',true);
      Config.BackupNumber   := GetIntPropertyXML(Node, 'BackupNumber',5);
      //Other functions
      Config.Autorun        := GetBoolPropertyXML(Node, 'ActiveAutorun',true);
      Config.Cache          := GetBoolPropertyXML(Node, 'ActiveCache',true);
      //Execution
      Config.ActionOnExe    := TActionOnExecution(GetIntPropertyXML(Node, 'ActionOnExe',0));
      Config.RunSingleClick := GetBoolPropertyXML(Node, 'RunSingleClick',false);
      //Trayicon
      Config.TrayIcon           := GetBoolPropertyXML(Node, 'ActiveTrayIcon',true);
      Config.TrayCustomIconPath := GetStrPropertyXML(Node, 'TrayIconPath','');
      Config.ActionClickLeft    := GetIntPropertyXML(Node, 'ActionClickLeft',0);
      Config.ActionClickRight   := GetIntPropertyXML(Node, 'ActionClickRight',2);
    end;
  end
  else
    if LowerCase(ExtractFileExt(edtPathList.Text)) = EXT_SQL then
    begin
      //ASuite 2.x
      DBImp := TDBManager.Create(True, edtPathList.Text);
      try
        DBImp.LoadOptions;
      finally
        DBImp.Destroy;
      end;
    end;
  //Config changed and focus vstList (so it repaint)
  Config.Changed := True;
  frmMain.FocusControl(frmMain.vstList);
end;

end.
