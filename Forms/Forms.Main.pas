{
Copyright (C) 2006-2020 Matteo Salvi

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

unit Forms.Main;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ComCtrls, VirtualTrees, Kernel.Consts, DataModules.Icons, Kernel.BaseMainForm,
  StdCtrls, Kernel.Enumerations, ExtCtrls, ActnList, EditBtn, DefaultTranslator;

type

  { TfrmMain }

  TfrmMain = class(TBaseMainForm)
    miStatistics: TMenuItem;
    MenuItem2: TMenuItem;
    mniOpenFolderItem: TMenuItem;
    mniRunAsAdminItem: TMenuItem;
    mniRunAsItem: TMenuItem;
    mniRunItem: TMenuItem;
    N9: TMenuItem;
    vstList: TVirtualStringTree;
    pcList: TPageControl;
    tbList: TTabSheet;
    tbSearch: TTabSheet;
    vstSearch: TVirtualStringTree;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miHelp: TMenuItem;
    N2: TMenuItem;
    miOptions1: TMenuItem;
    miImportList: TMenuItem;
    miExit1: TMenuItem;
    miInfoASuite: TMenuItem;
    miEdit: TMenuItem;
    mniAddCat: TMenuItem;
    mniAddSoftware: TMenuItem;
    mniDelete: TMenuItem;
    N8: TMenuItem;
    mniProperty: TMenuItem;
    pmWindow: TPopupMenu;
    mniAddFolder: TMenuItem;
    miSaveList1: TMenuItem;
    mniAddSeparator: TMenuItem;
    N3: TMenuItem;
    mniSortCatItems: TMenuItem;
    miExportList: TMenuItem;
    N4: TMenuItem;
    N1: TMenuItem;
    SaveDialog1: TSaveDialog;
    miN11: TMenuItem;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    pmSearch: TPopupMenu;
    miSearchName: TMenuItem;
    miSearchExePath: TMenuItem;
    miSearchIconPath: TMenuItem;
    miSearchWorkingDirPath: TMenuItem;
    miSearchParameters: TMenuItem;
    btnedtSearch: TEditButton;
    mniScanFolder: TMenuItem;
    
    ActionList1: TActionList;
    actRunItem: TAction;
    actRunAsItem: TAction;
    actRunAsAdminItem: TAction;
    actOpenFolderItem: TAction;
    actSortCatItems: TAction;
    actAddCat: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actDelete: TAction;
    actProperty: TAction;
    actAddSoftware: TAction;
    actAddFolder: TAction;
    actAddSeparator: TAction;
    tmrCheckItems: TTimer;
    actSortList: TAction;
    mniSortList: TMenuItem;
    procedure miOptionsClick(Sender: TObject);
    procedure miStatisticsClick(Sender: TObject);
    procedure miImportListClick(Sender: TObject);
    procedure miSaveListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miExportListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miInfoASuiteClick(Sender: TObject);
    procedure btnedtSearchRightButtonClick(Sender: TObject);
    procedure ChangeSearchTextHint(Sender: TObject);
    procedure btnedtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure mniScanFolderClick(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actRunItemUpdate(Sender: TObject);
    procedure actSortCatItemsUpdate(Sender: TObject);
    procedure actSortCatItemsExecute(Sender: TObject);
    procedure actAddItem(Sender: TObject);
    procedure actAddItemUpdate(Sender: TObject);
    procedure actCutCopyDeleteUpdate(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPropertyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure pcListChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure tmrCheckItemsTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actRunItemExecute(Sender: TObject);
    procedure actSortListUpdate(Sender: TObject);
    procedure actSortListExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnedtSearchChange(Sender: TObject);
  private
    { Private declarations }
    function  GetActiveTree: TBaseVirtualTree;
    procedure PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);
  public
    { Public declarations }
    procedure ShowMainForm(Sender: TObject);
    procedure HideMainForm;
    procedure DoSearchItem(const TreeSearch: TBaseVirtualTree; const Keyword: string;
                           const SearchType: TSearchType);
    procedure SetAllIcons;
  end;

var
  frmMain : TfrmMain;

implementation

uses
  Forms.Options, Forms.About, Utility.Misc, Forms.ScanFolder,
  DataModules.TrayMenu, Forms.ImportList, AppConfig.Main, Utility.System,
  VirtualTree.Methods, Frame.Options.Stats, NodeDataTypes.Base, Kernel.Scheduler,
  Kernel.Types, NodeDataTypes.Files, VirtualTree.Events, Utility.Process,
  Kernel.Logger, SynLog, FileUtil;

{$R *.lfm}

procedure TfrmMain.actAddItemUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  TASuiteLogger.Info('Copy nodes into clipboard', []);
  vstList.CopyToClipBoard;
end;

procedure TfrmMain.actCutCopyDeleteUpdate(Sender: TObject);
var
  Nodes: TNodeArray;
begin
  TAction(Sender).Enabled := False;
  if not(tsEditing in GetActiveTree.TreeStates) then
  begin
    Nodes := GetActiveTree.GetSortedSelection(True);
    TAction(Sender).Enabled := Length(Nodes) > 0;
  end;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  TASuiteLogger.Info('Cut nodes into clipboard', []);
  vstList.CutToClipBoard;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var
  Nodes: TNodeArray;
  Tree: TBaseVirtualTree;
begin
  TASuiteLogger.Enter('actDeleteExecute', Self);
  Config.ASuiteState := lsDeleting;
  try
    Tree := GetActiveTree;
    if (Tree.GetFirstSelected <> nil) and (Config.TVDisableConfirmDelete {or (MessageDlg((DKLangConstW('msgConfirmDeleteItem')), mtWarning, [mbYes,mbNo], 0) = mrYes)}) then
    begin
      Nodes := Tree.GetSortedSelection(true);
      //Delete items
      if Config.DBManager.DeleteItems(Tree, Nodes) then
      begin
        //Delete nodes and refresh list
        Tree.DeleteSelectedNodes;
        TVirtualTreeMethods.Create.RefreshList(Tree);
      end;
    end;
  finally
    Config.ASuiteState := lsNormal;
  end;
end;

procedure TfrmMain.actRunItemExecute(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ExecuteSelectedNodes(GetActiveTree, TRunMode(TAction(Sender).Tag), False);
end;

procedure TfrmMain.actAddItem(Sender: TObject);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.AddChildNodeByGUI(vstList, vstList.GetFirstSelected,
                                                           TvTreeDataType(TAction(Sender).Tag));
  if Assigned(NodeData) then
  begin
    vstList.ClearSelection;
    vstList.Selected[NodeData.PNode] := True;
  end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
var
  NodeData: TvBaseNodeData;
  Tree: TVirtualStringTree;
begin
  TASuiteLogger.Info('Paste clipboard content in ASuite', []);
  Tree := TVirtualStringTree(GetActiveTree);
  if Assigned(Tree) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Tree.GetFirstSelected, Tree);
    if Assigned(NodeData) then
    begin
      if NodeData.DataType = vtdtCategory then
        Tree.DefaultPasteMode := amAddChildLast
      else
        Tree.DefaultPasteMode := amInsertAfter;
      end
    else
      Tree.DefaultPasteMode := amAddChildLast;
    Tree.PasteFromClipboard;
    Tree.Expanded[Tree.GetFirstSelected] := True;
    TVirtualTreeMethods.Create.RefreshList(Tree);
  end;
end;

procedure TfrmMain.actPasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IsFormatInClipBoard(CF_VIRTUALTREE) and (GetActiveTree = vstList);
end;

procedure TfrmMain.actPropertyExecute(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ShowItemProperty(Self, GetActiveTree, GetActiveTree.GetFirstSelected);
end;

procedure TfrmMain.actRunItemUpdate(Sender: TObject);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
  I: Integer;
begin
  Nodes := GetActiveTree.GetSortedSelection(True);
  TAction(Sender).Enabled := False;
  if Length(Nodes) > 0 then
  begin
    for I := Low(Nodes) to High(Nodes) do
    begin
      NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Nodes[I], GetActiveTree);

      if NodeData.DataType <> vtdtSeparator then
        TAction(Sender).Enabled := True;

      if (TAction(Sender).Tag = 3) then
      begin
        if NodeData.DataType = vtdtFile then
        begin
          if IsValidURLProtocol(TvFileNodeData(NodeData).PathAbsoluteFile) then
            TAction(Sender).Enabled := False
        end
        else
          TAction(Sender).Enabled := False;
      end;
    end;
  end;
end;

procedure TfrmMain.actSortCatItemsExecute(Sender: TObject);
var
  Nodes: TNodeArray;
  I: Integer;
begin
  Nodes := GetActiveTree.GetSortedSelection(True);
  if Length(Nodes) > 0 then
  begin
    for I := Low(Nodes) to High(Nodes) do
      vstList.Sort(Nodes[I], 0, sdAscending);
  end;
  TVirtualTreeMethods.Create.RefreshList(vstList);
end;

procedure TfrmMain.actSortCatItemsUpdate(Sender: TObject);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
  I: Integer;
begin
  Nodes := GetActiveTree.GetSortedSelection(True);
  TAction(Sender).Visible := (GetActiveTree = vstList);
  TAction(Sender).Enabled := False;
  if (Length(Nodes) > 0) and (GetActiveTree = vstList) then
  begin
    for I := Low(Nodes) to High(Nodes) do
    begin
      NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Nodes[I], GetActiveTree);
      if NodeData.DataType = vtdtCategory then
        TAction(Sender).Enabled := True;
    end;
  end;
end;

procedure TfrmMain.actSortListExecute(Sender: TObject);
begin
  vstList.SortTree(-1, sdAscending);

  TVirtualTreeMethods.Create.RefreshList(vstList);
end;

procedure TfrmMain.actSortListUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
  TAction(Sender).Enabled := (vstList.HasChildren[vstList.RootNode]) and (GetActiveTree = vstList);
end;

procedure TfrmMain.btnedtSearchChange(Sender: TObject);
begin
  if Config.SearchAsYouType then
  begin
    //TODO lazarus
    {if btnedtSearch.Text <> '' then
      btnedtSearch.RightButton.ImageIndex := Config.IconsManager.GetIconIndex('cancel')
    else
      btnedtSearch.RightButton.ImageIndex := Config.IconsManager.GetIconIndex('search');}

    DoSearchItem(vstSearch, btnedtSearch.Text, TSearchType(GetCheckedMenuItem(pmSearch).Tag));
  end;
end;

procedure TfrmMain.btnedtSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    btnedtSearchRightButtonClick(Sender);
end;

procedure TfrmMain.btnedtSearchRightButtonClick(Sender: TObject);
begin
  if Config.SearchAsYouType then
    btnedtSearch.Text := ''
  else
    DoSearchItem(vstSearch, btnedtSearch.Text, TSearchType(GetCheckedMenuItem(pmSearch).Tag));
end;

procedure TfrmMain.miSaveListClick(Sender: TObject);
begin;
  {if Config.SaveList(True) then
    ShowMessageEx(DKLangConstW('msgSaveCompleted'))
  else
    ShowMessageEx(DKLangConstW('msgErrSave'),true);}
end;

procedure TfrmMain.ChangeSearchTextHint(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    //Set new placeholder and SearchType
    btnedtSearch.TextHint := StringReplace((Sender as TMenuItem).Caption, '&', '', []);
    (Sender as TMenuItem).Checked := True;
  end;
end;

procedure TfrmMain.miImportListClick(Sender: TObject);
begin
  TfrmImportList.Execute(Self);
  TVirtualTreeMethods.Create.RefreshList(GetActiveTree);
end;

procedure TfrmMain.miInfoASuiteClick(Sender: TObject);
begin
  if not IsFormOpen('frmAbout') then
    Application.CreateForm(TfrmAbout, frmAbout);
  frmAbout.show;
end;

procedure TfrmMain.miOptionsClick(Sender: TObject);
begin
  TfrmOptions.Execute(Self);
end;

procedure TfrmMain.miStatisticsClick(Sender: TObject);
begin
  TfrmOptions.Execute(Self, TfrmStatsOptionsPage);
end;

procedure TfrmMain.mniScanFolderClick(Sender: TObject);
begin
  TfrmScanFolder.Execute(Self);
  TVirtualTreeMethods.Create.RefreshList(GetActiveTree);
end;

procedure TfrmMain.pcListChange(Sender: TObject);
begin
  //Clear search when user click on tab search
  if pcList.ActivePageIndex = PG_SEARCH then
  begin
    vstSearch.Clear;
    btnedtSearch.Text := '';
  end;
  TVirtualTreeMethods.Create.CheckVisibleNodePathExe(GetActiveTree);
end;

procedure TfrmMain.SetAllIcons;
begin
  //Set IcoImages
  //Set submenuimages to three MainMenu's subitems
  miFile.SubMenuImages := dmImages.ilSmallIcons;
  miEdit.SubMenuImages := dmImages.ilSmallIcons;
  miHelp.SubMenuImages := dmImages.ilSmallIcons;
  pmSearch.Images      := dmImages.ilSmallIcons;
  pmWindow.Images      := dmImages.ilSmallIcons;
  pcList.Images        := dmImages.ilSmallIcons;
  btnedtSearch.Images  := dmImages.ilSmallIcons;
  //Set pcList tabs' ImageIndexes
  tbList.ImageIndex    := Config.IconsManager.GetIconIndex('tree_list');
  tbSearch.ImageIndex  := Config.IconsManager.GetIconIndex('search');
  //Set MainMenu's ImageIndexes
  miSaveList1.ImageIndex   := Config.IconsManager.GetIconIndex('save');
  miOptions1.ImageIndex    := Config.IconsManager.GetIconIndex('options');
  actAddCat.ImageIndex     := Config.IconsManager.GetIconIndex('add_category');
  actAddSoftware.ImageIndex := Config.IconsManager.GetIconIndex('add_software');
  actAddFolder.ImageIndex  := Config.IconsManager.GetIconIndex('add_folder');
  actCut.ImageIndex        := Config.IconsManager.GetIconIndex('cut');
  actCopy.ImageIndex       := Config.IconsManager.GetIconIndex('copy');
  actPaste.ImageIndex      := Config.IconsManager.GetIconIndex('paste');
  actDelete.ImageIndex     := Config.IconsManager.GetIconIndex('delete');
  actProperty.ImageIndex   := Config.IconsManager.GetIconIndex('property');
  miInfoASuite.ImageIndex  := Config.IconsManager.GetIconIndex('help');
  actRunItem.ImageIndex    := Config.IconsManager.GetIconIndex('run');
  //Set Search's ImageIndexes
  //TODO lazarus
  //btnedtSearch.LeftButton.ImageIndex  := Config.IconsManager.GetIconIndex('search_type');
  //btnedtSearch.RightButton.ImageIndex := Config.IconsManager.GetIconIndex('search');
end;

procedure TfrmMain.ShowMainForm(Sender: TObject);
begin
  //From CoolTrayicon source
  if Application.MainForm <> nil then
  begin
    // Restore the app, but don't automatically show its taskbar icon
    // Show application's TASKBAR icon (not the tray icon)
    ShowWindow(Application.Handle, SW_RESTORE);
    Application.Restore;
    // Show the form itself
    if Application.MainForm.WindowState = wsMinimized then
      Application.MainForm.WindowState := wsNormal;    // Override minimized state
    Application.MainForm.Visible := True;
    // Bring the main form (or its modal dialog) to the foreground
    SetForegroundWindow(Application.Handle);
  end;
end;

procedure TfrmMain.HideMainForm;
begin
  if Application.MainForm <> nil then
  begin
    // Hide the form itself (and thus any child windows)
    Application.MainForm.Visible := False;
    { Hide application's TASKBAR icon (not the tray icon). Do this AFTER
        the main form is hidden, or any child windows will redisplay the
        taskbar icon if they are visible. }
    if IsWindowVisible(Application.Handle) then
      ShowWindow(Application.Handle, SW_HIDE);
  end;
end;

procedure TfrmMain.PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);
var
  I: Integer;
  MenuItem : TMenuItem;
begin
  APopupMenu.Clear;

  for I := 0 to AParentMenuItem.Count - 1 do
  begin
    //Create menuitem and set it
    MenuItem := TMenuItem.Create(APopupMenu);
    if Assigned(AParentMenuItem.Items[I].Action) then
      MenuItem.Action := AParentMenuItem.Items[I].Action
    else
      MenuItem.Caption := '-';
    //Add new menu item in APopupMenu
    APopupMenu.Add(MenuItem) ;
  end;
end;

procedure TfrmMain.DoSearchItem(const TreeSearch: TBaseVirtualTree; const Keyword: string;
                                const SearchType: TSearchType);
var
  LauncherSearch: TLauncherSearch;
begin
  TreeSearch.Clear;
  if Length(Keyword) > 0 then
  begin
    TreeSearch.BeginUpdate;
    try
      //Set record LauncherSearch for search
      LauncherSearch.Tree       := TreeSearch;
      LauncherSearch.Keyword    := LowerCase(Keyword);
      LauncherSearch.SearchType := SearchType;
      //Do search using LauncherSearch for parameters
      Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.FindNode, @LauncherSearch, [], True);
    finally
      TreeSearch.EndUpdate;
      TVirtualTreeMethods.Create.CheckVisibleNodePathExe(TreeSearch);
    end;
  end;
end;

function TfrmMain.GetActiveTree: TBaseVirtualTree;
begin
  case pcList.ActivePageIndex of
    PG_LIST   : Result := vstList;
    PG_SEARCH : Result := vstSearch;
    else        Result := nil;
  end;
end;

procedure TfrmMain.tmrCheckItemsTimer(Sender: TObject);
begin
  if Config.ASuiteState = lsNormal then
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(GetActiveTree);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Config.ASuiteState := lsShutdown;
  Close;
end;

procedure TfrmMain.miExportListClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) then
  begin
    TVirtualTreeMethods.Create.RefreshList(GetActiveTree);
    CopyFile(PChar(Config.DBManager.DBFileName), PChar(SaveDialog1.FileName), False);
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TASuiteLogger.Enter('FormClose', Self);
  //Close all process opened by ASuite
  if Config.AutoCloseProcess then
    CloseProcessOpenByASuite;
  Config.ListManager.ExecuteAutorunList(amShutdown);
  //Execute actions on ASuite's shutdown (inside vstList)
  Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.ActionsOnShutdown, nil);
  //Hotkey
  Config.ListManager.HotKeyItemList.Clear;
  TVirtualTreeMethods.Create.RefreshList(nil);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Not(Config.TrayIcon) then
    CanClose := True
  else
    CanClose := ((Config.ASuiteState = lsShutdown) or (SessionEnding));
  //If user close window (not ASuite), hide form and taskbar icon
  if not (CanClose) then
    HideMainForm;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  VSTEvents: TVirtualTreeEvents;
begin
  TASuiteLogger.Enter('FormCreate', Self);
  //Set vstList as MainTree in Config
  Config.MainTree := vstList;
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TdmTrayMenu, dmTrayMenu);
  pcList.ActivePageIndex := PG_LIST;
  //Setup events in vsts
  VSTEvents := TVirtualTreeEvents.Create;
  VSTEvents.SetupVSTList(vstList);
  VSTEvents.SetupVSTSearch(vstSearch);
  //Check read only
  if Config.CheckReadOnlyMode then
  begin
    miOptions1.Enabled  := False;
    miSaveList1.Enabled := False;
  end;
  //Load Database and get icons (only first level of tree)
  Config.LoadList;
  Config.ListManager.ExecuteAutorunList(amStartup);
  //Check missed scheduler tasks
  TScheduler.Create.CheckMissedTasks;
  TVirtualTreeMethods.Create.RefreshList(nil);
  //Get placeholder for edtSearch
  btnedtSearch.TextHint := StringReplace(miSearchName.Caption, '&', '', []);
  PopulatePopUpMenuFromAnother(miEdit, pmWindow.Items);
  //Start threads
  TVirtualTreeMethods.Create.GetAllIcons(vstList, nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Config.Destroy;
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  tmrCheckItems.Enabled := False;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  GetActiveTree.Refresh;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  tmrCheckItems.Enabled := True;
end;

end.
