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

unit Forms.Main;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Forms, Dialogs, Menus,
  ComCtrls, VirtualTrees, UniqueInstance, Kernel.Consts, DataModules.Icons,
  Kernel.BaseMainForm, StdCtrls, {$IFDEF UNIX}VirtualTree.Helper, {$ENDIF}
  Kernel.Enumerations, ExtCtrls, ButtonedEdit, {Actions,} ActnList;

type

  { TfrmMain }

  TfrmMain = class(TBaseMainForm)
    actSepEdit: TAction;
    btnedtSearch: TButtonedEdit;
    miStatistics: TMenuItem;
    MenuItem2: TMenuItem;
    mniOpenFolderItem: TMenuItem;
    mniRunAsAdminItem: TMenuItem;
    mniRunAsItem: TMenuItem;
    mniRunItem: TMenuItem;
    N9: TMenuItem;
    UniqueInstance1: TUniqueInstance;
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
    procedure actSepEditUpdate(Sender: TObject);
    procedure btnedtSearchLeftButtonClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miStatisticsClick(Sender: TObject);
    procedure miImportListClick(Sender: TObject);
    procedure miSaveListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miExportListClick(Sender: TObject);
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
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
      const Parameters: array of AnsiString);
  private
    { Private declarations }
    procedure EmptyClipboard;
    function  GetActiveTree: TBaseVirtualTree;
    procedure PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);
    procedure CloseProcessOpenByASuite;
{$IFDEF UNIX}
    procedure DoDropFiles(Sender: TObject; const FileNames: array of AnsiString);        
{$ENDIF}
  public
    { Public declarations }
    procedure DoSearchItem(const TreeSearch: TBaseVirtualTree; const Keyword: string;
                           const SearchType: TSearchType);
    procedure SetAllIcons;
  end;

var
  frmMain : TfrmMain;

implementation

uses
  Forms.Options, Forms.About, Utility.Misc, Forms.ScanFolder, Clipbrd,
  DataModules.TrayMenu, Forms.ImportList, AppConfig.Main, Utility.System,
  VirtualTree.Methods, Frame.Options.Stats, NodeDataTypes.Base,
  Kernel.Types, NodeDataTypes.Files, VirtualTree.Events, Kernel.Manager,
  Kernel.Logger, mormot.core.log, FileUtil, Kernel.ResourceStrings, Kernel.Instance
  {$IFDEF MSWINDOWS} , JwaWinBase, jwatlhelp32, Windows {$ENDIF};

{$R *.lfm}

procedure TfrmMain.actAddItemUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  TASuiteLogger.Info('Copy nodes into clipboard', []);

  {$IFDEF MSWINDOWS}
  vstList.CopyToClipBoard;
  {$ELSE}
  vstList.FakeCopyToClipBoard;
  {$ENDIF}
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

  {$IFDEF MSWINDOWS}
  vstList.CutToClipBoard;
  {$ELSE}
  vstList.FakeCutToClipBoard;
  {$ENDIF}
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var
  Nodes: TNodeArray;
  Tree: TBaseVirtualTree;
  DeleteNode: Boolean;
  {%H-}log: ISynLog;
begin
  DeleteNode := Config.TVDisableConfirmDelete or AskUserWarningMessage(msgConfirmDeleteItem, []);

  log := TASuiteLogger.Enter('TfrmMain.actDeleteExecute', Self);
  Config.ASuiteState := lsDeleting;
  try
    Tree := GetActiveTree;
    if (Tree.GetFirstSelected <> nil) and (DeleteNode) then
    begin
      Nodes := Tree.GetSortedSelection(true);
      //Delete items
      if ASuiteManager.DBManager.DeleteItems(Tree, Nodes) then
      begin
        //Delete nodes and refresh list
        Tree.DeleteSelectedNodes;
        TVirtualTreeMethods.RefreshList(Tree);
      end;
    end;
  finally
    Config.ASuiteState := lsNormal;
  end;
end;

procedure TfrmMain.actRunItemExecute(Sender: TObject);
begin
  TVirtualTreeMethods.ExecuteSelectedNodes(GetActiveTree, TRunMode(TAction(Sender).Tag), False);
end;

procedure TfrmMain.actAddItem(Sender: TObject);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.AddChildNodeByGUI(vstList, vstList.GetFirstSelected,
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
  res: Boolean = False;
begin
  TASuiteLogger.Info('Paste clipboard content in ASuite', []);

  Tree := TVirtualStringTree(GetActiveTree);
  if Assigned(Tree) then
  begin
    NodeData := TVirtualTreeMethods.GetNodeItemData(Tree.GetFirstSelected, Tree);
    if Assigned(NodeData) then
    begin
      if NodeData.IsCategoryItem then
        Tree.DefaultPasteMode := amAddChildLast
      else
        Tree.DefaultPasteMode := amInsertAfter;
      end
    else
      Tree.DefaultPasteMode := amAddChildLast; 

    {$IFDEF MSWINDOWS}
    res := Tree.PasteFromClipboard;
    {$ELSE}
    res := vstList.FakePasteFromClipboard;
    {$ENDIF}

    if res then
    begin
      Tree.Expanded[Tree.GetFirstSelected] := True;
      TVirtualTreeMethods.RefreshList(Tree);
    end;
  end;
end;

procedure TfrmMain.actPasteUpdate(Sender: TObject);
begin               
  if Assigned(Sender) then
  {$IFDEF MSWINDOWS}
    TAction(Sender).Enabled := IsFormatInClipBoard(CF_VIRTUALTREE) and (GetActiveTree = vstList);
  {$ELSE}
    TAction(Sender).Enabled := (Length(vstList.GetSortedCutCopySet(True)) > 0) and (GetActiveTree = vstList);
  {$ENDIF}

end;

procedure TfrmMain.actPropertyExecute(Sender: TObject);
begin
  TVirtualTreeMethods.ShowItemProperty(Self, GetActiveTree, GetActiveTree.GetFirstSelected);
end;

procedure TfrmMain.actRunItemUpdate(Sender: TObject);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
  I: Integer;
begin
  Nodes := GetActiveTree.GetSortedSelection(True);

  TAction(Sender).Enabled := False;

  for I := Low(Nodes) to High(Nodes) do
  begin
    NodeData := TVirtualTreeMethods.GetNodeItemData(Nodes[I], GetActiveTree);

    if not(NodeData.IsSeparatorItem) then
      TAction(Sender).Enabled := True;

    if ((TAction(Sender).Tag = 1) or (TAction(Sender).Tag = 2)) and (NodeData.IsFileItem) then
      TAction(Sender).Enabled := IsExecutableFile(TvFileNodeData(NodeData).PathAbsoluteFile);

    if (TAction(Sender).Tag = 3) then
    begin
      if NodeData.IsFileItem then
      begin
        if IsValidURLProtocol(TvFileNodeData(NodeData).PathAbsoluteFile) then
          TAction(Sender).Enabled := False
      end
      else
        TAction(Sender).Enabled := False;
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
  TVirtualTreeMethods.RefreshList(vstList);
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
      NodeData := TVirtualTreeMethods.GetNodeItemData(Nodes[I], GetActiveTree);
      if NodeData.IsCategoryItem then
        TAction(Sender).Enabled := True;
    end;
  end;
end;

procedure TfrmMain.actSortListExecute(Sender: TObject);
begin
  vstList.SortTree(-1, sdAscending);

  TVirtualTreeMethods.RefreshList(vstList);
end;

procedure TfrmMain.actSortListUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
  TAction(Sender).Enabled := (vstList.RootNode.ChildCount > 1) and (GetActiveTree = vstList);
end;

procedure TfrmMain.btnedtSearchChange(Sender: TObject);
begin
  if Config.SearchAsYouType then
  begin
    if btnedtSearch.Text <> '' then
      btnedtSearch.RightButton.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('cancel')
    else
      btnedtSearch.RightButton.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('search');

    DoSearchItem(vstSearch, btnedtSearch.Text, TSearchType(GetCheckedMenuItem(pmSearch).Tag));
  end;
end;

procedure TfrmMain.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of AnsiString);
var
  I: Integer;
begin
  TASuiteLogger.Info('Started another instance', []);

  //Parse parameters
  for I := 0 to ParamCount - 1 do
    ASuiteInstance.HandleParam(Parameters[I], False);

  if Config.ShowGraphicMenuAnotherInstance then
  begin                        
    Application.Restore;
    dmTrayMenu.ShowGraphicMenu;
  end
  else
    ShowMainForm(Sender);
end;

procedure TfrmMain.EmptyClipboard;
begin                  
  TASuiteLogger.Enter('Clearing clipboard', Self);

  {$IFDEF MSWINDOWS}
  if IsFormatInClipBoard(CF_VIRTUALTREE) then
  begin
    Windows.OpenClipboard(0);
    try
      Windows.EmptyClipboard;
    finally
      Windows.CloseClipboard;
    end;
  end;
  {$ENDIF}

  Clipboard.Clear;
end;
           
{$IFDEF UNIX}
procedure TfrmMain.DoDropFiles(Sender: TObject;
  const FileNames: array of AnsiString);
var
  I: Integer;
begin
  if pcList.ActivePageIndex <> PG_LIST then
    Exit;

  //Filenames is not only files
  for I := 0 to Length(Filenames) - 1 do
  begin
    if FileExists(FileNames[I]) or DirectoryExists(FileNames[I]) then
      TVirtualTreeMethods.AddNodeByPathFile(vstList, nil, FileNames[I], amAddChildLast)
    else
      TVirtualTreeMethods.AddNodeByText(vstList, nil, FileNames[I], amAddChildLast)
  end;
end;
{$ENDIF}

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
begin
  if ASuiteInstance.SaveList(True) then
    ShowMessageEx(msgSaveCompleted)
  else
    ShowMessageEx(msgErrSave,true);
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
  TVirtualTreeMethods.RefreshList(GetActiveTree);
end;

procedure TfrmMain.miInfoASuiteClick(Sender: TObject);
begin
  TfrmAbout.Execute(Self);
end;

procedure TfrmMain.miOptionsClick(Sender: TObject);
begin
  TfrmOptions.Execute(Self);
end;

procedure TfrmMain.actSepEditUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
end;

procedure TfrmMain.btnedtSearchLeftButtonClick(Sender: TObject);
begin
  pmSearch.PopUp;
end;

procedure TfrmMain.miStatisticsClick(Sender: TObject);
begin
  TfrmOptions.Execute(Self, TfrmStatsOptionsPage);
end;

procedure TfrmMain.mniScanFolderClick(Sender: TObject);
begin
  TfrmScanFolder.Execute(Self);
  TVirtualTreeMethods.RefreshList(GetActiveTree);
  Config.SaveConfig;
end;

procedure TfrmMain.pcListChange(Sender: TObject);
begin
  //Clear search when user click on tab search
  if pcList.ActivePageIndex = PG_SEARCH then
  begin
    vstSearch.Clear;
    btnedtSearch.Text := '';
    btnedtSearch.SetFocus;
  end;
  TVirtualTreeMethods.CheckVisibleNodePathExe(GetActiveTree);
end;

procedure TfrmMain.SetAllIcons;
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TfrmMain.SetAllIcons', Self);

  //Set IcoImages
  //Set submenuimages to three MainMenu's subitems
  miFile.SubMenuImages := dmImages.ilLargeIcons;
  miFile.SubMenuImagesWidth := ICON_SIZE_SMALL;

  miEdit.SubMenuImages := dmImages.ilLargeIcons;
  miEdit.SubMenuImagesWidth := ICON_SIZE_SMALL;

  miHelp.SubMenuImages := dmImages.ilLargeIcons;
  miHelp.SubMenuImagesWidth := ICON_SIZE_SMALL;

  pmSearch.Images      := dmImages.ilLargeIcons;
  pmSearch.ImagesWidth := ICON_SIZE_SMALL;

  pmWindow.Images      := dmImages.ilLargeIcons;
  pmWindow.ImagesWidth := ICON_SIZE_SMALL;

  pcList.Images        := dmImages.ilLargeIcons;
  pcList.ImagesWidth := ICON_SIZE_SMALL;

  btnedtSearch.RightButton.Images := dmImages.ilLargeIcons;
  btnedtSearch.RightButton.ImagesWidth := ICON_SIZE_SMALL;

  btnedtSearch.LeftButton.Images := dmImages.ilLargeIcons;
  btnedtSearch.LeftButton.ImagesWidth := ICON_SIZE_SMALL;

  //Set pcList tabs' ImageIndexes
  tbList.ImageIndex    := ASuiteManager.IconsManager.GetIconIndex('tree_list');
  tbSearch.ImageIndex  := ASuiteManager.IconsManager.GetIconIndex('search');

  //Set MainMenu's ImageIndexes
  miSaveList1.ImageIndex   := ASuiteManager.IconsManager.GetIconIndex('save');
  miOptions1.ImageIndex    := ASuiteManager.IconsManager.GetIconIndex('options');
  actAddCat.ImageIndex     := ASuiteManager.IconsManager.GetIconIndex('add_category');
  actAddSoftware.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('add_software');
  actAddFolder.ImageIndex  := ASuiteManager.IconsManager.GetIconIndex('add_folder');
  actCut.ImageIndex        := ASuiteManager.IconsManager.GetIconIndex('cut');
  actCopy.ImageIndex       := ASuiteManager.IconsManager.GetIconIndex('copy');
  actPaste.ImageIndex      := ASuiteManager.IconsManager.GetIconIndex('paste');
  actDelete.ImageIndex     := ASuiteManager.IconsManager.GetIconIndex('delete');
  actProperty.ImageIndex   := ASuiteManager.IconsManager.GetIconIndex('property');
  miInfoASuite.ImageIndex  := ASuiteManager.IconsManager.GetIconIndex('help');
  actRunItem.ImageIndex    := ASuiteManager.IconsManager.GetIconIndex('run');

  //Set Search's ImageIndexes
  btnedtSearch.LeftButton.ImageIndex  := ASuiteManager.IconsManager.GetIconIndex('search_type');
  btnedtSearch.RightButton.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('search');
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
      MenuItem.Caption := cLineCaption;

    //TODO: Add other levels

    //Add new menu item in APopupMenu
    APopupMenu.Add(MenuItem) ;
  end;
end;

procedure TfrmMain.CloseProcessOpenByASuite;
{$IFDEF MSWINDOWS}
var
  hSnapShot, hProcess : THandle;
  ProcInfo  : TProcessEntry32;
  ContinueLoop: Boolean;
const
  PROCESS_TERMINATE = $0001;
{$ENDIF}
begin
  //TODO: Add linux method
  {$IFDEF MSWINDOWS}
  TASuiteLogger.Info('Close processes opened by ASuite', []);
  hSnapShot   := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  //Check processes
  if (hSnapShot <> THandle(-1)) then
  begin
    ProcInfo.dwSize := SizeOf(ProcInfo);
    ContinueLoop := Process32First(hSnapshot, ProcInfo);
    while ContinueLoop do
    begin
      //Close process with ParentID same as ASuite PID
      if (ProcInfo.th32ParentProcessID = GetCurrentProcessId) and (ProcInfo.szExeFile <> LowerCase('Rundll32.exe')) then
      begin
        hProcess := OpenProcess(PROCESS_TERMINATE, False, ProcInfo.th32ProcessID);
        TerminateProcess(hProcess, 0);
        FileClose(hProcess);
      end;

      ContinueLoop := Process32Next(hSnapShot, ProcInfo);
    end;
  end;
  FileClose(hSnapShot);
  {$ENDIF}
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
      ASuiteInstance.MainTree.IterateSubtree(nil, TVirtualTreeMethods.FindNode, @LauncherSearch, [], True);
    finally
      TreeSearch.EndUpdate;
      TVirtualTreeMethods.CheckVisibleNodePathExe(TreeSearch);
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
    TVirtualTreeMethods.CheckVisibleNodePathExe(GetActiveTree);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  CloseASuite(False);
end;

procedure TfrmMain.miExportListClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) then
  begin
    TVirtualTreeMethods.RefreshList(GetActiveTree);

    if SaveDialog1.FileName <> '' then
      FileUtil.CopyFile(ASuiteManager.DBManager.DBFileName, SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TfrmMain.FormClose', Self);

  //Clear clipboard before closing asuite (prevent fake memory leak)
  EmptyClipboard;

  //Close all process opened by ASuite
  if Config.AutoCloseProcess then
    CloseProcessOpenByASuite;

  ASuiteManager.ListManager.ExecuteAutorunList(amShutdown);

  //Execute actions on ASuite's shutdown (inside vstList)
  ASuiteInstance.MainTree.IterateSubtree(nil, TVirtualTreeMethods.ActionsOnShutdown, nil);

  //Hotkey
  ASuiteManager.ListManager.HotKeyItemList.Clear;

  Config.SaveConfig;

  TVirtualTreeMethods.RefreshList(nil);
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
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TfrmMain.MainFormCreate', Self);

  {$IFDEF UNIX}
  Self.AllowDropFiles := True;
  Self.OnDropFiles := DoDropFiles;

  //Disable right click select for context menu issues
  Self.vstList.TreeOptions.SelectionOptions := Self.vstList.TreeOptions.SelectionOptions - [toRightClickSelect];
  {$ENDIF}

  //Set vstList as MainTree in Config
  ASuiteInstance.MainTree := vstList;
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TdmTrayMenu, dmTrayMenu);
  pcList.ActivePageIndex := PG_LIST;

  //Setup events in vsts
  ASuiteInstance.VSTEvents.SetupVSTList(vstList);
  ASuiteInstance.VSTEvents.SetupVSTSearch(vstSearch);

  //Load Database and get icons (only first level of tree)
  Config.LoadConfig;
  ASuiteInstance.LoadList;
  ASuiteManager.ListManager.ExecuteAutorunList(amStartup);

  //Check missed scheduler tasks
  ASuiteInstance.Scheduler.CheckMissedTasks;
  TVirtualTreeMethods.RefreshList(nil);

  PopulatePopUpMenuFromAnother(miEdit, pmWindow.Items);

  //Start threads
  TVirtualTreeMethods.GetAllIcons(vstList, nil);
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  tmrCheckItems.Enabled := False;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  GetActiveTree.Refresh;
  Config.Changed := True;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  tmrCheckItems.Enabled := True;
end;

end.
