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

unit Forms.Main;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ComCtrls, VirtualTrees, ActiveX, Kernel.Consts, DataModules.Images,
  Kernel.BaseMainForm, StdCtrls, Buttons, System.UITypes,
  Kernel.Enumerations, Vcl.ExtCtrls, System.DateUtils, XMLDoc, DKLang, Lists.Manager,
  Database.Manager, System.Actions, Vcl.ActnList;

type

  { TfrmMain }

  TfrmMain = class(TBaseMainForm)
    miCheckUpdates: TMenuItem;
    miStatistics: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    sbtnSearch: TSpeedButton;
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
    edtSearch: TEdit;
    tmScheduler: TTimer;
    mniScanFolder: TMenuItem;
    DKLanguageController1: TDKLanguageController;
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
    procedure tmSchedulerTimer(Sender: TObject);
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
  private
    { Private declarations }
    function  GetActiveTree: TBaseVirtualTree;
    procedure RunStartupProcess;
    procedure RunShutdownProcess;
    procedure PopulatePopUpMenuFromAnother(APopupMenu: TPopupMenu; AParentMenuItem: TMenuItem);
  public
    { Public declarations }
    procedure ShowMainForm(Sender: TObject);
    procedure HideMainForm;
    procedure DoSearchItem(const TreeSearch: TBaseVirtualTree; const Keyword: string;
                           const SearchType: TSearchType);
  end;

var
  frmMain : TfrmMain;

implementation

uses
  TypInfo, Forms.Options, Forms.About, Utility.Misc, Forms.ScanFolder,
  DataModules.TrayMenu, Forms.ImportList, AppConfig.Main,
  VirtualTree.Methods, Frame.Options.Stats, NodeDataTypes.Base,
  NodeDataTypes.Custom, Kernel.Types,
  VirtualTree.Events;

{$R *.dfm}

procedure TfrmMain.actAddItemUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  vstList.CopyToClipBoard;
end;

procedure TfrmMain.actCutCopyDeleteUpdate(Sender: TObject);
var
  Nodes: TNodeArray;
begin
  Nodes := GetActiveTree.GetSortedSelection(True);
  TAction(Sender).Enabled := Length(Nodes) > 0;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  vstList.CutToClipBoard;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var
  Nodes: TNodeArray;
  I: Integer;
  Tree: TBaseVirtualTree;
begin
  Tree := GetActiveTree;
  if (Tree.GetFirstSelected <> nil) and (MessageDlg((DKLangConstW('msgConfirm')),mtWarning, [mbYes,mbNo], 0) = mrYes) then
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
    end;
  end;
end;

procedure TfrmMain.actSortCatItemsExecute(Sender: TObject);
var
  Nodes: TNodeArray;
  NodeData: TvBaseNodeData;
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

procedure TfrmMain.btnedtSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    btnedtSearchRightButtonClick(Sender);
end;

procedure TfrmMain.btnedtSearchRightButtonClick(Sender: TObject);
begin
  DoSearchItem(vstSearch, edtSearch.Text, TSearchType(GetCheckedMenuItem(pmSearch).Tag));
end;

procedure TfrmMain.miSaveListClick(Sender: TObject);
begin;
  if Config.SaveList(True) then
    ShowMessageEx(DKLangConstW('msgSaveCompleted'))
  else
    ShowMessageEx(DKLangConstW('msgErrSave'),true);
end;

procedure TfrmMain.ChangeSearchTextHint(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    //Set new placeholder and SearchType
    edtSearch.TextHint := StringReplace((Sender as TMenuItem).Caption, '&', '', []);
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
  TVirtualTreeMethods.Create.CheckVisibleNodePathExe(GetActiveTree);
  //Clear search when user click on tab search
  if pcList.ActivePageIndex = PG_SEARCH then
  begin
    vstSearch.Clear;
    edtSearch.Text := '';
  end;
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

procedure TfrmMain.PopulatePopUpMenuFromAnother(APopupMenu: TPopupMenu; AParentMenuItem: TMenuItem);
var
  I: Integer;
  MenuItem : TMenuItem;
begin
  APopupMenu.Items.Clear;

  for I := 0 to AParentMenuItem.Count - 1 do
  begin
    //Create menuitem and set it
    MenuItem         := TMenuItem.Create(APopupMenu);
    if Assigned(AParentMenuItem.Items[I].Action) then
      MenuItem.Action  := AParentMenuItem.Items[I].Action
    else
      MenuItem.Caption := '-';
    //Add new menu item in APopupMenu
    APopupMenu.Items.Add(MenuItem) ;
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
      //TODO: Fix it (dmImages)
//ImagesDM.GetChildNodesIcons(TreeSearch, TreeSearch.RootNode, isAny);
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

procedure TfrmMain.RunStartupProcess;
var
  NodeData : TvCustomRealNodeData;
  RunMode  : TRunMode;
  I : Integer;
begin
  //Autorun - Execute software
  if (Config.Autorun) then
  begin
    for I := 0 to Config.ListManager.StartupItemList.Count - 1 do
    begin
      NodeData := Config.ListManager.StartupItemList[I];
      //Set RunMode
      RunMode := rmAutorun;
      if (NodeData.Autorun = atSingleInstance) then
        RunMode := rmAutorunSingleInstance;
      //Start process
//      ExecuteItem(Config.MainTree, NodeData, RunMode);
    end;
  end;
end;

procedure TfrmMain.RunShutdownProcess;
var
  NodeData: TvCustomRealNodeData;
  I : Integer;
begin
  //Autorun - Execute software
  if (Config.Autorun) then
  begin
    for I := 0 to Config.ListManager.ShutdownItemList.Count - 1 do
    begin
      NodeData := Config.ListManager.ShutdownItemList[I];
      //Start process
//      ExecuteItem(Config.MainTree, NodeData, rmAutorun);
    end;
  end;
end;

procedure TfrmMain.tmrCheckItemsTimer(Sender: TObject);
begin
  if Config.ASuiteState = lsNormal then
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(GetActiveTree);
end;

procedure TfrmMain.tmSchedulerTimer(Sender: TObject);
var
  NodeData : TvCustomRealNodeData;
  I        : Integer;
  schTime, NowDateTime: TDateTime;
begin
  //TODO: Rewrite this code (see TSchedulerItemsList.CheckMissedTasks)
  if (Config.ASuiteState = lsStartUp) or (Config.ASuiteState = lsShutdown) then
    Exit;
  NowDateTime := RecodeMilliSecond(Now,0);
  schTime     := NowDateTime;
  //Check scheduler list to know which items to run
  for I := 0 to Config.ListManager.SchedulerItemList.Count - 1 do
  begin
    if Assigned(Config.ListManager.SchedulerItemList[I]) then
    begin
      NodeData := Config.ListManager.SchedulerItemList[I];
      //Compare time and/or date based of scheduler mode
      case NodeData.SchMode of
        smDisabled: schTime := 0;
        smOnce: schTime := NodeData.SchDateTime;
        smHourly:
        begin
          //Run software every hour
          schTime := RecodeMinute(NowDateTime,0);
          schTime := RecodeSecond(schTime,0);
        end;
        smDaily:
        begin
          //Run software every day (user choose time, hour and minute)
          schTime := RecodeYear(NodeData.SchDateTime, YearOf(NowDateTime));
          schTime := RecodeMonth(schTime, MonthOf(NowDateTime));
          schTime := RecodeDay(schTime, DayOf(NowDateTime));
        end;
      end;
      //If is its turn, run item
      if (CompareDateTime(NowDateTime, schTime) = 0) and (NodeData.SchMode <> smDisabled) then
      begin
        //Start process
//        ExecuteItem(Config.MainTree, NodeData, rmNormal);
      end;
    end;
  end;
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
    CopyFile(PChar(Config.DBManager.DBFileName),PChar(SaveDialog1.FileName),false)
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RunShutdownProcess;
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
  //TODO: Fix it (dmImages)
//ImagesDM.GetChildNodesIcons(vstList, nil, vstList.RootNode);
  RunStartupProcess;
  TVirtualTreeMethods.Create.RefreshList(nil);
  //Get placeholder for edtSearch
  edtSearch.TextHint := StringReplace(miSearchName.Caption, '&', '', []);
  PopulatePopUpMenuFromAnother(pmWindow, miEdit);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Config.Destroy;
end;

procedure TfrmMain.FormHide(Sender: TObject);
begin
  tmrCheckItems.Enabled := False;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  tmrCheckItems.Enabled := True;
end;

end.
