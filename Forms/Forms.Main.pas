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
  Database.Manager;

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
    miAddCat1: TMenuItem;
    miAddSw1: TMenuItem;
    miDelete1: TMenuItem;
    N8: TMenuItem;
    miProperty1: TMenuItem;
    pmWindow: TPopupMenu;
    miAddCat2: TMenuItem;
    miAddSw2: TMenuItem;
    miDelete2: TMenuItem;
    N6: TMenuItem;
    miProperty2: TMenuItem;
    miAddFolder2: TMenuItem;
    miAddFolder1: TMenuItem;
    miSaveList1: TMenuItem;
    miRunSelectedSw: TMenuItem;
    miOpenFolderSw: TMenuItem;
    N9: TMenuItem;
    miAddSeparator2: TMenuItem;
    miAddSeparator1: TMenuItem;
    N3: TMenuItem;
    miSortList: TMenuItem;
    miExportList: TMenuItem;
    N4: TMenuItem;
    N1: TMenuItem;
    SaveDialog1: TSaveDialog;
    miSortItems: TMenuItem;
    N5: TMenuItem;
    miCopy2: TMenuItem;
    miCut2: TMenuItem;
    miPaste2: TMenuItem;
    N10: TMenuItem;
    miN11: TMenuItem;
    miCut1: TMenuItem;
    miCopy1: TMenuItem;
    miPaste1: TMenuItem;
    pmSearch: TPopupMenu;
    miSearchName: TMenuItem;
    miSearchExePath: TMenuItem;
    miSearchIconPath: TMenuItem;
    miSearchWorkingDirPath: TMenuItem;
    miSearchParameters: TMenuItem;
    edtSearch: TEdit;
    miRunAs: TMenuItem;
    miRunAsAdmin: TMenuItem;
    tmScheduler: TTimer;
    mniScanFolder: TMenuItem;
    DKLanguageController1: TDKLanguageController;
    procedure miOptionsClick(Sender: TObject);
    procedure miStatisticsClick(Sender: TObject);
    procedure pcListChange(Sender: TObject);
    procedure miImportListClick(Sender: TObject);
    procedure miSaveListClick(Sender: TObject);
    procedure AddFolder(Sender: TObject);
    procedure AddSoftware(Sender: TObject);
    procedure AddCategory(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pmWindowPopup(Sender: TObject);
    procedure miAddSeparator2Click(Sender: TObject);
    procedure miSortListClick(Sender: TObject);
    procedure miExportListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miSortItemsClick(Sender: TObject);
    procedure miCopy2Click(Sender: TObject);
    procedure miPaste2Click(Sender: TObject);
    procedure miCut2Click(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miInfoASuiteClick(Sender: TObject);
    procedure btnedtSearchRightButtonClick(Sender: TObject);
    procedure ChangeSearchTextHint(Sender: TObject);
    procedure btnedtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure tmSchedulerTimer(Sender: TObject);
    procedure mniScanFolderClick(Sender: TObject);
  private
    { Private declarations }
    function  GetActiveTree: TBaseVirtualTree;
    procedure RunStartupProcess;
    procedure RunShutdownProcess;
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
  Utility.TreeView, Frame.Options.Stats, NodeDataTypes.Base,
  NodeDataTypes.Custom, Kernel.Types,
  VirtualTree.Events;

{$R *.dfm}

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

procedure TfrmMain.AddCategory(Sender: TObject);
begin
  CreateListItem(vstList,vtdtCategory);
end;

procedure TfrmMain.AddFolder(Sender: TObject);
begin
  CreateListItem(vstList,vtdtFolder);
end;

procedure TfrmMain.AddSoftware(Sender: TObject);
begin
  CreateListItem(vstList,vtdtFile);
end;

procedure TfrmMain.miAddSeparator2Click(Sender: TObject);
begin
  CreateListItem(vstList,vtdtSeparator);
end;

procedure TfrmMain.miCopy2Click(Sender: TObject);
begin
  vstList.CopyToClipBoard;
end;

procedure TfrmMain.miCut2Click(Sender: TObject);
begin
  vstList.CutToClipBoard;
end;

procedure TfrmMain.miImportListClick(Sender: TObject);
begin
  TfrmImportList.Execute(Self);
  RefreshList(GetActiveTree);
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
  RefreshList(GetActiveTree);
end;

procedure TfrmMain.miPaste2Click(Sender: TObject);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := GetNodeItemData(vstList.FocusedNode, vstList);
  if Assigned(NodeData) then
  begin
    if NodeData.DataType = vtdtCategory then
      vstList.DefaultPasteMode := amAddChildLast
    else
      vstList.DefaultPasteMode := amInsertAfter;
    end
  else
    vstList.DefaultPasteMode := amAddChildLast;
  vstList.PasteFromClipboard;
  vstList.Expanded[vstList.FocusedNode] := True;
  RefreshList(vstList);
end;

procedure TfrmMain.miSortItemsClick(Sender: TObject);
begin
  vstList.Sort(vstList.GetFirstSelected,0,sdAscending,True);
  RefreshList(vstList);
end;

procedure TfrmMain.miSortListClick(Sender: TObject);
begin
  vstList.SortTree(0,sdAscending,True);
  RefreshList(vstList);
end;

procedure TfrmMain.pcListChange(Sender: TObject);
var
  ActiveTab : Boolean;
begin
  ActiveTab := pcList.ActivePageIndex = PG_LIST;
  //Set property Visible for some components
  miEdit.Visible          := ActiveTab;
  miSortItems.Visible     := ActiveTab;
  miAddCat2.Visible       := ActiveTab;
  miAddSw2.Visible        := ActiveTab;
  miAddFolder2.Visible    := ActiveTab;
  miAddSeparator2.Visible := ActiveTab;
  miDelete2.Visible       := ActiveTab;
  miCut2.Visible          := ActiveTab;
  miCopy2.Visible         := ActiveTab;
  miPaste2.Visible        := ActiveTab;
end;

procedure TfrmMain.pmWindowPopup(Sender: TObject);
var
  NodeData : PBaseData;
  Point    : TPoint;
  HitInfo  : ThitInfo;
begin
  GetCursorPos(Point);
  Point    := vstList.ScreenToClient(Point);
  vstList.GetHitTestInfoAt(Point.X,Point.Y,true,HitInfo);
  //If user clicks on empty area of vstList
  if Not(hiOnItem in hitinfo.HitPositions) then
  begin
    //Set nil vstList.FocusedNode
    vstList.Selected[vstList.FocusedNode] := False;
    vstList.FocusedNode := nil;
  end;
  //Enable/disable item window menu
  if (Assigned(vstList.FocusedNode) and (pcList.ActivePageIndex = PG_LIST)) or
     (Assigned(vstSearch.FocusedNode) and (pcList.ActivePageIndex = PG_Search)) then
  begin
    NodeData := GetNodeDataEx(GetActiveTree.FocusedNode, GetActiveTree);
    miProperty2.Enabled     := True;
    miRunSelectedSw.Enabled := (NodeData.Data.DataType <> vtdtSeparator);
    miRunAs.Enabled         := (NodeData.Data.DataType <> vtdtSeparator);
    miRunAsAdmin.Enabled    := (NodeData.Data.DataType <> vtdtSeparator);
    miOpenFolderSw.Enabled  := (NodeData.Data.DataType in [vtdtFile,vtdtFolder]);
  end
  else begin
    miRunSelectedSw.Enabled := False;
    miRunAs.Enabled         := False;
    miRunAsAdmin.Enabled    := False;
    miOpenFolderSw.Enabled  := False;
    miProperty2.Enabled     := False;
  end;
  miPaste2.Enabled := IsFormatInClipBoard(CF_VIRTUALTREE);
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
      Config.MainTree.IterateSubtree(nil, TIterateSubtreeProcs.FindNode, @LauncherSearch, [], True);
    finally
      TreeSearch.EndUpdate;
      CheckVisibleNodePathExe(TreeSearch);
      //TODO: Fix it
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

procedure TfrmMain.miEditClick(Sender: TObject);
begin
  miPaste1.Enabled := IsFormatInClipBoard(CF_VIRTUALTREE);
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
    RefreshList(GetActiveTree);
    CopyFile(PChar(Config.DBManager.DBFileName),PChar(SaveDialog1.FileName),false)
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RunShutdownProcess;
  //Execute actions on ASuite's shutdown (inside vstList)
  Config.MainTree.IterateSubtree(nil, TIterateSubTreeProcs.ActionsOnShutdown, nil);
  //Hotkey
  Config.ListManager.HotKeyItemList.Clear;
  RefreshList(nil);
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
  //TODO: Fix it
//ImagesDM.GetChildNodesIcons(vstList, nil, vstList.RootNode);
  RunStartupProcess;
  RefreshList(nil);
  //Get placeholder for edtSearch
  edtSearch.TextHint := StringReplace(miSearchName.Caption, '&', '', []);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Config.Destroy;
end;

end.
