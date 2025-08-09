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
  Kernel.BaseMainForm, {$IFDEF UNIX}VirtualTree.Helper, {$ENDIF}
  Kernel.Enumerations, ExtCtrls, ButtonedEdit, {Actions,} ActnList,
  AppConfig.Observer, AppConfig.Main, Utility.SearchController,
  Utility.ClipboardController, Utility.RunController, Utility.SortController;

type

  { TfrmMain }

  TfrmMain = class(TBaseMainForm, IConfigObserver)
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
    procedure FormChangeBounds(Sender: TObject);
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
    procedure btnedtSearchChange(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
      const Parameters: array of AnsiString);
  private
    { Private declarations }
    FRestoringSettings: Boolean;
    // Controllers
    FSearch: TSearchController;
    FClipboard: TClipboardController;
    FRun: TRunController;
    FSort: TSortController;
    procedure EmptyClipboard;
    function  GetActiveTree: TBaseVirtualTree;
    procedure PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);
    procedure CloseProcessOpenByASuite;
{$IFDEF UNIX}
    procedure DoDropFiles(Sender: TObject; const FileNames: array of AnsiString);        
{$ENDIF}
    procedure ConfigChanged(const PropertyName: string);
    procedure RestoreSettings;
    // ConfigChanged helpers
    function  ShouldApply(const Prop, Target: string): Boolean; inline;
    procedure ApplyHoldSize(const PropertyName: string);
    procedure ApplyAlwaysOnTop(const PropertyName: string);
    procedure ApplyCustomTitle(const PropertyName: string);
    procedure ApplyHideTabSearch(const PropertyName: string);
    procedure ApplySearchColumns(const PropertyName: string);
    procedure ApplyTreeAutoExpand(const PropertyName: string);
    procedure ApplyTreeFont(const PropertyName: string);
    procedure ApplyTreeBackgroundFlag(const PropertyName: string);
    procedure ApplySmallIconSize(const PropertyName: string);
    procedure ApplyAfterUpdateConfig(const PropertyName: string);
    procedure ApplyBoundsGroup(const PropertyName: string);
    procedure ApplyBoundsIndividual(const PropertyName: string);
  public
    { Public declarations }
    procedure DoSearchItem(const TreeSearch: TBaseVirtualTree; const Keyword: string;
                           const SearchType: TSearchType);
    procedure SetAllIcons;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  frmMain : TfrmMain;

implementation

uses
  Forms.Options, Forms.About, Utility.Misc, Forms.ScanFolder, Clipbrd,
  DataModules.TrayMenu, Forms.ImportList, Utility.System, LCLTranslator,
  VirtualTree.Methods, Frame.Options.Stats, NodeDataTypes.Base, Utility.FileFolder,
  Kernel.Types, NodeDataTypes.Files, Kernel.Manager, VirtualTrees.Types,
  Kernel.Logger, mormot.core.log, FileUtil, Kernel.ResourceStrings, Kernel.Instance,
  VirtualTrees.ClipBoard, Forms.GraphicMenu
  {$IFDEF MSWINDOWS} , jwatlhelp32, Windows {$ENDIF},
  Utility.MenuUtils;

{$R *.lfm}

procedure TfrmMain.actAddItemUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (GetActiveTree = vstList);
end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  if Assigned(FClipboard) then
    FClipboard.DoCopy;
end;

procedure TfrmMain.actCutCopyDeleteUpdate(Sender: TObject);
var
  Nodes: TNodeArray;
  Tree: TBaseVirtualTree;
begin
  TAction(Sender).Enabled := False;

  //TODO: Why Editing!?
  Tree := GetActiveTree;
  if (Tree = nil) then
    Exit;

  if not (tsEditing in Tree.TreeStates) then
  begin
    Nodes := Tree.GetSortedSelection(True);
    TAction(Sender).Enabled := Length(Nodes) > 0;
  end;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  if Assigned(FClipboard) then
    FClipboard.DoCut;
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
  if Assigned(FRun) then
    FRun.ExecuteRun(GetActiveTree, TRunMode(TAction(Sender).Tag));
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
begin
  if Assigned(FClipboard) then
    FClipboard.DoPaste;
end;

procedure TfrmMain.actPasteUpdate(Sender: TObject);
var
  Enabled: Boolean;
begin               
  if Assigned(Sender) and Assigned(FClipboard) then
  begin
    Enabled := False;
    TClipboardController(FClipboard).UpdatePasteEnabled(Enabled, GetActiveTree = vstList);
    TAction(Sender).Enabled := Enabled;
  end;
end;

procedure TfrmMain.actPropertyExecute(Sender: TObject);
begin
  TVirtualTreeMethods.ShowItemProperty(Self, GetActiveTree, GetActiveTree.GetFirstSelected);
end;

procedure TfrmMain.actRunItemUpdate(Sender: TObject);
begin
  if Assigned(FRun) then
    FRun.UpdateRunAction(TAction(Sender), GetActiveTree);
end;

procedure TfrmMain.actSortCatItemsExecute(Sender: TObject);
begin
  if Assigned(FSort) then
    FSort.SortSelectedCategories(GetActiveTree, vstList);
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
  if Assigned(FSort) then
    FSort.SortListTree(vstList);
end;

procedure TfrmMain.actSortListUpdate(Sender: TObject);
begin
  if Assigned(FSort) then
    FSort.UpdateSortListAction(TAction(Sender), vstList, GetActiveTree);
end;

procedure TfrmMain.btnedtSearchChange(Sender: TObject);
begin
  if Assigned(FSearch) then
    FSearch.OnTextChange(Sender);
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
  if Assigned(FClipboard) then
    FClipboard.EmptyClipboard;
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
  if Assigned(FSearch) then
    FSearch.OnKeyPress(Sender, Key);
end;

procedure TfrmMain.btnedtSearchRightButtonClick(Sender: TObject);
begin
  if Assigned(FSearch) then
    FSearch.OnRightButtonClick(Sender);
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
  if Assigned(FSearch) then
    FSearch.OnLeftButtonClick(Sender);
end;

procedure TfrmMain.FormChangeBounds(Sender: TObject);
begin
  if FRestoringSettings then
    Exit;

  GetActiveTree.Refresh;

  // Use batching to avoid redundant observer notifications when updating form bounds
  Config.BeginUpdate;
  try
    Config.MainFormLeft   := Self.Left;
    Config.MainFormTop    := Self.Top;
    Config.MainFormWidth  := ScaleFormTo96(Self.Width);
    Config.MainFormHeight := ScaleFormTo96(Self.Height);
  finally
    Config.EndUpdate;
  end;

  Config.Changed := True;
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
  miFile.SubMenuImages := dmImages.ilIcons;
  miFile.SubMenuImagesWidth := ICON_SIZE_SMALL;

  miEdit.SubMenuImages := dmImages.ilIcons;
  miEdit.SubMenuImagesWidth := ICON_SIZE_SMALL;

  miHelp.SubMenuImages := dmImages.ilIcons;
  miHelp.SubMenuImagesWidth := ICON_SIZE_SMALL;

  pmSearch.Images      := dmImages.ilIcons;
  pmSearch.ImagesWidth := ICON_SIZE_SMALL;

  pmWindow.Images      := dmImages.ilIcons;
  pmWindow.ImagesWidth := ICON_SIZE_SMALL;

  pcList.Images        := dmImages.ilIcons;
  pcList.ImagesWidth := ICON_SIZE_SMALL;

  btnedtSearch.RightButton.Images := dmImages.ilIcons;
  btnedtSearch.RightButton.ImagesWidth := ICON_SIZE_SMALL;

  btnedtSearch.LeftButton.Images := dmImages.ilIcons;
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
  if Assigned(FSearch) then
    FSearch.InitIcons;
end;

procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FRestoringSettings := False;
end;

procedure TfrmMain.PopulatePopUpMenuFromAnother(APopupMenu: TMenuItem; AParentMenuItem: TMenuItem);
begin
  Utility.MenuUtils.PopulatePopUpMenuFromAnother(APopupMenu, AParentMenuItem);
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
begin
  if Assigned(FSearch) then
    FSearch.ExecuteOn(TreeSearch, Keyword, SearchType);
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
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TfrmMain.MainFormCreate', Self);

  FRestoringSettings := True;

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
  Application.CreateForm(TfrmGraphicMenu, frmGraphicMenu);
  pcList.ActivePageIndex := PG_LIST;

  //Setup events in vsts
  ASuiteInstance.VSTEvents.SetupVSTList(vstList);
  ASuiteInstance.VSTEvents.SetupVSTSearch(vstSearch);

  // Initialize controllers
  FSearch := TSearchController.Create(vstSearch, btnedtSearch, pmSearch);
  FClipboard := TClipboardController.Create(vstList);
  FRun := TRunController.Create;
  FSort := TSortController.Create;

  //Load Database and get icons (only first level of tree)
  Config.AddObserver(Self);
  Config.LoadConfig;
  ASuiteInstance.LoadList;
  ASuiteManager.ListManager.ExecuteAutorunList(amStartup);

  //Restore window and UI settings
  RestoreSettings;

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

procedure TfrmMain.FormShow(Sender: TObject);
begin
  tmrCheckItems.Enabled := True;
end;

procedure TfrmMain.BeforeDestruction;
begin
  FreeAndNil(FSearch);
  FreeAndNil(FClipboard);
  FreeAndNil(FRun);
  FreeAndNil(FSort);
  Config.RemoveObserver(Self);
  inherited BeforeDestruction;
end;

function TfrmMain.ShouldApply(const Prop, Target: string): Boolean;
begin
  Result := (Prop = '') or (Prop = Target);
end;

procedure TfrmMain.ApplyHoldSize(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'HoldSize') then
    Exit;

  Self.BorderStyle := bsSingle;
  Self.BorderIcons := [biSystemMenu, biMinimize];
  if not Config.HoldSize then
  begin
    Self.BorderStyle := bsSizeable;
    Self.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  end;
end;

procedure TfrmMain.ApplyAlwaysOnTop(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'AlwaysOnTop') then
    Exit;

  if Config.AlwaysOnTop then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;
end;

procedure TfrmMain.ApplyCustomTitle(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'UseCustomTitle') then
    Exit;

  if Config.UseCustomTitle and (Config.CustomTitleString <> '') then
    Self.Caption := Config.CustomTitleString
  else
    Self.Caption := APP_TITLE;
end;

procedure TfrmMain.ApplyHideTabSearch(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'HideTabSearch') then
    Exit;

  tbSearch.TabVisible    := not Config.HideTabSearch;
  tbList.TabVisible      := not Config.HideTabSearch;
  pcList.ActivePageIndex := 0;
end;

procedure TfrmMain.ApplySearchColumns(const PropertyName: string);
begin
  if ShouldApply(PropertyName, 'SearchNameColWidth') then
    vstSearch.Header.Columns[0].Width := Config.SearchNameColWidth;

  if ShouldApply(PropertyName, 'SearchCategoryColWidth') then
    vstSearch.Header.Columns[1].Width := Config.SearchCategoryColWidth;
end;

procedure TfrmMain.ApplyTreeAutoExpand(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'TVAutoOpClCats') then
    Exit;

  if Config.TVAutoOpClCats then
    ASuiteInstance.MainTree.TreeOptions.AutoOptions := ASuiteInstance.MainTree.TreeOptions.AutoOptions + [toAutoExpand]
  else
    ASuiteInstance.MainTree.TreeOptions.AutoOptions := ASuiteInstance.MainTree.TreeOptions.AutoOptions - [toAutoExpand];
end;

procedure TfrmMain.ApplyTreeFont(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'TVFont') then
    Exit;

  ASuiteInstance.MainTree.Font.Assign(Config.TVFont);
end;

procedure TfrmMain.ApplyTreeBackgroundFlag(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'TVBackground') then
    Exit;

  if Config.TVBackground then
    ASuiteInstance.MainTree.TreeOptions.PaintOptions := ASuiteInstance.MainTree.TreeOptions.PaintOptions + [toShowBackground]
  else
    ASuiteInstance.MainTree.TreeOptions.PaintOptions := ASuiteInstance.MainTree.TreeOptions.PaintOptions - [toShowBackground];
end;

procedure TfrmMain.ApplySmallIconSize(const PropertyName: string);
begin
  if not ShouldApply(PropertyName, 'TVSmallIconSize') then
    Exit;

  TVirtualTreeMethods.ChangeTreeIconSize(ASuiteInstance.MainTree, Config.TVSmallIconSize);
  if ASuiteInstance.MainTree.HasChildren[ASuiteInstance.MainTree.RootNode] then
  begin
    ASuiteInstance.MainTree.FullCollapse;
    TVirtualTreeMethods.ChangeAllNodeHeight(ASuiteInstance.MainTree, ASuiteInstance.MainTree.DefaultNodeHeight);
  end;
end;

procedure TfrmMain.ApplyAfterUpdateConfig(const PropertyName: string);
var
  sBackgroundPath: string;
begin
  if not ShouldApply(PropertyName, 'AfterUpdateConfig') then
    Exit;

  SetDefaultLang(Config.LangID, ASuiteInstance.Paths.SuitePathLocale);

  // Reload icons and item colors
  SetAllIcons;
  TVirtualTreeMethods.UpdateItemColor(ASuiteInstance.MainTree);

  // Update background image
  sBackgroundPath := ASuiteInstance.Paths.RelativeToAbsolute(Config.TVBackgroundPath);
  if Config.TVBackground and (Config.TVBackgroundPath <> '') and FileExists(sBackgroundPath) then
  begin
    if (ExtractLowerFileExt(sBackgroundPath) = EXT_PNG) or
       (ExtractLowerFileExt(sBackgroundPath) = EXT_BMP) then
      ASuiteInstance.MainTree.Background.LoadFromFile(sBackgroundPath);
  end;

  ASuiteInstance.MainTree.Update;
end;

procedure TfrmMain.ApplyBoundsGroup(const PropertyName: string);
begin
  if PropertyName <> 'MainFormBounds' then
    Exit;

  Self.Left   := Config.MainFormLeft;
  Self.Top    := Config.MainFormTop;
  Self.Width  := Scale96ToForm(Config.MainFormWidth);
  Self.Height := Scale96ToForm(Config.MainFormHeight);
end;

procedure TfrmMain.ApplyBoundsIndividual(const PropertyName: string);
begin
  if ShouldApply(PropertyName, 'MainFormLeft') then
    Self.Left := Config.MainFormLeft;

  if ShouldApply(PropertyName, 'MainFormTop') then
    Self.Top := Config.MainFormTop;

  if ShouldApply(PropertyName, 'MainFormWidth') then
    Self.Width := Scale96ToForm(Config.MainFormWidth);

  if ShouldApply(PropertyName, 'MainFormHeight') then
    Self.Height := Scale96ToForm(Config.MainFormHeight);
end;

procedure TfrmMain.ConfigChanged(const PropertyName: string);
begin
  ApplyHoldSize(PropertyName);
  ApplyAlwaysOnTop(PropertyName);
  ApplyCustomTitle(PropertyName);
  ApplyHideTabSearch(PropertyName);
  ApplySearchColumns(PropertyName);
  ApplyTreeAutoExpand(PropertyName);
  ApplyTreeFont(PropertyName);
  ApplyTreeBackgroundFlag(PropertyName);
  ApplySmallIconSize(PropertyName);
  ApplyAfterUpdateConfig(PropertyName);
  ApplyBoundsGroup(PropertyName);
  ApplyBoundsIndividual(PropertyName);
end;

procedure TfrmMain.RestoreSettings;
begin
  FRestoringSettings := True;
  try
    Self.Left   := Config.MainFormLeft;
    Self.Top    := Config.MainFormTop;
    Self.Width  := Scale96ToForm(Config.MainFormWidth);
    Self.Height := Scale96ToForm(Config.MainFormHeight);
  finally
    FRestoringSettings := False;
  end;
end;

end.
