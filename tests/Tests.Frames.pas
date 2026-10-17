unit Tests.Frames;

{$mode objfpc}{$H+}

{ Behaviour tests for the option/property frames (Frame.*).

  Each frame maps Config (or node data) to controls on load and back on
  save. The tests drive LoadData/SaveData directly and restore every
  touched Config property. Frames stream their .lfm like the application
  does; the autorun frame also gets the bare dmImages its tree setups
  expect (no tray menu is ever needed).

  Deliberately excluded: the hotkey frame (it registers real shortcuts),
  the trayicon frame (its save depends on tray availability) and the
  remaining property frames (same load/edit/save pattern as the behavior
  frame covered here). }

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry,
  VirtualTrees, VirtualTrees.Types,
  Frame.Options.MainWindow, Frame.Options.General, Frame.Options.Stats,
  Frame.Options.Autorun, Frame.Options.Advanced, Frame.Properties.Behavior,
  Frame.Properties.EnvironmentVars, Frame.Properties.General.Software,
  Frame.Properties.Advanced,
  DataModules.Icons,
  NodeDataTypes.Custom, NodeDataTypes.Files,
  Kernel.Enumerations, Kernel.Types, Kernel.Consts,
  AppConfig.Main, Kernel.Manager,
  Tests.Helpers.Tree;

type
  TTestMainWindowFrame = class(TTestCase)
  private
    FPage: TfrmMainWindowOptionsPage;
    FOldFont: TFont;
    FOldHold, FOldOnTop, FOldHideSearch, FOldAsYouType, FOldCustomTitle,
      FOldDialogCenter, FOldAutoOpCl, FOldAutoOpDrag, FOldNoConfirmDelete,
      FOldSmallIcon, FOldBackground: Boolean;
    FOldTitle, FOldBackgroundPath: UnicodeString;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMapsConfigToControls;
    procedure TestSaveWritesControlsToConfig;
  end;

  TTestGeneralFrame = class(TTestCase)
  private
    FPage: TfrmGeneralOptionsPage;
    FOldStartWindows, FOldShowPanel, FOldShowMenu, FOldMissedTask,
      FOldAnotherInstance, FOldConfirmClose: Boolean;
    FOldActionOnExe: TActionOnExecute;
    FOldSingleClick, FOldConfirmCat, FOldAutoClose: Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMapsConfigToControls;
    procedure TestSaveWritesControlsToConfig;
  end;

  TTestStatsFrame = class(TNodeTreeTestCase)
  published
    procedure TestLoadCountsNodes;
  end;

  TTestAutorunFrame = class(TNodeTreeTestCase)
  published
    procedure TestLoadPopulatesStartupList;
    procedure TestLoadLeavesShutdownEmpty;
  end;

  TTestBehaviorFrame = class(TNodeTreeTestCase)
  published
    procedure TestLoadFillsCombos;
    procedure TestSaveWritesNodeFields;
  end;

  TTestAdvancedFrame = class(TTestCase)
  private
    FPage: TfrmAdvancedOptionsPage;
    FOldMRU, FOldMFU, FOldBackup, FOldScheduler: Boolean;
    FOldMRUNumber, FOldMFUNumber, FOldBackupNumber: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMapsConfigToControls;
    procedure TestSaveWritesControlsToConfig;
  end;

  TTestSoftwareFrame = class(TNodeTreeTestCase)
  published
    procedure TestLoadMapsNodeToControls;
    procedure TestSaveWritesControlsToNode;
    procedure TestSaveEmptyPathFallsBackToDrive;
  end;

  TTestEnvironmentVarsFrame = class(TNodeTreeTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadShowsNodeVars;
    procedure TestSaveStoresNodeVars;
  end;

  TTestAdvancedPropertyFrame = class(TNodeTreeTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMapsNodeToControls;
    procedure TestSaveWritesControlsToNode;
  end;

implementation

{ TTestMainWindowFrame }

procedure TTestMainWindowFrame.SetUp;
begin
  inherited SetUp;
  FPage := TfrmMainWindowOptionsPage.Create(nil);
  FOldHold := Config.HoldSize;
  FOldOnTop := Config.AlwaysOnTop;
  FOldHideSearch := Config.HideTabSearch;
  FOldAsYouType := Config.SearchAsYouType;
  FOldTitle := Config.CustomTitleString;
  FOldCustomTitle := Config.UseCustomTitle;
  FOldDialogCenter := Config.DialogCenterMF;
  FOldAutoOpCl := Config.TVAutoOpClCats;
  FOldAutoOpDrag := Config.TVAutoOpCatsDrag;
  FOldNoConfirmDelete := Config.TVDisableConfirmDelete;
  FOldSmallIcon := Config.TVSmallIconSize;
  FOldBackgroundPath := Config.TVBackgroundPath;
  FOldBackground := Config.TVBackground;
  FOldFont := TFont.Create;
  FOldFont.Assign(Config.TVFont);
end;

procedure TTestMainWindowFrame.TearDown;
begin
  Config.HoldSize := FOldHold;
  Config.AlwaysOnTop := FOldOnTop;
  Config.HideTabSearch := FOldHideSearch;
  Config.SearchAsYouType := FOldAsYouType;
  Config.CustomTitleString := FOldTitle;
  Config.UseCustomTitle := FOldCustomTitle;
  Config.DialogCenterMF := FOldDialogCenter;
  Config.TVAutoOpClCats := FOldAutoOpCl;
  Config.TVAutoOpCatsDrag := FOldAutoOpDrag;
  Config.TVDisableConfirmDelete := FOldNoConfirmDelete;
  Config.TVSmallIconSize := FOldSmallIcon;
  Config.TVBackgroundPath := FOldBackgroundPath;
  Config.TVBackground := FOldBackground;
  Config.TVFont := FOldFont;
  FOldFont.Free;
  FPage.Free;
  inherited TearDown;
end;

procedure TTestMainWindowFrame.TestLoadMapsConfigToControls;
var
  NewFont: TFont;
begin
  Config.HoldSize := True;
  Config.CustomTitleString := 'loaded title';
  Config.UseCustomTitle := True;
  Config.TVBackgroundPath := 'bg.png';
  NewFont := TFont.Create;
  try
    NewFont.Assign(Config.TVFont);
    NewFont.Size := Config.TVFont.Size + 4;
    Config.TVFont := NewFont;

    AssertTrue('load succeeds', FPage.LoadData);
    AssertTrue('hold size mapped', FPage.cbHoldSize.Checked);
    AssertEquals('title mapped', 'loaded title', FPage.edtCustomTitle.Text);
    AssertTrue('custom title mapped', FPage.cbCustomTitle.Checked);
    AssertEquals('background mapped', 'bg.png', FPage.edtBackground.Text);
    AssertEquals('font mapped', Config.TVFont.Size, FPage.FontDialog1.Font.Size);
  finally
    NewFont.Free;
  end;
end;

procedure TTestMainWindowFrame.TestSaveWritesControlsToConfig;
begin
  AssertTrue('load succeeds', FPage.LoadData);

  FPage.cbHoldSize.Checked := not FOldHold;
  FPage.edtCustomTitle.Text := 'saved title';
  FPage.cbCustomTitle.Checked := not FOldCustomTitle;
  FPage.cbBackground.Checked := not FOldBackground;
  AssertTrue('save succeeds', FPage.SaveData);

  AssertEquals('hold size saved', not FOldHold, Config.HoldSize);
  AssertEquals('title saved', 'saved title', UTF8Encode(Config.CustomTitleString));
  AssertEquals('custom title saved', not FOldCustomTitle, Config.UseCustomTitle);
  AssertEquals('background saved', not FOldBackground, Config.TVBackground);
end;

{ TTestGeneralFrame }

procedure TTestGeneralFrame.SetUp;
begin
  inherited SetUp;
  FPage := TfrmGeneralOptionsPage.Create(nil);
  FOldStartWindows := Config.StartWithWindows;
  FOldShowPanel := Config.ShowPanelAtStartUp;
  FOldShowMenu := Config.ShowGraphicMenuAtStartUp;
  FOldMissedTask := Config.MissedSchedulerTask;
  FOldAnotherInstance := Config.ShowGraphicMenuAnotherInstance;
  FOldConfirmClose := Config.ConfirmMsgCloseApp;
  FOldActionOnExe := Config.ActionOnExe;
  FOldSingleClick := Config.RunSingleClick;
  FOldConfirmCat := Config.ConfirmRunCat;
  FOldAutoClose := Config.AutoCloseProcess;
end;

procedure TTestGeneralFrame.TearDown;
begin
  Config.StartWithWindows := FOldStartWindows;
  Config.ShowPanelAtStartUp := FOldShowPanel;
  Config.ShowGraphicMenuAtStartUp := FOldShowMenu;
  Config.MissedSchedulerTask := FOldMissedTask;
  Config.ShowGraphicMenuAnotherInstance := FOldAnotherInstance;
  Config.ConfirmMsgCloseApp := FOldConfirmClose;
  Config.ActionOnExe := FOldActionOnExe;
  Config.RunSingleClick := FOldSingleClick;
  Config.ConfirmRunCat := FOldConfirmCat;
  Config.AutoCloseProcess := FOldAutoClose;
  FPage.Free;
  inherited TearDown;
end;

procedure TTestGeneralFrame.TestLoadMapsConfigToControls;
begin
  AssertTrue('load succeeds', FPage.LoadData);
  AssertEquals('startup flag mapped', Config.StartWithWindows,
    FPage.cbWindowsStartup.Checked);
  AssertEquals('panel flag mapped', Config.ShowPanelAtStartUp,
    FPage.cbShowPanelStartup.Checked);
  AssertEquals('menu flag mapped', Config.ShowGraphicMenuAtStartUp,
    FPage.cbShowMenuStartup.Checked);
end;

procedure TTestGeneralFrame.TestSaveWritesControlsToConfig;
begin
  AssertTrue('load succeeds', FPage.LoadData);
  // cbWindowsStartup stays off: enabling it would write OS autostart entries
  FPage.cbShowPanelStartup.Checked := not FOldShowPanel;
  AssertTrue('save succeeds', FPage.SaveData);
  AssertEquals('panel flag saved', not FOldShowPanel, Config.ShowPanelAtStartUp);
end;

{ TTestStatsFrame }

procedure TTestStatsFrame.TestLoadCountsNodes;
var
  Page: TfrmStatsOptionsPage;
begin
  AddCategory('Cat');
  AddFile('File A', '/bin/a', Tree.GetFirst);
  AddFile('File B', '/bin/b', Tree.GetFirst);
  Page := TfrmStatsOptionsPage.Create(nil);
  try
    AssertTrue('load succeeds', Page.LoadData);
    AssertEquals('two embody software', '2',
      UTF8Encode(Page.lbSoftware.RightCaption));
    AssertEquals('one category', '1', UTF8Encode(Page.lbCat.RightCaption));
    AssertEquals('three total', '3', UTF8Encode(Page.lbTotal.RightCaption));
  finally
    Page.Free;
  end;
end;

{ TTestAutorunFrame }

procedure TTestAutorunFrame.TestLoadPopulatesStartupList;
var
  Page: TfrmAutorunOptionsPage;
  Node: TvFileNodeData;
begin
  dmImages := TdmImages.CreateNew(nil);
  try
    Node := AddFile('Tool', '/bin/tool');
    Node.Autorun := atAlwaysOnStart;
    Page := TfrmAutorunOptionsPage.Create(nil);
    try
      // Headless frames have no parent window, and SetupVSTAutorun sets
      // DefaultNodeHeight (which needs a handle while toAutoChangeScale is
      // on): drop the flag first, like the setup itself does right after.
      // Same for the display-only columns (AutoFitColumns needs a canvas).
      Page.vstStartupItems.TreeOptions.AutoOptions :=
        Page.vstStartupItems.TreeOptions.AutoOptions - [toAutoChangeScale];
      Page.vstShutdownItems.TreeOptions.AutoOptions :=
        Page.vstShutdownItems.TreeOptions.AutoOptions - [toAutoChangeScale];
      Page.vstStartupItems.Header.Columns.Clear;
      Page.vstShutdownItems.Header.Columns.Clear;
      AssertTrue('load succeeds', Page.LoadData);
      AssertEquals('startup tree populated', 1,
        Page.vstStartupItems.RootNodeCount);
      AssertTrue('mirror links the main node',
        PTreeDataX(Page.vstStartupItems.GetNodeData(
          Page.vstStartupItems.GetFirst))^.pNodeList = Node.PNode);
      AssertEquals('shutdown tree empty', 0,
        Page.vstShutdownItems.RootNodeCount);
    finally
      Page.Free;
    end;
  finally
    FreeAndNil(dmImages);
  end;
end;

procedure TTestAutorunFrame.TestLoadLeavesShutdownEmpty;
var
  Page: TfrmAutorunOptionsPage;
begin
  dmImages := TdmImages.CreateNew(nil);
  try
    Page := TfrmAutorunOptionsPage.Create(nil);
    try
      // See above: no canvas without a parent window
      Page.vstStartupItems.TreeOptions.AutoOptions :=
        Page.vstStartupItems.TreeOptions.AutoOptions - [toAutoChangeScale];
      Page.vstShutdownItems.TreeOptions.AutoOptions :=
        Page.vstShutdownItems.TreeOptions.AutoOptions - [toAutoChangeScale];
      Page.vstStartupItems.Header.Columns.Clear;
      Page.vstShutdownItems.Header.Columns.Clear;
      AssertTrue('load succeeds', Page.LoadData);
      AssertEquals('startup tree empty', 0,
        Page.vstStartupItems.RootNodeCount);
      AssertEquals('shutdown tree empty', 0,
        Page.vstShutdownItems.RootNodeCount);
    finally
      Page.Free;
    end;
  finally
    FreeAndNil(dmImages);
  end;
end;

{ TTestBehaviorFrame }

procedure TTestBehaviorFrame.TestLoadFillsCombos;
var
  Page: TfrmBehaviorPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Page := TfrmBehaviorPropertyPage.Create(nil,
    TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    AssertTrue('actions listed', Page.cxActionOnExe.Items.Count = 4);
    AssertTrue('autorun listed', Page.cxAutoExecute.Items.Count = 4);
    AssertTrue('states listed', Page.cxWindowState.Items.Count = 3);
    AssertEquals('action mapped', Ord(aeDefault), Page.cxActionOnExe.ItemIndex);
    AssertEquals('autorun mapped', Ord(atNever), Page.cxAutoExecute.ItemIndex);
    AssertEquals('state mapped', 0, Page.cxWindowState.ItemIndex);
  finally
    Page.Free;
  end;
end;

procedure TTestBehaviorFrame.TestSaveWritesNodeFields;
var
  Page: TfrmBehaviorPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Page := TfrmBehaviorPropertyPage.Create(nil,
    TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    Page.cxActionOnExe.ItemIndex := Ord(aeRunAndHide);
    Page.cxAutoExecute.ItemIndex := Ord(atAlwaysOnClose);
    Page.cxWindowState.ItemIndex := 2;
    AssertTrue('save succeeds', Page.SaveData);
    AssertTrue('action saved', Node.ActionOnExe = aeRunAndHide);
    AssertTrue('autorun saved', Node.Autorun = atAlwaysOnClose);
    AssertEquals('state saved', 2, Node.WindowState);
  finally
    Page.Free;
  end;
end;

{ TTestAdvancedFrame }

procedure TTestAdvancedFrame.SetUp;
begin
  inherited SetUp;
  FPage := TfrmAdvancedOptionsPage.Create(nil);
  FOldMRU := Config.MRU;
  FOldMRUNumber := Config.MRUNumber;
  FOldMFU := Config.MFU;
  FOldMFUNumber := Config.MFUNumber;
  FOldBackup := Config.Backup;
  FOldBackupNumber := Config.BackupNumber;
  FOldScheduler := Config.Scheduler;
end;

procedure TTestAdvancedFrame.TearDown;
begin
  Config.MRU := FOldMRU;
  Config.MRUNumber := FOldMRUNumber;
  Config.MFU := FOldMFU;
  Config.MFUNumber := FOldMFUNumber;
  Config.Backup := FOldBackup;
  Config.BackupNumber := FOldBackupNumber;
  Config.Scheduler := FOldScheduler;
  FPage.Free;
  inherited TearDown;
end;

procedure TTestAdvancedFrame.TestLoadMapsConfigToControls;
begin
  Config.MRU := True;
  Config.MRUNumber := 9;
  Config.MFU := True;
  Config.MFUNumber := 8;

  AssertTrue('load succeeds', FPage.LoadData);
  AssertTrue('mru checkbox', FPage.cbMRU.Checked);
  AssertEquals('mru count', 9, FPage.seRecents.Value);
  AssertTrue('mfu checkbox', FPage.cbMFU.Checked);
  AssertEquals('mfu count', 8, FPage.seMFU.Value);
  AssertEquals('backup checkbox', Config.Backup, FPage.cbBackup.Checked);
  AssertEquals('backup count', Config.BackupNumber, FPage.seBackup.Value);
  AssertEquals('scheduler checkbox', Config.Scheduler, FPage.cbScheduler.Checked);
end;

procedure TTestAdvancedFrame.TestSaveWritesControlsToConfig;
begin
  AssertTrue('load succeeds', FPage.LoadData);

  // cbCache is left as loaded: toggling it would touch the icon cache folders
  FPage.cbMRU.Checked := not FOldMRU;
  FPage.seRecents.Value := FOldMRUNumber + 2;
  FPage.cbMFU.Checked := not FOldMFU;
  FPage.seMFU.Value := FOldMFUNumber + 2;
  FPage.cbScheduler.Checked := not FOldScheduler;
  AssertTrue('save succeeds', FPage.SaveData);

  AssertEquals('mru saved', not FOldMRU, Config.MRU);
  AssertEquals('mru count saved', FOldMRUNumber + 2, Config.MRUNumber);
  AssertEquals('mfu saved', not FOldMFU, Config.MFU);
  AssertEquals('mfu count saved', FOldMFUNumber + 2, Config.MFUNumber);
  AssertEquals('scheduler saved', not FOldScheduler, Config.Scheduler);
end;

{ TTestSoftwareFrame }

procedure TTestSoftwareFrame.TestLoadMapsNodeToControls;
var
  Page: TfrmSWGeneralPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Node.Parameters := '--x';
  Node.WorkingDir := '/tmp';

  Page := TfrmSWGeneralPropertyPage.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    AssertEquals('path mapped', '/bin/tool',
      UTF8Encode(Page.edtPathExe.Text));
    AssertEquals('parameters mapped', '--x',
      UTF8Encode(Page.edtParameters.Text));
    AssertEquals('working dir mapped', '/tmp',
      UTF8Encode(Page.edtWorkingDir.Text));
  finally
    Page.Free;
  end;
end;

procedure TTestSoftwareFrame.TestSaveWritesControlsToNode;
var
  Page: TfrmSWGeneralPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Page := TfrmSWGeneralPropertyPage.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    Page.edtPathExe.Text := '/bin/other';
    Page.edtParameters.Text := '--y';
    Page.edtWorkingDir.Text := '/var';
    AssertTrue('save succeeds', Page.SaveData);
    AssertEquals('path saved', '/bin/other', UTF8Encode(Node.PathFile));
    AssertEquals('parameters saved', '--y', UTF8Encode(Node.Parameters));
    AssertEquals('working dir saved', '/var', UTF8Encode(Node.WorkingDir));
  finally
    Page.Free;
  end;
end;

procedure TTestSoftwareFrame.TestSaveEmptyPathFallsBackToDrive;
var
  Page: TfrmSWGeneralPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Page := TfrmSWGeneralPropertyPage.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    Page.edtPathExe.Text := '';
    AssertTrue('save succeeds', Page.SaveData);
    AssertEquals('drive placeholder stored', CONST_PATH_DRIVE,
      UTF8Encode(Node.PathFile));
  finally
    Page.Free;
  end;
end;

{ TTestEnvironmentVarsFrame }

procedure TTestEnvironmentVarsFrame.SetUp;
begin
  inherited SetUp;
  dmImages := TdmImages.CreateNew(nil);
end;

procedure TTestEnvironmentVarsFrame.TearDown;
begin
  FreeAndNil(dmImages);
  inherited TearDown;
end;

procedure TTestEnvironmentVarsFrame.TestLoadShowsNodeVars;
var
  Page: TfrmEnvironmentVars;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Node.EnvironmentVars.Add('FOO=bar');
  Node.EnvironmentVars.Add('BAZ=qux');

  Page := TfrmEnvironmentVars.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    AssertEquals('first var', 'bar', Page.vleVars.Strings.Values['FOO']);
    AssertEquals('second var', 'qux', Page.vleVars.Strings.Values['BAZ']);
  finally
    Page.Free;
  end;
end;

procedure TTestEnvironmentVarsFrame.TestSaveStoresNodeVars;
var
  Page: TfrmEnvironmentVars;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Node.EnvironmentVars.Add('FOO=bar');

  Page := TfrmEnvironmentVars.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    Page.vleVars.Strings.Values['NEW'] := 'value';
    AssertTrue('save succeeds', Page.SaveData);
    AssertEquals('new var stored', 'value', Node.EnvironmentVars.Values['NEW']);
    AssertEquals('old var kept', 'bar', Node.EnvironmentVars.Values['FOO']);
  finally
    Page.Free;
  end;
end;

{ TTestAdvancedPropertyFrame }

procedure TTestAdvancedPropertyFrame.SetUp;
begin
  inherited SetUp;
  dmImages := TdmImages.CreateNew(nil);
  Config.HotKey := False;
end;

procedure TTestAdvancedPropertyFrame.TearDown;
begin
  FreeAndNil(dmImages);
  inherited TearDown;
end;

procedure TTestAdvancedPropertyFrame.TestLoadMapsNodeToControls;
var
  Page: TfrmAdvancedPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Node.SchMode := smDaily;
  Node.SchDateTime := EncodeDate(2026, 1, 2) + EncodeTime(3, 4, 0, 0);
  Node.HideFromMenu := True;
  Node.NoMRU := True;
  Node.NoMFU := True;

  Page := TfrmAdvancedPropertyPage.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    AssertEquals('scheduler mode', Ord(smDaily), Page.cxScheduler.ItemIndex);
    AssertEquals('scheduler date', Trunc(EncodeDate(2026, 1, 2)),
      Trunc(Page.dtpSchDate.Date));
    AssertTrue('hide from menu', Page.cbHideSoftware.Checked);
    AssertTrue('no mru', Page.cbDontInsertMRU.Checked);
    AssertTrue('no mfu', Page.cbDontInsertMFU.Checked);
    AssertFalse('desktop shortcut', Page.cbShortcutDesktop.Checked);
  finally
    Page.Free;
  end;
end;

procedure TTestAdvancedPropertyFrame.TestSaveWritesControlsToNode;
var
  Page: TfrmAdvancedPropertyPage;
  Node: TvFileNodeData;
begin
  Node := AddFile('Tool', '/bin/tool');
  Page := TfrmAdvancedPropertyPage.Create(nil, TvCustomRealNodeData(Node));
  try
    AssertTrue('load succeeds', Page.LoadData);
    Page.cxScheduler.ItemIndex := Ord(smHourly);
    Page.dtpSchDate.Date := EncodeDate(2026, 5, 6);
    Page.dtpSchTime.Time := EncodeTime(7, 8, 0, 0);
    Page.cbHideSoftware.Checked := True;
    Page.cbDontInsertMRU.Checked := True;
    Page.cbDontInsertMFU.Checked := True;
    // cbShortcutDesktop stays unchecked: checking it would write a desktop file
    AssertTrue('save succeeds', Page.SaveData);

    AssertTrue('scheduler mode saved', Node.SchMode = smHourly);
    AssertEquals('scheduler date saved', Trunc(EncodeDate(2026, 5, 6)),
      Trunc(Node.SchDateTime));
    AssertTrue('hide from menu saved', Node.HideFromMenu);
    AssertTrue('no mru saved', Node.NoMRU);
    AssertTrue('no mfu saved', Node.NoMFU);
    AssertFalse('desktop shortcut untouched', Node.ShortcutDesktop);
  finally
    Page.Free;
  end;
end;

initialization
  RegisterTest(TTestMainWindowFrame);
  RegisterTest(TTestGeneralFrame);
  RegisterTest(TTestStatsFrame);
  RegisterTest(TTestAutorunFrame);
  RegisterTest(TTestBehaviorFrame);
  RegisterTest(TTestAdvancedFrame);
  RegisterTest(TTestSoftwareFrame);
  RegisterTest(TTestEnvironmentVarsFrame);
  RegisterTest(TTestAdvancedPropertyFrame);

end.
