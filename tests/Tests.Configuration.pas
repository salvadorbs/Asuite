unit Tests.Configuration;

{$mode objfpc}{$H+}

{ Behaviour tests for TConfiguration (AppConfig.Main).

  Every setter below is checked for its field effect and its observer
  notification through a recording probe; guarded setters are additionally
  checked for silence on identical values, and BeginUpdate/EndUpdate for
  batching.

  Deliberately excluded: SaveSettings/RestoreSettings (private, and replaying
  them touches the real icon cache, backup pruning and desktop hotkeys),
  LoadConfig/SaveConfig (they use the real settings file), SetCache
  (deletes the real icon cache folders), SetStartWithWindows (writes OS
  autostart entries), the hotkey-string setters (they register with the
  desktop portal and pop a modal dialog on failure) and SetTrayIcon
  (its result depends on tray availability). }

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry,
  Kernel.Enumerations, Kernel.Instance,
  AppConfig.Main, AppConfig.Observer;

type
  TConfigProbe = class(TInterfacedObject, IConfigObserver)
  private
    FNames: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConfigChanged(const PropertyName: UnicodeString = '');
    procedure Clear;
    function Last: UnicodeString;
    function Count: Integer;
  end;

  TTestConfiguration = class(TTestCase)
  private
    FProbe: TConfigProbe;
    FI: IConfigObserver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructorDefaults;
    procedure TestMainWindowSetters;
    procedure TestTreeViewSetters;
    procedure TestMruMfuSetters;
    procedure TestExecutionSetters;
    procedure TestAutorunAndBackupSetters;
    procedure TestTrayAndMenuSetters;
    procedure TestStringSetters;
    procedure TestIntegerSetters;
    procedure TestEnumSetters;
    procedure TestTVFontAssign;
    procedure TestChangedFlagIsSilent;
    procedure TestHotKeyBoolean;
    procedure TestSchedulerBoolean;
    procedure TestBeginEndUpdateBatches;
    procedure TestASuiteStateNotifies;
    procedure TestScanFolderExcludeNames;
  end;

implementation

{ TConfigProbe }

constructor TConfigProbe.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
end;

destructor TConfigProbe.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

procedure TConfigProbe.ConfigChanged(const PropertyName: UnicodeString);
begin
  FNames.Add(PropertyName);
end;

procedure TConfigProbe.Clear;
begin
  FNames.Clear;
end;

function TConfigProbe.Last: UnicodeString;
begin
  if FNames.Count > 0 then
    Result := FNames[FNames.Count - 1]
  else
    Result := '';
end;

function TConfigProbe.Count: Integer;
begin
  Result := FNames.Count;
end;

{ TTestConfiguration }

procedure TTestConfiguration.SetUp;
begin
  inherited SetUp;
  FProbe := TConfigProbe.Create;
  FI := FProbe;
  Config.AddObserver(FProbe);
end;

procedure TTestConfiguration.TearDown;
begin
  Config.RemoveObserver(FProbe);
  FI := nil;
  inherited TearDown;
end;

procedure TTestConfiguration.TestConstructorDefaults;
begin
  // Only properties no other suite ever mutates: ambient values equal the
  // constructor's, whatever ran before
  AssertFalse('no windows autostart', Config.StartWithWindows);
  AssertTrue('panel at startup', Config.ShowPanelAtStartUp);
  AssertFalse('graphic menu at startup', Config.ShowGraphicMenuAtStartUp);
  AssertFalse('no custom title', Config.UseCustomTitle);
  AssertTrue('dialogs centered', Config.DialogCenterMF);
  AssertEquals('default width', 200, Config.MainFormWidth);
  AssertEquals('default height', 385, Config.MainFormHeight);
  AssertFalse('tree background off', Config.TVBackground);
  AssertTrue('small tree icons', Config.TVSmallIconSize);
  AssertTrue('mru on', Config.MRU);
  AssertEquals('mru default count', 11, Config.MRUNumber);
  AssertTrue('mfu submenu on', Config.SubMenuMFU);
  AssertEquals('mfu default count', 11, Config.MFUNumber);
  AssertFalse('no single click run', Config.RunSingleClick);
  AssertTrue('confirm categories', Config.ConfirmRunCat);
  AssertFalse('no auto close', Config.AutoCloseProcess);
  AssertTrue('tray icon on', Config.TrayIcon);
  AssertTrue('hotkeys on', Config.HotKey);
end;

procedure TTestConfiguration.TestMainWindowSetters;
var
  Old: Boolean;
begin
  Old := Config.ShowPanelAtStartUp;
  FProbe.Clear;
  Config.ShowPanelAtStartUp := not Old;
  try
    AssertEquals('field flipped', not Old, Config.ShowPanelAtStartUp);
    AssertEquals('notified once', 1, FProbe.Count);
    AssertEquals('right name', 'ShowPanelAtStartUp', UTF8Encode(FProbe.Last));
  finally
    Config.ShowPanelAtStartUp := Old;
  end;

  Old := Config.HideTabSearch;
  FProbe.Clear;
  Config.HideTabSearch := not Old;
  try
    AssertEquals('field flipped', not Old, Config.HideTabSearch);
    AssertEquals('notified', 'HideTabSearch', UTF8Encode(FProbe.Last));
  finally
    Config.HideTabSearch := Old;
  end;

  Old := Config.SearchAsYouType;
  FProbe.Clear;
  Config.SearchAsYouType := not Old;
  try
    AssertEquals('field flipped', not Old, Config.SearchAsYouType);
    AssertEquals('notified', 'SearchAsYouType', UTF8Encode(FProbe.Last));
  finally
    Config.SearchAsYouType := Old;
  end;

  Old := Config.HoldSize;
  FProbe.Clear;
  Config.HoldSize := not Old;
  try
    AssertEquals('field flipped', not Old, Config.HoldSize);
    AssertEquals('notified', 'HoldSize', UTF8Encode(FProbe.Last));
  finally
    Config.HoldSize := Old;
  end;

  Old := Config.AlwaysOnTop;
  FProbe.Clear;
  Config.AlwaysOnTop := not Old;
  try
    AssertEquals('field flipped', not Old, Config.AlwaysOnTop);
    AssertEquals('notified', 'AlwaysOnTop', UTF8Encode(FProbe.Last));
  finally
    Config.AlwaysOnTop := Old;
  end;
end;

procedure TTestConfiguration.TestTreeViewSetters;
var
  Old: Boolean;
begin
  Old := Config.TVAutoOpCatsDrag;
  FProbe.Clear;
  Config.TVAutoOpCatsDrag := not Old;
  try
    AssertEquals('field flipped', not Old, Config.TVAutoOpCatsDrag);
    AssertEquals('notified', 'TVAutoOpCatsDrag', UTF8Encode(FProbe.Last));
  finally
    Config.TVAutoOpCatsDrag := Old;
  end;

  Old := Config.TVBackground;
  FProbe.Clear;
  Config.TVBackground := not Old;
  try
    AssertEquals('field flipped', not Old, Config.TVBackground);
    AssertEquals('notified', 'TVBackground', UTF8Encode(FProbe.Last));
  finally
    Config.TVBackground := Old;
  end;

  // Guarded: silence on identical values
  Old := Config.TVSmallIconSize;
  FProbe.Clear;
  Config.TVSmallIconSize := Old;
  AssertEquals('identical value is silent', 0, FProbe.Count);
  Config.TVSmallIconSize := not Old;
  try
    AssertEquals('change notifies', 'TVSmallIconSize', UTF8Encode(FProbe.Last));
  finally
    Config.TVSmallIconSize := Old;
  end;
end;

procedure TTestConfiguration.TestMruMfuSetters;
var
  OldBool: Boolean;
  OldNum: Integer;
begin
  OldBool := Config.MRU;
  FProbe.Clear;
  Config.MRU := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.MRU);
    AssertEquals('notified', 'MRU', UTF8Encode(FProbe.Last));
  finally
    Config.MRU := OldBool;
  end;

  OldBool := Config.SubMenuMFU;
  FProbe.Clear;
  Config.SubMenuMFU := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.SubMenuMFU);
    AssertEquals('notified', 'SubMenuMFU', UTF8Encode(FProbe.Last));
  finally
    Config.SubMenuMFU := OldBool;
  end;

  OldNum := Config.MRUNumber;
  FProbe.Clear;
  Config.MRUNumber := OldNum;
  AssertEquals('identical value is silent', 0, FProbe.Count);
  Config.MRUNumber := OldNum + 1;
  try
    AssertEquals('field updated', OldNum + 1, Config.MRUNumber);
    AssertEquals('notified', 'MRUNumber', UTF8Encode(FProbe.Last));
  finally
    Config.MRUNumber := OldNum;
  end;

  OldNum := Config.MFUNumber;
  FProbe.Clear;
  Config.MFUNumber := OldNum + 1;
  try
    AssertEquals('notified', 'MFUNumber', UTF8Encode(FProbe.Last));
  finally
    Config.MFUNumber := OldNum;
  end;
end;

procedure TTestConfiguration.TestExecutionSetters;
var
  OldBool: Boolean;
  OldAction: TActionOnExecute;
begin
  OldBool := Config.RunSingleClick;
  FProbe.Clear;
  Config.RunSingleClick := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.RunSingleClick);
    AssertEquals('notified', 'RunSingleClick', UTF8Encode(FProbe.Last));
  finally
    Config.RunSingleClick := OldBool;
  end;

  OldBool := Config.ConfirmRunCat;
  FProbe.Clear;
  Config.ConfirmRunCat := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.ConfirmRunCat);
    AssertEquals('notified', 'ConfirmRunCat', UTF8Encode(FProbe.Last));
  finally
    Config.ConfirmRunCat := OldBool;
  end;

  OldAction := Config.ActionOnExe;
  FProbe.Clear;
  Config.ActionOnExe := OldAction;
  AssertEquals('identical value is silent', 0, FProbe.Count);
  Config.ActionOnExe := aeRunAndHide;
  try
    AssertTrue('field updated', Config.ActionOnExe = aeRunAndHide);
    AssertEquals('notified', 'ActionOnExe', UTF8Encode(FProbe.Last));
  finally
    Config.ActionOnExe := OldAction;
  end;
end;

procedure TTestConfiguration.TestAutorunAndBackupSetters;
var
  OldBool: Boolean;
  OldNum: Integer;
begin
  OldBool := Config.AutorunStartup;
  FProbe.Clear;
  Config.AutorunStartup := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.AutorunStartup);
    AssertEquals('notified', 'AutorunStartup', UTF8Encode(FProbe.Last));
  finally
    Config.AutorunStartup := OldBool;
  end;

  // Enabling backups creates the (gitignored) backup folder
  OldBool := Config.Backup;
  FProbe.Clear;
  Config.Backup := True;
  try
    AssertTrue('field set', Config.Backup);
    AssertEquals('notified', 'Backup', UTF8Encode(FProbe.Last));
  finally
    Config.Backup := OldBool;
  end;

  // Raising never prunes; restoring the old (lower) value may prune the
  // backup folder, which only ever holds test leftovers
  OldNum := Config.BackupNumber;
  FProbe.Clear;
  Config.BackupNumber := OldNum + 1;
  AssertEquals('field raised', OldNum + 1, Config.BackupNumber);
  AssertEquals('notified', 'BackupNumber', UTF8Encode(FProbe.Last));
  Config.BackupNumber := OldNum;
  AssertEquals('field restored', OldNum, Config.BackupNumber);
end;

procedure TTestConfiguration.TestTrayAndMenuSetters;
var
  OldBool: Boolean;
  OldAction: TTrayiconActionClick;
begin
  OldBool := Config.GMFade;
  FProbe.Clear;
  Config.GMFade := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.GMFade);
    AssertEquals('notified', 'GMFade', UTF8Encode(FProbe.Last));
  finally
    Config.GMFade := OldBool;
  end;

  OldBool := Config.AutoExpansionFolder;
  FProbe.Clear;
  Config.AutoExpansionFolder := not OldBool;
  try
    AssertEquals('field flipped', not OldBool, Config.AutoExpansionFolder);
    AssertEquals('notified', 'AutoExpansionFolder', UTF8Encode(FProbe.Last));
  finally
    Config.AutoExpansionFolder := OldBool;
  end;

  OldAction := Config.ActionClickLeft;
  FProbe.Clear;
  Config.ActionClickLeft := OldAction;
  AssertEquals('identical value is silent', 0, FProbe.Count);
  if OldAction = tcNone then
    Config.ActionClickLeft := tcShowClassicMenu
  else
    Config.ActionClickLeft := tcNone;
  try
    AssertEquals('notified', 'ActionClickLeft', UTF8Encode(FProbe.Last));
  finally
    Config.ActionClickLeft := OldAction;
  end;
end;

procedure TTestConfiguration.TestStringSetters;
var
  Old: UnicodeString;
begin
  Old := Config.CustomTitleString;
  FProbe.Clear;
  Config.CustomTitleString := 'custom title';
  try
    AssertEquals('field set', 'custom title',
      UTF8Encode(Config.CustomTitleString));
    AssertEquals('notified', 'CustomTitleString', UTF8Encode(FProbe.Last));
  finally
    Config.CustomTitleString := Old;
  end;

  // Empty themes fall back to default
  Old := Config.GMTheme;
  FProbe.Clear;
  Config.GMTheme := '';
  AssertEquals('empty falls back', 'default', UTF8Encode(Config.GMTheme));
  AssertEquals('notified', 'GMTheme', UTF8Encode(FProbe.Last));
  Config.GMTheme := 'custom';
  AssertEquals('field set', 'custom', UTF8Encode(Config.GMTheme));
  Config.GMTheme := Old;
  FProbe.Clear;

  Old := Config.LangID;
  FProbe.Clear;
  Config.LangID := '';
  AssertEquals('empty falls back to english', 'en', UTF8Encode(Config.LangID));
  Config.LangID := 'it';
  AssertEquals('field set', 'it', UTF8Encode(Config.LangID));
  AssertEquals('notified', 'LangID', UTF8Encode(FProbe.Last));
  Config.LangID := Old;
end;

procedure TTestConfiguration.TestIntegerSetters;
var
  Old: Integer;
begin
  Old := Config.MainFormWidth;
  FProbe.Clear;
  Config.MainFormWidth := Old + 10;
  try
    AssertEquals('field updated', Old + 10, Config.MainFormWidth);
    AssertEquals('notified', 'MainFormWidth', UTF8Encode(FProbe.Last));
  finally
    Config.MainFormWidth := Old;
  end;

  Old := Config.SearchNameColWidth;
  FProbe.Clear;
  Config.SearchNameColWidth := Old;
  AssertEquals('identical value is silent', 0, FProbe.Count);
  Config.SearchNameColWidth := Old + 10;
  try
    AssertEquals('notified', 'SearchNameColWidth', UTF8Encode(FProbe.Last));
  finally
    Config.SearchNameColWidth := Old;
  end;

  Old := Config.GMPositionTop;
  FProbe.Clear;
  Config.GMPositionTop := Old + 1;
  try
    AssertEquals('notified', 'GMPositionTop', UTF8Encode(FProbe.Last));
  finally
    Config.GMPositionTop := Old;
  end;
end;

procedure TTestConfiguration.TestEnumSetters;
var
  Old: TTrayiconActionClick;
begin
  Old := Config.ActionClickMiddle;
  FProbe.Clear;
  Config.ActionClickMiddle := Old;
  AssertEquals('identical value is silent', 0, FProbe.Count);
  if Old = tcNone then
    Config.ActionClickMiddle := tcShowWindow
  else
    Config.ActionClickMiddle := tcNone;
  try
    AssertEquals('notified', 'ActionClickMiddle', UTF8Encode(FProbe.Last));
  finally
    Config.ActionClickMiddle := Old;
  end;

  Old := Config.ActionClickRight;
  FProbe.Clear;
  if Old = tcNone then
    Config.ActionClickRight := tcShowWindow
  else
    Config.ActionClickRight := tcNone;
  try
    AssertEquals('notified', 'ActionClickRight', UTF8Encode(FProbe.Last));
  finally
    Config.ActionClickRight := Old;
  end;
end;

procedure TTestConfiguration.TestTVFontAssign;
var
  NewFont, OrigFont: TFont;
begin
  OrigFont := TFont.Create;
  NewFont := TFont.Create;
  try
    OrigFont.Assign(Config.TVFont);
    NewFont.Assign(Config.TVFont);
    NewFont.Size := Config.TVFont.Size + 4;

    FProbe.Clear;
    Config.TVFont := NewFont;
    AssertEquals('font copied', Config.TVFont.Size, NewFont.Size);
    AssertEquals('notified', 'TVFont', UTF8Encode(FProbe.Last));

    FProbe.Clear;
    Config.TVFont := NewFont;
    AssertEquals('identical font is silent', 0, FProbe.Count);
  finally
    Config.TVFont := OrigFont;
    NewFont.Free;
    OrigFont.Free;
  end;
end;

procedure TTestConfiguration.TestChangedFlagIsSilent;
var
  Old: Boolean;
begin
  Old := Config.Changed;
  FProbe.Clear;
  Config.Changed := True;
  try
    AssertTrue('flag set', Config.Changed);
    AssertEquals('dirty marking notifies nobody', 0, FProbe.Count);
  finally
    Config.Changed := Old;
  end;
end;

procedure TTestConfiguration.TestHotKeyBoolean;
var
  Old: Boolean;
begin
  // RefreshRegs on an empty hotkey list is a backend no-op
  Old := Config.HotKey;
  FProbe.Clear;
  Config.HotKey := not Old;
  try
    AssertEquals('field flipped', not Old, Config.HotKey);
    AssertEquals('notified', 'HotKey', UTF8Encode(FProbe.Last));
  finally
    Config.HotKey := Old;
  end;
end;

procedure TTestConfiguration.TestSchedulerBoolean;
var
  Old: Boolean;
begin
  Old := Config.Scheduler;
  FProbe.Clear;
  Config.Scheduler := False;
  try
    AssertFalse('scheduler off', Config.Scheduler);
    AssertFalse('timer disarmed', ASuiteInstance.Scheduler.Timer.Enabled);
    AssertEquals('notified', 'Scheduler', UTF8Encode(FProbe.Last));
  finally
    Config.Scheduler := Old;
  end;
end;

procedure TTestConfiguration.TestBeginEndUpdateBatches;
var
  OldHold, OldTop: Boolean;
begin
  OldHold := Config.HoldSize;
  OldTop := Config.AlwaysOnTop;
  Config.BeginUpdate;
  try
    FProbe.Clear;
    Config.HoldSize := not OldHold;
    Config.AlwaysOnTop := not OldTop;
    AssertEquals('notifications deferred', 0, FProbe.Count);
  finally
    Config.EndUpdate;
  end;
  try
    AssertEquals('both flushed', 2, FProbe.Count);
  finally
    Config.HoldSize := OldHold;
    Config.AlwaysOnTop := OldTop;
  end;
end;

procedure TTestConfiguration.TestASuiteStateNotifies;
var
  Old: TLauncherState;
begin
  Old := Config.ASuiteState;
  FProbe.Clear;
  Config.ASuiteState := lsNormal;
  try
    AssertTrue('state changed', Config.ASuiteState = lsNormal);
    AssertEquals('notified', 'ASuiteState', UTF8Encode(FProbe.Last));
  finally
    Config.ASuiteState := Old;
  end;
end;

procedure TTestConfiguration.TestScanFolderExcludeNames;
var
  Fresh, Same: TStringList;
begin
  Fresh := TStringList.Create;
  try
    Fresh.Add('setup');
    FProbe.Clear;
    Config.ScanFolderExcludeNames := Fresh;
    AssertEquals('list copied, not referenced',
      'setup', UTF8Encode(Config.ScanFolderExcludeNames[0]));

    Same := TStringList.Create;
    try
      Same.Add('setup');
      FProbe.Clear;
      Config.ScanFolderExcludeNames := Same;
      AssertEquals('identical list is silent', 0, FProbe.Count);
    finally
      Same.Free;
    end;
  finally
    Fresh.Free;
  end;
end;

initialization
  RegisterTest(TTestConfiguration);

end.
