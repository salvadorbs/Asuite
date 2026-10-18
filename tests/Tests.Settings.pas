unit Tests.Settings;

{$mode objfpc}{$H+}

{ Behaviour tests for LoadXMLSettings (Utility.XML).

  The procedure applies a legacy ASuite 1.x settings document to Config,
  so every touched property is snapshotted and restored around each call
  and Config.HotKey is switched off (the hotkey setters would otherwise
  talk to the desktop portal). The document must always carry an <Option>
  element: LoadXMLSettings dereferences it unconditionally.

  Deliberately excluded: the hotkey fields (empty here, so nothing is
  registered) and StartOnWindowsStartup (enabling it writes an OS
  autostart entry). }

interface

uses
  Classes, SysUtils, Graphics, DOM, XMLRead, LCLType, fpcunit, testregistry,
  Kernel.Enumerations, Kernel.Consts,
  AppConfig.Main, Utility.XML;

type
  TTestXmlSettings = class(TTestCase)
  private
    FOldFont: TFont;
    FOldHotKey: Boolean;
    FOldGMTheme, FOldTitle, FOldBackgroundPath, FOldTrayIconPath,
      FOldPersonalPicture: UnicodeString;
    FOldShowPanel, FOldShowMenu, FOldUseCustomTitle, FOldHideSearch,
      FOldHoldSize, FOldOnTop, FOldBackground, FOldAutoOpCl, FOldMRU,
      FOldSubMenuMRU, FOldBackup, FOldAutorunStartup, FOldAutorunShutdown,
      FOldCache, FOldScheduler, FOldRunSingleClick, FOldTrayIcon,
      FOldGMFade: Boolean;
    FOldMRUNumber, FOldBackupNumber: Integer;
    FOldActionOnExe, FOldActionLeft, FOldActionRight: Integer;
    function LoadDoc(const AXml: string): TXMLDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadMapsOptionValues;
    procedure TestLoadAppliesDefaultsToEmptyOption;
  end;

implementation

function SettingsDoc(const ABody: string): TXMLDocument;
var
  Path: string;
  FS: TFileStream;
  Raw: RawByteString;
begin
  Path := GetTempDir(False) + 'asuite_test_settings.xml';
  Raw := RawByteString('<ASuite><Option>' + ABody + '</Option></ASuite>');
  FS := TFileStream.Create(Path, fmCreate);
  try
    if Length(Raw) > 0 then
      FS.WriteBuffer(Raw[1], Length(Raw));
  finally
    FS.Free;
  end;
  ReadXMLFile(Result, Path);
  DeleteFile(Path);
end;

function TTestXmlSettings.LoadDoc(const AXml: string): TXMLDocument;
begin
  Result := SettingsDoc(AXml);
end;

procedure TTestXmlSettings.SetUp;
begin
  inherited SetUp;
  FOldHotKey := Config.HotKey;
  // The legacy loader replays the hotkey setters: keep them off the portal
  Config.HotKey := False;

  FOldGMTheme := Config.GMTheme;
  FOldTitle := Config.CustomTitleString;
  FOldBackgroundPath := Config.TVBackgroundPath;
  FOldTrayIconPath := Config.TrayCustomIconPath;
  FOldPersonalPicture := Config.GMPersonalPicture;
  FOldShowPanel := Config.ShowPanelAtStartUp;
  FOldShowMenu := Config.ShowGraphicMenuAtStartUp;
  FOldUseCustomTitle := Config.UseCustomTitle;
  FOldHideSearch := Config.HideTabSearch;
  FOldHoldSize := Config.HoldSize;
  FOldOnTop := Config.AlwaysOnTop;
  FOldBackground := Config.TVBackground;
  FOldAutoOpCl := Config.TVAutoOpClCats;
  FOldMRU := Config.MRU;
  FOldSubMenuMRU := Config.SubMenuMRU;
  FOldMRUNumber := Config.MRUNumber;
  FOldBackup := Config.Backup;
  FOldBackupNumber := Config.BackupNumber;
  FOldAutorunStartup := Config.AutorunStartup;
  FOldAutorunShutdown := Config.AutorunShutdown;
  FOldCache := Config.Cache;
  FOldScheduler := Config.Scheduler;
  FOldRunSingleClick := Config.RunSingleClick;
  FOldTrayIcon := Config.TrayIcon;
  FOldGMFade := Config.GMFade;
  FOldActionOnExe := Ord(Config.ActionOnExe);
  FOldActionLeft := Ord(Config.ActionClickLeft);
  FOldActionRight := Ord(Config.ActionClickRight);

  FOldFont := TFont.Create;
  FOldFont.Assign(Config.TVFont);
end;

procedure TTestXmlSettings.TearDown;
begin
  Config.GMTheme := FOldGMTheme;
  Config.CustomTitleString := FOldTitle;
  Config.TVBackgroundPath := FOldBackgroundPath;
  Config.TrayCustomIconPath := FOldTrayIconPath;
  Config.GMPersonalPicture := FOldPersonalPicture;
  Config.ShowPanelAtStartUp := FOldShowPanel;
  Config.ShowGraphicMenuAtStartUp := FOldShowMenu;
  Config.UseCustomTitle := FOldUseCustomTitle;
  Config.HideTabSearch := FOldHideSearch;
  Config.HoldSize := FOldHoldSize;
  Config.AlwaysOnTop := FOldOnTop;
  Config.TVBackground := FOldBackground;
  Config.TVAutoOpClCats := FOldAutoOpCl;
  Config.MRU := FOldMRU;
  Config.SubMenuMRU := FOldSubMenuMRU;
  Config.MRUNumber := FOldMRUNumber;
  Config.Backup := FOldBackup;
  Config.BackupNumber := FOldBackupNumber;
  Config.AutorunStartup := FOldAutorunStartup;
  Config.AutorunShutdown := FOldAutorunShutdown;
  Config.Cache := FOldCache;
  Config.Scheduler := FOldScheduler;
  Config.RunSingleClick := FOldRunSingleClick;
  Config.TrayIcon := FOldTrayIcon;
  Config.GMFade := FOldGMFade;
  Config.ActionOnExe := TActionOnExecute(FOldActionOnExe);
  Config.ActionClickLeft := TTrayiconActionClick(FOldActionLeft);
  Config.ActionClickRight := TTrayiconActionClick(FOldActionRight);
  Config.TVFont := FOldFont;
  FOldFont.Free;
  Config.HotKey := FOldHotKey;
  inherited TearDown;
end;

procedure TTestXmlSettings.TestLoadMapsOptionValues;
var
  Doc: TXMLDocument;
begin
  // Deterministic tree font style: the loader adds to whatever is there
  Config.TVFont.Style := [];

  Doc := LoadDoc(
    '<MenuTheme>mytheme</MenuTheme>' +
    '<StartUpShowPanel>1</StartUpShowPanel>' +
    '<StartUpShowMenu>1</StartUpShowMenu>' +
    '<CustomTitleString>My Title</CustomTitleString>' +
    '<CustomTitle>1</CustomTitle>' +
    '<HideSearch>1</HideSearch>' +
    '<HoldSize>1</HoldSize>' +
    '<MainOnTop>1</MainOnTop>' +
    '<BackgroundPath>bg.png</BackgroundPath>' +
    '<Background>1</Background>' +
    '<AutoOpClCategories>1</AutoOpClCategories>' +
    '<TreeViewFontName>Courier New</TreeViewFontName>' +
    '<TreeViewFontSize>12</TreeViewFontSize>' +
    '<TreeViewFontColor>255</TreeViewFontColor>' +
    '<TreeViewFontStyle><fsBold>1</fsBold><fsItalic>1</fsItalic>' +
    '</TreeViewFontStyle>' +
    '<ActiveMRU>0</ActiveMRU>' +
    '<ActiveSubMenuMRU>1</ActiveSubMenuMRU>' +
    '<MRUNumber>7</MRUNumber>' +
    '<ActiveBackup>0</ActiveBackup>' +
    '<BackupNumber>3</BackupNumber>' +
    '<ActiveAutorun>0</ActiveAutorun>' +
    '<ActiveScheduler>0</ActiveScheduler>' +
    '<ActionOnExe>2</ActionOnExe>' +
    '<RunSingleClick>1</RunSingleClick>' +
    '<TrayIconPath>tray.ico</TrayIconPath>' +
    '<ActionClickLeft>1</ActionClickLeft>' +
    '<ActionClickRight>3</ActionClickRight>' +
    '<MenuFade>0</MenuFade>' +
    '<MenuPersonalPicture>pic.png</MenuPersonalPicture>');
  try
    LoadXMLSettings(Doc);

    AssertEquals('theme', 'mytheme', UTF8Encode(Config.GMTheme));
    AssertTrue('panel at startup', Config.ShowPanelAtStartUp);
    AssertTrue('menu at startup', Config.ShowGraphicMenuAtStartUp);
    AssertEquals('title', 'My Title', UTF8Encode(Config.CustomTitleString));
    // ClassicMenu (absent, i.e. false) overwrites the CustomTitle flag
    AssertFalse('custom title flag overwritten', Config.UseCustomTitle);
    AssertTrue('hide search', Config.HideTabSearch);
    AssertTrue('hold size', Config.HoldSize);
    AssertTrue('always on top', Config.AlwaysOnTop);
    AssertEquals('background path', 'bg.png',
      UTF8Encode(Config.TVBackgroundPath));
    AssertTrue('background', Config.TVBackground);
    AssertTrue('auto open-close categories', Config.TVAutoOpClCats);
    AssertEquals('font name', 'Courier New', UTF8Encode(Config.TVFont.Name));
    AssertEquals('font size', 12, Config.TVFont.Size);
    AssertEquals('font color', 255, Config.TVFont.Color);
    AssertTrue('font bold', fsBold in Config.TVFont.Style);
    AssertTrue('font italic', fsItalic in Config.TVFont.Style);
    AssertFalse('mru off', Config.MRU);
    AssertTrue('sub menu mru on', Config.SubMenuMRU);
    AssertEquals('mru number', 7, Config.MRUNumber);
    AssertFalse('backup off', Config.Backup);
    AssertEquals('backup number', 3, Config.BackupNumber);
    AssertFalse('autorun startup off', Config.AutorunStartup);
    AssertFalse('autorun shutdown off', Config.AutorunShutdown);
    AssertFalse('scheduler off', Config.Scheduler);
    AssertTrue('run and hide', Config.ActionOnExe = aeRunAndHide);
    AssertTrue('single click', Config.RunSingleClick);
    AssertEquals('tray icon path', 'tray.ico',
      UTF8Encode(Config.TrayCustomIconPath));
    AssertTrue('left click shows window',
      Config.ActionClickLeft = tcShowWindow);
    AssertTrue('right click classic menu',
      Config.ActionClickRight = tcShowClassicMenu);
    AssertFalse('menu fade off', Config.GMFade);
    AssertEquals('personal picture', 'pic.png',
      UTF8Encode(Config.GMPersonalPicture));
  finally
    Doc.Free;
  end;
end;

procedure TTestXmlSettings.TestLoadAppliesDefaultsToEmptyOption;
var
  Doc: TXMLDocument;
begin
  Doc := LoadDoc('');
  try
    LoadXMLSettings(Doc);

    AssertEquals('default theme', 'default', UTF8Encode(Config.GMTheme));
    AssertTrue('panel at startup default', Config.ShowPanelAtStartUp);
    AssertFalse('menu at startup default', Config.ShowGraphicMenuAtStartUp);
    AssertEquals('default title', UTF8Encode(APP_TITLE),
      UTF8Encode(Config.CustomTitleString));
    AssertEquals('default mru number', 5, Config.MRUNumber);
    AssertEquals('default backup number', 5, Config.BackupNumber);
    AssertTrue('mru on default', Config.MRU);
    AssertTrue('run exe default action', Config.ActionOnExe = aeDefault);
    AssertTrue('left click default', Config.ActionClickLeft = tcNone);
    AssertTrue('right click default',
      Config.ActionClickRight = tcShowGraphicMenu);
    AssertEquals('default font name', 'MS Sans Serif',
      UTF8Encode(Config.TVFont.Name));
    AssertEquals('default font size', 8, Config.TVFont.Size);
  finally
    Doc.Free;
  end;
end;

initialization
  RegisterTest(TTestXmlSettings);

end.
