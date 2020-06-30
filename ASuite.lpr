program ASuite;

{$MODE DelphiUnicode}

uses
  Forms,
  SysUtils,
  LCLIntf, LCLType, LMessages, Interfaces,
  SynSQLite3Static,
  Forms.About in 'Forms\Forms.About.pas' {frmAbout},
  Forms.GraphicMenu in 'Forms\Forms.GraphicMenu.pas' {frmGraphicMenu},
  Forms.ImportList in 'Forms\Forms.ImportList.pas' {frmImportList},
  Forms.Main in 'Forms\Forms.Main.pas' {frmMain},
  Forms.PropertySeparator in 'Forms\Forms.PropertySeparator.pas' {frmPropertySeparator},
  Forms.ScanFolder in 'Forms\Forms.ScanFolder.pas' {frmScanFolder},
  DataModules.Icons in 'DataModules\DataModules.Icons.pas' {dmImages: TDataModule},
  DataModules.TrayMenu in 'DataModules\DataModules.TrayMenu.pas' {dmTrayMenu: TDataModule},
  AppConfig.Main in 'Library\AppConfig.Main.pas',
  Kernel.BaseMainForm in 'Library\Kernel.BaseMainForm.pas',
  Kernel.Consts in 'Library\Kernel.Consts.pas',
  Kernel.Enumerations in 'Library\Kernel.Enumerations.pas',
  Utility.FileFolder in 'Utilities\Utility.FileFolder.pas',
  Utility.System in 'Utilities\Utility.System.pas',
  Utility.XML in 'Utilities\Utility.XML.pas',
  Frame.BaseEntity in 'Frame\Frame.BaseEntity.pas' {frmBaseEntityPage: TFrame},
  Frame.Properties.Base in 'Frame\Frame.Properties.Base.pas' {frmBasePropertyPage: TFrame},
  Frame.Properties.General in 'Frame\Frame.Properties.General.pas' {frmBaseGeneralPropertyPage: TFrame},
  Frame.Options.Advanced in 'Frame\Frame.Options.Advanced.pas' {frmAdvancedOptionsPage: TFrame},
  Frame.Options.General in 'Frame\Frame.Options.General.pas' {frmGeneralOptionsPage: TFrame},
  Frame.Options.Hotkey in 'Frame\Frame.Options.Hotkey.pas' {frmHotkeyOptionsPage: TFrame},
  Frame.Options.Autorun in 'Frame\Frame.Options.Autorun.pas' {frmAutorunOptionsPage: TFrame},
  Frame.Options.Stats in 'Frame\Frame.Options.Stats.pas' {frmStatsOptionsPage: TFrame},
  Frame.Options.Trayicon in 'Frame\Frame.Options.Trayicon.pas' {frmTrayiconOptionsPage: TFrame},
  Frame.Properties.Advanced in 'Frame\Frame.Properties.Advanced.pas' {frmAdvancedPropertyPage: TFrame},
  Frame.Properties.Behavior in 'Frame\Frame.Properties.Behavior.pas' {frmBehaviorPropertyPage: TFrame},
  Frame.Properties.General.Category in 'Frame\Frame.Properties.General.Category.pas' {frmCatGeneralPropertyPage: TFrame},
  Frame.Properties.General.Software in 'Frame\Frame.Properties.General.Software.pas' {frmSWGeneralPropertyPage: TFrame},
  Utility.Process in 'Utilities\Utility.Process.pas',
  Database.Version in 'Library\Database.Version.pas',
  Database.Options in 'Library\Database.Options.pas',
  Database.List in 'Library\Database.List.pas',
  Database.Manager in 'Library\Database.Manager.pas',
  NodeDataTypes.Files in 'Library\NodeDataTypes.Files.pas',
  NodeDataTypes.Separator in 'Library\NodeDataTypes.Separator.pas',
  NodeDataTypes.Base in 'Library\NodeDataTypes.Base.pas',
  NodeDataTypes.Category in 'Library\NodeDataTypes.Category.pas',
  NodeDataTypes.Custom in 'Library\NodeDataTypes.Custom.pas',
  Kernel.Types in 'Library\Kernel.Types.pas',
  Lists.Base in 'Library\Lists.Base.pas',
  Lists.Special in 'Library\Lists.Special.pas',
  Lists.HotKey in 'Library\Lists.HotKey.pas',
  Lists.Manager in 'Library\Lists.Manager.pas',
  Kernel.Singleton in 'Library\Kernel.Singleton.pas',
  AppConfig.Paths in 'Library\AppConfig.Paths.pas',
  Kernel.PopupMenu in 'Library\Kernel.PopupMenu.pas',
  Utility.Conversions in 'Utilities\Utility.Conversions.pas',
  Utility.Misc in 'Utilities\Utility.Misc.pas',
  Forms.ShortcutGrabber in 'Forms\Forms.ShortcutGrabber.pas' {frmShortcutGrabber},
  UniqueInstanceRaw,
  HlpXXHash32,
  HlpHashFactory,
  VirtualTree.Events in 'Library\VirtualTree.Events.pas',
  Forms.Dialog.BaseEntity in 'Forms\Forms.Dialog.BaseEntity.pas' {frmDialogBase},
  Forms.Options in 'Forms\Forms.Options.pas' {frmOptions},
  Forms.PropertyItem in 'Forms\Forms.PropertyItem.pas' {frmPropertyItem},
  VirtualTree.Methods in 'Library\VirtualTree.Methods.pas',
  GraphicMenu.ThemeEngine.Consts in 'Library\GraphicMenu.ThemeEngine.Consts.pas',
  GraphicMenu.ThemeEngine in 'Library\GraphicMenu.ThemeEngine.pas',
  Icons.Node in 'Library\Icons.Node.pas',
  Icons.Manager in 'Library\Icons.Manager.pas',
  Icons.Base in 'Library\Icons.Base.pas',
  Icons.Application in 'Library\Icons.Application.pas',
  Frame.Options.MainWindow in 'Frame\Frame.Options.MainWindow.pas' {frmMainWindowOptionsPage: TFrame},
  Icons.Thread in 'Library\Icons.Thread.pas',
  Kernel.Scheduler in 'Library\Kernel.Scheduler.pas',
  Kernel.Logger in 'Library\Kernel.Logger.pas';

//SQLite3 static library

{$R *.res}
{$R *.dkl_const.res}

var
  hash, identifier: string;

begin            
  hash := THashFactory.THash32.CreateXXHash32().ComputeString(Application.ExeName, TEncoding.UTF8).ToString();
  identifier := 'ASuite.SingleInstance.' + hash;

  if not(InstanceRunning(identifier, True)) then
  begin
    {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown := True;
    {$ENDIF}

    Application.Initialize;
    Config    := TConfiguration.Create;
  Application.Title := APP_TITLE;

    Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGraphicMenu, frmGraphicMenu);
  //Show MainForm and/or TrayMenu
    Application.ShowMainForm := Config.ShowPanelAtStartUp;
    if (Config.ShowGraphicMenuAtStartUp) then
      dmTrayMenu.ShowGraphicMenu;
    Application.Run;
  end;
end.
