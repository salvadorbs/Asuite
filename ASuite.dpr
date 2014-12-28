program ASuite;















































uses
  Forms,
  SysUtils,
  Windows,
  SynSQLite3Static,
  Forms.About in 'Forms\Forms.About.pas' {frmAbout},
  Forms.GraphicMenu in 'Forms\Forms.GraphicMenu.pas' {frmGraphicMenu},
  Forms.ImportList in 'Forms\Forms.ImportList.pas' {frmImportList},
  Forms.Main in 'Forms\Forms.Main.pas' {frmMain},
  Forms.Options in 'Forms\Forms.Options.pas' {frmOptions},
  Forms.PropertyItem in 'Forms\Forms.PropertyItem.pas' {frmPropertyItem},
  Forms.PropertySeparator in 'Forms\Forms.PropertySeparator.pas' {frmPropertySeparator},
  Forms.ScanFolder in 'Forms\Forms.ScanFolder.pas' {frmScanFolder},
  DataModules.Images in 'DataModules\DataModules.Images.pas' {ImagesDM: TDataModule},
  DataModules.TrayMenu in 'DataModules\DataModules.TrayMenu.pas' {ClassicMenu: TDataModule},
  Database in 'Library\Database.pas',
  Kernel.AppConfig in 'Library\Kernel.AppConfig.pas',
  Kernel.BaseMainForm in 'Library\Kernel.BaseMainForm.pas',
  Kernel.Consts in 'Library\Kernel.Consts.pas',
  Kernel.Enumerations in 'Library\Kernel.Enumerations.pas',
  NodeDataTypes in 'Library\NodeDataTypes.pas',
  ulCommonClasses in 'Library\ulCommonClasses.pas',
  ulCommonUtils in 'Library\ulCommonUtils.pas',
  Utility.FileFolder in 'Utilities\Utility.FileFolder.pas',
  Utility.Strings in 'Utilities\Utility.Strings.pas',
  Utility.System in 'Utilities\Utility.System.pas',
  Utility.TreeView in 'Utilities\Utility.TreeView.pas',
  Utility.XML in 'Utilities\Utility.XML.pas',
  Frame.BaseEntity in 'Frame\Frame.BaseEntity.pas' {frmBaseEntityPage: TFrame},
  Frame.Properties.Base in 'Frame\Frame.Properties.Base.pas' {frmBasePropertyPage: TFrame},
  Frame.Properties.General in 'Frame\Frame.Properties.General.pas' {frmBaseGeneralPropertyPage: TFrame},
  Frame.Options.Advanced in 'Frame\Frame.Options.Advanced.pas' {frmAdvancedOptionsPage: TFrame},
  Frame.Options.General in 'Frame\Frame.Options.General.pas' {frmGeneralOptionsPage: TFrame},
  Frame.Options.Hotkey in 'Frame\Frame.Options.Hotkey.pas' {frmHotkeyOptionsPage: TFrame},
  Frame.Options.Items in 'Frame\Frame.Options.Items.pas' {frmItemsOptionsPage: TFrame},
  Frame.Options.Stats in 'Frame\Frame.Options.Stats.pas' {frmStatsOptionsPage: TFrame},
  Frame.Options.Trayicon in 'Frame\Frame.Options.Trayicon.pas' {frmTrayiconOptionsPage: TFrame},
  Frame.Properties.Advanced in 'Frame\Frame.Properties.Advanced.pas' {frmAdvancedPropertyPage: TFrame},
  Frame.Properties.Behavior in 'Frame\Frame.Properties.Behavior.pas' {frmBehaviorPropertyPage: TFrame},
  Frame.Properties.General.Category in 'Frame\Frame.Properties.General.Category.pas' {frmCatGeneralPropertyPage: TFrame},
  Frame.Properties.General.Software in 'Frame\Frame.Properties.General.Software.pas' {frmSWGeneralPropertyPage: TFrame},
  Utility.Frame in 'Utilities\Utility.Frame.pas',
  Utility.Process in 'Utilities\Utility.Process.pas';

//SQLite3 static library

{$IFDEF DEBUG}
var
  cTempo1,cTempo2 : Cardinal;
  myTextFile   : TextFile;
{$ENDIF}

{$R *.res}
{$R *.dkl_const.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  //TODO: Use another single instance solution (see Delphi Dabbler)
//  if not CheckPrevious.RestoreIfRunning(Application.Handle, 1) then
  begin
    {$IFDEF DEBUG}
    cTempo1 := GetTickCount;
    {$ENDIF}

    Application.Initialize;
    SetCurrentDir(SUITE_WORKING_PATH);
    Config    := TConfiguration.Create;
    Application.Title := APP_TITLE;

    Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGraphicMenu, frmGraphicMenu);
  Application.CreateForm(TImagesDM, ImagesDM);
  Application.CreateForm(TClassicMenu, ClassicMenu);
  //Show MainForm and/or TrayMenu
    Application.ShowMainForm := Config.ShowPanelAtStartUp;
    if (Config.ShowMenuAtStartUp) then
      ClassicMenu.ShowTrayiconMenu;
    Application.Run;

    {$IFDEF DEBUG}
    //Timing startup
    cTempo2 := GetTickCount;
    AssignFile(myTextFile, DEBUG_FILE);
    if Not(FileExists(DEBUG_FILE)) then
      ReWrite(myTextFile)
    else
      Append(myTextFile);
    WriteLn(myTextFile, DateTimeToStr(now) + ' = ' + IntToStr(cTempo2 - cTempo1));
    CloseFile(myTextFile);
    {$ENDIF}
  end;
end.
