program ASuite;

uses
  Forms,
  SysUtils,
  Windows,
  ulCommonUtils in 'library\ulCommonUtils.pas',
  ulCommonClasses in 'library\ulCommonClasses.pas',
  AppConfig in 'AppConfig.pas',
  ulAppConfig in 'library\ulAppConfig.pas',
  Main in 'Main.pas' {frmMain},
  About in 'About.pas' {frmAbout},
  ulNodeDataTypes in 'library\ulNodeDataTypes.pas',
  ulEnumerations in 'library\ulEnumerations.pas',
  ulSysUtils in 'library\ulSysUtils.pas',
  ulStringUtils in 'library\ulStringUtils.pas',
  udImages in 'udImages.pas' {ImagesDM: TDataModule},
  ulDatabase in 'library\ulDatabase.pas',
  udClassicMenu in 'udClassicMenu.pas' {ClassicMenu: TDataModule},
  ulExeUtils in 'library\ulExeUtils.pas',
  ImportList in 'ImportList.pas' {frmImportList},
  Sensor in 'Sensor.pas' {frmSensor},
  ASuiteForm in 'library\ASuiteForm.pas',
  ulTreeView in 'library\ulTreeView.pas',
  SynSQLite3Static,
  GraphicMenu in 'GraphicMenu.pas' {frmGraphicMenu},
  ulFileFolder in 'library\ulFileFolder.pas',
  Options in 'Options.pas' {frmOptions},
  BaseEntityPage in 'frame\BaseEntityPage.pas' {frmBaseEntityPage: TFrame},
  GeneralOptionsPage in 'frame\GeneralOptionsPage.pas' {frmGeneralOptionsPage: TFrame},
  AdvancedOptionsPage in 'frame\AdvancedOptionsPage.pas' {frmAdvancedOptionsPage: TFrame},
  TrayIconOptionsPage in 'frame\TrayIconOptionsPage.pas' {frmTrayiconOptionsPage: TFrame},
  HotkeyOptionsPage in 'frame\HotkeyOptionsPage.pas' {frmHotkeyOptionsPage: TFrame},
  StatsOptionsPage in 'frame\StatsOptionsPage.pas' {frmStatsOptionsPage: TFrame},
  ItemsOptionsPage in 'frame\ItemsOptionsPage.pas' {frmItemsOptionsPage: TFrame},
  SensorsOptionsPage in 'frame\SensorsOptionsPage.pas' {frmSensorsOptionsPage: TFrame},
  PropertyItem in 'PropertyItem.pas' {frmPropertyItem},
  ulFrameUtils in 'library\ulFrameUtils.pas',
  AdvancedPropertyPage in 'frame\AdvancedPropertyPage.pas' {frmAdvancedPropertyPage: TFrame},
  BehaviorPropertyPage in 'frame\BehaviorPropertyPage.pas' {frmBehaviorPropertyPage: TFrame},
  BaseGeneralPropertyPage in 'frame\BaseGeneralPropertyPage.pas' {frmBaseGeneralPropertyPage: TFrame},
  SWGeneralPropertyPage in 'frame\SWGeneralPropertyPage.pas' {frmSWGeneralPropertyPage: TFrame},
  CatGeneralPropertyPage in 'frame\CatGeneralPropertyPage.pas' {frmCatGeneralPropertyPage: TFrame},
  PropertySeparator in 'PropertySeparator.pas' {frmPropertySeparator},
  BasePropertyPage in 'frame\BasePropertyPage.pas' {frmBasePropertyPage: TFrame},
  ScanFolder in 'ScanFolder.pas' {frmScanFolder},
  ulXMLUtils in 'library\ulXMLUtils.pas';

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
