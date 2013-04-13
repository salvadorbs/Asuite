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
  PropertyFile in 'PropertyFile.pas' {frmPropertyFile},
  PropertyCat in 'PropertyCat.pas' {frmPropertyCat},
  About in 'About.pas' {frmAbout},
  ClearElements in 'ClearElements.pas' {frmClearElements},
  Option in 'Option.pas' {frmOption},
  CheckPrevious in '3p\CheckPrevious.pas',
  ulNodeDataTypes in 'library\ulNodeDataTypes.pas',
  OrderSoftware in 'OrderSoftware.pas' {frmOrderSoftware},
  ulEnumerations in 'library\ulEnumerations.pas',
  ulSysUtils in 'library\ulSysUtils.pas',
  ulStringUtils in 'library\ulStringUtils.pas',
  udImages in 'udImages.pas' {ImagesDM: TDataModule},
  ulDatabase in 'library\ulDatabase.pas',
  udClassicMenu in 'udClassicMenu.pas' {ClassicMenu: TDataModule},
  PropertySeparator in 'PropertySeparator.pas' {frmPropertySeparator},
  ulExeUtils in 'library\ulExeUtils.pas',
  ImportList in 'ImportList.pas' {frmImportList},
  Sensor in 'Sensor.pas' {frmSensor},
  ASuiteForm in 'library\ASuiteForm.pas',
  ulTreeView in 'library\ulTreeView.pas',
  Stats in 'Stats.pas' {frmStats},
  SynSQLite3Static,
  DCPbase64 in '3p\gnugettext\DCPbase64.pas',
  gnugettext in '3p\gnugettext\gnugettext.pas',
  GTForm in '3p\gnugettext\GTForm.pas',
  GTLanguageFrame in '3p\gnugettext\GTLanguageFrame.pas' {GTfraLanguage: TFrame},
  GTLanguageList in '3p\gnugettext\GTLanguageList.pas',
  GTLanguagesEx in '3p\gnugettext\GTLanguagesEx.pas';

//SQLite3 static library

{$IFDEF DEBUG}
var
  cTempo1,cTempo2 : Cardinal;
  myTextFile   : TextFile;
{$ENDIF}

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  if not CheckPrevious.RestoreIfRunning(Application.Handle, 1) then
  begin
    {$IFDEF DEBUG}
    cTempo1 := GetTickCount;
    {$ENDIF}
    Application.Initialize;
    Application.Title := APP_TITLE;
    SetCurrentDir(SUITE_WORKING_PATH);
    Application.CreateForm(TImagesDM, ImagesDM);
    Application.CreateForm(TClassicMenu, ClassicMenu);
    Config    := TConfiguration.Create;
    DBManager := TDBManager.Create(SUITE_LIST_PATH);

    Application.CreateForm(TfrmMain, frmMain);
    Application.ShowMainForm := false;
    { TODO : Review this IF/Else }
    if (not(Config.ShowPanelAtStartUp)) then
      frmMain.close
    else begin
      frmMain.Visible := true;
      if Not(FileExists(SUITE_LIST_PATH)) then
      begin
        Application.CreateForm(TfrmAbout, frmAbout);
        frmAbout.show;
      end;
    end;
    if (Config.ShowMenuAtStartUp) then
      ClassicMenu.ShowTrayiconMenu;
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
    Application.Run;
  end;
end.
