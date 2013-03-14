program ASuite;

{$MODE Delphi}

uses
  Forms, FileUtil, sqlite3laz, InterfaceBase, win32int,
  SysUtils,
  LCLIntf, LCLType, LMessages, Interfaces,
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
  ulDatabase in 'data\ulDatabase.pas',
  udClassicMenu in 'udClassicMenu.pas' {ClassicMenu: TDataModule},
  PropertySeparator in 'PropertySeparator.pas' {frmPropertySeparator},
  ulExeUtils in 'library\ulExeUtils.pas',
  ImportList in 'ImportList.pas' {frmImportList},
  ASuiteForm in 'library\ASuiteForm.pas',
  ulTreeView in 'library\ulTreeView.pas',
  ulSQLite in 'library\ulSQLite.pas';

var
  cTempo1,cTempo2 : Cardinal;
  myTextFile   : TextFile;

{$R *.res}

begin
  if not CheckPrevious.RestoreIfRunning(TWin32WidgetSet(WidgetSet).AppHandle, 1) then
  begin
    cTempo1 := GetTickCount;
    Application.Initialize;
    Application.Title := APP_TITLE;
    SetCurrentDirUTF8(SUITE_WORKING_PATH); { *Converted from SetCurrentDir*  }
    Application.CreateForm(TImagesDM, ImagesDM);
    Application.CreateForm(TClassicMenu, ClassicMenu);

    Config    := TConfiguration.Create;
    DBManager := TDBManager.Create(false,'');

    Application.CreateForm(TfrmMain, frmMain);
    Application.ShowMainForm := false;
    if (not(Config.ShowPanelAtStartUp)) then
      frmMain.close
    else begin
      frmMain.Visible := true;
      if Not(FileExistsUTF8(SUITE_LIST_PATH) { *Converted from FileExists*  }) then
      begin
        //Create folder cache, if it doesn't exist
        if (not DirectoryExistsUTF8(SUITE_CACHE_PATH) { *Converted from DirectoryExists*  }) then
          CreateDirUTF8(SUITE_CACHE_PATH); { *Converted from CreateDir*  }
        Application.CreateForm(TfrmAbout, frmAbout);
        frmAbout.show;
      end;
    end;
    if (Config.ShowMenuAtStartUp) then
      ClassicMenu.ShowTrayiconMenu(ClassicMenu.pmTrayicon);
    //Timing startup
    if ParamStr(1) = 'debug' then
    begin
      cTempo2 := GetTickCount;
      AssignFile(myTextFile, 'Debug.txt');
      if Not(FileExistsUTF8('Debug.txt') { *Converted from FileExists*  }) then
        ReWrite(myTextFile)
      else
        Append(myTextFile);
      WriteLn(myTextFile, DateTimeToStr(now) + ' = ' + IntToStr(cTempo2 - cTempo1));
      CloseFile(myTextFile);
    end;
    Application.Run;
  end;
end.
