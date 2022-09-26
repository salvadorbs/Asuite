program ASuite;

{$MODE Delphi}

{$I ASuite.inc}

uses
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  Forms,
  SysUtils,
  LCLIntf, Interfaces,
  mormot.db.raw.sqlite3.static,
  Kernel.Instance in 'Library\Kernel.Instance.pas',
  Kernel.Manager in 'Library\Kernel.Manager.pas',
  AppConfig.Main in 'Library\AppConfig.Main.pas',
  Forms.GraphicMenu in 'Forms\Forms.GraphicMenu.pas' {frmGraphicMenu},
  Forms.Main in 'Forms\Forms.Main.pas' {frmMain},
  DataModules.TrayMenu in 'DataModules\DataModules.TrayMenu.pas' {dmTrayMenu: TDataModule},
  Kernel.Consts in 'Library\Kernel.Consts.pas',
  UniqueInstanceRaw,
  HlpHashFactory;

{$R *.res} 

var
  hash, identifier: string;

begin
  globalSkipIfNoLeaks := True;

  hash := THashFactory.THash32.CreateXXHash32().ComputeString(Application.ExeName, TEncoding.UTF8).ToString();
  identifier := 'ASuite.SingleInstance.' + hash;

  if not(InstanceRunning(identifier, True)) then
  begin
    // Set up -gh output for the Leakview package:
    if FileExists('heap.trc') then
      DeleteFile('heap.trc');
    SetHeapTraceOutput('heap.trc');
                                    
    Application.Scaled := True;
    Application.Initialize;
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
