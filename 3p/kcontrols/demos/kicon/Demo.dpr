program Demo;

{$include KControls.inc}

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}
{$R XPman.res}

begin
{$IFDEF COMPILER10_UP}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF} 
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
