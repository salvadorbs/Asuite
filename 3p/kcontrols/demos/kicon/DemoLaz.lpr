program DemoLaz;

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {MainForm}, LResources;

{$IFDEF WINDOWS}{$ENDIF}

begin
  {$I DemoLaz.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
