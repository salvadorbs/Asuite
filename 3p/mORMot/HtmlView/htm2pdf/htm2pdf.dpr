program htm2pdf;

uses
  {$I SynDprUses.inc}
  Forms,
  Main_Form in 'Main_Form.pas' {MainForm},
  Html2Pdf in 'Html2Pdf.pas';

{$I Synopse.inc}

{$R *.res}

begin
  Application.Initialize;
{$ifdef ISDELPHI2007ANDUP}
  Application.MainFormOnTaskbar := True;
{$endif}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
