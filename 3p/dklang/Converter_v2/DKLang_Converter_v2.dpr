program DKLang_Converter_v2;

uses
  Forms,
  Main in 'Main.pas' {fMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
