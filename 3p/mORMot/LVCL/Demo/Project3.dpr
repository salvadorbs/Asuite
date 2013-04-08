program Project3;

uses
  Forms,
  Unit3 in 'Unit3.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
