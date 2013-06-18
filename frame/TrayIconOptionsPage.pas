unit TrayIconOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage;

type
  TfrmTrayiconOptionsPage = class(TfrmBaseOptionsPage)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmTrayiconOptionsPage: TfrmTrayiconOptionsPage;

implementation

{$R *.dfm}

{ TfrmTrayiconOptionsPage }

function TfrmTrayiconOptionsPage.GetTitle: string;
begin
  Result := 'TrayIcon';
end;

end.
