unit ClassicMenuOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage;

type
  TfrmClassicMenuOptionsPage = class(TfrmBaseOptionsPage)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmClassicMenuOptionsPage: TfrmClassicMenuOptionsPage;

implementation

{$R *.dfm}

{ TfrmClassicMenuOptionsPage }

function TfrmClassicMenuOptionsPage.GetTitle: string;
begin
  Result := 'Classic Menu';
end;

end.
