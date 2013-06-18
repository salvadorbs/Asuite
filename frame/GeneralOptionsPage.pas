unit GeneralOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmGeneralOptionsPage = class(TfrmBaseOptionsPage)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmGeneralOptionsPage: TfrmGeneralOptionsPage;

implementation

{$R *.dfm}

{ TfrmBaseOptionsPage1 }

function TfrmGeneralOptionsPage.GetTitle: string;
begin
  Result := 'General';
end;

end.
