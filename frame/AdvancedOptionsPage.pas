unit AdvancedOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmAdvancedOptionsPage = class(TfrmBaseOptionsPage)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmAdvancedOptionsPage: TfrmAdvancedOptionsPage;

implementation

{$R *.dfm}

{ TfrmAdvancedOptionsPage }

function TfrmAdvancedOptionsPage.GetTitle: string;
begin
  Result := 'Advanced';
end;

end.
