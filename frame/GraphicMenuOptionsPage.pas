unit GraphicMenuOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage;

type
  TfrmGraphicMenuOptionsPage = class(TfrmBaseOptionsPage)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmGraphicMenuOptionsPage: TfrmGraphicMenuOptionsPage;

implementation

{$R *.dfm}

{ TfrmGraphicMenuOptionsPage }

function TfrmGraphicMenuOptionsPage.GetTitle: string;
begin
  Result := 'Graphic Menu';
end;

end.
