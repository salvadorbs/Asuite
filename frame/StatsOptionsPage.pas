unit StatsOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage;

type
  TfrmStatsOptionsPage = class(TfrmBaseOptionsPage)
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmStatsOptionsPage: TfrmStatsOptionsPage;

implementation

{$R *.dfm}

{ TfrmStatsOptionsPage }

function TfrmStatsOptionsPage.GetTitle: string;
begin
  Result := 'Stats';
end;

end.
