unit StatsOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmStatsOptionsPage = class(TfrmBaseOptionsPage)
    gbASuite: TGroupBox;
    lbSoftware: TLabel;
    lbCat: TLabel;
    lbTotal: TLabel;
    lbSoftware2: TLabel;
    lbCat2: TLabel;
    lbTotal2: TLabel;
    gbSupport: TGroupBox;
    lbSize: TLabel;
    lbSpaceUsed: TLabel;
    lbSpaceFree: TLabel;
    lbSpaceFree2: TLabel;
    lbSize2: TLabel;
    lbSpaceUsed2: TLabel;
    gbSystem: TGroupBox;
    lbOs: TLabel;
    lbNamePc: TLabel;
    lbUser: TLabel;
    lbOs2: TLabel;
    lbNamePc2: TLabel;
    lbUser2: TLabel;
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
