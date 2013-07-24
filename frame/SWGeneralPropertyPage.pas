unit SWGeneralPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseGeneralPropertyPage, Vcl.StdCtrls;

type
  TfrmSWGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    grpSoftware: TGroupBox;
    btnBrowseWorkingDir: TButton;
    edtWorkingDir: TEdit;
    lbWorkingDir: TLabel;
    btnBrowseExe: TButton;
    lbInfo2: TLabel;
    edtParameters: TEdit;
    lbParameters: TLabel;
    edtPathExe: TEdit;
    lbPathExe: TLabel;
  private
    { Private declarations }
  strict protected
    function InternalLoadData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmSWGeneralPropertyPage: TfrmSWGeneralPropertyPage;

implementation

uses
  AppConfig;

{$R *.dfm}

function TfrmSWGeneralPropertyPage.InternalLoadData: Boolean;
begin
  inherited;
  lbInfo2.Caption := Format(lbInfo2.Caption, [SUITE_WORKING_PATH, SUITE_DRIVE]);
end;

end.
