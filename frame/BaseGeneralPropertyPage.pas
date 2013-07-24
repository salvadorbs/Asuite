unit BaseGeneralPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.StdCtrls;

type
  TfrmBaseGeneralPropertyPage = class(TfrmBaseEntityPage)
    gbItem: TGroupBox;
    lbName: TLabel;
    edtName: TEdit;
    lbPathIcon: TLabel;
    edtPathIcon: TEdit;
    btnBrowseIcon: TButton;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmBaseGeneralPropertyPage: TfrmBaseGeneralPropertyPage;

implementation

{$R *.dfm}

{ TfrmGeneralPropertyPage }

function TfrmBaseGeneralPropertyPage.GetTitle: string;
begin
  Result := 'General';
end;

function TfrmBaseGeneralPropertyPage.InternalLoadData: Boolean;
begin

end;

function TfrmBaseGeneralPropertyPage.InternalSaveData: Boolean;
begin

end;

end.
