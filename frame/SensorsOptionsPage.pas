unit SensorsOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmSensorsOptionsPage = class(TfrmBaseOptionsPage)
    gbMouse: TGroupBox;
    lbSide: TLabel;
    lbLeftClick: TLabel;
    lbRightClick: TLabel;
    cxLCTop: TComboBox;
    cxLCLeft: TComboBox;
    cxLCRight: TComboBox;
    cxLCBottom: TComboBox;
    cxRCTop: TComboBox;
    cxRCBottom: TComboBox;
    cxRCRight: TComboBox;
    cxRCLeft: TComboBox;
    chkMouseSensors: TCheckBox;
    lbBottom: TLabel;
    lbRight: TLabel;
    lbLeft: TLabel;
    lbTop: TLabel;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmSensorsOptionsPage: TfrmSensorsOptionsPage;

implementation

{$R *.dfm}

{ TfrmSensorsOptionsPage }

function TfrmSensorsOptionsPage.GetTitle: string;
begin
  Result := 'Mouse Sensors';
end;

end.
