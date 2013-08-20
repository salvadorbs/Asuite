unit SensorsOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.StdCtrls, DKLang;

type
  TfrmSensorsOptionsPage = class(TfrmBaseEntityPage)
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
    cbMouseSensors: TCheckBox;
    lbBottom: TLabel;
    lbRight: TLabel;
    lbLeft: TLabel;
    lbTop: TLabel;
    DKLanguageController1: TDKLanguageController;
    procedure cbMouseSensorsClick(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmSensorsOptionsPage: TfrmSensorsOptionsPage;

implementation

uses
  ulAppConfig, AppConfig;

{$R *.dfm}

{ TfrmSensorsOptionsPage }

procedure TfrmSensorsOptionsPage.cbMouseSensorsClick(Sender: TObject);
begin
  //Left Click
  cxLCTop.Enabled    := cbMouseSensors.Checked;
  cxLCLeft.Enabled   := cbMouseSensors.Checked;
  cxLCRight.Enabled  := cbMouseSensors.Checked;
  cxLCBottom.Enabled := cbMouseSensors.Checked;
  //Right Click
  cxRCTop.Enabled    := cbMouseSensors.Checked;
  cxRCLeft.Enabled   := cbMouseSensors.Checked;
  cxRCRight.Enabled  := cbMouseSensors.Checked;
  cxRCBottom.Enabled := cbMouseSensors.Checked;
end;

function TfrmSensorsOptionsPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_Mouse;
end;

function TfrmSensorsOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgMouseSensors');
end;

function TfrmSensorsOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  cbMouseSensors.Checked := Config.UseMouseSensors;
  //Left Click
  cxLCTop.ItemIndex    := Config.SensorLeftClick[0];
  cxLCLeft.ItemIndex   := Config.SensorLeftClick[1];
  cxLCRight.ItemIndex  := Config.SensorLeftClick[2];
  cxLCBottom.ItemIndex := Config.SensorLeftClick[3];
  //Right Click
  cxRCTop.ItemIndex    := Config.SensorRightClick[0];
  cxRCLeft.ItemIndex   := Config.SensorRightClick[1];
  cxRCRight.ItemIndex  := Config.SensorRightClick[2];
  cxRCBottom.ItemIndex := Config.SensorRightClick[3];
  //Enable/Disable visual components
  cbMouseSensorsClick(Self);
end;

function TfrmSensorsOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  //Mouse Sensors
  Config.UseMouseSensors     := cbMouseSensors.Checked;
  //Left
  Config.SensorLeftClick[0]  := cxLCTop.ItemIndex;
  Config.SensorLeftClick[1]  := cxLCLeft.ItemIndex;
  Config.SensorLeftClick[2]  := cxLCRight.ItemIndex;
  Config.SensorLeftClick[3]  := cxLCBottom.ItemIndex;
  //Right
  Config.SensorRightClick[0] := cxRCTop.ItemIndex;
  Config.SensorRightClick[1] := cxRCLeft.ItemIndex;
  Config.SensorRightClick[2] := cxRCRight.ItemIndex;
  Config.SensorRightClick[3] := cxRCBottom.ItemIndex;
  //Update sensors
  Config.UpdateSensors;
end;

end.
