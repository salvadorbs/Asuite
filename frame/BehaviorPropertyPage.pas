unit BehaviorPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.StdCtrls;

type
  TfrmBehaviorPropertyPage = class(TfrmBaseEntityPage)
    grpAutoExecute: TGroupBox;
    grpWindowState: TGroupBox;
    grpOnExecute: TGroupBox;
    cxAutoExecute: TComboBox;
    btnChangeOrder: TButton;
    cxActionOnExe: TComboBox;
    cxWindowState: TComboBox;
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
  frmBehaviorPropertyPage: TfrmBehaviorPropertyPage;

implementation

{$R *.dfm}

{ TfrmMenuPropertyPage }

function TfrmBehaviorPropertyPage.GetTitle: string;
begin
  Result := 'Behavior';
end;

function TfrmBehaviorPropertyPage.InternalLoadData: Boolean;
begin

end;

function TfrmBehaviorPropertyPage.InternalSaveData: Boolean;
begin

end;

end.
