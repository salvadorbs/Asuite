unit ItemsOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons;

type
  TfrmItemsOptionsPage = class(TfrmBaseOptionsPage)
    gbExecution: TGroupBox;
    lbActionOnExe: TLabel;
    cbRunSingleClick: TCheckBox;
    cxActionOnExe: TComboBox;
    grpOrderSoftware: TGroupBox;
    lblOrderInfo: TLabel;
    pgcSoftwareOrder: TPageControl;
    tsStartUp: TTabSheet;
    lstStartUp: TListBox;
    tsShutdown: TTabSheet;
    lstShutdown: TListBox;
    cbAutorun: TCheckBox;
    btnDown: TBitBtn;
    btnUp: TBitBtn;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmItemsOptionsPage: TfrmItemsOptionsPage;

implementation

{$R *.dfm}

function TfrmItemsOptionsPage.GetTitle: string;
begin
  Result := 'Items';
end;

end.
