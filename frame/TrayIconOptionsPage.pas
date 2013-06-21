unit TrayIconOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmTrayiconOptionsPage = class(TfrmBaseOptionsPage)
    gbTrayicon: TGroupBox;
    lbTrayLeftClick: TLabel;
    lbTrayRightClick: TLabel;
    cxLeftClick: TComboBox;
    btnBrowse: TButton;
    cbTrayicon: TCheckBox;
    cxRightClick: TComboBox;
    cbTrayCustomIcon: TCheckBox;
    cbClassicMenu: TCheckBox;
    grpGraphicMenu: TGroupBox;
    cxTheme: TComboBox;
    cbMenuFade: TCheckBox;
    lbMenuTheme: TLabel;
    grpClassicMenu: TGroupBox;
    edtCustomIcon: TEdit;
    cbSubMenuMFU: TCheckBox;
    cbSubMenuMRU: TCheckBox;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmTrayiconOptionsPage: TfrmTrayiconOptionsPage;

implementation

{$R *.dfm}

{ TfrmTrayiconOptionsPage }

function TfrmTrayiconOptionsPage.GetTitle: string;
begin
  Result := 'TrayIcon';
end;

end.
