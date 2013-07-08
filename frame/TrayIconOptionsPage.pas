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
    procedure cbClassicMenuClick(Sender: TObject);
    procedure cbTrayCustomIconClick(Sender: TObject);
    procedure cbTrayiconClick(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function InternalLoadData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmTrayiconOptionsPage: TfrmTrayiconOptionsPage;

implementation

uses
  ulAppConfig, AppConfig;

{$R *.dfm}

{ TfrmTrayiconOptionsPage }

procedure TfrmTrayiconOptionsPage.cbClassicMenuClick(Sender: TObject);
begin
  if (cbTrayicon.Checked) then
  begin
    cbMenuFade.Enabled   := Not(cbClassicMenu.Checked);
    cxTheme.Enabled      := Not(cbClassicMenu.Checked);
    cbSubMenuMRU.Enabled := cbClassicMenu.Checked;
    cbSubMenuMFU.Enabled := cbClassicMenu.Checked;
  end
  else begin
    cbMenuFade.Enabled   := cbTrayicon.Checked;
    cxTheme.Enabled      := cbTrayicon.Checked;
    cbSubMenuMRU.Enabled := cbTrayicon.Checked;
    cbSubMenuMFU.Enabled := cbTrayicon.Checked;
  end;
end;

procedure TfrmTrayiconOptionsPage.cbTrayCustomIconClick(Sender: TObject);
begin
  edtCustomIcon.Enabled := cbTrayCustomIcon.Checked;
  btnBrowse.Enabled     := cbTrayCustomIcon.Checked;
end;

procedure TfrmTrayiconOptionsPage.cbTrayiconClick(Sender: TObject);
begin
  cbClassicMenu.Enabled    := cbTrayicon.Checked;
  cbTrayCustomIcon.Enabled := cbTrayicon.Checked;
  btnBrowse.Enabled        := cbTrayicon.Checked;
  cxLeftClick.Enabled      := cbTrayicon.Checked;
  cxRightClick.Enabled     := cbTrayicon.Checked;
  cbClassicMenuClick(Self);
end;

function TfrmTrayiconOptionsPage.GetTitle: string;
begin
  Result := 'TrayIcon';
end;

function TfrmTrayiconOptionsPage.InternalLoadData: Boolean;
var
  searchResult : TSearchRec;
begin
  inherited;
  //Trayicon
  cbTrayicon.Checked        := Config.TrayIcon;
  cbTrayCustomIcon.Checked  := Config.TrayUseCustomIcon;
  edtCustomIcon.Text        := Config.TrayCustomIconPath;
  cbClassicMenu.Checked     := Config.UseClassicMenu;
  cxLeftClick.ItemIndex     := Config.ActionClickLeft;
  cxRightClick.ItemIndex    := Config.ActionClickRight;
  //Graphic Menu
  cbMenuFade.Checked := Config.GMFade;
  //Get GM theme list
  if FindFirst(SUITE_MENUTHEMES_PATH + '*.*', faDirectory, searchResult) = 0 then
  begin
    repeat
      if ((searchResult.Name <> '.') and (searchResult.Name <> '..')) and
         ((searchResult.Attr and faDirectory) = (faDirectory)) then
        cxTheme.AddItem(SearchResult.Name,Self);
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;
  cxTheme.ItemIndex  := cxTheme.Items.IndexOf(Config.GMTheme);
  //Submenu
  cbSubMenuMRU.Checked := Config.SubMenuMRU;
  cbSubMenuMFU.Checked := Config.SubMenuMFU;
  //Enable/disable visual components
  cbTrayiconClick(Self);
  cbTrayCustomIconClick(Self);
  cbClassicMenuClick(Self);
end;

end.
