unit HotkeyOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmHotkeyOptionsPage = class(TfrmBaseOptionsPage)
    gbHotkey: TGroupBox;
    cxWindowHotKeyCode: TComboBox;
    cxWindowHotKeyMod: TComboBox;
    cbWindowHotKey: TCheckBox;
    cbMenuHotKey: TCheckBox;
    cxMenuHotKeyCode: TComboBox;
    cxMenuHotKeyMod: TComboBox;
    cbHotKey: TCheckBox;
    procedure cbHotKeyClick(Sender: TObject);
    procedure cbWindowHotKeyClick(Sender: TObject);
    procedure cbMenuHotKeyClick(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function InternalLoadData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmHotkeyOptionsPage: TfrmHotkeyOptionsPage;

implementation

uses
  ulAppConfig;

{$R *.dfm}

{ TfrmHotkeyOptionsPage }

procedure TfrmHotkeyOptionsPage.cbHotKeyClick(Sender: TObject);
begin
  cbWindowHotKey.Enabled := cbHotKey.Checked;
  cbMenuHotKey.Enabled   := cbHotKey.Checked;
end;

procedure TfrmHotkeyOptionsPage.cbMenuHotKeyClick(Sender: TObject);
begin
  cxMenuHotKeyMod.Enabled  := (cbMenuHotKey.Checked) And (cbHotKey.Checked);
  cxMenuHotKeyCode.Enabled := (cbMenuHotKey.Checked) And (cbHotKey.Checked);
end;

procedure TfrmHotkeyOptionsPage.cbWindowHotKeyClick(Sender: TObject);
begin
  cxWindowHotKeyMod.Enabled  := (cbWindowHotKey.Checked) And (cbHotKey.Checked);
  cxWindowHotKeyCode.Enabled := (cbWindowHotKey.Checked) And (cbHotKey.Checked);
end;

function TfrmHotkeyOptionsPage.GetTitle: string;
begin
  Result := 'HotKey';
end;

function TfrmHotkeyOptionsPage.InternalLoadData: Boolean;
begin
  inherited;
  //Hot Keys
  cbHotKey.Checked := Config.HotKey;
  //Window's Hotkey
  cbWindowHotKey.Checked       := Config.WindowHotKey;
  cxWindowHotKeyMod.ItemIndex  := Config.WindowHotKeyMod;
  cxWindowHotKeyCode.ItemIndex := Config.WindowHotKeyCode;
   //Window's menu Hotkey
  cbMenuHotKey.Checked         := Config.MenuHotkey;
  cxMenuHotKeyMod.ItemIndex    := Config.MenuHotKeyMod;
  cxMenuHotKeyCode.ItemIndex   := Config.MenuHotKeyCode;
  //Enable/disable visual components
  cbHotKeyClick(Self);
  cbMenuHotKeyClick(Self);
  cbWindowHotKeyClick(Self);
end;

end.
