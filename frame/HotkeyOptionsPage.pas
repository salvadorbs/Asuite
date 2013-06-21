unit HotkeyOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmHotkeyOptionsPage = class(TfrmBaseOptionsPage)
    cbHotKey: TCheckBox;
    gbWindow: TGroupBox;
    cxWindowHotKeyCode: TComboBox;
    cxWindowHotKeyMod: TComboBox;
    cbWindowHotKey: TCheckBox;
    cbMenuHotKey: TCheckBox;
    cxMenuHotKeyCode: TComboBox;
    cxMenuHotKeyMod: TComboBox;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
  public
    { Public declarations }
  end;

var
  frmHotkeyOptionsPage: TfrmHotkeyOptionsPage;

implementation

{$R *.dfm}

{ TfrmGraphicMenuOptionsPage }

function TfrmHotkeyOptionsPage.GetTitle: string;
begin
  Result := 'HotKey';
end;

end.
