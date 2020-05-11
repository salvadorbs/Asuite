{
Copyright (C) 2006-2020 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Forms.ShortcutGrabber;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, HotKeyManager, Menus,
  Vcl.ComCtrls, cySkinButton, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TfrmShortcutGrabber = class(TForm)
    HotKey1: THotKey;
    HotKeyManager1: THotKeyManager;
    btnAlt: TcySkinButton;
    btnWinKey: TcySkinButton;
    btnShift: TcySkinButton;
    btnCtrl: TcySkinButton;
    lblInfo: TLabel;
    pnlDialogPage: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    function CheckModButtons(): Boolean;
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FHotkey: string;

    function GetModifierFromGUI(): Word;
    function GetKeyFromGUI(): Word;
  public
    { Public declarations }
    property Hotkey: string read FHotkey;

    class function Execute(AOwner: TComponent): string;
  end;

var
  frmShortcutGrabber: TfrmShortcutGrabber;

implementation

uses
  Kernel.Logger, Utility.Misc, DKLang;

{$R *.dfm}

{ TfrmShortcutGrabber }

procedure TfrmShortcutGrabber.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmShortcutGrabber.btnOkClick(Sender: TObject);
var
  Key, Modifiers: Word;
  HotKeyVar: Cardinal;
begin
  FHotkey := '';
  Key := GetKeyFromGUI();
  Modifiers := GetModifierFromGUI();

  if Key <> 0 then
  begin
    if Modifiers <> 0 then
    begin
      //Convert Key + Modifier in Cardinal
      HotKeyVar := HotKeyManager.GetHotKey(Modifiers, Key);

      if HotKeyManager.HotKeyAvailable(HotKeyVar) then
      begin
        FHotkey := HotKeyToText(HotKeyVar, false);

        Close;
      end
      else
        ShowMessageEx(DKLangConstW('msgHotkeyNotAvailable'), True);
    end
    else
      ShowMessageEx(DKLangConstW('msgHotkeyNoMod'));
  end
  else
    ShowMessageEx(DKLangConstW('msgHotkeyNoKey'));
end;

function TfrmShortcutGrabber.CheckModButtons: Boolean;
begin
  Result := (btnCtrl.Down) or (btnShift.Down) or (btnAlt.Down) or (btnWinKey.down);
end;

class function TfrmShortcutGrabber.Execute(AOwner: TComponent): string;
begin
  TASuiteLogger.Info('Opening form Shortcut Grabber', []);

  frmShortcutGrabber := TfrmShortcutGrabber.Create(AOwner);
  try
    frmShortcutGrabber.ShowModal;

    Result := frmShortcutGrabber.FHotkey;

    TASuiteLogger.Info('User selected hotkey "%s"', [ShortCutToText(Result)]);
  finally
    FreeAndNil(frmShortcutGrabber);
  end;
end;

function TfrmShortcutGrabber.GetKeyFromGUI(): Word;
var
  Key: Word;
  Modifier: TShiftState;
begin
  ShortCutToKey(HotKey1.HotKey, Key, Modifier);

  Result := Key;
end;

function TfrmShortcutGrabber.GetModifierFromGUI(): Word;
begin
  Result := 0;

  if btnCtrl.Down then
    Result := Result or MOD_CONTROL;

  if btnShift.Down then
    Result := Result or MOD_SHIFT;

  if btnAlt.Down then
    Result := Result or MOD_ALT;

  if btnWinKey.Down then
    Result := Result or MOD_WIN;
end;

end.
