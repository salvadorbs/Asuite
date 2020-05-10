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
    Label1: TLabel;
    pnlDialogPage: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure Button1Click(Sender: TObject);
    function CheckModButtons(): Boolean;
  private
    { Private declarations }
    FHotkey: TShortCut;
  public
    { Public declarations }
    property Hotkey: TShortCut read FHotkey;

    class function Execute(AOwner: TComponent): TShortcut;
  end;

var
  frmShortcutGrabber: TfrmShortcutGrabber;

implementation

uses
  Kernel.Logger;

{$R *.dfm}

{ TfrmShortcutGrabber }

procedure TfrmShortcutGrabber.Button1Click(Sender: TObject);
var
  Modifiers: Word;
  Key: Word;
  nomod: TShiftState;
  HotKeyVar: Cardinal;
begin
  if HotKey1.HotKey <> 0 then
  begin
    Modifiers := 0;

    if CheckModButtons then
    begin
      if btnCtrl.Down then
        Modifiers := Modifiers or MOD_CONTROL;

      if btnShift.Down then
        Modifiers := Modifiers or MOD_SHIFT;

      if btnAlt.Down then
        Modifiers := Modifiers or MOD_ALT;

      if btnWinKey.Down then
        Modifiers := Modifiers or MOD_WIN;

      ShortCutToKey(HotKey1.HotKey, Key, nomod);

      HotKeyVar := HotKeyManager.GetHotKey(Modifiers, Key);

      if HotKeyManager.HotKeyAvailable(HotKeyVar) then
        ShowMessage(HotKeyToText(HotKeyVar, false))
      else
        ShowMessage('Hotkey not available!');
    end
    else
      ShowMessage('Forgot modifiers?');
  end
  else
    ShowMessage('Forgot keys?');
end;

function TfrmShortcutGrabber.CheckModButtons: Boolean;
begin
  Result := (btnCtrl.Down) or (btnShift.Down) or (btnAlt.Down) or (btnWinKey.down);
end;

class function TfrmShortcutGrabber.Execute(AOwner: TComponent): TShortcut;
var
  frm: TfrmShortcutGrabber;
begin
  TASuiteLogger.Info('Opening form Shortcut Grabber', []);

  frm := TfrmShortcutGrabber.Create(AOwner);
  try
    frm.ShowModal;

    Result := frm.FHotkey;

    TASuiteLogger.Info('User selected hotkey "%s"', [ShortCutToText(Result)]);
  finally
    frm.Free;
  end;
end;

end.
