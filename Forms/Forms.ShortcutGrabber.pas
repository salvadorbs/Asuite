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
    hkKeys: THotKey;
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
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure HotKey1Change(Sender: TObject);
  private
    { Private declarations }
    FHotkey: string;
    FCanClose: Boolean;

    function GetModifierFromGUI(): Word;
    function GetKeyFromGUI(): Word;

    procedure SetGUIKeyFromKey(AKey: Word);
    procedure SetGUIModifierFromMod(AMod: Word);
    procedure SetGUIModifierFromShiftState(AMod: TShiftState);
  public
    { Public declarations }
    property Hotkey: string read FHotkey;

    procedure SetGuiFromHotkey(AHotkey: string);

    class function Execute(AOwner: TComponent; AHotkey: string): string;
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
  FCanClose := True;
end;

procedure TfrmShortcutGrabber.btnOkClick(Sender: TObject);
var
  Key, Modifiers: Word;
  HotKeyVar: Cardinal;
begin
  FCanClose := False;
  FHotkey := '';

  //Get keys and modifier from interface
  Key := GetKeyFromGUI();
  Modifiers := GetModifierFromGUI();

  if Key <> 0 then
  begin
    if Modifiers <> 0 then
    begin
      //Convert Key + Modifier in Cardinal
      HotKeyVar := HotKeyManager.GetHotKey(Modifiers, Key);

      //Is it available?
      FCanClose := HotKeyManager.HotKeyAvailable(HotKeyVar);

      //TODO: Controllare che non sia già preso da un altro hotkey in asuite!
      if FCanClose then
        FHotkey := HotKeyToText(HotKeyVar, false)
      else
        ShowMessageEx(DKLangConstW('msgHotkeyNotAvailable'), True);
    end
    else
      ShowMessageEx(DKLangConstW('msgHotkeyNoMod'));
  end
  else
    ShowMessageEx(DKLangConstW('msgHotkeyNoKey'));

  if FHotkey = '' then
    hkKeys.SetFocus;
end;

function TfrmShortcutGrabber.CheckModButtons: Boolean;
begin
  Result := (btnCtrl.Down) or (btnShift.Down) or (btnAlt.Down) or (btnWinKey.down);
end;

class function TfrmShortcutGrabber.Execute(AOwner: TComponent; AHotkey: string): string;
begin
  TASuiteLogger.Info('Opening form Shortcut Grabber', []);

  frmShortcutGrabber := TfrmShortcutGrabber.Create(AOwner);
  try
    frmShortcutGrabber.SetGuiFromHotkey(AHotkey);

    if frmShortcutGrabber.ShowModal = mrOk then
      Result := frmShortcutGrabber.FHotkey;

    TASuiteLogger.Info('User selected hotkey "%s"', [Result]);
  finally
    FreeAndNil(frmShortcutGrabber);
  end;
end;

procedure TfrmShortcutGrabber.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmShortcutGrabber.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FHotkey := '';
end;

function TfrmShortcutGrabber.GetKeyFromGUI(): Word;
var
  Key: Word;
  Modifier: TShiftState;
begin
  ShortCutToKey(hkKeys.HotKey, Key, Modifier);

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

procedure TfrmShortcutGrabber.HotKey1Change(Sender: TObject);
var
  Key: Word;
  Modi: TShiftState;
begin
  //Separate key and mod from THotkey and set GUI properly
  ShortCutToKey(hkKeys.HotKey, key, Modi);
  SetGUIKeyFromKey(key);
  SetGUIModifierFromShiftState(Modi);

  //Change hotkey, reinsert only key
  hkKeys.HotKey := ShortCut(key, []);
end;

procedure TfrmShortcutGrabber.SetGuiFromHotkey(AHotkey: string);
var
  Hotkey: Cardinal;
  Modi, Key: Word;
begin
  //I need separate key from modifiers
  Hotkey := TextToHotKey(AHotkey, false);
  SeparateHotKey(Hotkey, Modi, Key);

  //Set gui
  SetGUIKeyFromKey(Key);
  SetGUIModifierFromMod(Modi);
end;

procedure TfrmShortcutGrabber.SetGUIKeyFromKey(AKey: Word);
begin
  hkKeys.HotKey := ShortCut(AKey, []);
end;

procedure TfrmShortcutGrabber.SetGUIModifierFromMod(AMod: Word);
begin
  if (AMod and MOD_ALT) <> 0 then
    btnAlt.Down := True;

  if (AMod and MOD_CONTROL) <> 0 then
    btnCtrl.Down := True;

  if (AMod and MOD_SHIFT) <> 0 then
    btnShift.Down := True;

  if (AMod and MOD_WIN) <> 0 then
    btnWinKey.Down := True;
end;

procedure TfrmShortcutGrabber.SetGUIModifierFromShiftState(AMod: TShiftState);
begin
  btnShift.Down := false;
  btnAlt.Down   := false;
  btnCtrl.Down  := false;

  if (ssShift in AMod) then
    btnShift.Down := True;

  if (ssAlt in AMod) then
    btnAlt.Down := True;

  if (ssCtrl in AMod) then
    btnCtrl.Down := True;
end;

end.
