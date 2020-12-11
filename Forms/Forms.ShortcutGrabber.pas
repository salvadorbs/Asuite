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

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ComCtrls, BCImageButton, ExtCtrls, HotKey, LCLProc,
  {$IFDEF Windows}Windows,{$ENDIF} DefaultTranslator;

type

  { TfrmShortcutGrabber }

  TfrmShortcutGrabber = class(TForm)
    btnAlt: TBCImageButton;
    btnCtrl: TBCImageButton;
    btnShift: TBCImageButton;
    btnWinKey: TBCImageButton;
    hkKeys: THotKey;
    lblInfo: TLabel;
    pnlDialogPage: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    function CheckModButtons(): Boolean;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure hkKeysChange(Sender: TObject);
  private
    { Private declarations }
    FHotkey: string;
    FCanClose: Boolean;

    function GetModifierFromGUI(): TShiftState;
    function GetKeyFromGUI(): Word;

    procedure SetGUIKeyFromKey(AKey: Word);
    procedure SetGUIModifierFromMod(AMod: TShiftState);
    procedure SetGUIModifierFromShiftState(AMod: TShiftState);
    procedure LoadPNGButtonState(AButton: TBCImageButton; APathFile: string);
    procedure LoadImages();
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
  Kernel.Logger, Utility.Misc, AppConfig.Main, Kernel.Consts,
  Utility.System, Kernel.ResourceStrings, Hotkeys.Manager;

{$R *.lfm}

{ TfrmShortcutGrabber }

procedure TfrmShortcutGrabber.btnCancelClick(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TfrmShortcutGrabber.btnOkClick(Sender: TObject);
var
  Key: Word;
  Modifiers: TShiftState;
  HotKeyVar: TShortCut;
  NewHotkey: string;
begin
  FCanClose := False;
  NewHotkey := '';

  //Get keys and modifier from interface
  Key := GetKeyFromGUI();
  Modifiers := GetModifierFromGUI();

  if Key <> 0 then
  begin
    if Modifiers <> [] then
    begin
      //Convert Key + Modifier in Cardinal
      HotKeyVar := KeyToShortCut(Key, Modifiers);
      NewHotkey := ShortCutToText(HotKeyVar);

      //Check old and new hotkey. They must differs (user choose another hotkey)
      if FHotkey <> UpperCase(NewHotkey) then
      begin
        //Is it available?
        FCanClose := HotkeyManager.IsHotkeyAvailable(HotKeyVar);

        if FCanClose then
          FHotkey := NewHotkey
        else
          ShowMessageEx(msgHotkeyNotAvailable, True);
      end
      else
        FCanClose := True;
    end
    else
      ShowMessageEx(msgHotkeyNoMod);
  end
  else
    ShowMessageEx(msgHotkeyNoKey);

  if NewHotkey = '' then
    hkKeys.SetFocus;
end;

function TfrmShortcutGrabber.CheckModButtons: Boolean;
begin
  Result := (btnCtrl.Pressed) or (btnShift.Pressed) or (btnAlt.Pressed) or (btnWinKey.Pressed);
end;

class function TfrmShortcutGrabber.Execute(AOwner: TComponent; AHotkey: string): string;
begin
  TASuiteLogger.Info('Opening form Shortcut Grabber', []);

  frmShortcutGrabber := TfrmShortcutGrabber.Create(AOwner);
  try
    frmShortcutGrabber.SetGuiFromHotkey(AHotkey);
    frmShortcutGrabber.FHotkey := AHotkey;

    Result := '';
    if frmShortcutGrabber.ShowModal = mrOk then
      Result := UpperCase(frmShortcutGrabber.FHotkey);

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

procedure TfrmShortcutGrabber.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_RETURN then
    btnOkClick(Sender)
  else
    if Ord(Key) = VK_ESCAPE then
      btnCancelClick(Sender);
end;

procedure TfrmShortcutGrabber.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FHotkey := '';

  LoadImages();
end;

function TfrmShortcutGrabber.GetKeyFromGUI(): Word;
var
  Key: Word;
  Modifier: TShiftState;
begin
  ShortCutToKey(hkKeys.HotKey, Key, Modifier);

  Result := Key;
end;

function TfrmShortcutGrabber.GetModifierFromGUI(): TShiftState;
begin
  Result := [];

  if btnCtrl.Pressed then
    Result := Result + [ssCtrl];

  if btnShift.Pressed then
    Result := Result + [ssShift];

  if btnAlt.Pressed then
    Result := Result + [ssAlt];

  if btnWinKey.Pressed then
    Result := Result + [ssMeta];
end;

procedure TfrmShortcutGrabber.hkKeysChange(Sender: TObject);
var
  Key: Word;
  Modi: TShiftState;
begin
  //Separate key and mod from THotkey and set GUI properly
  ShortCutToKey(hkKeys.HotKey, key, Modi);
  SetGUIModifierFromShiftState(Modi);
  SetGUIKeyFromKey(key);

  //Change hotkey, reinsert only key
  //hkKeys.HotKey := ShortCut(key, []);
end;

procedure TfrmShortcutGrabber.LoadImages;
begin
  LoadPNGButtonState(btnAlt, Config.Paths.SuitePathCurrentTheme + BUTTONS_DIR + FILENAME_ALT);
  LoadPNGButtonState(btnCtrl, Config.Paths.SuitePathCurrentTheme + BUTTONS_DIR + FILENAME_CTRL);
  LoadPNGButtonState(btnShift, Config.Paths.SuitePathCurrentTheme + BUTTONS_DIR + FILENAME_SHIFT);
  LoadPNGButtonState(btnWinKey, Config.Paths.SuitePathCurrentTheme + BUTTONS_DIR + FILENAME_WINKEY);
end;

procedure TfrmShortcutGrabber.LoadPNGButtonState(AButton: TBCImageButton; APathFile: string);
begin
  if FileExists(APathFile) then
  begin
    AButton.BitmapFile := APathFile;
    AButton.LoadFromBitmapFile;
  end;
end;

procedure TfrmShortcutGrabber.SetGuiFromHotkey(AHotkey: string);
var
  Shortcut: TShortcut;
  Key: Word;
  Modi: TShiftState;
begin
  //I need separate key from modifiers
  Shortcut := TextToShortCut(AHotkey);
  ShortCutToKey(Shortcut, Key, Modi);

  //Set gui
  SetGUIKeyFromKey(Key);
  SetGUIModifierFromMod(Modi);
end;

procedure TfrmShortcutGrabber.SetGUIKeyFromKey(AKey: Word);
begin
  hkKeys.HotKey := ShortCut(AKey, []);
end;

procedure TfrmShortcutGrabber.SetGUIModifierFromMod(AMod: TShiftState);
begin
  if ssAlt in AMod then
    btnAlt.Pressed := True;

  if ssCtrl in AMod then
    btnCtrl.Pressed := True;

  if ssShift in AMod then
    btnShift.Pressed := True;

  if ssMeta in AMod then
    btnWinKey.Pressed := True;
end;

procedure TfrmShortcutGrabber.SetGUIModifierFromShiftState(AMod: TShiftState);
begin
  if AMod <> [] then
  begin
    btnShift.Pressed := false;
    btnAlt.Pressed   := false;
    btnCtrl.Pressed  := false;
    btnWinKey.Pressed  := false;

    if (ssShift in AMod) then
      btnShift.Pressed := True;

    if (ssAlt in AMod) then
      btnAlt.Pressed := True;

    if (ssCtrl in AMod) then
      btnCtrl.Pressed := True;

    if (ssMeta in AMod) then
      btnWinKey.Pressed := True;
  end;
end;

end.
