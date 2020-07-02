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

unit Utility.Hotkey;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils, Windows;

const
  // Windows 2000/XP multimedia keys (adapted from winuser.h and renamed to avoid potential conflicts)
  // See also: http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/WindowsUserInterface/UserInput/VirtualKeyCodes.asp
  _VK_BROWSER_BACK            = $A6;        // Browser Back key
  _VK_BROWSER_FORWARD         = $A7;        // Browser Forward key
  _VK_BROWSER_REFRESH         = $A8;        // Browser Refresh key
  _VK_BROWSER_STOP            = $A9;        // Browser Stop key
  _VK_BROWSER_SEARCH          = $AA;        // Browser Search key
  _VK_BROWSER_FAVORITES       = $AB;        // Browser Favorites key
  _VK_BROWSER_HOME            = $AC;        // Browser Start and Home key
  _VK_VOLUME_MUTE             = $AD;        // Volume Mute key
  _VK_VOLUME_DOWN             = $AE;        // Volume Down key
  _VK_VOLUME_UP               = $AF;        // Volume Up key
  _VK_MEDIA_NEXT_TRACK        = $B0;        // Next Track key
  _VK_MEDIA_PREV_TRACK        = $B1;        // Previous Track key
  _VK_MEDIA_STOP              = $B2;        // Stop Media key
  _VK_MEDIA_PLAY_PAUSE        = $B3;        // Play/Pause Media key
  _VK_LAUNCH_MAIL             = $B4;        // Start Mail key
  _VK_LAUNCH_MEDIA_SELECT     = $B5;        // Select Media key
  _VK_LAUNCH_APP1             = $B6;        // Start Application 1 key
  _VK_LAUNCH_APP2             = $B7;        // Start Application 2 key
  // Self-invented names for the extended keys
  NAME_VK_BROWSER_BACK        = 'Browser Back';
  NAME_VK_BROWSER_FORWARD     = 'Browser Forward';
  NAME_VK_BROWSER_REFRESH     = 'Browser Refresh';
  NAME_VK_BROWSER_STOP        = 'Browser Stop';
  NAME_VK_BROWSER_SEARCH      = 'Browser Search';
  NAME_VK_BROWSER_FAVORITES   = 'Browser Favorites';
  NAME_VK_BROWSER_HOME        = 'Browser Start/Home';
  NAME_VK_VOLUME_MUTE         = 'Volume Mute';
  NAME_VK_VOLUME_DOWN         = 'Volume Down';
  NAME_VK_VOLUME_UP           = 'Volume Up';
  NAME_VK_MEDIA_NEXT_TRACK    = 'Next Track';
  NAME_VK_MEDIA_PREV_TRACK    = 'Previous Track';
  NAME_VK_MEDIA_STOP          = 'Stop Media';
  NAME_VK_MEDIA_PLAY_PAUSE    = 'Play/Pause Media';
  NAME_VK_LAUNCH_MAIL         = 'Start Mail';
  NAME_VK_LAUNCH_MEDIA_SELECT = 'Select Media';
  NAME_VK_LAUNCH_APP1         = 'Start Application 1';
  NAME_VK_LAUNCH_APP2         = 'Start Application 2';

function HotKeyAvailable(HotKey: Cardinal): Boolean;
function GetHotKey(Modifiers, Key: Word): Cardinal;
procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
function HotKeyToText(HotKey: Cardinal; Localized: Boolean): String;
function TextToHotKey(Text: String; Localized: Boolean): Cardinal;
function IsExtendedKey(Key: Word): Boolean;

implementation   

uses
  Forms;


const
  HotKeyAtomPrefix = 'HotKeyManagerHotKey';
  // Non-localized (!) modifier names
  ModName_Shift = 'Shift';
  ModName_Ctrl  = 'Ctrl';
  ModName_Alt   = 'Alt';
  ModName_Win   = 'Win';

var
  EnglishKeyboardLayout: HKL;
  ShouldUnloadEnglishKeyboardLayout: Boolean;
  // Localized (!) modifier names; initialized to English names
  LocalModName_Shift: String = ModName_Shift;
  LocalModName_Ctrl: String  = ModName_Ctrl;
  LocalModName_Alt: String   = ModName_Alt;
  LocalModName_Win: String   = ModName_Win;

function GetHotKey(Modifiers, Key: Word): Cardinal;
// Get a shortcut from key and modifiers
const
  VK2_SHIFT   =  32;
  VK2_CONTROL =  64;
  VK2_ALT     = 128;
  VK2_WIN     = 256;
var
  hk: Cardinal;
begin
  hk := 0;
  if (Modifiers and MOD_ALT) <> 0 then
    Inc(hk, VK2_ALT);
  if (Modifiers and MOD_CONTROL) <> 0 then
    Inc(hk, VK2_CONTROL);
  if (Modifiers and MOD_SHIFT) <> 0 then
    Inc(hk, VK2_SHIFT);
  if (Modifiers and MOD_WIN) <> 0 then
    Inc(hk, VK2_WIN);
  hk := hk shl 8;
  Inc(hk, Key);
  Result := hk;
end;


procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
// Separate key and modifiers, so they can be used with RegisterHotKey
const
  VK2_SHIFT   =  32;
  VK2_CONTROL =  64;
  VK2_ALT     = 128;
  VK2_WIN     = 256;
var
  Virtuals: Integer;
  V: Word;
//  x: Byte;
  x: Word;
begin
  Key := Byte(HotKey);
  x := HotKey shr 8;
  Virtuals := x;
  V := 0;
  if (Virtuals and VK2_WIN) <> 0 then
    Inc(V, MOD_WIN);
  if (Virtuals and VK2_ALT) <> 0 then
    Inc(V, MOD_ALT);
  if (Virtuals and VK2_CONTROL) <> 0 then
    Inc(V, MOD_CONTROL);
  if (Virtuals and VK2_SHIFT) <> 0 then
    Inc(V, MOD_SHIFT);
  Modifiers := V;
end;


function HotKeyAvailable(HotKey: Cardinal): Boolean;
// Test if HotKey is available (test if it can be registered - by this or any app.)
var
  M, K: Word;
  Atom: Word;
begin
  Atom := GlobalAddAtomW(PChar('HotKeyManagerHotKeyTest'));
  SeparateHotKey(HotKey, M, K);
  Result := RegisterHotKey(Application.Handle, Atom, M, K);
  if Result then
    UnregisterHotKey(Application.Handle, Atom);
  GlobalDeleteAtom(Atom);
end;


function IsExtendedKey(Key: Word): Boolean;
begin
  Result := ((Key >= _VK_BROWSER_BACK) and (Key <= _VK_LAUNCH_APP2));
end;


function HotKeyToText(HotKey: Cardinal; Localized: Boolean): String;
// Return localized(!) or English(!) string value from key combination

  function GetExtendedVKName(Key: Word): String;
  begin
    case Key of
      _VK_BROWSER_BACK:        Result := NAME_VK_BROWSER_BACK;
      _VK_BROWSER_FORWARD:     Result := NAME_VK_BROWSER_FORWARD;
      _VK_BROWSER_REFRESH:     Result := NAME_VK_BROWSER_REFRESH;
      _VK_BROWSER_STOP:        Result := NAME_VK_BROWSER_STOP;
      _VK_BROWSER_SEARCH:      Result := NAME_VK_BROWSER_SEARCH;
      _VK_BROWSER_FAVORITES:   Result := NAME_VK_BROWSER_FAVORITES;
      _VK_BROWSER_HOME:        Result := NAME_VK_BROWSER_HOME;
      _VK_VOLUME_MUTE:         Result := NAME_VK_VOLUME_MUTE;
      _VK_VOLUME_DOWN:         Result := NAME_VK_VOLUME_DOWN;
      _VK_VOLUME_UP:           Result := NAME_VK_VOLUME_UP;
      _VK_MEDIA_NEXT_TRACK:    Result := NAME_VK_MEDIA_NEXT_TRACK;
      _VK_MEDIA_PREV_TRACK:    Result := NAME_VK_MEDIA_PREV_TRACK;
      _VK_MEDIA_STOP:          Result := NAME_VK_MEDIA_STOP;
      _VK_MEDIA_PLAY_PAUSE:    Result := NAME_VK_MEDIA_PLAY_PAUSE;
      _VK_LAUNCH_MAIL:         Result := NAME_VK_LAUNCH_MAIL;
      _VK_LAUNCH_MEDIA_SELECT: Result := NAME_VK_LAUNCH_MEDIA_SELECT;
      _VK_LAUNCH_APP1:         Result := NAME_VK_LAUNCH_APP1;
      _VK_LAUNCH_APP2:         Result := NAME_VK_LAUNCH_APP2;
    else
      Result := '';
    end;
  end;

  function GetModifierNames: String;
  var
    S: String;
  begin
    S := '';
    if Localized then
    begin
      if (HotKey and $4000) <> 0 then       // scCtrl
        S := S + LocalModName_Ctrl + '+';
      if (HotKey and $2000) <> 0 then       // scShift
        S := S + LocalModName_Shift + '+';
      if (HotKey and $8000) <> 0 then       // scAlt
        S := S + LocalModName_Alt + '+';
      if (HotKey and $10000) <> 0 then
        S := S + LocalModName_Win + '+';
    end
    else
    begin
      if (HotKey and $4000) <> 0 then       // scCtrl
        S := S + ModName_Ctrl + '+';
      if (HotKey and $2000) <> 0 then       // scShift
        S := S + ModName_Shift + '+';
      if (HotKey and $8000) <> 0 then       // scAlt
        S := S + ModName_Alt + '+';
      if (HotKey and $10000) <> 0 then
        S := S + ModName_Win + '+';
    end;
    Result := S;
  end;

  function GetVKName(Special: Boolean): String;
  var
    ScanCode: Cardinal;
    KeyName: array[0..255] of AnsiChar;
    oldkl: HKL;
    Modifiers, Key: Word;
  begin
    Result := '';
    if Localized then        // Local language key names
    begin
      if Special then
        ScanCode := (MapVirtualKey(Byte(HotKey), 0) shl 16) or (1 shl 24)
      else
        ScanCode := (MapVirtualKey(Byte(HotKey), 0) shl 16);
      if ScanCode <> 0 then
      begin
        GetKeyNameTextA(ScanCode, KeyName, SizeOf(KeyName));
        Result := KeyName;
      end;
    end
    else                     // English key names
    begin
      if Special then
        ScanCode := (MapVirtualKeyEx(Byte(HotKey), 0, EnglishKeyboardLayout) shl 16) or (1 shl 24)
      else
        ScanCode := (MapVirtualKeyEx(Byte(HotKey), 0, EnglishKeyboardLayout) shl 16);
      if ScanCode <> 0 then
      begin
        oldkl := GetKeyboardLayout(0);
        if oldkl <> EnglishKeyboardLayout then
          ActivateKeyboardLayout(EnglishKeyboardLayout, 0);  // Set English kbd. layout
        GetKeyNameTextA(ScanCode, KeyName, SizeOf(KeyName));
        Result := KeyName;
        if oldkl <> EnglishKeyboardLayout then
        begin
          if ShouldUnloadEnglishKeyboardLayout then
            UnloadKeyboardLayout(EnglishKeyboardLayout);     // Restore prev. kbd. layout
          ActivateKeyboardLayout(oldkl, 0);
        end;
      end;
    end;

    if Length(Result) <= 1 then
    begin
      // Try the internally defined names
      SeparateHotKey(HotKey, Modifiers, Key);
      if IsExtendedKey(Key) then
        Result := GetExtendedVKName(Key);
    end;
  end;

var
  KeyName: String;
begin
  case Byte(HotKey) of
    // PgUp, PgDn, End, Home, Left, Up, Right, Down, Ins, Del
    $21..$28, $2D, $2E: KeyName := GetVKName(True);
  else
    KeyName := GetVKName(False);
  end;
  Result := GetModifierNames + KeyName;
end;


function TextToHotKey(Text: String; Localized: Boolean): Cardinal;
// Return key combination created from (non-localized!) string value
var
  Tokens: TStringList;

  function GetModifiersValue: Word;
  var
    I: Integer;
    M: Word;
    ModName: String;
  begin
    M := 0;
    for I := 0 to Tokens.Count -2 do
    begin
      ModName := Trim(Tokens[I]);
      if (AnsiCompareText(ModName, ModName_Shift) = 0) or
         (AnsiCompareText(ModName, LocalModName_Shift) = 0) then
        M := M or MOD_SHIFT
      else if (AnsiCompareText(ModName, ModName_Ctrl) = 0) or
              (AnsiCompareText(ModName, LocalModName_Ctrl) = 0) then
        M := M or MOD_CONTROL
      else if (AnsiCompareText(ModName, ModName_Alt) = 0) or
              (AnsiCompareText(ModName, LocalModName_Alt) = 0) then
        M := M or MOD_ALT
      else if (AnsiCompareText(ModName, ModName_Win) = 0) or
              (AnsiCompareText(ModName, LocalModName_Win) = 0) then
        M := M or MOD_WIN
      else
      begin
        // Unrecognized modifier encountered
        Result := 0;
        Exit;
      end;
    end;
    Result := M;
  end;

  function IterateVKNames(KeyName: String): Word;
  var
    I: Integer;
    K: Word;
  begin
    K := 0;
    for I := $08 to $FF do        // The brute force approach
      if AnsiCompareText(KeyName, HotKeyToText(I, Localized)) = 0 then
      begin
        K := I;
        Break;
      end;
    Result := K;
  end;

  function GetKeyValue: Word;
  var
    K: Word;
    KeyName: String;
    C: Char;
  begin
    K := 0;
    if Tokens.Count > 0 then
    begin
      KeyName := Trim(Tokens[Tokens.Count-1]);
      if Length(KeyName) = 1 then
      begin
        C := UpCase(KeyName[1]);
        case Byte(C) of
          $30..$39, $41..$5A:     // 0..9, A..Z
            K := Ord(C);
        else
          K := IterateVKNames(C);
        end;
      end
      else
      begin
        if KeyName = 'Num' then   // Special handling for 'Num +'
          KeyName := KeyName + ' +';
        if (KeyName <> ModName_Ctrl) and (KeyName <> LocalModName_Ctrl) and
           (KeyName <> ModName_Alt) and (KeyName <> LocalModName_Alt) and
           (KeyName <> ModName_Shift) and (KeyName <> LocalModName_Shift) and
           (KeyName <> ModName_Win) and (KeyName <> LocalModName_Win) then
        K := IterateVKNames(KeyName);
      end;
    end;
    Result := K;
  end;

var
  Modifiers, Key: Word;
begin
  Tokens := TStringList.Create;
  try
    ExtractStrings(['+'], [' '], PAnsiChar(Text), Tokens);
    Modifiers := GetModifiersValue;
    if (Modifiers = 0) and (Tokens.Count > 1) then
      // Something went wrong when translating the modifiers
      Result := 0
    else
    begin
      Key := GetKeyValue;
      if Key = 0 then
        // Something went wrong when translating the key
        Result := 0
      else
        Result := GetHotKey(Modifiers, Key);
    end;
  finally
    Tokens.Free;
  end;
end;

end.

