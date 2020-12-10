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

---

With some original code by Codebot (Cross Pascal Library) - https://github.com/sysrpl/Cross.Codebot/

}

unit Hotkeys.Manager;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, LCLType, Menus, ShortcutEx, Generics.Collections, Generics.Defaults;

{ THotkeyCaptureList }

type
  THotkeyList = TObjectList<TShortcutEx>;

  THotkeysComparer = TComparer<TShortcutEx>;

  { TGenericHotkeyCaptureList }

  THotkeyManager = class
  private
    FList: THotkeyList;
    function GetHotkey(Index: Integer): TShortcutEx;
    function GetCount: Integer;
  protected
    function DoRegister(Shortcut: TShortCutEx): Boolean; virtual; abstract;
    function DoUnregister(Shortcut: TShortCutEx): Boolean; virtual; abstract;

    property Hotkeys[Index: Integer]: TShortcutEx read GetHotkey; default;
    property Count: Integer read GetCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function RegisterNotify(Shortcut: TShortCut; Notify: TKeyNotifyEvent): Boolean;
    function UnregisterNotify(Shortcut: TShortCut): Boolean;
                      
    function FindHotkey(Key: Word; ShiftState: TShiftState): Integer; overload;
    function FindHotkey(Shortcut: TShortCut): Integer; overload;     
    function FindHotkeyByIndex(Index: Integer): Integer;
    procedure ClearAllHotkeys;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; virtual; abstract;
  end;

{ Used by THotkeyList }
function HotkeyCompare(constref A, B: TShortcutEx): Integer;

{ Returns the global hotkey capture instance }
function HotkeyCapture: TGenericHotkeyCaptureList;   

var
  InternalCapture: TGenericHotkeyCaptureList;

implementation

{$I Hotkeys.Manager.Platform.inc}

function IsKeyValid(Key: Word): Boolean;
begin
  case Key of
    VK_TAB: Result := True;
    VK_CLEAR: Result := True;
    VK_RETURN: Result := True;
    VK_MENU: Result := True;
    VK_ESCAPE: Result := True;
    VK_PAUSE: Result := True;
    VK_SPACE: Result := True;
    VK_PRIOR: Result := True;
    VK_NEXT: Result := True;
    VK_END: Result := True;
    VK_HOME: Result := True;
    VK_LEFT: Result := True;
    VK_UP: Result := True;
    VK_RIGHT: Result := True;
    VK_DOWN: Result := True;
    VK_SELECT: Result := True;
    VK_EXECUTE: Result := True;
    VK_SNAPSHOT: Result := True;
    VK_INSERT: Result := True;
    VK_DELETE: Result := True;
    VK_HELP: Result := True;
    VK_0: Result := True;
    VK_1: Result := True;
    VK_2: Result := True;
    VK_3: Result := True;
    VK_4: Result := True;
    VK_5: Result := True;
    VK_6: Result := True;
    VK_7: Result := True;
    VK_8: Result := True;
    VK_9: Result := True;
    VK_A: Result := True;
    VK_B: Result := True;
    VK_C: Result := True;
    VK_D: Result := True;
    VK_E: Result := True;
    VK_F: Result := True;
    VK_G: Result := True;
    VK_H: Result := True;
    VK_I: Result := True;
    VK_J: Result := True;
    VK_K: Result := True;
    VK_L: Result := True;
    VK_M: Result := True;
    VK_N: Result := True;
    VK_O: Result := True;
    VK_P: Result := True;
    VK_Q: Result := True;
    VK_R: Result := True;
    VK_S: Result := True;
    VK_T: Result := True;
    VK_U: Result := True;
    VK_V: Result := True;
    VK_W: Result := True;
    VK_X: Result := True;
    VK_Y: Result := True;
    VK_Z: Result := True;
    VK_NUMPAD0: Result := True;
    VK_NUMPAD1: Result := True;
    VK_NUMPAD2: Result := True;
    VK_NUMPAD3: Result := True;
    VK_NUMPAD4: Result := True;
    VK_NUMPAD5: Result := True;
    VK_NUMPAD6: Result := True;
    VK_NUMPAD7: Result := True;
    VK_NUMPAD8: Result := True;
    VK_NUMPAD9: Result := True;
    VK_MULTIPLY: Result := True;
    VK_ADD: Result := True;
    VK_SEPARATOR: Result := True;
    VK_SUBTRACT: Result := True;
    VK_DECIMAL: Result := True;
    VK_DIVIDE: Result := True;
    VK_F1: Result := True;
    VK_F2: Result := True;
    VK_F3: Result := True;
    VK_F4: Result := True;
    VK_F5: Result := True;
    VK_F6: Result := True;
    VK_F7: Result := True;
    VK_F8: Result := True;
    VK_F9: Result := True;
    VK_F10: Result := True;
    VK_F11: Result := True;
    VK_F12: Result := True;
    VK_LCL_EQUAL: Result := True;
    VK_LCL_COMMA: Result := True;
    VK_LCL_POINT: Result := True;
    VK_LCL_SLASH: Result := True;
    VK_LCL_SEMI_COMMA: Result := True;
    VK_LCL_MINUS: Result := True;
    VK_LCL_OPEN_BRAKET: Result := True;
    VK_LCL_CLOSE_BRAKET: Result := True;
    VK_LCL_BACKSLASH: Result := True;
    VK_LCL_TILDE: Result := True;
    VK_LCL_QUOTE: Result := True;
  else
    Result := False;
  end;
end;

constructor TGenericHotkeyCaptureList.Create;
begin
  inherited Create;

  FList := THotkeyList.Create(THotkeysComparer.Construct(HotkeyCompare), True);
end;

destructor TGenericHotkeyCaptureList.Destroy;
begin
  ClearAllHotkeys;
  FList.Free;

  inherited Destroy;
end;

function TGenericHotkeyCaptureList.GetHotkey(Index: Integer): TShortcutEx;
begin
  Result := FList[Index];
end;

function TGenericHotkeyCaptureList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function HotkeyCompare(constref A, B: TShortcutEx): Integer;
begin
  Result := A.Key - B.Key;
  if Result <> 0 then
    Exit;
  Result := LongInt(A.ShiftState) - LongInt(B.ShiftState);
end;

function TGenericHotkeyCaptureList.FindHotkey(Key: Word; ShiftState: TShiftState): Integer;
var
  Shortcut: TShortcut;
begin
  Shortcut := KeyToShortCut(Key, ShiftState);

  Result := FindHotkey(Shortcut);
end;

function TGenericHotkeyCaptureList.FindHotkey(Shortcut: TShortCut): Integer;
var
  Item: TShortcutEx;
begin     
  Result := -1;

  Item := TShortcutEx.Create(Shortcut);
  try
    Result := FList.IndexOf(Item);
  finally 
    Item.Free;
  end;
end;

function TGenericHotkeyCaptureList.RegisterNotify(Shortcut: TShortCut; Notify: TKeyNotifyEvent): Boolean;
var
  H: TShortcutEx;
  I: Integer;
  Key: Word;
  ShiftState: TShiftState;
begin
  ShortCutToKey(Shortcut, Key, ShiftState);

  if not IsKeyValid(Key) then
    Exit(False);

  I := FindHotkey(Key, ShiftState);

  Result := I < 0;
  if Result then
  begin      
    H := TShortcutEx.Create(Shortcut);
    try                               
      H.Notify := Notify;
      Result := DoRegister(H);
    finally      
      FList.Add(H);
    end;
  end;
end;

function TGenericHotkeyCaptureList.UnregisterNotify(Shortcut: TShortCut): Boolean;
var
  I: Integer;
  Key: Word;
  ShiftState: TShiftState;
begin
  Result := False;
  ShortCutToKey(Shortcut, Key, ShiftState);

  if not IsKeyValid(Key) then
    Exit(False);

  I := FindHotkey(Key, ShiftState);

  if I > -1 then
  begin
    Result := DoUnregister(FList[I]);     
    FList.Delete(I);
  end;
end;

function TGenericHotkeyCaptureList.FindHotkeyByIndex(Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to (FList.Count - 1) do
  begin
    if FList[I].Index = Index then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure TGenericHotkeyCaptureList.ClearAllHotkeys;
var
  H: TShortcutEx;
begin
  while Count > 0 do
  begin
    H := Hotkeys[Count - 1];
    UnregisterNotify(H.Shortcut);
  end;
end;

initialization
  InternalCapture := nil;

finalization
  InternalCapture.Free;
end.

