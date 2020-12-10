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

unit Hotkeys.ShortcutEx;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils, LCLProc, LCLType;

type       
  TKeyNotifyEvent = procedure(Sender: TObject; Key: Word; Shift: TShiftState) of object;

  { TShortcutEx }

  TShortcutEx = class
    private
      FKey: Word;
      FShiftState: TShiftState;
      FShortcut: TShortCut;
      FNotify: TKeyNotifyEvent;
      FIndex: Integer;

      function GetShortcut: TShortcut;
      procedure SetShortcut(AValue: TShortcut);
    public
      property Key: Word read FKey;
      property ShiftState: TShiftState read FShiftState;
      property Shortcut: TShortcut read GetShortcut write SetShortcut;
      property Notify: TKeyNotifyEvent read FNotify write FNotify;
      property Index: Integer read FIndex write FIndex;

      constructor Create(AShortCut: TShortcut);

      function ToString: String;
  end;

implementation

{ TShortcutEx }

procedure ShortCutToKey(const ShortCut: TShortCut; out Key: Word;
  out Shift : TShiftState);
begin
  Key := ShortCut and $FF;
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift, ssShift);
  if ShortCut and scAlt <> 0 then Include(Shift, ssAlt);
  if ShortCut and scCtrl <> 0 then Include(Shift, ssCtrl);
  if ShortCut and scMeta <> 0 then Include(Shift, ssMeta);
end;

function TShortcutEx.GetShortcut: TShortcut;
begin
  Result := FShortcut;
end;

procedure TShortcutEx.SetShortcut(AValue: TShortcut);
begin
  FShortcut := AValue;

  ShortCutToKey(Shortcut, FKey, FShiftState);
end;

constructor TShortcutEx.Create(AShortCut: TShortcut);
begin
  inherited Create;

  FIndex := -1;
  FNotify := nil;
  Self.Shortcut := AShortCut;
end;

function TShortcutEx.ToString: String;
begin
  Result := ShortCutToText(FShortcut);
end;

end.

