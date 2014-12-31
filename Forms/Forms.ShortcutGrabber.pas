{
Copyright (C) 2006-2015 Matteo Salvi

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
  Classes, Controls, StdCtrls, SysUtils, Forms, Menus, Windows;

type
  TShorcutGrabber = class
  private
    FGrabForm: TForm;
    FHotkey: TShortCut;
    procedure OnGrabFormKeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Hotkey: TShortCut read FHotkey;

    class function Execute(AOwner: TComponent): TShortcut;
  end;

implementation

{ TShorcutGrabber }

constructor TShorcutGrabber.Create(AOwner: TComponent);
begin
  FGrabForm := TForm.Create(AOwner);
  FGrabForm.BorderStyle := bsToolWindow;
  FGrabForm.KeyPreview  := true;
  FGrabForm.Position    := poScreenCenter;
  FGrabForm.OnKeyDown   := OnGrabFormKeyDown;
  FGrabForm.Caption     := 'Press a hotkey...';
  with TLabel.Create(AOwner) do begin
    Caption   := 'Press a combination of keys...';
    Align     := alClient;
    Alignment := taCenter;
    Layout    := tlCenter;
    Parent    := FGrabForm;
  end;
  FGrabForm.Width    := 200;
  FGrabForm.Height   := 100;
  FGrabForm.AutoSize := true;
  FGrabForm.ShowModal;
  //Return focus to AOwner
  if AOwner is TWinControl then
    TWinControl(AOwner).SetFocus;
end;

destructor TShorcutGrabber.Destroy;
begin
  FreeAndNil(FGrabForm);
  inherited;
end;

class function TShorcutGrabber.Execute(AOwner: TComponent): TShortcut;
var
  Grabber: TShorcutGrabber;
begin
  Grabber := TShorcutGrabber.Create(AOwner);
  try
    Result := Grabber.FHotkey;
  finally
    Grabber.Free;
  end;
end;

procedure TShorcutGrabber.OnGrabFormKeyDown(Sender: TObject; var AKey: Word;
  AShift: TShiftState);
begin
  if not (AKey in [VK_CONTROL, VK_LCONTROL, VK_RCONTROL,
             VK_SHIFT, VK_LSHIFT, VK_RSHIFT,
             VK_MENU, VK_LMENU, VK_RMENU,
             VK_LWIN, VK_RWIN,
             0, $FF])
  then begin
    if (AKey=VK_ESCAPE) and (AShift=[]) then
      FHotkey := 0
    else begin
      if AShift <> [] then
        FHotkey := ShortCut(AKey, AShift)
      else
        FHotkey := ShortCut(AKey, [ssAlt]);
    end;
    FGrabForm.ModalResult := mrOk;
  end;
end;

end.
