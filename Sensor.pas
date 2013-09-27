{
Copyright (C) 2006-2013 Matteo Salvi

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

unit Sensor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type

  TfrmSensor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteActionSensor(SensorSide: Integer);
  private
    { Private declarations }
  public
    Side: Integer; //0 Top, 1 Left, 2 Right, 3 Bottom
    { Public declarations }
  end;

const
  crCurTL = 1;
  crCurBR = 2;

{ Mouse Sensors}
procedure CreateFormSensors;
procedure CloseFormSensors;

implementation

uses Main, ulAppConfig, AppConfig, udClassicMenu;

{$R *.dfm}

procedure CreateFormSensors;
var
  I : Integer;
  frmSensor :TfrmSensor;
begin
  //Create FormSensors and set its height and width
  for I := 0 to 3 do
  begin
    frmSensor := nil;
    case I of
      0: //Top
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          frmSensor := TfrmSensor.Create(Application);
          frmSensor.Height := 1;
          frmSensor.Width  := GetDeviceCaps(GetDC(frmMain.Handle), HORZRES);
        end
      end;
      1: //Left
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          frmSensor := TfrmSensor.Create(Application);
          frmSensor.Height := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES);
          frmSensor.Width  := 1;
        end
      end;
      2: //Right
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          frmSensor := TfrmSensor.Create(Application);
          frmSensor.Left   := GetDeviceCaps(GetDC(frmMain.Handle), HORZRES) - 1;
          frmSensor.Height := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES);
          frmSensor.Width  := 1;
          frmSensor.Tag    := 1;
        end
      end;
      3: //Bottom
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          frmSensor := TfrmSensor.Create(Application);
          frmSensor.Top    := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES) - 1;
          frmSensor.Height := 1;
          frmSensor.Tag    := 1;
          frmSensor.Width  := GetDeviceCaps(GetDC(frmMain.Handle), HORZRES);
        end
      end;
    end;
    //If assigned, show current frmSensor and set Side (to I)
    //and FormSensors[I] (to current frmSensor)
    if Assigned(frmSensor) then
    begin
      frmSensor.Show;
      frmSensor.Side := I;
      FormSensors[I] := frmSensor;
      frmSensor.FormStyle := fsStayOnTop;
      frmSensor.BringToFront;
    end;
  end;
end;

procedure CloseFormSensors;
var
  I : Integer;
begin
  //If assigned, close and free FormSensors
  for I := 0 to 3 do
    if Assigned(FormSensors[I]) then
    begin
      FreeAndNil(FormSensors[i]);
    end;
end;

procedure TfrmSensor.FormCreate(Sender: TObject);
begin
  Screen.Cursors[crCurTL] := LoadCursorFromFile(PChar(SUITE_CURRENTTHEME_PATH + ICONS_DIR + 'asuiteTL.cur'));
  Screen.Cursors[crCurBR] := LoadCursorFromFile(PChar(SUITE_CURRENTTHEME_PATH + ICONS_DIR + 'asuiteBR.cur'));
  Self.Top  := 0;
  Self.Left := 0;
end;

procedure TfrmSensor.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ExecuteActionSensor(Config.SensorLeftClick[Side])
  else
    ExecuteActionSensor(Config.SensorRightClick[Side]);
end;

procedure TfrmSensor.ExecuteActionSensor(SensorSide: Integer);
begin
  case SensorSide of
    1: frmMain.ShowMainForm(self);
    2: ClassicMenu.ShowGraphicMenu;
    3: ClassicMenu.ShowClassicMenu;
  end;
//  frmMain.BringToFront;
end;

procedure TfrmSensor.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  cursor := 0 + Tag;
end;

procedure TfrmSensor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  //Transparent and sensor (form) topmost
  Params.Style   := WS_POPUP or WS_VISIBLE;
  //ontop over taskbar
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
  Params.WndParent := GetDesktopwindow;
  //this hide the form from taskbar
  Params.ExStyle := Params.ExStyle + WS_EX_NOACTIVATE;
end;


end.
