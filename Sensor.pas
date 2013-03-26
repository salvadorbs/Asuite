{
Copyright (C) 2006-2008 Matteo Salvi of SalvadorSoftware

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit Sensor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LCLType;

type
  TfrmSensor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteActionSensor(ArraySensor: Array of Integer);
  private
    { Private declarations }
  public
    Side: Integer; //0 Top, 1 Left, 2 Right, 3 Bottom
    { Public declarations }
  end;

var
  frmSensor     : TfrmSensor;

const
  crNewCur = 1;

{ Mouse Sensors}
procedure CreateFormSensors;
procedure CloseFormSensors;

implementation

uses Main, ulAppConfig, AppConfig, udClassicMenu;

{$R *.dfm}

procedure CreateFormSensors;
var
  I : Integer;
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
          Application.CreateForm(TfrmSensor,frmSensor);
          frmSensor.Height := 1;
          frmSensor.Width  := GetDeviceCaps(GetDC(frmMain.Handle), HORZRES);
        end
      end;
      1: //Left
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          Application.CreateForm(TfrmSensor,frmSensor);
          frmSensor.Height := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES);
          frmSensor.Width  := 1;
        end
      end;
      2: //Right
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          Application.CreateForm(TfrmSensor,frmSensor);
          frmSensor.Left   := GetDeviceCaps(GetDC(frmMain.Handle), HORZRES) - 1;
          frmSensor.Height := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES);
          frmSensor.Width  := 1;
        end
      end;
      3: //Bottom
      begin
        if (Config.SensorLeftClick[I] <> 0) or (Config.SensorRightClick[I] <> 0) then
        begin
          Application.CreateForm(TfrmSensor,frmSensor);
          frmSensor.Top    := GetDeviceCaps(GetDC(frmMain.Handle), VERTRES) - 1;
          frmSensor.Height := 1;
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
  Screen.Cursors[crNewCur] := LoadCursorFromFile(PChar(SUITE_ICONS_PATH + 'asuite.cur'));
  Self.Top  := 0;
  Self.Left := 0;
end;

procedure TfrmSensor.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ExecuteActionSensor(Config.SensorLeftClick)
  else
    ExecuteActionSensor(Config.SensorRightClick);
end;

procedure TfrmSensor.ExecuteActionSensor(ArraySensor: Array of Integer);
Var Point: TPoint;
begin
  with frmMain do
    case ArraySensor[Side] of
      1: ShowMainForm(self);
      2: //Default Menu
      begin
        //TODO not yet converted from 1.X source
        //if frmMenu.Visible then
        //  frmMenu.CloseMenu
        //else
        //  frmMenu.OpenMenu;
      end;
      3: //Classic Menu
      begin
        ClassicMenu.ShowTrayiconMenu;
      end;
    end;
end;

procedure TfrmSensor.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  cursor := crNewCur;
end;

procedure TfrmSensor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  //Transparent and sensor (form) topmost
  Params.Style   := WS_POPUP or WS_VISIBLE;
  //this show the form in taskbar
  //Params.WndParent := GetDesktopWindow;
end;


end.
