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
  Dialogs;

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

function SetLayeredWindowAttributes(hwnd: LongInt;crKey: byte; bAlpha: byte;
                                    dwFlags: LongInt): LongInt; stdcall;
external 'user32.dll' name 'SetLayeredWindowAttributes';

var
  frmSensor     : TfrmSensor;

const
  crNewCur = 1;

implementation

uses Main, CommonUtils,Menu;

{$R *.dfm}

procedure TfrmSensor.FormCreate(Sender: TObject);
begin
  Screen.Cursors[crNewCur] := LoadCursorFromFile(PChar(PathIcons + 'asuite.cur'));
end;

procedure TfrmSensor.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ExecuteActionSensor(LauncherOptions.SensorLeftClick)
  else
    ExecuteActionSensor(LauncherOptions.SensorRightClick);
end;

procedure TfrmSensor.ExecuteActionSensor(ArraySensor: Array of Integer);
Var Point: TPoint;
begin
  with frmMain do
    case ArraySensor[Side] of
      1: ShowMainForm(self);
      2: //Default Menu
      begin
        if frmMenu.Visible then
          frmMenu.CloseMenu
        else
          frmMenu.OpenMenu;
      end;
      3: //Classic Menu
      begin        
        //Get Mouse coordinates
        GetCursorPos(Point);
        //Classic Menu
        SetForegroundWindow(frmMain.Handle);
        //Populate classic menu at runtime
        UpdateClassicMenu(vstList, pmTrayicon);
        //Show classic menu
        pmTrayicon.Popup(Point.X, Point.Y);
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
  Params.ExStyle   := WS_EX_TRANSPARENT or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  Params.WndParent := GetDesktopWindow;
end;


end.
