{
Copyright (C) 2006-2009 Matteo Salvi of SalvadorSoftware

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

unit ASuiteForm;

interface

uses
  Forms, Controls, Classes, ActnList, Graphics, Windows, virtualtrees, win32int,
  InterfaceBase;

type
  TASuiteForm = class(TForm)
  private
    FSessionEnding : Boolean;
    FStartUpTime   : Boolean;
    FShutdownTime  : Boolean;
    FOldPoint      : TPoint;
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg : TWMEndSession); message WM_ENDSESSION;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RefreshTranslation; virtual;
    property SessionEnding: Boolean read FSessionEnding write FSessionEnding;
    property StartUpTime: Boolean read FStartUpTime write FStartUpTime;
    property ShutdownTime: Boolean read FShutdownTime write FShutdownTime;
    property OldPoint: TPoint read FOldPoint write FOldPoint;
  published

  end;

implementation

constructor TASuiteForm.Create(AOwner: TComponent);
begin
  inherited;

  FStartUpTime  := True;
  FShutdownTime := False;
end;

destructor TASuiteForm.Destroy;
begin
  inherited;
end;

procedure TASuiteForm.RefreshTranslation;
begin

end;

procedure TASuiteForm.WMQueryEndSession(var Message: TMessage);
begin
  FSessionEnding := True;
  Message.Result := 1;
end;

procedure TASuiteForm.WMEndSession(var Msg : TWMEndSession);
begin
  //Close ASuite on Windows shutdown
  if Msg.EndSession = True then
  begin
    FShutdownTime := True;
    Close;
  end;
end;

end.
