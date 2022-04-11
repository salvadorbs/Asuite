{
Copyright (C) 2006-2021 Matteo Salvi

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

unit Kernel.BaseMainForm;

{$MODE DelphiUnicode}

interface

uses
  Forms, Controls, Classes, Dialogs, Graphics, LCLIntf;

type

  { TBaseMainForm }

  TBaseMainForm = class(TForm)
  private
    FSessionEnding : Boolean;
    procedure ApplicationEndSession(Sender: TObject);
    procedure ApplicationMinimize(Sender: TObject);
    procedure ApplicationQueryEndSession(var Cancel: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ShowMainForm(const Sender: TObject);
    procedure HideMainForm;
    procedure CloseASuite(AForceExit: Boolean);

    property SessionEnding: Boolean read FSessionEnding write FSessionEnding;
  end;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Utility.Misc;

constructor TBaseMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Config.ASuiteState := lsNormal;

  Application.OnMinimize := ApplicationMinimize;
  Application.OnQueryEndSession := ApplicationQueryEndSession;
  Application.OnEndSession := ApplicationEndSession;
end;

destructor TBaseMainForm.Destroy;
begin
  inherited;
end;

procedure TBaseMainForm.ShowMainForm(const Sender: TObject);
begin
  //From CoolTrayicon source
  if Application.MainForm <> nil then
  begin
    // Restore the app, but don't automatically show its taskbar icon
    // Show application's TASKBAR icon (not the tray icon)
    Self.ShowInTaskBar := stDefault;

    Application.Restore;

    // Show the form itself
    if Application.MainForm.WindowState = wsMinimized then
      Application.MainForm.WindowState := wsNormal;    // Override minimized state
    Application.MainForm.Visible := True;

    // Bring the main form (or its modal dialog) to the foreground
    SetForegroundWindow(Application.Handle);
    BringToFront;
  end;
end;

procedure TBaseMainForm.HideMainForm;
begin
  if Application.MainForm <> nil then
  begin
    // Hide the form itself (and thus any child windows)
    Application.MainForm.Visible := False;
    { Hide application's TASKBAR icon (not the tray icon). Do this AFTER
        the main form is hidden, or any child windows will redisplay the
        taskbar icon if they are visible. }
    Self.ShowInTaskBar := stNever;
  end;
end;

procedure TBaseMainForm.CloseASuite(AForceExit: Boolean);
begin
  if AForceExit or AskUserCloseApp then
  begin
    Config.ASuiteState := lsShutdown;
    Close;
  end;
end;

procedure TBaseMainForm.ApplicationMinimize(Sender: TObject);
begin
  if Config.TrayIcon then
    HideMainForm;
end;

procedure TBaseMainForm.ApplicationQueryEndSession(var Cancel: Boolean);
begin
  FSessionEnding := True;
end;

procedure TBaseMainForm.ApplicationEndSession(Sender: TObject);
begin
  //Force close ASuite on Windows shutdown
  CloseASuite(True);
end;

end.
