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

unit Kernel.BaseMainForm;

interface

uses
  Forms, Controls, Classes, Dialogs, ActnList, Graphics, Windows, Messages;

type
  TBaseMainForm = class(TForm)
  private
    FSessionEnding : Boolean;
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg : TWMEndSession); message WM_ENDSESSION;
    procedure WMExitSizeMove(var Message: TMessage) ; message WM_EXITSIZEMOVE;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMHotKey(Var Msg : TWMHotKey); message WM_HOTKEY;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ShowMainForm(const Sender: TObject);
    procedure HideMainForm;

    property SessionEnding: Boolean read FSessionEnding write FSessionEnding;
  end;

implementation

uses
  AppConfig.Main, NodeDataTypes.Custom, Kernel.Consts, DataModules.TrayMenu,
  Kernel.Enumerations, USingleInst, VirtualTree.Methods;

constructor TBaseMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Config.ASuiteState := lsNormal;
  UseLatestCommonDialogs := False;
end;

procedure TBaseMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SingleInst.CreateParams(Params);
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
    ShowWindow(Application.Handle, SW_RESTORE);
    Application.Restore;
    // Show the form itself
    if Application.MainForm.WindowState = wsMinimized then
      Application.MainForm.WindowState := wsNormal;    // Override minimized state
    Application.MainForm.Visible := True;
    // Bring the main form (or its modal dialog) to the foreground
    SetForegroundWindow(Application.Handle);
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
    if IsWindowVisible(Application.Handle) then
      ShowWindow(Application.Handle, SW_HIDE);
  end;
end;

procedure TBaseMainForm.WMQueryEndSession(var Message: TMessage);
begin
  FSessionEnding := True;
  Message.Result := 1;
end;

procedure TBaseMainForm.WMSysCommand(var Message: TWMSysCommand);
begin
  inherited;
  if Message.cmdType = SC_MINIMIZE then
    HideMainForm;
end;

procedure TBaseMainForm.WndProc(var Msg: TMessage);
begin
  if not SingleInst.HandleMessages(Self.Handle, Msg) then
    inherited;
end;

procedure TBaseMainForm.WMEndSession(var Msg : TWMEndSession);
begin
  //Close ASuite on Windows shutdown
  if Msg.EndSession = True then
  begin
    Config.ASuiteState := lsShutdown;
    Close;
  end;
end;

procedure TBaseMainForm.WMExitSizeMove(var Message: TMessage);
begin
  Config.Changed := True;
  TVirtualTreeMethods.Create.RefreshList(nil);
end;

procedure TBaseMainForm.WMHotKey(var Msg: TWMHotKey);
var
  NodeData: TvCustomRealNodeData;
  intHandle: Integer;
begin
  if Config.HotKey then
  begin
    // Show frmMain
    if Msg.HotKey = Integer(Self.Handle) then
    begin
      if Self.Showing then
        HideMainForm
      else
        ShowMainForm(Self);
    end
    else
    begin
      case Msg.HotKey of
        // Show Graphic Menu
        frmGMenuID:
          begin
            dmTrayMenu.ShowGraphicMenu;
          end;
        // Show Classic Menu
        frmCMenuID:
          begin
            dmTrayMenu.ShowClassicMenu;
          end;
      else
        begin
          // Execute item
          NodeData := Config.ListManager.HotKeyItemList.IndexOfID(Msg.HotKey);
          if Assigned(NodeData) then
          begin
            if (NodeData.DataType <> vtdtSeparator) then
              NodeData.Execute(True, NodeData.DataType = vtdtCategory, False);

            TVirtualTreeMethods.Create.RefreshList(nil);
          end;
        end;
      end;
    end;
  end;
end;

end.
