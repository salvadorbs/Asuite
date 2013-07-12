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

unit ASuiteForm;

interface

uses
  Forms, Controls, Classes, Dialogs, ActnList, Graphics, Windows, Messages, GTForm;

type
  TASuiteForm = class(TGTForm)
  private
    FSessionEnding : Boolean;
    FOldPoint      : TPoint;
    procedure WMQueryEndSession(var Message: TMessage); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg : TWMEndSession); message WM_ENDSESSION;
    procedure WMExitSizeMove(var Message: TMessage) ; message WM_EXITSIZEMOVE;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMHotKey(Var Msg : TWMHotKey); message WM_HOTKEY;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RefreshTranslation; virtual;
    property SessionEnding: Boolean read FSessionEnding write FSessionEnding;
    property OldPoint: TPoint read FOldPoint write FOldPoint;
  published

  end;

implementation

uses
  ulTreeView, Main, ulAppConfig, ulNodeDataTypes, AppConfig, udClassicMenu,
  ulEnumerations, ulCommonUtils;

constructor TASuiteForm.Create(AOwner: TComponent);
begin
  inherited;
  Config.ASuiteState := asNormal;
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

procedure TASuiteForm.WMSysCommand(var Message: TWMSysCommand);
begin
  inherited;
  if Message.cmdType = SC_MINIMIZE then
  begin
    if IsWindowVisible(Application.Handle) then
      ShowWindow(Application.Handle, SW_HIDE);
  end;
end;

procedure TASuiteForm.WMEndSession(var Msg : TWMEndSession);
begin
  //Close ASuite on Windows shutdown
  if Msg.EndSession = True then
  begin
    Config.ASuiteState := asShutdown;
    Close;
  end;
end;

procedure TASuiteForm.WMExitSizeMove(var Message: TMessage);
begin
  Config.Changed := True;
  RefreshList(frmMain.vstList);
end;

procedure TASuiteForm.WMHotKey(var Msg: TWMHotKey);
var
  NodeData : PBaseData;
begin
  if Config.HotKey then
  begin
    //Show frmMain or execute a software (or group)
    if Msg.HotKey = Integer(frmMain.Handle) then
    begin
      if frmMain.Showing then
         frmMain.HideMainForm
      else
        frmMain.ShowMainForm(self);
    end
    else begin
      if Msg.HotKey = frmMenuID then
      begin
        ClassicMenu.ShowTrayiconMenu;
      end
      else begin
        NodeData := frmMain.vstList.GetNodeData(HotKeyApp[Msg.HotKey]);
        if Assigned(NodeData) then
        begin
          if (NodeData.Data.DataType <> vtdtCategory) then // <> Category
          begin
//            PvCustomRealNodeData(NodeData).Execute(frmMain.vstList,ProcessINfo);
//            AddMRU(frmMain.vstList,CoolTrayIcon1,NodeData.pNode,NodeData.DontInsertMRU);
//            RunActionOnExe(NodeData);
          end;
        end
        else begin
          ShowMessage(msgErrGeneric, True);
          exit;
        end;
      end;
    end;
  end;

end;

end.
