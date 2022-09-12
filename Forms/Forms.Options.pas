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

unit Forms.Options;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Classes,
  Controls, Forms, Dialogs, Forms.Dialog.BaseEntity, VirtualTrees,
  Frame.BaseEntity;

type

  { TfrmOptions }

  TfrmOptions = class(TfrmDialogBase)
    
  private
    { Private declarations }
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; APage: TPageFrameClass); overload;

    class function Execute(AOwner: TComponent; APage: TPageFrameClass = nil): Integer;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  Frame.Options.General, Frame.Options.Advanced, Frame.Options.Trayicon,
  Frame.Options.Stats, Frame.Options.Autorun, AppConfig.Main, Kernel.Logger,
  Forms.Main, Frame.Options.Hotkey, Frame.Options.MainWindow,
  Utility.Misc;

{$R *.lfm}

{ TfrmOptions }

constructor TfrmOptions.Create(AOwner: TComponent; APage: TPageFrameClass);
begin
  FDefaultPage := APage;
  inherited Create(AOwner);
end;

class function TfrmOptions.Execute(AOwner: TComponent;
  APage: TPageFrameClass): Integer;
begin
  TASuiteLogger.Info('Opening form Options', []);

  Result := mrCancel;

  if Assigned(frmOptions) then
    Exit;

  frmOptions := TfrmOptions.Create(AOwner, APage);
  try
    SetFormPositionFromConfig(frmOptions);

    Result := frmOptions.ShowModal;

    Config.SaveConfig;
    Config.AfterUpdateConfig;
  finally
    FreeAndNil(frmOptions);
  end;
end;

function TfrmOptions.InternalLoadData: Boolean;
var
  FFrameAdvanced: PVirtualNode;
  StartTime: Cardinal;
begin
  StartTime := TASuiteLogger.EnterMethod('TfrmOptions.InternalLoadData', Self);

  try
    Result := True;
    //General
    FFrameGeneral  := AddFrameNode(vstCategory, nil, TfrmGeneralOptionsPage.Create(Self));
    AddFrameNode(vstCategory, nil, TfrmMainWindowOptionsPage.Create(Self));
    //Advanced
    FFrameAdvanced := AddFrameNode(vstCategory, nil, TfrmAdvancedOptionsPage.Create(Self));
    if Assigned(FFrameAdvanced) then
    begin
      AddFrameNode(vstCategory, FFrameAdvanced, TfrmAutorunOptionsPage.Create(Self));
      AddFrameNode(vstCategory, FFrameAdvanced, TfrmHotkeyOptionsPage.Create(Self));
    end;
    //TrayIcon
    AddFrameNode(vstCategory, nil, TfrmTrayiconOptionsPage.Create(Self));
    //Stats
    AddFrameNode(vstCategory, nil, TfrmStatsOptionsPage.Create(Self));
  finally
    TASuiteLogger.ExitMethod('TfrmOptions.InternalLoadData', Self, StartTime);
  end;
end;

function TfrmOptions.InternalSaveData: Boolean;
var
  StartTime: Cardinal;
begin
  StartTime := TASuiteLogger.EnterMethod('TfrmOptions.InternalSaveData', Self);

  try
    Result := True;
    Config.Changed := True;
    if frmMain.Visible then
      frmMain.FocusControl(frmMain.vstList);
  finally
    TASuiteLogger.ExitMethod('TfrmOptions.InternalSaveData', Self, StartTime);
  end;
end;

end.
