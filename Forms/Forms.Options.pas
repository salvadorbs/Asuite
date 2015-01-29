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

unit Forms.Options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Forms.Dialog.BaseEntity, VirtualTrees,
  Vcl.ExtCtrls, Vcl.StdCtrls, DKLang, Frame.BaseEntity;

type
  TfrmOptions = class(TfrmDialogBase)
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; APage: TPageFrameClass);

    class function Execute(AOwner: TComponent; APage: TPageFrameClass = nil): Integer;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  Frame.Options.General, Frame.Options.Advanced, Frame.Options.TrayIcon,
  Frame.Options.Stats, Frame.Options.Autorun, AppConfig.Main,
  Forms.Main, Frame.Options.Hotkey, Frame.Options.MainWindow;

{$R *.dfm}

{ TfrmOptions }

constructor TfrmOptions.Create(AOwner: TComponent; APage: TPageFrameClass);
begin
  FDefaultPage := APage;
  inherited Create(AOwner);
end;

class function TfrmOptions.Execute(AOwner: TComponent;
  APage: TPageFrameClass): Integer;
var
  frm: TfrmOptions;
begin
  Result := mrCancel;
  frm := TfrmOptions.Create(AOwner, APage);
  try
    Result := frm.ShowModal;
  finally
    frm.Free;
  end;
end;

function TfrmOptions.InternalLoadData: Boolean;
var
  FFrameAdvanced: PVirtualNode;
begin
  //General
  FFrameGeneral  := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmGeneralOptionsPage.Create(Self)));
  AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmMainWindowOptionsPage.Create(Self)));
  //Advanced
  FFrameAdvanced := AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmAdvancedOptionsPage.Create(Self)));
  AddFrameNode(vstCategory, FFrameAdvanced, TPageFrameClass(TfrmAutorunOptionsPage.Create(Self)));
  AddFrameNode(vstCategory, FFrameAdvanced, TPageFrameClass(TfrmHotkeyOptionsPage.Create(Self)));
  //TrayIcon
  AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmTrayiconOptionsPage.Create(Self)));
  //Stats
  AddFrameNode(vstCategory, nil, TPageFrameClass(TfrmStatsOptionsPage.Create(Self)));
end;

function TfrmOptions.InternalSaveData: Boolean;
begin
  Config.Changed := True;
  if frmMain.Visible then
    frmMain.FocusControl(frmMain.vstList);
  LangManager.LanguageID := Config.LangID;
end;

end.
