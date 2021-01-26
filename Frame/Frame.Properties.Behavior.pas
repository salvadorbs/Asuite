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

unit Frame.Properties.Behavior;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Dialogs, StdCtrls, Frame.Properties.Base, ExtCtrls, DefaultTranslator, Classes;

type

  { TfrmBehaviorPropertyPage }

  TfrmBehaviorPropertyPage = class(TfrmBasePropertyPage)
    
    grpAutoExecute: TGroupBox;
    cxAutoExecute: TComboBox;
    btnChangeOrder: TButton;
    pnl1: TPanel;
    grpOnExecute: TGroupBox;
    cxActionOnExe: TComboBox;
    grpWindowState: TGroupBox;
    cxWindowState: TComboBox;
    procedure cxAutoExecuteChange(Sender: TObject);
    procedure btnChangeOrderClick(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmBehaviorPropertyPage: TfrmBehaviorPropertyPage;

implementation

uses
  Kernel.Enumerations, Forms.Options, Frame.Options.Autorun, AppConfig.Main,
  Kernel.ResourceStrings;

{$R *.lfm}

{ TfrmMenuPropertyPage }

procedure TfrmBehaviorPropertyPage.btnChangeOrderClick(Sender: TObject);
begin
  CurrentNodeData.Autorun := TAutorunType(cxAutoExecute.ItemIndex);
  TfrmOptions.Execute(Self.Parent, TfrmAutorunOptionsPage);
end;

procedure TfrmBehaviorPropertyPage.cxAutoExecuteChange(Sender: TObject);
begin
  btnChangeOrder.Enabled := (cxAutoExecute.ItemIndex <> 0);
end;

function TfrmBehaviorPropertyPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('behavior');
end;

function TfrmBehaviorPropertyPage.GetTitle: string;
begin
  Result := msgBehavior;
end;

function TfrmBehaviorPropertyPage.InternalLoadData: Boolean;
begin
  Result := inherited;

  cxAutoExecute.Items.Add(cxAutoExecute_item0);
  cxAutoExecute.Items.Add(cxAutoExecute_item1);
  cxAutoExecute.Items.Add(cxAutoExecute_item2);
  cxAutoExecute.Items.Add(cxAutoExecute_item3);

  cxActionOnExe.Items.Add(cxActionOnExe_item0);
  cxActionOnExe.Items.Add(cxActionOnExe_item1);
  cxActionOnExe.Items.Add(cxActionOnExe_item2);
  cxActionOnExe.Items.Add(cxActionOnExe_item3);

  cxWindowState.Items.Add(cxWindowState_item0);
  cxWindowState.Items.Add(cxWindowState_item1);
  cxWindowState.Items.Add(cxWindowState_item2);

  if Assigned(CurrentNodeData) then
  begin
    //Insert cat specific setting
    if CurrentNodeData.DataType = vtdtCategory then
    begin
      cxWindowState.Items.Insert(0, msgDefaultItemSettings);
    end;
    cxActionOnExe.ItemIndex := Ord(CurrentNodeData.ActionOnExe);
    cxAutoExecute.ItemIndex := Ord(CurrentNodeData.Autorun);
    btnChangeOrder.Enabled  := (cxAutoExecute.ItemIndex <> 0);
    cxWindowState.ItemIndex := CurrentNodeData.WindowState
  end;
end;

function TfrmBehaviorPropertyPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    CurrentNodeData.ActionOnExe := TActionOnExecute(cxActionOnExe.ItemIndex);
    CurrentNodeData.Autorun     := TAutorunType(cxAutoExecute.ItemIndex);
    CurrentNodeData.WindowState := cxWindowState.ItemIndex;
  end;
end;

end.
