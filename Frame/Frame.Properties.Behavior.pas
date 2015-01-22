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

unit Frame.Properties.Behavior;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DKLang, Frame.Properties.Base,
  Vcl.ExtCtrls;

type
  TfrmBehaviorPropertyPage = class(TfrmBasePropertyPage)
    DKLanguageController1: TDKLanguageController;
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
  Kernel.Enumerations, Forms.Options, Frame.Options.Items, AppConfig.Main;

{$R *.dfm}

{ TfrmMenuPropertyPage }

procedure TfrmBehaviorPropertyPage.btnChangeOrderClick(Sender: TObject);
begin
  TfrmOptions.Execute(Self.Parent, TfrmItemsOptionsPage);
end;

procedure TfrmBehaviorPropertyPage.cxAutoExecuteChange(Sender: TObject);
begin
  btnChangeOrder.Enabled := (cxAutoExecute.ItemIndex <> 0);
end;

function TfrmBehaviorPropertyPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetLargeIconIndex('behavior');
end;

function TfrmBehaviorPropertyPage.GetTitle: string;
begin
  Result := DKLangConstW('msgBehavior');
end;

function TfrmBehaviorPropertyPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    //Insert cat specific setting
    if CurrentNodeData.DataType = vtdtCategory then
      cxWindowState.Items.Insert(0, DKLangConstW('msgDefaultItemSettings'));
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
