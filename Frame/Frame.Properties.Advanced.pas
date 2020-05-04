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

unit Frame.Properties.Advanced;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Frame.Properties.Base, Vcl.ComCtrls,
  Vcl.StdCtrls, DKLang, DateUtils;

type
  TfrmAdvancedPropertyPage = class(TfrmBasePropertyPage)
    cbShortcutDesktop: TCheckBox;
    grpScheduler: TGroupBox;
    cxScheduler: TComboBox;
    dtpSchDate: TDateTimePicker;
    dtpSchTime: TDateTimePicker;
    grpHotkey: TGroupBox;
    GroupBox1: TGroupBox;
    cbHideSoftware: TCheckBox;
    cbDontInsertMRU: TCheckBox;
    cbDontInsertMFU: TCheckBox;
    DKLanguageController1: TDKLanguageController;
    hkHotkey: THotKey;
    cbHotKey: TCheckBox;
    procedure cxSchedulerChange(Sender: TObject);
    procedure cbHotKeyClick(Sender: TObject);
    procedure hkHotkeyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  frmAdvancedPropertyPage: TfrmAdvancedPropertyPage;

implementation

uses
  Kernel.Enumerations, NodeDataTypes.Files, Forms.ShortcutGrabber, AppConfig.Main;

{$R *.dfm}

{ TfrmAdvancedPropertyPage }


procedure TfrmAdvancedPropertyPage.cbHotKeyClick(Sender: TObject);
begin
  hkHotkey.Enabled := cbHotKey.Checked;
end;

procedure TfrmAdvancedPropertyPage.cxSchedulerChange(Sender: TObject);
begin
  dtpSchDate.Enabled := cxScheduler.ItemIndex = Ord(smOnce);
  dtpSchTime.Enabled := (cxScheduler.ItemIndex = Ord(smOnce)) or
                        (cxScheduler.ItemIndex = Ord(smDaily));
end;

function TfrmAdvancedPropertyPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('advanced');
end;

function TfrmAdvancedPropertyPage.GetTitle: string;
begin
  Result := DKLangConstW('msgAdvanced');
end;

procedure TfrmAdvancedPropertyPage.hkHotkeyMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  hkHotkey.HotKey := TShorcutGrabber.Execute(Self);
end;

function TfrmAdvancedPropertyPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    //Scheduler
    cxScheduler.ItemIndex  := Ord(CurrentNodeData.SchMode);
    dtpSchDate.Date        := CurrentNodeData.SchDateTime;
    dtpSchTime.Time        := CurrentNodeData.SchDateTime;
    cxSchedulerChange(Self);
    //Hotkey
    cbHotKey.Checked       := CurrentNodeData.ActiveHotkey;
    hkHotkey.HotKey        := CurrentNodeData.Hotkey;
    cbHotKeyClick(Self);
    //Specific file settings
    cbHideSoftware.Checked := CurrentNodeData.HideFromMenu;
    if CurrentNodeData.DataType = vtdtFile then
    begin
      cbDontInsertMRU.Checked   := TvFileNodeData(CurrentNodeData).NoMRU;
      cbDontInsertMFU.Checked   := TvFileNodeData(CurrentNodeData).NoMFU;
      cbShortcutDesktop.Checked := TvFileNodeData(CurrentNodeData).ShortcutDesktop;
    end
    else
      if CurrentNodeData.DataType = vtdtCategory then
      begin
        cbDontInsertMRU.Enabled   := False;
        cbDontInsertMFU.Enabled   := False;
        cbShortcutDesktop.Enabled := False;
      end;
  end;
end;

function TfrmAdvancedPropertyPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    //Scheduler
    CurrentNodeData.SchMode      := TSchedulerMode(cxScheduler.ItemIndex);
    CurrentNodeData.SchDateTime  := Int(dtpSchDate.Date) + Frac(dtpSchTime.Time);
    CurrentNodeData.SchDateTime  := RecodeSecond(CurrentNodeData.SchDateTime, 0);
    //Hotkey
    CurrentNodeData.Hotkey       := hkHotkey.HotKey;
    CurrentNodeData.ActiveHotkey := cbHotKey.Checked;
    //Specific file settings
    CurrentNodeData.HideFromMenu := cbHideSoftware.Checked;
    if CurrentNodeData.DataType = vtdtFile then
    begin
      TvFileNodeData(CurrentNodeData).NoMRU := cbDontInsertMRU.Checked;
      TvFileNodeData(CurrentNodeData).NoMFU := cbDontInsertMFU.Checked;
      TvFileNodeData(CurrentNodeData).ShortcutDesktop := cbShortcutDesktop.Checked;
    end;
  end;
end;

end.
