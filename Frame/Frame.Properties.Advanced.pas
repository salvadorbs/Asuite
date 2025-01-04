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

unit Frame.Properties.Advanced;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Classes, Dialogs, Frame.Properties.Base,
  ButtonedEdit, StdCtrls, DateUtils, DateTimePicker,
  ExtCtrls;

type

  { TfrmAdvancedPropertyPage }

  TfrmAdvancedPropertyPage = class(TfrmBasePropertyPage)
    cbDontInsertMFU: TCheckBox;
    cbDontInsertMRU: TCheckBox;
    cbHideSoftware: TCheckBox;
    cbShortcutDesktop: TCheckBox;
    edtHotkey: TButtonedEdit;
    grpOthers: TGroupBox;
    grpScheduler: TGroupBox;
    cxScheduler: TComboBox;
    dtpSchDate: TDateTimePicker;
    dtpSchTime: TDateTimePicker;
    grpHotkey: TGroupBox;

    pnlTop: TPanel;
    procedure cxSchedulerChange(Sender: TObject);
    procedure edtHotkeyRightButtonClick(Sender: TObject);
    procedure edtHotkeyChange(Sender: TObject);
    procedure edtHotkeyClick(Sender: TObject);
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
  Kernel.Enumerations, NodeDataTypes.Files, Forms.ShortcutGrabber, Kernel.Manager,
  DataModules.Icons, Kernel.ResourceStrings, LCLProc, Kernel.Consts;

{$R *.lfm}

{ TfrmAdvancedPropertyPage }

procedure TfrmAdvancedPropertyPage.cxSchedulerChange(Sender: TObject);
begin
  dtpSchDate.Enabled := cxScheduler.ItemIndex = Ord(smOnce);
  dtpSchTime.Enabled := (cxScheduler.ItemIndex = Ord(smOnce)) or
                        (cxScheduler.ItemIndex = Ord(smDaily));
end;

function TfrmAdvancedPropertyPage.GetImageIndex: Integer;
begin
  Result := AsuiteManager.IconsManager.GetIconIndex('advanced');
end;

function TfrmAdvancedPropertyPage.GetTitle: string;
begin
  Result := msgAdvanced;
end;

function TfrmAdvancedPropertyPage.InternalLoadData: Boolean;
begin
  Result := inherited;

  cxScheduler.Items.Add(cxScheduler_item0);
  cxScheduler.Items.Add(cxScheduler_item1);
  cxScheduler.Items.Add(cxScheduler_item2);
  cxScheduler.Items.Add(cxScheduler_item3);

  if Assigned(CurrentNodeData) then
  begin
    //Scheduler
    cxScheduler.ItemIndex  := Ord(CurrentNodeData.SchMode);
    dtpSchDate.Date        := CurrentNodeData.SchDateTime;
    dtpSchTime.Time        := CurrentNodeData.SchDateTime;
    cxSchedulerChange(Self);
    //Hotkey
    if (CurrentNodeData.IsHotkeyActive) then
      edtHotkey.Text         := ShortCutToText(CurrentNodeData.Hotkey);
    //Specific file settings
    cbHideSoftware.Checked := CurrentNodeData.HideFromMenu;
    if CurrentNodeData.IsFileItem then
    begin
      cbDontInsertMRU.Checked   := TvFileNodeData(CurrentNodeData).NoMRU;
      cbDontInsertMFU.Checked   := TvFileNodeData(CurrentNodeData).NoMFU;
      cbShortcutDesktop.Checked := TvFileNodeData(CurrentNodeData).ShortcutDesktop;
    end
    else
      if CurrentNodeData.IsCategoryItem then
      begin
        cbDontInsertMRU.Visible   := False;
        cbDontInsertMFU.Visible   := False;
        cbShortcutDesktop.Visible := False;
      end;
  end;

  edtHotkey.RightButton.Images := dmImages.ilIcons;
  edtHotkey.RightButton.ImagesWidth := ICON_SIZE_SMALL;
  edtHotkey.RightButton.ImageIndex := AsuiteManager.IconsManager.GetIconIndex('cancel');

  //Hide caret in hotkey control
  //HideCaret(edtHotkey.Handle);
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
    CurrentNodeData.Hotkey       := TextToShortCut(edtHotkey.Text);
    //Specific file settings
    CurrentNodeData.HideFromMenu := cbHideSoftware.Checked;
    if CurrentNodeData.IsFileItem then
    begin
      TvFileNodeData(CurrentNodeData).NoMRU := cbDontInsertMRU.Checked;
      TvFileNodeData(CurrentNodeData).NoMFU := cbDontInsertMFU.Checked;
      TvFileNodeData(CurrentNodeData).ShortcutDesktop := cbShortcutDesktop.Checked;
    end;
  end;
end;

procedure TfrmAdvancedPropertyPage.edtHotkeyChange(Sender: TObject);
var
  edtHotkey: TButtonedEdit;
begin
  if Sender is TButtonedEdit then
  begin
    edtHotkey := TButtonedEdit(Sender);
    edtHotkey.RightButton.Visible := edtHotkey.Text <> '';
  end;
end;

procedure TfrmAdvancedPropertyPage.edtHotkeyClick(Sender: TObject);
var
  strHotkey: string;
begin
  if Sender is TButtonedEdit then
  begin
    strHotkey := TfrmShortcutGrabber.Execute(Self, TButtonedEdit(Sender).Text);
    if (strHotkey <> '') then
      TButtonedEdit(Sender).Text := strHotkey;
  end;
end;

procedure TfrmAdvancedPropertyPage.edtHotkeyRightButtonClick(Sender: TObject);
begin
  edtHotkey.Text := '';
end;

end.
