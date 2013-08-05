unit AdvancedPropertyPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BasePropertyPage, Vcl.ComCtrls,
  Vcl.StdCtrls;

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
    cbHotKey: TCheckBox;
    cxHotkeyMod: TComboBox;
    cxHotKeyCode: TComboBox;
    procedure cxSchedulerChange(Sender: TObject);
    procedure cbHotKeyClick(Sender: TObject);
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
  AppConfig, PropertyItem, ulEnumerations, ulNodeDataTypes;

{$R *.dfm}

{ TfrmAdvancedPropertyPage }

procedure TfrmAdvancedPropertyPage.cbHotKeyClick(Sender: TObject);
begin
  cxHotkeyMod.Enabled  := cbHotKey.Checked;
  cxHotKeyCode.Enabled := cbHotKey.Checked;
end;

procedure TfrmAdvancedPropertyPage.cxSchedulerChange(Sender: TObject);
begin
  dtpSchDate.Enabled := cxScheduler.ItemIndex = Ord(smOnce);
  dtpSchTime.Enabled := (cxScheduler.ItemIndex = Ord(smOnce)) or
                        (cxScheduler.ItemIndex = Ord(smDaily));
end;

function TfrmAdvancedPropertyPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_Advanced;
end;

function TfrmAdvancedPropertyPage.GetTitle: string;
begin
  Result := 'Advanced';
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
    cbHotKey.Checked       := CurrentNodeData.Hotkey;
    cxHotkeyMod.ItemIndex  := CurrentNodeData.HotkeyMod;
    cxHotKeyCode.ItemIndex := CurrentNodeData.HotkeyCode;
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
    //Hotkey
    CurrentNodeData.HotkeyMod    := cxHotkeyMod.ItemIndex;
    CurrentNodeData.HotkeyCode   := cxHotKeyCode.ItemIndex;
    CurrentNodeData.Hotkey       := cbHotKey.Checked;
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
