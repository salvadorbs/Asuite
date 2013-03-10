{
Copyright (C) 2006-2008 Matteo Salvi of SalvadorSoftware

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit PropertyGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons;

type
  TfrmPropertyGroup = class(TForm)
    OpenDialog1: TOpenDialog;
    btnOk: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    tsInfo1: TTabSheet;
    tsInfo2: TTabSheet;
    lbScheduler: TLabel;
    cxScheduler: TComboBox;
    dtpSchDate: TDateTimePicker;
    dtpSchTime: TDateTimePicker;
    btnAdd: TButton;
    btnReplace: TButton;
    btnDelete: TButton;
    lxPathExe: TListBox;
    lbPathExe: TLabel;
    edtName: TEdit;
    lbName: TLabel;
    cbDontInsertMRU: TCheckBox;
    cxHotkey1: TComboBox;
    cbHotKey: TCheckBox;
    cxHotKey2: TComboBox;
    cxAutoExecute: TComboBox;
    lbAutoExecute: TLabel;
    cxWindowState: TComboBox;
    lbWindowState: TLabel;
    lbActionOnExe: TLabel;
    cxActionOnExe: TComboBox;
    lbPathIcon: TLabel;
    edtPathIcon: TEdit;
    btnBrowseIcon: TButton;
    edtPathExe: TEdit;
    btnBrowseExe: TButton;
    cbHideSoftware: TCheckBox;
    bbtnDown: TBitBtn;
    bbtnUp: TBitBtn;
    btnChangeOrder: TButton;
    procedure TranslateForm(Lingua:string);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Browse(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbHotKeyClick(Sender: TObject);
    procedure lxPathExeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lxPathExeClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure cxSchedulerChange(Sender: TObject);
    procedure bbtnUpClick(Sender: TObject);
    procedure bbtnDownClick(Sender: TObject);
    procedure btnChangeOrderClick(Sender: TObject);
    procedure cxAutoExecuteChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPropertyGroup: TfrmPropertyGroup;

implementation

{$R *.dfm}

uses Main, CommonUtils, OrderSoftware;

procedure TfrmPropertyGroup.TranslateForm(Lingua:string);
begin
  with frmMain.xmldTranslate.DocumentElement.ChildNodes['Form45'] do
  begin
    Caption                := ChildNodes['Form45Caption'].Text; 
    tsInfo1.Caption        := ChildNodes['TabGeneralCaption'].Text;
    tsInfo2.Caption        := ChildNodes['TabAdvancedCaption'].Text;
    //General
    lbName.Caption         := ChildNodes['LabelName'].Text;
    lbPathExe.Caption      := ChildNodes['LabelPathExeB'].Text;
    btnBrowseExe.Caption   := ChildNodes['ButtonBrowse'].Text;
    btnAdd.Caption         := ChildNodes['ButtonAddPath'].Text;
    btnReplace.Caption     := ChildNodes['ButtonReplacePath'].Text;
    btnDelete.Caption      := ChildNodes['ButtonDeletePath'].Text;
    //Advanced
    lbPathIcon.Caption     := ChildNodes['LabelPathIcon'].Text;
    btnBrowseIcon.Caption  := ChildNodes['ButtonBrowse'].Text;
    //On execution
    lbActionOnExe.Caption  := ChildNodes['LabelOnExecution'].Text;
    cxActionOnExe.Items.Add(ChildNodes['ComboBoxActionOnExe0'].Text);
    cxActionOnExe.Items.Add(ChildNodes['ComboBoxActionOnExe1B'].Text);
    cxActionOnExe.Items.Add(ChildNodes['ComboBoxActionOnExe2'].Text);
    cxActionOnExe.Items.Add(ChildNodes['ComboBoxActionOnExe3'].Text); 
    cbDontInsertMRU.Caption := ChildNodes['LabelDontInsertMRUB'].Text;
    cbHideSoftware.Caption  := ChildNodes['LabelHideSoftwareMenuB'].Text;
    //AutoExecute
    lbAutoExecute.Caption  := ChildNodes['LabelAutoExecute'].Text;
    cxAutoExecute.Items.Add(ChildNodes['ComboBoxAutoExecute0'].Text);
    cxAutoExecute.Items.Add(ChildNodes['ComboBoxAutoExecute1'].Text);
    cxAutoExecute.Items.Add(ChildNodes['ComboBoxAutoExecute2B'].Text); 
    cxAutoExecute.Items.Add(ChildNodes['ComboBoxAutoExecute3'].Text);
    btnChangeOrder.Caption := ChildNodes['ButtonChangeOrder'].Text;
    cbHotKey.Caption       := ChildNodes['CheckBoxHotKey'].Text;
    //Window State
    lbWindowState.Caption  := ChildNodes['LabelWindowState'].Text;
    cxWindowState.Items.Add(ChildNodes['ComboBoxWSNormal'].Text);
    cxWindowState.Items.Add(ChildNodes['ComboBoxWSMinimized'].Text);
    cxWindowState.Items.Add(ChildNodes['ComboBoxWSMaximized'].Text);
    //Scheduler
    lbScheduler.Caption  := ChildNodes['LabelScheduler'].Text;
    cxScheduler.Items.Add(ChildNodes['ComboBoxSchedulerMode0'].Text); 
    cxScheduler.Items.Add(ChildNodes['ComboBoxSchedulerMode1'].Text);
    cxScheduler.Items.Add(ChildNodes['ComboBoxSchedulerMode2'].Text);
    cxScheduler.Items.Add(ChildNodes['ComboBoxSchedulerMode3'].Text);
    //Info
    //Buttons
    btnOk.Caption          := ChildNodes['ButtonOk'].Text;
    btnCancel.Caption      := ChildNodes['ButtonCancel'].Text;
  end;
end;

procedure TfrmPropertyGroup.bbtnDownClick(Sender: TObject);    
var
  CurrIndex: Integer;
begin
  with lxPathExe do
  begin
    CurrIndex := ItemIndex;
    if (CurrIndex <> (-1 and (Count - 1))) and (Count <> 0)  then
    begin
      Items.Move(CurrIndex,CurrIndex + 1);
      Selected[CurrIndex + 1] := True;
    end;
  end;
end;

procedure TfrmPropertyGroup.bbtnUpClick(Sender: TObject);
var
  CurrIndex: Integer;
begin
  with lxPathExe do
  begin
    CurrIndex := ItemIndex;
    if (CurrIndex <> (-1 and 0)) and (Count <> 0)  then
    begin
      Items.Move(CurrIndex,CurrIndex - 1);
      Selected[CurrIndex - 1] := True;
    end;
  end;
end;

procedure TfrmPropertyGroup.Browse(Sender: TObject);
var
  PathTemp : String;
begin
  if (sender = btnBrowseExe) then
  begin
    OpenDialog1.Filter     := 'Executables (*.exe)|*.exe|All files|*.*';
    if (lxPathExe.count >= 1) and (lxPathExe.Items[0] <> '') and
       (lxPathExe.ItemIndex <> -1) then
      OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(lxPathExe.Items[lxPathExe.ItemIndex]));
  end;
  if (sender = btnBrowseIcon) then
  begin
    OpenDialog1.Filter     := 'Files supported (*.ico;*.exe)|*.ico;*.exe|All files|*.*';
    OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtPathIcon.Text));
  end;
  if (OpenDialog1.Execute) then
  begin
    PathTemp := AbsoluteToRelative(OpenDialog1.FileName);
    if (sender = btnBrowseExe) then
      edtPathExe.text    := PathTemp;
    if (sender = btnBrowseIcon) then
      edtPathIcon.text   := PathTemp;
  end;
  SetCurrentDir(ApplicationPath);
end;

procedure TfrmPropertyGroup.btnAddClick(Sender: TObject);
begin
  with lxPathExe do
  begin
    if (count = 10) then
    begin
      ShowMessage(ArrayMessages[14]);
      Exit;
    end;
    if ((Items.Count) = 1) and (Items[0] = '') then
      Items.Delete(0);
    Items.AddObject(edtPathExe.Text,Pointer(clBlack));
    edtPathExe.Clear;
  end;
end;

procedure TfrmPropertyGroup.btnCancelClick(Sender: TObject);
begin
  close;
  NewNode := false;
end;

procedure TfrmPropertyGroup.btnChangeOrderClick(Sender: TObject);
begin 
  //Autorun - Remove app and add app
  RemoveAutorunFromList(VNDataGroup);
  VNDataGroup.Autorun := cxAutoExecute.ItemIndex;
  AddAutorunInList(VNDataGroup);
  try
    if (VNDataGroup.Autorun = 1) or (VNDataGroup.Autorun = 2) then
      OrderSoftware.AutorunMode := True //Startup
    else
      if (VNDataGroup.Autorun = 3) then
        OrderSoftware.AutorunMode := False; //Shutdown
    Application.CreateForm(TfrmOrderSoftware, frmOrderSoftware);
    frmOrderSoftware.showmodal;
  finally
    frmOrderSoftware.Free;
  end;
end;

procedure TfrmPropertyGroup.btnDeleteClick(Sender: TObject);
begin
  with lxPathExe do
    if (ItemIndex <> -1) then
    begin
      Items.Delete(ItemIndex);
      ItemIndex := -1;
    end;
end;

procedure TfrmPropertyGroup.btnOkClick(Sender: TObject);
var
  I    : Integer;
begin
  try
    if edtName.Text = '' then
    begin
      ShowMessage(ArrayMessages[12]);
      exit;
    end;
    if (lxPathExe.Count = 0) then
    begin
      ShowMessage(ArrayMessages[13]);
      exit;
    end;
    VNDataGroup.Name := StringReplace(edtName.Text, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
    VNDataGroup.Name := StringReplace(VNDataGroup.Name, '&', '&&', [rfIgnoreCase,rfReplaceAll]);
    Finalize(VNDataGroup.PathExe);
    for I := 0 to lxPathExe.Count - 1 do
      VNDataGroup.PathExe[I]  := lxPathExe.Items[I];
    VNDataGroup.PathIcon      := edtPathIcon.Text;  
    //Autorun - Remove app and add app
    RemoveAutorunFromList(VNDataGroup);
    VNDataGroup.Autorun       := cxAutoExecute.ItemIndex;
    AddAutorunInList(VNDataGroup);
    VNDataGroup.ActionOnExe   := cxActionOnExe.ItemIndex; 
    //Others
    VNDataGroup.DontInsertMRU := cbDontInsertMRU.Checked;
    VNDataGroup.HideSoftwareMenu := cbHideSoftware.Checked;
    //Delete MRU entry if DontInsertMRU = true
    if (VNDataGroup.DontInsertMRU) then
      MRUList.Remove(VNDataGroup.pNode);
    if LauncherOptions.HotKey then
    begin
      //Unregister hotkey (if actived)
      if (VNDataGroup.HotKey) then
      begin
        for I := 0 to Length(HotKeyApp) - 1 do
          if HotKeyApp[I] = VNDataGroup.pNode then
            UnRegisterHotKey(frmMain.Handle, I)
      end;
      //Register hotkey
      VNDataGroup.HKModifier := cxHotKey1.ItemIndex;
      VNDataGroup.HKCode     := cxHotKey2.ItemIndex;
      if (cbHotKey.Checked) then
      begin
        if AddHotkey(frmMain.vstList,VNDataGroup.pNode,length(HotKeyApp)) = 0 then
        begin
          ShowMessage(ArrayMessages[22]);
          cbHotKey.Checked := false;
          exit;
        end;
      end;
    end;
    VNDataGroup.HotKey      := cbHotKey.Checked;
    VNDataGroup.WindowState := cxWindowState.ItemIndex;
    //Scheduler
    //Add software in SchedulerApp
    if (cxScheduler.ItemIndex <> 0) and (VNDataGroup.SchMode = 0) then
    begin
      SetLength(SchedulerApp,Length(SchedulerApp) + 1);
      SchedulerApp[Length(SchedulerApp) - 1] := VNDataGroup.pNode;
    end; 
    //Remove software from SchedulerApp
    if (cxScheduler.ItemIndex = 0) and (VNDataGroup.SchMode <> 0) then
    begin
      for I := 0 to Length(SchedulerApp) do
        if (SchedulerApp[I] = VNDataGroup.pNode) then
          SchedulerApp[I] := nil;
    end;
    VNDataGroup.SchMode     := cxScheduler.ItemIndex;
    VNDataGroup.SchDate     := FormatDateTime('dd/mm/yyyy',dtpSchDate.Date);
    VNDataGroup.SchTime     := FormatDateTime(LongTimeFormat,dtpSchTime.Date);
    VNDataGroup.ImageIndex  := IconAdd(frmMain.vstList, frmMain.ImageList1,
                                       VNDataGroup.pNode);
    NewNode                 := true;
  except
    on E : Exception do
    begin
      ShowMessageFmt(ArrayMessages[11],[E.ClassName,E.Message]);
      NewNode := false;
      close;
    end;
  end;
  close;
end;

procedure TfrmPropertyGroup.btnReplaceClick(Sender: TObject);
begin 
  with lxPathExe do
    if (ItemIndex <> -1) and (Count <> 0)  then
    begin
      Items.InsertObject(ItemIndex,edtPathExe.Text,Pointer(clBlack));
      Items.Delete(ItemIndex + 1);
      ItemIndex := -1; 
      edtPathExe.Clear;
    end;
end;

procedure TfrmPropertyGroup.cbHotKeyClick(Sender: TObject);
begin
  cxHotKey1.Enabled := cbHotKey.Checked;
  cxHotKey2.Enabled := cbHotKey.Checked;
end;

procedure TfrmPropertyGroup.cxAutoExecuteChange(Sender: TObject);
begin
  btnChangeOrder.Enabled := (cxAutoExecute.ItemIndex <> 0);
end;

procedure TfrmPropertyGroup.cxSchedulerChange(Sender: TObject);
begin
  if cxScheduler.ItemIndex <> 0 then
    dtpSchTime.Enabled := True;
  if cxScheduler.ItemIndex <> 1 then
    dtpSchDate.Enabled := False;
  if cxScheduler.ItemIndex <> 2 then
    dtpSchTime.Format  := 'HH.mm.ss';
  case cxScheduler.ItemIndex of
    0: dtpSchTime.Enabled := False;
    1: dtpSchDate.Enabled := True;
    2: dtpSchTime.Format  := 'mm.ss';
  end;
end;

procedure TfrmPropertyGroup.FormCreate(Sender: TObject);
var
  I        : Integer;
  Color    : pointer;
  PathTemp : String;
  Parameters : string;
begin
  TranslateForm(LauncherOptions.LangName);
  PageControl1.ActivePageIndex := 0;
  try
    //Card: General
    edtName.Text       := VNDataGroup.name;
    //Card: Execution
    for I := 0 to High(VNDataGroup.PathExe) do
    begin
      if VNDataGroup.PathExe[I] <> '' then
      begin
        PathTemp := RelativeToAbsolute(VNDataGroup.PathExe[I]);
        Parameters := GetParametersFromPath(PathTemp);
        //Get Path
        if Parameters <> '' then
        begin
          //Delete first "
          Delete(PathTemp,1,1);
          //Delete parameters from Path
          PathTemp := StringReplace(PathTemp,'"' + Parameters,'',[rfIgnoreCase])
        end;
        if ((VNDataGroup.Tipo = 1) or (VNDataGroup.Tipo = 2)) and
           Not((FileExists(PathTemp)) or (DirectoryExists(PathTemp)) or
           (pos('http://',PathTemp) = 1) or (pos('https://',PathTemp) = 1) or
           (pos('ftp://',PathTemp) = 1) or (pos('www.',PathTemp) = 1)) then
          Color := Pointer(clred)
        else
          Color := Pointer(clblack);
        lxPathExe.Items.AddObject(VNDataGroup.PathExe[I],Color);
      end;
    end;
    edtPathIcon.Text   := VNDataGroup.PathIcon;
    cxActionOnExe.ItemIndex := VNDataGroup.ActionOnExe;
    cbDontInsertMRU.Checked := VNDataGroup.DontInsertMRU;
    cbHideSoftware.Checked  := VNDataGroup.HideSoftwareMenu;
    cxAutoExecute.ItemIndex := VNDataGroup.Autorun;
    btnChangeOrder.Enabled := (cxAutoExecute.ItemIndex <> 0);
    //Hotkey
    cbHotKey.Checked   := VNDataGroup.HotKey;
    cbHotKey.Enabled   := LauncherOptions.HotKey;
    cxHotKey1.Enabled  := (cbHotKey.Checked) And (LauncherOptions.HotKey);
    if VNDataGroup.HKModifier <> -1 then
      cxHotKey1.ItemIndex := VNDataGroup.HKModifier
    else
      cxHotKey1.ItemIndex := 0;
    if VNDataGroup.HKCode <> -1 then
      cxHotKey2.ItemIndex := VNDataGroup.HKCode
    else
      cxHotKey2.ItemIndex := 0;
    cxHotKey2.Enabled   := (cbHotKey.Checked) And (LauncherOptions.HotKey);
    if (VNDataGroup.WindowState <> -1) and Not(VNDataGroup.WindowState >= 4) then
      cxWindowState.ItemIndex := VNDataGroup.WindowState
    else
      cxWindowState.ItemIndex := 0;
    //Scheduler
    cxScheduler.ItemIndex  := VNDataGroup.SchMode;
    if VNDataGroup.SchDate <> '' then
      dtpSchDate.Date := StrToDate(VNDataGroup.SchDate)
    else
      dtpSchDate.Date := Date;
    if VNDataGroup.SchTime <> '' then
      dtpSchTime.Time := StrToTime(VNDataGroup.SchTime)
    else
      dtpSchTime.Time := Time;
    cxSchedulerChange(Sender);
  except
    on E : Exception do
      ShowMessageFmt(ArrayMessages[11],[E.ClassName,E.Message]);
  end;
end;

procedure TfrmPropertyGroup.lxPathExeClick(Sender: TObject);
begin
  with lxPathExe do
    if ItemIndex <> -1 then
      edtPathExe.Text := Items[ItemIndex];
end;

procedure TfrmPropertyGroup.lxPathExeDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox do
  begin
    Canvas.FillRect(Rect);
    if odSelected in State then
      Canvas.Font.Color := clWhite
    else
      Canvas.Font.Color := TColor(Items.Objects[Index]);
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

end.
