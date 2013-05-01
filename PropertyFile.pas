{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

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

unit PropertyFile;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, GTForm,
  ulNodeDataTypes;

type

  { TfrmPropertyFile }

  TfrmPropertyFile = class(TGTForm)
    btnOk: TButton;
    btnCancel: TButton;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    tsInfo1: TTabSheet;
    lbInfo1: TLabel;
    lbName: TLabel;
    edtName: TEdit;
    tsInfo2: TTabSheet;
    lbPathIcon: TLabel;
    lbAutoExecute: TLabel;
    lbWindowState: TLabel;
    lbWorkingDir: TLabel;
    edtPathIcon: TEdit;
    btnBrowseIcon: TButton;
    cxAutoExecute: TComboBox;
    cxWindowState: TComboBox;
    edtWorkingDir: TEdit;
    btnBrowseWorkingDir: TButton;
    lbPathExe: TLabel;
    edtPathExe: TEdit;
    edtParameters: TEdit;
    lbParameters: TLabel;
    btnBrowseExe: TButton;
    lbInfo2: TLabel;
    lbActionOnExe: TLabel;
    cxActionOnExe: TComboBox;
    cbDontInsertMRU: TCheckBox;
    cbShortcutDesktop: TCheckBox;
    cbHideSoftware: TCheckBox;
    btnChangeOrder: TButton;
    cbDontInsertMFU: TCheckBox;
    procedure Browse(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edtNameEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtPathExeExit(Sender: TObject);
    procedure btnChangeOrderClick(Sender: TObject);
    procedure cxAutoExecuteChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FNodeData: PBaseData;
    procedure LoadNodeData(AData: TvFileNodeData);
    procedure SaveNodeData(AData: TvFileNodeData);
  public
    { Public declarations }
    class function Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
  end;

var
  frmPropertyFile :   TfrmPropertyFile;

implementation

{$R *.dfm}

uses
  AppConfig, ulEnumerations, udImages, ulSysUtils, OrderSoftware, Main,
  ulCommonUtils;

class function TfrmPropertyFile.Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
begin
  Result := mrNone;
  if not Assigned(NodeData) then
    ShowMessage(msgErrGeneric, true)
  else
    with TfrmPropertyFile.Create(AOwner) do
      try
        FNodeData := NodeData;
        LoadNodeData(TvFileNodeData(NodeData.Data));
        FormStyle := frmMain.FormStyle;
        ShowModal;
        if ModalResult = mrOK then
          SaveNodeData(TvFileNodeData(NodeData.Data));
        Result := ModalResult;
      finally
        Free;
      end;
end;

procedure TfrmPropertyFile.LoadNodeData(AData: TvFileNodeData);
begin
  edtName.Text              := AData.name;
  edtPathExe.Text           := AData.PathExe;
  if not(FileFolderPageWebExists(AData.PathAbsoluteExe)) then
  begin
    edtPathExe.Font.Color  := clRed;
    edtPathExe.Hint        := msgFileNotFound;
  end;
  edtParameters.Text        := AData.Parameters;
  edtWorkingDir.Text        := AData.WorkingDir;
  edtPathIcon.Text          := AData.PathIcon;
  cxActionOnExe.ItemIndex   := Ord(AData.ActionOnExe);
  cxAutoExecute.ItemIndex   := Ord(AData.Autorun);
  btnChangeOrder.Enabled    := (cxAutoExecute.ItemIndex <> 0);
  //Window State
  if (AData.WindowState <> -1) and Not(AData.WindowState >= 4) then
    cxWindowState.ItemIndex := AData.WindowState
  else
    cxWindowState.ItemIndex := 0;
  //Others
  cbDontInsertMRU.Checked   := AData.NoMRU;
  cbDontInsertMFU.Checked   := AData.NoMFU;
  cbShortcutDesktop.Checked := AData.ShortcutDesktop;
  cbHideSoftware.Checked    := AData.HideFromMenu;
end;

procedure TfrmPropertyFile.SaveNodeData(AData: TvFileNodeData);
begin
  if (edtPathExe.Text = '') then
    edtPathExe.Text := CONST_PATH_DRIVE;
  AData.Name        := StringReplace(edtName.Text, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
  AData.Name        := StringReplace(AData.Name, '&', '&&', [rfIgnoreCase,rfReplaceAll]);
  AData.PathExe     := edtPathExe.Text;
  AData.Parameters  := edtParameters.Text;
  //Advanced
  AData.WorkingDir  := edtWorkingDir.Text;
  AData.ActionOnExe := TActionOnExecute(cxActionOnExe.ItemIndex);
  AData.Autorun     := TAutorunType(cxAutoExecute.ItemIndex);
  //Others
  AData.NoMRU        := cbDontInsertMRU.Checked;
  AData.NoMFU        := cbDontInsertMFU.Checked;
  AData.HideFromMenu := cbHideSoftware.Checked;
  AData.WindowState  := cxWindowState.ItemIndex;
  AData.ShortcutDesktop := cbShortcutDesktop.Checked;
  //If changed, refresh cache icon
  if AData.PathIcon <> edtPathIcon.Text then
  begin
    AData.PathIcon   := edtPathIcon.Text;
    ImagesDM.DeleteCacheIcon(AData);
    AData.ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(AData));
  end;
  AData.Changed    := true;
end;

procedure TfrmPropertyFile.Browse(Sender: TObject);
var
  PathTemp : String;
begin
  //Set OpenDialog1's filter based of button pressed
  if (sender = btnBrowseExe) then
  begin
    OpenDialog1.Filter     := 'Executables (*.exe)|*.exe|All files|*.*';
    OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtPathExe.Text));
  end
  else
    if (sender = btnBrowseIcon) then
    begin
      OpenDialog1.Filter     := 'Files supported (*.ico;*.exe)|*.ico;*.exe|All files|*.*';
      OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtPathIcon.Text));
    end;
  //Show OpenDialog and get path
  if (sender = btnBrowseExe) or (sender = btnBrowseIcon) then
  begin
    if (OpenDialog1.Execute) then
    begin
      PathTemp := AbsoluteToRelative(OpenDialog1.FileName);
      { TODO -oMatteo -c : Get automatically its name from exe (like PStart) 26/07/2010 22:02:57 }
      if (sender = btnBrowseExe) then
      begin
        edtPathExe.text := PathTemp;
        edtPathExe.Font.Color := clWindowText;
        edtPathExe.Hint := '';
      end;
      if (sender = btnBrowseIcon) then
        edtPathIcon.text := PathTemp;
    end;
  end
  else
    if sender = btnBrowseWorkingDir then
    begin
      PathTemp := BrowseForFolder('', RelativeToAbsolute(edtWorkingDir.Text));
      if (PathTemp <> '') then
        edtWorkingDir.Text := AbsoluteToRelative(PathTemp);
    end;
  SetCurrentDir(SUITE_WORKING_PATH);
end;

procedure TfrmPropertyFile.btnCancelClick(Sender: TObject);
begin
  close;
end;

procedure TfrmPropertyFile.btnChangeOrderClick(Sender: TObject);
begin
  //Autorun
  with TvFileNodeData(FNodeData.Data) do
  begin
    Autorun := TAutorunType(cxAutoExecute.ItemIndex);
    try
      Application.CreateForm(TfrmOrderSoftware, frmOrderSoftware);
      frmOrderSoftware.FormStyle   := Self.FormStyle;
      frmOrderSoftware.AutorunType := Autorun;
      frmOrderSoftware.ShowModal;
    finally
      frmOrderSoftware.Free;
    end;
  end;
end;

procedure TfrmPropertyFile.btnOkClick(Sender: TObject);
begin
  CheckPropertyName(edtName);
end;

procedure TfrmPropertyFile.edtNameEnter(Sender: TObject);
begin
  TEdit(Sender).Color := clWindow;
end;

procedure TfrmPropertyFile.cxAutoExecuteChange(Sender: TObject);
begin
  btnChangeOrder.Enabled := (cxAutoExecute.ItemIndex <> 0);
end;

procedure TfrmPropertyFile.edtPathExeExit(Sender: TObject);
var
  PathTemp : String;
begin
  PathTemp := RelativeToAbsolute(edtPathExe.Text);
  if Not(FileFolderPageWebExists(PathTemp)) then
  begin
    //File not found - Change font color with red
    edtPathExe.Font.Color  := clRed;
    edtPathExe.Hint        := msgFileNotFound;
  end
  else begin
    //File found - Change font color with black
    edtPathExe.Font.Color  := clBlack;
    edtPathExe.Hint        := '';
  end;
end;

procedure TfrmPropertyFile.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := ((ModalResult = mrOK) and (Trim(edtName.Text) <> ''))
           or (ModalResult = mrCancel);
end;

procedure TfrmPropertyFile.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

end.
