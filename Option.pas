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
unit Option;

interface

uses
  Windows, SysUtils, Forms, Dialogs, StdCtrls, ComCtrls, ulEnumerations, GTForm,
  Vcl.Controls, System.Classes, Vcl.Graphics, System.UITypes;

type

  { TfrmOption }

  TfrmOption = class(TForm)
    cxLCBottom: TComboBox;
    cxLCLeft: TComboBox;
    cxLCRight: TComboBox;
    cxLCTop: TComboBox;
    cxRCBottom: TComboBox;
    cxRCLeft: TComboBox;
    cxRCRight: TComboBox;
    cxRCTop: TComboBox;
    gbMouse: TGroupBox;
    lbBottom: TLabel;
    lbLeft: TLabel;
    lbLeftClick: TLabel;
    lbRight: TLabel;
    lbRightClick: TLabel;
    lbSide: TLabel;
    lbTop: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    gbTreeView: TGroupBox;
    gbStartup: TGroupBox;
    cbWindowsStartup: TCheckBox;
    cbShowPanelStartup: TCheckBox;
    TabSheet2: TTabSheet;
    gbRecents: TGroupBox;
    lbMaxMRU: TLabel;
    lbNumbMRU: TLabel;
    cbMRU: TCheckBox;
    cbSubMenuMRU: TCheckBox;
    tbMRU: TTrackBar;
    gbBackup: TGroupBox;
    lbMaxBackup: TLabel;
    lbNumbBackup: TLabel;
    cbBackup: TCheckBox;
    tbBackup: TTrackBar;
    gbOtherFunctions: TGroupBox;
    cbCache: TCheckBox;
    TabSheet3: TTabSheet;
    gbExecution: TGroupBox;
    lbActionOnExe: TLabel;
    cbRunSingleClick: TCheckBox;
    cxActionOnExe: TComboBox;
    gbTrayicon: TGroupBox;
    lbTrayLeftClick: TLabel;
    cxLeftClick: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    OpenDialog1: TOpenDialog;
    cbBackground: TCheckBox;
    FontDialog1: TFontDialog;
    btnFontSettings: TButton;
    btnTrayCustomIcon: TButton;
    edtBackground: TEdit;
    btnBrowseBackground: TButton;
    cbTrayicon: TCheckBox;
    cxRightClick: TComboBox;
    lbTrayRightClick: TLabel;
    cbShowMenuStartup: TCheckBox;
    cbAutoOpClCat: TCheckBox;
    gbClearElements: TGroupBox;
    lbClearElements: TLabel;
    btnClearElements: TButton;
    cbAutorun: TCheckBox;
    gbMFU: TGroupBox;
    lbMaxMFU: TLabel;
    lbNumbMFU: TLabel;
    cbMFU: TCheckBox;
    cbSubMenuMFU: TCheckBox;
    tbMFU: TTrackBar;
    cbTrayCustomIcon: TCheckBox;
    btnChangeOrder: TButton;
    lblAutorunOrder: TLabel;
    cbScheduler: TCheckBox;
    gbWindow: TGroupBox;
    lbLanguage: TLabel;
    cxLanguage: TComboBox;
    cxWindowHotKeyCode: TComboBox;
    cxWindowHotKeyMod: TComboBox;
    cbWindowHotKey: TCheckBox;
    cbWindowOnTop: TCheckBox;
    cbFormCard: TCheckBox;
    cbHoldSize: TCheckBox;
    cbCustomTitle: TCheckBox;
    edtCustomTitle: TEdit;
    cbHideSearch: TCheckBox;
    cbMenuHotKey: TCheckBox;
    cxMenuHotKeyCode: TComboBox;
    cxMenuHotKeyMod: TComboBox;
    cbMenuFade: TCheckBox;
    cxTheme: TComboBox;
    lbMenuTheme: TLabel;
    cbClassicMenu: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tbMRUChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbBackupChange(Sender: TObject);
    procedure Browse(Sender: TObject);
    procedure btnFontSettingsClick(Sender: TObject);
    procedure cbTrayiconClick(Sender: TObject);
    procedure cbCustomTitleClick(Sender: TObject);
    procedure btnClearElementsClick(Sender: TObject);
    procedure tbMFUChange(Sender: TObject);
    procedure btnChangeOrderClick(Sender: TObject);
    procedure cbWindowHotKeyClick(Sender: TObject);
    procedure cbMenuHotKeyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOption: TfrmOption;

  arrayOfMouseSensorsString:array[0..3] of string = (
     'Disabled','Show window','Show default menu','Show classic menu'
  );

implementation

uses
  ClearElements, AppConfig, ulSysUtils, ulAppConfig, udClassicMenu, ulTreeView,
  Main, OrderSoftware, gnugettext;

{$R *.dfm}

procedure TfrmOption.Browse(Sender: TObject);  
var
  PathTemp : String;
begin
  if (Sender = btnBrowseBackground) then
  begin
    OpenDialog1.Filter     := 'Files supported (*.png;*.bmp)|*.png;*.bmp|All files|*.*';
    OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtBackground.Text));
  end;  
  if (Sender = btnTrayCustomIcon) then
  begin
    OpenDialog1.Filter     := 'Files supported (*.ico)|*.ico';
    OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(Config.TrayCustomIconPath));
  end;
  if (OpenDialog1.Execute) then
  begin
    PathTemp := AbsoluteToRelative(OpenDialog1.FileName);
    if (Sender = btnBrowseBackground) then
    begin
      Config.TVBackgroundPath := PathTemp;
      edtBackground.Text      := PathTemp;
    end;
    if (Sender = btnTrayCustomIcon) then
    begin
      Config.TrayCustomIconPath := PathTemp;
      cbTrayCustomIcon.Checked  := True;
    end;
  end;
  SetCurrentDir(SUITE_WORKING_PATH);
end;

procedure TfrmOption.btnCancelClick(Sender: TObject);
begin
  close;
end;

procedure TfrmOption.btnChangeOrderClick(Sender: TObject);
begin
  //Call Autorun order form
  try
    Application.CreateForm(TfrmOrderSoftware, frmOrderSoftware);
    frmOrderSoftware.AutorunType := atAlwaysOnStart;
    frmOrderSoftware.FormStyle   := Self.FormStyle;
    frmOrderSoftware.showmodal;
  finally
    frmOrderSoftware.Free;
  end;
end;

procedure TfrmOption.btnOkClick(Sender: TObject);
begin
  //General
  Config.TVAutoOpClCats     := cbAutoOpClCat.Checked;
  Config.StartWithWindows   := cbWindowsStartup.Checked;
  Config.ShowPanelAtStartUp := cbShowPanelStartup.Checked;
  Config.ShowMenuAtStartUp  := cbShowMenuStartup.Checked;
  Config.HoldSize           := cbHoldSize.Checked;
  Config.AlwaysOnTop        := cbWindowOnTop.Checked;
  Config.CustomTitleString  := edtCustomTitle.Text;
  Config.UseCustomTitle     := cbCustomTitle.Checked;
  { TODO -oMatteo -c : Language code 29/11/2009 21:39:41 }
//  Config.LangName         := cxLanguage.Items[cxLanguage.ItemIndex];
  Config.HideTabSearch    := cbHideSearch.Checked;
  //Treeview
  Config.TVFont           := FontDialog1.Font;
  Config.TVBackgroundPath := edtBackground.Text;
  Config.TVBackground     := cbBackground.Checked;
  //MRU
  Config.MRU        := cbMRU.Checked;
  Config.SubMenuMRU := cbSubMenuMRU.Checked;
  Config.MRUNumber  := tbMRU.Position;
  //MFU
  Config.MFU        := cbMFU.Checked;
  Config.SubMenuMFU := cbSubMenuMFU.Checked;
  Config.MFUNumber  := tbMFU.Position;
  //Backup
  Config.Backup     := cbBackup.Checked;
  //Delete backup useless
  if tbBackup.Position < Config.BackupNumber then
    DeleteOldBackups(tbBackup.Position);
  Config.BackupNumber   := tbBackup.Position;
  //Various
  Config.ActionOnExe    := TActionOnExecute(cxActionOnExe.ItemIndex);
  Config.RunSingleClick := cbRunSingleClick.Checked;
  Config.Autorun        := cbAutorun.Checked;
  Config.Cache          := cbCache.Checked;
  Config.Scheduler      := cbScheduler.Checked;
  //Trayicon
  Config.TrayUseCustomIcon := cbTrayCustomIcon.Checked;
  Config.TrayIcon          := cbTrayicon.Checked;
  Config.ActionClickLeft   := cxLeftClick.ItemIndex;
  Config.ActionClickRight  := cxRightClick.ItemIndex;
  Config.UseClassicMenu    := cbClassicMenu.Checked;
  Config.GraphicMenuTheme  := cxTheme.Items[cxTheme.ItemIndex];
  Config.GraphicMenuFade   := cbMenuFade.Checked;
  //Hot Keys
  Config.WindowHotKey      := cbWindowHotKey.Checked;
  Config.WindowHotKeyCode  := cxWindowHotKeyCode.ItemIndex;
  Config.WindowHotKeyMod   := cxWindowHotKeyMod.ItemIndex;
  Config.MenuHotKey        := cbMenuHotKey.Checked;
  Config.MenuHotKeyCode    := cxMenuHotKeyCode.ItemIndex;
  Config.MenuHotKeyMod     := cxMenuHotKeyMod.ItemIndex;
  //Mouse Sensors
  //Left
  Config.SensorLeftClick[0]  := cxLCTop.ItemIndex;
  Config.SensorLeftClick[1]  := cxLCLeft.ItemIndex;
  Config.SensorLeftClick[2]  := cxLCRight.ItemIndex;
  Config.SensorLeftClick[3]  := cxLCBottom.ItemIndex;
  //Right
  Config.SensorRightClick[0] := cxRCTop.ItemIndex;
  Config.SensorRightClick[1] := cxRCLeft.ItemIndex;
  Config.SensorRightClick[2] := cxRCRight.ItemIndex;
  Config.SensorRightClick[3] := cxRCBottom.ItemIndex;
  //Config changed and if frmMain is visible, focus vstList (repaint)
  Config.Changed := True;
  //Register HotKeys
  Config.RegisterHotKeys;
  //Update sensors
  Config.UpdateSensors;
  if frmMain.Visible then
    frmMain.FocusControl(frmMain.vstList);
  Close;
end;

procedure TfrmOption.btnFontSettingsClick(Sender: TObject);
begin
  FontDialog1.Execute;
end;

procedure TfrmOption.cbCustomTitleClick(Sender: TObject);
begin
  edtCustomTitle.Enabled := cbCustomTitle.Checked;
end;

procedure TfrmOption.cbMenuHotKeyClick(Sender: TObject);
begin
  cxMenuHotKeyMod.Enabled  := cbMenuHotKey.Checked;
  cxMenuHotKeyCode.Enabled := cbMenuHotKey.Checked;
end;

procedure TfrmOption.cbTrayiconClick(Sender: TObject);
begin
  cbTrayCustomIcon.Enabled  := cbTrayicon.Checked;
  btnTrayCustomIcon.Enabled := cbTrayicon.Checked;
  cxLeftClick.Enabled       := cbTrayicon.Checked;
  cxRightClick.Enabled      := cbTrayicon.Checked;
end;

procedure TfrmOption.cbWindowHotKeyClick(Sender: TObject);
begin
  cxWindowHotKeyMod.Enabled  := cbWindowHotKey.Checked;
  cxWindowHotKeyCode.Enabled := cbWindowHotKey.Checked;
end;

procedure TfrmOption.btnClearElementsClick(Sender: TObject);
begin
  try
    Application.CreateForm(TfrmClearElements, frmClearElements);
    frmClearElements.FormStyle := Self.FormStyle;
    frmClearElements.showmodal;
  finally
    frmClearElements.Free;
  end;
end;

procedure TfrmOption.FormCreate(Sender: TObject);
var
  I: integer;
  searchResult : TSearchRec;
begin
  TranslateComponent(self);
  PageControl1.TabIndex      := 0;
  //General
  cbAutoOpClCat.Checked      := Config.TVAutoOpClCats;
  cbWindowsStartup.Checked   := Config.StartWithWindows;
  cbShowPanelStartup.Checked := Config.ShowPanelAtStartUp;
  cbShowMenuStartup.Checked  := Config.ShowMenuAtStartUp;
  cbHoldSize.Checked         := Config.HoldSize;
  cbWindowOnTop.Checked      := Config.AlwaysOnTop;
  cbCustomTitle.Checked      := Config.UseCustomTitle;
  edtCustomTitle.Text        := Config.CustomTitleString;
  edtCustomTitle.Enabled     := Config.UseCustomTitle;
  //Languages
  { TODO -oMatteo -c : Language code 29/11/2009 21:39:24 }
//  if FindFirst(ApplicationPath + 'Lang\*.xml', faAnyFile, searchResult) = 0 then
//  begin
//    repeat
//      cxLanguage.AddItem(SearchResult.Name,sender);
//    until FindNext(searchResult) <> 0;
//    FindClose(searchResult);
//  end;
//  if FindFirst(ApplicationPath + '*.xml', faAnyFile, searchResult) = 0 then
//  begin
//    repeat
//      if (SearchResult.Name <> LauncherFileNameXML) and ((SearchResult.Name <> ExtractFileName(ConfigSqlTempFile))) then
//        cxLanguage.AddItem(SearchResult.Name,sender);
//    until FindNext(searchResult) <> 0;
//    FindClose(searchResult);
//  end;
//  cxLanguage.ItemIndex   := cxLanguage.Items.IndexOf(Config.LangName);
  //Hide tabs
  cbHideSearch.Checked   := Config.HideTabSearch;
  //Treeview Font
  with FontDialog1.Font do
  begin
    Name  := Config.TVFont.Name;
    Style := Config.TVFont.Style;
    Size  := Config.TVFont.Size;
    Color := Config.TVFont.Color;
  end;
  cbBackground.Checked := Config.TVBackground;
  edtBackground.Text   := Config.TVBackgroundPath;
  //MRU
  cbMRU.Checked        := Config.MRU;
  cbSubMenuMRU.Checked := Config.SubMenuMRU;
  tbMRU.position       := Config.MRUNumber;
  lbNumbMRU.Caption    := IntToStr(Config.MRUNumber);
  //MFU
  cbMFU.Checked        := Config.MFU;
  cbSubMenuMFU.Checked := Config.SubMenuMFU;
  tbMFU.position       := Config.MFUNumber;
  lbNumbMFU.Caption    := IntToStr(Config.MFUNumber);
  //Backup
  cbBackup.Checked     := Config.Backup;
  tbBackup.position    := Config.BackupNumber;
  lbNumbBackup.Caption := IntToStr(Config.BackupNumber);
  //Various
  cxActionOnExe.ItemIndex   := Ord(Config.ActionOnExe);
  cbRunSingleClick.Checked  := Config.RunSingleClick;
  cbAutorun.Checked         := Config.Autorun;
  cbCache.Checked           := Config.Cache;
  cbScheduler.Checked       := Config.Scheduler;
  //Trayicon
  cbTrayicon.Checked        := Config.TrayIcon;
  cbTrayCustomIcon.Checked  := Config.TrayUseCustomIcon;
  cxLeftClick.ItemIndex     := Config.ActionClickLeft;
  cxRightClick.ItemIndex    := Config.ActionClickRight;
  cxLeftClick.Enabled       := cbTrayicon.Checked;
  cxRightClick.Enabled      := cbTrayicon.Checked;
  cbTrayCustomIcon.Enabled  := cbTrayicon.Checked;
  btnTrayCustomIcon.Enabled := cbTrayicon.Checked;
  cbClassicMenu.Checked     := Config.UseClassicMenu;
  //Only default menu
  if FindFirst(SUITE_MENUTHEMES_PATH + '*.*', faDirectory, searchResult) = 0 then
  begin
    repeat
      if ((searchResult.Name <> '.') and (searchResult.Name <> '..')) and
         ((searchResult.Attr and faDirectory) = (faDirectory)) then
        cxTheme.AddItem(SearchResult.Name,sender);
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;
  cxTheme.ItemIndex         := cxTheme.Items.IndexOf(Config.GraphicMenuTheme);
  cbMenuFade.Checked        := Config.GraphicMenuFade;

  //Hot Keys
  //TODO -.... SOLO PER FARLO ANDARE
  Config.HotKey := True;
  //Window's Hotkey
  cbWindowHotKey.Checked       := Config.WindowHotKey;
  cbWindowHotKey.Enabled       := Config.HotKey;
  cxWindowHotKeyCode.Enabled   := (cbWindowHotKey.Checked) And (Config.HotKey);
  cxWindowHotKeyMod.Enabled    := (cbWindowHotKey.Checked) And (Config.HotKey);
//  cxWindowHotKeyCode.ItemIndex := Config.WindowHotKeyCode;
//  cxWindowHotKeyMod.ItemIndex  := Config.WindowHotKeyMod;
  cbMenuHotKey.Checked         := Config.MenuHotKey;
  cxMenuHotKeyCode.ItemIndex   := Config.MenuHotKeyCode;
  cxMenuHotKeyMod.ItemIndex    := Config.MenuHotKeyMod;
  if Config.WindowHotKeyMod <> -1 then
    cxWindowHotKeyMod.ItemIndex := Config.WindowHotKeyMod
  else
    cxWindowHotKeyMod.ItemIndex := 0;
  if Config.WindowHotKeyCode <> -1 then
    cxWindowHotKeyCode.ItemIndex := Config.WindowHotKeyCode
  else
    cxWindowHotKeyCode.ItemIndex := 0;

   //Window's menu Hotkey
  cbMenuHotKey.Checked     := Config.MenuHotkey;
  cbMenuHotKey.Enabled     := Config.HotKey;
  cxMenuHotKeyMod.Enabled  := (cbMenuHotKey.Checked) And (Config.HotKey);
  cxMenuHotKeyCode.Enabled := (cbMenuHotKey.Checked) And (Config.HotKey);
  if Config.MenuHotKeyMod <> -1 then
    cxMenuHotKeyMod.ItemIndex := Config.MenuHotKeyMod
  else
    cxMenuHotKeyMod.ItemIndex := 0;
  if Config.MenuHotKeyCode <> -1 then
    cxMenuHotKeyCode.ItemIndex := Config.MenuHotKeyCode
  else
    cxMenuHotKeyCode.ItemIndex := 0;








  //Mouse Sensors
  for I := 0 to 3 do
    begin
      //Left Click
      cxLCTop.Items.Add(arrayOfMouseSensorsString[i]);
      cxLCLeft.Items.Add(arrayOfMouseSensorsString[i]);
      cxLCRight.Items.Add(arrayOfMouseSensorsString[i]);
      cxLCBottom.Items.Add(arrayOfMouseSensorsString[i]);
      //Right Click
      cxRCTop.Items.Add(arrayOfMouseSensorsString[i]);
      cxRCLeft.Items.Add(arrayOfMouseSensorsString[i]);
      cxRCRight.Items.Add(arrayOfMouseSensorsString[i]);
      cxRCBottom.Items.Add(arrayOfMouseSensorsString[i]);
    end;
   //Left Click
   cxLCTop.ItemIndex    := Config.SensorLeftClick[0];
   cxLCLeft.ItemIndex   := Config.SensorLeftClick[1];
   cxLCRight.ItemIndex  := Config.SensorLeftClick[2];
   cxLCBottom.ItemIndex := Config.SensorLeftClick[3];
   //Right Click
   cxRCTop.ItemIndex    := Config.SensorRightClick[0];
   cxRCLeft.ItemIndex   := Config.SensorRightClick[1];
   cxRCRight.ItemIndex  := Config.SensorRightClick[2];
   cxRCBottom.ItemIndex := Config.SensorRightClick[3];
end;

procedure TfrmOption.tbBackupChange(Sender: TObject);
begin
  lbNumbBackup.Caption := IntToStr(tbBackup.position);
end;

procedure TfrmOption.tbMFUChange(Sender: TObject);
begin
  lbNumbMFU.Caption := IntToStr(tbMFU.position);
end;

procedure TfrmOption.tbMRUChange(Sender: TObject);
begin
  lbNumbMRU.Caption := IntToStr(tbMRU.position);
end;

end.
