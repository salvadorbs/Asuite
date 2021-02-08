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

unit Frame.Options.Trayicon;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Forms, Dialogs, Frame.BaseEntity, StdCtrls,
  EditBtn, DefaultTranslator, ExtCtrls;

type

  { TfrmTrayiconOptionsPage }

  TfrmTrayiconOptionsPage = class(TfrmBaseEntityPage)
    cxLeftClick: TComboBox;
    cxMiddleClick: TComboBox;
    cxRightClick: TComboBox;
    cxTheme: TComboBox;
    
    gbTrayicon: TGroupBox;
    lblMiddleClick: TLabel;
    lbMenuTheme: TLabel;
    lbTrayLeftClick: TLabel;
    cbTrayicon: TCheckBox;
    cbTrayCustomIcon: TCheckBox;
    edtCustomIcon: TFileNameEdit;
    grpClassicMenu: TGroupBox;
    cbSubMenuMFU: TCheckBox;
    cbSubMenuMRU: TCheckBox;
    chkAutoExpansion: TCheckBox;
    grpGraphicMenu: TGroupBox;
    cbMenuFade: TCheckBox;
    cbSmallIcon: TCheckBox;
    chkAutomaticHideMenu: TCheckBox;
    chkUserPicture: TCheckBox;
    lbTrayRightClick: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure cbTrayiconClick(Sender: TObject);
    procedure cbTrayCustomIconClick(Sender: TObject);
    procedure edtCustomIconAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtCustomIconBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
  private
    { Private declarations }
    procedure LoadComboMouseClickItems(AComboBox: TComboBox);
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmTrayiconOptionsPage: TfrmTrayiconOptionsPage;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Kernel.ResourceStrings;

procedure TfrmTrayiconOptionsPage.LoadComboMouseClickItems(AComboBox: TComboBox);
begin
  AComboBox.Items.Add(cxMouseClick_item0);
  AComboBox.Items.Add(cxMouseClick_item1);
  AComboBox.Items.Add(cxMouseClick_item2);
  AComboBox.Items.Add(cxMouseClick_item3);
end;

{$R *.lfm}

{ TfrmTrayiconOptionsPage }

procedure TfrmTrayiconOptionsPage.cbTrayCustomIconClick(Sender: TObject);
begin
  edtCustomIcon.Enabled := cbTrayCustomIcon.Checked;
end;

procedure TfrmTrayiconOptionsPage.cbTrayiconClick(Sender: TObject);
begin
  cbTrayCustomIcon.Enabled := cbTrayicon.Checked;
  cxLeftClick.Enabled      := cbTrayicon.Checked;
  cxRightClick.Enabled     := cbTrayicon.Checked;
end;

procedure TfrmTrayiconOptionsPage.edtCustomIconAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := Config.Paths.AbsoluteToRelative(AName);
end;

procedure TfrmTrayiconOptionsPage.edtCustomIconBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := Config.Paths.RelativeToAbsolute(AName);
  edtCustomIcon.Filter := msgFilterIcon;
end;

function TfrmTrayiconOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('trayicon');
end;

function TfrmTrayiconOptionsPage.GetTitle: string;
begin
  Result := msgTrayIcon;
end;

function TfrmTrayiconOptionsPage.InternalLoadData: Boolean;
var
  searchResult : TSearchRec;
begin
  Result := inherited;

  LoadComboMouseClickItems(cxLeftClick);
  LoadComboMouseClickItems(cxMiddleClick);
  LoadComboMouseClickItems(cxRightClick);

  //Trayicon
  cbTrayicon.Checked        := Config.TrayIcon;
  cbTrayCustomIcon.Checked  := Config.TrayUseCustomIcon;
  edtCustomIcon.Text        := Config.TrayCustomIconPath;
  cxLeftClick.ItemIndex     := Ord(Config.ActionClickLeft);
  cxMiddleClick.ItemIndex   := Ord(Config.ActionClickMiddle);
  cxRightClick.ItemIndex    := Ord(Config.ActionClickRight);

  //Graphic Menu
  cbMenuFade.Checked  := Config.GMFade;
  cbSmallIcon.Checked := Config.GMSmallIconSize;
  chkAutomaticHideMenu.Checked := Config.GMAutomaticHideMenu;
  chkUserPicture.Checked := Config.GMShowUserPicture;

  //Get GM theme list
  if FindFirst(Config.Paths.SuitePathMenuThemes + '*.*', faDirectory, searchResult) = 0 then
  begin
    repeat
      if ((searchResult.Name <> '.') and (searchResult.Name <> '..')) and
         ((searchResult.Attr and faDirectory) = (faDirectory)) then
        cxTheme.AddItem(SearchResult.Name,Self);
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;
  cxTheme.ItemIndex  := cxTheme.Items.IndexOf(Config.GMTheme);

  //ClassicMenu
  cbSubMenuMRU.Checked := Config.SubMenuMRU;
  cbSubMenuMFU.Checked := Config.SubMenuMFU;
  chkAutoExpansion.Checked := Config.AutoExpansionFolder;

  //Enable/disable visual components
  cbTrayiconClick(Self);
  cbTrayCustomIconClick(Self);
end;

function TfrmTrayiconOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  //Trayicon
  Config.TrayIcon           := cbTrayicon.Checked;
  Config.TrayCustomIconPath := edtCustomIcon.Text;
  Config.TrayUseCustomIcon  := cbTrayCustomIcon.Checked;
  Config.ActionClickLeft    := TTrayiconActionClick(cxLeftClick.ItemIndex);
  Config.ActionClickMiddle  := TTrayiconActionClick(cxMiddleClick.ItemIndex);
  Config.ActionClickRight   := TTrayiconActionClick(cxRightClick.ItemIndex);
  //Graphic Menu
  if cxTheme.ItemIndex <> -1 then
    Config.GMTheme         := cxTheme.Items[cxTheme.ItemIndex];
  Config.GMFade          := cbMenuFade.Checked;
  Config.GMSmallIconSize := cbSmallIcon.Checked;
  Config.GMAutomaticHideMenu := chkAutomaticHideMenu.Checked;
  Config.GMShowUserPicture := chkUserPicture.Checked;
  //Submenu
  Config.AutoExpansionFolder := chkAutoExpansion.Checked;
  Config.SubMenuMRU := cbSubMenuMRU.Checked;
  Config.SubMenuMFU := cbSubMenuMFU.Checked;
end;

end.
