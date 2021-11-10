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

unit Frame.Options.MainWindow;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Forms, Dialogs, Frame.BaseEntity, StdCtrls,
  EditBtn;

type

  { TfrmMainWindowOptionsPage }

  TfrmMainWindowOptionsPage = class(TfrmBaseEntityPage)
    btnFontSettings: TButton;
    cbAutoOpClCat: TCheckBox;
    cbAutoOpDragDrop: TCheckBox;
    cbDialogCenterMF: TCheckBox;
    cbDisableConfirmDelete: TCheckBox;
    cbSmallIcon: TCheckBox;
    gbTreeView: TGroupBox;
    cbBackground: TCheckBox;
    edtBackground: TFileNameEdit;
    gbWindow: TGroupBox;
    cbWindowOnTop: TCheckBox;
    cbHoldSize: TCheckBox;
    cbHideSearch: TCheckBox;
    edtCustomTitle: TEdit;
    cbCustomTitle: TCheckBox;
    FontDialog1: TFontDialog;
    chkSearchAsYouType: TCheckBox;
    procedure cbCustomTitleClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure btnFontSettingsClick(Sender: TObject);
    procedure edtBackgroundAcceptFileName(Sender: TObject; var Value: String);
    procedure edtBackgroundButtonClick(Sender: TObject);
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
  frmMainWindowOptionsPage: TfrmMainWindowOptionsPage;

implementation

uses
  AppConfig.Main, Kernel.ResourceStrings, Kernel.Instance, Kernel.Manager;

{$R *.lfm}

{ TfrmBaseEntityPage1 }

procedure TfrmMainWindowOptionsPage.btnFontSettingsClick(Sender: TObject);
begin
  FontDialog1.Execute;
end;

procedure TfrmMainWindowOptionsPage.edtBackgroundAcceptFileName(
  Sender: TObject; var Value: String);
begin
  Value := ASuiteInstance.Paths.AbsoluteToRelative(Value);
end;

procedure TfrmMainWindowOptionsPage.edtBackgroundButtonClick(Sender: TObject);
begin
  if edtBackground.Text <> '' then
    edtBackground.InitialDir := ExtractFileDir(ASuiteInstance.Paths.RelativeToAbsolute(edtBackground.Text));
end;

procedure TfrmMainWindowOptionsPage.cbBackgroundClick(Sender: TObject);
begin
  edtBackground.Enabled := cbBackground.Checked;
end;

procedure TfrmMainWindowOptionsPage.cbCustomTitleClick(Sender: TObject);
begin
  edtCustomTitle.Enabled := cbCustomTitle.Checked;
end;

function TfrmMainWindowOptionsPage.GetImageIndex: Integer;
begin
  Result := ASuiteManager.IconsManager.GetIconIndex('mainwindow');
end;

function TfrmMainWindowOptionsPage.GetTitle: string;
begin
  Result := msgMainWindow;
end;

function TfrmMainWindowOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;

  edtBackground.Filter := msgFilterBackground;

  //Window
  cbHoldSize.Checked         := Config.HoldSize;
  cbWindowOnTop.Checked      := Config.AlwaysOnTop;
  cbHideSearch.Checked       := Config.HideTabSearch;
  chkSearchAsYouType.Checked := Config.SearchAsYouType;
  cbCustomTitle.Checked      := Config.UseCustomTitle;
  edtCustomTitle.Text        := Config.CustomTitleString;
  edtCustomTitle.Enabled     := Config.UseCustomTitle;
  cbDialogCenterMF.Checked   := Config.DialogCenterMF;

  //Treeview
  cbAutoOpClCat.Checked := Config.TVAutoOpClCats;
  cbAutoOpDragDrop.Checked := Config.TVAutoOpCatsDrag;
  cbDisableConfirmDelete.Checked := Config.TVDisableConfirmDelete;
  cbSmallIcon.Checked   := Config.TVSmallIconSize;
  cbBackground.Checked  := Config.TVBackground;
  edtBackground.Text    := Config.TVBackgroundPath;

  //Treeview Font
  with FontDialog1.Font do
  begin
    Name  := Config.TVFont.Name;
    Style := Config.TVFont.Style;
    Size  := Config.TVFont.Size;
    Color := Config.TVFont.Color;
  end;

  //Update controls
  cbCustomTitleClick(Self);
  cbBackgroundClick(Self);
end;

function TfrmMainWindowOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;

  //Window
  Config.HoldSize           := cbHoldSize.Checked;
  Config.AlwaysOnTop        := cbWindowOnTop.Checked;
  Config.HideTabSearch      := cbHideSearch.Checked;
  Config.SearchAsYouType    := chkSearchAsYouType.Checked;
  Config.CustomTitleString  := edtCustomTitle.Text;
  Config.UseCustomTitle     := cbCustomTitle.Checked;
  Config.DialogCenterMF     := cbDialogCenterMF.checked;

  //Treeview
  Config.TVAutoOpClCats     := cbAutoOpClCat.Checked;
  Config.TVAutoOpCatsDrag   := cbAutoOpDragDrop.Checked;
  Config.TVDisableConfirmDelete := cbDisableConfirmDelete.Checked;
  Config.TVSmallIconSize    := cbSmallIcon.Checked;

  //Treeview
  Config.TVBackgroundPath   := edtBackground.Text;
  Config.TVBackground       := cbBackground.Checked;

  //Treeview Font
  Config.TVFont             := FontDialog1.Font;
end;

end.
