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

unit Frame.Options.MainWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Frame.BaseEntity, DKLang, Vcl.StdCtrls,
  Vcl.Mask, JvExMask, JvToolEdit;

type
  TfrmMainWindowOptionsPage = class(TfrmBaseEntityPage)
    DKLanguageController1: TDKLanguageController;
    gbTreeView: TGroupBox;
    cbBackground: TCheckBox;
    btnFontSettings: TButton;
    cbAutoOpClCat: TCheckBox;
    cbSmallIcon: TCheckBox;
    edtBackground: TJvFilenameEdit;
    gbWindow: TGroupBox;
    cbWindowOnTop: TCheckBox;
    cbHoldSize: TCheckBox;
    cbHideSearch: TCheckBox;
    edtCustomTitle: TEdit;
    cbCustomTitle: TCheckBox;
    FontDialog1: TFontDialog;
    procedure cbCustomTitleClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure btnFontSettingsClick(Sender: TObject);
    procedure edtBackgroundAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtBackgroundBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
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
  AppConfig.Main;

{$R *.dfm}

{ TfrmBaseEntityPage1 }

procedure TfrmMainWindowOptionsPage.btnFontSettingsClick(Sender: TObject);
begin
  FontDialog1.Execute;
end;

procedure TfrmMainWindowOptionsPage.cbBackgroundClick(Sender: TObject);
begin
  edtBackground.Enabled := cbBackground.Checked;
end;

procedure TfrmMainWindowOptionsPage.cbCustomTitleClick(Sender: TObject);
begin
  edtCustomTitle.Enabled := cbCustomTitle.Checked;
end;

procedure TfrmMainWindowOptionsPage.edtBackgroundAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := Config.Paths.AbsoluteToRelative(AName);
end;

procedure TfrmMainWindowOptionsPage.edtBackgroundBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  edtBackground.Filter := DKLangConstW('msgFilterBackground');
  AName := Config.Paths.RelativeToAbsolute(AName);
end;

function TfrmMainWindowOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('mainwindow');
end;

function TfrmMainWindowOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgMainWindow');
end;

function TfrmMainWindowOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  //Window
  cbHoldSize.Checked         := Config.HoldSize;
  cbWindowOnTop.Checked      := Config.AlwaysOnTop;
  cbHideSearch.Checked       := Config.HideTabSearch;
  cbCustomTitle.Checked      := Config.UseCustomTitle;
  edtCustomTitle.Text        := Config.CustomTitleString;
  edtCustomTitle.Enabled     := Config.UseCustomTitle;
  //Treeview
  cbAutoOpClCat.Checked := Config.TVAutoOpClCats;
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
  Config.CustomTitleString  := edtCustomTitle.Text;
  Config.UseCustomTitle     := cbCustomTitle.Checked;
  //Treeview
  Config.TVAutoOpClCats     := cbAutoOpClCat.Checked;
  Config.TVSmallIconSize    := cbSmallIcon.Checked;
  //Treeview
  Config.TVBackgroundPath   := edtBackground.Text;
  Config.TVBackground       := cbBackground.Checked;
  //Treeview Font
  Config.TVFont             := FontDialog1.Font;
end;

end.
