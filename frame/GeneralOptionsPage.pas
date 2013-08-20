unit GeneralOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.StdCtrls, DKLang;

type
  TfrmGeneralOptionsPage = class(TfrmBaseEntityPage)
    gbWindow: TGroupBox;
    cbWindowOnTop: TCheckBox;
    cbHoldSize: TCheckBox;
    cbCustomTitle: TCheckBox;
    edtCustomTitle: TEdit;
    cbHideSearch: TCheckBox;
    gbStartup: TGroupBox;
    cbWindowsStartup: TCheckBox;
    cbShowPanelStartup: TCheckBox;
    cbShowMenuStartup: TCheckBox;
    gbTreeView: TGroupBox;
    cbBackground: TCheckBox;
    btnFontSettings: TButton;
    edtBackground: TEdit;
    btnBrowseBackground: TButton;
    cbAutoOpClCat: TCheckBox;
    grpLanguage: TGroupBox;
    cxLanguage: TComboBox;
    FontDialog1: TFontDialog;
    DKLanguageController1: TDKLanguageController;
    procedure cbBackgroundClick(Sender: TObject);
    procedure cbCustomTitleClick(Sender: TObject);
    procedure btnFontSettingsClick(Sender: TObject);
    procedure btnBrowseBackgroundClick(Sender: TObject);
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
  TfrmGeneralOptionsPageClass = class of TfrmGeneralOptionsPage;
var
  frmGeneralOptionsPage: TfrmGeneralOptionsPage;

implementation

uses
  ulAppConfig, ulSysUtils, AppConfig;

{$R *.dfm}

{ TfrmGeneralOptionsPage }

procedure TfrmGeneralOptionsPage.btnBrowseBackgroundClick(Sender: TObject);
var
  PathTemp: string;
begin
  OpenDialog1.Filter     := DKLangConstW('msgFilterBackground');
  OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(edtBackground.Text));
  if (OpenDialog1.Execute) then
  begin
    PathTemp := AbsoluteToRelative(OpenDialog1.FileName);
    edtBackground.Text := PathTemp;
  end;
end;

procedure TfrmGeneralOptionsPage.btnFontSettingsClick(Sender: TObject);
begin
  FontDialog1.Execute;
end;

procedure TfrmGeneralOptionsPage.cbBackgroundClick(Sender: TObject);
begin
  edtBackground.Enabled := cbBackground.Checked;
  btnBrowseBackground.Enabled := cbBackground.Checked;
end;

procedure TfrmGeneralOptionsPage.cbCustomTitleClick(Sender: TObject);
begin
  edtCustomTitle.Enabled := cbCustomTitle.Checked;
end;

function TfrmGeneralOptionsPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_General;
end;

function TfrmGeneralOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgGeneral');
end;

function TfrmGeneralOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  //Window
  cbHoldSize.Checked         := Config.HoldSize;
  cbWindowOnTop.Checked      := Config.AlwaysOnTop;
  cbHideSearch.Checked       := Config.HideTabSearch;
  cbCustomTitle.Checked      := Config.UseCustomTitle;
  edtCustomTitle.Text        := Config.CustomTitleString;
  edtCustomTitle.Enabled     := Config.UseCustomTitle;
  //Startup
  cbWindowsStartup.Checked   := Config.StartWithWindows;
  cbShowPanelStartup.Checked := Config.ShowPanelAtStartUp;
  cbShowMenuStartup.Checked  := Config.ShowMenuAtStartUp;
  //Language
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
  //Treeview
  cbAutoOpClCat.Checked := Config.TVAutoOpClCats;
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

function TfrmGeneralOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  //Window
  Config.HoldSize           := cbHoldSize.Checked;
  Config.AlwaysOnTop        := cbWindowOnTop.Checked;
  Config.HideTabSearch      := cbHideSearch.Checked;
  Config.CustomTitleString  := edtCustomTitle.Text;
  Config.UseCustomTitle     := cbCustomTitle.Checked;
  //Startup
  Config.StartWithWindows   := cbWindowsStartup.Checked;
  Config.ShowPanelAtStartUp := cbShowPanelStartup.Checked;
  Config.ShowMenuAtStartUp  := cbShowMenuStartup.Checked;
  //Language
  { TODO -oMatteo -c : Language code 29/11/2009 21:39:41 }
//  Config.LangName         := cxLanguage.Items[cxLanguage.ItemIndex];
  //Treeview
  Config.TVAutoOpClCats     := cbAutoOpClCat.Checked;
  //Treeview
  Config.TVBackgroundPath   := edtBackground.Text;
  Config.TVBackground       := cbBackground.Checked;
  //Treeview Font
  Config.TVFont             := FontDialog1.Font;
end;

end.
