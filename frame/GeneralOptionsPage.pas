unit GeneralOptionsPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseOptionsPage, Vcl.StdCtrls;

type
  TfrmGeneralOptionsPage = class(TfrmBaseOptionsPage)
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
    procedure cbBackgroundClick(Sender: TObject);
    procedure cbCustomTitleClick(Sender: TObject);
    procedure btnFontSettingsClick(Sender: TObject);
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function InternalLoadData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmGeneralOptionsPage: TfrmGeneralOptionsPage;

implementation

uses
  ulAppConfig;

{$R *.dfm}

{ TfrmGeneralOptionsPage }

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

function TfrmGeneralOptionsPage.GetTitle: string;
begin
  Result := 'General';
end;

function TfrmGeneralOptionsPage.InternalLoadData: Boolean;
begin
  inherited;
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

end.
