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

unit Frame.Options.General;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Classes, Dialogs, Frame.BaseEntity, StdCtrls,
  Language.Manager;

type

  { TfrmGeneralOptionsPage }

  TfrmGeneralOptionsPage = class(TfrmBaseEntityPage)
    cbRunSingleClick: TCheckBox;
    cbSecondInstanceGM: TCheckBox;
    cbShowMenuStartup: TCheckBox;
    cbShowPanelStartup: TCheckBox;
    chkAutoCloseProcess: TCheckBox;
    chkConfirmMessageCat: TCheckBox;
    chkConfirmMsgCloseApp: TCheckBox;
    chkMissedSchedulerTask: TCheckBox;
    cxActionOnExe: TComboBox;
    cxLanguage: TComboBox;
    gbExecution: TGroupBox;

    gbStartup: TGroupBox;
    cbWindowsStartup: TCheckBox;
    gbStartup1: TGroupBox;
    lbActionOnExe: TLabel;
    lbLanguage: TLabel;
  private
    { Private declarations }
    // Language IDs (ex. 'it_IT') matching the items shown in cxLanguage,
    // which displays language names (ex. 'Italiano') instead of file names.
    FLangIDs: TStringList;
    procedure AddLanguages(AComboBox: TComboBox);
    procedure FillLanguageCombo(AComboBox: TComboBox; const Items: TLangItemArray);
    procedure SelectLanguage(AComboBox: TComboBox);
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
    destructor Destroy; override;
  end;

var
  frmGeneralOptionsPage: TfrmGeneralOptionsPage;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Kernel.ResourceStrings, Kernel.Consts,
  Kernel.Instance, Kernel.Manager;

{$R *.lfm}

{ TfrmGeneralOptionsPage }

function TfrmGeneralOptionsPage.GetImageIndex: Integer;
begin
  Result := ASuiteManager.IconsManager.GetIconIndex('general');
end;

destructor TfrmGeneralOptionsPage.Destroy;
begin
  FreeAndNil(FLangIDs);
  inherited Destroy;
end;

procedure TfrmGeneralOptionsPage.AddLanguages(AComboBox: TComboBox);
var
  Items: TLangItemArray;
begin
  Items := FindLanguageFiles(ASuiteInstance.Paths.SuitePathLocale, APP_NAME);
  ResolveLanguageNames(Items);
  SortLangItems(Items);
  FillLanguageCombo(AComboBox, Items);
  SelectLanguage(AComboBox);
end;

procedure TfrmGeneralOptionsPage.FillLanguageCombo(AComboBox: TComboBox;
  const Items: TLangItemArray);
var
  I: Integer;
begin
  if not Assigned(FLangIDs) then
    FLangIDs := TStringList.Create;
  FLangIDs.Clear;
  AComboBox.Items.BeginUpdate;
  try
    AComboBox.Items.Clear;
    for I := 0 to High(Items) do
    begin
      FLangIDs.Add(Items[I].ID);
      AComboBox.Items.Add(Items[I].DisplayName);
    end;
  finally
    AComboBox.Items.EndUpdate;
  end;
end;

procedure TfrmGeneralOptionsPage.SelectLanguage(AComboBox: TComboBox);
begin
  AComboBox.ItemIndex := IndexOfLangID(FLangIDs, Config.LangID);
end;

function TfrmGeneralOptionsPage.GetTitle: string;
begin
  Result := msgGeneral;
end;

function TfrmGeneralOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;

  cxActionOnExe.Items.Add(cxActionOnExeOpt_item0);
  cxActionOnExe.Items.Add(cxActionOnExeOpt_item1);
  cxActionOnExe.Items.Add(cxActionOnExeOpt_item2);

  //Startup
  cbWindowsStartup.Checked   := Config.StartWithWindows;
  cbShowPanelStartup.Checked := Config.ShowPanelAtStartUp;
  cbShowMenuStartup.Checked  := Config.ShowGraphicMenuAtStartUp;
  chkMissedSchedulerTask.Checked := Config.MissedSchedulerTask;
  cbSecondInstanceGM.Checked := Config.ShowGraphicMenuAnotherInstance;

  //Language: list every available translation
  AddLanguages(cxLanguage);

  if cxLanguage.ItemIndex = -1 then
    cxLanguage.ItemIndex  := 0;

  //Execution options
  cxActionOnExe.ItemIndex   := Ord(Config.ActionOnExe);
  cbRunSingleClick.Checked  := Config.RunSingleClick;
  chkConfirmMessageCat.Checked := Config.ConfirmRunCat;
  chkAutoCloseProcess.Checked  := Config.AutoCloseProcess;
  chkConfirmMsgCloseApp.Checked := Config.ConfirmMsgCloseApp;
end;

function TfrmGeneralOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;

  //Startup
  Config.StartWithWindows    := cbWindowsStartup.Checked;
  Config.ShowPanelAtStartUp  := cbShowPanelStartup.Checked;
  Config.ShowGraphicMenuAtStartUp := cbShowMenuStartup.Checked;
  Config.MissedSchedulerTask := chkMissedSchedulerTask.Checked;
  Config.ShowGraphicMenuAnotherInstance := cbSecondInstanceGM.Checked;

  //Language
  if (cxLanguage.ItemIndex <> -1) and Assigned(FLangIDs) and
     (cxLanguage.ItemIndex < FLangIDs.Count) then
    Config.LangID := FLangIDs[cxLanguage.ItemIndex];

  //Execution options
  Config.ActionOnExe    := TActionOnExecute(cxActionOnExe.ItemIndex);
  Config.RunSingleClick := cbRunSingleClick.Checked;
  Config.ConfirmRunCat  := chkConfirmMessageCat.Checked;
  Config.AutoCloseProcess := chkAutoCloseProcess.Checked;
  Config.ConfirmMsgCloseApp := chkConfirmMsgCloseApp.Checked;
end;

end.
