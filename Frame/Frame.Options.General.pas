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

unit Frame.Options.General;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Dialogs, Frame.BaseEntity, UniqueInstance, StdCtrls,
  DefaultTranslator;

type

  { TfrmGeneralOptionsPage }

  TfrmGeneralOptionsPage = class(TfrmBaseEntityPage)
    cbSecondInstanceGM: TCheckBox;
    
    gbStartup: TGroupBox;
    cbWindowsStartup: TCheckBox;
    cbShowPanelStartup: TCheckBox;
    cbShowMenuStartup: TCheckBox;
    chkMissedSchedulerTask: TCheckBox;
    grpLanguage: TGroupBox;
    cxLanguage: TComboBox;
    gbExecution: TGroupBox;
    lbActionOnExe: TLabel;
    cbRunSingleClick: TCheckBox;
    cxActionOnExe: TComboBox;
    chkConfirmMessageCat: TCheckBox;
    chkAutoCloseProcess: TCheckBox;
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
  frmGeneralOptionsPage: TfrmGeneralOptionsPage;

implementation

uses
  AppConfig.Main, Kernel.Enumerations, Kernel.ResourceStrings, FileUtil, LazFileUtils,
  Kernel.Consts;

{$R *.lfm}

{ TfrmGeneralOptionsPage }

function TfrmGeneralOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('general');
end;

function TfrmGeneralOptionsPage.GetTitle: string;
begin
  Result := msgGeneral;
end;

function TfrmGeneralOptionsPage.InternalLoadData: Boolean;
var
  I, idxDot: Integer;
  FileInfo: TSearchRec;
  strID: String;
  SearchMask: String;
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

  //Language
  //Search for all languages/xxx.po files

  //Search existing translations
  SearchMask := Config.Paths.SuitePathLocale + LowerCase(APP_NAME) + '.*' + EXT_PO;

  //TODO: Extract this code in a separate method
  if FindFirstUTF8(SearchMask, faAnyFile, FileInfo) = 0 then
  begin
    repeat
      I := -1;
      if (FileInfo.Attr and (faDirectory or faVolumeId) = 0) then
      begin
        if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
          continue;

        strID := ExtractFileNameWithoutExt(FileInfo.Name);
        idxDot := pos('.', strID);

        if idxDot <> 0 then
        begin
          strID := copy(strID, idxDot + 1, Length(strID) - 1);
          I := cxLanguage.Items.Add(strID);
        end;

        if (strID = Config.LangID) and (I <> -1 )then
          cxLanguage.ItemIndex  := I;
      end;
    until
      FindNextUTF8(FileInfo) <> 0;
  end;
  FindCloseUTF8(FileInfo);

  if cxLanguage.ItemIndex = -1 then
    cxLanguage.ItemIndex  := 0;

  //Execution options
  cxActionOnExe.ItemIndex   := Ord(Config.ActionOnExe);
  cbRunSingleClick.Checked  := Config.RunSingleClick;
  chkConfirmMessageCat.Checked := Config.ConfirmRunCat;
  chkAutoCloseProcess.Checked  := Config.AutoCloseProcess;
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
  if cxLanguage.ItemIndex <> -1 then
    Config.LangID := cxLanguage.Items[cxLanguage.ItemIndex];

  //Execution options
  Config.ActionOnExe    := TActionOnExecute(cxActionOnExe.ItemIndex);
  Config.RunSingleClick := cbRunSingleClick.Checked;
  Config.ConfirmRunCat  := chkConfirmMessageCat.Checked;
  Config.AutoCloseProcess := chkAutoCloseProcess.Checked;
end;

end.
