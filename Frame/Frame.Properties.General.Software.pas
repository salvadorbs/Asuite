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

unit Frame.Properties.General.Software;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DKLang, Frame.Properties.General;

type
  TfrmSWGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    grpSoftware: TGroupBox;
    btnBrowseWorkingDir: TButton;
    edtWorkingDir: TEdit;
    lbWorkingDir: TLabel;
    btnBrowseExe: TButton;
    lbInfo2: TLabel;
    edtParameters: TEdit;
    lbParameters: TLabel;
    edtPathExe: TEdit;
    lbPathExe: TLabel;
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
  strict protected
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmSWGeneralPropertyPage: TfrmSWGeneralPropertyPage;

implementation

uses
  Kernel.Consts, Utility.System, NodeDataTypes.Files, Utility.FileFolder,
  AppConfig.Main;

{$R *.dfm}

function TfrmSWGeneralPropertyPage.InternalLoadData: Boolean;
var
  FileNodeData: TvFileNodeData;
begin
  Result := inherited;
  lbInfo2.Caption := Format(lbInfo2.Caption, [Config.Paths.SuitePathWorking, Config.Paths.SuiteDrive]);
  if Assigned(CurrentNodeData) then
  begin
    FileNodeData := TvFileNodeData(CurrentNodeData);
    edtPathExe.Text    := FileNodeData.PathExe;
    edtParameters.Text := FileNodeData.Parameters;
    edtWorkingDir.Text := FileNodeData.WorkingDir;
  end;
end;

function TfrmSWGeneralPropertyPage.InternalSaveData: Boolean;
var
  FileNodeData: TvFileNodeData;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    FileNodeData := TvFileNodeData(CurrentNodeData);
    if (edtPathExe.Text <> '') then
      FileNodeData.PathExe  := edtPathExe.Text
    else
      FileNodeData.PathExe  := CONST_PATH_DRIVE;
    FileNodeData.Parameters := edtParameters.Text;
    FileNodeData.WorkingDir := edtWorkingDir.Text;
  end;
end;

end.
