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

unit Frame.Properties.General.Software;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Graphics, Dialogs, StdCtrls, Frame.Properties.General, EditBtn,
  DefaultTranslator, Classes;

type

  { TfrmSWGeneralPropertyPage }

  TfrmSWGeneralPropertyPage = class(TfrmBaseGeneralPropertyPage)
    edtParameters: TEdit;
    edtPathExe: TFileNameEdit;
    edtWorkingDir: TDirectoryEdit;
    grpSoftware: TGroupBox;
    lbInfo2: TLabel;
    lbParameters: TLabel;
    lbPathExe: TLabel;
    lbWorkingDir: TLabel;

    procedure edtPathExeAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtPathExeBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtPathExeChange(Sender: TObject);
    procedure btnExtractNameClick(Sender: TObject);
    procedure edtWorkingDirAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtWorkingDirBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtWorkingDirChange(Sender: TObject);
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
  Kernel.Consts, NodeDataTypes.Files, Kernel.ResourceStrings,
  Utility.FileFolder, Kernel.Instance, Kernel.Manager;

{$R *.lfm}

procedure TfrmSWGeneralPropertyPage.btnExtractNameClick(Sender: TObject);
begin
  edtName.Text := ExtractFileNameEx(edtPathExe.text);
end;

procedure TfrmSWGeneralPropertyPage.edtPathExeAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := ASuiteInstance.Paths.AbsoluteToRelative(AName);
end;

procedure TfrmSWGeneralPropertyPage.edtPathExeBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  edtPathExe.Filter := msgFilterExe;
  AName := ASuiteInstance.Paths.RelativeToAbsolute(AName);
end;

procedure TfrmSWGeneralPropertyPage.edtPathExeChange(Sender: TObject);
begin
  //btnExtractName.Enabled := CheckPropertyPath(edtPathExe);
end;

procedure TfrmSWGeneralPropertyPage.edtWorkingDirAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := ASuiteInstance.Paths.AbsoluteToRelative(AName);
  CheckPropertyPath(edtWorkingDir, AName);
end;

procedure TfrmSWGeneralPropertyPage.edtWorkingDirBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := ASuiteInstance.Paths.RelativeToAbsolute(AName);
end;

procedure TfrmSWGeneralPropertyPage.edtWorkingDirChange(Sender: TObject);
begin
  CheckPropertyPath(edtWorkingDir);
end;

function TfrmSWGeneralPropertyPage.InternalLoadData: Boolean;
var
  FileNodeData: TvFileNodeData;
begin
  Result := inherited;

  //lbInfo2.Caption := Format(lbInfo2.Caption, [ASuiteInstance.Paths.SuitePathWorking, ASuiteInstance.Paths.SuiteDrive]);

  //Strange bug. %asuite% and %drive% causing error "Invalid argument index in format...", so I'm using a workaround
  lbInfo2.Caption := StringReplace(lbInfo2.Caption, '%s', ASuiteInstance.Paths.SuitePathWorking, [rfIgnoreCase]);
  lbInfo2.Caption := StringReplace(lbInfo2.Caption, '%s', ASuiteInstance.Paths.SuiteDrive, [rfIgnoreCase]);

  if Assigned(CurrentNodeData) then
  begin
    FileNodeData := TvFileNodeData(CurrentNodeData);
    edtPathExe.Text    := FileNodeData.PathFile;
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
      FileNodeData.PathFile  := edtPathExe.Text
    else
      FileNodeData.PathFile  := CONST_PATH_DRIVE;
    FileNodeData.Parameters := edtParameters.Text;
    FileNodeData.WorkingDir := edtWorkingDir.Text;
  end;
end;

end.
