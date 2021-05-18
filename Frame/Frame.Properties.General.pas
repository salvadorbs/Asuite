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

unit Frame.Properties.General;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Frame.Properties.Base,
  EditBtn, DefaultTranslator;

type

  { TfrmBaseGeneralPropertyPage }

  TfrmBaseGeneralPropertyPage = class(TfrmBasePropertyPage)
    edtPathIcon: TFileNameEdit;
    gbItem: TGroupBox;
    lbName: TLabel;
    edtName: TEdit;
    lbPathIcon: TLabel;
    procedure edtPathIconAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtPathIconBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure edtPathIconExit(Sender: TObject);
    procedure edtNameEnter(Sender: TObject);
    procedure edtPathIconChange(Sender: TObject);
  private
    { Private declarations }
    function CheckPropertyName(Edit: TEdit): Boolean;
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;

    function CheckPropertyPath(ASender: TCustomEditButton; APath: string = ''): Boolean;
  public
    { Public declarations }
  end;

var
  frmBaseGeneralPropertyPage: TfrmBaseGeneralPropertyPage;

implementation

uses
  Utility.Misc, NodeDataTypes.Files, Utility.System, Kernel.ResourceStrings,
  Kernel.Instance, Kernel.Manager;

{$R *.lfm}

{ TfrmGeneralPropertyPage }

function TfrmBaseGeneralPropertyPage.CheckPropertyName(Edit: TEdit): Boolean;
begin
  Result := True;
  // Check if inserted name is empty, then
  if (Trim(Edit.Text) = '') then
  begin
    ShowMessageEx(msgErrEmptyName,true);
    Edit.Color := clYellow;
    Result := False;
  end;
end;

function TfrmBaseGeneralPropertyPage.CheckPropertyPath(ASender: TCustomEditButton;
  APath: string): Boolean;
var
  cColor : TColor;
  sHint, sPath: string;
begin
  sPath := APath;
  if APath = '' then
    sPath := ASender.Text;
  //Check path
  Result := IsPathExists(sPath);
  if Result then
  begin
    //File found - Change font color with default color (clWindowText)
    cColor := clWindowText;
    sHint  := '';
  end
  else begin
    //File not found - Change font color with red
    cColor := clRed;
    sHint  := msgFileNotFound;
  end;
  //Change ASender's properties Color and Hint in based of vars cColor and sHint
  ASender.Hint := sHint;
  if (ASender is TFileNameEdit) then
    TFileNameEdit(ASender).Font.Color := cColor
  else
    if (ASender is TDirectoryEdit) then
      TDirectoryEdit(ASender).Font.Color := cColor;
end;

procedure TfrmBaseGeneralPropertyPage.edtNameEnter(Sender: TObject);
begin
  if Sender is TEdit then
    TEdit(Sender).Color := clWindow;
end;

procedure TfrmBaseGeneralPropertyPage.edtPathIconAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  AName := ASuiteInstance.Paths.AbsoluteToRelative(AName);
  CheckPropertyPath(edtPathIcon, AName);
end;

procedure TfrmBaseGeneralPropertyPage.edtPathIconBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  edtPathIcon.Filter := msgFilterIconExe;
  AName := ASuiteInstance.Paths.RelativeToAbsolute(AName);
end;

procedure TfrmBaseGeneralPropertyPage.edtPathIconChange(Sender: TObject);
begin
  CheckPropertyPath(edtPathIcon);
end;

procedure TfrmBaseGeneralPropertyPage.edtPathIconExit(Sender: TObject);
begin
  CheckPropertyPath(edtPathIcon);
end;

function TfrmBaseGeneralPropertyPage.GetImageIndex: Integer;
begin
  Result := ASuiteManager.IconsManager.GetIconIndex('property_general');
end;

function TfrmBaseGeneralPropertyPage.GetTitle: string;
begin
  Result := msgGeneral;
end;

function TfrmBaseGeneralPropertyPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  if Assigned(CurrentNodeData) then
  begin
    edtName.Text     := CurrentNodeData.Name;
    edtPathIcon.Text := CurrentNodeData.PathIcon;
  end;
end;

function TfrmBaseGeneralPropertyPage.InternalSaveData: Boolean;
begin
  Result := CheckPropertyName(edtName);
  if Result then
  begin
    if Assigned(CurrentNodeData) then
    begin
      //Delete desktop shortcut
      if (CurrentNodeData.IsFileItem) then
        TvFileNodeData(CurrentNodeData).ShortcutDesktop := False;
      CurrentNodeData.Name     := edtName.Text;
      CurrentNodeData.PathIcon := edtPathIcon.Text;
    end;
  end;
end;

end.
