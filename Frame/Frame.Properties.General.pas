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

unit Frame.Properties.General;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DKLang, Frame.Properties.Base;

type
  TfrmBaseGeneralPropertyPage = class(TfrmBasePropertyPage)
    gbItem: TGroupBox;
    lbName: TLabel;
    edtName: TEdit;
    lbPathIcon: TLabel;
    edtPathIcon: TEdit;
    btnBrowseIcon: TButton;
    procedure edtNameEnter(Sender: TObject);
  private
    { Private declarations }
    function CheckPropertyName(Edit: TEdit): Boolean;
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmBaseGeneralPropertyPage: TfrmBaseGeneralPropertyPage;

implementation

uses
  Utility.Misc, Kernel.Enumerations, NodeDataTypes.Files;

{$R *.dfm}

{ TfrmGeneralPropertyPage }

function TfrmBaseGeneralPropertyPage.CheckPropertyName(Edit: TEdit): Boolean;
begin
  Result := True;
  // Check if inserted name is empty, then
  if (Trim(Edit.Text) = '') then
  begin
    ShowMessageEx(DKLangConstW('msgErrEmptyName'),true);
    Edit.Color := clYellow;
    Result := False;
  end;
end;

procedure TfrmBaseGeneralPropertyPage.edtNameEnter(Sender: TObject);
begin
  TEdit(Sender).Color := clWindow;
end;

function TfrmBaseGeneralPropertyPage.GetImageIndex: Integer;
begin
//  Result := IMAGELARGE_INDEX_PropGeneral;
end;

function TfrmBaseGeneralPropertyPage.GetTitle: string;
begin
  Result := DKLangConstW('msgGeneral');
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
      if (CurrentNodeData.DataType = vtdtFile) then
        TvFileNodeData(CurrentNodeData).ShortcutDesktop := False;
      CurrentNodeData.Name     := edtName.Text;
      CurrentNodeData.PathIcon := edtPathIcon.Text;
    end;
  end;
end;

end.
