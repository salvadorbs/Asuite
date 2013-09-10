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

unit PropertySeparator;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, ulNodeDataTypes, StdCtrls, ExtCtrls,
  DKLang;

type
  TfrmPropertySeparator = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Panel1: TPanel;
    lbName: TLabel;
    edtName: TEdit;
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
    procedure LoadNodeData(AData: TvBaseNodeData);
    procedure SaveNodeData(AData: TvBaseNodeData);
  public
    { Public declarations }
    class function Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
  end;

var
  frmPropertySeparator: TfrmPropertySeparator;

implementation

uses
  appConfig, Main, ulCommonUtils;

{$R *.dfm}

class function TfrmPropertySeparator.Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
begin
  Result := mrCancel;
  if Assigned(NodeData) then
    with TfrmPropertySeparator.Create(AOwner) do
      try
        LoadNodeData(NodeData.Data);
        FormStyle := frmMain.FormStyle;
        ShowModal;
        if ModalResult = mrOK then
          SaveNodeData(NodeData.Data);
        Result := ModalResult;
      finally
        Free;
      end;
end;

procedure TfrmPropertySeparator.LoadNodeData(AData: TvBaseNodeData);
begin
  edtName.Text := AData.name;
end;

procedure TfrmPropertySeparator.SaveNodeData(AData: TvBaseNodeData);
begin
  AData.Name := edtName.Text;
  AData.Changed := true;
end;

end.
