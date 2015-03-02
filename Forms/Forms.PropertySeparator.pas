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

unit Forms.PropertySeparator;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, NodeDataTypes.Base, StdCtrls, ExtCtrls,
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
    class function Execute(AOwner: TComponent; NodeData: TvBaseNodeData): TModalResult;
  end;

var
  frmPropertySeparator: TfrmPropertySeparator;

implementation

uses
  Kernel.Logger;

{$R *.dfm}

class function TfrmPropertySeparator.Execute(AOwner: TComponent; NodeData: TvBaseNodeData): TModalResult;
var
  frm: TfrmPropertySeparator;
begin
  TASuiteLogger.Info('Opening form Property Separator', []);

  Result := mrCancel;
  frm := TfrmPropertySeparator.Create(AOwner);
  try
    frm.LoadNodeData(NodeData);
    frm.ShowModal;
    if frm.ModalResult = mrOK then
      frm.SaveNodeData(NodeData);
    Result := frm.ModalResult;
  finally
    frm.Free;
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
