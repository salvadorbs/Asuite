{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit PropertySeparator;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, ulNodeDataTypes, StdCtrls,
  ExtCtrls;

type
  TfrmPropertySeparator = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Panel1: TPanel;
    lbName: TLabel;
    edtName: TEdit;
    procedure FormCreate(Sender: TObject);
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
  appConfig, Main;

{$R *.dfm}

class function TfrmPropertySeparator.Edit(AOwner: TComponent; NodeData: PBaseData): TModalResult;
begin
  Result := mrCancel;
  if not Assigned(NodeData) then
    ShowMessage(msgErrGeneric)
  else
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

procedure TfrmPropertySeparator.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPropertySeparator.LoadNodeData(AData: TvBaseNodeData);
begin
  edtName.Text := AData.name;
end;

procedure TfrmPropertySeparator.SaveNodeData(AData: TvBaseNodeData);
begin
  AData.Name    := StringReplace(edtName.Text, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
  AData.Name    := StringReplace(AData.Name, '&', '&&', [rfIgnoreCase,rfReplaceAll]);
  AData.Changed := true;
end;

end.
