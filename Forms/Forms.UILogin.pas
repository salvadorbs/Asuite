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

unit Forms.UILogin;

{$mode DelphiUnicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, Kernel.Types;

type

  { TfrmUILogin }

  TfrmUILogin = class(TForm)
    Bevel1: TBevel;
    btnPanel: TButtonPanel;
    edtUserName: TEdit;
    edtPassword: TEdit;
    imgTop: TImage;
    lblUserName: TLabel;
    lblPassword: TLabel;
    lblInfo: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private

  public
    class function Execute(AOwner: TComponent; ACaption: String): TUserData;
  end;

var
  frmUILogin: TfrmUILogin;

implementation

uses
  Kernel.Logger, Utility.Misc, Kernel.ResourceStrings;

{$R *.lfm}

{ TfrmUILogin }

procedure TfrmUILogin.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ((frmUILogin.edtUserName.Text <> '') and (ModalResult = mrOk)) or (ModalResult = mrCancel);
  if not CanClose then
    ShowMessageEx(msgErrEmptyUserName, True);
end;

class function TfrmUILogin.Execute(AOwner: TComponent; ACaption: String
  ): TUserData;
begin
  TASuiteLogger.Info('Opening form UILogin', []);

  Result.UserName := '';
  Result.Password := '';
  frmUILogin := TfrmUILogin.Create(AOwner);
  try
    SetFormPositionFromConfig(frmUILogin);

    frmUILogin.Caption := ACaption;

    if frmUILogin.ShowModal = mrOK then
    begin
      Result.UserName := Trim(frmUILogin.edtUserName.Text);
      Result.Password := Trim(frmUILogin.edtPassword.Text);
    end;
  finally
    FreeAndNil(frmUILogin);
  end;
end;

end.

