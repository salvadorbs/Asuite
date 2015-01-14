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

unit Forms.About;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DKLang;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    GroupBox3: TGroupBox;
    imASuiteLogo: TImage;
    lbASuiteTitle: TLabel;
    lbASuiteVersion: TLabel;
    memIntro: TMemo;
    lnklblWebSite: TLinkLabel;
    DKLanguageController1: TDKLanguageController;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout : TfrmAbout;

implementation

uses
  Kernel.Consts, AppConfig.Main, Utility.Misc;

{$R *.dfm}

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lbASuiteVersion.Caption := Format(lbASuiteVersion.Caption, [GetASuiteVersion(False)]);
end;

end.
