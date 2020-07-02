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

unit Forms.About;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Forms, Dialogs, StdCtrls, ExtCtrls, ATLinkLabel;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    GroupBox3: TGroupBox;
    lnklblWebSite: TATLabelLink;
    
    imgLicense: TImage;
    imgDonate: TImage;
    lblAppName: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    imgLogo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure imgDonateClick(Sender: TObject);
    procedure imgLicenseClick(Sender: TObject);
    procedure lnklblWebSiteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout : TfrmAbout;

implementation

uses
  Utility.Misc, Kernel.Consts, AppConfig.Main;

{$R *.lfm}

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblAppName.Caption := Format(lblAppName.Caption, [APP_NAME]);
  lblVersion.Caption := Format(lblVersion.Caption, [GetASuiteVersion(True), {$IFDEF Win32}'32'{$ELSE}'64'{$ENDIF}]);
end;

procedure TfrmAbout.imgDonateClick(Sender: TObject);
begin
  OpenURL(PChar('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=QYASG6DPAYSXW'));
end;

procedure TfrmAbout.imgLicenseClick(Sender: TObject);
begin
   OpenDocument(PChar(Config.Paths.SuitePathWorking + 'docs\license.txt'));
end;

procedure TfrmAbout.lnklblWebSiteClick(Sender: TObject);
begin
  OpenURL(PChar('http://www.salvadorsoftware.com'));
end;

end.
