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

unit About;

{$MODE Delphi}

interface

uses
  Forms, StdCtrls, ExtCtrls, LCLIntf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    GroupBox3: TGroupBox;
    imASuiteLogo: TImage;
    lbASuiteTitle: TLabel;
    lbASuiteVersion: TLabel;
    memIntro: TMemo;
    lnklblWebSite: TLabel;
    procedure lnklblWebSiteClick(Sender: TObject);
  private
    { Private declarations }
    FArea: Cardinal;
  public
    { Public declarations }
  end;

var
  frmAbout    : TfrmAbout;

implementation


{$R *.lfm}

procedure TfrmAbout.lnklblWebSiteClick(Sender: TObject);
begin
   OpenURL(lnklblWebSite.Caption);
end;

end.
