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

unit Frame.Options.Trayicon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DKLang, Frame.BaseEntity;

type
  TfrmTrayiconOptionsPage = class(TfrmBaseEntityPage)
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmTrayiconOptionsPage: TfrmTrayiconOptionsPage;

implementation

{$R *.dfm}

{ TfrmTrayiconOptionsPage }

function TfrmTrayiconOptionsPage.GetImageIndex: Integer;
begin
//  Result := IMAGELARGE_INDEX_Trayicon;
end;

function TfrmTrayiconOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgTrayIcon');
end;

function TfrmTrayiconOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
end;

function TfrmTrayiconOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
end;

end.
