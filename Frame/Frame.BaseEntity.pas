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

unit Frame.BaseEntity;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs;

type

  { TfrmBaseEntityPage }

  TfrmBaseEntityPage = class(TFrame)
  private
    function GetName: UnicodeString;
    { Private declarations }
  strict protected
    function GetTitle: string; virtual;
    function GetImageIndex: integer; virtual;
    function InternalLoadData: Boolean; virtual;
    function InternalSaveData: Boolean; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function SaveData: Boolean;
    function LoadData: Boolean;

    property Name: UnicodeString read GetName;
    property Title: string read GetTitle;
    property ImageIndex: integer read GetImageIndex;
  end;

TPageFrameClass = class of TfrmBaseEntityPage;

implementation

uses
  Kernel.Logger;

{$R *.lfm}

{ TfrmBaseEntityPage }

constructor TfrmBaseEntityPage.Create(AOwner: TComponent);
begin
  inherited;
  //ScaleBy(Screen.PixelsPerInch, 96);
end;

function TfrmBaseEntityPage.GetImageIndex: integer;
begin
  Result := -1;
end;

function TfrmBaseEntityPage.GetName: UnicodeString;
begin
  Result := Self.ClassName;
end;

function TfrmBaseEntityPage.GetTitle: string;
begin
  Result := '';
end;

function TfrmBaseEntityPage.InternalLoadData: Boolean;
begin
  TASuiteLogger.Enter(PUTF8Char(Format('TfrmBaseEntityPage.InternalLoadData(%s)', [Self.Name])), Self);
  Result := True;
end;

function TfrmBaseEntityPage.InternalSaveData: Boolean;
begin
  TASuiteLogger.Enter(PUTF8Char(Format('TfrmBaseEntityPage.InternalSaveData(%s)', [Self.Name])), Self);
  Result := True;
end;

function TfrmBaseEntityPage.SaveData: Boolean;
begin
  Result := Self.InternalSaveData;
end;

function TfrmBaseEntityPage.LoadData: Boolean;
begin
  Result := Self.InternalLoadData;
end;

end.
