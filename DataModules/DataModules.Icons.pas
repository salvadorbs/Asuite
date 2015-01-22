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

unit DataModules.Icons;

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, Dialogs,
  ShellApi, CommCtrl, Vcl.ImgList;

type
  TdmImages = class(TDataModule)
    ilSmallIcons: TImageList;
    ilLargeIcons: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    function SysImageListHandle(const Path: string; const WantLargeIcons: Boolean): Windows.THandle;
  public
    { Public declarations }
  end;

var
  dmImages: TdmImages;

implementation

uses
  AppConfig.Main;

{$R *.dfm}

{ TdmImages }

procedure TdmImages.DataModuleCreate(Sender: TObject);
begin
  //Use System ImageList
  ilSmallIcons.Handle := SysImageListHandle(Config.Paths.SuitePathWorking, False);
  ilLargeIcons.Handle := SysImageListHandle(Config.Paths.SuitePathWorking, True);
end;

function TdmImages.SysImageListHandle(const Path: string;
  const WantLargeIcons: Boolean): Windows.THandle;
  {Returns a handle to the system image list for path Path.
  WantLargeIcons determines if the image list is to contain large or small
  icons.}
var
  FI: ShellAPI.TSHFileInfo; // required file info structure
  Flags: Windows.UINT;      // flags used to request image list
begin
  Flags := ShellAPI.SHGFI_SYSICONINDEX or ShellAPI.SHGFI_ICON;
  if WantLargeIcons then
    Flags := Flags or ShellAPI.SHGFI_LARGEICON
  else
    Flags := Flags or ShellAPI.SHGFI_SMALLICON;
  Result := ShellAPI.SHGetFileInfo(PChar(Path), 0, FI, SizeOf(FI), Flags);
end;

end.

