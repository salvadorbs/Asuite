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

unit Icons.Base;

interface

uses
  SysUtils, Classes, Controls, ShellApi, Kernel.Enumerations;

type
  TBaseIcon = class
  private
    FPath: string;
    FSmallImageIndex: Integer; //Size 16p
    FLargeImageIndex: Integer; //Size 32p

    function GetImageIndex(const APath: string; const ASmallIcon: Boolean = True): Integer;
    function GetName: string;
  public
    constructor Create(APath: string);

    procedure Load(AIconSize: TIconSize); virtual;

    property Name: string read GetName;
    property Path: string read FPath write FPath;
    property SmallImageIndex: Integer read FSmallImageIndex;
    property LargeImageIndex: Integer read FLargeImageIndex;
  end;

implementation

{ TBaseIcon }

constructor TBaseIcon.Create(APath: string);
begin
  FPath := APath;
end;

function TBaseIcon.GetImageIndex(const APath: string;
  const ASmallIcon: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  Result := -1;
  //Small icon
  if ASmallIcon then
    Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON
  else //Else large icon
    Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON;
  //Get index
  if SHGetFileInfo(PChar(APath), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
    Result := FileInfo.iIcon;
end;

function TBaseIcon.GetName: string;
begin
  if FPath <> '' then
  begin
    Result := ExtractFileName(FPath);
    Result := ChangeFileExt(Result, '');
  end;
end;

procedure TBaseIcon.Load(AIconSize: TIconSize);
begin
  if FileExists(FPath) then
  begin
    //Get imageindex based of AIconSize
    case AIconSize of
      isSmall: FSmallImageIndex := GetImageIndex(FPath);
      isLarge: FLargeImageIndex := GetImageIndex(FPath, False);
    end;
  end;
end;

end.
