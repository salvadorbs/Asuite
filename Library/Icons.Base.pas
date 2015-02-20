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
  SysUtils, Classes, Controls, ShellApi, SyncObjs;

type
  TBaseIcon = class
  private
    FLock: TCriticalSection;

    function GetImageIndex: Integer;
  protected
    FImageIndex: Integer;

    function InternalGetImageIndex(const APathFile: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadIcon: Integer; virtual; abstract;
    procedure ResetIcon; virtual;

    property ImageIndex: Integer read GetImageIndex;
  end;

implementation

{ TBaseIcon }

constructor TBaseIcon.Create;
begin
  FImageIndex := -1;
  FLock := TCriticalSection.Create;
end;

destructor TBaseIcon.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TBaseIcon.GetImageIndex: Integer;
begin
  FLock.Acquire;
  try
    if FImageIndex = -1 then
      FImageIndex := LoadIcon;
    Result := FImageIndex;
  finally
    FLock.Release;
  end;
end;

function TBaseIcon.InternalGetImageIndex(const APathFile: string): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  Result := -1;
  Flags := SHGFI_SYSICONINDEX or SHGFI_ICON;
  //Get index
  if SHGetFileInfo(PChar(APathFile), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
    Result := FileInfo.iIcon;
end;

procedure TBaseIcon.ResetIcon;
begin
  FImageIndex := -1;
end;

end.
