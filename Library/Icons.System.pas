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

unit Icons.System;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, SyncObjs, BGRABitmap, Icons.Base, SystemIcons.Mime;

type

  { Single system-icon extractor shared by every node type. The platform
    details live in AsuiteComps (SystemIcons.Platform); here we only own a
    process-wide MIME table so icon loads never touch application globals
    and stay usable from background preloads and headless tests. }

  TSystemIcon = class(TBaseIcon)
  private
    class var FSharedMime: TMimeIconProvider;
    class var FSharedLock: TCriticalSection;
    class function SharedMime: TMimeIconProvider; static;
  protected
    function GetIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
  public
    class constructor CreateClass;
    class destructor DestroyClass;
  end;

implementation

uses
  Kernel.Consts, SystemIcons.Platform;

{ TSystemIcon }

class constructor TSystemIcon.CreateClass;
begin
  FSharedLock := TCriticalSection.Create;
end;

class destructor TSystemIcon.DestroyClass;
begin
  FreeAndNil(FSharedMime);
  FreeAndNil(FSharedLock);
end;

class function TSystemIcon.SharedMime: TMimeIconProvider;
begin
  FSharedLock.Acquire;
  try
    if not Assigned(FSharedMime) then
    begin
      FSharedMime := TMimeIconProvider.Create;
      FSharedMime.LoadSystemTable;
    end;
    Result := FSharedMime;
  finally
    FSharedLock.Release;
  end;
end;

function TSystemIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  Size: Integer;
begin
  if AWantLargeIcon then
    Size := ICON_SIZE_LARGE
  else
    Size := ICON_SIZE_SMALL;

  Result := TSystemIconHelper.GetSystemIcon(APathFile, Size, SharedMime);
  if Result = nil then
    Result := TBGRABitmap.Create;
end;

end.
