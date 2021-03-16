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

unit Icons.Base;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Controls, SyncObjs, LCLIntf, LCLType, Graphics, BGRAIconCursor, BGRABitmap
  {$IFDEF MSWINDOWS}, ShellApi, CommCtrl, uBitmap{$ENDIF};

type

  { TBaseIcon }

  TBaseIcon = class
  private
    FLock: SyncObjs.TCriticalSection;
    FStatic: Boolean;

    function BGRABitmapCreateFromHICON(AHIcon: HICON): TBGRABitmap;
    function ExtractIconFromSysImageList(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap;
    function ExtractIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap;
    function GetIconFromFile(const APathFile: string; const AWantLargeIcon: Boolean
      ): TBGRABitmap;
    function GetImageIndex: Integer;
  protected
    FImageIndex: Integer;

    function GetName: string; virtual; abstract;
    function InternalGetImageIndex(const APathFile: string): Integer;
    function LoadIcon: Integer; virtual; abstract;
  public
    constructor Create(AStatic: Boolean = False);
    destructor Destroy; override;
    procedure ResetIcon; virtual;

    property Name: string read GetName;
    property ImageIndex: Integer read GetImageIndex;
    property Static: Boolean read FStatic write FStatic;
  end;

implementation

uses
   DataModules.Icons, Kernel.Consts, BGRABitmapTypes;

{ TBaseIcon }

constructor TBaseIcon.Create(AStatic: Boolean);
begin
  FImageIndex := -1;
  FLock := SyncObjs.TCriticalSection.Create;
  FStatic := AStatic;
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

function TBaseIcon.ExtractIconFromSysImageList(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
{$IFDEF MSWINDOWS}
var
  FileInfo: TSHFileInfoW;
  Flags: Integer;
{$ENDIF}
begin
  //TODO: In linux we must get mime type and after image
  //      (see https://lists.lazarus-ide.org/pipermail/lazarus/2010-January/048660.html and https://forum.lazarus.freepascal.org/index.php?topic=40538.0)
  Result := nil;
  {$IFDEF MSWINDOWS}

  Assert(Assigned(dmImages));

  if AWantLargeIcon then
    Flags := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;

  try
    if SHGetFileInfoW(PChar(APathFile), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
      Result := BGRABitmapCreateFromHICON(FileInfo.hIcon);
  finally
    DestroyIcon(FileInfo.hIcon);
  end;
  {$ENDIF}
end;

function TBaseIcon.BGRABitmapCreateFromHICON(AHIcon: HICON): TBGRABitmap;
var
  bmpTemp: Graphics.TBitmap;
begin
  bmpTemp := BitmapCreateFromHICON(AHIcon);
  try
    Result := TBGRABitmap.Create(bmpTemp);
  finally
    FreeAndNil(bmpTemp);
  end;
end;

function TBaseIcon.ExtractIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  Icon : TBGRAIconCursor;
  IcoHandle: THandle;
begin
  Result := nil;

  if not FileExists(APathFile) then
    Exit;

  Icon := TBGRAIconCursor.Create(ifIco);
  try
    Icon.LoadFromFile(APathFile);

    if AWantLargeIcon then
      Result := (Icon.GetBestFitBitmap(ICON_SIZE_LARGE, ICON_SIZE_LARGE) as TBGRABitmap)
    else
      Result := (Icon.GetBestFitBitmap(ICON_SIZE_LARGE, ICON_SIZE_LARGE) as TBGRABitmap);
  finally
    Icon.Free;
  end;
end;

function TBaseIcon.GetIconFromFile(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
begin
  Result := nil;

  if ExtractFileExt(APathFile) = EXT_ICO then
    Result := ExtractIconFromFile(APathFile, AWantLargeIcon)
  else
    Result := ExtractIconFromSysImageList(APathFile, AWantLargeIcon);

  if Result = nil then
    Result := TBGRABitmap.Create;
end;

function TBaseIcon.InternalGetImageIndex(const APathFile: string): Integer;
var
  bmpSmallIcon, bmpLargeIcon: TBGRABitmap;
begin
  Result := -1;

  //TODO: Find icon cache

  //Get icon and insert it in ASuite ImageList
  bmpLargeIcon := GetIconFromFile(APathFile, True);
  bmpSmallIcon := GetIconFromFile(APathFile, False);
  try
    Result := dmImages.AddMultipleResolutions([bmpSmallIcon.Bitmap, bmpLargeIcon.Bitmap]);

    //TODO: Save icon cache
  finally
    FreeAndNil(bmpLargeIcon);
    FreeAndNil(bmpSmallIcon);
  end;
end;

procedure TBaseIcon.ResetIcon;
begin
  FImageIndex := -1;
end;

end.
