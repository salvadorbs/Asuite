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
  SysUtils, Controls, SyncObjs, LCLIntf, LCLType, Graphics
  {$IFDEF MSWINDOWS}, ShellApi, CommCtrl, uBitmap{$ENDIF};

type

  { TBaseIcon }

  TBaseIcon = class
  private
    FLock: SyncObjs.TCriticalSection;
    FStatic: Boolean;

    function GetIconFromSysImageList(const APathFile: string;
      const AWantLargeIcon: Boolean): Graphics.TBitmap;
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
   DataModules.Icons;

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

function TBaseIcon.GetIconFromSysImageList(const APathFile: string;
  const AWantLargeIcon: Boolean): Graphics.TBitmap;
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
    Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;

  try
    //TODO: Maybe use ExtractIconExW for exe/ico and SHGetFileInfoW for other file exts
    if SHGetFileInfoW(PChar(APathFile), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
    begin
      if AWantLargeIcon then
        Result := BitmapCreateFromHICON(ImageList_GetIcon(dmImages.SysImageListLarge, FileInfo.iIcon, ILD_NORMAL))
      else
        Result := BitmapCreateFromHICON(ImageList_GetIcon(dmImages.SysImageListSmall, FileInfo.iIcon, ILD_NORMAL));
    end;
  finally
    DestroyIcon(FileInfo.hIcon);
  end;

  if Result = nil then
    Result := Graphics.TBitmap.Create;
  {$ENDIF}
end;

function TBaseIcon.InternalGetImageIndex(const APathFile: string): Integer;
var
  bmpSmallIcon, bmpLargeIcon: TBitmap;
begin
  Result := -1;

  //Get icon and insert it in ASuite ImageList
  bmpLargeIcon := GetIconFromSysImageList(APathFile, True);
  bmpSmallIcon := GetIconFromSysImageList(APathFile, False);
  try
    Result := dmImages.AddMultipleResolutions([bmpSmallIcon, bmpLargeIcon]);
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
