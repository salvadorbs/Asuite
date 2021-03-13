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
  SysUtils, Controls, SyncObjs, LCLIntf, LCLType{$IFDEF MSWINDOWS}, ShellApi, CommCtrl{$ENDIF},
  Graphics;

type

  { TBaseIcon }

  TBaseIcon = class
  private
    FLock: SyncObjs.TCriticalSection;

    function GetIconFromSysImageList(const APathFile: string;
      const AWantLargeIcon: Boolean): Graphics.TBitmap;
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

uses
   DataModules.Icons, kicon;

{ TBaseIcon }

constructor TBaseIcon.Create;
begin
  FImageIndex := -1;
  FLock := SyncObjs.TCriticalSection.Create;
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
  hIco: HICON;
  FileIcon: kIcon.TIcon;
{$ENDIF}
begin
  //TODO: In linux we must get mime type and after image
  //      (see https://lists.lazarus-ide.org/pipermail/lazarus/2010-January/048660.html and https://forum.lazarus.freepascal.org/index.php?topic=40538.0)
  Result := Graphics.TBitmap.Create;
  {$IFDEF MSWINDOWS}

  Assert(Assigned(dmImages));

  if AWantLargeIcon then
    Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;

  try
    if SHGetFileInfoW(PChar(APathFile), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
    begin
      //Get icon handle
      if AWantLargeIcon then
        hIco := ImageList_GetIcon(dmImages.SysImageListLarge, FileInfo.iIcon, ILD_NORMAL)
      else
        hIco := ImageList_GetIcon(dmImages.SysImageListSmall, FileInfo.iIcon, ILD_NORMAL);

      //Check icon handle
      if hIco <> 0 then
      begin
        //Use kIcon's TIcon - It supports alpha 32bpp
        FileIcon := kIcon.TIcon.Create;
        try
          //Workaround: FileIcon lose Alpha, if it add directly in ImageList
          //Load icon handle and copy to bitmap bmp
          FileIcon.LoadFromHandle(hIco);
          FileIcon.CopyToBitmap(0, Result);
        finally
          FreeAndNil(FileIcon);
        end;
      end;
    end;
  finally
    DestroyIcon(FileInfo.hIcon);
  end;

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
