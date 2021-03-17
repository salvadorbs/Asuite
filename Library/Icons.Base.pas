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
    FCacheIconCRC: Integer;
    FLock: SyncObjs.TCriticalSection;
    FStatic: Boolean;

    {$IFDEF MSWINDOWS}
    function BGRABitmapCreateFromHICON(AHIcon: HICON): TBGRABitmap;
    {$ENDIF}
    function ExtractIconFromSysImageList(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap;
    function ExtractIconFromFile(const APathFile: string;
      const AWantLargeIcon: Boolean): TBGRABitmap;
    function GetIconFromFile(const APathFile: string; const AWantLargeIcon: Boolean
      ): TBGRABitmap;
    function GetImageIndex: Integer;
    function GetPathCacheIcon: string;
    procedure SetCacheIconCRC(AValue: Integer);
    procedure SaveCacheIcon(const APath: string; const AImageIndex: Integer; const AHash: Integer);
    function GetIconFromImgList(AImageList: TImageList; AImageIndex: Integer;
      ALargeIcon: Boolean): TBGRABitmap;
  protected
    FImageIndex: Integer;

    function InternalLoadIcon: Integer; virtual;
    function GetName: string; virtual; abstract;
    function InternalGetImageIndex(const APathFile: string): Integer;
    function LoadIcon: Integer; virtual;
    function GetDefaultPathIcon: string; virtual; abstract;
  public
    constructor Create(AStatic: Boolean = False);
    destructor Destroy; override;

    procedure ResetIcon; virtual;
    procedure ResetCacheIcon;

    property Name: string read GetName;
    property ImageIndex: Integer read GetImageIndex;
    property Static: Boolean read FStatic write FStatic;
    property CacheIconCRC: Integer read FCacheIconCRC write SetCacheIconCRC;
    property PathCacheIcon: string read GetPathCacheIcon;
  end;

implementation

uses
   DataModules.Icons, Kernel.Consts, BGRABitmapTypes, Utility.FileFolder,
   AppConfig.Main, Kernel.Enumerations, Utility.System, ImgList;

{ TBaseIcon }

constructor TBaseIcon.Create(AStatic: Boolean);
begin
  FImageIndex := -1;
  FLock := SyncObjs.TCriticalSection.Create;
  FStatic := AStatic;
  FCacheIconCRC := 0;
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

function TBaseIcon.InternalLoadIcon: Integer;
var
  sTempPath, sPathCacheIcon, sDefaultPath: string;
  intHash: Integer;
begin
  Result := -1;

  sDefaultPath := GetDefaultPathIcon;
  intHash := GetFileXXHash32(sDefaultPath);

  //Get cache icon path
  if (Config.Cache) and (Config.ASuiteState <> lsImporting) then
  begin
    //Check CRC, if it fails reset cache icon
    sPathCacheIcon := PathCacheIcon;
    if (FileExists(sPathCacheIcon)) and (CacheIconCRC = intHash) then
      sTempPath := sPathCacheIcon
    else
      ResetCacheIcon;
  end;

  //Icon or exe
  if Not FileExists(sTempPath) then
    sTempPath := sDefaultPath;

  //Get image index
  if (sTempPath <> '') then
  begin
    if IsPathExists(sTempPath) then
      Result := InternalGetImageIndex(sTempPath);
    //Save icon cache
    if (Config.ASuiteState <> lsImporting) then
      SaveCacheIcon(sTempPath, Result, intHash);
  end;
end;

function TBaseIcon.GetPathCacheIcon: string;
begin
  Result := '';
  if (Self.Name <> '') then
    Result := Config.Paths.SuitePathCache + Self.Name + EXT_ICO;
end;

procedure TBaseIcon.SetCacheIconCRC(AValue: Integer);
begin
  if FCacheIconCRC <> AValue then
    FCacheIconCRC := AValue;
end;

procedure TBaseIcon.SaveCacheIcon(const APath: string;
  const AImageIndex: Integer; const AHash: Integer);
var
  Icon: TBGRAIconCursor;
  bmpLargeIcon, bmpSmallIcon: TBGRABitmap;
begin
  if (Config.Cache) and (AImageIndex <> -1) then
  begin
    //TODO: Don't save temp icon. only node icon!
    if (Self.Name <> '') and (LowerCase(ExtractFileExt(APath)) = EXT_EXE) then
    begin
      Icon := TBGRAIconCursor.Create(ifIco);
      try
        //Extract and insert icons in TIcon
        bmpLargeIcon := GetIconFromImgList(dmImages.ilLargeIcons, AImageIndex, True);
        bmpSmallIcon := GetIconFromImgList(dmImages.ilLargeIcons, AImageIndex, False);

        Icon.Add(bmpSmallIcon, 32);
        Icon.Add(bmpLargeIcon, 32);

        //Save file and get CRC from it
        Icon.SaveToFile(PathCacheIcon);
        FCacheIconCRC := AHash;
      finally
        Icon.Free;
        bmpLargeIcon.Free;
        bmpSmallIcon.Free;
      end;
    end;
  end;
end;

function TBaseIcon.GetIconFromImgList(AImageList: TImageList;
  AImageIndex: Integer; ALargeIcon: Boolean): TBGRABitmap;
{$IFDEF MSWINDOWS}
var
  Images: TCustomImageListResolution;
  bmpTemp: Graphics.TBitmap;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  bmpTemp := Graphics.TBitmap.Create;
  try
    if ALargeIcon then
      AImageList.FindResolution(ICON_SIZE_LARGE, Images)
    else
      AImageList.FindResolution(ICON_SIZE_SMALL, Images);

    Assert(Assigned(Images), 'Images is not assigned!');

    Images.GetBitmap(AImageIndex, bmpTemp);

    Result := TBGRABitmap.Create(bmpTemp);
  finally
    bmpTemp.Free;
  end;
{$ENDIF}
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

{$IFDEF MSWINDOWS}
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
{$ENDIF}

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
      Result := (Icon.GetBestFitBitmap(ICON_SIZE_SMALL, ICON_SIZE_SMALL) as TBGRABitmap);
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

function TBaseIcon.LoadIcon: Integer;
begin
  Result := InternalLoadIcon;
end;

procedure TBaseIcon.ResetIcon;
begin
  FImageIndex := -1;

  //Delete cache icon and reset CRC
  //TODO: Only for .exe and node files
  ResetCacheIcon;
end;

procedure TBaseIcon.ResetCacheIcon;
begin
  //Small icon cache
  if FileExists(PathCacheIcon) then
    SysUtils.DeleteFile(PathCacheIcon);

  FCacheIconCRC := 0;
end;

end.
