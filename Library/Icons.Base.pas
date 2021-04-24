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
  SysUtils, Controls, SyncObjs, LCLIntf, LCLType, Graphics, BGRAIconCursor,
  BGRABitmap;

type
  //TODO: Add some logs in this and child class

  { TBaseIcon }

  TBaseIcon = class
  private
    FCacheIconCRC: Integer;
    FLock: SyncObjs.TCriticalSection;
    FStatic: Boolean;
    FImageIndex: Integer;
    FTempItem: Boolean;

    function GetImageIndex: Integer;
    function GetPathCacheIcon: string;
    procedure SetCacheIconCRC(AValue: Integer);
    procedure SaveCacheIcon(const APath: string; const AImageIndex: Integer; const AHash: Integer);
    function GetIconFromImgList(AImageList: TImageList; AImageIndex: Integer;
      ALargeIcon: Boolean): TBGRABitmap;
  protected
    function InternalGetImageIndex(const APathFile: string): Integer;
    function InternalLoadIcon: Integer; virtual;
    function LoadIcon: Integer; virtual;
    function GetName: string; virtual; abstract;
    function GetDefaultPathIcon: string; virtual; abstract;
    function GetIconFromFile(const APathFile: string; const AWantLargeIcon: Boolean
      ): TBGRABitmap; virtual; abstract;
    function LoadFromFileIcon(const APathFile: string; const AWantLargeIcon: Boolean
      ): TBGRABitmap;
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
    property TempItem: Boolean read FTempItem write FTempItem;
  end;

implementation

uses
   DataModules.Icons, Kernel.Consts, BGRABitmapTypes, Utility.FileFolder,
   AppConfig.Main, Kernel.Enumerations, Utility.System, ImgList, Kernel.Instance;

{ TBaseIcon }

constructor TBaseIcon.Create(AStatic: Boolean);
begin
  FImageIndex := -1;
  FLock := SyncObjs.TCriticalSection.Create;
  FStatic := AStatic;
  FCacheIconCRC := 0;
  FTempItem := True;
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
  isExeFile: Boolean;
begin
  Result := -1;
  intHash := 0;

  sDefaultPath := GetDefaultPathIcon;

  //Get cache icon path
  if (Config.Cache) and (Config.ASuiteState <> lsImporting) then
  begin
    isExeFile := (ExtractFileExtEx(sDefaultPath) = EXT_EXE);
    if isExeFile then
      intHash := GetFileXXHash32(sDefaultPath);

    //Check CRC for only .exe, if it fails reset cache icon
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
var
  sExt, sNameFile: string;
begin
  Result := '';

  sExt := ExtractFileExtEx(GetDefaultPathIcon);
  if ((sExt = EXT_EXE) or (sExt = EXT_ICO) or (sExt = '')) then
    sNameFile := Self.Name
  else
    sNameFile := Copy(sExt, 2, Length(sExt) - 1);

  if (sNameFile <> '') then
    Result := ASuiteInstance.Paths.SuitePathCache + sNameFile + EXT_ICO;
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
    if (Self.Name <> '') and (ExtractFileExtEx(APath) <> EXT_ICO) and
       ((not FTempItem) or (FTempItem and (ExtractFileExtEx(APath) <> EXT_EXE))) then
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
var
  Images: TCustomImageListResolution;
  bmpTemp: Graphics.TBitmap;
begin
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
end;

function TBaseIcon.LoadFromFileIcon(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  Icon : TBGRAIconCursor;
begin
  Result := nil;

  if not FileExists(APathFile) and (ExtractFileExtEx(APathFile) = EXT_ICO) then
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

function TBaseIcon.InternalGetImageIndex(const APathFile: string): Integer;
var
  bmpSmallIcon, bmpLargeIcon: TBGRABitmap;
begin
  Result := -1;

  //Get icon and insert it in ASuite ImageList
  bmpLargeIcon := GetIconFromFile(APathFile, True);
  bmpSmallIcon := GetIconFromFile(APathFile, False);
  try
    Result := dmImages.AddMultipleResolutions([bmpSmallIcon.Bitmap, bmpLargeIcon.Bitmap]);
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
