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

unit Icons.Base;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Controls, SyncObjs, LCLIntf, LCLType, BGRAIconCursor,
  BGRABitmap;

type
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
    function GetFileXXHash32(const FileName: String): Integer;
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
    function CheckImageIndex: Boolean;

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
   AppConfig.Main, Kernel.Enumerations, Utility.System, Kernel.Instance,
   Kernel.Manager, LazFileUtils, HlpHashFactory, Kernel.Logger, FileUtil,
   Kernel.ResourceStrings;

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
    isExeFile := (ExtractLowerFileExt(sDefaultPath) = EXT_EXE);
    if isExeFile then
      intHash := GetFileXXHash32(sDefaultPath);

    //Check CRC for only .exe, if it fails reset cache icon
    sPathCacheIcon := PathCacheIcon;
    if (FileExists(sPathCacheIcon)) and (CacheIconCRC = intHash) then
      sTempPath := sPathCacheIcon
    else begin
      TASuiteLogger.Debug('sPathCacheIcon = %s - CacheIconCRC = %d - intHash = %d', [sPathCacheIcon, CacheIconCRC, intHash]);
      ResetCacheIcon;
    end;
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

  sExt := ExtractLowerFileExt(GetDefaultPathIcon);
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
    if (Self.Name <> '') and (ExtractLowerFileExt(APath) <> EXT_ICO) and
       ((not FTempItem) or (FTempItem and (ExtractLowerFileExt(APath) <> EXT_EXE))) then
    begin
      Icon := TBGRAIconCursor.Create(ifIco);
      try
        //Extract and insert icons in TIcon
        bmpLargeIcon := ASuiteManager.IconsManager.GetIconFromImgList(AImageIndex, True);
        bmpSmallIcon := ASuiteManager.IconsManager.GetIconFromImgList(AImageIndex, False);

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

function TBaseIcon.GetFileXXHash32(const FileName: String): Integer;
begin
  Result := 0;

  try
    if (FileName <> '') and not IsUNCPath(FileName) and FileExists(FileName) and (FileUtil.FileSize(FileName) > 0) then
      Result := THashFactory.THash32.CreateXXHash32().ComputeFile(FileName).GetInt32();
  except
    on E : Exception do
      TASuiteLogger.Exception(E, Format(msgGenerateFileHashError, [FileName]));
  end;
end;

function TBaseIcon.LoadFromFileIcon(const APathFile: string;
  const AWantLargeIcon: Boolean): TBGRABitmap;
var
  Icon : TBGRAIconCursor;
begin
  Result := nil;

  if not FileExists(APathFile) and (ExtractLowerFileExt(APathFile) = EXT_ICO) then
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

function TBaseIcon.CheckImageIndex: Boolean;
begin
  Result := FImageIndex <> -1;
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
var
  strPathCacheIcon: String;
begin
  strPathCacheIcon := PathCacheIcon;
  TASuiteLogger.Debug('Reset cache icon %s', [PathCacheIcon]);

  //Small icon cache
  if FileExists(PathCacheIcon) then
    SysUtils.DeleteFile(PathCacheIcon);

  FCacheIconCRC := 0;
end;

end.
