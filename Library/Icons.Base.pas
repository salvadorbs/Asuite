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
  BGRABitmap, Icons.Services;

type
  { TBaseIcon }

  TBaseIcon = class
  private
    FCacheIconCRC: Integer;
    FLock: SyncObjs.TCriticalSection;
    FStatic: Boolean;
    FImageIndex: Integer;
    FTempItem: Boolean;
    FCachePolicy: ICachePolicy;
    FFileOps: IFileOps;

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
    function GetDefaultPathIcon: string; virtual;
    function GetIconFromFile(const APathFile: string; const AWantLargeIcon: Boolean
      ): TBGRABitmap; virtual; abstract;
    function LoadFromFileIcon(const APathFile: string; const AWantLargeIcon: Boolean
      ): TBGRABitmap;
    // Seams for tests: defaults use Config/dmImages/ASuiteInstance globals
    function DoUseCache: Boolean; virtual;
    function DoIsImporting: Boolean; virtual;
    function DoFileExists(const APath: string): Boolean; virtual;
    function DoPathExists(const APath: string): Boolean; virtual;
    function DoGetCacheDir: string; virtual;
    function DoAddMultipleResolutions(const ASmall, ALarge: TBGRABitmap): Integer; virtual;
    function DoGetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean): TBGRABitmap; virtual;
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
    property CachePolicy: ICachePolicy read FCachePolicy write FCachePolicy;
    property FileOps: IFileOps read FFileOps write FFileOps;
  end;

implementation

uses
   DataModules.Icons, Kernel.Consts, BGRABitmapTypes, Utility.FileFolder,
   AppConfig.Main, Kernel.Enumerations, Utility.System, Kernel.Instance,
   Kernel.Manager, LazFileUtils, HlpHashFactory, Kernel.Logger, FileUtil,
   Kernel.ResourceStrings, Icons.CacheKey;

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
  sTempPath := '';

  sDefaultPath := GetDefaultPathIcon;

  //Get cache icon path
  if DoUseCache and not DoIsImporting then
  begin
    //Check CRC for only .exe, if it fails reset cache icon
    isExeFile := (ExtractLowerFileExt(sDefaultPath) = EXT_EXE);
    sPathCacheIcon := PathCacheIcon;
    if isExeFile and (sPathCacheIcon <> '') and DoFileExists(sPathCacheIcon) then
    begin
      intHash := GetFileXXHash32(sDefaultPath);

      if (CacheIconCRC = intHash) then
        sTempPath := sPathCacheIcon
      else begin
        TASuiteLogger.Debug('sPathCacheIcon = %s - CacheIconCRC = %d - intHash = %d', [sPathCacheIcon, CacheIconCRC, intHash]);
        ResetCacheIcon;
      end;
    end;
  end;

  //Icon or exe
  if not DoFileExists(sTempPath) then
    sTempPath := sDefaultPath;

  //Get image index
  if (sTempPath <> '') then
  begin
    if DoPathExists(sTempPath) then
      Result := InternalGetImageIndex(sTempPath);
    //Save icon cache
    if not DoIsImporting then
      SaveCacheIcon(sTempPath, Result, intHash);
  end;
end;

function TBaseIcon.GetPathCacheIcon: string;
var
  sCacheFile: string;
begin
  Result := '';

  sCacheFile := Icons.CacheKey.BuildCacheFileName(Self.Name, GetDefaultPathIcon, FTempItem);
  if (sCacheFile <> '') then
    Result := DoGetCacheDir + sCacheFile;
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
  sCacheFile: string;
begin
  if DoUseCache and (AImageIndex <> -1) then
  begin
    if Icons.CacheKey.ShouldCacheIcon(Self.Name, APath, FTempItem) then
    begin
      sCacheFile := PathCacheIcon;
      if sCacheFile = '' then
        Exit;
      Icon := TBGRAIconCursor.Create(ifIco);
      try
        //Extract and insert icons in TIcon
        bmpLargeIcon := DoGetIconFromImgList(AImageIndex, True);
        bmpSmallIcon := DoGetIconFromImgList(AImageIndex, False);
        try
          if Assigned(bmpSmallIcon) and Assigned(bmpLargeIcon) then
          begin
            Icon.Add(bmpSmallIcon, 32);
            Icon.Add(bmpLargeIcon, 32);

            //Save file and get CRC from it
            Icon.SaveToFile(sCacheFile);
            FCacheIconCRC := AHash;
          end;
        finally
          bmpLargeIcon.Free;
          bmpSmallIcon.Free;
        end;
      finally
        Icon.Free;
      end;
    end;
  end;
end;

function TBaseIcon.GetFileXXHash32(const FileName: String): Integer;
begin
  Result := 0;

  try
    if (FileName <> '') and not IsUNCPath(FileName) and DoFileExists(FileName) and (FileUtil.FileSize(FileName) > 0) then
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

  if not DoFileExists(APathFile) or (ExtractLowerFileExt(APathFile) <> EXT_ICO) then
    Exit;

  Icon := TBGRAIconCursor.Create(ifIco);
  try
    try
      Icon.LoadFromFile(APathFile);
    except
      Exit;
    end;

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

function TBaseIcon.DoUseCache: Boolean;
begin
  if Assigned(FCachePolicy) then
    Result := FCachePolicy.UseCache
  else
    Result := Config.Cache;
end;

function TBaseIcon.DoIsImporting: Boolean;
begin
  if Assigned(FCachePolicy) then
    Result := FCachePolicy.IsImporting
  else
    Result := Config.ASuiteState = lsImporting;
end;

function TBaseIcon.DoFileExists(const APath: string): Boolean;
begin
  if Assigned(FFileOps) then
    Result := FFileOps.FileExists(APath)
  else
    Result := SysUtils.FileExists(APath);
end;

function TBaseIcon.DoPathExists(const APath: string): Boolean;
begin
  if Assigned(FFileOps) then
    Result := FFileOps.PathExists(APath)
  else
    Result := IsPathExists(APath);
end;

function TBaseIcon.DoGetCacheDir: string;
begin
  Result := ASuiteInstance.Paths.SuitePathCache;
end;

function TBaseIcon.DoAddMultipleResolutions(const ASmall, ALarge: TBGRABitmap): Integer;
begin
  Result := dmImages.AddMultipleResolutions([ASmall.Bitmap, ALarge.Bitmap]);
end;

function TBaseIcon.DoGetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean): TBGRABitmap;
begin
  Result := ASuiteManager.IconsManager.GetIconFromImgList(AImageIndex, ALargeIcon);
end;

function TBaseIcon.GetDefaultPathIcon: string;
begin
  Result := '';
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
    if Assigned(bmpSmallIcon) and Assigned(bmpLargeIcon) then
      Result := DoAddMultipleResolutions(bmpSmallIcon, bmpLargeIcon);
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
  TASuiteLogger.Debug('Reset cache icon %s', [strPathCacheIcon]);

  //Small icon cache
  if (strPathCacheIcon <> '') and DoFileExists(strPathCacheIcon) then
    SysUtils.DeleteFile(strPathCacheIcon);

  FCacheIconCRC := 0;
end;

end.
