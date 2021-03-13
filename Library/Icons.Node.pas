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

unit Icons.Node;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Icons.Base, NodeDataTypes.Base, Kernel.Enumerations,
  NodeDataTypes.Custom, Graphics, Controls, KIcon, {$IFDEF MSWINDOWS} CommCtrl, {$ENDIF}
  LCLIntf, LCLType;

type
  {$IFNDEF MSWINDOWS}
  TKIcon = TIcon;
  {$ENDIF}

  { TNodeIcon }

  TNodeIcon = class(TBaseIcon)
  private
    { private declarations }
    FNodeData: TvBaseNodeData;
    FCacheIconCRC: Integer;

    function InternalLoadIcon: Integer;
    function GetPathCacheIcon: string;
    procedure SaveCacheIcon(const APath: string; const AImageIndex: Integer);
    procedure ExtractAndAddIcon(AImageList: TImageList; AImageIndex: Integer;
      AIcon: TKIcon; ALargeIcon: Boolean);
    procedure SetCacheIconCRC(AValue: Integer);
  public
    { public declarations }
    constructor Create(ANodeData: TvBaseNodeData);

    property PathCacheIcon: string read GetPathCacheIcon;
    property CacheIconCRC: Integer read FCacheIconCRC write SetCacheIconCRC;

    function LoadIcon: Integer; override;
    procedure ResetIcon; override;
    procedure ResetCacheIcon;
  end;

implementation

uses
  Utility.System, AppConfig.Main, NodeDataTypes.Files, Kernel.Consts, ImgList,
  Utility.FileFolder, DataModules.Icons{$IFDEF MSWINDOWS}, Windows {$ENDIF};

{ TNodeIcon }

constructor TNodeIcon.Create(ANodeData: TvBaseNodeData);
begin
  inherited Create;
  FNodeData     := ANodeData;
  FCacheIconCRC := 0;
end;

procedure TNodeIcon.ExtractAndAddIcon(AImageList: TImageList; AImageIndex: Integer; AIcon: TKIcon; ALargeIcon: Boolean);
{$IFDEF MSWINDOWS}
var
  KIcon: TKIcon;
  hIcon: Windows.HICON;
  Images: TCustomImageListResolution;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Assert(Assigned(AIcon), 'Icon is not assigned!');

  KIcon := TKIcon.Create;
  try
    //Bug: TBitmap doesn't support alpha format
    //Workaround: Get HIcon from ImageList and load it in a TKIcon
    if ALargeIcon then
      AImageList.FindResolution(ICON_LARGE, Images)
    else
      AImageList.FindResolution(ICON_SMALL, Images);

    hIcon := ImageList_GetIcon(Images.Reference.Handle, AImageIndex, ILD_NORMAL);

    if hIcon <> 0 then
    begin
      KIcon.LoadFromHandle(hIcon);
      DestroyIcon(hIcon);

      //Add extracted icon in AIcon
      if KIcon.IconCount > 0 then
        AIcon.Add(KIcon.Handles[0]);
    end;
  finally
    KIcon.Free;
  end;
{$ENDIF}
end;

procedure TNodeIcon.SetCacheIconCRC(AValue: Integer);
begin
  if FCacheIconCRC <> AValue then
    FCacheIconCRC := AValue;
end;

function TNodeIcon.GetPathCacheIcon: string;
begin
  Result := '';
  if FNodeData.ID <> -1 then
    Result := Config.Paths.SuitePathCache + IntToStr(FNodeData.ID) + EXT_ICO;
end;

function TNodeIcon.InternalLoadIcon: Integer;
var
  sTempPath, sPathAbsoluteIcon, sPathCacheIcon: string;
begin
  //Priority cache->icon->exe
  sTempPath := '';
  Result   := -1;

  //Get cache icon path
  if (Config.Cache) and (Config.ASuiteState <> lsImporting) then
  begin
    //Check CRC, if it fails reset cache icon
    sPathCacheIcon := PathCacheIcon;
    if (FileExists(sPathCacheIcon)) and (CacheIconCRC = GetFileCRC32(sPathCacheIcon)) then
      sTempPath := sPathCacheIcon
    else
      ResetCacheIcon;
  end;

  //Icon or exe
  if Not FileExists(sTempPath) then
  begin
    //Get custom icon path
    sPathAbsoluteIcon := TvCustomRealNodeData(FNodeData).PathAbsoluteIcon;
    if FileExists(sPathAbsoluteIcon) then
      sTempPath := sPathAbsoluteIcon
    else //Else exe (if nodedata is a file item)
      if FNodeData.IsFileItem then
        sTempPath := TvFileNodeData(FNodeData).PathAbsoluteFile;
  end;

  //Get image index
  if (sTempPath <> '') then
  begin
    if IsPathExists(sTempPath) then
      Result := InternalGetImageIndex(sTempPath);
    //Save icon cache
    if (Config.ASuiteState <> lsImporting) then
      SaveCacheIcon(sTempPath, Result);
  end;

  //If it is a category, get directly cat icon
  if (FNodeData.IsCategoryItem) and (Result = -1) then
    Result := Config.IconsManager.GetIconIndex('category');
end;

function TNodeIcon.LoadIcon: Integer;
begin
  Result := -1;
  if FNodeData.IsSeparatorItem then
    Exit;

  //Check file path and if it isn't found, get fileicon_error and exit
  if FNodeData.IsFileItem then
  begin
    if Not(TvFileNodeData(FNodeData).IsPathFileExists) then
      Result := Config.IconsManager.GetIconIndex('file_error');
  end;
  //Get imageindex
  if (Result = -1) then
    Result := InternalLoadIcon;
end;

procedure TNodeIcon.ResetCacheIcon;
begin
  //Small icon cache
  if FileExists(PathCacheIcon) then
    SysUtils.DeleteFile(PathCacheIcon);
  FCacheIconCRC := 0;

  FNodeData.Changed := True;
end;

procedure TNodeIcon.ResetIcon;
begin
  inherited;
  //Delete cache icon and reset CRC
  ResetCacheIcon;
  //Force MainTree repaint node
  if Assigned(FNodeData.PNode) then
    Config.MainTree.InvalidateNode(FNodeData.PNode);
end;

procedure TNodeIcon.SaveCacheIcon(const APath: string;
  const AImageIndex: Integer);
var
  Icon: TKIcon;
begin
  if (Config.Cache) and (AImageIndex <> -1) then
  begin
    if (FNodeData.ID <> -1) and (LowerCase(ExtractFileExt(APath)) <>  EXT_ICO) then
    begin
      Icon := TKIcon.Create;
      try
        //TODO: Save cache in multiple resolution in a single file ico
        //Extract and insert icons in TKIcon
        ExtractAndAddIcon(dmImages.ilLargeIcons, AImageIndex, Icon, True);
        ExtractAndAddIcon(dmImages.ilLargeIcons, AImageIndex, Icon, False);

        //Save file and get CRC from it
        Icon.SaveToFile(PathCacheIcon);
        FCacheIconCRC := GetFileCRC32(PathCacheIcon);

        FNodeData.Changed := True;
      finally
        Icon.Free;
      end;
    end;
  end;
end;

end.

