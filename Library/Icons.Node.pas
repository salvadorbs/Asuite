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
  NodeDataTypes.Custom, Graphics, Controls, {$IFDEF MSWINDOWS} CommCtrl, {$ENDIF}
  LCLIntf, LCLType, BGRABitmap;

type

  { TNodeIcon }

  TNodeIcon = class(TBaseIcon)
  private
    { private declarations }
    FNodeData: TvBaseNodeData;

    function InternalLoadIcon: Integer;
    function GetPathCacheIcon: string;
    procedure SaveCacheIcon(const APath: string; const AImageIndex: Integer);
    function GetIconFromImgList(AImageList: TImageList; AImageIndex: Integer;
      ALargeIcon: Boolean): TBGRABitmap;
  protected
    function GetName: string; override;
    function LoadIcon: Integer; override;
  public
    { public declarations }
    constructor Create(ANodeData: TvBaseNodeData);

    property PathCacheIcon: string read GetPathCacheIcon;

    procedure ResetIcon; override;
    procedure ResetCacheIcon;
  end;

implementation

uses
  Utility.System, AppConfig.Main, NodeDataTypes.Files, Kernel.Consts, ImgList,
  Utility.FileFolder, DataModules.Icons{$IFDEF MSWINDOWS}, Windows {$ENDIF},
  BGRAIconCursor, BGRABitmapTypes;

{ TNodeIcon }

constructor TNodeIcon.Create(ANodeData: TvBaseNodeData);
begin
  inherited Create;
  FNodeData     := ANodeData;
end;

function TNodeIcon.GetIconFromImgList(AImageList: TImageList;
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

function TNodeIcon.GetName: string;
begin
  Result := '';
  if Assigned(FNodeData) then
    Result := IntToStr(FNodeData.ID);
end;

function TNodeIcon.GetPathCacheIcon: string;
begin
  Result := '';
  if FNodeData.ID <> -1 then
    Result := Config.Paths.SuitePathCache + IntToStr(FNodeData.ID) + EXT_ICO;
end;

function TNodeIcon.InternalLoadIcon: Integer;
var
  sTempPath, sPathAbsoluteIcon: string;
begin
  //Priority cache->icon->exe
  sTempPath := '';
  Result   := -1;

  //Get cache icon path
  if (Config.Cache) and (Config.ASuiteState <> lsImporting) then
  begin
    //Check if file exists
    if FileExists(PathCacheIcon) then
      sTempPath := PathCacheIcon
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

  FNodeData.Changed := True;
end;

procedure TNodeIcon.ResetIcon;
begin
  inherited;

  //Delete cache icon
  ResetCacheIcon;

  //Force MainTree repaint node
  if Assigned(FNodeData.PNode) then
    Config.MainTree.InvalidateNode(FNodeData.PNode);
end;

procedure TNodeIcon.SaveCacheIcon(const APath: string;
  const AImageIndex: Integer);
var
  Icon: TBGRAIconCursor;
  bmpLargeIcon, bmpSmallIcon: TBGRABitmap;
begin
  if (Config.Cache) and (AImageIndex <> -1) then
  begin
    if (FNodeData.ID <> -1) and (LowerCase(ExtractFileExt(APath)) <>  EXT_ICO) then
    begin
      Icon := TBGRAIconCursor.Create(ifIco);
      try
        //Extract and insert icons in TIcon
        bmpLargeIcon := GetIconFromImgList(dmImages.ilLargeIcons, AImageIndex, True);
        bmpSmallIcon := GetIconFromImgList(dmImages.ilLargeIcons, AImageIndex, False);

        Icon.Add(bmpSmallIcon, 32);
        Icon.Add(bmpLargeIcon, 32);

        //Save icon file
        Icon.SaveToFile(PathCacheIcon);

        FNodeData.Changed := True;
      finally
        Icon.Free;
        bmpLargeIcon.Free;
        bmpSmallIcon.Free;
      end;
    end;
  end;
end;

end.

