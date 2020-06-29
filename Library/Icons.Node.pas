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

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Icons.Base, NodeDataTypes.Base, Kernel.Enumerations,
  NodeDataTypes.Custom, Graphics, Controls, KIcon, CommCtrl, LCLIntf, LCLType, LMessages{, IOUtils};

type
  TNodeIcon = class(TBaseIcon)
  private
    { private declarations }
    FNodeData: TvBaseNodeData;
    FCacheIconCRC: Integer;

    function InternalLoadIcon: Integer;
    function GetPathCacheIcon: string;
    procedure SaveCacheIcon(const APath: string; const AImageIndex: Integer);
    procedure ExtractAndAddIcon(AImageList: TImageList; AImageIndex: Integer; AIcon: TKIcon);
  public
    { public declarations }
    constructor Create(ANodeData: TvBaseNodeData);

    property PathCacheIcon: string read GetPathCacheIcon;
    property CacheIconCRC: Integer read FCacheIconCRC;

    function LoadIcon: Integer; override;
    procedure ResetIcon; override;
    procedure ResetCacheIcon;
    procedure SetCacheCRC(ACRC: Integer);
  end;

implementation

uses
  Utility.System, AppConfig.Main, NodeDataTypes.Files, Kernel.Consts,
  Utility.FileFolder, DataModules.Icons, Windows;

{ TNodeIcon }

constructor TNodeIcon.Create(ANodeData: TvBaseNodeData);
begin
  inherited Create;
  FNodeData     := ANodeData;
  FCacheIconCRC := 0;
end;

procedure TNodeIcon.ExtractAndAddIcon(AImageList: TImageList; AImageIndex: Integer; AIcon: TKIcon);
var
  KIcon: TKIcon;
  hIcon: Windows.HICON;
begin
  Assert(Assigned(AIcon), 'Icon is not assigned!');

  KIcon := TKIcon.Create;
  try
    //Bug: TBitmap doesn't support alpha format
    //Workaround: Get HIcon from ImageList and load it in a TKIcon
    hIcon := ImageList_GetIcon(AImageList.ResolutionByIndex[0].Reference.Handle, AImageIndex, ILD_NORMAL);
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
    //Check CRC, if it fails reset cache icon
    if (CacheIconCRC = GetFileCRC32(PathCacheIcon)) then
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
      if FNodeData.DataType = vtdtFile then
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
  if (FNodeData.DataType = vtdtCategory) and (Result = -1) then
    Result := Config.IconsManager.GetIconIndex('category');
end;

function TNodeIcon.LoadIcon: Integer;
begin
  Result := -1;
  //Check file path and if it isn't found, get fileicon_error and exit
  if FNodeData.DataType = vtdtFile then
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
        //Extract and insert icons in TKIcon
        ExtractAndAddIcon(dmImages.ilSmallIcons, AImageIndex, Icon);
        ExtractAndAddIcon(dmImages.ilLargeIcons, AImageIndex, Icon);
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

procedure TNodeIcon.SetCacheCRC(ACRC: Integer);
begin
  if ACRC <> 0 then
    FCacheIconCRC := ACRC;
end;

end.

