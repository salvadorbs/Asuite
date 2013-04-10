{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit udImages;

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, Dialogs, ulEnumerations,
  ulNodeDataTypes, ShellApi, CommCtrl, Vcl.ImgList;


type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;

  TResType = (rtBMP, rtPNG, rtICO);

  TImagesDM = class(TDataModule)
    IcoImages: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    function LoadASuiteIconFromFile(const IconName: String): Integer;
    procedure SaveCacheIcon(Path: string; NodeData: TvBaseNodeData; ImageIndex: Integer);
  public
    { Public declarations }
    function GetIconIndex(NodeData:TvBaseNodeData): Integer;
    procedure DeleteCacheIcon(NodeData: TvBaseNodeData);
    function GetSimpleIconIndex(xpath : string): integer;
  end;

var
  ImagesDM: TImagesDM;

implementation

uses
  AppConfig, ulAppConfig;

{$R *.dfm}

function TImagesDM.GetIconIndex(NodeData:TvBaseNodeData): Integer;
var
  TempPath : String;
begin
  Result := -1;
  //Priority cache->icon->exe
  //Check cache icon
  if Not(FileExists(NodeData.PathCacheIcon)) and (NodeData.CacheID <> -1) then
    NodeData.CacheID := -1;
  if (Config.Cache) and FileExists(NodeData.PathCacheIcon) then
      TempPath := NodeData.PathCacheIcon;
  //Icon
  if NodeData.CacheID = -1 then
  begin
    if FileExists(NodeData.PathAbsoluteIcon) then
      TempPath := NodeData.PathAbsoluteIcon
    else
      //Exe, if nodedata is a file item
      if NodeData.DataType = vtdtFile then
      begin
        if (FileExists((NodeData as TvFileNodeData).PathAbsoluteExe)) or
           (DirectoryExists((NodeData as TvFileNodeData).PathAbsoluteExe)) then
          TempPath := (NodeData as TvFileNodeData).PathAbsoluteExe;
      end
      else begin
        if NodeData.DataType = vtdtCategory then
          Result := IMAGE_INDEX_Cat;
      end;
  end;
  //Get image index
  if (TempPath <> '') and (Result <> IMAGE_INDEX_Cat) then
    Result := GetSimpleIconIndex(TempPath);
  SaveCacheIcon(TempPath, NodeData, Result);
end;

procedure TImagesDM.DeleteCacheIcon(NodeData: TvBaseNodeData);
begin
  //Check Cache icon
  if (Config.Cache) then
  begin
    if FileExists(NodeData.PathCacheIcon) then
      DeleteFile(PWideChar(NodeData.PathCacheIcon));
    NodeData.CacheID := -1;
    NodeData.Changed := True;
  end
end;

procedure TImagesDM.SaveCacheIcon(Path: string; NodeData: TvBaseNodeData; ImageIndex: Integer);
var
  I    : Integer;
  Icon : TIcon;
begin
  if (NodeData.CacheID = -1) and (Config.Cache) then
  begin
    //Save file cache-icon
    if (FileExists(Path)) then
      if (LowerCase(ExtractFileExt(Path)) <>  EXT_ICO) then
      begin
        I := 0;
        //Get first ID slot free
        while (FileExists(SUITE_CACHE_PATH + IntToStr(I) + EXT_ICO)) do
          Inc(I);
        //Save cache icon in disk
        Icon := TIcon.Create;
        try
          IcoImages.GetIcon(ImageIndex, Icon);
          if Icon.HandleAllocated then
          begin
            Icon.SaveToFile(SUITE_CACHE_PATH + IntToStr(I) + EXT_ICO);
            NodeData.CacheID := I;
            NodeData.Changed := True;
          end;
        finally
          Icon.Free;
        end;
      end
      else
        NodeData.CacheID := -1;
  end;
end;

function TImagesDM.LoadASuiteIconFromFile(const IconName: String): Integer;
var
  FileName : String;
begin
  Result   := -1;
  FileName := SUITE_ICONS_PATH + IconName;
  if FileExists(FileName) then
    Result := GetSimpleIconIndex(FileName)
  else
    ShowMessageFmt(msgErrNoIcon, [FileName]);
end;

function TImagesDM.GetSimpleIconIndex(xpath: string): integer;
var
  FileInfo : TSHFileInfo;
begin
  //Get icon
  SHGetFileInfo(PChar(xpath), 0, FileInfo, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or
                                                                SHGFI_SMALLICON or
                                                                SHGFI_USEFILEATTRIBUTES);
  Result := FileInfo.iIcon;
end;

procedure TImagesDM.DataModuleCreate(Sender: TObject);
var
  Flags: integer;
  SFI: TSHFileInfo;
begin
  //Use System ImageList
  Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;
  IcoImages.Handle      := SHGetFileInfo('', 0, SFI, SizeOf(SFI), Flags);
  IcoImages.ShareImages := True;
  //Menu icons
  IMAGE_INDEX_ASuite     := LoadASuiteIconFromFile(FILEICON_ASuite);
  IMAGE_INDEX_Cat        := LoadASuiteIconFromFile(FILEICON_Cat);
  IMAGE_INDEX_Help       := LoadASuiteIconFromFile(FILEICON_Help);
  IMAGE_INDEX_Options    := LoadASuiteIconFromFile(FILEICON_Options);
  IMAGE_INDEX_AddCat     := LoadASuiteIconFromFile(FILEICON_AddCat);
  IMAGE_INDEX_AddFile    := LoadASuiteIconFromFile(FILEICON_AddFile);
  IMAGE_INDEX_AddFolder  := LoadASuiteIconFromFile(FILEICON_AddFolder);
  IMAGE_INDEX_Delete     := LoadASuiteIconFromFile(FILEICON_Delete);
  IMAGE_INDEX_Property   := LoadASuiteIconFromFile(FILEICON_Property);
  IMAGE_INDEX_Save       := LoadASuiteIconFromFile(FILEICON_Save);
  IMAGE_INDEX_Folder     := LoadASuiteIconFromFile(FILEICON_Folder);
  IMAGE_INDEX_AddGroupFile := LoadASuiteIconFromFile(FILEICON_AddGroupFile);
  IMAGE_INDEX_GroupFile  := LoadASuiteIconFromFile(FILEICON_GroupFile);
  IMAGE_INDEX_NOTFOUND   := LoadASuiteIconFromFile(FILEICON_NOTFOUND);
  IMAGE_INDEX_Run        := LoadASuiteIconFromFile(FILEICON_Run);
  IMAGE_INDEX_Cut        := LoadASuiteIconFromFile(FILEICON_Cut);
  IMAGE_INDEX_Copy       := LoadASuiteIconFromFile(FILEICON_Copy);
  IMAGE_INDEX_Paste      := LoadASuiteIconFromFile(FILEICON_Paste);
  IMAGE_INDEX_Search     := LoadASuiteIconFromFile(FILEICON_Search);
  IMAGE_INDEX_SearchType := LoadASuiteIconFromFile(FILEICON_SearchType);
  IMAGE_INDEX_Url        := LoadASuiteIconFromFile(FILEICON_Url);
  IMAGE_INDEX_Accept     := LoadASuiteIconFromFile(FILEICON_Accept);
  IMAGE_INDEX_Cancel     := LoadASuiteIconFromFile(FILEICON_Cancel);
end;

end.
