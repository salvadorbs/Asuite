{
Copyright (C) 2006-2013 Matteo Salvi

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

unit DataModules.Images;

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, Dialogs, ulEnumerations,
  ulNodeDataTypes, ShellApi, CommCtrl, Vcl.ImgList, ulCommonUtils, VirtualTrees,
  DKLang;

type
  TRGBArray = array[Word] of TRGBTriple;
  pRGBArray = ^TRGBArray;

  TResType = (rtBMP, rtPNG, rtICO);

  TImagesDM = class(TDataModule)
    IcoImages: TImageList;
    LargeIcoImages: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    function LoadASuiteIconFromFile(const IconName: String;SmallIcon: Boolean = True): Integer;
    procedure SaveCacheIcon(Path: string; NodeData: TvCustomRealNodeData;
                            ImageIndex: Integer;SmallIcon: Boolean);
    procedure InternalGetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree;
                                         Node: PVirtualNode;SmallIcon: Boolean = True);
    function InternalGetIconIndex(NodeData:TvCustomRealNodeData;
                                  TempCachePath: string;SmallIcon: Boolean): Integer;
    function SaveIconToFile(ImageIndex: Integer; TempCachePath: string; ImageList: TImageList): Integer;
  public
    { Public declarations }
    function GetIconIndex(NodeData:TvCustomRealNodeData;SmallIcon: Boolean = True): Integer;
    function GetSimpleIconIndex(xpath : string;SmallIcon: Boolean = True): integer;
    procedure GetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree; Node: PVirtualNode;
                                 UseThread: Boolean = True; SmallIcon: Boolean = True);
    procedure LoadASuiteIcons;
  end;

  TGetNodeIconsThread = class(TThread)
  private
    { private declarations }
    FMainTree  : TBaseVirtualTree; //frmMain.vstList
    FSubTree   : TBaseVirtualTree;
    FNode      : PVirtualNode;
    FSmallIcon : Boolean;
  public
    { public declarations }
    constructor Create(Suspended: Boolean;MainTree, SubTree: TBaseVirtualTree;
                       Node: PVirtualNode;SmallIcon: Boolean = True);
    Procedure Execute; override;
  end;

var
  ImagesDM: TImagesDM;

implementation

uses
  AppConfig, ulAppConfig, ulTreeView, Main;

{$R *.dfm}

procedure TImagesDM.GetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree; Node: PVirtualNode;
                                       UseThread: Boolean = True; SmallIcon: Boolean = True);
begin
  if UseThread then
    TGetNodeIconsThread.Create(False, MainTree, SubTree, Node, SmallIcon)
  else
    InternalGetChildNodesIcons(MainTree, SubTree, Node, SmallIcon);
end;

procedure TImagesDM.LoadASuiteIcons;
begin
  //Menu small icons
  IMAGE_INDEX_Help     := LoadASuiteIconFromFile(FILEICON_Help);
  IMAGE_INDEX_Options  := LoadASuiteIconFromFile(FILEICON_Options);
  IMAGE_INDEX_AddCat   := LoadASuiteIconFromFile(FILEICON_AddCat);
  IMAGE_INDEX_AddFile  := LoadASuiteIconFromFile(FILEICON_AddFile);
  IMAGE_INDEX_AddFolder := LoadASuiteIconFromFile(FILEICON_AddFolder);
  IMAGE_INDEX_Delete   := LoadASuiteIconFromFile(FILEICON_Delete);
  IMAGE_INDEX_ASuite   := LoadASuiteIconFromFile(FILEICON_ASuite);
  IMAGE_INDEX_Property := LoadASuiteIconFromFile(FILEICON_Property);
  IMAGE_INDEX_Save     := LoadASuiteIconFromFile(FILEICON_Save);
  IMAGE_INDEX_Folder   := LoadASuiteIconFromFile(FILEICON_Folder);
  IMAGE_INDEX_NOTFOUND := LoadASuiteIconFromFile(FILEICON_NOTFOUND);
  IMAGE_INDEX_Run      := LoadASuiteIconFromFile(FILEICON_Run);
  IMAGE_INDEX_Cut      := LoadASuiteIconFromFile(FILEICON_Cut);
  IMAGE_INDEX_Copy     := LoadASuiteIconFromFile(FILEICON_Copy);
  IMAGE_INDEX_Paste    := LoadASuiteIconFromFile(FILEICON_Paste);
  IMAGE_INDEX_Search   := LoadASuiteIconFromFile(FILEICON_Search);
  IMAGE_INDEX_SearchType := LoadASuiteIconFromFile(FILEICON_SearchType);
  IMAGE_INDEX_Url      := LoadASuiteIconFromFile(FILEICON_Url);
  IMAGE_INDEX_Accept   := LoadASuiteIconFromFile(FILEICON_Accept);
  IMAGE_INDEX_Cancel   := LoadASuiteIconFromFile(FILEICON_Cancel);
  IMAGE_INDEX_Cat      := LoadASuiteIconFromFile(FILEICON_Cat);
  //Menu large icons
  IMAGELARGE_INDEX_General  := LoadASuiteIconFromFile(FILELARGEICON_General, false);
  IMAGELARGE_INDEX_Advanced := LoadASuiteIconFromFile(FILELARGEICON_Advanced, false);
  IMAGELARGE_INDEX_Items    := LoadASuiteIconFromFile(FILELARGEICON_Items, false);
  IMAGELARGE_INDEX_Hotkey   := LoadASuiteIconFromFile(FILELARGEICON_Hotkey, false);
  IMAGELARGE_INDEX_Mouse    := LoadASuiteIconFromFile(FILELARGEICON_Mouse, false);
  IMAGELARGE_INDEX_Trayicon := LoadASuiteIconFromFile(FILELARGEICON_Trayicon, false);
  IMAGELARGE_INDEX_Stats    := LoadASuiteIconFromFile(FILELARGEICON_Stats, false);
  IMAGELARGE_INDEX_Behavior := LoadASuiteIconFromFile(FILELARGEICON_Behavior, false);
  IMAGELARGE_INDEX_PropGeneral := LoadASuiteIconFromFile(FILELARGEICON_PropGeneral, false);
end;

function TImagesDM.SaveIconToFile(ImageIndex: Integer; TempCachePath: string; ImageList: TImageList): Integer;
var
  Icon: TIcon;
  ID: Integer;
begin
  Result := -1;
  ID := 0;
  //Save cache icon in disk
  Icon := TIcon.Create;
  try
    //Get first ID slot free
    while (FileExists(TempCachePath + IntToStr(ID) + EXT_ICO)) do
      Inc(ID);
    //Get icon and save to file
    ImageList.GetIcon(ImageIndex, Icon);
    if Icon.HandleAllocated then
      Icon.SaveToFile(TempCachePath + IntToStr(ID) + EXT_ICO)
    else
      ID := -1;
  finally
    Icon.Free;
    Result := ID;
  end;
end;

function TImagesDM.GetIconIndex(NodeData:TvCustomRealNodeData;SmallIcon: Boolean = True): Integer;
begin
  if SmallIcon then
  begin
    if (NodeData.ImageIndex = -1) then
      Result := InternalGetIconIndex(NodeData, NodeData.PathCacheIcon, SmallIcon)
    else
      Result := NodeData.ImageIndex;
  end
  else begin
    if (NodeData.ImageLargeIndex = -1) then
      Result := InternalGetIconIndex(NodeData, NodeData.PathCacheLargeIcon, SmallIcon)
    else
      Result := NodeData.ImageLargeIndex;
  end;
end;

procedure TImagesDM.SaveCacheIcon(Path: string; NodeData: TvCustomRealNodeData;
                                  ImageIndex: Integer;SmallIcon: Boolean);
begin
  //Save file cache-icon
  if (Config.Cache) and (LowerCase(ExtractFileExt(Path)) <>  EXT_ICO) then
  begin
    if (NodeData.CacheID = -1) and SmallIcon then
      NodeData.CacheID := SaveIconToFile(ImageIndex, SUITE_CACHE_PATH, IcoImages)
    else
      if (NodeData.CacheLargeID = -1) and Not(SmallIcon) then
        NodeData.CacheLargeID := SaveIconToFile(ImageIndex, SUITE_CACHELARGE_PATH, LargeIcoImages);
    //If cacheID changed, we need to set NodeData.Changed to true
    if (NodeData.CacheID <> -1) or (NodeData.CacheLargeID <> -1) then
      NodeData.Changed := True;
  end;
end;

function TImagesDM.LoadASuiteIconFromFile(const IconName: String; SmallIcon: Boolean): Integer;
var
  FileName : String;
begin
  Result   := -1;
  //Get icon path
  if SmallIcon then
    FileName := SUITE_SMALLICONS_PATH + IconName
  else
    FileName := SUITE_LARGEICONS_PATH + IconName;
  //Get icon index
  if FileExists(FileName) then
    Result := GetSimpleIconIndex(FileName, SmallIcon)
  else
    ShowMessageFmt(DKLangConstW('msgErrNoIcon'), [FileName],true);
end;

function TImagesDM.GetSimpleIconIndex(xpath: string;SmallIcon: Boolean = True): integer;
var
  FileInfo : TSHFileInfo;
  Flags: integer;
begin
  //Get icon
  Result := -1;
  //Small icon
  if SmallIcon then
    Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON
  else //Else large icon
    Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON;
  //Get index
  if SHGetFileInfo(PChar(xpath), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
    Result := FileInfo.iIcon;
end;

procedure TImagesDM.DataModuleCreate(Sender: TObject);
var
  Flags: integer;
  SFI: TSHFileInfo;
begin
  //Use System ImageList
  Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON;
  IcoImages.Handle      := SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), Flags);
  IcoImages.ShareImages := True;
  Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON;
  LargeIcoImages.Handle      := SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), Flags);
  LargeIcoImages.ShareImages := True;
end;

procedure TImagesDM.InternalGetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree;
  Node: PVirtualNode;SmallIcon: Boolean = True);
var
  NodeData: TvBaseNodeData;
  ChildNode: PVirtualNode;
  IsMainList: Boolean;
begin
  //If SubTree is nil, get child node's icons directly from MainTree
  IsMainList := Not(Assigned(SubTree));
  //Get items' icons
  ChildNode := MainTree.GetFirstChild(Node);
  while Assigned(ChildNode) do
  begin
    if IsMainList then
      NodeData := PBaseData(MainTree.GetNodeData(ChildNode)).Data
    else
      NodeData := PBaseData(GetNodeDataSearch(ChildNode,SubTree,MainTree)).Data;
    //Get imageindex
    if Assigned(NodeData) and (NodeData.DataType <> vtdtSeparator) then
    begin
      if SmallIcon then
        NodeData.ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(NodeData), SmallIcon)
      else
        NodeData.ImageLargeIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(NodeData), SmallIcon);
    end;
    //Repaint node
    if IsMainList then
      MainTree.InvalidateNode(ChildNode)
    else
      SubTree.InvalidateNode(ChildNode);
    //Next node
    ChildNode := MainTree.GetNextSibling(ChildNode);
  end;
end;

function TImagesDM.InternalGetIconIndex(NodeData: TvCustomRealNodeData;
  TempCachePath: string; SmallIcon: Boolean): Integer;
var
  TempPath: string;
begin
  //Priority cache->icon->exe
  TempPath := '';
  Result   := -1;
  //Check cache icon
  if Config.ASuiteState <> asImporting then
    if (Config.Cache) then
      TempPath := TempCachePath;
  //Icon
  if TempPath = '' then
  begin
    if FileExists(NodeData.PathAbsoluteIcon) then
      TempPath := NodeData.PathAbsoluteIcon
    else begin
      //Exe, if nodedata is a file item
      if NodeData.DataType = vtdtFile then
        TempPath := (NodeData as TvFileNodeData).PathAbsoluteExe
      else //Else get category icon
        if NodeData.DataType = vtdtCategory then
          Result := IMAGE_INDEX_Cat;
    end;
  end;
  //Get image index
  if (TempPath <> '') and (Result <> IMAGE_INDEX_Cat) then
  begin
    if FileExists(TempPath) or DirectoryExists(TempPath) then
    begin
      Result := GetSimpleIconIndex(TempPath, SmallIcon);
      //Save icon cache
      if (Config.ASuiteState <> asImporting) and (Result <> -1) then
        SaveCacheIcon(TempPath, NodeData, Result, SmallIcon);
    end;
  end;
end;

{ TGetNodeIconsThread }

constructor TGetNodeIconsThread.Create(Suspended: Boolean;MainTree, SubTree: TBaseVirtualTree;
  Node: PVirtualNode;SmallIcon: Boolean = True);
begin
  inherited Create(Suspended);

  //Set thread's variables
  Self.FMainTree  := MainTree;
  Self.FSubTree   := SubTree;
  Self.FNode      := Node;
  Self.FSmallIcon := SmallIcon;

  Self.Priority   := tpNormal;
  Self.FreeOnTerminate := True;
end;

procedure TGetNodeIconsThread.Execute;
begin
  inherited;
  ImagesDM.InternalGetChildNodesIcons(FMainTree, FSubTree, FNode, FSmallIcon)
end;

end.

