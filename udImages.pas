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

unit udImages;

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, Dialogs, ulEnumerations,
  ulNodeDataTypes, ShellApi, CommCtrl, Vcl.ImgList, ulCommonUtils, VirtualTrees;

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
  public
    { Public declarations }
    function GetIconIndex(NodeData:TvCustomRealNodeData;SmallIcon: Boolean = True): Integer;
    function GetSimpleIconIndex(xpath : string;SmallIcon: Boolean = True): integer;
    procedure GetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree; Node: PVirtualNode;
                                 UseThread: Boolean = True; SmallIcon: Boolean = True);
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
  else begin
    InternalGetChildNodesIcons(MainTree, SubTree, Node, SmallIcon);
  end;
end;

function TImagesDM.GetIconIndex(NodeData:TvCustomRealNodeData;SmallIcon: Boolean = True): Integer;
var
  TempPath : String;
  TempCacheID : Integer;
  TempCachePath : string;
begin
  Result := -1;
  if SmallIcon then
  begin
    TempCacheID   := NodeData.CacheID;
    TempCachePath := NodeData.PathCacheIcon;
  end
  else begin
    TempCacheID   := NodeData.CacheLargeID;
    TempCachePath := NodeData.PathCacheLargeIcon;
  end;
  //Priority cache->icon->exe
  //Check cache icon
  if Config.ASuiteState <> asImporting then
  begin
    if Not(FileExists(TempCachePath)) and (TempCacheID <> -1) then
      TempCacheID := -1;
    if (Config.Cache) and FileExists(TempCachePath) then
      TempPath := TempCachePath;
  end;
  //Icon
  if TempCacheID = -1 then
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
    Result := GetSimpleIconIndex(TempPath, SmallIcon);
  //Save icon cache
  if Config.ASuiteState <> asImporting then
    SaveCacheIcon(TempPath, NodeData, Result, SmallIcon);
end;

procedure TImagesDM.SaveCacheIcon(Path: string; NodeData: TvCustomRealNodeData;
                                  ImageIndex: Integer;SmallIcon: Boolean);
var
  I    : Integer;
  Icon : TIcon;
  TempCacheID   : Integer;
  TempCachePath : string;
  TempImageList : TImageList;
begin
  //SmallIcon
  if SmallIcon then
  begin
    TempCacheID   := NodeData.CacheID;
    TempCachePath := SUITE_CACHE_PATH;
    TempImageList := IcoImages;
  end //Else large icon
  else begin
    TempCacheID   := NodeData.CacheLargeID;
    TempCachePath := SUITE_CACHELARGE_PATH;
    TempImageList := LargeIcoImages;
  end;

  if (TempCacheID = -1) and (Config.Cache) then
  begin
    //Save file cache-icon
    if (FileExists(Path)) then
      if (LowerCase(ExtractFileExt(Path)) <>  EXT_ICO) then
      begin
        I := 0;
        //Get first ID slot free
        while (FileExists(TempCachePath + IntToStr(I) + EXT_ICO)) do
          Inc(I);
        //Save cache icon in disk
        Icon := TIcon.Create;
        try
          TempImageList.GetIcon(ImageIndex, Icon);
          if Icon.HandleAllocated then
          begin
            Icon.SaveToFile(TempCachePath + IntToStr(I) + EXT_ICO);
            TempCacheID      := I;
            NodeData.Changed := True;
          end;
        finally
          Icon.Free;
        end;
      end
      else
        TempCacheID := -1;
    //If CacheID change, set property Changed to true
    if SmallIcon then
    begin
      if TempCacheID <> NodeData.CacheID then
        NodeData.Changed := True;
    end
    else
      if TempCacheID <> NodeData.CacheLargeID then
        NodeData.Changed := True;
    //Save TempCacheID
    if (TempCacheID <> -1) then
    begin
      if SmallIcon then
        NodeData.CacheID := TempCacheID
      else
        NodeData.CacheLargeID := TempCacheID;
    end;
  end;
end;

function TImagesDM.LoadASuiteIconFromFile(const IconName: String; SmallIcon: Boolean): Integer;
var
  FileName : String;
begin
  Result   := -1;
  if SmallIcon then
    FileName := SUITE_ICONS_PATH + IconName
  else
    FileName := SUITE_LARGEICONS_PATH + IconName;
  if FileExists(FileName) then
    Result := GetSimpleIconIndex(FileName, SmallIcon)
  else
    ShowMessage(Format(msgErrNoIcon, [FileName]),true);
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
    Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES
  else //Else large icon
    Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES;
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
  Flags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;
  IcoImages.Handle      := SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), Flags);
  IcoImages.ShareImages := True;
  Flags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES;
  LargeIcoImages.Handle      := SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), Flags);
  LargeIcoImages.ShareImages := True;
  //Menu small icons
  IMAGE_INDEX_Help       := LoadASuiteIconFromFile(FILEICON_Help);
  IMAGE_INDEX_Options    := LoadASuiteIconFromFile(FILEICON_Options);
  IMAGE_INDEX_AddCat     := LoadASuiteIconFromFile(FILEICON_AddCat);
  IMAGE_INDEX_AddFile    := LoadASuiteIconFromFile(FILEICON_AddFile);
  IMAGE_INDEX_AddFolder  := LoadASuiteIconFromFile(FILEICON_AddFolder);
  IMAGE_INDEX_Delete     := LoadASuiteIconFromFile(FILEICON_Delete);
  IMAGE_INDEX_ASuite     := LoadASuiteIconFromFile(FILEICON_ASuite);
  IMAGE_INDEX_Cat        := LoadASuiteIconFromFile(FILEICON_Cat);
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
  //Menu large icons
  IMAGELARGE_INDEX_General  := LoadASuiteIconFromFile(FILELARGEICON_General, false);
  IMAGELARGE_INDEX_Advanced := LoadASuiteIconFromFile(FILELARGEICON_Advanced, false);
  IMAGELARGE_INDEX_Items    := LoadASuiteIconFromFile(FILELARGEICON_Items, false);
  IMAGELARGE_INDEX_Hotkey   := LoadASuiteIconFromFile(FILELARGEICON_Hotkey, false);
  IMAGELARGE_INDEX_Mouse    := LoadASuiteIconFromFile(FILELARGEICON_Mouse, false);
  IMAGELARGE_INDEX_Trayicon := LoadASuiteIconFromFile(FILELARGEICON_Trayicon, false);
  IMAGELARGE_INDEX_Stats    := LoadASuiteIconFromFile(FILELARGEICON_Stats, false);
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
      begin
        if (NodeData.ImageIndex = -1) then
          NodeData.ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(NodeData));
      end
      else begin
        if (NodeData.ImageLargeIndex = -1) then
          NodeData.ImageLargeIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(NodeData), False);
      end;
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
  if FSmallIcon then
    ImagesDM.InternalGetChildNodesIcons(FMainTree, FSubTree, FNode)
  else
    ImagesDM.InternalGetChildNodesIcons(FMainTree, FSubTree, FNode, False);
end;

end.

