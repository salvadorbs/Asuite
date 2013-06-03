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
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    function LoadASuiteIconFromFile(const IconName: String): Integer;
    procedure SaveCacheIcon(Path: string; NodeData: TvCustomRealNodeData; ImageIndex: Integer);
    procedure InternalGetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree;Node: PVirtualNode);
  public
    { Public declarations }
    function GetIconIndex(NodeData:TvCustomRealNodeData): Integer;
    procedure DeleteCacheIcon(NodeData: TvCustomRealNodeData);
    function GetSimpleIconIndex(xpath : string): integer;
    procedure GetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree; Node: PVirtualNode;
                                 UseThread: Boolean = True);
  end;

  TGetNodeIconsThread = class(TThread)
  private
    { private declarations }
    FMainTree: TBaseVirtualTree;
    FSubTree: TBaseVirtualTree;
    FNode: PVirtualNode;
  public
    { public declarations }
    constructor Create(Suspended: Boolean;MainTree, SubTree: TBaseVirtualTree;Node: PVirtualNode);
    Procedure Execute; override;
  end;

var
  ImagesDM: TImagesDM;

implementation

uses
  AppConfig, ulAppConfig, ulTreeView, Main;

{$R *.dfm}

procedure TImagesDM.GetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree; Node: PVirtualNode;
                                       UseThread: Boolean = True);
var
  GetNodeIconsThread: TGetNodeIconsThread;
begin
  if UseThread then
    GetNodeIconsThread := TGetNodeIconsThread.Create(False, MainTree, SubTree, Node)
  else
    InternalGetChildNodesIcons(MainTree, SubTree, Node);
end;

function TImagesDM.GetIconIndex(NodeData:TvCustomRealNodeData): Integer;
var
  TempPath : String;
begin
  Result := -1;
  //Priority cache->icon->exe
  //Check cache icon
  if Config.ASuiteState <> asImporting then
  begin
    if Not(FileExists(NodeData.PathCacheIcon)) and (NodeData.CacheID <> -1) then
      NodeData.CacheID := -1;
    if (Config.Cache) and FileExists(NodeData.PathCacheIcon) then
        TempPath := NodeData.PathCacheIcon;
  end;
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
  //Save icon cache
  if Config.ASuiteState <> asImporting then
    SaveCacheIcon(TempPath, NodeData, Result);
end;

procedure TImagesDM.DeleteCacheIcon(NodeData: TvCustomRealNodeData);
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

procedure TImagesDM.SaveCacheIcon(Path: string; NodeData: TvCustomRealNodeData; ImageIndex: Integer);
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
    ShowMessage(Format(msgErrNoIcon, [FileName]),true);
end;

function TImagesDM.GetSimpleIconIndex(xpath: string): integer;
var
  FileInfo : TSHFileInfo;
begin
 //Get icon
  Result := -1;
  if SHGetFileInfo(PChar(xpath), 0, FileInfo, SizeOf(TSHFileInfo),
        SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES) <> 0
  then
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
  //Menu icons
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
end;

procedure TImagesDM.InternalGetChildNodesIcons(MainTree, SubTree: TBaseVirtualTree;
  Node: PVirtualNode);
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
    if Assigned(NodeData) and (NodeData.ImageIndex = -1) and (NodeData.DataType <> vtdtSeparator) then
      NodeData.ImageIndex := ImagesDM.GetIconIndex(TvCustomRealNodeData(NodeData));
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
  Node: PVirtualNode);
begin
  inherited Create(Suspended);

  //Set thread's variables
  Self.FMainTree := MainTree;
  Self.FSubTree  := SubTree;
  Self.FNode     := Node;

  Self.Priority  := tpNormal;
  Self.FreeOnTerminate := True;
end;

procedure TGetNodeIconsThread.Execute;
begin
  inherited;
  ImagesDM.InternalGetChildNodesIcons(FMainTree, FSubTree, FNode);
end;

end.

