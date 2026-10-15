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

unit Icons.Manager;

{$MODE DelphiUnicode}

{$I ASuite.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Icons.Files, Generics.Collections, SyncObjs,
  Kernel.Consts, LCLIntf, Icons.Base, Icons.ExtFile, BGRABitmap;

type
  TBaseIcons = class(TObjectList<TBaseIcon>);

  { TIconsManager }

  TIconsManager = class
  private
    { private declarations }
    FPathTheme: string;
    FItems: TBaseIcons;
    FExtItems: TBaseIcons;
    FLock: SyncObjs.TCriticalSection;
    FByName: TDictionary<string, TBaseIcon>;
    FByPath: TDictionary<string, TFileIcon>;
    FByExt: TDictionary<string, TExtFileIcon>;
    function GetPathTheme: string;

    procedure LoadAllIcons;
    procedure SetPathTheme(const Value: string);
    procedure RegisterIcon(ABaseIcon: TBaseIcon);
    procedure RegisterExtIcon(ABaseIcon: TBaseIcon);
    procedure RebuildIndexes;
  public
    { public declarations }
    constructor Create;
    destructor Destroy; override;

    procedure Clear(AOnlyStaticItems: Boolean);
    function GetIconIndex(AName: string): Integer;
    function GetPathIconIndex(APathIcon: string): Integer;
    function GetExtIconIndex(AExtension: string): Integer;
    procedure AddIcon(ABaseIcon: TBaseIcon);
    procedure AddExtIcon(ABaseIcon: TBaseIcon);
    function FindByPath(APathFile: String): TFileIcon;
    function FindByName(AName: String): TBaseIcon;
    function FindByExt(AExtension: String): TExtFileIcon;   
    function GetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean
      ): TBGRABitmap;

    property PathTheme: string read GetPathTheme write SetPathTheme;
  end;

implementation

uses
  Kernel.Logger, FileUtil, LazFileUtils, Kernel.Instance, ImgList,
  Graphics, DataModules.Icons, mormot.core.log, SystemIcons.Ext;

{ TIconsManager }

constructor TIconsManager.Create;
begin
  FItems := TBaseIcons.Create(True);
  FExtItems := TBaseIcons.Create(True);
  FLock := SyncObjs.TCriticalSection.Create;
  FByName := TDictionary<string, TBaseIcon>.Create;
  FByPath := TDictionary<string, TFileIcon>.Create;
  FByExt := TDictionary<string, TExtFileIcon>.Create;
end;

destructor TIconsManager.Destroy;
begin
  FByName.Free;
  FByPath.Free;
  FByExt.Free;
  FItems.Free;
  FExtItems.Free;
  FLock.Free;

  inherited;
end;

procedure TIconsManager.Clear(AOnlyStaticItems: Boolean);
var
  I: Integer;
begin
  FLock.Acquire;
  try
    if AOnlyStaticItems then
    begin
      for I := FItems.Count - 1 downto 0 do
        if FItems[I].Static then
          FItems.Delete(I);
    end
    else
      FItems.Clear;
    RebuildIndexes;
  finally
    FLock.Release;
  end;
end;

function TIconsManager.GetIconIndex(AName: string): Integer;
var
  Icon: TBaseIcon;
begin
  Result := -1;

  Icon := FindByName(AName);

  if Assigned(Icon) then
    Result := Icon.ImageIndex;
end;

function TIconsManager.GetPathIconIndex(APathIcon: string): Integer;
var
  Icon: TFileIcon;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TIconsManager.GetPathIconIndex', Self);
  Result := -1;

  //Double-checked: fast lookup under lock, create placeholder if missing
  //so concurrent threads do not create duplicates.
  FLock.Acquire;
  try
    if FByPath.TryGetValue(APathIcon, Icon) then
    begin
      // Found under lock, load outside lock below
    end
    else begin
      Icon := TFileIcon.Create(APathIcon);
      FItems.Add(Icon);
      RegisterIcon(Icon);
    end;
  finally
    FLock.Release;
  end;

  if Assigned(Icon) then
    Result := Icon.ImageIndex;
end;

function TIconsManager.GetExtIconIndex(AExtension: string): Integer;
var
  Icon: TExtFileIcon;
  sKey: string;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TIconsManager.GetExtIconIndex', Self);
  Result := -1;

  sKey := string(SystemIcons.Ext.NormalizeExtKey(AnsiString(AExtension)));
  if sKey = '' then
    Exit;
  FLock.Acquire;
  try
    if FByExt.TryGetValue(sKey, Icon) then
    begin
      // Found, load outside lock
    end
    else begin
      Icon := TExtFileIcon.Create(AExtension);
      FExtItems.Add(Icon);
      RegisterExtIcon(Icon);
    end;
  finally
    FLock.Release;
  end;

  if Assigned(Icon) then
    Result := Icon.ImageIndex;
end;

procedure TIconsManager.AddIcon(ABaseIcon: TBaseIcon);
begin
  Assert(Assigned(ABaseIcon), 'ABaseIcon is not assigned!');

  //Lazy: do not extract the icon here, it will be loaded on first ImageIndex
  //access. This also keeps the lock short and avoids duplicates.
  FLock.Acquire;
  try
    FItems.Add(ABaseIcon);
    RegisterIcon(ABaseIcon);
  finally
    FLock.Release;
  end;
end;

procedure TIconsManager.AddExtIcon(ABaseIcon: TBaseIcon);
begin
  Assert(Assigned(ABaseIcon), 'ABaseIcon is not assigned!');

  //Lazy like AddIcon: the icon is extracted on first ImageIndex access.
  //Previously this forced extraction here, holding no lock during I/O now.
  FLock.Acquire;
  try
    FExtItems.Add(ABaseIcon);
    RegisterExtIcon(ABaseIcon);
  finally
    FLock.Release;
  end;
end;

function TIconsManager.GetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean): TBGRABitmap;
var
  Images: TCustomImageListResolution;
  bmpTemp: Graphics.TBitmap;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TIconsManager.GetIconFromImgList', Self);
  Result := nil;

  if not Assigned(dmImages.ilIcons) then
    Exit;

  bmpTemp := Graphics.TBitmap.Create;
  try
    if ALargeIcon then
      dmImages.ilIcons.FindResolution(ICON_SIZE_LARGE, Images)
    else
      dmImages.ilIcons.FindResolution(ICON_SIZE_SMALL, Images);

    Assert(Assigned(Images), 'Images is not assigned!');
         
    FLock.Acquire;
    try
      Images.GetBitmap(AImageIndex, bmpTemp);
    finally  
      FLock.Release;
    end;                                

    Result := TBGRABitmap.Create(bmpTemp);
  finally
    bmpTemp.Free;
  end;
end;

function TIconsManager.GetPathTheme: string;
begin
  if FPathTheme <> '' then
    Result := FPathTheme
  else
    Result := ASuiteInstance.Paths.SuitePathCurrentTheme;
end;

procedure TIconsManager.LoadAllIcons;
var
  Icon: TBaseIcon;
  sPath: string;
  IconFiles: TStringList;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TIconsManager.LoadAllIcons', Self);
  TASuiteLogger.Info('Search and load all icons in folder "%s"', [FPathTheme + ICONS_DIR]);

  Clear(True);
  //Load all icons in FPathTheme + ICONS_DIR
  if DirectoryExists(FPathTheme + ICONS_DIR) then
  begin
    IconFiles := FileUtil.FindAllFiles(FPathTheme + ICONS_DIR, '*' + EXT_ICO);
    try
      //Add new icon in FItems
      for sPath in IconFiles do
      begin
        Icon := FindByName(ExtractFileNameOnly(sPath));

        if not(Assigned(Icon)) then
          AddIcon(TFileIcon.Create(sPath, True));
      end;
    finally
      IconFiles.Free;
    end;
  end;
end;

procedure TIconsManager.SetPathTheme(const Value: string);
begin
  FPathTheme := value;
  LoadAllIcons;
end;

function TIconsManager.FindByPath(APathFile: String): TFileIcon;
begin
  Result := nil;
  FLock.Acquire;
  try
    FByPath.TryGetValue(APathFile, Result);
  finally
    FLock.Release;
  end;
end;

function TIconsManager.FindByName(AName: String): TBaseIcon;
var
  sKey: string;
begin
  Result := nil;
  // Theme lookups use filename-only names; keep case-insensitive like before
  // (TFileIcon.GetName already lowercases, be tolerant here too).
  sKey := LowerCase(AName);
  FLock.Acquire;
  try
    if not FByName.TryGetValue(sKey, Result) then
      FByName.TryGetValue(AName, Result);
  finally
    FLock.Release;
  end;
end;

function TIconsManager.FindByExt(AExtension: String): TExtFileIcon;
var
  sKey: string;
begin
  Result := nil;
  sKey := string(SystemIcons.Ext.NormalizeExtKey(AnsiString(AExtension)));
  if sKey = '' then
    Exit;
  FLock.Acquire;
  try
    FByExt.TryGetValue(sKey, Result);
  finally
    FLock.Release;
  end;
end;

procedure TIconsManager.RegisterIcon(ABaseIcon: TBaseIcon);
var
  sKey: string;
begin
  // Caller must hold FLock
  if ABaseIcon is TFileIcon then
  begin
    if TFileIcon(ABaseIcon).PathFile <> '' then
      FByPath.AddOrSetValue(TFileIcon(ABaseIcon).PathFile, TFileIcon(ABaseIcon));
  end;
  sKey := LowerCase(ABaseIcon.Name);
  if sKey <> '' then
    FByName.AddOrSetValue(sKey, ABaseIcon);
end;

procedure TIconsManager.RegisterExtIcon(ABaseIcon: TBaseIcon);
var
  sKey: string;
begin
  // Caller must hold FLock
  if not (ABaseIcon is TExtFileIcon) then
    Exit;
  sKey := LowerCase(ABaseIcon.Name);
  if sKey <> '' then
    FByExt.AddOrSetValue(sKey, TExtFileIcon(ABaseIcon));
end;

procedure TIconsManager.RebuildIndexes;
var
  Item: TBaseIcon;
begin
  // Caller must hold FLock
  FByName.Clear;
  FByPath.Clear;
  for Item in FItems do
    RegisterIcon(Item);
  FByExt.Clear;
  for Item in FExtItems do
    RegisterExtIcon(Item);
end;

end.
