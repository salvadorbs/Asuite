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
  Kernel.Consts, LCLIntf, Icons.Base, Icons.ExtFile, BGRABitmap,
  SystemIcons.Mime;

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
    FMimeProvider: TMimeIconProvider;
    function GetPathTheme: string;

    procedure LoadAllIcons;
    procedure SetPathTheme(const Value: string);
    procedure RegisterIcon(ABaseIcon: TBaseIcon);
    procedure RegisterExtIcon(ABaseIcon: TBaseIcon);
    procedure RebuildIndexes;
    {$IFDEF UNIX}
    procedure EnsureMimeLoaded;
    function GetIconByDesktopFile(AFileName: String): String;
    {$ENDIF}
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
    {$IFDEF UNIX}
    function GetSystemIconName(const AFileName: String): String;
    function CheckSystemIconName(const AIconName: String): Boolean;
    {$ENDIF}

    property PathTheme: string read GetPathTheme write SetPathTheme;
  end;

implementation

uses
  Kernel.Logger, FileUtil, LazFileUtils, Kernel.Instance, ImgList,
  Graphics, DataModules.Icons, mormot.core.log, SystemIcons.Ext
  {$IFDEF UNIX}
  , IniFiles, BaseUnix, Utility.FileFolder
    {$IFDEF QT}
      {$IFDEF LCLQT5}
      , qt5
      {$ELSE}
      , qt6
      {$ENDIF}
    {$ELSE}   
      {$IFDEF LCLGTK2}
      , gtk2
      {$ELSE}
      , lazgtk3
      {$ENDIF}
    {$ENDIF}
  {$ENDIF};

{ TIconsManager }

constructor TIconsManager.Create;
begin
  FItems := TBaseIcons.Create(True);
  FExtItems := TBaseIcons.Create(True);
  FLock := SyncObjs.TCriticalSection.Create;
  FByName := TDictionary<string, TBaseIcon>.Create;
  FByPath := TDictionary<string, TFileIcon>.Create;
  FByExt := TDictionary<string, TExtFileIcon>.Create;
  FMimeProvider := TMimeIconProvider.Create;
  // MIME table is loaded lazily on first GetSystemIconName (see EnsureMimeLoaded)
  // to keep startup and unit tests fast.
end;

destructor TIconsManager.Destroy;
begin
  FMimeProvider.Free;
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

{$IFDEF UNIX}
function TIconsManager.GetSystemIconName(const AFileName: String): String;
var
  I: Integer;
  iconList: TStringList;
  Extension: String;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TIconsManager.GetSystemIconName', Self);
  Result := EmptyStr;
  EnsureMimeLoaded;

  //It is a link? Ok, get target file icon
  if FpReadLink(AFilename) <> EmptyStr then
    Extension := '*' + ExtractFileExt(FpReadLink(AFileName))
  else
    Extension := '*' + ExtractFileExt(AFileName);

  Extension := LowerCase(Extension);

  //TODO: Special folders icon https://gitlab.gnome.org/GNOME/glib/-/commit/129eb074823101102611690f053ffa246bb7784d#3549e1301fc4c17bf0dd809eca0a36fb87aac264_1582_1582

  if IsDirectory(AFileName) then
  begin
    if FileExists(AppendPathDelim(AFileName) + '.directory') then
      Result := GetIconByDesktopFile(AppendPathDelim(AFileName) + '.directory')
    else
      Result := 'folder';
  end
  else if (Extension = '*.desktop') then
  begin
    Result := GetIconByDesktopFile(AFileName);
  end
  else if (Extension = '*.ico') then
  begin
    Result := AFileName;
  end
  else if (Extension <> '*') then
  begin
    iconList := FMimeProvider.GetIconNames(AnsiString(Extension));
    if Assigned(iconList) then
    begin
      //First valid icon wins
      for I := 0 to iconList.Count - 1 do
      begin
        Result := string(iconList.Strings[I]);

        {$IFDEF QT}
        if QIcon_hasThemeIcon(@Result) then break;
        {$ELSE}
        if gtk_icon_theme_has_icon(gtk_icon_theme_get_default, PAnsiChar(UTF8Encode(Result))) then break;
        {$ENDIF}
      end;
    end;
  end
  else if FileIsExecutable(AFileName) then
  begin
    Result := 'application-x-executable';
  end;

  //Not found icon? No problem. Use generic icon
  if (not CheckSystemIconName(Result)) or (Result = EmptyStr) then
  begin
    if FileIsText(AFileName) then
      Result := 'text-x-generic'
    else
      Result := 'unknown';
  end;
end;
{$ENDIF}

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

{$IFDEF UNIX}
function TIconsManager.GetIconByDesktopFile(AFileName: String): String;
var
  iniDesktop: TIniFile = nil;
begin
  Result := EmptyStr;

  try
    iniDesktop := TIniFile.Create(AFileName);
    try
      Result := iniDesktop.ReadString(DESKTOP_GROUP, DESKTOP_KEY_ICON, EmptyStr);
    finally
      FreeAndNil(iniDesktop);
    end;
  except
    Exit;
  end;
end;
                 
{$IFDEF QT}
function TIconsManager.CheckSystemIconName(const AIconName: String): Boolean;
begin
  //QIcon_fromTheme can load icon name and absolute filepath, too
  Result := ((AIconName <> EmptyStr) and (QIcon_hasThemeIcon(@AIconName) or FileExists(AIconName)));
end;
{$ELSE}
function TIconsManager.CheckSystemIconName(const AIconName: String): Boolean;
begin
  Result := ((AIconName <> EmptyStr) and (gtk_icon_theme_has_icon(gtk_icon_theme_get_default, PAnsiChar(UTF8Encode(AIconName))) or
             FileExists(AIconName)));
end;
{$ENDIF}

procedure TIconsManager.EnsureMimeLoaded;
const
  mime_globs = '/usr/share/mime/globs';
  mime_generic_icons = '/usr/share/mime/generic-icons';
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TIconsManager.EnsureMimeLoaded', Self);
  FLock.Acquire;
  try
    if (FMimeProvider.Count = 0) and (FpAccess(mime_globs, R_OK) = 0) then
      FMimeProvider.LoadFromFiles(mime_globs, mime_generic_icons);
  finally
    FLock.Release;
  end;
end;
{$ENDIF}

end.
