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

interface

uses
  SysUtils, Classes, Controls, Forms, Icons.Files, Generics.Collections, SyncObjs,
  Kernel.Consts, LCLIntf, Icons.Base, Icons.ExtFile, Contnrs, BGRABitmap;

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
    {$IFDEF UNIX}
    FExtToMimeIconName: TFPDataHashTable;
    {$ENDIF}
    function GetPathTheme: string;

    procedure LoadAllIcons;
    procedure SetPathTheme(const Value: string);
    function FindByPath(APathFile: String): TFileIcon;
    function FindByName(AName: String): TBaseIcon;
    function FindByExt(AExtension: String): TExtFileIcon;
    {$IFDEF UNIX}
    procedure LoadMimeIconNames;
    function GetIconByDesktopFile(AFileName: String): String;
    procedure ClearExtToMimeList;
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
    function GetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean
      ): TBGRABitmap;
    {$IFDEF UNIX}
    {$IFDEF LCLQT5}  
    function GetSystemIconName(const AFileName: String): String;
    function CheckSystemIconName(const AIconName: String): Boolean;
    {$ELSE}                                                          
    function GetSystemIconName(const AFileName: AnsiString): AnsiString;
    function CheckSystemIconName(const AIconName: AnsiString): Boolean;
    {$ENDIF}
    {$ENDIF}

    property PathTheme: string read GetPathTheme write SetPathTheme;
  end;

implementation

uses
  Kernel.Logger, FileUtil, LazFileUtils, Kernel.Instance, ImgList,
  Graphics, DataModules.Icons
  {$IFDEF UNIX}
  , IniFiles, BaseUnix, StrUtils, Utility.FileFolder
    {$IFDEF LCLQT5}
    , qt5
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

  {$IFDEF UNIX}
  //Get mime and icons name from all extension in system
  FExtToMimeIconName := TFPDataHashTable.Create;
  LoadMimeIconNames;
  {$ENDIF}
end;

destructor TIconsManager.Destroy;
begin
  FItems.Free;
  FExtItems.Free;   
  FLock.Free;

  {$IFDEF UNIX}
  ClearExtToMimeList;
  FExtToMimeIconName.Free;
  {$ENDIF}

  inherited;
end;

procedure TIconsManager.Clear(AOnlyStaticItems: Boolean);
var
  Item: TBaseIcon;
begin
  if AOnlyStaticItems then
  begin
    for Item in FItems do
      if Item.Static then
        FItems.Remove(Item);
  end
  else
    FItems.Clear;
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
begin
  Result := -1;

  //Before find the icon, if it isn't exists yet, I will load it
  Icon := FindByPath(APathIcon);

  if Assigned(Icon) then
    Result := Icon.ImageIndex
  else begin
    Icon := TFileIcon.Create(APathIcon);
    try
      Result := Icon.ImageIndex;
    finally
      AddIcon(Icon);
    end;
  end;
end;

function TIconsManager.GetExtIconIndex(AExtension: string): Integer;
var
  Icon: TExtFileIcon;
begin
  Result := -1;

  //Before find the icon, if it isn't exists yet, I will load it
  Icon := FindByExt(AExtension);

  if Assigned(Icon) then
    Result := Icon.ImageIndex
  else begin
    Icon := TExtFileIcon.Create(AExtension);
    try
      Result := Icon.ImageIndex;
    finally
      AddIcon(Icon);
    end;
  end;
end;

procedure TIconsManager.AddIcon(ABaseIcon: TBaseIcon);
begin
  Assert(Assigned(ABaseIcon), 'ABaseIcon is not assigned!');

  try
    //It is doesn't necessary load now icon (so in this way, speed up icon loading)
    //ABaseIcon.LoadIcon;
  finally
    FItems.Add(ABaseIcon);
  end;
end;

procedure TIconsManager.AddExtIcon(ABaseIcon: TBaseIcon);
begin
  Assert(Assigned(ABaseIcon), 'ABaseIcon is not assigned!');

  try
    //Call ImageIndex, so I will extract the icon now
    ABaseIcon.ImageIndex;
  finally
    FExtItems.Add(ABaseIcon);
  end;
end;

function TIconsManager.GetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean): TBGRABitmap;
var
  Images: TCustomImageListResolution;
  bmpTemp: Graphics.TBitmap;
begin
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
{$IFDEF LCLQT5}  
function TIconsManager.GetSystemIconName(const AFileName: String): String;
{$ELSE}       
function TIconsManager.GetSystemIconName(const AFileName: AnsiString): AnsiString;
{$ENDIF}
var
  I: Integer;
  node: THTDataNode;
  iconList: TStringList;
  Extension: String;
begin
  Result := EmptyStr;

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
    node := THTDataNode(FExtToMimeIconName.Find(Extension));
    if Assigned(node) then
      begin
        iconList := TStringList(node.Data);

        //First valid icon wins
        for I := 0 to iconList.Count - 1 do
          begin
            Result := iconList.Strings[I];

            {$IFDEF LCLQT5}
            if QIcon_hasThemeIcon(@Result) then break;
            {$ELSE}
            if gtk_icon_theme_has_icon(gtk_icon_theme_get_default, PAnsiChar(Result)) then break;
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
  StartTime: Cardinal;
begin
  StartTime := TASuiteLogger.EnterMethod('TIconsManager.LoadAllIcons', Self);
  try
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
  finally
    TASuiteLogger.ExitMethod('TIconsManager.LoadAllIcons', Self, StartTime);
  end;
end;

procedure TIconsManager.SetPathTheme(const Value: string);
begin
  FPathTheme := value;
  LoadAllIcons;
end;

function TIconsManager.FindByPath(APathFile: String): TFileIcon;
var
  Item: TBaseIcon;
begin
  //Warning: only TFileIcon!
  Result := nil;

  for Item in FItems do
  begin
    if (Item is TFileIcon) and (TFileIcon(Item).PathFile = APathFile) then
    begin
      Result := TFileIcon(Item);
      break;
    end;
  end;
end;

function TIconsManager.FindByName(AName: String): TBaseIcon;
var
  Item: TBaseIcon;
begin
  Result := nil;

  for Item in FItems do
  begin
    if (Item.Name = AName) then
    begin
      Result := Item;
      break;
    end;
  end;
end;

function TIconsManager.FindByExt(AExtension: String): TExtFileIcon;
var
  Item: TBaseIcon;
begin
  Result := nil;

  for Item in FExtItems do
  begin
    if (Item.Name = LowerCase(AExtension)) and (Item is TExtFileIcon) then
    begin
      Result := TExtFileIcon(Item);
      break;
    end;
  end;
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
                 
{$IFDEF LCLQT5}
function TIconsManager.CheckSystemIconName(const AIconName: String): Boolean;
begin
  //QIcon_fromTheme can load icon name and absolute filepath, too
  Result := ((AIconName <> EmptyStr) and (QIcon_hasThemeIcon(@AIconName) or FileExists(AIconName)));
end;  
{$ELSE}  
function TIconsManager.CheckSystemIconName(const AIconName: AnsiString): Boolean;
begin
  Result := ((AIconName <> EmptyStr) and (gtk_icon_theme_has_icon(gtk_icon_theme_get_default, PAnsiChar(AIconName)) or
             FileExists(AIconName)));     
end;
{$ENDIF}

procedure TIconsManager.ClearExtToMimeList;
var
  nodeList: TFPObjectList;
  I, J : Integer;
begin
  for I := 0 to FExtToMimeIconName.HashTable.Count - 1 do
  begin
    begin
      nodeList := TFPObjectList(FExtToMimeIconName.HashTable.Items[I]);
      if Assigned(nodeList) then
        for J := 0 to nodeList.Count - 1 do
          TStringList(THtDataNode(nodeList.Items[J]).Data).Free;
    end;
  end;
end;

procedure TIconsManager.LoadMimeIconNames;
const
  mime_globs = '/usr/share/mime/globs';
  mime_generic_icons = '/usr/share/mime/generic-icons';
var
  I, J: Integer;
  globs: TStringList = nil;
  generic_icons: THashedStringList = nil;
  sMimeType,
  sMimeIconName,
  sExtension: String;
  node: THTDataNode = nil;
  iconsList: TStringList;
begin
  try
    if FExtToMimeIconName.Count = 0 then
    begin
      if FpAccess(mime_globs, R_OK) = 0 then
      begin
        // Load mapping: MIME type -> file extension.
        globs:= TStringList.Create;
        globs.NameValueSeparator:= ':';
        globs.LoadFromFile(mime_globs);

        // Try to load mapping: MIME type -> generic MIME icon name.
        if FileExists(mime_generic_icons) then
          begin
            generic_icons:= THashedStringList.Create;
            generic_icons.NameValueSeparator:= ':';
            generic_icons.LoadFromFile(mime_generic_icons);
          end;

        // Create mapping: file extension -> list of MIME icon names.
        for I:= 0 to globs.Count - 1 do
          if (globs.Strings[I]    <> EmptyStr) and   // bypass empty lines
             (globs.Strings[I][1] <> '#') then // and comments
          begin
            sMimeType := globs.Names[I];
            sMimeIconName:= StringReplace(sMimeType, '/', '-', []);
            sExtension:= globs.ValueFromIndex[I];

            // Support only extensions, not full file name masks.
            if (sExtension <> EmptyStr) and (sExtension <> '.*') then
            begin
              node := THTDataNode(FExtToMimeIconName.Find(sExtension));
              if not Assigned(node) then
                begin
                  iconsList := TStringList.Create;
                  FExtToMimeIconName.Add(sExtension, iconsList);
                end
              else
                iconsList := TStringList(node.Data);

              if iconsList.IndexOf(sMimeIconName) < 0 then
                iconsList.Add(sMimeIconName);

              // Shared-mime-info spec says:
              // "If [generic-icon] is not specified then the mimetype is used to generate the
              // generic icon by using the top-level media type (e.g. "video" in "video/ogg")
              // and appending "-x-generic" (i.e. "video-x-generic" in the previous example)."
              if Assigned(generic_icons) then
                begin
                  J := generic_icons.IndexOfName(sMimeType);
                  if J <> -1 then
                    sMimeIconName := generic_icons.ValueFromIndex[J] // found generic icon
                  else
                    sMimeIconName := Copy2Symb(sMimeIconName, '-') + '-x-generic';
                end
              else
                sMimeIconName := Copy2Symb(sMimeIconName, '-') + '-x-generic';

              if iconsList.IndexOf(sMimeIconName) < 0 then
                iconsList.Add(sMimeIconName);
            end;
          end;
      end;
    end;

  finally
    if Assigned(globs) then
      FreeAndNil(globs);
    if Assigned(generic_icons) then
      FreeAndNil(generic_icons);
  end;
end;
{$ENDIF}

end.
