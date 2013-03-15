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

unit ulAppConfig;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Graphics, Forms, Controls, VirtualTrees,
  ulEnumerations, FileUtil;

type
  TConfiguration = class
  private
    //General
    FStartWithWindows   : Boolean;
    FShowPanelAtStartUp : Boolean;
    FShowMenuAtStartUp  : Boolean;
    //Main Form
    FLanguage           : String; { TODO -oMatteo -c : Language code (type string?) 01/12/2009 23:02:23 }
    FUseCustomTitle     : Boolean;
    FCustomTitleString  : string;
    FHideTabSearch      : Boolean;
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : string;
    FTVAutoOpClCats     : Boolean; //  Automatic Opening/closing categories
    FTVFont             : TFont;
    FTVFontChanged      : Boolean;
    //MRU
    FMRU                : Boolean;
    FSubMenuMRU         : Boolean;
    FMRUNumber          : Integer;
    //MFU
    FMFU                : Boolean;
    FSubMenuMFU         : Boolean;
    FMFUNumber          : Integer;
    //Backup
    FBackup             : Boolean;
    FBackupNumber       : Integer;
    //Other functions
    FAutorun            : Boolean;
    FCache              : Boolean;
    //Execution
    FActionOnExe        : TActionOnExecution;
    FRunSingleClick     : Boolean;
    //Trayicon
    FTrayIcon           : Boolean;
    FTrayUseCustomIcon  : Boolean;
    FTrayCustomIconPath : string;
    FActionClickLeft    : Integer;
    FActionClickRight   : Integer;
    //Misc
    FReadOnlyMode       : Boolean;
    FChanged            : Boolean;
    procedure SetHoldSize(value: Boolean);
    procedure SetAlwaysOnTop(value: Boolean);
    procedure SetTrayIcon(value: Boolean);
    procedure SetTrayUseCustomIcon(value: Boolean);
    procedure SetUseCustomTitle(value: Boolean);
    procedure SetTVAutoOpClCats(value: Boolean);
    procedure SetHideTabSearch(value: Boolean);
    procedure SetShowPanelAtStartUp(value: Boolean);
    procedure SetTVBackground(value: Boolean);
    procedure SetCache(value: boolean);
    procedure SetStartWithWindows(value: boolean);
    procedure SetLanguage(value: string);
    procedure SetTVFont(value: TFont);
  public
    { public declarations }
    constructor Create; overload;
    destructor Destroy; override;
    //General
    property StartWithWindows: Boolean read FStartWithWindows write SetStartWithWindows;
    property ShowPanelAtStartUp: Boolean read FShowPanelAtStartUp write SetShowPanelAtStartUp;
    property ShowMenuAtStartUp: Boolean read FShowMenuAtStartUp write FShowMenuAtStartUp;
    // Main Form
    property Language: String read FLanguage write SetLanguage;
    property UseCustomTitle: Boolean read FUseCustomTitle write SetUseCustomTitle;
    property CustomTitleString : String read FCustomTitleString write FCustomTitleString;
    property HideTabSearch: Boolean read FHideTabSearch write SetHideTabSearch;
    // Main Form - Position and size
    property HoldSize: Boolean read FHoldSize write SetHoldSize;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write SetAlwaysOnTop;
    // Main Form - Treevew
    property TVBackground: Boolean read FTVBackground write SetTVBackground;
    property TVBackgroundPath: String read FTVBackgroundPath write FTVBackgroundPath;
    property TVAutoOpClCats: Boolean read FTVAutoOpClCats write SetTVAutoOpClCats;
    property TVFont: TFont read FTVFont write SetTVFont;
    property TVFontChanged: Boolean read FTVFontChanged write FTVFontChanged;
    // MRU
    property MRU: Boolean read FMRU write FMRU;
    property SubMenuMRU: Boolean read FSubMenuMRU write FSubMenuMRU;
    property MRUNumber: Integer read FMRUNumber write FMRUNumber;
    // MFU
    property MFU: Boolean read FMFU write FMFU;
    property SubMenuMFU: Boolean read FSubMenuMFU write FSubMenuMFU;
    property MFUNumber: Integer read FMFUNumber write FMFUNumber;
    // Backup
    property Backup: Boolean read FBackup write FBackup;
    property BackupNumber: Integer read FBackupNumber write FBackupNumber;
    // Other functions
    property Autorun: Boolean read FAutorun write FAutorun;
    property Cache: Boolean read FCache write SetCache;
    // Execution
    property ActionOnExe: TActionOnExecution read FActionOnExe write FActionOnExe;
    property RunSingleClick: Boolean read FRunSingleClick write FRunSingleClick;
    // Trayicon
    property TrayIcon: Boolean read FTrayIcon write SetTrayIcon;
    property TrayUseCustomIcon: Boolean read FTrayUseCustomIcon write SetTrayUseCustomIcon;
    property TrayCustomIconPath: String read FTrayCustomIconPath write FTrayCustomIconPath;
    property ActionClickLeft: Integer read FActionClickLeft write FActionClickLeft;
    property ActionClickRight: Integer read FActionClickRight write FActionClickRight;
    // Misc
    property ReadOnlyMode: Boolean read FReadOnlyMode write FReadOnlyMode;
    property Changed: Boolean read FChanged write FChanged;
  end;

var
  Config  : TConfiguration;

implementation

uses
  Main, udClassicMenu, ulSysUtils, AppConfig, ulDatabase;

constructor TConfiguration.Create;
begin
  //General
  FStartWithWindows   := False;
  FShowPanelAtStartUp := True;
  FShowMenuAtStartUp  := False;
  //Main Form
  { TODO -oMatteo -c : Insert code for language 26/11/2009 22:21:05 }
//  FLanguage           := '';
  FUseCustomTitle     := False;
  FCustomTitleString  := APP_TITLE;
  FHideTabSearch      := False;
  //Main Form - Position and size
  FHoldSize           := False;
  FAlwaysOnTop        := False;
  //Main Form - Treevew
  FTVBackground       := False;
  FTVBackgroundPath   := '';
  FTVAutoOpClCats     := True;
  //Treeview Font
  FTVFont             := TFont.Create;
  FTVFont.Name        := 'MS Sans Serif';
  FTVFont.Size        := 8;
  FTVFont.Color       := clWindowText;
  FTVFontChanged      := False;
  //MRU
  FMRU                := True;
  FSubMenuMRU         := False;
  FMRUNumber          := 5;
  //MFU
  FMFU                := True;
  FSubMenuMFU         := True;
  FMFUNumber          := 5;
  //Backup
  FBackup             := True;
  FBackupNumber       := 5;
  //Other functions
  FAutorun            := True;
  FCache              := True;
  //Execution
  FActionOnExe        := aeDefault;
  FRunSingleClick     := False;
  //Trayicon
  FTrayIcon           := True;
  FTrayUseCustomIcon  := False;
  FTrayCustomIconPath := '';
  FActionClickLeft    := 0;
  FActionClickRight   := 2;
  //Misc
  FReadOnlyMode       := False;
  FChanged            := False;
end;

destructor TConfiguration.Destroy;
begin
  inherited Destroy;
  FTVFont.Free;
end;

procedure TConfiguration.SetHoldSize(value: boolean);
begin
  FHoldSize := value;
  if FHoldSize then
  begin
    frmMain.BorderStyle := bsSingle;
    frmMain.BorderIcons := [biSystemMenu, biMinimize];
  end
  else begin
    frmMain.BorderStyle := bsSizeable;
    frmMain.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  end;
end;

procedure TConfiguration.SetAlwaysOnTop(value: Boolean);
begin
  FAlwaysOnTop := value;
  if FAlwaysOnTop then
    frmMain.FormStyle := fsSystemStayOnTop
  else
    frmMain.FormStyle := fsNormal;
end;

procedure TConfiguration.SetTrayIcon(value: Boolean);
begin
  FTrayIcon := value;
  ClassicMenu.tiTrayMenu.Visible := FTrayIcon;
end;

procedure TConfiguration.SetTrayUseCustomIcon(value: Boolean);
begin
  FTrayUseCustomIcon := value;
  if (FTrayUseCustomIcon) and (FileExistsUTF8(RelativeToAbsolute(FTrayCustomIconPath))) then
    ClassicMenu.tiTrayMenu.Icon.LoadFromFile(RelativeToAbsolute(FTrayCustomIconPath))
  else
    ClassicMenu.tiTrayMenu.Icon.LoadFromFile(RelativeToAbsolute(SUITE_ICONS_PATH + IntToStr(IMG_ASuite) + EXT_ICO));
end;

procedure TConfiguration.SetUseCustomTitle(value: Boolean);
begin
  FUseCustomTitle := value;
  if (FUseCustomTitle) and (FCustomTitleString <> '') then
    frmMain.Caption := FCustomTitleString
  else
    frmMain.Caption := APP_TITLE;
end;

procedure TConfiguration.SetTVAutoOpClCats(value: Boolean);
begin
  FTVAutoOpClCats := value;
  if FTVAutoOpClCats then
    frmMain.vstList.TreeOptions.AutoOptions := frmMain.vstList.TreeOptions.AutoOptions + [toAutoExpand]
  else
    frmMain.vstList.TreeOptions.AutoOptions := frmMain.vstList.TreeOptions.AutoOptions - [toAutoExpand];
end;

procedure TConfiguration.SetHideTabSearch(value: Boolean);
begin
  FHideTabSearch := value;
  with frmMain do
  begin
    tbSearch.TabVisible    := Not(FHideTabSearch);
    tbList.TabVisible      := Not(FHideTabSearch);
    pcList.ActivePageIndex := 0;
  end;
end;

procedure TConfiguration.SetShowPanelAtStartUp(value: Boolean);
begin
  FShowPanelAtStartUp := value;
  if (not(FShowPanelAtStartUp)) and (not(FTrayicon)) then
    FShowPanelAtStartUp := True;
end;

procedure TConfiguration.SetTVBackground(value: Boolean);
var
  BackgroundBMP : TBitmap;
  BackgroundPNG : TPortableNetworkGraphic;
begin
  FTVBackground := value;
  with frmMain do
  begin
    vstList.TreeOptions.PaintOptions := vstList.TreeOptions.PaintOptions - [toShowBackground];
    if (FTVBackground) and (FTVBackgroundPath <> '') and
       (FileExistsUTF8(RelativeToAbsolute(FTVBackgroundPath))) then
    begin
      if LowerCase(ExtractFileExt(RelativeToAbsolute(FTVBackgroundPath))) <> '.bmp' then
      begin
        BackgroundBMP := TBitmap.Create;
        BackgroundPNG := TPortableNetworkGraphic.Create;
        try
          BackgroundPNG.LoadFromFile(RelativeToAbsolute(FTVBackgroundPath));
          BackgroundBMP.Assign(BackgroundPNG);
          vstList.Background.Bitmap := BackgroundBMP;
        finally
          BackgroundBMP.Free;
          BackgroundPNG.Free;
        end;
      end
      else
        vstList.Background.LoadFromFile(RelativeToAbsolute(FTVBackgroundPath));
      vstList.TreeOptions.PaintOptions := vstList.TreeOptions.PaintOptions + [toShowBackground];
    end
    else
      vstList.TreeOptions.PaintOptions := vstList.TreeOptions.PaintOptions - [toShowBackground];
    vstList.Update;
  end;
end;

procedure TConfiguration.SetTVFont(value: TFont);
begin
  if FTVFont <> value then
  begin
    with FTVFont do
    begin
      Name  := value.Name;
      Style := value.Style;
      Size  := value.Size;
      Color := value.Color;
    end;
    FTVFontChanged := True;
  end;
end;

procedure TConfiguration.SetCache(value: Boolean);
begin
  FCache := value;
  if Not(FCache) then
  begin
    //Delete all file icon-cache and folder cache
    DeleteFiles(SUITE_CACHE_PATH,'*.*');
    RemoveDirUTF8(SUITE_CACHE_PATH);
  end
  else begin
    //Create folder cache, if it doesn't exist
    if (not DirectoryExistsUTF8(SUITE_CACHE_PATH)) then
      CreateDirUTF8(SUITE_CACHE_PATH);
  end;
end;

procedure TConfiguration.SetStartWithWindows(value: Boolean);
begin
  FStartWithWindows := value;
  if FStartWithWindows then
    SetASuiteAtWindowsStartup
  else
    DeleteASuiteAtWindowsStartup;
end;

procedure TConfiguration.SetLanguage(value: string);
begin
  if value <> FLanguage then
  begin
    FLanguage := value;
    frmMain.RefreshTranslation;
  end
  else
    FLanguage := value;
end;

end.
