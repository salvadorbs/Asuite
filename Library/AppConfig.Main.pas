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

unit AppConfig.Main;

{$MODE DelphiUnicode}

{$I ASuite.inc}

{$IFDEF LCLGTK3}
  {$LINKLIB libgdk-3.so.0}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Graphics, Forms, Controls, Kernel.Enumerations,
  Classes, jsonConf, LazFileUtils, Kernel.Logger, Dialogs,
  AppConfig.Observer
  {$IFDEF LINUX}
  , x, xlib

    {$IFDEF QT}
      {$IFDEF LCLQT5}
      , qt5
      {$ELSE}
      , qt6, qtint
      {$ENDIF}
    {$ELSE}
      {$IFDEF LCLGTK2}
      , gdk2, gdk2x
      {$ELSE}
      , LazGdk3, LazGLib2
      {$ENDIF}
    {$ENDIF}
  {$ENDIF};

type

  { TConfiguration }
  TConfiguration = class
  private
    FCMHideEjectMenuItem: Boolean;
    FGMHideEjectButton: Boolean;
    //General
    FStartWithWindows   : Boolean;
    FShowPanelAtStartUp : Boolean;
    FShowGraphicMenuAtStartUp  : Boolean;
    FShowGraphicMenuAnotherInstance: Boolean;
    FConfirmMsgCloseApp: Boolean;
    //Main Form
    FLangID             : String;
    FTVDisableConfirmDelete: Boolean;
    FUseCustomTitle     : Boolean;
    FCustomTitleString  : string;
    FHideTabSearch      : Boolean;
    FSearchAsYouType    : Boolean;
    // Main Form - Search Columns
    FSearchNameColWidth: Integer;
    FSearchCategoryColWidth: Integer;
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    FDialogCenterMF     : Boolean;
    // Main Form - Bounds
    FMainFormLeft: Integer;
    FMainFormTop: Integer;
    FMainFormWidth: Integer;
    FMainFormHeight: Integer;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : string;
    FTVAutoOpClCats     : Boolean; //Automatic Opening/closing categories
    FTVAutoOpCatsDrag   : Boolean;
    FTVFont             : Graphics.TFont;
    FTVSmallIconSize    : Boolean;
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
    FAutorunStartup     : Boolean;
    FAutorunShutdown    : Boolean;
    FCache              : Boolean;
    FScheduler          : Boolean;
    //Execution
    FActionOnExe        : TActionOnExecute;
    FRunSingleClick     : Boolean;
    FConfirmRunCat      : Boolean;
    FAutoCloseProcess   : Boolean;
    //Trayicon
    FTrayIcon           : Boolean;
    FTrayUseCustomIcon  : Boolean;
    FTrayCustomIconPath : string;
    FActionClickLeft    : TTrayiconActionClick;
    FActionClickRight   : TTrayiconActionClick;
    //Graphic Menu
    FGMTheme            : string;
    FGMFade             : Boolean;
    FGMPersonalPicture  : string;
    FGMPositionTop      : Integer;
    FGMPositionLeft     : Integer;
    FGMSmallIconSize    : Boolean;
    FGMShowUserPicture   : Boolean;
    //Right buttons
    FGMBtnDocuments     : string;
    FGMBtnMusic         : string;
    FGMBtnPictures      : string;
    FGMBtnVideos        : string;
    FGMBtnExplore       : string;
    //HotKeys
    FHotKey             : Boolean;
    FWindowHotKey       : string;
    FGraphicMenuHotKey  : string;
    FClassicMenuHotkey  : string;
    //Misc
    FChanged              : Boolean;
    FASuiteState          : TLauncherState;
    FMissedSchedulerTask  : Boolean;
    FAutoExpansionFolder  : Boolean;
    FActionClickMiddle    : TTrayiconActionClick;
    FScanFolderAutoExtractName : boolean;
    FScanFolderFileTypes  : TStringList;
    FScanFolderExcludeNames: TStringList;
    FObservers: TInterfaceList;
    // Batching notification support
    FNotificationBatchCount: Integer;
    FBatchedProperties: TStringList;

    function LoadPngAndConvertBMP(const APath: String): TBitmap;
    procedure RestoreSettings(AJSONConfig: TJSONConfig);
    procedure SaveSettings(AJSONConfig: TJSONConfig);
    procedure SetHoldSize(value: Boolean);
    procedure SetAlwaysOnTop(value: Boolean);
    procedure SetTrayIcon(value: Boolean);
    procedure SetTVBackgroundPath(AValue: String);
    procedure SetUseCustomTitle(value: Boolean);
    procedure SetTVAutoOpClCats(value: Boolean);
    procedure SetHideTabSearch(value: Boolean);
    procedure SetShowPanelAtStartUp(value: Boolean);
    procedure SetTVBackground(value: Boolean);
    procedure SetCache(value: boolean);
    procedure SetStartWithWindows(value: boolean);
    procedure SetLangID(value: String);
    procedure SetTVFont(value: Graphics.TFont);
    procedure SetScheduler(value: Boolean);
    procedure SetGMTheme(value: string);
    procedure SetGMBtnDocuments(Value: string);
    procedure SetGMBtnExplore(Value: string);
    procedure SetGMBtnMusic(Value: string);
    procedure SetGMBtnPictures(Value: string);
    procedure SetGMBtnVideos(Value: string);
    procedure SetBackupNumber(const Value: Integer);
    procedure SetChanged(const Value: Boolean);
    procedure SetBackup(const Value: Boolean);
    procedure SetGraphicMenuHotKey(const Value: string);
    procedure SetWindowHotKey(const Value: string);
    procedure SetHotKey(const Value: Boolean);
    procedure SetTVSmallIconSize(const Value: Boolean);
    procedure SetClassicMenuHotkey(const Value: string);
    procedure SetSearchNameColWidth(const Value: Integer);
    procedure SetSearchCategoryColWidth(const Value: Integer);
    procedure SetMainFormLeft(const Value: Integer);
    procedure SetMainFormTop(const Value: Integer);
    procedure SetMainFormWidth(const Value: Integer);
    procedure SetMainFormHeight(const Value: Integer);
    procedure SetASuiteState(const Value: TLauncherState);
    procedure SetShowGraphicMenuAtStartUp(const Value: Boolean);
    procedure SetMissedSchedulerTask(const Value: Boolean);
    procedure SetShowGraphicMenuAnotherInstance(const Value: Boolean);
    procedure SetConfirmMsgCloseApp(const Value: Boolean);
    procedure SetCustomTitleString(const Value: String);
    procedure SetSearchAsYouType(const Value: Boolean);
    procedure SetDialogCenterMF(const Value: Boolean);
    procedure SetTVAutoOpCatsDrag(const Value: Boolean);
    procedure SetTVDisableConfirmDelete(const Value: Boolean);
    procedure SetMRU(const Value: Boolean);
    procedure SetSubMenuMRU(const Value: Boolean);
    procedure SetMRUNumber(const Value: Integer);
    procedure SetMFU(const Value: Boolean);
    procedure SetSubMenuMFU(const Value: Boolean);
    procedure SetMFUNumber(const Value: Integer);
    procedure SetAutorunStartup(const Value: Boolean);
    procedure SetAutorunShutdown(const Value: Boolean);
    procedure SetActionOnExe(const Value: TActionOnExecute);
    procedure SetRunSingleClick(const Value: Boolean);
    procedure SetConfirmRunCat(const Value: Boolean);
    procedure SetAutoCloseProcess(const Value: Boolean);
    procedure SetTrayUseCustomIcon(const Value: Boolean);
    procedure SetTrayCustomIconPath(const Value: String);
    procedure SetActionClickLeft(const Value: TTrayiconActionClick);
    procedure SetActionClickMiddle(const Value: TTrayiconActionClick);
    procedure SetActionClickRight(const Value: TTrayiconActionClick);
    procedure SetAutoExpansionFolder(const Value: Boolean);
    procedure SetCMHideEjectMenuItem(const Value: Boolean);
    procedure SetGMFade(const Value: Boolean);
    procedure SetGMSmallIconSize(const Value: Boolean);
    procedure SetGMPersonalPicture(const Value: string);
    procedure SetGMPositionTop(const Value: Integer);
    procedure SetGMPositionLeft(const Value: Integer);
    procedure SetGMShowUserPicture(const Value: Boolean);
    procedure SetGMHideEjectButton(const Value: Boolean);
    procedure SetScanFolderAutoExtractName(const Value: Boolean);
    procedure SetScanFolderFileTypes(const Value: TStringList);
    procedure SetScanFolderExcludeNames(const Value: TStringList);

    function UpdateHotkey(OldValue, NewValue: String; Tag: Integer): Boolean;
    function isValidHotkeyString(AValue: String): Boolean;
    procedure NotifyObservers(const PropertyName: string = '');

    //These functions come from the Tomboy-NG project https://github.com/tomboy-notes/tomboy-ng
    function CheckGnomeExtras: Boolean;
    function CheckSysTray: Boolean;
  public
    { public declarations }
    constructor Create; overload;
    destructor Destroy; override;

    //General
    property StartWithWindows: Boolean read FStartWithWindows write SetStartWithWindows;
    property ShowPanelAtStartUp: Boolean read FShowPanelAtStartUp write SetShowPanelAtStartUp;
    property ShowGraphicMenuAtStartUp: Boolean read FShowGraphicMenuAtStartUp write SetShowGraphicMenuAtStartUp;
    property MissedSchedulerTask: Boolean read FMissedSchedulerTask write SetMissedSchedulerTask;
    property ShowGraphicMenuAnotherInstance: Boolean read FShowGraphicMenuAnotherInstance write SetShowGraphicMenuAnotherInstance;
    property ConfirmMsgCloseApp: Boolean read FConfirmMsgCloseApp write SetConfirmMsgCloseApp;
    // Main Form
    property LangID: String read FLangID write SetLangID;
    property UseCustomTitle: Boolean read FUseCustomTitle write SetUseCustomTitle;
    property CustomTitleString : String read FCustomTitleString write SetCustomTitleString;
    property HideTabSearch: Boolean read FHideTabSearch write SetHideTabSearch;
    property SearchAsYouType: Boolean read FSearchAsYouType write SetSearchAsYouType;
    property SearchNameColWidth: Integer read FSearchNameColWidth write SetSearchNameColWidth;
    property SearchCategoryColWidth: Integer read FSearchCategoryColWidth write SetSearchCategoryColWidth;
    // Main Form - Position and size
    property HoldSize: Boolean read FHoldSize write SetHoldSize;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write SetAlwaysOnTop;
    property DialogCenterMF: Boolean read FDialogCenterMF write SetDialogCenterMF;
    property MainFormLeft: Integer read FMainFormLeft write SetMainFormLeft;
    property MainFormTop: Integer read FMainFormTop write SetMainFormTop;
    property MainFormWidth: Integer read FMainFormWidth write SetMainFormWidth;
    property MainFormHeight: Integer read FMainFormHeight write SetMainFormHeight;
    // Main Form - Treevew
    property TVBackground: Boolean read FTVBackground write SetTVBackground;
    property TVSmallIconSize: Boolean read FTVSmallIconSize write SetTVSmallIconSize;
    property TVBackgroundPath: String read FTVBackgroundPath write SetTVBackgroundPath;
    property TVAutoOpClCats: Boolean read FTVAutoOpClCats write SetTVAutoOpClCats;
    property TVAutoOpCatsDrag: Boolean read FTVAutoOpCatsDrag write SetTVAutoOpCatsDrag;
    property TVDisableConfirmDelete: Boolean read FTVDisableConfirmDelete write SetTVDisableConfirmDelete;
    property TVFont: Graphics.TFont read FTVFont write SetTVFont;
    // MRU
    property MRU: Boolean read FMRU write SetMRU;
    property SubMenuMRU: Boolean read FSubMenuMRU write SetSubMenuMRU;
    property MRUNumber: Integer read FMRUNumber write SetMRUNumber;
    // MFU
    property MFU: Boolean read FMFU write SetMFU;
    property SubMenuMFU: Boolean read FSubMenuMFU write SetSubMenuMFU;
    property MFUNumber: Integer read FMFUNumber write SetMFUNumber;
    // Backup
    property Backup: Boolean read FBackup write SetBackup;
    property BackupNumber: Integer read FBackupNumber write SetBackupNumber;
    // Other functions
    property AutorunStartup: Boolean read FAutorunStartup write SetAutorunStartup;
    property AutorunShutdown: Boolean read FAutorunShutdown write SetAutorunShutdown;
    property Cache: Boolean read FCache write SetCache;
    property Scheduler: Boolean read FScheduler write SetScheduler;
    // Execution
    property ActionOnExe: TActionOnExecute read FActionOnExe write SetActionOnExe;
    property RunSingleClick: Boolean read FRunSingleClick write SetRunSingleClick;
    property ConfirmRunCat: Boolean read FConfirmRunCat write SetConfirmRunCat;
    property AutoCloseProcess: Boolean read FAutoCloseProcess write SetAutoCloseProcess;
    // Trayicon
    property TrayIcon: Boolean read FTrayIcon write SetTrayIcon;
    property TrayUseCustomIcon: Boolean read FTrayUseCustomIcon write SetTrayUseCustomIcon;
    property TrayCustomIconPath: String read FTrayCustomIconPath write SetTrayCustomIconPath;
    property ActionClickLeft: TTrayiconActionClick read FActionClickLeft write SetActionClickLeft;
    property ActionClickMiddle: TTrayiconActionClick read FActionClickMiddle write SetActionClickMiddle;
    property ActionClickRight: TTrayiconActionClick read FActionClickRight write SetActionClickRight;
    property AutoExpansionFolder: Boolean read FAutoExpansionFolder write SetAutoExpansionFolder;
    property CMHideEjectMenuItem: Boolean read FCMHideEjectMenuItem write SetCMHideEjectMenuItem;
    //Graphic Menu
    property GMTheme: string read FGMTheme write SetGMTheme;
    property GMFade: Boolean read FGMFade write SetGMFade;
    property GMSmallIconSize: Boolean read FGMSmallIconSize write SetGMSmallIconSize;
    property GMPersonalPicture: string read FGMPersonalPicture write SetGMPersonalPicture;
    property GMPositionTop: Integer read FGMPositionTop write SetGMPositionTop;
    property GMPositionLeft: Integer read FGMPositionLeft write SetGMPositionLeft;
    property GMShowUserPicture: Boolean read FGMShowUserPicture write SetGMShowUserPicture;
    property GMHideEjectButton: Boolean read FGMHideEjectButton write SetGMHideEjectButton;
    //Right buttons
    property GMBtnDocuments: string read FGMBtnDocuments write SetGMBtnDocuments;
    property GMBtnPictures: string read FGMBtnPictures write SetGMBtnPictures;
    property GMBtnMusic: string read FGMBtnMusic write SetGMBtnMusic;
    property GMBtnVideos: string read FGMBtnVideos write SetGMBtnVideos;
    property GMBtnExplore: string read FGMBtnExplore write SetGMBtnExplore;
    //HotKeys
    property HotKey: Boolean read FHotKey write SetHotKey;
    property WindowHotKey: string read FWindowHotKey write SetWindowHotKey;
    property GraphicMenuHotKey: string read FGraphicMenuHotKey write SetGraphicMenuHotKey;
    property ClassicMenuHotkey: string read FClassicMenuHotkey write SetClassicMenuHotkey;
    // Misc
    property Changed: Boolean read FChanged write SetChanged;
    property ASuiteState: TLauncherState read FASuiteState write SetASuiteState;
    property ScanFolderAutoExtractName: boolean read FScanFolderAutoExtractName write SetScanFolderAutoExtractName;
    property ScanFolderFileTypes: TStringList read FScanFolderFileTypes write SetScanFolderFileTypes;
    property ScanFolderExcludeNames: TStringList read FScanFolderExcludeNames write SetScanFolderExcludeNames;

    procedure AfterUpdateConfig;

    procedure LoadConfig;
    procedure SaveConfig;
    procedure AddObserver(const Observer: IConfigObserver);
    procedure RemoveObserver(const Observer: IConfigObserver);
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

var
  Config: TConfiguration;

implementation

uses
  Utility.System, Kernel.Consts, Utility.Misc,
  VirtualTree.Methods, Utility.FileFolder, mormot.core.log,
  LCLProc, VirtualTrees.Types, Process,
  TypInfo, Kernel.ResourceStrings, AppConfig.Consts, BGRABitmapTypes,
  Utility.Conversions, Hotkeys.Manager.Platform, Kernel.Instance, Kernel.Manager;

procedure TConfiguration.AfterUpdateConfig;
begin
  NotifyObservers('AfterUpdateConfig');
end;

constructor TConfiguration.Create;
begin
  //General
  FStartWithWindows   := False;
  FShowPanelAtStartUp := True;
  FShowGraphicMenuAtStartUp  := False;
  FMissedSchedulerTask := True;
  FShowGraphicMenuAnotherInstance := True;
  FConfirmMsgCloseApp := True;

  //Main Form
  FLangID             := 'en';
  FUseCustomTitle     := False;
  FCustomTitleString  := APP_TITLE;
  FHideTabSearch      := False;
  FSearchAsYouType    := True;

  // Main Form - Search Columns
  FSearchNameColWidth := 90;
  FSearchCategoryColWidth := 90;

  //Main Form - Position and size
  FHoldSize           := False;
  FAlwaysOnTop        := False;
  FDialogCenterMF     := True;

  // Main Form - Bounds
  FMainFormLeft       := 0;
  FMainFormTop        := 0;
  FMainFormWidth      := 200;
  FMainFormHeight     := 385;

  //Main Form - Treevew
  FTVBackground       := False;
  FTVBackgroundPath   := '';
  FTVSmallIconSize    := True;
  FTVAutoOpClCats     := True;
  FTVAutoOpCatsDrag   := True;
  TVDisableConfirmDelete := False;

  //Treeview Font
  FTVFont             := Graphics.TFont.Create;
  FTVFont.Name        := 'MS Sans Serif';
  FTVFont.Size        := 8;
  FTVFont.Color       := clWindowText;

  //MRU
  FMRU                := True;
  FSubMenuMRU         := False;
  FMRUNumber          := 11;

  //MFU
  FMFU                := True;
  FSubMenuMFU         := True;
  FMFUNumber          := 11;

  //Backup
  FBackup             := True;
  FBackupNumber       := 5;

  //Other functions
  FAutorunStartup     := True;
  FAutorunShutdown    := True;
  FCache              := True;
  FScheduler          := True;

  //Execution
  FActionOnExe        := aeDefault;
  FRunSingleClick     := False;
  FConfirmRunCat      := True;
  FAutoCloseProcess   := False;

  //Trayicon
  FTrayIcon           := True;
  FTrayUseCustomIcon  := False;
  FTrayCustomIconPath := '';
  FActionClickLeft    := tcShowClassicMenu;
  FActionClickMiddle  := tcNone;
  FActionClickRight   := tcShowGraphicMenu;
  FAutoExpansionFolder := True;
  FCMHideEjectMenuItem := False;

  //Graphic Menu
  FGMTheme            := 'default';
  FGMFade             := True;
  FGMPersonalPicture  := 'PersonalPicture.png';
  FGMPositionTop      := -1;
  FGMPositionLeft     := -1;
  FGMShowUserPicture  := True;
  FGMHideEjectButton  := False;

  //Right buttons
  FGMBtnDocuments     := '%USERPROFILE%\Documents';
  FGMBtnPictures      := '%USERPROFILE%\Pictures';
  FGMBtnMusic         := '%USERPROFILE%\Music';
  FGMBtnVideos        := '%USERPROFILE%\Videos';
  FGMBtnExplore       := '$drive';

  //Misc
  FChanged            := False;
  FASuiteState        := lsStartUp;
  FHotKey             := True;

  //Hotkey
  FWindowHotKey       := '';
  FGraphicMenuHotKey  := '';
  FClassicMenuHotkey  := '';

  //ScanFolder
  FScanFolderAutoExtractName := True;
  FScanFolderFileTypes  := TStringList.Create;
  FScanFolderFileTypes.Add(EXT_PATH_MASK + EXT_LNK);
  FScanFolderFileTypes.Add(EXT_PATH_MASK + EXT_EXE);
  FScanFolderExcludeNames := TStringList.Create;
  FScanFolderExcludeNames.Add('uninstall');

  FObservers := TInterfaceList.Create;
  FNotificationBatchCount := 0;
  FBatchedProperties := TStringList.Create;
end;

destructor TConfiguration.Destroy;
begin
  FBatchedProperties.Free;
  FTVFont.Free;
  FScanFolderFileTypes.Free;
  FScanFolderExcludeNames.Free;
  FObservers.Free;
  inherited Destroy;
end;

procedure TConfiguration.BeginUpdate;
begin
  // Increments the batch counter. While >0, notifications are batched.
  Inc(FNotificationBatchCount);
end;

procedure TConfiguration.EndUpdate;
var
  Excluded: TStringList;
  I: Integer;
begin
  // Decrements the batch counter. When it reaches 0, send notifications for all batched properties.
  if FNotificationBatchCount = 0 then Exit;
  Dec(FNotificationBatchCount);
  if FNotificationBatchCount = 0 then
  begin
    Excluded := TStringList.Create;
    try
      // If any of the MainForm bounds properties were changed, group as 'MainFormBounds'.
      // This avoids redundant notifications for each property when setting bounds together.
      if (FBatchedProperties.IndexOf('MainFormLeft') >= 0) or
         (FBatchedProperties.IndexOf('MainFormTop') >= 0) or
         (FBatchedProperties.IndexOf('MainFormWidth') >= 0) or
         (FBatchedProperties.IndexOf('MainFormHeight') >= 0) then
      begin
        NotifyObservers('MainFormBounds');
        // Exclude grouped properties from individual notification
        Excluded.Add('MainFormLeft');
        Excluded.Add('MainFormTop');
        Excluded.Add('MainFormWidth');
        Excluded.Add('MainFormHeight');
      end;
      // Notify for each property changed during the batch, except those grouped
      I := 0;
      while i < FBatchedProperties.Count do
      begin
        if Excluded.IndexOf(FBatchedProperties[i]) = -1 then
        begin
          NotifyObservers(FBatchedProperties[i]);
        end;
        Inc(i);
      end;
      FBatchedProperties.Clear;
    finally
      Excluded.Free;
    end;
  end;
end;

procedure TConfiguration.SetASuiteState(const Value: TLauncherState);
begin
  TASuiteLogger.Info('Changed ASuite State (old value %s, new value %s)',
    [GetEnumName(TypeInfo(TLauncherState), Ord(FASuiteState)),
     GetEnumName(TypeInfo(TLauncherState), Ord(Value))]);
  FASuiteState := Value;
  NotifyObservers('ASuiteState');
end;

function TConfiguration.UpdateHotkey(OldValue, NewValue: String; Tag: Integer
  ): Boolean;
var
  newShortcut, oldShortcut: TShortcut;
begin
  Result := not FHotKey;
  if (FHotKey) then
  begin
    oldShortcut := TextToShortCut(OldValue);
    newShortcut := TextToShortCut(NewValue);

    //Unregister hotkey (if actived)
    HotkeyManager.UnregisterNotify(oldShortcut);

    //Register hotkey
    if (newShortcut <> 0) then
      Result := HotkeyManager.RegisterNotify(newShortcut, TVirtualTreeMethods.HotKeyNotify, Tag)
    else
      Result := True;
  end;
end;

function TConfiguration.isValidHotkeyString(AValue: String): Boolean;
begin
  Result := (TextToShortCut(AValue) <> 0);
end;

procedure TConfiguration.SetHoldSize(value: Boolean);
begin
  FHoldSize := value;
  NotifyObservers('HoldSize');
end;

procedure TConfiguration.SaveSettings(AJSONConfig: TJSONConfig);
begin
  try
    AJSONConfig.SetValue(CONFIG_GMTHEME, Self.GMTheme);

    //General
    AJSONConfig.SetValue(CONFIG_STARTWITHWINDOWS, Self.StartWithWindows);
    AJSONConfig.SetValue(CONFIG_SHOWPANELATSTARTUP, Self.ShowPanelAtStartUp);
    AJSONConfig.SetValue(CONFIG_SHOWMENUATSTARTUP, Self.ShowGraphicMenuAtStartUp);
    AJSONConfig.SetValue(CONFIG_MISSEDSCHEDULERTASK, Self.MissedSchedulerTask);
    AJSONConfig.SetValue(CONFIG_SECONDINSTANCEGM, Self.ShowGraphicMenuAnotherInstance);
    AJSONConfig.SetValue(CONFIG_CONFIRMCLOSEASUITE, Self.ConfirmMsgCloseApp);

    // Main Form
    AJSONConfig.SetValue(CONFIG_LANGID, Self.LangID);
    AJSONConfig.SetValue(CONFIG_USECUSTOMTITLE, Self.UseCustomTitle);
    AJSONConfig.SetValue(CONFIG_CUSTOMTITLESTRING, Self.CustomTitleString);
    AJSONConfig.SetValue(CONFIG_HIDETABSEARCH, Self.HideTabSearch);
    AJSONConfig.SetValue(CONFIG_SEARCHASYOUTYPE, Self.SearchAsYouType);
    AJSONConfig.SetValue(CONFIG_SEARCH_NAME_WIDTH, Self.SearchNameColWidth);
    AJSONConfig.SetValue(CONFIG_SEARCH_CATEGORY_WIDTH, Self.SearchCategoryColWidth);
    AJSONConfig.SetValue(CONFIG_HOLDSIZE, Self.HoldSize);
    AJSONConfig.SetValue(CONFIG_ALWAYSONTOP, Self.AlwaysOnTop);
    AJSONConfig.SetValue(CONFIG_MAINFORM_DIALOGS_CENTER, Self.DialogCenterMF);

    // Main Form - Position and size
    AJSONConfig.SetValue(CONFIG_MAINFORM_LEFT, Self.MainFormLeft);
    AJSONConfig.SetValue(CONFIG_MAINFORM_TOP, Self.MainFormTop);
    AJSONConfig.SetValue(CONFIG_MAINFORM_WIDTH, Self.MainFormWidth);
    AJSONConfig.SetValue(CONFIG_MAINFORM_HEIGHT, Self.MainFormHeight);

    // Main Form - Treevew
    AJSONConfig.SetValue(CONFIG_TVBACKGROUND, Self.TVBackground);
    AJSONConfig.SetValue(CONFIG_TVBACKGROUNDPATH, Self.TVBackgroundPath);
    AJSONConfig.SetValue(CONFIG_TVSMALLICONSIZE, Self.TVSmallIconSize);
    AJSONConfig.SetValue(CONFIG_TVAUTOOPCLCATS, Self.TVAutoOpClCats);
    AJSONConfig.SetValue(CONFIG_TVAUTOOPCATSDRAG, Self.TVAutoOpCatsDrag);
    AJSONConfig.SetValue(CONFIG_TVDISABLECONFIRMDELETE, Self.TVDisableConfirmDelete);
    AJSONConfig.SetValue(CONFIG_TVFONTNAME, UnicodeString(Self.TVFont.Name));

    // Main Form - TVFont
    AJSONConfig.SetValue(CONFIG_TVFONTSIZE, Self.TVFont.Size);
    AJSONConfig.SetValue(CONFIG_TVFONTCOLOR, ColorToHtml(Self.TVFont.Color));
    AJSONConfig.SetValue(CONFIG_TVFONTSTYLE_BOLD, (fsBold in Self.TVFont.Style));
    AJSONConfig.SetValue(CONFIG_TVFONTSTYLE_ITALIC, (fsItalic in Self.TVFont.Style));
    AJSONConfig.SetValue(CONFIG_TVFONTSTYLE_UNDERLINE, (fsUnderline in Self.TVFont.Style));
    AJSONConfig.SetValue(CONFIG_TVFONTSTYLE_STRIKEOUT, (fsStrikeOut in Self.TVFont.Style));

    // MRU
    AJSONConfig.SetValue(CONFIG_MRU, Self.MRU);
    AJSONConfig.SetValue(CONFIG_SUBMENUMRU, Self.SubMenuMRU);
    AJSONConfig.SetValue(CONFIG_MRUNUMBER, Self.MRUNumber);

    // MFU
    AJSONConfig.SetValue(CONFIG_MFU, Self.MFU);
    AJSONConfig.SetValue(CONFIG_SUBMENUMFU, Self.SubMenuMFU);
    AJSONConfig.SetValue(CONFIG_MFUNUMBER, Self.MFUNumber);

    // Backup
    AJSONConfig.SetValue(CONFIG_BACKUP, Self.Backup);
    AJSONConfig.SetValue(CONFIG_BACKUPNUMBER, Self.BackupNumber);

    // Other functions
    AJSONConfig.SetValue(CONFIG_AUTORUNSTARTUP, Self.AutorunStartup);
    AJSONConfig.SetValue(CONFIG_AUTORUNSHUTDOWN, Self.AutorunShutdown);
    AJSONConfig.SetValue(CONFIG_CACHE, Self.Cache);
    AJSONConfig.SetValue(CONFIG_SCHEDULER, Self.Scheduler);

    // Execution
    AJSONConfig.SetValue(CONFIG_ACTIONONEXE, Integer(Self.ActionOnExe));
    AJSONConfig.SetValue(CONFIG_RUNSINGLECLICK, Self.RunSingleClick);
    AJSONConfig.SetValue(CONFIG_CONFIRMMESSAGECAT, Self.ConfirmRunCat);
    AJSONConfig.SetValue(CONFIG_AUTOCLOSEPROCESS, Self.AutoCloseProcess);

    // Trayicon
    AJSONConfig.SetValue(CONFIG_TRAYICON, Self.TrayIcon);
    AJSONConfig.SetValue(CONFIG_TRAYUSECUSTOMICON, Self.TrayUseCustomIcon);
    AJSONConfig.SetValue(CONFIG_TRAYCUSTOMICONPATH, Self.TrayCustomIconPath);
    AJSONConfig.SetValue(CONFIG_ACTIONCLICKLEFT, Integer(Self.ActionClickLeft));
    AJSONConfig.SetValue(CONFIG_ACTIONCLICKMIDDLE, Integer(Self.ActionClickMiddle));
    AJSONConfig.SetValue(CONFIG_ACTIONCLICKRIGHT, Integer(Self.ActionClickRight));
    AJSONConfig.SetValue(CONFIG_AUTOEXPANSIONFOLDER, Self.AutoExpansionFolder);
    AJSONConfig.SetValue(CONFIG_CMHIDEEJECTMENUITEM, Self.CMHideEjectMenuItem);

    //Graphic Menu
    AJSONConfig.SetValue(CONFIG_GMFADE, Self.GMFade);
    AJSONConfig.SetValue(CONFIG_GMSMALLICONSIZE, Self.GMSmallIconSize);
    AJSONConfig.SetValue(CONFIG_GMPERSONALPICTURE, Self.GMPersonalPicture);
    AJSONConfig.SetValue(CONFIG_GMPOSITIONTOP, Self.GMPositionTop);
    AJSONConfig.SetValue(CONFIG_GMPOSITIONLEFT, Self.GMPositionLeft);
    AJSONConfig.SetValue(CONFIG_GMSHOWUSERPICTURE, Self.GMShowUserPicture);
    AJSONConfig.SetValue(CONFIG_GMHIDEEJECTBUTTON, Self.GMHideEjectButton);

    //Right buttons
    AJSONConfig.SetValue(CONFIG_GMBTNDOCUMENTS, Self.GMBtnDocuments);
    AJSONConfig.SetValue(CONFIG_GMBTNPICTURES, Self.GMBtnPictures);
    AJSONConfig.SetValue(CONFIG_GMBTNMUSIC, Self.GMBtnMusic);
    AJSONConfig.SetValue(CONFIG_GMBTNVIDEOS, Self.GMBtnVideos);
    AJSONConfig.SetValue(CONFIG_GMBTNEXPLORE, Self.GMBtnExplore);

    //HotKeys
    AJSONConfig.SetValue(CONFIG_HOTKEY, Self.HotKey);
    AJSONConfig.SetValue(CONFIG_WINDOWHOTKEY, Self.WindowHotKey);
    AJSONConfig.SetValue(CONFIG_MENUHOTKEY, Self.GraphicMenuHotKey);
    AJSONConfig.SetValue(CONFIG_CLASSICMENUHOTKEY, Self.ClassicMenuHotkey);

    // Misc
    AJSONConfig.SetValue(CONFIG_SCANFOLDERAUTOEXTRACTNAME, Self.ScanFolderAutoExtractName);
    AJSONConfig.SetValue(CONFIG_SCANFOLDERFILETYPES, Self.ScanFolderFileTypes);
    AJSONConfig.SetValue(CONFIG_SCANFOLDEREXCLUDENAMES, Self.ScanFolderExcludeNames);
  finally
    Self.Changed := False;
  end;
end;

procedure TConfiguration.RestoreSettings(AJSONConfig: TJSONConfig);
var
  nLeft, nTop, nWidth, nHeight: Integer;
  tempTypes: TStringList;
  tempFont: Graphics.TFont;
begin
  Self.BeginUpdate;

  try
    //Get GMTheme before everything (so ASuite know where icons folder)
    Self.GMTheme := AJSONConfig.GetValue(CONFIG_GMTHEME, Self.GMTheme);
    TASuiteLogger.Info('Loaded GraphicMenu theme = %s', [Self.GMTheme]);

    //General
    Self.StartWithWindows          := AJSONConfig.GetValue(CONFIG_STARTWITHWINDOWS, Self.StartWithWindows);
    Self.ShowPanelAtStartUp        := AJSONConfig.GetValue(CONFIG_SHOWPANELATSTARTUP, Self.ShowPanelAtStartUp);
    Self.ShowGraphicMenuAtStartUp  := AJSONConfig.GetValue(CONFIG_SHOWMENUATSTARTUP, Self.ShowGraphicMenuAtStartUp);
    Self.MissedSchedulerTask       := AJSONConfig.GetValue(CONFIG_MISSEDSCHEDULERTASK, Self.MissedSchedulerTask);
    Self.ShowGraphicMenuAnotherInstance := AJSONConfig.GetValue(CONFIG_SECONDINSTANCEGM, Self.ShowGraphicMenuAnotherInstance);
    Self.ConfirmMsgCloseApp := AJSONConfig.GetValue(CONFIG_CONFIRMCLOSEASUITE, Self.ConfirmMsgCloseApp);

    // Main Form
    Self.LangID                    := AJSONConfig.GetValue(CONFIG_LANGID, Self.LangID);
    Self.UseCustomTitle            := AJSONConfig.GetValue(CONFIG_USECUSTOMTITLE, Self.UseCustomTitle);
    Self.CustomTitleString         := AJSONConfig.GetValue(CONFIG_CUSTOMTITLESTRING, Self.CustomTitleString);
    Self.HideTabSearch             := AJSONConfig.GetValue(CONFIG_HIDETABSEARCH, Self.HideTabSearch);
    Self.SearchAsYouType           := AJSONConfig.GetValue(CONFIG_SEARCHASYOUTYPE, Self.SearchAsYouType);
    Self.SearchNameColWidth        := AJSONConfig.GetValue(CONFIG_SEARCH_NAME_WIDTH, Self.SearchNameColWidth);
    Self.SearchCategoryColWidth    := AJSONConfig.GetValue(CONFIG_SEARCH_CATEGORY_WIDTH, Self.SearchCategoryColWidth);
    Self.HoldSize                  := AJSONConfig.GetValue(CONFIG_HOLDSIZE, Self.HoldSize);
    Self.AlwaysOnTop               := AJSONConfig.GetValue(CONFIG_ALWAYSONTOP, Self.AlwaysOnTop);
    Self.DialogCenterMF            := AJSONConfig.GetValue(CONFIG_MAINFORM_DIALOGS_CENTER, Self.DialogCenterMF);

    // Main Form - Position and size
    Self.MainFormLeft   := AJSONConfig.GetValue(CONFIG_MAINFORM_LEFT, Self.MainFormLeft);
    Self.MainFormTop    := AJSONConfig.GetValue(CONFIG_MAINFORM_TOP, Self.MainFormTop);
    Self.MainFormWidth  := AJSONConfig.GetValue(CONFIG_MAINFORM_WIDTH, Self.MainFormWidth);
    Self.MainFormHeight := AJSONConfig.GetValue(CONFIG_MAINFORM_HEIGHT, Self.MainFormHeight);

    // Main Form - Treevew
    Self.TVBackground              := AJSONConfig.GetValue(CONFIG_TVBACKGROUND, Self.TVBackground);
    Self.TVBackgroundPath          := AJSONConfig.GetValue(CONFIG_TVBACKGROUNDPATH, Self.TVBackgroundPath);
    Self.TVSmallIconSize           := AJSONConfig.GetValue(CONFIG_TVSMALLICONSIZE, Self.TVSmallIconSize);
    Self.TVAutoOpClCats            := AJSONConfig.GetValue(CONFIG_TVAUTOOPCLCATS, Self.TVAutoOpClCats);
    Self.TVAutoOpCatsDrag          := AJSONConfig.GetValue(CONFIG_TVAUTOOPCATSDRAG, Self.TVAutoOpCatsDrag);
    Self.TVDisableConfirmDelete    := AJSONConfig.GetValue(CONFIG_TVDISABLECONFIRMDELETE, Self.TVDisableConfirmDelete);

    // Main Form - TVFont
    tempFont := Graphics.TFont.Create;
    try
      tempFont.Assign(Self.TVFont);
      tempFont.Name  := AJSONConfig.GetValue(CONFIG_TVFONTNAME, UnicodeString(Self.TVFont.Name));
      tempFont.Color := HtmlToColor(AJSONConfig.GetValue(CONFIG_TVFONTCOLOR, ColorToHtml(Self.TVFont.Color)));
      tempFont.Size  := AJSONConfig.GetValue(CONFIG_TVFONTSIZE, Self.TVFont.Size);
      tempFont.Style := [];
      if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_BOLD, (fsBold in Self.TVFont.Style)) then
         tempFont.Style := tempFont.Style + [fsBold];
      if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_ITALIC, (fsItalic in Self.TVFont.Style)) then
         tempFont.Style := tempFont.Style + [fsItalic];
      if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_UNDERLINE, (fsUnderline in Self.TVFont.Style)) then
         tempFont.Style := tempFont.Style + [fsUnderline];
      if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_STRIKEOUT, (fsStrikeOut in Self.TVFont.Style)) then
         tempFont.Style := tempFont.Style + [fsStrikeOut];
      Self.TVFont := tempFont;
    finally
      tempFont.Free;
    end;

    // MRU
    Self.MRU                       := AJSONConfig.GetValue(CONFIG_MRU, Self.MRU);
    Self.SubMenuMRU                := AJSONConfig.GetValue(CONFIG_SUBMENUMRU, Self.SubMenuMRU);
    Self.MRUNumber                 := AJSONConfig.GetValue(CONFIG_MRUNUMBER, Self.MRUNumber);

    // MFU
    Self.MFU                       := AJSONConfig.GetValue(CONFIG_MFU, Self.MFU);
    Self.SubMenuMFU                := AJSONConfig.GetValue(CONFIG_SUBMENUMFU, Self.SubMenuMFU);
    Self.MFUNumber                 := AJSONConfig.GetValue(CONFIG_MFUNUMBER, Self.MFUNumber);

    // Backup
    Self.Backup                    := AJSONConfig.GetValue(CONFIG_BACKUP, Self.Backup);
    Self.BackupNumber              := AJSONConfig.GetValue(CONFIG_BACKUPNUMBER, Self.BackupNumber);

    // Other functions
    Self.AutorunStartup            := AJSONConfig.GetValue(CONFIG_AUTORUNSTARTUP, Self.AutorunStartup);
    Self.AutorunShutdown           := AJSONConfig.GetValue(CONFIG_AUTORUNSHUTDOWN, Self.AutorunShutdown);
    Self.Cache                     := AJSONConfig.GetValue(CONFIG_CACHE, Self.Cache);
    Self.Scheduler                 := AJSONConfig.GetValue(CONFIG_SCHEDULER, Self.Scheduler);

    // Execution
    Self.ActionOnExe               := TActionOnExecute(AJSONConfig.GetValue(CONFIG_ACTIONONEXE, Integer(Self.ActionOnExe)));
    Self.RunSingleClick            := AJSONConfig.GetValue(CONFIG_RUNSINGLECLICK, Self.RunSingleClick);
    Self.ConfirmRunCat             := AJSONConfig.GetValue(CONFIG_CONFIRMMESSAGECAT, Self.ConfirmRunCat);
    Self.AutoCloseProcess          := AJSONConfig.GetValue(CONFIG_AUTOCLOSEPROCESS, Self.AutoCloseProcess);

    // Trayicon
    Self.TrayIcon                  := AJSONConfig.GetValue(CONFIG_TRAYICON, Self.TrayIcon);
    Self.TrayUseCustomIcon         := AJSONConfig.GetValue(CONFIG_TRAYUSECUSTOMICON, Self.TrayUseCustomIcon);
    Self.TrayCustomIconPath        := AJSONConfig.GetValue(CONFIG_TRAYCUSTOMICONPATH, Self.TrayCustomIconPath);
    Self.ActionClickLeft           := TTrayiconActionClick(AJSONConfig.GetValue(CONFIG_ACTIONCLICKLEFT, Integer(Self.ActionClickLeft)));
    Self.ActionClickMiddle         := TTrayiconActionClick(AJSONConfig.GetValue(CONFIG_ACTIONCLICKMIDDLE, Integer(Self.ActionClickMiddle)));
    Self.ActionClickRight          := TTrayiconActionClick(AJSONConfig.GetValue(CONFIG_ACTIONCLICKRIGHT, Integer(Self.ActionClickRight)));
    Self.AutoExpansionFolder       := AJSONConfig.GetValue(CONFIG_AUTOEXPANSIONFOLDER, Self.AutoExpansionFolder);
    Self.CMHideEjectMenuItem       := AJSONConfig.GetValue(CONFIG_CMHIDEEJECTMENUITEM, Self.CMHideEjectMenuItem);

    //Graphic Menu
    Self.GMFade                    := AJSONConfig.GetValue(CONFIG_GMFADE, Self.GMFade);
    Self.GMSmallIconSize           := AJSONConfig.GetValue(CONFIG_GMSMALLICONSIZE, Self.GMSmallIconSize);
    Self.GMPersonalPicture         := AJSONConfig.GetValue(CONFIG_GMPERSONALPICTURE, Self.GMPersonalPicture);
    Self.GMPositionTop             := AJSONConfig.GetValue(CONFIG_GMPOSITIONTOP, Self.GMPositionTop);
    Self.GMPositionLeft            := AJSONConfig.GetValue(CONFIG_GMPOSITIONLEFT, Self.GMPositionLeft);
    Self.GMShowUserPicture         := AJSONConfig.GetValue(CONFIG_GMSHOWUSERPICTURE, Self.GMShowUserPicture);
    Self.GMHideEjectButton         := AJSONConfig.GetValue(CONFIG_GMHIDEEJECTBUTTON, Self.GMHideEjectButton);

    //Right buttons
    Self.GMBtnDocuments            := AJSONConfig.GetValue(CONFIG_GMBTNDOCUMENTS, Self.GMBtnDocuments);
    Self.GMBtnPictures             := AJSONConfig.GetValue(CONFIG_GMBTNPICTURES, Self.GMBtnPictures);
    Self.GMBtnMusic                := AJSONConfig.GetValue(CONFIG_GMBTNMUSIC, Self.GMBtnMusic);
    Self.GMBtnVideos               := AJSONConfig.GetValue(CONFIG_GMBTNVIDEOS, Self.GMBtnVideos);
    Self.GMBtnExplore              := AJSONConfig.GetValue(CONFIG_GMBTNEXPLORE, Self.GMBtnExplore);

    //HotKeys
    Self.HotKey                    := AJSONConfig.GetValue(CONFIG_HOTKEY, Self.HotKey);
    Self.WindowHotKey              := AJSONConfig.GetValue(CONFIG_WINDOWHOTKEY, Self.WindowHotKey);
    Self.GraphicMenuHotKey         := AJSONConfig.GetValue(CONFIG_MENUHOTKEY, Self.GraphicMenuHotKey);
    Self.ClassicMenuHotkey         := AJSONConfig.GetValue(CONFIG_CLASSICMENUHOTKEY, Self.ClassicMenuHotkey);

    // Misc
    Self.ScanFolderAutoExtractName := AJSONConfig.GetValue(CONFIG_SCANFOLDERAUTOEXTRACTNAME, Self.ScanFolderAutoExtractName);
    AJSONConfig.GetValue(CONFIG_SCANFOLDEREXCLUDENAMES, Self.ScanFolderExcludeNames, Self.ScanFolderExcludeNames);

    //Workaround for missing types
    tempTypes := TStringList.Create;
    try
      AJSONConfig.GetValue(CONFIG_SCANFOLDERFILETYPES, tempTypes, tempTypes);
      if tempTypes.count > 0 then
        Self.ScanFolderFileTypes.Assign(tempTypes);
    finally
      tempTypes.Free;
    end;

    Self.AfterUpdateConfig;
  finally
    Self.EndUpdate;
  end;
end;

procedure TConfiguration.SetHotKey(const Value: Boolean);
begin
  if (FHotKey <> Value) then
  begin
    FHotKey := Value;
    ASuiteManager.ListManager.HotKeyItemList.RefreshRegs;
    NotifyObservers('HotKey');
  end;
end;

procedure TConfiguration.LoadConfig;
var
  JSONConfig: TJSONConfig;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TConfiguration.LoadConfig', Self);
  TASuiteLogger.Debug('Settings file = %s', [ASuiteInstance.Paths.SuitePathSettings]);

  //if FileExists(ASuiteInstance.Paths.SuitePathSettings) then
  //begin
  //  TASuiteLogger.Info('Found ASuite configuration', []);
    try
      JSONConfig := TJSONConfig.Create(nil);
      JSONConfig.Formatted := True;
      JSONConfig.FormatIndentsize := 4;    
      JSONConfig.Filename := ASuiteInstance.Paths.SuitePathSettings; 
      RestoreSettings(JSONConfig);
    finally
      JSONConfig.Free;
    end;
  //end
  //else   
  //  TASuiteLogger.Info('ASuite Configuration not found', []);
end;

procedure TConfiguration.SaveConfig;  
var
  JSONConfig: TJSONConfig;
  {%H-}log: ISynLog;
begin
  //If settings is changed, insert it else (if it exists) update it
  if Config.Changed then
  begin
    log := TASuiteLogger.Enter('TConfiguration.SaveConfig', Self);
    try
      JSONConfig := TJSONConfig.Create(nil);
      JSONConfig.Formatted := True;
      JSONConfig.FormatIndentsize := 4;
      //TODO: Check this. If asuite uses a different folder for settings? ex. appdata
      JSONConfig.Filename := ASuiteInstance.Paths.SuitePathSettings;
      SaveSettings(JSONConfig);
    finally
      JSONConfig.Free;
    end;
  end;
end;

procedure TConfiguration.SetAlwaysOnTop(value: Boolean);
begin
  if FAlwaysOnTop <> value then
  begin
    FAlwaysOnTop := value;
    NotifyObservers('AlwaysOnTop');
  end;
end;

procedure TConfiguration.SetBackup(const Value: Boolean);
begin
  FBackup := Value;
  if FBackup then
    ASuiteInstance.Paths.CheckBackupFolder;
  NotifyObservers('Backup');
end;

procedure TConfiguration.SetBackupNumber(const Value: Integer);
begin
  if Value < FBackupNumber then
    DeleteOldBackups(Value);
  FBackupNumber := Value;
  NotifyObservers('BackupNumber');
end;

procedure TConfiguration.SetScheduler(value: Boolean);
begin
  FScheduler := value;
  ASuiteInstance.Scheduler.Timer.Enabled := FScheduler;
  NotifyObservers('Scheduler');
end;

procedure TConfiguration.SetTrayIcon(value: Boolean);
begin
  if not CheckSysTray then
    FTrayIcon := False
  else
    FTrayIcon := value;

  if (not(FShowPanelAtStartUp)) and (not(FTrayicon)) then
    FShowPanelAtStartUp := True;
  NotifyObservers('TrayIcon');
end;

procedure TConfiguration.SetTVBackgroundPath(AValue: String);
begin
  if FTVBackgroundPath = AValue then Exit;
  FTVBackgroundPath := AValue;

  Self.TVBackground := FTVBackground;
  NotifyObservers('TVBackgroundPath');
end;

procedure TConfiguration.SetUseCustomTitle(value: Boolean);
begin
  FUseCustomTitle := value;
  NotifyObservers('UseCustomTitle');
end;

procedure TConfiguration.SetWindowHotKey(const Value: string);
begin
  if not UpdateHotkey(FWindowHotKey, Value, frmMainID) then
    ShowMessageEx(msgErrRegWindowHotkey, true);
  FWindowHotKey := Value;
  NotifyObservers('WindowHotKey');
end;

procedure TConfiguration.SetTVAutoOpClCats(value: Boolean);
begin
  FTVAutoOpClCats := value;
  NotifyObservers('TVAutoOpClCats');
end;

procedure TConfiguration.SetHideTabSearch(value: Boolean);
begin
  FHideTabSearch := value;
  NotifyObservers('HideTabSearch');
end;

procedure TConfiguration.SetShowPanelAtStartUp(value: Boolean);
begin
  FShowPanelAtStartUp := value;
  NotifyObservers('ShowPanelAtStartUp');
end;

procedure TConfiguration.SetTVBackground(value: Boolean);
begin
  FTVBackground := value;
  NotifyObservers('TVBackground');
end;

function TConfiguration.LoadPngAndConvertBMP(const APath: String): TBitmap;
var
  BackgroundPNG: TPortableNetworkGraphic;
begin
  Result := Graphics.TBitmap.Create;

  BackgroundPNG := TPortableNetworkGraphic.Create;
  try
    BackgroundPNG.LoadFromFile(APath);
    Result.Assign(BackgroundPNG);
  finally
    BackgroundPNG.Free;
  end;
end;

procedure TConfiguration.SetTVFont(value: Graphics.TFont);
begin
  if not FTVFont.IsEqual(value) then
  begin
    FTVFont.Name  := value.Name;
    FTVFont.Style := value.Style;
    FTVFont.Size  := value.Size;
    FTVFont.Color := value.Color;

    NotifyObservers('TVFont');
  end;
end;

procedure TConfiguration.SetTVSmallIconSize(const Value: Boolean);
begin
  //If new value is different than old value, full collapse Tree and get icons
  if FTVSmallIconSize <> Value then
  begin
    FTVSmallIconSize := Value;
    NotifyObservers('TVSmallIconSize');
  end;
end;

procedure TConfiguration.SetCache(value: boolean);
begin
  FCache := value;
  //If disabled, delete all file icon-cache and folders cache
  if Not(FCache) then
    ASuiteInstance.Paths.RemoveCacheFolders
  else //Else create folders cache
    ASuiteInstance.Paths.CheckCacheFolders;
  NotifyObservers('Cache');
end;

procedure TConfiguration.SetChanged(const Value: Boolean);
begin
  FChanged := Value;
end;

procedure TConfiguration.SetClassicMenuHotkey(const Value: string);
begin
  if not UpdateHotkey(FClassicMenuHotkey, Value, frmCMenuID) then
    ShowMessageEx(msgErrRegCMHotkey, true);
  FClassicMenuHotKey := Value;
  NotifyObservers('ClassicMenuHotkey');
end;

procedure TConfiguration.SetGMBtnDocuments(Value: string);
begin
  if value = '' then
    FGMBtnDocuments := '%USERPROFILE%\Documents'
  else
    FGMBtnDocuments := value;
  NotifyObservers('GMBtnDocuments');
end;

procedure TConfiguration.SetGMBtnExplore(Value: string);
begin
  if value = '' then
    FGMBtnExplore := '$drive\'
  else
    FGMBtnExplore := value;
  NotifyObservers('GMBtnExplore');
end;

procedure TConfiguration.SetGMBtnMusic(Value: string);
begin
  if value = '' then
    FGMBtnMusic := '%USERPROFILE%\Music'
  else
    FGMBtnMusic := value;
  NotifyObservers('GMBtnMusic');
end;

procedure TConfiguration.SetGMBtnPictures(Value: string);
begin
  if value = '' then
    FGMBtnPictures := '%USERPROFILE%\Pictures'
  else
    FGMBtnPictures := value;
  NotifyObservers('GMBtnPictures');
end;

procedure TConfiguration.SetGMBtnVideos(Value: string);
begin
  if value = '' then
    FGMBtnVideos := '%USERPROFILE%\Videos'
  else
    FGMBtnVideos := value;
  NotifyObservers('GMBtnVideos');
end;

procedure TConfiguration.SetGMTheme(value: string);
begin
  if value = '' then
    FGMTheme := 'default'
  else
    FGMTheme := value;
  NotifyObservers('GMTheme');
end;

procedure TConfiguration.SetStartWithWindows(value: boolean);
begin
  FStartWithWindows := value;
  if FStartWithWindows then
    SetASuiteAtOsStartup
  else
    DeleteASuiteAtOsStartup;
  NotifyObservers('StartWithWindows');
end;

procedure TConfiguration.SetLangID(value: String);
begin
  if (value <> '') then
    FLangID := value
  else
    FLangID := 'en';
  NotifyObservers('LangID');
end;

procedure TConfiguration.SetGraphicMenuHotKey(const Value: string);
begin
  if not UpdateHotkey(FGraphicMenuHotKey, Value, frmGMenuID) then
    ShowMessageEx(msgErrRegGMHotkey, true);
  FGraphicMenuHotKey := Value;
  NotifyObservers('GraphicMenuHotKey');
end;

procedure TConfiguration.SetSearchNameColWidth(const Value: Integer);
begin
  if FSearchNameColWidth <> Value then
  begin
    FSearchNameColWidth := Value;
    NotifyObservers('SearchNameColWidth');
  end;
end;

procedure TConfiguration.SetSearchCategoryColWidth(const Value: Integer);
begin
  if FSearchCategoryColWidth <> Value then
  begin
    FSearchCategoryColWidth := Value;
    NotifyObservers('SearchCategoryColWidth');
  end;
end;

procedure TConfiguration.SetMainFormLeft(const Value: Integer);
begin
  if FMainFormLeft <> Value then
  begin
    FMainFormLeft := Value;
    NotifyObservers('MainFormLeft');
  end;
end;

procedure TConfiguration.SetMainFormTop(const Value: Integer);
begin
  if FMainFormTop <> Value then
  begin
    FMainFormTop := Value;
    NotifyObservers('MainFormTop');
  end;
end;

procedure TConfiguration.SetMainFormWidth(const Value: Integer);
begin
  if FMainFormWidth <> Value then
  begin
    FMainFormWidth := Value;
    NotifyObservers('MainFormWidth');
  end;
end;

procedure TConfiguration.SetMainFormHeight(const Value: Integer);
begin
  if FMainFormHeight <> Value then
  begin
    FMainFormHeight := Value;
    NotifyObservers('MainFormHeight');
  end;
end;

procedure TConfiguration.AddObserver(const Observer: IConfigObserver);
begin
  if (FObservers.IndexOf(Observer) = -1) then
    FObservers.Add(Observer);
end;

procedure TConfiguration.RemoveObserver(const Observer: IConfigObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TConfiguration.NotifyObservers(const PropertyName: string = '');
var
  I: Integer;
  Observer: IConfigObserver;
begin
  // If batching, just record the property name (if not already present)
  if FNotificationBatchCount > 0 then
  begin
    if (PropertyName <> '') and (FBatchedProperties.IndexOf(PropertyName) = -1) then
      FBatchedProperties.Add(PropertyName);
    Exit;
  end;

  // Otherwise, notify all observers immediately
  for I := 0 to FObservers.Count - 1 do
    if Supports(FObservers[i], IConfigObserver, Observer) then
      Observer.ConfigChanged(PropertyName);
end;

function TConfiguration.CheckGnomeExtras: Boolean;
var
  H : TLibHandle;

  function FindInStringList(aList: TStringList; aString: String): Integer;
  var
    I: Integer = 0;
  begin
    while I < aList.Count do
    begin
      if pos(aString, AList.Strings[I]) > 0 then
        Exit(I);

      Inc(I);
    end;

    Result := -1;
  end;

  function CheckPlugIn(PlugInName : string) : boolean;
  var
    AProcess: TProcess;
    List : TStringList = nil;
  begin
    result := false;

    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable:= 'gnome-extensions';
      AProcess.Parameters.Add('info');
      AProcess.Parameters.Add(PlugInName);

      AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];

      AProcess.Execute;

      if (AProcess.ExitStatus = 0) then
      begin
        List := TStringList.Create;
        List.LoadFromStream(AProcess.Output);

        if FindInStringList(List, 'ENABLED') > -1 then
          result := true;
      end;
    finally
      freeandnil(List);
      freeandnil(AProcess);
    end;
  end;

begin
  Result := false;

  H := LoadLibrary('libappindicator3.so.1');
  if H = NilHandle then
  begin
    TASuiteLogger.Info('Failed to Find libappindicator3, SysTray may not work.', []);
    exit(False);
  end;
  unloadLibrary(H);

  if CheckPlugIn('ubuntu-appindicators@ubuntu.com') or            // Ubuntu, Debian
     CheckPlugIn('appindicatorsupport@rgcjonas.gmail.com') then  // Fedora
    Result := True;

  if not Result then
    TASuiteLogger.Info('Failed to Find an enabled appindicator plugin, SysTray may not work.', []);
end;

function TConfiguration.CheckSysTray: Boolean;
{$IFDEF LINUX}
var
  A : TAtom;
  XDisplay: PDisplay;
{$ENDIF}
begin
  {$IFDEF LINUX}
    {$IFDEF LCLGTK2}
  XDisplay := gdk_display;
    {$ENDIF}

    {$IFDEF LCLQT5}
  XDisplay := QX11Info_display;
    {$ENDIF}

    {$IFDEF LCLQT6}
  XDisplay := QtWidgetSet.x11Display;
    {$ENDIF}

    {$IFDEF LCLGTK3}
  XDisplay := gdk_x11_display_get_xdisplay(gdk_window_get_display(gdk_get_default_root_window));
    {$ENDIF}

  Result := False;
  if XDisplay <> nil then
  begin
    A := XInternAtom(XDisplay, '_NET_SYSTEM_TRAY_S0', False);
    Result := (XGetSelectionOwner(XDisplay, A) <> 0);
  end;

  if not Result then
    Result := CheckGnomeExtras; // Thats libappindicator3 and an installed and enabled gnome-shell-extension-appindicator
  {$ELSE}
  //Windows is always true!
  Result := True;
  {$ENDIF}
end;

procedure TConfiguration.SetShowGraphicMenuAtStartUp(const Value: Boolean);
begin
  if FShowGraphicMenuAtStartUp <> Value then
  begin
    FShowGraphicMenuAtStartUp := Value;
    NotifyObservers('ShowGraphicMenuAtStartUp');
  end;
end;

procedure TConfiguration.SetMissedSchedulerTask(const Value: Boolean);
begin
  if FMissedSchedulerTask <> Value then
  begin
    FMissedSchedulerTask := Value;
    NotifyObservers('MissedSchedulerTask');
  end;
end;

procedure TConfiguration.SetShowGraphicMenuAnotherInstance(const Value: Boolean);
begin
  if FShowGraphicMenuAnotherInstance <> Value then
  begin
    FShowGraphicMenuAnotherInstance := Value;
    NotifyObservers('ShowGraphicMenuAnotherInstance');
  end;
end;

procedure TConfiguration.SetConfirmMsgCloseApp(const Value: Boolean);
begin
  if FConfirmMsgCloseApp <> Value then
  begin
    FConfirmMsgCloseApp := Value;
    NotifyObservers('ConfirmMsgCloseApp');
  end;
end;

procedure TConfiguration.SetCustomTitleString(const Value: String);
begin
  if FCustomTitleString <> Value then
  begin
    FCustomTitleString := Value;
    NotifyObservers('CustomTitleString');
  end;
end;

procedure TConfiguration.SetSearchAsYouType(const Value: Boolean);
begin
  if FSearchAsYouType <> Value then
  begin
    FSearchAsYouType := Value;
    NotifyObservers('SearchAsYouType');
  end;
end;

procedure TConfiguration.SetDialogCenterMF(const Value: Boolean);
begin
  if FDialogCenterMF <> Value then
  begin
    FDialogCenterMF := Value;
    NotifyObservers('DialogCenterMF');
  end;
end;

procedure TConfiguration.SetTVAutoOpCatsDrag(const Value: Boolean);
begin
  if FTVAutoOpCatsDrag <> Value then
  begin
    FTVAutoOpCatsDrag := Value;
    NotifyObservers('TVAutoOpCatsDrag');
  end;
end;

procedure TConfiguration.SetTVDisableConfirmDelete(const Value: Boolean);
begin
  if FTVDisableConfirmDelete <> Value then
  begin
    FTVDisableConfirmDelete := Value;
    NotifyObservers('TVDisableConfirmDelete');
  end;
end;

procedure TConfiguration.SetMRU(const Value: Boolean);
begin
  if FMRU <> Value then
  begin
    FMRU := Value;
    NotifyObservers('MRU');
  end;
end;

procedure TConfiguration.SetSubMenuMRU(const Value: Boolean);
begin
  if FSubMenuMRU <> Value then
  begin
    FSubMenuMRU := Value;
    NotifyObservers('SubMenuMRU');
  end;
end;

procedure TConfiguration.SetMRUNumber(const Value: Integer);
begin
  if FMRUNumber <> Value then
  begin
    FMRUNumber := Value;
    NotifyObservers('MRUNumber');
  end;
end;

procedure TConfiguration.SetMFU(const Value: Boolean);
begin
  if FMFU <> Value then
  begin
    FMFU := Value;
    NotifyObservers('MFU');
  end;
end;

procedure TConfiguration.SetSubMenuMFU(const Value: Boolean);
begin
  if FSubMenuMFU <> Value then
  begin
    FSubMenuMFU := Value;
    NotifyObservers('SubMenuMFU');
  end;
end;

procedure TConfiguration.SetMFUNumber(const Value: Integer);
begin
  if FMFUNumber <> Value then
  begin
    FMFUNumber := Value;
    NotifyObservers('MFUNumber');
  end;
end;

procedure TConfiguration.SetAutorunStartup(const Value: Boolean);
begin
  if FAutorunStartup <> Value then
  begin
    FAutorunStartup := Value;
    NotifyObservers('AutorunStartup');
  end;
end;

procedure TConfiguration.SetAutorunShutdown(const Value: Boolean);
begin
  if FAutorunShutdown <> Value then
  begin
    FAutorunShutdown := Value;
    NotifyObservers('AutorunShutdown');
  end;
end;

procedure TConfiguration.SetActionOnExe(const Value: TActionOnExecute);
begin
  if FActionOnExe <> Value then
  begin
    FActionOnExe := Value;
    NotifyObservers('ActionOnExe');
  end;
end;

procedure TConfiguration.SetRunSingleClick(const Value: Boolean);
begin
  if FRunSingleClick <> Value then
  begin
    FRunSingleClick := Value;
    NotifyObservers('RunSingleClick');
  end;
end;

procedure TConfiguration.SetConfirmRunCat(const Value: Boolean);
begin
  if FConfirmRunCat <> Value then
  begin
    FConfirmRunCat := Value;
    NotifyObservers('ConfirmRunCat');
  end;
end;

procedure TConfiguration.SetAutoCloseProcess(const Value: Boolean);
begin
  if FAutoCloseProcess <> Value then
  begin
    FAutoCloseProcess := Value;
    NotifyObservers('AutoCloseProcess');
  end;
end;

procedure TConfiguration.SetTrayUseCustomIcon(const Value: Boolean);
begin
  if FTrayUseCustomIcon <> Value then
  begin
    FTrayUseCustomIcon := Value;
    NotifyObservers('TrayUseCustomIcon');
  end;
end;

procedure TConfiguration.SetTrayCustomIconPath(const Value: String);
begin
  if FTrayCustomIconPath <> Value then
  begin
    FTrayCustomIconPath := Value;
    NotifyObservers('TrayCustomIconPath');
  end;
end;

procedure TConfiguration.SetActionClickLeft(const Value: TTrayiconActionClick);
begin
  if FActionClickLeft <> Value then
  begin
    FActionClickLeft := Value;
    NotifyObservers('ActionClickLeft');
  end;
end;

procedure TConfiguration.SetActionClickMiddle(const Value: TTrayiconActionClick);
begin
  if FActionClickMiddle <> Value then
  begin
    FActionClickMiddle := Value;
    NotifyObservers('ActionClickMiddle');
  end;
end;

procedure TConfiguration.SetActionClickRight(const Value: TTrayiconActionClick);
begin
  if FActionClickRight <> Value then
  begin
    FActionClickRight := Value;
    NotifyObservers('ActionClickRight');
  end;
end;

procedure TConfiguration.SetAutoExpansionFolder(const Value: Boolean);
begin
  if FAutoExpansionFolder <> Value then
  begin
    FAutoExpansionFolder := Value;
    NotifyObservers('AutoExpansionFolder');
  end;
end;

procedure TConfiguration.SetCMHideEjectMenuItem(const Value: Boolean);
begin
  if FCMHideEjectMenuItem <> Value then
  begin
    FCMHideEjectMenuItem := Value;
    NotifyObservers('CMHideEjectMenuItem');
  end;
end;

procedure TConfiguration.SetGMFade(const Value: Boolean);
begin
  if FGMFade <> Value then
  begin
    FGMFade := Value;
    NotifyObservers('GMFade');
  end;
end;

procedure TConfiguration.SetGMSmallIconSize(const Value: Boolean);
begin
  if FGMSmallIconSize <> Value then
  begin
    FGMSmallIconSize := Value;
    NotifyObservers('GMSmallIconSize');
  end;
end;

procedure TConfiguration.SetGMPersonalPicture(const Value: string);
begin
  if FGMPersonalPicture <> Value then
  begin
    FGMPersonalPicture := Value;
    NotifyObservers('GMPersonalPicture');
  end;
end;

procedure TConfiguration.SetGMPositionTop(const Value: Integer);
begin
  if FGMPositionTop <> Value then
  begin
    FGMPositionTop := Value;
    NotifyObservers('GMPositionTop');
  end;
end;

procedure TConfiguration.SetGMPositionLeft(const Value: Integer);
begin
  if FGMPositionLeft <> Value then
  begin
    FGMPositionLeft := Value;
    NotifyObservers('GMPositionLeft');
  end;
end;

procedure TConfiguration.SetGMShowUserPicture(const Value: Boolean);
begin
  if FGMShowUserPicture <> Value then
  begin
    FGMShowUserPicture := Value;
    NotifyObservers('GMShowUserPicture');
  end;
end;

procedure TConfiguration.SetGMHideEjectButton(const Value: Boolean);
begin
  if FGMHideEjectButton <> Value then
  begin
    FGMHideEjectButton := Value;
    NotifyObservers('GMHideEjectButton');
  end;
end;

procedure TConfiguration.SetScanFolderAutoExtractName(const Value: Boolean);
begin
  if FScanFolderAutoExtractName <> Value then
  begin
    FScanFolderAutoExtractName := Value;
    NotifyObservers('ScanFolderAutoExtractName');
  end;
end;

procedure TConfiguration.SetScanFolderFileTypes(const Value: TStringList);
begin
  if not FScanFolderFileTypes.Equals(Value) then
  begin
    FScanFolderFileTypes.Assign(Value);
    NotifyObservers('ScanFolderFileTypes');
  end;
end;

procedure TConfiguration.SetScanFolderExcludeNames(const Value: TStringList);
begin
  if not FScanFolderExcludeNames.Equals(Value) then
  begin
    FScanFolderExcludeNames.Assign(Value);
    NotifyObservers('ScanFolderExcludeNames');
  end;
end;

initialization
  Config := TConfiguration.Create;

finalization
  FreeAndNil(Config);

end.
