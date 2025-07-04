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

interface

uses
  LCLIntf, LCLType, SysUtils, Graphics, Forms, Controls, VirtualTrees, Kernel.Enumerations,
  Classes, jsonConf, LazFileUtils, Kernel.Logger, BGRABitmap, Dialogs;

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
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    FDialogCenterMF     : Boolean;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : string;
    FTVAutoOpClCats     : Boolean; //Automatic Opening/closing categories
    FTVAutoOpCatsDrag   : Boolean;
    FTVFont             : TFont;
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

    function LoadPngAndConvertBMP(const APath: String): TBitmap;
    function LoadTrayIconFromFile(const APath: string): TBGRABitmap;
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
    procedure SetTVFont(value: TFont);
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

    procedure SetASuiteState(const Value: TLauncherState);
    function UpdateHotkey(OldValue, NewValue: String; Tag: Integer): Boolean;
    function isValidHotkeyString(AValue: String): Boolean;
    procedure UpdateTrayIcon;
  public
    { public declarations }
    constructor Create; overload;
    destructor Destroy; override;

    //General
    property StartWithWindows: Boolean read FStartWithWindows write SetStartWithWindows;
    property ShowPanelAtStartUp: Boolean read FShowPanelAtStartUp write SetShowPanelAtStartUp;
    property ShowGraphicMenuAtStartUp: Boolean read FShowGraphicMenuAtStartUp write FShowGraphicMenuAtStartUp;
    property MissedSchedulerTask: Boolean read FMissedSchedulerTask write FMissedSchedulerTask;
    property ShowGraphicMenuAnotherInstance: Boolean read FShowGraphicMenuAnotherInstance write FShowGraphicMenuAnotherInstance;
    property ConfirmMsgCloseApp: Boolean read FConfirmMsgCloseApp write FConfirmMsgCloseApp;
    // Main Form
    property LangID: String read FLangID write SetLangID;
    property UseCustomTitle: Boolean read FUseCustomTitle write SetUseCustomTitle;
    property CustomTitleString : String read FCustomTitleString write FCustomTitleString;
    property HideTabSearch: Boolean read FHideTabSearch write SetHideTabSearch;
    property SearchAsYouType: Boolean read FSearchAsYouType write FSearchAsYouType;
    // Main Form - Position and size
    property HoldSize: Boolean read FHoldSize write SetHoldSize;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write SetAlwaysOnTop;
    property DialogCenterMF: Boolean read FDialogCenterMF write FDialogCenterMF;
    // Main Form - Treevew
    property TVBackground: Boolean read FTVBackground write SetTVBackground;
    property TVSmallIconSize: Boolean read FTVSmallIconSize write SetTVSmallIconSize;
    property TVBackgroundPath: String read FTVBackgroundPath write SetTVBackgroundPath;
    property TVAutoOpClCats: Boolean read FTVAutoOpClCats write SetTVAutoOpClCats;
    property TVAutoOpCatsDrag: Boolean read FTVAutoOpCatsDrag write FTVAutoOpCatsDrag;
    property TVDisableConfirmDelete: Boolean read FTVDisableConfirmDelete write FTVDisableConfirmDelete;
    property TVFont: TFont read FTVFont write SetTVFont;
    // MRU
    property MRU: Boolean read FMRU write FMRU;
    property SubMenuMRU: Boolean read FSubMenuMRU write FSubMenuMRU;
    property MRUNumber: Integer read FMRUNumber write FMRUNumber;
    // MFU
    property MFU: Boolean read FMFU write FMFU;
    property SubMenuMFU: Boolean read FSubMenuMFU write FSubMenuMFU;
    property MFUNumber: Integer read FMFUNumber write FMFUNumber;
    // Backup
    property Backup: Boolean read FBackup write SetBackup;
    property BackupNumber: Integer read FBackupNumber write SetBackupNumber;
    // Other functions
    property AutorunStartup: Boolean read FAutorunStartup write FAutorunStartup;
    property AutorunShutdown: Boolean read FAutorunShutdown write FAutorunShutdown;
    property Cache: Boolean read FCache write SetCache;
    property Scheduler: Boolean read FScheduler write SetScheduler;
    // Execution
    property ActionOnExe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property RunSingleClick: Boolean read FRunSingleClick write FRunSingleClick;
    property ConfirmRunCat: Boolean read FConfirmRunCat write FConfirmRunCat;
    property AutoCloseProcess: Boolean read FAutoCloseProcess write FAutoCloseProcess;
    // Trayicon
    property TrayIcon: Boolean read FTrayIcon write SetTrayIcon;
    property TrayUseCustomIcon: Boolean read FTrayUseCustomIcon write FTrayUseCustomIcon;
    property TrayCustomIconPath: String read FTrayCustomIconPath write FTrayCustomIconPath;
    property ActionClickLeft: TTrayiconActionClick read FActionClickLeft write FActionClickLeft;
    property ActionClickMiddle: TTrayiconActionClick read FActionClickMiddle write FActionClickMiddle;
    property ActionClickRight: TTrayiconActionClick read FActionClickRight write FActionClickRight;
    property AutoExpansionFolder: Boolean read FAutoExpansionFolder write FAutoExpansionFolder;
    property CMHideEjectMenuItem: Boolean read FCMHideEjectMenuItem write FCMHideEjectMenuItem;
    //Graphic Menu
    property GMTheme: string read FGMTheme write SetGMTheme;
    property GMFade: Boolean read FGMFade write FGMFade;
    property GMSmallIconSize: Boolean read FGMSmallIconSize write FGMSmallIconSize;
    property GMPersonalPicture: string read FGMPersonalPicture write FGMPersonalPicture;
    property GMPositionTop: Integer read FGMPositionTop write FGMPositionTop;
    property GMPositionLeft: Integer read FGMPositionLeft write FGMPositionLeft;
    property GMShowUserPicture: Boolean read FGMShowUserPicture write FGMShowUserPicture;
    property GMHideEjectButton: Boolean read FGMHideEjectButton write FGMHideEjectButton;
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
    property ScanFolderAutoExtractName: boolean read FScanFolderAutoExtractName write FScanFolderAutoExtractName;
    property ScanFolderFileTypes: TStringList read FScanFolderFileTypes write FScanFolderFileTypes;
    property ScanFolderExcludeNames: TStringList read FScanFolderExcludeNames write FScanFolderExcludeNames;

    procedure AfterUpdateConfig;

    //Update theme paths
    procedure UpdateGMTheme;

    procedure LoadConfig;
    procedure SaveConfig;
  end;

var
  Config: TConfiguration;

implementation

uses
  Forms.Main, DataModules.TrayMenu, Utility.System, Kernel.Consts, Utility.Misc,
  Forms.GraphicMenu, VirtualTree.Methods, Utility.FileFolder, mormot.core.log,
  LCLProc, BGRAIconCursor, VirtualTrees.Types,
  TypInfo, Kernel.ResourceStrings, LCLTranslator, AppConfig.Consts, BGRABitmapTypes,
  Utility.Conversions, Hotkeys.Manager.Platform, Kernel.Instance, Kernel.Manager;

procedure TConfiguration.AfterUpdateConfig;
var
  sBackgroundPath: String;

begin   
  TVirtualTreeMethods.UpdateItemColor(ASuiteInstance.MainTree);

  SetDefaultLang(FLangID, ASuiteInstance.Paths.SuitePathLocale);

  //Update background
  sBackgroundPath := ASuiteInstance.Paths.RelativeToAbsolute(FTVBackgroundPath);
  if (FTVBackground) and (FTVBackgroundPath <> '') and (FileExists(sBackgroundPath)) then
  begin
    if ((ExtractLowerFileExt(sBackgroundPath) = EXT_PNG) or
        (ExtractLowerFileExt(sBackgroundPath) = EXT_BMP)) then
      ASuiteInstance.MainTree.Background.LoadFromFile(sBackgroundPath);
  end;

  ASuiteInstance.MainTree.Update;

  UpdateGMTheme;

  UpdateTrayIcon;
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

  //Main Form - Position and size
  FHoldSize           := False;
  FAlwaysOnTop        := False;
  FDialogCenterMF     := True;

  //Main Form - Treevew
  FTVBackground       := False;
  FTVBackgroundPath   := '';
  FTVSmallIconSize    := True;
  FTVAutoOpClCats     := True;
  FTVAutoOpCatsDrag   := True;
  TVDisableConfirmDelete := False;

  //Treeview Font
  FTVFont             := TFont.Create;
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
end;

destructor TConfiguration.Destroy;
begin
  inherited Destroy;
  FTVFont.Free;
  FScanFolderFileTypes.Free;
  FScanFolderExcludeNames.Free;
end;

procedure TConfiguration.SetASuiteState(const Value: TLauncherState);
begin
  TASuiteLogger.Info('Changed ASuite State (old value %s, new value %s)',
    [GetEnumName(TypeInfo(TLauncherState), Ord(FASuiteState)),
     GetEnumName(TypeInfo(TLauncherState), Ord(Value))]);

  FASuiteState := Value;
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

procedure TConfiguration.UpdateTrayIcon;
var
  bmp: TBGRABitmap;
  sPath: string;
begin
  dmTrayMenu.tiTrayMenu.Visible := False;

  sPath := ASuiteInstance.Paths.RelativeToAbsolute(FTrayCustomIconPath);
  if not((FTrayUseCustomIcon) and (FileExists(sPath))) then
    sPath := AppendPathDelim(ASuiteInstance.Paths.SuitePathCurrentTheme +
      ICONS_DIR) + LowerCase(APP_NAME) + EXT_ICO;

  try
    bmp := LoadTrayIconFromFile(sPath);

    if Assigned(bmp) then
      dmTrayMenu.tiTrayMenu.Icon.Assign(bmp.Bitmap)
    else
      dmTrayMenu.tiTrayMenu.Icon.Assign(Application.Icon);
  finally
    bmp.Free;
  end;

  //If you can't change trayicon's property visible, it will use old icon
  dmTrayMenu.tiTrayMenu.Visible := FTrayIcon;
  if dmTrayMenu.tiTrayMenu.Visible then
    dmTrayMenu.tiTrayMenu.Show;
end;

procedure TConfiguration.SetHoldSize(value: Boolean);
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
    AJSONConfig.SetValue(CONFIG_SEARCH_NAME_WIDTH, frmMain.vstSearch.Header.Columns[0].Width);
    AJSONConfig.SetValue(CONFIG_SEARCH_CATEGORY_WIDTH, frmMain.vstSearch.Header.Columns[1].Width);
    AJSONConfig.SetValue(CONFIG_HOLDSIZE, Self.HoldSize);
    AJSONConfig.SetValue(CONFIG_ALWAYSONTOP, Self.AlwaysOnTop);
    AJSONConfig.SetValue(CONFIG_MAINFORM_DIALOGS_CENTER, Self.DialogCenterMF);

    // Main Form - Position and size
    AJSONConfig.SetValue(CONFIG_MAINFORM_LEFT, frmMain.Left);
    AJSONConfig.SetValue(CONFIG_MAINFORM_TOP, frmMain.Top);
    AJSONConfig.SetValue(CONFIG_MAINFORM_WIDTH, frmMain.ScaleFormTo96(frmMain.Width));
    AJSONConfig.SetValue(CONFIG_MAINFORM_HEIGHT, frmMain.ScaleFormTo96(frmMain.Height));

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
begin
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
  frmMain.vstSearch.Header.Columns[0].Width := AJSONConfig.GetValue(CONFIG_SEARCH_NAME_WIDTH, frmMain.vstSearch.Header.Columns[0].Width);
  frmMain.vstSearch.Header.Columns[1].Width := AJSONConfig.GetValue(CONFIG_SEARCH_CATEGORY_WIDTH, frmMain.vstSearch.Header.Columns[1].Width);
  Self.HoldSize                  := AJSONConfig.GetValue(CONFIG_HOLDSIZE, Self.HoldSize);
  Self.AlwaysOnTop               := AJSONConfig.GetValue(CONFIG_ALWAYSONTOP, Self.AlwaysOnTop);
  Self.DialogCenterMF            := AJSONConfig.GetValue(CONFIG_MAINFORM_DIALOGS_CENTER, Self.DialogCenterMF);

  // Main Form - Position and size
  nLeft   := AJSONConfig.GetValue(CONFIG_MAINFORM_LEFT, frmMain.Left);
  nTop    := AJSONConfig.GetValue(CONFIG_MAINFORM_TOP, frmMain.Top);
  nWidth  := AJSONConfig.GetValue(CONFIG_MAINFORM_WIDTH, frmMain.Width);
  nHeight := AJSONConfig.GetValue(CONFIG_MAINFORM_HEIGHT, frmMain.Height);
  frmMain.SetBounds(nLeft, nTop, nWidth, nHeight);

  // Main Form - Treevew
  Self.TVBackground              := AJSONConfig.GetValue(CONFIG_TVBACKGROUND, Self.TVBackground);
  Self.TVBackgroundPath          := AJSONConfig.GetValue(CONFIG_TVBACKGROUNDPATH, Self.TVBackgroundPath);
  Self.TVSmallIconSize           := AJSONConfig.GetValue(CONFIG_TVSMALLICONSIZE, Self.TVSmallIconSize);
  Self.TVAutoOpClCats            := AJSONConfig.GetValue(CONFIG_TVAUTOOPCLCATS, Self.TVAutoOpClCats);
  Self.TVAutoOpCatsDrag          := AJSONConfig.GetValue(CONFIG_TVAUTOOPCATSDRAG, Self.TVAutoOpCatsDrag);
  Self.TVDisableConfirmDelete    := AJSONConfig.GetValue(CONFIG_TVDISABLECONFIRMDELETE, Self.TVDisableConfirmDelete);

  // Main Form - TVFont
  Self.TVFont.Name               := AJSONConfig.GetValue(CONFIG_TVFONTNAME, UnicodeString(Self.TVFont.Name));
  Self.TVFont.Color              := HtmlToColor(AJSONConfig.GetValue(CONFIG_TVFONTCOLOR, ColorToHtml(Self.TVFont.Color)));
  Self.TVFont.Size               := AJSONConfig.GetValue(CONFIG_TVFONTSIZE, Self.TVFont.Size);
  if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_BOLD, (fsBold in Self.TVFont.Style)) then
     Self.TVFont.Style := Self.TVFont.Style + [fsBold];
  if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_ITALIC, (fsItalic in Self.TVFont.Style)) then
     Self.TVFont.Style := Self.TVFont.Style + [fsItalic];
  if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_UNDERLINE, (fsUnderline in Self.TVFont.Style)) then
     Self.TVFont.Style := Self.TVFont.Style + [fsUnderline];
  if AJSONConfig.GetValue(CONFIG_TVFONTSTYLE_STRIKEOUT, (fsStrikeOut in Self.TVFont.Style)) then
     Self.TVFont.Style := Self.TVFont.Style + [fsStrikeOut];
  ASuiteInstance.MainTree.Font.Assign(Self.TVFont);

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
end;

function TConfiguration.LoadTrayIconFromFile(const APath: string): TBGRABitmap;
var
  Icon: TBGRAIconCursor;
begin
  Result := nil;

  if not FileExists(APath) then
    Exit;

  Icon := TBGRAIconCursor.Create(ifIco);
  try
    Icon.LoadFromFile(APath);

    Result := (Icon.GetBestFitBitmap(ICON_SIZE_TRAY, ICON_SIZE_TRAY) as TBGRABitmap);
  finally
    Icon.Free;
  end;
end;

procedure TConfiguration.SetHotKey(const Value: Boolean);
begin
  if (FHotKey <> Value) then
  begin
    FHotKey := Value;
    ASuiteManager.ListManager.HotKeyItemList.RefreshRegs;
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
    if FAlwaysOnTop then
      frmMain.FormStyle := fsStayOnTop
    else begin
      ShowMessageEx(msgRestartAsuiteChanges);
      frmMain.FormStyle := fsNormal;
    end;
  end;
end;

procedure TConfiguration.SetBackup(const Value: Boolean);
begin
  FBackup := Value;
  if FBackup then
    ASuiteInstance.Paths.CheckBackupFolder;
end;

procedure TConfiguration.SetBackupNumber(const Value: Integer);
begin
  if Value < FBackupNumber then
    DeleteOldBackups(Value);
  FBackupNumber := Value;
end;

procedure TConfiguration.SetScheduler(value: Boolean);
begin
  FScheduler := value;
  ASuiteInstance.Scheduler.Timer.Enabled := FScheduler;
end;

procedure TConfiguration.SetTrayIcon(value: Boolean);
begin
  if not TdmTrayMenu.CheckSysTray then
    FTrayIcon := False
  else
    FTrayIcon := value;

  //Workaround for bug in GTK3 (unit gtk3wstrayicon - line 128)
  if (dmTrayMenu.tiTrayMenu.Icon.Handle <> 0) then
    dmTrayMenu.tiTrayMenu.Visible := FTrayIcon;

  if (not(FShowPanelAtStartUp)) and (not(FTrayicon)) then
    FShowPanelAtStartUp := True;
end;

procedure TConfiguration.SetTVBackgroundPath(AValue: String);
begin
  if FTVBackgroundPath = AValue then Exit;
  FTVBackgroundPath := AValue;

  Self.TVBackground := FTVBackground;
end;

procedure TConfiguration.SetUseCustomTitle(value: Boolean);
begin
  FUseCustomTitle := value;
  if (FUseCustomTitle) and (FCustomTitleString <> '') then
    frmMain.Caption := FCustomTitleString
  else
    frmMain.Caption := APP_TITLE;
end;

procedure TConfiguration.SetWindowHotKey(const Value: string);
begin
  if not UpdateHotkey(FWindowHotKey, Value, frmMainID) then
    ShowMessageEx(msgErrRegWindowHotkey, true);
  FWindowHotKey := Value;
end;

procedure TConfiguration.UpdateGMTheme;
begin
  TASuiteLogger.Info('Change Current Theme path to "%s"', [ASuiteInstance.Paths.SuitePathMenuThemes + FGMTheme]);
  //Set Paths
  ASuiteInstance.Paths.SuitePathCurrentTheme := AppendPathDelim(ASuiteInstance.Paths.SuitePathMenuThemes + FGMTheme);
  ASuiteManager.IconsManager.PathTheme       := ASuiteInstance.Paths.SuitePathCurrentTheme;
  //Loading icons
  frmMain.SetAllIcons;
  //Refresh GraphicMenu
  if Assigned(frmGraphicMenu) then
    frmGraphicMenu.LoadTheme;
end;

procedure TConfiguration.SetTVAutoOpClCats(value: Boolean);
begin
  FTVAutoOpClCats := value;
  if FTVAutoOpClCats then
    ASuiteInstance.MainTree.TreeOptions.AutoOptions := ASuiteInstance.MainTree.TreeOptions.AutoOptions + [toAutoExpand]
  else
    ASuiteInstance.MainTree.TreeOptions.AutoOptions := ASuiteInstance.MainTree.TreeOptions.AutoOptions - [toAutoExpand];
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
end;

procedure TConfiguration.SetTVBackground(value: Boolean);
begin
  FTVBackground := value;

  if FTVBackground then
    ASuiteInstance.MainTree.TreeOptions.PaintOptions := ASuiteInstance.MainTree.TreeOptions.PaintOptions + [toShowBackground]
  else
    ASuiteInstance.MainTree.TreeOptions.PaintOptions := ASuiteInstance.MainTree.TreeOptions.PaintOptions - [toShowBackground];
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

procedure TConfiguration.SetTVFont(value: TFont);
begin
  if not FTVFont.IsEqual(value) then
  begin
    FTVFont.Name  := value.Name;
    FTVFont.Style := value.Style;
    FTVFont.Size  := value.Size;
    FTVFont.Color := value.Color;

    ASuiteInstance.MainTree.Font.Assign(FTVFont);
  end;
end;

procedure TConfiguration.SetTVSmallIconSize(const Value: Boolean);
begin
  //If new value is different than old value, full collapse Tree and get icons
  if FTVSmallIconSize <> Value then
  begin
    FTVSmallIconSize := Value;
    //Change node height and imagelist
    TVirtualTreeMethods.ChangeTreeIconSize(ASuiteInstance.MainTree, FTVSmallIconSize);
    if ASuiteInstance.MainTree.HasChildren[ASuiteInstance.MainTree.RootNode] then
    begin
      ASuiteInstance.MainTree.FullCollapse;
      TVirtualTreeMethods.ChangeAllNodeHeight(ASuiteInstance.MainTree, ASuiteInstance.MainTree.DefaultNodeHeight);
    end;
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
end;

procedure TConfiguration.SetGMBtnDocuments(Value: string);
begin
  if value = '' then
    FGMBtnDocuments := '%USERPROFILE%\Documents'
  else
    FGMBtnDocuments := value;
end;

procedure TConfiguration.SetGMBtnExplore(Value: string);
begin
  if value = '' then
    FGMBtnExplore := '$drive\'
  else
    FGMBtnExplore := value;
end;

procedure TConfiguration.SetGMBtnMusic(Value: string);
begin
  if value = '' then
    FGMBtnMusic := '%USERPROFILE%\Music'
  else
    FGMBtnMusic := value;
end;

procedure TConfiguration.SetGMBtnPictures(Value: string);
begin
  if value = '' then
    FGMBtnPictures := '%USERPROFILE%\Pictures'
  else
    FGMBtnPictures := value;
end;

procedure TConfiguration.SetGMBtnVideos(Value: string);
begin
  if value = '' then
    FGMBtnVideos := '%USERPROFILE%\Videos'
  else
    FGMBtnVideos := value;
end;

procedure TConfiguration.SetGMTheme(value: string);
begin
  if value = '' then
    FGMTheme := 'default'
  else
    FGMTheme := value;
end;

procedure TConfiguration.SetStartWithWindows(value: boolean);
begin
  FStartWithWindows := value;
  if FStartWithWindows then
    SetASuiteAtOsStartup
  else
    DeleteASuiteAtOsStartup;
end;

procedure TConfiguration.SetLangID(value: String);
begin
  if (value <> '') then
    FLangID := value
  else
    FLangID := 'en';
end;

procedure TConfiguration.SetGraphicMenuHotKey(const Value: string);
begin
  if not UpdateHotkey(FGraphicMenuHotKey, Value, frmGMenuID) then
    ShowMessageEx(msgErrRegGMHotkey, true);
  FGraphicMenuHotKey := Value;
end;

initialization
  Config := TConfiguration.Create;

finalization
  FreeAndNil(Config);

end.
