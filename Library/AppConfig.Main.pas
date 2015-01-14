{
Copyright (C) 2006-2015 Matteo Salvi

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

interface

uses
  Windows, SysUtils, Graphics, Forms, Controls, VirtualTrees, Kernel.Enumerations,
  Vcl.Imaging.pngimage, System.UITypes, Classes, DKLang, AppConfig.Paths,
  Lists.Manager, Database.Manager;

type

  { TConfiguration }
  TConfiguration = class
  private
    //General
    FStartWithWindows   : Boolean;
    FShowPanelAtStartUp : Boolean;
    FShowMenuAtStartUp  : Boolean;
    //Main Form
    FLangID             : Word;
    FUseCustomTitle     : Boolean;
    FCustomTitleString  : string;
    FHideTabSearch      : Boolean;
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : string;
    FTVAutoOpClCats     : Boolean; //Automatic Opening/closing categories
    FTVFont             : TFont;
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
    FScheduler          : Boolean;
    //Execution
    FActionOnExe        : TActionOnExecute;
    FRunSingleClick     : Boolean;
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
    //Right buttons
    FGMBtnDocuments     : string;
    FGMBtnMusic         : string;
    FGMBtnPictures      : string;
    FGMBtnVideos        : string;
    FGMBtnExplore       : string;
    //HotKeys
    FHotKey             : Boolean;
    FWindowHotKey       : TShortcut;
    FMenuHotKey         : TShortcut;
    //Misc
    FReadOnlyMode       : Boolean;
    FChanged            : Boolean;
    FASuiteState        : TLauncherState;
    FScanFolderLastPath: string;
    FScanFolderSubFolders: boolean;
    FScanFolderFileTypes: TStringList;
    FScanFolderExcludeNames: TStringList;

    FMainTree: TVirtualStringTree;
    FPaths: TConfigPaths;
    FListManager : TListManager;
    FDBManager : TDBManager;
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
    procedure SetLangID(value: Word);
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
    procedure SetMenuHotKey(const Value: TShortcut);
    procedure SetWindowHotKey(const Value: TShortcut);
    procedure SetHotKey(const Value: Boolean);

    function GetMainTree: TVirtualStringTree;
  protected
    procedure HandleParam(const Param: string);
  public
    { public declarations }
    constructor Create; overload;
    destructor Destroy; override;

    property MainTree: TVirtualStringTree read GetMainTree write FMainTree;
    property Paths: TConfigPaths read FPaths write FPaths;
    property ListManager: TListManager read FListManager write FListManager;
    property DBManager: TDBManager read FDBManager write FDBManager;

    //General
    property StartWithWindows: Boolean read FStartWithWindows write SetStartWithWindows;
    property ShowPanelAtStartUp: Boolean read FShowPanelAtStartUp write SetShowPanelAtStartUp;
    property ShowMenuAtStartUp: Boolean read FShowMenuAtStartUp write FShowMenuAtStartUp;
    // Main Form
    property LangID: Word read FLangID write SetLangID;
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
    property Autorun: Boolean read FAutorun write FAutorun;
    property Cache: Boolean read FCache write SetCache;
    property Scheduler: Boolean read FScheduler write SetScheduler;
    // Execution
    property ActionOnExe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property RunSingleClick: Boolean read FRunSingleClick write FRunSingleClick;
    // Trayicon
    property TrayIcon: Boolean read FTrayIcon write SetTrayIcon;
    property TrayUseCustomIcon: Boolean read FTrayUseCustomIcon write SetTrayUseCustomIcon;
    property TrayCustomIconPath: String read FTrayCustomIconPath write FTrayCustomIconPath;
    property ActionClickLeft: TTrayiconActionClick read FActionClickLeft write FActionClickLeft;
    property ActionClickRight: TTrayiconActionClick read FActionClickRight write FActionClickRight;
    //Graphic Menu
    property GMTheme: string read FGMTheme write SetGMTheme;
    property GMFade: Boolean read FGMFade write FGMFade;
    property GMPersonalPicture: string read FGMPersonalPicture write FGMPersonalPicture;
    //Right buttons
    property GMBtnDocuments: string read FGMBtnDocuments write SetGMBtnDocuments;
    property GMBtnPictures: string read FGMBtnPictures write SetGMBtnPictures;
    property GMBtnMusic: string read FGMBtnMusic write SetGMBtnMusic;
    property GMBtnVideos: string read FGMBtnVideos write SetGMBtnVideos;
    property GMBtnExplore: string read FGMBtnExplore write SetGMBtnExplore;
    //HotKeys
    property HotKey: Boolean read FHotKey write SetHotKey;
    property WindowHotKey: TShortcut read FWindowHotKey write SetWindowHotKey;
    property MenuHotKey: TShortcut read FMenuHotKey write SetMenuHotKey;
    // Misc
    property ReadOnlyMode: Boolean read FReadOnlyMode write FReadOnlyMode;
    property Changed: Boolean read FChanged write SetChanged;
    property ASuiteState: TLauncherState read FASuiteState write FASuiteState;
    property ScanFolderLastPath: string read FScanFolderLastPath write FScanFolderLastPath;
    property ScanFolderSubFolders: boolean read FScanFolderSubFolders write FScanFolderSubFolders;
    property ScanFolderFileTypes: TStringList read FScanFolderFileTypes write FScanFolderFileTypes;
    property ScanFolderExcludeNames: TStringList read FScanFolderExcludeNames write FScanFolderExcludeNames;

    function CheckReadOnlyMode: Boolean;
    //Update theme paths
    procedure UpdateGMTheme;
    //Database
    procedure LoadList;
    function SaveList(DoBackup: Boolean): Boolean;
  end;

var
  Config: TConfiguration;

implementation

uses
  Forms.Main, DataModules.TrayMenu, Utility.System, Kernel.Consts, Utility.Misc,
  Forms.GraphicMenu, VirtualTree.Methods, Utility.FileFolder, USingleInst,
  Utility.XML, GraphicMenu.ThemeEngine;

function TConfiguration.CheckReadOnlyMode: Boolean;
begin
  //Check if ASuite is running from a cd rom
  Result := GetDriveType(PChar(Config.Paths.SuiteDrive)) = DRIVE_CDROM;
  if (Config.ReadOnlyMode) then
  begin
    Config.Cache  := False;
    Config.Backup := False;
    Config.MRU    := False;
  end;
  Config.ReadOnlyMode := Result;
end;

constructor TConfiguration.Create;
var
  I: Integer;
begin
  SingleInst.OnProcessParam := HandleParam;
  for I := 1 to ParamCount do
    HandleParam(ParamStr(I));
  //Create some classes
  FPaths := TConfigPaths.Create;
  FListManager := TListManager.Create;
  FDBManager := TDBManager.Create();
  //Find language files and register them in LangManager
  LangManager.ScanForLangFiles(FPaths.SuitePathLocale, '*.lng', False);
  //General
  FStartWithWindows   := False;
  FShowPanelAtStartUp := True;
  FShowMenuAtStartUp  := False;
  //Main Form
  FLangID             := 1033; //1033 = English (United States)
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
  FScheduler          := True;
  //Execution
  FActionOnExe        := aeDefault;
  FRunSingleClick     := False;
  //Trayicon
  FTrayIcon           := True;
  FTrayUseCustomIcon  := False;
  FTrayCustomIconPath := '';
  FActionClickLeft    := tcShowClassicMenu;
  FActionClickRight   := tcShowGraphicMenu;
  //Graphic Menu
  FGMTheme            := 'Default';
  FGMFade             := True;
  FGMPersonalPicture  := 'PersonalPicture.jpg';
  //Right buttons
  FGMBtnDocuments     := '%USERPROFILE%\Documents';
  FGMBtnPictures      := '%USERPROFILE%\Pictures';
  FGMBtnMusic         := '%USERPROFILE%\Music';
  FGMBtnVideos        := '%USERPROFILE%\Videos';
  FGMBtnExplore       := '$drive';
  //Misc
  FReadOnlyMode       := False;
  FChanged            := False;
  FASuiteState        := lsStartUp;
  FHotKey             := True;
  //Hotkey
  FWindowHotKey       := 0;
  FMenuHotKey         := 0;
  //ScanFolder
  FScanFolderLastPath   := FPaths.SuitePathWorking;
  FScanFolderSubFolders := True;
  FScanFolderFileTypes  := TStringList.Create;
  FScanFolderFileTypes.Add(EXT_LNK);
  FScanFolderFileTypes.Add(EXT_EXE);
  FScanFolderExcludeNames := TStringList.Create;
  FScanFolderExcludeNames.Add('uninstall');
end;

destructor TConfiguration.Destroy;
begin
  inherited Destroy;
  FTVFont.Free;
  FScanFolderFileTypes.Free;
  FScanFolderExcludeNames.Free;
  FPaths.Free;
  FListManager.Destroy;
  FDBManager.Destroy;
end;

function TConfiguration.GetMainTree: TVirtualStringTree;
begin
  Assert(Assigned(FMainTree));
  Result := FMainTree;
end;

procedure TConfiguration.HandleParam(const Param: string);
var
  sName, sValue: string;

  procedure ParseParam(s: string);
  var
    iSplit: Integer;
  begin
    if (s[1] in ['-', '/']) then
    begin
      Delete(s, 1, 1);
      iSplit := Pos('=', s);
      if iSplit <> 0 then
      begin
        sName := Copy(s, 1, iSplit - 1);
        sValue := Copy(s, iSplit + 1, 666);
      end;
    end;
  end;

begin
  ParseParam(Param);
  if sName <> '' then
  begin
    if CompareText(sName, 'list') = 0 then
      FPaths.SuitePathList := FPaths.RelativeToAbsolute(RemoveAllQuotes(sValue));
    if CompareText(sName, 'additem') = 0 then
    begin
      //Add new node
      if Assigned(FDBManager) then
        TVirtualTreeMethods.Create.AddNodeByPathFile(FMainTree, nil, RemoveAllQuotes(sValue), amInsertAfter);
    end;
  end;
end;

procedure TConfiguration.LoadList;
var
  sFilePath  : string;
begin
  Assert(Assigned(FMainTree), 'FMainTree is not assigned!');
  try
    //List & Options
    if ExtractFileExt(FPaths.SuitePathList) = EXT_XML then
    begin
      sFilePath := FPaths.SuitePathList;
      FPaths.SuitePathList := ChangeFileExt(FPaths.SuiteFileName, EXT_SQL);
    end;
    if Assigned(FDBManager) then
      FDBManager.Setup(FPaths.SuitePathList);
  finally
    //If exists old list format (xml), use it
    if sFilePath <> '' then
      LoadDatabaseFromXML(sFilePath)
    else //Use new list format (sqlite db)
      FDBManager.LoadData(FMainTree);
  end;
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

procedure TConfiguration.SetHotKey(const Value: Boolean);
begin
  if (FHotKey <> Value) then
  begin
    FHotKey := Value;
    ListManager.HotKeyItemList.RefreshRegs;
  end;
end;

function TConfiguration.SaveList(DoBackup: Boolean): Boolean;
begin
  Result := False;
  if not FReadOnlyMode then
    Result := FDBManager.SaveData(Config.MainTree, DoBackup);
end;

procedure TConfiguration.SetAlwaysOnTop(value: Boolean);
begin
  FAlwaysOnTop := value;
  if FAlwaysOnTop then
    frmMain.FormStyle := fsStayOnTop
  else
    frmMain.FormStyle := fsNormal;
end;

procedure TConfiguration.SetBackup(const Value: Boolean);
begin
  FBackup := Value;
  if FBackup then
    FPaths.CheckBackupFolder;
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
  frmMain.tmScheduler.Enabled := FScheduler;
end;

procedure TConfiguration.SetTrayIcon(value: Boolean);
begin
  FTrayIcon := value;
  dmTrayMenu.tiTrayMenu.Visible := FTrayIcon;
  if (not(FShowPanelAtStartUp)) and (not(FTrayicon)) then
    FShowPanelAtStartUp := True;
end;

procedure TConfiguration.SetTrayUseCustomIcon(value: Boolean);
var
  sPath: string;
begin
  FTrayUseCustomIcon := value;
  dmTrayMenu.tiTrayMenu.Visible := False;
  sPath := FPaths.RelativeToAbsolute(FTrayCustomIconPath);
  if (FTrayUseCustomIcon) and (FileExists(sPath)) then
    dmTrayMenu.tiTrayMenu.Icon.LoadFromFile(sPath)
  else begin
    //TODO: Fix it (dmImages)
//    sPath := FPaths.RelativeToAbsolute(FPaths.FSuitePathIconsPopupMenu + FILEICON_ASuite);
    if FileExists(sPath) then
      dmTrayMenu.tiTrayMenu.Icon.LoadFromFile(sPath);
  end;
  //If you can't change trayicon's property visible, it will use old icon
  dmTrayMenu.tiTrayMenu.Visible := FTrayIcon;
end;

procedure TConfiguration.SetUseCustomTitle(value: Boolean);
begin
  FUseCustomTitle := value;
  if (FUseCustomTitle) and (FCustomTitleString <> '') then
    frmMain.Caption := FCustomTitleString
  else
    frmMain.Caption := APP_TITLE;
end;

procedure TConfiguration.SetWindowHotKey(const Value: TShortcut);
begin
  if (FHotKey) then
  begin
    //Unregister hotkey (if actived)
    UnregisterHotKeyEx(frmMain.Handle);
    //Register hotkey
    if (Value <> 0) then
    begin
      if Not(RegisterHotKeyEx(frmMain.Handle, Value)) then
        ShowMessageEx(DKLangConstW('msgErrRegWindowHotkey'));
    end;
  end;
  FWindowHotKey := Value;
end;

procedure TConfiguration.UpdateGMTheme;
begin
  //TODO: Fix it (dmImages)
  //Set Paths
  FPaths.SuitePathCurrentTheme   := IncludeTrailingBackslash(FPaths.SuitePathMenuThemes + FGMTheme);
  FPaths.SuitePathIconsPopupMenu := FPaths.SuitePathCurrentTheme + ICONS_POPUPMENU_DIR;
  FPaths.SuitePathIconsTree      := FPaths.SuitePathCurrentTheme + ICONS_TREE_DIR;
  FPaths.SuitePathIconsOptions   := FPaths.SuitePathCurrentTheme + ICONS_OPTIONS_DIR;
  //Loading icons
//  FASuiteIcons.LoadIcons;
//  frmMain.LoadGlyphs;
  //Refresh GraphicMenu
  if Assigned(frmGraphicMenu) then
    TThemeEngine.Create.LoadTheme;
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
end;

procedure TConfiguration.SetTVBackground(value: Boolean);
var
  BackgroundBMP : TBitmap;
  BackgroundPNG : TPngImage;
begin
  FTVBackground := value;
  with frmMain do
  begin
    vstList.TreeOptions.PaintOptions := vstList.TreeOptions.PaintOptions - [toShowBackground];
    if (FTVBackground) and (FTVBackgroundPath <> '') and
       (FileExists(FPaths.RelativeToAbsolute(FTVBackgroundPath))) then
    begin
      if LowerCase(ExtractFileExt(FPaths.RelativeToAbsolute(FTVBackgroundPath))) <> '.bmp' then
      begin
        BackgroundBMP := TBitmap.Create;
        BackgroundPNG := TPngImage.Create;
        try
          BackgroundPNG.LoadFromFile(FPaths.RelativeToAbsolute(FTVBackgroundPath));
          BackgroundBMP.Assign(BackgroundPNG);
          vstList.Background.Bitmap := BackgroundBMP;
        finally
          BackgroundBMP.Free;
          BackgroundPNG.Free;
        end;
      end
      else
        vstList.Background.LoadFromFile(FPaths.RelativeToAbsolute(FTVBackgroundPath));
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
    FTVFont.Name  := value.Name;
    FTVFont.Style := value.Style;
    FTVFont.Size  := value.Size;
    FTVFont.Color := value.Color;
    FMainTree.Font.Assign(FTVFont);
  end;
end;

procedure TConfiguration.SetCache(value: Boolean);
begin
  FCache := value;
  //If disabled, delete all file icon-cache and folders cache
  if Not(FCache) then
    FPaths.RemoveCacheFolders
  else //Else create folders cache
    FPaths.CheckCacheFolders;
end;

procedure TConfiguration.SetChanged(const Value: Boolean);
begin
  FChanged := Value;
  TVirtualTreeMethods.Create.RefreshList(frmMain.vstList);
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
  UpdateGMTheme;
end;

procedure TConfiguration.SetStartWithWindows(value: Boolean);
begin
  FStartWithWindows := value;
  if FStartWithWindows then
    SetASuiteAtWindowsStartup
  else
    DeleteASuiteAtWindowsStartup;
end;

procedure TConfiguration.SetLangID(value: Word);
begin
  if (value <> 0) then
    FLangID := value
  else
    FLangID := 1033;
end;

procedure TConfiguration.SetMenuHotKey(const Value: TShortcut);
begin

end;

end.
