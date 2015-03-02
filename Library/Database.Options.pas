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

unit Database.Options;

interface

uses
  mORMot, SynCommons, Kernel.Enumerations, Classes, Kernel.Types, SynLog,
  SysUtils, Database.Manager, AppConfig.Main, Types;

type
  TSQLtbl_options = class(TSQLRecord)
  private
    //General
    FStartWithWindows   : Boolean;
    FShowPanelAtStartUp : Boolean;
    FShowMenuAtStartUp  : Boolean;
    FMissedSchedulerTask: Boolean;
    //Main Form
    FLangID             : Word;
    FUseCustomTitle     : Boolean;
    FCustomTitleString  : RawUTF8;
    FHideTabSearch      : Boolean;
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    FMainFormPosSize    : TArrayRect;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : RawUTF8;
    FTVAutoOpClCats     : Boolean; //Automatic Opening/closing categories
    FTVAutoOpCatsDrag   : Boolean;
    FTVFont             : RawUTF8;
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
    Fconfirmmessagecat  : Boolean;
    FAutoCloseProcess   : Boolean;
    //Trayicon
    FTrayIcon           : Boolean;
    FTrayUseCustomIcon  : Boolean;
    FTrayCustomIconPath : RawUTF8;
    FActionClickLeft    : TTrayiconActionClick;
    Factionclickmiddle  : TTrayiconActionClick;
    FActionClickRight   : TTrayiconActionClick;
    FAutoExpansionFolder: Boolean;
    //Graphic Menu
    FGMTheme            : RawUTF8;
    FGMFade             : Boolean;
    FGMPersonalPicture  : RawUTF8;
    FGMPosition         : TArrayPoint;
    FGMAutoHideMenu     : Boolean;
    FGMShowUserPicture  : Boolean;
 		FGMSmallIconSize    : boolean;
    //Right buttons
//    FGMBtnDocuments     : RawUTF8;
//    FGMBtnMusic         : RawUTF8;
//    FGMBtnPictures      : RawUTF8;
//    FGMBtnVideos        : RawUTF8;
//    FGMBtnExplore       : RawUTF8;
    //HotKeys
    FHotKey             : Boolean;
    FWindowHotKey       : Word;
    FMenuHotKey         : Word;
    //ScanFolder
    Fscanfolderflatstructure   : boolean;
    Fscanfolderautoextractname : boolean;
    Fscanfolderfiletypes    : RawUTF8;
    Fscanfolderexcludenames : RawUTF8;
    FFTVAutoOpCatsDrag: Boolean;
  public
    class procedure Load(ADBManager: TDBManager; AConfig: TConfiguration);
    class procedure Save(ADBManager: TDBManager; AConfig: TConfiguration);
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    //General
    property startwithwindows: Boolean read FStartWithWindows write FStartWithWindows;
    property showpanelatstartup: Boolean read FShowPanelAtStartUp write FShowPanelAtStartUp;
    property showmenuatstartup: Boolean read FShowMenuAtStartUp write FShowMenuAtStartUp;
    property missedschedulertask: Boolean read Fmissedschedulertask write Fmissedschedulertask;
    // Main Form
    property langid: Word read FLangID write FLangID;
    property usecustomtitle: Boolean read FUseCustomTitle write FUseCustomTitle;
    property customtitlestring : RawUTF8 read FCustomTitleString write FCustomTitleString;
    property hidetabsearch: Boolean read FHideTabSearch write FHideTabSearch;
    // Main Form - Position and size
    property holdsize: Boolean read FHoldSize write FHoldSize;
    property alwaysontop: Boolean read FAlwaysOnTop write FAlwaysOnTop;
    property mainformpossize: TArrayRect read FMainFormPosSize write FMainFormPosSize;
    // Main Form - Treevew
    property tvbackground: Boolean read FTVBackground write FTVBackground;
    property tvbackgroundpath: RawUTF8 read FTVBackgroundPath write FTVBackgroundPath;
    property tvautoopclcats: Boolean read FTVAutoOpClCats write FTVAutoOpClCats;
    property tvautoopcatsdrag: Boolean read FFTVAutoOpCatsDrag write FFTVAutoOpCatsDrag;
    property tvfont: RawUTF8 read FTVFont write FTVFont;
    property tvsmalliconsize: Boolean read Ftvsmalliconsize write Ftvsmalliconsize;
    // MRU
    property mru: Boolean read FMRU write FMRU;
    property submenumru: Boolean read FSubMenuMRU write FSubMenuMRU;
    property mrunumber: Integer read FMRUNumber write FMRUNumber;
    // MFU
    property mfu: Boolean read FMFU write FMFU;
    property submenumfu: Boolean read FSubMenuMFU write FSubMenuMFU;
    property mfunumber: Integer read FMFUNumber write FMFUNumber;
    // Backup
    property backup: Boolean read FBackup write FBackup;
    property backupnumber: Integer read FBackupNumber write FBackupNumber;
    // Other functions
    property autorunstartup: Boolean read FAutorunStartup write FAutorunStartup;
    property autorunshutdown: Boolean read FAutorunShutdown write FAutorunShutdown;
    property cache: Boolean read FCache write FCache;
    property scheduler: Boolean read FScheduler write FScheduler;
    // Execution
    property actiononexe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property runsingleclick: Boolean read FRunSingleClick write FRunSingleClick;
    property confirmmessagecat: Boolean read Fconfirmmessagecat write Fconfirmmessagecat;
    property autocloseprocess: Boolean read FAutoCloseProcess write FAutoCloseProcess;
    // Trayicon
    property trayicon: Boolean read FTrayIcon write FTrayIcon;
    property trayusecustomicon: Boolean read FTrayUseCustomIcon write FTrayUseCustomIcon;
    property traycustomiconpath: RawUTF8 read FTrayCustomIconPath write FTrayCustomIconPath;
    property actionclickleft: TTrayiconActionClick read FActionClickLeft write FActionClickLeft;
    property actionclickmiddle: TTrayiconActionClick read Factionclickmiddle write Factionclickmiddle;
    property actionclickright: TTrayiconActionClick read FActionClickRight write FActionClickRight;
    property autoexpansionfolder: Boolean read Fautoexpansionfolder write Fautoexpansionfolder;
    //Graphic Menu
    property gmtheme: RawUTF8 read FGMTheme write FGMTheme;
    property gmfade: Boolean read FGMFade write FGMFade;
    property gmpersonalpicture: RawUTF8 read FGMPersonalPicture write FGMPersonalPicture;
    property gmsmalliconsize: boolean read Fgmsmalliconsize write Fgmsmalliconsize;
    property gmposition: TArrayPoint read Fgmposition write Fgmposition;
    property gmautohidemenu: Boolean read Fgmautohidemenu write Fgmautohidemenu;
    property gmshowuserpicture: Boolean read Fgmshowuserpicture write Fgmshowuserpicture;
    //Right buttons
//    property gmbtndocuments: RawUTF8 read FGMBtnDocuments write FGMBtnDocuments;
//    property gmbtnpictures: RawUTF8 read FGMBtnPictures write FGMBtnPictures;
//    property gmbtnmusic: RawUTF8 read FGMBtnMusic write FGMBtnMusic;
//    property gmbtnvideos: RawUTF8 read FGMBtnVideos write FGMBtnVideos;
//    property gmbtnexplore: RawUTF8 read FGMBtnExplore write FGMBtnExplore;
    //Hot Keys
    property hotkey: Boolean read FHotKey write FHotKey;
    property windowhotkey: Word read FWindowHotKey write FWindowHotKey;
    property menuhotkey: Word read FMenuHotKey write FMenuHotKey;
    //Scan Folder
    property scanfolderflatstructure: boolean read Fscanfolderflatstructure write Fscanfolderflatstructure;
    property scanfolderautoextractname: boolean read Fscanfolderautoextractname write Fscanfolderautoextractname;
    property scanfolderfiletypes: RawUTF8 read Fscanfolderfiletypes write Fscanfolderfiletypes;
    property scanfolderexcludenames: RawUTF8 read Fscanfolderexcludenames write Fscanfolderexcludenames;
  end;

implementation

uses
  DKLang, Utility.Conversions, Forms.Main, Utility.Misc, Forms, Kernel.Logger;

class procedure TSQLtbl_options.Load(ADBManager: TDBManager; AConfig: TConfiguration);
var
  SQLOptionsData : TSQLtbl_options;
begin
  TASuiteLogger.Enter(Load, Self);

  //If tbl_Options is empty, save default options in it
  if Not(ADBManager.Database.TableHasRows(TSQLtbl_options)) then
  begin
    TASuiteLogger.Info('ASuite Options not found', []);
    Self.Save(ADBManager, AConfig);
  end;
  //Create and fillprepare SQLOptionsData
  SQLOptionsData := TSQLtbl_options.CreateAndFillPrepare(ADBManager.Database, '');
  try
    //Get values settings from table tbl_options
    if (SQLOptionsData.FillOne) then
    begin
      TASuiteLogger.Info('Load ASuite Options', []);

      //Get GMTheme before everything (so ASuite know where icons folder)
      AConfig.GMTheme            := SQLOptionsData.gmtheme;
      //General
      AConfig.StartWithWindows   := SQLOptionsData.startwithwindows;
      AConfig.ShowPanelAtStartUp := SQLOptionsData.showpanelatstartup;
      AConfig.ShowGraphicMenuAtStartUp := SQLOptionsData.showmenuatstartup;
      //Main Form
      AConfig.LangID             := SQLOptionsData.langid;
      LangManager.LanguageID     := AConfig.LangID;
      AConfig.CustomTitleString  := UTF8ToString(SQLOptionsData.customtitlestring);
      AConfig.UseCustomTitle     := SQLOptionsData.usecustomtitle;
      AConfig.HideTabSearch      := SQLOptionsData.hidetabsearch;
      //Main Form - Position and size
      AConfig.HoldSize    := SQLOptionsData.holdsize;
      AConfig.AlwaysOnTop := SQLOptionsData.alwaysontop;
      //frmMain's size
      if Length(SQLOptionsData.mainformpossize) = 1 then
      begin
        frmMain.Width  := SQLOptionsData.mainformpossize[0].Width;
        frmMain.Height := SQLOptionsData.mainformpossize[0].Height;
        SetFormPosition(frmMain, SQLOptionsData.mainformpossize[0].Left,
                        SQLOptionsData.mainformpossize[0].Top);
      end
      else
        frmMain.Position := poScreenCenter;
      AConfig.MissedSchedulerTask := SQLOptionsData.missedschedulertask;
      //Main Form - Treevew
      AConfig.TVBackgroundPath   := UTF8ToString(SQLOptionsData.tvbackgroundpath);
      AConfig.TVBackground       := SQLOptionsData.tvbackground;
      AConfig.TVSmallIconSize    := SQLOptionsData.tvsmalliconsize;
      AConfig.TVAutoOpClCats     := SQLOptionsData.tvautoopclcats;
      AConfig.TVAutoOpCatsDrag   := SQLOptionsData.tvautoopcatsdrag;
      //Treeview Font
      StrToFont(UTF8ToString(SQLOptionsData.tvfont), AConfig.TVFont);
      AConfig.MainTree.Font.Assign(AConfig.TVFont);
      //MRU
      AConfig.MRU            := SQLOptionsData.mru;
      AConfig.SubMenuMRU     := SQLOptionsData.submenumru;
      AConfig.MRUNumber      := SQLOptionsData.mrunumber;
      //MFU
      AConfig.MFU            := SQLOptionsData.mfu;
      AConfig.SubMenuMFU     := SQLOptionsData.submenumfu;
      AConfig.MFUNumber      := SQLOptionsData.mfunumber;
      //Backup
      AConfig.Backup         := SQLOptionsData.backup;
      AConfig.BackupNumber   := SQLOptionsData.backupnumber;
      //Other functions
      AConfig.AutorunStartup  := SQLOptionsData.autorunstartup;
      AConfig.AutorunShutdown := SQLOptionsData.autorunshutdown;
      AConfig.Cache          := SQLOptionsData.cache;
      AConfig.Scheduler      := SQLOptionsData.scheduler;
      //Execution
      AConfig.ActionOnExe    := SQLOptionsData.actiononexe;
      AConfig.RunSingleClick := SQLOptionsData.runsingleclick;
      AConfig.ConfirmRunCat  := SQLOptionsData.confirmmessagecat;
      AConfig.AutoCloseProcess  := SQLOptionsData.autocloseprocess;
      //Trayicon
      AConfig.TrayIcon           := SQLOptionsData.trayicon;
      AConfig.TrayCustomIconPath := UTF8ToString(SQLOptionsData.traycustomiconpath);
      AConfig.TrayUseCustomIcon  := SQLOptionsData.trayusecustomicon;
      AConfig.ActionClickLeft    := SQLOptionsData.actionclickleft;
      AConfig.ActionClickMiddle  := SQLOptionsData.actionclickmiddle;
      AConfig.ActionClickRight   := SQLOptionsData.actionclickright;
      //Graphic Menu
      AConfig.GMFade             := SQLOptionsData.gmfade;
      AConfig.GMSmallIconSize    := SQLOptionsData.gmsmalliconsize;
      AConfig.GMPersonalPicture  := SQLOptionsData.gmpersonalpicture;
      if Length(SQLOptionsData.gmposition) = 1 then
 			begin
        AConfig.GMPositionTop := SQLOptionsData.gmposition[0].Y;
        AConfig.GMPositionLeft := SQLOptionsData.gmposition[0].X;
 			end;
 			AConfig.GMShowUserPicture := SQLOptionsData.gmshowuserpicture;
 			AConfig.GMAutomaticHideMenu := SQLOptionsData.gmautohidemenu;
 			AConfig.AutoExpansionFolder := SQLOptionsData.autoexpansionfolder;
      //Hot Keys
      AConfig.HotKey             := SQLOptionsData.HotKey;
      AConfig.WindowHotKey       := SQLOptionsData.WindowHotKey;
      AConfig.MenuHotKey         := SQLOptionsData.MenuHotKey;
      //Scan Folder
      AConfig.ScanFolderFlatStructure     := SQLOptionsData.scanfolderflatstructure;
      AConfig.ScanFolderAutoExtractName   := SQLOptionsData.scanfolderautoextractname;
      AConfig.ScanFolderFileTypes.Text    := UTF8ToString(SQLOptionsData.scanfolderfiletypes);
      AConfig.ScanFolderExcludeNames.Text := UTF8ToString(SQLOptionsData.scanfolderexcludenames);
    end
  finally
    SQLOptionsData.Free;
  end;
end;

class procedure TSQLtbl_options.Save(ADBManager: TDBManager; AConfig: TConfiguration);
var
  SQLOptionsData : TSQLtbl_options;
  rectMainForm   : TRect;
  IsDataExists   : Boolean;
  pointGraphicMenu : TPoint;
begin
  TASuiteLogger.Info('Saving ASuite Options', []);
  //Save ASuite options
  SQLOptionsData := TSQLtbl_options.CreateAndFillPrepare(ADBManager.Database, '');
  try
    IsDataExists := SQLOptionsData.FillOne;
    //general
    SQLOptionsData.startwithwindows   := AConfig.StartWithWindows;
    SQLOptionsData.showpanelatstartup := AConfig.ShowPanelAtStartUp;
    SQLOptionsData.showmenuatstartup  := AConfig.ShowGraphicMenuAtStartUp;
    //main form
    SQLOptionsData.langid            := AConfig.LangID;
    SQLOptionsData.usecustomtitle    := AConfig.UseCustomTitle;
    SQLOptionsData.customtitlestring := StringToUTF8(AConfig.CustomTitleString);
    SQLOptionsData.hidetabsearch     := AConfig.HideTabSearch;
    //main form - position and size
    SQLOptionsData.holdsize          := AConfig.HoldSize;
    SQLOptionsData.alwaysontop       := AConfig.AlwaysOnTop;
    SQLOptionsData.DynArray('mainformpossize').Clear;
    rectMainForm.top    := frmMain.Top;
    rectMainForm.left   := frmMain.Left;
    rectMainForm.width  := frmMain.Width;
    rectMainForm.height := frmMain.Height;
    SQLOptionsData.DynArray('mainformpossize').Add(rectMainForm);
    SQLOptionsData.missedschedulertask := AConfig.MissedSchedulerTask;
    //main form - treevew
    SQLOptionsData.tvbackground     := AConfig.TVBackground;
    SQLOptionsData.tvbackgroundpath := StringToUTF8(AConfig.TVBackgroundPath);
    SQLOptionsData.tvsmalliconsize  := AConfig.TVSmallIconSize;
    SQLOptionsData.tvautoopclcats   := AConfig.TVAutoOpClCats;
    SQLOptionsData.tvautoopcatsdrag := AConfig.TVAutoOpCatsDrag;
    SQLOptionsData.tvfont           := StringToUTF8(FontToStr(AConfig.TVFont));
    //mru
    SQLOptionsData.mru          := AConfig.MRU;
    SQLOptionsData.submenumru   := AConfig.SubMenuMRU;
    SQLOptionsData.mrunumber    := AConfig.MRUNumber;
    //mfu
    SQLOptionsData.mfu          := AConfig.MFU;
    SQLOptionsData.submenumfu   := AConfig.SubMenuMFU;
    SQLOptionsData.mfunumber    := AConfig.MFUNumber;
    //backup
    SQLOptionsData.backup       := AConfig.Backup;
    SQLOptionsData.backupnumber := AConfig.BackupNumber;
    //other functions
    SQLOptionsData.autorunstartup  := AConfig.AutorunStartup;
    SQLOptionsData.autorunshutdown := AConfig.AutorunShutdown;
    SQLOptionsData.cache        := AConfig.Cache;
    SQLOptionsData.scheduler    := AConfig.Scheduler;
    //execution
    SQLOptionsData.actiononexe    := AConfig.ActionOnExe;
    SQLOptionsData.runsingleclick := AConfig.RunSingleClick;
    SQLOptionsData.confirmmessagecat := AConfig.ConfirmRunCat;
    SQLOptionsData.autocloseprocess  := AConfig.AutoCloseProcess;
    //trayicon
    SQLOptionsData.trayicon := AConfig.TrayIcon;
    SQLOptionsData.trayusecustomicon  := AConfig.TrayUseCustomIcon;
    SQLOptionsData.traycustomiconpath := StringToUTF8(AConfig.TrayCustomIconPath);
    SQLOptionsData.actionclickleft    := AConfig.ActionClickLeft;
    SQLOptionsData.actionclickmiddle  := AConfig.ActionClickMiddle;
    SQLOptionsData.actionclickright   := AConfig.ActionClickRight;
    //Graphic Menu
    SQLOptionsData.gmtheme            := AConfig.GMTheme;
    SQLOptionsData.gmfade             := AConfig.GMFade;
    SQLOptionsData.gmsmalliconsize    := AConfig.GMSmallIconSize;
    SQLOptionsData.gmpersonalpicture  := AConfig.GMPersonalPicture;
    SQLOptionsData.DynArray('gmposition').Clear;
    pointGraphicMenu.Y := AConfig.GMPositionTop;
    pointGraphicMenu.X := AConfig.GMPositionLeft;
    SQLOptionsData.DynArray('gmposition').Add(pointGraphicMenu);
    SQLOptionsData.gmshowuserpicture   := AConfig.GMShowUserPicture;
    SQLOptionsData.gmautohidemenu      := AConfig.GMAutomaticHideMenu;
    SQLOptionsData.autoexpansionfolder := AConfig.AutoExpansionFolder;
    //Hot Keys
    SQLOptionsData.HotKey       := AConfig.HotKey;
    SQLOptionsData.WindowHotKey := AConfig.WindowHotKey;
    SQLOptionsData.MenuHotKey   := AConfig.MenuHotKey;
    //Scan Folder
    SQLOptionsData.scanfolderflatstructure   := AConfig.ScanFolderFlatStructure;
    SQLOptionsData.scanfolderautoextractname := AConfig.ScanFolderAutoExtractName;
    SQLOptionsData.scanfolderfiletypes    := StringToUTF8(AConfig.ScanFolderFileTypes.Text);
    SQLOptionsData.scanfolderexcludenames := StringToUTF8(AConfig.ScanFolderExcludeNames.Text);

    if IsDataExists then
      ADBManager.Database.Update(SQLOptionsData)
    else
      ADBManager.Database.Add(SQLOptionsData, True);
  finally
    Config.Changed := False;
    SQLOptionsData.Free;
  end;
end;

end.
