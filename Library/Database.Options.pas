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
  mORMot, SynCommons, Kernel.Enumerations, Classes,
  SysUtils, Database.Manager, AppConfig.Main, Types;

type
  TSQLtbl_options = class(TSQLRecord)
  private
    //General
    FStartWithWindows   : Boolean;
    FShowPanelAtStartUp : Boolean;
    FShowMenuAtStartUp  : Boolean;
    //Main Form
    FLangID             : Word;
    FUseCustomTitle     : Boolean;
    FCustomTitleString  : RawUTF8;
    FHideTabSearch      : Boolean;
    //Main Form - Position and size
    FHoldSize           : Boolean;
    FAlwaysOnTop        : Boolean;
    FMainFormPosSize    : TRect;
    //Main Form - Treevew
    FTVBackground       : Boolean;
    FTVBackgroundPath   : RawUTF8;
    FTVAutoOpClCats     : Boolean; //Automatic Opening/closing categories
    FTVFont             : RawUTF8;
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
    Fconfirmmessagecat  : Boolean;
    //Trayicon
    FTrayIcon           : Boolean;
    FTrayUseCustomIcon  : Boolean;
    FTrayCustomIconPath : RawUTF8;
    FActionClickLeft    : TTrayiconActionClick;
    FActionClickRight   : TTrayiconActionClick;
    FUseClassicMenu     : Boolean;
    //Graphic Menu
    FGMTheme            : RawUTF8;
    FGMFade             : Boolean;
    FGMPersonalPicture  : RawUTF8;
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
    Fscanfolderlastpath : RawUTF8;
    Fscanfoldersubfolders   : Boolean;
    Fscanfolderfiletypes    : RawUTF8;
    Fscanfolderexcludenames : RawUTF8;
  public
    class procedure Load(ADBManager: TDBManager; AConfig: TConfiguration);
    class procedure Save(ADBManager: TDBManager; AConfig: TConfiguration);
  published
    //property FIELDNAME: TYPE read FFIELDNAME write FFIELDNAME;
    //General
    property startwithwindows: Boolean read FStartWithWindows write FStartWithWindows;
    property showpanelatstartup: Boolean read FShowPanelAtStartUp write FShowPanelAtStartUp;
    property showmenuatstartup: Boolean read FShowMenuAtStartUp write FShowMenuAtStartUp;
    // Main Form
    property langid: Word read FLangID write FLangID;
    property usecustomtitle: Boolean read FUseCustomTitle write FUseCustomTitle;
    property customtitlestring : RawUTF8 read FCustomTitleString write FCustomTitleString;
    property hidetabsearch: Boolean read FHideTabSearch write FHideTabSearch;
    // Main Form - Position and size
    property holdsize: Boolean read FHoldSize write FHoldSize;
    property alwaysontop: Boolean read FAlwaysOnTop write FAlwaysOnTop;
    property mainformpossize: TRect read FMainFormPosSize write FMainFormPosSize;
    // Main Form - Treevew
    property tvbackground: Boolean read FTVBackground write FTVBackground;
    property tvbackgroundpath: RawUTF8 read FTVBackgroundPath write FTVBackgroundPath;
    property tvautoopclcats: Boolean read FTVAutoOpClCats write FTVAutoOpClCats;
    property tvfont: RawUTF8 read FTVFont write FTVFont;
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
    property autorun: Boolean read FAutorun write FAutorun;
    property cache: Boolean read FCache write FCache;
    property scheduler: Boolean read FScheduler write FScheduler;
    // Execution
    property actiononexe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property runsingleclick: Boolean read FRunSingleClick write FRunSingleClick;
    property confirmmessagecat: Boolean read Fconfirmmessagecat write Fconfirmmessagecat;
    // Trayicon
    property trayicon: Boolean read FTrayIcon write FTrayIcon;
    property trayusecustomicon: Boolean read FTrayUseCustomIcon write FTrayUseCustomIcon;
    property traycustomiconpath: RawUTF8 read FTrayCustomIconPath write FTrayCustomIconPath;
    property actionclickleft: TTrayiconActionClick read FActionClickLeft write FActionClickLeft;
    property actionclickright: TTrayiconActionClick read FActionClickRight write FActionClickRight;
    property useclassicmenu: Boolean read Fuseclassicmenu write Fuseclassicmenu;
    //Graphic Menu
    property gmtheme: RawUTF8 read FGMTheme write FGMTheme;
    property gmfade: Boolean read FGMFade write FGMFade;
    property gmpersonalpicture: RawUTF8 read FGMPersonalPicture write FGMPersonalPicture;
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
    property scanfolderlastpath: RawUTF8 read Fscanfolderlastpath write Fscanfolderlastpath;
    property scanfoldersubfolders: Boolean read Fscanfoldersubfolders write Fscanfoldersubfolders;
    property scanfolderfiletypes: RawUTF8 read Fscanfolderfiletypes write Fscanfolderfiletypes;
    property scanfolderexcludenames: RawUTF8 read Fscanfolderexcludenames write Fscanfolderexcludenames;
  end;

implementation

uses
  DKLang, Utility.Conversions, Forms.Main, Utility.Misc, Forms;

class procedure TSQLtbl_options.Load(ADBManager: TDBManager; AConfig: TConfiguration);
var
  SQLOptionsData : TSQLtbl_options;
begin
  //If tbl_Options is empty, save default options in it
  if Not(ADBManager.Database.TableHasRows(TSQLtbl_options)) then
    Self.Save(ADBManager, AConfig);
  //Create and fillprepare SQLOptionsData
  SQLOptionsData := TSQLtbl_options.CreateAndFillPrepare(ADBManager.Database, '');
  try
    //Get values settings from table tbl_options
    if (SQLOptionsData.FillOne) then
    begin
      //Get GMTheme before everything (so ASuite know where icons folder)
      AConfig.GMTheme            := SQLOptionsData.gmtheme;
      //General
      AConfig.StartWithWindows   := SQLOptionsData.startwithwindows;
      AConfig.ShowPanelAtStartUp := SQLOptionsData.showpanelatstartup;
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
      if Not SQLOptionsData.mainformpossize.IsEmpty then
      begin
        frmMain.Width  := SQLOptionsData.mainformpossize.Width;
        frmMain.Height := SQLOptionsData.mainformpossize.Height;
        SetFormPosition(frmMain, SQLOptionsData.mainformpossize.Left, SQLOptionsData.mainformpossize.Top);
      end
      else begin
        frmMain.Top  := (Screen.WorkAreaHeight - frmMain.Height) div 2;
        frmMain.Left := (Screen.WorkAreaWidth - frmMain.Width) div 2;
        AConfig.Changed := True;
      end;
//      AConfig.MissedSchedulerTask := SQLOptionsData.missedschedulertask;
      //Main Form - Treevew
      AConfig.TVBackgroundPath   := UTF8ToString(SQLOptionsData.tvbackgroundpath);
      AConfig.TVBackground       := SQLOptionsData.tvbackground;
      AConfig.TVAutoOpClCats     := SQLOptionsData.tvautoopclcats;
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
      AConfig.Autorun        := SQLOptionsData.autorun;
      AConfig.Cache          := SQLOptionsData.cache;
      AConfig.Scheduler      := SQLOptionsData.scheduler;
      //Execution
      AConfig.ActionOnExe    := SQLOptionsData.actiononexe;
      AConfig.RunSingleClick := SQLOptionsData.runsingleclick;
      AConfig.ConfirmRunCat  := SQLOptionsData.confirmmessagecat;
      //Trayicon
      AConfig.TrayIcon           := SQLOptionsData.trayicon;
      AConfig.TrayCustomIconPath := UTF8ToString(SQLOptionsData.traycustomiconpath);
      AConfig.TrayUseCustomIcon  := SQLOptionsData.trayusecustomicon;
      AConfig.ActionClickLeft    := SQLOptionsData.actionclickleft;
      AConfig.ActionClickRight   := SQLOptionsData.actionclickright;
      //Graphic Menu
      AConfig.GMFade             := SQLOptionsData.gmfade;
      AConfig.GMPersonalPicture  := SQLOptionsData.gmpersonalpicture;
      //Hot Keys
      AConfig.HotKey             := SQLOptionsData.HotKey;
      AConfig.WindowHotKey       := SQLOptionsData.WindowHotKey;
      AConfig.MenuHotKey         := SQLOptionsData.MenuHotKey;
      //Scan Folder
      AConfig.ScanFolderLastPath   := UTF8ToString(SQLOptionsData.scanfolderlastpath);
      AConfig.ScanFolderSubFolders := SQLOptionsData.scanfoldersubfolders;
      AConfig.ScanFolderFileTypes.Text    := UTF8ToString(SQLOptionsData.scanfolderfiletypes);
      AConfig.ScanFolderExcludeNames.Text := UTF8ToString(SQLOptionsData.scanfolderexcludenames);
    end
  finally
    SQLOptionsData.Free;
  end;
end;

class procedure TSQLtbl_options.Save(ADBManager: TDBManager; AConfig: TConfiguration);
var
  SQLOptionsData   : TSQLtbl_options;
  rectMainForm     : TRect;
  IsDataExists: Boolean;
begin
  //Save ASuite options
  SQLOptionsData := TSQLtbl_options.CreateAndFillPrepare(ADBManager.Database, '');
  try
    IsDataExists := SQLOptionsData.FillOne;
    //general
    SQLOptionsData.startwithwindows   := AConfig.StartWithWindows;
    SQLOptionsData.showpanelatstartup := AConfig.ShowPanelAtStartUp;
    //main form
    SQLOptionsData.langid            := AConfig.LangID;
    SQLOptionsData.usecustomtitle    := AConfig.UseCustomTitle;
    SQLOptionsData.customtitlestring := StringToUTF8(AConfig.CustomTitleString);
    SQLOptionsData.hidetabsearch     := AConfig.HideTabSearch;
    //main form - position and size
    SQLOptionsData.holdsize          := AConfig.HoldSize;
    SQLOptionsData.alwaysontop       := AConfig.AlwaysOnTop;
    rectMainForm.top    := frmMain.Top;
    rectMainForm.left   := frmMain.Left;
    rectMainForm.width  := frmMain.Width;
    rectMainForm.height := frmMain.Height;
    SQLOptionsData.mainformpossize := rectMainForm;
//    SQLOptionsData.missedschedulertask := AConfig.MissedSchedulerTask;
    //main form - treevew
    SQLOptionsData.tvbackground     := AConfig.TVBackground;
    SQLOptionsData.tvbackgroundpath := StringToUTF8(AConfig.TVBackgroundPath);
    SQLOptionsData.tvautoopclcats   := AConfig.TVAutoOpClCats;
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
    SQLOptionsData.autorun      := AConfig.Autorun;
    SQLOptionsData.cache        := AConfig.Cache;
    SQLOptionsData.scheduler    := AConfig.Scheduler;
    //execution
    SQLOptionsData.actiononexe    := AConfig.ActionOnExe;
    SQLOptionsData.runsingleclick := AConfig.RunSingleClick;
    SQLOptionsData.confirmmessagecat := AConfig.ConfirmRunCat;
    //trayicon
    SQLOptionsData.trayicon := AConfig.TrayIcon;
    SQLOptionsData.trayusecustomicon  := AConfig.TrayUseCustomIcon;
    SQLOptionsData.traycustomiconpath := StringToUTF8(AConfig.TrayCustomIconPath);
    SQLOptionsData.actionclickleft    := AConfig.ActionClickLeft;
    SQLOptionsData.actionclickright   := AConfig.ActionClickRight;
    //Graphic Menu
    SQLOptionsData.gmtheme            := AConfig.GMTheme;
    SQLOptionsData.gmfade             := AConfig.GMFade;
    SQLOptionsData.gmpersonalpicture  := AConfig.GMPersonalPicture;
    //Hot Keys
    SQLOptionsData.HotKey             := AConfig.HotKey;
    SQLOptionsData.WindowHotKey       := AConfig.WindowHotKey;
    SQLOptionsData.MenuHotKey         := AConfig.MenuHotKey;
    //Scan Folder
    SQLOptionsData.scanfolderlastpath     := StringToUTF8(AConfig.ScanFolderLastPath);
    SQLOptionsData.scanfoldersubfolders   := AConfig.ScanFolderSubFolders;
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
