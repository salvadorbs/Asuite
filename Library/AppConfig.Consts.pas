{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www/salvadorsoftware/com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version/

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE/  See the
GNU General Public License for more details/

You should have received a copy of the GNU General Public License
along with this program/  If not, see <http://www/gnu/org/licenses/>/
}

unit AppConfig.Consts;

{$MODE DelphiUnicode}

interface

const
  //GENERAL
  CONFIG_STARTWITHWINDOWS = 'application/start_with_windows';
  CONFIG_SHOWPANELATSTARTUP = 'startup/showp_anel';
  CONFIG_SHOWMENUATSTARTUP = 'startup/show_menu';
  CONFIG_MISSEDSCHEDULERTASK = 'startup/check_missed_scheduler_task';
  CONFIG_SECONDINSTANCEGM = 'startup/singleinstance/show_graphicmenu';
  CONFIG_CONFIRMCLOSEASUITE = 'application/other/confirm_close_asuite';

  //MAIN FORM
  CONFIG_LANGID = 'application/langid';
  CONFIG_USECUSTOMTITLE = 'mainform/customtitle/active';
  CONFIG_CUSTOMTITLESTRING = 'mainform/customtitle/string';
  CONFIG_HIDETABSEARCH = 'mainform/hide_tab_search';
  CONFIG_SEARCHASYOUTYPE = 'mainform/search_as_you_type';
  CONFIG_SEARCH_NAME_WIDTH = 'mainform/search/columns/name/width';
  CONFIG_SEARCH_CATEGORY_WIDTH = 'mainform/search/columns/category/width';

  //MAIN FORM - POSITION AND SIZE
  CONFIG_HOLDSIZE = 'mainform/hold_size';
  CONFIG_ALWAYSONTOP = 'mainform/always_on_top';
  CONFIG_MAINFORM_LEFT = 'mainform/pos/left';
  CONFIG_MAINFORM_TOP = 'mainform/pos/top';
  CONFIG_MAINFORM_WIDTH = 'mainform/pos/width';
  CONFIG_MAINFORM_HEIGHT = 'mainform/pos/height';
  CONFIG_MAINFORM_DIALOGS_CENTER = 'mainform/open_dialog_center';

  //MAIN FORM - TREEVEW
  CONFIG_TVBACKGROUND = 'mainform/treeview/background/active';
  CONFIG_TVBACKGROUNDPATH = 'mainform/treeview/background/path';
  CONFIG_TVAUTOOPCLCATS = 'mainform/treeview/auto_open_category/active';
  CONFIG_TVAUTOOPCATSDRAG = 'mainform/treeview/auto_open_category/drag';
  CONFIG_TVDISABLECONFIRMDELETE = 'mainform/treeview/disable_confirm_delete';
  CONFIG_TVFONTNAME = 'mainform/treeview/font/name';
  CONFIG_TVFONTSIZE = 'mainform/treeview/font/size';
  CONFIG_TVFONTCOLOR = 'mainform/treeview/font/color';
  CONFIG_TVFONTSTYLE_BOLD = 'mainform/treeview/font/style/bold';
  CONFIG_TVFONTSTYLE_ITALIC = 'mainform/treeview/font/style/italic';
  CONFIG_TVFONTSTYLE_UNDERLINE = 'mainform/treeview/font/style/underline';
  CONFIG_TVFONTSTYLE_STRIKEOUT = 'mainform/treeview/font/style/strikeout';
  CONFIG_TVSMALLICONSIZE = 'mainform/treeview/icons/small_size';

  //MRU
  CONFIG_MRU = 'application/mru/active';
  CONFIG_SUBMENUMRU = 'application/mru/submenu';
  CONFIG_MRUNUMBER = 'application/mru/number';

  //MFU
  CONFIG_MFU = 'application/mfu/active';
  CONFIG_SUBMENUMFU = 'application/mfu/submenu';
  CONFIG_MFUNUMBER = 'application/mfu/number';

  //BACKUP
  CONFIG_BACKUP = 'application/backup/active';
  CONFIG_BACKUPNUMBER = 'application/backup/number';

  //OTHER FUNCTIONS
  CONFIG_AUTORUNSTARTUP = 'application/autorun/startup';
  CONFIG_AUTORUNSHUTDOWN = 'application/autorun/shutdown';
  CONFIG_CACHE = 'application/cache';
  CONFIG_SCHEDULER = 'application/scheduler';

  //EXECUTION
  CONFIG_ACTIONONEXE = 'application/execute/action';
  CONFIG_RUNSINGLECLICK = 'application/execute/run_single_click';
  CONFIG_CONFIRMMESSAGECAT = 'application/execute/confirm_cat';
  CONFIG_AUTOCLOSEPROCESS = 'application/shutdown/auto_close_process';

  //TRAYICON
  CONFIG_TRAYICON = 'trayicon/active';
  CONFIG_TRAYUSECUSTOMICON = 'trayicon/classicmenu/custom_icon/active';
  CONFIG_TRAYCUSTOMICONPATH = 'trayicon/classicmenu/custom_icon/path';
  CONFIG_ACTIONCLICKLEFT = 'trayicon/classicmenu/click_left';
  CONFIG_ACTIONCLICKMIDDLE = 'trayicon/classicmenu/click_middle';
  CONFIG_ACTIONCLICKRIGHT = 'trayicon/classicmenu/click_right';
  CONFIG_AUTOEXPANSIONFOLDER = 'trayicon/classicmenu/autoexpansion_folder';

  //GRAPHIC MENU
  CONFIG_GMTHEME = 'graphicmenu/theme';
  CONFIG_GMFADE = 'graphicmenu/fade';
  CONFIG_GMPERSONALPICTURE = 'graphicmenu/personalpicture/path';
  CONFIG_GMSMALLICONSIZE = 'graphicmenu/treeview/small_icon';
  CONFIG_GMPOSITIONLEFT = 'graphicmenu/position/left';
  CONFIG_GMPOSITIONTOP = 'graphicmenu/position/top';
  CONFIG_GMAUTOHIDEMENU = 'graphicmenu/auto_hide_menu';
  CONFIG_GMSHOWUSERPICTURE = 'graphicmenu/show_user_picture';

  //RIGHT BUTTONS
  CONFIG_GMBTNDOCUMENTS = 'graphicmenu/path/documents';
  CONFIG_GMBTNPICTURES = 'graphicmenu/path/pictures';
  CONFIG_GMBTNMUSIC = 'graphicmenu/path/music';
  CONFIG_GMBTNVIDEOS = 'graphicmenu/path/videos';
  CONFIG_GMBTNEXPLORE = 'graphicmenu/path/explore';

  //HOT KEYS
  CONFIG_HOTKEY = 'application/hotkey/active';
  CONFIG_WINDOWHOTKEY = 'application/hotkey/window';
  CONFIG_MENUHOTKEY = 'application/hotkey/graphicmenu';
  CONFIG_CLASSICMENUHOTKEY = 'application/hotkey/classicmenu';

  //SCAN FOLDER
  CONFIG_SCANFOLDERAUTOEXTRACTNAME = 'scanfolder/autoextract_name';
  CONFIG_SCANFOLDERFILETYPES = 'scanfolder/file_types';
  CONFIG_SCANFOLDEREXCLUDENAMES = 'scanfolder/exclude_names';

implementation

end.

