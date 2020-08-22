{
Copyright (C) 2006-2020 Matteo Salvi

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
  CONFIG_STARTWITHWINDOWS = 'application/startwithwindows';
  CONFIG_SHOWPANELATSTARTUP = 'startup/showpanel';
  CONFIG_SHOWMENUATSTARTUP = 'startup/showmenu';
  CONFIG_MISSEDSCHEDULERTASK = 'startup/checkmissedschedulertask';

  //MAIN FORM
  CONFIG_LANGID = 'application/langid';
  CONFIG_USECUSTOMTITLE = 'mainform/customtitle';
  CONFIG_CUSTOMTITLESTRING = 'mainform/customtitle/string';
  CONFIG_HIDETABSEARCH = 'mainform/hidetabsearch';
  CONFIG_SEARCHASYOUTYPE = 'mainform/searchasyoutype';

  //MAIN FORM - POSITION AND SIZE
  CONFIG_HOLDSIZE = 'mainform/holdsize';
  CONFIG_ALWAYSONTOP = 'mainform/alwaysontop';
  //CONFIG_MAINFORMPOSSIZE = '';

  //MAIN FORM - TREEVEW
  CONFIG_TVBACKGROUND = 'mainform/treeview/background';
  CONFIG_TVBACKGROUNDPATH = 'mainform/treeview/background/path';
  CONFIG_TVAUTOOPCLCATS = 'mainform/treeview/autoopencategory';
  CONFIG_TVAUTOOPCATSDRAG = 'mainform/treeview/autoopencategory/drag';
  CONFIG_TVDISABLECONFIRMDELETE = 'mainform/treeview/';
  CONFIG_TVFONTNAME = 'mainform/treeview/font/name';
  CONFIG_TVFONTSIZE = 'mainform/treeview/font/size';
  CONFIG_TVFONTCOLOR = 'mainform/treeview/font/color';
  CONFIG_TVFONTSTYLE = 'mainform/treeview/font/style';
  CONFIG_TVSMALLICONSIZE = 'mainform/treeview/icons/smallsize';

  //MRU
  CONFIG_MRU = 'application/mru';
  CONFIG_SUBMENUMRU = 'application/mru/submenu';
  CONFIG_MRUNUMBER = 'application/mru/number';

  //MFU
  CONFIG_MFU = 'application/mfu';
  CONFIG_SUBMENUMFU = 'application/mfu/submenu';
  CONFIG_MFUNUMBER = 'application/mfu/number';

  //BACKUP
  CONFIG_BACKUP = 'application/backup';
  CONFIG_BACKUPNUMBER = 'application/backup/number';

  //OTHER FUNCTIONS
  CONFIG_AUTORUNSTARTUP = 'application/autorun/startup';
  CONFIG_AUTORUNSHUTDOWN = 'application/autorun/shutdown';
  CONFIG_CACHE = 'application/cache';
  CONFIG_SCHEDULER = 'application/scheduler';

  //EXECUTION
  CONFIG_ACTIONONEXE = 'application/execute/action';
  CONFIG_RUNSINGLECLICK = 'application/execute/runsingleclick';
  CONFIG_CONFIRMMESSAGECAT = 'application/execute/confirmcat';
  CONFIG_AUTOCLOSEPROCESS = 'application/shutdown/autocloseprocess';

  //TRAYICON
  CONFIG_TRAYICON = 'trayicon';
  CONFIG_TRAYUSECUSTOMICON = 'trayicon/classicmenu/customicon';
  CONFIG_TRAYCUSTOMICONPATH = 'trayicon/classicmenu/customicon/path';
  CONFIG_ACTIONCLICKLEFT = 'trayicon/classicmenu/clickleft';
  CONFIG_ACTIONCLICKMIDDLE = 'trayicon/classicmenu/clickmiddle';
  CONFIG_ACTIONCLICKRIGHT = 'trayicon/classicmenu/clickright';
  CONFIG_AUTOEXPANSIONFOLDER = 'trayicon/classicmenu/autoexpansionfolder';

  //GRAPHIC MENU
  CONFIG_GMTHEME = 'graphicmenu/theme';
  CONFIG_GMFADE = 'graphicmenu/fade';
  CONFIG_GMPERSONALPICTURE = 'graphicmenu/personalpicture/path';
  CONFIG_GMSMALLICONSIZE = 'graphicmenu/treeview/smallicon';
  CONFIG_GMPOSITIONLEFT = 'graphicmenu/position/left';
  CONFIG_GMPOSITIONTOP = 'graphicmenu/position/top';
  CONFIG_GMAUTOHIDEMENU = 'graphicmenu/autohidemenu';
  CONFIG_GMSHOWUSERPICTURE = 'graphicmenu/showuserpicture';

  //RIGHT BUTTONS
  CONFIG_GMBTNDOCUMENTS = 'graphicmenu/path/documents';
  CONFIG_GMBTNPICTURES = 'graphicmenu/path/pictures';
  CONFIG_GMBTNMUSIC = 'graphicmenu/path/music';
  CONFIG_GMBTNVIDEOS = 'graphicmenu/path/videos';
  CONFIG_GMBTNEXPLORE = 'graphicmenu/path/explore';

  //HOT KEYS
  CONFIG_HOTKEY = 'application/hotkey';
  CONFIG_WINDOWHOTKEY = 'application/hotkey/window';
  CONFIG_MENUHOTKEY = 'application/hotkey/graphicmenu';
  CONFIG_CLASSICMENUHOTKEY = 'application/hotkey/classicmenu';

  //SCAN FOLDER
  CONFIG_SCANFOLDERAUTOEXTRACTNAME = 'scanfolder/autoextractname';
  CONFIG_SCANFOLDERFILETYPES = 'scanfolder/filetypes';
  CONFIG_SCANFOLDEREXCLUDENAMES = 'scanfolder/excludenames';

implementation

end.

