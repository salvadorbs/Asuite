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

unit Kernel.Consts;

{$MODE DelphiUnicode}    

{$I ASuite.inc}

interface

const

  // Application's informations
  APP_NAME  = 'ASuite';
  APP_TITLE = APP_NAME;

  VERSION_PRERELEASE = 'Alpha 2'; //For Alpha and Beta version

  BACKUP_DIR     = 'backup';
  CACHE_DIR      = 'cache';
  LOCALE_DIR     = 'locale';
  MENUTHEMES_DIR = 'themes';
  ICONS_DIR      = 'icons';
  BUTTONS_DIR    = 'buttons';

  //FileSystem
  DriveLetters      = ['a'..'z', 'A'..'Z'];

  //Extensions
  EXT_XML       = '.xml';
  EXT_SQL       = '.sqlite';
  EXT_SQLBCK    = '.sqbck';
  EXT_XMLBCK    = '.bck';
  EXT_ICO       = '.ico';
  EXT_LNK       = '.lnk';
  EXT_URL       = '.url';
  EXT_EXE       = '.exe';
  EXT_PO        = '.po';
  EXT_BAT       = '.bat';
  EXT_CMD       = '.cmd';
  EXT_BMP       = '.bmp';
  EXT_PNG       = '.png';
  EXT_LOG       = '.log';

  EXT_PATH_MASK = '*';    
  EXT_PATH_DOT = '.';

  //File
  THEME_INI     = 'theme.ini';
  BACKUP_FILE   = APP_NAME + '_%s' + EXT_SQLBCK;

  //Form
  frmMainID     = 294257584;
  frmGMenuID    = 711285620;
  frmCMenuID    = 459741425;

  //Icons Size
  ICON_SIZE_SMALL = 16;
  ICON_SIZE_LARGE = 32;

  {$IFDEF QT}
  ICON_SIZE_TRAY = 22;
  {$ELSE}
  ICON_SIZE_TRAY = 16;
  {$ENDIF}

  //Node Heidht
  NODE_HEIGHT_SMALL = 18;
  NODE_HEIGHT_LARGE = 36;
  CAPTION_LINE_ITEM_HEIGHT = 15;

  // PageControl Indexes
  PG_LIST       = 0;
  PG_SEARCH     = 1;

  // PageControl Indexes
  PG_MENULIST   = 0;
  PG_MENUMRU    = 1;
  PG_MENUMFU    = 2;
  PG_MENUSEARCH = 3;

  //ASuite placeholders for path
  CONST_PATH_ASUITE_old  = '$asuite'; //Deprecated
  CONST_PATH_DRIVE_old   = '$drive'; //Deprecated
  CONST_PATH_ASUITE      = '%asuite%';
  CONST_PATH_DRIVE       = '%drive%';
  CONST_PATH_FOLDERICON  = '%foldericon%';
  CONST_PATH_URLICON     = '%urlicon%';

  //ASuite filename icons
  FILEICON_Folder = 'folder';
  FILEICON_Url    = 'page_url';

  //ASuite files
  SETTINGS_FILENAME = 'settings.json';

  //Modifier buttons for TfrmShortcutGrabber
  FILENAME_CTRL   = 'ctrl.png';
  FILENAME_ALT    = 'alt.png';
  FILENAME_SHIFT  = 'shift.png';
  FILENAME_WINKEY = 'winkey.png';

  //Desktop file entries for Linux
  DESKTOP_GROUP = 'Desktop Entry';
  DESKTOP_KEY_NAME = 'Name';
  DESKTOP_KEY_TYPE = 'Type';
  DESKTOP_KEY_ICON = 'Icon';
  DESKTOP_KEY_EXEC = 'Exec';
  DESKTOP_KEY_STARTUPNOTIFY = 'StartupNotify';
  DESKTOP_KEY_TERMINAL = 'Terminal';

  DESKTOP_TYPE_APPLICATION = 'Application';

implementation

end.
