{
Copyright (C) 2006-2013 Matteo Salvi

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

interface

const

  // Application's informations
  APP_NAME  = 'ASuite';
  APP_TITLE = APP_NAME;

  VERSION_PRERELEASE = 'Beta'; //For Alpha and Beta version

  BACKUP_DIR     = 'backup\';
  CACHE_DIR      = 'cache\';
  LOCALE_DIR     = 'locale\';
  CACHELARGE_DIR = 'cache\large\';
  MENUTHEMES_DIR = 'menuthemes\';
  ICONS_DIR      = 'icons\';
  ICONS_POPUPMENU_DIR = ICONS_DIR + 'popupmenu\';
  ICONS_OPTIONS_DIR   = ICONS_DIR + 'options\';
  ICONS_TREE_DIR      = ICONS_DIR + 'tree\';

  // Caratteri speciali
  LF                = #10;              { line feed }
  CR                = #13;              { carriage return }
  SLASH             = '/';              { forward slash }
  BACKSLASH         = '\';              { back slash }
  CRLF              = CR + LF;          { new line }

  //FileSystem
  PATH_SEPARATOR    = BACKSLASH;  // Windows only, for now
  SLASHES           = [SLASH, BACKSLASH];
  DriveLetters      = ['a'..'z', 'A'..'Z'];

  //Extensions
  EXT_XML       = '.xml';
  EXT_SQL       = '.sqlite';
  EXT_SQLBCK    = '.sqbck';
  EXT_XMLBCK    = '.bck';
  EXT_ICO       = '.ico';
  EXT_LNK       = '.lnk';
  EXT_EXE       = '.exe';

  //File
  DEBUG_FILE    = 'Debug.txt';
  THEME_INI     = 'theme.ini';
  BACKUP_FILE   = APP_NAME + '_%s' + EXT_SQLBCK;

  //Form
  frmMainWidth  = 190;
  frmMainHeight = 440;
  frmMenuID     = 123456789;

  // PageControl Indexes
  PG_LIST       = 0;
  PG_SEARCH     = 1;

  // PageControl Indexes
  PG_MENULIST   = 0;
  PG_MENUMRU    = 1;
  PG_MENUMFU    = 2;
  PG_MENUSEARCH = 3;

  //ASuite placeholders for path
  CONST_PATH_ASUITE  = '$asuite';
  CONST_PATH_DRIVE   = '$drive';

  //Parameters' variables: constant placeholders
  CONST_PARAM_PENDRV = '%pendrive%';

  //ASuite filename icons
  FILEICON_ASuite     = '0.ico';
  FILEICON_Cat        = '1.ico';
  FILEICON_Help       = '2.ico';
  FILEICON_Options    = '3.ico';
  FILEICON_AddCat     = '4.ico';
  FILEICON_AddFile    = '5.ico';
  FILEICON_AddFolder  = '6.ico';
  FILEICON_Delete     = '7.ico';
  FILEICON_Property   = '8.ico';
  FILEICON_Save       = '9.ico';
  FILEICON_Folder     = '10.ico';
  FILEICON_NOTFOUND   = '13.ico';
  FILEICON_Run        = '14.ico';
  FILEICON_Cut        = '15.ico';
  FILEICON_Copy       = '16.ico';
  FILEICON_Paste      = '17.ico';
  FILEICON_Search     = '18.ico';
  FILEICON_SearchType = '19.ico';
  FILEICON_Url        = '20.ico';
  FILEICON_Accept     = '21.ico';
  FILEICON_Cancel     = '22.ico';

  FILELARGEICON_General  = '0.ico';
  FILELARGEICON_Advanced = '1.ico';
  FILELARGEICON_Items    = '2.ico';
  FILELARGEICON_Hotkey   = '3.ico';
  FILELARGEICON_Mouse    = '4.ico';
  FILELARGEICON_Trayicon = '5.ico';
  FILELARGEICON_Stats    = '6.ico';
  FILELARGEICON_Behavior = '7.ico';
  FILELARGEICON_PropGeneral = '8.ico';

implementation

end.
