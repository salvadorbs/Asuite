{
Copyright (C) 2006-2020 Matteo Salvi

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

interface

const

  // Application's informations
  APP_NAME  = 'ASuite';
  APP_TITLE = APP_NAME;

  VERSION_PRERELEASE = ''; //For Alpha and Beta version

  BACKUP_DIR     = 'backup\';
  CACHE_DIR      = 'cache\';
  LOCALE_DIR     = 'locale\';
  MENUTHEMES_DIR = 'themes\';
  ICONS_DIR      = 'icons\';
  BUTTONS_DIR    = 'buttons\';

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
  EXT_URL       = '.url';
  EXT_EXE       = '.exe';

  EXT_PATH_MASK = '*';

  //File
  DEBUG_FILE    = 'Debug.txt';
  THEME_INI     = 'theme.ini';
  BACKUP_FILE   = APP_NAME + '_%s' + EXT_SQLBCK;

  //Form
  frmMainWidth  = 190;
  frmMainHeight = 440;
  frmGMenuID    = 123456789;
  frmCMenuID    = 987654321;

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
  CONST_PATH_FOLDERICON = '%foldericon%';
  CONST_PATH_URLICON = '%urlicon%';

  //ASuite filename icons
  FILEICON_Folder     = 'folder';
  FILEICON_Url        = 'page_url';

  //Modifier buttons for TfrmShortcutGrabber
  CTRL_NORMAL_FILENAME  = 'ctrl_normal.png';
  CTRL_HOVER_FILENAME   = 'ctrl_hover.png';
  CTRL_CLICKED_FILENAME = 'ctrl_clicked.png';

  ALT_NORMAL_FILENAME  = 'alt_normal.png';
  ALT_HOVER_FILENAME   = 'alt_hover.png';
  ALT_CLICKED_FILENAME = 'alt_clicked.png';

  SHIFT_NORMAL_FILENAME  = 'shift_normal.png';
  SHIFT_HOVER_FILENAME   = 'shift_hover.png';
  SHIFT_CLICKED_FILENAME = 'shift_clicked.png';

  WINKEY_NORMAL_FILENAME  = 'winkey_normal.png';
  WINKEY_HOVER_FILENAME   = 'winkey_hover.png';
  WINKEY_CLICKED_FILENAME = 'winkey_clicked.png';

implementation

end.
