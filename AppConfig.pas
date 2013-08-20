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

unit AppConfig;

interface

uses
  SysUtils, Forms;

const

  // Application's informations
  APP_NAME  = 'ASuite';
  APP_TITLE = APP_NAME;

  VERSION_MAJOR    = '2'; { TODO : Update version info with IDE macro }
  VERSION_MINOR    = '0';
  VERSION_RELEASE  = '0';
  VERSION_BUILD    = '1163';
  VERSION_COMPLETE = VERSION_MAJOR   + '.' +
                     VERSION_MINOR   + '.' +
                     VERSION_RELEASE;
  VERSION_PRERELEASE = 'Alpha 3'; //For Alpha and Beta version
                                 //(VERSION_RELEASE + VERSION_PRERELEASE = Version)

  BACKUP_DIR     = 'backup\';
  CACHE_DIR      = 'cache\';
  CACHELARGE_DIR = 'cache\large\';
  MENUTHEMES_DIR = 'menuthemes\';
  ICONS_DIR      = 'icons\';
  SMALLICONS_DIR = ICONS_DIR + '16x16\';
  LARGEICONS_DIR = ICONS_DIR + '32x32\';

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

var
  //Paths
  SUITE_FULLFILENAME : String;
  SUITE_FILENAME     : String;
  SUITE_DRIVE        : String;
  SUITE_PATH         : String;
  SUITE_WORKING_PATH : String;
  SUITE_CACHE_PATH   : String;
  SUITE_CACHELARGE_PATH : String;
  SUITE_BACKUP_PATH     : String;
  SUITE_SMALLICONS_PATH : String;
  SUITE_LARGEICONS_PATH : String;
  SUITE_MENUTHEMES_PATH : String;
  SUITE_CURRENTTHEME_PATH : String;

  // Application's files
  SUITE_LIST_PATH    : String;

  //Menu small icons
  IMAGE_INDEX_ASuite,       //ID = 0;
  IMAGE_INDEX_Cat,          //ID = 1;
  IMAGE_INDEX_Help,         //ID = 2;
  IMAGE_INDEX_Options,      //ID = 3;
  IMAGE_INDEX_AddCat,       //ID = 4;
  IMAGE_INDEX_AddFile ,     //ID = 5;
  IMAGE_INDEX_AddFolder,    //ID = 6;
  IMAGE_INDEX_Delete,       //ID = 7;
  IMAGE_INDEX_Property,     //ID = 8;
  IMAGE_INDEX_Save,         //ID = 9;
  IMAGE_INDEX_Folder,       //ID = 10;
  IMAGE_INDEX_AddGroupFile, //ID = 11;
  IMAGE_INDEX_GroupFile,    //ID = 12;
  IMAGE_INDEX_NOTFOUND,     //ID = 13;
  IMAGE_INDEX_Run,          //ID = 14;
  IMAGE_INDEX_Cut,          //ID = 15;
  IMAGE_INDEX_Copy,         //ID = 16;
  IMAGE_INDEX_Paste ,       //ID = 17;
  IMAGE_INDEX_Search,       //ID = 18;
  IMAGE_INDEX_SearchType,   //ID = 19;
  IMAGE_INDEX_Url,          //ID = 20;
  IMAGE_INDEX_Accept,       //ID = 21;
  IMAGE_INDEX_Cancel        //ID = 22;
                            : Integer;

  //Menu large icons
  IMAGELARGE_INDEX_General,  //ID = 0;
  IMAGELARGE_INDEX_Advanced, //ID = 1;
  IMAGELARGE_INDEX_Items,    //ID = 2;
  IMAGELARGE_INDEX_Hotkey,   //ID = 3;
  IMAGELARGE_INDEX_Mouse,    //ID = 4;
  IMAGELARGE_INDEX_Trayicon, //ID = 5;
  IMAGELARGE_INDEX_Stats,    //ID = 6;
  IMAGELARGE_INDEX_Behavior, //ID = 7;
  IMAGELARGE_INDEX_PropGeneral //ID = 8;
                            : Integer;

implementation

uses ulSysUtils;

initialization
  //Default paths
  SUITE_FULLFILENAME        := Application.ExeName;
  SUITE_FILENAME            := ExtractFileName(SUITE_FULLFILENAME);
  SUITE_DRIVE               := LowerCase(ExtractFileDrive(SUITE_FULLFILENAME));
  SUITE_PATH                := IncludeTrailingBackslash(ExtractFileDir(SUITE_FULLFILENAME));
  if IsDriveRoot(SUITE_PATH)
    then SUITE_WORKING_PATH := GetCorrectWorkingDir(SUITE_PATH)
    else SUITE_WORKING_PATH := SUITE_PATH;
  SUITE_WORKING_PATH        := LowerCase(SUITE_WORKING_PATH);
  SUITE_CACHE_PATH          := SUITE_WORKING_PATH + CACHE_DIR;
  SUITE_CACHELARGE_PATH     := SUITE_WORKING_PATH + CACHELARGE_DIR;
  SUITE_BACKUP_PATH         := SUITE_WORKING_PATH + BACKUP_DIR;
  SUITE_MENUTHEMES_PATH     := SUITE_WORKING_PATH + MENUTHEMES_DIR;
  //List
  if (ExtractFileExt(ParamStr(1)) = EXT_SQL) and FileExists(ParamStr(1)) then
    SUITE_LIST_PATH := ParamStr(1)
  else
    SUITE_LIST_PATH := SUITE_WORKING_PATH + ChangeFileExt(SUITE_FILENAME, EXT_SQL);

end.
