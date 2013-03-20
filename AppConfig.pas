{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit AppConfig;

{$MODE Delphi}

interface

uses
  SysUtils, Forms;

const

  // Application's informations
  APP_NAME = 'ASuite';

  APP_TITLE         = APP_NAME;
  ReleaseVersion    = '2.0';
  PreReleaseVersion = ' Alpha 2'; //For Alpha and Beta version
                               //(ReleaseVersion + PreReleaseVersion = Version)

  BACKUP_DIR    = 'backup\';
  CACHE_DIR     = 'cache\';
  ICONS_DIR     = 'icons\';
//  MENUTHEMES_DIR = 'menuthemes\'; -> For graphic menu

  // Caratteri speciali
  LF                = #10;              { line feed }
  CR                = #13;              { carriage return }
  SLASH             = '/';              { forward slash }
  BACKSLASH         = '\';              { back slash }
  CRLF              = CR + LF;          { new line }

  //{ FileSystem
  PATH_SEPARATOR    = BACKSLASH;  // Windows only, for now
  SLASHES           = [SLASH, BACKSLASH];
  DriveLetters      = ['a'..'z', 'A'..'Z'];

  // Extensions
  EXT_XML       = '.xml';
  EXT_SQL       = '.sqlite';
  EXT_SQLBCK    = '.sqbck';
  EXT_XMLBCK    = '.bck';
  EXT_ICO       = '.ico';
  EXT_LNK       = '.lnk';

  //Form
  frmMainWidth  = 190;
  frmMainHeight = 440;
  frmMenuID     = 123456789;

  // PageControl Indexes
  PG_LIST       = 0;
  PG_SEARCH     = 1;

  //ASuite placeholders for path
  CONST_PATH_ASUITE  = '$asuite';
  CONST_PATH_DRIVE   = '$drive';

  //Parameters' variables: constant placeholders
  CONST_PARAM_PENDRV = '%pendrive%';

var
  //Paths
  SUITE_FULLFILENAME : String;
  SUITE_FILENAME     : String;
  SUITE_DRIVE        : String;
  SUITE_PATH         : String;
  SUITE_WORKING_PATH : String;
  SUITE_CACHE_PATH   : String;
  SUITE_BACKUP_PATH  : String;
  SUITE_ICONS_PATH   : String;

  // Application's files
  SUITE_LIST_PATH     : String;

  //Menu icons
  IMG_ASuite,       //ID = 0;
  IMG_Cat,          //ID = 1;
  IMG_Help,         //ID = 2;
  IMG_Options,      //ID = 3;
  IMG_AddCat,       //ID = 4;
  IMG_AddFile ,     //ID = 5;
  IMG_AddFolder,    //ID = 6;
  IMG_Delete,       //ID = 7;
  IMG_Property,     //ID = 8;
  IMG_Save,         //ID = 9;
  IMG_Folder,       //ID = 10;
  IMG_AddGroupFile, //ID = 11;
  IMG_GroupFile,    //ID = 12;
  IMG_NOTFOUND,     //ID = 13;
  IMG_Run,          //ID = 14;
  IMG_Cut,          //ID = 15;
  IMG_Copy,         //ID = 16;
  IMG_Paste ,       //ID = 17;
  IMG_Search,       //ID = 18;
  IMG_SearchType,   //ID = 19;
  IMG_Url,          //ID = 20;
  IMG_Accept,       //ID = 21;
  IMG_Cancel       //ID = 22;
                    : Integer;

resourcestring

  msgCopy           = 'Copy_';
  msgNoName         = 'No name';
  msgSaveCompleted  = 'Save completed';
  msgConfirm        = 'Are you sure?';
  msgProcessingItems = 'Processing items (%.0f%%): %d';
  //Import
  msgImportProgress = 'Import in progress...';
  msgImportTitle1   = 'Select a launcher from which to import list and settings';
  msgImportTitle2   = 'Select the location of list to import in ASuite. Select if you want list and/or settings';
  msgImportTitle3   = 'Select which items to import';
  msgImportTitle4   = 'Complete';
  msgItemsImported  = 'Import finished. %d items imported';
  msgNext           = 'Next >';
  msgImport         = 'Import';
  msgClose          = 'Close';
  //Classic Menu
  msgLongMFU        = 'Most Frequently Used';
  msgLongMRU        = 'Recents';
  msgShortMFU       = 'MFU';
  msgShortMRU       = 'MRU';
  msgList           = 'List';
  //Errors
  msgErrEmptyName   = 'Application''s name field is empty';
  msgErrGeneric     = '%s error raised, with message: %s';
  msgErrIcon        = 'Cannot use icon %s';
  msgErrNoIcon      = 'Couldn''t find the icon %s';
  msgErrRun         = 'Cannot run %s';
  msgErrSave        = 'Save failed because of an error';
  msgFileNotFound   = 'File not found';
  msgImportFailed   = 'Import failed because of an error';

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
  SUITE_BACKUP_PATH         := SUITE_WORKING_PATH + BACKUP_DIR;
  SUITE_ICONS_PATH          := SUITE_WORKING_PATH + ICONS_DIR;
  //List
  if (ExtractFileExt(ParamStr(1)) = EXT_SQL) and FileExists(ParamStr(1)) then
    SUITE_LIST_PATH := ParamStr(1)
  else
    SUITE_LIST_PATH := SUITE_WORKING_PATH + ChangeFileExt(SUITE_FILENAME, EXT_SQL);

end.
