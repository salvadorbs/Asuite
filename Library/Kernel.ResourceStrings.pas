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

unit Kernel.ResourceStrings;

{$Mode DelphiUnicode}

interface

resourcestring

  //TODO: reorder them in categories

  msgCopy = 'Copy_';
  msgClose = 'Close';
  msgErrRun = 'Cannot run %s';
  msgAutorun = 'Autorun';
  msgAdvanced = 'Advanced';
  msgBehavior = 'Behavior';
  msgConfirm = 'Are you sure?';
  msgErrGeneric = '%s error raised, with message: %s';
  msgCMOpenFolder = '[Open this folder]';
  msgConfirmRunCat = 'Are you sure execute category %s?';
  msgEjectHardware = 'Safely remove hardware';
  msgErrEmptyName = 'Application''s name field is empty';
  msgAvailableVersion = 'There is a new version of ASuite available. Do you want download it?';
  msgCancelScanFolder = 'ScanFolder is still running. Are you sure you want to cancel it?';
  msgConfirmDeleteItem = 'Are you sure you want to delete it?';
  msgDefaultItemSettings = 'Default (item''s settings)';
  msgErrEmptyPath = 'Path field is empty';
  msgErrEmptyUserName = 'Username field is empty';
  msgErrIcon = 'Cannot use icon %s';
  msgErrNoIcon = 'Couldn''t find icon file %s';
  msgErrNoThemeIni = 'Couldn''t find file %s';
  msgErrRegCMHotkey = 'Register Classic Menu''s hotkey failed';
  msgErrRegGMHotkey = 'Register Graphic Menu''s hotkey failed';
  msgErrRegWindowHotkey = 'Register window''s hotkey failed';
  msgErrSave = 'Save failed because of an error';
  msgErrScanFolderEmptyPath = 'You haven''t select any folder!';
  msgErrScanFolderMissingTypes = 'You haven''t add any file types!';
  msgExit = 'Exit';
  msgFileNotFound = 'File not found';
  msgFilterBackground = 'Files supported (*.png;*.bmp)|*.png;*.bmp|All files|*.*';
  msgFilterExe = 'Executables (*.exe)|*.exe|All files|*.*';
  msgFilterIcon = 'Icons (*.ico)|*.ico';
  msgFilterIconExe = 'Files supported (*.ico;*.exe)|*.ico;*.exe|All files|*.*';
  msgFilterPicture = 'Personal Picture [48x48] (*.jpg, *.png)|*.jpg;*.png';
  msgFolderNotFound = 'Folder not found';
  msgFoundNumFiles = 'Found and added %d files in list';
  msgGMAbout = 'About';
  msgGMDocuments = 'Documents';
  msgGMDriveName = 'Drive (%s)';
  msgGMExplore = 'Explore';
  msgGMHardDiskSpace = '%s free of %s';
  msgGMMusic = 'Music';
  msgGMOptions = 'Options';
  msgGMPictures = 'Pictures';
  msgGMShow = 'Show %s';
  msgGMVideos = 'Videos';
  msgGeneral = 'General';
  msgHotkey = 'Hotkey';
  msgImport = 'Import';
  msgImportFailed = 'Import failed because of an error';
  msgImportProgress = 'Import in progress...';
  msgImportTitle2 = 'Select the location of file list to import in ASuite';
  msgImportTitle3 = 'Select which items to import';
  msgItTakeTime = 'It may take time...';
  msgItems = 'Items';
  msgItemsImported = 'Import finished. %d items imported';
  msgList = 'List';
  msgLongMFU = 'Most Used';
  msgLongMRU = 'Recents';
  msgMainWindow = 'Main Window';
  msgMissedTask = 'Seems scheduler didn''t run the item %s as expected by options. Do you want execute it now?';
  msgMouse = 'Mouse';
  msgNext = 'Next >';
  msgNoAvailableVersion = 'No new version available';
  msgNoName = 'No name';
  msgNotifyDropFiles = 'Added successfully %d files';
  msgNotifyDropLink = 'Dropped text in list successfully';
  msgNotifyExecuteItem = 'Execute %s';
  msgNotifyHotkey = 'Execute %s using hotkey';
  msgNotifyScheduler = 'Execute %s using scheduler';
  msgOpenOptions = 'Options...';
  msgOperationCompleted = 'Operation completed';
  msgProcessingItems = 'Processing items (%.0f%%): %d';
  msgRestartAsuiteChanges = 'You must restart ASuite to apply these changes.';
  msgRunAsTitle = 'Run As...';
  msgSaveCompleted = 'Save completed';
  msgScanningCancel = 'Cancel';
  msgScanningProgress = 'Scanning in progress...';
  msgShortMFU = 'MFU';
  msgShortMRU = 'MRU';
  msgShowASuite = 'Show ASuite';
  msgStats = 'Stats';
  msgTrayicon = 'Trayicon';
  msgUseTriggerSensors = 'Active Sensors for %s';
  msgHotkeyNoMod = 'You haven''t select any modifier keys!';
  msgHotkeyNoKey = 'You haven''t select any keys!';
  msgHotkeyNotAvailable = 'This hotkey is being used already by another software or system itself. Please choose another one.';
  msgCancel = 'Cancel';
  msgStop = 'Stop';

  implementation

end.

