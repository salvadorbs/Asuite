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

unit Kernel.Enumerations;

{$MODE Delphi}

interface

type
  //Autorun
  TAutorunType = (
      atNever,
      atAlwaysOnStart,
      atSingleInstance, //atNoPrevInstances
      atAlwaysOnClose
  );

  TTrayiconActionClick = (
      tcNone,
      tcShowWindow,
      tcShowGraphicMenu,
      tcShowClassicMenu
  );

  //Run action on file execution
  TActionOnExecute = (
      aeDefault = 0,     //Value From Config.cxActionOnExe
      aeJustRun = 1,
      aeRunAndHide = 2,
      aeRunAndClose = 3
  );

  //Tree types
  TvTreeDataType = (
      vtdtCategory,
      vtdtFile,
      vtdtFolder,
      vtdtSeparator
  );

  //Search types
  TSearchType = (
      stName,
      stPathFile,
      stPathIcon,
      stWorkingDir,
      stParameters
  );

  //Shortcut fields
  TShortcutField = (
      sfPathFile,
      sfParameter,
      sfWorkingDir,
      sfPathIcon
  );

  //Scheduler modes
  TSchedulerMode = (
      smDisabled = 0,
      smOnce = 1,
      smHourly = 2,
      smDaily = 3
  );

  //Launcher State
  TLauncherState = (
      lsStartUp,  //Launcher Startup Time
      lsShutdown, //Launcher Shutdown Time
      lsNormal,
      lsImporting,
      lsScanning,
      lsDeleting
  );

  TIconSize = (
      isSmall, //Small Icon
      isLarge  //Large Icon
  );

  TSpecialListMode = (
      lmMRU,
      lmMFU
  );

  TAutorunListMode = (
      amStartup,
      amShutdown
  );

  TRunMode = (
      rmNormal,
      rmAsUser,
      rmAsAdmin,
      rmExplorePath
  );

  TListType = (
      ltASuite1,
      ltASuite2,
      ltwppLauncher1,
      ltPStart1
  );

  TASuiteTheme = (
      atWindowsSystem,
      atLight,
      atDark
  );

implementation

end.
