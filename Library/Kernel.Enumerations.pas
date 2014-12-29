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

unit Kernel.Enumerations;

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
      stPathExe,
      stPathIcon,
      stWorkingDir,
      stParameters
  );

  //Shortcut fields
  TShortcutField = (
      sfPathExe,
      sfParameter,
      sfWorkingDir
  );

  //Run modes
  TRunMode = (
      rmNormal,
      rmRunAs,
      rmRunAsAdmin,
      rmAutorun,
      rmAutorunSingleInstance,
      rmOpenFolder
  );

  //Run modes
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
      lsImporting
  );

  TIconSize = (
      isSmall, //Small Icon
      isLarge, //Large Icon
      isAny    //Small or Large Icon
  );

  TMousePosition = (
      mpTopLeft,
      mpLeft,
      mpBottomLeft,
      mpBottom,
      mpBottomRight,
      mpRight,
      mpTopRight,
      mpTop,
      mpInside
      );

  TMouseTrigger = (
      mtLeft,
      mtMiddle,
      mtRight,
      mtWheelUp,
      mtWheelDown,
      mtMove,
      mtNone
      );

  TSensorAction = (
      saNone,
      saShowMainForm,
      saShowGraphicMenu,
      saShowClassicMenu,
      saRunItemX,
      saShowCategory,
      saShowFolder,
      saShowDesktop
      );

  TSpecialListMode = (
      lmMRU,
      lmMFU
      );

implementation

end.
