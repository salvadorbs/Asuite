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

unit ulEnumerations;

interface

type
  //Autorun
  TAutorunType = (
      atNever,
      atAlwaysOnStart,
      atSingleInstance, //atNoPrevInstances
      atAlwaysOnClose
  );

  //Run action on file execution
  TActionOnExecution = (
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
      rmOpenFolder
  );

implementation

end.
