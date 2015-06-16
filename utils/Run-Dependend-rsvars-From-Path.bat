@echo off
  for %%v in (L Latest 5 6 7 8 9 10 11 12 13 14 15 16) do if /I !%1!==!%%v! goto :bds
  if /I !%1!==!! goto :help
  echo Unknown BDS version "%1"
  goto :help

:bds
  :: now %1 has been verified; verify %2
  if !%2!==!! goto :bdsBegin
  for %%v in (F fixTargets B msbuild R D C S RS Delphi CBuilder RadStudio win32 win64) do if /I !%2!==!%%v! goto :bdsBegin
  echo Unknown BDS Target "%2"
  goto :help

:bdsBegin
  echo Parameters: "%*"
  setlocal
  call :%1
  set bdsBatch=%0
  set bdsVersion=%1
  set bdsTarget=%2
  echo product: %bdsProduct%
  echo BDS EXE: %bdsExe%
  if !!==!%bdsExe%! goto :noBdsExe
  :: there is no rsvars.bat path in the registry, so get it from the bds.exe path:
  for %%p in (%bdsExe%) do set rsVars="%%~dpprsvars.bat" & set binDir="%%~dpp"
  echo rsvars: %rsVars%
  echo binDir: %binDir%
::  type %rsVars%
  call %rsVars%
  :: init only for msbuild or fixTargets:
  for %%v in (F fixTargets B R msbuild) do if /I !%bdsTarget%!==!%%v! goto :performInit
  goto :skipInit
:performInit
  :: Delphi 2007 has environment variables setup differently
  if not %bdsVersion%==5 call :initDelphiUnicode
  if %bdsVersion%==5 call :initDelphi2007
  echo msbuild.exe: %msBuildExe%
  if !!==!%msBuildExe%! goto :noMsBuildExe
  if not !!==!%missingTarget%! goto :missingTarget
:skipInit
::  path
  call :do pushd %~dp0
::  call :do call Dependencies Set
  call :do popd
  if !%2!==!! goto :bdsEnd
  if not exist %bdsExe% goto :noBdsExe
  goto :bdsExe
:bdsExe
  :: cannot pass %* as there is no way to get rid of %1 and %2, see http://stackoverflow.com/questions/9363080/how-to-make-shift-work-with-in-batch-files
  call :%2 %3 %4
  goto :bdsEnd
:missingTarget
  echo missing one or more of "%requiredTargets%" in "%targetDirectory%"
  goto :bdsEnd
:noMsBuildExe
  echo msbuild.exe does not exist
  goto :bdsEnd
:noBdsExe
  echo bds.exe does not exist
  goto :bdsEnd
:bdsEnd
  endlocal
  goto :eof

:B
:msbuild
  :: check if the project exists
  call :do %msBuildExe% /p:Platform=%2 /target:build /p:DCC_BuildAllUnits=true /p:config=Debug %1 /l:FileLogger,Microsoft.Build.Engine;logfile="log.txt"
  goto :eof

:R
  :: check if the project exists
  echo %Platform%
  call :do %msBuildExe% /p:Platform=%2 /target:build /p:DCC_BuildAllUnits=true /p:config=Release %1 /l:FileLogger,Microsoft.Build.Engine;logfile="log64.txt"
  goto :eof

:: TODO add options for build Config 3, build Platform 4, search paths 5, conditional defines 6.

    for /f "tokens=2,4" %%c in ('echo %~4 %~3') do set buildlog=%~dp2%%c.%%d.MSBuildLog.txt
    echo %buildlog%
    %FrameworkDir%\msbuild.exe /nologo %2 /target:build /p:DCC_BuildAllUnits=true /p:%3 /p:%4 /p:%5 /p:%6 /l:FileLogger,Microsoft.Build.Engine;logfile=%buildlog%
  goto :eof

:F
:fixTargets
  :: F/fixTargets is handled implicitly by :performInit
  goto :eof

:D
:Delphi
  call :FixEditorLineEndsTtr
  call :do start "%bdsProduct%" %bdsExe% -pDelphi
  goto :eof

:C
:CBuilder
  call :FixEditorLineEndsTtr
  call :do start "%bdsProduct%" %bdsExe% -pCBuilder
  goto :eof

:S
:RS
:RadStudio
  call :FixEditorLineEndsTtr
  call :do start "%bdsProduct%" %bdsExe%
  goto :eof

:FixEditorLineEndsTtr
  :: http://stackoverflow.com/questions/18499797/less-than-or-equal-to
  :: Delphi > 2010, no EditorLineEnds.ttr issues:
  if %bdsVersion% gtr 6 goto :eof
  :: http://wiert.me/2011/09/29/editorlineends-ttr-what-is-it/
  set EditorLineEndsTtr=%temp%\EditorLineEnds.ttr
  :: http://unserializableone.blogspot.nl/2009/04/create-unique-temp-filename-with-batch.html
  set EditorLineEndsTtrRandom=%EditorLineEndsTtr%%Random%.ttr
  if exist "%EditorLineEndsTtr%" (
	echo EditorLineEndsTtrRandom="%EditorLineEndsTtrRandom%"
	echo Moving "%EditorLineEndsTtr%" to "%EditorLineEndsTtrRandom%":
	move "%EditorLineEndsTtr%" "%EditorLineEndsTtrRandom%" > nul
  )
  goto :eof

:getBdsExe
  :: HKCU
  FOR /F "tokens=2*" %%P IN ('REG QUERY HKEY_CURRENT_USER\Software\%1 /v App 2^>NUL') DO call :do set bdsExe="%%Q"
  :: HKLM x86
  if [%bdsExe%]==[] FOR /F "tokens=2*" %%P IN ('REG QUERY HKEY_LOCAL_MACHINE\SOFTWARE\%1 /v App 2^>NUL') DO call :do set bdsExe="%%Q"
  :: HKLM x64
  if [%bdsExe%]==[] FOR /F "tokens=2*" %%P IN ('REG QUERY HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\%1 /v App 2^>NUL') DO call :do set bdsExe="%%Q"
  goto :eof

:5
  call :getBdsExe Borland\BDS\5.0
  set bdsProduct=RAD Studio 2007
  goto :eof
:6
  call :getBdsExe Embarcadero\BDS\6.0
  set bdsProduct=RAD Studio 2009
  goto :eof
:7
  call :getBdsExe Embarcadero\BDS\7.0
  set bdsProduct=RAD Studio 2010
  goto :eof
:8
  call :getBdsExe Embarcadero\BDS\8.0
  set bdsProduct=RAD Studio XE1
  goto :eof
:9
  call :getBdsExe Embarcadero\BDS\9.0
  set bdsProduct=RAD Studio XE2
  goto :eof
:10
  call :getBdsExe Embarcadero\BDS\10.0
  set bdsProduct=RAD Studio XE3
  goto :eof
:11
  call :getBdsExe Embarcadero\BDS\11.0
  set bdsProduct=RAD Studio XE4
  goto :eof
:12
  call :getBdsExe Embarcadero\BDS\12.0
  set bdsProduct=RAD Studio XE5
  goto :eof
:13
  call :getBdsExe Embarcadero\BDS\13.0
  set bdsProduct=Appmethod 1
  goto :eof
:14
  call :getBdsExe Embarcadero\BDS\14.0
  set bdsProduct=RAD Studio XE6
  goto :eof
:15
  FOR /F "tokens=2*" %%P IN ('REG QUERY HKEY_CURRENT_USER\Software\Embarcadero\BDS\15.0 /v App 2^>NUL') DO call :do set bdsExe="%%Q"
  set bdsProduct=Studio XE7
  goto :eof
:16
  FOR /F "tokens=2*" %%P IN ('REG QUERY HKEY_CURRENT_USER\Software\Embarcadero\BDS\16.0 /v App 2^>NUL') DO call :do set bdsExe="%%Q"
  set bdsProduct=Studio XE7
  goto :eof

:L
:latest
  :: get %bdsExe% for youngest Delphi version
  call :16
  if [%bdsExe%]==[] call :15
  if [%bdsExe%]==[] call :14
  if [%bdsExe%]==[] call :13
  if [%bdsExe%]==[] call :12
  if [%bdsExe%]==[] call :11
  if [%bdsExe%]==[] call :10
  if [%bdsExe%]==[] call :9
  if [%bdsExe%]==[] call :8
  if [%bdsExe%]==[] call :7
  if [%bdsExe%]==[] call :6
  if [%bdsExe%]==[] call :5
  goto :eof

:initDelphi2007
  :: upon failure: clear msBuildExe
  set targetDirectory=%FrameworkDir%%FrameworkVersion%
    :: x64 system: alternative is the non-64 Framework directory
    :: prevent the below error as Delphi 2007 projects refer hard coded to "$(MSBuildBinPath)\Borland.Delphi.Targets"
    :: but the installer forgets to copy the Borland*.Targets to the x64 framework directory.
    :: myDelphi2007Project.dproj(49,11): error MSB4019: The imported project "C:\Windows\Microsoft.NET\Framework64\v2.0.50727\Borland.Delphi.Targets" was not found. Confirm that the path in the <Import> declaration is correct, and that the file exists on disk.
  :: http://stackoverflow.com/questions/601089/detect-whether-current-windows-version-is-32-bit-or-64-bit/1833044#1833044
  if defined ProgramFiles(x86) (
    :: delete the last 2 characters ("64\") of %FrameworkDir% to get the non-x64 framework directory: %var:~0,-4%
    set sourceDirectory=%FrameworkDir:~0,-3%\%FrameworkVersion%\
  ) else (
    set sourceDirectory=your installation DVD
  )
  set msBuildExe=%targetDirectory%\msbuild.exe
  set requiredTargets=Borland.Common.Targets Borland.Cpp.Targets Borland.Delphi.Targets Borland.Group.Targets
  call :assertRequiredTargets
  goto :eof

:initDelphiUnicode
  :: remove double quote from %binDir% at begin and end, so not %binDir:"=% but %binDir:"=% but %binDir:~1,-1%
  :: also remove trailing backslash.
  set targetDirectory=%binDir:~1,-2%
  set sourceDirectory=%binDir%
  set msBuildExe=%FrameworkDir%\msbuild.exe
  :: Defaults, then exception
  set requiredTargets=CodeGear.Common.Targets CodeGear.Cpp.Targets CodeGear.Delphi.Targets CodeGear.Deployment.Targets CodeGear.Group.Targets CodeGear.Profiles.Targets
  if %bdsVersion%==8 set requiredTargets=CodeGear.Common.Targets CodeGear.Cpp.Targets CodeGear.Delphi.Targets CodeGear.Group.Targets
  if %bdsVersion%==13 set requiredTargets=CodeGear.Common.Targets CodeGear.Cpp.Targets CodeGear.Delphi.Targets CodeGear.Deployment.Targets CodeGear.Group.Targets CodeGear.Profiles.Targets
  call :assertRequiredTargets
  goto :eof

:assertRequiredTargets
  echo targetDirectory: %targetDirectory%
  echo sourceDirectory: %sourceDirectory%
  set missingTarget=
  for %%r in (%requiredTargets%) do if not exist "%targetDirectory%\%%r" call :missingRequiredTarget "%targetDirectory%\%%r"
  goto :eof

:missingRequiredTarget
  :: first check if we are in "fixing" mode, running as Administrator and can fix the issue.
  if [%sourceDirectory%]==[your installation DVD] goto :cantFixMissingRequiredTarget
  if %bdsTarget%==F goto :tryFixMissingRequiredTarget
  if %bdsTarget%==fixTargets goto :tryFixMissingRequiredTarget
:cantFixMissingRequiredTarget
  set missingTarget=%*
  echo Missing %*
  echo You can usually copy it as Administrator from %sourceDirectory% using:
  echo %bdsBatch% %bdsVersion% fixTargets
  goto :eof

:tryFixMissingRequiredTarget
  call :isAdmin
  if %errorlevel% == 0 (
    :: Running with admin rights.
    call :do copy %sourceDirectory%%~nx1 %*
    goto :eof
  ) else (
    echo Error: not running as administrator, so cannot fix missing Targets file %*
    goto :cantFixMissingRequiredTarget
  )

:isAdmin
  ::sfc 2>&1 | find /i "/SCANNOW" >nul
  setlocal enabledelayedexpansion
  for /f "tokens=* delims=" %%s in ('sfc 2^>^&1^|MORE') do @set "output=!output!%%s"
  echo "%output%"|findstr /I /C:"/scannow">nul 2>&1
  endlocal
  exit /b

:do
  cd
  echo %*
  %*
  goto :eof

:help
  echo Syntax:
  echo     %0 Version [Target] [Parameters]
  echo.
  echo     where `Version` is the BDS Version to run `rsvars.bat` for
  echo     and `Target` is the BDS Target to optionally run after `rsvars.bat` has been run.
  echo.
  echo Supported BDS versions:
  echo      5 - Delphi 2007
  echo      6 - Delphi 2009
  echo      7 - Delphi 2010
  echo      8 - Delphi XE
  echo      9 - Delphi XE2
  echo     10 - Delphi XE3
  echo     11 - Delphi XE4
  echo     12 - Delphi XE5
  echo     13 - Appmethod 1.13
  echo     14 - Delphi XE6/Appnethod 1.14
  echo     15 - Delphi XE7/Appnethod 1.15
  echo     16 - Delphi XE8
  echo      L - Latest/Youngest installed version of the above.
  echo.
  echo Supported BDS Targets:
  echo     D          - Delphi
  echo     C          - C++ Builder
  echo     S          - RAD Studio/Appmethod
  echo     RS         - RAD Studio/Appmethod
  echo     Delphi     - Delphi
  echo     CBuilder   - C++ Builder
  echo     RadStudio  - RAD Studio/Appmethod
  echo     B          - msbuild build project in Debug mode
  echo     msbuild    - msbuild build project in Debug mode
  echo     R          - msbuild build project in Release mode
  echo     F          - fix missing *.Targets files (requires UAC Administrator token)
  echo     fixTargets - fix missing *.Targets files (requires UAC Administrator token)
  echo.
  echo Some Targets (like B) support extra parameters.
  echo     B          - .dproj or .groupproj file
  echo     msbuild    - .dproj or .groupproj file

