@echo off

if [%1]==[help] GOTO :help else GOTO :SetVars

:SetVars
  set OLDDIR=%CD%
  set UPXPath=%1
  set StripRelocPath=%2
  set GitTag=%3
  set Map2Mab=%4
  goto :Run

:Run
  pushd %temp%\
  if [%GitTag%]==[] goto :GitLastCommit
  if not [%GitTag%]==[] goto :GitTag

:RemoveGitFolder
  cd asuite
  rd /s /q .git
  goto :CompressSource

:CompressSource
  "%programfiles%\7-zip\7z.exe" a -tzip %OLDDIR%\asuitesrc.zip
  "%programfiles%\7-zip\7z.exe" a -t7z %OLDDIR%\asuitesrc.7z
  goto :Build

:Build
  call "%OLDDIR%\..\build_release_asuite.bat"
  cd utils
  %Map2Mab% ..\bin\asuite.exe
  %StripRelocPath% /B ..\bin\asuite.exe
  %UPXPath% --best ..\bin\asuite.exe
  goto :CompressRelease

:CompressRelease
  cd ..\bin
  "%programfiles%\7-zip\7z.exe" a -tzip %OLDDIR%\asuite.zip
  "%programfiles%\7-zip\7z.exe" a -t7z %OLDDIR%\asuite.7z
  goto :RemoveAll

:RemoveAll
  cd ..\..
  rd /s /q asuite
  goto :eof

:GitLastCommit
  git.exe clone --depth 1 https://github.com/salvadorbs/Asuite.git
  goto :RemoveGitFolder

:GitTag
  git.exe clone -b %GitTag% --depth 1 https://github.com/salvadorbs/Asuite.git
  goto :RemoveGitFolder

:help
  echo Syntax:
  echo     %0 UPX StripReloc [Tag]
  echo.
  echo     where 'UPX' is the path to upx.exe,
  echo     `StripReloc` is the path to stripreloc.exe
  echo     and optional 'Tag' is a tag git
