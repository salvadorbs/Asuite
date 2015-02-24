::@echo off

if [%1]==[help] GOTO :help else GOTO :SetVars

:SetVars
  set OLDDIR=%CD%
  set UPXPath=%1
  set StripRelocPath=%2
  set Map2Mab=%3
  set GitTag=%4
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
  %Map2Mab% bin\asuite.exe
  %StripRelocPath% /B bin\asuite.exe
  %UPXPath% --best bin\asuite.exe
  pause
  goto :CompressRelease

:Build64
  call "%OLDDIR%\..\build_release64_asuite.bat"
  %Map2Mab% bin\asuite.exe
  %UPXPath% --best bin\asuite.exe
  pause
  goto :CompressRelease64

:CompressRelease
  cd bin
  "%programfiles%\7-zip\7z.exe" a -tzip %OLDDIR%\asuite.zip * -x!sqlite3-64.dll
  "%programfiles%\7-zip\7z.exe" a -t7z %OLDDIR%\asuite.7z * -x!sqlite3-64.dll
  cd ..
  goto :Build64

:CompressRelease64
  cd bin
  "%programfiles%\7-zip\7z.exe" a -tzip %OLDDIR%\asuite_64.zip
  "%programfiles%\7-zip\7z.exe" a -t7z %OLDDIR%\asuite_64.7z
  cd ..
  goto :RemoveAll

:RemoveAll
  cd ..
  rd /s /q asuite
  goto :eof

:GitLastCommit
  git.exe clone --depth 1 https://github.com/salvadorbs/Asuite.git
  pause
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
