@echo off

if [%1]==[help] GOTO :help else GOTO :SetVars

:SetVars
  call set_paths.bat
  set OLDDIR=%CD%
  set GitTag=%1
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
  %c7z% a -tzip %OLDDIR%\asuitesrc.zip
  %c7z% a -t7z %OLDDIR%\asuitesrc.7z
  goto :Build64

:Build
  call "%OLDDIR%\build_release_asuite.bat"
  %Map2Mab% bin\asuite.exe
  goto :CompressRelease

:Build64
  call "%OLDDIR%\build_release64_asuite.bat"
  ren "bin\asuite.exe" "asuite_x64.exe"
  %Map2Mab% bin\asuite_x64.exe
  goto :Build

:CompressRelease
  cd bin
  %c7z% a -tzip %OLDDIR%\asuite.zip * -x!asuite_x64.exe
  %c7z% a -t7z %OLDDIR%\asuite.7z * -x!asuite_x64.exe
  cd ..
  goto :CompressRelease64

:CompressRelease64
  cd bin
  %c7z% a -tzip %OLDDIR%\asuite_x64.zip -x!asuite.exe
  %c7z% a -t7z %OLDDIR%\asuite_x64.7z -x!asuite.exe
  cd ..
  goto :CompileSetup

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

:CompileSetup
  %ISCompiler% /cc utils/setup.iss
  move utils\asuite_setup.exe %OLDDIR%
  goto :RemoveAll

:help
  echo Syntax:
  echo     %0 [Tag]
  echo.
  echo     where optional 'Tag' is a tag git 
  echo
  echo     P.S.: Remember to change set_paths.bat
  echo     before run this batch
