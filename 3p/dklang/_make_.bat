@echo off
rem ********************************************************************************************************************
rem $Id$
rem --------------------------------------------------------------------------------------------------------------------
rem DKLang Localization Package
rem Copyright 2002-2009 DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
rem ** Making bundle of the package files 

rem --------------------------------------------------------------------------------------------------------------------
rem  Variable declaration
rem --------------------------------------------------------------------------------------------------------------------

set VERSION=4.0RC1

set BASE_DIR=C:\Dev\DKLang
set INSTALL_DIR=%BASE_DIR%\Install
set HELP_DIR=%BASE_DIR%\Help

set ARCHIVE_FILE=%INSTALL_DIR%\dklang-package-%VERSION%.zip
set CHM_FILE=dklang.chm

set HELP_COMPILER=C:\Program Files\HTML Help Workshop\hhc.exe
set CHM_API_MAKER=%HELP_DIR%\ChmDoc.pl
set CHM_API_FILE_PREFIX=__chmdoc__
set ARCHIVER=C:\Progra~1\WinRAR\winrar.exe
set CLEANER=%BASE_DIR%\_cleanup_.bat

rem --------------------------------------------------------------------------------------------------------------------
rem  Let's start here
rem --------------------------------------------------------------------------------------------------------------------

echo [1] Removing old files...
if exist "%ARCHIVE_FILE%" del "%ARCHIVE_FILE%"
if exist "%BASE_DIR%\%CHM_FILE%" del "%BASE_DIR%\%CHM_FILE%"

echo [2] Generating and compiling CHM docs...
cd "%HELP_DIR%"
perl "%CHM_API_MAKER%"
if errorlevel 1 goto err
move "%HELP_DIR%\%CHM_FILE%" "%BASE_DIR%\"
if errorlevel 1 goto err

echo [3] Cleaning up...
call "%CLEANER%"

echo [4] Archiving the files...
cd "%INSTALL_DIR%"
rem -m5    = compression best
rem -afzip = create zip archive
start /w %ARCHIVER% a -m5 -afzip "%ARCHIVE_FILE%" @include_list.txt -x@exclude_list.txt >nul
if errorlevel 1 goto err

goto ok

:err
pause
:ok
