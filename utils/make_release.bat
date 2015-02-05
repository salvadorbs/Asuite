set OLDDIR=%CD%

pushd %temp%\

git.exe clone https://github.com/salvadorbs/Asuite.git

cd asuite

rd /s /q .git

"%programfiles%\7-zip\7z.exe" a -tzip %OLDDIR%\asuitesrc.zip
"%programfiles%\7-zip\7z.exe" a -t7z %OLDDIR%\asuitesrc.7z

call "%OLDDIR%\..\build_release_asuite.bat"

cd bin

"%programfiles%\7-zip\7z.exe" a -tzip %OLDDIR%\asuite.zip
"%programfiles%\7-zip\7z.exe" a -t7z %OLDDIR%\asuite.7z

pause

cd ..

rd /s /q asuite

pause
