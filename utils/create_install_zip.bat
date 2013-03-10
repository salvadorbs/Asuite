@SET BDS=%programfiles%\Embarcadero\RAD Studio\8.0
@SET BDSCOMMONDIR=%ALLUSERSPROFILE%\Documenti\RAD Studio\8.0
@SET FrameworkDir=%SYSTEMROOT%\Microsoft.NET\Framework\
@SET FrameworkVersion=v2.0.50727
@SET FrameworkSDKDir=

@ECHO Setting environment for using CodeGear RAD Studio tools

@SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%PATH%

msbuild.exe ..\ASuite.dproj /t:clean /p:configuration=release
msbuild.exe ..\ASuite.dproj /t:build /p:configuration=release

mkdir Release\

cd Release\

mkdir Icons\
mkdir Docs\

copy ..\bin\asuite.exe ..\..\bin\asuite.exe.old
E:\Programmi\UPX\upx.exe --best ..\..\bin\asuite.exe

copy ..\..\bin\asuite.exe
copy ..\..\bin\sqlite3.dll

copy ..\..\bin\Icons\asuite.cur Icons\
copy ..\..\bin\Icons\0.ico Icons\
copy ..\..\bin\Icons\1.ico Icons\
copy ..\..\bin\Icons\10.ico Icons\
copy ..\..\bin\Icons\11.ico Icons\
copy ..\..\bin\Icons\12.ico Icons\
copy ..\..\bin\Icons\13.ico Icons\
copy ..\..\bin\Icons\14.ico Icons\
copy ..\..\bin\Icons\15.ico Icons\
copy ..\..\bin\Icons\16.ico Icons\
copy ..\..\bin\Icons\17.ico Icons\
copy ..\..\bin\Icons\18.ico Icons\
copy ..\..\bin\Icons\19.ico Icons\
copy ..\..\bin\Icons\20.ico Icons\
copy ..\..\bin\Icons\21.ico Icons\
copy ..\..\bin\Icons\22.ico Icons\
copy ..\..\bin\Icons\2.ico Icons\
copy ..\..\bin\Icons\3.ico Icons\
copy ..\..\bin\Icons\4.ico Icons\
copy ..\..\bin\Icons\5.ico Icons\
copy ..\..\bin\Icons\6.ico Icons\
copy ..\..\bin\Icons\7.ico Icons\
copy ..\..\bin\Icons\8.ico Icons\
copy ..\..\bin\Icons\9.ico Icons\

copy ..\..\bin\Docs\license.txt Docs
copy "..\..\bin\Docs\SalvadorSoftware Site.url" Docs
copy "..\..\bin\Docs\Project ASuite.url" Docs

"%programfiles%\7-zip\7z.exe" a -tzip ..\asuite20a1.zip

"%programfiles%\7-zip\7z.exe" a -t7z ..\asuite20a1.7z

cd ..

"%programfiles%\NSIS\makensis.exe" /V2 /X"SetCompressor /FINAL /SOLID lzma" "install_script.nsi"

rd /s /q release\

pause