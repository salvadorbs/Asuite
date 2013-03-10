cd ..\3p\Virtual Treeview
"%programfiles%\7-zip\7z.exe" a -t7z ..\virtualtreesrc.7z
cd ..\..\utils

mkdir Releasesrc\
mkdir Releasesrc\3p\
mkdir Releasesrc\bin\
mkdir Releasesrc\dcu\
mkdir Releasesrc\data\
mkdir Releasesrc\library\
mkdir Releasesrc\plugin\
mkdir Releasesrc\plugin\Update\
mkdir Releasesrc\utils\

cd Releasesrc\bin\

mkdir Icons\
mkdir Docs\

copy ..\..\..\bin\sqlite3.dll

copy ..\..\..\bin\Icons\asuite.cur Icons\
copy ..\..\..\bin\Icons\0.ico Icons\
copy ..\..\..\bin\Icons\1.ico Icons\
copy ..\..\..\bin\Icons\10.ico Icons\
copy ..\..\..\bin\Icons\11.ico Icons\
copy ..\..\..\bin\Icons\12.ico Icons\
copy ..\..\..\bin\Icons\13.ico Icons\
copy ..\..\..\bin\Icons\14.ico Icons\
copy ..\..\..\bin\Icons\15.ico Icons\
copy ..\..\..\bin\Icons\16.ico Icons\
copy ..\..\..\bin\Icons\17.ico Icons\
copy ..\..\..\bin\Icons\18.ico Icons\
copy ..\..\..\bin\Icons\19.ico Icons\
copy ..\..\..\bin\Icons\20.ico Icons\
copy ..\..\..\bin\Icons\21.ico Icons\
copy ..\..\..\bin\Icons\22.ico Icons\
copy ..\..\..\bin\Icons\2.ico Icons\
copy ..\..\..\bin\Icons\3.ico Icons\
copy ..\..\..\bin\Icons\4.ico Icons\
copy ..\..\..\bin\Icons\5.ico Icons\
copy ..\..\..\bin\Icons\6.ico Icons\
copy ..\..\..\bin\Icons\7.ico Icons\
copy ..\..\..\bin\Icons\8.ico Icons\
copy ..\..\..\bin\Icons\9.ico Icons\

copy ..\..\..\bin\Docs\license.txt Docs
copy "..\..\..\bin\Docs\SalvadorSoftware Site.url" Docs
copy "..\..\..\bin\Docs\Project ASuite.url" Docs

cd ..
cd 3p\
copy ..\..\..\3p\CheckPrevious.pas
copy ..\..\..\3p\DCPbase64.pas
copy ..\..\..\3p\gnugettext.pas
copy ..\..\..\3p\SQLite3.pas
copy ..\..\..\3p\SQLite3Dyn.pas
copy ..\..\..\3p\sqlite3udf.pas
copy ..\..\..\3p\SQLiteTable3.pas
copy ..\..\..\3p\VirtualTreeHDrop.pas
copy ..\..\..\3p\virtualtreesrc.7z
del ..\..\..\3p\virtualtreesrc.7z

cd ..
cd data\
copy ..\..\..\data\ulDatabase.pas

cd ..
cd library\
copy ..\..\..\library\ASuiteForm.pas
copy ..\..\..\library\ulAppConfig.pas
copy ..\..\..\library\ulCommonClasses.pas
copy ..\..\..\library\ulCommonUtils.pas
copy ..\..\..\library\ulEnumerations.pas
copy ..\..\..\library\ulExeUtils.pas
copy ..\..\..\library\ulNodeDataTypes.pas
copy ..\..\..\library\ulSQLite.pas
copy ..\..\..\library\ulStringUtils.pas
copy ..\..\..\library\ulSysUtils.pas
copy ..\..\..\library\ulTreeView.pas

cd ..
cd plugin\Update\
copy ..\..\..\..\plugin\Update\update.ini
copy ..\..\..\..\plugin\Update\Updater.dpr
copy ..\..\..\..\plugin\Update\Updater.dproj
copy ..\..\..\..\plugin\Update\Updater.drc

cd ..\..
cd utils\
copy ..\..\..\utils\install_script.nsi
copy ..\..\..\utils\build-clean_asuite.bat
copy ..\..\..\utils\create_install_zip.bat
copy ..\..\..\utils\create_srczip.bat

cd ..
copy ..\..\About.dfm
copy ..\..\About.pas
copy ..\..\AppConfig.pas
copy ..\..\ASuite.dpr
copy ..\..\ASuite.dproj
copy ..\..\ASuite.drc
copy ..\..\ASuite.inc
copy ..\..\ASuite.res
copy ..\..\ClearElements.dfm
copy ..\..\ClearElements.pas
copy ..\..\ignore.po
copy ..\..\ImportList.dfm
copy ..\..\ImportList.pas
copy ..\..\ListComponentsUsed.txt
copy ..\..\Main.dfm
copy ..\..\Main.pas
copy ..\..\Option.dfm
copy ..\..\Option.pas
copy ..\..\OrderSoftware.dfm
copy ..\..\OrderSoftware.pas
copy ..\..\PropertyCat.dfm
copy ..\..\PropertyCat.pas
copy ..\..\PropertyFile.dfm
copy ..\..\PropertyFile.pas
copy ..\..\PropertySeparator.dfm
copy ..\..\PropertySeparator.pas
copy ..\..\udClassicMenu.dfm
copy ..\..\udClassicMenu.pas
copy ..\..\udImages.dfm
copy ..\..\udImages.pas

"%programfiles%\7-zip\7z.exe" a -tzip ..\asuite20src.zip
"%programfiles%\7-zip\7z.exe" a -t7z ..\asuite20src.7z

cd ..

rd /s /q releasesrc\

pause