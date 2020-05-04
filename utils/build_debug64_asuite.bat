call ..\clean.bat
call .\Run-Dependend-rsvars-From-Path.bat L rsvars
msbuild /nologo /t:rebuild /verbosity:quiet /p:Platform=Win64 /p:config=Debug ASuite.dproj