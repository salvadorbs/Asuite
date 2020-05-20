cd ..
call clean.bat
call .\utils\Run-Dependend-rsvars-From-Path.bat L rsvars
msbuild /nologo /t:rebuild /verbosity:quiet /p:Platform=Win32 /p:config=Debug ASuite.dproj

cd utils