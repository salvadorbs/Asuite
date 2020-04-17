call clean.bat
call .\utils\Run-Dependend-rsvars-From-Path.bat L rsvars
msbuild /nologo /t:rebuild /verbosity:quiet /p:Platform=Win64 /p:config=Release ASuite.dproj