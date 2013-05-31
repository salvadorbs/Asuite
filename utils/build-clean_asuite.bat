call "%ProgramFiles%\Embarcadero\RAD Studio\11.0\bin\rsvars.bat"

msbuild.exe ..\ASuite.dproj /t:%1 /p:config="Release Build"

pause