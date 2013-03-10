@SET BDS=%programfiles%\Embarcadero\RAD Studio\8.0
@SET BDSCOMMONDIR=%ALLUSERSPROFILE%\Documenti\RAD Studio\8.0
@SET FrameworkDir=%SYSTEMROOT%\Microsoft.NET\Framework\
@SET FrameworkVersion=v2.0.50727
@SET FrameworkSDKDir=

@ECHO Setting environment for using CodeGear RAD Studio tools

@SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%PATH%

msbuild.exe ..\ASuite.dproj /t:%1 /p:configuration=release

pause