@SET BDS=C:\Program Files\Embarcadero\RAD Studio\10.0
@SET BDSINCLUDE=C:\Program Files\Embarcadero\RAD Studio\10.0\include
@SET BDSCOMMONDIR=C:\Users\Public\Documents\RAD Studio\10.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET FrameworkSDKDir=
@SET PATH=%FrameworkDir%;%FrameworkSDKDir%;C:\Program Files\Embarcadero\RAD Studio\10.0\bin;C:\Program Files\Embarcadero\RAD Studio\10.0\bin64;%PATH%
@SET LANGDIR=EN

msbuild.exe ..\ASuite.dproj /t:%1 /p:config="Release Build"

pause