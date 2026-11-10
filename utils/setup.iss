; Inno Setup script for the ASuite Windows installer.
;
; The version is supplied by the build pipeline so it cannot drift from the
; release tag:
;   ISCC.exe /DMyAppVersion=<version> utils\setup.iss

#ifndef MyAppVersion
  #error MyAppVersion is not defined; pass /DMyAppVersion=<version> to ISCC
#endif

#define MyAppName "ASuite"
#define MyAppPublisher "SalvadorSoftware"
#define MyAppURL "https://www.salvadorsoftware.com/asuite"
#define MyAppExeName "ASuite.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{85E14F3B-9C17-4CB6-9F85-8394F1C4078D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile=..\bin\docs\license.txt
OutputDir=..\dist
OutputBaseFilename=asuite-{#MyAppVersion}-setup
SetupIconFile=..\ASuite.ico
UninstallDisplayIcon={app}\{#MyAppExeName}
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[InstallDelete]
; Remove the separate 64-bit executable used by the old dual-arch installer.
Type: files; Name: "{app}\ASuite_x64.exe"

[Files]
; The Windows build is 64-bit only, so there is a single executable.
Source: "..\bin\ASuite.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\docs\*"; DestDir: "{app}\docs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\bin\locale\*"; DestDir: "{app}\locale"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\bin\themes\*"; DestDir: "{app}\themes"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
