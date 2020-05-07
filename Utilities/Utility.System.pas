{
Copyright (C) 2006-2020 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Utility.System;

interface

uses
  Kernel.Consts, Windows, ShellApi, SysUtils, Classes, Registry, StrUtils,
  ShlObj, ActiveX, ComObj, Forms, Dialogs, System.IOUtils;

{ Check functions }
function HasDriveLetter(const Path: String): Boolean;
function IsDriveRoot(const Path: String): Boolean;
function IsValidURLProtocol(const URL: string): Boolean;
function IsPathExists(const Path: String): Boolean;

{ Registry }
procedure SetASuiteAtWindowsStartup;
procedure DeleteASuiteAtWindowsStartup;

{ Misc }
function DarkModeIsEnabled: boolean;
procedure EjectDialog(Sender: TObject);
function ExtractDirectoryName(const Filename: string): string;
function GetCorrectWorkingDir(Default: string): string;
function RegisterHotkeyEx(AId: Integer; AShortcut: TShortCut): Boolean;
function UnRegisterHotkeyEx(AId: Integer): Boolean;
function IsHotkeyAvailable(AId: Integer; AShortcut: TShortcut): Boolean;

implementation

uses
  Utility.Conversions, Forms.Main, AppConfig.Main, Utility.Misc, Kernel.Logger;

function HasDriveLetter(const Path: String): Boolean;
var P: PChar;
begin
  if Length(Path) < 2 then
    Exit(False);
  P := Pointer(Path);
  if not CharInSet(P^, DriveLetters) then
    Exit(False);
  Inc(P);
  if not CharInSet(P^, [':']) then
    Exit(False);
  Result := True;
end;

function IsDriveRoot(const Path: String): Boolean;
begin
  Result := (Length(Path) = 3) and HasDriveLetter(Path) and (Path[3] = PathDelim);
end;

function IsValidURLProtocol(const URL: string): Boolean;
  {Checks if the given URL is valid per RFC1738. Returns True if valid and False
  if not.}
const
  Protocols: array[1..12] of string = (
    // Array of valid protocols - per RFC 1738
    'ftp://', 'http://', 'gopher://', 'mailto:', 'news:', 'nntp://',
    'telnet://', 'wais://', 'file://', 'prospero://', 'https://', 'steam://'
  );
var
  I: Integer;   // loops thru known protocols
begin
  // Scan array of protocols checking for a match with start of given URL
  Result := False;
  for I := Low(Protocols) to High(Protocols) do
    if Pos(Protocols[I], SysUtils.LowerCase(URL)) = 1 then
    begin
      Result := True;
      Exit;
    end;
end;

function IsPathExists(const Path: String): Boolean;
var
  PathTemp : String;
begin
  PathTemp := Config.Paths.RelativeToAbsolute(Path);
  if TPath.IsUNCPath(PathTemp) then
    Result := True
  else
    if IsValidURLProtocol(PathTemp) then
      Result := True
    else
      Result := (FileExists(PathTemp)) or (SysUtils.DirectoryExists(PathTemp));
end;

function DarkModeIsEnabled: boolean;
const
  TheKey   = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  TheValue = 'AppsUseLightTheme';
var
  Reg: TRegistry;
begin
  Result := False;
  Reg    := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.KeyExists(TheKey) then
      if Reg.OpenKey(TheKey, False) then
      try
        if Reg.ValueExists(TheValue) then
          Result := Reg.ReadInteger(TheValue) = 0;
      finally
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
  end;
end;

procedure EjectDialog(Sender: TObject);
var
  WindowsPath : string;
  bShellExecute: Boolean;
begin
  //Call "Safe Remove hardware" Dialog
  WindowsPath := GetEnvironmentVariable('WinDir');
  if FileExists(PChar(WindowsPath + '\System32\Rundll32.exe')) then
  begin
    TASuiteLogger.Info('Call Eject Dialog', []);
    bShellExecute := ShellExecute(0,'open',
                     PChar(WindowsPath + '\System32\Rundll32.exe'),
                     PChar('Shell32,Control_RunDLL hotplug.dll'),
                     PChar(WindowsPath + '\System32'),SW_SHOWNORMAL) > 32;
    //Error message
    if not bShellExecute then
      ShowMessageEx(Format('%s [%s]', [SysErrorMessage(GetLastError), 'Rundll32']), True);
  end;
  //Close ASuite
  frmMain.miExitClick(Sender);
end;

function ExtractDirectoryName(const Filename: string): string;
var
  AList : TStringList;
begin
  AList := TStringList.create;
  try
    StrToStrings(Filename,PathDelim,AList);
    if AList.Count > 1 then
      Result := AList[AList.Count - 1]
    else
      Result := '';
  finally
    AList.Free;
  end;
end;

procedure SetASuiteAtWindowsStartup;
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run',False) then
        if Not(ValueExists(APP_NAME)) then
          WriteString(APP_NAME,(Application.ExeName));
    end
  finally
    Registry.Free;
  end;
end;

procedure DeleteASuiteAtWindowsStartup;
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  try
    with Registry do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run',False) then
        DeleteValue(APP_NAME)
    end
  finally
    Registry.Free;
  end;
end;

function GetCorrectWorkingDir(Default: string): string;
var
  sPath: String;
begin
  Result := Default;
  sPath := IncludeTrailingBackslash(Config.Paths.SuiteDrive + sPath);
  if SysUtils.DirectoryExists(sPath) then
    Result := sPath;
end;

function RegisterHotkeyEx(AId: Integer; AShortcut: TShortCut): Boolean;
begin
  Result := RegisterHotKey(frmMain.Handle, AId,
                           GetHotKeyMod(AShortcut),
                           GetHotKeyCode(AShortcut))
end;

function UnRegisterHotkeyEx(AId: Integer): Boolean;
begin
  Result := UnregisterHotKey(frmMain.Handle, AId);
end;

function IsHotkeyAvailable(AId: Integer; AShortcut: TShortcut): Boolean;
begin
  Result := RegisterHotkeyEx(AId, AShortcut);
  if Result then
    UnregisterHotKeyEx(AShortcut);
end;

end.
