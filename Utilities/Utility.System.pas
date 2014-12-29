{
Copyright (C) 2006-2013 Matteo Salvi

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
  ShlObj, ActiveX, ComObj, Forms, Dialogs, FileCtrl, Vcl.StdCtrls, System.IOUtils;

{ Browse }
function  BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;

{ Check functions }
function HasDriveLetter(const Path: String): Boolean;
function IsAbsolutePath(const Path: String): Boolean;
function IsDirectory(const Path: String): Boolean;
function IsDriveRoot(const Path: String): Boolean;
function IsValidURLProtocol(const URL: string): Boolean;
function IsPathExists(const Path: String): Boolean;
function SHAutoComplete(hwndEdit: HWND; dwFlags: dWord): LongInt; stdcall; external 'shlwapi.dll';
function AutoCompleteInEdit(Edit: TEdit): Boolean;

{ Relative & Absolute path }
function ExpandEnvVars(const Str: string): string;

{ Registry }
procedure SetASuiteAtWindowsStartup;
procedure DeleteASuiteAtWindowsStartup;

{ Misc }
procedure EjectDialog(Sender: TObject);
function ExtractDirectoryName(const Filename: string): string;
function GetCorrectWorkingDir(Default: string): string;
function RegisterHotkeyEx(AId: Integer; AShortcut: TShortCut): Boolean;
function UnRegisterHotkeyEx(AId: Integer): Boolean;
function IsHotkeyAvailable(AId: Integer; AShortcut: TShortcut): Boolean;

const
  SHACF_AUTOAPPEND_FORCE_OFF  = $80000000; // Ignore the registry default and force the feature off. (Also know as AutoComplete)
  SHACF_AUTOAPPEND_FORCE_ON   = $40000000; // Ignore the registry default and force the feature on. (Also know as AutoComplete)
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000; // Ignore the registry default and force the feature off.
  SHACF_AUTOSUGGEST_FORCE_ON  = $10000000; // Ignore the registry default and force the feature on.
  SHACF_DEFAULT = $00000000;    // Currently (SHACF_FILESYSTEM | SHACF_URLALL)
  SHACF_FILESYSTEM = $00000001; // This includes the File System as well as the rest of the shell (Desktop\My Computer\Control Panel\)
  SHACF_URLHISTORY = $00000002; // URLs in the User's History
  SHACF_URLMRU = $00000004;     // URLs in the User's Recently Used list.
  SHACF_USETAB = $00000008;
  SHACF_URLALL = (SHACF_URLHISTORY + SHACF_URLMRU);

implementation

uses
  Utility.Strings, Forms.Main, AppConfig.Main, ulCommonUtils;

function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
begin
  //Set initial directory
  if uMsg = BFFM_INITIALIZED then
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
  Result := 0;
end;

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

function IsAbsolutePath(const Path: String): Boolean;
begin
  if Path = '' then
    Result := False
  else if HasDriveLetter(Path) then
    Result := True
  else if CharInSet(PChar(Pointer(Path))^, ['\', '/']) then
    Result := True else
  Result := False;
end;

function IsDirectory(const Path: String): Boolean;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Path);
  if L = 0 then
    Result := False
  else if (L = 2) and HasDriveLetter(Path) then
    Result := True
  else
    begin
      P := Pointer(Path);
      Inc(P, L - 1);
      Result := CharInSet(P^, SLASHES);
    end;
end;

function IsDriveRoot(const Path: String): Boolean;
begin
  Result := (Length(Path) = 3) and HasDriveLetter(Path) and (Path[3] = PathDelim);
end;

function IsValidURLProtocol(const URL: string): Boolean;
  {Checks if the given URL is valid per RFC1738. Returns True if valid and False
  if not.}
const
  Protocols: array[1..11] of string = (
    // Array of valid protocols - per RFC 1738
    'ftp://', 'http://', 'gopher://', 'mailto:', 'news:', 'nntp://',
    'telnet://', 'wais://', 'file://', 'prospero://', 'https://'
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

function AutoCompleteInEdit(Edit: TEdit): Boolean;
begin
  Result := (SHAutoComplete(Edit.Handle, SHACF_FILESYSTEM or SHACF_AUTOAPPEND_FORCE_ON) = 0);
end;

function ExpandEnvVars(const Str: string): string;
var
  BufSize: Integer; // size of expanded string
begin
  // Get required buffer size
  BufSize := ExpandEnvironmentStrings(PChar(Str), nil, 0);
  if BufSize > 0 then
  begin
    // Read expanded string into result string
    SetLength(Result, BufSize - 1);
    ExpandEnvironmentStrings(PChar(Str), PChar(Result), BufSize);
  end
  else
    // Trying to expand empty string
    Result := '';
end;

procedure EjectDialog(Sender: TObject);
var
  WindowsPath : string;
begin
  //Call "Safe Remove hardware" Dialog
  WindowsPath := GetEnvironmentVariable('WinDir');
  if FileExists(PChar(WindowsPath + '\System32\Rundll32.exe')) then
  begin
    ShellExecute(0,'open',
                 PChar(WindowsPath + '\System32\Rundll32.exe'),
                 PChar('Shell32,Control_RunDLL hotplug.dll'),
                 PChar(WindowsPath + '\System32'),SW_SHOWNORMAL);
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
