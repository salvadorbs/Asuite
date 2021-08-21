{
Copyright (C) 2006-2021 Matteo Salvi

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

unit Kernel.ShellLink;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils;

type

  { TShellLinkFile }

  TShellLinkFile = class
  private
    FLinkPath: String;
    FTargetPath: String;
    FWorkingDir: String;
    FParameters: String;

    procedure ClearProps;
    function GetWorkingDir: String;

    class function GetDesktopDir: String;
    function IsShellLink(var APath: String): Boolean;
  public
    constructor Create; overload;
    constructor Create(APath: String); overload;

    property TargetPath: String read FTargetPath write FTargetPath;
    property WorkingDir: String read GetWorkingDir write FWorkingDir;
    property Parameters: String read FParameters write FParameters;
    property LinkPath: String read FLinkPath write FLinkPath;

    procedure LoadShellLink(APathFile: String);
    function SaveShellLink(APathFile: String): Boolean;

    class procedure CreateShortcutOnDesktop(const FileName, TargetFilePath, Params, WorkingDir: String);
    class procedure DeleteShortcutOnDesktop(const FileName: String);
  end;

implementation

uses
  Kernel.Manager, Kernel.Logger, Utility.FileFolder, Kernel.Consts, LazFileUtils
  {$IFDEF Windows}, ComObj, ActiveX, ShlObj, Windows{$ELSE}, BaseUnix {$ENDIF};

{ TShellLinkFile }

procedure TShellLinkFile.ClearProps;
begin
  FTargetPath := '';
  FWorkingDir := '';
  FParameters := '';
end;

function TShellLinkFile.GetWorkingDir: String;
begin
  if FWorkingDir = '' then
    Result := ExtractFilePath(FWorkingDir)
  else
    Result := FWorkingDir;
end;

class function TShellLinkFile.GetDesktopDir: String;
begin
{$IFDEF MSWINDOWS}
  Result := SHGetFolderPathUTF8(CSIDL_DESKTOPDIRECTORY);
{$ELSE}
  Result := GetUserDir + 'Desktop';
{$ENDIF}
end;

function TShellLinkFile.IsShellLink(var APath: String): Boolean;
{$IFDEF UNIX}
var
  Info : Stat;
{$ENDIF}
begin
  Result := False;

  if not FileExists(APath) then
    Exit;

  {$IFDEF MSWINDOWS}
  Result := (ExtractLowerFileExt(APath) = EXT_LNK);
  {$ELSE}
  Result := (fplstat(APath, Info)>=0) and fpS_ISLNK(Info.st_mode);
  {$ENDIF}
end;

constructor TShellLinkFile.Create;
begin
  ClearProps;
end;

constructor TShellLinkFile.Create(APath: String);
begin
  inherited Create;

  LoadShellLink(APath);
end;

procedure TShellLinkFile.LoadShellLink(APathFile: String);
{$IFDEF MSWINDOWS}
var
  Obj: IUnknown;
  ISLink: IShellLinkW;
  PersistFile: IPersistFile;
  Buffer: array[0..MAX_PATH] of Char;
  wfs: WIN32_FIND_DATAW;

  procedure FillBuffer;
  begin
    FillChar(Buffer, SizeOf(Buffer), 0);
  end;
{$ENDIF}
begin
  ClearProps;

  if not IsShellLink(APathFile) then
    Exit;

  FLinkPath := APathFile;

{$IFDEF MSWINDOWS}
  CoInitialize(nil);
  Obj := CreateComObject(CLSID_ShellLink);
  try
    ISLink := Obj as IShellLinkW;
    PersistFile := Obj as IPersistFile;

    if PersistFile.Load(PChar(APathFile), STGM_READ) <> S_OK then
      Exit;


    FillBuffer;
    if ISLink.GetPath(Buffer, SizeOf(Buffer), @wfs, SLGP_UNCPRIORITY) = S_OK then
      Self.TargetPath := Buffer;

    FillBuffer;
    if ISLink.GetArguments(Buffer, SizeOf(Buffer)) = S_OK then
      Self.Parameters := Buffer;

    FillBuffer;
    if ISLink.GetWorkingDirectory(Buffer, SizeOf(Buffer)) = S_OK then
      Self.WorkingDir := Buffer;
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
  end;
{$ELSE}
  try
    Self.LinkPath := ReadAllLinks(APathFile, False);
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
  end;
{$ENDIF}
end;

function TShellLinkFile.SaveShellLink(APathFile: String): Boolean;
{$IFDEF MSWINDOWS}
var
  Obj: IUnknown;
  ShellLink: IShellLinkW;
  PersistFile: IPersistFile;
{$ENDIF}
begin
  Result := False;

  if Self.TargetPath = '' then
    Exit;

  FLinkPath := APathFile;

{$IFDEF MSWINDOWS}
  CoInitialize(nil);
  Obj := CreateComObject(CLSID_ShellLink);
  try
    ShellLink := Obj as IShellLinkW;
    PersistFile := Obj as IPersistFile;

    ShellLink.SetPath(PChar(Self.TargetPath));
    ShellLink.SetArguments(PChar(Self.Parameters));
    ShellLink.SetWorkingDirectory(PChar(Self.WorkingDir));

    PersistFile.Save(PChar(APathFile), False);
  finally
    Result := True;
  end;
{$ELSE}
  try
    fpSymlink(PAnsiChar(Self.LinkPath), PAnsiChar(APathFile));
  finally
    Result := True;
  end;
{$ENDIF}
end;

class procedure TShellLinkFile.CreateShortcutOnDesktop(const FileName,
  TargetFilePath, Params, WorkingDir: String);
var
  ShellLink: TShellLinkFile;
begin
  ShellLink := TShellLinkFile.Create;
  try
    ShellLink.TargetPath := TargetFilePath;
    ShellLink.Parameters := Params;
    ShellLink.WorkingDir := WorkingDir;

    ShellLink.SaveShellLink(AppendPathDelim(TShellLinkFile.GetDesktopDir) + FileName);
  finally
    ShellLink.Free;
  end;
end;

class procedure TShellLinkFile.DeleteShortcutOnDesktop(const FileName: String);
var
  LinkName: String;
begin
  LinkName := AppendPathDelim(TShellLinkFile.GetDesktopDir) + FileName;
  if (FileExists(LinkName)) then
    SysUtils.DeleteFile(LinkName);
end;

end.
