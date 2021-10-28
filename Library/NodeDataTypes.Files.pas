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

unit NodeDataTypes.Files;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Kernel.Enumerations, NodeDataTypes.Base, Kernel.Types, Classes,
  NodeDataTypes.Custom, LCLIntf, LCLType, DateUtils, process, LazFileUtils;

type

  { TvFileNodeData }

  TvFileNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    //Paths
    FPathFile        : string;
    //Advanced
    FParameters      : string;
    FWorkingDir      : string;
    FShortcutDesktop : Boolean;
    FClickCount      : Integer;
    FNoMRU           : Boolean;
    FNoMFU           : Boolean;
    FRunFromCategory : Boolean;
    FIsPathFileExists: Boolean;
    FEnvironmentVars : TStringList;

    procedure SetPathFile(value:string);
    procedure SetWorkingDir(value:string);
    procedure SetNoMRU(value:Boolean);
    procedure SetNoMFU(value:Boolean);
    procedure SetShortcutDesktop(value:Boolean);
    function GetPathAbsoluteFile: String;
    function GetWorkingDirAbsolute: string;
    procedure SetClickCount(const Value: Integer);
    function GetWindowState(ARunFromCategory: Boolean): Integer;
    function  IsProcessExists(exeFileName: string): Boolean;
  protected
    procedure SetLastAccess(const Value: Int64); override;
    procedure AfterExecute(ADoActionOnExe: Boolean); override;

    function InternalExecute(ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean; override;
    function InternalExecuteAsUser(ARunFromCategory: Boolean; AUserData: TUserData): boolean; override;
    function InternalExecuteAsAdmin(ARunFromCategory: Boolean): boolean; override;
  public
    //Specific properties
    constructor Create(AType: TvTreeDataType); overload;
    destructor Destroy; override;

    procedure Copy(source: TvBaseNodeData); override;

    function ExplorePath: Boolean;

    property ClickCount: Integer read FClickCount write SetClickCount;
    property NoMRU: Boolean read FNoMRU write SetNoMRU;
    property NoMFU: Boolean read FNoMFU write SetNoMFU;
    property PathFile: String read FPathFile write SetPathFile;
    property PathAbsoluteFile: String read GetPathAbsoluteFile;
    property IsPathFileExists: Boolean read FIsPathFileExists;
    property Parameters: string read FParameters write FParameters;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property WorkingDirAbsolute: string read GetWorkingDirAbsolute;
    property ShortcutDesktop:Boolean read FShortcutDesktop write SetShortcutDesktop;
    property RunFromCategory: Boolean read FRunFromCategory write FRunFromCategory;
    property EnvironmentVars: TStringList read FEnvironmentVars write FEnvironmentVars;

    procedure DeleteShortcutFile;
    procedure CheckPathFile;
  end;
  PvFileNodeData = ^TvFileNodeData;

implementation

uses
  AppConfig.Main, Kernel.Consts, Utility.FileFolder, LazUTF8, Kernel.ResourceStrings,
  Utility.System, VirtualTree.Methods, Utility.Misc, Kernel.Instance, Kernel.Manager
  {$IFDEF Windows}, JwaWindows, JwaWinBase, ShellApi{$ENDIF}, mormot.core.log,
  Kernel.ShellLink;

constructor TvFileNodeData.Create(AType: TvTreeDataType);
begin
  inherited Create(vtdtFile);
  if AType = vtdtFolder then
    Self.PathIcon := CONST_PATH_FOLDERICON;
  //Paths
  FPathFile         := '';
  FIsPathFileExists := True;
  //Advanced
  FParameters      := '';
  FWorkingDir      := '';
  FNoMRU           := False;
  FNoMFU           := False;
  FClickCount      := 0;
  FShortcutDesktop := False;
  //Misc
  FRunFromCategory := False;
  FEnvironmentVars := TStringList.Create;
end;

destructor TvFileNodeData.Destroy;
begin
  inherited Destroy;

  FEnvironmentVars.Clear;
  FEnvironmentVars.Free;
end;

procedure TvFileNodeData.DeleteShortcutFile;
begin
  if (FShortcutDesktop) then
    TShellLinkFile.DeleteShortcutOnDesktop(Self.Name + EXT_LNK);
end;

function TvFileNodeData.GetPathAbsoluteFile: String;
begin
  if FPathFile <> '' then
    Result := ASuiteInstance.Paths.RelativeToAbsolute(FPathFile)
  else
    Result := '';
end;

function TvFileNodeData.GetWorkingDirAbsolute: string;
begin
  if FWorkingDir = '' then
  begin
    if IsDirectory(Self.PathAbsoluteFile) then
      Result := Self.PathAbsoluteFile
    else
      Result := ExtractFileDir(Self.PathAbsoluteFile);
  end
  else
    Result := ASuiteInstance.Paths.RelativeToAbsolute(FWorkingDir);
end;

function TvFileNodeData.InternalExecute(ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean;
var
  Path: String;
begin
  Result := False;

  Path := PathAbsoluteFile;

  //If ACheckSingleInstance, we must check if process exists, and if yes, exit
  if (ACheckSingleInstance) and (Autorun = atSingleInstance) then
    if (IsProcessExists(ExtractFileName(Path))) then
      Exit;

  //For url & documents, use OpenDocument (who uses xdg-open in Linux and ShellExecute in Windows)
  //For executable (EXE, CMD, BAT for Windows, or files with permission Execute for Linux), use CreateProcess
  if IsExecutableFile(Path) then
    Result := CreateProcessEx(PathAbsoluteFile, ASuiteInstance.Paths.RelativeToAbsolute(FParameters, False),
                              WorkingDirAbsolute, GetWindowState(ARunFromCategory),
                              EnvironmentVars) <> -1
  else
    Result := OpenDocument(Path);

  //Error message
  if not Result then
    ShowMessageFmtEx(msgErrorExecute, [Self.Name], True);
end;

function TvFileNodeData.InternalExecuteAsAdmin(ARunFromCategory: Boolean): boolean;
{$IFDEF MSWINDOWS}
var
  ShellExecuteInfo: TShellExecuteInfoW;
{$ENDIF}
begin
  Result := False;

  if not(IsExecutableFile(PathAbsoluteFile)) then
    Exit;

  {$IFDEF MSWINDOWS}
  ZeroMemory(@ShellExecuteInfo, SizeOf(ShellExecuteInfo));
  ShellExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellExecuteInfo.Wnd    := GetDesktopWindow;
  ShellExecuteInfo.fMask  := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  ShellExecuteInfo.lpVerb := PChar('runas');
  //Set process's path, working dir, parameters and window state
  ShellExecuteInfo.lpFile := PChar(PathAbsoluteFile);
  ShellExecuteInfo.lpDirectory := PChar(GetWorkingDirAbsolute);
  if FParameters <> '' then
    ShellExecuteInfo.lpParameters := PChar(ASuiteInstance.Paths.RelativeToAbsolute(FParameters, False));
  ShellExecuteInfo.nShow := GetWindowState(ARunFromCategory);
  //Run process
  Result := ShellExecuteExW(@ShellExecuteInfo);

  if not Result then
    ShowMessageFmtEx(msgErrorExecuteAdmin, [Self.Name], True);
  {$ELSE}
  Result := CreateProcessEx('pkexec', Format('%s %s', [PathAbsoluteFile, Parameters]), WorkingDirAbsolute,
                            GetWindowState(ARunFromCategory), EnvironmentVars) <> -1;
  {$ENDIF}
end;

function TvFileNodeData.InternalExecuteAsUser(ARunFromCategory: Boolean; AUserData: TUserData): boolean;
{$IFDEF MSWINDOWS}
var
  StartupInfo : JwaWinBase.TStartupInfoW;
  ProcInfo    : TProcessInformation;
{$ENDIF}
begin
  Result := False;

  if not(IsExecutableFile(PathAbsoluteFile)) then
    Exit;

  {$IFDEF MSWINDOWS}
  FillMemory(@StartupInfo, sizeof(StartupInfo), 0);
  FillMemory(@ProcInfo, sizeof(ProcInfo), 0);
  StartupInfo.cb := sizeof(TStartupInfoW);
  StartupInfo.wShowWindow := WindowState;
  //Run process as Windows another user
  Result := CreateProcessWithLogonW(PWideChar(AUserData.UserName), nil,
                                    PWideChar(AUserData.Password),
                                    LOGON_WITH_PROFILE,
                                    PWideChar(PathAbsoluteFile), nil,
                                    CREATE_UNICODE_ENVIRONMENT, nil,
                                    PWideChar(GetWorkingDirAbsolute),
                                    StartupInfo, ProcInfo);
  //Close handles
  if Result then
  begin
    FileClose(ProcInfo.hProcess);
    FileClose(ProcInfo.hThread);
  end
  else
    ShowMessageFmtEx(msgErrorExecuteUser, [Self.Name, AUserData.UserName], True);
  {$ELSE}
  Result := CreateProcessEx('pkexec', Format('%s %s %s', [AUserData.UserName, PathAbsoluteFile, Parameters]), WorkingDirAbsolute,
                            GetWindowState(ARunFromCategory), EnvironmentVars) <> -1;
  {$ENDIF}
end;

procedure TvFileNodeData.AfterExecute(ADoActionOnExe: Boolean);
begin
  //MFU
  SetClickCount(FClickCount + 1);
  ASuiteManager.ListManager.MFUList.Sort;
  //MRU
  SetLastAccess(DateTimeToUnix(Now));
  ASuiteManager.ListManager.MRUList.Sort;
  inherited;
end;

procedure TvFileNodeData.CheckPathFile;
var
  bPathExists: Boolean;
begin
  bPathExists := IsPathExists(Self.PathAbsoluteFile);
  if FIsPathFileExists <> bPathExists then
  begin
    FIsPathFileExists := bPathExists;
    if Config.ASuiteState = lsNormal then
      Icon.ResetIcon;
  end;
end;

function TvFileNodeData.GetWindowState(ARunFromCategory: Boolean): Integer;
var
  ParentNodeData: TvCustomRealNodeData;
begin
  Result := SW_SHOWDEFAULT;
  //Window state
  if ARunFromCategory then
  begin
    Assert(PNode.Parent <> ASuiteInstance.MainTree.RootNode, 'Parent''s item = Main tree root node (run from category mode)');

    ParentNodeData := TvCustomRealNodeData(TVirtualTreeMethods.GetNodeItemData(PNode.Parent, ASuiteInstance.MainTree));
    //Override child item's FWindowState
    case ParentNodeData.WindowState of
      1: Result := SW_SHOWDEFAULT;
      2: Result := SW_SHOWMINNOACTIVE;
      3: Result := SW_SHOWMAXIMIZED;
    end;
  end
  else
  begin
    case Self.WindowState of
      1: Result := SW_SHOWMINNOACTIVE;
      2: Result := SW_SHOWMAXIMIZED;
    end;
  end;
end;

function TvFileNodeData.IsProcessExists(exeFileName: string): Boolean;
var
{$IFDEF MSWINDOWS}
  hSnapShot : THandle;
  ProcInfo  : TProcessEntry32;
{$ELSE}
  Process: TProcess;
  Output: TStringList;
  Index: Integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result      := False;
  exeFileName := UpperCase(ExeFileName);
  hSnapShot   := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  //Check processes
  if (hSnapShot <> THandle(-1)) then
  begin
    ProcInfo.dwSize := SizeOf(ProcInfo);
    //First process
    if (Process32First(hSnapshot, ProcInfo)) then
    begin
      //Compare first process with ExeFileName
      if (UpperCase(ExtractFileName(ProcInfo.szExeFile)) = ExeFileName) then
        Result := True;
      while (Process32Next(hSnapShot, ProcInfo)) do
        if (UpperCase(ExtractFileName(ProcInfo.szExeFile)) = ExeFileName) then
          Result := True;
    end;
  end;
  FileClose(hSnapShot);
  {$ELSE}
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'ps';
    Process.Parameters.Add('-C');
    Process.Parameters.Add(ExeFileName);
    Process.Options := [poUsePipes, poWaitOnExit];

    Process.Execute;

    Output := TStringList.Create;
    try
      Output.LoadFromStream(Process.Output);

      repeat
        Index := UTF8Pos(ExeFileName, Output.Text, Index + 1);
        if Index <> 0 then
          Result := True;
      until Index = 0;
    finally
      Output.Free;
    end;
  finally
    Process.Free;
  end;
  {$ENDIF}
end;

procedure TvFileNodeData.Copy(source: TvBaseNodeData);
var
  SourceNodeData: TvFileNodeData;
begin
  inherited;
  if Source is TvFileNodeData then
  begin
    SourceNodeData  := TvFileNodeData(Source);

    //Copy from Source
    Self.PathFile   := SourceNodeData.PathFile;
    Self.Parameters := SourceNodeData.Parameters;
    Self.WorkingDir := SourceNodeData.WorkingDir;
    Self.NoMRU      := SourceNodeData.FNoMRU;
    Self.NoMFU      := SourceNodeData.FNoMFU;
    Self.ShortcutDesktop := SourceNodeData.ShortcutDesktop;
    Self.RunFromCategory := SourceNodeData.FRunFromCategory;
    Self.EnvironmentVars.Assign(SourceNodeData.EnvironmentVars);
  end;
end;

function TvFileNodeData.ExplorePath: Boolean;
begin
  Result := False;
  if Not(IsValidURLProtocol(Self.PathAbsoluteFile)) then
    Result := OpenDocument(ExtractFileDir(Self.PathAbsoluteFile));
end;

procedure TvFileNodeData.SetPathFile(value:string);
begin
  FPathFile := value;
  CheckPathFile;
end;

procedure TvFileNodeData.SetWorkingDir(value:string);
begin
  FWorkingDir := value;
end;

procedure TvFileNodeData.SetNoMRU(value:Boolean);
begin
  if (Config.ASuiteState <> lsImporting) then
  begin
    //If value is true, delete it from list
    if (value and (FNoMRU <> value)) then
      ASuiteManager.ListManager.MRUList.RemoveItem(Self)
    else //else add it in list
      if (not value and (FNoMRU <> value)) and (LastAccess > -1) then
        ASuiteManager.ListManager.MRUList.AddItem(Self);
  end;
  FNoMRU := value;
end;

procedure TvFileNodeData.SetClickCount(const Value: Integer);
begin
  FClickCount := Value;
  if (Config.ASuiteState <> lsImporting) then
    if (FClickCount > 0) and (not Self.FNoMFU) then
      ASuiteManager.ListManager.MFUList.AddItem(Self);
end;

procedure TvFileNodeData.SetLastAccess(const Value: Int64);
begin
  inherited;
  if (Config.ASuiteState <> lsImporting) then
    if (Self.LastAccess > -1) and (not FNoMRU) then
      ASuiteManager.ListManager.MRUList.AddItem(Self);
end;

procedure TvFileNodeData.SetNoMFU(value:Boolean);
begin
  if (Config.ASuiteState <> lsImporting) then
  begin
    //If value is true, delete it from list
    if (value and (FNoMFU <> value)) then
      ASuiteManager.ListManager.MFUList.RemoveItem(Self)
    else //else add it in list
      if (not value and (FNoMFU <> value)) and (FClickCount > 0) then
        ASuiteManager.ListManager.MFUList.AddItem(Self);
  end;
  FNoMFU := value;
end;

procedure TvFileNodeData.SetShortcutDesktop(value:Boolean);
begin
  if (Config.ASuiteState <> lsImporting) then
  begin
    //If value is true, create shortcut in desktop
    if (value and (FShortcutDesktop <> value)) then
      TShellLinkFile.CreateShortcutOnDesktop(Name + EXT_LNK, Self.PathAbsoluteFile, FParameters, Self.WorkingDirAbsolute)
    else //else delete it from desktop
      if (not value and (FShortcutDesktop <> value)) then
        TShellLinkFile.DeleteShortcutOnDesktop(Self.Name + EXT_LNK);
  end;
  FShortcutDesktop := value;
end;

end.
