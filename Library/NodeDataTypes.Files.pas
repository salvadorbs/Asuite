{
Copyright (C) 2006-2015 Matteo Salvi

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

interface

uses
  SysUtils, Kernel.Enumerations, NodeDataTypes.Base, Kernel.Types,
  NodeDataTypes.Custom, WinApi.Windows, WinApi.ShellApi, DateUtils;

type
  TvFileNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    //Paths
    FPathFile         : string;
    //Advanced
    FParameters      : string;
    FWorkingDir      : string;
    FShortcutDesktop : Boolean;
    FLastAccess      : Int64;
    FClickCount      : Integer;
    FNoMRU           : Boolean;
    FNoMFU           : Boolean;
    FRunFromCategory : Boolean;
    FIsPathFileExists: Boolean;
    procedure SetPathFile(value:string);
    procedure SetWorkingDir(value:string);
    procedure SetNoMRU(value:Boolean);
    procedure SetNoMFU(value:Boolean);
    procedure SetShortcutDesktop(value:Boolean);
    function GetPathAbsoluteFile: String;
    function GetWorkingDirAbsolute: string;
    procedure SetClickCount(const Value: Integer);
    procedure SetLastAccess(const Value: Int64);
    function GetWorkingDir(): string;
    function GetWindowState(ARunFromCategory: Boolean): Integer;
  protected
    procedure AfterExecute(ADoActionOnExe: Boolean); override;
    function InternalExecute(ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean; override;
    function InternalExecuteAsUser(ARunFromCategory: Boolean; AUserData: TUserData): boolean; override;
    function InternalExecuteAsAdmin(ARunFromCategory: Boolean): boolean; override;
  public
    //Specific properties
    constructor Create(AType: TvTreeDataType); overload;

    procedure Copy(source: TvBaseNodeData); override;

    function ExplorePath: Boolean;

    property ClickCount: Integer read FClickCount write SetClickCount;
    property LastAccess: Int64 read FLastAccess write SetLastAccess;
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

    procedure DeleteShortcutFile;
    procedure CheckPathFile;
  end;
  PvFileNodeData = ^TvFileNodeData;

implementation

uses
  AppConfig.Main, Lists.Manager, Kernel.Consts, Utility.FileFolder, Lists.Special,
  Lists.Base, Utility.System, Utility.Process, VirtualTree.Methods, Utility.Misc;

constructor TvFileNodeData.Create(AType: TvTreeDataType);
begin
  inherited Create(vtdtFile);
  if AType = vtdtFolder then
    Self.PathIcon := CONST_PATH_FOLDERICON;
  //Paths
  FPathFile         := '';
  FIsPathFileExists := False;
  //Advanced
  FParameters      := '';
  FWorkingDir      := '';
  FNoMRU           := False;
  FNoMFU           := False;
  FLastAccess      := -1;
  FClickCount      := 0;
  FShortcutDesktop := False;
  //Misc
  FRunFromCategory := False;
end;

procedure TvFileNodeData.DeleteShortcutFile;
begin
  if (FShortcutDesktop) then
    DeleteShortcutOnDesktop(Self.Name + EXT_LNK);
end;

function TvFileNodeData.GetPathAbsoluteFile: String;
begin
  if FPathFile <> '' then
    Result := Config.Paths.RelativeToAbsolute(FPathFile)
  else
    Result := '';
end;

function TvFileNodeData.GetWorkingDirAbsolute: string;
begin
  if FWorkingDir <> '' then
    Result := Config.Paths.RelativeToAbsolute(FWorkingDir)
  else
    Result := '';
end;

function TvFileNodeData.InternalExecute(ARunFromCategory: Boolean; ACheckSingleInstance: Boolean): boolean;
begin
  Result := False;

  //If ACheckSingleInstance, we must check if process exists, and if yes, exit
  if (ACheckSingleInstance) and (Autorun = atSingleInstance) then
    if (IsProcessExists(ExtractFileName(Self.PathAbsoluteFile))) then
      Exit;

  //Execute
  Result := ShellExecute(GetDesktopWindow, nil, PChar(PathAbsoluteFile),
                         PChar(Config.Paths.RelativeToAbsolute(FParameters)),
                         PChar(GetWorkingDir), GetWindowState(ARunFromCategory)) > 32;
  //Error message
  if not Result then
    ShowMessageEx(Format('%s [%s]', [SysErrorMessage(GetLastError), Self.Name]), True);
end;

function TvFileNodeData.InternalExecuteAsAdmin(ARunFromCategory: Boolean): boolean;
var
  ShellExecuteInfo: TShellExecuteInfo;
begin
  ZeroMemory(@ShellExecuteInfo, SizeOf(ShellExecuteInfo));
  ShellExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellExecuteInfo.Wnd    := GetDesktopWindow;
  ShellExecuteInfo.fMask  := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  ShellExecuteInfo.lpVerb := PChar('runas');
  //Set process's path, working dir, parameters and window state
  ShellExecuteInfo.lpFile := PChar(PathAbsoluteFile);
  ShellExecuteInfo.lpDirectory := PChar(GetWorkingDir);
  if FParameters <> '' then
    ShellExecuteInfo.lpParameters := PChar(Config.Paths.RelativeToAbsolute(FParameters));
  ShellExecuteInfo.nShow := GetWindowState(ARunFromCategory);
  //Run process
  Result := ShellExecuteEx(@ShellExecuteInfo);

  if not Result then
    ShowMessageEx(SysErrorMessage(GetLastError), True);
end;

function TvFileNodeData.InternalExecuteAsUser(ARunFromCategory: Boolean; AUserData: TUserData): boolean;
var
  StartupInfo : TStartupInfoW;
  ProcInfo    : TProcessInformation;
begin
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
                                    PWideChar(GetWorkingDir),
                                    StartupInfo, ProcInfo);
  //Close handles
  if Result then
  begin
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
  end
  else
    ShowMessageEx(SysErrorMessage(GetLastError), True);
end;

procedure TvFileNodeData.AfterExecute(ADoActionOnExe: Boolean);
begin
  //MFU
  SetClickCount(FClickCount + 1);
  Config.ListManager.MFUList.Sort;
  //MRU
  SetLastAccess(DateTimeToUnix(Now));
  Config.ListManager.MRUList.Sort;
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
    Assert(PNode.Parent <> Config.MainTree.RootNode, 'Parent''s item = Main tree root node (run from category mode)');

    ParentNodeData := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(PNode.Parent, Config.MainTree));
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

function TvFileNodeData.GetWorkingDir(): string;
begin
  //Working directory
  if FWorkingDir = '' then
  begin
    if IsDirectory(Self.PathAbsoluteFile) then
      Result := Self.PathAbsoluteFile
    else
      Result := ExtractFileDir(Self.PathAbsoluteFile);
  end
  else
    Result := Self.WorkingDirAbsolute;
end;

procedure TvFileNodeData.Copy(Source: TvBaseNodeData);
var
  SourceNodeData: TvFileNodeData;
begin
  inherited;
  if Source is TvFileNodeData then
  begin
    SourceNodeData   := TvFileNodeData(Source);
    //Copy from Source
    SetPathFile(SourceNodeData.PathFile);
    FParameters      := SourceNodeData.Parameters;
    FWorkingDir      := SourceNodeData.WorkingDir;
    FNoMRU           := SourceNodeData.FNoMRU;
    FNoMFU           := SourceNodeData.FNoMFU;
    SetShortcutDesktop(SourceNodeData.ShortcutDesktop);
    FRunFromCategory := SourceNodeData.FRunFromCategory;
  end;
end;

function TvFileNodeData.ExplorePath: Boolean;
begin
  Result := False;
  if Not(IsValidURLProtocol(Self.PathAbsoluteFile)) then
    Result := ShellExecute(GetDesktopWindow, 'open',
                           PChar(ExtractFileDir(Self.PathAbsoluteFile)),
                           nil, nil, SW_NORMAL) > 32;
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
      Config.ListManager.MRUList.RemoveItem(Self)
    else //else add it in list
      if (not value and (FNoMRU <> value)) and (LastAccess > -1) then
        Config.ListManager.MRUList.AddItem(Self);
  end;
  FNoMRU := value;
end;

procedure TvFileNodeData.SetClickCount(const Value: Integer);
begin
  FClickCount := Value;
  if (Config.ASuiteState <> lsImporting) then
    if (FClickCount > 0) and (not Self.FNoMFU) then
      Config.ListManager.MFUList.AddItem(Self);
end;

procedure TvFileNodeData.SetLastAccess(const Value: Int64);
begin
  FLastAccess := Value;
  if (Config.ASuiteState <> lsImporting) then
    if (FLastAccess > -1) and (not Self.NoMRU) then
      Config.ListManager.MRUList.AddItem(Self);
end;

procedure TvFileNodeData.SetNoMFU(value:Boolean);
begin
  if (Config.ASuiteState <> lsImporting) then
  begin
    //If value is true, delete it from list
    if (value and (FNoMFU <> value)) then
      Config.ListManager.MFUList.RemoveItem(Self)
    else //else add it in list
      if (not value and (FNoMFU <> value)) and (FClickCount > 0) then
        Config.ListManager.MFUList.AddItem(Self);
  end;
  FNoMFU := value;
end;

procedure TvFileNodeData.SetShortcutDesktop(value:Boolean);
begin
  if (Config.ASuiteState <> lsImporting) then
  begin
    //If value is true, create shortcut in desktop
    if (value and (FShortcutDesktop <> value)) then
      CreateShortcutOnDesktop(Name + EXT_LNK, Self.PathAbsoluteFile, FParameters, Self.WorkingDirAbsolute)
    else //else delete it from desktop
      if (not value and (FShortcutDesktop <> value)) then
        DeleteShortcutOnDesktop(Self.Name + EXT_LNK);
  end;
  FShortcutDesktop := value;
end;

end.
