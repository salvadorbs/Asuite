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

 (*
 TvBaseNodeData
              +->TvSeparatorNodeData
              +->TvCustomRealNodeData
                                +->TvCategoryData
                                +->TvFileNodeData
                                                +->TvFolderNodeData
 *)

unit ulNodeDataTypes;

interface

uses
  VirtualTrees, Menus, SysUtils, Dialogs, appConfig, DateUtils, ulEnumerations,
  Winapi.ShellAPI, Winapi.Windows, BaseEntityPage;

type

  TProcessInfo = record
    RunMode     : TRunMode;
    RunFromCat  : Boolean;
    //Misc
    PathExe     : string;
    WorkingDir  : string;
    WindowState : Integer;
    Parameters  : string;
    //Windows user
    UserName    : string;
    Password    : string;
  end;

  //Base
  TvBaseNodeData = class
  private
    //Base private variables and functions
    FID          : Int64;
    FParentID    : Int64;
    FPosition    : Cardinal;
    FChanged     : boolean;
    FName        : String;
    FDataType    : TvTreeDataType;
    FImageIndex  : Integer;
    FImageLargeIndex : Integer;
    FParentNode  : PVirtualNode;
    FPNode       : PVirtualNode; //Self PVirtualNode
    FAddDate     : Int64;
    FEditDate    : Int64;
    FHideFromMenu : Boolean;
    function GetName:String;
    procedure SetName(Value: String);
    function GetDataType: TvTreeDataType;
    procedure SetDataType(const Value: TvTreeDataType);
    function GetAddDate:TDateTime;
    procedure SetAddDate(Value: TDateTime);
    function GetUnixAddDate:Int64;
    procedure SetUnixAddDate(Value: Int64);
    function GetEditDate:TDateTime;
    procedure SetEditDate(Value: TDateTime);
    function GetUnixEditDateEdit:Int64;
    procedure SetUnixEditDateEdit(Value: Int64);
  public
    //Base properties
    constructor Create(AType: TvTreeDataType); // virtual;
    property ID : Int64 read FID write FID;
    property ParentID : Int64 read FParentID write FParentID;
    property Position : Cardinal read FPosition write FPosition;
    property Changed: boolean read FChanged write FChanged;
    procedure Copy(source:TvBaseNodeData); virtual;
    property Name: string read GetName write SetName;
    property DataType: TvTreeDataType read GetDataType write SetDataType;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property ImageLargeIndex: Integer read FImageLargeIndex write FImageLargeIndex;
    property ParentNode: PVirtualNode read FParentNode write FParentNode;
    property PNode: PVirtualNode read FPNode write FPNode;
    property AddDate: TDateTime read GetAddDate write SetAddDate;
    property UnixAddDate: Int64 read GetUnixAddDate write SetUnixAddDate;
    property EditDate: TDateTime read GetEditDate write SetEditDate;
    property UnixEditDate: Int64 read GetUnixEditDateEdit write SetUnixEditDateEdit;
    property HideFromMenu:Boolean read FHideFromMenu write FHideFromMenu;
  end;
  PvBaseNodeData = ^TvBaseNodeData;

  //Separator
  TvCustomRealNodeData = class(TvBaseNodeData)
  private
    FPathIcon    : String;
    FPathAbsoluteIcon : String;
    FCacheID     : Integer;
    FCacheLargeID  : Integer;
    FPathCacheIcon : string;
    FPathCacheLargeIcon : string;
    FMRUPosition : Int64;
    FClickCount  : Integer;
    FWindowState : Integer;
    FActionOnExe : TActionOnExecute;
    FAutorun     : TAutorunType;
    FAutorunPos  : Integer; //Position for ASuiteStartUpApp and ASuiteShutdownApp
    FSchMode     : TSchedulerMode; //0 Disabled, 1 Once, 2 Hourly, 3 Daily, 4 Weekly
    FSchDateTime : TDateTime;
    procedure SetPathIcon(value:string);
    procedure SetCacheIcon(value:integer);
    procedure SetCacheLargeIcon(value:integer);
    procedure SetMRUPosition(Value: Int64);
    procedure SetClickCount(Value: Integer);
    procedure SetAutorun(value: TAutorunType);
    procedure SetSchMode(value: TSchedulerMode);
    procedure SetSchDateTime(value: TDateTime);
  public
    constructor Create(AType: TvTreeDataType); // virtual;
    procedure Copy(source:TvBaseNodeData); override;
    function Execute(Tree: TBaseVirtualTree;ProcessInfo: TProcessInfo): boolean; virtual;
    property MRUPosition: Int64 read FMRUPosition write SetMRUPosition;
    property ClickCount: Integer read FClickCount write SetClickCount;
    property PathIcon: string read FPathIcon write SetPathIcon;
    property PathAbsoluteIcon: String read FPathAbsoluteIcon write FPathAbsoluteIcon;
    property CacheID: Integer read FCacheID write SetCacheIcon;
    property CacheLargeID: Integer read FCacheLargeID write SetCacheLargeIcon;
    property PathCacheIcon: string read FPathCacheIcon write FPathCacheIcon;
    property PathCacheLargeIcon: string read FPathCacheLargeIcon write FPathCacheLargeIcon;
    property WindowState: Integer read FWindowState write FWindowState;
    property ActionOnExe: TActionOnExecute read FActionOnExe write FActionOnExe;
    property Autorun: TAutorunType read FAutorun write SetAutorun;
    property AutorunPos: Integer read FAutorunPos write FAutorunPos;
    property SchMode: TSchedulerMode read FSchMode write SetSchMode;
    property SchDateTime: TDateTime read FSchDateTime write SetSchDateTime;
    procedure ResetIcon;
  end;
  PvCustomRealNodeData = ^TvCustomRealNodeData;

  //Category
  TvCategoryNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    function InternalExecute(Tree: TBaseVirtualTree; NodeData: TvBaseNodeData;ProcessInfo: TProcessInfo): boolean;
  public
    //Specific properties
    constructor Create; overload;
    property Name;
    property DataType;
    property ImageIndex;
    property ParentNode;
    property PathIcon;
    property CacheID;
    property CacheLargeID;
    property PathCacheIcon;
    property AddDate;
    property UnixAddDate;
    property EditDate;
    property UnixEditDate;
    function Execute(Tree: TBaseVirtualTree;ProcessInfo: TProcessInfo): boolean; override;
  end;
  PvCategoryNodeData = ^TvCategoryNodeData;

  //Software
  TvFileNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    //Paths
    FPathExe         : string; //User (relative or absolute) pathExe
    FPathAbsoluteExe : string; //Absolute pathExe
    //Advanced
    FParameters      : string;
    FWorkingDir      : string;
    FWorkingDirAbsolute : string;
    FShortcutDesktop : Boolean;
    FNoMRU           : Boolean;
    FNoMFU           : Boolean;
    FRunFromCategory : Boolean;
    procedure SetPathExe(value:string);
    procedure SetWorkingDir(value:string);
    procedure SetNoMRU(value:Boolean);
    procedure SetNoMFU(value:Boolean);
    procedure SetShortcutDesktop(value:Boolean);
    function InternalExecute(ProcessInfo: TProcessInfo): boolean;
    function RunProcess(ProcessInfo: TProcessInfo): boolean;
    function RunProcessAsUser(ProcessInfo: TProcessInfo): boolean;
    function RunProcessAsAdmin(ProcessInfo: TProcessInfo): boolean;
  public
    //Specific properties
    constructor Create; overload;
    procedure Copy(source:TvBaseNodeData); override;
    function OpenExtractedFolder: Boolean;
    property Name;
    property ID;
    property ParentID;
    property Position;
    property Changed;
    property DataType;
    property ImageIndex;
    property ParentNode;
    property CacheID;
    property CacheLargeID;
    property PathCacheIcon;
    property NoMRU: Boolean read FNoMRU write SetNoMRU;
    property NoMFU: Boolean read FNoMFU write SetNoMFU;
    property PathExe: String read FPathExe write SetPathExe;
    property PathAbsoluteExe: String read FPathAbsoluteExe write FPathAbsoluteExe;
    property Parameters: string read FParameters write FParameters;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property WorkingDirAbsolute: string read FWorkingDirAbsolute write FWorkingDirAbsolute;
    property ShortcutDesktop:Boolean read FShortcutDesktop write SetShortcutDesktop;
    property RunFromCategory: Boolean read FRunFromCategory write FRunFromCategory;
    function Execute(Tree: TBaseVirtualTree;ProcessInfo: TProcessInfo): boolean; override;
  end;
  PvFileNodeData = ^TvFileNodeData;

  //Folder (for now vtdtFolder = vtdtSoftware)
//  TvFolderNodeData = class(TvFileNodeData)
//  private
//    //Specific private variables and functions
//  public
//    //Specific properties
//  end;
//  PvFolderNodeData = ^TvFolderNodeData;

  //Separator
  TvSeparatorNodeData = class(TvBaseNodeData)
  public
    //Specific properties
    constructor Create; overload;
    property Name;
    property DataType;
    property ImageIndex;
  end;
  PvSeparatorNodeData = ^TvSeparatorNodeData;

  rBaseData = record
    Data     : TvBaseNodeData;
    MenuItem : TMenuItem; //Classic Menu
    MenuNode : PVirtualNode; //Graphic Menu
  end;
  PBaseData = ^rBaseData;

  //TODO: Rename TTreeDataX in rTreeDataX
  PTreeDataX = ^TTreeDataX; //X = Search or TrayMenu
  TTreeDataX = record
    pNodeList : PVirtualNode;
  end;

  //Record for Options and Property form
  rFramesNodeData = record
    Title : string;
    Frame : TPageFrameClass;
    ImageIndex: Integer;
  end;
  PFramesNodeData = ^rFramesNodeData;

function CreateNodeData(AType: TvTreeDataType): TvBaseNodeData;

implementation

uses
  ulSysUtils, ulTreeView, ulExeUtils, ulCommonUtils, ulAppConfig, ulFileFolder;

function CreateNodeData(AType: TvTreeDataType): TvBaseNodeData;
begin
  case AType of
    vtdtCategory  : Result := TvCategoryNodeData.Create;
    vtdtFile      : Result := TvFileNodeData.Create(vtdtFile);
    vtdtFolder    : Result := TvFileNodeData.Create(vtdtFile);
    vtdtSeparator : Result := TvSeparatorNodeData.Create;
  else
    Result := nil;
  end;
end;

constructor TvBaseNodeData.Create(AType: TvTreeDataType);
begin
  FID          := -1;
  FParentID    := -1;
  FName        := '';
  FImageIndex  := -1;
  FImageLargeIndex := -1;
  FDataType    := AType;
  FParentNode  := nil;
  FPNode       := nil;
  FHideFromMenu := False;
  FAddDate     := DateTimeToUnix(Now);
  FEditDate    := FAddDate;
end;

procedure TvBaseNodeData.Copy(source:TvBaseNodeData);
begin
  FName       := msgCopy + source.Name;
  FImageIndex := -1;
  FImageLargeIndex := -1;
  FDataType   := source.DataType;
  FHideFromMenu := source.HideFromMenu;
end;

function TvBaseNodeData.GetName: String;
begin
  Result := FName;
end;

procedure TvBaseNodeData.SetName(Value: String);
begin
  FName := StringReplace(Value, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
  FName := StringReplace(FName, '&', '&&', [rfIgnoreCase,rfReplaceAll]);
end;

function TvBaseNodeData.GetDataType: TvTreeDataType;
begin
  Result := FDataType;
end;

procedure TvBaseNodeData.SetDataType(const Value: TvTreeDataType);
begin
  FDataType := Value;
end;

function TvBaseNodeData.GetAddDate: TDateTime;
begin
  Result := UnixToDateTime(FAddDate);
end;

procedure TvBaseNodeData.SetAddDate(Value: TDateTime);
begin
  FAddDate := DateTimeToUnix(Value);
end;

function TvBaseNodeData.GetUnixAddDate: Int64;
begin
  Result := FAddDate;
end;

procedure TvBaseNodeData.SetUnixAddDate(Value: Int64);
begin
  FAddDate := Value
end;

function TvBaseNodeData.GetEditDate: TDateTime;
begin
  Result := UnixToDateTime(FEditDate);
end;

procedure TvBaseNodeData.SetEditDate(Value: TDateTime);
begin
  FEditDate := DateTimeToUnix(Value);
end;

function TvBaseNodeData.GetUnixEditDateEdit: Int64;
begin
  Result := FEditDate;
end;

procedure TvBaseNodeData.SetUnixEditDateEdit(Value: Int64);
begin
  FEditDate := Value;
end;

//------------------------------------------------------------------------------

constructor TvCategoryNodeData.Create;
begin
  inherited Create(vtdtCategory);
  FAddDate    := 0;
  FEditDate   := 0;
end;

//------------------------------------------------------------------------------

constructor TvFileNodeData.Create;
begin
  inherited Create(vtdtFile);
  //Paths
  FPathExe         := '';
  FPathAbsoluteExe := '';
  //Advanced
  FParameters      := '';
  FWorkingDir      := '';
  FWindowState     := -1;
  FActionOnExe     := aeDefault;
  FNoMRU           := False;
  FNoMFU           := False;
  FShortcutDesktop := False;
  //Misc
  FAutorun         := atNever;
  FAutorunPos      := 0;
  FRunFromCategory := False;
end;

function TvFileNodeData.Execute(Tree: TBaseVirtualTree; ProcessInfo: TProcessInfo): boolean;
begin
  //If runmode is rmAutorunSingleInstance, check if process exists
  if ProcessInfo.RunMode = rmAutorunSingleInstance then
    if IsProcessExists(ExtractFileName(Self.PathAbsoluteExe)) then
    begin
      Result := True;
      Exit;
    end;
  //Run process
  Result := InternalExecute(ProcessInfo);
  if Result then
  begin
    //Add to MFU and increment clickcount
    Inc(FClickCount);
    if not(Self.NoMFU) then
      MFUList.Add(Self);
    MFUList.Sort;
    if (ProcessInfo.RunMode <> rmAutorunSingleInstance) and
       (ProcessInfo.RunMode <> rmAutorun) then
    begin
      //Add to mru and update mruposition
      if not(Self.NoMRU) then
        MRUList.Insert(0, Self);
      FMRUPosition := DateTimeToUnix(Now);
      //Run action after execution
      if Not(ProcessInfo.RunFromCat) then
        RunActionOnExe(Self.ActionOnExe);
    end;
    inherited;
  end
  else begin
    //Show error message
    ShowMessageFmt(msgErrRun,[FName],true);
  end;
end;

procedure TvFileNodeData.Copy(source:TvBaseNodeData);
var
  SourceNodeData : TvFileNodeData;
begin
  inherited;
  if source is TvFileNodeData then
  begin
    SourceNodeData     := TvFileNodeData(source);
    //Copy from source
    SetPathExe(SourceNodeData.PathExe);
    FParameters      := SourceNodeData.Parameters;
    FWorkingDir      := SourceNodeData.WorkingDir;
    FNoMRU           := SourceNodeData.FNoMRU;
    FNoMFU           := SourceNodeData.FNoMFU;
    SetShortcutDesktop(SourceNodeData.ShortcutDesktop);
    FRunFromCategory := SourceNodeData.FRunFromCategory;
  end;
end;

function TvFileNodeData.OpenExtractedFolder: Boolean;
begin
  Result := ShellExecute(GetDesktopWindow, 'open',
                         PChar(ExtractFileDir(FPathAbsoluteExe)),
                         nil, nil, SW_NORMAL) > 32;
end;

procedure TvFileNodeData.SetPathExe(value:string);
begin
  FPathExe := value;
  FPathAbsoluteExe := RelativeToAbsolute(value);
end;

procedure TvFileNodeData.SetWorkingDir(value:string);
begin
  FWorkingDir := value;
  FWorkingDirAbsolute := RelativeToAbsolute(value);
end;

procedure TvFileNodeData.SetNoMRU(value:Boolean);
begin
  if (Config.ASuiteState <> asImporting) then
  begin
    //If value is true, delete it from list
    if (value and (FNoMRU <> value)) then
      MRUList.Remove(Self)
    else //else add it in list
      if (not value and (FNoMRU <> value)) and (FMRUPosition > -1) then
        MRUList.Add(Self);
  end;
  FNoMRU := value;
end;

procedure TvFileNodeData.SetNoMFU(value:Boolean);
begin
  if (Config.ASuiteState <> asImporting) then
  begin
    //If value is true, delete it from list
    if (value and (FNoMFU <> value)) then
      MFUList.Remove(Self)
    else //else add it in list
      if (not value and (FNoMFU <> value)) and (FClickCount > 0) then
        MFUList.Add(Self);
  end;
  FNoMFU := value;
end;

procedure TvFileNodeData.SetShortcutDesktop(value:Boolean);
begin
  if (Config.ASuiteState <> asImporting) then
  begin
    //If value is true, create shortcut in desktop
    if (value and (FShortcutDesktop <> value)) then
      CreateShortcutOnDesktop(Name + EXT_LNK, FPathAbsoluteExe,FParameters,FWorkingDir)
    else //else delete it from desktop
      if (not value and (FShortcutDesktop <> value)) then
        DeleteShortcutOnDesktop(FName + EXT_LNK);
  end;
  FShortcutDesktop := value;
end;

function TvFileNodeData.InternalExecute(ProcessInfo: TProcessInfo): boolean;
begin
  Result := False;
  //File
  ProcessInfo.PathExe := FPathAbsoluteExe;
  //Working directory
  if FWorkingDir = '' then
    ProcessInfo.WorkingDir := ExtractFileDir(FPathAbsoluteExe)
  else
    ProcessInfo.WorkingDir := RelativeToAbsolute(FWorkingDir);
  //Window state
  if Not(ProcessInfo.RunFromCat) or (ProcessInfo.WindowState = -1) then
  begin
    case FWindowState of
      1: ProcessInfo.WindowState := SW_SHOWMINNOACTIVE;
      2: ProcessInfo.WindowState := SW_SHOWMAXIMIZED;
    else
      ProcessInfo.WindowState := SW_SHOWDEFAULT;
    end;
  end;
  //Parameters
  ProcessInfo.Parameters := RelativeToAbsolute(FParameters);
  //Execution
  if (ProcessInfo.RunMode in [rmNormal,rmAutorun,rmAutorunSingleInstance]) then
    Result := Self.RunProcess(ProcessInfo)
  else
    if ProcessInfo.RunMode = rmRunAs then
      Result := Self.RunProcessAsUser(ProcessInfo)
    else
      if ProcessInfo.RunMode = rmRunAsAdmin then
        Result := Self.RunProcessAsAdmin(ProcessInfo);
end;

function TvFileNodeData.RunProcess(ProcessInfo: TProcessInfo): boolean;
begin
  Result := ShellExecute(GetDesktopWindow, 'open', PChar(ProcessInfo.PathExe),
                            PChar(ProcessInfo.Parameters),
                            PChar(ProcessInfo.WorkingDir), ProcessInfo.WindowState) > 32;
end;

function TvFileNodeData.RunProcessAsUser(ProcessInfo: TProcessInfo): boolean;
var
  StartupInfo : TStartupInfoW;
  ProcInfo    : TProcessInformation;
begin
  FillMemory(@StartupInfo, sizeof(StartupInfo), 0);
  FillMemory(@ProcInfo, sizeof(ProcInfo), 0);
  StartupInfo.cb := sizeof(TStartupInfoW);
  StartupInfo.wShowWindow := WindowState;
  //Run process as Windows another user
  Result := CreateProcessWithLogonW(PWideChar(ProcessInfo.UserName), nil,
                                    PWideChar(ProcessInfo.Password),
                                    LOGON_WITH_PROFILE,
                                    PWideChar(ProcessInfo.PathExe), nil,
                                    CREATE_UNICODE_ENVIRONMENT, nil,
                                    PWideChar(ProcessInfo.WorkingDir),
                                    StartupInfo, ProcInfo);
  //Close handles
  if Result then
  begin
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
  end
end;

function TvFileNodeData.RunProcessAsAdmin(ProcessInfo: TProcessInfo): boolean;
var
  ShellExecuteInfo: TShellExecuteInfo;
begin
  ZeroMemory(@ShellExecuteInfo, SizeOf(ShellExecuteInfo));
  ShellExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellExecuteInfo.Wnd    := GetDesktopWindow;
  ShellExecuteInfo.fMask  := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  ShellExecuteInfo.lpVerb := PChar('runas');
  //Set process's path, working dir, parameters and window state
  ShellExecuteInfo.lpFile := PChar(ProcessInfo.PathExe);
  ShellExecuteInfo.lpDirectory := PChar(ProcessInfo.WorkingDir);
  if ProcessInfo.Parameters <> '' then
    ShellExecuteInfo.lpParameters := PChar(ProcessInfo.Parameters);
  ShellExecuteInfo.nShow := ProcessInfo.WindowState;
  //Run process
  Result := ShellExecuteEx(@ShellExecuteInfo);
end;

//------------------------------------------------------------------------------

constructor TvSeparatorNodeData.Create;
begin
  inherited Create(vtdtSeparator);
  FName := '';
end;

//------------------------------------------------------------------------------

{ TvCustomRealNodeData }

procedure TvCustomRealNodeData.Copy(source: TvBaseNodeData);
var
  SourceNodeData : TvCustomRealNodeData;
begin
  inherited;
  if source is TvCustomRealNodeData then
  begin
    SourceNodeData := TvCustomRealNodeData(source);
    //Copy from source
    SetPathIcon(SourceNodeData.PathIcon);
    FWindowState := SourceNodeData.WindowState;
    FActionOnExe := SourceNodeData.ActionOnExe;
    SetAutorun(SourceNodeData.Autorun);
    SetSchMode(SourceNodeData.SchMode);
    SetSchDateTime(SourceNodeData.SchDateTime);
  end;
end;

constructor TvCustomRealNodeData.Create(AType: TvTreeDataType);
begin
  inherited;
  FMRUPosition := -1;
  FClickCount  := 0;
  FPathIcon    := '';
  FCacheID     := -1;
  FCacheLargeID  := -1;
  FPathCacheIcon := '';
  FSchMode     := smDisabled;
  FSchDateTime := Now;
end;

procedure TvCustomRealNodeData.SetMRUPosition(Value: Int64);
begin
  FMRUPosition := Value;
  if (Config.ASuiteState <> asImporting) then
    if (FMRUPosition > -1) and (not TvFileNodeData(Self).FNoMRU) then
      MRUList.Add(Self);
end;

procedure TvCustomRealNodeData.SetClickCount(Value: Integer);
begin
  FClickCount := Value;
  if (Config.ASuiteState <> asImporting) then
    if (FClickCount > 0) and (not TvFileNodeData(Self).FNoMFU) then
      MFUList.Add(Self);
end;

procedure TvCustomRealNodeData.SetSchDateTime(value: TDateTime);
begin
  //If value is not a empty TDateTime, set it in FSchDateTime
  if value <> 0 then
    FSchDateTime := value
  else //Else use function Now
    FSchDateTime := Now;
end;

procedure TvCustomRealNodeData.SetPathIcon(value:string);
begin
  FPathIcon := value;
  FPathAbsoluteIcon := RelativeToAbsolute(value);
end;

procedure TvCustomRealNodeData.SetSchMode(value: TSchedulerMode);
begin
  if (Config.ASuiteState <> asImporting) then
    if (FSchMode <> value) then
    begin
      if (FSchMode <> smDisabled) and (value = smDisabled) then
        SchedulerItemList.Remove(Self);
      if (FSchMode = smDisabled) and (value <> smDisabled) then
        SchedulerItemList.Add(Self);
    end;
  FSchMode := value;
end;

procedure TvCustomRealNodeData.SetCacheIcon(value:integer);
begin
  FCacheID := value;
  if (value <> -1) then
    FPathCacheIcon := SUITE_CACHE_PATH + IntToStr(value) + EXT_ICO
  else begin
    if FileExists(FPathCacheIcon) then
    begin
      DeleteFile(PWideChar(FPathCacheIcon));
      FImageIndex := -1;
      FChanged := True;
    end;
    FPathCacheIcon := '';
  end;
end;

procedure TvCustomRealNodeData.SetCacheLargeIcon(value: integer);
begin
  FCacheLargeID := value;
  if (value <> -1) then
    FPathCacheLargeIcon := SUITE_CACHELARGE_PATH + IntToStr(value) + EXT_ICO
  else begin
    if FileExists(FPathCacheLargeIcon) then
    begin
      DeleteFile(PWideChar(FPathCacheLargeIcon));
      FImageLargeIndex := -1;
      FChanged := True;
    end;
    FPathCacheLargeIcon := '';
  end;
end;

function TvCustomRealNodeData.Execute(Tree: TBaseVirtualTree;ProcessInfo: TProcessInfo): boolean;
begin
  FChanged := True;
  RefreshList(Tree);
  Result := True;
end;

procedure TvCustomRealNodeData.ResetIcon;
begin
  Self.CacheID      := -1;
  Self.CacheLargeID := -1;
  Self.ImageIndex   := -1;
end;

procedure TvCustomRealNodeData.SetAutorun(value:TAutorunType);
begin
  if (Config.ASuiteState <> asImporting) then
  begin
    //If it is changed, remove from old list and insert in new list
    if (value > atNever) and ((FAutorun) <> (value)) then
    begin
      if (FAutorun in [atAlwaysOnStart, atSingleInstance, atNever]) and (value in [atAlwaysOnClose]) then
      begin
        StartupItemList.Remove(Self);
        ShutdownItemList.Insert(Self.FAutorunPos, Self)
      end
      else
        if (FAutorun in [atAlwaysOnClose, atNever]) and (value in [atAlwaysOnStart, atSingleInstance]) then
        begin
          ShutdownItemList.Remove(Self);
          StartupItemList.Insert(Self.FAutorunPos, Self);
        end;
    end
    else begin
      //If it is changed, remove from old list
      if (FAutorun in [atAlwaysOnStart, atSingleInstance]) and (value in [atNever]) then
        StartupItemList.Remove(Self)
      else
        if (FAutorun in [atAlwaysOnClose]) and (value in [atNever]) then
          ShutdownItemList.Remove(Self);
    end;
  end;
  //Set new value
  FAutorun := value;
end;

//------------------------------------------------------------------------------

{ TvCategoryNodeData }

function TvCategoryNodeData.Execute(Tree: TBaseVirtualTree;
  ProcessInfo: TProcessInfo): boolean;
var
  Node : PVirtualNode;
  CurrentNodeData : TvBaseNodeData;
begin
  //Get Category's child (only first level)
  Node := FPNode.FirstChild;
  try
    while Assigned(Node) do
    begin
      CurrentNodeData := PBaseData(Tree.GetNodeData(Node)).Data;
      if (CurrentNodeData.DataType in [vtdtFile,vtdtFolder]) then
        InternalExecute(Tree,CurrentNodeData,ProcessInfo);
      Node := Node.NextSibling;
    end;
  finally
    //Override action on execute property
    if (ProcessInfo.RunMode <> rmAutorunSingleInstance) and
       (ProcessInfo.RunMode <> rmAutorun) then
      RunActionOnExe(Self.ActionOnExe);
    Result := True;
  end;
end;

function TvCategoryNodeData.InternalExecute(Tree: TBaseVirtualTree;
  NodeData: TvBaseNodeData;ProcessInfo: TProcessInfo): boolean;
begin
  Result := False;
  //If necessary, Override child item's FWindowState
  case FWindowState of
    0: ProcessInfo.WindowState := -1;
    1: ProcessInfo.WindowState := SW_SHOWDEFAULT;
    2: ProcessInfo.WindowState := SW_SHOWMINNOACTIVE;
    3: ProcessInfo.WindowState := SW_SHOWMAXIMIZED;
  end;
  //Execute file item
  if TvFileNodeData(NodeData).RunFromCategory then
    Result := TvFileNodeData(NodeData).Execute(Tree, ProcessInfo);
end;

end.
