{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

//ToDo: Create another class scheme. See below:
 (*
 TvBaseNodeData
              +->TvSeparatorNodeData
              +->TvCustomNodeData
                                +->TvCategoryData
                                +->TvFileNodeData
                                                +->TvFolderNodeData
 *)

unit ulNodeDataTypes;

interface

uses
  VirtualTrees, Menus, SysUtils, Dialogs, appConfig, DateUtils, ulEnumerations,
  LCLIntf, Process, FileUtil;

type

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
    FParentNode  : PVirtualNode;
    FPathIcon    : String;
    FPathAbsoluteIcon : String;
    FCacheID     : Integer;
    FPathCacheIcon : string;
    FAddDate     : Int64;
    FEditDate    : Int64;
    FMRUPosition : Int64;
    FClickCount  : Integer;
    FHideFromMenu : Boolean;
    function GetName:String;
    procedure SetName(Value: String);
    function GetDataType: TvTreeDataType;
    procedure SetDataType(const Value: TvTreeDataType);
    procedure SetPathIcon(value:string);
    procedure SetPathCacheIcon(value:integer);
    function GetAddDate:TDateTime;
    procedure SetAddDate(Value: TDateTime);
    function GetUnixAddDate:Int64;
    procedure SetUnixAddDate(Value: Int64);
    function GetEditDate:TDateTime;
    procedure SetEditDate(Value: TDateTime);
    function GetUnixEditDateEdit:Int64;
    procedure SetUnixEditDateEdit(Value: Int64);
    procedure SetMRUPosition(Value: Int64);
    procedure SetClickCount(Value: Integer);
    function InternalExecute: boolean; virtual;
  public
    //Base properties
    constructor Create(AType: TvTreeDataType); // virtual;
    function Execute(Tree: TBaseVirtualTree;Autorun:Boolean): boolean;
    property ID : Int64 read FID write FID;
    property ParentID : Int64 read FParentID write FParentID;
    property Position : Cardinal read FPosition write FPosition;
    property Changed: boolean read FChanged write FChanged;
    procedure Copy(source:TvBaseNodeData); virtual;
    property Name: string read GetName write SetName;
    property DataType: TvTreeDataType read GetDataType write SetDataType;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property ParentNode: PVirtualNode read FParentNode write FParentNode;
    property PathIcon: string read FPathIcon write SetPathIcon;
    property PathAbsoluteIcon: String read FPathAbsoluteIcon write FPathAbsoluteIcon;
    property CacheID: Integer read FCacheID write SetPathCacheIcon;
    property PathCacheIcon: string read FPathCacheIcon write FPathCacheIcon;
    property MRUPosition: Int64 read FMRUPosition write SetMRUPosition;
    property AddDate: TDateTime read GetAddDate write SetAddDate;
    property UnixAddDate: Int64 read GetUnixAddDate write SetUnixAddDate;
    property EditDate: TDateTime read GetEditDate write SetEditDate;
    property UnixEditDate: Int64 read GetUnixEditDateEdit write SetUnixEditDateEdit;
    property ClickCount: Integer read FClickCount write SetClickCount;
    property HideFromMenu:Boolean read FHideFromMenu write FHideFromMenu;
  end;
  PvBaseNodeData = ^TvBaseNodeData;

  //Category
  TvCategoryNodeData = class(TvBaseNodeData)
  private
    //Specific private variables and functions
  public
    //Specific properties
    constructor Create; overload;
    property Name;
    property DataType;
    property ImageIndex;
    property ParentNode;
    property PathIcon;
    property CacheID;
    property PathCacheIcon;
    property AddDate;
    property UnixAddDate;
    property EditDate;
    property UnixEditDate;
  end;
  PvCategoryNodeData = ^TvCategoryNodeData;

  //Software
  TvFileNodeData = class(TvBaseNodeData)
  private
    //Specific private variables and functions
    //Paths
    FPathExe         : string; //User (relative or absolute) pathExe
    FPathAbsoluteExe : string; //Absolute pathExe
    //Advanced
    FParameters      : string;
    FWorkingDir      : string;
    FWorkingDirAbsolute : string;
    FWindowState     : Integer;
    FActionOnExe     : TActionOnExecution;
    FShortcutDesktop : Boolean;
    FNoMRU           : Boolean;
    FNoMFU           : Boolean;
    //Misc
    FAutorun         : TAutorunType;
    FAutorunPos      : Integer; //Position for ASuiteStartUpApp and ASuiteShutdownApp
    procedure SetPathExe(value:string);
    procedure SetWorkingDir(value:string);
    procedure SetAutorun(value:TAutorunType);
    procedure SetNoMRU(value:Boolean);
    procedure SetNoMFU(value:Boolean);
    procedure SetShortcutDesktop(value:Boolean);
    function InternalExecute: boolean; override;
    function RunProcess: boolean;
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
    property PathCacheIcon;
    property NoMRU: Boolean read FNoMRU write SetNoMRU;
    property NoMFU: Boolean read FNoMFU write SetNoMFU;
    property PathExe: String read FPathExe write SetPathExe;
    property PathAbsoluteExe: String read FPathAbsoluteExe write FPathAbsoluteExe;
    property Parameters: string read FParameters write FParameters;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property WorkingDirAbsolute: string read FWorkingDirAbsolute write FWorkingDirAbsolute;
    property WindowState: Integer read FWindowState write FWindowState;
    property ActionOnExe: TActionOnExecution read FActionOnExe write FActionOnExe;
    property ShortcutDesktop:Boolean read FShortcutDesktop write SetShortcutDesktop;
    property Autorun: TAutorunType read FAutorun write SetAutorun;
    property AutorunPos: Integer read FAutorunPos write FAutorunPos;
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
    MenuItem : TMenuItem;
    pNode    : PVirtualNode;
  end;
  PBaseData = ^rBaseData;

  PTreeDataX = ^TTreeDataX; //X = Search or TrayMenu
  TTreeDataX = record
    pNodeList : PVirtualNode;
    pNodeX    : PVirtualNode;
  end;

function CreateNodeData(AType: TvTreeDataType): TvBaseNodeData;

implementation

uses
  ulSysUtils, ulTreeView, ulExeUtils;

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
  FDataType    := AType;
  FMRUPosition := -1;
  FParentNode  := nil;
  FPathIcon    := '';
  FCacheID     := -1;
  FPathCacheIcon := '';
  FClickCount  := 0;
  FHideFromMenu := False;
  FAddDate     := DateTimeToUnix(Now);
  FEditDate    := FAddDate;
end;

procedure TvBaseNodeData.Copy(source:TvBaseNodeData);
begin
  FName       := msgCopy + source.Name;
  FImageIndex := source.ImageIndex;
  FDataType   := source.DataType;
  FPathIcon   := source.PathIcon;
  FHideFromMenu := source.HideFromMenu;
  //Set nil to some specific properties
  FCacheID    := -1;
  FPathCacheIcon := '';
  PathAbsoluteIcon := '';
end;

function TvBaseNodeData.GetName: String;
begin
  Result := FName;
end;

procedure TvBaseNodeData.SetName(Value: String);
begin
  FName := Value;
end;

function TvBaseNodeData.GetDataType: TvTreeDataType;
begin
  Result := FDataType;
end;

procedure TvBaseNodeData.SetDataType(const Value: TvTreeDataType);
begin
  FDataType := Value;
end;

procedure TvBaseNodeData.SetPathIcon(value:string);
begin
  FPathIcon := value;
  FPathAbsoluteIcon := RelativeToAbsolute(value);
end;

procedure TvBaseNodeData.SetPathCacheIcon(value:integer);
begin
  FCacheID := value;
  if (value <> -1) then
    FPathCacheIcon := SUITE_CACHE_PATH + IntToStr(value) + EXT_ICO
  else
    FPathCacheIcon := '';
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

procedure TvBaseNodeData.SetMRUPosition(Value: Int64);
begin
  FMRUPosition := Value;
  if (FMRUPosition > -1) and (not TvFileNodeData(Self).FNoMRU) then
    MRUList.Add(Self);
end;

procedure TvBaseNodeData.SetClickCount(Value: Integer);
begin
  FClickCount := Value;
  if (FClickCount > 0) and (not TvFileNodeData(Self).FNoMFU) then
    MFUList.Add(Self);
end;

function TvBaseNodeData.InternalExecute: boolean;
begin
  Result := False;
end;

function TvBaseNodeData.Execute(Tree: TBaseVirtualTree;Autorun:Boolean): boolean;
begin
  Result := InternalExecute;
  if Result then
  begin
    //Add to MFU and increment clickcount
    Inc(FClickCount);
    if not(TvFileNodeData(Self).NoMFU) then
      MFUList.Add(Self);
    MFUList.Sort;
    if Not(Autorun) then
    begin
      //Add to mru and update mruposition
      if not(TvFileNodeData(Self).NoMRU) then
        MRUList.Insert(0, Self);
      FMRUPosition := DateTimeToUnix(Now);
      //Run action after execution
      RunActionOnExe(TvFileNodeData(Self));
    end;
    FChanged := True;
    RefreshList(Tree);
  end
  else begin
    //Show error message
    ShowMessageFmt(msgErrRun,[FName]);
  end;
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
end;

procedure TvFileNodeData.Copy(source:TvBaseNodeData);
var
  FileNodeData : TvFileNodeData;
begin
  inherited;
  if source is TvFileNodeData then
  begin
    FileNodeData     := TvFileNodeData(source);
    //Paths
    SetPathExe(FileNodeData.PathExe);
    SetPathIcon(FileNodeData.PathIcon);
    //Advanced
    FParameters      := FileNodeData.Parameters;
    FWorkingDir      := FileNodeData.WorkingDir;
    FWindowState     := FileNodeData.WindowState;
    FActionOnExe     := FileNodeData.ActionOnExe;
    FNoMRU           := FileNodeData.FNoMRU;
    FNoMFU           := FileNodeData.FNoMFU;
    SetShortcutDesktop(FileNodeData.ShortcutDesktop);
    //Misc
    SetAutorun(FileNodeData.Autorun);
  end;
end;

function TvFileNodeData.OpenExtractedFolder: Boolean;
begin
  Result := OpenDocument(ExtractFileDir(FPathAbsoluteExe));
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

procedure TvFileNodeData.SetAutorun(value:TAutorunType);
begin
  //If it is changed, remove from old list and insert in new list
  if (value > atNever) and ((FAutorun) <> (value)) then
  begin
    if (FAutorun in [atAlwaysOnStart, atSingleInstance, atNever]) and (value in [atAlwaysOnClose]) then
    begin
      ASuiteStartUpApp.Remove(Self);
      ASuiteShutdownApp.Insert(Self.FAutorunPos, Self)
    end
    else
      if (FAutorun in [atAlwaysOnClose, atNever]) and (value in [atAlwaysOnStart, atSingleInstance]) then
      begin
        ASuiteShutdownApp.Remove(Self);
        ASuiteStartUpApp.Insert(Self.FAutorunPos, Self);
      end;
  end
  else begin
    //If it is changed, remove from old list
    if (FAutorun in [atAlwaysOnStart, atSingleInstance]) and (value in [atNever]) then
      ASuiteStartUpApp.Remove(Self)
    else
      if (FAutorun in [atAlwaysOnClose]) and (value in [atNever]) then
        ASuiteShutdownApp.Remove(Self);
  end;
  //Set new value
  FAutorun := value;
end;

procedure TvFileNodeData.SetNoMRU(value:Boolean);
begin
  //If value is true, delete it from list
  if (value and (FNoMRU <> value)) then
    MRUList.Remove(Self)
  else //else add it in list
    if (not value and (FNoMRU <> value)) and (FMRUPosition > -1) then
      MRUList.Add(Self);
  FNoMRU := value;
end;

procedure TvFileNodeData.SetNoMFU(value:Boolean);
begin
  //If value is true, delete it from list
  if (value and (FNoMFU <> value)) then
    MFUList.Remove(Self)
  else //else add it in list
    if (not value and (FNoMFU <> value)) and (FClickCount > 0) then
      MFUList.Add(Self);
  FNoMFU := value;
end;

procedure TvFileNodeData.SetShortcutDesktop(value:Boolean);
begin
  //If value is true, create shortcut in desktop
  if (value and (FShortcutDesktop <> value)) then
    CreateShortcutOnDesktop(Name + EXT_LNK, FPathAbsoluteExe,FParameters,FWorkingDir)
  else //else delete it from desktop
    if (not value and (FShortcutDesktop <> value)) then
      DeleteShortcutOnDesktop(FName + EXT_LNK);
  FShortcutDesktop := value;
end;

function TvFileNodeData.InternalExecute: boolean;
begin
  Result := Self.RunProcess;
end;

function TvFileNodeData.RunProcess: boolean;
var
  TestProcess: TProcess;
begin
  //Execution
  if FileFolderPageWebExists(FPathAbsoluteExe) then
  begin
    if FileExistsUTF8(FPathAbsoluteExe) then
    begin
      //Is it exe? If yes, use a TProcess
      if(ExtractFileExt(FPathAbsoluteExe) = '.exe') then
      begin
        TestProcess := TProcess.Create(nil);
        try
          TestProcess.Executable := FPathAbsoluteExe;
          //Working directory
          if FWorkingDirAbsolute = '' then
            TestProcess.CurrentDirectory := ExtractFileDir(FPathAbsoluteExe)
          else
            TestProcess.CurrentDirectory := FWorkingDirAbsolute;
          //Parameters
          if FParameters <> '' then
            TestProcess.Parameters.Add(RelativeToAbsolute(FParameters));
          //Window state
          case FWindowState of
            1: TestProcess.ShowWindow := swoshowMinNOActive;
            2: TestProcess.ShowWindow := swoShowMaximized;
          else
            TestProcess.ShowWindow := swoShowDefault;
          end;
          TestProcess.StartupOptions := [suoUseShowWindow];
          TestProcess.Execute;
          Result := TestProcess.ProcessID <> 0;
        finally
          TestProcess.Free;
        end;
      end
      else //Else use OpenDocument
        Result := OpenDocument(FPathAbsoluteExe);
    end
    else //Folders
      if DirectoryExistsUTF8(FPathAbsoluteExe) then
        Result :=  OpenDocument(FPathAbsoluteExe)
      else //Url
        if IsUrl(FPathAbsoluteExe) then
          Result :=  OpenURL(FPathAbsoluteExe);
  end;
end;

//------------------------------------------------------------------------------

constructor TvSeparatorNodeData.Create;
begin
  inherited Create(vtdtSeparator);
  FName := '';
end;

//------------------------------------------------------------------------------

end.
