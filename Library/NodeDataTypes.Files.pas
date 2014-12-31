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
  SysUtils, Kernel.Enumerations, NodeDataTypes.Base,
  NodeDataTypes.Custom, WinApi.Windows, WinApi.ShellApi, DateUtils;

type
  TvFileNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    //Paths
    FPathExe         : string; //User pathExe //TODO: Rename it as FPathFile
    //Advanced
    FParameters      : string;
    FWorkingDir      : string;
    FShortcutDesktop : Boolean;
    FClickCount      : Integer;
    FNoMRU           : Boolean;
    FNoMFU           : Boolean;
    FRunFromCategory : Boolean;
    FIsPathAbsoluteExeExists: Boolean;
    procedure SetPathExe(value:string);
    procedure SetWorkingDir(value:string);
    procedure SetNoMRU(value:Boolean);
    procedure SetNoMFU(value:Boolean);
    procedure SetShortcutDesktop(value:Boolean);
    function GetPathAbsoluteExe: String;
    function GetWorkingDirAbsolute: string;
    procedure SetClickCount(const Value: Integer);
  public
    //Specific properties
    constructor Create(AType: TvTreeDataType); overload;

    procedure Copy(source: TvBaseNodeData); override;

    function ExplorePath: Boolean;

    property ClickCount: Integer read FClickCount write SetClickCount;
    property NoMRU: Boolean read FNoMRU write SetNoMRU;
    property NoMFU: Boolean read FNoMFU write SetNoMFU;
    property PathExe: String read FPathExe write SetPathExe;
    property PathAbsoluteExe: String read GetPathAbsoluteExe;
    property IsPathAbsoluteExeExists: Boolean read FIsPathAbsoluteExeExists;
    property Parameters: string read FParameters write FParameters;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property WorkingDirAbsolute: string read GetWorkingDirAbsolute;
    property ShortcutDesktop:Boolean read FShortcutDesktop write SetShortcutDesktop;
    property RunFromCategory: Boolean read FRunFromCategory write FRunFromCategory;

    procedure DeleteShortcutFile;
  end;
  PvFileNodeData = ^TvFileNodeData;

implementation

uses
  AppConfig.Main, Lists.Manager, Kernel.Consts, Utility.FileFolder,
  Lists.Base;

constructor TvFileNodeData.Create(AType: TvTreeDataType);
begin
  inherited Create(vtdtFile);
  if AType = vtdtFolder then
    Self.PathIcon := Config.Paths.AbsoluteToRelative(Config.Paths.SuitePathIconsTree + FILEICON_Folder);
  //Paths
  FPathExe         := '';
  FIsPathAbsoluteExeExists := False;
  //Advanced
  FParameters      := '';
  FWorkingDir      := '';
  FNoMRU           := False;
  FNoMFU           := False;
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

function TvFileNodeData.GetPathAbsoluteExe: String;
begin
  if FPathExe <> '' then
    Result := Config.Paths.RelativeToAbsolute(FPathExe)
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

procedure TvFileNodeData.Copy(Source: TvBaseNodeData);
var
  SourceNodeData: TvFileNodeData;
begin
  inherited;
  if Source is TvFileNodeData then
  begin
    SourceNodeData   := TvFileNodeData(Source);
    //Copy from Source
    SetPathExe(SourceNodeData.PathExe);
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
  Result := ShellExecute(GetDesktopWindow, 'open',
                         PChar(ExtractFileDir(Self.PathAbsoluteExe)),
                         nil, nil, SW_NORMAL) > 32;
end;

procedure TvFileNodeData.SetPathExe(value:string);
begin
  FPathExe := value;
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
    if (FClickCount > 0) and (not TvFileNodeData(Self).FNoMFU) then
      Config.ListManager.MFUList.AddItem(Self);
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
      CreateShortcutOnDesktop(Name + EXT_LNK, Self.PathAbsoluteExe, FParameters, Self.WorkingDirAbsolute)
    else //else delete it from desktop
      if (not value and (FShortcutDesktop <> value)) then
        DeleteShortcutOnDesktop(Self.Name + EXT_LNK);
  end;
  FShortcutDesktop := value;
end;

end.
