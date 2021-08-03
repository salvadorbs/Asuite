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

unit Kernel.Instance;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils, VirtualTrees, AppConfig.Paths, Kernel.Scheduler;

type

  { TASuiteInstance }

  TASuiteInstance = class
  private
    FMainTree: TVirtualStringTree;
    FPaths: TConfigPaths;
    FScheduler: TScheduler;

    function GetBigHeightNode: Integer;
    function GetImportTree: TVirtualStringTree;
    function GetMainTree: TVirtualStringTree;
    function GetSmallHeightNode: Integer;
  public
    constructor Create; overload;
    destructor Destroy; override;

    property MainTree: TVirtualStringTree read GetMainTree write FMainTree;
    property ImportTree: TVirtualStringTree read GetImportTree;
    property Paths: TConfigPaths read FPaths;
    property Scheduler: TScheduler read FScheduler;

    property SmallHeightNode: Integer read GetSmallHeightNode;
    property BigHeightNode: Integer read GetBigHeightNode;

    procedure HandleParam(const Param: string; FirstInstance: Boolean = True);

    //Database
    procedure LoadList;
    function SaveList(DoBackup: Boolean): Boolean;
  end;

var
  ASuiteInstance: TASuiteInstance;

implementation

uses
  Forms.ImportList, Kernel.Logger, Forms, Kernel.Consts, Utility.FileFolder,
  Utility.Misc, Utility.XML, VirtualTree.Methods, SynLog, Kernel.Manager, SynCommons;

{ TASuiteInstance }

function TASuiteInstance.GetImportTree: TVirtualStringTree;
begin
  Result := nil;
  if Assigned(frmImportList) then
    Result := frmImportList.vstListImp;
end;

function TASuiteInstance.GetBigHeightNode: Integer;
begin
  //Node height based of DPI
  Result := Round((Screen.PixelsPerInch / 96.0) * NODE_HEIGHT_LARGE);
end;

function TASuiteInstance.GetMainTree: TVirtualStringTree;
begin
  Assert(Assigned(FMainTree));
  Result := FMainTree;
end;

function TASuiteInstance.GetSmallHeightNode: Integer;
begin
  //Node height based of DPI
  Result := Round((Screen.PixelsPerInch / 96.0) * NODE_HEIGHT_SMALL);
end;

constructor TASuiteInstance.Create;
var
  I: Integer;
begin
  FScheduler := TScheduler.Create;

  //Params
  for I := 1 to ParamCount do
    HandleParam(ParamStr(I));

  //Create some classes
  FPaths  := TConfigPaths.Create;

  //Setup logger
  with TSynLog.Family do
  begin
    DestinationPath := Self.Paths.SuitePathData;
    Level := LOG_VERBOSE;
    {$IFNDEF DEBUG}
    //TODO: Define log_level for release builds - No sql logs? No debug infos? Yes stacktrace?
    Level := LOG_VERBOSE - [sllStackTrace, sllDebug];
    {$ENDIF}
    RotateFileCount := 1;
    RotateFileDailyAtHour := 0;
  end;
end;

destructor TASuiteInstance.Destroy;
begin
  inherited Destroy;

  FPaths.Destroy;
end;

procedure TASuiteInstance.HandleParam(const Param: string;
  FirstInstance: Boolean);
var
  sName, sValue: string;

  procedure ParseParam(s: string);
  var
    iSplit: Integer;
  begin
    if (s[1] in ['-', '/']) then
    begin
      Delete(s, 1, 1);
      iSplit := Pos('=', s);
      if iSplit <> 0 then
      begin
        sName := Copy(s, 1, iSplit - 1);
        sValue := Copy(s, iSplit + 1, 666);
      end;
    end;
  end;

begin
  TASuiteLogger.Info('Received parameter "%s"', [Param]);
  ParseParam(Param);
  if sName <> '' then
  begin
    if (CompareText(sName, 'list') = 0) and (FirstInstance) then
      FPaths.SuitePathList := FPaths.RelativeToAbsolute(RemoveAllQuotes(sValue));

    //Add new node
    if (CompareText(sName, 'additem') = 0) and (Assigned(ASuiteManager.DBManager)) then
      TVirtualTreeMethods.AddNodeByPathFile(FMainTree, nil, RemoveAllQuotes(sValue), amInsertAfter);
  end;
end;

procedure TASuiteInstance.LoadList;
var
  sFilePath  : string;
begin
  TASuiteLogger.Info('Finding ASuite SQLite Database', []);
  Assert(Assigned(ASuiteInstance.MainTree), 'ASuiteInstance.MainTree is not assigned!');
  try
    //List
    if ExtractFileExtEx(ASuiteInstance.Paths.SuitePathList) = EXT_XML then
    begin
      sFilePath := ASuiteInstance.Paths.SuitePathList;
      ASuiteInstance.Paths.SuitePathList := ChangeFileExt(ASuiteInstance.Paths.SuitePathList, EXT_SQL);
    end;

    if Assigned(ASuiteManager.DBManager) then
      ASuiteManager.DBManager.Setup(ASuiteInstance.Paths.SuitePathList);
  finally
    //If exists old list format (xml), use it
    if sFilePath <> '' then
      LoadDatabaseFromXML(sFilePath)
    else //Use new list format (sqlite db)
      ASuiteManager.DBManager.LoadData(ASuiteInstance.MainTree);
  end;
end;

function TASuiteInstance.SaveList(DoBackup: Boolean): Boolean;
begin
  Result := ASuiteManager.DBManager.SaveData(ASuiteInstance.MainTree, DoBackup);
end;

initialization
  ASuiteInstance := TASuiteInstance.Create;

finalization
  FreeAndNil(ASuiteInstance);

end.

