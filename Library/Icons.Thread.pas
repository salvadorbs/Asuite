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

unit Icons.Thread;

{$MODE DelphiUnicode}

interface

uses
  Classes, VirtualTrees;

type
  { Preloads tree icons asynchronously without touching the LCL/VCL,
    the ImageList or the tree itself from the worker thread.

    The node-data snapshot is collected in the constructor (caller thread,
    expected to be the main thread). Execute only drives the preload and
    runs every actual icon load synchronized on the main thread, so
    platform extraction, ImageList inserts and VirtualTree access stay
    where the LCL expects them. }

  TTreeIconsThread = class(TThread)
  private
    FItems: TList;
    FIndex: Integer;
    FImportTreeOnly: Boolean;
    procedure CollectItem(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure SyncLoadOne;
    function PreloadAllowed: Boolean;
  public
    constructor Create(ASenderTree: TBaseVirtualTree; AParentNode: PVirtualNode);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  VirtualTree.Methods, NodeDataTypes.Base, AppConfig.Main, Kernel.Enumerations,
  Kernel.Logger, mormot.core.log, Kernel.Instance, SysUtils;

{ TTreeIconsThread }

constructor TTreeIconsThread.Create(ASenderTree: TBaseVirtualTree; AParentNode: PVirtualNode);
begin
  inherited Create(True);
  TASuiteLogger.Info('Start thread to get all icons', []);
  FItems := TList.Create;
  FIndex := -1;

  // Snapshot in the caller (main) thread: touching the tree here is safe,
  // touching it later from Execute would not be.
  if Assigned(ASenderTree) then
  begin
    FImportTreeOnly := Assigned(Config) and (Config.ASuiteState = lsImporting) and
      Assigned(ASuiteInstance) and (ASenderTree = ASuiteInstance.ImportTree);
    try
      ASenderTree.IterateSubtree(AParentNode, CollectItem, nil);
    except
      on E: Exception do
        TASuiteLogger.Exception(E, 'TTreeIconsThread snapshot failed');
    end;
  end;

  FreeOnTerminate := True;
end;

destructor TTreeIconsThread.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TTreeIconsThread.CollectItem(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.GetNodeItemData(Node, Sender);
  if Assigned(NodeData) then
    FItems.Add(NodeData);
end;

function TTreeIconsThread.PreloadAllowed: Boolean;
begin
  if not Assigned(Config) then
    Exit(False);
  Result := (Config.ASuiteState = lsNormal) or
    ((Config.ASuiteState = lsImporting) and FImportTreeOnly);
end;

procedure TTreeIconsThread.SyncLoadOne;
var
  NodeData: TvBaseNodeData;
  {%H-}log: ISynLog;
begin
  // Runs on the main thread via Synchronize.
  log := TASuiteLogger.Enter('TTreeIconsThread.SyncLoadOne', Self);
  if (FIndex < 0) or (FIndex >= FItems.Count) then
    Exit;
  if not PreloadAllowed then
    Exit;
  try
    NodeData := TvBaseNodeData(FItems[FIndex]);
    if Assigned(NodeData) and Assigned(NodeData.Icon) then
      NodeData.Icon.ImageIndex;
  except
    on E: Exception do
      TASuiteLogger.Exception(E, 'TTreeIconsThread.SyncLoadOne failed');
  end;
end;

procedure TTreeIconsThread.Execute;
var
  {%H-}log: ISynLog;
  I: Integer;
begin
  log := TASuiteLogger.Enter('TTreeIconsThread.Execute', Self);
  try
    for I := 0 to FItems.Count - 1 do
    begin
      if Terminated or not PreloadAllowed then
        Break;
      FIndex := I;
      try
        Synchronize(SyncLoadOne);
      except
        on E: Exception do
          TASuiteLogger.Exception(E, 'TTreeIconsThread.Execute failed');
      end;
    end;
  finally
    FIndex := -1;
  end;
end;

end.
