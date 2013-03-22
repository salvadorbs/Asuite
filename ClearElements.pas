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

unit ClearElements;

{$MODE Delphi}

interface

uses
  Forms, StdCtrls, ExtCtrls, VirtualTrees, AppConfig, FileUtil;

type
  TfrmClearElements = class(TForm)
    lbClearElements: TLabel;
    cbBackup: TCheckBox;
    cbCache: TCheckBox;
    cbRecents: TCheckBox;
    btnClear: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    cbMFU: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure ClearCache(Sender: TBaseVirtualTree; Node: PVirtualNode;
                         Data: Pointer; var Abort: Boolean);
    procedure ClearMFU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    procedure ClearMRU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmClearElements: TfrmClearElements;

implementation   

uses Main, ulTreeView, ulNodeDataTypes, ulSysUtils;

{$R *.lfm}

procedure TfrmClearElements.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmClearElements.btnClearClick(Sender: TObject);
begin
  //Clear MRU
  if cbRecents.Checked then
  begin
    MRUList.Clear;
    frmMain.vstList.IterateSubtree(nil, ClearMRU, nil, [], True);
  end;
  //Clear MFU
  if cbMFU.Checked then
  begin
    MFUList.Clear;
    frmMain.vstList.IterateSubtree(nil, ClearMFU, nil, [], True);
  end;
  //Clear Backup
  if cbBackup.Checked then
    DeleteFiles(SUITE_BACKUP_PATH,'ASuite_*' + EXT_SQLBCK);
  //Clear Cache
  if cbCache.Checked then
    frmMain.vstList.IterateSubtree(nil, ClearCache, nil, [], True);
  RefreshList(frmMain.vstList);
  Close;
end;

procedure TfrmClearElements.ClearCache(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  CurrentNodeData : PBaseData;
begin
  CurrentNodeData := Sender.GetNodeData(Node);
  with CurrentNodeData.Data do
  begin
    if (CacheID <> -1) then
    begin
      if FileExistsUTF8(PathCacheIcon) then
        DeleteFileUTF8(PathCacheIcon); 
      CacheID := -1;
      Changed := True;
    end;
  end;
end;

procedure TfrmClearElements.ClearMRU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := PBaseData(Sender.GetNodeData(Node)).Data;
  NodeData.MRUPosition := -1;
  NodeData.Changed := True;
end;

procedure TfrmClearElements.ClearMFU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := PBaseData(Sender.GetNodeData(Node)).Data;
  NodeData.ClickCount := 0;
  NodeData.Changed := True;
end;

procedure TfrmClearElements.FormCreate(Sender: TObject);
begin

end;

end.
