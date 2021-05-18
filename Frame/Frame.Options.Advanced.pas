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

unit Frame.Options.Advanced;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Variants, Controls, Dialogs, Frame.BaseEntity, VirtualTrees,
  StdCtrls, ExtCtrls, Spin, DefaultTranslator;

type

  { TfrmAdvancedOptionsPage }

  TfrmAdvancedOptionsPage = class(TfrmBaseEntityPage)
    cbCache: TCheckBox;
    cbMFU: TCheckBox;
    cbScheduler: TCheckBox;
    gbMFU: TGroupBox;
    gbOtherFunctions: TGroupBox;
    lbMaxMFU: TLabel;
    
    pnlLeft: TPanel;
    gbRecents: TGroupBox;
    lbMaxMRU: TLabel;
    cbMRU: TCheckBox;
    pnlRight: TPanel;
    grpClearElements: TGroupBox;
    lbClearElements: TLabel;
    cbClearMFU: TCheckBox;
    cbClearBackup: TCheckBox;
    cbClearCache: TCheckBox;
    btnClear: TButton;
    cbClearMRU: TCheckBox;
    seMFU: TSpinEdit;
    seRecents: TSpinEdit;
    gbBackup: TGroupBox;
    lbMaxBackup: TLabel;
    cbBackup: TCheckBox;
    seBackup: TSpinEdit;
    procedure btnClearClick(Sender: TObject);
    procedure UpdateBtnClear(Sender: TObject);
    procedure cbMRUClick(Sender: TObject);
    procedure cbMFUClick(Sender: TObject);
    procedure cbBackupClick(Sender: TObject);
  private
    { Private declarations }
    procedure ClearCache(Sender: TBaseVirtualTree; Node: PVirtualNode;
                         Data: Pointer; var Abort: Boolean);
    procedure ClearMFU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
    procedure ClearMRU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                       Data: Pointer; var Abort: Boolean);
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmAdvancedOptionsPage: TfrmAdvancedOptionsPage;

implementation

uses
  AppConfig.Main, Utility.FileFolder, Kernel.Consts, VirtualTree.Methods,
  NodeDataTypes.Files, NodeDataTypes.Custom, Kernel.ResourceStrings,
  Utility.Misc, Kernel.Instance, Kernel.Manager;

{$R *.lfm}

{ TfrmAdvancedOptionsPage }

procedure TfrmAdvancedOptionsPage.btnClearClick(Sender: TObject);
begin
  try
    //Clear MRU
    if cbClearMRU.Checked then
    begin
      ASuiteManager.ListManager.MRUList.Clear;
      ASuiteInstance.MainTree.IterateSubtree(nil, ClearMRU, nil);
      cbClearMRU.Checked := False;
    end;
    //Clear MFU
    if cbClearMFU.Checked then
    begin
      ASuiteManager.ListManager.MFUList.Clear;
      ASuiteInstance.MainTree.IterateSubtree(nil, ClearMFU, nil);
      cbClearMFU.Checked := False;
    end;
    //Clear Backup
    if cbClearBackup.Checked then
    begin
      DeleteFiles(ASuiteInstance.Paths.SuitePathBackup, APP_NAME + '_*' + EXT_SQLBCK);
      cbClearBackup.Checked := False;
    end;
    //Clear Cache
    if cbClearCache.Checked then
    begin
      ASuiteInstance.MainTree.FullCollapse;
      ASuiteInstance.MainTree.IterateSubtree(nil, ClearCache, nil);
      cbClearCache.Checked := False;
    end;
  finally
    ShowMessageEx(msgOperationCompleted);
    //Save list
    TVirtualTreeMethods.RefreshList(nil);
  end;
end;

procedure TfrmAdvancedOptionsPage.cbBackupClick(Sender: TObject);
begin
  seBackup.Enabled := cbBackup.Checked;
end;

procedure TfrmAdvancedOptionsPage.cbMFUClick(Sender: TObject);
begin
  seMFU.Enabled := cbMFU.Checked
end;

procedure TfrmAdvancedOptionsPage.cbMRUClick(Sender: TObject);
begin
  seRecents.Enabled := cbMRU.Checked
end;

procedure TfrmAdvancedOptionsPage.ClearCache(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  CurrentNodeData : TvCustomRealNodeData;
begin
  CurrentNodeData := TvCustomRealNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Sender));
  if not(CurrentNodeData.IsSeparatorItem) then
    CurrentNodeData.Icon.ResetIcon;
  CurrentNodeData.Changed := True;
end;

procedure TfrmAdvancedOptionsPage.ClearMFU(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData : TvFileNodeData;
begin
  NodeData := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Sender));
  NodeData.ClickCount := 0;
  NodeData.Changed := True;
end;

procedure TfrmAdvancedOptionsPage.ClearMRU(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData : TvFileNodeData;
begin
  NodeData := TvFileNodeData(TVirtualTreeMethods.GetNodeItemData(Node, Sender));
  NodeData.LastAccess := -1;
  NodeData.Changed := True;
end;

function TfrmAdvancedOptionsPage.GetImageIndex: Integer;
begin
  Result := ASuiteManager.IconsManager.GetIconIndex('advanced');
end;

function TfrmAdvancedOptionsPage.GetTitle: string;
begin
  Result := msgAdvanced;
end;

function TfrmAdvancedOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  //MRU
  cbMRU.Checked       := Config.MRU;
  seRecents.Value     := Config.MRUNumber;
  //MFU
  cbMFU.Checked       := Config.MFU;
  seMFU.Value         := Config.MFUNumber;
  //Backup
  cbBackup.Checked    := Config.Backup;
  seBackup.Value      := Config.BackupNumber;
  //Other functions
  cbCache.Checked     := Config.Cache;
  cbScheduler.Checked := Config.Scheduler;
  //Enable/disable visual components
  UpdateBtnClear(Self);
  cbBackupClick(Self);
  cbMFUClick(Self);
  cbMRUClick(Self);
end;

function TfrmAdvancedOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  //MRU
  Config.MRU       := cbMRU.Checked;
  Config.MRUNumber := seRecents.Value;
  //MFU
  Config.MFU       := cbMFU.Checked;
  Config.MFUNumber := seMFU.Value;
  //Backup
  Config.Backup    := cbBackup.Checked;
  Config.BackupNumber := seBackup.Value;
  //Other functions
  //Refresh icons, if user change cache settings
  if (Config.Cache <> cbCache.Checked) then
    Config.Cache   := cbCache.Checked;
  Config.Scheduler := cbScheduler.Checked;
end;

procedure TfrmAdvancedOptionsPage.UpdateBtnClear(Sender: TObject);
begin
  btnClear.Enabled := cbClearMFU.Checked or cbClearMRU.Checked or cbClearBackup.Checked or
                      cbClearCache.Checked;
end;

end.
