unit Frame.Options.Advanced;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BaseEntityPage, Vcl.StdCtrls,
  Vcl.ComCtrls, VirtualTrees, DKLang, Frame.BaseEntity;

type
  TfrmAdvancedOptionsPage = class(TfrmBaseEntityPage)
    gbRecents: TGroupBox;
    lbMaxMRU: TLabel;
    lbNumbMRU: TLabel;
    cbMRU: TCheckBox;
    tbMRU: TTrackBar;
    gbMFU: TGroupBox;
    lbMaxMFU: TLabel;
    lbNumbMFU: TLabel;
    cbMFU: TCheckBox;
    tbMFU: TTrackBar;
    gbBackup: TGroupBox;
    lbMaxBackup: TLabel;
    lbNumbBackup: TLabel;
    cbBackup: TCheckBox;
    tbBackup: TTrackBar;
    grpClearElements: TGroupBox;
    lbClearElements: TLabel;
    cbRecents: TCheckBox;
    cbClearMFU: TCheckBox;
    cbClearBackup: TCheckBox;
    cbClearCache: TCheckBox;
    btnClear: TButton;
    gbOtherFunctions: TGroupBox;
    cbCache: TCheckBox;
    cbScheduler: TCheckBox;
    cbClearMRU: TCheckBox;
    DKLanguageController1: TDKLanguageController;
    procedure btnClearClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
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
    procedure RefreshCache(Sender: TBaseVirtualTree);
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
  Main, ulTreeView, ulNodeDataTypes, ulFileFolder, ulEnumerations, udImages,
  AppConfig, ulAppConfig;

{$R *.dfm}

{ TfrmAdvancedOptionsPage }

procedure TfrmAdvancedOptionsPage.UpdateBtnClear(Sender: TObject);
begin
  btnClear.Enabled := cbClearMFU.Checked or cbClearMRU.Checked or cbClearBackup.Checked or
                      cbClearCache.Checked;
end;

procedure TfrmAdvancedOptionsPage.cbBackupClick(Sender: TObject);
begin
  tbBackup.Enabled := cbBackup.Checked;
end;

procedure TfrmAdvancedOptionsPage.cbMFUClick(Sender: TObject);
begin
  tbMFU.Enabled := cbMFU.Checked
end;

procedure TfrmAdvancedOptionsPage.cbMRUClick(Sender: TObject);
begin
  tbMRU.Enabled := cbMRU.Checked
end;

procedure TfrmAdvancedOptionsPage.ClearCache(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  CurrentNodeData : TvCustomRealNodeData;
begin
  CurrentNodeData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  if CurrentNodeData.DataType <> vtdtSeparator then
  begin
    CurrentNodeData.CacheID      := -1;
    CurrentNodeData.CacheLargeID := -1;
  end;
end;

procedure TfrmAdvancedOptionsPage.RefreshCache(Sender: TBaseVirtualTree);
begin
  Sender.IterateSubtree(nil, ClearCache, nil, [], True);
  ImagesDM.GetChildNodesIcons(Sender, nil, Sender.RootNode);
  Sender.FullCollapse;
end;

procedure TfrmAdvancedOptionsPage.ClearMRU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  NodeData : TvCustomRealNodeData;
begin
  NodeData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  NodeData.MRUPosition := -1;
  NodeData.Changed := True;
end;

procedure TfrmAdvancedOptionsPage.ClearMFU(Sender: TBaseVirtualTree; Node: PVirtualNode;
                            Data: Pointer; var Abort: Boolean);
var
  NodeData : TvCustomRealNodeData;
begin
  NodeData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(Node)).Data);
  NodeData.ClickCount := 0;
  NodeData.Changed := True;
end;


procedure TfrmAdvancedOptionsPage.btnClearClick(Sender: TObject);
begin
  //Clear MRU
  if cbClearMRU.Checked then
  begin
    MRUList.Clear;
    frmMain.vstList.IterateSubtree(nil, ClearMRU, nil, [], True);
  end;
  //Clear MFU
  if cbClearMFU.Checked then
  begin
    MFUList.Clear;
    frmMain.vstList.IterateSubtree(nil, ClearMFU, nil, [], True);
  end;
  //Clear Backup
  if cbClearBackup.Checked then
    DeleteFiles(SUITE_BACKUP_PATH, APP_NAME + '_*' + EXT_SQLBCK);
  //Clear Cache
  if cbClearCache.Checked then
    RefreshCache(frmMain.vstList);
  RefreshList(frmMain.vstList);
end;

function TfrmAdvancedOptionsPage.GetImageIndex: Integer;
begin
  Result := IMAGELARGE_INDEX_Advanced;
end;

function TfrmAdvancedOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgAdvanced');
end;

function TfrmAdvancedOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  //MRU
  cbMRU.Checked        := Config.MRU;
  tbMRU.position       := Config.MRUNumber;
  lbNumbMRU.Caption    := IntToStr(Config.MRUNumber);
  //MFU
  cbMFU.Checked        := Config.MFU;
  tbMFU.position       := Config.MFUNumber;
  lbNumbMFU.Caption    := IntToStr(Config.MFUNumber);
  //Backup
  cbBackup.Checked     := Config.Backup;
  tbBackup.position    := Config.BackupNumber;
  lbNumbBackup.Caption := IntToStr(Config.BackupNumber);
  //Other functions
  cbCache.Checked      := Config.Cache;
  cbScheduler.Checked  := Config.Scheduler;
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
  Config.MRUNumber := tbMRU.Position;
  //MFU
  Config.MFU       := cbMFU.Checked;
  Config.MFUNumber := tbMFU.Position;
  //Backup
  Config.Backup    := cbBackup.Checked;
  Config.BackupNumber := tbBackup.Position;
  //Other functions
  //Refresh icons, if user change cache settings
  if (Config.Cache <> cbCache.Checked) then
  begin
    Config.Cache   := cbCache.Checked;
    RefreshCache(frmMain.vstList);
  end;
  Config.Scheduler := cbScheduler.Checked;
end;

procedure TfrmAdvancedOptionsPage.TrackBarChange(Sender: TObject);
begin
  if Sender = tbMRU then
    lbNumbMRU.Caption := IntToStr(tbMRU.position)
  else
    if Sender = tbMFU then
      lbNumbMFU.Caption := IntToStr(tbMFU.position)
    else
      if Sender = tbBackup then
        lbNumbBackup.Caption := IntToStr(tbBackup.position);
end;

end.
