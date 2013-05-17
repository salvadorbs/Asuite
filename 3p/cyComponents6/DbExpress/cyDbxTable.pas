unit cyDbxTable;

{$I ..\Core\cyCompilerDefines.inc}

{   Component(s):
    tcyDbxTable

    Description:
    tcyDbxTable allows:
    - automatic server side updates in order to simplify SQL table records handling.
    - automatic refresh records from server after delete/post a record.

    How to retrieve AutoInc field value from server on Update record:
    procedure TForm1.DatasetProvider1AfterUpdateRecord(Sender: TObject; SourceDS: TDataSet; DeltaDS: TCustomClientDataSet;
      UpdateKind: TUpdateKind);
    begin
      // (Include poPropogateChanges to TDatasetProvider.Options)
      if UpdateKind = ukInsert then
        if not DeltaDS.FieldByName('yourPrimaryKey').ReadOnly then
          DeltaDS.FieldByName('yourPrimaryKey').NewValue := cyDBX.SQLConnection_GetLastInsertID(SQLConnection1);
    end;

    The purpose is to reproduce BDE TTable component behavior: no worry with server side updates/refresh.
    Because tcyDbxTable descends of TClientDataset, if you set CachedUpdates to true, tcyDbxTable behavior is equal to TClientDataset.

    *** Important notes of differences between TTable and MySQL handling ***
    - AutoInc fields handling: In both tcyDbxTable and associated provider's query, put "required" property to false
    - For fast updates and avoid reconcile errors, put TDatasetProvider.UpdateMode to upWhereKeyOnly.
      Don' t forget to add [pfInKey] in ProviderFlags property to associated provider's query unique index field.


    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

interface

uses Classes, SysUtils, Dialogs, DB, DBClient, SqlExpr, Provider, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} cyDBX, cyDbxBaseTable, cyDbxReconcileError;

type
  TcyDbxTable = class;

  TcyDbxTableUpdatesOptions = class(TcyDbxBaseTableUpdatesOptions)
  private
  protected
  public
  published
    property MaxErrors;
    property SendResetAutoIncCmd;
  end;

  TcyDbxTable = class(TClientDataset)
  private
    FActiveProvider: TDatasetProvider;
    FActiveConnection: TSQLConnection;
    FActiveTableName: String;
    FInserting: Boolean;
    FReleaseLockId: String;
    FQLock: TSQLQuery;
    //
    FBeforeApplyUpdatesInsertCount: Integer;
    FUpdatesOptions: TcyDbxTableUpdatesOptions;
    FCachedUpdates: Boolean;
    FDbxReconcileError: TcyDbxReconcileError;
    FLastErrorCount: Integer;
    FAutoRefreshOptions: TcyDbxAutoRefreshOptions;
    FRecordLockOptions: TcyDbxRecordLockOptions;
    procedure SetUpdatesOptions(const Value: TcyDbxTableUpdatesOptions);
    procedure SetDbxReconcileError(const Value: TcyDbxReconcileError);
    procedure DbxReconcileErrorEvent(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
    procedure cyInternalRefreshRecords;
    procedure cyRefreshQuery;
    procedure SetAutoRefreshOptions(const Value: TcyDbxAutoRefreshOptions);
    procedure SetCachedUpdates(const Value: Boolean);
    procedure SetRecordLockOptions(const Value: TcyDbxRecordLockOptions);
    function GetInternalRecordLockId: String;
    function GetInternalRecordLocked: Boolean;
    function InternalLock: Boolean;
    procedure InternalUnLock;
  protected
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterDelete; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure DoAfterCancel; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates(MaxErrors: Integer): Integer; override;
    procedure Refresh; // Because TDataset does not Refresh the provider's Query!
    property RecordLocked: Boolean read GetInternalRecordLocked;
    property RecordLockId: String read GetInternalRecordLockId;
    property LastErrorCount: Integer read FLastErrorCount;
  published
    property AutoRefreshOptions: TcyDbxAutoRefreshOptions read FAutoRefreshOptions write SetAutoRefreshOptions;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates default false;
    property DbxReconcileError: TcyDbxReconcileError read FDbxReconcileError write SetDbxReconcileError;
    property RecordLockOptions: TcyDbxRecordLockOptions read FRecordLockOptions write SetRecordLockOptions;
    property UpdatesOptions: TcyDbxTableUpdatesOptions read FUpdatesOptions write SetUpdatesOptions;
  end;


implementation

constructor TcyDbxTable.Create(AOwner: TComponent);
begin
  inherited;
  FReleaseLockId := '';
  FQLock := Nil;
  FCachedUpdates := false;
  FAutoRefreshOptions := TcyDbxAutoRefreshOptions.Create(self);
  FRecordLockOptions := TcyDbxRecordLockOptions.Create(Self);
  FUpdatesOptions := TcyDbxTableUpdatesOptions.Create(self);
  FLastErrorCount := 0;
  FBeforeApplyUpdatesInsertCount := 0;
end;

destructor TcyDbxTable.Destroy;
begin
  FAutoRefreshOptions.Free;
  FRecordLockOptions.Free;
  FUpdatesOptions.Free;
  inherited;
end;

procedure TcyDbxTable.SetAutoRefreshOptions(const Value: TcyDbxAutoRefreshOptions);
begin
  FAutoRefreshOptions := Value;
end;

procedure TcyDbxTable.SetCachedUpdates(const Value: Boolean);
begin
  FCachedUpdates := Value;

  // ApplyUpdates :
  if not FCachedUpdates then
    if Active then
      if ChangeCount <> 0 then
        FLastErrorCount := ApplyUpdates(FUpdatesOptions.MaxErrors);
end;

procedure TcyDbxTable.SetDbxReconcileError(const Value: TcyDbxReconcileError);
begin
  FDbxReconcileError := Value;

  if (Value <> nil) and (not  (csLoading in ComponentState)) then
    FDbxReconcileError.FreeNotification(Self);  // Inform TcyDbxTable if component removed ...
end;

procedure TcyDbxTable.SetRecordLockOptions(const Value: TcyDbxRecordLockOptions);
begin
  FRecordLockOptions := Value;
end;

procedure TcyDbxTable.SetUpdatesOptions(const Value: TcyDbxTableUpdatesOptions);
begin
  FUpdatesOptions := Value;
end;

procedure TcyDbxTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if FDbxReconcileError <> nil then
    if (Operation = opRemove) and (AComponent = FDbxReconcileError) then
      FDbxReconcileError := nil;
end;

procedure TcyDbxTable.Refresh;
begin
  DoBeforeRefresh;
  CheckBrowseMode;

  // Refresh the query before :
  cyRefreshQuery;
  UpdateCursorPos;
  try
    InternalRefresh;
  finally
    Resync([]);
    DoAfterRefresh;
  end;
end;

procedure TcyDbxTable.DbxReconcileErrorEvent(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
  // Action default value = raSkip
  // Delphi self handling : HandleReconcileError(DataSet, UpdateKind, E);

  case UpdateKind of
    ukInsert:
      begin
        if FDbxReconcileError.InsertMessage <> '' then
          MessageDlg(FDbxReconcileError.InsertMessage, mtError, [mbOk], 0);
        Action := FDbxReconcileError.InsertAction;
      end;

    ukModify:
      begin
        if FDbxReconcileError.ModifyMessage <> '' then
          MessageDlg(FDbxReconcileError.ModifyMessage, mtError, [mbOk], 0);
        Action := FDbxReconcileError.ModifyAction;
      end;

    ukDelete:
      begin
        if FDbxReconcileError.DeleteMessage <> '' then
          MessageDlg(FDbxReconcileError.DeleteMessage, mtError, [mbOk], 0);
        Action := FDbxReconcileError.DeleteAction;
      end;

    else
      MessageDlg('Not handled UpadeKind!', mtWarning, [mbOk], 0);
  end;

  // Erro ao alterar registo quando este está desactualizado:
  // Action := raSkip;     // Anula para o registo actual - necessita Rollback!
  // Action := raAbort;    // Anula para o registo actual e seguintes - necessita Rollback!
  // Action := raMerge;    // Altera sómente campos não alterados por autras conexões - Chama constantemente SimpleDataSet1ReconcileError!
  // Action := raCorrect;  // Altera o registo com dados no clientDataset - necessita Rollback!
  // Action := raCancel;   // Anula para o registo actual e retira alteração do delta - Mostra valores antes da alteração!
  // Action := raRefresh;  // Chama constantemente SimpleDataSet1ReconcileError!
end;

{ TcyDbxTable }
function TcyDbxTable.ApplyUpdates(MaxErrors: Integer): Integer;
var
  Remove: Boolean;
begin
  Result := 0;
  if ChangeCount = 0 then Exit;
  Remove := false;

  if not Assigned(OnReconcileError) then  // No event assigned on component
    if Assigned(FDbxReconcileError) then  // Any TcyDbxReconcileError component assigned?
    begin
      Remove := true;
      OnReconcileError := DbxReconcileErrorEvent;
    end;

  try
    // Reset AUTO_INCREMENT before ApplyUpdates :
    if FUpdatesOptions.SendResetAutoIncCmd and (FBeforeApplyUpdatesInsertCount <> 0) then
    begin
      FBeforeApplyUpdatesInsertCount := 0;

      if FActiveConnection <> nil then
        if FActiveTableName <> '' then
          FActiveConnection.Execute('ALTER TABLE ' + FActiveTableName + ' AUTO_INCREMENT = 1', nil, nil);
    end;

    Result := Inherited ApplyUpdates(MaxErrors);
  finally

  end;

  if Remove then
    OnReconcileError := Nil;
end;

procedure TcyDbxTable.InternalFirst;
begin
  inherited;

  // Refresh dataset and query (must be after moving to first record) :
  if not CachedUpdates then
    if (PacketRecords = 0) and (Assigned(MasterSource)) then
    begin
      // Stack overflow error in Delphi 2009 (not tested on other versions) :
      PacketRecords := -1;
    end
      else
        if FAutoRefreshOptions.MovingToFirst then
          if State = dsBrowse then     // Can be inserting new record
          begin
            cyRefreshQuery;
            InternalRefresh;
          end;
end;

// Also called when inserting a new record at the end of the dataset
procedure TcyDbxTable.InternalLast;
begin
  // Refresh dataset and query before (must be before moving to last row) :
  if not CachedUpdates then
    if FAutoRefreshOptions.MovingToLast then
      if State = dsBrowse then     // Can be inserting new record
      begin
        cyRefreshQuery;
        InternalRefresh;
      end;

  inherited;
end;

// Refresh without calling BeforeRefresh and AfterRefresh:
procedure TcyDbxTable.cyInternalRefreshRecords;
begin
  if ChangeCount <> 0 then Exit;

  UpdateCursorPos;
  try
    InternalRefresh;
  finally
    Resync([]);
  end;
end;

procedure TcyDbxTable.cyRefreshQuery;
var aProvider: TDatasetProvider;
begin
  aProvider := ClientDS_GetProvider(Self);

  if aProvider <> Nil then
    if aProvider.DataSet.Active then
      aProvider.DataSet.Refresh;  // Refresh server side records
end;

procedure TcyDbxTable.DoAfterCancel;
begin
  if FReleaseLockId <> '' then
    InternalUnLock;

  inherited;
end;

procedure TcyDbxTable.DoAfterDelete;
begin
  // Apply updates :
  if not CachedUpdates then
    if ChangeCount <> 0 then
    begin
      FLastErrorCount := ApplyUpdates(FUpdatesOptions.MaxErrors);
      // RHR - se entretanto, foi apagado um registo mais acima, a tabela não se posicionará no registo seguinte mas sim 2 seguintes ...

      // Refresh dataset and query
      if ChangeCount = 0 then
        if FAutoRefreshOptions.AfterDelete then
        begin
          // Not working here : InternalRefresh;
          cyRefreshQuery;
          cyInternalRefreshRecords;
        end;
    end;

  inherited;
end;

procedure TcyDbxTable.DoAfterOpen;
begin
  inherited;

  // Save some information:
  FActiveConnection := Nil;
  FActiveProvider := ClientDS_GetProvider(Self);
  FActiveTableName := '';

  if FActiveProvider <> Nil then
    if FActiveProvider.DataSet <> Nil then
      if FActiveProvider.DataSet is TCustomSQLDataSet then
      begin
        FActiveConnection := TCustomSQLDataSet(FActiveProvider.DataSet).SQLConnection;
        FActiveTableName := '';

        if FActiveProvider.DataSet is TSQLTable then
          FActiveTableName := TSQLTable(FActiveProvider.DataSet).TableName
        else
          if FActiveProvider.DataSet is TSQLQuery then
            FActiveTableName := cyDBX.ExtractSQLTablenames(TSQLQuery(FActiveProvider.DataSet).SQL.Text, true)
          else
            if FActiveProvider.DataSet is TSQLDataset then
              with TSQLDataset(FActiveProvider.DataSet) do
              begin
                if CommandType = ctTable
                then FActiveTableName := '`' + CommandText + '`'
                else FActiveTableName := cyDBX.ExtractSQLTablenames(CommandText, true);
              end;
      end;
end;

procedure TcyDbxTable.DoBeforeEdit;
var LockSuccess: Boolean;
begin
  inherited;

  if FRecordLockOptions.Enabled then
  begin
    LockSuccess := false;
    if not GetInternalRecordLocked then
      LockSuccess := InternalLock;

    if not LockSuccess then
      raise Exception.Create(vDbxErrorRecordLocked);
  end;
end;

procedure TcyDbxTable.DoBeforeOpen;
begin
  FBeforeApplyUpdatesInsertCount := 0;
  inherited;
end;

procedure TcyDbxTable.DoBeforePost;
begin
  inherited;
  FInserting := State = dsInsert;
end;

procedure TcyDbxTable.DoAfterPost;
begin
  if FReleaseLockId <> '' then
    InternalUnLock;

  if FInserting then
    Inc(FBeforeApplyUpdatesInsertCount);

  // Apply updates :
  if not CachedUpdates then
    if ChangeCount <> 0 then
    begin
      FLastErrorCount := ApplyUpdates(FUpdatesOptions.MaxErrors);

      // RHR - Modo edição: pode não estar no registo certo se entretanto foram inseridos/apagados outros ...
      // RHR - Modo Inserção: pode não estar no registo certo se entretanto foram inseridos outros ...

      // Refresh dataset and query :
      if ChangeCount = 0 then
      begin
        if FInserting then
        begin
          case FAutoRefreshOptions.AfterPostNewRecord of
            rtTable:
              begin
                cyRefreshQuery;
                cyInternalRefreshRecords;
              end;

            rtRecord:
            begin
              cyRefreshQuery;           // Refreshing single record not avaible
              RefreshRecord;            // Raise error if not key specified!
            end;
          end;
        end
        else
          case FAutoRefreshOptions.AfterPostModifiedRecord of
            rtTable:
            begin
              cyRefreshQuery;
              cyInternalRefreshRecords;
            end;

            rtRecord:
            begin
              cyRefreshQuery;           // Refreshing single record not avaible
              RefreshRecord;            // Raise error if not key specified!
            end;
          end;
      end;
    end;

  FInserting := false;

  inherited;   // Will call OnAfterPost event ...
end;

function TcyDbxTable.GetInternalRecordLockId: String;
var
  aDatabaseName, aTableName, aPrimaryKeyValue: String;
begin
  Result := '';
  aPrimaryKeyValue := '';

  aDatabaseName := FActiveConnection.Params.Values['Database'];
  aTableName := FActiveTableName;

  if aTableName <> '' then
  begin
    aPrimaryKeyValue := FieldByName( SQLGetPrimaryKey(FActiveConnection, aTableName) ).AsString;
    Result := cyDBX.GetInternalRecordLockId(aDatabaseName, aTableName, aPrimaryKeyValue);
  end;
end;

function TcyDbxTable.GetInternalRecordLocked: Boolean;
begin
  Result := not cyDBX.IsFreeLockId(FActiveConnection, GetInternalRecordLockId);
end;

function TcyDbxTable.InternalLock: Boolean;
begin
  Result := false;
  FReleaseLockId := GetInternalRecordLockId;

  if FReleaseLockId <> '' then
  begin
    if not Assigned(FQLock) then
      FQLock := TSQLQuery.Create(Self);

    FQLock.Active := false;
    FQLock.SQLConnection := FActiveConnection;
    FQLock.SQL.Text := 'SELECT GET_LOCK(' + cyDBX.SQLGetStringExpr(FReleaseLockId) + ',' + intToStr(FRecordLockOptions.WaitSeconds) + ')';
    FQLock.Active := true;

    Result := FQLock.Fields[0].AsString = '1';
  end;
end;

procedure TcyDbxTable.InternalUnLock;
begin
  try
    FQLock.Active := false;   // No need to execute RELEASE_LOCK() because closing cursor release the lock ...
  finally

  end;
  FReleaseLockId := '';
end;

// *** Problema de Resync *** //

// Se DatasetProvider.UpdateMode = upWhereKeyOnly :
// Ver quais são os campos utilizados para o update para poder posicionar-nos no registo certo ...

// show index from TABLE where Key_name = 'PRIMARY' ;

end.
