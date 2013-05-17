unit cyDbxSimpleTable;

{$I ..\Core\cyCompilerDefines.inc}

{   Component(s):
    TcySimpleDataset

    *************************** IMPORTANT ***************************
    *  TcySimpleDataset: some source code was copied from original  *
    *  Delphi TSimpleDataset component (unit 'SimpleDS').           *
    *****************************************************************


    TcyDbxSimpleTable

    Description:
    TcyDbxSimpleTable allows:
    - automatic server side updates in order to simplify SQL table records handling.
    - automatic refresh records from server after delete/post a record.

    How to retrieve AutoInc field value from server on Update record:
    procedure TForm1.cyDbxSimpleTable1InternalProviderAfterUpdateRecord(Sender: TObject; SourceDS: TDataSet; DeltaDS: TCustomClientDataSet;
      UpdateKind: TUpdateKind);
    begin
      // (Include poPropogateChanges to TDatasetProvider.Options)
      if UpdateKind = ukInsert then
        if not DeltaDS.FieldByName('yourPrimaryKey').ReadOnly then
          DeltaDS.FieldByName('yourPrimaryKey').NewValue := cyDBX.SQLConnection_GetLastInsertID(SQLConnection1);
    end;


    The purpose is to reproduce BDE TTable component behavior: no worry with server side updates/refresh.
    Because TcyDbxSimpleTable descends of TClientDataset, if you set CachedUpdates to true, TcyDbxSimpleTable behavior is equal to TClientDataset.

    *** Important notes of differences between TTable and MySQL handling ***
    - AutoInc fields handling: In TcyDbxSimpleTable, put "required" property to false
    - For fast updates and avoid reconcile errors, put ProviderOptions.UpdateMode to upWhereKeyOnly.
      Don' t forget to add [pfInKey] in ProviderFlags property to unique index field.


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

uses Classes, sysUtils, Dialogs, DB, DBClient, Provider, SqlConst, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} SimpleDS, SQLExpr,
      cyDBX, cyDbxBaseTable, cyDbxReconcileError;

type
  TcyDbxSimpleTableUpdatesOptions = class(TcyDbxBaseTableUpdatesOptions)
  private
  protected
  public
  published
    property MaxErrors;
    property SendResetAutoIncCmd;
  end;


  // In order do access DoAfterOpen :
  TcyInternalSQLDataSet = class(TInternalSQLDataSet)
  protected
    procedure DoAfterOpen; override;
  end;


  TcySimpleDataset = class(TCustomClientDataSet)
  private
    FConnection: TSQLConnection;
    FDataSet: TcyInternalSQLDataSet;
    FProvider: TDataSetProvider;
  protected
    procedure AllocDataSet; virtual;
    procedure AllocProvider; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure SetConnection(Value: TSQLConnection); virtual;
    {$IFDEF DELPHI2009_OR_ABOVE}
    { IProviderSupport }
    function PSGetCommandText: string; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  protected
    property Connection: TSQLConnection read FConnection write SetConnection;
    property DataSet: TcyInternalSQLDataSet read FDataSet;
    property Provider: TDatasetProvider read FProvider;
  published
    property Active;
    property Aggregates;
    property AggregatesActive;
    property AutoCalcFields;
    property CommandText;
    property ConnectionBroker;
    property Constraints;
    property DataSetField;
    property DisableStringTrim;
    property FileName;
    property Filter;
    property Filtered;
    property FilterOptions;
    property FieldDefs;
    property IndexDefs;
    property IndexFieldNames;
    property IndexName;
    property FetchOnDemand;
    property MasterFields;
    property MasterSource;
    property ObjectView;
    property PacketRecords;
    property Params;
    // property ProviderName;
    property ReadOnly;
    property RemoteServer;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
  end;


  TcyDbxSimpleTable = class(TcySimpleDataset)
  private
    FInserting: Boolean;
    FReleaseLockId: String;
    FQLock: TSQLQuery;
    //
    FBeforeApplyUpdatesInsertCount: Integer;
    FUpdatesOptions: TcyDbxSimpleTableUpdatesOptions;
    FCachedUpdates: Boolean;
    FDbxReconcileError: TcyDbxReconcileError;
    FLastErrorCount: Integer;
    FAutoRefreshOptions: TcyDbxAutoRefreshOptions;
    FRecordLockOptions: TcyDbxRecordLockOptions;
    FInternalRecordLocked: Boolean;
    procedure SetUpdatesOptions(const Value: TcyDbxSimpleTableUpdatesOptions);
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
    procedure DoAfterOpen; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterDelete; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure DoAfterCancel; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates(MaxErrors: Integer): Integer; override;
    procedure Refresh; // Because TDataset does not Refresh the provider's Query!
    procedure DefineDatasetFieldsProperties;
    property RecordLockId: String read GetInternalRecordLockId;
    property RecordLocked: Boolean read GetInternalRecordLocked;
    property LastErrorCount: Integer read FLastErrorCount;
  published
    // TcySimpleDataset :
    property Connection;
    property DataSet;
    property Provider;
    //
    property AutoRefreshOptions: TcyDbxAutoRefreshOptions read FAutoRefreshOptions write SetAutoRefreshOptions;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates default false;
    property DbxReconcileError: TcyDbxReconcileError read FDbxReconcileError write SetDbxReconcileError;
    property RecordLockOptions: TcyDbxRecordLockOptions read FRecordLockOptions write SetRecordLockOptions;
    property UpdatesOptions: TcyDbxSimpleTableUpdatesOptions read FUpdatesOptions write SetUpdatesOptions;
  end;


implementation

{ TcyInternalSQLDataSet }
procedure TcyInternalSQLDataSet.DoAfterOpen;
begin
  if Owner is TcyDbxSimpleTable then
    TcyDbxSimpleTable(Owner).DefineDatasetFieldsProperties;

  inherited;
end;


{ TcySimpleDataSet }
constructor TcySimpleDataSet.Create(AOwner: TComponent);
begin
  inherited;
  AllocProvider;
  AllocDataSet;
end;

procedure TcySimpleDataSet.AllocDataSet;
begin
  FDataSet := TcyInternalSQLDataSet.Create(Self);
  FDataSet.Name := 'InternalDataSet';                   { Do not localize }
  FDataSet.SQLConnection := FConnection;
  FDataSet.SetSubComponent(True);
  FProvider.DataSet := FDataSet;
end;

procedure TcySimpleDataSet.AllocProvider;
begin
  FProvider := TDataSetProvider.Create(Self);
  FProvider.DataSet := FDataSet;
  FProvider.Name := 'InternalProvider';                 { Do not localize }
  FProvider.SetSubComponent(True);
  SetProvider(FProvider);
end;

procedure TcySimpleDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if FConnection <> nil then
    if (Operation = opRemove) and (AComponent = FConnection) then
      FConnection := nil;

{ 20/11/2012
  if not (csDestroying in ComponentState ) and (Operation = opRemove) and
     (AComponent = FConnection) and (AComponent.Owner <> Self) then
    FConnection := Nil; }
end;

procedure TcySimpleDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if Assigned(FProvider) then
    SetProvider(FProvider);
  if FProvider.DataSet = Self then
    raise Exception.Create(SCircularProvider);
  inherited;
end;

procedure TcySimpleDataSet.SetConnection(Value: TSQLConnection);
begin
  if Value = FConnection then
    Exit;

  if Assigned(FConnection) then
    FConnection.RemoveFreeNotification(Self);

  FConnection := Value;

  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);

  FDataSet.SQLConnection := FConnection;
end;

{$IFDEF DELPHI2009_OR_ABOVE}
function TcySimpleDataSet.PSGetCommandText: string;
var
  IP: IProviderSupport;
begin
  if Supports(FDataSet, IProviderSupport, IP) then
    Result := IP.PSGetCommandText
  else
    Result := CommandText;
end;
{$ENDIF}


{ TcyDbxSimpleTable }
constructor TcyDbxSimpleTable.Create(AOwner: TComponent);
begin
  inherited;
  FReleaseLockId := '';
  FQLock := Nil;
  FCachedUpdates := false;
  FAutoRefreshOptions := TcyDbxAutoRefreshOptions.Create(self);
  FRecordLockOptions := TcyDbxRecordLockOptions.Create(Self);
  FUpdatesOptions := TcyDbxSimpleTableUpdatesOptions.Create(self);
  FLastErrorCount := 0;
  FBeforeApplyUpdatesInsertCount := 0;
end;

destructor TcyDbxSimpleTable.Destroy;
begin
  FAutoRefreshOptions.Free;
  FRecordLockOptions.Free;
  FUpdatesOptions.Free;
  inherited;
end;

procedure TcyDbxSimpleTable.SetAutoRefreshOptions(const Value: TcyDbxAutoRefreshOptions);
begin
  FAutoRefreshOptions := Value;
end;

procedure TcyDbxSimpleTable.SetCachedUpdates(const Value: Boolean);
begin
  FCachedUpdates := Value;

  // ApplyUpdates :
  if not FCachedUpdates then
    if Active then
      if ChangeCount <> 0 then
        FLastErrorCount := ApplyUpdates(FUpdatesOptions.MaxErrors);
end;

procedure TcyDbxSimpleTable.SetDbxReconcileError(const Value: TcyDbxReconcileError);
begin
  FDbxReconcileError := Value;

  if (Value <> nil) and (not  (csLoading in ComponentState)) then
    FDbxReconcileError.FreeNotification(Self);  // Inform TcyDbxSimpleTable if component removed ...
end;

procedure TcyDbxSimpleTable.SetRecordLockOptions(const Value: TcyDbxRecordLockOptions);
begin
  FRecordLockOptions := Value;
end;

procedure TcyDbxSimpleTable.SetUpdatesOptions(const Value: TcyDbxSimpleTableUpdatesOptions);
begin
  FUpdatesOptions := Value;
end;

procedure TcyDbxSimpleTable.Loaded;
begin
  inherited;
end;

procedure TcyDbxSimpleTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if FDbxReconcileError <> nil then
    if (Operation = opRemove) and (AComponent = FDbxReconcileError) then
      FDbxReconcileError := nil;
end;

procedure TcyDbxSimpleTable.DefineDatasetFieldsProperties;
var
  f: Integer;
  aQuery: TSQLQuery;
  TableName, PKName: String;
begin
  // Adjust providerFlags on Dataset:
  if not (csDesigning in ComponentState) then
    if FieldCount <> 0 then
    begin
      // *** Fields exists *** //

      // Apply Required + ProviderFlags to Dataset fields :
      for f := 0 to FieldCount-1 do
        if DataSet.FindField(Fields[f].FieldName) <> Nil then
          with DataSet.FindField(Fields[f].FieldName) do
          begin
            Required := Fields[f].Required;
            ProviderFlags := Fields[f].ProviderFlags;
          end;
    end
    else begin
      // *** Fields will herit Required and ProviderFlags values from Dataset *** //

      // Get table name :
      if DataSet.CommandType <> ctTable then
      begin
        TableName := ExtractSQLTableNames(DataSet.CommandText);
        if Pos(',', TableName) <> 0 then
          TableName := '';
      end
      else
        TableName := '`' + DataSet.CommandText + '`';

      if TableName <> '' then
      begin
        // Get table PRIMARY Key :
        aQuery := TSQLQuery.Create(Nil);
        aQuery.SQLConnection := Connection;
        aQuery.SQL.Clear;
        aQuery.SQL.Text := 'DESCRIBE ' + TableName;
        aQuery.Active := true;

        while not aQuery.Eof do
        begin
          if aQuery.FieldByName('KEY').AsString = 'PRI' then
          begin
            PKName := aQuery.FieldByName('FIELD').AsString;

            if Dataset.FindField(PKName) <> Nil then
              with Dataset.FindField(PKName) do
              begin
                ProviderFlags := ProviderFlags + [pfInKey];
                if aQuery.FieldByName('Extra').AsString = 'auto_increment' then
                begin
                  Required := false;
                  ProviderFlags := ProviderFlags - [pfInUpdate];
                end;
              end;

            Break;
          end;

          aQuery.Next;
        end;

        aQuery.Active := false;
        aQuery.Free;
      end;
    end;
end;

procedure TcyDbxSimpleTable.DoBeforeEdit;
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

procedure TcyDbxSimpleTable.DoBeforeOpen;
begin
  FBeforeApplyUpdatesInsertCount := 0;
  inherited;
end;

procedure TcyDbxSimpleTable.DoAfterOpen;
begin

  inherited;
end;

procedure TcyDbxSimpleTable.Refresh;
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

procedure TcyDbxSimpleTable.DbxReconcileErrorEvent(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
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

{ TcyDbxSimpleTable }
function TcyDbxSimpleTable.ApplyUpdates(MaxErrors: Integer): Integer;
var
  Remove: Boolean;
  TableName: String;
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

      if FDataSet.CommandType = ctTable
      then TableName := '`' + FDataSet.CommandText + '`'
      else TableName := cyDBX.ExtractSQLTablenames(FDataSet.CommandText, true);

      if TableName <> '' then
        Connection.Execute('ALTER TABLE ' + TableName + ' AUTO_INCREMENT = 1', nil, nil);
    end;

    Result := Inherited ApplyUpdates(MaxErrors);
  finally

  end;

  if Remove then
    OnReconcileError := Nil;
end;

procedure TcyDbxSimpleTable.InternalFirst;
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
procedure TcyDbxSimpleTable.InternalLast;
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
procedure TcyDbxSimpleTable.cyInternalRefreshRecords;
begin
  if ChangeCount <> 0 then Exit;

  UpdateCursorPos;
  try
    InternalRefresh;
  finally
    Resync([]);
  end;
end;

procedure TcyDbxSimpleTable.cyRefreshQuery;
begin
  if FProvider.DataSet.Active then
    FProvider.DataSet.Refresh;  // Refresh server side records
end;

procedure TcyDbxSimpleTable.DoAfterCancel;
begin
  if FReleaseLockId <> '' then
    InternalUnLock;

  inherited;
end;

procedure TcyDbxSimpleTable.DoAfterDelete;
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

procedure TcyDbxSimpleTable.DoBeforePost;
begin
  inherited;
  FInserting := State = dsInsert;
end;

procedure TcyDbxSimpleTable.DoAfterPost;
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

function TcyDbxSimpleTable.GetInternalRecordLockId: String;
var
  aDatabaseName, aTableName, aPrimaryKeyValue: String;
  aQuery: TSQLQuery;
begin
  Result := '';
  aPrimaryKeyValue := '';

  aDatabaseName := FConnection.Params.Values['Database'];
  if FDataSet.CommandType = ctTable
  then aTableName := FDataSet.CommandText
  else aTableName := cyDBX.ExtractSQLTablenames(FDataSet.CommandText, true);

  if aTableName <> '' then
  begin
    aPrimaryKeyValue := FieldByName( SQLGetPrimaryKey(FConnection, aTableName) ).AsString;
    Result := cyDBX.GetInternalRecordLockId(aDatabaseName, aTableName, aPrimaryKeyValue);
  end;
end;

function TcyDbxSimpleTable.GetInternalRecordLocked: Boolean;
begin
  Result := not cyDBX.IsFreeLockId(FConnection, GetInternalRecordLockId);
end;

function TcyDbxSimpleTable.InternalLock: Boolean;
begin
  Result := false;
  FReleaseLockId := GetInternalRecordLockId;

  if FReleaseLockId <> '' then
  begin
    if not Assigned(FQLock) then
      FQLock := TSQLQuery.Create(Self);

    FQLock.Active := false;
    FQLock.SQLConnection := FConnection;
    FQLock.SQL.Text := 'SELECT GET_LOCK(' + cyDBX.SQLGetStringExpr(FReleaseLockId) + ',' + intToStr(FRecordLockOptions.WaitSeconds) + ')';
    FQLock.Active := true;

    Result := FQLock.Fields[0].AsString = '1';
  end;
end;

procedure TcyDbxSimpleTable.InternalUnLock;
begin
  try
    FQLock.Active := false;   // No need to execute RELEASE_LOCK() because closing cursor will release the lock ...
  finally

  end;
  FReleaseLockId := '';
end;

// *** Problema de Resync *** //

// Se DatasetProvider.UpdateMode = upWhereKeyOnly :
// Ver quais são os campos utilizados para o update para poder posicionar-nos no registo certo ...

end.
