unit cyDBX;

{   Unit cyDBX

    Description:
    Unit with ClientDataset/DB Express functions

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

{$I ..\Core\cyCompilerDefines.inc}

interface

uses
  {$IFDEF MsWindows}
  Dialogs,
  {$ELSE}
  fmx.Dialogs, System.UITypes,
  {$ENDIF}

  Classes, Variants, SimpleDS, DB, DBClient, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} Provider, SysUtils, SqlExpr;

var
  vDbxErrorRecordLocked: String;  // Set on initialization section!

const
  cDbxErrorServerInsert = 'Error! cannot insert record!';
  cDbxErrorServerModify   = 'Error! cannot modify: record modified or deleted by another connection!';
  cDbxErrorServerDelete = 'Error! cannot delete: record modified or deleted by another connection!';

type
  TDbxIndexType = (itNone, itPrimaryKey, itUnique, itIndexed);

type TDatasetProviderDefaultErrorHandler = class
public
  procedure ReconcileError(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind;
    var Action: TReconcileAction);
end;


{Utilities}
function BackQuotedStr(aIdentifier: String): String;
function ExtractSQLTableNames(fromSQL: String; const OnlyFirstFound: Boolean = false): String;
function ExtractSQLFields(fromSQL: String): String;
function ExtractSQLOrderByFields(fromSQL: String): String;
// Get lock string id for mutex lock :
function GetInternalRecordLockId(aDatabaseName, aTableName, aPrimaryKeyValue: String): String;
// Know if LockId is in use :
function IsFreeLockId(aConnection: TSQLConnection; aLockId: String): Boolean;

{SQL Expressions}
function SQLGetStringExpr(Value: String): String;
function SQLGetDateExpr(Value: TDateTime; const SQLDateFormat: String = 'YYYY-MM-DD'): String;
function SQLGetFloatExpr(Value: Extended): String;


{SQL Parameters}
function SQLGetDateParam(ParamValue: TDateTime; const SQLDateFormat: String = 'YYYY-MM-DD'): String;
function SQLGetFloatParam(ParamValue: Extended): String;


{SQL commands}
// Execute commands (return number of rows affected) :
function SQLExecute(aConnection: TSQLConnection; SQL: TStrings): Integer; overload;
function SQLExecute(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer; overload;

{$IFDEF DELPHI2007_OR_ABOVE}
// Specify an Insert command and the function will retrieve the Primary key of the inserted record:
function SQLExecuteInsert(aConnection: TSQLConnection; SQL: TStrings): String; overload;
function SQLExecuteInsert(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): String; overload;
{$ENDIF}

function SQLGetPrimaryKey(aConnection: TSQLConnection; aTableName: String): String;

// Open query and returns first result field value:
function SQLReturnFieldValue(aConnection: TSQLConnection; SQL: TStrings): Variant; overload;
function SQLReturnFieldValue(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Variant; overload;
function SQLReturnFieldValueAsString(aConnection: TSQLConnection; SQL: TStrings): string; overload;
function SQLReturnFieldValueAsString(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): String; overload; overload;
function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; SQL: TStrings): Integer; overload;
function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer; overload; overload;
// Get last generated AutoInc value :
function SQLExecute_GetLastInsertID(aConnection: TSQLConnection; const SQLCommand: String = 'SELECT last_insert_id()'): String;
{ You need to include [poPropogateChanges] to TDatasetProvider.Options and call this function on TDatasetProvider.AfterUpdateRecord like this:
  if UpdateKind = ukInsert then
    DeltaDS.FieldByName('YourIndexFieldName').NewValue := SQLExecute_GetLastInsertID(SqlConnection1);
}



{TSimpleDataset functions}
// Apply updates and refresh SimpleDataset:
procedure SimpleDS_Refresh(aSimpleDataSet: TSimpleDataSet; const ApplyUpdates: Boolean = true);
// Apply avaible updates :
function SimpleDS_ApplyUpdates(aSimpleDataSet: TSimpleDataSet; const MaxErrors: Integer = -1): Integer;



{TClientDataset functions}
// Apply updates and refresh ClientDataset:
function ClientDS_Refresh(aClientDataSet: TClientDataSet; const ApplyUpdates: Boolean = true): Integer; overload;
function ClientDS_Refresh(aClientDataSet: TClientDataSet; aProvider: TDatasetProvider; const ApplyUpdates: Boolean = true): Integer; overload;
// Apply avaible updates :
function ClientDS_ApplyUpdates(aClientDataSet: TClientDataSet; const MaxErrors: Integer = -1): Integer;
// Get ClientDataset's TDatasetProvider :
function ClientDS_GetProvider(aClientDataSet: TCustomClientDataset): TDatasetProvider;

implementation

{ TDatasetProviderDefaultErrorHandler }
procedure TDatasetProviderDefaultErrorHandler.ReconcileError(DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
  // Action default value = raSkip
  // Delphi self handling with : HandleReconcileError(DataSet, UpdateKind, E);

  case UpdateKind of
    ukInsert:
      begin
        {$IFDEF MsWindows}
        MessageDlg(cDbxErrorServerInsert, mtError, [mbOk], 0);
        {$ELSE}
        MessageDlg(cDbxErrorServerInsert, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
        {$ENDIF}

        Action := raCancel;
      end;

    ukModify:
      begin
        {$IFDEF MsWindows}
        MessageDlg(cDbxErrorServerModify, mtError, [mbOk], 0);
        {$ELSE}
        MessageDlg(cDbxErrorServerModify, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
        {$ENDIF}

        Action := raCancel;
      end;

    ukDelete:
      begin
        {$IFDEF MsWindows}
        MessageDlg(cDbxErrorServerDelete, mtError, [mbOk], 0);
        {$ELSE}
        MessageDlg(cDbxErrorServerDelete, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
        {$ENDIF}
        Action := raCancel;
      end;

    else
      ShowMessage('Not handled UpadeKind!');
  end;

  // Erro ao alterar registo quando este está desactualizado:
  // Action := raSkip;     // Anula para o registo actual - necessita Rollback!
  // Action := raAbort;    // Anula para o registo actual e seguintes - necessita Rollback!
  // Action := raMerge;    // Altera sómente campos não alterados por autras conexões - Chama constantemente SimpleDataSet1ReconcileError!
  // Action := raCorrect;  // Altera o registo com dados no clientDataset - necessita Rollback!
  // Action := raCancel;   // Anula para o registo actual e retira alteração do delta - Mostra valores antes da alteração!
  // Action := raRefresh;  // Chama constantemente SimpleDataSet1ReconcileError!
end;

function SQLExecute_GetLastInsertID(aConnection: TSQLConnection; const SQLCommand: String = 'SELECT last_insert_id()'): String;
var aQuery: TSQLQuery;
begin
  Result := '';
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.Text := SQLCommand;
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsString;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function BackQuotedStr(aIdentifier: String): String;
begin
  Result := aIdentifier;
  if Result = '' then
    Exit;

  if Result[1] <> '`' then
    Result := '`' + Result;

  if Result[Length(Result)] <> '`' then
    Result := Result + '`';
end;

function ExtractSQLTableNames(fromSQL: String; const OnlyFirstFound: Boolean = false): String;
var
  lengthFromSQL, i, j, p: Integer;
  Cont: Boolean;
begin
  Result := '';

  // No worry with enters:
  fromSQL := StringReplace(fromSQL, #$D#$A, ' ', [rfReplaceAll]);
  lengthFromSQL := length(fromSQL);

  // Locate FROM statement :
  p := pos(' FROM ', AnsiUpperCase(fromSQL));

  // Extract tablenames :
  if p = 0 then Exit;

  for i := p + 6 to lengthFromSQL do
    if fromSQL[i] = ' ' then
    begin
      // Check if there' s more any table :
      Cont := false;
      for j := i + 1 to lengthFromSQL do
        if not OnlyFirstFound then
          if fromSQL[j] <> ' ' then
          begin
            Cont := fromSQL[j] = ',';  // There' s more!
            Break;
          end;

      if not Cont then
        Break;
    end
    else
      Result := Result + fromSQL[i];
end;

function ExtractSQLFields(fromSQL: String): String;
var
  lengthFromSQL, i, j, p: Integer;
  Cont: Boolean;
begin
  Result := '';

  // No worry with enters:
  fromSQL := StringReplace(fromSQL, #$D#$A, ' ', [rfReplaceAll]);
  lengthFromSQL := length(fromSQL);

  // Locate SELECT statement :
  p := pos('SELECT ', AnsiUpperCase(fromSQL));

  // Extract fields :
  if p = 0 then Exit;

  for i := p + 7 to lengthFromSQL do
    if fromSQL[i] = ' ' then
    begin
      // Check if there' s more any field :
      Cont := false;
      for j := i + 1 to lengthFromSQL do
        if fromSQL[j] <> ' ' then
        begin
          Cont := fromSQL[j] = ',';  // There' s more!
          Break;
        end;

      if not Cont then
        Break;
    end
    else
      Result := Result + fromSQL[i];
end;

function ExtractSQLOrderByFields(fromSQL: String): String;
var
  lengthFromSQL, i, j, p: Integer;
  Cont: Boolean;
begin
  Result := '';

  // No worry with enters:
  fromSQL := StringReplace(fromSQL, #$D#$A, ' ', [rfReplaceAll]);
  lengthFromSQL := length(fromSQL);

  // Locate FROM statement :
  p := pos(' ORDER BY ', AnsiUpperCase(fromSQL));

  // Extract tablenames :
  if p = 0 then Exit;

  for i := p + 6 to lengthFromSQL do
    if fromSQL[i] = ' ' then
    begin
      // Check if there' s more any table :
      Cont := false;
      for j := i + 1 to lengthFromSQL do
        if fromSQL[j] <> ' ' then
        begin
          Cont := fromSQL[j] = ',';  // There' s more!
          Break;
        end;

      if not Cont then
        Break;
    end
    else
      Result := Result + fromSQL[i];
end;

function GetInternalRecordLockId(aDatabaseName, aTableName, aPrimaryKeyValue: String): String;
begin
  Result := BackQuotedStr(AnsiLowerCase(aDatabaseName)) + '.' + BackQuotedStr(AnsiLowerCase(aTableName)) + '.' + aPrimaryKeyValue;
end;

function IsFreeLockId(aConnection: TSQLConnection; aLockId: String): Boolean;
var v: Variant;
begin
  Result := false;
  v := SQLReturnFieldValue(aConnection, 'SELECT IS_FREE_LOCK(' + SQLGetStringExpr(aLockId) + ')');
  Result := v = 1;
end;

function SQLExecute(aConnection: TSQLConnection; SQL: TStrings): Integer;
var aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.ParamCheck := false;
    aQuery.SQL.AddStrings(SQL);
    Result := aQuery.ExecSQL(True);  // Use True if query does not include any parameters ...
  finally
    aQuery.Free;
  end;
end;

function SQLExecute(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer;
var aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.ParamCheck := false;
    if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
    if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
    if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
    if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
    if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
    if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
    if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
    if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
    Result := aQuery.ExecSQL(True);  // Query does not include any parameters ...
  finally
    aQuery.Free;
  end;
end;

{$IFDEF DELPHI2007_OR_ABOVE}
function SQLExecuteInsert(aConnection: TSQLConnection; SQL: TStrings): String;
var
  aQuery: TSQLQuery;
  TmpTransaction: TDBXTransaction;
  TransactionCreated: Boolean;
begin
  Result := '';

  TransactionCreated := false;
  if not aConnection.InTransaction then
  begin
    TmpTransaction := aConnection.BeginTransaction;
    TransactionCreated := true;
  end;

  try

    aQuery := TSQLQuery.Create(Nil);

    try
      // Execute user insert command:
      aQuery.SQLConnection := aConnection;
      aQuery.ParamCheck := false;
      aQuery.SQL.AddStrings(SQL);
      aQuery.ExecSQL(True);  // Use True if query does not include any parameters ...

      // Retrieve last_insert_id:
      aQuery.Active := false;
      aQuery.SQL.Text := 'SELECT last_insert_id()';
      aQuery.Active := true;
      Result := aQuery.Fields[0].AsString;
      aQuery.Active := false;
    finally
      aQuery.Free;
    end;

  finally
    if TransactionCreated then
      aConnection.CommitFreeAndNil(TmpTransaction);
  end;
end;

function SQLExecuteInsert(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                    const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): String;
var
  aQuery: TSQLQuery;
  TmpTransaction: TDBXTransaction;
  TransactionCreated: Boolean;
begin
  Result := '';

  TransactionCreated := false;
  if not aConnection.InTransaction then
  begin
    TmpTransaction := aConnection.BeginTransaction;
    TransactionCreated := true;
  end;

  try

    aQuery := TSQLQuery.Create(Nil);

    try
      // Execute user insert command:
      aQuery.SQLConnection := aConnection;
      aQuery.ParamCheck := false;
      if SqlLine1 <> '' then aQuery.SQL.Add(SqlLine1);
      if SqlLine2 <> '' then aQuery.SQL.Add(SqlLine2);
      if SqlLine3 <> '' then aQuery.SQL.Add(SqlLine3);
      if SqlLine4 <> '' then aQuery.SQL.Add(SqlLine4);
      if SqlLine5 <> '' then aQuery.SQL.Add(SqlLine5);
      if SqlLine6 <> '' then aQuery.SQL.Add(SqlLine6);
      if SqlLine7 <> '' then aQuery.SQL.Add(SqlLine7);
      if SqlLine8 <> '' then aQuery.SQL.Add(SqlLine8);
      aQuery.ExecSQL(True);  // Query does not include any parameters ...

      // Retrieve last_insert_id:
      aQuery.Active := false;
      aQuery.SQL.Text := 'SELECT last_insert_id()';
      aQuery.Active := true;
      Result := aQuery.Fields[0].AsString;
      aQuery.Active := false;
    finally
      aQuery.Free;
    end;

  finally
    if TransactionCreated then
      aConnection.CommitFreeAndNil(TmpTransaction);
  end;
end;
{$ENDIF}

function SQLGetPrimaryKey(aConnection: TSQLConnection; aTableName: String): String;
var
  aQuery: TSQLQuery;
begin
  Result := '';
  if aTableName = '' then Exit;

  aQuery := TSQLQuery.Create(Nil);
  aQuery.SQLConnection := aConnection;
  aQuery.SQL.Text := 'show index from ' + cyDBX.BackQuotedStr(aTableName) + ' where Key_name = ''PRIMARY''';
  aQuery.Active := true;
  if not aQuery.Eof then
    Result := aQuery.FieldByName('column_name').AsString;
  aQuery.Active := false;
  aQuery.Free;
end;

function SQLReturnFieldValue(aConnection: TSQLConnection; SQL: TStrings): Variant;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    Result := aQuery.Fields[0].Value;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValue(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Variant;
var aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.Add(SqlLine1);
    aQuery.SQL.Add(SqlLine2);
    aQuery.SQL.Add(SqlLine3);
    aQuery.SQL.Add(SqlLine4);
    aQuery.Active := true;
    Result := aQuery.Fields[0].Value;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsString(aConnection: TSQLConnection; SQL: TStrings): string;
var
  aQuery: TSQLQuery;
  v: Variant;
begin
  Result := '';
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    v := aQuery.Fields[0].Value;
    if v <> Null then
      Result := v;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsString(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): String;
var
  aQuery: TSQLQuery;
  v: Variant;
begin
  Result := '';
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.Add(SqlLine1);
    aQuery.SQL.Add(SqlLine2);
    aQuery.SQL.Add(SqlLine3);
    aQuery.SQL.Add(SqlLine4);
    aQuery.Active := true;
    v := aQuery.Fields[0].Value;
    if v <> Null then
      Result := v;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; SQL: TStrings): Integer;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsInteger;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsInteger(aConnection: TSQLConnection; const SqlLine1: String; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                             const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Integer;
var
  aQuery: TSQLQuery;
begin
  Result := 0;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.Add(SqlLine1);
    aQuery.SQL.Add(SqlLine2);
    aQuery.SQL.Add(SqlLine3);
    aQuery.SQL.Add(SqlLine4);
    aQuery.Active := true;
    Result := aQuery.Fields[0].AsInteger;
    aQuery.Active := false;
  finally
    aQuery.Free;
  end;
end;

function SQLGetStringExpr(Value: String): String;
begin
  // Sanitize :
{  Value := cyStrUtils.String_Subst('\',  '\\', Value, csCaseNotSensitive, false);     // Need to be first line!
  Value := cyStrUtils.String_Subst('''', '\''', Value, csCaseNotSensitive, false);
  Value := cyStrUtils.String_Subst('"',  '\"', Value, csCaseNotSensitive, false); }

  // More slow :
  Value := StringReplace(Value, '\',  '\\', [rfReplaceAll]);     // Need to be first line!
  Value := StringReplace(Value, '''', '\''', [rfReplaceAll]);
  Value := StringReplace(Value, '"',  '\"', [rfReplaceAll]);

  Result := '''' + Value + ''''; // QuotedStr(Value) doesn' t work if the value already contains quote car ...
end;

function SQLGetDateExpr(Value: TDateTime; const SQLDateFormat: String = 'YYYY-MM-DD'): String;
begin
  Result := QuotedStr(FormatDateTime(SQLDateFormat, Value));
end;

function SQLGetFloatExpr(Value: Extended): String;
var fs: TFormatSettings;
begin
  fs.DecimalSeparator := '.';
  Result := FloatToStrF(Value, ffGeneral, 18, 18, fs);
end;

function SQLGetDateParam(ParamValue: TDateTime; const SQLDateFormat: String = 'YYYY-MM-DD'): String;
begin
  Result := FormatDateTime(SQLDateFormat, ParamValue);
end;

function SQLGetFloatParam(ParamValue: Extended): String;
begin
  Result := SQLGetFloatExpr(ParamValue);
end;

procedure SimpleDS_Refresh(aSimpleDataSet: TSimpleDataSet; const ApplyUpdates: Boolean = true);
begin
  if aSimpleDataSet.State in [dsInsert, dsEdit] then
    aSimpleDataSet.Post;

  if aSimpleDataSet.ChangeCount > 0 then
    if ApplyUpdates
    then SimpleDS_ApplyUpdates(aSimpleDataSet)
    else aSimpleDataSet.CancelUpdates;

  if aSimpleDataSet.DataSet.Active then
    aSimpleDataSet.DataSet.Refresh;  // Refresh records from server

  aSimpleDataSet.Refresh;            // Refresh clientdataset records
end;

function SimpleDS_ApplyUpdates(aSimpleDataSet: TSimpleDataSet; const MaxErrors: Integer = -1): Integer;
var RemoveReconcileErrorEvent: Boolean;
begin
  Result := 0;
  RemoveReconcileErrorEvent := false;

  if aSimpleDataSet.ChangeCount > 0 then
  begin
    // Auto handle errors?
    if not Assigned(aSimpleDataSet.OnReconcileError) then
    begin
      aSimpleDataSet.OnReconcileError := TDatasetProviderDefaultErrorHandler(nil).ReconcileError;
      RemoveReconcileErrorEvent := true;
    end;

    Result := aSimpleDataSet.ApplyUpdates(MaxErrors);

    if RemoveReconcileErrorEvent then
      aSimpleDataSet.OnReconcileError := Nil;
  end;
end;

function ClientDS_Refresh(aClientDataSet: TClientDataSet; const ApplyUpdates: Boolean = true): Integer;
var DataSetProvider: TDatasetProvider;
begin
  DataSetProvider := ClientDS_GetProvider(aClientDataSet);
  Result := ClientDS_Refresh(aClientDataSet, DataSetProvider, ApplyUpdates);
end;

function ClientDS_Refresh(aClientDataSet: TClientDataSet; aProvider: TDatasetProvider; const ApplyUpdates: Boolean = true): Integer;
begin
  Result := 0;
  if aClientDataSet.State in [dsInsert, dsEdit] then
    aClientDataSet.Post;

  if aClientDataSet.ChangeCount > 0 then
    if ApplyUpdates
    then Result := ClientDS_ApplyUpdates(aClientDataSet)
    else aClientDataSet.CancelUpdates;

  if aProvider <> Nil then
    if aProvider.DataSet.Active then
      aProvider.DataSet.Refresh;  // Refresh server records (Works with TSQlTable)

  aClientDataSet.Refresh;         // Refresh clientdataset records
end;

function ClientDS_ApplyUpdates(aClientDataSet: TClientDataSet; const MaxErrors: Integer = -1): Integer;
var
  RemoveReconcileErrorEvent: Boolean;
begin
  Result := 0;
  RemoveReconcileErrorEvent := false;

  if aClientDataSet.ChangeCount > 0 then
  begin
    // Auto handle errors?
    if not Assigned(aClientDataSet.OnReconcileError) then
    begin
      aClientDataSet.OnReconcileError := TDatasetProviderDefaultErrorHandler(nil).ReconcileError;
      RemoveReconcileErrorEvent := true;
    end;

    Result := aClientDataSet.ApplyUpdates(MaxErrors);

    if RemoveReconcileErrorEvent then
      aClientDataSet.OnReconcileError := Nil;
  end;
end;

function ClientDS_GetProvider(aClientDataSet: TCustomClientDataset): TDatasetProvider;
var
  c: Integer;
  aComponent: TComponent;
begin
  Result := nil;

  {$IFDEF DELPHI2009_OR_ABOVE}
  if aClientDataSet.ProviderName = '' then
  begin
    for c := 0 to aClientDataSet.ComponentCount-1 do
      if aClientDataSet.Components[c] is TDataSetProvider then
      begin
        Result := TDataSetProvider(aClientDataSet.Components[c]);
        Break;
      end;
  end
  else begin
    aComponent := aClientDataSet.Owner.FindComponent(aClientDataSet.ProviderName);
    if Assigned(aComponent) then
      if aComponent is TDataSetProvider then
        Result := TDataSetProvider(aComponent);
  end;
  {$ELSE}
    for c := 0 to aClientDataSet.ComponentCount-1 do
      if aClientDataSet.Components[c] is TDataSetProvider then
      begin
        Result := TDataSetProvider(aClientDataSet.Components[c]);
        Break;
      end;
  {$ENDIF}
end;

initialization

  vDbxErrorRecordLocked := 'Record locked by another user!';

end.
