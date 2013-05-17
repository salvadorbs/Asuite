unit cyDbxUpdateSql;

{   Component(s):
    tcyDbxUpdateSql

    Description:
    Allows database tables/columns updates handling

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

uses Classes, Windows, SysUtils, SqlExpr, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} Dialogs, cyDBX;

type
  TcyDbxUpdateSql = class;

  TSQLTask = (stCustom, stCreateTable, stCreateColumn, stDeleteTable, stDeleteColumn, stClearTable, stClearColumn, stUpdateColumnValue, stModifyTable, stModifyColumn);
  TDiscardSQLUpdateReason = (drNotDiscarded, drDisabled, drEmptySQL, drDependenceNotExecuted, drTableExists, drTableNotExists, drColumnExists, drColumnNotExists, drRejectOnBeforeExecuteEvent);

  TSQLUpdateItem = class(TCollectionItem)
  private
    FTag: Integer;
    FDescription: String;
    FSQL: TStrings;
    FTableName: String;
    FSQLTask: TSQLTask;
    FSQLResult: Integer;
    FExecuted: Boolean;
    FEnabled: Boolean;
    FColumnName: String;
    FifSQLUpdateIndexExecuted: integer;
    FDiscardIfExistsCmd: Boolean;
    procedure SetSQL(const Value: TStrings);
    procedure SetSQLTask(const Value: TSQLTask);
    procedure SetTableName(const Value: String);
    procedure SetColumnName(const Value: String);
  protected
    function GetDisplayName: string; override;
    procedure BuildSQL;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function TableExists: Boolean;
    function ColumnExists: Boolean;
    property SQLResult: Integer read FSQLResult;
    property Executed: Boolean read FExecuted;
  published
    property Description: String read FDescription write FDescription;
    property DiscardIfExistsCmd: Boolean read FDiscardIfExistsCmd write FDiscardIfExistsCmd default false;
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property ifSQLUpdateIndexExecuted: integer read FifSQLUpdateIndexExecuted write FifSQLUpdateIndexExecuted default -1;
    property SQLTask: TSQLTask read FSQLTask write SetSQLTask default stCustom;
    property TableName: String read FTableName write SetTableName;
    property ColumnName: String read FColumnName write SetColumnName;
    property SQL: TStrings read FSQL write SetSQL;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TSQLUpdateItemClass = class of TSQLUpdateItem;

  TSQLUpdates = Class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetSQLUpdateItem(Index: Integer): TSQLUpdateItem;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aDbxUpdateSql: TcyDbxUpdateSql; SQLUpdateItemClass: TSQLUpdateItemClass);
    function Add: TSQLUpdateItem;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TSQLUpdateItem read GetSQLUpdateItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TProcOnBeforeExecute = procedure (Sender: TObject; aSQLUpdateIndex: Integer; aQuery: TSQLQuery; var SQLEnabled: Boolean) of object;
  TProcSQLEvent = procedure (Sender: TObject; aSQLUpdateIndex: Integer) of object;
  TProcOnDiscardEvent = procedure (Sender: TObject; aSQLUpdateIndex: Integer; Reason: TDiscardSQLUpdateReason) of object;

  TcyDbxUpdateSqlOptions = class(TPersistent)
  private
    FOwner: TcyDbxUpdateSql;
    FMySQL_UseInformationSchema: Boolean;
//    FUseTransaction: Boolean;
    procedure SetMySQL_UseInformationSchema(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property MySQL_UseInformationSchema: Boolean read FMySQL_UseInformationSchema write SetMySQL_UseInformationSchema default false;
//    property UseTransaction: Boolean read FUseTransaction write FUseTransaction default true;
  end;

  TcyDbxUpdateSql = class(TComponent)
  private
    FSQLConnection: TSQLConnection;
    FSQLUpdates: TSQLUpdates;
    FAfterExecuteSqlUpdate: TProcSQLEvent;
    FBeforeExecuteSqlUpdate: TProcOnBeforeExecute;
    FCreateTableCharset: String;
    FCreateTableEngine: String;
    FOnSQLError: TProcSQLEvent;
    FOnDiscardSqlUpdate: TProcOnDiscardEvent;
    FOptions: TcyDbxUpdateSqlOptions;
    procedure SetSQLConnection(const Value: TSQLConnection);
    procedure SetSQLUpdates(const Value: TSQLUpdates);
    procedure SetOptions(const Value: TcyDbxUpdateSqlOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoUpdate(aSQLUpdateIndex: Integer);
    procedure DoUpdates;
  published
    property CreateTableEngine: String read FCreateTableEngine write FCreateTableEngine;
    property CreateTableCharset: String read FCreateTableCharset write FCreateTableCharset;
    property Options: TcyDbxUpdateSqlOptions read FOptions write SetOptions;
    property SQLConnection: TSQLConnection read FSQLConnection write SetSQLConnection;
    property SQLUpdates: TSQLUpdates read FSQLUpdates write SetSQLUpdates;
    property BeforeExecuteSqlUpdate: TProcOnBeforeExecute read FBeforeExecuteSqlUpdate write FBeforeExecuteSqlUpdate;
    property AfterExecuteSqlUpdate: TProcSQLEvent read FAfterExecuteSqlUpdate write FAfterExecuteSqlUpdate;
    property OnDiscardSqlUpdate: TProcOnDiscardEvent read FOnDiscardSqlUpdate write FOnDiscardSqlUpdate;
    property OnSQLError: TProcSQLEvent read FOnSQLError write FOnSQLError;
  end;

implementation

{ TSQLUpdateItem }
constructor TSQLUpdateItem.Create(Collection: TCollection);
begin
  inherited;
  FDescription := '';
  FDiscardIfExistsCmd := false;
  FEnabled := True;
  FifSQLUpdateIndexExecuted := -1;
  FSQLTask := stCustom;
  FTableName := '';
  FColumnName := '';
  FSQL := TStringList.Create;
  FExecuted := false;
  FSQLResult := 0;
  FTag := 0;
end;

destructor TSQLUpdateItem.Destroy;
begin
  FSQL.Free;
  inherited;
end;

function TSQLUpdateItem.GetDisplayName: string;
begin
  if FDescription = '' then
  begin
    case FSQLTask of
      stCustom:            Result := '[Custom task] ';
      stCreateTable:       Result := '[Create table] ' + FTableName;
      stCreateColumn:      Result := '[Create column] ' + FTableName + '.' + FColumnName;
      stDeleteTable:       Result := '[Delete table] ' + FTableName;
      stDeleteColumn:      Result := '[Delete column] ' + FColumnName;
      stClearTable:        Result := '[Clear table] ' + FTableName;
      stClearColumn:       Result := '[Clear column] ' + FColumnName;
      stUpdateColumnValue: Result := '[Update column value] ' + FTableName + '.' + FColumnName;
      stModifyTable:       Result := '[Modify table] ' + FTableName;
      stModifyColumn:      Result := '[Modify column] ' + FColumnName;
    end;
  end
  else
    Result := FDescription;

  if not FEnabled then
    Result := Result + '  (Disabled)';
end;

procedure TSQLUpdateItem.SetSQLTask(const Value: TSQLTask);
begin
  if FSQLTask = Value then Exit;

  FSQLTask := Value;
  FSQL.Text := '';
  BuildSQL;
end;

procedure TSQLUpdateItem.SetTableName(const Value: String);
var OldValue: String;
begin
  if FTableName = Value then Exit;
  OldValue := FTableName;
  FTableName := Value;

  if OldValue = '' then
    FSQL.Text := '';

  if FTableName <> '' then
    if FSQL.Text = '' then
      BuildSQL
    else
      FSQL.Text := StringReplace(FSQL.Text, OldValue, FTableName, [rfReplaceAll, rfIgnoreCase]);
end;

function TSQLUpdateItem.TableExists: Boolean;
var
  aQuery: TSQLQuery;
  cyDbxUpdateSql: TcyDbxUpdateSql;
begin
  Result := false;
  if FTableName = '' then Exit;

  aQuery := TSQLQuery.Create(Nil);
  cyDbxUpdateSql := TcyDbxUpdateSql( TSQLUpdates(Collection).FOwner );
  aQuery.SQLConnection := cyDbxUpdateSql.FSQLConnection;

  try
    if cyDbxUpdateSql.FOptions.FMySQL_UseInformationSchema then
    begin
      aQuery.SQL.Text := 'SELECT DISTINCT TABLE_NAME FROM information_schema.columns WHERE TABLE_SCHEMA = ' + SQLGetStringExpr(cyDbxUpdateSql.FSQLConnection.Params.Values['Database'])
                          + ' AND TABLE_NAME = ' + SQLGetStringExpr(FTableName);
    end
    else begin
      // Not working on MySQL prior 5.x and on Delphi 7:
      aQuery.SQL.Text := 'SHOW TABLES LIKE ' + SQLGetStringExpr(FTableName);
    end;

    aQuery.Tag := aQuery.ExecSQL(True);         // Returns -1 if fails, else returns records number
    if aQuery.Tag > 0 then
      Result := true;
  finally
  end;

  aQuery.Free;
end;

function TSQLUpdateItem.ColumnExists: Boolean;
var
  aQuery: TSQLQuery;
  cyDbxUpdateSql: TcyDbxUpdateSql;
begin
  Result := false;
  if FColumnName = '' then Exit;
  if not TableExists then Exit;

  aQuery := TSQLQuery.Create(Nil);
  cyDbxUpdateSql := TcyDbxUpdateSql( TSQLUpdates(Collection).FOwner );
  aQuery.SQLConnection := cyDbxUpdateSql.FSQLConnection;

  try
    if cyDbxUpdateSql.FOptions.FMySQL_UseInformationSchema then
    begin
      aQuery.SQL.Text := 'SELECT COLUMN_NAME FROM information_schema.columns WHERE TABLE_SCHEMA = ' + SQLGetStringExpr(cyDbxUpdateSql.FSQLConnection.Params.Values['Database'])
                           + ' AND TABLE_NAME = ' + SQLGetStringExpr(FTableName) + ' AND COLUMN_NAME = ' + SQLGetStringExpr(FColumnName);
    end
    else begin
      // Not working on MySQL prior 5.x :
      aQuery.SQL.Text := 'SHOW COLUMNS FROM ' + FTableName + ' LIKE ' + SQLGetStringExpr(FColumnName);
    end;

    aQuery.Tag := aQuery.ExecSQL(True);         // Returns -1 if fails, else returns records number
    if aQuery.Tag > 0 then
      Result := true;
  finally
  end;

  aQuery.Free;
end;

procedure TSQLUpdateItem.SetColumnName(const Value: String);
var OldValue: String;
begin
  if FColumnName = Value then Exit;
  OldValue := FColumnName;
  FColumnName := Value;

  if FSQLTask in [stCreateTable, stCreateColumn, stDeleteColumn, stClearColumn, stUpdateColumnValue, stModifyColumn] then
  begin
    if OldValue = '' then
      FSQL.Text := '';

    if (FTableName <> '') and (FColumnName <> '') then
      if FSQL.Text = '' then
        BuildSQL
      else
        FSQL.Text := StringReplace(FSQL.Text, OldValue, FColumnName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TSQLUpdateItem.BuildSQL;

            procedure BuildSQLCreateTable;
            begin
              FSQL.Clear;
              FSQL.Add('CREATE TABLE IF NOT EXISTS `' + FTableName + '` (');
              FSQL.Add('`id' + FTableName + '` INT NOT NULL AUTO_INCREMENT ,');
              if FColumnName <> '' then
                FSQL.Add('`' + FColumnName + '` VARCHAR(45) NULL ,');
              FSQL.Add('PRIMARY KEY (`id' + FTableName + '`) )');

              with TcyDbxUpdateSql( TSQLUpdates(Collection).FOwner ) do
              begin
                if FCreateTableEngine <> '' then
                  FSQL.Add('ENGINE = ' + FCreateTableEngine);
                if FCreateTableCharset <> '' then
                FSQL.Add('DEFAULT CHARACTER SET = ' + FCreateTableCharset);
              end;
            end;

            procedure BuildSQLCreateColumn;
            begin
              FSQL.Clear;
              FSQL.Add('ALTER TABLE `' + FTableName + '` ADD COLUMN `' + FColumnName + '` VARCHAR(45) NULL DEFAULT ' + QuotedStr(''));
            end;

            procedure BuildSQLDeleteTable;
            begin
              FSQL.Clear;
              FSQL.Add('DROP TABLE IF EXISTS `' + FTableName + '`');
            end;

            procedure BuildSQLDeleteColumn;
            begin
              FSQL.Clear;
              FSQL.Add('ALTER TABLE `' + FTableName + '` DROP COLUMN `' + FColumnName + '`');
            end;

            procedure BuildSQLClearTable;
            begin
              FSQL.Clear;
              FSQL.Add('TRUNCATE TABLE `' + FTableName + '`');
            end;

            procedure BuildSQLClearColumn;
            begin
              FSQL.Clear;
              FSQL.Add('UPDATE `' + FTableName + '` SET `' + FColumnName + '` = DEFAULT');
            end;

            procedure BuildSQLUpdateValue;
            begin
              FSQL.Clear;
              FSQL.Add('UPDATE `' + FTableName + '` SET `' + FColumnName + '` = NULL');
            end;

begin
  if csLoading in TcyDbxUpdateSql( TSQLUpdates(Collection).FOwner ).ComponentState then Exit;
  if (FSQLTask = stCustom) or (FTableName = '') then Exit;

  case FSQLTask of
    stCreateTable:       BuildSQLCreateTable;
    stCreateColumn:      BuildSQLCreateColumn;
    stDeleteTable:       BuildSQLDeleteTable;
    stDeleteColumn:      BuildSQLDeleteColumn;
    stClearTable:        BuildSQLClearTable;
    stClearColumn:       BuildSQLClearColumn;
    stUpdateColumnValue: BuildSQLUpdateValue;
//    stModifyTable
//    stModifyColumn
  end;
end;

procedure TSQLUpdateItem.SetSQL(const Value: TStrings);
begin
  if Assigned(FSQL) then
    FSQL.Assign(Value)
  else
    FSQL := Value;
end;

procedure TSQLUpdateItem.Assign(Source: TPersistent);
begin
//  inherited;
  if Source is TSQLUpdateItem then
  begin
    FDescription := TSqlUpdateItem(Source).FDescription;
    FTableName := TSqlUpdateItem(Source).FTableName;
    FSQL  := TSqlUpdateItem(Source).FSQL;
    FTag := TSqlUpdateItem(Source).FTag;
  end;
end;

{ TSQLUpdates }
constructor TSQLUpdates.Create(aDbxUpdateSql: TcyDbxUpdateSql; SQLUpdateItemClass: TSQLUpdateItemClass);
begin
  inherited Create(SQLUpdateItemClass);
  FOwner := aDbxUpdateSql;
end;

function TSQLUpdates.Add: TSQLUpdateItem;
begin
  Result := TSQLUpdateItem(inherited Add);
  Result.Changed(false);      // It will call TSQLUpdates.Update only at run-time!
end;

procedure TSQLUpdates.Delete(Index: Integer);
begin
  Inherited;
  Update(Nil);
end;

function TSQLUpdates.GetSQLUpdateItem(Index: Integer): TSQLUpdateItem;
begin
  Result := TSQLUpdateItem(inherited Items[Index]);
end;

function TSQLUpdates.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// Event Called by setting properties/events of TSQLUpdateItem :
procedure TSQLUpdates.Update(Item: TCollectionItem);
begin
  inherited;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TcyDbxUpdateSqlOptions }
constructor TcyDbxUpdateSqlOptions.Create(AOwner: TComponent);
begin
  FOwner := TcyDbxUpdateSql(AOwner);
  FMySQL_UseInformationSchema := false;
//  FUseTransaction := true;
end;

procedure TcyDbxUpdateSqlOptions.SetMySQL_UseInformationSchema(const Value: Boolean);
begin
  FMySQL_UseInformationSchema := Value;

  if FMySQL_UseInformationSchema then
  if (csDesigning in FOwner.ComponentState) and (not (csLoading in FOwner.ComponentState)) then
      ShowMessage('Note that connected user must have specific rights in order to access InformationSchema !');
end;

{ TcyDbxUpdateSql }
constructor TcyDbxUpdateSql.Create(AOwner: TComponent);
begin
  inherited;
  FSQLUpdates := TSQLUpdates.Create(self, TSQLUpdateItem);
  FOptions := TcyDbxUpdateSqlOptions.Create(self);
  FCreateTableEngine := '';
  FCreateTableCharset := '';
end;

destructor TcyDbxUpdateSql.Destroy;
begin
  FSQLUpdates.Free;
  FSQLUpdates := Nil;
  FOptions.Free;

  inherited;
end;

procedure TcyDbxUpdateSql.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if FSQLConnection <> nil
  then
    if (Operation = opRemove) and (AComponent = FSQLConnection)
    then FSQLConnection := nil;
end;

procedure TcyDbxUpdateSql.SetOptions(const Value: TcyDbxUpdateSqlOptions);
begin
  FOptions := Value;
end;

procedure TcyDbxUpdateSql.SetSQLConnection(const Value: TSQLConnection);
begin
  FSQLConnection := Value;

  if Value <> nil then
    Value.FreeNotification(Self);  // Inform cyDbxUpdateSql if component removed ...
end;

procedure TcyDbxUpdateSql.SetSQLUpdates(const Value: TSQLUpdates);
begin
  FSQLUpdates := Value;
end;

procedure TcyDbxUpdateSql.DoUpdate(aSQLUpdateIndex: Integer);
var
  {$IFDEF DELPHI2007_OR_ABOVE}
  TmpTransaction: TDBXTransaction;
  {$ENDIF}

  aQuery: TSQLQuery;
  DiscardSQLUpdateReason: TDiscardSQLUpdateReason;
  TransactionCreated, Allow: Boolean;
begin
  FSQLUpdates[aSQLUpdateIndex].FExecuted := false;
  DiscardSQLUpdateReason := drNotDiscarded;

  if not FSQLUpdates[aSQLUpdateIndex].FEnabled then
    DiscardSQLUpdateReason := drDisabled
  else
    if FSQLUpdates[aSQLUpdateIndex].FSQL.Text = '' then
      DiscardSQLUpdateReason := drEmptySQL
    else
      if FSQLUpdates[aSQLUpdateIndex].FifSQLUpdateIndexExecuted <> -1 then
        if not FSQLUpdates[FSQLUpdates[aSQLUpdateIndex].FifSQLUpdateIndexExecuted].FExecuted then
          DiscardSQLUpdateReason := drDependenceNotExecuted;

  if DiscardSQLUpdateReason = drNotDiscarded then
  begin
    // Create transaction :

    {$IFDEF DELPHI2007_OR_ABOVE}
    TransactionCreated := false;
    if not FSQLConnection.InTransaction then
    begin
      TmpTransaction := FSQLConnection.BeginTransaction;
      TransactionCreated := true;
    end;
    {$ENDIF}

    // Check Table/column exists :
    if not FSQLUpdates[aSQLUpdateIndex].FDiscardIfExistsCmd then
      case FSQLUpdates[aSQLUpdateIndex].FSQLTask of
          stCreateTable:       if FSQLUpdates[aSQLUpdateIndex].TableExists  then DiscardSQLUpdateReason     := drTableExists;
          stDeleteTable:       if not FSQLUpdates[aSQLUpdateIndex].TableExists  then DiscardSQLUpdateReason := drTableNotExists;
          stCreateColumn:
          begin
                               if not FSQLUpdates[aSQLUpdateIndex].TableExists  then DiscardSQLUpdateReason := drTableNotExists;
                               if FSQLUpdates[aSQLUpdateIndex].ColumnExists then DiscardSQLUpdateReason     := drColumnExists;
          end;
          stDeleteColumn:      if not FSQLUpdates[aSQLUpdateIndex].ColumnExists then DiscardSQLUpdateReason := drColumnNotExists;
          stClearTable:        if not FSQLUpdates[aSQLUpdateIndex].TableExists  then DiscardSQLUpdateReason := drTableNotExists;
          stClearColumn:       if not FSQLUpdates[aSQLUpdateIndex].ColumnExists then DiscardSQLUpdateReason := drColumnNotExists;
          stUpdateColumnValue: if not FSQLUpdates[aSQLUpdateIndex].ColumnExists then DiscardSQLUpdateReason := drColumnNotExists;
          stModifyTable:       if not FSQLUpdates[aSQLUpdateIndex].TableExists  then DiscardSQLUpdateReason := drTableNotExists;
          stModifyColumn:      if not FSQLUpdates[aSQLUpdateIndex].ColumnExists then DiscardSQLUpdateReason := drColumnNotExists;
      end;

    if DiscardSQLUpdateReason = drNotDiscarded then
      try
        Allow := True;
        aQuery := TSQLQuery.Create(Nil);
        aQuery.SQLConnection := FSQLConnection;
        aQuery.SQL.AddStrings(FSQLUpdates[aSQLUpdateIndex].FSQL);

        if Assigned(FBeforeExecuteSqlUpdate) then
          FBeforeExecuteSqlUpdate(Self, aSQLUpdateIndex, aQuery, Allow);

        if Allow then
        begin
          try
            FSQLUpdates[aSQLUpdateIndex].FSQLResult := aQuery.ExecSQL(aQuery.Params.Count = 0);
            FSQLUpdates[aSQLUpdateIndex].FExecuted := true;
          except
            if Assigned(FOnSQLError) then
              FOnSQLError(Self, aSQLUpdateIndex);
          end;

          if Assigned(FAfterExecuteSqlUpdate) then
            FAfterExecuteSqlUpdate(Self, aSQLUpdateIndex);
        end
        else
          DiscardSQLUpdateReason := drRejectOnBeforeExecuteEvent;
      finally
        aQuery.Free;
      end;

    {$IFDEF DELPHI2007_OR_ABOVE}
    if TransactionCreated then
      if DiscardSQLUpdateReason = drNotDiscarded then
        FSQLConnection.CommitFreeAndNil(TmpTransaction)
      else
        FSQLConnection.RollbackFreeAndNil(TmpTransaction);
    {$ENDIF}
  end;

  if DiscardSQLUpdateReason <> drNotDiscarded then
    if Assigned(FOnDiscardSqlUpdate) then
      FOnDiscardSqlUpdate(Self, aSQLUpdateIndex, DiscardSQLUpdateReason);
end;

procedure TcyDbxUpdateSql.DoUpdates;
var u: Integer;
begin
  for u := 0 to FSQLUpdates.Count-1 do
    DoUpdate(u);
end;

end.
