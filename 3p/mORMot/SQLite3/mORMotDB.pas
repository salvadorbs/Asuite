/// Virtual Tables for external DB access for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotDB;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2013 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2013
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.15
  - first public release, corresponding to mORMot Framework 1.15

  Version 1.16
  - TSQLRestServerStaticExternal.EngineList method now handles an optional
    integer pointer, to return the count of row data (excluding field names)

  Version 1.17
  - changed column named 'RowID' into 'ID' since it is reserved e.g. in Oracle
  - external direct insert, update or delete actions (i.e. when the
    TSQLRestServerStaticExternal instance is called directly) will now
    flush the low-level SQLite3 DB cache, as expected by the virtual tables
  - added TSQLRestServerStaticExternal.AdaptSQLForEngineList overriden method to
    handle most generic SELECT to by-pass the SQLite3 virtual module for speed
  - added TSQLRestServerStaticExternal.EndCurrentThread overriden method which
    will be called e.g. by TSQLite3HttpServer or TSQLRestServerNamedPipeResponse
    for each terminating threads, to release external connection resource
    (calling TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method)
  - any direct or virtual-table based insertion to the external database will
    now use a binding matching the exact time of each column: it will e.g. allow
    to support DBMS which does not accept date/time to be supplied as ISO-8601
    text, and make more efficient data conversion (like avoid conversion to
    floating-point from a currency value) - code shared with BATCH mode and newly
    added TSQLRestServerStaticExternal.ExecuteFromJSON() protected method
  - inlined parameters in any SQL query will bind explicitely TDateTime values
    if the parameter is transmitted as DateToSQL() or DateTimeToSQL() TEXT
  - removed TSQLRecordExternal class type, to allow any TSQLRecord (e.g.
    TSQLRecordMany) to be used with VirtualTableExternalRegister() - there was
    indeed no implementation requirement to force a specific class type
  - now create properly UNIQUE fields (i.e. "stored AS_UNIQUE") in external tables
  - handle NULL values for BLOBs as expected

  Version 1.18
  - unit SQLite3DB.pas renamed mORMotDB.pas
  - huge performance boost when inserting individual data rows, by maintaining
    the IDs in memory instead of executing "select max(id)" - added new property
    EngineAddUseSelectMaxID to unset this optimization
  - new function VirtualTableExternalRegisterAll(), to register all tables
    of a mORMot model to be handled via a specified database
  - TSQLRestServerStaticExternal.AdaptSQLForEngineList() will now accept
    'select count(*) from TableName [where...]' statements directly (virtual
    behavior for count(*) is to loop through all records, which may be slow)
  - now TSQLRestServerStaticExternal won't create any columns for external
    tables with unsupported published property types (sftUnknown or sftMany),
    just like TSQLRecord.GetSQLCreate() method
  - fixed issue in TSQLRestServerStaticExternal.EngineDeleteWhere() when
    calling commands like MyDB.Delete(TSQLMyClass, 'PLU < ?', [20000])
  - fixed ticket [3c41462594] in TSQLRestServerStaticExternal.ExecuteFromJSON()
  - fixed ticket [9a821d26ee] in TSQLRestServerStaticExternal.Create() not
    creating any missing field
  - fixed TSQLRestServerStaticExternal.UpdateBlobFields() to return true
    if no BLOB field is defined, and to proper handle multi-field update
  - fixed ticket [21c2d5ae96] when inserting/updating blob-only table content
  - handle null binding in TSQLRestServerStaticExternal.ExecuteInlined()
  - added TSQLRestServerStaticExternal.TableHasRows/TableRowCount overrides
  - optimized TSQLRestServerStaticExternal.UpdateBlobFields()/RetrieveBlobFields()
    methods, updating/retrieving all BLOB fields at once in the same SQL statement
  - this unit will now set SynDBLog := TSQLLog during its initialization


}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  SynDB;

type
  /// REST server with direct access to a SynDB-based external database
  // - handle all REST commands, using the external SQL database connection,
  // and prepared statements
  // - is used by TSQLRestServer.URI for faster RESTful direct access
  // - for JOINed SQL statements, the external database is also defined as
  // a SQLite3 virtual table, via the TSQLVirtualTableExternal[Cursor] classes
  TSQLRestServerStaticExternal = class(TSQLRestServerStatic)
  protected
    fProperties: TSQLDBConnectionProperties;
    fTableName: RawUTF8;
    fSelectOneDirectSQL, fSelectAllDirectSQL, fSelectTableHasRowsSQL: RawUTF8;
    fEngineUseSelectMaxID: Boolean;
    fEngineLockedLastID: integer;
    /// used internaly to guess e.g. if the column is indexed
    fFields: TSQLDBColumnDefineDynArray;
    fFieldsDynArray: TDynArrayHashed;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TSQLURIMethod;
    fBatchCapacity, fBatchCount, fBatchAddedID: integer;
    // BATCH sending uses TEXT storage for direct sending to database driver
    fBatchValues, fBatchIDs: TRawUTF8DynArray;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function ExecuteInlined(const aSQL: RawUTF8; ExpectResults: Boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and inlined parameters
    function ExecuteInlined(SQLFormat: PUTF8Char; const Args: array of const; ExpectResults: Boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirect(SQLFormat: PUTF8Char; const Args, Params: array of const;
      ExpectResults: Boolean): ISQLDBRows;
    function ExecuteDirectVarData(SQLFormat: PUTF8Char; const Args: array of const;
       var Params: TVarDataDynArray; LastParam: integer): boolean;
    // overriden methods calling the external engine with SQL via Execute
    function EngineRetrieve(TableModelIndex, ID: integer): RawUTF8; override;
    function EngineLockedNextID: Integer; virtual;
    function EngineAdd(Table: TSQLRecordClass; const SentData: RawUTF8): integer; override;
    function EngineUpdate(Table: TSQLRecordClass; ID: integer; const SentData: RawUTF8): boolean; override;
    function EngineDeleteWhere(Table: TSQLRecordClass; const SQLWhere: RawUTF8;
      const IDs: TIntegerDynArray): boolean; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function EngineSearchField(const FieldName: ShortString;
      const FieldValue: array of const; var ResultID: TIntegerDynArray): boolean;
    // overriden method returning TRUE + next calls to EngineAdd/Update/Delete (
    // will properly handle operations until InternalBatchStop is called
    function InternalBatchStart(Method: TSQLURIMethod): boolean; override;
    /// internal method called by TSQLRestServer.RunBatch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    procedure InternalBatchStop; override;
    function InternalBatchAdd(const aValue: RawUTF8; aID: integer): integer;
    /// TSQLRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overriden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - change 'RowID' into 'ID' column name, and SQLTableName into fTableName
    // - any 'LIMIT #' clause will be changed into the appropriate SQL statement
    // - handle statements to avoid slow virtual table loop over all rows, like
    // $ SELECT count(*) FROM table
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    /// run INSERT of UPDATE from the corresponding JSON object
    // - Occasion parameter shall be only either soInsert or soUpate
    // - each JSON field will be bound with the proper SQL type corresponding to
    // the real external table columns (e.g. as TEXT for variant)
    // - returns 0 on error, or the Updated/Inserted ID 
    function ExecuteFromJSON(const SentData: RawUTF8; Occasion: TSQLOccasion;
      UpdatedID: integer): integer;
  public
    /// initialize the remote database connection
    // - all filename/binary parameters are ignored here, since it will rely
    // on the RecordProps.ExternalDatabase property to create the connection -
    // in practice, just call the global VirtualTableExternalRegister() procedure
    // - RecordProps.ExternalDatabase will map the associated TSQLDBConnectionProperties
    // - RecordProps.ExternalTableName will retrieve the real full table name,
    // e.g. including any database schema prefix
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean=false); override;
    /// delete a row, calling the external engine with SQL
    // - made public since a TSQLRestServerStatic instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(Table: TSQLRecordClass; ID: integer): boolean; override;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: RawUTF8; FieldValue: Integer;
      var ResultID: TIntegerDynArray): boolean; overload; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName, FieldValue: RawUTF8;
      var ResultID: TIntegerDynArray): boolean; overload; override;
    /// overriden method for direct external database engine call
    function TableRowCount(Table: TSQLRecordClass): integer; override;
    /// overriden method for direct external database engine call
    function TableHasRows(Table: TSQLRecordClass): boolean; override;
     {{ begin a transaction (implements REST BEGIN Member)
     - to be used to speed up some SQL statements like Insert/Update/Delete
     - must be ended with Commit on success
     - must be aborted with Rollback if any SQL statement failed
     - return true if no transaction is active, false otherwize }
    function TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean; override;
    {{ end a transaction (implements REST END Member)
     - write all pending SQL statements to the external database }
    procedure Commit(SessionID: cardinal=1); override;
    {{ abort a transaction (implements REST ABORT Member)
     - restore the previous state of the database, before the call to TransactionBegin }
    procedure RollBack(SessionID: cardinal=1); override;
    /// overriden method for direct external SQL database engine thread-safe process
    // - this method will in fact call only one (first) statement
    // - it will convert all inlined parameters (like :(1234): into bound
    // parameters)
    function EngineExecuteAll(const aSQL: RawUTF8): boolean; override;
    /// update a field value of the external database
    function EngineUpdateField(Table: TSQLRecordClass;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
     /// overriden method for direct external database engine call
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overriden method for direct external database engine call
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;
    /// create one index for all specific FieldNames at once
    // - this method will in fact call the SQLAddIndex method, if the index
    // is not already existing
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;

    /// this method is called by TSQLRestServer.EndCurrentThread method just
    // before a thread is finished to ensure that the associated external DB
    // connection will be released for this thread
    // - this overriden implementation will clean thread-specific connections,
    // i.e. call TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method
    // - this method shall be called directly, nor from the main thread
    procedure EndCurrentThread(Sender: TObject); override;

    /// retrieve the external database connection associated to a TSQLRecord
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TSQLRestServerStaticExternal
    class function ExternalRecordProperties(aClass: TSQLRecordClass;
      aServer: TSQLRestServer): TSQLDBConnectionProperties;
    /// the associated external database connection
    property Properties: TSQLDBConnectionProperties read fProperties;
    /// by default, any INSERT will compute the new ID from an internal variable
    // - it is very fast and reliable, unless external IDs can be created
    // outside this engine
    // - you can set EngineAddUseSelectMaxID=true to execute a slower
    // 'select max(ID) from TableName' SQL statement
    property EngineAddUseSelectMaxID: Boolean read fEngineUseSelectMaxID
      write fEngineUseSelectMaxID;
  end;

  {{ A Virtual Table cursor for reading a TSQLDBStatement content
    - this is the cursor class associated to TSQLVirtualTableExternal }
  TSQLVirtualTableCursorExternal = class(TSQLVirtualTableCursor)
  protected
    fStatement: ISQLDBStatement;
    fHasData: boolean;
  public
    /// called to begin a search in the virtual table, creating a SQL query
    // - the TSQLVirtualTablePrepared parameters were set by
    // TSQLVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSQLite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - all WHERE and ORDER BY clauses are able to be translated into a plain 
    // SQL statement calling the external DB engine
    // - will create the internal fStatement from a SQL query, bind the
    // parameters, then execute it, ready to be accessed via HasData/Next
    function Search(const Prepared: TSQLVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row
    // - handled types in aResult are varNull, varInt64, varDouble, varString
    // (mapping a constant PUTF8Char) and varAny (BLOB with size = VLongs[0])
    // - if aColumn=VIRTUAL_TABLE_ROWID_COLUMN(-1), will return the row ID
    // as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TVarData): boolean; override;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; override;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; override;
  end;
  
  {{ A SynDB-based virtual table for accessing any external database
   - for ORM access, you should use VirtualTableExternalRegister method to
     associated this virtual table module to any TSQLRecord class
   - transactions are handled by this module, according to the external database }
  TSQLVirtualTableExternal = class(TSQLVirtualTable)
  public { overriden methods }
    /// returns the main specifications of the associated TSQLVirtualTableModule
    // - this is a read/write table, without transaction (yet), associated to the
    // TSQLVirtualTableCursorExternal cursor type, with 'External' as module name
    // and TSQLRestServerStaticExternal as the related static class
    // - no particular class is supplied here, since it will depend on the
    // associated Static TSQLRestServerStaticExternal instance
    class procedure GetTableModuleProperties(var aProperties: TVirtualTableModuleProperties);
      override;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TSQLVirtualTableCursor.Search()
    // - this overriden method will let the external DB engine perform the search,
    // using a standard SQL "SELECT * FROM .. WHERE .. ORDER BY .." statement
    // - in Where[], Expr must be set to not 0 if needed for Search method,
    // and OmitCheck always set to true since double check is not necessary
    // - OmitOrderBy will be set to true since double sort is not necessary
    // - EstimatedCost will receive the estimated cost, with lowest value if
    // fStatic.fFields[].ColumnIndexed is set (i.e. if column has an index)
    function Prepare(var Prepared: TSQLVirtualTablePrepared): boolean; override;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - returns true on success, false otherwise
    function Drop: boolean; override;
    /// called to delete a virtual table row
    // - returns true on success, false otherwise
    function Delete(aRowID: Int64): boolean; override;
    /// called to insert a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - the column values are available via some TVarData of type
    // varNull, varInt64, varDouble, varString (mapping a constant PUTF8Char),
    // and varAny (BLOB with size = VLongs[0])
    // - returns true on success, false otherwise
    // - returns the just created row ID in insertedRowID on success
    function Insert(aRowID: Int64; var Values: TVarDataDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - the column values are available via some TVarData of type
    // varNull, varInt64, varDouble, varString (mapping a constant PUTF8Char),
    // and varAny (BLOB with size = VLongs[0])
    // - returns true on success, false otherwise
    function Update(oldRowID, newRowID: Int64; var Values: TVarDataDynArray): boolean; override;
  end;


/// register on the Server-side an external database for an ORM class
// - will associate the supplied class with a TSQLVirtualTableExternal module
// (calling aModel.VirtualTableRegister method), even if the class does not
// inherit from TSQLRecordVirtualTableAutoID (it can be any plain TSQLRecord or
// TSQLRecordMany sub-class for instance)
// - note that TSQLModel.Create() will reset all supplied classes to be defined
// as non virtual (i.e. Kind=rSQLite3)
// - this function shall be called BEFORE TSQLRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - the full table name, as expected by the external database, could be
// provided here (SQLTableName will be used internaly as table name when
// called via the associated SQLite3 Virtual Table) - if no table name is
// specified (''), will use SQLTableName (e.g. 'Customer' for 'TSQLCustomer')
// - typical usage is therefore for instance:
// !  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
// !  Model := TSQLModel.Create([TSQLCustomer],'root');
// !  VirtualTableExternalRegister(Model,TSQLCustomer,Props,'Sales.Customer');
// !  Server := TSQLRestServerDB.Create(aModel,'application.db'),true)
// - the supplied aExternalDB parameter is stored within aClass.RecordProps, so
// the instance must stay alive until all database access to this external table
// is finished (e.g. use a private/protected property)
// - server-side may omit a call to VirtualTableExternalRegister() if the need of
// an internal database is expected: it will allow custom database configuration
// at runtime, depending on the customer's expectations (or license)
function VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8): boolean;

/// register all tables of the model to be external
// - by default, all tables are handled by the SQLite3 engine, unless they
// are explicitely declared as external via VirtualTableExternalRegister: this
// function can be used to register all tables to be handled by an external DBs
// - this function shall be called BEFORE TSQLRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - by default, TSQLAuthUser and TSQLAuthGroup tables will be handled via the
// external DB, but you can avoid it for speed when handling session and security
function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties; DoNotCacheUserGroupTables: boolean=false): boolean;


implementation

function VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8): boolean;
begin
  if (aModel=nil) or (aClass=nil) or (aExternalDB=nil) then begin
    result := False;
    exit; // avoid GPF
  end;
  aModel.Props[aClass].Kind := rCustomAutoID; // force creation use of SQLite3 virtual table
  result := aModel.VirtualTableRegister(aClass,TSQLVirtualTableExternal,
    aExternalTableName,aExternalDB);
end;

function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties; DoNotCacheUserGroupTables: boolean=false): boolean;
var i: integer;
begin
  if (aModel=nil) or (aExternalDB=nil) then begin
    result := false;
    exit; // avoid GPF
  end;
  result := true;
  for i := 0 to high(aModel.Tables) do
    if DoNotCacheUserGroupTables and ((aModel.Tables[i]=TSQLAuthGroup)
       or (aModel.Tables[i]=TSQLAuthUser)) then
      continue else
    if not VirtualTableExternalRegister(aModel,aModel.Tables[i],aExternalDB,'') then
      result := false;
end;


{ TSQLRestServerStaticExternal }

procedure TSQLRestServerStaticExternal.Commit(SessionID: cardinal);
begin
  inherited Commit(SessionID); // reset fTransactionActive + write all TSQLVirtualTableJSON
  try
    fProperties.ThreadSafeConnection.Commit;
  except
    on Exception do
      ; // just catch exception
  end;
end;

function TSQLPropInfoToExternalField(Prop: TSQLPropInfo; var Column: TSQLDBColumnProperty): boolean;
const
  mORMotType: array[TSQLFieldType] of TSQLDBFieldType =
    (ftUnknown,   // sftUnknown
     ftUTF8,      // sftAnsiText
     ftUTF8,      // sftUTF8Text
     ftInt64,     // sftEnumerate
     ftInt64,     // sftSet
     ftInt64,     // sftInteger
     ftInt64,     // sftID
     ftInt64,     // sftRecord
     ftInt64,     // sftBoolean
     ftDouble,    // sftFloat
     ftDate,      // sftDateTime
     ftInt64,     // sftTimeLog
     ftCurrency,  // sftCurrency
     ftUTF8,      // sftObject
{$ifdef USEVARIANTS}
     ftUTF8,      // sftVariant
{$endif}
     ftBlob,      // sftBlob
     ftBlob,      // sftBlobDynArray
     ftBlob,      // sftBlobCustom
     ftUTF8,      // sftUTF8Comp
{$ifdef PUBLISHRECORD}
     ftBlob,      // sftBlobRecord
{$endif}
     ftUnknown,   // sftMany
     ftInt64,     // sftModTime
     ftInt64);    // sftCreateTime
begin
  result := false;
  Column.ColumnType := mORMotType[Prop.SQLFieldType];
  if Column.ColumnType=ftUnknown then
    exit; // ignore unkwnown fields
  Column.ColumnName := Prop.Name;
  Column.ColumnAttr := Prop.FieldWidth;
  Column.ColumnUnique := aIsUnique in Prop.Attributes;
  result := true;
end;

function TSQLRecordClassToExternalFields(aClass: TSQLRecordClass): TSQLDBColumnPropertyDynArray;
var i,f: integer;
begin
  with aClass.RecordProps do begin
    SetLength(result,Fields.Count);
    f := 0;
    for i := 0 to Fields.Count-1 do
      if TSQLPropInfoToExternalField(Fields.List[i],result[f]) then
        inc(f);
    if f<>Fields.Count then
      SetLength(result,f);
  end;
end;

constructor TSQLRestServerStaticExternal.Create(aClass: TSQLRecordClass;
  aServer: TSQLRestServer; const aFileName: TFileName; aBinaryFile: boolean);
var SQL: RawUTF8;
    f: integer;
    Field: TSQLDBColumnProperty;
    FieldAdded: Boolean;
begin
  inherited Create(aClass,aServer,aFileName,aBinaryFile);
  // initialize external DB properties
  if fStoredClassProps=nil then
    raise EBusinessLayerException.CreateFmt(
      'StoredClassProps needed for %s',[fStoredClassRecordProps.SQLTableName]);
  fTableName := fStoredClassProps.ExternalTableName;
  fProperties := fStoredClassProps.ExternalDatabase as TSQLDBConnectionProperties;
  if fProperties=nil then
    raise EBusinessLayerException.CreateFmt(
      'No external DB defined for %s',[fStoredClassRecordProps.SQLTableName]);
  if Owner<>nil then
    try
      Owner.ServerTimeStamp := fProperties.MainConnection.ServerTimeStamp;
    except
      on E: Exception do ; // ignore any error here
    end;
  // create corresponding external table if necessary, and retrieve its fields info
  fProperties.GetFields(fTableName,fFields);
  if fFields=nil then begin
    // table is not yet existing -> try to create it
    SQL := fProperties.SQLCreate(fTableName,TSQLRecordClassToExternalFields(aClass));
    if SQL<>'' then
      if ExecuteDirect(pointer(SQL),[],[],false)<>nil then begin
        fProperties.GetFields(fTableName,fFields); // fields from DB after create
        if fFields=nil then
          raise EORMException.CreateFmt(
            '%s: external table creation %s failed: GetFields() returned nil - SQL="%s"',
            [fStoredClass.ClassName,fTableName,SQL]);
      end;
  end;
  fFieldsDynArray.Init(TypeInfo(TSQLDBColumnDefineDynArray),fFields,nil,nil,nil,nil,true);
  fFieldsDynArray.ReHash;
  // create any missing field if necessary
  FieldAdded := false;
  with fStoredClassProps.Props do
  for f := 0 to Fields.Count-1 do
    with Fields.List[f] do
    if SQLFieldType in COPIABLE_FIELDS then
    /// real database columns exist for Simple + Blob fields (not Many)
    if fFieldsDynArray.FindHashed(Name)<0 then begin
      // add new missing Field
      Finalize(Field);
      fillchar(Field,sizeof(Field),0);
      if TSQLPropInfoToExternalField(Fields.List[f],Field) then begin
        SQL := fProperties.SQLAddColumn(fTableName,Field);
        if (SQL<>'') and (ExecuteDirect(pointer(SQL),[],[],false)<>nil) then
          FieldAdded := true else
          raise EORMException.CreateFmt(
            '%s: unable to create external missing field %s.%s - SQL="%s"',
            [fStoredClass.ClassName,fTableName,Name,SQL]);
      end;
    end;
  if FieldAdded then begin
    fProperties.GetFields(fTableName,fFields); // fields from DB after alter
    fFieldsDynArray.ReHash;
  end;
  // compute the SQL statements used internaly for external DB requests
  fSelectOneDirectSQL := StoredClassProps.SQL.TableSimpleFields[true,false];
  if IdemPChar(pointer(fSelectOneDirectSQL),'ROWID') then
    System.Delete(fSelectOneDirectSQL,1,3); // RowID -> ID when executed in direct
  fSelectOneDirectSQL := FormatUTF8('select % from %',[fSelectOneDirectSQL,fTableName]);
  if fOwner<>nil then // override only if was not retrieved  from fModel[0]
    fStoredClassProps.SQL.SelectAll[true] := fSelectOneDirectSQL;
  fSelectOneDirectSQL := fSelectOneDirectSQL+' where ID=?';
  fSelectAllDirectSQL := FormatUTF8('select %,ID from %',
    [StoredClassProps.SQL.InsertSet,fTableName]);
  fSelectTableHasRowsSQL := 'SELECT ID FROM '+fStoredClassRecordProps.SQLTableName+' LIMIT 1';
  AdaptSQLForEngineList(fSelectTableHasRowsSQL);
end;
    
function TSQLRestServerStaticExternal.AdaptSQLForEngineList(var SQL: RawUTF8): boolean;
var Prop: ShortString;
    P: PUTF8Char;
    BPos,AfterSelectPos,RowIDFromPos,TablePos,RowIDWherePos,RowIDOrderByPos: integer;
    LimitPos, LimitRowCount, WhereClausePos, err: integer;
function PropIsField: boolean;
var tmp: RawUTF8;
begin
  result := true;
  if Prop='ID' then
    exit;
  tmp := Prop;
  if fFieldsDynArray.FindHashed(tmp)<0 then
    result := false;
end;
label O;
procedure GetFieldProp;
var i,L: integer;
    B: PUTF8Char;
begin
  if P^=#0 then begin
    Prop[0] := #0;
    exit;
  end;
  P := GotoNextNotSpace(P); // trim left
  B := P;
  BPos := B-pointer(SQL)+1;
  while ord(P^) in IsIdentifier do inc(P); // go to end of field name
  L := P-B;
  Prop[0] := AnsiChar(L);
  for i := 0 to L-1 do
    Prop[i+1] := NormToUpperAnsi7[B[i]];
  P := GotoNextNotSpace(P); // trim right
end;
function NextTokenIsTable: boolean;
begin
  GetFieldProp;
  with StoredClassProps.Props do
    result := IdemPropName(Prop,pointer(SQLTableName),length(SQLTableName));
  TablePos := BPos;
end;
procedure ChangeTableName;
begin
  system.delete(SQL,TablePos,length(StoredClassRecordProps.SQLTableName));
  insert(fTableName,SQL,TablePos); 
end;
begin
  result := inherited AdaptSQLForEngineList(SQL);
  if result or (SQL='') then
    exit; // found generic 'SELECT * FROM table' query
  // change 'RowID' into 'ID' column name, and SQLTableName into fTableName
  // process 'SELECT Field1,Field2 FROM table WHERE RowID=... AND/OR/NOT Field2='
  LimitPos := 0;
  LimitRowCount := 0;
  WhereClausePos := 0;
  RowIDFromPos := 0;
  RowIDWherePos := 0;
  RowIDOrderByPos := 0;
  P := pointer(SQL);
  GetFieldProp;
  if Prop<>'SELECT' then exit;
  AfterSelectPos := P-pointer(SQL)+1;
  repeat
    GetFieldProp;
    if Prop='' then exit;
    if (Prop='COUNT') and IdemPChar(P,'(*)') then begin
      inc(P,3);
      GetFieldProp;
      if Prop<>'FROM' then
        exit;
      if P^ in [#0,';'] then begin
        if not NextTokenIsTable then
          exit;
        ChangeTableName;
        result := true; // handled 'select count(*) from TableName'
        exit;
      end;
      break; // will process 'select count(*) from tablename where ...'
    end;
    if Prop='ROWID' then
      if RowIDFromPos>0 then
        exit else
        RowIDFromPos := BPos else
      if not PropIsField then
        exit; // unknown field name
    if P^=',' then
      inc(P) else begin
      GetFieldProp;
      if Prop<>'FROM' then
        exit else
        break;
    end;
  until false;
  if not NextTokenIsTable then
    exit;
  GetFieldProp;
  if Prop='ORDER' then begin // simple ORDER BY clause is accepted
    WhereClausePos := -BPos; // WhereClausePos<0 for ORDER BY position
O:  GetFieldProp;
    if Prop<>'BY' then exit;
    GetFieldProp;
    if Prop='ROWID' then
      RowIDOrderByPos := BPos else // 'RowID' -> 'ID';
      if not PropIsField then
          exit; // unknown field name in 'ORDER BY' clause
    GetFieldProp;
    if Prop='LIMIT' then
      LimitPos := BPos else
      if not (GotoNextNotSpace(P)^ in [#0,';']) then
        exit; // allow only one column name
  end else
  if Prop='WHERE' then
  repeat
    GetFieldProp;
    WhereClausePos := BPos;
    if Prop='NOT' then
      GetFieldProp; // allow  field1=456 AND NOT field2='Toto'
    if Prop='' then exit else
    if Prop='ROWID' then
      if RowIDWherePos>0 then
        exit else
        RowIDWherePos := BPos else
    if not PropIsField then
        exit; // unknown field name or 'LIMIT' / 'ORDER BY' clause
    if P^='=' then
      inc(P) else
    if P^ in ['>','<'] then
      if P[1] in ['=','>'] then
        inc(P,2) else
        inc(P) else
    if IdemPChar(P,'LIKE ') then
      GetFieldProp else
      exit; // only handle "Field = > >= < <= <> LIKE Value" pairs
    P := GotoNextNotSpace(P);
    if PWord(P)^=ord(':')+ord('(') shl 8 then
      P := GotoNextNotSpace(P+2); // +2 to ignore :(...): parameter
    if P^ in ['''','"'] then
      P := GotoEndOfQuotedString(P) else
      repeat inc(P) until P^ in [#0..' ',';',')']; // go to end of value
    P := GotoNextNotSpace(P);
    if PWord(P)^=ord(')')+ord(':')shl 8 then
      inc(P,2); // ignore :(...): parameter
    P := GotoNextNotSpace(P);
    if P^ in [#0,';'] then break; // properly ended the WHERE clause
    GetFieldProp;
    if Prop='ORDER' then
      goto O else
    if Prop='LIMIT' then begin
      LimitPos := BPos;
      break;
    end else
    if (Prop<>'AND') and (Prop<>'OR') then exit;
  until false else
  if Prop='LIMIT' then
    LimitPos := BPos else
  if Prop<>'' then
    exit;
  // handle LIMIT # statement at the end of the SQL
  if LimitPos>0 then begin
    GetFieldProp;
    Prop[ord(Prop[0])+1] := #0;
    LimitRowCount := GetInteger(@Prop[1],err);
    if (err<>0) or (LimitRowCount<=0) or (fProperties=nil) then
      exit;
    system.delete(SQL,LimitPos,P-pointer(SQL)+1-LimitPos); // erase 'LIMIT #'
    while (SQL<>'') and (SQL[length(SQL)]=' ') do
      SetLength(SQL,length(SQL)-1);
  end;
  // convert all 'RowID' to 'ID' and adapt 'LIMIT #' statement
  err := length(StoredClassRecordProps.SQLTableName)-length(fTableName);
  if WhereClausePos<0 then
    inc(WhereClausePos,err) else
  if WhereClausePos>0 then
    dec(WhereClausePos,err);
  if LimitPos>0 then
    dec(LimitPos,err);
  if RowIDOrderByPos>0 then begin
    system.delete(SQL,RowIDOrderByPos,3);
    if LimitPos>0 then
      dec(LimitPos,3);
  end;
  if RowIDWherePos>0 then begin
    system.delete(SQL,RowIDWherePos,3); // 'RowID' -> 'ID'
    if LimitPos>0 then
      dec(LimitPos,3);
    if WhereClausePos<0 then
      inc(WhereClausePos,3);
  end;
  ChangeTableName;
  if RowIDFromPos>0 then begin
    system.delete(SQL,RowIDFromPos,3); // 'RowID' -> 'ID'
    if LimitPos>0 then
      dec(LimitPos,3);
    if WhereClausePos<0 then
      inc(WhereClausePos,3) else
    if WhereClausePos>0 then
      dec(WhereClausePos,3);
  end;
  if LimitPos>0 then
    if not fProperties.AdaptSQLLimitForEngineList(SQL,
       LimitRowCount,AfterSelectPos,WhereClausePos,LimitPos) then
      exit;
  result := true;
end;

function TSQLRestServerStaticExternal.EngineLockedNextID: Integer;
// fProperties.SQLCreate: ID Int64 PRIMARY KEY -> compute unique RowID
// (not all DB engines handle autoincrement feature - e.g. Oracle does not)
var Rows: ISQLDBRows;
begin
  if (fEngineLockedLastID=0) or EngineAddUseSelectMaxID then begin
    // first method call -> retrieve value from DB
    Rows := ExecuteDirect('select max(ID) from %',[fTableName],[],true);
    if (Rows<>nil) and Rows.Step then
      fEngineLockedLastID := Rows.ColumnInt(0) else
      fEngineLockedLastID := 0;
  end;
  inc(fEngineLockedLastID);
  result := fEngineLockedLastID;
end;

function TSQLRestServerStaticExternal.InternalBatchStart(
  Method: TSQLURIMethod): boolean;
const BATCH: array[mPOST..mDELETE] of TSQLDBStatementCRUD = (
  cCreate, cUpdate, cDelete);
begin
  result := false;
  if (self<>nil) and (method in [mPOST..mDELETE]) and
     (BATCH[method] in fProperties.BatchSendingAbilities) then begin
    Lock(true); // protected by try..finally in TSQLRestServer.RunBatch
    try
      assert(fBatchMethod=mNone,'InternalBatchStop should have been called');
      if Method=mPOST then
        fBatchAddedID := EngineLockedNextID else
        fBatchAddedID := 0;
      fBatchMethod := Method;
      fBatchCount := 0;
      result := true;
    finally
      if not result then
        UnLock;
    end;
  end;
end;

procedure TSQLRestServerStaticExternal.InternalBatchStop;
var i,j,k,n,max,BatchBegin,BatchEnd,ValuesMax: integer;
    Query: ISQLDBStatement;
    SQL: RawUTF8;
    P: PUTF8Char;
    Fields: TRawUTF8DynArray;
    Types: TSQLDBFieldTypeDynArray;
    Values: array of TRawUTF8DynArray;
    Occasion: TSQLOccasion;
    Decode: TJSONObjectDecoder;
begin
  assert(fBatchMethod<>mNone);
  try
    if fBatchCount>0 then begin
      with fProperties do
        if BatchMaxSentAtOnce>0 then
          max := BatchMaxSentAtOnce else
          max := 1000;
      BatchBegin := 0;
      BatchEnd := fBatchCount-1;
      repeat
        case fBatchMethod of
        mPost, mPut: begin
          assert(fBatchIDs<>nil);
          BatchEnd := fBatchCount-1;
          for i := BatchBegin to BatchEnd do begin
            P := @fBatchValues[i][1]; // make copy before in-place decoding
            while P^ in [#1..' ','{','['] do inc(P);
            case fBatchMethod of
            mPost: Occasion := soInsert;
            mPut:  Occasion := soUpdate;
              else Occasion := soSelect; // makes compiler happy
            end;
            case Occasion of
            soInsert: // mPost=INSERT with the supplied fields and computed ID
              Decode.Decode(P,nil,pQuoted,GetInteger(pointer(fBatchIDs[i])),true);
            soUpdate: // mPut=UPDATE with the supplied fields and ID set appart
              Decode.Decode(P,nil,pQuoted,0,true);
            end;
            if Fields=nil then begin
              Decode.AssignFieldNamesTo(Fields);
              n := Decode.FieldCount;
              if Occasion=soUpdate then
                inc(n); // additional ?=ID parameter at update
              SetLength(Values,n);
              SetLength(Types,n);
              ValuesMax := fBatchCount-BatchBegin;
              if ValuesMax>max then
                ValuesMax := max;
              for j := 0 to high(Fields) do begin
                k := fFieldsDynArray.FindHashed(Fields[j]);
                if k<0 then
                  raise ESQLDBException.CreateFmt('Unknown field name "%s"',[Fields[j]]);
                Types[j] := fFields[k].ColumnType;
                if Values[j]=nil then // only initialize Values[] once
                  SetLength(Values[j],ValuesMax);
              end;
              SQL := Decode.EncodeAsSQLPrepared(fTableName,Occasion);
              if Occasion=soUpdate then begin
                Types[Decode.FieldCount] := ftInt64; // additional ?=ID parameter
                if Values[Decode.FieldCount]=nil then
                  SetLength(Values[Decode.FieldCount],ValuesMax);
              end;
            end else
              if not Decode.SameFieldNames(Fields) then
                break; // this item would break the SQL statement
            n := i-BatchBegin;
            for j := 0 to Decode.FieldCount-1 do
              Values[j,n] := Decode.FieldValues[j]; // regroup by parameter
            if Occasion=soUpdate then
              Values[Decode.FieldCount,n] := fBatchIDs[i]; // ?=ID parameter
            BatchEnd := i; // mark fBatchValues[i] has to be copied in Values[]
            if n+1>=max then
              break; // do not send too much items at once, for better speed
          end;
        end;
        mDelete:
          SQL := 'delete from '+fTableName+' where ID=?';
        end;
        n := BatchEnd-BatchBegin+1;
        if n<=0 then
          break;
        Query := fProperties.NewThreadSafeStatementPrepared(SQL,false);
        try
          case fBatchMethod of
          mPost, mPut:
            for i := 0 to high(Values) do
              Query.BindArray(i+1,Types[i],Values[i],n);
          mDelete:
            Query.BindArray(1,ftInt64,fBatchIDs,n);
          end;
          Query.ExecutePrepared;
          if Owner<>nil then begin
            // add/update/delete should flush DB cache
            Owner.FlushInternalDBCache;
            // force deletion coherency
            if fBatchMethod=mDelete then
              for i := 0 to high(Values) do
                Owner.AfterDeleteForceCoherency(
                  fStoredClass,GetInteger(pointer(Values[i])));
          end;
        finally
          Query := nil;
        end;
        SetLength(Fields,0); // force new sending block
        BatchBegin := BatchEnd+1;
      until BatchBegin>=fBatchCount;
    end;
  finally
    if fBatchMethod=mPost then
      fEngineLockedLastID := fBatchAddedID+fBatchCount;
    SetLength(fBatchValues,0);
    SetLength(fBatchIDs,0);
    fBatchCount := 0;
    fBatchCapacity := 0;
    fBatchMethod := mNone;
    UnLock;
  end;
end;

function TSQLRestServerStaticExternal.InternalBatchAdd(
  const aValue: RawUTF8; aID: integer): integer;
begin
  result := fBatchAddedID+fBatchCount;
  if fBatchCount>=fBatchCapacity then begin
    fBatchCapacity := fBatchCapacity+64+fBatchCount shr 3;
    SetLength(fBatchIDs,fBatchCapacity);
    if aValue<>'' then
      SetLength(fBatchValues,fBatchCapacity);
  end;
  if aValue<>'' then
    fBatchValues[fBatchCount] := aValue;
  if aID=0 then
    aID := result;
  fBatchIDs[fBatchCount] := Int32ToUtf8(aID);
  inc(fBatchCount);
end;

function TSQLRestServerStaticExternal.EngineAdd(Table: TSQLRecordClass;
  const SentData: RawUTF8): integer;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := 0 else // avoid GPF
  if fBatchMethod<>mNone then
    if fBatchMethod<>mPOST then
      result := 0 else
      result := InternalBatchAdd(SentData,0) else
    result := ExecuteFromJSON(SentData,soInsert,0); // UpdatedID=0 -> insert with EngineLockedNextID
end;

function TSQLRestServerStaticExternal.EngineUpdate(Table: TSQLRecordClass;
  ID: integer; const SentData: RawUTF8): boolean;
begin
  if (self=nil) or (Table<>fStoredClass) or (ID<=0) then
    result := false else
    if fBatchMethod<>mNone then
      if fBatchMethod<>mPUT then
        result := false else
        result := InternalBatchAdd(SentData,ID)>=0 else
      result := ExecuteFromJSON(SentData,soUpdate,ID)=ID;
end;

function TSQLRestServerStaticExternal.EngineDelete(Table: TSQLRecordClass;
  ID: integer): boolean;
begin
  if (self=nil) or (Table<>fStoredClass) or (ID<=0) then
    result := false else
    if fBatchMethod<>mNone then
      if fBatchMethod<>mDELETE then
        result := false else
        result := InternalBatchAdd('',ID)>=0 else
      result := ExecuteDirect('delete from % where ID=?',[fTableName],[ID],false)<>nil;
end;

function TSQLRestServerStaticExternal.EngineDeleteWhere(
  Table: TSQLRecordClass; const SQLWhere: RawUTF8; const IDs: TIntegerDynArray): boolean;
var i: integer;
begin
  result := false;
  if (self=nil) or (Table<>fStoredClass) or (SQLWhere='') or (IDs=nil) then
    exit;
  if fBatchMethod<>mNone then
    if fBatchMethod<>mDELETE then
      exit else
      for i := 0 to high(IDs) do
        InternalBatchAdd('',IDs[i]) else
    if ExecuteInlined('delete from % where %',[fTableName,SQLWhere],false)=nil then
      exit;
  result := true;
end;

function TSQLRestServerStaticExternal.EngineExecuteAll(
  const aSQL: RawUTF8): boolean;
begin
  result := ExecuteInlined(aSQL,false)<>nil; // only execute the first statement
end;

function TSQLRestServerStaticExternal.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var Rows: ISQLDBRows;
begin
  Rows := ExecuteInlined(SQL,true);
  if Rows=nil then
    result := '' else
    result := Rows.FetchAllAsJSON(ForceAJAX or (not NoAJAXJSON),ReturnedRowCount);
end;

function TSQLRestServerStaticExternal.EngineRetrieve(TableModelIndex, ID: integer): RawUTF8;
var Rows: ISQLDBRows;
begin // TableModelIndex is not usefull here
  result := '';
  if (self=nil) or (ID<=0) then
    exit;
  Rows := ExecuteDirect(pointer(fSelectOneDirectSQL),[],[ID],true);
  if Rows<>nil then begin
    result := Rows.FetchAllAsJSON(true); // Expanded=true -> '[{"ID":10,...}]'#10
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := '' else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result,2,length(result)-3);
  end;
end;

function TSQLRestServerStaticExternal.TableHasRows(Table: TSQLRecordClass): boolean;
var Rows: ISQLDBRows;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := false else begin
    Rows := ExecuteDirect(pointer(fSelectTableHasRowsSQL),[],[],true);
    if Rows=nil then
      result := false else
      result := Rows.Step;
  end;
end;

function TSQLRestServerStaticExternal.TableRowCount(Table: TSQLRecordClass): integer;
var Rows: ISQLDBRows;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := 0 else begin
    Rows := ExecuteDirect('SELECT count(*) FROM %',[fTableName],[],true);
    if (Rows=nil) or not Rows.Step then
      result := 0 else
      result := Rows.ColumnInt(0);
  end;
end;

function TSQLRestServerStaticExternal.EngineRetrieveBlob(
  Table: TSQLRecordClass; aID: integer; BlobField: PPropInfo;
  out BlobData: TSQLRawBlob): boolean;
var Rows: ISQLDBRows;
begin
  result := false;
  if (self=nil) or (Table<>fStoredClass) or (aID<=0) or not BlobField^.IsBlob then
    exit;
  Rows := ExecuteDirect('select % from % where ID=?',[BlobField^.Name,fTableName],[aID],true);
  if (Rows<>nil) and Rows.Step then
  try
    BlobData := Rows.ColumnBlob(0);
    result := true; // success
    Rows := nil;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestServerStaticExternal.RetrieveBlobFields(Value: TSQLRecord): boolean;
var Rows: ISQLDBRows;
    f: Integer;
    data: TVarData;
    temp: RawByteString;
begin
  result := false;
  if (Value<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^=fStoredClass) then
  with Value.RecordProps do
  if BlobFields<>nil then begin
    Rows := ExecuteDirect('select % from % where ID=?',
      [SQLTableRetrieveBlobFields,fTableName],[Value.ID],true);
    if (Rows<>nil) and Rows.Step then
    try
      for f := 0 to High(BlobFields) do begin
        Rows.ColumnToVarData(f,data,temp);
        BlobFields[f].SetFieldVarData(Value,data);
      end;
      result := true; // success
      Rows := nil;
    except
      on Exception do
        result := false;
    end;
  end;
end;


function TSQLRestServerStaticExternal.EngineUpdateField(Table: TSQLRecordClass;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
begin
  result := ExecuteInlined('update % set %=:(%): where %=:(%):',
    [fTableName,SetFieldName,SetValue,WhereFieldName,WhereValue],false)<>nil;
end;

function TSQLRestServerStaticExternal.EngineUpdateBlob(
  Table: TSQLRecordClass; aID: integer; BlobField: PPropInfo;
  const BlobData: TSQLRawBlob): boolean;
var Statement: ISQLDBStatement;
begin
  result := false;
  if (Table=fStoredClass) and (aID>0) and BlobField^.IsBlob then
  try
    if Owner<>nil then
      Owner.FlushInternalDBCache;
    Statement := fProperties.NewThreadSafeStatementPrepared(
      'update % set %=? where ID=?',[fTableName,BlobField^.Name],false);
    if Statement<>nil then begin
      if BlobData='' then
        Statement.BindNull(1) else
        Statement.BindBlob(1,BlobData); // fast explicit BindBlob() call
      Statement.Bind(2,aID);
      Statement.ExecutePrepared;
      result := true; // success
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestServerStaticExternal.UpdateBlobFields(Value: TSQLRecord): boolean;
var f: integer;
    temp: array of RawByteString;
    Params: TVarDataDynArray;
begin
  result := false;
  if (Value<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^=fStoredClass) then
  with Value.RecordProps do
  if BlobFields<>nil then begin
    if Owner<>nil then
      Owner.FlushInternalDBCache;
    SetLength(Params,length(BlobFields));
    SetLength(temp,length(BlobFields));
    for f := 0 to high(Params) do
      BlobFields[f].GetFieldVarData(Value,Params[f],temp[f]);
    result := ExecuteDirectVarData('update % set % where ID=?',
      [fTableName,SQLTableUpdateBlobFields],Params,Value.ID);
  end else
    result := true; // as TSQLRest.UpdateblobFields()
end;

function TSQLRestServerStaticExternal.ExecuteInlined(const aSQL: RawUTF8;
  ExpectResults: Boolean): ISQLDBRows;
begin
  result := nil; // returns nil interface on error
  if self=nil then
    exit;
  if (not ExpectResults) and (Owner<>nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    result := fProperties.ExecuteInlined(aSQL,ExpectResults);
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestServerStaticExternal.ExecuteInlined(SQLFormat: PUTF8Char;
  const Args: array of const; ExpectResults: Boolean): ISQLDBRows;
begin
  result := ExecuteInlined(FormatUTF8(SQLFormat,Args),ExpectResults);
end;

function TSQLRestServerStaticExternal.ExecuteDirect(SQLFormat: PUTF8Char;
  const Args, Params: array of const; ExpectResults: Boolean): ISQLDBRows;
var Query: ISQLDBStatement;
begin
  result := nil;
  if self=nil then
    exit;
  if (not ExpectResults) and (Owner<>nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,ExpectResults);
  if Query<>nil then
  try
    Query.Bind(Params);
    Query.ExecutePrepared;
    result := Query;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestServerStaticExternal.ExecuteDirectVarData(SQLFormat: PUTF8Char;
  const Args: array of const; var Params: TVarDataDynArray; LastParam: integer): boolean;
var Query: ISQLDBStatement;
    f: integer;
begin
  result := false;
  if Self<>nil then
  try
    Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,false);
    if Query=nil then
      exit;
    with fStoredClassRecordProps do
    if length(Params)<>Fields.Count then
      raise EORMException.Create('Invalid ExecuteDirectVarData') else
    for f := 0 to high(Params) do
      if Fields.List[f].SQLFieldType=sftDateTime then
      with Params[f] do
      if VType=varString then begin
        VType := varDate;
        VDate := Iso8601ToDateTimePUTF8Char(PUTF8Char(VAny));
      end;
    Query.Bind(Params);
    if LastParam<>0 then
      Query.Bind(length(Params)+1,LastParam);
    Query.ExecutePrepared;
    result := true; // success
  except
    on Exception do
      result := false;
  end;
end;

procedure TSQLRestServerStaticExternal.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset fTransactionActive
  try
    fProperties.ThreadSafeConnection.Rollback;
  except
    on Exception do
      ; // just catch exception
  end;
end;

function TSQLRestServerStaticExternal.EngineSearchField(
  const FieldName: ShortString; const FieldValue: array of const;
  var ResultID: TIntegerDynArray): boolean;
var n: Integer;
    Rows: ISQLDBRows;
begin
  n := 0;
  Rows := ExecuteDirect('select ID from % where %=?',
    [fTableName,FieldName],FieldValue,true);
  if Rows<>nil then
    while Rows.Step do
      AddInteger(ResultID,n,Rows.ColumnInt(0));
  SetLength(ResultID,n);
  result := n>0;
end;

function TSQLRestServerStaticExternal.SearchField(const FieldName: RawUTF8;
  FieldValue: Integer; var ResultID: TIntegerDynArray): boolean;
begin
  result := EngineSearchField(FieldName,[FieldValue],ResultID);
end;

function TSQLRestServerStaticExternal.SearchField(const FieldName, FieldValue: RawUTF8;
  var ResultID: TIntegerDynArray): boolean;
begin
  result := EngineSearchField(FieldName,[FieldValue],ResultID);
end;

function TSQLRestServerStaticExternal.TransactionBegin(
  aTable: TSQLRecordClass; SessionID: cardinal): boolean;
begin
  result := false;
  if (aTable=fStoredClass) and inherited TransactionBegin(aTable,SessionID) then
  try
    fProperties.ThreadSafeConnection.StartTransaction;
    result := true; // success
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestServerStaticExternal.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
var SQL: RawUTF8;
    i: integer;
begin
  result := false;
  if (self=nil) or (fProperties=nil) or (Table<>fStoredClass) then
    exit;
  if high(FieldNames)=0 then begin
    i := fFieldsDynArray.FindHashed(FieldNames[0]);
    if (i>=0) and (fFields[i].ColumnIndexed) then begin
      result := true; // column already indexed
      exit;
    end;
  end;
  SQL := fProperties.SQLAddIndex(fTableName,FieldNames,Unique,IndexName);
  if (SQL<>'') and (ExecuteDirect(pointer(SQL),[],[],false)<>nil) then
    result := true;
end;

class function TSQLRestServerStaticExternal.ExternalRecordProperties(
  aClass: TSQLRecordClass; aServer: TSQLRestServer): TSQLDBConnectionProperties;
var Static: TSQLRestServerStatic;
begin
  result := nil;
  if (aClass=nil) or (aServer=nil) then
    exit;
  Static := aServer.StaticVirtualTable[aClass];
  if (Static<>nil) and Static.InheritsFrom(TSQLRestServerStaticExternal) then
    result := TSQLRestServerStaticExternal(Static).Properties;
end;

function TSQLRestServerStaticExternal.ExecuteFromJSON(
  const SentData: RawUTF8; Occasion: TSQLOccasion; UpdatedID: integer): integer;
var Decoder: TJSONObjectDecoder;
    SQL: RawUTF8;
    Types: array[0..MAX_SQLFIELDS-1] of TSQLDBFieldType;
    InsertedID, F,k: integer;
    Query: ISQLDBStatement;
    P: PUTF8Char;
begin
  result := 0;
  Lock(false); // avoid race condition against max(ID)
  try
    case Occasion of
    soInsert: begin
      InsertedID := JSONRetrieveIDField(pointer(SentData));
      if InsertedID=0 then // now "ID":... field value forced
        InsertedID := EngineLockedNextID;
    end;
    soUpdate:
      if UpdatedID<>0 then
        InsertedID := 0 else
        raise ESQLDBException.CreateFmt('ExecuteFromJSON(soUpdate) called with UpdatedID=%d',[UpdatedID]);
    else raise ESQLDBException.Create('Invalid ExecuteFromJSON occasion');
    end;
    // decode fields
    Decoder.Decode(SentData,nil,pNonQuoted,InsertedID,true);
    if (Decoder.FieldCount=0) and (Occasion=soUpdate) then begin
      result := UpdatedID; // SentData='' -> no column to update
      exit;
    end; 
    for F := 0 to Decoder.FieldCount-1 do begin
      k := fFieldsDynArray.FindHashed(Decoder.FieldNames[F]);
      if k<0 then
        exit; // JSON field does not exist in the external DB table
      Types[F] := fFields[k].ColumnType;
    end;
    // compute SQL statement and associated bound parameters
    SQL := Decoder.EncodeAsSQLPrepared(fTableName,Occasion);
    if Occasion=soUpdate then begin
      Types[Decoder.FieldCount] := ftInt64; // add "where ID=?" parameter
      Decoder.FieldValues[Decoder.FieldCount] := Int32ToUTF8(UpdatedID);
      inc(Decoder.FieldCount);
    end;
    // execute statement
    Query := fProperties.NewThreadSafeStatementPrepared(SQL,false);
    if Query=nil then
      exit;
    try
      for F := 0 to Decoder.FieldCount-1 do
      if F in Decoder.FieldNull then
        Query.BindNull(F+1) else begin
        P := pointer(Decoder.FieldValues[F]);
        case Types[F] of
        ftInt64:    Query.Bind(F+1,GetInt64(P));
        ftDouble:   Query.Bind(F+1,GetExtended(P));
        ftDate:     Query.BindDateTime(F+1,Iso8601ToDateTimePUTF8Char(P));
        ftCurrency: Query.BindCurrency(F+1,StrToCurrency(P));
        ftBlob:     Query.BindBlob(F+1,Decoder.FieldValues[F]);
        ftUTF8:     Query.BindTextU(F+1,Decoder.FieldValues[F]);
        else raise ESQLDBException.CreateFmt(
          'ExecuteFromJSON: Invalid Types[%d]=%d',[F,ord(result)]);
        end;
      end;
      Query.ExecutePrepared;
    except
      exit; // leave result=0
    end;
    // mark success
    if UpdatedID=0 then
      result := InsertedID else
      result := UpdatedID;
  finally
    UnLock;
  end;
end;

procedure TSQLRestServerStaticExternal.EndCurrentThread(Sender: TObject);
begin
  if fProperties.InheritsFrom(TSQLDBConnectionPropertiesThreadSafe) then
    TSQLDBConnectionPropertiesThreadSafe(fProperties).EndCurrentThread;
end;



{ TSQLVirtualTableCursorExternal }

function TSQLVirtualTableCursorExternal.Column(aColumn: integer;
  var aResult: TVarData): boolean;
var n: cardinal;
begin
  result := false;
  if (self<>nil) and (fStatement<>nil) then
  try
    n := fStatement.ColumnCount-1;
    if aColumn=VIRTUAL_TABLE_ROWID_COLUMN then
      aColumn := n else // RowID in latest column (select %,RowID from..)
      if cardinal(aColumn)>=n then
        exit; // error if aColumn is out of range
    result := fStatement.ColumnToVarData(aColumn,aResult,fColumnTemp)<>ftUnknown;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLVirtualTableCursorExternal.HasData: boolean;
begin
  result := (self<>nil) and (fStatement<>nil) and fHasData;
end;

function TSQLVirtualTableCursorExternal.Next: boolean;
begin
  result := false;
  if (self<>nil) and (fStatement<>nil) then
  try
    fHasData := fStatement.Step;
    result := true; // success (may be with no more data)
  except
    on Exception do
      fHasData := false; // returns false on error + HasData=false
  end;
end;

const
  SQL_OPER_WITH_PARAM: array[soEqualTo..soGreaterThanOrEqualTo] of RawUTF8 = (
    '=?','<>?','<?','<=?','>?','>=?');

function TSQLVirtualTableCursorExternal.Search(
  const Prepared: TSQLVirtualTablePrepared): boolean;
var i: integer;
    SQL: RawUTF8;
    Params: TVarDataDynArray;
begin
  result := false;
  if (Self=nil) or (fStatement<>nil) or (Table=nil) or (Table.Static=nil) then
    exit;
  with Table.Static as TSQLRestServerStaticExternal do begin
    // compute the SQL query corresponding to this prepared request
    SQL := fSelectAllDirectSQL;
    if Prepared.WhereCount<>0 then begin
      SetLength(Params,Prepared.WhereCount);
      for i := 0 to Prepared.WhereCount-1 do
      with Prepared.Where[i] do begin
        if Operation>high(SQL_OPER_WITH_PARAM) then
          exit; // invalid specified operator -> abort search
        if i=0 then
          SQL := SQL+' where ' else
          SQL := SQL+' and ';
        if StoredClassRecordProps.AppendFieldName(Column,SQL,true) then
          exit; // invalid column index -> abort search
        SQL := SQL+SQL_OPER_WITH_PARAM[Operation];
        move(Value,Params[i],SizeOf(Value)); // fast copy bound parameter value
      end;
    end;
    // e.g. 'select FirstName,..,ID from PeopleExternal where FirstName=? and LastName=?'
    for i := 0 to Prepared.OrderByCount-1 do
    with Prepared.OrderBy[i] do begin
      if i=0 then
        SQL := SQL+' order by ' else
        SQL := SQL+', ';
      if StoredClassRecordProps.AppendFieldName(Column,SQL,true) then
        exit; // invalid column index -> abort search
      if Desc then
        SQL := SQL+' desc';
    end;
    // execute the SQL statement
    try
      fStatement := fProperties.NewThreadSafeStatementPrepared(SQL,true);
      if fStatement<>nil then begin
        fStatement.Bind(Params);
        fStatement.ExecutePrepared;
        result := Next; // on execution success, go to the first row
      end;
    except
      on Exception do
        fStatement := nil;
    end;
  end;
end;


{ TSQLVirtualTableExternal }

function TSQLVirtualTableExternal.Delete(aRowID: Int64): boolean;
begin
  result := (self<>nil) and (Static<>nil) and
    (Static as TSQLRestServerStaticExternal).EngineDelete(Static.StoredClass,aRowID);
end;

function TSQLVirtualTableExternal.Drop: boolean;
begin
  if (self=nil) or (Static=nil) then
    result := false else
    with Static as TSQLRestServerStaticExternal do
      result := ExecuteDirect('drop table %',[fTableName],[],false)<>nil;
end;

class procedure TSQLVirtualTableExternal.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWrite]; 
  aProperties.CursorClass := TSQLVirtualTableCursorExternal;
  aProperties.StaticClass := TSQLRestServerStaticExternal;
end;

function TSQLVirtualTableExternal.Insert(aRowID: Int64;
  var Values: TVarDataDynArray; out insertedRowID: Int64): boolean;
begin // aRowID is just ignored here since IDs are always auto calculated
  result := false;
  if (self<>nil) and (Static<>nil) then
  with Static as TSQLRestServerStaticExternal do begin
    Lock(false); // to avoid race condition against max(RowID)
    try
      insertedRowID := EngineLockedNextID;
      result := ExecuteDirectVarData('insert into % (%,ID) values (%,?)',
        [fTableName,StoredClassProps.SQL.InsertSet,CSVOfValue('?',length(Values))],
        Values,insertedRowID);
    finally
      UnLock;
    end;
  end;
end;

function TSQLVirtualTableExternal.Prepare(var Prepared: TSQLVirtualTablePrepared): boolean;
var i, col: integer;
    hasIndex: boolean;
    Fields: TSQLPropInfoList;
begin
  result := inherited Prepare(Prepared); // Prepared.EstimatedCost := 1E10;
  if result and (Static<>nil) then
  with Static as TSQLRestServerStaticExternal do begin
    // mark Where[] clauses will be handled by SQL
    Fields := StoredClassRecordProps.Fields;
    result := false;
    for i := 0 to Prepared.WhereCount-1 do
      with Prepared.Where[i] do
      if (Column<>VIRTUAL_TABLE_IGNORE_COLUMN) and
         (Operation<=high(SQL_OPER_WITH_PARAM)) then begin
        if Column=VIRTUAL_TABLE_ROWID_COLUMN then // is an indexed primary key
          hasIndex := true else begin
          if cardinal(Column)>=cardinal(Fields.Count) then
            exit; // invalid column index -> abort query
          col := fFieldsDynArray.FindHashed(Fields.List[Column].Name);
          if col<0 then
            exit; // column not known in the external database -> abort query
          hasIndex := fFields[col].ColumnIndexed;
        end;
        OmitCheck := true; // search handled via SQL query
        Value.VType := varAny; // *CursorExternal.Search() method expects value
        if hasIndex then // the more indexes, the faster
          Prepared.EstimatedCost := Prepared.EstimatedCost/100;
      end;
    // check the OrderBy[] clauses
    if Prepared.OrderByCount>0 then begin
      for i := 0 to Prepared.OrderByCount-1 do
        with Prepared.OrderBy[i] do
        if (Column<>VIRTUAL_TABLE_ROWID_COLUMN) and
           (cardinal(Column)>=cardinal(Fields.Count)) then
          exit; // invalid column index -> abort query
      Prepared.OmitOrderBy := true; // order handled via SQL query
    end;
    result := true; // success
  end;
end;

function TSQLVirtualTableExternal.Update(oldRowID, newRowID: Int64;
  var Values: TVarDataDynArray): boolean;
begin
  if (self<>nil) and (Static<>nil) and
     (oldRowID=newRowID) and (newRowID>0) then // don't allow ID change
    with Static as TSQLRestServerStaticExternal do
      result := ExecuteDirectVarData('update % set % where ID=?',
        [fTableName,StoredClassProps.SQL.UpdateSet[true]], // [true]=all fields
        Values,oldRowID) else
    result := false;
end;



initialization
  // all our SynDB related functions shall log to main TSQLLog
  SynDBLog := TSQLLog;
end.