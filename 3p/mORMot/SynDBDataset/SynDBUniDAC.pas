/// UniDAC-based classes for SynDB units
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBUniDAC;

{
  This file is part of Synopse framework.

  Synopse framework. Copyright (C) 2013 Arnaud Bouchez
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

  Version 1.18
  - first public release, corresponding to mORMot framework 1.18


  Todo:

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows, SysUtils,
  {$IFNDEF DELPHI5OROLDER}
  Variants,
  {$ENDIF}
  Classes, Contnrs,
  SynCommons,
  SynDB,
  SynDBDataset,
  Uni,
  UniProvider,
  UniScript;
  

{ -------------- UniDAC database access }

type
  /// Exception type associated to UniDAC database access
  ESQLDBUniDAC = class(ESQLDBDataset);


  ///	connection properties definition using UniDAC database access
  TSQLDBUniDACConnectionProperties = class(TSQLDBDatasetConnectionProperties)
  protected
    fSpecificOptions: TStringList;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (UniDAC metadata may be used in the future)
    procedure GetForeignKeys; override;
  public
    /// initialize the properties to connect via UniDAC database access
    // - aServerName shall contain the UniDAC provider name, e.g. 'Oracle' - you
    // can use the TSQLDBUniDACConnectionProperties.URI() to retrieve the
    // provider name from its SynDB.TSQLDBDefinition enumeration, and optionally
    // set some options, which will be added to the internal SpecificOptions[]
    // - aDatabaseName shall contain the database server name
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// release internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overriden method will create an TSQLDBUniDACConnection instance
    function NewConnection: TSQLDBConnection; override;

    /// compute the UniDAC URI from a given database engine
    // - you can set an optional full path to the client library name,
    // to be completed on the left side with the executable path
    // - possible use may be:
    // ! PropsOracle := TSQLDBUniDACConnectionProperties.Create(
    // !   TSQLDBUniDACConnectionProperties.URI(dOracle,'oci64\oci.dll'),
    // !   'tnsname','user',pass');
    // ! PropsFirebird := TSQLDBUniDACConnectionProperties.Create(
    // !   TSQLDBUniDACConnectionProperties.URI(dFirebird,'Firebird\fbembed.dll'),
    // !   'databasefilename','',');
    class function URI(aServer: TSQLDBDefinition;
      const aLibraryLocation: TFileName='';
      aLibraryLocationAppendExePath: boolean=true): RawUTF8;

    /// retrieve the column/field layout of a specified table
    // - this overriden method will use UniDAC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray); override;
    /// get all table names
    // - this overriden method will use UniDAC metadata to retrieve the information
    procedure GetTableNames(var Tables: TRawUTF8DynArray); override;

    /// allow to set the options specific to a UniDAC driver
    // - for instance, you can set for both SQLite3 and Firebird/Interbase:
    // ! Props.SpecificOptions.Values['ClientLibrary'] := ClientDllName;
    property SpecificOptions: TStringList read fSpecificOptions;
  end;


  ///	implements a direct connection via UniDAC database access
  TSQLDBUniDACConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: TUniConnection;
  public
    /// prepare a connection for a specified UniDAC database access
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified database server using UniDAC
    // - should raise an ESQLDBUniDAC on error
    procedure Connect; override;
    /// stop connection to the specified database server using UniDAC
    // - should raise an ESQLDBUniDAC on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated UniDAC connection instance
    property Database: TUniConnection read fDatabase;
  end;

  ///	implements a statement via a UniDAC connection
  TSQLDBUniDACStatement = class(TSQLDBDatasetStatement)
  protected
    /// initialize and set fQuery: TUniQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    procedure DatasetPrepare(const aSQL: string); override;
    /// execute underlying TUniQuery.ExecSQL
    procedure DatasetExecSQL; override;
  public
  end;


const
  /// UniDAC provider names corresponding to SynDB recognized SQL engines
  UNIDAC_PROVIDER: array[dOracle..high(TSQLDBDefinition)] of RawUTF8 = (
    'Oracle','SQL Server','Access','MySQL','SQLite','InterBase','NexusDB');

implementation

uses
  DAScript, DBAccess;


{ TSQLDBUniDACConnectionProperties }

constructor TSQLDBUniDACConnectionProperties.Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
var p: TSQLDBDefinition;
    server,options,namevalue: RawUTF8;
    opt: PUTF8Char;
begin
  Split(aServerName,'?',server,options);
  for p := Low(UNIDAC_PROVIDER) to high(UNIDAC_PROVIDER) do
    if SameTextU(UNIDAC_PROVIDER[p],server) then begin
      fDBMS := p;
      break;
    end;
  inherited Create(server,aDatabaseName,aUserID,aPassWord);
  fSpecificOptions := TStringList.Create;
  opt := pointer(options);
  while opt<>nil do begin
    namevalue := GetNextItem(opt,';');
    if namevalue<>'' then
      fSpecificOptions.Add(UTF8ToString(namevalue));
  end;
  case fDBMS of
  dSQLite: begin // UniDAC support of SQLite3 is just buggy
    fSpecificOptions.Values['ForceCreateDatabase'] := 'true';
    fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for UniDAC
  end;
  dFirebird: begin
    {$ifndef UNICODE}
    fForceUseWideString := true;
    {$endif}
    fSpecificOptions.Values['CharSet'] := 'UTF8';
    fSpecificOptions.Values['UseUnicode'] := 'true';
    fSpecificOptions.Values['CharLength'] := '2';
    fSpecificOptions.Values['DescribeParams'] := 'true';
  end;
  end;
end;

destructor TSQLDBUniDACConnectionProperties.Destroy;
begin
  fSpecificOptions.Free;
  inherited;
end;

procedure TSQLDBUniDACConnectionProperties.GetFields(
  const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray);
var meta: TDAMetaData;
    n: integer;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
    hasSubType: boolean;
begin
  meta := (MainConnection as TSQLDBUniDACConnection).fDatabase.CreateMetaData;
  try
    FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
    FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
    FillChar(F,sizeof(F),0);
    meta.MetaDataKind := 'Columns';
    meta.Restrictions.Values['TABLE_NAME'] := UTF8ToString(UpperCase(aTableName));
    meta.Open;
    hasSubType := meta.FindField('DATA_SUBTYPE')<>nil;
    while not meta.Eof do begin
      F.ColumnName := StringToUTF8(meta.FieldByName('COLUMN_NAME').AsString);
      F.ColumnTypeNative := StringToUTF8(meta.FieldByName('DATA_TYPE').AsString);
      if hasSubType then
        F.ColumnTypeNative := F.ColumnTypeNative+
          StringToUTF8(meta.FieldByName('DATA_SUBTYPE').AsString);
      F.ColumnLength := meta.FieldByName('DATA_LENGTH').AsInteger;
      F.ColumnScale := meta.FieldByName('DATA_SCALE').AsInteger;
      F.ColumnPrecision := meta.FieldByName('DATA_PRECISION').AsInteger;
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative,F.ColumnScale);
      FA.Add(F);
      meta.Next;
    end;
    Setlength(Fields,n);
    exit;
  finally
    meta.Free;
  end;
end;

procedure TSQLDBUniDACConnectionProperties.GetForeignKeys;
var conn: TUniConnection;
begin
  conn := (MainConnection as TSQLDBUniDACConnection).Database;
  if conn=nil then
    exit;
  { TODO : get FOREIGN KEYS from UniDAC metadata ? }
end;

procedure TSQLDBUniDACConnectionProperties.GetTableNames(
  var Tables: TRawUTF8DynArray);
var List: TStringList;
    i: integer;
begin
  List := TStringList.Create;
  try
    (MainConnection as TSQLDBUniDACConnection).fDatabase.GetTableNames(List,false,true);
    SetLength(Tables,List.Count);
    for i := 0 to List.Count-1 do
      Tables[i] := StringToUTF8(List[i]);
    exit;
  finally
    List.Free;
  end;
  inherited;
end;

function TSQLDBUniDACConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBUniDACConnection.Create(self);
end;

class function TSQLDBUniDACConnectionProperties.URI(aServer: TSQLDBDefinition;
  const aLibraryLocation: TFileName;
  aLibraryLocationAppendExePath: boolean): RawUTF8;
begin
  if aServer<low(UNIDAC_PROVIDER) then
    result := '' else
    result := UNIDAC_PROVIDER[aServer];
  if (aLibraryLocation='') or (result='') then
    exit;
  result := result+'?ClientLibrary=';
  if aLibraryLocationAppendExePath then
    result := result+StringToUTF8(ExtractFilePath(ParamStr(0)));
  result := result+StringToUTF8(aLibraryLocation);
end;


{ TSQLDBUniDACConnection }

procedure TSQLDBUniDACConnection.Commit;
begin
  inherited Commit;
  fDatabase.Commit;
end;

constructor TSQLDBUniDACConnection.Create(aProperties: TSQLDBConnectionProperties);
begin
  inherited Create(aProperties);
  fDatabase := TUniConnection.Create(nil);
  fDatabase.ProviderName := UTF8ToString(fProperties.ServerName);
  case aProperties.DBMS of
  dSQLite, dFirebird:
    fDatabase.Database := UTF8ToString(fProperties.DatabaseName);
  else
    fDatabase.Server := UTF8ToString(fProperties.DatabaseName);
  end;
  fDatabase.Username := UTF8ToString(fProperties.UserID);
  fDatabase.Password := UTF8ToString(fProperties.PassWord);
  fDatabase.SpecificOptions.AddStrings(
    (fProperties as TSQLDBUniDACConnectionProperties).fSpecificOptions);
end;

procedure TSQLDBUniDACConnection.Connect;
var Log: ISynLog;
begin
  if fDatabase=nil then
    raise ESQLDBUniDAC.CreateFmt('TSQLDBUniDACConnection.Connect(%s): Database=nil',
      [fProperties.ServerName]);
  Log := SynDBLog.Enter(Self,pointer(FormatUTF8('Connect to ProviderName=% Database=% on Server=%',
    [fDatabase.ProviderName,fDatabase.Database,fDatabase.Server])),true);
  try
    case fProperties.DBMS of
    dFirebird:
    if (fDatabase.Server= '') and not FileExists(fDatabase.Database) then 
    with TUniScript.Create(nil) do // always create database for embedded Firebird
    try
      NoPreconnect := true;
      SQL.Text := UTF8ToString(fProperties.SQLCreateDatabase(fProperties.DatabaseName));
      Connection := fDatabase;
      Execute;
    finally
      Free;
    end;
    end;
    fDatabase.Open;
    inherited Connect; // notify any re-connection 
    Log.Log(sllDB,'Connected to % (%)',
      [fDatabase.ProviderName,fDatabase.ServerVersionFull]);
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSQLDBUniDACConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fDatabase<>nil then
      fDatabase.Close;
  end;
end;

destructor TSQLDBUniDACConnection.Destroy;
begin
  try
   Disconnect;
  except
    on Exception do
  end;
  inherited;
  FreeAndNil(fDatabase);
end;

function TSQLDBUniDACConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and fDatabase.Connected;
end;

function TSQLDBUniDACConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBUniDACStatement.Create(self);
end;

procedure TSQLDBUniDACConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSQLDBUniDACConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;


{ TSQLDBUniDACStatement }

procedure TSQLDBUniDACStatement.DatasetCreate;
begin
  fQuery := TUniQuery.Create(nil);
  TUniQuery(fQuery).Connection := (fConnection as TSQLDBUniDACConnection).Database;
end;

procedure TSQLDBUniDACStatement.DatasetPrepare(const aSQL: string);
begin
  (fQuery as TUniQuery).SQL.Text := aSQL;
  fQueryParams := TUniQuery(fQuery).Params;
end;

procedure TSQLDBUniDACStatement.DatasetExecSQL;
begin
  (fQuery as TUniQuery).Execute;
end;


end.
