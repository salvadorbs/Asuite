unit PerfMain;

interface

{$I Synopse.inc}

{.$define USENEXUSDB}
{.$define USEBDE}
{.$define USEUNIDAC}
{.$define USEZEOS}
{$ifdef CPU64}
  {$undef USENEXUSDB} // NexusDB is not yet 64 bit ready :(
{$endif}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Contnrs, ShellApi,
  SynCommons, SynSQLite3, SynSQLite3Static, 
  mORMot, mORMotSQLite3, mORMotDB,
  SynDB, SynDBSQLite3, SynDBOracle, SynOleDB, SynDBODBC,
  {$ifdef USENEXUSDB}SynDBNexusDB,{$endif}
  {$ifdef USEBDE}SynDBBDE,{$endif}
  {$ifdef USEUNIDAC}SynDBUniDAC,
    SQLiteUniProvider, InterbaseUniProvider, OracleUniProvider, {$endif}
  {$ifdef USEZEOS}SynDBZeos,
    ZDbcSqLite, ZdbcOracle, ZDbcInterbase6, {$endif}
  SynDBFirebird;

type
  TMainForm = class(TForm)
    LogMemo: TMemo;
    OraTNSName: TEdit;
    OraUser: TEdit;
    OraPass: TEdit;
    Label1: TLabel;
    BtnRunTests: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnRunTestsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Ini: RawUTF8;
    Stats: TObjectList;
    procedure SaveStats;
  public
    procedure Test(PropsClass: TSQLDBConnectionPropertiesClass;
      const aServerName, aDatabaseName, aUserID, aPassWord, aTrailDesc: RawUTF8;
      DBIsFile: boolean; Mode: TSQLSynchronousMode=smNormal);
  end;

var
  MainForm: TMainForm;

implementation

uses DateUtils;

{$R *.dfm}

// if defined, will create two "stored false" properties, to test UNIQUE columns
{.$define UNIK}

type
  TStat = class(TPersistent)
  private
    fCreateTable: RawUTF8;
    fNumberOfElements: integer;
    fInsertTime: RawUTF8;
    fEngine: RawUTF8;
    fClientCloseTime: RawUTF8;
    fInsertRate: integer;
    fReadOneByOneTime: RawUTF8;
    fReadOneByOneRate: integer;
    fInsertBatchTransactionRate: integer;
    fInsertTransactionRate: integer;
    fInsertBatchRate: integer;
    fInsertBatchTransactionTime: RawUTF8;
    fInsertTransactionTime: RawUTF8;
    fInsertBatchTime: RawUTF8;
    fReadAllVirtualRate: integer;
    fReadAllDirectRate: integer;
    fReadAllDirectTime: RawUTF8;
    fReadAllVirtualTime: RawUTF8;
    {$ifdef UNIK}
    fReadOneByNameRate: integer;
    fReadOneByNameTime: RawUTF8;
    {$endif}
  published
    property Engine: RawUTF8 read fEngine;
    property CreateTableTime: RawUTF8 read fCreateTable;
    property NumberOfElements: integer read fNumberOfElements;
    property InsertTime: RawUTF8 read fInsertTime;
    property InsertRate: integer read fInsertRate;
    property InsertBatchTime: RawUTF8 read fInsertBatchTime;
    property InsertBatchRate: integer read fInsertBatchRate;
    property InsertTransactionTime: RawUTF8 read fInsertTransactionTime;
    property InsertTransactionRate: integer read fInsertTransactionRate;
    property InsertBatchTransactionTime: RawUTF8 read fInsertBatchTransactionTime;
    property InsertBatchTransactionRate: integer read fInsertBatchTransactionRate;
    property ReadOneByOneTime: RawUTF8 read fReadOneByOneTime;
    property ReadOneByOneRate: integer read fReadOneByOneRate;
    {$ifdef UNIK}
    property ReadOneByNameTime: RawUTF8 read fReadOneByNameTime;
    property ReadOneByNameRate: integer read fReadOneByNameRate;
    {$endif}
    property ReadAllVirtualTime: RawUTF8 read fReadAllVirtualTime;
    property ReadAllVirtualRate: integer read fReadAllVirtualRate;
    property ReadAllDirectTime: RawUTF8 read fReadAllDirectTime;
    property ReadAllDirectRate: integer read fReadAllDirectRate;
    property ClientCloseTime: RawUTF8 read fClientCloseTime;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Ini := StringFromFile(ChangeFileExt(paramstr(0),'.ini'));
  OraTNSName.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','TNSName'));
  OraUser.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','User'));
  OraPass.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','Password'));
  Stats := TObjectList.Create;
end;

procedure TMainForm.BtnRunTestsClick(Sender: TObject);
var T,U,P: RawUTF8;
    Suffix: TFileName;
begin
  {$ifdef CPU64}
  Suffix := '64';
  {$endif}
  ExeVersionRetrieve;
  //SynDBLog.Family.Level := LOG_VERBOSE;  // for debugging
  T := StringToUTF8(OraTNSName.Text);
  U := StringToUTF8(OraUser.Text);
  P := StringToUTF8(OraPass.Text);
  UpdateIniEntry(Ini,'Oracle','TNSName',T);
  UpdateIniEntry(Ini,'Oracle','User',U);
  UpdateIniEntry(Ini,'Oracle','Password',P);
  FileFromString(Ini,ChangeFileExt(paramstr(0),'.ini'));
  LogMemo.Clear;
{  FreeAndNil(sqlite3);
  sqlite3 := TSQLite3LibraryDynamic.Create('sqlite3.dll'); }
  try
    //(*
    Test(nil,'','','','','SQLite3 (file full)',true,smFull);
    Test(nil,'','','','','SQLite3 (file off)',true,smOff);
    Test(nil,':memory:','','','','SQLite3 (mem)',false);
    Test(nil,'static','','','','TObjectList (static)',true);
    Test(nil,'SQL','','','','TObjectList (virtual)',true);
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext full)',true,smFull);
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext off)',true,smOff);
    Test(TSQLDBSQLite3ConnectionProperties,':memory:','','','',' (ext mem)',true);
    {$ifdef USEUNIDAC} // smFull -> only 50 rows if no batch mode
    Test(TSQLDBUniDACConnectionProperties,UNIDAC_PROVIDER[dSQLite],'','','',' SQlite3',true,smFull);
    {$endif}
    {$ifdef USEZEOS} // smFull -> only 50 rows if no batch mode
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(dSQLite
      {$ifdef CPU64},'sqlite3-64.dll'{$endif}),'','','',' SQlite3',true,smFull);
    {$endif}
    //*)
    //Test(TSQLDBFirebirdEmbeddedConnectionProperties,'','','','','',false);
    {$ifdef USEZEOS}
    Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(dFirebird,
      'Firebird'+suffix+'\fbembed.dll'),'','','',' Firebird',true);
    {$endif}
    {$ifdef USEUNIDAC}
    Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(dFirebird,
      'Firebird'+suffix+'\fbembed.dll'),'','','',' Firebird',true);
    {$endif}
    {$ifndef CPU64} // Jet is not available on Win64 platform
    Test(TOleDBJetConnectionProperties,'','','','','',true);
    {$endif}
    {$ifdef USENEXUSDB}
    Test(TSQLDBNexusDBConnectionProperties,'.\nexusDB','','','','',false);
    {$endif}
    if T<>'' then begin
      {$ifdef CPU64}
      SynDBOracleOCIpath := 'oci64';
      {$endif}
      Test(TSQLDBOracleConnectionProperties,T,'',U,P,'',false);
      {$ifndef CPU64}
      Test(TODBCConnectionProperties,TSQLDBOracleConnectionProperties.ExtractTnsName(T),
        '',U,P,' Oracle',false);
      {$endif}
      {$ifdef USEBDE}
      Test(TSQLDBBDEConnectionProperties,T,'',U,P,' Oracle',false);
      {$endif}
      {$ifdef USEZEOS}
      Test(TSQLDBZEOSConnectionProperties,TSQLDBZEOSConnectionProperties.URI(dOracle
        {$ifdef CPU64},'oci64\oci.dll'{$endif}),T,U,P,' Oracle',false);
      {$endif}
      {$ifdef USEUNIDAC}
      Test(TSQLDBUniDACConnectionProperties,TSQLDBUniDACConnectionProperties.URI(dOracle
        {$ifdef CPU64},'oci64\oci.dll'{$endif}),T,U,P,' Oracle',false);
      {$endif}
    end;
//*)
  except
    on E: Exception do
      LogMemo.Lines.Add(E.Message);
  end;
  Label3.Caption := '';
  T := ObjectToJSON(Stats,[woHumanReadable]);
  FileFromString(T,ChangeFileExt(paramstr(0),'.stats'));
  FileFromString(T,Ansi7ToString(NowToString(false))+'.log');
  SaveStats;
end;

type
  TSQLRecordSample = class(TSQLRecord)
  private
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fAmount: Currency;
    fBirthDate: TDateTime;
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUTF8 index 40 read fFirstName write fFirstName
      {$ifdef UNIK}stored false{$endif};
    property LastName: RawUTF8 index 40 read fLastName write fLastName
      {$ifdef UNIK}stored false{$endif};
    property Amount: Currency read fAmount write fAmount;
    property BirthDate: TDateTime read fBirthDate write fBirthDate;
    property LastChange: TModTime read fLastChange;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;

procedure TMainForm.Test(PropsClass: TSQLDBConnectionPropertiesClass;
  const aServerName, aDatabaseName, aUserID, aPassWord, aTrailDesc: RawUTF8;
  DBIsFile: boolean; Mode: TSQLSynchronousMode);
var aUseBatch, aUseTransactions, aUseDirect: boolean;
    Props: TSQLDBConnectionProperties;
    Model: TSQLModel;
    Client: TSQLRestClientDB;
    Value: TSQLRecordSample;
    ValueLastName, ValueFirstName: TRawUTF8DynArray;
    Stat: TStat;
    Start: TTimeLog;
    Timer: TPrecisionTimer;
    Res: TIntegerDynArray;
    U, Server,DBName, MainDBName, Num, Time: RawUTF8;
    Rate, i: integer;

  procedure ValueCheck;
  var err: RawUTF8;
  begin
    err := '';
    if Value.fID<>Res[i] then
      err := FormatUTF8('Value.fID=Res[i] %<>% ',[Value.fID,Res[i]]);
    if PInt64(@Value.Amount)^<>(i+1)*100 then
      err := FormatUTF8('%Value.Amount=(i+1)*0.01 %<>% ',
        [err,Value.Amount,(i+1)*0.01]);
    if Value.LastChange<Start then
      err := FormatUTF8('%Value.LastChange>=Start %>=%',
        [err,Value.LastChange,Start]);
    if Value.FirstName<>ValueFirstName[i] then
      err := FormatUTF8('%Value.FirstName=ValueFirstName[i] "%"<>"%"',
        [err,Value.FirstName,ValueFirstName[i]]);
    assert(err='',Stat.fEngine+' read failure: '+err);
    Value.ClearProperties;
  end;
var TableNames: TRawUTF8DynArray;
begin
  U := 'Namee ';
  PWord(@U[4])^ := $a9c3;  // some 'e' acute to test UTF-8 encoding
  Stat := TStat.Create;
  Stat.fEngine := PropsClass.EngineName;
  if aTrailDesc<>'' then
    Stat.fEngine := Stat.fEngine+aTrailDesc;
  Model := TSQLModel.Create([TSQLRecordSample]);
  Value := TSQLRecordSample.Create;
  Num := '1';
  for aUseTransactions := false to true do
  for aUseBatch := false to true do begin
    // open connection and initialize mORMot Client-Server instance
    Label3.Caption := Format('Running tests phase #%s on %s...',[Num,Stat.fEngine]);
    Application.ProcessMessages;
    DBName := aDatabaseName;
    if aServerName='' then begin
      Server := LowerCaseU(Stat.Engine)+'.'+Num;
      DeleteFile(UTF8ToString(Server));
    end else begin
      Server := aServerName;
      if DBIsFile and (DBName='') then begin
        DBName := LowerCaseU(Stat.Engine)+'.'+Num;
        DeleteFile(UTF8ToString(DBName));
      end;
    end;
    if PropsClass<>nil then begin
      MainDBName := ':memory:';
      Props := PropsClass.Create(Server,DBName,aUserID,aPassWord);
      {$ifdef USENEXUSDB}
      if PropsClass=TSQLDBNexusDBConnectionProperties then
        TSQLDBNexusDBConnectionProperties(Props).DeleteDatabase;
      {$endif}
    end else begin
      MainDBName := Server;
      Props := nil;
    end;
    try
      if Server='SQL' then begin
        MainDBName := ':memory:';
        Model.VirtualTableRegister(TSQLRecordSample,TSQLVirtualTableBinary);
      end else
        VirtualTableExternalRegister(Model,TSQLRecordSample,Props,'SampleRecord');
      Client := TSQLRestClientDB.Create(Model,nil,string(MainDBName),TSQLRestServerDB,false,'');
      if Server='static' then begin
        DeleteFile('static.data');
        Client.Server.StaticDataCreate(TSQLRecordSample,'static.data',true);
      end;
      Client.Server.DB.Synchronous := Mode;
      if PropsClass=TSQLDBSQLite3ConnectionProperties then
        TSQLDBSQLite3Connection(Props.MainConnection).Synchronous := Mode;
      try
        // huge insertion in virtual table, with 4 kinds of process
        Timer.Start;
        Client.Server.CreateMissingTables;
{        Props.ExecuteNoResult(
          'insert into SampleRecord (ID,FirstName,LastName,Amount,LastChange,CreatedAt) VALUES (?,?,?,?,?,?)',
          [1,U,'B',10.02,10,20]);
          //'insert into SampleRecord (ID,BirthDate) values (?,null)',[1.0]);
        U := Props.Execute('select * from samplerecord',[]).FetchAllAsJSON(true); }
        Start := Client.ServerTimeStamp;
        if Stat.CreateTableTime='' then
          Stat.fCreateTable := Timer.Stop;
        if (Mode=smFull) and not aUseTransactions then
          Stat.fNumberOfElements := 500 else // SQLite3 is dead slow without transactions
        {if (PropsClass=TOleDBJetConnectionProperties) or
           (PropsClass=TODBCConnectionProperties) then
          Stat.fNumberOfElements := 1000 else}
          Stat.fNumberOfElements := 5000;
        //Stat.fNumberOfElements := 5;
        SetLength(ValueLastName,Stat.fNumberOfElements);
        SetLength(ValueFirstName,Stat.fNumberOfElements);
        for i := 0 to Stat.fNumberOfElements-1 do begin
          ValueLastName[i] := Int32ToUtf8(i+1);
          ValueFirstName[i] := U+ValueLastName[i];
        end;
        Timer.Start;
        if aUseTransactions then
          Client.TransactionBegin(TSQLRecordSample);
        if aUseBatch then
          Client.BatchStart(TSQLRecordSample) else
          SetLength(Res,Stat.fNumberOfElements);
        Value.BirthDate := 0;
        for i := 0 to Stat.fNumberOfElements-1 do begin
          Value.Amount := (i+1)*0.01;
          Value.LastName := ValueLastName[i];
          Value.FirstName := ValueFirstName[i];
          if aUseBatch then
            Client.BatchAdd(Value,true) else
            Res[i] := Client.Add(Value,true);
          Value.BirthDate := Value.BirthDate+1;
        end;
        if aUseBatch then
          Client.BatchSend(Res);
        if aUseTransactions then
          Client.Commit;
        Time := Timer.Stop;
        i := 1;
        Value.ClearProperties;
        Client.Retrieve(Res[i],Value);
        ValueCheck;
        Rate := Timer.PerSec(Stat.fNumberOfElements);
        case Num[1] of
        '1': begin
          Stat.fInsertTime := Time;
          Stat.fInsertRate := Rate;
        end;
        '2': begin
          Stat.fInsertBatchTime := Time;
          Stat.fInsertBatchRate := Rate;
        end;
        '3': begin
          Stat.fInsertTransactionTime := Time;
          Stat.fInsertTransactionRate := Rate;
        end;
        '4': begin
          Stat.fInsertBatchTransactionTime := Time;
          Stat.fInsertBatchTransactionRate := Rate;
          Label3.Caption := Format('Running reading tests on %s...',[Stat.fEngine]);
          Application.ProcessMessages;
          // one by one retrieve values from server
          Timer.Start;
          for i := 0 to Stat.fNumberOfElements-1 do begin
            Client.Retrieve(Res[i],Value);
            ValueCheck;
          end;
          Stat.fReadOneByOneTime := Timer.Stop;
          Stat.fReadOneByOneRate := Timer.PerSec(Stat.fNumberOfElements);
          {$ifdef UNIK}
          // one by one retrieve values using Name property
          Timer.Start;
          for i := 0 to Stat.fNumberOfElements-1 do begin
            Client.Retrieve('LastName=?',[],[ValueLastName[i]],Value);
            assert((Value.fID=Res[i])and
              (PInt64(@Value.Amount)^=(i+1)*100)and(Value.LastChange>=Start));
          end;
          Stat.fReadOneByNameTime := Timer.Stop;
          Stat.fReadOneByNameRate := Timer.PerSec(Stat.fNumberOfElements);
          {$endif}
          // retrieve all rows with or without the virtual module
          for aUseDirect := false to true do begin
            with Client.Server do begin
              Cache.Flush; // fair benchmark
              DB.CacheFlush; // fair benchmark (16100 rows/s->456000 with cache!)
              StaticVirtualTableDirect := aUseDirect;
            end;
            Timer.Start;
            Value.ClearProperties;
            if Server='SQL' then
              Value.FillPrepare(Client,'') else
              Value.FillPrepare(Client,'order by RowId');
            //FileFromString((Value.FillTable as TSQLTableJSON).PrivateInternalCopy,Stat.Engine+'.json');
            i := 0;
            while Value.FillOne do begin
              ValueCheck;
              {if err<>'' then
                FileFromString((Value.FillTable as TSQLTableJSON).PrivateInternalCopy,
                  Stat.fEngine+'.json');}
              inc(i);
            end;
            assert(i=Stat.fNumberOfElements);
            if aUseDirect then begin
              Stat.fReadAllDirectTime := Timer.Stop;
              Stat.fReadAllDirectRate := Timer.PerSec(Stat.fNumberOfElements);
            end else begin
              Stat.fReadAllVirtualTime := Timer.Stop;
              Stat.fReadAllVirtualRate := Timer.PerSec(Stat.fNumberOfElements);
            end;
          end;
{          // backup (for testing purposes)
          if MainDBName<>':memory:' then
            Client.Server.BackupGZ(MainDBName+'.gz'); } 
        end;
        end;
      finally
        Timer.Start;
        try
          if not DBIsFile then
            Client.Server.EngineExecuteAll('drop table '+Value.SQLTableName);
        finally
          Client.Free;
        end;
        Stat.fClientCloseTime := Timer.Stop;
      end;
      inc(Num[1]);
    finally
      Props.Free;
    end;
  end;
  Stats.Add(Stat);
  Model.Free;
  Value.Free;
  LogMemo.Lines.Add(UTF8ToString(ObjectToJSON(Stat,[woHumanReadable])));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Stats.Free;
end;


procedure TMainForm.SaveStats;
type TStatArray = array[0..1000] of TStat;
var Stat: ^TStatArray;
    mode,s,txt: RawUTF8;
    m,nCat: integer;
    max,Cat1,Cat2,Eng1,Eng2,Eng: RawUTF8;
    Rows: TRawUTF8DynArray;
    Doc: RawUTF8;
procedure SetCategories(const Title: RawUTF8; const Cat: array of RawUTF8);
var i: integer;
begin
  mode := UrlEncode(Title);
  s := s+'<h1>'+copy(Title,1,posEx(' (',Title)-1)+'</h1>'#13#10;
  max := Int32ToUtf8(m);
  nCat := length(Cat);
  Cat1 := '';
  Cat2 := '';
  SetLength(Rows,Stats.Count+1);
  Rows[0] := '<td>&nbsp;</td>';
  for i := 0 to high(Cat) do begin
    Rows[0] := Rows[0]+'<td><b>'+Cat[i]+'</b></td>';
    Cat1 := Cat1+UrlEncode(Cat[i])+'|';
    Cat2 := Cat2+UrlEncode(Cat[high(Cat)-i])+'|';
  end;
  SetLength(Cat1,length(Cat1)-1);
  SetLength(Cat2,length(Cat2)-1);
  Eng1 := '';
  Eng2 := '';
  for i := 0 to Stats.Count-1 do begin
    Eng := Stat[i].Engine;
   { j := PosEx(' ',Eng);
    if j>0 then begin
      Delete(Eng,j,1);
      insert('<br>',Eng,j);
    end;}
    Rows[i+1] := '<td><b>'+Eng+'</b></td>';
    Eng1 := Eng1+UrlEncode(Stat[i].Engine)+'|';
    Eng2 := Eng2+UrlEncode(Stat[Stats.Count-1-i].Engine)+'|';
  end;
  SetLength(Eng1,length(Eng1)-1);
  SetLength(Eng2,length(Eng2)-1);
end;
procedure Pic1(const Leg: RawUTF8; n: integer);
var i: integer;
begin
  txt := 'http://chart.apis.google.com/chart?chtt='+mode+'&chxl=1:|'+Leg+
    '&chxt=x,y&chbh=a&chs=600x400&cht=bhg&chco='+
//    '3D7930,3D8930,309F30,6070F0,5070E0,40C355,65D055,80C1A2,F05050,F0A280'+
    '3D7930,3D8930,309F30,6070F0,5070E0,40C355,65D055,80C1A2,3D7930,3D8930,F05050,F04050,F04040,F01040,F0A280'+
    '&chxr=0,0,'+max+'&chds=';
  for i := 1 to n do
    txt := txt+'0,'+max+',';
  txt[length(txt)] := '&';
  txt := txt+'chd=t:';
end;
procedure PicEnd(const Legend: RawUTF8);
begin
  txt[length(txt)] := '&';
  s := s+'<p><img src='+txt+'chdl='+Legend+'></p>'#13#10;
  txt := '';
end;
procedure Table;
var i: integer;
begin
  s := s+'<p><table>';
  for i := 0 to High(Rows) do
    s := s+'<tr align=center>'+Rows[i]+'</tr>'#13#10;
  s := s+'</table></p>';
  Doc := Doc+'|%30';
  for i := 1 to nCat do
    Doc := Doc+'%15';
  Doc := Doc+#13#10;
  for i := 0 to High(Rows) do begin
    Doc := Doc+StringReplaceAll(StringReplaceAll(StringReplaceAll(StringReplaceAll(
      StringReplaceAll(StringReplaceAll(StringReplaceAll(StringReplaceAll(
      Rows[i],'</td>',''),'</tr>',''),'<tr align=center>',''),
      '</b>','}'),'</td>',''),'<b>','{\b '),'<td>','|'),'&nbsp;','')+#13#10;
  end;
  Doc := Doc+'|%'#13#10;
end;
var i: integer;
begin
  Stat := pointer(Stats.List);

  m := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if InsertRate>m then m := InsertRate;
      if InsertBatchRate>m then m := InsertBatchRate;
      if InsertTransactionRate>m then m := InsertTransactionRate;
      if InsertBatchTransactionRate>m then m := InsertBatchTransactionRate;
    end;
  SetCategories('Insertion speed (rows/second)',['Direct','Batch','Trans','Batch Trans']);
  Pic1(Cat2,5);
  for i := 0 to Stats.Count-1 do
  with Stat[i] do begin
    txt := FormatUTF8('%%,%,%,%|',
      [txt,InsertRate,InsertBatchRate,InsertTransactionRate,InsertBatchTransactionRate]);
    Rows[i+1] := FormatUTF8('%<td>%</td><td>%</td><td>%</td><td>%</td>',
      [Rows[i+1],InsertRate,InsertBatchRate,InsertTransactionRate,InsertBatchTransactionRate]);
  end;
  Table;
  PicEnd(Eng1);

  Pic1(Eng2,Stats.Count);
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertBatchRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertTransactionRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertBatchTransactionRate)+',';
  PicEnd(Cat1);

  m := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if ReadOneByOneRate>m then m := ReadOneByOneRate;
      {$ifdef UNIK}
      if ReadOneByNameRate>m then m := ReadOneByNameRate;
      {$endif}
      if ReadAllVirtualRate>m then m := ReadAllVirtualRate;
      if ReadAllDirectRate>m then m := ReadAllDirectRate;
    end;
  SetCategories('Read speed (rows/second)',['By one',
    {$ifdef UNIK}'By name',{$endif}'All Virtual','All Direct']);
  Pic1(Cat2,{$ifdef UNIK}4{$else}3{$endif});
  for i := 0 to Stats.Count-1 do
  with Stat[i] do begin
    txt := FormatUTF8({$ifdef UNIK}'%%,%,%,%|'{$else}'%%,%,%|'{$endif},
      [txt,ReadOneByOneRate,{$ifdef UNIK}ReadOneByNameRate,{$endif}
        ReadAllVirtualRate,ReadAllDirectRate]);
    Rows[i+1] := FormatUTF8('%<td>%</td>',[Rows[i+1],ReadOneByOneRate]);
    {$ifdef UNIK}
    Rows[i+1] := FormatUTF8('%<td>%</td>',[Rows[i+1],ReadOneByNameRate]);
    {$endif}
    Rows[i+1] := FormatUTF8('%<td>%</td><td>%</td>',
      [Rows[i+1],ReadAllVirtualRate,ReadAllDirectRate]);
  end;
  Table;
  PicEnd(Eng1);

  Pic1(Eng2,Stats.Count);
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadOneByOneRate)+',';
  txt[length(txt)] := '|';
  {$ifdef UNIK}
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadOneByNameRate)+',';
  txt[length(txt)] := '|';
  {$endif}
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadAllVirtualRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadAllDirectRate)+',';
  PicEnd(Cat1);

  FileFromString(Doc,ChangeFileExt(paramstr(0),'.txt'));
  FileFromString('<html><body>'#13#10+s,ChangeFileExt(paramstr(0),'.htm'));
end;

procedure TMainForm.FormShow(Sender: TObject);
var Valid: boolean;
    S: RawUTF8;
begin
  exit;
  S := StringFromFile('TEST2.stats');
  JSONToObject(Stats,pointer(S),Valid,TStat);
  if Valid then
    SaveStats;
  Close;
end;

end.
