unit VCL.cyBDE;

interface

uses Classes, SysUtils, 
     Controls, // Controls.TDate for Delphi 7
     BDE, Db, DbTables;

     function  TablePackTable(Tab: TTable): Boolean;
     // Rebuild indexes and packtable.

     function  TableRegenIndexes(Tab: TTable): Boolean;
     // Regenerate indexes.

     function TableShowDeletedRecords(Tab: TTable; Show: Boolean): Boolean;
     // Show / hide deleted records

     function  TableUndeleteRecord(Tab: TTable): Boolean;
     // Restore deleted record

     function TableAddIndex(Tab: TTable; FieldName: String; FieldExpression: String; IOpt: TIndexOptions): Boolean;
     // Create new index

     function TableDeleteIndex(Tab: TTable; IndexFieldName: String): Boolean;
     // Delete index

     function TableEmptyTable(Tab: TTable): Boolean;
     // Empty table

     function TableFindKey(aTable: TTable; Value: String): Boolean;
     // Make secured FindKey()

     procedure TableFindNearest(aTable: TTable; Value: String);
     // Make secured FindNearest()

     function TableCreate(Owner: TComponent; DataBaseName: ShortString; TableName: String; IndexName: ShortString; ReadOnly: Boolean): TTable;
     // Create a table dynamically

     function TableOpen(Tab: TTable; FileName: String; IndexFieldName: String; RecordIndexValue: Variant; GotoRecordIndexValue: Boolean): Boolean;
     // Open a table on defined index value :

     function DateToBDESQLDate(aDate: TDate; const DateFormat: String = 'dd.mm.yyyy'): String;
     // Convert TDate to understanding BDE Date string

implementation

function TablePackTable(Tab: TTable): Boolean;
var Activ, Excl : Boolean;
begin
  Result := false;
  Activ  := Tab.Active;
  Excl   := Tab.Exclusive;

  try
    if not Tab.Exclusive then
    begin
      Tab.Active    := false;
      Tab.Exclusive := True;
    end;

    if not Tab.Active then
      Tab.Active := True;

    DbiPackTable(Tab.DbHandle, Tab.Handle, Nil, SzDBase, True);
    Result := True;
  except

  end;

  if not Excl then
  begin
    Tab.Active    := false;
    Tab.Exclusive := false;
  end;

  Tab.Active := Activ;
end;

function TableRegenIndexes(Tab: TTable) : Boolean;
var Activ, Excl : Boolean;
begin
  Result := false;
  Activ  := Tab.Active;
  Excl   := Tab.Exclusive;

  try
    if not Tab.Exclusive then
    begin
       Tab.Active    := false;
       Tab.Exclusive := true;
    end;

    if not Tab.Active then
      Tab.Active := true;

    DbiRegenIndexes(Tab.Handle);
    Result := true;
  except

  end;

  if not Excl then
  begin
     Tab.Active    := false;
     Tab.Exclusive := false;
  end;

  Tab.Active := Activ;
end;

function TableShowDeletedRecords(Tab: TTable; Show: Boolean): Boolean;
begin
  Result := false;

  try
    Check(DbiSetProp(hDBIObj(Tab.Handle), CurSOFTDELETEON, LongInt(Show)));
    Tab.Refresh;
    Result := true;
  except
  end;
end;

function TableUndeleteRecord(Tab: TTable): Boolean;
begin
  Result := false;
  Tab.CheckBrowseMode;                         // Need post record to work ...

  try
    Tab.Edit;
    DbiUndeleteRecord(Tab.Handle);
    Tab.CheckBrowseMode; // Save modificatin
    Result := true;
  except

  end;
end;

function TableAddIndex(Tab: TTable; FieldName: String; FieldExpression: String; IOpt: TIndexOptions): Boolean;
var Excl, Activ : Boolean;
    I           : Integer;
begin
  Result := false;
  Activ  := Tab.Active;
  Excl   := Tab.Exclusive;

  try
    Tab.IndexDefs.Update;

    for i := 0 to Tab.IndexDefs.Count - 1 do
      if AnsiUpperCase(Tab.IndexDefs[i].FieldExpression) = AnsiUpperCase(FieldExpression) then
      begin
        Result := True;
        Exit;
      end;

    if not Tab.Exclusive then
    begin
      Tab.Active := false;
      Tab.Exclusive := true;
    end;

    Tab.AddIndex(FieldName, FieldExpression, IOpt);
    Tab.IndexDefs.Update;

    Result := true;
  except
  end;

  if (not Excl) and (Tab.Exclusive) then
  begin
    Tab.Active := false;
    Tab.Exclusive := false;
  end;

  Tab.Active := Activ;
end;

function TableDeleteIndex(Tab: TTable; IndexFieldName: String): Boolean;
var Excl, Activ : Boolean;
    I           : Integer;
begin
  Result := false;
  Activ  := Tab.Active;
  Excl   := Tab.Exclusive;

  try
    if not Tab.Exclusive then
    begin
      Tab.Active := false;
      Tab.Exclusive := true;
    end;

    if AnsiUpperCase(Tab.IndexName) = AnsiUpperCase(IndexFieldName) then
      Tab.IndexName := '';

    if AnsiUpperCase(Tab.IndexFieldNames) = AnsiUpperCase(IndexFieldName) then
      Tab.IndexFieldNames := '';

    Tab.IndexDefs.Update;

    for i := 0 To Tab.IndexDefs.Count - 1 do
      if AnsiUpperCase(Tab.IndexDefs[i].Name) = AnsiUpperCase(IndexFieldName) then
      begin
        Tab.DeleteIndex(IndexFieldName);
        Result := true;
        Break;
      end;
  except
  end;

  if not Excl then
  begin
    Tab.Active := false;
    Tab.Exclusive := false;
  end;

  Tab.Active := Activ;
end;

function TableEmptyTable(Tab: TTable): Boolean;
var Activ, Excl: Boolean;
begin
  Result := false;
  Activ  := Tab.Active;
  Excl   := Tab.Exclusive;

  try
    if not Tab.Exclusive then
    begin
      Tab.Active    := false;
      Tab.Exclusive := true;
    end;

    if not Tab.Active then
      Tab.Active := true;

    Tab.EmptyTable;
    Result := true;
  except
  end;

  if not Excl then
  begin
    Tab.Active := false;
    Tab.Exclusive := false;
  end;

  Tab.Active := Activ;
end;

function TableFindKey(aTable: TTable; Value: String): Boolean;
begin
  try
    Result := aTable.FindKey([Value]);
  except
    Result := false;
  end;
end;

procedure TableFindNearest(aTable: TTable; Value: String);
begin
  try
    aTable.FindNearest([Value]);
  except

  end;
end;

function TableCreate(Owner: TComponent; DataBaseName: ShortString; TableName: String; IndexName: ShortString; ReadOnly: Boolean): TTable;
begin
  Result := TTable.Create(Owner);
  Result.DatabaseName := DataBaseName;
  Result.TableName    := TableName;
  Result.IndexName    := IndexName;
  Result.ReadOnly     := ReadOnly;
end;

function TableOpen(Tab: TTable; FileName: String; IndexFieldName: String; RecordIndexValue: Variant; GotoRecordIndexValue: Boolean): Boolean;
begin
  Result := false;

  Tab.Active       := false;
  Tab.TableName    := FileName;
  Tab.IndexName    := IndexFieldName;
  Tab.Filtered     := false;
  Tab.MasterFields := '';

  try
    Tab.Active     := True;

    if (IndexFieldName <> '') and (GotoRecordIndexValue)
    then Result := Tab.FindKey([RecordIndexValue])
    else Result := True;
  except

  end;
end;

function DateToBDESQLDate(aDate: TDate; const DateFormat: String = 'dd.mm.yyyy'): String;
begin
  Result := FormatDateTime(DateFormat, aDate);
end;                                               

end.
