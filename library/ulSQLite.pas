{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit ulSQLite;

interface

uses
  ulAppConfig, VirtualTrees, Sqlite3DS;

function  SaveASuiteSQLite(Tree: TBaseVirtualTree; DeleteAll: boolean = false): Boolean;
procedure LoadASuiteSQLite(Tree: TBaseVirtualTree; Import: Boolean);

function ReadIntegerSQLite(dsTable: TSqlite3Dataset;FieldName: string;Default: Integer = -1): Integer;
function ReadBooleanSQLite(dsTable: TSqlite3Dataset;FieldName: string;Default: Boolean = false): Boolean;
function ReadStringSQLite (dsTable: TSqlite3Dataset;FieldName: string;Default: String = ''): String;

procedure WriteIntegerSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Integer);
procedure WriteBooleanSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Boolean);
procedure WriteStringSQLite (dsTable: TSqlite3Dataset;FieldName: string;value: String);

procedure UpdateIntegerSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Integer);
procedure UpdateBooleanSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Boolean);
procedure UpdateStringSQLite (dsTable: TSqlite3Dataset;FieldName: string;value: String);

implementation

uses
  ulTreeView, ulDatabase, AppConfig, db;

function SaveASuiteSQLite(Tree: TBaseVirtualTree; DeleteAll: boolean = false): Boolean;
begin
  Result := True;
  //If launcher is in ReadOnlyMode, exit from this function
  if (Config.ReadOnlyMode) then
    Exit;
  //List & Options
  try
    DBManager.SaveData(Tree,Tree.GetFirst,0);
  except
    Result := False;
  end;
end;

procedure LoadASuiteSQLite(Tree: TBaseVirtualTree; Import: Boolean);
begin
  //List & Options
  DBManager.LoadData(Tree, Import);
  //Backup sqlite database
  DBManager.DoBackupList(SUITE_LIST_PATH);
  //Get rootnode's Icons
  GetChildNodesIcons(Tree, Tree.RootNode);
end;

function ReadIntegerSQLite(dsTable: TSqlite3Dataset;FieldName: string;Default: Integer): Integer;
var
  Field : TField;
begin
  Result := Default;
  Field  := dsTable.FieldByName(FieldName);
  if Assigned(Field) then
    Result := Field.AsInteger;
end;

function ReadBooleanSQLite(dsTable: TSqlite3Dataset;FieldName: string;Default: Boolean): Boolean;
var
  Field : TField;
begin
  Result := Default;
  Field  := dsTable.FieldByName(FieldName);
  if Assigned(Field) then
    Result := Field.AsBoolean;
end;

function ReadStringSQLite(dsTable: TSqlite3Dataset;FieldName: string;Default: string): string;
var
  Field : TField;
begin
  Result := Default;
  Field  := dsTable.FieldByName(FieldName);
  if Assigned(Field) then
    Result := Field.AsString;
end;

procedure WriteIntegerSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Integer);
begin
  dsTable.FieldByName(FieldName).AsInteger := value;
end;

procedure WriteBooleanSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Boolean);
begin
  dsTable.FieldByName(FieldName).AsBoolean := value;
end;

procedure WriteStringSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: String);
begin
  dsTable.FieldByName(FieldName).AsString := value;
end;

procedure UpdateIntegerSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Integer);
begin
  dsTable.FieldByName(FieldName).AsInteger := value;
end;

procedure UpdateBooleanSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: Boolean);
begin
  dsTable.FieldByName(FieldName).AsBoolean := value;
end;

procedure UpdateStringSQLite(dsTable: TSqlite3Dataset;FieldName: string;value: String);
begin
  dsTable.FieldByName(FieldName).AsString := value;
end;


end.
