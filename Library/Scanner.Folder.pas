{
Copyright (C) 2006-2015 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Scanner.Folder;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TCheckType = (fctUnchecked, fctChecked, fctMixed);

  TScannerFolder = class
  private
    FItems: TList<TScannerFolder>;
    FParent: TScannerFolder;
    FPath: string;
    FCheckType: TCheckType;

    function GetItem(Index: Integer): TScannerFolder;
    function GetCount: Integer;
    function GetName: string;
  public
    constructor Create(APath: string; ACheckType: TCheckType);
    destructor Destroy; override;

    property Name: string read GetName;
    property Path: string read FPath write FPath;
    property CheckType: TCheckType read FCheckType write FCheckType;
    property Items[Index: Integer]: TScannerFolder read GetItem; default;
    property Parent: TScannerFolder read FParent write FParent;

    procedure Add(Item: TScannerFolder);
    function AddItem(APath: string; ACheckType: TCheckType): TScannerFolder;
    procedure Remove(Item: TScannerFolder);
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
  end;

implementation

{ TScannerFolder }

procedure TScannerFolder.Add(Item: TScannerFolder);
begin
  if FItems = nil then
    FItems := TList<TScannerFolder>.Create;

  FItems.Add(Item);
  Item.Parent := Self;
end;

function TScannerFolder.AddItem(APath: string; ACheckType: TCheckType): TScannerFolder;
begin
  Result := TScannerFolder.Create(APath, ACheckType);
  Self.Add(Result);
end;

constructor TScannerFolder.Create(APath: string; ACheckType: TCheckType);
begin
  inherited Create;
  FPath   := APath;
  FCheckType := ACheckType;
  FParent := nil;
end;

procedure TScannerFolder.Delete(Index: Integer);
var
  Cur: TScannerFolder;
begin
  if Not((Index < 0) or (FItems = nil) or (Index >= GetCount)) then
  begin
    Cur := FItems[Index];
    FItems.Delete(Index);
    Cur.FParent := nil;
  end;
end;

destructor TScannerFolder.Destroy;
begin
  if FParent <> nil then
  begin
    FParent.Remove(Self);
    FParent := nil;
  end;

  while Count > 0 do
    Items[0].Free;

  FreeAndNil(FItems);
  inherited Destroy;
end;

function TScannerFolder.GetCount: Integer;
begin
  if FItems = nil then
    Result := 0
  else
    Result := FItems.Count;
end;

function TScannerFolder.GetItem(Index: Integer): TScannerFolder;
begin
  if FItems <> nil then
    Result := TScannerFolder(FItems[Index]);
end;

function TScannerFolder.GetName: string;
begin
  Result := ExtractFileName(FPath);
  if Result = '' then
    Result := ExtractFileDrive(FPath);
end;

procedure TScannerFolder.Remove(Item: TScannerFolder);
begin
  Delete(FItems.IndexOf(Item));
end;

end.
