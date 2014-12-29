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

unit Lists.Base;

interface

uses
  Classes, VirtualTrees, NodeDataTypes.Custom, Menus, SysUtils, Kernel.Types,
  Windows, Kernel.Enumerations, NodeDataTypes.Files, NodeDataTypes.Base,
  DateUtils, Dialogs, DKLang, UITypes;

type
  TBaseItemsList = class
  protected
    FItems: TNodeDataItems;

    function GetItems(Index: Integer): TvCustomRealNodeData;
    procedure SetItems(Index: Integer; const Value: TvCustomRealNodeData);
  public
    constructor Create;
    destructor Destroy; override;

    function AddItem(AItem: TvCustomRealNodeData): Integer; virtual;
    function InsertItem(AIndex: Integer; AItem: TvCustomRealNodeData): Integer; virtual;
    function RemoveItem(AItem: TvCustomRealNodeData): Integer; virtual;

    procedure Delete(AIndex: Integer); virtual;
    function Count: Integer; virtual;

    procedure Clear; virtual;

    property Items[Index: Integer]: TvCustomRealNodeData read GetItems write SetItems; default;
  end;

implementation

function TBaseItemsList.AddItem(AItem: TvCustomRealNodeData): Integer;
begin
  FItems.Remove(AItem);

  Result := FItems.Add(AItem);
end;

procedure TBaseItemsList.Clear;
begin
  FItems.Clear;
end;

function TBaseItemsList.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TBaseItemsList.Create;
begin
  FItems := TNodeDataItems.Create;
end;

procedure TBaseItemsList.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor TBaseItemsList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TBaseItemsList.GetItems(Index: Integer): TvCustomRealNodeData;
begin
  Result := TvCustomRealNodeData(FItems[Index]);
end;

function TBaseItemsList.InsertItem(AIndex: Integer; AItem: TvCustomRealNodeData): Integer;
begin
  FItems.Remove(AItem);

  Result := AIndex;
  if (Result > FItems.Count) then
    Result := FItems.Count;

  FItems.Insert(Result, AItem);
end;

function TBaseItemsList.RemoveItem(AItem: TvCustomRealNodeData): Integer;
begin
  Result := FItems.Remove(AItem);
end;

procedure TBaseItemsList.SetItems(Index: Integer; const Value: TvCustomRealNodeData);
begin
  FItems[Index] := Value;
end;

end.
