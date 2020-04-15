{
Copyright (C) 2006-2020 Matteo Salvi

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

unit Lists.Special;

interface

uses
  Classes, NodeDataTypes.Custom, Menus, SysUtils, Kernel.Types,
  Windows, Kernel.Enumerations, NodeDataTypes.Files, NodeDataTypes.Base,
  DateUtils, Dialogs, UITypes, Lists.Base;

type
  TMRUItemsComparer = class(TItemsComparer)
  public
    function Compare(const Left, Right: TvBaseNodeData): Integer; override;
  end;

  TMFUItemsComparer = class(TItemsComparer)
  public
    function Compare(const Left, Right: TvBaseNodeData): Integer; override;
  end;

  TSpecialItemsList = class(TBaseItemsList)
  private
    FListMode: TSpecialListMode;
    FComparison: TItemsComparer;

    function CompareItems(AItem1, AItem2: TvFileNodeData): Boolean;
  public
    constructor Create(AMode: TSpecialListMode);
    destructor Destroy; override;

    function AddItem(AItem: TvCustomRealNodeData): Integer; override;
    procedure Sort;
  end;

implementation

uses
  Utility.Misc;

function TSpecialItemsList.AddItem(AItem: TvCustomRealNodeData): Integer;
var
  I: integer;
  Inserted : Boolean;
begin
  Result := -1;

  if AItem is TvFileNodeData then
  begin
    I := 0;
    Inserted := False;
    while (not inserted) and (I < FItems.Count)  do
    begin
      if CompareItems(TvFileNodeData(FItems[I]), TvFileNodeData(AItem)) then
      begin
        Result := inherited InsertItem(I, AItem);
        Inserted := True;
      end;
      Inc(I);
    end;

    if not Inserted then
      Result := inherited AddItem(AItem);
  end;
end;

function TSpecialItemsList.CompareItems(AItem1, AItem2: TvFileNodeData): Boolean;
begin
  Result := False;
  case FListMode of
    lmMRU: Result := (AItem1.LastAccess < AItem2.LastAccess);
    lmMFU: Result := (AItem1.ClickCount < AItem2.ClickCount);
  end;
end;

constructor TSpecialItemsList.Create(AMode: TSpecialListMode);
begin
  inherited Create;

  FListMode   := AMode;
  case FListMode of
    lmMRU: FComparison := TMRUItemsComparer.Create;
    lmMFU: FComparison := TMFUItemsComparer.Create;
  end;
end;

destructor TSpecialItemsList.Destroy;
begin
  FComparison.Free;
  inherited;
end;

procedure TSpecialItemsList.Sort;
begin
  FItems.Sort(FComparison);
end;

function TMRUItemsComparer.Compare(const Left, Right: TvBaseNodeData): Integer;
begin
  Result := 0;
  if (Left.DataType in [vtdtFile, vtdtFolder]) and (Right.DataType in [vtdtFile, vtdtFolder]) then
  begin
    Result := CompareInteger(TvFileNodeData(Right).LastAccess,
                             TvFileNodeData(Left).LastAccess);
  end;
end;

function TMFUItemsComparer.Compare(const Left, Right: TvBaseNodeData): Integer;
begin
  Result := 0;
  if (Left.DataType in [vtdtFile, vtdtFolder]) and (Right.DataType in [vtdtFile, vtdtFolder]) then
  begin
    Result := CompareInteger(TvFileNodeData(Right).ClickCount,
                             TvFileNodeData(Left).ClickCount);
  end;
end;

end.
