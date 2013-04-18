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

unit ulCommonClasses;

interface

uses
  Classes, VirtualTrees, ulNodeDataTypes, Menus, AppConfig, sysutils;

type
  TNodeDataList = class (TList)
  private
    fMaxItems: Integer;
  protected
    procedure RemoveMaxItems;
    function  GetItems(Index: Integer): TvBaseNodeData;
    procedure SetItems(Index: Integer; Item: TvBaseNodeData);
  public
    constructor Create(MaxItems: Integer);
    destructor  Destroy; override;
    function  Add(Item: TvBaseNodeData): Integer;
    procedure Delete(Index: integer);
    function  First: TvBaseNodeData;
    function  IndexOf(Item: TvBaseNodeData): Integer;
    procedure Insert(Index: integer; Item: TvBaseNodeData);
    function  Last: TvBaseNodeData;
    function  Remove(Item: TvBaseNodeData): Integer;
    property  Items[Index: Integer]: TvBaseNodeData read GetItems write SetItems;
    procedure SetMaxItems(Number: Integer);
    function  GetMaxItems: Integer;
  end;

  //Most Recently Used
  TMRUList = class(TNodeDataList)
  public
    constructor Create(MaxItems: Integer);
    procedure Add(Item: TvBaseNodeData);
  end;

  //Most Frequently Used
  TMFUList = class(TNodeDataList)
  public
    constructor Create(MaxItems: Integer);
    procedure Add(Item: TvBaseNodeData);
    procedure Sort;
  end;

  //ASuite TrayIcon Menu
  TASMenuItem = class(TMenuItem)
  private
    FData: TvBaseNodeData;
    FpNode: PVirtualNode;
    procedure SetData(const Value: TvBaseNodeData);
    procedure SetpNode(const Value: PVirtualNode);
  public
    constructor Create(AOwner: TComponent); override;
    property Data : TvBaseNodeData read FData write SetData;
    property pNode : PVirtualNode read FpNode write SetpNode;
    function NewBottomLine: Integer;
    function InsertNewLine(ABefore: Boolean; AItem: TMenuItem): Integer;
  end;

  //StartUp Item List
  TAutorunItemList = class(TNodeDataList)
  protected
    function  GetItems(Index: Integer): TvFileNodeData;
    procedure SetItems(Index: Integer; Item: TvFileNodeData);
  public
    constructor Create;
    procedure Insert(Index: integer; Item: TvFileNodeData);
    property  Items[Index: Integer]: TvFileNodeData read GetItems write SetItems;
  end;

  { TVersionInfo }

  TVersionInfo = class
    private
      FMajor   : Integer;
      FMinor   : Integer;
      FRelease : Integer;
      FBuild   : Integer;
    public
      constructor Create; overload; //Get actual ASuite version info
      constructor Create(aMajor, aMinor, aRelease, aBuild: Integer); overload;
      function ConvertToString: string;
      property Major: Integer   read FMajor   write FMajor;
      property Minor: Integer   read FMinor   write FMinor;
      property Release: Integer read FRelease write FRelease;
      property Build: Integer   read FBuild   write FBuild;
  end;

var
  ASuiteStartUpApp: TAutorunItemList;

implementation

constructor TNodeDataList.Create(MaxItems: Integer);
begin
  fMaxItems := MaxItems;

  inherited Create;
end;

destructor TNodeDataList.destroy;
begin
  clear;
  inherited Destroy;
end;

function TNodeDataList.Add(Item: TvBaseNodeData): Integer;
begin
  Self.Remove(Item);

  Result := inherited Add(Item);

  if fMaxItems > 0 then
    Self.RemoveMaxItems;
end;

procedure TNodeDataList.Delete(Index: integer);
begin
  inherited Delete(Index);
end;

function TNodeDataList.First: TvBaseNodeData;
begin
  Result := inherited First;
end;

procedure TNodeDataList.RemoveMaxItems;
var
  I : Integer;
begin
  if (Self.Count - fMaxItems) > 0 then
    for I := 1 to Self.Count - fMaxItems do
      Self.Delete(fMaxItems);
end;

function TNodeDataList.IndexOf(Item: TvBaseNodeData): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TNodeDataList.Insert(Index: integer; Item: TvBaseNodeData);
begin
  Self.Remove(Item);

  inherited insert(Index,Item);

  if fMaxItems > 0 then
    Self.RemoveMaxItems;
end;

function TNodeDataList.Last: TvBaseNodeData;
begin
  Result := inherited Last;
end;

function TNodeDataList.Remove(Item: TvBaseNodeData): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TNodeDataList.GetItems(Index: Integer): TvBaseNodeData;
begin
  Result  := inherited Get(Index);
end;

procedure TNodeDataList.SetItems(Index: Integer; Item: TvBaseNodeData);
begin
  inherited Put(Index, Item);
end;

procedure TNodeDataList.SetMaxItems(Number: Integer);
begin
  fMaxItems := Number;
  //Remove unnecessary items
  RemoveMaxItems;
end;

function TNodeDataList.GetMaxItems: Integer;
begin
  Result := fMaxItems;
end;

//------------------------------------------------------------------------------

constructor TMRUList.Create(MaxItems: Integer);
begin
  inherited Create(MaxItems);
end;

procedure TMRUList.Add(Item: TvBaseNodeData);
var
  i: integer;
  Inserted : Boolean;
begin
  i := 0;
  Inserted := False;
  while (not inserted) and (i < Self.Count)  do
  begin
    if (Items[i].MRUPosition < Item.MRUPosition) then
    begin
      inherited Insert(i, Item);
      Inserted := True;
    end;
    Inc(i);
  end;
  if not Inserted then
    inherited Add(Item);
end;

//------------------------------------------------------------------------------

constructor TMFUList.Create(MaxItems: Integer);
begin
  inherited Create(MaxItems);
end;

procedure TMFUList.Add(Item: TvBaseNodeData);
var
  i: integer;
  Inserted : Boolean;
begin
  i := 0;
  Inserted := False;
  while (not inserted) and (i < Self.Count)  do
  begin
    if (Items[i].ClickCount < Item.ClickCount) then
    begin
      inherited Insert(i, Item);
      Inserted := True;
    end;
    Inc(i);
  end;
  if not Inserted then
    inherited Add(Item);
end;

function CompareClicks(Item1 : Pointer; Item2 : Pointer) : Integer;
var
  NodeData1, NodeData2 : TvFileNodeData;
begin
  NodeData1 := TvFileNodeData(Item1);
  NodeData2 := TvFileNodeData(Item2);
  Result := 0;
  if NodeData1.ClickCount > NodeData2.ClickCount then
    Result := -1
  else
    if NodeData1.ClickCount = NodeData2.ClickCount then
      Result := 0
    else
      if NodeData1.ClickCount < NodeData2.ClickCount then
        Result := 1;
end;

procedure TMFUList.Sort;
begin
  inherited Sort(CompareClicks);
end;

//------------------------------------------------------------------------------

constructor TASMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData  := nil;
  FpNode := nil;
end;

procedure TASMenuItem.SetData(const Value: TvBaseNodeData);
begin
  FData := Value;
end;

procedure TASMenuItem.SetpNode(const Value: PVirtualNode);
begin
  FpNode := Value;
end;

function TASMenuItem.NewBottomLine: Integer;
begin
  Result := 0;
  if Count = 0 then
    Add(NewLine)
  else
    Result := InsertNewLine(False, Items[Count - 1]);
end;

function TASMenuItem.InsertNewLine(ABefore: Boolean; AItem: TMenuItem): Integer;
begin
  if ABefore then
  begin
    if (AItem.MenuIndex > 0) and
       Items[AItem.MenuIndex - 1].IsLine then
    begin
      Result := AItem.MenuIndex - 1;
      Items[AItem.MenuIndex - 1].Visible := True;
    end
    else
    begin
      Result := AItem.MenuIndex;
      Insert(AItem.MenuIndex, NewLine);
    end;
  end
  else
  begin
    if (AItem.MenuIndex < Count - 1) and
       Items[AItem.MenuIndex + 1].IsLine then
    begin
      Result := AItem.MenuIndex + 2;
      Items[AItem.MenuIndex + 1].Visible := True;
    end
    else
    begin
      Result := AItem.MenuIndex + 2;
      Insert(AItem.MenuIndex + 1, NewLine);
    end;
  end;
end;

//------------------------------------------------------------------------------

constructor TAutorunItemList.Create;
begin
  inherited Create(-1);
end;

function TAutorunItemList.GetItems(Index: Integer): TvFileNodeData;
begin
  Result  := inherited Get(Index);
end;

procedure TAutorunItemList.SetItems(Index: Integer; Item: TvFileNodeData);
begin
  inherited Put(Index, Item);
end;

procedure TAutorunItemList.Insert(Index: integer; Item: TvFileNodeData);
begin
  if (Item.AutorunPos > Self.Count) then
    Item.AutorunPos := Self.Count;
  inherited Insert(Item.AutorunPos, Item);
end;

//------------------------------------------------------------------------------

constructor TVersionInfo.Create;
begin
  //Get actual ASuite version info
  FMajor   := StrToInt(VERSION_MAJOR);
  FMinor   := StrToInt(VERSION_MINOR);
  FRelease := StrToInt(VERSION_RELEASE);
  FBuild   := StrToInt(VERSION_BUILD);
end;

constructor TVersionInfo.Create(aMajor, aMinor, aRelease, aBuild: Integer);
begin
  FMajor   := aMajor;
  FMinor   := aMinor;
  FRelease := aRelease;
  FBuild   := aBuild;
end;

function TVersionInfo.ConvertToString: string;
begin
  Result := Format('%s.%s.%s.%s',[IntToStr(FMajor),
                                  IntToStr(FMinor),
                                  IntToStr(FRelease),
                                  IntToStr(FBuild)]);
end;

end.
