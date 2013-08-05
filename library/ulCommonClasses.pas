{
Copyright (C) 2006-2013 Matteo Salvi

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

unit ulCommonClasses;

interface

uses
  Classes, VirtualTrees, ulNodeDataTypes, Menus, AppConfig, sysutils;

type
  TNodeDataList = class (TList)
  private
  protected
    function  GetItems(Index: Integer): TvCustomRealNodeData;
    procedure SetItems(Index: Integer; Item: TvCustomRealNodeData);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(Item: TvCustomRealNodeData): Integer;
    procedure Delete(Index: integer);
    function  First: TvCustomRealNodeData;
    function  IndexOf(Item: TvCustomRealNodeData): Integer;
    procedure Insert(Index: integer; Item: TvCustomRealNodeData);
    function  Last: TvCustomRealNodeData;
    function  Remove(Item: TvCustomRealNodeData): Integer;
    property  Items[Index: Integer]: TvCustomRealNodeData read GetItems write SetItems; default;
  end;

  //Most Recently Used
  TMRUList = class(TNodeDataList)
  public
    constructor Create;
    procedure Add(Item: TvCustomRealNodeData);
  end;

  //Most Frequently Used
  TMFUList = class(TNodeDataList)
  public
    constructor Create;
    procedure Add(Item: TvCustomRealNodeData);
    procedure Sort;
  end;

  THotkeyList = class(TNodeDataList)
  public
    function Add(Item: TvCustomRealNodeData): Integer;
    function Remove(Item: TvCustomRealNodeData): Integer;
    function IndexOfID(ID: Integer): TvCustomRealNodeData;
    procedure RefreshRegs;
    procedure Clear;
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
    function  GetItems(Index: Integer): TvCustomRealNodeData;
    procedure SetItems(Index: Integer; Item: TvCustomRealNodeData);
  public
    constructor Create;
    procedure Insert(Index: integer; Item: TvCustomRealNodeData);
    property  Items[Index: Integer]: TvCustomRealNodeData read GetItems write SetItems;
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

implementation

uses
  Main, Windows, ulAppConfig, ulCommonUtils;

constructor TNodeDataList.Create;
begin
  inherited Create;
end;

destructor TNodeDataList.destroy;
begin
  clear;
  inherited Destroy;
end;

function TNodeDataList.Add(Item: TvCustomRealNodeData): Integer;
begin
  Self.Remove(Item);

  Result := inherited Add(Item);
end;

procedure TNodeDataList.Delete(Index: integer);
begin
  inherited Delete(Index);
end;

function TNodeDataList.First: TvCustomRealNodeData;
begin
  Result := inherited First;
end;

function TNodeDataList.IndexOf(Item: TvCustomRealNodeData): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TNodeDataList.Insert(Index: integer; Item: TvCustomRealNodeData);
begin
  Self.Remove(Item);

  inherited insert(Index,Item);
end;

function TNodeDataList.Last: TvCustomRealNodeData;
begin
  Result := inherited Last;
end;

function TNodeDataList.Remove(Item: TvCustomRealNodeData): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TNodeDataList.GetItems(Index: Integer): TvCustomRealNodeData;
begin
  Result  := inherited Get(Index);
end;

procedure TNodeDataList.SetItems(Index: Integer; Item: TvCustomRealNodeData);
begin
  inherited Put(Index, Item);
end;

//------------------------------------------------------------------------------

constructor TMRUList.Create;
begin
  inherited Create;
end;

procedure TMRUList.Add(Item: TvCustomRealNodeData);
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

constructor TMFUList.Create;
begin
  inherited Create;
end;

procedure TMFUList.Add(Item: TvCustomRealNodeData);
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
  inherited Create;
end;

function TAutorunItemList.GetItems(Index: Integer): TvCustomRealNodeData;
begin
  Result  := inherited Get(Index);
end;

procedure TAutorunItemList.SetItems(Index: Integer; Item: TvCustomRealNodeData);
begin
  inherited Put(Index, Item);
end;

procedure TAutorunItemList.Insert(Index: integer; Item: TvCustomRealNodeData);
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

{ THotkeyList }

function THotkeyList.Add(Item: TvCustomRealNodeData): Integer;
begin
  Assert((Item.ID <> -1), 'Current Item doesn'' have a valid ID');

  Result := inherited;
  if Config.HotKey then
    RegisterHotKey(frmMain.Handle, Item.ID,
                      GetHotKeyMod(Item.HotkeyMod),
                      GetHotKeyCode(Item.HotkeyCode));
end;

procedure THotkeyList.Clear;
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    UnregisterHotKey(frmMain.Handle, Self[I].ID);
  inherited;
end;

function THotkeyList.IndexOfID(ID: Integer): TvCustomRealNodeData;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do
  begin
    if Self[I].ID = ID then
    begin
      Result := Self[I];
      Exit;
    end;
  end;
end;

procedure THotkeyList.RefreshRegs;
var
  I: Integer;
begin
  //This method unregister and register hotkey again for every item
  for I := 0 to Self.Count - 1 do
  begin
    UnregisterHotKey(frmMain.Handle, Self[I].ID);
    if Config.HotKey then
      RegisterHotKey(frmMain.Handle, Self[I].ID,
                     GetHotKeyMod(Self[I].HotkeyMod),
                     GetHotKeyCode(Self[I].HotkeyCode));
  end;
end;

function THotkeyList.Remove(Item: TvCustomRealNodeData): Integer;
begin
  Assert((Item.ID <> -1), 'Current Item doesn'' have a valid ID');

  Result := inherited;
  if Config.HotKey then
    UnregisterHotKey(frmMain.Handle, Item.ID);
end;

end.
