{
Copyright (C) 2006-2021 Matteo Salvi

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

unit NodeDataTypes.Base;

{$MODE DelphiUnicode}

interface

uses
  VirtualTrees, SysUtils, DateUtils, Kernel.Enumerations,
  Icons.Custom;

type

  { TvBaseNodeData }

  TvBaseNodeData = class
  private
    //Base private variables and functions
    FID          : Int64;
    FMenuCaption : string;
    FParentID    : Int64;
    FPosition    : Cardinal;
    FChanged     : boolean;
    FName        : String;
    FDescription : string;
    FIcon        : TCustomIcon;
    FDataType    : TvTreeDataType;
    FPNode       : PVirtualNode; //Self PVirtualNode
    FAddDate     : Int64;
    FEditDate    : Int64;
    FHideFromMenu : Boolean;
    function GetName:String;
    procedure SetName(Value: String);
    function GetDataType: TvTreeDataType;
    procedure SetDataType(const Value: TvTreeDataType);
    function GetAddDate:TDateTime;
    procedure SetAddDate(Value: TDateTime);
    function GetUnixAddDate:Int64;
    procedure SetUnixAddDate(Value: Int64);
    function GetEditDate:TDateTime;
    procedure SetEditDate(Value: TDateTime);
    function GetUnixEditDate:Int64;
    procedure SetUnixEditDate(Value: Int64);
    function GetParentNode: PVirtualNode;
    procedure SetChanged(const Value: boolean);
  public
    //Base properties
    constructor Create(AType: TvTreeDataType); // virtual;
    destructor Destroy; override;

    procedure SetPointerNode(APNode: PVirtualNode);
    procedure Copy(source:TvBaseNodeData); virtual;
    function IsFileItem: Boolean;
    function IsSeparatorItem: Boolean;
    function IsCategoryItem: Boolean;

    property ID : Int64 read FID write FID;
    property ParentID : Int64 read FParentID write FParentID;
    property Position : Cardinal read FPosition write FPosition;
    property Changed: boolean read FChanged write SetChanged;
    property Name: string read GetName write SetName;
    property MenuCaption: string read FMenuCaption;
    property Description: string read FDescription write FDescription;
    property Icon: TCustomIcon read FIcon;
    property DataType: TvTreeDataType read GetDataType write SetDataType;
    property ParentNode: PVirtualNode read GetParentNode;
    property PNode: PVirtualNode read FPNode;
    property AddDate: TDateTime read GetAddDate write SetAddDate;
    property UnixAddDate: Int64 read GetUnixAddDate write SetUnixAddDate;
    property EditDate: TDateTime read GetEditDate write SetEditDate;
    property UnixEditDate: Int64 read GetUnixEditDate write SetUnixEditDate;
    property HideFromMenu:Boolean read FHideFromMenu write FHideFromMenu;
  end;
  PvBaseNodeData = ^TvBaseNodeData;

implementation

uses
  Icons.Node, Kernel.ResourceStrings, Utility.Misc;

constructor TvBaseNodeData.Create(AType: TvTreeDataType);
begin
  FID          := -1;
  FParentID    := -1;
  FName        := '';
  FDataType    := AType;
  FPNode       := nil;
  FHideFromMenu := False;
  FAddDate     := DateTimeToUnix(Now);
  FEditDate    := FAddDate;
  FIcon        := TCustomIcon(TNodeIcon.Create(Self));
end;

destructor TvBaseNodeData.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TvBaseNodeData.Copy(Source:TvBaseNodeData);
begin
  Self.Name         := msgCopy + Source.Name;
  Self.DataType     := Source.DataType;
  Self.HideFromMenu := Source.HideFromMenu;
  Self.Description  := Source.Description;
end;

function TvBaseNodeData.IsFileItem: Boolean;
begin
  Result := Self.DataType in [vtdtFile, vtdtFolder];
end;

function TvBaseNodeData.IsSeparatorItem: Boolean;
begin
  Result := (Self.DataType = vtdtSeparator);
end;

function TvBaseNodeData.IsCategoryItem: Boolean;
begin
  Result := (Self.DataType = vtdtCategory);
end;

function TvBaseNodeData.GetName: String;
begin
  Result := FName;
end;

function TvBaseNodeData.GetParentNode: PVirtualNode;
begin
  Result := FPNode.Parent;
end;

procedure TvBaseNodeData.SetName(Value: String);
begin
  FName := Value;
  FMenuCaption := DoubleAmpersands(Value);
end;

procedure TvBaseNodeData.SetPointerNode(APNode: PVirtualNode);
begin
  FPNode := APNode;
end;

function TvBaseNodeData.GetDataType: TvTreeDataType;
begin
  Result := FDataType;
end;

procedure TvBaseNodeData.SetDataType(const Value: TvTreeDataType);
begin
  FDataType := Value;
end;

function TvBaseNodeData.GetAddDate: TDateTime;
begin
  Result := UnixToDateTime(FAddDate);
end;

procedure TvBaseNodeData.SetAddDate(Value: TDateTime);
begin
  FAddDate := DateTimeToUnix(Value);
end;

procedure TvBaseNodeData.SetChanged(const Value: boolean);
begin
  FEditDate := DateTimeToUnix(Now);
  FChanged  := Value;
end;

function TvBaseNodeData.GetUnixAddDate: Int64;
begin
  Result := FAddDate;
end;

procedure TvBaseNodeData.SetUnixAddDate(Value: Int64);
begin
  FAddDate := Value
end;

function TvBaseNodeData.GetEditDate: TDateTime;
begin
  Result := UnixToDateTime(FEditDate);
end;

procedure TvBaseNodeData.SetEditDate(Value: TDateTime);
begin
  FEditDate := DateTimeToUnix(Value);
end;

function TvBaseNodeData.GetUnixEditDate: Int64;
begin
  Result := FEditDate;
end;

procedure TvBaseNodeData.SetUnixEditDate(Value: Int64);
begin
  FEditDate := Value;
end;

end.
