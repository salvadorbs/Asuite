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

unit Icons.Node;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Icons.Base, NodeDataTypes.Base, Graphics, Icons.Custom,
  NodeDataTypes.Custom, Controls, LCLIntf, BGRABitmap;

type

  { TNodeIcon }

  TNodeIcon = class(Icons.Custom.TCustomIcon)
  private
    { private declarations }
    FNodeData: TvBaseNodeData;
    FCacheIconCRC: Integer;
  protected
    function InternalLoadIcon: Integer; override;
    function GetName: string; override;
    function GetDefaultPathIcon: string; override;
  public
    { public declarations }
    constructor Create(ANodeData: TvBaseNodeData);

    function LoadIcon: Integer; override;
    procedure ResetIcon; override;
  end;

implementation

uses
  NodeDataTypes.Files, ImgList, BGRABitmapTypes, Kernel.Instance, Kernel.Manager;

{ TNodeIcon }

constructor TNodeIcon.Create(ANodeData: TvBaseNodeData);
begin
  inherited Create;
  FNodeData     := ANodeData;
  FCacheIconCRC := 0;
  Self.TempItem := False;
end;

function TNodeIcon.GetName: string;
begin
  Result := '';
  if Assigned(FNodeData) and (FNodeData.ID <> -1) then
    Result := IntToStr(FNodeData.ID);
end;

function TNodeIcon.GetDefaultPathIcon: string;
var
  sPathAbsoluteIcon: string;
begin
  Result := '';

  //Get custom icon path
  sPathAbsoluteIcon := '';
  if (FNodeData is TvCustomRealNodeData) then
    sPathAbsoluteIcon := TvCustomRealNodeData(FNodeData).PathAbsoluteIcon;

  if (sPathAbsoluteIcon <> '') and FileExists(sPathAbsoluteIcon) then
    Result := sPathAbsoluteIcon
  else //Else absolute filename (if nodedata is a file item)
    if FNodeData.IsFileItem then
      Result := TvFileNodeData(FNodeData).PathAbsoluteFile;
end;

function TNodeIcon.InternalLoadIcon: Integer;
var
  oldCacheIconCRC: Integer;
begin
  //Priority cache->icon->exe
  oldCacheIconCRC := Self.CacheIconCRC;
  Result := inherited;

  if oldCacheIconCRC <> Self.CacheIconCRC then
    FNodeData.Changed := True;

  //If it is a category, get directly cat icon
  if (FNodeData.IsCategoryItem) and (Result = -1) then
    Result := ASuiteManager.IconsManager.GetIconIndex('category');
end;

function TNodeIcon.LoadIcon: Integer;
begin
  Result := -1;
  if FNodeData.IsSeparatorItem then
    Exit;

  //Check file path and if it isn't found, get fileicon_error and exit
  if FNodeData.IsFileItem then
  begin
    if Not(TvFileNodeData(FNodeData).IsPathFileExists) then
      Result := ASuiteManager.IconsManager.GetIconIndex('file_error');
  end;
  //Get imageindex
  if (Result = -1) then
    Result := inherited;
end;

procedure TNodeIcon.ResetIcon;
begin
  inherited;

  //Force MainTree repaint node
  if Assigned(FNodeData.PNode) then
    ASuiteInstance.MainTree.InvalidateNode(FNodeData.PNode);
end;

end.

