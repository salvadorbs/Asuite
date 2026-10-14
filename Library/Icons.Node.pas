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
  SysUtils, Icons.Base, NodeDataTypes.Base, Graphics, Icons.Custom,
  NodeDataTypes.Custom, Controls, LCLIntf, Classes;

type

  { TNodeIcon }

  TNodeIcon = class(Icons.Custom.TCustomIcon)
  private
    { private declarations }
    FNodeData: TvBaseNodeData;
    FOnInvalidate: TNotifyEvent;
    procedure DoInvalidateNode;
  protected
    function InternalLoadIcon: Integer; override;
    function GetName: string; override;
    function GetDefaultPathIcon: string; override;
  public
    { public declarations }
    constructor Create(ANodeData: TvBaseNodeData);

    function LoadIcon: Integer; override;
    procedure ResetIcon; override;

    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
  end;

implementation

uses
  NodeDataTypes.Files, BGRABitmapTypes, Kernel.Instance, Kernel.Manager,
  Kernel.Consts;

{ TNodeIcon }

constructor TNodeIcon.Create(ANodeData: TvBaseNodeData);
begin
  inherited Create;
  FNodeData     := ANodeData;
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
  if not Assigned(FNodeData) then
    Exit;

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

  if Assigned(FNodeData) and (oldCacheIconCRC <> Self.CacheIconCRC) then
    FNodeData.Changed := True;

  //If it is a category, get directly cat icon
  if Assigned(FNodeData) and (FNodeData.IsCategoryItem) and (Result = -1) then
    Result := ASuiteManager.IconsManager.GetIconIndex(ICON_NAME_CATEGORY);
end;

function TNodeIcon.LoadIcon: Integer;
begin
  Result := -1;
  if not Assigned(FNodeData) then
    Exit;
  if FNodeData.IsSeparatorItem then
    Exit;

  //Check file path and if it isn't found, get fileicon_error and exit
  if FNodeData.IsFileItem then
  begin
    if Not(TvFileNodeData(FNodeData).IsPathFileExists) then
      Result := ASuiteManager.IconsManager.GetIconIndex(ICON_NAME_FILE_ERROR);
  end;
  //Get imageindex
  if (Result = -1) then
    Result := inherited;
end;

procedure TNodeIcon.DoInvalidateNode;
begin
  if Assigned(FNodeData) and Assigned(FNodeData.PNode) and Assigned(ASuiteInstance) and
    Assigned(ASuiteInstance.MainTree) then
    ASuiteInstance.MainTree.InvalidateNode(FNodeData.PNode);
end;

procedure TNodeIcon.ResetIcon;
begin
  inherited;

  if not Assigned(FNodeData) or not Assigned(FNodeData.PNode) then
    Exit;

  if Assigned(FOnInvalidate) then
  begin
    FOnInvalidate(Self);
    Exit;
  end;

  //Force MainTree repaint node without touching VCL from worker threads
  if GetCurrentThreadId = MainThreadID then
    DoInvalidateNode
  else
    TThread.Queue(nil, DoInvalidateNode);
end;

end.

