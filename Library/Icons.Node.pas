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

unit Icons.Node;

interface

uses
  SysUtils, Classes, Icons.Base, NodeDataTypes.Base, Kernel.Enumerations,
  NodeDataTypes.Custom;

type
  TNodeIcon = class(TBaseIcon)
  private
    { private declarations }
    FNodeData: TvBaseNodeData;

    function InternalGetIconIndex: Integer;
    function GetIconPath: string;
  public
    { public declarations }
    constructor Create(ANodeData: TvBaseNodeData);

    function LoadIcon: Integer; override;
    procedure ResetIcon; override;
  end;

implementation

uses
  Utility.System, AppConfig.Main, NodeDataTypes.Files;

{ TNodeIcon }

constructor TNodeIcon.Create(ANodeData: TvBaseNodeData);
begin
  inherited Create;
  FNodeData := ANodeData;
end;

function TNodeIcon.GetIconPath: string;
begin
  //Get custom icon path
  Result := '';
  if FileExists(TvCustomRealNodeData(FNodeData).PathAbsoluteIcon) then
    Result := TvCustomRealNodeData(FNodeData).PathAbsoluteIcon
  else //Else exe (if nodedata is a file item)
    if FNodeData.DataType = vtdtFile then
      Result := (FNodeData as TvFileNodeData).PathAbsoluteFile;
end;

function TNodeIcon.InternalGetIconIndex: Integer;
var
  sTempPath: string;
begin
  //Priority cache->icon->exe
  sTempPath := '';
  Result   := -1;

  if Not FileExists(sTempPath) then
  begin
    //Get custom icon path (or exe file name, if it is a file type)
    sTempPath := GetIconPath;
  end;
  //Get image index
  if (sTempPath <> '') then
  begin
    if IsPathExists(sTempPath) then
      Result := InternalGetImageIndex(sTempPath);
  end;
end;

function TNodeIcon.LoadIcon: Integer;
begin
  Result := -1;
  //Check file path and if it isn't found, get fileicon_error and exit
  if FNodeData.DataType = vtdtFile then
  begin
    if Not(TvFileNodeData(FNodeData).IsPathFileExists) then
      Result := Config.IconsManager.GetIconIndex('file_error');
  end;
  //Get imageindex
  if (Result = -1) then
    Result := InternalGetIconIndex();
end;

procedure TNodeIcon.ResetIcon;
begin
  inherited;
  //Force MainTree repaint node
  Config.MainTree.InvalidateNode(FNodeData.PNode);
end;

end.
