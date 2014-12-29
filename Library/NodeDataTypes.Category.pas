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

unit NodeDataTypes.Category;

interface

uses
  VirtualTrees, Menus, SysUtils, Dialogs, DateUtils, Kernel.Enumerations,
  Winapi.Windows, NodeDataTypes.Base, NodeDataTypes.Custom, Kernel.Types,
  DKLang, UITypes;

type
  TvCategoryNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    function CheckRunnableSubItems(Tree: TBaseVirtualTree): Boolean;
  public
    //Specific properties
    constructor Create; overload;
  end;
  PvCategoryNodeData = ^TvCategoryNodeData;

implementation

uses
  Utility.Process, NodeDataTypes.Files, AppConfig.Main, Utility.TreeView;

function TvCategoryNodeData.CheckRunnableSubItems(Tree: TBaseVirtualTree): Boolean;
var
  Node: PVirtualNode;
  CurrentNodeData: TvBaseNodeData;
begin
  Result := False;
  Node := Self.PNode.FirstChild;
  while Assigned(Node) do
  begin
    CurrentNodeData := GetNodeItemData(Node, Tree);
    if (Assigned(CurrentNodeData)) and (CurrentNodeData.DataType in [vtdtFile,vtdtFolder]) then
    begin
      if (TvFileNodeData(CurrentNodeData).RunFromCategory) then
      begin
        Result := True;
        Break;
      end;
    end;

    Node := Node.NextSibling;
  end;
end;

constructor TvCategoryNodeData.Create;
begin
  inherited Create(vtdtCategory);
//  FImageIndex := IMAGE_INDEX_Cat;
  //TODO: Add big size icon
end;

end.
