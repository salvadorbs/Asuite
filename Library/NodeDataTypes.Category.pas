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

unit NodeDataTypes.Category;

{$MODE DelphiUnicode}

interface

uses
  VirtualTrees, SysUtils, Dialogs, Kernel.Enumerations, Controls,
  LCLIntf, NodeDataTypes.Base, NodeDataTypes.Custom, Kernel.Types;

type
  TvCategoryNodeData = class(TvCustomRealNodeData)
  private
    //Specific private variables and functions
    function CheckRunnableSubItems(Tree: TBaseVirtualTree): Boolean;
    function ConfirmRunCategory: Boolean;

    procedure CallBackExecuteNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                  Data: Pointer; var Abort: Boolean);
    procedure CallBackExecuteNodeAsUser(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                        Data: Pointer; var Abort: Boolean);
    procedure CallBackExecuteNodeAsAdmin(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                         Data: Pointer; var Abort: Boolean);
  protected
    function InternalExecute(ARunFromCategory: Boolean; ASingleInstance: Boolean): boolean; override;
    function InternalExecuteAsUser(ARunFromCategory: Boolean; AUserData: TUserData): boolean; override;
    function InternalExecuteAsAdmin(ARunFromCategory: Boolean): boolean; override;
  public
    //Specific properties
    constructor Create; overload;
  end;
  PvCategoryNodeData = ^TvCategoryNodeData;

implementation

uses
  NodeDataTypes.Files, VirtualTree.Methods, AppConfig.Main, Kernel.ResourceStrings;

procedure TvCategoryNodeData.CallBackExecuteNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
  SingleInstance: PBoolean;
begin
  SingleInstance := Data;
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if (Assigned(NodeData)) and (NodeData.IsFileItem) then
  begin
    if TvFileNodeData(NodeData).RunFromCategory then
      TvFileNodeData(NodeData).Execute(False, True, Boolean(SingleInstance));
  end;
end;

procedure TvCategoryNodeData.CallBackExecuteNodeAsAdmin(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
  var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  if (Assigned(NodeData)) and (NodeData.IsFileItem) then
  begin
    if TvFileNodeData(NodeData).RunFromCategory then
      TvFileNodeData(NodeData).ExecuteAsAdmin(False, True);
  end;
end;

procedure TvCategoryNodeData.CallBackExecuteNodeAsUser(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: TvBaseNodeData;
  UserData: pUserData;
begin
  NodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Sender);
  UserData := Data;
  if (Assigned(NodeData)) and (NodeData.IsFileItem) then
  begin
    if TvFileNodeData(NodeData).RunFromCategory then
      TvFileNodeData(NodeData).ExecuteAsUser(False, True, UserData^);
  end;
end;

function TvCategoryNodeData.CheckRunnableSubItems(Tree: TBaseVirtualTree): Boolean;
var
  Node : PVirtualNode;
  ChildNodeData : TvBaseNodeData;
begin
  Result := False;
  Node := Self.PNode.FirstChild;
  while Assigned(Node) do
  begin
    ChildNodeData := TVirtualTreeMethods.Create.GetNodeItemData(Node, Tree);
    if (Assigned(ChildNodeData)) and (ChildNodeData.IsFileItem) then
    begin
      if (TvFileNodeData(ChildNodeData).RunFromCategory) then
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
end;

function TvCategoryNodeData.ConfirmRunCategory: Boolean;
begin
  Result := False;
  if (Config.ConfirmRunCat) then
  begin
    if CheckRunnableSubItems(Config.MainTree) then
      Result := (MessageDlg(Format(msgConfirmRunCat, [Self.Name]), mtWarning, [mbYes, mbNo], 0) = mrYes);
  end
  else
    Result := True;
end;

function TvCategoryNodeData.InternalExecute(ARunFromCategory: Boolean; ASingleInstance: Boolean): boolean;
begin
  Result := ConfirmRunCategory;

  if Result then
    Config.MainTree.IterateSubtree(Self.PNode, CallBackExecuteNode, @ASingleInstance);
end;

function TvCategoryNodeData.InternalExecuteAsAdmin(
  ARunFromCategory: Boolean): boolean;
begin
  Result := ConfirmRunCategory;

  if Result then
    Config.MainTree.IterateSubtree(Self.PNode, CallBackExecuteNodeAsAdmin, nil);
end;

function TvCategoryNodeData.InternalExecuteAsUser(ARunFromCategory: Boolean;
  AUserData: TUserData): boolean;
begin
  Result := ConfirmRunCategory;

  if Result then
    Config.MainTree.IterateSubtree(Self.PNode, CallBackExecuteNodeAsUser, @AUserData);
end;

end.
