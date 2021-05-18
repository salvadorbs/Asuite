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

unit VirtualTree.Helper;

{$Mode delphiunicode}

interface

uses
  Classes, SysUtils, VirtualTrees;

type

  { VirtualTreeHelper }

  VirtualTreeHelper = class helper for TVirtualStringTree
    private
      procedure MarkNodes(State: TVirtualTreeStates);
      function ProcessPaste(Source: TBaseVirtualTree; TargetNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean;
    public
      procedure FakeCopyToClipBoard;
      procedure FakeCutToClipBoard;
      function FakePasteFromClipboard: Boolean;
  end;

implementation


{ VirtualTreeHelper }

function VirtualTreeHelper.ProcessPaste(Source: TBaseVirtualTree;
  TargetNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean;
var
  Nodes: TNodeArray;
  I: Integer;
  IsCopy: Boolean;

begin
  Nodes := nil;

  if not (toReadOnly in Self.TreeOptions.MiscOptions) then
  begin
    BeginUpdate;
    Result := False;
    try
      if TargetNode = nil then
        TargetNode := Self.RootNode;

      if TargetNode = Self.RootNode then
      begin
        case Mode of
          amInsertBefore:
            Mode := amAddChildFirst;
          amInsertAfter:
            Mode := amAddChildLast;
        end;
      end;

      Nodes := Source.GetSortedCutCopySet(True);
      IsCopy := not(tsCutPending in Self.TreeStates);

      if Mode in [amInsertBefore,amAddChildLast] then
      begin
        for I := 0 to High(Nodes) do
          if not HasAsParent(TargetNode, Nodes[I]) then
          begin
            if IsCopy then
              Source.CopyTo(Nodes[I], TargetNode, Mode, False)
            else
              Source.MoveTo(Nodes[I], TargetNode, Mode, False);
          end;
      end
      else
      begin
        for I := High(Nodes) downto 0 do
          if not HasAsParent(TargetNode, Nodes[I]) then
          begin
            if IsCopy then
              Source.CopyTo(Nodes[I], TargetNode, Mode, False)
            else
              Source.MoveTo(Nodes[I], TargetNode, Mode, False);
          end;
      end;

      Result := True;
    finally
      EndUpdate;
    end;
  end;
end;

procedure VirtualTreeHelper.FakeCopyToClipBoard;
begin
  MarkNodes([tsCopyPending]);
end;

procedure VirtualTreeHelper.FakeCutToClipBoard;
begin
  MarkNodes([tsCutPending]);
end;

function VirtualTreeHelper.FakePasteFromClipboard: Boolean;
begin
  Result := False;
  if not (toReadOnly in Self.TreeOptions.MiscOptions) then
    Result := ProcessPaste(Self, Self.FocusedNode, Self.DefaultPasteMode);

  CancelCutOrCopy;
end;

procedure VirtualTreeHelper.MarkNodes(State: TVirtualTreeStates);
begin             
  if (toReadOnly in Self.TreeOptions.MiscOptions) then
    Exit;

  CancelCutOrCopy;

  if SelectedCount > 0 then
  begin
    MarkCutCopyNodes;
    DoStateChange(State);
    Invalidate;
  end;
end;

end.

