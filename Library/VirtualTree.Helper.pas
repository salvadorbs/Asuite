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

unit VirtualTree.Helper;

interface

uses
  Classes, SysUtils, VirtualTrees;

type
  TVSTHelper = class helper for TBaseVirtualTree
  public
    procedure SetCurrentHotNode(const Value: PVirtualNode);
  end;

  THackOptions = Class(TCustomVirtualTreeOptions);

implementation

procedure TVSTHelper.SetCurrentHotNode(const Value: PVirtualNode);
var
  DoInvalidate: Boolean;
const
  MouseButtonDown = [tsLeftButtonDown, tsMiddleButtonDown, tsRightButtonDown];
begin
 if Self.FCurrentHotNode <> Value then
 begin
   DoInvalidate := (toHotTrack in THackOptions(Self.FOptions).PaintOptions) or (toCheckSupport in THackOptions(Self.FOptions).MiscOptions);
   DoHotChange(Self.FCurrentHotNode, Value);
   //Invalidate old Self.FCurrentHotNode
   if Assigned(Self.FCurrentHotNode) and DoInvalidate then
     InvalidateNode(Self.FCurrentHotNode);
   //Set new Self.FCurrentHotNode and invalidate it
   Self.FCurrentHotNode := Value;
   if Assigned(Self.FCurrentHotNode) and DoInvalidate then
     InvalidateNode(Self.FCurrentHotNode);
   //Scroll view
   if (Self.FUpdateCount = 0) and not (toDisableAutoscrollOnFocus in THackOptions(Self.FOptions).AutoOptions) then
     ScrollIntoView(Self.FCurrentHotNode, (toCenterScrollIntoView in THackOptions(Self.FOptions).SelectionOptions) and
        (MouseButtonDown * Self.FStates = []), not (toFullRowSelect in THackOptions(Self.FOptions).SelectionOptions) );
  end;
end;

end.
