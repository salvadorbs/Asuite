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

unit VirtualTree.Helper;

{$MODE Delphi}

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
  with Self do begin
    if FCurrentHotNode <> Value then
    begin
      DoInvalidate := (toHotTrack in THackOptions(FOptions).PaintOptions) or
        (toCheckSupport in THackOptions(FOptions).MiscOptions);
      DoHotChange(FCurrentHotNode, Value);
      // Invalidate old FCurrentHotNode
      if Assigned(FCurrentHotNode) and DoInvalidate then
        InvalidateNode(FCurrentHotNode);
      // Set new FCurrentHotNode and invalidate it
      FCurrentHotNode := Value;
      if Assigned(FCurrentHotNode) and DoInvalidate then
        InvalidateNode(FCurrentHotNode);
      // Scroll view
      if (FUpdateCount = 0) and
        not(toDisableAutoscrollOnFocus in THackOptions(FOptions).AutoOptions)
      then
        ScrollIntoView(FCurrentHotNode,
          (toCenterScrollIntoView in THackOptions(FOptions).SelectionOptions)
          and (MouseButtonDown * FStates = []),
          not(toFullRowSelect in THackOptions(FOptions).SelectionOptions));
    end;
  end;
end;

end.
