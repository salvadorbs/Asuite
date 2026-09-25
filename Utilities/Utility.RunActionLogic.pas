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

{ Pure decision logic for the Run/Run as/Run as admin/Open file location
  actions. Kept free of VirtualTrees/LCL so it can be unit tested. }
unit Utility.RunActionLogic;

{$MODE DelphiUnicode}

interface

uses
  Kernel.Enumerations;

type
  { The relevant traits of one selected node. }
  TRunActionItem = record
    IsSeparator: Boolean;
    IsFileItem: Boolean;
    IsExecutable: Boolean;
    IsUrlProtocol: Boolean;
  end;
  TRunActionItems = array of TRunActionItem;

{ True when at least one node can handle the given run mode. The result does
  not depend on the order of the items. }
function IsRunActionEnabled(AMode: TRunMode;
  const AItems: TRunActionItems): Boolean;

implementation

function IsRunActionEnabled(AMode: TRunMode;
  const AItems: TRunActionItems): Boolean;
var
  I: Integer;
  Item: TRunActionItem;
begin
  for I := Low(AItems) to High(AItems) do
  begin
    Item := AItems[I];
    case AMode of
      rmNormal:
        // Run: any node but a separator (categories run their children).
        if not Item.IsSeparator then
          Exit(True);

      rmAsUser, rmAsAdmin:
        // Run as/as admin: categories are allowed, files must be executable.
        if (not Item.IsSeparator) and
           ((not Item.IsFileItem) or Item.IsExecutable) then
          Exit(True);

      rmExplorePath:
        // Open file location: only real file paths, not URL protocols.
        if Item.IsFileItem and (not Item.IsUrlProtocol) then
          Exit(True);
    end;
  end;

  Result := False;
end;

end.
