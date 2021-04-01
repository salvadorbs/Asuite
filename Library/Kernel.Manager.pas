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

unit Kernel.Manager;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils, Lists.Manager, Database.Manager, Icons.Manager;

type

  { TASuiteManager }

  TASuiteManager = class
  private
    FListManager : TListManager;
    FDBManager : TDBManager;
    FIconsManager: TIconsManager;
  public
    constructor Create;
    destructor Destroy; override;

    property ListManager: TListManager read FListManager;
    property DBManager: TDBManager read FDBManager;
    property IconsManager: TIconsManager read FIconsManager;
  end;

var
  ASuiteManager: TASuiteManager;

implementation

uses
  Kernel.Logger;

{ TASuiteManager }

constructor TASuiteManager.Create;
begin
  TASuiteLogger.Info('Creating Managers', []);

  FListManager  := TListManager.Create;
  FDBManager    := TDBManager.Create;
  FIconsManager := TIconsManager.Create;
end;

destructor TASuiteManager.Destroy;
begin
  inherited Destroy;

  FListManager.Destroy;
  FDBManager.Destroy;
  FIconsManager.Destroy;
end;

initialization
  ASuiteManager := TASuiteManager.Create;

finalization
  FreeAndNil(ASuiteManager);

end.

