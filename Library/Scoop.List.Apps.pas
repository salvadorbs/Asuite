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

unit Scoop.List.Apps;

interface

uses
  classes, SysUtils, System.Generics.Collections, MPcommonObjects,
  System.Generics.Defaults, Scoop.App;

type

  TScoopApps = class(TObjectList<TScoopApp>)
  public
    constructor Create();

    function SearchByName(const AName: string): TScoopApp;
    function AddItem(const AName: string; ABucket: TObject): TScoopApp;
    procedure ClearInstalledApps();
  end;

implementation

{ TScoopApps }

function TScoopApps.AddItem(const AName: string; ABucket: TObject): TScoopApp;
var
  App: TScoopApp;
begin
  //Avoid possible duplicates
  App := SearchByName(AName);
  if not(Assigned(App)) then
  begin
    App := TScoopApp.Create(AName, ABucket);
    Self.Add(App);
  end;

  Result := App;
end;

procedure TScoopApps.ClearInstalledApps;
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    Self[I].Installed := False;
end;

constructor TScoopApps.Create;
var comparer : IComparer<TScoopApp>;
    comparison : TComparison<TScoopApp>;
begin
    comparison := function(const l, r : TScoopApp): Integer
                  begin
                    Result := CompareStr(l.Name, r.Name);
                  end;

    comparer := TComparer<TScoopApp>.Construct(comparison);

    inherited Create(comparer);
end;

function TScoopApps.SearchByName(const AName: string): TScoopApp;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do
  begin
    if Self[I].Name = AName then
    begin
      Result := Self[I];
      Exit;
    end;
  end;
end;

end.

