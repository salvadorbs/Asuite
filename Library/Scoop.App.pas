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

unit Scoop.App;

interface

uses
  classes, SysUtils, System.Generics.Collections, MPcommonObjects,
  System.Generics.Defaults;

type

  TScoopApp = class
  private
    { private declarations }
    FName: string;
    FInstalledVersion: string;
    FLatestVersion: string;
    FInstalled: Boolean;

    function GetPathInstManifest: string;
    function GetPathManifest: string;
    function GetPathInstall: string;
    function GetPathDir: string;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AName: string);
    destructor Destroy(); override;

    property Name: string read FName;
    //TODO: Come lo gestiamo???? Version -> manifest interno dell'app, LatestVersion -> manifest del bucket?
    property InstalledVersion: string read FInstalledVersion write FInstalledVersion;
    property LatestVersion: string read FLatestVersion write FLatestVersion;
    property PathDir: string read GetPathDir; //TODO: E no! devo recuperarli dalla ricerca centrallizzata e non in automaticoo recuperarli dalla ricerca centrallizzata e non in automatico
    property PathInstManifest: string read GetPathInstManifest;
    property PathManifest: string read GetPathManifest;
    property PathInstall: string read GetPathInstall;
    property Installed: Boolean read FInstalled write FInstalled;
  end;

  //TODO: Move in another unit
  TScoopApps = class(TObjectList<TScoopApp>)
  public
    constructor Create();

    function SearchByName(const AName: string): TScoopApp;
    function AddItem(const AName: string): TScoopApp;
    procedure ClearInstalledApps();
  end;

implementation

uses
  Path.Utils;

{ TScoopApp }

constructor TScoopApp.Create(AName: string);
begin
  FName := AName;
  FInstalled := False;
end;

destructor TScoopApp.Destroy;
begin

end;

function TScoopApp.GetPathDir: string;
begin
  //TODO: Use TPath.combine!
  //TODO: Siamo proprio sicuri?
  Result := '';
  if FInstalled then
    Result := IncludeTrailingPathDelimiter(ExpandEnvVars('%SCOOP%\apps\' + Self.Name));
end;

function TScoopApp.GetPathInstall: string;
begin
  //TODO: Use TPath.combine!
  Result := '';
  if FInstalled then
    Result := IncludeTrailingPathDelimiter(Self.PathDir + 'current\install.json');
end;

function TScoopApp.GetPathInstManifest: string;
begin
  //TODO: Use TPath.combine!
  Result := '';
  if FInstalled then
    Result := IncludeTrailingPathDelimiter(Self.PathDir + 'current\manifest.json');
end;

function TScoopApp.GetPathManifest: string;
begin
  //TODO: Use TPath.combine!
  //TODO: RICERCA
  //Result := IncludeTrailingPathDelimiter(FBucket.Path + FName + '.json');
end;

{ TScoopApps }

{function TScoopApps.AddApp(const App: TScoopApp): TScoopApp;
begin
  if not Self.Contains() then
  begin

  end;

  App := FApps.SearchByName(Match.Groups[1].Value);

  if not(Assigned(App)) then
  begin
    App := TScoopApp.Create(Match.Groups[1].Value);
    FApps.Add(App);
  end;
end;  }

function TScoopApps.AddItem(const AName: string): TScoopApp;
var
  App: TScoopApp;
begin
  //Avoid possible duplicates
  App := SearchByName(AName);
  if not(Assigned(App)) then
  begin
    App := TScoopApp.Create(AName);
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
