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
  classes, SysUtils, MPcommonObjects;

type

  TScoopApp = class
  private
    { private declarations }
    FName: string;
    FVersionInstalled: string;
    FVersionLatest: string;
    FInstalled: Boolean;
    FBucket: TObject;

    function GetPathManifest: string;
    function GetPathInstallDir: string;
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AName: string; ABucket: TObject);
    destructor Destroy(); override;

    property Name: string read FName;
    property Bucket: TObject read FBucket;

    //Versions
    property VersionInstalled: string read FVersionInstalled write FVersionInstalled;
    property VersionLatest: string read FVersionLatest write FVersionLatest;

    //Paths
    property PathDir: string read GetPathInstallDir;
    property PathManifest: string read GetPathManifest;

    property Installed: Boolean read FInstalled write FInstalled;
  end;

implementation

uses
  Path.Utils, Scoop.Bucket, System.IOUtils;

{ TScoopApp }

constructor TScoopApp.Create(AName: string; ABucket: TObject);
begin
  FName := AName;
  FBucket := ABucket;

  FInstalled := False;
end;

destructor TScoopApp.Destroy;
begin

end;

function TScoopApp.GetPathInstallDir: string;
begin
  Result := TPath.Combine(ExpandEnvVars('%SCOOP%\apps\'), Self.Name);
end;

function TScoopApp.GetPathManifest: string;
begin
  Result := TPath.Combine(TScoopBucket(FBucket).Path, FName + '.json');
end;

end.
