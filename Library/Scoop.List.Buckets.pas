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

unit Scoop.List.Buckets;

interface

uses
  classes, SysUtils, Scoop.App, Scoop.Bucket,
  System.Generics.Defaults, System.Generics.Collections, Winapi.Windows,
  MPcommonObjects, JsonDataObjects, MPCommonUtilities;

type

  TScoopBuckets = class(TObjectList<TScoopBucket>)
  public
    function AddApp(const AName: string; const ABucketName: string): TScoopApp;
    function AddBucket(const AName: string): TScoopBucket;
    function SearchAppByName(const AName: string): TScoopApp;
    function SearchBucketByName(const AName: string): TScoopBucket;
    procedure ClearApps();
    procedure ClearInstalledApps();
    procedure LoadAllApps();
  end;

implementation

uses
  MPShellUtilities, StrUtils;

{ TScoopBuckets }

function TScoopBuckets.AddApp(const AName: string; const ABucketName: string): TScoopApp;
var
  Bucket: TScoopBucket;
begin
  Result := nil;

  Bucket := SearchBucketByName(ABucketName);
  if Assigned(Bucket) then
    Result := Bucket.Apps.AddItem(AName, Bucket);
end;

function TScoopBuckets.AddBucket(const AName: string): TScoopBucket;
var
  Bucket: TScoopBucket;
begin
  //Avoid possible duplicates
  Bucket := SearchBucketByName(AName);
  if not(Assigned(Bucket)) then
  begin
    Bucket := TScoopBucket.Create(AName);
    Self.Add(Bucket);
  end;

  Result := Bucket;
end;

procedure TScoopBuckets.ClearApps;
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    Self.Items[I].Apps.Clear;
end;

procedure TScoopBuckets.ClearInstalledApps;
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    Self.Items[I].Apps.ClearInstalledApps;
end;

procedure TScoopBuckets.LoadAllApps;
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    Self[I].LoadApps;
end;

function TScoopBuckets.SearchAppByName(const AName: string): TScoopApp;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Self.Count - 1 do
  begin
    Result := Self.Items[I].Apps.SearchByName(AName);
    if Assigned(Result) then
      Break;
  end;
end;

function TScoopBuckets.SearchBucketByName(const AName: string): TScoopBucket;
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
    end
  end;
end;

end.
