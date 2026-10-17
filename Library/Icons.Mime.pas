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

unit Icons.Mime;

{$MODE DelphiUnicode}

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  TIconAvailableFunc = function(const AIconName: string): Boolean of object;

  { TMimeIconProvider }

  TMimeIconProvider = class
  private
    FMap: TObjectDictionary<string, TStringList>;
    function GenericFallback(const AMimeIconName: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStrings(const AGlobs, AGenericIcons: TStrings);
    procedure LoadFromFiles(const AGlobsFile, AGenericIconsFile: string);
    function GetIconNames(const AExtension: string): TStringList;
    function ChooseBestIcon(const AExtension: string; AIsAvailable: TIconAvailableFunc;
      const AFallback: string = ''): string;

    function Count: Integer;
  end;

implementation

{ TMimeIconProvider }

constructor TMimeIconProvider.Create;
begin
  FMap := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
end;

destructor TMimeIconProvider.Destroy;
begin
  FMap.Free;
  inherited;
end;

procedure TMimeIconProvider.Clear;
begin
  FMap.Clear;
end;

function TMimeIconProvider.Count: Integer;
begin
  Result := FMap.Count;
end;

function TMimeIconProvider.GenericFallback(const AMimeIconName: string): string;
var
  p: Integer;
begin
  // "video-ogg" -> "video-x-generic" per shared-mime-info spec
  p := Pos('-', AMimeIconName);
  if p > 0 then
    Result := Copy(AMimeIconName, 1, p - 1) + '-x-generic'
  else
    Result := AMimeIconName + '-x-generic';
end;

procedure TMimeIconProvider.LoadFromStrings(const AGlobs, AGenericIcons: TStrings);
var
  I, p, q: Integer;
  sLine, sMimeType, sMimeIconName, sExtension, sGeneric: string;
  oGeneric: TStringList;
  iconsList: TStringList;
begin
  // NOTE: MIME files and icon names are ASCII; conversions between the
  // AnsiString-based TStrings and this unit's UnicodeString are explicit below.
  Clear;
  if not Assigned(AGlobs) then
    Exit;

  oGeneric := TStringList.Create;
  try
    oGeneric.NameValueSeparator := '=';
    if Assigned(AGenericIcons) then
    begin
      for I := 0 to AGenericIcons.Count - 1 do
      begin
        sLine := UnicodeString(AGenericIcons[I]);
        if (sLine = '') or (sLine[1] = '#') then
          Continue;
        p := Pos(':', sLine);
        if p > 0 then
          oGeneric.Add(AnsiString(Copy(sLine, 1, p - 1) + '=' + Copy(sLine, p + 1, MaxInt)));
      end;
    end;

    for I := 0 to AGlobs.Count - 1 do
    begin
      sLine := UnicodeString(AGlobs[I]);
      if (sLine = '') or (sLine[1] = '#') then
        Continue;
      p := Pos(':', sLine);
      if p <= 0 then
        Continue;
      sMimeType := Copy(sLine, 1, p - 1);
      sExtension := LowerCase(Trim(Copy(sLine, p + 1, MaxInt)));

      // Support only extensions, not full file name masks (keep historic filter)
      if (sExtension = '') or (sExtension = '.*') then
        Continue;

      sMimeIconName := StringReplace(sMimeType, '/', '-', [rfReplaceAll]);

      if not FMap.TryGetValue(sExtension, iconsList) then
      begin
        iconsList := TStringList.Create;
        FMap.Add(sExtension, iconsList);
      end;

      if iconsList.IndexOf(AnsiString(sMimeIconName)) < 0 then
        iconsList.Add(AnsiString(sMimeIconName));

      q := oGeneric.IndexOfName(AnsiString(sMimeType));
      if q >= 0 then
        sGeneric := UnicodeString(oGeneric.ValueFromIndex[q])
      else
        sGeneric := GenericFallback(sMimeIconName);

      if (sGeneric <> '') and (iconsList.IndexOf(AnsiString(sGeneric)) < 0) then
        iconsList.Add(AnsiString(sGeneric));
    end;
  finally
    oGeneric.Free;
  end;
end;

procedure TMimeIconProvider.LoadFromFiles(const AGlobsFile, AGenericIconsFile: string);
var
  globs, generics: TStringList;
begin
  globs := TStringList.Create;
  generics := TStringList.Create;
  try
    if FileExists(AGlobsFile) then
      globs.LoadFromFile(AGlobsFile);
    if (AGenericIconsFile <> '') and FileExists(AGenericIconsFile) then
      generics.LoadFromFile(AGenericIconsFile);
    LoadFromStrings(globs, generics);
  finally
    globs.Free;
    generics.Free;
  end;
end;

function TMimeIconProvider.GetIconNames(const AExtension: string): TStringList;
begin
  if not FMap.TryGetValue(LowerCase(AExtension), Result) then
    Result := nil;
end;

function TMimeIconProvider.ChooseBestIcon(const AExtension: string;
  AIsAvailable: TIconAvailableFunc; const AFallback: string): string;
var
  names: TStringList;
  I: Integer;
begin
  Result := '';
  names := GetIconNames(AExtension);
  if Assigned(names) and Assigned(@AIsAvailable) then
  begin
    for I := 0 to names.Count - 1 do
    begin
      Result := UnicodeString(names[I]);
      if AIsAvailable(Result) then
        Exit;
    end;
  end;
  Result := AFallback;
end;

end.
