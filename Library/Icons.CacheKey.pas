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

unit Icons.CacheKey;

{$MODE DelphiUnicode}

interface

const
  EXT_ICO_CACHE = '.ico';
  EXT_EXE_CACHE = '.exe';

function NormalizeExt(const AExt: string): string;
function NormalizeExtKey(const AExt: string): string;
function SanitizeCacheName(const AName: string): string;
function HashPathString(const APath: string): Cardinal;
function ShouldCacheIcon(const AName, APath: string; ATempItem: Boolean): Boolean;
function BuildCacheFileName(const AName, ADefaultPath: string; ATempItem: Boolean): string;

implementation

uses
  SysUtils;

function NormalizeExt(const AExt: string): string;
var
  s: string;
begin
  s := Trim(AExt);
  if s = '' then
    Exit('');
  // Accept '*.txt', '*.', '*', 'txt', '.txt' forms
  if (Length(s) >= 2) and (s[1] = '*') and (s[2] = '.') then
    Delete(s, 1, 1)
  else if (s = '*') or (s = '*.*') then
    Exit('');
  if (s <> '') and (s[1] <> '.') then
    s := '.' + s;
  if s = '.*' then
    Exit('');
  Result := LowerCase(s);
end;

function SanitizeCacheName(const AName: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AName) do
  begin
    C := AName[I];
    if C in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'] then
      Result := Result + C
    else if C in [' ', '.'] then
      Result := Result + '_'
    // Skip any other char (path delimiters, etc.)
  end;
  // Limit length to avoid filesystem issues (keep room for hash + ext)
  if Length(Result) > 64 then
    SetLength(Result, 64);
end;

function HashPathString(const APath: string): Cardinal;
var
  I: Integer;
  sLower: string;
begin
  // FNV-1a 32-bit over lowercase path, dependency free
  Result := $811C9DC5;
  sLower := LowerCase(APath);
  for I := 1 to Length(sLower) do
  begin
    Result := Result xor Cardinal(Ord(sLower[I]) and $FFFF);
    Result := Result * $01000193;
  end;
end;

function ExtractExtLower(const APath: string): string;
begin
  Result := LowerCase(ExtractFileExt(APath));
end;

function NormalizeExtKey(const AExt: string): string;
var
  sNorm: string;
begin
  sNorm := NormalizeExt(AExt);
  if sNorm = '' then
    Result := LowerCase(Trim(AExt))
  else
    Result := '*' + sNorm;
end;

function ShouldCacheIcon(const AName, APath: string; ATempItem: Boolean): Boolean;
var
  sExt: string;
begin
  Result := False;
  if AName = '' then
    Exit;
  sExt := ExtractExtLower(APath);
  if sExt = EXT_ICO_CACHE then
    Exit;
  if ATempItem and (sExt = EXT_EXE_CACHE) then
    Exit;
  Result := True;
end;

function BuildCacheFileName(const AName, ADefaultPath: string; ATempItem: Boolean): string;
var
  sExt, sBase: string;
begin
  Result := '';
  if AName = '' then
    Exit;
  sExt := ExtractExtLower(ADefaultPath);
  // Never cache .ico sources themselves
  if sExt = EXT_ICO_CACHE then
    Exit;
  if (sExt = EXT_EXE_CACHE) or (sExt = '') or (not ATempItem) then
  begin
    // Unique per item: Name is ID for nodes (stable), basename for temp files.
    // For temp items add a path hash to avoid basename collisions.
    // NOTE: non-temp generic extensions (e.g. .lnk/.pdf nodes) previously shared
    // a single '<ext>.ico' file; they now use '<id>.ico'. Legacy shared files may
    // remain orphaned in the cache dir (still reused by temp items) - harmless.
    sBase := SanitizeCacheName(LowerCase(AName));
    if sBase = '' then
      Exit;
    if ATempItem then
      Result := sBase + '_' + IntToHex(HashPathString(ADefaultPath), 8) + EXT_ICO_CACHE
    else
      Result := sBase + EXT_ICO_CACHE;
  end
  else
  begin
    // Temp + generic extension: icon depends only on extension, share cache
    if (Length(sExt) > 1) then
      Result := Copy(sExt, 2, Length(sExt) - 1) + EXT_ICO_CACHE;
  end;
end;

end.
