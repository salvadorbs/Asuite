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

unit Language.Manager;

{$MODE DelphiUnicode}

interface

uses
  Classes;

type
  // One available translation: ID selects the .po file and is stored in
  // the config, FilePath locates it, DisplayName is shown to the user.
  TLangItem = record
    ID: String;
    FilePath: String;
    DisplayName: String;
  end;
  TLangItemArray = array of TLangItem;

// Scans LocaleDir for '<base>.<langid>.po' files
function FindLanguageFiles(const LocaleDir, BaseName: String): TLangItemArray;
// Fills DisplayName of every item (native name from the .po header,
// file ID when the header is missing)
procedure ResolveLanguageNames(var Items: TLangItemArray);
// Sorts items by display name, deterministically on every platform
procedure SortLangItems(var Items: TLangItemArray);
// Index of ALangID in AList: exact match first, then language-code match
// (ex. legacy 'en' matches 'en_EN'), -1 when absent
function IndexOfLangID(AList: TStringList; const ALangID: String): Integer;

implementation

uses
  SysUtils, FileUtil, LazFileUtils, Kernel.Consts;

function LanguageBaseCode(const ALangID: String): String;
var
  pSep, pDash: Integer;
begin
  // Language code without country code, lowercased (ex. 'it_IT' -> 'it')
  pSep := Pos('_', ALangID);
  pDash := Pos('-', ALangID);
  if (pDash <> 0) and ((pSep = 0) or (pDash < pSep)) then
    pSep := pDash;
  if pSep > 0 then
    Result := LowerCase(Copy(ALangID, 1, pSep - 1))
  else
    Result := LowerCase(ALangID);
end;

function GetLanguageName(const APOFile: String): String;
const
  NATIVE_LANG_KEY = 'X-Native-Language:';
  MAX_HEADER_SIZE = 8192; // the header is at the top of the file
var
  FS: TFileStream;
  Raw: RawByteString;
  Text: String;
  StartPos, EndPos: Integer;
begin
  // Reads the native language name from the 'X-Native-Language' header
  // entry of a .po file (the same convention used by Double Commander),
  // '' when missing or unreadable. The caller falls back to the file ID.
  Result := '';
  try
    // .po files are UTF-8: read raw bytes and decode explicitly, so the
    // content is correct on every platform
    FS := TFileStream.Create(APOFile, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size > MAX_HEADER_SIZE then
        SetLength(Raw, MAX_HEADER_SIZE)
      else
        SetLength(Raw, FS.Size);
      if Length(Raw) > 0 then
        FS.ReadBuffer(Raw[1], Length(Raw));
    finally
      FreeAndNil(FS);
    end;
    Text := UTF8Decode(Raw);
    StartPos := Pos(NATIVE_LANG_KEY, Text);
    if StartPos = 0 then
      Exit;
    StartPos := StartPos + Length(NATIVE_LANG_KEY);
    while (StartPos <= Length(Text)) and
          ((Text[StartPos] = ' ') or (Text[StartPos] = #9)) do
      Inc(StartPos);
    // The value ends at the '\n' escape closing the header line
    EndPos := StartPos;
    while (EndPos <= Length(Text)) and (Text[EndPos] <> '\') and
          (Text[EndPos] <> #10) and (Text[EndPos] <> #13) do
      Inc(EndPos);
    Result := Trim(Copy(Text, StartPos, EndPos - StartPos));
  except
    Result := '';
  end;
end;

function TryExtractLangID(const FileName, BasePrefix: String; out LangID: String): Boolean;
var
  NameOnly: String;
begin
  // Splits '<base>.<langid>' out of a .po file name, matching <base>
  // case-insensitively so files are found on case-sensitive filesystems
  // (Linux) whatever case the base name was saved with
  Result := False;
  LangID := '';
  NameOnly := ExtractFileNameWithoutExt(FileName);
  if Pos('.', NameOnly) = 0 then
    Exit;
  if not SameText(Copy(NameOnly, 1, Length(BasePrefix)), BasePrefix) then
    Exit;
  LangID := Copy(NameOnly, Length(BasePrefix) + 1, MaxInt);
  Result := LangID <> '';
end;

function IndexOfID(const Items: TLangItemArray; const LangID: String): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Items) do
    if Items[I].ID = LangID then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function FindLanguageFiles(const LocaleDir, BaseName: String): TLangItemArray;
var
  FileInfo: TSearchRec;
  BasePrefix, LangID: String;
begin
  SetLength(Result, 0);
  BasePrefix := LowerCase(BaseName) + '.';
  if FindFirstUTF8(LocaleDir + '*' + EXT_PO, faAnyFile, FileInfo) <> 0 then
    Exit;
  try
    repeat
      if (FileInfo.Attr and faDirectory = 0) and
         TryExtractLangID(FileInfo.Name, BasePrefix, LangID) and
         (IndexOfID(Result, LangID) = -1) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)].ID := LangID;
        Result[High(Result)].FilePath := LocaleDir + FileInfo.Name;
      end;
    until FindNextUTF8(FileInfo) <> 0;
  finally
    FindCloseUTF8(FileInfo);
  end;
end;

procedure ResolveLanguageNames(var Items: TLangItemArray);
var
  I: Integer;
begin
  for I := 0 to High(Items) do
  begin
    Items[I].DisplayName := GetLanguageName(Items[I].FilePath);
    if Items[I].DisplayName = '' then
      Items[I].DisplayName := Items[I].ID;
  end;
end;

function CompareLangItems(const A, B: TLangItem): Integer;
begin
  Result := CompareText(A.DisplayName, B.DisplayName);
  if Result = 0 then
    Result := CompareStr(A.DisplayName, B.DisplayName);
  if Result = 0 then
    Result := CompareText(A.ID, B.ID);
end;

procedure SortLangItems(var Items: TLangItemArray);
var
  I, J: Integer;
  Current: TLangItem;
begin
  // Insertion sort: a language folder holds a handful of entries, and a
  // small stable sort keeps this unit self-contained
  for I := 1 to High(Items) do
  begin
    Current := Items[I];
    J := I - 1;
    while (J >= 0) and (CompareLangItems(Items[J], Current) > 0) do
    begin
      Items[J + 1] := Items[J];
      Dec(J);
    end;
    Items[J + 1] := Current;
  end;
end;

function IndexOfLangID(AList: TStringList; const ALangID: String): Integer;
var
  I: Integer;
begin
  Result := AList.IndexOf(ALangID);
  if Result <> -1 then
    Exit;
  for I := 0 to AList.Count - 1 do
    if LanguageBaseCode(AList[I]) = LanguageBaseCode(ALangID) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

end.
