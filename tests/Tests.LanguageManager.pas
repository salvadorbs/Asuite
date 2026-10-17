unit Tests.LanguageManager;

{$mode objfpc}{$H+}

{ Tests for the language-change logic (Language.Manager): scanning the
  locale folder, resolving native display names from the .po headers,
  sorting, and matching the configured LangID (including legacy codes
  such as 'en' matching 'en_EN'). }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Language.Manager;

type
  TTestLanguageManager = class(TTestCase)
  private
    FTempDir: String;
    procedure WriteFixture(const AFileName, AContent: String);
    procedure WritePo(const AFileName, ANativeName: String);
    procedure DeleteDirRecursive(const ADir: String);
    function SortedIDs(const Items: TLangItemArray): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // FindLanguageFiles
    procedure TestFindsMatchingFiles;
    procedure TestBaseNameMatchIsCaseInsensitive;
    procedure TestIgnoresNonPoFiles;
    procedure TestIgnoresFileWithoutLangID;
    procedure TestIgnoresDirectories;
    procedure TestMissingDirectoryReturnsEmpty;
    procedure TestDeduplicatesSameID;
    // ResolveLanguageNames (covers the X-Native-Language header parser)
    procedure TestNativeNameFromHeader;
    procedure TestFallsBackToIDWhenHeaderMissing;
    procedure TestMissingFileFallsBackToID;
    procedure TestHeaderValueIsTrimmed;
    // SortLangItems
    procedure TestSortsByDisplayName;
    procedure TestSortIsCaseInsensitiveAndDeterministic;
    procedure TestSortEmptyAndSingleAreSafe;
    // IndexOfLangID
    procedure TestExactMatch;
    procedure TestLegacyBaseCodeMatch;
    procedure TestDashFormMatches;
    procedure TestAbsentReturnsMinusOne;
    // End to end on the shipped locale folder
    procedure TestRealLocaleFolder;
  end;

implementation

const
  PO_HEADER_TEMPLATE =
    'msgid ""' + LineEnding +
    'msgstr ""' + LineEnding +
    '"Language: %s\n"' + LineEnding +
    '"X-Native-Language: %s\n"' + LineEnding;

procedure TTestLanguageManager.WriteFixture(const AFileName, AContent: String);
var
  FS: TFileStream;
  Raw: RawByteString;
begin
  Raw := RawByteString(AContent);
  FS := TFileStream.Create(FTempDir + AFileName, fmCreate);
  try
    if Length(Raw) > 0 then
      FS.WriteBuffer(Raw[1], Length(Raw));
  finally
    FS.Free;
  end;
end;

procedure TTestLanguageManager.WritePo(const AFileName, ANativeName: String);
begin
  WriteFixture(AFileName, Format(PO_HEADER_TEMPLATE, [AFileName, ANativeName]));
end;

procedure TTestLanguageManager.DeleteDirRecursive(const ADir: String);
var
  SR: TSearchRec;
  P: String;
begin
  if FindFirst(ADir + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        P := ADir + SR.Name;
        if (SR.Attr and faDirectory) <> 0 then
          DeleteDirRecursive(IncludeTrailingPathDelimiter(P))
        else
          DeleteFile(P);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
  RemoveDir(ADir);
end;

function TTestLanguageManager.SortedIDs(const Items: TLangItemArray): String;
var
  List: TStringList;
  I: Integer;
begin
  // FindFirst order is filesystem-dependent: sort before comparing.
  List := TStringList.Create;
  try
    for I := 0 to High(Items) do
      List.Add(Items[I].ID);
    List.Sort;
    Result := '';
    for I := 0 to List.Count - 1 do
    begin
      if I > 0 then
        Result := Result + ',';
      Result := Result + List[I];
    end;
  finally
    List.Free;
  end;
end;

procedure TTestLanguageManager.SetUp;
begin
  inherited SetUp;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'ASuiteLangTest' +
    PathDelim;
  DeleteDirRecursive(FTempDir);
  ForceDirectories(FTempDir);
end;

procedure TTestLanguageManager.TearDown;
begin
  DeleteDirRecursive(FTempDir);
  inherited TearDown;
end;

procedure TTestLanguageManager.TestFindsMatchingFiles;
var
  Items: TLangItemArray;
begin
  WritePo('asuite.en_EN.po', 'English');
  WritePo('asuite.it_IT.po', 'Italiano');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  AssertEquals('Two languages found', 2, Length(Items));
  AssertTrue('IDs', SortedIDs(Items) = 'en_EN,it_IT');
end;

procedure TTestLanguageManager.TestBaseNameMatchIsCaseInsensitive;
var
  Items: TLangItemArray;
begin
  // Files are found on case-sensitive filesystems whatever case the base
  // name was saved with (see TryExtractLangID).
  WritePo('ASUITE.fr_FR.po', 'Francais');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  AssertEquals('Found', 1, Length(Items));
  AssertTrue('ID keeps file case', Items[0].ID = 'fr_FR');
end;

procedure TTestLanguageManager.TestIgnoresNonPoFiles;
var
  Items: TLangItemArray;
begin
  WritePo('asuite.en_EN.po', 'English');
  WriteFixture('asuite.pot', 'template');
  WriteFixture('asuite.en_EN.mo', 'compiled');
  WriteFixture('readme.txt', 'notes');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  AssertEquals('Only .po found', 1, Length(Items));
  AssertTrue('ID', Items[0].ID = 'en_EN');
end;

procedure TTestLanguageManager.TestIgnoresFileWithoutLangID;
var
  Items: TLangItemArray;
begin
  WritePo('asuite.en_EN.po', 'English');
  WriteFixture('asuite.po', 'no language id');
  WriteFixture('other.en_EN.po', 'wrong base');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  AssertEquals('Only valid found', 1, Length(Items));
end;

procedure TTestLanguageManager.TestIgnoresDirectories;
begin
  WritePo('asuite.en_EN.po', 'English');
  ForceDirectories(FTempDir + 'asuite.de_DE.po');
  try
    AssertEquals('Directory ignored', 1,
      Length(FindLanguageFiles(FTempDir, 'asuite')));
  finally
    RemoveDir(FTempDir + 'asuite.de_DE.po');
  end;
end;

procedure TTestLanguageManager.TestMissingDirectoryReturnsEmpty;
var
  Items: TLangItemArray;
begin
  // Must not raise.
  Items := FindLanguageFiles(FTempDir + 'does-not-exist' + PathDelim, 'asuite');
  AssertEquals('Empty', 0, Length(Items));
end;

procedure TTestLanguageManager.TestDeduplicatesSameID;
var
  Items: TLangItemArray;
begin
  WritePo('asuite.en_EN.po', 'English');
  WritePo('ASUITE.en_EN.po', 'English again');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  AssertEquals('Deduplicated', 1, Length(Items));
  AssertTrue('ID', Items[0].ID = 'en_EN');
end;

procedure TTestLanguageManager.TestNativeNameFromHeader;
var
  Items: TLangItemArray;
begin
  WritePo('asuite.it_IT.po', 'Italiano');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  ResolveLanguageNames(Items);
  AssertTrue('Native name', Items[0].DisplayName = 'Italiano');
end;

procedure TTestLanguageManager.TestFallsBackToIDWhenHeaderMissing;
var
  Items: TLangItemArray;
begin
  WriteFixture('asuite.xx_YY.po', 'msgid ""' + LineEnding + 'msgstr ""' + LineEnding);
  Items := FindLanguageFiles(FTempDir, 'asuite');
  ResolveLanguageNames(Items);
  AssertTrue('Falls back to ID', Items[0].DisplayName = 'xx_YY');
end;

procedure TTestLanguageManager.TestMissingFileFallsBackToID;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 1);
  Items[0].ID := 'zz_ZZ';
  Items[0].FilePath := FTempDir + 'gone.po';
  // Must not raise.
  ResolveLanguageNames(Items);
  AssertTrue('Falls back to ID', Items[0].DisplayName = 'zz_ZZ');
end;

procedure TTestLanguageManager.TestHeaderValueIsTrimmed;
var
  Items: TLangItemArray;
begin
  WriteFixture('asuite.pt_BR.po',
    'msgid ""' + LineEnding +
    'msgstr ""' + LineEnding +
    '"X-Native-Language:   Português  \n"' + LineEnding);
  Items := FindLanguageFiles(FTempDir, 'asuite');
  ResolveLanguageNames(Items);
  // Compare on UTF-8 bytes: in this objfpc unit the literal is an AnsiString
  // holding raw UTF-8, while DisplayName is UnicodeString, and the RTL would
  // otherwise convert with the wrong code page (DefaultSystemCodePage = 0).
  AssertTrue('Trimmed UTF-8 value', UTF8Encode(Items[0].DisplayName) = 'Português');
end;

procedure TTestLanguageManager.TestSortsByDisplayName;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 3);
  Items[0].ID := 'it_IT'; Items[0].DisplayName := 'Italiano';
  Items[1].ID := 'en_EN'; Items[1].DisplayName := 'English';
  Items[2].ID := 'de_DE'; Items[2].DisplayName := 'Deutsch';
  SortLangItems(Items);
  AssertTrue('Order', (Items[0].ID = 'de_DE') and (Items[1].ID = 'en_EN') and
    (Items[2].ID = 'it_IT'));
end;

procedure TTestLanguageManager.TestSortIsCaseInsensitiveAndDeterministic;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 3);
  Items[0].ID := 'x1'; Items[0].DisplayName := 'banana';
  Items[1].ID := 'x2'; Items[1].DisplayName := 'apple';
  Items[2].ID := 'x3'; Items[2].DisplayName := 'Apple';
  SortLangItems(Items);
  // 'Apple' vs 'apple' compare equal case-insensitively: the ordinal
  // tie-break keeps the order deterministic on every platform.
  AssertTrue('Order', (Items[0].DisplayName = 'Apple') and
    (Items[1].DisplayName = 'apple') and (Items[2].DisplayName = 'banana'));
end;

procedure TTestLanguageManager.TestSortEmptyAndSingleAreSafe;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 0);
  SortLangItems(Items); // Must not raise.
  SetLength(Items, 1);
  Items[0].ID := 'it_IT';
  Items[0].DisplayName := 'Italiano';
  SortLangItems(Items);
  AssertTrue('Kept', Items[0].ID = 'it_IT');
end;

procedure TTestLanguageManager.TestExactMatch;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('en_EN');
    List.Add('it_IT');
    AssertEquals('Exact', 1, IndexOfLangID(List, 'it_IT'));
  finally
    List.Free;
  end;
end;

procedure TTestLanguageManager.TestLegacyBaseCodeMatch;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('en_EN');
    List.Add('it_IT');
    // A stored 'en' (pre-ID configs) still selects en_EN.
    AssertEquals('Base code', 0, IndexOfLangID(List, 'en'));
  finally
    List.Free;
  end;
end;

procedure TTestLanguageManager.TestDashFormMatches;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('en_EN');
    AssertEquals('Dash form', 0, IndexOfLangID(List, 'en-US'));
  finally
    List.Free;
  end;
end;

procedure TTestLanguageManager.TestAbsentReturnsMinusOne;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('en_EN');
    AssertEquals('Absent', -1, IndexOfLangID(List, 'fr_FR'));
    List.Clear;
    AssertEquals('Empty list', -1, IndexOfLangID(List, 'en_EN'));
  finally
    List.Free;
  end;
end;

procedure TTestLanguageManager.TestRealLocaleFolder;
var
  LocaleDir: String;
  Items: TLangItemArray;
  I, EnIdx, ItIdx: Integer;
begin
  // End to end on the shipped translations, located from the test binary
  // so the test works whatever the current directory is.
  LocaleDir := IncludeTrailingPathDelimiter(ExpandFileName(
    ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'bin' + PathDelim +
    'locale'));
  if not DirectoryExists(LocaleDir) then
    Exit; // Layout without shipped locales: nothing to check.
  Items := FindLanguageFiles(LocaleDir, 'asuite');
  ResolveLanguageNames(Items);
  SortLangItems(Items);
  EnIdx := -1;
  ItIdx := -1;
  for I := 0 to High(Items) do
  begin
    if Items[I].ID = 'en_EN' then
      EnIdx := I;
    if Items[I].ID = 'it_IT' then
      ItIdx := I;
  end;
  AssertTrue('en_EN shipped', EnIdx <> -1);
  AssertTrue('it_IT shipped', ItIdx <> -1);
  AssertTrue('English native name', Items[EnIdx].DisplayName = 'English');
  AssertTrue('Italiano native name', Items[ItIdx].DisplayName = 'Italiano');
  AssertTrue('Sorted', EnIdx < ItIdx);
end;

initialization
  RegisterTest(TTestLanguageManager);

end.
