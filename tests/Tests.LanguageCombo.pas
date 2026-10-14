unit Tests.LanguageCombo;

{$mode objfpc}{$H+}

{ GUI tests for the language combo on the general options page: the items
  produced by the Find/Resolve/Sort pipeline land in the combo in order,
  refills replace the previous content, and the page creates/frees safely. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry, StdCtrls,
  Frame.Options.General, Language.Manager;

type
  TTestLanguageCombo = class(TTestCase)
  private
    FPage: TfrmGeneralOptionsPage;
    FCombo: TComboBox;
    FTempDir: String;
    procedure WritePo(const AFileName, ANativeName: String);
    procedure DeleteDirRecursive(const ADir: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFillShowsDisplayNamesInOrder;
    procedure TestFillReplacesPreviousContent;
    procedure TestFillEmptyClearsCombo;
    procedure TestEndToEndTempLocaleDir;
  end;

implementation

procedure TTestLanguageCombo.WritePo(const AFileName, ANativeName: String);
var
  FS: TFileStream;
  Raw: RawByteString;
begin
  Raw := RawByteString(
    'msgid ""' + LineEnding +
    'msgstr ""' + LineEnding +
    '"X-Native-Language: ' + ANativeName + '\n"' + LineEnding);
  FS := TFileStream.Create(FTempDir + AFileName, fmCreate);
  try
    if Length(Raw) > 0 then
      FS.WriteBuffer(Raw[1], Length(Raw));
  finally
    FS.Free;
  end;
end;

procedure TTestLanguageCombo.DeleteDirRecursive(const ADir: String);
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

procedure TTestLanguageCombo.SetUp;
begin
  inherited SetUp;
  FPage := TfrmGeneralOptionsPage.Create(nil);
  FCombo := TComboBox.Create(nil);
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'ASuiteLangCombo' +
    PathDelim;
  DeleteDirRecursive(FTempDir);
  ForceDirectories(FTempDir);
end;

procedure TTestLanguageCombo.TearDown;
begin
  DeleteDirRecursive(FTempDir);
  FCombo.Free;
  FPage.Free;
  inherited TearDown;
end;

procedure TTestLanguageCombo.TestFillShowsDisplayNamesInOrder;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 2);
  Items[0].ID := 'it_IT'; Items[0].DisplayName := 'Italiano';
  Items[1].ID := 'en_EN'; Items[1].DisplayName := 'English';
  FPage.FillLanguageCombo(FCombo, Items);
  AssertEquals('Count', 2, FCombo.Items.Count);
  AssertTrue('First', FCombo.Items[0] = 'Italiano');
  AssertTrue('Second', FCombo.Items[1] = 'English');
end;

procedure TTestLanguageCombo.TestFillReplacesPreviousContent;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 2);
  Items[0].ID := 'it_IT'; Items[0].DisplayName := 'Italiano';
  Items[1].ID := 'en_EN'; Items[1].DisplayName := 'English';
  FPage.FillLanguageCombo(FCombo, Items);
  SetLength(Items, 1);
  Items[0].ID := 'de_DE'; Items[0].DisplayName := 'Deutsch';
  FPage.FillLanguageCombo(FCombo, Items);
  AssertEquals('Replaced', 1, FCombo.Items.Count);
  AssertTrue('Content', FCombo.Items[0] = 'Deutsch');
end;

procedure TTestLanguageCombo.TestFillEmptyClearsCombo;
var
  Items: TLangItemArray;
begin
  SetLength(Items, 1);
  Items[0].ID := 'it_IT'; Items[0].DisplayName := 'Italiano';
  FPage.FillLanguageCombo(FCombo, Items);
  SetLength(Items, 0);
  FPage.FillLanguageCombo(FCombo, Items);
  AssertEquals('Cleared', 0, FCombo.Items.Count);
end;

procedure TTestLanguageCombo.TestEndToEndTempLocaleDir;
var
  Items: TLangItemArray;
begin
  // The exact pipeline AddLanguages runs, fed from a fake locale folder.
  WritePo('asuite.it_IT.po', 'Italiano');
  WritePo('asuite.en_EN.po', 'English');
  WritePo('asuite.de_DE.po', 'Deutsch');
  Items := FindLanguageFiles(FTempDir, 'asuite');
  ResolveLanguageNames(Items);
  SortLangItems(Items);
  FPage.FillLanguageCombo(FCombo, Items);
  AssertEquals('Count', 3, FCombo.Items.Count);
  AssertTrue('Sorted native names',
    (FCombo.Items[0] = 'Deutsch') and (FCombo.Items[1] = 'English') and
    (FCombo.Items[2] = 'Italiano'));
end;

initialization
  RegisterTest(TTestLanguageCombo);

end.
