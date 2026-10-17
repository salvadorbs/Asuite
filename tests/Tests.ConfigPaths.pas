unit Tests.ConfigPaths;

{$mode objfpc}{$H+}

{ GUI-suite tests for TConfigPaths (AppConfig.Paths): the suite folder
  layout and the absolute/relative conversions used when saving the list
  and importing it (issue #95 area). The constructor changes the process
  working directory, so every test saves and restores it. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  AppConfig.Paths, AppConfig.PathList;

type
  TTestConfigPaths = class(TTestCase)
  private
    FPaths: TConfigPaths;
    FTempDir: String;
    FOldDir: String;
    procedure DeleteDirRecursive(const ADir: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSuitePathsPointInsideExeFolder;
    procedure TestAbsoluteToRelativeUsesAsuiteVar;
    procedure TestRoundTrip;
    procedure TestEmptyStaysEmpty;
    procedure TestLegacyDollarVarsExpanded;
    procedure TestOutsidePathsStayAbsolute;
    procedure TestUrlPassthrough;
  end;

  TTestPathList = class(TTestCase)
  private
    function NewList: TPathList;
  published
    procedure TestExpandKnownVar;
    procedure TestExpandLeavesUnknownUntouched;
    procedure TestExpandWithoutVars;
    procedure TestAddIgnoresDuplicateKey;
    procedure TestDeExpandRoundTrip;
    procedure TestClearRemovesVars;
  end;

implementation

procedure TTestConfigPaths.DeleteDirRecursive(const ADir: String);
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

procedure TTestConfigPaths.SetUp;
begin
  inherited SetUp;
  FOldDir := GetCurrentDir;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'ASuitePathsTest' +
    PathDelim;
  DeleteDirRecursive(FTempDir);
  ForceDirectories(FTempDir);
  // A writable folder: SuitePathData stays inside, nothing is written
  // outside the temp dir.
  FPaths := TConfigPaths.Create(FTempDir + 'ASuite.exe');
end;

procedure TTestConfigPaths.TearDown;
begin
  FPaths.Free;
  FPaths := nil;
  SetCurrentDir(FOldDir);
  DeleteDirRecursive(FTempDir);
  AssertTrue('Working directory restored', GetCurrentDir = FOldDir);
  inherited TearDown;
end;

procedure TTestConfigPaths.TestSuitePathsPointInsideExeFolder;
begin
  AssertTrue('Locale', FPaths.SuitePathLocale = FTempDir + 'locale' + PathDelim);
  AssertTrue('Data', FPaths.SuitePathData = FTempDir);
  {$IFDEF MSWINDOWS}
  // The launcher's drive is the real drive letter on Windows
  AssertTrue('Drive', FPaths.SuiteDrive =
    LowerCase(ExtractFileDrive(FTempDir)));
  {$ELSE}
  // There is no drive on Linux: the launcher's folder is used instead
  AssertTrue('Drive', FPaths.SuiteDrive = FTempDir);
  {$ENDIF}
end;

procedure TTestConfigPaths.TestAbsoluteToRelativeUsesAsuiteVar;
var
  Rel: String;
begin
  Rel := FPaths.AbsoluteToRelative(FTempDir + 'sub' + PathDelim + 'app.exe');
  AssertTrue('Portable form',
    Rel = '%asuite%' + PathDelim + 'sub' + PathDelim + 'app.exe');
end;

procedure TTestConfigPaths.TestRoundTrip;
var
  Abs, Rel: String;
begin
  Abs := FTempDir + 'sub' + PathDelim + 'app.exe';
  Rel := FPaths.AbsoluteToRelative(Abs);
  AssertTrue('Back to absolute', FPaths.RelativeToAbsolute(Rel) = Abs);
end;

procedure TTestConfigPaths.TestEmptyStaysEmpty;
begin
  AssertTrue('To relative', FPaths.AbsoluteToRelative('') = '');
  AssertTrue('To absolute', FPaths.RelativeToAbsolute('') = '');
end;

procedure TTestConfigPaths.TestLegacyDollarVarsExpanded;
begin
  // Old '$asuite' form is still understood for backwards compatibility.
  // Build the path with the platform delimiter: $asuite expands to the
  // launcher's folder without a trailing separator, so the stored value
  // must use the native one.
  AssertTrue('Legacy var',
    FPaths.RelativeToAbsolute('$asuite' + PathDelim + 'sub' + PathDelim +
      'app.exe') =
    FTempDir + 'sub' + PathDelim + 'app.exe');
end;

procedure TTestConfigPaths.TestOutsidePathsStayAbsolute;
var
  Outside: String;
begin
  // An absolute path outside the suite folder. On Windows it is re-expressed
  // through %drive%, but it must never collapse into the suite-relative
  // %asuite% form; on Linux it stays literally unchanged.
  Outside := ExtractFileDrive(FTempDir) + PathDelim +
    'asuite_outside' + PathDelim + 'file.txt';
  AssertTrue('not rewritten into %asuite%',
    Pos('%asuite%', FPaths.AbsoluteToRelative(Outside)) = 0);
  AssertTrue('already absolute stays absolute',
    FPaths.RelativeToAbsolute(Outside) = Outside);
end;

procedure TTestConfigPaths.TestUrlPassthrough;
begin
  AssertTrue('URL untouched',
    FPaths.RelativeToAbsolute('https://example.com/x') =
    'https://example.com/x');
end;

{ TTestPathList }

function TTestPathList.NewList: TPathList;
begin
  Result := TPathList.Create('%(\w+)%');
end;

procedure TTestPathList.TestExpandKnownVar;
var
  L: TPathList;
begin
  L := NewList;
  try
    L.Add('ASUITE', '/opt/asuite');
    // Matching is case-insensitive on the key, the value is inserted as-is
    AssertEquals('var expanded', '/opt/asuite/app.exe',
      UTF8Encode(L.ExpandVars('%asuite%/app.exe')));
  finally
    L.Free;
  end;
end;

procedure TTestPathList.TestExpandLeavesUnknownUntouched;
var
  L: TPathList;
begin
  L := NewList;
  try
    L.Add('ASUITE', '/opt/asuite');
    AssertEquals('unknown var kept', '%missing%/x',
      UTF8Encode(L.ExpandVars('%missing%/x')));
  finally
    L.Free;
  end;
end;

procedure TTestPathList.TestExpandWithoutVars;
var
  L: TPathList;
begin
  L := NewList;
  try
    AssertEquals('plain path untouched', '/usr/bin/tool',
      UTF8Encode(L.ExpandVars('/usr/bin/tool')));
  finally
    L.Free;
  end;
end;

procedure TTestPathList.TestAddIgnoresDuplicateKey;
var
  L: TPathList;
begin
  L := NewList;
  try
    L.Add('ASUITE', '/first');
    L.Add('ASUITE', '/second');
    AssertEquals('first value wins', '/first/app',
      UTF8Encode(L.ExpandVars('%ASUITE%/app')));
  finally
    L.Free;
  end;
end;

procedure TTestPathList.TestDeExpandRoundTrip;
var
  L: TPathList;
begin
  L := NewList;
  try
    L.Add('ASUITE', '/opt/asuite');
    AssertEquals('value becomes var', '%asuite%/app.exe',
      UTF8Encode(L.DeExpandVars('/opt/asuite/app.exe', 'ASUITE')));
  finally
    L.Free;
  end;
end;

procedure TTestPathList.TestClearRemovesVars;
var
  L: TPathList;
begin
  L := NewList;
  try
    L.Add('ASUITE', '/opt/asuite');
    L.Clear;
    AssertEquals('no var after clear', '%ASUITE%/app',
      UTF8Encode(L.ExpandVars('%ASUITE%/app')));
  finally
    L.Free;
  end;
end;

initialization
  RegisterTest(TTestConfigPaths);
  RegisterTest(TTestPathList);

end.
