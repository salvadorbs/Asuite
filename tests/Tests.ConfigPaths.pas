unit Tests.ConfigPaths;

{$mode objfpc}{$H+}

{ GUI-suite tests for TConfigPaths (AppConfig.Paths): the suite folder
  layout and the absolute/relative conversions used when saving the list
  and importing it (issue #95 area). The constructor changes the process
  working directory, so every test saves and restores it. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  AppConfig.Paths;

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
  AssertTrue('Drive', FPaths.SuiteDrive = FTempDir);
end;

procedure TTestConfigPaths.TestAbsoluteToRelativeUsesAsuiteVar;
var
  Rel: String;
begin
  Rel := FPaths.AbsoluteToRelative(FTempDir + 'sub' + PathDelim + 'app.exe');
  AssertTrue('Portable form', Rel = '%asuite%/sub/app.exe');
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
  AssertTrue('Legacy var',
    FPaths.RelativeToAbsolute('$asuite/sub/app.exe') =
    FTempDir + 'sub/app.exe');
end;

procedure TTestConfigPaths.TestOutsidePathsStayAbsolute;
begin
  AssertTrue('Unchanged', FPaths.AbsoluteToRelative('/etc/hostname') =
    '/etc/hostname');
  AssertTrue('Unchanged', FPaths.RelativeToAbsolute('/etc/hostname') =
    '/etc/hostname');
end;

procedure TTestConfigPaths.TestUrlPassthrough;
begin
  AssertTrue('URL untouched',
    FPaths.RelativeToAbsolute('https://example.com/x') =
    'https://example.com/x');
end;

initialization
  RegisterTest(TTestConfigPaths);

end.
