unit Tests.FindFiles;

{$mode objfpc}{$H+}

{ Behaviour tests for TFindFiles (Thread.FindFiles).

  The scanner runs in its own thread over a temp folder and reports through
  events, which only append here: the test waits for termination before
  asserting, so no locking is needed. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Thread.FindFiles, Kernel.Consts;

type
  TTestFindFiles = class(TTestCase)
  private
    FFound: TStringList;
    FEndTotal: Integer;
    FEndFired: Boolean;
    FDir: string;
    procedure OnFileFound(AFilePath: UnicodeString);
    procedure OnSearchEnd(ATotalFiles: Integer; ATime: Cardinal);
    function NewFinder: TFindFiles;
    procedure MakeDir;
    procedure WipeDir;
    procedure WriteFile(const AName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultProperties;
    procedure TestEmptyDirectoryFiresNothing;
    procedure TestFindsMatchingFiles;
    procedure TestCriteriaWithMask;
    procedure TestExcludeListSkips;
    procedure TestStopBeforeStart;
    procedure TestMissingDirectoryEndsEmpty;
  end;

implementation

procedure TTestFindFiles.OnFileFound(AFilePath: UnicodeString);
begin
  FFound.Add(UTF8Encode(AFilePath));
end;

procedure TTestFindFiles.OnSearchEnd(ATotalFiles: Integer; ATime: Cardinal);
begin
  FEndFired := True;
  FEndTotal := ATotalFiles;
end;

function TTestFindFiles.NewFinder: TFindFiles;
begin
  Result := TFindFiles.Create;
  // Manual lifetime: WaitFor then Free, no race with FreeOnTerminate
  Result.FreeOnTerminate := False;
  Result.OnFileFound := @OnFileFound;
  Result.OnSearchEnd := @OnSearchEnd;
end;

procedure TTestFindFiles.MakeDir;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'asuite_test_find' +
    PathDelim;
  ForceDirectories(FDir);
end;

procedure TTestFindFiles.WipeDir;

  procedure WipeOne(const ADir: string);
  var
    Inner: TSearchRec;
    Q: string;
  begin
    if FindFirst(ADir + '*', faAnyFile, Inner) = 0 then
    try
      repeat
        if (Inner.Name <> '.') and (Inner.Name <> '..') then
        begin
          Q := ADir + Inner.Name;
          if (Inner.Attr and faDirectory) <> 0 then
            WipeOne(IncludeTrailingPathDelimiter(Q))
          else
            DeleteFile(Q);
        end;
      until FindNext(Inner) <> 0;
    finally
      FindClose(Inner);
    end;
    RemoveDir(ADir);
  end;

begin
  WipeOne(FDir);
end;

procedure TTestFindFiles.WriteFile(const AName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FDir + AName, fmCreate);
  FS.Free;
end;

procedure TTestFindFiles.SetUp;
begin
  inherited SetUp;
  FFound := TStringList.Create;
  FEndTotal := -1;
  FEndFired := False;
  MakeDir;
end;

procedure TTestFindFiles.TearDown;
begin
  WipeDir;
  FFound.Free;
  inherited TearDown;
end;

procedure TTestFindFiles.TestDefaultProperties;
var
  Finder: TFindFiles;
begin
  Finder := NewFinder;
  try
    AssertEquals('no directory', '', Finder.Directory);
    AssertNotNull('criteria list assigned', Finder.SearchCriteriaFilename);
    AssertNotNull('exclude list assigned', Finder.SearchExcludeFilename);
    AssertEquals('lists start empty', 0, Finder.SearchCriteriaFilename.Count);
    AssertEquals('lists start empty', 0, Finder.SearchExcludeFilename.Count);
  finally
    Finder.Free;
  end;
end;

procedure TTestFindFiles.TestEmptyDirectoryFiresNothing;
var
  Finder: TFindFiles;
begin
  Finder := NewFinder;
  try
    Finder.Start;
    Finder.WaitFor;
    AssertFalse('no end on empty directory', FEndFired);
    AssertEquals('nothing found', 0, FFound.Count);
  finally
    Finder.Free;
  end;
end;

procedure TTestFindFiles.TestFindsMatchingFiles;
var
  Finder: TFindFiles;
begin
  WriteFile('a.tmp');
  WriteFile('b.tmp');
  WriteFile('c.dat');
  Finder := NewFinder;
  try
    Finder.Directory := FDir;
    Finder.SearchCriteriaFilename.Add('.tmp');
    Finder.Start;
    Finder.WaitFor;
    AssertTrue('end fired', FEndFired);
    AssertEquals('two matches reported', 2, FEndTotal);
    AssertEquals('two matches collected', 2, FFound.Count);
  finally
    Finder.Free;
  end;
end;

procedure TTestFindFiles.TestCriteriaWithMask;
var
  Finder: TFindFiles;
begin
  WriteFile('a.tmp');
  WriteFile('b.dat');
  Finder := NewFinder;
  try
    Finder.Directory := FDir;
    Finder.SearchCriteriaFilename.Add(EXT_PATH_MASK + '.tmp');
    Finder.Start;
    Finder.WaitFor;
    AssertEquals('masked criteria match', 1, FEndTotal);
  finally
    Finder.Free;
  end;
end;

procedure TTestFindFiles.TestExcludeListSkips;
var
  Finder: TFindFiles;
begin
  WriteFile('run.tmp');
  WriteFile('uninstall.tmp');
  Finder := NewFinder;
  try
    Finder.Directory := FDir;
    Finder.SearchCriteriaFilename.Add('.tmp');
    Finder.SearchExcludeFilename.Add('uninstall');
    Finder.Start;
    Finder.WaitFor;
    AssertEquals('excluded file skipped', 1, FEndTotal);
    AssertEquals('one collected', 1, FFound.Count);
    AssertTrue('the right one',
      Pos('run.tmp', FFound[0]) > 0);
  finally
    Finder.Free;
  end;
end;

procedure TTestFindFiles.TestStopBeforeStart;
var
  Finder: TFindFiles;
begin
  WriteFile('a.tmp');
  Finder := NewFinder;
  try
    Finder.Directory := FDir;
    Finder.SearchCriteriaFilename.Add('.tmp');
    Finder.Stop;
    Finder.Start;
    Finder.WaitFor;
    AssertTrue('terminated', FEndFired);
    AssertEquals('aborted before counting', 0, FEndTotal);
  finally
    Finder.Free;
  end;
end;

procedure TTestFindFiles.TestMissingDirectoryEndsEmpty;
var
  Finder: TFindFiles;
begin
  Finder := NewFinder;
  try
    Finder.Directory := FDir + 'no-such-dir' + PathDelim;
    Finder.SearchCriteriaFilename.Add('.tmp');
    Finder.Start;
    Finder.WaitFor;
    AssertTrue('end fired', FEndFired);
    AssertEquals('nothing found', 0, FEndTotal);
  finally
    Finder.Free;
  end;
end;

initialization
  RegisterTest(TTestFindFiles);

end.
