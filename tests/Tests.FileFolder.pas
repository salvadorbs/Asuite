unit Tests.FileFolder;

{$mode objfpc}{$H+}

{ GUI-suite tests for the side-effect-free helpers in Utility.FileFolder
  (ExtractFileNameEx and DeleteOldBackups are excluded: they need the
  running application singleton). }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Kernel.Types, Utility.FileFolder;

type
  TTestFileFolder = class(TTestCase)
  private
    FTempDir: String;
    procedure WriteFile(const AFileName, AContent: String);
    procedure DeleteDirRecursive(const ADir: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsDirectory;
    procedure TestListFilesByWildcard;
    procedure TestListFilesMissingDir;
    procedure TestDeleteFiles;
    procedure TestDeleteFilesMissingDir;
    procedure TestExtractLowerFileExt;
    procedure TestGetUrlTarget;
    procedure TestGetUrlTargetMissingFile;
  end;

implementation

procedure TTestFileFolder.WriteFile(const AFileName, AContent: String);
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

procedure TTestFileFolder.DeleteDirRecursive(const ADir: String);
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

procedure TTestFileFolder.SetUp;
begin
  inherited SetUp;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'ASuiteFileTest' +
    PathDelim;
  DeleteDirRecursive(FTempDir);
  ForceDirectories(FTempDir);
end;

procedure TTestFileFolder.TearDown;
begin
  DeleteDirRecursive(FTempDir);
  inherited TearDown;
end;

procedure TTestFileFolder.TestIsDirectory;
begin
  WriteFile('f.txt', 'x');
  AssertTrue('Dir', IsDirectory(FTempDir));
  AssertFalse('File', IsDirectory(FTempDir + 'f.txt'));
  AssertFalse('Missing', IsDirectory(FTempDir + 'nope'));
end;

procedure TTestFileFolder.TestListFilesByWildcard;
var
  List: TStringList;
begin
  WriteFile('a.txt', 'a');
  WriteFile('b.txt', 'b');
  WriteFile('c.log', 'c');
  List := TStringList.Create;
  try
    AssertTrue('Found', ListFiles(FTempDir, '*.txt', List));
    List.Sort;
    AssertEquals('Count', 2, List.Count);
    AssertTrue('Names', (List[0] = 'a.txt') and (List[1] = 'b.txt'));
  finally
    List.Free;
  end;
end;

procedure TTestFileFolder.TestListFilesMissingDir;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    AssertFalse('Missing dir', ListFiles(FTempDir + 'nope' + PathDelim,
      '*.txt', List));
    AssertEquals('Empty', 0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestFileFolder.TestDeleteFiles;
var
  List: TStringList;
begin
  WriteFile('a.tmp', 'a');
  WriteFile('b.tmp', 'b');
  WriteFile('c.tmp', 'c');
  WriteFile('keep.txt', 'keep');
  AssertEquals('Deleted', 3, DeleteFiles(FTempDir, '*.tmp'));
  List := TStringList.Create;
  try
    AssertTrue('Listed', ListFiles(FTempDir, '*', List));
    AssertEquals('Only keep remains', 1, List.Count);
    AssertTrue('Name', List[0] = 'keep.txt');
  finally
    List.Free;
  end;
end;

procedure TTestFileFolder.TestDeleteFilesMissingDir;
begin
  // Must not raise.
  AssertEquals('Zero', 0, DeleteFiles(FTempDir + 'nope' + PathDelim, '*.tmp'));
end;

procedure TTestFileFolder.TestExtractLowerFileExt;
begin
  AssertTrue('Lowered', ExtractLowerFileExt('App.EXE') = '.exe');
  AssertTrue('Empty', ExtractLowerFileExt('noext') = '');
  AssertTrue('Last ext', ExtractLowerFileExt('archive.tar.gz') = '.gz');
end;

procedure TTestFileFolder.TestGetUrlTarget;
var
  Url: TUrlFile;
begin
  WriteFile('s.url',
    '[InternetShortcut]' + LineEnding +
    'URL=https://example.com/x' + LineEnding +
    'WorkingDirectory=/tmp' + LineEnding +
    'IconFile=/tmp/i.ico' + LineEnding);
  Url := GetUrlTarget(FTempDir + 's.url');
  AssertTrue('URL', Url.TargetFile = 'https://example.com/x');
  AssertTrue('Workdir', Url.WorkingDir = '/tmp');
  AssertTrue('Icon', Url.PathIcon = '/tmp/i.ico');
end;

procedure TTestFileFolder.TestGetUrlTargetMissingFile;
var
  Url: TUrlFile;
begin
  // Missing file: defaults, no raise.
  Url := GetUrlTarget(FTempDir + 'gone.url');
  AssertTrue('Default target', Url.TargetFile = FTempDir + 'gone.url');
  AssertTrue('Default workdir', Url.WorkingDir = '');
end;

initialization
  RegisterTest(TTestFileFolder);

end.
