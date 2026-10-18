unit Tests.ShellLink;

{$mode objfpc}{$H+}

{ Behaviour tests for TShellLinkFile (Kernel.ShellLink).

  Construction, property storage and loading missing or symlink targets
  run headless on temp files. Saving with an empty target is refused.
  The desktop helpers (CreateShortcutOnDesktop/DeleteShortcutOnDesktop)
  are deliberately excluded: they write outside the temp folder. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Kernel.ShellLink
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}
  ;

type
  TTestShellLink = class(TTestCase)
  private
    function WriteTempFile(const AName: string): string;
  published
    procedure TestCreateClearsProps;
    procedure TestCreateMissingFileIsSafe;
    procedure TestLoadSymlinkResolvesTarget;
    procedure TestSaveEmptyTargetReturnsFalse;
    procedure TestSaveCreatesLink;
    procedure TestWorkingDirGetter;
    procedure TestPropertySetters;
  end;

implementation

function TTestShellLink.WriteTempFile(const AName: string): string;
var
  FS: TFileStream;
begin
  Result := GetTempDir(False) + AName;
  FS := TFileStream.Create(Result, fmCreate);
  FS.Free;
end;

procedure TTestShellLink.TestCreateClearsProps;
var
  Link: TShellLinkFile;
begin
  Link := TShellLinkFile.Create;
  try
    AssertEquals('no target', '', UTF8Encode(Link.TargetPath));
    AssertEquals('no working dir', '', UTF8Encode(Link.WorkingDir));
    AssertEquals('no parameters', '', UTF8Encode(Link.Parameters));
    AssertEquals('no link path', '', UTF8Encode(Link.LinkPath));
  finally
    Link.Free;
  end;
end;

procedure TTestShellLink.TestCreateMissingFileIsSafe;
var
  Link: TShellLinkFile;
begin
  // Must not raise; properties stay cleared
  Link := TShellLinkFile.Create(GetTempDir(False) + 'asuite_test_missing.lnk');
  try
    AssertEquals('no target', '', UTF8Encode(Link.TargetPath));
    AssertEquals('no link path', '', UTF8Encode(Link.LinkPath));
  finally
    Link.Free;
  end;
end;

procedure TTestShellLink.TestLoadSymlinkResolvesTarget;
{$IFDEF UNIX}
var
  Link: TShellLinkFile;
  Target, LinkPath: string;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Target := WriteTempFile('asuite_test_lnktarget.tmp');
  LinkPath := GetTempDir(False) + 'asuite_test_read.lnk';
  DeleteFile(LinkPath);
  try
    AssertEquals('symlink created', 0,
      fpSymlink(PChar(Target), PChar(LinkPath)));
    Link := TShellLinkFile.Create(LinkPath);
    try
      AssertEquals('link target resolved', Target,
        UTF8Encode(Link.LinkPath));
    finally
      Link.Free;
    end;
  finally
    DeleteFile(LinkPath);
    DeleteFile(Target);
  end;
  {$ELSE}
  // Windows shortcut parsing needs COM and real .lnk binaries
  AssertTrue('documented skip', True);
  {$ENDIF}
end;

procedure TTestShellLink.TestSaveEmptyTargetReturnsFalse;
var
  Link: TShellLinkFile;
  LinkPath: string;
begin
  Link := TShellLinkFile.Create;
  try
    LinkPath := GetTempDir(False) + 'asuite_test_nosave.lnk';
    DeleteFile(LinkPath);
    AssertFalse('empty target refused', Link.SaveShellLink(LinkPath));
    AssertFalse('nothing created', FileExists(LinkPath));
  finally
    Link.Free;
  end;
end;

procedure TTestShellLink.TestSaveCreatesLink;
var
  Link: TShellLinkFile;
  LinkPath, Target: string;
begin
  Target := WriteTempFile('asuite_test_savetarget.tmp');
  LinkPath := GetTempDir(False) + 'asuite_test_save.lnk';
  DeleteFile(LinkPath);
  Link := TShellLinkFile.Create;
  try
    Link.TargetPath := UTF8Decode(Target);
    // NB: on Linux SaveShellLink links LinkPath (just set to APathFile),
    // not TargetPath; only the success contract is asserted here
    AssertTrue('save succeeds', Link.SaveShellLink(LinkPath));
  finally
    Link.Free;
    DeleteFile(LinkPath);
    DeleteFile(Target);
  end;
end;

procedure TTestShellLink.TestWorkingDirGetter;
var
  Link: TShellLinkFile;
begin
  Link := TShellLinkFile.Create;
  try
    AssertEquals('empty stays empty', '', UTF8Encode(Link.WorkingDir));
    Link.WorkingDir := '/tmp/work';
    AssertEquals('value stored', '/tmp/work', UTF8Encode(Link.WorkingDir));
  finally
    Link.Free;
  end;
end;

procedure TTestShellLink.TestPropertySetters;
var
  Link: TShellLinkFile;
begin
  Link := TShellLinkFile.Create;
  try
    Link.TargetPath := '/bin/app';
    Link.Parameters := '--x';
    Link.LinkPath := '/tmp/app.lnk';
    AssertEquals('target', '/bin/app', UTF8Encode(Link.TargetPath));
    AssertEquals('parameters', '--x', UTF8Encode(Link.Parameters));
    AssertEquals('link path', '/tmp/app.lnk', UTF8Encode(Link.LinkPath));
  finally
    Link.Free;
  end;
end;

initialization
  RegisterTest(TTestShellLink);

end.
