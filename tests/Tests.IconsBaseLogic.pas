unit Tests.IconsBaseLogic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  BGRABitmap, Icons.Base, Icons.Files, Icons.Thread, VirtualTrees;

type
  { Counts LoadIcon calls, never touches globals. }
  TCountingIcon = class(TBaseIcon)
  private
    FCalls: Integer;
  protected
    function GetName: UnicodeString; override;
    function GetDefaultPathIcon: UnicodeString; override;
    function GetIconFromFile(const APathFile: UnicodeString;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
    function LoadIcon: Integer; override;
  public
    property Calls: Integer read FCalls;
  end;

  { Stub with in-memory FS and injectable cache behaviour. }
  TStubCacheIcon = class(TBaseIcon)
  private
    FUseCache: Boolean;
    FImporting: Boolean;
    FCacheDir: UnicodeString;
    FRequestedPaths: TStringList;
    FAddedCount: Integer;
    FImgListCalls: Integer;
    FSmallW, FSmallH, FLargeW, FLargeH: Integer;
  protected
    function GetName: UnicodeString; override;
    function GetDefaultPathIcon: UnicodeString; override;
    function GetIconFromFile(const APathFile: UnicodeString;
      const AWantLargeIcon: Boolean): TBGRABitmap; override;
    function DoUseCache: Boolean; override;
    function DoIsImporting: Boolean; override;
    function DoFileExists(const APath: UnicodeString): Boolean; override;
    function DoPathExists(const APath: UnicodeString): Boolean; override;
    function DoGetCacheDir: UnicodeString; override;
    function DoAddMultipleResolutions(const ASmall, ALarge: TBGRABitmap): Integer; override;
    function DoGetIconFromImgList(AImageIndex: Integer; ALargeIcon: Boolean): TBGRABitmap; override;
  public
    constructor Create(AUseCache, AImporting: Boolean);
    destructor Destroy; override;
    property RequestedPaths: TStringList read FRequestedPaths;
    property AddedCount: Integer read FAddedCount;
    property ImgListCalls: Integer read FImgListCalls;
    property CacheDir: UnicodeString read FCacheDir write FCacheDir;
  end;

  { Real .ico parsing through TFileIcon, but ImageList insert stubbed. }
  TSmokeFileIcon = class(TFileIcon)
  private
    FAdded: Integer;
    FSmallW, FSmallH, FLargeW, FLargeH: Integer;
  protected
    function DoUseCache: Boolean; override;
    function DoIsImporting: Boolean; override;
    function DoGetCacheDir: UnicodeString; override;
    function DoAddMultipleResolutions(const ASmall, ALarge: TBGRABitmap): Integer; override;
  public
    property Added: Integer read FAdded;
    property SmallW: Integer read FSmallW;
    property SmallH: Integer read FSmallH;
    property LargeW: Integer read FLargeW;
    property LargeH: Integer read FLargeH;
  end;

  TTestIconsBaseLogic = class(TTestCase)
  published
    procedure TestLazyLoadOnce;
    procedure TestCacheHitUsesCachePath;
    procedure TestImportingSkipsCache;
    procedure TestRealIcoFixtureLoads;
    procedure TestSaveCacheRoundTrip;
    procedure TestResetCacheIcon;
  end;

  { Lifecycle only: with no app globals the preload is a safe no-op, but the
    snapshot/sync plumbing must start and finish cleanly. }
  TTestIconsThreadLifecycle = class(TTestCase)
  private
    procedure PumpAndWait(AThread: TThread);
  published
    procedure TestNilTreeFinishes;
    procedure TestEmptyTreeFinishes;
  end;

implementation

function FixtureIco: string;
var
  base: string;
begin
  base := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim +
    'bin' + PathDelim + 'themes' + PathDelim + 'default' + PathDelim +
    'icons' + PathDelim + 'accept.ico');
  if FileExists(base) then
    Exit(base);
  Result := ExpandFileName('bin' + PathDelim + 'themes' + PathDelim + 'default' +
    PathDelim + 'icons' + PathDelim + 'accept.ico');
end;

function TempCacheDir(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) + AName;
  ForceDirectories(Result);
end;

procedure CleanCacheDir(const ADir, AFile: string);
begin
  DeleteFile(ADir + PathDelim + AFile);
  RemoveDir(ADir);
end;

{ TCountingIcon }

function TCountingIcon.GetName: UnicodeString;
begin
  Result := 'stub';
end;

function TCountingIcon.GetDefaultPathIcon: UnicodeString;
begin
  Result := '';
end;

function TCountingIcon.GetIconFromFile(const APathFile: UnicodeString;
  const AWantLargeIcon: Boolean): TBGRABitmap;
begin
  Result := nil;
end;

function TCountingIcon.LoadIcon: Integer;
begin
  Inc(FCalls);
  Result := 7;
end;

{ TStubCacheIcon }

constructor TStubCacheIcon.Create(AUseCache, AImporting: Boolean);
begin
  inherited Create(False);
  FUseCache := AUseCache;
  FImporting := AImporting;
  FRequestedPaths := TStringList.Create;
  FCacheDir := '/cache/';
  TempItem := False;
end;

destructor TStubCacheIcon.Destroy;
begin
  FRequestedPaths.Free;
  inherited;
end;

function TStubCacheIcon.GetName: UnicodeString;
begin
  Result := 'stub';
end;

function TStubCacheIcon.GetDefaultPathIcon: UnicodeString;
begin
  Result := '/docs/app.exe';
end;

function TStubCacheIcon.GetIconFromFile(const APathFile: UnicodeString;
  const AWantLargeIcon: Boolean): TBGRABitmap;
begin
  FRequestedPaths.Add(APathFile);
  if AWantLargeIcon then
    Result := TBGRABitmap.Create(32, 32)
  else
    Result := TBGRABitmap.Create(16, 16);
end;

function TStubCacheIcon.DoUseCache: Boolean;
begin
  Result := FUseCache;
end;

function TStubCacheIcon.DoIsImporting: Boolean;
begin
  Result := FImporting;
end;

function TStubCacheIcon.DoFileExists(const APath: UnicodeString): Boolean;
begin
  // Fake FS: source exe and its legacy cache file both "exist";
  // anything else falls through to the real FS (temp dirs for save tests)
  Result := (APath = '/docs/app.exe') or (APath = '/cache/stub.ico') or
    FileExists(APath);
end;

function TStubCacheIcon.DoPathExists(const APath: UnicodeString): Boolean;
begin
  Result := APath <> '';
end;

function TStubCacheIcon.DoGetCacheDir: UnicodeString;
begin
  Result := FCacheDir;
end;

function TStubCacheIcon.DoAddMultipleResolutions(const ASmall,
  ALarge: TBGRABitmap): Integer;
begin
  Inc(FAddedCount);
  if Assigned(ASmall) then
  begin
    FSmallW := ASmall.Width;
    FSmallH := ASmall.Height;
  end;
  if Assigned(ALarge) then
  begin
    FLargeW := ALarge.Width;
    FLargeH := ALarge.Height;
  end;
  Result := 42;
end;

function TStubCacheIcon.DoGetIconFromImgList(AImageIndex: Integer;
  ALargeIcon: Boolean): TBGRABitmap;
begin
  Inc(FImgListCalls);
  if ALargeIcon then
    Result := TBGRABitmap.Create(32, 32)
  else
    Result := TBGRABitmap.Create(16, 16);
end;

{ TSmokeFileIcon }

function TSmokeFileIcon.DoUseCache: Boolean;
begin
  Result := False;
end;

function TSmokeFileIcon.DoIsImporting: Boolean;
begin
  Result := False;
end;

function TSmokeFileIcon.DoGetCacheDir: UnicodeString;
begin
  Result := '';
end;

function TSmokeFileIcon.DoAddMultipleResolutions(const ASmall,
  ALarge: TBGRABitmap): Integer;
begin
  Inc(FAdded);
  if Assigned(ASmall) then
  begin
    FSmallW := ASmall.Width;
    FSmallH := ASmall.Height;
  end;
  if Assigned(ALarge) then
  begin
    FLargeW := ALarge.Width;
    FLargeH := ALarge.Height;
  end;
  Result := 11;
end;

{ TTestIconsBaseLogic }

procedure TTestIconsBaseLogic.TestLazyLoadOnce;
var
  Icon: TCountingIcon;
begin
  Icon := TCountingIcon.Create;
  try
    AssertEquals(7, Icon.ImageIndex);
    AssertEquals(7, Icon.ImageIndex);
    AssertEquals(1, Icon.Calls);
    AssertTrue(Icon.CheckImageIndex);
  finally
    Icon.Free;
  end;
end;

procedure TTestIconsBaseLogic.TestCacheHitUsesCachePath;
var
  Icon: TStubCacheIcon;
begin
  // CacheIconCRC = 0 matches fake hash of missing real file (0), so cache hits
  Icon := TStubCacheIcon.Create(True, False);
  try
    Icon.CacheIconCRC := 0;
    AssertEquals(42, Icon.ImageIndex);
    AssertEquals(2, Icon.RequestedPaths.Count);
    AssertEquals('/cache/stub.ico', Icon.RequestedPaths[0]);
    AssertEquals('/cache/stub.ico', Icon.RequestedPaths[1]);
    AssertEquals(1, Icon.AddedCount);
    AssertEquals(0, Icon.ImgListCalls);
  finally
    Icon.Free;
  end;
end;

procedure TTestIconsBaseLogic.TestImportingSkipsCache;
var
  Icon: TStubCacheIcon;
begin
  Icon := TStubCacheIcon.Create(True, True);
  try
    Icon.CacheIconCRC := 0;
    AssertEquals(42, Icon.ImageIndex);
    AssertEquals(2, Icon.RequestedPaths.Count);
    AssertEquals('/docs/app.exe', Icon.RequestedPaths[0]);
    AssertEquals('/docs/app.exe', Icon.RequestedPaths[1]);
    AssertEquals(0, Icon.ImgListCalls);
  finally
    Icon.Free;
  end;
end;

procedure TTestIconsBaseLogic.TestRealIcoFixtureLoads;
var
  Icon: TSmokeFileIcon;
  path: string;
begin
  path := FixtureIco;
  AssertTrue('missing fixture: ' + path, FileExists(path));
  Icon := TSmokeFileIcon.Create(path);
  try
    AssertEquals(11, Icon.ImageIndex);
    AssertEquals(1, Icon.Added);
    AssertTrue(Icon.SmallW > 0);
    AssertTrue(Icon.LargeW > 0);
  finally
    Icon.Free;
  end;
end;

procedure TTestIconsBaseLogic.TestSaveCacheRoundTrip;
var
  Icon: TStubCacheIcon;
  dir, cached: string;
begin
  dir := TempCacheDir('icons-savetest');
  cached := dir + PathDelim + 'stub.ico';
  DeleteFile(cached);
  Icon := TStubCacheIcon.Create(True, False);
  try
    Icon.CacheDir := dir + PathDelim;
    Icon.CacheIconCRC := 12345; // mismatch vs hash 0 -> cache miss -> reload+save
    AssertEquals(42, Icon.ImageIndex);
    AssertTrue('cache file written: ' + cached, FileExists(cached));
    AssertEquals(0, Icon.CacheIconCRC);
    AssertEquals(2, Icon.ImgListCalls);
  finally
    Icon.Free;
    CleanCacheDir(dir, 'stub.ico');
  end;
end;

procedure TTestIconsBaseLogic.TestResetCacheIcon;
var
  Icon: TStubCacheIcon;
  dir, cached: string;
  f: TextFile;
begin
  dir := TempCacheDir('icons-resetest');
  cached := dir + PathDelim + 'stub.ico';
  AssignFile(f, cached);
  Rewrite(f);
  Write(f, 'stale');
  CloseFile(f);
  Icon := TStubCacheIcon.Create(True, False);
  try
    Icon.CacheDir := dir + PathDelim;
    Icon.CacheIconCRC := 99;
    Icon.ResetCacheIcon;
    AssertFalse('cache file removed: ' + cached, FileExists(cached));
    AssertEquals(0, Icon.CacheIconCRC);
  finally
    Icon.Free;
    CleanCacheDir(dir, 'stub.ico');
  end;
end;

{ TTestIconsThreadLifecycle }

procedure TTestIconsThreadLifecycle.PumpAndWait(AThread: TThread);
var
  deadline: QWord;
begin
  deadline := GetTickCount64 + 5000;
  while not AThread.Finished and (GetTickCount64 < deadline) do
  begin
    CheckSynchronize(10);
    Sleep(10);
  end;
  AssertTrue('thread did not finish', AThread.Finished);
end;

procedure TTestIconsThreadLifecycle.TestNilTreeFinishes;
var
  Thread: TTreeIconsThread;
begin
  Thread := TTreeIconsThread.Create(nil, nil);
  try
    Thread.FreeOnTerminate := False;
    Thread.Start;
    PumpAndWait(Thread);
    WaitForThreadTerminate(Thread.Handle, 1000);
  finally
    Thread.Free;
  end;
end;

procedure TTestIconsThreadLifecycle.TestEmptyTreeFinishes;
var
  Tree: TVirtualStringTree;
  Thread: TTreeIconsThread;
begin
  Tree := TVirtualStringTree.Create(nil);
  try
    Thread := TTreeIconsThread.Create(Tree, nil);
    try
      Thread.FreeOnTerminate := False;
      Thread.Start;
      PumpAndWait(Thread);
      WaitForThreadTerminate(Thread.Handle, 1000);
    finally
      Thread.Free;
    end;
  finally
    Tree.Free;
  end;
end;

initialization
  RegisterTest(TTestIconsBaseLogic);
  RegisterTest(TTestIconsThreadLifecycle);

end.
