unit Tests.IconsCacheKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Icons.CacheKey;

type
  TTestIconsCacheKey = class(TTestCase)
  published
    procedure TestSanitizeCacheName;
    procedure TestHashDeterministic;
    procedure TestShouldCacheIcon;
    procedure TestBuildCacheFileName;
    procedure TestTempSharedVsUnique;
  end;

implementation

procedure TTestIconsCacheKey.TestSanitizeCacheName;
begin
  AssertEquals('abc-123_X', SanitizeCacheName('abc-123_X'));
  AssertEquals('a_b', SanitizeCacheName('a b'));
  AssertTrue(SanitizeCacheName('a/b\c:d') = 'abcd');
  AssertTrue(Length(SanitizeCacheName(StringOfChar('x', 200))) <= 64);
  AssertEquals('', SanitizeCacheName('///'));
end;

procedure TTestIconsCacheKey.TestHashDeterministic;
begin
  AssertEquals(HashPathString('/a/b.exe'), HashPathString('/a/b.exe'));
  AssertEquals(HashPathString('C:\A\B.EXE'), HashPathString('c:\a\b.exe'));
  AssertTrue(HashPathString('/a/one.exe') <> HashPathString('/b/two.exe'));
end;

procedure TTestIconsCacheKey.TestShouldCacheIcon;
begin
  AssertFalse(ShouldCacheIcon('', '/a/b.exe', False));
  AssertFalse(ShouldCacheIcon('a', '/a/b.ico', False));
  AssertFalse(ShouldCacheIcon('a', '/a/b.ico', True));
  AssertFalse(ShouldCacheIcon('a', '/a/b.exe', True));
  AssertTrue(ShouldCacheIcon('a', '/a/b.exe', False));
  AssertTrue(ShouldCacheIcon('a', '/a/b.pdf', True));
  AssertTrue(ShouldCacheIcon('42', '/a/b.pdf', False));
end;

procedure TTestIconsCacheKey.TestBuildCacheFileName;
var
  s: string;
begin
  // Non-temp exe uses stable ID name
  AssertEquals('123.ico', BuildCacheFileName('123', 'C:\apps\tool.exe', False));
  // .ico sources are never cached
  AssertEquals('', BuildCacheFileName('123', '/a/b.ico', False));
  AssertEquals('', BuildCacheFileName('', '/a/b.exe', False));
  // Non-temp generic ext uses ID, not shared ext
  AssertEquals('42.ico', BuildCacheFileName('42', '/a/b.pdf', False));
  // Temp generic ext shares per-extension cache
  AssertEquals('pdf.ico', BuildCacheFileName('tmp', '/a/b.pdf', True));
  // Temp exe gets unique name with path hash (even though ShouldCache is False)
  s := BuildCacheFileName('myapp', '/a/myapp.exe', True);
  AssertTrue((s <> '') and (Pos('myapp_', LowerCase(s)) = 1));
end;

procedure TTestIconsCacheKey.TestTempSharedVsUnique;
var
  s1, s2, p1, p2: string;
begin
  // Same basename, different dirs, temp exe-like: must differ (hash)
  s1 := BuildCacheFileName('docs', '/a/docs', True);
  s2 := BuildCacheFileName('docs', '/b/docs', True);
  AssertTrue((s1 <> '') and (s2 <> '') and (s1 <> s2));
  // Temp pdf in different dirs shares cache (same system icon)
  p1 := BuildCacheFileName('x', '/a/one.pdf', True);
  p2 := BuildCacheFileName('x', '/b/two.pdf', True);
  AssertEquals('pdf.ico', p1);
  AssertEquals(p1, p2);
end;

initialization
  RegisterTest(TTestIconsCacheKey);

end.
