unit Tests.IconsMime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Icons.Mime;

type
  TAvailStub = class
  public
    Available: TStringList;
    function IsAvailable(const AIconName: UnicodeString): Boolean;
  end;

  TTestIconsMime = class(TTestCase)
  private
    FProvider: TMimeIconProvider;
    FAvail: TAvailStub;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure LoadSample;
  published
    procedure TestParseCount;
    procedure TestTxtIcons;
    procedure TestPngUsesGeneric;
    procedure TestIgnoreNoise;
    procedure TestCaseInsensitive;
    procedure TestChooseBest;
    procedure TestMissingExt;
  end;

implementation

{ TAvailStub }

function TAvailStub.IsAvailable(const AIconName: UnicodeString): Boolean;
begin
  Result := Assigned(Available) and (Available.IndexOf(AIconName) >= 0);
end;

{ TTestIconsMime }

procedure TTestIconsMime.SetUp;
begin
  FProvider := TMimeIconProvider.Create;
  FAvail := TAvailStub.Create;
  FAvail.Available := TStringList.Create;
end;

procedure TTestIconsMime.TearDown;
begin
  FAvail.Available.Free;
  FAvail.Free;
  FProvider.Free;
end;

procedure TTestIconsMime.LoadSample;
var
  globs, generics: TStringList;
begin
  globs := TStringList.Create;
  generics := TStringList.Create;
  try
    globs.Add('text/plain:*.txt');
    globs.Add('image/png:*.png');
    globs.Add('# comment line');
    globs.Add('');
    globs.Add('application/x-foo:*.foo');
    globs.Add('weird:.*');
    generics.Add('image/png:custom-png-icon');
    FProvider.LoadFromStrings(globs, generics);
  finally
    globs.Free;
    generics.Free;
  end;
end;

procedure TTestIconsMime.TestParseCount;
begin
  LoadSample;
  AssertEquals(3, FProvider.Count);
end;

procedure TTestIconsMime.TestTxtIcons;
var
  names: TStringList;
begin
  LoadSample;
  names := FProvider.GetIconNames('*.txt');
  AssertNotNull(names);
  AssertTrue(names.IndexOf('text-plain') >= 0);
  // No generic entry for text/plain -> fallback text-x-generic
  AssertTrue(names.IndexOf('text-x-generic') >= 0);
end;

procedure TTestIconsMime.TestPngUsesGeneric;
var
  names: TStringList;
begin
  LoadSample;
  names := FProvider.GetIconNames('*.png');
  AssertNotNull(names);
  AssertTrue(names.IndexOf('image-png') >= 0);
  AssertTrue(names.IndexOf('custom-png-icon') >= 0);
end;

procedure TTestIconsMime.TestIgnoreNoise;
begin
  LoadSample;
  AssertNull(FProvider.GetIconNames('.*'));
  AssertNull(FProvider.GetIconNames('*.missing'));
end;

procedure TTestIconsMime.TestCaseInsensitive;
begin
  LoadSample;
  AssertNotNull(FProvider.GetIconNames('*.TXT'));
  AssertNotNull(FProvider.GetIconNames('*.Txt'));
end;

procedure TTestIconsMime.TestChooseBest;
var
  s: string;
begin
  LoadSample;
  // Only second icon available -> second wins
  FAvail.Available.Add('text-x-generic');
  s := FProvider.ChooseBestIcon('*.txt', @FAvail.IsAvailable, 'fallback');
  AssertEquals('text-x-generic', s);
  // None available -> fallback
  FAvail.Available.Clear;
  s := FProvider.ChooseBestIcon('*.txt', @FAvail.IsAvailable, 'fallback');
  AssertEquals('fallback', s);
  // First available -> first wins
  FAvail.Available.Add('text-plain');
  FAvail.Available.Add('text-x-generic');
  s := FProvider.ChooseBestIcon('*.txt', @FAvail.IsAvailable, 'fallback');
  AssertEquals('text-plain', s);
end;

procedure TTestIconsMime.TestMissingExt;
var
  s: string;
begin
  LoadSample;
  s := FProvider.ChooseBestIcon('*.nodata', @FAvail.IsAvailable, 'fallback');
  AssertEquals('fallback', s);
end;

initialization
  RegisterTest(TTestIconsMime);

end.
