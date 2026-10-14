unit Tests.IconsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Icons.Manager, Icons.Files, Icons.ExtFile, Icons.Base;

type
  TTestIconsManagerRegistry = class(TTestCase)
  private
    FMgr: TIconsManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndFindByNameCaseInsensitive;
    procedure TestFindByPath;
    procedure TestClearStaticOnly;
    procedure TestExtNormalize;
    procedure TestExtSeparateLists;
    procedure TestMissingNameReturnsMinusOne;
  end;

implementation

procedure TTestIconsManagerRegistry.SetUp;
begin
  FMgr := TIconsManager.Create;
end;

procedure TTestIconsManagerRegistry.TearDown;
begin
  FMgr.Free;
end;

procedure TTestIconsManagerRegistry.TestAddAndFindByNameCaseInsensitive;
var
  Icon: TBaseIcon;
begin
  FMgr.AddIcon(TFileIcon.Create('/tmp/Alpha.ico', True));
  Icon := FMgr.FindByName('alpha');
  AssertNotNull(Icon);
  AssertNotNull(FMgr.FindByName('ALPHA'));
  AssertNull(FMgr.FindByName('nonexistent_xyz'));
end;

procedure TTestIconsManagerRegistry.TestFindByPath;
begin
  FMgr.AddIcon(TFileIcon.Create('/tmp/one.pdf'));
  FMgr.AddIcon(TFileIcon.Create('/tmp/two.pdf'));
  AssertNotNull(FMgr.FindByPath('/tmp/one.pdf'));
  AssertNotNull(FMgr.FindByPath('/tmp/two.pdf'));
  AssertNull(FMgr.FindByPath('/tmp/three.pdf'));
end;

procedure TTestIconsManagerRegistry.TestClearStaticOnly;
begin
  FMgr.AddIcon(TFileIcon.Create('/tmp/s1.ico', True));
  FMgr.AddIcon(TFileIcon.Create('/tmp/s2.ico', True));
  FMgr.AddIcon(TFileIcon.Create('/tmp/dyn.pdf', False));
  FMgr.Clear(True);
  AssertNull(FMgr.FindByName('s1'));
  AssertNull(FMgr.FindByName('s2'));
  AssertNotNull(FMgr.FindByName('dyn'));
end;

procedure TTestIconsManagerRegistry.TestExtNormalize;
var
  A, B, C: TExtFileIcon;
begin
  A := TExtFileIcon.Create('.TXT');
  try
    AssertEquals('*.txt', A.Name);
  finally
    A.Free;
  end;
  B := TExtFileIcon.Create('txt');
  try
    AssertEquals('*.txt', B.Name);
  finally
    B.Free;
  end;
  C := TExtFileIcon.Create('*.Txt');
  try
    AssertEquals('*.txt', C.Name);
  finally
    C.Free;
  end;

  FMgr.AddExtIcon(TExtFileIcon.Create('*.txt'));
  AssertNotNull(FMgr.FindByExt('.txt'));
  AssertNotNull(FMgr.FindByExt('TXT'));
  AssertNotNull(FMgr.FindByExt('*.TXT'));
  AssertNull(FMgr.FindByExt('*.nodata123'));
end;

procedure TTestIconsManagerRegistry.TestExtSeparateLists;
begin
  FMgr.AddIcon(TFileIcon.Create('/tmp/shared.pdf'));
  FMgr.AddExtIcon(TExtFileIcon.Create('*.pdf'));
  // Ext lookup finds ext icon, name lookup finds file icon
  AssertNotNull(FMgr.FindByExt('*.pdf'));
  AssertNotNull(FMgr.FindByName('shared'));
end;

procedure TTestIconsManagerRegistry.TestMissingNameReturnsMinusOne;
begin
  AssertEquals(-1, FMgr.GetIconIndex('nonexistent_xyz_123'));
end;

initialization
  RegisterTest(TTestIconsManagerRegistry);

end.
