unit Tests.IconsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Icons.Manager, Icons.Files, Icons.ExtFile, Icons.Base;

type
  TAddThread = class(TThread)
  private
    FMgr: TIconsManager;
    FTag, FCount: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AMgr: TIconsManager; ATag, ACount: Integer);
  end;

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
    procedure TestConcurrentAddAndClear;
  end;

implementation

constructor TAddThread.Create(AMgr: TIconsManager; ATag, ACount: Integer);
begin
  inherited Create(True);
  FMgr := AMgr;
  FTag := ATag;
  FCount := ACount;
  FreeOnTerminate := False;
end;

procedure TAddThread.Execute;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    FMgr.AddIcon(TFileIcon.Create(Format('/tmp/ct%d_%d.pdf', [FTag, I])));
    FMgr.FindByName(Format('ct%d_%d', [FTag, I]));
  end;
end;

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

procedure TTestIconsManagerRegistry.TestConcurrentAddAndClear;
var
  Threads: array[0..3] of TAddThread;
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    Threads[I] := TAddThread.Create(FMgr, I, 25);
    Threads[I].Start;
  end;
  for I := 0 to 3 do
  begin
    Threads[I].WaitFor;
    Threads[I].Free;
  end;
  AssertNotNull(FMgr.FindByName('ct2_10'));
  AssertNotNull(FMgr.FindByPath('/tmp/ct3_5.pdf'));
  FMgr.Clear(False);
  AssertNull(FMgr.FindByName('ct0_0'));
  AssertNull(FMgr.FindByPath('/tmp/ct1_1.pdf'));
end;

initialization
  RegisterTest(TTestIconsManagerRegistry);

end.
