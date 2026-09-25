unit Tests.SortController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ActnList, VirtualTrees,
  Utility.SortController;

type
  TTestSortController = class(TTestCase)
  private
    FController: TSortController;
    FListTree: TVirtualStringTree;
    FActiveTree: TVirtualStringTree;
    FAction: TAction;
    procedure AddChildren(ATree: TVirtualStringTree; ACount: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestVisibleWhenListIsActiveTree;
    procedure TestNotVisibleWhenAnotherTreeIsActive;
    procedure TestEnabledWithMoreThanOneChild;
    procedure TestDisabledWithASingleChild;
    procedure TestDisabledWithNoChildren;
    procedure TestDisabledWhenListIsNotActiveTree;
    procedure TestNilActionIsSafe;
    procedure TestNilListTreeIsSafe;
  end;

implementation

procedure TTestSortController.SetUp;
begin
  inherited SetUp;
  FController := TSortController.Create;
  FListTree := TVirtualStringTree.Create(nil);
  FActiveTree := TVirtualStringTree.Create(nil);
  FAction := TAction.Create(nil);
  AddChildren(FListTree, 2);
end;

procedure TTestSortController.TearDown;
begin
  FAction.Free;
  FActiveTree.Free;
  FListTree.Free;
  FController.Free;
  inherited TearDown;
end;

procedure TTestSortController.AddChildren(ATree: TVirtualStringTree;
  ACount: Integer);
var
  I: Integer;
begin
  ATree.BeginUpdate;
  try
    for I := 1 to ACount do
      ATree.AddChild(nil);
  finally
    ATree.EndUpdate;
  end;
end;

procedure TTestSortController.TestVisibleWhenListIsActiveTree;
begin
  FController.UpdateSortListAction(FAction, FListTree, FListTree);
  AssertTrue('Visible', FAction.Visible);
end;

procedure TTestSortController.TestNotVisibleWhenAnotherTreeIsActive;
begin
  FController.UpdateSortListAction(FAction, FListTree, FActiveTree);
  AssertFalse('Not visible', FAction.Visible);
end;

procedure TTestSortController.TestEnabledWithMoreThanOneChild;
begin
  FController.UpdateSortListAction(FAction, FListTree, FListTree);
  AssertTrue('Enabled', FAction.Enabled);
end;

procedure TTestSortController.TestDisabledWithASingleChild;
var
  Single: TVirtualStringTree;
begin
  Single := TVirtualStringTree.Create(nil);
  try
    AddChildren(Single, 1);
    FController.UpdateSortListAction(FAction, Single, Single);
    AssertFalse('Disabled', FAction.Enabled);
  finally
    Single.Free;
  end;
end;

procedure TTestSortController.TestDisabledWithNoChildren;
var
  Empty: TVirtualStringTree;
begin
  Empty := TVirtualStringTree.Create(nil);
  try
    FController.UpdateSortListAction(FAction, Empty, Empty);
    AssertFalse('Disabled', FAction.Enabled);
  finally
    Empty.Free;
  end;
end;

procedure TTestSortController.TestDisabledWhenListIsNotActiveTree;
begin
  FController.UpdateSortListAction(FAction, FListTree, FActiveTree);
  AssertFalse('Disabled', FAction.Enabled);
end;

procedure TTestSortController.TestNilActionIsSafe;
begin
  // Must not raise.
  FController.UpdateSortListAction(nil, FListTree, FListTree);
end;

procedure TTestSortController.TestNilListTreeIsSafe;
begin
  // Must not raise.
  FController.UpdateSortListAction(FAction, nil, FListTree);
end;

initialization
  RegisterTest(TTestSortController);

end.
