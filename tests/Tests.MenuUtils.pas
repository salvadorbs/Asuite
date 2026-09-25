unit Tests.MenuUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Menus, ActnList,
  Utility.MenuUtils;

type
  TTestMenuUtils = class(TTestCase)
  private
    FTarget: TMenuItem;
    FSource: TMenuItem;
    FActionA: TAction;
    FActionB: TAction;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopiesItemsInOrder;
    procedure TestItemWithoutActionBecomesSeparator;
    procedure TestItemWithActionKeepsAction;
    procedure TestClearsExistingTarget;
    procedure TestNilTargetIsSafe;
    procedure TestNilSourceIsSafe;
    procedure TestEmptySourceClearsTarget;
  end;

implementation

procedure TTestMenuUtils.SetUp;
begin
  inherited SetUp;
  FTarget := TMenuItem.Create(nil);
  FSource := TMenuItem.Create(nil);
  FActionA := TAction.Create(nil);
  FActionA.Caption := 'A';
  FActionB := TAction.Create(nil);
  FActionB.Caption := 'B';
end;

procedure TTestMenuUtils.TearDown;
begin
  FActionA.Free;
  FActionB.Free;
  FSource.Free;
  FTarget.Free;
  inherited TearDown;
end;

procedure TTestMenuUtils.TestCopiesItemsInOrder;
var
  ItemA, ItemB, ItemC: TMenuItem;
begin
  ItemA := TMenuItem.Create(FSource);
  ItemA.Action := FActionA;
  ItemB := TMenuItem.Create(FSource);
  ItemB.Caption := 'plain';
  ItemC := TMenuItem.Create(FSource);
  ItemC.Action := FActionB;
  FSource.Add(ItemA);
  FSource.Add(ItemB);
  FSource.Add(ItemC);

  PopulatePopUpMenuFromAnother(FTarget, FSource);

  AssertEquals(3, FTarget.Count);
  AssertSame(FActionA, FTarget.Items[0].Action);
  AssertSame(FActionB, FTarget.Items[2].Action);
end;

procedure TTestMenuUtils.TestItemWithoutActionBecomesSeparator;
var
  Item: TMenuItem;
begin
  Item := TMenuItem.Create(FSource);
  Item.Caption := 'ignored';
  FSource.Add(Item);

  PopulatePopUpMenuFromAnother(FTarget, FSource);

  AssertEquals(1, FTarget.Count);
  AssertEquals('-', FTarget.Items[0].Caption);
  AssertNull(FTarget.Items[0].Action);
end;

procedure TTestMenuUtils.TestItemWithActionKeepsAction;
var
  Item: TMenuItem;
begin
  Item := TMenuItem.Create(FSource);
  Item.Action := FActionA;
  Item.Caption := 'should be overridden by action';
  FSource.Add(Item);

  PopulatePopUpMenuFromAnother(FTarget, FSource);

  AssertSame(FActionA, FTarget.Items[0].Action);
end;

procedure TTestMenuUtils.TestClearsExistingTarget;
var
  Existing: TMenuItem;
  NewItem: TMenuItem;
begin
  Existing := TMenuItem.Create(FTarget);
  Existing.Caption := 'old';
  FTarget.Add(Existing);

  NewItem := TMenuItem.Create(FSource);
  NewItem.Action := FActionA;
  FSource.Add(NewItem);

  PopulatePopUpMenuFromAnother(FTarget, FSource);

  AssertEquals(1, FTarget.Count);
  AssertSame(FActionA, FTarget.Items[0].Action);
end;

procedure TTestMenuUtils.TestNilTargetIsSafe;
begin
  // Must not raise.
  PopulatePopUpMenuFromAnother(nil, FSource);
end;

procedure TTestMenuUtils.TestNilSourceIsSafe;
begin
  PopulatePopUpMenuFromAnother(FTarget, nil);
  AssertEquals(0, FTarget.Count);
end;

procedure TTestMenuUtils.TestEmptySourceClearsTarget;
var
  Existing: TMenuItem;
begin
  Existing := TMenuItem.Create(FTarget);
  Existing.Caption := 'old';
  FTarget.Add(Existing);

  PopulatePopUpMenuFromAnother(FTarget, FSource);

  AssertEquals(0, FTarget.Count);
end;

initialization
  RegisterTest(TTestMenuUtils);

end.
