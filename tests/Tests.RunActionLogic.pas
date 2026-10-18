unit Tests.RunActionLogic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, Kernel.Enumerations, Utility.RunActionLogic;

type
  TTestRunActionLogic = class(TTestCase)
  private
    function Item(ASeparator, AFile, AExec, AUrl: Boolean): TRunActionItem;
  published
    // Run
    procedure TestNormalEmptyIsDisabled;
    procedure TestNormalSeparatorOnlyIsDisabled;
    procedure TestNormalCategoryIsEnabled;
    procedure TestNormalFileIsEnabled;

    // Run as / Run as admin
    procedure TestAsUserCategoryIsEnabled;
    procedure TestAsUserExecutableIsEnabled;
    procedure TestAsUserNonExecutableIsDisabled;
    procedure TestAsUserSeparatorIsDisabled;
    procedure TestAsAdminExecutableIsEnabled;
    procedure TestAsAdminNonExecutableIsDisabled;

    // Open file location
    procedure TestExploreFileIsEnabled;
    procedure TestExploreUrlIsDisabled;
    procedure TestExploreCategoryIsDisabled;
    procedure TestExploreSeparatorIsDisabled;

    // Order independence
    procedure TestOrderDoesNotChangeResult;
    procedure TestOneQualifyingItemIsEnough;
  end;

implementation

function TTestRunActionLogic.Item(ASeparator, AFile, AExec,
  AUrl: Boolean): TRunActionItem;
begin
  Result.IsSeparator  := ASeparator;
  Result.IsFileItem   := AFile;
  Result.IsExecutable := AExec;
  Result.IsUrlProtocol := AUrl;
end;

procedure TTestRunActionLogic.TestNormalEmptyIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmNormal, []));
end;

procedure TTestRunActionLogic.TestNormalSeparatorOnlyIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmNormal, [Item(True, False, False, False)]));
end;

procedure TTestRunActionLogic.TestNormalCategoryIsEnabled;
begin
  AssertTrue(IsRunActionEnabled(rmNormal, [Item(False, False, False, False)]));
end;

procedure TTestRunActionLogic.TestNormalFileIsEnabled;
begin
  AssertTrue(IsRunActionEnabled(rmNormal, [Item(False, True, False, False)]));
end;

procedure TTestRunActionLogic.TestAsUserCategoryIsEnabled;
begin
  AssertTrue(IsRunActionEnabled(rmAsUser, [Item(False, False, False, False)]));
end;

procedure TTestRunActionLogic.TestAsUserExecutableIsEnabled;
begin
  AssertTrue(IsRunActionEnabled(rmAsUser, [Item(False, True, True, False)]));
end;

procedure TTestRunActionLogic.TestAsUserNonExecutableIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmAsUser, [Item(False, True, False, False)]));
end;

procedure TTestRunActionLogic.TestAsUserSeparatorIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmAsUser, [Item(True, False, False, False)]));
end;

procedure TTestRunActionLogic.TestAsAdminExecutableIsEnabled;
begin
  AssertTrue(IsRunActionEnabled(rmAsAdmin, [Item(False, True, True, False)]));
end;

procedure TTestRunActionLogic.TestAsAdminNonExecutableIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmAsAdmin, [Item(False, True, False, False)]));
end;

procedure TTestRunActionLogic.TestExploreFileIsEnabled;
begin
  AssertTrue(IsRunActionEnabled(rmExplorePath, [Item(False, True, True, False)]));
end;

procedure TTestRunActionLogic.TestExploreUrlIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmExplorePath, [Item(False, True, True, True)]));
end;

procedure TTestRunActionLogic.TestExploreCategoryIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmExplorePath, [Item(False, False, False, False)]));
end;

procedure TTestRunActionLogic.TestExploreSeparatorIsDisabled;
begin
  AssertFalse(IsRunActionEnabled(rmExplorePath, [Item(True, False, False, False)]));
end;

procedure TTestRunActionLogic.TestOrderDoesNotChangeResult;
var
  AB, BA: TRunActionItems;
begin
  AB := [Item(False, True, False, False), Item(False, False, False, False)];
  BA := [Item(False, False, False, False), Item(False, True, False, False)];
  AssertEquals(IsRunActionEnabled(rmAsUser, AB), IsRunActionEnabled(rmAsUser, BA));
end;

procedure TTestRunActionLogic.TestOneQualifyingItemIsEnough;
begin
  // A separator and a non-executable file first, an executable file last.
  AssertTrue(IsRunActionEnabled(rmAsUser,
    [Item(True, False, False, False), Item(False, True, False, False),
     Item(False, True, True, False)]));
end;

initialization
  RegisterTest(TTestRunActionLogic);

end.
