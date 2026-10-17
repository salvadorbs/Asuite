unit Tests.Instance;

{$mode objfpc}{$H+}

{ Behaviour tests for TASuiteInstance.HandleParam (Kernel.Instance):
  the command-line parameters read at startup. Only the "-list" branch is
  exercised: "-additem" depends on the DB manager singleton being alive,
  which is owned by the database fixture and would make the result
  order-dependent.

  The suite path list is the only piece of global state touched here, so
  it is snapshotted and restored around every test. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Kernel.Instance, AppConfig.Paths;

type
  TTestInstanceParams = class(TTestCase)
  private
    FOldList: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestListParamSetsListPath;
    procedure TestListParamStripsQuotes;
    procedure TestListParamKeepsEmbeddedEquals;
    procedure TestListParamIgnoredForSecondaryInstance;
    procedure TestEmptyListValueClearsPath;
    procedure TestUnknownParamIsIgnored;
    procedure TestParamWithoutPrefixIsIgnored;
    procedure TestLastListParamWins;
  end;

implementation

procedure TTestInstanceParams.SetUp;
begin
  inherited SetUp;
  FOldList := ASuiteInstance.Paths.SuitePathList;
end;

procedure TTestInstanceParams.TearDown;
begin
  ASuiteInstance.Paths.SuitePathList := FOldList;
  inherited TearDown;
end;

procedure TTestInstanceParams.TestListParamSetsListPath;
begin
  ASuiteInstance.HandleParam('-list=/tmp/asuite.sqlite');
  AssertEquals('list path set', '/tmp/asuite.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestListParamStripsQuotes;
begin
  ASuiteInstance.HandleParam('-list="/tmp/my list.sqlite"');
  AssertEquals('quotes stripped', '/tmp/my list.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestListParamKeepsEmbeddedEquals;
begin
  ASuiteInstance.HandleParam('-list=/tmp/a=b.sqlite');
  AssertEquals('value after first equals kept', '/tmp/a=b.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestListParamIgnoredForSecondaryInstance;
begin
  ASuiteInstance.Paths.SuitePathList := '/tmp/primary.sqlite';
  ASuiteInstance.HandleParam('-list=/tmp/secondary.sqlite', False);
  AssertEquals('secondary instance keeps list', '/tmp/primary.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestEmptyListValueClearsPath;
begin
  ASuiteInstance.Paths.SuitePathList := '/tmp/primary.sqlite';
  ASuiteInstance.HandleParam('-list=');
  AssertEquals('empty value clears list', '',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestUnknownParamIsIgnored;
begin
  ASuiteInstance.Paths.SuitePathList := '/tmp/primary.sqlite';
  ASuiteInstance.HandleParam('-unknown=/tmp/other.sqlite');
  AssertEquals('unknown param ignored', '/tmp/primary.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestParamWithoutPrefixIsIgnored;
begin
  ASuiteInstance.Paths.SuitePathList := '/tmp/primary.sqlite';
  ASuiteInstance.HandleParam('list=/tmp/other.sqlite');
  AssertEquals('prefix-less param ignored', '/tmp/primary.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

procedure TTestInstanceParams.TestLastListParamWins;
begin
  ASuiteInstance.HandleParam('-list=/tmp/first.sqlite');
  ASuiteInstance.HandleParam('-list=/tmp/second.sqlite');
  AssertEquals('last param wins', '/tmp/second.sqlite',
    UTF8Encode(ASuiteInstance.Paths.SuitePathList));
end;

initialization
  RegisterTest(TTestInstanceParams);

end.
