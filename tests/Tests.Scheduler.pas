unit Tests.Scheduler;

{$mode objfpc}{$H+}

{ GUI-suite tests for the pure firing logic in TScheduler (CheckOnce,
  CheckDaily, CheckHourly): everything else in the scheduler walks live
  trees and timers and is excluded. Fixed datetimes keep the tests
  deterministic. }

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  Kernel.Scheduler;

type
  TTestScheduler = class(TTestCase)
  private
    FScheduler: TScheduler;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHourlyFiresOnTheHour;
    procedure TestHourlySkipsOtherMinutes;
    procedure TestHourlySkipsNonZeroSeconds;
    procedure TestDailyFiresAtSameTime;
    procedure TestDailySkipsOtherHours;
    procedure TestDailySkipsNonZeroSeconds;
    procedure TestOnceFiresSameDateTime;
    procedure TestOnceSkipsOtherDates;
    procedure TestOnceSkipsOtherTimes;
  end;

implementation

procedure TTestScheduler.SetUp;
begin
  inherited SetUp;
  FScheduler := TScheduler.Create;
end;

procedure TTestScheduler.TearDown;
begin
  FScheduler.Free;
  inherited TearDown;
end;

procedure TTestScheduler.TestHourlyFiresOnTheHour;
begin
  AssertTrue('Fires', FScheduler.CheckHourly(EncodeDateTime(2026, 10, 14, 9, 0, 0, 0)));
end;

procedure TTestScheduler.TestHourlySkipsOtherMinutes;
begin
  AssertFalse('Skips', FScheduler.CheckHourly(EncodeDateTime(2026, 10, 14, 9, 30, 0, 0)));
end;

procedure TTestScheduler.TestHourlySkipsNonZeroSeconds;
begin
  AssertFalse('Skips', FScheduler.CheckHourly(EncodeDateTime(2026, 10, 14, 9, 0, 5, 0)));
end;

procedure TTestScheduler.TestDailyFiresAtSameTime;
begin
  AssertTrue('Fires', FScheduler.CheckDaily(
    EncodeDateTime(2026, 10, 14, 9, 30, 0, 0),
    EncodeDateTime(2020, 1, 1, 9, 30, 0, 0)));
end;

procedure TTestScheduler.TestDailySkipsOtherHours;
begin
  AssertFalse('Skips', FScheduler.CheckDaily(
    EncodeDateTime(2026, 10, 14, 10, 30, 0, 0),
    EncodeDateTime(2020, 1, 1, 9, 30, 0, 0)));
end;

procedure TTestScheduler.TestDailySkipsNonZeroSeconds;
begin
  AssertFalse('Skips', FScheduler.CheckDaily(
    EncodeDateTime(2026, 10, 14, 9, 30, 7, 0),
    EncodeDateTime(2020, 1, 1, 9, 30, 0, 0)));
end;

procedure TTestScheduler.TestOnceFiresSameDateTime;
begin
  AssertTrue('Fires', FScheduler.CheckOnce(
    EncodeDateTime(2026, 10, 14, 9, 30, 0, 0),
    EncodeDateTime(2026, 10, 14, 9, 30, 0, 0)));
end;

procedure TTestScheduler.TestOnceSkipsOtherDates;
begin
  AssertFalse('Skips', FScheduler.CheckOnce(
    EncodeDateTime(2026, 10, 15, 9, 30, 0, 0),
    EncodeDateTime(2026, 10, 14, 9, 30, 0, 0)));
end;

procedure TTestScheduler.TestOnceSkipsOtherTimes;
begin
  AssertFalse('Skips', FScheduler.CheckOnce(
    EncodeDateTime(2026, 10, 14, 9, 31, 0, 0),
    EncodeDateTime(2026, 10, 14, 9, 30, 0, 0)));
end;

initialization
  RegisterTest(TTestScheduler);

end.
