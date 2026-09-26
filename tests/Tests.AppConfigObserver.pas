unit Tests.AppConfigObserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  AppConfig.Observer, AppConfig.Notifications;

type
  { Records every notification it receives. The test keeps an interface
    reference (FI*) alive so refcounting frees the instance on teardown. }
  TRecordingObserver = class(TInterfacedObject, IConfigObserver)
  private
    FNames: TStringList;
    FOwner: TConfigNotifier;
    FRemoveOnNotify: Boolean;
    FRaiseOnce: Boolean;
    FAddOnNotify: TRecordingObserver;
    FRemoveOther: TRecordingObserver;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConfigChanged(const PropertyName: UnicodeString = '');
    property Names: TStringList read FNames;
    property Owner: TConfigNotifier read FOwner write FOwner;
    property RemoveOnNotify: Boolean read FRemoveOnNotify write FRemoveOnNotify;
    property RaiseOnce: Boolean read FRaiseOnce write FRaiseOnce;
    property AddOnNotify: TRecordingObserver read FAddOnNotify write FAddOnNotify;
    property RemoveOtherOnNotify: TRecordingObserver read FRemoveOther write FRemoveOther;
  end;

  TTestConfigNotifier = class(TTestCase)
  private
    FNotifier: TConfigNotifier;
    FO1, FO2, FO3: TRecordingObserver;
    FI1, FI2, FI3: IConfigObserver;
    procedure AddBoundsGroup;
    function NamesOf(AObserver: TRecordingObserver): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Registration
    procedure TestObserverCount;
    procedure TestDuplicateObserverIgnored;
    procedure TestRemoveObserver;
    procedure TestRemoveUnknownObserverIsSafe;

    // Immediate notifications
    procedure TestNotifyReachesObserver;
    procedure TestNotifyReachesAllObservers;
    procedure TestEmptyNameNotifiedImmediately;
    procedure TestNotifyDoesNotChangeObserverCount;

    // Batching
    procedure TestBatchDefersUntilEndUpdate;
    procedure TestNestedBatchDefersUntilOuterEnd;
    procedure TestBatchDuplicatePropertyNotifiedOnce;
    procedure TestBatchEmptyNameIsNotBatched;
    procedure TestEndUpdateWithoutBeginIsNoop;
    procedure TestExtraEndUpdateIsNoop;

    // Groups
    procedure TestGroupCollapsesMembers;
    procedure TestGroupMembersNotNotifiedIndividually;
    procedure TestGroupWithUnrelatedPropertyKeepsBoth;
    procedure TestGroupOnlyFiresWhenAMemberChanged;

    // Reentrancy
    procedure TestRemoveObserverDuringNotificationKeepsDispatch;
    procedure TestRemoveOtherObserverDuringNotification;
    procedure TestAddObserverDuringNotificationAppliesToNextNotification;

    // Direct (single observer) notification
    procedure TestNotifyObserverReachesOnlyTarget;
    procedure TestNotifyObserverNilIsSafe;
    procedure TestNotifyObserverWithEmptyName;
    procedure TestNotifyWithoutObserversIsSafe;

    // Error handling
    procedure TestExceptionInCallbackDoesNotLeakBatch;
  end;

  TTestConfigNotificationGroup = class(TTestCase)
  published
    procedure TestHasMember;
    procedure TestMemberCountAndAccess;
    procedure TestAnyMemberBatched;
    procedure TestAnyMemberBatchedWithNilList;
  end;

implementation

{ TRecordingObserver }

constructor TRecordingObserver.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
end;

destructor TRecordingObserver.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

procedure TRecordingObserver.ConfigChanged(const PropertyName: UnicodeString);
begin
  if FRaiseOnce then
  begin
    FRaiseOnce := False;
    raise Exception.Create('observer failure');
  end;

  FNames.Add(PropertyName);

  if Assigned(FRemoveOther) and Assigned(FOwner) then
    FOwner.RemoveObserver(FRemoveOther);

  if FRemoveOnNotify and Assigned(FOwner) then
    FOwner.RemoveObserver(Self);

  if Assigned(FAddOnNotify) and Assigned(FOwner) then
    FOwner.AddObserver(FAddOnNotify);
end;

{ TTestConfigNotifier }

procedure TTestConfigNotifier.SetUp;
begin
  FNotifier := TConfigNotifier.Create;
  FO1 := TRecordingObserver.Create;
  FO2 := TRecordingObserver.Create;
  FO3 := TRecordingObserver.Create;
  FI1 := FO1;
  FI2 := FO2;
  FI3 := FO3;
  FO1.Owner := FNotifier;
  FO2.Owner := FNotifier;
  FO3.Owner := FNotifier;
end;

procedure TTestConfigNotifier.TearDown;
begin
  FNotifier.Free;
  FI1 := nil;
  FI2 := nil;
  FI3 := nil;
end;

procedure TTestConfigNotifier.AddBoundsGroup;
begin
  FNotifier.AddGroup('MainFormBounds', ['MainFormLeft', 'MainFormTop',
    'MainFormWidth', 'MainFormHeight']);
end;

function TTestConfigNotifier.NamesOf(AObserver: TRecordingObserver): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AObserver.Names.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + AObserver.Names[I];
  end;
end;

procedure TTestConfigNotifier.TestObserverCount;
begin
  AssertEquals(0, FNotifier.ObserverCount);
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO2);
  AssertEquals(2, FNotifier.ObserverCount);
end;

procedure TTestConfigNotifier.TestDuplicateObserverIgnored;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO1);
  AssertEquals(1, FNotifier.ObserverCount);
end;

procedure TTestConfigNotifier.TestRemoveObserver;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO2);
  FNotifier.RemoveObserver(FO1);
  AssertEquals(1, FNotifier.ObserverCount);
  FNotifier.Notify('X');
  AssertEquals('', NamesOf(FO1));
  AssertEquals('X', NamesOf(FO2));
end;

procedure TTestConfigNotifier.TestRemoveUnknownObserverIsSafe;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.RemoveObserver(FO2);
  AssertEquals(1, FNotifier.ObserverCount);
end;

procedure TTestConfigNotifier.TestNotifyReachesObserver;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.Notify('HoldSize');
  AssertEquals('HoldSize', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestNotifyReachesAllObservers;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO2);
  FNotifier.Notify('TrayIcon');
  AssertEquals('TrayIcon', NamesOf(FO1));
  AssertEquals('TrayIcon', NamesOf(FO2));
end;

procedure TTestConfigNotifier.TestEmptyNameNotifiedImmediately;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.Notify('');
  AssertEquals(1, FO1.Names.Count);
  AssertEquals('', FO1.Names[0]);
end;

procedure TTestConfigNotifier.TestNotifyDoesNotChangeObserverCount;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.Notify('X');
  AssertEquals(1, FNotifier.ObserverCount);
end;

procedure TTestConfigNotifier.TestBatchDefersUntilEndUpdate;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('A');
  AssertEquals('', NamesOf(FO1));
  FNotifier.EndUpdate;
  AssertEquals('A', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestNestedBatchDefersUntilOuterEnd;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('A');
  FNotifier.BeginUpdate;
  FNotifier.Notify('B');
  FNotifier.EndUpdate;
  AssertEquals('', NamesOf(FO1));
  FNotifier.EndUpdate;
  AssertEquals('A,B', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestBatchDuplicatePropertyNotifiedOnce;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('A');
  FNotifier.Notify('A');
  FNotifier.EndUpdate;
  AssertEquals('A', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestBatchEmptyNameIsNotBatched;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('');
  FNotifier.EndUpdate;
  AssertEquals('', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestEndUpdateWithoutBeginIsNoop;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.EndUpdate;
  AssertEquals('', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestExtraEndUpdateIsNoop;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('A');
  FNotifier.EndUpdate;
  FNotifier.EndUpdate;
  AssertEquals('A', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestGroupCollapsesMembers;
begin
  AddBoundsGroup;
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('MainFormLeft');
  FNotifier.Notify('MainFormTop');
  FNotifier.Notify('MainFormWidth');
  FNotifier.Notify('MainFormHeight');
  FNotifier.EndUpdate;
  AssertEquals('MainFormBounds', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestGroupMembersNotNotifiedIndividually;
begin
  AddBoundsGroup;
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('MainFormLeft');
  FNotifier.EndUpdate;
  AssertEquals('MainFormBounds', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestGroupWithUnrelatedPropertyKeepsBoth;
begin
  AddBoundsGroup;
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('MainFormWidth');
  FNotifier.Notify('TrayIcon');
  FNotifier.EndUpdate;
  AssertEquals('MainFormBounds,TrayIcon', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestGroupOnlyFiresWhenAMemberChanged;
begin
  AddBoundsGroup;
  FNotifier.AddObserver(FO1);
  FNotifier.BeginUpdate;
  FNotifier.Notify('TrayIcon');
  FNotifier.EndUpdate;
  AssertEquals('TrayIcon', NamesOf(FO1));
end;

procedure TTestConfigNotifier.TestRemoveObserverDuringNotificationKeepsDispatch;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO2);
  FO1.RemoveOnNotify := True;
  FNotifier.Notify('X');
  // The in-flight notification still reaches both observers.
  AssertEquals('X', NamesOf(FO1));
  AssertEquals('X', NamesOf(FO2));
  // FO1 removed itself, so the next notification only reaches FO2.
  FNotifier.Notify('Y');
  AssertEquals('X', NamesOf(FO1));
  AssertEquals('X,Y', NamesOf(FO2));
end;

procedure TTestConfigNotifier.TestAddObserverDuringNotificationAppliesToNextNotification;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO2);
  FO1.AddOnNotify := FO3;
  FNotifier.Notify('X');
  // FO3 was added while dispatching: it does not receive the in-flight event.
  AssertEquals('', NamesOf(FO3));
  FNotifier.Notify('Y');
  AssertEquals('Y', NamesOf(FO3));
end;

procedure TTestConfigNotifier.TestRemoveOtherObserverDuringNotification;
begin
  FNotifier.AddObserver(FO1);
  FNotifier.AddObserver(FO2);
  FO1.RemoveOtherOnNotify := FO2;
  FNotifier.Notify('X');
  // The in-flight notification still reaches the removed observer.
  AssertEquals('X', NamesOf(FO1));
  AssertEquals('X', NamesOf(FO2));
  FNotifier.Notify('Y');
  AssertEquals('X,Y', NamesOf(FO1));
  AssertEquals('X', NamesOf(FO2));
end;

procedure TTestConfigNotifier.TestNotifyObserverReachesOnlyTarget;
begin
  FNotifier.AddObserver(FO2);
  FNotifier.NotifyObserver(FO1, 'AfterUpdateConfig');
  AssertEquals('AfterUpdateConfig', NamesOf(FO1));
  AssertEquals('', NamesOf(FO2));
end;

procedure TTestConfigNotifier.TestNotifyObserverNilIsSafe;
begin
  // Must not raise.
  FNotifier.NotifyObserver(nil, 'X');
end;

procedure TTestConfigNotifier.TestNotifyObserverWithEmptyName;
begin
  FNotifier.NotifyObserver(FO1);
  AssertEquals(1, FO1.Names.Count);
  AssertEquals('', FO1.Names[0]);
end;

procedure TTestConfigNotifier.TestNotifyWithoutObserversIsSafe;
begin
  // Must not raise.
  FNotifier.Notify('X');
end;

procedure TTestConfigNotifier.TestExceptionInCallbackDoesNotLeakBatch;
begin
  FNotifier.AddObserver(FO1);
  FO1.RaiseOnce := True;

  FNotifier.BeginUpdate;
  FNotifier.Notify('A');
  try
    FNotifier.EndUpdate;
    Fail('The observer exception should propagate');
  except
    on E: Exception do
      ; // expected
  end;

  AssertEquals('nothing recorded before the failure', 0, FO1.Names.Count);

  // The stale 'A' must not be replayed by the next batch.
  FNotifier.BeginUpdate;
  FNotifier.Notify('B');
  FNotifier.EndUpdate;
  AssertEquals('B', NamesOf(FO1));
end;

{ TTestConfigNotificationGroup }

procedure TTestConfigNotificationGroup.TestHasMember;
var
  Group: TConfigNotificationGroup;
begin
  Group := TConfigNotificationGroup.Create('Grp', ['A', 'B']);
  try
    AssertTrue(Group.HasMember('A'));
    AssertTrue(Group.HasMember('B'));
    AssertFalse(Group.HasMember('C'));
  finally
    Group.Free;
  end;
end;

procedure TTestConfigNotificationGroup.TestMemberCountAndAccess;
var
  Group: TConfigNotificationGroup;
begin
  Group := TConfigNotificationGroup.Create('Grp', ['A', 'B']);
  try
    AssertEquals('Grp', Group.Name);
    AssertEquals(2, Group.MemberCount);
    AssertEquals('A', Group.Members[0]);
    AssertEquals('B', Group.Members[1]);
  finally
    Group.Free;
  end;
end;

procedure TTestConfigNotificationGroup.TestAnyMemberBatched;
var
  Group: TConfigNotificationGroup;
  Batched: TStringList;
begin
  Group := TConfigNotificationGroup.Create('Grp', ['A', 'B']);
  Batched := TStringList.Create;
  try
    AssertFalse(Group.AnyMemberBatched(Batched));

    Batched.Add('C');
    AssertFalse(Group.AnyMemberBatched(Batched));

    Batched.Add('B');
    AssertTrue(Group.AnyMemberBatched(Batched));
  finally
    Batched.Free;
    Group.Free;
  end;
end;

procedure TTestConfigNotificationGroup.TestAnyMemberBatchedWithNilList;
var
  Group: TConfigNotificationGroup;
begin
  Group := TConfigNotificationGroup.Create('Grp', ['A']);
  try
    // Must not raise.
    AssertFalse(Group.AnyMemberBatched(nil));
  finally
    Group.Free;
  end;
end;

initialization
  RegisterTest(TTestConfigNotifier);
  RegisterTest(TTestConfigNotificationGroup);

end.
