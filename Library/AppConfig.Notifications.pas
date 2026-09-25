{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{ Notification/batching machinery for TConfiguration.

  This unit has no dependency on LCL and can be unit tested standalone.
  A group collapses the notification of several properties into a single
  notification (e.g. the four MainForm bounds properties are notified as
  'MainFormBounds'). }
unit AppConfig.Notifications;

{$MODE DelphiUnicode}

interface

uses
  Classes, Contnrs, AppConfig.Observer;

type
  { A named set of properties notified together. }
  TConfigNotificationGroup = class
  private
    FName: string;
    FMembers: TStringList;
    function GetMemberCount: Integer;
    function GetMember(AIndex: Integer): string;
  public
    constructor Create(const AName: string; const AMembers: array of string);
    destructor Destroy; override;
    function HasMember(const AProperty: string): Boolean;
    function AnyMemberBatched(const ABatched: TStrings): Boolean;
    property Name: string read FName;
    property MemberCount: Integer read GetMemberCount;
    property Members[AIndex: Integer]: string read GetMember;
  end;

  { Keeps the list of configuration observers and dispatches notifications,
    optionally batching them between BeginUpdate/EndUpdate. }
  TConfigNotifier = class
  private
    FObservers: TInterfaceList;
    FBatchCount: Integer;
    FBatchedProperties: TStringList;
    FGroups: TObjectList;
    function GetObserverCount: Integer;
    procedure DoNotify(const PropertyName: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObserver(const Observer: IConfigObserver);
    procedure RemoveObserver(const Observer: IConfigObserver);
    procedure AddGroup(const AName: string; const AMembers: array of string);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Notify(const PropertyName: string = '');

    property ObserverCount: Integer read GetObserverCount;
  end;

implementation

uses
  SysUtils;

{ TConfigNotificationGroup }

constructor TConfigNotificationGroup.Create(const AName: string;
  const AMembers: array of string);
var
  I: Integer;
begin
  FName := AName;
  FMembers := TStringList.Create;
  for I := Low(AMembers) to High(AMembers) do
    FMembers.Add(AMembers[I]);
end;

destructor TConfigNotificationGroup.Destroy;
begin
  FMembers.Free;
  inherited Destroy;
end;

function TConfigNotificationGroup.GetMemberCount: Integer;
begin
  Result := FMembers.Count;
end;

function TConfigNotificationGroup.GetMember(AIndex: Integer): string;
begin
  Result := FMembers[AIndex];
end;

function TConfigNotificationGroup.HasMember(const AProperty: string): Boolean;
begin
  Result := FMembers.IndexOf(AProperty) >= 0;
end;

function TConfigNotificationGroup.AnyMemberBatched(const ABatched: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FMembers.Count - 1 do
  begin
    if ABatched.IndexOf(FMembers[I]) >= 0 then
      Exit(True);
  end;
end;

{ TConfigNotifier }

constructor TConfigNotifier.Create;
begin
  inherited Create;
  FObservers := TInterfaceList.Create;
  FBatchCount := 0;
  FBatchedProperties := TStringList.Create;
  FGroups := TObjectList.Create(True);
end;

destructor TConfigNotifier.Destroy;
begin
  FGroups.Free;
  FBatchedProperties.Free;
  FObservers.Free;
  inherited Destroy;
end;

function TConfigNotifier.GetObserverCount: Integer;
begin
  Result := FObservers.Count;
end;

procedure TConfigNotifier.AddObserver(const Observer: IConfigObserver);
begin
  if FObservers.IndexOf(Observer) = -1 then
    FObservers.Add(Observer);
end;

procedure TConfigNotifier.RemoveObserver(const Observer: IConfigObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TConfigNotifier.AddGroup(const AName: string;
  const AMembers: array of string);
begin
  FGroups.Add(TConfigNotificationGroup.Create(AName, AMembers));
end;

procedure TConfigNotifier.BeginUpdate;
begin
  // While the counter is > 0 notifications are recorded, not dispatched.
  Inc(FBatchCount);
end;

procedure TConfigNotifier.EndUpdate;
var
  Excluded: TStringList;
  I, J: Integer;
  Group: TConfigNotificationGroup;
begin
  if FBatchCount = 0 then
    Exit;

  Dec(FBatchCount);
  if FBatchCount > 0 then
    Exit;

  Excluded := TStringList.Create;
  try
    // Collapse grouped properties into a single notification.
    for I := 0 to FGroups.Count - 1 do
    begin
      Group := TConfigNotificationGroup(FGroups[I]);
      if Group.AnyMemberBatched(FBatchedProperties) then
      begin
        for J := 0 to Group.MemberCount - 1 do
          Excluded.Add(Group.Members[J]);
        DoNotify(Group.Name);
      end;
    end;

    // Notify the remaining properties individually, once each.
    for I := 0 to FBatchedProperties.Count - 1 do
      if Excluded.IndexOf(FBatchedProperties[I]) = -1 then
        DoNotify(FBatchedProperties[I]);
  finally
    // Always drop the recorded properties: if an observer raised, a later
    // EndUpdate must not replay the stale batch.
    FBatchedProperties.Clear;
    Excluded.Free;
  end;
end;

procedure TConfigNotifier.Notify(const PropertyName: string);
begin
  if FBatchCount > 0 then
  begin
    // An empty name is not a property: it is not worth replaying later.
    if (PropertyName <> '') and (FBatchedProperties.IndexOf(PropertyName) = -1) then
      FBatchedProperties.Add(PropertyName);
    Exit;
  end;

  DoNotify(PropertyName);
end;

procedure TConfigNotifier.DoNotify(const PropertyName: string);
var
  I: Integer;
  Snapshot: array of IConfigObserver;
begin
  // Take a snapshot so an observer can add/remove observers (including itself)
  // from within its callback without disturbing the dispatch loop.
  SetLength(Snapshot, FObservers.Count);
  for I := 0 to FObservers.Count - 1 do
    Supports(FObservers[I], IConfigObserver, Snapshot[I]);

  for I := 0 to High(Snapshot) do
    if Assigned(Snapshot[I]) then
      Snapshot[I].ConfigChanged(PropertyName);
end;

end.
