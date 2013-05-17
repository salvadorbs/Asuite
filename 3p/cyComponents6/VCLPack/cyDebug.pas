{   Component(s):
    tcyDebug

    Description:
    Component that help you debugging the code like :
    - identify where your code is spending more time.
    - Show occurences and statistics.
    - In/out code portions

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyDebug;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, SysUtils, Grids;

type
  TProcProcessEvent = procedure (Sender: TObject; Index: Integer) of object;

  RecProcess = record
    Name: ShortString;
    DurationMs: Cardinal;
    FirstDurationMs: Cardinal;
    LastDurationMs: Cardinal;
    MinDurationMs: Int64;
    MaxDurationMs: Cardinal;
    MarksCount: Integer;
    ArrayMarks: Array of Cardinal;
    EnterCount: Integer;
    ExitCount: Integer;
  end;

  TcyDebug = class(TComponent)
  private
    FProcessList: Array of RecProcess;
    FProcessCount: Integer;
    FNeedInitialization: Boolean;
    FOnEnterProcess: TProcProcessEvent;
    FOnExitProcess: TProcProcessEvent;
    FProcessGrid: TStringGrid;
    FActive: Boolean;
    function GetProcessName(Index: Integer): ShortString;
    function GetProcessDurationMs(Index: Integer): Cardinal;
    function GetProcessFirstDurationMs(Index: Integer): Cardinal;
    function GetProcessLastDurationMs(Index: Integer): Cardinal;
    function GetProcessEnterCount(Index: Integer): Integer;
    function GetProcessExitCount(Index: Integer): Integer;
    function GetProcessMaxDurationMs(Index: Integer): Cardinal;
    function GetProcessMinDurationMs(Index: Integer): Cardinal;
    procedure UpdateProcessGrid(Index: Integer);
    procedure SetProcessGrid(const Value: TStringGrid);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetProcessIndex(aProcessName: String): Integer;
    procedure ProcessEnter(aProcessName: String);
    procedure ProcessExit(aProcessName: String);
    procedure InitializeProcesses;
    property ProcessCount: Integer read FProcessCount;
    property ProcessName[Index: Integer]: ShortString read GetProcessName;
    property ProcessDurationMs[Index: Integer]: Cardinal read GetProcessDurationMs;
    property ProcessFirstDurationMs[Index: Integer]: Cardinal read GetProcessFirstDurationMs;
    property ProcessLastDurationMs[Index: Integer]: Cardinal read GetProcessLastDurationMs;
    property ProcessMinDurationMs[Index: Integer]: Cardinal read GetProcessMinDurationMs;
    property ProcessMaxDurationMs[Index: Integer]: Cardinal read GetProcessMaxDurationMs;
    property ProcessEnterCount[Index: Integer]: Integer read GetProcessEnterCount;
    property ProcessExitCount[Index: Integer]: Integer read GetProcessExitCount;
  published
    property Active: Boolean read FActive write FActive default true;
    property ProcessGrid: TStringGrid read FProcessGrid write SetProcessGrid;
    property OnEnterProcess: TProcProcessEvent read FOnEnterProcess write FOnEnterProcess;
    property OnExitProcess: TProcProcessEvent read FOnExitProcess write FOnExitProcess;
  end;

implementation

{ TcyDebug }
constructor TcyDebug.Create(AOwner: TComponent);
begin
  FActive := True;
  FNeedInitialization := True;
  inherited;
end;

destructor TcyDebug.Destroy;
begin
  SetLength(FProcessList, 0);
  inherited;
end;

procedure TcyDebug.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if FProcessGrid <> nil then
    if (Operation = opRemove) and (AComponent = FProcessGrid) then
      FProcessGrid := nil;
end;

procedure TcyDebug.SetProcessGrid(const Value: TStringGrid);
begin
  FProcessGrid := Value;

  if (Value <> nil) and (not  (csLoading in ComponentState)) then
    FProcessGrid.FreeNotification(Self);  // Inform TcyDebug if component removed ...
end;

procedure TcyDebug.InitializeProcesses;
var c, r: Integer;
begin
  FNeedInitialization := false;
  FProcessCount := 0;
  SetLength(FProcessList, FProcessCount);

  // Clear rows and prepare TStringGrid:
  if Assigned(FProcessGrid) then
  begin
    for c := 0 to FProcessGrid.ColCount-1 do
      for r := 1 to FProcessGrid.RowCount-1 do
        FProcessGrid.Cells[c, r] := '';

    // Prepare columns and headers :
    if FProcessGrid.ColCount < 9 then
      FProcessGrid.ColCount := 9;

    FProcessGrid.Cells[0, 0] := 'ProcessName';
    FProcessGrid.Cells[1, 0] := 'Total duration Ms';
    FProcessGrid.Cells[2, 0] := 'First Ms';
    FProcessGrid.Cells[3, 0] := 'Last Ms';
    FProcessGrid.Cells[4, 0] := 'Min Ms';
    FProcessGrid.Cells[5, 0] := 'Max Ms';
    FProcessGrid.Cells[6, 0] := 'Average Ms';
    FProcessGrid.Cells[7, 0] := 'In';
    FProcessGrid.Cells[8, 0] := 'Out';
  end;
end;

function TcyDebug.GetProcessFirstDurationMs(Index: Integer): Cardinal;
begin
  Result := FProcessList[Index].FirstDurationMs;
end;

function TcyDebug.GetProcessLastDurationMs(Index: Integer): Cardinal;
begin
  Result := FProcessList[Index].LastDurationMs;
end;

function TcyDebug.GetProcessDurationMs(Index: Integer): Cardinal;
begin
  Result := FProcessList[Index].DurationMs;
end;

function TcyDebug.GetProcessEnterCount(Index: Integer): Integer;
begin
  Result := FProcessList[Index].EnterCount;
end;

function TcyDebug.GetProcessExitCount(Index: Integer): Integer;
begin
  Result := FProcessList[Index].ExitCount;
end;

function TcyDebug.GetProcessIndex(aProcessName: String): Integer;
var i: Integer;
begin
  Result := -1;
  aProcessName := AnsiUpperCase(aProcessName);
  for i := 0 to FProcessCount-1 do
    if aProcessName = AnsiUpperCase(FProcessList[i].Name) then
    begin
      Result := i;
      Break;
    end;
end;

function TcyDebug.GetProcessMaxDurationMs(Index: Integer): Cardinal;
begin
  Result := FProcessList[Index].MaxDurationMs;
end;

function TcyDebug.GetProcessMinDurationMs(Index: Integer): Cardinal;
begin
  Result := FProcessList[Index].MinDurationMs;
end;

function TcyDebug.GetProcessName(Index: Integer): ShortString;
begin
  Result := FProcessList[Index].Name;
end;

procedure TcyDebug.ProcessEnter(aProcessName: String);
var ProcessIndex: Integer;
begin
  if not FActive then Exit;
  if FNeedInitialization then InitializeProcesses;

  ProcessIndex := GetProcessIndex(aProcessName);
  if ProcessIndex = -1 then
  begin
    ProcessIndex := FProcessCount;
    Inc(FProcessCount, 1);
    SetLength(FProcessList, FProcessCount);

    FProcessList[ProcessIndex].Name := aProcessName;
    FProcessList[ProcessIndex].DurationMs := 0;
    FProcessList[ProcessIndex].FirstDurationMs := 0;
    FProcessList[ProcessIndex].LastDurationMs := 0;
    FProcessList[ProcessIndex].MinDurationMs := -1;
    FProcessList[ProcessIndex].MaxDurationMs := 0;
    FProcessList[ProcessIndex].MarksCount := 0;
    Setlength(FProcessList[ProcessIndex].ArrayMarks, 0);
    FProcessList[ProcessIndex].EnterCount := 0;
    FProcessList[ProcessIndex].ExitCount := 0;
  end;

  Inc(FProcessList[ProcessIndex].EnterCount);

  // Add new time mark :
  Inc(FProcessList[ProcessIndex].MarksCount);
  SetLength(FProcessList[ProcessIndex].ArrayMarks, FProcessList[ProcessIndex].MarksCount);
  FProcessList[ProcessIndex].ArrayMarks[FProcessList[ProcessIndex].MarksCount-1] := GetTickCount;

  // Update grid information :
  if Assigned(FProcessGrid) then
    UpdateProcessGrid(ProcessIndex);

  if Assigned(FOnEnterProcess) then
    FOnEnterProcess(Self, ProcessIndex);
end;

procedure TcyDebug.ProcessExit(aProcessName: String);
var
  ProcessIndex: Integer;
  DurationMs: Cardinal;
begin
  if not FActive then Exit;
  if FNeedInitialization then InitializeProcesses;

  ProcessIndex := GetProcessIndex(aProcessName);
  if ProcessIndex <> -1 then
  begin
    Inc(FProcessList[ProcessIndex].ExitCount);
    // Register duration :
    DurationMs := GetTickCount - FProcessList[ProcessIndex].ArrayMarks[FProcessList[ProcessIndex].MarksCount-1];
    Inc(FProcessList[ProcessIndex].DurationMs, DurationMs);
    if FProcessList[ProcessIndex].MinDurationMs = -1 then
      FProcessList[ProcessIndex].FirstDurationMs := DurationMs;
    FProcessList[ProcessIndex].LastDurationMs := DurationMs;
    if (DurationMs < FProcessList[ProcessIndex].MinDurationMs) or (FProcessList[ProcessIndex].MinDurationMs = -1) then
      FProcessList[ProcessIndex].MinDurationMs := DurationMs;
    if DurationMs > FProcessList[ProcessIndex].MaxDurationMs then
      FProcessList[ProcessIndex].MaxDurationMs := DurationMs;
    // Free last time mark :
    Dec(FProcessList[ProcessIndex].MarksCount);
    SetLength(FProcessList[ProcessIndex].ArrayMarks, FProcessList[ProcessIndex].MarksCount);

    // Update grid information :
    if Assigned(FProcessGrid) then
      UpdateProcessGrid(ProcessIndex);

    if Assigned(FOnExitProcess) then
      FOnExitProcess(Self, ProcessIndex);
  end
  else
    raise Exception.Create('ProcessExit called with unknown ProcessName: ' + aProcessName + '!');
end;

procedure TcyDebug.UpdateProcessGrid(Index: Integer);
begin
  if FProcessGrid.RowCount <= Index + 1 then
    FProcessGrid.RowCount := Index + 2;

  FProcessGrid.Cells[0, Index + 1] := FProcessList[Index].Name;
  FProcessGrid.Cells[1, Index + 1] := intToStr(FProcessList[Index].DurationMs);
  FProcessGrid.Cells[2, Index + 1] := intToStr(FProcessList[Index].FirstDurationMs);
  FProcessGrid.Cells[3, Index + 1] := intToStr(FProcessList[Index].LastDurationMs);
  FProcessGrid.Cells[4, Index + 1] := intToStr(FProcessList[Index].MinDurationMs);
  FProcessGrid.Cells[5, Index + 1] := intToStr(FProcessList[Index].MaxDurationMs);
  if FProcessList[Index].EnterCount <> 0 then
    FProcessGrid.Cells[6, Index + 1] := intToStr(FProcessList[Index].DurationMs div FProcessList[Index].EnterCount);
  FProcessGrid.Cells[7, Index + 1] := intToStr(FProcessList[Index].EnterCount);
  FProcessGrid.Cells[8, Index + 1] := intToStr(FProcessList[Index].ExitCount);
end;

end.
