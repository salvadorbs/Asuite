{   Component(s):
    TcyAppInstances

    Description:
    Avoid application (or application group) multiple instances

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

unit cyAppInstances;

interface

uses Classes, Windows, Forms, Controls, cyBaseComm, SysUtils;

type
  TcyInstanceInfo = Packed Record
    BaseComHandle: THandle;
    OwnerWindowHandle: THandle;
    ApplicationHandle: THandle;
    Instances: Integer;
    MaxInstances: Word;
  end;

  TProcOnReceiveCmdLine = procedure (Sender: TObject; fromInstance: TcyInstanceInfo; Parameters: TStrings) of object;
  TProcOnServerExists = procedure (Sender: TObject; var DoExitProcess, DoSendCmdLine, ShowServer: Boolean) of object;

  TcyAppInstances = class(TcyBaseComm)
  private
    // file mapping handling variables:
    FileMapHandle: THandle;
    FileMapData: Pointer;
    //
    FInstanceID: String;
    FActive: Boolean;
    FOnReceiveCmdLine: TProcOnReceiveCmdLine;
    FSendCmdLine: Boolean;
    FAutoExitProcess: Boolean;
    FOnServerExists: TProcOnServerExists;
    FOnReceiveString: TProcOnReceiveString;
    FOnReceiveStream: TProcOnReceiveMemoryStream;
    FMaxInstances: Word;
    procedure SetInstanceID(Value: String);
    function GetServerInfo: TcyInstanceInfo;
    procedure SetActive(const Value: Boolean);
    procedure CloseFileMap;
    procedure CreateFileMap;
    function GetInstances: Integer;
    function GetIsServer: Boolean;
    procedure SetMaxInstances(const Value: Word);
  protected
    procedure DoReceiveDevelopperStream(From: THandle; aStream: TMemoryStream; Param: Integer); override;
    procedure DoReceiveString(From: THandle; aString: String; Param: Integer); override;
    procedure DoReceiveUserStream(From: THandle; aStream: TMemoryStream; Param: Integer); override;
    procedure Loaded; override;
    procedure ActiveChanged;
    function SendParams: Boolean;
    procedure RegisterInstance;
    procedure UnregisterInstance;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RegisterAsServer: Boolean;
    procedure ShowWindowServer;
    property ServerInfo: TcyInstanceInfo read GetServerInfo;
  published
    property Active: Boolean read FActive write SetActive default true;
    property AutoExitProcess: Boolean read FAutoExitProcess write FAutoExitProcess default true;
    property InstanceID: String read FInstanceID write SetInstanceID;
    property Instances: Integer read GetInstances;
    property IsServer: Boolean read GetIsServer;
    property MaxInstances: Word read FMaxInstances write SetMaxInstances default 1;
    property SendCmdLine: Boolean read FSendCmdLine write FSendCmdLine default true;
    property OnReceiveCmdLine: TProcOnReceiveCmdLine read FOnReceiveCmdLine write FOnReceiveCmdLine;
    property OnReceiveString: TProcOnReceiveString read FOnReceiveString write FOnReceiveString;
    property OnReceiveStream: TProcOnReceiveMemoryStream read FOnReceiveStream write FOnReceiveStream;
    property OnServerExists: TProcOnServerExists read FOnServerExists write FOnServerExists;
  end;

const
  CmdLineParam = 0;
  sPrefixInstanceID = 'CYAPPINST';
  seNotActive = 'Error: not Active!';

implementation

{ TcyAppInstances }
constructor TcyAppInstances.Create(AOwner: TComponent);
begin
  inherited;
  FActive := true;
  FAutoExitProcess := true;
  FInstanceID := 'Unique name please';
  FMaxInstances := 1;
  FSendCmdLine := true;
end;

procedure TcyAppInstances.Loaded;
begin
  inherited;
  ActiveChanged;

end;

destructor TcyAppInstances.Destroy;
begin
  if Factive then
    CloseFileMap;
  inherited;
end;

procedure TcyAppInstances.ActiveChanged;

        function ServerExists: Boolean;
        begin
          RESULT := false;

          if ServerInfo.BaseComHandle <> 0 then
            RESULT := IsWindow(ServerInfo.ApplicationHandle);
        end;

var InstanceNotAllowed, _ShowServer, _ExitProcess, _SendCmdLine: Boolean;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  if FActive
  then begin
    CreateFileMap;

    if ServerExists
    then begin
      InstanceNotAllowed := ServerInfo.Instances > ServerInfo.MaxInstances;
      _ExitProcess := FAutoExitProcess and InstanceNotAllowed;
      _ShowServer  := _ExitProcess;
      _SendCmdLine := FSendCmdLine and InstanceNotAllowed;

      if Assigned(FOnServerExists)
      then FOnServerExists(Self, _ExitProcess, _SendCmdLine, _ShowServer);

      if _ShowServer then
        ShowWindowServer;

      if _SendCmdLine then
        SendParams;

      if _ExitProcess
      then begin
        CloseFileMap;
        ExitProcess(0);  // Application.Terminate does not close application immediatly ...
      end;
    end
    else
      RegisterAsServer;
  end
  else
    CloseFileMap;
end;

procedure TcyAppInstances.DoReceiveDevelopperStream(From: THandle; aStream: TMemoryStream; Param: Integer);
var Parameters: TStrings;
begin
//  inherited;
  if Param = CmdLineParam
  then
    if Assigned(FOnReceiveCmdLine)
    then
      try
        Parameters := TStringList.Create;
        aStream.Position := 0;
        Parameters.LoadFromStream(aStream);
        FOnReceiveCmdLine(Self, ServerInfo, Parameters);
      finally
        Parameters.Free;
      end;
end;

procedure TcyAppInstances.DoReceiveString(From: THandle; aString: String; Param: Integer);
begin
//  inherited;
  if Assigned(FOnReceiveString) then
    FOnReceiveString(Self, From, aString, Param);
end;

procedure TcyAppInstances.DoReceiveUserStream(From: THandle; aStream: TMemoryStream; Param: Integer);
begin
//  inherited;
  if Assigned(FOnReceiveStream) then
    FOnReceiveStream(Self, From, aStream, Param);
end;

procedure TcyAppInstances.CreateFileMap;
begin
  FileMapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, pchar(sPrefixInstanceID + FInstanceID));
  if FileMapHandle = 0 then  // Not created yet, must use CreateFileMapping ...
    FileMapHandle := CreateFileMapping($FFFFFFFF, Nil, PAGE_READWRITE, 0, SizeOf(TcyInstanceInfo), pchar(sPrefixInstanceID + FInstanceID));

  if FileMapHandle = 0 then Exit;
  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  // Initialize FileMapData if not already opened :
  // Not needed!

  // Register new instance :
  if Assigned(FileMapData) then
    RegisterInstance;
end;

procedure TcyAppInstances.CloseFileMap;
begin
  if FileMapHandle = 0 then
    Exit;
  if Assigned(FileMapData)
  then begin
    UnregisterInstance;
    UnmapViewOfFile(FileMapData);
  end;

  CloseHandle(FileMapHandle);
  FileMapHandle := 0;
end;

procedure TcyAppInstances.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;
  FActive := Value;
  ActiveChanged;
end;

procedure TcyAppInstances.SetInstanceID(Value: String);
begin
  Value := ValidateFileMappingName(Value);
  if FInstanceID = Value then
    Exit;
  FInstanceID := Value;

  if (FActive) and (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState))
  then begin
    // Close previous FileMapping:
    Active := false;

    // Open FileMapping with new name:
    Active := true;
  end;
end;

procedure TcyAppInstances.SetMaxInstances(const Value: Word);
var Info: TcyInstanceInfo;
begin
  if FMaxInstances = Value then Exit;
  FMaxInstances := Value;

  if IsServer
  then begin
    Info := GetServerInfo;
    Info.MaxInstances := FMaxInstances;
    Move(Info, FileMapData^, SizeOf(TcyInstanceInfo));
  end;
end;

function TcyAppInstances.GetServerInfo: TcyInstanceInfo;

    procedure InitializeServerInfo;
    begin
      RESULT.ApplicationHandle := 0;
      RESULT.OwnerWindowHandle := 0;
      RESULT.BaseComHandle := 0;
      RESULT.Instances := 0;
      RESULT.MaxInstances := 0;
    end;

begin
  InitializeServerInfo;

  if FileMapHandle <> 0 then
    if Assigned(FileMapData) then
      try
        Move(FileMapData^, RESULT, SizeOf(TcyInstanceInfo));
      except
        InitializeServerInfo;
      end;
end;

function TcyAppInstances.GetInstances: Integer;
begin
  RESULT := 0;

  if FActive
  then RESULT := ServerInfo.Instances;
end;

function TcyAppInstances.GetIsServer: Boolean;
begin
  RESULT := false;

  if FActive
  then RESULT := Handle = ServerInfo.BaseComHandle;
end;

function TcyAppInstances.RegisterAsServer: Boolean;
var Info: TcyInstanceInfo;
begin
  RESULT := false;
  if not FActive then
    raise Exception.Create(seNotActive);
  if FileMapHandle = 0 then
    Exit;

  if Assigned(FileMapData) then
    try
      Info := GetServerInfo;      // We need to keep Instances ...
      Info.BaseComHandle := Self.Handle;
      Info.ApplicationHandle := Application.Handle;
      Info.OwnerWindowHandle := 0;
      if Owner <> Nil then
        if Owner is TWinControl then
          Info.OwnerWindowHandle := TWinControl(Owner).Handle;
      Info.MaxInstances := FMaxInstances;
      Move(Info, FileMapData^, SizeOf(TcyInstanceInfo));
      RESULT := true;
    except

    end;
end;

procedure TcyAppInstances.RegisterInstance;
var Info: TcyInstanceInfo;
begin
  Info := GetServerInfo;
  Info.Instances := Info.Instances + 1;
  Move(Info, FileMapData^, SizeOf(TcyInstanceInfo));
end;

procedure TcyAppInstances.UnregisterInstance;
var Info: TcyInstanceInfo;
begin
  Info := GetServerInfo;
  Info.Instances := Info.Instances - 1;
  Move(Info, FileMapData^, SizeOf(TcyInstanceInfo));
end;

procedure TcyAppInstances.ShowWindowServer;

        procedure _ShowWindow(Hnd: Integer);
        begin
          if IsIconic(Hnd)
          then ShowWindow(Hnd, Sw_Restore);

          SetForegroundWindow(Hnd);
          BringWindowToTop(Hnd);
        end;

var aHandle: THandle;
begin
  aHandle := ServerInfo.ApplicationHandle;
  if aHandle <> 0 then
    _ShowWindow(aHandle);

  aHandle := ServerInfo.OwnerWindowHandle;
  if aHandle <> 0 then
    _ShowWindow(aHandle);
end;

function TcyAppInstances.SendParams: Boolean;
var
  Str: String;
  i: Integer;
begin
  Str := '';
  for i:= 0 to ParamCount do
    if Str = ''
    then Str := ParamStr(i)
    else Str := Str + #$D#$A + ParamStr(i);

  RESULT := DevelopperSendStream(ServerInfo.BaseComHandle, Str, CmdLineParam);
end;

end.
