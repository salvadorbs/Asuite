{   Component(s):
    TcyBaseComm

    Description:
    Base communication component to communicate between TcyBaseComm components from same or different applications

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

unit cyBaseComm;

interface

uses Classes, Windows, Controls, Messages, SysUtils;

type
  TCommandType = (ctDevelopperDefined, ctUserDefined);
  TStreamContentType = (scDevelopperDefined, scUserDefined, scString);

  TProcOnReceiveCommand = procedure (Sender: TObject; aCommand: Word; userParam: Integer) of object;
  TProcOnReceiveString = procedure (Sender: TObject; fromBaseCommHandle: THandle; aString: String; userParam: Integer) of object;
  TProcOnReceiveMemoryStream = procedure (Sender: TObject; fromBaseCommHandle: THandle; aStream: TMemoryStream; userParam: Integer) of object;

  TcyBaseComm = class(TComponent)
  private
    // Hidden window handle in order to receive message:
    FHWnd: HWND;
    // Variables used for sending TWMCopyData message (Can' t be local variables):
    SendCopyDataStruct: TCopyDataStruct;
    SendCopyDataStructAddress: Integer;
    procedure HandleIncomingCommand(var Msg: TMessage);
    procedure HandleIncomingStream(var Msg: TWMCopyData; Param: Integer);
    function InternalSendCommand(ToBaseCommHandle: THandle; CommandType: TCommandType; aCommand: Word; Param: Integer): Boolean;
    function InternalSendStream(ToBaseCommHandle: THandle; aMemoryStream: TMemoryStream; StreamContentType: TStreamContentType; Param: Integer): Boolean;
  protected
    function DevelopperSendCommand(ToBaseCommHandle: THandle; aCommand: Word; Param: Integer): Boolean;
    function DevelopperSendStream(ToBaseCommHandle: THandle; aMemoryStream: TMemoryStream; Param: Integer): Boolean; overload;
    function DevelopperSendStream(ToBaseCommHandle: THandle; aString: String; Param: Integer): Boolean; overload;  // To facilitate developper send string
    procedure DoReceiveDevelopperCommand(aCommand: Word; aParam: Integer); virtual;
    procedure DoReceiveCommand(aCommand: Word; aParam: Integer); virtual;
    procedure DoReceiveDevelopperStream(From: THandle; aStream: TMemoryStream; Param: Integer); virtual;
    procedure DoReceiveString(From: THandle; aString: String; Param: Integer); virtual;
    procedure DoReceiveUserStream(From: THandle; aStream: TMemoryStream; Param: Integer); virtual;
    procedure WndMethod(var Msg: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SendCommand(ToBaseCommHandle: THandle; aCommand: Word; UserParam: Integer): Boolean;
    function SendStream(ToBaseCommHandle: THandle; aMemoryStream: TMemoryStream; UserParam: Integer): Boolean;
    function SendString(ToBaseCommHandle: THandle; aString: String; UserParam: Integer): Boolean;
    function MemoryStreamToString(Stream: TMemoryStream): String;
    property Handle: HWND read FHWnd;
  end;

const
  MsgCommand = WM_USER + 1;      // Sending and receiving commands ...
  MsgResultOk = 99;              // Arbitrary value, only can' t be 0!

  function ValidateFileMappingName(aName: String): String;

implementation

function ValidateFileMappingName(aName: String): String;

    function ValidateCar(aCar: Char): Boolean;
    begin
      RESULT := false;

      if aCar in ['a'..'z', 'A'..'Z', '0'..'9', ' ']
      then RESULT := true;
    end;

var l: Integer;
begin
  RESULT := '';

  for l := 1 to Length(aName) do
    if ValidateCar(aName[l])
    then RESULT := RESULT + aName[l];
end;

{ TcyBaseComm }
constructor TcyBaseComm.Create(AOwner: TComponent);
begin
  inherited;
  // Create a window to process windows messages with WndMethod() :
  FHWnd := AllocateHWnd(WndMethod);
end;

destructor TcyBaseComm.Destroy;
begin
  DeallocateHWnd(FHWnd);  // Delete window handle ...
  inherited;
end;

// Process windows messages :
procedure TcyBaseComm.WndMethod(var Msg: TMessage);
var
  Handled: Boolean;
begin
  Handled := false;

  // Receiving commands:
  if Msg.Msg = MsgCommand
  then begin
    HandleIncomingCommand(Msg);
    Handled := true;
  end;

  // Receiving stream:
  if Msg.Msg = WM_COPYDATA
  then begin
    HandleIncomingStream(TWMCopyData(Msg), Msg.WParam);
    Handled := true;
  end;

  if Handled
  then Msg.Result := MsgResultOk
  else Msg.Result := DefWindowProc(fHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TcyBaseComm.HandleIncomingCommand(var Msg: TMessage);
var
  CommandType: TCommandType;
  aCommand: Word;
  aParam: Integer;
begin
  CommandType := TCommandType(Msg.WParamHi);
  aCommand := Msg.WParamLo;
  aParam := Msg.LParam;

  case CommandType of
    ctDevelopperDefined:
      DoReceiveDevelopperCommand(aCommand, aParam);

    ctUserDefined:
      DoReceiveCommand(aCommand, aParam);
  end;
end;

procedure TcyBaseComm.HandleIncomingStream(var Msg: TWMCopyData; Param: Integer);
var
  ReceiveMemoryStream: TMemoryStream;
  StreamContentType: TStreamContentType;
  FromHandle: THandle;
begin
  ReceiveMemoryStream := TMemoryStream.Create;

  try
    ReceiveMemoryStream.Clear;
    ReceiveMemoryStream.Write(Msg.copyDataStruct.lpData^, Msg.copyDataStruct.cbData);
    StreamContentType := TStreamContentType(Msg.CopyDataStruct.dwData);
    FromHandle := Msg.From;

    case StreamContentType of
      scUserDefined:
        DoReceiveUserStream(FromHandle, ReceiveMemoryStream, Param);

      scDevelopperDefined:
        DoReceiveDevelopperStream(FromHandle, ReceiveMemoryStream, Param);

      scString:
        DoReceiveString(FromHandle, MemoryStreamToString(ReceiveMemoryStream), Param);
    end;
  finally
    ReceiveMemoryStream.Free;
  end;
end;

function TcyBaseComm.MemoryStreamToString(Stream: TMemoryStream): String;
var
  StringStream: TStringStream;
begin
  try
    StringStream := TStringStream.Create('');
    Stream.Seek(0, soFromBeginning);
    StringStream.CopyFrom(Stream, Stream.Size);
    RESULT := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

procedure TcyBaseComm.DoReceiveDevelopperCommand(aCommand: Word; aParam: Integer);
begin
  //
end;

procedure TcyBaseComm.DoReceiveCommand(aCommand: Word; aParam: Integer);
begin
  //
end;

procedure TcyBaseComm.DoReceiveDevelopperStream(From: THandle; aStream: TMemoryStream; Param: Integer);
begin
  //
end;

procedure TcyBaseComm.DoReceiveUserStream(From: THandle; aStream: TMemoryStream; Param: Integer);
begin
  //
end;

procedure TcyBaseComm.DoReceiveString(From: THandle; aString: String; Param: Integer);
begin
  //
end;

function TcyBaseComm.InternalSendCommand(ToBaseCommHandle: THandle; CommandType: TCommandType; aCommand: Word; Param: Integer): Boolean;
var wParam: Integer;
begin
  wParam := MakeWParam(aCommand, Ord(CommandType));  // Contraire de MakeWParam c' est HIWORD/LOWORD ...
  RESULT := PostMessage(ToBaseCommHandle, MsgCommand, wParam, Param);
end;

function TcyBaseComm.DevelopperSendCommand(ToBaseCommHandle: THandle; aCommand: Word; Param: Integer): Boolean;
begin
  RESULT := InternalSendCommand(ToBaseCommHandle, ctDevelopperDefined, aCommand, Param);
end;

function TcyBaseComm.InternalSendStream(ToBaseCommHandle: THandle; aMemoryStream: TMemoryStream; StreamContentType: TStreamContentType; Param: Integer): Boolean;
begin
  RESULT := false;

  try
    SendCopyDataStruct.dwData := Ord(StreamContentType);    // In order to know stream content type
    SendCopyDataStruct.cbData := aMemoryStream.Size;        // Size
    SendCopyDataStruct.lpData := aMemoryStream.Memory;      // Pointer
    SendCopyDataStructAddress := Integer(@SendCopyDataStruct);
    // SendMessage is Synch method:
    Result := SendMessage(ToBaseCommHandle, WM_COPYDATA, Param, SendCopyDataStructAddress) = MsgResultOk;
    // PostMessage(ServerInfo.Handle, WM_COPYDATA, Param, SendCopyDataStructAddress);     // Asynch message with WM_COPYDATA doesn' t work!
  finally

  end;
end;

function TcyBaseComm.DevelopperSendStream(ToBaseCommHandle: THandle; aMemoryStream: TMemoryStream; Param: Integer): Boolean;
begin
  RESULT := InternalSendStream(ToBaseCommHandle, aMemoryStream, scDevelopperDefined, Param);
end;

function TcyBaseComm.DevelopperSendStream(ToBaseCommHandle: THandle; aString: String; Param: Integer): Boolean;
var
  {$IFDEF UNICODE}
  Bytes: TBytes;
  {$ENDIF}
  SendMemoryStream: TMemoryStream;
begin
  RESULT := false;
  SendMemoryStream := TMemoryStream.Create;

  try
    SendMemoryStream.clear;
    {$IFDEF UNICODE}
    Bytes := BytesOf(aString);
    SendMemoryStream.Write(Bytes[0], Length(Bytes));
    {$ELSE}
    SendMemoryStream.WriteBuffer(aString[1], Length(aString));
    {$ENDIF}
    RESULT := InternalSendStream(ToBaseCommHandle, SendMemoryStream, scDevelopperDefined, Param);
  finally
    SendMemoryStream.Free;
  end;
end;

function TcyBaseComm.SendCommand(ToBaseCommHandle: THandle; aCommand: Word; UserParam: Integer): Boolean;
begin
  RESULT := InternalSendCommand(ToBaseCommHandle, ctUserDefined, aCommand, UserParam);
end;

function TcyBaseComm.SendStream(ToBaseCommHandle: THandle; aMemoryStream: TMemoryStream; UserParam: Integer): Boolean;
begin
  RESULT := InternalSendStream(ToBaseCommHandle, aMemoryStream, scUserDefined, UserParam);
end;

function TcyBaseComm.SendString(ToBaseCommHandle: THandle; aString: String; UserParam: Integer): Boolean;
var
  {$IFDEF UNICODE}
  Bytes: TBytes;
  {$ENDIF}
  SendMemoryStream: TMemoryStream;
begin
  RESULT := false;
  SendMemoryStream := TMemoryStream.Create;

  try
    SendMemoryStream.clear;
    {$IFDEF UNICODE}
    Bytes := BytesOf(aString);
    SendMemoryStream.Write(Bytes[0], Length(Bytes));
    {$ELSE}
    SendMemoryStream.WriteBuffer(aString[1], Length(aString));
    {$ENDIF}
    RESULT := InternalSendStream(ToBaseCommHandle, SendMemoryStream, scString, UserParam);
  finally
    SendMemoryStream.Free;
  end;
end;

end.
