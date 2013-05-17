{   Component(s):
    TcyBaseCommRoomConnector

    Description:
    Base communication room connector component to connect to a virtual room for TcyBaseComm components to see and communicate with each other

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

unit cyBaseCommRoomConnector;

interface

uses Classes, Windows, Controls, SysUtils, cyBaseComm;

type
  TNickName = String[20];

  TcyRoomInfo = packed record
    ConnexionCount: Integer;
    Tag: Integer;
  end;

  TcyConnexionInfo = packed record
    NickName: TNickName;
    BaseComHandle: THandle;
    Tag: Integer;
  end;

  TcyConnexions = Array of TcyConnexionInfo;

  TcyBaseCommRoomConnector = class(TComponent)
  private
    // file mapping handling variables:
    FileMapHandle: THandle;
  protected
    procedure CreateFileMap(RoomID: String);
    procedure CloseFileMap;
    function SetRoomInfo(WithRoomInfo: TcyRoomInfo): Boolean;
    function SetConnexions(WithConnexions: TcyConnexions): Boolean;
    // Inform other connexions of any new/deleted/updated connexion:
    procedure CastChange(aCommand: Word; aParam: Integer);
  public
    function GetRoomInfo: TcyRoomInfo;
    function GetConnexions: TcyConnexions;
    function AddConnexion(ConnexionInfo: TcyConnexionInfo): Boolean; overload;
    function AddConnexion(BaseComHandle: Thandle; NickName: TNickName; ConnexionTag: Integer): Boolean; overload;
    function DeleteConnexion(ConnexionInfo: TcyConnexionInfo): Integer; overload;
    function DeleteConnexion(BaseComHandle: THandle): Integer; overload;
    function UpdateConnexion(ConnexionInfo: TcyConnexionInfo): Boolean; overload;
    function UpdateConnexion(BaseComHandle: Thandle; withNickName: TNickName; withTag: Integer): Boolean; overload;
    function GetConnexion(BaseComHandle: Thandle; var ConnexionInfo: TcyConnexionInfo): Boolean; overload;
    function GetConnexion(WithNickName: TNickName; var ConnexionInfo: TcyConnexionInfo): Boolean; overload;
    function GetConnexion(WithTag: Integer; var ConnexionInfo: TcyConnexionInfo): Boolean; overload;
  end;

const
  sPrefixInstanceID = 'CYCOMMROOM';

  CmdAddedConnexion = 0;
  CmdRemovedConnexion = 1;
  CmdUpdatedConnexion = 2;

implementation

{ TcyBaseCommRoomConnector }
procedure TcyBaseCommRoomConnector.CreateFileMap(RoomID: String);
var
  MaxSize: Cardinal;
  MaxConnexions: Cardinal;
begin
  FileMapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, pchar(sPrefixInstanceID + RoomID));
  if FileMapHandle = 0  // Not created yet, must use CreateFileMapping ...
  then begin
    MaxConnexions := 1000;
    MaxSize := SizeOf(TcyRoomInfo) + SizeOf(TcyConnexionInfo) * MaxConnexions;
    FileMapHandle := CreateFileMapping($FFFFFFFF, NIL, PAGE_READWRITE, 0, MaxSize, pchar(sPrefixInstanceID + RoomID));
  end;
end;

procedure TcyBaseCommRoomConnector.CloseFileMap;
begin
  if FileMapHandle = 0 then
    Exit;
  CloseHandle(FileMapHandle);
  FileMapHandle := 0;
end;

function TcyBaseCommRoomConnector.GetRoomInfo: TcyRoomInfo;
var FileMapData: Pointer;

    procedure InitializeRoom;
    begin
      RESULT.ConnexionCount := 0;
    end;

begin
  InitializeRoom;
  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      Move(FileMapData^, RESULT, SizeOf(TcyRoomInfo));
    except
      InitializeRoom;
    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

function TcyBaseCommRoomConnector.SetRoomInfo(WithRoomInfo: TcyRoomInfo): Boolean;
var FileMapData: Pointer;
begin
  RESULT := false;
  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_WRITE, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      Move(WithRoomInfo, FileMapData^, SizeOf(TcyRoomInfo));
      RESULT := true;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

function TcyBaseCommRoomConnector.GetConnexions: TcyConnexions;
var
  FileMapData: Pointer;
  Header: TcyRoomInfo;
  OffsetValue: SmallInt;
  i: Integer;

    procedure InitializeConnexions;
    begin
      SetLength(RESULT, 0);
    end;

begin
  InitializeConnexions;
  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      // Get number of connexions:
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      // Fill array item by item (otherwise, it doesn' t work trying to transfer Array at once) :
      SetLength(RESULT, Header.ConnexionCount);

      for i := 0 to Header.ConnexionCount - 1 do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(FileMapData^, RESULT[i], OffsetValue);
      end;
    except
      InitializeConnexions;
    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

function TcyBaseCommRoomConnector.SetConnexions(WithConnexions: TcyConnexions): Boolean;
var
  Header: TcyRoomInfo;
  FileMapData: Pointer;
  OffsetValue: SmallInt;
  i: Integer;
begin
  RESULT := false;

  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_WRITE, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      // Update FileMapping Header ConnexionCount:
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);
      Header.ConnexionCount := High(WithConnexions) + 1;
      Move(Header, FileMapData^, SizeOf(TcyRoomInfo));

      // Fill item by item (otherwise, it doesn' t work trying to transfer Array at once) :
      for i := 0 to High(WithConnexions) do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(WithConnexions[i], FileMapData^, OffsetValue);
      end;

      RESULT := true;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

function TcyBaseCommRoomConnector.AddConnexion(ConnexionInfo: TcyConnexionInfo): Boolean;
begin
  RESULT := AddConnexion(ConnexionInfo.BaseComHandle, ConnexionInfo.NickName, ConnexionInfo.Tag);
end;

function TcyBaseCommRoomConnector.AddConnexion(BaseComHandle: Thandle; NickName: TNickName; ConnexionTag: Integer): Boolean;
var
  FileMapData, Sav: Pointer;
  Header: TcyRoomInfo;
  ConnexionInfo, CurConnexion: TcyConnexionInfo;
  OffsetValue: SmallInt;
  i: Integer;
  ConnexionExists: Boolean;
begin
  RESULT := false;
  ConnexionExists := false;

  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_WRITE, 0, 0, 0);
  Sav := FileMapData;

  if Assigned(FileMapData)
  then begin
    try
      ConnexionInfo.BaseComHandle := BaseComHandle;
      ConnexionInfo.NickName := NickName;
      ConnexionInfo.Tag := ConnexionTag;

      // Get header with the number of connexions :
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      // Compare all connexions to see if the connexion is already in the list :
      for i := 0 to Header.ConnexionCount - 1 do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(FileMapData^, CurConnexion, OffsetValue);

        if CurConnexion.BaseComHandle = ConnexionInfo.BaseComHandle
        then ConnexionExists := true;
      end;

      if not ConnexionExists
      then begin
        // *** Add connexion *** //
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(ConnexionInfo, FileMapData^, OffsetValue);

        // *** Update header with ConnexionCount *** //
        Inc(Header.ConnexionCount);
        Move(Header, Sav^, SizeOf(TcyRoomInfo));
        RESULT := true;
      end;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;

  if RESULT
  then CastChange(CmdAddedConnexion, BaseComHandle);
end;

function TcyBaseCommRoomConnector.DeleteConnexion(ConnexionInfo: TcyConnexionInfo): Integer;
begin
  RESULT := DeleteConnexion(ConnexionInfo.BaseComHandle);
end;

function TcyBaseCommRoomConnector.DeleteConnexion(BaseComHandle: THandle): Integer;
var
  Header: TcyRoomInfo;
  Connexions: TcyConnexions;
  FileMapData, Sav: Pointer;
  OffsetValue: SmallInt;
  i: Integer;
begin
  RESULT := 0;

  if FileMapHandle = 0 then
    Exit;

  Connexions := GetConnexions;
  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_WRITE, 0, 0, 0);
  Sav := FileMapData;

  if Assigned(FileMapData)
  then begin
    try
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      for i := 0 to High(Connexions) do
        if Connexions[i].BaseComHandle <> BaseComHandle
        then begin
          // Offset pointer address:
          Inc(PByte(FileMapData), OffsetValue);
          OffsetValue := SizeOf(TcyConnexionInfo);
          Move(Connexions[i], FileMapData^, OffsetValue);
        end
        else
          Inc(RESULT);

      if RESULT <> 0
      then begin
        if Header.ConnexionCount > RESULT
        then Dec(Header.ConnexionCount, RESULT)
        else Header.ConnexionCount := 0;

        // Update FileMapping Header ConnexionCount:
        Move(Header, Sav^, SizeOf(TcyRoomInfo));
      end;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;

  if RESULT <> 0 then
    CastChange(CmdRemovedConnexion, BaseComHandle);
end;

function TcyBaseCommRoomConnector.UpdateConnexion(ConnexionInfo: TcyConnexionInfo): Boolean;
begin
  RESULT := UpdateConnexion(ConnexionInfo.BaseComHandle, ConnexionInfo.NickName, ConnexionInfo.Tag);
end;

function TcyBaseCommRoomConnector.UpdateConnexion(BaseComHandle: Thandle; withNickName: TNickName; withTag: Integer): Boolean;
var
  FileMapData, Sav: Pointer;
  Header: TcyRoomInfo;
  ConnexionInfo, CurConnexion: TcyConnexionInfo;
  OffsetValue: SmallInt;
  i: Integer;
  ConnexionExists: Boolean;
begin
  RESULT := false;
  ConnexionExists := false;

  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_WRITE, 0, 0, 0);
  Sav := FileMapData;

  if Assigned(FileMapData)
  then begin
    try
      ConnexionInfo.BaseComHandle := BaseComHandle;
      ConnexionInfo.NickName := withNickName;
      ConnexionInfo.Tag := withTag;

      // Get header with the number of connexions :
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      // Compare all connexions to find location :
      for i := 0 to Header.ConnexionCount - 1 do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(FileMapData^, CurConnexion, OffsetValue);

        if CurConnexion.BaseComHandle = ConnexionInfo.BaseComHandle
        then begin
          // Update connexion data :
          Move(ConnexionInfo, FileMapData^, OffsetValue);
          RESULT := true;
        end;
      end;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;

  if RESULT
  then CastChange(CmdUpdatedConnexion, BaseComHandle);
end;

function TcyBaseCommRoomConnector.GetConnexion(BaseComHandle: Thandle; var ConnexionInfo: TcyConnexionInfo): Boolean;
var
  FileMapData: Pointer;
  Header: TcyRoomInfo;
  CurConnexion: TcyConnexionInfo;
  OffsetValue: SmallInt;
  i: Integer;
begin
  RESULT := false;
  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      // Get number of connexions:
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      // Locate connexion :
      for i := 0 to Header.ConnexionCount - 1 do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(FileMapData^, CurConnexion, OffsetValue);

        if CurConnexion.BaseComHandle = BaseComHandle
        then begin
          ConnexionInfo := CurConnexion;
          RESULT := true;
          Break;
        end;
      end;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

function TcyBaseCommRoomConnector.GetConnexion(WithNickName: TNickName; var ConnexionInfo: TcyConnexionInfo): Boolean;
var
  FileMapData: Pointer;
  Header: TcyRoomInfo;
  CurConnexion: TcyConnexionInfo;
  OffsetValue: SmallInt;
  i: Integer;
begin
  RESULT := false;
  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      // Get number of connexions:
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      // Locate connexion :
      for i := 0 to Header.ConnexionCount - 1 do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(FileMapData^, CurConnexion, OffsetValue);

        if CurConnexion.NickName = WithNickName
        then begin
          ConnexionInfo := CurConnexion;
          RESULT := true;
          Break;
        end;
      end;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

function TcyBaseCommRoomConnector.GetConnexion(WithTag: Integer; var ConnexionInfo: TcyConnexionInfo): Boolean;
var
  FileMapData: Pointer;
  Header: TcyRoomInfo;
  CurConnexion: TcyConnexionInfo;
  OffsetValue: SmallInt;
  i: Integer;
begin
  RESULT := false;
  if FileMapHandle = 0 then
    Exit;

  FileMapData := MapViewOfFile(FileMapHandle, FILE_MAP_READ, 0, 0, 0);

  if Assigned(FileMapData)
  then begin
    try
      // Get number of connexions:
      OffsetValue := SizeOf(TcyRoomInfo);
      Move(FileMapData^, Header, OffsetValue);

      // Locate connexion :
      for i := 0 to Header.ConnexionCount - 1 do
      begin
        // Offset pointer address:
        Inc(PByte(FileMapData), OffsetValue);
        OffsetValue := SizeOf(TcyConnexionInfo);
        Move(FileMapData^, CurConnexion, OffsetValue);

        if CurConnexion.Tag = WithTag
        then begin
          ConnexionInfo := CurConnexion;
          RESULT := true;
          Break;
        end;
      end;
    except

    end;

    UnmapViewOfFile(FileMapData);
  end;
end;

procedure TcyBaseCommRoomConnector.CastChange(aCommand: Word; aParam: Integer);
var
  Connexions: TcyConnexions;
  wParam, i: Integer;
begin
  // Send a command to all connexions if Handle <> aParam :
  wParam := MakeWParam(aCommand, Ord(ctDevelopperDefined));
  Connexions := GetConnexions;
  for i := 0 to High(Connexions) do
    if Connexions[i].BaseComHandle <> aParam
    then PostMessage(Connexions[i].BaseComHandle, MsgCommand, wParam, aParam);
end;

end.
