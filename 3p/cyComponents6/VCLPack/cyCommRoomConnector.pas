{   Component(s):
    TcyCommRoomConnector

    Description:
    Communication room connector component to connect to a virtual room for TcyBaseComm components to see and communicate with each other

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

unit cyCommRoomConnector;

interface

uses Classes, Windows, cyBaseComm, cyBaseCommRoomConnector;

type
  TcyCommRoomConnector = class(TcyBaseCommRoomConnector)
  private
    FRoomID: String;
    FActive: Boolean;
    FNickName: TNickName;
    FCommComponent: TcyBaseComm;
    FConnexionTag: Integer;
    procedure SetRoomID(Value: String);
    procedure SetActive(const Value: Boolean);
    procedure SetNickName(const Value: TNickName);
    procedure SetCommComponent(const Value: TcyBaseComm);
    procedure SetConnexionTag(const Value: Integer);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ActiveChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetInfo(var ConnectorInfo: TcyConnexionInfo): Boolean;
  published
    property Active: Boolean read FActive write SetActive default true;
    property CommComponent: TcyBaseComm read FCommComponent write SetCommComponent;
    property ConnexionTag: Integer read FConnexionTag write SetConnexionTag default 0;
    property NickName: TNickName read FNickName write SetNickName;
    property RoomID: String read FRoomID write SetRoomID;
  end;

implementation

{ TcyCommRoomConnector }
constructor TcyCommRoomConnector.Create(AOwner: TComponent);
begin
  inherited;
  FActive := true;
  FConnexionTag := 0;
  FNickName := '';
  FRoomID := 'Unique name please';
end;

procedure TcyCommRoomConnector.Loaded;
begin
  inherited;
  ActiveChanged;
end;

procedure TcyCommRoomConnector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(FCommComponent) then
    if AComponent = FCommComponent then
      CommComponent := Nil;
end;

destructor TcyCommRoomConnector.Destroy;
var ConnexionInfo: TcyConnexionInfo;
begin
  if Factive
  then begin
    if GetInfo(ConnexionInfo)
    then DeleteConnexion(ConnexionInfo);

    CloseFileMap;
  end;

  inherited;
end;

function TcyCommRoomConnector.GetInfo(var ConnectorInfo: TcyConnexionInfo): Boolean;
begin
  RESULT := Assigned(FCommComponent);

  if RESULT
  then begin
    ConnectorInfo.BaseComHandle := FCommComponent.Handle;
    ConnectorInfo.NickName := FNickName;
    ConnectorInfo.Tag := FConnexionTag;
  end;
end;

procedure TcyCommRoomConnector.ActiveChanged;
var ConnexionInfo: TcyConnexionInfo;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  if FActive
  then begin
    CreateFileMap(FRoomID);
    if GetInfo(ConnexionInfo)
    then AddConnexion(ConnexionInfo);
  end
  else begin
    if GetInfo(ConnexionInfo)
    then DeleteConnexion(ConnexionInfo);
    CloseFileMap;
  end;
end;

procedure TcyCommRoomConnector.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;
  FActive := Value;
  ActiveChanged;
end;

procedure TcyCommRoomConnector.SetCommComponent(const Value: TcyBaseComm);
var ConnexionInfo: TcyConnexionInfo;
begin
  if (FActive) and (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState))
  then begin
    // Remove old connexion :
    if GetInfo(ConnexionInfo) then
      DeleteConnexion(ConnexionInfo);

    // Add new connexion :
    if Assigned(Value)
    then begin
      AddConnexion(Value.Handle, FNickName, FConnexionTag);
      FreeNotification(Value);
    end;
  end;

  FCommComponent := Value;
end;

procedure TcyCommRoomConnector.SetConnexionTag(const Value: Integer);
var
  OldTag: Integer;
  ConnexionInfo: TcyConnexionInfo;
begin
  if FConnexionTag = Value then
    Exit;
  OldTag := FConnexionTag;
  FConnexionTag := Value;

  if (FActive) and (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState)) then
    if Assigned(FCommComponent) then
      if GetConnexion(FCommComponent.Handle, ConnexionInfo)
      then begin
        ConnexionInfo.Tag := FConnexionTag;
        UpdateConnexion(ConnexionInfo)
      end
      else begin
        GetInfo(ConnexionInfo);
        AddConnexion(ConnexionInfo);
      end;
end;

procedure TcyCommRoomConnector.SetNickName(const Value: TNickName);
var
  OldNickName: TNickName;
  ConnexionInfo: TcyConnexionInfo;
begin
  if FNickName = Value then
    Exit;
  OldNickName := FNickName;
  FNickName := Value;

  if (FActive) and (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState)) then
    if Assigned(FCommComponent) then
      if GetConnexion(FCommComponent.Handle, ConnexionInfo)
      then begin
        ConnexionInfo.NickName := FNickName;
        UpdateConnexion(ConnexionInfo)
      end
      else begin
        GetInfo(ConnexionInfo);
        AddConnexion(ConnexionInfo);
      end;
end;

procedure TcyCommRoomConnector.SetRoomID(Value: String);
begin
  Value := ValidateFileMappingName(Value);
  if FRoomID = Value then
    Exit;
  FRoomID := Value;

  if (FActive) and (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState))
  then begin
    // Close previous FileMapping:
    Active := false;

    // Open FileMapping with new name:
    Active := true;
  end;
end;

end.
