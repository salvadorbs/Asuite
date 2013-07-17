{   Component(s):
    TcyCommunicate

    Description:
    Allow communication between TcyCommunicate components from same or different applications

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

unit cyCommunicate;

interface

uses classes, cyBaseComm, cyBaseCommRoomConnector;

type
  TProcOnRoomCommand = procedure (Sender: TObject; BaseComHandle: THandle) of object;

  TcyCommunicate = class(TcyBaseComm)
  private
    FOnReceiveString: TProcOnReceiveString;
    FOnReceiveStream: TProcOnReceiveMemoryStream;
    FOnReceiveCommand: TProcOnReceiveCommand;
    FOnRoomSomeoneEnter: TProcOnRoomCommand;
    FOnRoomSomeoneExit: TProcOnRoomCommand;
    FOnRoomSomeoneUpdate: TProcOnRoomCommand;
  protected
    procedure DoReceiveCommand(aCommand: Word; aParam: Integer); override;
    procedure DoReceiveDevelopperCommand(aCommand: Word; aParam: Integer); override;
    procedure DoReceiveString(From: THandle; aString: String; Param: Integer); override;
    procedure DoReceiveUserStream(From: THandle; aStream: TMemoryStream; Param: Integer); override;
  public
  published
    property Handle;
    property OnReceiveCommand: TProcOnReceiveCommand read FOnReceiveCommand write FOnReceiveCommand;
    property OnReceiveString: TProcOnReceiveString read FOnReceiveString write FOnReceiveString;
    property OnReceiveStream: TProcOnReceiveMemoryStream read FOnReceiveStream write FOnReceiveStream;
    property OnRoomSomeoneEnter: TProcOnRoomCommand read FOnRoomSomeoneEnter write FOnRoomSomeoneEnter;
    property OnRoomSomeoneExit: TProcOnRoomCommand read FOnRoomSomeoneExit write FOnRoomSomeoneExit;
    property OnRoomSomeoneUpdate: TProcOnRoomCommand read FOnRoomSomeoneUpdate write FOnRoomSomeoneUpdate;
  end;

implementation

{ TcyCommunicate }

procedure TcyCommunicate.DoReceiveCommand(aCommand: Word; aParam: Integer);
begin
//  inherited;
  if Assigned(FOnReceiveCommand) then
    FOnReceiveCommand(Self, aCommand, aParam);
end;

procedure TcyCommunicate.DoReceiveUserStream(From: THandle; aStream: TMemoryStream; Param: Integer);
begin
//  inherited;
  if Assigned(FOnReceiveStream) then
    FOnReceiveStream(Self, From, aStream, Param);
end;

procedure TcyCommunicate.DoReceiveDevelopperCommand(aCommand: Word; aParam: Integer);
begin
//  inherited;
  if aCommand = CmdAddedConnexion then
    if Assigned(FOnRoomSomeoneEnter) then
      FOnRoomSomeoneEnter(Self, aParam);

  if aCommand = CmdRemovedConnexion then
    if Assigned(FOnRoomSomeoneExit) then
      FOnRoomSomeoneExit(Self, aParam);

  if aCommand = CmdUpdatedConnexion then
    if Assigned(FOnRoomSomeoneUpdate) then
      FOnRoomSomeoneUpdate(Self, aParam);
end;

procedure TcyCommunicate.DoReceiveString(From: THandle; aString: String; Param: Integer);
begin
//  inherited;
  if Assigned(FOnReceiveString) then
    FOnReceiveString(Self, From, aString, Param);
end;

end.
