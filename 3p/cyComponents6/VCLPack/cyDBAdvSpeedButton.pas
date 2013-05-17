{   Component(s):
    tcyDBAdvSpeedButton

    Description:
    A simple DBControl button

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

unit cyDBAdvSpeedButton;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses SysUtils, StdCtrls, Graphics, classes, Windows, Messages, Controls, DBConsts, DB,
      DBCtrls, cyAdvSpeedButton, Dialogs;

type
  TcyDBAdvSpeedButton = class(TcyAdvSpeedButton)
  private
    FDataLink: TFieldDataLink;
    FValueDown: String;
    FValueUp: String;
    fReadOnly: Boolean;
    procedure DataChange(Sender: TObject);  // Inform cyDBAdvSpeedButton of the value of the field
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldValue: boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    // Allow personalize value for string Fields :
    procedure SetValueDown(const Value: String);
    procedure SetValueUp(const Value: String);
    procedure UpdateData(Sender: TObject);  // Modify field value with current Down value
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure AssignDefaultGroupIndex;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read fReadOnly write SetReadOnly default false;
    property ValueDown: String read FValueDown write SetValueDown;
    property ValueUp: String read FValueUp write SetValueUp;
  end;

implementation

constructor TcyDBAdvSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FValueDown := STextTrue;     Not allow storing '' value !
//  FValueUp := STextFalse;   Not allow storing '' value !
  fReadOnly := false;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TcyDBAdvSpeedButton.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TcyDBAdvSpeedButton.SetValueDown(const Value: String);
begin
  FValueDown := Value;
  DataChange(Self);
end;

procedure TcyDBAdvSpeedButton.SetValueUp(const Value: String);
begin
  FValueUp := Value;
  DataChange(Self);
end;

procedure TcyDBAdvSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TcyDBAdvSpeedButton.AssignDefaultGroupIndex;
begin
  if not (csLoading in ComponentState) then
    if (GroupIndex = 0) and (not AllowAllUp) then
      ShowMessage('Please set GroupIndex to non zero value and eventually set AllowAllUp to true.');
end;

function TcyDBAdvSpeedButton.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyDBAdvSpeedButton.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);

  AssignDefaultGroupIndex;
  Down := GetFieldValue;
end;

procedure TcyDBAdvSpeedButton.SetReadOnly(const Value: Boolean);
begin
  fReadOnly := Value;
  FDataLink.ReadOnly := Value;
end;

function TcyDBAdvSpeedButton.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TcyDBAdvSpeedButton.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  AssignDefaultGroupIndex;
  Down := GetFieldValue;
end;

function TcyDBAdvSpeedButton.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TcyDBAdvSpeedButton.GetFieldValue: boolean;
begin
  Result := false;

  if FDataLink.Field <> nil then
    case FDataLink.Field.DataType of
      ftBoolean:
      begin
        if FValueDown = ''
        then Result := FDataLink.Field.AsBoolean
        else Result := FDataLink.Field.Text = FValueDown;
      end;

      ftSmallint, ftInteger, ftWord, ftLargeint:
      begin
        if FValueDown = ''
        then Result := FDataLink.Field.AsInteger = 0
        else Result := FDataLink.Field.AsInteger = StrToInt(FValueDown);        // Handle negative values :) !
      end;

      {$IFDEF DELPHI2009_OR_ABOVE}
      ftLongWord, ftShortint, ftByte:
      begin
        if FValueDown = ''
        then Result := FDataLink.Field.AsInteger = 0
        else Result := FDataLink.Field.AsInteger = StrToInt(FValueDown);        // Handle negative values :) !
      end;
      {$ENDIF}

      {$IFDEF DELPHI2009_OR_ABOVE}
      ftExtended:
      begin
        if FValueDown = ''
        then Result := FDataLink.Field.AsExtended = 0
        else Result := FDataLink.Field.AsExtended = StrToInt(FValueDown);       // Handle negative values :) !
      end;
      {$ENDIF}

      ftFloat:
      begin
        if FValueDown = ''
        then Result := FDataLink.Field.AsFloat = 0
        else Result := FDataLink.Field.AsFloat = StrToFloat(FValueDown);        // Handle negative values :) !
      end;

      ftDate, ftTime, ftDateTime: Result := FDataLink.Field.AsDateTime = StrToDateTime('0' + FValueDown);
      // ftCurrency, ftBCD, ftBytes, ftVarBytes, ftAutoInc, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftOraTimeStamp, ftOraInterval, ftConnection, ftParams, ftStream
      else
        Result := FDataLink.Field.Text = FValueDown; // Strings ...
    end;
end;

procedure TcyDBAdvSpeedButton.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Down := GetFieldValue;
end;

procedure TcyDBAdvSpeedButton.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then
    if (FDataLink.Field.DataType = ftBoolean) and ((fValueDown = '') or (fValueUp = ''))
    then
      FDataLink.Field.AsBoolean := Down
    else
      if Down
      then FDataLink.Field.Text := FValueDown
      else FDataLink.Field.Text := FValueUp;
end;

procedure TcyDBAdvSpeedButton.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TcyDBAdvSpeedButton.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TcyDBAdvSpeedButton.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TcyDBAdvSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TcyDBAdvSpeedButton;
begin
  if Message.WParam = GroupIndex                 // Same group?
  then begin
    Sender := TcyDBAdvSpeedButton(Message.LParam);
    if Sender <> Self
    then begin
      if Sender.Down and Down
      then begin                          // Turn off ...
        if Enabled and (FDataLink.Field <> nil)
        then Down := false;
      end;

      AllowAllUp := Sender.AllowAllUp;
    end;
  end;

  Inherited;
end;

procedure TcyDBAdvSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CallInherited: Boolean;
begin
  if fReadOnly or (FDataLink.Field = nil) then
  begin
    if Assigned(OnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
    Exit;
  end;

  CallInherited := true;

  if (Button = mbLeft) and Enabled then
    if (not Down) or (AllowAllUp) then
      if FDataLink.Edit
      then begin
        if Assigned(OnMouseDown) then
          OnMouseDown(Self, Button, Shift, X, Y);
        CallInherited := false;
        Down := not Down;

        FDataLink.Modified;
        FDataLink.UpdateRecord;  // Call UpdateData and Update other DBControls ...
      end;

  if CallInherited then
    Inherited;
end;

end.
