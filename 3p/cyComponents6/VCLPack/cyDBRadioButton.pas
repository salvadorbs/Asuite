{   Component(s):
    tcyDBRadioButton

    Description:
    A simple DBControl RadioButton with AllowAllOff property

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

unit cyDBRadioButton;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses SysUtils, StdCtrls, Graphics, classes, Windows, Messages, Controls, DBConsts, DB,
      DBCtrls;

type
  TcyDBRadioButton = class(TRadioButton)
  private
    InternalChange: Boolean;
    FDataLink: TFieldDataLink;
    FValueOn: String;
    FValueOff: String;
    fReadOnly: Boolean;
    fAllowAllOff: Boolean;
    fGroupIndex: Integer;
    procedure DataChange(Sender: TObject);  // Inform cyDBRadioButton of the value of the field
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldValue: boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
    // Allow personalize value for string Fields :
    procedure SetValueOn(const Value: String);
    procedure SetValueOff(const Value: String);
    procedure UpdateData(Sender: TObject);  // Modify field value with current Checked property value
    procedure UpdateExclusive;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure SetAllowAllOff(const Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);  // Called in UpdateExclusive procedure ...
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetChecked(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property AllowAllOff: Boolean read fAllowAllOff write SetAllowAllOff default false;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property GroupIndex: Integer read fGroupIndex write SetGroupIndex default 0;
    property ReadOnly: Boolean read fReadOnly write SetReadOnly default false;
    property ValueOn: String read FValueOn write SetValueOn;
    property ValueOff: String read FValueOff write SetValueOff;
  end;

implementation

constructor TcyDBRadioButton.Create(AOwner: TComponent);
begin
  InternalChange := true;     // Must be True by default ...
  inherited Create(AOwner);
//  FValueOn := STextTrue;     Not allow storing '' value !
//  FValueOff := STextFalse;   Not allow storing '' value !
  fAllowAllOff := false;
  fGroupIndex := 0;
  fReadOnly := false;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TcyDBRadioButton.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TcyDBRadioButton.SetAllowAllOff(const Value: Boolean);
begin
  if FAllowAllOff <> Value
  then begin
    FAllowAllOff := Value;
    UpdateExclusive;        // Inform FAllowAllOff value to the others from the same group
  end;
end;

procedure TcyDBRadioButton.SetGroupIndex(const Value: Integer);
begin
  if fGroupIndex <> Value
  then begin
    fGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TcyDBRadioButton.SetReadOnly(const Value: Boolean);
begin
  fReadOnly := Value;
  FDataLink.ReadOnly := Value;
end;

procedure TcyDBRadioButton.SetValueOn(const Value: String);
begin
  FValueOn := Value;
  DataChange(Self);
end;

procedure TcyDBRadioButton.SetValueOff(const Value: String);
begin
  FValueOff := Value;
  DataChange(Self);
end;

procedure TcyDBRadioButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TcyDBRadioButton.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyDBRadioButton.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);

  Checked := GetFieldValue;
end;

function TcyDBRadioButton.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TcyDBRadioButton.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  Checked := GetFieldValue;
end;

function TcyDBRadioButton.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TcyDBRadioButton.GetFieldValue: boolean;
begin
  Result := false;

  if FDataLink.Field <> nil then
    case FDataLink.Field.DataType of
      ftBoolean:
      begin
        if FValueOn = ''
        then Result := FDataLink.Field.AsBoolean
        else Result := FDataLink.Field.Text = FValueOn;
      end;

      ftSmallint, ftInteger, ftWord, ftLargeint:
      begin
        if FValueOn = ''
        then Result := FDataLink.Field.AsInteger = 0
        else Result := FDataLink.Field.AsInteger = StrToInt(FValueOn);        // Handle negative values :) !
      end;

      {$IFDEF DELPHI2009_OR_ABOVE}
      ftLongWord, ftShortint, ftByte:
      begin
        if FValueOn = ''
        then Result := FDataLink.Field.AsInteger = 0
        else Result := FDataLink.Field.AsInteger = StrToInt(FValueOn);        // Handle negative values :) !
      end;
      {$ENDIF}

      {$IFDEF DELPHI2009_OR_ABOVE}
      ftExtended:
      begin
        if FValueOn = ''
        then Result := FDataLink.Field.AsExtended = 0
        else Result := FDataLink.Field.AsExtended = StrToInt(FValueOn);       // Handle negative values :) !
      end;
      {$ENDIF}

      ftFloat:
      begin
        if FValueOn = ''
        then Result := FDataLink.Field.AsFloat = 0
        else Result := FDataLink.Field.AsFloat = StrToFloat(FValueOn);        // Handle negative values :) !
      end;

      ftDate, ftTime, ftDateTime: Result := FDataLink.Field.AsDateTime = StrToDateTime('0' + FValueOn);
      // ftCurrency, ftBCD, ftBytes, ftVarBytes, ftAutoInc, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftOraTimeStamp, ftOraInterval, ftConnection, ftParams, ftStream
      else
        Result := FDataLink.Field.Text = FValueOn; // Strings ...
    end;
end;

procedure TcyDBRadioButton.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Checked := GetFieldValue;
end;

procedure TcyDBRadioButton.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then
    if (FDataLink.Field.DataType = ftBoolean) and ((fValueOn = '') or (fValueOff = ''))
    then
      FDataLink.Field.AsBoolean := Checked
    else
      if Checked
      then FDataLink.Field.Text := FValueOn
      else FDataLink.Field.Text := FValueOff;
end;

procedure TcyDBRadioButton.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TcyDBRadioButton.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TcyDBRadioButton.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TcyDBRadioButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil)
  then begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TcyDBRadioButton.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    BN_CLICKED:
    begin
      InternalChange := false;

      if fAllowAllOff and Checked
      then SetChecked(false)
      else SetChecked(True);

      InternalChange := true;
    end;

    BN_DOUBLECLICKED: DblClick;
  end;
end;

procedure TcyDBRadioButton.SetChecked(Value: Boolean);
begin
  if InternalChange then
  begin
    inherited;
    Exit;
  end;

  if fReadOnly or (not Enabled) then
  begin
    Value := GetFieldValue;
    inherited;
    Exit;
  end;

  if (Checked <> Value) and (FDataLink.Field <> nil) then
  begin
    if FDataLink.Edit
    then begin
      if (not Value) and (not FAllowAllOff) and (FGroupIndex <> 0) then
      begin
        // Can' t change Checked property!
      end
      else begin
        inherited;

        if Value then
          UpdateExclusive;   // Send message to turn off the other one ...
      end;

      FDataLink.Modified;
      FDataLink.UpdateRecord;  // Call UpdateData and Update other DBControls ...
    end;
  end
  else
    inherited;
end;

procedure TcyDBRadioButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TcyDBRadioButton;
begin
  if Message.WParam = GroupIndex                 // Same group?
  then begin
    Sender := TcyDBRadioButton(Message.LParam);
    if Sender <> Self
    then begin
      if Sender.Checked and Checked
      then begin                          // Turn off ...
        if Enabled and (FDataLink.Field <> nil)
        then Checked := false;
{  Not needed        if FDataLink.Edit
          then begin
            Checked := false;
            FDataLink.Modified;
            FDataLink.UpdateRecord;  // Update other DBControls like DBGrid ...
          end;   }
      end;

      AllowAllOff := Sender.AllowAllOff;
    end;
  end;
end;

end.
