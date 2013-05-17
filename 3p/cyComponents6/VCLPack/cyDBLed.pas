{   Component(s):
    tcyDBLed

    Description:
    A simple DBControl led with Group feature

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

unit cyDBLed;   

{$I ..\Core\cyCompilerDefines.inc}

interface    

uses SysUtils, StdCtrls, Graphics, classes, Windows, Messages, Controls, DBConsts, DB,
      DBCtrls, cyBaseLed, cyLed;

type
  TcyDBLed = class(TcyCustomLed)
  private
    FDataLink: TFieldDataLink;
    FValueOn: String;
    FValueOff: String;
    procedure DataChange(Sender: TObject);  // Inform cyDBLed of the value of the field 
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldValue: boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    // Allow personalize value for string Fields :
    procedure SetValueOn(const Value: String);
    procedure SetValueOff(const Value: String);
    procedure UpdateData(Sender: TObject);  // Modify field value with current LedStatus value
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
//    procedure CMButtonPressed(var Message: TMessage); override;
    function  GetLedStatus: TLedStatus; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetLedvalue(Value: Boolean); override;
    procedure SetReadOnly(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ValueOn: String read FValueOn write SetValueOn;
    property ValueOff: String read FValueOff write SetValueOff;
    property Align;
    property Anchors;
    property Enabled;
    property Constraints;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Herited from TcyBaseLed :
    property AllowAllOff;
    property GroupIndex;
    property LedValue;
    property ReadOnly;
    property Color;
    // Herited from TcyCustomLed :
    property Bevels;
    property LedColorOn;
    property LedColorOff;
    property LedColorDisabled;
    property ShapeLedColorOn;
    property ShapeLedColorOff;
    property ShapeLedColorDisabled;
    property ShapePenWidth;
    property ShapeType;
    property ShapeRoundRectX;
    property ShapeRoundRectY;
    property Transparent;
  end;

implementation

constructor TcyDBLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FValueOn := STextTrue;     Not allow storing '' value !
//  FValueOff := STextFalse;   Not allow storing '' value !
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TcyDBLed.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TcyDBLed.SetValueOn(const Value: String);
begin
  FValueOn := Value;
  DataChange(Self);
end;

procedure TcyDBLed.SetValueOff(const Value: String);
begin
  FValueOff := Value;
  DataChange(Self);
end;

procedure TcyDBLed.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TcyDBLed.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyDBLed.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);

  LedStatusChanged;
end;

function TcyDBLed.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TcyDBLed.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  LedStatusChanged;
end;

function TcyDBLed.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TcyDBLed.GetFieldValue: boolean;
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

procedure TcyDBLed.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
  Inherited;
end;

procedure TcyDBLed.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then SetInternalLedValue(GetFieldValue);
end;

procedure TcyDBLed.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then
    if (FDataLink.Field.DataType = ftBoolean) and ((fValueOn = '') or (fValueOff = ''))
    then
      FDataLink.Field.AsBoolean := LedValue
    else
      if LedValue
      then FDataLink.Field.Text := FValueOn
      else FDataLink.Field.Text := FValueOff;
end;

procedure TcyDBLed.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TcyDBLed.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TcyDBLed.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TcyDBLed.SetLedvalue(Value: Boolean);
begin
  if (Enabled) and (value <> Ledvalue) and (FDataLink.Field <> nil)
  then
    if FDataLink.Edit
    then begin
      inherited SetLedvalue(Value);

      FDataLink.Modified;
      FDataLink.UpdateRecord;  // Call UpdateData and Update other DBControls ...
    end;
end;

function TcyDBLed.GetLedStatus: TLedStatus;
begin
  if Field = nil
  then RESULT := lsDisabled
  else RESULT := inherited GetLedStatus;
end;

{procedure TcyDBLed.CMButtonPressed(var Message: TMessage);
var
  Sender: TcyDBLed;
begin
  if Message.WParam = GroupIndex                 // Same group?
  then begin
    Sender := TcyDBLed(Message.LParam);
    if Sender <> Self
    then begin
      if Sender.LedValue and LedValue
      then begin                          // Turn off ...
        if Enabled and (FDataLink.Field <> nil)
        then SetInternalLedValue(false);
//   Mot needed :
//        if FDataLink.Edit
//          then begin
//            SetInternalLedValue(false);
//            FDataLink.Modified;
//            FDataLink.UpdateRecord;  // Update other DBControls like DBGrid ...
//          end;
      end;

      AllowAllOff := Sender.AllowAllOff;
    end;
  end;
end; }

end.
