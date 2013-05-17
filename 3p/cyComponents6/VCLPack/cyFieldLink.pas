{   Component(s):
    cyFieldLink

    Description:
    Visualize dataset field value in any non DB control.

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

unit cyFieldLink;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses StdCtrls, Graphics, classes, Windows, Messages, Controls, DB, DBCtrls, typInfo, Variants;

type
  TProcGetValue = procedure (Sender: TObject; Value: Variant) of object;

  TcyFieldLink = class(TComponent)
  private
    FDataLink: TFieldDataLink;       
    FComponent: TComponent;
    FComponentProperty: ShortString;
    FCustomChangeValue: TNotifyEvent;
    FOnError: TNotifyEvent;
    FEnabled: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetComponent(const Value: TComponent);
    procedure SetComponentProperty(const Value: ShortString);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Component: TComponent read FComponent write SetComponent;
    property ComponentProperty: ShortString read FComponentProperty write SetComponentProperty;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Enabled: Boolean read FEnabled write SetEnabled default true;    
    property OnCustomChangeValue: TNotifyEvent read FCustomChangeValue write FCustomChangeValue;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

implementation

uses Types;

constructor TcyFieldLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled  := true;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TcyFieldLink.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;    
end;

procedure TcyFieldLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TcyFieldLink.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyFieldLink.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TcyFieldLink.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TcyFieldLink.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TcyFieldLink.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TcyFieldLink.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TcyFieldLink.DataChange(Sender: TObject);
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;

         function RangedValue(const AMin, AMax: int64): Int64;
         begin
           RESULT := FDataLink.Field.AsInteger;

           if RESULT < AMin
           then RESULT := AMin;

           if RESULT > AMax
           then RESULT := AMax;
         end;

begin
  if (FDataLink.Field <> nil) and (FEnabled) and (not (csDesigning in ComponentState))
  then
    if Assigned(FCustomChangeValue)   // Allow developer to control assignment value to the control ...
    then begin
      FCustomChangeValue(Self);
    end
    else
      if (FComponent <> nil) and (FComponentProperty <> '')
      then begin
        PropInfo := GetPropInfo(FComponent.ClassInfo, FComponentProperty);

        if Assigned(PropInfo)
        then
          try                         // Assign value to the property ...
            TypeData := GetTypeData(PropInfo^.PropType^);

            case PropInfo.PropType^.Kind of
              tkInteger, tkChar, tkWChar:
                SetOrdProp(FComponent, FComponentProperty, RangedValue(Typedata^.MinValue, Typedata^.MaxValue));

              tkEnumeration:
                if VarType(FDataLink.Field.Value) = varString
                then SetEnumProp(FComponent, FComponentProperty, FDataLink.Field.AsString)
                else SetOrdProp(FComponent, FComponentProperty, RangedValue(Typedata^.MinValue, Typedata^.MaxValue));

              tkSet:
                if VarType(FDataLink.Field.Value) = varInteger
                then SetOrdProp(FComponent, FComponentProperty, FDataLink.Field.AsInteger)
                else SetSetProp(FComponent, FComponentProperty, FDataLink.Field.AsString);

              tkFloat:
                SetFloatProp(FComponent, FComponentProperty, FDataLink.Field.AsFloat);

              tkString, tkLString, tkWString {$IFDEF DELPHI2009_OR_ABOVE}, tkUString {$ENDIF}:
                SetStrProp(FComponent, FComponentProperty, FDataLink.Field.AsString);

              tkVariant:
                SetVariantProp(FComponent, FComponentProperty, FDataLink.Field.Value);

              tkInt64:
                SetInt64Prop(FComponent, FComponentProperty, RangedValue(Typedata^.MinValue, Typedata^.MaxValue));
            end;
          except
            if not (csDesigning in ComponentState)
            then
              if Assigned(FOnError)
              then FOnError(Self);
          end;
      end;
end;

procedure TcyFieldLink.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TcyFieldLink.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TcyFieldLink.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TcyFieldLink.SetComponent(const Value: TComponent);
begin
  FComponent := Value;
  DataChange(Self);
end;

procedure TcyFieldLink.SetComponentProperty(const Value: ShortString);
begin
  FComponentProperty := Value;  
  DataChange(Self);
end;

end.
