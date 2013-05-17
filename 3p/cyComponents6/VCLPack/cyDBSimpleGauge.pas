{   Component(s):
    tcyDBSimpleGauge

    Description:
    It' s a gauge connected to a Field table with the capacity of TcySimpleGauge.

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

unit cyDBSimpleGauge;

interface

uses StdCtrls, Graphics, classes, Windows, Messages, Controls, DB, DBCtrls, cySimpleGauge;

type
  TcyDBSimpleGauge = class(TcySimpleGauge)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetFieldValue: Double;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure UpdateData(Sender: TObject);  // Modify the field value ...
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetPosition(const Value: Double); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

constructor TcyDBSimpleGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TcyDBSimpleGauge.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;      
  inherited Destroy;
end;

procedure TcyDBSimpleGauge.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TcyDBSimpleGauge.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TcyDBSimpleGauge.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TcyDBSimpleGauge.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TcyDBSimpleGauge.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TcyDBSimpleGauge.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TcyDBSimpleGauge.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TcyDBSimpleGauge.GetFieldValue: Double;
begin
  if FDataLink.Field <> nil
  then Result := FDataLink.Field.AsFloat
  else Result := Min;
end;

procedure TcyDBSimpleGauge.DataChange(Sender: TObject);
begin
  inherited SetPosition(GetFieldValue);
end;

procedure TcyDBSimpleGauge.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil
  then begin
    if FDataLink.Field.DataType in [ftInteger, ftSmallint, ftWord, ftFloat, ftCurrency, ftBCD, ftLargeint]
    then FDataLink.Field.AsInteger := Round(Position);

    if FDataLink.Field.DataType in [ftFloat, ftCurrency]
    then FDataLink.Field.AsFloat := Position;
  end;
end;

procedure TcyDBSimpleGauge.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

function TcyDBSimpleGauge.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TcyDBSimpleGauge.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TcyDBSimpleGauge.SetPosition(const Value: Double);
begin
  if not (csDesigning in ComponentState)
  then
    if Enabled and (Value <> Position) and (FDataLink.Field <> nil)
    then
      if FDataLink.Edit
      then begin
        inherited SetPosition(Value);
        FDataLink.Modified;
        FDataLink.UpdateRecord;  // Update other DBControls like DBGrid ...
      end;
end;

end.
