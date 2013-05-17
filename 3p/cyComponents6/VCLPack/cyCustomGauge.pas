{   Component(s):
    tcyCustomGauge

    Description:
    Base for Gauge components

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

unit cyCustomGauge;

interface

uses classes, Windows, Graphics, Controls, VCL.cyTypes, VCL.cyClasses, cyBaseMeasure;

type
  TProcOnPaintItem = procedure (Sender: TObject; aRect: TRect; Value: Double) of object;

  TcyCustomGauge = class(TcyBaseMeasure)
  private
    FBevels: TcyBevels;
    FPosition: Double;
    FOldPosition: Double;  
    FStep: Double;
    FTracking: boolean;
    FOnChange: TNotifyEvent;
    FOnCustomDrawItem: TProcOnPaintItem;
    FPrecision: Integer;
    FPartialPaint: Boolean;
    procedure SetBevels(const Value: TcyBevels);
    procedure SetPrecision(const Value: Integer);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;                       // Call OnMouseMove procedure ...
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;   // Call OnMouseUp procedure ...
    procedure SetInternalPosition(Value: Double);
    procedure SetPosition(const Value: Double); Virtual;   
    procedure BevelsChanged(Sender: TObject);
    property Bevels: TcyBevels read FBevels write SetBevels;
    property PartialPaint: Boolean read FPartialPaint write FPartialPaint default true;
    property Position: Double read FPosition write SetPosition;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property OldPosition: Double read FOldPosition write FOldPosition;
    property Step: Double read FStep write FStep;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCustomDrawItem: TProcOnPaintItem read FOnCustomDrawItem write FOnCustomDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetValueFromPos(fromPoint: TPoint; var Value: Double): boolean; virtual;
    procedure StepBy(aValue: Double);
    procedure StepIt;
    property Canvas;
  published
  end;

implementation

uses Math;

{ TcyCustomGauge }

constructor TcyCustomGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBevels := TcyBevels.Create(self, TcyBevel);
  FBevels.OnChange := BevelsChanged;

  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then begin
        with FBevels.Add do    // Frame
        begin
          HighlightColor := clBlack;
          ShadowColor := clBlack;
        end;

        with FBevels.Add do    // Inner 3D frame
          Width := 3;

        with FBevels.Add do    // Contrast Frame
          Style := bcLowered;

        with FBevels.Add do    // Border between Bevels and Shape
        begin
          HighlightColor := clBlack;
          ShadowColor := clBlack;
          Width := 1;
        end;
      end;

  FPartialPaint := true;
  FPosition := 0;
  FPrecision := 0;
  FOldPosition := 0;
  FStep := 1;
  FTracking := false;  
end;

destructor TcyCustomGauge.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;
  inherited Destroy;
end;

procedure TcyCustomGauge.Loaded;
begin
  Inherited;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCustomGauge.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
  MeasureBoundsChanged;
end;

procedure TcyCustomGauge.BevelsChanged(Sender: TObject);
begin
  MeasureBoundsChanged;   
  Invalidate;
end;

procedure TcyCustomGauge.SetPosition(const Value: Double);
var Rect: TRect;
begin
  FOldPosition := FPosition;
  FPosition := RoundTo(Value, (-1) * FPrecision);

  Rect := ClientRect;
  if Assigned(OnBeforePaint) then OnBeforePaint(Self);
  Draw(Rect, not FPartialPaint);  // Avoid clipping using Invalidate !!!
  if Assigned(OnAfterPaint) then OnAfterPaint(Self);

  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCustomGauge.SetInternalPosition(Value: Double);
begin
  FOldPosition := FPosition;
  FPosition := RoundTo(Value, (-1) * FPrecision);
end;

procedure TcyCustomGauge.StepIt;
begin
  if FStep > 0
  then begin
    if Position + FStep > Max
    then Position := Max
    else Position := Position + FStep;
  end
  else begin
    if Position + FStep < Min
    then Position := Min
    else Position := Position + FStep;
  end;    
end;

procedure TcyCustomGauge.StepBy(aValue: Double);
begin
  if FStep > 0
  then begin
    if Position + aValue > Max
    then Position := Max
    else Position := Position + aValue;
  end
  else begin
    if Position + aValue < Min
    then Position := Min
    else Position := Position + aValue;
  end;
end;

procedure TcyCustomGauge.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var PositionValue: Double;
begin
  if not ReadOnly
  then begin
    FTracking := true;
    if GetValueFromPos(point(x, y), PositionValue)
    then begin
//      if not Smooth then PositionValue := RoundToItemValue(PositionValue);
      Position := PositionValue;
    end;
  end;

  inherited;
end;

procedure TcyCustomGauge.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FTracking := false;
  inherited;
end;

procedure TcyCustomGauge.MouseMove(Shift: TShiftState; X, Y: Integer);
var PositionValue: Double;
begin
  if FTracking
  then begin
    if GetValueFromPos(point(x, y), PositionValue)
    then begin
//      if not Smooth then PositionValue := RoundToItemValue(PositionValue);
      Position := PositionValue;
    end;
  end;

  inherited;
end;

{function TcyCustomGauge.RoundToItemValue(aValue: Double): Double;
var OneItemValue: Double;
begin
  RESULT := aValue;

  if (RESULT > Min) and (RESULT < Max)
  then begin
    OneItemValue := GetItemValue(1);

    ItemValue := Min + (I * (Max-Min) / ItemsCount);
  end;
end; }

function TcyCustomGauge.GetValueFromPos(fromPoint: TPoint; var Value: Double): boolean;
begin
  RESULT := false;
end;

procedure TcyCustomGauge.SetPrecision(const Value: Integer);
begin
  if (Value >= 0) and (Value <> FPrecision)
  then begin
    FPrecision := Value;
    Position := FPosition; // Apply precision to Position ...
  end;
end;

end.
