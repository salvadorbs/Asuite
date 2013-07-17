{   Component(s):
    tcyPieGauge

    Description:
    It' s a gauge with graduations that allow user defined visualisation and custom paint.
    The orientation can be left-to-right, right-to-left, top-to-bottom and bottom-to-top.

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

unit cyPieGauge;

interface

uses classes, Windows, cyCustomGauge;

type
  TcyCustomPieGauge = class(TcyCustomGauge)
  private
    FAngleMin: Integer;
    FAngleMax: Integer;
    procedure SetAngleMin(const Value: Integer);
    procedure SetAngleMax(const Value: Integer);
  protected
    procedure Draw(Rect: TRect; FullRepaint: Boolean); override;
    property Width default 200;
    property Height default 150;
    property AngleMin: Integer read FAngleMin write SetAngleMin default 0;
    property AngleMax: Integer read FAngleMax write SetAngleMax default 180;
  public
    constructor Create(AOwner: TComponent); override;
    function GetValueFromPos(fromPoint: TPoint; var Value: Double): boolean; override;
  published
  end;

  tcyPieGauge = class(TcyCustomPieGauge)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // TcyBaseMeasure properties :
    property MarginBottom;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    property Color;
    property ItemsCount;
    property ItemsSpacing;
    property ItemsWidth;
    property ItemsHeight;
    property Min;
    property Max;
    property ReadOnly;
    property Smooth;
    property Transparent;
    property OnBeforePaint;
    property OnAfterPaint;
    // TcyCustomGauge properties :
    property Bevels;
    property Position;
    property Precision;
    property Step;
    property OnChange;
    property OnCustomDrawItem;
    // TcyCustomPieGauge properties :
    property AngleMin;
    property AngleMax;
  end;

implementation

{ TcyCustomPieGauge }

constructor TcyCustomPieGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngleMin := 0;
  FAngleMax := 180;
  Width := 200;
  Height := 150;
end;

procedure TcyCustomPieGauge.SetAngleMax(const Value: Integer);
begin
  FAngleMax := Value;
end;

procedure TcyCustomPieGauge.SetAngleMin(const Value: Integer);
begin
  FAngleMin := Value;
end;

procedure TcyCustomPieGauge.Draw(Rect: TRect; FullRepaint: Boolean);
begin
  if not Transparent
  then begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;

  Bevels.DrawBevels(Canvas, Rect, false);
  Rect := classes.Rect(Rect.Left + MarginLeft, Rect.Top + MarginTop,
                        Rect.Right - MarginRight, Rect.Bottom - MarginBottom);
                        
end;

function TcyCustomPieGauge.GetValueFromPos(fromPoint: TPoint; var Value: Double): boolean;
begin
  RESULT := false;


end;

end.
