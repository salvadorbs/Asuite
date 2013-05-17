{   Component(s):
    tcySkinButton

    Description:
    A TGraphicControl that you personnalise with several bitmaps
    representing his different states.

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

unit cySkinButton;

interface

uses Classes, Types, Controls, Graphics, Messages, Windows, VCL.cyGraphics;

type
  TcySkinButton = class(TGraphicControl)
  private
    FPicDown: TPicture;
    FPicDownMouseOver: TPicture;
    FPicDownMouseDown: TPicture;
    FPicMouseDown: TPicture;
    FPicNormal: TPicture;
    FPicMouseOver: TPicture;
    FptMouseDown: TPoint;
    FPicDisabled: TPicture;
    FDown: Boolean;
    FGroupIndex: Integer;
    FAllowAllUp: Boolean;
    FStretch: Boolean;
    FMouseDown: Boolean;
    FMouseOver: Boolean;
    FTransparent: Boolean;
    procedure UpdateExclusive;
    procedure SetPicNormal(Value: TPicture);
    procedure SetPicDown(Value: TPicture);
    procedure SetPicMouseOver(Value: TPicture);
    procedure SetPicDownMouseOver(Value: TPicture);
    procedure SetPicMouseDown(Value: TPicture);
    procedure SetPicDisabled(Value: TPicture);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetStretch(Value: Boolean);
    procedure ForceMouseDown(aValue: Boolean);
    procedure ForceMouseOver(aValue: Boolean);
    function  IsSolidColor(aPoint: TPoint): Boolean;
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure SetTransparent(const Value: Boolean);
    procedure SetPicDownMouseDown(const Value: TPicture);
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Let delphi developpers read/change mouse satus:
    property MouseIsDown: Boolean read FMouseDown write ForceMouseDown;     // Mouse down in bitmap area ?
    property MouseIsOver: Boolean read FMouseOver write ForceMouseOver;  // Mouse entered in bitmap area ?
  published
    property Align;
    property Anchors;
    property Constraints;     
    property Enabled;
    property Height default 40;
    property Width default 40;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property PicDown: TPicture read FPicDown write SetPicDown;
    property PicDownMouseOver: TPicture read FPicDownMouseOver write SetPicDownMouseOver;
    property PicDownMouseDown: TPicture read FPicDownMouseDown write SetPicDownMouseDown;
    property PicNormal: TPicture read FPicNormal write SetPicNormal;
    property PicMouseOver: TPicture read FPicMouseOver write SetPicMouseOver;
    property PicMouseDown: TPicture read FPicMouseDown write SetPicMouseDown;
    property PicDisabled: TPicture read FPicDisabled write SetPicDisabled;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default false;
    property Stretch: Boolean read FStretch write SetStretch default false;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
  end;

implementation

constructor TcySkinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicNormal := TPicture.Create;
  FPicDown := TPicture.Create;
  FPicMouseOver := TPicture.Create;
  FPicDownMouseOver := TPicture.Create;
  FPicDownMouseDown := TPicture.Create;
  FPicMouseDown := TPicture.Create;
  FPicDisabled := TPicture.Create;
  FDown := false;
  FAllowAllUp := false;
  FGroupIndex := 0;
  FMouseDown := false;
  FMouseOver := false;
  FStretch:= false;
  FTransparent := true;
  Height := 40;
  Width := 40;
end;

destructor TcySkinButton.Destroy;
begin
  FPicNormal.free;
  FPicDown.free;
  FPicMouseOver.free;
  FPicDownMouseOver.free;
  FPicDownMouseDown.free;
  FPicMouseDown.free;
  FPicDisabled.free;

  inherited Destroy;
end;

procedure TcySkinButton.Click;
begin
  if IsSolidColor(FptMouseDown)
  then begin
    if FGroupIndex <> 0
    then Down := not FDown;      // Change down state

    Inherited;
  end;
end;

procedure TcySkinButton.Paint;
var
  Painted: Boolean;
  Rect: TRect;

    procedure PaintWith(const aPic: TPicture);
    begin
      if aPic.Graphic = Nil then EXIT;

      Painted := true;

      if aPic.Graphic.Transparent <> FTransparent
      then aPic.Graphic.Transparent := FTransparent;

      if FStretch
      then begin
        if aPic.Graphic is TIcon   // Stretch draw doesn't work for icons!
        then DrawIconEx(Canvas.Handle, Rect.Left, Rect.Top, aPic.Icon.Handle, Rect.Right-Rect.Left, Rect.Bottom-Rect.Top, 0, 0, DI_Normal)
        else Canvas.StretchDraw(Rect, aPic.Graphic);
      end
      else
        Canvas.Draw(Rect.Left, Rect.Top, aPic.Graphic);
    end;

begin
  Painted := false;
  Rect := ClientRect;

  if csDesigning in ComponentState
  then begin
    canvas.Pen.Mode := pmNot;
    canvas.Brush.Style := bsClear;
    canvas.Rectangle(Rect);
    canvas.Brush.Style := bsSolid;
  end;

  if Enabled
  then begin
    // Button clicked picture :
    if FMouseDown
    then
      if Down
      then PaintWith(FPicDownMouseDown)
      else PaintWith(FPicMouseDown);

    // Mouse over pictures :
    if (not Painted) and (FMouseOver)
    then
      if FDown
      then PaintWith(FPicDownMouseOver)
      else PaintWith(FPicMouseOver);

    // Down state picture :
    if (not Painted) and (FDown or FMouseDown)
    then PaintWith(FPicDown);
  end
  else
    PaintWith(FPicDisabled);

  if not Painted
  then PaintWith(FPicNormal);
end;

procedure TcySkinButton.SetPicNormal(Value: TPicture);
begin
  FPicNormal.Assign(Value);

  if FPicNormal.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicNormal.Graphic.Width;
      Height := FPicNormal.Graphic.Height;
    end;

    if Enabled and (not FDown) and (not FMouseDown) and (not FMouseOver)
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetPicMouseOver(Value: TPicture);
begin
  FPicMouseOver.Assign(Value);

  if FPicMouseOver.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicMouseOver.Graphic.Width;
      Height := FPicMouseOver.Graphic.Height;
    end;

    if Enabled and (not FDown) and (FMouseOver)
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetPicDownMouseDown(const Value: TPicture);
begin
  FPicDownMouseDown.Assign(Value);

  if FPicDownMouseDown.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicDownMouseDown.Graphic.Width;
      Height := FPicDownMouseDown.Graphic.Height;
    end;

    if Enabled and (FDown) and (FMouseDown)
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetPicDownMouseOver(Value: TPicture);
begin
  FPicDownMouseOver.Assign(Value);

  if FPicDownMouseOver.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicDownMouseOver.Graphic.Width;
      Height := FPicDownMouseOver.Graphic.Height;
    end;

    if Enabled and (FDown) and (FMouseOver)
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetPicDown(Value: TPicture);
begin
  FPicDown.Assign(Value);

  if FPicDown.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicDown.Graphic.Width;
      Height := FPicDown.Graphic.Height;
    end;

    if Enabled and (FDown) and (not FMouseDown)
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetPicMouseDown(Value: TPicture);
begin
  FPicMouseDown.Assign(Value);

  if FPicMouseDown.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicMouseDown.Graphic.Width;
      Height := FPicMouseDown.Graphic.Height;
    end;

    if Enabled and (not FDown) and (FMouseDown)
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetPicDisabled(Value: TPicture);
begin
  FPicDisabled.Assign(Value);

  if FPicDisabled.Graphic <> Nil
  then begin
    // Resize component :
    if not FStretch
    then begin
      Width  := FPicDisabled.Graphic.Width;
      Height := FPicDisabled.Graphic.Height;
    end;

    if not Enabled
    then Invalidate;
  end;
end;

procedure TcySkinButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;

  if value <> FDown
  then begin
    if FDown and (not FAllowAllUp) then Exit;

    FDown := Value;
    Invalidate;
    if Value then UpdateExclusive;   // Envoyer un message pour relever l' autre ...
  end;
end;

procedure TcySkinButton.SetStretch(Value: Boolean);
begin
  if value <> FStretch
  then begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TcySkinButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TcySkinButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value
  then begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TcySkinButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value
  then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TcySkinButton.UpdateExclusive;
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

procedure TcySkinButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TcySkinButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TcySkinButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TcySkinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseOver <> IsSolidColor(Point(X, Y))
  then begin
    FMouseOver := not FMouseOver;
    Invalidate;
  end;

  inherited;
end;

procedure TcySkinButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FptMouseDown := Point(X, Y);

  if IsSolidColor(FptMouseDown)
  then begin
    FMouseDown := true;
    Invalidate;
  end;

  inherited;
end;

procedure TcySkinButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown
  then begin
    FMouseDown := false;
    Invalidate;
  end;

  inherited;
end;

procedure TcySkinButton.CmMouseLeave(var Msg: TMessage);
begin
  if FMouseOver
  then begin
    FMouseOver := false;
    Invalidate;
  end;

  inherited;
end;

procedure TcySkinButton.ForceMouseDown(aValue: Boolean);
begin
  if aValue <> FMouseDown
  then begin
    FMouseDown := aValue;
    Invalidate;
  end;
end;

procedure TcySkinButton.ForceMouseOver(aValue: Boolean);
begin
  if aValue <> FMouseOver
  then begin
    FMouseOver := aValue;
    Invalidate;
  end;
end;

function TcySkinButton.IsSolidColor(aPoint: TPoint): Boolean;
var
  CurrentFound: Boolean;
  curPic: TPicture;

    procedure SetCurrentPicture(const aPic: TPicture);
    begin
      if aPic.Graphic <> Nil
      then begin
        CurrentFound := true;
        curPic := aPic;
      end;
    end;

begin
  RESULT := false;

  // Get current picture :
  CurrentFound := false;

  if Enabled
  then begin
    // Button clicked picture :
    if FMouseDown
    then
      if FDown
      then SetCurrentPicture(FPicDownMouseDown)
      else SetCurrentPicture(FPicMouseDown);

    // Mouse over pictures :
    if (not CurrentFound) and (FMouseOver)
    then
      if FDown
      then SetCurrentPicture(FPicDownMouseOver)
      else SetCurrentPicture(FPicMouseOver);

    // Down state picture :
    if (not CurrentFound) and (FDown or FMouseDown)
    then SetCurrentPicture(FPicDown);
  end
  else
    SetCurrentPicture(FPicDisabled);

  if not CurrentFound
  then SetCurrentPicture(FPicNormal);

  if CurrentFound
  then begin
    if not FTransparent
    then begin
      RESULT := true;
      Exit;
    end;

    // Get graphic pixel coord :
    if FStretch
    then begin
      aPoint.x := (aPoint.x * curPic.Graphic.Width) div Width;
      aPoint.y := (aPoint.y * curPic.Graphic.Height) div Height;
    end;

    if (aPoint.X < curPic.Width) and (aPoint.y < curPic.Height)
    then RESULT := not PictureIsTransparentAtPos(curPic, aPoint);
  end;
end;

end.
