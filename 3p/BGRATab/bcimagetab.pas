{
Copyright (C) 2006-2020 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit BCImageTab;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  {$IFDEF FPC}{$ifdef Windows}Windows,{$endif}LCLType, LResources, LMessages,{$ENDIF} ExtCtrls,
  Types,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  { BGRAControls }
  BCBaseCtrls, BCEffect,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes, BGRASliceScaling, BCImageButton;

{off $DEFINE DEBUG}

type

  { TBCCustomImageTab }

  TBCCustomImageTab = class(TBCCustomImageButton)
  private
    FGroupIndex: Integer;

    function GetFPressed: boolean;
    procedure SetFPressed(AValue: boolean);
    procedure SetGroupIndex(AValue: Integer);
    procedure UpdateExclusive;
  protected
    procedure CMButtonPressed(var {%H-}Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_BUTTONPRESSED; {$IFDEF FPC}virtual;{$ENDIF}
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;

    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Pressed: boolean read GetFPressed write SetFPressed default False;
  end;

  TBCImageTab = class(TBCCustomImageTab)
  published
    property AlphaTest;
    property AlphaTestValue;
    property Action;
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    property BidiMode;
    property BitmapFile;
    property BitmapOptions;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
    property ModalResult;
    {$IFDEF FPC}
    property OnChangeBounds;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TextVisible;
    property Toggle;
    property Pressed;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls',[TBCImageTab]);
end;

{ TBCCustomImageTab }

procedure TBCCustomImageTab.SetFPressed(AValue: boolean);
begin
  if Self.Toggle and (FGroupIndex <> 0) then
  begin

    inherited Pressed := AValue;

    if AValue then
      UpdateExclusive;
  end;
end;

function TBCCustomImageTab.GetFPressed: boolean;
begin
  Result := inherited Pressed;
end;

procedure TBCCustomImageTab.SetGroupIndex(AValue: Integer);
begin
  if FGroupIndex <> AValue then
  begin
    FGroupIndex := AValue;
    UpdateExclusive;
  end;
end;

procedure TBCCustomImageTab.UpdateExclusive;
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

procedure TBCCustomImageTab.CMButtonPressed(var Message: TLMessage);
var
  Sender: TBCCustomImageButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TBCCustomImageTab(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Pressed and Self.Pressed then
      begin
        Self.Pressed := False;
        Invalidate;
      end;
    end;
  end;
end;

procedure TBCCustomImageTab.Click;
var
  oldPressed: boolean;
begin
  oldPressed := Self.Pressed;

  inherited Click;

  if (FGroupIndex <> 0) and (Self.Toggle) then
  begin
    if not(oldPressed) then
      Self.Pressed := not oldPressed
    else
      Self.Pressed := oldPressed;
  end;
end;

constructor TBCCustomImageTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGroupIndex := 0;
end;

end.

