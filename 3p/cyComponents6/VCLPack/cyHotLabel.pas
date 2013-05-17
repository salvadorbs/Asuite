{   Component(s):
    tcyHotLabel

    Description:
    With TcyLabel features and responding to the mouse events like a link

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

unit cyHotLabel;

interface

uses VCL.cyClasses, cyBaseLabel, Windows, Classes, ExtCtrls, Controls, Messages, Graphics;

type
  TcyCustomHotLabel = class(TcyBaseLabel)
  private
    FBevels: TcyBevels;
    FlabelFocused: Boolean;
    FlabelDown: Boolean;
    FFontEnter: TFont;
    FFontLeave: TFont;
    FFontMouseDown: TFont;
    FDegradeEnter: TcyGradient;
    FDegradeLeave: TcyGradient;
    FDegradeMouseDown: TcyGradient;
    FShadowEnter: TcyShadowText;
    FShadowMouseDown: TcyShadowText;
    FShadowLeave: TcyShadowText;
    procedure BevelsChange(Sender: TObject);
    procedure SetBevels(const Value: TcyBevels);
    procedure SetFontEnter(AValue: TFont);
    procedure SetFontLeave(AValue: TFont);
    procedure SetFontMouseDown(AValue: TFont);
    procedure CmMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetDegradeEnter(const Value: TcyGradient);
    procedure SetDegradeLeave(const Value: TcyGradient);
    procedure SetDegradeMouseDown(const Value: TcyGradient);
    procedure SetShadowEnter(const Value: TcyShadowText);
    procedure SetShadowLeave(const Value: TcyShadowText);
    procedure SetShadowMouseDown(const Value: TcyShadowText);
  protected
    procedure DrawBackground(aRect: TRect); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;   // Call OnMouseUp procedure ...
    procedure DegradeEnterChanged(Sender: TObject);
    procedure DegradeLeaveChanged(Sender: TObject);
    procedure DegradeMouseDownChanged(Sender: TObject);
    procedure FontEnterChanged(Sender: TObject);
    procedure FontLeaveChanged(Sender: TObject);
    procedure FontMouseDownChanged(Sender: TObject);
    procedure ShadowEnterChanged(Sender: TObject);
    procedure ShadowLeaveChanged(Sender: TObject);
    procedure ShadowMouseDownChanged(Sender: TObject);
    property Bevels: TcyBevels read FBevels write SetBevels;
    property DegradeEnter: TcyGradient read FDegradeEnter write SetDegradeEnter;
    property DegradeLeave: TcyGradient read FDegradeLeave write SetDegradeLeave;
    property DegradeMouseDown: TcyGradient read FDegradeMouseDown write SetDegradeMouseDown;
    property FontEnter: TFont read FFontEnter write SetFontEnter;
    property FontLeave: TFont read FFontLeave write SetFontLeave;
    property FontMouseDown: TFont read FFontMouseDown write SetFontMouseDown;
    property ShadowEnter: TcyShadowText read FShadowEnter write SetShadowEnter;
    property ShadowLeave: TcyShadowText read FShadowLeave write SetShadowLeave;
    property ShadowMouseDown: TcyShadowText read FShadowMouseDown write SetShadowMouseDown;
    procedure PreviewEnter;
    procedure PreviewLeave;
    procedure PreviewMouseDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyHotLabel = class(TcyCustomHotLabel)
  private
  protected
  public
    property Canvas;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
//    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
//    property Font;
    property FocusControl;
    property ParentBiDiMode;
//    property ParentColor;
//    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
    // Herited from TcyBaseLabel :
//    property Shadow;
    property Animation;
    property CaptionIndentLeft;
    property CaptionIndentRight;
    property CaptionIndentTop;
    property CaptionIndentBottom;
    property CaptionRender;
    property CaptionOrientation;
    property OnPaint;
    // Herited from TcyCustomHotLabel :
    property Bevels;
    property DegradeEnter;
    property DegradeLeave;
    property DegradeMouseDown;
    property FontEnter;
    property FontLeave;
    property FontMouseDown;
    property ShadowEnter;
    property ShadowLeave;
    property ShadowMouseDown;
  end;

implementation

uses StdCtrls;

constructor TcyCustomHotLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBevels := TcyBevels.Create(self, TcyBevel);
  FBevels.OnChange := BevelsChange;

  FDegradeEnter := TcyGradient.Create(self);
  FDegradeEnter.OnChange := DegradeEnterChanged;
  FDegradeLeave := TcyGradient.Create(self);
  FDegradeLeave.OnChange := DegradeLeaveChanged;
  FDegradeMouseDown := TcyGradient.Create(self);
  FDegradeMouseDown.OnChange := DegradeMouseDownChanged;

  FFontLeave := TFont.Create;
  FFontLeave.OnChange := FontLeaveChanged;
  FFontEnter := TFont.Create;
  FFontEnter.Size := FFontLeave.Size + 2;
  FFontEnter.OnChange := FontEnterChanged;
  FFontMouseDown := TFont.Create;
  FFontMouseDown.Size := FFontLeave.Size - 2;
  FFontMouseDown.OnChange := FontMouseDownChanged;

  FShadowEnter := TcyShadowText.Create(self);
  FShadowEnter.OnChange := ShadowEnterChanged;
  FShadowLeave := TcyShadowText.Create(self);
  FShadowLeave.OnChange := ShadowLeaveChanged;
  FShadowMouseDown := TcyShadowText.Create(self);
  FShadowMouseDown.OnChange := ShadowMouseDownChanged;

  Cursor := crHandPoint;
end;

destructor TcyCustomHotLabel.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;

  FDegradeEnter.Free;
  FDegradeLeave.Free;
  FDegradeMouseDown.Free;

  FFontEnter.Free;
  FFontLeave.Free;
  FFontMouseDown.Free;

  FShadowEnter.Free;
  FShadowLeave.Free;
  FShadowMouseDown.Free;

  inherited Destroy;
end;

procedure TcyCustomHotLabel.Loaded;
begin
  Inherited;
  FlabelFocused := false;
  FlabelDown := false;
  Font.Assign(FFontLeave);
  Shadow.Assign(FShadowLeave);
end;

procedure TcyCustomHotLabel.PreviewEnter;
begin
  FlabelFocused := true;
  FlabelDown := false;
  Font.Assign(FFontEnter);
  Shadow.Assign(FShadowEnter);
  Invalidate;
end;

procedure TcyCustomHotLabel.PreviewLeave;
begin
  FlabelFocused := false;
  FlabelDown := false;
  Font.Assign(FFontLeave);
  Shadow.Assign(FShadowLeave);
  Invalidate;
end;

procedure TcyCustomHotLabel.PreviewMouseDown;
begin
  FlabelFocused := true;
  FlabelDown := true;
  Font.Assign(FFontMouseDown);
  Shadow.Assign(FShadowMouseDown);
  Invalidate;
end;

procedure TcyCustomHotLabel.BevelsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomHotLabel.DegradeEnterChanged(Sender: TObject);
begin
  if csDesigning in ComponentState  // Visualize modifications at design time on IDE
  then
    PreviewEnter
  else
    if (FlabelFocused) And (not FlabelDown)
    then Invalidate;
end;

procedure TcyCustomHotLabel.DegradeLeaveChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewLeave
  else
    if not FlabelFocused
    then Invalidate;
end;

procedure TcyCustomHotLabel.DegradeMouseDownChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewMouseDown
  else
    if FlabelFocused and FlabelDown
    then Invalidate;
end;

procedure TcyCustomHotLabel.FontEnterChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewEnter
  else
    if (FlabelFocused) and (not FlabelDown)
    then Font.Assign(FFontEnter);
end;

procedure TcyCustomHotLabel.FontLeaveChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewLeave
  else
    if not FlabelFocused
    then Font.Assign(FFontLeave);
end;

procedure TcyCustomHotLabel.FontMouseDownChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewMouseDown
  else
    if FlabelFocused And FlabelDown
    then Font.Assign(FFontMouseDown);
end;

procedure TcyCustomHotLabel.ShadowEnterChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewEnter
  else
    if (FlabelFocused) and (not FlabelDown)
    then Shadow.Assign(FShadowEnter);
end;

procedure TcyCustomHotLabel.ShadowLeaveChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewLeave
  else
    if not FlabelFocused
    then Shadow.Assign(FShadowLeave);
end;

procedure TcyCustomHotLabel.ShadowMouseDownChanged(Sender: TObject);
begin
  if csDesigning in ComponentState
  then
    PreviewMouseDown
  else
    if FlabelFocused And FlabelDown
    then Shadow.Assign(FShadowMouseDown);
end;

procedure TcyCustomHotLabel.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

procedure TcyCustomHotLabel.SetDegradeEnter(const Value: TcyGradient);
begin
  FDegradeEnter.Assign(Value);
end;

procedure TcyCustomHotLabel.SetDegradeLeave(const Value: TcyGradient);
begin
  FDegradeLeave.Assign(Value);
end;

procedure TcyCustomHotLabel.SetDegradeMouseDown(const Value: TcyGradient);
begin
  FDegradeMouseDown.Assign(Value);
end;

procedure TcyCustomHotLabel.SetFontEnter(AValue: TFont);
begin
  FFontEnter.Assign(AValue);
end;

procedure TcyCustomHotLabel.SetFontLeave(AValue: TFont);
begin
  FFontLeave.Assign(AValue);
end;

procedure TcyCustomHotLabel.SetFontMouseDown(AValue: TFont);
begin
  FFontMouseDown.Assign(AValue);
end;

procedure TcyCustomHotLabel.SetShadowEnter(const Value: TcyShadowText);
begin
  FShadowEnter := Value;
end;

procedure TcyCustomHotLabel.SetShadowLeave(const Value: TcyShadowText);
begin
  FShadowLeave := Value;
end;

procedure TcyCustomHotLabel.SetShadowMouseDown(const Value: TcyShadowText);
begin
  FShadowMouseDown := Value;
end;

procedure TcyCustomHotLabel.MouseDown(Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  FlabelDown := true;
  FlabelFocused := true;
  Font.Assign(FFontMouseDown);
  Shadow.Assign(FShadowMouseDown);
  inherited;
end;

procedure TcyCustomHotLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FlabelDown := false;

  if FlabelFocused
  then begin
    Font.Assign(FFontEnter);
    Shadow.Assign(FShadowEnter);
  end
  else begin
    Font.Assign(FFontLeave);
    Shadow.Assign(FShadowLeave);
  end;

  inherited;
end;

procedure TcyCustomHotLabel.CmMouseEnter(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState)
  then begin
    FlabelFocused := true;

    if FlabelDown
    then begin
      Font.Assign(FFontMouseDown);
      Shadow.Assign(FShadowMouseDown);
    end
    else begin
      Font.Assign(FFontEnter);
      Shadow.Assign(FShadowEnter);
    end;
  end;

  inherited;
end;

procedure TcyCustomHotLabel.CmMouseLeave(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState)
  then begin
    FlabelFocused := false;
    Font.Assign(FFontLeave);
    Shadow.Assign(FShadowLeave);
  end;
  
  inherited;
end;

procedure TcyCustomHotLabel.DrawBackground(aRect: TRect);
var aDegrade: TcyGradient;
begin
  if not Transparent
  then begin
    if FlabelFocused
    then begin
      if FlabelDown
      then aDegrade := FDegradeMouseDown
      else aDegrade := FDegradeEnter;
    end
    else
      aDegrade := FDegradeLeave;

    aDegrade.Draw(Canvas, aRect);
  end;

  FBevels.DrawBevels(Canvas, aRect, false);
end;

end.
