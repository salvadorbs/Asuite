{   Component(s):
    tcyLabel

    Description:
    A label with degrade effect, text orientation and ellipsis mode

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

unit cyLabel;

interface

uses VCL.cyClasses, cyBaseLabel, StdCtrls, Graphics, classes, Windows, Controls;

type
  TcyCustomLabel = class(TcyBaseLabel)
  private
    FBevels: TcyBevels;
    FDegrade: TcyGradient;
    procedure BevelsChange(Sender: TObject);
    procedure SetBevels(const Value: TcyBevels);
    procedure SetDegrade(const Value: TcyGradient);
  protected
    procedure DrawBackground(aRect: TRect); override;
    property Bevels: TcyBevels read FBevels write SetBevels;
    property Degrade: TcyGradient read FDegrade write SetDegrade;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyLabel = class(TcyCustomLabel)
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
    property Font;
    property FocusControl;
    property ParentBiDiMode;
//    property ParentColor;
    property ParentFont;
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
    property Animation;
    property CaptionIndentLeft;
    property CaptionIndentRight;
    property CaptionIndentTop;
    property CaptionIndentBottom;
    property CaptionRender;
    property CaptionOrientation;
    property Shadow;
    property OnPaint;
    // Herited from TcyCustomLabel:
    property Bevels;
    property Degrade;
  end;

implementation

uses Types;

constructor TcyCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBevels := TcyBevels.Create(self, TcyBevel);
  FBevels.OnChange := BevelsChange;

  FDegrade := TcyGradient.Create(self);
  FDegrade.OnChange := SubPropertiesChanged;
end;

destructor TcyCustomLabel.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;

  FDegrade.Free;
  inherited Destroy;
end;

procedure TcyCustomLabel.BevelsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomLabel.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

procedure TcyCustomLabel.SetDegrade(const Value: TcyGradient);
begin
  FDegrade.Assign(Value);
end;

procedure TcyCustomLabel.DrawBackground(aRect: TRect);
begin
  if Transparent
  then begin
    if csDesigning in ComponentState
    then begin
      // Draw frame in order to see the component at design time :
      Canvas.Pen.Style := psDash;
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
    end;
  end
  else
    FDegrade.Draw(Canvas, aRect);

  FBevels.DrawBevels(Canvas, aRect, false);
end;

end.
