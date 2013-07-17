{   Component(s):
    tcyColorMatrix

    Description:
    It's a color matrix representation !!!
    This component allow best performance than TcyColorGrid
    despite of non published ColorList property ...

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

unit cyColorMatrix;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Types, Controls, Graphics, Messages, ExtCtrls, VCL.cyTypes, VCL.cyClasses, cyBaseColorMatrix;

type
  TcyCustomColorMatrix = class(TcyBaseColorMatrix)
  private
    FBevels: TcyBevels;
    FWallpaper: TcyBgPicture;
    procedure SetBevels(const Value: TcyBevels);
    procedure BevelsChange(Sender: TObject);
    procedure SetWallpaper(const Value: TcyBgPicture);
  protected
    procedure DrawBackground(Rect: TRect); override;
    function GetBorderSize: Integer; override;
    property Bevels: TcyBevels read FBevels write SetBevels;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyColorMatrix = class(TcyCustomColorMatrix)
  private
  protected
  public
    property Canvas;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseActivate; {$ENDIF}
    property OnMouseDown;
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseEnter; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseLeave; {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // Herited from TcyBaseColorMatrix:
    property Background;
    property BorderWidth;
    property CellHeight;
    property CellWidth;
    property ColCount;
    property CellFrameColor;
    property CellFrameWidth;
    property CellSpacingWidth;
    property CellSpacingHeight;
    property DefaultColor;
    property TopRowValue;
    property LeftColumnValue;
    property BottomRowValue;
    property RightColumnValue;
    property RowCount;
    property OnCustomDrawCell;
    property OnCellClick;
    property OnPaint;
    // Herited from TcyCustomColorMatrix:
    property Bevels;
    property Wallpaper;
  end;

implementation

{ TcyCustomColorMatrix }

constructor TcyCustomColorMatrix.Create(AOwner: TComponent);
begin
  inherited;
  FBevels := TcyBevels.Create(self, TcyBevel);
  FBevels.OnChange := BevelsChange;
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := BackgroundChanged;
end;

destructor TcyCustomColorMatrix.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;
  FWallpaper.Free;
  inherited;
end;

procedure TcyCustomColorMatrix.DrawBackground(Rect: TRect);
begin
  Inherited DrawBackground(Rect);

  // Draw wallpaper:
  if Assigned(FWallpaper) then
  cyDrawBgPicture(Canvas, Rect, FWallpaper);

  // Draw bevels:
  if Assigned(FBevels) then
    FBevels.DrawBevels(Canvas, Rect, false);
end;

procedure TcyCustomColorMatrix.BevelsChange(Sender: TObject);
begin
  // if FBevels.NeedOwnerRealign then  Doesn' t work at design-time
  if not (csLoading in ComponentState) then
    AdjustSize;
end;

procedure TcyCustomColorMatrix.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

procedure TcyCustomColorMatrix.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

function TcyCustomColorMatrix.GetBorderSize: Integer;
begin
  RESULT := Inherited GetBorderSize;

  if Assigned(FBevels) then
    Inc(Result, FBevels.BevelsWidth);
end;

end.
