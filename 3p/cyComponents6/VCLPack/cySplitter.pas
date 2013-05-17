{   Component(s):
    tcySplitter

    ************************ IMPORTANT ************************
    *  This component source code was copied from original    *
    *  Delphi TSplitter component (unit 'ExtCtrls').          *
    *  Few modifications were made to correspond to what      *
    *  i wanted to do.                                        *
    ***********************************************************

    Description:
    ResizeMode = rmClassic: Same as TSplitter component fonctionalities
    ResizeMode = rmParent: TcySplitter manipulates his parent control instead of
    manipulating the nearest control in the same parent control.

    So, why another Splitter component?
    - Because some controls like TCategoryPanelGroup (Delphi 2009) don't accept components between
      panels and i want to resize each TCategoryPanels with a splitter component.
      Just put a TcySplitter component aligned to bottom on each TCategoryPanel
      and set ResizeSide property to rmParent.
    - TcySplitter doesn' t need to be aligned when property ResizeMode = rmParent.
    - You can specify ResizeSide property to manually define splitter job.
    - Visual features added: Bevels, Gradient, hot and grip.
    - You can define handled control with OnDetermineHandledControl event.

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

unit cySplitter;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Graphics, Windows, Types, Messages, Controls, ExtCtrls, Forms, VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics;

type
  TcySplitter = class;

  TGripStyle = (gsNone, gsRaisedRect, gsLoweredRect, gsRaisedLine, gsLoweredLine);

  TGrip=class(TPersistent)
  private
    FOwner: TcySplitter;
    FHeight: Integer;
    FWidth: Integer;
    FCount: Integer;
    FSpacing: Integer;
    FStyle: TGripStyle;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetStyle(const Value: TGripStyle);
    procedure DrawRects(Rect: TRect; HighLightColor, ShadowColor: TColor);
    procedure DrawLines(Rect: TRect; HighLightColor, ShadowColor: TColor);
  protected
    procedure PropertiesChanged(Sender: TObject);
    procedure DrawGrip(Rect: TRect; fromColor, toColor: TColor);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property Count: Integer read FCount write SetCount default 6;
    property Height: Integer read FHeight write SetHeight default 3;
    property Width: Integer read FWidth write SetWidth default 3;
    property Spacing: Integer read FSpacing write SetSpacing default 1;
    property Style: TGripStyle read FStyle write SetStyle default gsRaisedRect;
  end;

  TProcOnDetermineHandledControl = procedure (Sender: TObject; var HandledControl: TControl) of object;
  TMaxSizeRule = (mrDefault, mrNoRules);
  TResizeMode = (rmClassic, rmParent);
  TResizeSide = (rsLeft, rsTop, rsRight, rsBottom);

  TcySplitter = class(TGraphicControl)
  private
    FOldAlign: TAlign;
    FHot: Boolean;
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBevels: TcyBevels;
    FBrush: TBrush;
    FhandledControl: TControl;
    FDrawMotionControl: TWinControl;
    FDegrade: TcyGradient;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FResizeSide: TResizeSide;
    FOnDetermineHandledControl: TProcOnDetermineHandledControl;
    FDegradeHot: TcyGradient;
    FResizeMode: TResizeMode;
    FGrip: TGrip;
    FMaxSizeRule: TMaxSizeRule;
    FWallpaper: TcyBgPicture;
    procedure CmMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    procedure DetermineHandledControl;
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure UpdateControlSize;
    procedure UpdateResizeSide;
    procedure SetBevels(const Value: TcyBevels);
    procedure SetDegrade(const Value: TcyGradient);
    procedure SetResizeSide(const Value: TResizeSide);
    procedure SetDegradeHot(const Value: TcyGradient);
    procedure SetGrip(const Value: TGrip);
    procedure SetResizeMode(const Value: TResizeMode);
    procedure SetWallpaper(const Value: TcyBgPicture);
  protected
    procedure ApplyDefaultMaxSizeRules; virtual;
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure SubPropertiesChanged(Sender: TObject);
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align;
    property Anchors;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default false;
    property Bevels: TcyBevels read FBevels write SetBevels;
//    property Color;
    property Cursor default crHSplit;
    property Constraints;
    property Degrade: TcyGradient read FDegrade write SetDegrade;
    property DegradeHot: TcyGradient read FDegradeHot write SetDegradeHot;
    property Grip: TGrip read FGrip write SetGrip;
    property Height default 100;
    property MaxSizeRule: TMaxSizeRule read FMaxSizeRule write FMaxSizeRule default mrDefault;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ResizeMode: TResizeMode read FResizeMode write SetResizeMode default rmParent;
//    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle
      default rsPattern;
    property ResizeSide: TResizeSide read FResizeSide write SetResizeSide default rsLeft;
    property Visible;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property Width default 10;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnDetermineHandledControl: TProcOnDetermineHandledControl
              read FOnDetermineHandledControl write FOnDetermineHandledControl;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;


implementation

{ TGrip }
constructor TGrip.Create(AOwner: TComponent);
begin
  FOwner   := TcySplitter(AOwner);
  FHeight  := 3;
  FWidth   := 3;
  FCount   := 6;
  FSpacing := 1;
  FStyle   := gsRaisedRect;
end;

destructor TGrip.Destroy;
begin
  inherited Destroy;
end;

procedure TGrip.PropertiesChanged(Sender: TObject);
begin
  if not (csLoading in FOwner.ComponentState)
  then FOwner.Repaint;
end;

procedure TGrip.SetCount(const Value: Integer);
begin
  FCount := Value;
  PropertiesChanged(nil);
end;

procedure TGrip.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  PropertiesChanged(nil);
end;

procedure TGrip.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  PropertiesChanged(nil);
end;

procedure TGrip.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  PropertiesChanged(nil);
end;

procedure TGrip.SetStyle(const Value: TGripStyle);
begin
  FStyle := Value;
  PropertiesChanged(nil);
end;

procedure TGrip.DrawGrip(Rect: TRect; fromColor, toColor: TColor);
var
  MedColor: TColor;
begin
  if FStyle <> gsNone
  then begin
    MedColor := MediumColor(fromColor, toColor);
    fromColor := ColorSetPercentBrightness(MedColor, 20);
    toColor := ColorSetPercentBrightness(MedColor, -20);

    case FStyle of
      gsRaisedRect:  DrawRects(Rect, fromColor, toColor);
      gsLoweredRect: DrawRects(Rect, toColor, fromColor);
      gsRaisedLine: DrawLines(Rect, fromColor, toColor);
      gsLoweredLine: DrawLines(Rect, toColor, fromColor);
    end;
  end;
end;

procedure TGrip.DrawRects(Rect: TRect; HighLightColor, ShadowColor: TColor);
var
  SpaceNeeded, StartPos, FixedPos, i: Integer;

      procedure DrawSingleRect(X, Y, aWidth, aHeight: Integer);
      var R: TRect;
      begin
        // Draw from StartPos :
        with FOwner do
        begin
          Canvas.Brush.Color := HighLightColor;
          R := classes.Rect(X, Y, X + aWidth-1, Y + aHeight-1);
          Canvas.FillRect(R);

          Canvas.Brush.Color := ShadowColor;
          OffsetRect(R, 1, 1);
          Canvas.FillRect(R);
        end;
      end;

begin
  if FOwner.FResizeSide in [rsTop, rsBottom]
  then begin
    SpaceNeeded := FCount * FWidth + (FCount-1) * FSpacing;
    StartPos := (Rect.Right - Rect.Left - SpaceNeeded) div 2;
    FixedPos := (Rect.Bottom - Rect.Top - FHeight) div 2;

    for i :=  1 to FCount do
    begin
      DrawSingleRect(StartPos, FixedPos, FWidth, FHeight);
      Inc(StartPos, FSpacing + FWidth);
    end;
  end
  else begin
    SpaceNeeded := FCount * FHeight + (FCount-1) * FSpacing;
    StartPos := (Rect.Bottom - Rect.Top - SpaceNeeded) div 2;
    FixedPos := (Rect.Right - Rect.Left - FWidth) div 2;

    for i :=  1 to FCount do
    begin
      DrawSingleRect(FixedPos, StartPos, FWidth, FHeight);
      Inc(StartPos, FSpacing + FHeight);
    end;
  end;
end;

procedure TGrip.DrawLines(Rect: TRect; HighLightColor, ShadowColor: TColor);
var
  R: TRect;
  SpaceNeeded, StartPos, w1, w2, FixedPos, i: Integer;
begin
  if FOwner.FResizeSide in [rsTop, rsBottom]
  then begin
    w1  := FHeight div 2;
    w2  := FHeight - w1;
    SpaceNeeded := FCount * FHeight + (FCount-1) * FSpacing;
    StartPos := (Rect.Bottom - Rect.Top - SpaceNeeded) div 2;
    FixedPos := (Rect.Right - Rect.Left - FWidth) div 2;

    for i :=  1 to FCount do
      with FOwner do
      begin
        Canvas.Brush.Color := HighLightColor;
        R := classes.Rect(FixedPos, StartPos, FixedPos + FWidth, StartPos + w1);
        Canvas.FillRect(R);
        Inc(StartPos, w1);

        Canvas.Brush.Color := ShadowColor;
        R := classes.Rect(FixedPos, StartPos, FixedPos + FWidth, StartPos + w2);
        Canvas.FillRect(R);
        Inc(StartPos, FSpacing + w2);
      end;
  end
  else begin
    w1  := FWidth div 2;
    w2  := FWidth - w1;
    SpaceNeeded := FCount * FWidth + (FCount-1) * FSpacing;
    StartPos := (Rect.Right - Rect.Left - SpaceNeeded) div 2;
    FixedPos := (Rect.Bottom - Rect.Top - FHeight) div 2;

    for i :=  1 to FCount do
      with FOwner do
      begin
        Canvas.Brush.Color := HighLightColor;
        R := classes.Rect(StartPos, FixedPos, StartPos + w1, FixedPos + FHeight);
        Canvas.FillRect(R);
        Inc(StartPos, w1);

        Canvas.Brush.Color := ShadowColor;
        R := classes.Rect(StartPos, FixedPos, StartPos + w2, FixedPos + FHeight);
        Canvas.FillRect(R);
        Inc(StartPos, FSpacing + w2);
      end;
  end;
end;

{ TcySplitter }
{$IF NOT DEFINED(CLR)}
type
  TWinControlAccess = class(TWinControl);
{$IFEND}

constructor TcySplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 100;
  Width := 10;
  Cursor := crHSplit;
  FAutoSnap := false;
  FHot := false;
  FMaxSizeRule := mrDefault;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
  FResizeMode := rmParent;
  FResizeSide := rsLeft;
  FDegrade := TcyGradient.Create(self);
  FDegrade.FromColor := $00C19788;
  FDegrade.ToColor := clNavy;
  FDegrade.OnChange := SubPropertiesChanged;
  FDegradeHot := TcyGradient.Create(self);
  FDegradeHot.FromColor := $00DFCAC1;
  FDegradeHot.ToColor := $00C19788;
  FDegradeHot.OnChange := SubPropertiesChanged;
  FBevels := TcyBevels.Create(self, VCL.cyClasses.TcyBevel);
  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then FBevels.Add.Style := bcRaised;

  FGrip := TGrip.Create(Self);
  FWallpaper := TcyBgPicture.Create(AOwner);
  FWallpaper.OnChange := SubPropertiesChanged;
end;

destructor TcySplitter.Destroy;
begin
  FWallpaper.Free;
  FGrip.Free;
  FBevels.Free;
  FBevels := Nil;
  FDegrade.Free;
  FDegradeHot.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TcySplitter.SubPropertiesChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TcySplitter.Loaded;
begin
  inherited;
  FOldAlign := Align;
end;

procedure TcySplitter.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

procedure TcySplitter.SetDegrade(const Value: TcyGradient);
begin
  FDegrade.Assign(Value);
end;

procedure TcySplitter.SetDegradeHot(const Value: TcyGradient);
begin
  FDegradeHot.Assign(Value);
end;

procedure TcySplitter.SetGrip(const Value: TGrip);
begin
  FGrip := Value;
end;

procedure TcySplitter.SetResizeMode(const Value: TResizeMode);
begin
  FResizeMode := Value;
  if not (csLoading in ComponentState)
  then UpdateResizeSide;
end;

procedure TcySplitter.SetResizeSide(const Value: TResizeSide);
var ChangeCursor: Boolean;
begin
  FResizeSide := Value;
  ChangeCursor := (Cursor = crHSplit) or (Cursor = crVSplit);
  Repaint;

  if not (csLoading in ComponentState)
  then
    case FResizeSide of
      rsLeft:
      begin
        if ChangeCursor
        then Cursor := crHSplit;

        if Align = alNone
        then Anchors := [akLeft]
        else Anchors := [];
      end;

      rsTop:
      begin
        if ChangeCursor
        then Cursor := crVSplit;

        if Align = alNone
        then Anchors := [akTop]
        else Anchors := [];
      end;

      rsRight:
      begin
        if ChangeCursor
        then Cursor := crHSplit;

        if Align = alNone
        then Anchors := [akRight]
        else Anchors := [];
      end;

      rsBottom:
      begin
        if ChangeCursor
        then Cursor := crVSplit;

        if Align = alNone
        then Anchors := [akBottom]
        else Anchors := [];
      end;
    end;
end;

procedure TcySplitter.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

procedure TcySplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  R: TRect;
begin
  R := ClientRect;

  // Draw gradient background and grip:
  if FHot
  then begin
    FDegradeHot.Draw(Canvas, R);
    FGrip.DrawGrip(R, FDegradeHot.FromColor, FDegradeHot.ToColor);
  end
  else begin
    FDegrade.Draw(Canvas, R);
    FGrip.DrawGrip(R, FDegrade.FromColor, FDegrade.ToColor);
  end;

  Bevels.DrawBevels(Canvas, R, false);
  cyDrawBgPicture(Canvas, R, FWallpaper);
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TcySplitter.CmMouseEnter(var Msg: TMessage);
begin
  FHot := true;
  Repaint;

  inherited;
end;

procedure TcySplitter.CmMouseLeave(var Msg: TMessage);
begin
  FHot := false;
  Repaint;

  inherited;
end;

procedure TcySplitter.DetermineHandledControl;
begin
  if FResizeMode = rmClassic
  then begin
    FhandledControl := FindControl;
  end
  else begin
    FhandledControl := Parent;

    // *** Exceptions *** //
    // TCategoryPanels on a TCategoryPanelGroup are not the parent, it is TCategoryPanelSurface component.
    // TCategoryPanelSurface.Parent are TCategoryPanel component
    {$IFDEF DELPHI2009_OR_ABOVE}
    if FhandledControl is TCategoryPanelSurface
    then FhandledControl := FhandledControl.Parent;
    {$ENDIF}
    // *** Exceptions *** //
  end;

  if Assigned(FOnDetermineHandledControl)
  then FOnDetermineHandledControl(Self, FhandledControl);
end;

// FindControl on the same parent (FResizeMode = rmClassic) :
function TcySplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  {$IFDEF DELPHI2009_OR_ABOVE}
    case Align of
      alLeft:
        if not AlignWithMargins then
          Dec(P.X)
        else
          Dec(P.X, Margins.Left + 1);
      alRight:
        if not AlignWithMargins then
          Inc(P.X, Width)
        else
          Inc(P.X, Width + Margins.Right + 1);
      alTop:
        if not AlignWithMargins then
          Dec(P.Y)
        else
          Dec(P.Y, Margins.Top + 1);
      alBottom:
        if not AlignWithMargins then
          Inc(P.Y, Height)
        else
          Inc(P.Y, Height + Margins.Bottom + 1);
    else
      Exit;
    end;
  {$ELSE}
    case Align of
      alLeft:   Dec(P.X);
      alRight:  Inc(P.X, Width);
      alTop:    Dec(P.Y);
      alBottom: Inc(P.Y, Height);
    else
      Exit;
    end;
  {$ENDIF}

  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;

      {$IFDEF DELPHI2009_OR_ABOVE}
      if Result.AlignWithMargins then
      begin
        Inc(R.Right, Result.Margins.Right);
        Dec(R.Left, Result.Margins.Left);
        Inc(R.Bottom, Result.Margins.Bottom);
        Dec(R.Top, Result.Margins.Top);
      end;
      {$ENDIF}

      if (R.Right - R.Left) = 0 then        // Hidden
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);                     // Hidden
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);

      if PtInRect(R, P)
      then begin
        P := Point(Left, Top);
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TcySplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);

  // Handle autosnap :
  if Result and (NewSize <= MinSize) and FAutoSnap
  then NewSize := 0;

{    if FResizeMode = rmParent
    then begin
      // Resize parent in order to see only the cySplitter:
      if FResizeSide in [rsLeft, rsRight]
      then NewSize := Width
      else NewSize := Height;
    end
    else
      NewSize := 0;  }
end;

function TcySplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

procedure TcySplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(0, 0);

  case FResizeSide of
    rsTop, rsBottom: P.Y := FSplit;    // Horizontal line
    rsLeft, rsRight: P.X := FSplit;    // Vertical line
  end;

  P := ClientToScreen(P);
  P := FDrawMotionControl.ScreenToClient(P);

  with P do
    PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

// TControl.RequestAlign is called in various source code locations like SetAlign procedure ...
procedure TcySplitter.RequestAlign;
begin
  inherited RequestAlign;

  if not (csLoading in ComponentState)
  then
    if FOldAlign <> Align  // Align property changed!
    then begin
      FOldAlign := Align;
      UpdateResizeSide;
    end;
end;

procedure TcySplitter.UpdateResizeSide;
begin
  if FResizeMode = rmClassic
  then begin
    case Align of
      alLeft: ResizeSide := rsRight;
      alTop: ResizeSide := rsBottom;
      alRight: ResizeSide := rsLeft;
      alBottom: ResizeSide := rsTop;
    end;
  end
  else
    case Align of
      alLeft: ResizeSide := rsLeft;
      alTop: ResizeSide := rsTop;
      alRight: ResizeSide := rsRight;
      alBottom: ResizeSide := rsBottom;
    end;
end;

procedure TcySplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(FDrawMotionControl.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
{$IF NOT DEFINED(LINUX)}
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$ELSE}
      FBrush.Color := clGray;
{$IFEND}
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TcySplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(FDrawMotionControl.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

procedure TcySplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft
  then begin
    FDownPos := Point(X, Y);
    // Control to be resized:
    DetermineHandledControl;

    if Assigned(FhandledControl)
    then begin
      if FHandledControl.Parent <> Nil
      then FDrawMotionControl := FHandledControl.Parent
      else FDrawMotionControl := TWinControl(FHandledControl);

      // Control FMaxSize value :
      if FMaxSizeRule = mrDefault
      then ApplyDefaultMaxSizeRules
      else FMaxSize := 0;              // No rules ...

      CalcSplitSize(X, Y, FNewSize, FSplit);
      AllocateLineDC;

      // Access to active control KeyDown to handle escape key :
      with ValidParentForm(Self) do
        if ActiveControl <> nil
        then begin
          FActiveControl := ActiveControl;
{$IF DEFINED(CLR)}
          FOldKeyDown := @FocusKeyDown;
          (FActiveControl as IControl).HookDelegate('OnKeyDown', @FOldKeyDown);
{$ELSE}
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
{$IFEND}
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TcySplitter.ApplyDefaultMaxSizeRules;
var I: Integer;
begin
  if FhandledControl.Parent = Nil // No parent
  then begin
    FMaxSize := 0;  // No rules ...
    EXIT;
  end;

  if FResizeSide in [rsLeft, rsRight]
  then begin
    FMaxSize := FhandledControl.Parent.ClientWidth;

    if FhandledControl.Align in [alLeft, alRight]
    then begin
      Dec(FMaxSize, FMinSize);

      for I := 0 to FhandledControl.Parent.ControlCount - 1 do
        with FhandledControl.Parent.Controls[I] do
          if Visible and (Align in [alLeft, alRight])
          then Dec(FMaxSize, FhandledControl.Parent.Controls[I].Width);

      Inc(FMaxSize, FhandledControl.Width);
    end
    else
      if FResizeSide = rsLeft
      then FMaxSize := FhandledControl.Left + FhandledControl.Width
      else Dec(FMaxSize, FhandledControl.Left);
  end
  else begin
    FMaxSize := FhandledControl.Parent.ClientHeight;

    if FhandledControl.Align in [alTop, alBottom]
    then begin
      Dec(FMaxSize, FMinSize);

      for I := 0 to FhandledControl.Parent.ControlCount - 1 do
        with FhandledControl.Parent.Controls[I] do
          if Visible and (Align in [alTop, alBottom])
          then Dec(FMaxSize, Height);

      Inc(FMaxSize, FhandledControl.Height);
    end
    else
      if FResizeSide = rsTop
      then FMaxSize := FhandledControl.Top + FhandledControl.Height
      else Dec(FMaxSize, FhandledControl.Top);
  end;
end;

procedure TcySplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  CalcSize: Integer;
begin
  if FResizeSide in [rsLeft, rsRight]
  then Split := X - FDownPos.X
  else Split := Y - FDownPos.Y;

  CalcSize := 0;
  case FResizeSide of
    rsLeft:   CalcSize := FhandledControl.Width - Split;
    rsRight:  CalcSize := FhandledControl.Width + Split;
    rsTop:    CalcSize := FhandledControl.Height - Split;
    rsBottom: CalcSize := FhandledControl.Height + Split;
  end;

  if CalcSize < FMinSize
  then begin
    if FResizeSide in [rsLeft, rsTop]
    then Inc(Split, CalcSize - FMinSize)
    else Inc(Split, FMinSize - CalcSize);

    CalcSize := FMinSize;
  end;

  if FMaxSize > 0   // Rules?
  then
    if CalcSize > FMaxSize
    then begin
      if FResizeSide in [rsLeft, rsTop]
      then Inc(Split, CalcSize - FMaxSize)
      else Inc(Split, FMaxSize - CalcSize);

      CalcSize := FMaxSize;
    end;

  NewSize := CalcSize;
end;

procedure TcySplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FHandledControl)
  then begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize)
    then begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TcySplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FHandledControl)
  then begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

procedure TcySplitter.UpdateControlSize;
var Sauv: Integer;
begin
  if FNewSize = FOldSize
  then EXIT;

  case FResizeSide of
    rsRight:
      begin
        // If we change Width to 0, the FhandledControl will go to the right of the splitter :
        Sauv := FhandledControl.Left;
        FhandledControl.Width := FNewSize;
        if FhandledControl.Left <> Sauv
        then FhandledControl.Left := Sauv;
      end;

    rsBottom:
      begin
        FhandledControl.Height := FNewSize;
      end;

    rsLeft:
      begin
        FDrawMotionControl.DisableAlign;
        try
          //if FhandledControl.Align = alNone
          //then

          // Change handled control Left before changing Width to avoid permuting controls with same align :
          FhandledControl.Left := FhandledControl.Left + (FhandledControl.Width - FNewSize);
          FhandledControl.Width := FNewSize;
        finally

        end;
        FDrawMotionControl.EnableAlign;
      end;

    rsTop:
      begin
        FDrawMotionControl.DisableAlign;
        try
//          if FhandledControl.Align = alNone   // or ResizeMode = rmClassic
//          then

          // Change handled control top before changing Height to avoid permuting controls with same align :
          FhandledControl.Top := FhandledControl.Top + (FhandledControl.Height - FNewSize);
          FhandledControl.Height := FNewSize;
        finally

        end;
        FDrawMotionControl.EnableAlign;
      end;
  end;

  Update;
  if Assigned(FOnMoved) then FOnMoved(Self);
  FOldSize := FNewSize;
end;

procedure TcySplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
{$IF NOT DEFINED(CLR)}
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
{$IFEND}
end;

procedure TcySplitter.StopSizing;
begin
  if Assigned(FhandledControl) then
  begin
    if FLineVisible then DrawLine;
    FhandledControl := nil;
    ReleaseLineDC;
{$IF DEFINED(CLR)}
    if Assigned(FActiveControl) and Assigned(FOldKeyDown) then
    begin
      (FActiveControl as IControl).UnhookDelegate('OnKeyDown', @FOldKeyDown);
      FActiveControl := nil;
      FOldKeyDown := nil;
    end;
{$ELSE}
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
{$IFEND}
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

end.
