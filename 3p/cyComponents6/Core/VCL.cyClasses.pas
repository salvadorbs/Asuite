{  Unit VCL.cyClasses

    Description:
    Unit with sub-properties for components.

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

unit VCL.cyClasses;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Forms, Buttons, Graphics, Math, Controls, ExtCtrls, StdCtrls, Jpeg, SysUtils, VCL.cyTypes, VCL.cyGraphics;

type
  TcyRunTimeDesign=class(TPersistent)
  private
    FAllowMove: Boolean;
    FAllowResizeTop: Boolean;
    FAllowResizeLeft: Boolean;
    FAllowResizeRight: Boolean;
    FAllowResizeBottom: Boolean;
    FControl: TControl;
    FJob: TRunTimeDesignJob;
    FJobAtPos: TRunTimeDesignJob;
    FResizeBorderSize: Word;
    FOutsideParentRect: Boolean;
    procedure SetJobAtPos(const Value: TRunTimeDesignJob);
  protected
    FSavCursor: TCursor;
    FFromRect: TRect;
    FFromX: Integer;
    FFromY: Integer;
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure Assign(Source: TPersistent); override;
    function DetermineJobAtPos(X, Y: Integer): TRunTimeDesignJob;
    procedure StartJob(X, Y: Integer);
    procedure DoJob(X, Y: Integer);
    procedure EndJob(X, Y: Integer);
    property Control: TControl read FControl write FControl;
    property Job: TRunTimeDesignJob read FJob write FJob default rjNothing;
    property JobAtPos: TRunTimeDesignJob read FJobAtPos write SetJobAtPos default rjNothing;
  published
    property AllowMove: Boolean read FAllowMove write FAllowMove default false;
    property AllowResizeTop: Boolean read FAllowResizeTop write FAllowResizeTop default false;
    property AllowResizeLeft: Boolean read FAllowResizeLeft write FAllowResizeLeft default false;
    property AllowResizeRight: Boolean read FAllowResizeRight write FAllowResizeRight default false;
    property AllowResizeBottom: Boolean read FAllowResizeBottom write FAllowResizeBottom default false;
    property OutsideParentRect: Boolean read FOutsideParentRect write FOutsideParentRect default true;
    property ResizeBorderSize: Word read FResizeBorderSize write FResizeBorderSize default 7;
  end;

  TcyShadowText=class(TPersistent)
  private
    FRelativePosX: Integer;
    FRelativePosY: Integer;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FZoomPercent: Word;
    procedure SetColor(const Value: TColor);
    procedure SetRelativePosX(const Value: Integer);
    procedure SetRelativePosY(const Value: Integer);
    procedure SetZoomPercent(const Value: Word);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure Assign(Source: TPersistent); override;
    function Active: boolean;
    function CalcShadowRect(CaptionRect: TRect; Alignment: TAlignment; Layout: TTextLayout; NormalFontHeight: Integer): TRect;
    procedure DrawShadowText(Canvas: TCanvas; NormalCaptionRect: TRect; Caption: String; Alignment: TAlignment;
                              Layout: TTextLayout; CaptionOrientation: TCaptionOrientation; TextFormat: Longint);
  published
    property Color: TColor read FColor write SetColor default clGray;
    property ZoomPercent: Word read FZoomPercent write SetZoomPercent default 100;
    property RelativePosX: Integer read FRelativePosX write SetRelativePosX default 0;
    property RelativePosY: Integer read FRelativePosY write SetRelativePosY default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcyBgPicture=class(TPersistent)
  private
    FIndentX: Integer;
    FIndentY: Integer;
    FOwner: TComponent;
    FTransparent: Boolean;
    FPicture: TPicture;
    FStyle: TBgStyle;
    FOnChange: TNotifyEvent;
    FRepeatX: Word;
    FRepeatY: Word;
    FIntervalX: Integer;
    FIntervalY: Integer;
    FPosition: TBgPosition;
    procedure SetIndentX(const Value: Integer);
    procedure SetIndentY(const Value: Integer);
    procedure SetPicture(const Value: TPicture);
    procedure SetStyle(const Value: TBgStyle);
    procedure SetTransparent(const Value: Boolean);
    procedure SetRepeatX(const Value: Word);
    procedure SetRepeatY(const Value: Word);
    procedure SetIntervalX(const Value: Integer);
    procedure SetIntervalY(const Value: Integer);
    procedure SetPosition(const Value: TBgPosition);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function SomethingToDraw: Boolean;
  published
    property IndentX: Integer read FIndentX write SetIndentX default 0;
    property IndentY: Integer read FIndentY write SetIndentY default 0;
    property IntervalX: Integer read FIntervalX write SetIntervalX default 0;
    property IntervalY: Integer read FIntervalY write SetIntervalY default 0;
    property RepeatX: Word read FRepeatX write SetRepeatX default 1;
    property RepeatY: Word read FRepeatY write SetRepeatY default 1;
    property Picture: TPicture read FPicture write SetPicture;
    property Position: TBgPosition read FPosition write SetPosition default bgTopLeft;
    property Style: TBgStyle read FStyle write SetStyle default bgMosaic;
    property Transparent: Boolean Read FTransparent write SetTransparent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcyGradient=class(TPersistent)
  private
    FBalance: Word;
    FFromColor: TColor;
    FToColor: TColor;
    FOrientation: TDgradOrientation;
    FOnChange: TNotifyEvent;
    FAngleDegree: Word;
    FBalanceMode: TDgradBalanceMode;
    FMaxDegrade: byte;
    FSpeedPercent: Integer;
    procedure SetBalance(const Value: Word);
    procedure SetFromColor(const Value: TColor);
    procedure SetToColor(const Value: TColor);
    procedure SetOrientation(const Value: TDgradOrientation);
    procedure SetAngleDegree(const Value: Word);
    procedure SetBalanceMode(const Value: TDgradBalanceMode);
    procedure SetMaxDegrade(const Value: byte);
    procedure SetSpeedPercent(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(aCanvas: TCanvas; aRect: TRect);
  published
    property AngleDegree: Word read FAngleDegree Write SetAngleDegree;
    property Balance: Word read FBalance write SetBalance default 50;
    property BalanceMode: TDgradBalanceMode read FBalanceMode write SetBalanceMode default bmNormal;
    property FromColor: TColor read FFromColor write SetFromColor;
    property MaxDegrade: byte read FMaxDegrade write SetMaxDegrade default 255;
    property Orientation: TDgradOrientation read FOrientation write SetOrientation default dgdVertical;
    property SpeedPercent: Integer read FSpeedPercent write SetSpeedPercent; // default 100; Cannot set default because SpeedPercent modified in some components
    property ToColor: TColor read FToColor write SetToColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  tcyBevel = class(TCollectionItem)
  private
    FHighlightColor: TColor;
    FShadowColor: TColor;
    FWidth: Word;
    FStyle: TcyBevelCut;
    FDrawRight: Boolean;
    FDrawLeft: Boolean;
    FDrawTop: Boolean;
    FDrawBottom: Boolean;
    FNeedOwnerRealign: Boolean;
    procedure SetHighlightColor(const Value: TColor);
    procedure SetShadowColor(const Value: TColor);
    procedure SetWidth(const Value: Word);
    procedure SetStyle(const Value: TcyBevelCut);
    procedure SetDrawBottom(const Value: Boolean);
    procedure SetDrawLeft(const Value: Boolean);
    procedure SetDrawRight(const Value: Boolean);
    procedure SetDrawTop(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DrawLeft: Boolean read FDrawLeft write SetDrawLeft default True;
    property DrawTop: Boolean read FDrawTop write SetDrawTop default True;
    property DrawRight: Boolean read FDrawRight write SetDrawRight default True;
    property DrawBottom: Boolean read FDrawBottom write SetDrawBottom default True;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clBtnHighlight;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property Style: TcyBevelCut read FStyle write SetStyle default bcRaised;
    property Width: Word read FWidth write SetWidth default 1;
  end;

  TcyBevelClass = class of tcyBevel;

  tcyBevels = Class(TCollection)
  private
    FControl: TControl;
    FOnChange: TNotifyEvent;
    FNeedOwnerRealign: Boolean;
    function GetBevel(Index: Integer): TcyBevel;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aControl: TControl; BevelClass: TcyBevelClass);
    function Add: TcyBevel;
    procedure Delete(Index: Integer);
    procedure DrawBevels(aCanvas: TCanvas; var BoundsRect: TRect; RoundRect: Boolean);
    function BevelsWidth: Word;
    property Items[Index: Integer]: TcyBevel read GetBevel; default;
    property NeedOwnerRealign: Boolean read FNeedOwnerRealign;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // Use TImageList instead of Glyph:
  TcyImagelistOptions = class(TPersistent)
  private
    FOwner: TComponent;
    FImageList: TImageList;
    FDisabledIndex: Integer;
    FMouseDownIndex: Integer;
    FExclusiveIndex: Integer;
    FNormalIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure SetDisabledIndex(const Value: Integer);
    procedure SetExclusiveIndex(const Value: Integer);
    procedure SetImageList(const Value: TImageList);
    procedure SetMouseDownIndex(const Value: Integer);
    procedure SetNormalIndex(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    procedure GetDrawingParams(State: TButtonState; var ImageIndexParam: Integer; var EnabledParam: Boolean);
  published
    property DisabledIndex: Integer read FDisabledIndex write SetDisabledIndex default -1;
    property ExclusiveIndex: Integer read FExclusiveIndex write SetExclusiveIndex default -1;
    property MouseDownIndex: Integer read FMouseDownIndex write SetMouseDownIndex default -1;
    property NormalIndex: Integer read FNormalIndex write SetNormalIndex default -1;
    property ImageList: TImageList read FImageList write SetImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  procedure cyDrawBgPicture(aCanvas: TCanvas; aRect: TRect; aBgPicture: TcyBgPicture);

implementation

{ TcyRunTimeDesign }
procedure TcyRunTimeDesign.Assign(Source: TPersistent);
begin
  if Source is TcyRunTimeDesign then
  begin
    FJob := TcyRunTimeDesign(Source).FJob;
    FJobAtPos := TcyRunTimeDesign(Source).FJobAtPos;
    FAllowMove := TcyRunTimeDesign(Source).FAllowMove;
    FAllowResizeTop := TcyRunTimeDesign(Source).FAllowResizeTop;
    FAllowResizeLeft := TcyRunTimeDesign(Source).FAllowResizeLeft;
    FAllowResizeRight := TcyRunTimeDesign(Source).FAllowResizeRight;
    FAllowResizeBottom := TcyRunTimeDesign(Source).FAllowResizeBottom;
    FOutsideParentRect := TcyRunTimeDesign(Source).FOutsideParentRect;
    FResizeBorderSize := TcyRunTimeDesign(Source).FResizeBorderSize;
  end;
//  inherited Assign(Source);
end;

constructor TcyRunTimeDesign.Create(AOwner: TComponent);
begin
  if AOwner is TControl
  then FControl := TControl(AOwner);
  FJob := rjNothing;
  FJobAtPos := rjNothing;
  FAllowMove := false;
  FAllowResizeTop := false;
  FAllowResizeLeft := false;
  FAllowResizeRight := false;
  FAllowResizeBottom := false;
  FOutsideParentRect := true;
  FResizeBorderSize := 7;
end;

procedure TcyRunTimeDesign.SetJobAtPos(const Value: TRunTimeDesignJob);
begin
  if not Assigned(FControl) then Exit;
  if FJobAtPos = Value then EXIT;

  // Save cursor before modifying it ...
  if FJobAtPos = rjNothing
  then FSavCursor := FControl.Cursor;

  FJobAtPos := Value;
  case Value of
    rjNothing:           FControl.Cursor := FSavCursor;    // Restore saved cursor ...
    rjResizeTopLeft:     FControl.Cursor := crSizeNWSE;
    rjResizeTop:         FControl.Cursor := crSizeNS;
    rjResizeTopRight:    FControl.Cursor := crSizeNESW;
    rjResizeLeft:        FControl.Cursor := crSizeWE;
    rjMove:              FControl.Cursor := crHandPoint;
    rjResizeRight:       FControl.Cursor := crSizeWE;
    rjResizeBottomLeft:  FControl.Cursor := crSizeNESW;
    rjResizeBottom:      FControl.Cursor := crSizeNS;
    rjResizeBottomRight: FControl.Cursor := crSizeNWSE;
  end;
end;

function TcyRunTimeDesign.DetermineJobAtPos(X, Y: Integer): TRunTimeDesignJob;
var
  ResizeLeft, ResizeRight, ResizeTop, ResizeBottom: Boolean;
begin
  RESULT := rjNothing;
  if not Assigned(FControl) then Exit;

  if FAllowMove then RESULT := rjMove;

  ResizeLeft := (X < FResizeBorderSize) and FAllowResizeLeft;
  ResizeTop := (Y < FResizeBorderSize) and  FAllowResizeTop;
  ResizeRight := (X > FControl.ClientWidth - FResizeBorderSize) and FAllowResizeRight;  // Use ClientWidth because some controls like TCOmbobox don' t respond when cursor over scroll
  ResizeBottom := (Y > FControl.ClientHeight - FResizeBorderSize) and FAllowResizeBottom;

  if ResizeLeft
  then begin
    if ResizeTop
    then
      RESULT := rjResizeTopLeft
    else
      if ResizeBottom
      then RESULT := rjResizeBottomLeft
      else RESULT := rjResizeLeft;
  end
  else
    if ResizeRight
    then begin
      if ResizeTop
      then
        RESULT := rjResizeTopRight
      else
        if ResizeBottom
        then RESULT := rjResizeBottomRight
        else RESULT := rjResizeRight;
    end
    else
      if ResizeTop
      then
        RESULT := rjResizeTop
      else
        if ResizeBottom
        then RESULT := rjResizeBottom;
end;

procedure TcyRunTimeDesign.StartJob(X, Y: Integer);
begin
  if not Assigned(FControl) then Exit;

  Job := DetermineJobAtPos(X, Y);
  JobAtPos := Job;

  if Job <> rjNothing   // Are we doing something?
  then begin
    FFromRect := FControl.BoundsRect;
    FFromX := X;
    FFromY := Y;
  end;
end;

procedure TcyRunTimeDesign.DoJob(X, Y: Integer);

      function CorrectMoveLeft(Value: Integer): Integer;
      var Limit: Integer;
      begin
        RESULT := Value;

        if not FOutsideParentRect
        then
          if Value > 0
          then begin
            if FControl.Parent = Nil
            then Limit := Screen.Width - FControl.Width
            else Limit := FControl.Parent.Width - FControl.Width;

            if Value > Limit
            then RESULT := Limit;
          end
          else
            RESULT := 0;
      end;

      function CorrectMoveTop(Value: Integer): Integer;
      var Limit: Integer;
      begin
        RESULT := Value;

        if not FOutsideParentRect
        then
          if Value > 0
          then begin
            if FControl.Parent = Nil
            then Limit := Screen.Height - FControl.Height
            else Limit := FControl.Parent.Height - FControl.Height;

            if Value > Limit
            then RESULT := Limit;
          end
          else
            RESULT := 0;
      end;

      function CorrectResizeUpperLeft(Value: Integer): Integer;
      begin
        if (not FOutsideParentRect) and (Value < 0)
        then RESULT := 0
        else RESULT := Value;
      end;

      function CorrectResizeRight(Value: Integer): Integer;
      var Limit: Integer;
      begin
        RESULT := Value;

        if not FOutsideParentRect
        then begin
          if FControl.Parent = Nil
          then Limit := Screen.Width
          else Limit := FControl.Parent.Width;

          if Value > Limit
          then RESULT := Limit;
        end;
      end;

      function CorrectResizeBottom(Value: Integer): Integer;
      var Limit: Integer;
      begin
        RESULT := Value;

        if not FOutsideParentRect
        then begin
          if FControl.Parent = Nil
          then Limit := Screen.Height
          else Limit := FControl.Parent.Height;

          if Value > Limit
          then RESULT := Limit;
        end;
      end;

begin
  if not Assigned(FControl) then Exit;

  if FJob = rjNothing
  then
    JobAtPos := DetermineJobAtPos(X, Y)
  else
    if FJob = rjMove
    then begin
      FControl.Left := CorrectMoveLeft(FControl.Left + X - FFromX);
      FControl.Top := CorrectMoveTop(FControl.Top + Y - FFromY);
    end
    else begin
      if FJob in [rjResizeRight, rjResizeTopRight, rjResizeBottomRight]
      then begin
        FFromRect.Right := CorrectResizeRight(FFromRect.Right + X - FFromX);
        FFromX := FFromRect.Right - FFromRect.Left;
      end
      else
        if FJob in [rjResizeLeft, rjResizeTopLeft, rjResizeBottomLeft]
        then begin
          FFromRect.Left := CorrectResizeUpperLeft(FFromRect.Left - FFromX + X);

          // Check constraints values to avoid moving the component:
          if FControl.Constraints.MaxWidth > 0
          then
            if FFromRect.Right - FFromRect.Left > FControl.Constraints.MaxWidth
            then FFromRect.Left := FFromRect.Right - FControl.Constraints.MaxWidth;

          if FControl.Constraints.MinWidth > 0
          then
            if FFromRect.Right - FFromRect.Left < FControl.Constraints.MinWidth
            then FFromRect.Left := FFromRect.Right - FControl.Constraints.MinWidth;
        end;

      if FJob in [rjResizeBottom, rjResizeBottomLeft, rjResizeBottomRight]
      then begin
        FFromRect.Bottom := CorrectResizeBottom(FFromRect.Bottom + Y - FFromY);
        FFromY := FFromRect.Bottom - FFromRect.Top;
      end
      else
        if FJob in [rjResizeTop, rjResizeTopLeft, rjResizeTopRight]
        then begin
          FFromRect.Top := CorrectResizeUpperLeft(FFromRect.Top - FFromY + Y);

          // Check constraints values to avoid moving the component:
          if FControl.Constraints.MaxHeight > 0
          then
            if FFromRect.Bottom - FFromRect.Top > FControl.Constraints.MaxHeight
            then FFromRect.Top := FFromRect.Bottom - FControl.Constraints.MaxHeight;

          if FControl.Constraints.MinHeight > 0
          then
            if FFromRect.Bottom - FFromRect.Top < FControl.Constraints.MinHeight
            then FFromRect.Top := FFromRect.Bottom - FControl.Constraints.MinHeight;
        end;

      FControl.SetBounds(FFromRect.Left, FFromRect.Top, FFromRect.Right - FFromRect.Left, FFromRect.Bottom - FFromRect.Top);
    end;
end;

procedure TcyRunTimeDesign.EndJob(X, Y: Integer);
begin
  if not Assigned(FControl) then Exit;

  if Job <> rjNothing   // Are we doing something?
  then begin
    DoJob(X, Y);
    Job := rjNothing;
  end;
end;

{ TcyShadowText }
constructor TcyShadowText.Create(AOwner: TComponent);
begin
  FColor := clGray;
  FZoomPercent := 100;
  FRelativePosX := 0;
  FRelativePosY := 0;
end;

procedure TcyShadowText.DrawShadowText(Canvas: TCanvas; NormalCaptionRect: TRect; Caption: String; Alignment: TAlignment;
  Layout: TTextLayout; CaptionOrientation: TCaptionOrientation; TextFormat: Integer);
var
  ShadowRect: TRect;
  SaveColor: TColor;
  SaveFontHeight: Integer;
begin
  SaveColor := Canvas.Font.Color;
  SaveFontHeight := Canvas.Font.Height;

  ShadowRect := CalcShadowRect(NormalCaptionRect, Alignment, Layout, Canvas.Font.Height);
  Canvas.Font.Color := Color;
  Canvas.Font.Height := Round(Canvas.Font.Height * ZoomPercent / 100);

  if CaptionOrientation = coHorizontal
  then cyDrawText(Canvas.Handle, Caption, ShadowRect, TextFormat)
  else cyDrawVerticalText(Canvas, Caption, ShadowRect, TextFormat, CaptionOrientation, Alignment, Layout);

  // Restore definitions in order to draw normal text:
  Canvas.Font.Color := SaveColor;
  Canvas.Font.Height := SaveFontHeight;
end;

procedure TcyShadowText.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyShadowText.SetRelativePosX(const Value: Integer);
begin
  FRelativePosX := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyShadowText.SetRelativePosY(const Value: Integer);
begin
  FRelativePosY := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyShadowText.SetZoomPercent(const Value: Word);
begin
  if Value > 0
  then begin
    FZoomPercent := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

function TcyShadowText.Active: boolean;
begin
  RESULT := (FZoomPercent <> 100) or (FRelativePosX <> 0) or (FRelativePosY <> 0);
end;

function TcyShadowText.CalcShadowRect(CaptionRect: TRect; Alignment: TAlignment; Layout: TTextLayout; NormalFontHeight: Integer): TRect;
var
  FFontZoom: Double;
  incWidth, incHeight: Integer;
begin
  RESULT := CaptionRect;
  FFontZoom := Round(NormalFontHeight * FZoomPercent / 100) / NormalFontHeight; // = New calculated font height / normal font height ...
  incWidth := Ceil( (CaptionRect.Right - CaptionRect.Left) * (FFontZoom - 1) );
  incHeight := Ceil( (CaptionRect.Bottom - CaptionRect.Top) * (FFontZoom - 1) );

  // Adjust horizontal position :
  case Alignment of
    taLeftJustify:
      inc(RESULT.Right, incWidth);

    taCenter:
    begin
      dec(RESULT.Left, incWidth);
      OffsetRect(RESULT, incWidth div 2, 0);
    end;

    taRightJustify:
      dec(RESULT.Left, incWidth);
  end;

  // Adjust vertical position :
  case Layout of
    tlTop:
      inc(RESULT.Bottom, incHeight);

    tlCenter:
    begin
      dec(RESULT.Top, incHeight);
      OffsetRect(RESULT, 0, incHeight div 2);
    end;

    tlBottom:
      dec(RESULT.Top, incHeight);
  end;

  // Modify position with RelativePosX and RelativePosY :
  OffsetRect(RESULT, FRelativePosX, FRelativePosY);
end;

procedure TcyShadowText.Assign(Source: TPersistent);
begin
//  Inherited;

  if Source is TcyShadowText
  then begin
    FColor := TcyShadowText(Source).Color;
    FRelativePosX := TcyShadowText(Source).RelativePosX;
    FRelativePosY := TcyShadowText(Source).RelativePosY;
    FZoomPercent := TcyShadowText(Source).ZoomPercent;
  end;

  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TcyBgPicture }
constructor TcyBgPicture.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FIndentX := 0;
  FIndentY := 0;
  FRepeatX := 1;
  FRepeatY := 1;
  FPicture := TPicture.Create;
  FStyle := bgMosaic;
  FPosition := bgTopLeft;
  FTransparent := false;
end;

procedure TcyBgPicture.Assign(Source: TPersistent);
begin
  if Source is TcyBgPicture
  then begin
    FIndentX := TcyBgPicture(Source).IndentX;
    FIndentY := TcyBgPicture(Source).IndentY;
    FIntervalX := TcyBgPicture(Source).IntervalX;
    FIntervalY := TcyBgPicture(Source).IntervalY;
    FRepeatX := TcyBgPicture(Source).RepeatX;
    FRepeatY := TcyBgPicture(Source).RepeatY;
    FStyle := TcyBgPicture(Source).Style;
    FPicture.Assign(TcyBgPicture(Source).Picture);
    FTransparent := TcyBgPicture(Source).Transparent;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
//  inherited Assign(Source);
end;

destructor TcyBgPicture.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TcyBgPicture.SetIndentX(const Value: Integer);
begin
  FIndentX := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetIndentY(const Value: Integer);
begin
  FIndentY := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetIntervalX(const Value: Integer);
begin
  FIntervalX := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetIntervalY(const Value: Integer);
begin
  FIntervalY := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);

  if csDesigning in FOwner.ComponentState
  then
    if FStyle = bgNone
    then FStyle := bgMosaic;

  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetPosition(const Value: TBgPosition);
begin
  FPosition := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetRepeatX(const Value: Word);
begin
  FRepeatX := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetRepeatY(const Value: Word);
begin
  FRepeatY := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetStyle(const Value: TBgStyle);
begin
  FStyle := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyBgPicture.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TcyBgPicture.SomethingToDraw: Boolean;
begin
  RESULT := (Picture.Graphic <> Nil) and (Style <> bgNone);
end;

{ TcyGradient }
constructor TcyGradient.Create(AOwner: TComponent);
begin
  FBalance := 50;
  FFromColor := clWhite;
  FToColor := clBtnFace;
  FOrientation := dgdVertical;
  FBalanceMode := bmNormal;
  FMaxDegrade := 255;
  FSpeedPercent := 100;
end;

procedure TcyGradient.Draw(aCanvas: TCanvas; aRect: TRect);
begin
  if FFromColor = FToColor
  then begin
    aCanvas.Brush.Color := FFromColor;
    aCanvas.Brush.Style := bsSolid;
    aCanvas.FillRect(aRect);
  end
  else
    cyGradientFill(aCanvas, aRect, FFromColor, FToColor, FOrientation,
                    FBalance, FBalanceMode, FMaxDegrade, FSpeedPercent);
end;

procedure TcyGradient.Assign(Source: TPersistent);
begin
  if Source is TcyGradient
  then begin
    FAngleDegree := TcyGradient(Source).AngleDegree;
    FBalance := TcyGradient(Source).Balance;
    FFromColor := TcyGradient(Source).FromColor;
    FToColor := TcyGradient(Source).ToColor;
    FBalanceMode := TcyGradient(Source).BalanceMode;
    FOrientation := TcyGradient(Source).Orientation;
    FSpeedPercent := TcyGradient(Source).SpeedPercent;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
//  inherited Assign(Source);
end;

procedure TcyGradient.SetFromColor(const Value: TColor);
begin
  if Value <> FFromColor
  then begin
    FFromColor:= Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetToColor(const Value: TColor);
begin
  if Value <> FToColor
  then begin
    FToColor:= Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetOrientation(const Value: TDgradOrientation);
begin
  if Value <> fOrientation
  then begin
    FOrientation := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetSpeedPercent(const Value: Integer);
begin
  if (Value <> FSpeedPercent)
    and (Value In [0..100])
  then begin
    FSpeedPercent := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetBalance(const Value: Word);
begin
  if (Value <> FBalance)
    and (Value In [0..100])
  then begin
    FBalance := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetBalanceMode(const Value: TDgradBalanceMode);
begin
  if (Value <> FBalanceMode)
  then begin
    FBalanceMode := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetAngleDegree(const Value: Word);
begin
  if (Value <> FAngleDegree) and (Value < 360)
  then begin
    FAngleDegree := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyGradient.SetMaxDegrade(const Value: byte);
begin
  if (Value <> FMaxDegrade) and (Value <> 0)
  then begin
    FMaxDegrade := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ tcyBevel }
procedure tcyBevel.Assign(Source: TPersistent);
begin
  if Source is tcyBevel then
  begin
    FHighlightColor := tcyBevel(Source).FHighlightColor;
    FShadowColor := tcyBevel(Source).FShadowColor;
    FWidth := tcyBevel(Source).FWidth;
    FStyle  := tcyBevel(Source).FStyle;
    FDrawRight  := tcyBevel(Source).FDrawRight;
    FDrawLeft    := tcyBevel(Source).FDrawLeft;
    FDrawTop := tcyBevel(Source).FDrawTop;
    FDrawBottom := tcyBevel(Source).FDrawBottom;
  end;
//  inherited Assign(Source);
end;

constructor tcyBevel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FHighlightColor := clBtnHighlight;
  FShadowColor := clBtnShadow;
  FWidth := 1;
  FStyle := bcRaised;
  FDrawLeft := true;
  FDrawTop := true;
  FDrawRight := true;
  FDrawBottom := true;
  FNeedOwnerRealign := true;
end;

function tcyBevel.GetDisplayName: string;
begin
  case FStyle of
    bcLowered: Result := 'Lowered';
    bcRaised: Result := 'Raised';
    bcNone: Result := 'None';
    bcTransparent: Result := 'Transparent';
  end;

  Result := Result + ' Bevel';
  Result := Result + ' Width = ' + intToStr(FWidth);
end;

procedure tcyBevel.SetDrawBottom(const Value: Boolean);
begin
  FDrawBottom := Value;
  Changed(false);            // It will call TcyBevels.Update !
end;

procedure tcyBevel.SetDrawLeft(const Value: Boolean);
begin
  FDrawLeft := Value;
  Changed(false);
end;

procedure tcyBevel.SetDrawRight(const Value: Boolean);
begin
  FDrawRight := Value;
  Changed(false);
end;

procedure tcyBevel.SetDrawTop(const Value: Boolean);
begin
  FDrawTop := Value;
  Changed(false);
end;

procedure tcyBevel.SetHighlightColor(const Value: TColor);
begin
  FHighlightColor := Value;
  Changed(false);
end;

procedure tcyBevel.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Changed(false);
end;

procedure tcyBevel.SetStyle(const Value: TcyBevelCut);
begin
  if FStyle = Value then EXIT;

  if (FStyle = bcNone) or (Value = bcNone)
  then FNeedOwnerRealign := true;

  FStyle := Value;
  Changed(false);
end;

procedure tcyBevel.SetWidth(const Value: Word);
begin
  if FWidth = Value then EXIT;

  FWidth := Value;
  FNeedOwnerRealign := true;
  Changed(false);
end;

{TcyBevels}
constructor TcyBevels.Create(aControl: TControl; BevelClass: TcyBevelClass);
begin
  inherited Create(BevelClass);
  FControl := aControl;
  FNeedOwnerRealign := false;
end;

function TcyBevels.GetBevel(Index: Integer): TcyBevel;
begin
  Result := TcyBevel(inherited Items[Index]);
end;

function TcyBevels.GetOwner: TPersistent;
begin
  Result := FControl;
end;

// Event Called by setting properties/events of TcyBevel :
procedure TcyBevels.Update(Item: TCollectionItem);
begin
  Inherited;
  if Assigned(FOnChange)
  then begin
    if Item <> nil
    then
      if TcyBevel(Item).FNeedOwnerRealign
      then begin
        FNeedOwnerRealign := true;
        TcyBevel(Item).FNeedOwnerRealign := false;
      end;

    FOnChange(Self);
    FNeedOwnerRealign := false;
  end
  else
    FControl.Invalidate;
end;

function TcyBevels.Add: TcyBevel;
begin
  Result := TcyBevel(inherited Add);
  Result.Changed(false);      // It will call TcyBevels.Update only at run-time!
end;

procedure TcyBevels.Delete(Index: Integer);
begin
  Inherited;
  FNeedOwnerRealign := true;
  Update(Nil);
end;

procedure TcyBevels.DrawBevels(aCanvas: TCanvas; var BoundsRect: TRect; RoundRect: Boolean);
var
  b: Integer;

  // Gradient rendering:
  NextHighlightColor, NextShadowColor: TColor;
  w, nbDgrad, fromColorHighlightRGB, toColorHighlightRGB, fromColorShadowRGB, toColorShadowRGB: Integer;
  Arr_StartHighlightRGB, Arr_StartShadowRGB, Arr_CurHighlightRGB, Arr_CurShadowRGB: Array[0..2] of Byte;
  Arr_DifHighlightRGB, Arr_DifShadowRGB: Array[0..2] of Integer;
begin
  for b := 0 to Count-1 do
    case Items[b].FStyle of
      bcRaised:
      begin
        cyFrame3D(aCanvas, BoundsRect, Items[b].FHighlightColor, Items[b].FShadowColor, Items[b].FWidth,
            Items[b].FDrawLeft, Items[b].FDrawTop, Items[b].FDrawRight, Items[b].FDrawBottom, RoundRect);
        RoundRect := false;
      end;

      bcLowered:
      begin
        cyFrame3D(aCanvas, BoundsRect, Items[b].FShadowColor, Items[b].FHighlightColor, Items[b].FWidth,
            Items[b].FDrawLeft, Items[b].FDrawTop, Items[b].FDrawRight, Items[b].FDrawBottom, RoundRect);
        RoundRect := false;
      end;

      bcTransparent:   // Just Inflate Rect
      begin
        InflateRect(BoundsRect, (-1) * Items[b].FWidth, (-1) * Items[b].FWidth);
        RoundRect := false;
      end;

      bcNone: ;

      bcGradientToNext:
      begin
        // *** Draw gradient from this bevel to next one *** //

        // Draw first one pixel frame defined colors :
        cyFrame3D(aCanvas, BoundsRect, Items[b].FHighlightColor, Items[b].FShadowColor, 1,
            Items[b].FDrawLeft, Items[b].FDrawTop, Items[b].FDrawRight, Items[b].FDrawBottom, RoundRect);
        RoundRect := false;

        // Draw gradient :
        if b = Count-1 then
        begin
          NextHighlightColor := Items[b].FHighlightColor;
          NextShadowColor := Items[b].FShadowColor;
        end
        else begin
          NextHighlightColor := Items[b+1].FHighlightColor;
          NextShadowColor := Items[b+1].FShadowColor;
        end;

        // Highlight initialization:
        nbDgrad := Items[b].FWidth + 1;
        fromColorHighlightRGB := ColorToRGB(Items[b].FHighlightColor);
        toColorHighlightRGB   := ColorToRGB(NextHighlightColor);

        Arr_StartHighlightRGB[0] := GetRValue(fromColorHighlightRGB);
        Arr_StartHighlightRGB[1] := GetGValue(fromColorHighlightRGB);
        Arr_StartHighlightRGB[2] := GetBValue(fromColorHighlightRGB);

        Arr_DifHighlightRGB[0] := GetRValue(toColorHighlightRGB) - Arr_StartHighlightRGB[0] ;
        Arr_DifHighlightRGB[1] := GetGValue(toColorHighlightRGB) - Arr_StartHighlightRGB[1] ;
        Arr_DifHighlightRGB[2] := GetBValue(toColorHighlightRGB) - Arr_StartHighlightRGB[2] ;

        // Shadow initialization:
        fromColorShadowRGB := ColorToRGB(Items[b].FShadowColor);
        toColorShadowRGB   := ColorToRGB(NextShadowColor);

        Arr_StartShadowRGB[0] := GetRValue(fromColorShadowRGB);
        Arr_StartShadowRGB[1] := GetGValue(fromColorShadowRGB);
        Arr_StartShadowRGB[2] := GetBValue(fromColorShadowRGB);

        Arr_DifShadowRGB[0] := GetRValue(toColorShadowRGB) - Arr_StartShadowRGB[0] ;
        Arr_DifShadowRGB[1] := GetGValue(toColorShadowRGB) - Arr_StartShadowRGB[1] ;
        Arr_DifShadowRGB[2] := GetBValue(toColorShadowRGB) - Arr_StartShadowRGB[2] ;

        for w := 2 to Items[b].FWidth do
        begin
          Arr_CurHighlightRGB[0] := Arr_StartHighlightRGB[0] + MulDiv(w, Arr_DifHighlightRGB[0], nbDgrad);
          Arr_CurHighlightRGB[1] := Arr_StartHighlightRGB[1] + MulDiv(w, Arr_DifHighlightRGB[1], nbDgrad);
          Arr_CurHighlightRGB[2] := Arr_StartHighlightRGB[2] + MulDiv(w, Arr_DifHighlightRGB[2], nbDgrad);

          Arr_CurShadowRGB[0] := Arr_StartShadowRGB[0] + MulDiv(w, Arr_DifShadowRGB[0], nbDgrad);
          Arr_CurShadowRGB[1] := Arr_StartShadowRGB[1] + MulDiv(w, Arr_DifShadowRGB[1], nbDgrad);
          Arr_CurShadowRGB[2] := Arr_StartShadowRGB[2] + MulDiv(w, Arr_DifShadowRGB[2], nbDgrad);

        cyFrame3D(aCanvas, BoundsRect,
                 RGB(Arr_CurHighlightRGB[0], Arr_CurHighlightRGB[1], Arr_CurHighlightRGB[2]),
                 RGB(Arr_CurShadowRGB[0], Arr_CurShadowRGB[1], Arr_CurShadowRGB[2]),
                 1, Items[b].FDrawLeft, Items[b].FDrawTop, Items[b].FDrawRight, Items[b].FDrawBottom, RoundRect);
        end;
      end;
    end;
end;

function TcyBevels.BevelsWidth: Word;
var i: Integer;
begin
  RESULT := 0;

  for i := 0 to Count-1 do
    if Items[i].FStyle <> bcNone
    then Inc(RESULT, Items[i].FWidth);
end;

procedure cyDrawBgPicture(aCanvas: TCanvas; aRect: TRect; aBgPicture: TcyBgPicture);
begin
  DrawGraphic(aCanvas, aRect, aBgPicture.FPicture.Graphic, aBgPicture.FTransparent, aBgPicture.Style, aBgPicture.Position,
    aBgPicture.FIndentX, aBgPicture.FIndentY, aBgPicture.FIntervalX, aBgPicture.FIntervalY, aBgPicture.FRepeatX, aBgPicture.FRepeatY);
end;

{ TcyImagelistOptions }
constructor TcyImagelistOptions.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FDisabledIndex := -1;
  FExclusiveIndex := -1;
  FMouseDownIndex := -1;
  FNormalIndex := -1;
end;

procedure TcyImagelistOptions.SetDisabledIndex(const Value: Integer);
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    if Assigned(FImageList) then
      if Assigned(FOnChange) then
        FOnChange(Self);
  end;
end;

procedure TcyImagelistOptions.SetExclusiveIndex(const Value: Integer);
begin
  if FExclusiveIndex <> Value then
  begin
    FExclusiveIndex := Value;
    if Assigned(FImageList) then
      if Assigned(FOnChange) then
        FOnChange(Self);
  end;
end;

procedure TcyImagelistOptions.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);

  if Value <> nil then
    Value.FreeNotification(FOwner);
end;

procedure TcyImagelistOptions.SetMouseDownIndex(const Value: Integer);
begin
  if FMouseDownIndex <> Value then
  begin
    FMouseDownIndex := Value;
    if Assigned(FImageList) then
      if Assigned(FOnChange) then
        FOnChange(Self);
  end;
end;

procedure TcyImagelistOptions.SetNormalIndex(const Value: Integer);
begin
  if FNormalIndex <> Value then
  begin
    FNormalIndex := Value;
    if Assigned(FImageList) then
      if Assigned(FOnChange) then
        FOnChange(Self);
  end;
end;

procedure TcyImagelistOptions.GetDrawingParams(State: TButtonState; var ImageIndexParam: Integer; var EnabledParam: Boolean);
begin
  EnabledParam := true;

  case State of
    bsUp:
      ImageIndexParam := FNormalIndex;

    bsDown:
      ImageIndexParam := FMouseDownIndex;

    bsExclusive:
      ImageIndexParam := FExclusiveIndex;

    bsDisabled:
      begin
        ImageIndexParam := FDisabledIndex;
        EnabledParam := ImageIndexParam > -1;
      end;
  end;

  if ImageIndexParam = -1 then
    ImageIndexParam := FNormalIndex;
end;

end.
