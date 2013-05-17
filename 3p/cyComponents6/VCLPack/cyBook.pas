{   Component(s):
    tcyBook

    Description:
    Virtual Book with page view handling and turn page effect.

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

unit cyBook;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Types, SysUtils, Controls, Graphics, VCL.cyClasses, VCL.cyTypes, VCL.cyGraphics, VCL.cyImage;

type
  TcyBook = class;
  TPageRender = (prFitHighQuality, prFitLowQuality, prStretchHighQuality, prStretchLowQuality, prCentered);

  TBookPageView = class(TPersistent)
  private
    FOwner: TcyBook;
    FOriginalPage: TBitmap;
    FOriginalPageBehind: TBitmap;
    FPage: TBitmap;
    FPageBehind: TBitmap;
    FInvalidOriginalPageBehind: Boolean;
    FInvalidOriginalPage: Boolean;
    FInvalidPageBehind: Boolean;
    FInvalidPage: Boolean;
    FPaperShadeColor: TColor;
    FPaperColor: TColor;
    FWallpaperUnderPage: TcyBgPicture;
    FWallpaperOverPage: TcyBgPicture;
    FPageRender: TPageRender;
    FPageBehindRender: TPageRender;
    FMargins: Integer;
    procedure SetPaperColor(const Value: TColor);
    procedure SetPaperShadeColor(const Value: TColor);
    procedure SetWallpaperOverPage(const Value: TcyBgPicture);
    procedure SetWallpaperUnderPage(const Value: TcyBgPicture);
  protected
    procedure WallpaperChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property InvalidOriginalPage: Boolean read FInvalidOriginalPage write FInvalidOriginalPage;
    property InvalidOriginalPageBehind: Boolean read FInvalidOriginalPageBehind write FInvalidOriginalPageBehind;
    property InvalidPage: Boolean read FInvalidPage write FInvalidPage;
    property InvalidPageBehind: Boolean read FInvalidPageBehind write FInvalidPageBehind;
    property OriginalPage: TBitmap read FOriginalPage write FOriginalPage;
    property OriginalPageBehind: TBitmap read FOriginalPageBehind write FOriginalPageBehind;
    property Page: TBitmap read FPage;
    property PageBehind: TBitmap read FPageBehind;
    property PageRender: TPageRender read FPageRender;
    property PageBehindRender: TPageRender read FPageBehindRender;
    procedure SetPage(const withGraphic: TGraphic; TransparentColor: TColor);
    procedure SetPageBehind(const withGraphic: TGraphic; TransparentColor: TColor);
    procedure InvalidateOriginalPage;
    procedure InvalidateOriginalPageBehind;
    procedure InvalidatePage;
    procedure InvalidatePageBehind;
  published
    property Margins: Integer read FMargins write FMargins default 0;
    property PaperColor: TColor read FPaperColor write SetPaperColor default clWhite;
    property PaperShadeColor: TColor read FPaperShadeColor write SetPaperShadeColor default clgray;
    property WallpaperOverPage: TcyBgPicture read FWallpaperOverPage write SetWallpaperOverPage;
    property WallpaperUnderPage: TcyBgPicture read FWallpaperUnderPage write SetWallpaperUnderPage;
  end;

  THeadBand = class(TPersistent)
  private
    FOwner: TcyBook;
    FWidth: Integer;
    FPageSpacing: Integer;
    procedure SetWidth(const Value: Integer);
    procedure SetPageSpacing(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property PageSpacing: Integer read FPageSpacing write SetPageSpacing default 0;
    property Width: Integer read FWidth write SetWidth default 20;
  end;

  TPageMode = (pmLeftPage, pmBehindLeftPage, pmBehindRightPage, pmRightPage);

  TTurnPageAnimation = class(TPersistent)
  private
    FBuffer: TBitmap;
    FPageBehind: TBitmap;
    FOwner: TcyBook;
    FEnabled: Boolean;
    FActive: Boolean;
    FAnimatedPage: TPageMode;
    FPercentDone: Integer;
    FBookImage: TBitmap;
    FDoubleBuffered: Boolean;
    FDrawPageBehind: Boolean;
    procedure SetPercentDone(const Value: Integer);
    procedure SetActive(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Active: Boolean read Factive write SetActive;
    property AnimatedPage: TPageMode read FAnimatedPage;
    property PercentDone: Integer read FPercentDone write SetPercentDone;
    property BookImage: TBitmap read FBookImage write FBookImage;
  published
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered default true;
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property DrawPageBehind: Boolean read FDrawPageBehind write FDrawPageBehind default true;
  end;

  TBookOption = (boInvalidatePagesOnResize,             // Reprocess bitmap pages rendering ...
                 boFitSmallImages,
                 boStretchSmallImages
                 );                    // !!! New properties must be added at the end !!!

  TBookOptions = Set of TBookOption;

  TProcOnNeedPage = procedure (Sender: TObject; PageNumber: Integer; PageMode: TPageMode) of object;
  TProcBeforePreparePage = procedure (Sender: TObject; PageNumber: Integer; PageMode: TPageMode; PageRect: TRect; var RenderMode: TPageRender; var Handled: Boolean) of object;
  TProcAfterPreparePage = procedure (Sender: TObject; PageNumber: Integer; PageMode: TPageMode) of object;

  TcyBook = class(TGraphicControl)
  private
    FMouseDown: Boolean;
    FMouseDownPosition: TPoint;
    FCurrentLeftPage: Integer;
    FCurrentRightPage: Integer;
    FPages: Integer;
    FBorders: TcyBevels;
    FRightPageView: TBookPageView;
    FLeftPageView: TBookPageView;
    FToColor: TColor;
    FFromColor: TColor;
    FHeadBand: THeadBand;
    FOnNeedPage: TProcOnNeedPage;
    FOnPaint: TNotifyEvent;
    FPageRender: TPageRender;
    FTurnPageAnimation: TTurnPageAnimation;
    FReadOnly: Boolean;
    FOnLeftPageChange: TNotifyEvent;
    FOnRightPageChange: TNotifyEvent;
    FOnLeftPageClick: TNotifyEvent;
    FOnRightPageClick: TNotifyEvent;
    FOptions: TBookOptions;
    FBeforePreparePage: TProcBeforePreparePage;
    FAfterPreparePage: TProcAfterPreparePage;
    procedure SetCurrentLeftPage(const Value: Integer);
    procedure SetCurrentRightPage(const Value: Integer);
    procedure SetPages(const Value: Integer);
    procedure SetBorders(const Value: TcyBevels);
    procedure SetLeftPageView(const Value: TBookPageView);
    procedure SetRightPageView(const Value: TBookPageView);
    procedure SetFromColor(const Value: TColor);
    procedure SetToColor(const Value: TColor);
    procedure SetHeadBand(const Value: THeadBand);
    procedure SetTurnPageAnimation(const Value: TTurnPageAnimation);
    procedure SetOptions(const Value: TBookOptions);
  protected
    procedure Paint; override;
    procedure DrawPage(PageBmp: TBitmap; PageRect: TRect; PageMargins: Integer; PageRender: TPageRender; ToCanvas: TCanvas);
    procedure DoDrawLeftSidePage(PageRect: TRect);
    procedure DoDrawRightSidePage(PageRect: TRect);
    procedure DoDrawLeftTurnPageEffect(PageRect: TRect; PercentDone: Integer);
    procedure DoDrawRightTurnPageEffect(PageRect: TRect; PercentDone: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseUp procedure ...
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    function CanViewPrior2Pages: Boolean;
    function CanViewNext2Pages: Boolean;
    procedure New(BookPages: Integer);
    procedure CalcRects(var BookRect, LeftPageRect, RightPageRect, HeadBandRect: TRect);
    procedure DrawLeftSideEmptyPage(PageRect: TRect);
    procedure DrawRightSideEmptyPage(PageRect: TRect);
    procedure SetCurrentGraphicPages(const LeftSide, RightSide: TGraphic; LeftSideTransparentColor, RightSideTransparentColor: TColor);
    procedure PrepareLeftPageBehind(PageRect: TRect);
    procedure PrepareRightPageBehind(PageRect: TRect);
    procedure PrepareLeftPage(PageRect: TRect);
    procedure PrepareRightPage(PageRect: TRect);
    function AdjustPageRenderForBmp(aBmp: TBitmap; PageRect: TRect; PageMargins: Integer): TPageRender;
    function ViewNext2Pages(const AnimationFrames: Word = 20; const AnimationDuration: Word = 300): Boolean;
    function ViewPrior2Pages(const AnimationFrames: Word = 20; const AnimationDuration: Word = 300): Boolean;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property Height nodefault;
    property Width nodefault;
    property Borders: TcyBevels read FBorders write SetBorders;
    property FromColor: TColor read FFromColor write SetFromColor;
    property ToColor: TColor read FToColor write SetToColor;
    property HeadBand: THeadBand read FHeadBand write SetHeadBand;
    property Options: TBookOptions read FOptions write SetOptions default [boInvalidatePagesOnResize, boFitSmallImages, boStretchSmallImages];
    property LeftPageView: TBookPageView read FLeftPageView write SetLeftPageView;
    property RightPageView: TBookPageView read FRightPageView write SetRightPageView;
    property PageRender: TPageRender read FPageRender write FPageRender default prFitHighQuality;
    property Pages: Integer read FPages write SetPages;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property TurnPageAnimation: TTurnPageAnimation read FTurnPageAnimation write SetTurnPageAnimation;
    property CurrentLeftPage: Integer read FCurrentLeftPage write SetCurrentLeftPage;
    property CurrentRightPage: Integer read FCurrentRightPage write SetCurrentRightPage;
    property OnLeftPageChange: TNotifyEvent read FOnLeftPageChange write FOnLeftPageChange;
    property OnLeftPageClick: TNotifyEvent read FOnLeftPageClick write FOnLeftPageClick;
    property OnRightPageClick: TNotifyEvent read FOnRightPageClick write FOnRightPageClick;
    property OnRightPageChange: TNotifyEvent read FOnRightPageChange write FOnRightPageChange;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnNeedPage: TProcOnNeedPage read FOnNeedPage write FOnNeedPage;
    property BeforePreparePage: TProcBeforePreparePage read FBeforePreparePage write FBeforePreparePage;
    property AfterPreparePage: TProcAfterPreparePage read FAfterPreparePage write FAfterPreparePage;
  end;


implementation

{ TBookPageView }
constructor TBookPageView.Create(AOwner: TComponent);
begin
  FOwner := TcyBook(AOwner);

  FOriginalPage := TBitmap.Create;
  FOriginalPageBehind := TBitmap.Create;
  FPage := TBitmap.Create;
  FPageBehind := TBitmap.Create;
  FWallpaperOverPage := TcyBgPicture.Create(FOwner);
  FWallpaperOverPage.OnChange := WallpaperChanged;
  FWallpaperUnderPage := TcyBgPicture.Create(FOwner);
  FWallpaperUnderPage.OnChange := WallpaperChanged;

  FMargins := 0;
  FPaperColor := clWhite;
  FPaperShadeColor := clGray;
  FInvalidOriginalPage := true;
  FInvalidOriginalPageBehind := true;
  FInvalidPage := true;
  FInvalidPageBehind := true;
end;

destructor TBookPageView.Destroy;
begin
  FOriginalPage.Free;
  FOriginalPageBehind.Free;
  FPage.Free;
  FPageBehind.Free;
  FWallpaperOverPage.Free;
  FWallpaperUnderPage.Free;
  inherited;
end;

procedure TBookPageView.WallpaperChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

procedure TBookPageView.InvalidateOriginalPage;
begin
  FInvalidOriginalPage := true;
end;

procedure TBookPageView.InvalidateOriginalPageBehind;
begin
  FInvalidOriginalPageBehind := true;
end;

procedure TBookPageView.InvalidatePage;
begin
  FInvalidPage := true;
end;

procedure TBookPageView.InvalidatePageBehind;
begin
  FInvalidPageBehind := true;
end;

procedure TBookPageView.SetPage(const withGraphic: TGraphic; TransparentColor: TColor);
begin
  FInvalidPage := true;

  if withGraphic = Nil then
  begin
    FInvalidOriginalPage := true;
    Exit;
  end;

  if withGraphic.Empty then
  begin
    FInvalidOriginalPage := true;
    Exit;
  end;

  FOriginalPage.Assign(withGraphic);
  FOriginalPage.Transparent := TransparentColor <> clNone;
  if FOriginalPage.Transparent then
    FOriginalPage.TransparentColor := TransparentColor;
  FInvalidOriginalPage := false;
// Don't ... FOwner.Invalidate;
end;

procedure TBookPageView.SetPageBehind(const withGraphic: TGraphic; TransparentColor: TColor);
begin
  FInvalidPageBehind := true;

  if withGraphic = Nil then
  begin
    FInvalidOriginalPage := true;
    Exit;
  end;

  if withGraphic.Empty then
  begin
    FInvalidOriginalPage := true;
    Exit;
  end;

  FOriginalPageBehind.Assign(withGraphic);
  FOriginalPageBehind.Transparent := TransparentColor <> clNone;
  if FOriginalPageBehind.Transparent then
    FOriginalPageBehind.TransparentColor := TransparentColor;
  FInvalidOriginalPageBehind := false;
// Don't  FOwner.Invalidate;
end;

procedure TBookPageView.SetPaperColor(const Value: TColor);
begin
  FPaperColor := Value;
  FOwner.Invalidate;
end;

procedure TBookPageView.SetPaperShadeColor(const Value: TColor);
begin
  FPaperShadeColor := Value;
  FOwner.Invalidate;
end;

procedure TBookPageView.SetWallpaperOverPage(const Value: TcyBgPicture);
begin
  FWallpaperOverPage := Value;
  FOwner.Invalidate;
end;

procedure TBookPageView.SetWallpaperUnderPage(const Value: TcyBgPicture);
begin
  FWallpaperUnderPage := Value;
  FOwner.Invalidate;
end;

{ THeadBand }
constructor THeadBand.Create(AOwner: TComponent);
begin
  FOwner := TcyBook(AOwner);
  FPageSpacing := 0;
  FWidth := 20;
end;

destructor THeadBand.Destroy;
begin
  inherited;
end;

procedure THeadBand.SetPageSpacing(const Value: Integer);
begin
  FPageSpacing := Value;
  FOwner.Invalidate;
end;

procedure THeadBand.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  FOwner.Invalidate;
end;

{ TTurnPageAnimation }
constructor TTurnPageAnimation.Create(AOwner: TComponent);
begin
  FOwner := TcyBook(AOwner);
  FActive := false;
  FDoubleBuffered := true;
  FDrawPageBehind := true;
  FEnabled := true;
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FPageBehind  := TBitmap.Create;
  FPageBehind.PixelFormat := pf32bit;
  FBookImage := TBitmap.Create;
  FBookImage.PixelFormat := pf32bit;
end;

destructor TTurnPageAnimation.Destroy;
begin
  FBookImage.Free;
  FPageBehind.Free;
  FBuffer.Free;
  inherited;
end;

procedure TTurnPageAnimation.SetActive(const Value: Boolean);
var Rect, LeftPageRect, RightPageRect, HeadBandRect: TRect;
begin
  if Factive = Value then Exit;
  Factive := Value;

  if FActive then
  begin
    // Set Buffer size :
    FBuffer.Width := FOwner.Width;
    FBuffer.Height := FOwner.Height;

    // Take "screenshoot" of component :
    FBookImage.Width := FOwner.Width;
    FBookImage.Height := FOwner.Height;
    Rect := FOwner.ClientRect;
    FBookImage.Canvas.CopyRect(Rect, FOwner.Canvas, Rect);

    // Set Page behind bitmap for turn page effect:
    if FDrawPageBehind then
    begin
      FOwner.CalcRects(Rect, LeftPageRect, RightPageRect, HeadBandRect);
      FPageBehind.Width := FOwner.Width;
      FPageBehind.Height := FOwner.Height;

      if FAnimatedPage = pmLeftPage then
      begin
        if not FOwner.FLeftPageView.FInvalidPageBehind then
        begin
          FPageBehind.Canvas.Brush.Color := FOwner.FLeftPageView.FPaperColor;
          FPageBehind.Canvas.FillRect(Rect);

          if not FOwner.FLeftPageView.FPageBehind.Empty then
            FOwner.DrawPage(FOwner.FLeftPageView.FPageBehind, LeftPageRect, FOwner.FLeftPageView.FMargins, FOwner.FLeftPageView.FPageBehindRender, FPageBehind.Canvas);
        end;
      end
      else begin
        if not FOwner.FRightPageView.FInvalidPageBehind then
        begin
          FPageBehind.Canvas.Brush.Color := FOwner.FRightPageView.FPaperColor;
          FPageBehind.Canvas.FillRect(Rect);

          if not FOwner.FRightPageView.FPageBehind.Empty then
            FOwner.DrawPage(FOwner.FRightPageView.FPageBehind, RightPageRect, FOwner.FRightPageView.FMargins, FOwner.FRightPageView.FPageBehindRender, FPageBehind.Canvas);
        end;
      end;
    end;

  end
  else
    FOwner.Invalidate;
end;

procedure TTurnPageAnimation.SetPercentDone(const Value: Integer);
begin
  FPercentDone := Value;
  if FPercentDone <   0 then FPercentDone :=   0;
  if FPercentDone > 100 then FPercentDone := 100;
end;

{ TcyBook }
constructor TcyBook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorders := TcyBevels.Create(self, TcyBevel);
  FHeadBand := THeadBand.Create(Self);
  FLeftPageView := TBookPageView.Create(Self);
  FRightPageView := TBookPageView.Create(Self);
  FTurnPageAnimation := TTurnPageAnimation.Create(Self);

  FMouseDown := false;
  FOptions := [boInvalidatePagesOnResize, boFitSmallImages, boStretchSmallImages];
  FPageRender := prFitHighQuality;
  FPages := 1;
  FReadOnly := false;
  FCurrentLeftPage := 0;
  FCurrentRightPage := 1;

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil then
    begin
      if not (csLoading in Owner.ComponentState) then // We have just added the component at design time
      begin
        with FBorders.Add do    // Frame
        begin
          HighlightColor := $00005B5B;
          ShadowColor := $00004242;
        end;

        with FBorders.Add do
        begin
          HighlightColor := $00009B9B;
          ShadowColor := $00007171;
          Width := 2;
        end;

        with FBorders.Add do
        begin
          Style := bcTransparent;
          Width := 5;
        end;
      end;

      FLeftPageView.WallpaperOverPage.Position := bgCenterRight;
      FLeftPageView.WallpaperUnderPage.Position := bgCenterRight;
      FRightPageView.WallpaperOverPage.Position := bgCenterLeft;
      FRightPageView.WallpaperUnderPage.Position := bgCenterLeft;
    end;

  FromColor := clOlive;
  ToColor := $00005555;
  Height := 210;
  Width := 300;
end;

destructor TcyBook.Destroy;
begin
  FBorders.Free;
  FBorders := Nil;
  FHeadBand.Free;
  FHeadBand := Nil;
  FLeftPageView.Free;
  FLeftPageView := Nil;
  FRightPageView.Free;
  FRightPageView := Nil;
  FTurnPageAnimation.Free;
  FTurnPageAnimation := Nil;
  inherited;
end;

procedure TcyBook.SetBorders(const Value: TcyBevels);
begin
  FBorders := Value;
end;

procedure TcyBook.SetFromColor(const Value: TColor);
begin
  FFromColor := Value;
  Invalidate;
end;

procedure TcyBook.SetHeadBand(const Value: THeadBand);
begin
  FHeadBand := Value;
end;

procedure TcyBook.SetLeftPageView(const Value: TBookPageView);
begin
  FLeftPageView := Value;
end;

procedure TcyBook.SetOptions(const Value: TBookOptions);
begin
  FOptions := Value;
end;

procedure TcyBook.SetRightPageView(const Value: TBookPageView);
begin
  FRightPageView := Value;
end;

procedure TcyBook.SetToColor(const Value: TColor);
begin
  FToColor := Value;
  Invalidate;
end;

procedure TcyBook.SetTurnPageAnimation(const Value: TTurnPageAnimation);
begin
  FTurnPageAnimation := Value;
end;

procedure TcyBook.SetCurrentLeftPage(const Value: Integer);
begin
  if Value < 0 then Exit;
  if Value = FCurrentLeftPage then Exit;

  // Left page behind will be left page ?
  if (Value = FCurrentLeftPage - 2) then
  begin
    FLeftPageView.FInvalidOriginalPage := true;
    FLeftPageView.FInvalidPage := true;

    if not FLeftPageView.FInvalidOriginalPageBehind then
    begin
      FLeftPageView.FInvalidOriginalPage := false;
      FLeftPageView.FOriginalPage.Assign(FLeftPageView.FOriginalPageBehind);
    end;

    if not FLeftPageView.FInvalidPageBehind then
    begin
      FLeftPageView.FInvalidPage := false;
      FLeftPageView.FPage.Assign(FLeftPageView.FPageBehind);
      FLeftPageView.FPageRender := FLeftPageView.FPageBehindRender;
    end;

    FLeftPageView.FInvalidOriginalPageBehind := true;
    FLeftPageView.FInvalidPageBehind := true;
  end
  else begin
    FLeftPageView.FInvalidOriginalPageBehind := true;
    FLeftPageView.FInvalidPageBehind := true;

    // Current left page will be left page behind?
    if (Value = FCurrentLeftPage + 2) then
    begin
      if not FLeftPageView.FInvalidOriginalPage then
      begin
        FLeftPageView.FInvalidOriginalPageBehind := false;
        FLeftPageView.FOriginalPageBehind.Assign(FLeftPageView.FOriginalPage);
      end;

      if not FLeftPageView.FInvalidPage then
      begin
        FLeftPageView.FInvalidPageBehind := false;
        FLeftPageView.FPageBehind.Assign(FLeftPageView.FPage);
        FLeftPageView.FPageBehindRender := FLeftPageView.FPageRender;
      end;
    end;

    FLeftPageView.FInvalidOriginalPage := true;
    FLeftPageView.FInvalidPage := true;
  end;


  FCurrentLeftPage := Value;

  // Set FPages :
  if FPages < Value then
    FPages := Value;

  if Assigned(FOnLeftPageChange) then
    FOnLeftPageChange(Self);

  // Set right page number :
  if Value + 1 <= FPages
  then CurrentRightPage := Value + 1
  else CurrentRightPage := 0;

  Invalidate;
end;

procedure TcyBook.SetCurrentRightPage(const Value: Integer);
begin
  if Value < 0 then Exit;
  if Value = FCurrentRightPage then Exit;

  // Right page behind will be Right page ?
  if (Value = FCurrentRightPage + 2) then
  begin
    FRightPageView.FInvalidOriginalPage := true;
    FRightPageView.FInvalidPage := true;

    if not FRightPageView.FInvalidOriginalPageBehind then
    begin
      FRightPageView.FInvalidOriginalPage := false;
      FRightPageView.FOriginalPage.Assign(FRightPageView.FOriginalPageBehind);
    end;

    if not FRightPageView.FInvalidPageBehind then
    begin
      FRightPageView.FInvalidPage := false;
      FRightPageView.FPage.Assign(FRightPageView.FPageBehind);
      FRightPageView.FPageRender := FRightPageView.FPageBehindRender;
    end;

    FRightPageView.FInvalidOriginalPageBehind := true;
    FRightPageView.FInvalidPageBehind := true;
  end
  else begin
    FRightPageView.FInvalidOriginalPageBehind := true;
    FRightPageView.FInvalidPageBehind := true;

    // Current Right page will be Right page behind?
    if (Value = FCurrentRightPage - 2) then
    begin
      if not FRightPageView.FInvalidOriginalPage then
      begin
        FRightPageView.FInvalidOriginalPageBehind := false;
        FRightPageView.FOriginalPageBehind.Assign(FRightPageView.FOriginalPage);
      end;

      if not FRightPageView.FInvalidPage then
      begin
        FRightPageView.FInvalidPageBehind := false;
        FRightPageView.FPageBehind.Assign(FRightPageView.FPage);
        FRightPageView.FPageBehindRender := FRightPageView.FPageRender;
      end;
    end;

    FRightPageView.FInvalidOriginalPage := true;
    FRightPageView.FInvalidPage := true;
  end;


  FCurrentRightPage := Value;

  // Set FPages :
  if FPages < Value then
    FPages := Value;

  if Assigned(FOnRightPageChange) then
    FOnRightPageChange(Self);

  // Set left page number :
  if Value = 0
  then CurrentLeftPage := FPages
  else CurrentLeftPage := Value - 1;

  Invalidate;
end;

// !!! FPages set on SetCurrentLeftPage and on SetCurrentRightPage !!!
procedure TcyBook.SetPages(const Value: Integer);
begin
  if Value < 0 then Exit;
  if Value = FPages then Exit;

  FPages := Value;

  // No pages now :
  if FPages = 0 then
  begin
    CurrentLeftPage := 0;
    Exit;
  end;

  // No pages before :
  if (FCurrentLeftPage = 0) and (FCurrentRightPage = 0) then
  begin
    CurrentRightPage := 1;
    Exit;
  end;

  if FCurrentLeftPage = Value then
    CurrentRightPage := 0
  else
    if FCurrentLeftPage < Value then
      CurrentRightPage := FCurrentLeftPage + 1
    else begin
      // FCurrentLeftPage <> 0 :
      if FCurrentLeftPage mod 2 = 0 then
        // Left page is an Even number :
        CurrentLeftPage := (FPages div 2) * 2
      else
        // Left page is an odd number :
        if Value mod 2 = 0                    // Value is an Even number?
        then CurrentRightPage := FPages
        else CurrentLeftPage  := FPages;
    end;

  Invalidate;
end;

function TcyBook.CanViewPrior2Pages: Boolean;
begin
  Result := FCurrentLeftPage > 1;
end;

function TcyBook.CanViewNext2Pages: Boolean;
begin
  Result := (FCurrentRightPage <> FPages) and (FCurrentRightPage <> 0);
end;

procedure TcyBook.SetCurrentGraphicPages(const LeftSide, RightSide: TGraphic; LeftSideTransparentColor, RightSideTransparentColor: TColor);
begin
  if FCurrentLeftPage <> 0 then
    FLeftPageView.SetPage(LeftSide, LeftSideTransparentColor);

  if FCurrentRightPage <> 0 then
    FRightPageView.SetPage(RightSide, RightSideTransparentColor);

  Invalidate;
end;

procedure TcyBook.CalcRects(var BookRect, LeftPageRect, RightPageRect, HeadBandRect: TRect);
var
  PageWidth, i: Integer;
  TmpRect: TRect;
begin
  BookRect := ClientRect;
  TmpRect := BookRect;

  for i := 0 to FBorders.Count-1 do
    InflateRect(TmpRect, (-1) * FBorders.Items[i].Width, (-1) * FBorders.Items[i].Width);

  PageWidth := (TmpRect.Right - TmpRect.Left - HeadBand.FPageSpacing) div 2;
  LeftPageRect  := classes.Rect(TmpRect.Left, TmpRect.Top, TmpRect.Left + PageWidth, TmpRect.Bottom);
  RightPageRect := classes.Rect(LeftPageRect.Right + HeadBand.FPageSpacing, TmpRect.Top, TmpRect.Right, TmpRect.Bottom);
  HeadBandRect := classes.Rect(LeftPageRect.Right, TmpRect.Top, RightPageRect.Left, TmpRect.Bottom);
end;

procedure TcyBook.Resize;
begin
  // Invalidate pages ?
  if boInvalidatePagesOnResize in FOptions then
  begin
    FLeftPageView.InvalidatePage;
    FLeftPageView.InvalidatePageBehind;
    FRightPageView.InvalidatePage;
    FRightPageView.InvalidatePageBehind;
  end;

  inherited;
end;

// Paint the book (without animation) :
procedure TcyBook.Paint;
var
  Rect, LeftPageRect, RightPageRect, HeadBandRect, TmpRect: TRect;
  NeedIncRect: Integer;
begin
  // Define pages and headband Rects :
  CalcRects(Rect, LeftPageRect, RightPageRect, HeadBandRect);

  // *** Draw background *** //
  cyGradientFillVertical(Canvas, Rect, FromColor, ToColor, 255);

  // *** Draw under page wallpaper *** //

  // Left side:
  if FCurrentLeftPage = 0 then
    if ValidGraphic(FLeftPageView.FWallpaperUnderPage.Picture.Graphic) then
    begin
      TmpRect := classes.Rect(LeftPageRect.Left, LeftPageRect.Top, LeftPageRect.Right + (HeadBandRect.Right - HeadBandRect.Left) div 2, LeftPageRect.Bottom);

      if FLeftPageView.FWallpaperUnderPage.Style in [bgMosaic, bgNormal] then
      begin
        NeedIncRect := FLeftPageView.FWallpaperUnderPage.Picture.Graphic.Width - (TmpRect.Right - TmpRect.Left);
        if NeedIncRect > 0 then
          TmpRect.Left := TmpRect.Left - NeedIncRect;
      end;

      cyDrawBgPicture(Canvas, TmpRect, FLeftPageView.FWallpaperUnderPage);
    end;

  // Right side:
  if FCurrentRightPage = 0 then
    if ValidGraphic(FRightPageView.FWallpaperUnderPage.Picture.Graphic) then
    begin
      TmpRect := classes.Rect(RightPageRect.Left - (HeadBandRect.Right - HeadBandRect.Left) div 2, RightPageRect.Top, RightPageRect.Right, RightPageRect.Bottom);

      if FRightPageView.FWallpaperUnderPage.Style in [bgMosaic, bgNormal] then
      begin
        NeedIncRect := FRightPageView.FWallpaperUnderPage.Picture.Graphic.Width - (TmpRect.Right - TmpRect.Right);
        if NeedIncRect > 0 then
          TmpRect.Right := TmpRect.Right + NeedIncRect;
      end;

      cyDrawBgPicture(Canvas, TmpRect, FRightPageView.FWallpaperUnderPage);
    end;

  // *** Draw borders *** //
  FBorders.DrawBevels(Canvas, Rect, false);


  // *** Left page *** //
  DoDrawLeftSidePage(LeftPageRect);


  // *** Right page *** //
  DoDrawRightSidePage(RightPageRect);


  // *** Draw over page wallpaper *** //

  // Left side:
  if FCurrentLeftPage <> 0 then
    if ValidGraphic(FLeftPageView.FWallpaperOverPage.Picture.Graphic) then
    begin
      TmpRect := classes.Rect(LeftPageRect.Left, LeftPageRect.Top, LeftPageRect.Right + (HeadBandRect.Right - HeadBandRect.Left) div 2, LeftPageRect.Bottom);

      if FLeftPageView.FWallpaperOverPage.Style in [bgMosaic, bgNormal] then
      begin
        NeedIncRect := FLeftPageView.FWallpaperOverPage.Picture.Graphic.Width - (TmpRect.Right - TmpRect.Left);
        if NeedIncRect > 0 then
          TmpRect.Left := TmpRect.Left - NeedIncRect;
      end;

      cyDrawBgPicture(Canvas, TmpRect, FLeftPageView.FWallpaperOverPage);
    end;

  // Right side:
  if FCurrentRightPage <> 0 then
    if ValidGraphic(FRightPageView.FWallpaperOverPage.Picture.Graphic) then
    begin
      TmpRect := classes.Rect(RightPageRect.Left - (HeadBandRect.Right - HeadBandRect.Left) div 2, RightPageRect.Top, RightPageRect.Right, RightPageRect.Bottom);

      if FRightPageView.FWallpaperOverPage.Style in [bgMosaic, bgNormal] then
      begin
        NeedIncRect := FRightPageView.FWallpaperOverPage.Picture.Graphic.Width - (TmpRect.Right - TmpRect.Right);
        if NeedIncRect > 0 then
          TmpRect.Right := TmpRect.Right + NeedIncRect;
      end;

      cyDrawBgPicture(Canvas, TmpRect, FRightPageView.FWallpaperOverPage);
    end;

  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TcyBook.PrepareLeftPageBehind(PageRect: TRect);
var
  Handled: Boolean;
  FotoRect, OriginalRect: TRect;
  PageRendering: TPageRender;
begin
  if FCurrentLeftPage < 3 then Exit;

  FotoRect := PageRect;
  InflateRect(FotoRect, -LeftPageView.FMargins, -LeftPageView.FMargins);
  if (FotoRect.Left >= FotoRect.Right) or (FotoRect.Top >= FotoRect.Bottom) then Exit;

  if FLeftPageView.FInvalidOriginalPageBehind then
    if Assigned(FOnNeedPage) then
      FOnNeedPage(Self, FCurrentLeftPage - 2, pmBehindLeftPage);

  // Valid page ?
  if FLeftPageView.FInvalidPageBehind and (not FLeftPageView.FInvalidOriginalPageBehind) then
  begin
    Handled := false;
    PageRendering := AdjustPageRenderForBmp(FLeftPageView.FOriginalPageBehind, PageRect, FLeftPageView.FMargins);

    if Assigned(FBeforePreparePage) then
      FBeforePreparePage(Self, FCurrentLeftPage-2, pmBehindLeftPage, PageRect, PageRendering, Handled);

    FLeftPageView.FPageBehindRender := PageRendering;

    if not Handled then
      if FLeftPageView.FPageBehindRender in [prFitHighQuality, prStretchHighQuality] then
      begin
        OriginalRect := classes.Rect(0, 0, FLeftPageView.FOriginalPageBehind.Width, FLeftPageView.FOriginalPageBehind.Height);

        if FLeftPageView.FPageBehindRender = prFitHighQuality then
          FotoRect := GetProportionalRect(OriginalRect, FotoRect);

        FLeftPageView.FPageBehind.Width := FotoRect.Right - FotoRect.Left;
        FLeftPageView.FPageBehind.Height := FotoRect.Bottom - FotoRect.Top;
        FLeftPageView.FPageBehind.PixelFormat := pf24bit;
        FLeftPageView.FPageBehind.Transparent := false;
        DrawCanvas(FLeftPageView.FPageBehind.Canvas, classes.Rect(0, 0, FLeftPageView.FPageBehind.Width, FLeftPageView.FPageBehind.Height),
                    FLeftPageView.FOriginalPageBehind.Canvas, OriginalRect);
      end
      else begin
        FLeftPageView.FPageBehind.Assign(FLeftPageView.FOriginalPageBehind);
        FLeftPageView.FPageBehind.PixelFormat := pf24bit; // if not, transparency don' t work!
      end;

    FLeftPageView.FPageBehind.TransparentColor := FLeftPageView.FOriginalPageBehind.TransparentColor;
    FLeftPageView.FPageBehind.Transparent := FLeftPageView.FOriginalPageBehind.Transparent;
    FLeftPageView.FInvalidPageBehind := false;

    if Assigned(FAfterPreparePage) then
      FAfterPreparePage(Self, FCurrentLeftPage-2, pmBehindLeftPage);
  end;
end;

procedure TcyBook.PrepareRightPageBehind(PageRect: TRect);
var
  Handled: Boolean;
  FotoRect, OriginalRect: TRect;
  PageRendering: TPageRender;
begin
  if (FCurrentRightPage = 0) or (FCurrentRightPage + 2 > FPages) then Exit;

  FotoRect := PageRect;
  InflateRect(FotoRect, -RightPageView.FMargins, -RightPageView.FMargins);
  if (FotoRect.Left >= FotoRect.Right) or (FotoRect.Top >= FotoRect.Bottom) then Exit;

  if FRightPageView.FInvalidOriginalPageBehind then
    if Assigned(FOnNeedPage) then
      FOnNeedPage(Self, FCurrentRightPage + 2, pmBehindRightPage);

  // Valid page ?
  if FRightPageView.FInvalidPageBehind and (not FRightPageView.FInvalidOriginalPageBehind) then
  begin
    Handled := false;
    PageRendering := AdjustPageRenderForBmp(FRightPageView.FOriginalPageBehind, PageRect, FRightPageView.FMargins);

    if Assigned(FBeforePreparePage) then
      FBeforePreparePage(Self, FCurrentRightPage+2, pmBehindRightPage, PageRect, PageRendering, Handled);

    FRightPageView.FPageBehindRender := PageRendering;

    if not Handled then
      if FRightPageView.FPageBehindRender in [prFitHighQuality, prStretchHighQuality] then
      begin
        OriginalRect := classes.Rect(0, 0, FRightPageView.FOriginalPageBehind.Width, FRightPageView.FOriginalPageBehind.Height);

        if FRightPageView.FPageBehindRender = prFitHighQuality then
          FotoRect := GetProportionalRect(OriginalRect, FotoRect);

        FRightPageView.FPageBehind.Width := FotoRect.Right - FotoRect.Left;
        FRightPageView.FPageBehind.Height := FotoRect.Bottom - FotoRect.Top;
        FRightPageView.FPageBehind.PixelFormat := pf24bit;
        FRightPageView.FPageBehind.Transparent := false;
        DrawCanvas(FRightPageView.FPageBehind.Canvas, classes.Rect(0, 0, FRightPageView.FPageBehind.Width, FRightPageView.FPageBehind.Height),
                    FRightPageView.FOriginalPageBehind.Canvas, OriginalRect);
      end
      else begin
        FRightPageView.FPageBehind.Assign(FRightPageView.FOriginalPageBehind);
        FRightPageView.FPageBehind.PixelFormat := pf24bit; // if not, transparency don' t work!
      end;

    FRightPageView.FPageBehind.TransparentColor := FRightPageView.FOriginalPageBehind.TransparentColor;
    FRightPageView.FPageBehind.Transparent := FRightPageView.FOriginalPageBehind.Transparent;
    FRightPageView.FInvalidPageBehind := false;

    if Assigned(FAfterPreparePage) then
      FAfterPreparePage(Self, FCurrentRightPage+2, pmBehindRightPage);
  end;
end;

procedure TcyBook.PrepareLeftPage(PageRect: TRect);
var
  Handled: Boolean;
  FotoRect, OriginalRect: TRect;
  PageRendering: TPageRender;
begin
  if FCurrentLeftPage <= 0 then Exit;

  FotoRect := PageRect;
  InflateRect(FotoRect, -LeftPageView.FMargins, -LeftPageView.FMargins);
  if (FotoRect.Left >= FotoRect.Right) or (FotoRect.Top >= FotoRect.Bottom) then Exit;

  // Valid original page ?
  if FLeftPageView.FInvalidOriginalPage then
    if Assigned(FOnNeedPage) then
      FOnNeedPage(Self, CurrentLeftPage, pmLeftPage);

  // Valid page ?
  if FLeftPageView.FInvalidPage and (not FLeftPageView.FInvalidOriginalPage) then
  begin
    Handled := false;
    PageRendering := AdjustPageRenderForBmp(FLeftPageView.FOriginalPage, PageRect, FLeftPageView.FMargins);

    if Assigned(FBeforePreparePage) then
      FBeforePreparePage(Self, FCurrentLeftPage, pmLeftPage, PageRect, PageRendering, Handled);

    FLeftPageView.FPageRender := PageRendering;

    if not Handled then
      if FLeftPageView.FPageRender in [prFitHighQuality, prStretchHighQuality] then
      begin
        OriginalRect := classes.Rect(0, 0, FLeftPageView.FOriginalPage.Width, FLeftPageView.FOriginalPage.Height);

        if FLeftPageView.FPageRender = prFitHighQuality then
          FotoRect := GetProportionalRect(OriginalRect, FotoRect);

        FLeftPageView.FPage.Width := FotoRect.Right - FotoRect.Left;
        FLeftPageView.FPage.Height := FotoRect.Bottom - FotoRect.Top;
        FLeftPageView.FPage.PixelFormat := pf24bit;
        FLeftPageView.FPage.Transparent := false;
        DrawCanvas(FLeftPageView.FPage.Canvas, classes.Rect(0, 0, FLeftPageView.FPage.Width, FLeftPageView.FPage.Height),
                    FLeftPageView.FOriginalPage.Canvas, OriginalRect);
      end
      else begin
        FLeftPageView.FPage.Assign(FLeftPageView.FOriginalPage);
        FLeftPageView.FPage.PixelFormat := pf24bit; // if not, transparency don' t work!
      end;

    FLeftPageView.FPage.TransparentColor := FLeftPageView.FOriginalPage.TransparentColor;
    FLeftPageView.FPage.Transparent := FLeftPageView.FOriginalPage.Transparent;
    FLeftPageView.FInvalidPage := false;

    if Assigned(FAfterPreparePage) then
      FAfterPreparePage(Self, FCurrentLeftPage, pmLeftPage);
  end;
end;

procedure TcyBook.PrepareRightPage(PageRect: TRect);
var
  Handled: Boolean;
  FotoRect, OriginalRect: TRect;
  PageRendering: TPageRender;
begin
  if FCurrentRightPage <= 0 then Exit;

  FotoRect := PageRect;
  InflateRect(FotoRect, -RightPageView.FMargins, -RightPageView.FMargins);
  if (FotoRect.Left >= FotoRect.Right) or (FotoRect.Top >= FotoRect.Bottom) then Exit;

  // Valid original page ?
  if FRightPageView.FInvalidOriginalPage then
    if Assigned(FOnNeedPage) then
      FOnNeedPage(Self, CurrentRightPage, pmRightPage);

  // Valid page ?
  if FRightPageView.FInvalidPage and (not FRightPageView.FInvalidOriginalPage) then
  begin
    Handled := false;
    PageRendering := AdjustPageRenderForBmp(FRightPageView.FOriginalPage, PageRect, FRightPageView.FMargins);

    if Assigned(FBeforePreparePage) then
      FBeforePreparePage(Self, FCurrentRightPage, pmRightPage, PageRect, PageRendering, Handled);

    FRightPageView.FPageRender := PageRendering;

    if not Handled then
      if FRightPageView.FPageRender in [prFitHighQuality, prStretchHighQuality] then
      begin
        OriginalRect := classes.Rect(0, 0, FRightPageView.FOriginalPage.Width, FRightPageView.FOriginalPage.Height);

        if FRightPageView.FPageRender = prFitHighQuality then
          FotoRect := GetProportionalRect(OriginalRect, FotoRect);

        FRightPageView.FPage.Width := FotoRect.Right - FotoRect.Left;
        FRightPageView.FPage.Height := FotoRect.Bottom - FotoRect.Top;
        FRightPageView.FPage.PixelFormat := pf24bit;
        FRightPageView.FPage.Transparent := false;
        DrawCanvas(FRightPageView.FPage.Canvas, classes.Rect(0, 0, FRightPageView.FPage.Width, FRightPageView.FPage.Height),
                    FRightPageView.FOriginalPage.Canvas, OriginalRect);
      end
      else begin
        FRightPageView.FPage.Assign(FRightPageView.FOriginalPage);
        FRightPageView.FPage.PixelFormat := pf24bit; // if not, transparency don' t work!
      end;

    FRightPageView.FPage.TransparentColor := FRightPageView.FOriginalPage.TransparentColor;
    FRightPageView.FPage.Transparent := FRightPageView.FOriginalPage.Transparent;
    FRightPageView.FInvalidPage := false;

    if Assigned(FAfterPreparePage) then
      FAfterPreparePage(Self, FCurrentRightPage, pmRightPage);
  end;
end;

function TcyBook.AdjustPageRenderForBmp(aBmp: TBitmap; PageRect: TRect; PageMargins: Integer): TPageRender;
var FotoRect: TRect;
begin
  Result := FPageRender;  // Default
  FotoRect := PageRect;
  InflateRect(FotoRect, -PageMargins, -PageMargins);

  // Adjust page rendering :
  if not (boFitSmallImages in FOptions) then
    if Result in [prFitHighQuality, prFitLowQuality] then
      if (aBmp.Height < FotoRect.Bottom - FotoRect.Top) and (aBmp.Width < FotoRect.Right - FotoRect.Left) then
        Result := prCentered;

  if not (boStretchSmallImages in FOptions) then
    if Result in [prStretchHighQuality, prStretchLowQuality] then
      if (aBmp.Height < FotoRect.Bottom - FotoRect.Top) and (aBmp.Width < FotoRect.Right - FotoRect.Left) then
        Result := prCentered;
end;

procedure TcyBook.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Rect, LeftPageRect, RightPageRect, HeadBandRect: TRect;
  aPoint: TPoint;
begin
  // We need to copy actual view to a buffer :
  FMouseDown := true;
  FMouseDownPosition := Point(x, y);

  // Activate animation :
  if (not FReadOnly) and (not FTurnPageAnimation.FActive) then
  begin
     CalcRects(Rect, LeftPageRect, RightPageRect, HeadBandRect);
     aPoint := Point(x, y);

     // Left page ?
    if CanViewPrior2Pages then
      if PointInRect(aPoint, LeftPageRect) then
      begin
        FTurnPageAnimation.FAnimatedPage := pmLeftPage;
        FTurnPageAnimation.Active := true;
      end;

    // Right page ?
    if CanViewNext2Pages then
      if PointInRect(aPoint, RightPageRect) then
      begin
        FTurnPageAnimation.FAnimatedPage := pmRightPage;
        FTurnPageAnimation.Active := true;
      end;

    if FTurnPageAnimation.Active then
      FTurnPageAnimation.PercentDone := 0;
  end;

  inherited;
end;

procedure TcyBook.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Rect, LeftPageRect, RightPageRect, HeadBandRect: TRect;
  aPoint: TPoint;
begin
  inherited;

  CalcRects(Rect, LeftPageRect, RightPageRect, HeadBandRect);
  aPoint := Point(x, y);

  // *** Always Load page behind *** //

  // Left page?
  if PointInRect(aPoint, LeftPageRect) then
    PrepareLeftPageBehind(LeftPageRect);

  // Right page?
  if PointInRect(aPoint, RightPageRect) then
    PrepareRightPageBehind(RightPageRect);



  if FTurnPageAnimation.FActive then
  begin
    // Calc PercentDone turn page complete (between 0 and 100, that correspond to page with):
    if FTurnPageAnimation.FAnimatedPage = pmRightPage then
    begin
      if RightPageRect.Right <> RightPageRect.Left
      then FTurnPageAnimation.PercentDone := Round((x - RightPageRect.Right) / (RightPageRect.Left - RightPageRect.Right) * 100)
      else FTurnPageAnimation.PercentDone := 0;
    end
    else begin
      if LeftPageRect.Right <> LeftPageRect.Left
      then FTurnPageAnimation.PercentDone := Round((x - LeftPageRect.Left) / (LeftPageRect.Right - LeftPageRect.Left) * 100)
      else FTurnPageAnimation.PercentDone := 0;
    end;

    if FTurnPageAnimation.FEnabled then
    begin
      // *** Draw animation *** //
      if FTurnPageAnimation.PercentDone <> 0 then
        if FTurnPageAnimation.FAnimatedPage = pmRightPage
        then DoDrawRightTurnPageEffect(RightPageRect, FTurnPageAnimation.PercentDone)
        else DoDrawLeftTurnPageEffect(LeftPageRect, FTurnPageAnimation.PercentDone);
    end;
  end;
end;

procedure TcyBook.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := false;

  // PageClick event :
  if Abs(X - FMouseDownPosition.X) + Abs(Y - FMouseDownPosition.Y) < 5 then   // Mouse moved ?
  begin
    if FTurnPageAnimation.FAnimatedPage = pmLeftPage then
    begin
      if Assigned(FOnLeftPageClick) then
        FOnLeftPageClick(Self);
    end
    else
      if Assigned(FOnRightPageClick) then
        FOnRightPageClick(Self);
  end;

  // Let developper handle turn page on the OnMouseUp:
  inherited;

  // If developper don' t deactivate the turn page effect :
  if FTurnPageAnimation.FActive then
  begin
    FTurnPageAnimation.Active := false;

    if FTurnPageAnimation.PercentDone > 50 then
      if Abs(X - FMouseDownPosition.X) >= 5 then   // Mouse moved horizontally ?
      if FTurnPageAnimation.FAnimatedPage = pmRightPage
      then ViewNext2Pages(0, 0)
      else ViewPrior2Pages(0, 0);
  end;
end;

procedure TcyBook.New(BookPages: Integer);
begin
  FLeftPageView.InvalidateOriginalPage;
  FLeftPageView.InvalidateOriginalPage;
  FLeftPageView.InvalidatePage;
  FLeftPageView.InvalidatePage;

  FRightPageView.InvalidateOriginalPage;
  FRightPageView.InvalidateOriginalPage;
  FRightPageView.InvalidatePage;
  FRightPageView.InvalidatePage;

  Pages := BookPages;

  if FPages > 0 then
    CurrentRightPage := 1;  // First page at right by default ...
end;

procedure TcyBook.DoDrawLeftTurnPageEffect(PageRect: TRect; PercentDone: Integer);
var
  FoldLine: TLineCoord;
  TopPageCorner, BottomPageCorner: TPoint;
begin
  if FTurnPageAnimation.FDoubleBuffered or FTurnPageAnimation.FDrawPageBehind then
  begin
    // Draw BookImage:
    FTurnPageAnimation.FBuffer.Canvas.Draw(0, 0, FTurnPageAnimation.FBookImage);

    // Draw page behind bitmap:
    if FTurnPageAnimation.FDrawPageBehind and (not FLeftPageView.FInvalidPageBehind) then
    begin
      FoldLine := DrawLeftTurnPageEffect(FTurnPageAnimation.FBuffer.Canvas, FLeftPageView.FPaperColor, PageRect, PercentDone, true);
      FTurnPageAnimation.FBuffer.Canvas.Brush.Bitmap := FTurnPageAnimation.FPageBehind;
      TopPageCorner := Point(PageRect.Left, PageRect.Top);
      BottomPageCorner := Point(PageRect.Left, PageRect.Bottom);
      FTurnPageAnimation.FBuffer.Canvas.Pen.Width := 0;
      FTurnPageAnimation.FBuffer.Canvas.Pen.Style := psClear;
      FTurnPageAnimation.FBuffer.Canvas.Polygon([FoldLine.BottomCoord, FoldLine.TopCoord, TopPageCorner, BottomPageCorner]);
      FTurnPageAnimation.FBuffer.Canvas.Pen.Style := psSolid;
      FTurnPageAnimation.FBuffer.Canvas.Brush.Bitmap := Nil;
    end;

    DrawLeftTurnPageEffect(FTurnPageAnimation.FBuffer.Canvas, FLeftPageView.FPaperColor, PageRect, PercentDone);
    Canvas.Draw(0, 0, FTurnPageAnimation.FBuffer);
  end
  else begin
    // Draw BookImage:
    Canvas.Draw(0, 0, FTurnPageAnimation.FBookImage);
    DrawLeftTurnPageEffect(Canvas, FLeftPageView.FPaperColor, PageRect, PercentDone);
  end;
end;

procedure TcyBook.DoDrawRightTurnPageEffect(PageRect: TRect; PercentDone: Integer);
var
  FoldLine: TLineCoord;
  TopPageCorner, BottomPageCorner: TPoint;
begin
  if FTurnPageAnimation.FDoubleBuffered or FTurnPageAnimation.FDrawPageBehind then
  begin
    FTurnPageAnimation.FBuffer.Canvas.Draw(0, 0, FTurnPageAnimation.FBookImage);

    // Draw page behind bitmap:
    if FTurnPageAnimation.FDrawPageBehind and (not FRightPageView.FInvalidPageBehind) then
    begin
      FoldLine := DrawRightTurnPageEffect(FTurnPageAnimation.FBuffer.Canvas, FRightPageView.FPaperColor, PageRect, PercentDone, true);
      FTurnPageAnimation.FBuffer.Canvas.Brush.Bitmap := FTurnPageAnimation.FPageBehind;
      TopPageCorner := Point(PageRect.Right, PageRect.Top);
      BottomPageCorner := Point(PageRect.Right, PageRect.Bottom);
      FTurnPageAnimation.FBuffer.Canvas.Pen.Width := 0;
      FTurnPageAnimation.FBuffer.Canvas.Pen.Style := psClear;
      FTurnPageAnimation.FBuffer.Canvas.Polygon([FoldLine.BottomCoord, FoldLine.TopCoord, TopPageCorner, BottomPageCorner]);
      FTurnPageAnimation.FBuffer.Canvas.Pen.Style := psSolid;
      FTurnPageAnimation.FBuffer.Canvas.Brush.Bitmap := Nil;
    end;

    DrawRightTurnPageEffect(FTurnPageAnimation.FBuffer.Canvas, FRightPageView.FPaperColor, PageRect, PercentDone);
    Canvas.Draw(0, 0, FTurnPageAnimation.FBuffer);
  end
  else begin
    // Draw BookImage:
    Canvas.Draw(0, 0, FTurnPageAnimation.FBookImage);
    DrawRightTurnPageEffect(Canvas, FRightPageView.FPaperColor, PageRect, PercentDone);
  end;
end;

function TcyBook.ViewNext2Pages(const AnimationFrames: Word; const AnimationDuration: Word): Boolean;
var
  i: Integer;
  Rect, LeftPageRect, RightPageRect, HeadBandRect: TRect;
begin
  Result := false;
  if not CanViewNext2Pages then Exit;

  // *** Play animation *** //
  if AnimationFrames > 0 then
  begin
    CalcRects(Rect, LeftPageRect, RightPageRect, HeadBandRect);
    PrepareRightPage(RightPageRect);
    if FTurnPageAnimation.FDrawPageBehind then
      PrepareRightPageBehind(RightPageRect);
    FTurnPageAnimation.FAnimatedPage := pmRightPage;
    FTurnPageAnimation.Active := true;

    for i := 1 to AnimationFrames do
    begin
      DoDrawRightTurnPageEffect(RightPageRect, Round(i / AnimationFrames * 100));

      if AnimationDuration <> 0 then
        Sleep(AnimationDuration div AnimationFrames);
    end;

    FTurnPageAnimation.Active := false;
  end;

  // *** Set page number *** //
  Result := true;
  CurrentLeftPage := FCurrentLeftPage + 2;
end;

function TcyBook.ViewPrior2Pages(const AnimationFrames: Word; const AnimationDuration: Word): Boolean;
var
  i: Integer;
  Rect, LeftPageRect, RightPageRect, HeadBandRect: TRect;
begin
  Result := false;
  if not CanViewPrior2Pages then Exit;

  // *** Play animation *** //
  if AnimationFrames > 0 then
  begin
    CalcRects(Rect, LeftPageRect, RightPageRect, HeadBandRect);
    PrepareLeftPage(LeftPageRect);
    if FTurnPageAnimation.FDrawPageBehind then
      PrepareLeftPageBehind(LeftPageRect);
    FTurnPageAnimation.FAnimatedPage := pmLeftPage;
    FTurnPageAnimation.Active := true;

    for i := 1 to AnimationFrames do
    begin
      DoDrawLeftTurnPageEffect(LeftPageRect, Round(i / AnimationFrames * 100));

      if AnimationDuration <> 0 then
        Sleep(AnimationDuration div AnimationFrames);
    end;

    FTurnPageAnimation.Active := false;
  end;

  // *** Set page number *** //
  Result := true;
  CurrentRightPage := FCurrentLeftPage - 1;
end;

procedure TcyBook.DrawPage(PageBmp: TBitmap; PageRect: TRect; PageMargins: Integer; PageRender: TPageRender; ToCanvas: TCanvas);
var
  bgStyle: TBgStyle;
begin
  InflateRect(PageRect, -PageMargins, -PageMargins);

  case PageRender of
    prFitHighQuality:
      begin
        if boInvalidatePagesOnResize in FOptions
        then bgStyle := bgNormal
        else bgStyle := bgStretchProportional;
      end;

    prFitLowQuality:
      bgStyle := bgStretchProportional;

    prStretchHighQuality:
      begin
        if boInvalidatePagesOnResize in FOptions
        then bgStyle := bgNormal
        else bgStyle := bgStretch;
      end;

    prStretchLowQuality:
      bgStyle := bgStretch;

    prCentered:
      bgStyle := bgNormal;
  end;

  DrawGraphic(ToCanvas, PageRect, PageBmp, PageBmp.Transparent, bgStyle, bgCentered);
end;

procedure TcyBook.DoDrawLeftSidePage(PageRect: TRect);
begin
  if FCurrentLeftPage <= 0 then Exit;

  DrawLeftSideEmptyPage(PageRect);
  PrepareLeftPage(PageRect);

  // Draw page bitmap :
  if not FLeftPageView.FInvalidPage then
    if not FLeftPageView.FPage.Empty then
      DrawPage(FLeftPageView.FPage, PageRect, FLeftPageView.FMargins, FLeftPageView.FPageRender, canvas);
end;

procedure TcyBook.DoDrawRightSidePage(PageRect: TRect);
var
  bgStyle: TBgStyle;
begin
  if FCurrentRightPage <= 0 then Exit;

  DrawRightSideEmptyPage(PageRect);
  PrepareRightPage(PageRect);

  // Draw page bitmap :
  if not FRightPageView.FInvalidPage then
    if not FRightPageView.FPage.Empty then
      DrawPage(FRightPageView.FPage, PageRect, FRightPageView.FMargins, FRightPageView.FPageRender, Canvas);
end;

procedure TcyBook.DrawLeftSideEmptyPage(PageRect: TRect);
var TmpRect: TRect;
begin
  Canvas.Brush.Color := FLeftPageView.FPaperColor;
  Canvas.FillRect(PageRect);

  // Curved sheet effect :
  TmpRect := PageRect;
  TmpRect.Left := TmpRect.Right - FHeadBand.FWidth;
  cyGradientFill(Canvas, TmpRect, FLeftPageView.FPaperColor, FLeftPageView.FPaperShadeColor, dgdHorizontal, 70, bmNormal, 255, 100);

  // Deepness effect :
  // Deepness := Round(PageWidth * 0.2 * FCurrentRightPage / 200);
  // cyGraphics.cyFrame(Canvas, PageRect, FPaperShadeColor, clNone, clNone, FPaperShadeColor, Deepness);
end;

procedure TcyBook.DrawRightSideEmptyPage(PageRect: TRect);
var TmpRect: TRect;
begin
  Canvas.Brush.Color := FRightPageView.FPaperColor;
  Canvas.FillRect(PageRect);

  // Curved sheet effect :
  TmpRect := PageRect;
  TmpRect.Right := TmpRect.Left + FHeadBand.FWidth;
  cyGradientFill(Canvas, TmpRect, FRightPageView.FPaperShadeColor, FRightPageView.FPaperColor, dgdHorizontal, 30, bmNormal, 255, 100);

  // Deepness effect :
  // Deepness := Round(PageWidth * 0.2 * (FPages - FCurrentRightPage) / 200);
  // cyGraphics.cyFrame(Canvas, PageRect, clNone, clNone, FPaperShadeColor, FPaperShadeColor, Deepness);
end;

end.
