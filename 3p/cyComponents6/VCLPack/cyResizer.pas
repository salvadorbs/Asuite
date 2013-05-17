{   Component(s):
    tcyResizer

    Description:
    Move and resize components at run-time

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

unit cyResizer;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Forms, Windows, RTLConsts, Controls, Graphics, Messages, VCL.cyTypes, VCL.cyGraphics;

type
  TTmpGraphicControl = class(TGraphicControl)
  private
  protected
  public
  published
  end;

  TcyResizer = class;

  // Control item of THandlingControlList :
  THandlingControlItem=class
  private
    FControl: TControl;
    FOldRect: TRect;
  protected
    property OldRect: TRect read FOldRect;
  public
    property Control: TControl read FControl;
  end;

  // TcyResizer Control List of handled controls :
  TProcOnItem = procedure (Sender: TObject; Index: Integer) of object;

  THandlingControlList = class
  private
    FcyResizer: TcyResizer;
    FList: array of THandlingControlItem;
    FOnInsertItem: TProcOnItem;
    FOnRemoveItem: TProcOnItem;
    FOnClear: TNotifyEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): THandlingControlItem;
    procedure AddItem(Item: TControl);
    procedure DeleteItem(Index: Integer);
  protected
  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure Clear;
    function ControlsFromSameParent: boolean;
    function InsertControl(aControl: TControl): Boolean;
    function RemoveControl(aControl: TControl): Boolean;
    function RemoveFromIndex(Index: Word): Boolean;
    function Find(aControl: TControl; var Index: Integer): Boolean;
    function IndexOf(aControl: TControl): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: THandlingControlItem read GetItem; default;
    property OnInsertItem: TProcOnItem read FOnInsertItem write FOnInsertItem;
    property OnRemoveItem: TProcOnItem read FOnRemoveItem write FOnRemoveItem;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

  THandlerStyle = (bsSquare, bsGradientSquare, bsCircle, bsGradientCircle);

  // Properties to draw frame control
  TcyControlFrame=class(TPersistent)
  private
    FColor: TColor;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Visible: Boolean read FVisible write SetVisible default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // Properties to draw resizing control Handlers
  TcyHandler=class(TPersistent)
  private
    FBrush: TBrush;
    FPen: TPen;
    FSize: integer;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FHandlerStyle: THandlerStyle;
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetSize(const Value: integer);
    procedure SetVisible(Value: Boolean);
    procedure SetHandlerStyle(const Value: THandlerStyle);
  protected
    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property Size:integer read FSize write SetSize default 5;
    property Style: THandlerStyle read FHandlerStyle write SetHandlerStyle default bsSquare;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // Sub-properties of TcyResizer for the 2 types of resizing Handlers : corner and middle resizing Handlers
  TcyHandlingDef=class(TPersistent)
  private
    FCornerHandlers: TcyHandler;
    FMiddleHandlers: TcyHandler;
    FOnChange: TNotifyEvent;
    FFrame: TcyControlFrame;
    procedure SetCornerHandlers(const Value: TcyHandler);
    procedure SetMiddleHandlers(const Value: TcyHandler);
    procedure SetFrame(const Value: TcyControlFrame);
  protected
    procedure CornerHandlersChanged(Sender: TObject);
    procedure MiddleHandlersChanged(Sender: TObject);
    procedure FrameChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property CornerHandlers: TcyHandler read FCornerHandlers write SetCornerHandlers;
    property MiddleHandlers: TcyHandler read FMiddleHandlers write SetMiddleHandlers;
    property Frame: TcyControlFrame read FFrame write SetFrame;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TResizerState = (rsInactive, rsHandlingNothing, rsRangeSelect, rsOneControlSelected, rsMultipleControlsSelected,
               rsMovingOneControl, rsMovingMultipleControls, rsResizingOneControl, rsResizingMultipleControls);

  // Properties to draw design grid
  TGridStyle = (gsDot, gsLine, gsHorizontalLine, gsVerticalLine);

  TcyGrid=class(TPersistent)
  private
    FRowSize: integer;
    FColumnSize: integer;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FStyle: TGridStyle;
    FColor: TColor;
    FSnapToGrid: Boolean;
    procedure SetColumnSize(const Value: integer);
    procedure SetRowSize(const Value: integer);
    procedure SetStyle(const Value: TGridStyle);
    procedure SetVisible(const Value: Boolean);
    procedure SetColor(const Value: TColor);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor default clSilver;
    property ColumnSize: integer read FColumnSize write SetColumnSize default 8;
    property RowSize: integer read FRowSize write SetRowSize default 8;
    property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid default true;
    property Style: TGridStyle read FStyle write SetStyle default gsDot;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // Properties to draw guidelines
  TGuideLinesControls = (gmNone, gmFirstSelected, gmLastSelected, gmAllSelected);
  TGuideLinesTarget = (gtUnselectedControls, gtSelectedControls);
  TGuideLinesTargets = set of TGuideLinesTarget;

  TcyGuideLines=class(TPersistent)
  private
    FColor: TColor;
    FControls: TGuideLinesControls;
    FTargets: TGuideLinesTargets;
    procedure SetColor(const Value: TColor);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property Controls: TGuideLinesControls read FControls write FControls default gmAllSelected;
    property Targets: TGuideLinesTargets read FTargets write FTargets default [gtUnselectedControls, gtSelectedControls];
  end;

  TMouseJob = (mjNothing, mjRangeSelect, mjResizeTopLeft,    mjResizeTop,    mjResizeTopRight,
                                         mjResizeLeft,       mjMove,         mjResizeRight,
                                         mjResizeBottomLeft, mjResizeBottom, mjResizeBottomRight);

  TKeyJob = (kjDoNothing, kjSelectNearest, kjLargejMove, kj1PixelMove, kj1PixelResize, kjLargeResize);

  TResizeEdge = (reLeft, reTop, reRight, reBottom);
  TResizeEdges = Set of TResizeEdge;

  TAlignControls = (acLeftEdges, acTopEdges, acRightEdges, acBottomEdges, acVerticalCenters, acHorizontalsCenters);

  TResizerOption = (roMouseSelect,                  // Allow select one control with mouse
                    roMouseMove,                    // Allow move one control with mouse
                    roMouseResize,                  // Allow resize one control with mouse
                    roMouseMultiSelect,             // Allow select multiple controls with mouse
                    roMouseMultiMove,               // Allow move multiple controls with mouse
                    roMouseMultiResize,             // Allow resize multiple controls with mouse

                    roKeySelect,                    // Allow select one control with keyboard
                    roKeyMove,                      // Allow move one control with keyboard
                    roKeyResize,                    // Allow resize one control with keyboard
                    roKeyMultiSelect,               // Allow select multiple controls with keyboard, !!! not used for now !!!
                    roKeyMultiMove,                 // Allow move multiple controls with keyboard
                    roKeyMultiResize,               // Allow resize multiple controls with keyboard
                    roKeyUnselectAll,               // Unselect all with esc key

                    roOnlyMultiSelectIfSameParent,  // Avoid multi-selection controls from diferent parents
                    roOnlyMultiMoveIfSameParent,    // Avoid multi-move controls from diferent parents
                    roOnlyMultiResizeIfSameParent,  // Avoid multi-resize controls from diferent parents


                    roMouseUnselectAll,             // Unselect all
                    roOutsideParentRect             // move and resize ouside parent bounds rect
                    );   // !!! New properties must be added at the end !!!

  TResizerOptions = Set of TResizerOption;

  TProcOnPaintControl = procedure (Sender: TObject; Control: TControl; Left, Top: Integer; var Draw: Boolean) of object;
  TProcOnListInsert = procedure (Sender: TObject; Control: TControl; var Accept: Boolean) of object;
  TProcOnMoveSelection = procedure (Sender: TObject; var IncX, IncY: Integer) of object;
  TProcOnResizeSelection = procedure (Sender: TObject; ResizeEdges: TResizeEdges; var IncX, IncY: Integer) of object;

  TcyResizer = class(TCustomControl)
  private
    FSurface: TWinControl;
    FMouseHandling: Boolean;
    FMouseHandlingFromPoint: TPoint;
    FMouseHandlingJob: TMouseJob;
    FMouseHandling1stTask: Boolean;
    FMouseFocusRect: TRect;
    FRangeSelectMinX: Integer;
    FRangeSelectMaxX: Integer;
    FRangeSelectMinY: Integer;
    FRangeSelectMaxY: Integer;
    FKeyHandlingJob: TKeyJob;
    FArrowKeyUpPressed: Boolean;
    FArrowKeyDownPressed: Boolean;
    FArrowKeyRightPressed: Boolean;
    FArrowKeyLeftPressed: Boolean;
    FHandlingMultipleControls: TcyHandlingDef;
    FHandlingSingleControl: TcyHandlingDef;
    FHandlingControlList: THandlingControlList;
    FGrid: TcyGrid;
    FReadOnly: Boolean;
    FOptions: TResizerOptions;
    FActive: Boolean;
    FOnControlListInsert: TProcOnListInsert;
    FLastMouseJob: TMouseJob;
    FLastMouseDownControl: TControl;
    FGuideLines: TcyGuideLines;
    FBeforePaint: TNotifyEvent;
    FAfterPaint: TNotifyEvent;
    FOnMouseJobChange: TNotifyEvent;
    FOnKeyJobChange: TNotifyEvent;
    FOnMoveSelection: TProcOnMoveSelection;
    FOnResizeSelection: TProcOnResizeSelection;
    FControlFrame: TcyControlFrame;
    FAfterMoveSelection: TNotifyEvent;
    FAfterResizeSelection: TNotifyEvent;
    FOnPaintControl: TProcOnPaintControl;
    procedure SetHandlingMultipleControls(const Value: TcyHandlingDef);
    procedure SetHandlingSingleControl(const Value: TcyHandlingDef);
    function GetState: TResizerState;
    procedure SetGrid(const Value: TcyGrid);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetOptions(const Value: TResizerOptions);
    procedure SetGuideLines(const Value: TcyGuideLines);
    procedure WMMove(var Message: TMessage); message WM_MOVE;  // In order to know when TcyResizer was moved/scrolled by a TScrollBox for exemple
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var message: TMessage); message WM_GETDLGCODE;
    procedure SetMouseHandlingJob(const Value: TMouseJob);
    procedure SetKeyHandlingJob(const Value: TKeyJob);
    procedure SetControlFrame(const Value: TcyControlFrame);
  protected
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function GetControlRect(aControl: TControl): TRect; // Only for controls on Surface
    procedure PaintControl(aControl: TControl);         // Only for controls on Surface
    function CalcHandlerRect(Handler: TcyHandler; Job: TMouseJob; ControlRect: TRect): TRect;
    procedure DrawHandler(Handler: TcyHandler; var Rect: TRect);
    procedure DrawDesignTimeControlHandlers;
    procedure OnHandlingListInsertControl(Sender: TObject; Index: Integer);
    procedure OnHandlingListRemoveControl(Sender: TObject; Index: Integer);
    procedure OnHandlingListClear(Sender: TObject);
    procedure HandlingSingleControlChanged(Sender: TObject);
    procedure HandlingMultipleControlsChanged(Sender: TObject);
    procedure ControlsFrameChanged(Sender: TObject);
    procedure GridChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ItemToClientPoint(Item: THandlingControlItem; aPoint: TPoint): TPoint;
    function GetHandlingControlItemClientRect(Item: THandlingControlItem): TRect;
    procedure GetHandlingMouseInformation(aPoint: TPoint; var aControl: TControl; var ControlRect, HandlerRect: TRect; var Job: TMouseJob);
    procedure GetHandlingKeyInformation(Key: Word; ShiftState: TShiftState; KeyDown: Boolean; var Job: TKeyJob);
    function GetMouseJobCursor(const Job: TMouseJob): TCursor;
    procedure DrawGrid;
    procedure DrawHandlers(HandlingDef: TcyHandlingDef; aRect: TRect);
    procedure DrawControlHandlers(ControlIndex: Integer);
    procedure DrawControlGuideLines(ControlIndex: Integer);
    procedure DrawSquareHandler(var Rect: TRect; FrameWidth: Integer);
    procedure DrawGradientSquareHandler(var Rect: TRect; FrameWidth: Integer);
    procedure DrawCircleHandler(var Rect: TRect; FrameWidth: Integer);
    procedure DrawGradientCircleHandler(var Rect: TRect; FrameWidth: Integer);
    function PointInHandlerRect(Point: TPoint; Rect: TRect; Style: THandlerStyle): Boolean;
    function DetermineControlAtPos(ParentControl: TWinControl; atClientPoint: TPoint): TControl;
    procedure InsertControlsInRect(ParentControl: TWinControl; ControlRect: TRect);
    //
    procedure Activate(Surface: TWinControl);
    procedure Deactivate;
    // Mouse controls handling:
    procedure MoveSelectionFromOriginalPosition(IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
    procedure ResizeSelectionFromOriginalPosition(ResizeEdges: TResizeEdges; IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
    // key controls handling :
    procedure MoveSelection(IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
    procedure ResizeSelection(ResizeEdges: TResizeEdges; IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
    // Design functions:
    procedure AlignSelection(AlignControls: TAlignControls; WithFirstControl: Boolean); overload;
    procedure AlignSelection(AlignControls: TAlignControls; ToValue: Integer); overload;
    procedure ValidateNewLeft(aControl: TControl; var NewLeft: Integer);
    procedure ValidateNewTop(aControl: TControl; var NewTop: Integer);
    procedure ValidateNewWidth(aControl: TControl; var NewWidth: Integer; ResizeEdges: TResizeEdges);
    procedure ValidateNewHeight(aControl: TControl; var NewHeight: Integer; ResizeEdges: TResizeEdges);
    procedure AlignSelectionToGrid;
    function CenterSelection(Horizontally, Vertically: Boolean): Boolean;
    procedure MakeSelectionSameSize(SameWidth, SameHeight: Boolean);
    procedure MakeSelectionSameWidth(NewWidth: Integer);
    procedure MakeSelectionSameHeight(NewHeight: Integer);
    procedure SpaceSelectionEquallyHorizontally(IncSpacingSize: Integer; RemovePreviousSpacing: Boolean); overload;
    procedure SpaceSelectionEquallyHorizontally(TotalSpacingSize: Integer); overload;
    procedure SpaceSelectionEquallyVertically(IncSpacingSize: Integer; RemovePreviousSpacing: Boolean); overload;
    procedure SpaceSelectionEquallyVertically(TotalSpacingSize: Integer); overload;
    //
    property Active: Boolean read FActive default false;
    property HandlingControlList: THandlingControlList read FHandlingControlList;
    property KeyHandlingJob: TKeyJob read FKeyHandlingJob write SetKeyHandlingJob default kjSelectNearest;
    property LastMouseDownControl: TControl read FLastMouseDownControl;
    property LastMouseJob: TMouseJob read FLastMouseJob;
    property MouseHandling: Boolean read FMouseHandling write FMouseHandling default false;
    property MouseHandlingJob: TMouseJob read FMouseHandlingJob write SetMouseHandlingJob default mjNothing;
    property State: TResizerState read GetState;
    property Canvas;
  published
    property ControlsFrame: TcyControlFrame read FControlFrame write SetControlFrame;
    property Grid: TcyGrid read FGrid write SetGrid;
    property GuideLines: TcyGuideLines read FGuideLines write SetGuideLines;
    property HandlingSingleControl: TcyHandlingDef read FHandlingSingleControl write SetHandlingSingleControl;
    property HandlingMultipleControls: TcyHandlingDef read FHandlingMultipleControls write SetHandlingMultipleControls;
    property Options: TResizerOptions read FOptions write SetOptions default [
              roMouseSelect, roMouseMove, roMouseResize, roMouseMultiSelect, roMouseMultiMove, roMouseMultiResize,
               roKeySelect, roKeyMove, roKeyResize, roKeyMultiSelect, roKeyMultiMove, roKeyMultiResize];
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property OnControlListInsert: TProcOnListInsert read FOnControlListInsert write FOnControlListInsert;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
    property BeforePaint: TNotifyEvent read FBeforePaint write FBeforePaint;
    property OnPaintControl: TProcOnPaintControl read FOnPaintControl write FOnPaintControl;
    property OnKeyJobChange: TNotifyEvent read FOnKeyJobChange write FOnKeyJobChange;
    property OnMouseJobChange: TNotifyEvent read FOnMouseJobChange write FOnMouseJobChange;
    property OnMoveSelection: TProcOnMoveSelection  read FOnMoveSelection write FOnMoveSelection;
    property AfterMoveSelection: TNotifyEvent read FAfterMoveSelection write FAfterMoveSelection;
    property OnResizeSelection: TProcOnResizeSelection  read FOnResizeSelection write FOnResizeSelection;
    property AfterResizeSelection: TNotifyEvent read FAfterResizeSelection write FAfterResizeSelection;
    property Color;
    property ParentColor;
    property PopupMenu;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseActivate; {$ENDIF}
    property OnMouseDown;
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseEnter; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property OnMouseLeave; {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation

{ THandlingControlList }
constructor THandlingControlList.Create(Owner: TComponent);
begin
  inherited Create;
  FcyResizer := TcyResizer(Owner);
  SetLength(FList, 0);
end;

destructor THandlingControlList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THandlingControlList.Clear;
begin
  if Length(FList) = 0 then Exit;
  SetLength(FList, 0);

  if Assigned(FOnClear)
  then FOnClear(Self);
end;

function THandlingControlList.ControlsFromSameParent: boolean;
var L: Integer;
begin
  RESULT := true;

  for L := 1 to Count-1 do
    if FList[L].FControl.Parent <> FList[0].FControl.Parent
    then begin
      RESULT := false;
      Break;
    end;
end;

function THandlingControlList.Find(aControl: TControl; var Index: Integer): Boolean;
var
  L, C: Integer;
begin
  RESULT := false;
  L := 0;
  C := Length(FList) - 1;

  while (not RESULT) and (L <= C) do
  begin
    if FList[L].FControl = aControl
    then begin
      RESULT := true;
      Index := L;
    end
    else
      Inc(L, 1);
  end;
end;

function THandlingControlList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function THandlingControlList.GetItem(Index: Integer): THandlingControlItem;
begin
  Result := FList[Index];
end;

function THandlingControlList.IndexOf(aControl: TControl): Integer;
begin
  if not Find(aControl, RESULT)
  then RESULT := -1;
end;

procedure THandlingControlList.AddItem(Item: TControl);
var C: Integer;
    New: THandlingControlItem;
begin
  C := Count;
  SetLength(FList, C + 1);

  New := THandlingControlItem.Create;
  New.FControl := Item;
  FList[C] := New;
  Item.FreeNotification(FcyResizer);     // Inform cyResizer if the component is going to be deleted ...

  if Assigned(FOnInsertItem)
  then FOnInsertItem(Self, C);
end;

procedure THandlingControlList.DeleteItem(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.Create(SListIndexError);

  if Assigned(FOnRemoveItem)
  then FOnRemoveItem(Self, Index);

  // Move items :
  FList[Index] := nil;

  if Index < Count-1
  then begin
    System.Move(FList[Index + 1], FList[Index], (Count - Index - 1) * SizeOf(THandlingControlItem));
    PPointer(@FList[Count-1])^ := nil;
  end;

  SetLength(FList, Count-1);
end;

function THandlingControlList.InsertControl(aControl: TControl): Boolean;
var Index: Integer;
begin
  if (FcyResizer.FActive) and (aControl <> FcyResizer)
  then begin
    RESULT := not Find(aControl, Index);

    if RESULT
    then begin
      if (Count <> 0) and (roOnlyMultiSelectIfSameParent in FcyResizer.FOptions)
      then
        if Items[0].FControl.Parent <> aControl.Parent
        then Clear;

      AddItem(aControl);
    end;
  end
  else
    RESULT := false;
end;

function THandlingControlList.RemoveControl(aControl: TControl): Boolean;
var Index: Integer;
begin
  if FcyResizer.FActive
  then begin
    RESULT := Find(aControl, Index);

    if RESULT
    then DeleteItem(Index);
  end
  else
    RESULT := false;
end;

function THandlingControlList.RemoveFromIndex(Index: Word): Boolean;
begin
  if FcyResizer.FActive
  then begin
    RESULT := Index < Count;

    if RESULT
    then DeleteItem(Index);
  end
  else
    RESULT := false;
end;

{ TcyControlFrame }
constructor TcyControlFrame.Create(AOwner: TComponent);
begin
  FColor := clBlack;
  FVisible := False;
end;

destructor TcyControlFrame.Destroy;
begin
  inherited;
end;

procedure TcyControlFrame.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyControlFrame.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyControlFrame.StyleChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TcyHandler }
constructor TcyHandler.Create(AOwner: TComponent);
begin
  FPen := TPen.Create;
  FPen.OnChange := StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;
  FSize   := 5;
  FVisible := True;
end;

destructor TcyHandler.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TcyHandler.StyleChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyHandler.SetHandlerStyle(const Value: THandlerStyle);
begin
  if FHandlerStyle <> Value
  then begin
    FHandlerStyle := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyHandler.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TcyHandler.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TcyHandler.SetSize(const Value: integer);
begin
  if (FSize <> Value) and (Value > 0)
  then begin
    FSize := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TcyHandler.SetVisible(Value: Boolean);
begin
  if Value <> FVisible
  then begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ TcyHandlingDef }
constructor TcyHandlingDef.Create(AOwner: TComponent);
begin
  FCornerHandlers := TcyHandler.Create(AOwner);
  FCornerHandlers.OnChange := CornerHandlersChanged;
  FMiddleHandlers := TcyHandler.Create(AOwner);
  FMiddleHandlers.OnChange := MiddleHandlersChanged;
  FFrame := TcyControlFrame.Create(AOwner);
  FFrame.OnChange := FrameChanged;
end;

destructor TcyHandlingDef.Destroy;
begin
  FCornerHandlers.Free;
  FMiddleHandlers.Free;
  FFrame.Free;
  inherited Destroy;
end;

procedure TcyHandlingDef.FrameChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnchange(Sender);
end;

procedure TcyHandlingDef.CornerHandlersChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnchange(Sender);
end;

procedure TcyHandlingDef.MiddleHandlersChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnchange(Sender);
end;

procedure TcyHandlingDef.SetCornerHandlers(const Value: TcyHandler);
begin
  FCornerHandlers := Value;
end;

procedure TcyHandlingDef.SetFrame(const Value: TcyControlFrame);
begin
  FFrame := Value;
end;

procedure TcyHandlingDef.SetMiddleHandlers(const Value: TcyHandler);
begin
  FMiddleHandlers := Value;
end;

{ TcyGrid }
constructor TcyGrid.Create(AOwner: TComponent);
begin
  FColor := clSilver;
  FColumnSize := 8;
  FRowSize := 8;
  FSnapToGrid := true;
  FStyle := gsDot;
  FVisible := True;
end;

destructor TcyGrid.Destroy;
begin
  inherited;
end;

procedure TcyGrid.SetColor(const Value: TColor);
begin
  if FColor <> Value
  then begin
    FColor := Value;
    if Assigned(FOnChange) then FOnchange(Self);
  end;
end;

procedure TcyGrid.SetColumnSize(const Value: integer);
begin
  if (Value > 0) and (Value <> FColumnSize)
  then begin
    FColumnSize := Value;
    if Assigned(FOnChange) then FOnchange(Self);
  end;
end;

procedure TcyGrid.SetRowSize(const Value: integer);
begin
  if (Value > 0) and (Value <> FRowSize)
  then begin
    FRowSize := Value;
    if Assigned(FOnChange) then FOnchange(Self);
  end;
end;

procedure TcyGrid.SetStyle(const Value: TGridStyle);
begin
  if FStyle <> Value
  then begin
    FStyle := Value;
    if Assigned(FOnChange) then FOnchange(Self);
  end;
end;

procedure TcyGrid.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value
  then begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnchange(Self);
  end;
end;

{ TcyGuideLines }
constructor TcyGuideLines.Create(AOwner: TComponent);
begin
  FColor := clBlue;
  FControls := gmAllSelected;
  FTargets := [gtUnselectedControls, gtSelectedControls];
end;

destructor TcyGuideLines.Destroy;
begin
  inherited;
end;

procedure TcyGuideLines.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

{ TcyResizer }
constructor TcyResizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMouseHandling := false;
  FMouseHandlingJob := mjNothing;
  FKeyHandlingJob := kjSelectNearest;
  FArrowKeyUpPressed := false;
  FArrowKeyDownPressed := false;
  FArrowKeyRightPressed := false;
  FArrowKeyLeftPressed := false;

  FOptions := [roMouseSelect, roMouseMove, roMouseResize, roMouseMultiSelect, roMouseMultiMove, roMouseMultiResize, roMouseUnselectAll,
               roKeySelect, roKeyMove, roKeyResize, roKeyMultiSelect, roKeyMultiMove, roKeyMultiResize, roKeyUnselectAll, roOutsideParentRect];

  FControlFrame := TcyControlFrame.Create(Self);
  FControlFrame.OnChange := ControlsFrameChanged;
  FGrid := TcyGrid.Create(Self);
  FGrid.OnChange := GridChanged;
  FGuideLines := TcyGuideLines.Create(Self);
  FHandlingSingleControl := TcyHandlingDef.Create(self);
  FHandlingSingleControl.OnChange := HandlingSingleControlChanged;
  FHandlingMultipleControls := TcyHandlingDef.Create(self);
  FHandlingMultipleControls.OnChange := HandlingMultipleControlsChanged;
  FHandlingControlList := THandlingControlList.Create(self);
  FHandlingControlList.FOnInsertItem := OnHandlingListInsertControl;
  FHandlingControlList.FOnRemoveItem := OnHandlingListRemoveControl;
  FHandlingControlList.FOnClear := OnHandlingListClear;
  FReadOnly := false;
  FActive := false;
  Anchors := [akLeft, akTop, akRight, akBottom];
  DoubleBuffered := true;     // Avoid flickering !!!
  ParentBackground := false;  // Avoid flickering !!!

  if csDesigning in ComponentState
  then begin
    Width := 170;
    Height := 120;
  end
  else
    Visible := false;
end;

destructor TcyResizer.Destroy;
begin
  FHandlingControlList.Free;
  FHandlingControlList := nil;
  HandlingSingleControl.Free;
  HandlingMultipleControls.Free;
  FGuideLines.Free;
  FGrid.Free;
  FControlFrame.Free;
  inherited Destroy;
end;

procedure TcyResizer.Notification(AComponent: TComponent; Operation: TOperation);
var Index: Integer;
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent <> Self)
  then
    if not (csDestroying in Owner.ComponentState)
    then
      if AComponent is TControl
      then begin
        if AComponent = FLastMouseDownControl
        then FLastMouseDownControl := nil;

        if FHandlingControlList.Find(TControl(AComponent), Index)
        then FHandlingControlList.RemoveFromIndex(Index);
      end;
end;

procedure TcyResizer.OnHandlingListInsertControl(Sender: TObject; Index: Integer);
begin
  if Index = 1    // Passing from 1 control handled to 2, we need to repaint the first one !!!
  then Invalidate
  else DrawControlHandlers(Index);
end;

// Item is not yet removed !!!
procedure TcyResizer.OnHandlingListRemoveControl(Sender: TObject; Index: Integer);
begin
  Invalidate;
end;

procedure TcyResizer.OnHandlingListClear(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyResizer.DrawGrid;
var
  Rect: TRect;
  x, y: Integer;
begin
  Rect := ClientRect;
  Canvas.Pen.Width := 1;

  if FGrid.Style = gsDot
  then begin
    x := Rect.Left + FGrid.FColumnSize;

    while x <= Rect.Right do
    begin
      y := Rect.Top + FGrid.FRowSize;

      while y <= Rect.Bottom do
      begin
        Canvas.Pixels[x, y] := FGrid.Color;
        inc(y, FGrid.FColumnSize);
      end;

      inc(x, FGrid.FColumnSize);
    end;
  end;

  if FGrid.Style in [gsLine, gsVerticalLine]
  then begin
    Canvas.Pen.Color := FGrid.Color;
    x := Rect.Left + FGrid.FColumnSize;

    while x <= Rect.Right do
    begin
      Canvas.MoveTo(x, Rect.Top);
      Canvas.LineTo(x, Rect.Bottom);
      inc(x, FGrid.FColumnSize);
    end;
  end;

  if FGrid.Style in [gsLine, gsHorizontalLine]
  then begin
    Canvas.Pen.Color := FGrid.Color;
    y := Rect.Top + FGrid.FRowSize;

    while y <= Rect.Bottom do
    begin
      Canvas.MoveTo(Rect.Left, y);
      Canvas.LineTo(Rect.Right, y);
      inc(y, FGrid.FColumnSize);
    end;
  end;
end;

function TcyResizer.GetControlRect(aControl: TControl): TRect;
var _Left, _Top: Integer;
begin
  _Left := aControl.Left - Left;
  _Top := aControl.Top - Top;
  Result := classes.Rect(_Left, _Top, _Left + aControl.Width, _Top + aControl.Height);
end;

procedure TcyResizer.PaintControl(aControl: TControl);
var
  Draw: Boolean;
  SaveIndex: Integer;
  destRect: TRect;
  ClipRgn: HRGN;
  res: Integer;
begin
  if Assigned(FOnPaintControl) then
  begin
    Draw := true;
    FOnPaintControl(Self, aControl, aControl.Left - Left, aControl.Top - Top, Draw);
    if not Draw then Exit;
  end;

  if aControl is TWinControl then
  begin
    TWinControl(aControl).PaintTo(Canvas, aControl.Left - Left, aControl.Top - Top);
  end
  else
    if aControl is TGraphicControl then
    begin
      // Create a region in order to handle TImage painting (when graphic is clipped into TImage) :
      try
        destRect := aControl.BoundsRect;
        OffsetRect(destRect, -aControl.Left, -aControl.Top);
        OffsetRect(destRect, aControl.Left - Left, aControl.Top - Top);
        ClipRgn := CreateRectRgnIndirect(destRect);
        Res := SelectClipRgn(Canvas.Handle, ClipRgn);

        try
          SaveIndex := SaveDC(Canvas.Handle);
          MoveWindowOrg(Canvas.Handle, aControl.Left - Left, aControl.Top - Top);
          aControl.Perform(WM_PAINT, Canvas.Handle, 0);
        finally
          RestoreDC(Canvas.Handle, SaveIndex);
        end;

      finally
        DeleteObject(ClipRgn);
        // Remove clipped region :
        SelectClipRgn(Canvas.Handle, 0);
      end;
    end
    else
      try
        SaveIndex := SaveDC(Canvas.Handle);
        MoveWindowOrg(Canvas.Handle, aControl.Left - Left, aControl.Top - Top);
        aControl.Perform(WM_PAINT, Canvas.Handle, 0);    // WM_PRINT, WM_PRINTCLIENT
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
end;

procedure TcyResizer.Paint;
var
  i, j: Integer;
  Rect: TRect;

        procedure DrawChildControlsFrame(aControl: TWinControl);
        var
          c: Integer;
          aRect: TRect;
        begin
          for c := 0 to aControl.ControlCount-1 do
            if aControl.Controls[c].Visible then
              if not FHandlingControlList.Find(aControl.Controls[c], j) then
              begin
                if aControl.Controls[c] is TWinControl then
                  DrawChildControlsFrame( TWinControl(aControl.Controls[c]) );

                aRect := aControl.Controls[c].BoundsRect;
                aRect := ClientToScreenRect(aControl, aRect);
                aRect := ScreenToClientRect(Self, aRect);
                Canvas.FrameRect(aRect);
              end;
        end;

begin
//   inherited;
  Rect := ClientRect;

  if csDesigning in ComponentState
  then begin
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Brush.Color := Color;
    Canvas.Rectangle(Rect);

    if FGrid.Visible
    then DrawGrid;

    // Preview Control handlers :
    DrawDesignTimeControlHandlers;
  end
  else begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);

    // Call BeforePaint after drawing the grid :
    if Assigned(FBeforePaint)
    then FBeforePaint(Self);

    if FGrid.Visible
    then DrawGrid;

    // Draw FSurface controls in the canvas :
    for i := 0 to FSurface.ControlCount-1 do
      if not (FSurface.Controls[i] is TcyResizer)
      then
        if FSurface.Controls[i].Visible then
        begin
          PaintControl(FSurface.Controls[i]);

          // Draw frame :
          if FControlFrame.Visible then
            if not FHandlingControlList.Find(FSurface.Controls[i], j) then
            begin
              Canvas.Brush.Color := FControlFrame.Color;

              // Draw child controls frame :
              if FSurface.Controls[i] is TWinControl then
                DrawChildControlsFrame( TWinControl(FSurface.Controls[i]) );

              Canvas.FrameRect(GetControlRect(FSurface.Controls[i]));
            end;
        end;

    // Draw Guidelines for selected component :
    case FGuideLines.FControls of
      gmFirstSelected:
        begin
         if FHandlingControlList.Count > 0
         then DrawControlGuideLines(0);
        end;

      gmLastSelected:
        begin
         if FHandlingControlList.Count > 0
         then DrawControlGuideLines(FHandlingControlList.Count-1);
        end;

      gmAllSelected:
        begin
         for i := 0 to FHandlingControlList.Count-1 do
           DrawControlGuideLines(i);
        end;
    end;

    // Draw control handlers :
    for i := 0 to FHandlingControlList.Count-1 do
      DrawControlHandlers(i);

    // Call AfterPaint :
    if Assigned(FAfterPaint)
    then FAfterPaint(Self);
  end;
end;

function TcyResizer.ItemToClientPoint(Item: THandlingControlItem; aPoint: TPoint): TPoint;
begin
  RESULT := aPoint;
  RESULT := Item.FControl.ClientToScreen(RESULT);
  RESULT := ScreenToClient(RESULT);
end;

function TcyResizer.PointInHandlerRect(Point: TPoint; Rect: TRect; Style: THandlerStyle): Boolean;
begin
  if Style in [bsSquare, bsGradientSquare]
  then RESULT := PointInRect(Point, Rect)
  else RESULT := PointInEllispe(Point, Rect);
end;

procedure TcyResizer.GetHandlingMouseInformation(aPoint: TPoint; var aControl: TControl;
                          var ControlRect, HandlerRect: TRect; var Job: TMouseJob);
var
  C, ListCount: Integer;
  Search: Boolean;
  HandlingDef: TcyHandlingDef;
  ControlItem: THandlingControlItem;
begin
  aControl := Nil;
  Job := mjNothing;

  if FActive
  then begin
    ListCount := FHandlingControlList.Count;

    if ListCount > 0
    then begin
      C := 0;
      Search := true;

      if ListCount > 1
      then HandlingDef := FHandlingMultipleControls
      else HandlingDef := FHandlingSingleControl;

      while (C <= ListCount - 1) and (Search) do
      begin
        ControlItem := FHandlingControlList.Items[C];
        ControlRect := GetHandlingControlItemClientRect(ControlItem);

        // Cursor in control Rect ?
        if PointInRect(aPoint, ControlRect)
        then begin
          aControl := ControlItem.FControl;
          Job := mjMove;
        end;

        // Cursor in Corner control handlers ?
        if HandlingDef.FCornerHandlers.Visible
        then begin
          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeTopLeft, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FCornerHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeTopLeft;
            end;
          end;

          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeTopRight, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FCornerHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeTopRight;
            end;
          end;

          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeBottomRight, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FCornerHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeBottomRight;
            end;
          end;

          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeBottomLeft, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FCornerHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeBottomLeft;
            end;
          end;
        end;

        // Cursor in middle control handlers ?
        if HandlingDef.FMiddleHandlers.Visible
        then begin
          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeTop, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FMiddleHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeTop;
            end;
          end;

          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeRight, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FMiddleHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeRight;
            end;
          end;

          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeBottom, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FMiddleHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeBottom;
            end;
          end;

          if Search
          then begin
            HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeLeft, ControlRect);

            if PointInHandlerRect(aPoint, HandlerRect, HandlingDef.FMiddleHandlers.Style)
            then begin
              Search := false;
              aControl := ControlItem.FControl;
              Job := mjResizeLeft;
            end;
          end;
        end;

        Inc(C, 1);
      end;
    end;
  end;
end;

function TcyResizer.GetMouseJobCursor(const Job: TMouseJob): TCursor;
begin
  case Job of
    mjNothing:           RESULT := crDefault;
    mjRangeSelect:       RESULT := crCross;
    mjMove:              RESULT := crHandPoint;
    mjResizeTopLeft:     RESULT := crSizeNWSE;
    mjResizeTop:         RESULT := crSizeNS;
    mjResizeTopRight:    RESULT := crSizeNESW;
    mjResizeLeft:        RESULT := crSizeWE;
    mjResizeRight:       RESULT := crSizeWE;
    mjResizeBottomLeft:  RESULT := crSizeNESW;
    mjResizeBottom:      RESULT := crSizeNS;
    mjResizeBottomRight: RESULT := crSizeNWSE;
  end;
end;

function TcyResizer.GetHandlingControlItemClientRect(Item: THandlingControlItem): TRect;
var TopLeft, BottomRight: TPoint;
begin
  RESULT := classes.Rect(0, 0, Item.FControl.Width-1, Item.FControl.Height-1);
  TopLeft := ItemToClientPoint(Item, RESULT.TopLeft);
  BottomRight := ItemToClientPoint(Item, RESULT.BottomRight);
  RESULT := classes.Rect(TopLeft.X, TopLeft.Y, BottomRight.X, BottomRight.Y);
end;

function TcyResizer.CalcHandlerRect(Handler: TcyHandler; Job: TMouseJob; ControlRect: TRect): TRect;
var aPoint: TPoint;
begin
  case Job of
    // Corners:
    mjResizeTopLeft:     aPoint := ControlRect.TopLeft;
    mjResizeTopRight:    aPoint := Point(ControlRect.Right, ControlRect.Top);
    mjResizeBottomLeft:  aPoint := Point(ControlRect.Left, ControlRect.Bottom);
    mjResizeBottomRight: aPoint := ControlRect.BottomRight;
    // Middle:
    mjResizeTop:         aPoint := Point(ControlRect.Left + (ControlRect.Right-ControlRect.Left) div 2, ControlRect.Top);
    mjResizeLeft:        aPoint := Point(ControlRect.Left, ControlRect.Top + (ControlRect.Bottom-ControlRect.Top) div 2);
    mjResizeRight:       aPoint := Point(ControlRect.Right, ControlRect.Top + (ControlRect.Bottom-ControlRect.Top) div 2);
    mjResizeBottom:      aPoint := Point(ControlRect.Left + (ControlRect.Right-ControlRect.Left) div 2, ControlRect.Bottom);
  end;

  // Determine RESULT position :
  aPoint := Point(aPoint.X - Handler.FSize div 2, aPoint.Y - Handler.FSize div 2);
  // Determine Rect size :
  RESULT := classes.Rect(aPoint.X, aPoint.Y, aPoint.X + Handler.FSize, aPoint.Y + Handler.FSize);
end;

procedure TcyResizer.DrawSquareHandler(var Rect: TRect; FrameWidth: Integer);
var
  p: Integer;
  SauvB: TColor;
begin
  // Draw frame :
  SauvB := Canvas.Brush.Color;
  Canvas.Brush.Color := Canvas.Pen.Color;

  for p := 0 to FrameWidth - 1 do
  begin
    Canvas.FrameRect(Rect);
    InflateRect(Rect, -1, -1);
  end;
  Canvas.Brush.Color := SauvB;

  // Fill frame :
  Canvas.FillRect(Rect);
end;

procedure TcyResizer.DrawGradientSquareHandler(var Rect: TRect; FrameWidth: Integer);
var
  p: Integer;
  SauvB, SauvP: TColor;
  eye: TRect;
begin
  // Draw frame :
  SauvB := Canvas.Brush.Color;
  SauvP := Canvas.Pen.Color;

  Canvas.Brush.Color := Canvas.Pen.Color;

  for p := 0 to FrameWidth - 2 do
  begin
    Canvas.FrameRect(Rect);
    InflateRect(Rect, -1, -1);
  end;

  // Fill frame :
  eye := Rect;
  OffSetRect(Eye, -1, -1);
  InflateRectPercent(eye, -1);  // Inflate Rect -100%
  cyGradientFillShape(Canvas, Rect, SauvP, SauvB, 255, Eye, osRectangle);
  Canvas.Brush.Color := SauvB;
  Canvas.Pen.Color := SauvP;
end;

procedure TcyResizer.DrawCircleHandler(var Rect: TRect; FrameWidth: Integer);
var
  p: Integer;
  SauvB, SauvP: TColor;
begin
  // Draw frame :
  SauvB := Canvas.Brush.Color;
  SauvP := Canvas.Pen.Color;
  Canvas.Brush.Color := Canvas.Pen.Color;

  for p := 0 to FrameWidth - 1 do
  begin
    Canvas.Ellipse(Rect);
    InflateRect(Rect, -1, -1);
  end;
  Canvas.Brush.Color := SauvB;

  // Fill frame :
  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Ellipse(Rect);
  Canvas.Pen.Color := SauvP;
end;

procedure TcyResizer.DrawGradientCircleHandler(var Rect: TRect; FrameWidth: Integer);
var
  p: Integer;
  SauvB, SauvP: TColor;
  eye: TRect;
begin
  // Draw frame :
  SauvB := Canvas.Brush.Color;
  SauvP := Canvas.Pen.Color;
  Canvas.Brush.Color := Canvas.Pen.Color;

  for p := 0 to FrameWidth - 2 do
  begin
    Canvas.Ellipse(Rect);
    InflateRect(Rect, -1, -1);
  end;
  Canvas.Brush.Color := SauvB;

  // Fill frame :
  eye := Rect;
  OffSetRect(Eye, -1, -1);
  InflateRectPercent(eye, -1);  // Inflate Rect -100%
  cyGradientFillShape(Canvas, Rect, SauvP, SauvB, 255, Eye, osRadial);
  Canvas.Brush.Color := SauvB;
  Canvas.Pen.Color := SauvP;
end;

procedure TcyResizer.DrawHandler(Handler: TcyHandler; var Rect: TRect);
begin
  case Handler.FHandlerStyle of
    bsSquare:         DrawSquareHandler(Rect, Handler.Pen.Width);
    bsGradientSquare: DrawGradientSquareHandler(Rect, Handler.Pen.Width);
    bsCircle:         DrawCircleHandler(Rect, Handler.Pen.Width);
    bsGradientCircle: DrawGradientCircleHandler(Rect, Handler.Pen.Width);
  end;
end;

procedure TcyResizer.DrawHandlers(HandlingDef: TcyHandlingDef; aRect: TRect);
var HandlerRect: TRect;
begin
  // Draw frame :
  if HandlingDef.Frame.Visible then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := HandlingDef.Frame.Color;
    Canvas.FrameRect(aRect);
  end;

  // Draw corner Handlers :
  if HandlingDef.CornerHandlers.Visible
  then begin
    Canvas.Pen.Assign(HandlingDef.FCornerHandlers.Pen);
    Canvas.Pen.Width := 1;
    Canvas.Brush.Assign(HandlingDef.FCornerHandlers.FBrush);

    HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeTopLeft, aRect);
    DrawHandler(HandlingDef.FCornerHandlers, HandlerRect);

    HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeTopRight, aRect);
    DrawHandler(HandlingDef.FCornerHandlers, HandlerRect);

    HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeBottomRight, aRect);
    DrawHandler(HandlingDef.FCornerHandlers, HandlerRect);

    HandlerRect := CalcHandlerRect(HandlingDef.FCornerHandlers, mjResizeBottomLeft, aRect);
    DrawHandler(HandlingDef.FCornerHandlers, HandlerRect);
  end;

  // Draw middle Handlers :
  if HandlingDef.MiddleHandlers.Visible
  then begin
    Canvas.Pen.Assign(HandlingDef.FMiddleHandlers.Pen);
    Canvas.Pen.Width := 1;
    Canvas.Brush.Assign(HandlingDef.FMiddleHandlers.FBrush);

    HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeTop, aRect);
    DrawHandler(HandlingDef.FMiddleHandlers, HandlerRect);

    HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeRight, aRect);
    DrawHandler(HandlingDef.FMiddleHandlers, HandlerRect);

    HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeBottom, aRect);
    DrawHandler(HandlingDef.FMiddleHandlers, HandlerRect);

    HandlerRect := CalcHandlerRect(HandlingDef.FMiddleHandlers, mjResizeLeft, aRect);
    DrawHandler(HandlingDef.FMiddleHandlers, HandlerRect);
  end;
end;

procedure TcyResizer.DrawControlHandlers(ControlIndex: Integer);
var
  HandlingControlItem: THandlingControlItem;
  ControlRect: TRect;
  MultiSelection: Boolean;
begin
  HandlingControlItem := FHandlingControlList.GetItem(ControlIndex);
  ControlRect := GetHandlingControlItemClientRect(HandlingControlItem);

  if ControlIndex = 0
  then begin
    if csDesigning in ComponentState
    then MultiSelection := false
    else MultiSelection := FHandlingControlList.Count > 1;
  end
  else
    MultiSelection := true;

  if MultiSelection
  then DrawHandlers(FHandlingMultipleControls, ControlRect)
  else DrawHandlers(FHandlingSingleControl, ControlRect);
end;

procedure TcyResizer.DrawControlGuideLines(ControlIndex: Integer);
var
  HandlingControlItem: THandlingControlItem;
  HandlingControlRect, BoundsRecti : TRect;
  fromScreenPoint, toScreenPoint, fromClientPoint, toClientPoint: TPoint;
  i, HCenter, VCenter, Inner1, Inner2, Outer1, Outer2: integer;

      function CanDrawWithTarget(curSelected: TControl; aTargetControl: TControl): boolean;
      begin
        RESULT := false;

        if curSelected <> aTargetControl
        then
          if FGuideLines.FTargets <> [gtUnselectedControls, gtselectedControls]
          then begin
            if gtUnselectedControls in FGuideLines.FTargets
            then RESULT := FHandlingControlList.IndexOf(aTargetControl) = -1;

            if gtselectedControls in FGuideLines.FTargets
            then RESULT := FHandlingControlList.IndexOf(aTargetControl) <> -1;
          end
          else
            RESULT := true;
      end;

      procedure DrawGuideLine;
      begin
        fromClientPoint := ScreenToClient(fromScreenPoint);
        toClientPoint   := ScreenToClient(toScreenPoint);
        Canvas.Pen.Width := 1;
        Canvas.Pen.Color := FGuideLines.Color;
        Canvas.MoveTo(fromClientPoint.x, fromClientPoint.y);
        Canvas.LineTo(toClientPoint.x, toClientPoint.y);
      end;

begin
  if ((not FMouseHandling) or (not (FMouseHandlingJob in [mjResizeTopLeft..mjResizeBottomRight])))
    and (not (FKeyHandlingJob in [kjLargejMove..kjLargeResize]))
  then EXIT;

  HandlingControlItem := FHandlingControlList.GetItem(ControlIndex);
  HandlingControlRect := HandlingControlItem.FControl.BoundsRect;
  HCenter := (HandlingControlRect.Left + (HandlingControlRect.Right - HandlingControlRect.Left) div 2);
  VCenter := (HandlingControlRect.Top + (HandlingControlRect.Bottom - HandlingControlRect.Top) div 2);

  with HandlingControlItem do
    for i := 0 to FControl.Parent.ControlCount-1 do
      if CanDrawWithTarget(FControl, FControl.Parent.Controls[i])
      then begin
        BoundsRecti := FControl.Parent.Controls[i].BoundsRect;

        // *** Check vertical alignment *** //
        if BoundsRecti.Top < HandlingControlRect.Top
        then begin
          Inner1 := BoundsRecti.Bottom;
          Inner2 := HandlingControlRect.Top;
          Outer1 := BoundsRecti.Top;
          Outer2 := HandlingControlRect.Bottom;
        end
        else begin
          Inner1 := HandlingControlRect.Bottom;
          Inner2 := BoundsRecti.Top;
          Outer1 := HandlingControlRect.Top;
          Outer2 := BoundsRecti.Bottom;
        end;

        // Compare with left position:
        if (HandlingControlRect.Left = BoundsRecti.Left) or (HandlingControlRect.Left = BoundsRecti.Right)
        then begin
          fromScreenPoint := FControl.Parent.ClientToScreen( point(HandlingControlRect.Left, Outer1) );
          ToScreenPoint   := FControl.Parent.ClientToScreen( point(HandlingControlRect.Left, Outer2) );
          DrawGuideLine;
        end;

        // Compare with right position:
        if (HandlingControlRect.Right = BoundsRecti.Left) or (HandlingControlRect.Right = BoundsRecti.Right)
        then begin
          fromScreenPoint := FControl.Parent.ClientToScreen( point(HandlingControlRect.Right, Outer1) );
          ToScreenPoint   := FControl.Parent.ClientToScreen( point(HandlingControlRect.Right, Outer2) );
          DrawGuideLine;
        end;

        // Compare with centered position :
        if HCenter = (BoundsRecti.Left + (BoundsRecti.Right - BoundsRecti.Left) div 2)
        then begin
          fromScreenPoint := FControl.Parent.ClientToScreen( point(HCenter, Inner1) );
          ToScreenPoint   := FControl.Parent.ClientToScreen( point(HCenter, Inner2) );
          DrawGuideLine;
        end;

        // *** Check horizontal alignment *** //
        if BoundsRecti.Left < HandlingControlRect.Left
        then begin
          Inner1 := BoundsRecti.Right;
          Inner2 := HandlingControlRect.Left;
          Outer1 := BoundsRecti.Left;
          Outer2 := HandlingControlRect.Right
        end
        else begin
          Inner1 := HandlingControlRect.Right;
          Inner2 := BoundsRecti.Left;
          Outer1 := HandlingControlRect.Left;
          Outer2 := BoundsRecti.Right;
        end;

        // Compare with top position:
        if (HandlingControlRect.Top = BoundsRecti.Top) or (HandlingControlRect.Top = BoundsRecti.Bottom)
        then begin
          fromScreenPoint := FControl.Parent.ClientToScreen( point(Outer1, HandlingControlRect.Top) );
          ToScreenPoint   := FControl.Parent.ClientToScreen( point(Outer2, HandlingControlRect.Top) );
          DrawGuideLine;
        end;

        // Compare with bottom position:
        if (HandlingControlRect.Bottom = BoundsRecti.Top) or (HandlingControlRect.Bottom = BoundsRecti.Bottom)
        then begin
          fromScreenPoint := FControl.Parent.ClientToScreen( point(Outer1, HandlingControlRect.Bottom) );
          ToScreenPoint   := FControl.Parent.ClientToScreen( point(Outer2, HandlingControlRect.Bottom) );
          DrawGuideLine;
        end;

        // Compare with centered position :
        if VCenter = (BoundsRecti.Top + (BoundsRecti.Bottom - BoundsRecti.Top) div 2)
        then begin
          fromScreenPoint := FControl.Parent.ClientToScreen( point(Inner1, VCenter) );
          ToScreenPoint   := FControl.Parent.ClientToScreen( point(Inner2, VCenter) );
          DrawGuideLine;
        end;
      end;
end;

procedure TcyResizer.DrawDesignTimeControlHandlers;
var
  aRect: TRect;
  Flags: Longint;
begin
  Flags := DT_EXPANDTABS or DT_VCENTER or DT_CENTER;
  Flags := DrawTextBiDiModeFlags(Flags);

  // Preview FHandlingSingleControl
  aRect := classes.Rect(10, 10, 160, 50);
  Canvas.Brush.Color := clBtnShadow;
  Canvas.FrameRect(aRect);
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, 'HandlingSingleControl', -1, aRect, Flags);
  Canvas.Brush.Style := bsSolid;
  DrawHandlers(FHandlingSingleControl, aRect);

  // Preview FHandlingMultipleControls
  aRect := classes.Rect(10, 70, 160, 110);
  Canvas.Brush.Color := clBtnShadow;
  Canvas.FrameRect(aRect);
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, 'HandlingMultipleControls', -1, aRect, Flags);
  Canvas.Brush.Style := bsSolid;
  DrawHandlers(FHandlingMultipleControls, aRect);
end;

procedure TcyResizer.Activate(Surface: TWinControl);
begin
  if not (FActive) and (Surface <> nil)
  then begin
    FSurface := Surface;
    Parent := Surface;

    Top := 0;
    Left := 0;
    Height := Surface.ClientHeight;
    Width := Surface.ClientWidth;
    Visible := true;
    FActive := true;
    BringToFront;
    SetFocus;
  end;
end;

procedure TcyResizer.DblClick;
begin
  inherited;
end;

procedure TcyResizer.Deactivate;
begin
  if FActive
  then begin
    FActive := false;
    FHandlingControlList.Clear;
    Visible := false;
  end;
end;

function TcyResizer.GetState: TResizerState;
var C: Integer;
begin
  if FActive
  then begin
    C := FHandlingControlList.Count;

    if FMouseHandling  // Mouse down ...
    then begin
      if FMouseHandlingJob = mjRangeSelect
      then
        RESULT := rsRangeSelect
      else
        if FMouseHandlingJob = mjMove
        then begin
          if C = 1
          then RESULT := rsMovingOneControl
          else RESULT := rsMovingMultipleControls;
        end
        else begin
          if C = 1
          then RESULT := rsResizingOneControl
          else RESULT := rsResizingMultipleControls;
        end;
    end
    else begin
      case C of
        0: RESULT := rsHandlingNothing;
        1: RESULT := rsOneControlSelected;
        else
           RESULT := rsMultipleControlsSelected;
      end;
    end;
  end
  else
    RESULT := rsInactive;
end;

procedure TcyResizer.ControlsFrameChanged(Sender: TObject);
begin
  invalidate;
end;

procedure TcyResizer.GridChanged(Sender: TObject);
begin
  invalidate;
end;

procedure TcyResizer.HandlingMultipleControlsChanged(Sender: TObject);
begin
  invalidate;
end;

procedure TcyResizer.HandlingSingleControlChanged(Sender: TObject);
begin
  invalidate;
end;

procedure TcyResizer.SetControlFrame(const Value: TcyControlFrame);
begin
  FControlFrame := Value;
end;

procedure TcyResizer.SetGrid(const Value: TcyGrid);
begin
  FGrid := Value;
end;

procedure TcyResizer.SetGuideLines(const Value: TcyGuideLines);
begin
  FGuideLines := Value;
end;

procedure TcyResizer.SetHandlingMultipleControls(const Value: TcyHandlingDef);
begin
  FHandlingMultipleControls := Value;
end;

procedure TcyResizer.SetHandlingSingleControl(const Value: TcyHandlingDef);
begin
  FHandlingSingleControl := Value;
end;

procedure TcyResizer.SetKeyHandlingJob(const Value: TKeyJob);
begin
  if FKeyHandlingJob = Value then Exit;
  FKeyHandlingJob := Value;

  if Assigned(FOnKeyJobChange) then
    FOnKeyJobChange(Self);
end;

procedure TcyResizer.SetOptions(const Value: TResizerOptions);
begin
  FOptions := Value;
end;

procedure TcyResizer.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;

  if FReadOnly
  then begin
    FMouseHandling := false;
    Cursor := GetMouseJobCursor(mjNothing);
  end;
end;

function TcyResizer.DetermineControlAtPos(ParentControl: TWinControl; atClientPoint: TPoint): TControl;

    function IsControlAtPos(AControl: TControl): Boolean;
    var AControlPoint: TPoint;
    begin
      AControlPoint := Point(atClientPoint.X - AControl.Left, atClientPoint.Y - AControl.Top);
      Result := PtInRect(classes.Rect(0, 0, AControl.Width-1, AControl.Height-1), AControlPoint);
    end;

    function GetChildControlAtPos: TControl;
    var i: Integer;
    begin
      RESULT := Nil;
      for i := ParentControl.ControlCount - 1 downto 0 do
        if not (ParentControl.Controls[i] is TcyResizer)
        then begin
          if IsControlAtPos(ParentControl.Controls[i])
          then
            if ParentControl.Controls[i].Visible
            then begin
              RESULT := ParentControl.Controls[i];
              Break;
            end;
        end;
    end;

var FoundControl: TControl;
begin
  RESULT := nil;

  while ParentControl <> nil do
  begin
    FoundControl := GetChildControlAtPos;

    if FoundControl <> nil
    then begin
      RESULT := FoundControl;

      if FoundControl is TWinControl
      then begin
        atClientPoint := ParentControl.ClientToScreen(atClientPoint);
        ParentControl := TWinControl(FoundControl);
        atClientPoint := ParentControl.ScreenToClient(atClientPoint);
      end
      else
        ParentControl := Nil;
    end
    else
      ParentControl := nil;
  end;
end;

procedure TcyResizer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  aMouseJob: TMouseJob;
  aHandlingControl: TControl;
  aHandlingControlRect, HandlerRect: TRect;
  ControlAccepted: Boolean;
  aPt: TPoint;
  i: Integer;
begin
  ControlAccepted := true;
  if not Focused then SetFocus;

  if (not FReadOnly) and (Button = mbLeft)
  then begin
    FMouseHandlingFromPoint := Point(x, y);
    // Determine mouse over control :
    GetHandlingMouseInformation(FMouseHandlingFromPoint, aHandlingControl, aHandlingControlRect, HandlerRect, aMouseJob);
    MouseHandlingJob := aMouseJob;

    if (aHandlingControl = Nil) or (FMouseHandlingJob = mjMove)
    then FLastMouseDownControl := DetermineControlAtPos(FSurface, FMouseHandlingFromPoint)  // Retrieve topMost control at mouse position ...
    else FLastMouseDownControl := aHandlingControl;


    if FLastMouseDownControl <> Nil
    then begin
      if (aHandlingControl = FLastMouseDownControl) and (MouseHandlingJob <> mjNothing)
      then begin
        if ssCtrl in Shift
        then begin
          if roMouseSelect in FOptions
          then begin
            FHandlingControlList.Clear;

            if Assigned(FOnControlListInsert)
            then FOnControlListInsert(Self, aHandlingControl, ControlAccepted);

            if ControlAccepted
            then begin
              FHandlingControlList.InsertControl(aHandlingControl);
              MouseHandlingJob := mjRangeSelect;
              FMouseHandling := MouseHandlingJob <> mjNothing;
            end;
          end;
        end
        else
          if ssShift in Shift
          then begin
            if roMouseSelect in FOptions
            then FHandlingControlList.RemoveControl(aHandlingControl);
          end
          else begin
            // Prepare to move/resize :
            FMouseHandling := MouseHandlingJob <> mjNothing;
          end;
      end
      else
        if roMouseSelect in FOptions
        then begin
          if not (ssShift in Shift) or not (roMouseMultiSelect in FOptions)
          then FHandlingControlList.Clear;

          if not (ssCtrl in Shift)   // Add control to handled ControlList
          then begin
            if Assigned(FOnControlListInsert)
            then FOnControlListInsert(Self, aHandlingControl, ControlAccepted);

            if ControlAccepted
            then begin
              FHandlingControlList.InsertControl(FLastMouseDownControl);
              MouseHandlingJob := mjMove;  // Start to move after selecting ...
              FMouseHandling := MouseHandlingJob <> mjNothing;
            end;
          end;
        end;
    end
    else
      if not (ssShift in Shift) or not (roMouseMultiSelect in FOptions) then
        if roMouseUnselectAll in FOptions    // Unselect all ...
        then FHandlingControlList.Clear;
  end
  else
    FLastMouseDownControl := DetermineControlAtPos(FSurface, FMouseHandlingFromPoint);  // Retrieve topMost control at mouse position ...

  if (not FMouseHandling) and (ControlAccepted)
  then begin
    MouseHandlingJob := mjRangeSelect;
    FMouseHandling := MouseHandlingJob <> mjNothing;
  end;

  if FMouseHandling
  then begin
    if FMouseHandlingJob = mjRangeSelect
    then begin
      FRangeSelectMinX := 0;
      FRangeSelectMaxX := Width;
      FRangeSelectMinY := 0;
      FRangeSelectMaxY := Height;

      // Range selection inside a control?
      if FLastMouseDownControl <> Nil
      then
        if FLastMouseDownControl is TWinControl
        then
          if csAcceptsControls in FLastMouseDownControl.ControlStyle
          then begin
            aPt := ScreenToClient( FLastMouseDownControl.ClientToScreen(Point(0, 0)) );
            FRangeSelectMinX := aPt.X;
            FRangeSelectMinY := aPt.Y;
            FRangeSelectMaxX := aPt.X + FLastMouseDownControl.Width;
            FRangeSelectMaxY := aPt.Y + FLastMouseDownControl.Height;
          end;
    end
    else begin
      for i := 0 to FHandlingControlList.Count-1 do
        FHandlingControlList.Items[i].FOldRect := FHandlingControlList.Items[i].FControl.BoundsRect;
    end;

    FLastMouseJob := FMouseHandlingJob;
    FMouseHandling1stTask := true;
  end;

  Cursor := GetMouseJobCursor(FLastMouseJob);
  inherited;
end;

procedure TcyResizer.MouseMove(Shift: TShiftState; X, Y: Integer);

    function AllowMouseMove: boolean;
    begin
      if FHandlingControlList.Count > 1
      then begin
        RESULT := roMouseMultiMove in FOptions;

        if RESULT
        then
          if roOnlyMultiMoveIfSameParent in FOptions
          then
            if not FHandlingControlList.ControlsFromSameParent
            then RESULT := false;
      end
      else
        RESULT := roMouseMove in FOptions;
    end;

    function AllowMouseResize: boolean;
    begin
      if FHandlingControlList.Count > 1
      then begin
        RESULT := roMouseMultiResize in FOptions;

        if RESULT
        then
          if roOnlyMultiResizeIfSameParent in FOptions
          then
            if not FHandlingControlList.ControlsFromSameParent
            then RESULT := false;
      end
      else
        RESULT := roMouseResize in FOptions;
    end;

    procedure MakeValidFocusRect(var DestRect: TRect; X1, X2, Y1, Y2: Integer);
    var i: Integer;
    begin
      if X2 < X1
      then begin
        i := X1;
        X1 := X2;
        X2 := i;
      end;

      if Y2 < Y1
      then begin
        i := Y1;
        Y1 := Y2;
        Y2 := i;
      end;

      DestRect := classes.Rect(X1, Y1, X2, Y2);
    end;

var
  IgnoreSnapToGrid: Boolean;
  MousePosition: TPoint;
  aControl: TControl;
  ControlRect, HandlerRect: TRect;
  Job: TMouseJob;
begin
  if FMouseHandling
  then begin
    IgnoreSnapToGrid := ssAlt in Shift;
    // Do the job :
    case FMouseHandlingJob of
      mjRangeSelect:
          begin
            if not FMouseHandling1stTask
            then canvas.DrawFocusRect(FMouseFocusRect);

            if X < FRangeSelectMinX then X := FRangeSelectMinX;
            if X > FRangeSelectMaxX then X := FRangeSelectMaxX;
            if Y < FRangeSelectMinY then Y := FRangeSelectMinY;
            if Y > FRangeSelectMaxY then Y := FRangeSelectMaxY;

            MakeValidFocusRect(FMouseFocusRect, FMouseHandlingFromPoint.X, X, FMouseHandlingFromPoint.Y, Y);
            canvas.DrawFocusRect(FMouseFocusRect);
          end;

      mjMove:
        begin
          if AllowMouseMove
          then MoveSelectionFromOriginalPosition(X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeTopLeft:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reLeft, reTop], X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeTop:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reTop], X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeTopRight:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reTop, reRight], X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeLeft:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reLeft], X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeRight:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reRight], X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeBottomLeft:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reLeft, reBottom],  X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeBottom:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reBottom],  X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;

      mjResizeBottomRight:
        begin
          if AllowMouseResize
          then ResizeSelectionFromOriginalPosition([reRight, reBottom],  X - FMouseHandlingFromPoint.X, Y - FMouseHandlingFromPoint.Y, IgnoreSnapToGrid);
        end;
    end;

    FMouseHandling1stTask := false;
  end
  else begin
    // Visualize correct cursor :
    if FReadOnly
    then begin
      Job := mjNothing;
    end
    else begin
      MousePosition := Point(x, y);
      GetHandlingMouseInformation(MousePosition, aControl, ControlRect, HandlerRect, Job);
    end;

    Cursor := GetMouseJobCursor(Job);
  end;

  inherited;
end;

procedure TcyResizer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aParentControl: TWinControl;
  aParentRect: TRect;
begin
  inherited;

  if FMouseHandling
  then
    if FMouseHandlingJob = mjRangeSelect
    then begin
      if not FMouseHandling1stTask   // Just do a simple click
      then begin
        canvas.DrawFocusRect(FMouseFocusRect);
        aParentControl := FSurface;
        aParentRect := FMouseFocusRect;

        if FLastMouseDownControl <> Nil
        then
          if FLastMouseDownControl is TWinControl
          then
            if csAcceptsControls in FLastMouseDownControl.ControlStyle
            then begin
              aParentControl := TWinControl(FLastMouseDownControl);
              aParentRect := ClientToScreenRect(FSurface, aParentRect);
              aParentRect := ScreenToClientRect(aParentControl, aParentRect);
            end;

//        if not (ssShift in Shift)
//        then FHandlingControlList.Clear;

        InsertControlsInRect(aParentControl, aParentRect);
      end;
    end
    else
      Invalidate;   // Hide guidelines?

  FMouseHandling := false;
end;

procedure TcyResizer.InsertControlsInRect(ParentControl: TWinControl; ControlRect: TRect);
var
  i: Integer;
  aRsltRect: TRect;
begin
  for i := 0 to ParentControl.ControlCount-1 do
    if not (ParentControl.Controls[i] is TcyResizer)
    then
      if ParentControl.Controls[i].Visible
      then
        if IntersectRect(aRsltRect, ControlRect, ParentControl.Controls[i].BoundsRect)
        then begin
          FHandlingControlList.InsertControl(ParentControl.Controls[i]);

          if not (roMouseMultiSelect in FOptions) then
            Break;
        end;
end;

procedure TcyResizer.ValidateNewLeft(aControl: TControl; var NewLeft: Integer);
var Limit: Integer;
begin
  if not (roOutsideParentRect in FOptions) then
    if NewLeft > 0 then
    begin
      if aControl.Parent = Nil
      then Limit := Screen.Width - aControl.Width
      else Limit := aControl.Parent.Width - aControl.Width;

      if NewLeft > Limit then
        NewLeft := Limit;
    end
    else
      NewLeft := 0;
end;

procedure TcyResizer.ValidateNewTop(aControl: TControl; var NewTop: Integer);
var Limit: Integer;
begin
  if not (roOutsideParentRect in FOptions) then
    if NewTop > 0 then
    begin
      if aControl.Parent = Nil
      then Limit := Screen.Height - aControl.Height
      else Limit := aControl.Parent.Height - aControl.Height;

      if NewTop > Limit then
        NewTop := Limit;
    end
    else
      NewTop := 0;
end;

procedure TcyResizer.ValidateNewWidth(aControl: TControl; var NewWidth: Integer; ResizeEdges: TResizeEdges);
var NewLeft, Limit: Integer;
begin
  if aControl.Constraints.MaxWidth <> 0
  then
    if NewWidth > aControl.Constraints.MaxWidth
    then NewWidth := aControl.Constraints.MaxWidth;

  if aControl.Constraints.MinWidth <> 0
  then
    if NewWidth < aControl.Constraints.MinWidth
    then NewWidth := aControl.Constraints.MinWidth;

  if not (roOutsideParentRect in FOptions) then
  begin
    // Correct resizing left corner :
    NewLeft := aControl.Left + aControl.Width - NewWidth;
    if (reLeft in ResizeEdges) and (NewLeft < 0) then
      NewWidth := NewWidth + NewLeft;

    // Correct resizing right corner :
    if reRight in ResizeEdges then
    begin
      if aControl.Parent = Nil
      then Limit := Screen.Width
      else Limit := aControl.Parent.Width;

      if aControl.Left + NewWidth > Limit then
        NewWidth := Limit - aControl.Left;
    end;
  end;

  if NewWidth < 1 then NewWidth := 1;
end;

procedure TcyResizer.ValidateNewHeight(aControl: TControl; var NewHeight: Integer; ResizeEdges: TResizeEdges);
var NewTop, Limit: Integer;
begin
  if aControl.Constraints.MaxHeight <> 0
  then
    if NewHeight > aControl.Constraints.MaxHeight
    then NewHeight := aControl.Constraints.MaxHeight;

  if aControl.Constraints.MinHeight <> 0
  then
    if NewHeight < aControl.Constraints.MinHeight
    then NewHeight := aControl.Constraints.MinHeight;

  if not (roOutsideParentRect in FOptions) then
  begin
    // Correct resizing top corner :
    NewTop := aControl.Top + aControl.Height - NewHeight;
    if (reTop in ResizeEdges) and (NewTop < 0) then
      NewHeight := NewHeight + NewTop;

    // Correct resizing bottom corner :
    if reBottom in ResizeEdges then
    begin
      if aControl.Parent = Nil
      then Limit := Screen.Height
      else Limit := aControl.Parent.Height;

      if aControl.Top + NewHeight > Limit then
        NewHeight := Limit - aControl.Top;
    end;
  end;

  if NewHeight < 1 then NewHeight := 1;
end;

procedure TcyResizer.MoveSelection(IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
var
  i, x, NewLeft, NewTop: Integer;
  DoInvalidate: Boolean;
begin
  DoInvalidate := false;

  if Assigned(FOnMoveSelection) then
    FOnMoveSelection(Self, IncX, IncY);

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
      then begin
        if IncX <> 0
        then begin
          x := (FControl.Left + IncX) div FGrid.FColumnSize;
          NewLeft := x * FGrid.FColumnSize;

          // Force make a move :
          if (IncX > 0) and (NewLeft <= FControl.Left)
          then NewLeft := NewLeft + FGrid.FColumnSize;
        end
        else
          NewLeft := FControl.Left;

        if IncY <> 0
        then begin
          x := (FControl.Top + IncY) div FGrid.FRowSize;
          NewTop := x * FGrid.FRowSize;

          // Force make a move :
          if (IncY > 0) and (NewTop <= FControl.Top)
          then NewTop := NewTop + FGrid.FRowSize;
        end
        else
          NewTop := FControl.Top;
      end
      else begin
        NewLeft := FControl.Left + IncX;
        NewTop := FControl.Top + IncY;
      end;

      ValidateNewLeft(FControl, NewLeft);
      ValidateNewTop(FControl, NewTop);

      if (NewLeft <> FControl.Left) or (NewTop <> FControl.Top)
      then begin
        FControl.Left := NewLeft;
        FControl.Top := NewTop;
        DoInvalidate := true;
      end;
    end;

  if Assigned(FAfterMoveSelection) then
    FAfterMoveSelection(Self);

  if DoInvalidate
  then Invalidate;
end;

procedure TcyResizer.ResizeSelection(ResizeEdges: TResizeEdges; IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
var
  i, x, NewWidth, NewHeight: Integer;
  DoInvalidate: Boolean;
begin
  DoInvalidate := false;

  if Assigned(FOnResizeSelection) then
    FOnResizeSelection(Self, ResizeEdges, IncX, IncY);

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      NewWidth := FControl.Width;
      NewHeight := FControl.Height;

      if FControl.Align in [alClient, alTop, alBottom]
      then IncX := 0;

      if FControl.Align in [alClient, alLeft, alRight]
      then IncY := 0;

      if ReLeft in ResizeEdges    // Adjust left edge (right edge must be identique)
      then begin
        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Determine snaped left edge position :
          x := (FControl.Left + IncX) div FGrid.FColumnSize;
          x := x * FGrid.FColumnSize;
          NewWidth := FControl.Left + FControl.Width - x;
        end
        else
          NewWidth := FControl.Width - IncX;

        ValidateNewWidth(FControl, NewWidth, ResizeEdges);
        FControl.Left := FControl.Left + FControl.Width - NewWidth;
      end;

      if ReTop in ResizeEdges     // Adjust top edge (bottom edge must be identique)
      then begin
        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Determine snaped top edge position :
          x := (FControl.Top + IncY) div FGrid.FRowSize;
          x := x * FGrid.FRowSize;
          NewHeight := FControl.Top + FControl.Height - x;
        end
        else
          NewHeight := FControl.Height - IncY;

        ValidateNewHeight(FControl, NewHeight, ResizeEdges);
        FControl.Top := FControl.Top + FControl.Height - NewHeight;
      end;

      if ReRight in ResizeEdges
      then begin
        NewWidth := NewWidth + IncX;

        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Adjust right edge :
          x := (FControl.Left + NewWidth) div FGrid.FColumnSize;
          NewWidth := x * FGrid.FColumnSize - FControl.Left;

          // Force resize :
          if (IncX > 0) and (NewWidth <= FControl.Width)
          then NewWidth := NewWidth + FGrid.FColumnSize;
        end;

        ValidateNewWidth(FControl, NewWidth, ResizeEdges);
      end;

      if ReBottom in ResizeEdges
      then begin
        NewHeight := NewHeight + IncY;

        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Adjust bottom edge :
          x := (FControl.Top + NewHeight) div FGrid.FRowSize;
          NewHeight := x * FGrid.FRowSize - FControl.Top;

          // Force resize :
          if (IncY > 0) and (NewHeight <= FControl.Height)
          then NewHeight := NewHeight + FGrid.FRowSize;
        end;

        ValidateNewHeight(FControl, NewHeight, ResizeEdges);
      end;

      if (FControl.Width <> NewWidth) or (FControl.Height <> NewHeight)
      then begin
        DoInvalidate := true;
        FControl.Width := NewWidth;
        FControl.Height := NewHeight;
      end;
    end;

  if Assigned(FAfterResizeSelection) then
    FAfterResizeSelection(Self);

  if DoInvalidate
  then Invalidate;
end;

procedure TcyResizer.MoveSelectionFromOriginalPosition(IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
var
  i, x, NewLeft, NewTop: Integer;
  DoInvalidate: Boolean;
begin
  DoInvalidate := false;

  if Assigned(FOnMoveSelection) then
    FOnMoveSelection(Self, IncX, IncY);

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
      then begin
        x := (FOldRect.Left + IncX) div FGrid.FColumnSize;
        NewLeft := x * FGrid.FColumnSize;

        x := (FOldRect.Top + IncY) div FGrid.FRowSize;
        NewTop := x * FGrid.FRowSize;
      end
      else begin
        NewLeft := FOldRect.Left + IncX;
        NewTop := FOldRect.Top + IncY;
      end;

      ValidateNewLeft(FControl, NewLeft);
      ValidateNewTop(FControl, NewTop);

      if (NewLeft <> FControl.Left) or (NewTop <> FControl.Top)
      then begin
        FControl.Left := NewLeft;
        FControl.Top := NewTop;
        DoInvalidate := true;
      end;
    end;

  if Assigned(FAfterMoveSelection) then
    FAfterMoveSelection(Self);

  if DoInvalidate
  then Invalidate;
end;

procedure TcyResizer.ResizeSelectionFromOriginalPosition(ResizeEdges: TResizeEdges; IncX, IncY: Integer; IgnoreSnapToGrid: Boolean);
var
  i, x, NewWidth, NewHeight: Integer;
  DoInvalidate: Boolean;
begin
  DoInvalidate := false;

  if Assigned(FOnResizeSelection) then
    FOnResizeSelection(Self, ResizeEdges, IncX, IncY);

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      NewWidth := FControl.Width;
      NewHeight := FControl.Height;

      if FControl.Align in [alClient, alTop, alBottom]
      then IncX := 0;

      if FControl.Align in [alClient, alLeft, alRight]
      then IncY := 0;

      if ReLeft in ResizeEdges    // Adjust left edge (right edge must be identique)
      then begin
        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Determine snaped left edge position :
          x := (FOldRect.Left + IncX) div FGrid.FColumnSize;
          x := x * FGrid.FColumnSize;
          NewWidth := FOldRect.Right - x;
        end
        else
          NewWidth := (FOldRect.Right - FOldRect.Left) - IncX;

        ValidateNewWidth(FControl, NewWidth, ResizeEdges);
        FControl.Left := FOldRect.Left + (FOldRect.Right - FOldRect.Left) - NewWidth;
      end;

      if ReTop in ResizeEdges     // Adjust top edge (bottom edge must be identique)
      then begin
        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Determine snaped top edge position :
          x := (FOldRect.Top + IncY) div FGrid.FRowSize;
          x := x * FGrid.FRowSize;
          NewHeight := FOldRect.Bottom - x;
        end
        else
          NewHeight := (FOldRect.Bottom - FOldRect.Top) - IncY;

        ValidateNewHeight(FControl, NewHeight, ResizeEdges);
        FControl.Top := FOldRect.Top + (FOldRect.Bottom - FOldRect.Top) - NewHeight;
      end;

      if ReRight in ResizeEdges
      then begin
        NewWidth := (FOldRect.Right - FOldRect.Left) + IncX;

        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Adjust right edge :
          x := (FOldRect.Left + NewWidth) div FGrid.FColumnSize;
          NewWidth := x * FGrid.FColumnSize - FOldRect.Left;
        end;

        ValidateNewWidth(FControl, NewWidth, ResizeEdges);
      end;

      if ReBottom in ResizeEdges
      then begin
        NewHeight := (FOldRect.Bottom - FOldRect.Top) + IncY;

        if FGrid.SnapToGrid and (not IgnoreSnapToGrid)
        then begin
          // Adjust bottom edge :
          x := (FOldRect.Top + NewHeight) div FGrid.FRowSize;
          NewHeight := x * FGrid.FRowSize - FOldRect.Top;
        end;

        ValidateNewHeight(FControl, NewHeight, ResizeEdges);
      end;

      if (FControl.Width <> NewWidth) or (FControl.Height <> NewHeight)
      then begin
        DoInvalidate := true;
        FControl.Width := NewWidth;
        FControl.Height := NewHeight;
      end;
    end;

  if Assigned(FAfterResizeSelection) then
    FAfterResizeSelection(Self);

  if DoInvalidate
  then Invalidate;
end;

procedure TcyResizer.GetHandlingKeyInformation(Key: Word; ShiftState: TShiftState; KeyDown: Boolean; var Job: TKeyJob);
var Allow: Boolean;
begin
  if roKeySelect in FOptions
  then Job := kjSelectNearest
  else Job := kjDoNothing;

  if ssCtrl in ShiftState
  then begin
    // *** moving controls *** //
    if FHandlingControlList.Count = 1
    then Allow := roKeyMove in FOptions
    else Allow := roKeyMultiMove in FOptions;

    if Allow
    then
      if ssShift in ShiftState
      then Job := kjLargejMove      // Large move
      else Job := kj1PixelMove;     // 1 pixel move
  end
  else
    // *** resizing controls *** //
    if ssShift in ShiftState
    then begin
      if FHandlingControlList.Count = 1
      then Allow := roKeyResize in FOptions
      else Allow := roKeyMultiResize in FOptions;

      if Allow
      then
        if SSAlt in ShiftState
        then Job := kjLargeResize      // Don't work for now ...
        else Job := kj1PixelResize;
    end;

  // Save Arrow key state :
  case Key of
    VK_UP    : FArrowKeyUpPressed    := KeyDown;
    VK_DOWN  : FArrowKeyDownPressed  := KeyDown;
    VK_RIGHT : FArrowKeyRightPressed := KeyDown;
    VK_LEFT  : FArrowKeyLeftPressed  := KeyDown;
  end;
end;

procedure TcyResizer.SetMouseHandlingJob(const Value: TMouseJob);
begin
  if FMouseHandlingJob = Value then Exit;
  FMouseHandlingJob := Value;

  if Assigned(FOnMouseJobChange) then
    FOnMouseJobChange(Self);
end;

procedure TcyResizer.KeyDown(var Key: Word; Shift: TShiftState);
var
  aJob: TKeyJob;
  ParentCtrl: TControl;
begin
  Inherited;

  if Key = VK_ESCAPE
  then begin
    // Select last selected parent control :
    if FHandlingControlList.Count > 0
    then begin
      ParentCtrl := FHandlingControlList.Items[FHandlingControlList.Count-1].FControl.Parent;

      if ParentCtrl <> FSurface
      then begin
        FHandlingControlList.Clear;
        FHandlingControlList.InsertControl(ParentCtrl);
      end
      else
        if roKeyUnselectAll in FOptions    // Unselect all ...
        then FHandlingControlList.Clear;
    end;
  end
  else begin
    GetHandlingKeyInformation(Key, Shift, True, aJob);
    KeyHandlingJob := aJob;

    if (Shift <> []) and (FGuideLines.FControls <> gmNone)
    then Invalidate;   // Show guidelines ...
  end;
end;

procedure TcyResizer.KeyUp(var Key: Word; Shift: TShiftState);
var aJob: TKeyJob;
begin
  Inherited;
  GetHandlingKeyInformation(Key, Shift, false, aJob);
  KeyHandlingJob := aJob;

  if (Shift = []) and (FGuideLines.FControls <> gmNone)
  then Invalidate;   // Hide guidelines?
end;

// Allows arrow keys, this code will also permit ".KeyUp" calls when arrow keys pressed for a while ...
procedure TcyResizer.WMGetDlgCode(var message: TMessage);
begin
  message.Result := DLGC_WANTARROWS // or DLGC_WANTCHARS;
end;

// Handle up/down/right/left keys
// unfortunatley, we can' t know if the key is down, up or pressed for a while !!!
procedure TcyResizer.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
var
  c: Integer;
  Nearest: TControl;
begin
  { Important Note :
    Msg.Result := 1 will permit ".KeyDown" call
    Msg.Result := 0 will permit ".KeyUp" call   }

  case Msg.CharCode of
    VK_UP: if FArrowKeyUpPressed then Msg.Result := 0 else Msg.Result := 1;
    VK_DOWN: if FArrowKeyDownPressed then Msg.Result := 0 else Msg.Result := 1;
    VK_RIGHT: if FArrowKeyRightPressed then Msg.Result := 0 else Msg.Result := 1;
    VK_LEFT: if FArrowKeyLeftPressed then Msg.Result := 0 else Msg.Result := 1;
  end;

  // Do the job :
  case FKeyHandlingJob of
    kjSelectNearest:
      if FHandlingControlList.Count = 1
      then
        with FHandlingControlList.Items[0].FControl do
        begin
          Nearest := Nil;

          for c := 0 to Parent.ControlCount-1 do
          begin
            if FArrowKeyUpPressed
            then begin
              if (Parent.Controls[c].Visible) and (Parent.Controls[c].BoundsRect.Top < BoundsRect.Top)
              then
                if Nearest = Nil
                then
                  Nearest := Parent.Controls[c]
                else
                  if Parent.Controls[c].BoundsRect.Top > Nearest.BoundsRect.Top
                  then Nearest := Parent.Controls[c];
            end;

            if FArrowKeyRightPressed
            then begin
              if (Parent.Controls[c].Visible) and (Parent.Controls[c].BoundsRect.Left > BoundsRect.Left)
              then
                if Nearest = Nil
                then
                  Nearest := Parent.Controls[c]
                else
                  if Parent.Controls[c].BoundsRect.Left < Nearest.BoundsRect.Left
                  then Nearest := Parent.Controls[c];
            end;

            if FArrowKeyDownPressed
            then begin
              if (Parent.Controls[c].Visible) and (Parent.Controls[c].BoundsRect.Top > BoundsRect.Top)
              then
                if Nearest = Nil
                then
                  Nearest := Parent.Controls[c]
                else
                  if Parent.Controls[c].BoundsRect.Top < Nearest.BoundsRect.Top
                  then Nearest := Parent.Controls[c];
            end;

            if FArrowKeyLeftPressed
            then begin
              if (Parent.Controls[c].Visible) and (Parent.Controls[c].BoundsRect.Left < BoundsRect.Left)
              then
                if Nearest = Nil
                then
                  Nearest := Parent.Controls[c]
                else
                  if Parent.Controls[c].BoundsRect.Left > Nearest.BoundsRect.Left
                  then Nearest := Parent.Controls[c];
            end;
          end;

          // Select nearest control :
          if Nearest <> Nil
          then begin
            FHandlingControlList.Clear;
            FHandlingControlList.InsertControl(Nearest);
          end;
        end;

    kj1PixelMove:
    begin
      if FArrowKeyUpPressed    then MoveSelection(0, -1, true);
      if FArrowKeyRightPressed then MoveSelection(1, 0, true);
      if FArrowKeyDownPressed  then MoveSelection(0, 1, true);
      if FArrowKeyLeftPressed  then MoveSelection(-1, 0, true);
    end;

    kjLargejMove:
    begin
      if FArrowKeyUpPressed    then MoveSelection(0, (-1) * FGrid.RowSize, true);
      if FArrowKeyRightPressed then MoveSelection(FGrid.ColumnSize, 0, true);
      if FArrowKeyDownPressed  then MoveSelection(0, FGrid.RowSize, true);
      if FArrowKeyLeftPressed  then MoveSelection((-1) * FGrid.ColumnSize, 0, true);
    end;

    kj1PixelResize:
    begin
      if FArrowKeyUpPressed    then ResizeSelection([reBottom], 0, -1, true);
      if FArrowKeyRightPressed then ResizeSelection([reRight], 1, 0, true);
      if FArrowKeyDownPressed  then ResizeSelection([reBottom], 0, 1, true);
      if FArrowKeyLeftPressed  then ResizeSelection([reRight], -1, 0, true);
    end;

    kjLargeResize:
    begin
      if FArrowKeyUpPressed    then ResizeSelection([reBottom], 0, (-1) * FGrid.RowSize, true);
      if FArrowKeyRightPressed then ResizeSelection([reRight], FGrid.ColumnSize, 0, true);
      if FArrowKeyDownPressed  then ResizeSelection([reBottom], 0, FGrid.RowSize, true);
      if FArrowKeyLeftPressed  then ResizeSelection([reRight], (-1) * FGrid.ColumnSize, 0, true);
    end;
  end;
end;

procedure TcyResizer.AlignSelection(AlignControls: TAlignControls; WithFirstControl: Boolean);
var
  i, ToValue: Integer;
  WithControl: TControl;
begin
  if FHandlingControlList.Count <= 0 then exit;

  WithControl := FHandlingControlList.Items[0].FControl;

  if not WithFirstControl
  then
    for i := 1 to FHandlingControlList.Count-1 do
      with FHandlingControlList.Items[i] do
      begin
        case AlignControls of
          acLeftEdges, acHorizontalsCenters:
            if FControl.Left < WithControl.Left then WithControl := FControl;

          acTopEdges, acVerticalCenters:
            if FControl.Top < WithControl.Top then WithControl := FControl;

          acRightEdges:
            if FControl.Left + FControl.Width > WithControl.Left + WithControl.Width then WithControl := FControl;

          acBottomEdges:
            if FControl.Top + FControl.Height > WithControl.Top + WithControl.Height then WithControl := FControl;
        end;
      end;

  with FHandlingControlList.Items[0] do
    case AlignControls of
      acLeftEdges: ToValue := WithControl.Left;
      acTopEdges: ToValue := WithControl.Top;
      acRightEdges: ToValue := WithControl.Left + WithControl.Width;
      acBottomEdges: ToValue := WithControl.Top + WithControl.Height;
      acHorizontalsCenters: ToValue := WithControl.Left + WithControl.Width div 2;
      acVerticalCenters: ToValue := WithControl.Top + WithControl.Height div 2;
    end;

  AlignSelection(AlignControls, ToValue);
end;

procedure TcyResizer.AlignSelection(AlignControls: TAlignControls; ToValue: Integer);
var i: Integer;
begin
  if FHandlingControlList.Count <= 0 then exit;

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      case AlignControls of
        acLeftEdges: FControl.Left := ToValue;
        acTopEdges: FControl.Top := ToValue;
        acRightEdges: FControl.Left := ToValue - FControl.Width;
        acBottomEdges: FControl.Top := ToValue - FControl.Height;
        acHorizontalsCenters: FControl.Left := ToValue - FControl.Width div 2;
        acVerticalCenters: FControl.Top := ToValue - FControl.Height div 2;
      end;
    end;

  Invalidate;
end;

procedure TcyResizer.AlignSelectionToGrid;
var i, x: Integer;
begin
  if FHandlingControlList.Count <= 0 then exit;

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      // Horizontal position:
      x := Round(FControl.Left / FGrid.FColumnSize);
      FControl.Left := x * FGrid.FColumnSize;

      // Vertical position:
      x := Round(FControl.Top / FGrid.FRowSize);
      FControl.Top := x * FGrid.FRowSize;
    end;

  Invalidate;
end;

function TcyResizer.CenterSelection(Horizontally, Vertically: Boolean): Boolean;
var
  i, MostLeft, MostRight, MostTop, MostBottom, CalcMostLeft, CalcMostTop: Integer;
  ParentControl: TWinControl;
begin
  Result := false;
  if FHandlingControlList.Count <= 0 then exit;

  with FHandlingControlList.Items[0] do
  begin
    ParentControl := FControl.Parent;
    MostLeft   := FControl.Left;
    MostRight  := FControl.Left + FControl.Width;
    MostTop    := FControl.Top;
    MostBottom := FControl.Top + FControl.Height;
  end;

  Result := true;
  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
    begin
      if ParentControl <> FControl.Parent
      then begin
        Result := false;
        Break;
      end;
      if MostLeft > FControl.Left then MostLeft := FControl.Left;
      if MostRight < FControl.Left + FControl.Width then MostRight := FControl.Left + FControl.Width;
      if MostTop > FControl.Top then MostTop := FControl.Top;
      if MostBottom < FControl.Top + FControl.Height then MostBottom := FControl.Top + FControl.Height;
    end;

  if Result
  then begin
    // Calc selection center :
    CalcMostLeft := (ParentControl.Width - (MostRight - MostLeft)) div 2;
    CalcMostTop := (ParentControl.Height - (MostBottom - MostTop)) div 2;

    // Move controls :
    for i := 0 to FHandlingControlList.Count-1 do
      with FHandlingControlList.Items[i] do
      begin
        if Horizontally then FControl.Left := FControl.Left + CalcMostLeft - MostLeft;
        if Vertically then FControl.Top := FControl.Top + CalcMostTop - MostTop;
      end;
  end;

  Invalidate;
end;

procedure TcyResizer.MakeSelectionSameSize(SameWidth, SameHeight: Boolean);
begin
  if FHandlingControlList.Count <= 0 then exit;

  if SameWidth then MakeSelectionSameWidth(FHandlingControlList.Items[0].FControl.Width);
  if SameHeight then MakeSelectionSameHeight(FHandlingControlList.Items[0].FControl.Height);
end;

procedure TcyResizer.MakeSelectionSameWidth(NewWidth: Integer);
var i: Integer;
begin
  if FHandlingControlList.Count <= 0 then exit;

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
      FControl.Width := NewWidth;

  Invalidate;
end;

procedure TcyResizer.MakeSelectionSameHeight(NewHeight: Integer);
var i: Integer;
begin
  if FHandlingControlList.Count <= 0 then exit;

  for i := 0 to FHandlingControlList.Count-1 do
    with FHandlingControlList.Items[i] do
      FControl.Height := NewHeight;

  Invalidate;
end;

procedure TcyResizer.SpaceSelectionEquallyHorizontally(IncSpacingSize: Integer; RemovePreviousSpacing: Boolean);
var
  OrderedControlList: array of TControl;
  MostleftControl: TControl;
  i, h, TotalSpacingSize: Integer;

      function ControlOnArray(aControl: TControl): Boolean;
      var c: Integer;
      begin
        Result := false;

        for c := 0 to i-1 do
          if OrderedControlList[c] = aControl
          then begin
            Result := true;
            Break;
          end;
      end;

begin
  if FHandlingControlList.Count <= 1 then exit;

  TotalSpacingSize := 0;
  SetLength(OrderedControlList, FHandlingControlList.Count);

  if not RemovePreviousSpacing
  then begin
    // Retrieve sorted list :
    for i := 0 to High(OrderedControlList) do
    begin
      MostleftControl := Nil;

      for h := 0 to FHandlingControlList.Count-1 do
        if not ControlOnArray(FHandlingControlList.Items[h].FControl)
        then
          if MostLeftControl = nil
          then
            MostleftControl := FHandlingControlList.Items[h].FControl
          else
            if FHandlingControlList.Items[h].FControl.Left < MostLeftControl.Left
            then MostleftControl := FHandlingControlList.Items[h].FControl;

      OrderedControlList[i] := MostLeftControl;
    end;

    // Calc TotalSpacingSize :
    TotalSpacingSize := OrderedControlList[High(OrderedControlList)].Left - OrderedControlList[0].Left;
    for i := 0 to High(OrderedControlList) - 1 do  // Not the last control
      Dec(TotalSpacingSize, OrderedControlList[i].Width);
  end;

  if IncSpacingSize <> 0  // Apply same spacing size between components :
  then TotalSpacingSize := ((TotalSpacingSize + IncSpacingSize) div FHandlingControlList.Count) * FHandlingControlList.Count;

  SpaceSelectionEquallyHorizontally(TotalSpacingSize);
end;

procedure TcyResizer.SpaceSelectionEquallyHorizontally(TotalSpacingSize: Integer);
var
  OrderedControlList: array of TControl;
  MostleftControl: TControl;
  i, h, SpacingSize: Integer;

      function ControlOnArray(aControl: TControl): Boolean;
      var c: Integer;
      begin
        Result := false;

        for c := 0 to i-1 do
          if OrderedControlList[c] = aControl
          then begin
            Result := true;
            Break;
          end;
      end;

begin
  if FHandlingControlList.Count <= 1 then exit;
  SetLength(OrderedControlList, FHandlingControlList.Count);

  // Retrieve sorted list :
  for i := 0 to High(OrderedControlList) do
  begin
    MostleftControl := Nil;

    for h := 0 to FHandlingControlList.Count-1 do
      if not ControlOnArray(FHandlingControlList.Items[h].FControl)
      then
        if MostLeftControl = nil
        then
          MostleftControl := FHandlingControlList.Items[h].FControl
        else
          if FHandlingControlList.Items[h].FControl.Left < MostLeftControl.Left
          then MostleftControl := FHandlingControlList.Items[h].FControl;

    OrderedControlList[i] := MostLeftControl;
  end;

  // Apply control spacing (except for the first control) :
  for i := 1 to High(OrderedControlList) do
  begin
    SpacingSize := TotalSpacingSize div (High(OrderedControlList) + 1 - i);
    OrderedControlList[i].Left := OrderedControlList[i-1].Left + OrderedControlList[i-1].Width + SpacingSize;
    Dec(TotalSpacingSize, SpacingSize);
  end;

  Invalidate;
end;

procedure TcyResizer.SpaceSelectionEquallyVertically(IncSpacingSize: Integer; RemovePreviousSpacing: Boolean);
var
  OrderedControlList: array of TControl;
  MostTopControl: TControl;
  i, h, TotalSpacingSize: Integer;

      function ControlOnArray(aControl: TControl): Boolean;
      var c: Integer;
      begin
        Result := false;

        for c := 0 to i-1 do
          if OrderedControlList[c] = aControl
          then begin
            Result := true;
            Break;
          end;
      end;

begin
  if FHandlingControlList.Count <= 1 then exit;

  TotalSpacingSize := 0;
  SetLength(OrderedControlList, FHandlingControlList.Count);

  if not RemovePreviousSpacing
  then begin
    // Retrieve sorted list :
    for i := 0 to High(OrderedControlList) do
    begin
      MostTopControl := Nil;

      for h := 0 to FHandlingControlList.Count-1 do
        if not ControlOnArray(FHandlingControlList.Items[h].FControl)
        then
          if MostTopControl = nil
          then
            MostTopControl := FHandlingControlList.Items[h].FControl
          else
            if FHandlingControlList.Items[h].FControl.Top < MostTopControl.Top
            then MostTopControl := FHandlingControlList.Items[h].FControl;

      OrderedControlList[i] := MostTopControl;
    end;

    // Calc TotalSpacingSize :
    TotalSpacingSize := OrderedControlList[High(OrderedControlList)].Top - OrderedControlList[0].Top;
    for i := 0 to High(OrderedControlList) - 1 do  // Not the last control
      Dec(TotalSpacingSize, OrderedControlList[i].Height);
  end;

  if IncSpacingSize <> 0  // Apply same spacing size between components :
  then TotalSpacingSize := ((TotalSpacingSize + IncSpacingSize) div FHandlingControlList.Count) * FHandlingControlList.Count;

  SpaceSelectionEquallyVertically(TotalSpacingSize);
end;

procedure TcyResizer.SpaceSelectionEquallyVertically(TotalSpacingSize: Integer);
var
  OrderedControlList: array of TControl;
  MostTopControl: TControl;
  i, h, SpacingSize: Integer;

      function ControlOnArray(aControl: TControl): Boolean;
      var c: Integer;
      begin
        Result := false;

        for c := 0 to i-1 do
          if OrderedControlList[c] = aControl
          then begin
            Result := true;
            Break;
          end;
      end;

begin
  if FHandlingControlList.Count <= 1 then exit;
  SetLength(OrderedControlList, FHandlingControlList.Count);

  // Retrieve sorted list :
  for i := 0 to High(OrderedControlList) do
  begin
    MostTopControl := Nil;

    for h := 0 to FHandlingControlList.Count-1 do
      if not ControlOnArray(FHandlingControlList.Items[h].FControl)
      then
        if MostTopControl = nil
        then
          MostTopControl := FHandlingControlList.Items[h].FControl
        else
          if FHandlingControlList.Items[h].FControl.Top < MostTopControl.Top
          then MostTopControl := FHandlingControlList.Items[h].FControl;

    OrderedControlList[i] := MostTopControl;
  end;

  // Apply control spacing (except for the first control) :
  for i := 1 to High(OrderedControlList) do
  begin
    SpacingSize := TotalSpacingSize div (High(OrderedControlList) + 1 - i);
    OrderedControlList[i].Top := OrderedControlList[i-1].Top + OrderedControlList[i-1].Height + SpacingSize;
    Dec(TotalSpacingSize, SpacingSize);
  end;

  Invalidate;
end;

procedure TcyResizer.WMMove(var Message: TMessage);
begin
  Inherited;

  if FActive then
    if (Top <> 0) or (Left <> 0) then
    begin
      Top := 0;
      Left := 0;
      Height := fSurface.ClientHeight;
      Width := fSurface.ClientWidth;
      Invalidate;
    end;
end;

end.
