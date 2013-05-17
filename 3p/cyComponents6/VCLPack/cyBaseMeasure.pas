{   Component(s):
    TcyBaseMeasure

    Description:
    Base and properties for measure and gauge components

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

unit cyBaseMeasure;

interface

uses VCL.cyGraphics, Classes, Windows, Graphics, Controls;

type
  TBookmarkStyle = (bsNone, bsArrow, bsTriangle, bsLine, bsRectangle, bsCirle, bsPicture);

  TBookmarkItem = class(TCollectionItem)
  private
    FVisible: Boolean;
    FStyle: TBookmarkStyle;
    FWidth: Integer;
    FHeight: Integer;
    FPicture: TPicture;
    FPen: TPen;
    FBrush: TBrush;
    FShowValue: boolean;
    FValueSpacing: integer;
    FSpacing: Integer;
    FValueFormat: String;
    FFont: TFont;
    FValue: Double;
    procedure ChangedProperty(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
    procedure SetBookmarkStyle(const Value: TBookmarkStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetPicture(const Value: TPicture);
    procedure SetWidth(const Value: Integer);
    procedure SetPen(const Value: TPen);
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetShowValue(const Value: boolean);
    procedure SetSpacing(const Value: Integer);
    procedure SetValueFormat(const Value: String);
    procedure SetValueSpacing(const Value: integer);
    procedure SetValue(const Value: Double);
  protected
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Height: Integer read FHeight write SetHeight;
    property Pen: TPen read FPen write SetPen;
    property Picture: TPicture read FPicture write SetPicture;
    property Spacing: Integer read FSpacing write SetSpacing;
    property ShowValue: boolean read FShowValue write SetShowValue default true;
    property Style: TBookmarkStyle read FStyle write SetBookmarkStyle default bsArrow;
    property Value: Double read FValue write SetValue;
    property ValueFormat: String read FValueFormat write SetValueFormat;
    property ValueSpacing: integer read FValueSpacing write SetValueSpacing;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Width: Integer read FWidth write SetWidth;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
  end;

  TBookmarkItemClass = class of TBookmarkItem;

  TBookmarks = Class(TCollection)
  private
    FControl: TControl;
    function GetBookmarkItem(Index: Integer): TBookmarkItem;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); Override;
  public
    constructor Create(aControl: TControl; BookmarkItemClass: TBookmarkItemClass);
    function Add: TBookmarkItem;
    property Items[Index: Integer]: TBookmarkItem read GetBookmarkItem; default;
  end;



  TLevel = class(TCollectionItem)
  private
    FVisible: Boolean;
    FFromValue: Double;
    FToValue: Double;
    FToColor: TColor;
    FFromColor: TColor;
    procedure SetFromColor(const Value: TColor);
    procedure SetFromValue(const Value: Double);
    procedure SetToColor(const Value: TColor);
    procedure SetToValue(const Value: Double);
    procedure SetVisible(const Value: Boolean);
  protected
  public
    constructor Create(Collection: TCollection); override;
  published
    property FromColor: TColor read FFromColor write SetFromColor;
    property FromValue: Double read FFromValue write SetFromValue;
    property ToColor: TColor read FToColor write SetToColor;
    property ToValue: Double read FToValue write SetToValue;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TLevelClass = class of TLevel;

  TLevels = Class(TCollection)
  private
    FControl: TControl;
    function GetLevel(Index: Integer): TLevel;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); Override;
  public
    constructor Create(aControl: TControl; LevelClass: TLevelClass);
    function Add: TLevel;
    property Items[Index: Integer]: TLevel read GetLevel; default;
  end;



  TGraduationStyle = (gsGuideLine, gsMarks);   

  TGraduation = class(TCollectionItem)
  private
    FPen: TPen;
    FSize: Word;
    FFont: TFont;
    FStep: Word;
    FSpacing: integer;
    FValueFormat: String;
    FShowMarks: boolean;
    FShowValues: boolean;
    FStyle: TGraduationStyle;
    procedure ChangedProperty(Sender: TObject);
    procedure SetSize(const Value: Word);
    procedure SetPen(const Value: TPen);
    procedure SetFont(const Value: TFont);
    procedure SetStep(const Value: Word);
    procedure SetSpacing(const Value: integer);
    procedure SetvalueFormat(const Value: String);
    procedure SetShowMarks(const Value: boolean);
    procedure SetShowValues(const Value: boolean);
    procedure SetStyle(const Value: TGraduationStyle);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont;
    property Size: Word read FSize write SetSize;
    property Pen: TPen read FPen write SetPen;
    property ShowMarks: boolean read FShowMarks write SetShowMarks default true;
    property ShowValues: boolean read FShowValues write SetShowValues default true;
    property Spacing: integer read FSpacing write SetSpacing;
    property Step: Word read FStep write SetStep;
    property Style: TGraduationStyle read FStyle write SetStyle default gsMarks;
    property ValueFormat: String read FValueFormat write SetValueFormat;
  end;                                

  TGraduationClass = class of tGraduation;

  TGraduations = Class(TCollection)
  private
    FControl: TControl;
    function GetGraduation(Index: Integer): TGraduation;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); Override;
  public
    constructor Create(aControl: TControl; GraduationClass: TGraduationClass);
    function Add: TGraduation;
    property Items[Index: Integer]: TGraduation read GetGraduation; default;
  end;


  
  TcyBaseMeasure = class(TGraphicControl)
  private
    FColor: TColor;
    FOnAfterPaint: TNotifyEvent;
    FOnBeforePaint: TNotifyEvent;
    FItemsWidth: integer;
    FItemsHeight: integer;
    FItemsSpacing: integer;
    FItemsCount: integer;
    FMarginTop: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginBottom: Integer;
    FMax: Double;
    FMin: Double;
    FReadOnly: Boolean;
    FTransparent: Boolean;
    FSmooth: boolean;
    FTracking: boolean;
    procedure SetColor(const Value: TColor);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetItemsWidth(const Value: integer);
    procedure SetItemsSpacing(const Value: integer);
    procedure SetItemsCount(const Value: integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetSmooth(const Value: boolean);
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetmarginBottom(const Value: Integer);
    procedure SetmarginLeft(const Value: Integer);
    procedure SetmarginRight(const Value: Integer);
    procedure SetmarginTop(const Value: Integer);
    procedure SetItemsHeight(const Value: integer);
  protected
    procedure Paint; override;
    procedure Draw(Rect: TRect; FullRepaint: Boolean); virtual;
    procedure MeasureBoundsChanged; virtual;
    property Color: TColor read FColor write SetColor default clWindow;
    property ItemsCount: integer read FItemsCount write SetItemsCount default 0;
    property ItemsSpacing: integer read FItemsSpacing write SetItemsSpacing default 1;
    property ItemsWidth: integer read FItemsWidth write SetItemsWidth default 10;
    property ItemsHeight: integer read FItemsHeight write SetItemsHeight default 25;
    property MarginLeft: Integer read FMarginLeft write SetmarginLeft default 0;
    property MarginTop: Integer read FMarginTop write SetmarginTop default 0;
    property MarginRight: Integer read FMarginRight write SetmarginRight default 0;
    property MarginBottom: Integer read FMarginBottom write SetmarginBottom default 0;
    property Max: Double read FMax write SetMax;
    property Min: Double read FMin write SetMin;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default true;
    property Smooth: boolean read FSmooth write SetSmooth default false;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property OnBeforePaint: TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShowHint;
  end;


implementation

{tBookmarkItem}
constructor tBookmarkItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPicture := TPicture.Create;
  FPen := TPen.Create;
  FPen.Color := clBlack;
  FPen.Width := 1;
  FPen.OnChange := ChangedProperty;
  FBrush := TBrush.Create;
  FBrush.Color := clWindow;
  FBrush.OnChange := ChangedProperty;
  FFont := TFont.Create;
  FFont.OnChange := ChangedProperty;
  FBrush.Color := clYellow;
  FHeight := 15;
  FSpacing := 10;
  FShowValue := true;
  FStyle := bsArrow;
  FVisible := true;
  FWidth := 30;
  FValue := TcyBaseMeasure(Collection.Owner).FMin;
end;

destructor tBookmarkItem.Destroy;
begin
  FPicture.Free;
  FPen.Free;
  FBrush.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure tBookmarkItem.ChangedProperty(Sender: TObject);
begin
  Changed(false);            // It will call TBookmarks.Update !
end;

procedure tBookmarkItem.SetBookmarkStyle(const Value: TBookmarkStyle);
begin
  FStyle := Value;
  if FVisible
  then Changed(false);         
end;

procedure tBookmarkItem.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetPen(const Value: TPen);
begin
  FPen := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetPicture(const Value: TPicture);
begin
  FPicture := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed(false);
end;

procedure tBookmarkItem.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetBrush(const Value: TBrush);
begin
  FBrush := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

procedure tBookmarkItem.SetShowValue(const Value: boolean);
begin
  FShowValue := Value;
end;

procedure tBookmarkItem.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetValueFormat(const Value: String);
begin
  FValueFormat := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetValueSpacing(const Value: integer);
begin
  FValueSpacing := Value;
  if FVisible
  then Changed(false);
end;

procedure tBookmarkItem.SetValue(const Value: Double);
begin
  FValue := Value;
  if FVisible
  then Changed(false);
end;

{tBookmarks}
constructor tBookmarks.Create(aControl: TControl; BookmarkItemClass: TBookmarkItemClass);
begin
  inherited Create(BookmarkItemClass);
  FControl := aControl;
end;

function tBookmarks.GetBookmarkItem(Index: Integer): TBookmarkItem;
begin
  Result := TBookmarkItem(inherited Items[Index]);
end;

function tBookmarks.GetOwner: TPersistent;
begin
  Result := FControl;
end;

procedure tBookmarks.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnDeleting)  // Not called in Design-time ...
  then Update(Item);

  Inherited;
end;

// Event Called by setting properties/events of tLevel :
procedure tBookmarks.Update(Item: TCollectionItem);
begin
  Inherited;
  FControl.Invalidate;
end;

function tBookmarks.Add: TBookmarkItem;
begin
  Result := TBookmarkItem(inherited Add);
  Result.Changed(false);
end;

{ tLevel }
constructor tLevel.Create(Collection: TCollection);
begin
  FFromColor := clYellow;
  FToColor := clRed;
  FVisible := true;
  FFromValue := TcyBaseMeasure(Collection.Owner).FMin;
  FToValue := TcyBaseMeasure(Collection.Owner).FMax;
end;

procedure tLevel.SetFromColor(const Value: TColor);
begin
  FFromColor := Value;
  if FVisible
  then Changed(false);            // It will call TLevels.Update !
end;

procedure tLevel.SetFromValue(const Value: Double);
begin
  FFromValue := Value;
  if FVisible
  then Changed(false);
end;

procedure tLevel.SetToColor(const Value: TColor);
begin
  FToColor := Value;
  if FVisible
  then Changed(false);
end;

procedure tLevel.SetToValue(const Value: Double);
begin
  FToValue := Value;
  if FVisible
  then Changed(false);
end;

procedure tLevel.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed(false);
end;

{tLevels}
constructor tLevels.Create(aControl: TControl; LevelClass: TLevelClass);
begin
  inherited Create(LevelClass);
  FControl := aControl;
end;

function tLevels.GetLevel(Index: Integer): TLevel;
begin
  Result := TLevel(inherited Items[Index]);
end;

function tLevels.GetOwner: TPersistent;
begin
  Result := FControl;
end;

procedure tLevels.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnDeleting)  // Not called in Design-time ...
  then Update(Item);

  Inherited;
end;

// Event Called by setting properties/events of tLevel :
procedure tLevels.Update(Item: TCollectionItem);
begin
  Inherited;
  FControl.Invalidate;
end;

function tLevels.Add: TLevel;
begin
  Result := TLevel(inherited Add);
  Result.Changed(false);
end;


{tGraduation}
constructor tGraduation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPen := TPen.Create;
  FPen.Color := clBlack;
  FPen.Width := 1;
  FPen.OnChange := ChangedProperty;
  FFont := TFont.Create;
  FFont.OnChange := ChangedProperty;
  FShowMarks := true;
  FShowValues := true;
  FSize := 10;
  FStep := 10;
  FSpacing := 5;
  FStyle := gsMarks;
  FValueFormat := '';
end;

destructor tGraduation.Destroy;
begin
  FPen.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure tGraduation.ChangedProperty(Sender: TObject);
begin
  Changed(false);            // It will call TGraduations.Update !

  if Sender = FFont
  then begin
    if FShowValues
    then Changed(false);
  end
  else begin
    if FShowMarks
    then Changed(false);
  end;
end;

procedure tGraduation.SetFont(const Value: TFont);
begin
  FFont := Value;
  if FShowValues
  then Changed(false);
end;

procedure tGraduation.SetSize(const Value: Word);
begin
  FSize := Value;
  if FShowmarks
  then Changed(false);
end;

procedure tGraduation.SetPen(const Value: TPen);
begin
  FPen := Value;
  if FShowMarks
  then Changed(false);
end;

procedure tGraduation.SetShowMarks(const Value: boolean);
begin
  FShowMarks := Value;
  Changed(false);
end;

procedure tGraduation.SetShowValues(const Value: boolean);
begin
  FShowValues := Value;
  Changed(false);
end;

procedure tGraduation.SetSpacing(const Value: integer);
begin
  FSpacing := Value;
  if FShowValues or FShowMarks
  then Changed(false);
end;

procedure tGraduation.SetStep(const Value: Word);
begin
  FStep := Value;
  if FShowValues or FShowMarks
  then Changed(false);
end;

procedure tGraduation.SetStyle(const Value: TGraduationStyle);
begin
  FStyle := Value;
  Changed(false);
end;

procedure tGraduation.SetValueFormat(const Value: String);
begin
  FValueFormat := Value;
  if FShowValues
  then Changed(false);
end;

{tGraduations}
constructor tGraduations.Create(aControl: TControl; GraduationClass: TGraduationClass);
begin
  inherited Create(GraduationClass);
  FControl := aControl;
end;

function tGraduations.GetGraduation(Index: Integer): TGraduation;
begin
  Result := TGraduation(inherited Items[Index]);
end;

function tGraduations.GetOwner: TPersistent;
begin
  Result := FControl;
end;

procedure tGraduations.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnDeleting)  // Not called in Design-time ...
  then Update(Item);

  Inherited;
end;

// Event Called by setting properties/events of tGraduation :
procedure tGraduations.Update(Item: TCollectionItem);
begin
  Inherited;
  FControl.Invalidate;
end;

function tGraduations.Add: TGraduation;
begin
  Result := TGraduation(inherited Add);
  Result.Changed(false);
end;

{ TcyBaseMeasure }
constructor TcyBaseMeasure.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clWindow;
  FMarginLeft := 0;
  FMarginTop := 0;
  FMarginRight := 0;
  FMarginBottom := 0;
  FSmooth := false;
  FItemsCount := 10;
  FItemsSpacing := 1;
  FItemsWidth := 10;
  FItemsHeight := 25;
  FMax := 100;
  FMin := 0;
  FReadOnly := true;
  FTransparent := false;
  FTracking := false;
end;

destructor TcyBaseMeasure.Destroy;
begin
  inherited Destroy;
end;

procedure TcyBaseMeasure.SetmarginBottom(const Value: Integer);
begin
  if FMarginBottom <> Value
  then begin
    FMarginBottom := Value;
    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetmarginLeft(const Value: Integer);
begin
  if FMarginLeft <> Value
  then begin
    FMarginLeft := Value;
    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetmarginRight(const Value: Integer);
begin
  if FMarginRight <> Value
  then begin
    FMarginRight := Value;
    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetmarginTop(const Value: Integer);
begin
  if FMarginTop <> Value
  then begin
    FMarginTop := Value;
    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetColor(const Value: TColor);
begin
  if Value <> FColor
  then begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TcyBaseMeasure.SetItemsCount(const Value: integer);
begin
  if (FItemsCount <> Value) and (Value > 0)
  then begin
    FItemsCount := Value;
    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetItemsSpacing(const Value: integer);
begin
  if Value <> FItemsSpacing
  then
    if FItemsWidth + Value > 0  // FItemsSpacing can be negative ...
    then begin
      FItemsSpacing := Value;
      MeasureBoundsChanged;
    end;
end;

procedure TcyBaseMeasure.SetItemsWidth(const Value: integer);
begin
  if (Value <> FItemsWidth) and (Value > 0)
  then begin
    FItemsWidth := Value;
    if FItemsWidth + FItemsSpacing <= 0  // FItemsSpacing can be negative ...
    then ItemsSpacing := 0;

    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetItemsHeight(const Value: integer);
begin
  if (Value <> FItemsHeight) and (Value > 0)
  then begin
    FItemsHeight := Value;
    MeasureBoundsChanged;
  end;
end;

procedure TcyBaseMeasure.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value
  then begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TcyBaseMeasure.SetSmooth(const Value: boolean);
begin
  if FSmooth <> Value
  then begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TcyBaseMeasure.SetMax(const Value: Double);
begin
  if Value <> FMax
  then begin
    if Value < FMin
    then FMin := Value;

    FMax := Value;
    Invalidate;
  end;
end;

procedure TcyBaseMeasure.SetMin(const Value: Double);
begin
  if Value <> FMin
  then begin
    if Value > FMax
    then FMax := Value;

    FMin := Value;
    Invalidate;
  end;
end;

procedure TcyBaseMeasure.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TcyBaseMeasure.Paint;
var Rect: TRect;
begin
  if Assigned(FOnBeforePaint) then FOnBeforePaint(Self);
  Rect := ClientRect;
  Draw(Rect, true);
  if Assigned(FOnAfterPaint) then FOnAfterPaint(Self);
end;

procedure TcyBaseMeasure.Draw(Rect: TRect; FullRepaint: Boolean);
begin
  //
end;

procedure TcyBaseMeasure.MeasureBoundsChanged;
begin
  //
end;

end.
