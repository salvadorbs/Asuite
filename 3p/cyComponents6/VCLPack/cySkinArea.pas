{   Component(s):
    tcySkinArea

    Description:
    Allow to skin an area.

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

unit cySkinArea;     

interface

uses Classes, Windows, Types, Controls, ExtCtrls, Graphics, SysUtils, Messages, VCL.cyGraphics;

type
  TRGBQuadArray = Array[0..0] of TRGBQuad;
  pRGBQuadArray = ^TRGBQuadArray;
  TcySkinArea = class;
  TAreaState = (asNone, asAny, asDisabled, asNormal, asMouseOver, asMouseDown, asDown, asDownMouseOver, asDownMouseDown);

  tcyArea = class(TCollectionItem)      // Item of tcyAreas ...
  private
    FColor: TColor;
    FRGBValue: Integer;
    FDescription: String;
    FDown: Boolean;
    FEnabled: Boolean;
    FMouseOver: Boolean;
    FRowBegin: Integer;                 // Indice of the first row containing FColor - internal use for best performance ...
    FRowEnd: Integer;                   // Indice of the last row containing FColor - internal use for best performance ...
    FState: TAreaState;
    FDrawAreaState: TAreaState;
    FTag: Integer;
    FMouseDown: Boolean;
    FGroupIndex: Integer;
    FAllowAllUp: Boolean;
    procedure UpdateState;
    procedure SetMouseOver(const Value: Boolean);
    procedure SetMouseDown(const Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetAllowAllUp(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetColor(Value: TColor);
    procedure SetDescription(Value: String);
    procedure SetDown(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure UpdateExclusive;
  public
    constructor Create(Collection: TCollection); override;
    property MouseOver: Boolean read FMouseOver write SetMouseOver default false;
    property MouseDown: Boolean read FMouseDown write SetMouseDown default false;
    property DrawAreaState: TAreaState read FDrawAreaState;
    property RGBValue: Integer read FRGBValue;
    property RowBegin: Integer read FRowBegin;
    property RowEnd: Integer read FRowEnd;
    property State: TAreaState read FState default asNormal;
  published
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
    property Color: TColor read FColor write SetColor;
    property Description: String read FDescription write SetDescription;
    property Down: Boolean read FDown write SetDown default false;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Tag: Integer read FTag write FTag default 0;
  end;



  TcyAreaClass = class of TcyArea;

  tcyAreas = Class(TCollection)         // Collection of item tcyArea ...
  private
    FOwner: TcySkinArea;
    function GetArea(Index: Integer): TcyArea;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); Override;
  public
    constructor Create(acySkinArea: TcySkinArea; AreaClass: TcyAreaClass);
    function Add: TcyArea;
    property Items[Index: Integer]: TcyArea read GetArea; default;
    function FindItem(aRGBValue: Integer): Integer;
  end;



  TClickAreaEvent = procedure(Sender: TObject; Item: tcyArea) of object;

  tcySkinArea = class(TImage)
  private
    FPicNormal: TPicture;        // Area = Normal view. Can be empty ...
    FPicMouseOver: TPicture;     // Area = Mouse over. Can be empty ...
    FPicDownMouseOver: TPicture; // Area = Mouse over + down. Can be empty ...
    FPicDown: TPicture;          // Area = down. Can be empty ...
    FPicDisabled: TPicture;      // Area = Disabled. Can be empty ...
    FBmpArea: TBitmap;           // Bitmap with colors defined in FAreas in order to indentify areas. Can' t be empty !!!
    FAreas: TcyAreas;            // Areas definitions ...
    FAreaIndex: Integer;         // Area under mouse position ...
    FAreaMouseDown: Integer;     // Area clicked and not released...
    FOnClickArea: TClickAreaEvent;
    FPicMouseDown: TPicture;
    FPicDownMouseDown: TPicture;
    procedure SetPicNormal(const Value: TPicture);
    procedure SetPicMouseOver(const Value: TPicture);
    procedure SetPicDownMouseOver(const Value: TPicture);
    procedure SetPicDown(const Value: TPicture);
    procedure SetPicDisabled(const Value: TPicture);
    procedure SetBmpArea(const Value: TBitmap);
    procedure SetAreas(Value: TcyAreas);
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CreateDefaultAreas;
    procedure SetPicMouseDown(const Value: TPicture);
    procedure SetPicDownMouseDown(const Value: TPicture);
  protected
    procedure Loaded; override;
    procedure Click;  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure BmpAreaChanged(Sender: TObject);
    procedure PictureDisabledChanged(Sender: TObject);
    procedure PictureNormalChanged(Sender: TObject);
    procedure PictureMouseOverChanged(Sender: TObject);
    procedure PictureMouseDownChanged(Sender: TObject);
    procedure PictureDownChanged(Sender: TObject);
    procedure PictureDownMouseOverChanged(Sender: TObject);
    procedure PictureDownMouseDownChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AreaIndex: Integer read FAreaIndex;
    function GetAreaIndexByPos(x, y: Integer): Integer;
    function GetDrawAreaStateFromState(aState: TAreaState): TAreaState;
    procedure UpdateArea(Item: TCollectionItem; withState: TAreaState);
    procedure DrawArea(aItem: tcyArea; aPicture: TPicture);
    function GetBitmapArea(aBitmap: TBitmap; var aLocalisationArea: TRect;
                            aItem: tcyArea; aState: TAreaState; BckgrndColor: TColor): Boolean;
  published
    property PictureDisabled: TPicture read FPicDisabled write SetPicDisabled;
    property PictureNormal: TPicture read FPicNormal write SetPicNormal;
    property PictureMouseOver: TPicture read FPicMouseOver write SetPicMouseOver;
    property PictureMouseDown: TPicture read FPicMouseDown write SetPicMouseDown;
    property PictureDown: TPicture read FPicDown write SetPicDown;
    property PictureDownMouseOver: TPicture read FPicDownMouseOver write SetPicDownMouseOver;
    property PictureDownMouseDown: TPicture read FPicDownMouseDown write SetPicDownMouseDown;
    property Area: TBitmap read FBmpArea write SetBmpArea;
    property Areas: TcyAreas read FAreas write SetAreas;
    property OnClickArea: TClickAreaEvent read FOnClickArea write FOnClickArea;
  end;

const
  pixelFormatRendering: TPixelFormat = pf32bit;

implementation

{tcyArea}
constructor tcyArea.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAllowAllUp := false;
  FDown := false;
  FDrawAreaState := asAny;
  FEnabled := true;
  FMouseDown := false;
  FMouseOver := false;
  FDescription := '';
  FGroupIndex := 0;
  FTag := 0;
  FState := asNormal;
end;

procedure tcyArea.UpdateState;
begin
  if FEnabled
  then begin
    if FDown
    then begin
      if FMouseDown
      then
        FState := asDownMouseDown
      else
        if FMouseOver
        then FState := asDownMouseOver
        else FState := asDown;
    end
    else begin
      if FMouseDown
      then
        FState := asMouseDown
      else
        if FMouseOver
        then FState := asMouseOver
        else FState := asNormal;
    end;
  end
  else
    FState := asDisabled;
end;

procedure tcyArea.UpdateExclusive;
var
  i: Integer;
  item: TcyArea;
  AllUp: Boolean;
begin
  AllUp := not FDown;

  for i := 0 to Collection.Count -1 do
    if i <> Index
    then begin
      item := TcyArea(Collection.Items[i]);

      if item.FGroupIndex = FGroupIndex
      then begin
        if item.FAllowAllUp <> FAllowAllUp
        then item.FAllowAllUp := FAllowAllUp;

        if item.FDown
        then begin
          AllUp := false;

          if FDown
          then item.Down := false;
        end;
      end;
    end;

  if (not FAllowAllUp) and (AllUp)
  then Down := true;
end;

function tcyArea.GetDisplayName: string;
begin
  Result := 'tcyArea ' + FDescription;
end;

procedure tcyArea.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp = Value then Exit;

  FAllowAllUp := Value;
  UpdateExclusive;
end;

procedure tcyArea.SetColor(Value: TColor);
begin
  FColor := Value;
  FRGBvalue := ColorToRGB(FColor);
  FRowBegin := -1;
  FRowEnd   := -1;
  Changed(false);  // It will call TcyAreas.Update !
end;

procedure tcyArea.SetDescription(Value: String);
begin
  FDescription := Value;
end;

procedure tcyArea.SetDown(Value: Boolean);
begin
  if FDown <> Value
  then begin
    FDown := Value;
    UpdateExclusive;
    UpdateState;
    Changed(false);  // It will call TcyAreas.Update !
  end;
end;

procedure tcyArea.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled
  then begin
    FEnabled := Value;
    UpdateState;
    Changed(false);  // It will call TcyAreas.Update !
  end;
end;

procedure tcyArea.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateExclusive;
end;

procedure tcyArea.SetMouseDown(const Value: Boolean);
begin
  if Value <> FMouseDown
  then begin
    FMouseDown := Value;
    UpdateState;
    Changed(false);  // It will call TcyAreas.Update !
  end;
end;

procedure tcyArea.SetMouseOver(const Value: Boolean);
begin
  if Value <> FMouseOver
  then begin
    FMouseOver := Value;
    UpdateState;
    Changed(false);  // It will call TcyAreas.Update !
  end;
end;




{tcyAreas}
constructor tcyAreas.Create(acySkinArea: TcySkinArea; AreaClass: TcyAreaClass);
begin
  inherited Create(AreaClass);
  FOwner := acySkinArea;
end;

function tcyAreas.GetArea(Index: Integer): TcyArea;
begin
  Result := TcyArea(inherited Items[Index]);
end;

function tcyAreas.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure tcyAreas.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnDeleting)  // Not called in Design-time ...
  then begin
    tcyArea(Item).FMouseOver := false;
    tcyArea(Item).FDown := false;
    tcyArea(Item).FEnabled := true;
    tcyArea(Item).UpdateState;          // Actualize State property ...
    Update(Item);                       // Paint as normal ...
  end;

  Inherited;
end;

// Called by setting properties tcyArea.Color/tcyArea.Down/tcyArea.Enabled/tcyArea.MouseOver/tcyArea.MousDown
procedure tcyAreas.Update(Item: TCollectionItem);
begin
  Inherited;

  if Item <> Nil
  then FOwner.UpdateArea(Item, tcyArea(Item).State);   // Update Item with state ...
end;

function tcyAreas.FindItem(aRGBValue: Integer): Integer;
var i: integer;
begin
  RESULT := -1;
  i := 0;
  while (RESULT = -1) and (i <= Count-1) do
    if aRGBValue = Items[i].FRGBValue
    then RESULT := i
    else inc(i, 1);
end;

function tcyAreas.Add: TcyArea;
begin
  Result := TcyArea(inherited Add);
end;




{tcySkinArea}
constructor tcySkinArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicDisabled := TPicture.Create;
  FPicDisabled.OnChange := PictureDisabledChanged;
  FPicNormal := TPicture.Create;
  FPicNormal.OnChange := PictureNormalChanged;
  FPicMouseOver := TPicture.Create;
  FPicMouseOver.OnChange := PictureMouseOverChanged;
  FPicMouseDown := TPicture.Create;
  FPicMouseDown.OnChange := PictureMouseDownChanged;
  FPicDown := TPicture.Create;
  FPicDown.OnChange := PictureDownChanged;
  FPicDownMouseOver := TPicture.Create;
  FPicDownMouseOver.OnChange := PictureDownMouseOverChanged;
  FPicDownMouseDown := TPicture.Create;
  FPicDownMouseDown.OnChange := PictureDownMouseDownChanged;
  FBmpArea := TBitmap.Create;
  FBmpArea.OnChange := BmpAreaChanged;
  FAreas := TcyAreas.Create(self, TcyArea);
end;

destructor tcySkinArea.Destroy;
begin
  FPicDisabled.Free;
  FPicNormal.Free;
  FPicMouseOver.Free;
  FPicMouseDown.Free;
  FPicDown.Free;
  FPicDownMouseOver.Free;
  FPicDownMouseDown.Free;
  FBmpArea.Free;
  FAreas.Free;
  FAreas := Nil;

  inherited Destroy;
end;

procedure tcySkinArea.Loaded;
begin
  Inherited;
  FAreaIndex := -1;
  FAreaMouseDown := -1;
end;

procedure tcySkinArea.SetBmpArea(const Value: TBitmap);
begin
  try
    FBmpArea.Assign(Value);
  finally

  end;
end;

// Will be called on loading BmpArea property!
procedure tcySkinArea.BmpAreaChanged(Sender: TObject);
var i: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  // Set picture size and format, will Autosize component if Autosize = true :
  if not FBmpArea.Empty
  then begin
    if FBmpArea.PixelFormat <> pixelFormatRendering then
      FBmpArea.PixelFormat := pixelFormatRendering;

    Picture.Bitmap.Width  := FBmpArea.Width;
    Picture.Bitmap.Height := FBmpArea.Height;
    Picture.Bitmap.PixelFormat := pixelFormatRendering;
    // Picture.Bitmap.Canvas.Draw(0, 0, FBmpArea);
  end;

  if FAreas.Count <> 0
  then begin
    for i := 0 to FAreas.Count - 1 do
    begin
      FAreas.Items[i].FRowBegin := -1;
      FAreas.Items[i].FRowEnd := -1;
    end;
  end
  else
    CreateDefaultAreas;

  UpdateArea(Nil, asAny);
  Invalidate;
end;

procedure tcySkinArea.SetPicDisabled(const Value: TPicture);
begin
  FPicDisabled.Assign(Value);
end;

procedure tcySkinArea.PictureDisabledChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asDisabled);
end;

procedure tcySkinArea.SetPicNormal(const Value: TPicture);
begin
  FPicNormal.Assign(Value);
end;

procedure tcySkinArea.PictureNormalChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asAny);  // asAny because when there is no image for a specified state, we use FPicNormal image ...
end;

procedure tcySkinArea.SetPicMouseDown(const Value: TPicture);
begin
  FPicMouseDown.Assign(Value);
end;

procedure tcySkinArea.PictureMouseDownChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asMouseDown);
end;

procedure tcySkinArea.SetPicMouseOver(const Value: TPicture);
begin
  FPicMouseOver.Assign(Value);
end;

procedure tcySkinArea.PictureMouseOverChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asMouseOver);
end;

procedure tcySkinArea.SetPicDown(const Value: TPicture);
begin
  FPicDown.Assign(Value);
end;

procedure tcySkinArea.PictureDownChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asDown);
end;

procedure tcySkinArea.SetPicDownMouseDown(const Value: TPicture);
begin
  FPicDownMouseDown.Assign(Value);
end;

procedure tcySkinArea.PictureDownMouseDownChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asDownMouseDown);
end;

procedure tcySkinArea.SetPicDownMouseOver(const Value: TPicture);
begin
  FPicDownMouseOver.Assign(Value);
end;

procedure tcySkinArea.PictureDownMouseOverChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;

  UpdateArea(Nil, asDownMouseOver);
end;

function tcySkinArea.GetAreaIndexByPos(x, y: Integer): Integer;
var PicPix: TPoint;
begin
  if (not Picture.Bitmap.Empty) and (not FBmpArea.Empty)
  then begin
    PicPix.x := ((X - DestRect.Left) * Picture.Bitmap.Width) div (DestRect.Right-DestRect.Left);
    PicPix.y := ((Y - DestRect.Top) * Picture.Bitmap.Height) div (DestRect.Bottom-DestRect.Top);

    // Locate the area in the coordinates x = PicPix.x and y = PicPix.x :
    RESULT := Areas.FindItem(ColorToRGB(FBmpArea.Canvas.Pixels[PicPix.x, PicPix.y]));
  end
  else
    RESULT := -1;
end;

function tcySkinArea.GetDrawAreaStateFromState(aState: TAreaState): TAreaState;
begin
  case aState of
    asDisabled:      RESULT := asDisabled;
    asMouseOver:     RESULT := asMouseOver;
    asMouseDown:     RESULT := asMouseDown;
    asDown:          RESULT := asDown;
    asDownMouseOver: RESULT := asDownMouseOver;
    asDownMouseDown: RESULT := asDownMouseDown;
    else
                     RESULT := asNormal;
  end;

  // *** Area disabled *** //
  if RESULT = asDisabled
  then
    if not ValidGraphic(FPicDisabled.Graphic)
    then RESULT := asNormal;     // Draw as normal

  // *** Area Down *** //
  if RESULT = asDownMouseDown
  then
    if not ValidGraphic(FPicDownMouseDown.Graphic)
    then RESULT := asDown;       // Draw as down

  if RESULT = asDownMouseOver
  then
    if not ValidGraphic(FPicDownMouseOver.Graphic)
    then RESULT := asDown;       // Draw as down

  if RESULT = asDown
  then
    if not ValidGraphic(FPicDown.Graphic)
    then RESULT := asMouseDown;  // Draw as mouse down

  // *** Area NOT down *** //
  if RESULT = asMouseDown
  then
    if not ValidGraphic(FPicMouseDown.Graphic)
    then RESULT := asMouseOver;  // Draw as mouse over

  if RESULT = asMouseOver
  then
    if not ValidGraphic(FPicMouseOver.Graphic)
    then RESULT := asNormal;     // Draw as normal

  if RESULT = asNormal
  then
    if not ValidGraphic(FPicNormal.Graphic)
    then RESULT := asAny;       // Draw as bmp area

  if RESULT = asAny
  then
    if FBmpArea.Empty
    then RESULT := asNone;
end;

// Update Picture according item state ...
procedure tcySkinArea.UpdateArea(Item: TCollectionItem; withState: TAreaState);
var
  a: Integer;
  ForceUpdate: Boolean;

        procedure DoUpdate(aItem: TcyArea; aState: TAreaState);
        var aDrawAreaState: TAreaState;
        begin
          aDrawAreaState := GetDrawAreaStateFromState(aState);

          if (aItem.FDrawAreaState <> aDrawAreaState) or (ForceUpdate)
          then begin
            aItem.FDrawAreaState := aDrawAreaState;

            case aDrawAreaState of
              asDisabled      : DrawArea(aItem, FPicDisabled);
              asMouseOver     : DrawArea(aItem, FPicMouseOver);
              asMouseDown     : DrawArea(aItem, FPicMouseDown);
              asNormal        : DrawArea(aItem, FPicNormal);
              asDown          : DrawArea(aItem, FPicDown);
              asDownMouseOver : DrawArea(aItem, FPicDownMouseOver);
              asDownMouseDown : DrawArea(aItem, FPicDownMouseDown);
              asAny           : DrawArea(aItem, Nil);
              // asNone, do nothing ...
            end;
          end;
        end;

begin
  if Item = Nil
  then begin
    ForceUpdate := true;   // When a picture changes ...

    // We need to draw BmpArea or FPicNormal for non determined areas (color not present on Areas items) :
    if withState = asAny
    then begin
      if ValidGraphic(FPicNormal.Graphic)
      then begin
        FPicNormal.OnChange := Nil;
        Picture.Bitmap.Canvas.Draw(0, 0, FPicNormal.Graphic);
        FPicNormal.OnChange := PictureNormalChanged;
      end
      else
        if not FBmpArea.Empty
        then Picture.Bitmap.Canvas.Draw(0, 0, FBmpArea);
    end;

    for a := 0 to Areas.Count - 1 do
      if withState = asAny
      then
        DoUpdate(Areas.Items[a], Areas.Items[a].State)    // Update all areas ...
      else
        if Areas.Items[a].State = withState
        then DoUpdate(Areas.Items[a], withState);         // Update all areas with in the specified state ...
  end
  else begin
    ForceUpdate := false;
    DoUpdate(tcyArea(Item), withState);                   // Update specified area with a state ...
  end;
end;
                
procedure tcySkinArea.DrawArea(aItem: tcyArea; aPicture: TPicture);
var
  BmpCopyFrom: TBitmap;
  RowBytesBmpArea, RowBytesFrom, RowBytesTo: pRGBQuadArray;
  Start, MaxWidth, MaxHeight, x, y, RGBVal: Integer;
  Sav: TNotifyEvent;
begin
  BmpCopyFrom := TBitmap.Create;

  if aPicture <> Nil
  then begin
    BmpCopyFrom.Width  := aPicture.Graphic.Width;
    BmpCopyFrom.Height := aPicture.Graphic.Height;
    Sav := aPicture.OnChange;
    aPicture.OnChange := Nil;
    BmpCopyFrom.Canvas.Draw(0, 0, aPicture.Graphic);
    aPicture.OnChange := Sav;
  end
  else
    BmpCopyFrom.Assign(FBmpArea);

  // Change picture property !!!
  if (not BmpCopyFrom.Empty) and (not Picture.Bitmap.Empty)
  then begin
    if BmpCopyFrom.PixelFormat <> pixelFormatRendering
    then BmpCopyFrom.PixelFormat := pixelFormatRendering;

    // Normally, not needed:
    if Picture.Bitmap.PixelFormat <> pixelFormatRendering
    then Picture.Bitmap.PixelFormat := pixelFormatRendering;

    MaxWidth  := Picture.Bitmap.Width;
    if MaxWidth > BmpCopyFrom.Width
    then MaxWidth := BmpCopyFrom.Width;
    if MaxWidth > FBmpArea.Width
    then MaxWidth := FBmpArea.Width;

    MaxHeight := Picture.Bitmap.Height;
    if MaxHeight > BmpCopyFrom.Height
    then MaxHeight := BmpCopyFrom.Height;
    if MaxHeight > FBmpArea.Height
    then MaxHeight := FBmpArea.Height;
    if (aItem.FRowEnd <> -1) and (aItem.FRowEnd + 1 < MaxHeight)
    then MaxHeight := aItem.FRowEnd + 1;

    Start := 0;
    if aItem.FRowBegin <> -1
    then Start := aItem.FRowBegin;

    for y := Start to MaxHeight - 1 do
    begin
      RowBytesBmpArea := FBmpArea.ScanLine[y];
      RowBytesFrom    := BmpCopyFrom.ScanLine[y];
      RowBytesTo      := Picture.Bitmap.ScanLine[y];

      for x := 0 to MaxWidth - 1 do
      begin
        RGBVal := RGB(RowBytesBmpArea[x].rgbRed, RowBytesBmpArea[x].rgbGreen, RowBytesBmpArea[x].rgbBlue);

        if RGBVal = aItem.RGBValue     // Same area !!!
        then begin
          RowBytesTo[x] := RowBytesFrom[x];
          if aItem.FRowBegin = -1 then aItem.FRowBegin := y; // first row of area localisation ...
          if aItem.FRowEnd < y then aItem.FRowEnd := y; // last row of area localisation ...
        end;
      end;
    end;

    Picture.Bitmap.Modified := True;
  end;

  BmpCopyFrom.Free;
end;

procedure tcySkinArea.Click;
begin
  if FAreaIndex <> -1
  then
    if FAreas.Items[FAreaIndex].Enabled
    then begin
      if FAreas.Items[FAreaIndex].GroupIndex <> 0
      then FAreas.Items[FAreaIndex].Down := not FAreas.Items[FAreaIndex].FDown;

      if Assigned(FOnClickArea)
      then FOnClickArea(self, FAreas.Items[FAreaIndex]);
    end;

  Inherited;
end;

procedure tcySkinArea.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FAreaMouseDown := GetAreaIndexByPos(x, y);
  if FAreaMouseDown <> -1
  then Areas[FAreaMouseDown].MouseDown := true;

  inherited;
end;

procedure tcySkinArea.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // Set FMouseDown to false :
  if FAreaMouseDown <> -1
  then Areas[FAreaMouseDown].MouseDown := false;

  inherited;
end;

procedure tcySkinArea.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, AreaFocused : Integer;
begin
  AreaFocused := GetAreaIndexByPos(x, y);

  // Area focused changed ?
  if AreaFocused <> FAreaIndex
  then begin
    // Release focus area :
    for i := 0 to Areas.Count - 1 do
      if (i <> AreaFocused) and (Areas[i].FMouseOver)
      then Areas[i].MouseOver := false;

    // Area under mouse :
    FAreaIndex := AreaFocused;

    if FAreaIndex <> -1
    then Areas[FAreaIndex].MouseOver := true;
  end;

  inherited;
end;

procedure tcySkinArea.CmMouseLeave(var Msg: TMessage);
var i: Integer;
begin
  for i := 0 to Areas.Count - 1 do
    if Areas[i].FMouseOver
    then Areas[i].MouseOver := false;

  FAreaIndex := -1;
  inherited;
end;

procedure tcySkinArea.SetAreas(Value: TcyAreas);
begin
  Areas.Assign(Value);
end;

procedure tcySkinArea.CreateDefaultAreas;
var
  RowBytes: pRGBQuadArray;
  x, y, OldRGBVal, RGBVal: integer;
begin
  // Delete current area definitions:
  while FAreas.Count <> 0 do
    FAreas.Delete(0);

  if not FBmpArea.Empty
  then begin
    // Scanning FBmpArea in order to create a list of colors:
    for y := 0 to FBmpArea.Height - 1 do
    begin
      OldRGBVal := -1;
      RowBytes  := FBmpArea.ScanLine[y];

      for x := 0 to FBmpArea.Width - 1 do
      begin
        RGBVal := RGB(RowBytes[x].rgbRed, RowBytes[x].rgbGreen, RowBytes[x].rgbBlue);

        if (OldRGBVal <> RGBVal)  // Prevent making FindItem a lot of time !!!
        then begin
          if (Self.Areas.FindItem(RGBVal)) = -1
          then
            with Areas.Add do
            begin
              Color := FBmpArea.Canvas.Pixels[x, y];  // Save Color ...
              FRowBegin := y;
            end;

          OldRGBVal := RGBVal;
        end;
      end;
    end;
  end;
end;

function tcySkinArea.GetBitmapArea(aBitmap: TBitmap; var aLocalisationArea: TRect;
                          aItem: tcyArea; aState: TAreaState; BckgrndColor: TColor): Boolean;
var
  aDrawAreaState: TAreaState;
  BmpCopyFrom: TBitmap;
  RowBytesBmpArea, RowBytesFrom, RowBytesTo: pRGBQuadArray;
  Start, MaxWidth, MaxHeight, x, y, RGBVal: Integer;

         procedure PrepareBitmap(aPicture: TPicture);
         var Sav: TNotifyEvent;
         begin
           BmpCopyFrom.Width  := aPicture.Graphic.Width;
           BmpCopyFrom.Height := aPicture.Graphic.Height;
           Sav := aPicture.OnChange;
           aPicture.OnChange := Nil;
           BmpCopyFrom.Canvas.Draw(0, 0, aPicture.Graphic);
           aPicture.OnChange := Sav;
           RESULT := true;
         end;

begin
  RESULT := false;
  if aItem = Nil then EXIT;

  aDrawAreaState := GetDrawAreaStateFromState(aState);
  if aDrawAreaState = asNone then EXIT;

  try
    BmpCopyFrom := TBitmap.Create;

    case aDrawAreaState of
      asNormal:        PrepareBitmap(FPicNormal);
      asDisabled:      PrepareBitmap(FPicDisabled);
      asMouseOver:     PrepareBitmap(FPicMouseOver);
      asMouseDown:     PrepareBitmap(FPicMouseDown);
      asDown:          PrepareBitmap(FPicDown);
      asDownMouseOver: PrepareBitmap(FPicDownMouseOver);
      asDownMouseDown: PrepareBitmap(FPicDownMouseDown);
      else begin
         BmpCopyFrom.Assign(FBmpArea);
         RESULT := true;
       end;
    end;

    if BmpCopyFrom.PixelFormat <> pixelFormatRendering
    then BmpCopyFrom.PixelFormat := pixelFormatRendering;

    aLocalisationArea := classes.Rect(-1, -1, -1, -1);
    aBitmap.Width  := BmpCopyFrom.Width;
    aBitmap.Height := BmpCopyFrom.Height;
    if aBitmap.PixelFormat <> pixelFormatRendering
    then aBitmap.PixelFormat := pixelFormatRendering;

    if BckgrndColor <> clNone
    then begin
      aBitmap.Canvas.Brush.Color := BckgrndColor;
      aBitmap.Canvas.FillRect(classes.Rect(0, 0, aBitmap.Width, aBitmap.Height));
    end;

    MaxWidth  := aBitmap.Width;
    if MaxWidth > BmpCopyFrom.Width
    then MaxWidth := BmpCopyFrom.Width;
    if MaxWidth > FBmpArea.Width
    then MaxWidth := FBmpArea.Width;

    MaxHeight := aBitmap.Height;
    if MaxHeight > BmpCopyFrom.Height
    then MaxHeight := BmpCopyFrom.Height;
    if MaxHeight > FBmpArea.Height
    then MaxHeight := FBmpArea.Height;
    if (aItem.FRowEnd <> -1) and (aItem.FRowEnd + 1 < MaxHeight)
    then MaxHeight := aItem.FRowEnd + 1;

    Start := 0;
    if aItem.FRowBegin <> -1
    then Start := aItem.FRowBegin;

    for y := Start to MaxHeight - 1 do
    begin
      RowBytesBmpArea := FBmpArea.ScanLine[y];
      RowBytesFrom    := BmpCopyFrom.ScanLine[y];
      RowBytesTo      := aBitmap.ScanLine[y];

      for x := 0 to MaxWidth - 1 do
      begin
        RGBVal := RGB(RowBytesBmpArea[x].rgbRed, RowBytesBmpArea[x].rgbGreen, RowBytesBmpArea[x].rgbBlue);

        if RGBVal = aItem.RGBValue     // Same area !!!
        then begin
          RowBytesTo[x] := RowBytesFrom[x];
          if aItem.FRowBegin = -1 then aItem.FRowBegin := y; // first row of area localisation ...
          if aItem.FRowEnd < y then aItem.FRowEnd := y; // last row of area localisation ...

          if aLocalisationArea.Top = -1
          then aLocalisationArea.Top := y;
          aLocalisationArea.Bottom := y;
          if (aLocalisationArea.Left > x) or (aLocalisationArea.Left = -1)
          then aLocalisationArea.Left := x;
          if aLocalisationArea.Right < x
          then aLocalisationArea.Right := x;
        end;
      end;
    end;

    RESULT := true;
  finally
    BmpCopyFrom.Free;
  end;
end;

end.
