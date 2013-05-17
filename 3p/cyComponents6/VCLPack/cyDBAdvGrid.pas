{   Component(s):
    TcyCustomDBAdvGrid

    Description:
    A custom DBGrid with
    - revealed herited properties
    - OnSelectCell event
    - mouse wheel handling for records navigation, selecting rows (with Shift key) , do nothing or do as original
    - 2 clicks for massive rows selection (with Shift key)
    - Both scrollbars disabling option
    - CheckBox for each record
    - Auto stretch one column
    - Title custom appearence
    etc ...


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

unit cyDBAdvGrid;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Forms, Windows, Controls, Graphics, Math, Db, Grids, DBGrids, ImgList, stdCtrls, sysUtils,
       VCL.cyTypes, VCL.cyClasses, VCL.cyGraphics, cyBaseDBGrid, dialogs;

// AfterScroll and AfterScrollPause features :
{$IFDEF DELPHI2009_OR_ABOVE}
{$DEFINE SCROLL_FEATURE}
{$ENDIF}

type
  TFixedBgStyle = (fsFixedColor, fsGradient);
  TSortIndicatorsStyle = (siCaptionSide, siBackground);

  TAdvDrawTitleCell = procedure (Sender: TObject; ACol, ARow: Longint; ARect: TRect; Column: TColumn; var AState: TGridDrawState;
                                   var fromColor, toColor, frameColor: TColor; var DrawBackground: Boolean) of Object;

  TAdvDrawIndicatorCell = procedure (Sender: TObject; ACol, ARow: Longint; ARect: TRect; var AState: TGridDrawState;
                                      var fromColor, toColor, frameColor: TColor; var DrawBackground: Boolean) of Object;

  TAdvDrawColumnCell = procedure (Sender: TObject; ACol, ARow: Longint; ARect: TRect; Column: TColumn; var AState: TGridDrawState;
                                   Highlight: Boolean; var DrawBackground, DrawFieldValue: Boolean) of Object;

  TAdvSetFieldContentRendering = procedure (Sender: TObject; ACol, ARow: Longint; ARect: TRect; Column: TColumn; AState: TGridDrawState;
                                             Highlight: Boolean; var FieldContentRendering: TFieldContentRendering) of object;

  TAdvSortColumnUserDefine = procedure (Sender: TObject; Column: TColumn; var Ascendant, Accept: Boolean) of Object;

  TcyCustomDBAdvGrid = class;

  // Customize indicators with a TImageList :
  TcyIndicatorsOptions = class(TPersistent)
  private
    FGrid: TcyCustomDBAdvGrid;
    FCustomIndicators: TImageList;
    FCustomActiveIndex: Integer;
    FCustomEditIndex: Integer;
    FCustomInsertIndex: Integer;
    FCustomMultiActiveIndex: Integer;
    FCustomMultiIndex: Integer;
    FAlignment: TAlignment;
    procedure SetCustomIndicators(const Value: TImageList);
    procedure SetCustomActiveIndex(const Value: Integer);
    procedure SetCustomEditIndex(const Value: Integer);
    procedure SetCustomInsertIndex(const Value: Integer);
    procedure SetCustomMultiActiveIndex(const Value: Integer);
    procedure SetCustomMultiIndex(const Value: Integer);
    procedure SetAlignment(const Value: TAlignment);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property CustomActiveIndex: Integer read FCustomActiveIndex write SetCustomActiveIndex default 0;
    property CustomEditIndex: Integer read FCustomEditIndex write SetCustomEditIndex default 0;
    property CustomInsertIndex: Integer read FCustomInsertIndex write SetCustomInsertIndex default 0;
    property CustomMultiIndex: Integer read FCustomMultiIndex write SetCustomMultiIndex default 0;
    property CustomMultiActiveIndex: Integer read FCustomMultiActiveIndex write SetCustomMultiActiveIndex default 0;
    property CustomIndicators: TImageList read FCustomIndicators write SetCustomIndicators;
  end;

  // Sort indicators options :
  TcySortIndicatorsOptions = class(TPersistent)
  private
    FGrid: TcyCustomDBAdvGrid;
    FCustomSortIndicators: TImageList;
    FReadOnly: Boolean;
    FMultiSelect: Boolean;
    FMultiSelectKeys: TShiftState;
    FStyle: TSortIndicatorsStyle;
    procedure SetCustomSortIndicators(const Value: TImageList);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetMultiSelectKeys(const Value: TShiftState);
    procedure SetStyle(const Value: TSortIndicatorsStyle);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property CustomSortIndicators: TImageList read FCustomSortIndicators write SetCustomSortIndicators;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default true;
    property MultiSelectKeys: TShiftState read FMultiSelectKeys write SetMultiSelectKeys default [ssCtrl];
    property ReadOnly: Boolean read FReadOnly write FReadOnly default true;
    property Style: TSortIndicatorsStyle read FStyle write SetStyle default siBackground;
  end;

  // Sort collection item :
  TcySortColumn = class(TCollectionItem)
  private
    FFieldName: String;
    FAscendant: Boolean;
    FCustomIndicatorIndex: Integer;
    procedure SetAscendant(const Value: Boolean);
    procedure SetCustomIndicatorIndex(const Value: Integer);
    procedure SetFieldName(const Value: String);
  protected
    function GetDisplayName: string; override;
    function GetInternalSortIndicatorIndex: Integer;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Ascendant: Boolean read FAscendant write SetAscendant default True;
    property CustomIndicatorIndex: Integer read FCustomIndicatorIndex write SetCustomIndicatorIndex default 0;
    property FieldName: String read FFieldName write SetFieldName;
  end;

  TcySortClass = class of tcySortColumn;

  // Sort collection :
  tcySortColumns = Class(TCollection)
  private
    FGrid: TcyCustomDBAdvGrid;
    function GetSortColumn(Index: Integer): TcySortColumn;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aGrid: TcyCustomDBAdvGrid; SortClass: TcySortClass);
    function Add: TcySortColumn;
    procedure Delete(Index: Integer);
    function LocateField(aFieldName: String): TcySortColumn;
    procedure ClearExcluding(ItemIndex: Integer);
    function GetSQLCode: String;
    property Items[Index: Integer]: TcySortColumn read GetSortColumn; default;
  end;

  // Titles and indicators options:
  TcyFixedOptions = class(TPersistent)
  private
    FGrid: TcyCustomDBAdvGrid;
    FFlat: Boolean;
    FFrame: Boolean;
    FFrameColor: TColor;
    FStyle: TFixedBgStyle;
    FOnChange: TNotifyEvent;
    procedure SetFlat(const Value: Boolean);
    procedure SetFrame(const Value: Boolean);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFixedBgStyle(const Value: TFixedBgStyle);
  protected
    procedure PropertiesChanged;
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property Flat: Boolean read FFlat write SetFlat default false;
    property Frame: Boolean read FFrame write SetFrame default false;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBlack;
    property Style: TFixedBgStyle read FStyle write SetFixedBgStyle default fsGradient;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TcyCustomDBAdvGrid = class(TcyBaseDBGrid)
  private
    FInternalIndicators: TImageList;
    FInternalSortIndicators: TImageList;
    //
    FFixedColsColors: TcyGradient;
    FFixedRowsColors: TcyGradient;
    FOnBeforeDrawColumnCell: TAdvDrawColumnCell;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FForceScrollRepaint: Boolean;
    FWallpaper: TcyBgPicture;
    FOnBeforeDrawTitleCell: TAdvDrawTitleCell;
    FOnBeforeDrawIndicatorCell: TAdvDrawIndicatorCell;
    FIndicatorsOptions: TcyIndicatorsOptions;
    FFixedRowsOptions: TcyFixedOptions;
    FFixedColsOptions: TcyFixedOptions;
    FOnSetContentFieldRendering: TAdvSetFieldContentRendering;
    FTitleWordWrap: Boolean;
    FSortColumns: tcySortColumns;
    FSortIndicatorsOptions: TcySortIndicatorsOptions;
    FOnSortColumnUserDefine: TAdvSortColumnUserDefine;
    FOnSortColumnsUserChange: TNotifyEvent;
    procedure SetFixedColsColors(const Value: TcyGradient);
    procedure SetFixedRowsColors(const Value: TcyGradient);
    procedure SubPropertiesChanged(Sender: TObject);
    procedure FixedColsPropertiesChanged(Sender: TObject);
    procedure FixedRowsPropertiesChanged(Sender: TObject);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionFontColor(const Value: TColor);
    procedure SetWallpaper(const Value: TcyBgPicture);
    procedure SetIndicatorsOptions(const Value: TcyIndicatorsOptions);
    procedure SetFixedColsOptions(const Value: TcyFixedOptions);
    procedure SetFixedRowsOptions(const Value: TcyFixedOptions);
    procedure SetTitleWordWrap(const Value: Boolean);
    procedure SetSortColumns(const Value: tcySortColumns);
    procedure SetSortFIndicatorsOptions(const Value: TcySortIndicatorsOptions);
  protected
    procedure BeforePaint; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(Distance: Integer); override;
    procedure DoDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawTitleCell(ACol, ARow: Longint; ARect: TRect; Column: TColumn; AState: TGridDrawState);
    procedure DrawTitleCellBackGround(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState; fromColor, toColor, frameColor: TColor);
    procedure _DrawDataCell(ACol, ARow: Longint; ARect: TRect; Column: TColumn; AState: TGridDrawState);
    procedure DrawDataCellBackGround(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawIndicatorCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawIndicatorCellBackGround(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState; fromColor, toColor, frameColor: TColor);
    property IndicatorsOptions: TcyIndicatorsOptions read FIndicatorsOptions write SetIndicatorsOptions;
    property FixedColsColors: TcyGradient read FFixedColsColors write SetFixedColsColors;
    property FixedColsOptions: TcyFixedOptions read FFixedColsOptions write SetFixedColsOptions;
    property FixedRowsColors: TcyGradient read FFixedRowsColors write SetFixedRowsColors;
    property FixedRowsOptions: TcyFixedOptions read FFixedRowsOptions write SetFixedRowsOptions;
    property ForceScrollRepaint: Boolean read FForceScrollRepaint write FForceScrollRepaint default false;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clHighLight;
    property SelectionFontColor: TColor read FSelectionFontColor write SetSelectionFontColor default clHighLightText;
    property SortColumns: tcySortColumns read FSortColumns write SetSortColumns;
    property SortIndicatorsOptions: TcySortIndicatorsOptions read FSortIndicatorsOptions write SetSortFIndicatorsOptions;
    property TitleWordWrap: Boolean read FTitleWordWrap write SetTitleWordWrap default false;
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property OnBeforeDrawColumnCell: TAdvDrawColumnCell read FOnBeforeDrawColumnCell write FOnBeforeDrawColumnCell;
    property OnBeforeDrawIndicatorCell: TAdvDrawIndicatorCell read FOnBeforeDrawIndicatorCell write FOnBeforeDrawIndicatorCell;
    property OnBeforeDrawTitleCell: TAdvDrawTitleCell read FOnBeforeDrawTitleCell write FOnBeforeDrawTitleCell;
    property OnSetContentFieldRendering: TAdvSetFieldContentRendering read FOnSetContentFieldRendering write FOnSetContentFieldRendering;
    property OnSortColumnsUserChange: TNotifyEvent read FOnSortColumnsUserChange write FOnSortColumnsUserChange;
    property OnSortColumnUserDefine: TAdvSortColumnUserDefine read FOnSortColumnUserDefine write FOnSortColumnUserDefine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TcyDBAdvGrid = class(TcyCustomDBAdvGrid)
  private
  protected
  public
    // Herited from TDBGrid (Can not be published to avoid error on load project when a cyDBAdvGrid is hiding his indicator) :
    property Col;              // Current column
    property ColCount;         // Displayed columnss
    property LeftCol;          // First displayed column
    property InplaceEditor;
    // Display errors on change ... property FixedCols;        // number of fixed columns
    property MouseJob;
    property Row;              // Current row
    property RowCount;         // Displayed rows
    property RowHeights;       // Array with Height of each row
    property VisibleColCount;  // ReadOnly - Visible data columns (Not Fixed Columns)
    property VisibleRowCount;  // ReadOnly - Visible data rows (Not Fixed Rows)
    property TopRow;           // Index of the first data row
    property Selection;        // Selected cell coordinates ...
    // Herited from TcyBaseDBGrid :
    property CheckedList;
    property ColumnIndex;
    property SelectedVisibleIndex;
  published
    // Herited from TDBGrid :
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    // Herited from TcyBaseDBGrid :
    property AllowDeleteRecord;
    property AllowInsertRecord;
    property AllowAppendRecord;
    property CheckBoxes;
    property ClientColumn;
    property CustomLayoutOptions;
    property DefaultDrawingCheckBox;
    property dgColumnResizeMode;
    property EditorOptions;
    property MoveRows;
    property MoveRowsColor;
    property MoveRowRendering;
    property MouseWheelMode;
    property HotMode;
    property HotColor;
    property HotFontColor;
    property HorzScrollBar;
    property VertScrollBar;
    {$IFDEF SCROLL_FEATURE}
    property ScrollPauseMSeconds;
    {$ENDIF}
    property OnCheckBoxClick;
    property OnDrawCheckBox;
    property OnSelectCell;
    property OnBeforePaint;
    property OnAfterPaint;
    property OnBeforeDrawCell;
    property OnAfterDrawCell;
    {$IFDEF SCROLL_FEATURE}
    property OnColWidthsChanged;
    property OnAfterScroll;
    property OnAfterScrollPause;
    {$ENDIF}
    property OnBeginMoveRows;
    property OnMoveRows;
    property OnEndMoveRows;
    property OnResize;
    property OnScrollBarMouseDown;
    property OnScrollBarMouseUp;
    // Herited from TcyCustomDBAdvGrid :
    property IndicatorsOptions;
    property ContentCheckBoxOptions;
    property ContentImageOptions;
    property ContentMemoOptions;
    property ContentFieldsRender;
    property FixedColsColors;
    property FixedColsOptions;
    property FixedRowsColors;
    property FixedRowsOptions;
    property ForceScrollRepaint;
    property OnEditorAcceptKey;
    property OnEditorSetFieldText;
    property OnShowEditor;
    property SelectionColor;
    property SelectionFontColor;
    property SortColumns;
    property SortIndicatorsOptions;
    property TitleWordWrap;
    property Wallpaper;
    property OnBeforeDrawColumnCell;
    property OnBeforeDrawIndicatorCell;
    property OnBeforeDrawTitleCell;
    property OnSetContentFieldRendering;
    property OnSortColumnsUserChange;
    property OnSortColumnUserDefine;
  end;

implementation

const
  // Internal indicators :
  AdvbmArrow = 'ADVDBGARROW';
  AdvbmEdit = 'ADVDBEDIT';
  AdvbmInsert = 'ADVDBINSERT';
  AdvbmMultiDot = 'ADVDBMULTIDOT';
  AdvbmMultiArrow = 'ADVDBMULTIARROW';
  InternalIndicatorArrow = 0;
  InternalIndicatorEdit = 1;
  InternalIndicatorInsert = 2;
  InternalIndicatorMultiSelect = 3;
  InternalIndicatorMultiSelectCurrent = 4;

  FrameOffs = 2;  // Indent text ...

{$R cyCustomDBAdvGrid.res}
{$R cyGridSort.res}

{ TcyIndicatorsOptions }
constructor TcyIndicatorsOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyCustomDBAdvGrid(AOwner);
  FAlignment := taCenter;
  FCustomActiveIndex := 0;
  FCustomEditIndex := 0;
  FCustomInsertIndex := 0;
  FCustomMultiIndex := 0;
  FCustomMultiActiveIndex := 0;
end;

procedure TcyIndicatorsOptions.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  if dgIndicator in FGrid.Options
  then FGrid.InvalidateCol(0);
end;

procedure TcyIndicatorsOptions.SetCustomActiveIndex(const Value: Integer);
begin
  if FCustomActiveIndex <> Value
  then begin
    FCustomActiveIndex := Value;
    if Assigned(FCustomIndicators)
    then FGrid.InvalidateCol(0);
  end;
end;

procedure TcyIndicatorsOptions.SetCustomEditIndex(const Value: Integer);
begin
  if FCustomEditIndex <> Value
  then begin
    FCustomEditIndex := Value;
    if Assigned(FCustomIndicators)
    then FGrid.InvalidateCol(0);
  end;
end;

procedure TcyIndicatorsOptions.SetCustomInsertIndex(const Value: Integer);
begin
  if FCustomInsertIndex <> Value
  then begin
    FCustomInsertIndex := Value;
    if Assigned(FCustomIndicators)
    then FGrid.InvalidateCol(0);
  end;
end;

procedure TcyIndicatorsOptions.SetCustomMultiActiveIndex(const Value: Integer);
begin
  if FCustomMultiActiveIndex <> Value
  then begin
    FCustomMultiActiveIndex := Value;
    if Assigned(FCustomIndicators)
    then FGrid.InvalidateCol(0);
  end;
end;

procedure TcyIndicatorsOptions.SetCustomMultiIndex(const Value: Integer);
begin
  if FCustomMultiIndex <> Value
  then begin
    FCustomMultiIndex := Value;
    if Assigned(FCustomIndicators)
    then FGrid.InvalidateCol(0);
  end;
end;

procedure TcyIndicatorsOptions.SetCustomIndicators(const Value: TImageList);
begin
  FCustomIndicators := Value;
  FGrid.InvalidateCol(0);

  if Value <> nil
  then Value.FreeNotification(FGrid);
end;

{ TcySortIndicatorsOptions }
constructor TcySortIndicatorsOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyCustomDBAdvGrid(AOwner);
  FMultiSelect := true;
  FMultiSelectKeys := [ssCtrl];
  FReadOnly := true;
  FStyle := siBackground;
end;

procedure TcySortIndicatorsOptions.SetCustomSortIndicators(const Value: TImageList);
begin
  FCustomSortIndicators := Value;
  if FGrid.FSortColumns.Count <> 0
  then FGrid.InvalidateTitles;

  if Value <> nil
  then Value.FreeNotification(FGrid);
end;

procedure TcySortIndicatorsOptions.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect := Value;
  if not FMultiSelect
  then
    if (FMultiSelectKeys = []) and (FGrid.SortColumns.Count > 1)
    then FGrid.SortColumns.ClearExcluding(0);
end;

procedure TcySortIndicatorsOptions.SetMultiSelectKeys(const Value: TShiftState);
begin
  FMultiSelectKeys := Value;

  if (FMultiSelectKeys = []) and (FGrid.SortColumns.Count > 1)
  then FGrid.SortColumns.ClearExcluding(0);
end;

procedure TcySortIndicatorsOptions.SetStyle(const Value: TSortIndicatorsStyle);
begin
  if FStyle = Value then
    Exit;

  FStyle := Value;

  if FGrid.SortColumns.Count <> 0 then
    FGrid.InvalidateTitles;
end;

{ tcySortColumn }
constructor tcySortColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAscendant := true;
  FCustomIndicatorIndex := 0;
  FFieldName := '';
end;

function TcySortColumn.GetDisplayName: string;
begin
  Result := FFieldName;
end;

procedure tcySortColumn.SetAscendant(const Value: Boolean);
begin
  FAscendant := Value;
  Changed(false);            // It will call TcySortColumns.Update !
end;

procedure tcySortColumn.SetCustomIndicatorIndex(const Value: Integer);
begin
  FCustomIndicatorIndex := Value;
  Changed(false);
end;

procedure tcySortColumn.SetFieldName(const Value: String);
begin
  FFieldName := Value;
  Changed(false);
end;

function tcySortColumn.GetInternalSortIndicatorIndex: Integer;
begin
  if Self.Collection.Count = 1
  then
    Result := 0
  else
    if Self.Index > 8    // bitmaps until number 9!
    then Result := 0
    else Result := (Self.Index + 1) * 2;

  if not FAscendant
  then Inc(Result, 1);
end;

{ tcySortColumns }
constructor tcySortColumns.Create(aGrid: TcyCustomDBAdvGrid; SortClass: TcySortClass);
begin
  inherited Create(SortClass);
  FGrid := aGrid;
end;

function tcySortColumns.Add: TcySortColumn;
begin
  Result := TcySortColumn(inherited Add);
  Result.Changed(false);      // It will call TcySortColumns.Update only at run-time!
end;

procedure tcySortColumns.Delete(Index: Integer);
begin
  Inherited;
  Update(Nil);
end;

function tcySortColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function tcySortColumns.GetSortColumn(Index: Integer): TcySortColumn;
begin
  Result := TcySortColumn(inherited Items[Index]);
end;

function tcySortColumns.GetSQLCode: String;
var i: Integer;
begin
  Result := '';
  for i := 0 to Count-1 do
  begin
    if Result = '' then
    begin
      Result := 'ORDER BY ';
    end
    else
      Result := Result + ', ';

    Result := Result + Items[i].FieldName;
    if not Items[i].Ascendant then Result := Result + ' DESC';
  end;
end;

// Event Called by setting properties/events of TcySortColumn :
procedure tcySortColumns.Update(Item: TCollectionItem);
begin
  inherited;
  FGrid.InvalidateTitles;
end;

function tcySortColumns.LocateField(aFieldName: String): TcySortColumn;
var i: Integer;
begin
  Result := nil;

  for i := 0 to Count-1 do
    if AnsiUpperCase(aFieldName) = AnsiUpperCase(Items[i].FieldName)
    then begin
      Result := Items[i];
      Break;
    end;
end;

procedure tcySortColumns.ClearExcluding(ItemIndex: Integer);
var i: Integer;
begin
  for i := Count-1 downto 0 do
    if i <> ItemIndex
    then Delete(I);
end;

{ TcyFixedOptions }
constructor TcyFixedOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyCustomDBAdvGrid(AOwner);
  FFlat := false;
  FFrame := false;
  FFrameColor := clBlack;
  FStyle := fsGradient;
end;

procedure TcyFixedOptions.PropertiesChanged;
begin
  if Assigned(FOnChange)
  then FOnChange(FGrid);
end;

procedure TcyFixedOptions.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value
  then begin
    FFlat := Value;
    PropertiesChanged;
  end;
end;

procedure TcyFixedOptions.SetFrame(const Value: Boolean);
begin
  if FFrame <> Value
  then begin
    FFrame := Value;
    PropertiesChanged;
  end;
end;

procedure TcyFixedOptions.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value
  then begin
    FFrameColor := Value;

    if Frame
    then PropertiesChanged;
  end;
end;

procedure TcyFixedOptions.SetFixedBgStyle(const Value: TFixedBgStyle);
begin
  if FStyle <> Value
  then begin
    FStyle := Value;
    PropertiesChanged;
  end;
end;

{ TcyCustomDBAdvGrid}
constructor TcyCustomDBAdvGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
  r: Integer;
  StrResName: String;
begin
  inherited Create(AOwner);
  FIndicatorsOptions := TcyIndicatorsOptions.Create(Self);
  FFixedColsColors := TcyGradient.Create(self);
  FFixedColsColors.OnChange := SubPropertiesChanged;
  FFixedRowsColors := TcyGradient.Create(self);
  FFixedRowsColors.OnChange := SubPropertiesChanged;
  FFixedColsOptions := TcyFixedOptions.Create(Self);
  FFixedColsOptions.OnChange := FixedColsPropertiesChanged;
  FFixedRowsOptions := TcyFixedOptions.Create(Self);
  FFixedRowsOptions.OnChange := FixedRowsPropertiesChanged;
  FForceScrollRepaint := false;
  FSelectionColor := clHighlight;
  FSelectionFontColor := clHighlightText;
  FSortColumns := tcySortColumns.Create(Self, TcySortColumn);
  FSortIndicatorsOptions := TcySortIndicatorsOptions.Create(Self);
  FTitleWordWrap := false;
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := SubPropertiesChanged;

  Bmp := TBitmap.Create;
  try
    // Internal indicators :
    Bmp.LoadFromResourceName(HInstance, AdvbmArrow);
    FInternalIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    FInternalIndicators.DrawingStyle := dsTransparent;
    FInternalIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, AdvbmEdit);
    FInternalIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, AdvbmInsert);
    FInternalIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, AdvbmMultiDot);
    FInternalIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, AdvbmMultiArrow);
    FInternalIndicators.AddMasked(Bmp, clWhite);

    // Internal sort indicators :
    for r := 0 to 9 do
    begin
      StrResName := 'ASC' + intToStr(r);
      Bmp.LoadFromResourceName(HInstance, StrResName);

      if r = 0
      then begin
        FInternalSortIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
        FInternalSortIndicators.DrawingStyle := dsTransparent;
      end;

      FInternalSortIndicators.AddMasked(Bmp, clWhite);
      StrResName := 'DESC' + intToStr(r);
      Bmp.LoadFromResourceName(HInstance, StrResName);
      FInternalSortIndicators.AddMasked(Bmp, clWhite);
    end;
  finally
    Bmp.Free;
  end;
end;

destructor TcyCustomDBAdvGrid.Destroy;
begin
  FIndicatorsOptions.Free;
  FFixedColsColors.Free;
  FFixedRowsColors.Free;
  FFixedColsOptions.Free;
  FFixedRowsOptions.Free;
  FSortColumns.Free;
  FSortColumns := Nil;
  FSortIndicatorsOptions.Free;
  FWallpaper.Free;
  FInternalIndicators.Free;
  FInternalSortIndicators.Free;
  Inherited;
end;

procedure TcyCustomDBAdvGrid.BeforePaint;
begin
  // Draw wallpaper in data cells rect:
  if FWallpaper.Style <> bgNone
  then cyDrawBgPicture(Canvas, ClientRectDataCells, FWallpaper);

  inherited;
end;
procedure TcyCustomDBAdvGrid.FixedColsPropertiesChanged(Sender: TObject);
begin
  if dgIndicator in Options
  then InvalidateCol(0);
end;

procedure TcyCustomDBAdvGrid.FixedRowsPropertiesChanged(Sender: TObject);
begin
  if dgTitles in Options
  then InvalidateTitles;
end;

procedure TcyCustomDBAdvGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SaveState: TGridState;
  Cell: TGridCoord;
  Column: TColumn;
  ColumnSorted: TcySortColumn;
  AscendantSort, AcceptSort: Boolean;
begin
  SaveState := FGridState;
  inherited MouseUp(Button, Shift, X, Y);

  // Sort columns handling :
  if (SaveState = gsRowSizing) or (SaveState = gsColSizing) or
    ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
     (PtInRect(InplaceEditor.BoundsRect, Point(X,Y))))
  then Exit;

  if (Button = mbLeft) and (not FSortIndicatorsOptions.FReadOnly)
  then begin
    Cell := MouseCoord(X, Y);

    if (Cell.X >= FirstDataColIndex) and (Cell.Y < FirstDataRowIndex)
    then begin
      AcceptSort := true;
      Column := Columns[RawToDataColumn(Cell.X)];
      ColumnSorted := FSortColumns.LocateField(Column.FieldName);

      if ColumnSorted = nil           // Column already in sort list?
      then begin
        // Add new sort column field :
        AscendantSort := true;
        if Assigned(FOnSortColumnUserDefine)
        then FOnSortColumnUserDefine(Self, Column, AscendantSort, AcceptSort);

        if AcceptSort
        then begin
          // Delete current sorted columns?
          if (not FSortIndicatorsOptions.FMultiSelect) or (Shift <> FSortIndicatorsOptions.FMultiSelectKeys)
          then FSortColumns.Clear;

          with FSortColumns.Add do
          begin
            FFieldName := Column.FieldName;
            FAscendant := AscendantSort;
          end;
        end;
      end
      else begin
        // Change current column sort settings:
        AscendantSort := not ColumnSorted.FAscendant;

        // Delete other sorted columns?
        if (not FSortIndicatorsOptions.FMultiSelect) or (Shift <> FSortIndicatorsOptions.FMultiSelectKeys)
        then FSortColumns.ClearExcluding(ColumnSorted.Index);

        if Assigned(FOnSortColumnUserDefine)
        then FOnSortColumnUserDefine(Self, Column, AscendantSort, AcceptSort);

        // Modify sort column field :
        if AcceptSort
        then ColumnSorted.Ascendant := AscendantSort;
      end;

      // Notify changes :
      if AcceptSort
      then
        if Assigned(FOnSortColumnsUserChange)
        then FOnSortColumnsUserChange(self);
    end;
  end;
end;

procedure TcyCustomDBAdvGrid.SubPropertiesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyCustomDBAdvGrid.SetIndicatorsOptions(const Value: TcyIndicatorsOptions);
begin
  FIndicatorsOptions.Assign(Value);
end;

procedure TcyCustomDBAdvGrid.SetFixedColsColors(const Value: TcyGradient);
begin
  FFixedColsColors.Assign(Value);
end;

procedure TcyCustomDBAdvGrid.SetFixedColsOptions(const Value: TcyFixedOptions);
begin
  FFixedColsOptions.Assign(Value);
end;

procedure TcyCustomDBAdvGrid.SetFixedRowsColors(const Value: TcyGradient);
begin
  FFixedRowsColors.Assign(Value);
end;

procedure TcyCustomDBAdvGrid.SetFixedRowsOptions(const Value: TcyFixedOptions);
begin
  FFixedRowsOptions.Assign(Value);
end;

procedure TcyCustomDBAdvGrid.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
  Invalidate;
end;

procedure TcyCustomDBAdvGrid.SetSelectionFontColor(const Value: TColor);
begin
  FSelectionFontColor := Value;
  Invalidate;
end;

procedure TcyCustomDBAdvGrid.SetSortColumns(const Value: tcySortColumns);
begin
  FSortColumns := Value;
end;

procedure TcyCustomDBAdvGrid.SetSortFIndicatorsOptions(const Value: TcySortIndicatorsOptions);
begin
  FSortIndicatorsOptions.Assign(Value);
end;

procedure TcyCustomDBAdvGrid.SetTitleWordWrap(const Value: Boolean);
begin
  FTitleWordWrap := Value;
  InvalidateTitles;
end;

procedure TcyCustomDBAdvGrid.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

procedure TcyCustomDBAdvGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;

  BeforeDrawCell(ACol, ARow, ARect, AState);
  DoDrawCell(ACol, ARow, ARect, AState);

  // Draw checkbox in Indicators col when painting last column:
  if DataLink.Active and CheckBoxes.Visible and (CheckBoxes.ColumnMode = cmIndicatorsCol) then
    if (ACol < FirstDataColIndex) and (ARow >= FirstDataRowIndex) then
      DrawCheckBoxCell(0, ARow);

  AfterDrawCell(ACol, ARow, ARect, AState);
end;

// *** Note: a part of the code below is from original DBGrids.pas unit *** //
var
  DrawBitmap: TBitmap;
  UserCount: Integer;

procedure UsesBitmap;
begin
  if UserCount = 0 then
    DrawBitmap := TBitmap.Create;
  Inc(UserCount);
end;

procedure ReleaseBitmap;
begin
  Dec(UserCount);
  if UserCount = 0 then DrawBitmap.Free;
end;

procedure TcyCustomDBAdvGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  // Careful, FIndicatorsOptions may be not created already, check operation first!
  if Operation = opRemove
  then begin
    // Removed Custom Indicators ?
    if FIndicatorsOptions.FCustomIndicators <> nil
    then
      if AComponent = FIndicatorsOptions.FCustomIndicators
      then begin
        FIndicatorsOptions.FCustomIndicators := nil;
        InvalidateCol(0);
      end;

    // Removed Custom Sort Indicators ?
    if FSortIndicatorsOptions.FCustomSortIndicators <> nil
    then
      if AComponent = FSortIndicatorsOptions.FCustomSortIndicators
      then begin
        FSortIndicatorsOptions.FCustomSortIndicators := nil;
        InvalidateTitles;
      end;
  end;
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; ARightToLeft: Boolean);
const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX );
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  B, R: TRect;
  Hold, Left: Integer;
  I: TColorRef;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if GetNearestColor(ACanvas.Handle, I) = I then
  begin                       { Use ExtTextOut for solid colors }
    { In BiDi, because we changed the window origin, the text that does not
      change alignment, actually gets its alignment changed. }
    if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
  end
  else begin                  { Use FillRect and Drawtext for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
        DrawText(Handle, PChar(Text), Length(Text), R,
          AlignFlags[Alignment] or RTL[ARightToLeft]);
      end;
      if (ACanvas.CanvasOrientation = coRightToLeft) then
      begin
        Hold := ARect.Left;
        ARect.Left := ARect.Right;
        ARect.Right := Hold;
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

procedure TcyCustomDBAdvGrid.DoDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

      // We need to adjust ARect because of dgColLines/dgRowLines in options in order to paint all title cell area:
      procedure AdjustFixedRect;
      begin
        if [dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]
        then
          ARect := classes.Rect(ARect.Left, ARect.Top, ARect.Right + 1, ARect.Bottom + 1)
        else
          if dgColLines in Options
          then
            ARect := classes.Rect(ARect.Left, ARect.Top, ARect.Right + 1, ARect.Bottom)
          else
            if dgRowLines in Options
            then ARect := classes.Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom + 1);
      end;

var
  DrawColumn: TColumn;
  FTitleOffset, FIndicatorOffset: Byte;
begin
  if csLoading in ComponentState
  then begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;

  FTitleOffset := FirstDataRowIndex;
  FIndicatorOffset := FirstDataColIndex;
  Dec(ARow, FTitleOffset);
  Dec(ACol, FIndicatorOffset);

  if (gdFixed in AState) and (ACol < FixedCols-1)
  then begin
    // Draw indicators:
    AdjustFixedRect;
    DrawIndicatorCell(ACol + FIndicatorOffset, ARow + FTitleOffset, ARect, AState);
  end
  else begin
    DrawColumn := Columns[ACol];
    if DrawColumn.Showing
    then
      if ARow < 0
      then begin
        AdjustFixedRect;
        DrawTitleCell(ACol + FIndicatorOffset, ARow + FTitleOffset, ARect, DrawColumn, AState);  // Draw titles
      end
      else begin
        _DrawDataCell(ACol + FIndicatorOffset, ARow + FTitleOffset, ARect, DrawColumn, AState);   // Draw Data cells
      end;
  end;
end;

procedure TcyCustomDBAdvGrid.DrawTitleCellBackGround(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState; fromColor, toColor, frameColor: TColor);
begin
  if fromColor = toColor
  then begin
    Canvas.Brush.Color := fromColor;
    Canvas.FillRect(ARect);
  end
  else
    cyGradientFill(Canvas, ARect, fromColor, toColor, FFixedRowsColors.Orientation, FFixedRowsColors.Balance,
      FFixedRowsColors.BalanceMode, FFixedRowsColors.MaxDegrade, FFixedRowsColors.SpeedPercent);

  // Draw frame :
  if FFixedRowsOptions.FFrame
  then begin
    cyFrame3D(Canvas, ARect, frameColor, frameColor, 1, ACol = 0, ARow = 0, true, true, false);
    if ACol > 0  // We don' t have paint left side ...
    then
      if not FFixedRowsOptions.FFlat
      then ARect := classes.Rect(ARect.Left - 1, ARect.Top, ARect.Right, ARect.Bottom);
  end;

  // Draw edges :
  if not FFixedRowsOptions.FFlat
  then cyFrame3D(Canvas, ARect, fromColor, toColor, 1, true, true, true, true, false);
end;

procedure TcyCustomDBAdvGrid.DrawTitleCell(ACol, ARow: Longint; ARect: TRect; Column: TColumn; AState: TGridDrawState);
const
  ScrollArrows: array [Boolean, Boolean] of Integer =
    ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
var
  fromColor, toColor, frameColor: TColor;
  DrawBackground: Boolean;
  MasterCol: TColumn;
  TitleRect, TextRect, ButtonRect: TRect;
  I: Integer;
  InBiDiMode: Boolean;
  SortColumn: TcySortColumn;

      procedure DrawSortIndicator(ImageList: TImageList; ImageListIndex: Integer);
      var aLeft, aTop: Integer;
      begin
        aLeft := TextRect.Left;
        aTop := TextRect.Top + (TextRect.Bottom - TextRect.Top - ImageList.Height) div 2;

        if FSortIndicatorsOptions.FStyle <> siCaptionSide then
        begin
          // Draw
          if Column.Title.Alignment = taLeftJustify then
            aLeft := TextRect.Right - ImageList.Width;
        end
        else
          TextRect.Left := TextRect.Left + ImageList.Width;

        ImageList.Draw(Canvas, aLeft, aTop, ImageListIndex, True);
      end;

begin
  TitleRect := CalcTitleRect(Column, ARow, MasterCol);

  if MasterCol = nil
  then Exit;

  Canvas.Font := MasterCol.Title.Font;
  TextRect := TitleRect;
  Inc(TextRect.Left, FrameOffs);
  Dec(TextRect.Right, FrameOffs);

  if FFixedRowsOptions.FStyle = fsFixedColor
  then begin
    fromColor := Column.Title.Color;  // FixedColor
    toColor := fromColor;
  end
  else begin
    fromColor := FFixedRowsColors.FromColor;
    toColor := FFixedRowsColors.ToColor;

    // if FromColor = Self.FixedColor, we set FromColor with Column.Title.Color:
    if fromColor = FixedColor then
      fromColor := Column.Title.Color;

    if toColor = FixedColor then
      toColor := Column.Title.Color;
  end;

  frameColor := FFixedRowsOptions.FFrameColor;
  DrawBackground := true;
  if Assigned(FOnBeforeDrawTitleCell)
  then FOnBeforeDrawTitleCell(Self, ACol, ARow, ARect, Column, AState, fromColor, toColor, frameColor, DrawBackground);

  if DrawBackground
  then DrawTitleCellBackGround(ACol, ARow, ARect, AState, fromColor, toColor, frameColor);

  I := GetSystemMetrics(SM_CXHSCROLL);
  if ((TextRect.Right - TextRect.Left) > I)
    and MasterCol.Expandable
  then begin
    Dec(TextRect.Right, I);
    ButtonRect := TitleRect;
    ButtonRect.Left := TextRect.Right;
    I := SaveDC(Canvas.Handle);
    try
      Canvas.FillRect(ButtonRect);
      InflateRect(ButtonRect, -1, -1);
      IntersectClipRect(Canvas.Handle, ButtonRect.Left,
        ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
      InflateRect(ButtonRect, 1, 1);
      // DrawFrameControl doesn't draw properly when orienatation has changed.
      //  It draws as ExtTextOut does.
      InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
      if InBiDiMode then // stretch the arrows box
        Inc(ButtonRect.Right, GetSystemMetrics(SM_CXHSCROLL) + 4);
      DrawFrameControl(Canvas.Handle, ButtonRect, DFC_SCROLL,
        ScrollArrows[InBiDiMode, MasterCol.Expanded] or DFCS_FLAT);
    finally
      RestoreDC(Canvas.Handle, I);
    end;
  end;

  // Draw Sort indicator :
  if Column.Field <> Nil
  then SortColumn := FSortColumns.LocateField(Column.Field.FieldName)
  else SortColumn := Nil;

  if SortColumn <> nil
  then
    if Assigned(FSortIndicatorsOptions.FCustomSortIndicators)
    then DrawSortIndicator(FSortIndicatorsOptions.FCustomSortIndicators, SortColumn.FCustomIndicatorIndex)
    else DrawSortIndicator(FInternalSortIndicators, SortColumn.GetInternalSortIndicatorIndex);

  // Draw text:
  with MasterCol.Title do
  begin
    Canvas.Brush.Style := bsClear;
    // WriteText(Canvas, TextRect, FrameOffs, FrameOffs, Caption, Alignment, IsRightToLeft);
    cyDrawText(Canvas.Handle, Caption, TextRect, DrawTextFormatFlags(dt_NoPrefix, Alignment, tlCenter, FTitleWordWrap));
    Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TcyCustomDBAdvGrid.DrawIndicatorCellBackGround(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState; fromColor, toColor, frameColor: TColor);
begin
  if fromColor = toColor
  then begin
    Canvas.Brush.Color := fromColor;
    Canvas.FillRect(ARect);
  end
  else
    cyGradientFill(Canvas, ARect, fromColor, toColor, FFixedColsColors.Orientation, FFixedColsColors.Balance,
      FFixedColsColors.BalanceMode, FFixedColsColors.MaxDegrade, FFixedColsColors.SpeedPercent);

  // Draw frame :
  if FFixedColsOptions.FFrame
  then cyFrame3D(Canvas, ARect, frameColor, frameColor, 1, ACol = 0, ARow = 0, true, true, false);

  // Draw edges :
  if not FFixedColsOptions.FFlat
  then cyFrame3D(Canvas, ARect, fromColor, toColor, 1, true, true, true, true, false);
end;

procedure TcyCustomDBAdvGrid.DrawIndicatorCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

      function RowIsMultiSelected: Boolean;
      var
        Index: Integer;
      begin
        Result := (dgMultiSelect in Options) and Datalink.Active and
          SelectedRows.Find(Datalink.Datasource.Dataset.Bookmark, Index);
      end;

var
  fromColor, toColor, frameColor: TColor;
  DrawBackground: Boolean;
  ALeft, ATop: Integer;
  DataRow: Integer;
  OldActive: Integer;
  Indicator: Integer;
  MultiSelected: Boolean;
  IndicatorsList: TImageList;
begin
  if FFixedColsOptions.FStyle = fsFixedColor
  then begin
    fromColor := FixedColor;
    toColor := FixedColor;
  end
  else begin
    fromColor := FFixedColsColors.FromColor;
    toColor := FFixedColsColors.ToColor;
  end;

  frameColor := FFixedColsOptions.FFrameColor;
  DrawBackground := true;
  if Assigned(FOnBeforeDrawIndicatorCell)
  then FOnBeforeDrawIndicatorCell(Self, ACol, ARow, ARect, AState, fromColor, toColor, frameColor, DrawBackground);

  if DrawBackground
  then DrawIndicatorCellBackGround(ACol, ARow, ARect, AState, fromColor, toColor, frameColor);

  // We can' t draw checkboxes here because TDBGrid paints indicators cells before painting data cells.
  // So, we are in the same record.

  // Get Indicator glyph index :
  if Assigned(DataLink) and DataLink.Active  then
  begin
    DataRow := ARow - FirstDataRowIndex;
    MultiSelected := False;
    if DataRow >= 0 then
    begin
      OldActive := DataLink.ActiveRecord;
      try
        Datalink.ActiveRecord := DataRow;
        MultiSelected := RowIsMultiselected;
      finally
        Datalink.ActiveRecord := OldActive;
      end;
    end;

    if (DataRow = DataLink.ActiveRecord) or MultiSelected
    then begin
      if Assigned(FIndicatorsOptions.FCustomIndicators)
      then begin
        IndicatorsList := FIndicatorsOptions.FCustomIndicators;

        Indicator := FIndicatorsOptions.FCustomActiveIndex;  // arrow

        if DataLink.DataSet <> nil
        then
          if DataRow = DataLink.ActiveRecord
          then begin
            case DataLink.DataSet.State of
              dsEdit: Indicator := FIndicatorsOptions.FCustomEditIndex;
              dsInsert: Indicator := FIndicatorsOptions.FCustomInsertIndex;
              dsBrowse:
                if MultiSelected
                then Indicator := FIndicatorsOptions.FCustomMultiActiveIndex;  // multiselected and current row
            end;
          end
          else
            Indicator := FIndicatorsOptions.FCustomMultiIndex;   // multiselected but not current row
      end
      else begin
        IndicatorsList := FInternalIndicators;

        Indicator := InternalIndicatorArrow;  // arrow

        if DataLink.DataSet <> nil
        then
          if DataRow = DataLink.ActiveRecord
          then begin
            case DataLink.DataSet.State of
              dsEdit: Indicator := InternalIndicatorEdit;
              dsInsert: Indicator := InternalIndicatorInsert;
              dsBrowse:
                if MultiSelected
                then Indicator := InternalIndicatorMultiSelectCurrent;  // multiselected and current row
            end;
          end
          else
            Indicator := InternalIndicatorMultiSelect;   // multiselected but not current row
      end;

      if Assigned(IndicatorsList)
      then begin
        case FIndicatorsOptions.FAlignment of
          taCenter:
            ALeft := (ARect.Right + ARect.Left - IndicatorsList.Width) shr 1; // same as (ARect.Right - ARect.Left - IndicatorsList.Width) div 2

          taLeftJustify:
          begin
            ALeft := ARect.Left + 1;
            if not FFixedColsOptions.FFlat
            then Inc(ALeft, 1);
            if FFixedColsOptions.FFrame
            then Inc(ALeft, 1);
          end;

          taRightJustify:
          begin
            ALeft := ARect.Right - IndicatorsList.Width - 1;
            if not FFixedColsOptions.FFlat
            then Dec(ALeft, 1);
            if FFixedColsOptions.FFrame
            then Dec(ALeft, 1);
          end;
        end;

        ATop := (ARect.Top + ARect.Bottom - IndicatorsList.Height) shr 1;

        if Canvas.CanvasOrientation = coRightToLeft then Inc(ALeft);
        IndicatorsList.Draw(Canvas, ALeft, ATop, Indicator, True);
      end;

// We can´t access FSelRow :
//        if ADataRow = Datalink.ActiveRecord then
//          FSelRow := ADataRow + FTitleOffset;
    end;
  end;
end;

procedure TcyCustomDBAdvGrid._DrawDataCell(ACol, ARow: Longint; ARect: TRect; Column: TColumn; AState: TGridDrawState);
var
  Value: string;
  DataCol: Integer;
  DataRow: Integer;
  OldActive: Integer;
  Highlight, DrawBackground, DrawFieldValue: Boolean;
  RectText, RectCheckBox: TRect;
  CheckBoxState: Boolean;
  Index: Integer;
  FieldContentRendering: TFieldContentRendering;

          procedure DoDrawBackGround;
          begin
            if DrawBackground
            then begin
              DrawDataCellBackGround(ACol, ARow, ARect, AState);
              DrawBackground := false;
            end;
          end;

begin
  DataRow := ARow - FirstDataRowIndex;
  DataCol := ACol - FirstDataColIndex;

  with Canvas do
  begin
    Font := Column.Font;
    Brush.Color := Column.Color;

    if (DataLink = nil) or not DataLink.Active
    then
      FillRect(ARect)
    else begin
      Value := '';
      OldActive := DataLink.ActiveRecord;
      try
        DataLink.ActiveRecord := DataRow;
        if Assigned(Column.Field) then
          Value := Column.Field.DisplayText;

        if (HotMode <> hmNone) and ((FMouseOverCell.X = ACol) or (HotMode = hmRow)) and (FMouseOverCell.Y = ARow) then
        begin
          Brush.Color := HotColor;
          Font.Color := HotFontColor;
        end
        else begin
          Highlight := HighlightCell(DataCol, DataRow, Value, AState);
          if Highlight then
          begin
            Brush.Color := FSelectionColor;
            Font.Color := FSelectionFontColor;
          end;
        end;

        if not Enabled then
          Font.Color := clGrayText;

        // Only draw background with wallpaper if cell highlighted:
        DrawBackground := DefaultDrawing and ((not FWallpaper.SomethingToDraw) or (HighLight));
        DrawFieldValue := DefaultDrawing;

        // Let user change Canvas, AState, DrawBackground and DrawFieldValue:
        if Assigned(FOnBeforeDrawColumnCell)
        then FOnBeforeDrawColumnCell(Self, ACol, ARow, ARect, Column, AState, HighLight, DrawBackground, DrawFieldValue);

        // CheckBoxes:
        if not CheckBoxes.Visible
        then
          RectText := ARect
        else
          if DataCol = GetCheckBoxColumnIndex
          then begin
            DoDrawBackGround;
            CalcCheckBoxColumnCellLayout(ARect, RectText, RectCheckBox);
            // Paint CheckBox :
            CheckBoxState := CheckedList.Find(Datalink.Datasource.Dataset.Bookmark, Index);
            DrawCheckBox(RectCheckBox, CheckBoxState);
          end
          else
            RectText := ARect;

        if DrawFieldValue
        then begin
          // Define text rendering:
          FieldContentRendering := fcDefault;

          if ContentFieldsRender.BooleanField = bfCheckBox then
            if Column.Field is TBooleanField then
              FieldContentRendering := fcCheckBox;

          if ContentFieldsRender.MemoField = mfMemo then
            if Column.Field is TMemoField then
              FieldContentRendering := fcMemo;

          if ContentFieldsRender.GraphicField = gfImage then
            if Column.Field is TGraphicField then    // TGraphicField is descendent of TBlobField and can contain bitmap, ico etc ...
              FieldContentRendering := ContentImageOptions.FieldContent;

          // Detect TBlobField type (graphic or memo):
          if FieldContentRendering = fcDefault
          then
            if Column.Field is TBlobField
            then
              case TBlobField(Column.Field).BlobType of
                ftGraphic, ftTypedBinary, ftBlob:
                  if ContentFieldsRender.GraphicField = gfImage then
                    FieldContentRendering := ContentImageOptions.FieldContent;

                ftFmtMemo {$IFDEF DELPHI2009_OR_ABOVE} , ftWideMemo {$ENDIF} :
                  if ContentFieldsRender.MemoField = mfMemo then
                    FieldContentRendering := fcMemo;
              end;

            // Let user change FieldContentRendering:
          if Assigned(FOnSetContentFieldRendering)
          then FOnSetContentFieldRendering(Self, ACol, ARow, ARect, Column, AState, Highlight, FieldContentRendering);

          case FieldContentRendering of
            fcDefault:
            begin
              if DrawBackground
              then DrawBackground := false         // WriteText fill background ...
              else Canvas.Brush.Style := bsClear;

              WriteText(Canvas, RectText, 2, 2, Value, Column.Alignment,
                UseRightToLeftAlignmentForField(Column.Field, Column.Alignment));

              if not DrawBackground
              then Canvas.Brush.Style := bsSolid;  // Restore brush style
            end;

            fcWordwrap:
            begin
              DoDrawBackGround;
              DrawWordwrapTextField(RectText, Column.Field);
            end;

            fcMemo:
            begin
              DoDrawBackGround;
              DrawMemoField(RectText, Column.Field, ContentMemoOptions.Wordwrap, not DrawBackGround, ContentMemoOptions.MaxCars, ContentMemoOptions.MaxLines);
            end;

            fcCheckBox:
            begin
              DoDrawBackGround;
              DrawBooleanField(RectText, Column.Field);
            end;

            fcBitmap, fcIcon, fcJpeg, fcPng:
            begin
              DoDrawBackGround;
              DrawGraphicField(RectText, TBlobField(Column.Field), GetGraphicClass(FieldContentRendering),
                ContentImageOptions.Transparent, ContentImageOptions.DrawStyle, ContentImageOptions.Position);
            end;
          end;
        end;

        DoDrawBackGround;  // if DrawFieldValue = false, background is not yet painted ...

        if Columns.State = csDefault then
          Inherited DrawDataCell(ARect, Column.Field, AState);
        DrawColumnCell(ARect, DataCol, Column, AState);
      finally
        DataLink.ActiveRecord := OldActive;
      end;
      if DefaultDrawing and (gdSelected in AState)
        and ((dgAlwaysShowSelection in Options) or Focused)
        and not (csDesigning in ComponentState)
        and not (dgRowSelect in Options)
        and (UpdateLock = 0)
        and (ValidParentForm(Self).ActiveControl = Self) then
        Windows.DrawFocusRect(Handle, ARect);
    end;
  end;
end;

procedure TcyCustomDBAdvGrid.DrawDataCellBackGround(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  Canvas.FillRect(ARect);
end;

procedure TcyCustomDBAdvGrid.Scroll(Distance: Integer);
var RedrawRect: TRect;
begin
  if not HandleAllocated then Exit;

  if ForceScrollRepaint
    and (Distance <> 0)    // Partial Row painting and each record is painted in a different Row
  then begin
    RedrawRect := BoxRect(0, 0, ColCount - 1, RowCount - 1);
    InvalidateRect(Handle, @RedrawRect, false);
  end;

  // Inherited at the end avoid flickering!
  Inherited;
end;

end.
