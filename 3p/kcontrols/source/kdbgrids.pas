{ @abstract(This unit contains the TKDBGrid component and all supporting classes)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(20 Sep 2009)
  @lastmod(20 Jun 2010)

  Copyright © 2009 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  This unit provides a data aware control for TKGrid.
  Note: I am still a newbie to Delphi/Lazarus database solutions. If anything
  is totally wrong here please feel free to send a patch or hint to me.

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. However, you may distribute only the original
  package. The Author accepts no liability for any damage that may result
  from using this code. }

unit KDBGrids;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, DB, DBCtrls,
  KFunctions, KGraphics, KGrids;

resourcestring
  { @exclude }
  SKDBGridIndex = 'Index';

type
  { Declares possible values for the @link(TKCustomDBGrid.DBOptions) property. }
  TKDBGridOption = (
    { Automatically moves current record to edited or selected row. }
    dboAutoMoveRecord,
    { Forces the cells with boolean fields to be automatically adjusted to checkbox frame size. }
    dboAutoSizeBooleanCells,
    { Forces the cells with image fields to be automatically adjusted to image size. }
    dboAutoSizeImageCells,
    { Forces the column names to be assigned to fixed cells in the first fixed row. }
    dboColNamesToHeader,
    { Does not clear fixed cell texts if table is closed. }
    dboDontClearFixedCells,
    { For all BLOB/image columns, images will be displayed in original size in the cell hint window. }
    dboImageHint,
    { Images loaded from database can be modified by user and thus will be
      saved into database if this option is included. }
    dboImagesWritable,
    { Forces the row indexes to be assigned to fixed cells in the first fixed column. }
    dboIndexFixedCol,
    { Indicates the active record row. }
    dboIndicateActiveRecord
  );

  { Set type for @link(TKDBGridOption) enumeration. }
  TKDBGridOptions = set of TKDBGridOption;

const
  { Default value for the @link(TKCustomDBGrid.DBOptions) property. }
  cDBOptionsDef = [dboAutoMoveRecord, dboAutoSizeBooleanCells,
    dboColNamesToHeader, dboIndexFixedCol, dboIndicateActiveRecord];

  { Default value for the @link(TKDBGridColors.ActiveRecord) property. }
  cActiveRecordDef = clCream;

  { Used by default to distinguish image field type. }
  cDefaultImageSet = [ftBlob, ftGraphic];

  { Used by default to distinguish string field type. }
  cDefaultStringSet = [ftString, ftSmallInt, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftAutoInc, ftMemo, ftFmtMemo,
    ftFixedChar, ftWideString, ftLargeInt, ftGuid, ftTimeStamp, ftFmtBCD
  {$IF DEFINED(FPC) OR DEFINED(COMPILER10_UP)}
    , ftWideMemo
  {$IFEND}
    ];

  { Index for the @link(TKDBGridColors.ActiveRecord) property. }
  ciActiveRecord = TKGridColorIndex(ciGridColorsMax + 1);
  { Maximum color array index }
  ciDBGridColorsMax = ciActiveRecord;

  { This internal flag is set if grid is being updated. }
  cGF_DBDataUpdating             = $00010000;
  { This internal flag is set if data record is being changed. }
  cGF_DBInternalChanging         = $00020000;

type
  TKCustomDBGrid = class;

  { @abstract(Data link override for TKCustomDBGrid)
    This class overrides TDataLink to extend behavior for TKCustomDBGrid. }
  TKDBGridDataLink  = class(TDataLink)
  private
    FGrid: TKCustomDBGrid;
    FModified: Boolean;
    procedure SetModified(const Value: Boolean);
  protected
    { Called if data set has been opened or closed. }
    procedure ActiveChanged; override;
    { Called if data in the data set has been changed. }
    procedure DataSetChanged; override;
    { Called if current record has been moved. }
    procedure DataSetScrolled(Distance: Integer); override;
    { Called if data set layout has been modified. }
    procedure LayoutChanged; override;
    { Called if current record has been modified. }
    procedure RecordChanged(Field: TField); override;
    { Called if unsaved data is about to be saved into database. }
    procedure UpdateData; override;
  public
    { Creates the instance. }
    constructor Create(AGrid: TKCustomDBGrid);
    { Specifies the TKCustomDBGrid instance assigned to this TKDBGridDataLink instance. }
    property Grid: TKCustomDBGrid read FGrid;
    { Determines if the current record has been modified. }
    property Modified: Boolean read FModified write SetModified;
  end;

{$IFDEF TKDBGRIDCELL_IS_TKGRIDATTRTEXTCELL}
  { @exclude }
  TKDBGridCellAncestor = TKGridAttrTextCell;
{$ELSE}
  { @exclude }
  TKDBGridCellAncestor = TKGridTextCell;
{$ENDIF}

  { @abstract(Base cell class for TKDBGrid)
    This is the base cell class. It has always a Text property. Descendants can
    add other specific data, e.g. BLOB pointers etc. }
  TKDBGridCell = class(TKDBGridCellAncestor)
  private
    FGraphic: TGraphic;
  protected
    { Calls @link(TKCustomDBGrid.BeforeCellUpdate). }
    procedure BeforeUpdate; override;
    { Loads appropriate image. }
    function CreateImageByType(const Header: TKImageHeaderString): TGraphic; virtual;
    { Assigns cell properties to field data. }
    procedure FieldFromCell(AField: TField); virtual;
    { Assigns field data to cell properties. }
    procedure FieldToCell(AField: TField); virtual;
    { Assigns AField buffer to Graphic property. }
    procedure ImageFromField(AField: TField);
    { Assigns Graphic property to AField buffer. }
    procedure ImageToField(AField: TField);
    { Initializes the cell data. }
    procedure Initialize; override;
    { Assigns AField buffer to Text property. }
    procedure TextFromField(AField: TField);
    { Assigns Text property to AField buffer. }
    procedure TextToField(AField: TField);
  public
    { Creates the instance. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Applies TKDBGridCell properties to the cell painter. }
    procedure ApplyDrawProperties; override;
    { Returns a pointer to the image read from database. }
    property Graphic: TGraphic read FGraphic;
  end;

  { @abstract(Column class for TKCustomDBGrid)
    This column class implements some extra properties for TKCustomDBGrid. }
  TKDBGridCol = class(TKGridCol)
  private
    FCurrencyFormat: TKCurrencyFormat;
    FDataType: TFieldType;
    FName: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  public
    { Creates the instance. Do not create custom instances. All necessary
      TKDBGridCol instances are created automatically by TKCustomDBGrid. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Specifies the currency formatting settings if the column has currency data type.  }
    property CurrencyFormat: TKCurrencyFormat read FCurrencyFormat write FCurrencyFormat;
    { Returns the field data type. It is assigned automatically
      by the TKDGGrid's data source. }
    property DataType: TFieldType read FDataType;
    { Specifies the database column name. It is assigned automatically
      by the TKDGGrid's data source. }
    property Name: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF} read FName;
  end;

  { @abstract(Metaclass for @link(TKDBGridCol)). }
  TKDBGridColClass = class of TKDBGridCol;

  { @abstract(Cell painter class used by TKCustomDBGrid class)
    Overrides some TKGridCellPainter methods for usage with TKCustomDBGrid. }
  TKDBGridCellPainter = class(TKGridCellPainter)
  public
    { Low level method. Prepares default painting attributes. Applies
      attributes specific for TKDBGrid. }
    procedure DefaultAttributes; override;
  end;

  { @abstract(Container for all colors used by TKCustomDBGrid class)
    Adds some extra colors used by TKCustomDBGrid. }
  TKDBGridColors = class(TKGridColors)
  private
    function GetColor(Index: TKGridColorIndex): TColor;
    procedure SetColor(Index: TKGridColorIndex; Value: TColor);
  protected
    { Initializes the color array. }
    procedure Initialize; override;
  published
    { Specifies the color used to indicate active record. }
    property ActiveRecord: TColor index ciActiveRecord read GetColor write SetColor default cActiveRecordDef;
  end;

  { @abstract(KGrid data aware base component) This is the class that you use as
    the ancestor for your TKCustomDBGrid overrides. }
  TKCustomDBGrid = class(TKCustomGrid)
  private
    FActiveRecord: Integer;
    FDBOptions: TKDBGridOptions;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDBOptions(const Value: TKDBGridOptions);
  protected
    { This field represents the internal data link. } 
    FDataLink: TKDBGridDataLink;
    { Does nothing. Row moving not supported. }
    function BeginRowDrag(var Origin: Integer; const MousePt: TPoint): Boolean; override;
    { Fills the grid with data from database and/or updates the grid. }
    procedure DataChanged; dynamic;
    { Called if current record has been moved. }
    procedure DataSetScrolled; dynamic;
    { Extends TKCustomGrid behavior. Sets the data set into edited state and
      informs the data link about cell change. }
    procedure Changed; override;
    { Extends TKCustomGrid behavior. Updates the grid if column has been moved. }
    procedure ColMoved(FromIndex, ToIndex: Integer); override;
    { Extends TKCustomGrid behavior. Calls the event if data set is active etc. }
    function CustomSortRows(ByCol: Integer; var SortMode: TKGridSortMode): Boolean; override;
    { Extends TKCustomGrid behavior. Does not allow to edit if data set is writable
      or closed etc. }
    function EditorCreate(ACol, ARow: Integer): TWinControl; override;
    { Moves to another record if initiated by the grid. }
    procedure InternalSetActiveRecord(Value: Integer); dynamic;
    { Used internally to set column count. }
    procedure InternalSetColCount(Value: Integer); override;
    { Used internally to set fixed column count. }
    procedure InternalSetFixedCols(Value: Integer); override;
    { Used internally to set fixed row count. }
    procedure InternalSetFixedRows(Value: Integer); override;
    { Used internally to set row count. }
    procedure InternalSetRowCount(Value: Integer); override;
    { Allows to decide whether the goVirtualGrid option can be modified.
      Returns always False as no virtual grid possible in TKDBGrid. }
    function InternalUpdateVirtualGrid: Boolean; override;
    { Called if current record has been modified. }
    procedure RecordChanged; dynamic;
    { Extends TKCustomGrid behavior. Forces the previous modified record to be
      written into database. }
    function SelectCell(ACol, ARow: Integer): Boolean; override;
    { Extends TKCustomGrid behavior. Updates the grid if top row or left column has
      been changed. }
    procedure TopLeftChanged; override;
    { Called if unsaved data is about to be saved into database. }
    procedure UpdateData; dynamic;
    { Extends TKCustomGrid Behavior. Updates the grid if control size has
      been changed. }
    procedure UpdateSize; override;
  public
    { Creates the instance. Assigns default values to properties, allocates
      default column, row and cell data, constucts a data link. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the instance along with all allocated column, row and cell data,
      destroys the data link. }
    destructor Destroy; override;
    { Notifies the grid that a cell has been modified. }
    procedure BeforeCellUpdate(ACol, ARow: Integer); dynamic;
    { Does nothing. Clearing entire column is not supported. }
    procedure ClearCol(ACol: Integer); override;
    { Does nothing. Clearing entire grid is not supported. }
    procedure ClearGrid; override;
    { Does nothing. Clearing entire row is not supported. }
    procedure ClearRow(ARow: Integer); override;
    { Writes any modified data in the current record into database. }
    procedure Commit; dynamic;
    { Provides default behavior for the @link(OnEditorCreate) event. }
    procedure DefaultEditorCreate(ACol, ARow: Integer;
      var AEditor: TWinControl); override;
    { Provides default behavior for the @link(OnEditorDataFromGrid) event. }
    procedure DefaultEditorDataFromGrid(AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean); override;
    { Provides default behavior for the @link(OnEditorDataToGrid) event. }
    procedure DefaultEditorDataToGrid(AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean); override;
    { Provides default behavior for the @link(OnEditorResize) event. }
    procedure DefaultEditorResize(AEditor: TWinControl;
      ACol, ARow: Integer; var ARect: TRect); override;
    { Provides default behavior for the @link(OnEditorSelect) event. }
    procedure DefaultEditorSelect(AEditor: TWinControl;
      ACol, ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean); override;
    { Provides default cell hint behavior. }
    procedure DefaultMouseCellHint(ACol, ARow: Integer; AShow: Boolean); override;
    { Does nothing. Deleting columns not supported. }
    procedure DeleteCols(At, Count: Integer); override;
    { Forces the data set to delete record at location At. }
    procedure DeleteRow(At: Integer); override;
    { Does nothing. Deleting more rows not supported. }
    procedure DeleteRows(At, Count: Integer); override;
    { Does nothing. Inserting columns not supported. }
    procedure InsertCols(At, Count: Integer); override;
    { Forces the data set to insert new record at location At. }
    procedure InsertRow(At: Integer); override;
    { Does nothing. Inserting more rows not supported. }
    procedure InsertRows(At, Count: Integer); override;
    { Does nothing. Inserting sorted columns not supported. }
    function InsertSortedCol(out ByRow, ACol: Integer): Boolean; override;
    { Does nothing. Inserting sorted rows not supported. }
    function InsertSortedRow(out ByCol, ARow: Integer): Boolean; override;
    { Does nothing. Row moving not supported. }
    procedure MoveRow(FromIndex, ToIndex: Integer); override;
    { Specifies the data source. }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    { Specifies various display and behavioral properties of TKDGGrid. }
    property DBOptions: TKDBGridOptions read FDBOptions write SetDBOptions default cDBOptionsDef;
  end;

  { For backward compatibility. }
  TKDBCustomGrid = TKCustomDBGrid;

  { @abstract(KDBGrid design-time component) This is the class you use both
    on run-time and design-time. }
  TKDBGrid = class(TKCustomDBGrid)
  published
    { Inherited property - see Delphi help. }
    property Align;
    { Inherited property - see Delphi help. }
    property Anchors;
    { See TKCustomGrid.@link(TKCustomControl.BorderStyle) for details. }
    property BorderStyle;
    { Inherited property - see Delphi help. }
    property BorderWidth;
    { See TKCustomDBGrid.@link(TKCustomDBGrid.DBOptions) for details. }
    property DBOptions;
    { See TKCustomGrid.@link(TKCustomGrid.ColCount) for details. }
    property ColCount;
    { See TKCustomGrid.@link(TKCustomGrid.Color) for details. }
    property Color;
    { See TKCustomGrid.@link(TKCustomGrid.Colors) for details. }
    property Colors;
    { Inherited property - see Delphi help. }
    property Constraints;
  {$IFDEF FPC}
    { See TKCustomGrid.@link(TKCustomGrid.Flat) for details. }
    property Flat;
  {$ELSE}
    { Inherited property - see Delphi help. }
    property Ctl3D;
  {$ENDIF}
    { See TKCustomDBGrid.@link(TKCustomDBGrid.DataSource) for details. }
    property DataSource;
    { See TKCustomGrid.@link(TKCustomGrid.DefaultColWidth) for details. }
    property DefaultColWidth;
    { See TKCustomGrid.@link(TKCustomGrid.DefaultDrawing) for details. }
    property DefaultDrawing;
    { See TKCustomGrid.@link(TKCustomGrid.DefaultRowHeight) for details. }
    property DefaultRowHeight;
    { See TKCustomGrid.@link(TKCustomGrid.DisabledDrawStyle) for details. }
    property DisabledDrawStyle;
    { Inherited property - see Delphi help. }
    property DragCursor;
    { Inherited property - see Delphi help. }
    property DragKind;
    { Inherited property - see Delphi help. }
    property DragMode;
    { See TKCustomGrid.@link(TKCustomGrid.DragStyle) for details. }
    property DragStyle;
    { Inherited property - see Delphi help. }
    property Enabled;
    { See TKCustomGrid.@link(TKCustomGrid.FixedCols) for details. }
    property FixedCols;
    { See TKCustomGrid.@link(TKCustomGrid.FixedRows) for details. }
    property FixedRows;
    { Inherited property - see Delphi help. }
    property Font;
    { See TKCustomGrid.@link(TKCustomGrid.GridLineWidth) for details. }
    property GridLineWidth;
    { See TKCustomGrid.@link(TKCustomGrid.MinColWidth) for details. }
    property MinColWidth;
    { See TKCustomGrid.@link(TKCustomGrid.MinRowHeight) for details. }
    property MinRowHeight;
    { See TKCustomGrid.@link(TKCustomGrid.MouseCellHintTime) for details. }
    property MouseCellHintTime;
    { See TKCustomGrid.@link(TKCustomGrid.MoveDirection) for details. }
    property MoveDirection;
    { See TKCustomGrid.@link(TKCustomGrid.Options) for details. }
    property Options;
    { Inherited property - see Delphi help. }
    property ParentColor;
    { Inherited property - see Delphi help. }
    property ParentFont;
    { Inherited property - see Delphi help. }
    property ParentShowHint;
    { Inherited property - see Delphi help. }
    property PopupMenu;
    { See TKCustomGrid.@link(TKCustomGrid.RangeSelectStyle) for details. }
    property RangeSelectStyle;
    { See TKCustomGrid.@link(TKCustomGrid.RowCount) for details. }
    property RowCount;
    { See TKCustomGrid.@link(TKCustomGrid.ScrollBars) for details. }
    property ScrollBars;
    { See TKCustomGrid.@link(TKCustomGrid.ScrollModeHorz) for details. }
    property ScrollModeHorz;
    { See TKCustomGrid.@link(TKCustomGrid.ScrollModeVert) for details. }
    property ScrollModeVert;
    { See TKCustomGrid.@link(TKCustomGrid.ScrollSpeed) for details. }
    property ScrollSpeed;
    { Inherited property - see Delphi help. }
    property ShowHint;
    { See TKCustomGrid.@link(TKCustomGrid.SizingStyle) for details. }
    property SizingStyle;
    { See TKCustomGrid.@link(TKCustomGrid.SortStyle) for details. }
    property SortStyle;
    { Inherited property - see Delphi help. }
    property TabOrder;
    { Inherited property - see Delphi help. }
    property TabStop default True;
    { Inherited property - see Delphi help. }
    property Visible;
    { See TKCustomGrid.@link(TKCustomGrid.OnBeginColDrag) for details. }
    property OnBeginColDrag;
    { See TKCustomGrid.@link(TKCustomGrid.OnBeginColSizing) for details. }
    property OnBeginColSizing;
    { See TKCustomGrid.@link(TKCustomGrid.OnBeginRowSizing) for details. }
    property OnBeginRowSizing;
    { See TKCustomGrid.@link(TKCustomGrid.OnCellSpan) for details. }
    property OnCellSpan;
    { See TKCustomGrid.@link(TKCustomGrid.OnChanged) for details. }
    property OnChanged;
    { See TKCustomGrid.@link(TKCustomGrid.OnCheckColDrag) for details. }
    property OnCheckColDrag;
    { Inherited property - see Delphi help. }
    property OnClick;
    { See TKCustomGrid.@link(TKCustomGrid.OnColumnMoved) for details. }
    property OnColumnMoved;
    { See TKCustomGrid.@link(TKCustomGrid.OnColWidthsChanged) for details. }
    property OnColWidthsChanged;
    { Inherited property - see Delphi help. }
    property OnContextPopup;
    { See TKCustomGrid.@link(TKCustomGrid.OnCustomSortCols) for details. }
    property OnCustomSortCols;
    { See TKCustomGrid.@link(TKCustomGrid.OnCustomSortRows) for details. }
    property OnCustomSortRows;
    { Inherited property - see Delphi help. }
    property OnDblClick;
    { Inherited property - see Delphi help. }
    property OnDockDrop;
    { Inherited property - see Delphi help. }
    property OnDockOver;
    { Inherited property - see Delphi help. }
    property OnDragDrop;
    { Inherited property - see Delphi help. }
    property OnDragOver;
    { See TKCustomGrid.@link(TKCustomGrid.OnDrawCell) for details. }
    property OnDrawCell;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorCreate) for details. }
    property OnEditorCreate;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorDataFromGrid) for details. }
    property OnEditorDataFromGrid;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorDataToGrid) for details. }
    property OnEditorDataToGrid;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorDestroy) for details. }
    property OnEditorDestroy;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorKeyPreview) for details. }
    property OnEditorKeyPreview;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorResize) for details. }
    property OnEditorResize;
    { See TKCustomGrid.@link(TKCustomGrid.OnEditorSelect) for details. }
    property OnEditorSelect;
    { See TKCustomGrid.@link(TKCustomGrid.OnEndColDrag) for details. }
    property OnEndColDrag;
    { See TKCustomGrid.@link(TKCustomGrid.OnEndColSizing) for details. }
    property OnEndColSizing;
    { Inherited property - see Delphi help. }
    property OnEndDock;
    { Inherited property - see Delphi help. }
    property OnEndDrag;
    { See TKCustomGrid.@link(TKCustomGrid.OnEndRowSizing) for details. }
    property OnEndRowSizing;
    { Inherited property - see Delphi help. }
    property OnEnter;
    { Inherited property - see Delphi help. }
    property OnExit;
    { See TKCustomGrid.@link(TKCustomGrid.OnExchangeCols) for details. }
    property OnExchangeCols;
    { See TKCustomGrid.@link(TKCustomGrid.OnExchangeRows) for details. }
    property OnExchangeRows;
    { Inherited property - see Delphi help. }
    property OnGetSiteInfo;
    { Inherited property - see Delphi help. }
    property OnKeyDown;
    { Inherited property - see Delphi help. }
    property OnKeyPress;
    { Inherited property - see Delphi help. }
    property OnKeyUp;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseCellHint) for details. }
    property OnMouseCellHint;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseClickCell) for details. }
    property OnMouseClickCell;
    { Inherited property - see Delphi help. }
    property OnMouseDown;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseEnterCell) for details. }
    property OnMouseEnterCell;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseLeaveCell) for details. }
    property OnMouseLeaveCell;
    { Inherited property - see Delphi help. }
    property OnMouseMove;
    { Inherited property - see Delphi help. }
    property OnMouseUp;
    { Inherited property - see Delphi help. }
    property OnMouseWheel;
    { Inherited property - see Delphi help. }
    property OnMouseWheelDown;
    { Inherited property - see Delphi help. }
    property OnMouseWheelUp;
    { Inherited property - see Delphi help. }
    property OnResize;
    { See TKCustomGrid.@link(TKCustomGrid.OnRowHeightsChanged) for details. }
    property OnRowHeightsChanged;
    { See TKCustomGrid.@link(TKCustomGrid.OnSelectCell) for details. }
    property OnSelectCell;
    { See TKCustomGrid.@link(TKCustomGrid.OnSelectionExpand) for details. }
    property OnSelectionExpand;
    { See TKCustomGrid.@link(TKCustomGrid.OnSizeChanged) for details. }
    property OnSizeChanged;
    { Inherited property - see Delphi help. }
    property OnStartDock;
    { Inherited property - see Delphi help. }
    property OnStartDrag;
    { See TKCustomGrid.@link(TKCustomGrid.OnTopLeftChanged) for details. }
    property OnTopLeftChanged;
    { Inherited property - see Delphi help. }
    property OnUnDock;
  end;

implementation

uses
  Math, Types, ComCtrls, StdCtrls
{$IFDEF FPC}
  , EditBtn
{$ENDIF}
  ;

{ TKDBGridDataLink }

constructor TKDBGridDataLink.Create(AGrid: TKCustomDBGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FModified := False;
  VisualControl := True;
end;

procedure TKDBGridDataLink.ActiveChanged;
begin
  inherited;
  if Assigned(FGrid) then
    FGrid.DataChanged;
  FModified := False;
end;

procedure TKDBGridDataLink.DataSetChanged;
begin
  inherited;
  if Assigned(FGrid) then
    FGrid.DataChanged;
  FModified := False;
end;

procedure TKDBGridDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;
  if Assigned(FGrid) then
    FGrid.DataSetScrolled;
end;

procedure TKDBGridDataLink.LayoutChanged;
begin
  inherited;
  if Assigned(FGrid) then
    FGrid.DataChanged;
  FModified := False;
end;

procedure TKDBGridDataLink.RecordChanged;
begin
  inherited;
  if Assigned(FGrid) and not FGrid.Flag(cGF_EditorUpdating or cGF_DBDataUpdating) then
  begin
    FGrid.RecordChanged;
    FModified := False;
  end;
end;

procedure TKDBGridDataLink.SetModified(const Value: Boolean);
begin
  FModified := FModified or Value;
end;

procedure TKDBGridDataLink.UpdateData;
begin
  if FModified and Assigned(FGrid) then
    FGrid.UpdateData;
  FModified := False;
end;

{ TKGridDBCell }

constructor TKDBGridCell.Create(AGrid: TKCustomGrid);
begin
  FGraphic := nil;
  inherited;
end;

procedure TKDBGridCell.ApplyDrawProperties;
var
  ACol: TKDBGridCol;
begin
  inherited;
  Grid.CellPainter.Graphic := FGraphic;
  if not (gdFixed in Grid.CellPainter.State) and (Grid.Cols[Grid.CellPainter.Col] is TKDBGridCol) then
  begin
    ACol := TKDBGridCol(Grid.Cols[Grid.CellPainter.Col]);
    case ACol.DataType of
      ftBoolean:
        Grid.CellPainter.Text := '';
      ftCurrency, ftBcd:
        Grid.CellPainter.Text := FormatCurrency(StrToCurrDef(Grid.CellPainter.Text, 0), ACol.CurrencyFormat);
    end;
  end;
end;

procedure TKDBGridCell.BeforeUpdate;
var
  ACol, ARow: Integer;
begin
  inherited;
  if (Grid is TKDBGrid) and not Grid.Flag(cGF_EditorUpdating or cGF_DBDataUpdating)
    and FindCell(ACol, ARow) then
    TKDBGrid(Grid).BeforeCellUpdate(ACol, ARow);
end;

function TKDBGridCell.CreateImageByType(const Header: TKImageHeaderString): TGraphic;
begin
  Result := ImageByType(Header);
end;

procedure TKDBGridCell.FieldFromCell(AField: TField);
begin
  if AField <> nil then
  begin
    if AField.DataType in cDefaultStringSet then
      TextToField(AField)
    else if (AField.DataType in cDefaultImageSet) and
      (dboImagesWritable in TKCustomDBGrid(Grid).DBOptions) then
      ImageToField(AField);
    // else - override TKDBGridCell
  end;
end;

procedure TKDBGridCell.FieldToCell(AField: TField);
begin
  if AField <> nil then
  begin
    if AField.DataType in cDefaultStringSet then
    begin
      FreeAndNil(FGraphic);
      TextFromField(AField);
    end
    else if AField.DataType in cDefaultImageSet then
    begin
      Text := '';
      ImageFromField(AField);
    end;
    // else - override TKDBGridCell
  end;
end;

procedure TKDBGridCell.ImageFromField(AField: TField);
var
  MS: TMemoryStream;
  S: AnsiString;
begin
  if AField is TBlobField then
  begin
    FreeAndNil(FGraphic);
    MS := TMemoryStream.Create;
    try
      TBlobField(AField).SaveToStream(MS);
      if MS.Size > SizeOf(TKImageHeaderString) then
      begin
        MS.Seek(0, soFromBeginning);
        SetLength(S, SizeOf(TKImageHeaderString));
        MS.Read(S[1], SizeOf(TKImageHeaderString));
        FGraphic := CreateImageByType(S);
        if Assigned(FGraphic) then
        begin
          MS.Seek(0, soFromBeginning);
          FGraphic.LoadFromStream(MS);
        end;
      end;
    finally
      MS.Free;
    end;
  end;
end;

procedure TKDBGridCell.ImageToField(AField: TField);
var
  MS: TMemoryStream;
begin
  if (AField is TBlobField) and Assigned(FGraphic) then
  begin
    MS := TMemoryStream.Create;
    try
      FGraphic.SaveToStream(MS);
      MS.Seek(0, soFromBeginning);
      TBlobField(AField).LoadFromStream(MS);
    finally
      MS.Free;
    end;
  end;
end;

procedure TKDBGridCell.Initialize;
begin
  inherited;
  FreeAndNil(FGraphic);
end;

procedure TKDBGridCell.TextFromField(AField: TField);
begin
{$IFDEF STRING_IS_UNICODE}
  Text := AField.AsString
{$ELSE}
 {$IFDEF COMPILER10_UP}
  Text := AField.AsWideString
 {$ELSE}
  Text := AField.AsString
 {$ENDIF}
{$ENDIF}
end;

procedure TKDBGridCell.TextToField(AField: TField);
begin
  if not AField.ReadOnly then
  try
  {$IFDEF STRING_IS_UNICODE}
    AField.AsString := Text
  {$ELSE}
   {$IFDEF COMPILER10_UP}
    AField.AsWideString := Text
   {$ELSE}
    AField.AsString := Text
   {$ENDIF}
  {$ENDIF}
  except
  end
end;

{ TKDBGridCol }

constructor TKDBGridCol.Create(AGrid: TKCustomGrid);
begin
  inherited;
  FCurrencyFormat.CurrencyFormat := SysUtils.CurrencyFormat;
  FCurrencyFormat.CurrencyDecimals := SysUtils.CurrencyDecimals;
  FCurrencyFormat.CurrencyString := SysUtils.CurrencyString;
  FCurrencyFormat.DecimalSep := SysUtils.DecimalSeparator;
  FCurrencyFormat.ThousandSep := SysUtils.ThousandSeparator;
  FCurrencyFormat.UseThousandSep := True;
  FDataType := ftUnknown;
  FName := '';
end;

{ TKDBGridCellPainter }

procedure TKDBGridCellPainter.DefaultAttributes;
begin
  inherited;
  if Assigned(TKCustomDBGrid(Grid).FDataLink) then
  begin
    if (dboIndicateActiveRecord in TKCustomDBGrid(Grid).DBOptions) and
      Assigned(TKCustomDBGrid(Grid).FDataLink.DataSet) and
      (TKCustomDBGrid(Grid).FDataLink.ActiveRecord = Row - Grid.FixedRows) and
      (State * [gdSelected, gdFocused] = []) then
      Canvas.Brush.Color := TKDBGridColors(TKCustomDBGrid(Grid).Colors).ActiveRecord;
    if (dboIndexFixedCol in TKCustomDBGrid(Grid).DBOptions) and (Col = 0) and (Grid.FixedCols > 0) then
      HAlign := halRight;
    if not (gdFixed in State) and (Grid.Cols[Col] is TKDBGridCol) then
      case TKDBGridCol(Grid.Cols[Col]).DataType of
        ftMemo
      {$IF DEFINED(FPC) OR DEFINED(COMPILER10_UP)}
        , ftWideMemo
      {$IFEND}
          :
          Attributes := Attributes + [taLineBreak];
        ftCurrency, ftBCD, ftFmtBCD:
          HAlign := halRight;
        ftBoolean:
        begin
          CheckBox := True;
          CheckBoxChecked := LowerCase(Grid.Cells[Col, Row]) = 'true';
        end;
      end;
  end;
end;

{ TKDBGridColors }

function TKDBGridColors.GetColor(Index: TKGridColorIndex): TColor;
begin
  Result := InternalGetColor(Index);
end;

procedure TKDBGridColors.Initialize;
begin
  inherited;
  SetLength(FColors, ciDBGridColorsMax + 1);
  SetLength(FBrightColors, ciDBGridColorsMax + 1);
  FColors[ciActiveRecord] := cActiveRecordDef;
end;

procedure TKDBGridColors.SetColor(Index: TKGridColorIndex; Value: TColor);
begin
  InternalSetColor(Index, Value);
end;

{ TKCustomDBGrid }

constructor TKCustomDBGrid.Create(AOwner: TComponent);
begin
  FDataLink := TKDBGridDataLink.Create(Self);
  inherited;
  FActiveRecord := -1;
  FDBOptions := cDBOptionsDef;
  FColors.Free;
  FColors := TKDBGridColors.Create(Self);
  CellClass := TKDBGridCell;
  CellPainterClass := TKDBGridCellPainter;
  ColClass := TKDBGridCol;
  RealizeColClass;
end;

destructor TKCustomDBGrid.Destroy;
begin
  inherited;
  FDataLink.Free;
end;

function TKCustomDBGrid.BeginRowDrag(var Origin: Integer;
  const MousePt: TPoint): Boolean;
begin
  // does nothing
  Result := False;
end;

procedure TKCustomDBGrid.BeforeCellUpdate(ACol, ARow: Integer);
begin
  if FDataLink.Active and not FDataLink.ReadOnly then
  begin
    InternalSetActiveRecord(ARow - FixedRows);
    FDataLink.Edit;
    FDataLink.Modified := True;
  end;
end;

procedure TKCustomDBGrid.Changed;
begin
  inherited;
  FDataLink.Edit;
  FDataLink.Modified := True;
end;

procedure TKCustomDBGrid.ClearCol(ACol: Integer);
begin
  // does nothing
end;

procedure TKCustomDBGrid.ClearGrid;
begin
  // does nothing
end;

procedure TKCustomDBGrid.ClearRow(ARow: Integer);
begin
  // does nothing
end;

procedure TKCustomDBGrid.ColMoved(FromIndex, ToIndex: Integer);
begin
  inherited;
  DataChanged;
end;

procedure TKCustomDBGrid.Commit;
begin
  if Assigned(FDataLink.DataSet) and FDataLink.Modified then
    FDataLink.DataSet.Post;
end;

function TKCustomDBGrid.CustomSortRows(ByCol: Integer; var SortMode: TKGridSortMode): Boolean;
begin
  if Assigned(FDataLink.DataSet) and FDataLink.Active then
  begin
    Commit;
    Result := inherited CustomSortRows(ByCol, SortMode);
    if Result then
      ClearSortModeVert
    else
      SortMode := smNone;
  end else
  begin
    ClearSortModeHorz;
    Result := False;
  end;
end;

procedure TKCustomDBGrid.DataChanged;
var
  I, Index, J, Tmp, LastRow: Integer;
  S: WideString;
  ADataType: TFieldType;
  Cell: TKGridCell;
begin
  if Assigned(FDataLink.DataSet) and not Flag(cGF_DBDataUpdating) then
  begin
    FlagSet(cGF_DBDataUpdating);
    try
      if FDataLink.Active then
      begin
        RowCount := FixedRows + FDataLink.DataSet.RecordCount;
        if FixedCols + FDataLink.DataSet.FieldCount <> ColCount then
        begin
          ClearSortMode;
          ColCount := FixedCols + FDataLink.DataSet.FieldCount;
          for I := 0 to ColCount - 1 do
            Cols[I].InitialPos := I;
        end;
        if FDataLink.DataSet.RecNo >= 1 then
        begin
          Tmp := FixedRows + FDataLink.DataSet.RecNo - 1;
          if not Flag(cGF_DBInternalChanging) and (Row <> Tmp) then
          begin
            if dboAutoMoveRecord in FDBOptions then
              Row := Tmp
            else
              EditorMode := False;
          end;
        end;
        LastRow := Min(LastVisibleRow + 1, RowCount - 1);
        // here memory only grows. I don't know if it is possible to make this more memory effective
        FDataLink.BufferCount := Max(FDataLink.BufferCount, Max(LastRow, FDataLink.DataSet.RecNo - 1) + 1);
        if (dboIndexFixedCol in FDBOptions) and (FixedCols > 0) then
        begin
          Cell := InternalGetCell(0, 0);
          if Cell is TKDBGridCell then
            TKDBGridCell(Cell).Text := SKDBGridIndex;
        end;
        Tmp := FDataLink.ActiveRecord;
        try
          for I := FixedCols to ColCount - 1 do
          begin
            Index := Cols[I].InitialPos;
            if Index < ColCount then
            begin
              S := FDataLink.DataSet.FieldDefs[Index - FixedCols].Name;
              ADataType := FDataLink.DataSet.FieldDefs[Index - FixedCols].DataType;
              if Cols[I] is TKDBGridCol then
              begin
                TKDBGridCol(Cols[I]).FName := S;
                TKDBGridCol(Cols[I]).FDataType := ADataType;
              end;
              if dboColNamesToHeader in FDBOptions then
              begin
                Cell := InternalGetCell(I, 0);
                if Cell is TKDBGridCell then
                  TKDBGridCell(Cell).Text := S;
              end;
              if (dboAutoSizeBooleanCells in FDBOptions) and (ADataType = ftBoolean) then
              begin
                ColWidths[I] := cCheckBoxFrameSize + CellPainter.HPadding * 2;
                Cols[I].CanResize := False;
              end;
            end;
          end;
          for J := TopRow to LastRow do
          begin
            FDataLink.ActiveRecord := J - FixedRows;
            if (FDataLink.ActiveRecord <> Tmp) or not FDataLink.Modified then
              for I := FixedCols to ColCount - 1 do
              begin
                Index := Cols[I].InitialPos;
                if Index < ColCount then
                begin
                  Cell := InternalGetCell(I, J);
                  if Cell is TKDBGridCell then
                  begin
                    TKDBGridCell(Cell).FieldToCell(FDataLink.DataSet.Fields[Index - FixedCols]);
                    if Assigned(TKDBGridCell(Cell).Graphic) then
                    begin
                      if dboAutoSizeImageCells in FDBOptions then
                      begin
                        if ColWidths[I] > 0 then
                          ColWidths[I] := Max(ColWidths[I], TKDBGridCell(Cell).Graphic.Width + CellPainter.GraphicHPadding * 2);
                        if RowHeights[J] > 0 then
                          RowHeights[J] := Max(RowHeights[J], TKDBGridCell(Cell).Graphic.Height + CellPainter.GraphicVPadding * 2);
                      end;
                      if dboImageHint in FDBOptions then
                        Cols[I].CellHint := True;
                    end;
                  end;
                end;
              end;
            if (dboIndexFixedCol in FDBOptions) and (FixedCols > 0) then
            begin
              Cell := InternalGetCell(0, J);
              if Cell is TKDBGridCell then
              begin
                TKDBGridCell(Cell).Text := IntToStr(J - FixedRows + 1);
                if Cell is TKGridAttrTextCell then
                  TKGridAttrTextCell(Cell).HAlign := halRight;
              end;
            end;
          end;
        finally
          FDataLink.ActiveRecord := Tmp;
        end;
        if dboIndicateActiveRecord in FDBOptions then
        begin
          if FDataLink.ActiveRecord <> FActiveRecord then
          begin
            if FActiveRecord >= 0 then
              InvalidateRow(FActiveRecord + FixedRows);
            FActiveRecord := FDataLink.ActiveRecord;
            InvalidateRow(FActiveRecord + FixedRows);
          end;
        end;
      end else
      begin
        RowCount := FixedRows + 1;
        FMaxRow := FixedRows;
        if dboDontClearFixedCells in FDBOptions then Tmp := FixedRows else Tmp := 0;
        for I := 0 to ColCount - 1 do
        begin
          Cols[I].InitialPos := I;
          if Cols[I] is TKDBGridCol then
          begin
            TKDBGridCol(Cols[I]).FName := '';
            TKDBGridCol(Cols[I]).FDataType := ftUnknown;
          end;
          if not (dboDontClearFixedCells in FDBOptions) or (I >= FixedCols) then
          begin
            for J := Tmp to RowCount - 1 do
            begin
              Cell := InternalGetCell(I, J);
              if Cell is TKDBGridCell then
                TKDBGridCell(Cell).Clear;
            end;
          end;
        end;
        ClearSortMode;
        FActiveRecord := -1;
      end;
    finally
      FlagClear(cGF_DBDataUpdating);
    end;
  end;
end;

procedure TKCustomDBGrid.DataSetScrolled;
begin
  DataChanged;
end;

procedure TKCustomDBGrid.DefaultEditorCreate(ACol, ARow: Integer; var AEditor: TWinControl);
begin
  // create custom editors according to table column type
  if Cols[ACol] is TKDBGridCol then
    case TKDBGridCol(Cols[ACol]).DataType of
      ftString, ftWideString, ftInteger, ftSmallInt, ftWord, ftLargeInt, ftFloat, ftCurrency, ftBcd:
      begin
        AEditor := TEdit.Create(nil);
      end;
      ftMemo
    {$IF DEFINED(FPC) OR DEFINED(COMPILER10_UP)}
      , ftWideMemo
    {$IFEND}
        :
      begin
        AEditor := TMemo.Create(nil);
      end;
      ftDate, ftTime, ftDateTime:
      begin
        AEditor := {$IFDEF FPC}TDateEdit{$ELSE}TDateTimePicker{$ENDIF}.Create(nil);
      end;
      ftBoolean:
      begin
        AEditor := TCheckBox.Create(nil);
      end;
    else
      AEditor := nil;
    end
  else
    AEditor := nil;
end;

procedure TKCustomDBGrid.DefaultEditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean);
begin
  if Cols[ACol] is TKDBGridCol then
    case TKDBGridCol(Cols[ACol]).DataType of
      ftDate, ftTime, ftDateTime:
        if AEditor is {$IFDEF FPC}TDateEdit{$ELSE}TDateTimePicker{$ENDIF} then
        begin
        {$IFDEF FPC}
          TDateEdit(AEditor).Date :=
        {$ELSE}
          TDateTimePicker(AEditor).DateTime :=
        {$ENDIF}
            StrToDateTime(Cells[ACol, ARow]);
          AssignText := False;
        end;
      ftCurrency, ftBcd:
        if AEditor is TEdit then
        begin
          TEdit(AEditor).Text := CurrToStrF(StrToCurrDef(Cells[ACol, ARow], 0),
            ffFixed, TKDBGridCol(Cols[ACol]).CurrencyFormat.CurrencyDecimals);
          AssignText := False;
        end;
      ftBoolean:
        if AEditor is TCheckBox then
        begin
          TCheckBox(AEditor).Checked := LowerCase(Cells[ACol, ARow]) = 'true';
          AssignText := False;
        end;
    end;
end;

procedure TKCustomDBGrid.DefaultEditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean);
var
  I: Int64;
  ADataType: TFieldType;
begin
  if Cols[ACol] is TKDBGridCol then
  begin
    ADataType := TKDBGridCol(Cols[ACol]).DataType;
    case ADataType of
      ftDate, ftTime, ftDateTime:
        if AEditor is {$IFDEF FPC}TDateEdit{$ELSE}TDateTimePicker{$ENDIF} then
        begin
          Cells[ACol, ARow] := DateTimeToStr(
          {$IFDEF FPC}
            TDateEdit(AEditor).Date);
          {$ELSE}
            TDateTimePicker(AEditor).DateTime);
          {$ENDIF}
          AssignText := False;
        end;
      ftLargeInt, ftInteger, ftSmallInt, ftWord:
        if AEditor is TEdit then
        begin
          I := StrToInt64Def(TEdit(AEditor).Text, 0);
          case ADataType of
            ftInteger: I := MinMax(I, -MaxInt - 1, MaxInt);
            ftSmallInt: I := MinMax(I, -32768, 32767);
            ftWord: I := MinMax(I, 0, 65535);
          end;
          Cells[ACol, ARow] := IntToStr(I);
          AssignText := False;
        end;
      ftFloat:
        if AEditor is TEdit then
        begin
          Cells[ACol, ARow] := FloatToStr(StrToFloatDef(TEdit(AEditor).Text, 0));
          AssignText := False;
        end;
      ftCurrency, ftBcd:
        if AEditor is TEdit then
        begin
          Cells[ACol, ARow] := CurrToStrF(StrToCurrDef(TEdit(AEditor).Text, 0),
            ffFixed, TKDBGridCol(Cols[ACol]).CurrencyFormat.CurrencyDecimals);
          AssignText := False;
        end;
      ftBoolean:
        if AEditor is TCheckBox then
        begin
          if TCheckBox(AEditor).Checked then
            Cells[ACol, ARow] := 'True'
          else
            Cells[ACol, ARow] := 'False';
          AssignText := False;
        end;
    end;
  end;
end;

procedure TKCustomDBGrid.DefaultEditorResize(AEditor: TWinControl; ACol, ARow: Integer;
  var ARect: TRect);
begin
  if Cols[ACol] is TKDBGridCol then
    case TKDBGridCol(Cols[ACol]).DataType of
      ftBoolean:
      {$IFNDEF LCLGTK2}
        if AEditor is TCheckBox then
          Inc(ARect.Left, 2);
      {$ENDIF}
    end;
end;

procedure TKCustomDBGrid.DefaultEditorSelect(AEditor: TWinControl;
  ACol, ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean);
begin
  inherited;
  if Cols[ACol] is TKDBGridCol then
    case TKDBGridCol(Cols[ACol]).DataType of
      ftBoolean:
        if (AEditor is TCheckBox) and SelectedByMouse then
          ThroughClick := True;
    end;
end;

procedure TKCustomDBGrid.DefaultMouseCellHint(ACol, ARow: Integer;
  AShow: Boolean);
var
  R: TRect;
  Extent: TPoint;
  ACell: TKGridCell;
  AGraphic: TGraphic;
begin
  if ColValid(ACol) and Cols[ACol].CellHint then
  begin
    ACell := Cell[ACol, ARow];
    if ACell is TKDBGridCell then
    begin
      AGraphic := TKDBGridCell(ACell).Graphic;
      if AGraphic <> nil then
      begin
        if AShow then
        begin
          if (ARow >= FixedRows) and ((ARow <> FEditorCell.Row) or (ACol <> FEditorCell.Col) or not EditorMode) and
            CellRect(ACol, ARow, R, True) then
          begin
            Extent := MeasureCell(ACol, ARow, R, GetDrawState(ACol, ARow, HasFocus), mpCellExtent);
            if (Extent.X > R.Right - R.Left) or (Extent.Y > R.Bottom - R.Top) then
            begin
              FreeAndNil(FHint);
              FHint := TKGraphicHint.Create(nil);
              TKGraphicHint(FHint).Graphic := AGraphic;
              Inc(R.Left, 10);
              Inc(R.Top, 10);
              FHint.ShowAt(ClientToScreen(R.TopLeft));
            end;
          end;
        end else
          FreeAndNil(FHint);
      end else
        inherited;
    end else
      inherited;
  end else
    FreeAndNil(FHint);
end;

procedure TKCustomDBGrid.DeleteCols(At, Count: Integer);
begin
  // does nothing
end;

procedure TKCustomDBGrid.DeleteRow(At: Integer);
begin
  if Assigned(FDataLink.DataSet) and RowValid(At) then
  begin
    InternalSetActiveRecord(At - FixedRows);
    FDataLink.DataSet.Delete;
  end;
end;

procedure TKCustomDBGrid.DeleteRows(At, Count: Integer);
begin
  // does nothing
end;

function TKCustomDBGrid.EditorCreate(ACol, ARow: Integer): TWinControl;
begin
  if Assigned(FDataLink.DataSet) and FDataLink.Active and
    not FDataLink.ReadOnly and (FDataLink.ActiveRecord = ARow - FixedRows) then
    Result := inherited EditorCreate(ACol, ARow)
  else
    Result := nil;
end;

function TKCustomDBGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TKCustomDBGrid.InsertCols(At, Count: Integer);
begin
  // does nothing
end;

procedure TKCustomDBGrid.InsertRow(At: Integer);
begin
  if Assigned(FDataLink.DataSet) and RowValid(At) then
  begin
    InternalSetActiveRecord(At - FixedRows);
    FDataLink.DataSet.Insert;
  end;
end;

procedure TKCustomDBGrid.InsertRows(At, Count: Integer);
begin
  // does nothing
end;

function TKCustomDBGrid.InsertSortedCol(out ByRow, ACol: Integer): Boolean;
begin
  // does nothing
  Result := False;
end;

function TKCustomDBGrid.InsertSortedRow(out ByCol, ARow: Integer): Boolean;
begin
  // does nothing
  Result := False;
end;

procedure TKCustomDBGrid.InternalSetActiveRecord(Value: Integer);
var
  IsEditorMode, IsEditorModeActive: Boolean;
begin
  if Assigned(FDataLink.DataSet) and (Value <> FDataLink.ActiveRecord) and
    not Flag(cGF_EditorUpdating or cGF_DBInternalChanging) then
  begin
    FlagSet(cGF_DBInternalChanging);
    try
      IsEditorMode := EditorMode;
      IsEditorModeActive := Flag(cGF_EditorModeActive);
      EditorMode := False;
      Commit;
      FDataLink.MoveBy(Value - FDataLink.ActiveRecord);
      EditorMode := IsEditorMode;
      if IsEditorModeActive then FlagSet(cGF_EditorModeActive);
    finally
      FlagClear(cGF_DBInternalChanging);
    end;
  end;
end;

procedure TKCustomDBGrid.InternalSetColCount(Value: Integer);
begin
  if not FDataLink.Active or Flag(cGF_DBDataUpdating) then
    inherited;
end;

procedure TKCustomDBGrid.InternalSetFixedCols(Value: Integer);
begin
  if not FDataLink.Active and not Flag(cGF_DBDataUpdating) then
  begin
    FlagSet(cGF_DBDataUpdating);
    try
      inherited;
    finally
      FlagClear(cGF_DBDataUpdating);
    end;
  end;
end;

procedure TKCustomDBGrid.InternalSetFixedRows(Value: Integer);
begin
  if not FDataLink.Active and not Flag(cGF_DBDataUpdating) then
  begin
    FlagSet(cGF_DBDataUpdating);
    try
      inherited;
    finally
      FlagClear(cGF_DBDataUpdating);
    end;
  end;
end;

procedure TKCustomDBGrid.InternalSetRowCount(Value: Integer);
begin
  if not FDataLink.Active or Flag(cGF_DBDataUpdating) then
    inherited;
end;

function TKCustomDBGrid.InternalUpdateVirtualGrid: Boolean;
begin
  Result := False;
end;

procedure TKCustomDBGrid.MoveRow(FromIndex, ToIndex: Integer);
begin
  // does nothing
end;

procedure TKCustomDBGrid.RecordChanged;
var
  ARow, I, Index: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FDataLink.DataSet) and not Flag(cGF_DBDataUpdating) then
  begin
    FlagSet(cGF_DBDataUpdating);
    try
      ARow := FDataLink.ActiveRecord + FixedRows;
      if Assigned(FDataLink.DataSet) and (ARow < RowCount) then
      begin
        for I := FixedCols to ColCount - 1 do
        begin
          Index := Cols[I].InitialPos;
          Cell := InternalGetCell(I, ARow);
          if Cell is TKDBGridCell then
            TKDBGridCell(Cell).FieldToCell(FDataLink.DataSet.Fields[Index - FixedCols]);
        end;
      end;
    finally
      FlagClear(cGF_DBDataUpdating);
    end;
  end;
end;

function TKCustomDBGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  if Result and (dboAutoMoveRecord in FDBOptions) then
    InternalSetActiveRecord(ARow - FixedRows);
end;

procedure TKCustomDBGrid.SetDataSource(Value: TDataSource);
begin
  if Assigned(FDataLink.DataSource) then FDataLink.DataSource.FreeNotification(Self);
  FDataLink.DataSource := Value;
end;

procedure TKCustomDBGrid.SetDBOptions(const Value: TKDBGridOptions);
begin
  if Value <> FDBOptions then
  begin
    FDBOptions := Value;
    DataChanged;
  end;
end;

procedure TKCustomDBGrid.TopLeftChanged;
begin
  inherited;
  DataChanged;
end;

procedure TKCustomDBGrid.UpdateData;
var
  ARow, I, Index: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FDataLink.DataSet) and FDataLink.Modified and not Flag(cGF_DBDataUpdating) then
  begin
    FlagSet(cGF_DBDataUpdating);
    try
      ARow := FDataLink.ActiveRecord + FixedRows;
      for I := FixedCols to ColCount - 1 do
      begin
        Index := Cols[I].InitialPos;
        Cell := InternalGetCell(I, ARow);
        if Cell is TKDBGridCell then
          TKDBGridCell(Cell).FieldFromCell(FDataLink.DataSet.Fields[Index - FixedCols]);
      end;
    finally
      FlagClear(cGF_DBDataUpdating);
    end;
  end;
end;

procedure TKCustomDBGrid.UpdateSize;
begin
  inherited;
  DataChanged;
end;

end.
