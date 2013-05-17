{   Component(s):
    TcyBaseDBGrid

    Description:
    A DBGrid class with
    - revealed herited properties
    - OnSelectCell event
    - mouse wheel handling for records navigation, selecting rows (with Shift key) , do nothing or do as original
    - 2 clicks for massive rows selection (with Shift key)
    - Both scrollbars disabling option
    - CheckBox for each record
    - Auto stretch one column
    - Title custom appearence
    - Memo and graphic embedded display
    - Editor Parser
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

unit cyBaseDBGrid;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Controls, Graphics, Grids, DBGrids, Messages, Db, jpeg, ExtCtrls, StdCtrls, ComCtrls, RichEdit, Forms, StrUtils,
      VCL.cyTypes, VCL.cyGraphics,
       {$IFDEF DELPHI2009_OR_ABOVE}
       pngimage, cyBookmarkList;
       {$ELSE}
       cyBookmarkListD2007;
       {$ENDIF}

// AfterScroll and AfterScrollPause features :
{$IFDEF DELPHI2006_OR_ABOVE}
{$DEFINE SCROLL_FEATURE}
{$ENDIF}

type
  TRecordPosition = Record
    ActiveRecord: Integer;
    Bookmark: TBookmark;
  end;

  TcyBaseDBGrid = class;

  TColumnMode = (cmByIndex, cmByFieldName, cmIndicatorsCol);
  TFieldContentRendering = (fcDefault, fcWordwrap, fcMemo, fcBitmap, fcJpeg, fcIcon, fcPng, fcCheckBox);
  TGraphicClassField = fcBitmap..fcPng;

  // In order to access protected TInplaceEdit properties
  TcyDBGridInplaceEdit = class(TInplaceEdit)
  private
  protected
  published
  public
    property Color;
    property Font;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;

    property BorderStyle;
    property CharCase;
    property HideSelection;
    {$IFDEF DELPHI2009_OR_ABOVE}
    property NumbersOnly;
    {$ENDIF}
    property PasswordChar;
    property OnChange;

    property EditMask;
    property MaskState;
    property MaxLength;
  end;

  TcyEditorOptions = class(TPersistent)
  private
    IsIntegerField: Boolean;
    IsFloatField: Boolean;
    FBaseGrid: TcyBaseDBGrid;
    FParse: Boolean;
    FColor: TColor;
    FCanParse: Boolean;
  public
    constructor Create(AOwner: TComponent); virtual;
    property CanParse: Boolean read FCanParse write FCanParse;
  published
    property Parse: Boolean read FParse write FParse default true;
    property Color: TColor read FColor write FColor default clWindow;
  end;

  // Vertical scroll properties :
  TcyHorzScrollbar = class(TPersistent)
  private
    FBaseGrid: TcyBaseDBGrid;
    FVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  // Vertical scroll properties :
  TVScrollLineMode = (vlDefault, vlSingleRecord);
  TVScrollOption = (voLINEUP, voLINEDOWN, voPAGEUP, voPAGEDOWN, voBOTTOM, voTOP, voTHUMBPOSITION);
  TVScrollOptions = set of TVScrollOption;

  TcyVertScrollbar = class(TPersistent)
  private
    FBaseGrid: TcyBaseDBGrid;
    FVisible: Boolean;
    FScrollOptions: TVScrollOptions;
    FVScrollLineMode: TVScrollLineMode;
    procedure SetVisible(const Value: Boolean);
    procedure SetScrollOptions(const Value: TVScrollOptions);
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property ScrollLineMode: TVScrollLineMode read FVScrollLineMode write FVScrollLineMode default vlDefault;
    property ScrollOptions: TVScrollOptions read FScrollOptions write SetScrollOptions default [voLINEUP, voLINEDOWN, voPAGEUP, voPAGEDOWN, voBOTTOM, voTOP, voTHUMBPOSITION];
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  // Allows auto adjust size of one column with avaible space
  TcyClientColumn = class(TPersistent)
  private
    FBaseGrid: TcyBaseDBGrid;
    FIndex: Integer;
    FFieldName: String;
    FMode: TColumnMode;
    FMinWidth: Integer;
    FEnabled: Boolean;
    procedure SetFieldName(const Value: String);
    procedure SetIndex(const Value: Integer);
    procedure SetMode(const Value: TColumnMode);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default false;
    property FieldName: String read FFieldName write SetFieldName;
    property Index: Integer read FIndex write SetIndex default -1;         // Use -1 for last column
    property MinWidth: Integer read FMinWidth write FMinWidth default 10;
    property Mode: TColumnMode read FMode write SetMode default cmByIndex;
  end;

  // Checkboxes for user record selection like SelectedRows :
  TcyCheckBoxes = class(TPersistent)
  private
    FBaseGrid: TcyBaseDBGrid;
    FSize: Word;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FColumnIndex: Integer;
    FReadOnly: Boolean;
    FMargin: Word;
    FColumnFieldName: String;
    FColumnMode: TColumnMode;
    FAlignLeft: Boolean;
    procedure SetSize(const Value: Word);
    procedure SetVisible(const Value: Boolean);
    procedure SetColumnIndex(const Value: Integer);
    procedure SetMargin(const Value: Word);
    procedure SetColumnFieldName(const Value: String);
    procedure SetColumnMode(const Value: TColumnMode);
    procedure SetAlignLeft(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property AlignLeft: Boolean read FAlignLeft write SetAlignLeft default true;
    property ColumnFieldName: String read FColumnFieldName write SetColumnFieldName;
    property ColumnIndex: Integer read FColumnIndex write SetColumnIndex default 0;
    property ColumnMode: TColumnMode read FColumnMode write SetColumnMode default cmByIndex;
    property Margin: Word read FMargin write SetMargin default 1;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property Size: Word read FSize write SetSize default 16;
    property Visible: Boolean read FVisible write SetVisible default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // Customize layout (row height/col width):
  TcyCustomLayoutOptions = class(TPersistent)
  private
    FGrid: TcyBaseDBGrid;
    FTitleRowHeight: Integer;
    FDataRowHeight: Integer;
    FIndicatorColumnWidth: Integer;
    FActiveDataRowHeight: Integer;
    FGridLineWidth: Integer;
    procedure SetDataRowHeight(const Value: Integer);
    procedure SetTitleRowHeight(const Value: Integer);
    procedure SetIndicatorColumnWidth(const Value: Integer);
    procedure SetActiveDataRowHeight(const Value: Integer);
    procedure SetGridLineWidth(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property DataRowHeight: Integer read FDataRowHeight write SetDataRowHeight default 0;
    property IndicatorColumnWidth: Integer read FIndicatorColumnWidth write SetIndicatorColumnWidth default 0;
    property TitleRowHeight: Integer read FTitleRowHeight write SetTitleRowHeight default 0;
    property ActiveDataRowHeight: Integer read FActiveDataRowHeight write SetActiveDataRowHeight default 0;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
  end;

  // View memo fields directly on the DBGrid :
  TcyContentMemoOptions = class(TPersistent)
  private
    FGrid: TcyBaseDBGrid;
    FWordwrap: Boolean;
    FMaxCars: Word;
    FMaxLines: Word;
    procedure SetWordwrap(const Value: Boolean);
    procedure SetMaxCars(const Value: Word);
    procedure SetMaxLines(const Value: Word);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property MaxCars: Word read FMaxCars write SetMaxCars default 0;
    property MaxLines: Word read FMaxLines write SetMaxLines default 0;
    property Wordwrap: Boolean read FWordwrap write SetWordwrap default true;
  end;

  // View graphic fields directly on the DBGrid :
  TcyContentImageOptions = class(TPersistent)
  private
    FGrid: TcyBaseDBGrid;
    FTransparent: Boolean;
    FStyle: TBgStyle;
    FFieldContent: TGraphicClassField;
    FPosition: TBgPosition;
    procedure SetTransparent(const Value: Boolean);
    procedure SetStyle(const Value: TBgStyle);
    procedure SetFieldContent(const Value: TGraphicClassField);
    procedure SetPosition(const Value: TBgPosition);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property DrawStyle: TBgStyle read FStyle write SetStyle default bgStretchProportional;
    property FieldContent: TGraphicClassField read FFieldContent write SetFieldContent default fcBitmap;
    property Position: TBgPosition read FPosition write SetPosition default bgCentered;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
  end;

  // View Boolean fields with checkBoxes :
  TcyContentCheckBoxOptions = class(TPersistent)
  private
    FGrid: TcyBaseDBGrid;
    FSize: Word;
    FMargin: Word;
    FAlignLeft: Boolean;
    procedure SetSize(const Value: Word);
    procedure SetMargin(const Value: Word);
    procedure SetAlignLeft(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property AlignLeft: Boolean read FAlignLeft write SetAlignLeft default true;
    property Margin: Word read FMargin write SetMargin default 1;
    property Size: Word read FSize write SetSize default 16;
  end;

  // Custom field display :
  TBooleanFieldDisplay = (bfDefault, bfCheckBox);
  TGraphicFieldDisplay = (gfDefault, gfImage);
  TMemoFieldDisplay = (mfDefault, mfMemo);

  TcyContentFieldsRender = class(TPersistent)
  private
    FGrid: TcyBaseDBGrid;
    FBooleanField: TBooleanFieldDisplay;
    FGraphicField: TGraphicFieldDisplay;
    FMemoField: TMemoFieldDisplay;
    procedure SetBooleanField(const Value: TBooleanFieldDisplay);
    procedure SetGraphicField(const Value: TGraphicFieldDisplay);
    procedure SetMemoField(const Value: TMemoFieldDisplay);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property BooleanField: TBooleanFieldDisplay read FBooleanField write SetBooleanField default bfCheckBox;
    property GraphicField: TGraphicFieldDisplay read FGraphicField write SetGraphicField default gfImage;
    property MemoField: TMemoFieldDisplay read FMemoField write SetMemoField default mfMemo;
  end;

  // Access protected properties ..
  TcyCustomGrid = class(TCustomGrid)
  end;

  TdgColumnResizeMode = (crBoth, crResizeOnly, crMoveOnly);
  THotMode = (hmNone, hmCell, hmRow);
  TMouseWheelMode = (mwDoNothing, mwDefault, mwNavigate, mwRowSelect);
  TMouseJob = (mjWait, mjUnavaible, mjPrepareMovingRows, mjMovingRows, mjScrollBarScrolling);
  TMoveRowRendering = (mrLine, mrFrame);
  TDrawCheckBoxEvent = procedure (Sender: TObject; const Rect: TRect; Checked: Boolean) of object;
  TMoveRowBeginEvent = procedure (Sender: TObject; var Accept: Boolean) of object;
  TMoveRowEvent = procedure (Sender: TObject; Position: TGridCoord; var AcceptPosition: Boolean) of object;
  TMoveRowEndEvent = procedure (Sender: TObject; fromPosition, toPosition: TGridCoord) of object;
  TEditorAcceptKeyEvent = procedure (Sender: TObject; Key: Char; var Accept: Boolean) of object;
  TEditorSetFieldText = procedure (Sender: TObject; var Value: String; var Parse: Boolean) of object;
  TShowEditorEvent = procedure (Sender: TObject; Editor: TcyDBGridInplaceEdit) of object;

  TcyBaseDBGrid = class(TDBGrid)
  private
    FSavInplaceEdit: TcyDBGridInplaceEdit;
    FMouseJob: TMouseJob;
    // RichText field embedded viewing variables :
    FRichEdit: TRichEdit;
    FRichTextStream: TStringStream;
    FFormatRange: TFormatRange;
    //
    {$IFDEF SCROLL_FEATURE}
    LastScrollRecNo: Integer;
    FTimerScroll: TTimer;
    {$ENDIF}
    FExtendedAcceptKey: Boolean;  // Allow more control when Key accepted ...
    FIsLoaded: Boolean;
    FOnSelectCell: TSelectCellEvent;
    FMouseWheelMode: TMouseWheelMode;
    FCheckBoxes: TcyCheckBoxes;
    FCheckedList: TcyBookmarkList;
    FAllowDeleteRecord: Boolean;
    FAllowInsertRecord: Boolean;
    FAllowAppendRecord: Boolean;
    FDefaultDrawingCheckBox: Boolean;
    FOnDrawCheckBox: TDrawCheckBoxEvent;
    FOnCheckBoxClick: TNotifyEvent;
    FOnBeforePaint: TNotifyEvent;
    FOnAfterPaint: TNotifyEvent;
    FOnBeforeDrawCell: TDrawCellEvent;
    FOnAfterDrawCell: TDrawCellEvent;
    FClientColumn: TcyClientColumn;
    FdgColumnResizeMode: TdgColumnResizeMode;
    FOnResize: TNotifyEvent;
    FCalculatedTitleRowHeight: Integer;
    FCustomLayoutOptions: TcyCustomLayoutOptions;
    FContentMemoOptions: TcyContentMemoOptions;
    FContentImageOptions: TcyContentImageOptions;
    FOnColWidthsChanged: TNotifyEvent;
    FHorzScrollBar: TcyHorzScrollbar;
    FVertScrollBar: TcyVertScrollbar;
    FContentFieldsRender: TcyContentFieldsRender;
    FContentCheckBoxOptions: TcyContentCheckBoxOptions;
    FHotMode: THotMode;
    FHotColor: TColor;
    FHotFontColor: TColor;
    FMoveRowsColor: TColor;
    FMoveRows: Boolean;
    FOnBeginMoveRows: TMoveRowBeginEvent;
    FOnMoveRows: TMoveRowEvent;
    FOnEndMoveRows: TMoveRowEndEvent;
    FMoveRowRendering: TMoveRowRendering;
    FEditorSetFieldText: TEditorSetFieldText;
    FOnShowEditor: TShowEditorEvent;
    FOnEditorAcceptKey: TEditorAcceptKeyEvent;
    FEditorOptions: TcyEditorOptions;
    FOnScrollBarMouseDown: TMouseEvent;
    FOnScrollBarMouseUp: TMouseEvent;
    {$IFDEF SCROLL_FEATURE}
    FonAfterScroll: TNotifyEvent;
    FScrollPauseMSeconds: Word;
    FonAfterScrollPause: TNotifyEvent;
    {$ENDIF}
    procedure SetCheckBoxes(Value: TcyCheckBoxes);
    procedure SetDefaultDrawingCheckBox(const Value: Boolean);
    function GetSelectedVisibleIndex: Integer;
    procedure SetSelectedVisibleIndex(const Value: Integer);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure SetCustomGridOptions;
    procedure SetdgColumnResizeMode(const Value: TdgColumnResizeMode);
    procedure SetCustomLayoutOptions(const Value: TcyCustomLayoutOptions);
    procedure SetContentMemoOptions(const Value: TcyContentMemoOptions);
    procedure SetContentImageOptions(const Value: TcyContentImageOptions);
    procedure SetContentFieldsRender(const Value: TcyContentFieldsRender);
    procedure SetContentCheckBoxOptions(const Value: TcyContentCheckBoxOptions);
    function GetColumnIndex: Integer;
    procedure SetColumnIndex(const Value: Integer);
    procedure WMNCCalcSize(var Msg: TMessage); message WM_NCCALCSIZE;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    // procedure WMNCLButtonUp(var Msg: TWMNCLButtonUp); message WM_NCLBUTTONUP; // !!! Not working because ScrollBar is part of non-client area
    procedure WMNCHITTEST(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    FMouseOverCell: TGridCoord;
    FMoveHere: TGridCoord;
    FMouseDownCell: TGridCoord;
    procedure ColWidthsChanged; override;
    procedure KeyPress(var Key: Char); override;
    procedure LayoutChanged; override;
    function  SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure WndProc(var Message: TMessage); override;
    procedure LinkActive(Value: Boolean); override;
    function  CanEditAcceptKey(Key: Char): Boolean; override;
    function  CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;
    function  CreateEditor: TInplaceEdit; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    // Note: TDBGrid.DefaultDrawColumnCell never called by TDBGrid component, allows users to paint cell text as default ...
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    // InplaceEdit related functions :
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function  GetEditLimit: Integer; override;
    {$IFDEF SCROLL_FEATURE}
    procedure UpdateScrollBar; override;
    {$ENDIF}
    procedure UpdateScrollBarsView;
    procedure AfterDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure AfterPaint; virtual;
    procedure BeforeDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure BeforePaint; virtual;
    procedure CheckBoxesChanged(Sender: TObject);
    procedure CalcCheckBoxColumnCellLayout(const Rect: TRect; var RectText, RectCheckbox: TRect);
    procedure CheckBoxFieldSwitchValue(Column: TColumn);
    procedure DrawCheckBoxCell(ACol, ARow: Longint);
    procedure DrawCheckBox(const Rect: TRect; Checked: Boolean);
    function  PointInContentCheckBox(CellRect: TRect; X, Y: Integer): Boolean;
    procedure CalcContentCheckBoxColumnCellLayout(const Rect: TRect; var RectText, RectCheckbox: TRect);
    procedure DataSetNext;
    procedure DataSetPrior;
    {$IFDEF SCROLL_FEATURE}
    procedure FTimerScrollTimer(Sender: TObject);
    {$ENDIF}
    // Only public properties :
    property CheckedList: TcyBookmarkList read FCheckedList;
    property ColumnIndex: Integer read GetColumnIndex write SetColumnIndex;
    property SelectedVisibleIndex: Integer read GetSelectedVisibleIndex write SetSelectedVisibleIndex;
    // Published properties :
    property AllowAppendRecord: Boolean read FAllowAppendRecord write FAllowAppendRecord default true;
    property AllowDeleteRecord: Boolean read FAllowDeleteRecord write FAllowDeleteRecord default true;
    property AllowInsertRecord: Boolean read FAllowInsertRecord write FAllowInsertRecord default true;
    property CheckBoxes: TcyCheckBoxes read FCheckBoxes write SetCheckBoxes;
    property ClientColumn: TcyClientColumn read FClientColumn write FClientColumn;
    property CustomLayoutOptions: TcyCustomLayoutOptions read FCustomLayoutOptions write SetCustomLayoutOptions;
    property DefaultDrawingCheckBox: Boolean read FDefaultDrawingCheckBox write SetDefaultDrawingCheckBox default true;
    property ContentCheckBoxOptions: TcyContentCheckBoxOptions read FContentCheckBoxOptions write SetContentCheckBoxOptions;
    property ContentImageOptions: TcyContentImageOptions read FContentImageOptions write SetContentImageOptions;
    property ContentMemoOptions: TcyContentMemoOptions read FContentMemoOptions write SetContentMemoOptions;
    property ContentFieldsRender: TcyContentFieldsRender read FContentFieldsRender write SetContentFieldsRender;
    property dgColumnResizeMode: TdgColumnResizeMode read FdgColumnResizeMode write SetdgColumnResizeMode default crBoth;
    property EditorOptions: TcyEditorOptions read FEditorOptions write FEditorOptions;
    property HotMode: THotMode read FHotMode write FHotMode default hmNone;
    property HotColor: TColor read FHotColor write FHotColor default clAqua;
    property HotFontColor: TColor read FHotFontColor write FHotFontColor default clBlue;
    property HorzScrollBar: TcyHorzScrollbar read FHorzScrollBar write FHorzScrollBar;
    property VertScrollBar: TcyVertScrollbar read FVertScrollBar write FVertScrollBar;
    property MoveRows: Boolean read FMoveRows write FMoveRows default false;
    property MoveRowsColor: TColor read FMoveRowsColor write FMoveRowsColor default clBlue;
    property MoveRowRendering: TMoveRowRendering read FMoveRowRendering write FMoveRowRendering default mrLine;
    property MouseJob: TMouseJob read FMouseJob write FMouseJob default mjWait;
    property MouseWheelMode: TMouseWheelMode read FMouseWheelMode write FMouseWheelMode default mwRowSelect;
    {$IFDEF SCROLL_FEATURE}
    property ScrollPauseMSeconds: Word read FScrollPauseMSeconds write FScrollPauseMSeconds default 600;  // 500 is not enough when beginning scrolling with keyboard ...
    {$ENDIF}
    property OnCheckBoxClick: TNotifyEvent read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnDrawCheckBox: TDrawCheckBoxEvent read FOnDrawCheckBox write FOnDrawCheckBox;
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write FOnSelectCell;
    property OnBeforePaint: TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnBeforeDrawCell: TDrawCellEvent read FOnBeforeDrawCell write FOnBeforeDrawCell;
    property OnAfterDrawCell: TDrawCellEvent read FOnAfterDrawCell write FOnAfterDrawCell;
    {$IFDEF SCROLL_FEATURE}
    property OnAfterScroll: TNotifyEvent read FonAfterScroll write FonAfterScroll;
    property OnAfterScrollPause: TNotifyEvent read FonAfterScrollPause write FonAfterScrollPause;
    {$ENDIF}
    property OnScrollBarMouseDown: TMouseEvent read FOnScrollBarMouseDown write FOnScrollBarMouseDown;
    property OnScrollBarMouseUp: TMouseEvent read FOnScrollBarMouseUp write FOnScrollBarMouseUp;
    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
    property OnBeginMoveRows: TMoveRowBeginEvent read FOnBeginMoveRows write FOnBeginMoveRows;
    property OnMoveRows: TMoveRowEvent read FOnMoveRows write FOnMoveRows;
    property OnEndMoveRows: TMoveRowEndEvent read FOnEndMoveRows write FOnEndMoveRows;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnEditorAcceptKey: TEditorAcceptKeyEvent read FOnEditorAcceptKey write FOnEditorAcceptKey;
    property OnEditorSetFieldText: TEditorSetFieldText read FEditorSetFieldText write FEditorSetFieldText;
    property OnShowEditor: TShowEditorEvent read FOnShowEditor write FOnShowEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CellRectX(ACol, ARow: Longint): TRect;
    procedure CheckSelectedRows;
    procedure UnCheckSelectedRows;
    function FindColumnByFieldName(FieldName: String): TColumn;
    function FirstDataRowIndex: Byte;
    function FirstDataColIndex: Byte;
    procedure DefaultDrawCheckBox(const Rect: TRect; Checked: Boolean);
    function CalcColumnsWidth: Integer;
    procedure ClientColumnByFieldName(ColumnFieldName: String; MinWidth: Integer);
    procedure ClientColumnByIndex(ColumnIndex: Integer; MinWidth: Integer);
    function GetCheckBoxColumnIndex: Integer;
    function ClientRectDataCells: TRect;
    procedure AdjustClientColumn;
    procedure UpdateLayout;
    function PointInCheckBox(CellRect: TRect; X, Y: Integer): Boolean;
    // Fields rendering :
    function GetGraphicClass(GraphicClassField: TGraphicClassField): TGraphicClass;
    procedure DrawGraphicField(Rect: TRect; Field: TBlobField; FieldContent: TGraphicClass;
      Transparent: Boolean; bgStyle: TBgStyle; Position: TBgPosition);
    procedure DrawMemoField(Rect: TRect; Field: TField; TextWordwrap, BgTransparent: Boolean; MaxCars, MaxLines: Integer);
    procedure DrawWordwrapTextField(Rect: TRect; Field: TField);
    procedure DrawBooleanField(Rect: TRect; Field: TField);
    //
    function GetRecordPosition: TRecordPosition;
    function GotoRecordPosition(RecordPosition: TRecordPosition): Boolean;
    procedure FreeRecordPosition(RecordPosition: TRecordPosition);
    procedure TestSomething(Value: Integer);
  published
  end;

const
  IsChecked : array[Boolean] of Integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);

implementation

uses SysUtils, cyMathParser;

{TcyEditorOptions}
constructor TcyEditorOptions.Create(AOwner: TComponent);
begin
  FBaseGrid := TcyBaseDBGrid(AOwner);
  FParse := true;
  FColor := clWindow;
end;

{ TcyHorzScrollbar }
constructor TcyHorzScrollbar.Create(AOwner: TComponent);
begin
  FBaseGrid := TcyBaseDBGrid(AOwner);
  FVisible := true;
end;

procedure TcyHorzScrollbar.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;

  ShowScrollBar(FBaseGrid.Handle, SB_HORZ, FVisible);  // will be displayed only if needed ...
//  FBaseGrid.Invalidate;
end;

{ TcyVertScrollbar }
constructor TcyVertScrollbar.Create(AOwner: TComponent);
begin
  FBaseGrid := TcyBaseDBGrid(AOwner);
  FVisible := true;
  FVScrollLineMode := vlDefault;
  FScrollOptions := [voLINEUP, voLINEDOWN, voPAGEUP, voPAGEDOWN, voBOTTOM, voTOP, voTHUMBPOSITION];
end;

procedure TcyVertScrollbar.SetScrollOptions(const Value: TVScrollOptions);
begin
  FScrollOptions := Value;
end;

procedure TcyVertScrollbar.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;

  ShowScrollBar(FBaseGrid.Handle, SB_VERT, FVisible);
//  FBaseGrid.Invalidate;
end;

{ TcyClientColumn }
constructor TcyClientColumn.Create(AOwner: TComponent);
begin
  FBaseGrid := TcyBaseDBGrid(AOwner);
  FEnabled := false;
  FFieldName := '';
  FIndex := -1;
  FMinWidth := 10;
  FMode := cmByIndex;
end;

procedure TcyClientColumn.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;

  if (not (csLoading in FBaseGrid.ComponentState)) and FEnabled then
    FBaseGrid.AdjustClientColumn;
end;

procedure TcyClientColumn.SetFieldName(const Value: String);
begin
  if FFieldName = Value then Exit;
  FFieldName := Value;

  if (csDesigning in FBaseGrid.ComponentState) and (not (csLoading in FBaseGrid.ComponentState)) then
    begin
      FEnabled := true;
      FMode := cmByFieldName;
    end;

  if (not (csLoading in FBaseGrid.ComponentState)) and FEnabled then
    FBaseGrid.AdjustClientColumn;
end;

procedure TcyClientColumn.SetIndex(const Value: Integer);
begin
  if Value < -1
  then FIndex := -1
  else FIndex := Value;

  if (csDesigning in FBaseGrid.ComponentState) and (not (csLoading in FBaseGrid.ComponentState)) then
    begin
      FEnabled := true;
      FMode := cmByIndex;
    end;

  if (not (csLoading in FBaseGrid.ComponentState)) and FEnabled then
    FBaseGrid.AdjustClientColumn;
end;

procedure TcyClientColumn.SetMode(const Value: TColumnMode);
begin
  if Value = cmIndicatorsCol then Exit;
  FMode := Value;

  if (not (csLoading in FBaseGrid.ComponentState)) and FEnabled then
    FBaseGrid.AdjustClientColumn;
end;

{ TcyCheckBoxes }
constructor TcyCheckBoxes.Create(AOwner: TComponent);
begin
  FBaseGrid := TcyBaseDBGrid(AOwner);
  FAlignLeft := true;
  FColumnIndex := 0;
  FColumnMode := cmByIndex;
  FMargin := 1;
  FReadOnly := false;
  FSize := 16;
  FVisible := false;
end;

procedure TcyCheckBoxes.SetAlignLeft(const Value: Boolean);
begin
  FAlignLeft := Value;
  if Assigned(FOnChange) and FVisible
  then FOnChange(Self);
end;

procedure TcyCheckBoxes.SetColumnFieldName(const Value: String);
begin
  FColumnFieldName := Value;

  if (csDesigning in FBaseGrid.ComponentState) and (not (csLoading in FBaseGrid.ComponentState)) then
    begin
      FVisible := true;
      FColumnMode := cmByFieldName;
    end;

  if Assigned(FOnChange) and FVisible
  then FOnChange(Self);
end;

procedure TcyCheckBoxes.SetColumnIndex(const Value: Integer);
begin
  if Value < -1
  then FColumnIndex := -1
  else FColumnIndex := Value;

  if (csDesigning in FBaseGrid.ComponentState) and (not (csLoading in FBaseGrid.ComponentState)) then
    begin
      FVisible := true;
      FColumnMode := cmByIndex;
    end;

  if Assigned(FOnChange) and FVisible
  then FOnChange(Self);
end;

procedure TcyCheckBoxes.SetColumnMode(const Value: TColumnMode);
begin
  FColumnMode := Value;

  if Assigned(FOnChange) and FVisible
  then FOnChange(Self);
end;

procedure TcyCheckBoxes.SetMargin(const Value: Word);
begin
  FMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCheckBoxes.SetSize(const Value: Word);
begin
  FSize := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TcyCheckBoxes.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TcyCustomLayoutOptions }
constructor TcyCustomLayoutOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyBaseDBGrid(AOwner);
  FDataRowHeight := 0;
  FIndicatorColumnWidth := 0;
  FTitleRowHeight := 0;
  FActiveDataRowHeight := 0;
  FGridLineWidth := 1;
end;

procedure TcyCustomLayoutOptions.SetDataRowHeight(const Value: Integer);
begin
  if FDataRowHeight <> Value
  then begin
    FDataRowHeight := Value;
    FGrid.BeginLayout;
    FGrid.EndLayout;

    // If DBGrid can show more rows, we need to refresh once more :
    FGrid.BeginLayout;
    FGrid.EndLayout;
  end;
end;

procedure TcyCustomLayoutOptions.SetGridLineWidth(const Value: Integer);
begin
  FGridLineWidth := Value;

  if FGrid.GridLineWidth <> FGridLineWidth then
    FGrid.GridLineWidth := FGridLineWidth;
end;

procedure TcyCustomLayoutOptions.SetIndicatorColumnWidth(const Value: Integer);
begin
  if FIndicatorColumnWidth <> Value
  then begin
    FIndicatorColumnWidth := Value;
    FGrid.AdjustClientColumn;
    FGrid.UpdateLayout;
  end;
end;

procedure TcyCustomLayoutOptions.SetActiveDataRowHeight(const Value: Integer);
begin
  if FActiveDataRowHeight <> Value
  then begin
    FActiveDataRowHeight := Value;
    FGrid.UpdateLayout;
  end;
end;

procedure TcyCustomLayoutOptions.SetTitleRowHeight(const Value: Integer);
begin
  if FTitleRowHeight <> Value
  then begin
    FTitleRowHeight := Value;
    FGrid.UpdateLayout;
  end;
end;

{ TcyContentMemoOptions }
constructor TcyContentMemoOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyBaseDBGrid(AOwner);
  FMaxCars := 0;
  FMaxLines := 0;
  FWordwrap := true;
end;

procedure TcyContentMemoOptions.SetMaxCars(const Value: Word);
begin
  FMaxCars := Value;
  if FGrid.FContentFieldsRender.FMemoField = mfMemo then
    FGrid.Invalidate;
end;

procedure TcyContentMemoOptions.SetMaxLines(const Value: Word);
begin
  FMaxLines := Value;
  if FGrid.FContentFieldsRender.FMemoField = mfMemo then
    FGrid.Invalidate;
end;

procedure TcyContentMemoOptions.SetWordwrap(const Value: Boolean);
begin
  FWordwrap := Value;
  if FGrid.FContentFieldsRender.FMemoField = mfMemo then
    FGrid.Invalidate;
end;

{ TcyContentImageOptions }
constructor TcyContentImageOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyBaseDBGrid(AOwner);
  FFieldContent := fcBitmap;
  FStyle := bgStretchProportional;
  FPosition := bgCentered;
  FTransparent := false;
end;

procedure TcyContentImageOptions.SetFieldContent(const Value: TGraphicClassField);
begin
  FFieldContent := Value;
  if FGrid.FContentFieldsRender.FGraphicField = gfImage then
    FGrid.Invalidate;
end;

procedure TcyContentImageOptions.SetPosition(const Value: TBgPosition);
begin
  FPosition := Value;
  if FGrid.FContentFieldsRender.FGraphicField = gfImage then
    FGrid.Invalidate;
end;

procedure TcyContentImageOptions.SetStyle(const Value: TBgStyle);
begin
  FStyle := Value;
  if FGrid.FContentFieldsRender.FGraphicField = gfImage then
    FGrid.Invalidate;
end;

procedure TcyContentImageOptions.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  if FGrid.FContentFieldsRender.FGraphicField = gfImage then
    FGrid.Invalidate;
end;

{ TcyContentCheckBoxOptions }
constructor TcyContentCheckBoxOptions.Create(AOwner: TComponent);
begin
  FGrid := TcyBaseDBGrid(AOwner);
  FAlignLeft := true;
  FMargin := 1;
  FSize := 16;
end;

procedure TcyContentCheckBoxOptions.SetAlignLeft(const Value: Boolean);
begin
  FAlignLeft := Value;
  if FGrid.FContentFieldsRender.FMemoField = mfMemo then
    FGrid.Invalidate;
end;

procedure TcyContentCheckBoxOptions.SetMargin(const Value: Word);
begin
  FMargin := Value;
  if FGrid.FContentFieldsRender.FMemoField = mfMemo then
    FGrid.Invalidate;
end;

procedure TcyContentCheckBoxOptions.SetSize(const Value: Word);
begin
  FSize := Value;
  if FGrid.FContentFieldsRender.FMemoField = mfMemo then
    FGrid.Invalidate;
end;

{ TcyContentFieldsRender }
constructor TcyContentFieldsRender.Create(AOwner: TComponent);
begin
  FGrid := TcyBaseDBGrid(AOwner);
  FBooleanField := bfCheckBox;
  FGraphicField := gfImage;
  FMemoField := mfMemo;
end;

procedure TcyContentFieldsRender.SetBooleanField(const Value: TBooleanFieldDisplay);
begin
  FBooleanField := Value;
  FGrid.Invalidate;
end;

procedure TcyContentFieldsRender.SetGraphicField(const Value: TGraphicFieldDisplay);
begin
  FGraphicField := Value;
  FGrid.Invalidate;
end;

procedure TcyContentFieldsRender.SetMemoField(const Value: TMemoFieldDisplay);
begin
  FMemoField := Value;
  FGrid.Invalidate;
end;

{ TcyBaseDBGrid}
constructor TcyBaseDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Vertical scrollbar that disapears (Delphi TDBGrid bug) :
  // We need only to initialize ScrollBars property to ssNone
//  if ScrollBars <> ssNone then
//    ScrollBars := ssNone;
// Conflicting with UpdateScrollBarsView!


  // Create a RichEdit for embedded rtf rendering :
  FRichEdit := TRichEdit.Create(Self);
  FRichEdit.Parent := Self;
  FRichEdit.BorderStyle := bsNone;
  FRichEdit.SetBounds(-10, -10, 0, 0);  // Hide richEdit at design time
  FRichEdit.TabStop := false;
  // FRichEdit.Visible := false;   // Avoid error: first cell with RichText will not be painted

  try
    FRichTextStream := TStringStream.Create('');
  except
    FRichTextStream := Nil;
  end;

  FSavInplaceEdit  := Nil;
  FMouseOverCell.X := -1;
  FMouseOverCell.Y := -1;
  FMouseJob := mjWait;
  FExtendedAcceptKey := true;
  FCheckedList := TcyBookmarkList.Create(self);
  FCheckedList.OnChange := CheckBoxesChanged;
  FdgColumnResizeMode := crBoth;
  FMouseWheelMode := mwRowSelect;
  FAllowDeleteRecord := true;
  FAllowInsertRecord := true;
  FAllowAppendRecord := true;
  {$IFDEF SCROLL_FEATURE}
  LastScrollRecNo := -1;
  FScrollPauseMSeconds := 600;
  {$ENDIF}
  FHotMode := hmNone;
  FHotColor := clAqua;
  FHotFontColor := clBlue;
  FHorzScrollBar := TcyHorzScrollbar.Create(self);
  FVertScrollBar := TcyVertScrollbar.Create(self);
  FCheckBoxes := TcyCheckBoxes.Create(self);
  FCheckBoxes.OnChange := CheckBoxesChanged;
  FClientColumn := TcyClientColumn.Create(self);
  FCustomLayoutOptions := TcyCustomLayoutOptions.Create(self);
  FDefaultDrawingCheckBox := true;
  FEditorOptions := TcyEditorOptions.Create(self);
  FMoveRows := false;
  FMoveRowsColor := clBlue;
  FMoveRowRendering := mrLine;
  FContentCheckBoxOptions := TcyContentCheckBoxOptions.Create(Self);
  FContentImageOptions := TcyContentImageOptions.Create(Self);
  FContentMemoOptions := TcyContentMemoOptions.Create(Self);
  FContentFieldsRender := TcyContentFieldsRender.Create(Self);
end;

destructor TcyBaseDBGrid.Destroy;
begin
  {$IFDEF SCROLL_FEATURE}
  if Assigned(FTimerScroll)
  then begin
    FTimerScroll.Enabled := false;
    FTimerScroll.Free;
    FTimerScroll := Nil;
  end;
  {$ENDIF}

  FCheckedList.Free;
  FCheckedList := nil;
  FCheckBoxes.Free;
  FClientColumn.Free;
  FEditorOptions.Free;
  FVertScrollBar.Free;
  FCustomLayoutOptions.Free;
  FContentCheckBoxOptions.Free;
  FContentImageOptions.Free;
  FContentMemoOptions.Free;
  FContentFieldsRender.Free;
  if FRichTextStream <> nil then
    FRichTextStream.Free;
  FRichEdit.Free;
  Inherited;
end;

procedure TcyBaseDBGrid.Loaded;
begin
  Inherited;
  FIsLoaded := not (csLoading In ComponentState);
  AdjustClientColumn;
end;

procedure TcyBaseDBGrid.WndProc(var Message: TMessage);
begin
  // UpdateScrollBarsView;
  Inherited WndProc(Message);
end;

procedure TcyBaseDBGrid.WMNCCalcSize(var Msg: TMessage);
begin
  UpdateScrollBarsView;
  inherited;
end;

// ScrollBars property value are not correct!
procedure TcyBaseDBGrid.UpdateScrollBarsView;
var
  VertScrollBarVisible, HorzScrollBarVisible, NeedHorzScrollBars: Boolean;
  wl: Integer;
begin
  // Show/hide scrollbars (Vertical scrollbar will be always seen after scroll record, so, we need to hide it) :
  if FIsLoaded and (not (csDestroying in ComponentState)) then
  begin
    // Check if we need to hide Horizontal ScrollBar :
    if (not FHorzScrollBar.Visible) then
    begin
      wl := GetWindowlong(Handle, GWL_STYLE);
      HorzScrollBarVisible := (wl and WS_HSCROLL) <> 0;

      if HorzScrollBarVisible then
      begin
        ShowScrollBar(Handle, SB_HORZ, FHorzScrollBar.Visible);
      end;
    end;

    // After handling horizontal scrollbar because of AdjustClientColumn :

    // Check if we need to hide Vertical ScrollBar :
    if not FVertScrollBar.Visible then
    begin
      wl := GetWindowlong(Handle, GWL_STYLE);
      VertScrollBarVisible := (wl and WS_VSCROLL) <> 0;

      if VertScrollBarVisible then
      begin
        if ShowScrollBar(Handle, SB_VERT, FVertScrollBar.Visible) then
          AdjustClientColumn;
      end;
    end;

(* Old: Não gere bem quando as barras invisiveis ...

  // Horizontal scrollbar :
    wl := GetWindowlong(Handle, GWL_STYLE);
    HorzScrollBarVisible := (wl and WS_HSCROLL) <> 0;
    NeedHorzScrollBars := CalcColumnsWidth > ClientWidth;

    if HorzScrollBarVisible then
    begin
      if not FHorzScrollBar.Visible then
        ShowScrollBar(Handle, SB_HORZ, false)
      else
        if NeedHorzScrollBars <> HorzScrollBarVisible then
          ShowScrollBar(Handle, SB_HORZ, NeedHorzScrollBars);
    end
    else
      if NeedHorzScrollBars <> HorzScrollBarVisible then
        ShowScrollBar(Handle, SB_HORZ, NeedHorzScrollBars);


    // Vertical scrollbar (After handling horizontal scrollbar because of AdjustClientColumn) :
    wl := GetWindowlong(Handle, GWL_STYLE);
    VertScrollBarVisible := (wl and WS_VSCROLL) <> 0;

    // if VertScrollBarVisible and (not FVertScrollBar.Visible) then
    if VertScrollBarVisible <> FVertScrollBar.Visible then
      if ShowScrollBar(Handle, SB_VERT, FVertScrollBar.Visible) then
        AdjustClientColumn;    *)
  end;
end;

// Also called when DataSource property changes ...
procedure TcyBaseDBGrid.LinkActive(Value: Boolean);
begin
  {$IFDEF SCROLL_FEATURE}
  LastScrollRecNo := -1;
  {$ENDIF}

  Inherited;

  if Value then
    FCheckedList.DataSource := DataSource;

  FCheckedList.LinkActive(Value);

  if Value and (not (csLoading in ComponentState)) then
    AdjustClientColumn;
end;

function TcyBaseDBGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ARow <> Row then
    if FCustomLayoutOptions.FActiveDataRowHeight <> 0 then  // New row with custom height ...
      UpdateLayout;

  Result := Inherited SelectCell(ACol, ARow);
  if Assigned(FOnSelectCell) then FOnSelectCell(Self, ACol, ARow, Result);
end;

function TcyBaseDBGrid.CellRectX(ACol, ARow: Longint): TRect;
begin
  RESULT := CellRect(ACol, ARow);
end;

procedure TcyBaseDBGrid.SetDefaultDrawingCheckBox(const Value: Boolean);
begin
  FDefaultDrawingCheckBox := Value;
  Invalidate;
end;

procedure TcyBaseDBGrid.SetdgColumnResizeMode(const Value: TdgColumnResizeMode);
begin
  FdgColumnResizeMode := Value;
  SetCustomGridOptions;
end;

procedure TcyBaseDBGrid.SetContentImageOptions(const Value: TcyContentImageOptions);
begin
  FContentImageOptions := Value;
  Invalidate;
end;

procedure TcyBaseDBGrid.SetContentMemoOptions(const Value: TcyContentMemoOptions);
begin
  FContentMemoOptions := Value;
  Invalidate;
end;

procedure TcyBaseDBGrid.SetColumnIndex(const Value: Integer);
begin
  Col := Value + FirstDataColIndex;
end;

procedure TcyBaseDBGrid.SetContentCheckBoxOptions(const Value: TcyContentCheckBoxOptions);
begin
  FContentCheckBoxOptions := Value;
  Invalidate;
end;

procedure TcyBaseDBGrid.SetContentFieldsRender(const Value: TcyContentFieldsRender);
begin
  FContentFieldsRender := Value;
  Invalidate;
end;

procedure TcyBaseDBGrid.SetCheckBoxes(Value: TcyCheckBoxes);
begin
  CheckBoxes.Assign(Value);
end;

procedure TcyBaseDBGrid.CheckBoxesChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  if csDestroying in ComponentState then Exit;   // Avoid error on destroying component
  AdjustClientColumn;
  UpdateLayout;
end;

procedure TcyBaseDBGrid.CheckBoxFieldSwitchValue(Column: TColumn);
begin
  if Column.ReadOnly then
    Exit;

  try
    if DataLink.DataSet.State = dsBrowse then
      DataLink.DataSet.Edit;
    Column.Field.AsBoolean := not Column.Field.AsBoolean;
  finally

  end;
end;

procedure TcyBaseDBGrid.CheckSelectedRows;
var i: Integer;
begin
  if DataLink.Active
  then
    for i := 0 to SelectedRows.Count-1 do
      FCheckedList.InsertBookmark(SelectedRows[i])
end;

procedure TcyBaseDBGrid.UnCheckSelectedRows;
var i: Integer;
begin
  if DataLink.Active
  then
    for i := 0 to SelectedRows.Count-1 do
      FCheckedList.DeleteBookmark(SelectedRows[i]);
end;

function TcyBaseDBGrid.FirstDataRowIndex: byte;
begin
  if dgTitles in Options
  then RESULT := 1
  else RESULT := 0;
end;

function TcyBaseDBGrid.FindColumnByFieldName(FieldName: String): TColumn;
var i: Integer;
begin
  Result := Nil;
  FieldName := AnsiUpperCase(FieldName);

  for i := 0 to Columns.Count-1 do
    if FieldName = AnsiUpperCase(Columns[i].FieldName)
    then begin
      Result := Columns[i];
      Break;
    end;
end;

function TcyBaseDBGrid.FirstDataColIndex: byte;
begin
  if dgIndicator in Options
  then RESULT := 1
  else RESULT := 0;
end;

function TcyBaseDBGrid.GetSelectedVisibleIndex: Integer;
var c: Integer;
begin
  RESULT := -1;
  for c := 0 to SelectedIndex do
    if Columns[c].Visible
    then RESULT := RESULT + 1;
end;

procedure TcyBaseDBGrid.KeyPress(var Key: Char);
begin
  if DataLink.Active then
  begin
    // Don' t let create a new record if FAllowAppendRecord = false on empty table :
    if not FAllowAppendRecord then
      if DataLink.DataSet.Bof and DataLink.DataSet.Eof
      then Key := #0;

    // Set Boolean fields :
    if (ColumnIndex > -1) and (FContentFieldsRender.FBooleanField = bfCheckBox) then
      if (Columns[ColumnIndex].Field is TBooleanField) then
      begin
        EditorMode := false;
        if (Key in [#13, ' ']) then
        begin
          CheckBoxFieldSwitchValue(Columns[ColumnIndex]);
          Key := #0;
        end;
      end;
  end;

  inherited KeyPress(Key);
end;

procedure TcyBaseDBGrid.SetSelectedVisibleIndex(const Value: Integer);
var c, CurrentVisibleIndex: Integer;
begin
  CurrentVisibleIndex := -1;
  for c := 0 to Columns.Count - 1 do
    if Columns[c].Visible
    then begin
      Inc(CurrentVisibleIndex, 1);
      if CurrentVisibleIndex = Value
      then begin
        SelectedIndex := c;
        Break;
      end;
    end;
end;

// !!! Note: not called by regular keys like numbers and characters ...
// Called on TCustomDBGrid.KeyDown procedure ... OnKeyDown event always executed ...
function TcyBaseDBGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
var
  SavReadOnly: Boolean;
  SavKewDownEvent: TKeyEvent;
  aKey: Word;
  aShift: TShiftState;

    function CanBeModified: Boolean;
    begin
      // Note: in D2009, we can delete records without dgEditing in Options ...
      RESULT := (not ReadOnly) and (Datalink.Active) and (not Datalink.Readonly) and (DataLink.DataSet.CanModify);
    end;

begin
  RESULT := Inherited CanGridAcceptKey(Key, Shift);

  if RESULT and FExtendedAcceptKey
  then
    // Avoid deleting/inserting records with keyboard using ReadOnly property:
    if Assigned(DataLink.DataSet)
    then
        case key of
          VK_DOWN, VK_NEXT:         // AVOID INSERT NEW RECORD IF WE ARE AT THE END OF THE DATASET //
            if CanBeModified
            then
              if (not (ssCtrl in Shift))       // if ssCtrl in Shift, it moves from first or last row grid ...
               and (not FAllowAppendRecord)
              then begin
                RESULT := false;
                // Handle VK_DOWN by recalling KeyDown with ReadOnly = true to avoid append record and handle multiselection :
                SavReadOnly := ReadOnly;
                ReadOnly := true;
                FExtendedAcceptKey := false;
                SavKewDownEvent := OnKeyDown;
                OnKeyDown := Nil;
                // Recall KeyDown for multiselection :
                KeyDown(Key, Shift);

                OnKeyDown := SavKewDownEvent;
                FExtendedAcceptKey := true;
                if SavReadOnly <> ReadOnly
                then ReadOnly := SavReadOnly;

                if dgAlwaysShowEditor in Options then  // Avoid editing problem after a VK_DOWN :
                {$IFDEF DELPHI2009_OR_ABOVE}
                  InplaceEditor.ReadOnly := false;
                {$ELSE}
                  // InplaceEditor.ReadOnly does not exists!
                  TEdit(InplaceEditor).ReadOnly := false;
                {$ENDIF}
              end;

          VK_TAB:
            if CanBeModified
            then
              if not (ssShift in Shift)                // Move prior column ...
               and (Col = ColCount-1) and (not FAllowAppendRecord)
              then begin
                RESULT := false;
                // Do Next and move to first column :
                DataSetNext;
                Col := IndicatorOffset;
              end;

          VK_INSERT:
            if CanBeModified
            then
              if not FAllowInsertRecord
              then RESULT := false;

          VK_DELETE:
            if CanBeModified
            then
              if not FAllowDeleteRecord
              then RESULT := false;

          VK_END:
            if Shift = [ssCtrl, ssShift]
            then begin
              RESULT := false;
              // Select all record until the end of table :
              aKey := VK_DOWN;
              aShift := [ssShift];

              SavReadOnly := ReadOnly;
              ReadOnly := true;
              FExtendedAcceptKey := false;
              SavKewDownEvent := OnKeyDown;
              OnKeyDown := Nil;

              while (not DataLink.DataSet.Eof) do
                KeyDown(aKey, aShift);

              OnKeyDown := SavKewDownEvent;
              FExtendedAcceptKey := true;
              if SavReadOnly <> ReadOnly
              then ReadOnly := SavReadOnly;
            end;

          VK_HOME:
            if Shift = [ssCtrl, ssShift]
            then begin
              RESULT := false;
              // Select all record until the beginning of table :
              aKey := VK_UP;
              aShift := [ssShift];
              SavKewDownEvent := OnKeyDown;
              OnKeyDown := Nil;

              while (not DataLink.DataSet.Bof) do
                KeyDown(aKey, aShift);

              OnKeyDown := SavKewDownEvent;
            end;
        end;
end;

function TcyBaseDBGrid.PointInCheckBox(CellRect: TRect; X, Y: Integer): Boolean;
var RectText, RectCheckbox: TRect;
begin
  CalcCheckBoxColumnCellLayout(CellRect, RectText, RectCheckbox);
  Result := PointInRect(Point(X, Y), RectCheckbox);
end;

procedure TcyBaseDBGrid.DoEnter;
begin
  FMouseJob := mjWait;
  inherited;
end;

procedure TcyBaseDBGrid.DoExit;
begin
  FMouseJob := mjWait;
  inherited;
end;

function TcyBaseDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var aKey: Word;
begin
  if FMouseWheelMode = mwDefault
  then
    Result := inherited DoMouseWheelDown(Shift, MousePos)  // Will call OnMouseWheelDown ...
  else begin
    RESULT := false;

    if Datalink.Active
    then
      case FMouseWheelMode of
        mwRowSelect:
          begin
            RESULT := true;
            if Shift <> []
            then begin
              if ssShift in Shift
              then begin
                aKey := VK_DOWN;
                KeyDown(aKey, Shift);
              end
              else
                DataLink.DataSet.MoveBy(RowCount-1);
            end
            else
              DataSetNext;
          end;

        mwNavigate:
          begin
            RESULT := true;
            DataSetNext;
          end;

        mwDoNothing:
          RESULT := true;
      end;

    if Assigned(OnMouseWheelDown)
    then OnMouseWheelDown(self, Shift, MousePos, RESULT);
  end;
end;

function TcyBaseDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var aKey: Word;
begin
  if FMouseWheelMode = mwDefault
  then
    Result := inherited DoMouseWheelUp(Shift, MousePos)
  else begin
    RESULT := false;

    if Datalink.Active
    then
      case FMouseWheelMode of
        mwRowSelect:
          begin
            RESULT := true;
            if Shift <> []
            then begin
              if ssShift in Shift
              then begin
                aKey := VK_UP;
                KeyDown(aKey, Shift);
              end
              else
                DataLink.DataSet.MoveBy((-1) * (RowCount-1));
            end
            else
              DataSetPrior;
          end;

        mwNavigate:
          begin
            RESULT := true;
            DataSetPrior;
          end;

        mwDoNothing:
          RESULT := true;
      end;

    if Assigned(OnMouseWheelUp)
    then OnMouseWheelUp(self, Shift, MousePos, RESULT);
  end;
end;

procedure TcyBaseDBGrid.DataSetNext;
begin
  with Datalink.Dataset do
  begin
    if (State = dsInsert) and (not Modified) and (not Datalink.Editing)
    then
      if DataLink.EOF
      then Exit
      else Cancel
    else
      DataLink.DataSet.Next;

    if DataLink.EOF and CanModify and (not ReadOnly) and FAllowAppendRecord and (dgEditing in Options)
    then Append;
  end;
end;

procedure TcyBaseDBGrid.DataSetPrior;
begin
  with Datalink.Dataset do
    if (State = dsInsert) and (not Modified) and DataLink.EOF and (not Datalink.Editing)
    then
      Cancel
    else
      DataLink.DataSet.Prior;
end;

procedure TcyBaseDBGrid.BeforePaint;
begin
  if Assigned(FOnBeforePaint) then FOnBeforePaint(Self);
end;

procedure TcyBaseDBGrid.AfterPaint;
begin
  if Assigned(FOnAfterPaint) then FOnAfterPaint(Self);
end;

procedure TcyBaseDBGrid.Paint;
begin
  BeforePaint;
  Inherited;
  AfterPaint;
end;

procedure TcyBaseDBGrid.AfterDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if Assigned(FOnAfterDrawCell)
  then FOnAfterDrawCell(Self, ACol, ARow, ARect, AState);
end;

procedure TcyBaseDBGrid.BeforeDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if Assigned(FOnBeforeDrawCell)
  then FOnBeforeDrawCell(Self, ACol, ARow, ARect, AState);
end;

procedure TcyBaseDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;

  BeforeDrawCell(ACol, ARow, ARect, AState);
  Inherited;

  // Draw checkbox in Indicators col when painting last column:
  if DataLink.Active and FCheckBoxes.FVisible and (FCheckBoxes.FColumnMode = cmIndicatorsCol) then
    if (ACol < FirstDataColIndex) and (ARow >= FirstDataRowIndex) then
      DrawCheckBoxCell(0, ARow);

  AfterDrawCell(ACol, ARow, ARect, AState);
end;

procedure TcyBaseDBGrid.CalcCheckBoxColumnCellLayout(const Rect: TRect; var RectText, RectCheckbox: TRect);
var LeftCheckBoxPos: Integer;
begin
  if FCheckBoxes.FAlignLeft
  then begin
    LeftCheckBoxPos := Rect.left + FCheckBoxes.FMargin;
    RectCheckBox := classes.Rect(LeftCheckBoxPos, Rect.Top, LeftCheckBoxPos + FCheckBoxes.FSize, Rect.Bottom);
    RectText := classes.Rect(LeftCheckBoxPos + FCheckBoxes.FSize + FCheckBoxes.FMargin, Rect.Top, Rect.Right, Rect.Bottom);
  end
  else begin
    LeftCheckBoxPos := Rect.Right - FCheckBoxes.FMargin - FCheckBoxes.FSize;
    RectCheckBox := classes.Rect(LeftCheckBoxPos, Rect.Top, LeftCheckBoxPos + FCheckBoxes.FSize, Rect.Bottom);
    RectText := classes.Rect(Rect.Left, Rect.Top, LeftCheckBoxPos - FCheckBoxes.FMargin, Rect.Bottom);
  end;
end;

procedure TcyBaseDBGrid.DrawCheckBoxCell(ACol, ARow: Longint);
var
  ARect, RectText, RectCheckbox: TRect;
  CheckBoxState: Boolean;
  OldActive, Index: Integer;
begin
  ARect := CellRect(ACol, ARow);
  CalcCheckBoxColumnCellLayout(ARect, RectText, RectCheckbox);
  if dgTitles in Options
  then Dec(ARow, 1);
  OldActive := DataLink.ActiveRecord;
  try
    DataLink.ActiveRecord := ARow;
    CheckBoxState := CheckedList.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  finally
    DataLink.ActiveRecord := OldActive;
    DrawCheckBox(RectCheckBox, CheckBoxState);
  end;
end;

procedure TcyBaseDBGrid.DrawCheckBox(const Rect: TRect; Checked: Boolean);
begin
  if FDefaultDrawingCheckBox
  then DefaultDrawCheckBox(Rect, Checked);

  if Assigned(FOnDrawCheckBox)
  then FOnDrawCheckBox(Self, Rect, Checked);
end;

procedure TcyBaseDBGrid.DefaultDrawCheckBox(const Rect: TRect; Checked: Boolean);
begin
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, ISChecked[Checked]);
end;

procedure TcyBaseDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;

  if not (csLoading in ComponentState) then
    AdjustClientColumn;

  if Assigned(FOnResize)
  then FOnResize(Self);
end;

procedure TcyBaseDBGrid.WMVScroll(var Message: TWMVScroll);
var Handled: Boolean;
begin
  if FVertScrollBar.ScrollOptions = [] then
    Exit;

  if Message.ScrollCode = SB_PAGEUP then
    if not (voPAGEUP in FVertScrollBar.FScrollOptions) then
      Exit;

  if Message.ScrollCode = SB_PAGEDOWN then
    if not (voPAGEDOWN in FVertScrollBar.FScrollOptions) then
      Exit;

  if Message.ScrollCode = SB_BOTTOM then
    if not (voBOTTOM in FVertScrollBar.FScrollOptions) then
      Exit;

  if Message.ScrollCode = SB_TOP then
    if not (voTOP in FVertScrollBar.FScrollOptions) then
      Exit;

  if Message.ScrollCode = SB_THUMBPOSITION then
    if not (voTHUMBPOSITION in FVertScrollBar.FScrollOptions) then
      Exit;

  Handled := false;

  if Message.ScrollCode = SB_LINEUP then
  begin
    if not (voLINEUP in FVertScrollBar.FScrollOptions) then
      Exit;

    if FVertScrollBar.FVScrollLineMode = vlSingleRecord then
    begin
      Handled := true;
      if Datalink.Active then
        DataSetPrior;
    end;
  end;

  if Message.ScrollCode = SB_LINEDOWN then
  begin
    if not (voLINEDOWN in FVertScrollBar.FScrollOptions) then
      Exit;

    if FVertScrollBar.FVScrollLineMode = vlSingleRecord then
    begin
      Handled := true;
      if Datalink.Active then
        DataSetNext;
    end;
  end;

{  // Catch left mouseUp :
  if Message.ScrollCode = SB_ENDSCROLL then
  begin
  end;  }

  if not Handled then
    Inherited;
end;

procedure TcyBaseDBGrid.AdjustClientColumn;
begin
  if FClientColumn.FEnabled
  then
    case FClientColumn.Mode of
      cmByIndex: ClientColumnByIndex(FClientColumn.FIndex, FClientColumn.FMinWidth);
      cmByFieldName: ClientColumnByFieldName(FClientColumn.FFieldName, FClientColumn.FMinWidth);
    end;
end;

procedure TcyBaseDBGrid.ClientColumnByFieldName(ColumnFieldName: String; MinWidth: Integer);
var i: Integer;
begin
  for i := 0 to Columns.Count-1 do
    if AnsiUpperCase(ColumnFieldName) = AnsiUpperCase(Columns[i].FieldName)
    then begin
      ClientColumnByIndex(i, MinWidth);
      Break;
    end;
end;

function TcyBaseDBGrid.CalcColumnsWidth: Integer;
var
  ColInc: Byte;
  c: Integer;
begin
  if dgColLines in Options
  then ColInc := GridLineWidth
  else ColInc := 0;

  Result := 1; // In order to see last column frame ...

  if dgIndicator in Options
  then
    if FCustomLayoutOptions.FIndicatorColumnWidth <> 0
    then
      inc(Result, FCustomLayoutOptions.FIndicatorColumnWidth)
    else
      if (FCheckBoxes.ColumnMode = cmIndicatorsCol) and (FCheckBoxes.Visible)
      then inc(Result, FCheckBoxes.Size + FCheckBoxes.Margin * 2)            // CheckBoxes on indicators column
      else inc(Result, DBGrids.IndicatorWidth);

  for c := 0 to Columns.Count - 1 do
    inc(Result, Columns[c].Width + ColInc);
end;

procedure TcyBaseDBGrid.ClientColumnByIndex(ColumnIndex: Integer; MinWidth: Integer);
var
  ColsWidth, NewWidth: Integer;
begin
  if (ColumnIndex = -1) or (ColumnIndex > Columns.Count-1) then
    ColumnIndex := Columns.Count-1;

  if ColumnIndex = -1 then  // No columns?
    Exit;

  ColsWidth := CalcColumnsWidth;

  if ColsWidth <> ClientWidth
  then begin
    NewWidth := ClientWidth - (ColsWidth - Columns[ColumnIndex].Width);
    if NewWidth < MinWidth then
      NewWidth := MinWidth;
    if Columns[ColumnIndex].Width <> NewWidth
    then Columns[ColumnIndex].Width := NewWidth;
  end;
end;

procedure TcyBaseDBGrid.SetCustomGridOptions;
begin
  with TcyCustomGrid(Self) do
    if dgColumnResize in Self.Options
    then begin
      if FdgColumnResizeMode = crResizeOnly
      then Options := Options - [goColMoving]
      else Options := Options + [goColMoving];

      if FdgColumnResizeMode = crMoveOnly
      then Options := Options - [goColSizing]
      else Options := Options + [goColSizing];
    end
    else begin
      Options := Options - [goColMoving];
      Options := Options - [goColSizing];
    end;
end;

procedure TcyBaseDBGrid.SetCustomLayoutOptions(const Value: TcyCustomLayoutOptions);
begin
  FCustomLayoutOptions.Assign(Value);
end;

function TcyBaseDBGrid.GetCheckBoxColumnIndex: Integer;
var i: Integer;
begin
  RESULT := - 1;

  case FCheckBoxes.ColumnMode of
    cmByIndex:
      begin
        if FCheckBoxes.FColumnIndex = -1
        then RESULT := Columns.Count-1
        else RESULT := FCheckBoxes.FColumnIndex;
      end;

    cmByFieldName:
      begin
        for i := 0 to Columns.Count-1 do
          if AnsiUpperCase(FCheckBoxes.FColumnFieldName) = AnsiUpperCase(Columns[i].FieldName)
          then begin
            RESULT := i;
            Break;
          end;

        if RESULT = - 1
        then RESULT := Columns.Count-1;
      end;

{      cmIndicatorsCol:
        RESULT := -1;       }
  end;
end;

function TcyBaseDBGrid.GetColumnIndex: Integer;
begin
  Result := Col - FirstDataColIndex;
end;

function TcyBaseDBGrid.ClientRectDataCells: TRect;
var SrcRect1, SrcRect2: TRect;
begin
  srcRect1 := CellRectX(FirstDataColIndex, FirstDataRowIndex);
  srcRect2 := CellRectX(ColCount-1, RowCount-1);
  UnionRect(RESULT, SrcRect1, SrcRect2);
end;

procedure TcyBaseDBGrid.ColWidthsChanged;
begin
  inherited;
  AdjustClientColumn;

  if Assigned(FOnColWidthsChanged)
  then FOnColWidthsChanged(self);
end;

procedure TcyBaseDBGrid.UpdateLayout;
begin
  BeginLayout;
  LayoutChanged;      // It seems not needed 05/07/2011 ...
  EndLayout;
end;

procedure TcyBaseDBGrid.LayoutChanged;
var R: Integer;
begin
  inherited LayoutChanged;

//  if FixedCols < ColCount-1 then
//    FixedCols := 2;

  // Indicator colwidth:
  if (dgIndicator in Options) and (ColCount > 0)
  then begin
    if FCustomLayoutOptions.FIndicatorColumnWidth <> 0
    then
      ColWidths[0] := FCustomLayoutOptions.FIndicatorColumnWidth
    else
      if (FCheckBoxes.ColumnMode = cmIndicatorsCol) and (FCheckBoxes.Visible)
      then ColWidths[0] := FCheckBoxes.Size + FCheckBoxes.Margin * 2;
  end;

  // Data RowHeight :
  if (FCustomLayoutOptions.FDataRowHeight <> 0) and (RowCount > 0)
  then begin
    if FCustomLayoutOptions.FTitleRowHeight = 0    // We need FCalculatedTitleRowHeight in order to avoid title height to be set with DefaultRowHeight
    then FCalculatedTitleRowHeight := RowHeights[0];

    DefaultRowHeight := FCustomLayoutOptions.FDataRowHeight;
  end;

  // Titles RowHeight (after DataRowHeight because we need FCalculatedTitleRowHeight) :
  if (dgTitles in Options) and (RowCount > 0)
  then
    if FCustomLayoutOptions.FTitleRowHeight = 0
    then begin
      if FCustomLayoutOptions.FDataRowHeight <> 0
      then RowHeights[0] := FCalculatedTitleRowHeight;
    end
    else
      RowHeights[0] := FCustomLayoutOptions.FTitleRowHeight;

  // Selected Data Row Height:
  if FCustomLayoutOptions.FActiveDataRowHeight <> 0
  then
    for R := 1 to RowCount - 1 do
      if R = Row
      then RowHeights[R] := FCustomLayoutOptions.FActiveDataRowHeight;
end;

function TcyBaseDBGrid.GetGraphicClass(GraphicClassField: TGraphicClassField): TGraphicClass;
begin
  case GraphicClassField of
    fcBitmap: RESULT := TBitmap;
    fcJpeg:   RESULT := TJPEGImage;
    fcIcon:   RESULT := TIcon;
    {$IFDEF DELPHI2009_OR_ABOVE}
    fcPng:    RESULT := TPngImage;
    {$ENDIF}
    else
              RESULT := TBitmap;
  end;
end;

// Also works:
{procedure TcyBaseDBGrid.DrawGraphicField(Rect: TRect; Field: TBlobField; FieldContent: TGraphicClass;
  Transparent: Boolean; bgStyle: TBgStyle; Position: TBgPosition);
var
  aGraphic: TGraphic;
  aStream: TStream;
begin
  if Field.IsNull then Exit;

  try
    aStream := Field.DataSet.CreateBlobStream(Field, bmRead);
    try
      aGraphic := FieldContent.Create;
      aGraphic.LoadFromStream(aStream);
      DrawGraphic(Canvas, Rect, aGraphic, Transparent, bgStyle, Position, 0, 0, 0, 0, 1, 1);
    finally
      aGraphic.Free;
    end;
  finally
    aStream.Free;
  end;
end;}

procedure TcyBaseDBGrid.DrawGraphicField(Rect: TRect; Field: TBlobField; FieldContent: TGraphicClass;
  Transparent: Boolean; bgStyle: TBgStyle; Position: TBgPosition);
var
  aGraphic: TGraphic;
  aStream: TMemoryStream;
begin
  if Field.IsNull then Exit;

  try
    aGraphic := FieldContent.Create;

    if Field is TGraphicField          // TGraphicField is descendent of TBlobField and can contain bitmap, ico etc ...
    then begin
      try
        aStream := TMemoryStream.Create;
        Field.SaveToStream(aStream);

        // Does not work with TBlobField:
        aStream.Position := 0;
        aGraphic.LoadFromStream(aStream);
      finally
        aStream.Free;
      end;
    end
    else
      aGraphic.Assign(Field);  // TBlobField handling

    DrawGraphic(Canvas, Rect, aGraphic, Transparent, bgStyle, Position, 0, 0, 0, 0, 1, 1);
  finally
    aGraphic.Free;
  end;
end;

procedure TcyBaseDBGrid.DrawMemoField(Rect: TRect; Field: TField; TextWordwrap, BgTransparent: Boolean; MaxCars, MaxLines: Integer);
var
  i, pMax, fPixelsperInch: Integer;
  FieldValue: String;

        function PixelsToTwips(PixelValue: Integer): Integer; // (Twips = 1/1440 inch)
        begin
          Result := PixelValue * 1440 div fPixelsperInch;
        end;

begin
  fPixelsperInch := GetDeviceCaps(canvas.Handle, LOGPIXELSX);
  FieldValue := Field.AsString;

  if (Copy(FieldValue, 1, 6) = '{\rtf1') and (FRichTextStream <> Nil)
  then begin
    // *** Draw rich text *** //

    // Background :
    if FRichEdit.Color <> Canvas.Brush.Color then
      FRichEdit.Color := Canvas.Brush.Color;         // Doesn' t work if wallpaper activated ...

    // Transparent :
    // Not working : if BgTransparent then SetBkMode(Canvas.Handle, TRANSPARENT);

    FRichTextStream.Size := 0;
    FRichTextStream.WriteString(FieldValue);
    FRichTextStream.Seek(0, soFromBeginning);
    FRichEdit.Lines.LoadFromStream(FRichTextStream);

    if FRichEdit.WordWrap <> TextWordwrap then
      FRichEdit.WordWrap := TextWordwrap;

    pMax := 0;  // Lat car to be seen

    // MaxLines :
    if MaxLines <> 0
    then begin
      for i := 1 to MaxLines do
      begin
        pMax := posEx(#$D#$A, FRichEdit.Text, pMax + 1);

        if pMax = 0    // Lines < MaxLines
        then Break;
      end;
    end;

    if pMax = 0
    then pMax := -1 // FRichEdit.GetTextLen   // All text
    else pMax := pMax - 1;

    // MaxCars :
    if MaxCars <> 0 then
      if (MaxCars < pMax) or (pMax = -1) then
        pMax := MaxCars;

    FFormatRange.hdc := Canvas.Handle;
    FFormatRange.hdcTarget := Canvas.Handle;
    // rc is the destination rect on the DBGrid:
    FFormatRange.rc := classes.Rect(PixelsToTwips(Rect.Left), PixelsToTwips(Rect.Top), PixelsToTwips(Rect.Right), PixelsToTwips(Rect.Bottom));
    FFormatRange.rcPage := FFormatRange.rc;
    FFormatRange.chrg.cpMin := 0;     // First car
    FFormatRange.chrg.cpMax := pMax;  // Max cars
    FRichEdit.Perform(EM_FORMATRANGE, 1, Longint(@FFormatRange));   // Draw text
    FRichEdit.Perform(EM_FORMATRANGE, 0, 0);  // Free cached data
    Canvas.Font.OnChange(Canvas);  // Restore Canvas.Font (Otherwise others fields values will be displayed with richText font on the DBGrid)

//    if BgTransparent then
//      SetBkMode(Canvas.Handle, OPAQUE);
  end
  else begin
    // *** Draw simple text *** //
    if BgTransparent then
      Canvas.Brush.Style := bsClear;

    // MaxCars :
    if MaxCars <> 0 then
      FieldValue := copy(FieldValue, 1, MaxCars);

    // MaxLines :
    if MaxLines <> 0
    then begin
      pMax := 0;
      for i := 1 to MaxLines do
      begin
        pMax := posEx(#$D#$A, FieldValue, pMax + 1);

        if pMax = 0    // Lines < MaxLines
        then Break;
      end;

      if pMax <> 0
      then FieldValue := copy(FieldValue, 1, pMax);
    end;

    DrawText(Canvas.Handle, PChar(FieldValue), Length(FieldValue), Rect, WordWraps[TextWordwrap] or dt_NoPrefix);
    if BgTransparent then
      Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TcyBaseDBGrid.DrawWordwrapTextField(Rect: TRect; Field: TField);
begin
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, PChar(Field.AsString), Length(Field.AsString), Rect, WordWraps[True] or dt_NoPrefix);
  Canvas.Brush.Style := bsSolid;
end;

{$IFDEF SCROLL_FEATURE}
procedure TcyBaseDBGrid.UpdateScrollBar;
begin
  inherited;
  if not DataLink.Active then Exit;

  // If any field data changed, UpdateScrollBar is also called.
  // So, we need to control if we have really moved to another record.
  if DataLink.DataSet.RecNo = LastScrollRecNo then Exit;

  LastScrollRecNo := DataLink.DataSet.RecNo;

  if Assigned(FonAfterScroll)
  then FonAfterScroll(Self);

  if Assigned(FonAfterScrollPause)
  then
    if FScrollPauseMSeconds <> 0
    then begin
      // Timer update/creation :
      if not Assigned(FTimerScroll)
      then begin
        FTimerScroll := TTimer.Create(Self.Owner);
        FTimerScroll.OnTimer := FTimerScrollTimer;
      end;

      FTimerScroll.Enabled := false;
      FTimerScroll.Interval := FScrollPauseMSeconds;
      FTimerScroll.Enabled := true;
    end
    else
      FonAfterScrollPause(Self);
end;
{$ENDIF}

{$IFDEF SCROLL_FEATURE}
procedure TcyBaseDBGrid.FTimerScrollTimer(Sender: TObject);
begin
  if DataLink.Active
  then begin
    FTimerScroll.Enabled := false;
    if Assigned(FonAfterScrollPause)
    then FonAfterScrollPause(Self);
  end;

  FTimerScroll.Free;
  FTimerScroll := Nil;
end;
{$ENDIF}

function TcyBaseDBGrid.GetRecordPosition: TRecordPosition;
begin
  if not DataLink.Active then Exit;
  Result.ActiveRecord := DataLink.ActiveRecord;
  Result.Bookmark := DataLink.DataSet.GetBookmark;
end;

function TcyBaseDBGrid.GotoRecordPosition(RecordPosition: TRecordPosition): Boolean;
var m, LastRow: Integer;
begin
  Result := false;
  if not DataLink.Active then Exit;

  if DataLink.DataSet.BookmarkValid(RecordPosition.Bookmark) then
  begin
    BeginUpdate;    // Avoid flickering
    DataLink.DataSet.GotoBookmark(RecordPosition.Bookmark);

    if DataLink.ActiveRecord <> RecordPosition.ActiveRecord
    then begin
      // *** We are not placed on the same row *** //
      if DataLink.ActiveRecord < RecordPosition.ActiveRecord
      then begin
        // Goto to first row and scroll DBGrid to put the bookmarked record on the correct row:
        m := DataLink.ActiveRecord + (RecordPosition.ActiveRecord - DataLink.ActiveRecord);
        DataLink.DataSet.MoveBy((-1) * m);

        // Goto to the correct record:
        DataLink.DataSet.MoveBy(RecordPosition.ActiveRecord);
      end
      else begin
        LastRow := RowCount-1;
        if dgTitles in Options
        then Dec(LastRow, 1);
        // Goto last row and scroll DBGrid ti put the bookmarked record on the correct row:
        m := (LastRow - DataLink.ActiveRecord) + (DataLink.ActiveRecord - RecordPosition.ActiveRecord);
        DataLink.DataSet.MoveBy(m);

        // Goto to the correct record:
        DataLink.DataSet.MoveBy(RecordPosition.ActiveRecord - DataLink.ActiveRecord);
      end;

      // Can fail if a record was added/removed:
      if DataLink.DataSet.CompareBookmarks(RecordPosition.Bookmark, DataLink.DataSet.GetBookmark) <> 0
      then DataLink.DataSet.GotoBookmark(RecordPosition.Bookmark);
    end;

    Result := DataLink.DataSet.CompareBookmarks(RecordPosition.Bookmark, DataLink.DataSet.GetBookmark) = 0;
    EndUpdate;
  end;
end;

procedure TcyBaseDBGrid.FreeRecordPosition(RecordPosition: TRecordPosition);
begin
  if not DataLink.Active then Exit;
  DataLink.DataSet.FreeBookmark(RecordPosition.Bookmark);
end;

procedure TcyBaseDBGrid.TestSomething(value: Integer);
begin
  //
end;

function TcyBaseDBGrid.PointInContentCheckBox(CellRect: TRect; X, Y: Integer): Boolean;
var RectText, RectCheckbox: TRect;
begin
  CalcContentCheckBoxColumnCellLayout(CellRect, RectText, RectCheckbox);
  Result := PointInRect(Point(X, Y), RectCheckbox);
end;

procedure TcyBaseDBGrid.CalcContentCheckBoxColumnCellLayout(const Rect: TRect; var RectText, RectCheckbox: TRect);
var LeftCheckBoxPos: Integer;
begin
  if FContentCheckBoxOptions.FAlignLeft
  then begin
    LeftCheckBoxPos := Rect.left + FContentCheckBoxOptions.FMargin;
    RectCheckBox := classes.Rect(LeftCheckBoxPos, Rect.Top, LeftCheckBoxPos + FContentCheckBoxOptions.FSize, Rect.Bottom);
    RectText := classes.Rect(LeftCheckBoxPos + FContentCheckBoxOptions.FSize + FContentCheckBoxOptions.FMargin, Rect.Top, Rect.Right, Rect.Bottom);
  end
  else begin
    LeftCheckBoxPos := Rect.Right - FContentCheckBoxOptions.FMargin - FContentCheckBoxOptions.FSize;
    RectCheckBox := classes.Rect(LeftCheckBoxPos, Rect.Top, LeftCheckBoxPos + FContentCheckBoxOptions.FSize, Rect.Bottom);
    RectText := classes.Rect(Rect.Left, Rect.Top, LeftCheckBoxPos - FContentCheckBoxOptions.FMargin, Rect.Bottom);
  end;
end;

procedure TcyBaseDBGrid.DrawBooleanField(Rect: TRect; Field: TField);
var
  RectText, RectCheckbox: TRect;
  Text: String;
  x: Integer;
  _Checked: Boolean;
begin
  CalcContentCheckBoxColumnCellLayout(Rect, RectText, RectCheckbox);

  // Draw CheckBox :
  if Field is TBooleanField
  then _Checked := Field.AsBoolean
  else _Checked := Field.AsString <> '0';
  DefaultDrawCheckBox(RectCheckBox, _Checked);

  // Draw Display value :
  Text := Field.AsString;

  if Field is TBooleanField then
    if TBooleanField(Field).DisplayValues <> '' then
    begin
      Text := TBooleanField(Field).DisplayValues;
      x := pos(';', text);

      if Field.AsBoolean then
      begin
        if x <> 0
        then Text := copy(Text, 1, x-1);
      end
      else begin
        if x <> 0
        then Text := copy(Text, x+1, length(Text))
        else Text := '';
      end;
    end;

  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), RectText, dt_NoPrefix or DT_SINGLELINE or DT_VCENTER);
  Canvas.Brush.Style := bsSolid;
end;

procedure TcyBaseDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, ColumnIndex: Integer;
  DoInherited, Handled: Boolean;
  aKey: Word;
begin
  SetCustomGridOptions;

  DoInherited := true;
  Handled := false;
  FMouseDownCell := MouseCoord(X, Y);
  ColumnIndex := FMouseDownCell.X - FirstDataColIndex;

  // Personalize LEFT CLICK //
  if (Button = mbLeft) and (DataLink.Active)
  then begin
    if FMouseDownCell.Y >= FirstDataRowIndex   // Not in title columns ...
    then begin
      // Handle Row CheckBox :
      if (not Handled) and CheckBoxes.FVisible then
        if (GetCheckBoxColumnIndex = ColumnIndex) then // DownCell.X - FirstDataColIndex) then
          if PointInCheckBox(CellRect(FMouseDownCell.X, FMouseDownCell.Y), X, Y) then
          begin
            Handled := true;
            DoInherited := false;

            if Row <> FMouseDownCell.Y
            then Inherited;               // Moving to the selected record ...

            if not CheckBoxes.FReadOnly
            then FCheckedList.CurrentRecordSwitch;

            if Assigned(FOnCheckBoxClick)
            then FOnCheckBoxClick(self);
          end;

      if (not Handled) and (ssShift in Shift) and (dgMultiSelect in Options) then    // Multi-selection
      begin
        if Row >= FirstDataRowIndex
        then begin
          if Row < FMouseDownCell.Y
          then aKey := vk_Down
          else aKey := vk_Up;

          if ssCtrl in Shift
          then Exclude(Shift, ssCtrl);   // Because Ctrl key unselect previous selected rows on OnKeyDown ...

          FExtendedAcceptKey := false;
          for i := 1 to abs(Row - FMouseDownCell.Y) - 1 do
            KeyDown(aKey, Shift);
          FExtendedAcceptKey := true;

          Include(Shift, ssCtrl);   // In order to select current record on Inherited line below!
        end;
      end;
    end
    else begin
      // Handle column moving :
//      if not (dgColumnResize in Options)   // TDBGrid don' t let moving columns ...
    end;
  end;

  if DoInherited
  then begin
    {$IFDEF DELPHI2009}
    {DELPHI 2009 BUG: Avoid deleting active cell field value on click (left or right click) outside any dbgrid's cell when DataSet not in browse mode}
    if (not EditorMode)
     and ((FMouseDownCell.X = -1) or (FMouseDownCell.Y = -1))  // Outside any cell
    then begin
      if not Focused
      then SetFocus;     // Avoid Inherited!
    end
    else
    {$ENDIF}

    Inherited;
  end;

  {$IFDEF DELPHI2009}
  {DELPHI 2009 BUG: OnMouseDown not called}
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
  {$ENDIF}

  // Handle Checkbox rendering for TBooleanFields :
  if (not Handled) and (ColumnIndex > -1) and (FContentFieldsRender.BooleanField = bfCheckBox) then
    if Columns[ColumnIndex].Field is TBooleanField then
    begin
      EditorMode := false;

      if PointInContentCheckBox(CellRect(FMouseDownCell.X, FMouseDownCell.Y), X, Y) then
      begin
        Handled := true;
        DoInherited := false;
        CheckBoxFieldSwitchValue(Columns[ColumnIndex]);
      end;
    end;

  // Handle data row dragging :
  if (FMouseJob = mjWait) and (FMoveRows) and (not Handled) and (DataLink.Active) then
  begin
    if (ColumnIndex > -1) and (FMouseDownCell.Y >= FirstDataRowIndex) then   // Not in title columns ...
      FMouseJob := mjPrepareMovingRows;
  end
  else
    FMouseJob := mjWait;
end;

procedure TcyBaseDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  _CellCoord, _OldCellCoord: TGridCoord;
  _CellRect: TRect;
  AcceptMove, AcceptPosition: Boolean;
begin
  inherited;

  if not DataLink.Active then
    Exit;

  // Handle data row dragging :
  if FMouseJob = mjPrepareMovingRows then
  begin
    AcceptMove := true;
    if Assigned(FOnBeginMoveRows) then
      FOnBeginMoveRows(Self, AcceptMove);

    if AcceptMove then
    begin
      Windows.SetCursor(Screen.Cursors[DragCursor]);
      FMouseJob := mjMovingRows;
    end;
  end;

  if FMouseJob = mjMovingRows then
  begin
    // Draw moving row line :
    _CellCoord := MouseCoord(X, Y);

    if _CellCoord.Y >= FirstDataRowIndex then     // Valid data row?
    begin
      AcceptPosition := true;
      if Assigned(FOnMoveRows) then
        FOnMoveRows(Self, _CellCoord, AcceptPosition);

      if AcceptPosition then
      begin
        _OldCellCoord := FMoveHere;
        FMoveHere := _CellCoord;

        if _OldCellCoord.Y <> _CellCoord.Y then
          InvalidateRow(_OldCellCoord.Y);

        if FMoveHere.Y <> FMouseDownCell.Y then
        begin
          _CellRect := CellRect(_CellCoord.X, _CellCoord.Y);
          Canvas.Pen.Color := FMoveRowsColor;
          Canvas.Pen.Width := 1;

          if FMoveRowRendering = mrFrame then
          begin
            // Left:
            Canvas.MoveTo(0, _CellRect.Top);
            Canvas.LineTo(0, _CellRect.Bottom);
            Canvas.MoveTo(1, _CellRect.Top);
            Canvas.LineTo(1, _CellRect.Bottom);

            // Top:
            Canvas.MoveTo(0, _CellRect.Top);
            Canvas.LineTo(ClientWidth-1, _CellRect.Top);
            Canvas.MoveTo(0, _CellRect.Top+1);
            Canvas.LineTo(ClientWidth-1, _CellRect.Top+1);

            // Right :
            Canvas.MoveTo(ClientWidth-2, _CellRect.Top);
            Canvas.LineTo(ClientWidth-2, _CellRect.Bottom);
            Canvas.MoveTo(ClientWidth-3, _CellRect.Top);
            Canvas.LineTo(ClientWidth-3, _CellRect.Bottom);

            // Bottom:
            Canvas.MoveTo(0, _CellRect.Bottom);
            Canvas.LineTo(ClientWidth-1, _CellRect.Bottom);
            Canvas.MoveTo(0, _CellRect.Bottom-1);
            Canvas.LineTo(ClientWidth-1, _CellRect.Bottom-1);
          end
          else
            if FMoveHere.Y < FMouseDownCell.Y then
            begin
              Canvas.MoveTo(0, _CellRect.Top);
              Canvas.LineTo(ClientWidth-1, _CellRect.Top);
              Canvas.MoveTo(0, _CellRect.Top+1);
              Canvas.LineTo(ClientWidth-1, _CellRect.Top+1);
            end
            else begin
              Canvas.MoveTo(0, _CellRect.Bottom);
              Canvas.LineTo(ClientWidth-1, _CellRect.Bottom);
              Canvas.MoveTo(0, _CellRect.Bottom-1);
              Canvas.LineTo(ClientWidth-1, _CellRect.Bottom-1);
            end;
        end;
      end;
    end;
  end
  else
    if FHotMode <> hmNone then
    begin
      // Draw hot cell :
      _OldCellCoord := FMouseOverCell;
      FMouseOverCell := MouseCoord(X, Y);

      if ((_OldCellCoord.X <> FMouseOverCell.X) and (HotMode <> hmRow)) or (_OldCellCoord.Y <> FMouseOverCell.Y) then
        if HotMode = hmRow then
        begin
          InvalidateRow(_OldCellCoord.Y);
          InvalidateRow(FMouseOverCell.Y);
        end
        else begin
          InvalidateCell(_OldCellCoord.X, _OldCellCoord.Y);
          InvalidateCell(FMouseOverCell.X, FMouseOverCell.Y);
        end;
    end;
end;

procedure TcyBaseDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseJob = mjPrepareMovingRows then
    FMouseJob := mjWait;

  if FMouseJob = mjMovingRows then
  begin
    InvalidateRow(FMoveHere.Y);
    FMouseJob := mjWait;

    if Assigned(FOnEndMoveRows) then
      FOnEndMoveRows(Self, FMouseDownCell, FMoveHere);
  end;
end;

// The first time DBGrid enter in edit mode ...
function TcyBaseDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  FSavInplaceEdit := TcyDBGridInplaceEdit(Result);
end;

// Last function called before show Editor
function TcyBaseDBGrid.GetEditLimit: Integer;
begin
  Result := Inherited GetEditLimit;

  if not DataLink.Active then Exit;
  if not Assigned(SelectedField) then Exit;

  FSavInplaceEdit.Color := FEditorOptions.FColor;
//  if FSavInplaceEdit is TCustomComboBox then
//    TCustomComboBox(FSavInplaceEdit).Style := csDropDownList;

  FEditorOptions.IsIntegerField := SelectedField.DataType in [ftSmallint, ftInteger, ftWord, ftLargeint{$IFDEF DELPHI2009_OR_ABOVE}, ftShortint, ftByte{$ENDIF}];
  FEditorOptions.IsFloatField := SelectedField.DataType in [ftFloat, ftCurrency];
  FEditorOptions.CanParse := (FEditorOptions.IsIntegerField or FEditorOptions.IsFloatField) and FEditorOptions.Parse;

  if Assigned(FOnShowEditor) then
  begin
    FSavInplaceEdit.MaxLength := Result;
    FOnShowEditor(Self, FSavInplaceEdit);
    Result := FSavInplaceEdit.MaxLength;
  end;
end;

// Editor :
function TcyBaseDBGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  // Result := Inherited;
  Result := false;
  if SelectedIndex = -1 then Exit;

  with Columns[SelectedIndex] do
    if Datalink.Active and Assigned(Field) then
    begin
      Result := Field.IsValidChar(Key) or FEditorOptions.CanParse;

      if Assigned(FOnEditorAcceptKey) then
        FOnEditorAcceptKey(Self, Key, Result);
    end;
end;

// Assign InplaceEditor value to field:
// InplaceEditor accept + and - operators
procedure TcyBaseDBGrid.SetEditText(ACol, ARow: Integer; const Value: string);
var
  ModifiedValue: String;
  Parse: Boolean;
  Parser: TcyMathParser;
begin
  ModifiedValue := Value;
  Parse := FEditorOptions.CanParse;

  if Assigned(FEditorSetFieldText) then
    FEditorSetFieldText(Self, ModifiedValue, Parse);

  // Parse value :
  if Parse and (Value <> '') then
  begin
    Parser := TcyMathParser.Create(Self);
    Parser.Expression := ModifiedValue;
    if Parser.Parse then
    begin
      if FEditorOptions.IsIntegerField
      then ModifiedValue := IntToStr(Round(Parser.ParserResult))
      else ModifiedValue := floatToStr(Parser.ParserResult);
    end;
    Parser.Free;
  end;

  Inherited SetEditText(ACol, ARow, ModifiedValue);
end;

// Scrollbar MouseDown handling (not handle DblClick):
procedure TcyBaseDBGrid.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
begin
  MouseJob := mjScrollBarScrolling;
  if Assigned(FOnScrollbarMouseDown) then
    FOnScrollbarMouseDown(Self, mbLeft, [], Msg.XCursor, Msg.YCursor);

  Inherited;
end;

procedure TcyBaseDBGrid.WMNCHITTEST(var Msg: TWMNCHitTest);
begin
  Inherited;

  if MouseJob = mjScrollBarScrolling then
  begin
    MouseJob := mjWait;

    if Assigned(FOnScrollbarMouseUp) then
      FOnScrollbarMouseUp(Self, mbLeft, [], Msg.XPos, Msg.YPos);
  end;
end;

end.
