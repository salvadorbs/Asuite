  { @abstract(This unit contains the TKGrid component and all supporting classes)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(15 Oct 2006)
  @lastmod(07 Dec 2010)

  Copyright © 2006 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  This unit provides an enhanced replacement for components contained
  in Grids.pas. Major features:
  <UL>
  <LI><I>95% compatible with TDraw(String)Grid</I></LI>
  <LI><I>any TWinControl descendant can be used as inplace editor</I></LI>
  <LI><I>cell clipping and double buffering</I></LI>
  <LI><I>cell merging/splitting</I></LI>
  <LI><I>column/row/grid autosizing</I></LI>
  <LI><I>cross platform control in Lazarus</I></LI>
  <LI><I>index mapping - current column/row indexes can be mapped to their initial values</I></LI>
  <LI><I>last row/column aligning (no scrollbar)</I></LI>
  <LI><I>printing and previewing</I></LI>
  <LI><I>row/column hiding with optional visual indication</I></LI>
  <LI><I>rows, columns and cells are classes</I></LI>
  <LI><I>several styles possible when moving/sizing cells</I></LI>
  <LI><I>sorting interface</I></LI>
  <LI><I>Unicode control</I></LI>
  <LI><I>various text output attributes (multiline text, end ellipsis etc.)</I></LI>
  <LI><I>versatile cell painting interface</I></LI>
  <LI><I>virtual grid option</I></LI>
  </UL>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KGrids;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  KFunctions, KGraphics, KControls, Types
{$IFDEF TKGRID_USE_JCL}
  , JclUnicode
{$ENDIF}
  ;

type
  { Declares possible values for the Mask parameter in the @link(TKCustomGrid.GetAxisInfoHorz)
    or @link(TKCustomGrid.GetAxisInfoVert) functions. }
  TKGridAxisInfoMaskMembers = (
    { The FixedBoundary field in the @link(TKGridAxisInfo) structure will be evaluated. }
    aiFixedParams,
    { The GridExtent field in the @link(TKGridAxisInfo) structure will be evaluated. }
    aiGridExtent,
    { The GridBoundary and GridCells fields in the @link(TKGridAxisInfo) structure will be evaluated. }
    aiGridBoundary,
    { The FullVisBoundary and FullVisCells fields in the @link(TKGridAxisInfo) structure will be evaluated. }
    aiFullVisBoundary
  );

  { Set type for @link(TKGridAxisInfoMaskMembers) enumeration. }
  TKGridAxisInfoMask = set of TKGridAxisInfoMaskMembers;

  { Method type for the CellExtent field in the @link(TKGridAxisInfo) structure. }
  TKGridGetExtentFunc = function(Index: Integer): Integer of object;

  { Method type for the CanResize field in the @link(TKGridAxisInfo) structure. }
  TKGridCanResizeFunc = function(var Index, Pos: Integer): Boolean of object;

 { @abstract(Declares a structure returned by the @link(TKCustomGrid.GetAxisInfoHorz)
      or @link(TKCustomGrid.GetAxisInfoVert) functions)
    This structure contains information either about columns or rows, depending on what
    function returned the structure.
    <UL>
    <LH>Members:</LH>
    <LI><I>InfoMask</I> - set of parameters that specify what fields in this structure
    need to be evaluated.</LI>
    <LI><I>AlignLastCell</I> - specifies if the last cell is aligned to client
    area extent - see @link(goAlignLastCol) or @link(goAlignLastRow) for details.</LI>
    <LI><I>FixedSelectable</I> - reflects the gxEditFixedCols or gxEditFixedRows.</LI>
    <LI><I>CanResize</I> - this is the pointer to a function that determines
    if a column or row can be resized - i.e. either BeginColSizing or BeginRowSizing methods.</LI>
    <LI><I>CellExtent</I> - this is the pointer to a function that evaluates cell
    width or height - i.e. either GetColWidths or GetColHeights (private members).</LI>
    <LI><I>EffectiveSpacing</I> - specifies the effective space between cells
    as returned by @link(TKCustomGrid.EffectiveColSpacing) or
    @link(TKCustomGrid.EffectiveRowSpacing).</LI>
    <LI><I>FixedCellCount</I> - specifies the amount of fixed columns or rows -
    see @link(TKCustomGrid.FixedCols) or @link(TKCustomGrid.FixedRows) for details.</LI>
    <LI><I>FirstGridCell</I> - specifies the first visible non-fixed cell as given
    by @link(TKCustomGrid.LeftCol) or @link(TKCustomGrid.TopRow).</LI>
    <LI><I>FirstGridCellExtent</I> - specifies the maximum value for the first visible
    non-fixed cell as given by TKCustomGrid.FTopLeftExtent.</LI>
    <LI><I>ClientExtent</I> - this is either the TControl.ClientWidth or
    TControl.ClientHeight value.</LI>
    <LI><I>MinCellExtent</I> - specifies the minimum cell extent as given by
    @link(TKCustomGrid.InternalGetMinColWidth) or @link(TKCustomGrid.InternalGetMinRowHeight).</LI>
    <LI><I>MaxCellExtent</I> - specifies the maximum cell extent as given by
    @link(TKCustomGrid.InternalGetMaxColWidth) or @link(TKCustomGrid.InternalGetMaxRowHeight).</LI>
    <LI><I>TotalCellCount</I> - specifies the total cell amount in desired direction
    as given by @link(TKCustomGrid.ColCount) or @link(TKCustomGrid.RowCount).</LI>
    <LI><I>FixedBoundary</I> - specifies the point in pixels where the
    first non-fixed cell begins.</LI>
    <LI><I>GridBoundary</I> - specifies the grid extent as returned by the
    @link(TKCustomGrid.GridWidth) or @link(TKCustomGrid.GridHeight) properties.</LI>
    <LI><I>GridCells</I> - gives the amount of cells that correspond to GridBoundary.</LI>
    <LI><I>FullVisBoundary</I> - specifies the point in pixels where the
    last fully visible cell ends.</LI>
    <LI><I>FullVisCells</I> - gives the amount of cells that correspond to FullVisBoundary.</LI>
    <LI><I>GridExtent</I> - returns the extent of all cells in the grid.</LI>
    </UL> }
  TKGridAxisInfo = record
    InfoMask: TKGridAxisInfoMask;
    // col/row independent parameters
    AlignLastCell: Boolean;
    FixedSelectable: Boolean;
    CanResize: TKGridCanResizeFunc;
    CellExtent: TKGridGetExtentFunc;
    EffectiveSpacing: TKGridGetExtentFunc;
    FixedCellCount: Integer;
    FirstGridCell: Integer;
    FirstGridCellExtent: Integer;
    ClientExtent: Integer;
    MinCellExtent: TKGridGetExtentFunc;
    MaxCellExtent: TKGridGetExtentFunc;
    TotalCellCount: Integer;
    ScrollOffset: Integer;
    // calculated parameters
    FixedBoundary: Integer;
    GridBoundary: Integer;
    GridCells: Integer;
    FullVisBoundary: Integer;
    FullVisCells: Integer;
    GridExtent: Int64;
  end;

  { @abstract(Declares a structure returned by the
      @link(TKCustomGrid.GetAxisInfoBoth) function)
    <UL>
    <LH>Members:</LH>
    <LI><I>Horz</I> - structure as returned by @link(TKCustomGrid.GetAxisInfoHorz).</LI>
    <LI><I>Vert</I> - structure as returned by @link(TKCustomGrid.GetAxisInfoVert).</LI>
    </UL> }
  TKGridAxisInfoBoth = record
    Horz, Vert: TKGridAxisInfo;
  end;

  { Declares possible values for the Flags parameter in the
    @link(TKCustomGrid.UpdateAxes) method. }
  TKGridAxisUpdateFlag = (
    { Forces the @link(TKCustomGrid.OnColWidthsChanged) and/or
      @link(TKCustomGrid.OnRowHeightsChanged) to be called even if no column width/
      row height has been modified by this call of UpdateAxes. }
    afCallEvent,
    { Ensures all columns/rows have at least the minimum width/height as specified
      by @link(TKCustomGrid.MinColWidth)/@link(TKCustomGrid.MinRowHeight). }
    afCheckMinExtent
  );

  { Set type for @link(TKGridAxisUpdateFlags) enumeration. }
  TKGridAxisUpdateFlags = set of TKGridAxisUpdateFlag;

  { Declares possible values for the State parameter in the
    @link(TKCustomGrid.SuggestDrag) or @link(TKCustomGrid.SuggestSizing)
    functions. }
  TKGridCaptureState = (
    { Suggestion is about to start - e.g. user clicked the movable column by mouse. }
    csStart,
    { Suggestion is about to temporarily hide - e.g. user drags a column by mouse
      and the grid needs to be updated. }
    csHide,
    { Suggestion is about to show again - e.g. user drags a column by mouse
      and the grid was updated. }
    csShow,
    { Suggestion is about to stop - e.g. user released the mouse button
      and the dragged column need to be actually moved. }
    csStop
  );

  { @abstract(Declares a structure that holds both column and row span of a cell)
    <UL>
    <LH>Members:</LH>
    <LI><I>ColSpan</I> - column span.</LI>
    <LI><I>RowSpan</I> - row span.</LI>
    </UL> }
  TKGridCellSpan = record
    ColSpan: Integer;
    RowSpan: Integer;
  end;

  { @abstract(Declares a structure that hold both column and row index of a cell)
    <UL>
    <LH>Members:</LH>
    <LI><I>Col</I> - coordinate or index of a column.</LI>
    <LI><I>Row</I> - coordinate or index of a row.</LI>
    </UL> }
  TKGridCoord = record
    Col: Integer;
    Row: Integer;
  end;

  { Declares possible indexes e.g. for the @link(TKGridColors.Color) property. }
  TKGridColorIndex = Integer;

  { Declares possible values for the @link(TKGridColors.ColorScheme) property. }
  TKGridColorScheme = (
    { GetColor returns normal color currently defined for each item. }
    csNormal,
    { GetColor returns gray for text. }
    csGrayed,
    { GetColor returns brighter version of normal color. }
    csBright,
    { GetColor returns grayscaled color versions. }
    csGrayScale
  );

  { Method type for the Compare parameter e.g. in the
    @link(TKCustomGrid.InternalQuickSortNR) method. }
  TKGridCompareProc = function(ByIndex, Index1, Index2: Integer): Integer of object;

  { Declares possible values for the @link(TKCustomGrid.DisabledDrawStyle) property. }
  TKGridDisabledDrawStyle = (
    { The lines will be painted with brighter colors when control is disabled. }
    ddBright,
    { The lines will be painted with gray text and white background when control is disabled. }
    ddGrayed,
    { The lines will be painted normally when control is disabled. }
    ddNormal
  );

  { Declares possible values for the @link(TKCustomGrid.DragStyle) property. }
  TKGridDragStyle = (
    { The moved column or row is displayed beneath mouse cursor in a layered window (Win2k+)
      or white window (other OS) with fading opacity. }
    dsLayeredConst,
    { The moved column or row is displayed beneath mouse cursor in a layered window (Win2k+)
      or white window (other OS) with constant opacity. }
    dsLayeredFaded,
    { The moved column or row is not displayed, behavior of original TCustomGrid
      but line has red color. }
    dsLine,
    { The moved column or row is not displayed, behavior of original TCustomGrid. }
    dsXORLine
  );

  { Declares possible values for the State parameter in the
    @link(TKGridDrawCellEvent) event handler or @link(TKGridCell.DrawCell) method. }
  TKGridDrawStateMembers = (
    { The cell has input focus and is currently edited. This is always 1 cell
      that correspond to @link(TKCustomGrid.Col) and @link(TKCustomGrid.Row)
      properties. Painting of the cell is automatically invoked to allow e.g.
      background filling for inplace editors that don't fill the entire cell area. }
    gdEdited,
    { The cell is in the fixed region of the grid. }
    gdFixed,
    { The cell has input focus. This is always 1 cell that correspond to
      @link(TKCustomGrid.Col) and @link(TKCustomGrid.Row) properties. }
    gdFocused,
    { Left mouse button is pressed. Cell repainting
      is automatically invoked when mouse cursor is over the cell and the left
      button is pressed. The cell is repainter if mouse button is released, either. }
    gdMouseDown,
    { Mouse cursor is over the cell in @link(goMouseOverCells) mode. Cell repainting
      is automatically invoked when mouse cursor enters or leaves the cell.
      Furthermore, if @link(TKCustomGrid.EditorMode) is True, the inplace editor will
      be invalidated to allow proper editor underpainting, either. }
    gdMouseOver,
    { The cell is currently selected. This includes all cells that appear with
      different default background color in either @link(goRangeSelect) or
      @link(goRowSelect) mode. }
    gdSelected,
    { The cell belongs to the sorted column or row. }
    gdSorted,
    { Applies to the left most fixed column (if any).
      The cell should paint visual shape like arrow to indicate that the
      columns are sorted from lowest to highest value, like 'A' to 'Z'. }
    gdColsSortedUp,
    { Applies to the left most fixed column (if any).
      The cell should paint visual shape like arrow to indicate that the
      columns are sorted from highest to lowest value, like 'Z' to 'A'. }
    gdColsSortedDown,
    { Applies to the top most fixed row (if any).
      The cell should paint visual shape like arrow to indicate that the
      rows are sorted from lowest to highest value, like 'A' to 'Z'. }
    gdRowsSortedUp,
    { Applies to the top most fixed row (if any).
      The cell should paint visual shape like arrow to indicate that the
      rows are sorted from highest to lowest value, like 'Z' to 'A'. }
    gdRowsSortedDown
  );

  { Set type for @link(TKGridDrawStateMembers) enumeration. }
  TKGridDrawState = set of TKGridDrawStateMembers;

  { Declares possible values for the @link(TKCustomGrid.EditorTransparency) property. }
  TKGridEditorTransparency = (
    { The grid decides which inplace editor should be treated as a transparent
      control. This method works for all standard VCL controls. }
    etDefault,
    { Current inplace editor should be treated as opaque, i.e. not transparent. }
    etNormal,
    { Current inplace editor should be treated as transparent. }
    etTransparent
  );

  { Method type for the Exchange parameter e.g. in the
    @link(TKCustomGrid.InternalQuickSortNR) method. }
  TKGridExchangeProc = procedure(Index1, Index2: Integer) of object;

  { Declares possible values for the InvisibleCells parameter in the
    @link(TKCustomGrid.PointToCell) method. }
  TKGridInvisibleCells = (
    { No invisible cells will be taken into account. Invisible cells are those
      that are hidden (non-fixed) to the left or top. }
    icNone,
    { Invisible cells to the left will be taken into account at the expense of
      the (possible) fixed cells. }
    icFixedCols,
    { Invisible cells to the top will be taken into account at the expense of
      the (possible) fixed cells. }
    icFixedRows,
    { All invisible cells will be taken into account at the expense of
      the (possible) fixed cells. }
    icCells
  );

  { Declares a structure for hidden cell indicator glyphs. }
  TKGridHCIBitmaps = record
    HBegin, HCenter, HEnd,
    VBegin, VCenter, VEnd: TKAlphaBitmap;
  end;

  { Declares possible values for the @link(TKCustomGrid.Options) property. }
  TKGridOption = (
    { Tries to put all columns to the visible area of the grid and omit free space
      to the right of the last cell. No horizontal scrollbar appears. }
    goAlignLastCol,
    { Tries to put all rows to the visible area of the grid and omit free space
      below the last cell. No vertical scrollbar appears. }
    goAlignLastRow,
    { The grid is locked into edit mode. The user does not need to use Enter or F2
      to turn on EditorMode everytime he moves to another cell. The behavior is
      slightly different as in TCustomGrid. }
    goAlwaysShowEditor,
    { Enables the WM_ERASEBKGND message to be handled if True. This can be used
      to avoid grid flickering for the case the grid is placed into a container
      that requires repainting itself and all of its children after resizing.
      This behavior is typical for nested TPanels. If you don't use these you
      can set this option True to erase the background. The grid does not need
      the background to be erased as it fills the entire client area through
      WM_PAINT. But some users might need to erase the background due to the
      strange behavior when activating an application by clicking the main form's
      title bar. }
    goEraseBackground,
    { No painting is allowed beyond the cell outline. }
    goClippedCells,
    { Scrollable columns can be moved using the mouse. }
    goColMoving,
    { Scrollable columns can be individually resized. }
    goColSizing,
    { Scrollable columns can be sorted by mouse click at the first fixed row. The
      sorted column is visually indicated by arrow at the first fixed row. }
    goColSorting,
    { Instructs the cell painter to draw each cell with double buffering to
      avoid cell flickering. }
    goDoubleBufferedCells,
    { Selected cells are drawn with with a focus rectangle if the grid (not the
      inplace editor) has the input focus. The behavior is slightly different
      as in TCustomGrid. }
    goDrawFocusSelected,
    { Users can edit the contents of cells. No another limitation applicable. }
    goEditing,
    { If included, Enter does not turn on EditorMode, but causes another cell to
      be focused. What cell it is depends on @link(TKCustomGrid.MoveDirection). }
    goEnterMoves,
    { Horizontal lines are drawn to separate the fixed (nonscrolling) rows
      in the grid. }
    goFixedHorzLine,
    { Vertical lines are drawn to separate the fixed (nonscrolling) columns
      in the grid. If @link(TKCustomGrid.ThemedCells) is True, these are not
      drawn for fixed rows by default, as these are meant as a grid header. }
    goFixedVertLine,
    { Draws cells in the first fixed row in standard Win-API header style. }
    goHeader,
    { Terminates the first fixed row drawn in standard Win-API header style
      by drawing a standard Win-API header terminator in an area not occupied by cells. }
    goHeaderAlignment,
    { Horizontal lines are drawn to separate the scrollable rows in the grid. }
    goHorzLine,
    { Hidden columns or rows are indicated in fixed cell area. }
    goIndicateHiddenCells,
    { Selection is indicated in the fixed cells by a specific color. }
    goIndicateSelection,
    { Columns or rows can be hidden with the mouse while being resized. Set to
      False to enforce KGrid 1.2 behavior. }
    goMouseCanHideCells,
    { If included, then if the mouse enters or leaves a cell, these cells
      will be invalidated and the cell under the mouse pointer gets a
      @link(gdMouseOver) state. }
    goMouseOverCells,
    { If included, no text will be selected in the inplace editor upon its creation.
      This applies only to inplace editors having a selectable text, of course.
      The default behavior works only for editors responding to EM_SETSEL message.
      For another editors, the behavior can be maintained by the
      @link(TKCustomGrid.OnEditorSelect) event handler. }
    goNoSelEditText,
    { Users can select ranges of cells at one time. No another limitation applicable. }
    goRangeSelect,
    { Scrollable rows can be moved using the mouse. }
    goRowMoving,
    { Entire rows are selected rather than individual cells. No another limitation applicable. }
    goRowSelect,
    { Scrollable rows can be individually resized. Caution: Some inplace editors
      cannot be resized in height - for example TComboBox. }
    goRowSizing,
    { Scrollable rows can be sorted by mouse click at the first fixed column. The
      sorted row is visually indicated by arrow at the first fixed column. }
    goRowSorting,
    { Users can navigate through the cells in the grid using Tab and Shift+Tab. }
    goTabs,
    { Enables OS themes for both non-client and cells. }
    goThemes,
    { Enables OS themes for cells. }
    goThemedCells,
    { Vertical lines are drawn to separate the scrollable columns in the grid. }
    goVertLine,
    { If included, the grid becomes virtual grid. In this mode, data for the cells
      must be supplied externally. No cell class instances are allocated.
      @link(TKCustomGrid.Cell) property cannot be set and returns always nil.
      @link(TKCustomGrid.Cells) property cannot be set and returns always empty string.
      @link(TKCustomGrid.FCells) field is always nil - no grid structure is allocated.
      Column and Row structures (@link(TKCustomGrid.FCols) and
      @link(TKCustomGrid.FRows)) remain always allocated. }
    goVirtualGrid
  );

  { Set type for @link(TKGridOption) enumeration. }
  TKGridOptions = set of TKGridOption;

  { Declares possible values for the @link(TKCustomGrid.OptionsEx) property. }
  TKGridOptionEx = (
    { When Inplace editor has horizontal constraint it will be horizontally centered. }
    gxEditorHCenter,
    { When Inplace editor has vertical constraint it will be vertically centered. }
    gxEditorVCenter,
    { Pressing Enter at the last cell appends a row. }
    gxEnterAppendsRow,
    { Pressing Enter wraps selection to next column/row. }
    gxEnterWraps,
    { Clicking fixed cells together with Shift key selects/unselects respective columns/rows. }
    gxFixedCellClickSelect,
    { All fixed cells will be painted with header theme (looks bad e.g. with classic WinXP). }
    gxFixedThemedCells,
    { Pressing TAB at the last cell appends a row. }
    gxTabAppendsRow,
    { Pressing TAB wraps selection to next column/row. }
    gxTabWraps,
    // aki:
    { Allow edit fixed rows}
    gxEditFixedRows,
    { Allow edit fixed cols}
    gxEditFixedCols
    );

  { Set type for @link(TKGridOptionEx) enumeration. }
  TKGridOptionsEx = set of TKGridOptionEx;

  { Declares possible values for the Priority parameter in the @link(TKCustomGrid.MeasureCell) method. }
  TKGridMeasureCellPriority = (
    { Row height stays, column width is adjusted. }
    mpColWidth,
    { Column width stays, row height is adjusted. }
    mpRowHeight,
    { Default cell extent adjustment. }
    mpCellExtent
  );

  { Declares possible values for the Command parameter in the @link(TKCustomGrid.InternalMove) method. }
  TKGridMoveCommand = (
    { No command. }
    mcNone,
    { Move to last row. }
    mcBottom,
    { Move to next row. }
    mcDown,
    { Move to last column. }
    mcEnd,
    { Move to first column. }
    mcHome,
    { Move to previous column. }
    mcLeft,
    { Move to bottom row on current page. }
    mcMoveDown,
    { Move to top row on current page. }
    mcMoveUp,
    { Move to next vertical page. }
    mcPageDown,
    { Move to next horizontal page. }
    mcPageLeft,
    { Move to previous horizontal page. }
    mcPageRight,
    { Move to previous vertical page. }
    mcPageUp,
    { Move to next column. }
    mcRight,
    { Move to first row. }
    mcTop,
    { Move to previous row. }
    mcUp
  );

  { Declares possible values for the @link(TKCustomGrid.MoveDirection) property. }
  TKGridMoveDirection = (
    { By pressing Enter, the cell below the currently focused cell will be focused. }
    mdDown,
    { By pressing Enter, the cell to the left of the currently focused cell will be focused. }
    mdLeft,
    { By pressing Enter, the cell to the right of the currently focused cell will be focused. }
    mdRight,
    { By pressing Enter, the cell above the currently focused cell will be focused. }
    mdUp
  );

  { Declares possible values for the @link(TKCustomGrid.RangeSelectStyle) property. }
  TKGridRangeSelectStyle = (
    { The focused cell is not the base cell and expands the selection. }
    rsDefault,
    { The focused cell is the base cell and does not expand the selection. }
    rsMS_Excel
  );

  { @abstract(Declares the type e.g. for the @link(TKCustomGrid.Selection) property)
    Declares the type for grid rectangle. A grid rectangle is a structure of
    two independent grid points.
    <UL>
    <LH>Members:</LH>
    <LI><I>Col1, Row1, Col2, Row2</I> - rectangle of grid cells given by indexes.</LI>
    <LI><I>Cell1, Cell2</I> - rectangle of grid cells given e.g. by top-left and bottom-right cells.</LI>
    </UL> }
  TKGridRect = record
    case Integer of
      0: (Col1, Row1, Col2, Row2: Integer);
      1: (Cell1, Cell2: TKGridCoord);
  end;

  { Declares possible values for the @link(TKCustomGrid.ScrollModeHorz) and @link(TKCustomGrid.ScrollModeVert) properties. }
  TKGridScrollMode = (
    { The trackbar scrolls per pixel. }
    smSmooth,
    { The trackbar scrolls per cell. }
    smCell
  );

  { Declares possible values for the Stage parameter in the @link(TKGridSelectionExpandEvent)
    event handler or @link(TKCustomGrid.SelectionMove) method. }
  TKGridSelectionStage = (
    { The selection moves entirely - the selection base cell changes. }
    ssInit,
    { The selection expands - the selection base cell remains unchanged. }
    ssExpand
  );

  { Declares possible values for the Flags parameter in the
    @link(TKCustomGrid.SelectionMove) method. }
  TKGridSelectionFlag = (
    { Do not call the @link(TKCustomGrid.SelectCell) method. }
    sfDontCallSelectCell,
    { Force invalidation of the old and new selection. }
    sfMustUpdate,
    { Force calling of the @link(TKCustomGrid.ClampInView) method. }
    sfClampInView,
    { Do not set @link(TKCustomGrid.FMemCol) and @link(TKCustomGrid.FMemRow) fields. }
    sfNoMemPos
  );

  { Set type for @link(TKGridSelectionFlag) enumeration. }
  TKGridSelectionFlags = set of TKGridSelectionFlag;

  { Declares possible values for the Change parameter in the
    @link(TKGridSizeChangedEvent) event handler. }
  TKGridSizeChange = (
    { Columns have been deleted. }
    scColDeleted,
    { Columns have been inserted. }
    scColInserted,
    { Rows have been deleted. }
    scRowDeleted,
    { Rows have been inserted. }
    scRowInserted
    );

  { Declares possible values for the @link(TKCustomGrid.SizingStyle) property. }
  TKGridSizingStyle = (
    { Column widths or row heights update after the mouse button is released.
      Old TCustomGrid behavior but line has red color. }
    ssLine,
    { Column widths or row heights update immediately. }
    ssUpdate,
    { Column widths or row heights update after the mouse button is released.
      Old TCustomGrid behavior. }
    ssXORLine
  );

  { Declares possible values for the @link(TKGridAxisItem.SortMode) property. }
  TKGridSortMode = (
    { Corresponding column or row is not sorted. }
    smNone,
    { Corresponding column or row is sorted from lowest to highest value. }
    smDown,
    { Corresponding column or row is sorted from highest to lowest value. }
    smUp
    );

  { Declares possible values for the @link(TKCustomGrid.SortStyle) property. }
  TKGridSortStyle = (
    { First click sorts from lowest to highest value, second click sorts from highest to lowest value. }
    ssDownUp,
    { First click sorts from lowest to highest value, second click sorts from highest to lowest value, third click turns sorting off. }
    ssDownUpNone,
    { First click sorts from highest to lowest value, second click sorts from lowest to highest value, third click turns sorting off. }
    ssUpDownNone
    );

  { Declares possible values for the @link(TKCustomGrid.FGridState) field. }
  TKGridState = (
    { The mouse button has been pressed on a fixed cell that triggers
      mouse click event. }
    gsClickWaiting,
    { The mouse button has been pressed on a fixed cell that triggers
      column dragging. }
    gsColMoveWaiting,
    { The user is dragging a column to a new position. }
    gsColMoving,
    { The user is changing the width of a column. }
    gsColSizing,
    { The mouse button has been pressed on a fixed cell that triggers
      column sorting. }
    gsColSortWaiting,
    { The grid layout is not changing. }
    gsNormal,
    { The mouse button has been pressed on a fixed cell that triggers
      row dragging. }
    gsRowMoveWaiting,
    { The user is dragging a row to a new position. }
    gsRowMoving,
    { The user is changing the height of a row. }
    gsRowSizing,
    { The mouse button has been pressed on a fixed cell that triggers
      row sorting. }
    gsRowSortWaiting,
    { The user is selecting a cell or row. }
    gsSelecting
    );

  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnBeginColDrag) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Origin</I> - row or column index where dragging should be started.</LI>
    <LI><I>MousePt</I> - position of mouse cursor.</LI>
    <LI><I>CanBeginDrag</I> - True by default to allow the dragging to be started.</LI>
    </UL> }
  TKGridBeginDragEvent = procedure(Sender: TObject; var Origin: Integer;
    const MousePt: TPoint; var CanBeginDrag: Boolean) of object;

  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnBeginColSizing) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Index</I> - index of a row or column that should be resized.</LI>
    <LI><I>Pos</I> - position of the sizing line
    (even if it actually doesn't exist in @link(ssUpdate) sizing mode).</LI>
    <LI><I>CanBeginSizing</I> - True by default to allow the sizing to be started.</LI>
    </UL> }
  TKGridBeginSizingEvent = procedure(Sender: TObject; var Index, Pos: Integer;
    var CanBeginSizing: Boolean) of object;

  { @abstract(Declares event handler for any cell notification events)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the corresponding cell.</LI>
    </UL> }
  TKGridCellEvent = procedure(Sender: TObject; ACol, ARow: Integer) of object;

  { @abstract(Declares event handler for any cell notification events)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AOldCol, AOldRow</I> - column and row indexes of the old cell.</LI>
    <LI><I>ANewCol, ANewRow</I> - column and row indexes of the new cell.</LI>
    </UL> }
  TKGridCellChangedEvent = procedure(Sender: TObject; AOldCol, AOldRow, ANewCol, ANewRow: Integer) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnMouseCellHint) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the corresponding cell.</LI>
    <LI><I>AShow</I> - True if hint should be displayed, otherwise False.</LI>
    </UL> }
  TKGridCellHintEvent = procedure(Sender: TObject; ACol, ARow: Integer; AShow: Boolean) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnCellSpan) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the cell whose span data is to be retrieved.</LI>
    <LI><I>Span</I> - resulting span data for that cell.</LI>
    </UL> }
  TKGridCellSpanEvent = procedure(Sender: TObject; ACol, ARow: Integer; var Span: TKGridCellSpan) of object;

  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnCheckColDrag) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Origin</I> - row or column index where dragging was started.</LI>
    <LI><I>Destination</I> - row or column index where dragging is about to end at this moment.</LI>
    <LI><I>MousePt</I> - position of mouse cursor.</LI>
    <LI><I>CanDrop</I> - True by default to allow the dropping to Destination.</LI>
    </UL> }
  TKGridCheckDragEvent = procedure(Sender: TObject; Origin: Integer;
    var Destination: Integer; const MousePt: TPoint; var CanDrop: Boolean) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnCompareCells))
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Col1</I> - column index of the first cell or @link(cInvalidIndex) -
      see @link(TKCustomGrid.InsertSortedCol).</LI>
    <LI><I>Row1</I> - row index of the first cell or @link(cInvalidIndex) -
      see @link(TKCustomGrid.InsertSortedRow).</LI>
    <LI><I>Col2</I> - column index of the second cell.</LI>
    <LI><I>Row2</I> - row index of the second cell.</LI>
    </UL>
    <UL>
    <LH>Returns:</LH>
    <LI>Negative value (<0) if the value of the first cell is lower than
      the value of the second cell.</LI>
    <LI>Positive value (>0) if the value of the first cell is greater than
      the value of the second cell.</LI>
    <LI>Zero if values of both cells are the same.</LI>
    </UL> }
  TKGridCompareCellsEvent = function(Sender: TObject; Col1, Row1, Col2, Row2: Integer):
    Integer of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnCustomSortCols) or
    @link(TKCustomGrid.OnCustomSortRows) events)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ByIndex</I> - column or row index to sort rows or columns by.</LI>
    <LI><I>SortMode</I> - the sorting mode to sort rows or columns.</LI>
    <LI><I>Sorted</I> - set to True to avoid default sorting to be called.</LI>
    </UL> }
  TKGridCustomSortEvent = procedure(Sender: TObject; ByIndex: Integer;
    SortMode: TKGridSortMode; var Sorted: Boolean) of object;
    
  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnDrawCell) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the cell being drawn.</LI>
    <LI><I>R</I> - location of cell on the canvas.</LI>
    <LI><I>State</I> - indicates the state of the cell.</LI>
    </UL> }
  TKGridDrawCellEvent = procedure(Sender: TObject; ACol, ARow: Integer;
    R: TRect; State: TKGridDrawState) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnEditorCreate) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the focused cell that
      is about to become edited cell.</LI>
    <LI><I>AEditor</I> - nil by default to indicate that no inplace editor is
      wanted for the cell. Assign any TWinControl instance to this Parameter
      to create a custom inplace editor for the cell. Always create new
      instance because it is owned by the grid and destroyed automatically
      if no longer needed.</LI>
    </UL> }
  TKGridEditorCreateEvent = procedure(Sender: TObject; ACol, ARow: Integer;
    var AEditor: TWinControl) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnEditorDataFromGrid)
    or @link(TKCustomGrid.OnEditorDataToGrid) events)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AEditor</I> - identifies the inplace editor.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the edited cell.</LI>
    <LI><I>AssignText</I> - Allows to automatically set the cell text
    to or from inplace editor. Set to False to disable this behavior.</LI>
    </UL> }
  TKGridEditorDataEvent = procedure(Sender: TObject; AEditor: TWinControl;
    ACol, ARow: Integer; var AssignText: Boolean) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnEditorDestroy) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AEditor</I> - identifies the inplace editor.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the edited cell.</LI>
    </UL> }
  TKGridEditorDestroyEvent = procedure(Sender: TObject; var AEditor: TWinControl;
    ACol, ARow: Integer) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnEditorKeyPreview) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AEditor</I> - identifies the inplace editor.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the edited cell.</LI>
    <LI><I>Key</I> - key code as passed to OnKeyDown, can be modified.</LI>
    <LI><I>Shift</I> - state of the control keys as passed to OnKeyDown.</LI>
    <LI><I>IsGridKey</I> - True by default to indicate that the key will be handled
    by the grid. Set to False to let the inplace editor handle the key.</LI>
    </UL> }
  TKGridEditorKeyPreviewEvent = procedure(Sender: TObject; AEditor: TWinControl;
    ACol, ARow: Integer; var Key: Word; Shift: TShiftState; var IsGridKey: Boolean) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnEditorResize) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AEditor</I> - identifies the inplace editor.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the edited cell.</LI>
    <LI><I>ARect</I> - initial bounding rectangle of the inplace editor.
    You can modify it in order to place the editor somewhere else within the cell.
    The inplace editor is always clipped within the cell.</LI>
    </UL> }
  TKGridEditorResizeEvent = procedure(Sender: TObject; AEditor: TWinControl;
    ACol, ARow: Integer; var ARect: TRect) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnEditorSelect) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AEditor</I> - identifies the inplace editor.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the edited cell.</LI>
    <LI><I>SelectAll</I> - all the text should be selected in the inplace editor.</LI>
    <LI><I>CaretToLeft</I> - caret should be positioned to the left.</LI>
    <LI><I>SelectedByMouse</I> - the cell has been selected by mouse.</LI>
    </UL> }
  TKGridEditorSelectEvent = procedure(Sender: TObject; AEditor: TWinControl;
    ACol, ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean) of object;

  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnEndColDrag) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Origin</I> - row or column index where dragging was started.</LI>
    <LI><I>Destination</I> - row or column index where dragging ends.</LI>
    <LI><I>MousePt</I> - position of mouse cursor.</LI>
    <LI><I>CanEndDrag</I> - True by default to allow the dragging to be ended.</LI>
    </UL> }
  TKGridEndDragEvent = procedure(Sender: TObject; Origin: Integer;
    Destination: Integer; const MousePt: TPoint; var CanEndDrag: Boolean) of object;

  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnEndColSizing) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Index</I> - index of a row or column that is being resized.</LI>
    <LI><I>Pos</I> - current position of the sizing line
    (even if it actually doesn't exist in @link(ssUpdate) sizing mode).</LI>
    <LI><I>CanEndSizing</I> - True by default to allow the resizing to be ended.</LI>
    </UL> }
  TKGridEndSizingEvent = procedure(Sender: TObject; Index, Pos: Integer;
    var CanEndSizing: Boolean) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnExchangeCols) or
    @link(TKCustomGrid.OnExchangeRows) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Index1</I> - index of the first column or row.</LI>
    <LI><I>Index2</I> - index of the second column or row.</LI>
    </UL> }
  TKGridExchangeEvent = procedure(Sender: TObject;
    Index1, Index2: Integer) of object;

  { @abstract(Declares event handler for any cell extent notification events)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>AIndex</I> - column or row index.</LI>
    </UL> }
  TKGridExtentEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  { @abstract(Declares event handler e.g. for the @link(TKCustomGrid.OnMeasureCell) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the cell being drawn.</LI>
    <LI><I>R</I> - location of cell on the canvas.</LI>
    <LI><I>State</I> - indicates the state of the cell.</LI>
    <LI><I>Priority</I> - specifies the cell measurement priority.</LI>
    <LI><I>Extent</I> - returns calculated cell extent.</LI>
    </UL> }
  TKGridMeasureCellEvent = procedure(Sender: TObject; ACol, ARow: Integer;
    R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority;
    var Extent: TPoint) of object;
    
  { @abstract(Declares event handler for the @link(TKCustomGrid.OnColumnMoved) or
    @link(TKCustomGrid.OnRowMoved) events)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>FromIndex</I> - initial position of the column or row being moved.</LI>
    <LI><I>ToIndex</I> - final position of the column or row being moved.</LI>
    </UL> }
  TKGridMovedEvent = procedure(Sender: TObject; FromIndex, ToIndex: Integer) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnSizeChanged) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Change</I> - identifies the change type.</LI>
    <LI><I>At</I> - index where column(s) or row(s) have been inserted or deleted.</LI>
    <LI><I>Count</I> - number of column(s) or row(s) that have been inserted or deleted.</LI>
    </UL> }
  TKGridSizeChangedEvent = procedure(Sender: TObject;
    Change: TKGridSizeChange; At, Count: Integer) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnSelectCell) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the cell that is about to be selected.</LI>
    <LI><I>CanSelect</I> - True by default to indicate that the cell can be selected.
    Set to False to inhibit selecting of this cell.</LI>
    </UL> }
  TKGridSelectCellEvent = procedure(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean) of object;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnSelectCell) event)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>ACol, ARow</I> - column and row indexes of the cell that is about to
    expand the current selection.</LI>
    <LI><I>CanExpand</I> - True by default to indicate that the cell
    can the selection. Set to False to inhibit further selection expanding.</LI>
    </UL> }
  TKGridSelectionExpandEvent = procedure(Sender: TObject; ACol, ARow: Integer;
    var CanExpand: Boolean) of object;

const
  { This message can be posted to TKGrid to (re)create inplace editor in
    special needs, e.g. outside of inplace editor events. }
  GM_RECREATE_EDITOR = KM_BASE + 10;

  { Constant for invalid column or row indexes. Currently, it is used internally.
    Functions @link(TKCustomGrid.InitialCol) and @link(TKCustomGrid.InitialRow)
    return this value in case of invalid parameters. }
  cInvalidIndex = -1;

  { This constant can be passed into the FirstCol or FirstRow parameter of the
    @link(TKCustomGrid.UpdateAxes) method. }
  cAll = -1;

  { Default value for the @link(TKCustomGrid.ColCount) property. }
  cColCountDef = 5;

  { Default value for the @link(TKCustomGrid.DefaultColWidth) property. }
  cDefaultColWidthDef = 64;

  { Default value for the @link(TKCustomGrid.DefaultRowHeight) property. }
  cDefaultRowHeightDef = 21;

  { Default value for the @link(TKCustomGrid.DisabledDrawStyle) property. }
  cDisabledDrawStyleDef = ddBright;

  { Default value for the @link(TKCustomGrid.DragStyle) property. }
  cDragStyleDef = dsLayeredFaded;

  { Default value for the @link(TKCustomGrid.EditorTransparency) property. }
  cEditorTransparencyDef = etDefault;

  { Default value for the @link(TKCustomGrid.FixedCols) property. }
  cFixedColsDef = 1;

  { Default value for the @link(TKCustomGrid.FixedRows) property. }
  cFixedRowsDef = 1;

  { Default value for the @link(TKCustomGrid.GridLineWidth) property. }
  cGridLineWidthDef = 1;

  { Minimum value for the @link(TKCustomGrid.MinColWidth) property. }
  cMinColWidthMin = 5;
  { Default value for the @link(TKCustomGrid.MinColWidth) property. }
  cMinColWidthDef = 10;

  { Minimum value for the @link(TKCustomGrid.MinRowHeight) property. }
  cMinRowHeightMin = 5;
  { Default value for the @link(TKCustomGrid.MinRowHeight) property. }
  cMinRowHeightDef = 10;

  { Minimum value for the @link(TKCustomGrid.MouseCellHintTime) property. }
  cMouseCellHintTimeMin = 100;
  { Maximum value for the @link(TKCustomGrid.MouseCellHintTime) property. }
  cMouseCellHintTimeMax = 10000;
  { Default value for the @link(TKCustomGrid.MouseCellHintTime) property. }
  cMouseCellHintTimeDef = 800;

  { Default value for the @link(TKCustomGrid.MoveDirection) property. }
  cMoveDirectionDef = mdRight;

  { Default value for the @link(TKCustomGrid.Options) property. }
  cOptionsDef = [goAlwaysShowEditor, goDrawFocusSelected,
    goEnterMoves, goFixedVertLine, goFixedHorzLine, goIndicateHiddenCells,
    goHeader, goHeaderAlignment, goHorzLine, goMouseCanHideCells,
    goMouseOverCells, goRangeSelect, goThemes, goThemedCells, goVertLine];

  { Default value for the @link(TKCustomGrid.OptionsEx) property. }
  cOptionsExDef = [gxEnterWraps, gxTABWraps];

  { Default value for the @link(TKCustomGrid.RangeSelectStyle) property. }
  cRangeSelectStyleDef = rsDefault;

  { Default value for the @link(TKCustomGrid.RowCount) property. }
  cRowCountDef = 5;

  { Default value for the @link(TKCustomGrid.ScrollBars) property. }
  cScrollBarsDef = ssBoth;

  { Minimum value for the @link(TKCustomGrid.ScrollSpeed) property. }
  cScrollSpeedMin = 50;
  { Maximum value for the @link(TKCustomGrid.ScrollSpeed) property. }
  cScrollSpeedMax = 1000;
  { Default value for the @link(TKCustomGrid.ScrollSpeed) property. }
  cScrollSpeedDef = 100;

  { Default value for the @link(TKCustomGrid.ScrollModeHorz) and @link(TKCustomGrid.ScrollModeVert) properties. }
  cScrollModeDef = smSmooth;

  { Default value for the @link(TKCustomGrid.SizingStyle) property. }
  cSizingStyleDef = ssUpdate;

  { Default value for the @link(TKCustomGrid.SortStyle) property. }
  cSortStyleDef = ssDownUp;

  { Default value for the @link(TKGridColors.CellBkGnd) color property. }
  cCellBkGndDef = clWindow;
  { Default value for the @link(TKGridColors.CellLines) color property. }
  cCellLinesDef = clBtnFace;
  { Default value for the @link(TKGridColors.CellText) color property. }
  cCellTextDef = clWindowText;
  { Default value for the @link(TKGridColors.DragSuggestionBkGnd) color property. }
  cDragSuggestionBkGndDef = clLime;
  { Default value for the @link(TKGridColors.DragSuggestionLine) color property. }
  cDragSuggestionLineDef = clBlack;
  { Default value for the @link(TKGridColors.FixedCellBkGnd) color property. }
  cFixedCellBkGndDef = clBtnFace;
  { Default value for the @link(TKGridColors.FixedCellIndication) color property. }
  cFixedCellIndicationDef = clCream;
  { Default value for the @link(TKGridColors.FixedCellLines) color property. }
  cFixedCellLinesDef = clWindowText;
  { Default value for the @link(TKGridColors.FixedCellText) color property. }
  cFixedCellTextDef = clBtnText;
  { Default value for the @link(TKGridColors.FixedThemedCellLines) color property. }
  cFixedThemedCellLinesDef = {$IFDEF USE_WINAPI}clBtnShadow{$ELSE}clWindowText{$ENDIF};
  { Default value for the @link(TKGridColors.FixedThemedCellHighlight) color property. }
  cFixedThemedCellHighlightDef = clBtnHighlight;
  { Default value for the @link(TKGridColors.FixedThemedCellShadow) color property. }
  cFixedThemedCellShadowDef = clBtnFace;
  { Default value for the @link(TKGridColors.FocusedCellBkGnd) color property. }
  cFocusedCellBkGndDef = clHighlight;
  { Default value for the @link(TKGridColors.FocusedCellText) color property. }
  cFocusedCellTextDef = clHighlightText;
  { Default value for the @link(TKGridColors.FocusedRangeBkgnd) color property. }
  cFocusedRangeBkGndDef = clHighlight; // to be brigtened
  { Default value for the @link(TKGridColors.FocusedRangeText) color property. }
  cFocusedRangeTextDef = clHighlightText;
  { Default value for the @link(TKGridColors.SelectedCellBkGnd) color property. }
  cSelectedCellBkGndDef = clBtnFace;
  { Default value for the @link(TKGridColors.SelectedCellText) color property. }
  cSelectedCellTextDef = clBtnText;
  { Default value for the @link(TKGridColors.SelectedRangeBkGnd) color property. }
  cSelectedRangeBkGndDef = clBtnFace; // to be brigtened
  { Default value for the @link(TKGridColors.SelectedRangeText) color property. }
  cSelectedRangeTextDef = clBtnText;
  // aki:
  { Default value for then @link(TKGridColors.SelectedFixedCell) color property. }
  cSelectedFixedCellBkGndDef = clCream;
  { Index for the @link(TKGridColors.CellBkGnd) property. }
  ciCellBkGnd = TKGridColorIndex(0);
  { Index for the @link(TKGridColors.CellLines) property. }
  ciCellLines = TKGridColorIndex(1);
  { Index for the @link(TKGridColors.CellText) property. }
  ciCellText = TKGridColorIndex(2);
  { Index for the @link(TKGridColors.DragSuggestionBkGnd) property. }
  ciDragSuggestionBkGnd = TKGridColorIndex(3);
  { Index for the @link(TKGridColors.DragSuggestionLine) property. }
  ciDragSuggestionLine = TKGridColorIndex(4);
  { Index for the @link(TKGridColors.FixedCellBkGnd) property. }
  ciFixedCellBkGnd = TKGridColorIndex(5);
  { Index for the @link(TKGridColors.FixedCellIndication) property. }
  ciFixedCellIndication = TKGridColorIndex(6);
  { Index for the @link(TKGridColors.FixedCellLines) property. }
  ciFixedCellLines = TKGridColorIndex(7);
  { Index for the @link(TKGridColors.FixedCellText) property. }
  ciFixedCellText = TKGridColorIndex(8);
  { Index for the @link(TKGridColors.FixedThemedCellLines) property. }
  ciFixedThemedCellLines = TKGridColorIndex(9);
  { Index for the @link(TKGridColors.FixedThemedCellHighlight) property. }
  ciFixedThemedCellHighlight = TKGridColorIndex(10);
  { Index for the @link(TKGridColors.FixedThemedCellShadow) property. }
  ciFixedThemedCellShadow = TKGridColorIndex(11);
  { Index for the @link(TKGridColors.FocusedCellBkGnd) property. }
  ciFocusedCellBkGnd = TKGridColorIndex(12);
  { Index for the @link(TKGridColors.FocusedCellText) property. }
  ciFocusedCellText = TKGridColorIndex(13);
  { Index for the @link(TKGridColors.FocusedRangeBkGnd) property. }
  ciFocusedRangeBkGnd = TKGridColorIndex(14);
  { Index for the @link(TKGridColors.FocusedRangeText) property. }
  ciFocusedRangeText = TKGridColorIndex(15);
  { Index for the @link(TKGridColors.SelectedCellBkGnd) property. }
  ciSelectedCellBkGnd = TKGridColorIndex(16);
  { Index for the @link(TKGridColors.SelectedCellText) property. }
  ciSelectedCellText = TKGridColorIndex(17);
  { Index for the @link(TKGridColors.SelectedRangeBkGnd) property. }
  ciSelectedRangeBkGnd = TKGridColorIndex(18);
  { Index for the @link(TKGridColors.SelectedRangeText) property. }
  ciSelectedRangeText = TKGridColorIndex(19);
  // aki:
  { Index for the @link(TKGridColors.SelectedFixedCell) property. }
  ciSelectedFixedCellBkGnd = TKGridColorIndex(20);
  // aki:
  { Maximum color array index }
  ciGridColorsMax = ciSelectedFixedCellBkGnd;

  { This internal flag is set if caret should be moved to the left side of the inplace editor. }
  cGF_CaretToLeft                 = $00000001;
  { This internal flag is set if the Set.. methods in @link(TKGridAxisItem) and
    @link(TKGridCell) and their descendants must not call any grid methods that
    could cause infinite recursion. }
  cGF_GridUpdates                 = $00000002;
  { This internal flag is set to allow column or row sizing at design time. }
  cGF_DesignHitTest               = $00000004;
  { This internal flag is set to prevent recursive calls while inplace editor is being updated. }
  cGF_EditorUpdating              = $00000008;
  { This internal flag is set to remember inplace editor state if the grid
    has no input focus. }
  cGF_EditorModeActive            = $00000010;
  { This internal flag is set if a cell is selected by mouse click. }
  cGF_SelectedByMouse             = $00000020;
  { This internal flag is set if a cell is 'through-clicked'. }
  cGF_ThroughClick                = $00000040;
  { This internal flag is set if a selectable grid area contains at least 1 merged cell. }
  cGF_SelCellsMerged              = $00000080;
  { This internal flag is set if enter key has been pressed and handled by the grid. }
  cGF_EnterPressed                = $00000100;

type
  TKCustomGrid = class;
  TKGridCell = class;

  { @abstract(Declares event handler for the @link(TKCustomGrid.OnCompareCellInstances))
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller.</LI>
    <LI><I>Cell1</I> - pointer to the first cell</LI>
    <LI><I>Cell2</I> - pointer to the second cell</LI>
    </UL>
    <UL>
    <LH>Returns:</LH>
    <LI>Negative value (<0) if the value of the first cell is lower than
      the value of the second cell.</LI>
    <LI>Positive value (>0) if the value of the first cell is greater than
      the value of the second cell.</LI>
    <LI>Zero if values of both cells are the same.</LI>
    </UL> }
  TKGridCompareCellInstancesEvent = function(Sender: TObject; Cell1, Cell2: TKGridCell):
    Integer of object;

  { @abstract(Base class to store column or row properties)
    This is the base class for storing column or row properties.
    It implements properties and methods that are common for columns and rows. }
  TKGridAxisItem = class(TObject)
  private
    FCanResize: Boolean;
    FExtent: Integer;
    FGrid: TKCustomGrid;
    FInitialPos: Integer;
    FMaxExtent: Integer;
    FMinExtent: Integer;
    FSortArrowIndex: Integer;
    FSortMode: TKGridSortMode;
    FTag: TObject;
    procedure SetMaxExtent(AValue: Integer);
    procedure SetMinExtent(AValue: Integer);
  protected
    FBackExtent: Integer;
    { Cell class aware version of @link(TKCustomGrid.OnBeginColDrag) or @link(TKCustomGrid.OnBeginRowDrag)
      events. See the @link(TKGridBeginDragEvent) type for parameter interpretation. }
    procedure BeginDrag(var Origin: Integer; const MousePt: TPoint;
      var CanBeginDrag: Boolean); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnCheckColDrag) or @link(TKCustomGrid.OnCheckRowDrag)
      events. See the @link(TKGridCheckDragEvent) type for parameter interpretation. }
    procedure CheckDrag(Origin: Integer; var Destination: Integer;
      const MousePt: TPoint; var CanDrop: Boolean); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEndColDrag) or @link(TKCustomGrid.OnEndRowDrag)
      events. See the @link(TKGridEndDragEvent) type for parameter interpretation. }
    procedure EndDrag(Origin, Destination: Integer; const MousePt: TPoint;
      var CanEndDrag: Boolean); virtual;
    { Read method for the @link(TKGridAxisItem.Objects) property. Without implementation. }
    function GetObjects(Index: Integer): TObject; virtual; abstract;
    { Read method for the @link(TKGridAxisItem.Strings) property. Without implementation. }
    function GetStrings(Index: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; virtual; abstract;
    { Read method for the @link(TKGridAxisItem.Visible) property. Without implementation. }
    function GetVisible: Boolean; virtual;
    { Write method for the @link(TKGridAxisItem.Extent) property. Without implementation. }
    procedure SetExtent(const Value: Integer); virtual; abstract;
    { Write method for the @link(TKGridAxisItem.Objects) property. Without implementation. }
    procedure SetObjects(Index: Integer; const Value: TObject); virtual; abstract;
    { Write method for the @link(TKGridAxisItem.SortArrowIndex) property. Without implementation. }
    procedure SetSortArrowIndex(Value: Integer); virtual; abstract;
    { Write method for the @link(TKGridAxisItem.SortMode) property. Without implementation. }
    procedure SetSortMode(const Value: TKGridSortMode); virtual; abstract;
    { Write method for the @link(TKGridAxisItem.Strings) property. Without implementation. }
    procedure SetStrings(Index: Integer; const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}); virtual; abstract;
    { Write method for the @link(TKGridAxisItem.Visible) property. Without implementation. }
    procedure SetVisible(Value: Boolean); virtual; abstract;
  public
    { Creates the instance. Do not create custom instances. All necessary
      TKGridAxisItem instances are created automatically by TKCustomGrid. }
    constructor Create(AGrid: TKCustomGrid); virtual;
    { Copies shareable properties of another TKGridAxisItem instances into this
      TKGridAxisItem instance. }
    procedure Assign(Source: TKGridAxisItem); overload; virtual;
    { Makes it possible to assign a list of strings contained in TStrings
      to the grid. This method is provided to retain compatibility with
      TStringGrid. It behaves exactly the same way as the corresponding method
      in TStringGrid. Without implementation. }
    procedure Assign(Source: TStrings); overload; virtual; abstract;
{$IFDEF TKGRID_USE_JCL}
    { Makes it possible to assign a list of strings contained in TWideStrings
      to the grid. Without implementation. }
    procedure Assign(Source: TWideStrings); overload; virtual; abstract;
{$ENDIF}
    { Abstract prototype. Sets text of all cells corresponding to this column or row to empty string. }
    procedure Clear; virtual; abstract;
    { Returns True if shareable properties of this TKGridAxisItem instance have
      the same value as those in Item. }
    function {$ifdef COMPILER12_UP}EqualProperties{$ELSE}Equals{$ENDIF}(Item: TKGridAxisItem): Boolean; virtual;
    { Shareable property. Determines if this column or row can be resized.
      This property virtually covers the @link(TKCustomGrid.OnBeginColSizing) or
      @link(TKCustomGrid.OnBeginRowSizing) events. }
    property CanResize: Boolean read FCanResize write FCanResize;
    { Shareable property. Determines the column width or row height.
      Do not write this property unless you write a TKCustomGrid descendant. }
    property Extent: Integer read FExtent write SetExtent;
    { Pointer to the grid. You will probably need it when implementing application
      specific behavior. }
    property Grid: TKCustomgrid read FGrid;
    { Non-shareable property. Determines the initial column or row position
      just after it was inserted into the grid. Do not write this property
      unless you write a TKCustomGrid descendant. }
    property InitialPos: Integer read FInitialPos write FInitialPos;
    { Specifies the maximum extent of this column or row. Set zero to disable check.
      Does not work (cannot work) in goAlignLast... mode. }
    property MaxExtent: Integer read FMaxExtent write SetMaxExtent;
    { Specifies the minimum extent of this column or row. Set zero to disable check.
      This setting overrides the @link(TKCustomGrid.MinColWidth) or
      @link(TKCustomGrid.MinRowHeight) setting. }
    property MinExtent: Integer read FMinExtent write SetMinExtent;
    { Provides access to the object cell instances corresponding to the column or
      row referred by this TKGridAxisItem instance. Provided to retain compatibility
      with TStringGrid. }
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    { Specifies the index of the fixed column or row where the sorting can be
      initiated/changed by mouse click and where the sorting arrow will be displayed.
      This applies only for multiline column or row headers, i.e. if there are
      two or more fixed columns or rows defined. }
    property SortArrowIndex: Integer read FSortArrowIndex write SetSortArrowIndex;
    { Makes it possible to sort column or row referred by this TKGridAxisItem
      instance. }
    property SortMode: TKGridSortMode read FSortMode write SetSortMode;
    { Provides access to the obj cell instances corresponding to the column or
      row referred by this TKGridAxisItem instance. }
    property Strings[Index: Integer]: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF} read GetStrings write SetStrings; default;
    { Shareable property. Determines if the column or row is visible. }
    property Visible: Boolean read GetVisible write SetVisible;
    { Provides access to custom object for Row }
    property Tag: TObject read FTag write FTag;
  end;

  { @abstract(Metaclass for @link(TKGridAxisItem)) This type is used internally. }
  TKGridAxisItemClass = class of TKGridAxisItem;

  { @abstract(Dynamic array type to store @link(TKGridAxisItem) instances)
    There are always two arrays of this type in TKCustomGrid. First of them
    stores column properties - @link(TKCustomGrid.FCols) - and the second stores
    row properties - @link(TKCustomGrid.FRows). }
  TKGridAxisItems = array of TKGridAxisItem;

  { @abstract(Class to store column properties)
    This class implements properties and methods specific to columns. }
  TKGridCol = class(TKGridAxisItem)
  private
    FCellHint: Boolean;
    FTabStop: Boolean;
    function FindCol(out Index: Integer): Boolean;
  protected
    { Read method for the @link(TKGridAxisItem.Objects) property. Implementation for columns. }
    function GetObjects(Index: Integer): TObject; override;
    { Read method for the @link(TKGridAxisItem.Strings) property. Implementation for columns. }
    function GetStrings(Index: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; override;
    { Write method for the @link(TKGridAxisItem.Extent) property. Implementation for columns. }
    procedure SetExtent(const Value: Integer); override;
    { Write method for the @link(TKGridAxisItem.Objects) property. Implementation for columns. }
    procedure SetObjects(Index: Integer; const Value: TObject); override;
    { Write method for the @link(TKGridAxisItem.SortArrowIndex) property. Implementation for columns. }
    procedure SetSortArrowIndex(Value: Integer); override;
    { Write method for the @link(TKGridAxisItem.SortMode) property. Implementation for columns. }
    procedure SetSortMode(const Value: TKGridSortMode); override;
    { Write method for the @link(TKGridAxisItem.Strings) property. Implementation for columns. }
    procedure SetStrings(Index: Integer; const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}); override;
    { Write method for the @link(TKGridAxisItem.Visible) property. Implementation for columns. }
    procedure SetVisible(Value: Boolean); override;
  public
    { Creates the instance. Do not create custom instances. All necessary
      TKGridCol instances are created automatically by TKCustomGrid. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Copies the properties of another TKGridAxisItem instances into this
      TKGridCol instance. }
    procedure Assign(Source: TKGridAxisItem); override;
    { Makes it possible to assign a list of strings contained in TStrings
      to the grid. It behaves exactly the same way as the corresponding method
      in TStringGrid. Implementation for columns, i.e. the strings contained
      in Source are copied to the text cells corresponding to the column
      referred by this TKGridCol instance. }
    procedure Assign(Source: TStrings); override;
{$IFDEF TKGRID_USE_JCL}
    { Makes it possible to assign a list of strings contained in TWideStrings
      to the grid. Implementation for columns, i.e. the strings contained
      in Source are copied to the text cells corresponding to the column
      referred by this TKGridCol instance. }
    procedure Assign(Source: TWideStrings); override;
{$ENDIF}
    { Sets text of all cells corresponding to this column to empty string. }
    procedure Clear; override;
    { Returns True if shareable properties of this TKGridAxisItem instance have
      the same value as those in Item. }
    function {$ifdef COMPILER12_UP}EqualProperties{$ELSE}Equals{$ENDIF}(Item: TKGridAxisItem): Boolean; override;
    { Shareable property. Determines if cell hint is enabled for this column. }
    property CellHint: Boolean read FCellHint write FCellHint;
    { Shareable property. Determines if pressing the TAB or Shift+TAB key can
      move the input focus at a cell that belongs to this column. This property
      has effect only if goTabs is present under @link(TKCustomGrid.Options). }
    property TabStop: Boolean read FTabStop write FTabStop;
  end;

  { @abstract(Metaclass for @link(TKGridCol)) This type is used in
    @link(TKCustomGrid.ColClass) property. }
  TKGridColClass = class of TKGridCol;

  { @abstract(Class to store row properties)
    This class implements properties and methods specific to rows. }
  TKGridRow = class(TKGridAxisItem)
  private
    function FindRow(out Index: Integer): Boolean;
  protected
    { Read method for the @link(TKGridAxisItem.Objects) property. Implementation for rows. }
    function GetObjects(Index: Integer): TObject; override;
    { Read method for the @link(TKGridAxisItem.Strings) property. Implementation for rows. }
    function GetStrings(Index: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; override;
    { Write method for the @link(TKGridAxisItem.Extent) property. Implementation for rows. }
    procedure SetExtent(const Value: Integer); override;
    { Write method for the @link(TKGridAxisItem.Objects) property. Implementation for rows. }
    procedure SetObjects(Index: Integer; const Value: TObject); override;
    { Write method for the @link(TKGridAxisItem.SortArrowIndex) property. Implementation for rows. }
    procedure SetSortArrowIndex(Value: Integer); override;
    { Write method for the @link(TKGridAxisItem.SortMode) property. Implementation for rows. }
    procedure SetSortMode(const Value: TKGridSortMode); override;
    { Write method for the @link(TKGridAxisItem.Strings) property. Implementation for rows. }
    procedure SetStrings(Index: Integer; const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}); override;
    { Write method for the @link(TKGridAxisItem.Visible) property. Implementation for rows. }
    procedure SetVisible(Value: Boolean); override;
  public
    { Creates the instance. Do not create custom instances. All necessary
      TKGridRow instances are created automatically by TKCustomGrid. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Sets text of all cells corresponding to this row to empty string. }
    procedure Clear; override;
    { Makes it possible to assign a list of strings contained in TStrings
      to the grid. It behaves exactly the same way as the corresponding method
      in TStringGrid. Implementation for rows, i.e. the strings contained
      in Source are copied to the text cells corresponding to the row
      referred by this TKGridRow instance. }
    procedure Assign(Source: TStrings); override;
{$IFDEF TKGRID_USE_JCL}
    { Makes it possible to assign a list of strings contained in TWideStrings
      to the grid. Implementation for rows, i.e. the strings contained
      in Source are copied to the text cells corresponding to the row
      referred by this TKGridRow instance. }
    procedure Assign(Source: TWideStrings); override;
{$ENDIF}
  end;

  { @abstract(Metaclass for @link(TKGridRow)) This type is used in
    @link(TKCustomGrid.RowClass) property. }
  TKGridRowClass = class of TKGridRow;

  { @abstract(Base class to store cell properties)
    This class implements properties and methods common to all cell classes. }
  TKGridCell = class(TObject)
  private
    FGrid: TKCustomGrid;
    FSpan: TKGridCellSpan;
    procedure SetColSpan(const Value: Integer);
    procedure SetRowSpan(const Value: Integer);
    procedure SetSpan(const Value: TKGridCellSpan);
  protected
    { Called after specific property has been updated. Default behavioor:
      Searches the cell in the parent grid and invalidates the cell. You can
      override this method to extend behavior. }
    procedure AfterUpdate; virtual; // formerly UpdateCell
    { Called before specific property is to be updated. }
    procedure BeforeUpdate; virtual;
    { Cell class aware version of @link(TKCustomGrid.OnDrawCell).
      Fills ARect with predefined Brush. }
    procedure DrawCell(ACol, ARow: Integer; const ARect: TRect;
      State: TKGridDrawState); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorCreate).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorCreate). }
    procedure EditorCreate(ACol, ARow: Integer; var AEditor: TWinControl); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorDataFromGrid).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorDataFromGrid). }
    procedure EditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer;
      var AssignText: Boolean); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorDataToGrid).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorDataToGrid). }
    procedure EditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer;
      var AssignText: Boolean); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorDestroy).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorDestroy). }
    procedure EditorDestroy(var AEditor: TWinControl; ACol, ARow: Integer); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorKeyPreview).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorKeyPreview). }
    procedure EditorKeyPreview(AEditor: TWinControl; ACol, ARow: Integer;
      var Key: Word; Shift: TShiftState; var IsGridKey: Boolean); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorResize).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorResize). }
    procedure EditorResize(AEditor: TWinControl; ACol, ARow: Integer;
      var ARect: TRect); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorSelect).
      The TKGridCell's implementation calls @link(TKCustomGrid.DefaultEditorSelect). }
    procedure EditorSelect(AEditor: TWinControl; ACol, ARow: Integer;
      SelectAll, CaretToLeft, SelectedByMouse: Boolean); virtual;
    { Searches the cell in the parent grid. }
    function FindCell(out ACol, ARow: Integer): Boolean; virtual;
    { Initializes the cell data. }
    procedure Initialize; virtual;
    { Cell class aware version of @link(TKCustomGrid.OnMeasureCell). }
    procedure MeasureCell(ACol, ARow: Integer; const ARect: TRect;
      State: TKGridDrawState; Priority: TKGridMeasureCellPriority;
      var Extent: TPoint); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnSelectCell).
      The TKGridCell's implementation does nothing. }
    procedure SelectCell(ACol, ARow: Integer; var ACanSelect: Boolean);
      virtual;
    { Cell class aware version of @link(TKCustomGrid.OnSelectionExpand).
      The TKGridCell's implementation does nothing. }
    procedure SelectionExpand(ACol, ARow: Integer; var ACanExpand: Boolean); virtual;
  public
    { Creates the instance. You can create a custom instance and pass it
      e.g. to a @link(TKCustomGrid.Cell) property. The AGrid parameter has no meaning
      in this case and you may set it to nil. }
    constructor Create(AGrid: TKCustomGrid); virtual;
    { Applies TKGridCell properties to the cell painter.
      The TKGridCell's implementation does nothing. }
    procedure ApplyDrawProperties; virtual;
    { Copies the properties of another TKGridCell instances into this
      TKGridCell instance. }
    procedure Assign(Source: TKGridCell); virtual;
    { Clears the cell data. }
    procedure Clear;
    { Specifies the number of columns the cell should be spanned to. }
    property ColSpan: Integer read FSpan.ColSpan write SetColSpan;
    { Pointer to the grid. You will probably need it when implementing application
      specific behavior. }
    property Grid: TKCustomgrid read FGrid;
    { Specifies the number of rows the cell should be spanned to. }
    property RowSpan: Integer read FSpan.RowSpan write SetRowSpan;
    { Specifies both cell span parameters. }
    property Span: TKGridCellSpan read FSpan write SetSpan;
  end;

  { @abstract(Metaclass for @link(TKGridCell)) This type is used in the
    @link(TKCustomGrid.CellClass) property. }
  TKGridCellClass = class of TKGridCell;

  { @abstract(Dynamic array type to store row of @link(TKGridCell) instances)
    This one-dimensional array stores cell properties. }
  TKGridCellRow = array of TKGridCell;

  { @abstract(Dynamic array type to store the entire grid of @link(TKGridCell) instances)
    This two-dimensional array stores cell properties - @link(TKCustomGrid.FCells). }
  TKGridCells = array of TKGridCellRow;

  { @abstract(Class for simple textual cell)
    This cell class implements properties and methods needed to display/edit a cell
    with simple text. }
  TKGridTextCell = class(TKGridCell)
  private
  {$IFDEF STRING_IS_UNICODE}
    FText: string;
    function GetTextPtr: PChar;
  {$ELSE}
    FText: PWideChar; // WideString is slow as storage here
    function GetText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  {$ENDIF}
    procedure SetText(const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
  protected
    { Assigns a new text string into this TKGridTextCell instance. The new
      string will be assigned by a grow on demand method, i.e. the memory
      allocated for the string can only grow within each assignment. It continues
      to grow until the TKGridTextCell instance is destroyed. }
    procedure AssignText(const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}); virtual;
    { Cell class aware version of @link(TKCustomGrid.OnEditorCreate).
      Creates a TEdit inplace editor. }
    procedure EditorCreate(ACol, ARow: Integer; var AEditor: TWinControl); override;
    { Initializes the cell data. }
    procedure Initialize; override;
  public
    { Creates the instance. See @link(TKGridCell.Create) for details. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Destroys the instance. See TObject.Destroy in Delphi help. }
    destructor Destroy; override;
    { Applies TKGridTextCell properties to the cell painter. }
    procedure ApplyDrawProperties; override;
    { Copies shareable properties of another instance that inherits from
      TKGridCell into this TKGridTextCell instance. }
    procedure Assign(Source: TKGridCell); override;
    { Readonly property. This is the editable text that appears in the cell -
      published as pointer for fast read operations like sorting. }
    property TextPtr: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF} read {$IFDEF STRING_IS_UNICODE}GetTextPtr{$ELSE}FText{$ENDIF};
    { Shareable property. This is the editable text that appears in the cell. }
    property Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF} read {$IFDEF STRING_IS_UNICODE}FText{$ELSE}GetText{$ENDIF} write SetText;
  end;

  { @abstract(Class for a textual cell with custom appearance)
    This cell class implements properties and methods needed to display/edit
    a textual cell with custom appearance. }
  TKGridAttrTextCell = class(TKGridTextCell)
  private
    FAttributes: TKTextAttributes;
    FBackColor: TColor;
    FBrush: TBrush;
    FBrushChanged: Boolean;
    FFont: TFont;
    FFontChanged: Boolean;
    FHAlign: TKHAlign;
    FHPadding: Integer;
    FVAlign: TKVAlign;
    FVPadding: Integer;
    procedure SetAttributes(const AValue: TKTextAttributes);
    procedure SetFHAlign(const Value: TKHAlign);
    procedure SetFHPadding(const Value: Integer);
    procedure SetFVAlign(const Value: TKVAlign);
    procedure SetFVPadding(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
  protected
    { Called from FFont.OnChange. Sets FontChanged to True. }
    procedure FontChange(Sender: TObject);
    { Called from FBrush.OnChange. Sets BrushChanged to True. }
    procedure BrushChange(Sender: TObject);
    { Initializes the cell data. }
    procedure Initialize; override;
  public
    { Creates the instance. See @link(TKGridCell.Create) for details. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Destroys the instance. See TObject.Destroy in Delphi help. }
    destructor Destroy; override;
    { Applies TKGridAttrTextCell properties to the cell painter. }
    procedure ApplyDrawProperties; override;
    { Copies shareable properties of another instance that inherits from
      TKGridCell into this TKGridAttrTextCell instance. }
    procedure Assign(Source: TKGridCell); override;
    { Shareable property. These are the text attributes to render the text. }
    property Attributes: TKTextAttributes read FAttributes write SetAttributes;
    { Shareable property. This is the color used to fill the gaps between
      a non solid @link(TKGridAttrTextCell.Brush). }
    property BackColor: TColor read FBackColor write SetBackColor;
    { Shareable property. This is the brush that will be used to fill the cell background. }
    property Brush: TBrush read FBrush;
    { Non-shareable property. Returns True if Brush.OnChange occured. }
    property BrushChanged: Boolean read FBrushChanged;
    { Shareable property. This is the font that will be used to render the text. }
    property Font: TFont read FFont;
    { Non-shareable property. Returns True if Font.OnChange occured. }
    property FontChanged: Boolean read FFontChanged;
    { Shareable property. This is the horizontal alignment
      that will be used to place the text within the cell rectangle. }
    property HAlign: TKHAlign read FHAlign write SetFHAlign;
    { Shareable property. This is the horizontal padding
      of the text from the cell rectangle. }
    property HPadding: Integer read FHPadding write SetFHPadding;
    { Shareable property. This is the vertical alignment
      that will be used to place the text within the cell rectangle. }
    property VAlign: TKVAlign read FVAlign write SetFVAlign;
    { Shareable property. This is the vertical padding
      of the text from the cell rectangle. }
    property VPadding: Integer read FVPadding write SetFVPadding;
  end;

{$IFDEF TKGRIDOBJECTCELL_IS_TKGRIDATTRTEXTCELL}
  { @exclude }
  TKGridObjectCellAncestor = TKGridAttrTextCell;
{$ELSE}
 {$IFDEF TKGRIDOBJECTCELL_IS_TKGRIDTEXTCELL}
  { @exclude }
  TKGridObjectCellAncestor = TKGridTextCell;
 {$ELSE}
  { @exclude }
  TKGridObjectCellAncestor = TKGridCell;
 {$ENDIF}
{$ENDIF}

  { @abstract(Class for an object cell)
    This cell class implements properties and methods needed to store a custom
    object in a cell. This class is implemented for backward compatibility
    with TStringGrid. You can implement different cell classes to store any user
    defined data. }
  TKGridObjectCell = class(TKGridObjectCellAncestor)
  private
    FCellObject: TObject;
    procedure SetCellObject(Value: TObject);
  protected
    { Initializes the cell data. }
    procedure Initialize; override;
  public
    { Creates the instance. See @link(TKGridCell.Create) for details. }
    constructor Create(AGrid: TKCustomGrid); override;
    { Destroys the instance. See TObject.Destroy in Delphi help. }
    destructor Destroy; override;
    { Copies shareable properties of another instance that inherits from
      TKGridCell into this TKGridObjectCell instance. }
    procedure Assign(Source: TKGridCell); override;
    { Shareable property. This is the object stored within the cell class.
      A single object instance passed to CellObject cannot be shared among multiple
      cell class instances. The reason is that TObject instances do not support
      Assign method, more convenient it would be to store TPersistents. }
    property CellObject: TObject read FCellObject write SetCellObject;
  end;

  { @abstract(Wrapper for a versatile and easily extensible cell painting engine)
    Properties and methods of this class provide standard cell painting.
    To adapt cell painting, you can use combinations of elementary painting
    methods in the @link(TKCustomGrid.OnDrawCell) event handler or
    override and adapt some high level methods of TKGridCellPainter. }
  TKGridCellPainter = class(TObject)
  private
    FAttributes: TKTextAttributes;
    FBackColor: TColor;
    FBlockRect: TRect;
    FButton: Boolean;
    FButtonPressed: Boolean;
    FCanvas: TCanvas;
    FCheckBox: Boolean;
    FCheckBoxHAlign: TKHAlign;
    FCheckBoxHPadding: Integer;
    FCheckboxState: TCheckBoxState;
    FCheckBoxVAlign: TKVAlign;
    FCheckBoxVPadding: Integer;
    FCellPos: TPoint;
    FCellRect: TRect;
    FClipLock: Integer;
    FCol: Integer;
    FGraphic: TGraphic;
    FGraphicDrawText: Boolean;
    FGraphicHAlign: TKHAlign;
    FGraphicHPadding: Integer;
    FGraphicStretchMode: TKStretchMode;
    FGraphicVAlign: TKVAlign;
    FGraphicVPadding: Integer;
    FGrid: TKCustomGrid;
    FHotFrameOnly: Boolean;
    FHAlign: TKHAlign;
    FHPadding: Integer;
    FRgn: HRGN;
    FRow: Integer;
    FSortArrow: TKAlphaBitmap;
    FSortArrowHAlign: TKHAlign;
    FSortArrowHPadding: Integer;
    FState: TKGridDrawState;
    FText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
    FValidClipping: Boolean;
    FVAlign: TKVAlign;
    FVPadding: Integer;
    function GetCheckBoxChecked: Boolean;
    procedure SetCheckBox(AValue: Boolean);
    procedure SetCheckBoxChecked(const Value: Boolean);
  protected
    { Returns True if the grid is being printed out. }
    FPrinting: Boolean;
    { High level method. Provides default behavior needed to initialize painting
      of a cell. It is called automatically in @link(TKCustomGrid.PaintCell). }
    procedure BeginDraw; virtual;
    { High level method. Provides default behavior needed to finalize painting
      of a cell. It is called automatically in @link(TKCustomGrid.PaintCell). }
    procedure EndDraw; virtual;
    { Read method for the @link(TKGridCellPainter.SortArrowWidth) property. }
    function GetSortArrowWidth: Integer; virtual;
    { Initializes all canvas independent attributes to default values. Called
      from DefaultAttributes just before cell painting. }
    procedure Initialize; virtual;
  public
    { Creates the instance. Do not create custom instances. All necessary
      TKGridCellPainter instances are created automatically by TKCustomGrid. }
    constructor Create(AGrid: TKCustomGrid);
    { Destroys the instance. See TObject.Destroy in Delphi help. }
    destructor Destroy; override;
    { Forces the drawing output to be clipped within @link(TKGridCellPainter.CellRect).
      This behavior must be cancelled by @link(TKGridCellPainter.EndClip) when
      no longer needed. You don't need to call BeginClip if the @link(TKCustomGrid.Options)
      property already contains goClippedCells. }
    function BeginClip: Boolean; virtual;
    { Calculates the checkbox position within BaseRect, if any. The position
      is stored in Bounds (checkbox with padding) and Interior (checkbox without
      padding). Bounds are excluded from BaseRect. }
    function CellCheckBoxRect(var BaseRect: TRect; out Bounds, Interior: TRect; StretchMode: TKStretchMode): Boolean;
    { Calculates the graphic position within BaseRect, if any. The position
      is stored in Bounds (graphic with padding) and Interior (graphic without
      padding). Bounds are excluded from BaseRect. }
    function CellGraphicRect(var BaseRect: TRect; out Bounds, Interior: TRect; StretchMode: TKStretchMode): Boolean;
    { Calculates the sorting arrow position within BaseRect, if any. The position
      is stored in Bounds (sorting arrow with padding) and Interior (sorting arrow
      without padding). Bounds are excluded from BaseRect. }
    function CellSortArrowRect(var BaseRect: TRect; out Bounds, Interior: TRect): Boolean;
    { Calculates the cell text horizontal and vertical extent, if any. }
    function CellTextExtent(const BaseRect: TRect; out Extent: TPoint): Boolean;
    { Calculates the text position within BaseRect, if any. The position
      is stored in Bounds (text with padding) and Interior (text without padding).
      Bounds are excluded from BaseRect. }
    function CellTextRect(var BaseRect: TRect; out Bounds, Interior: TRect): Boolean;
    { Low level method. Prepares default painting attributes. Under current
      implementation, DefaultAttributes applies default colors to the
      @link(TKGridCellPainter.Canvas)'s Brush and Font properties. }
    procedure DefaultAttributes; virtual;
    { Highest level method. Provides default painting of any cell. You should call
      DefaultDraw when implementing the @link(TKCustomGrid.OnDrawCell) event handler
      unless any specific cell painting is required. This method supersedes
      the obsolete @link(DefaultDrawCell) function. }
    procedure DefaultDraw; virtual;
    { Returns the combination of edge masks (BF_...) to paint a fixed cell
      correctly in old TCustomGrid style or if no OS themes are available. }
    function DefaultEdges: Cardinal; virtual;
    { Highest level method. Provides default cell extent calculation. }
    function DefaultMeasure(Priority: TKGridMeasureCellPriority): TPoint; virtual;
    { Low level method. Paints common parts of a themed and non-themed cell. }
    procedure DrawCellCommon; virtual;
    { Low level method. Paints button frame. }
    procedure DrawCellButton(Bounds: TRect);
    { Low level method. Paints checkbox frame. }
    procedure DrawCellCheckBox(const Bounds, Interior: TRect);
    { Low level method. Paints the graphic (if any) within the rectangle specified by Interior.
      Fills the rectangle specified by Bounds with current brush. }
    procedure DrawCellGraphic(const Bounds, Interior: TRect);
    { Low level method. Paints the sort arrow within the rectangle specified by Interior.
      Fills the rectangle specified by Bounds with current brush. }
    procedure DrawCellSortArrow(const Bounds, Interior: TRect);
    { Low level method. Paints a button frame. }
    procedure DrawButtonFrame(const ARect: TRect); virtual;
    { Low level method. Paints a check box frame. }
    procedure DrawCheckBoxFrame(const ARect: TRect); virtual;
    { Low level method. Paints cell text. }
    procedure DrawCellText(var ARect: TRect); virtual;
    { Low level method. Paints a standard focus rectangle around the focused
      cell. }
    procedure DrawCellFocus(const ARect: TRect; SkipTest: Boolean = False); virtual;
    { High level method. Paints an empty cell, i.e. only fills the cell background. }
    procedure DrawEmptyCell; virtual;
    { High level method. Paints a non themed fixed cell. }
    procedure DrawFixedCell; virtual;
    { Low level method. Paints fixed cell background. }
    procedure DrawFixedCellBackground(const ARect: TRect); virtual;
    { Low level method. Paints non-themed fixed cell background. }
    procedure DrawFixedCellNonThemedBackground(const ARect: TRect); virtual;
    { High level method. Paints a fixed cell in Windows Header style. }
    procedure DrawHeaderCell; virtual;
    { Low level method. Paints header background. }
    procedure DrawHeaderCellBackground(const ARect: TRect);
    { Low level method. Paints selection background frame. }
    procedure DrawNormalCellBackground(const ARect: TRect); virtual;
    { High level method. Paints a selectable cell. }
    procedure DrawSelectableCell; virtual;
    { Low level method. Paints selection background frame. }
    procedure DrawSelectedCellBackground(const ARect: TRect; RClip: PRect = nil); virtual;
    { High level method. Paints a themed fixed cell. }
    procedure DrawThemedFixedCell; virtual;
    { High level method. Paints a themed fixed cell in Windows Header style. }
    procedure DrawThemedHeaderCell; virtual;
    { Restores normal drawing output after previous
      @link(TKGridCellPainter.BeginClip) call. }
    procedure EndClip; virtual;
    { Specifies the text attributes used to render the cell text. }
    property Attributes: TKTextAttributes read FAttributes write FAttributes;
    { Specifies the color used to fill the gaps if the Brush
      referred by @link(TKGridCellPainter.Canvas) is not solid brush. }
    property BackColor: TColor read FBackColor write FBackColor;
    { Specifies the bounding rectangle of block of cells. This value can be given
      either in TKCustomGrid's client coordinates or, in @link(goDoubleBufferedCells)
      mode, relative to @link(TKGridCellPainter.CellPos). }
    property BlockRect: TRect read FBlockRect write FBlockRect;
    { Determines if a standard button frame should be painted for a selectable
      cell. To paint a button frame, you need to implement the @link(OnDrawCell)
      event handler, set Button to True and call @link(TKGridCellPainter.DefaultDraw),
      which ensures correct painting of a button frame. }
    property Button: Boolean read FButton write FButton;
    { Specifies if the button frame should be painted in pressed or released (normal)
      state. This property has no effect unless @link(TKGridCellPainter.Button)
      is True. }
    property ButtonChecked: Boolean read FButtonPressed write FButtonPressed;
    { Identifies the Canvas where the cell will be painted to. The value of this
      property is either equal to TKCustomGrid.@link(TKCustomGrid.Canvas) or, in
      @link(goDoubleBufferedCells) mode, equal to a memory device context whose
      dimensions correspond to the size of the cell. When implementing the
      @link(OnDrawCell) event handler, you can paint to TKCustomGrid.Canvas as
      usual in TStringGrid. However, if you wish to use goDoubleBufferedCells,
      you must paint to TKGridCellPainter.Canvas. }
    property Canvas: TCanvas read FCanvas write FCanvas;
    { Determines if a standard check box frame should be painted for a selectable
      cell. To paint a check box frame, you need to implement the @link(OnDrawCell)
      event handler, set CheckBox to True and call @link(TKGridCellPainter.DefaultDraw),
      which ensures correct painting of a check box frame. }
    property CheckBox: Boolean read FCheckBox write SetCheckBox;
    { Specifies if the check box frame should be painted in checked or unchecked
      state. This property is for backward compatibility and has no effect unless
      @link(TKGridCellPainter.CheckBox) is True. For new designs use the CheckBoxState property. }
    property CheckBoxChecked: Boolean read GetCheckBoxChecked write SetCheckBoxChecked;
    { Specifies the horizontal padding for the sorting arrow. }
    property CheckBoxHAlign: TKHAlign read FCheckBoxHAlign write FCheckBoxHAlign;
    { Specifies the horizontal padding for the sorting arrow. }
    property CheckBoxHPadding: Integer read FCheckBoxHPadding write FCheckBoxHPadding;
    { Specifies if the check box frame should be painted in checked, grayed
      or unchecked state. This property has no effect unless
      @link(TKGridCellPainter.CheckBox) is True. Added by Karol Schmidt }
    property CheckboxState: TCheckBoxState read FCheckboxState write FCheckboxState;
    { Specifies the vertical padding for the sorting arrow. }
    property CheckBoxVAlign: TKVAlign read FCheckBoxVAlign write FCheckBoxVAlign;
    { Specifies the vertical padding for the sorting arrow. }
    property CheckBoxVPadding: Integer read FCheckBoxVPadding write FCheckBoxVPadding;
    { Specifies the left and top position/origin of the cell in TKCustomGrid's client
      coordinates. }
    property CellPos: TPoint read FCellPos write FCellPos;
    { Specifies the bounding rectangle of the cell. This value can be given
      either in TKCustomGrid's client coordinates or, in @link(goDoubleBufferedCells)
      mode, relative to @link(TKGridCellPainter.CellPos). }
    property CellRect: TRect read FCellRect write FCellRect;
    { Specifies the column index of the cell. }
    property Col: Integer read FCol write FCol;
    { Specifies the image that should be drawn in the cell. }
    property Graphic: TGraphic read FGraphic write FGraphic;
    { Specifies if the text should appear next to the image. }
    property GraphicDrawText: Boolean read FGraphicDrawText write FGraphicDrawText;
    { Specifies the horizontal alignment for the image. }
    property GraphicHAlign: TKHAlign read FGraphicHAlign write FGraphicHAlign;
    { Specifies the horizontal padding for the image. }
    property GraphicHPadding: Integer read FGraphicHPadding write FGraphicHPadding;
    { Specifies if the the image should be stretched within the cell (aspect ratio is preserved). }
    property GraphicStretchMode: TKStretchMode read FGraphicStretchMode write FGraphicStretchMode;
    { Specifies the vertical alignment for the image. }
    property GraphicVAlign: TKVAlign read FGraphicVAlign write FGraphicVAlign;
    { Specifies the vertical padding for the image. }
    property GraphicVPadding: Integer read FGraphicVPadding write FGraphicVPadding;
    { Specifies the calling grid. }
    property Grid: TKCustomGrid read FGrid;
    { This is the default horizontal alignment that will be used to place the text
      within the cell rectangle. }
    property HAlign: TKHAlign read FHAlign write FHAlign;
    { When true, a check box frame etc. is only painted "hot" when mouse cursor is
      over that frame. When false, it is painted "hot" when mouse cursor is over
      entire cell. }
    property HotFrameOnly: Boolean read FHotFrameOnly write FHotFrameOnly;
    { This is the default horizontal padding for the text. }
    property HPadding: Integer read FHPadding write FHPadding;
    { Returns True if the grid is being printed out. Needed e.g. for font height
      adjstment while printing. }
    property Printing: Boolean read FPrinting;
    { Specifies the row index of the cell. }
    property Row: Integer read FRow write FRow;
    { Returns the width of the sorting arrow glyph. This value can be either zero
      if no sorting arrow should be drawn for the cell (most cases), or a width
      of the glyph for column/row sorting. }
    property SortArrowWidth: Integer read GetSortArrowWidth;
    { Specifies the horizontal padding for the check box. }
    property SortArrowHAlign: TKHAlign read FSortArrowHAlign write FSortArrowHAlign;
    { Specifies the horizontal padding for the sorting arrow. }
    property SortArrowHPadding: Integer read FSortArrowHPadding write FSortArrowHPadding;
    { Specifies the draw state of the cell. }
    property State: TKGridDrawState read FState write FState;
    { Specifies the text that appears in the cell. }
    property Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF} read FText write FText;
    { This is the default vertical alignment that will be used to place the text
      within the cell rectangle. }
    property VAlign: TKVAlign read FVAlign write FVAlign;
    { This is the default vertical padding for the text. }
    property VPadding: Integer read FVPadding write FVPadding;
  end;

  { @abstract(Metaclass for @link(TKGridCellPainter)) This type is used in the
    @link(TKCustomGrid.CellPainterClass) property. }
  TKGridCellPainterClass = class of TKGridCellPainter;

  { @abstract(Container for all colors used by @link(TKCustomGrid) class)
    This container allows to group many colors into one item in object inspector.
    Colors are accessible via published properties or several public Color*
    properties. }
  TKGridColors = class(TPersistent)
  private
    FGrid: TKCustomGrid;
    FBrightRangeBkGnd: Boolean;
    FColorScheme: TKGridColorScheme;
    function GetColor(Index: TKGridColorIndex): TColor;
    function GetColorEx(Index: TKGridColorIndex): TColor;
    procedure SetColor(Index: TKGridColorIndex; Value: TColor);
    procedure SetColorEx(Index: TKGridColorIndex; Value: TColor);
    procedure SetColors(const Value: TKColorArray);
  protected
    FBrightColors: TKColorArray;
    FColors: TKColorArray;
    { Initializes the color array. }
    procedure Initialize; virtual;
    { Returns the specific color according to ColorScheme. }
    function InternalGetColor(Index: TKGridColorIndex): TColor; virtual;
    { Replaces the specific color. }
    procedure InternalSetColor(Index: TKGridColorIndex; Value: TColor); virtual;
  public
    { Creates the instance. You can create a custom instance and pass it
      e.g. to a @link(TKCustomGrid.Colors) property. The AGrid parameter has no meaning
      in this case and you may set it to nil. }
    constructor Create(AGrid: TKCustomGrid);
    { Copies the properties of another instance that inherits from
      TPersistent into this TKGridColors instance. }
    procedure Assign(Source: TPersistent); override;
    { Ensures cell range background colors will be brightened if specified by
      @link(TKGridColors.BrightRangeBkGnd). }
    procedure BrightRangeBkGnds;
    { Clears cached brighter colors. }
    procedure ClearBrightColors;
    { Specifies color scheme for reading of published properties - see GetColor in source code}
    property ColorScheme: TKGridColorScheme read FColorScheme write FColorScheme;
    { Returns always normal color - regardless of the ColorScheme setting. }
    property Color[Index: TKGridColorIndex]: TColor read GetColorEx write SetColorEx;
    { Returns array of normal colors. }
    property Colors: TKColorArray read FColors write SetColors;
  published
    { Specifies if cell range colors should be brightened from focused cell colors. }
    property BrightRangeBkGnd: Boolean read FBrightRangeBkGnd write FBrightRangeBkGnd default True;
    { Background color for non-fixed cells. }
    property CellBkGnd: TColor index ciCellBkGnd read GetColor write SetColor default cCellBkGndDef;
    { Color for lines around non-fixed cells. }
    property CellLines: TColor index ciCellLines read GetColor write SetColor default cCellLinesDef;
    { Text color for non-fixed cells. }
    property CellText: TColor index ciCellText read GetColor write SetColor default cCellTextDef;
    { Background color for drag suggestion stroke. }
    property DragSuggestionBkGnd: TColor index ciDragSuggestionBkGnd read GetColor write SetColor default cDragSuggestionBkGndDef;
    { Line color for drag suggestion stroke. }
    property DragSuggestionLine: TColor index ciDragSuggestionLine read GetColor write SetColor default cDragSuggestionLineDef;
    { Background color for fixed cells. }
    property FixedCellBkGnd: TColor index ciFixedCellBkGnd read GetColor write SetColor default cFixedCellBkGndDef;
    { Background color for fixed cells that currently indicate selection. }
    property FixedCellIndication: TColor index ciFixedCellIndication read GetColor write SetColor default cFixedCellIndicationDef;
    { Color for lines around fixed cells. }
    property FixedCellLines: TColor index ciFixedCellLines read GetColor write SetColor default cFixedCellLinesDef;
    { Text color for fixed cells. }
    property FixedCellText: TColor index ciFixedCellText read GetColor write SetColor default cFixedCellTextDef;
    { Color for lines around fixed cells if goThemedCells is True}
    property FixedThemedCellLines: TColor index ciFixedThemedCellLines read GetColor write SetColor default cFixedThemedCellLinesDef;
    { Color for 3D highlight effects for fixed cells if goThemedCells is True}
    property FixedThemedCellHighlight: TColor index ciFixedThemedCellHighlight read GetColor write SetColor default cFixedThemedCellHighlightDef;
    { Color for 3D shadow effects for fixed cells if goThemedCells is True}
    property FixedThemedCellShadow: TColor index ciFixedThemedCellShadow read GetColor write SetColor default cFixedThemedCellShadowDef;
    { Background color for focused cell defined by Selection.Cell1. }
    property FocusedCellBkGnd: TColor index ciFocusedCellBkGnd read GetColor write SetColor default cFocusedCellBkGndDef;
    { Text color for focused cell defined by Selection.Cell1. }
    property FocusedCellText: TColor index ciFocusedCellText read GetColor write SetColor default cFocusedCellTextDef;
    { Background color for another focused cells within the range or full row selection. }
    property FocusedRangeBkGnd: TColor index ciFocusedRangeBkGnd read GetColor write SetColor default cFocusedRangeBkGndDef;
    { Text color for another focused cells within the range or full row selection. }
    property FocusedRangeText: TColor index ciFocusedRangeText read GetColor write SetColor default cFocusedRangeTextDef;
    { Background color for selected cells defined by Selection.Cell1. }
    property SelectedCellBkGnd: TColor index ciSelectedCellBkGnd read GetColor write SetColor default cSelectedCellBkGndDef;
    { Text color for selected cells defined by Selection.Cell1. }
    property SelectedCellText: TColor index ciSelectedCellText read GetColor write SetColor default cSelectedCellTextDef;
    { Background color for another selected cells within the range or full row selection. }
    property SelectedRangeBkGnd: TColor index ciSelectedRangeBkGnd read GetColor write SetColor default cSelectedRangeBkGndDef;
    { Text color for another selected cells within the range or full row selection. }
    property SelectedRangeText: TColor index ciSelectedRangeText read GetColor write SetColor default cSelectedRangeTextDef;
    // aki:
    { Background color for selected cells defined by Selection.Cell1. }
    property SelectedFixedCellBkGnd: TColor index ciSelectedFixedCellBkGnd read GetColor write SetColor default cSelectedFixedCellBkGndDef;
  end;

  { @abstract(KGrid base component) This is the class that you use
    as the ancestor for your TKCustomGrid overrides. }
  TKCustomGrid = class(TKCustomControl)
  private
  {$IFDEF FPC}
    FFlat: Boolean;
  {$ENDIF}
    FCellClass: TKGridCellClass;
    FCellPainter: TKGridCellPainter;
    FCellPainterClass: TKGridCellPainterClass;
    FColClass: TKGridColClass;
    FColCount: Integer;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FDisabledDrawStyle: TKGridDisabledDrawStyle;
    FDragDest: Integer;
    FDragOrigin: Integer;
    FDragStyle: TKGridDragStyle;
    FEditorTransparency: TKGridEditorTransparency;
    FFixedCols: Integer;
    FFixedRows: Integer;
    FGridLineWidth: Integer;
    FKeyPreview: Boolean;
    FMinColWidth: Integer;
    FMinRowHeight: Integer;
    FMouseCellHintTime: Cardinal;
    FMoveDirection: TKGridMoveDirection;
    FOptions: TKGridOptions;
    FOptionsEx: TKGridOptionsEx;
    FRowClass: TKGridRowClass;
    FRowCount: Integer;
    FRangeSelectStyle: TKGridRangeSelectStyle;
    FScrollBars: TScrollStyle;
    FScrollModeVert: TKGridScrollMode;
    FScrollModeHorz: TKGridScrollMode;
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FSizingIndex: Integer;
    FSizingDest: Integer;
    FSizingStyle: TKGridSizingStyle;
    FSortModeLock: Integer;
    FSortStyle: TKGridSortStyle;
    FThroughClick: Boolean;
    FTopLeft: TKGridCoord;
    FTopLeftExtent: TKGridCoord;
    FOnBeginColDrag: TKGridBeginDragEvent;
    FOnBeginColSizing: TKGridBeginSizingEvent;
    FOnBeginRowDrag: TKGridBeginDragEvent;
    FOnBeginRowSizing: TKGridBeginSizingEvent;
    FOnCellChanging: TKGridCellChangedEvent;
    FOnCellSpan: TKGridCellSpanEvent;
    FOnChanged: TKGridCellEvent;
    FOnCheckColDrag: TKGridCheckDragEvent;
    FOnCheckRowDrag: TKGridCheckDragEvent;
    FOnColMoved: TKGridMovedEvent;
    FOnColWidthsChanged: TNotifyEvent;
    FOnColWidthsChangedEx: TKGridExtentEvent;
    FOnCompareCellInstances: TKGridCompareCellInstancesEvent;
    FOnCompareCells: TKGridCompareCellsEvent;
    FOnCustomSortCols: TKGridCustomSortEvent;
    FOnCustomSortRows: TKGridCustomSortEvent;
    FOnDrawCell: TKGridDrawCellEvent;
    FOnEditorCreate: TKGridEditorCreateEvent;
    FOnEditorDataFromGrid: TKGridEditorDataEvent;
    FOnEditorDataToGrid: TKGridEditorDataEvent;
    FOnEditorDestroy: TKGridEditorDestroyEvent;
    FOnEditorKeyPreview: TKGridEditorKeyPreviewEvent;
    FOnEditorResize: TKGridEditorResizeEvent;
    FOnEditorSelect: TKGridEditorSelectEvent;
    FOnEndColDrag: TKGridEndDragEvent;
    FOnEndColSizing: TKGridEndSizingEvent;
    FOnEndRowDrag: TKGridEndDragEvent;
    FOnEndRowSizing: TKGridEndSizingEvent;
    FOnExchangeCols: TKGridExchangeEvent;
    FOnExchangeRows: TKGridExchangeEvent;
    FOnMeasureCell: TKGridMeasureCellEvent;
    FOnMouseCellHint: TKGridCellHintEvent;
    FOnMouseClickCell: TKGridCellEvent;
    FOnMouseDblClickCell: TKGridCellEvent;
    FOnMouseEnterCell: TKGridCellEvent;
    FOnMouseLeaveCell: TKGridCellEvent;
    FOnRowMoved: TKGridMovedEvent;
    FOnRowHeightsChanged: TNotifyEvent;
    FOnRowHeightsChangedEx: TKGridExtentEvent;
    FOnSelectCell: TKGridSelectCellEvent;
    FOnSelectionExpand: TKGridSelectionExpandEvent;
    FOnSizeChanged: TKGridSizeChangedEvent;
    FOnTopLeftChanged: TNotifyEvent;
    function GetAllCellsSelected: Boolean;
    function GetAllRowsSelected: Boolean;
    function GetAllColsSelected: Boolean;
    function GetCell(ACol, ARow: Integer): TKGridCell;
    function GetCells(ACol, ARow: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
    function GetCellSpan(ACol, ARow: Integer): TKGridCellSpan;
    function GetCols(Index: Integer): TKGridCol;
    function GetColWidths(Index: Integer): Integer;
    function GetDefaultDrawing: Boolean;
    function GetEditorMode: Boolean;
    function GetEffectiveColSpacing(ACol: Integer): Integer;
    function GetEffectiveRowSpacing(ARow: Integer): Integer;
    function GetEntireColSelected(Index: Integer): Boolean;
    function GetEntireSelectedColCount: Integer;
    function GetEntireRowSelected(Index: Integer): Boolean;
    function GetEntireSelectedRowCount: Integer;
    function GetGridHeight: Integer;
    function GetGridWidth: Integer;
    function GetLastVisibleCol: Integer;
    function GetLastVisibleRow: Integer;
    function GetMoreCellsSelected: Boolean;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetRowHeights(Index: Integer): Integer;
    function GetRows(Index: Integer): TKGridRow;
    function GetSelection: TKGridRect;
    function GetSelectionCount: Integer;
    function GetSelectionRect: TRect;
    function GetSelections(Index: Integer): TKGridRect;
    function GetSortCol: Integer;
    function GetSortRow: Integer;
    function GetTabStops(Index: Integer): Boolean;
    function GetThemedCells: Boolean;
    function GetThemes: Boolean;
    function GetVisibleColCount: Integer;
    function GetVisibleGridRect: TKGridRect;
    function GetVisibleRowCount: Integer;
    procedure ReadColWidths(Reader: TReader);
    procedure ReadRowHeights(Reader: TReader);
  {$IFDEF FPC}
    procedure SetFlat(Value: Boolean);
  {$ENDIF}
    procedure SetCell(ACol, ARow: Integer; Value: TKGridCell);
    procedure SetCellPainterClass(Value: TKGridCellPainterClass);
    procedure SetCells(ACol, ARow: Integer; const Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
    procedure SetCellSpan(ACol, ARow: Integer; Value: TKGridCellSpan);
    procedure SetCol(Value: Integer);
    procedure SetColCount(Value: Integer);
    procedure SetColors(Value: TKGridColors);
    procedure SetColWidths(Index: Integer; Value: Integer);
    procedure SetDefaultColWidth(Value: Integer);
    procedure SetDefaultDrawing(Value: Boolean);
    procedure SetDefaultRowHeight(Value: Integer);
    procedure SetDisabledDrawStyle(Value: TKGridDisabledDrawStyle);
    procedure SetDragStyle(Value: TKGridDragStyle);
    procedure SetEditorMode(Value: Boolean);
    procedure SetEditorTransparency(Value: TKGridEditorTransparency);
    procedure SetFixedCols(Value: Integer);
    procedure SetFixedRows(Value: Integer);
    procedure SetGridLineWidth(Value: Integer);
    procedure SetLeftCol(Value: Integer);
    procedure SetMinColWidth(Value: Integer);
    procedure SetMinRowHeight(Value: Integer);
    procedure SetMouseCellHintTime(const AValue: Cardinal);
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetOptions(Value: TKGridOptions);
    procedure SetOptionsEx(Value: TKGridOptionsEx);
    procedure SetRow(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetRowHeights(Index: Integer; Value: Integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetScrollModeHorz(const Value: TKGridScrollMode);
    procedure SetScrollModeVert(const Value: TKGridScrollMode);
    procedure SetScrollSpeed(Value: Cardinal);
    procedure SetSelection(const Value: TKGridRect);
    procedure SetSelections(Index: Integer; const Value: TKGridRect);
    procedure SetSizingStyle(Value: TKGridSizingStyle);
    procedure SetTabStops(Index: Integer; Value: Boolean);
    procedure SetTopRow(Value: Integer);
    procedure WriteColWidths(Writer: TWriter);
    procedure WriteRowHeights(Writer: TWriter);
    procedure CMDesignHitTest(var Msg: TLMMouse); message CM_DESIGNHITTEST;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMShowingChanged(var Msg: TLMessage); message CM_SHOWINGCHANGED;
    procedure CMSysColorChange(var Msg: TLMessage); message CM_SYSCOLORCHANGE;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMWantSpecialKey(var Msg: TLMKey); message CM_WANTSPECIALKEY;
    procedure GMRecreateEditor(var Msg: TLMessage); message GM_RECREATE_EDITOR;
    procedure WMChar(var Msg: TLMChar); message LM_CHAR;
    procedure WMEraseBkGnd(var Msg: TLMEraseBkGnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  protected
    { Gains access to the cell hint timer. }
    FCellHintTimer: TTimer;
    { Two-dimensional dynamic array to store cell instances. Different cell
      classes can be used for cell instances. }
    FCells: TKGridCells;
    { Provides direct access to the color class for TKCustomGrid descendants }
    FColors: TKGridColors;
    { Dynamic array to store column instances. Different column classes can
      be used for column instances. }
    FCols: TKGridAxisItems;
    { Icon for column/row moving suggestion arrow. }
    FDragArrow: TKAlphaBitmap;
    { Wrapper for the window used to visually indicate a dragged column or row.
      Under Win2K or later system, this is a layered window. Under Win98SE or older
      system, it is a normal popup window. }
    FDragWindow: TKDragWindow;
    { Copy of the cell being currently edited. }
    FEditedCell: TKGridCell;
    { Provides direct access to the inplace editor instance for TKCustomGrid descendants. }
    FEditor: TWinControl;
    { Specifies the current bounding rectangle of inplace editor. }
    FEditorRect: TRect;
    { Specifies the current position of inplace editor. If @link(TKCustomGrid.Selection).Cell1
      is different from FEditorCell, the editor needs to be updated immediatelly
      to make these two values equal again. }
    FEditorCell: TKGridCoord;
    { Pointer to the original WindowProc property of the inplace editor. }
    FEditorWindowProc: TWndMethod;
    { Holds the mutually exclusive grid state. }
    FGridState: TKGridState;
    { Glyphs for hidden cell indicators. }
    FHCI: TKGridHCIBitmaps;
    { Specifies the cell hint window. }
    FHint: TKHintWindow;
    { Specifies the cell where hint timer has been started. }
    FHintCell: TKGridCoord;
    { Specifies the cell where left mouse button has been pressed. }
    FHitCell: TKGridCoord;
    { Specifies the point where left mouse button has been pressed. }
    FHitPos: TPoint;
    { Field for @link(TKCustomGrid.MaxCol) property. Descendants can modify it. }
    FMaxCol: Integer;
    { Field for @link(TKCustomGrid.MaxRow) property. Descendants can modify it. }
    FMaxRow: Integer;
    { Field to remember current column position for keyboard commands. }
    FMemCol: Integer;
    { Field to remember current row position for keyboard commands. }
    FMemRow: Integer;
    { Specifies the cell where mouse is over. FMouseOver is valid if goMouseOverCells
      is included in @link(TKCustomGrid.Options). }
    FMouseOver: TKGridCoord;
    { Dynamic array to store row instances. Different row classes can
      be used for row instances. }
    FRows: TKGridAxisItems;
    { Specifies current(topmost) selection not affected by @link(goRowSelect) as
      @link(TKCustomGrid.Selection). }
    FSelection: TKGridRect;
    { Specifies all selections except FSelection. This separation is done for
      backward compatibility. }
    FSelections: array of TKGridRect;
    { Current scrolling position in pixels (bound to cell boundary). }
    FScrollPos: TPoint;
    { Current scrolling offset in pixels for smSmooth mode (relative to cell boundary). }
    FScrollOffset: TPoint;
    { Auxilliary bitmap for various tasks. }
    FTmpBitmap: TBitmap;
  {$IFDEF FPC}
    { Temporary mouse cursor. }
    FTmpCursor: TCursor;
  {$ENDIF}
    { Adjusts the page setup. Ensures the PrintingMapped property is always True. }
    procedure AdjustPageSetup; override;
    { Adjusts any selection rectangle specified by ASelection to be valid
      selection in @link(goRowSelect) mode, i.e. makes ASelection to span
      the entire row(s). }
    function AdjustSelection(const ASelection: TKGridRect): TKGridRect; virtual;
    { Calls @link(TKCustomGrid.OnBeginColDrag) event handler or column class aware equivalent.
      See the @link(TKGridBeginDragEvent) type for parameter interpretation. }
    function BeginColDrag(var Origin: Integer;  const MousePt: TPoint): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnBeginColSizing) event handler or checks the
      @link(TKGridAxisItem.CanResize) property to decide whether the column can
      be resized. See the @link(TKGridBeginSizingEvent) type for parameter interpretation. }
    function BeginColSizing(var Index, Pos: Integer): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnBeginRowDrag) event handler or row class aware equivalent.
      See the @link(TKGridBeginDragEvent) type for parameter interpretation. }
    function BeginRowDrag(var Origin: Integer; const MousePt: TPoint): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnBeginRowSizing) event handler or checks the
      @link(TKGridAxisItem.CanResize) property to decide whether the row can
      be resized. See the @link(TKGridBeginSizingEvent) type for parameter interpretation. }
    function BeginRowSizing(var Index, Pos: Integer): Boolean; virtual;
    { Cancels any dragging or resizing operations performed by mouse. }
    procedure CancelMode; override;
    { Calls @link(TKCustomGrid.OnCellChanged) event handler. }
    procedure CellChanging(AOldCol, AOldRow, ANewCol, ANewRow: Integer); virtual;
    { This method is called periodically from the cell hint timer. }
    procedure CellHintTimerHandler(Sender: TObject); virtual;
    { In a non virtual grid, this method is called after @link(TKCustomGrid.OnEditorDestroy)
      if the cell content has been modified. Changed calls @link(TKCustomGrid.OnChanged)
      event handler. }
    procedure Changed; virtual;
    { Modifies the size of @link(FCols), @link(FRows) and @link(FCells). Updates
      @link(TKCustomGrid.FixedCols), @link(TKCustomGrid.ColCount), @link(TKCustomGrid.MaxCol),
      @link(TKCustomGrid.FixedRows), @link(TKCustomGrid.RowCount), @link(TKCustomGrid.MaxRow). }
    procedure ChangeDataSize(ColInsert: Boolean; ColAt, ColCnt: Integer;
      RowInsert: Boolean; RowAt, RowCnt: Integer); virtual;
    { Calls @link(TKCustomGrid.OnCheckColDrag) event handler or column class aware equivalent.
      See the @link(TKGridCheckDragEvent) type for parameter interpretation. }
    function CheckColDrag(Origin: Integer; var Destination: Integer;
      const MousePt: TPoint): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnCheckRowDrag) event handler or row class aware equivalent.
      See the @link(TKGridCheckDragEvent) type for parameter interpretation. }
    function CheckRowDrag(Origin: Integer; var Destination: Integer;
      const MousePt: TPoint): Boolean; virtual;
    { Forces the scrollable cell specified by ACol and ARow to become visible. }
    function ClampInView(ACol, ARow: Integer): Boolean;
    { Calls @link(TKCustomGrid.OnColumnMoved) event handler.
      See the @link(TKGridMovedEvent) type for parameter interpretation. }
    procedure ColMoved(FromIndex, ToIndex: Integer); virtual;
    { Calls @link(TKCustomGrid.OnColWidthsChanged) event handler. }
    procedure ColWidthsChanged(ACol: Integer); virtual;
    { Calls @link(TKCustomGrid.OnCompareCells) event handler for the given two cell instances. }
    function CompareCellInstances(ACell1, ACell2: TKGridCell): Integer; virtual;
    { Calls @link(TKCustomGrid.OnCompareCells) event handler for the given two cells. }
    function CompareCells(ACol1, ARow1, ACol2, ARow2: Integer): Integer; virtual;
    { Calls @link(TKCustomGrid.OnCompareCells) event handler for two cells
      belonging to the same row identified by ARow. ACol1 and ACol2 are column
      indexes of these two cells. Method is used to compare grid rows. }
    function CompareCols(ARow, ACol1, ACol2: Integer): Integer; virtual;
    { Calls @link(TKCustomGrid.OnCompareCells) event handler for two cells
      belonging to the same column identified by ACol. ARow1 and ARow2 are row
      indexes of these two cells. Method is used to compare grid columns. }
    function CompareRows(ACol, ARow1, ARow2: Integer): Integer; virtual;
    { Overriden method - see Delphi help. CreateParams defines additional styles
      for the KGrid window (scrollbars etc.)}
    procedure CreateParams(var Params: TCreateParams); override;
    { Calls @link(TKCustomGrid.OnCustomSortCols) event handler.
      See the @link(TKGridCustomSortEvent) type for parameter interpretation. }
    function CustomSortCols(ByRow: Integer; var SortMode: TKGridSortMode): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnCustomSortRows) event handler.
      See the @link(TKGridCustomSortEvent) type for parameter interpretation. }
    function CustomSortRows(ByCol: Integer; var SortMode: TKGridSortMode): Boolean; virtual;
    { Clears all user defined column widths. }
    procedure DefaultColWidthChanged; virtual;
    { Clears all user defined row heights. }
    procedure DefaultRowHeightChanged; virtual;
    { Provides default behavior for an inplace editor if it's caret should be
      positioned to the left side. }
    procedure DefaultSetCaretToLeft(Key: Word; ShiftState: TShiftState); virtual;
    { Defines the custom properties for *.dfm streaming. }
    procedure DefineProperties(Filer: TFiler); override;
    { Overriden method - see Delphi help. Responds to mouse wheel events. }
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    { Overriden method - see Delphi help. Responds to mouse wheel events. }
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    { Updates column/row dragging state if mouse is moved or scrolling is initiated by mouse.
      Called from @link(TKCustomGrid.MouseMove) and @link(TKCustomGrid.ScrollTimerHandler). }
    procedure DragMove(ACol, ARow: Integer; MousePt: TPoint);
    { Calls @link(TKCustomGrid.OnDrawCell) event handler or cell class aware equivalent.
      See the @link(TKGridDrawCellEvent) type for parameter interpretation. }
    function DrawCell(ACol, ARow: Integer; ARect: TRect;
      AState: TKGridDrawState): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnEditorCreate) event handler or cell class aware equivalent.
      See the @link(TKGridEditorCreateEvent) type for parameter interpretation. }
    function EditorCreate(ACol, ARow: Integer): TWinControl; virtual;
    { Calls @link(TKCustomGrid.OnEditorDataFromGrid) event handler or cell class aware equivalent.
      See the @link(TKGridEditorDataEvent) type for parameter interpretation. }
    procedure EditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer); virtual;
    { Calls @link(TKCustomGrid.OnEditorDataToGrid) event handler or cell class aware equivalent.
      See the @link(TKGridEditorDataEvent) type for parameter interpretation. }
    procedure EditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer); virtual;
    { Calls @link(TKCustomGrid.OnEditorDestroy) event handler or cell class aware equivalent.
      See the @link(TKGridEditorDestroyEvent) type for parameter interpretation. }
    procedure EditorDestroy(var AEditor: TWinControl; ACol, ARow: Integer); virtual;
    { Determines if the current inplace editor should be treated as transparent
      control from the grid's point of view. @link(TKCustomGrid.EditorTransparency)
      has higher priority than the default behavior implemented by this method. }
    function EditorIsTransparent: Boolean; virtual;
    { Calls @link(TKCustomGrid.OnEditorKeyPreview) event handler or cell class aware equivalent.
      See the @link(TKGridEditorDataEvent) type for parameter interpretation. }
    function EditorKeyPreview(AEditor: TWinControl; ACol, ARow: Integer;
      var Key: Word; Shift: TShiftState): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnEditorResize) event handler or cell class aware equivalent.
      See the @link(TKGridEditorResizeEvent) type for parameter interpretation. }
    procedure EditorResize(AEditor: TWinControl; ACol, ARow: Integer;
      var ARect: TRect); virtual;
    { Calls @link(TKCustomGrid.OnEditorSelect) event handler or cell class aware equivalent.
      See the @link(TKGridEditorSelectEvent) type for parameter interpretation. }
    procedure EditorSelect(AEditor: TWinControl; ACol, ARow: Integer;
      SelectAll, CaretToLeft, SelectedByMouse: Boolean); virtual;
    { EditorWindowProc is the subclassed window procedure for inplace editor. }
    procedure EditorWindowProc(var Msg: TLMessage); virtual;
    { Calls @link(TKCustomGrid.OnEndColDrag) event handler or column class aware equivalent.
      See the @link(TKGridEndDragEvent) type for parameter interpretation. }
    function EndColDrag(Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnEndColSizing) event handler.
      See the @link(TKGridEndSizingEvent) type for parameter interpretation. }
    function EndColSizing(var Index, Pos: Integer): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnEndRowDrag) event handler or row class aware equivalent.
      See the @link(TKGridEndDragEvent) type for parameter interpretation. }
    function EndRowDrag(Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; virtual;
    { Calls @link(TKCustomGrid.OnEndRowSizing) event handler.
      See the @link(TKGridEndSizingEvent) type for parameter interpretation. }
    function EndRowSizing(var Index, Pos: Integer): Boolean; virtual;
    { Destroys all column, row and cell instances. }
    procedure FreeData;
    { Returns information structure for column or row axis. Some fields of the
      Info structure must be already defined before calling this function.
      See @link(TKGridAxisInfo) for details. }
    procedure GetAxisInfo(var Info: TKGridAxisInfo); virtual;
    { Returns bounding rectangle where dragged column or row should appear. }
    function GetDragRect(Info: TKGridAxisInfoBoth; out DragRect: TRect): Boolean; virtual;
    { Returns the combination of invisible cells that must be taken into account
      for the state indicated by GridState.  }
    function GridStateToInvisibleCells: TKGridInvisibleCells;
    { Determines if the grid can have a horizontal scrollbar. }
    function HasHorzScrollBar: Boolean; virtual;
    { Determines if the grid can have a vertical scrollbar. }
    function HasVertScrollBar: Boolean; virtual;
    { Used internally to physically exchange two distinct columns. }
    procedure InternalExchangeCols(Index1, Index2: Integer); virtual;
    { Used internally to physically exchange two distinct rows. }
    procedure InternalExchangeRows(Index1, Index2: Integer); virtual;
    { Used internally to check if the given grid rectangle contains any merged cell areas
      and if so, then expand it so that the result encloses all respective merged cells. }
    function InternalExpandGridRect(const GridRect: TKGridRect): TKGridRect; virtual;
    { Retrieves the base cell if the cell given by ACol and ARow belongs to a merged cell
      or returns ACol and ARow if it is a non-merged cell. }
    procedure InternalFindBaseCell(ACol, ARow: Integer; out BaseCol, BaseRow: Integer); virtual;
    { Used internally to reverse the order of previously sorted rows or columns
      in a fast manner, without cell comparisons. }
    procedure InternalFlip(Left, Right: Integer; Exchange: TKGridExchangeProc); virtual;
    { Used internally. Returns a cell instance for the cell identified by ACol and ARow. If the
      cell instance is nil, creates a new instance for the cell using
      @link(TKCustomGrid.CellClass). }
    function InternalGetCell(ACol, ARow: Integer): TKGridCell; virtual;
    { Returns the column span and row span for given cell. Does not perform cell validity check. }
    function InternalGetCellSpan(ACol, ARow: Integer): TKGridCellSpan; virtual;
    { Returns the column width. Does not perform column validity check. }
    function InternalGetColWidths(Index: Integer): Integer; virtual;
    { Returns the effective column spacing. Does not perform column validity check. }
    function InternalGetEffectiveColSpacing(ACol: Integer): Integer; virtual;
    { Returns the effective row spacing. Does not perform row validity check. }
    function InternalGetEffectiveRowSpacing(ARow: Integer): Integer; virtual;
    { Returns width and spacing for several cells according to given parameters. }
    procedure InternalGetHExtent(AIndex, AColSpan: Integer;
      out DestExtent, DestSpacing: Integer); virtual;
    { Returns the maximum column width. Does not perform column validity check. }
    function InternalGetMaxColWidth(Index: Integer): Integer; virtual;
    { Returns the maximum row height. Does not perform row validity check. }
    function InternalGetMaxRowHeight(Index: Integer): Integer; virtual;
    { Returns the minimum column width. Does not perform column validity check. }
    function InternalGetMinColWidth(Index: Integer): Integer; virtual;
    { Returns the minimum row height. Does not perform row validity check. }
    function InternalGetMinRowHeight(Index: Integer): Integer; virtual;
    { Returns the row height. Does not perform row validity check. }
    function InternalGetRowHeights(Index: Integer): Integer; virtual;
    { Returns always True. }
    function InternalGetSelAvail: Boolean; override;
    { Returns height and spacing for several cells according to given parameters. }
    procedure InternalGetVExtent(AIndex, ARowSpan: Integer;
      out DestExtent, DestSpacing: Integer); virtual;
    { Used internally by e.g. @link(TKCustomGrid.InsertSortedRow) to insert
      a new row/column into previously sorted rows/column in a fast manner,
      using a binary tree search. }
    function InternalInsertNR(ByIndex, Left, Right: Integer;
      SortedUp: Boolean; Compare: TKGridCompareProc): Integer; virtual;
    { Used internally by @link(TKCustomGrid.KeyDown) or other methods.
      Modifies ACol and ARow according to Command. }
    function InternalMove(var ACol, ARow: Integer; Command: TKGridMoveCommand;
      Wrap, Expanding: Boolean): Boolean; virtual;
    { Used internally by @link(TKCustomGrid.UpdateSortMode) to place a modified
      cell into a correct location in a sorted row or column. This is performed
      in a fast manner using a binary tree search. }
    function InternalInsertIfCellModifiedNR(ByIndex, Index, Left,
      Right: Integer; SortedUp: Boolean; Compare: TKGridCompareProc): Integer;
    { Paints a cell identified by ACol and ARow. The cell will be painted to
      ACanvas according to the draw state specified by AState into a position
      specified by ARect. If ADoubleBufferedCells is True, ACanvas must be
      a memory device context. PaintCell ensures the correct memory bitmap for
      cell double buffering will be selected to this device context. }
    procedure InternalPaintCell(ACol, ARow: Integer; AState: TKGridDrawState;
      const ARect, ABlockRect: TRect; ACanvas: TCanvas; Clip, Printing: Boolean); virtual;
    { Used internally by e.g. @link(TKCustomGrid.SortRows) to sort rows or columns
      using a non recursive quick sort algorithm. }
    procedure InternalQuickSortNR(ByIndex, Left, Right: Integer;
      SortedDown: Boolean; Compare: TKGridCompareProc; Exchange: TKGridExchangeProc); virtual;
    { Used internally to assign new cell value. }
    procedure InternalSetCell(ACol, ARow: Integer; Value: TKGridCell); virtual;
    { Used internally to assign new text to a cell. }
    procedure InternalSetCells(ACol, ARow: Integer; const Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}); virtual;
    { Sets the cell span paramters according to given parameters. Automatically
      splits any existing overlapping areas. Returns a grid rectangle that can
      be used to update all affected cells. }
    function InternalSetCellSpan(ACol, ARow: Integer;
      const Value: TKGridCellSpan): TKGridRect; virtual;
    { Used internally to set column count. }
    procedure InternalSetColCount(Value: Integer); virtual;
    { Used internally to set fixed column count. }
    procedure InternalSetFixedCols(Value: Integer); virtual;
    { Used internally to set fixed row count. }
    procedure InternalSetFixedRows(Value: Integer); virtual;
    { Used internally to set row count. }
    procedure InternalSetRowCount(Value: Integer); virtual;
    { Allows the descendant to decide whether the goVirtualGrid option can be modified. }
    function InternalUpdateVirtualGrid: Boolean; virtual;
    { Allows the changes to be reflected. }
    procedure InternalUnlockUpdate; override;
    { Determines if control can be painted with OS themes. }
    function IsThemed: Boolean; override;
    { Overriden method - see Delphi help. Responds to keyboard events. Implements
      TCustomGrid specific behavior when the user presses a key. }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Overriden method - performs late update. }
    procedure LateUpdate(var Msg: TLMessage); override;
    { Overriden method - see Delphi help. Updates grid colors. }
    procedure Loaded; override;
    { Calls @link(TKCustomGrid.OnMeasureCell) event handler or cell class aware equivalent.
      See the @link(TKGridMeasureCellEvent) type for parameter interpretation. }
    function MeasureCell(ACol, ARow: Integer; const ARect: TRect;
      AState: TKGridDrawState; Priority: TKGridMeasureCellPriority): TPoint; virtual;
    { Measures the grid and updates information about printed shape. }
    procedure MeasurePages(var Info: TKPrintMeasureInfo); override;
    { Calls @link(TKCustomGrid.OnMouseCellHint) event handler.
      See the @link(TKGridCellEvent) type for parameter interpretation. }
    procedure MouseCellHint(ACol, ARow: Integer; AShow: Boolean); virtual;
    { Calls @link(TKCustomGrid.OnMouseClickCell) event handler.
      See the @link(TKGridCellEvent) type for parameter interpretation. }
    procedure MouseClickCell(ACol, ARow: Integer); virtual;
    { Calls @link(TKCustomGrid.OnMouseDblClickCell) event handler.
      See the @link(TKGridCellEvent) type for parameter interpretation. }
    procedure MouseDblClickCell(ACol, ARow: Integer); virtual;
    { Overriden method - see Delphi help. Responds to mouse events. Implements
      TCustomGrid specific behavior when the user presses a mouse button. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    { Calls @link(TKCustomGrid.OnMouseEnterCell) event handler.
      See the @link(TKGridCellEvent) type for parameter interpretation. }
    procedure MouseEnterCell(ACol, ARow: Integer); virtual;
    { Overriden method. Responds to WM_MOUSELEAVE message. }
    procedure MouseFormLeave; override;
    { Calls @link(TKCustomGrid.OnMouseLeaveCell) event handler.
      See the @link(TKGridCellEvent) type for parameter interpretation. }
    procedure MouseLeaveCell(ACol, ARow: Integer); virtual;
    { Overriden method - see Delphi help. Responds to mouse events. Implements
      TCustomGrid specific behavior when the user moves the mouse cursor. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Implements default behavior to visually indicate that the mouse cursor
      enters or leaves the cell if goMouseOverCells is included in @link(TKCustomGrid.Options). }
    procedure MouseOverCells; virtual;
    { Overriden method - see Delphi help. Responds to mouse events. Implements
      TCustomGrid specific behavior when the user releases a mouse button. }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    { Returns the amount of rows in current page, minimum is 1. }
    function PageHeight: Integer; virtual;
    { Returns the amount of columns in current page, minimum is 1. }
    function PageWidth: Integer; virtual;
    { Paints a range of cells. }
    function PaintCells(ACanvas: TCanvas; CellBitmap: TBitmap;
      MainClipRgn: HRGN; FirstCol, LastCol, FirstRow, LastRow, X, Y, MaxX,
      MaxY: Integer; Printing, PaintSelection: Boolean; const ABlockRect: TRect): TPoint;
    { Paints the suggestion for drop target when dragging a column or row. }
    procedure PaintDragSuggestion(ACanvas: TCanvas); virtual;
    { Paints a header terminating rectangle to align the header with the right
      client area edge. }
    procedure PaintHeaderAlignment(ACanvas: TCanvas; ARect: TRect); virtual;
    { Paints a page to a printer/preview canvas. }
    procedure PaintPage; override;
    { Paints the suggestion for new width/height of a column/row being resized. }
    procedure PaintSizingSuggestion(ACanvas: TCanvas); virtual;
    { Determines which cell lies at client coordinates specified by Point.
      Set OutSide to True to evaluate a cell that does not actually lie at Point
      but is the closest. Such a cell always lies at the boundary of scrollable
      cell area. This is used for scrolling by mouse. InvisibleCells specifies
      if some invisible cells should be considered in some cases. Currently, this
      is used for scrolling by mouse, either. This function returns True if the
      corresponding cell has been found. In this case, ACol and ARow contain
      column and row indexes of the returned cell. }
    function PointToCell(Point: TPoint; OutSide: Boolean; InvisibleCells: TKGridInvisibleCells;
      out HitCol, HitRow, SelCol, SelRow: Integer): Boolean; virtual;
    { Determines the possible column or row sizing state along with default sizing
      parameters for client coordinates specified by Point. The possible sizing
      state is returned in State and sizing parameters in Index and Pos. This
      function returns True if Point is in an area where sizing of a column or
      row can begin. }
    function PointToSizing(Point: TPoint; var State: TKGridState;
      var Index, Pos: Integer): Boolean; virtual;
    { Updates drag object's (layered) window used to visually indicate the dragged
      column or row. This window is updated according to mouse cursor coordinates
      in MousePt, column or row index specified by Index. The Hide parameter forces
      the window to hide and thus visually indicate that column or row dragging has ended. }
    procedure ProcessDragWindow(const PtIni, PtCur: TPoint; Index: Integer; ColDrag, Hide: Boolean); virtual;
    { Resets the @link(TKCustomGrid.LeftCol) and @link(TKCustomGrid.TopRow) property
      after the @link(TKCustomGrid.FixedCols) or @link(TKCustomGrid.FixedRows)
      properties have changed. }
    procedure ResetTopLeft; virtual;
    { Calls @link(TKCustomGrid.OnRowMoved) event handler.
      See the @link(TKGridMovedEvent) type for parameter interpretation. }
    procedure RowMoved(FromIndex, ToIndex: Integer); virtual;
    { Calls @link(TKCustomGrid.OnRowHeightsChanged) event handler. }
    procedure RowHeightsChanged(ARow: Integer); virtual;
    { Tries to set input focus to the grid if @link(TKCustomGrid.EditorMode)
      is False or to the inplace editor if EditorMode is True. }
    procedure SafeSetFocus; virtual;
    { Scrolls the scrollable cells either horizontally by DeltaHorz or vertically
      by DeltaVert or in both directions. CodeHorz and CodeVert are the codes
      coming from WM_HSCROLL or WM_VSCROLL messages. Set CallUpdateEditor to True
      to call @link(TKCustomGrid.UpdateEditor) within this method to scroll
      the inplace editor, either. Set CallUpdateEditor to False if you don't want
      to scroll the inplace editor and update it by some other means, such as
      @link(TKCustomgrid.SelectionMove). This method avoids inplace editor flickering
      when scrolling with EditorMode = True. }
    procedure Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer;
      CallUpdateEditor: Boolean); virtual;
    { This method is called periodically from the timer used to automatically
      scroll the scrollable cells while the mouse pointer is captured and
      held outside the grid client area. }
    procedure ScrollTimerHandler(Sender: TObject); virtual;
    { Calls @link(TKCustomGrid.OnSelectCell) event handler or cell class aware equivalent
      See the @link(TKGridSelectCellEvent) type for parameter interpretation. }
    function SelectCell(ACol, ARow: Integer): Boolean; virtual;
    procedure SelectionChanged(NewSelection: TKGridRect;
      Flags: TKGridSelectionFlags);
    { Calls @link(TKCustomGrid.OnSelectionExpand) event handler or cell class aware equivalent
      See the @link(TKGridSelectionExpandEvent) type for parameter interpretation. }
    function SelectionExpand(ACol, ARow: Integer): Boolean; virtual;
    { Adjusts the grid rectangle identified by Sel and makes it valid. This method
      is intended to adjust FSelection or a rectangle assumed to be assigned
      to FSelection later. }
    procedure SelectionFix(var Sel: TKGridRect); virtual;
    { Initializes or expands the current selection and performs all necessary adjustments.
      ACol and ARow are the indexes used to initialize or expand the selection.
      Stage determines, if the selection should be initialized or expanded.
      Flags forces various adjustments to be performed after the selection has been
      initialized or expanded. Returns True if the selection could be changed or
      would not be modified, either. }
    function SelectionMove(ACol, ARow: Integer; Stage: TKGridSelectionStage;
      Flags: TKGridSelectionFlags): Boolean; virtual;
    { Assigns new selection and performs all necessary adjustments. }
    function SelectionSet(const NewSelection: TKGridRect): Boolean;
  {$IFDEF FPC}
    { Overriden LCL method. This allows a custom mouse cursor to be assigned for the grid. }
    procedure SetCursor(Value: TCursor); override;
  {$ENDIF}
    { Updates mouse cursor according to the grid state determined from current mouse
      position. Returns True if cursor has been changed. }
    function SetMouseCursor(X, Y: Integer): Boolean; override;
    { Calls @link(TKCustomGrid.OnSizeChanged) event handler.
      See the @link(TKGridSizeChangedEvent) type for parameter interpretation. }
    procedure SizeChanged(Change: TKGridSizeChange; Index, Count: Integer); virtual;
    { Forces the column/row dragging suggestion to be created, destroyed or
      temporarilly hidden and shown, depending on the State parameter. }
    procedure SuggestDrag(State: TKGridCaptureState); virtual;
    { Forces the column/row sizing suggestion to be created, destroyed or
      temporarilly hidden and shown, depending on the State parameter. }
    procedure SuggestSizing(State: TKGridCaptureState); virtual;
    { Calls @link(TKCustomGrid.OnTopLeftChanged) event handler. }
    procedure TopLeftChanged; virtual;
    { Updates the column axis if Horz is True and/or row axis if Vert is True.
      Adjusts column widths/row heights if goAlignLastCol/goAlignLastRow
      is included in @link(TKCustomGrid.Options). Adjusts scrolling range -
      calls @link(TKCustomgrid.UpdateScrollRange). Invalidates columns/rows
      as needed or starting by column/row index given by FirstCol/FirstRow.
      Specify @link(cAll) as FirstCol/FirstRow to invalidate all columns/rows.
      Performs additional actions as specified by Flags. }
    procedure UpdateAxes(Horz: Boolean; FirstCol: Integer; Vert: Boolean;
      FirstRow: Integer; Flags: TKGridAxisUpdateFlags); virtual;
    { Updates/re-calculates the column/row span paramteres of all cells
      if necessary. Fixes all broken or incomplete merged cell areas, e.g. upon
      column or row moving or grid resizing. }
    procedure UpdateCellSpan; virtual;
    { Updates the grid size. }
    procedure UpdateSize; override;
    { Updates the Delphi form designer if @link(TKCustomGrid.ColWidths) or
      @link(TKCustomGrid.RowHeights) have been changed. }
    procedure UpdateDesigner; virtual;
    { Updates the inplace editor state. Set Show to True to create and display
      the inplace editor. Set Show to False to hide and destroy the inplace editor. }
    procedure UpdateEditor(Show: Boolean); virtual;
    { Updates the scrolling range of the column axis if Horz is True and/or row
      axis if Vert is True. Set UpdateNeeded to True to force the invalidation
      of respective grid areas. Set UpdateNeeded to False to let UpdateScrollRange
      decide whether these need to be invalidated. }
    procedure UpdateScrollRange(Horz, Vert, UpdateNeeded: Boolean); virtual;
  {$IFNDEF FPC}
    { Inherited method. Used to ensure correct painting for transparent inplace
      editors. }
    procedure WndProc(var Msg: TMessage); override;
  {$ENDIF}
  public
    { Creates the instance. Assigns default values to properties, allocates
      default column, row and cell data. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the instance along with all allocated column, row and cell data. }
    destructor Destroy; override;
    { Resizes the column automatically so that the cell contents fit horizontally.
      Does include merged cell areas with their base cells located in this column.
      Set FixedCells to True to include fixed cells into autosizing. }
    procedure AutoSizeCol(ACol: Integer; FixedCells: Boolean = True);
    { Resizes the entire grid automatically so that the cell contents fit both
      horizontally and vertically. Set FixedCells to True to include fixed cells
      into autosizing. }
    procedure AutoSizeGrid(Priority: TKGridMeasureCellPriority; FixedCells: Boolean = True);
    { Resizes the row automatically so that the cell contents fit vertically.
      Does include merged cell areas with their base cells located in this row.
      Set FixedCells to True to include fixed cells into autosizing. }
    procedure AutoSizeRow(ARow: Integer; FixedCells: Boolean = True);
    { Determines if a cell specified by ACol and ARow is selected. }
    function CellSelected(ACol, ARow: Integer): Boolean; virtual;
    { Returns the bounding rectangle of a cell specified by ACol and ARow without the
      column and row spacing areas defined by @link(TKCustomGrid.GridLineWidth).
      The function returns False if the cell indexes are invalid. }
    function CellRect(ACol, ARow: Integer; out R: TRect; VisibleOnly: Boolean = False): Boolean;
    { Returns the left and top coordinates of a cell specified by ACol and ARow.
      The function returns False if the cell indexes are invalid. }
    function CellToPoint(ACol, ARow: Integer; var Point: TPoint;
      VisibleOnly: Boolean = False): Boolean; virtual;
    { Determines if a cell specified by ACol and ARow is visible. }
    function CellVisible(ACol, ARow: Integer): Boolean; virtual;
    { Clears all cells in a column identified by ACol. }
    procedure ClearCol(ACol: Integer); virtual;
    { Clears all cells. }
    procedure ClearGrid; virtual;
    { Clears all cells in a row identified by ARow. }
    procedure ClearRow(ARow: Integer); virtual;
    { Clears sorting mode of both rows and columns if grid sorting mode is not locked
      by @link(TKCustomGrid.LockSortMode). }
    procedure ClearSortMode;
    { Clears sorting mode of rows if grid sorting mode is not locked
      by @link(TKCustomGrid.LockSortMode). Ensures that every column has it's
      @link(TKGridAxisItem.SortMode) equal to smNone. }
    procedure ClearSortModeHorz; virtual;
    { Clears sorting mode of columns if grid sorting mode is not locked
      by @link(TKCustomGrid.LockSortMode). Ensures that every row has it's
      @link(TKGridAxisItem.SortMode) equal to smNone. }
    procedure ClearSortModeVert; virtual;
    { Determines if a column specified by ACol can be selected,
      i.e. lies in non-fixed area. }
    function ColSelectable(ACol: Integer): Boolean; virtual;
    { Determines if current selection includes a column specified by ACol. }
    function ColSelected(ACol: Integer): Boolean; virtual;
    { Determines if a column specified by ACol is valid column. }
    function ColValid(ACol: Integer): Boolean; virtual;
    { Decides whether a key stroke should be handled by inplace editor identified by
      AEditor or by the grid. AEditor must be a descendant of
      TCustomComboBox. See @link(TKGridEditorKeyPreviewEvent) for interpretation of
      another parameters. }
    procedure DefaultComboKeyPreview(AEditor: TComboBox; ACol, ARow: Integer;
      var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean); virtual;
    { This function allows you to correctly set the caret position within
      inplace editor identified by AEditor. AEditor must be a descendant of TCustomComboBox.
      See @link(TKGridEditorSelectEvent) for interpretation of another parameters. }
    procedure DefaultComboSelect(AEditor: TComboBox; SelectAll, CaretToLeft: Boolean); virtual;
    { Provides default behavior while comparing two cells identified by
      ACell1 and ACell2. Under current implementation, only text strings will be
      compared if if any of the cells inherits @link(TKGridTextCell). }
    function DefaultCompareCells(ACell1, ACell2: TKGridCell): Integer; virtual;
    { Decides whether a key stroke should be handled by inplace editor identified by
      AEditor or by the grid. AEditor must be a descendant of
      TCustomEdit. See @link(TKGridEditorKeyPreviewEvent) for interpretation of
      another parameters. }
    procedure DefaultEditKeyPreview(AEditor: TCustomEdit; ACol, ARow: Integer;
      var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean); virtual;
    { Provides default behavior for the @link(OnEditorCreate) event. }
    procedure DefaultEditorCreate(ACol, ARow: Integer;
      var AEditor: TWinControl); virtual;
    { Provides default behavior for the @link(OnEditorDataFromGrid) event. }
    procedure DefaultEditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer;
      var AssignText: Boolean); virtual;
    { Provides default behavior for the @link(OnEditorDataToGrid) event. }
    procedure DefaultEditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer;
      var AssignText: Boolean); virtual;
    { Provides default behavior for the @link(OnEditorCreate) event. }
    procedure DefaultEditorDestroy(AEditor: TWinControl; ACol, ARow: Integer); virtual;
    { Decides whether a key stroke should be handled by inplace editor identified by
      AEditor or by the grid. Calls all implemented DefaultxxKeyPreview methods
      or nothing if no ancestor is found for given AEditor.
      See @link(TKGridEditorKeyPreviewEvent) for interpretation of another parameters. }
    procedure DefaultEditorKeyPreview(AEditor: TWinControl; ACol, ARow: Integer;
      var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean); virtual;
    { Provides default behavior for the @link(OnEditorResize) event. }
    procedure DefaultEditorResize(AEditor: TWinControl; ACol, ARow: Integer;
      var ARect: TRect); virtual;
    { This function allows you to correctly set the caret position within
      inplace editor identified by AEditor. Calls all implemented DefaultxxSelect methods
      or nothing if no ancestor is found for given AEditor.
      See @link(TKGridEditorSelectEvent) for interpretation of another parameters. }
    procedure DefaultEditorSelect(AEditor: TWinControl; ACol, ARow: Integer;
      SelectAll, CaretToLeft, SelectedByMouse: Boolean); virtual;
    { This function allows you to correctly set the caret position within
      inplace editor identified by AEditor. AEditor must be a descendant of TCustomEdit.
      See @link(TKGridEditorSelectEvent) for interpretation of another parameters. }
    procedure DefaultEditSelect(AEditor: TCustomEdit; SelectAll, CaretToLeft: Boolean); virtual;
    { Provides default cell hint behavior. }
    procedure DefaultMouseCellHint(ACol, ARow: Integer; AShow: Boolean); virtual;
    { Decides whether a key stroke should be handled by inplace editor identified by
      AEditor or by the grid. AEditor must be a descendant of
      TScrollBar. See @link(TKGridEditorKeyPreviewEvent) for interpretation of
      another parameters. }
    procedure DefaultScrollBarKeyPreview(AEditor: TScrollBar; ACol, ARow: Integer;
      var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
    { Deletes a column specified by At. At must be valid column index and
      @link(TKCustomGrid.ColCount) must be > 1. Otherwise, nothing happens. }
    procedure DeleteCol(At: Integer); virtual;
    { Deletes Count columns starting at index At. At must be valid column index
      and @link(TKCustomGrid.ColCount) must be > 1. Otherwise, nothing happens.
      Count will be adapted so that no more but available columns will be deleted. }
    procedure DeleteCols(At, Count: Integer); virtual;
    { Deletes a row specified by At. At must be valid row index and
      @link(TKCustomGrid.RowCount) must be > 1. Otherwise, nothing happens. }
    procedure DeleteRow(At: Integer); virtual;
    { Deletes Count rows starting at index At. At must be valid row index
      and @link(TKCustomGrid.RowCount) must be > 1. Otherwise, nothing happens.
      Count will be adapted so that no more but available rows will be deleted. }
    procedure DeleteRows(At, Count: Integer); virtual;
    { Retrieves the base cell if the cell given by ACol and ARow belongs to a merged cell
      or returns ACol and ARow if it is a non-merged cell. }
    procedure FindBaseCell(ACol, ARow: Integer; out BaseCol, BaseRow: Integer); virtual;
    { Selects a cell specified by ACol and ARow. If the grid has input focus,
      this cell becomes it automatically. }
    procedure FocusCell(ACol, ARow: Integer);
    { Returns miscellaneous information about both grid axes, i.e. column axis and row axis. }
    function GetAxisInfoBoth(Mask: TKGridAxisInfoMask): TKGridAxisInfoBoth;
    { Returns miscellaneous information about column axis. }
    function GetAxisInfoHorz(Mask: TKGridAxisInfoMask): TKGridAxisInfo; virtual;
    { Returns miscellaneous information about row axis. }
    function GetAxisInfoVert(Mask: TKGridAxisInfoMask): TKGridAxisInfo; virtual;
    { Returns default draw state for a cell identified by ACol and ARow.
      Called by Paint - override to implement specific behavior. }
    function GetDrawState(ACol, ARow: Integer; AFocused: Boolean): TKGridDrawState; virtual;
    { Determines if the entire grid rectangle lies within the non-fixed and thus
      selectable area. }
    function GridRectSelectable(const GridRect: TKGridRect): Boolean; virtual;
    { Converts a grid rectangle into client coordinates. Set VisibleOnly to True
      to take only the visible part of the rectangle. Indexes in GridRect will be
      automatically trimmed either to non-fixed area or to a fixed area depending
      on top-left cell specified in GridRect. Set Merged to True to expand the grid
      rectangle by possible merged cell areas. The returned coordinates include
      column and row spacing areas defined by @link(TKCustomGrid.GridLineWidth). }
    function GridRectToRect(GridRect: TKGridRect; var R: TRect;
      VisibleOnly: Boolean = False; Merged: Boolean = True): Boolean; virtual;
    { Determines if all indexes in GridRect are valid column or row indexes. }
    function GridRectValid(const GridRect: TKGridRect): Boolean; virtual;
    { Determines if the grid, inplace editor or any child window of inplace editor
      has input focus. }
    function HasFocus: Boolean; virtual;
    { Forces the cell hint to hide. }
    procedure HideCellHint;
    { Determines the initial index of a column identified by ACol. This function
      is a part of index mapping mechanism. Initial index is assigned to a column
      immediately after it is inserted into the grid either by changing
      @link(TKCustomGrid.ColCount) or @link(TKCustomGrid.InsertCols). }
    function InitialCol(ACol: Integer): Integer; virtual;
    { Determines the current column index from initial column position given by ACol.
      This function is a part of index mapping mechanism. }
    function InitialColInv(ACol: Integer): Integer; virtual;
    { Determines the initial index of a row identified by ARow. This function
      is a part of index mapping mechanism. Initial index is assigned to a row
      immediately after it is inserted into the grid either by changing
      @link(TKCustomGrid.RowCount) or @link(TKCustomGrid.InsertRows). }
    function InitialRow(ARow: Integer): Integer; virtual;
    { Determines the current row index from initial row position given by ARow.
      This function is a part of index mapping mechanism. }
    function InitialRowInv(ARow: Integer): Integer; virtual;
    { Inserts a new column into the grid. The new column will be inserted before
      the column identified by At. You can set this parameter greater or equal
      @link(TKCustomGrid.ColCount) to insert a new column behind the last column. }
    procedure InsertCol(At: Integer); virtual;
    { Inserts multiple new columns into the grid. The new columns will be inserted
      before the column identified by At. You can set this parameter greater or equal
      @link(TKCustomGrid.ColCount) to insert these after the last column. }
    procedure InsertCols(At, Count: Integer); virtual;
    { Inserts a new row into the grid. The new row will be inserted before
      the row identified by At. You can set this parameter greater or equal
      @link(TKCustomGrid.RowCount) to insert a new row behind the last row. }
    procedure InsertRow(At: Integer); virtual;
    { Inserts multiple new rows into the grid. The new rows will be inserted
      before the row identified by At. You can set this parameter greater or equal
      @link(TKCustomGrid.RowCount) to insert these after the last row. }
    procedure InsertRows(At, Count: Integer); virtual;
    { Inserts an empty column at the corresponding position. If columns are not sorted
      at this point, InsertSortedCol does nothing and returns False. During
      InsertSortedCol, a non recursive binary tree search is performed and
      the @link(TKCustomGrid.OnCompareCells) event handler is called several times
      with slightly different parameters than e.g. during @link(TKCustomGrid.SortCols),
      i.e. the ACol1 is always @link(cInvalidIndex). You can detect it to perform
      custom comparisons with the new value. }
    function InsertSortedCol(out ByRow, ACol: Integer): Boolean; virtual;
    { Inserts an empty row at the corresponding position. If rows are not sorted
      at this point, InsertSortedRow does nothing and returns False. During
      InsertSortedRow, a non recursive binary tree search is performed and
      the @link(TKCustomGrid.OnCompareCells) event handler is called several times
      with slightly different parameters than e.g. during @link(TKCustomGrid.SortRows),
      i.e. the ARow1 is always @link(cInvalidIndex). You can detect it to perform
      custom comparisons with the new value. }
    function InsertSortedRow(out ByCol, ARow: Integer): Boolean; virtual;
    { Invalidates the cell specified by ACol and ARow if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateCell(ACol, ARow: Integer);
    { Invalidates the entire column specified by ACol if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateCol(ACol: Integer); virtual;
    { Invalidates all columns starting with FirstCol if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateCols(FirstCol: Integer); virtual;
    { Invalidates the current selection including the fixed cells in
      @link(goIndicateSelection) mode if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateCurrentSelection; virtual;
    { Invalidates the grid rectangle specified by GridRect if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateGridRect(const GR: TKGridRect; Merged: Boolean = True); virtual;
    { Invalidates the entire row specified by ARow if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateRow(ARow: Integer); virtual;
    { Invalidates all rows starting with FirstRow if grid updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateRows(FirstRow: Integer); virtual;
    { Invalidates any custom grid rectangle that should be treated as grid selection,
      including the fixed cells in @link(goIndicateSelection) mode,
      if grid updating is not locked by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateSelection(ASelection: TKGridRect); virtual;
    { Returns True either if the DoubleBuffered property is True
      or if @link(goDoubleBufferedCells) is included in grid's @link(TKCustomGrid.Options).}
    function IsDoubleBuffered: Boolean; virtual;
    { Locks sort mode updating so that all changes made to the cell data
      will not affect the current sort status of any column or row. Every LockSortMode
      call must have a corresponding @link(TKCustomGrid.UnlockSortMode) call, please use a
      try-finally section. }
    procedure LockSortMode; virtual;
    { Determines the cell that contains client area coordinates X and Y.
      If there is such a cell, the function returns True and corresponding cell
      indexes are returned in ACol and ARow. Otherwise, the function returns False. }
    function MouseToCell(X, Y: Integer; var ACol, ARow: Integer): Boolean;
    { Moves a column from a position specified by FromIndex to a new
      position specified by ToIndex. Both column indexes must be valid and
      FromIndex must not equal to ToIndex. Otherwise, nothing happens. }
    procedure MoveCol(FromIndex, ToIndex: Integer); virtual;
    { Moves a row from a position specified by FromIndex to a new
      position specified by ToIndex. Both row indexes must be valid and
      FromIndex must not equal to ToIndex. Otherwise, nothing happens. }
    procedure MoveRow(FromIndex, ToIndex: Integer); virtual;
    { Forces to move the input focus to the next cell according to
      @link(TKCustomGrid.MoveDirection) and calls OnClick event if that succeeds. }
    procedure MoveToNextCell; virtual;
    { Paints a cell identified by ACol and ARow to ACanvas.
      This is faster way than InvalidateCell but won't work under Qt.
      Set ACanvas to nil to paint to grid's Canvas. Otherwise, set AX and AY
      to specify painting origin on custom ACanvas. }
    procedure PaintCell(ACanvas: TCanvas; ACol, ARow: Integer;
      AX: Integer = 0; AY: Integer = 0; APrinting: Boolean = False; 
      ABlockRect: PRect = nil); virtual;
    { Paints the control to the specified canvas. }
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    { Posts a message to system queue to (re)create the inplace editor. }
    procedure PostRecreateEditor; virtual;
    { Forces the cell class specified by @link(TKCustomGrid.CellClass) to replace
      all other cell classes that do not inherit from it. Call this method to
      ensure that all the cells in the grid contain instances of CellClass or those
      inherited from CellClass. All possible cell class properties are copied by
      the @link(TKGridCell.Assign) method. }
    procedure RealizeCellClass;
    { Forces the column class specified by @link(TKCustomGrid.ColClass) to replace
      all other column classes that do not inherit from it. Call this method to
      ensure that the entire horizontal grid axis contains instances of ColClass
      or those inherited from ColClass. All possible column class properties are
      copied by the @link(TKGridAxisItem.Assign) method. }
    procedure RealizeColClass;
    { Forces the row class specified by @link(TKCustomGrid.RowClass) to replace
      all other row classes that do not inherit from it. Call this method to
      ensure that the entire vertical grid axis contains instances of RowClass
      or those inherited from RowClass. All possible row class properties are
      copied by the @link(TKGridAxisItem.Assign) method. }
    procedure RealizeRowClass;
    { Determines if a row specified by ARow can be selected,
      i.e. lies in non-fixed area. }
    function RowSelectable(ARow: Integer): Boolean; virtual;
    { Determines if current selection includes a row specified by ARow. }
    function RowSelected(ARow: Integer): Boolean; virtual;
    { Determines if a row specified by ARow is valid row. }
    function RowValid(ARow: Integer): Boolean; virtual;
    { Scrolls the non-fixed cells horizontally by AColCount cells or vertically
      by ARowCount cells. If the cells cannot be scrolled, nothing happens. }
    procedure ScrollBy(AColCount, ARowCount: Integer);
    { Retrieves the amount of pixels corresponding to the amount of cells
      specified by ADelta, relative from @link(TKCustomGrid.LeftCol) and
      @link(TKCustomGrid.TopRow). }
    function ScrollDeltaFromDelta(const Info: TKGridAxisInfo; ADelta: Integer): Integer; virtual;
    { Determines if a cell specified by ACol and ARow should be scrolled, i.e. is
      not fully visible. }
    function ScrollNeeded(ACol, ARow: Integer; out DeltaHorz, DeltaVert: Integer): Boolean; virtual;
    { Selects all cells. }
    procedure SelectAll;
    { Selects a column. }
    procedure SelectCol(ACol: Integer);
    { Select more columns. }
    procedure SelectCols(FirstCol, Count: Integer);
    { Normalize current selection. }
    procedure SelectionNormalize;
    { Selects a row. }
    procedure SelectRow(ARow: Integer);
    { Selects more rows. }
    procedure SelectRows(FirstRow, Count: Integer);
    { Forces the cell hint to show on screen. }
    procedure ShowCellHint;
    { Sorts columns by values of a row if grid sorting mode is not locked
      by @link(TKCustomGrid.LockSortMode). }
    procedure SortCols(ByRow: Integer; SortMode: TKGridSortMode); virtual;
    { Returns True if sort mode updating is not locked, i.e. there is no open
      LockSortMode and UnlockSortMode pair. }
    function SortModeUnlocked: Boolean; virtual;
    { Sorts rows by values of a column if grid sorting mode is not locked
      by @link(TKCustomGrid.LockSortMode). }
    procedure SortRows(ByCol: Integer; SortMode: TKGridSortMode); virtual;
    { Unlocks sort mode updating so that all changes made to the cell data
      will clear the current sort status of any column or row. }
    procedure UnlockSortMode; virtual;
    { Unselects range of cells. }
    procedure UnselectRange;
    { Updates column and row sorting mode (if there is one) if data has been
      modified in a single cell. Must be called explicitly each time a cell data
      has been modified if sorting interface is used. }
    procedure UpdateSortMode(ACol, ARow: Integer); virtual;
    { Provides fast read only access to the cell array @link(TKCustomGrid.FCells).
      Any cell can be directly accessed through ArrayOfCells[RowIndex, ColIndex].
      In contrast with the @link(TKCustomGrid.Cell) property, row index
      comes BEFORE column index here. It has been designed to speed up operations
      with rows because most grids usually contain much more rows than colums. }
    property ArrayOfCells: TKGridCells read FCells;
    { Provides fast read only access to column array @link(TKCustomGrid.FCols). }
    property ArrayOfCols: TKGridAxisItems read FCols;
    { Provides fast read only access to row array @link(TKCustomGrid.FRows). }
    property ArrayOfRows: TKGridAxisItems read FRows;
  {$IFDEF FPC}
    { Specifies the same as Ctl3D in Delphi. }
    property Flat: Boolean read FFlat write SetFlat default False;
  {$ENDIF}
    { Determines if all cells are selected. }
    property AllCellsSelected: Boolean read GetAllCellsSelected;
    { Determines if all columns are selected. }
    property AllRowsSelected: Boolean read GetAllRowsSelected;
    { Determines if all columns are selected. }
    property AllColsSelected: Boolean read GetAllColsSelected;
    { Inherited property - see Delphi help. }
    property Canvas;
    { Gains access to the cell instances. New cell instances are always created
      on demand by utilizing @link(TKCustomGrid.CellClass). To replace all other
      cell instances with CellClass, call @link(TKCustomGrid.RealizeCellClass). }
    property Cell[ACol, ARow: Integer]: TKGridCell read GetCell write SetCell;
    { Cell class used to create new cell instances. Cell instances are always
      created on demand. }
    property CellClass: TKGridCellClass read FCellClass write FCellClass;
    { Gains access to the active cell painter. }
    property CellPainter: TKGridCellPainter read FCellPainter;
    { Specifies the cell painter class used to create new @link(TKCustomGrid.CellPainter).
      The new cell painter instance will be created immediately. }
    property CellPainterClass: TKGridCellPainterClass read FCellPainterClass write SetCellPainterClass;
    { Gains simplified access to the probably most used property of an textual
      cell instance. If the cell instance at the position specified by ACol and ARow
      does not inherit from a textual cell class @link(TKGridTextCell), it will be
      created for this cell regardless of the current CellClass assignment. }
    property Cells[ACol, ARow: Integer]: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF} read GetCells write SetCells;
    { Specifies the column span and row span for given cell. Always specify positive
      values. Reading this property may return zero or negative values, which
      are used internally to find base cell of the respective merged area. }
    property CellSpan[ACol, ARow: Integer]: TKGridCellSpan read GetCellSpan write SetCellSpan;
    { Gains access to selection base cell. Setting Col discards the current selection
      and moves focus to a new base cell in the current row that is in the new column.
      The first column has an index of 0, the second column an index of 1, and so on.
      If the index denotes a column that is not selectable, nothing happens. }
    property Col: Integer read FSelection.Col1 write SetCol;
    { Column class used to create new column instances. Column instances are always
      created when @link(TKCustomGrid.ColCount) grows. }
    property ColClass: TKGridColClass read FColClass write FColClass;
    { Specifies the number of columns in the grid. Set ColCount to add or delete
      columns at the righthand side of the grid. The value of ColCount includes
      any fixed columns at the left of the grid as well as the scrollable columns
      in the body of the grid. }
    property ColCount: Integer read FColCount write SetColCount default cColCountDef;
    { Inherited property - see Delphi help. Specifies the default background color
      for client area erasing and for parts of client area not occupied by cells. }
    property Color default clWindow;
    { Specifies all colors used by TKCustomGrid's default painting. }
    property Colors: TKGridColors read FColors write SetColors;
    { Gains access to the column instances. Column instances are always
      created by utilizing @link(TKCustomGrid.ColClass) when @link(TKCustomGrid.ColCount)
      grows. To replace all other column instances with ColClass, call
      @link(TKCustomGrid.RealizeColClass). }
    property Cols[Index: Integer]: TKGridCol read GetCols;
    { Indicates the width (in pixels) of all the columns in the grid. Set ColWidths
      at runtime to change the width of an individual column. If the width of
      a column has not been set explicitly by resizing with the mouse, or by using
      the ColWidths property, its width is @link(TKCustomGrid.DefaultColWidth). }
    property ColWidths[Index: Integer]: Integer read GetColWidths write SetColWidths;
    { Determines the width (in pixels) of all columns that have not been explicitly
      resized. Set DefaultColWidth to change the size of all columns in the grid.
      When DefaultColWidth is set, columns that have been resized using the mouse
      or by setting the @link(TKCustomGrid.ColWidths) property are given the DefaultColWidth
      as well. When new columns are added to the grid, they are created with
      a width of DefaultColWidth. }
    property DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default cDefaultColWidthDef;
    { Dummy property - introduced for backward compatibility with TCustomGrid. }
    property DefaultDrawing: Boolean read GetDefaultDrawing write SetDefaultDrawing default False;
    { Determines the height (in pixels) of all rows that have not been explicitly
      resized. Set DefaultRowHeight to change the size of all rows in the grid.
      When DefaultRowHeight is set, rows that have been resized using the mouse
      or by setting the @link(TKCustomGrid.RowHeights) property are given the DefaultRowHeight
      as well. When new rows are added to the grid, they are created with
      a height of DefaultRowHeight. }
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default cDefaultRowHeightDef;
    { Specifies the style how the control is drawn while not enabled. }
    property DisabledDrawStyle: TKGridDisabledDrawStyle read FDisabledDrawStyle write SetDisabledDrawStyle default cDisabledDrawStyleDef;
    { Specifies how a column or row appears while being moved by mouse. }
    property DragStyle: TKGridDragStyle read FDragStyle write SetDragStyle default cDragStyleDef;
    { Returns reference to current inplace editor instance. }
    property Editor: TWinControl read FEditor;
    { Determines if inplace editor is active. Set EditorMode to true, at runtime,
      to put the grid in edit mode. When EditorMode is true, the user can edit cells
      in the grid. When the user presses F2, EditorMode is set to true. When the
      user presses Enter, the value of EditorMode is toggled or, depending on
      @link(goEnterMoves) and @link(TKCustomGrid.MoveDirection) configuration,
      another cell is focused. Inplace editor can be activated only if goEditing
      is included in @link(TKCustomGrid.Options). }
    property EditorMode: Boolean read GetEditorMode write SetEditorMode;
    { Determines if current inplace editor should be treated as a transparent
      control from the grid's point of view. If a transparent inplace editor
      needs to be painted, the cell background is painted
      first to the inplace editor's Canvas/device context. Typically, check boxes
      or radio buttons should appear as transparent controls in TKCustomGrid.
      Unfortunatelly we must use a custom decision mechanism as there is no standard
      VCL/LCL-based mechanism to design a control fully transparent in all cases.
      The algorithm used to paint the cell background should work for a wide range
      of controls either with or without OS themes. }
    property EditorTransparency: TKGridEditorTransparency read FEditorTransparency write SetEditorTransparency default cEditorTransparencyDef;
    { Returns the effective spacing between columns. This is nonzero,
      if goFixedVertLine or goVertLine is included in @link(TKCustomGrid.Options). }
    property EffectiveColSpacing[Index: Integer]: Integer read GetEffectiveColSpacing;
    { Returns the effective spacing between rows. This is nonzero,
      if goFixedHorzLine or goHorzLine is included in @link(TKCustomGrid.Options). }
    property EffectiveRowSpacing[Index: Integer]: Integer read GetEffectiveRowSpacing;
    { Determines if an entire column is selected. }
    property EntireColSelected[Index: Integer]: Boolean read GetEntireColSelected;
    { Determines number of entirely selected columns. }
    property EntireSelectedColCount: Integer read GetEntireSelectedColCount;
    { Determines if an entire row is selected. }
    property EntireRowSelected[Index: Integer]: Boolean read GetEntireRowSelected;
    { Determines number of entirely selected rows. }
    property EntireSelectedRowCount: Integer read GetEntireSelectedRowCount;
    { Specifies the number of columns on the left of the grid that cannot be scrolled.
      Set FixedCols to create or get rid of nonscrolling columns. Nonscrolling
      columns appear at the left of the grid, and are always visible, even when
      the user scrolls the other columns in the grid. Use nonscrolling columns
      for displaying row titles or row numbers, or to implement a scroll lock that
      the user can set. }
    property FixedCols: Integer read FFixedCols write SetFixedCols default cFixedColsDef;
    { Specifies the number of rows on the top of the grid that cannot be scrolled.
      Set FixedRows to create or get rid of nonscrolling rows. Nonscrolling rows
      appear at the top of the grid, and are always visible, even when the user
      scrolls the other rows in the grid. Use nonscrolling rows for displaying
      column titles or column numbers. }
    property FixedRows: Integer read FFixedRows write SetFixedRows default cFixedRowsDef;
    { Specifies the height of the grid in pixels. If GridHeight is less than
      the value of ClientHeight, all of the rows of the grid appear in the control
      with an empty region below the grid. If the underlying grid is too tall
      to appear in the control, GridHeight is the same as ClientHeight,
      and the user must scroll to see the entire contents of the grid. }
    property GridHeight: Integer read GetGridHeight;
    { Specifies the width (in pixels) of the lines that separate the cells of the grid. }
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default cGridLineWidthDef;
    { Specifies the width of the grid in pixels. If GridWidth is less than the value
      of ClientWidth, all of the columns of the grid appear in the control with
      an empty region to the right of the grid. If the underlying grid is
      too wide to appear in the control, GridWidth is the same as ClientWidth,
      and the user must scroll to see the entire contents of the grid. }
    property GridWidth: Integer read GetGridWidth;
    { Can be used only in OnEditorKeyPreview event to determine the source of this event.
      Introduced here as property because of backward compatibility. }
    property KeyPreview: Boolean read FKeyPreview;
    { Returns the last (even partially) visible column in the grid. }
    property LastVisibleCol: Integer read GetLastVisibleCol;
    { Returns the last (even partially) visible row in the grid. }
    property LastVisibleRow: Integer read GetLastVisibleRow;
    { Specifies the index of the first visible scrollable column in the grid.
      Set LeftCol to scroll the columns in the grid so that the column with index
      LeftCol is the first column after the fixed columns. }
    property LeftCol: Integer read FTopLeft.Col write SetLeftCol;
    { Specifies the number of columns the grid would have if no columns would
      have been deleted. }
    property MaxCol: Integer read FMaxCol;
    { Specifies the number of rows the grid would have if no rows would
      have been deleted. }
    property MaxRow: Integer read FMaxRow;
    { Specifies the minimum width a column can have. }
    property MinColWidth: Integer read FMinColWidth write SetMinColWidth default cMinColWidthDef;
    { Specifies the minimum height a row can have. }
    property MinRowHeight: Integer read FMinRowHeight write SetMinRowHeight default cMinRowHeightDef;
    { Determines if more cells are selected (more than one cell). }
    property MoreCellsSelected: Boolean read GetMoreCellsSelected;
    { Specifies how fast the mouse cell hint should be. }
    property MouseCellHintTime: Cardinal read FMouseCellHintTime write SetMouseCellHintTime default cMouseCellHintTimeDef;
    { Specifies the behavior after the user presses Enter. This property has
      no effect unless goEnterMoves is included in @link(TKCustomGrid.Options). }
    property MoveDirection: TKGridMoveDirection read FMoveDirection write FMoveDirection default cMoveDirectionDef;
    { Lists the objects for each cell in the grid. Setting Objects forces a descendant
      of @link(TKGridObjectCell) to be created for the related cell. If @link(TKCustomGrid.CellClass)
      contains such a descendant, then it will be used instead of TKGridObjectCell.
      TObject instance given to Objects will be then stored in @link(TKGridObjectCell.CellObject)
      property. In contrast to TStringGrid, the passed TObject is owned by the
      TKGridObjectCell instance. Override TKGridObjectCell to implement another
      behavior. }
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    { Specifies basic display and behavioral properties of the grid. }
    property Options: TKGridOptions read FOptions write SetOptions default cOptionsDef;
    { Specifies extended display and behavioral properties of the grid. }
    property OptionsEx: TKGridOptionsEx read FOptionsEx write SetOptionsEx default cOptionsExDef;
    { Inherited property - see Delphi help. }
    property ParentColor default False;
    { Specifies the style how multiple cells are selected. }
    property RangeSelectStyle: TKGridRangeSelectStyle read FRangeSelectStyle write FRangeSelectStyle default cRangeSelectStyleDef;
    { Gains access to selection base cell. Setting Row discards the current selection
      and moves focus to a new base cell in the current column that is in the new row.
      The first row has an index of 0, the second row an index of 1, and so on.
      If the index denotes a row that is not selectable, nothing happens. }
    property Row: Integer read FSelection.Row1 write SetRow;
    { Row class used to create new row instances. Row instances are always
      created when @link(TKCustomGrid.RowCount) grows. }
    property RowClass: TKGridRowClass read FRowClass write FRowClass;
    { Specifies the number of rows in the grid. Set RowCount to add or delete rows
      at the bottom of the grid. The value of RowCount includes any fixed rows at
      the top of the grid as well as the scrollable rows in the body of the grid. }
    property RowCount: Integer read FRowCount write SetRowCount default cRowCountDef;
    { Indicates the height (in pixels) of all the rows in the grid. Set RowHeights
      at runtime to change the height of an individual row. If the height of
      a row has not been set explicitly by resizing with the mouse, or by using
      the RowHeights property, its height is @link(TKCustomGrid.DefaultRowHeight). }
    property RowHeights[Index: Integer]: Integer read GetRowHeights write SetRowHeights;
    { Gains access to the row instances. Row instances are always
      created by utilizing @link(TKCustomGrid.RowClass) when @link(TKCustomGrid.RowCount)
      grows. To replace all other row instances with ColClass, call
      @link(TKCustomGrid.RealizeRowClass). }
    property Rows[Index: Integer]: TKGridRow read GetRows;
    { Specifies whether the grid includes horizontal and vertical scroll bars.
      If all the cells in the grid fit in the ClientWidth, no horizontal scroll bar
      appears, even if ScrollBars is ssHorizontal or ssBoth. If all the cells fit
      in the ClientHeight, no vertical scroll bar appears, even if ScrollBars is
      ssVertical or ssBoth. }
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default cScrollBarsDef;
    { Specifies how horizontal scrollbar's trackbar scrolls the grid. }
    property ScrollModeHorz: TKGridScrollMode read FScrollModeHorz write SetScrollModeHorz default cScrollModeDef;
    { Specifies how vertical scrollbar's trackbar scrolls the grid. }
    property ScrollModeVert: TKGridScrollMode read FScrollModeVert write SetScrollModeVert default cScrollModeDef;
    { Specifies how fast the scrolling by timer should be. }
    property ScrollSpeed: Cardinal read FScrollSpeed write SetScrollSpeed default cScrollSpeedDef;
    { Indicates the boundaries of the current selection. Set Selection to select
      a range of cells in the grid. In the TKGridRect structure, the Cell1 parameter
      always denotes base selection cell and Cell2 expanded selection cell.
      A base cell is always the cell that has input focus and can be currently
      edited. An expanded cell denotes the other selection corner. }
    property Selection: TKGridRect read GetSelection write SetSelection;
    { Returns the current number of selections. Returns always a value greater or equal to 1. }
    property SelectionCount: Integer read GetSelectionCount;
    { Returns the selection rectangle. }
    property SelectionRect: TRect read GetSelectionRect;
    { Gains access to all currently existing selections. This property cannot be used
      to add new selection. Please use @link(TKCustomGrid.SelectionAdd) instead. }
    property Selections[Index: Integer]: TKGridRect read GetSelections write SetSelections;
    { Specifies how a column or row appears while being resized by mouse. }
    property SizingStyle: TKGridSizingStyle read FSizingStyle write SetSizingStyle default cSizingStyleDef;
    { Returns index of the column having its SortMode property smDown or smUp.
      There must be always one such column in the grid. }
    property SortCol: Integer read GetSortCol;
    { Returns index of the row having its SortMode property smDown or smUp.
      There must be always one such row in the grid. }
    property SortRow: Integer read GetSortRow;
    { Specifies how sorting is performed when user clicks on clickable fixed cells
      that normally indicate sorting by an arrow. }
    property SortStyle: TKGridSortStyle read FSortStyle write FSortStyle default cSortStyleDef;
    { Indicates whether the user can tab to specified columns in the grid if
      goTabs is included in @link(TKCustomGrid.Options). Set TabStops to False
      to remove the column identified by Index from the tab order. The first column
      in the grid is identified by an Index of 0. Setting TabStops for fixed
      columns has no effect. }
    property TabStops[Index: Integer]: Boolean read GetTabStops write SetTabStops;
    { Determines if cells can be painted with OS themes at the moment. Returns
      True if OS themes are available and both goThemes and goThemedCells are
      included in @link(TKCustomGrid.Options). }
    property ThemedCells: Boolean read GetThemedCells;
    { Determines if OS themes are available to the grid. }
    property Themes: Boolean read GetThemes;
    { If the SelectedByMouse parameter in @link(TKCustomGrid.OnEditorSelect) is True
      you can set ThroughClick to True to click the inplace editor within the same
      mouse click that selected this cell. }
    property ThroughClick: Boolean read FThroughClick write FThroughClick;
    { Specifies the index of the first visible scrollable row in the grid.
      Set TopRow to scroll the rows in the grid so that the row with index
      TopRow is the first row after the fixed rows. }
    property TopRow: Integer read FTopLeft.Row write SetTopRow;
    { Use VisibleColCount to determine the number of scrollable columns fully visible in the grid.
      VisibleColCount does not include the fixed columns counted by the FixedCols property.
      It does not include any partially visible columns on the right edge of the grid. }
    property VisibleColCount: Integer read GetVisibleColCount;
    { Indicates the area of scrollable cells visible in the grid. VisibleGridRect does not
      include any fixed cells or partially visible cells on the right or bottom side of the grid. }
    property VisibleGridRect: TKGridRect read GetVisibleGridRect;
    { Use VisibleRowCount to determine the number of scrollable rows fully visible in the grid.
      VisibleRowCount does not include the fixed rows counted by the FixedRows property.
      It does not include any partially visible rows on the bottom of the grid. }
    property VisibleRowCount: Integer read GetVisibleRowCount;
    { OnBeginColDrag is called when the user clicks on a column to start dragging.
      It enables the grid to control whether the column can be repositioned and
      if so, which column. Origin is the index of the column to be dragged.
      When OnBeginColDrag occurs, this is the index of the column in which the mouse
      was clicked. You can change this value for application specific behavior. }
    property OnBeginColDrag: TKGridBeginDragEvent read FOnBeginColDrag write FOnBeginColDrag;
    { OnBeginColSizing is called when the user clicks between two columns to start
      resizing. It enables the grid to control whether the column can be resized and
      if so, which column. Index is the index of the column to be resized. Pos is
      the X-coordinate of the sizing line. These values correspond with the default
      processing initiated by mouse click. You can change both of these values
      for application specific behavior. }
    property OnBeginColSizing: TKGridBeginSizingEvent read FOnBeginColSizing write FOnBeginColSizing;
    { OnBeginRowDrag is called when the user clicks on a row to start dragging.
      It enables the grid to control whether the row can be repositioned and
      if so, which row. Origin is the index of the row to be dragged.
      When OnBeginRowDrag occurs, this is the index of the row in which the mouse
      was clicked. You can change this value for application specific behavior. }
    property OnBeginRowDrag: TKGridBeginDragEvent read FOnBeginRowDrag write FOnBeginRowDrag;
    { OnBeginRowSizing is called when the user clicks between two rows to start
      resizing. It enables the grid to control whether the row can be resized and
      if so, which row. Index is the index of the row to be resized. Pos is
      the Y-coordinate of the sizing line. These values correspond with the default
      processing initiated by mouse click. You can change both of these values
      for application specific behavior. }
    property OnBeginRowSizing: TKGridBeginSizingEvent read FOnBeginRowSizing write FOnBeginRowSizing;
    { OnCellChanging is called whenever the grid evaluates a condition where cell needs to be changed. }
    property OnCellChanging: TKGridCellChangedEvent read FOnCellChanging write FOnCellChanging;
    { OnCellSpan is called whenever the grid needs to get information about column
      or row span of current cell. Use this only in virtual grid mode and do not write
      complex code here as this event is called really VERY frequently. You must provide
      the same cell span information as the TKGridCell.@link(TKGridCell.Span) for
      non-virtual mode. }
    property OnCellSpan: TKGridCellSpanEvent read FOnCellSpan write FOnCellSpan;
    { OnChanged is called after @link(TKCustomGrid.OnEditorDataToGrid) only if
      @link(TKCustomGrid.OnCompareCellInstances) returns different cells. Its purpose
      is to notify the application about any changes the user made via inplace editors.
      Does not work in virtual mode (if goVirtualGrid is included in @link(TKCustomGrid.Options)). }
    property OnChanged: TKGridCellEvent read FOnChanged write FOnChanged;
    { OnCheckColDrag validates whether the column currently selected for dragging
      can be dropped at the current location. Origin is the index of the column being
      actually dragged. Destination represents the potential drop target. You can
      modify Destination or set CanDrop to False for application specific behavior. }
    property OnCheckColDrag: TKGridCheckDragEvent read FOnCheckColDrag write FOnCheckColDrag;
    { OnCheckRowDrag validates whether the row currently selected for dragging
      can be dropped at the current location. Origin is the index of the row being
      actually dragged. Destination represents the potential drop target. You can
      modify Destination or set CanDrop to False for application specific behavior. }
    property OnCheckRowDrag: TKGridCheckDragEvent read FOnCheckRowDrag write FOnCheckRowDrag;
    { OnColumnMoved is called after a column has been physically moved. }
    property OnColumnMoved: TKGridMovedEvent read FOnColMoved write FOnColMoved;
    { OnColWidthsChanged is called whenever the width of a single or more columns changes. }
    property OnColWidthsChanged: TNotifyEvent read FOnColWidthsChanged write FOnColWidthsChanged;
    { OnColWidthsChangedEx is called whenever the width of a single or more columns changes.
      AIndex corresponds to the first column whose width has been modified. }
    property OnColWidthsChangedEx: TKGridExtentEvent read FOnColWidthsChangedEx write FOnColWidthsChangedEx;
    { OnCompareCellInstances is currently called only if the grid needs to decide
      whether to call the @link(TKCustomGrid.OnChanged) event handler. This event
      is not called in virtual mode (if goVirtualGrid is included in @link(TKCustomGrid.Options)). }
    property OnCompareCellInstances: TKGridCompareCellInstancesEvent read FOnCompareCellInstances write FOnCompareCellInstances;
    { OnCompareCells is called whenever the grid needs to compare contents of two
      cells. This occurs if the @link(TKCustomGrid.SortCols), @link(TKCustomGrid.SortRows),
      @link(TKCustomGrid.InsertSortedCol) and @link(TKCustomGrid.InsertSortedRow)
      methods are called, either programmatically or by mouse click on the first
      fixed column or row. Do not write complex code here as this event is called
      VERY frequently. To speed up sorting, use properties introduced for fast
      data access, such as @link(TKCustomGrid.ArrayOfCells) or
      @link(TKGridTextCell.TextPtr). }
    property OnCompareCells: TKGridCompareCellsEvent read FOnCompareCells write FOnCompareCells;
    { OnCustomSortCols is called whenever the grid needs to sort columns. Use this
      event to override the default sorting algorithm. }
    property OnCustomSortCols: TKGridCustomSortEvent read FOnCustomSortCols write FOnCustomSortCols;
    { OnCustomSortRows is called whenever the grid needs to sort rows. Use this
      event to override the default sorting algorithm. }
    property OnCustomSortRows: TKGridCustomSortEvent read FOnCustomSortRows write FOnCustomSortRows;
    { OnDrawCell is called whenever a cell in the grid needs to be drawn. Draw
      on the cell using the methods of the Canvas property. If the OnDrawCell event
      handler is not assigned, all cells in grid will be painted with the cell
      class aware @link(TKGridCell.DrawCell) method. If the cell has no assigned
      cell instance, it appears empty. }
    property OnDrawCell: TKGridDrawCellEvent read FOnDrawCell write FOnDrawCell;
    { OnEditorCreate is called whenever a cell is about to be edited. This event
      handler allows you to create a custom inplace editor for each cell. The editor
      should only be created, such as by means of AEditor := TEdit.Create(nil).
      Correct positioning within the grid, focusing, painting etc. is maintained
      later by grid itself. No manipulation requiring the editor's Handle is allowed here. }
    property OnEditorCreate: TKGridEditorCreateEvent read FOnEditorCreate write FOnEditorCreate;
    { OnEditorDataFromGrid is called after @link(TKCustomGrid.OnEditorCreate).
      The inplace editor is correctly positioned, has a parent control, its Handle
      is allocated but is still not visible. Set data from the grid to the inplace
      editor in an user defined way here. Data can be set in EditorCreate but some
      assignments need that the inplace editor has a parent control. Grid is always
      parent control of inplace editor. }
    property OnEditorDataFromGrid: TKGridEditorDataEvent read FOnEditorDataFromGrid write FOnEditorDataFromGrid;
    { OnEditorDataToGrid is called if the inplace editor is about to disappear.
      The inplace editor is still visible here and has a parent control. Its Handle
      is still allocated. Set data from the inplace editor to the grid in an user
      defined way here. Data can be transferred in @link(TKCustomGrid.EditorDestroy)
      but some assignments need that the inplace editor has a parent control. }
    property OnEditorDataToGrid: TKGridEditorDataEvent read FOnEditorDataToGrid write FOnEditorDataToGrid;
    { OnEditorDestroy is called after @link(TKCustomGrid.OnEditorDataToGrid),
      just before the inplace editor is destroyed. It is no longer visible here
      and has no parent control. Its Handle is no more valid. Perform application
      specific operations just before the editor is destroyed here. You need not
      destroy the AEditor instance, but if so, set AEditor to nil after destroying it
      <i>Example:</i> FreeAndNil(AEditor). }
    property OnEditorDestroy: TKGridEditorDestroyEvent read FOnEditorDestroy write FOnEditorDestroy;
    { OnEditorKeyPreview is called whenever inplace editor is focused and the user
      presses some key that is normally handled by the grid if no inplace editor
      is visible. Sometimes this key needs to be handled by the grid instead of
      the inplace editor. For example, the @link(EditKeyPreview) function decides
      whether the key needs to be handled by the grid or by the inplace editor.
      Write your own code that is specific for your custom inplace editors. }
    property OnEditorKeyPreview: TKGridEditorKeyPreviewEvent read FOnEditorKeyPreview write FOnEditorKeyPreview;
    { OnEditorResize is called whenever the grid needs to relocate the inplace
      editor (this might be quite often). By default, each inplace editor is always
      located so that its bounding rectangle equals to the cell rectangle.
      Write your own code to change this behavior. Inplace editors cannot appear
      outside the edited cell, clipping is always present. <i>Note:</i> Not every
      TWinControl instance intended as inplace editor can be arbitrary resized. }
    property OnEditorResize: TKGridEditorResizeEvent read FOnEditorResize write FOnEditorResize;
    { OnEditorSelect is called immediately after @link(TKCustomGrid.OnEditorDataFromGrid).
      This event handler allows you to correctly set the caret position within
      your inplace editor. }
    property OnEditorSelect: TKGridEditorSelectEvent read FOnEditorSelect write FOnEditorSelect;
    { Determines whether a particular column can be dropped immediately after
      the user releases the mouse button but before the column is actually moved. }
    property OnEndColDrag: TKGridEndDragEvent read FOnEndColDrag write FOnEndColDrag;
    { Determines whether a particular column can be resized immediately after
      the user releases the mouse button but before the column is actually resized.
      This event handler has no effect if @link(TKCustomGrid.SizingStyle) is ssUpdate. }
    property OnEndColSizing: TKGridEndSizingEvent read FOnEndColSizing write FOnEndColSizing;
    { Determines whether a particular row can be dropped immediately after
      the user releases the mouse button but before the row is actually moved. }
    property OnEndRowDrag: TKGridEndDragEvent read FOnEndRowDrag write FOnEndRowDrag;
    { Determines whether a particular row can be resized immediately after
      the user releases the mouse button but before the row is actually resized.
      This event handler has no effect if @link(TKCustomGrid.SizingStyle) is ssUpdate. }
    property OnEndRowSizing: TKGridEndSizingEvent read FOnEndRowSizing write FOnEndRowSizing;
    { OnExchangeCols is called whenever the grid sorts columns or needs to
      exchange two columns. Typically you assign this event handler in virtual
      grid mode @link(goVirtualGrid) to physically sort your data or when
      implementing a custom behavior parallel to sorting cell instances owned
      by the grid. This event is called from @link(TKCustomGrid.MoveCol), either. }
    property OnExchangeCols: TKGridExchangeEvent read FOnExchangeCols write FOnExchangeCols;
    { OnExchangeRows is called whenever the grid sorts rows or needs to
      exchange two rows. Typically you assign this event handler in virtual
      grid mode @link(goVirtualGrid) to physically sort your data or when
      implementing a custom behavior parallel to sorting cell instances owned
      by the grid. This event is called from @link(TKCustomGrid.MoveRow), either. }
    property OnExchangeRows: TKGridExchangeEvent read FOnExchangeRows write FOnExchangeRows;
    { OnMeasureCell is called whenever the grid needs to get the horizontal and vertical extent
      of the data displayed in a cell. If the OnMeasureCell event
      handler is not assigned, all cells in the grid will be measured by default. }
    property OnMeasureCell: TKGridMeasureCellEvent read FOnMeasureCell write FOnMeasureCell;
    { OnMouseCellHint is called whenever a cell is clicked by left mouse button. }
    property OnMouseCellHint: TKGridCellHintEvent read FOnMouseCellHint write FOnMouseCellHint;
    { OnMouseClickCell is called whenever a cell is clicked by left mouse button. }
    property OnMouseClickCell: TKGridCellEvent read FOnMouseClickCell write FOnMouseClickCell;
    { OnMouseDblClickCell is called whenever a cell is clicked by left mouse button. }
    property OnMouseDblClickCell: TKGridCellEvent read FOnMouseDblClickCell write FOnMouseDblClickCell;
    { OnMouseEnterCell is called whenever mouse enters a cell. }
    property OnMouseEnterCell: TKGridCellEvent read FOnMouseEnterCell write FOnMouseEnterCell;
    { OnMouseLeaveCell is called whenever mouse leaves a cell. }
    property OnMouseLeaveCell: TKGridCellEvent read FOnMouseLeaveCell write FOnMouseLeaveCell;
    { OnRowHeightsChanged is called whenever the height of a single or more rows changes. }
    property OnRowHeightsChanged: TNotifyEvent read FOnRowHeightsChanged write FOnRowHeightsChanged;
    { OnRowHeightsChangedEx is called whenever the height of a single or more rows changes.
      AIndex corresponds to the first row whose height has been modified. }
    property OnRowHeightsChangedEx: TKGridExtentEvent read FOnRowHeightsChangedEx write FOnRowHeightsChangedEx;
    { OnRowMoved is called after a row has been physically moved. }
    property OnRowMoved: TKGridMovedEvent read FOnRowMoved write FOnRowMoved;
    { OnSelectCell is called whenever a cell is about to be selected. A cell can
      be selected either by mouse or keyboard, or programmatically e.g. by the
      @link(TKCustomGrid.FocusCell) method. CanSelect is True by default to allow all
      selectable cells to be selected. Change this parameter to False to disallow
      cell selection. A cell that cannot be selected, cannot be edited as well.
      Many times you need some cells not to become editable. In this case,
      let @link(TKCustomGrid.OnEditorCreate) decide it rather than OnSelectCell. }
    property OnSelectCell: TKGridSelectCellEvent read FOnSelectCell write FOnSelectCell;
    { OnSelectionExpand is called if the user expands the current selection.
      The selection can be expanded either by mouse or keyboard, or programmatically
      e.g. by the @link(TKCustomGrid.Selection) property. CanExpand is True by default
      to allow all cells to become a target of selection expansion. Change this
      parameter to False to disallow selection expansion.}
    property OnSelectionExpand: TKGridSelectionExpandEvent read FOnSelectionExpand write FOnSelectionExpand;
    { OnSizeChanged is called whenever the @link(TKCustomGrid.ColCount) or
      @link(TKCustomGrid.RowCount) properties change. }
    property OnSizeChanged: TKGridSizeChangedEvent read FOnSizeChanged write FOnSizeChanged;
    { OnTopLeftChanged is called whenever the @link(TKCustomGrid.LeftCol) or
      @link(TKCustomGrid.TopRow) properties change. }
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
  end;

  { @abstract(KGrid design-time component) This is the class you use both
    on run-time and design-time. }
  TKGrid = class(TKCustomGrid)
  published
    { Inherited property - see Delphi help. }
    property Align;
    { Inherited property - see Delphi help. }
    property Anchors;
    { See TKCustomControl.@link(TKCustomControl.BorderStyle) for details. }
    property BorderStyle;
    { Inherited property - see Delphi help. }
    property BorderWidth;
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
    { See TKCustomGrid.@link(TKCustomGrid.OptionsEx) for details. }
    property OptionsEx;
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
    { See TKCustomGrid.@link(TKCustomGrid.OnBeginRowDrag) for details. }
    property OnBeginRowDrag;
    { See TKCustomGrid.@link(TKCustomGrid.OnBeginRowSizing) for details. }
    property OnBeginRowSizing;
    { See TKCustomGrid.@link(TKCustomGrid.OnCellChanging) for details. }
    property OnCellChanging;
    { See TKCustomGrid.@link(TKCustomGrid.OnCellSpan) for details. }
    property OnCellSpan;
    { See TKCustomGrid.@link(TKCustomGrid.OnChanged) for details. }
    property OnChanged;
    { See TKCustomGrid.@link(TKCustomGrid.OnCheckColDrag) for details. }
    property OnCheckColDrag;
    { See TKCustomGrid.@link(TKCustomGrid.OnCheckRowDrag) for details. }
    property OnCheckRowDrag;
    { Inherited property - see Delphi help. }
    property OnClick;
    { See TKCustomGrid.@link(TKCustomGrid.OnColumnMoved) for details. }
    property OnColumnMoved;
    { See TKCustomGrid.@link(TKCustomGrid.OnColWidthsChanged) for details. }
    property OnColWidthsChanged;
    { See TKCustomGrid.@link(TKCustomGrid.OnColWidthsChangedEx) for details. }
    property OnColWidthsChangedEx;
    { See TKCustomGrid.@link(TKCustomGrid.OnCompareCellInstances) for details. }
    property OnCompareCellInstances;
    { See TKCustomGrid.@link(TKCustomGrid.OnCompareCells) for details. }
    property OnCompareCells;
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
    { See TKCustomGrid.@link(TKCustomGrid.OnEndRowDrag) for details. }
    property OnEndRowDrag;
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
    { See TKCustomGrid.@link(TKCustomGrid.OnMeasureCell) for details. }
    property OnMeasureCell;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseCellHint) for details. }
    property OnMouseCellHint;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseClickCell) for details. }
    property OnMouseClickCell;
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseDblClickCell) for details. }
    property OnMouseDblClickCell;
    { Inherited property - see Delphi help. }
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    { Inherited property - see Delphi help. }
    property OnMouseEnter;
  {$ENDIF}
    { See TKCustomGrid.@link(TKCustomGrid.OnMouseEnterCell) for details. }
    property OnMouseEnterCell;
  {$IFDEF COMPILER9_UP}
    { Inherited property - see Delphi help. }
    property OnMouseLeave;
  {$ENDIF}
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
    { This event is called at certain phases of the actually running print job. }
    property OnPrintNotify;
    { This event is called after the shape is drawn onto the printer canvas. }
    property OnPrintPaint;
    { Inherited property - see Delphi help. }
    property OnResize;
    { See TKCustomGrid.@link(TKCustomGrid.OnRowHeightsChanged) for details. }
    property OnRowHeightsChanged;
    { See TKCustomGrid.@link(TKCustomGrid.OnRowHeightsChangedEx) for details. }
    property OnRowHeightsChangedEx;
    { See TKCustomGrid.@link(TKCustomGrid.OnRowMoved) for details. }
    property OnRowMoved;
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

{ Determines if the Cell specified by ACol and ARow lies within grid rectangle R. }
function CellInGridRect(ACol, ARow: Integer; const R: TKGridRect): Boolean;

{ Determines if the grid rectangle contains a subset of cells belonging to the
  column specified by ACol. }
function ColInGridRect(ACol: Integer; const R: TKGridRect): Boolean;

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultComboKeyPreview) instead. }
procedure ComboKeyPreview(AGrid: TKCustomGrid; AEditor: TComboBox;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultComboSelect) instead. }
procedure ComboSelect(AGrid: TKCustomGrid; AEditor: TComboBox; SelectAll,
  CaretToLeft: Boolean);

{ Compares two TKGridAxisItems arrays. The function returns True if the arrays are
  equal in length and all corresponding TKGridAxisItem instances within both arrays
  have equal property values. }
function CompareAxisItems(AxisItems1, AxisItems2: TKGridAxisItems): Boolean;

{ Obsolete function. Implements default painting for TKCustomGrid cells.
  Call TKCustomGrid.CellPainter.@link(TKGridCellPainter.DefaultDraw) instead. }
procedure DefaultDrawCell(AGrid: TKCustomGrid; ACol, ARow: Integer; ARect: TRect;
  AState: TKGridDrawState; HAlign: TKHAlign; VAlign: TKVAlign;
  HPadding, VPadding: Integer; const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultEditorKeyPreview) instead. }
procedure DefaultKeyPreview(AGrid: TKCustomGrid; AEditor: TWinControl;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultEditorSelect) instead. }
procedure DefaultSelect(AGrid: TKCustomGrid; AEditor: TWinControl; ACol, ARow: Integer;
  SelectAll, CaretToLeft, SelectedByMouse: Boolean);

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultEditKeyPreview) instead. }
procedure EditKeyPreview(AGrid: TKCustomGrid; AEditor: TCustomEdit;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultEditSelect) instead. }
procedure EditSelect(AGrid: TKCustomGrid; AEditor: TCustomEdit; SelectAll,
  CaretToLeft: Boolean);

{ Makes a @link(TKGridCoord) record from ACol and ARow. }
function GridPoint(ACol, ARow: Integer): TKGridCoord;

{ Makes a @link(TKGridRect) record from ACell. Cell will be copied both to Cell1 and
  Cell2 fields of the resulting grid rectangle. }
function GridRect(ACell: TKGridCoord): TKGridRect; overload;

{ Makes a @link(TKGridRect) record from ACell1 and ACell2. All the input parameters
  will be copied to the corresponding fields of the resulting grid rectangle. }
function GridRect(ACell1, ACell2: TKGridCoord): TKGridRect; overload;

{ Makes a @link(TKGridRect) record from ACol1, ARow1, ACol2 and ARow2. All the input
  parameters will be copied to the corresponding fields of the resulting grid rectangle. }
function GridRect(ACol1, ARow1, ACol2, ARow2: Integer): TKGridRect; overload;

{ Compares two grid rectangles. Returns True if all the corresponding fields
  in GridRect1 equal those in GridRect2. }
function GridRectEqual(const GridRect1, GridRect2: TKGridRect): Boolean;

{ Makes a @link(TKGridCellSpan) record from AColumns and ARows. }
function MakeCellSpan(AColumns, ARows: Integer): TKGridCellSpan;

{ Makes Cell1 field of GridRect always top-left cell and Cell2 field always
  bottom-right cell. }
procedure NormalizeGridRect(var GridRect: TKGridRect);

{ Determines if the grid rectangle contains a subset of cells belonging to the
  row specified by ARow. }
function RowInGridRect(ARow: Integer; const R: TKGridRect): Boolean;

{ Obsolete function. Call TKCustomGrid.@link(TKCustomGrid.DefaultScrollBarKeyPreview) instead. }
procedure ScrollBarKeyPreview(AGrid: TKCustomGrid; AEditor: TScrollBar;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);

implementation

uses
  Math, TypInfo
{$IFDEF USE_THEMES}
  , Themes
 {$IFNDEF FPC}
  , UxTheme
 {$ENDIF}
{$ENDIF}
  ;

function CellInGridRect(ACol, ARow: Integer; const R: TKGridRect): Boolean;
begin
  Result := (
    (R.Col1 <= R.Col2) and (ACol >= R.Col1) and (ACol <= R.Col2) or
    (R.Col1 > R.Col2) and (ACol >= R.Col2) and (ACol <= R.Col1)
    ) and (
    (R.Row1 <= R.Row2) and (ARow >= R.Row1) and (ARow <= R.Row2) or
    (R.Row1 > R.Row2) and (ARow >= R.Row2) and (ARow <= R.Row1)
    )
end;

function ColInGridRect(ACol: Integer; const R: TKGridRect): Boolean;
begin
  Result := (
    (R.Col1 <= R.Col2) and (ACol >= R.Col1) and (ACol <= R.Col2) or
    (R.Col1 > R.Col2) and (ACol >= R.Col2) and (ACol <= R.Col1)
    );
end;

procedure ComboKeyPreview(AGrid: TKCustomGrid; AEditor: TComboBox;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  AGrid.DefaultComboKeyPreview(AEditor, ACol, ARow, Key, ShiftState, IsGridKey);
end;

procedure ComboSelect(AGrid: TKCustomGrid; AEditor: TComboBox; SelectAll,
  CaretToLeft: Boolean);
begin
  AGrid.DefaultComboSelect(AEditor, SelectAll, CaretToLeft);
end;

function CompareAxisItems(AxisItems1, AxisItems2: TKGridAxisItems): Boolean;
var
  I: Integer;
begin
  Result := Length(AxisItems1) = Length(AxisItems2);
  if Result then
    for I := 0 to Length(AxisItems1) - 1 do
      if not AxisItems1[I].Equals(AxisItems2[I]) then
      begin
        Result := False;
        Exit;
      end;
end;

procedure DefaultDrawCell(AGrid: TKCustomGrid; ACol, ARow: Integer; ARect: TRect;
  AState: TKGridDrawState; HAlign: TKHAlign; VAlign: TKVAlign;
  HPadding, VPadding: Integer; const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
begin
  with AGrid do
  begin
    CellPainter.Initialize;  
    CellPainter.Col := ACol;
    CellPainter.Row := ARow;
    CellPainter.CellRect := ARect;
    CellPainter.State := AState;
    CellPainter.HAlign := HAlign;
    CellPainter.VAlign := VAlign;
    CellPainter.HPadding := HPadding;
    CellPainter.VPadding := VPadding;
    CellPainter.Text := AText;
    CellPainter.DefaultDraw;
  end;
end;

procedure DefaultKeyPreview(AGrid: TKCustomGrid; AEditor: TWinControl;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  AGrid.DefaultEditorKeyPreview(AEditor, ACol, ARow, Key, ShiftState, IsGridKey);
end;

procedure DefaultSelect(AGrid: TKCustomGrid; AEditor: TWinControl; ACol, ARow: Integer;
  SelectAll, CaretToLeft, SelectedByMouse: Boolean);
begin
  AGrid.DefaultEditorSelect(AEditor, ACol, ARow, SelectAll, CaretToLeft, SelectedByMouse);
end;

function DirectionToCommand(Direction: TKGridMoveDirection): TKGridMoveCommand;
begin
  case Direction of
    mdDown: Result := mcDown;
    mdLeft: Result := mcLeft;
    mdRight: Result := mcRight;
  else
    Result := mcUp;
  end;
end;

procedure DoEditKeyPreview(ATextLen, ASelStart, ASelLength, ALineCount: Integer;
  AMultiLine, AStartLine, AEndLine: Boolean; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  if ((Key in [VK_LEFT, VK_HOME]) and ((ASelStart > 0) or (ASelLength > 1))) or // 1 to support TMaskEdit
    ((Key in [VK_RIGHT, VK_END]) and ((ASelStart < ATextLen) or (ASelLength > 0))) or
    ((Key in [VK_PRIOR, VK_UP]) and AMultiLine and (not AStartLine or (ASelLength > 0) and (ASelLength < ATextLen))) or
    ((Key in [VK_NEXT, VK_DOWN]) and AMultiLine and (not AEndLine or (ASelLength > 0) and (ASelLength < ATextLen))) or
    (Key = VK_RETURN) and AMultiLine then
    IsGridKey := False;
end;

procedure EditKeyPreview(AGrid: TKCustomGrid; AEditor: TCustomEdit;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  AGrid.DefaultEditKeyPreview(AEditor, ACol, ARow, Key, ShiftState, IsGridKey);
end;

procedure EditSelect(AGrid: TKCustomGrid; AEditor: TCustomEdit; SelectAll,
  CaretToLeft: Boolean);
begin
  AGrid.DefaultEditSelect(AEditor, SelectAll, CaretToLeft);
end;

function GridRectEqual(const GridRect1, GridRect2: TKGridRect): Boolean;
begin
  Result := CompareMem(@GridRect1, @GridRect2, SizeOf(TKGridRect));
end;

function GridPoint(ACol, ARow: Integer): TKGridCoord;
begin
  with Result do
  begin
    Col := ACol;
    Row := ARow;
  end;
end;

function GridRect(ACell: TKGridCoord): TKGridRect; overload;
begin
  with Result do
  begin
    Col1 := ACell.Col;
    Col2 := ACell.Col;
    Row1 := ACell.Row;
    Row2 := ACell.Row;
  end;
end;

function GridRect(ACell1, ACell2: TKGridCoord): TKGridRect; overload;
begin
  with Result do
  begin
    Cell1 := ACell1;
    Cell2 := ACell2;
  end;
end;

function GridRect(ACol1, ARow1, ACol2, ARow2: Integer): TKGridRect; overload;
begin
  with Result do
  begin
    Col1 := ACol1;
    Col2 := ACol2;
    Row1 := ARow1;
    Row2 := ARow2;
  end;
end;

function MakeCellSpan(AColumns, ARows: Integer): TKGridCellSpan;
begin
  Result.ColSpan := AColumns;
  Result.RowSpan := ARows;
end;

procedure NormalizeGridRect(var GridRect: TKGridRect);
begin
  if GridRect.Col1 > GridRect.Col2 then Exchange(GridRect.Col1, GridRect.Col2);
  if GridRect.Row1 > GridRect.Row2 then Exchange(GridRect.Row1, GridRect.Row2);
end;

function RowInGridRect(ARow: Integer; const R: TKGridRect): Boolean;
begin
  Result := (
    (R.Row1 <= R.Row2) and (ARow >= R.Row1) and (ARow <= R.Row2) or
    (R.Row1 > R.Row2) and (ARow >= R.Row2) and (ARow <= R.Row1)
    );
end;

procedure ScrollBarKeyPreview(AGrid: TKCustomGrid; AEditor: TScrollBar;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  AGrid.DefaultScrollBarKeyPreview(AEditor, ACol, ARow, Key, ShiftState, IsGridKey);
end;

{ TKGridAxisItem }

constructor TKGridAxisItem.Create(AGrid: TKCustomGrid);
begin
  FGrid := AGrid;
  FCanResize := True;
  FExtent := 0;
  FInitialPos := -1;
  FMaxExtent := 0;
  FMinExtent := 0;
  FSortArrowIndex := 0;
end;

procedure TKGridAxisItem.Assign(Source: TKGridAxisItem);
begin
  FCanResize := Source.CanResize;
  FExtent := Source.Extent;
//  FInitialPos := Source.InitialPos;
end;

procedure TKGridAxisItem.BeginDrag(var Origin: Integer;
  const MousePt: TPoint; var CanBeginDrag: Boolean);
begin
end;

procedure TKGridAxisItem.CheckDrag(Origin: Integer; var Destination: Integer;
  const MousePt: TPoint; var CanDrop: Boolean);
begin
end;

procedure TKGridAxisItem.EndDrag(Origin, Destination: Integer;
  const MousePt: TPoint; var CanEndDrag: Boolean);
begin
end;

function TKGridAxisItem.{$ifdef COMPILER12_UP}EqualProperties{$ELSE}Equals{$ENDIF}(Item: TKGridAxisItem): Boolean;
begin
  Result := (Item.Extent = FExtent) and
    (Item.CanResize = FCanResize);
end;

procedure TKGridAxisItem.SetMaxExtent(AValue: Integer);
begin
  if FMinExtent > 0 then
    AValue := Max(AValue, FMinExtent);
  if (AValue >= 0) and (FMaxExtent <> AValue) then
  begin
    FMaxExtent := AValue;
    if (FMaxExtent > 0) and (FExtent > FMaxExtent) then
      Extent := FMaxExtent;
  end;
end;

procedure TKGridAxisItem.SetMinExtent(AValue: Integer);
begin
  if FMaxExtent > 0 then
    AValue := Min(AValue, FMaxExtent);
  if (AValue >= 0) and (FMinExtent <> AValue) then
  begin
    FMinExtent := AValue;
    if (FMinExtent > 0) and Visible and (FExtent < FMinExtent) then
      Extent := FMinExtent;
  end;
end;

function TKGridAxisItem.GetVisible: Boolean;
begin
  Result := FExtent > 0;
end;

{ TKGridCol }

constructor TKGridCol.Create(AGrid: TKCustomGrid);
begin
  inherited;
  FExtent := FGrid.DefaultColWidth;
  FCellHint := False;
  FTabStop := True;
end;

procedure TKGridCol.Assign(Source: TKGridAxisItem);
begin
  inherited;
  if Source is TKGridCol then
  begin
    FCellHint := TKGridCol(Source).CellHint;
    FTabStop := TKGridCol(Source).TabStop;
  end;
end;

procedure TKGridCol.Assign(Source: TStrings);
var
  I, J: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and (Source.Count > 0) and FindCol(I) then
  begin
    FGrid.LockUpdate;
    try
      for J := 0 to Min(FGrid.RowCount, Source.Count) - 1 do
      begin
        Cell := FGrid.ArrayOfCells[J, I];
        if Cell is TKGridTextCell then
          TKGridTextCell(Cell).Text := Source[J];
      end;
    finally
      FGrid.UnlockUpdate;
    end;
  end;
end;

{$IFDEF TKGRID_USE_JCL}
procedure TKGridCol.Assign(Source: TWideStrings);
var
  I, J: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and (Source.Count > 0) and FindCol(I) then
  begin
    FGrid.LockUpdate;
    try
      for J := 0 to Min(FGrid.RowCount, Source.Count) - 1 do
      begin
        Cell := FGrid.ArrayOfCells[J, I];
        if Cell is TKGridTextCell then
          TKGridTextCell(Cell).Text := Source[J];
      end;
    finally
      FGrid.UnlockUpdate;
    end;
  end;
end;
{$ENDIF}

procedure TKGridCol.Clear;
var
  I: Integer;
begin
  if Assigned(FGrid) and FindCol(I) then
    FGrid.ClearCol(I);
end;

function TKGridCol.{$ifdef COMPILER12_UP}EqualProperties{$ELSE}Equals{$ENDIF}(Item: TKGridAxisItem): Boolean;
begin
  Result := inherited Equals(Item) and (Item is TKGridCol) and
    (TKGridCol(Item).TabStop = FTabStop) and
    (TKGridCol(Item).CellHint = FCellHint);
end;

function TKGridCol.FindCol(out Index: Integer): Boolean;
begin
  Result := False;
  Index := 0;
  while Index < FGrid.ColCount do
  begin
    if FGrid.ArrayOfCols[Index] <> Self then
      Inc(Index)
    else
    begin
      Result := True;
      Exit;
    end;  
  end;
end;

function TKGridCol.GetObjects(Index: Integer): TObject;
var
  I: Integer;
  Cell: TKGridCell;
begin
  Result := nil;
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.RowValid(Index) and FindCol(I) then
  begin
    Cell := FGrid.ArrayOfCells[Index, I];
    if Cell is TKGridObjectCell then
      Result := TKGridObjectCell(Cell).CellObject;
  end;
end;

function TKGridCol.GetStrings(Index: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
var
  I: Integer;
  Cell: TKGridCell;
begin
  Result := '';
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.RowValid(Index) and FindCol(I) then
  begin
    Cell := FGrid.ArrayOfCells[Index, I];
    if Cell is TKGridTextCell then
      Result := TKGridTextCell(Cell).Text;
  end;
end;

procedure TKGridCol.SetExtent(const Value: Integer);
var
  I: Integer;
begin
  if (Value >= 0) and (Value <> FExtent) then
  begin
    if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) and FindCol(I) then
      FGrid.ColWidths[I] := Value
    else
    begin
      if FExtent <> 0 then FBackExtent := FExtent;
      FExtent := Value;
    end;
  end;
end;

procedure TKGridCol.SetObjects(Index: Integer; const Value: TObject);
var
  I: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.RowValid(Index) and FindCol(I) then
  begin
    Cell := FGrid.ArrayOfCells[Index, I];
    if Cell is TKGridObjectCell then
      TKGridObjectCell(Cell).CellObject := Value;
  end;
end;

procedure TKGridCol.SetSortArrowIndex(Value: Integer);
var
  I: Integer;
begin
  Value := Max(Value, 0);
  if Value <> FSortArrowIndex then
  begin
    FSortArrowIndex := Value;
    if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) and
      (FSortMode <> smNone) and (FGrid.FixedRows > 1) and FindCol(I) then
      FGrid.InvalidateGridRect(GridRect(I, 0, I, FGrid.FixedRows - 1));
  end;
end;

procedure TKGridCol.SetSortMode(const Value: TKGridSortMode);
var
  I: Integer;
begin
  if (Value <> FSortMode) and FGrid.SortModeUnlocked then
  begin
    if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) and FindCol(I) then
      FGrid.SortRows(I, Value)
    else
      FSortMode := Value;
  end;
end;

procedure TKGridCol.SetStrings(Index: Integer; const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
var
  I: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.RowValid(Index) and FindCol(I) then
  begin
    Cell := FGrid.ArrayOfCells[Index, I];
    if Cell is TKGridTextCell then
      TKGridObjectCell(Cell).Text := Value;
  end;
end;

procedure TKGridCol.SetVisible(Value: Boolean);
begin
  if Value then
  begin
    if FBackExtent <= FGrid.MinColWidth then
      Extent := FGrid.MinColWidth
    else
      Extent := FBackExtent;
  end else
    Extent := 0
end;

{ TKGridRow }

constructor TKGridRow.Create(AGrid: TKCustomGrid);
begin
  inherited;
  FExtent := FGrid.DefaultRowHeight;
end;

procedure TKGridRow.Assign(Source: TStrings);
var
  I, J: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and (Source.Count > 0) and FindRow(I) then
  begin
    FGrid.LockUpdate;
    try
      for J := 0 to Min(FGrid.ColCount, Source.Count) - 1 do
      begin
        Cell := FGrid.ArrayOfCells[I, J];
        if Cell is TKGridTextCell then
          TKGridTextCell(Cell).Text := Source[J];
      end;
    finally
      FGrid.UnlockUpdate;
    end;
  end;
end;

{$IFDEF TKGRID_USE_JCL}
procedure TKGridRow.Assign(Source: TWideStrings);
var
  I, J: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and (Source.Count > 0) and FindRow(I) then
  begin
    FGrid.LockUpdate;
    try
      for J := 0 to Min(FGrid.ColCount, Source.Count) - 1 do
      begin
        Cell := FGrid.ArrayOfCells[I, J];
        if Cell is TKGridTextCell then
          TKGridTextCell(Cell).Text := Source[J];
      end;
    finally
      FGrid.UnlockUpdate;
    end;
  end;
end;
{$ENDIF}

procedure TKGridRow.Clear;
var
  I: Integer;
begin
  for I := 0 to FGrid.RowCount - 1 do
    if FGrid.Rows[I] = Self then
    begin
      FGrid.ClearRow(I);
      Exit;
    end;
end;

function TKGridRow.FindRow(out Index: Integer): Boolean;
begin
  Result := False;
  Index := 0;
  while Index < FGrid.RowCount do
  begin
    if FGrid.ArrayOfRows[Index] <> Self then
      Inc(Index)
    else
    begin
      Result := True;
      Exit;
    end;  
  end;
end;

function TKGridRow.GetObjects(Index: Integer): TObject;
var
  I: Integer;
  Cell: TKGridCell;
begin
  Result := nil;
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.ColValid(Index) and FindRow(I) then
  begin
    Cell := FGrid.ArrayOfCells[I, Index];
    if Cell is TKGridObjectCell then
      Result := TKGridObjectCell(Cell).CellObject;
  end;
end;

function TKGridRow.GetStrings(Index: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
var
  I: Integer;
  Cell: TKGridCell;
begin
  Result := '';
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.ColValid(Index) and FindRow(I) then
  begin
    Cell := FGrid.ArrayOfCells[I, Index];
    if Cell is TKGridTextCell then
      Result := TKGridTextCell(Cell).Text;
  end;
end;

procedure TKGridRow.SetExtent(const Value: Integer);
var
  I: Integer;
begin
  if (Value >= 0) and (Value <> FExtent) then
  begin
    if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) and FindRow(I) then
      FGrid.RowHeights[I] := Value
    else
    begin
      if FExtent <> 0 then FBackExtent := FExtent;
      FExtent := Value;
    end;
  end;
end;

procedure TKGridRow.SetObjects(Index: Integer; const Value: TObject);
var
  I: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.ColValid(Index) and FindRow(I) then
  begin
    Cell := FGrid.ArrayOfCells[I, Index];
    if Cell is TKGridObjectCell then
      TKGridObjectCell(Cell).CellObject := Value;
  end;
end;

procedure TKGridRow.SetSortArrowIndex(Value: Integer);
var
  I: Integer;
begin
  Value := Max(Value, 0);
  if Value <> FSortArrowIndex then
  begin
    FSortArrowIndex := Value;
    if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) and
      (FSortMode <> smNone) and (FGrid.FixedCols > 1) and FindRow(I) then
      FGrid.InvalidateGridRect(GridRect(0, I, FGrid.FixedCols - 1, I));
  end;
end;

procedure TKGridRow.SetSortMode(const Value: TKGridSortMode);
var
  I: Integer;
begin
  if (Value <> FSortMode) and FGrid.SortModeUnlocked then
  begin
    if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) and FindRow(I) then
      FGrid.SortCols(I, Value)
    else
      FSortMode := Value;
  end;
end;

procedure TKGridRow.SetStrings(Index: Integer; const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
var
  I: Integer;
  Cell: TKGridCell;
begin
  if Assigned(FGrid) and Assigned(FGrid.ArrayOfCells) and FGrid.ColValid(Index) and FindRow(I) then
  begin
    Cell := FGrid.ArrayOfCells[I, Index];
    if Cell is TKGridTextCell then
      TKGridTextCell(Cell).Text := Value;
  end;
end;


procedure TKGridRow.SetVisible(Value: Boolean);
begin
  if Value then
  begin
    if FBackExtent <= FGrid.MinRowHeight then
      Extent := FGrid.MinRowHeight
    else
      Extent := FBackExtent;
  end else
    Extent := 0
end;

{ TKGridCell }

constructor TKGridCell.Create(AGrid: TKCustomGrid);
begin
  FGrid := AGrid;
  Initialize;
end;

procedure TKGridCell.Assign(Source: TKGridCell);
begin
  BeforeUpdate;
  FSpan := Source.Span;
  AfterUpdate;
end;

procedure TKGridCell.Clear;
begin
  BeforeUpdate;
  try
    Initialize;
  finally
    AfterUpdate;
  end;
end;

procedure TKGridCell.AfterUpdate;
var
  Cells: TKGridCells;
  Info: TKGridAxisInfoBoth;
  I, J, HExtent, VExtent: Integer;
begin
  if Assigned(FGrid) and FGrid.UpdateUnlocked and not FGrid.Flag(cGF_GridUpdates) then
  begin
    // invalidate cell, iterate only visible cells in a fast way
    Cells := FGrid.ArrayOfCells;
    Info := FGrid.GetAxisInfoBoth([]);
    I := 0; HExtent := 0;
    while (I < Info.Horz.TotalCellCount) and (HExtent < Info.Horz.ClientExtent) do
    begin
      if I = Info.Horz.FixedCellCount then
        I := Info.Horz.FirstGridCell; // switch to first visible nonfixed cell
      J := 0; VExtent := 0;
      while (J < Info.Vert.TotalCellCount) and (VExtent < Info.Vert.ClientExtent) do
      begin
        if J = Info.Vert.FixedCellCount then
          J := Info.Vert.FirstGridCell; // switch to first visible nonfixed cell
        if Cells[J, I] = Self then
        begin
          FGrid.InvalidateCell(I, J);
          Exit;
        end;
        Inc(VExtent, Info.Vert.CellExtent(J) + Info.Vert.EffectiveSpacing(J));
        Inc(J);
      end;
      Inc(HExtent, Info.Horz.CellExtent(I) + Info.Horz.EffectiveSpacing(I));
      Inc(I);
    end;
  end;
end;

procedure TKGridCell.BeforeUpdate;
begin
  // empty
end;

procedure TKGridCell.ApplyDrawProperties;
begin
end;

procedure TKGridCell.DrawCell(ACol, ARow: Integer; const ARect: TRect;
  State: TKGridDrawState);
begin
  FGrid.CellPainter.DefaultDraw;
end;

procedure TKGridCell.EditorCreate(ACol, ARow: Integer; var AEditor: TWinControl);
begin
  FGrid.DefaultEditorCreate(ACol, ARow, AEditor);
end;

procedure TKGridCell.EditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer;
  var AssignText: Boolean);
begin
  FGrid.DefaultEditorDataFromGrid(AEditor, ACol, ARow, AssignText);
end;

procedure TKGridCell.EditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer;
  var AssignText: Boolean);
begin
  FGrid.DefaultEditorDataToGrid(AEditor, ACol, ARow, AssignText);
end;

procedure TKGridCell.EditorDestroy(var AEditor: TWinControl; ACol, ARow: Integer);
begin
  FGrid.DefaultEditorDestroy(AEditor, ACol, ARow);
end;

procedure TKGridCell.EditorKeyPreview(AEditor: TWinControl; ACol, ARow: Integer;
  var Key: Word; Shift: TShiftState; var IsGridKey: Boolean);
begin
  FGrid.DefaultEditorKeyPreview(AEditor, ACol, ARow, Key, Shift, IsGridKey);
end;

procedure TKGridCell.EditorResize(AEditor: TWinControl; ACol, ARow: Integer;
  var ARect: TRect);
begin
  FGrid.DefaultEditorResize(AEditor, ACol, ARow, ARect);
end;

procedure TKGridCell.EditorSelect(AEditor: TWinControl; ACol, ARow: Integer;
  SelectAll, CaretToLeft, SelectedByMouse: Boolean);
begin
  FGrid.DefaultEditorSelect(AEditor, ACol, ARow, SelectAll, CaretToLeft, SelectedByMouse);
end;

function TKGridCell.FindCell(out ACol, ARow: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if Assigned(FGrid) then
    for I := 0 to FGrid.ColCount - 1 do
      for J := 0 to FGrid.RowCount - 1 do
        if FGrid.ArrayOfCells[J, I] = Self then
        begin
          ACol := I;
          ARow := J;
          Result := True;
          Exit;
        end;
end;

procedure TKGridCell.Initialize;
begin
  FSpan := MakeCellSpan(1, 1);
end;

procedure TKGridCell.MeasureCell(ACol, ARow: Integer; const ARect: TRect;
  State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
begin
  Extent := FGrid.CellPainter.DefaultMeasure(Priority);
end;

procedure TKGridCell.SelectCell(ACol, ARow: Integer; var ACanSelect: Boolean);
begin
end;

procedure TKGridCell.SelectionExpand(ACol, ARow: Integer; var ACanExpand: Boolean);
begin
end;

procedure TKGridCell.SetColSpan(const Value: Integer);
var
  ACol, ARow: Integer;
begin
  if Value <> FSpan.ColSpan then
  begin
    if Assigned(FGrid) and not FGrid.Flag(cGF_GridUpdates) then
    begin
      if FindCell(ACol, ARow) then
        FGrid.CellSpan[ACol, ARow] := MakeCellSpan(Value, FSpan.RowSpan);
    end else
      FSpan.ColSpan := Value;
  end;
end;

procedure TKGridCell.SetRowSpan(const Value: Integer);
var
  ACol, ARow: Integer;
begin
  if Value <> FSpan.RowSpan then
  begin
    if Assigned(FGrid) and not FGrid.Flag(cGF_GridUpdates) then
    begin
      if FindCell(ACol, ARow) then
        FGrid.CellSpan[ACol, ARow] := MakeCellSpan(FSpan.ColSpan, Value);
    end else
      FSpan.RowSpan := Value;
  end;
end;

procedure TKGridCell.SetSpan(const Value: TKGridCellSpan);
var
  ACol, ARow: Integer;
begin
  if (Value.ColSpan <> FSpan.ColSpan) or (Value.RowSpan <> FSpan.RowSpan) then
  begin
    if Assigned(FGrid) and not FGrid.Flag(cGF_GridUpdates) then
    begin
      if FindCell(ACol, ARow) then
        FGrid.CellSpan[ACol, ARow] := Value;
    end else
      FSpan := Value;
  end;
end;

{ TKGridTextCell }

constructor TKGridTextCell.Create(AGrid: TKCustomGrid);
begin
{$IFDEF STRING_IS_UNICODE}
  FText := '';
{$ELSE}
  FText := nil;
{$ENDIF}
  inherited;
end;

destructor TKGridTextCell.Destroy;
begin
  inherited;
{$IFNDEF STRING_IS_UNICODE}
  FreeMem(FText);
{$ENDIF}
end;

procedure TKGridTextCell.ApplyDrawProperties;
begin
  FGrid.CellPainter.Text := Text;
end;

procedure TKGridTextCell.Assign(Source: TKGridCell);
begin
  inherited;
  if Source is TKGridTextCell then
    SetText(TKGridTextCell(Source).TextPtr);
end;

procedure TKGridTextCell.AssignText(const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
{$IFNDEF STRING_IS_UNICODE}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF STRING_IS_UNICODE}
  FText := Value;
{$ELSE}
  Len := (Length(Value) + 1) * SizeOf(WideChar);
  ReallocMem(FText, Len);
  if Value <> '' then
    Move(Value[1], FText^, Len)
  else if FText <> nil then
    FText[0] := #0;
{$ENDIF}
end;

procedure TKGridTextCell.EditorCreate(ACol, ARow: Integer; var AEditor: TWinControl);
begin
  AEditor := TEdit.Create(nil);
end;

{$IFDEF STRING_IS_UNICODE}
function TKGridTextCell.GetTextPtr: PChar;
begin
  Result := PChar(FText);
end;
{$ELSE}
function TKGridTextCell.GetText: WideString;
begin
  Result := FText;
end;
{$ENDIF}

procedure TKGridTextCell.Initialize;
begin
  inherited;
{$IFDEF STRING_IS_UNICODE}
  FText := '';
{$ELSE}
  FreeMem(FText);
  FText := nil;
{$ENDIF}
end;

procedure TKGridTextCell.SetText(const Value: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
begin
{$IFDEF STRING_IS_UNICODE}
  if Value <> FText then
{$ELSE}
  if CompareWideChars(PWideChar(Value), FText) <> 0 then
{$ENDIF}
  begin
    BeforeUpdate;
    AssignText(Value);
    AfterUpdate;
  end;
end;

{ TKGridAttrTextCell }

constructor TKGridAttrTextCell.Create(AGrid: TKCustomGrid);
begin
  inherited;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChange;
  FBrushChanged := False;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FFontChanged := False;
  Initialize;
end;

destructor TKGridAttrTextCell.Destroy;
begin
  FBrush.Free;
  FFont.Free;
  inherited;
end;

procedure TKGridAttrTextCell.ApplyDrawProperties;
var
  AColor: TColor;
begin
  inherited;
  if FGrid.CellPainter.State * [gdSelected] <> [] then
  begin
    // Brush remains unaffected by default
    if FFontChanged then
    begin
      // Font color remains unaffected by default
      AColor := FGrid.CellPainter.Canvas.Font.Color;
      FGrid.CellPainter.Canvas.Font := FFont;
      FGrid.CellPainter.Canvas.Font.Color := AColor;
      if FGrid.CellPainter.FPrinting then
        FGrid.CellPainter.Canvas.Font.Height := Abs(FFont.Height);
    end;
  end else
  begin
    FGrid.CellPainter.BackColor := FBackColor;
    if FBrushChanged then
    begin
      FGrid.CellPainter.Canvas.Brush := FBrush;
    {$IFNDEF FPC}
      SetBrushOrgEx(FGrid.CellPainter.Canvas.Handle, FGrid.CellPainter.CellRect.Left,
        FGrid.CellPainter.CellRect.Top, nil);
    {$ENDIF}
    end;
    if FFontChanged then
    begin
      FGrid.CellPainter.Canvas.Font := FFont;
      if FGrid.CellPainter.FPrinting then
        FGrid.CellPainter.Canvas.Font.Height := Abs(FFont.Height);
    end;
  end;
  FGrid.CellPainter.HAlign := FHAlign;
  FGrid.CellPainter.VAlign := FVAlign;
  FGrid.CellPainter.HPadding := FHPadding;
  FGrid.CellPainter.VPadding := FVPadding;
end;

procedure TKGridAttrTextCell.Assign(Source: TKGridCell);
begin
  inherited;
  if Source is TKGridAttrTextCell then
  begin
    FBackColor := TKGridAttrTextCell(Source).BackColor;
    FBrush.Assign(TKGridAttrTextCell(Source).Brush);
    FBrushChanged := TKGridAttrTextCell(Source).BrushChanged;
    FFont.Assign(TKGridAttrTextCell(Source).Font);
    FFontChanged := TKGridAttrTextCell(Source).FontChanged;
    FHAlign := TKGridAttrTextCell(Source).HAlign;
    FHPadding := TKGridAttrTextCell(Source).HPadding;
    FVAlign := TKGridAttrTextCell(Source).VAlign;
    FVPadding := TKGridAttrTextCell(Source).VPadding;
  end;
end;

procedure TKGridAttrTextCell.BrushChange(Sender: TObject);
begin
  BeforeUpdate;
  FBrushChanged := True;
  AfterUpdate;
end;

procedure TKGridAttrTextCell.FontChange(Sender: TObject);
begin
  BeforeUpdate;
  FFontChanged := True;
  AfterUpdate;
end;

procedure TKGridAttrTextCell.Initialize;
begin
  inherited;
  FBackColor := clWindow;
  FHAlign := halLeft;
  FHPadding := 2;
  FVAlign := valCenter;
  FVPadding := 0;
  // no defaults for Brush and Font!
end;

procedure TKGridAttrTextCell.SetBackColor(const Value: TColor);
begin
  if Value <> FBackColor then
  begin
    BeforeUpdate;
    FBackColor := Value;
    AfterUpdate;
  end;
end;

procedure TKGridAttrTextCell.SetFHAlign(const Value: TKHAlign);
begin
  if Value <> FHAlign then
  begin
    BeforeUpdate;
    FHAlign := Value;
    AfterUpdate;
  end;
end;

procedure TKGridAttrTextCell.SetAttributes(const AValue: TKTextAttributes);
begin
  if AValue <> FAttributes then
  begin
    BeforeUpdate;
    FAttributes := AValue;
    AfterUpdate;
  end;
end;

procedure TKGridAttrTextCell.SetFHPadding(const Value: Integer);
begin
  if Value <> FHPadding then
  begin
    BeforeUpdate;
    FHPadding := Value;
    AfterUpdate;
  end;
end;

procedure TKGridAttrTextCell.SetFVAlign(const Value: TKVAlign);
begin
  if Value <> FVAlign then
  begin
    BeforeUpdate;
    FVAlign := Value;
    AfterUpdate;
  end;
end;

procedure TKGridAttrTextCell.SetFVPadding(const Value: Integer);
begin
  if Value <> FVPadding then
  begin
    BeforeUpdate;
    FVPadding := Value;
    AfterUpdate;
  end;
end;

{ TKGridObjectCell }

constructor TKGridObjectCell.Create(AGrid: TKCustomGrid);
begin
  FCellObject := nil;
  inherited;
end;

destructor TKGridObjectCell.Destroy;
begin
  inherited;
  FCellObject.Free;
end;

procedure TKGridObjectCell.Assign(Source: TKGridCell);
var
  Obj: TObject;
begin
  inherited;
  if Source is TKGridObjectCell then
  begin
    Obj := TKGridObjectCell(Source).CellObject;
    if (Obj is TPersistent) and (FCellObject.ClassType = Obj.ClassType) then
      TPersistent(FCellObject).Assign(TPersistent(Obj));
  end;
end;

procedure TKGridObjectCell.Initialize;
begin
  inherited;
  FreeAndNil(FCellObject);
end;

procedure TKGridObjectCell.SetCellObject(Value: TObject);
begin
  if Value <> FCellObject then
  begin
    FCellObject.Free;
    FCellObject := Value;
  end;
end;

{ TKGridCellPainter }

constructor TKGridCellPainter.Create(AGrid: TKCustomGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FCanvas := nil;
  FCol := 0;
  FClipLock := 0;
  FRgn := 0;
  FRow := 0;
  FState := [];
  FValidClipping := False;
  FSortArrow := TKAlphaBitmap.CreateFromRes('KGRID_SORT_ARROW');
  Initialize;
end;

destructor TKGridCellPainter.Destroy;
begin
  FSortArrow.Free;
  inherited;
end;

function TKGridCellPainter.BeginClip;
var
  R: TRect;
begin
  if FClipLock = 0 then with FCanvas do
  begin
    R := FCellRect;
    TranslateRectToDevice(Handle, R);
    FValidClipping := ExtSelectClipRect(Handle, R, RGN_AND, FRgn);
  end;
  Inc(FClipLock);
  Result := FValidClipping;
end;

procedure TKGridCellPainter.BeginDraw;
begin
  DefaultAttributes;
end;

function TKGridCellPainter.CellCheckBoxRect(var BaseRect: TRect; out Bounds, Interior: TRect; StretchMode: TKStretchMode): Boolean;
begin
  if FCheckBox and not IsRectEmpty(BaseRect) then
  begin
    ExcludeShapeFromBaseRect(BaseRect, cCheckBoxFrameSize{$IFDEF LCLQT} + 1{$ENDIF}, cCheckBoxFrameSize, FCheckBoxHAlign,
      FCheckBoxVAlign, FCheckBoxHPadding, FCheckBoxVPadding, StretchMode, Bounds, Interior);
    Result := True;
  end else
    Result := False;
end;

function TKGridCellPainter.CellGraphicRect(var BaseRect: TRect; out Bounds, Interior: TRect; StretchMode: TKStretchMode): Boolean;
begin
  if Assigned(FGraphic) and not IsRectEmpty(BaseRect) then
  begin
    ExcludeShapeFromBaseRect(BaseRect, FGraphic.Width, FGraphic.Height, FGraphicHAlign,
      FGraphicVAlign, FGraphicHPadding, FGraphicVPadding, StretchMode, Bounds, Interior);
    Result := True;
  end else
    Result := False;
end;

function TKGridCellPainter.CellSortArrowRect(var BaseRect: TRect; out Bounds, Interior: TRect): Boolean;
var
  ArrowWidth: Integer;
begin
  ArrowWidth := SortArrowWidth;
  if (ArrowWidth > 0) and not IsRectEmpty(BaseRect) then
  begin
    ExcludeShapeFromBaseRect(BaseRect, ArrowWidth, BaseRect.Bottom - BaseRect.Top, FSortArrowHAlign,
      valCenter, FSortArrowHPadding, 0, stmNone, Bounds, Interior);
    Result := True;
  end else
    Result := False;
end;

function TKGridCellPainter.CellTextExtent(const BaseRect: TRect; out Extent: TPoint): Boolean;
var
  R: TRect;
begin
  if (FText <> '') and not IsRectEmpty(BaseRect) then
  begin
    R := BaseRect;
    DrawAlignedText(FCanvas, R, FHAlign, FVAlign,
      FHPadding, FVPadding, FText, FBackColor, FAttributes + [taCalcRect]);
    Extent.X := R.Right - R.Left;
    Extent.Y := R.Bottom - R.Top;
    Result := True;
  end else
    Result := False;
end;

function TKGridCellPainter.CellTextRect(var BaseRect: TRect; out Bounds, Interior: TRect): Boolean;
var
  Extent: TPoint;
begin
  if CellTextExtent(BaseRect, Extent) then
  begin
    ExcludeShapeFromBaseRect(BaseRect, Extent.X, Extent.Y, FHAlign,
      FVAlign, FHPadding, FVPadding, stmNone, Bounds, Interior);
    Result := True;
  end else
    Result := False;
end;

procedure TKGridCellPainter.DefaultAttributes;
var
  Color: TColor;
begin
  Initialize;
  // prepare default brush and font style
  with FCanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Font := FGrid.Font;
    if FPrinting then
      Font.Height := Abs(FGrid.Font.Height);
    if gdFixed in FState then
    begin
      // aki:
      if gdSelected in FState then
        Color := FGrid.Colors.SelectedFixedCellBkGnd
      else if (goIndicateSelection in FGrid.Options) and (FGrid.ColSelected(FCol) and
        not (goRowSelect in FGrid.Options) or FGrid.RowSelected(FRow)) then
        Color := FGrid.Colors.FixedCellIndication
      else
        Color := FGrid.Colors.FixedCellBkGnd;
      if gdMouseDown in FState then
        Brush.Color := BrightColor(Color, 0.6, bsOfTop)
      else
        Brush.Color := Color;
      Font.Color := FGrid.Colors.FixedCellText;
    end else if gdSelected in FState then
    begin
      if FPrinting or FGrid.HasFocus then
      begin
        if (FGrid.Col = FCol) and (FGrid.Row = FRow) then
        begin
          Brush.Color := FGrid.Colors.FocusedCellBkGnd;
          Font.Color := FGrid.Colors.FocusedCellText;
        end else
        begin
          Brush.Color := FGrid.Colors.FocusedRangeBkGnd;
          Font.Color := FGrid.Colors.FocusedRangeText;
        end;
      end else
      begin
        if (FGrid.Col = FCol) and (FGrid.Row = FRow) then
        begin
          Brush.Color := FGrid.Colors.SelectedCellBkGnd;
          Font.Color := FGrid.Colors.SelectedCellText;
        end else
        begin
          Brush.Color := FGrid.Colors.SelectedRangeBkGnd;
          Font.Color := FGrid.Colors.SelectedRangeText;
        end;
      end;
    end else
    begin
      Brush.Color := FGrid.Colors.CellBkGnd;
      Font.Color := FGrid.Colors.CellText;
    end;
  end;
end;

procedure TKGridCellPainter.DefaultDraw;
begin
  if gdFixed in FState then
  begin
    if (FRow < FGrid.FixedRows) and (goHeader in FGrid.Options) then
      DrawHeaderCellBackground(FCellRect)
    else
      DrawFixedCellBackground(FCellRect);
  end
  else if gdSelected in FState then
  begin
    if FGrid.Options * [goRowSelect, goRangeSelect] <> [] then    
      DrawSelectedCellBackground(FBlockRect, @FCellRect)
    else  
      DrawSelectedCellBackground(FCellRect)
  end else
    DrawNormalCellBackground(FCellRect);
  DrawCellCommon;
end;

function TKGridCellPainter.DefaultEdges: Cardinal;
begin
  Result := 0;
  if goFixedHorzLine in FGrid.Options then
  begin
    Result := BF_TOP;
    if not (goAlignLastRow in FGrid.Options) or (FRow < FGrid.RowCount - 1) then
      Result := Result or BF_BOTTOM;
  end;
  if goFixedVertLine in FGrid.Options then
  begin
    Result := Result or BF_LEFT;
    if not (goAlignLastCol in FGrid.Options) or (FCol < FGrid.ColCount - 1) then
      Result := Result or BF_RIGHT;
  end;
end;

function TKGridCellPainter.DefaultMeasure(Priority: TKGridMeasureCellPriority): TPoint;
const
  cMaxAutoSizeColWidth = 10000;
  cMaxAutoSizeRowHeight = 10000;
  cMaxAutoSizeStretchImageHeight = 1024;
var
  BaseRect, Bounds, Interior: TRect;
begin
  BaseRect := FCellRect;
  case Priority of
    mpColWidth: BaseRect.Right := cMaxAutoSizeColWidth;
    mpRowHeight: BaseRect.Bottom := cMaxAutoSizeRowHeight;
  else
    BaseRect.Right := cMaxAutoSizeColWidth;
    BaseRect.Bottom := cMaxAutoSizeRowHeight;
  end;
  if Assigned(FGraphic) and (FGraphicStretchMode in [stmZoom, stmZoomInOnly]) then
    BaseRect.Bottom := Min(BaseRect.Bottom, BaseRect.Top + (FGraphicVPadding shl 1) + cMaxAutoSizeStretchImageHeight);
//  BaseRect.Right := MaxInt; // keep cell height, maximize cell width and cut each object from BaseRect
  Result.X := 0;
  Result.Y := 0;
  if CellSortArrowRect(BaseRect, Bounds, Interior) then
  begin
    Inc(Result.X, Bounds.Right - Bounds.Left);
    Result.Y := Interior.Bottom - Interior.Top;
  end;
  if CellCheckBoxRect(BaseRect, Bounds, Interior, stmNone) then // for measuring always consider check box frame with original size
  begin
    Inc(Result.X, Bounds.Right - Bounds.Left);
    Result.Y := Max(Result.Y, Interior.Bottom - Interior.Top + (FCheckBoxVPadding shl 1));
  end;
  if CellGraphicRect(BaseRect, Bounds, Interior, FGraphicStretchMode) then // for measuring consider stretched image as for drawing
  begin
    Inc(Result.X, Bounds.Right - Bounds.Left);
    Result.Y := Max(Result.Y, Interior.Bottom - Interior.Top + (FGraphicVPadding shl 1));
  end;
  if CellTextExtent(BaseRect, Interior.TopLeft) then
  begin
    Inc(Result.X, Interior.Left + (FHPadding shl 1));
    Result.Y := Max(Result.Y, Interior.Top + (FVPadding shl 1));
  end;
end;

procedure TKGridCellPainter.DrawCellCommon;
var
  BaseRect, Bounds, Interior, BoundsSA, InteriorSA: TRect;
  IsSortArrow: Boolean;
begin
  if not (gdEdited in FState) then
  begin
    BaseRect := FCellRect;
    IsSortArrow := CellSortArrowRect(BaseRect, BoundsSA, InteriorSA);
    if CellCheckBoxRect(BaseRect, Bounds, Interior, stmZoomOutOnly) then // disallow zoom in for check box frame
      DrawCellCheckBox(Bounds, Interior);
    if CellGraphicRect(BaseRect, Bounds, Interior, FGraphicStretchMode) then
      DrawCellGraphic(Bounds, Interior);
    if not IsRectEmpty(BaseRect) then
    begin
      if FButton then
        DrawCellButton(BaseRect)
      else
        DrawCellText(BaseRect);
    end;
    if IsSortArrow then
      DrawCellSortArrow(BoundsSA, InteriorSA);
    if gdSelected in FState then
      DrawCellFocus(FCellRect);
  end;
end;

procedure TKGridCellPainter.DrawButtonFrame(const ARect: TRect);
var
  BM: TBitmap;
  TmpCanvas: TCanvas;
  TmpRect: TRect;
  ButtonState: Integer;
  IsHot: Boolean;
  MousePt: TPoint;
{$IFDEF USE_THEMES}
  ButtonTheme: TThemedButton;
{$ENDIF}
begin
  // a LOT of tweaking here...
{$IF DEFINED(USE_WINAPI) OR DEFINED(LCLQT) } // GTK2 cannot strech and paint on bitmap canvas, grrr..
  if CanvasScaled(FCanvas) {$IFDEF USE_WINAPI}and FGrid.ThemedCells{$ENDIF} then
  begin
    BM := TBitmap.Create;
    BM.Width := ARect.Right - ARect.Left;
    BM.Height := ARect.Bottom - ARect.Top;
    BM.Canvas.Brush.Assign(FCanvas.Brush);
    TmpRect := Rect(0, 0, BM.Width, BM.Height);
    BM.Canvas.FillRect(TmpRect);
    TmpCanvas := BM.Canvas;
  end else
{$IFEND}
  begin
    BM := nil;
    TmpRect := ARect;
    TmpCanvas := FCanvas;
  end;
  try
    MousePt := FGrid.ScreenToClient(Mouse.CursorPos);
    IsHot := (gdMouseOver in FState) and
      (not FHotFrameOnly or PtInRect(ARect, MousePt));
  {$IFDEF USE_THEMES}
    if FGrid.ThemedCells then
    begin
      if FGrid.Enabled then
        if FButtonPressed then
          ButtonTheme := tbPushButtonPressed
        else
          if IsHot then
            ButtonTheme := tbPushButtonHot
          else
            ButtonTheme := tbPushButtonNormal
      else
        ButtonTheme := tbPushButtonDisabled;
      ThemeServices.DrawElement(TmpCanvas.Handle, ThemeServices.GetElementDetails(ButtonTheme), TmpRect);
    end else
  {$ENDIF}
    begin
      ButtonState := DFCS_BUTTONPUSH;
      if FButtonPressed then ButtonState := ButtonState or DFCS_PUSHED;
      if not FGrid.Enabled then ButtonState := ButtonState or DFCS_INACTIVE;
      DrawFrameControl(TmpCanvas.Handle, TmpRect, DFC_BUTTON, ButtonState);
    end;
    if BM <> nil then
      FCanvas.Draw(ARect.Left, ARect.Top, BM);
  finally
    BM.Free;
  end;
end;

procedure TKGridCellPainter.DrawCellButton(Bounds: TRect);
begin
  DrawButtonFrame(Bounds);
  DrawCellText(Bounds);
end;

procedure TKGridCellPainter.DrawCellCheckBox(const Bounds, Interior: TRect);
begin
  DrawCheckBoxFrame(Interior);
end;

procedure TKGridCellPainter.DrawCellGraphic(const Bounds, Interior: TRect);
begin
  if Assigned(FGraphic) then
  begin
    if FGraphicStretchMode = stmZoom then
      SafeStretchDraw(FCanvas, Interior, FGraphic, FBackColor)
    else if BeginClip then
    try
      SafeStretchDraw(FCanvas, Interior, FGraphic, FBackColor);
    finally
      EndClip;
    end;
  end;
end;

procedure TKGridCellPainter.DrawCellFocus(const ARect: TRect; SkipTest: Boolean);
begin
  if (gdFocused in FState) and (SkipTest or (FGrid.Options * [goRangeSelect, goRowSelect,
    goDrawFocusSelected] = [goDrawFocusSelected])) then
  begin
    // to ensure coming DrawFocusRect will be painted correctly:
    SetBkColor(FCanvas.Handle, $FFFFFF);
    SetTextColor(FCanvas.Handle, 0);
    FCanvas.DrawFocusRect(FCellRect);
  end;
end;

procedure TKGridCellPainter.DrawCellSortArrow(const Bounds, Interior: TRect);
var
  ArrowCopy: TKAlphaBitmap;
  Mirror, Rotate: Boolean;
begin
  if FSortArrow <> nil then
  begin
    if BeginClip then
    try
      Mirror := FState * [gdColsSortedDown, gdRowsSortedDown] <> [];
      Rotate := FState * [gdColsSortedDown, gdColsSortedUp] <> [];
      ArrowCopy := TKAlphaBitmap.Create;
      try
        if Rotate then
        begin
          ArrowCopy.CopyFromRotated(FSortArrow);
          if Mirror then
            ArrowCopy.MirrorHorz;
        end else
        begin
          ArrowCopy.CopyFrom(FSortArrow);
          if Mirror then
            ArrowCopy.MirrorVert;
        end;
        ArrowCopy.AlphaDrawTo(FCanvas, Interior.Left, Interior.Top + (Interior.Bottom - Interior.Top - ArrowCopy.Height) div 2);
      finally
        ArrowCopy.Free;
      end;
    finally
      EndClip;
    end;
  end;
end;

procedure TKGridCellPainter.DrawCellText(var ARect: TRect);
var
  TextAttributes: TKTextAttributes;
begin
  TextAttributes := FAttributes;
{  if FFillCellBackground then
    Include(TextAttributes, taFillRect)
  else
    Exclude(TextAttributes, taFillRect);}
  DrawAlignedText(FCanvas, ARect, FHAlign, FVAlign,
    FHPadding, FVPadding, FText, FBackColor, TextAttributes);
end;

procedure TKGridCellPainter.DrawCheckBoxFrame(const ARect: TRect);
var
  BM: TBitmap;
  TmpCanvas: TCanvas;
  TmpRect: TRect;
  State: Integer;
  IsHot: Boolean;
  MousePt: TPoint;
{$IFDEF USE_THEMES}
  CheckBoxTheme: TThemedButton;
{$ENDIF}
begin
  // a LOT of tweaking here...
{$IF DEFINED(USE_WINAPI) OR DEFINED(LCLQT) } // GTK2 cannot strech and paint on bitmap canvas, grrr..
  if CanvasScaled(FCanvas) {$IFDEF USE_WINAPI}and FGrid.ThemedCells{$ENDIF} then
  begin
    BM := TBitmap.Create;
    BM.Width := ARect.Right - ARect.Left;
    BM.Height := ARect.Bottom - ARect.Top;
    BM.Canvas.Brush.Assign(FCanvas.Brush);
    TmpRect := Rect(0, 0, BM.Width, BM.Height);
    BM.Canvas.FillRect(TmpRect);
    TmpCanvas := BM.Canvas;
  end else
{$IFEND}
  begin
    BM := nil;
    TmpRect := ARect;
    TmpCanvas := FCanvas;
  end;
  try
  {$IFDEF USE_THEMES}
    MousePt := FGrid.ScreenToClient(Mouse.CursorPos);
    IsHot := (gdMouseOver in FState) and
      (not FHotFrameOnly or PtInRect(ARect, MousePt));
    if FGrid.ThemedCells then
    begin
      if FGrid.Enabled then
        case FCheckBoxState of
          cbChecked:
          begin
            if IsHot then
              CheckBoxTheme := tbCheckBoxCheckedHot
            else
              CheckBoxTheme := tbCheckBoxCheckedNormal;
          end;
          cbUnchecked:
          begin
            if IsHot then
              CheckBoxTheme := tbCheckBoxUncheckedHot
            else
              CheckBoxTheme := tbCheckBoxUncheckedNormal;
          end;
        else
          if IsHot then
            CheckBoxTheme := tbCheckBoxMixedHot
          else
            CheckBoxTheme := tbCheckBoxMixedNormal;
        end
      else
        case FCheckboxState of
          cbChecked:
            CheckBoxTheme := tbCheckBoxCheckedDisabled;
          cbUnchecked:
            CheckBoxTheme := tbCheckBoxUncheckedDisabled;
        else
          CheckBoxTheme := tbCheckBoxMixedDisabled;
        end;
      ThemeServices.DrawElement(TmpCanvas.Handle, ThemeServices.GetElementDetails(CheckBoxTheme), TmpRect);
    end else
  {$ENDIF}
    begin
      State := DFCS_BUTTON3STATE;
      case FCheckBoxState of
        cbChecked:
          State := State or DFCS_CHECKED;
//        cbGrayed:
//          State := State or DFCS_GRAYED;
        end;
      if not FGrid.Enabled then State := State or DFCS_INACTIVE;
      DrawFrameControl(TmpCanvas.Handle, TmpRect, DFC_BUTTON, State);
    end;
    if BM <> nil then
      FCanvas.Draw(ARect.Left, ARect.Top, BM);
  finally
    BM.Free;
  end;
end;

procedure TKGridCellPainter.DrawHeaderCellBackground(const ARect: TRect);
{$IFDEF USE_THEMES}
var
  Details: TThemedElementDetails;
  Header: TThemedHeader;
  TmpRect: TRect;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FGrid.ThemedCells then with ThemeServices do
  begin
    if gdSelected in FState then
      Header := thHeaderItemPressed
    else if gdMouseDown in FState then
      Header := thHeaderItemPressed
    else if gdMouseOver in FState then
      Header := thHeaderItemHot
    else
      Header := thHeaderItemNormal;
    { The background for the themed header is messy. HasTransparentParts returns
      always True and we cannot call DrawParentBackground as this is wrong
      approach here. So for this reason, thHeaderItemNormal is always supposed
      to be visually not transparent. We paint it only if double buffering is
      present because double buffer is a temporary memory and, of course,
      the screen content is not copied back to the double buffer. }
    TmpRect := ARect;
    Inc(TmpRect.Bottom); // it is nicer
    if FGrid.IsDoubleBuffered and (Header <> thHeaderItemNormal) then
      DrawElement(FCanvas.Handle, GetElementDetails(thHeaderItemNormal), TmpRect);
    Details := GetElementDetails(Header);
    DrawElement(FCanvas.Handle, Details, TmpRect);
  end else
{$ENDIF}
    DrawFixedCellNonThemedBackground(ARect);
end;

procedure TKGridCellPainter.DrawEmptyCell;
begin
  DrawNormalCellBackground(FCellRect);
end;

procedure TKGridCellPainter.DrawFixedCell;
begin
  DrawFixedCellBackground(FCellRect);
  DrawCellCommon;
end;

procedure TKGridCellPainter.DrawFixedCellBackground(const ARect: TRect);
{$IFDEF USE_THEMES}
var
  Color1, Color2: TColor;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FGrid.ThemedCells and (gxFixedThemedCells in FGrid.OptionsEx) then
    DrawHeaderCellBackground(ARect)
  else if FGrid.ThemedCells then
  begin
    DrawFilledRectangle(FCanvas, ARect, FBackColor);
    if {$IFDEF FPC}not FGrid.Flat{$ELSE}FGrid.Ctl3D{$ENDIF} then
    begin
      if gdMouseDown in FState then
      begin
        Color1 := FGrid.Colors.FixedThemedCellShadow;
        Color2 := FGrid.Colors.FixedThemedCellHighlight;
      end else
      begin
        Color1 := FGrid.Colors.FixedThemedCellHighlight;
        Color2 := FGrid.Colors.FixedThemedCellShadow;
      end;
      DrawEdges(FCanvas, ARect, Color1, Color2, DefaultEdges);
    end;
  end else
{$ENDIF}
    DrawFixedCellNonThemedBackground(ARect);
end;

procedure TKGridCellPainter.DrawFixedCellNonThemedBackground(const ARect: TRect);
{$IFDEF USE_WINAPI}
var
  R: TRect;
{$ENDIF}
begin
  DrawFilledRectangle(FCanvas, ARect, FBackColor);
  if {$IFDEF FPC}not FGrid.Flat{$ELSE}FGrid.Ctl3D{$ENDIF} and not (gdMouseDown in FState) then
  begin
    {$IFDEF USE_WINAPI}
      // looks somewhat better though
      R := ARect;
      DrawEdge(FCanvas.Handle, R, BDR_RAISEDINNER, DefaultEdges);
    {$ELSE}
      DrawEdges(FCanvas, ARect, cl3DHilight, cl3DShadow, DefaultEdges);
    {$ENDIF}
  end;
end;

procedure TKGridCellPainter.DrawHeaderCell;
begin
  DrawHeaderCellBackground(FCellRect);
  DrawCellCommon;
end;

procedure TKGridCellPainter.DrawNormalCellBackground(const ARect: TRect);
begin
  DrawFilledRectangle(FCanvas, ARect, FBackColor);
end;

procedure TKGridCellPainter.DrawSelectableCell;
begin
  if gdSelected in FState then
    DrawSelectedCellBackground(FCellRect)
  else
    DrawNormalCellBackground(FCellRect);
  DrawCellCommon;
end;

procedure TKGridCellPainter.DrawSelectedCellBackground(const ARect: TRect; RClip: PRect);
var
{$IFDEF USE_THEMES}
 {$IF (DEFINED(COMPILER11_UP) OR DEFINED(FPC)) AND DEFINED(USE_WINAPI)}
  {$IFDEF FPC}
  Details: TThemedElementDetails;
  {$ELSE}
  SelectionTheme: HTHEME;
  {$ENDIF}
  Color: TColorRef;
 {$IFEND}
{$ENDIF}
  R: TRect;
begin
{$IFDEF USE_THEMES}
 {$IF (DEFINED(COMPILER11_UP) OR DEFINED(FPC)) AND DEFINED(USE_WINAPI)}
  if FGrid.ThemedCells and (Win32MajorVersion >= 6) then // Windows Vista and later
  begin
    // make the background brigther
    if FPrinting or FGrid.HasFocus then
      FCanvas.Brush.Color := BrightColor(FCanvas.Brush.Color, 0.8, bsOfTop)
    else
      FCanvas.Brush.Color := clWhite;
    if RClip <> nil then
      FCanvas.FillRect(RClip^)
    else
      FCanvas.FillRect(ARect);
  {$IFDEF FPC}
    Details := ThemeServices.GetElementDetails(tmPopupItemHot);
    ThemeServices.DrawElement(FCanvas.Handle, Details, ARect, RClip);
    Color := clWindowText; // getting text color not supported
  {$ELSE}
    SelectionTheme := ThemeServices.Theme[teMenu];
    DrawThemeBackground(SelectionTheme, FCanvas.Handle, MENU_POPUPITEM, MPI_HOT, ARect, RClip);
    GetThemeColor(SelectionTheme, MENU_POPUPITEM, MPI_HOT, TMT_TEXTCOLOR, Color);
  {$ENDIF}
    FCanvas.Font.Color := Color;
  end else
 {$IFEND}
{$ENDIF}
  begin
    if RClip <> nil then
      R := RClip^
    else
      R := ARect;
    DrawFilledRectangle(FCanvas, R, FBackColor);
  end;
end;

procedure TKGridCellPainter.DrawThemedFixedCell;
begin
  DrawFixedCellBackground(FCellRect);
  DrawCellCommon;
end;

procedure TKGridCellPainter.DrawThemedHeaderCell;
begin
  DrawHeaderCellBackground(FCellRect);
  DrawCellCommon;
end;

procedure TKGridCellPainter.EndClip;
begin
  if FClipLock > 0 then with FCanvas do
  begin
    Dec(FClipLock);
    if FClipLock = 0 then
    begin
      FinalizePrevRgn(Handle, FRgn);
      FValidClipping := False;
    end;
  end;
end;

procedure TKGridCellPainter.EndDraw;
begin
end;

function TKGridCellPainter.GetCheckBoxChecked: Boolean;
begin
  Result := FCheckBoxState = cbChecked;
end;

function TKGridCellPainter.GetSortArrowWidth: Integer;
begin
  if FSortArrow <> nil then
  begin
    if FState * [gdColsSortedDown, gdColsSortedUp] <> [] then
      Result := FSortArrow.Height + 3
    else if FState * [gdRowsSortedDown, gdRowsSortedUp] <> [] then
      Result := FSortArrow.Width + 3
    else
      Result := 0;
  end else
    Result := 0;
end;

procedure TKGridCellPainter.Initialize;
begin
  FAttributes := [taEndEllipsis];
  FBackColor := clWindow;
  FButton := False;
  FButtonPressed := False;
  FCheckBox := False;
  FCheckBoxHAlign := halLeft;
  FCheckBoxHPadding := 2;
  FCheckBoxVAlign := valCenter;
  FCheckBoxVPadding := 2;
  FCheckBoxState := cbUnchecked;
  FGraphic := nil;
  FGraphicDrawText := False;
  FGraphicHAlign := halCenter;
  FGraphicHPadding := 2;
  FGraphicStretchMode := stmZoom;
  FGraphicVAlign := valCenter;
  FGraphicVPadding := 2;
  FHAlign := halLeft;
  FHotFrameOnly := False;
  FHPadding := 2;
  FSortArrowHAlign := halRight;
  FSortArrowHPadding := 2;
  FText := '';
  FVAlign := valCenter;
  FVPadding := 0;
end;

procedure TKGridCellPainter.SetCheckBox(AValue: Boolean);
begin
  if AValue <> FCheckBox then
  begin
    FCheckBox := AValue;
    if AValue then
    begin
      // set default padding for check box text (not tested for Linux and MAC)
      if FGrid.Themes then
        FHPadding := 3
      else
        FHPadding := 4;
    end;
  end;
end;

procedure TKGridCellPainter.SetCheckBoxChecked(const Value: Boolean);
begin
  if Value then
    FCheckBoxState := cbChecked
  else
    FCheckBoxState := cbUnchecked;
end;

{ TKGridColors }

constructor TKGridColors.Create(AGrid: TKCustomGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FBrightRangeBkgnd := True;
  Initialize;
  ClearBrightColors;
  //BrightRangeBkGnds;
end;

procedure TKGridColors.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TKGridColors then
  begin
    Colors := TKGridColors(Source).Colors;
    FGrid.Invalidate;
  end
end;

procedure TKGridColors.BrightRangeBkGnds;
  procedure DoBright(Src: TColor; var Dest: TColor);
  begin
    Dest := BrightColor(Src, 0.4, bsOfTop);
  end;
begin
  if FBrightRangeBkGnd and (FGrid.ComponentState * [csDesigning, csLoading] = []) then
  begin
    DoBright(FColors[ciFocusedCellBkGnd], FColors[ciFocusedRangeBkGnd]);
    DoBright(FColors[ciSelectedCellBkGnd], FColors[ciSelectedRangeBkGnd]);
  end;
end;

procedure TKGridColors.ClearBrightColors;
var
  I: TKGridColorIndex;
begin
  for I := 0 to Length(FBrightColors) - 1 do
    FBrightColors[I] := clNone;
end;

function TKGridColors.GetColor(Index: TKGridColorIndex): TColor;
begin
  Result := InternalGetColor(Index);
end;

function TKGridColors.GetColorEx(Index: TKGridColorIndex): TColor;
begin
  Result := FColors[Index];
end;

procedure TKGridColors.Initialize;
begin
  SetLength(FColors, ciGridColorsMax + 1);
  SetLength(FBrightColors, ciGridColorsMax + 1);
  FColors[ciCellBkGnd] := cCellBkGndDef;
  FColors[ciCellLines] := cCellLinesDef;
  FColors[ciCellText] := cCellTextDef;
  FColors[ciDragSuggestionBkGnd] := cDragSuggestionBkGndDef;
  FColors[ciDragSuggestionLine] := cDragSuggestionLineDef;
  FColors[ciFixedCellBkGnd] := cFixedCellBkGndDef;
  FColors[ciFixedCellIndication] := cFixedCellIndicationDef;
  FColors[ciFixedCellLines] := cFixedCellLinesDef;
  FColors[ciFixedCellText] := cFixedCellTextDef;
  FColors[ciFixedThemedCellLines] := cFixedThemedCellLinesDef;
  FColors[ciFixedThemedCellHighlight] := cFixedThemedCellHighlightDef;
  FColors[ciFixedThemedCellShadow] := cFixedThemedCellShadowDef;
  FColors[ciFocusedCellBkGnd] := cFocusedCellBkGndDef;
  FColors[ciFocusedCellText] := cFocusedCellTextDef;
  FColors[ciFocusedRangeBkGnd] := cFocusedRangeBkGndDef;
  FColors[ciFocusedRangeText] := cFocusedRangeTextDef;
  FColors[ciSelectedCellBkGnd] := cSelectedCellBkGndDef;
  FColors[ciSelectedCellText] := cSelectedCellTextDef;
  FColors[ciSelectedRangeBkGnd] := cSelectedRangeBkGndDef;
  FColors[ciSelectedRangeText] := cSelectedRangeTextDef;
  // aki:
  FColors[ciSelectedFixedCellBkGnd] := cSelectedFixedCellBkGndDef;
end;

function TKGridColors.InternalGetColor(Index: TKGridColorIndex): TColor;
begin
  case FColorScheme of
    csBright:
    begin
      if FBrightColors[Index] = clNone then
        FBrightColors[Index] := BrightColor(FColors[Index], 0.5, bsOfTop);
      Result := FBrightColors[Index];
    end;
    csGrayed:
      case Index of
        ciCellBkGnd, ciFocusedCellText, ciSelectedCellText: Result := clWindow;
        ciCellText, ciFixedCellText, ciFocusedCellBkGnd: Result := clGrayText;
      else
        Result := FColors[Index];
      end;
    csGrayScale:
      Result := ColorToGrayScale(FColors[Index]);
  else
    Result := FColors[Index];
  end;
end;

procedure TKGridColors.InternalSetColor(Index: TKGridColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    FBrightColors[Index] := clNone;
    if not (csLoading in FGrid.ComponentState) then
      FGrid.Invalidate;
  end;
end;

procedure TKGridColors.SetColor(Index: TKGridColorIndex; Value: TColor);
begin
  InternalSetColor(Index, Value);
end;

procedure TKGridColors.SetColorEx(Index: TKGridColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    FBrightColors[Index] := clNone;
  end;
end;

procedure TKGridColors.SetColors(const Value: TKColorArray);
var
  I: Integer;
begin
  for I := 0 to Min(Length(FColors), Length(Value)) - 1 do
    FColors[I] := Value[I];
  ClearBrightColors;
  BrightRangeBkGnds;
end;

{ TKCustomGrid }

constructor TKCustomGrid.Create(AOwner: TComponent);
const
  GridStyle = [csCaptureMouse, csDoubleClicks, csOpaque];
begin
  inherited;
  if NewStyleControls then
    ControlStyle := GridStyle
  else
    ControlStyle := GridStyle + [csFramed];
{$IFDEF FPC}
  FFlat := False;
{$ENDIF}
  FCellHintTimer := TTimer.Create(Self);
  FCellHintTimer.Enabled := False;
  FCellHintTimer.Interval := cMouseCellHintTimeDef;
  FCellHintTimer.OnTimer := CellHintTimerHandler;
  FCells := nil;
  FCellClass := TKGridTextCell;
  FCellPainterClass := TKGridCellPainter;
  FCellPainter := FCellPainterClass.Create(Self);
  FColClass := TKGridCol;
  FColCount := cInvalidIndex;
  FCols := nil;
  FColors := TKGridColors.Create(Self);
  FDefaultColWidth := cDefaultColWidthDef;
  FDefaultRowHeight := cDefaultRowHeightDef;
  FDisabledDrawStyle := cDisabledDrawStyleDef;
  FDragArrow := TKAlphaBitmap.CreateFromRes('KGRID_DRAG_ARROW');
  FDragWindow := nil;
  FDragStyle := dsLayeredFaded;
  FEditedCell := nil;
  FEditor := nil;
  FEditorCell := GridPoint(-1, -1);
  FEditorTransparency := cEditorTransparencyDef;
  FFixedCols := cInvalidIndex;
  FFixedRows := cInvalidIndex;
  FGridLineWidth := cGridLineWidthDef;
  FGridState := gsNormal;
  FHCI.HBegin := TKAlphaBitmap.CreateFromRes('KGRID_HCI_HBEGIN');
  FHCI.HCenter := TKAlphaBitmap.CreateFromRes('KGRID_HCI_HCENTER');
  FHCI.HEnd := TKAlphaBitmap.CreateFromRes('KGRID_HCI_HEND');
  FHCI.VBegin := TKAlphaBitmap.CreateFromRes('KGRID_HCI_VBEGIN');
  FHCI.VCenter := TKAlphaBitmap.CreateFromRes('KGRID_HCI_VCENTER');
  FHCI.VEnd := TKAlphaBitmap.CreateFromRes('KGRID_HCI_VEND');
  FMaxCol := cInvalidIndex;
  FMaxRow := cInvalidIndex;
  FMemCol := cInvalidIndex;
  FMemRow := cInvalidIndex;
  FMinColWidth := cMinColWidthDef;
  FMinRowHeight := cMinRowHeightDef;
  FMouseCellHintTime := cMouseCellHintTimeDef;
  FMouseOver := GridPoint(-1, -1);
  FMoveDirection := cMoveDirectionDef;
  FOptions := cOptionsDef;
  FOptionsEx := cOptionsExDef;
  FRangeSelectStyle := cRangeSelectStyleDef;
  FRowClass := TKGridRow;
  FRowCount := cInvalidIndex;
  FRows := nil;
  FScrollBars := cScrollBarsDef;
  FScrollModeHorz := cScrollModeDef;
  FScrollModeVert := cScrollModeDef;
  FScrollOffset := Point(0, 0);
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FSelections := nil;
  FSizingStyle := cSizingStyleDef;
  FSortStyle := cSortStyleDef;
  FSortModeLock := 0;
  FTmpBitmap := TBitmap.Create;
  FTmpBitmap.Width := 1;
  FTmpBitmap.Height := 1;
  FTopLeft := GridPoint(cInvalidIndex, cInvalidIndex);
  FOnBeginColDrag := nil;
  FOnBeginColSizing := nil;
  FOnBeginRowDrag := nil;
  FOnBeginRowSizing := nil;
  FOnCellChanging := nil;
  FOnCellSpan := nil;
  FOnChanged := nil;
  FOnCheckColDrag := nil;
  FOnCheckRowDrag := nil;
  FOnColMoved := nil;
  FOnColWidthsChanged := nil;
  FOnColWidthsChangedEx := nil;
  FOnCompareCellInstances := nil;
  FOnCompareCells := nil;
  FOnCustomSortCols := nil;
  FOnCustomSortRows := nil;
  FOnDrawCell := nil;
  FOnEditorCreate := nil;
  FOnEditorDataFromGrid := nil;
  FOnEditorDataToGrid := nil;
  FOnEditorDestroy := nil;
  FOnEditorKeyPreview := nil;
  FOnEditorResize := nil;
  FOnEndColDrag := nil;
  FOnEndColSizing := nil;
  FOnEndRowDrag := nil;
  FOnEndRowSizing := nil;
  FOnExchangeCols := nil;
  FOnExchangeRows := nil;
  FOnMeasureCell := nil;
  FOnMouseCellHint := nil;
  FOnMouseClickCell := nil;
  FOnMouseDblClickCell := nil;
  FOnMouseEnterCell := nil;
  FOnMouseLeaveCell := nil;
  FOnRowHeightsChanged := nil;
  FOnRowMoved := nil;
  FOnSelectCell := nil;
  FOnSizeChanged := nil;
  FOnTopLeftChanged := nil;
  Color := clWindow;
  LoadCustomCursor(crHResize, 'KGRID_CURSOR_HRESIZE');
  LoadCustomCursor(crVResize, 'KGRID_CURSOR_VRESIZE');
  ParentColor := False;
  TabStop := True;
  ChangeDataSize(True, 0, cColCountDef, True, 0, cRowCountDef);
  SetBounds(Left, Top, FColCount * FDefaultColWidth,
    FRowCount * FDefaultRowHeight);
end;

destructor TKCustomGrid.Destroy;
begin
  EditorMode := False;
  inherited Destroy;
  FHint.Free;
  FCellPainter.Free;
  FColors.Free;
  FEditedCell.Free;
  FDragArrow.Free;
  FDragWindow.Free;
  FHCI.HBegin.Free;
  FHCI.HCenter.Free;
  FHCI.HEnd.Free;
  FHCI.VBegin.Free;
  FHCI.VCenter.Free;
  FHCI.VEnd.Free;
  FTmpBitmap.Free;
  FreeData;
end;

procedure TKCustomGrid.AdjustPageSetup;
begin
  inherited;
  PageSetup.PrintingMapped := True;
end;

function TKCustomGrid.AdjustSelection(const ASelection: TKGridRect): TKGridRect;
begin
  Result := ASelection;
  if goRowSelect in FOptions then
  begin
    // aki:
    if gxEditFixedCols in FOptionsEx then
    begin
      Result.Col1:=0;
      Result.Col2:=FColCount - 1;
    end else
    begin
      Result.Col1 := FFixedCols;
      Result.Col2 := FColCount - 1;
    end;
  end;
end;

procedure TKCustomGrid.AutoSizeCol(ACol: Integer; FixedCells: Boolean);
var
  R: TRect;
  Dummy, Extent, FirstRow, I, MaxExtent: Integer;
  Span: TKGridCellSpan;
  GridFocused: Boolean;
begin
  if ColValid(ACol) then
  begin
    GridFocused := HasFocus;
    R.Left := 0;
    R.Top := 0;
    MaxExtent := FMinColWidth;
    Extent := InternalGetColWidths(ACol);
    if FixedCells then FirstRow := 0 else FirstRow := FFixedRows;
    for I := FirstRow to FRowCount - 1 do
    begin
      Span := InternalGetCellSpan(ACol, I);
      if (Span.RowSpan > 0) and (Span.ColSpan > 0) then
      begin
        InternalGetHExtent(ACol, Span.ColSpan, R.Right, Dummy);
        InternalGetVExtent(I, Span.RowSpan, R.Bottom, Dummy);
        MaxExtent := Max(MaxExtent, MeasureCell(ACol, I, R, GetDrawState(ACol, I, GridFocused), mpColWidth).X - R.Right + Extent);
      end;
    end;
    ColWidths[ACol] := MaxExtent;
  end;
end;

procedure TKCustomGrid.AutoSizeGrid(Priority: TKGridMeasureCellPriority; FixedCells: Boolean);
var
  R: TRect;
  CellExtent: TPoint;
  Dummy, Extent, FirstCol, FirstRow, I, J, MaxExtent: Integer;
  Span: TKGridCellSpan;
  ModifyCols, ModifyRows, GridFocused: Boolean;
  ColMaxExtents: TDynIntegers;
begin
  LockUpdate;
  try
    { Despite the update lock, this function is rather slow for huge grids,
      of course, because it has to measure all cells. }
    GridFocused := HasFocus;
    MaxExtent := 0;
    Extent := 0;
    R.Left := 0;
    R.Top := 0;
    if FixedCells then FirstCol := 0 else FirstCol := FFixedCols;
    if FixedCells then FirstRow := 0 else FirstRow := FFixedRows;
    ModifyCols := Priority in [mpColWidth, mpCellExtent];
    ModifyRows := Priority in [mpRowHeight, mpCellExtent];
    if ModifyCols then
    begin
      SetLength(ColMaxExtents, FColCount - FirstCol);
      for I := 0 to FColCount - FirstCol - 1 do
        ColMaxExtents[I] := FMinColWidth;
    end;
    for J := FirstRow to FRowCount - 1 do
    begin
      if ModifyRows then
      begin
        MaxExtent := 0;
        Extent := InternalGetRowHeights(J);
      end;
      for I := FirstCol to FColCount - 1 do
      begin
        Span := InternalGetCellSpan(I, J);
        if (Span.RowSpan > 0) and (Span.ColSpan > 0) then
        begin
          InternalGetHExtent(I, Span.ColSpan, R.Right, Dummy);
          InternalGetVExtent(J, Span.RowSpan, R.Bottom, Dummy);
          CellExtent := MeasureCell(I, J, R, GetDrawState(I, J, GridFocused), Priority);
          if ModifyRows then
            MaxExtent := Max(MaxExtent, CellExtent.Y - R.Bottom + Extent);
          if ModifyCols then
            ColMaxExtents[I - FirstCol] := Max(ColMaxExtents[I - FirstCol], CellExtent.x - R.Right + InternalGetColWidths(I));
        end;
      end;
      if ModifyRows then
        RowHeights[J] := MaxExtent;
    end;
    if ModifyCols then
      for I := FirstCol to FColCount - 1 do
        ColWidths[I] := ColMaxExtents[I - FirstCol];
  finally
    UnlockUpdate;
  end;
end;

procedure TKCustomGrid.AutoSizeRow(ARow: Integer; FixedCells: Boolean);
var
  R: TRect;
  Dummy, Extent, FirstCol, I, MaxExtent: Integer;
  Span: TKGridCellSpan;
  GridFocused: Boolean;
begin
  if RowValid(ARow) then
  begin
    GridFocused := HasFocus;
    R.Left := 0;
    R.Top := 0;
    MaxExtent := FMinRowHeight;
    Extent := InternalGetRowHeights(ARow);
    if FixedCells then FirstCol := 0 else FirstCol := FFixedCols;
    for I := FirstCol to FColCount - 1 do
    begin
      Span := InternalGetCellSpan(I, ARow);
      if (Span.RowSpan > 0) and (Span.ColSpan > 0) then
      begin
        InternalGetHExtent(I, Span.ColSpan, R.Right, Dummy);
        InternalGetVExtent(ARow, Span.RowSpan, R.Bottom, Dummy);
        MaxExtent := Max(MaxExtent, MeasureCell(I, ARow, R, GetDrawState(I, ARow, GridFocused), mpRowHeight).Y - R.Bottom + Extent);
      end;
    end;
    RowHeights[ARow] := MaxExtent;
  end;
end;

function TKCustomGrid.BeginColDrag(var Origin: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnBeginColDrag) then
    FOnBeginColDrag(Self, Origin, MousePt, Result)
  else if Assigned(FCols) then
    FCols[Origin].BeginDrag(Origin, MousePt, Result);
  Origin := MinMax(Origin, FFixedCols, FColCount - 1);
end;

function TKCustomGrid.BeginColSizing(var Index, Pos: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeginColSizing) then
    FOnBeginColSizing(Self, Index, Pos, Result)
  else if Assigned(FCols) then
    Result := FCols[Index].CanResize;
  Index := MinMax(Index, 0, FColCount - 1);
end;

function TKCustomGrid.BeginRowDrag(var Origin: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnBeginRowDrag) then
    FOnBeginRowDrag(Self, Origin, MousePt, Result)
  else if Assigned(FRows) then
    FRows[Origin].BeginDrag(Origin, MousePt, Result);
  Origin := MinMax(Origin, FFixedRows, FRowCount - 1);
end;

function TKCustomGrid.BeginRowSizing(var Index, Pos: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeginRowSizing) then
    FOnBeginRowSizing(Self, Index, Pos, Result)
  else if Assigned(FRows) then
    Result := FRows[Index].CanResize;
  Index := MinMax(Index, 0, FRowCount - 1);
end;

procedure TKCustomGrid.CancelMode;
begin
  try
    case FGridState of
      gsColSizing, gsRowSizing:
        SuggestSizing(csStop);
      gsColMoving, gsRowMoving:
      begin
        ProcessDragWindow(FHitPos, Point(0, 0), cInvalidIndex, FGridState = gsColMoving, True);
        SuggestDrag(csStop);
      end;
    else
      InvalidateCell(FHitCell.Col, FHitCell.Row);
    end;
  finally
    MouseCapture := False;
    FGridState := gsNormal;
  end;
end;

procedure TKCustomGrid.CellChanging(AOldCol, AOldRow, ANewCol, ANewRow: Integer);
begin
  if Assigned(FOnCellChanging) then
    FOnCellChanging(Self, AOldCol, AOldRow, ANewCol, ANewRow);
end;

procedure TKCustomGrid.CellHintTimerHandler(Sender: TObject);
begin
  if (FMouseOver.Col = FHintCell.Col) and (FMouseOver.Row = FHintCell.Row) then
    MouseCellHint(FMouseOver.Col, FMouseOver.Row, True);
  FCellHintTimer.Enabled := False;  
end;

function TKCustomGrid.CellRect(ACol, ARow: Integer; out R: TRect; 
  VisibleOnly: Boolean): Boolean;
var
  I, W, H: Integer;
  Span: TKGridCellSpan;
begin
  Result := False;
  if ColValid(ACol) and RowValid(ARow) then
  begin
    Span := InternalGetCellSpan(ACol, ARow);
    if (Span.ColSpan <= 0) or (Span.RowSpan <= 0) then
      Span := MakeCellSpan(1, 1);
    if CellToPoint(ACol, ARow, R.TopLeft, VisibleOnly) then
    begin
      W := 0;
      for I := ACol to ACol + Span.ColSpan - 1 do
        Inc(W, InternalGetColWidths(I) + InternalGetEffectiveColSpacing(I));
      H := 0;
      for I := ARow to ARow + Span.RowSpan - 1 do
        Inc(H, InternalGetRowHeights(I) + InternalGetEffectiveRowSpacing(I));
      if ACol >= FFixedCols then
      begin
        if goVertLine in FOptions then Dec(W, FGridLineWidth);
      end else
        if goFixedVertLine in FOptions then Dec(W, FGridLineWidth);
      if ARow >= FFixedRows then
      begin
        if goHorzLine in FOptions then Dec(H, FGridLineWidth);
      end else
        if goFixedHorzLine in FOptions then Inc(H, FGridLineWidth);
      if (W > 0) and (H > 0) then
      begin
        R.Right := R.Left + W;
        R.Bottom := R.Top + H;
        Result := True;
      end;
    end;
  end;
end;

function TKCustomGrid.CellSelected(ACol, ARow: Integer): Boolean;
begin
  Result := CellInGridRect(ACol, ARow, Selection);
end;

function TKCustomGrid.CellToPoint(ACol, ARow: Integer; var Point: TPoint; 
  VisibleOnly: Boolean): Boolean;

  function Axis(const Info: TKGridAxisInfo; Cell: Integer; out Coord: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if (Cell >= 0) and (Cell < Info.TotalCellCount) then
    begin
      I := 0;
      Coord := 0;
      while (I < Cell) and (I < Info.FixedCellCount) and (not VisibleOnly or (Coord < Info.ClientExtent)) do
      begin
        Inc(Coord, Info.CellExtent(I) + Info.EffectiveSpacing(I));
        Inc(I);
      end;
      if not VisibleOnly or (Coord < Info.ClientExtent) then
      begin
        if I = Info.FixedCellCount then
        begin
          Dec(Coord, Info.ScrollOffset);
          I := Info.FirstGridCell;
          while not VisibleOnly and (Cell < I) and (I > Info.FixedCellCount) do
          begin
            Dec(I);
            Dec(Coord, Info.CellExtent(I) + Info.EffectiveSpacing(I));
          end;
          while (I < Cell) and (I < Info.TotalCellCount) and (not VisibleOnly or (Coord < Info.ClientExtent)) do
          begin
            Inc(Coord, Info.CellExtent(I) + Info.EffectiveSpacing(I));
            Inc(I);
          end;
        end;
        Result := Cell = I;
        if Result then
        begin
          while (I >= 0) and (Info.CellExtent(I) = 0) do Dec(I);
          if I < Cell - 1 then
            Dec(Coord, Info.EffectiveSpacing(I + 1));
        end;
      end;
    end;
  end;

begin
  if ColValid(ACol) and RowValid(ARow) then
  begin
    Result := Axis(GetAxisInfoHorz([]), ACol, Point.X);
    if Result then
      Result := Axis(GetAxisInfoVert([]), ARow, Point.Y);
  end else
    Result := False;
end;

function TKCustomGrid.CellVisible(ACol, ARow: Integer): Boolean;
begin
  Result := CellInGridRect(ACol, ARow, VisibleGridRect);
end;

procedure TKCustomGrid.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, FEditorCell.Col, FEditorCell.Row);
end;

procedure TKCustomGrid.ChangeDataSize(ColInsert: Boolean; ColAt, ColCnt: Integer;
  RowInsert: Boolean; RowAt, RowCnt: Integer);

  procedure Axis(var Data: TKGridAxisItems; AxisItemClass: TKGridAxisItemClass;
    Insert: Boolean; DefFixedCnt: Integer; var At, Cnt, MaxLen, ItemCount, FixedCount: Integer);
  var
    I, Len: Integer;
  begin
    if Cnt > 0 then
    begin
      Len := Length(Data);
      if Insert then
      begin
        At := MinMax(At, 0, Len);
        SetLength(Data, Len + Cnt);
        for I := Len - 1 downto At do Data[I + Cnt] := Data[I];
        for I := At to At + Cnt - 1 do
        begin
          Data[I] := AxisItemClass.Create(Self);
          Data[I].InitialPos := MaxLen + 1;
          Inc(MaxLen);
        end;
        if FixedCount < 0 then
          FixedCount := DefFixedCnt
        else if At < FixedCount then
          Inc(FixedCount, Cnt);
      end
      else if Len > 0 then
      begin
        At := MinMax(At, 0, Len - 1);
        Cnt := Min(Cnt, Len - At);
        if Cnt > 0 then
        begin
          for I := At to At + Cnt - 1 do
            Data[I].Free;
          for I := At to Len - Cnt - 1 do Data[I] := Data[I + Cnt];
          SetLength(Data, Len - Cnt);
          if At < FixedCount then
            Dec(FixedCount, FixedCount - At);
        end;
      end;
      ItemCount := Length(Data);
      FixedCount := Min(FixedCount, ItemCount - 1);
    end;
  end;

var
  OldFixedRows, OldFixedCols, I, J, Len: Integer;
  UpdateNeeded: Boolean;
  Reason: TKGridSizeChange;
begin
  EditorMode := False;
  UpdateNeeded := False;
  if not ColInsert then
    ColCnt := Min(ColCnt, FColCount - 1);
  if not RowInsert then
    RowCnt := Min(RowCnt, FRowCount - 1);
  OldFixedCols := FFixedCols;
  Axis(FCols, FColClass, ColInsert, cFixedColsDef,
    ColAt, ColCnt, FMaxCol, FColCount, FFixedCols);
  OldFixedRows := FFixedRows;
  Axis(FRows, FRowClass, RowInsert, cFixedRowsDef,
    RowAt, RowCnt, FMaxRow, FRowCount, FFixedRows);
  FMemCol := cInvalidIndex;
  FMemRow := cInvalidIndex;
  if goVirtualGrid in FOptions then
  begin
    if Assigned(FCells) then
    begin
      for I := 0 to Length(FCells) - 1 do
        for J := 0 to Length(FCells[I]) - 1 do
          FCells[I, J].Free;
      FCells := nil;
      UpdateNeeded := True;
    end;
  end else
  begin
    // take rows first because probably there will be always much more rows
    if RowCnt > 0 then
    begin
      Len := Length(FCells);
      if FRowCount > Len then
      begin
        SetLength(FCells, Len + RowCnt);
        for I := Len - 1 downto RowAt do FCells[I + RowCnt] := FCells[I];
        for I := RowAt to RowAt + RowCnt - 1 do
        begin
          SetLength(FCells[I], FColCount);
          for J := 0 to Length(FCells[I]) - 1 do FCells[I, J] := nil;
        end;
      end else
      begin
        for I := RowAt to RowAt + RowCnt - 1 do
        begin
          for J := 0 to Length(FCells[I]) - 1 do FCells[I, J].Free;
          FCells[I] := nil;
        end;
        for I := RowAt to Len - RowCnt - 1 do FCells[I] := FCells[I + RowCnt];
        SetLength(FCells, Len - RowCnt);
      end;
    end;
    if ColCnt > 0 then
    begin
      for I := 0 to Length(FCells) - 1 do
      begin
        Len := Length(FCells[I]);
        if FColCount > Len then
        begin
          SetLength(FCells[I], Len + ColCnt);
          for J := Len - 1 downto ColAt do FCells[I, J + ColCnt] := FCells[I, J];
          for J := ColAt to ColAt + ColCnt - 1 do FCells[I, J] := nil;
        end
        else if FColCount < Len then
        begin
          for J := ColAt to ColAt + ColCnt - 1 do FCells[I, J].Free;
          for J := ColAt to Len - ColCnt - 1 do FCells[I, J] := FCells[I, J + ColCnt];
          SetLength(FCells[I], Len - ColCnt);
        end;
      end;
    end;
  end;
  if (ColCnt > 0) or (RowCnt > 0) then
  begin
    SelectionFix(FSelection);
    if (FFixedRows <> OldFixedRows) or (FFixedCols <> OldFixedCols) then
      ResetTopLeft;
    UpdateAxes(ColCnt > 0, cAll, RowCnt > 0, cAll, []);
    UpdateCellSpan;
    if ColCnt > 0 then
    begin
      if ColInsert then
      begin
        ClearSortModeVert;
        Reason := scColInserted;
      end else
        Reason := scColDeleted;
      SizeChanged(Reason, ColAt, ColCnt);
    end;
    if RowCnt > 0 then
    begin
      if RowInsert then
      begin
        ClearSortModeHorz;
        Reason := scRowInserted
      end else
        Reason := scRowDeleted;
      SizeChanged(Reason, RowAt, RowCnt);
    end;
  end else if UpdateNeeded then
    Invalidate;
end;

function TKCustomGrid.CheckColDrag(Origin: Integer; var Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckColDrag) then
    FOnCheckColDrag(Self, Origin, Destination, MousePt, Result)
  else if Assigned(FCols) then
    FCols[Destination].CheckDrag(Origin, Destination, MousePt, Result);
  Destination := MinMax(Destination, FFixedCols, FColCount - 1);
end;

function TKCustomGrid.CheckRowDrag(Origin: Integer; var Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckRowDrag) then
    FOnCheckRowDrag(Self, Origin, Destination, MousePt, Result)
  else if Assigned(FRows) then
    FRows[Destination].CheckDrag(Origin, Destination, MousePt, Result);
  Destination := MinMax(Destination, FFixedRows, FRowCount - 1);
end;

function TKCustomGrid.ClampInView(ACol, ARow: Integer): Boolean;
var
  DeltaHorz, DeltaVert: Integer;
begin
  Result := ScrollNeeded(ACol, ARow, DeltaHorz, DeltaVert);
  if Result then
    Scroll(cScrollDelta, cScrollDelta, DeltaHorz, DeltaVert, True);
end;

procedure TKCustomGrid.ClearCol(ACol: Integer);
var
  I: Integer;
begin
  if Assigned(FCells) and ColValid(ACol) then
  begin
    for I := 0 to FRowCount - 1 do
      FreeAndNil(FCells[I, ACol]);
    UpdateCellSpan;
    InvalidateCol(ACol);
  end;
end;

procedure TKCustomGrid.ClearGrid;
var
  I, J: Integer;
begin
  if Assigned(FCells) then
  begin
    for I := 0 to FColCount - 1 do
      for J := 0 to FRowCount - 1 do
        FreeAndNil(FCells[J, I]);
    UpdateCellSpan;
    Invalidate;
  end;
end;

procedure TKCustomGrid.ClearRow(ARow: Integer);
var
  I: Integer;
begin
  if Assigned(FCells) and RowValid(ARow) then
  begin
    for I := 0 to FColCount - 1 do
      FreeAndNil(FCells[ARow, I]);
    UpdateCellSpan;
    InvalidateRow(ARow);
  end;
end;

procedure TKCustomGrid.ClearSortMode;
begin
  ClearSortModeHorz;
  ClearSortModeVert;
end;

procedure TKCustomGrid.ClearSortModeHorz;
var
  OldIndex: Integer;
begin
  if SortModeUnlocked then
  begin
    OldIndex := SortCol;
    if OldIndex >= 0 then
    begin
      FlagSet(cGF_GridUpdates);
      try
        FCols[OldIndex].SortMode := smNone;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      InvalidateGridRect(GridRect(OldIndex, 0, OldIndex, FFixedRows - 1));
    end;
  end;
end;

procedure TKCustomGrid.ClearSortModeVert;
var
  OldIndex: Integer;
begin
  if SortModeUnlocked then
  begin
    OldIndex := SortRow;
    if OldIndex >= 0 then
    begin
      FlagSet(cGF_GridUpdates);
      try
        FRows[OldIndex].SortMode := smNone;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      InvalidateGridRect(GridRect(0, OldIndex, FFixedCols - 1, OldIndex));
    end;
  end;
end;

procedure TKCustomGrid.CMDesignHitTest(var Msg: TLMMouse);
begin
  Msg.Result := Integer(Flag(cGF_DesignHitTest));
end;

procedure TKCustomGrid.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  if not Enabled then EditorMode := False;
  Invalidate;
end;

procedure TKCustomGrid.CMShowingChanged(var Msg: TLMessage);
begin
  inherited;
  if Showing then
    UpdateScrollRange(True, True, False);
end;

procedure TKCustomGrid.CMSysColorChange(var Msg: TLMessage);
begin
  inherited;
  FColors.ClearBrightColors;
end;

procedure TKCustomGrid.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited;
  if not Visible then
    EditorMode := False;
end;

procedure TKCustomGrid.CMWantSpecialKey(var Msg: TLMKey);
begin
  inherited;
  if (goEditing in Options) and (Msg.CharCode in [VK_RETURN, VK_ESCAPE]) then
    Msg.Result := 1;
end;

procedure TKCustomGrid.ColMoved(FromIndex, ToIndex: Integer);
begin
  if Assigned(FOnColMoved) then
    FOnColMoved(Self, FromIndex, ToIndex);
end;

function TKCustomGrid.ColSelectable(ACol: Integer): Boolean;
begin
  Result := (ACol >= FFixedCols) and (ACol < FColCount);
end;

function TKCustomGrid.ColSelected(ACol: Integer): Boolean;
begin
  Result := ColInGridRect(ACol, Selection);
end;

function TKCustomGrid.ColValid(ACol: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < FColCount);
end;

procedure TKCustomGrid.ColWidthsChanged(ACol: Integer);
begin
  if Assigned(FOnColWidthsChanged) then
    FOnColWidthsChanged(Self)
  else if Assigned(FOnColWidthsChangedEx) then
    FOnColWidthsChangedEx(Self, ACol);
end;

function TKCustomGrid.CompareCellInstances(ACell1, ACell2: TKGridCell): Integer;
begin
  if Assigned(FOnCompareCellInstances) then
    Result := FOnCompareCellInstances(Self, ACell1, ACell2)
  else if Assigned(FCells) then
    Result := DefaultCompareCells(ACell1, ACell2)
  else
    Result := 0;
end;

function TKCustomGrid.CompareCells(ACol1, ARow1, ACol2, ARow2: Integer): Integer;
begin
  if Assigned(FOnCompareCells) then
    Result := FOnCompareCells(Self, ACol1, ARow1, ACol2, ARow2)
  else if Assigned(FCells) then
    Result := DefaultCompareCells(InternalGetCell(ACol1, ARow1), InternalGetCell(ACol2, ARow1))
  else
    Result := 0;
end;

function TKCustomGrid.CompareCols(ARow, ACol1, ACol2: Integer): Integer;
begin
  if Assigned(FOnCompareCells) then
    Result := FOnCompareCells(Self, ACol1, ARow, ACol2, ARow)
  else if Assigned(FCells) then
    Result := DefaultCompareCells(InternalGetCell(ACol1, ARow), InternalGetCell(ACol2, ARow))
  else
    Result := 0;
end;

function TKCustomGrid.CompareRows(ACol, ARow1, ARow2: Integer): Integer;
begin
  if Assigned(FOnCompareCells) then
    Result := FOnCompareCells(Self, ACol, ARow1, ACol, ARow2)
  else if Assigned(FCells) then
    Result := DefaultCompareCells(InternalGetCell(ACol, ARow1), InternalGetCell(ACol, ARow2))
  else
    Result := 0;
end;

procedure TKCustomGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    if HasHorzScrollBar then Style := Style or WS_HSCROLL;
    if HasVertScrollBar then Style := Style or WS_VSCROLL;
  end;
end;

function TKCustomGrid.CustomSortCols(ByRow: Integer;
  var SortMode: TKGridSortMode): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomSortCols) then FOnCustomSortCols(Self, ByRow, SortMode, Result);
end;

function TKCustomGrid.CustomSortRows(ByCol: Integer;
  var SortMode: TKGridSortMode): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomSortRows) then FOnCustomSortRows(Self, ByCol, SortMode, Result);
end;

procedure TKCustomGrid.DefaultColWidthChanged;
var
  I: Integer;
begin
  FlagSet(cGF_GridUpdates);
  try
    for I := 0 to FColCount - 1 do FCols[I].Extent := FDefaultColWidth;
  finally
    FlagClear(cGF_GridUpdates);
  end;
  UpdateAxes(True, cAll, False, cAll, [afCheckMinExtent]);
end;

procedure TKCustomGrid.DefaultComboKeyPreview(AEditor: TComboBox;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  if Key in [VK_RETURN, VK_ESCAPE, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT] then
  begin
    if AEditor.DroppedDown then
      IsGridKey := False;
  end
  else if AEditor.Style in [csSimple, csDropDown] then
  begin
    // we have a combo box with edit control
    DoEditKeyPreview(StringLength(AEditor.Text), AEditor.SelStart, AEditor.SelLength, 1, False, True, True,
      Key, ShiftState, IsGridKey);
  end;
end;

procedure TKCustomGrid.DefaultComboSelect(AEditor: TComboBox; SelectAll,
  CaretToLeft: Boolean);
begin
  if AEditor.Style in [csSimple, csDropDown] then
  begin
    // we have a combo box with edit control
    if SelectAll then
      AEditor.SelectAll
    else
    begin
      AEditor.SelLength := 0;
      if CaretToLeft then
        AEditor.SelStart := 0
      else
        AEditor.SelStart := StringLength(AEditor.Text);
    end;
  end;
end;

procedure TKCustomGrid.DefaultEditKeyPreview(AEditor: TCustomEdit;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
var
  MultiLine, StartLine, EndLine: Boolean;
  TextLen, LineCount: Integer;
begin
  TextLen := StringLength(AEditor.Text);
  if AEditor is TCustomMemo then
  begin
    MultiLine := True;
    LineCount := TCustomMemo(AEditor).Lines.Count;
    StartLine := AEditor.SelStart < StringLength(TCustomMemo(AEditor).Lines[0]);
    EndLine := AEditor.SelStart > TextLen - StringLength(TCustomMemo(AEditor).Lines[TCustomMemo(AEditor).Lines.Count - 1]);
  end else
  begin
    MultiLine := False;
    StartLine := True;
    EndLine := True;
    LineCount := 1;
  end;
  DoEditKeyPreview(StringLength(AEditor.Text), AEditor.SelStart, AEditor.SelLength, LineCount, MultiLine, StartLine, EndLine,
    Key, ShiftState, IsGridKey);
end;

procedure TKCustomGrid.DefaultEditorCreate(ACol, ARow: Integer;
  var AEditor: TWinControl);
begin
  AEditor := TEdit.Create(nil);
end;

procedure TKCustomGrid.DefaultEditorDataFromGrid(AEditor: TWinControl;
  ACol, ARow: Integer; var AssignText: Boolean);
begin
  // empty
end;

procedure TKCustomGrid.DefaultEditorDataToGrid(AEditor: TWinControl;
  ACol, ARow: Integer; var AssignText: Boolean);
begin
  // empty
end;

procedure TKCustomGrid.DefaultEditorDestroy(AEditor: TWinControl; ACol,
  ARow: Integer);
begin
  // empty
end;

procedure TKCustomGrid.DefaultEditorKeyPreview(AEditor: TWinControl;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  if AEditor is TCustomEdit then
    DefaultEditKeyPreview(TCustomEdit(AEditor), ACol, ARow, Key, ShiftState, IsGridKey)
  else if AEditor is TCustomComboBox then
    DefaultComboKeyPreview(TComboBox(AEditor), ACol, ARow, Key, ShiftState, IsGridKey)
end;

procedure TKCustomGrid.DefaultEditorResize(AEditor: TWinControl;
  ACol, ARow: Integer; var ARect: TRect);
begin
  // empty
end;

procedure TKCustomGrid.DefaultEditorSelect(AEditor: TWinControl;
  ACol, ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean);
begin
  if AEditor is TCustomEdit then
    DefaultEditSelect(TCustomEdit(AEditor), SelectAll, CaretToLeft)
  else if AEditor is TCustomComboBox then
    DefaultComboSelect(TComboBox(AEditor), SelectAll, CaretToLeft);
end;

procedure TKCustomGrid.DefaultEditSelect(AEditor: TCustomEdit; SelectAll,
  CaretToLeft: Boolean);
begin
  if SelectAll then
    AEditor.SelectAll
  else
  begin
    if CaretToLeft then
      AEditor.SelStart := 0
    else
      AEditor.SelStart := StringLength(AEditor.Text);
    AEditor.SelLength := 0;
  end;
end;

function TKCustomGrid.DefaultCompareCells(ACell1, ACell2: TKGridCell): Integer;
var
{$IFDEF STRING_IS_UNICODE}
  S1, S2: string;
{$ELSE}
  W1, W2: PWideChar;
{$ENDIF}
begin
{$IFDEF STRING_IS_UNICODE}
  if ACell1 is TKGridTextCell then S1 := TKGridTextCell(ACell1).Text else S1 := '';
  if ACell2 is TKGridTextCell then S2 := TKGridTextCell(ACell2).Text else S2 := '';
  Result := CompareStrings(S1, S2);
{$ELSE}
  if ACell1 is TKGridTextCell then W1 := TKGridTextCell(ACell1).TextPtr else W1 := '';
  if ACell2 is TKGridTextCell then W2 := TKGridTextCell(ACell2).TextPtr else W2 := '';
  Result := CompareWideChars(W1, W2);
{$ENDIF}
end;

procedure TKCustomGrid.DefaultMouseCellHint(ACol, ARow: Integer;
  AShow: Boolean);
var
  R: TRect;
  Extent: TPoint;
  AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  if ColValid(ACol) and Cols[ACol].CellHint then
  begin
    if AShow then
    begin
      AText := Cells[ACol, ARow];
      if (AText <> '') and (ARow >= FFixedRows) and
        ((ARow <> FEditorCell.Row) or (ACol <> FEditorCell.Col) or not EditorMode) and
        CellRect(ACol, ARow, R, True) then
      begin
        Extent := MeasureCell(ACol, ARow, R, GetDrawState(ACol, ARow, HasFocus), mpCellExtent);
        if (Extent.X > R.Right - R.Left) or (Extent.Y > R.Bottom - R.Top) then
        begin
          FreeAndNil(FHint);
          FHint := TKTextHint.Create(nil);
          TKTextHint(FHint).Text := AText;
          Inc(R.Left, 10);
          Inc(R.Top, 10);
          FHint.ShowAt(ClientToScreen(R.TopLeft));
        end;
      end;
    end else
      FreeAndNil(FHint);
  end else
    FreeAndNil(FHint);
end;

procedure TKCustomGrid.DefaultRowHeightChanged;
var
  I: Integer;
begin
  FlagSet(cGF_GridUpdates);
  try
    for I := 0 to FRowCount - 1 do FRows[I].Extent := FDefaultRowHeight;
  finally
    FlagClear(cGF_GridUpdates);
  end;
  UpdateAxes(False, cAll, True, cAll, [afCheckMinExtent]);
end;

procedure TKCustomGrid.DefaultScrollBarKeyPreview(AEditor: TScrollBar;
  ACol, ARow: Integer; var Key: Word; ShiftState: TShiftState; var IsGridKey: Boolean);
begin
  if (Key = VK_LEFT) and (AEditor.Position > AEditor.Min) or
    (Key = VK_RIGHT) and (AEditor.Position < AEditor.Max) then
    IsGridKey := False;
end;

procedure TKCustomGrid.DefaultSetCaretToLeft(Key: Word; ShiftState: TShiftState);
begin
  if (Key in [VK_DOWN, VK_NEXT]) or (Key in [VK_RIGHT, VK_END]) and (Col < FColCount - 1) then
    FlagSet(cGF_CaretToLeft);
end;

procedure TKCustomGrid.DefineProperties(Filer: TFiler);

  function DoColData: Boolean;
  begin
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TKCustomGrid) then
      Result := not CompareAxisItems(TKCustomGrid(Filer.Ancestor).FCols, FCols)
    else
      Result := FCols <> nil;
  end;

  function DoRowData: Boolean;
  begin
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TKCustomGrid) then
      Result := not CompareAxisItems(TKCustomGrid(Filer.Ancestor).FRows, FRows)
    else
      Result := FRows <> nil;
  end;

begin
  inherited;
  with Filer do
  begin
    DefineProperty('ColWidths', ReadColWidths, WriteColWidths, DoColData);
    DefineProperty('RowHeights', ReadRowHeights, WriteRowHeights, DoRowData);
  end;
end;

procedure TKCustomGrid.DeleteCol(At: Integer);
begin
  DeleteCols(At, 1);
end;

procedure TKCustomGrid.DeleteCols(At, Count: Integer);
begin
  if ColValid(At) and (FColCount > 1) then
  begin
    Count := Min(Count, FColCount - Max(At, 1));
    ChangeDataSize(False, At, Count, False, 0, 0);
  end;
end;

procedure TKCustomGrid.DeleteRow(At: Integer);
begin
  DeleteRows(At, 1);
end;

procedure TKCustomGrid.DeleteRows(At, Count: Integer);
begin
  if RowValid(At) and (FRowCount > 1) then
  begin
    Count := Min(Count, FRowCount - Max(At, 1));
    ChangeDataSize(False, 0, 0, False, At, Count);
  end;
end;

function TKCustomGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  Key: Word;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    Key := VK_DOWN;
    KeyDown(Key, []);
    Result := True;
  end;
end;

function TKCustomGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  Key: Word;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    Key := VK_UP;
    KeyDown(Key, []);
    Result := True;
  end;
end;

procedure TKCustomGrid.DragMove(ACol, ARow: Integer; MousePt: TPoint);
begin
  case FGridState of
    gsColMoving: if CheckColDrag(FDragOrigin, ACol, MousePt) and (FDragDest <> ACol) then
    begin
      SuggestDrag(csHide);
      FDragDest := ACol;
      SuggestDrag(csShow);
    end;
    gsRowMoving: if CheckRowDrag(FDragOrigin, ARow, MousePt) and (FDragDest <> ARow) then
    begin
      SuggestDrag(csHide);
      FDragDest := ARow;
      SuggestDrag(csShow);
    end;
  end;
end;

function TKCustomGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TKGridDrawState): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACol, ARow, ARect, AState)
  else if Assigned(FCells) then with InternalGetCell(ACol, ARow) do
  begin
    ApplyDrawProperties;
    DrawCell(ACol, ARow, ARect, AState)
  end else
    Result := False;
end;

function TKCustomGrid.EditorCreate(ACol, ARow: Integer): TWinControl;
begin
  Result := nil;
  if Assigned(FOnEditorCreate) then
    FOnEditorCreate(Self, ACol, ARow, Result)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorCreate(ACol, ARow, Result)
  else
    DefaultEditorCreate(ACol, ARow, Result);
end;

procedure TKCustomGrid.EditorDataFromGrid(AEditor: TWinControl; ACol, ARow: Integer);
var
  AssignText: Boolean;
begin
  AssignText := True;
  if Assigned(FOnEditorDataFromGrid) then
    FOnEditorDataFromGrid(Self, AEditor, ACol, ARow, AssignText)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorDataFromGrid(AEditor, ACol, ARow, AssignText)
  else
    DefaultEditorDataFromGrid(AEditor, ACol, ARow, AssignText);
  if AssignText then
    SetControlText(AEditor, Cells[ACol, ARow]);
end;

procedure TKCustomGrid.EditorDataToGrid(AEditor: TWinControl; ACol, ARow: Integer);
var
  AssignText: Boolean;
begin
  AssignText := True;
  if Assigned(FOnEditorDataToGrid) then
    FOnEditorDataToGrid(Self, AEditor, ACol, ARow, AssignText)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorDataToGrid(AEditor, ACol, ARow, AssignText)
  else
    DefaultEditorDataToGrid(AEditor, ACol, ARow, AssignText);
  if AssignText then
    Cells[ACol, ARow] := GetControlText(AEditor);
end;

procedure TKCustomGrid.EditorDestroy(var AEditor: TWinControl; ACol, ARow: Integer);
begin
  if Assigned(FOnEditorDestroy) then
    FOnEditorDestroy(Self, AEditor, ACol, ARow)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorDestroy(AEditor, ACol, ARow)
  else
    DefaultEditorDestroy(AEditor, ACol, ARow);
end;

function TKCustomGrid.EditorIsTransparent: Boolean;
begin
  Result := False;
  if FEditorTransparency = etTransparent then
    Result := True
  else if FEditorTransparency = etDefault then
  begin
    { Default behavior. For example TCheckBox is not meant to be transparent
      by VCL/LCL but from grid's point of view it should be. }
    Result :=
      (FEditor is TCustomCheckBox) or
      (FEditor is TRadioButton) or
      (FEditor is TStaticText);
  end;
end;

function TKCustomGrid.EditorKeyPreview(AEditor: TWinControl; ACol, ARow: Integer;
  var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  if Assigned(FOnEditorKeyPreview) then
    FOnEditorKeyPreview(Self, AEditor, ACol, ARow, Key, Shift, Result)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorKeyPreview(AEditor, ACol, ARow, Key, Shift, Result)
  else
    DefaultEditorKeyPreview(AEditor, ACol, ARow, Key, Shift, Result);
end;

procedure TKCustomGrid.EditorResize(AEditor: TWinControl; ACol, ARow: Integer;
  var ARect: TRect);
begin
  if Assigned(FOnEditorResize) then
    FOnEditorResize(Self, AEditor, ACol, ARow, ARect)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorResize(AEditor, ACol, ARow, ARect)
  else
    DefaultEditorResize(AEditor, ACol, ARow, ARect);
end;

procedure TKCustomGrid.EditorSelect(AEditor: TWinControl; ACol, ARow: Integer;
  SelectAll, CaretToLeft, SelectedByMouse: Boolean);
begin
  if Assigned(FOnEditorSelect) then
    FOnEditorSelect(Self, AEditor, ACol, ARow, SelectAll, CaretToLeft, SelectedByMouse)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).EditorSelect(AEditor, ACol, ARow, SelectAll, CaretToLeft, SelectedByMouse)
  else
    DefaultEditorSelect(AEditor, ACol, ARow, SelectAll, CaretToLeft, SelectedByMouse);
end;

procedure TKCustomGrid.EditorWindowProc(var Msg: TLMessage);

  procedure PaintCellBackground(DC: HDC);
  var
    SaveIndex: Integer;
    ACanvas: TCanvas;
    R, TmpBlockRect: TRect;
  begin
    if CellRect(Col, Row, R) then
    begin
      ACanvas := TCanvas.Create;
      SaveIndex := SaveDC(DC);
      try
        ACanvas.Handle := DC;
        R := Rect(0, 0, R.Right - R.Left, R.Bottom - R.Top);
        TmpBlockRect := SelectionRect;
        OffsetRect(TmpBlockRect, -R.Left, -R.Top);
        InternalPaintCell(Col, Row, GetDrawState(Col, Row, HasFocus),
          R, TmpBlockRect, ACanvas, False, False);
        FEditor.Brush.Color := ACanvas.Brush.Color;
      finally
        RestoreDC(DC, SaveIndex);
        ACanvas.Free;
      end;
    end;
  end;

  procedure GotFocus;
  begin
    InvalidateCurrentSelection;
  end;

  procedure LostFocus;
  begin
    InvalidateCurrentSelection;
  end;

var
  Key: Word;
  Shift: TShiftState;
  CallDefault: Boolean;
  Form: TCustomForm;
begin
  CallDefault := True;
  case Msg.Msg of
    CM_MOUSEENTER, CM_MOUSELEAVE: // not called if editor is captured
    try
      MouseOverCells; // some win32 error might popup here
    except
    end;
    CN_CHAR:
      ClampInView(FEditorCell.Col, FEditorCell.Row);
  {$IFNDEF FPC}
    CN_COMMAND:
      if TWMCommand(Msg).Ctl = FEditor.Handle then
      begin
        case TWMCommand(Msg).NotifyCode of
          CBN_KILLFOCUS, BN_KILLFOCUS, LBN_KILLFOCUS, EN_KILLFOCUS: LostFocus;
          CBN_SETFOCUS, BN_SETFOCUS, EN_SETFOCUS: GotFocus;
        end;
      end;
  {$ELSE}
    LM_ERASEBKGND:
    begin
      if EditorIsTransparent then
      begin
        PaintCellBackground(TLMEraseBkGnd(Msg).DC);
        CallDefault := False;
        Msg.Result := 1;
      end;
    end;
  {$ENDIF}
    { CN_KEYDOWN is sent from TApplication.IsKeyMsg as 'preview' so this message
      is used as KeyPreview. WM_KEYDOWN is not sent here by all inplace editors as
      some of them might have another child window with input focus
      (in such cases WM_KEYDOWN is sent directly to it). But if it is sent here
      so let's decide if it can be processed by inplace editor, either. }
    CN_KEYDOWN, LM_KEYDOWN:
    begin
      FKeyPreview := Msg.Msg = CN_KEYDOWN;
      Key := TLMKey(Msg).CharCode;
      Shift := KeyDataToShiftState(TLMKey(Msg).KeyData);
      case Key of
        VK_RETURN, VK_ESCAPE, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
        VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_TAB:
        begin
          if EditorKeyPreview(FEditor, FEditorCell.Col, FEditorCell.Row, Key, Shift) then
          begin
            DefaultSetCaretToLeft(Key, Shift);
            if Msg.Msg = CN_KEYDOWN then
              PostLateUpdate(Msg)
            else
              ClampInView(FEditorCell.Col, FEditorCell.Row);
            if (Key <> VK_TAB) or (goTabs in FOptions) then
            begin
              CallDefault := False;
              Msg.Result := 1;
            end;
          end;
        end else
          ClampInView(FEditorCell.Col, FEditorCell.Row);
      end;
    end;
    LM_GETDLGCODE: if goTabs in FOptions then
      Msg.Result := Msg.Result or DLGC_WANTTAB or DLGC_WANTARROWS;
    LM_KILLFOCUS:
      LostFocus;
    LM_MOUSEMOVE:
    begin
      if Flag(cGF_ThroughClick) and (GetCaptureControl = FEditor) then
      begin
        if (FGridState = gsSelecting) and not PtInRect(FEditor.BoundsRect, ScreenToClient(Mouse.CursorPos)) then
        begin
          MouseCapture := True;
          FlagClear(cGF_ThroughClick);
        end;
        MouseOverCells;
      end;
    end;
    LM_LBUTTONUP:
    begin
      if Flag(cGF_ThroughClick) then
      begin
        FGridState := gsNormal;
        FlagClear(cGF_ThroughClick);
      end;
    end;
    LM_SETFOCUS:
    begin
      Form := GetParentForm(Self);
      if Assigned(Form) and not (csDestroying in Form.ComponentState) then
        GotFocus
      else
        CallDefault := False; // eat the message to avoid an exception in LCL (TForm.SetFocusedControl)
    end;
  end;
  if CallDefault then
    FEditorWindowProc(Msg);
end;

function TKCustomGrid.EndColDrag(Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnEndColDrag) then
    FOnEndColDrag(Self, Origin, Destination, MousePt, Result)
  else if Assigned(FCols) then
    FCols[Destination].EndDrag(Origin, Destination, MousePt, Result)
end;

function TKCustomGrid.EndColSizing(var Index, Pos: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnEndColSizing) then
    FOnEndColSizing(Self, Index, Pos, Result);
  Index := MinMax(Index, 0, FColCount - 1);
end;

function TKCustomGrid.EndRowDrag(Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := True;
  if Assigned(FOnEndRowDrag) then
    FOnEndRowDrag(Self, Origin, Destination, MousePt, Result)
  else if Assigned(FRows) then
    FRows[Destination].EndDrag(Origin, Destination, MousePt, Result)
end;

function TKCustomGrid.EndRowSizing(var Index, Pos: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnEndRowSizing) then
    FOnEndRowSizing(Self, Index, Pos, Result);
  Index := MinMax(Index, 0, FRowCount - 1);
end;

procedure TKCustomGrid.FindBaseCell(ACol, ARow: Integer; out BaseCol,
  BaseRow: Integer);
begin
  if ColValid(ACol) and RowValid(ARow) then
    InternalFindBaseCell(ACol, ARow, BaseCol, BaseRow);
end;

procedure TKCustomGrid.FocusCell(ACol, ARow: Integer);
begin
  if ColValid(ACol) and RowValid(ARow) then
  begin
    InternalFindBaseCell(ACol, ARow, ACol, ARow);
    if (Col <> ACol) or (Row <> ARow) then
      CellChanging(Col, Row, ACol, ARow);
    if SelectionMove(ACol, ARow, ssInit, [sfMustUpdate, sfClampInView]) then
      Click;
  end;
end;

procedure TKCustomGrid.FreeData;
var
  I, J: Integer;
begin
  for I := 0 to FColCount - 1 do
    FCols[I].Free;
  FCols := nil;
  for I := 0 to FRowCount - 1 do
    FRows[I].Free;
  FRows := nil;
  for I := 0 to Length(FCells) - 1 do
    for J := 0 to Length(FCells[I]) - 1 do
      FCells[I, J].Free;
  FCells := nil;
end;

function TKCustomGrid.GetAllCellsSelected: Boolean;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  Result := (R.Col1 = FFixedCols) and (R.Col2 = FColCount - 1) and
    (R.Row1 = FFixedRows) and (R.Row2 = FRowCount - 1);
end;

function TKCustomGrid.GetAllColsSelected: Boolean;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  Result := (R.Row1 = FFixedRows) and (R.Row2 = FRowCount - 1);
end;

function TKCustomGrid.GetAllRowsSelected: Boolean;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  Result := (R.Col1 = FFixedCols) and (R.Col2 = FColCount - 1);
end;

procedure TKCustomGrid.GetAxisInfo(var Info: TKGridAxisInfo);
var
  I, Extent: Integer;
begin
  with Info do
  begin
    if InfoMask * [aiFixedParams, aiFullVisBoundary, aiGridBoundary, aiGridExtent] <> [] then
    begin
      FixedBoundary := 0;
      I := 0;
      while I < FixedCellCount do
      begin
        Inc(FixedBoundary, CellExtent(I) + EffectiveSpacing(I));
        Inc(I);
      end;
    end;
    if aiGridExtent in InfoMask then
    begin
      I := FixedCellCount;
      GridExtent := FixedBoundary;
      while I < TotalCellCount do
      begin
        Inc(GridExtent, Int64(CellExtent(I)) + EffectiveSpacing(I));
        Inc(I);
      end;
    end;
    if aiGridBoundary in InfoMask then
    begin
      GridCells := FirstGridCell;
      GridBoundary := FixedBoundary - ScrollOffset;
      while (GridCells < TotalCellCount) and (GridBoundary < ClientExtent) do
      begin
        Inc(GridBoundary, CellExtent(GridCells) + EffectiveSpacing(GridCells));
        Inc(GridCells);
      end;
      GridBoundary := Min(GridBoundary, ClientExtent);
      GridCells := Min(GridCells, TotalCellCount);
    end;
    if aiFullVisBoundary in InfoMask then
    begin
      FullVisCells := FirstGridCell;
      FullVisBoundary := FixedBoundary - ScrollOffset;
      while FullVisCells < TotalCellCount do
      begin
        Extent := CellExtent(FullVisCells) + EffectiveSpacing(FullVisCells);
        if FullVisBoundary + Extent <= ClientExtent then
        begin
          Inc(FullVisBoundary, Extent);
          Inc(FullVisCells);
        end else
          Break;
      end;
      FullVisCells := Min(FullVisCells, TotalCellCount);
    end;
  end;
end;

function TKCustomGrid.GetAxisInfoBoth(Mask: TKGridAxisInfoMask): TKGridAxisInfoBoth;
begin
  Result.Horz := GetAxisInfoHorz(Mask);
  Result.Vert := GetAxisInfoVert(Mask);
end;

function TKCustomGrid.GetAxisInfoHorz(Mask: TKGridAxisInfoMask): TKGridAxisInfo;
begin
  with Result do
  begin
    AlignLastCell := goAlignLastCol in FOptions;
    CanResize := BeginColSizing;
    CellExtent := InternalGetColWidths;
    EffectiveSpacing := InternalGetEffectiveColSpacing;
    FixedCellCount := FFixedCols;
    FixedSelectable := gxEditFixedCols in FOptionsEx;
    FirstGridCell := FTopLeft.Col;
    FirstGridCellExtent := FTopLeftExtent.Col;
    if HandleAllocated then
      ClientExtent := ClientWidth
    else
      // don't create Handle, fake ClientWidth instead
      ClientExtent := Width;
    MinCellExtent := InternalGetMinColWidth;
    MaxCellExtent := InternalGetMaxColWidth;
    TotalCellCount := FColCount;
    ScrollOffset := FScrollOffset.X;
    InfoMask := Mask;
  end;
  GetAxisInfo(Result);
end;

function TKCustomGrid.GetAxisInfoVert(Mask: TKGridAxisInfoMask): TKGridAxisInfo;
begin
  with Result do
  begin
    AlignLastCell := goAlignLastRow in FOptions;
    CanResize := BeginRowSizing;
    CellExtent := InternalGetRowHeights;
    EffectiveSpacing := InternalGetEffectiveRowSpacing;
    FixedCellCount := FFixedRows;
    FixedSelectable := gxEditFixedRows in FOptionsEx;
    FirstGridCell := FTopLeft.Row;
    FirstGridCellExtent := FTopLeftExtent.Row;
    if HandleAllocated then
      ClientExtent := ClientHeight
    else
      // don't create Handle, fake ClientWidth instead
      ClientExtent := Height;
    MinCellExtent := InternalGetMinRowHeight;
    MaxCellExtent := InternalGetMaxRowHeight;
    TotalCellCount := FRowCount;
    ScrollOffset := FScrollOffset.Y;
    InfoMask := Mask;
  end;
  GetAxisInfo(Result);
end;

function TKCustomGrid.GetCell(ACol, ARow: Integer): TKGridCell;
begin
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
    Result := InternalGetCell(ACol, ARow)
  else
    Result := nil;
end;

function TKCustomGrid.GetCells(ACol, ARow: Integer): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
var
  Data: TKGridCell;
begin
  Result := '';
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
  begin
    Data := InternalGetCell(ACol, ARow);
    if Data is TKGridTextCell then
      Result := TKGridTextCell(Data).Text;
  end;
end;

function TKCustomGrid.GetCellSpan(ACol, ARow: Integer): TKGridCellSpan;
begin
  if ColValid(ACol) and RowValid(ARow) then
    Result := InternalGetCellSpan(ACol, ARow)
  else
    Result := MakeCellSpan(1, 1);
end;

function TKCustomGrid.GetCols(Index: Integer): TKGridCol;
begin
  if ColValid(Index) and (FCols[Index] is TKGridCol) then
    Result := TKGridCol(FCols[Index])
  else
    Result := nil;
end;

function TKCustomGrid.GetColWidths(Index: Integer): Integer;
begin
  if ColValid(Index) then
    Result := FCols[Index].Extent
  else
    Result := 0
end;

function TKCustomGrid.GetDefaultDrawing: Boolean;
begin
  Result := False;
end;

function TKCustomGrid.GetDragRect(Info: TKGridAxisInfoBoth; out DragRect: TRect): Boolean;
var
  W, H, ES: Integer;
  P: TPoint;
begin
  Result := False;
  if FGridState = gsColMoving then
  begin
    if CellToPoint(FDragDest, 0, P) then
    begin
      if FDragDest > FDragOrigin then
      begin
        ES := Info.Horz.EffectiveSpacing(FDragDest);
        Inc(P.X, Info.Horz.CellExtent(FDragDest));
      end else
      begin
        if FDragDest > 0 then
          ES := Info.Horz.EffectiveSpacing(FDragDest - 1)
        else
          ES := 0;
        Dec(P.X, ES);
      end;
      case FDragStyle of
        dsLayeredConst, dsLayeredFaded:
        begin
          W := FDragArrow.Width;
          H := Min(Info.Vert.FixedBoundary, Info.Vert.ClientExtent);
        end;
      else
        W := 5;
        H := Info.Vert.GridBoundary;
      end;
      Dec(P.X, (W - ES) shr 1);
      DragRect := Rect(P.X, 0, P.X + W, H);
      Result := True;
    end;
  end else
  begin
    if CellToPoint(0, FDragDest, P) then
    begin
      if FDragDest >= FDragOrigin then
      begin
        ES := Info.Vert.EffectiveSpacing(FDragDest);
        Inc(P.Y, Info.Vert.CellExtent(FDragDest));
      end else
      begin
        if FDragDest > 0 then
          ES := Info.Vert.EffectiveSpacing(FDragDest - 1)
        else
          ES := 0;
        Dec(P.Y, ES);
      end;
      case FDragStyle of
        dsLayeredConst, dsLayeredFaded:
        begin
          W := Min(Info.Horz.FixedBoundary, Info.Horz.ClientExtent);
          H := FDragArrow.Height;
        end;
      else
        W := Info.Horz.GridBoundary;
        H := 5;
      end;
      Dec(P.Y, (H - ES) shr 1);
      DragRect := Rect(0, P.Y, W, P.Y + H);
      Result := True;
    end;
  end;
end;

function TKCustomGrid.GetDrawState(ACol, ARow: Integer; AFocused: Boolean): TKGridDrawState;
var
  BaseCol, BaseRow: Integer;
begin
  Result := [];
  if ColValid(ACol) and RowValid(ARow) then
  begin
    if (ACol < FFixedCols) or (ARow < FFixedRows) then
    begin
      Result := [gdFixed];
      if (goRowSorting in FOptions) and (ARow = FCols[ACol].SortArrowIndex) then
        if FCols[ACol].SortMode = smDown then
          Include(Result, gdRowsSortedDown)
        else if FCols[ACol].SortMode = smUp then
          Include(Result, gdRowsSortedUp);
      if (goColSorting in FOptions) and (ACol = FRows[ARow].SortArrowIndex) and
        ((ARow > 0) or not (goRowSorting in FOptions)) then
        if (FRows[ARow].SortMode = smDown) then
          Include(Result, gdColsSortedDown)
        else if FRows[ARow].SortMode = smUp then
          Include(Result, gdColsSortedUp);
      //aki:
      if (((gxEditFixedRows in FOptionsEx) and (ARow < FFixedRows)) or
        ((gxEditFixedCols in FOptionsEx) and (ACol < FFixedCols))) and
        CellSelected(ACol, ARow) then
        begin
          Include(Result, gdSelected);
          if (ACol = FSelection.Col1) and (ARow = FSelection.Row1) then
          begin
            if EditorMode and (FEditor.Left >= 0) and (FEditor.Top >= 0) then
              Include(Result, gdEdited)
            else if AFocused then
              Include(Result, gdFocused);
          end;
        end;
    end else
    begin
      if CellSelected(ACol, ARow) then
      begin
        Result := [gdSelected];
        if (ACol = FSelection.Col1) and (ARow = FSelection.Row1) then
        begin
          if EditorMode and (FEditor.Left >= 0) and (FEditor.Top >= 0) then
            Include(Result, gdEdited)
          else if AFocused then
            Include(Result, gdFocused);
        end;
      end else
        Result := [];
      if (FCols[ACol].SortMode <> smNone) or (FRows[ARow].SortMode <> smNone) then
        Include(Result, gdSorted);
    end;
    if (FGridState in [gsNormal, gsSelecting, gsColMoveWaiting, gsRowMoveWaiting,
      gsColSortWaiting, gsRowSortWaiting]) and not (csDesigning in ComponentState) then
    begin
      InternalFindBaseCell(ACol, ARow, BaseCol, BaseRow);
      if (ACol = FMouseOver.Col) and (ARow = FMouseOver.Row) then
      begin
        if MouseCapture and ColValid(FHitCell.Col) and RowValid(FHitCell.Row) then
        begin
          InternalFindBaseCell(FHitCell.Col, FHitCell.Row, BaseCol, BaseRow);
          if (BaseCol = ACol) and (BaseRow = ARow) then
            Include(Result, gdMouseDown);
        end;
        if goMouseOverCells in FOptions then
          Include(Result, gdMouseOver);
      end;
    end;
  end;
end;

function TKCustomGrid.GetEditorMode: Boolean;
begin
  Result := Assigned(FEditor);
end;

function TKCustomGrid.GetEffectiveColSpacing(ACol: Integer): Integer;
begin
  if ColValid(ACol) then
    Result := InternalGetEffectiveColSpacing(ACol)
  else
    Result := 0;
end;

function TKCustomGrid.GetEffectiveRowSpacing(ARow: Integer): Integer;
begin
  if RowValid(ARow) then
    Result :=  InternalGetEffectiveRowSpacing(ARow)
  else
    Result := 0;
end;

function TKCustomGrid.GetEntireColSelected(Index: Integer): Boolean;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  Result := (R.Row1 = FFixedRows) and (R.Row2 = FRowCount - 1) and
    (R.Col1 <= Index) and (Index <= R.Col2);
end;

function TKCustomGrid.GetEntireRowSelected(Index: Integer): Boolean;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  Result := (R.Col1 = FFixedCols) and (R.Col2 = FColCount - 1) and
    (R.Row1 <= Index) and (Index <= R.Row2);
end;

function TKCustomGrid.GetEntireSelectedColCount: Integer;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  if (R.Row1 = FFixedRows) and (R.Row2 = FRowCount - 1) then
    Result := R.Col2 - R.Col1
  else
    Result := 0;
end;

function TKCustomGrid.GetEntireSelectedRowCount: Integer;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  if (R.Col1 = FFixedCols) and (R.Col2 = FColCount - 1) then
    Result := R.Row2 - R.Row1
  else
    Result := 0;
end;

function TKCustomGrid.GetGridHeight: Integer;
begin
  Result := GetAxisInfoVert([aiGridBoundary]).GridBoundary;
end;

function TKCustomGrid.GetGridWidth: Integer;
begin
  Result := GetAxisInfoHorz([aiGridBoundary]).GridBoundary;
end;

function TKCustomGrid.GetLastVisibleCol: Integer;
begin
  Result := GetAxisInfoHorz([aiGridBoundary]).GridCells - 1;
end;

function TKCustomGrid.GetLastVisibleRow: Integer;
begin
  Result := GetAxisInfoVert([aiGridBoundary]).GridCells - 1;
end;

function TKCustomGrid.GetMoreCellsSelected: Boolean;
begin
  Result := (FSelection.Row1 <> FSelection.Row2) or
    (FSelection.Col1 <> FSelection.Col2);
end;

function TKCustomGrid.GetObjects(ACol, ARow: Integer): TObject;
var
  Data: TKGridCell;
begin
  Result := nil;
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
  begin
    Data := InternalGetCell(ACol, ARow);
    if Data is TKGridObjectCell then
      Result := TKGridObjectCell(Data).CellObject;
  end;
end;

function TKCustomGrid.GetRowHeights(Index: Integer): Integer;
begin
  if RowValid(Index) then
    Result := FRows[Index].Extent
  else
    Result := 0;
end;

function TKCustomGrid.GetRows(Index: Integer): TKGridRow;
begin
  if RowValid(Index) and (FRows[Index] is TKGridRow) then
    Result := TKGridRow(FRows[Index])
  else
    Result := nil;
end;

function TKCustomGrid.InternalGetSelAvail: Boolean;
begin
  Result := True;
end;

function TKCustomGrid.GetSelection: TKGridRect;
begin
  Result := AdjustSelection(FSelection);
end;

function TKCustomGrid.GetSelectionCount: Integer;
begin
  Result := Length(FSelections) + 1;
end;

function TKCustomGrid.GetSelectionRect: TRect;
begin
  Result := Rect(0,0,0,0);
  if GridRectToRect(Selection, Result, False, goRangeSelect in FOptions) then
  begin
    if FOptions * [goFixedHorzLine, goHorzLine] = [goFixedHorzLine, goHorzLine] then
      Dec(Result.Bottom, GetEffectiveRowSpacing(Max(Selection.Row1, Selection.Row2)));
    if FOptions * [goFixedVertLine, goVertLine] = [goFixedVertLine, goVertLine] then
      Dec(Result.Right, GetEffectiveColSpacing(Max(Selection.Col1, Selection.Col2)));
  end;
end;

function TKCustomGrid.GetSelections(Index: Integer): TKGridRect;
begin
  if Index = 0 then
    Result := Selection
  else if (Index > 0) and (Index < SelectionCount) then
    Result := FSelections[Index - 1]
  else
    Result := GridRect(0,0,0,0);
end;

function TKCustomGrid.GetSortCol: Integer;
var
  I: Integer;
begin
  Result := cInvalidIndex;
  for I := 0 to FColCount - 1 do
    if FCols[I].SortMode <> smNone then
    begin
      Result := I;
      Break;
    end;
end;

function TKCustomGrid.GetSortRow: Integer;
var
  I: Integer;
begin
  Result := cInvalidIndex;
  for I := 0 to FRowCount - 1 do
    if FRows[I].SortMode <> smNone then
    begin
      Result := I;
      Break;
    end;
end;

function TKCustomGrid.GetTabStops(Index: Integer): Boolean;
begin
  if ColValid(Index) and (FCols[Index] is TKGridCol) then
    Result := TKGridCol(FCols[Index]).TabStop
  else
    Result := True
end;

function TKCustomGrid.GetThemedCells: Boolean;
begin
  Result := Themes and (FOptions * [goThemes, goThemedCells] = [goThemes, goThemedCells]);
end;

function TKCustomGrid.GetThemes: Boolean;
begin
{$IFDEF USE_THEMES}
  Result := ThemeServices.ThemesEnabled
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TKCustomGrid.GetVisibleColCount: Integer;
begin
  Result := LastVisibleCol;
end;

function TKCustomGrid.GetVisibleGridRect: TKGridRect;
begin
  Result := GridRect(FTopLeft.Col, FTopLeft.Row, VisibleColCount, VisibleRowCount);
end;

function TKCustomGrid.GetVisibleRowCount: Integer;
begin
  Result := LastVisibleRow;
end;

procedure TKCustomGrid.GMRecreateEditor(var Msg: TLMessage);
begin
  EditorMode := False;
  EditorMode := True;
end;

function TKCustomGrid.GridRectSelectable(const GridRect: TKGridRect): Boolean;
begin
  Result := ColSelectable(GridRect.Col1) and ColSelectable(GridRect.Col2) and
    RowSelectable(GridRect.Row1) and RowSelectable(GridRect.Row2);
end;

function TKCustomGrid.GridRectToRect(GridRect: TKGridRect; var R: TRect;
  VisibleOnly: Boolean; Merged: Boolean): Boolean;

  function Axis(const Info: TKGridAxisInfo; var Index1, Index2: Integer; Split: Boolean): Boolean;
  begin
    Result := True;
    if Split then
    begin
      // adjust indexes for either fixed or nonfixed area
      if Index1 >= Info.FixedCellCount then
      begin
        if VisibleOnly then
          if Index2 >= Info.FirstGridCell then
            Index1 := Max(Index1, Info.FirstGridCell)
          else
            Result := False;
        Index2 := Max(Index2, Index1);
      end else
        Index2 := Min(Index2, Info.FixedCellCount - 1);
    end
    else if (Index1 >= Info.FixedCellCount) and VisibleOnly then
    begin
      if Index2 >= Info.FirstGridCell then
        Index1 := Max(Index1, Info.FirstGridCell)
      else
        Result := False;
    end;
  end;

  procedure Axis1(const Info: TKGridAxisInfo; Index1, Index2, AMin: Integer;
    out AMax: Integer);
  var
    I: Integer;
  begin
    AMax := AMin;
    I := Index1;
    if Info.CellExtent(I) = 0 then
    begin
      while (I >= 0) and (Info.CellExtent(I) = 0) do Dec(I);
      Inc(I);
    end;
    while (I <= Index2) and (not VisibleOnly or (AMax < Info.ClientExtent)) do
    begin
      if not VisibleOnly or (I < Info.FixedCellCount) or (I >= Info.FirstGridCell) then
        Inc(AMax, Info.CellExtent(I) + Info.EffectiveSpacing(I));
     // if (Index1 < Info.FirstGridCell) and (I = Info.FirstGridCell) then
     //   Dec(AMax, Info.ScrollOffset);
      Inc(I);
    end;
  end;

var
  Info: TKGridAxisInfoBoth;
begin
  Result := False;
  NormalizeGridRect(GridRect);
  if GridRectValid(GridRect) then
  begin
    Info := GetAxisInfoBoth([]);
    if Merged then
      GridRect := InternalExpandGridRect(GridRect);
    // aki:
    if Axis(Info.Horz, GridRect.Col1, GridRect.Col2, not (gxEditFixedCols in FOptionsEx)) and
      Axis(Info.Vert, GridRect.Row1, GridRect.Row2, not (gxEditFixedRows in FOptionsEx)) then
    begin
      if CellToPoint(GridRect.Col1, GridRect.Row1, R.TopLeft, VisibleOnly) then
      begin
        Axis1(Info.Horz, GridRect.Col1, GridRect.Col2, R.Left, R.Right);
        Axis1(Info.Vert, GridRect.Row1, GridRect.Row2, R.Top, R.Bottom);
        Result := (R.Right > R.Left) and (R.Bottom > R.Top);
      end;
    end;
  end;
end;

function TKCustomGrid.GridRectValid(const GridRect: TKGridRect): Boolean;
begin
  Result := ColValid(GridRect.Col1) and ColValid(GridRect.Col2) and
    RowValid(GridRect.Row1) and RowValid(GridRect.Row2);
end;

function TKCustomGrid.GridStateToInvisibleCells: TKGridInvisibleCells;
begin
  case FGridState of
    gsColMoving: Result := icFixedCols;
    gsRowMoving: Result := icFixedRows;
    gsSelecting: Result := icCells;
  else
    Result := icNone;
  end;
end;

function TKCustomGrid.HasFocus: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and Form.Active then
    Result := (Form.ActiveControl = Self) or (FEditor <> nil) and (Form.ActiveControl = FEditor)
  else
    Result := False;
end;

function TKCustomGrid.HasHorzScrollBar: Boolean;
begin
  Result := (FScrollBars in [ssHorizontal, ssBoth]) and
    not (goAlignLastCol in FOptions);
end;

function TKCustomGrid.HasVertScrollBar: Boolean;
begin
  Result := (FScrollBars in [ssVertical, ssBoth]) and
    not (goAlignLastRow in FOptions);
end;

procedure TKCustomGrid.HideCellHint;
begin
  DefaultMouseCellHint(-1, -1, False);
end;

function TKCustomGrid.InitialCol(ACol: Integer): Integer;
var
  Item: TKGridAxisItem;
begin
  Item := FCols[ACol];
  if Item <> nil then
    Result := Item.InitialPos
  else
    Result := ACol;
end;

function TKCustomGrid.InitialColInv(ACol: Integer): Integer;
var
  I: Integer;
  Item: TKGridAxisItem;
begin
  Result := ACol;
  for I := 0 to FColCount - 1 do
  begin
    Item := FCols[I];
    if (Item <> nil) and (Item.InitialPos = ACol) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TKCustomGrid.InitialRow(ARow: Integer): Integer;
var
  Item: TKGridAxisItem;
begin
  Item := FRows[ARow];
  if Item <> nil then
    Result := Item.InitialPos
  else
    Result := ARow;
end;

function TKCustomGrid.InitialRowInv(ARow: Integer): Integer;
var
  I: Integer;
  Item: TKGridAxisItem;
begin
  Result := ARow;
  for I := 0 to FRowCount - 1 do
  begin
    Item := FRows[I];
    if (Item <> nil) and (Item.InitialPos = ARow) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TKCustomGrid.InsertCol(At: Integer);
begin
  InsertCols(At, 1);
end;

procedure TKCustomGrid.InsertCols(At, Count: Integer);
begin
  if not ColValid(At) then At := FColCount;
  ChangeDataSize(True, At, Count, False, 0, 0);
end;

procedure TKCustomGrid.InsertRow(At: Integer);
begin
  InsertRows(At, 1);
end;

procedure TKCustomGrid.InsertRows(At, Count: Integer);
begin
  if not RowValid(At) then At := FRowCount;
  ChangeDataSize(False, 0, 0, True, At, Count);
end;

function TKCustomGrid.InsertSortedCol(out ByRow, ACol: Integer): Boolean;
begin
  ByRow := SortRow;
  if ByRow >= 0 then
  begin
    ACol := InternalInsertNR(ByRow, FFixedCols, FColCount - 1, FRows[ByRow].SortMode = smUp,
      CompareCols);
    if ACol >= FFixedCols then
    begin
      LockSortMode;
      try
        InsertCol(ACol);
      finally
        UnlockSortMode;
      end;
    end;
  end;
  Result := (ByRow >= 0) and (ACol >= FFixedCols);
end;

function TKCustomGrid.InsertSortedRow(out ByCol, ARow: Integer): Boolean;
begin
  ByCol := SortCol;
  if ByCol >= 0 then
  begin
    ARow := InternalInsertNR(ByCol, FFixedRows, FRowCount - 1, FCols[ByCol].SortMode = smUp,
      CompareRows);
    if ARow >= FFixedRows then
    begin
      LockSortMode;
      try
        InsertRow(ARow);
      finally
        UnlockSortMode;
      end;
    end;
  end;
  Result := (ByCol >= 0) and (ARow >= FFixedRows);
end;

procedure TKCustomGrid.InternalExchangeCols(Index1, Index2: Integer);
var
  I: Integer;
  AxisItem: TKGridAxisItem;
  CellPtr: TKGridCell;
begin
  AxisItem := FCols[Index1];
  FCols[Index1] := FCols[Index2];
  FCols[Index2] := AxisItem;
  if Assigned(FCells) then
  begin
    for I := 0 to FRowCount - 1 do
    begin
      CellPtr := FCells[I, Index1];
      FCells[I, Index1] := FCells[I, Index2];
      FCells[I, Index2] := CellPtr;
    end;
  end;
  if Assigned(FOnExchangeCols) then
    FOnExchangeCols(Self, Index1, Index2);
  if FSelection.Col1 = Index1 then
    FSelection.Col1 := Index2
  else if FSelection.Col1 = Index2 then
    FSelection.Col1 := Index1;
  FSelection.Col2 := FSelection.Col1;
  FEditorCell.Col := FSelection.Col1;
end;

procedure TKCustomGrid.InternalExchangeRows(Index1, Index2: Integer);
var
  AxisItem: TKGridAxisItem;
  CellPtr: TKGridCellRow;
begin
  AxisItem := FRows[Index1];
  FRows[Index1] := FRows[Index2];
  FRows[Index2] := AxisItem;
  if Assigned(FCells) then
  begin
    CellPtr := FCells[Index1];
    FCells[Index1] := FCells[Index2];
    FCells[Index2] := CellPtr;
  end else
    CellPtr := nil;
  if Assigned(FOnExchangeRows) then
    FOnExchangeRows(Self, Index1, Index2);
  if FSelection.Row1 = Index1 then
    FSelection.Row1 := Index2
  else if FSelection.Row1 = Index2 then
    FSelection.Row1 := Index1;
  FSelection.Row2 := FSelection.Row1;
  FEditorCell.Row := FSelection.Row1;
end;

function TKCustomGrid.InternalExpandGridRect(const GridRect: TKGridRect): TKGridRect;
var
  I, J, MyCol, MyRow: Integer;
  Span: TKGridCellSpan;
begin
  Result := GridRect;
  for I := GridRect.Col1 to GridRect.Col2 do
    for J := GridRect.Row1 to GridRect.Row2 do
    begin
      InternalFindBaseCell(I, J, MyCol, MyRow);
      Span := InternalGetCellSpan(MyCol, MyRow);
      Result.Col1 := Min(Result.Col1, MyCol);
      Result.Col2 := Max(Result.Col2, MyCol + Span.ColSpan - 1);
      Result.Row1 := Min(Result.Row1, MyRow);
      Result.Row2 := Max(Result.Row2, MyRow + Span.RowSpan - 1);
    end;
end;

procedure TKCustomGrid.InternalFindBaseCell(ACol, ARow: Integer; out BaseCol, BaseRow: Integer);
begin
  BaseCol := ACol;
  BaseRow := ARow;
  with InternalGetCellSpan(ACol, ARow) do
    if (ColSpan <= 0) and (RowSpan <= 0) then
    begin
      BaseCol := ACol + ColSpan;
      BaseRow := ARow + RowSpan;
    end;
end;

procedure TKCustomGrid.InternalFlip(Left, Right: Integer;
  Exchange: TKGridExchangeProc);
var
  I: Integer;
begin
  for I := 0 to (Right - Left) div 2 do
    Exchange(Left + I, Right - I);
end;

function TKCustomGrid.InternalGetCell(ACol, ARow: Integer): TKGridCell;
begin
  if FCells[ARow, ACol] = nil then
    FCells[ARow, ACol] := FCellClass.Create(Self);
  Result := FCells[ARow, ACol];
end;

function TKCustomGrid.InternalGetCellSpan(ACol, ARow: Integer): TKGridCellSpan;
begin
  Result := MakeCellSpan(1, 1);
  if Assigned(FOnCellSpan) then
    FOnCellSpan(Self, ACol, ARow, Result)
  else if Assigned(FCells) then with InternalGetCell(ACol, ARow) do
    Result := Span;
end;

function TKCustomGrid.InternalGetColWidths(Index: Integer): Integer;
begin
  Result := FCols[Index].Extent
end;

function TKCustomGrid.InternalGetEffectiveColSpacing(ACol: Integer): Integer;
begin
  if FCols[ACol].Extent = 0 then
  begin
    if (goIndicateHiddenCells in FOptions) and ((ACol = 0) or (FCols[ACol - 1].Extent <> 0)) then
      Result := FHCI.VBegin.Width
    else
      Result := 0;
  end
  else if FOptions * [goFixedVertLine, goVertLine] <> [] then
  begin
    if (ACol = FColCount - 1) and (goAlignLastCol in FOptions) then
      Result := 0
    else
      Result := FGridLineWidth
  end else
    Result := 0;
end;

function TKCustomGrid.InternalGetEffectiveRowSpacing(ARow: Integer): Integer;
begin
  if FRows[ARow].Extent = 0 then
  begin
    if (goIndicateHiddenCells in FOptions) and ((ARow = 0) or (FRows[ARow - 1].Extent <> 0)) then
      Result := FHCI.HBegin.Height
    else
      Result := 0;
  end
  else if FOptions * [goFixedHorzLine, goHorzLine] <> [] then
  begin
    if (ARow = FRowCount - 1) and (goAlignLastRow in FOptions) or
      ThemedCells and (ARow < FFixedRows) and (goHeader in FOptions) then
      Result := 0
    else
      Result := FGridLineWidth
  end else
    Result := 0;
end;

procedure TKCustomGrid.InternalGetHExtent(AIndex, AColSpan: Integer;
  out DestExtent, DestSpacing: Integer);
var
  I, J, K, L, Spacing: Integer;
begin
  DestExtent := InternalGetColWidths(AIndex);
  Spacing := InternalGetEffectiveColSpacing(AIndex);
  DestSpacing := Spacing;
  if AColSpan > 1 then
  begin
    // cell is merged across columns
    if DestExtent > 0 then J := DestSpacing else J := 0;
    for I := AIndex + 1 to AIndex + AColSpan - 1 do
    begin
      K := InternalGetColWidths(I);
      L := InternalGetEffectiveColSpacing(I);
      if K > 0 then
        J := L;
      Inc(DestExtent, K);
      Inc(DestSpacing, L);
    end;
    if DestExtent > 0 then
    begin
      Inc(DestExtent, DestSpacing - J);
      DestSpacing := J;
    end else
      DestSpacing := Spacing;
  end;
end;

procedure TKCustomGrid.InternalGetVExtent(AIndex, ARowSpan: Integer;
  out DestExtent, DestSpacing: Integer);
var
  I, J, K, L, Spacing: Integer;
begin
  DestExtent := InternalGetRowHeights(AIndex);
  Spacing := InternalGetEffectiveRowSpacing(AIndex);
  DestSpacing := Spacing;
  if ARowSpan > 1 then
  begin
    // cell is merged across rows
    if DestExtent > 0 then J := DestSpacing else J := 0;
    for I := AIndex + 1 to AIndex + ARowSpan - 1 do
    begin
      K := InternalGetRowHeights(I);
      L := InternalGetEffectiveRowSpacing(I);
      if K > 0 then
        J := L;
      Inc(DestExtent, K);
      Inc(DestSpacing, L);
    end;
    if DestExtent > 0 then
    begin
      Inc(DestExtent, DestSpacing - J);
      DestSpacing := J;
    end else
      DestSpacing := Spacing;
  end;
end;

function TKCustomGrid.InternalGetMaxColWidth(Index: Integer): Integer;
begin
  if (FCols[Index].MaxExtent > 0) and not (goAlignLastCol in FOptions) then
    Result := FCols[Index].MaxExtent
  else
    Result := MaxInt;
end;

function TKCustomGrid.InternalGetMaxRowHeight(Index: Integer): Integer;
begin
  if (FRows[Index].MaxExtent > 0) and not (goAlignLastRow in FOptions) then
    Result := FRows[Index].MaxExtent
  else
    Result := MaxInt;
end;

function TKCustomGrid.InternalGetMinColWidth(Index: Integer): Integer;
begin
  if FCols[Index].MinExtent > 0 then
    Result := FCols[Index].MinExtent
  else
    Result := FMinColWidth;
end;

function TKCustomGrid.InternalGetMinRowHeight(Index: Integer): Integer;
begin
  if FRows[Index].MinExtent > 0 then
    Result := FRows[Index].MinExtent
  else
    Result := FMinRowHeight;
end;

function TKCustomGrid.InternalGetRowHeights(Index: Integer): Integer;
begin
  Result := FRows[Index].Extent;
  if (Result > 0) and (Index < FFixedRows) and (goHeader in FOptions) and ThemedCells
    and (FOptions * [goFixedHorzLine, goHorzLine] <> []) then
    Inc(Result, FGridLineWidth);
end;

function TKCustomGrid.InternalInsertNR(ByIndex, Left, Right: Integer;
  SortedUp: Boolean; Compare: TKGridCompareProc): Integer;
var
  Key, Mult: Integer;
begin
  if SortedUp then Mult := -1 else Mult := 1;
  repeat
    Key := (Left + Right) div 2;
    if Compare(ByIndex, cInvalidIndex, Key) * Mult < 0 then
      Right := Key - 1
    else
      Left := Key + 1;
  until Left > Right;
  Result := Left;
end;

function TKCustomGrid.InternalInsertIfCellModifiedNR(ByIndex, Index, Left, Right: Integer;
  SortedUp: Boolean; Compare: TKGridCompareProc): Integer;
var
  Key, Mult, TmpLeft, TmpRight: Integer;
begin
  Result := Index;
  if SortedUp then Mult := 1 else Mult := -1;
  if Left < Index then
  begin
    TmpLeft := Left;
    TmpRight := Index - 1;
    repeat
      Key := (TmpLeft + TmpRight) div 2;
      if Compare(ByIndex, Key, Index) * Mult < 0 then
        TmpRight := Key - 1
      else
        TmpLeft := Key + 1;
    until TmpLeft > TmpRight;
    if TmpLeft < Index then
    begin
      Result := TmpLeft;
      Exit;
    end;
  end;
  if Index < Right then
  begin
    TmpLeft := Index + 1;
    TmpRight := Right;
    repeat
      Key := (TmpLeft + TmpRight) div 2;
      if Compare(ByIndex, Key, Index) * Mult < 0 then
        TmpRight := Key - 1
      else
        TmpLeft := Key + 1;
    until TmpLeft > TmpRight;
    Result := TmpRight;
  end;
  Result := MinMax(Result, Left, Right);
end;

function TKCustomGrid.InternalMove(var ACol, ARow: Integer; Command: TKGridMoveCommand; Wrap, Expanding: Boolean): Boolean;
var
  BaseCol, BaseRow, BkCol, BkRow, BkBaseCol, BkBaseRow: Integer;
  BkCommand: TKGridMoveCommand;
begin
  BkCol := ACol;
  BkRow := ARow;
  BkCommand := mcNone;
  InternalFindBaseCell(ACol, ARow, BkBaseCol, BkBaseRow);
  repeat
    case Command of
      mcDown:
      begin
        Inc(ARow);
        if ARow < FRowCount then
        begin
          if FMemCol >= 0 then
            ACol := FMemCol;
          FMemRow := ARow;
        end
        else if Wrap then
        begin
          ARow := FFixedRows;
          Inc(ACol);
          if ACol >= FColCount then
          begin
            // aki:
            if (gxEditFixedCols in FOptionsEx) or ((gxEditFixedRows in FOptionsEx) and (ARow < FFixedRows)) then
              ACol := 0
            else
              ACol := FFixedCols;
          end;
          FMemCol := ACol;
        end
        else if BkCommand <> mcNone then
        begin
          Dec(ARow);
          Command := BkCommand;
          BkCommand := mcNone;
        end else
          ARow := BkRow;
      end;
      mcEnd:
      begin
        ACol := FColCount - 1;
        FMemCol := ACol;
        if FMemRow >= 0 then
          ARow := FMemRow;
        Command := mcLeft;
      end;
      mcHome:
      begin
        // aki:
        if (gxEditFixedCols in FOptionsEx) or ((gxEditFixedRows in FOptionsEx) and (ARow < FFixedRows)) then
          ACol := 0
        else
          ACol := FFixedCols;
        FMemCol := ACol;
        if FMemRow >= 0 then
          ARow := FMemRow;
        Command := mcRight;
      end;
      mcLeft:
      begin
        Dec(ACol);
        // aki:
        if (gxEditFixedCols in FOptionsEx) or ((gxEditFixedRows in FOptionsEx) and (ARow<FFixedRows)) then
        begin
         if ACol >= 0 then
          begin
            if FMemRow >= 0 then
              ARow := FMemRow;
            FMemCol := ACol;
          end
          else
          if Wrap then
          begin
            ACol := FColCount - 1;
            Dec(ARow);
            if ARow < 0 then ARow := FRowCount - 1;
            FMemRow := ARow;
          end
          else if BkCommand <> mcNone then
          begin
            Inc(ACol);
            Command := BkCommand;
            BkCommand := mcNone;
          end else
            ACol := BkCol;
        end else
        begin
          if ACol >= FFixedCols then
          begin
            if FMemRow >= 0 then
              ARow := FMemRow;
            FMemCol := ACol;
          end
          else if Wrap then
          begin
            ACol := FColCount - 1;
            Dec(ARow);
            if ARow < FFixedRows then ARow := FRowCount - 1;
            FMemRow := ARow;
          end
          else if BkCommand <> mcNone then
          begin
            Inc(ACol);
            Command := BkCommand;
            BkCommand := mcNone;
          end else
            ACol := BkCol;
        end;
      end;
      mcMoveUp:
      begin
        // aki:
        if ((gxEditFixedRows in FOptionsEx) or (ARow > FFixedRows)) and (FMemCol >= 0) then
          ACol := FMemCol;
        ARow := FTopLeft.Row;
        Command := mcUp;
        BkCommand := mcDown;
      end;
      mcMoveDown:
      begin
        if (ARow < FRowCount - 1) and (FMemCol >= 0) then
          ACol := FMemCol;
        ARow := FTopLeft.Row + PageHeight - 1;
        Command := mcDown;
        BkCommand := mcUp;
      end;
      mcRight:
      begin
        Inc(ACol);
        if ACol < FColCount then
        begin
          if FMemRow >= 0 then
            ARow := FMemRow;
          FMemCol := ACol;
        end
        else if Wrap then
        begin
          ACol := FFixedCols;
          Inc(ARow);
          if ARow >= FRowCount then ARow := FFixedRows;
          FMemRow := ARow;
        end
        else if BkCommand <> mcNone then
        begin
          Dec(ACol);
          Command := BkCommand;
          BkCommand := mcNone;
        end else
          ACol := BkCol;
      end;
      mcUp:
      begin
        Dec(ARow);
        // aki:
        if (ARow >= FFixedRows) or ((gxEditFixedRows in FOptionsEx) and (ARow >= 0)) then
        begin
          if FMemCol >= 0 then
            ACol := FMemCol;
          FMemRow := ARow;
        end
        else if Wrap then
        begin
          ARow := FRowCount - 1;
          Dec(ACol);
          if ACol < FFixedCols then ACol := FColCount - 1;
          FMemCol := ACol;
        end
        else if BkCommand <> mcNone then
        begin
          Inc(ARow);
          Command := BkCommand;
          BkCommand := mcNone;
        end else
          ARow := BkRow;
      end;
      mcPageDown:
      begin
        if (ARow < FRowCount - 1) and (FMemCol >= 0) then
          ACol := FMemCol;
        ARow := Min(ARow + PageHeight, FRowCount - 1);
        Command := mcDown;
        BkCommand := mcUp;
        FMemRow := ARow;
      end;
      mcPageLeft:
      begin
        if (ARow > FFixedCols) and (FMemRow >= 0) then
          ARow := FMemRow;
        ACol := Max(ACol - PageWidth, FFixedCols);
        Command := mcLeft;
        BkCommand := mcRight;
        FMemCol := ACol;
      end;
      mcPageRight:
      begin
        if (ARow < FColCount - 1) and (FMemRow >= 0) then
          ARow := FMemRow;
        ACol := Min(ACol + PageWidth, FColCount - 1);
        Command := mcRight;
        BkCommand := mcLeft;
        FMemCol := ACol;
      end;
      mcPageUp:
      begin
        if (ARow > FFixedRows) and (FMemCol >= 0) then
          ACol := FMemCol;
        ARow := Max(ARow - PageHeight, FFixedRows);
        Command := mcUp;
        BkCommand := mcDown;
        FMemRow := ARow;
      end;
      mcTop:
      begin
        ACol := FFixedCols;
        ARow := FFixedRows;
        FMemCol := ACol;
        FMemRow := ARow;
        Command := mcRight;
        Wrap := True;
      end;
      mcBottom:
      begin
        ACol := FColCount - 1;
        ARow := FRowCount - 1;
        FMemCol := ACol;
        FMemRow := ARow;
        Command := mcLeft;
        Wrap := True;
      end;
    end;
    InternalFindBaseCell(ACol, ARow, BaseCol, BaseRow);
  until (ACol = BkCol) and (ARow = BkRow) or ((BaseCol <> BkBaseCol) or (BaseRow <> BkBaseRow)) and
    (not Expanding and SelectCell(BaseCol, BaseRow) or Expanding and SelectionExpand(BaseCol, BaseRow));
  Result := (ACol <> BkCol) or (ARow <> BkRow);
  ACol := BaseCol;
  ARow := BaseRow;
end;

procedure TKCustomGrid.InternalPaintCell(ACol, ARow: Integer; AState: TKGridDrawState;
  const ARect, ABlockRect: TRect; ACanvas: TCanvas; Clip, Printing: Boolean);
begin
  FCellPainter.Col := ACol;
  FCellPainter.Row := ARow;
  FCellPainter.State := AState;
  FCellPainter.CellPos := ARect.TopLeft;
  FCellPainter.Canvas := ACanvas;
  FCellPainter.CellRect := ARect;
  FCellPainter.BlockRect := ABlockRect;
  FCellPainter.FPrinting := Printing;
  // prepare cell painter and draw cell
  FCellPainter.BeginDraw;
  try
    if Clip or Printing then
      FCellPainter.BeginClip;
    try
//      FCellPainter.Canvas.TextRect(ARect, ARect.Left, ARect.Top, 'debugtest');
      if not DrawCell(FCellPainter.Col, FCellPainter.Row, FCellPainter.CellRect, FCellPainter.State) then
        FCellPainter.DrawEmptyCell; // stub function
    finally
      FCellPainter.EndClip;
    end;
  finally
    FCellPainter.EndDraw;
  end;
end;

procedure TKCustomGrid.InternalQuickSortNR(ByIndex, Left, Right: Integer;
  SortedDown: Boolean; Compare: TKGridCompareProc; Exchange: TKGridExchangeProc);
type
  TStackItem = record
    LIndex, RIndex: Integer;
  end;
const
  cStackGrow = 100;
var
  Key, L, R, LBack, RBack, StackLen, StackPtr: Integer;
  Stack: array of TStackItem;
begin
  { this is the non recursive quick sort algorithm to avoid stack overflows.
    Right parts of divided arrays are stored into a stack-like array
    in dynamic memory for later use. }
  SetLength(Stack, cStackGrow);
  StackPtr := 0;
  with Stack[StackPtr] do begin LIndex := Left; RIndex := Right end;
  repeat
    with Stack[StackPtr] do begin Left := LIndex; Right := RIndex end;
    Dec(StackPtr);
    repeat
      L := Left;
      R := Right;
      Key := (L + R) div 2;
      LBack := Left - 1;
      RBack := Right;
      repeat
        if SortedDown then
        begin
          while (L < Right) and (Compare(ByIndex, L, Key) < 0) do Inc(L);
          while (R > Left) and (Compare(ByIndex, R, Key) > 0) do Dec(R);
        end else
        begin
          while (L < Right) and (Compare(ByIndex, L, Key) > 0) do Inc(L);
          while (R > Left) and (Compare(ByIndex, R, Key) < 0) do Dec(R);
        end;
        if L <= R then
        begin
          if L < R then
            if (L = Key) or (R = Key) then
            begin
              // preserve Key, exchange later
              LBack := L;
              RBack := R;
            end else
              Exchange(L, R);
          Dec(R);
          Inc(L);
        end;
      until L >= R;
      // exchange anything with former Key
      if LBack >= Left then
        Exchange(LBack, RBack);
      if L < Right then
      begin
        Inc(StackPtr);
        StackLen := Length(Stack);
        if StackPtr >= StackLen then
          SetLength(Stack, StackLen + cStackGrow);
        with Stack[StackPtr] do begin LIndex := L; RIndex := Right end;
      end;
      Right := R;
    until Left >= Right;
  until StackPtr < 0;
end;

procedure TKCustomGrid.InternalSetCell(ACol, ARow: Integer; Value: TKGridCell);
var
  TmpClass: TClass;
  TmpCell: TKGridCell;
  Span: TKGridCellSpan;
begin
  if FCells[ARow, ACol] <> nil then
    Span := FCells[ARow, ACol].Span
  else
    Span := MakeCellSpan(1, 1);
  FreeAndNil(FCells[ARow, ACol]);
  if Value <> nil then
  begin
    TmpClass := Value.ClassType;
    TmpCell := TKGridCellClass(TmpClass).Create(Self);
    FlagSet(cGF_GridUpdates);
    try
      TmpCell.Assign(Value);
      TmpCell.Span := Span;
    finally
      FlagClear(cGF_GridUpdates);
    end;
    FCells[ARow, ACol] := TmpCell;
  end;
  InvalidateCell(ACol, ARow);
end;

procedure TKCustomGrid.InternalSetCells(ACol, ARow: Integer; const Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
var
  Cell, Tmp: TKGridCell;
begin
  Cell := InternalGetCell(ACol, ARow);
  FlagSet(cGF_GridUpdates);
  try
    if not (Cell is TKGridTextCell) then
    begin
      if FCellClass.InheritsFrom(TKGridTextCell) then
        Tmp := FCellClass.Create(Self)
      else
        Tmp := TKGridTextCell.Create(Self);
      Tmp.Assign(Cell);
      Cell.Free;
      FCells[ARow, ACol] := Tmp;
    end;
    TKGridTextCell(FCells[ARow, ACol]).Text := Text;
  finally
    FlagClear(cGF_GridUpdates);
  end;
  InvalidateCell(ACol, ARow);
end;

function TKCustomGrid.InternalSetCellSpan(ACol, ARow: Integer;
  const Value: TKGridCellSpan): TKGridRect;

  procedure Merge(ACol1, ARow1, ACol2, ARow2: Integer);
  var
    I, J: Integer;
    Cell: TKGridCell;
  begin
    for I := ACol1 to ACol2 - 1 do
      for J := ARow1 to ARow2 - 1 do
      begin
        Cell := InternalGetCell(I, J);
        if (I = ACol1) and (J = ARow1) then
          Cell.Span := MakeCellSpan(ACol2 - ACol1, ARow2 - ARow1)
        else
          Cell.Span := MakeCellSpan(ACol1 - I, ARow1 - J);
      end;
  end;

  procedure Split(ACol1, ARow1, ACol2, ARow2: Integer);
  var
    I, J: Integer;
    RefSpan: TKGridCellSpan;
  begin
    RefSpan := MakeCellSpan(1, 1);
    for I := ACol1 to ACol2 - 1 do
      for J := ARow1 to ARow2 - 1 do
        InternalGetCell(I, J).Span := RefSpan;
  end;

var
  I, J, BaseCol, BaseRow: Integer;
  Span: TKGridCellSpan;
  Cell: TKGridCell;
begin
  Result := GridRect(ACol, ARow, ACol + Value.ColSpan - 1, ARow + Value.ColSpan - 1);
  if (ACol >= FFixedCols) and (ARow >= FFixedRows) then
    FlagSet(cGF_SelCellsMerged);
  Span := InternalGetCell(ACol, ARow).Span;
  if (Span.ColSpan > 1) or (Span.RowSpan > 1) then
  begin
    // destroy previously merged area
    Split(ACol, ARow, ACol + Span.ColSpan, ARow + Span.RowSpan);
    Result.Col2 := Max(Result.Col2, ACol + Span.ColSpan - 1);
    Result.Row2 := Max(Result.Row2, ARow + Span.RowSpan - 1);
  end;
  for I := ACol to ACol + Value.ColSpan - 1 do
    for J := ARow to ARow + Value.RowSpan - 1 do
    begin
      Cell := InternalGetCell(I, J);
      Span := Cell.Span;
      if (Span.ColSpan <> 1) or (Span.RowSpan <> 1) then
      begin
        // adjust all four overlapping spans
        InternalFindBaseCell(I, J, BaseCol, BaseRow);
        if (BaseCol <> ACol) or (BaseRow <> ARow) then
        begin
          Span := InternalGetCell(BaseCol, BaseRow).Span;
          Split(Max(ACol, BaseCol), Max(ARow, BaseRow),
            Min(ACol + Value.ColSpan, BaseCol + Span.ColSpan), Min(ARow + Value.RowSpan, BaseRow + Span.RowSpan));
          Merge(BaseCol, BaseRow, BaseCol + Span.ColSpan, ARow);
          Merge(BaseCol, ARow + Value.RowSpan, BaseCol + Span.ColSpan, BaseRow + Span.RowSpan);
          Merge(BaseCol, Max(ARow, BaseRow), ACol, Min(ARow + Value.RowSpan, BaseRow + Span.RowSpan));
          Merge(ACol + Value.ColSpan, Max(ARow, BaseRow), BaseCol + Span.ColSpan, Min(ARow + Value.RowSpan, BaseRow + Span.RowSpan));
          Result.Col1 := Min(Result.Col1, BaseCol);
          Result.Row1 := Min(Result.Row1, BaseRow);
          Result.Col2 := Max(Result.Col2, BaseCol + Span.ColSpan - 1);
          Result.Row2 := Max(Result.Row2, BaseRow + Span.RowSpan - 1);
        end;
      end;
      if (I = ACol) and (J = ARow) then
        Cell.Span := Value
      else
        Cell.Span := MakeCellSpan(ACol - I, ARow - J);
    end;
end;

procedure TKCustomGrid.InternalSetColCount(Value: Integer);
begin
  if Value > FColCount then
    ChangeDataSize(True, FColCount, Value - FColCount, False, 0, 0)
  else if Value < FColCount then
    ChangeDataSize(False, Value, FColCount - Value, False, 0, 0);
end;

procedure TKCustomGrid.InternalSetFixedCols(Value: Integer);
begin
  ColCount := Max(ColCount, Value + 1);
  FFixedCols := Value;
  ResetTopLeft;
  SelectionFix(FSelection);
  UpdateAxes(True, cAll, False, cAll, []);
end;

procedure TKCustomGrid.InternalSetFixedRows(Value: Integer);
begin
  RowCount := Max(RowCount, Value + 1);
  FFixedRows := Value;
  ResetTopLeft;
  SelectionFix(FSelection);
  UpdateAxes(False, cAll, True, cAll, []);
end;

procedure TKCustomGrid.InternalSetRowCount(Value: Integer);
begin
  if Value > FRowCount then
    ChangeDataSize(False, 0, 0, True, FRowCount, Value - FRowCount)
  else if Value < FRowCount then
    ChangeDataSize(False, 0, 0, False, Value, FRowCount - Value);
end;

function TKCustomGrid.InternalUpdateVirtualGrid: Boolean;
begin
  Result := True;
end;

procedure TKCustomGrid.InternalUnlockUpdate;
begin
  ClearSortMode;
  UpdateAxes(True, cAll, True, cAll, [afCheckMinExtent]);
end;

procedure TKCustomGrid.InvalidateCell(ACol, ARow: Integer);
begin
  InvalidateGridRect(GridRect(GridPoint(ACol, ARow)));
end;

procedure TKCustomGrid.InvalidateCol(ACol: Integer);
var
  GR: TKGridRect;
begin
  if UpdateUnlocked and HandleAllocated then
  begin
    ACol := MinMax(ACol, 0, FColCount - 1);
    GR.Col1 := ACol;
    GR.Col2 := ACol;
    if FFixedRows > 0 then
    begin
      GR.Row1 := 0;
      GR.Row2 := FFixedRows - 1;
      InvalidateGridRect(GR);
    end;
    GR.Row1 := FFixedRows;
    GR.Row2 := LastVisibleRow;
    InvalidateGridRect(GR);
  end;
end;

procedure TKCustomGrid.InvalidateCols(FirstCol: Integer);
var
  Boundary, FirstRow: Integer;
  P: TPoint;
  R: TRect;
  GR: TKGridRect;
begin
  if UpdateUnlocked and HandleAllocated then
  begin
    FirstCol := MinMax(FirstCol, 0, FColCount - 1);
    if FirstCol >= FFixedCols then
      FirstCol := Max(FirstCol, FTopLeft.Col);
    if FFixedRows > 0 then
    begin
      GR := GridRect(FirstCol, 0, FirstCol, FFixedRows - 1);
      GR := InternalExpandGridRect(GR);
      Boundary := GR.Col1;
      FirstRow := 0;
    end else
    begin
      Boundary := MaxInt;
      FirstRow := FTopLeft.Row;
    end;
    GR := GridRect(FirstCol, FTopLeft.Row, FirstCol, LastVisibleRow);
    GR := InternalExpandGridRect(GR);
    FirstCol := Min(Boundary, GR.Col1);
    if FirstCol >= FFixedCols then
      FirstCol := Max(FirstCol, FTopLeft.Col);
    if CellToPoint(FirstCol, FirstRow, P, True) then
    begin
      if FirstCol >= FFixedCols then
        Boundary := GetAxisInfoHorz([aiFixedParams]).FixedBoundary
      else
        Boundary := 0;
      R := Rect(Max(P.X, Boundary), 0, ClientWidth, ClientHeight);
      InvalidateRect(Handle, @R, False);
    end;
  end;
end;

procedure TKCustomGrid.InvalidateCurrentSelection;
begin
  InvalidateSelection(Selection);
  if EditorMode and CellInGridRect(Col, Row, Selection) then
    FEditor.Invalidate;
end;

procedure TKCustomGrid.InvalidateGridRect(const GR: TKGridRect; Merged: Boolean);
var
  R: TRect;
begin
  if UpdateUnlocked and HandleAllocated and GridRectToRect(GR, R, True, Merged) then
    InvalidateRect(Handle, @R, False);
end;

procedure TKCustomGrid.InvalidateRow(ARow: Integer);
var
  GR: TKGridRect;
begin
  if UpdateUnlocked and HandleAllocated then
  begin
    ARow := MinMax(ARow, 0, FRowCount - 1);
    GR.Row1 := ARow;
    GR.Row2 := ARow;
    if FFixedCols > 0 then
    begin
      GR.Col1 := 0;
      GR.Col2 := FFixedCols - 1;
      InvalidateGridRect(GR);
    end;
    GR.Col1 := FFixedCols;
    GR.Col2 := LastVisibleCol;
    InvalidateGridRect(GR);
  end;
end;

procedure TKCustomGrid.InvalidateRows(FirstRow: Integer);
var
  Boundary, FirstCol: Integer;
  P: TPoint;
  R: TRect;
  GR: TKGridRect;
begin
  if UpdateUnlocked and HandleAllocated then
  begin
    FirstRow := MinMax(FirstRow, 0, FRowCount - 1);
    if FirstRow >= FFixedRows then
      FirstRow := Max(FirstRow, FTopLeft.Row);
    if FFixedCols > 0 then
    begin
      GR := GridRect(0, FirstRow, FFixedCols - 1, FirstRow);
      GR := InternalExpandGridRect(GR);
      Boundary := GR.Row1;
      FirstCol := 0;
    end else
    begin
      Boundary := MaxInt;
      FirstCol := FTopLeft.Col;
    end;
    GR := GridRect(FirstCol, FirstRow, LastVisibleCol, FirstRow);
    GR := InternalExpandGridRect(GR);
    FirstRow := Min(Boundary, GR.Row1);
    if FirstRow >= FFixedRows then
      FirstRow := Max(FirstRow, FTopLeft.Row);
    if CellToPoint(FirstCol, FirstRow, P, True) then
    begin
      if FirstRow >= FFixedRows then
        Boundary := GetAxisInfoVert([aiFixedParams]).FixedBoundary
      else
        Boundary := 0;
      R := Rect(0, Max(P.Y, Boundary), ClientWidth, ClientHeight);
      InvalidateRect(Handle, @R, False);
    end;
  end;
end;

procedure TKCustomGrid.InvalidateSelection(ASelection: TKGridRect);
var
  R: TRect;
begin
  if UpdateUnlocked and HandleAllocated then
  begin
    ASelection := AdjustSelection(ASelection);
    if GridRectToRect(ASelection, R, True) then
      InvalidateRect(Handle, @R, False);
    if goIndicateSelection in FOptions then
    begin
      // this causes extremely slow painting under GTKx!
      // do not use goIndicateSelection here!
      if not (goRowSelect in FOptions) and (FFixedRows > 0) and GridRectToRect(
        GridRect(ASelection.Col1, 0, ASelection.Col2, FFixedRows - 1), R, True) then
        InvalidateRect(Handle, @R, False);
      if (FFixedCols > 0) and GridRectToRect(
        GridRect(0, ASelection.Row1, FFixedCols - 1, ASelection.Row2), R, True) then
        InvalidateRect(Handle, @R, False);
    end;
  end;
end;

function TKCustomGrid.IsDoubleBuffered: Boolean;
begin
  Result := DoubleBuffered or (goDoubleBufferedCells in FOptions);
end;

function TKCustomGrid.IsThemed: Boolean;
begin
  Result := goThemes in FOptions;
end;

procedure TKCustomGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  ACol, ARow, ATopRow, SelCol, SelRow: Integer;
  Stage: TKGridSelectionStage;
  Expanding: Boolean;
begin
  inherited;
  SelCol := FSelection.Col1;
  SelRow := FSelection.Row1;
  Expanding := False;
  if ssShift in Shift then
  begin
    Stage := ssExpand;
    if (goRangeSelect in FOptions) and (FRangeSelectStyle = rsMS_Excel) then
    begin
      SelCol := FSelection.Col2;
      SelRow := FSelection.Row2;
      Expanding := True;
    end;
  end else
    Stage := ssInit;
  ACol := SelCol;
  ARow := SelRow;
  ATopRow := FTopLeft.Row;
  if ssCtrl in Shift then
    case Key of
      VK_UP: Dec(ATopRow);
      VK_DOWN: Inc(ATopRow);
      VK_LEFT: InternalMove(ACol, ARow, mcPageLeft, False, Expanding);
      VK_RIGHT: InternalMove(ACol, ARow, mcPageRight, False, Expanding);
      VK_PRIOR: InternalMove(Acol, ARow, mcMoveUp, False, Expanding);
      VK_NEXT: InternalMove(Acol, ARow, mcMoveDown, False, Expanding);
      VK_HOME: InternalMove(Acol, ARow, mcTop, False, Expanding);
      VK_END: InternalMove(Acol, ARow, mcBottom, False, Expanding);
    end
  else
    case Key of
      VK_RETURN:
      begin
        FlagSet(cGF_EnterPressed);
        if goEnterMoves in FOptions then
        begin
           if (ACol = FColCount - 1) and (ARow = FRowCount - 1) and (gxEnterAppendsRow in FOptionsEx) then
           begin
             InsertRow(FRowCount);
             InternalMove(ACol, ARow, DirectionToCommand(FMoveDirection), True, Expanding);
           end else
             InternalMove(ACol, ARow, DirectionToCommand(FMoveDirection), (gxEnterWraps in FOptionsEx), Expanding);
        end else
          EditorMode := not EditorMode;
      end;
      VK_ESCAPE:
      begin
        CancelMode;
        if EditorMode then
        begin
          EditorDataFromGrid(FEditor, FEditorCell.Col, FEditorCell.Row);
          EditorMode := False;
        end;
      end;
      VK_UP: InternalMove(ACol, ARow, mcUp, False, Expanding);
      VK_DOWN: InternalMove(ACol, ARow, mcDown, False, Expanding);
      VK_LEFT: InternalMove(ACol, ARow, mcLeft, False, Expanding);
      VK_RIGHT: InternalMove(ACol, ARow, mcRight, False, Expanding);
      VK_NEXT: InternalMove(ACol, ARow, mcPageDown, False, Expanding);
      VK_PRIOR: InternalMove(ACol, ARow, mcPageUp, False, Expanding);
      VK_HOME: InternalMove(ACol, ARow, mcHome, False, Expanding);
      VK_END: InternalMove(ACol, ARow, mcEnd, False, Expanding);
      VK_TAB:
      begin
        if goTabs in FOptions then
        begin
          if not (ssAlt in Shift) then
          repeat
            if ssShift in Shift then
            begin
              InternalMove(ACol, ARow, mcLeft, gxTabWraps in FOptionsEx, Expanding);
              Stage := ssInit;
            end else
            begin
              if (ACol = FColCount - 1) and (ARow = FRowCount - 1) and (gxTabAppendsRow in FOptionsEx) then
              begin
                InsertRow(FRowCount);
                InternalMove(ACol, ARow, mcRight, True, Expanding);
              end else
                InternalMove(ACol, ARow, mcRight, gxTabWraps in FOptionsEx, Expanding);
            end;
          until TabStops[ACol] or (ACol = FSelection.Col1);
        end;
      end;
      VK_F2: EditorMode := True;
    end;
  DefaultSetCaretToLeft(Key, Shift);
  // aki:
  if (gxEditFixedCols in FOptionsEx) and (gxEditFixedRows in FOptionsEx) then
  begin
    ACol := MinMax(ACol, 0, FColCount - 1);
    ARow := MinMax(ARow, 0, FRowCount - 1);
  end
  else if (gxEditFixedCols in FOptionsEx) then
  begin
    ACol := MinMax(ACol, 0, FColCount - 1);
    ARow := MinMax(ARow, FFixedRows, FRowCount - 1);
  end
  else if (gxEditFixedRows in FOptionsEx) and (ARow < FFixedRows) then
  begin
    ACol := MinMax(ACol, 0, FColCount - 1);
    ARow := MinMax(ARow, 0, FRowCount - 1);
  end else
  begin
    ACol := MinMax(ACol, FFixedCols, FColCount - 1);
    ARow := MinMax(ARow, FFixedRows, FRowCount - 1);
  end;
  if (ACol <> SelCol) or (ARow <> SelRow) then
  begin
    CellChanging(SelCol, SelRow, ACol, ARow);
    if SelectionMove(ACol, ARow, Stage, [sfMustUpdate, sfClampInView, sfDontCallSelectCell, sfNoMemPos]) then
    begin
      Click;
      if not (goAlwaysShowEditor in FOptions) then
        EditorMode := False;
      Key := 0;
    end;
  end
  else if ATopRow <> FTopLeft.Row then
    TopRow := MinMax(ATopRow, FFixedRows, FRowCount - 1);
  // whenever set, this flag is only valid for the nearest KeyDown call
  FlagClear(cGF_CaretToLeft or cGF_EnterPressed);
end;

procedure TKCustomGrid.Loaded;
begin
  inherited;
  FColors.ClearBrightColors;
  FColors.BrightRangeBkGnds;
end;

procedure TKCustomGrid.LateUpdate(var Msg: TLMessage);
begin
  inherited;
  case Msg.Msg of
    CN_KEYDOWN:
    begin
      KeyDown(TLMKey(Msg).CharCode, KeyDataToShiftState(TLMKey(Msg).KeyData));
    end;
    LM_SETFOCUS:
    begin
      InvalidateCurrentSelection;
      SafeSetFocus;
    end;
  end;
end;

procedure TKCustomGrid.LockSortMode;
begin
  Inc(FSortModeLock);
end;

function TKCustomGrid.MeasureCell(ACol, ARow: Integer; const ARect: TRect;
  AState: TKGridDrawState; Priority: TKGridMeasureCellPriority): TPoint;
begin
  FCellPainter.Col := ACol;
  FCellPainter.Row := ARow;
  FCellPainter.State := AState;
  FCellPainter.CellPos := ARect.TopLeft;
  FCellPainter.Canvas := Canvas;
  FCellPainter.CellRect := ARect;
  FCellPainter.FPrinting := False;
  // prepare cell painter and measure cell data
  FCellPainter.BeginDraw;
  try
    Result.X := ARect.Right - ARect.Left;
    Result.Y := ARect.Bottom - ARect.Top;
    if Assigned(FOnMeasureCell) then
      FOnMeasureCell(Self, ACol, ARow, ARect, AState, Priority, Result)
    else if Assigned(FCells) then with InternalGetCell(ACol, ARow) do
    begin
      ApplyDrawProperties;
      MeasureCell(ACol, ARow, ARect, AState, Priority, Result)
    end else
      Result := FCellPainter.DefaultMeasure(Priority);
  finally
    FCellPainter.EndDraw;
  end;
end;


procedure TKCustomGrid.MeasurePages(var Info: TKPrintMeasureInfo);

  procedure Axis(const Info: TKGridAxisInfo; CanvasExtent, SelStart, SelEnd: Integer;
    SelOnly, FitToPage: Boolean; out Pages, OutlineExtent: Integer);
  var
    I, StartIndex, EndIndex, Extent, PageExtent: Integer;
  begin
    Pages := 1;
    PageExtent := 0;
    OutlineExtent := 0;
    if SelOnly then
    begin
      StartIndex := SelStart;
      EndIndex := SelEnd;
    end else
    begin
      StartIndex := 0;
      EndIndex := Info.TotalCellCount - 1;
    end;
    for I := StartIndex to EndIndex do
    begin
      Extent := Info.CellExtent(I) + Info.EffectiveSpacing(I);
      if FitToPage or (PageExtent + Extent < CanvasExtent) or (I = 0) then
        Inc(PageExtent, Extent)
      else
      begin
        Inc(Pages);
        OutlineExtent := Max(OutlineExtent, PageExtent);
        PageExtent := Extent;
      end;
    end;
    OutlineExtent := Max(OutlineExtent, PageExtent);
  end;

var
  ColPages, RowPages: Integer;
  Scale: Double;
  FitToPage, SelOnly: Boolean;
  R: TKGridRect;
  APageSetup: TKPrintPageSetup;
begin
  R := InternalExpandGridRect(Selection);
  NormalizeGridRect(R);
  APageSetup := PageSetup;
  FitToPage := poFitToPage in APageSetup.Options;
  SelOnly := APageSetup.Range = prSelectedOnly;
  Scale := APageSetup.Scale / 100;
  Axis(GetAxisInfoHorz([]), Round(APageSetup.PaintAreaWidth / Scale), R.Col1, R.Col2,
    SelOnly, FitToPage, ColPages, Info.OutlineWidth);
  if FitToPage then
    Scale := APageSetup.PaintAreaWidth / Info.OutlineWidth;
  Axis(GetAxisInfoVert([]), Round(APageSetup.PaintAreaHeight / Scale), R.Row1, R.Row2,
    SelOnly, False, RowPages, Info.OutlineHeight);
  Info.HorzPageCount := ColPages;
  Info.VertPageCount := RowPages;
  Info.PageCount := ColPages * RowPages;
end;

procedure TKCustomGrid.MouseCellHint(ACol, ARow: Integer; AShow: Boolean);
begin
  if Assigned(FOnMouseCellHint) then
    FOnMouseCellHint(Self, ACol, ARow, AShow)
  else
    DefaultMouseCellHint(ACol, ARow, AShow);
end;

procedure TKCustomGrid.MouseClickCell(ACol, ARow: Integer);
begin
  if (gxFixedCellClickSelect in FOptionsEx) and ((ARow < FFixedRows) or (ACol < FFixedCols)) and (ssShift in GetShiftState) then
  begin
    if (ARow < FFixedRows) and (ACol < FFixedCols) then
    begin
      if AllCellsSelected then
        UnselectRange
      else
        SelectAll;
    end else
    begin
      if ACol >= FFixedCols then
      begin
        if EntireColSelected[ACol] then
          UnselectRange
        else
          SelectCol(ACol);
      end else
      begin
        if EntireRowSelected[ARow] then
          UnselectRange
        else
          SelectRow(ARow);
      end;
    end;
  end;
  if Assigned(FOnMouseClickCell) then
    FOnMouseClickCell(Self, ACol, ARow);
end;

procedure TKCustomGrid.MouseDblClickCell(ACol, ARow: Integer);
begin
  if Assigned(FOnMouseDblClickCell) then
    FOnMouseDblClickCell(Self, ACol, ARow);
end;

procedure TKCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  BaseCol, BaseRow: Integer;
  CellFound: Boolean;
  State: TKGridState;
begin
  inherited;
  if (Button = mbLeft) and not FScrollTimer.Enabled then
  begin
    SafeSetFocus;
    if ssDouble in Shift then
      DblClick;
    FHitPos := Point(X, Y);
    State := gsNormal;
    CellFound := PointToCell(FHitPos, False, icNone, FHitCell.Col, FHitCell.Row, BaseCol, BaseRow);
    if CellFound then
      InternalFindBaseCell(FHitCell.Col, FHitCell.Row, BaseCol, BaseRow);
    if PointToSizing(FHitPos, State, FSizingIndex, FSizingDest) then
    begin
      if (State = gsColSizing) and
        BeginColSizing(FSizingIndex, FSizingDest) or
        (State = gsRowSizing) and
        BeginRowSizing(FSizingIndex, FSizingDest) then
      begin
        FGridState := State;
        if CellFound then
          InvalidateCell(BaseCol, BaseRow);
        Update;
        SuggestSizing(csStart);
      end;
    end
    else if CellFound then
    begin
      if FMouseOver.Col >= 0 then
      begin
        MouseCellHint(FMouseOver.Col, FMouseOver.Row, False);
        FCellHintTimer.Enabled := False;
      end;
      // aki: row for greater than fixed cols:
      if (FHitCell.Row < FFixedRows) and (FHitCell.Col >= FFixedCols) and (not (gxEditFixedRows in FOptionsEx)) then
      begin
        if goColMoving in FOptions then
          FGridState := gsColMoveWaiting
        else if goRowSorting in FOptions then
          FGridState := gsRowSortWaiting;
      end
      // aki: col
      else if ((FHitCell.Col < FFixedCols) and (FHitCell.Row >= FFixedRows)) and (not (gxEditFixedCols in FOptionsEx)) then
      begin
        if goRowMoving in FOptions then
          FGridState := gsRowMoveWaiting
        else if goColSorting in FOptions then
          FGridState := gsColSortWaiting;
      end
      // aki: row for greater than fixed row:
      else if ((FHitCell.Col < FFixedCols) and (FHitCell.Row < FFixedRows)) and (not (gxEditFixedRows in FOptionsEx)) then
      begin
        FGridState := gsClickWaiting;
      end else
      begin
        FlagSet(cGF_SelectedByMouse);
        try
          if (Col <> BaseCol) or (Row <> BaseRow) then
            CellChanging(Col, Row, BaseCol, BaseRow);
          if SelectionMove(BaseCol, BaseRow, ssInit, [sfMustUpdate, sfClampInView]) then
          begin
            FGridState := gsSelecting;
            EditorMode := (goAlwaysShowEditor in FOptions) or (ssDouble in Shift);
          end;
        finally
          FlagClear(cGF_SelectedByMouse);
        end;
      end;
      InvalidateCell(BaseCol, BaseRow);
      if ssDouble in Shift then
        MouseDblClickCell(BaseCol, BaseRow);
    end;
  end;
end;

procedure TKCustomGrid.MouseEnterCell(ACol, ARow: Integer);
begin
  if Assigned(FOnMouseEnterCell) then
    FOnMouseEnterCell(Self, ACol, ARow);
end;

procedure TKCustomGrid.MouseFormLeave;
var
  P: TPoint;
begin
  inherited;
  if EditorMode then
  begin
    P := FEditor.ScreenToClient(Mouse.CursorPos);
    if PtInRect(FEditor.ClientRect, P) then
      FEditor.Perform(CM_MOUSEENTER, 0, 0)
    else
      MouseOverCells;
  end else
    MouseOverCells;
end;

procedure TKCustomGrid.MouseLeaveCell(ACol, ARow: Integer);
begin
  if Assigned(FOnMouseLeaveCell) then
    FOnMouseLeaveCell(Self, ACol, ARow);
end;

procedure TKCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);

  function CanDrag: Boolean;
  begin
    Result :=
      (X > FHitPos.X + 8) or
      (X < FHitPos.X - 8) or
      (Y > FHitPos.Y + 8) or
      (Y < FHitPos.Y - 8);
  end;

var
  MustScroll: Boolean;
  DeltaHorz, DeltaVert, HitCol, HitRow, SelCol, SelRow: Integer;
  MousePt: TPoint;
begin
  inherited;
  MousePt := Point(X, Y);
  if MouseCapture then
  begin
    case FGridState of
      gsColSizing:
      begin
        SuggestSizing(csHide);
        FSizingDest := X;
        SuggestSizing(csShow);
      end;
      gsRowSizing:
      begin
        SuggestSizing(csHide);
        FSizingDest := Y;
        SuggestSizing(csShow);
      end;
      gsColMoveWaiting: if CanDrag then
      begin
        FDragOrigin := FHitCell.Col;
        if BeginColDrag(FDragOrigin, MousePt) then
        begin
          ProcessDragWindow(FHitPos, MousePt, FDragOrigin, True, False);
          FGridState := gsColMoving;
          FDragDest := FDragOrigin;
          SuggestDrag(csStart);
        end;
      end;
      gsRowMoveWaiting: if CanDrag then
      begin
        FDragOrigin := FHitCell.Row;
        if BeginRowDrag(FDragOrigin, MousePt) then
        begin
          ProcessDragWindow(FHitPos, MousePt, FDragOrigin, False, False);
          FGridState := gsRowMoving;
          FDragDest := FDragOrigin;
          SuggestDrag(csStart);
        end;
      end;
      gsSelecting, gsColMoving, gsRowMoving:
      begin
        if FGridState <> gsSelecting then
          ProcessDragWindow(FHitPos, MousePt, cInvalidIndex, FGridState = gsColMoving, False);
        if not FScrollTimer.Enabled and PointToCell(MousePt, True,
          GridStateToInvisibleCells, HitCol, HitRow, SelCol, SelRow) then
        begin
          MustScroll := ScrollNeeded(HitCol, HitRow, DeltaHorz, DeltaVert);
          if MustScroll then
          begin
            Scroll(cScrollDelta, cScrollDelta, DeltaHorz, DeltaVert, False);
            FScrollTimer.Enabled := True;
          end;
          if FGridState = gsSelecting then
          begin
            InternalFindBaseCell(SelCol, SelRow, SelCol, SelRow);
            if (Col <> SelCol) or (Row <> SelRow) then
              CellChanging(Col, Row, SelCol, SelRow);
            SelectionMove(SelCol, SelRow, ssExpand, [sfMustUpdate])
          end else
            DragMove(HitCol, HitRow, MousePt);
        end;
      end;
    end;
  end;
  MouseOverCells;
end;

procedure TKCustomGrid.MouseOverCells;
var
  MousePt: TPoint;
  HitCol, HitRow, BaseCol, BaseRow: Integer;
begin
  MousePt := ScreenToClient(Mouse.CursorPos);
  if not (FGridState in [gsColMoving, gsRowMoving]) and
    ((goMouseOverCells in FOptions) or (FGridState <> gsNormal)) and
    PtInRect(ClientRect, MousePt) and
    PointToCell(MousePt, False, icNone, HitCol, HitRow, BaseCol, BaseRow) then
  begin
    InternalFindBaseCell(HitCol, HitRow, BaseCol, BaseRow);
    if (BaseCol <> FMouseOver.Col) or (BaseRow <> FMouseOver.Row) then
    begin
      if FMouseOver.Col >= 0 then
      begin
        InvalidateCell(FMouseOver.Col, FMouseOver.Row);
        MouseCellHint(FMouseOver.Col, FMouseOver.Row, False);
        MouseLeaveCell(FMouseOver.Col, FMouseOver.Row);
      end;
      InvalidateCell(BaseCol, BaseRow);
      if EditorMode and (
        (FMouseOver.Col = FEditorCell.Col) and (FMouseOver.Row = FEditorCell.Row) and
        ((BaseCol <> FEditorCell.Col) or (BaseRow <> FEditorCell.Row))
        or
        (BaseCol = FEditorCell.Col) and (BaseRow = FEditorCell.Row) and
        ((FMouseOver.Col <> FEditorCell.Col) or (FMouseOver.Row <> FEditorCell.Row))
        ) then
        FEditor.Invalidate;
      FMouseOver := GridPoint(BaseCol, BaseRow);
      MouseEnterCell(FMouseOver.Col, FMouseOver.Row);
      if not MouseCapture then
      begin
        FHintCell := FMouseOver;
        FCellHintTimer.Enabled := False;
        FCellHintTimer.Interval := FMouseCellHintTime;
        FCellHintTimer.Enabled := True;
      end;
    end;
  end
  else if FMouseOver.Col >= 0 then
  begin
    if EditorMode and (FMouseOver.Col = FEditorCell.Col) and (FMouseOver.Row = FEditorCell.Row) then
      FEditor.Invalidate;
    InvalidateCell(FMouseOver.Col, FMouseOver.Row);
    MouseCellHint(FMouseOver.Col, FMouseOver.Row, False);
    MouseLeaveCell(FMouseOver.Col, FMouseOver.Row);
    FMouseOver := GridPoint(-1, -1);
  end;
end;

procedure TKCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  function NextSortMode(ASortMode: TKGridSortMode): TKGridSortMode;
  begin
    case FSortStyle of
      ssDownUp: if ASortMode = smDown then Result := smUp else Result := smDown;
      ssDownUpNone:
        case ASortMode of
          smDown: Result := smUp;
          smUp: Result := smNone;
        else
          Result := smDown;
        end;
      else
        case ASortMode of
          smUp: Result := smDown;
          smDown: Result := smNone;
        else
          Result := smUp;
        end;
    end;
  end;

var
  BaseCol, BaseRow, BaseHitCol, BaseHitRow, HitCol, HitRow, Tmp: Integer;
  CellFound: Boolean;
  CellPt, MousePt: TPoint;
begin
  inherited;
  if Button = mbLeft then
  begin
    MousePt := Point(X, Y);
    case FGridState of
      gsColMoving, gsRowMoving:
      begin
        ProcessDragWindow(FHitPos, MousePt, -1, FGridState = gsColMoving, True);
        SuggestDrag(csStop);
      end;
      gsColSizing, gsRowSizing:
        SuggestSizing(csStop);
    end;
    if ColValid(FHitCell.Col) and RowValid(FHitCell.Row) and
      PointToCell(MousePt, False, icNone, HitCol, HitRow, BaseCol, BaseRow) then
    begin
      InternalFindBaseCell(HitCol, HitRow, BaseCol, BaseRow);
      InternalFindBaseCell(FHitCell.Col, FHitCell.Row, BaseHitCol, BaseHitRow);
      CellFound := (BaseHitCol = BaseCol) and (BaseHitRow = BaseRow);
    end else
      CellFound := False;
    case FGridState of
      gsSelecting:
      begin
        ClampInView(Col, Row);
        if CellFound then
          MouseClickCell(BaseCol, BaseRow);
        Click;
      end;
      gsColSortWaiting, gsRowSortWaiting, gsColMoveWaiting, gsRowMoveWaiting:
        if CellFound then
        begin
          if not (ssShift in Shift) and ((FGridState = gsColSortWaiting) or (FGridState = gsRowMoveWaiting)) and
            (goColSorting in FOptions) and (BaseCol = FRows[BaseRow].SortArrowIndex) then
            SortCols(BaseRow, NextSortMode(Rows[BaseRow].SortMode))
          else if not (ssShift in Shift) and ((FGridState = gsRowSortWaiting) or (FGridState = gsColMoveWaiting)) and
            (goRowSorting in FOptions) and (BaseRow = FCols[BaseCol].SortArrowIndex) then
            SortRows(BaseCol, NextSortMode(Cols[BaseCol].SortMode))
          else
          begin
            InvalidateCell(BaseCol, BaseRow);
            MouseClickCell(BaseCol, BaseRow);
            Click;
          end;
        end;
      gsClickWaiting:
        if CellFound then
        begin
          InvalidateCell(BaseCol, BaseRow);
          MouseClickCell(BaseCol, BaseRow);
          Click;
        end;
      gsColMoving:
        if EndColDrag(FDragOrigin, FDragDest, MousePt) and (FDragOrigin <> FDragDest) then
          MoveCol(FDragOrigin, FDragDest)
        else
          InvalidateCol(FDragOrigin);
      gsRowMoving:
        if EndRowDrag(FDragOrigin, FDragDest, MousePt) and (FDragOrigin <> FDragDest) then
          MoveRow(FDragOrigin, FDragDest)
        else
          InvalidateRow(FDragOrigin);
      gsColSizing:
      begin
        case FSizingStyle of
          ssLine, ssXORLine:
            if EndColSizing(FSizingIndex, FSizingDest) and CellToPoint(FSizingIndex, 0, CellPt) then
            begin
              Tmp := FSizingDest - CellPt.X;
              if not (goMouseCanHideCells in FOptions) then
                Tmp := Max(Tmp, InternalGetMinColWidth(FSizingIndex));
              ColWidths[FSizingIndex] := Tmp;
            end;
        end;
        UpdateDesigner;
      end;
      gsRowSizing:
      begin
        case FSizingStyle of
          ssLine, ssXORLine:
            if EndRowSizing(FSizingIndex, FSizingDest) and CellToPoint(0, FSizingIndex, CellPt) then
            begin
              Tmp := FSizingDest - CellPt.Y;
              if not (goMouseCanHideCells in FOptions) then
                Tmp := Max(Tmp, InternalGetMinRowHeight(FSizingIndex));
              RowHeights[FSizingIndex] := Tmp;
            end;
        end;
        UpdateDesigner;
      end
    else
      if CellFound then
        InvalidateCell(BaseCol, BaseRow);
    end;
    FlagClear(cGF_ThroughClick);
    FGridState := gsNormal;
  end;
end;

function TKCustomGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer): Boolean;
var
  DummyCol, DummyRow: Integer;
begin
  Result := PointToCell(Point(X, Y), False, icNone, ACol, ARow, DummyCol, DummyRow);
  if Result then
    InternalFindBaseCell(ACol, ARow, ACol, ARow);
end;

procedure TKCustomGrid.MoveCol(FromIndex, ToIndex: Integer);
var
  I: Integer;
begin
  if (FromIndex <> ToIndex) and ColValid(FromIndex) and ColValid(ToIndex) then
  begin
    if ToIndex > FromIndex then
      for I := ToIndex downto FromIndex do
        InternalExchangeCols(I, FromIndex)
    else
      for I := ToIndex to FromIndex do
        InternalExchangeCols(I, FromIndex);
    SelectionFix(FSelection);
    UpdateAxes(True, cAll, False, cAll, []);
    UpdateCellSpan;
    ClearSortModeVert;
    ColMoved(FromIndex, ToIndex);
  end;
end;

procedure TKCustomGrid.MoveRow(FromIndex, ToIndex: Integer);
var
  I: Integer;
begin
  if (FromIndex <> ToIndex) and RowValid(FromIndex) and RowValid(ToIndex) then
  begin
    if ToIndex > FromIndex then
      for I := ToIndex downto FromIndex do
        InternalExchangeRows(I, FromIndex)
    else
      for I := ToIndex to FromIndex do
        InternalExchangeRows(I, FromIndex);
    SelectionFix(FSelection);
    UpdateAxes(False, cAll, True, cAll, []);
    UpdateCellSpan;
    ClearSortModeHorz;
    RowMoved(FromIndex, ToIndex);
  end;
end;

procedure TKCustomGrid.MoveToNextCell;
var
  ACol , ARow : Integer;
begin
  ACol := Col;
  ARow := Row;
  InternalMove(ACol, ARow, DirectionToCommand(FMoveDirection), True, False);
  if (Col <> ACol) or (Row <> ARow) then
    CellChanging(Col, Row, ACol, ARow);
  if SelectionMove(ACol, ARow, ssInit, [sfMustUpdate, sfClampInView, sfDontCallSelectCell]) then
    Click;
end;

function TKCustomGrid.PageHeight: Integer;
var
  Info: TKGridAxisInfo;
begin
  Info := GetAxisInfoVert([aiFullVisBoundary]);
  Result := Max(Info.FullVisCells - Info.FirstGridCell, 1);
end;

function TKCustomGrid.PageWidth: Integer;
var
  Info: TKGridAxisInfo;
begin
  Info := GetAxisInfoHorz([aiFullVisBoundary]);
  Result := Max(Info.FullVisCells - Info.FirstGridCell, 1);
end;

procedure TKCustomGrid.PaintCell(ACanvas: TCanvas; ACol, ARow: Integer; AX, AY: Integer; APrinting: Boolean; ABlockRect: PRect);
var
  R, ClipRect, TmpRect, TmpBlockRect: TRect;
  CellBitmap: TBitmap;
  TmpCanvas: TCanvas;
  ClipCells: Boolean;
  Info: TKGridAxisInfoBoth;
begin
  if (ColWidths[ACol] > 0) and (RowHeights[ARow] > 0) then
    if CellRect(ACol, ARow, R, True) then
    begin
      if not APrinting and ((goDoubleBufferedCells in FOptions) or DoubleBuffered) then
        CellBitmap := TBitmap.Create
      else
        CellBitmap := nil;
      try
        if CellBitmap <> nil then
        begin
          TmpRect := Rect(0, 0, R.Right - R.Left, R.Bottom - R.Top);
          CellBitmap.Width := TmpRect.Right; // SetSize not supported prior Delphi 2006
          CellBitmap.Height := TmpRect.Bottom;
          TmpCanvas := CellBitmap.Canvas;
          SelectClipRect(TmpCanvas.Handle, TmpRect);
          ClipCells := False;
        end else
        begin
          if ACanvas <> nil then
          begin
            TmpRect := Rect(AX, AY, AX + R.Right - R.Left, AY + R.Bottom - R.Top);
            TmpCanvas := ACanvas;
            SelectClipRect(TmpCanvas.Handle, TmpRect);
          end else
          begin
            TmpRect := R;
            TmpCanvas := Canvas;
          end;
          ClipCells := goClippedCells in FOptions;
        end;
        if ABlockRect <> nil then
          TmpBlockRect :=  ABlockRect^
        else
          TmpBlockRect := SelectionRect;
        if CellBitmap <> nil then
          OffsetRect(TmpBlockRect, -R.Left, -R.Top);
        if (ACanvas = nil) or (ACanvas = Canvas) then
        begin
          Info := GetAxisInfoBoth([aiFixedParams]);
          if (ARow >= FFixedRows) and (ACol < FFixedCols) then
            ClipRect := Rect(0, Info.Vert.FixedBoundary, Info.Horz.FixedBoundary, Info.Vert.ClientExtent)
          else if (ARow < FFixedRows) and (ACol >= FFixedCols) then
            ClipRect := Rect(Info.Horz.FixedBoundary, 0, Info.Horz.ClientExtent, Info.Vert.FixedBoundary)
          else if (ARow >= FFixedRows) and (ACol >= FFixedCols) then
            ClipRect := Rect(Info.Horz.FixedBoundary, Info.Vert.FixedBoundary, Info.Horz.ClientExtent, Info.Vert.ClientExtent)
          else
            ClipRect := Rect(0, 0, Info.Horz.FixedBoundary, Info.Vert.FixedBoundary);
          SelectClipRect(Canvas.Handle, ClipRect);
        end;
        InternalPaintCell(ACol, ARow, GetDrawState(ACol, ARow, HasFocus), TmpRect, TmpBlockRect, TmpCanvas, ClipCells, False);
        if CellBitmap <> nil then
        begin
          Canvas.Lock;
          try
            Canvas.Draw(R.Left, R.Top, CellBitmap);
          finally
            Canvas.Unlock;
          end;
        end;
      finally
        CellBitmap.Free;
      end;
    end;
end;

function TKCustomGrid.PaintCells(ACanvas: TCanvas; CellBitmap: TBitmap; MainClipRgn: HRGN;
  FirstCol, LastCol, FirstRow, LastRow, X, Y, MaxX, MaxY: Integer; Printing, PaintSelection: Boolean;
  const ABlockRect: TRect): TPoint;
var
  I, J, I1, J1, XBack, YBack,
  CHExtent, CHSpacing, CVExtent, CVSpacing,
  HExtent, HSpacing, VExtent, VSpacing: Integer;
  ClipCells, DrawLinesHorz, DrawLinesVert, GridFocused, HasHeader, HasFixedThemedCells, UseThemedCells: Boolean;
  CellState: TKGridDrawState;
  Span: TKGridCellSpan;
  BorderRect, CellRect, TmpRect, TmpBlockRect: TRect;
  TmpCanvas: TCanvas;
begin
  GridFocused := Printing or HasFocus;
  UseThemedCells := ThemedCells;
  YBack := Y;
  XBack := X;
  // search for hidden merged cells first and update the FirstCol and FirstRow
  // this is supposed to be faster for huge grids than to parse entire grid all the time
  HExtent := FirstCol;
  VExtent := FirstRow;
  if FirstCol > 0 then // not for fixed cells
  begin
    I := FirstRow;
    while (I <= LastRow) and (Y <= MaxY) do
    begin
      InternalFindBaseCell(FirstCol, I, I1, J1);
      HExtent := Min(HExtent, I1);
      VExtent := Min(VExtent, J1);
      Inc(Y, InternalGetRowHeights(I) + InternalGetEffectiveRowSpacing(I));
      Inc(I);
    end;
  end;
  if FirstRow > 0 then // not for fixed cells
  begin
    I := FirstCol;
    while (I <= LastCol) and (X <= MaxX) do
    begin
      InternalFindBaseCell(I, FirstRow, I1, J1);
      HExtent := Min(HExtent, I1);
      VExtent := Min(VExtent, J1);
      Inc(X, InternalGetColWidths(I) + InternalGetEffectiveColSpacing(I));
      Inc(I);
    end;
  end;
  while FirstCol > HExtent do
  begin
    Dec(FirstCol);
    Dec(XBack, InternalGetColWidths(FirstCol) + InternalGetEffectiveColSpacing(FirstCol));
  end;
  while FirstRow > VExtent do
  begin
    Dec(FirstRow);
    Dec(YBack, InternalGetRowHeights(FirstRow) + InternalGetEffectiveRowSpacing(FirstRow));
  end;
  // now draw the grid
  Y := YBack;
  I := FirstRow;
  while (I <= LastRow) and (Y <= MaxY) do
  begin
    X := XBack;
    VExtent := InternalGetRowHeights(I);
    VSpacing := InternalGetEffectiveRowSpacing(I);
    J := FirstCol;
    while (J <= LastCol) and (X <= MaxX) do
    begin
      HExtent := InternalGetColWidths(J);
      HSpacing := InternalGetEffectiveColSpacing(J);
      Span := InternalGetCellSpan(J, I);
      if (Span.ColSpan > 0) and (Span.RowSpan > 0) then
      begin
        InternalGetHExtent(J, Span.ColSpan, CHExtent, CHSpacing);
        InternalGetVExtent(I, Span.RowSpan, CVExtent, CVSpacing);
        CellRect := Rect(X, Y, X + CHExtent, Y + CVExtent);
        BorderRect := CellRect;
        Inc(BorderRect.Bottom, CVSpacing);
        Inc(BorderRect.Right, CHSpacing);
        TmpRect := BorderRect;
        if not Printing then
          TranslateRectToDevice(ACanvas.Handle, TmpRect);
        if Printing or RectInRegion(MainClipRgn, TmpRect) then
        begin
          if (CHExtent > 0) and (CVExtent > 0) then
          begin
            CellState := GetDrawState(J, I, GridFocused);
            if Printing then
            begin
              CellState := CellState - [gdEdited, gdMouseDown, gdMouseOver];
              if not PaintSelection then
                CellState := CellState - [gdSelected, gdFocused];
            end;
            // default brush style for lines
            ACanvas.Brush.Style := bsSolid;
            // draw default grid
            if (CHSpacing > 0) or (CVSpacing > 0) then
            begin
              DrawLinesHorz := CVSpacing > 0;
              DrawLinesVert := CHSPacing > 0;
              if gdFixed in CellState then
              begin
                HasHeader := (I < FFixedRows) and (goHeader in FOptions) and UseThemedCells;
                HasFixedThemedCells := ((I < FFixedRows) or (J < FFixedCols)) and (gxFixedThemedCells in FOptionsEx) and UseThemedCells;
                DrawLinesHorz := DrawLinesHorz and (goFixedHorzLine in FOptions) and not HasFixedThemedCells;
                DrawLinesVert := DrawLinesVert and (goFixedVertLine in FOptions) and not (HasHeader or HasFixedThemedCells);
                if UseThemedCells then
                  ACanvas.Brush.Color := FColors.FixedThemedCellLines
                else
                  ACanvas.Brush.Color := FColors.FixedCellLines;
              end else
              begin
                ACanvas.Brush.Color := FColors.CellLines;
                DrawLinesHorz := DrawLinesHorz and (goHorzLine in FOptions);
                DrawLinesVert := DrawLinesVert and (goVertLine in FOptions);
              end;
              if DrawLinesHorz then
              begin
                TmpRect := Rect(CellRect.Left, CellRect.Bottom, BorderRect.Right, BorderRect.Bottom);
                ACanvas.FillRect(TmpRect);
              end else
                CellRect.Bottom := BorderRect.Bottom;
              if DrawLinesVert then
              begin
                TmpRect := Rect(CellRect.Right, CellRect.Top, BorderRect.Right, CellRect.Bottom);
                ACanvas.FillRect(TmpRect);
              end else
                CellRect.Right := BorderRect.Right;
            end;
            TmpBlockRect := ABlockRect;
            if CellBitmap <> nil then
            begin
              TmpRect := Rect(0, 0, CellRect.Right - CellRect.Left, CellRect.Bottom - CellRect.Top);
              CellBitmap.Width := TmpRect.Right; // SetSize not supported prior Delphi 2006
              CellBitmap.Height := TmpRect.Bottom;
              TmpCanvas := CellBitmap.Canvas;
              SelectClipRect(TmpCanvas.Handle, TmpRect);
              ClipCells := False;
              OffsetRect(TmpBlockRect, -CellRect.Left, -CellRect.Top);
            end else
            begin
              TmpRect := CellRect;
              TmpCanvas := ACanvas;
              ClipCells := goClippedCells in FOptions;
            end;
            InternalPaintCell(J, I, CellState, TmpRect, TmpBlockRect, TmpCanvas, ClipCells, Printing);
            if CellBitmap <> nil then
              ACanvas.Draw(CellRect.Left, CellRect.Top, CellBitmap);
          end
          else if goIndicateHiddenCells in FOptions then
          begin
            TmpRect := BorderRect;
            if (HExtent = 0) and (HSpacing > 0) then
            begin
              if (I = 0) and (CVExtent > FHCI.VBegin.Height) then
              begin
                ACanvas.Draw(TmpRect.Left, TmpRect.Top, FHCI.VBegin);
                Inc(TmpRect.Top, FHCI.VBegin.Height);
              end;
              if (I = FRowCount - 1) and (CVExtent > FHCI.VEnd.Height) then
              begin
                Dec(TmpRect.Bottom, FHCI.VEnd.Height);
                ACanvas.Draw(TmpRect.Left, TmpRect.Bottom, FHCI.HEnd);
              end;
              ACanvas.StretchDraw(TmpRect, FHCI.VCenter);
            end;
            if (VExtent = 0) and (VSpacing > 0) then
            begin
              if (J = 0) and (CHExtent > FHCI.HBegin.Width) then
              begin
                ACanvas.Draw(TmpRect.Left, TmpRect.Top, FHCI.HBegin);
                Inc(TmpRect.Left, FHCI.HBegin.Width);
              end;
              if (J = FColCount - 1) and (CHExtent > FHCI.HEnd.Width) then
              begin
                Dec(TmpRect.Right, FHCI.HEnd.Width);
                ACanvas.Draw(TmpRect.Right, TmpRect.Top, FHCI.HEnd);
              end;
              ACanvas.StretchDraw(TmpRect, FHCI.HCenter);
            end;
          end;
        end;
      end;
      Inc(X, HExtent + HSpacing);
      Inc(J);
    end;
    Inc(Y, VExtent + VSpacing);
    Inc(I);
  end;
  Result := Point(X, Y);
end;

procedure TKCustomGrid.PaintDragSuggestion(ACanvas: TCanvas);

  procedure DragSuggLine(X, Y, W, H: Integer);
  begin
    with ACanvas do
    begin
      Pen.Color := FColors.DragSuggestionLine;
      Pen.Style := psSolid;
      Pen.Width := 1;
      Brush.Color := FColors.DragSuggestionBkGnd;
      Brush.Style := bsSolid;
      Rectangle(X, Y, X + W, Y + H);
    end;
  end;

var
  Len: Integer;
  ArrowCopy: TKAlphaBitmap;
  R: TRect;
begin
  if GetDragRect(GetAxisInfoBoth([aiGridBoundary]), R) then
  begin
    case FDragStyle of
      dsLayeredConst, dsLayeredFaded:
      begin
        ArrowCopy := TKAlphaBitmap.Create;
        try
          if FGridState = gsColMoving then
          begin
            ArrowCopy.CopyFrom(FDragArrow);
            ArrowCopy.AlphaDrawTo(ACanvas, R.Left, R.Top);
            Len := R.Bottom - R.Top - ArrowCopy.Height shl 1;
            if Len > 0 then
            begin
              ArrowCopy.MirrorVert;
              ArrowCopy.AlphaDrawTo(ACanvas, R.Left, R.Bottom - ArrowCopy.Height);
              if Len > 6 then
                DragSuggLine(R.Left + ArrowCopy.Width shr 1 - 1,
                  R.Top + ArrowCopy.Height + 1, 3, Len - 2);
            end;
          end else
          begin
            ArrowCopy.CopyFromRotated(FDragArrow);
            ArrowCopy.AlphaDrawTo(ACanvas, R.Left, R.Top);
            Len := R.Right - R.Left - ArrowCopy.Width shl 1;
            if Len > 0 then
            begin
              ArrowCopy.MirrorHorz;
              ArrowCopy.AlphaDrawTo(ACanvas, R.Right - ArrowCopy.Width, R.Top);
              if Len > 6 then
                DragSuggLine(R.Left + ArrowCopy.Width + 1,
                  R.Top + ArrowCopy.Height shr 1 - 1, Len - 2, 3);
            end;
          end;
        finally
          ArrowCopy.Free;
        end;
      end;
      dsLine, dsXORLine:
      begin
        with ACanvas do
        begin
          // prevent rounded caps
          Pen.Width := 1;
          if FDragStyle = dsLine then
            Pen.Color := clRed
          else
          begin
            Pen.Mode := pmXOR;
            Pen.Color := clWhite;
          end;
          try
            if FGridState = gsColMoving then
            begin
              for Len := 0 to 4 do
              begin
                ACanvas.MoveTo(R.Left + Len, R.Top);
                ACanvas.LineTo(R.Left + Len, R.Bottom);
              end;
            end else
            begin
              for Len := 0 to 4 do
              begin
                ACanvas.MoveTo(R.Left, R.Top + Len);
                ACanvas.LineTo(R.Right, R.Top + Len);
              end;
            end;
          finally
            Pen.Mode := pmCopy;
          end;
        end;
      end;
    end;
  end;
end;

procedure TKCustomGrid.PaintHeaderAlignment(ACanvas: TCanvas; ARect: TRect);
begin
  {$IFDEF USE_THEMES}
  if ThemedCells then with ThemeServices do
  begin
    Inc(ARect.Bottom);
    DrawElement(ACanvas.Handle, GetElementDetails(thHeaderItemRightNormal), ARect)
  end else
  {$ENDIF}
  begin
    ACanvas.Brush.Color := FColors.FixedCellBkGnd;
    Dec(ARect.Bottom);
    ACanvas.FillRect(ARect);
    if {$IFDEF FPC}not Flat{$ELSE}Ctl3D{$ENDIF} then
    {$IFDEF USE_WINAPI}
      // looks somewhat better though
      DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_LEFT or BF_TOP or BF_BOTTOM or BF_SOFT);
    {$ELSE}
      DrawEdges(ACanvas, ARect, cl3DHilight, cl3DShadow, BF_LEFT or BF_TOP or BF_BOTTOM);
    {$ENDIF}
    ACanvas.Brush.Color := FColors.FixedCellLines;
    ACanvas.FillRect(Rect(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom + 1));
  end;
end;

procedure TKCustomGrid.PaintPage;

  procedure Axis(const Info: TKGridAxisInfo; CanvasExtent, Page, SelStart, SelEnd: Integer;
    SelOnly, FitToPage: Boolean; out FirstIndex, LastIndex, PageExtent: Integer);
  var
    I, Extent, StartIndex, EndIndex, Pages: Integer;
  begin
    Pages := 1;
    PageExtent := 0;
    if SelOnly then
    begin
      StartIndex := SelStart;
      EndIndex := SelEnd;
    end else
    begin
      StartIndex := 0;
      EndIndex := Info.TotalCellCount - 1;
    end;
    FirstIndex := StartIndex;
    LastIndex := StartIndex;
    for I := StartIndex to EndIndex do
    begin
      Extent := Info.CellExtent(I) + Info.EffectiveSpacing(I);
      if FitToPage or (PageExtent + Extent < CanvasExtent) or (I = 0) then
        Inc(PageExtent, Extent)
      else
      begin
        FirstIndex := LastIndex;
        LastIndex := I;
        if Page = Pages then
        begin
          Dec(LastIndex);
          Exit;
        end;
        Inc(Pages);
        PageExtent := Extent;
      end;
    end;
    FirstIndex := LastIndex;
    LastIndex := EndIndex;
  end;

var
  FirstCol, FirstRow, LastCol, LastRow, OutlineWidth, OutlineHeight, AreaWidth, AreaHeight: Integer;
  FitToPage, SelOnly{$IFDEF LCLQT}, AThemedCells{$ENDIF}: Boolean;
  TmpRect, TmpRect1: TRect;
  MainClipRgn: HRGN;
  R: TKGridRect;
  APageSetup: TKPrintPageSetup;
//  CellBitmap: TBitmap;
begin
  R := InternalExpandGridRect(Selection);
  NormalizeGridRect(R);
  APageSetup := PageSetup;
  FitToPage := poFitToPage in APageSetup.Options;
  SelOnly := APageSetup.Range = prSelectedOnly;
  AreaWidth := Round(APageSetup.PaintAreaWidth / APageSetup.CurrentScale);
  AreaHeight := Round(APageSetup.PaintAreaHeight / APageSetup.CurrentScale);
  Axis(GetAxisInfoHorz([]), AreaWidth, (APageSetup.CurrentPage - 1) mod APageSetup.HorzPageCount + 1,
    R.Col1, R.Col2, SelOnly, FitToPage, FirstCol, LastCol, OutlineWidth);
  Axis(GetAxisInfoVert([]), AreaHeight, (APageSetup.CurrentPage - 1) div APageSetup.HorzPageCount + 1,
    R.Row1, R.Row2, SelOnly, False, FirstRow, LastRow, OutlineHeight);
  if poUseColor in APageSetup.Options then
    FColors.ColorScheme := csNormal
  else
    FColors.ColorScheme := csGrayScale;
  TmpRect := Rect(0, 0, OutlineWidth, OutlineHeight);
  TmpRect1 := Rect(0, 0, AreaWidth, AreaHeight);
  IntersectRect(TmpRect, TmpRect, TmpRect1);
  TranslateRectToDevice(APageSetup.Canvas.Handle, TmpRect);
{$IFDEF LCLQT}
  AThemedCells := goThemedCells in FOptions;
  Exclude(FOptions, goThemedCells);
{$ENDIF}
  MainClipRgn := CreateRectRgnIndirect(TmpRect);
//  if goDoubleBufferedCells in FOptions then
//    CellBitmap := TBitmap.Create
//  else
//    CellBitmap := nil;
  try
    SelectClipRgn(APageSetup.Canvas.Handle, MainClipRgn);
    TmpRect := SelectionRect;
    if SelOnly then
      OffsetRect(TmpRect, -TmpRect.Left, -TmpRect.Top);
    PaintCells(PageSetup.Canvas, nil, MainClipRgn, FirstCol, LastCol, FirstRow, LastRow,
      0, 0, OutlineWidth, OutlineHeight, True, poPaintSelection in APageSetup.Options, TmpRect);
  finally
    DeleteObject(MainClipRgn);
//    CellBitmap.Free;
  {$IFDEF LCLQT}
    if AThemedCells then
      Include(FOptions, goThemedCells);
  {$ENDIF}
  end;
end;

procedure TKCustomGrid.PaintSizingSuggestion(ACanvas: TCanvas);
var
  Info: TKGridAxisInfo;
  I: Integer;
begin
  case FSizingStyle of
    ssLine, ssXORLine:
    begin
      with ACanvas do
      begin
        Pen.Width := 1;
        if FSizingStyle = ssLine then
          Pen.Color := clRed
        else
        begin
          Pen.Mode := pmXOR;
          Pen.Color := clWhite;
        end;
        try
          case FGridState of
            gsColSizing:
            begin
              Info := GetAxisInfoVert([aiGridBoundary]);
              for I := 0 to 1 do
              begin
                ACanvas.MoveTo(FSizingDest + I, 0);
                ACanvas.LineTo(FSizingDest + I, Info.GridBoundary);
              end;
            end;
            gsRowSizing:
            begin
              Info := GetAxisInfoHorz([aiGridBoundary]);
              for I := 0 to 1 do
              begin
                ACanvas.MoveTo(0, FSizingDest + I);
                ACanvas.LineTo(Info.GridBoundary, FSizingDest + I);
              end;
            end;
          end;
        finally
          Pen.Mode := pmCopy;
        end;
      end;
    end;
  end;
end;

procedure TKCustomGrid.PaintToCanvas(ACanvas: TCanvas);
var
  I, Bottom, ClientH, ClientW, GridW, GridH, SaveIndex: Integer;
  TmpExtent: TPoint;
  TmpRect: TRect;
  CurClipRgn, MainClipRgn: HRGN;
  DC: HDC;
  CellBitmap: TBitmap;
  Info: TKGridAxisInfoBoth;
  TmpBlockRect: TRect;
begin
  DC := ACanvas.Handle;
  SaveIndex := SaveDC(DC); // don't delete
  ACanvas.Lock;
  try
    if Enabled or (FDisabledDrawStyle = ddNormal) then
      FColors.ColorScheme := csNormal
    else if FDisabledDrawStyle = ddGrayed then
      FColors.ColorScheme := csGrayed
    else
      FColors.ColorScheme := csBright;
    ClientH := ClientHeight;
    ClientW := ClientWidth;
    Info := GetAxisInfoBoth([aiFixedParams]);
    GridW := 0; GridH := 0;
    TmpExtent := Point(0, 0);
    if (goDoubleBufferedCells in FOptions) and not DoubleBuffered then
      CellBitmap := TBitmap.Create
    else
      CellBitmap := nil;
    MainClipRgn := CreateEmptyRgn;
    CurClipRgn := CreateEmptyRgn;
    try
      TmpBlockRect := SelectionRect;
      if GetClipRgn(DC, MainClipRgn) <> 1 then
      begin
        DeleteObject(MainClipRgn);
        TmpRect := Rect(0, 0, ClientW, ClientH);
        TranslateRectToDevice(DC, TmpRect);
        MainClipRgn := CreateRectRgnIndirect(TmpRect);
      end;
      // draw clipped selectable cells first (to avoid some GTK clipping problems)
      TmpRect := Rect(Info.Horz.FixedBoundary, Info.Vert.FixedBoundary, ClientW, ClientH);
      if not IsRectEmpty(TmpRect) then
      begin
        TranslateRectToDevice(DC, TmpRect);
        if ExtSelectClipRectEx(DC, TmpRect, RGN_AND, CurClipRgn, MainClipRgn) then
        begin
          TmpExtent := PaintCells(ACanvas, CellBitmap, CurClipRgn, FTopLeft.Col, FColCount - 1, FTopLeft.Row, FRowCount - 1,
            Info.Horz.FixedBoundary - FScrollOffset.X, Info.Vert.FixedBoundary - FScrollOffset.Y, ClientW, ClientH, False, True, TmpBlockRect);
        end;
      end;
      GridW := Max(GridW, TmpExtent.X); GridH := Max(GridH, TmpExtent.Y);
      // clipped fixed rows
      TmpRect := Rect(Info.Horz.FixedBoundary, 0, ClientW, Info.Vert.FixedBoundary);
      if not IsRectEmpty(TmpRect) then
      begin
        TranslateRectToDevice(DC, TmpRect);
        if ExtSelectClipRectEx(DC, TmpRect, RGN_AND, CurClipRgn, MainClipRgn) then
          TmpExtent := PaintCells(ACanvas, CellBitmap, CurClipRgn, FTopLeft.Col, FColCount - 1, 0, FFixedRows - 1,
            Info.Horz.FixedBoundary - FScrollOffset.X, 0, ClientW, ClientH, False, True, TmpBlockRect);
      end;
      GridW := Max(GridW, TmpExtent.X); GridH := Max(GridH, TmpExtent.Y);
      // clipped fixed columns
      TmpRect := Rect(0, Info.Vert.FixedBoundary, Info.Horz.FixedBoundary, ClientH);
      if not IsRectEmpty(TmpRect) then
      begin
        TranslateRectToDevice(DC, TmpRect);
        if ExtSelectClipRectEx(DC, TmpRect, RGN_AND, CurClipRgn, MainClipRgn) then
          TmpExtent := PaintCells(ACanvas, CellBitmap, CurClipRgn, 0, FFixedCols - 1, FTopLeft.Row, FRowCount - 1, 0,
            Info.Vert.FixedBoundary - FScrollOffset.Y, ClientW, ClientH, False, True, TmpBlockRect);
      end;
      GridW := Max(GridW, TmpExtent.X); GridH := Max(GridH, TmpExtent.Y);
      // non-clipped fixed cells
      TmpRect := Rect(0, 0, Info.Horz.FixedBoundary, Info.Vert.FixedBoundary);
      if not IsRectEmpty(TmpRect) then
      begin
        TranslateRectToDevice(DC, TmpRect);
        if ExtSelectClipRectEx(DC, TmpRect, RGN_AND, CurClipRgn, MainClipRgn) then
          TmpExtent := PaintCells(ACanvas, CellBitmap, CurClipRgn, 0, FFixedCols - 1, 0, FFixedRows - 1,
          0, 0, ClientW, ClientH, False, True, TmpBlockRect);
      end;
      GridW := Max(GridW, TmpExtent.X); GridH := Max(GridH, TmpExtent.Y);
    finally
      FinalizePrevRgn(DC, MainClipRgn);
      DeleteObject(CurClipRgn);
      CellBitmap.Free;
    end;
    // draw a focus rectangle around cells in goRangeSelect and goRowSelect mode
    if not (csDesigning in ComponentState) and (goDrawFocusSelected in FOptions) and
      (FOptions * [goRangeSelect, goRowSelect] <> []) and Focused and not EditorMode then
    begin
      // to ensure coming DrawFocusRect will be painted correctly:
      SetBkColor(DC, $FFFFFF);
      SetTextColor(DC, 0);
      ACanvas.DrawFocusRect(TmpBlockRect);
    end;
    // default color for client area parts not consumed by cells
    ACanvas.Brush.Style := bsSolid;
    // fill window client area parts not consumed by cells
    if GridH < ClientH then
    begin
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(Rect(0, GridH, GridW, ClientH));
    end;
    if GridW < ClientW then
    begin
      if (goHeader in FOptions) and (goHeaderAlignment in FOptions) and (FFixedRows > 0) then
      begin
        Bottom := 0;
        for I := 0 to FFixedRows - 1 do
          Inc(Bottom, InternalGetRowHeights(I) + InternalGetEffectiveRowSpacing(I));
        PaintHeaderAlignment(ACanvas, Rect(GridW, 0, ClientW, Bottom));
      end else
        Bottom := 0;
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(Rect(GridW, Bottom, ClientW, ClientH));
    end;
    if FGridState in [gsColMoving, gsRowMoving] then PaintDragSuggestion(ACanvas);
    if FGridState in [gsColSizing, gsRowSizing] then PaintSizingSuggestion(ACanvas);
  finally
    RestoreDC(DC, SaveIndex);
    Canvas.Unlock;
  end;
end;

function TKCustomGrid.PointToCell(Point: TPoint; OutSide: Boolean;
  InvisibleCells: TKGridInvisibleCells; out HitCol, HitRow, SelCol, SelRow: Integer): Boolean;

  function Axis1(const Info: TKGridAxisInfo; Coord: Integer; InVis1, InVis2: TKGridInvisibleCells): Integer;
  var
    I, PtBegin, PtEnd, PtEOFixed: Integer;
  begin
    Result := -1;
    // check fixed cells
    I := 0;
    PtBegin := 0;
    while (I < Info.FixedCellCount) and (Result < 0) do
    begin
      PtEnd := PtBegin + Info.CellExtent(I) + Info.EffectiveSpacing(I);
      if (InvisibleCells in [icNone, InVis1]) {or (Info.FixedSelectable and (Info.FirstGridCell = Info.FixedCellCount)) } and
        (Coord >= PtBegin) and (Coord < PtEnd) then
        Result := I;
      PtBegin := PtEnd;
      Inc(I);
    end;
    if (Result < 0) then
    begin
      PtEOFixed := PtBegin - Info.ScrollOffset;
      I := Info.FirstGridCell;
      if (Coord < PtEOFixed) and (InvisibleCells in [InVis2, icCells]) then
      begin
        // check the invisible cells to the left or top
        PtEnd := PtEOFixed;
        while (I > Info.FixedCellCount) and (Result < 0) do
        begin
          Dec(I);
          PtBegin := PtEnd - Info.CellExtent(I) - Info.EffectiveSpacing(I);
          if (Coord >= PtBegin) and (Coord < PtEnd) then
            Result := I;
          PtEnd := PtBegin;
        end;
        if OutSide and (Result < 0) then
          if Info.FixedSelectable then
            Result := 0
          else
            Result := Info.FixedCellCount;
      end else
      begin
        // check visible cells and invisible ones to the right or bottom
        PtBegin := PtEOFixed;
        while (I < Info.TotalCellCount) and (Result < 0) do
        begin
          PtEnd := PtBegin + Info.CellExtent(I) + Info.EffectiveSpacing(I);
          if (Coord >= PtBegin) and (Coord < PtEnd) then
            Result := I;
          PtBegin := PtEnd;
          Inc(I);
        end;
        if OutSide and (Result < 0) then
          Result := Info.TotalCellCount - 1;
      end;
    end;
  end;

  function Axis2(const Info: TKGridAxisInfo; Index: Integer): Integer;
  begin
    if Index = Info.FixedCellCount then
    begin
      // some first nonfixed columns or rows may be hidden, so take first visible column/row
      while (Index < Info.TotalCellCount) and (Info.CellExtent(Index) = 0) do
        Inc(Index);
      if Index >= Info.TotalCellCount then
        Index := Info.FixedCellCount;
    end
    else if Index = Info.TotalCellCount - 1 then
    begin
      // some last columns or rows may be hidden, so take last visible column/row
      while (Index >= Info.FixedCellCount) and (Info.CellExtent(Index) = 0) do
        Dec(Index);
      if Index < Info.FixedCellCount then
        Index := Info.TotalCellCount - 1;
    end;
    Result := Index;
  end;

var
  Info: TKGridAxisInfo;
begin
  Result := False;
  Info := GetAxisInfoHorz([]);
  HitCol := Axis1(Info, Point.X, icFixedRows, icFixedCols);
  if HitCol >= 0 then
  begin
    if OutSide then SelCol := Axis2(Info ,HitCol) else SelCol := HitCol;
    Info := GetAxisInfoVert([]);
    HitRow := Axis1(Info, Point.Y, icFixedCols, icFixedRows);
    if HitRow >= 0 then
    begin
      if OutSide then SelRow := Axis2(Info, HitRow) else SelRow := HitRow;
      Result := True;
    end;
  end
end;

function TKCustomGrid.PointToSizing(Point: TPoint; var State: TKGridState;
  var Index, Pos: Integer): Boolean;

  function AxisSizing(const Info: TKGridAxisInfo; Coord: Integer;
    var Index, Pos: Integer): Boolean;
  const
    cDelta = 3;
  var
    I, ICopy, Dummy, ES, Line, StartCell: Integer;
  begin
    Result := False;
    if (Info.FullVisCells < Info.GridCells) and
      (Coord >= Info.ClientExtent - cDelta) and (Coord <= Info.ClientExtent) then
    begin
      Index := Info.FullVisCells;
      Pos := Info.ClientExtent;
      Result := True;
    end else
    begin
      Line := Info.FullVisBoundary;
      StartCell := Info.FullVisCells - 1;
      for I := StartCell downto Info.FirstGridCell do
      begin
        ES := Info.EffectiveSpacing(I);
        ICopy := I;
        if ((I < StartCell) or not Info.AlignLastCell) and
          ({(Info.CellExtent(I) <> 0) or (I = StartCell) and} Info.CanResize(ICopy, Dummy)) and
          (Coord >= Line - ES - cDelta) and (Coord <= Line + cDelta) then
        begin
          Index := I;
          Pos := Line;
          Result := True;
          Break;
        end;
        Dec(Line, Info.CellExtent(I) + ES);
      end;
      if not Result then
      begin
        Line := Info.FixedBoundary;
        for I := Info.FixedCellCount - 1 downto 0 do
        begin
          ES := Info.EffectiveSpacing(I);
          ICopy := I;
          if (Coord >= Line - ES - cDelta) and (Coord <= Line + cDelta) and Info.CanResize(ICopy, Dummy) then
          begin
            Index := I;
            Pos := Line;
            Result := True;
            Break;
          end;
          Dec(Line, Info.CellExtent(I) + ES);
        end;
      end;
    end;
  end;

var
  EffColSizing, EffRowSizing: Boolean;
  Info: TKGridAxisInfoBoth;
begin
  Result := False;
  EffColSizing := (goColSizing in FOptions) or (csDesigning in ComponentState);
  EffRowSizing := (goRowSizing in FOptions) or (csDesigning in ComponentState);
  if EffColSizing or EffRowSizing then
  begin
    Info := GetAxisInfoBoth([aiFullVisBoundary, aiGridBoundary]);
    if EffColSizing and AxisSizing(Info.Horz, Point.X, Index, Pos) and
      ((Point.Y < Info.Vert.FixedBoundary) or (Point.Y < Info.Vert.GridBoundary) and
      (InternalGetColWidths(Index) = 0)) then
    begin
      Result := True;
      State := gsColSizing;
    end
    else if EffRowSizing and AxisSizing(Info.Vert, Point.Y, Index, Pos) and
      ((Point.X < Info.Horz.FixedBoundary) or (Point.X < Info.Horz.GridBoundary) and
      (InternalGetRowHeights(Index) = 0)) then
    begin
      Result := True;
      State := gsRowSizing;
    end;
  end;
end;

procedure TKCustomGrid.PostRecreateEditor;
begin
  if HandleAllocated then
    PostMessage(Handle, GM_RECREATE_EDITOR, 0, 0);
end;

procedure TKCustomGrid.ProcessDragWindow(const PtIni, PtCur: TPoint; Index: Integer; ColDrag, Hide: Boolean);
var
  MaxWidth, MaxHeight: Integer;
  Alpha: Byte;
  Gradient: Boolean;
  RClip, RSrc, RDest: TRect;
  P: TKGridCoord;
  Form: TCustomForm;
  Info: TKGridAxisInfoBoth;
begin
  if FDragStyle in [dsLayeredConst, dsLayeredFaded] then
  begin
    if Index >= 0 then
    begin
      // (re)initialize drag image bitmaps
      if ColDrag then
        P := GridPoint(Index, 0)
      else
        P := GridPoint(0, Index);
      if CellToPoint(P.Col, P.Row, RSrc.TopLeft) then
      begin
        Form := GetParentForm(Self);
        if Form <> nil then
        begin
          MaxWidth := Min(ClientWidth, Form.ClientWidth - Left);
          MaxHeight := Min(ClientHeight, Form.ClientHeight - Top);
        end else
        begin
          MaxWidth := ClientWidth;
          MaxHeight := ClientHeight;
        end;
        Info := GetAxisInfoBoth([aiGridBoundary]);
        if ColDrag then
        begin
          RSrc.Right := RSrc.Left + GetColWidths(Index);
          RSrc.Bottom := Info.Vert.GridBoundary;
          RClip := Rect(Info.Horz.FixedBoundary, 0, MaxWidth, MaxHeight);
        end else
        begin
          RSrc.Bottom := RSrc.Top + GetRowHeights(Index);
          RSrc.Right := Info.Horz.GridBoundary;
          RClip := Rect(0, Info.Vert.FixedBoundary, MaxWidth, MaxHeight);
        end;
        if IntersectRect(RDest, RSrc, RClip) then
        begin
          if FDragWindow = nil then FDragWindow := TKDragWindow.Create;
          if FDragStyle = dsLayeredFaded then
          begin
            Alpha := $E0;
            Gradient := True;
          end else
          begin
            Alpha := $80;
            Gradient := False;
          end;
          EditorMode := False;
          Update;
          FDragWindow.Show(Self, RDest, PtIni, PtCur, Alpha, Gradient);
        end;
      end;
    end
    else if FDragWindow <> nil then
    begin
      if Hide then
        FDragWindow.Hide
      else
        FDragWindow.Move(PtCur);
    end;
  end;
end;

procedure TKCustomGrid.RealizeCellClass;
var
  I, J: Integer;
  Cell, TmpCell: TKGridCell;
  UpdateNeeded: Boolean;
begin
  if Assigned(FCells) then
  begin
    UpdateNeeded := False;
    for I := 0 to FColCount - 1 do
      for J := 0 to FRowCount - 1 do
      begin
        Cell := FCells[J, I];
        if (Cell <> nil) and (Cell.ClassType <> FCellClass) then
        begin
          TmpCell := FCellClass.Create(Self);
          FlagSet(cGF_GridUpdates);
          try
            TmpCell.Assign(Cell); // copy known properties
          finally
            FlagClear(cGF_GridUpdates);
          end;
          Cell.Free;
          FCells[J, I] := TmpCell;
          UpdateNeeded := True;
        end;
      end;
    if UpdateNeeded then
      Invalidate;
  end;
end;

procedure TKCustomGrid.RealizeColClass;
var
  I: Integer;
  TmpItem: TKGridAxisItem;
  UpdateNeeded: Boolean;
begin
  UpdateNeeded := False;
  for I := 0 to FColCount - 1 do
    if FCols[I].ClassType <> FColClass then
    begin
      TmpItem := FColClass.Create(Self);
      FlagSet(cGF_GridUpdates);
      try
        TmpItem.Assign(FCols[I]);
        TmpItem.InitialPos := FCols[I].InitialPos;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      FCols[I].Free;
      FCols[I] := TmpItem;
      UpdateNeeded := True;
    end;
  if UpdateNeeded then
    UpdateAxes(True, cAll, False, cAll, []);
end;

procedure TKCustomGrid.RealizeRowClass;
var
  I: Integer;
  TmpItem: TKGridAxisItem;
  UpdateNeeded: Boolean;
begin
  UpdateNeeded := False;
  for I := 0 to FRowCount - 1 do
    if FRows[I].ClassType <> FRowClass then
    begin
      TmpItem := FRowClass.Create(Self);
      FlagSet(cGF_GridUpdates);
      try
        TmpItem.Assign(FRows[I]);
        TmpItem.InitialPos := FRows[I].InitialPos;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      FRows[I].Free;
      FRows[I] := TmpItem;
      UpdateNeeded := True;
    end;
  if UpdateNeeded then
    UpdateAxes(False, cAll, True, cAll, []);
end;

procedure TKCustomGrid.ReadColWidths(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListBegin;
    for I := 0 to FColCount - 1 do ColWidths[I] := ReadInteger;
    ReadListEnd;
  end;
end;

procedure TKCustomGrid.ReadRowHeights(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListBegin;
    for I := 0 to FRowCount - 1 do RowHeights[I] := ReadInteger;
    ReadListEnd;
  end;
end;

procedure TKCustomGrid.ResetTopLeft;
begin
  if (FTopLeft.Col <> FFixedCols) or (FTopLeft.Row <> FFixedRows) then
  begin
    FTopLeft := GridPoint(FFixedCols, FFixedRows);
    Invalidate;
    TopLeftChanged;
  end;
end;

procedure TKCustomGrid.RowHeightsChanged(ARow: Integer);
begin
  if Assigned(FOnRowHeightsChanged) then
    FOnRowHeightsChanged(Self)
  else if Assigned(FOnRowHeightsChangedEx) then
    FOnRowHeightsChangedEx(Self, ARow)
end;

procedure TKCustomGrid.RowMoved(FromIndex, ToIndex: Integer);
begin
  if Assigned(FOnRowMoved) then
    FOnRowMoved(Self, FromIndex, ToIndex);
end;

function TKCustomGrid.RowSelectable(ARow: Integer): Boolean;
begin
  Result := (ARow >= FFixedRows) and (ARow < FRowCount);
end;

function TKCustomGrid.RowSelected(ARow: Integer): Boolean;
begin
  Result := RowInGridRect(ARow, FSelection);
end;

function TKCustomGrid.RowValid(ARow: Integer): Boolean;
begin
  Result := (ARow >= 0) and (ARow < FRowCount);
end;

procedure TKCustomGrid.SafeSetFocus;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and not (csDestroying in Form.ComponentState) then
    if EditorMode and FEditor.Enabled then
      Form.ActiveControl := FEditor
    else if Visible and Enabled then
      Form.ActiveControl := Self;
end;

procedure TKCustomGrid.Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer;
  CallUpdateEditor: Boolean);

  function Axis(Code: Cardinal; HasScrollBar: Boolean; ScrollCode: Cardinal; Delta: Integer;
    ScrollMode: TKGridScrollMode; const Info: TKGridAxisInfo;
    var FirstGridCell, ScrollPos, ScrollOffset: Integer): Boolean;

    procedure DoScroll(ADelta: Integer; OnlyIfGreater: Boolean = False);
    var
      I, TotalExtent: Integer;
    begin
      I := 0;
      ScrollOffset := 0;
      if ADelta > 0 then
      begin
        while (I < ADelta) and (FirstGridCell < Info.FirstGridCellExtent) do
        begin
          TotalExtent := Info.CellExtent(FirstGridCell) + Info.EffectiveSpacing(FirstGridCell);
          if OnlyIfGreater then
            if I + TotalExtent > ADelta then
            begin
              if ScrollMode = smSmooth then
                ScrollOffset := ADelta - I;
              Break;
            end;
          Inc(I, TotalExtent);
          Inc(FirstGridCell);
        end;
      end
      else if ADelta < 0 then
      begin
        while (I > ADelta) and (FirstGridCell > Info.FixedCellCount) do
        begin
          Dec(FirstGridCell);
          TotalExtent := Info.CellExtent(FirstGridCell) + Info.EffectiveSpacing(FirstGridCell);
          if OnlyIfGreater then
            if I - TotalExtent < ADelta then
            begin
              if ScrollMode = smSmooth then
              begin
                ScrollOffset := ADelta - I + TotalExtent;
                Dec(ScrollPos, TotalExtent);
              end else
                Inc(FirstGridCell);
              Break;
            end;
          Dec(I, TotalExtent);
        end;
      end;
      Inc(ScrollPos, I);
    end;

  var
    OldScrollPos, OldScrollOffset: Integer;
    SI: TScrollInfo;
  begin
    Result := False;
    if HasScrollBar then
    begin
      FillChar(SI, SizeOf(TScrollInfo), 0);
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_PAGE or SIF_RANGE or SIF_TRACKPOS;
      GetScrollInfo(Handle, Code, SI);
    {$IF DEFINED(LCLGTK2)}
      {.$WARNING "scrollbar arrows still not working properly on GTK2 in some cases!"}
      SI.nTrackPos := Delta;
    {$IFEND}
    end;
    OldScrollPos := ScrollPos;
    OldScrollOffset := ScrollOffset;
    if ScrollCode = Cardinal(cScrollDelta) then
      DoScroll(Delta) // in Pixels!
    else if HasScrollBar then
      case ScrollCode of
        SB_TOP:
        begin
          FirstGridCell := Info.FixedCellCount;
          ScrollPos := SI.nMin;
          ScrollOffset := 0;
        end;
        SB_BOTTOM:
        begin
          FirstGridCell := Info.FirstGridCellExtent;
          ScrollPos := SI.nMax - Max(SI.nPage - 1, 0);
          ScrollOffset := 0;
        end;
        SB_LINEUP: DoScroll(ScrollDeltaFromDelta(Info, -1));
        SB_LINEDOWN: DoScroll(ScrollDeltaFromDelta(Info, 1));
        SB_PAGEUP: DoScroll(-SI.nPage);
        SB_PAGEDOWN: DoScroll(SI.nPage);
        SB_THUMBTRACK, SB_THUMBPOSITION: DoScroll(SI.nTrackPos - ScrollPos, True);
      end;
    FirstGridCell := MinMax(FirstGridCell, Info.FixedCellCount, Info.FirstGridCellExtent);
    if (ScrollPos <> OldScrollPos) or (ScrollOffset <> OldScrollOffset) then
    begin
      if HasScrollBar then
      begin
        FillChar(SI, SizeOf(TScrollInfo), 0);
        SI.cbSize := SizeOf(TScrollInfo);
        SI.fMask := SIF_POS;
        SI.nPos := ScrollPos + ScrollOffset;
        SetScrollInfo(Handle, Code, SI, True);
      end;
      Result := True;
    end;
  end;

var
  Horz, Vert: Boolean; //because of $B-
  OldTopLeft: TKGridCoord;
begin
  OldTopLeft := FTopLeft;
  Horz := Axis(SB_HORZ, FScrollBars in [ssHorizontal, ssBoth], CodeHorz, DeltaHorz,
    FScrollModeHorz, GetAxisInfoHorz([]), FTopLeft.Col, FScrollPos.x, FScrollOffset.X);
  Vert := Axis(SB_VERT, FScrollBars in [ssVertical, ssBoth], CodeVert, DeltaVert,
    FScrollModeVert, GetAxisInfoVert([]), FTopLeft.Row, FScrollPos.y, FScrollOffset.Y);
  if Horz or Vert then
  begin
    if Horz then
      InvalidateCols(FFixedCols);
    if Vert then
      InvalidateRows(FFixedRows);
    if CallUpdateEditor then
      UpdateEditor(Flag(cGF_EditorModeActive));
    if (OldTopLeft.Col <> FTopLeft.Col) or (OldTopLeft.Row <> FTopLeft.Row) then
      TopLeftChanged;
  end;
end;

procedure TKCustomGrid.ScrollBy(AColCount, ARowCount: Integer);
begin
  Scroll(cScrollDelta, cScrollDelta,
    ScrollDeltaFromDelta(GetAxisInfoHorz([]), AColCount),
    ScrollDeltaFromDelta(GetAxisInfoVert([]), ARowCount),
    True);
end;

function TKCustomGrid.ScrollDeltaFromDelta(const Info: TKGridAxisInfo; ADelta: Integer): Integer;
var
  I, CellExtent, MaxIndex: Integer;
begin
  Result := 0;
  I := Info.FirstGridCell;
  MaxIndex := Info.FirstGridCell + ADelta;
  if ADelta > 0 then
  begin
    while (I < Info.FirstGridCellExtent) do
    begin
      CellExtent := Info.CellExtent(I);
      Inc(Result, CellExtent + Info.EffectiveSpacing(I));
      Inc(I);
      if (CellExtent > 0) and (I >= MaxIndex) then
        Break;
    end;
  end
  else if ADelta < 0 then
  begin
    while (I > Info.FixedCellCount) do
    begin
      Dec(I);
      CellExtent := Info.CellExtent(I);
      Dec(Result, CellExtent + Info.EffectiveSpacing(I));
      if (CellExtent > 0) and (I <= MaxIndex) then
        Break;
    end;
  end;
end;

function TKCustomGrid.ScrollNeeded(ACol, ARow: Integer;
  out DeltaHorz, DeltaVert: Integer): Boolean;

  function Axis(Info: TKGridAxisInfo; Index, Span: Integer; var Delta: Integer): Boolean;
  var
    I, Extent: Integer;
  begin
    Result := False;
    Delta := 0;
    if Index < Info.FixedCellCount then Exit;
    if Index = Info.FirstGridCell then
    begin
      if Info.ScrollOffset <> 0 then
        Result := True;
    end;
    if Index < Info.FirstGridCell then
    begin
      for I := Info.FirstGridCell - 1 downto Index do
        Dec(Delta, Info.CellExtent(I) + Info.EffectiveSpacing(I));
      Result := True;  
    end else
    begin
      Extent := Info.FixedBoundary - Info.ScrollOffset;
      for I := Info.FirstGridCell to Index - 1 do
        Inc(Extent, Info.CellExtent(I) + Info.EffectiveSpacing(I));
      Delta := Extent;
      for I := Index to Index + Span - 1 do
        Inc(Delta, Info.CellExtent(I) + Info.EffectiveSpacing(I));
      if Delta > Info.ClientExtent then
      begin
        Delta := Min(Extent - Info.FixedBoundary, Delta + Info.ScrollOffset - Info.ClientExtent);
        if Delta > 0 then
          Result := True;
      end else
        Delta := 0;
    end;
  end;

var
  Horz, Vert: Boolean;
  Span: TKGridCellSpan;
begin
  DeltaHorz := 0; DeltaVert := 0;
  Span := InternalGetCellSpan(ACol, ARow);
  Horz := (FGridState <> gsRowMoving) and Axis(GetAxisInfoHorz([aiFixedParams]), ACol, Span.ColSpan, DeltaHorz);
  Vert := (FGridState <> gsColMoving) and Axis(GetAxisInfoVert([aiFixedParams]), ARow, Span.RowSpan, DeltaVert);
  Result := Horz or Vert;
end;

procedure TKCustomGrid.ScrollTimerHandler(Sender: TObject);
var
  DeltaHorz, DeltaVert, HitCol, HitRow, SelCol, SelRow: Integer;
  MousePt: TPoint;
begin
  MousePt := ScreenToClient(Mouse.CursorPos);
  if MouseCapture and not Dragging and
    PointToCell(MousePt, True, GridStateToInvisibleCells, HitCol, HitRow,
    SelCol, SelRow) and ScrollNeeded(HitCol, HitRow, DeltaHorz, DeltaVert) then
  begin
    Scroll(cScrollDelta, cScrollDelta, DeltaHorz, DeltaVert, False);
    if FGridState = gsSelecting then
    begin
      InternalFindBaseCell(SelCol, SelRow, SelCol, SelRow);
      if (Col <> SelCol) or (Row <> SelRow) then
        CellChanging(Col, Row, SelCol, SelRow);
      SelectionMove(SelCol, SelRow, ssExpand, [sfMustUpdate])
    end else
      DragMove(HitCol, HitRow, MousePt);
  end else
  begin
    FScrollTimer.Enabled := False;
    UpdateEditor(Flag(cGF_EditorModeActive));
  end;
end;

procedure TKCustomGrid.SelectAll;
begin
  if goRangeSelect in Options then
    // aki:
    if (gxEditFixedRows in FOptionsEx) and (gxEditFixedCols in FOptionsEx) then
      Selection := GridRect(0, 0, FColCount - 1, FRowCount - 1)
    else if gxEditFixedRows in FOptionsEx then
      Selection := GridRect(FFixedCols, 0, FColCount - 1, FRowCount - 1)
    else if gxEditFixedCols in FOptionsEx then
      Selection := GridRect(0, FFixedRows, FColCount - 1, FRowCount - 1)
    else
      Selection := GridRect(FFixedCols, FFixedRows, FColCount - 1, FRowCount - 1);
end;

function TKCustomGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  // aki:
  if (ColWidths[ACol] = 0) or (RowHeights[ARow] = 0) then
    Result := False
  else if (ARow < FFixedRows) and (not(gxEditFixedRows in FOptionsEx)) then
    Result := False
  else if not (gxEditFixedCols in FOptionsEx) and not (gxEditFixedRows in FOptionsEx) and (ACol < FFixedCols) then
    Result := False
  else
  begin
    Result := True;
    if Assigned(FOnSelectCell) then
      FOnSelectCell(Self, ACol, ARow, Result)
    else if Assigned(FCells) then
      InternalGetCell(ACol, ARow).SelectCell(ACol, ARow, Result);
  end;
end;

procedure TKCustomGrid.SelectCol(ACol: Integer);
begin
  if goRangeSelect in Options then
    // aki:
    if gxEditFixedRows in FOptionsEx then
      Selection := GridRect(ACol, 0, ACol, FRowCount - 1)
    else
      Selection := GridRect(ACol, FFixedRows, ACol, FRowCount - 1);
end;

procedure TKCustomGrid.SelectCols(FirstCol, Count: Integer);
begin
  if goRangeSelect in Options then
    // aki:
    if gxEditFixedRows in FOptionsEx then
      Selection := GridRect(FirstCol, 0, FirstCol + Count, FRowCount - 1)
    else
      Selection := GridRect(FirstCol, FFixedRows, FirstCol + Count, FRowCount - 1);
end;

procedure TKCustomGrid.SelectionChanged(NewSelection: TKGridRect;
  Flags: TKGridSelectionFlags);
var
  ICol, IRow: Integer;
begin
  SelectionFix(NewSelection);
  if FRangeSelectStyle = rsMS_Excel then
  begin
    ICol := NewSelection.Col2;
    IRow := NewSelection.Row2;
  end else
  begin
    ICol := NewSelection.Col1;
    IRow := NewSelection.Row1;
  end;
  if (sfMustUpdate in Flags) and not GridRectEqual(FSelection, NewSelection) then
  begin
    if not (goAlwaysShowEditor in FOptions) then
      FlagClear(cGF_EditorModeActive);
    InvalidateCurrentSelection;
    FSelection := NewSelection;
    if not (sfClampInView in Flags) or not ClampInView(ICol, IRow) then
      InvalidateCurrentSelection;
  end else
    FSelection := NewSelection;
  InvalidatePageSetup;
  if not (sfNoMemPos in Flags) then
  begin
    FMemCol := ICol;
    FMemRow := IRow;
  end;
  if sfMustUpdate in Flags then
    UpdateEditor(Flag(cGF_EditorModeActive));
end;

function TKCustomGrid.SelectionExpand(ACol, ARow: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectionExpand) then
    FOnSelectionExpand(Self, ACol, ARow, Result)
  else if Assigned(FCells) then
    InternalGetCell(ACol, ARow).SelectionExpand(ACol, ARow, Result);
end;

procedure TKCustomGrid.SelectionFix(var Sel: TKGridRect);
begin
  //aki:
  if (not (gxEditFixedCols in FOptionsEx) and (Sel.Row1 >= FFixedRows)) or (not (gxEditFixedRows in FOptionsEx) and (Sel.Row1 < FFixedRows)) then
  begin
    Sel.Col1 := MinMax(Sel.Col1, FFixedCols, FColCount - 1);
    Sel.Col2 := MinMax(Sel.Col2, FFixedCols, FColCount - 1);
  end else
  begin
    Sel.Col1 := MinMax(Sel.Col1, 0, FColCount - 1);
    Sel.Col2 := MinMax(Sel.Col2, 0, FColCount - 1);
  end;
  if not (gxEditFixedRows in FOptionsEx) then
  begin
    Sel.Row1 := MinMax(Sel.Row1, FFixedRows, FRowCount - 1);
    Sel.Row2 := MinMax(Sel.Row2, FFixedRows, FRowCount - 1);
  end else
  begin
    Sel.Row1 := MinMax(Sel.Row1, 0, FRowCount - 1);
    Sel.Row2 := MinMax(Sel.Row2, 0, FRowCount - 1);
  end;
  if not (goRangeSelect in FOptions) then
    Sel.Cell2 := Sel.Cell1
end;

function TKCustomGrid.SelectionMove(ACol, ARow: Integer;
  Stage: TKGridSelectionStage;
  Flags: TKGridSelectionFlags): Boolean;
var
  NewSelection: TKGridRect;
begin
  Result := False;
  if (Stage = ssExpand) and not (goRangeSelect in FOptions) then
    Stage := ssInit;
  case Stage of
    ssInit:
    begin
      NewSelection := GridRect(ACol, ARow, ACol, ARow);
      if not GridRectEqual(FSelection, NewSelection) and
        ((sfDontCallSelectCell in Flags) or SelectCell(ACol, ARow)) then
        Result := True;
    end;
    ssExpand:
    begin
      NewSelection := FSelection;
      if FRangeSelectStyle = rsMS_Excel then
      begin
        NewSelection.Cell2 := GridPoint(ACol, ARow);
        if not GridRectEqual(FSelection, NewSelection) and
          ((sfDontCallSelectCell in Flags) or SelectionExpand(ACol, ARow)) then
          Result := True;
      end else
      begin
        NewSelection.Cell1 := GridPoint(ACol, ARow);
        if not GridRectEqual(FSelection, NewSelection) and
          ((sfDontCallSelectCell in Flags) or SelectCell(ACol, ARow)) then
          Result := True;
      end;
    end;
  end;
  if Result then
    SelectionChanged(NewSelection, Flags);
  if not Result and GridRectEqual(NewSelection, FSelection) then
    Result := True;
end;

procedure TKCustomGrid.SelectionNormalize;
var
  R: TKGridRect;
begin
  R := Selection;
  NormalizeGridRect(R);
  Selection := R;
end;

function TKCustomGrid.SelectionSet(const NewSelection: TKGridRect): Boolean;
begin
  Result := False;
  if not GridRectEqual(FSelection, NewSelection) then
  begin
    if ((FSelection.Col1 <> NewSelection.Col1) or (FSelection.Row1 <> NewSelection.Row1)
      and SelectCell(NewSelection.Col1, NewSelection.Row1)) or
      ((FSelection.Col2 <> NewSelection.Col2) or (FSelection.Row2 <> NewSelection.Row2)
      and SelectionExpand(NewSelection.Col2, NewSelection.Row2)) then
      Result := True;
  end;
  if Result then
    SelectionChanged(NewSelection, [sfMustUpdate, sfClampInView]);
//  if not Result and GridRectEqual(NewSelection, FSelection) then
//    Result := True;
end;

procedure TKCustomGrid.SelectRow(ARow: Integer);
begin
  if goRangeSelect in Options then
    Selection := GridRect(FFixedCols, ARow, FColCount - 1, ARow);
end;

procedure TKCustomGrid.SelectRows(FirstRow, Count: Integer);
begin
  if goRangeSelect in Options then
    Selection := GridRect(FFixedCols, FirstRow, FColCount - 1, FirstRow + Count);
end;

procedure TKCustomGrid.SetCell(ACol, ARow: Integer; Value: TKGridCell);
begin
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
    InternalSetCell(ACol, ARow, Value);
end;

procedure TKCustomGrid.SetCellPainterClass(Value: TKGridCellPainterClass);
begin
  if Value <> FCellPainterClass then
  begin
    FCellPainterClass := Value;
    FCellPainter.Free;
    FCellPainter := FCellPainterClass.Create(Self);
  end;
end;

procedure TKCustomGrid.SetCells(ACol, ARow: Integer; const Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});
begin
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
    InternalSetCells(ACol, ARow, Text);
end;

procedure TKCustomGrid.SetCellSpan(ACol, ARow: Integer; Value: TKGridCellSpan);
var
  I: Integer;
begin
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
  begin
    // cells cannot be merged across fixed area boundaries
    if ACol >= FFixedCols then I := FColCount else I := FFixedCols;
    Value.ColSpan := MinMax(Value.ColSpan, 1, I - ACol);
    if ARow >= FFixedRows then I := FRowCount else I := FFixedRows;
    Value.RowSpan := MinMax(Value.RowSpan, 1, I - ARow);
    with InternalGetCell(ACol, ARow) do
      if (ColSpan <> Value.ColSpan) or (RowSpan <> Value.RowSpan) then
      begin
        EditorMode := False;
        FlagSet(cGF_GridUpdates);
        try
          InvalidateGridRect(InternalSetCellSpan(ACol, ARow, Value), False);
        finally
          FlagClear(cGF_GridUpdates);
        end;
      end;
  end;
end;

procedure TKCustomGrid.SetCol(Value: Integer);
begin
  if ColSelectable(Value) and ((Value <> FSelection.Col1) or (FSelection.Col1 <> FSelection.Col2)) then
    FocusCell(Value, Row);
end;

procedure TKCustomGrid.SetColCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  InternalSetColCount(Value);
end;

procedure TKCustomGrid.SetColWidths(Index: Integer; Value: Integer);
begin
  if ColValid(Index) then
  begin
    if Value < InternalGetMinColWidth(Index) then
      Value := 0
    else
      Value := Min(Value, InternalGetMaxColWidth(Index));
    if Value <> FCols[Index].Extent then
    begin
      FlagSet(cGF_GridUpdates);
      try
        FCols[Index].Extent := Value;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      UpdateAxes(True, Index, False, cAll, [afCallEvent, afCheckMinExtent]);
    end;
  end;
end;

procedure TKCustomGrid.SetColors(Value: TKGridColors);
begin
  FColors.Assign(Value);
end;

{$IFDEF FPC}
procedure TKCustomGrid.SetCursor(Value: TCursor);
begin
  FTmpCursor := Value;
  if (FCursor <> crHSplit) and (FCursor <> crVSplit) and
    (FCursor <> crHResize) and (FCursor <> crVResize) then
    inherited;
end;
{$ENDIF}

procedure TKCustomGrid.SetDefaultColWidth(Value: Integer);
begin
  if Value <> FDefaultColWidth then
  begin
    FDefaultColWidth := Value;
    DefaultColWidthChanged;
  end;
end;

procedure TKCustomGrid.SetDefaultDrawing(Value: Boolean);
begin
  // does nothing
end;

procedure TKCustomGrid.SetDefaultRowHeight(Value: Integer);
begin
  if Value <> FDefaultRowHeight then
  begin
    FDefaultRowHeight := Value;
    DefaultRowHeightChanged;
  end;
end;

procedure TKCustomGrid.SetDisabledDrawStyle(Value: TKGridDisabledDrawStyle);
begin
  if Value <> FDisabledDrawStyle then
  begin
    FDisabledDrawStyle := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TKCustomGrid.SetDragStyle(Value: TKGridDragStyle);
begin
  if Value <> FDragStyle then
  begin
    CancelMode;
    FDragStyle := Value;
  end;
end;

procedure TKCustomGrid.SetEditorMode(Value: Boolean);
begin
  if Value <> EditorMode then
    UpdateEditor(Value);
  FlagAssign(cGF_EditorModeActive, Value);
end;

procedure TKCustomGrid.SetEditorTransparency(Value: TKGridEditorTransparency);
begin
  if Value <> FEditorTransparency then
  begin
    FEditorTransparency := Value;
    if EditorMode then
      FEditor.Invalidate;
  end;
end;

procedure TKCustomGrid.SetFixedCols(Value: Integer);
begin
  if (Value <> FFixedCols) and (FFixedCols >= 0) then
    InternalSetFixedCols(Value);
end;

procedure TKCustomGrid.SetFixedRows(Value: Integer);
begin
  if (Value <> FFixedRows) and (FFixedRows >= 0) then
    InternalSetFixedRows(Value);
end;

{$IFDEF FPC}
procedure TKCustomGrid.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TKCustomGrid.SetGridLineWidth(Value: Integer);
begin
  if FGridLineWidth <> Value then
  begin
    FGridLineWidth := Value;
    UpdateAxes(FOptions * [goFixedHorzLine, goHorzLine] <> [], cAll,
      FOptions * [goFixedVertLine, goVertLine] <> [], cAll, []);
  end;
end;

procedure TKCustomGrid.SetLeftCol(Value: Integer);
begin
  if ColValid(Value) and (Value <> FTopLeft.Col) then
    ScrollBy(Value - FTopLeft.Col, 0);
end;

procedure TKCustomGrid.SetMinColWidth(Value: Integer);
var
  I, Extent, MinColWidth: Integer;
begin
  Value := Max(Value, cMinColWidthMin);
  if Value <> FMinColWidth then
  begin
    FMinColWidth := Value;
    if FMinColWidth > FDefaultColWidth then
    begin
      FDefaultColWidth := FMinColWidth;
      DefaultColWidthChanged;
    end else
    begin
      FlagSet(cGF_GridUpdates);
      try
        for I := 0 to FColCount - 1 do
        begin
          Extent := FCols[I].Extent;
          MinColWidth := InternalGetMinColWidth(I);
          if (Extent > 0) and (Extent < MinColWidth) then
            FCols[I].Extent := MinColWidth;
        end;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      UpdateAxes(True, cAll, False, cAll, []);
    end;
  end;
end;

procedure TKCustomGrid.SetMinRowHeight(Value: Integer);
var
  I, Extent, MinRowHeight: Integer;
begin
  Value := Max(Value, cMinRowHeightMin);
  if Value <> FMinRowHeight then
  begin
    FMinRowHeight := Value;
    if FMinRowHeight > FDefaultRowHeight then
    begin
      FDefaultRowHeight := FMinRowHeight;
      DefaultRowHeightChanged;
    end else
    begin
      FlagSet(cGF_GridUpdates);
      try
        for I := 0 to FRowCount - 1 do
        begin
          Extent := FRows[I].Extent;
          MinRowHeight := InternalGetMinRowHeight(I);
          if (Extent > 0) and (Extent < MinRowHeight) then
            FRows[I].Extent := MinRowHeight;
        end;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      UpdateAxes(False, cAll, True, cAll, []);
    end;
  end;
end;

procedure TKCustomGrid.SetMouseCellHintTime(const AValue: Cardinal);
begin
  FMouseCellHintTime := MinMax(AValue, cMouseCellHintTimeMin, cMouseCellHintTimeMax);
end;

function TKCustomGrid.SetMouseCursor(X, Y: Integer): Boolean;
var
  ACursor: TCursor;
  Index, Pos: Integer;
  State: TKGridState;
begin
{$IFDEF FPC}
  ACursor := FTmpCursor;
{$ELSE}
  ACursor := Cursor;
{$ENDIF}
  State := gsNormal;
  Index := 0; Pos := 0;
  PointToSizing(Point(X, Y), State, Index, Pos);
  case State of
    gsColSizing:
    begin
      FlagSet(cGF_DesignHitTest);
      if (FCols[Index].Extent = 0) or (csDesigning in ComponentState) then
        ACursor := crHSplit
      else
        ACursor := crHResize
    end;
    gsRowSizing:
    begin
      FlagSet(cGF_DesignHitTest);
      if (FRows[Index].Extent = 0) or (csDesigning in ComponentState) then
        ACursor := crVSplit
      else
        ACursor := crVResize;
    end;
  else
    FlagClear(cGF_DesignHitTest);
  end;
{$IFDEF FPC}
  FCursor := ACursor;
  SetTempCursor(ACursor);
{$ELSE}
  Windows.SetCursor(Screen.Cursors[ACursor]);
{$ENDIF}
  Result := True;
end;

procedure TKCustomGrid.SetObjects(ACol, ARow: Integer; Value: TObject);
var
  Cell, Tmp: TKGridCell;
begin
  if Assigned(FCells) and ColValid(ACol) and RowValid(ARow) then
  begin
    FlagSet(cGF_GridUpdates);
    try
      Cell := InternalGetCell(ACol, ARow);
      if not (Cell is TKGridObjectCell) then
      begin
        if FCellClass.InheritsFrom(TKGridObjectCell) then
          Tmp := FCellClass.Create(Self)
        else
          Tmp := TKGridObjectCell.Create(Self);
        Tmp.Assign(Cell);
        Cell.Free;
        FCells[ARow, ACol] := Tmp;
      end;
      TKGridObjectCell(FCells[ARow, ACol]).CellObject := Value;
    finally
      FlagClear(cGF_GridUpdates);
    end;
    InvalidateCell(ACol, ARow);
  end
end;

procedure TKCustomGrid.SetOptions(Value: TKGridOptions);
const
  UpdatePaintSet = [goClippedCells, goDrawFocusSelected, goDoubleBufferedCells,
    goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine,
    goIndicateSelection, goIndicateHiddenCells, goThemedCells, goThemes, goVertLine];
  UpdateScrollBarsSet = [goAlignLastCol, goAlignLastRow];
  UpdateSelectionSet = [goRangeSelect, goRowSelect];
  UpdateSortingSet = [goColSorting, goRowSorting];
var
  UpdateCols, UpdatePaint, UpdateRows, UpdateScrollBars,
  UpdateSelection, UpdateSorting, UpdateThemes, UpdateThemedCells,
  UpdateVirtualGrid, WasVirtual: Boolean;
begin
  if FOptions <> Value then
  begin
    UpdateCols := ((Value * [goHorzLine, goFixedHorzLine] = []) xor
      (FOptions * [goHorzLine, goFixedHorzLine] = [])) or
      ((goIndicateHiddenCells in Value) <> (goIndicateHiddenCells in FOptions));
    UpdateRows := ((Value * [goVertLine, goFixedVertLine] = []) xor
      (FOptions * [goVertLine, goFixedVertLine] = [])) or
      ((goIndicateHiddenCells in Value) <> (goIndicateHiddenCells in FOptions));
    UpdatePaint := Value * UpdatePaintSet <> FOptions * UpdatePaintSet;
    UpdateScrollBars := Value * UpdateScrollBarsSet <> FOptions * UpdateScrollBarsSet;
    UpdateSelection := Value * UpdateSelectionSet <> FOptions * UpdateSelectionSet;
    UpdateSorting := Value * UpdateSortingSet <> FOptions * UpdateSortingSet;
    UpdateThemes := (goThemes in Value) <> (goThemes in FOptions);
    UpdateThemedCells := (goThemedCells in Value) <> (goThemedCells in FOptions);
    UpdateVirtualGrid := (goVirtualGrid in Value) <> (goVirtualGrid in FOptions);
    WasVirtual := goVirtualGrid in FOptions;
    FOptions := Value;
    if UpdateSelection then
      SelectionFix(FSelection);
    if UpdateCols or UpdateRows then
      UpdateAxes(UpdateCols, cAll, UpdateRows, cAll, []);
    if UpdateScrollBars or UpdateThemes then
    {$IFDEF FPC}
      UpdateSize;
    {$ELSE}
      RecreateWnd;
    {$ENDIF}
    if UpdateSorting then
      ClearSortMode;
    if UpdateVirtualGrid then
    begin
      if InternalUpdateVirtualGrid then
        ChangeDataSize(False, 0, 0, False, 0, 0)
      else if WasVirtual then
        Include(FOptions, goVirtualGrid)
      else
        Exclude(FOptions, goVirtualGrid);
    end;
    if UpdatePaint or UpdateSelection or UpdateVirtualGrid then
    begin
      Invalidate;
      InvalidatePageSetup;
    end;
    if UpdateThemedCells then
      Include(FOptions, goMouseOverCells);
    if not (goEditing in FOptions) then
      EditorMode := False;
  end;
end;

procedure TKCustomGrid.SetOptionsEx(Value: TKGridOptionsEx);
const
  UpdatePaintSet = [gxFixedThemedCells];
var
  UpdatePaint: Boolean;
begin
  if FOptionsEx <> Value then
  begin
    UpdatePaint := Value * UpdatePaintSet <> FOptionsEx * UpdatePaintSet;
    FOptionsEx := Value;
    if UpdatePaint then
      Invalidate;
  end;
end;

procedure TKCustomGrid.SetRow(Value: Integer);
begin
  if RowSelectable(Value) and ((Value <> FSelection.Row1) or (FSelection.Row1 <> FSelection.Row2)) then
    FocusCell(Col, Value);
end;

procedure TKCustomGrid.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  InternalSetRowCount(Value);
end;

procedure TKCustomGrid.SetRowHeights(Index: Integer; Value: Integer);
begin
  if RowValid(Index) then
  begin
    if Value < InternalGetMinRowHeight(Index) then
      Value := 0
    else
      Value := Min(Value, InternalGetMaxRowHeight(Index));
    if Value <> FRows[Index].Extent then
    begin
      FlagSet(cGF_GridUpdates);
      try
        FRows[Index].Extent := Value;
      finally
        FlagClear(cGF_GridUpdates);
      end;
      UpdateAxes(False, cAll, True, Index, [afCallEvent, afCheckMinExtent]);
    end;
  end;
end;

procedure TKCustomGrid.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
  {$IFDEF FPC}
    UpdateSize;
  {$ELSE}
    RecreateWnd;
  {$ENDIF}
  end;
end;

procedure TKCustomGrid.SetScrollModeHorz(const Value: TKGridScrollMode);
begin
  if Value <> FScrollModeHorz then
  begin
    FScrollModeHorz := Value;
    UpdateScrollRange(True, False, True);
  end;
end;

procedure TKCustomGrid.SetScrollModeVert(const Value: TKGridScrollMode);
begin
  if Value <> FScrollModeVert then
  begin
    FScrollModeVert := Value;
    UpdateScrollRange(False, True, True);
  end;
end;

procedure TKCustomGrid.SetScrollSpeed(Value: Cardinal);
begin
  Value := MinMax(Integer(Value), cScrollSpeedMin, cScrollSpeedMax);
  if Value <> FScrollSpeed then
  begin
    FScrollSpeed := Value;
    FScrollTimer.Enabled := False;
    FScrollTimer.Interval := FScrollSpeed;
  end;
end;

procedure TKCustomGrid.SetSelection(const Value: TKGridRect);
var
  G: TKGridRect;
begin
  if GridRectSelectable(Value) and not GridRectEqual(Value, FSelection) then
  begin
    InternalFindBaseCell(Value.Col1, Value.Row1, G.Col1, G.Row1);
    InternalFindBaseCell(Value.Col2, Value.Row2, G.Col2, G.Row2);
    SelectionSet(G);
  end;
end;

procedure TKCustomGrid.SetSelections(Index: Integer; const Value: TKGridRect);
begin
  if (Index >= 0) and (Index < SelectionCount) then
  begin
    //TODO!
  end;
end;

procedure TKCustomGrid.SetSizingStyle(Value: TKGridSizingStyle);
begin
  if Value <> FSizingStyle then
  begin
    CancelMode;
    FSizingStyle := Value;
  end;
end;

procedure TKCustomGrid.SetTabStops(Index: Integer; Value: Boolean);
begin
  if ColValid(Index) and (FCols[Index] is TKGridCol) then
    TKGridCol(FCols[Index]).TabStop := Value;
end;

procedure TKCustomGrid.SetTopRow(Value: Integer);
begin
  if RowValid(Value) and (Value <> FTopLeft.Row) then
    ScrollBy(0, Value - FTopLeft.Row);
end;

procedure TKCustomGrid.ShowCellHint;
begin
  DefaultMouseCellHint(FHintCell.Col, FHintCell.Row, True);
end;

procedure TKCustomGrid.SizeChanged(Change: TKGridSizeChange;
  Index, Count: Integer);
begin
  if Assigned(FOnSizeChanged) then
    FOnSizeChanged(Self, Change, Index, Count);
end;

procedure TKCustomGrid.SortCols(ByRow: Integer; SortMode: TKGridSortMode);
var
  Sorted: Boolean;
  OldSortMode: TKGridSortMode;
begin
  if SortModeUnlocked and RowValid(ByRow) and (FRows[ByRow].SortMode <> SortMode) then
  begin
    OldSortMode := FRows[ByRow].SortMode;
    ClearSortModeVert;
    if FColCount > 1 then
    begin
      EditorMode := False;
      Sorted := CustomSortCols(ByRow, SortMode);
      if not Sorted and (SortMode <> smNone) then
      begin
        if OldSortMode <> smNone then
          InternalFlip(FFixedCols, FColCount - 1, InternalExchangeCols)
        else
          InternalQuickSortNR(ByRow, FFixedCols, FColCount - 1, SortMode = smDown,
            CompareCols, InternalExchangeCols);
      end;
      if Sorted or (SortMode <> smNone) then
      begin
        UpdateScrollRange(True, False, False);
        UpdateCellSpan;
        if not ClampInView(Col, Row) then
          InvalidateCols(FFixedCols);
      end;
      if SortMode <> smNone then
      begin
        FlagSet(cGF_GridUpdates);
        try
          FRows[ByRow].SortMode := SortMode;
        finally
          FlagClear(cGF_GridUpdates);
        end;
        Row := ByRow;
      end;
      InvalidateGridRect(GridRect(0, ByRow, FFixedCols - 1, ByRow));
    end;
  end;
end;

function TKCustomGrid.SortModeUnlocked: Boolean;
begin
  Result := FSortModeLock = 0;
end;

procedure TKCustomGrid.SortRows(ByCol: Integer; SortMode: TKGridSortMode);
var
  Sorted: Boolean;
  OldSortMode: TKGridSortMode;
begin
  if SortModeUnlocked and ColValid(ByCol) and (FCols[ByCol].SortMode <> SortMode) then
  begin
    OldSortMode := FCols[ByCol].SortMode;
    ClearSortModeHorz;
    if FRowCount > 1 then
    begin
      EditorMode := False;
      Sorted := CustomSortRows(ByCol, SortMode);
      if not Sorted and (SortMode <> smNone) then
      begin
        if OldSortMode <> smNone then
          InternalFlip(FFixedRows, FRowCount - 1, InternalExchangeRows)
        else
          InternalQuickSortNR(ByCol, FFixedRows, FRowCount - 1, SortMode = smDown,
            CompareRows, InternalExchangeRows);
      end;
      if Sorted or (SortMode <> smNone) then
      begin
        UpdateScrollRange(False, True, False);
        UpdateCellSpan;
        if not ClampInView(Col, Row) then
          InvalidateRows(FFixedRows);
      end;
      if SortMode <> smNone then
      begin
        FlagSet(cGF_GridUpdates);
        try
          FCols[ByCol].SortMode := SortMode;
        finally
          FlagClear(cGF_GridUpdates);
        end;
        Col := ByCol;
      end;
      InvalidateGridRect(GridRect(ByCol, 0, ByCol, FFixedRows - 1));
    end;
  end;
end;

procedure TKCustomGrid.SuggestDrag(State: TKGridCaptureState);
var
  R: TRect;
begin
  if HandleAllocated and GetDragRect(GetAxisInfoBoth([aiGridBoundary]), R) then
  begin
    if State = csStart then
    begin
      InvalidateCell(FHitCell.Col, FHitCell.Row);
      Update;
    end;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TKCustomGrid.SuggestSizing(State: TKGridCaptureState);

  function Axis(const Info: TKGridAxisInfo; CellPt: Integer; AxisItems: TKGridAxisItems): Integer;
  var
    Tmp, MinExtent: Integer;
  begin
    Result := Info.CellExtent(FSizingIndex);
    MinExtent := Info.MinCellExtent(FSizingIndex);
    if goMouseCanHideCells in FOptions then
    begin
      if Result > 0 then
      begin
        if FSizingDest - CellPt > Max(MinExtent div 2, MinExtent - 5) then
          Result := Max(FSizingDest - CellPt, MinExtent)
        else
          Result := 0;
      end else
      begin
        Tmp := FSizingIndex;
        while (Tmp > 0) and (Info.CellExtent(Tmp - 1) = 0) do
          Dec(Tmp);
        if Tmp <> FSizingIndex then
          Inc(CellPt, Info.EffectiveSpacing(Tmp));
        if FSizingDest - CellPt >= MinExtent then
          Result := FSizingDest - CellPt;
      end;
    end else
      Result := Max(FSizingDest - CellPt, MinExtent);
  end;

var
  R: TRect;
  Info: TKGridAxisInfo;
begin
  if HandleAllocated then
  begin
    case FGridState of
      gsColSizing:
      begin
        case FSizingStyle of
          ssLine, ssXORLine:
          begin
            Info := GetAxisInfoVert([aiGridBoundary]);
            R := Rect(FSizingDest, 0, FSizingDest + 2, Info.GridBoundary);
            InvalidateRect(Handle, @R, False);
          end;
          ssUpdate:
          begin
            if (State = csShow) and CellToPoint(FSizingIndex, 0, R.TopLeft) then
              ColWidths[FSizingIndex] := Axis(GetAxisInfoHorz([]), R.Left, FCols);
          end;
        end;
      end;
      gsRowSizing:
      begin
        case FSizingStyle of
          ssLine, ssXORLine:
          begin
            Info := GetAxisInfoHorz([aiGridBoundary]);
            R := Rect(0, FSizingDest, Info.GridBoundary, FSizingDest + 2);
            InvalidateRect(Handle, @R, False);
          end;
          ssUpdate:
          begin
            if (State = csShow) and CellToPoint(0, FSizingIndex, R.TopLeft) then
              RowHeights[FSizingIndex] := Axis(GetAxisInfoVert([]), R.Top, FRows);
          end;
        end;
      end;
    end;
  end;
end;

procedure TKCustomGrid.TopLeftChanged;
begin
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(Self);
end;

procedure TKCustomGrid.UnlockSortMode;
begin
  if FSortModeLock > 0 then
    Dec(FSortModeLock);
end;

procedure TKCustomGrid.UnselectRange;
begin
  Selection := GridRect(FSelection.Cell1);
end;

procedure TKCustomGrid.UpdateAxes(Horz: Boolean; FirstCol: Integer;
  Vert: Boolean; FirstRow: Integer; Flags: TKGridAxisUpdateFlags);

  procedure Axis1(Info: TKGridAxisInfo; AxisItems: TKGridAxisItems;
    var FirstIndex: Integer);
  var
    I, AExtent: Integer;
  begin
    FlagSet(cGF_GridUpdates);
    try
      for I := 0 to Info.TotalCellCount - 1 do
      begin
        AExtent := Info.CellExtent(I);
        if (AExtent > 0) and (AExtent < Info.MinCellExtent(I)) then
        begin
          AxisItems[I].Extent := 0;
          FirstIndex := Min(FirstIndex, I);
        end
        else if AExtent > Info.MaxCellExtent(I) then
        begin
          AxisItems[I].Extent := Info.MaxCellExtent(I);
          FirstIndex := Min(FirstIndex, I);
        end;
      end;
    finally
      FlagClear(cGF_GridUpdates);
    end;
  end;

  procedure Axis2(Info: TKGridAxisInfo; AxisItems: TKGridAxisItems;
    var FirstIndex: Integer);

    function CalcGridExtent(FixedExtent: Integer): Integer;
    var
      I: Integer;
    begin
      Result := FixedExtent;
      for I := Info.FixedCellCount to Info.TotalCellCount - 1 do
        Inc(Result, Info.CellExtent(I) + Info.EffectiveSpacing(I));
    end;

  var
    I, CellExtent, Delta, FixedExtent, GridExtent: Integer;
  begin
    FlagSet(cGF_GridUpdates);
    try
      FixedExtent := 0;
      for I := 0 to Info.FixedCellCount - 1 do
        Inc(FixedExtent, Info.CellExtent(I) + Info.EffectiveSpacing(I));
      GridExtent := CalcGridExtent(FixedExtent);
      if GridExtent <> Info.ClientExtent then
      begin
        if GridExtent < Info.ClientExtent then
        begin
          // cells would occupy a smaller area than Info.ClientExtent
          // try to enlarge the last cell if visible:
          I := Info.TotalCellCount - 1;
          while (I >= Info.FixedCellCount) and (GridExtent < Info.ClientExtent) do
          begin
            CellExtent := Info.CellExtent(I);
            if CellExtent <> 0 then
            begin
              Delta := Info.ClientExtent - GridExtent;
              AxisItems[I].Extent := CellExtent + Delta;
              Inc(GridExtent, Delta);
              FirstIndex := Min(FirstIndex, I);
            end;
            Dec(I);
          end;
          if I < Info.FixedCellCount then
          begin
            // apparently all cells are hidden. Try to unhide last cell
            I := Info.TotalCellCount - 1;
            AxisItems[I].Extent := Info.MinCellExtent(I);
            GridExtent := CalcGridExtent(FixedExtent);
            if GridExtent > Info.ClientExtent then
              AxisItems[I].Extent := 0 //oops, does not fit, hide again, leave empty area
            else
              AxisItems[I].Extent := Info.CellExtent(I) + Info.ClientExtent - GridExtent;
          end;
        end else
        begin
          // cells would occupy a greater area than Info.ClientExtent
          // try to decrease the extents of not fully visible cells
          I := Info.TotalCellCount - 1;
          while (I >= Info.FixedCellCount) and (GridExtent > Info.ClientExtent) do
          begin
            CellExtent := Info.CellExtent(I);
            if CellExtent > Info.MinCellExtent(I) then
            begin
              Delta := Min(GridExtent - Info.ClientExtent, CellExtent - Info.MinCellExtent(I));
              AxisItems[I].Extent := CellExtent - Delta;
              Dec(GridExtent, Delta);
              FirstIndex := Min(FirstIndex, I);
            end;
            Dec(I);
          end;
          if GridExtent > Info.ClientExtent then
          begin
            // still everything not visible, hide some cells
            I := Info.TotalCellCount - 1;
            while (I >= Info.FixedCellCount) and (GridExtent > Info.ClientExtent) do
            begin
              CellExtent := Info.CellExtent(I);
              if CellExtent > 0 then
              begin
                AxisItems[I].Extent := 0;
                FirstIndex := Min(FirstIndex, I);
                GridExtent := CalcGridExtent(FixedExtent);
              end;
              Dec(I);
            end;
            if (GridExtent < Info.ClientExtent) and (I >= Info.FixedCellCount) then
            begin
              // cells would occupy a smaller area than Info.ClientExtent - 2nd test
              I := Info.TotalCellCount - 1;
              while (I >= Info.FixedCellCount) and (GridExtent < Info.ClientExtent) do
              begin
                CellExtent := Info.CellExtent(I);
                if CellExtent <> 0 then
                begin
                  Delta := Info.ClientExtent - GridExtent;
                  AxisItems[I].Extent := CellExtent + Delta;
                  Inc(GridExtent, Delta);
                  FirstIndex := Min(FirstIndex, I);
                end;
                Dec(I);
              end;
            end;
          end;
        end;
      end;
    finally
      FlagClear(cGF_GridUpdates);
    end;
  end;

var
  ColIndex, RowIndex: Integer;
  Info: TKGridAxisInfo;
begin
  if not UpdateUnlocked then Exit;
  if Horz then
  begin
    Info := GetAxisInfoHorz([]);
    if FirstCol >= 0 then ColIndex := FirstCol else ColIndex := FColCount;
    if afCheckMinExtent in Flags then
      Axis1(Info, FCols, ColIndex);
    if (goAlignLastCol in FOptions) then
    begin
      if FTopLeft.Col <> FFixedCols then
      begin
        FTopLeft.Col := FFixedCols;
        TopLeftChanged;
      end;
      Axis2(Info, FCols, ColIndex);
    end;
  end;
  if Vert then
  begin
    Info := GetAxisInfoVert([]);
    if FirstRow >= 0 then RowIndex := FirstRow else RowIndex := FRowCount;
    if afCheckMinExtent in Flags then
      Axis1(Info, FRows, RowIndex);
    if (goAlignLastRow in FOptions) then
    begin
      if FTopLeft.Row <> FFixedRows then
      begin
        FTopLeft.Row := FFixedRows;
        TopLeftChanged;
      end;
      Axis2(Info, FRows, RowIndex);
    end;
  end;
  UpdateScrollRange(Horz, Vert, False);
  UpdateEditor(Flag(cGF_EditorModeActive));
  if Horz then
  begin
    if ColIndex < FColCount then
      ColWidthsChanged(ColIndex);
    if FirstCol = cAll then
      Invalidate
    else if ColIndex < FColCount then
      InvalidateCols(ColIndex);
  end;
  if Vert then
  begin
    if RowIndex < FRowCount then
      RowHeightsChanged(RowIndex);
    if FirstRow = cAll then
      Invalidate
    else if RowIndex < FRowCount then
      InvalidateRows(RowIndex);
  end;
end;

procedure TKCustomGrid.UpdateCellSpan;

  function DoUpdate(FirstCol, FirstRow, LastCol, LastRow: Integer): Boolean;
  var
    I, J: Integer;
    Span, RefSpan: TKGridCellSpan;
  begin
    Result := False;
    RefSpan := MakeCellSpan(1, 1);
    // don't make this too complicated, but maybe it is little bit slower:
    // reset all negative spans
    for I := FirstCol to LastCol - 1 do
      for J := FirstRow to LastRow - 1 do
        if FCells[J, I] <> nil then
        begin
          with FCells[J, I].Span do
            if (ColSpan <= 0) or (RowSpan <= 0) then
              FCells[J, I].Span := RefSpan;
        end;
    // create all spans
    for I := FirstCol to LastCol - 1 do
      for J := FirstRow to LastRow - 1 do
        if FCells[J, I] <> nil then
        begin
          with FCells[J, I].Span do
            if (ColSpan > 1) or (RowSpan > 1) then
            begin
              Result := True;
              Span := MakeCellSpan(Min(ColSpan, LastCol - I), Min(RowSpan, LastRow - J));
              FCells[J, I].Span := RefSpan;
              InternalSetCellSpan(I, J, Span);
            end;
        end;
  end;

begin
  if Assigned(FCells) then
  begin
    FlagSet(cGF_GridUpdates);
    try
      // cells cannot be merged across fixed area boundaries
      DoUpdate(0, 0, FFixedCols, FFixedRows);
      DoUpdate(FFixedCols, 0, FColCount, FFixedRows);
      DoUpdate(0, FFixedRows, FFixedCols, FRowCount);
      if Flag(cGF_SelCellsMerged) then
        if not DoUpdate(FFixedCols, FFixedRows, FColCount, FRowCount) then
          FlagClear(cGF_SelCellsMerged);
    finally
      FlagClear(cGF_GridUpdates);
    end;
  end;
end;

procedure TKCustomGrid.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and HandleAllocated and
    not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

procedure TKCustomGrid.UpdateEditor(Show: Boolean);

  procedure InternalEditorSetPos(R: TRect; CallResize: Boolean);
  begin
    // aki:
    if ((FEditorCell.Col >= FTopLeft.Col) and ((FScrollOffset.X = 0) or (FEditorCell.Col > FTopLeft.Col)) or
      (gxEditFixedCols in FOptionsEx) and (FEditorCell.Col < FFixedCols)) and
      ((FEditorCell.Row >= FTopLeft.Row) and ((FScrollOffset.Y = 0) or (FEditorCell.Row > FTopLeft.Row)) or
      (gxEditFixedRows in FOptionsEx) and (FEditorCell.Row < FFixedRows)) then
    begin
      if CallResize then
        EditorResize(FEditor, FEditorCell.Col, FEditorCell.Row, R);
      with R do
      begin
        if IsThemed then // due to TComboBox bug in VCL!
        begin
          FEditor.Constraints.MaxWidth := Right - Left + 1;
          FEditor.Constraints.MaxHeight := Bottom - Top + 1;
        end;
        FEditor.Height := Bottom - Top;
        FEditor.Width := Right - Left;
        if gxEditorVCenter in FOptionsEx then
          Inc(Top, (Bottom - Top - FEditor.Height) div 2);
        if gxEditorHCenter in FOptionsEx then
          Inc(Left, (Right - Left - FEditor.Width) div 2);
        FEditor.SetBounds(Left, Top, FEditor.Width, FEditor.Height);
      end;
    end else
    begin
      // hide the editor in some way
      FEditor.Left := - 10 - FEditor.Width;
      FEditor.Top := - 10 - FEditor.Height;
    end;
  end;

  procedure InternalEditorMove;
  var
    R: TRect;
  begin
    if (FEditor <> nil) and CellRect(FEditorCell.Col, FEditorCell.Row, R) then
    begin
      if not EqualRect(FEditorRect, R) then with R do
      begin
        FEditorRect := R;
        InternalEditorSetPos(R, True);
        SetControlClipRect(FEditor, R);
      end;
    end;
  end;

  procedure InternalEditorCreate;
  var
    R: TRect;
    PropInfo: PPropInfo;
    P: TPoint;
    ACell: TKGridCell;
  begin
    if (FEditor = nil) and HandleAllocated and Enabled and Visible then
    begin
      if CellRect(Col, Row, R) and SelectCell(Col, Row) then
      begin
        FEditorCell.Col := Col;
        FEditorCell.Row := Row;
        FEditor := EditorCreate(FEditorCell.Col, FEditorCell.Row);
        if FEditor <> nil then
        begin
          if Assigned(FCells) then
          begin
            ACell := Cell[FEditorCell.Col, FEditorCell.Row];
            if FEditedCell = nil then FEditedCell := TKGridCellClass(ACell.ClassType).Create(nil);
            FEditedCell.Assign(ACell);
          end;
          FThroughClick := False;
          FEditorRect := R;
          TabStop := False;
          FEditor.Visible := False;
          FEditor.Align := alNone;
          FEditor.Constraints.MinWidth := 0;
          FEditor.Constraints.MinHeight := 0;
          PropInfo := GetPropInfo(FEditor, 'AutoSize');
          if PropInfo <> nil then
            SetOrdProp(FEditor, PropInfo, Integer(False));
          // I hope no other steps to delimit inplace editor's size
          // some controls as TComboBox on Win cannot be arbitrary resized :-(
          FEditor.ControlStyle := FEditor.ControlStyle - [csAcceptsControls, csFramed, csFixedWidth, csFixedHeight] {$IFDEF COMPILER7_UP} - [csParentBackground] {$ENDIF};
          FEditor.TabStop := True;
          InternalEditorSetPos(R, True); // call this here too to be compatible with all LCL widget sets
          FEditor.Parent := Self;
          FEditor.HandleNeeded;
          InternalEditorSetPos(R, True);
          SetControlClipRect(FEditor, R);
          EditorDataFromGrid(FEditor, FEditorCell.Col, FEditorCell.Row);
          FEditor.Visible := True; // Remark: don't set DoubleBuffered because not all editors support it!
          EditorSelect(FEditor, FEditorCell.Col, FEditorCell.Row,
            not (goNoSelEditText in FOptions), Flag(cGF_CaretToLeft), Flag(cGF_SelectedByMouse));
          FlagClear(cGF_CaretToLeft);
          SafeSetFocus;
          if FThroughClick then
          begin
            P := FEditor.ScreenToClient(Mouse.CursorPos);
            PostMessage(FEditor.Handle, LM_LBUTTONDOWN, 1, MakeLong(P.X, P.Y));
            MouseCapture := False;
            FlagSet(cGF_ThroughClick);
            FThroughClick := False;
          end;
          InvalidateCurrentSelection;
          FEditorWindowProc := FEditor.WindowProc;
          FEditor.WindowProc := EditorWindowProc;
        end;
      end;
    end;
  end;

  procedure InternalEditorDestroy;
  var
    Form: TCustomForm;
  begin
    if FEditor <> nil then
    begin
      FEditor.WindowProc := FEditorWindowProc;
      Form := GetParentForm(Self);
      if Assigned(Form) and (csDestroying in Form.ComponentState) then
        Form := nil;
      if FEditor.HandleAllocated then
        EditorDataToGrid(FEditor, FEditorCell.Col, FEditorCell.Row);
      TabStop := True;
      if Assigned(Form) and (Form.ActiveControl = FEditor) then
        Form.ActiveControl := Self;
      FEditor.Visible := False;
      FEditor.Parent := nil;
      EditorDestroy(FEditor, FEditorCell.Col, FEditorCell.Row);
      FreeAndNil(FEditor);
      if Assigned(FCells) then
        if CompareCellInstances(FEditedCell, InternalGetCell(FEditorCell.Col, FEditorCell.Row)) <> 0 then
          Changed;
      FEditorCell := GridPoint(-1, -1);
      if Assigned(Form) and (Form.ActiveControl = nil) then
        Form.ActiveControl := Self;
      InvalidateCurrentSelection;
    end;
  end;

var
  PosChanged: Boolean;
begin
  if not Flag(cGF_EditorUpdating) then
  begin
    FlagSet(cGF_EditorUpdating);
    try
      if (goEditing in FOptions) and Show and (InternalGetColWidths(Col) > 0) and (InternalGetRowHeights(Row) > 0) then
      begin
        PosChanged := (FEditorCell.Col <> Col) or (FEditorCell.Row <> Row);
        if (FEditor = nil) or PosChanged then
        begin
          InternalEditorDestroy;
          InternalEditorCreate;
        end else
          InternalEditorMove;
      end else
        InternalEditorDestroy;
    finally
      FlagClear(cGF_EditorUpdating);
    end;
  end;
end;

procedure TKCustomGrid.UpdateScrollRange(Horz, Vert, UpdateNeeded: Boolean);

  function Axis(Code: Cardinal; HasScrollBar: Boolean; ScrollMode: TKGridScrollMode;
    Info: TKGridAxisInfo; out FirstGridCell, FirstGridCellExtent, ScrollPos: Integer;
    var ScrollOffset: Integer): Boolean;
  var
    I, CellExtent, MaxExtent, PageExtent, ScrollExtent: Integer;
    CheckFirstGridCell: Boolean;
    SI: TScrollInfo;
  begin
    Result := False;
    CheckFirstGridCell := True;
    ScrollExtent := 0;
    PageExtent := 0;
    MaxExtent := Info.ClientExtent - Info.FixedBoundary;
    I := Info.TotalCellCount - 1;
    FirstGridCellExtent := I;
    while I >= Info.FixedCellCount do
    begin
      CellExtent := Info.CellExtent(I);
      Inc(ScrollExtent, CellExtent + Info.EffectiveSpacing(I));
      if CheckFirstGridCell then
      begin
        if (ScrollExtent <= MaxExtent) then
        begin
          PageExtent := ScrollExtent;
          FirstGridCellExtent := I;
          if (Info.FirstGridCell > I) or (Info.FirstGridCell = I) and (ScrollOffset <> 0) then
          begin
            FirstGridCell := I;
            Result := True;
          end;
        end else
        begin
          if PageExtent = 0 then
            PageExtent := ScrollExtent;
          CheckFirstGridCell := False;
        end;
      end;
      if I = FirstGridCell then
        ScrollPos := ScrollExtent;
      Dec(I);
    end;
    ScrollPos := ScrollExtent - ScrollPos;
    if Result or ((ScrollMode = smCell) or not HasScrollBar) and (ScrollOffset <> 0) then
    begin
      ScrollOffset := 0;
      Result := True;
    end;
    if HandleAllocated then
      if HasScrollBar then
      begin
        FillChar(SI, SizeOf(TScrollInfo), 0);
        SI.cbSize := SizeOf(TScrollInfo);
        SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS {$IFDEF UNIX}or SIF_UPDATEPOLICY{$ENDIF};
        SI.nMin := 0;
        SI.nMax := ScrollExtent {$IFNDEF FPC}- 1{$ENDIF};
        SI.nPos := ScrollPos + ScrollOffset;
        SI.nPage := PageExtent;
      {$IFDEF UNIX}
        SI.ntrackPos := SB_POLICY_CONTINUOUS;
      {$ENDIF}
        SetScrollInfo(Handle, Code, SI, True);
        ShowScrollBar(Handle, Code, PageExtent < ScrollExtent);
      end else
        ShowScrollBar(Handle, Code, False);
  end;

var
  UpdateHorz, UpdateVert: Boolean;
begin
  if not UpdateUnlocked then Exit;
  UpdateHorz := Horz and Axis(SB_HORZ, HasHorzScrollBar, FScrollModeHorz,
    GetAxisInfoHorz([aiFixedParams]), FTopLeft.Col, FTopLeftExtent.Col, FScrollPos.X, FScrollOffset.X);
  UpdateVert := Vert and Axis(SB_VERT, HasVertScrollBar, FScrollModeVert,
    GetAxisInfoVert([aiFixedParams]), FTopLeft.Row, FTopLeftExtent.Row, FScrollPos.Y, FScrollOffset.Y);
  if UpdateNeeded or UpdateHorz then
    InvalidateCols(FFixedCols);
  if UpdateNeeded or UpdateVert then
    InvalidateRows(FFixedRows);
  if UpdateNeeded or UpdateHorz or UpdateVert then
  begin
    UpdateEditor(Flag(cGF_EditorModeActive));
    if UpdateHorz or UpdateVert then
      TopLeftChanged;
    if FOptions * [goRowSelect, goRangeSelect] <> [] then
      InvalidateCurrentSelection;
  end;
  InvalidatePageSetup;
end;

procedure TKCustomGrid.UpdateSize;
begin
  inherited;
  UpdateAxes(True, FColCount, True, FRowCount, [afCheckMinExtent]);
end;

procedure TKCustomGrid.UpdateSortMode(ACol, ARow: Integer);
var
  Index: Integer;
begin
  LockSortMode;
  try
    if FCols[ACol].SortMode <> smNone then
    begin
      Index := InternalInsertIfCellModifiedNR(ACol, ARow, FFixedRows, FRowCount - 1, FCols[ACol].SortMode = smUp, CompareRows);
      if Index <> ARow then
      begin
        MoveRow(ARow, Index);
        ClampInView(ACol, Index);
      end;
    end;
    if FRows[ARow].SortMode <> smNone then
    begin
      Index := InternalInsertIfCellModifiedNR(ARow, ACol, FFixedCols, FColCount - 1, FRows[ARow].SortMode = smUp, CompareCols);
      if Index <> ACol then
      begin
        MoveCol(ACol, Index);
        ClampInView(Index, ARow);
      end;
    end;
  finally
    UnlockSortMode;
  end;
end;

procedure TKCustomGrid.WMChar(var Msg: {$IFDEF FPC}TLMChar{$ELSE}TWMChar{$ENDIF});
begin
  if (goEditing in Options) and CharInSetEx(Char(Msg.CharCode), [^H, #32..#255]) then
  begin
    EditorMode := True;
    if EditorMode then
      PostMessage(FEditor.Handle, LM_CHAR, Word(Msg.CharCode), 0);
    Msg.Result := 1;
  end else
    inherited;
end;

procedure TKCustomGrid.WMEraseBkGnd(var Msg: TLMEraseBkGnd);
begin
  if Flag(cGF_EditorUpdating) or not (goEraseBackground in FOptions) then
    Msg.Result := 1
  else
    inherited;
end;

procedure TKCustomGrid.WMGetDlgCode(var Msg: TLMNoParams);
begin
  Msg.Result := DLGC_WANTARROWS;
  if goTabs in FOptions then Msg.Result := Msg.Result or DLGC_WANTTAB;
  if goEditing in FOptions then Msg.Result := Msg.Result or DLGC_WANTCHARS;
end;

procedure TKCustomGrid.WMHScroll(var Msg: TLMHScroll);
begin
  if not EditorMode or (Msg.ScrollBar <> FEditor.Handle) then
  begin
    SafeSetFocus;
    Scroll(Msg.ScrollCode, cScrollNoAction, Msg.Pos, 0, True);
  end else
    inherited;
end;

procedure TKCustomGrid.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  // focus moves to another control including inplace editor
  if not Flag(cGF_EditorUpdating) then
    InvalidateCurrentSelection;
end;

procedure TKCustomGrid.WMSetFocus(var Msg: TLMSetFocus);
begin
  // focus moves to the grid - post message
  if not Flag(cGF_EditorUpdating) then
    PostLateUpdate(FillMessage(LM_SETFOCUS, 0, 0), True);
end;

procedure TKCustomGrid.WMVScroll(var Msg: TLMVScroll);
begin
  if not EditorMode or (Msg.ScrollBar <> FEditor.Handle) then
  begin
    SafeSetFocus;
    Scroll(cScrollNoAction, Msg.ScrollCode, 0, Msg.Pos, True);
  end else
    inherited;
end;

{$IFNDEF FPC}
procedure TKCustomGrid.WndProc(var Msg: TMessage);

  procedure PaintCellBackground(ACanvas: TCanvas; R: TRect);
  var
    TmpBlockRect: TRect;
  begin
    R := Rect(0, 0, R.Right - R.Left, R.Bottom - R.Top);
    TmpBlockRect := SelectionRect;
    OffsetRect(TmpBlockRect, -R.Left, -R.Top);
    InternalPaintCell(Col, Row, GetDrawState(Col, Row, HasFocus),
      R, TmpBlockRect, ACanvas, False, False);
  end;

var
  R: TRect;
  SaveIndex: Integer;
  ACanvas: TCanvas;
begin
  case Msg.Msg of
    WM_CTLCOLORBTN..WM_CTLCOLORSTATIC:
    begin
      if EditorMode and EditorIsTransparent and
        CellRect(Col, Row, R) then
      begin
        if Themes then
        begin
          ACanvas := TCanvas.Create;
          SaveIndex := SaveDC(Msg.WParam);
          try
            ACanvas.Handle := Msg.WParam;
            PaintCellBackground(ACanvas, R);
          finally
            RestoreDC(Msg.WParam, SaveIndex);
            ACanvas.Free;
          end;
          Msg.Result := GetStockObject(NULL_BRUSH);
        end else
        begin
          PaintCellBackground(FTmpBitmap.Canvas, R);
          SetTextColor(Msg.WParam, ColorToRGB(TCheckBox(FEditor).Font.Color));
          SetBkColor(Msg.WParam, ColorToRGB(FTmpBitmap.Canvas.Brush.Color));
          Msg.Result := FTmpBitmap.Canvas.Brush.Handle;
        end;
      end else
        inherited;
    end;
  else
    inherited;
  end
end;
{$ENDIF}

procedure TKCustomGrid.WriteColWidths(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to FColCount - 1 do WriteInteger(ColWidths[I]);
    WriteListEnd;
  end;
end;

procedure TKCustomGrid.WriteRowHeights(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to FRowCount - 1 do WriteInteger(RowHeights[I]);
    WriteListEnd;
  end;
end;

{$IFDEF FPC}
initialization
  {$i kgrids.lrs}
{$ELSE}
  {$R kgrids.res}
{$ENDIF}
end.
