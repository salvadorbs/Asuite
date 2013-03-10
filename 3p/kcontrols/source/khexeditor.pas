{ @abstract(This unit contains the TKHexEditor component and all supporting classes)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(12 Oct 2005)
  @lastmod(20 Jun 2010)

  This unit provides a powerfull hexadecimal editor component @link(TKHexEditor)
  with following major features:
  <UL>
  <LI><I>advanced editing capabilities</I></LI>
  <LI><I>advanced rendering styles</I></LI>
  <LI><I>clipboard operations</I></LI>
  <LI><I>virtually unlimited undo/redo operations</I></LI>
  <LI><I>key mapping functionality</I></LI>
  <LI><I>fast search/replace function</I></LI>
  <LI><I>print/preview function</I></LI>
  </UL>

  Copyright © 2006 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KHexEditor;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  ExtCtrls, StdCtrls, Forms, KFunctions, KControls, KEditCommon;

resourcestring
  { @exclude }
  sAddressText = 'Address area text';
  { @exclude }
  sAddressBkGnd = 'Address area background';
  { @exclude }
  sBkGnd = 'Editor background';
  { @exclude }
  sDigitTextEven = 'Digit area even column';
  { @exclude }
  sDigitTextOdd = 'Digit area odd column';
  { @exclude }
  sDigitBkgnd = 'Digit area background';
  { @exclude }
  sHorzLines = 'Horizontal lines';
  { @exclude }
  sInactiveCaretBkGnd = 'Inactive caret background';
  { @exclude }
  sInactiveCaretSelBkGnd = 'Selected inactive caret background';
  { @exclude }
  sInactiveCaretSelText = 'Selected inactive caret text';
  { @exclude }
  sInactiveCaretText = 'Inactive caret text';
  { @exclude }
  sLinesHighLight = 'Lines highlight';
  { @exclude }
  sSelBkGnd = 'Selection background';
  { @exclude }
  sSelBkGndFocused = 'Focused selection background';
  { @exclude }
  sSelText = 'Selection text';
  { @exclude }
  sSelTextFocused = 'Focused selection text';
  { @exclude }
  sSeparators = 'Area separating lines';
  { @exclude }
  sTextText = 'Text area text';
  { @exclude }
  sTextBkGnd = 'Text area background';
  { @exclude }
  sVertLines = 'Vertical lines';

type
  { Declares possible values for the @link(TKCustomHexEditor.AddressMode) property }
  TKHexEditorAddressMode = (
    { Address will be shown in decimal format }
    eamDec,
    { Address will be shown in hexadecimal format }
    eamHex
  );

  { Declares possible values e.g. for the @link(TKCustomHexEditor.EditArea) property }
  TKHexEditorArea = (
    { No area is selected, e.g. when clicked outside of visible text }
    eaNone,
    { Address area selected/used }
    eaAddress,
    { Digits area selected/used }
    eaDigits,
    { Text area selected/used }
    eaText
  );

  { @abstract(Contains dimensions of all areas in characters)
    <UL>
    <LH>Members:</LH>
    <LI><I>Address</I> - address area width</LI>
    <LI><I>AddressOut</I> - address area leadout</LI>
    <LI><I>Digits</I> - digits area width</LI>
    <LI><I>DigitsIn</I> - digits area leadin</LI>
    <LI><I>DigitsOut</I> - digits area leadout</LI>
    <LI><I>Text</I> - text area width</LI>
    <LI><I>TextIn</I> - text area leadin</LI>
    <LI><I>TotalHorz</I> - total width of all defined areas</LI>
    <LI><I>TotalVert</I> - total number of lines</LI>
    </UL>
  }
  TKHexEditorAreaDimensions = record
    Address,
    AddressOut,
    Digits,
    DigitsIn,
    DigitsOut,
    Text,
    TextIn,
    TotalHorz,
    TotalVert: Integer;
  end;

  { Declares possible indexes e.g. for the @link(TKHexEditorColors.Color) property. }
  TKHexEditorColorIndex = Integer;

  { @abstract(Declares @link(TKHexEditorColors) color item description)
    <UL>
    <LH>Members:</LH>
    <LI><I>Def</I> - default color value</LI>
    <LI><I>Name</I> - color name (can be localized)</LI>
    </UL>
  }
  TKHexEditorColorSpec = record
    Def: TColor;
    Name: string;
  end;

  { Declares possible values for the @link(TKCustomHexEditor.DisabledDrawStyle) property }
  TKHexEditorDisabledDrawStyle = (
    { The lines will be painted with brighter colors when editor is disabled }
    eddBright,
    { The lines will be painted with gray text and white background when editor is disabled }
    eddGrayed,
    { The lines will be painted normally when editor is disabled }
    eddNormal
  );

  { Declares drawing styles - possible values for the @link(TKCustomHexEditor.DrawStyles) property }
  TKHexEditorDrawStyle = (
    { Show adress area }
    edAddress,
    { Show digits area }
    edDigits,
    { Show text area }
    edText,
    { Show horizontal leading lines }
    edHorzLines,
    { Show caret position when editor is inactive (has no input focus) }
    edInactiveCaret,
    { Show vertical area separating lines }
    edSeparators,
    { Show vertical leading lines (digits area only) }
    edVertLines,
    { @link(TKHexEditorColors.BkGnd) is used for all areas if included }
    edSingleBkGnd
  );

  { Drawing styles can be arbitrary combined }
  TKHexEditorDrawStyles = set of TKHexEditorDrawStyle;

  { @abstract(Declares the paint data structure for the @link(TKCustomHexEditor.PaintLines) method)
    <UL>
    <LH>Members:</LH>
    <LI><I>Canvas</I> - destination canvas</LI>
    <LI><I>PainRect</I> - bounding rectangle for painted lines (no clipping necessary,
    this is performed by window/page client area)</LI>
    <LI><I>TopLine</I> - first line painted (vertical scroll offset)</LI>
    <LI><I>BottomLine</I> - last line painted</LI>
    <LI><I>LeftChar</I> - first character painted (horizontal scroll offset)</LI>
    <LI><I>CharWidth</I> - character width in pixels for supplied canvas</LI>
    <LI><I>CharHeight</I> - character height in pixels for supplied canvas</LI>
    <LI><I>CharSpacing</I> - inter-character spacing in pixels for supplied canvas</LI>
    <LI><I>Printing</I> - determines whether normal painting or page printing should be performed</LI>
    <LI><I>PaintAll</I> - when Printing is True, specifies whether all data or selection only
    should be painted, this applies only to the first and/or last painted line</LI>
    <LI><I>PaintColors</I> - when Printing is True, specifies whether to paint with colors or grayscale</LI>
    <LI><I>PaintSelection</I> - when Printing is True, specifies whether to indicate the selection</LI>
    </UL>
  }
  TKHexEditorPaintData = record
    Canvas: TCanvas;
    PaintRect: TRect;
    TopLine,
    BottomLine,
    LeftChar,
    CharWidth,
    CharHeight,
    CharSpacing: Integer;
    Printing,
    PaintAll,
    PaintColors,
    PaintSelection,
    CaretShown: Boolean;
  end;

  { @abstract(Declares the selection structure)
    <UL>
    <LH>Members:</LH>
    <LI><I>Index</I> - byte index</LI>
    <LI><I>Digit</I> - digit index</LI>
    </UL>
  }
  TKHexEditorSelection = record
    Index: Integer;
    Digit: Integer;
  end;

  { @abstract(Declares the structure for the @link(TKCustomHexEditor.SelText) property)
    <UL>
    <LH>Members:</LH>
    <LI><I>AsBinaryRaw</I> - selected data as binary characters not mapped</LI>
    <LI><I>AsBinaryMapped</I> - selected data as binary characters mapped</LI>
    <LI><I>AsDigits</I> - selected data as hexadecimal digits</LI>
    <LI><I>AsDigitsByteAligned</I> - selected data as hexadecimal digits
    without regarding cross-byte selections</LI>
    </UL>
  }
  TKHexEditorSelText = record
    AsBinaryRaw,
    AsBinaryMapped,
    AsDigits,
    AsDigitsByteAligned: AnsiString;
  end;

  { Declares hex editor states - possible values for the @link(TKCustomHexEditor.States) property
    (protected) }
  TKHexEditorState = (
    { Caret is visible }
    elCaretVisible,
    { Caret is being updated }
    elCaretUpdate,
    { Ignore following WM_CHAR message }
    elIgnoreNextChar,
    { Buffer modified }
    elModified,
    { Mouse captured }
    elMouseCapture,
    { Overwrite mode active }
    elOverwrite,
    { Read only editor }
    elReadOnly
  );

  { Hex editor states can be arbitrary combined }
  TKHexEditorStates = set of TKHexEditorState;

  { @abstract(Declares the color description structure returned by @link(TKHexEditorColors.ColorData) property)
    <UL>
    <LH>Members:</LH>
    <LI><I>Index</I> - color index</LI>
    <LI><I>Color</I> - current color value</LI>
    <LI><I>Default</I> - default color value</LI>
    <LI><I>Name</I> - color name</LI>
    </UL>
  }
  TKHexEditorColorData = record
    Index: TKHexEditorColorIndex;
    Color: TColor;
    Default: TColor;
    Name: string;
  end;

  { Declares possible values for the @link(TKHexEditorColors.ColorScheme) property }
  TKHexEditorColorScheme = (
    { GetColor returns normal color currently defined for each item }
    ecsNormal,
    { GetColor returns gray for text and line colors and white for background colors }
    ecsGrayed,
    { GetColor returns brighter version of normal color }
    ecsBright,
    { GetColor returns grayscaled color versions }
    ecsGrayScale
  );

const
  { Minimum for the @link(TKCustomHexEditor.AddressSize) property }
  cAddressSizeMin = 2;
  { Maximum for the @link(TKCustomHexEditor.AddressSize) property }
  cAddressSizeMax = 10;
  { Default value for the @link(TKCustomHexEditor.AddressSize) property }
  cAddressSizeDef = 8;

  { Minimum for the @link(TKCustomHexEditor.AreaSpacing) property }
  cAreaSpacingMin = 1;
  { Maximum for the @link(TKCustomHexEditor.AreaSpacing) property }
  cAreaSpacingMax = 20;
  { Default value for the @link(TKCustomHexEditor.AreaSpacing) property }
  cAreaSpacingDef = 1;

  { Minimum for the @link(TKCustomHexEditor.CharSpacing) property }
  cCharSpacingMin = 0;
  { Maximum for the @link(TKCustomHexEditor.CharSpacing) property }
  cCharSpacingMax = 100;
  { Default value for the @link(TKCustomHexEditor.CharSpacing) property }
  cCharSpacingDef = 0;

  { Minimum for the @link(TKCustomHexEditor.DigitGrouping) property }
  cDigitGroupingMin = 1;
  { Maximum for the @link(TKCustomHexEditor.DigitGrouping) property }
  cDigitGroupingMax = 8;
  { Default value for the @link(TKCustomHexEditor.DigitGrouping) property }
  cDigitGroupingDef = 2;

  { Minimum for the @link(TKCustomHexEditor.LineHeightPercent) property }
  cLineHeightPercentMin = 10;
  { Maximum for the @link(TKCustomHexEditor.LineHeightPercent) property }
  cLineHeightPercentMax = 1000;
  { Default value for the @link(TKCustomHexEditor.LineHeightPercent) property }
  cLineHeightPercentDef = 130;

  { Minimum for the @link(TKCustomHexEditor.UndoLimit) property }
  cUndoLimitMin = 100;
  { Maximum for the @link(TKCustomHexEditor.UndoLimit) property }
  cUndoLimitMax = 10000;
  { Default value for the @link(TKCustomHexEditor.UndoLimit) property }
  cUndoLimitDef = 1000;

  { Minimum for the @link(TKCustomHexEditor.LineSize) property }
  cLineSizeMin = 1;
  { Maximum for the @link(TKCustomHexEditor.LineSize) property }
  cLineSizeMax = 128;
  { Default value for the @link(TKCustomHexEditor.LineSize) property }
  cLineSizeDef = 16;

  { Minimum for the @link(TKCustomHexEditor.ScrollSpeed) property }
  cScrollSpeedMin = 50;
  { Maximum for the @link(TKCustomHexEditor.ScrollSpeed) property }
  cScrollSpeedMax = 1000;
  { Default value for the @link(TKCustomHexEditor.ScrollSpeed) property }
  cScrollSpeedDef = 100;

  { Minimum for the @link(TKHexEditor.Font).Size property }
  cFontSizeMin = 8;
  { Maximum for the @link(TKHexEditor.Font).Size property }
  cFontSizeMax = 100;
  { Default value for the @link(TKHexEditor.Font).Size property }
  cFontSizeDef = 11;

  { Default value for the @link(TKHexEditorColors.AddressText) color property }
  cAddressTextDef = clWindowText;
  { Default value for the @link(TKHexEditorColors.AddressBkGnd) color property }
  cAddressBkgndDef = clWindow;
  { Default value for the @link(TKHexEditorColors.BkGnd) color property }
  cBkGndDef = clWindow;
  { Default value for the @link(TKHexEditorColors.DigitTextEven) color property }
  cDigitTextEvenDef = clMaroon;
  { Default value for the @link(TKHexEditorColors.DigitTextOdd) color property }
  cDigitTextOddDef = clRed;
  { Default value for the @link(TKHexEditorColors.DigitBkGnd) color property }
  cDigitBkGndDef = clWindow;
  { Default value for the @link(TKHexEditorColors.HorzLines) color property }
  cHorzLinesDef = clWindowText;
  { Default value for the @link(TKHexEditorColors.InactiveCaretBkGnd) color property }
  cInactiveCaretBkGndDef = clBlack;
  { Default value for the @link(TKHexEditorColors.InactiveCaretSelBkGnd) color property }
  cInactiveCaretSelBkGndDef = clBlack;
  { Default value for the @link(TKHexEditorColors.InactiveCaretSelText) color property }
  cInactiveCaretSelTextDef = clYellow;
  { Default value for the @link(TKHexEditorColors.InactiveCaretText) color property }
  cInactiveCaretTextDef = clYellow;
  { Default value for the @link(TKHexEditorColors.LinesHighLight) color property }
  cLinesHighLightDef = clHighLightText;
  { Default value for the @link(TKHexEditorColors.SelBkGnd) color property }
  cSelBkGndDef = clGrayText;
  { Default value for the @link(TKHexEditorColors.SelBkGndFocused) color property }
  cSelBkGndFocusedDef = clHighlight;
  { Default value for the @link(TKHexEditorColors.SelText) color property }
  cSelTextDef = clHighlightText;
  { Default value for the @link(TKHexEditorColors.SelTextFocused) color property }
  cSelTextFocusedDef = clHighlightText;
  { Default value for the @link(TKHexEditorColors.Separators) color property }
  cSeparatorsDef = clWindowText;
  { Default value for the @link(TKHexEditorColors.TextText) color property }
  cTextTextDef = clWindowText;
  { Default value for the @link(TKHexEditorColors.TextBkgnd) color property }
  cTextBkgndDef = clWindow;
  { Default value for the @link(TKHexEditorColors.VertLines) color property }
  cVertLinesDef = clWindowText;

  { Index for the @link(TKHexEditorColors.AddressText) color property }
  ciAddressText = TKHexEditorColorIndex(0);
  { Index for the @link(TKHexEditorColors.AddressBkGnd) color property }
  ciAddressBkGnd = TKHexEditorColorIndex(1);
  { Index for the @link(TKHexEditorColors.BkGnd) color property }
  ciBkGnd = TKHexEditorColorIndex(2);
  { Index for the @link(TKHexEditorColors.DigitTextEven) color property }
  ciDigitTextEven = TKHexEditorColorIndex(3);
  { Index for the @link(TKHexEditorColors.DigitTextOdd) color property }
  ciDigitTextOdd = TKHexEditorColorIndex(4);
  { Index for the @link(TKHexEditorColors.DigitBkGnd) color property }
  ciDigitBkGnd = TKHexEditorColorIndex(5);
  { Index for the @link(TKHexEditorColors.HorzLines) color property }
  ciHorzLines = TKHexEditorColorIndex(6);
  { Index for the @link(TKHexEditorColors.InactiveCaretBkGnd) color property }
  ciInactiveCaretBkGnd = TKHexEditorColorIndex(7);
  { Index for the @link(TKHexEditorColors.InactiveCaretSelBkGnd) color property }
  ciInactiveCaretSelBkGnd = TKHexEditorColorIndex(8);
  { Index for the @link(TKHexEditorColors.InactiveCaretSelText) color property }
  ciInactiveCaretSelText = TKHexEditorColorIndex(9);
  { Index for the @link(TKHexEditorColors.InactiveCaretText) color property }
  ciInactiveCaretText = TKHexEditorColorIndex(10);
  { Index for the @link(TKHexEditorColors.LinesHighLight) color property }
  ciLinesHighLight = TKHexEditorColorIndex(11);
  { Index for the @link(TKHexEditorColors.SelBkGnd) color property }
  ciSelBkGnd = TKHexEditorColorIndex(12);
  { Index for the @link(TKHexEditorColors.SelBkGndFocused) color property }
  ciSelBkGndFocused = TKHexEditorColorIndex(13);
  { Index for the @link(TKHexEditorColors.SelText) color property }
  ciSelText = TKHexEditorColorIndex(14);
  { Index for the @link(TKHexEditorColors.SelTextFocused) color property }
  ciSelTextFocused = TKHexEditorColorIndex(15);
  { Index for the @link(TKHexEditorColors.Separators) color property }
  ciSeparators = TKHexEditorColorIndex(16);
  { Index for the @link(TKHexEditorColors.TextText) color property }
  ciTextText = TKHexEditorColorIndex(17);
  { Index for the @link(TKHexEditorColors.TextBkgnd) color property }
  ciTextBkGnd = TKHexEditorColorIndex(18);
  { Index for the @link(TKHexEditorColors.VertLines) color property }
  ciVertLines = TKHexEditorColorIndex(19);
  { Maximum color array index }
  ciHexEditorColorsMax = ciVertLines;

  { Default value for the @link(TKCustomHexEditor.AddressMode) property }
  cAddressModeDef = eamHex;

  { Default value for the @link(TKCustomHexEditor.Addressoffset) property }
  cAddressOffsetDef = 0;

  { Default value for the @link(TKCustomHexEditor.DisabledDrawStyle) property }
  cDisabledDrawStyleDef = eddBright;

  { Default value for the  @link(TKCustomHexEditor.DrawStyles) property }
  cDrawStylesDef = [edAddress, edDigits, edText, edInactiveCaret, edSeparators];

  { Default value for the @link(TKCustomHexEditor.AddressPrefix) property }
  cAddressPrefixDef = '0x';

  { Default value for the @link(TKHexEditor.Font).Name property }
  cFontNameDef = {$IFDEF MSWINDOWS}'Courier New'{$ELSE}'Courier'{$ENDIF};

  { Default value for the @link(TKHexEditor.Font).Style property }
  cFontStyleDef = [fsBold];

  { Declares the Index member of the @link(TKHexEditorSelection) record invalid}
  cInvalidIndex = -1;

  { Default value for the @link(TKCustomHexEditor.AddressCursor) property }
  cAddressCursorDef = crHandPoint;

  { Default value for the @link(TKHexEditor.Height) property }
  cHeight = 300;

  { Default value for the @link(TKHexEditor.Width) property }
  cWidth = 400;

type
  TKCustomHexEditor = class;

  { @abstract(Container for all colors used by @link(TKCustomHexEditor) class)
    This container allows to group many colors into one item in object inspector.
    Colors are accessible via published properties or several public Color*
    properties.
  }
  TKHexEditorColors = class(TPersistent)
  private
    FOwner: TKCustomHexEditor;
    FBrightColors: TKColorArray;
    FColors: TKColorArray;
    FColorScheme: TKHexEditorColorScheme;
    FSingleBkGnd: Boolean;
    function GetColor(Index: TKHexEditorColorIndex): TColor;
    function GetColorData(Index: TKHexEditorColorIndex): TKHexEditorColorData;
    function GetColorEx(Index: TKHexEditorColorIndex): TColor;
    function GetColorName(Index: TKHexEditorColorIndex): string;
    procedure SetColor(Index: TKHexEditorColorIndex; Value: TColor);
    procedure SetColorEx(Index: TKHexEditorColorIndex; Value: TColor);
    procedure SetColors(const Value: TKColorArray);
  public
    { Performs necessary initializations }
    constructor Create(AOwner: TKCustomHexEditor);
    { Takes property values from another TKHexEditorColors class }
    procedure Assign(Source: TPersistent); override;
    { Clears cached brighter colors }
    procedure ClearBrightColors;
    { Initializes the color array. }
    procedure Initialize; virtual;
    { Specifies color scheme for reading of published properties - see GetColor in source code}
    property ColorScheme: TKHexEditorColorScheme read FColorScheme write FColorScheme;
    { Returns always normal color - regardless of the ColorScheme setting }
    property Color[Index: TKHexEditorColorIndex]: TColor read GetColorEx write SetColorEx;
    { Returns always a complete color description }
    property ColorData[Index: TKHexEditorColorIndex]: TKHexEditorColorData read GetColorData;
    { Returns (localizable) color name }
    property ColorName[Index: TKHexEditorColorIndex]: string read GetColorName;
    { Returns array of normal colors }
    property Colors: TKColorArray read FColors write SetColors;
    { @link(TKHexEditorColors.BkGnd) is used for all areas if True - @link(edSingleBkGnd) forward }
    property SingleBkGnd: Boolean read FSingleBkGnd write FSingleBkGnd;
  published
    { Address area text color }
    property AddressText: TColor index ciAddressText read GetColor write SetColor default cAddressTextDef;
    { Address area background color }
    property AddressBkGnd: TColor index ciAddressBkgnd read GetColor write SetColor default cAddressBkGndDef;
    { Hex editor client area background }
    property BkGnd: TColor index ciBkGnd read GetColor write SetColor default cBkGndDef;
    { Digits area text color - even digit group }
    property DigitTextEven: TColor index ciDigitTextEven read GetColor write SetColor default cDigitTextEvenDef;
    { Digits area text color - odd digit group }
    property DigitTextOdd: TColor index ciDigitTextOdd read GetColor write SetColor default cDigitTextOddDef;
    { Digits area background color }
    property DigitBkGnd: TColor index ciDigitBkGnd read GetColor write SetColor default cDigitBkGndDef;
    { Color of the horizontal leading lines }
    property HorzLines: TColor index ciHorzLines read GetColor write SetColor default cHorzLinesDef;
    { Inactive (hex editor without focus) caret background color - caret mark is not part of a selection }
    property InactiveCaretBkGnd: TColor index ciInactiveCaretBkGnd read GetColor write SetColor default cInactiveCaretBkGndDef;
    { Inactive (hex editor without focus) caret background color - caret mark is part of a selection }
    property InactiveCaretSelBkGnd: TColor index ciInactiveCaretSelBkGnd read GetColor write SetColor default cInactiveCaretSelBkGndDef;
    { Inactive (hex editor without focus) caret text color - caret mark is part of a selection }
    property InactiveCaretSelText: TColor index ciInactiveCaretSelText read GetColor write SetColor default cInactiveCaretSelTextDef;
    { Inactive (hex editor without focus) caret text color - caret mark is not part of a selection }
    property InactiveCaretText: TColor index ciInactiveCaretText read GetColor write SetColor default cInactiveCaretTextDef;
    { Color of horizontal leading lines involved into a selection }
    property LinesHighLight: TColor index ciLinesHighLight read GetColor write SetColor default cLinesHighLightDef;
    { Selection background - inactive edit area }
    property SelBkGnd: TColor index ciSelBkGnd read GetColor write SetColor default cSelBkGndDef;
    { Selection background - active edit area }
    property SelBkGndFocused: TColor index ciSelBkGndFocused read GetColor write SetColor default cSelBkGndFocusedDef;
    { Selection text - inactive edit area }
    property SelText: TColor index ciSelText read GetColor write SetColor default cSelTextDef;
    { Selection text - active edit area }
    property SelTextFocused: TColor index ciSelTextFocused read GetColor write SetColor default cSelTextFocusedDef;
    { Color of the vertical area separating lines }
    property Separators: TColor index ciSeparators read GetColor write SetColor default cSeparatorsDef;
    { Text area text color }
    property TextText: TColor index ciTextText read GetColor write SetColor default cTextTextDef;
    { Text area background color }
    property TextBkgnd: TColor index ciTextBkgnd read GetColor write SetColor default cTextBkGndDef;
    { Color of the vertical leading lines }
    property VertLines: TColor index ciVertLines read GetColor write SetColor default cVertLinesDef;
  end;

  { Declares possible values for the ItemReason member of the @link(TKHexEditorChangeItem) structure }
  TKHexEditorChangeReason = (
    { Save caret position only }
    crCaretPos,
    { Save inserted character to be able to delete it }
    crDeleteChar,
    { Save inserted hexadecimal digits to be able to delete them }
    crDeleteDigits,
    { Save inserted binary string to be able to delete it }
    crDeleteString,
    { Save deleted character to be able to insert it }
    crInsertChar,
    { Save deleted hexadecimal digits to be able to insert them }
    crInsertDigits,
    { Save deleted binary string to be able to insert it }
    crInsertString
    );

  { @abstract(Declares @link(TKHexEditorChangeList.OnChange) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>ItemReason</I> - specifies the undo/redo reason</LI>
    </UL>
  }
  TKHexEditorUndoChangeEvent = procedure(Sender: TObject;
    ItemReason: TKHexEditorChangeReason) of object;

  { @abstract(Declares the undo/redo item description structure used by the @link(TKHexEditorChangeList) class)
    <UL>
    <LH>Members:</LH>
    <LI><I>Data</I> - characters (binary or digit string) needed to execute this item</LI>
    <LI><I>EditArea</I> - active edit area at the time this item was recorded</LI>
    <LI><I>Group</I> - identifies the undo/redo group. Some editor modifications
      produce a sequence of 2 or more undo items. This sequence is called undo/redo
      group and is always interpreted as a single undo/redo item. Moreover,
      if there is eoGroupUndo in @link(TKCustomHexEditor.Options),
      a single ecUndo or ecRedo command manipulates all following undo groups
      of the same kind (reason) as if they were a single undo/redo item. </LI>
    <LI><I>GroupReason</I> - reason (kind) of this undo group</LI>
    <LI><I>ItemReason</I> - reason (kind) of this item</LI>
    <LI><I>SelEnd</I> - end of the selection at the time this item was recorded</LI>
    <LI><I>SelStart</I> - start of the selection at the time this item was recorded</LI>
    </UL>
  }
  TKHexEditorChangeItem = record
    Data: AnsiString;
    EditArea: TKHexEditorArea;
    Group: Cardinal;
    GroupReason: TKHexEditorChangeReason;
    Inserted: Boolean;
    ItemReason: TKHexEditorChangeReason;
    SelEnd: TKHexEditorSelection;
    SelStart: TKHexEditorSelection;
  end;

  { Pointer to @link(TKHexEditorChangeItem) }
  PKHexEditorChangeItem = ^TKHexEditorChangeItem;

  { @abstract(Change (undo/redo item) list manager) }
  TKHexEditorChangeList = class(TList)
  private
    FEditor: TKCustomHexEditor;
    FGroup: Cardinal;
    FGroupUseLock: Integer;
    FGroupReason: TKHexEditorChangeReason;
    FIndex: Integer;
    FModifiedIndex: Integer;
    FLimit: Integer;
    FRedoList: TKHexEditorChangeList;
    FOnChange: TKHexEditorUndoChangeEvent;
    function GetModified: Boolean;
    procedure SetLimit(Value: Integer);
    procedure SetModified(Value: Boolean);
  protected
    { Redefined to properly destroy the items }
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    { Performs necessary initializations
      <UL>
      <LH>Parameters:</LH>
      <LI><I>AEditor</I> - identifies the undo/redo list owner</LI>
      <LI><I>RedoList</I> - when this instance is used as undo list, specify
      a redo list to allow clear it at each valid AddChange call</LI>
      </UL>}
    constructor Create(AEditor: TKCustomHexEditor; RedoList: TKHexEditorChangeList);
    { Inserts a undo/redo item
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemReason</I> - specifies the undo/redo item reason. The change list doesn't
      allow to insert succesive crCaretPos items unless Inserted is True</LI>
      <LI><I>Data</I> - specifies the item data. Some items (crCaretPos)
      don't need to supply any data</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies whether the item
      was recorded with @link(TKCustomHexEditor.InsertMode) on (True) or
      off (False). See ItemReason for crCaretPos behavior.</LI>
      </UL>}
    procedure AddChange(ItemReason: TKHexEditorChangeReason; const Data: AnsiString = '';
      Inserted: Boolean = True); virtual;
    { Tells the undo list a new undo/redo group is about to be created. Each
      BeginGroup call must have a corresponding EndGroup call (use try-finally).
      BeginGroup calls may be nested, however, only the first call will create an
      undo/redo group. Use the GroupReason parameter to specify the reason of this group. }
    procedure BeginGroup(GroupReason: TKHexEditorChangeReason); virtual;
    { Informs whether there are any undo/redo items available - i.e. CanUndo/CanRedo}
    function CanPeek: Boolean;
    { Clears the entire list - overriden to execute some adjustments }
    procedure Clear; override;
    { Completes the undo/redo group. See @link(TKHexEditorChangeList.BeginGroup) for details }
    procedure EndGroup; virtual;
    { Returns the topmost item to handle or inspect it}
    function PeekItem: PKHexEditorChangeItem;
    { If there is no reason to handle an item returned by PeekItem, it has to be
    poked back with this function to become active for next undo/redo command }
    procedure PokeItem;
    { For redo list only - each undo command creates a redo command with the same
      group information - see source }
    procedure SetGroupData(Group: Integer; GroupReason: TKHexEditorChangeReason);
    { Specifies maximum number of items - not groups }
    property Limit: Integer read FLimit write SetLimit;
    { For undo list only - returns True if undo list contains some items with regard
      to the eoUndoAfterSave option }
    property Modified: Boolean read GetModified write SetModified;
    { Allows to call TKCustomHexEditor.@link(TKCustomHexEditor.OnChange) event}
    property OnChange: TKHexEditorUndoChangeEvent read FOnChange write FOnChange;
  end;

  { @abstract(Hexadecimal editor base component) }
  TKCustomHexEditor = class(TKCustomControl)
  private
    FAddressCursor: TCursor;
    FAddressMode: TKHexEditorAddressMode;
    FAddressOffset: Integer;
    FAddressPrefix: string;
    FAddressSize: Integer;
    FAreaSpacing: Integer;
    FBuffer: PBytes;
    FCharHeight: Integer;
    FCharMapping: TKEditCharMapping;
    FCharSpacing: Integer;
    FCharWidth: Integer;
    FClipboardFormat: Word;
    FColors: TKHexEditorColors;
    FDigitGrouping: Integer;
    FDisabledDrawStyle: TKHexEditorDisabledDrawStyle;
    FDrawStyles: TKHexEditorDrawStyles;
    FEditArea: TKHexEditorArea;
    FKeyMapping: TKEditKeyMapping;
    FLeftChar: Integer;
    FLineHeightPercent: Integer;
    FLineSize: Integer;
    FMouseWheelAccumulator: Integer;
    FOptions: TKEditOptions;
    FRedoList: TKHexEditorChangeList;
    FScrollBars: TScrollStyle;
    FScrollDeltaX: Integer;
    FScrollDeltaY: Integer;
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FSelEnd: TKHexEditorSelection;
    FSelStart: TKHexEditorSelection;
    FSize: Integer;
    FStates: TKHexEditorStates;
    FTopLine: Integer;
    FTotalCharSpacing: Integer;
    FUndoList: TKHexEditorChangeList;
    FOnChange: TNotifyEvent;
    FOnDropFiles: TKEditDropFilesEvent;
    FOnReplaceText: TKEditReplaceTextEvent;
    function GetCommandKey(Index: TKEditCommand): TKEditKey;
    function GetCaretVisible: Boolean;
    function GetData: TDataSize;
    function GetEmpty: Boolean;
    function GetFirstVisibleIndex: Integer;
    function GetInsertMode: Boolean;
    function GetLastVisibleIndex: Integer;
    function GetLineCount: Integer;
    function GetLines(Index: Integer): TDataSize;
    function GetModified: Boolean;
    function GetReadOnly: Boolean;
    function GetSelLength: TKHexEditorSelection;
    function GetSelText: TKHexEditorSelText;
    function GetUndoLimit: Integer;
    function IsAddressPrefixStored: Boolean;
    function IsDrawStylesStored: Boolean;
    function IsOptionsStored: Boolean;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SetAddressCursor(Value: TCursor);
    procedure SetAddressMode(Value: TKHexEditorAddressMode);
    procedure SetAddressOffset(Value: Integer);
    procedure SetAddressPrefix(const Value: string);
    procedure SetAddressSize(Value: Integer);
    procedure SetAreaSpacing(Value: Integer);
    procedure SetCharSpacing(Value: Integer);
    procedure SetColors(Value: TKHexEditorColors);
    procedure SetCommandKey(Index: TKEditCommand; Value: TKEditKey);
    procedure SetData(Value: TDataSize);
    procedure SetDigitGrouping(Value: Integer);
    procedure SetDisabledDrawStyle(Value: TKHexEditorDisabledDrawStyle);
    procedure SetDrawStyles(const Value: TKHexEditorDrawStyles);
    procedure SetEditArea(Value: TKHexEditorArea);
    procedure SetLeftChar(Value: Integer);
    procedure SetLineHeightPercent(Value: Integer);
    procedure SetLines(Index: Integer; const Value: TDataSize);
    procedure SetLineSize(Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(const Value: TKEditOptions);
    procedure SetReadOnly(Value: Boolean);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetScrollSpeed(Value: Cardinal);
    procedure SetSelEnd(Value: TKHexEditorSelection);
    procedure SetSelLength(Value: TKHexEditorSelection);
    procedure SetSelStart(Value: TKHexEditorSelection);
    procedure SetTopLine(Value: Integer);
    procedure SetUndoLimit(Value: Integer);
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Msg: TLMessage); message CM_SYSCOLORCHANGE;
  {$IFNDEF FPC}
    // no way to get filenames in Lazarus inside control (why??)
    procedure WMDropFiles(var Msg: TLMessage); message LM_DROPFILES;
  {$ENDIF}
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  protected
    { Inserts a single crCaretPos item into undo list. Unless Force is set to True,
      this change will be inserted only if previous undo item is not crCaretPos. }
    procedure AddUndoCaretPos(Force: Boolean = True);
    { Inserts a single byte change into undo list.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemReason</I> - specifies the undo/redo item reason - most likely
      crInsertChar or crDeleteChar.</LI>
      <LI><I>Data</I> - specifies the data byte needed to restore the original
      buffer state</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies the current
      @link(TKCustomHexEditor.InsertMode) status.</LI>
      </UL>}
    procedure AddUndoByte(ItemReason: TKHexEditorChangeReason; Data: Byte;
      Inserted: Boolean = True);
    { Inserts a byte array change into undo list.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemReason</I> - specifies the undo/redo item reason - crInsert* or
      crDelete*.</LI>
      <LI><I>Data</I> - specifies the data bytes needed to restore the original
      buffer state</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies the current
      @link(TKCustomHexEditor.InsertMode) status.</LI>
      </UL>}
    procedure AddUndoBytes(ItemReason: TKHexEditorChangeReason; Data: PBytes;
      Length: Integer; Inserted: Boolean = True);
    { Inserts a string change into undo list. Has the same functionality as AddUndoBytes
      only Data is supplied as a string. }
    procedure AddUndoString(ItemReason: TKHexEditorChangeReason; const S: AnsiString;
      Inserted: Boolean = True);
    { Begins a new undo group. Use the GroupReason parameter to label it. }
    procedure BeginUndoGroup(GroupReason: TKHexEditorChangeReason);
    { Performs necessary adjustments when the buffer is modified programatically
      (not by user) }
    procedure BufferChanged;
    { Determines whether an ecScroll* command can be executed }
    function CanScroll(Command: TKEditCommand): Boolean; virtual;
    { Clears a character at position At. Doesn't perform any succesive adjustments. }
    procedure ClearChar(At: Integer);
    { Clears a the digit fields both in SelStart and SelEnd. Doesn't perform any succesive adjustments.}
    procedure ClearDigitSelection;
    { Clears a string of the Size length at position At. Doesn't perform any succesive adjustments. }
    procedure ClearString(At, Size: Integer);
    { Overriden method - defines additional styles for the hex editor window (scrollbars etc.)}
    procedure CreateParams(var Params: TCreateParams); override;
    { Overriden method - adjusts file drag&drop functionality }
    procedure CreateWnd; override;
    { Overriden method - adjusts file drag&drop functionality }
    procedure DestroyWnd; override;
    { Calls the @link(TKCustomHexEditor.OnChange) event }
    procedure DoChange; virtual;
    { Overriden method - handles mouse wheel messages }
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    { Validates the EditArea property after it has been modified }
    procedure EditAreaChanged; virtual;
    { Closes the undo group created by @link(TKCustomHexEditor.BeginUndoGroup) }
    procedure EndUndoGroup;
    { Ensures that font pitch is always fpFixed and Font.Size is not too small or big }
    procedure FontChange(Sender: TObject); virtual;
    { Returns the horizontal page extent for the current edit area. This function is
      used by the ecPageLeft and ecPageRight commands. }
    function GetPageHorz: Integer; virtual;
    { Determines if the editor has input focus. }
    function HasFocus: Boolean; virtual;
    { Hides the caret. }
    procedure HideEditorCaret; virtual;
    { Inserts a character at specified position. Doesn't perform any succesive adjustments.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the character should be inserted.</LI>
      <LI><I>Value</I> - character (data byte)</LI>
      </UL> }
    procedure InsertChar(At: Integer; Value: Byte);
    { Inserts a string at specified position. Doesn't perform any succesive adjustments.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the string should be inserted.</LI>
      <LI><I>Value</I> - data byte string</LI>
      <LI><I>Size</I> - length of the data byte string</LI>
      </UL> }
    procedure InsertString(At: Integer; const Value: AnsiString; Size: Integer);
    { Returns True if the control has a selection. }
    function InternalGetSelAvail: Boolean; override;
    { Moves the caret one position left. Doesn't perform any succesive adjustments.}
    procedure InternalMoveLeft; virtual;
    { Moves the caret one position right. Doesn't perform any succesive adjustments.}
    procedure InternalMoveRight; virtual;
    { Overriden method - processes virtual key strokes according to current @link(TKCustomHexEditor.KeyMapping) }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Overriden method - processes character key strokes - data editing }
    procedure KeyPress(var Key: Char); override;
    { Updates information about printed shape. }
    procedure MeasurePages(var Info: TKPrintMeasureInfo); override;
    { Processes scrollbar messages.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ScrollBar</I> - scrollbar type from OS</LI>
      <LI><I>ScrollCode</I> - scrollbar action from OS</LI>
      <LI><I>Delta</I> - scrollbar position change</LI>
      <LI><I>UpdateNeeded</I> - set to True if you want to invalidate
      and update caret position</LI>
      </UL> }
    procedure ModifyScrollBar(ScrollBar, ScrollCode, Delta: Integer;
      UpdateNeeded: Boolean);
    { Overriden method - updates caret position/selection }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - updates caret position/selection and initializes scrolling
      when needed. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - releases mouse capture acquired by MouseDown }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - calls PaintLines for drawing the hex editor outline
      into window client area }
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    { Paints/prints hex editor outline. This function must retain its reentrancy.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Data</I> - paint settings</LI>
      </UL> }
    procedure PaintLines(const Data: TKHexEditorPaintData); virtual;
    { Paints a page to a printer/preview canvas. }
    procedure PaintPage; override;
    { Grants the input focus to the control when possible and the control has had none before }
    procedure SafeSetFocus;
    { Performs necessary adjustments after a selection property changed.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>StartEqualEnd</I> - forces SelStart equal to SelEnd</LI>
      <LI><I>ScrollToView</I> - forces scrolling if SelEnd (caret) became invisible</LI>
      </UL> }
    procedure SelectionChanged(StartEqualEnd: Boolean; ScrollToView: Boolean = True);
    { Scrolls the hex editor window horizontaly by HChars characters and/or
      vertically by VChars characters }
    procedure ScrollBy(HChars, VChars: Integer; UpdateNeeded: Boolean);
    { Scrolls the hex editor window to ensure data under defined (mouse) coordinates are visible
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Point</I> - (mouse) coordinates</LI>
      <LI><I>Timed</I> - set to True to continue scroll via a timer. The scrolling
      will continue until the mouse cursor is outside of the modified client rect
      (@link(TKCustomHexEditor.GetModifiedClientRect)).</LI>
      <LI><I>AlwaysScroll</I> - set to True to disable new line overscrolling</LI>
      </UL> }
    procedure ScrollTo(Point: TPoint; Timed, AlwaysScroll: Boolean); virtual;
    { Updates mouse cursor according to the state determined from current mouse
      position. Returns True if cursor has been changed. }
    function SetMouseCursor(X, Y: Integer): Boolean; override;
    { Shows the caret. }
    procedure ShowEditorCaret; virtual;
    { Calls the @link(TKCustomHexEditor.DoChange) method}
    procedure UndoChange(Sender: TObject; ItemReason: TKHexEditorChangeReason);
    { Updates caret position, shows/hides caret according to the input focus
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Recreate</I> - set to True to recreate the caret after it has already
      been created and displayed</LI>
      </UL> }
    procedure UpdateEditorCaret(Recreate: Boolean = False); virtual;
    { Updates font based dimensions }
    procedure UpdateCharMetrics; virtual;
    { Updates mouse cursor }
    procedure UpdateMouseCursor; virtual;
    { Updates the scrolling range }
    procedure UpdateScrollRange; virtual;
    { Updates selection according to the supplied coordinates.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Point</I> - specifies the coordinates </LI>
      <LI><I>ClipToClient</I> - specifies whether the coordinates should be clipped
      to modified client rectangle (@link(TKCustomHexEditor.GetModifiedClientRect))
      first</LI>
      </UL> }
    procedure UpdateSelEnd(Point: TPoint; ClipToClient: Boolean); virtual;
    { Updates the control size. }
    procedure UpdateSize; override;
    { Data buffer - made accessible for descendant classes }
    property Buffer: PBytes read FBuffer write FBuffer;
    { Redo list manager - made accessible for descendant classes }
    property RedoList: TKHexEditorChangeList read FRedoList;
    { Data buffer size - made accessible for descendant classes }
    property Size: Integer read FSize write FSize;
    { States of this class - made accessible for descendant classes }
    property States: TKHexEditorStates read FStates write FStates;
    { Undo list manager - made accessible for descendant classes }
    property UndoList: TKHexEditorChangeList read FUndoList;
  public
    { Performs necessary initializations - default values to properties, create
      undo/redo list managers }
    constructor Create(AOwner: TComponent); override;
    { Destroy instance, undo/redo list managers, dispose buffer... }
    destructor Destroy; override;
    { Appends data at current position. Use TKHexEditor.Data.Size for At parameter
      to append at the end of the buffer. }
    procedure Append(At: Integer; Data: TDataSize); overload; virtual;
    { Appends data at current position. Use TKHexEditor.Data.Size for At parameter
      to append at the end of the buffer. }
    procedure Append(At: Integer; const Data: AnsiString); overload; virtual;
    { Takes property values from another TKCustomHexEditor class }
    procedure Assign(Source: TPersistent); override;
    { Determines whether the caret is visible }
    function CaretInView: Boolean;
    { Clears entire data buffer. Unlike ecClearAll this method clears everything
      inclusive undo a redo lists. }
    procedure Clear;
    { Clears undo (and redo) list }
    procedure ClearUndo;
    { Determines whether given command can be executed at this time. Use this
      function in TAction.OnUpdate events.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Command</I> - specifies the command to inspect</LI>
      </UL> }
    function CommandEnabled(Command: TKEditCommand): Boolean; virtual;
    { Executes given command. This function first calls CommandEnabled to
      assure given command can be executed.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Command</I> - specifies the command to execute</LI>
      <LI><I>Data</I> - specifies the data needed for the command</LI>
      </UL> }
    function ExecuteCommand(Command: TKEditCommand; Data: Pointer = nil): Boolean; virtual;
    { Returns dimensions of all 3 possible areas according to current area
      definition }
    function GetAreaDimensions: TKHexEditorAreaDimensions; virtual;
    { Returns current character mapping. }
    function GetCharMapping: TKEditCharMapping;
    { Returns number of characters that vertically fit into client window }
    function GetClientHeightChars: Integer; virtual;
    { Returns number of characters that horizontally fit into client window }
    function GetClientWidthChars: Integer; virtual;
    { Returns the current key stroke mapping scheme. }
    function GetKeyMapping: TKEditKeyMapping;
    { Returns modified client rect - a window client rect aligned to character width and
      character height }
    function GetModifiedClientRect: TRect; virtual;
    { Returns current maximum value for the @link(TKCustomHexEditor.LeftChar) property
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Extent</I> - specify @link(TKHexEditorAreaDimensions).TotalHorz
      here, otherwise the function calculates it itself</LI>
      </UL> }
    function GetMaxLeftChar(Extent: Integer = 0): Integer; virtual;
    { Returns current maximum value for the @link(TKCustomHexEditor.TopLine) property
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Extent</I> - specify @link(TKHexEditorAreaDimensions).TotalVert
      here, otherwise the function calculates it itself</LI>
      </UL> }
    function GetMaxTopLine(Extent: Integer = 0): Integer; virtual;
    { Returns "real" selection end - with always higher index value than selection
      start value }
    function GetRealSelEnd: TKHexEditorSelection;
    { Returns "real" selection start - with always lower index value than selection
      end value }
    function GetRealSelStart: TKHexEditorSelection;
    { Loads data from a file }
    procedure LoadFromFile(const FileName: TFileName);
    { Loads data from a stream - stream position remains untouched }
    procedure LoadFromStream(Stream: TStream);
    { Paints the editor outline to another canvas
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ACanvas</I> - canvas to paint the outline to</LI>
      <LI><I>ARect</I> - given rectangle in the canvas</LI>
      <LI><I>ALeftChar</I> - first left visible character</LI>
      <LI><I>ATopLine</I> - first top visible line</LI>
      </UL> }
    procedure PaintToCanvasEx(ACanvas: TCanvas; ARect: TRect; ALeftChar, ATopLine: Integer);
    { Converts window coordinates into a selection
      <UL>
      <LH>Parameters:</LH>
      <LI><I>P</I> - window client coordinates</LI>
      <LI><I>OutOfArea</I> - uses the Area parameter to compute selection for
      this area even if the supplied coordinates are outside of the area outline</LI>
      <LI><I>Area</I> output parameter if OutOfArea = False, otherwise
      input parameter</LI>
      </UL> }
    function PointToSel(P: TPoint; OutOfArea: Boolean; var Area: TKHexEditorArea): TKHexEditorSelection; virtual;
    { Saves data into a file }
    procedure SaveToFile(const FileName: TFileName);
    { Saves data into a stream - stream position remains untouched }
    procedure SaveToStream(Stream: TStream);
    { Determines whether a seletion (not digit selection) is available }
    function SelAvail: Boolean;
    { Determines whether a given selection is valid for given area
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - selection to examine</LI>
      <LI><I>Area</I> - area for which the selection must be examined</LI>
      </UL> }
    function SelectionValid(Value: TKHexEditorSelection; Area: TKHexEditorArea): Boolean; virtual;
    { Converts a selection into window coordinates
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - selection to convert</LI>
      <LI><I>Area</I> - the same selection delivers another coordinates for each area</LI>
      </UL> }
    function SelToPoint(Value: TKHexEditorSelection; Area: TKHexEditorArea): TPoint; virtual;
    { Specifies character mapping. The main purpose of this is to avoid non-printable
      characters in the text area and in AsText copies. Avoid non-printable characters
      when delivering a new character mapping. }
    procedure SetCharMapping(const Value: TKEditCharMapping);
    { Specifies the current key stroke mapping scheme }
    procedure SetKeyMapping(const Value: TKEditKeyMapping);
    { Validates a selection for given area
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - selection to validate</LI>
      <LI><I>Area</I> - area for which the selection must be validated</LI>
      </UL> }
    procedure ValidateSelection(var Value: TKHexEditorSelection; Area: TKHexEditorArea); virtual;
    { Specifies the address area mouse cursor. Other areas have crIBeam - should not
      be needed to modify that }
    property AddressCursor: TCursor read FAddressCursor write SetAddressCursor default cAddressCursorDef;
    { Specifies the radix of addresses }
    property AddressMode: TKHexEditorAddressMode read FAddressMode write SetAddressMode default cAddressModeDef;
    { Specifies the address offset }
    property AddressOffset: Integer read FAddressOffset write SetAddressOffset default cAddressOffsetDef;
    { Specifies the address number prefix i.e. 0x or $ - modify together with AddressMode }
    property AddressPrefix: string read FAddressPrefix write SetAddressPrefix stored IsAddressPrefixStored;
    { Specifies the number of address digits - up to 10 for decimal addresses }
    property AddressSize: Integer read FAddressSize write SetAddressSize default cAddressSizeDef;
    { Defines space between neighbour areas }
    property AreaSpacing: Integer read FAreaSpacing write SetAreaSpacing default cAreaSpacingDef;
    { Returns current caret position = selection end }
    property CaretPos: TKHexEditorSelection read FSelEnd;
    { Returns True if caret is visible }
    property CaretVisible: Boolean read GetCaretVisible;
    { Returns current character width = not necessarily equal to font character width }
    property CharWidth: Integer read FCharWidth;
    { Defines additional inter-character spacing }
    property CharSpacing: Integer read FCharSpacing write SetCharSpacing default cCharSpacingDef;
    { Returns current character height = not equal to font character height }
    property CharHeight: Integer read FCharHeight;
    { Returns the binary data clipboard format }
    property ClipboardFormat: Word read FClipboardFormat;
    { Makes it possible to take all color properties from another TKCustomHexEditor class }
    property Colors: TKHexEditorColors read FColors write SetColors;
    { Specifies a new key stroke combination for given command }
    property CommandKey[Index: TKEditCommand]: TKEditKey read GetCommandKey write SetCommandKey;
    { This property provides direct access to the data buffer }
    property Data: TDataSize read GetData write SetData;
    { Specifies the byte grouping in the digits area }
    property DigitGrouping: Integer read FDigitGrouping write SetDigitGrouping default cDigitGroupingDef;
    { Specifies the style how the outline is drawn when editor is disabled }
    property DisabledDrawStyle: TKHexEditorDisabledDrawStyle read FDisabledDrawStyle write SetDisabledDrawStyle default cDisabledDrawStyleDef;
    { Defines areas to paint, whether to paint horizontal and vertical trailing lines,
      area separator lines and caret mark when the editor has no input focus }
    property DrawStyles: TKHexEditorDrawStyles read FDrawStyles write SetDrawStyles stored IsDrawStylesStored;
    { Specifies the current area for editing }
    property EditArea: TKHexEditorArea read FEditArea write SetEditArea default eaDigits;
    { Returns True if data buffer is empty }
    property Empty: Boolean read GetEmpty;
    { Returns the first visible index }
    property FirstVisibleIndex: Integer read GetFirstVisibleIndex;
    { Returns True if insert mode is on }
    property InsertMode: Boolean read GetInsertMode;
    { Returns the last visible index }
    property LastVisibleIndex: Integer read GetLastVisibleIndex;
    { Specifies the horizontal scroll position }
    property LeftChar: Integer read FLeftChar write SetLeftChar;
    { Determines the number of lines }
    property LineCount: Integer read GetLineCount;
    { Specifies the line height. 100% is the current font height }
    property LineHeightPercent: Integer read FLineHeightPercent write SetLineHeightPercent default cLineHeightPercentDef;
    { Allows to modify/add data lines. If greater than LineSize, the Size member
      of the supplied TDataSize structure will be always trimmed to LineSize.
      If Index points to last incomplete line or even higher, last line will be
      extended/completed, i.e new data will be added to the buffer }
    property Lines[Index: Integer]: TDataSize read GetLines write SetLines;
    { Specifies the size (length) of a single line }
    property LineSize: Integer read FLineSize write SetLineSize default cLineSizeDef;
    { Returns True if the buffer was modified - eoUndoAfterSave taken into
      account }
    property Modified: Boolean read GetModified write SetModified;
    { Specifies the editor options that do not affect painting }
    property Options: TKEditOptions read FOptions write SetOptions stored IsOptionsStored;
    { Specifies whether the editor has to be read only editor }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    { Defines visible scrollbars - horizontal, vertical or both }
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    { Specifies how fast the scrolling by timer should be }
    property ScrollSpeed: Cardinal read FScrollSpeed write SetScrollSpeed default cScrollSpeedDef;
    { Specifies the current selection end }
    property SelEnd: TKHexEditorSelection read FSelEnd write SetSelEnd;
    { Specifies the current selection length. SelStart remains unchanged, SelEnd will be
      updated accordingly. To mark a selection, either set both SelStart and SelEnd properties
      or both SelStart and SelLength properties }
    property SelLength: TKHexEditorSelection read GetSelLength write SetSelLength;
    { Specifies the current selection start }
    property SelStart: TKHexEditorSelection read FSelStart write SetSelStart;
    { Returns selected text in many different formats }
    property SelText: TKHexEditorSelText read GetSelText;
    { Specifies the vertical scroll position }
    property TopLine: Integer read FTopLine write SetTopLine;
    { Specifies the maximum number of undo items. Please note this value
      affects the undo item limit, not undo group limit. }
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit default cUndoLimitDef;
    { When assigned, this event will be invoked at each buffer change, made either
      by the user or programmatically by public functions }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { When assigned, this event will be invoked when the user drops any files onto
      the window }
    property OnDropFiles: TKEditDropFilesEvent read FOnDropFiles write FOnDropFiles;
    { When assigned, this event will be invoked at each prompt-forced search match }
    property OnReplaceText: TKEditReplaceTextEvent read FOnReplaceText write FOnReplaceText;
  end;

  { @abstract(Hexadecimal editor design-time component) }
  TKHexEditor = class(TKCustomHexEditor)
  published
    { See TKCustomHexEditor.@link(TKCustomHexEditor.AddressCursor) for details }
    property AddressCursor;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.AddressMode) for details }
    property AddressMode;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.AddressOffset) for details }
    property AddressOffset;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.AddressPrefix) for details }
    property AddressPrefix;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.AddressSize) for details }
    property AddressSize;
    { Inherited property - see Delphi help }
    property Align;
    { Inherited property - see Delphi help }
    property Anchors;
    { See TKCustomControl.@link(TKCustomControl.BorderStyle) for details }
    property BorderStyle;
    { Inherited property - see Delphi help }
    property BorderWidth;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.CharSpacing) for details }
    property CharSpacing;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.Colors) for details }
    property Colors;
    { Inherited property - see Delphi help }
    property Constraints;
  {$IFNDEF FPC}
    { Inherited property - see Delphi help. }
    property Ctl3D;
  {$ENDIF}
    { See TKCustomHexEditor.@link(TKCustomHexEditor.DigitGrouping) for details }
    property DigitGrouping;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.DisabledDrawStyle) for details }
    property DisabledDrawStyle;
    { Inherited property - see Delphi help }
    property DragCursor;
    { Inherited property - see Delphi help }
    property DragKind;
    { Inherited property - see Delphi help }
    property DragMode;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.DrawStyles) for details }
    property DrawStyles;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.EditArea) for details }
    property EditArea;
    { Inherited property - see Delphi help }
    property Enabled;
    { Inherited property - see Delphi help. Font pitch must always remain fpFixed
      - specify fixed fonts only. Font.Size will also be trimmed if too small or big }
    property Font;
    { Inherited property - see Delphi help }
    property Height default cHeight;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.LineHeightPercent) for details }
    property LineHeightPercent;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.LineSize) for details }
    property LineSize;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.Options) for details }
    property Options;
    { Inherited property - see Delphi help }
    property ParentShowHint;
    { Inherited property - see Delphi help }
    property PopupMenu;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.ReadOnly) for details }
    property ReadOnly;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.ScrollBars) for details }
    property ScrollBars;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.ScrollSpeed) for details }
    property ScrollSpeed;
    { Inherited property - see Delphi help }
    property ShowHint;
    { Inherited property - see Delphi help }
    property TabOrder;
    { Inherited property - see Delphi help }
    property TabStop default True;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.UndoLimit) for details }
    property UndoLimit;
    { Inherited property - see Delphi help }
    property Visible;
    { Inherited property - see Delphi help }
    property Width default cWidth;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.OnChange) for details }
    property OnChange;
    { Inherited property - see Delphi help }
    property OnClick;
    { Inherited property - see Delphi help }
    property OnContextPopup;
    { Inherited property - see Delphi help }
    property OnDblClick;
    { Inherited property - see Delphi help }
    property OnDockDrop;
    { Inherited property - see Delphi help }
    property OnDockOver;
    { Inherited property - see Delphi help }
    property OnDragDrop;
    { Inherited property - see Delphi help }
    property OnDragOver;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.OnDropFiles) for details }
    property OnDropFiles;
    { Inherited property - see Delphi help }
    property OnEndDock;
    { Inherited property - see Delphi help }
    property OnEndDrag;
    { Inherited property - see Delphi help }
    property OnEnter;
    { Inherited property - see Delphi help }
    property OnExit;
    { Inherited property - see Delphi help }
    property OnGetSiteInfo;
    { Inherited property - see Delphi help }
    property OnKeyDown;
    { Inherited property - see Delphi help }
    property OnKeyPress;
    { Inherited property - see Delphi help }
    property OnKeyUp;
    { Inherited property - see Delphi help }
    property OnMouseDown;
    { Inherited property - see Delphi help }
    property OnMouseMove;
    { Inherited property - see Delphi help }
    property OnMouseUp;
    { Inherited property - see Delphi help }
    property OnMouseWheel;
    { Inherited property - see Delphi help }
    property OnMouseWheelDown;
    { Inherited property - see Delphi help }
    property OnMouseWheelUp;
    { See TKCustomControl.@link(TKCustomControl.OnPrintNotify) for details }
    property OnPrintNotify;
    { See TKCustomControl.@link(TKCustomControl.OnPrintPaint) for details }
    property OnPrintPaint;
    { See TKCustomHexEditor.@link(TKCustomHexEditor.OnReplaceText) for details }
    property OnReplaceText;
    { Inherited property - see Delphi help }
    property OnResize;
    { Inherited property - see Delphi help }
    property OnStartDock;
    { Inherited property - see Delphi help }
    property OnStartDrag;
    { Inherited property - see Delphi help }
    property OnUnDock;
  end;

{ Creates a selection structure from given Index and Digit parameters }
function MakeSelection(Index, Digit: Integer): TKHexEditorSelection;

{ Converts a hexadecimal digit character ('0'..'F') to binary value }
function DigitToBin(Value: AnsiChar): Integer;

{ Examines/converts hexadecimal digit string to binary value string. Returns
  True if the digit string is valid.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>S</I> - hexadecimal digit string (e.g. 'AF01 DC05 3'). White spaces will
  be ignored. When Convert is True, the converted binary value string will be returned
  via this parameter (in this exammple '#A#F#0#1#D#C#0#5#3').</LI>
  <LI><I>Convert</I> - the digit string will be converted if True, otherwise it will
  be examined only.</LI>
  </UL> }
function DigitsToBinStr(var S: AnsiString; Convert: Boolean = True): Boolean;

{ Converts a binary value string into binary data. If the binary value string
  is not divisible by 2, it will be right padded with zero. Example:
  '#A#F#0#1#D#C#0#5#3' is converted into '#AF#01#DC#05#30'. }
function BinStrToBinary(const S: AnsiString): AnsiString;

{ Converts binary data into hexadecimal digit string.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Buffer</I> - binary data - intended for @link(TKCustomHexEditor.Buffer)</LI>
  <LI><I>SelStart, SelEnd</I> - specifies which part of the buffer is about to be
  converted. SelStart.Index must be lower or equal to SelEnd.Index - intended for
  @link(TKCustomHexEditor.GetRealSelStart) and @link(TKCustomHexEditor.GetRealSelEnd).</LI>
  </UL> }
function BinaryToDigits(Buffer: PBytes; SelStart, SelEnd: TKHexEditorSelection): AnsiString;

{ Converts binary data into text using given character mapping.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Buffer</I> - binary data - intended for @link(TKCustomHexEditor.Buffer)</LI>
  <LI><I>SelStart, SelEnd</I> - specifies which part of the buffer is about to be
  converted. SelStart must be lower or equal to SelEnd. These parameters are integers
  since no digit selections are necessary.</LI>
  <LI><I>CharMapping</I> - required character mapping scheme</LI>
  </UL> }
function BinaryToText(Buffer: PBytes; SelStart, SelEnd: Integer;
  CharMapping: PKEditCharMapping): AnsiString;

{ Replaces a hexadecimal digit in the given binary value. Returns the original
  value with a replaced digit.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Value</I> - original binary value</LI>
  <LI><I>Digit</I> - digit value (0..15)</LI>
  <LI><I>Pos</I> - digit position (order)</LI>
  </UL>
  Example: Value = $A18D, Digit = $C, Pos = 3: Result = $AC8D }
function ReplaceDigit(Value, Digit, Pos: Integer): Integer;

{ Returns the instance-independent color specification for
  the given color index }
function GetColorSpec(Index: TKHexEditorColorIndex): TKHexEditorColorSpec;

implementation

uses
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  Math,
{$IFDEF USE_WINAPI}
  ShellApi,
{$ENDIF}
  ClipBrd, Printers,
  Types, KGraphics;

const
  cFmtText = '%.2x';
  cBase = 16;
  cDigitCount = 2;

function MakeSelection(Index, Digit: Integer): TKHexEditorSelection;
begin
  Result.Index := Index;
  Result.Digit := Digit;
end;

function DigitToBin(Value: AnsiChar): Integer;
begin
  if ((Value >= 'a') and (Value <= 'f')) then Result := Ord(Value) - Ord('a') + 10
  else if ((Value >= 'A') and (Value <= 'F')) then Result := Ord(Value) - Ord('A') + 10
  else if ((Value >= '0') and (Value <= '9')) then Result := Ord(Value) - Ord('0')
  else Result := -1;
end;

function DigitsToBinStr(var S: AnsiString; Convert: Boolean = True): Boolean;
var
  I, J, K: Integer;
  T: AnsiString;
begin
  // check and convert text characters to hex values 0..15
  Result := True;
  if Convert then
    SetLength(T, Length(S));
  J := 0;
  for I := 1 to Length(S) do if not CharInSetEx(S[I], [#9, #32]) then
  begin
    K := DigitToBin(S[I]);
    if K >= 0 then
    begin
      if Convert then
      begin
        Inc(J);
        T[J] := AnsiChar(K)
      end;
    end else
    begin
      Result := False;
      Break;
    end;
  end;
  if Result and Convert then
  begin
    SetLength(T, J);
    S := T;
  end;
end;

function BinStrToBinary(const S: AnsiString): AnsiString;
var
  I, J, L: Integer;
  B1, B2: Byte;
begin
  L := Length(S);
  Result := '';
  if L > 0 then
  begin
    SetLength(Result, DivUp(L, 2));
    if L = 1 then
      Result := S
    else
    begin
      J := 1;
      for I := 1 to Length(Result) do
      begin
        B1 := Byte(S[J]); Inc(J);
        if J <= L then
        begin
          B2 := Byte(S[J]); Inc(J);
        end else
          B2 := 0;
        Result[I] := AnsiChar(B1 shl 4 + B2);
      end;
    end;
  end;
end;

function BinaryToDigits(Buffer: PBytes; SelStart, SelEnd: TKHexEditorSelection): AnsiString;
var
  I, J: Integer;
  S: AnsiString;
begin
  Result := '';
  S := '%s' + cFmtText;
  for I := SelStart.Index to SelEnd.Index do
  begin
    Result := AnsiString(Format(string(S), [Result, Buffer[I]]));
    if I = SelStart.Index then
    begin
      for J := 0 to SelStart.Digit - 1 do
        Delete(Result, 1, 1);
    end;
    if I = SelEnd.Index then
    begin
      for J := SelEnd.Digit to cDigitCount - 1 do
        Delete(Result, Length(Result), 1);
    end;
  end;
end;

function BinaryToText(Buffer: PBytes; SelStart, SelEnd: Integer;
  CharMapping: PKEditCharMapping): AnsiString;
var
  I: Integer;
begin
  if SelEnd > SelStart then
  begin
    SetLength(Result, SelEnd - SelStart);
    System.Move(Buffer[SelStart], Result[1], SelEnd - SelStart);
    if CharMapping <> nil then
      for I := 1 to Length(Result) do
        Result[I] := CharMapping^[Byte(Result[I])];
  end else
    Result := '';
end;

function ReplaceDigit(Value, Digit, Pos: Integer): Integer;
var
  I, Mask, O: Integer;
begin
  O := 1;
  for I := Pos to cDigitCount - 2 do
    O := O * cBase;
  Mask := cBase - 1;
  Result := (((Value div O) and not Mask) + (Digit and Mask)) * O + Value mod O;
end;

function OppositeReason(ItemReason: TKHexEditorChangeReason): TKHexEditorChangeReason;
begin
  case ItemReason of
    crDeleteChar: Result := crInsertChar;
    crDeleteDigits: Result := crInsertDigits;
    crDeleteString: Result := crInsertString;
    crInsertChar: Result := crDeleteChar;
    crInsertDigits: Result := crDeleteDigits;
    crInsertString: Result := crDeleteString;
  else
    Result := ItemReason;
  end;
end;

{ TKHexEditorColors }

constructor TKHexEditorColors.Create(AOwner: TKCustomHexEditor);
begin
  FOwner := AOwner;
  Initialize;
  ClearBrightColors;
end;

procedure TKHexEditorColors.Assign(Source: TPersistent);
begin
  if Source is TKHexEditorColors then
  begin
    Colors := TKHexEditorColors(Source).Colors;
    FOwner.Invalidate;
  end
  else
    inherited;
end;

procedure TKHexEditorColors.ClearBrightColors;
var
  I: TKHexEditorColorIndex;
begin
  for I := 0 to Length(FBrightColors) - 1 do
    FBrightColors[I] := clNone;
end;

function TKHexEditorColors.GetColor(Index: TKHexEditorColorIndex): TColor;
const
  AreaBkGndSet = [ciAddressBkgnd, ciDigitBkGnd, ciTextBkGnd];
  BkGndSet = [ciAddressBkgnd, ciBkGnd, ciDigitBkGnd, ciInactiveCaretBkGnd,
    ciInactiveCaretSelBkGnd, ciSelBkGnd, ciSelBkGndFocused, ciTextBkgnd];
begin
  case FColorScheme of
    ecsGrayed: if Index in BkGndSet then Result := clWindow else Result := clGrayText;
    ecsBright:
    begin
      if FBrightColors[Index] = clNone then
        FBrightColors[Index] := BrightColor(FColors[Index], 0.5, bsOfTop);
      if FSingleBkGnd and (Index in AreaBkGndSet) then
        Result := FBrightColors[ciBkGnd]
      else
        Result := FBrightColors[Index];
    end;
    ecsGrayScale: Result := ColorToGrayScale(FColors[Index]);
  else
    if FSingleBkGnd and (Index in AreaBkGndSet) then
      Result := FColors[ciBkGnd]
    else
      Result := FColors[Index];
  end;
end;

function TKHexEditorColors.GetColorData(Index: TKHexEditorColorIndex): TKHexEditorColorData;
var
  ColorSpec: TKHexEditorColorSpec;
begin
  Result.Index := Index;
  Result.Color := FColors[Index];
  ColorSpec := GetColorSpec(Index);
  Result.Default := ColorSpec.Def;
  Result.Name := ColorSpec.Name;
end;

function TKHexEditorColors.GetColorEx(Index: TKHexEditorColorIndex): TColor;
begin
  Result := FColors[Index];
end;

function TKHexEditorColors.GetColorName(Index: TKHexEditorColorIndex): string;
begin
  Result := GetColorSpec(Index).Name;
end;

procedure TKHexEditorColors.Initialize;
var
  I: TKHexEditorColorIndex;
begin
  SetLength(FColors, ciHexEditorColorsMax + 1);
  SetLength(FBrightColors, ciHexEditorColorsMax + 1);
  for I := 0 to Length(FColors) - 1 do
    FColors[I] := GetColorSpec(I).Def;
end;

procedure TKHexEditorColors.SetColor(Index: TKHexEditorColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    FBrightColors[Index] := clNone;
    if not (csLoading in FOwner.ComponentState) and FOwner.HandleAllocated then
      FOwner.Invalidate;
  end;
end;

procedure TKHexEditorColors.SetColorEx(Index: TKHexEditorColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    FBrightColors[Index] := clNone;
  end;
end;

procedure TKHexEditorColors.SetColors(const Value: TKColorArray);
begin
  FColors := Value;
  ClearBrightColors;
end;

{ TKHexEditorChangeList }

constructor TKHexEditorChangeList.Create(AEditor: TKCustomHexEditor;
  RedoList: TKHexEditorChangeList);
begin
  inherited Create;
  FEditor := AEditor;
  FGroupUseLock := 0;
  FLimit := cUndoLimitDef;
  FIndex := -1;
  FModifiedIndex := FIndex;
  FRedoList := RedoList;
  FOnChange := nil;
end;

procedure TKHexEditorChangeList.AddChange(ItemReason: TKHexEditorChangeReason;
  const Data: AnsiString; Inserted: Boolean);
var
  P: PKHexEditorChangeItem;
begin
  // don't allow succesive crCaretPos
  if (ItemReason = crCaretPos) and not Inserted and (FIndex >= 0) and
    (PKHexEditorChangeItem(Items[FIndex]).ItemReason = crCaretPos) then
    Exit;
  if FIndex < FLimit - 1 then
  begin
    if FIndex < Count - 1 then
      Inc(FIndex)
    else
      FIndex := Add(New(PKHexEditorChangeItem));
    P := Items[FIndex];
    if FGroupUseLock > 0 then
    begin
      P.Group := FGroup;
      P.GroupReason := FGroupReason;
    end else
    begin
      P.Group := 0;
      P.GroupReason := ItemReason;
    end;
    P.ItemReason := ItemReason;
    P.EditArea := FEditor.EditArea;
    P.SelEnd := FEditor.SelEnd;
    P.SelStart := FEditor.SelStart;
    P.Data := Data;
    P.Inserted := Inserted;
    if FRedoList <> nil then
      FRedoList.Clear;
    if Assigned(FOnChange) then
      FOnChange(Self, ItemReason);
  end;
end;

procedure TKHexEditorChangeList.BeginGroup(GroupReason: TKHexEditorChangeReason);
begin
  if FGroupUseLock = 0 then
  begin
    FGroupReason := GroupReason;
    Inc(FGroup);
    if FGroup = 0 then Inc(FGroup);
  end;
  Inc(FGroupUseLock);
end;

function TKHexEditorChangeList.CanPeek: Boolean;
begin
  Result := FIndex >= 0;
end;

procedure TKHexEditorChangeList.Clear;
begin
  inherited;
  FGroupUseLock := 0;
  FIndex := -1;
  FModifiedIndex := FIndex;
end;

procedure TKHexEditorChangeList.EndGroup;
begin
  if FGroupUseLock > 0 then
    Dec(FGroupUseLock);
end;

function TKHexEditorChangeList.GetModified: Boolean;

  function CaretPosOnly: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := FModifiedIndex + 1 to FIndex do
    begin
      if PKHexEditorChangeItem(Items[I]).ItemReason <> crCaretPos then
      begin
        Result := False;
        Exit;
      end;  
    end;
  end;

begin
  Result := (FIndex > FModifiedIndex) and not CaretPosOnly;
end;

procedure TKHexEditorChangeList.Notify(Ptr: Pointer; Action: TListNotification);
var
  P: PKHexEditorChangeItem;
begin
  case Action of
    lnDeleted:
      if Ptr <> nil then
      begin
        P := Ptr;
        Dispose(P);
      end;
  end;
end;

function TKHexEditorChangeList.PeekItem: PKHexEditorChangeItem;
begin
  if CanPeek then
  begin
    Result := Items[FIndex];
    Dec(FIndex);
  end else
    Result := nil;
end;

procedure TKHexEditorChangeList.PokeItem;
begin
  if FIndex < Count - 1 then
    Inc(FIndex);
end;

procedure TKHexEditorChangeList.SetGroupData(Group: Integer;
  GroupReason: TKHexEditorChangeReason);
begin
  FGroup := Group;
  FGroupReason := GroupReason;
  FGroupUseLock := 1;
end;

procedure TKHexEditorChangeList.SetLimit(Value: Integer);
begin
  if Value <> FLimit then
  begin
    FLimit := MinMax(Value, cUndoLimitMin, cUndoLimitMax);
    while Count > FLimit do
      Delete(0);
    FIndex := Min(FIndex, FLimit - 1);
  end;
end;

procedure TKHexEditorChangeList.SetModified(Value: Boolean);
begin
  if not Value then
    FModifiedIndex := FIndex;
end;

{ TKCustomHexEditor }

constructor TKCustomHexEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks, csCaptureMouse];
  Font.Name := cFontNameDef;
  Font.Style := cFontStyleDef;
  Font.Size := cFontSizeDef;
  Font.Pitch := fpFixed;
  Font.OnChange := FontChange;
  Height := cHeight;
  ParentColor := False;
  ParentFont := False;
  TabStop := True;
  Width := cWidth;
  FAddressCursor := cAddressCursorDef;
  FAddressMode := cAddressModeDef;
  FAddressOffset := cAddressOffsetDef;
  FAddressPrefix := cAddressPrefixDef;
  FAddressSize := cAddressSizeDef;
  FAreaSpacing := cAreaSpacingDef;
  FBuffer := nil;
{$IFNDEF FPC}
  FClipBoardFormat := RegisterClipboardFormat('Any binary data');
{$ENDIF}
  FColors := TKHexEditorColors.Create(Self);
  FCharHeight := 8;
  FCharMapping := DefaultCharMapping;
  FCharSpacing := cCharSpacingDef;
  FCharWidth := 6;
  FDigitGrouping := cDigitGroupingDef;
  FDisabledDrawStyle := cDisabledDrawStyleDef;
  FDrawStyles := cDrawStylesDef;
  FEditArea := eaDigits;
  FLeftChar := 0;
  FLineHeightPercent := cLineHeightPercentDef;
  FLineSize := cLineSizeDef;
  FMouseWheelAccumulator := 0;
  FOptions := [eoGroupUndo];
  FKeyMapping := CreateDefaultKeyMapping;
  FRedoList := TKHexEditorChangeList.Create(Self, nil);
  FScrollBars := ssBoth;
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FSelStart := MakeSelection(0, 0);
  FSelEnd := MakeSelection(0, 0);
  FStates := [];
  FTopLine := 0;
  FTotalCharSpacing := 0;
  FUndoList := TKHexEditorChangeList.Create(Self, FRedoList);
  FUndoList.OnChange := UndoChange;
  FOnChange := nil;
  FOnReplaceText := nil;
  UpdateCharMetrics;
end;

destructor TKCustomHexEditor.Destroy;
begin
  inherited;
  FOnChange := nil;
  FColors.Free;
  FUndoList.Free;
  FRedoList.Free;
  FreeMem(FBuffer);
  FBuffer := nil;
end;

procedure TKCustomHexEditor.AddUndoCaretPos(Force: Boolean);
begin
  FUndoList.AddChange(crCaretPos, '', Force);
end;

procedure TKCustomHexEditor.AddUndoByte(ItemReason: TKHexEditorChangeReason; Data: Byte;
  Inserted: Boolean = True);
begin
  FUndoList.AddChange(ItemReason, AnsiChar(Data), Inserted);
end;

procedure TKCustomHexEditor.AddUndoBytes(ItemReason: TKHexEditorChangeReason;
  Data: PBytes; Length: Integer; Inserted: Boolean = True);
var
  S: AnsiString;
begin
  if Length > 0 then
  begin
    SetLength(S, Length);
    Move(Data^, S[1], Length);
    FUndoList.AddChange(ItemReason, S, Inserted);
  end;
end;

procedure TKCustomHexEditor.AddUndoString(ItemReason: TKHexEditorChangeReason;
  const S: AnsiString; Inserted: Boolean = True);
begin
  if S <> '' then
    FUndoList.AddChange(ItemReason, S, Inserted);
end;

procedure TKCustomHexEditor.Append(At: Integer; Data: TDataSize);
var
  S: AnsiString;
begin
  if (Data.Size > 0) and (Data.Data <> nil) then
  begin
    SetString(S, PAnsiChar(Data.Data), Data.Size);
    InsertString(At, S, Data.Size);
  end;
end;

procedure TKCustomHexEditor.Append(At: Integer; const Data: AnsiString);
begin
  InsertString(At, Data, Length(Data));
end;

procedure TKCustomHexEditor.Assign(Source: TPersistent);
begin
  if Source is TKCustomHexEditor then with Source as TKCustomHexEditor do
  begin
    Self.AddressCursor := AddressCursor;
    Self.AddressMode := AddressMode;
    Self.AddressPrefix := AddressPrefix;
    Self.AddressSize := AddressSize;
    Self.Align := Align;
    Self.Anchors := Anchors;
    Self.AutoSize := AutoSize;
    Self.BiDiMode := BiDiMode;
    Self.BorderStyle := BorderStyle;
    Self.BorderWidth := BorderWidth;
    Self.CharSpacing := CharSpacing;
    Self.Color := Color;
    Self.Colors := Colors;
    Self.Constraints.Assign(Constraints);
  {$IFNDEF FPC}
    Self.Ctl3D := Ctl3D;
  {$ENDIF}
    Self.Data := Data;
    Self.DigitGrouping := DigitGrouping;
    Self.DisabledDrawStyle := DisabledDrawStyle;
    Self.DragCursor := DragCursor;
    Self.DragKind := DragKind;
    Self.DragMode := DragMode;
    Self.DrawStyles := DrawStyles;
    Self.EditArea := EditArea;
    Self.Enabled := Enabled;
    Self.Font := Font;
  {$IFNDEF FPC}
    Self.ImeMode := ImeMode;
    Self.ImeName := ImeName;
  {$ENDIF}
    Self.LineHeightPercent := LineHeightPercent;
    Self.LineSize := LineSize;
    Self.Modified := False;
    Self.Options := Options;
    Self.ParentBiDiMode := ParentBiDiMode;
    Self.ParentColor := ParentColor;
  {$IFNDEF FPC}
    Self.ParentCtl3D := ParentCtl3D;
  {$ENDIF}
    Self.ParentFont := ParentFont;
    Self.ParentShowHint := ParentShowHint;
    Self.PopupMenu := PopupMenu;
    Self.ScrollBars := ScrollBars;
    Self.SelEnd := SelEnd;
    Self.SelStart := SelStart;
    Self.SetCharMapping(GetCharMapping);
    Self.SetKeyMapping(GetKeyMapping);
    Self.ShowHint := ShowHint;
    Self.TabOrder := TabOrder;
    Self.TabStop := TabStop;
    Self.Visible := Visible;
  end
  else
    inherited;
end;

procedure TKCustomHexEditor.BeginUndoGroup(GroupReason: TKHexEditorChangeReason);
begin
  FUndoList.BeginGroup(GroupReason);
end;

procedure TKCustomHexEditor.BufferChanged;
begin
  FUndoList.Clear;
  FRedoList.Clear;
  UpdateScrollRange;
  SelectionChanged(False);
  DoChange;
end;

function TKCustomHexEditor.CanScroll(Command: TKEditCommand): Boolean;
var
  XMax, YMax: Integer;
  P: TPoint;
  AD: TKHExEditorAreaDimensions;
begin
  AD := GetAreaDimensions;
  XMax := GetMaxLeftChar(AD.TotalHorz);
  YMax := GetMaxTopLine(AD.TotalVert);
  case Command of
    ecScrollUp:  Result := FTopLine > 0;
    ecScrollDown: Result := FTopLine < YMax;
    ecScrollLeft: Result := FLeftChar > 0;
    ecScrollRight: Result := FLeftChar < XMax;
    ecScrollCenter:
    begin
      P := SelToPoint(FSelEnd, FEditArea);
      P.X := P.X - ClientWidth div 2;
      P.Y := P.Y - ClientHeight div 2;
      Result := (FLeftChar > 0) and (P.X < 0) or (FLeftChar < XMax) and (P.X > FCharWidth) or
        (FTopLine > 0) and (P.Y < 0) or (FTopLine < YMax) and (P.Y > FCharHeight);
    end;
  else
    Result := False;
  end;
end;

function TKCustomHexEditor.CaretInView: Boolean;
begin
  Result := PtInRect(GetModifiedClientRect, SelToPoint(FSelEnd, FEditArea));
end;

procedure TKCustomHexEditor.Clear;
begin
  if FBuffer <> nil then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
    FSize := 0;
    BufferChanged;
  end;
end;

procedure TKCustomHexEditor.ClearChar(At: Integer);
begin
  ClearString(At, 1);
end;

procedure TKCustomHexEditor.ClearDigitSelection;
begin
  FSelStart.Digit := 0;
  FSelEnd.Digit := 0;
end;

procedure TKCustomHexEditor.ClearString(At, Size: Integer);
begin
  if (FBuffer <> nil) and (Size > 0) and (At >= 0) and (At + Size <= FSize) then
  begin
    Move(FBuffer[At + Size], FBuffer[At], (FSize - At - Size) * SizeOf(Byte));
    Dec(FSize, Size);
    ReallocMem(FBuffer, FSize);
    UpdateScrollRange;
    Invalidate;
  end;
end;

procedure TKCustomHexEditor.ClearUndo;
begin
  FUndoList.Clear;
  FRedoList.Clear;
end;

procedure TKCustomHexEditor.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  UpdateEditorCaret;
  Invalidate;
end;

procedure TKCustomHexEditor.CMSysColorChange(var Msg: TLMessage);
begin
  inherited;
  FColors.ClearBrightColors;
end;

function TKCustomHexEditor.CommandEnabled(Command: TKEditCommand): Boolean;
var
  L: TKHexEditorSelection;
begin
  if Enabled and Visible and not (csDesigning in ComponentState) then
  begin
    L := SelLength;
    case Command of
      // movement commands
      ecLeft, ecSelLeft: Result := (FSelEnd.Index > 0) or (FEditArea = eaDigits) and (FSelEnd.Digit > 0);
      ecRight, ecSelRight: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize);
      ecUp, ecSelUp: Result := FSelEnd.Index >= FLineSize;
      ecDown, ecSelDown: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize);
      ecLineStart, ecSelLineStart: Result := (FEditArea <> eaNone) and (FSelEnd.Index mod FLineSize > 0);
      ecLineEnd, ecSelLineEnd: Result := (FEditArea <> eaNone) and (FSelEnd.Index mod FLineSize < Min(FLineSize - 1, FSize));
      ecPageUp, ecSelPageUp: Result := FSelEnd.Index >= FlineSize;
      ecPageDown, ecSelPageDown: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize div FLineSize * FLineSize);
      ecPageLeft, ecSelPageLeft: Result := (FEditArea <> eaNone) and (GetPageHorz > 0) and (FSelEnd.Index mod FLineSize > 0);
      ecPageRight, ecSelPageRight: Result := (FEditArea <> eaNone) and (GetPageHorz > 0) and (FSelEnd.Index mod FLineSize < Min(FLineSize - 1, FSize));
      ecPageTop, ecSelPageTop: Result := (FEditArea <> eaNone) and (FSelEnd.Index > 0) and (SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea).Y div FCharHeight <> 0);
      ecPageBottom, ecSelPageBottom: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize) and ((ClientHeight - SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea).Y) div FCharHeight - 1 <> 0);
      ecEditorTop, ecSelEditorTop: Result := FSelEnd.Index > 0;
      ecEditorBottom, ecSelEditorBottom: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize);
      ecGotoXY, ecSelGotoXY: Result := True;
      // scroll commands
      ecScrollUp, ecScrollDown, ecScrollLeft, ecScrollRight, ecScrollCenter: Result := CanScroll(Command);
      // editing commands
      ecUndo: Result := not ReadOnly and FUndoList.CanPeek;
      ecRedo: Result := not ReadOnly and FRedoList.CanPeek;
      ecCopy, ecCut: Result := not Empty and (not ReadOnly or (Command = ecCopy)) and ((L.Index <> 0) or (L.Digit <> 0));
      ecPaste: Result := not ReadOnly and (FEditArea <> eaNone) and (ClipBoard.FormatCount > 0);
      ecInsertChar: Result := not ReadOnly and (FEditArea <> eaNone);
      ecInsertDigits: Result := not ReadOnly and (FEditArea = eaDigits);
      ecInsertString: Result := not ReadOnly and (FEditArea <> eaNone);
      ecDeleteLastChar: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index > 0));
      ecDeleteChar: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index < FSize));
      ecDeleteBOL: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index mod FLineSize > 0));
      ecDeleteEOL: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index mod FLineSize < Min(FLineSize, FSize)));
      ecDeleteLine: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index mod FLineSize > 0) or (FSelEnd.Index < FSize));
      ecSelectAll: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone);
      ecClearAll: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone);
      ecClearIndexSelection, ecClearSelection: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and (L.Index > 0);
      ecSearch: Result := not Empty;
      ecReplace: Result := not (Empty or ReadOnly);
      ecInsertMode: Result := elOverwrite in FStates;
      ecOverwriteMode: Result := not (elOverwrite in FStates);
    else
      Result := True;
    end;
  end else
    Result := False;
end;

procedure TKCustomHexEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
  end;
end;

procedure TKCustomHexEditor.CreateWnd;
begin
  inherited;
{$IFDEF USE_WINAPI}
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
{$ENDIF}
end;

procedure TKCustomHexEditor.DestroyWnd;
begin
{$IFDEF USE_WINAPI}
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, FALSE);
{$ENDIF}
  inherited;
end;

procedure TKCustomHexEditor.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TKCustomHexEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120;
var
  LinesToScroll, WheelClicks: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    if ssCtrl in Shift then
      LinesToScroll := GetModifiedClientRect.Bottom div FCharHeight
    else
      LinesToScroll := 3;
    Inc(FMouseWheelAccumulator, WheelDelta);
    WheelClicks := FMouseWheelAccumulator div WHEEL_DIVISOR;
    FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DIVISOR;
    ScrollBy(0, - WheelClicks * LinesToScroll, True);
    Result := True;
  end;
end;

procedure TKCustomHexEditor.EditAreaChanged;
begin
  if FEditArea = eaNone then
    FEditArea := eaDigits;
  if not (edAddress in FDrawStyles) and (FEditArea = eaAddress) then
      FEditArea := eaDigits;
  if not (edDigits in FDrawStyles) and (FEditArea = eaDigits) then
      FEditArea := eaText;
  if not (edText in FDrawStyles) and (FEditArea = eaText) then
    if edDigits in FDrawStyles then
      FEditArea := eaDigits
    else
      FEditArea := eaNone;
end;

procedure TKCustomHexEditor.EndUndoGroup;
begin
  FUndoList.EndGroup;
end;

function TKCustomHexEditor.ExecuteCommand(Command: TKEditCommand;
  Data: Pointer): Boolean;
var
  I, J, K, M, N, O: Integer;
  CanInsert, MoreBytes, Found, MatchCase: Boolean;
  C1, C2, C3: AnsiChar;
  S, S_FirstChar, S_LastChar, T: AnsiString;
  P: TPoint;
  Area: TKHexEditorArea;
  L, OldSelStart, OldSelEnd, Sel1, Sel2: TKHexEditorSelection;
  PChI, PChI_First, PChI_Next: PKHexEditorChangeItem;
  PSD: PKEditSearchData;
  ReplaceAction: TKEditReplaceAction;
{$IFNDEF FPC}
  BA: PBytes;
  H: THandle;
{$ENDIF}
begin
  Result := False;
  if CommandEnabled(Command) then
  begin
    Result := True;
    L := SelLength;
    OldSelEnd := FSelEnd;
    OldSelStart := FSelStart;
    case Command of
      ecLeft..ecSelGotoXY: AddUndoCaretPos(False);
    end;  
    case Command of
      ecLeft, ecSelLeft:
      begin
        InternalMoveLeft;
        SelectionChanged(Command <> ecSelLeft);
      end;
      ecRight, ecSelRight:
      begin
        InternalMoveRight;
        SelectionChanged(Command <> ecSelRight);
      end;
      ecUp, ecSelUp:
      begin
        Dec(FSelEnd.Index, FLineSize);
        SelectionChanged(Command <> ecSelUp);
      end;
      ecDown, ecSelDown:
      begin
        Inc(FSelEnd.Index, FLineSize);
        SelectionChanged(Command <> ecSelDown);
      end;
      ecLineStart, ecSelLineStart:
      begin
        FSelEnd := MakeSelection((FSelEnd.Index div FLineSize) * FLineSize, 0);
        SelectionChanged(Command <> ecSelLineStart);
      end;
      ecLineEnd, ecSelLineEnd:
      begin
        FSelEnd := MakeSelection((FSelEnd.Index div FLineSize) * FLineSize + FLineSize - 1, cDigitCount - 1);
        SelectionChanged(Command <> ecSelLineEnd);
      end;
      ecPageUp, ecSelPageUp:
      begin
        Dec(FSelEnd.Index, Min(ClientHeight div FCharHeight, FSelEnd.Index div FLineSize) * FLineSize);
        SelectionChanged(Command <> ecSelPageUp);
      end;
      ecPageDown, ecSelPageDown:
      begin
        Inc(FSelEnd.Index, Min(ClientHeight div FCharHeight, (FSize - FSelEnd.Index) div FLineSize) * FLineSize);
        SelectionChanged(Command <> ecSelPageDown);
      end;
      ecPageLeft, ecSelPageLeft:
      begin
        Dec(FSelEnd.Index, Min(GetPageHorz, FSelEnd.Index mod FLineSize));
        SelectionChanged(Command <> ecSelPageLeft);
      end;
      ecPageRight, ecSelPageRight:
      begin
        Inc(FSelEnd.Index, Min(GetPageHorz, FLineSize - 1 - FSelEnd.Index mod FLineSize));
        SelectionChanged(Command <> ecSelPageRight);
      end;
      ecPageTop, ecSelPageTop:
      begin
        P := SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea);
        Dec(FSelEnd.Index, P.Y div FCharHeight * FLineSize);
        SelectionChanged(Command <> ecSelPageTop);
      end;
      ecPageBottom, ecSelPageBottom:
      begin
        P := SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea);
        Inc(FSelEnd.Index, ((ClientHeight - P.Y) div FCharHeight - 1) * FLineSize);
        SelectionChanged(Command <> ecSelPageBottom);
      end;
      ecEditorTop, ecSelEditorTop:
      begin
        FSelEnd := MakeSelection(0, 0);
        SelectionChanged(Command <> ecSelEditorTop);
      end;
      ecEditorBottom, ecSelEditorBottom:
      begin
        FSelEnd := MakeSelection(FSize, 0);
        SelectionChanged(Command <> ecSelEditorBottom);
      end;
      ecGotoXY, ecSelGotoXY:
      begin
        Sel1 := PointToSel(PPoint(Data)^, False, Area);
        if Area <> eaNone then
        begin
          FSelEnd := Sel1;
          FEditArea := Area;
          SelectionChanged(Command <> ecSelGotoXY);
        end else
          Result := False;
      end;
      // scroll commands
      ecScrollUp:
      begin
        if (FEditArea <> eaNone) and (SelToPoint(FSelEnd, FEditArea).Y >= GetModifiedClientRect.Bottom - FCharHeight) then
        begin
          ScrollBy(0, -1, False);
          Dec(FSelEnd.Index, FLineSize);
          SelectionChanged(True, False);
          Invalidate;
        end else
          ScrollBy(0, -1, True);
      end;
      ecScrollDown:
      begin
        if (FEditArea <> eaNone) and (SelToPoint(FSelEnd, FEditArea).Y <= GetModifiedClientRect.Top) then
        begin
          ScrollBy(0, 1, False);
          Inc(FSelEnd.Index, FLineSize);
          SelectionChanged(True, False);
          Invalidate;
        end else
          ScrollBy(0, 1, True);
      end;
      ecScrollLeft:
      begin
        if FEditArea <> eaNone then
        begin
          // overscroll check
          P := SelToPoint(MakeSelection(0, 0), FEditArea);
          if P.X < GetModifiedClientRect.Right - FCharWidth then
          begin
            ScrollBy(-1, 0, True);
            P := SelToPoint(FSelEnd, FEditArea);
            if (P.X >= GetModifiedClientRect.Right) and ((FSelEnd.Index mod FLineSize > 0) or (FSelEnd.Digit > 0)) then
              ExecuteCommand(ecLeft)
          end;
        end else
          ScrollBy(-1, 0, True);
      end;
      ecScrollRight:
      begin
        if FEditArea <> eaNone then
        begin
          // overscroll check
          P := SelToPoint(MakeSelection(FLineSize - 1, cDigitCount - 1), FEditArea);
          if P.X > 0 then
          begin
            ScrollBy(1, 0, True);
            P := SelToPoint(FSelEnd, FEditArea);
            if (P.X < 0) and ((FSelEnd.Index mod FLineSize < FLineSize - 1) or (FSelEnd.Digit < cDigitCount - 1)) then
              ExecuteCommand(ecRight)
          end;
        end else
          ScrollBy(1, 0, True);
      end;
      ecScrollCenter:
      begin
        P := SelToPoint(FSelEnd, FEditArea);
        I := (P.X - ClientWidth div 2) div FCharWidth;
        J := (P.Y - ClientHeight div 2) div FCharHeight;
        ScrollBy(I, J, True);
      end;
      // editing commands
      ecUndo:
      begin
        PChI := FUndoList.PeekItem;
        PChI_First := PChI;
        while PChI <> nil do
        begin
          I := Length(PChI.Data);
          J := Min(I, FSize - PChI.SelEnd.Index);
          FRedoList.SetGroupData(PChI.Group, PChI.GroupReason);
          case PChI.ItemReason of
            crCaretPos:
              FRedoList.AddChange(crCaretPos, '');
            crDeleteChar, crDeleteDigits, crDeleteString:
            begin
              if FBuffer <> nil then
              begin
                SetLength(S, J);
                System.Move(FBuffer[PChI.SelEnd.Index], S[1], J);
              end else
                S := '';
              FRedoList.AddChange(OppositeReason(PChI.ItemReason), S, PChI.Inserted);
            end;
            crInsertChar, crInsertDigits, crInsertString:
              FRedoList.AddChange(OppositeReason(PChI.ItemReason), PChI.Data);
          end;
          FSelEnd := PChI.SelEnd;
          FSelStart := PChI.SelStart;
          FEditArea := PChI.EditArea;
          case PChI.ItemReason of
            crDeleteChar, crDeleteDigits, crDeleteString:
            begin
              if PChI.Inserted then
                ClearString(PChI.SelEnd.Index, I)
              else if FBuffer <> nil then
              begin
                System.Move(PChI.Data[1], FBuffer[PChI.SelEnd.Index], J);
                Invalidate;
              end;
            end;
            crInsertChar, crInsertDigits, crInsertString:
              InsertString(GetRealSelStart.Index, PChI.Data, I);
          end;
          EditAreaChanged;
          SelectionChanged(False, False);
          if PChI.ItemReason <> crCaretPos then
            DoChange;
          PChI_Next := FUndoList.PeekItem;
          if (PChI_Next <> nil) and not ((PChI.Group <> 0) and (PChI.Group = PChI_Next.Group) or
            (eoGroupUndo in FOptions) and (PChI_First.GroupReason = PChI_Next.GroupReason)) then
          begin
            FUndoList.PokeItem;
            Break;
          end;
          PChI := PChI_Next;
        end;
        if not CaretInView then
          ExecuteCommand(ecScrollCenter);
      end;
      ecRedo:
      begin
        PChI := FRedoList.PeekItem;
        PChI_First := PChI;
        while PChI <> nil do
        begin
          FUndoList.PokeItem;
          I := Length(PChI.Data);
          Sel1 := GetRealSelStart;
          case PChI.ItemReason of
            crInsertChar, crInsertDigits, crInsertString:
            begin
              if PChI.Inserted then
                InsertString(Sel1.Index, PChI.Data, I)
              else if FBuffer <> nil then
              begin
                System.Move(PChI.Data[1], FBuffer[Sel1.Index], Min(I, FSize - FSelEnd.Index));
                Invalidate;
              end;
            end;
            crDeleteChar, crDeleteDigits, crDeleteString:
              ClearString(Sel1.Index, I);
          end;
          FSelEnd := PChI.SelEnd;
          FSelStart := PChI.SelStart;
          FEditArea := PChI.EditArea;
          EditAreaChanged;
          SelectionChanged(False, False);
          if PChI.ItemReason <> crCaretPos then
            DoChange;
          PChI_Next := FRedoList.PeekItem;
          if (PChI_Next <> nil) and not ((PChI.Group <> 0) and (PChI.Group = PChI_Next.Group) or
            (eoGroupUndo in FOptions) and (PChI_First.GroupReason = PChI_Next.GroupReason)) then
          begin
            FRedoList.PokeItem;
            Break;
          end;
          PChI := PChI_Next;
        end;
        if not CaretInView then
          ExecuteCommand(ecScrollCenter);
      end;
      ecCopy:
      begin
        Sel1 := GetRealSelStart;
        Sel2 := GetRealSelEnd;
      {$IFDEF FPC}
        ClipBoard.AsText := string(BinaryToDigits(FBuffer, Sel1, Sel2))
      {$ELSE}
        if FEditArea = eaDigits then
          ClipBoard.AsText := string(BinaryToDigits(FBuffer, Sel1, Sel2))
        else if L.Index <> 0 then
        begin
          S := BinaryToText(FBuffer, Sel1.Index, Sel2.Index, @FCharMapping);
          H := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, L.Index);
          try
            BA := GlobalLock(H);
            try
              System.Move(FBuffer[Sel1.Index], BA^, L.Index);
            finally
              GlobalUnlock(H);
            end;
            ClipBoard.Open;
            try
              ClipBoard.SetAsHandle(FClipboardFormat, H);
              ClipBoard.AsText := string(S);
            finally
              ClipBoard.Close;
            end;
          except
            GlobalFree(H);
          end;
        end;
      {$ENDIF}
      end;
      ecCut:
      begin
        ExecuteCommand(ecCopy);
        ExecuteCommand(ecClearSelection);
      end;
      ecPaste:
      begin
        if L.Index > 0 then
          ExecuteCommand(ecClearSelection);
        if ClipBoard.FormatCount > 0 then
        begin
          S := '';
        {$IFNDEF FPC}
          H := 0;
          // paste as binary data
          if ClipBoard.HasFormat(FClipboardFormat) then
            H := ClipBoard.GetAsHandle(FClipboardFormat) else
        {$ENDIF}
          if ClipBoard.HasFormat(CF_TEXT) then
          begin
            S := AnsiString(ClipBoard.AsText);
            if S <> '' then
            begin
              M := Length(S);
              if (FEditArea = eaDigits) and ExecuteCommand(ecInsertDigits, Pointer(S)) then
              begin
                S := '';
                if M >= cDigitCount then
                begin
                  Inc(FSelEnd.Index, M div cDigitCount)
                end else
                begin
                  Inc(FSelEnd.Digit, M);
                  if FSelEnd.Digit >= cDigitCount then
                  begin
                    Inc(FSelEnd.Index);
                    FSelEnd.Digit := FSelEnd.Digit mod cDigitCount;
                  end;
                end;
                SelectionChanged(True);
              end else
                ExecuteCommand(ecInsertString, Pointer(S));
            end;
          end
        {$IFNDEF FPC}
          else
            H := ClipBoard.GetAsHandle(ClipBoard.Formats[0]);
          if H <> 0 then
          begin
            BA := GlobalLock(H);
            try
              I := GlobalSize(H);
              if I > 0 then
              begin
                SetLength(S, I);
                System.Move(BA^, S[1], I);
              end;
            finally
              GlobalUnlock(H);
            end;
            if S <> '' then
              ExecuteCommand(ecInsertString, Pointer(S));
          end
        {$ENDIF}
          ;
          if S <> '' then
          begin
            Inc(FSelEnd.Index, Length(S));
            FSelEnd.Digit := 0;
            SelectionChanged(True);
          end;
        end;
      end;
      ecInsertChar:
      begin
        BeginUndoGroup(crInsertChar);
        try
          N := PByte(Data)^;
          if L.Index > 0 then
            ExecuteCommand(ecClearSelection);
          ValidateSelection(FSelEnd, FEditArea);
          if FBuffer <> nil then
            I := FBuffer[FSelEnd.Index]
          else
            I := 0;
          CanInsert := (FBuffer = nil) or (FSelEnd.Digit = 0) and
            (not (elOverwrite in FStates) or (FSelEnd.Index = FSize));
          AddUndoByte(crDeleteChar, I, CanInsert);
          if CanInsert then
            InsertChar(FSelEnd.Index, 0)
          else
            Invalidate;
          case FEditArea of
            eaDigits:
            begin
              FBuffer[FSelEnd.Index] := ReplaceDigit(FBuffer[FSelEnd.Index], N, FSelEnd.Digit);
              InternalMoveRight;
            end;
            eaText:
            begin
              FBuffer[FSelEnd.Index] := N;
              InternalMoveRight;
            end;
          end;
          SelectionChanged(True);
        finally
          EndUndoGroup;
        end;
      end;
      ecInsertDigits:
      begin
        S := AnsiString(Data);
        if (S <> '') and DigitsToBinStr(S) then
        begin
          BeginUndoGroup(crInsertDigits);
          try
            if L.Index > 0 then
              ExecuteCommand(ecClearSelection);
            ValidateSelection(FSelEnd, FEditArea);
            MoreBytes := Length(S) >= cDigitCount;
            if MoreBytes then
              // we don't move digit positions of the remaining block
              SetLength(S, Length(S) div cDigitCount * cDigitCount);
            J := 0;
            if (FBuffer <> nil) and (not MoreBytes or (FSelEnd.Digit > 0)) then
            begin
              I := FBuffer[FSelEnd.Index];
              S_FirstChar := AnsiChar(I);
              S_LastChar := S_FirstChar;
              // split current byte
              AddUndoByte(crInsertChar, I);
              ClearChar(FSelEnd.Index);
              N := Length(S);
              for I := FSelEnd.Digit to cDigitCount - 1 do
              begin
                if J < N then
                begin
                  Inc(J);
                  S_FirstChar := AnsiChar(ReplaceDigit(Ord(S_FirstChar[1]), Ord(S[J]), I));
                end else
                  Break;
              end;
              K := Length(S);
              if K > J then
                for I := FSelEnd.Digit - 1 downto 0 do
                begin
                  if K > J then
                  begin
                    S_LastChar := AnsiChar(ReplaceDigit(Ord(S_LastChar[1]), Ord(S[K]), I));
                    Dec(K);
                  end else
                    Break;
                end
              else
                S_LastChar := '';
              O := cDigitCount;
            end else
            begin
              S_FirstChar := '';
              S_LastChar := '';
              O := 0;
            end;
            T := '';
            if MoreBytes then
            begin
              N := Length(S) - O;
              O := J;
              for I := 0 to N div cDigitCount - 1 do
              begin
                K := 0;
                for J := 1 to cDigitCount do
                begin
                  K := K * cBase;
                  M := I * 2 + J + O;
                  Inc(K, Ord(S[M]));
                end;
                T := AnsiString(Format('%s%s', [T, Char(K)]));
              end;
            end;
            S := S_FirstChar + T + S_LastChar;
            // always insert (don't overwrite)
            AddUndoString(crDeleteDigits, S);
            InsertString(FSelEnd.Index, S, Length(S));
            SelectionChanged(True);
          finally
            EndUndoGroup;
          end;
        end else
          Result := False;
      end;
      ecInsertString:
      begin
        S := AnsiString(Data);
        if S <> '' then
        begin
          BeginUndoGroup(crInsertString);
          try
            if L.Index > 0 then
              ExecuteCommand(ecClearIndexSelection);
            // always insert (don't overwrite)
            AddUndoString(crDeleteString, S);
            InsertString(FSelEnd.Index, S, Length(S));
            SelectionChanged(True);
          finally
            EndUndoGroup;
          end;
        end else
          Result := False;
      end;
      ecDeleteLastChar:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := FSelEnd.Index - 1;
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteChar:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := FSelEnd.Index + 1;
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteBOL:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := (FSelEnd.Index div FLineSize) * FLineSize;
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteEOL:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := Min((FSelEnd.Index div FLineSize + 1) * FLineSize, FSize);
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteLine:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := (FSelEnd.Index div FLineSize) * FLineSize;
            FSelEnd.Index := Min(FSelStart.Index + FLineSize, FSize);
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecSelectAll:
      begin
        AddUndoCaretPos;
        FSelStart := MakeSelection(0, 0);
        FSelEnd := MakeSelection(FSize, 0);
        SelectionChanged(False);
      end;
      ecClearAll:
      begin
        ExecuteCommand(ecSelectAll);
        ExecuteCommand(ecClearIndexSelection);
      end;
      ecClearIndexSelection:
      begin
        I := GetRealSelStart.Index;
        AddUndoBytes(crInsertString, PBytes(@FBuffer[I]), L.Index, True);
        ClearString(I, L.Index);
        FSelEnd := MakeSelection(I, 0);
        SelectionChanged(True);
      end;
      ecClearSelection:
      begin
        Sel1 := GetRealSelStart;
        Sel2 := GetRealSelEnd;
        if (Sel1.Digit > 0) {and (Sel1.Digit + Sel2.Digit = cDigitCount) }then
        begin
          BeginUndoGroup(crDeleteDigits);
          try
            // digit clear mode
            AddUndoCaretPos;
            FSelEnd := MakeSelection(Sel1.Index + 1, 0);
            FSelStart := FSelEnd;
            if Sel2.Digit = 0 then
            begin
              Dec(L.Index);
              N := FBuffer[Sel2.Index - 1];
            end else
              N := FBuffer[Sel2.Index];
            AddUndoBytes(crInsertDigits, PBytes(@FBuffer[FSelEnd.Index]), L.Index, True);
            ClearString(FSelEnd.Index, L.Index);
            FSelEnd := Sel1;
            AddUndoByte(crDeleteChar, FBuffer[Sel1.Index], False);
            for I := Sel1.Digit to cDigitCount - 1 do
            begin
              FBuffer[Sel1.Index] := ReplaceDigit(FBuffer[Sel1.Index], N mod cBase, I);
              N := N div cBase;
            end;
            SelectionChanged(True);
          finally
            EndUndoGroup;
          end;
        end else
          ExecuteCommand(ecClearIndexSelection);
      end;
      ecSearch, ecReplace:
      begin
        // doesn't search for single digits
        PSD := Data;
        if PSD <> nil then
        begin
          PSD.ErrorReason := eseOk;
          S := AnsiString(PSD.TextToFind);
          if Command = ecReplace then
          begin
            T := AnsiString(PSD.TextToReplace);
            ReplaceAction := eraYes;
          end;
          if esoSelectedOnly in PSD.Options then
            if esoFirstSearch in PSD.Options then
            begin
              PSD.SelStart := GetRealSelStart.Index;
              PSD.SelEnd := GetRealSelEnd.Index;
            end else
            begin
              PSD.SelStart := MinMax(PSD.SelStart, 0, FSize);
              PSD.SelEnd := MinMax(PSD.SelEnd, 0, FSize);
            end;
          if esoFirstSearch in PSD.Options then
            Exclude(PSD.Options, esoWereDigits);
          if esoTreatAsDigits in PSD.Options then
          begin
            if DigitsToBinStr(S) then
            begin
              S := BinStrToBinary(S);
              if Command = ecReplace then
              begin
                if DigitsToBinStr(T) then
                begin
                  T := BinStrToBinary(T);
                  PSD.TextToFind := string(S);
                  PSD.TextToReplace := string(T);
                  Exclude(PSD.Options, esoTreatAsDigits);
                  Include(PSD.Options, esoWereDigits);
                end else
                  PSD.ErrorReason := eseNoDigitsReplace;
              end else
              begin
                PSD.TextToFind := string(S);
                Exclude(PSD.Options, esoTreatAsDigits);
                Include(PSD.Options, esoWereDigits);
              end;
            end else
              PSD.ErrorReason := eseNoDigitsFind;
          end;
          if PSD.ErrorReason = eseOk then
          begin
            N := Length(S);
            if esoBackwards in PSD.Options then
            begin
              O := -1;
              if (esoEntireScope in PSD.Options) and (esoFirstSearch in PSD.Options) then
                I := FSize
              else
                I := GetRealSelStart.Index - 1;
              if esoSelectedOnly in PSD.Options then
              begin
                M := PSD.SelStart;
                if esoFirstSearch in PSD.Options then
                  I := PSD.SelEnd
              end else
                M := 0;
              I := Min(I, FSize - N);
              if I < M then
                PSD.ErrorReason := eseNoMatch
            end else
            begin
              O := 1;
              if (esoEntireScope in PSD.Options) and (esoFirstSearch in PSD.Options) then
                I := 0
              else
                I := GetRealSelEnd.Index;
              if esoSelectedOnly in PSD.Options then
              begin
                M := PSD.SelEnd;
                if esoFirstSearch in PSD.Options then
                  I := PSD.SelStart
              end else
                M := FSize;
              M := Min(M, FSize - N);
              if I >= M then
                PSD.ErrorReason := eseNoMatch
            end;
            if PSD.ErrorReason = eseOk then
            begin
              Found := False;
              MatchCase := PSD.Options * [esoMatchCase, esoWereDigits] <> [];
              if MatchCase then
                C1 := S[1]
              else
                C1 := UpCase(S[1]);
              I := MinMax(I, 0, FSize - 1);
              while I <> M do
              begin
                if MatchCase then
                  C2 := AnsiChar(FBuffer[I])
                else
                  C2 := UpCase(AnsiChar(FBuffer[I]));
                if C1 = C2 then
                begin
                  if FSize - I >= N then
                  begin
                    J := 2;
                    Dec(I);
                    while (J <= N) do
                    begin
                      if MatchCase then
                      begin
                        C2 := AnsiChar(FBuffer[I + J]);
                        C3 := S[J];
                      end else
                      begin
                        C2 := Upcase(AnsiChar(FBuffer[I + J]));
                        C3 := Upcase(S[J]);
                      end;
                      if C2 = C3 then
                        Inc(J)
                      else
                        Break;
                    end;
                    Inc(I);
                    if J = N + 1 then
                    begin
                      Found := True;
                      FSelStart := MakeSelection(I, 0);
                      FSelEnd := MakeSelection(I + N, 0);
                      if Command = ecReplace then
                      begin
                        if (esoPrompt in PSD.Options) and Assigned(FOnReplaceText) then
                        begin
                          SelectionChanged(False, False);
                          if not CaretInView then
                            ExecuteCommand(ecScrollCenter);
                          FOnReplaceText(Self, string(S), string(T), ReplaceAction)
                        end else
                          ReplaceAction := eraYes;
                        case ReplaceAction of
                          eraCancel: Break;
                          eraYes, eraAll:
                          begin
                            if T = '' then
                              ExecuteCommand(ecClearIndexSelection)
                            else
                              ExecuteCommand(ecInsertString, Pointer(T));
                            FSelEnd := MakeSelection(I + Length(T), 0);
                            AddUndoCaretPos;
                            if ReplaceAction  = eraAll then
                              Include(PSD.Options, esoAll);
                          end;
                        end;
                        if not (esoAll in PSD.Options) then
                          Break;
                      end else
                        Break;
                    end
                  end;
                end;
                Inc(I, O);
              end;
              if Found then
              begin
                SelectionChanged(False, False);
                if not CaretInView then
                  ExecuteCommand(ecScrollCenter);
              end else
                PSD.ErrorReason := eseNoMatch;
            end;
          end;
          Exclude(PSD.Options, esoFirstSearch);
        end else
          Result := False;
      end;
      ecInsertMode:
      begin
        Exclude(FStates, elOverwrite);
        UpdateEditorCaret(True);
      end;
      ecOverwriteMode:
      begin
        Include(FStates, elOverwrite);
        UpdateEditorCaret(True);
      end;
      ecToggleMode:
      begin
        if elOverwrite in FStates then
          Exclude(FStates, elOverwrite)
        else
          Include(FStates, elOverwrite);
        UpdateEditorCaret(True);
      end;
      // focus change
      ecGotFocus,
      ecLostFocus:
      begin
        UpdateEditorCaret;
        Invalidate;
      end;
    end;
    if (OldSelStart.Index <> OldSelEnd.Index) or (FSelStart.Index <> FSelEnd.Index) or
      (OldSelStart.Digit <> OldSelEnd.Digit) or (FSelStart.Digit <> FSelEnd.Digit) or
      not (elCaretVisible in FStates) and (edInactiveCaret in FDrawStyles) and
      ((FSelStart.Index <> OldSelStart.Index) or (FSelStart.Digit <> OldSelStart.Digit) or
      (FSelEnd.Index <> OldSelEnd.Index) or (FSelEnd.Digit <> OldSelEnd.Digit)) then
      Invalidate;
  end;
end;

procedure TKCustomHexEditor.FontChange(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    Font.Pitch := fpFixed;
    if Font.Size >= 0 then
      Font.Size := MinMax(Font.Size, cFontSizeMin, cFontSizeMax);
    UpdateCharMetrics;
    UpdateScrollRange;
  end;
end;

function TKCustomHexEditor.GetAreaDimensions: TKHexEditorAreaDimensions;
begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    if edAddress in FDrawStyles then
    begin
      Address := Length(FAddressPrefix) + FAddressSize;
      if FDrawStyles * [edDigits, edText] <> [] then
        AddressOut := FAreaSpacing;
    end;
    if edDigits in FDrawStyles then
    begin
      Digits := FLineSize * cDigitCount + FLineSize div FDigitGrouping;
      if FLineSize mod FDigitGrouping = 0 then
        Dec(Digits);
      if edAddress in FDrawStyles then
        DigitsIn := FAreaSpacing;
      if edText in FDrawStyles then
        DigitsOut := FAreaSpacing;
    end;
    if edText in FDrawStyles then
    begin
      Text := FLineSize;
      if FDrawStyles * [edAddress, edDigits] <> [] then
        TextIn := FAreaSpacing;
    end;
    TotalHorz := Address + AddressOut + Digits + DigitsIn + DigitsOut + Text + TextIn;
    if [edAddress, edDigits, edText] * FDrawStyles <> [] then
      TotalVert := LineCount
    else
      TotalVert := 0;
  end;
end;

function TKCustomHexEditor.GetCaretVisible: Boolean;
begin
  Result := elCaretVisible in FStates;
end;

function TKCustomHexEditor.GetCharMapping: TKEditCharMapping;
begin
  Result := FCharMapping;
end;

function TKCustomHexEditor.GetClientHeightChars: Integer;
begin
  Result := ClientHeight div FCharHeight;
end;

function TKCustomHexEditor.GetClientWidthChars: Integer;
begin
  Result := ClientWidth div FCharWidth;
end;

function TKCustomHexEditor.GetCommandKey(Index: TKEditCommand): TKEditKey;
var
  I: Integer;
begin
  Result.Key := 0;
  Result.Shift := [];
  for I := 0 to Length(FKeyMapping) - 1 do
    if FKeyMapping[I].Command = Index then
    begin
      Result := FKeyMapping[I].Key;
      Exit;
    end;
end;

function TKCustomHexEditor.GetData: TDataSize;
begin
  Result.Data := FBuffer;
  Result.Size := FSize;
end;

function TKCustomHexEditor.GetEmpty: Boolean;
begin
  Result := FBuffer = nil;
end;

function TKCustomHexEditor.GetFirstVisibleIndex: Integer;
begin
  Result := PointToSel(Point(0, 0), False, FEditArea).Index;
end;

function TKCustomHexEditor.GetInsertMode: Boolean;
begin
  Result := not (elOverwrite in FStates);
end;

function TKCustomHexEditor.GetKeyMapping: TKEditKeyMapping;
begin
  Result := FKeyMapping;
end;

function TKCustomHexEditor.GetLastVisibleIndex: Integer;
begin
  Result := PointToSel(GetModifiedClientRect.BottomRight, False, FEditArea).Index;
end;

function TKCustomHexEditor.GetLineCount: Integer;
begin
  Result := DivUp(FSize + 1, FLineSize);
end;

function TKCustomHexEditor.GetLines(Index: Integer): TDataSize;
var
  I: Integer;
begin
  I := Index * FLineSize;
  if (FBuffer <> nil) and (I >= 0) and (I < FSize) then
  begin
    Result.Data := @FBuffer[I];
    Result.Size := Min(FLineSize, FSize - I);
  end else
  begin
    Result.Data := nil;
    Result.Size := 0;
  end;
end;

function TKCustomHexEditor.GetModified: Boolean;
begin
  Result := (elModified in FStates) or FUndoList.Modified;
end;

function TKCustomHexEditor.GetModifiedClientRect: TRect;
begin
  Result := Rect(0, 0, GetClientWidthChars * FCharWidth, GetClientHeightChars * FCharHeight);
end;

function TKCustomHexEditor.GetMaxLeftChar(Extent: Integer): Integer;
begin
  if Extent <= 0 then
    Extent := GetAreaDimensions.TotalHorz;
  Result := Max(Extent - GetClientWidthChars, 0);
end;

function TKCustomHexEditor.GetMaxTopLine(Extent: Integer): Integer;
begin
  if Extent <= 0 then
    Extent := GetAreaDimensions.TotalVert;
  Result := Max(Extent - GetClientHeightChars, 0);
end;

function TKCustomHexEditor.GetPageHorz: Integer;
begin
  case FEditArea of
    eaDigits: Result := ClientWidth * FDigitgrouping div (FCharWidth * (cDigitCount * FDigitGrouping + 1));
    eaText: Result := ClientWidth div FCharWidth;
  else
    Result := 0;
  end;
end;

function TKCustomHexEditor.GetReadOnly: Boolean;
begin
  Result := elReadOnly in FStates;
end;

function TKCustomHexEditor.GetRealSelEnd: TKHexEditorSelection;
begin
  if FSelStart.Index <= FSelEnd.Index then
    Result := FSelEnd
  else
    Result := FSelStart;
end;

function TKCustomHexEditor.GetRealSelStart: TKHexEditorSelection;
begin
  if FSelStart.Index <= FSelEnd.Index then
    Result := FSelStart
  else
    Result := FSelEnd;
end;

function TKCustomHexEditor.GetSelLength: TKHexEditorSelection;
begin
  if FSelStart.Index <= FSelEnd.Index then
    Result.Index := FSelEnd.Index - FSelStart.Index
  else
    Result.Index := FSelStart.Index - FSelEnd.Index;
  if FSelStart.Digit <= FSelEnd.Digit then
    Result.Digit := FSelEnd.Digit - FSelStart.Digit
  else
    Result.Digit := FSelStart.Digit - FSelEnd.Digit;
end;

function TKCustomHexEditor.GetSelText: TKHexEditorSelText;
var
  L, Sel1, Sel2: TKHexEditorSelection;
begin
  L := SelLength;
  with Result do
  begin
    if L.Index > 0 then
    begin
      Sel1 := GetRealSelStart;
      Sel2 := GetRealSelEnd;
      AsBinaryRaw  := BinaryToText(FBuffer, Sel1.Index, Sel2.Index, nil);
      AsBinaryMapped := BinaryToText(FBuffer, Sel1.Index, Sel2.Index, @FCharMapping);
      AsDigits := BinaryToDigits(FBuffer, Sel1, Sel2);
      Sel1.Digit := 0;
      Sel2.Digit := 0;
      AsDigitsByteAligned := BinaryToDigits(FBuffer, Sel1, Sel2);
    end else
    begin
      AsBinaryRaw := '';
      AsBinaryMapped := '';
      AsDigits := '';
      AsDigitsByteAligned := '';
    end;
  end;
end;

function TKCustomHexEditor.GetUndoLimit: Integer;
begin
  Result := FUndoList.Limit;
end;

function TKCustomHexEditor.HasFocus: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and Form.Active then
    Result := (Form.ActiveControl = Self)
  else
    Result := False;
end;

procedure TKCustomHexEditor.InsertChar(At: Integer; Value: Byte);
begin
  InsertString(At, AnsiChar(Value), 1);
end;

procedure TKCustomHexEditor.InsertString(At: Integer; const Value: AnsiString; Size: Integer);
begin
  if (At >= 0) and (At <= FSize) and (Length(Value) > 0) then
  begin
    Inc(FSize, Size);
    ReallocMem(FBuffer, FSize);
    if At < FSize - Size then
      Move(FBuffer[At], FBuffer[At + Size], (FSize - At - Size) * SizeOf(Byte));
    Move(Value[1], FBuffer[At], Size);
    UpdateScrollRange;
  end;
end;

function TKCustomHexEditor.InternalGetSelAvail: Boolean;
begin
  Result := SelAvail;
end;

procedure TKCustomHexEditor.InternalMoveLeft;
begin
  if FEditArea = eaDigits then
  begin
    if FSelEnd.Digit > 0 then
      Dec(FSelEnd.Digit)
    else if FSelEnd.Index > 0 then
    begin
      FSelEnd.Digit := cDigitCount - 1;
      Dec(FSelEnd.Index);
    end
  end else
    Dec(FSelEnd.Index);
end;

procedure TKCustomHexEditor.InternalMoveRight;
begin
  if FEditArea = eaDigits then
  begin
    if (FSelEnd.Index < FSize) and (FSelEnd.Digit < cDigitCount - 1) then
      Inc(FSelEnd.Digit)
    else
    begin
      FSelEnd.Digit := 0;
      Inc(FSelEnd.Index);
    end
  end else
    Inc(FSelEnd.Index);
end;    

function TKCustomHexEditor.IsAddressPrefixStored: Boolean;
begin
  Result := FAddressPrefix <> '0x';
end;

function TKCustomHexEditor.IsDrawStylesStored: Boolean;
begin
  Result := FDrawStyles <> cDrawStylesDef;
end;

function TKCustomHexEditor.IsOptionsStored: Boolean;
begin
  Result := FOptions <> [eoGroupUndo];
end;

procedure TKCustomHexEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
  HK: TKEditKey;
begin
  inherited;
  Exclude(FStates, elIgnoreNextChar);
  if not (csDesigning in ComponentState) then
  begin
    for I := 0 to Length(FKeyMapping) - 1 do
    begin
      HK := FKeyMapping[I].Key;
      if (HK.Key = Key) and (HK.Shift = Shift) then
      begin
        ExecuteCommand(FKeyMapping[I].Command);
        Key := 0;
        Include(FStates, elIgnoreNextChar);
        Exit;
      end;
    end;
    if Key = VK_ESCAPE then
      Include(FStates, elIgnoreNextChar);
  end;
end;

procedure TKCustomHexEditor.KeyPress(var Key: Char);
var
  I: Integer;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if not (elIgnoreNextChar in FStates) then
    begin
      case FEditArea of
        eaDigits: I := DigitToBin(AnsiChar(Key));
        eaText: I := Ord(Key);
      else
        I := -1;
      end;
      if I >= 0 then
        ExecuteCommand(ecInsertChar, @I);
    end else
      Exclude(FStates, elIgnoreNextChar);
  end;
end;

procedure TKCustomHexEditor.LoadFromFile(const FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TKCustomHexEditor.LoadFromStream(Stream: TStream);
var
  Size: Integer;
begin
  Size := Stream.Size - Stream.Position;
  if Size > 0 then
  begin
    Clear;
    FSize := Size;
    GetMem(FBuffer, FSize);
    Stream.Read(FBuffer^, FSize);
    BufferChanged;
  end;
end;

procedure TKCustomHexEditor.MeasurePages(var Info: TKPrintMeasureInfo);
var
  AD: TKHexEditorAreaDimensions;
  PageLines, ActiveLines: Integer;
  FitToPage, SelOnly: Boolean;
  Scale: Double;
  APageSetup: TKPrintPageSetup;
begin
  APageSetup := PageSetup;
  FitToPage := poFitToPage in APageSetup.Options;
  SelOnly := APageSetup.Range = prSelectedOnly;
  Scale := APageSetup.Scale / 100;
  AD := GetAreaDimensions;
  Info.OutlineWidth := AD.TotalHorz * FCharWidth;
  if FitToPage then
    Scale := APageSetup.PaintAreaWidth / Info.OutlineWidth;
  PageLines := Round(APageSetup.PaintAreaHeight / Scale) div FCharHeight;
  if SelOnly then
    ActiveLines := DivUp(GetRealSelEnd.Index, FLineSize) - GetRealSelStart.Index div FLineSize
  else
    ActiveLines := LineCount;
  Info.OutlineHeight := PageLines * FCharHeight;
  Info.HorzPageCount := 1; // cut text off
  Info.VertPageCount := DivUp(ActiveLines, PageLines);
  Info.PageCount := Info.VertPageCount;
end;

procedure TKCustomHexEditor.ModifyScrollBar(ScrollBar, ScrollCode,
  Delta: Integer; UpdateNeeded: Boolean);
var
  I, J, K: Integer;
  HasScrollBar: Boolean;
  SI: TScrollInfo;
begin
  HasScrollBar := (ScrollBar = SB_HORZ) and (ScrollBars = ssHorizontal) or
    (ScrollBar = SB_VERT) and (ScrollBars = ssVertical) or (ScrollBars = ssBoth);
  if HasScrollBar then
  begin
    FillChar(SI, SizeOf(TScrollInfo), 0);
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_PAGE or SIF_TRACKPOS;
    GetScrollInfo(Handle, ScrollBar, SI);
  {$IF DEFINED(LCLGTK2)}
    {.$WARNING "scrollbar arrows still not working properly on GTK2 in some cases!"}
    SI.nTrackPos := Delta;
  {$IFEND}
  end;
  if ScrollBar = SB_HORZ then
  begin
    I := FLeftChar;
    J := GetMaxLeftChar;
  end else
  begin
    I := FTopLine;
    J := GetMaxTopLine;
  end;
  K := I;
  if ScrollCode = cScrollDelta then
    Inc(I, Delta)
  else if HasScrollBar then
  case ScrollCode of
    SB_LINEUP: Dec(I);
    SB_LINEDOWN: Inc(I);
    SB_PAGEUP: Dec(I, SI.nPage);
    SB_PAGEDOWN: Inc(I, SI.nPage);
    SB_THUMBTRACK, SB_THUMBPOSITION: I := SI.nTrackPos;
  end;
  I := MinMax(I, 0, J);
  if K <> I then
  begin
    if HasScrollBar then
    begin
      FillChar(SI, SizeOf(TScrollInfo), 0);
      SI.nPos := I;
      SI.fMask := SIF_POS;
      SetScrollInfo(Handle, ScrollBar, SI, True);
    end;
    if ScrollBar = SB_HORZ then
      FLeftChar := I
    else
      FTopLine := I;
    if UpdateNeeded then
    begin
      UpdateEditorCaret;
      Invalidate;
    end;  
  end;
end;

procedure TKCustomHexEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  Command: TKEditCommand;
begin
  inherited;
  if Enabled and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    SafeSetFocus;
    P := Point(X, Y);
    if ssShift in Shift then
      Command := ecSelGotoXY
    else
      Command := ecGotoXY;
    if ExecuteCommand(Command, @P) then
      Include(FStates, elMouseCapture);
  end;
end;

procedure TKCustomHexEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  R: TRect;
begin
  inherited;
  if (elMouseCapture in FStates) then
  begin
    P := Point(X, Y);
    R := GetModifiedClientRect;
    if PtInRect(R, P) then
      UpdateSelEnd(P, False)
    else if not FScrollTimer.Enabled then
      ScrollTo(P, True, False);
  end;
end;

procedure TKCustomHexEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  Exclude(FStates, elMouseCapture);
end;

procedure TKCustomHexEditor.PaintLines(const Data: TKHexEditorPaintData);
var
  HalfPosWidth, I, Index, J, K, L, M, MaxAddress, WHorz, WVert, WSep,
  LeftIndent, VTextIndent: Integer;
  BC1, BC2, FC1, FC2, PC1: TColor;
  EditorFocused, DrawInactiveCaret, DrawNormal, DigitSep, SelCondition: Boolean;
  S: AnsiString;
  Fmt: string;
  C: Char;
  R, R1, RClip: TRect;
  OldColorScheme: TKHexEditorColorScheme;
  ASelStart, ASelEnd: TKHexEditorSelection;
  AD: TKHexEditorAreaDimensions;
begin
  { this function must be reentrant because of print function
    so there is necessary to backup all changes to global properties}
  OldColorScheme := FColors.ColorScheme;
  with Data.Canvas do
  try
    R := Data.PaintRect;
    AD := GetAreaDimensions;
    // add possible inter-character spacing (in Lazarus not fully implemented yet)
    SetTextCharacterExtra(Handle, Data.CharSpacing);
    LeftIndent := R.Left - Data.LeftChar * Data.CharWidth;
    VTextIndent := (Data.CharHeight - Abs(Font.Height)) div 2;
    HalfPosWidth := Data.CharWidth div 2;
    Fmt := '';
    MaxAddress := 0;
    L := LineCount;
    DrawInactiveCaret := not (Data.Printing or Data.CaretShown) and
      (edInactiveCaret in FDrawStyles);
    DrawNormal := not Data.Printing;
    EditorFocused := HasFocus;
    if FSelStart.Index <= FSelEnd.Index then
    begin
      ASelStart := FSelStart;
      ASelEnd := FSelEnd;
    end else
    begin
      ASelStart := FSelEnd;
      ASelEnd := FSelStart;
    end;
    // preserve space for lines and separators
    if edHorzLines in FDrawStyles then
      WVert := Max(1, Data.CharHeight div 25)
    else
      WVert := 0;
    if edVertLines in FDrawStyles then
      WHorz := Max(1, Data.CharWidth div 20)
    else
      WHorz := 0;
    if edSeparators in FDrawStyles then
      WSep := Max(1, Data.CharWidth div 20)
    else
      WSep := 0;
    // address area pre-comp
    if edAddress in FDrawStyles then
    begin
      if FAddressMode = eamDec then
      begin
        C := 'd';
        J := 10;
      end else
      begin
        C := 'x';
        J := 16;
      end;
      Fmt := Format('%s%%.%d%s', [FAddressPrefix, FAddressSize, C]);
      MaxAddress := 1;
      for I := 1 to FAddressSize do
        MaxAddress := MaxAddress * J;
    end;
    // update color scheme
    if Data.Printing then
    begin
      if Data.PaintColors then
        FColors.ColorScheme := ecsNormal
      else
        FColors.ColorScheme := ecsGrayScale;
    end else
    begin
      if Enabled or (FDisabledDrawStyle = eddNormal) then
        FColors.ColorScheme := ecsNormal
      else if FDisabledDrawStyle = eddGrayed then
        FColors.ColorScheme := ecsGrayed
      else
        FColors.ColorScheme := ecsBright
    end;
    FColors.SingleBkGnd := edSingleBkGnd in FDrawStyles;
    // get clip box for updating;
    if Data.Printing then
      RClip := R
    else
      GetClipBox(Handle, {$IFDEF FPC}@{$ENDIF}RClip);
    // now paint text lines
    for I := Data.TopLine to Min(L - 1, Data.BottomLine) do
    begin
      Brush.Style := bsSolid;
      K := LeftIndent;
      R.Bottom := R.Top + Data.CharHeight - WVert;
      if (R.Top <= RClip.Bottom) and (R.Bottom >= RClip.Top) then
      begin
        if edAddress in FDrawStyles then
        begin
          Index := I * FLineSize;
          Brush.Color := clRed;
          if (DrawNormal or Data.PaintSelection) and ((ASelStart.Index <> ASelEnd.Index) or (ASelStart.Digit <> ASelEnd.Digit)) and
            (Index + FLineSize - 1 >= ASelStart.Index) and (Index < ASelEnd.Index) then
          begin
            PC1 := FColors.LinesHighLight;
            if (FEditArea = eaAddress) and (EditorFocused or Data.PaintSelection) then
            begin
              FC1 := FColors.SelTextFocused;
              BC1 := FColors.SelBkGndFocused;
            end else
            begin
              FC1 := FColors.SelText;
              BC1 := FColors.SelBkGnd;
            end;
          end else
          begin
            PC1 := FColors.HorzLines;
            FC1 := FColors.AddressText;
            BC1 := FColors.AddressBkGnd;
          end;
          Brush.Color := BC1;
          Font.Color := FC1;
          R.Left := K;
          Inc(K, AD.Address * Data.CharWidth);
          R.Right := K;
          J := I * FLineSize + FAddressOffset;
          if MaxAddress <> 0 then J := J mod MaxAddress;
          FillRect(R);
          TextOut(R.Left, R.Top + VTextIndent, Format(Fmt, [J]));
          if edHorzLines in FDrawStyles then
          begin
            Brush.Color := PC1;
            FillRect(Rect(R.Left, R.Bottom, R.Right, R.Bottom + WVert));
          end;
          if AD.AddressOut > 0 then
          begin
            R.Left := K;
            Inc(K, AD.AddressOut * Data.CharWidth);
            R.Right := K;
            Brush.Color := FColors.AddressBkGnd;
            FillRect(Rect(R.Left, R.Top, R.Right - WSep, R.Bottom));
            if edHorzLines in FDrawStyles then
            begin
              Brush.Color := FColors.HorzLines;
              FillRect(Rect(R.Left, R.Bottom, R.Right - WSep, R.Bottom + WVert));
            end;
          end;
        end;
        if edDigits in FDrawStyles then
        begin
          if AD.DigitsIn > 0 then
          begin
            R.Left := K;
            Inc(K, AD.DigitsIn * Data.CharWidth);
            R.Right := K;
            Brush.Color := FColors.DigitBkGnd;
            FillRect(Rect(R.Left + WSep, R.Top, R.Right, R.Bottom));
            if edHorzLines in FDrawStyles then
            begin
              Brush.Color := FColors.HorzLines;
              FillRect(Rect(R.Left + WSep, R.Bottom, R.Right, R.Bottom + WVert));
            end;
          end;
          Index := 0;
          for J := 0 to FLineSize - 1 do
          begin
            Index := I * FLineSize + J;
            DigitSep := (J < FLineSize - 1) and ((J + 1) mod FDigitGrouping = 0);
            R.Left := K;
            Inc(K, cDigitCount * Data.CharWidth);
            R.Right := K;
            if Index <= FSize then
            begin
              if Index < FSize then
                S := AnsiString(Format(cFmtText, [FBuffer[Index]]))
              else
                S := '  ';
              if (Index <> FSelStart.Index) and (Index <> FSelEnd.Index) then
              begin
                SelCondition := (Index >= ASelStart.Index) and (Index < ASelEnd.Index);
                if (DrawNormal or Data.PaintSelection) and SelCondition then
                begin
                  PC1 := FColors.LinesHighLight;
                  if (FEditArea = eaDigits) and (EditorFocused or Data.PaintSelection) then
                  begin
                    FC1 := FColors.SelTextFocused;
                    BC1 := FColors.SelBkGndFocused;
                  end else
                  begin
                    FC1 := FColors.SelText;
                    BC1 := FColors.SelBkGnd;
                  end;
                  FC2 := FColors.InactiveCaretSelText;
                  BC2 := FColors.InactiveCaretSelBkGnd;
                end else
                begin
                  PC1 := FColors.HorzLines;
                  if DrawNormal or Data.PaintAll or SelCondition then
                  begin
                    if (J div FDigitGrouping) and 1 = 0 then
                      FC1 := FColors.DigitTextEven
                    else
                      FC1 := FColors.DigitTextOdd;
                  end else
                    FC1 := FColors.DigitBkGnd;
                  BC1 := FColors.DigitBkGnd;
                  FC2 := FColors.InactiveCaretText;
                  BC2 := FColors.InactiveCaretBkGnd;
                end;
                Brush.Color := BC1;
                Font.Color := FC1;
                Brush.Style := bsSolid;
                FillRect(R);
                Brush.Style := bsClear;
                TextOut(R.Left, R.Top + VTextIndent, Char(S[1]));
                TextOut(R.Left + Data.CharWidth, R.Top + VTextIndent, Char(S[2]));
                if (Index = FSelEnd.Index) and DrawInactiveCaret then
                begin
                  // draw inactive caret - place into previous drawn text
                  R1 := R;
                  Inc(R1.Left, Data.CharWidth * Min(FSelEnd.Digit, cDigitCount - 1));
                  R1.Right := R1.Left + Data.CharWidth;
                  Font.Color := FC2;
                  Brush.Color := BC2;
                  Brush.Style := bsSolid;
                  FillRect(R1);
                  Brush.Style := bsClear;
                  TextOut(R1.Left, R1.Top + VTextIndent, string(S));
                end;
                if edHorzLines in FDrawStyles then
                begin
                  Brush.Color := PC1;
                  Brush.Style := bsSolid;
                  FillRect(Rect(R.Left, R.Bottom, R.Right, R.Bottom + WVert));
                end;
              end else
              begin
                R1 := R;
                R1.Right := R1.Left;
                Inc(R1.Right, Data.CharWidth);
                for M := 0 to cDigitCount - 1 do
                begin
                  SelCondition :=
                    (ASelStart.Index = ASelEnd.Index) and (
                    (M >= ASelStart.Digit) and (M < ASelEnd.Digit) or
                    (M >= ASelEnd.Digit) and (M < ASelStart.Digit)
                    )
                    or
                    (ASelStart.Index <> ASelEnd.Index) and (
                    (Index = ASelStart.Index) and (M >= ASelStart.Digit) or
                    (Index = ASelEnd.Index) and (M < ASelEnd.Digit)
                    );
                  if (DrawNormal or Data.PaintSelection) and SelCondition then
                  begin
                    PC1 := FColors.LinesHighLight;
                    if DrawInactiveCaret and (Index = FSelEnd.Index) and (M = FSelEnd.Digit) then
                    begin
                      FC1 := FColors.InactiveCaretSelText;
                      BC1 := FColors.InactiveCaretSelBkGnd;
                    end
                    else if (FEditArea = eaDigits) and (EditorFocused or Data.PaintSelection) then
                    begin
                      FC1 := FColors.SelTextFocused;
                      BC1 := FColors.SelBkGndFocused;
                    end else
                    begin
                      FC1 := FColors.SelText;
                      BC1 := FColors.SelBkGnd;
                    end;
                  end else
                  begin
                    PC1 := FColors.HorzLines;
                    if DrawInactiveCaret and (Index = FSelEnd.Index) and (M = FSelEnd.Digit) then
                    begin
                      FC1 := FColors.InactiveCaretText;
                      BC1 := FColors.InactiveCaretBkGnd;
                    end else
                    begin
                      if DrawNormal or Data.PaintAll or SelCondition then
                      begin
                        if (J div FDigitGrouping) and 1 = 0 then
                          FC1 := FColors.DigitTextEven
                        else
                          FC1 := FColors.DigitTextOdd;
                      end else
                        FC1 := FColors.DigitBkGnd;
                      BC1 := FColors.DigitBkGnd;
                    end;
                  end;
                  Brush.Color := BC1;
                  Font.Color := FC1;
                  Brush.Style := bsSolid;
                  FillRect(R1);
                  Brush.Style := bsClear;
                  TextOut(R1.Left, R1.Top + VTextIndent, Char(S[M + 1]));
                  if edHorzLines in FDrawStyles then
                  begin
                    Brush.Color := PC1;
                    Brush.Style := bsSolid;
                    FillRect(Rect(R1.Left, R1.Bottom, R1.Right, R1.Bottom + WVert));
                  end;
                  R1.Left := R1.Right;
                  Inc(R1.Right, Data.CharWidth);
                end;
              end;
              if DigitSep then
              begin
                if Index < FSize then
                  M := Data.CharWidth
                else
                  M := HalfPosWidth;
                Brush.Color := FColors.DigitBkGnd;
                Brush.Style := bsSolid;
                FillRect(Rect(R.Right, R.Top, R.Right + Data.CharWidth, R.Bottom));
                if edHorzLines in FDrawStyles then
                begin
                  Brush.Color := FColors.HorzLines;
                  FillRect(Rect(R.Right, R.Bottom, R.Right + M, R.Bottom + WVert));
                end;
                if edVertLines in FDrawStyles then
                begin
                  M := R.Right + HalfPosWidth;
                  Brush.Color := FColors.VertLines;
                  FillRect(Rect(M, R.Top, M + WHorz, R.Bottom));
                end;
                Inc(K, Data.CharWidth);
              end;
            end else
            begin
              Inc(K, Integer(DigitSep) * Data.CharWidth);
              Brush.Color := FColors.DigitBkGnd;
              Brush.Style := bsSolid;
              FillRect(Rect(R.Left, R.Top, K, R.Bottom + WVert));
            end;
          end;
          if AD.DigitsOut > 0 then
          begin
            R.Left := K;
            Inc(K, AD.DigitsOut * Data.CharWidth);
            R.Right := K;
            Brush.Style := bsSolid;
            Brush.Color := FColors.DigitBkGnd;
            FillRect(Rect(R.Left, R.Top, R.Right - WSep, R.Bottom));
            if edHorzLines in FDrawStyles then
            begin
              if Index < FSize then
                Brush.Color := FColors.HorzLines
              else
                Brush.Color := FColors.DigitBkGnd;
              FillRect(Rect(R.Left, R.Bottom, R.Right - WSep, R.Bottom + WVert));
            end;
          end;
        end;
        if edText in FDrawStyles then
        begin
          if AD.TextIn > 0 then
          begin
            R.Left := K;
            Inc(K, AD.TextIn * Data.CharWidth);
            R.Right := K;
            Brush.Color := FColors.TextBkGnd;
            Brush.Style := bsSolid;
            FillRect(Rect(R.Left + WSep, R.Top, R.Right, R.Bottom));
            if edHorzLines in FDrawStyles then
            begin
              Brush.Color := FColors.HorzLines;
              FillRect(Rect(R.Left + WSep, R.Bottom, R.Right, R.Bottom + WVert));
            end;
          end;
          for J := 0 to FLineSize - 1 do
          begin
            Index := I * FLineSize + J;
            R.Left := K;
            Inc(K, Data.CharWidth);
            R.Right := K;
            if Index <= FSize then
            begin
              SelCondition := (Index >= ASelStart.Index) and (Index < ASelEnd.Index);
              if (DrawNormal or Data.PaintSelection) and SelCondition then
              begin
                PC1 := FColors.LinesHighLight;
                if DrawInactiveCaret and (Index = FSelEnd.Index) then
                begin
                  FC1 := FColors.InactiveCaretSelText;
                  BC1 := FColors.InactiveCaretSelBkGnd;
                end
                else if (FEditArea = eaText) and (EditorFocused or Data.PaintSelection) then
                begin
                  FC1 := FColors.SelTextFocused;
                  BC1 := FColors.SelBkGndFocused;
                end else
                begin
                  FC1 := FColors.SelText;
                  BC1 := FColors.SelBkGnd;
                end;
              end else
              begin
                PC1 := FColors.HorzLines;
                if DrawInactiveCaret and (Index = FSelEnd.Index) then
                begin
                  FC1 := FColors.InactiveCaretText;
                  BC1 := FColors.InactiveCaretBkGnd;
                end else
                begin
                  if DrawNormal or Data.PaintAll or SelCondition then
                    FC1 := FColors.TextText
                  else
                    FC1 := FColors.TextBkgnd;
                  BC1 := FColors.TextBkgnd;
                end;
              end;
              Brush.Color := BC1;
              Brush.Style := bsSolid;
              FillRect(R);
              Brush.Style := bsClear;
              if Index < FSize then
              begin
                Font.Color := FC1;
                TextOut(R.Left, R.Top + VTextIndent, Char(FCharMapping[FBuffer[Index]]));
              end;
              if edHorzLines in FDrawStyles then
              begin
                Brush.Color := PC1;
                Brush.Style := bsSolid;
                FillRect(Rect(R.Left, R.Bottom, R.Right, R.Bottom + WVert));
              end;
            end else
            begin
              Brush.Color := FColors.TextBkGnd;
              Brush.Style := bsSolid;
              FillRect(Rect(R.Left, R.Top, K, R.Bottom + WVert));
            end;
          end;
        end;
      end;
      Inc(R.Top, Data.CharHeight);
    end;
    // now complete blank areas below text and optionally paint separators
    K := LeftIndent;
    R.Bottom := Data.PaintRect.Bottom;
    Brush.Style := bsSolid;
    if edAddress in FDrawStyles then
    begin
      R.Left := K;
      Inc(K, (AD.Address + AD.AddressOut) * Data.CharWidth);
      R.Right := K; if FDrawStyles * [edDigits, edText] <> [] then Dec(R.Right, WSep);
      if R.Top < R.Bottom then
      begin
        Brush.Color := FColors.AddressBkGnd;
        FillRect(R);
      end;
      if (edSeparators in FDrawStyles) and (FDrawStyles * [edDigits, edText] <> []) then
      begin
        Brush.Color := FColors.Separators;
        FillRect(Rect(K - WSep, Data.PaintRect.Top, K + WSep, Data.PaintRect.Bottom));
      end;
    end;
    if edDigits in FDrawStyles then
    begin
      R.Left := K; if edAddress in FDrawStyles then Inc(R.Left, WSep);
      Inc(K, (AD.Digits + AD.DigitsIn + AD.DigitsOut) * Data.CharWidth);
      R.Right := K; if edText in FDrawStyles then Dec(R.Right, WSep);
      if R.Top < R.Bottom then
      begin
        Brush.Color := FColors.DigitBkGnd;
        FillRect(R);
      end;
      if (edSeparators in FDrawStyles) and (edText in FDrawStyles) then
      begin
        Brush.Color := FColors.Separators;
        FillRect(Rect(K - WSep, Data.PaintRect.Top, K + WSep, Data.PaintRect.Bottom));
      end;
    end;
    if edText in FDrawStyles then
    begin
      R.Left := K; if FDrawStyles * [edAddress, edDigits] <> [] then Inc(R.Left, WSep);
      Inc(K, (AD.TextIn + AD.Text) * Data.CharWidth);
      R.Right := K;
      if R.Top < R.Bottom then
      begin
        Brush.Color := FColors.TextBkGnd;
        FillRect(R);
      end;
    end;
    if K < ClientWidth then
    begin
      Brush.Color := FColors.BkGnd;
      FillRect(Rect(K, 0, ClientWidth, ClientHeight));
    end;
  finally
    FColors.ColorScheme := OldColorScheme;
  end;
end;

procedure TKCustomHexEditor.PaintPage;
var
  ActiveLines, AreaWidth, AreaHeight, FirstLine, PageLines: Integer;
  SelOnly: Boolean;
  TmpRect, TmpRect1: TRect;
  APageSetup: TKPrintPageSetup;
  Data: TKHexEditorPaintData;
begin
  APageSetup := PageSetup;
  SelOnly := APageSetup.Range = prSelectedOnly;
  AreaWidth := Round(APageSetup.PaintAreaWidth / APageSetup.CurrentScale);
  AreaHeight := Round(APageSetup.PaintAreaHeight / APageSetup.CurrentScale);
  PageLines := AreaHeight div FCharHeight;
  if SelOnly then
  begin
    FirstLine := GetRealSelStart.Index div FLineSize;
    ActiveLines := DivUp(GetRealSelEnd.Index, FLineSize) - FirstLine;
  end else
  begin
    FirstLine := 0;
    ActiveLines := LineCount;
  end;
  TmpRect := Rect(0, 0, APageSetup.OutlineWidth, APageSetup.OutlineHeight);
  TmpRect1 := Rect(0, 0, AreaWidth, AreaHeight);
  IntersectRect(TmpRect, TmpRect, TmpRect1);
  TmpRect1 := TmpRect;
  TranslateRectToDevice(APageSetup.Canvas.Handle, TmpRect1);
  SelectClipRect(APageSetup.Canvas.Handle, TmpRect1);
  Data.Canvas := APageSetup.Canvas;
  Data.Canvas.Font := Font;
  Data.Canvas.Font.Height := Abs(Font.Height);
  Data.PaintRect := TmpRect;
  Data.TopLine := (APageSetup.CurrentPage - 1) * PageLines;
  Data.BottomLine := Min(Data.TopLine + PageLines, ActiveLines) - 1;
  Inc(Data.TopLine, FirstLine);
  Inc(Data.BottomLine, FirstLine);
  Data.LeftChar := 0;
  Data.CharWidth := FCharWidth;
  Data.CharHeight := FCharHeight;
  Data.CharSpacing := FTotalCharSpacing;
  Data.Printing := True;
  Data.PaintSelection := poPaintSelection in APageSetup.Options;
  Data.PaintAll := not SelOnly;
  Data.PaintColors := poUseColor in APageSetup.Options;
  PaintLines(Data);
end;

procedure TKCustomHexEditor.PaintToCanvas(ACanvas: TCanvas);
var
  Data: TKHexEditorPaintData;
begin
  ACanvas.Font := Font;
  with Data do
  begin
    Canvas := ACanvas;
    PaintRect := ClientRect;
    LeftChar := FLeftChar;
    TopLine := FTopLine;
    CharWidth := FCharWidth;
    CharHeight := FCharHeight;
    BottomLine := TopLine + ClientHeight div FCharHeight;
    CharSpacing := FTotalCharSpacing;
    Printing := False;
    PaintSelection := False;
    CaretShown := elCaretVisible in FStates;
  end;
{$IFDEF FPC}
  if Data.CaretShown then
    HideEditorCaret;
  try
{$ENDIF}
    PaintLines(Data);
{$IFDEF FPC}
  finally
    if Data.CaretShown then
      ShowEditorCaret;
  end;
{$ENDIF}
end;

procedure TKCustomHexEditor.PaintToCanvasEx(ACanvas: TCanvas; ARect: TRect; ALeftChar, ATopLine: Integer);
var
  Data: TKHexEditorPaintData;
  Region: HRGN;
begin
  ACanvas.Font := Font;
  with Data do
  begin
    Canvas := ACanvas;
    PaintRect := ARect;
    LeftChar := ALeftChar;
    TopLine := ATopLine;
    CharWidth := FCharWidth;
    CharHeight := FCharHeight;
    BottomLine := TopLine + (ARect.Bottom - ARect.Top) div FCharHeight;
    CharSpacing := FTotalCharSpacing;
    Printing := False;
    PaintSelection := False;
  end;
  Region := CreateRectRgnIndirect(ARect);
  try
    SelectClipRgn(ACanvas.Handle, Region);
    try
      PaintLines(Data);
    finally
      SelectClipRgn(ACanvas.Handle, 0);
    end;
  finally
    DeleteObject(Region);
  end;
end;

function TKCustomHexEditor.PointToSel(P: TPoint; OutOfArea: Boolean; var Area: TKHexEditorArea): TKHexEditorSelection;
var
  Digit, HalfPosWidth, I, X, X1, XMax: Integer;
  DigitSep: Boolean;
  AD: TKHexEditorAreaDimensions;
  Sel: TKHexEditorSelection;
begin
  Result := MakeSelection(cInvalidIndex, 0);
  P.X := P.X + FLeftChar * FCharWidth;
  P.Y := P.Y div FCharHeight + FTopLine;
  AD := GetAreaDimensions;
  HalfPosWidth := FCharWidth div 2;
  X := 0;
  if OutOfArea then
    P.Y := MinMax(P.Y, 0, LineCount - 1)
  else
    Area := eaNone;
  if P.Y < LineCount then
  begin
    if edAddress in FDrawStyles then
    begin
      XMax := X + (AD.Address + AD.AddressOut) * FCharWidth;
      if not OutOfArea or (Area = eaAddress) then
        if (P.X >= X) and (P.X < XMax) then
        begin
          Result := MakeSelection(P.Y * FLineSize, 0);
          Area := eaAddress;
        end
        else if Area = eaAddress then // OutOfArea = True
        begin
          Result.Index := P.Y * FLineSize;
          if P.X >= XMax then
            Inc(Result.Index, FLineSize);
        end;
      X := XMax;
    end;
    if (P.X >= X) or OutOfArea then
    begin
      if edDigits in FDrawStyles then
      begin
        XMax := X + (AD.Digits + AD.DigitsIn + AD.DigitsOut) * FCharWidth;
        if not OutOfArea or (Area = eaDigits) then
          if (P.X >= X) and (P.X < XMax) then
          begin
            Inc(X, AD.DigitsIn * FCharWidth);
            for I := 0 to FLineSize - 1 do
            begin
              DigitSep := (I < FLineSize - 1) and ((I + 1) mod FDigitGrouping = 0);
              X1 := X;
              Inc(X, cDigitCount * FCharWidth);
              if DigitSep then
                Inc(X, HalfPosWidth)
              else if I = FLineSize - 1 then
                Inc(X, AD.DigitsOut * FCharWidth);
              if P.X < X then
              begin
                Digit := (Max(P.X - X1, 0) + HalfPosWidth) div FCharWidth;
                Sel := MakeSelection(P.Y * FLineSize + I, Digit);
                if (Digit >= cDigitCount) and (Sel.Index < FSize) then // don't split the FSize character box
                begin
                  Inc(Sel.Index);
                  Sel.Digit := 0;
                end;
                if (Sel.Index <= FSize) or OutOfArea then
                begin
                  Result := Sel;
                  Area := eaDigits;
                end;
                Break;
              end;
              if DigitSep then
                Inc(X, HalfPosWidth);
            end;
          end
          else if Area = eaDigits then // OutOfArea = True
          begin
            Result.Index := P.Y * FLineSize;
            if P.X >= XMax then
              Inc(Result.Index, FLineSize);
          end;
        X := XMax;
      end;
      if ((P.X >= X) or OutOfArea) and (edText in FDrawStyles) then
      begin
        XMax := X + (AD.Text + AD.TextIn) * FCharWidth;
        if not OutOfArea or (Area = eaText) then
          if (P.X >= X) and (P.X < XMax) then
          begin
            Inc(X, AD.TextIn * FCharWidth);
            Sel := MakeSelection(P.Y * FLineSize, 0);
            I := Max(P.X - X, 0) div FCharWidth;
            if Sel.Index + I = FSize then
              Sel.Index := FSize // don't split the FSize character box
            else
              Inc(Sel.Index, (Max(P.X - X, 0) + HalfPosWidth) div FCharWidth);
            if (Sel.Index <= FSize) or OutOfArea then
            begin
              Result := Sel;
              Area := eaText;
            end;
          end
          else if Area = eaText then // OutOfArea = True
          begin
            Result.Index := P.Y * FLineSize;
            if P.X >= XMax then
              Inc(Result.Index, FLineSize);
          end;
      end;
    end;
  end;
  ValidateSelection(Result, Area);
end;

procedure TKCustomHexEditor.SafeSetFocus;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and not (csDestroying in Form.ComponentState)
    and Visible and Enabled then
      Form.ActiveControl := Self;
end;

procedure TKCustomHexEditor.SaveToFile(const FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TKCustomHexEditor.SaveToStream(Stream: TStream);
begin
  if FBuffer <> nil then
    Stream.Write(FBuffer^, FSize);
end;

procedure TKCustomHexEditor.ScrollBy(HChars, VChars: Integer; UpdateNeeded: Boolean);
begin
  if HChars <> 0 then
    ModifyScrollBar(SB_HORZ, cScrollDelta, HChars, UpdateNeeded);
  if VChars <> 0 then
    ModifyScrollBar(SB_VERT, cScrollDelta, VChars, UpdateNeeded);
end;

procedure TKCustomHexEditor.ScrollTo(Point: TPoint; Timed, AlwaysScroll: Boolean);
var
  ScrollHorz: Boolean;
  R: TRect;
begin
  // disable horizontal overscroll when scrolling e.g. with mouse
  ScrollHorz := AlwaysScroll or (FSelEnd.Index mod FLineSize <> 0) and
    (FSelEnd.Index < FSize) or (FSelEnd.Digit > 0);
  R := GetModifiedClientRect;
  if ScrollHorz then
  begin
    if Point.X < R.Left then
      FScrollDeltaX := DivDown(Point.X, FCharWidth)
    else if Point.X >= R.Right then
      FScrollDeltaX := (Point.X - R.Right) div FCharWidth + 1
    else
      FScrollDeltaX := 0;
  end else
    FScrollDeltaX := 0;
  if Point.Y < R.Top then
    FScrollDeltaY := DivDown(Point.Y, FCharHeight)
  else if Point.Y >= R.Bottom then
    FScrollDeltaY := (Point.Y - R.Bottom) div FCharHeight + 1
  else
    FScrollDeltaY := 0;
  if (FScrollDeltaX <> 0) or (FScrollDeltaY <> 0) then
    if Timed then
    begin
      ScrollBy(FScrollDeltaX, FScrollDeltaY, False);
      FScrollTimer.Enabled := True;
    end else
      ScrollBy(FScrollDeltaX, FScrollDeltaY, True);
  UpdateSelEnd(Point, True);
end;

procedure TKCustomHexEditor.ScrollTimerHandler(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (elMouseCapture in FStates) and not (Dragging or
    PtInRect(GetModifiedClientRect, P)) then
    ScrollTo(P, True, False)
  else
    FScrollTimer.Enabled := False;
end;

function TKCustomHexEditor.SelAvail: Boolean;
begin
  Result := SelLength.Index > 0;
end;

procedure TKCustomHexEditor.SelectionChanged(StartEqualEnd: Boolean; ScrollToView: Boolean = True);
begin
  ValidateSelection(FSelEnd, FEditArea);
  if StartEqualEnd then
    FSelStart := FSelEnd
  else
    ValidateSelection(FSelStart, FEditArea);
  if HasParent then
  begin
    if ScrollToView and (FEditArea <> eaNone) then
      ScrollTo(SelToPoint(FSelEnd, FEditArea), False, True);
    UpdateEditorCaret;
    Invalidate;
    InvalidatePageSetup;
  end;  
end;

function TKCustomHexEditor.SelectionValid(Value: TKHexEditorSelection; Area: TKHexEditorArea): Boolean;
begin
  Result := (Area <> eaNone) and (
    (Value.Index >= 0) and (Value.Index < FSize) or
    (Value.Index = FSize) and (Value.Digit = 0))
end;

function TKCustomHexEditor.SelToPoint(Value: TKHexEditorSelection; Area: TKHexEditorArea): TPoint;
var
  AD: TKHexEditorAreaDimensions;
begin
  Result := Point(0, 0);
  AD := GetAreaDimensions;
  ValidateSelection(Value, Area);
  if (Area = eaDigits) and (edDigits in FDrawStyles) then
  begin
    Result.X := ((Value.Index mod FLineSize) div FDigitGrouping * (cDigitCount * FDigitGrouping + 1) +
      (Value.Index mod FLineSize) mod FDigitGrouping * cDigitCount + Value.Digit + AD.DigitsIn)
  end else if (Area = eaText) and (edText in FDrawStyles) then
    Result.X := (Value.Index mod FLineSize + AD.DigitsIn + AD.Digits + AD.DigitsOut + AD.TextIn)
  else if Area = eaAddress then
  begin
    if edDigits in FDrawStyles then
      Result.X := AD.DigitsIn
    else if edText in FDrawStyles then
      Result.X := AD.TextIn;
  end;
  Result.X := (Result.X + AD.Address + AD.AddressOut - FLeftChar) * FCharWidth;
  Result.Y := (Value.Index div FLineSize - FTopLine) * FCharHeight;
end;

procedure TKCustomHexEditor.SetAddressCursor(Value: TCursor);
begin
  if Value <> FAddressCursor  then
  begin
    FAddressCursor := Value;
    UpdateMouseCursor;
  end;
end;

procedure TKCustomHexEditor.SetAddressMode(Value: TKHexEditorAddressMode);
begin
  if Value <> FAddressMode then
  begin
    FAddressMode := Value;
    Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetAddressOffset(Value: Integer);
begin
  if Value <> FAddressOffset then
  begin
    FAddressOffset := Value;
    Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetAddressPrefix(const Value: string);
begin
  if Value <> FAddressPrefix then
  begin
    FAddressPrefix := Value;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetAddressSize(Value: Integer);
begin
  Value := MinMax(Value, cAddressSizeMin, cAddressSizeMax);
  if Value <> FAddressSize then
  begin
    FAddressSize := Value;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetAreaSpacing(Value: Integer);
begin
  Value := MinMax(Value, cAreaSpacingMin, cAreaSpacingMax);
  if Value <> FAreaSpacing then
  begin
    FAreaSpacing := Value;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetCharMapping(const Value: TKEditCharMapping);
begin
  if not CompareMem(@Value, @FCharMapping, SizeOf(TKEditCharMapping)) and
    (edText in FDrawStyles) then
    Invalidate;
end;

procedure TKCustomHexEditor.SetCharSpacing(Value: Integer);
begin
  Value := MinMax(Value, cCharSpacingMin, cCharSpacingMax);
  if Value <> FCharSpacing then
  begin
    FCharSpacing := Value;
    UpdateCharMetrics;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetColors(Value: TKHexEditorColors);
begin
  FColors.Assign(Value);
end;

procedure TKCustomHexEditor.SetCommandKey(Index: TKEditCommand; Value: TKEditKey);
var
  I: Integer;
begin
  for I := 0 to Length(FKeyMapping) - 1 do
    if FKeyMapping[I].Command = Index then
    begin
      FKeyMapping[I].Key := Value;
      Exit;
    end;
end;

procedure TKCustomHexEditor.SetData(Value: TDataSize);
begin
  if (Value.Data <> FBuffer) or (Value.Size <> FSize) then
  begin
    Clear;
    if Value.Data <> nil then
    begin
      FSize := Value.Size;
      GetMem(FBuffer, FSize);
      System.Move(Value.Data^, FBuffer^, FSize);
      BufferChanged;
    end;
  end;
end;

procedure TKCustomHexEditor.SetDigitGrouping(Value: Integer);
begin
  Value := MinMax(Value, cDigitGroupingMin, Min(FLineSize, cDigitGroupingMax));
  if Value <> FDigitGrouping then
  begin
    FDigitGrouping := Value;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetDisabledDrawStyle(Value: TKHexEditorDisabledDrawStyle);
begin
  if Value <> FDisabledDrawStyle then
  begin
    FDisabledDrawStyle := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetDrawStyles(const Value: TKHexEditorDrawStyles);
begin
  if Value <> FDrawStyles then
  begin
    FDrawStyles := Value;
    EditAreaChanged; // must be called first
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetEditArea(Value: TKHexEditorArea);
begin
  if Value <> FEditArea then
  begin
    FEditArea := Value;
    EditAreaChanged;
    if Value <> FEditArea then
      Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetKeyMapping(const Value: TKEditKeyMapping);
begin
  SetLength(FKeyMapping, Length(Value));
  Move(Value, FKeyMapping, Length(Value) * SizeOf(TKEditCommandAssignment));
end;

procedure TKCustomHexEditor.SetLineHeightPercent(Value: Integer);
begin
  Value := MinMax(Value, cLineHeightPercentMin, cLineHeightPercentMax);
  if Value <> FLineHeightPercent then
  begin
    FLineHeightPercent := Value;
    UpdateCharMetrics;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetLeftChar(Value: Integer);
begin
  Value := MinMax(Value, 0, GetMaxLeftChar);
  if Value <> FLeftChar then
    ScrollBy(Value - FLeftChar, 0, True);
end;

procedure TKCustomHexEditor.SetLines(Index: Integer; const Value: TDataSize);
var
  I, Size: Integer;
begin
  I := Index * FLineSize;
  if (Value.Data <> nil) and (Value.Size > 0) and (I >= 0) and (I <= FSize) then
  begin
    Size := Min(FLineSize, Value.Size);
    if I + Size > FSize then
    begin
      FSize := Size;
      ReallocMem(FBuffer, FSize);
    end;
    System.Move(Value.Data^, FBuffer[I], Size);
    BufferChanged;
  end;
end;

procedure TKCustomHexEditor.SetLineSize(Value: Integer);
begin
  Value := MinMax(Value, cLineSizeMin, cLineSizeMax);
  if Value <> FLineSize then
  begin
    FLineSize := Value;
    UpdateScrollRange;
  end;
end;

procedure TKCustomHexEditor.SetModified(Value: Boolean);
begin
  if Value <> GetModified then
  begin
    if Value then
      Include(FStates, elModified)
    else
    begin
      Exclude(FStates, elModified);
      if eoUndoAfterSave in FOptions then
        FUndoList.Modified := False
      else
      begin
        FUndoList.Clear;
        FRedoList.Clear;
      end;
    end;
  end;
end;

function TKCustomHexEditor.SetMouseCursor(X, Y: Integer): Boolean;
var
  ACursor: TCursor;
  P: TPoint;
  Area: TKHexEditorArea;
begin
  P := Point(X, Y);
  PointToSel(P, False, Area);
  if PtInRect(ClientRect, P) then
  begin
    case Area of
      eaAddress: ACursor := FAddressCursor;
      eaDigits: ACursor := crIBeam;
      eaText: ACursor := crIBeam;
    else
      ACursor := crDefault;
    end;
  end else
    ACursor := crDefault;
{$IFDEF FPC}
  FCursor := ACursor;
  SetTempCursor(ACursor);
{$ELSE}
  Windows.SetCursor(Screen.Cursors[ACursor]);
{$ENDIF}
  Result := True;
end;

procedure TKCustomHexEditor.SetOptions(const Value: TKEditOptions);
{$IFDEF USE_WINAPI}
var
  UpdateDropFiles: Boolean;
{$ENDIF}
begin
  if Value <> FOptions then
  begin
  {$IFDEF USE_WINAPI}
    UpdateDropFiles := (eoDropFiles in Value) <> (eoDropFiles in FOptions);
    FOptions := Value;
    // (un)register HWND as drop target
    if UpdateDropFiles and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
  {$ELSE}
    FOptions := Value;
  {$ENDIF}
  end;
end;

procedure TKCustomHexEditor.SetReadOnly(Value: Boolean);
begin
  if Value <> GetReadOnly then
  begin
    if Value then
      Include(FStates, elReadOnly)
    else
      Exclude(FStates, elReadOnly);
  end;
end;

procedure TKCustomHexEditor.SetScrollBars(Value: TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
  {$IFDEF FPC}
    UpdateSize;
  {$ELSE}
    RecreateWnd;
  {$ENDIF}
  end;
end;

procedure TKCustomHexEditor.SetScrollSpeed(Value: Cardinal);
begin
  Value := MinMax(Integer(Value), cScrollSpeedMin, cScrollSpeedMax);
  if Value <> FScrollSpeed then
  begin
    FScrollSpeed := Value;
    FScrollTimer.Enabled := False;
    FScrollTimer.Interval := FScrollSpeed;
  end;
end;

procedure TKCustomHexEditor.SetSelEnd(Value: TKHexEditorSelection);
begin
  if (Value.Index <> FSelEnd.Index) or (Value.Digit <> FSelEnd.Digit) then
  begin
    FSelEnd := Value;
    SelectionChanged(False, False);
    Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetSelLength(Value: TKHexEditorSelection);
var
  X: TKHexEditorSelection;
begin
  X := GetSelLength;
  if (Value.Index <> X.Index) or (Value.Digit <> X.Digit) then
  begin
    FSelEnd.Index := FSelStart.Index + Value.Index;
    FSelEnd.Digit := FSelStart.Digit + Value.Digit;
    if FSelEnd.Digit >= cDigitCount then
      Inc(FSelEnd.Index);
    SelectionChanged(False, False);
    Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetSelStart(Value: TKHexEditorSelection);
begin
  if (Value.Index <> FSelStart.Index) or (Value.Digit <> FSelStart.Digit) then
  begin
    FSelStart := Value;
    SelectionChanged(False, False);
    Invalidate;
  end;
end;

procedure TKCustomHexEditor.SetTopLine(Value: Integer);
begin
  Value := MinMax(Value, 0, GetMaxTopLine);
  if Value <> FTopLine then
    ScrollBy(0, Value - FTopLine, True);
end;

procedure TKCustomHexEditor.SetUndoLimit(Value: Integer);
begin
  Value := MinMax(Value, cUndoLimitMin, cUndoLimitMax);
  if Value <> FUndoList.Limit then
  begin
    FUndoList.Limit := Value;
    FRedoList.Limit := Value;
  end;
end;

procedure TKCustomHexEditor.HideEditorCaret;
var
  P: TPoint;
begin
  P := SelToPoint(FSelEnd, FEditArea);
  HideCaret(Handle);
  {$IFDEF FPC}SetCaretPosEx(Handle,{$ELSE}SetCaretPos({$ENDIF} P.X, P.Y + 1);
end;

procedure TKCustomHexEditor.ShowEditorCaret;
var
  P: TPoint;
begin
  P := SelToPoint(FSelEnd, FEditArea);
  {$IFDEF FPC}SetCaretPosEx(Handle,{$ELSE}SetCaretPos({$ENDIF} P.X, P.Y + 1);
  ShowCaret(Handle);
end;

procedure TKCustomHexEditor.UndoChange(Sender: TObject; ItemReason: TKHexEditorChangeReason);
begin
  if (Sender = FUndoList) and (ItemReason <> crCaretPos) then
    DoChange;
end;

procedure TKCustomHexEditor.UpdateEditorCaret(Recreate: Boolean = False);
var
  CW, CH: Integer;
begin
  Include(FStates, elCaretUpdate);
  try
    if Enabled and Focused and (FEditArea in [eaDigits, eaText]) and not (csDesigning in ComponentState) then
    begin
      if not (elCaretVisible in FStates) or Recreate then
      begin
        if elOverwrite in FStates then
          CW := FCharWidth
        else
          CW := Max(2, (Abs(Font.Height) * 2) div 25);
        if edHorzLines in FDrawStyles then
          CH := FCharHeight - Max(1, FCharHeight div 25)
        else
          CH := FCharHeight;
      {$IFDEF FPC}
        CreateCaret(Handle, 0, CW, CH - 2);
      {$ELSE}
        if CreateCaret(Handle, 0, CW, CH - 2) then
      {$ENDIF}
          Include(FStates, elCaretVisible);
        Invalidate;
      end;
      if elCaretVisible in FStates then
        ShowEditorCaret;
    end else
    begin
      Exclude(FStates, elCaretVisible);
      HideEditorCaret;
    {$IFDEF FPC}
      DestroyCaret(Handle);
    {$ELSE}
      DestroyCaret;
    {$ENDIF}
    end;
  finally
    Exclude(FStates, elCaretUpdate);
  end;
end;

procedure TKCustomHexEditor.UpdateCharMetrics;
var
  DC: HDC;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    FTotalCharSpacing := FCharSpacing * 2;
    // ensure even char spacing because of PointToSel
    if TM.tmAveCharWidth and 1 <> 0 then
      Inc(FTotalCharSpacing);
    FCharWidth := TM.tmAveCharWidth + FTotalCharSpacing;
    FCharHeight := TM.tmHeight * FLineHeightPercent div 100;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TKCustomHexEditor.UpdateMouseCursor;
var
  P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  SetMouseCursor(P.X, P.Y);
end;

procedure TKCustomHexEditor.UpdateScrollRange;
var
  I: Integer;
  AD: TKHexEditorAreaDimensions;
  SI: TScrollInfo;
begin
  if HandleAllocated then
  begin
    AD := GetAreaDimensions;
    // update horizontal scroll position
    I := FLeftChar - GetMaxLeftChar(AD.TotalHorz);
    if I > 0 then
      Dec(FLeftChar, I);
    FLeftChar := Max(FLeftChar, 0);
    // update vertical scroll position
    I := FTopLine - GetMaxTopLine(AD.TotalVert);
    if I > 0 then
      Dec(FTopLine, I);
    FTopLine := Max(FTopLine, 0);
    if FScrollBars in [ssBoth, ssHorizontal, ssVertical] then
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS {$IFDEF UNIX}or SIF_UPDATEPOLICY{$ENDIF};
      SI.nMin := 0;
    {$IFDEF UNIX}
      SI.ntrackPos := SB_POLICY_CONTINUOUS;
    {$ENDIF}
      if FScrollBars in [ssBoth, ssHorizontal] then
      begin
        SI.nMax := AD.TotalHorz{$IFNDEF FPC}- 1{$ENDIF};
        SI.nPage := GetClientWidthChars;
        SI.nPos := FLeftChar;
        SetScrollInfo(Handle, SB_HORZ, SI, True);
        ShowScrollBar(Handle, SB_HORZ, Integer(SI.nPage) < AD.TotalHorz);
      end else
        ShowScrollBar(Handle, SB_HORZ, False);
      if FScrollBars in [ssBoth, ssVertical] then
      begin
        SI.nMax := AD.TotalVert{$IFNDEF FPC}- 1{$ENDIF};
        SI.nPage := GetClientHeightChars;
        SI.nPos := FTopLine;
        SetScrollInfo(Handle, SB_VERT, SI, True);
        ShowScrollBar(Handle, SB_VERT, Integer(SI.nPage) < AD.TotalVert);
      end else
        ShowScrollBar(Handle, SB_VERT, False);
    end;
    UpdateEditorCaret(True);
    Invalidate;
    InvalidatePageSetup;
  end;
end;

procedure TKCustomHexEditor.UpdateSelEnd(Point: TPoint; ClipToClient: Boolean);
var
  R: TRect;
  Sel: TKHexEditorSelection;
begin
  if ClipToClient then
  begin
    R := GetModifiedClientRect;
    Dec(R.Right, FCharWidth);
    Dec(R.Bottom, FCharHeight);
    if CanScroll(ecScrollLeft) and (Point.X < R.Left) then
      Point.X := R.Left
    else if CanScroll(ecScrollRight) and (Point.X > R.Right) then
      Point.X := R.Right;
    if CanScroll(ecScrollUp) and (Point.Y < R.Top) then
      Point.Y := R.Top
    else if CanScroll(ecScrollDown) and (Point.Y > R.Bottom) then
      Point.Y := R.Bottom;
  end;
  Sel := PointToSel(Point, True, FEditArea);
  if (Sel.Index <> cInvalidIndex) and
    ((Sel.Index <> FSelEnd.Index) or (Sel.Digit <> FSelEnd.Digit)) then
  begin
    FSelEnd := Sel;
    UpdateEditorCaret;
    Invalidate;
    InvalidatePageSetup;
  end;
end;

procedure TKCustomHexEditor.UpdateSize;
begin
  UpdateScrollRange;
end;

procedure TKCustomHexEditor.ValidateSelection(var Value: TKHexEditorSelection; Area: TKHexEditorArea);
begin
  if Area <> eaNone then
  begin
    Value.Index := MinMax(Value.Index, 0, FSize);
    if Value.Index = FSize then
      Value.Digit := 0
    else
      Value.Digit := MinMax(Value.Digit, 0, cDigitCount - 1);
  end else
    Value := MakeSelection(cInvalidIndex, 0);
end;

{$IFNDEF FPC}
procedure TKCustomHexEditor.WMDropFiles(var Msg: TLMessage);
var
  I, FileCount: Integer;
  PathName: array[0..260] of Char;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(FOnDropFiles) then
    begin
      FilesList := TStringList.Create;
      try
        FileCount := DragQueryFile(THandle(Msg.wParam), Cardinal(-1), nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);
        for i := 0 to FileCount - 1 do
        begin
          DragQueryFile(THandle(Msg.wParam), I, PathName, SizeOf(PathName));
          FilesList.Add(PathName);
        end;
        FOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;
{$ENDIF}

procedure TKCustomHexEditor.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TKCustomHexEditor.WMGetDlgCode(var Msg: TLMNoParams);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TKCustomHexEditor.WMHScroll(var Msg: TLMHScroll);
begin
  SafeSetFocus;
  ModifyScrollBar(SB_HORZ, Msg.ScrollCode, Msg.Pos, True);
end;

procedure TKCustomHexEditor.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  ExecuteCommand(ecLostFocus);
end;

procedure TKCustomHexEditor.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  ExecuteCommand(ecGotFocus);
end;

procedure TKCustomHexEditor.WMVScroll(var Msg: TLMVScroll);
begin
  SafeSetFocus;
  ModifyScrollBar(SB_VERT, Msg.ScrollCode, Msg.Pos, True);
end;

function GetColorSpec(Index: TKHexEditorColorIndex): TKHexEditorColorSpec;
begin
  case Index of
    ciAddressText: begin Result.Def := cAddressTextDef; Result.Name := sAddressText; end;
    ciAddressBkGnd: begin Result.Def := cAddressBkgndDef; Result.Name := sAddressBkGnd; end;
    ciBkGnd: begin Result.Def := cBkGndDef; Result.Name := sBkGnd; end;
    ciDigitTextEven: begin Result.Def := cDigitTextEvenDef; Result.Name := sDigitTextEven; end;
    ciDigitTextOdd: begin Result.Def := cDigitTextOddDef; Result.Name := sDigitTextOdd; end;
    ciDigitBkGnd: begin Result.Def := cDigitBkGndDef; Result.Name := sDigitBkgnd; end;
    ciHorzLines: begin Result.Def := cHorzLinesDef; Result.Name := sHorzLines; end;
    ciInactiveCaretBkGnd: begin Result.Def := cInactiveCaretBkGndDef; Result.Name := sInactiveCaretBkGnd; end;
    ciInactiveCaretSelBkGnd: begin Result.Def := cInactiveCaretSelBkGndDef; Result.Name := sInactiveCaretSelBkGnd; end;
    ciInactiveCaretSelText: begin Result.Def := cInactiveCaretSelTextDef; Result.Name := sInactiveCaretSelText; end;
    ciInactiveCaretText: begin Result.Def := cInactiveCaretTextDef; Result.Name := sInactiveCaretText; end;
    ciLinesHighLight: begin Result.Def := cLinesHighLightDef; Result.Name := sLinesHighLight; end;
    ciSelBkGnd: begin Result.Def := cSelBkGndDef; Result.Name := sSelBkGnd; end;
    ciSelBkGndFocused: begin Result.Def := cSelBkGndFocusedDef; Result.Name := sSelBkGndFocused; end;
    ciSelText: begin Result.Def := cSelTextDef; Result.Name := sSelText; end;
    ciSelTextFocused: begin Result.Def := cSelTextFocusedDef; Result.Name := sSelTextFocused; end;
    ciSeparators: begin Result.Def := cSeparatorsDef; Result.Name := sSeparators; end;
    ciTextText: begin Result.Def := cTextTextDef; Result.Name := sTextText; end;
    ciTextBkGnd: begin Result.Def := cTextBkgndDef; Result.Name := sTextBkGnd; end;
    ciVertLines: begin Result.Def := cVertLinesDef; Result.Name := sVertLines; end;
  else
    Result.Def := clNone;
    Result.Name := '';
  end;
end;

end.
