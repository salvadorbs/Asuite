{ @abstract(This unit contains the base class for all visible controls.)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(18 Sep 2009)
  @lastmod(20 Jun 2010)

  This unit implements the base class TKCustomControl for all visible controls
  from the KControls Development Suite.

  Copyright © 2009 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KControls;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, KFunctions
{$IFDEF USE_THEMES}
  , Themes
 {$IFNDEF FPC}
  , UxTheme
 {$ENDIF}
{$ENDIF}
  ;

type
  { This array serves as storage place for all colors. }
  TKColorArray = array of TColor;

  { Declares possible indexes e.g. for the @link(TKPreviewColors.Color) property. }
  TKPreviewColorIndex = Integer;

  { Declares print options - possible values for the @link(TKPrintPageSetup.Options) property. }
  TKPrintOption = (
    { If there are more printed copies these will be collated. }
    poCollate,
    { The printed shape will be scaled to fit on page. }
    poFitToPage,
    { Every even page will be printed with mirrored (swapped) margins. }
    poMirrorMargins,
    { Page numbers will be added to the bottom of each printed page. }
    poPageNumbers,
    { Paints the selection in control's specific manner. }
    poPaintSelection,
    { Title will be printed to the top of each printed page. }
    poTitle,
    { Color page will be printed instead of B/W page. }
    poUseColor
  );

  { Print options can be arbitrary combined. }
  TKPrintOptions = set of TKPrintOption;

  { Declares possible values for the @link(TKPrintPageSetup.Range) property. }
  TKPrintRange = (
    { All pages will be printed. }
    prAll,
    { Only selected block will be printed. }
    prSelectedOnly,
    { Only given range of pages will be printed. }
    prRange
  );

  { Declares measurement units for KControls printing system. }
  TKPrintUnits = (
    { Corresponding value is given in millimeters. }
    puMM,
    { Corresponding value is given in centimeters. }
    puCM,
    { Corresponding value is given in inches. }
    puInch,
    { Corresponding value is given in hundredths of inches. }
    puHundredthInch
  );

const
  { Default value for the @link(TKCustomControl.BorderStyle) property. }
  cBorderStyleDef = bsSingle;

  { Minimum for the @link(TKPrintPageSetup.Copies) property }
  cCopiesMin = 1;
  { Maximum for the @link(TKPrintPageSetup.Copies) property }
  cCopiesMax = 1000;
  { Default value for the @link(TKPrintPageSetup.Copies) property }
  cCopiesDef = 1;

  { Default value for the @link(TKPrintPageSetup.MarginBottom) property }
  cMarginBottomDef = 2.0;
  { Default value for the @link(TKPrintPageSetup.MarginLeft) property }
  cMarginLeftDef = 1.5;
  { Default value for the @link(TKPrintPageSetup.MarginRight) property }
  cMarginRightDef = 1.5;
  { Default value for the @link(TKPrintPageSetup.MarginTop) property }
  cMarginTopDef = 1.8;

  { Default value for the @link(TKPrintPageSetup.Options) property. }
  cOptionsDef = [poFitToPage, poPageNumbers, poUseColor];

  { Default value for the @link(TKPrintPageSetup.Options) property. }
  cRangeDef = prAll;

  { Minimum for the @link(TKPrintPageSetup.Scale) property }
  cScaleDef = 100;
  { Maximum for the @link(TKPrintPageSetup.Scale) property }
  cScaleMin = 10;
  { Default value for the @link(TKPrintPageSetup.Scale) property }
  cScaleMax = 500;

  { Default value for the @link(TKPrintPageSetup.Units) property. }
  cUnitsDef = puCM;

  { Default value for the @link(TKPreviewColors.Paper) color property. }
  cPaperDef = clWhite;
  { Default value for the @link(TKPreviewColors.BkGnd) color property. }
  cBkGndDef = clAppWorkSpace;
  { Default value for the @link(TKPreviewColors.Border) color property. }
  cBorderDef = clBlack;
  { Default value for the @link(TKPreviewColors.SelectedBorder) color property. }
  cSelectedBorderDef = clNavy;

  { Index for the @link(TKPreviewColors.Paper) property. }
  ciPaper = TKPreviewColorIndex(0);
  { Index for the @link(TKPreviewColors.BkGnd) property. }
  ciBkGnd = TKPreviewColorIndex(1);
  { Index for the @link(TKPreviewColors.Border) property. }
  ciBorder = TKPreviewColorIndex(2);
  { Index for the @link(TKPreviewColors.SelectedBorder) property. }
  ciSelectedBorder = TKPreviewColorIndex(3);
  { Maximum color array index }
  ciPreviewColorsMax = ciSelectedBorder;

  { Constant for control scrollbars. It means: Leave that scrollbar untouched. }
  cScrollNoAction = -1;

  { Constant for control scrollbars. It means: Use given Delta to update scrollbar. }
  cScrollDelta = -2;

  { Internal flag for TKPrintPreview. }
  cPF_Dragging          = $00000001;
  { Internal flag for TKPrintPreview. }
  cPF_UpdateRange       = $00000002;

type
  { Declares possible values for the @link(ScaleMode) property }
  TKPreviewScaleMode = (
    { Apply scale defined by the @link(Scale) property }
    smScale,
    { Scale the page so that it horizontally fits to the window client area }
    smPageWidth,
    { Scale the page so that it fits to the window client area }
    smWholePage);

  { @abstract(Declares @link(TKPrintPreview.OnChanged) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    </UL>
  }
  TKPreviewChangedEvent = procedure(Sender: TObject) of object;

  { @abstract(Declares the information structure for the @link(TKCustomControl.MeasurePages) method)
    <UL>
    <LH>Members:</LH>
    <LI><I>OutlineWidth</I> - printed outline width (maximum of all pages) in desktop pixels</LI>
    <LI><I>OutlineHeight</I> - printed outline height (maximum of all pages) in desktop pixels</LI>
    <LI><I>HorzPageCount</I> - number of pages to split a wide shape into</LI>
    <LI><I>VertPageCount</I> - number of pages to split a tall shape into</LI>
    <LI><I>PageCount</I> - total number of pages for 1 copy. Might be HorzPageCount * VertPageCount 
      but does not necessarilly have to be. </LI>
    </UL>
  }
  TKPrintMeasureInfo = record
    OutlineWidth: Integer;
    OutlineHeight: Integer;
    HorzPageCount: Integer;
    VertPageCount: Integer;
    PageCount: Integer;
  end;

  { Declares possible values for the Status parameter in the @link(TKPrintNotifyEvent) event }
  TKPrintStatus = (
    { This event occurs at the beginning of the print job - you may show an Abort dialog here }
    epsBegin,
    { This event occurs after each page has been printed - you may update the Page/Copy information
      in the Abort dialog }
    epsNewPage,
    { This event occurs at the end of the print job - you may hide the Abort dialog here }
    epsEnd
  );

  { @abstract(Declares @link(TKCustomControl.OnPrintNotify) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>Status</I> - specifies the event type</LI>
    <LI><I>Abort</I> - set to True to abort the print job</LI>
    </UL>
    Remark: At certain time slots, the print spooler allows the message queue
    to be processed for the thread where the print job is running. This e.g. allows
    the user to press a button on the Abort dialog. Because this message loop can be invoked
    e.g. during a Printer.Canvas.TextRect function and any painting messages may hover in
    the message queue, any functions used both to print a job and to process particular
    messages should be reentrant to avoid conflicts. Perhaps should print jobs be run
    in seperate threads?
  }
  TKPrintNotifyEvent = procedure(Sender: TObject; Status: TKPrintStatus;
    var Abort: Boolean) of object;

  { @abstract(Declares @link(TKCustomControl.OnPrintPaint) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    </UL>
  }
  TKPrintPaintEvent = procedure(Sender: TObject) of object;

  TKPrintPageSetup = class;
  TKPrintPreview = class;

  { Base class for all visible controls in KControls. }
  TKCustomControl = class(TCustomControl)
  private
  {$IFNDEF FPC}
    FBorderStyle: TBorderStyle;
  {$ENDIF}
  {$IFNDEF COMPILER10_UP}
    FMouseInClient: Boolean;
  {$ENDIF}
    FMemoryCanvas: TCanvas;
    FMemoryCanvasRect: TRect;
    FPageSetup: TKPrintPageSetup;
    FUpdateLock: Integer;
    FOnPrintNotify: TKPrintNotifyEvent;
    FOnPrintPaint: TKPrintPaintEvent;
  {$IFNDEF FPC}
    procedure CMCancelMode(var Msg: TMessage); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
  {$ENDIF}
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
    function GetCanPrint: Boolean;
    function GetPageSetup: TKPrintPageSetup;
    function GetPageSetupAllocated: Boolean;
    procedure KMLateUpdate(var Msg: TLMessage); message KM_LATEUPDATE;
  {$IFNDEF FPC}
    procedure SetBorderStyle(Value: TBorderStyle);
  {$ENDIF}
    procedure SetPageSetup(Value: TKPrintPageSetup);
  {$IFNDEF FPC}
    procedure WMCancelMode(var Msg: TWMCancelMode); message WM_CANCELMODE;
  {$ENDIF}
  {$IFNDEF COMPILER10_UP}
    procedure WMMouseLeave(var Msg: TLMessage); message KM_MOUSELEAVE;
  {$ENDIF}
  {$IFNDEF FPC}
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
  {$ENDIF}
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
  {$IFNDEF FPC}
   {$IFDEF USE_THEMES}
    procedure WMThemeChanged(var Msg: TMessage); message WM_THEMECHANGED;
   {$ENDIF}
  {$ENDIF}
  protected
    { Holds the mutually inexclusive state as cXF... flags. }
    FFlags: Cardinal;
    { Defines the message queue for late update. }
    FMessages: array of TLMessage;
    { Gains access to the list of associated previews. }
    FPreviewList: TList;
    { Adds a preview control to the internal list of associated previews. }
    procedure AddPreview(APreview: TKPrintPreview);
    { Gives the descendant the possibility to adjust the associated TKPrintPageSetup
      instance just before printing. }
    procedure AdjustPageSetup; virtual;
    { Cancels any dragging or resizing operations performed by mouse. }
    procedure CancelMode; virtual;
    { Defines additional styles. }
    procedure CreateParams(var Params: TCreateParams); override;
  {$IFDEF FPC}
    { Overriden method. Calls @link(TKCustomControl.UpdateSize). }
    procedure CreateWnd; override;
    { Overriden method. Calls @link(TKCustomControl.UpdateSize). }
    procedure DoOnChangeBounds; override;
  {$ENDIF}
    { If Value is True, includes the flag specified by AFLag to @link(FFlags).
      If Value is False, excludes the flag specified by AFLag from @link(FFlags). }
    procedure FlagAssign(AFlag: Cardinal; Value: Boolean);
    { Excludes the flag specified by AFLag from @link(FFlags). }
    procedure FlagClear(AFlag: Cardinal);
    { Includes the flag specified by AFLag to @link(FFlags). }
    procedure FlagSet(AFlag: Cardinal);
    { If the flag specified by AFLag is included in @link(FFlags), FlagToggle
      excludes it and vice versa. }
    procedure FlagToggle(AFlag: Cardinal);
    { Invalidates the page setup settings. If page setup is required again,
      it's UpdateSettings method is called. }
    procedure InvalidatePageSetup;
    { Invalidates a rectangular part of the client area if control updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure InvalidateRectArea(const R: TRect); virtual;
    { Returns True if the control has a selection. }
    function InternalGetSelAvail: Boolean; virtual;
    { Called in UnlockUpdate. Allows the changes to be reflected. }
    procedure InternalUnlockUpdate; virtual;
    { Determines if control can be painted with OS themes. }
    function IsThemed: Boolean; virtual;
    { Called from KM_LATEUPDATE. Performs late update. Override to adapt. }
    procedure LateUpdate(var Msg: TLMessage); virtual;
    { Updates information about printed shape. }
    procedure MeasurePages(var Info: TKPrintMeasureInfo); virtual;
    { Retrieves a message from message queue if there is one. Used for late update.}
    function MessagePeek(out Msg: TLMessage): Boolean;
    { Puts a new message into the message queue. Used for late update.}
    procedure MessagePoke(const Msg: TLMessage);
    { Searches the message queue for given message code. }
    function MessageSearch(MsgCode: Cardinal): Boolean;
    { Responds to WM_MOUSELEAVE message. }
    procedure MouseFormLeave; virtual;
    { Overriden method - see Delphi help. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Notifies all associated previews about a change in the associated page setup. }
    procedure NotifyPreviews;
    { Overriden method - see Delphi help. Paints the entire control client area. }
    procedure Paint; override;
    { Paints a page to a printer/preview canvas. }
    procedure PaintPage; virtual;
    { Paints the control to the specified canvas. Must always be overriden. }
    procedure PaintToCanvas(ACanvas: TCanvas); virtual; abstract;
    { Adds a message to message queue for late update. Set IfNotExists to True to
      add that message only if the specified message code does not exist in the
      message queue at this moment. }
    procedure PostLateUpdate(const Msg: TLMessage; IfNotExists: Boolean = False);
    { Calls the @link(TKCustomControl.OnPrintNotify) event }
    procedure PrintNotify(Status: TKPrintStatus; var Abort: Boolean); virtual;
    { Calls the @link(TKCustomControl.OnPrintPaint) event }
    procedure PrintPaint; virtual;
    { Removse a preview control to the internal list of associated previews. }
    procedure RemovePreview(APreview: TKPrintPreview);
    { Updates mouse cursor according to the state determined from current mouse
      position. Returns True if cursor has been changed. }
    function SetMouseCursor(X, Y: Integer): Boolean; virtual;
    { Updates the control size. Responds to WM_SIZE under Delphi and similar
      notifications under Lazarus. }
    procedure UpdateSize; virtual;
  public
    { Creates the instance. Assigns default values to properties, allocates
      default column, row and cell data. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the instance along with all allocated column, row and cell data.
      See TObject.Destroy in Delphi help. }
    destructor Destroy; override;
    { Determines whether a flag specified by AFlag is included in @link(FFlags). }
    function Flag(AFlag: Cardinal): Boolean;
    { Invalidates the entire control if control updating is not locked
      by @link(TKCustomControl.LockUpdate). }
    procedure Invalidate; override;
    { Locks control updating so that all possibly slow operations such as all Invalidate...
      methods will not be performed. This is useful e.g. when assigning many
      properties at one time. Every LockUpdate call must have
      a corresponding @link(TKCustomControl.UnlockUpdate) call, please use a
      try-finally section. }
    procedure LockUpdate;
    { Prints the control. }
    procedure PrintOut;
    { Unlocks back to normal control updating and calls InternalUnlockUpdate
      to reflect (possible) multiple changes made. Each @link(LockUpdate) call must
      be always followed by the UnlockUpdate call. }
    procedure UnlockUpdate;
    { Returns True if control updating is not locked, i.e. there is no open
      LockUpdate and UnlockUpdate pair. }
    function UpdateUnlocked: Boolean;
    { Determines whether a single line border is drawn around the control.
      Set BorderStyle to bsSingle to add a single line border around the control.
      Set BorderStyle to bsNone to omit the border. }
  {$IFDEF FPC}
    property BorderStyle default cBorderStyleDef;
  {$ELSE}
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default cBorderStyleDef;
  {$ENDIF}
    { Returns True if the control has anything to print and a printer is installed. }
    property CanPrint: Boolean read GetCanPrint;
  {$IFNDEF COMPILER10_UP}
    { This property has the same meaning as the MouseInClient property introduced
      into TWinControl in BDS 2006. }
    property MouseInClient: Boolean read FMouseInClient;
  {$ENDIF}
    { Setting this property causes the control to be painted to MemoryCanvas in it's
      Paint method. This approach replaces PaintTo as it does not work good for all
      LCL widget sets. The control is painted normally on it's Canvas and then
      copied only once to MemoryCanvas. MemoryCanvas is then set to nil (not freed)
      to indicate the copying is complete. }
    property MemoryCanvas: TCanvas read FMemoryCanvas write FMemoryCanvas;
    { Specifies what rectangular part of the control should be copied on MemoryCanvas. }
    property MemoryCanvasRect: TRect read FMemoryCanvasRect write FMemoryCanvasRect;
    { This event is called at certain phases of the actually running print job. }
    property OnPrintNotify: TKPrintNotifyEvent read FOnPrintNotify write FOnPrintNotify;
    { This event is called after the shape was drawn onto the printer canvas. }
    property OnPrintPaint: TKPrintPaintEvent read FOnPrintPaint write FOnPrintPaint;
    { Specifies the page setup component used for this control. }
    property PageSetup: TKPrintPageSetup read GetPageSetup write SetPageSetup;
    {Returns True if page setup component is allocated for this control. }
    property PageSetupAllocated: Boolean read GetPageSetupAllocated;
  end;

  { @abstract(Class to specify the print job parameters) }
  TKPrintPageSetup = class(TPersistent)
  private
    FActive: Boolean;
    FCanvas: TCanvas;
    FControl: TKCustomControl;
    FCopies: Integer;
    FCurrentCopy: Integer;
    FCurrentPage: Integer;
    FCurrentScale: Double;
    FDesktopPixelsPerInchX: Integer;
    FDesktopPixelsPerInchY: Integer;
    FEndPage: Integer;
    FFooterSpace: Double;
    FHeaderSpace: Double;
    FHorzPageCount: Integer;
    FIsValid: Boolean;
    FMarginBottom: Double;
    FMarginLeft: Double;
    FMarginRight: Double;
    FMarginTop: Double;
    FOptions: TKPrintOptions;
    FOutlineHeight: Integer;
    FOutlineWidth: Integer;
    FPageCount: Integer;
    FPageHeight: Integer;
    FPageWidth: Integer;
    FPaintAreaHeight: Integer;
    FPaintAreaWidth: Integer;
    FPreviewing: Boolean;
    FPrinterFooterSpace: Integer;
    FPrinterHeaderSpace: Integer;
    FPrinterMarginBottom: Integer;
    FPrinterMarginLeft: Integer;
    FPrinterMarginLeftMirrored: Integer;
    FPrinterMarginRight: Integer;
    FPrinterMarginRightMirrored: Integer;
    FPrinterMarginTop: Integer;
    FPrinterName: string;
    FPrinterPixelsPerInchX: Integer;
    FPrinterPixelsPerInchY: Integer;
    FPrintingMapped: Boolean;
    FRange: TKPrintRange;
    FStartPage: Integer;
    FScale: Integer;
    FTitle: string;
    FUnits: TKPrintUnits;
    FUpdateLock: Integer;
    FValidating: Boolean;
    FVertPageCount: Integer;
    function GetCanPrint: Boolean;
    procedure SetCopies(Value: Integer);
    procedure SetEndPage(Value: Integer);
    procedure SetFooterSpace(Value: Double);
    procedure SetHeaderSpace(Value: Double);
    procedure SetMarginBottom(Value: Double);
    procedure SetMarginLeft(Value: Double);
    procedure SetMarginRight(Value: Double);
    procedure SetMarginTop(Value: Double);
    procedure SetOptions(Value: TKPrintOptions);
    procedure SetPrinterName(const Value: string);
    procedure SetPrintingMapped(Value: Boolean);
    procedure SetRange(Value: TKPrintRange);
    procedure SetScale(Value: Integer);
    procedure SetStartPage(Value: Integer);
    procedure SetUnits(Value: TKPrintUnits);
    function GetSelAvail: Boolean;
  protected
    { Called before new Units are set. Converts the margins to inches by default. }
    procedure AfterUnitsChange; virtual;
    { Called after new Units are set. Converts the margins from inches by default. }
    procedure BeforeUnitsChange; virtual;
    { Paints a page to APreview.Canvas. }
    procedure PaintPageToPreview(APreview: TKPrintPreview); virtual;
    { Prints the page number at the bottom of the page, horizontally centered. }
    procedure PrintPageNumber(Value: Integer); virtual;
    { Prints the title at the top of the page. }
    procedure PrintTitle; virtual;
    { Updates entire printing information. }
    procedure UpdateSettings; virtual;
  public
    { Creates the instance. Assigns default values to properties. }
    constructor Create(AControl: TKCustomControl);
    { Copies shareable properties of another TKPrintPageSetup instance
      to this instance. }
    procedure Assign(Source: TPersistent); override;
    { Returns a value mapped from desktop horizontal units to printer horizontal units. }
    function HMap(Value: Integer): Integer;
    { Invalidates the settings. }
    procedure Invalidate;
    { Prints the associated control. }
    procedure PrintOut;
    { Locks page setup updating. Use this if you assign many properties at the
      same time. Every LockUpdate call must have a corresponding
      @link(TKPrintPageSetup.UnlockUpdate) call, please use a try-finally section. }
    procedure LockUpdate; virtual;
    { Unlocks page setup updating and updates the page settings.
      Each @link(TKPrintPageSetup.LockUpdate) call must be always followed
      by the UnlockUpdate call. }
    procedure UnlockUpdate; virtual;
    { Returns True if updating is not locked, i.e. there is no open
      LockUpdate and UnlockUpdate pair. }
    function UpdateUnlocked: Boolean; virtual;
    { Validates the settings. }
    procedure Validate;
    { Returns a value mapped from desktop vertical units to printer vertical units. }
    function VMap(Value: Integer): Integer;
    { Returns True if printing or previewing is active. }
    property Active: Boolean read FActive;
    { Returns True if the control is associated and has anything to print. }
    property CanPrint: Boolean read GetCanPrint;
    { Returns the Printer.Canvas or TkPrintPreview.Canvas. Do not access outside
      print job. }
    property Canvas: TCanvas read FCanvas;
    { Returns the control to which this TKPrintPageSetup instance is assigned. }
    property Control: TKCustomControl read FControl;
    { Specifies the number of copies to print. }
    property Copies: Integer read FCopies write SetCopies;
    { Returns the currently printed copy. }
    property CurrentCopy: Integer read FCurrentCopy;
    { Returns the currently printed page. }
    property CurrentPage: Integer read FCurrentPage;
    { Returns the horizontal scale for the printed shape, without dimension. }
    property CurrentScale: Double read FCurrentScale;
    { Returns the amount of pixels per inch for the desktop device context's horizontal axis }
    property DesktopPixelsPerInchX: Integer read FDesktopPixelsPerInchX;
    { Returns the amount of pixels per inch for the desktop device context's vertical axis }
    property DesktopPixelsPerInchY: Integer read FDesktopPixelsPerInchY;
    { Specifies last page printed if Range is eprRange. }
    property EndPage: Integer read FEndPage write SetEndPage;
    { Specifies the vertical space that should stay free for application
      specific footer. Value is given in Units. }
    property FooterSpace: Double read FFooterSpace write SetFooterSpace;
    { Specifies the vertical space that should stay free for application
      specific header. Value is given in Units. }
    property HeaderSpace: Double read FHeaderSpace write SetHeaderSpace;
    { Returns the maximum amount of pages for horizontal axis of the control. }
    property HorzPageCount: Integer read FHorzPageCount;
    { Specifies the bottom margin. Value is given in Units. }
    property MarginBottom: Double read FMarginBottom write SetMarginBottom;
    { Specifies the left margin. Value is given in Units. }
    property MarginLeft: Double read FMarginLeft write SetMarginLeft;
    { Specifies the right margin. Value is given in Units. }
    property MarginRight: Double read FMarginRight write SetMarginRight;
    { Specifies the top margin. Value is given in Units. }
    property MarginTop: Double read FMarginTop write SetMarginTop;
    { Specifies the printing options. }
    property Options: TKPrintOptions read FOptions write SetOptions;
    { Returns the printed shape height (maximum of all pages)
      in units depending on PrintingMapped.. }
    property OutlineHeight: Integer read FOutlineHeight;
    { Returns the printed shape width (maximum of all pages)
      in units depending on PrintingMapped.. }
    property OutlineWidth: Integer read FOutlineWidth;
    { Returns the amount of all pages. }
    property PageCount: Integer read FPageCount;
    { Returns the page height in printer device context's pixels. }
    property PageHeight: Integer read FPageHeight;
    { Returns the page width in printer device context's pixels. }
    property PageWidth: Integer read FPageWidth;
    { Returns the top paint area width on canvas in units depending on PrintingMapped. }
    property PaintAreaHeight: Integer read FPaintAreaHeight;
    { Returns the top paint area width on canvas in units depending on PrintingMapped. }
    property PaintAreaWidth: Integer read FPaintAreaWidth;
    { Returns True if painting to a TKPrintPreview.Canvas is active. }
    property Previewing: Boolean read FPreviewing;
    { Returns the footer space in printer device context's units. }
    property PrinterFooterSpace: Integer read FPrinterFooterSpace;
    { Returns the header space in printer device context's units. }
    property PrinterHeaderSpace: Integer read FPrinterHeaderSpace;
    { Returns the bottom margin in printer device context's units. }
    property PrinterMarginBottom: Integer read FPrinterMarginBottom;
    { Returns the left margin in printer device context's units. }
    property PrinterMarginLeft: Integer read FPrinterMarginLeft;
    { Returns the left margin in printer device context's units with respect to current page. }
    property PrinterMarginLeftMirrored: Integer read FPrinterMarginLeftMirrored;
    { Returns the right margin in printer device context's units. }
    property PrinterMarginRight: Integer read FPrinterMarginRight;
    { Returns the left margin in printer device context's units with respect to current page. }
    property PrinterMarginRightMirrored: Integer read FPrinterMarginRightMirrored;
    { Returns the top margin in printer device context's units. }
    property PrinterMarginTop: Integer read FPrinterMarginTop;
    { Specifies the printer name. }
    property PrinterName: string read FPrinterName write SetPrinterName;
    { Returns the amount of pixels per inch for the printer device context's horizontal axis }
    property PrinterPixelsPerInchX: Integer read FPrinterPixelsPerInchX;
    { Returns the amount of pixels per inch for the printer device context's vertical axis }
    property PrinterPixelsPerInchY: Integer read FPrinterPixelsPerInchY;
    { Specifies the units for printing the control's shape and OutlineX properties.
      If True, those extents are given in printer device context's pixels,
      otherwise in desktop device context's pixels. It can be adjusted by the descendant
      in the AdjustPageSetup method. }
    property PrintingMapped: Boolean read FPrintingMapped write SetPrintingMapped;
    { Specifies the printing range. }
    property Range: TKPrintRange read FRange write SetRange;
    { Returns True if the associated control has a selection. }
    property SelAvail: Boolean read GetSelAvail;
    { Specifies first page printed if Range is eprRange. }
    property StartPage: Integer read FStartPage write SetStartPage;
    { Specifies the requested scale for the printed shape, in percent.
      If epoFitToPage is specified in Options, this parameter is ignored. }
    property Scale: Integer read FScale write SetScale;
    { Specifies the document title as it appears in printer manager. }
    property Title: string read FTitle write FTitle;
    { Specifies the units for print margins. }
    property Units: TKPrintUnits read FUnits write SetUnits;
    { Returns the maximum amount of pages for vertical axis of the control. }
    property VertPageCount: Integer read FVertPageCount;
  end;

  { @abstract(Container for all colors used by @link(TKPrintPreview) class)
    This container allows to group many colors into one item in object inspector.
    Colors are accessible via published properties or several public Color*
    properties. }
  TKPreviewColors = class(TPersistent)
  private
    FPreview: TKPrintPreview;
    function GetColor(Index: TKPreviewColorIndex): TColor;
    function GetColorEx(Index: TKPreviewColorIndex): TColor;
    procedure SetColor(Index: TKPreviewColorIndex; Value: TColor);
    procedure SetColorEx(Index: TKPreviewColorIndex; Value: TColor);
    procedure SetColors(const Value: TKColorArray);
  protected
    FColors: TKColorArray;
    { Initializes the color array. }
    procedure Initialize; virtual;
    { Returns the specific color according to ColorScheme. }
    function InternalGetColor(Index: TKPreviewColorIndex): TColor; virtual;
    { Replaces the specific color. }
    procedure InternalSetColor(Index: TKPreviewColorIndex; Value: TColor); virtual;
  public
    { Creates the instance. You can create a custom instance and pass it
      e.g. to a @link(TKPrintPreview.Colors) property. The APreview parameter has no meaning
      in this case and you may set it to nil. }
    constructor Create(APreview: TKPrintPreview);
    { Copies the properties of another instance that inherits from
      TPersistent into this TKPreviewColors instance. }
    procedure Assign(Source: TPersistent); override;
    { Returns color for given index. }
    property Color[Index: TKPreviewColorIndex]: TColor read GetColorEx write SetColorEx;
    { Returns array of colors. }
    property Colors: TKColorArray read FColors write SetColors;
  published
    { Specifies the paper background color. }
    property Paper: TColor index ciPaper read GetColor write SetColor default cPaperDef;
    { Specifies the color of the background around paper. }
    property BkGnd: TColor index ciBkGnd read GetColor write SetColor default cBkGndDef;
    { Specifies the color of the paper border. }
    property Border: TColor index ciBorder read GetColor write SetColor default cBorderDef;
    { Specifies the color of the paper border when the control has input focus. }
    property SelectedBorder: TColor index ciSelectedBorder read GetColor write SetColor default cSelectedBorderDef;
  end;

  { @abstract(Print preview control for the TKCustomControl component) }
  TKPrintPreview = class(TKCustomControl)
  private
    FColors: TKPreviewColors;
    FControl: TKCustomControl;
    FMouseWheelAccumulator: Integer;
    FPage: Integer;
    FPageOld: Integer;
    FPageSize: TPoint;
    FExtent: TPoint;
    FPageOffset: TPoint;
    FScale: Integer;
    FScaleMode: TKPreviewScaleMode;
    FScrollExtent: TPoint;
    FScrollPos: TPoint;
    FScrollPosOld: TPoint;
    FX: Integer;
    FY: Integer;
    FOnChanged: TKPreviewChangedEvent;
    function GetCurrentScale: Integer;
    function GetEndPage: Integer;
    function GetStartPage: Integer;
    procedure SetControl(Value: TKCustomControl);
    procedure SetPage(Value: Integer);
    procedure SetScale(Value: Integer);
    procedure SetScaleMode(Value: TKPreviewScaleMode);
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure SetColors(const Value: TKPreviewColors);
  protected
    { Initializes a scroll message handling. }
    procedure BeginScrollWindow;
    { Defines additional styles. }
    procedure CreateParams(var Params: TCreateParams); override;
    { Overriden method - handles mouse wheel messages. }
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    { Calls the ScrollWindowEx function to complete a scroll message. }
    procedure EndScrollWindow;
    { Returns current page rectangle inside of the window client area. }
    function GetPageRect: TRect;
    { Processes virtual key strokes. }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Processes scrollbar messages.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ScrollBar</I> - scrollbar type from OS</LI>
      <LI><I>ScrollCode</I> - scrollbar action from OS</LI>
      <LI><I>Delta</I> - scrollbar position change</LI>
      </UL> }
    procedure ModifyScrollBar(ScrollBar, ScrollCode, Delta: Integer);
    { Initializes drag&scroll functionality. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Performs drag&scroll functionality. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Finalizes drag&scroll functionality. }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Notifies about associated TKCustomControl control removal. }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Paints paper and control shape. }
    procedure Paint; override;
    { Calls the @link(OnChanged) event. }
    procedure Changed;
    { Grants the input focus to the control when possible and the control has had none before. }
    procedure SafeSetFocus;
    { Updates mouse cursor. }
    function SetMouseCursor(X, Y: Integer): Boolean; override;
    { Updates page sizes and scrollbar ranges. }
    procedure UpdateScrollRange;
    { Updates the control size. }
    procedure UpdateSize; override;
  public
    { Performs necessary initializations - default values to properties. }
    constructor Create(AOwner: TComponent); override;
    { Destroy instance... }
    destructor Destroy; override;
    { Shows first page for the given range. }
    procedure FirstPage;
    { Shows last page for the given range. }
    procedure LastPage;
    { Shows next page. }
    procedure NextPage;
    { Shows previous page. }
    procedure PreviousPage;
    { Updates the preview. }
    procedure UpdatePreview;
    { Returns the page scaling with regard to the @link(ScaleMode) property. }
    property CurrentScale: Integer read GetCurrentScale;
    { Returns the current page area rectangle in desktop pixels. }
    property PageRect: TRect read GetPageRect;
    { Returns the last page for the given range. }
    property EndPage: Integer read GetEndPage;
    { Returns the first page for the given range. }
    property StartPage: Integer read GetStartPage;
  published
    { Inherited property - see Delphi help. }
    property Align;
    { Inherited property - see Delphi help. }
    property Anchors;
    { See TKCustomControl.@link(TKCustomControl.BorderStyle) for details. }
    property BorderStyle;
    { Inherited property - see Delphi help. }
    property BorderWidth;
    { Specifies all colors used by TKPrintPreview's default painting. }
    property Colors: TKPreviewColors read FColors write SetColors;
    { Inherited property - see Delphi help. }
    property Constraints;
    { Specifies the associated control. }
    property Control: TKCustomControl read FControl write SetControl;
    { Inherited property - see Delphi help. }
    property DragCursor;
    { Inherited property - see Delphi help. }
    property DragKind;
    { Inherited property - see Delphi help. }
    property DragMode;
    { Specifies the currently displayed page. }
    property Page: Integer read FPage write SetPage default 1;
    { Inherited property - see Delphi help. }
    property ParentShowHint;
    { Inherited property - see Delphi help. }
    property PopupMenu;
    { Specifies the user defined page scale - i.e. when ScaleMode = smScale. }
    property Scale: Integer read FScale write SetScale default 100;
    { Specifies the scale mode to display and scroll previewed pages. }
    property ScaleMode: TKPreviewScaleMode read FScaleMode write SetScaleMode default smPageWidth;
    { Inherited property - see Delphi help. }
    property ShowHint;
    { Inherited property - see Delphi help. }
    property TabStop;
    { Inherited property - see Delphi help. }
    property TabOrder;
    { Inherited property - see Delphi help. }
    property Visible;
    { Called whenever print preview is updated. }
    property OnChanged: TKPreviewChangedEvent read FOnChanged write FOnChanged;
    { Inherited property - see Delphi help. }
    property OnClick;
    { Inherited property - see Delphi help. }
    property OnContextPopup;
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
    { Inherited property - see Delphi help. }
    property OnEndDock;
    { Inherited property - see Delphi help. }
    property OnEndDrag;
    { Inherited property - see Delphi help. }
    property OnEnter;
    { Inherited property - see Delphi help. }
    property OnExit;
    { Inherited property - see Delphi help. }
    property OnGetSiteInfo;
    { Inherited property - see Delphi help. }
    property OnKeyDown;
    { Inherited property - see Delphi help. }
    property OnKeyPress;
    { Inherited property - see Delphi help. }
    property OnKeyUp;
    { Inherited property - see Delphi help. }
    property OnMouseDown;
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
    { Inherited property - see Delphi help. }
    property OnStartDock;
    { Inherited property - see Delphi help. }
    property OnStartDrag;
    { Inherited property - see Delphi help. }
    property OnUnDock;
  end;

{ Converts a value given in inches into a value given in specified units.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Units</I> - measurement units for the output value</LI>
  <LI><I>Value</I> - input value to convert</LI>
  </UL> }
function InchesToValue(Units: TKPrintUnits; Value: Double): Double;

{ Converts value given in specified units into a value given in inches.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Units</I> - measurement units for the input value</LI>
  <LI><I>Value</I> - input value to convert</LI>
  </UL> }
function ValueToInches(Units: TKPrintUnits; Value: Double): Double;

implementation

uses
  Math, Printers, KGraphics;

const
  cPreviewHorzBorder = 30;
  cPreviewVertBorder = 30;
  cPreviewShadowSize = 3;

function InchesToValue(Units: TKPrintUnits; Value: Double): Double;
begin
  case Units of
    puMM: Result := Value * 25.4;
    puCM: Result := Value * 2.54;
    puHundredthInch: Result := Value * 100;
  else
    Result := Value;
  end;
end;

function ValueToInches(Units: TKPrintUnits; Value: Double): Double;
begin
  case Units of
    puMM: Result := Value / 25.4;
    puCM: Result := Value / 2.54;
    puHundredthInch: Result := Value / 100;
  else
    Result := Value;
  end;
end;

{ TKCustomControl }

constructor TKCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := cBorderStyleDef;
  FFlags := 0;
  FMemoryCanvas := nil;
  FMessages := nil;
{$IFNDEF COMPILER10_UP}
  FMouseInClient := False;
{$ENDIF}
  FPageSetup := nil;
  FPreviewList := TList.Create;
  FUpdateLock := 0;
  FOnPrintNotify := nil;
  FOnPrintPaint := nil;
end;

destructor TKCustomControl.Destroy;
begin
  inherited;
  FMessages := nil;
  FreeAndNil(FPreviewList);
  FreeAndNil(FPageSetup);
end;

procedure TKCustomControl.AddPreview(APreview: TKPrintPreview);
begin
  if Assigned(APreview) then
    FPreviewList.Add(APreview);
end;

procedure TKCustomControl.AdjustPageSetup;
begin
end;

procedure TKCustomControl.CancelMode;
begin
end;

{$IFNDEF FPC}
procedure TKCustomControl.CMCancelMode(var Msg: TLMessage);
begin
  inherited;
  CancelMode;
end;

procedure TKCustomControl.CMCtl3DChanged(var Msg: TLMessage);
begin
  inherited;
  RecreateWnd;
end;
{$ENDIF}

procedure TKCustomControl.CMMouseLeave(var Msg: TLMessage);
begin
  inherited;
  try
    MouseFormLeave;
  except
  end;
end;

procedure TKCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
{$IFNDEF FPC}
  with Params do
  begin
    WindowClass.style := CS_DBLCLKS;
    if BorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
{$ENDIF}
end;

{$IFDEF FPC}
procedure TKCustomControl.CreateWnd;
begin
  inherited;
  UpdateSize;
end;

procedure TKCustomControl.DoOnChangeBounds;
begin
  inherited;
  UpdateSize;
end;
{$ENDIF}

function TKCustomControl.Flag(AFlag: Cardinal): Boolean;
begin
  Result := FFlags and AFlag <> 0;
end;

procedure TKCustomControl.FlagAssign(AFlag: Cardinal; Value: Boolean);
begin
  if Value then
    FlagSet(AFlag)
  else
    FlagClear(AFlag);
end;

procedure TKCustomControl.FlagClear(AFlag: Cardinal);
begin
  FFlags := FFlags and not AFlag;
end;

procedure TKCustomControl.FlagSet(AFlag: Cardinal);
begin
  FFlags := FFlags or AFlag;
end;

procedure TKCustomControl.FlagToggle(AFlag: Cardinal);
begin
  FFlags := FFlags xor AFlag;
end;

function TKCustomControl.GetCanPrint: Boolean;
begin
  Result := PageSetup.CanPrint;
end;

function TKCustomControl.GetPageSetup: TKPrintPageSetup;
begin
  if not Assigned(FPageSetup) and not (csDestroying in ComponentState) then
  begin
    FPageSetup := TKPrintPageSetup.Create(Self);
    AdjustPageSetup;
  end;
  if Assigned(FPageSetup) then
    FPageSetup.Validate;
  Result := FPageSetup;
end;

function TKCustomControl.GetPageSetupAllocated: Boolean;
begin
  Result := Assigned(FPageSetup);
end;

function TKCustomControl.InternalGetSelAvail: Boolean;
begin
  Result := False;
end;

procedure TKCustomControl.InternalUnlockUpdate;
begin
end;

procedure TKCustomControl.Invalidate;
begin
  if UpdateUnlocked and HandleAllocated then
    inherited;
end;

procedure TKCustomControl.InvalidatePageSetup;
begin
  if Assigned(FPageSetup) then
    FPageSetup.Invalidate;
end;

procedure TKCustomControl.InvalidateRectArea(const R: TRect);
begin
  if UpdateUnlocked and HandleAllocated then
    InvalidateRect(Handle, @R, False);
end;

function TKCustomControl.IsThemed: Boolean;
begin
  Result := True;
end;

procedure TKCustomControl.KMLateUpdate(var Msg: TLMessage);
var
  M: TLMessage;
begin
  if MessagePeek(M) then
    LateUpdate(M);
end;

procedure TKCustomControl.LateUpdate(var Msg: TLMessage);
begin
  case Msg.Msg of
    LM_SIZE: UpdateSize;
  end;
end;

procedure TKCustomControl.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TKCustomControl.MeasurePages(var Info: TKPrintMeasureInfo);
begin
end;

function TKCustomControl.MessagePeek(out Msg: TLMessage): Boolean;
var
  ALen: Integer;
begin
  ALen := Length(FMessages);
  if ALen > 0 then
  begin
    Dec(ALen);
    Msg := FMessages[ALen];
    SetLength(FMessages, ALen);
    Result := True;
  end else
    Result := False;
end;

procedure TKCustomControl.MessagePoke(const Msg: TLMessage);
var
  ALen: Integer;
begin
  ALen := Length(FMessages);
  SetLength(FMessages, ALen + 1);
  FMessages[ALen] := Msg;
end;

function TKCustomControl.MessageSearch(MsgCode: Cardinal): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FMessages) - 1 do
    if FMessages[I].Msg = MsgCode then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TKCustomControl.MouseFormLeave;
begin
end;

procedure TKCustomControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
{$IFNDEF COMPILER10_UP}
  CallTrackMouseEvent(Self, FMouseInClient);
{$ENDIF}
{$IFDEF FPC}
  if not MouseCapture then
    SetMouseCursor(X, Y);
{$ENDIF}
end;

procedure TKCustomControl.NotifyPreviews;
var
  I: Integer;
begin
  for I := 0 to FPreviewList.Count - 1 do
    TKPrintPreview(FPreviewList[I]).UpdatePreview;
end;

procedure TKCustomControl.Paint;
begin
  PaintToCanvas(Canvas);
  if Assigned(FMemoryCanvas) then
  begin
  {$IFDEF USE_WINAPI}
    // this is the best method but does not work both on QT and GTK!
    MoveWindowOrg(FMemoryCanvas.Handle, -FMemoryCanvasRect.Left, -FMemoryCanvasRect.Top);
    try
      PaintToCanvas(FMemoryCanvas);
    finally
      MoveWindowOrg(FMemoryCanvas.Handle, FMemoryCanvasRect.Left, FMemoryCanvasRect.Top);
    end;
  {$ELSE}
    FMemoryCanvas.CopyRect(Rect(0, 0, FMemoryCanvasRect.Right - FMemoryCanvasRect.Left,
      FMemoryCanvasRect.Bottom - FMemoryCanvasRect.Top), Canvas, FMemoryCanvasRect);
  {$ENDIF}
    FMemoryCanvas := nil;
  end;
end;

procedure TKCustomControl.PostLateUpdate(const Msg: TLMessage;
  IfNotExists: Boolean);
begin
  if HandleAllocated then
  begin
    if not IfNotExists or not MessageSearch(Msg.Msg) then
      MessagePoke(Msg);
    PostMessage(Handle, KM_LATEUPDATE, 0, 0);
  end;
end;

procedure TKCustomControl.PrintNotify(Status: TKPrintStatus; var Abort: Boolean);
begin
  if Assigned(FOnPrintNotify) then
    FOnPrintNotify(Self, Status, Abort);
end;

procedure TKCustomControl.PrintPaint;
begin
  if Assigned(FOnPrintPaint) then
    FOnPrintPaint(Self);
end;

procedure TKCustomControl.PrintOut;
begin
  GetPageSetup.PrintOut;
end;

procedure TKCustomControl.PaintPage;
begin
end;

procedure TKCustomControl.RemovePreview(APreview: TKPrintPreview);
begin
  if Assigned(FPreviewList) and (FPreviewList.IndexOf(APreview) >= 0) then
    FPreviewList.Remove(APreview);
end;

{$IFNDEF FPC}
procedure TKCustomControl.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;
{$ENDIF}

function TKCustomControl.SetMouseCursor(X, Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TKCustomControl.SetPageSetup(Value: TKPrintPageSetup);
begin
  if Value <> FPageSetup then
    GetPageSetup.Assign(Value);
end;

procedure TKCustomControl.UnlockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then
      InternalUnlockUpdate;
  end;
end;

procedure TKCustomControl.UpdateSize;
begin
end;

function TKCustomControl.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock = 0;
end;

{$IFNDEF FPC}
procedure TKCustomControl.WMCancelMode(var Msg: TWMCancelMode);
begin
  inherited;
  CancelMode;
end;
{$ENDIF}

{$IFNDEF COMPILER10_UP}
procedure TKCustomControl.WMMouseLeave(var Msg: TLMessage);
begin
  { this is because of CM_MOUSELEAVE is not sent if mouse has left client area
    and entered any of the standard control scrollbars. This behavior has been
    fixed via TrackMouseEvent in BDS 2006. }
  inherited;
  FMouseInClient := False;
  Perform(CM_MOUSELEAVE, 0, 0);
end;
{$ENDIF}

{$IFNDEF FPC}
procedure TKCustomControl.WMNCPaint(var Msg: TWMNCPaint);
{$IFDEF USE_THEMES}
var
  R: TRect;
  ExStyle: Integer;
  TempRgn: HRGN;
  BorderWidth,
  BorderHeight: Integer;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  with ThemeServices do if IsThemed and ThemesEnabled then
  begin
    // If OS themes are enabled and the client edge border is set for the window then prevent the default window proc
    // from painting the old border to avoid flickering.
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, R);
      // Determine width of the client edge.
      BorderWidth := GetSystemMetrics(SM_CXEDGE);
      BorderHeight := GetSystemMetrics(SM_CYEDGE);
      InflateRect(R, -BorderWidth, -BorderHeight);
      TempRgn := CreateRectRgnIndirect(R);
      // Exclude the border from the message region if there is one. Otherwise just use the inflated
      // window area region.
      if Msg.Rgn <> 1 then
        CombineRgn(TempRgn, Msg.Rgn, TempRgn, RGN_AND);
      DefWindowProc(Handle, Msg.Msg, Integer(TempRgn), 0);
      DeleteObject(TempRgn);
      PaintBorder(Self, True);
    end else
      inherited;
  end else
{$ENDIF}
    inherited;
end;

procedure TKCustomControl.WMSetCursor(var Msg: TWMSetCursor);
var
  MousePt: TPoint;
begin
  if (Msg.HitTest = HTCLIENT) and (Msg.CursorWnd = Handle) then
  begin
    MousePt := ScreenToClient(Mouse.CursorPos);
    if SetMouseCursor(MousePt.X, MousePt.Y) then
      Msg.Result := 1
    else
      inherited
  end else
    inherited;
end;
{$ENDIF}

procedure TKCustomControl.WMSize(var Msg: TLMSize);
begin
  inherited;
  PostLateUpdate(FillMessage(LM_SIZE, 0, 0), True);
end;

{$IFNDEF FPC}
{$IFDEF USE_THEMES}
procedure TKCustomControl.WMThemeChanged(var Msg: TLMessage);
begin
  if IsThemed then
  begin
    inherited;
    ThemeServices.UpdateThemes;
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_VALIDATE or RDW_FRAME);
  end;
end;
{$ENDIF}
{$ENDIF}

{ TKPrintPageSetup }

constructor TKPrintPageSetup.Create(AControl: TKCustomControl);
begin
  inherited Create;
  FActive := False;
  FCanvas := nil;
  FControl := AControl;
  FCopies := cCopiesDef;
  FCurrentCopy := 0;
  FCurrentPage := 0;
  FCurrentScale := 0;
  FDesktopPixelsPerInchX := 0;
  FDesktopPixelsPerInchY := 0;
  FEndPage := 0;
  FFooterSpace := 0;
  FHeaderSpace := 0;
  FHorzPageCount := 0;
  FIsValid := False;
  FMarginBottom := cMarginBottomDef;
  FMarginLeft := cMarginLeftDef;
  FMarginRight := cMarginRightDef;
  FMarginTop := cMarginTopDef;
  FOptions := cOptionsDef;
  FOutlineHeight := 0;
  FOutlineWidth := 0;
  FPageCount := 0;
  FPageHeight := 0;
  FPageWidth := 0;
  FPaintAreaHeight := 0;
  FPaintAreaWidth := 0;
  FPreviewing := False;
  FPrinterFooterSpace := 0;
  FPrinterHeaderSpace := 0;
  FPrinterMarginBottom := 0;
  FPrinterMarginLeft := 0;
  FPrinterMarginLeftMirrored := 0;
  FPrinterMarginRight := 0;
  FPrinterMarginRightMirrored := 0;
  FPrinterMarginTop := 0;
  FPrinterName := '';
  FPrinterPixelsPerInchX := 0;
  FPrinterPixelsPerInchY := 0;
  FPrintingMapped := True;
  FRange := cRangeDef;
  FStartPage := 0;
  FScale := cScaleDef;
  FTitle := '';
  FUnits := cUnitsDef;
  FUpdateLock := 0;
  FValidating := False;
  FVertPageCount := 0;
end;

function TKPrintPageSetup.GetCanPrint: Boolean;
begin
  Result := Assigned(FControl) and (FPageCount > 0) and (Printer.Printers.Count > 0);
end;

function TKPrintPageSetup.GetSelAvail: Boolean;
begin
  if Assigned(FControl) then
    Result := FControl.InternalGetSelAvail
  else
    Result := False;
end;

procedure TKPrintPageSetup.AfterUnitsChange;
begin
  FFooterSpace := InchesToValue(FUnits, FFooterSpace);
  FHeaderSpace := InchesToValue(FUnits, FHeaderSpace);
  FMarginBottom := InchesToValue(FUnits, FMarginBottom);
  FMarginLeft := InchesToValue(FUnits, FMarginLeft);
  FMarginRight := InchesToValue(FUnits, FMarginRight);
  FMarginTop := InchesToValue(FUnits, FMarginTop);
end;

procedure TKPrintPageSetup.Assign(Source: TPersistent);
begin
  if Source is TKPrintPageSetup then
  begin
    LockUpdate;
    try
      Copies := TKPrintPageSetup(Source).Copies;
      EndPage := TKPrintPageSetup(Source).EndPage;
      FooterSpace := TKPrintPageSetup(Source).FooterSpace;
      HeaderSpace := TKPrintPageSetup(Source).HeaderSpace;
      MarginBottom := TKPrintPageSetup(Source).MarginBottom;
      MarginLeft := TKPrintPageSetup(Source).MarginLeft;
      MarginRight := TKPrintPageSetup(Source).MarginRight;
      MarginTop := TKPrintPageSetup(Source).MarginTop;
      Options := TKPrintPageSetup(Source).Options;
      PrinterName := TKPrintPageSetup(Source).PrinterName;
      Range := TKPrintPageSetup(Source).Range;
      StartPage := TKPrintPageSetup(Source).StartPage;
      Scale := TKPrintPageSetup(Source).Scale;
      Title := TKPrintPageSetup(Source).Title;
      Units := TKPrintPageSetup(Source).Units;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKPrintPageSetup.BeforeUnitsChange;
begin
  FFooterSpace := ValueToInches(FUnits, FFooterSpace);
  FHeaderSpace := ValueToInches(FUnits, FHeaderSpace);
  FMarginBottom := ValueToInches(FUnits, FMarginBottom);
  FMarginLeft := ValueToInches(FUnits, FMarginLeft);
  FMarginRight := ValueToInches(FUnits, FMarginRight);
  FMarginTop := ValueToInches(FUnits, FMarginTop);
end;

function TKPrintPageSetup.HMap(Value: Integer): Integer;
begin
  Result := MulDiv(Value, FPrinterPixelsPerInchX, FDesktopPixelsPerInchX);
end;

procedure TKPrintPageSetup.Invalidate;
begin
  FIsValid := False;
end;

procedure TKPrintPageSetup.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TKPrintPageSetup.PaintPageToPreview;
var
  PaperWidth, PaperHeight, SaveIndex: Integer;
  R, PageRect: TRect;
begin
  if UpdateUnlocked and Assigned(FControl) then
  begin
    FCanvas := APreview.Canvas;
    FActive := True;
    FPreviewing := True;
    try
      FCurrentCopy := 1;
      FCurrentPage := APreview.Page;
      if (poMirrorMargins in FOptions) and (FCurrentPage and 1 <> 0) then
      begin
        FPrinterMarginLeftMirrored := FPrinterMarginRight;
        FPrinterMarginRightMirrored := FPrinterMarginLeft;
      end else
      begin
        FPrinterMarginLeftMirrored := FPrinterMarginLeft;
        FPrinterMarginRightMirrored := FPrinterMarginRight;
      end;
      R := APreview.PageRect;
      PaperWidth := R.Right - R.Left;
      PaperHeight := R.Bottom - R.Top;
      SaveIndex := SaveDC(FCanvas.Handle);
      try
        // change the canvas mapping mode to scale the page outline
        CanvasSetOffset(FCanvas,
          R.Left + MulDiv(FPrinterMarginLeftMirrored, PaperWidth, FPageWidth),
          R.Top + MulDiv(FPrinterMarginTop + FPrinterHeaderSpace, PaperHeight, FPageHeight));
        if FPrintingMapped then
          CanvasSetScale(FCanvas, Round(PaperWidth * FCurrentScale), Round(PaperHeight * FCurrentScale),
            MulDiv(FPageWidth, FDesktopPixelsPerInchX, FPrinterPixelsPerInchX),
            MulDiv(FPageHeight, FDesktopPixelsPerInchY, FPrinterPixelsPerInchY))
        else
          CanvasSetScale(FCanvas, PaperWidth, PaperHeight, FPageWidth, FPageHeight);
        FControl.PaintPage;
      finally
        RestoreDC(FCanvas.Handle, SaveIndex);
      end;
      SaveIndex := SaveDC(FCanvas.Handle);
      try
        CanvasSetOffset(FCanvas, R.Left, R.Top);
        CanvasSetScale(FCanvas, PaperWidth, PaperHeight, FPageWidth, FPageHeight);
        PageRect := Rect(0, 0, FPageWidth, FPageHeight);
        TranslateRectToDevice(FCanvas.Handle, PageRect);
        SelectClipRect(FCanvas.Handle, PageRect);
        FControl.PrintPaint;
      finally
        RestoreDC(FCanvas.Handle, SaveIndex);
      end;
      SaveIndex := SaveDC(FCanvas.Handle);
      try
        CanvasSetOffset(FCanvas, R.Left, R.Top);
        CanvasSetScale(FCanvas, PaperWidth, PaperHeight, FPageWidth, FPageHeight);
        PageRect := Rect(0, 0, FPageWidth, FPageHeight);
        TranslateRectToDevice(FCanvas.Handle, PageRect);
        SelectClipRect(FCanvas.Handle, PageRect);
        PrintTitle;
        PrintPageNumber(FCurrentPage);
      finally
        RestoreDC(FCanvas.Handle, SaveIndex);
      end;
    finally
      FActive := False;
      FPreviewing := False;
      FCanvas := nil;
    end;
  end;
end;

procedure TKPrintPageSetup.PrintPageNumber(Value: Integer);
var
  S: string;
begin
  if poPageNumbers in FOptions then
  begin
    FCanvas.Brush.Style := bsClear;
    FCanvas.Font.Color := clBlack;
    FCanvas.Font.Height := 1;
    FCanvas.Font.Height := VMap(16);
    FCanvas.Font.Name := 'Arial';
    FCanvas.Font.Pitch := fpDefault;
    FCanvas.Font.Style := [fsBold];
    S := Format('- %d -', [Value]);
    FCanvas.TextOut(FPrinterMarginLeftMirrored + (FPageWidth - FPrinterMarginLeft - FPrinterMarginRight - FCanvas.TextWidth(S)) div 2,
      FPageHeight - FPrinterMarginBottom + VMap(5), S);
  end;
end;

procedure TKPrintPageSetup.PrintTitle;
begin
  if poTitle in FOptions then
  begin
    FCanvas.Brush.Style := bsClear;
    FCanvas.Font.Color := clBlack;
    FCanvas.Font.Height := 1;
    FCanvas.Font.Height := VMap(16);
    FCanvas.Font.Name := 'Arial';
    FCanvas.Font.Pitch := fpDefault;
    FCanvas.Font.Style := [fsBold];
    FCanvas.TextOut(FPrinterMarginLeftMirrored, FPrinterMarginTop - VMap(36), Title);
    FCanvas.Brush.Style := bsSolid;
    FCanvas.Brush.Color := clBlack;
    FCanvas.FillRect(Rect(FPrinterMarginLeftMirrored, FPrinterMarginTop - VMap(14), FPageWidth - FPrinterMarginRight, FPrinterMarginTop - VMap(12)));
  end;
end;

procedure TKPrintPageSetup.PrintOut;

  function DoPrint: Boolean;
  var
    SaveIndex: Integer;
    PageRect: TRect;
  begin
    Result := False;
    if (poMirrorMargins in FOptions) and (FCurrentPage and 1 <> 0) then
    begin
      FPrinterMarginLeftMirrored := FPrinterMarginRight;
      FPrinterMarginRightMirrored := FPrinterMarginLeft;
    end else
    begin
      FPrinterMarginLeftMirrored := FPrinterMarginLeft;
      FPrinterMarginRightMirrored := FPrinterMarginRight;
    end;
    SaveIndex := SaveDC(FCanvas.Handle);
    try
      CanvasSetOffset(FCanvas, FPrinterMarginLeftMirrored, FPrinterMarginTop + FPrinterHeaderSpace);
      if FPrintingMapped then
      begin
        // change the canvas mapping mode to scale the page outline
        CanvasSetScale(FCanvas, Round(FPageWidth * FCurrentScale), Round(FPageHeight * FCurrentScale),
          MulDiv(FPageWidth, FDesktopPixelsPerInchX, FPrinterPixelsPerInchX),
          MulDiv(FPageHeight, FDesktopPixelsPerInchY, FPrinterPixelsPerInchY));
      end else
        CanvasResetScale(FCanvas);
      FControl.PaintPage;
    finally
      RestoreDC(FCanvas.Handle, SaveIndex);
    end;
    SaveIndex := SaveDC(FCanvas.Handle);
    try
      CanvasResetScale(FCanvas);
      PageRect := Rect(0, 0, FPageWidth, FPageHeight);
      TranslateRectToDevice(FCanvas.Handle, PageRect);
      SelectClipRect(FCanvas.Handle, PageRect);
      FControl.PrintPaint;
    finally
      RestoreDC(FCanvas.Handle, SaveIndex);
    end;
    SaveIndex := SaveDC(FCanvas.Handle);
    try
      CanvasResetScale(FCanvas);
      PageRect := Rect(0, 0, FPageWidth, FPageHeight);
      TranslateRectToDevice(FCanvas.Handle, PageRect);
      SelectClipRect(FCanvas.Handle, PageRect);
      PrintTitle;
      PrintPageNumber(FCurrentPage);
    finally
      RestoreDC(FCanvas.Handle, SaveIndex);
    end;
    FControl.PrintNotify(epsNewPage, Result);
    if ((FCurrentPage < FEndPage) or (FCurrentCopy < FCopies)) and not Result then
      Printer.NewPage;
  end;

var
  I, J: Integer;
  AbortPrint: Boolean;
{  Orientation: TPrinterOrientation;
  PaperSize: TPaperSize;
  APageWidth, ApageHeight, APaperWidth, APaperHeight: Integer;
  PrinterType: TPrinterType;
  APaperRect: TPaperRect;}
begin
  if UpdateUnlocked and Assigned(FControl) then
  begin
    UpdateSettings;
    if FPageCount > 0 then
    begin
      AbortPrint := False;
      FCanvas := Printer.Canvas;
      Printer.Title := FTitle;
      Printer.Copies := 1;
{      PrinterType := Printer.PrinterType;
      APageWidth := Printer.PageWidth;
      APageHeight := Printer.PageHeight;
      APaperRect := Printer.PaperSize.PaperRect;
      Orientation := Printer.Orientation;}
      Printer.BeginDoc;
      FActive := True;
      try
        FControl.PrintNotify(epsBegin, AbortPrint);
{        Printer.Canvas.Font.Name := 'Arial';
        Printer.Canvas.Font.color := clBlack;
        Printer.Canvas.Font.height := 100;
        Printer.Canvas.TextOut(200, 200, 'hello!');}
        if not AbortPrint then
        begin
          if poCollate in FOptions then
            for I := 1 to FCopies do
            begin
              FCurrentCopy := I;
              for J := FStartPage to FEndPage do
              begin
                FCurrentPage := J;
                AbortPrint := DoPrint;
                if AbortPrint then Break;
              end;
              if AbortPrint then Break;
            end
          else
            for J := FStartPage to FEndPage do
            begin
              FCurrentPage := J;
              for I := 1 to FCopies do
              begin
                FCurrentCopy := I;
                AbortPrint := DoPrint;
                if AbortPrint then Break;
              end;
              if AbortPrint then Break;
            end
        end;
        FCurrentPage := 0;
        FCurrentCopy := 0;
        FControl.PrintNotify(epsEnd, AbortPrint);
      finally
        FActive := False;
        Printer.EndDoc;
        FCanvas := nil;
      end;
    end;
  end;
end;

procedure TKPrintPageSetup.SetCopies(Value: Integer);
begin
  if FActive then Exit;
  if Value <> FCopies then
  begin
    FCopies := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetEndPage(Value: Integer);
begin
  if FActive then Exit;
  if Value <> FEndPage then
  begin
    FEndPage := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetFooterSpace(Value: Double);
begin
  if FActive then Exit;
  if Value <> FFooterSpace then
  begin
    FFooterSpace := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetHeaderSpace(Value: Double);
begin
  if FActive then Exit;
  if Value <> FHeaderSpace then
  begin
    FHeaderSpace := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetMarginBottom(Value: Double);
begin
  if FActive then Exit;
  if Value <> FMarginBottom then
  begin
    FMarginBottom := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetMarginLeft(Value: Double);
begin
  if FActive then Exit;
  if Value <> FMarginLeft then
  begin
    FMarginLeft := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetMarginRight(Value: Double);
begin
  if FActive then Exit;
  if Value <> FMarginRight then
  begin
    FMarginRight := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetMarginTop(Value: Double);
begin
  if FActive then Exit;
  if Value <> FMarginTop then
  begin
    FMarginTop := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetOptions(Value: TKPrintOptions);
begin
  if FActive then Exit;
  if Value <> FOptions then
  begin
    FOptions := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetPrinterName(const Value: string);
begin
  if FActive then Exit;
  if Value <> FPrinterName then
  begin
    FPrinterName := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetPrintingMapped(Value: Boolean);
begin
  if FActive then Exit;
  if Value <> FPrintingMapped then
  begin
    FPrintingMapped := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetRange(Value: TKPrintRange);
begin
  if FActive then Exit;
  if Value <> FRange then
  begin
    FRange := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetScale(Value: Integer);
begin
  if FActive then Exit;
  if Value <> FScale then
  begin
    FScale := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetStartPage(Value: Integer);
begin
  if FActive then Exit;
  if Value <> FStartPage then
  begin
    FStartPage := Value;
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.SetUnits(Value: TKPrintUnits);
begin
  if FActive then Exit;
  if Value <> FUnits then
  begin
    BeforeUnitsChange;
    FUnits := Value;
    AfterUnitsChange;
  end;
end;

procedure TKPrintPageSetup.UnlockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    UpdateSettings;
  end;
end;

procedure TKPrintPageSetup.UpdateSettings;
var
  I, PixelsPerInchX, PixelsPerInchY: Integer;
  D: Double;
  DC: HDC;
  Info: TKPrintMeasureInfo;
begin
  if UpdateUnlocked and not FActive and not FValidating then
  begin
    FValidating := True;
    try
      Printer.Refresh;
      I := Printer.Printers.IndexOf(FPrinterName);
      if I >= 0 then
        Printer.PrinterIndex := I;
      // limit copies and Scale
      FCopies := MinMax(FCopies, cCopiesMin, cCopiesMax);
      FScale := MinMax(FScale, cScaleMin, cScaleMax);
      // get metrics for the desktop
      DC := GetDC(0);
      try
        FDesktopPixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
        FDesktopPixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
      finally
        ReleaseDC(0, DC);
      end;
      // get metrics for the printer
      if Printer.Printers.Count > 0 then
      begin
        FPageWidth := Printer.PageWidth;
        FPageHeight := Printer.PageHeight;
      {$IFDEF FPC}
        FPrinterPixelsPerInchX := Printer.XDPI;
        FPrinterPixelsPerInchY := Printer.YDPI;
      {$ELSE}
        FPrinterPixelsPerInchX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
        FPrinterPixelsPerInchY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
      {$ENDIF}
      end else
      begin
        // fake printer metrics if no printer is installed
        FPageWidth := 2360;
        FPageHeight := 3400;
        FPrinterPixelsPerInchX := 300;
        FPrinterPixelsPerInchY := 300;
      end;
      // decide how to outline extent
      if FPrintingMapped then
      begin
        PixelsPerInchX := FDesktopPixelsPerInchX;
        PixelsPerInchY := FDesktopPixelsPerInchY;
      end else
      begin
        PixelsPerInchX := FPrinterPixelsPerInchX;
        PixelsPerInchY := FPrinterPixelsPerInchY;
      end;
      // limit and convert margins
      D := FPageWidth * 0.4; // 40% of the page
      FPrinterMarginLeft := Round(MinMax(ValueToInches(FUnits, FMarginLeft) * FPrinterPixelsPerInchX, 0, D));
      FPrinterMarginLeftMirrored := FPrinterMarginLeft;
      FMarginLeft := InchesToValue(FUnits, FPrinterMarginLeft / FPrinterPixelsPerInchX);
      FPrinterMarginRight := Round(MinMax(ValueToInches(FUnits, FMarginRight) * FPrinterPixelsPerInchX, 0, D));
      FPrinterMarginRightMirrored := FPrinterMarginRight;
      FMarginRight := InchesToValue(FUnits, FPrinterMarginRight / FPrinterPixelsPerInchX);
      D := FPageHeight * 0.4; // 40% of the page
      FPrinterMarginTop := Round(MinMax(ValueToInches(FUnits, FMarginTop) * FPrinterPixelsPerInchY, 0, D));
      FMarginTop := InchesToValue(FUnits, FPrinterMarginTop / FPrinterPixelsPerInchY);
      FPrinterMarginBottom := Round(MinMax(ValueToInches(FUnits, FMarginBottom) * FPrinterPixelsPerInchY, 0, D));
      FMarginBottom := InchesToValue(FUnits, FPrinterMarginBottom / FPrinterPixelsPerInchY);
      // limit and convert header and footer space
      FPrinterHeaderSpace := Round(MinMax(ValueToInches(FUnits, Max(FHeaderSpace, 0)) * FPrinterPixelsPerInchY, 0, D -  FPrinterMarginTop));
      FHeaderSpace := InchesToValue(FUnits, FPrinterHeaderSpace / FPrinterPixelsPerInchY);
      FPrinterFooterSpace := Round(MinMax(ValueToInches(FUnits, Max(FFooterSpace, 0)) * FPrinterPixelsPerInchY, 0, D -  FPrinterMarginBottom));
      FFooterSpace := InchesToValue(FUnits, FPrinterFooterSpace / FPrinterPixelsPerInchY);
      // paint area extent
      FPaintAreaHeight := MulDiv(FPageHeight - FPrinterMarginTop - FPrinterMarginBottom - FPrinterHeaderSpace - FPrinterFooterSpace, PixelsPerInchY, FPrinterPixelsPerInchY);
      FPaintAreaWidth := MulDiv(FPageWidth - FPrinterMarginLeft - FPrinterMarginRight, PixelsPerInchX, FPrinterPixelsPerInchX);
      // default horizontal scaling
      FCurrentScale := FScale / 100;
      // default page/copy info
      FCurrentCopy := 0;
      FCurrentPage := 0;
      // measured data
      if Assigned(FControl) then
      begin
        FillChar(Info, SizeOf(TKPrintMeasureInfo), 0);
        FControl.MeasurePages(Info);
        FOutlineWidth := Info.OutlineWidth;
        FOutlineHeight := Info.OutlineHeight;
        FHorzPageCount := Info.HorzPageCount;
        FVertPageCount := Info.VertPageCount;
        FPageCount := Info.PageCount;
        if FPageCount > 0 then
        begin
          // update horizontal scaling
          if (poFitToPage in FOptions) and (FOutlineWidth > 0) then
            FCurrentScale := FPaintAreaWidth / FOutlineWidth;
          // limit start and end page
          case FRange of
            prAll, prSelectedOnly:
            begin
              FStartPage := 1;
              FEndPage := FPageCount;
            end;
            prRange:
            begin
              FEndPage := MinMax(FEndPage, 1, FPageCount);
              FStartPage := MinMax(FStartPage, 1, FEndPage);
            end;
          end;
        end;
        // notify all previews/ force their repainting
        FControl.NotifyPreviews;
      end else
      begin
        FOutlineWidth := 0;
        FOutlineHeight := 0;
        FHorzPageCount := 0;
        FVertPageCount := 0;
        FPageCount := 0;
        FEndPage := 0;
        FStartPage := 0;
      end;
      FIsValid := True;
    finally
      FValidating := False;
    end;
  end;
end;

function TKPrintPageSetup.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock = 0;
end;

procedure TKPrintPageSetup.Validate;
begin
  if not FIsValid and not FValidating then
    UpdateSettings;
end;

function TKPrintPageSetup.VMap(Value: Integer): Integer;
begin
  Result := MulDiv(Value, FPrinterPixelsPerInchY, FDesktopPixelsPerInchY);
end;

{ TKPreviewColors }

constructor TKPreviewColors.Create(APreview: TKPrintPreview);
begin
  inherited Create;
  FPreview := APreview;
  Initialize;
end;

procedure TKPreviewColors.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TKPreviewColors then
  begin
    Colors := TKPreviewColors(Source).Colors;
    FPreview.Invalidate;
  end
end;

function TKPreviewColors.GetColor(Index: TKPreviewColorIndex): TColor;
begin
  Result := InternalGetColor(Index);
end;

function TKPreviewColors.GetColorEx(Index: TKPreviewColorIndex): TColor;
begin
  Result := FColors[Index];
end;

procedure TKPreviewColors.Initialize;
begin
  SetLength(FColors, ciPreviewColorsMax + 1);
  FColors[ciPaper] := cPaperDef;
  FColors[ciBkGnd] := cBkGndDef;
  FColors[ciBorder] := cBorderDef;
  FColors[ciSelectedBorder] := cSelectedBorderDef;
end;

function TKPreviewColors.InternalGetColor(Index: TKPreviewColorIndex): TColor;
begin
  Result := FColors[Index];
end;

procedure TKPreviewColors.InternalSetColor(Index: TKPreviewColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    if not (csLoading in FPreview.ComponentState) then
      FPreview.Invalidate;
  end;
end;

procedure TKPreviewColors.SetColor(Index: TKPreviewColorIndex; Value: TColor);
begin
  InternalSetColor(Index, Value);
end;

procedure TKPreviewColors.SetColorEx(Index: TKPreviewColorIndex; Value: TColor);
begin
  FColors[Index] := Value;
end;

procedure TKPreviewColors.SetColors(const Value: TKColorArray);
var
  I: Integer;
begin
  for I := 0 to Min(Length(FColors), Length(Value)) - 1 do
    FColors[I] := Value[I];
end;

{ TKPrintPreview }

constructor TKPrintPreview.Create(AOwner: TComponent);
begin
  inherited;
  FColors := TKPreviewColors.Create(Self);
  FControl := nil;
  FMouseWheelAccumulator := 0;
  FPage := 1;
  FPageSize := Point(0, 0);
  FScale := 100;
  FScaleMode := smPageWidth;
  FOnChanged := nil;
  LoadCustomCursor(crDragHandFree, 'KPREVIEW_CURSOR_HAND_FREE');
  LoadCustomCursor(crDragHandGrip, 'KPREVIEW_CURSOR_HAND_GRIP');
  Width := 300;
  Height := 200;
end;

destructor TKPrintPreview.Destroy;
begin
  if Assigned(FControl) then
    FControl.RemovePreview(Self);
  inherited;
  FColors.Free;
end;

procedure TKPrintPreview.BeginScrollWindow;
begin
  FPageOld := FPage;
  FScrollPosOld := FScrollPos;
end;

procedure TKPrintPreview.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    Style := Style or WS_HSCROLL or WS_VSCROLL;
end;

function TKPrintPreview.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  cWheelDivisor = 120;
var
  Delta, WheelClicks: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    if ssCtrl in Shift then
    begin
      if FScaleMode = smWholePage then Delta := 10 else Delta := ClientHeight;
    end else
      if FScaleMode = smWholePage then Delta := 1 else Delta := ClientHeight div 10;
    Inc(FMouseWheelAccumulator, WheelDelta);
    WheelClicks := FMouseWheelAccumulator div cWheelDivisor;
    FMouseWheelAccumulator := FMouseWheelAccumulator mod cWheelDivisor;
    BeginScrollWindow;
    ModifyScrollBar(SB_VERT, -1, -WheelClicks * Delta);
    EndScrollWindow;
    Result := True;
  end;
end;

procedure TKPrintPreview.EndScrollWindow;
begin
  if (FPage <> FPageOld) then
    Invalidate
  else if (FScrollPos.X <> FScrollPosOld.X) or (FScrollPos.Y <> FScrollPosOld.Y) then
  begin
    ScrollWindowEx(Handle, FScrollPosOld.X - FScrollPos.X, FScrollPosOld.Y - FScrollPos.Y,
      nil, nil, 0, nil, SW_INVALIDATE);
  end;
end;

procedure TKPrintPreview.FirstPage;
begin
  Page := StartPage;
end;

function TKPrintPreview.GetCurrentScale: Integer;
begin
  if Assigned(FControl) then
    Result := MulDiv(FPageSize.X, 100, MulDiv(FControl.PageSetup.PageWidth, 300, FControl.PageSetup.PrinterPixelsPerInchX))
  else
    Result := FScale;
end;

function TKPrintPreview.GetEndPage: Integer;
begin
  if Assigned(FControl) then
  begin
    Result := FControl.PageSetup.EndPage;
    if Result = 0 then
    begin
      FControl.PageSetup.UpdateSettings;
      Result := FControl.PageSetup.EndPage
    end;
  end else
    Result := 0;
end;

function TKPrintPreview.GetPageRect: TRect;
begin
  with Result do
  begin
    Left := FPageOffset.X - FScrollPos.X;
    if FScaleMode = smWholePage then
      Top := FPageOffset.Y
    else
      Top := FPageOffset.Y - FScrollPos.Y;
    Right := Left + FPageSize.X;
    Bottom := Top + FPageSize.Y;
  end;
end;

function TKPrintPreview.GetStartPage: Integer;
begin
  if Assigned(FControl) then
  begin
    Result := FControl.PageSetup.StartPage;
    if Result = 0 then
    begin
      FControl.PageSetup.UpdateSettings;
      Result := FControl.PageSetup.StartPage
    end;
  end else
    Result := 0;
end;

procedure TKPrintPreview.KeyDown(var Key: Word; Shift: TShiftState);
var
  DeltaX, DeltaY, LineX, PageY: Integer;
  NoAlt, NoAltCtrl: Boolean;
begin
  NoAlt := Shift * [ssAlt] = [];
  NoAltCtrl := Shift * [ssAlt, ssCtrl] = [];
  DeltaX := 0;
  DeltaY := 0;
  LineX := ClientWidth div 10;
  PageY := ClientHeight;
  case Key of
    VK_UP:
      if NoAltCtrl then
      begin
        if FScaleMode = smWholePage then
          PreviousPage
        else
          DeltaY := -PageY div 10;
      end;
    VK_DOWN:
      if NoAltCtrl then
      begin
        if FScaleMode = smWholePage then
          NextPage
        else
          DeltaY := PageY div 10;
      end;
    VK_PRIOR:
      if NoAltCtrl then
      begin
        if FScaleMode = smWholePage then
          PreviousPage
        else
          DeltaY := -PageY;
      end;
    VK_NEXT:
      if NoAltCtrl then
      begin
        if FScaleMode = smWholePage then
          NextPage
        else
          DeltaY := PageY;
      end;
    VK_LEFT: if NoAltCtrl then DeltaX := -LineX;
    VK_RIGHT: if NoAltCtrl then DeltaX := LineX;
    VK_HOME:
      if NoAlt then
      begin
        if ssCtrl in Shift then
          FirstPage
        else
          DeltaX := -FScrollPos.X;
      end;
    VK_END:
      if NoAlt then
      begin
        if ssCtrl in Shift then
          LastPage
        else
          DeltaX := FScrollExtent.X - FScrollPos.X;
      end;
  end;
  if (DeltaX <> 0) or (DeltaY <> 0) then
  begin
    BeginScrollWindow;
    if DeltaX <> 0 then
      ModifyScrollBar(SB_HORZ, -1, DeltaX);
    if DeltaY <> 0 then
      ModifyScrollBar(SB_VERT, -1, DeltaY);
    EndScrollWindow;
  end;
end;

procedure TKPrintPreview.LastPage;
begin
  Page := EndPage;
end;

procedure TKPrintPreview.ModifyScrollBar(ScrollBar, ScrollCode, Delta: Integer);
var
  I, AEndPage: Integer;
  Divisor: Cardinal;
  PPos, PExtent: PInteger;
  SI: TScrollInfo;
begin
  Divisor := 10;
  if ScrollBar = SB_HORZ then
  begin
    PPos := @FScrollPos.X;
    PExtent := @FScrollExtent.X;
  end else
  begin
    if FScaleMode = smWholePage then
    begin
      PPos := @FPage;
      AEndPage := EndPage;
      PExtent := @AEndPage;
      Divisor := 1;
    end else
    begin
      PPos := @FScrollPos.Y;
      PExtent := @FScrollExtent.Y;
    end;
  end;
  if PExtent^ > 0 then
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_RANGE or SIF_PAGE or SIF_TRACKPOS;
    GetScrollInfo(Handle, ScrollBar, SI);
  {$IF DEFINED(LCLGTK2)}
    {.$WARNING "scrollbar arrows still not working properly on GTK2 in some cases!"}
    SI.nTrackPos := Delta;
  {$IFEND}
    I := PPos^;
    case ScrollCode of
      SB_TOP: I := SI.nMin;
      SB_BOTTOM: I := SI.nMax; // will be trimmed below
      SB_LINEUP: Dec(I, SI.nPage div Divisor);
      SB_LINEDOWN: Inc(I, SI.nPage div Divisor);
      SB_PAGEUP: Dec(I, SI.nPage);
      SB_PAGEDOWN: Inc(I, SI.nPage);
      SB_THUMBTRACK, SB_THUMBPOSITION: I := SI.nTrackPos;
    else
      Inc(I, Delta)
    end;
    if FScaleMode = smWholePage then
      I := MinMax(I, 1, PExtent^)
    else
      I := MinMax(I, 0, PExtent^);
    PPos^ := I;  
    SI.nPos := I;
    SI.fMask := SIF_POS;
    SetScrollInfo(Handle, ScrollBar, SI, True);
  end;
end;

procedure TKPrintPreview.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    SafeSetFocus;
    if (FScaleMode <> smWholePage) and PtInRect(GetPageRect, Point(X, Y)) then
    begin
      FlagSet(cPF_Dragging);
      FX := X;
      FY := Y;
      SetMouseCursor(X, Y);
    end;
  end;
end;

procedure TKPrintPreview.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Flag(cPF_Dragging) and MouseCapture then
  begin
    BeginScrollWindow;
    if (X > FX) and (FScrollPos.X > 0) or (X < FX) and (FScrollPos.X < FScrollExtent.X) then
    begin
      ModifyScrollBar(SB_HORZ, -1, FX - X);
      FX := X;
    end;
    if (Y > FY) and (FScrollPos.Y > 0) or (Y < FY) and (FScrollPos.Y < FScrollExtent.Y) then
    begin
      ModifyScrollBar(SB_VERT, -1, FY - Y);
      FY := Y;
    end;
    EndScrollWindow;
  end;
end;

procedure TKPrintPreview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FlagClear(cPF_Dragging);
  SetMouseCursor(X, Y);
end;

procedure TKPrintPreview.NextPage;
begin
  Page := Page + 1;
end;

procedure TKPrintPreview.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FControl) then
  begin
    FControl := nil;
    UpdatePreview;
  end;
end;

procedure TKPrintPreview.Paint;

  procedure DoPaint(IsBuffer: Boolean);
  var
    C: TColor;
    R, RPaper, RPage: TRect;
    RgnPaper: HRGN;
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    RPage := GetPageRect;
    RPaper := RPage;
    with RPaper do
    begin
      Inc(Right, cPreviewShadowSize);
      Inc(Bottom, cPreviewShadowSize);
    end;
    if not IsBuffer then
      RgnPaper := CreateRectRgnIndirect(RPaper)
    else
      RgnPaper := 0;
    try
      // paint background around paper, we don't want at least this to flicker
      if IsBuffer or (ExtSelectClipRgn(Canvas.Handle, RgnPaper, RGN_DIFF) <> NULLREGION) then
      begin
        Canvas.Brush.Color := FColors.BkGnd;
        Canvas.FillRect(ClientRect);
      end;
      if not IsBuffer then
        SelectClipRgn(Canvas.Handle, RgnPaper);
    finally
      if not IsBuffer then
        DeleteObject(rgnPaper);
    end;
    // paint paper outline
    if Focused then
      C := FColors.SelectedBorder
    else
      C := FColors.Border;
    Canvas.Pen.Color := C;
    Canvas.Brush.Color := FColors.Paper;
    Canvas.Rectangle(RPage);
    Canvas.Brush.Color := FColors.BkGnd;
    R := Rect(RPage.Left, RPage.Bottom, RPage.Left + cPreviewShadowSize, RPage.Bottom + cPreviewShadowSize);
    Canvas.FillRect(R);
    R := Rect(RPage.Right, RPage.Top, RPage.Right + cPreviewShadowSize, RPage.Top + cPreviewShadowSize);
    Canvas.FillRect(R);
    Canvas.Brush.Color := C;
    R := Rect(RPage.Left + cPreviewShadowSize, RPage.Bottom, RPaper.Right, RPaper.Bottom);
    Canvas.FillRect(R);
    R := Rect(RPage.Right, RPage.Top + cPreviewShadowSize, RPaper.Right, RPaper.Bottom);
    Canvas.FillRect(R);
    // paint page outline
    InflateRect(RPage, -1, -1);
    FControl.PageSetup.PaintPageToPreview(Self);
  end;

var
  SaveIndex: Integer;
  RClient: TRect;
{$IFDEF USE_WINAPI}
  Org: TPoint;
  MemBitmap, OldBitmap: HBITMAP;
  DC: HDC;
{$ENDIF}
begin
  RClient := ClientRect;
  if Assigned(FControl) then
  begin
    SaveIndex := SaveDC(Canvas.Handle);
    try
    {$IFDEF USE_WINAPI}
      if DoubleBuffered then
      begin
        // we must paint always the entire client because of canvas scaling
        MemBitmap := CreateCompatibleBitmap(Canvas.Handle, RClient.Right - RClient.Left, RClient.Bottom - RClient.Top);
        try
          OldBitmap := SelectObject(Canvas.Handle, MemBitmap);
          try
            SetWindowOrgEx(Canvas.Handle, 0, 0, @Org);
            SelectClipRect(Canvas.Handle, Rect(0, 0, RClient.Right - RClient.Left, RClient.Bottom - RClient.Top));
            DoPaint(True);
          finally
            SelectObject(Canvas.Handle, OldBitmap);
            SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
          end;
          // copy MemBitmap to original canvas
          DC := CreateCompatibleDC(Canvas.Handle);
          try
            OldBitmap := SelectObject(DC, MemBitmap);
            try
              CopyBitmap(Canvas.Handle, RClient, DC, 0, 0);
            finally
              SelectObject(DC, OldBitmap);
            end;
          finally
            DeleteDC(DC);
          end;
        finally
          DeleteObject(MemBitmap);
        end;
      end else
    {$ENDIF}
        DoPaint(False);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end else
  begin
    Canvas.Brush.Color := FColors.BkGnd;
    Canvas.FillRect(RClient);
  end;
end;

procedure TKPrintPreview.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TKPrintPreview.PreviousPage;
begin
  Page := Page - 1;
end;

procedure TKPrintPreview.SafeSetFocus;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and Visible and Enabled then
    Form.ActiveControl := Self;
end;

procedure TKPrintPreview.SetColors(const Value: TKPreviewColors);
begin
  FColors.Assign(Value);
end;

procedure TKPrintPreview.SetControl(Value: TKCustomControl);
begin
  if (Value <> FControl) and (Value <> Self) and not (Value is TKPrintPreview) then
  begin
    if Assigned(FControl) then
      FControl.RemovePreview(Self);
    FControl := Value;
    if Assigned(FControl) then
      FControl.AddPreview(Self);
    UpdatePreview;
  end;
end;

procedure TKPrintPreview.SetPage(Value: Integer);
begin
  Value := MinMax(Value, StartPage, EndPage);
  if Value <> FPage then
  begin
    BeginScrollWindow;
    if FScaleMode = smWholePage then
      ModifyScrollBar(SB_VERT, -1, Value - FPage)
    else
      FPage := Value;
    EndScrollWindow;
    Changed;
  end;
end;

procedure TKPrintPreview.SetScale(Value: Integer);
begin
  Value := MinMax(Value, cScaleMin, cScaleMax);
  if Value <> FScale then
  begin
    FScale := Value;
    UpdatePreview;
  end;
end;

procedure TKPrintPreview.SetScaleMode(Value: TKPreviewScaleMode);
begin
  if Value <> FScaleMode then
  begin
    FScaleMode := Value;
    UpdatePreview;
  end;
end;

function TKPrintPreview.SetMouseCursor(X, Y: Integer): Boolean;
var
  ACursor: TCursor;
begin
  if PtInRect(GetPageRect, Point(X, Y)) and (FScaleMode <> smWholePage) then
  begin
    if MouseCapture then
      ACursor := crDragHandGrip
    else
      ACursor := crDragHandFree;
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

procedure TKPrintPreview.UpdatePreview;
begin
  Page := FPage;
  UpdateScrollRange;
  Changed;
end;

procedure TKPrintPreview.UpdateScrollRange;
var
  I: Integer;
  PageWidth100Percent, PageHeight100Percent: Integer;
  SI: TScrollInfo;
begin
  if HandleAllocated and not Flag(cPF_UpdateRange) then
  begin
    FlagSet(cPF_UpdateRange);
    try
      if Assigned(FControl) then
      begin
        // get isotropic page size in 300 dpi
        PageWidth100Percent := MulDiv(FControl.PageSetup.PageWidth, 300, FControl.PageSetup.PrinterPixelsPerInchX);
        PageHeight100Percent := MulDiv(FControl.PageSetup.PageHeight, 300, FControl.PageSetup.PrinterPixelsPerInchY);
        case FScaleMode of
          smScale:
          begin
            FPageSize.X := MulDiv(PageWidth100Percent, FScale, 100);
            FPageSize.Y := MulDiv(PageHeight100Percent, FScale, 100);
          end;
          smPageWidth:
          begin
            FPageSize.X := Max(ClientWidth - 2 * cPreviewHorzBorder - cPreviewShadowSize, 40);
            FPageSize.Y := MulDiv(FPageSize.X, PageHeight100Percent, PageWidth100Percent);
          end;
          smWholePage:
          begin
            FPageSize.X := Max(ClientWidth - 2 * cPreviewHorzBorder - cPreviewShadowSize, 40);
            FPageSize.Y := Max(ClientHeight - 2 * cPreviewVertBorder - cPreviewShadowSize, 40);
            I := MulDiv(FPageSize.Y, PageWidth100Percent, PageHeight100Percent);
            if I < FPageSize.X then
              FPageSize.X := I
            else
              FPageSize.Y := MulDiv(FPageSize.X, PageHeight100Percent, PageWidth100Percent);
          end;
        end;
        FExtent.X := FPageSize.X + 2 * cPreviewHorzBorder + cPreviewShadowSize;
        FExtent.Y := FPageSize.Y + 2 * cPreviewVertBorder + cPreviewShadowSize;
        FPageOffset.X := cPreviewHorzBorder;
        if (FExtent.X < ClientWidth) then
          Inc(FPageOffset.X, (ClientWidth - FExtent.X) div 2);
        FPageOffset.Y := cPreviewVertBorder;
        if (FExtent.Y < ClientHeight) then
          Inc(FPageOffset.Y, (ClientHeight - FExtent.Y) div 2);
        // adjust horizontal scroll position
        I := FScrollPos.X + ClientWidth - FExtent.X - 1;
        if I > 0 then
          Dec(FScrollPos.X, I);
        FScrollPos.X := Max(FScrollPos.X, 0);
        // adjust vertical scroll position
        I := FScrollPos.Y + ClientHeight - FExtent.Y - 1;
        if I > 0 then
          Dec(FScrollPos.Y, I);
        FScrollPos.Y := Max(FScrollPos.Y, 0);
        // update scroll range
        FScrollExtent.X := 0;
        FScrollExtent.Y := 0;
        FillChar(SI, SizeOf(TScrollInfo), 0);
        SI.cbSize := SizeOf(TScrollInfo);
        SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS or SIF_DISABLENOSCROLL {$IFDEF UNIX}or SIF_UPDATEPOLICY{$ENDIF};
        SI.nMin := 0;
      {$IFDEF UNIX}
        SI.ntrackPos := SB_POLICY_CONTINUOUS;
      {$ENDIF}
        case FScaleMode of
          smScale:
          begin
            ShowScrollbar(Handle, SB_HORZ, True);
            ShowScrollbar(Handle, SB_VERT, True);
            SI.nMax := FExtent.X{$IFDEF FPC}+ 1{$ENDIF};
            SI.nPage := ClientWidth;
            SI.nPos := FScrollPos.X;
            FScrollExtent.X := SI.nMax - Integer(SI.nPage);
            SetScrollInfo(Handle, SB_HORZ, SI, True);
            SI.nMax := FExtent.Y{$IFDEF FPC}+ 1{$ENDIF};
            SI.nPage := ClientHeight;
            SI.nPos := FScrollPos.Y;
            FScrollExtent.Y := SI.nMax - Integer(SI.nPage);
            SetScrollInfo(Handle, SB_VERT, SI, True);
          end;
          smPageWidth:
          begin
            ShowScrollbar(Handle, SB_HORZ, False);
            ShowScrollbar(Handle, SB_VERT, True);
            SI.nMax := FExtent.Y{$IFDEF FPC}+ 1{$ENDIF};
            SI.nPage := ClientHeight;
            SI.nPos := FScrollPos.Y;
            FScrollExtent.Y := SI.nMax - Integer(SI.nPage);
            SetScrollInfo(Handle, SB_VERT, SI, True);
          end;
          smWholePage:
          begin
            // another mode for vertical scrollbar - page selection
            ShowScrollbar(Handle, SB_HORZ, False);
            ShowScrollbar(Handle, SB_VERT, True);
            SI.nMin := StartPage;
            SI.nMax := EndPage{$IFDEF FPC}+ 1{$ENDIF};
            SI.nPage := 1;
            SI.nPos := FPage;
            SetScrollInfo(Handle, SB_VERT, SI, True);
          end;
        end;
      end else
      begin
        ShowScrollbar(Handle, SB_HORZ, False);
        ShowScrollbar(Handle, SB_VERT, False);
      end;
      Invalidate;
    finally
      FlagClear(cPF_UpdateRange);
    end;
  end;
end;

procedure TKPrintPreview.UpdateSize;
begin
  inherited;
  UpdatePreview;
end;

procedure TKPrintPreview.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TKPrintPreview.WMGetDlgCode(var Msg: TLMNoParams);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TKPrintPreview.WMHScroll(var Msg: TLMHScroll);
begin
  SafeSetFocus;
  BeginScrollWindow;
  ModifyScrollBar(SB_HORZ, Msg.ScrollCode, Msg.Pos);
  EndScrollWindow;
end;

procedure TKPrintPreview.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TKPrintPreview.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TKPrintPreview.WMVScroll(var Msg: TLMVScroll);
begin
  SafeSetFocus;
  BeginScrollWindow;
  ModifyScrollBar(SB_VERT, Msg.ScrollCode, Msg.Pos);
  EndScrollWindow;
end;

{$IFDEF FPC}
initialization
  {$i kcontrols.lrs}
{$ELSE}
  {$R kcontrols.res}
{$ENDIF}
end.
