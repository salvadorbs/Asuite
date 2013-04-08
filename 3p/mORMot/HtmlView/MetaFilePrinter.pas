{Version 10.1}
{***************************************************************}
{*                  METAFILEPRINTER.PAS                        *}
{*                                                             *}
{*              Thanks to Chris Wallace for the                *}
{*         ideas and most of the code for Print Preview        *)
{*                                                             *}
{*              Bugs introduced by Dave Baldwin                *}
{***************************************************************}



unit MetaFilePrinter;

{$I htmlcons.inc}

interface

uses
  Classes, Graphics, Printers;

type
  TUnits = (unInches, unCentimeters);
  TPageEvent = procedure(Sender: TObject; NumPage: Integer;
    var StopPrinting: Boolean) of object;

  TMetaFilePrinter = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FPrinting: boolean;
    FMFList: TList;
    FCurCanvas: TCanvas;
    FPPIX: integer;
    FPPIY: integer;
    FPaperWidth: integer;
    FPaperHeight: integer;
    FOffsetX: integer;
    FOffsetY: integer;
    FPgHeight: integer;
    FPgWidth: integer;
    FUnits: TUnits;
    FConvFac: double;
    FUsedPage: boolean;
    FPrinterDC: THandle;
    FOnPageEvent: TPageEvent;

    function GetCanvas: TCanvas;
    function GetPageNum: integer;
    procedure FreeMetaFiles;
    function GetMetaFile(I: integer): TMetaFile;
    procedure SetUnits(Val: TUnits);
    function GetLastAvailPage: integer;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

      // Printer Methods
    procedure BeginDoc; virtual;
    procedure NewPage; virtual;
    procedure EndDoc;
    procedure Abort;
//BG, 01.12.2006: beg of modification
    procedure getPrinterCapsOf(Printer: TPrinter);
//BG, 01.12.2006: end of modification

    property MetaFiles[I: integer]: TMetaFile read GetMetaFile;
    property PixelsPerInchX: integer read FPPIX;
    property PixelsPerInchY: integer read FPPIY;
    property PaperWidth: integer read FPaperWidth;
    property PaperHeight: integer read FPaperHeight;
    property PageHeight: integer read FPgHeight;
    property PageWidth: integer read FPgWidth;
    property OffsetX: integer read FOffsetX;
    property OffsetY: integer read FOffsetY;
    property Canvas: TCanvas read GetCanvas;
    property PageNumber: integer read GetPageNum;
    property Printing: boolean read FPrinting;
    property LastAvailablePage: integer read GetLastAvailPage;
    property PrinterDC: THandle read FPrinterDC;
  published
    { Published declarations }
    property Units: TUnits read FUnits write SetUnits;
    property OnPageEvent: TPageEvent read FOnPageEvent write FOnPageEvent;
  end;

implementation

uses
  Windows, SysUtils, Forms {$IFNDEF NoGDIPlus}, GDIPL2A{$ENDIF NoGDIPlus};

const
  INCH_TO_CM = 2.54;

// TMetaFilePrinter

constructor TMetaFilePrinter.Create(AOwner: TComponent);
begin
  inherited;
  FMFList := TList.Create;
  FUnits := unInches;
end;

destructor TMetaFilePrinter.Destroy;
begin
  FreeMetaFiles;
  FMFList.Free;
  inherited;
end;


procedure TMetaFilePrinter.FreeMetaFiles;
var
  I: integer;
begin
  for I := 0 to FMFList.Count - 1 do
    MetaFiles[I].Free;
  FMFList.Clear;
  FreeAndNil(FCurCanvas);
end;

function TMetaFilePrinter.GetMetaFile(I: integer): TMetaFile;
begin
  Result := FMFList[I];
end;

procedure TMetaFilePrinter.SetUnits(Val: TUnits);
begin
  FUnits := Val;
  case FUnits of
    unInches: FConvFac := 1;
    unCentimeters: FConvFac := INCH_TO_CM;
  end;
end;

//BG, 01.12.2006: beg of modification: extracted getPrinterCapsOf from BeginDoc

procedure TMetaFilePrinter.getPrinterCapsOf(Printer: TPrinter);
begin
  if Printer.Printers.Count = 0 then
    raise Exception.Create('Printer not available');

  FPrinterDC := Printer.Handle;
  FPPIX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  FPPIY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  FPaperWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  FPaperHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  FOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  FOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  FPgHeight := Printer.PageHeight;
  FPgWidth := Printer.PageWidth;
end;

procedure TMetaFilePrinter.BeginDoc;
begin
  FPrinting := True;
  FreeMetaFiles;

  getPrinterCapsOf(Printer);

  NewPage;
end;
//BG, 01.12.2006: end of modification

procedure TMetaFilePrinter.EndDoc;
var
  I: integer;
begin
  FPrinting := False;
  FCurCanvas.Free;
  FCurCanvas := nil;
  FPrinterDC := 0;

   // in case NewPage was called but nothing drawn on it
  if not FUsedPage then
  begin
    I := FMFList.Count - 1;
    MetaFiles[FMFList.Count - 1].Free;
    FMFList.Delete(I);
  end;

end;

procedure TMetaFilePrinter.Abort;
begin
  FPrinting := False;
  FCurCanvas.Free;
  FCurCanvas := nil;
  FreeMetaFiles;
end;

procedure TMetaFilePrinter.NewPage;
var
  MetaFile: TMetaFile;
  NewCanvas: TCanvas;
  Done: boolean;
begin
  MetaFile := TMetaFile.Create;
  FMFList.Add(MetaFile);

  {$IFNDEF NoGDIPlus}
  if GDIPlusActive then                                  
    NewCanvas := TMetaFileCanvas.Create(MetaFile, Printer.Handle)
  else
    {$ENDIF NoGDIPlus}
    NewCanvas := TMetaFileCanvas.Create(MetaFile, 0);
   { fill the page with "whiteness" }
  NewCanvas.Brush.Color := clWhite;
  NewCanvas.Pen.Color := clWhite;
  NewCanvas.Brush.Style := bsSolid;
  NewCanvas.Rectangle(0, 0, FPaperWidth, FPaperHeight);

  if FCurCanvas = nil then
  begin
    NewCanvas.Font.PixelsPerInch := Screen.PixelsPerInch;
    NewCanvas.Font.Name := 'Arial';
    NewCanvas.Font.Size := 10;
    NewCanvas.Brush.Style := bsClear;
  end;

  FCurCanvas.Free;
  FCurCanvas := NewCanvas;
  FUsedPage := False;

  if Assigned(FOnPageEvent) then
  begin
    Done := False;
    FOnPageEvent(Self, FMFList.Count, Done);
  end;
end;

function TMetaFilePrinter.GetPageNum: integer;
begin
  Result := FMFList.Count;
end;

function TMetaFilePrinter.GetLastAvailPage: integer;
begin
  Result := GetPageNum;
end;

function TMetaFilePrinter.GetCanvas: TCanvas;
begin
  Result := FCurCanvas;
  FUsedPage := True;
end;

end.
