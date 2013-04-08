// -------------------------------------------------------------------------------------------------
// This file contains class for HTML to PDF conversion using great components HTMLVIEW and SynPdf.
// Thanks for authors of these components: HTMLVIEW - L. David Baldwin and SynPdf - Arnaud Bouchez.
// This class was written by Pawel Stroinski on 2010/07/07 and is public domain.
// -------------------------------------------------------------------------------------------------

{ AB enhancements:
  - drawing clipping, since THtmlView sometime writes outside the margins :(
  - optional page number print }

unit Html2Pdf;

interface

uses SynPdf, HTMLView, Printers, Windows, Forms, SysUtils, Classes, Graphics;

type
  THtml2Pdf = class(TPdfDocument)
  private
    function Points2Pixels(APoints: Single): Integer;
    function Centimeters2Points(ACentimeters: Double): Single;
    function GetOrientation: TPrinterOrientation;
    procedure SetOrientation(const Value: TPrinterOrientation);
    procedure SetDefaultPaperSize(const Value: TPDFPaperSize);
  public
    Viewer: THTMLViewer;
    MarginLeft, MarginTop, MarginRight, MarginBottom: Double;
    ScaleToFit: Boolean;
    DrawPageNumber: Boolean;
    DrawPageNumberText: string;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property DefaultPaperSize: TPDFPaperSize write SetDefaultPaperSize;
    procedure Execute;
  end;

implementation


{ THtml2Pdf }

const
  CPointsPerInch = 72;
  CCentimetersPerInch = 2.54;

function THtml2Pdf.Points2Pixels(APoints: Single): Integer;
begin
  Result := Round(APoints / CPointsPerInch * Screen.PixelsPerInch);
end;

function THtml2Pdf.Centimeters2Points(ACentimeters: Double): Single;
begin
  Result := ACentimeters / CCentimetersPerInch * CPointsPerInch;
end;

procedure THtml2Pdf.SetOrientation(const Value: TPrinterOrientation);
var
  LTmp: Integer;
begin
  if Value <> Orientation then
  begin
    LTmp := DefaultPageWidth;
    DefaultPageWidth := DefaultPageHeight;
    DefaultPageHeight := LTmp;
  end;
end;

function THtml2Pdf.GetOrientation: TPrinterOrientation;
begin
  Result := TPrinterOrientation(Ord(DefaultPageWidth > DefaultPageHeight));
end;

procedure THtml2Pdf.SetDefaultPaperSize(const Value: TPDFPaperSize);
var
  LB: TPrinterOrientation;
begin
  LB := Orientation;
  inherited DefaultPaperSize := Value;
  Orientation := LB;
end;

procedure THtml2Pdf.Execute;
var
  LFormatWidth, LWidth, LHeight, LI: Integer;
  LPages: TList;
  LPage: TMetafile;
  LScale: Single;
  LMarginX, LMarginY, LPointsWidth,  LPointsHeight, LMarginBottom: Single;
  PageText: string;
begin
  //ForceJPEGCompression := 80;
  LPointsWidth := DefaultPageWidth - Centimeters2Points(MarginLeft + MarginRight);
  LFormatWidth := Points2Pixels(LPointsWidth);
  LWidth := Viewer.FullDisplaySize(LFormatWidth).cx;
  if ScaleToFit and (LWidth > LFormatWidth) and (LFormatWidth > 0) then
    LScale := LFormatWidth / LWidth
  else
    LScale := 1;
  LPointsHeight := (DefaultPageHeight - Centimeters2Points(MarginTop + MarginBottom)) / LScale;
  LHeight := Points2Pixels(LPointsHeight);
  LPages := Viewer.MakePagedMetaFiles(LWidth, LHeight);
  LMarginX := Centimeters2Points(MarginLeft);
  LMarginY := -Centimeters2Points(MarginTop);
  LMarginBottom := Centimeters2Points(MarginBottom);
  for LI := 0 to LPages.Count - 1 do
  begin
    AddPage;
    LPage := TMetafile(LPages[LI]);
    with Canvas do begin
      GSave;
      Rectangle(LMarginX,LMarginBottom,LPointsWidth,LPointsHeight);
      Clip; // THtmlView may print out of the margins ;)
      RenderMetaFile(LPage, LScale, LMarginX, LMarginY);
      GRestore;
      if DrawPageNumber then begin
        if DrawPageNumberText='' then
          PageText := 'Page %d/%d' else
          PageText := DrawPageNumberText;
        PageText := Format(PageText,[LI+1,LPages.Count]);
        SetFont('Arial',9,[]);
        SetRGBStrokeColor(clBlack);
        TextOut(LMarginX+(LPointsWidth-TextWidth(PDFString(PageText)))/2,
          LMarginBottom-9, PDFString(PageText));
      end;
    end;
    //LPage.SaveToFile('Page'+IntToStr(LI+1)+'.emf');
    FreeAndNil(LPage);
  end;
  FreeAndNil(LPages);
end;

end.

