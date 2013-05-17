{ cyDER  Components
  Description: Document Elements Recognition classes
}

unit cyDocER;

interface

uses
  Classes, Windows, Controls, SysUtils, cyStrUtils, cyDERUtils, VCL.cyGraphics, cyDateUtils;

type
  TReadingDirection = (rdLeft, rdTop, rdRight, rdBottom);
  TReadingDirections = set of TReadingDirection;

  TOCRCarInfo = record
    Value: String;
    PageNumber: Integer;
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
    WordNumber: Integer;
  end;

  TcyDocER = class;

  TOCRExpression = class
  private
    FValue: String;
    FAssociatedElementIndex: Integer;
    FAssociatedElementKeywordIndex: Integer;
    FAssociatedExpressionKeywordIndex: Integer; // Connexion to the expression's keyword found!
    FRecognizedFloat: String;
    FRecognizedNumbers: String;
    FRecognizedInteger: String;
    FRecognizedPercentage: String;
    FRecognizedWebMail: String;
    FRecognizedWebsite: String;
    FRecognizedMoney: String;
    FRecognizedDate: String;
    FRightPxPos: Integer;
    FBottomPxPos: Integer;
    FTopPxPos: Integer;
    FLeftPxPos: Integer;
    FPageNumber: Integer;
    FDERValue: DERString;
    FRecognizedType: TElementsType;
    FOCRConfidence: Extended;
  protected
  public
    // OCR information :
    function RecognizedValue: String;
    property LeftPxPos: Integer read FLeftPxPos;
    property TopPxPos: Integer read FTopPxPos;
    property RightPxPos: Integer read FRightPxPos;
    property BottomPxPos: Integer read FBottomPxPos;
    property Value: String read FValue write FValue;
    property PageNumber: Integer read FPageNumber;
    // DER information :
    property DERValue: DERString read FDERValue;
    property AssociatedElementIndex: Integer read FAssociatedElementIndex write FAssociatedElementIndex;
    property AssociatedElementKeywordIndex: Integer read FAssociatedElementKeywordIndex write FAssociatedElementKeywordIndex;
    property AssociatedExpressionKeywordIndex: Integer read FAssociatedExpressionKeywordIndex write FAssociatedExpressionKeywordIndex;
    property OCRConfidence: Extended read FOCRConfidence write FOCRConfidence;
    property RecognizedType: TElementsType read FRecognizedType write FRecognizedType;
    property RecognizedDate: String read FRecognizedDate write FRecognizedDate;
    property RecognizedMoney: String read FRecognizedMoney write FRecognizedMoney;
    property RecognizedWebsite: String read FRecognizedWebsite write FRecognizedWebsite;
    property RecognizedWebMail: String read FRecognizedWebMail write FRecognizedWebMail;
    property RecognizedPercentage: String read FRecognizedPercentage write FRecognizedPercentage;
    property RecognizedFloat: String read FRecognizedFloat write FRecognizedFloat;
    property RecognizedInteger: String read FRecognizedInteger write FRecognizedInteger;
    property RecognizedNumbers: String read FRecognizedNumbers write FRecognizedNumbers;
  end;



  TSearchValueLocation = (slFromLeft, slFromTop, slFromRight, slFromBottom);
  TPatternPositionMode = (ppFromTopLeftPage, ppFromTopLeftKeyword);
  TPatternPageNumberMode = (ppFromBeginning, ppFromEnd);

  TElement = class(TCollectionItem)
  private
    FTag: Integer;
    FKeyWords: TStrings;
    FPatternFromLeftMm: Double;
    FPatternFromTopMm: Double;
    FPatternToRightMm: Double;
    FPatternToBottomMm: Double;
    FPatternValueLocation: TSearchValueLocation;
    FPatternValueNumber: Integer;
    FValueCount: Integer;
    FPatternPageNumber: Integer;
    FPatternPageNumberMode: TPatternPageNumberMode;
    FValueType: TElementsType;
    FPatternPositionMode: TPatternPositionMode;
    procedure SetKeyWords(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function PatternRecognition: boolean;
  published
    property KeyWords: TStrings read FKeyWords write SetKeyWords;
    property PatternPageNumber: Integer read FPatternPageNumber write FPatternPageNumber default 0;
    property PatternPageNumberMode: TPatternPageNumberMode read FPatternPageNumberMode write FPatternPageNumberMode default ppFromBeginning;
    property PatternFromLeftMm: Double read FPatternFromLeftMm write FPatternFromLeftMm;
    property PatternFromTopMm: Double read FPatternFromTopMm write FPatternFromTopMm;
    property PatternToRightMm: Double read FPatternToRightMm write FPatternToRightMm;
    property PatternToBottomMm: Double read FPatternToBottomMm write FPatternToBottomMm;
    property PatternPositionMode: TPatternPositionMode read FPatternPositionMode write FPatternPositionMode default ppFromTopLeftPage;
    property PatternValueLocation: TSearchValueLocation read FPatternValueLocation write FPatternValueLocation default slFromBottom;
    property PatternValueNumber: Integer read FPatternValueNumber write FPatternValueNumber default 1;   // Which value must be returned if more than one value in defined area
    property ValueCount: Integer read FValueCount write FValueCount default 1;          // Number of values that must be retrieved
    property ValueType: TElementsType read FValueType write FValueType default etMoney;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TElementClass = class of TElement;

  TElements = Class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetElement(Index: Integer): TElement;
  protected
    function GetOwner: TPersistent; Override;
    procedure Update(Item: TCollectionItem); Override;
  public
    constructor Create(aDocument: TcyDocER; ElementClass: TElementClass);
    function Add: TElement;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TElement read GetElement; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TRecognitionElementsMode = (reKeywords, reValues, reKeywordsAndValues);
  TRecognitionPriorityMode = (rpKeywordsLenth, rpSinglelineKeywordsLength);
  TRecognitionOption = (roKeywordsByPriority, roSmartNumbersRec, roSmartWebsiteRec, roSmartKeywordRec);
  TRecognitionOptions = Set of TRecognitionOption;

  TLocateExpressionOption = (lePartialKey, leRelativePositionKey, leInsensitive, leSmartKeywordRec);
  TLocateExpressionOptions = Set of TLocateExpressionOption;

  TValidateElementValueResult =(veValueOk, veInvalidValue, veInvalidValueStopSearching, veValueTooFar);

  TProcOnExpressionMerged = procedure (Sender: TObject; aExpressionIndex, toExpressionIndex: Integer) of object;
  TProcOnRecognizeElementKeyword = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex: Integer) of object;
  TProcValidateElementValue = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex, ExpressionKeywordIndex, ExpressionValueIndex: Integer; var ValidateElementValueResult: TValidateElementValueResult) of object;
  TProcValidateElementKeyword = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex: Integer; ExpressionsList: TStrings; var Accept: Boolean) of object;
  TProcRetrieveElementValuesFromRect = procedure (Sender: TObject; ElementIndex, ElementKeywordIndex, ExpressionKeywordIndex: Integer; ExpressionList: TStrings) of object;

  // For individual component ... [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TcyDocER = class(TComponent)
  private
    FExpressions: TList;  // TOCRExpression objects list
    FElements: TElements;
    FPageCount: Integer;
    FResolution: Integer;
    FPixelsWidth: Integer;
    FPixelsHeight: Integer;
    FRecognitionOptions: TRecognitionOptions;
    FBeforeRecognizeElementsValues: TNotifyEvent;
    FBeforeRecognizeElementsKeywords: TNotifyEvent;
    FBeforeRecognizeElementKeyword: TProcOnRecognizeElementKeyword;
    FAfterRecognizeElementKeyword: TProcOnRecognizeElementKeyword;
    FOnValidateElementValue: TProcValidateElementValue;
    FOnValidateElementKeyword: TProcValidateElementKeyword;
    FRecognitionPriorityMode: TRecognitionPriorityMode;
    FOnRetrieveElementValuesFromRect: TProcRetrieveElementValuesFromRect;
    FOnExpressionMerged: TProcOnExpressionMerged;
    FShortMonthNames: TStrings;
    function GetExpressions(Index: Integer): TOCRExpression;
    function GetExpressionCount: Integer;
    procedure SetElements(const Value: TElements);
    procedure SetRecognitionOptions(const Value: TRecognitionOptions);
    procedure SetShortMonthNames(const Value: TStrings);
  protected
    procedure ExtractTesseractBoxlineInfo(aLine: String; var RsltCar: String; var RsltPage, RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
    function RecognizeMonth(aDERString: DERString): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // OCR feeding :
    procedure NewPage(ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
    procedure LoadFromTesseractString(aString: String; aPageNumber: Integer);
    procedure LoadFromTesseractFile(aFilename: String; aPageNumber: Word{$IFDEF UNICODE}; aEncoding: TEncoding{$ENDIF});
    procedure LoadFromTesseractBoxStringList(aStrings: TStrings; ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
    procedure LoadFromTesseractBoxFile(aFilename: String; {$IFDEF UNICODE}aEncoding: TEncoding; {$ENDIF}ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
    procedure LoadFromStringList(aStrings: TStrings);
    // Load / Save :
    procedure LoadFromFile(aFilename: String{$IFDEF UNICODE}; aEncoding: TEncoding{$ENDIF});
    procedure SaveToStringList(aStrings: TStrings);
    procedure SaveToFile(aFilename: String{$IFDEF UNICODE}; aEncoding: TEncoding{$ENDIF});
    // Utilities :
    function MmToPx(MmValue: Double): Integer;
    function PxToMm(pxValue: Extended): Double;
    procedure ClearExpressions;
    function GetOCRText(const FromPage: Integer = 0): String;
    function GetOCRTextFromRect(FromRect: TRect; FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
    function GetAsDocumentOCRText(const FromPage: Integer = 0): String;
    function GetAsDocumentOCRTextFromRect(FromRect: TRect; FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
    procedure RotatePageExpressions(PageNumber, PageWidthBeforeRotation, PageHeightBeforeRotation: Integer; ToRight: Boolean);
    function ExpressionInRect(aExpressionIndex: Integer; InRect: TRect; const AllowPartiallyInside: Boolean = true): Boolean;
    procedure RecognizeExpressionType(aExpressionIndex: Integer);
    function AddExpression(aString: String; aPageNumber: Word; aOCRConfidence: Extended; aRect: TRect): Integer; overload;
    function AddExpression(aString: String; aPageNumber: Word; aOCRConfidence: Extended): Integer; overload;
    procedure ExpressionAdded;
    procedure ExpressionLoaded;
    procedure DeleteExpression(aExpressionIndex: Integer);
    function LocateExpression(Value: String; FromIndex, ToIndex, MaxCarErrors: Integer; Options: TLocateExpressionOptions; var RsltCarPos: Integer): Integer;
    function ExpressionsInSameLine(aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
    procedure GetAroundExpressions(aExpressionIndex, MaxCarErrors, ScopePx: Integer; SearchValue: String; SearchOptions: TLocateExpressionOptions; var RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
    procedure MergeExpressions(aExpressionIndex, ToExpressionIndex: Integer);
    function ExpressionsSideBySide(ExpressionIndexAtLeft, ExpressionIndexAtRight, MaxPxSpacing: Integer): Boolean;
    function IsElementKeyword(aElementIndex: Integer; aStr: String): Integer;
    function FindExpression(StartIndex, AssociatedElementIndex, AssociatedElementKeywordIndex, AssociatedExpressionKeywordIndex: Integer; IncludeElementsTypes, ExcludeElementsTypes: TElementsTypes): Integer; overload;
    function FindExpression(Value: Variant; ValueType: TElementsType; FromIndex, ToIndex: Integer): Integer; overload;
    procedure DissociateExpressions(FromElementIndex, FromElementKeywordIndex: Integer);
    procedure InitializeRecognition;
    function RecognizeElementKeyword(ElementIndex, KeywordIndex: Integer; InlineKeyword: Boolean): Boolean;
    function RecognizeElementValuesFromRect(OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex: Integer; aValueType: TElementsType;
     aPageNumber, LeftPos, TopPos, RightPos, BottomPos: Integer; ValueLocation: TSearchValueLocation;
     ValueNumber, ValueCount: Integer; PatternMode: Boolean; var UserAbort: Boolean): Integer;
     procedure RecognizeLongDates;
    procedure RecognizeElementValues(ElementIndex: Integer);
    procedure RecognizeElementsKeywords;
    procedure RecognizeElementsValues;
    procedure RecognizeElements;
    // Properties :
    property ExpressionCount: Integer read GetExpressionCount;
    property Expressions[Index: Integer]: TOCRExpression read GetExpressions; default;
    property PageCount: Integer read FPageCount;
    property PixelsHeight: Integer read FPixelsHeight;
    property PixelsWidth: Integer read FPixelsWidth;
    property Resolution: Integer read FResolution;
  published
    property ShortMonthNames: TStrings read FShortMonthNames write SetShortMonthNames;
    property Elements: TElements read FElements write SetElements;
    property RecognitionOptions: TRecognitionOptions read FRecognitionOptions write SetRecognitionOptions default [roSmartNumbersRec, roSmartWebsiteRec, roSmartKeywordRec, roKeywordsByPriority];
    property RecognitionPriorityMode: TRecognitionPriorityMode read FRecognitionPriorityMode write FRecognitionPriorityMode default rpSinglelineKeywordsLength;
    property BeforeRecognizeElementsKeywords: TNotifyEvent read FBeforeRecognizeElementsKeywords write FBeforeRecognizeElementsKeywords;
    property BeforeRecognizeElementsValues: TNotifyEvent read FBeforeRecognizeElementsValues write FBeforeRecognizeElementsValues;
    property BeforeRecognizeElementKeyword: TProcOnRecognizeElementKeyword read FBeforeRecognizeElementKeyword write FBeforeRecognizeElementKeyword;
    property AfterRecognizeElementKeyword: TProcOnRecognizeElementKeyword read FAfterRecognizeElementKeyword write FAfterRecognizeElementKeyword;
    property OnValidateElementKeyword: TProcValidateElementKeyword read FOnValidateElementKeyword write FOnValidateElementKeyword;
    property OnRetrieveElementValuesFromRect: TProcRetrieveElementValuesFromRect read FOnRetrieveElementValuesFromRect write FOnRetrieveElementValuesFromRect;
    property OnValidateElementValue: TProcValidateElementValue read FOnValidateElementValue write FOnValidateElementValue;
    property OnExpressionMerged: TProcOnExpressionMerged read FOnExpressionMerged write FOnExpressionMerged;
  end;

implementation

{ TOCRExpression }
function TOCRExpression.RecognizedValue: String;
begin
  case FRecognizedType of
    etText:              Result := FValue;
    etExpressionKeyWord: Result := FValue;
    etNumbers:           Result := FRecognizedNumbers;
    etInteger:           Result := FRecognizedInteger;
    etFloat:             Result := FRecognizedFloat;
    etPercentage:        Result := FRecognizedPercentage;
    etwebSite:           Result := FRecognizedWebsite;
    etWebMail:           Result := FRecognizedWebMail;
    etMoney:             Result := FRecognizedMoney;
    etDate:              Result := FRecognizedDate;
  end;
end;

{ TElement }
constructor TElement.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FKeyWords := TStringList.Create;
  FPatternPageNumber := 0;
  PatternPageNumberMode := ppFromBeginning;
  FPatternFromLeftMm := 0;
  FPatternFromTopMm := 0;
  FPatternToRightMm := 0;
  FPatternToBottomMm := 0;
  FPatternPositionMode := ppFromTopLeftPage;
  FPatternValueLocation := slFromBottom;
  FPatternValueNumber := 1;
  FValueCount := 1;
  FValueType := etMoney;
  FTag := 0;
end;

destructor TElement.Destroy;
begin
  FKeyWords.Free;
  inherited;
end;

function TElement.GetDisplayName: string;
var k: Integer;
begin
  Result := '';

  for k := 0 to FKeyWords.Count-1 do
    if Result = ''
    then Result := FKeyWords[k]
    else Result := Result + ', ' + FKeyWords[k];
end;

function TElement.PatternRecognition: boolean;
begin
  Result := (FPatternFromLeftMm <> 0) or (FPatternFromTopMm <> 0) or (FPatternToRightMm <> 0) or (FPatternToBottomMm <> 0);
end;

procedure TElement.SetKeyWords(const Value: TStrings);
begin
  if Assigned(FKeyWords) then
    FKeyWords.Assign(Value)
  else
    FKeyWords := Value;
end;

procedure TElement.Assign(Source: TPersistent);
begin
  if Source is TElement then
  begin
    FKeyWords  := TElement(Source).FKeyWords;
    FPatternPageNumber := TElement(Source).FPatternPageNumber;
    FPatternPageNumberMode := TElement(Source).FPatternPageNumberMode;
    FPatternFromLeftMm := TElement(Source).FPatternFromLeftMm;
    FPatternFromTopMm := TElement(Source).FPatternFromTopMm;
    FPatternToRightMm := TElement(Source).FPatternToRightMm;
    FPatternToBottomMm := TElement(Source).FPatternToBottomMm;
    FPatternPositionMode := TElement(Source).FPatternPositionMode;
    FPatternValueLocation := TElement(Source).FPatternValueLocation;
    FPatternValueNumber := TElement(Source).FPatternValueNumber;
    FValueCount := TElement(Source).FValueCount;
    FValueType := TElement(Source).FValueType;
    FTag := TElement(Source).FTag;
  end;
//  inherited Assign(Source);
end;

{ TElements }
constructor TElements.Create(aDocument: TcyDocER; ElementClass: TElementClass);
begin
  inherited Create(ElementClass);
  FOwner := aDocument;
end;

function TElements.Add: TElement;
begin
  Result := TElement(inherited Add);
  Result.Changed(false);      // It will call TcyElements.Update only at run-time!
end;

procedure TElements.Delete(Index: Integer);
begin
  Inherited;
  Update(Nil);
end;

function TElements.GetElement(Index: Integer): TElement;
begin
  Result := TElement(inherited Items[Index]);
end;

function TElements.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// Event Called by setting properties/events of TElement :
procedure TElements.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;



{ TcyDocER }
constructor TcyDocER.Create(AOwner: TComponent);
var
  m: Integer;
  {$IFDEF DCC} // Delphi XE2/XE3 Win 32/64
  fs: TFormatSettings;
  {$ENDIF DCC}
begin
  inherited Create(AOwner);
  FExpressions := TList.Create;
  FShortMonthNames := TStringList.Create;

  {$IFDEF DCC} // Delphi XE2/XE3 Win 32/64
  fs := TFormatSettings.Create;
  for m := 1 to 12 do
    FShortMonthNames.Add(fs.ShortMonthNames[m]);
  {$ELSE} // Delphi other
  for m := 1 to 12 do
    FShortMonthNames.Add(SysUtils.ShortMonthNames[m]);
  {$ENDIF DCC}

  FElements := TElements.Create(self, TElement);
  FResolution := 0;
  FPageCount := 0;
  FPixelsHeight := 0;
  FPixelsWidth := 0;
  FRecognitionOptions := [roKeywordsByPriority, roSmartNumbersRec, roSmartWebsiteRec, roSmartKeywordRec];
  FRecognitionPriorityMode := rpSinglelineKeywordsLength;
end;

destructor TcyDocER.Destroy;
begin
  ClearExpressions;
  FShortMonthNames.Free;
  FElements.Free;
  FElements := Nil;
  inherited;
end;

procedure TcyDocER.DissociateExpressions(FromElementIndex, FromElementKeywordIndex: Integer);
var i: Integer;
begin
  for i := 0 to ExpressionCount-1 do
    if (Expressions[i].FAssociatedElementIndex = FromElementIndex) or (FromElementIndex = -1) then
      if (Expressions[i].FAssociatedElementKeywordIndex = FromElementKeywordIndex) or (FromElementKeywordIndex = -1) then
      begin
        Expressions[i].FAssociatedElementIndex := -1;
        Expressions[i].FAssociatedElementKeywordIndex := -1;
        Expressions[i].FAssociatedExpressionKeywordIndex := -1;
        if Expressions[i].FRecognizedType = etExpressionKeyWord then
          RecognizeExpressionType(i);
      end;
end;

procedure TcyDocER.SetShortMonthNames(const Value: TStrings);
begin
  if Assigned(FShortMonthNames) then
    FShortMonthNames.Assign(Value)
  else
    FShortMonthNames := Value;
end;

procedure TcyDocER.SetElements(const Value: TElements);
begin
  FElements := Value;
end;

procedure TcyDocER.SetRecognitionOptions(const Value: TRecognitionOptions);
begin
  FRecognitionOptions := Value;
end;

procedure TcyDocER.ClearExpressions;
var i: Integer;
begin
  // Free Expressions :
  for i := 0 to ExpressionCount-1 do
    try
      Expressions[i].Free;
    except

    end;

  FExpressions.Clear;
end;

function TcyDocER.GetExpressionCount: Integer;
begin
  Result := FExpressions.Count;
end;

function TcyDocER.GetExpressions(Index: Integer): TOCRExpression;
begin
  Result := FExpressions[Index];
end;

procedure TcyDocER.RecognizeExpressionType(aExpressionIndex: Integer);
var _Numbers, _Integer, _Float, _Percentage, _webSite, _WebMail, _Money, _Date: String;
begin
  Expressions[aExpressionIndex].FDERValue := StringToDERCharSet(Expressions[aExpressionIndex].FValue);

  Expressions[aExpressionIndex].FRecognizedType := DERExecute(Expressions[aExpressionIndex].FDERValue,_Numbers,_Integer,_Float,_Percentage,_webSite,_WebMail,_Money,_Date,
                                                      roSmartNumbersRec in FRecognitionOptions, roSmartWebsiteRec in FRecognitionOptions);
  Expressions[aExpressionIndex].FRecognizedDate       := _Date;
  Expressions[aExpressionIndex].FRecognizedMoney      := _Money;
  Expressions[aExpressionIndex].FRecognizedWebsite    := _Website;
  Expressions[aExpressionIndex].FRecognizedWebMail    := _WebMail;
  Expressions[aExpressionIndex].FRecognizedPercentage := _Percentage;
  Expressions[aExpressionIndex].FRecognizedFloat      := _Float;
  Expressions[aExpressionIndex].FRecognizedInteger    := _Integer;
  Expressions[aExpressionIndex].FRecognizedNumbers    := _Numbers;
end;

function TcyDocER.AddExpression(aString: String; aPageNumber: Word; aOCRConfidence: Extended; aRect: TRect): Integer;
begin
  Result := AddExpression(aString, aPageNumber, aOCRConfidence);

  if Result <> -1 then
  begin
    Expressions[Result].FLeftPxPos := aRect.Left;
    Expressions[Result].FTopPxPos := aRect.Top;
    Expressions[Result].FRightPxPos := aRect.Right;
    Expressions[Result].FBottomPxPos := aRect.Bottom;

    ExpressionAdded;  // Merge Expression in some cases ...
  end;
end;

function TcyDocER.AddExpression(aString: String; aPageNumber: Word; aOCRConfidence: Extended): Integer;
var NewExpression: TOCRExpression;
begin
  Result := -1;
  if aString = '' then Exit;
  if StringToDERCharSet(aString) = '' then Exit;

  NewExpression := TOCRExpression.Create;
  FExpressions.Add(NewExpression);
  Result := FExpressions.Count - 1;

  Expressions[Result].FValue := aString;
  Expressions[Result].FPageNumber := aPageNumber;
  Expressions[Result].FAssociatedElementIndex := -1;
  Expressions[Result].FAssociatedElementKeywordIndex := -1;
  Expressions[Result].FAssociatedExpressionKeywordIndex := -1;
  Expressions[Result].FLeftPxPos := -1;
  Expressions[Result].FTopPxPos := -1;
  Expressions[Result].FRightPxPos := -1;
  Expressions[Result].FBottomPxPos := -1;
  Expressions[Result].FOCRConfidence := aOCRConfidence;

  // Recognize :
  RecognizeExpressionType(Result);

  // ExpressionAdded() No need to call here because we don' t have pxPos ...
end;

procedure TcyDocER.DeleteExpression(aExpressionIndex: Integer);
begin
  if aExpressionIndex < 0 then
    raise Exception.Create('Cannot remove expression index below 0!');

  if aExpressionIndex > FExpressions.Count - 1 then
    raise Exception.Create('Cannot remove expression index above max.!');


  // Exit;   RHR 2013/03/01 Occurs error

  // Free stored object :
  Expressions[aExpressionIndex].Free;

  // Delete from list :
  FExpressions.Delete(aExpressionIndex);
end;

(* Old code
procedure TcyDocER.DeleteExpression(aExpressionIndex: Integer);
var i, TailCount: Integer;
begin
  if aExpressionIndex < 0 then
    raise Exception.Create('Cannot remove expression index below 0!');

  if aExpressionIndex > ExpressionCount-1 then
    raise Exception.Create('Cannot remove expression index above max.!');

  // Object will be implicitly deleted by Setlength()  Expressions[aExpressionIndex].Free;  // Free object ...

  if aExpressionIndex = ExpressionCount-1 then
  begin
    SetLength(FExpressions, ExpressionCount-1) ;
    Exit;
  end;

{  // Old code :
  for i := aExpressionIndex+1 to ExpressionCount-1 do
    Expressions[i-1] := Expressions[i];
    cc
  Expressions[ExpressionCount - 1] := TOCRExpression.Create;  // Because it was copied to prior position and will avoid to free it twice ...
  SetLength(FExpressions, ExpressionCount - 1);
}

  // ADRESS ERROR BEGIN
       //  Finalize(FExpressions, aExpressionIndex);
  TailCount := ExpressionCount - aExpressionIndex;
  if TailCount > 0 then
    Move(Expressions[aExpressionIndex + 1], Expressions[aExpressionIndex], SizeOf(TOCRExpression) * TailCount);
      //   Initialize(Expressions[ExpressionCount - 1]);  // Initialize object that will be implicitly deleted by SetLength() ...

  //Expressions[ExpressionCount - 1] := Nil;  // Because it was copied to prior position and will avoid to free it twice ...
  SetLength(FExpressions, ExpressionCount - 1);
  // ADRESS ERROR END
end;                  *)

// Retrieve Tesseract line information into variables :
procedure TcyDocER.ExtractTesseractBoxlineInfo(aLine: String; var RsltCar: String; var RsltPage, RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);
begin
  try
    RsltCar    := (aLine + ' ')[1];
    RsltPage   := StrToInt( SubString_Get(aLine, ' ', 6) ) + 1;
    RsltLeft   := StrToInt( SubString_Get(aLine, ' ', 2) );
    RsltBottom := FPixelsHeight - StrToInt( SubString_Get(aLine, ' ', 3) );
    RsltRight  := StrToInt( SubString_Get(aLine, ' ', 4) );
    RsltTop    := FPixelsHeight - StrToInt( SubString_Get(aLine, ' ', 5) );
  except

  end;
end;

function TcyDocER.FindExpression(StartIndex, AssociatedElementIndex, AssociatedElementKeywordIndex, AssociatedExpressionKeywordIndex: Integer;
  IncludeElementsTypes, ExcludeElementsTypes: TElementsTypes): Integer;
var i: Integer;
begin
  Result := -1;
  for i := StartIndex to ExpressionCount-1 do
    if (AssociatedElementIndex = -1) or (Expressions[i].AssociatedElementIndex = AssociatedElementIndex) then
      if (AssociatedElementKeywordIndex = -1) or (Expressions[i].AssociatedElementKeywordIndex = AssociatedElementKeywordIndex) then
        if (AssociatedExpressionKeywordIndex = -1) or (Expressions[i].AssociatedExpressionKeywordIndex = AssociatedExpressionKeywordIndex) then
          if (IncludeElementsTypes = []) or (Expressions[i].FRecognizedType in IncludeElementsTypes) then
            if not (Expressions[i].FRecognizedType in ExcludeElementsTypes) then
            begin
              Result := i;
              Break;
            end;
end;

procedure TcyDocER.LoadFromTesseractFile(aFilename: String; aPageNumber: Word{$IFDEF UNICODE}; aEncoding: TEncoding{$ENDIF});
var
  FileLines: TStrings;
begin
  FResolution := 0;
  FPageCount := 0;
  FileLines := TStringList.Create;
  FileLines.LoadFromFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  LoadFromTesseractString(FileLines.Text, aPageNumber);
  FileLines.Free;
end;

procedure TcyDocER.LoadFromFile(aFilename: String{$IFDEF UNICODE}; aEncoding: TEncoding{$ENDIF});
var
  FileLines: TStrings;
begin
  FileLines := TStringList.Create;
  FileLines.LoadFromFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  LoadFromStringList(FileLines);
  FileLines.Free;
end;

procedure TcyDocER.LoadFromStringList(aStrings: TStrings);
var
  l, LengthStrLine: Integer;
  StrLine: String;
  ExpressionString: String;
  ExpressionPageNumber: Integer;
  ExpressionOCRConfidence: Extended;
  ExpressionLeft, ExpressionTop, ExpressionRight, ExpressionBottom: Integer;

const
  SeparatorCar = ';';

        function CutLineValue: String;
        begin
          Result := '';
          while LengthStrLine > 0 do
            if StrLine[LengthStrLine] = SeparatorCar then
            begin
              Delete(StrLine, LengthStrLine, 1);
              Dec(LengthStrLine);
              Break;
            end
            else begin
              Result := StrLine[LengthStrLine] + Result;
              Delete(StrLine, LengthStrLine, 1);
              Dec(LengthStrLine);
            end;
        end;

begin
  // Header :
  StrLine := aStrings[0];
  LengthStrLine := Length(StrLine);
  try FPixelsHeight := StrToInt(CutLineValue); except FPixelsHeight := 3508; end;
  try FPixelsWidth := StrToInt(CutLineValue); except FPixelsWidth := 2448; end;
  try FResolution := StrToInt(CutLineValue); except FResolution := 300; end;

  for l := 1 to aStrings.Count-1 do
  begin
    StrLine := aStrings[l];
    LengthStrLine := Length(StrLine);

    try ExpressionBottom := StrToInt(CutLineValue); except end;
    try ExpressionRight := StrToInt(CutLineValue); except end;
    try ExpressionTop := StrToInt(CutLineValue); except end;
    try ExpressionLeft := StrToInt(CutLineValue); except end;
    try ExpressionOCRConfidence := StrToFloat(CutLineValue); except end;
    try ExpressionPageNumber := StrToInt(CutLineValue); except end;
    ExpressionString := StrLine;  // No matter if Strline contains SeparatorCar !

    AddExpression(ExpressionString, ExpressionPageNumber, ExpressionOCRConfidence, classes.Rect(ExpressionLeft, ExpressionTop, ExpressionRight, ExpressionBottom));

    // Retrieve short/long date :
    ExpressionLoaded;
  end;
end;

procedure TcyDocER.SaveToFile(aFilename: String{$IFDEF UNICODE}; aEncoding: TEncoding{$ENDIF});
var
  FileLines: TStrings;
begin
  FileLines := TStringList.Create;
  SaveToStringList(FileLines);
  FileLines.SaveToFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  FileLines.Free;
end;

procedure TcyDocER.SaveToStringList(aStrings: TStrings);
var
  i: Integer;
  Str, StrLine: String;

const
  SeparatorCar = ';';

begin
  aStrings.Clear;
  // Header :
  aStrings.Add(intToStr(FResolution) + SeparatorCar + intToStr(FPixelsWidth) + SeparatorCar + intToStr(FPixelsHeight));

  for i := 0 to ExpressionCount-1 do
  begin
    Str := Expressions[i].Value;

    StrLine := Str + SeparatorCar + intToStr(Expressions[i].FPageNumber) + SeparatorCar + FloatToStr(Expressions[i].FOCRConfidence) + SeparatorCar
                + intToStr(Expressions[i].FLeftPxPos) + SeparatorCar + intToStr(Expressions[i].FTopPxPos) + SeparatorCar
                 + intToStr(Expressions[i].FRightPxPos) + SeparatorCar + intToStr(Expressions[i].FBottomPxPos);
    aStrings.Add(StrLine);
  end;
end;

procedure TcyDocER.LoadFromTesseractBoxFile(aFilename: String; {$IFDEF UNICODE}aEncoding: TEncoding; {$ENDIF}ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
var
  FileLines: TStrings;
begin
  FileLines := TStringList.Create;
  FileLines.LoadFromFile(aFilename{$IFDEF UNICODE}, aEncoding{$ENDIF});
  LoadFromTesseractBoxStringList(FileLines, ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight);
  FileLines.Free;
end;

procedure TcyDocER.LoadFromTesseractString(aString: String; aPageNumber: Integer);
var
  i, lengthString: Integer;
  StrExpression: String;
begin
  if aPageNumber > FPageCount then
    FPageCount := aPageNumber;

  StrExpression := '';
  aString := aString + ' ';
  lengthString := length(aString);

  repeat
    aString := String_Subst(' %', '%', aString, csCaseSensitive, true);
  until pos(' %', aString) = 0;

  repeat
    aString := String_Subst(' €', '€', aString, csCaseSensitive, true);
  until pos(' €', aString) = 0;

  // Add all words from file :
  for i := 1 to lengthString do
    if (aString[i] in [#$D, #$A, ' ']) or (i = lengthString) then
    begin
      AddExpression(StrExpression, aPageNumber, 0);
      StrExpression := '';
    end
    else
      StrExpression := StrExpression + aString[i];
end;

// Convert Tesseract box file into Expressions :
procedure TcyDocER.LoadFromTesseractBoxStringList(aStrings: TStrings; ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
var
  l, LineCount: Integer;
  CarValue: String;
  CarPage, CarLeft, CarTop, CarRight, CarBottom: Integer;
  LineCars: Array of TOCRCarInfo;
  LineCarCount: Integer;
  MinLineTop, MaxLineBottom: Integer;

        // **************************************************************************************************************************

        procedure CutLineIntoExpressions;

                  // Handle small heights ("-" car) and small width ("i" car)
                  // If Height * 0.75 > Width ("i" char for exemple), we try to calculate correct font size
                  function CalcFontSize(CarIndex: Integer): Integer;
                  var MinSize, _width, _Height: Integer;
                  begin
                    MinSize := Round( (FResolution/100) * 6 );    // Min font size recognition = 6
                    _Width  := LineCars[CarIndex].Right - LineCars[CarIndex].Left + 1;
                    _Height := LineCars[CarIndex].Bottom - LineCars[CarIndex].Top + 1;

                    if _Width < MinSize then
                      _Width := MinSize;

                    if _Height < MinSize then
                      _Height := MinSize;

                    if _Height * 0.75 > _Width then
                    begin
                      if _Height / _width <= 3
                      then
                        Result := Round((_Height * 0.9) * 0.8 + _Width * 0.2)  // H20/W7 = 20*0.9*0.8+7*0.2 = 15,8
                      else
                        if _Height / _Width <= 4.5
                        then Result := Round((_Height * 0.75) * 0.8 + _Width * 0.2)      // H25/W6 = 25*0.75*0.8+6*0.2 = 16,2
                        else Result := Round(_Height * 0.8);                             // H20/W4 = 20*0.8 = 16
                    end
                    else
                      if _Height > _Width
                      then Result := _Height
                      else Result := _Width;

                    if LineCars[CarIndex].Value = '@' then
                      Result := Round(Result * 0.6);
                  end;

                  // Space char is aproximatively 1/3 to 1/2 of font size
                  // Letters Spacing between cars is 15% font' s size
                  // Handle Courier new space char + anormal space between chars
                  // Handle when some word's caracters are embbeded into another ...
                  function CarBelongsToWord(CarIndex: Integer; LettersSpacing, NormalFontSize: Integer): Boolean;
                  var
                    MaxSpaceCharWidth: Integer;
                  begin
                    if NormalFontSize = 0 then
                    begin
                      Result := true;
                      Exit;
                    end;

                    Result := false;

                    // letters embedded into another (Logo or italic font):
                    if LettersSpacing < 0 then
                      LettersSpacing := 0;

                    if CalcFontSize(CarIndex) > NormalFontSize then
                      NormalFontSize := CalcFontSize(CarIndex);

                    // Handling spacing between "1" followed by another "1" (the normal char spacing is raised by 1/4 font px height):
                    if (CarIndex > 0) and (LineCars[CarIndex].Value[1] in ['l', '1']) then
                      if LineCars[CarIndex-1].Value[1] in ['l', '1'] then
                        Inc(LettersSpacing, (LineCars[CarIndex].Bottom - LineCars[CarIndex].Top) div 4);

                    // Handling anormal spacing between number and decimal separator for float/money values (exemple: " 3 8  ,  0 0  € ") :
                    if (CarIndex > 0) and (LineCars[CarIndex].Value[1] in [',', '.']) then
                      if LineCars[CarIndex-1].Value[1] in ['0'..'9'] then
                        Inc(LettersSpacing, NormalFontSize div 2);    // Ok ...

                    // 0.25 = (normal char space width (33% font size) + Normal Spacing between cars (15% font size)) div 2 :
                    MaxSpaceCharWidth := Round(NormalFontSize * 0.261);  // 0.26 is not enougth for numbers recognition ("01/11/2011")

                    if LineCars[CarIndex].Left - LineCars[CarIndex-1].Right < LettersSpacing + MaxSpaceCharWidth then
                      Result := true;
                  end;

        var
          i, j, CharFontSize, LettersSpacing, CurrentWordNumber: Integer;
          aWord: String;
          aRect: TRect;
        begin
          if LineCarCount = 0 then Exit;

          if LineCarCount = 1 then
          begin
            AddExpression(LineCars[0].Value, LineCars[0].PageNumber, 0, Rect(LineCars[0].Left, LineCars[0].Top, LineCars[0].Right, LineCars[0].Bottom));
            Exit;
          end;

          // At least, we have 2 chars :
          CharFontSize := 0;
          CurrentWordNumber := 1;
          LettersSpacing := LineCars[LineCarCount-1].Right - LineCars[0].Left;

          // Detect words:
          for i := 0 to LineCarCount-1 do
          begin
            LineCars[i].WordNumber := CurrentWordNumber;

            j := i;
            // Will allow detect single char word followed by spacing! exemple: "tel : 000"  ...
            while CarBelongsToWord(j, LettersSpacing, CharFontSize) do
            begin
              // Smallest char spacing between chars from same word:
              if CharFontSize <> 0 then   // not first word char
                if LineCars[j].Left - LineCars[j-1].Right < LettersSpacing then
                  LettersSpacing := LineCars[j].Left - LineCars[j-1].Right;

              // Bigger char size :
              if CalcFontSize(j) > CharFontSize then
                CharFontSize := CalcFontSize(j);

              inc(j);
              if j > LineCarCount-1 then
                Break;
            end;

            // Detect if next char is in a new word:
            if i <> LineCarCount-1 then
            begin
              if not CarBelongsToWord(i+1, LettersSpacing, CharFontSize) then
              begin
                Inc(CurrentWordNumber);
                CharFontSize := 0;
                LettersSpacing := LineCars[LineCarCount-1].Right - LineCars[i+1].Left;
              end;
            end;
          end;

          // Create Expressions :
          aWord := '';
          aRect := Rect(-1, -1, -1, -1);
          CurrentWordNumber := 1;
          for i := 0 to LineCarCount-1 do
          begin
            if LineCars[i].WordNumber <> CurrentWordNumber then
            begin
              AddExpression(aWord, LineCars[i-1].PageNumber, 0, aRect);
              CurrentWordNumber := LineCars[i].WordNumber;
              aWord := '';
              aRect := Rect(-1, -1, -1, -1);
            end;

            // Build Word cars + Rect:
            aWord := aWord + LineCars[i].Value;
            if (LineCars[i].Left < aRect.Left) or (aRect.Left = -1) then aRect.Left := LineCars[i].Left;
            if (LineCars[i].Top < aRect.Top) or (aRect.Top = -1) then aRect.Top := LineCars[i].Top;
            if (LineCars[i].Right > aRect.Right) or (aRect.Right = -1) then aRect.Right := LineCars[i].Right;
            if (LineCars[i].Bottom > aRect.Bottom) or (aRect.Bottom = -1) then aRect.Bottom := LineCars[i].Bottom;
          end;

          // Add last word :
          AddExpression(aWord, LineCars[LineCarCount-1].PageNumber, 0, aRect);
        end;


        function CarInSameLine: Boolean;
        var y: Integer;
        begin
          Result := True;
          if LineCarCount = 0 then Exit;  // No cars

          Result := false;
          if LineCars[0].PageNumber <> CarPage then Exit;

          // See if char is just after last inserted in LineCars []:
          // Handle bricked Logo letters (like Tetris pieces)
          // '.' char followed by italic letter like 'f'can have same left position
          if CarRight > LineCars[LineCarCount-1].Right then
          // Not work if chars begins at same left position ... if CarLeft >= LineCars[LineCarCount-1].Right - (LineCars[LineCarCount-1].Right-LineCars[LineCarCount-1].Left) div 2 then
          begin
            // Check horizontal position :
            y := CarTop + (CarBottom - CarTop) div 2;
            if (y >= MinLineTop) and (y <= MaxLineBottom) then
              Result := true;
          end;
        end;

begin
  FPageCount := ImagePageCount;
  FResolution := ImageResolution;
  FPixelsWidth := ImagePixelsWidth;
  FPixelsHeight := ImagePixelsHeight;

  l := 0;
  LineCount := aStrings.Count;

  LineCarCount := 0;
  SetLength(LineCars, 0);
  MinLineTop := -1;
  MaxLineBottom := -1;

  while l <= LineCount-1 do    // All caracters ...
  begin
    ExtractTesseractBoxlineInfo(aStrings[l], CarValue, CarPage, CarLeft, CarTop, CarRight, CarBottom);

    if not CarInSameLine then
    begin
      // Cut into Expressions :
      CutLineIntoExpressions;

      LineCarCount := 0;
      SetLength(LineCars, 0);
      MinLineTop := -1;
      MaxLineBottom := -1;
    end;

    Inc(LineCarCount);
    SetLength(LineCars, LineCarCount);
    LineCars[LineCarCount-1].Value := CarValue;
    LineCars[LineCarCount-1].PageNumber := CarPage;
    LineCars[LineCarCount-1].Left := CarLeft;
    LineCars[LineCarCount-1].Top := CarTop;
    LineCars[LineCarCount-1].Right := CarRight;
    LineCars[LineCarCount-1].Bottom := CarBottom;
    LineCars[LineCarCount-1].WordNumber := 0;     // Not defined yet

    if (CarTop < MinLineTop) or (MinLineTop = -1) then
      MinLineTop := CarTop;

    if (CarBottom > MaxLineBottom) or (MaxLineBottom = -1) then
      MaxLineBottom := CarBottom;

    inc(l);
  end;

  // Cut last line into Expressions :
  CutLineIntoExpressions;
end;

function TcyDocER.FindExpression(Value: Variant; ValueType: TElementsType; FromIndex, ToIndex: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := FromIndex to ToIndex do
  begin
    case ValueType of
      etText, etExpressionKeyWord:
      begin
        if AnsiUpperCase(Expressions[i].FDERValue) = AnsiUpperCase(StringToDERCharSet(Value)) then Result := i;
      end;

      etNumbers:
      begin
        if Expressions[i].FRecognizedNumbers = Value then Result := i;
      end;

      etInteger:
      begin
        if Expressions[i].FRecognizedInteger <> '' then
          if Expressions[i].FRecognizedInteger = Value then Result := i;
      end;

      etFloat:
      begin
        if Expressions[i].FRecognizedFloat <> '' then
          if Expressions[i].FRecognizedFloat = Value then Result := i;
      end;

      etPercentage:
      begin
        if Expressions[i].FRecognizedFloat = Value then Result := i;
      end;

      etwebSite, etWebMail:
      begin
        if Expressions[i].FDERValue = Value then Result := i;
      end;

      etMoney:
      begin
        if Expressions[i].FRecognizedMoney <> '' then
          if Expressions[i].FRecognizedFloat = Value then Result := i;
      end;

      etDate:
      begin
        if Expressions[i].FRecognizedDate <> '' then
          if StrToInt(Expressions[i].FRecognizedDate) = Value then Result := i;
      end;
    end;

    if Result <> -1 then
      Break;
  end;
end;

function TcyDocER.LocateExpression(Value: String; FromIndex, ToIndex, MaxCarErrors: Integer; Options: TLocateExpressionOptions; var RsltCarPos: Integer): Integer;
var
  i, LengthValue, LengthExpressionValue: Integer;
  ExpressionValue, UnmodifiedCarCaseValue: DERString;

      function RemovePointChar(aDERString: DERString): DERString;
      begin
        Result := StringReplace(aDERString, '.', '', [rfReplaceAll]);
      end;

      function GetValuePos: Integer;
      var CarErrors, c, s: Integer;
      begin
        Result := 0;

        if leSmartKeywordRec in Options then
        begin
          if ExpressionValue = '' then
          begin
            if LengthValue <= MaxCarErrors then
            begin
              Result := 1;
              RsltCarPos := Result;
            end;

            Exit;
          end;

          for c := 1 to (LengthExpressionValue - LengthValue)+1 do             // Search at possible positions
          begin
            CarErrors := 0;

            for s := 1 to LengthValue do
              if Value[s] <> ExpressionValue[c+s-1] then
              begin
                if not (UnmodifiedCarCaseValue[s] in ['a'..'z']) then  // Uppercase and symbols cars in Value must be the same as in expression
                  CarErrors := MaxCarErrors; // Let search at next c position ...

                Inc(CarErrors);
              end;

            if CarErrors <= MaxCarErrors then
            begin
              Result := c;
              RsltCarPos := Result;
              Exit;
            end;
          end;
        end
        else begin
          Result := pos(Value, ExpressionValue);
          RsltCarPos := Result;
        end;
      end;

begin
  Result := -1;

  Value  := StringToDERCharSet(Value);
  Value  := RemovePointChar(Value);
  UnmodifiedCarCaseValue := Value;
  LengthValue := Length(Value);

  if leInsensitive in Options then
    Value  := AnsiUpperCase(Value);

  for i := FromIndex to ToIndex do
  begin
    ExpressionValue := RemovePointChar(Expressions[i].DERValue);
    LengthExpressionValue := Length(ExpressionValue);

    if leInsensitive in Options then
      ExpressionValue := AnsiUpperCase(ExpressionValue);

    if lePartialKey in Options then
    begin
      // Must contain "value" at position 1 :
      if GetValuePos = 1 then
        Result := i;
    end
    else
      if leRelativePositionKey in Options then
      begin
        // In any position :
        if GetValuePos <> 0 then
          Result := i;
      end
      else begin
        // Must be equal :
        if LengthValue = LengthExpressionValue then
          if GetValuePos = 1 then
            Result := i;
      end;

    if Result <> -1 then
      Exit;
  end;
end;

function TcyDocER.ExpressionsInSameLine(aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
var y1: Integer;
begin
  // Calculate horizontal position :
  y1 := Expressions[aExpressionIndex1].FTopPxPos + (Expressions[aExpressionIndex1].FBottomPxPos - Expressions[aExpressionIndex1].FTopPxPos) div 2;
  Result := (y1 >= Expressions[aExpressionIndex2].FTopPxPos) and (y1 <= Expressions[aExpressionIndex2].FBottomPxPos);
end;

procedure TcyDocER.GetAroundExpressions(aExpressionIndex, MaxCarErrors, ScopePx: Integer; SearchValue: String; SearchOptions: TLocateExpressionOptions; var RsltLeft, RsltTop, RsltRight, RsltBottom: Integer);

        function SearchValueMatch(aIndex: Integer): Boolean;
        var RsltCarPos: Integer;
        begin
          Result := true;
          if SearchValue = '' then Exit;

          Result := LocateExpression(SearchValue, aIndex, aIndex, MaxCarErrors, SearchOptions, RsltCarPos) = aIndex;
        end;

var
  i, x, LetterWidth, PosRsltLeft, PosRsltTop, PosRsltRight, PosRsltBottom: Integer;
begin
  RsltLeft := -1;    // Must be in same line
  RsltTop := -1;     // Must be in same column
  RsltRight := -1;   // Must be in same line
  RsltBottom := -1;  // Must be in same column

  PosRsltLeft := -1;
  PosRsltTop := -1;
  PosRsltRight := -1;
  PosRsltBottom := -1;

  if length(Expressions[aExpressionIndex].FValue) <> 0
  then LetterWidth := (Expressions[aExpressionIndex].FRightPxPos - Expressions[aExpressionIndex].FLeftPxPos) div length(Expressions[aExpressionIndex].FValue)
  else LetterWidth := 0;

  for i := 0 to ExpressionCount-1 do
  begin
    if (i = aExpressionIndex) or (Expressions[i].FPageNumber <> Expressions[aExpressionIndex].FPageNumber) then
      Continue;

    // Expression in same line ?
    if ExpressionsInSameLine(i, aExpressionIndex) then
    begin
      // Left :
      if Expressions[i].FLeftPxPos < Expressions[aExpressionIndex].FLeftPxPos then
        if Expressions[i].FRightPxPos + ScopePx >= Expressions[aExpressionIndex].FLeftPxPos then
          if (Expressions[i].FLeftPxPos > PosRsltLeft) or (PosRsltLeft = -1) then    // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltLeft := i
            else RsltLeft := -1;   // 24/04/2012

            PosRsltLeft := Expressions[i].FLeftPxPos;
          end;

      // Right :
      if Expressions[i].FLeftPxPos > Expressions[aExpressionIndex].FLeftPxPos then  // Compare left again
        if Expressions[i].FLeftPxPos - ScopePx <=  Expressions[aExpressionIndex].FRightPxPos then
          if (Expressions[i].FLeftPxPos < PosRsltRight) or (PosRsltRight = -1) then   // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltRight := i
            else RsltRight := -1;  // 24/04/2012

            PosRsltRight := Expressions[i].FLeftPxPos;
          end;
    end;

    // Expression in same "column" ?
    x := Expressions[i].FLeftPxPos + (Expressions[i].FRightPxPos - Expressions[i].FLeftPxPos) div 2;
    if (x >= Expressions[aExpressionIndex].FLeftPxPos) and (x <= Expressions[aExpressionIndex].FRightPxPos)
     or (abs(Expressions[i].FLeftPxPos - Expressions[aExpressionIndex].FLeftPxPos) < LetterWidth)
      or (abs(Expressions[i].FRightPxPos - Expressions[aExpressionIndex].FRightPxPos) < LetterWidth) then  // Solve problem if much more letters and text align = left or right
    begin
      // Top :
      if Expressions[i].FTopPxPos < Expressions[aExpressionIndex].FTopPxPos then
        if Expressions[i].FBottomPxPos + ScopePx >= Expressions[aExpressionIndex].FTopPxPos then
          if (Expressions[i].FTopPxPos > PosRsltTop) or (PosRsltTop = -1) then     // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltTop := i
            else RsltTop := -1;  // 24/04/2012

            PosRsltTop := Expressions[i].FTopPxPos;
          end;

      // Bottom :
      if Expressions[i].FTopPxPos > Expressions[aExpressionIndex].FTopPxPos then  // Compare top again
        if Expressions[i].FTopPxPos - ScopePx <=  Expressions[aExpressionIndex].FBottomPxPos then
          if (Expressions[i].FTopPxPos < PosRsltBottom) or (PosRsltBottom = -1) then  // 24/04/2012
          begin
            if SearchValueMatch(i)
            then RsltBottom := i
            else RsltBottom := -1;  // 24/04/2012

            PosRsltBottom := Expressions[i].FTopPxPos;
          end;
    end;
  end;
end;

procedure TcyDocER.MergeExpressions(aExpressionIndex, ToExpressionIndex: Integer);
var i: Integer;
begin
  if aExpressionIndex = ToExpressionIndex then Exit;

  Expressions[ToExpressionIndex].Value := Expressions[ToExpressionIndex].Value + ' ' + Expressions[aExpressionIndex].Value;

  if Expressions[aExpressionIndex].FLeftPxPos < Expressions[ToExpressionIndex].FLeftPxPos then
    Expressions[ToExpressionIndex].FLeftPxPos := Expressions[aExpressionIndex].FLeftPxPos;

  if Expressions[aExpressionIndex].FTopPxPos < Expressions[ToExpressionIndex].FTopPxPos then
    Expressions[ToExpressionIndex].FTopPxPos := Expressions[aExpressionIndex].FTopPxPos;

  if Expressions[aExpressionIndex].FRightPxPos > Expressions[ToExpressionIndex].FRightPxPos then
    Expressions[ToExpressionIndex].FRightPxPos := Expressions[aExpressionIndex].FRightPxPos;

  if Expressions[aExpressionIndex].FBottomPxPos > Expressions[ToExpressionIndex].FBottomPxPos then
    Expressions[ToExpressionIndex].FBottomPxPos := Expressions[aExpressionIndex].FBottomPxPos;

  RecognizeExpressionType(ToExpressionIndex);

  // Delete aExpressionIndex from list :
  DeleteExpression(aExpressionIndex);

  // Resynch :
  if ToExpressionIndex > aExpressionIndex then
    Dec(ToExpressionIndex);

  for i := 0 to ExpressionCount-1 do
  begin
    if Expressions[i].FAssociatedExpressionKeywordIndex = aExpressionIndex then
      Expressions[i].FAssociatedExpressionKeywordIndex := ToExpressionIndex
    else
      if Expressions[i].FAssociatedExpressionKeywordIndex > aExpressionIndex then
        Expressions[i].FAssociatedExpressionKeywordIndex := Expressions[i].FAssociatedExpressionKeywordIndex - 1;
  end;

  // User Resynch :
  if Assigned(FOnExpressionMerged) then
    FOnExpressionMerged(Self, aExpressionIndex, toExpressionIndex);
end;

function TcyDocER.ExpressionsSideBySide(ExpressionIndexAtLeft, ExpressionIndexAtRight, MaxPxSpacing: Integer): Boolean;
begin
  Result := false;
  if Expressions[ExpressionIndexAtLeft].FPageNumber <> Expressions[ExpressionIndexAtRight].FPageNumber then Exit;
  if Expressions[ExpressionIndexAtLeft].FLeftPxPos >= Expressions[ExpressionIndexAtRight].FLeftPxPos then Exit;

  // Check horizontal spacing between expressions:
  if Expressions[ExpressionIndexAtRight].FLeftPxPos - Expressions[ExpressionIndexAtLeft].FRightPxPos > MaxPxSpacing then Exit;

  // Check horizontal position (must intersect):
  Result := true;
  if Expressions[ExpressionIndexAtLeft].FTopPxPos < Expressions[ExpressionIndexAtRight].FTopPxPos then
    Result := Expressions[ExpressionIndexAtLeft].FBottomPxPos >= Expressions[ExpressionIndexAtRight].FTopPxPos
  else
    Result := Expressions[ExpressionIndexAtLeft].FTopPxPos <= Expressions[ExpressionIndexAtRight].FBottomPxPos;
end;

function TcyDocER.MmToPx(MmValue: Double): Integer;
var Inches: Double;
begin
  Inches := MmValue / 25.4;
  Result := Round(Inches * FResolution);
end;

procedure TcyDocER.NewPage(ImagePageCount, ImageResolution, ImagePixelsWidth, ImagePixelsHeight: Integer);
begin
  FPageCount := ImagePageCount;
  FResolution := ImageResolution;
  FPixelsWidth := ImagePixelsWidth;
  FPixelsHeight := ImagePixelsHeight;
end;

function TcyDocER.PxToMm(pxValue: Extended): Double;
var Inches: Double;
begin
  if FResolution = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Inches := pxValue / FResolution;
  Result := Inches * 25.4;
end;

// Returns located expression Index of the specicied keyword :
function TcyDocER.RecognizeElementKeyword(ElementIndex, KeywordIndex: Integer; InlineKeyword: Boolean): Boolean;
var
  ExpressionList: TStrings;
  DERWordStack: TStrings;
  RemainedExpression: DERString;
  aKeyword, CurrentWord: String;
  i, m, LocateCarPos, KeywordLength, ExpressionIndex, SearchWordIndex, AroundExpressionIndex, ScopePx, MergingExpressionIndex: Integer;
  LeftExpression, TopExpression, RightExpression, BottomExpression: Integer;
  SearchWords: Boolean;
  isExpressionAssigned: Boolean;
  MaxCarErrors, IncCarErrors: Integer;
  _AssociatedElementIndex, _AssociatedElementKeywordIndex: Integer;
  Locate1stWordOptions, LocateAroundOptions: TLocateExpressionOptions;

      // Cannot split in the middle of a word (Because the user's keyword could be just some first letters of a word) !
      procedure TrySplitOnRemainedExpression(aExpressionIndex: Integer);
      var
        LengthValue, LengthRemainedExpression, k, n: Integer;
        RemainedStr, NewValue: String;
      begin
        LengthValue := length(Expressions[aExpressionIndex].FValue);
        LengthRemainedExpression := Length(RemainedExpression);

        // for k := LengthValue downto 1 do  // 30/04/2012
        for k := LengthValue downto 2 do
        begin
          RemainedStr := Copy(Expressions[aExpressionIndex].FValue, k, LengthValue); // Copy from k
          // 05/05/2012 - Remove '.' char :
          RemainedStr := StringReplace(RemainedStr, '.', '', [rfReplaceAll]);

          if length(StringToDERCharSet(RemainedStr)) = LengthRemainedExpression then
          begin
            // 30/04/2012  if not (RemainedStr[1] in ['.', 'a'..'z', 'A'..'Z', '0'..'9']) then
            if not (Expressions[aExpressionIndex].FValue[k-1] in ['.', 'a'..'z', 'A'..'Z', '0'..'9']) then  // Don' t split last word ("tot", "tel.", 'H.T.')
            begin
              NewValue := RemainedStr;

              // Modify actual expression:
              Expressions[aExpressionIndex].FValue := Copy(Expressions[aExpressionIndex].FValue, 1, k - 1);
              RecognizeExpressionType(aExpressionIndex);

              n := AddExpression(NewValue, Expressions[aExpressionIndex].FPageNumber, Expressions[aExpressionIndex].FOCRConfidence);

              if n <> -1 then
                with Expressions[n] do
                begin
                  FLeftPxPos := Expressions[aExpressionIndex].FLeftPxPos;
                  FTopPxPos := Expressions[aExpressionIndex].FTopPxPos;
                  FRightPxPos := Expressions[aExpressionIndex].FRightPxPos;
                  FBottomPxPos := Expressions[aExpressionIndex].FBottomPxPos;
                  // ExpressionAdded; Can' t call here because it is not inserted in reading words order ...
                end;
            end;

            Break;
          end;
        end;
      end;

begin
  Result := false;
  aKeyword := Elements[ElementIndex].KeyWords[KeywordIndex];
  if aKeyword = '' then Exit;

  if Assigned(FBeforeRecognizeElementKeyword) then
    FBeforeRecognizeElementKeyword(Self, ElementIndex, KeywordIndex);

  ExpressionList := TStringList.Create;
  if aKeyword[1] = ' '
  then Locate1stWordOptions := [lePartialKey, leInsensitive]  // Expression is "isolated" from a sentence or expression is the beginning of a sentence.
  else Locate1stWordOptions := [leRelativePositionKey, leInsensitive];
  LocateAroundOptions := [lePartialKey, leInsensitive];

  if roSmartKeywordRec in FRecognitionOptions then
  begin
    //Include(Locate1stWordOptions, leSmartKeywordRec);
    Locate1stWordOptions := Locate1stWordOptions + [leSmartKeywordRec];
    LocateAroundOptions := LocateAroundOptions + [leSmartKeywordRec];
  end;

  KeywordLength := Length(aKeyword);
  aKeyword := aKeyword + ' ';

  // *** Initialize DERWordStack with Keyword's word *** //
  IncCarErrors := 0;
  DERWordStack := TStringlist.Create;
  CurrentWord := '';
  for i := 1 to length(aKeyword) do
    if aKeyWord[i] <> ' ' then
    begin
      if aKeyWord[i] <> '.' then         // 05/05/2012  - points are ignored
        CurrentWord := CurrentWord + aKeyWord[i];
    end
    else
      if CurrentWord <> '' then
      begin
        CurrentWord := StringToDERCharSet(CurrentWord);
        if CurrentWord <> '' then
          DERWordStack.Add(CurrentWord);
        CurrentWord := '';
      end;

  // *** Search for DERWordStack items in List *** //
  if DERWordStack.Count > 0 then
  begin
    if DERWordStack.Count > 1 then
      IncCarErrors := 1;

    ExpressionIndex := 0;
    while (ExpressionIndex <> -1) and (not Result) do
    begin
      // Locate first aKeyword's word from "ExpressionIndex":
      MaxCarErrors := Length(DERWordStack[0]) div 6 + IncCarErrors;
      ExpressionIndex := LocateExpression(DERWordStack[0], ExpressionIndex, ExpressionCount-1, MaxCarErrors, Locate1stWordOptions, LocateCarPos);

      if ExpressionIndex <> -1 then
      begin
        // Handle when any element's Keyword text is part of another element's keyword :
        isExpressionAssigned := false;
        _AssociatedElementIndex := Expressions[ExpressionIndex].AssociatedElementIndex;
        _AssociatedElementKeywordIndex := Expressions[ExpressionIndex].AssociatedElementKeywordIndex;

        if (_AssociatedElementIndex = ElementIndex) and (_AssociatedElementKeywordIndex = KeywordIndex)
        then
          isExpressionAssigned := true   // Already assigned to this element and keyword ...
        else
          if Expressions[ExpressionIndex].RecognizedType <> etExpressionKeyWord then
            isExpressionAssigned := _AssociatedElementIndex <> -1
          else
            if (Expressions[ExpressionIndex].RecognizedType = etExpressionKeyWord) and (_AssociatedElementIndex <> -1) and (_AssociatedElementKeywordIndex <> -1) then
            begin
              if KeywordLength <= Length( Elements[_AssociatedElementIndex].KeyWords[_AssociatedElementKeywordIndex] ) then
                isExpressionAssigned := true;
            end;

        if not isExpressionAssigned then
        begin
          ExpressionList.Clear;
          ExpressionList.Add(intToStr(ExpressionIndex));
          RemainedExpression := AnsiUppercase(Expressions[ExpressionIndex].DERValue);
          // 05/05/2012 - Remove '.' char :
          RemainedExpression := StringReplace(RemainedExpression, '.', '', [rfReplaceAll]);
          // Remained Expression DERString :
          RemainedExpression := Copy(RemainedExpression, LocateCarPos + length(DERWordStack[0]), length(RemainedExpression));

          // *** Search for the others keyword words *** //
          SearchWords := True;
          SearchWordIndex := 1;    // Search for 2nd keyword's word
          AroundExpressionIndex := ExpressionIndex;

          while (SearchWords) and (SearchWordIndex <= DERWordStack.Count-1) do
          begin
            if RemainedExpression = '' then
            begin
              // Locate Expression Index with remained searched text:
              if InlineKeyword and (DERWordStack[SearchWordIndex] = ':')   // 21/04/2012 - Natural separator, can be far away from prior words ...
              then ScopePx := FPixelsWidth
              else ScopePx := (Expressions[AroundExpressionIndex].FBottomPxPos-Expressions[AroundExpressionIndex].FTopPxPos)* 2; // Twice of expression's height

              MaxCarErrors := Length(DERWordStack[SearchWordIndex]) div 6 + IncCarErrors;
              GetAroundExpressions(AroundExpressionIndex, MaxCarErrors, ScopePx, DERWordStack[SearchWordIndex], LocateAroundOptions, LeftExpression, TopExpression, RightExpression, BottomExpression);
              AroundExpressionIndex := -1;

              AroundExpressionIndex := RightExpression;

              if not InlineKeyword then
                if (BottomExpression <> -1) and (AroundExpressionIndex = -1) then
                  AroundExpressionIndex := BottomExpression;

              if AroundExpressionIndex <> -1 then
              begin
                ExpressionList.Add(intToStr(AroundExpressionIndex));
                RemainedExpression := AnsiUppercase(Expressions[AroundExpressionIndex].DERValue);
                // 05/05/2012 - Remove '.' char :
                RemainedExpression := StringReplace(RemainedExpression, '.', '', [rfReplaceAll]);
              end
              else begin
                SearchWords := false;
                Break;
              end;
            end
            else
              LocateCarPos := pos(AnsiUppercase(DERWordStack[SearchWordIndex]), RemainedExpression);      // RHR - roSmartKeywordRec not handled here !

            if LocateCarPos = 1 then
            begin
              RemainedExpression := Copy(RemainedExpression, 1 + length(DERWordStack[SearchWordIndex]), length(RemainedExpression));
              Inc(SearchWordIndex);
            end
            else
              SearchWords := false;
          end;

          if SearchWords then  // All keyword's word retrieved:
            Result := true;
        end;

        Inc(ExpressionIndex);
      end;
    end;
  end;

  if Result then
    if Assigned(FOnValidateElementKeyword) then
      FOnValidateElementKeyword(Self, ElementIndex, KeywordIndex, ExpressionList, Result);

  if Result then
  begin
    // In some cases, Expression value contains element's keyword + element's value (exemple: "fax:06328974")
    if RemainedExpression <> '' then
    begin
      // Split expression in order to have the key in actual expression and the RemainedExpression in a new expression :
      TrySplitOnRemainedExpression(StrToInt(ExpressionList[ExpressionList.Count-1]));
    end;

    for i := 1 to ExpressionList.Count-1 do
    begin
      MergingExpressionIndex := StrToInt(ExpressionList[i]);
      MergeExpressions(StrToInt(ExpressionList[i]), StrToInt(ExpressionList[0]));

      // All expressions with index greater that MergingExpressionIndex moved up on Expression list !
      for m := 0 to ExpressionList.Count-1 do
        if StrToInt(ExpressionList[m]) > MergingExpressionIndex then
          ExpressionList[m] := intToStr( StrToInt(ExpressionList[m]) - 1 );
    end;

    Expressions[StrToInt(ExpressionList[0])].FAssociatedElementIndex := ElementIndex;
    Expressions[StrToInt(ExpressionList[0])].FAssociatedElementKeywordIndex := KeywordIndex;
    Expressions[StrToInt(ExpressionList[0])].FRecognizedType := etExpressionKeyWord;
  end;

  DERWordStack.Free;
  ExpressionList.Free;

  if Assigned(FAfterRecognizeElementKeyword) then
    FAfterRecognizeElementKeyword(Self, ElementIndex, KeywordIndex);
end;

// Recognize element's expressions that intersect with Rect :
function TcyDocER.RecognizeElementValuesFromRect(OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex: Integer; aValueType: TElementsType;
 aPageNumber, LeftPos, TopPos, RightPos, BottomPos: Integer; ValueLocation: TSearchValueLocation;
 ValueNumber, ValueCount: Integer; PatternMode: Boolean; var UserAbort: Boolean): Integer;

       function ExpressionInArea(aExpressionIndex: Integer): Boolean;
       begin
         Result := false;

         // Check horizontal intersection :
         if Expressions[aExpressionIndex].FLeftPxPos <= LeftPos
         then Result := Expressions[aExpressionIndex].FRightPxPos >= LeftPos
         else Result := Expressions[aExpressionIndex].FLeftPxPos <= RightPos;

         // Check vertical intersection :
         if Result then
           if Expressions[aExpressionIndex].FTopPxPos <= TopPos
           then Result := Expressions[aExpressionIndex].FBottomPxPos >= TopPos
           else Result := Expressions[aExpressionIndex].FTopPxPos <= BottomPos;
       end;

       function GetExpressionValue(aExpression: TOCRExpression; aValueType: TElementsType): String;
       begin
         Result := '';

         // etMoney can be recognized as etFloat:
         if (aValueType = etMoney) and (aExpression.FRecognizedType <> aValueType) then
           aValueType := etFloat;

         // etPercentage can be recognized as etFloat:
         if (aValueType = etPercentage) and (aExpression.FRecognizedType <> aValueType) then
           aValueType := etFloat;

          // etNumbers can be recognized as several etFloat :
         if (aValueType = etNumbers) and (aExpression.FRecognizedType <> aValueType) then
           aValueType := etFloat;

         // etFloat can be recognized as etInteger :
         if (aValueType = etFloat) and (aExpression.FRecognizedType <> aValueType) then
           aValueType := etInteger;

         if aValueType = etText  // Expression can be recognized as other type
         then
           Result := aExpression.FValue
         else
           if aExpression.FRecognizedType = aValueType then
             Result := aExpression.RecognizedValue;
       end;

var
  i, j, SortValuePosition, LastAddedExpressionIndex: Integer;
  SortedExpressionList, ExpressionList: TStringList;
  ExpressionValue: String;
  ValidationResult: TValidateElementValueResult;
  StopSearching, RemoveAll: Boolean;

       function GetExpressionIndexFromSortedList(ListIndex: Integer): Integer;
       var StrExpressionIndex: String;
       begin
         StrExpressionIndex := copy(SortedExpressionList[ListIndex], pos(';', SortedExpressionList[ListIndex]) + 1, length(SortedExpressionList[ListIndex]));
         Result := StrToInt(StrExpressionIndex);
       end;

begin
  Result := 0;
  UserAbort := false;
  SortedExpressionList := TStringList.Create;
  SortedExpressionList.Sorted := True;
  ExpressionList := TStringList.Create;

  // Retrieve all expressions in defined area sorted by ValueLocation :
  for i := 0 to ExpressionCount-1 do
    if (Expressions[i].FPageNumber = aPageNumber) or (aPageNumber = 0) then
      if Expressions[i].FAssociatedElementIndex <> OfElementIndex then  // Ignore element's Keyword ...
        if (not IsDERSymbols(Expressions[i].FDERValue)) or (SortedExpressionList.Count <> 0) then   // Remove symbol ...
          if ExpressionInArea(i) then
          begin
            // We will sort expressions as ValueLocation :
            case ValueLocation of
              slFromLeft:   SortValuePosition := Expressions[i].FLeftPxPos;
              slFromTop:    SortValuePosition := Expressions[i].FTopPxPos;
              slFromRight:  SortValuePosition := PixelsWidth - Expressions[i].FLeftPxPos;
              slFromBottom: SortValuePosition := PixelsHeight - Expressions[i].FTopPxPos;
            end;

            SortedExpressionList.Add(FormatFloat('000000', SortValuePosition) + ';' + intToStr(i));
          end;

  // Add Expressions to ExpressionList:
  for i := 0 to SortedExpressionList.Count-1 do
    ExpressionList.Add(intToStr(GetExpressionIndexFromSortedList(i)));

  // Remove all expressions from any already associated to an element until the end of the sorted list
  // Except if associated element type is date and searched value is Money/Float/Integer:
  for i := 0 to ExpressionList.Count-1 do
    with Expressions[ StrToInt(ExpressionList[i]) ] do
      if FAssociatedElementIndex <> -1 then
        if FAssociatedElementIndex <> OfElementIndex then                  // Found another Keyword or associated expression to another element ...
        begin
          RemoveAll := true;
          if FElements[OfElementIndex].ValueType in [etFloat, etMoney] then
            if FElements[FAssociatedElementIndex].ValueType = etDate then
              RemoveAll := false;

          if RemoveAll then
          begin
            for j := ExpressionList.Count-1 downto i do
              ExpressionList.Delete(j);

            Break;
          end;
        end;

  // Remove ValueNumber-1 or invalid expressions from list :
  while ExpressionList.Count <> 0 do
    if (GetExpressionValue(Expressions[ StrToInt(ExpressionList[0]) ], aValueType) = '') or (ValueNumber > 1) then // Value number for pattern recognition ...
    begin
      ExpressionList.Delete(0);
      Dec(ValueNumber);
    end
    else
      Break;

  // Validate candidates list :
  if Assigned(FOnRetrieveElementValuesFromRect) then
    FOnRetrieveElementValuesFromRect(Self, OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex, ExpressionList);

  if ExpressionList.Count <> 0 then
  begin
    // Assign Values to element and Keyword :
    StopSearching := false;
    LastAddedExpressionIndex := -1;
    while (ExpressionList.Count <> 0) and (not StopSearching) do
    begin
      ExpressionValue := GetExpressionValue(Expressions[ StrToInt(ExpressionList[0]) ], aValueType);

      if ExpressionValue <> '' then   // Same type
      begin
        ValidationResult := veValueOk;

        // Stop if distance is too big between current expression and LastAddedExpressionIndex :
        if (not PatternMode) and (LastAddedExpressionIndex <> -1) then
          if ValueLocation in [slFromTop, slFromBottom] then
            with Expressions[ StrToInt(ExpressionList[0]) ] do
              if Abs(FTopPxPos - Expressions[LastAddedExpressionIndex].FTopPxPos) > 3 * (FBottomPxPos - FTopPxPos) then  // + (Expressions[LastAddedExpressionIndex].FBottomPxPos - Expressions[LastAddedExpressionIndex].FTopPxPos)
              begin
                ValidationResult := veValueTooFar;
                StopSearching := true;
              end;

        // Validate event :
        if Assigned(FOnValidateElementValue) then
          FOnValidateElementValue(Self, OfElementIndex, OfElementKeywordIndex, OfExpressionKeywordIndex, StrToInt(ExpressionList[0]), ValidationResult);

        if ValidationResult <> veValueOk then
        begin
          ExpressionList.Delete(0);
          if ValidationResult = veInvalidValueStopSearching then
          begin
            UserAbort := true;
            StopSearching := True;
          end;

          // RHR AfterValidateEvent ...
          Continue;
        end;

        // Assign value to element:
        Inc(Result);
        LastAddedExpressionIndex := StrToInt(ExpressionList[0]);
        with Expressions[LastAddedExpressionIndex] do
        begin
          FAssociatedElementIndex := OfElementIndex;
          FAssociatedElementKeywordIndex := OfElementKeywordIndex;
          FAssociatedExpressionKeywordIndex := OfExpressionKeywordIndex;
        end;

        if aValueType in [etNumbers, etText] then
        begin
          if ValueLocation in [slFromTop, slFromBottom] then
          begin
            if ValueCount <> 0 then // Retrieve a specified number of expressions ...
              if ValueCount = Result then
                StopSearching := True;
          end
          else               // Retrieving from Left / right
            if ValueCount = 1 then
              StopSearching := True;
        end
        else
          if ValueCount = 0 then
          begin
            if (not PatternMode) and (ValueLocation = slFromLeft) then   // Retrieve only first value in the horizontal position ...
              StopSearching := True;
          end
          else           // Retrieve a specified number of expressions ...
            if ValueCount = Result then
              StopSearching := True;

      end
      else
        StopSearching := true;

      ExpressionList.Delete(0);
      // RHR AfterValidate event ...
    end;
  end;

  ExpressionList.Free;
  SortedExpressionList.Free;
end;

procedure TcyDocER.RecognizeElementValues(ElementIndex: Integer);
var
  ElementValuesCount, ExpressionValuesCount, k, i: Integer;
  PatternExpressionIndex: Integer;
  ReadingDirection1: TReadingDirection;
  ReadingDirection2: TReadingDirection;
  ValueLocation: TSearchValueLocation;
  LeftPx, TopPx, RightPx, BottomPx, PageNumber: Integer;
  UserAborted: Boolean;

      procedure InitializeVariables(aReadingDirection: TReadingDirection; ExpressionIndex: Integer);
      var ExpressionHeight: Integer;
      begin
        case aReadingDirection of
          rdRight:     // We will search values at expression right side
          begin
            ValueLocation := slFromLeft;
            LeftPx   := Expressions[ExpressionIndex].FLeftPxPos;
            RightPx  := FPixelsWidth;

            // Using only HorzLinePx don' t work if keyword expression in more than single line and searched value not centered horinzontally :
            // HorzLinePx := Expressions[ExpressionIndex].FTopPxPos + ExpressionHeight div 2;
            ExpressionHeight := Expressions[ExpressionIndex].FBottomPxPos - Expressions[ExpressionIndex].FTopPxPos;
            TopPx    := Expressions[ExpressionIndex].FTopPxPos + ExpressionHeight div 4;
            BottomPx := Expressions[ExpressionIndex].FBottomPxPos - ExpressionHeight div 4;
          end;

          rdBottom:    // We will search values at expression bottom
          begin
            ValueLocation := slFromTop;
            LeftPx   := Expressions[ExpressionIndex].FLeftPxPos;   // We can have problems if values not aligned correctly RHR
            TopPx    := Expressions[ExpressionIndex].FTopPxPos;
            RightPx  := Expressions[ExpressionIndex].FRightPxPos;  // We can have problems if values not aligned correctly RHR
            BottomPx := FPixelsHeight;
          end;
        end;
      end;

begin
  ElementValuesCount := 0;

  if Elements[ElementIndex].PatternRecognition then
  begin
    if Elements[ElementIndex].FPatternPageNumberMode = ppFromBeginning
    then PageNumber := Elements[ElementIndex].FPatternPageNumber
    else PageNumber := FPageCount - Elements[ElementIndex].FPatternPageNumber + 1;

    if Elements[ElementIndex].FPatternPositionMode = ppFromTopLeftPage then
    begin
      LeftPx   := MmToPx(Elements[ElementIndex].FPatternFromLeftMm);
      TopPx    := MmToPx(Elements[ElementIndex].FPatternFromTopMm);
      RightPx  := MmToPx(Elements[ElementIndex].FPatternToRightMm);
      BottomPx := MmToPx(Elements[ElementIndex].FPatternToBottomMm);
    end
    else begin
      // Get associated keyword:
      PatternExpressionIndex := -1;
      for i := 0 to ExpressionCount-1 do
        if Expressions[i].FAssociatedElementIndex = ElementIndex then
        begin
          PatternExpressionIndex := i;
          Break;
        end;

      if PatternExpressionIndex = -1 then Exit;

      LeftPx   := MmToPx(Elements[ElementIndex].FPatternFromLeftMm + Expressions[PatternExpressionIndex].FLeftPxPos);
      TopPx    := MmToPx(Elements[ElementIndex].FPatternFromTopMm + Expressions[PatternExpressionIndex].FTopPxPos);
      RightPx  := MmToPx(Elements[ElementIndex].FPatternToRightMm + Expressions[PatternExpressionIndex].FRightPxPos);
      BottomPx := MmToPx(Elements[ElementIndex].FPatternToBottomMm + Expressions[PatternExpressionIndex].FBottomPxPos);
    end;

    ElementValuesCount := RecognizeElementValuesFromRect(
                           ElementIndex,
                           -1,
                           -1,
                           Elements[ElementIndex].FValueType,
                           PageNumber,
                           LeftPx,
                           TopPx,
                           RightPx,
                           BottomPx,
                           Elements[ElementIndex].FPatternValueLocation,
                           Elements[ElementIndex].FPatternValueNumber,
                           Elements[ElementIndex].FValueCount,
                           True, UserAborted);

    Exit;
  end;


  if Elements[ElementIndex].ValueCount = 1 then
  begin
    ReadingDirection1 := rdRight;
    ReadingDirection2 := rdBottom;
  end
  else begin
    ReadingDirection1 := rdBottom;
    ReadingDirection2 := rdRight;
  end;

  // Deal keywords list by roKeywordsByPriority:
  for k := 0 to Elements[ElementIndex].KeyWords.Count-1 do
  begin
    i := 0;
    while i <= ExpressionCount-1 do
    begin
      ExpressionValuesCount := 0;

      if Expressions[i].FRecognizedType = etExpressionKeyWord then
        if (Expressions[i].AssociatedElementIndex = ElementIndex) and (Expressions[i].AssociatedElementKeywordIndex = k) then
        begin
          // Read associated value(s) from ReadingDirection1 :
          InitializeVariables(ReadingDirection1, i);

          ExpressionValuesCount := RecognizeElementValuesFromRect(
                                     ElementIndex,
                                     k,
                                     i,
                                     Elements[ElementIndex].FValueType,
                                     Expressions[i].FPageNumber,
                                     LeftPx,
                                     TopPx,
                                     RightPx,
                                     BottomPx,
                                     ValueLocation,
                                     1,
                                     Elements[ElementIndex].FValueCount, false, UserAborted);

          // Read associated value(s) from ReadingDirection2 :
          if (ExpressionValuesCount = 0) and (not UserAborted) then
          begin
            InitializeVariables(ReadingDirection2, i);
            ExpressionValuesCount := RecognizeElementValuesFromRect(
                                       ElementIndex,
                                       k,
                                       i,
                                       Elements[ElementIndex].FValueType,
                                       Expressions[i].FPageNumber,
                                       LeftPx,
                                       TopPx,
                                       RightPx,
                                       BottomPx,
                                       ValueLocation,
                                       1,
                                       Elements[ElementIndex].FValueCount, false, UserAborted);
          end;

          Inc(ElementValuesCount, ExpressionValuesCount);
        end;

      Inc(i);
    end;
  end;
end;

procedure TcyDocER.InitializeRecognition;
var
  i, m: Integer;
  MoneyHandled: Boolean;
begin
  // Initialize :
  for i := 0 to ExpressionCount-1 do
  begin
    Expressions[i].FAssociatedElementIndex := -1;
    Expressions[i].FAssociatedElementKeywordIndex := -1;
    Expressions[i].FAssociatedExpressionKeywordIndex := -1;
    if Expressions[i].FRecognizedType = etExpressionKeyWord then
      Expressions[i].FRecognizedType := etText;
  end;

  // Merge some separated cars in some cases :
  for i := ExpressionCount-1 downto 0 do
  begin
    // Merge percentage symbol with its value :
    if Expressions[i].FDERValue = '%' then
      if i > 0 then
        if Expressions[i-1].FRecognizedType in [etInteger, etFloat] then
          if ExpressionsSideBySide(i-1, i, Round((Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos) * 1.5)) then
            MergeExpressions(i, i-1);

    // Merge money char with its value:
    for m := 1 to length(DERMoneyCars) do
      if Expressions[i].FDERValue = DERMoneyCars[m] then
      begin
        MoneyHandled := false;

        // Value is before ?
        if i > 0 then
          if Expressions[i-1].FRecognizedType in [etInteger, etFloat] then
            if ExpressionsSideBySide(i-1, i, Round((Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos) * 1.5)) then
            begin
              MoneyHandled := true;
              MergeExpressions(i, i-1);
            end;

        // Value is after ?
        if not MoneyHandled then
          if i < ExpressionCount-1 then
            if Expressions[i+1].FRecognizedType in [etInteger, etFloat] then
              if ExpressionsSideBySide(i, i+1, Round((Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos) * 1.5)) then
                MergeExpressions(i, i+1);
      end;
  end;
end;

function TcyDocER.IsElementKeyword(aElementIndex: Integer; aStr: String): Integer;
var
  k: Integer;
  aKeyword: String;
begin
  Result := -1;
  aStr := AnsiUpperCase( StringToDERCharSet(aStr) );

  for k := 0 to Elements[aElementIndex].KeyWords.Count-1 do
  begin
    aKeyword := AnsiUpperCase( StringToDERCharSet(Elements[aElementIndex].KeyWords[k]) );

    if aKeyword = aStr then
    begin
      Result := k;
      Break;
    end;
  end;
end;

procedure TcyDocER.RecognizeElementsKeywords;
type
  rKeywords = record
    ElementIndex: Integer;
    KeywordIndex: Integer;
    LengthKeyword: Integer;
  end;

          procedure OrderByLengthAndRecognizeKeywords;
          var
            SortedKeywords: Array of rKeywords;
            NewElementIndex, NewKeywordIndex, NewLengthKeyword: Integer;
            TmpElementIndex, TmpKeywordIndex, TmpLengthKeyword: Integer;
            i, s, k, LengthSortedKeywords: Integer;
            ExpressionFound: Boolean;
          begin
            LengthSortedKeywords := 0;

            for i := 0 to FElements.Count-1 do
              for k := 0 to FElements[i].FKeyWords.Count-1 do
              begin
                NewElementIndex  := i;
                NewKeywordIndex  := k;
                NewLengthKeyword := Length(FElements[i].FKeyWords[k]);

                // Insert in array ordered by keyword's length :
                for s := 0 to LengthSortedKeywords-1 do
                  if NewLengthKeyword > SortedKeywords[s].LengthKeyword then
                  begin
                    TmpElementIndex  := SortedKeywords[s].ElementIndex;
                    TmpKeywordIndex  := SortedKeywords[s].KeywordIndex;
                    TmpLengthKeyword := SortedKeywords[s].LengthKeyword;

                    SortedKeywords[s].ElementIndex  := NewElementIndex;
                    SortedKeywords[s].KeywordIndex  := NewKeywordIndex;
                    SortedKeywords[s].LengthKeyword := NewLengthKeyword;

                    NewElementIndex  := TmpElementIndex;
                    NewKeywordIndex  := TmpKeywordIndex;
                    NewLengthKeyword := TmpLengthKeyword;
                  end;

                // Insert at the end :
                Inc(LengthSortedKeywords);
                SetLength(SortedKeywords, LengthSortedKeywords);
                SortedKeywords[LengthSortedKeywords-1].ElementIndex  := NewElementIndex;
                SortedKeywords[LengthSortedKeywords-1].KeywordIndex  := NewKeywordIndex;
                SortedKeywords[LengthSortedKeywords-1].LengthKeyword := NewLengthKeyword;
              end;

            // Recognize inline (in a single line) keywords :
            if FRecognitionPriorityMode = rpSinglelineKeywordsLength then
              for k := 0 to LengthSortedKeywords-1 do
                repeat
                  ExpressionFound := RecognizeElementKeyword(SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex, true);   // Search same keyword until not ExpressionFound
                until not ExpressionFound;

            // Recognize keywords :
            for k := 0 to LengthSortedKeywords-1 do
              repeat
                ExpressionFound := RecognizeElementKeyword(SortedKeywords[k].ElementIndex, SortedKeywords[k].KeywordIndex, false);   // Search same keyword until not ExpressionFound
              until not ExpressionFound;
          end;

          // Find all element' s Keyword ocurrences in the document:
          procedure RecognizeElementKeywords(ElementIndex: Integer);
          var
            ExpressionFound: Boolean;
            k: Integer;
          begin
            // Search for all keywords of specified element:
            for k := 0 to FElements[ElementIndex].FKeyWords.Count-1 do
              repeat
                ExpressionFound := RecognizeElementKeyword(ElementIndex, k, false);   // Search same keyword until not ExpressionFound
              until not ExpressionFound;
          end;

var
  i: Integer;
begin
  if Assigned(FBeforeRecognizeElementsKeywords) then
    FBeforeRecognizeElementsKeywords(Self);

  // *** Find Keywords *** //
  if roKeywordsByPriority in FRecognitionOptions then
  begin
    OrderByLengthAndRecognizeKeywords;
  end
  else begin
    for i := 0 to Elements.Count-1 do
      RecognizeElementKeywords(i);
  end;
end;

procedure TcyDocER.RecognizeElementsValues;
var
  i: Integer;
begin
  if Assigned(FBeforeRecognizeElementsValues) then
    FBeforeRecognizeElementsValues(Self);

  // *** Find values *** //
  for i := 0 to Elements.Count-1 do       // Single value recognition ...
    if Elements[i].ValueCount = 1 then
      RecognizeElementValues(i);

  for i := 0 to Elements.Count-1 do       // Multiple values recognition ...
    if Elements[i].ValueCount <> 1 then
      RecognizeElementValues(i);
end;

function TcyDocER.RecognizeMonth(aDERString: DERString): Integer;
var
  m, i: Integer;
  ShortMonthValue: DERString;
begin
  Result := 0;
  if FShortMonthNames.Count < 12 then Exit;

  aDERString := AnsiUppercase(aDERString);

  for m := 1 to 12 do
  begin
    ShortMonthValue := AnsiUppercase( StringToDERCharSet(FShortMonthNames[m-1]) );

    for i := 1 to length(ShortMonthValue) do
      if not (ShortMonthValue[i] in ['A'..'Z']) then  // Remove any invalid char like '.' from "Jan." for exemple ...
      begin
        Delete(ShortMonthValue, i, Length(ShortMonthValue));
        Break;
      end;

    if pos(ShortMonthValue, aDERString) > 0 then
    begin
      Result := m;
      Break;
    end;
  end;
end;

procedure TcyDocER.RecognizeElements;
begin
  InitializeRecognition;

  RecognizeElementsKeywords;
  RecognizeElementsValues;
end;

// Handle some anormal situations :
// - 3 625,00 divided into 2 expressions ("3" and "625,00")
// - 3 625,00 € divided into 2 expressions ("3" and "625,00€")
// - 3 625 divided into 2 expressions ("3" and "625")
// - 3 625 € divided into 2 expressions ("3" and "625€")
// - Long date detection ([day] + [Month name] + [year]) :
procedure TcyDocER.ExpressionAdded;
var
  b: Boolean;
  pxTolerence, aYear, aMonth, aDay: Integer;
  aDate: TDateTime;

      // See there' s natural separators into expression value: '()'
      function CanMergeExpressions(aExpressionIndex1, aExpressionIndex2: Integer): Boolean;
      begin
        Result := true;
        if pos('(', Expressions[aExpressionIndex1].Value) <> 0 then Result := false;
        if pos(')', Expressions[aExpressionIndex1].Value) <> 0 then Result := false;

        if pos('(', Expressions[aExpressionIndex2].Value) <> 0 then Result := false;
        if pos(')', Expressions[aExpressionIndex2].Value) <> 0 then Result := false;
      end;

begin
  if ExpressionCount <= 1 then Exit;

  pxTolerence := Round((Expressions[ExpressionCount-1].FBottomPxPos - Expressions[ExpressionCount-1].FTopPxPos) * 1.5);

  // Merge with negative symbol :
  if Expressions[ExpressionCount-2].FDERValue = '-' then
    if Expressions[ExpressionCount-1].FRecognizedType in [etMoney, etFloat, etInteger, etPercentage] then
    begin
      if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
      begin
        MergeExpressions(ExpressionCount-1, ExpressionCount-2);
        if ExpressionCount <= 1 then Exit;
      end;
    end;

  // Merge with prior added expression value for cutted float/integer/money/percentage values:
  case Expressions[ExpressionCount-1].FRecognizedType of
    etMoney:
    begin
      if Expressions[ExpressionCount-1].FDERValue[1] in ['.', '0'..'9'] then    // Merge if money symbol at the end!
      begin
        if Expressions[ExpressionCount-1].FRecognizedFloat = Expressions[ExpressionCount-1].FRecognizedInteger
        then b := Expressions[ExpressionCount-2].FRecognizedType in [etFloat, etInteger]                         // Money value is an integer
        else b := Expressions[ExpressionCount-2].FRecognizedType in [etInteger];                                 // Money value is a float

        if b then
          if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
            if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
            begin
              MergeExpressions(ExpressionCount-1, ExpressionCount-2);
            end;
      end;
    end;

    etFloat:                //  "22  33,6"         or      "€22 33,6"
    begin
      case Expressions[ExpressionCount-2].FRecognizedType of
        etInteger:
          if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
            if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
            begin
              MergeExpressions(ExpressionCount-1, ExpressionCount-2);
            end;

        etMoney:
          if not (Expressions[ExpressionCount-2].FDERValue[1] in ['.', '0'..'9']) then    // Merge if money symbol at the beginning!
            if Expressions[ExpressionCount-2].FRecognizedFloat = Expressions[ExpressionCount-2].FRecognizedInteger then    // Money value is an integer
              if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
                if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
                begin
                  MergeExpressions(ExpressionCount-1, ExpressionCount-2);
                end;
      end;
    end;

    etInteger, etPercentage:
    begin
      if (Expressions[ExpressionCount-1].FRecognizedType <> etPercentage) or (Expressions[ExpressionCount-2].FRecognizedType <> etMoney) then // Cannot merge money with percentage expressions
        case Expressions[ExpressionCount-2].FRecognizedType of
          etText:
            if (ExpressionCount > 2) and (Expressions[ExpressionCount-2].FDERValue = '.') then // Decimal separator
              if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
                if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
                  if CanMergeExpressions(ExpressionCount-2, ExpressionCount-3) then
                    case Expressions[ExpressionCount-3].FRecognizedType of
                      etInteger:
                        if ExpressionsSideBySide(ExpressionCount-3, ExpressionCount-2, pxTolerence) then
                        begin
                          MergeExpressions(ExpressionCount-1, ExpressionCount-2);
                          MergeExpressions(ExpressionCount-1, ExpressionCount-2);
                        end;

                      etMoney:
                        if not (Expressions[ExpressionCount-3].FDERValue[1] in ['0'..'9']) then    // Merge if money symbol at the beginning!
                          if ExpressionsSideBySide(ExpressionCount-3, ExpressionCount-2, pxTolerence) then
                          begin
                            MergeExpressions(ExpressionCount-1, ExpressionCount-2);
                            MergeExpressions(ExpressionCount-1, ExpressionCount-2);
                          end;
                    end;

          etInteger:
            if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
              if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
                MergeExpressions(ExpressionCount-1, ExpressionCount-2);

          etFloat:
            if Expressions[ExpressionCount-1].RecognizedFloat = Expressions[ExpressionCount-1].RecognizedInteger then
              if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
                if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
                  MergeExpressions(ExpressionCount-1, ExpressionCount-2);

          etMoney:
            if not (Expressions[ExpressionCount-2].FDERValue[1] in ['.', '0'..'9']) then    // Merge if money symbol at the beginning!
              if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
                if CanMergeExpressions(ExpressionCount-1, ExpressionCount-2) then
                  MergeExpressions(ExpressionCount-1, ExpressionCount-2);
        end;
    end;
  end;

  // short/long Date detection :
  if FShortMonthNames.Count >= 12 then
    if (ExpressionCount > 2) and (Expressions[ExpressionCount-1].FRecognizedType = etInteger) then
      if Length(Expressions[ExpressionCount-1].FRecognizedInteger) = 4 then            // Can use StrToInt() ?
      if (StrToInt(Expressions[ExpressionCount-1].FRecognizedInteger) > 1940)          // We are in presence of a year!?
         and (StrToInt(Expressions[ExpressionCount-1].FRecognizedInteger) < 2100) then
        if ExpressionsSideBySide(ExpressionCount-2, ExpressionCount-1, pxTolerence) then
        begin
          aMonth := RecognizeMonth(Expressions[ExpressionCount-2].FDERValue);

          if aMonth <> 0 then
            if ExpressionsSideBySide(ExpressionCount-3, ExpressionCount-2, pxTolerence) then
              if Expressions[ExpressionCount-3].FRecognizedType = etInteger then
                if length(Expressions[ExpressionCount-3].FRecognizedInteger) < 3 then // We are in presence of a day !?
                  if StrToInt(Expressions[ExpressionCount-3].FRecognizedInteger) > 0 then
                    try
                      // Validate date :
                      aYear := StrToInt(Expressions[ExpressionCount-1].FRecognizedInteger);
                      aDay  := StrToInt(Expressions[ExpressionCount-3].FRecognizedInteger);

                      if (aDay > 0) and (aDay < 32) then
                        if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
                        begin
                          // Merge 3 expressions into a date :
                          MergeExpressions(ExpressionCount-1, ExpressionCount-2);
                          MergeExpressions(ExpressionCount-1, ExpressionCount-2);  // Carefull: ExpressionCount was modified by MergeExpressions() call ...

                          with Expressions[ExpressionCount-1] do
                          begin
                            FRecognizedDate := intToStr(Trunc(aDate));
                            FRecognizedType := etDate;
                          end;
                        end;
                    except

                    end;
        end;
end;

procedure TcyDocER.ExpressionLoaded;
var
  aYear, aMonth, aDay: Integer;
  aDate: TDateTime;
begin
  // short/long Date detection :
  if FShortMonthNames.Count >= 12 then
    if SubString_Count(Expressions[ExpressionCount-1].FValue, ' ') >= 3 then
      if TryStrToInt( SubString_Get(Expressions[ExpressionCount-1].FValue, ' ', 3 ), aYear ) then
        if (aYear > 1940) and (aYear < 2100) then
        begin
          aMonth := RecognizeMonth( SubString_Get(Expressions[ExpressionCount-1].FValue, ' ', 2 ) );

          if aMonth <> 0 then
            if TryStrToInt( SubString_Get(Expressions[ExpressionCount-1].FValue, ' ', 1 ), aDay ) then
              if (aDay > 0) and (aDay < 32) then
                try
                  if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
                    with Expressions[ExpressionCount-1] do
                    begin
                      FRecognizedDate := intToStr(Trunc(aDate));
                      FRecognizedType := etDate;
                    end;
                except

                end;
        end;

end;

function TcyDocER.ExpressionInRect(aExpressionIndex: Integer; InRect: TRect; const AllowPartiallyInside: Boolean = true): Boolean;
var DestRect: TRect;
begin
  Result := false;
  with Expressions[aExpressionIndex] do
    if AllowPartiallyInside then
      Result := IntersectRect(DestRect, InRect, classes.Rect(FLeftPxPos, FTopPxPos, FRightPxPos, FBottomPxPos))
    else
      if (FLeftPxPos >= InRect.Left) and (FTopPxPos >= InRect.Top) and (FRightPxPos <= InRect.Right) and (FBottomPxPos <= InRect.Bottom) then
        Result := true;
end;

// - Long date detection ([day] + [Month name] + [year]) :
procedure TcyDocER.RecognizeLongDates;
var
  i, pxTolerence, aYear, aMonth, aDay: Integer;
  aDate: TDateTime;
begin
  if FShortMonthNames.Count < 12 then Exit;
  i := ExpressionCount-1;

  while i > 0 do
  begin
    if (i >= 2) and (Expressions[i].FRecognizedType = etInteger) then
      if Length(Expressions[i].FRecognizedInteger) = 4 then            // Can use StrToInt() ?
      if (StrToInt(Expressions[i].FRecognizedInteger) > 1940)          // We are in presence of a year!?
         and (StrToInt(Expressions[i].FRecognizedInteger) < 2100) then
        if ExpressionsSideBySide(i-1, i, pxTolerence) then
        begin
          aMonth := RecognizeMonth(Expressions[i-1].FDERValue);

          if aMonth <> 0 then
            if ExpressionsSideBySide(i-2, i-1, pxTolerence) then
              if Expressions[i-2].FRecognizedType = etInteger then
                if length(Expressions[i-2].FRecognizedInteger) < 3 then // We are in presence of a day !?
                  try
                    // Validate date :
                    aYear := StrToInt(Expressions[i].FRecognizedInteger);
                    aDay  := StrToInt(Expressions[i-2].FRecognizedInteger);

                    if (aDay > 0) and (aDay < 32) then
                      if TryToEncodeDate(aYear, aMonth, aDay, aDate) then
                      begin
                        // Merge 3 expressions into a date :
                        MergeExpressions(i, i-1);
                        Dec(i);
                        MergeExpressions(i, i-1);  // Carefull: ExpressionCount was modified by MergeExpressions() call ...
                        Dec(i);

                        with Expressions[i] do
                        begin
                          FRecognizedDate := intToStr(Trunc(aDate));
                          FRecognizedType := etDate;
                        end;
                      end;
                  except

                  end;
        end;

    Dec(i);
  end;
end;

function TcyDocER.GetOCRTextFromRect(FromRect: TRect; FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
var
  i: Integer;
  IgnoreFromRect: Boolean;
  IncludeExpression: Boolean;
  StrConexion: String;
begin
  Result := '';
  IgnoreFromRect := (FromRect.Left = 0) and (FromRect.Top = 0) and (FromRect.Right = 0) and (FromRect.Bottom = 0);

  for i := 0 to ExpressionCount-1 do
  begin
    IncludeExpression := (FromPage = 0) or (FromPage = Expressions[i].PageNumber);

    if IncludeExpression and (not IgnoreFromRect) then
      IncludeExpression := ExpressionInRect(i, FromRect, AllowPartiallyInside);

    if IncludeExpression then
    begin
      StrConexion := '';

      if (Result <> '') and (i > 0) then
        if Expressions[i].FPageNumber = Expressions[i-1].FPageNumber then
          if ExpressionsInSameLine(i, i-1)
          then StrConexion := ' '
          else StrConexion := #13#10;

      Result := Result + StrConexion + Expressions[i].FValue;
    end;
  end;
end;

function TcyDocER.GetOCRText(const FromPage: Integer): String;
begin
  Result := GetOCRTextFromRect(classes.Rect(0, 0, 0, 0), FromPage, true);
end;

function TcyDocER.GetAsDocumentOCRText(const FromPage: Integer = 0): String;
begin
  Result := GetAsDocumentOCRTextFromRect(classes.Rect(0, 0, 0, 0), FromPage, true);
end;

function TcyDocER.GetAsDocumentOCRTextFromRect(FromRect: TRect; FromPage: Integer; const AllowPartiallyInside: Boolean = true): String;
var
  i, c, CarsCount, EntersCount: Integer;
  CurrentPage, CurrentPageLinesCount: Integer;
  IgnoreFromRect: Boolean;
  IncludeExpression: Boolean;
  StrConexion: String;

  TotalCars: Int64;
  TotalCarsWidth, TotalExpressionsHeight: Int64;

const
  FontSize = 10;

      function CalcSpaceChars(PixelsWidth: Integer): Integer;
      begin
        Result := 0;
        if FResolution = 0 then Exit;

        Result := round(PixelsWidth / FResolution * FontSize);
      end;

      function CalcEnters(PixelsHeight: Integer): Integer;
      begin
        Result := 0;
        if FResolution = 0 then Exit;

        Result := round( (PixelsHeight / FResolution * FontSize) / 1.5);
      end;

      function GetLastLineLength(aStr: String): Integer;
      var i: Integer;
      begin
        Result := 0;
        for i := Length(aStr) downto 1 do
          if aStr[i] <> #10
          then Inc(Result)
          else Break;
      end;

begin
  // Initialisation :
  Result := '';
  CurrentPage := -1;

  // Determine font size :
  TotalCars := 0;
  TotalCarsWidth := 0;
  TotalExpressionsHeight := 0;
  for i := 0 to ExpressionCount-1 do
  begin
    inc(TotalCars, length(Expressions[i].FValue));
    inc(TotalCarsWidth, (Expressions[i].FRightPxPos - Expressions[i].FLeftPxPos));
    inc(TotalExpressionsHeight, (Expressions[i].FBottomPxPos - Expressions[i].FTopPxPos));
  end;

  // Extract text :
  IgnoreFromRect := (FromRect.Left = 0) and (FromRect.Top = 0) and (FromRect.Right = 0) and (FromRect.Bottom = 0);

  for i := 0 to ExpressionCount-1 do
  begin
    if CurrentPage <> Expressions[i].PageNumber then
    begin
      CurrentPage := Expressions[i].PageNumber;
      CurrentPageLinesCount := 1;
    end;

    IncludeExpression := (FromPage = 0) or (FromPage = Expressions[i].PageNumber);

    if IncludeExpression and (not IgnoreFromRect) then
      IncludeExpression := ExpressionInRect(i, FromRect, AllowPartiallyInside);

    if IncludeExpression then
    begin
      if i > 0 then
      begin
        if ExpressionsInSameLine(i, i-1) and (Expressions[i].FPageNumber = Expressions[i-1].FPageNumber) then
        begin
          // *** Continue line *** //
          StrConexion := '';

          // Insert space chars :
          CarsCount := CalcSpaceChars(Expressions[i].FLeftPxPos) - GetLastLineLength(Result);
          if CarsCount <= 0 then CarsCount := 1;     // At least one char ...

          for c := 1 to CarsCount do
            StrConexion := StrConexion + ' ';
        end
        else begin
          // *** From new line/new page *** //
          StrConexion := '';

          // Insert Enters :
          EntersCount := CalcEnters(Expressions[i].FTopPxPos) - CurrentPageLinesCount;
          if EntersCount <= 0 then EntersCount := 1; // At least one Enter ...
          for c := 1 to EntersCount do
            StrConexion := StrConexion + #13#10;

          Inc(CurrentPageLinesCount, EntersCount);

          // Insert space chars :
          CarsCount := CalcSpaceChars(Expressions[i].FLeftPxPos);
          for c := 1 to CarsCount do
            StrConexion := StrConexion + ' ';
        end;
      end
      else begin
        // *** First document line *** //
        StrConexion := '';

        // Insert Enters :
        EntersCount := CalcEnters(Expressions[i].FTopPxPos);

        for c := 1 to EntersCount do
          StrConexion := StrConexion + #13#10;

        Inc(CurrentPageLinesCount, EntersCount);

        // Insert space chars :
        CarsCount := CalcSpaceChars(Expressions[i].FLeftPxPos);
        for c := 1 to CarsCount do
          StrConexion := StrConexion + ' ';
      end;

      Result := Result + StrConexion + Expressions[i].FValue;
    end;
  end;
end;

procedure TcyDocER.RotatePageExpressions(PageNumber, PageWidthBeforeRotation, PageHeightBeforeRotation: Integer; ToRight: Boolean);
var
  i, SaveLeftPxPos, SaveRightPxPos, SaveTopPxPos, SaveBottomPxPos: Integer;
begin
  for i := 0 to ExpressionCount-1 do
    if (PageNumber = 0) or (Expressions[i].FPageNumber = PageNumber) then
    begin
      SaveLeftPxPos := Expressions[i].FLeftPxPos;
      SaveRightPxPos := Expressions[i].FRightPxPos;
      SaveTopPxPos := Expressions[i].FTopPxPos;
      SaveBottomPxPos := Expressions[i].FBottomPxPos;

      if ToRight then
      begin
        Expressions[i].FLeftPxPos   := PageHeightBeforeRotation - SaveBottomPxPos;
        Expressions[i].FRightPxPos  := PageHeightBeforeRotation - SaveTopPxPos;
        Expressions[i].FTopPxPos    := SaveLeftPxPos;
        Expressions[i].FBottomPxPos := SaveRightPxPos;
      end
      else begin
        Expressions[i].FLeftPxPos   := SaveTopPxPos;
        Expressions[i].FRightPxPos  := SaveBottomPxPos;
        Expressions[i].FTopPxPos    := PageWidthBeforeRotation - SaveRightPxPos;
        Expressions[i].FBottomPxPos := PageWidthBeforeRotation - SaveLeftPxPos;
      end;
    end;
end;

end.
