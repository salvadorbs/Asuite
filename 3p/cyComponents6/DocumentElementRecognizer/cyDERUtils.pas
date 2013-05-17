unit cyDERUtils;

{ cyDERUtils
  Description: Document elements recognition utilities

  Definitions:
  Elements: value types recognized on documents like date, money, webiste, webmail, percentage, float, integer and etNumber
  - etDate: Date recognition in any formats ('00/00', '00-00-0000' etc ...)
  - etMoney: money values recognition ('30.56€', '$30.56' etc ...)
  - etwebSite: webSite recognition
  - etWebMail: webmail recognition
  - etPercentage: percentage values recognition like VAT ('5%')
  - etFloat: float values recognition
  - etInteger: integer values recognition
  - etNumbers: Integer, Postal code, telefone, fax recognition
  - etExpressionKeyWord: text that was been defined has a key

  DERString: avoid trash cars from OCR's isolated word using StringToDERCharSet() function

  DERNString: charset for date, money, percentage, float and integer elements recognition
}

interface

uses Classes, Windows, Controls, SysUtils, cyDateUtils;

type
  DERString = String;
  DERChar = Char;
  TElementsType = (etText, etExpressionKeyWord, etNumbers, etInteger, etFloat, etPercentage, etwebSite, etWebMail, etMoney, etDate);
  TElementsTypes = Set of TElementsType;

  // For smart numbers Recognition :
  DERNString = String;

var
  DERMoneyCars: String = '€';       // Add/redefine money chars
  DERExceptionCars: String = '';    // Add/redefine exceptions chars

  LocalFormatSettings: TFormatSettings;

const
  DERDecimalSeparator = '.';
  DERDefaultChars = '+º@/%-_.:0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';  // Charset used to recognize all elements

  DERNDefaultChars = '/%-.0123456789abcdefghjkmnopqrstuvwxyz'; // Charset used to recognize etInteger, etnumbers, etFloat, etPercentage, etMoney, etDate elements

// *** General functions *** //
function isValidWebSiteChar(aChar: Char): Boolean;
function isValidWebMailChar(aChar: Char): Boolean;
function isValidwebSite(aStr: String): Boolean;
function isValidWebMail(aStr: String): Boolean;

function ValidateDate(aDERStr: DERString; var RsltFormat: String): Boolean;
function DERStrToDate(aDERStr, aFormat: String): TDate;


// *** DER functions *** //
// Know if a char belongs to the DER charset
function IsDERChar(aChar: Char): Boolean;

// Know if a char is a defined on DERDefaultChars variable
function IsDERDefaultChar(aChar: Char): Boolean;

// Know if a char is a money char defined on DERMoneyCars variable
function IsDERMoneyChar(aChar: Char): Boolean;

// Know if a char is an exception char defined on DERExceptionCars variable
function IsDERExceptionCar(aChar: Char): Boolean;

// Know if a DERString only contains symbols
function IsDERSymbols(aDERString: String): Boolean;

// Convert a string to DERString :
function StringToDERCharSet(aStr: String): DERString;

// *** DERN functions *** //
function IsDERNDefaultChar(aChar: Char): Boolean;

function IsDERNChar(aChar: Char): Boolean;

// Convert a DERString into a DERNString that was made for numbers Recognition :
function DERToDERNCharset(aDERStr: DERString): DERNString;

// *** Other functions *** //
// Get any webSite present in a DERString :
function DERExtractwebSite(aDERStr: DERString; SmartRecognition: Boolean): String;

// Get any webmail present in a DERString :
function DERExtractWebMail(aDERStr: DERString): String;

function DERExtractPhoneNr(aDERStr: DERString): String;

// Recognize from string any integer/float/percentage/money/date value :
function DERExecute(aDERStr: DERString; SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType; overload;

function DERExecute(aDERStr: DERString; var RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
                     SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType; overload;

function RetrieveElementValue(aStr: String; SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; aElementsType: TElementsType): String; overload;
procedure RetrieveElementValue(aStr: String; SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; var RsltDERStr: DERString; var RsltElementType: TElementsType); overload;

implementation


function IsDERSymbols(aDERString: String): Boolean;
var i: Integer;
begin
  Result := true;

  for i := 1 to length(aDERString) do
    if aDERString[i] in ['0'..'9', 'a'..'z', 'A'..'Z'] then
    begin
      Result := false;
      Break;
    end;
end;

function isValidWebSiteChar(aChar: Char): Boolean;
begin
  Result:= aChar in ['/', ':', '.', '_', '-', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

function isValidWebMailChar(aChar: Char): Boolean;
begin
  Result:= aChar in ['a'..'z', '@', '.', '_', '-', '0'..'9', 'A'..'Z'];
end;

// Valid webSite:
// 'http://' + domain + '.' + country       (We consider 'www.' as part of domain)
// domain + '.' + country
function isValidwebSite(aStr: String): Boolean;
var
  i: Integer;
  WasSpecialChar, FoundDomain, FoundCountry: Boolean;

        function ValidCountry(StrCountry: String): Boolean;
        var i: Integer;
        begin
          Result := false;

          if Length(StrCountry) in [2, 3]  then
          begin
            Result := True;
            for i := 1 to Length(StrCountry) do
              if not (StrCountry[i] in ['a'..'z', 'A'..'Z']) then
                Result := false;
          end;
        end;

begin
  Result := false;

  aStr := AnsiLowercase(aStr);
  if pos('http://', aStr) = 1 then
    Delete(aStr, 1, 7);

  if pos('https://', aStr) = 1 then
    Delete(aStr, 1, 7);

  if pos('www.', aStr) = 1 then
    Delete(aStr, 1, 4);

  if pos(':', aStr) > 0 then
    Exit;             // Result is false

  if (aStr + ' ')[1] in ['/', '.'] then
    Exit;             // Result is false

  WasSpecialChar := false;
  FoundDomain := false;
  FoundCountry := false;

  for i := length(aStr) downto 1 do
  begin
    if not isValidWebSiteChar(aStr[i]) then
      Exit;       // Result is false

    // Test 2 special chars besides :
    if aStr[i] in ['/', '.'] then
    begin
      if WasSpecialChar
      then Exit       // Result is false
      else WasSpecialChar := true;
    end
    else
      WasSpecialChar := false;

    case aStr[i] of
      '.' :
      begin
        if not FoundCountry then
        begin
          if (FoundDomain) or (i = length(aStr)) or (i = 1)
          then
            Exit   // Result is false
          else
            if ValidCountry( copy(aStr, i+1, length(aStr)) )
            then FoundCountry := true
            else Exit;
        end
        else
          if i=1 then
            Exit;     // Result is false
      end
    else
      begin
        if FoundCountry then
          FoundDomain := true;
      end;
    end;
  end;

  Result := FoundDomain and FoundCountry;
end;

// Valid @mail:
// Body + '@' + domain + '.' + country
function isValidWebMail(aStr: String): Boolean;
var
  i: Integer;
  WasSpecialChar, FoundBody, FoundDomain, FoundCountry: Boolean;
begin
  Result := false;

  WasSpecialChar := false;
  FoundBody := false;
  FoundDomain := false;
  FoundCountry := false;

  for i := length(aStr) downto 1 do
  begin
    // Test 2 special chars besides :
    if aStr[i] in ['@', '.'] then
    begin
      if WasSpecialChar
      then Exit       // Result is false
      else WasSpecialChar := true;
    end
    else
      WasSpecialChar := false;

    case aStr[i] of
      '@' :
      begin
        if FoundDomain
        then
          Exit        // Result is false
        else
          if not FoundCountry
          then Exit   // Result is false
          else FoundDomain := true;
      end;

      '.' :
      begin
        if not FoundCountry then
        begin
          if (FoundBody) or (FoundDomain) or (i = length(aStr)) or (i = 1)
          then Exit   // Result is false
          else FoundCountry := true;
        end
        else
          if i=1 then
            Exit;     // Result is false
      end
    else
      begin
        if not isValidWebMailChar(aStr[i]) then
          Exit;       // Result is false

        if FoundDomain and FoundCountry then
          FoundBody := true;
      end;
    end;
  end;

  Result := FoundBody and FoundDomain and FoundCountry;
end;

// Valid Dates exemples: (0 is optional compared to 9):
// 09 + separator + 09 + separator + 0099
// 09 + separator + 09 + separator + 99
// 0099 + separator + 09 + separator + 09
function ValidateDate(aDERStr: String; var RsltFormat: String): Boolean;
var
  SeparatorChar: String;
  DigitGroupIndex, YearGroupIndex, i: Integer;
  DigitsGroup: Array[1..3] of integer;

      function DetermineYearGroupIndex: Integer;
      begin
        Result := -1;

        if DigitsGroup[3] in [2, 4] then
          Result := 3;

        if DigitsGroup[2] = 4 then
          Result := -1;

        if DigitsGroup[1] = 4 then
          if DigitsGroup[3] = 4
          then Result := -1
          else Result := 1;
      end;

      function ReturnLetter(aLetter: Char; Times: Integer): String;
      var i: Integer;
      begin
        Result := '';
        for i:= 1 to Times do
          Result := Result + aLetter;
      end;

begin
  Result := false;
  RsltFormat := '';

  SeparatorChar := '';
  DigitGroupIndex := 1;
  DigitsGroup[1] := 0; DigitsGroup[2] := 0; DigitsGroup[3] := 0;

  for i := 1 to length(aDERStr) do
    if not (aDERStr[i] in ['0'..'9']) then
    begin
      if SeparatorChar = ''
      then
        SeparatorChar := aDERStr[i]
      else
        if SeparatorChar <> aDERStr[i] then
          Exit;

      if DigitGroupIndex = 3
      then Exit
      else Inc(DigitGroupIndex);
    end
    else
      Inc(DigitsGroup[DigitGroupIndex]);

  // Validate Date :
  if not ( (DigitsGroup[1] in [1, 2, 4]) and (DigitsGroup[2] in [1, 2, 4]) and (DigitsGroup[3] in [1, 2, 4]) ) then
    Exit;

  YearGroupIndex := DetermineYearGroupIndex;
  if YearGroupIndex = -1 then
    Exit;

  if YearGroupIndex = 3 then
  begin
    RsltFormat := ReturnLetter('d', DigitsGroup[1]) + SeparatorChar + ReturnLetter('m', DigitsGroup[2]) + SeparatorChar + ReturnLetter('y', DigitsGroup[3]);

    try
      Result := DERStrToDate(aDERStr, RsltFormat) <> 0;
    except
    end;
  end
  else begin
    RsltFormat := ReturnLetter('y', DigitsGroup[1]) + SeparatorChar + ReturnLetter('m', DigitsGroup[2]) + SeparatorChar + ReturnLetter('d', DigitsGroup[3]);

    try
      Result := DERStrToDate(aDERStr, RsltFormat) <> 0;
      Result := true;
    except
    end;
  end;
end;

function DERStrToDate(aDERStr, aFormat: String): TDate;
var TmpDateTime: TDateTime;

      function GetNumbers(Pattern: Char): String;
      var i: Integer;
      begin
        Result := '';
        for i := 1 to length(aDERStr) do
          if aFormat[i] = Pattern then
            Result := Result + aDERStr[i];
      end;

var sYear, sMonth, sDay: String;
begin
  Result := 0;

  sYear  := GetNumbers('y');
  sMonth := GetNumbers('m');
  sDay   := GetNumbers('d');

  while length(sYear) < 3 do
    sYear := '0' + sYear;

  if length(sYear) = 3 then
    sYear := '2' + sYear;

  while length(sMonth) < 2 do
    sMonth := '0' + sMonth;

  while length(sDay) < 2 do
    sDay := '0' + sDay;

  if TryToEncodeDate(StrToInt(sYear), StrToInt(sMonth), StrToInt(sDay), TmpDateTime) then
    Result := Trunc(TmpDateTime);
end;

function IsDERChar(aChar: Char): Boolean;
begin
  Result := IsDERDefaultChar(aChar);

  if not Result then
    Result := IsDERMoneyChar(aChar);

  if not Result then
    Result := IsDERExceptionCar(aChar);
end;

function IsDERDefaultChar(aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERDefaultChars) do
    if DERDefaultChars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERMoneyChar(aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERMoneyCars) do
    if DERMoneyCars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERExceptionCar(aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;
  for i := 1 to length(DERExceptionCars) do
    if DERExceptionCars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function StringToDERCharSet(aStr: String): DERString;
var
  lengthResult, i: Integer;
begin
  Result := aStr;
  lengthResult := Length(Result);

  for i := 1 to lengthResult do
  begin
    // Decimal/mail/webSite recognition :
    if Result[i] = ',' then Result[i] := '.';
    if Result[i] = ';' then Result[i] := ':';  // ';' will be converted to '.' for decimal recognition
    if Result[i] = '°' then Result[i] := 'º';  // Not same character!!!    Ord('°') = 176     Ord('º') = 186

    // Replace unexpected symbols:
    if Result[i] = '|' then Result[i] := 'l';  // Will be replaced by "1" on DERToDERNCharset() ...

    // Remove accents :
    if Result[i] = 'é' then Result[i] := 'e';
    if Result[i] = 'è' then Result[i] := 'e';
    if Result[i] = 'ê' then Result[i] := 'e';
    if Result[i] = 'ë' then Result[i] := 'e';

    if Result[i] = 'à' then Result[i] := 'a';
    if Result[i] = 'á' then Result[i] := 'a';
    if Result[i] = 'â' then Result[i] := 'a';
    if Result[i] = 'ã' then Result[i] := 'a';
    if Result[i] = 'ä' then Result[i] := 'a';

    if Result[i] = 'í' then Result[i] := 'i';
    if Result[i] = 'ì' then Result[i] := 'i';
    if Result[i] = 'î' then Result[i] := 'i';
    if Result[i] = 'ï' then Result[i] := 'i';

    if Result[i] = 'ò' then Result[i] := 'o';
    if Result[i] = 'ó' then Result[i] := 'o';
    if Result[i] = 'ô' then Result[i] := 'o';
    if Result[i] = 'õ' then Result[i] := 'o';
    if Result[i] = 'ö' then Result[i] := 'o';

    if Result[i] = 'ú' then Result[i] := 'u';
    if Result[i] = 'ù' then Result[i] := 'u';
    if Result[i] = 'û' then Result[i] := 'u';
    if Result[i] = 'ü' then Result[i] := 'u';

    if Result[i] = 'É' then Result[i] := 'E';
    if Result[i] = 'È' then Result[i] := 'E';
    if Result[i] = 'Ê' then Result[i] := 'E';
    if Result[i] = 'Ë' then Result[i] := 'E';

    if Result[i] = 'À' then Result[i] := 'A';
    if Result[i] = 'Á' then Result[i] := 'A';
    if Result[i] = 'Â' then Result[i] := 'A';
    if Result[i] = 'Ã' then Result[i] := 'A';
    if Result[i] = 'Ä' then Result[i] := 'A';

    if Result[i] = 'Í' then Result[i] := 'I';
    if Result[i] = 'Ì' then Result[i] := 'I';
    if Result[i] = 'Î' then Result[i] := 'I';
    if Result[i] = 'Ï' then Result[i] := 'I';

    if Result[i] = 'Ò' then Result[i] := 'O';
    if Result[i] = 'Ó' then Result[i] := 'O';
    if Result[i] = 'Ô' then Result[i] := 'O';
    if Result[i] = 'Õ' then Result[i] := 'O';
    if Result[i] = 'Ö' then Result[i] := 'O';

    if Result[i] = 'Ú' then Result[i] := 'U';
    if Result[i] = 'Ù' then Result[i] := 'U';
    if Result[i] = 'Û' then Result[i] := 'U';
    if Result[i] = 'Ü' then Result[i] := 'U';

    if Result[i] = '×' then Result[i] := 'x';    // × = char(215)
    if Result[i] = '·' then Result[i] := '.';
  end;

  // Remove non DER chars :
  for i := lengthResult downto 1 do
    if not IsDERChar(Result[i]) then
      Delete(Result, i, 1);
end;

function IsDERNDefaultChar(aChar: Char): Boolean;
var i: Integer;
begin
  Result := false;

  for i := 1 to length(DERNDefaultChars) do
    if DERNDefaultChars[i] = aChar then
    begin
      Result := true;
      Exit;
    end;
end;

function IsDERNChar(aChar: Char): Boolean;
begin
  Result := IsDERNDefaultChar(aChar);

  if not Result then
    Result := IsDERMoneyChar(aChar);
end;

function DERToDERNCharset(aDERStr: DERString): DERNString;
var lengthResult, i: Integer;
begin
  Result := aDERStr;
  lengthResult := length(Result);

  for i := 1 to lengthResult do
  begin
    // Numbers recognition :
    if Result[i] = ':' then Result[i] := '.';
    if Result[i] = 'O' then Result[i] := '0';
    if Result[i] = 'o' then Result[i] := '0';
    if Result[i] = 'I' then Result[i] := '1';
    if Result[i] = 'l' then Result[i] := '1';
    if Result[i] = 'S' then Result[i] := '5';

    // Negative / Date separator recognition :
    if Result[i] = '_' then Result[i] := '-';
  end;

  Result := AnsiLowerCase(Result);

  // Remove non DERN chars :
  for i := lengthResult downto 1 do
    if not IsDERNChar(Result[i]) then
      Delete(Result, i, 1);
end;

function DERExtractwebSite(aDERStr: DERString; SmartRecognition: Boolean): String;
var
  p, i, vCount: Integer;
  LowerCaseDERStr: String;
begin
  Result := '';
  if pos('@', aDERStr) <> 0 then Exit;


  // OCR text as "VWVW", "WVVW" and variants solution:
  if SmartRecognition then
  begin
    vCount := 0;
    p := pos('.', aDERStr);

    for i := p-1 downto 1 do
      if aDERStr[i] in ['v', 'V']
      then
        vCount := vCount + 1
      else
        if aDERStr[i] in ['w', 'W']
        then vCount := vCount + 2
        else Break;

    if vCount = 6 then  // We have 6 "v" that is equal to "W W W"
    begin
      Delete(aDERStr, i+1, p-i-1);
      Insert('www', aDERStr, i+1);
    end;
  end;

  LowerCaseDERStr := AnsiLowerCase(aDERStr);
  p := pos('http:', LowerCaseDERStr);
  if p = 0 then
    p := pos('www.', LowerCaseDERStr);

  if p <> 0 then
  begin
    // Extract from p :
    for i := p to length(aDERStr) do
      if isValidWebSiteChar(aDERStr[i])
      then Result := Result + aDERStr[i]
      else Break; // End of website string ...

    // Validate :
    if not isValidwebSite(Result) then
      Result := '';
  end;

  while (pos('.', aDERStr) > 0) and (Result = '') do
  begin
    // We Will try to extract website from here :
    p := pos('.', aDERStr);

    if (p > 1) and (p < length(aDERStr)) then
    begin
      for i := p downto 1 do
        if isValidWebSiteChar(aDERStr[i])
        then Result := aDERStr[i] + Result
        else Break;

      for i := p+1 to length(aDERStr) do
        if isValidWebSiteChar(aDERStr[i])
        then Result := Result + aDERStr[i]
        else Break;

      // Remove '.' char from the beginning or end :
      if Result <> '' then
        while Result[1] = '.' do
        begin
          Delete(Result, 1, 1);
          if Result = '' then Break;
        end;

      if Result <> '' then
        while Result[length(Result)] = '.' do
        begin
          Delete(Result, length(Result), 1);
          if Result = '' then Break;
        end;

      if not isValidWebsite(Result) then
        Result := '';
    end;

    if not isValidwebSite(Result) then
      Result := '';

    // Try searching forward :
    if Result = '' then
      Delete(aDERStr, 1, p);
  end;
end;

function DERExtractWebMail(aDERStr: DERString): String;
var p, i: Integer;
begin
  Result := '';

  // The better strategy is to detect '@' char :
  p := pos('@', aDERStr);

  if (p > 1) and (p < length(aDERStr)) then
  begin
    // Get '@' + prefix :
    for i := p downto 1 do
      if isValidWebMailChar(aDERStr[i])
      then Result := aDERStr[i] + Result
      else Break;

    // Get sufix :
    for i := p+1 to length(aDERStr) do
      if isValidWebMailChar(aDERStr[i])
      then Result := Result + aDERStr[i]
      else Break;

    // Remove '.' char from the beginning or end :
    if Result <> '' then
      while (Result)[1] = '.' do
      begin
        Delete(Result, 1, 1);
        if Result = '' then Break;
      end;

    if Result <> '' then
      while Result[length(Result)] = '.' do
      begin
        Delete(Result, length(Result), 1);
        if Result = '' then Break;
      end;

    if not isValidWebMail(Result) then
      Result := '';
  end;
end;

// Retrieve numbers and '+' cars :
function DERExtractPhoneNr(aDERStr: DERString): String;
var i: Integer;
begin
  Result := '';

  if pos('+', aDERStr) <> 0 then
  begin
    aDERStr := copy(aDERStr, pos('+', aDERStr)+1, length(aDERStr));
    Result := '+';
  end
  else
    while aDERStr <> '' do
      if (DERToDERNCharset(aDERStr[1])+' ')[1] in ['0'..'9']
      then Break
      else Delete(aDERStr, 1, 1);

  for i := 1 to Length(aDERStr) do
    if (DERToDERNCharset(aDERStr[i])+' ')[1] in ['0'..'9'] then
      Result := Result + DERToDERNCharset(aDERStr[i]);
end;

function DERExecute(aDERStr: DERString; SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType; overload;
var RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
begin
  Result := DERExecute(aDERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail,
                                RsltMoney, RsltDate, SmartNumbersRecognition, SmartWebsiteRecognition);
end;

// Recognize from DERString any numbers/integer/float/money/percentage/date/website and webmail:
// 23.526,59 float/money handling ok
// 23 526,59 float/money handling ok
// 25-02-589 returns as numbers (2502589) and not as Integer (-589)
function DERExecute(aDERStr: DERString; var RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
                     SmartNumbersRecognition, SmartWebsiteRecognition: Boolean): TElementsType;

                  // Clear bounds from non numbers cars:
                  function DERExtractDate(aDate: String): String;    // Exemple:  '.15.01.1977.'
                  var i: Integer;
                  begin
                    Result := aDate;

                    while not ((Result + '0')[1] in ['0'..'9']) do
                      Delete(Result, 1, 1);

                    for i := length(aDate) downto 1 do
                      if Result[i] in ['0'..'9']
                      then Break
                      else Delete(Result, i, 1);
                  end;

                  function IsNumbers(aDERNStr: DERNString): Boolean;
                  var i: Integer;
                  begin
                    Result := true;

                    for i := 1 to length(aDERNStr) do
                      if aDERNStr[i] in ['a'..'z'] then
                      begin
                        Result := false;
                        Break;
                      end;
                  end;

var
  aDERNStr: DERNString;
  _DateSeparator, DateFormat: String;
  i, NumbersCount, AlphaCount: Integer;
  HandledCar, ReadInteger, ReadFloat, ReadMoney, ReadPercentage, ReadDate: Boolean;
  FloatValue: Extended;
begin
  if SmartNumbersRecognition
  then aDERNStr := DERToDERNCharset(aDERStr)
  else aDERNStr := aDERStr;

  NumbersCount := 0;
  AlphaCount := 0;

  for i := 1 to length(aDERNStr) do
  begin
    if aDERNStr[i] in ['0'..'9'] then
      inc(NumbersCount)
    else
      if aDERNStr[i] in ['a'..'z', 'A'..'Z'] then
        inc(AlphaCount);
  end;

  _DateSeparator := '';

  RsltWebMail := DERExtractWebMail(aDERStr);
  RsltwebSite := DERExtractwebSite(aDERStr, SmartWebsiteRecognition);
  RsltNumbers := '';
  RsltInteger := '';
  RsltFloat := '';
  RsltMoney := '';
  RsltPercentage := '';
  RsltDate := '';

  ReadInteger := true;
  ReadFloat := true;
  ReadMoney := true;
  ReadPercentage := true;
  ReadDate := true;

  // Read string from the end in order to detect decimal separator :
  for i := length(aDERNStr) downto 1 do
  begin
    HandledCar := false;

    if IsDERMoneyChar(aDERNStr[i]) then
    begin
      HandledCar := True;

      if ReadMoney then
      begin
        RsltMoney := aDERNStr[i];
        ReadMoney := false;

        // The value can be before or after money char ...
      end;
    end
    else
      if aDERNStr[i] = '%' then
      begin
        HandledCar := True;

        if ReadPercentage then
        begin
          RsltPercentage := aDERNStr[i];
          ReadPercentage := false;

          // Detecting percentage values is more important than integer or float values :
          ReadInteger := true;
          RsltInteger := '';

          ReadFloat := true;
          RsltFloat := '';
        end;
      end
      else
        if (aDERNStr[i] = '-') or (aDERNStr[i] = '_') then
        begin
          HandledCar := True;

          if ReadInteger and (RsltInteger <> '') then
          begin
            RsltInteger := '-' + RsltInteger;
            ReadInteger := false;
          end;

          if ReadFloat and (RsltFloat <> '') then
          begin
            RsltFloat := aDERNStr[i] + RsltFloat;
            ReadFloat := false;
          end;

          if ReadDate and (RsltDate <> '') then
            if (_DateSeparator = '') or (_DateSeparator = '-') then
            begin
              _DateSeparator := '-';
              RsltDate := '-' + RsltDate;
            end;
        end
        else
          if (aDERNStr[i] = '.') or (aDERNStr[i] = ':') then  // ':' can be returned by OCR engine because of paper dirt
          begin
            HandledCar := True;

            if ReadInteger and (RsltInteger <> '') then
              ReadInteger := false;

            if ReadFloat and (RsltFloat <> '') then
              if Pos('.', RsltFloat) = 0 then
                RsltFloat := '.' + RsltFloat;

              if ReadDate and (RsltDate <> '') then
                if (_DateSeparator = '') or (_DateSeparator = '.') then
                begin
                  _DateSeparator := '.';
                  RsltDate := '.' + RsltDate;
                end;
          end
          else
            if aDERNStr[i] = '/' then
            begin
              HandledCar := True;

              if ReadDate and (RsltDate <> '') then
                if (_DateSeparator = '') or (_DateSeparator = '/') then
                begin
                  _DateSeparator := '/';
                  RsltDate := '/' + RsltDate;
                end;
            end
            else
              if aDERNStr[i] in ['0'..'9'] then
              begin
                HandledCar := True;

                RsltNumbers := aDERNStr[i] + RsltNumbers;

                if ReadDate then
                  RsltDate := aDERNStr[i] + RsltDate;

                if ReadInteger then
                  RsltInteger := aDERNStr[i] + RsltInteger;

                if ReadFloat then
                  RsltFloat := aDERNStr[i] + RsltFloat;
              end;

    if not HandledCar then
    begin
      if ReadInteger and (RsltInteger <> '') then
        ReadInteger := false;

      if ReadFloat and (RsltFloat <> '') then
        ReadFloat := false;

      if ReadDate and (_DateSeparator <> '') then
        ReadDate := false;
    end;
  end;

  // Validate date:
  if RsltDate <> '' then
  begin
    // Remove extra chars from RsltDate:
    RsltDate := DERExtractDate(RsltDate);

    if ValidateDate(RsltDate, DateFormat)
    then RsltDate := intToStr( Trunc(DERStrToDate(RsltDate, DateFormat)) )
    else RsltDate := '';
  end;

  // Validate float:
  if RsltFloat <> '' then
  begin
    // Apply system decimal separator :
    if LocalFormatSettings.DecimalSeparator <> DERDecimalSeparator then
      RsltFloat := StringReplace(RsltFloat, DERDecimalSeparator, LocalFormatSettings.DecimalSeparator, []);

    if not TryStrToFloat(RsltFloat, FloatValue) then
      RsltFloat := '';
  end;

  // Validate money:
  if RsltMoney <> '' then   // RsltMoney = '%'
    if RsltFloat = ''
    then RsltMoney := ''
    else RsltMoney := RsltFloat + RsltMoney;

  // Validate percentage:
  if RsltPercentage <> '' then   // RsltPercentage = '%'
    if RsltFloat = ''
    then RsltPercentage := ''
    else RsltPercentage := RsltFloat + RsltPercentage;


  Result := etText;  // By default




  if RsltDate <> ''
  then
    Result := etDate
  else
    if RsltMoney <> ''
    then
      Result := etMoney
    else
      if (RsltWebsite <> '') and (length(RsltWebsite) > length(RsltFloat))  // "614.00" recognized as website ...
      then
        Result := etWebsite
      else
        if RsltWebMail <> ''
        then
          Result := etWebMail
        else
          if IsNumbers(aDERNStr) then            // Function that allows to recognize if expression is really a money/percentage/float/integer or numbers
            if RsltPercentage <> ''
            then
              Result := etPercentage
            else
              if (RsltFloat <> '') and (RsltFloat <> RsltInteger) and (length(RsltFloat) >= length(RsltNumbers))
              then
                Result := etFloat
              else
                if (RsltInteger <> '') and (length(RsltInteger) >= length(RsltNumbers))
                then
                  Result := etInteger
                else
                  if (RsltNumbers <> '') and (AlphaCount = 0)
                  then
                    Result := etNumbers;
end;

function RetrieveElementValue(aStr: String; SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; aElementsType: TElementsType): String;
var
  DERStr: DERString;
  RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
begin
  Result := '';
  DERStr := StringToDERCharSet(aStr);
  DERExecute(DERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail,
               RsltMoney, RsltDate, SmartNumbersRecognition, SmartWebsiteRecognition);

  case aElementsType of
    //etText
    //etExpressionKeyWord,
    etNumbers:    Result := RsltNumbers;
    etInteger:    Result := RsltInteger;
    etFloat:      Result := RsltFloat;
    etPercentage: Result := RsltPercentage;
    etwebSite:    Result := RsltWebsite;
    etWebMail:    Result := RsltWebMail;
    etMoney:      Result := RsltMoney;
    etDate:       Result := RsltDate;
    else
                  Result := aStr;
  end;
end;

procedure RetrieveElementValue(aStr: String; SmartNumbersRecognition, SmartWebsiteRecognition: Boolean; var RsltDERStr: DERString; var RsltElementType: TElementsType);
var
  RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail, RsltMoney, RsltDate: String;
begin
  RsltDERStr := '';
  RsltDERStr := StringToDERCharSet(aStr);
  RsltElementType := DERExecute(RsltDERStr, RsltNumbers, RsltInteger, RsltFloat, RsltPercentage, RsltwebSite, RsltWebMail,
                              RsltMoney, RsltDate, SmartNumbersRecognition, SmartWebsiteRecognition);

  case RsltElementType of
    //etText
    //etExpressionKeyWord,
    etNumbers:    RsltDERStr := RsltNumbers;
    etInteger:    RsltDERStr := RsltInteger;
    etFloat:      RsltDERStr := RsltFloat;
    etPercentage: RsltDERStr := RsltPercentage;
    etwebSite:    RsltDERStr := RsltWebsite;
    etWebMail:    RsltDERStr := RsltWebMail;
    etMoney:      RsltDERStr := RsltMoney;
    etDate:       RsltDERStr := RsltDate;
    else
                  RsltDERStr := aStr;
  end;
end;

initialization

GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, LocalFormatSettings);

end.
