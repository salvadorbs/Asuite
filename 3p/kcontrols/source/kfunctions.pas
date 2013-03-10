{ @abstract(This unit contains miscellaneous supporting functions)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(20 Oct 2001)
  @lastmod(20 Jun 2010)

  Copyright © 2001 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KFunctions;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
 // use the LCL interface support whenever possible
 {$IFDEF USE_WINAPI}
  Windows,
 {$ENDIF}
  LCLType, LCLIntf, LMessages, LCLProc, LCLVersion,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Controls, ComCtrls, Graphics;

const
{$IFNDEF FPC}
  { @exclude }
  KM_MOUSELEAVE = WM_MOUSELEAVE;
  { @exclude }
  LM_USER = WM_USER;
  { @exclude }
  LM_CANCELMODE = WM_CANCELMODE;
  { @exclude }
  LM_CHAR = WM_CHAR;
  { @exclude }
  LM_DROPFILES = WM_DROPFILES;
  { @exclude }
  LM_ERASEBKGND = WM_ERASEBKGND;
  { @exclude }
  LM_GETDLGCODE = WM_GETDLGCODE;
  { @exclude }
  LM_HSCROLL = WM_HSCROLL;
  { @exclude }
  LM_KEYDOWN = WM_KEYDOWN;
  { @exclude }
  LM_KILLFOCUS = WM_KILLFOCUS;
  { @exclude }
  LM_LBUTTONDOWN = WM_LBUTTONDOWN;
  { @exclude }
  LM_LBUTTONUP = WM_LBUTTONUP;
  { @exclude }
  LM_MOUSEMOVE = WM_MOUSEMOVE;
  { @exclude }
  LM_SETFOCUS = WM_SETFOCUS;
  { @exclude }
  LM_SIZE = WM_SIZE;
  { @exclude }
  LM_VSCROLL = WM_VSCROLL;
  { @exclude }
  LCL_MAJOR = 0;
  { @exclude }
  LCL_MINOR = 0;
  { @exclude }
  LCL_RELEASE = 0;

{$ELSE}
  // hope this is correct about WM_MOUSELEAVE otherwise adapt it as you wish
 {$IFDEF LCLWin32}
  {$IF ((LCL_MAJOR=0) AND (LCL_MINOR=9) AND (LCL_RELEASE<27))}
  { @exclude }
  KM_MOUSELEAVE = LM_LEAVE; // LCL 0.9.26.2-
  {$ELSE}
  { @exclude }
  KM_MOUSELEAVE = LM_MOUSELEAVE; // LCL 0.9.27+
  {$IFEND}
 {$ELSE}
  {$IFDEF LCLWinCE}
  { @exclude }
  KM_MOUSELEAVE = LM_LEAVE;
  {$ELSE}
  { @exclude }
  KM_MOUSELEAVE = LM_MOUSELEAVE;
  {$ENDIF}
 {$ENDIF}
 { @exclude }
 //WM_CTLCOLORBTN = Messages.WM_CTLCOLORBTN;
 { @exclude }
 //WM_CTLCOLORSTATIC = Messages.WM_CTLCOLORSTATIC;
{$ENDIF}

{$IFDEF USE_WINAPI}
  { @exclude }
  SHFolderDll = 'SHFolder.dll';
{$ENDIF}

  { Base for custom messages used by KControls suite. }
  KM_BASE = LM_USER + 1024;

  { Custom message. }
  KM_LATEUPDATE = KM_BASE + 1;

  { Constant for horizontal resize cursor. }
  crHResize = TCursor(101);
  { Constant for vertical resize cursor. }
  crVResize = TCursor(102);
  { Constant for uncaptured dragging cursor. }
  crDragHandFree = TCursor(103);
  { Constant for captured dragging cursor. }
  crDragHandGrip = TCursor(104);

  { Checkbox frame size in logical screen units. }
  cCheckBoxFrameSize = 13;

  { Set of word break characters. }
  cWordBreaks = [#0, #9,  #32];
  { Set of line break characters. }
  cLineBreaks = [#10, #13];
  { Carriage return character. }
  cCR = #10;
  { Line feed character. }
  cLF = #13;
  { Text ellipsis string. }
  cEllipsis = '...';

type
{$IFNDEF FPC}
  { @exclude }
  TLMessage = TMessage;
  { @exclude }
  TLMMouse = TWMMouse;
  { @exclude }
  TLMNoParams = TWMNoParams;
  { @exclude }
  TLMKey = TWMKey;
  { @exclude }
  TLMChar = TWMChar;
  { @exclude }
  TLMEraseBkGnd = TWMEraseBkGnd;
  { @exclude }
  TLMHScroll = TWMHScroll;
  { @exclude }
  TLMKillFocus = TWMKillFocus;
  { @exclude }
  TLMSetFocus = TWMSetFocus;
  { @exclude }
  TLMSize = TWMSize;
  { @exclude }
  TLMVScroll = TWMVScroll;
{$ENDIF}

  //PInteger = ^Integer; defined by System.pas
  { Static array for Integer. }
  TIntegers = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;
  { Pointer for TIntegers. }
  PIntegers = ^TIntegers;
  { Dynamic array for Integer. }
  TDynIntegers = array of Integer;

  //PCardinal = ^Cardinal; defined by System.pas
  { Static array for Cardinal. }
  TCardinals = array[0..MaxInt div SizeOf(Cardinal) - 1] of Cardinal;
  { Pointer for TCardinals. }
  PCardinals = ^TCardinals;
  { Dynamic array for Cardinal. }
  TDynCardinals = array of Cardinal;

  //PShortInt = ^ShortInt; defined by System.pas
  { Static array for ShortInt. }
  TShortInts = array[0..MaxInt div SizeOf(ShortInt) - 1] of ShortInt;
  { Pointer for TShortInts. }
  PShortInts = ^TShortInts;
  { Dynamic array for ShortInt. }
  TDynShortInts = array of ShortInt;

  //PSmallInt = ^SmallInt; defined by System.pas
  { Static array for SmallInt. }
  TSmallInts = array[0..MaxInt div SizeOf(SmallInt) - 1] of SmallInt;
  { Pointer for TSmallInts. }
  PSmallInts = ^TSmallInts;
  { Dynamic array for SmallInt. }
  TDynSmallInts = array of SmallInt;

  //PLongInt = ^LongInt; defined by System.pas
  { Static array for LongInt. }
  TLongInts = array[0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  { Pointer for TLongInts. }
  PLongInts = ^TLongInts;
  { Dynamic array for LongInt. }
  TDynLongInts = array of LongInt;

  //PInt64 = ^Int64; defined by System.pas
  { Static array for Int64. }
  TInt64s = array[0..MaxInt div SizeOf(Int64) - 1] of Int64;
  { Pointer for TInt64s. }
  PInt64s = ^TInt64s;
  { Dynamic array for Int64. }
  TDynInt64s = array of Int64;

  //PByte = ^Byte; defined by System.pas
  { Static array for Byte. }
  TBytes = array[0..MaxInt div SizeOf(Byte) - 1] of Byte;
  { Pointer for TBytes. }
  PBytes = ^TBytes;
  { Dynamic array for Byte. }
  TDynBytes = array of Byte;

  //PWord = ^Word; defined by System.pas
  { Static array for Word. }
  TWords = array[0..MaxInt div SizeOf(Word) - 1] of Word;
  { Pointer for TWords. }
  PWords = ^TWords;
  { Dynamic array for Word. }
  TDynWords = array of Word;

  //PLongWord = ^LongWord; defined by System.pas
  { Static array for LongWord. }
  TLongWords = array[0..MaxInt div SizeOf(LongWord) - 1] of LongWord;
  { Pointer for TLongWords. }
  PLongWords = ^TLongWords;
  { Dynamic array for LongWord. }
  TDynLongWords = array of LongWord;

{$IFDEF COMPILER10_UP}
  { Static array for UInt64. }
  TUInt64s = array[0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  { Pointer for TUInt64s. }
  PUInt64s = ^TUInt64s;
  { Dynamic array for UInt64. }
  TDynUInt64s = array of UInt64;
{$ENDIF}

  //PSingle = ^Single; defined by System.pas
  { Static array for Single. }
  TSingles = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  { Pointer for TSingles. }
  PSingles = ^TSingles;
  { Dynamic array for Single. }
  TDynSingles = array of Single;

  //PDouble = ^Double; defined by System.pas
  { Static array for Double. }
  TDoubles = array[0..MaxInt div SizeOf(Double) - 1] of Double;
  { Pointer for TDoubles. }
  PDoubles = ^TDoubles;
  { Dynamic array for Double. }
  TDynDoubles = array of Double;

{$IFNDEF FPC}
  //PExtended = ^Extended; defined by System.pas
  { Static array for Extended. }
  TExtendeds = array[0..MaxInt div SizeOf(Extended) - 1] of Extended;
  { Pointer for TExtendeds. }
  PExtendeds = ^TExtendeds;
  { Dynamic array for Extended. }
  TDynExtendeds = array of Extended;
{$ENDIF}

  //PChar is special type
  { Static array for Char. }
  TChars = array[0..MaxInt div SizeOf(Char) - 1] of Char;
  { Pointer for TChars. }
  PChars = ^TChars;
  { Dynamic array for Char. }
  TDynChars = array of Char;

  { Useful structure to handle general data and size as a single item }
  TDataSize = record
    Data: Pointer;
    Size: Integer;
  end;
  { Pointer for TDataSize }
  PDataSize = ^TDataSize;

  { Set type for @link(CharInSetEx). }
  TKSysCharSet = set of AnsiChar;

  { Defines a currency format settings for @link(FormatCurrency). }
  TKCurrencyFormat = record
    CurrencyFormat,
    CurrencyDecimals: Byte;
    CurrencyString: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
    DecimalSep: Char;
    ThousandSep: Char;
    UseThousandSep: Boolean;
  end;

{ Replaces possible decimal separators in S with DecimalSeparator variable.}
function AdjustDecimalSeparator(const S: string): string;

{$IFNDEF FPC}
{ Converts an AnsiString into a PWideChar string. If CodePage is not set
  the current system code page for ANSI-UTFx translations will be used. }
function AnsiStringToWideChar(const Text: AnsiString; CodePage: Cardinal = CP_ACP): PWideChar;
{$ENDIF}

{ Under Windows this function calls the WinAPI TrackMouseEvent. Under other OSes
  the implementation is still missing. }
procedure CallTrackMouseEvent(Control: TWinControl; var Status: Boolean);

{ Compiler independent Delphi2009-like CharInSet function for ANSI characters. }
function CharInSetEx(AChar: AnsiChar; const ASet: TKSysCharSet): Boolean; overload;

{ Compiler independent Delphi2009-like CharInSet function for Unicode characters. }
function CharInSetEx(AChar: WideChar; const ASet: TKSysCharSet): Boolean; overload;

{ Compares two Integers. Returns 1 if I1 > I2, -1 if I1 < I2 and 0 if I1 = I2. }
function CompareIntegers(I1, I2: Integer): Integer;

{ Compares two PWideChar strings. Returns 1 if W1 > W2, -1 if W1 < W2 and
  0 if W1 = W2. The strings will be compared using the default user locale
  unless another locale has been specified in Locale. }
function CompareWideChars(W1, W2: PWideChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;

{$IFDEF STRING_IS_UNICODE}
{ Compares two Unicode strings (Lazarus, Delphi 2009 and better). Returns 1 if S1 > S2,
  -1 if S1 < S2 and 0 if S1 = S2. The strings will be compared using the default
  user locale unless another locale has been specified in Locale. }
function CompareChars(S1, S2: PChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;
{$ENDIF}

{ Compares two WideString strings. Returns 1 if W1 > W2, -1 if W1 < W2 and
  0 if W1 = W2. The strings will be compared using the default user locale
  unless another locale has been specified in Locale. }
function CompareWideStrings(W1, W2: WideString{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;

{$IFDEF STRING_IS_UNICODE}
{ Compares two Unicode strings (Lazarus, Delphi 2009 and better). Returns 1 if S1 > S2,
  -1 if S1 < S2 and 0 if S1 = S2. The strings will be compared using the default
  user locale unless another locale has been specified in Locale. }
function CompareStrings(S1, S2: string{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;
{$ENDIF}

{ Performs integer division. If there is a nonzero remainder,
  the result will be incremented. }
function DivUp(Dividend, Divisor: Integer): Integer;

{ Performs integer division. If there is a nonzero remainder,
  the result will be decremented. }
function DivDown(Dividend, Divisor: Integer): Integer;

procedure EnableControls(AParent: TWinControl; AEnabled, ARecursive: Boolean);

{ Raises a general exception with associated message Msg. }
procedure Error(const Msg: string);

{ Swaps values of two SmallInt variables. }
procedure Exchange(var Value1, Value2: SmallInt); overload;
{ Swaps values of two ShortInt variables. }
procedure Exchange(var Value1, Value2: ShortInt); overload;
{ Swaps values of two Integer variables. }
procedure Exchange(var Value1, Value2: Integer); overload;
{ Swaps values of two Int64 variables. }
procedure Exchange(var Value1, Value2: Int64); overload;
{ Swaps values of two Byte variables. }
procedure Exchange(var Value1, Value2: Byte); overload;
{ Swaps values of two Word variables. }
procedure Exchange(var Value1, Value2: Word); overload;
{ Swaps values of two Cardinal variables. }
procedure Exchange(var Value1, Value2: Cardinal); overload;
{$IFDEF COMPILER10_UP }
{ Swaps values of two UInt64 variables. }
procedure Exchange(var Value1, Value2: UInt64); overload;
{$ENDIF}
{ Swaps values of two Single variables. }
procedure Exchange(var Value1, Value2: Single); overload;
{ Swaps values of two Double variables. }
procedure Exchange(var Value1, Value2: Double); overload;
{$IFNDEF FPC}
{ Swaps values of two Extended variables. }
procedure Exchange(var Value1, Value2: Extended); overload;
{$ENDIF}
{ Swaps values of two Char variables. }
procedure Exchange(var Value1, Value2: Char); overload;

{ Fills the message record. }
function FillMessage(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): TLMessage;

{ Formats the given currency value with to specified parameters. Not thread safe. }
function FormatCurrency(Value: Currency; const AFormat: TKCurrencyFormat): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};

{ Returns the module version for given module. Works under WinX only. }
function GetAppVersion(const ALibName: string; var MajorVersion, MinorVersion, BuildNumber, RevisionNumber: Word): Boolean;

{ Returns the Text property of any TWinControl instance as WideString (up to Delphi 2007)
  or string (Delphi 2009, Lazarus). }
function GetControlText(Value: TWinControl): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};

{ Returns current status of Shift, Alt and Ctrl keys. }
function GetShiftState: TShiftState;

{ Converts an integer into binary string with custom alignment
  (given by Digits). }
function IntToAscii(Value: Int64; Digits: Integer): string;
{ Converts an integer into binary digit string with custom alignment
  (given by Digits) and suffix. }
function IntToBinStr(Value: Int64; Digits: Byte; const Suffix: string): string;
{ Converts an integer value into BCD number. }
function IntToBCD(Value: Cardinal): Cardinal;
{ Converts an integer into decimal digit string. Equals to IntToStr. }
function IntToDecStr(Value: Int64): string;
{ Converts an integer into hexadecimal digit string with custom alignment
  (given by Digits), prefix and suffix. Digits represented by alphabetical
  characters can be either in lower or upper case. }
function IntToHexStr(Value: Int64; Digits: Byte; const Prefix, Suffix: string;
  UseLowerCase: Boolean): string;
{ Converts an integer into octal digit string. }
function IntToOctStr(Value: Int64): string;

function IntPowerInt(Value: Int64; Exponent: Integer): Int64;

{ Converts a binary string into integer with custom alignment (given by Digits). }
function AsciiToInt(S: string; Digits: Integer): Int64;
{ Converts a BCD number into integer value. }
function BCDToInt(Value: Cardinal): Cardinal;
{ Converts a binary digit string into integer with custom alignment
  (given by Digits) and sign of a value represented by the string (given by Signed).
  Code returns either zero for a successful conversion or the position of
  first bad character. }
function BinStrToInt(S: string; Digits: Byte; Signed: Boolean;
  var Code: Integer): Int64;
{ Converts a decimal digit string into integer. Code returns either zero for
  a successful conversion or the position of first bad character. Equals to Val. }
function DecStrToInt(S: string; var Code: Integer): Int64;
{ Converts a hexadecimal digit string into integer with custom alignment
  (given by Digits) and sign of a value represented by the string (given by Signed).
  Code returns either zero for a successful conversion or the position of
  first bad character. }
function HexStrToInt(S: string; Digits: Byte; Signed: Boolean;
  var Code: Integer): Int64;
{ Converts an octal digit string into integer. Code returns either zero for
  a successful conversion or the position of first bad character. }
function OctStrToInt(S: string; var Code: Integer): Int64;

{ Returns a clipped ShortInt value so that it lies between Min and Max }
function MinMax(Value, Min, Max: ShortInt): ShortInt; overload;
{ Returns a clipped SmallInt value so that it lies between Min and Max }
function MinMax(Value, Min, Max: SmallInt): SmallInt; overload;
{ Returns a clipped Integer value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Integer): Integer; overload;
{ Returns a clipped Int64 value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Int64): Int64; overload;
{ Returns a clipped Single value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Single): Single; overload;
{ Returns a clipped Double value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Double): Double; overload;
{$IFNDEF FPC}
{ Returns a clipped Extended value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Extended): Extended; overload;
{$ENDIF}

{ Under Windows this function calls the WinAPI SetWindowRgn. Under other OSes
  the implementation is still missing. }
procedure SetControlClipRect(AControl: TWinControl; const ARect: TRect);

{ Modifies the Text property of any TWinControl instance. The value is given as
  WideString (up to Delphi 2007) or string (Delphi 2009, Lazarus). }
procedure SetControlText(Value: TWinControl; const Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});

{ Returns next character index for given null terminated string and character index.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StrNextCharIndex(AText: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}; Index: Integer): Integer;

{ Returns the index for given string where character at given index begins.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StringCharBegin(const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; Index: Integer): Integer;

{ Returns the number of characters in a string. Under Delphi it equals Length,
  under Lazarus it equals UTF8Length. }
function StringLength(const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}): Integer;

{ Returns next character index for given string and character index.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StringNextCharIndex(const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; Index: Integer): Integer;

{ Trims characters specified by ASet from the beginning and end of AText.
  New text length is returned by ALen. }
procedure TrimWhiteSpaces(var AText: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}; var ALen: Integer; const ASet: TKSysCharSet); overload;

{ Trims characters specified by ASet from the beginning and end of AText. }
procedure TrimWhiteSpaces(var AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; const ASet: TKSysCharSet); overload;

{$IFNDEF FPC}
{ Converts a PWideChar string into AnsiString. If CodePage is not set
  the current system code page for ANSI-UTFx translations will be used. }
function WideCharToAnsiString(Text: PWideChar; CodePage: Cardinal = CP_ACP): AnsiString;
{$ENDIF}

{$IFDEF USE_WINAPI}
function GetWindowsFolder(CSIDL: Cardinal; var APath: string): Boolean;
{$ENDIF}

implementation

uses
  Forms, Math, SysUtils, TypInfo
{$IFDEF USE_WINAPI}
  , ShlObj
{$ENDIF}
{$IFDEF USE_WIDEWINPROCS}
  , KWideWinProcs
{$ENDIF}
;

function AdjustDecimalSeparator(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if CharInSetEx(Result[I], [',', '.']) then
      Result[I] := DecimalSeparator;
end;

{$IFNDEF FPC}
function AnsiStringToWideChar(const Text: AnsiString; CodePage: Cardinal): PWideChar;
var
  Len: Integer;
begin
  Len := MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, nil, 0);
  GetMem(Result, Len shl 1);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, Result, Len);
end;
{$ENDIF}

procedure CallTrackMouseEvent(Control: TWinControl; var Status: Boolean);
{$IFDEF USE_WINAPI}
var
  TE: TTrackMouseEvent;
begin
  if not Status then
  begin
    TE.cbSize := SizeOf(TE);
    TE.dwFlags := TME_LEAVE;
    TE.hwndTrack := Control.Handle;
    TE.dwHoverTime := HOVER_DEFAULT;
    TrackMouseEvent(TE);
    Status := True;
  end;
end;
{$ELSE}
begin
  // This is a TODO for Lazarus team.
end;
{$ENDIF}

function CharInSetEx(AChar: AnsiChar; const ASet: TKSysCharSet): Boolean;
begin
  Result := AChar in ASet;
end;

function CharInSetEx(AChar: WideChar; const ASet: TKSysCharSet): Boolean;
begin
  Result := (Ord(AChar) < $100) and
  {$IFDEF COMPILER12_UP}
    CharInSet(AChar, ASet);
  {$ELSE}
    (AnsiChar(AChar) in ASet);
  {$ENDIF}
end;

function CompareIntegers(I1, I2: Integer): Integer;
begin
  if I1 > I2 then Result := 1
  else if I1 < I2 then Result := -1
  else Result := 0;
end;

function CompareWideChars(W1, W2: PWideChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
  if (W1 = nil) or (W2 = nil) then
  begin
    if W1 <> nil then Result := 1
    else if W2 <> nil then Result := -1
    else Result := 0;
  end else
  begin
  {$IFDEF USE_WIDEWINPROCS}
    Result := WideWinProcs.CompareString(Locale, 0, W1, -1, W2, -1);
    Dec(Result, 2);
  {$ELSE}
    Result := WideCompareStr(WideString(W1), WideString(W2));
  {$ENDIF}
  end;
end;

{$IFDEF STRING_IS_UNICODE}
function CompareChars(S1, S2: PChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
  if (S1 = nil) or (S2 = nil) then
  begin
    if S1 <> nil then Result := 1
    else if S2 <> nil then Result := -1
    else Result := 0;
  end else
  begin
  {$IFDEF USE_WIDEWINPROCS}
    Result := WideWinProcs.CompareString(Locale, 0, PWideChar(S1), -1, PWideChar(S2), -1);
    Dec(Result, 2);
  {$ELSE}
    Result := CompareStr(string(S1), string(S2));
  {$ENDIF}
  end;
end;
{$ENDIF}

function CompareWideStrings(W1, W2: WideString{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
{$IFDEF USE_WIDEWINPROCS}
  Result := WideWinProcs.CompareString(Locale, 0, PWideChar(W1), -1, PWideChar(W2), -1);
  Dec(Result, 2);
{$ELSE}
  Result := WideCompareStr(W1, W2);
{$ENDIF}
end;

{$IFDEF STRING_IS_UNICODE}
function CompareStrings(S1, S2: string{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
{$IFDEF USE_WIDEWINPROCS}
  Result := WideWinProcs.CompareString(Locale, 0, PWideChar(S1), -1, PWideChar(S2), -1);
  Dec(Result, 2);
{$ELSE}
  Result := CompareStr(S1, S2);
{$ENDIF}
end;
{$ENDIF}

function DivUp(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor > 0 then
    Result := Dividend div Divisor + 1
  else
    Result := Dividend div Divisor;
end;

function DivDown(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor < 0 then
    Result := Dividend div Divisor - 1
  else
    Result := Dividend div Divisor;
end;

procedure EnableControls(AParent: TWinControl; AEnabled, ARecursive: Boolean);

  procedure DoEnable(AParent: TWinControl);
  var
    I: Integer;
  begin
    if AParent <> nil then
      for I := 0 to AParent.ControlCount - 1 do
      begin
        AParent.Controls[I].Enabled := AEnabled;
        if ARecursive and (AParent.Controls[I] is TWinControl) then
          DoEnable(TWinControl(AParent.Controls[I]));
      end;
  end;

begin
  DoEnable(AParent);
end;

procedure Exchange(var Value1, Value2: ShortInt);
var
  Tmp: ShortInt;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: SmallInt);
var
  Tmp: SmallInt;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Int64);
var
  Tmp: Int64;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Byte);
var
  Tmp: Byte;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Word);
var
  Tmp: Word;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Cardinal);
var
  Tmp: Cardinal;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

{$IFDEF COMPILER10_UP }
procedure Exchange(var Value1, Value2: UINT64);
var
  Tmp: UINT64;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;
{$ENDIF}

procedure Exchange(var Value1, Value2: Single);
var
  Tmp: Single;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Double);
var
  Tmp: Double;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

{$IFNDEF FPC}
procedure Exchange(var Value1, Value2: Extended);
var
  Tmp: Extended;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;
{$ENDIF}

procedure Exchange(var Value1, Value2: Char);
var
  Tmp: Char;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function FillMessage(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): TLMessage;
begin
  Result.Msg := Msg;
  Result.LParam := LParam;
  Result.WParam := WParam;
  Result.Result := 0;
end;

function FormatCurrency(Value: Currency; const AFormat: TKCurrencyFormat): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
var
  OldDecimalSep, OldThousandSep: Char;
  Fmt: string;
begin
  OldThousandSep := ThousandSeparator;
  if AFormat.UseThousandSep then
  begin
    ThousandSeparator := AFormat.ThousandSep;
    Fmt := '%.*n';
  end else
    Fmt := '%.*f';
  OldDecimalSep := DecimalSeparator;
  DecimalSeparator := AFormat.DecimalSep;
  try
    case AFormat.CurrencyFormat of
      0: Result := {$IFDEF STRING_IS_UNICODE}Format{$ELSE}WideFormat{$ENDIF}(
        '%s' + Fmt, [AFormat.CurrencyString, AFormat.CurrencyDecimals, Value]);
      1: Result := {$IFDEF STRING_IS_UNICODE}Format{$ELSE}WideFormat{$ENDIF}(
        Fmt + '%s', [AFormat.CurrencyDecimals, Value, AFormat.CurrencyString]);
      2: Result := {$IFDEF STRING_IS_UNICODE}Format{$ELSE}WideFormat{$ENDIF}(
        '%s ' + Fmt, [AFormat.CurrencyString, AFormat.CurrencyDecimals, Value]);
    else
      Result := {$IFDEF STRING_IS_UNICODE}Format{$ELSE}WideFormat{$ENDIF}(
        Fmt + ' %s', [AFormat.CurrencyDecimals, Value, AFormat.CurrencyString]);
    end;
  finally
    DecimalSeparator := OldDecimalSep;
    if AFormat.UseThousandSep then
      ThousandSeparator := OldThousandSep;
  end;
end;

function GetAppVersion(const ALibName: string; var MajorVersion, MinorVersion, BuildNumber, RevisionNumber: Word): Boolean;
{$IFDEF USE_WINAPI}
var
 dwHandle, dwLen: DWORD;
 BufLen: Cardinal;
 lpData: LPTSTR;
 pFileInfo: ^VS_FIXEDFILEINFO;
{$ENDIF}
begin
  Result := False;
{$IFDEF USE_WINAPI}
  dwLen := GetFileVersionInfoSize(PChar(ALibName), dwHandle);
  if dwLen <> 0 then
  begin
    GetMem(lpData, dwLen);
    try
      if GetFileVersionInfo(PChar(ALibName), dwHandle, dwLen, lpData) then
      begin
        if VerQueryValue(lpData, '\\', Pointer(pFileInfo), BufLen) then
        begin
          MajorVersion := HIWORD(pFileInfo.dwFileVersionMS);
          MinorVersion := LOWORD(pFileInfo.dwFileVersionMS);
          BuildNumber := HIWORD(pFileInfo.dwFileVersionLS);
          RevisionNumber := LOWORD(pFileInfo.dwFileVersionLS);
          Result := True;
        end;
      end;
    finally
      FreeMem(lpData);
    end;
  end;
{$ENDIF}
end;

function GetControlText(Value: TWinControl): {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};

  function GetTextBuffer(Value: TWinControl): string;
  begin
    SetLength(Result, Value.GetTextLen);
    Value.GetTextBuf(PChar(Result), Length(Result) + 1);
  end;

begin
{$IFDEF FPC}
  Result := GetTextBuffer(Value); // conversion from UTF8 forced anyway
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  Result := GetTextBuffer(Value);
 {$ELSE}
  if Value.HandleAllocated and (Win32Platform = VER_PLATFORM_WIN32_NT) then // unicode fully supported
  begin
    SetLength(Result, GetWindowTextLengthW(Value.Handle));
    GetWindowTextW(Value.Handle, PWideChar(Result), Length(Result) + 1);
  end else
    Result := GetTextBuffer(Value);
 {$ENDIF}
{$ENDIF}
end;

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function IntToAscii(Value: Int64; Digits: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := 0;
  while I < Digits do
  begin
    Result := Result + Chr(Value and $FF);
    Value := Value shr 8;
    Inc(I);
  end;
end;

function IntToBCD(Value: Cardinal): Cardinal;
var
  Exp: Cardinal;
begin
  Result := 0;
  Exp := 1;
  while (Value > 0) and (Exp > 0) do
  begin
    Result := Result + Value mod 10 * Exp;
    Value := Value div 10;
    Exp := Exp * 16;
  end;
end;

function IntToBinStr(Value: Int64; Digits: Byte; const Suffix: string): string;
var
  B: Byte;
  C: Char;
begin
  Result := '';
  if Digits <> 0 then
    Digits := MinMax(Digits, 1, 64);
  repeat
    B := Byte(Value and $1);
    Value := Value shr 1;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until (Value = 0) or ((Digits <> 0) and (Length(Result) = Digits));
  while Length(Result) < Digits do
    Result := '0' + Result;
  Result := Result + Suffix;
end;

function IntToDecStr(Value: Int64): string;
var
  B: Byte;
  C: Char;
  Signum: Boolean;
begin
  if Value < 0 then
  begin
    Signum := True;
    Value := -Value;
  end else
    Signum := False;
  Result := '';
  repeat
    B := Byte(Value mod 10);
    Value := Value div 10;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  Result := '0' + Result;
  if Signum then
    Result := '-' + Result;
end;

function IntToHexStr(Value: Int64; Digits: Byte; const Prefix, Suffix: string; UseLowerCase: Boolean): string;
var
  B: Byte;
  C: Char;
begin
  Result := '';
  if Digits <> 0 then
    Digits := MinMax(Digits, 1, 16);
  repeat
    B := Byte(Value and $F);
    Value := Value shr 4;
    if B < 10 then
      C := Chr(Ord('0') + B) else
      if UseLowerCase then
        C := Chr(Ord('a') + B - 10)
      else
        C := Chr(Ord('A') + B - 10);
    Result := C + Result;
  until (Value = 0) or ((Digits <> 0) and (Length(Result) = Digits));
  while Length(Result) < Digits do
    Result := '0' + Result;
  Result := Prefix + Result + Suffix;
end;

function IntToOctStr(Value: Int64): string;
var
  B: Byte;
  C: Char;
  Signum: Boolean;
begin
  if Value < 0 then
  begin
    Signum := True;
    Value := -Value;
  end else
    Signum := False;
  Result := '';
  repeat
    B := Byte(Value mod 8);
    Value := Value div 8;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  Result := '0' + Result;
  if Signum then
    Result := '-' + Result;
end;

function IntPowerInt(Value: Int64; Exponent: Integer): Int64;
begin
  Result := Value;
  while Exponent > 1 do
  begin
    Result := Result * Value;
    Dec(Exponent);
  end;
end;

function AsciiToInt(S: string; Digits: Integer): Int64;
var
  I: Integer;
begin
  Result := 0;
  I := Min(Length(S), Digits);
  while I > 0 do
  begin
    Result := Result shl 8;
    Result := Ord(S[I]) + Result;
    Dec(I);
  end;
end;

function BCDToInt(Value: Cardinal): Cardinal;
var
  Exp: Cardinal;
begin
  Result := 0;
  Exp := 1;
  while Value > 0 do
  begin
    Result := Result + Min(Value and 15, 9) * Exp;
    Value := Value shr 4;
    Exp := Exp * 10;
  end;
end;

function BinStrToInt(S: string; Digits: Byte; Signed: Boolean; var Code: Integer): Int64;
var
  I, L, Len: Integer;
  N: Byte;
  C: Char;
  M: Int64;
begin
  Result := 0;
  Code := 0;
  L := 0;
  Len := Length(S);
  if (Digits = 0) or (Digits > 64) then
    Digits := 64;
  if (Len >= 1) and CharInSetEx(S[Len], ['b', 'B']) then
  begin
    Delete(S, Len, 1);
    Dec(Len);
  end;
  I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '1') then N := Ord(C) - Ord('0');
    if N > 1 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      if L >= Digits then
      begin
        Code := I;
        Break;
      end;
      Result := Result shl 1;
      Inc(Result, N);
      Inc(L);
    end;
    Inc(I);
  end;
  if Signed and (Digits < 64) then
  begin
    M := Int64(1) shl Digits;
    if Result >= M shr 1 - 1 then
      Dec(Result, M);
  end;
end;

function DecStrToInt(S: string; var Code: Integer): Int64;
var
  I, Len: Integer;
  N: Byte;
  C: Char;
  Minus: Boolean;
begin
  Result := 0;
  Code := 0;
  Len := Length(S);
  Minus := S[1] = '-';
  if Minus then I := 2 else I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '9') then N := Ord(C) - Ord('0');
    if N > 9 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      Result := Result * 10;
      Inc(Result, N);
    end;
    Inc(I);
  end;
  if Minus then Result := -Result;
end;

function HexStrToInt(S: string; Digits: Byte; Signed: Boolean; var Code: Integer): Int64;
var
  I, L, Len: Integer;
  N: Byte;
  C: Char;
  M: Int64;
begin
  Result := 0;
  Code := 0;
  L := 0;
  Len := Length(S);
  if (Digits = 0) or (Digits > 16) then
    Digits := 16;
  if (Len >= 2) and (AnsiChar(S[1]) = '0') and CharInSetEx(S[2], ['x', 'X']) then
    I := 3
  else if (Len >= 1) and CharInSetEx(S[1], ['x', 'X', '$']) then
    I := 2
  else
    I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '9') then N := Ord(C) - Ord('0')
    else if (C >= 'a') and (C <= 'f') then N := Ord(C) - Ord('a') + 10
    else if (C >= 'A') and (C <= 'F') then N := Ord(C) - Ord('A') + 10;
    if N > 15 then
    begin
      if CharInSetEx(C, ['h', 'H']) then
      begin
        if Len > I then Code := I + 1;
      end else
        Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      if L >= Digits then
      begin
        Code := I;
        Break;
      end;
      Result := Result shl 4;
      Inc(Result, N);
      Inc(L);
    end;
    Inc(I);
  end;
  if Signed and (Digits < 16) then
  begin
    M := Int64(1) shl (Digits shl 2);
    if Result >= M shr 1 - 1 then
      Dec(Result, M);
  end;
end;

function OctStrToInt(S: string; var Code: Integer): Int64;
var
  I, Len: Integer;
  N: Byte;
  C: Char;
  Minus: Boolean;
begin
  Result := 0;
  Code := 0;
  Len := Length(S);
  Minus := S[1] = '-';
  if Minus then I := 2 else I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '7') then N := Ord(C) - Ord('0');
    if N > 7 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      Result := Result * 8;
      Inc(Result, N);
    end;
    Inc(I);
  end;
  if Minus then Result := -Result;
end;

function MinMax(Value, Min, Max: ShortInt): ShortInt;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: SmallInt): SmallInt;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Integer): Integer;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Int64): Int64;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Single): Single;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Double): Double;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

{$IFNDEF FPC}
function MinMax(Value, Min, Max: Extended): Extended;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;
{$ENDIF}

procedure SetControlClipRect(AControl: TWinControl; const ARect: TRect);
begin
  if AControl.HandleAllocated then
  begin
  {$IFDEF USE_WINAPI}
    SetWindowRgn(AControl.Handle, CreateRectRgn(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top), True);
  {$ELSE}
    //how to do that?
  {$ENDIF}
  end;
end;

procedure SetControlText(Value: TWinControl; const Text: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF});

  procedure SetTextBuffer(Value: TWinControl; const Text: string);
  begin
    Value.SetTextBuf(PChar(Text));
  end;

begin
{$IFDEF FPC}
  SetTextBuffer(Value, Text); // conversion to UTF8 forced anyway
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  SetTextBuffer(Value, Text);
 {$ELSE}
  if Value.HandleAllocated and (Win32Platform = VER_PLATFORM_WIN32_NT) then // unicode fully supported
    SetWindowTextW(Value.Handle, PWideChar(Text))
  else
    SetTextBuffer(Value, Text);
 {$ENDIF}
{$ENDIF}
end;

function StrNextCharIndex(AText: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index + UTF8CharacterLength(@AText[Index]);
{$ELSE}
  Result := Index + 1; // neglecting surrogate pairs
{$ENDIF}
end;

function StringCharBegin(const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := UTF8CharToByteIndex(PChar(AText), Length(AText), Index)
{$ELSE}
  Result := Index // neglecting surrogate pairs
{$ENDIF}
end;

function StringLength(const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}): Integer;
begin
{$IFDEF FPC}
  Result := UTF8Length(AText)
{$ELSE}
  Result := Length(AText) // neglecting surrogate pairs
{$ENDIF}
end;

function StringNextCharIndex(const AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index + UTF8CharacterLength(@AText[Index]);
{$ELSE}
  Result := Index + 1; // neglecting surrogate pairs
{$ENDIF}
end;

procedure TrimWhiteSpaces(var AText: {$IFDEF STRING_IS_UNICODE}PChar{$ELSE}PWideChar{$ENDIF}; var ALen: Integer; const ASet: TKSysCharSet);
begin
  while (ALen > 0) and CharInSetEx(AText[0], ASet) do
  begin
    AText := @AText[1];
    Dec(ALen)
  end;
  while (ALen > 0) and CharInSetEx(AText[ALen - 1], ASet) do
    Dec(ALen);
end;

procedure TrimWhiteSpaces(var AText: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF}; const ASet: TKSysCharSet);
begin
  while (Length(AText) > 0) and CharInSetEx(AText[1], ASet) do
    Delete(AText, 1, 1);
  while (Length(AText) > 0) and CharInSetEx(AText[Length(AText)], ASet) do
    Delete(AText, Length(AText), 1);
end;

{$IFNDEF FPC}
function WideCharToAnsiString(Text: PWideChar; CodePage: Cardinal): AnsiString;
var
  Len: Integer;
begin
  Len := WideCharToMultiByte(CodePage, 0, Text, -1, nil, 0, nil, nil);
  SetLength(Result, Len);
  WideCharToMultiByte(CodePage, 0, Text, -1, PAnsiChar(Result), Len, nil, nil);
end;
{$ENDIF}

{$IFDEF USE_WINAPI}
function GetWindowsFolder(CSIDL: Cardinal; var APath: string): Boolean;
type
  TSHGetFolderPathProc = function(hWnd: HWND; CSIDL: Integer; hToken: THandle;
    dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
var
  SHFolderHandle: HMODULE;
  SHGetFolderPathProc: TSHGetFolderPathProc;
  Buffer: PAnsiChar;
begin
  Result := False;
  APath := '';
  SHFolderHandle := GetModuleHandle(SHFolderDll);
  if SHFolderHandle <> 0 then
  begin
    SHGetFolderPathProc := GetProcAddress(SHFolderHandle, 'SHGetFolderPathA');
    if Assigned(SHGetFolderPathProc) then
    begin
      GetMem(Buffer, MAX_PATH);
      try
        if Succeeded(SHGetFolderPathProc(0, CSIDL, 0, 0, Buffer)) then
        begin
          APath := string(Buffer);
          Result := True;
        end
      finally
        FreeMem(Buffer);
      end;
    end;
  end;
end;
{$ENDIF}

end.
