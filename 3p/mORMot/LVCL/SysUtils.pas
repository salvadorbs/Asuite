unit SysUtils;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL SysUtils.pas
   Just put the LVCL directory in your Project/Options/Directories/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - Some routines are improved/faster: EncodeDate, DecodeDate, DecodeTime,
      IntToStr, HexToStr, UpperCase, CompareText, StrCopy, StrLen, StrComp,
      FileExists, CompareMem...
   - Date strings have a fixed format: 'YYYY/MM/DD hh:mm:ss'
   - format() supports quite all usual format (%% %s %d %x %.prec? %index:?),
      but without floating point args (saves 3KB on EXE size -> use str() + %s)
   - slow MBCS Ansi*() function mostly removed
   - support Win NT 4.0 and Win95 OSR2 minimum 
   - Cross-Platform: this SysUtils unit can be used on (Cross)Kylix under Linux 

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (c)2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are (c)2001 Paul Toth - http://tothpaul.free.fr
  All Rights Reserved.

}

{ $D-,L-}

Interface

uses
{$ifdef Win32}
  Windows;
{$else}
  Types,
  LibC;
{$endif}

{$WARNINGS OFF}

type
  TMethod = record
    Code, Data: Pointer;
  end;
  LongRec = packed record
    case integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;
  Int64Rec = packed record
    case integer of
      0: (Lo, Hi: Cardinal);
      1: (Cardinals: array [0..1] of Cardinal);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;
  PWordArray = ^TWordArray;
  TWordArray = array[0..32767] of Word;

type
  PDayTable = ^TDayTable;
  TDayTable = array[1..12] of Word;
  TTimeStamp = record
    Time: integer;      { Number of milliseconds since midnight }
    Date: integer;      { One plus number of days since 1/1/0001 }
  end;

const
  MonthDays: array [Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;
  FMSecsPerDay: Single = MSecsPerDay;
  IMSecsPerDay: integer = MSecsPerDay;
  DateDelta = 693594;

{$ifdef Win32}
const
  clBlack = $000000;
  clMaroon = $000080;
  clGreen = $008000;
  clOlive = $008080;
  clNavy = $800000;
  clPurple = $800080;
  clTeal = $808000;
  clGray = $808080;
  clSilver = $C0C0C0;
  clRed = $0000FF;
  clLime = $00FF00;
  clYellow = $00FFFF;
  clBlue = $FF0000;
  clFuchsia = $FF00FF;
  clAqua = $FFFF00;
  clLtGray = $C0C0C0;
  clDkGray = $808080;
  clWhite = $FFFFFF;
  clNone = $1FFFFFFF;
  clDefault = $20000000;
  clScrollBar = COLOR_SCROLLBAR or $80000000;
  clBackground = COLOR_BACKGROUND or $80000000;
  clActiveCaption = COLOR_ACTIVECAPTION or $80000000;
  clInactiveCaption = COLOR_INACTIVECAPTION or $80000000;
  clMenu = COLOR_MENU or $80000000;
  clWindow = COLOR_WINDOW or $80000000;
  clWindowFrame = COLOR_WINDOWFRAME or $80000000;
  clMenuText = COLOR_MENUTEXT or $80000000;
  clWindowText = COLOR_WINDOWTEXT or $80000000;
  clCaptionText = COLOR_CAPTIONTEXT or $80000000;
  clActiveBorder = COLOR_ACTIVEBORDER or $80000000;
  clInactiveBorder = COLOR_INACTIVEBORDER or $80000000;
  clAppWorkSpace = COLOR_APPWORKSPACE or $80000000;
  clHighlight = COLOR_HIGHLIGHT or $80000000;
  clHighlightText = COLOR_HIGHLIGHTTEXT or $80000000;
  clBtnFace = COLOR_BTNFACE or $80000000;
  clBtnShadow = COLOR_BTNSHADOW or $80000000;
  clGrayText = COLOR_GRAYTEXT or $80000000;
  clBtnText = COLOR_BTNTEXT or $80000000;
  clInactiveCaptionText = COLOR_INACTIVECAPTIONTEXT or $80000000;
  clBtnHighlight = COLOR_BTNHIGHLIGHT or $80000000;
  cl3DDkShadow = COLOR_3DDKSHADOW or $80000000;
  cl3DLight = COLOR_3DLIGHT or $80000000;
  clInfoText = COLOR_INFOTEXT or $80000000;
  clInfoBk = COLOR_INFOBK or $80000000;

const
  PathDelim  = '\';
  DriveDelim = ':';
  PathSep    = ';';

procedure Error(const Msg: string; Value: integer = -1);
procedure MsgBox(const Msg: string);
function IdentToColor(Ident: PChar): integer;
{$endif}

function Rect(Left, Top, Width, Height: integer): TRect;

function StringIndex(const str: string; const p: array of PChar):integer;

function IntToHex(Value: cardinal; Digits: integer): string;
function StrToInt(const S: string): integer;
function StrToIntDef(const S: string; Default: integer): integer;
function TryStrToInt(const S: string; out Value: integer): Boolean;
function GUIDToString(const GUID: TGUID): string;

function StrLen(S: PChar): integer;
function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;
function StrComp(Str1, Str2: PChar): integer;
function StrIComp(Str1, Str2: PChar): Integer;
function StrEnd(Str: PChar): PChar;
function StrCopy(Dest: PChar; const Source: PChar): PChar;
function StrCat(Dest: PChar; const Source: PChar): PChar;
function StrPCopy(Dest: PChar; const Source: string): PChar;
function StrScan(Str: PChar; Chr: Char): PChar;

function Trim(const S: string): string;

function ExtractFilePath(const FileName: string): string;
function ExtractFileDir(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;
function ChangeFileExt(const FileName, Extension: string): string;

function IncludeTrailingPathDelimiter(const S: string): string;
function LastDelimiter(const Delimiters, S: string): integer;

function UpperCase(const S: string): string;
function LowerCase(const S: string): string;

{$ifdef Win32}
function DiskFree(Drive: Byte): Int64;
function DiskSize(Drive: Byte): Int64;
{$endif}

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;

function CompareStr(const S1, S2: string): Integer; 

function CompareText(const S1, S2: string): integer;
function SameText(const S1, S2: string): Boolean;

// Borland's code
const
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeID  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faSymLink   = $00000040;
  faAnyFile   = $0000003F;
{ File open modes }
{$ifdef Win32}
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;

function ExpandFileName(const FileName: string): string; // not relevant

{$else}
  fmOpenRead       = O_RDONLY;
  fmOpenWrite      = O_WRONLY;
  fmOpenReadWrite  = O_RDWR;
//  fmShareCompat not supported
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
//  fmShareDenyRead  not supported
  fmShareDenyNone  = $0030;

const
  PathDelim  = '/';
  DriveDelim = '';
  PathSep    = ':';

{$endif} // Linux

type
  TFileName = type string;

function  FileCreate(const FileName: TFileName): THandle;
function  FileOpen(const FileName: TFileName; Mode: LongWord): THandle;
procedure FileClose(Handle: THandle);
function  FileSeek(Handle: THandle; Offset, Origin: integer): integer;
function  FileRead(Handle: THandle; var Buffer; Count: LongWord): integer;
function  FileWrite(Handle: THandle; const Buffer; Count: LongWord): integer;

function FileDateToDateTime(FileDate: integer): TDateTime;
function DateTimeToFileDate(DateTime: TDateTime): integer;


function DirectoryExists(const Directory: TFileName): Boolean;
function GetCurrentDir: TFileName;
function CreateDir(const Dir: TFileName): Boolean;
function RemoveDir(const Dir: TFileName): Boolean;

function SafeLoadLibrary(const Filename: TFileName): HMODULE;

function FileExists(const FileName: TFileName): Boolean;
function FileAge(const FileName: TFileName): integer;
function FileSetDate(F: THandle; Age: integer): integer; overload;
function FileSetDate(const FileName: TFileName; Age: integer): integer; overload;
function RenameFile(const OldName, NewName: TFileName): Boolean;
function DeleteFile(const FileName: TFileName): Boolean;

{$ifdef Win32}
function GetModuleName(Module: HMODULE): TFileName;
{$endif}

var
  lastFileAgeSize: integer; // updated with FileAge(fileName)


type
  TSearchRec = record
    Time: integer;
    Size: integer;
    Attr: integer;
    Name: string;
    ExcludeAttr: integer;
{$ifdef Win32}
    FindHandle: THandle;
    FindData: TWin32FindData;
{$else}
    Mode: mode_t;
    FindHandle: Pointer;
    PathOnly: string;
    Pattern: string;
{$endif}
  end;

function FindFirst(const Path: string; Attr: integer;
  var F: TSearchRec): integer;
function FindNext(var F: TSearchRec): integer;
procedure FindClose(var F: TSearchRec);

function Now: TDateTime;
function FileGetDate(Handle: THandle): Integer;

function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime; overload;
function EncodeTime(Hour, Min, Sec, MSec: cardinal): TDateTime; overload;

procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
               
function SystemTimeToDosDateTime(const SystemTime: TSystemTime): integer;
function NowToDosDateTime: integer;

procedure DosDateTimeToSystemTime(DosDateTime: integer; out SystemTime: TSystemTime);
// warning: wDayOfWeek is not set

function DateTimeToStr(const DateTime: TDateTime): string;
function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
// date format is fixed to 'YYYY/MM/DD hh:mm:ss' string layout

/// fast write Y (as 4 chars) in P^
procedure YearToPAnsiChar(Y: Word; P: PAnsiChar);

{$ifdef Win32}
(*function SelectDirectory(Handle: integer; const Caption: string;
  var Directory: string): Boolean;*)
procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);

function GetFileVersion(const aFileName: string): Cardinal;
{$endif}

function AllocMem(Size: integer): pointer;
procedure FreeAndNil(var Obj); {$ifdef UNICODE}inline;{$endif}
function CompareMem(P1, P2: Pointer; Length: integer): Boolean;

function Format(const Format: string; const Args: array of const): string;
// supported: %% %s %d %x %.prec? %index:?

function IntToStr(Value: integer): string; overload;
function IntToStr(Value: Int64): string; overload;

function EncodeDate(Year, Month, Day: Word): TDateTime;
procedure DecodeDate(const DT: TDateTime; out Year, Month, Day: word); overload;
function IsLeapYear(Year: cardinal): Boolean;

type
  Exception = class(TObject)
  private
    FMessage: string;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    property Message: string read FMessage write FMessage;
  end;
  EAssertionFailed = class(Exception);
  EStreamError = class(Exception);
  EExternal  = class(Exception);
  EExternalException = class(Exception);
  ERangeError = class(Exception);
  EAccessViolation = class(EExternal);
  EDivByZero = class(EExternal);
  ExceptClass = class of Exception;
  EOSError = class(EExternal)
  public
    ErrorCode: cardinal;
  end;

procedure OutOfMemoryError;

function SysErrorMessage(ErrorCode: Integer): string;

procedure RaiseLastOSError;

{$ifdef Win32}
var
  Win32Platform: integer = 0;
  Win32MajorVersion: integer = 0;
  Win32MinorVersion: integer = 0;
  Win32BuildNumber: integer = 0;
  Win32CSDVersion: string = '';
{$endif}

procedure Sleep(milliseconds: Cardinal);{$ifdef Win32} stdcall; {$endif}

function AnsiSameText(const S1, S2: string): Boolean;
function AnsiCompareText(const S1, S2: string): Integer;
function AnsiSameStr(const S1, S2: string): Boolean;
function AnsiCompareStr(const S1, S2: string): Integer;
function AnsiUpperCase(const S: string): string;
function AnsiLowerCase(const S: string): string;


const
  // our customs SysUtils.pas (normal and LVCL) contains the same array
  TwoDigitLookup: packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

  HexChars: array[0..15] of Char = '0123456789ABCDEF';


implementation

uses
  Classes;

procedure AddCharLS(var S: string; c: char);
var L: integer;
begin
  L := integer(s);
  if L<>0 then
    L := pInteger(L-4)^; // L := length(S)
  SetLength(s,L+1); // with FastMM4, this SetLength() is almost immediate
{$ifdef UNICODE}
  PWordArray(s)[L] := ord(c);
{$else}
  PByteArray(s)[L] := ord(c);
{$endif}
end;

procedure DecodeDate(const DT: TDateTime; out Year, Month, Day: word);
var  J: integer;
begin
  J := pred((Trunc (DT) + 693900) shl 2);
  Year := J div 146097;
  Day := (J - 146097 * Year) shr 2;
  J := (Day shl 2 + 3) div 1461;
  Day := (Day shl 2 + 7 - 1461 * J) shr 2;
  Month := (5 * Day - 3) div 153;
  Day := (5 * Day + 2 - 153 * Month) div 5;
  Year := 100 * Year + J;
  if Month < 10 then
    Inc (Month, 3)
  else begin
    Dec (Month, 9);
    Inc (Year);
  end;
end;

function IsLeapYear(Year: cardinal): Boolean;
asm
  test  al,3
  jz    @@CheckCentury
  xor   eax,eax        {Return False}
  ret
@@CheckCentury:
  mov   edx,$028F5C29 {((2^32)+100-1)/100}
  mov   ecx,eax
  mul   edx            {EDX = Year DIV 100}
  mov   eax,edx
  imul  edx,100       {EDX = (Year DIV 100) * 100}
  cmp   ecx,edx
  je    @@Century      {Year is Divisible by 100}
  mov   al,true       {Return True}
  ret
@@Century:
  test  al,3          {Check if Divisible by 400}
  setz  al             {Set Result}
end;

type TWordRec = packed record YDiv100, YMod100: byte; end;

function Div100(Y: Word): TWordRec;
asm
  mov cl,100
  div cl // ah=remainder=Y mod 100, al=quotient=Year div 100
end;

function EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  result := 0;
  if (Month < 1) or (Month > 12) then exit;
  if (Day <= MonthDays[true][Month]) and // test worse case = leap year
    (Year >= 1) and (Year < 10000) and
    (Month < 13) and (Day > 0) then begin
    if Month > 2 then
      Dec(Month, 3) else
    if Month > 0 then begin
      Inc(Month, 9);
      Dec(Year);
    end
    else // Month <= 0
      exit;
    with Div100(Year) do
      result := (146097 * YDiv100) shr 2 + (1461 * YMod100) shr 2 +
            (153 * Month + 2) div 5 + Day - 693900;
  end;
end;

function IntToStr(Value : integer): string;
{$ifdef UNICODE}
begin
  str(Value,result);
end;
{$else}
 // 3x faster than SysUtils.IntToStr
// from IntToStr32_JOH_IA32_6_a
asm
  push   ebx
  push   edi
  push   esi
  mov    ebx,eax                {Value}
  sar    ebx,31                 {0 for +ve Value or -1 for -ve Value}
  xor    eax,ebx
  sub    eax,ebx                {ABS(Value)}
  mov    esi,10                 {Max Digits in result}
  mov    edi,edx                {@result}
  cmp    eax,10;         sbb    esi, 0
  cmp    eax,100;        sbb    esi, 0
  cmp    eax,1000;       sbb    esi, 0
  cmp    eax,10000;      sbb    esi, 0
  cmp    eax,100000;     sbb    esi, 0
  cmp    eax,1000000;    sbb    esi, 0
  cmp    eax,10000000;   sbb    esi, 0
  cmp    eax,100000000;  sbb    esi, 0
  cmp    eax,1000000000; sbb    esi, ebx    {Digits (Including Sign Character)}
  mov    ecx,[edx]              {result}
  test   ecx,ecx
  je     @@NewStr               {Create New string for result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr            {Reference Count<>1}
  cmp    esi,[ecx-4]
  je     @@LengthOk             {Existing Length = Required Length}
  sub    ecx,8                  {Allocation Address}
  push   eax                    {ABS(Value)}
  push   ecx
  mov    eax,esp
  lea    edx,[esi+9]            {New Allocation Size}
  call   system.@ReallocMem     {Reallocate result string}
  pop    ecx
  pop    eax                    {ABS(Value)}
  add    ecx,8                  {result}
  mov    [ecx-4],esi            {Set New Length}
  mov    byte ptr [ecx+esi],0   {Add Null Terminator}
  mov    [edi],ecx              {Set result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx,dword ptr [ecx-8]  {Reference Count}
  add     edx,1
  jz      @@NewStr               {RefCount = -1 (string Constant)}
  lock    dec dword ptr [ecx-8]  {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax,esi                 {Length}
  call   system.@NewAnsiString
  mov    [edi],eax               {Set result Address}
  mov    ecx,eax                 {result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    byte ptr [ecx],'-'      {Store '-' Character (May be Overwritten)}
  add    esi,ebx                 {Digits (Excluding Sign Character)}
  sub    ecx,ebx                 {Destination of 1st Digit}
  sub    esi,2                   {Digits (Excluding Sign Character) - 2}
  jle    @@FinalDigits           {1 or 2 Digit Value}
  cmp    esi,8                   {10 Digit Value?}
  jne    @@SetResult             {Not a 10 Digit Value}
  sub    eax,2000000000          {Digit 10 must be either '1' or '2'}
  mov    dl,'2'
  jnc    @@SetDigit10            {Digit 10 = '2'}
  mov    dl,'1'                  {Digit 10 = '1'}
  add    eax,1000000000
@@SetDigit10:
  mov    [ecx],dl                {Save Digit 10}
  mov    esi,7                   {9 Digits Remaining}
  add    ecx,1                   {Destination of 2nd Digit}
@@SetResult:
  mov    edi,$28F5C29            {((2^32)+100-1)/100}
@@Loop:
  mov    ebx,eax                 {Dividend}
  mul    edi                     {EDX = Dividend DIV 100}
  mov    eax,edx                 {Set Next Dividend}
  imul   edx,-200                {-2 * (100 * Dividend DIV  100)}
  movzx  edx,word ptr [TwoDigitLookup+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [ecx+esi],dx
  sub    esi,2
  jg     @@Loop                  {Loop until 1 or 2 Digits Remaining}
@@FinalDigits:
  pop    esi
  pop    edi
  pop    ebx
  jnz    @@LastDigit
  movzx  eax,word ptr [TwoDigitLookup+eax*2]
  mov    [ecx],ax                {Save Final 2 Digits}
  ret
@@LastDigit:
  or     al,'0'                  {Ascii Adjustment}
  mov    [ecx],al                {Save Final Digit}
end;
{$endif}

function IntToStr(Value: Int64): string;
{$ifdef UNICODE}
begin
  str(Value,result);
end;
{$else}
// from IntToStr64_JOH_IA32_6_b
asm
  push   ebx
  mov    ecx, [ebp+8]            {Low Integer of Value}
  mov    edx, [ebp+12]           {High Integer of Value}
  xor    ebp, ebp                {Clear Sign Flag (EBP Already Pushed)}
  mov    ebx, ecx                {Low Integer of Value}
  test   edx, edx
  jnl    @@AbsValue
  mov    ebp, 1                  {EBP = 1 for -ve Value or 0 for +ve Value}
  neg    ecx
  adc    edx, 0
  neg    edx
@@AbsValue:                      {EDX:ECX = Abs(Value)}
  jnz    @@Large
  test   ecx, ecx
  js     @@Large
  mov    edx, eax                {@Result}
  mov    eax, ebx                {Low Integer of Value}
  call   IntToStr                {Call Fastest Integer IntToStr Function}
  pop    ebx
@@Exit:
  pop    ebp                     {Restore Stack and Exit}
  ret    8
@@Large:
  push   edi
  push   esi
  mov    edi, eax
  xor    ebx, ebx
  xor    eax, eax
@@Test15:                        {Test for 15 or More Digits}
  cmp    edx, $00005af3          {100000000000000 div $100000000}
  jne    @@Check15
  cmp    ecx, $107a4000          {100000000000000 mod $100000000}
@@Check15:
  jb     @@Test13
@@Test17:                        {Test for 17 or More Digits}
  cmp    edx, $002386f2          {10000000000000000 div $100000000}
  jne    @@Check17
  cmp    ecx, $6fc10000          {10000000000000000 mod $100000000}
@@Check17:
  jb     @@Test15or16
@@Test19:                        {Test for 19 Digits}
  cmp    edx, $0de0b6b3          {1000000000000000000 div $100000000}
  jne    @@Check19
  cmp    ecx, $a7640000          {1000000000000000000 mod $100000000}
@@Check19:
  jb     @@Test17or18
  mov    al, 19
  jmp    @@SetLength
@@Test17or18:                    {17 or 18 Digits}
  mov    bl, 18
  cmp    edx, $01634578          {100000000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $5d8a0000          {100000000000000000 mod $100000000}
  jmp    @@SetLen
@@Test15or16:                    {15 or 16 Digits}
  mov    bl, 16
  cmp    edx, $00038d7e          {1000000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $a4c68000          {1000000000000000 mod $100000000}
  jmp    @@SetLen
@@Test13:                        {Test for 13 or More Digits}
  cmp    edx, $000000e8          {1000000000000 div $100000000}
  jne    @@Check13
  cmp    ecx, $d4a51000          {1000000000000 mod $100000000}
@@Check13:
  jb     @@Test11
@@Test13or14:                    {13 or 14 Digits}
  mov    bl, 14
  cmp    edx, $00000918          {10000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $4e72a000          {10000000000000 mod $100000000}
  jmp    @@SetLen
@@Test11:                        {10, 11 or 12 Digits}
  cmp    edx, $02                {10000000000 div $100000000}
  jne    @@Check11
  cmp    ecx, $540be400          {10000000000 mod $100000000}
@@Check11:
  mov    bl, 11
  jb     @@SetLen                {10 Digits}
@@Test11or12:                    {11 or 12 Digits}
  mov    bl, 12
  cmp    edx, $17                {100000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $4876e800          {100000000000 mod $100000000}
@@SetLen:
  sbb    eax, 0                  {Adjust for Odd/Evem Digit Count}
  add    eax, ebx
@@SetLength:                     {Abs(Value) in EDX:ECX, Digits in EAX}
  push   ecx                     {Save Abs(Value)}
  push   edx
  lea    edx, [eax+ebp]          {Digits Needed (Including Sign Character)}
  mov    ecx, [edi]              {@Result}
  mov    esi, edx                {Digits Needed (Including Sign Character)}
  test   ecx, ecx
  je     @@NewStr                {Create New AnsiString for Result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr             {Reference Count<>1}
  cmp    esi, [ecx-4]
  je     @@LengthOk              {Existing Length = Required Length}
  sub    ecx, 8                  {Allocation Address}
  push   eax                     {ABS(Value)}
  push   ecx
  mov    eax, esp
  lea    edx, [esi+9]            {New Allocation Size}
  call   system.@ReallocMem      {Reallocate Result AnsiString}
  pop    ecx
  pop    eax                     {ABS(Value)}
  add    ecx, 8                  {@Result}
  mov    [ecx-4], esi            {Set New Length}
  mov    byte ptr [ecx+esi], 0   {Add Null Terminator}
  mov    [edi], ecx              {Set Result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx, dword ptr [ecx-8]  {Reference Count}
  add     edx, 1
  jz      @@NewStr                {RefCount = -1 (AnsiString Constant)}
  lock    dec dword ptr [ecx-8]   {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax, esi                {Length}
  call   system.@NewAnsiString
  mov    [edi], eax              {Set Result Address}
  mov    ecx, eax                {@Result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    edi, [edi]              {@Result}
  sub    esi, ebp                {Digits Needed (Excluding Sign Character)}
  mov    byte ptr [edi], '-'     {Store '-' Character (May be Overwritten)}
  add    edi, ebp                {Destination of 1st Digit}
  pop    edx                     {Restore Abs(Value)}
  pop    eax
  cmp    esi, 17
  jl     @@LessThan17Digits      {Digits < 17}
  je     @@SetDigit17            {Digits = 17}
  cmp    esi, 18
  je     @@SetDigit18            {Digits = 18}
  mov    cl, '0' - 1
  mov    ebx, $a7640000          {1000000000000000000 mod $100000000}
  mov    ebp, $0de0b6b3          {1000000000000000000 div $100000000}
@@CalcDigit19:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit19
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1
@@SetDigit18:
  mov    cl, '0' - 1
  mov    ebx, $5d8a0000          {100000000000000000 mod $100000000}
  mov    ebp, $01634578          {100000000000000000 div $100000000}
@@CalcDigit18:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit18
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1
@@SetDigit17:
  mov    cl, '0' - 1
  mov    ebx, $6fc10000          {10000000000000000 mod $100000000}
  mov    ebp, $002386f2          {10000000000000000 div $100000000}
@@CalcDigit17:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit17
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1                  {Update Destination}
  mov    esi, 16                 {Set 16 Digits Left}
@@LessThan17Digits:              {Process Next 8 Digits}
  mov    ecx, 100000000          {EDX:EAX = Abs(Value) = Dividend}
  div    ecx
  mov    ebp, eax                {Dividend DIV 100000000}
  mov    ebx, edx
  mov    eax, edx                {Dividend MOD 100000000}
  mov    edx, $51EB851F
  mul    edx
  shr    edx, 5                  {Dividend DIV 100}
  mov    eax, edx                {Set Next Dividend}
  lea    edx, [edx*4+edx]
  lea    edx, [edx*4+edx]
  shl    edx, 2                  {Dividend DIV 100 * 100}
  sub    ebx, edx                {Remainder (0..99)}
  movzx  ebx, word ptr [TwoDigitLookup+ebx*2]
  shl    ebx, 16
  mov    edx, $51EB851F
  mov    ecx, eax                {Dividend}
  mul    edx
  shr    edx, 5                  {Dividend DIV 100}
  mov    eax, edx
  lea    edx, [edx*4+edx]
  lea    edx, [edx*4+edx]
  shl    edx, 2                  {Dividend DIV 100 * 100}
  sub    ecx, edx                {Remainder (0..99)}
  or     bx, word ptr [TwoDigitLookup+ecx*2]
  mov    [edi+esi-4], ebx        {Store 4 Digits}
  mov    ebx, eax
  mov    edx, $51EB851F
  mul    edx
  shr    edx, 5                  {EDX = Dividend DIV 100}
  lea    eax, [edx*4+edx]
  lea    eax, [eax*4+eax]
  shl    eax, 2                  {EAX = Dividend DIV 100 * 100}
  sub    ebx, eax                {Remainder (0..99)}
  movzx  ebx, word ptr [TwoDigitLookup+ebx*2]
  movzx  ecx, word ptr [TwoDigitLookup+edx*2]
  shl    ebx, 16
  or     ebx, ecx
  mov    [edi+esi-8], ebx        {Store 4 Digits}
  mov    eax, ebp                {Remainder}
  sub    esi, 10                 {Digits Left - 2}
  jz     @@Last2Digits
@@SmallLoop:                     {Process Remaining Digits}
  mov    edx, $28F5C29           {((2^32)+100-1)/100}
  mov    ebx, eax                {Dividend}
  mul    edx
  mov    eax, edx                {Set Next Dividend}
  imul   edx, -200
  movzx  edx, word ptr [TwoDigitLookup+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [edi+esi], dx
  sub    esi, 2
  jg     @@SmallLoop             {Repeat Until Less than 2 Digits Remaining}
  jz     @@Last2Digits
  or     al , '0'                {Ascii Adjustment}
  mov    [edi], al               {Save Final Digit}
  jmp    @@Done
@@Last2Digits:
  movzx  eax, word ptr [TwoDigitLookup+eax*2]
  mov    [edi], ax               {Save Final 2 Digits}
@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;
{$endif}

{$ifdef Win32}
procedure Error(const Msg:string; Value: integer = -1);
begin
   if Value=-1 then
    MessageBox(0,pointer(Msg),nil,MB_ICONSTOP or MB_TASKMODAL or MB_DEFAULT_DESKTOP_ONLY) else
    MessageBox(0,pointer(Msg+' '+IntToStr(Value)),nil,MB_ICONSTOP or MB_TASKMODAL or MB_DEFAULT_DESKTOP_ONLY); 
end;

procedure MsgBox(const Msg:string);
begin
  MessageBox(0,pointer(Msg),nil,0);
end;
{$endif}

function Rect(Left,Top,Width,Height:integer):TRect;
begin
  result.Left   := Left;
  result.Top    := Top;
  result.Right  := Left+Width;
  result.Bottom := Top+Height;
end;

function StringIndex(const str: string; const p: array of PChar): integer;
begin
  result := High(p);
  while (result>=0) and (StrComp(p[result],pointer(str))<>0) do
    dec(result);
end;

{$ifdef Win32}

{ Color mapping routines }

function IdentToColor(Ident: PChar): integer;
type
  TIdentMapEntry = packed record
    Value: integer;
    Name: PChar;
  end;
const  // Value are integer, not enumerates -> no RTTI trick possible
  Colors: array[0..41] of TIdentMapEntry = (
    (Value: clBlack; Name: 'Black'),
    (Value: clMaroon; Name: 'Maroon'),
    (Value: clGreen; Name: 'Green'),
    (Value: clOlive; Name: 'Olive'),
    (Value: clNavy; Name: 'Navy'),
    (Value: clPurple; Name: 'Purple'),
    (Value: clTeal; Name: 'Teal'),
    (Value: clGray; Name: 'Gray'),
    (Value: clSilver; Name: 'Silver'),
    (Value: clRed; Name: 'Red'),
    (Value: clLime; Name: 'Lime'),
    (Value: clYellow; Name: 'Yellow'),
    (Value: clBlue; Name: 'Blue'),
    (Value: clFuchsia; Name: 'Fuchsia'),
    (Value: clAqua; Name: 'Aqua'),
    (Value: clWhite; Name: 'White'),
    (Value: clScrollBar; Name: 'ScrollBar'),
    (Value: clBackground; Name: 'Background'),
    (Value: clActiveCaption; Name: 'ActiveCaption'),
    (Value: clInactiveCaption; Name: 'InactiveCaption'),
    (Value: clMenu; Name: 'Menu'),
    (Value: clWindow; Name: 'Window'),
    (Value: clWindowFrame; Name: 'WindowFrame'),
    (Value: clMenuText; Name: 'MenuText'),
    (Value: clWindowText; Name: 'WindowText'),
    (Value: clCaptionText; Name: 'CaptionText'),
    (Value: clActiveBorder; Name: 'ActiveBorder'),
    (Value: clInactiveBorder; Name: 'InactiveBorder'),
    (Value: clAppWorkSpace; Name: 'AppWorkSpace'),
    (Value: clHighlight; Name: 'Highlight'),
    (Value: clHighlightText; Name: 'HighlightText'),
    (Value: clBtnFace; Name: 'BtnFace'),
    (Value: clBtnShadow; Name: 'BtnShadow'),
    (Value: clGrayText; Name: 'GrayText'),
    (Value: clBtnText; Name: 'BtnText'),
    (Value: clInactiveCaptionText; Name: 'InactiveCaptionText'),
    (Value: clBtnHighlight; Name: 'BtnHighlight'),
    (Value: cl3DDkShadow; Name: '3DDkShadow'),
    (Value: cl3DLight; Name: '3DLight'),
    (Value: clInfoText; Name: 'InfoText'),
    (Value: clInfoBk; Name: 'InfoBk'),
    (Value: clNone; Name: 'None'));
begin
  if PWord(Ident)^=ord('c')+ord('l')shl 8 then begin
    inc(Ident,2);
    result := high(Colors);
    while result>=0 do
      if StrComp(Colors[result].Name,Ident)=0 then begin
        result := Colors[result].Value;
        if result<0 then
          result := GetSysColor(result and $FF);
        exit;
      end else
      dec(result);
  end;
  result := clNone;
end;
{$endif}

function IntToHex(Value: cardinal; Digits: integer): string;
begin
  result := '';
  while (Digits>0) or (Value>0) do begin
    result := HexChars[(Value shr 4) and $F]+HexChars[Value and $F]+result;
    dec(Digits,2);
    Value := Value shr 8;
  end;
end;

function GUIDToString(const GUID: TGUID): string;
procedure Write(P: PChar; B: PByteArray);
var i: integer;
begin // encode as '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}'
  P^ := '{'; inc(P);
  for i := 3 downto 0 do begin
    P^ := HexChars[(B[i] shr 4) and $F]; inc(P);
    P^ := HexChars[B[i] and $F]; inc(P);
  end;
  inc(integer(B),4);
  for i := 1 to 2 do begin
    P^ := '-'; inc(P);
    P^ := HexChars[(B[1] shr 4) and $F]; inc(P);
    P^ := HexChars[B[1] and $F]; inc(P);
    P^ := HexChars[(B[0] shr 4) and $F]; inc(P);
    P^ := HexChars[B[0] and $F]; inc(P);
    inc(integer(B),2);
  end;
  P^ := '-'; inc(P);
  P^ := HexChars[(B[0] shr 4) and $F]; inc(P);
  P^ := HexChars[B[0] and $F]; inc(P);
  P^ := HexChars[(B[1] shr 4) and $F]; inc(P);
  P^ := HexChars[B[1] and $F]; inc(P);
  inc(integer(B),2);
  P^ := '-'; inc(P);
  for i := 1 to 6 do begin
    P^ := HexChars[(B[0] shr 4) and $F]; inc(P);
    P^ := HexChars[B[0] and $F]; inc(integer(B)); inc(P);
  end;
  P^ := '}';
end;
begin
  SetString(result,nil,38);
  Write(pointer(result),@GUID);
end;

function TryStrToInt(const S: string; out Value: integer): Boolean;
var E: integer;
begin
  Val(S,Value,E);
  result := (E=0);
end;

function StrToInt(const S: string): integer;
begin
  result := StrToIntDef(S,0);
end;

function StrToIntDef(const S: string; Default: integer): integer;
begin
  if not TryStrToInt(S, result) then
    result := Default;
end;

function StrComp(Str1, Str2: PChar): integer;
{$ifdef UNICODE}
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    if Str1^=Str2^ then
    repeat
      if (Str1^=#0) or (Str2^=#0) then break;
      inc(Str1);
      inc(Str2);
    until Str1^<>Str2^;
    result := pWord(Str1)^-pWord(Str2)^;
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;
{$else}
asm // faster version by AB
        MOV     ECX,EAX
        XOR     EAX,EAX
        CMP     ECX,EDX
        JE      @Exit2  //same string or both nil
        OR      ECX,ECX
        MOV     AL,1
        JZ      @Exit2  //Str1=''
        OR      EDX,EDX
        JE      @min
@1:     MOV     AL,[ECX]
        INC     ECX
        MOV     AH,[EDX]
        INC     EDX
        TEST    AL,AL
        JE      @Exit
        CMP     AL,AH
        JE      @1
@Exit:  XOR     EDX,EDX
        XCHG    AH,DL
        SUB     EAX,EDX
@Exit2: RET
@min:   OR      EAX,-1
end;
{$endif}

function StrIComp(Str1, Str2: PChar): integer;
{$ifdef UNICODE}
var C1, C2: Char;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    repeat
      C1 := Str1^;
      C2 := Str2^;
      if ord(C1) in [ord('a')..ord('z')] then dec(C1,32);
      if ord(C2) in [ord('a')..ord('z')] then dec(C2,32);
      if (C1<>C2) or (C1=#0) then
        break;
      Inc(Str1);
      Inc(Str2);
    until false;
    Result := Ord(C1) - Ord(C2);
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;
{$else}
asm // faster version by AB
        MOV     ECX,EAX
        XOR     EAX,EAX
        CMP     ECX,EDX
        JE      @Exit2  //same string or both nil
        OR      ECX,ECX
        MOV     AL,1
        JZ      @Exit2  //Str1=''
        OR      EDX,EDX
        JE      @min
@1:     MOV     AL,[ECX]
        INC     ECX
        TEST    AL,AL
        MOV     AH,[EDX]
        LEA     EDX,EDX+1
        JE      @Exit
        CMP     AL,AH
        JE      @1
        SUB     AL,'a'
        SUB     AH,'a'
        CMP     AL,'z'-'a'
        JA      @@2
        SUB     AL,20H
@@2:    CMP     AH,'z'-'a'
        JA      @@3
        SUB     AH,20H
@@3:    CMP     AL,AH
        JE      @1
@Exit:  XOR     EDX,EDX
        XCHG    AH,DL
        SUB     EAX,EDX
@Exit2: RET
@min:   OR      EAX,-1
end;
{$endif}

function StrLen(S: PChar): integer;
{$ifdef UNICODE}
begin
  result := 0;
  if S<>nil then
  while true do
    if S[0]<>#0 then
    if S[1]<>#0 then
    if S[2]<>#0 then
    if S[3]<>#0 then begin
      inc(S,4);
      inc(result,4);
    end else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;
{$else}
// faster than default SysUtils version
asm
     test eax,eax
     jz @@z
     cmp   byte ptr [eax  ],0; je @@0
     cmp   byte ptr [eax+1],0; je @@1
     cmp   byte ptr [eax+2],0; je @@2
     cmp   byte ptr [eax+3],0; je @@3
     push  eax
     and   eax,-4              {DWORD Align Reads}
@@Loop:
     add   eax,4
     mov   edx,[eax]           {4 Chars per Loop}
     lea   ecx,[edx-$01010101]
     not   edx
     and   edx,ecx
     and   edx,$80808080       {Set Byte to $80 at each #0 Position}
     jz    @@Loop              {Loop until any #0 Found}
@@SetResult:
     pop   ecx
     bsf   edx,edx             {Find First #0 Position}
     shr   edx,3               {Byte Offset of First #0}
     add   eax,edx             {Address of First #0}
     sub   eax,ecx
@@z: ret
@@0: xor eax,eax; ret
@@1: mov eax,1;   ret
@@2: mov eax,2;   ret
@@3: mov eax,3
end;
{$endif}

function StrEnd(Str: PChar): PChar;
asm // faster version by AB
  push eax
  call StrLen
  pop edx
  {$ifdef UNICODE}
  lea eax,eax+edx*2
  {$else}
  add eax,edx
  {$endif}
end;

function StrCopy(Dest: PChar; const Source: PChar): PChar;
asm // faster version by AB
  push eax
  push eax
  push edx
  mov eax,edx
  call StrLen
  {$ifdef UNICODE}
  lea ecx,eax*2+2 // also copy last #0
  {$else}
  lea ecx,eax+1
  {$endif}
  pop eax
  pop edx // xchg eax,edx
  call move
  pop eax
end;

function StrCat(Dest: PChar; const Source: PChar): PChar;
begin
  StrCopy(Dest+StrLen(Dest), Source);
  Result := Dest;
end;

function StrPCopy(Dest: PChar; const Source: string): PChar;
asm // faster version by AB
    or edx,edx
    push eax
    xchg eax,edx
    jz @z
    mov ecx,[eax-4]
    {$ifdef UNICODE}
    lea ecx,ecx*2+2
    {$else}
    lea ecx,ecx+1 // copy last #0
    {$endif}
    call move
@z: pop eax
end;

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;
asm // faster version by AB
    or edx,edx
    jz @z
    push eax
    push ebx
    xchg eax,edx
    {$ifdef UNICODE}
    lea ebx,ecx*2
    {$else}
    mov ebx,ecx
    {$endif}
    xor ecx,ecx
@1: {$ifdef UNICODE}
    cmp word ptr [eax+ecx],0
    lea ecx,ecx+2
    {$else}
    cmp byte ptr [eax+ecx],0
    lea ecx,ecx+1 // copy last #0
    {$endif}
    je @s
    cmp ecx,ebx
    jb @1
@s: pop ebx
    call Move
    pop eax
@z: 
end;

function StrScan(Str: PChar; Chr: Char): PChar;
asm // faster version by AB - eax=Str dl=Chr
    or eax,eax
    jz @z
{$ifdef UNICODE}
@1: mov cx,[eax]
    cmp cx,dx
    jz @z
    lea eax,eax+2
    or cx,cx
{$else}
@1: mov cl,[eax]
    cmp cl,dl
    jz @z
    inc eax
    or cl,cl
{$endif}
    jnz @1
    xor eax,eax
@z:
end;

function Trim(const S: string): string;
{$ifdef UNICODE}
asm  // fast implementation by John O'Harrow
  test eax,eax                   {S = nil?}
  xchg eax,edx
  jz   System.@UStrClr           {Yes, Return Empty String}
  mov  ecx,[edx-4]               {Length(S)}
  cmp  byte ptr [edx],' '        {S[1] <= ' '?}
  jbe  @@TrimLeft                {Yes, Trim Leading Spaces}
  cmp  byte ptr [edx+ecx-1],' '  {S[Length(S)] <= ' '?}
  jbe  @@TrimRight               {Yes, Trim Trailing Spaces}
  jmp  System.@UStrLAsg          {No, Result := S (which occurs most time)}
@@TrimLeft:                      {Strip Leading Whitespace}
  dec  ecx
  jle  System.@UStrClr           {All Whitespace}
  inc  edx
  cmp  byte ptr [edx],' '
  jbe  @@TrimLeft
@@CheckDone:
  cmp  byte ptr [edx+ecx-1],' '
  ja   System.@UStrFromPCharLen
@@TrimRight:                     {Strip Trailing Whitespace}
  dec  ecx
  jmp  @@CheckDone
end;
{$else}
asm  // fast implementation by John O'Harrow
  test eax,eax                   {S = nil?}
  xchg eax,edx
  jz   System.@LStrClr           {Yes, Return Empty String}
  mov  ecx,[edx-4]               {Length(S)}
  cmp  byte ptr [edx],' '        {S[1] <= ' '?}
  jbe  @@TrimLeft                {Yes, Trim Leading Spaces}
  cmp  byte ptr [edx+ecx-1],' '  {S[Length(S)] <= ' '?}
  jbe  @@TrimRight               {Yes, Trim Trailing Spaces}
  jmp  System.@LStrLAsg          {No, Result := S (which occurs most time)}
@@TrimLeft:                      {Strip Leading Whitespace}
  dec  ecx
  jle  System.@LStrClr           {All Whitespace}
  inc  edx
  cmp  byte ptr [edx],' '
  jbe  @@TrimLeft
@@CheckDone:
  cmp  byte ptr [edx+ecx-1],' '
  ja   System.@LStrFromPCharLen
@@TrimRight:                     {Strip Trailing Whitespace}
  dec  ecx
  jmp  @@CheckDone
end;
{$endif}

{$ifdef Win32}
function FileCreate(const FileName: TFileName): THandle;
begin
  result := CreateFile(pointer(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

function FileOpen(const FileName: TFileName; Mode: LongWord): THandle;
const
  AccessMode: array[0..2] of LongWord = (GENERIC_READ,GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (0,0,FILE_SHARE_READ,FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  result := integer(CreateFile(pointer(FileName), AccessMode[Mode and 3],
    ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0));
end;

procedure FileClose(Handle: THandle);
begin
  CloseHandle(Handle);
end;

function FileSeek(Handle: THandle; Offset, Origin: integer): integer;
begin
  result := SetFilePointer(Handle, Offset, nil, Origin);
end;

function FileRead(Handle: THandle; var Buffer; Count: LongWord): integer;
begin
  if not ReadFile(Handle, Buffer, Count, LongWord(result), nil) then
    result := 0;
end;

function FileWrite(Handle: THandle; const Buffer; Count: LongWord): integer;
begin
  if not WriteFile(Handle, Buffer, Count, LongWord(result), nil) then
    result := 0;
end;

function CreateDir(const Dir: TFileName): Boolean;
begin
  result := CreateDirectory(pointer(Dir), nil);
end;

function RemoveDir(const Dir: TFileName): Boolean;
begin
  result := RemoveDirectory(pointer(Dir));
end;

function SafeLoadLibrary(const Filename: TFileName): HMODULE;
var OldMode: UINT;
    FPUControlWord: Word;
begin
  OldMode := SetErrorMode(0);
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;

function ExpandFileName(const FileName: string): string;
var FName: PChar;
    Buffer: array[0..MAX_PATH-1] of Char;
begin
  SetString(result, Buffer, GetFullPathName(pointer(FileName), SizeOf(Buffer),
    Buffer, FName));
end;

function FileExists(const FileName: TFileName): Boolean;
// use GetFileAttributes: much faster than standard FileAge -> FindFirst
var Attr: Integer;
    LastError: Cardinal;
begin
  Attr := Integer(GetFileAttributes(pointer(FileName)));
  if Attr<>-1 then
    Result := Attr and FILE_ATTRIBUTE_DIRECTORY = 0 else begin
    LastError := GetLastError;
    Result := (LastError<>ERROR_FILE_NOT_FOUND) and
              (LastError<>ERROR_PATH_NOT_FOUND) and
              (LastError<>ERROR_INVALID_NAME) and
              // (use FileAge to test SHARE_EXCLUSIVE files)
              ((LastError = ERROR_SHARING_VIOLATION) or (FileAge(FileName)<>-1));
  end;
end;

function FileAge(const FileName: TFileName): integer;
var Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(pointer(FileName), FindData);
  if Handle<>INVALID_HANDLE_VALUE then begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      lastfileagesize := FindData.nFileSizeLow;
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(result).Hi,
        LongRec(result).Lo) then Exit;
    end;
  end;
  result := -1;
end;

function FileSetDate(F: THandle; Age: integer): integer; overload;
var LocalFileTime, FileTime: TFileTime;
begin
  if (f = THandle(-1)) or (Age=0) then result := GetLastError else
    if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
      LocalFileTimeToFileTime(LocalFileTime, FileTime) and
      SetFileTime(f, nil, nil, @FileTime) then
        result := 0 else
        result := GetLastError;
end;

function FileSetDate(const FileName: TFileName; Age: integer): integer; overload;
var f: THandle;
    LocalFileTime, FileTime: TFileTime;
begin
  f := FileOpen(FileName, fmOpenWrite);
  if (f = THandle(-1)) or (Age=0) then result := GetLastError else begin
    if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
      LocalFileTimeToFileTime(LocalFileTime, FileTime) and
      SetFileTime(f, nil, nil, @FileTime) then
        result := 0 else
        result := GetLastError;
    FileClose(f);
  end;
end;

function RenameFile(const OldName, NewName: TFileName): Boolean;
begin
  result := MoveFile(pointer(OldName), pointer(NewName));
end;

function DeleteFile(const FileName: TFileName): Boolean;
begin
  result := Windows.DeleteFile(pointer(FileName));
end;

function DirectoryExists(const Directory: TFileName): Boolean;
var Code: integer;
begin
  Code := GetFileAttributes(pointer(Directory));
  result := (Code<>-1)  and ((FILE_ATTRIBUTE_DIRECTORY and Code)<> 0);
end;


{$else}
// Linux version of the code:

function FileCreate(const FileName: TFileName): integer;
begin
  result := integer(open(pointer(FileName), O_RDWR or O_CREAT or O_TRUNC, FileAccessRights));
end;

function FileOpen(const FileName: TFileName; Mode: LongWord): integer;
const ShareMode: array[0..fmShareDenyNone shr 4] of Byte = (0, F_WRLCK, F_RDLCK, 0);
var FileHandle, Tvar: integer;
    LockVar: TFlock;
    smode: Byte;
begin
  result := -1;
  if FileExists(FileName) and ((Mode and 3) <= fmOpenReadWrite) and
     ((Mode and $F0) <= fmShareDenyNone) then begin
    FileHandle := open(pointer(FileName), (Mode and 3), FileAccessRights);
    if FileHandle = -1 then  Exit;
    smode := Mode and $F0 shr 4;
    if ShareMode[smode]<>0 then begin
      with LockVar do begin
        l_whence := SEEK_SET;
        l_start := 0;
        l_len := 0;
        l_type := ShareMode[smode];
      end;
      Tvar :=  fcntl(FileHandle, F_SETLK, LockVar);
      if Tvar = -1 then begin
        __close(FileHandle);
        Exit;
      end;
    end;
    result := FileHandle;
  end;
end;

procedure FileClose(Handle: integer);
begin
  __close(Handle); // No need to unlock since all locks are released on close.
end;

function FileSeek(Handle, Offset, Origin: integer): integer;
begin
  result :=  __lseek(Handle, Offset, Origin);
end;

function FileRead(Handle: integer; var Buffer; Count: LongWord): integer;
begin
  result := __read(Handle, Buffer, Count);
end;

function FileWrite(Handle: integer; const Buffer; Count: LongWord): integer;
begin
  result := __write(Handle, Buffer, Count);
end;

function CreateDir(const Dir: TFileName): Boolean;
begin
  result := __mkdir(pointer(Dir), mode_t(-1)) = 0;
end;

function SafeLoadLibrary(const FileName: TFileName): HMODULE;
var FPUControlWord: Word;
begin
  asm
    FNSTCW  FPUControlWord
  end;
  try
    Result := LoadLibrary(PChar(Filename));
  finally
    asm
      FNCLEX
      FLDCW FPUControlWord
    end;
  end;
end;

function FileExists(const FileName: TFileName): Boolean;
begin
  result := euidaccess(pointer(FileName), F_OK) = 0;
end;

function FileAge(const FileName: TFileName): integer;
var st: TStatBuf;
begin
  if stat(pointer(FileName), st) = 0 then
    result := st.st_mtime else
    result := -1;
end;

function FileSetDate(const FileName: TFileName; Age: integer): integer;
var ut: TUTimeBuffer;
begin
  result := 0;
  ut.actime := Age;
  ut.modtime := Age;
  if utime(pointer(FileName), @ut) = -1 then
    result := GetLastError;
end;

function RenameFile(const OldName, NewName: TFileName): Boolean;
begin
  result := __rename(pointer(OldName), pointer(NewName)) = 0;
end;

function DeleteFile(const FileName: TFileName): Boolean;
begin
  result := unlink(pointer(FileName))<>-1;
end;

function DirectoryExists(const Directory: TFileName): Boolean;
var st: TStatBuf;
begin
  if stat(pointer(Directory), st) = 0 then
    result := S_ISDIR(st.st_mode) else
    result := False;
end;

{$endif} // end of Linux-specific part

function GetCurrentDir: TFileName;
begin
  GetDir(0, result);
end;

{$ifdef Win32}
function GetModuleName(Module: HMODULE): TFileName;
var tmp: array[byte] of char;
begin
  SetString(Result,tmp,GetModuleFileName(Module,tmp,SizeOf(tmp)));
end;

(* this implementation doesn't release memory -> don't put here
type
  TBrowseInfo = packed record
    hwndOwner: HWND;
    pidlRoot: THandle;
    pszDisplayName: PAnsiChar; { Return display name of item selected. }
    lpszTitle: PAnsiChar; { text to go in the banner over the tree. }
    ulFlags: UINT; { Flags that control the return stuff }
    lpfn: pointer;
    lParam: LPARAM; { extra info that's passed back in callbacks }
    iImage: integer; { output var: where to return the Image index. }
  end;

function SHBrowseForFolder(var bi: TBrowseInfo): THandle; stdcall; external
  'shell32.dll' name 'SHBrowseForFolderA';
function SHGetPathFromIDList(id: THandle; Path: PChar): bool; stdcall; external
  'shell32.dll' name 'SHGetPathFromIDListA';

function SelectDirectory(Handle: integer; const Caption: string;
  var Directory: string): Boolean;
var
 bi:TBrowseInfo;
 id:THandle;
 nm:array[0..MAX_PATH] of Char;
begin
  FillChar(bi, sizeof(bi), 0);
  bi.hwndOwner := Handle;
  bi.lpszTitle := pointer(Caption);
  id := SHBrowseForFolder(bi);
  result := id<>0;
  if not result then exit;
  SHGetPathFromIDList(id, nm);
  Directory := nm;
end;
*)

procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);
var Y,M,D: word;
begin
  DecodeDate(DateTime, Y,M,D);
  with SystemTime do begin
    wYear := Y;
    wMonth := M;
    wDay := D;
    wDayOfWeek := 0;
    DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
  end;
end;

function GetFileVersion(const aFileName: string): Cardinal;
var FileName: array[0..MAX_PATH] of char;
    InfoSize, Wnd: DWORD;
    VerBuf: Pointer;
    FI: PVSFixedFileInfo;
    VerSize: DWORD;
begin
  Result := Cardinal(-1);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  StrCopy(FileName,pointer(AFileName));
  InfoSize := GetFileVersionInfoSize(FileName, Wnd);
  if InfoSize<>0 then begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(FileName, Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result:= FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

{$endif}

function LowerCase(const S: string): string;
procedure Lower(Source, Dest: PChar; L: cardinal);
var Ch: Char; // this sub-call is shorter and faster than 1 plain proc
begin
  repeat
    Ch := Source^;
    if (Ch>='A') and (Ch<='Z') then inc(Ch, 32);
    Dest^ := Ch;
    dec(L);
    inc(Source);
    inc(Dest);
  until L=0;
end;
var L: cardinal;
begin
  L := Length(S);
  SetLength(result, L);
  if L<>0 then
    Lower(pointer(S),pointer(result),L);
end;

function UpperCase(const S: string): string;
{$ifdef UNICODE}
procedure Upper(Source, Dest: PChar; L: cardinal);
var Ch: Char; // this sub-call is shorter and faster than 1 plain proc
begin
  repeat
    Ch := Source^;
    if (Ch>='a') and (Ch<='z') then dec(Ch, 32);
    Dest^ := Ch;
    dec(L);
    inc(Source);
    inc(Dest);
  until L=0;
end;
var L: cardinal;
begin
  L := Length(S);
  SetLength(result, L);
  if L<>0 then
    Upper(pointer(S),pointer(result),L);
end;
{$else}
asm
  push    edi
  push    esi
  push    ebx
  mov     esi,eax               {@S}
  mov     edi,edx               {@Result}
  mov     eax,edx               {@Result}
  xor     edx,edx
  test    esi,esi               {Test for S = NIL}
  jz      @@Setlen              {S = NIL}
  mov     edx,[esi-4]           {Length(S)}
@@SetLen:
  lea     ebx,[edx-4]           {Length(S) - 4}
  call    system.@LStrSetLength {Create Result String}
  mov     edi,[edi]             {@Result}
  add     esi,ebx
  add     edi,ebx
  neg     ebx
  jg      @@Remainder           {Length(S) < 4}
@@Loop:                         {Loop converting 4 Characters per Loop}
  mov     eax,[esi+ebx]
  mov     ecx,eax               {4 Original Bytes}
  or      eax,$80808080         {Set High Bit of each Byte}
  mov     edx,eax               {Comments Below apply to each Byte...}
  sub     eax,$7B7B7B7B         {Set High Bit if Original <= Ord('z')}
  xor     edx,ecx               {80h if Original < 128 else 00h}
  or      eax,$80808080         {Set High Bit}
  sub     eax,$66666666         {Set High Bit if Original >= Ord('a')}
  and     eax,edx               {80h if Orig in 'a'..'z' else 00h}
  shr     eax,2                 {80h > 20h ('a'-'A')}
  sub     ecx,eax               {Clear Bit 5 if Original in 'a'..'z'}
  mov     [edi+ebx], ecx
  add     ebx,4
  jle     @@Loop
@@Remainder:
  sub     ebx,4
  jz      @@Done
@@SmallLoop:                    {Loop converting 1 Character per Loop}
  movzx   eax,byte ptr [esi+ebx+4]
  lea     edx,[eax-'a']
  cmp     edx,'z'-'a'+1
  sbb     ecx,ecx
  and     ecx,$20
  sub     eax,ecx
  mov     [edi+ebx+4],al
  inc     ebx
  jnz     @@SmallLoop
@@Done:
  pop     ebx
  pop     esi
  pop     edi
end;
{$endif}

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: integer;
begin
  if rfIgnoreCase in Flags then begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  result := '';
  while SearchStr<>'' do begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then begin
      result := result + NewStr;
      Break;
    end;
    result := result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then begin
      result := result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function CompareText(const S1, S2: string): integer; 
{$ifdef UNICODE}
asm // John O'Harrow version
        TEST   EAX, EAX
        JNZ    @@CheckS2
        TEST   EDX, EDX
        JZ     @@Ret
        MOV    EAX, [EDX-4]
        NEG    EAX
@@Ret:  RET
@@CheckS2:
        TEST   EDX, EDX
        JNZ    @@Compare
        MOV    EAX, [EAX-4]
        RET
@@Compare:
        PUSH   EBX
        PUSH   EBP
        PUSH   ESI
        PUSH   0
        PUSH   0
        CMP    WORD PTR [EAX-10],2
        JE     @@S1IsUnicode
        PUSH   EDX
        MOV    EDX,EAX
        LEA    EAX,[ESP+4]
        CALL   System.@UStrFromLStr
        POP    EDX
        MOV    EAX,[ESP]
@@S1IsUnicode:
        CMP    WORD PTR [EDX-10],2
        JE     @@S2IsUnicode
        PUSH   EAX
        LEA    EAX,[ESP+8]
        CALL   System.@UStrFromLStr
        POP    EAX
        MOV    EDX,[ESP+4]
@@S2IsUnicode:
        MOV    EBP, [EAX-4]     // length(S1)
        MOV    EBX, [EDX-4]     // length(S2)
        SUB    EBP, EBX         // Result if All Compared Characters Match
        SBB    ECX, ECX
        AND    ECX, EBP
        ADD    ECX, EBX         // min(length(S1),length(S2)) = Compare Length
        LEA    ESI, [EAX+ECX*2] // Last Compare Position in S1
        ADD    EDX, ECX         // Last Compare Position in S2
        ADD    EDX, ECX         // Last Compare Position in S2
        NEG    ECX
        JZ     @@SetResult      // Exit if Smallest Length = 0
@@Loop:                         // Load Next 2 Chars from S1 and S2
                                // May Include Null Terminator}
        MOV    EAX, [ESI+ECX*2]
        MOV    EBX, [EDX+ECX*2]
        CMP    EAX,EBX
        JE     @@Next           // Next 2 Chars Match
        CMP    AX,BX
        JE     @@SecondPair     // First Char Matches
        AND    EAX,$0000FFFF
        AND    EBX,$0000FFFF
        CMP    EAX, 'a'
        JL     @@UC1
        CMP    EAX, 'z'
        JG     @@UC1
        SUB    EAX, 'a'-'A'
@@UC1:  CMP    EBX, 'a'
        JL     @@UC2
        CMP    EBX, 'z'
        JG     @@UC2
        SUB    EBX, 'a'-'A'
@@UC2:  SUB    EAX,EBX          // Compare Both Uppercase Chars
        JNE    @@Done           // Exit with Result in EAX if Not Equal
        MOV    EAX, [ESI+ECX*2] // Reload Same 2 Chars from S1
        MOV    EBX, [EDX+ECX*2] // Reload Same 2 Chars from S2
        AND    EAX,$FFFF0000
        AND    EBX,$FFFF0000
        CMP    EAX,EBX
        JE     @@Next           // Second Char Matches
@@SecondPair:
        SHR    EAX, 16
        SHR    EBX, 16
        CMP    EAX, 'a'
        JL     @@UC3
        CMP    EAX, 'z'
        JG     @@UC3
        SUB    EAX, 'a'-'A'
@@UC3:  CMP    EBX, 'a'
        JL     @@UC4
        CMP    EBX, 'z'
        JG     @@UC4
        SUB    EBX, 'a'-'A'
@@UC4:  SUB    EAX,EBX           // Compare Both Uppercase Chars
        JNE    @@Done           // Exit with Result in EAX if Not Equal
@@Next: ADD    ECX, 2
        JL     @@Loop           // Loop until All required Chars Compared
@@SetResult:
        MOV    EAX,EBP          // All Matched, Set Result from Lengths
@@Done: MOV    ECX,ESP
        MOV    EDX,[ECX]
        OR     EDX,[ECX + 4]
        JZ     @@NoClear
        PUSH   EAX
        MOV    EAX,ECX
        MOV    EDX,2
        CALL   System.@LStrArrayClr
        POP    EAX
@@NoClear:
        ADD    ESP,8
        POP    ESI
        POP    EBP
        POP    EBX
end;
{$else}
asm // fast version, optimized for 7 bits Ansi uppercase
         test  eax,eax
         jz    @nil1
         test  edx,edx
         jz   @nil2
         push  edi
         push  ebx
         xor   edi,edi
         mov   ebx,[eax-4]
         mov   ecx,ebx
         sub   ebx,[edx-4]
         adc   edi,-1
         push  ebx    // save length(S1)-length(S2)
         and   ebx,edi
         mov   edi,eax
         sub   ebx,ecx  //ebx := -min(Length(s1),Length(s2))
         jge   @len
@lenok:  sub   edi,ebx
         sub   edx,ebx
@loop:   mov   eax,[ebx+edi]
         mov   ecx,[ebx+edx]
         xor   eax,ecx
         jne   @differ
@same:   add   ebx,4
         jl    @loop
@len:    pop   eax
         pop   ebx
         pop   edi
         ret
@loop2:  mov   eax,[ebx+edi]
         mov   ecx,[ebx+edx]
         xor   eax,ecx
         je    @same
@differ: test  eax,$DFDFDFDF  //$00 or $20
         jnz   @find
         add   eax,eax        //$00 or $40
         add   eax,eax        //$00 or $80
         test  eax,ecx
         jnz   @find
         and   ecx,$5F5F5F5F  //$41..$5A
         add   ecx,$3F3F3F3F  //$80..$99
         and   ecx,$7F7F7F7F  //$00..$19
         add   ecx,$66666666  //$66..$7F
         test  ecx,eax
         jnz   @find
         add   ebx,4
         jl    @loop2
@len2:   pop   eax
         pop   ebx
         pop   edi
         ret
@nil2:   mov   eax,[eax-4]
         ret
@nil1:   test  edx,edx
         jz    @nil0
         sub   eax,[edx-4]
@nil0:   ret
@loop3:  add   ebx, 1
         jge   @len2
@find:   movzx eax,byte ptr [ebx+edi]
         movzx ecx,byte ptr [ebx+edx]
         sub   eax,'a'
         sub   ecx,'a'
         cmp   al,'z'-'a'
         ja    @upa
         sub   eax,'a'-'A'
@upa:    cmp   cl,'z'-'a'
         ja    @upc
         sub   ecx,'a'-'A'
@upc:    sub   eax,ecx
         jz    @loop3
@found:  pop   ecx
         pop   ebx
         pop   edi
end;
{$endif}

function SameText(const s1, s2: string): boolean; 
asm
    cmp  eax,edx
    jz   @1
    or   eax,eax
    jz   @2
    or   edx,edx
    jz   @3
    mov  ecx,[eax-4]
    cmp  ecx,[edx-4]
    jne  @3 // length must be the same
    call CompareText // compare chars inside 
    test eax,eax
    jnz  @3
@1: mov  al,1
@2: ret
@3: xor  eax,eax
end;

function LastDelimiter(const Delimiters, S: string): integer;
begin
  result := Length(S);
  while result>0 do
    if (S[result]<>#0) and (Pos(S[result],Delimiters)=0) then
      dec(result) else
      break;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var i: integer;
begin
  i := LastDelimiter('.' + PathDelim{$ifndef Linux}+DriveDelim{$endif},Filename);
  if (i = 0) or (FileName[i]<>'.') then i := MaxInt;
  result := Copy(FileName,1,i-1) + Extension;
end;

function ExtractFileName(const FileName: string): string;
var i: integer;
begin
  i := LastDelimiter(PathDelim{$ifndef Linux}+DriveDelim{$endif}, FileName);
  result := Copy(FileName,i+1,MaxInt);
end;

function ExtractFileExt(const FileName: string): string;
var i: integer;
begin
  i := LastDelimiter('.' + PathDelim{$ifndef Linux}+DriveDelim{$endif}, FileName);
  if (i>0) and (FileName[i]='.') then
    result := Copy(FileName,i,MaxInt) else
    result := '';
end;

function ExtractFilePath(const FileName: string): string;
var i: integer;
begin
  i := LastDelimiter(PathDelim{$ifndef Linux}+DriveDelim{$endif}, FileName);
  result := Copy(FileName,1,i);
end;

function ExtractFileDir(const FileName: string): string;
var i: integer;
begin
  i := LastDelimiter(PathDelim{$ifndef Linux}+DriveDelim{$endif}, Filename);
  if (i>1) and (FileName[i]=PathDelim) and (not (FileName[i-1]
    in [PathDelim{$ifndef Linux},DriveDelim{$endif}])) then
    dec(i);
  result := Copy(FileName,1,i);
end;

function IsPathDelimiter(const S: string; Index: integer): Boolean;
begin
  dec(Index);
  result := (cardinal(Index)<cardinal(Length(S))) and (S[Index+1]=PathDelim);
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  result := S;
  if not IsPathDelimiter(result,Length(result)) then
    result := result + PathDelim;
end;

function FindMatchingFile(var F: TSearchRec): integer;
{$ifdef Win32}
var LocalFileTime: TFileTime;
begin
  with F do begin
    while FindData.dwFileAttributes and ExcludeAttr<>0 do
      if not FindNextFile(FindHandle, FindData) then begin
        result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  result := 0;
end;
{$else}
var PtrDirEnt: PDirEnt;
  Scratch: TDirEnt;
  StatBuf: TStatBuf;
  LinkStatBuf: TStatBuf;
  FName: string;
  Attr: integer;
  Mode: mode_t;
begin
  result := -1;
  PtrDirEnt := nil;
  if readdir_r(F.FindHandle, @Scratch, PtrDirEnt)<>0 then
    Exit;
  while PtrDirEnt<>nil do begin
    if fnmatch(PChar(F.Pattern), PtrDirEnt.d_name, 0) = 0 then begin
      FName := F.PathOnly + string(PtrDirEnt.d_name);
      if lstat(pointer(FName), StatBuf) = 0 then begin
        Attr := 0;
        Mode := StatBuf.st_mode;
        if S_ISDIR(Mode) then
          Attr := Attr or faDirectory
        else
        if not S_ISREG(Mode) then begin
          if S_ISLNK(Mode) then begin
            Attr := Attr or faSymLink;
            if (stat(pointer(FName), LinkStatBuf) = 0) and S_ISDIR(LinkStatBuf.st_mode) then
                Attr := Attr or faDirectory
          end;
          Attr := Attr or faSysFile;
        end;
        if (PtrDirEnt.d_name[0] = '.') and (PtrDirEnt.d_name[1]<>#0) then begin
          if not ((PtrDirEnt.d_name[1] = '.') and (PtrDirEnt.d_name[2] = #0)) then
            Attr := Attr or faHidden;
        end;
        if euidaccess(pointer(FName), W_OK)<>0 then
          Attr := Attr or faReadOnly;
        if Attr and F.ExcludeAttr = 0 then begin
          F.Size := StatBuf.st_size;
          F.Attr := Attr;
          F.Mode := StatBuf.st_mode;
          F.Name := PtrDirEnt.d_name;
          F.Time := StatBuf.st_mtime;
          result := 0;
          Break;
        end;
      end;
    end;
    result := -1;
    if readdir_r(F.FindHandle, @Scratch, PtrDirEnt)<>0 then
      Break;
  end // end of While
end;
{$endif}

function FindFirst(const Path: string; Attr: integer;
  var  F: TSearchRec): integer;
const faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
{$ifdef Win32}
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFile(pointer(Path), F.FindData);
  if F.FindHandle<>INVALID_HANDLE_VALUE then begin
    result := FindMatchingFile(F);
    if result<>0 then FindClose(F);
  end else
    result := GetLastError;
end;
{$else}
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.PathOnly := ExtractFilePath(Path);
  F.Pattern := ExtractFileName(Path);
  if F.PathOnly = '' then
    F.PathOnly := IncludeTrailingPathDelimiter(GetCurrentDir);
  F.FindHandle := opendir(pointer(F.PathOnly));
  if F.FindHandle<>nil then begin
    result := FindMatchingFile(F);
    if result<>0 then
      FindClose(F);
  end else
    result :=  GetLastError;
end;
{$endif}

function FindNext(var F: TSearchRec): integer;
begin
{$ifdef Win32}
  if FindNextFile(F.FindHandle, F.FindData) then
    result := FindMatchingFile(F) else
    result := GetLastError;
{$else}
  result := FindMatchingFile(F);
{$endif}
end;

procedure FindClose(var F: TSearchRec);
begin
{$ifdef Win32}
  if F.FindHandle<>INVALID_HANDLE_VALUE then begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
{$else}
  if F.FindHandle<>nil then begin
    closedir(F.FindHandle);
    F.FindHandle := nil;
  end;
{$endif}
end;

function Now: TDateTime;
{$ifdef Win32}
var SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;
{$else}
var T: TTime_T;
    TV: TTimeVal;
    UT: TUnixTime;
begin
  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(@T, UT);
  Result := EncodeDate(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday) +
    EncodeTime(UT.tm_hour, UT.tm_min, UT.tm_sec, TV.tv_usec div 1000);
end;
{$endif}

function FileGetDate(Handle: THandle): Integer;
{$ifdef Win32}
var FileTime, LocalFileTime: TFileTime;
begin
  if GetFileTime(Handle, nil, nil, @FileTime) and
    FileTimeToLocalFileTime(FileTime, LocalFileTime) and
    FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
      LongRec(Result).Lo) then Exit;
  Result := -1;
end;
{$else}
var
  st: TStatBuf;
begin
  if fstat(Handle, st) = 0 then
    Result := st.st_mtime else
    Result := -1;
end;
{$endif}


function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime; overload;
begin
  if (Hour < HoursPerDay) and (Min < MinsPerHour) and (Sec < SecsPerMin) and
     (MSec < MSecsPerSec) then
    result := (Hour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             Min * (SecsPerMin * MSecsPerSec) + Sec * MSecsPerSec + MSec)
             / MSecsPerDay else
    result := 0;
end;

function EncodeTime(Hour, Min, Sec, MSec: cardinal): TDateTime; overload;
begin
  if (Hour < HoursPerDay) and (Min < MinsPerHour) and (Sec < SecsPerMin) and
     (MSec < MSecsPerSec) then
    result := (Hour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             Min * (SecsPerMin * MSecsPerSec) + Sec * MSecsPerSec + MSec)
             / MSecsPerDay else
    result := 0;
end;

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
asm // faster version by AB
     push  eax
     mov   ecx,MSecsPerDay
     fld   datetime
     fmul  fmsecsperday
     sub   esp,8
     fistp qword ptr [esp]
     pop   eax
     pop   edx
     or    edx,edx
     jns   @@1
     neg   edx
     neg   eax
     sbb   edx,0
     div   ecx
     neg   eax
     jmp   @@2
@@1: div   ecx
@@2: add   eax,datedelta
     pop   ecx
     mov   [ecx].ttimestamp.time,edx
     mov   [ecx].ttimestamp.date,eax
end;

function DateTimeToMS(const DateTime: TDateTime): integer;
asm // faster version by AB
     fld   datetime
     fmul  fmsecsperday
     sub   esp,8
     fistp qword ptr [esp]
     pop   eax
     pop   edx
     or    edx,edx
     mov   ecx,MSecsPerDay
     jns   @@1
     neg   edx
     neg   eax
     sbb   edx,0
@@1: div   ecx
     mov   eax,edx // we only need ttimestamp.time = Milli Seconds count
end;

procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
begin // faster asm version by AB
  asm // inside a begin...end block -> copy all parameters to the stack
     fld   datetime
     fmul  fmsecsperday
     sub   esp,8
     fistp qword ptr [esp]
     pop eax
     pop edx
     or    edx,edx
     mov   ecx,MSecsPerDay
     jns   @@1
     neg   edx
     neg   eax
     sbb   edx,0
@@1: div   ecx
     mov ecx,SecsPerMin*MSecsPerSec
     mov eax,edx
     shr edx,16 // dx:ax = time
     div cx  // (dx:ax) div cx -> dx=remainder=MSecCount, ax=quotient=MinCount
     mov cl,MinsPerHour // =60 -> byte division
     div cl  // ax div cl -> ah=remainder=Min, al=quotient=Hour
     mov ecx,Min
     mov [ecx],ah
     inc ecx
     xor ah,ah
     mov [ecx],ah // make word value
     mov ecx,Hour
     mov [ecx],ax
     mov eax,edx
     xor edx,edx
     mov ecx,MSecsPerSec
     div cx // (dx:ax) div cx -> dx=remainder=MSec ax=quotient=Sec
     mov ecx,Sec
     mov [ecx],ax
     mov ecx,MSec
     mov [ecx],dx
  end;
end;

{$ifdef Win32}

function DateTimeToFileDate(DateTime: TDateTime): integer;
var Year, Month, Day: word;
    Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  if (Year < 1980) or (Year > 2107) then result := 0 else begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    LongRec(result).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
    LongRec(result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
  end;
end;

function FileDateToDateTime(FileDate: integer): TDateTime;
begin
  result := EncodeDate(
      LongRec(FileDate).Hi shr 9 + 1980, LongRec(FileDate).Hi shr 5 and 15,
      LongRec(FileDate).Hi and 31) + EncodeTime(
      LongRec(FileDate).Lo shr 11, LongRec(FileDate).Lo shr 5 and 63,
      LongRec(FileDate).Lo and 31 shl 1, 0);
end;

{$else}

function DateTimeToFileDate(DateTime: TDateTime): integer;
var Year, Month, Day: integer;
    Hour, Min, Sec, MSec: Word;
var tm: TUnixTime;
begin
  DecodeDate(DateTime, Year, Month, Day);
  { Valid range for 32 bit Unix time_t:  1970 through 2038  }
  if (Year < 1970) or (Year > 2038) then
    result := 0 else begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    FillChar(tm, sizeof(tm), 0);
    with tm do begin
      tm_sec := Sec;
      tm_min := Min;
      tm_hour := Hour;
      tm_mday := Day;
      tm_mon  := Month - 1;
      tm_year := Year - 1900;
      tm_isdst := -1;
    end;
    result := mktime(tm);
  end;
end;

function FileDateToDateTime(FileDate: integer): TDateTime;
var UT: TUnixTime;
begin
  localtime_r(@FileDate, UT);
  result := EncodeDate(UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday) +
              EncodeTime(UT.tm_hour, UT.tm_min, UT.tm_sec, 0);
end;
{$endif}

function SystemTimeToDosDateTime(const SystemTime: TSystemTime): integer;
begin
  with SystemTime do begin
    LongRec(result).Lo := (wSecond shr 1) or (wMinute shl 5) or (wHour shl 11);
    LongRec(result).Hi := wDay or (wMonth shl 5) or ((wYear - 1980) shl 9);
  end;
end;

function NowToDosDateTime: integer;
var SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  result := SystemTimeToDosDateTime(SystemTime);
end;

procedure DosDateTimeToSystemTime(DosDateTime: integer; out SystemTime: TSystemTime);
// warning: wDayOfWeek is not set
begin
  with SystemTime do begin
    wSecond := (DosDateTime and 31)*2; DosDateTime := DosDateTime shr 5;
    wMinute := DosDateTime and 63;     DosDateTime := DosDateTime shr 6;
    wHour   := DosDateTime and 31;     DosDateTime := DosDateTime shr 5;
    wDay    := DosDateTime and 31;     DosDateTime := DosDateTime shr 5;
    wMonth  := DosDateTime and 15;     DosDateTime := DosDateTime shr 4;
    wYear   := DosDateTime and 127+1980;
  end;
end;

procedure YearToPAnsiChar(Y: Word; P: PAnsiChar);
asm
  mov cl,100
  div cl // ah=remainder=Y mod 100, al=quotient=Year div 100
  movzx ecx,al // al=quotient=Y div 100
  mov cx,word ptr [TwoDigitLookup+ecx*2]
  mov [edx],cx
  movzx ecx,ah // ah=remainder=Y mod 100
  mov cx,word ptr [TwoDigitLookup+ecx*2]
  mov [edx+2],cx
end;

var
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their ASCII equivalence
  TwoDigitLookupW: packed array[0..99] of word absolute TwoDigitLookup;

procedure DateTimeToPAnsiChar(DateTime: TDateTime; dest: PAnsiChar);
var Y,M,D: word;  // 'YYYY/MM/DD hh:mm:ss'
    H,MI,S,MS: word;
begin
  DecodeDate(DateTime, Y,M,D);
  DecodeTime(DateTime, H,MI,S,MS);
  if (Y>2100) or (H>23) or (D>31) then exit; // M,D, MI,S,MS are always good
  YearToPAnsiChar(Y,Dest);
  Dest[4] := '/';
  pWord(Dest+5)^  := TwoDigitLookupW[M];
  Dest[7] := '/';
  pWord(Dest+8)^  := TwoDigitLookupW[D];
  Dest[10] := ' ';
  pWord(Dest+11)^  := TwoDigitLookupW[H];
  Dest[13] := ':';
  pWord(Dest+14)^ := TwoDigitLookupW[MI];
  Dest[16] := ':';
  pWord(Dest+17)^ := TwoDigitLookupW[S];
end;

function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
var Y,M,D, H,MI,SS: cardinal;
begin
  if length(s)<>19 then begin
    result := false;
    exit;
  end;
  Y := ord(S[1])*1000+ord(S[2])*100+ord(S[3])*10+ord(S[4])-(48+480+4800+48000);
  M := ord(S[6])*10+ord(S[7])-(48+480);
  D := ord(S[9])*10+ord(S[10])-(48+480);
  H := ord(S[12])*10+ord(S[13])-(48+480);
  MI := ord(S[15])*10+ord(S[16])-(48+480);
  SS := ord(S[18])*10+ord(S[19])-(48+480);
  result := (Y<=2100) and (Y>=1980) and (M in [1..12]) and (D<=MonthDays[true][M]) and
    (D<>0) and (H<=23) and (MI<=59) and (SS<=59);
  if result then
    Value := EncodeDate(Y,M,D)+EncodeTime(H,MI,SS,0);
end;

function DateTimeToStr(const DateTime: TDateTime): string;
// 'YYYY/MM/DD hh:mm:ss'
{$ifdef UNICODE}
var tmp: string[23];
begin
  tmp[0] := #19;
  DateTimeToPAnsiChar(DateTime,@tmp[1]);
  result := tmp;
end;
{$else}
begin
  SetLength(result,19);
  DateTimeToPAnsiChar(DateTime, pointer(result));
end;
{$endif}

procedure FreeAndNil(var Obj);
var Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

function AllocMem(Size: integer): pointer;
begin
  GetMem(Result,Size);
  FillChar(Result^,Size,0);
end;

function CompareStr(const S1, S2: string): Integer; 
asm
     push  esi
     push  edi
     mov   esi,eax
     mov   edi,edx
     or    eax,eax
     je    @@1
     mov   eax,[eax-4]
@@1: or    edx,edx
     je    @@2
     mov   edx,[edx-4]
@@2: mov   ecx,eax
     cmp   ecx,edx
     jbe   @@3
     mov   ecx,edx
@@3: // eax=length(s1), edx=length(s2), ecx=min(eax,edx)
     cmp   ecx,ecx
{$ifdef UNICODE}
     repe  cmpsw
     je    @@4
     movzx eax,word ptr [esi-2]
     movzx edx,word ptr [edi-2]
{$else}
     repe  cmpsb
     je    @@4
     movzx eax,byte ptr [esi-1]
     movzx edx,byte ptr [edi-1]
{$endif}
@@4: sub   eax,edx
     pop   edi
     pop   esi
end;

function CompareMem(P1, P2: Pointer; Length: integer): Boolean; 
asm
  push  ebx
  sub   ecx, 8
  jl    @@Small
  mov   ebx, [eax]         {Compare First 4 Bytes}
  cmp   ebx, [edx]
  jne   @@False
  lea   ebx, [eax+ecx]     {Compare Last 8 Bytes}
  add   edx, ecx
  mov   eax, [ebx]
  cmp   eax, [edx]
  jne   @@False
  mov   eax, [ebx+4]
  cmp   eax, [edx+4]
  jne   @@False
  sub   ecx, 4
  jle   @@True             {All Bytes already Compared}
  neg   ecx                {-(Length-12)}
  add   ecx, ebx           {DWORD Align Reads}
  and   ecx, -4
  sub   ecx, ebx
@@LargeLoop:               {Compare 8 Bytes per Loop}
  mov   eax, [ebx+ecx]
  cmp   eax, [edx+ecx]
  jne   @@False
  mov   eax, [ebx+ecx+4]
  cmp   eax, [edx+ecx+4]
  jne   @@False
  add   ecx, 8
  jl    @@LargeLoop
@@True:
  mov   eax, 1
  pop   ebx
  ret
@@Small:
  add   ecx, 8
  jle   @@True             {Length <= 0}
@@SmallLoop:
  mov   bl, [eax]
  cmp   bl, [edx]
  jne   @@False
  inc   eax
  inc   edx
  dec   ecx
  jnz   @@SmallLoop
  jmp   @@True
@@False:
  xor   eax, eax
  pop   ebx
end;

function Format(const Format: string; const Args: array of const): string;
// supported: %% %s %d %x %.prec? %index:?
var i,j, c, L: integer;
    decim: string;
begin
  if high(Args)<0 then begin
    result := Format;
    exit;
  end;
  result := '';
  L := length(Format);
  if L=0 then exit;
  i := 1;
  c := 0;
  while (i<=L) do begin
    j := i;
    while (i<=L) and (Format[i]<>'%') do inc(i);
    case i-j of
      0: ;
      1: AddCharLS(result,Format[j]);
      else result := result+copy(Format,j,i-j);
    end;
    inc(i);
    if i>L then break;
    if (ord(Format[i]) in [ord('0')..ord('9')]) and (i<L) and
       (Format[i+1]=':') then begin
      c := ord(Format[i])-48;  // Format('%d %d %d %0:d %d',[1,2,3,4]) = '1 2 3 1 2'
      inc(i,2);
      if i>L then break;
    end;
    if Format[i]='%' then         // Format('%%') = '%'
      AddCharLS(result,'%') else  // Format('%.3d',[4]) = '004':
    if (Format[i]='.') and (i+2<=L) and (c<=high(Args)) and
       (ord(Format[i+1]) in [ord('1')..ord('9')]) and
       (ord(Format[i+2]) in [ord('d'),ord('x'),ord('p')]) and
       (Args[c].VType=vtInteger) then begin
      j := Args[c].VInteger;
      if Format[i+2]='d' then
        decim := IntToStr(j) else
        decim := IntToHex(j,ord(Format[i+1])-49);
      for j := length(decim) to ord(Format[i+1])-49 do
        decim := '0'+decim;
      result := result+decim;
      inc(c);
      inc(i,2);
    end else
    if c<=high(Args) then begin
      with Args[c] do
      case Format[i] of
      's': case VType of
        vtString:     result := result+VString^;
        vtAnsiString: result := result+AnsiString(VAnsiString);
{$ifdef UNICODE}
        vtUnicodeString: result := result+UnicodeString(VUnicodeString);
{$endif}vtPChar:     result := result+VPChar;
        vtChar:      result := result+VChar;
        vtPWideChar: result := result+VPWideChar;
        vtWideChar:  result := result+VWideChar;
      end;
{      'g','f','n','m': case VType of
      vtExtended: begin
         str(VExtended^,decim);
         result := result+decim;
       end;
       vtCurrency: begin
         str(VCurrency^,decim);
         result := result+decim;
       end;
       end;  // add 3kb to the .exe -> use str() and %s parameter }
      'd': if VType=vtInteger then
             result := result+IntToStr(VInteger) else
           if VType=vtInt64 then
             result := result+IntToStr(VInt64^);
      'x','p': if VType in [vtInteger,vtPointer] then
        result := result+IntToHex(VInteger,8);
      end;
      inc(c);
    end;
    inc(i);
  end;
end;


{ Exception }

constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
//{$ifdef Win32}  MsgBox(FMessage); {$endif}
end;

constructor Exception.CreateFmt(const Msg: string; const Args: array of const);
begin
  Create(Format(Msg, Args));
end;

procedure ErrorHandler(ErrorCode: integer; ErrorAddr: Pointer);
begin
  raise Exception.CreateFmt('Error %d at %x',[ErrorCode,ErrorAddr])
    at ErrorAddr;
end;

procedure OutOfMemoryError;
begin
  ErrorHandler(203,nil);
end;

function SysErrorMessage(ErrorCode: Integer): string;
var Buffer: array[0..255] of Char;
{$IFDEF WIN32}
var
  Len: Integer;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer,
    SizeOf(Buffer), nil);
  while (Len > 0) and (ord(Buffer[Len-1]) in [0..32,ord('.')]) do Dec(Len);
  SetString(Result, Buffer, Len);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := strerror_r(ErrorCode, Buffer, sizeof(Buffer));
end;
{$ENDIF}

procedure RaiseLastOSError;
var LastError: Integer;
    Error: EOSError;
begin
  LastError := GetLastError;
  if LastError=0 then
    exit;
  Error := EOSError.CreateFmt('OSError %d (%s)', [LastError, SysErrorMessage(LastError)]);
  Error.ErrorCode := LastError;
  raise Error;
end;

procedure RaiseAssertException(const E: Exception; const ErrorAddr, ErrorStack: Pointer);
asm
  mov esp,ecx
  mov [esp],edx
  mov ebp,[ebp]
  jmp System.@RaiseExcept
end;

function GetExceptionClass(P: PExceptionRecord): ExceptClass;
begin
  case P^.ExceptionCode of
  STATUS_ACCESS_VIOLATION: result := EAccessViolation;
  STATUS_INTEGER_DIVIDE_BY_ZERO, STATUS_FLOAT_DIVIDE_BY_ZERO: result := EDivByZero;
  else result := EExternalException;
  end;
end;

function GetExceptionObject(P: PExceptionRecord): Exception;
begin
  result := GetExceptionClass(P).CreateFmt('Exception %x',[P^.Exceptioncode]);
end;

procedure AssertErrorHandler(const aMessage, aFilename: string;
  aLineNumber: integer; aErrorAddr: Pointer);
var E: Exception;
begin
  E := EAssertionFailed.CreateFmt('%s in %s (%d) at %x',
    [aMessage,aFileName,aLineNumber,aErrorAddr]);
  RaiseAssertException(E, aErrorAddr, PAnsiChar(@aErrorAddr)+4);
end;

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); far;
begin
  if ExceptObject.InheritsFrom(Exception) then
  with Exception(ExceptObject) do
  if FMessage<>'' then begin
{$ifdef Win32}
    MessageBox(0,pointer(FMessage),nil,MB_OK or MB_ICONSTOP or MB_TASKMODAL);
{$else}
    if TTextRec(ErrOutput).Mode = fmOutput then
      Flush(ErrOutput);
    __write(STDERR_FILENO, pointer(FMessage), length(FMessage));
{$endif}
  end else begin
    ErrorAddr := ExceptAddr;
    Halt(230);
  end;
end;

{$ifdef Win32}
procedure InitPlatformId;
var OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    with OSVersionInfo do begin
      Win32Platform := dwPlatformId;
      Win32MajorVersion := dwMajorVersion;
      Win32MinorVersion := dwMinorVersion;
      if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        Win32BuildNumber := dwBuildNumber and $FFFF else
        Win32BuildNumber := dwBuildNumber;
      Win32CSDVersion := szCSDVersion;
    end;
end;

function InternalGetDiskSpace(Drive: longint;
  var TotalSpace, FreeSpaceAvailable: Int64): Bool;
var RootPtr: PChar;
    tmp: array[0..3] of Char;
begin
  if Drive>0 then begin
    tmp[0] := char(Drive+ord('A'));
    tmp[1] := ':';
    tmp[2] := '\';
    tmp[3] := #0;
    RootPtr := @tmp;
  end else
    RootPtr := nil;
  Result := GetDiskFreeSpaceEx(RootPtr, FreeSpaceAvailable, TotalSpace, nil);
end;

function DiskFree(Drive: Byte): Int64;
var TotalSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, TotalSpace, Result) then
    Result := -1;
end;

function DiskSize(Drive: Byte): Int64;
var FreeSpace: Int64;
begin
  if not InternalGetDiskSpace(Drive, Result, FreeSpace) then
    Result := -1;
end;
{$endif}

{$IFDEF WIN32}
procedure Sleep; external kernel32 name 'Sleep'; stdcall;
{$ENDIF}
{$IFDEF LINUX}
procedure Sleep(milliseconds: Cardinal);
begin
  usleep(milliseconds * 1000);  // usleep is in microseconds
end;
{$ENDIF}

function AnsiUpperCase(const S: string): string;
{$IFDEF WIN32}
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PChar(pointer(S)), Len);
  if Len > 0 then CharUpperBuff(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := UpperCase(S);
end;
{$ENDIF}

function AnsiLowerCase(const S: string): string;
{$IFDEF WIN32}
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PChar(pointer(S)), Len);
  if Len > 0 then CharLowerBuff(Pointer(Result), Len);
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := LowerCase(S);
end;
{$ENDIF}

function AnsiCompareStr(const S1, S2: string): Integer;
begin
{$IFDEF WIN32}
  Result := CompareString(LOCALE_USER_DEFAULT, 0, pointer(S1), Length(S1),
    pointer(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF LINUX}
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := strcoll(PChar(S1), PChar(S2));
{$ENDIF}
end;

function AnsiSameStr(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareStr(S1, S2) = 0;
end;

function AnsiCompareText(const S1, S2: string): Integer;
begin
{$IFDEF WIN32}
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, pointer(S1),
    Length(S1), pointer(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF LINUX}
  Result := WideCompareText(S1, S2);
{$ENDIF}
end;

function AnsiSameText(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;


initialization
  InitPlatformId; // always called once
  ErrorProc := @ErrorHandler;
  AssertErrorProc := @AssertErrorHandler;
  ExceptProc := @ExceptHandler;
  ExceptionClass := Exception; 
  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;

finalization
  AssertErrorProc := nil;
  ErrorProc := nil;
  ExceptProc := nil;
end.
