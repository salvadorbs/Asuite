/// commons types, functions and tables for fast data processing
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectCommons;

(*
    This file is part of SynProject.

    Synopse SynProject. Copyright (C) 2008-2011 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SynProject is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or (at
    your option) any later version.

    SynProject is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SynProject. If not, see <http://www.gnu.org/licenses/>.

*)

{
  Contains some commons types, functions and tables:
   type TIntegerDynArray TStringDynArray TNormToUpper TTwoDigit
   PosEx() AnsiPosEx() StringReplaceAll() ValAt()
   FastFindInteger[Index]()
   AddInteger() AddString() IdemPChar() WinAnsiToUnicodeBuffer()
   NormToLower[] NormToUpper[] TwoDigitLookup[] HexChars[]

  Version history:
  1.0 20080708 AB Initial release
  1.1 20080820 AB FastCodeCPUID + FastMove + FastFillChar units integration
  1.2 20081201 AB code review and Delphi 2009 adaptation
  1.3 20090101 AB Linux compatibility added
  1.4 20090202 AB new System/SysUtils uses fastcode -> no redirection anymore
                  FastCode unit now contains only some usefull types/functions
  1.9 20101015 AB
}

{ $D-,L-}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

{$ifndef ENHANCEDRTL}
  {$define NOENHANCEDRTL}
{$endif}

interface


uses
  SysUtils,
  Classes,
{$ifdef Win32}
  Windows;
{$else}
  LibC, Types;
{$endif}

{$ifdef NOENHANCEDRTL}
type
  PNormTable = ^TNormTable;
  TNormTable = packed array[char] of char;
  PNormTableByte = ^TNormTableByte;
  TNormTableByte = packed array[byte] of byte;

var
  // values locked to WinAnsi, with only A..Z ('Å'->'A' e.g.)
  NormToLower,                    // for Lower*
  NormToUpper: TNormTable;        // for Upper*
  NormToLowerByte: TNormTableByte absolute NormToLower;
  NormToUpperByte: TNormTableByte absolute NormToUpper;

type
  TTwoDigit = packed array[1..2] of char;

const
  TwoDigitLookup: packed array[0..99] of TTwoDigit =
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

/// very fast copy with Table-case transform
procedure TabledPCharCpy(src, dest: pAnsiChar; Count: integer; var Table: TNormTable);

{$endif}

/// this version will use NormToUpper[]
function UpperCase(const S: string): string;

/// Faster Equivalent of Delphi 7 StrUtils.PosEx
function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;

/// fast PosEx, with ignoreCase (use NormToUpper)
function AnsiPosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;

/// fast replacement of StringReplace(S, OldPattern, NewPattern,[rfReplaceAll]);
function StringReplaceAll(const S, OldPattern, NewPattern: AnsiString): AnsiString;

/// ValAt('un,deux',1)='deux'
function ValAt(const value: AnsiString; index: integer; charLimit: AnsiChar = ','): AnsiString;

/// ValAt('un,deux',1,','res) -> res='deux'
procedure ValAtPChar(pc: pAnsiChar; index: integer; charLimit: AnsiChar; var result: AnsiString);

type
  /// wrapper to a dynamic array of integer
  TIntegerDynArray = array of integer;
  /// wrapper to a dynamic array of string
  TStringDynArray = array of AnsiString;

  PIntegers = ^TIntegers;
  /// old-fashion object to store a dynamic array of integer
  // - old-fashion objects are stored as record -> don't need to call free
  // - memory is allocated by chunk: it's more efficient than plain
  // TIntegerDynArray to store a growing array of integer
  // - use internaly FastFindInteger*(), AddInteger(), etc...
  TIntegers = object
    Value: TIntegerDynArray;
    Count: integer;
    procedure Init(InitialCapacity: integer=50);
    function Find(aValue: integer): boolean;
    function FindIndex(aValue: integer): integer;
    function Add(aValue: integer; DontDuplicate: boolean = false): integer;
    /// force Value[] having exactly Count as Length
    procedure SetLength;
  end;

  PAnsiStrings = ^TAnsiStrings;
  /// old-fashion object to store a dynamic array of string
  // - old-fashion objects are stored as record -> don't need to call free
  // - memory is allocated by chunk: it's more efficient than plain
  // TStringDynArray to store a growing array of string
  TAnsiStrings = object
    Value: TStringDynArray;
    Count: integer;
    procedure Init(InitialCapacity: integer=50);
    function Find(const aValue: AnsiString; CaseSensitive: boolean=false): boolean;
    function FindIndex(const aValue: AnsiString; CaseSensitive: boolean=false): integer;
    /// find aName=???
    function FindValue(const aName: AnsiString; const aSep: AnsiString='='): AnsiString;
    function Add(const aValue: AnsiString; DontDuplicate: boolean=false; CaseSensitive: boolean=false): integer;
    procedure Delete(aIndex: integer);
    procedure SetLength;
  end;

/// find the index of the first Value found in an array of integer
// - p points to the first integer
// - Count is the number of total integer (not integer memory size)
function FastFindIntegerIndex(p: PInteger; Count: Cardinal; Value: integer): integer;

/// return true if the Value is found in an array of integer
// - p points to the first integer
// - Count is the number of total integer (not integer memory size)
function FastFindInteger(p: PInteger; Count: Cardinal; Value: integer): boolean;

/// wrapper to add an integer in Res[] + Count
function AddInteger(var Res: TIntegerDynArray; var Count: integer; aValue: integer;
  DontDuplicate: boolean = false): integer;

/// wrapper to add a string in Res[] + Count
function AddString(var Res: TStringDynArray; var Count: integer; aValue: AnsiString): integer;

/// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
function IdemPChar(p, up: pAnsiChar): boolean;

/// GetStringIndex(['un','deux','trois'],'deux') -> 1
function GetStringIndex(const Values: array of AnsiString; const Value: AnsiString): integer;

/// return next CSV string in P, nil if no more
function GetNextItem(var P: PAnsiChar; Sep: AnsiChar= ','): AnsiString;

/// return trim(next CSV string) from P, set P=nil if no more string available
function GetNextItemTrimed(var P: PAnsiChar; Sep: AnsiChar= ','): AnsiString;

/// return the next value as an integer, 0 if not a positive integer
function GetNextItemCardinal(var P: PAnsiChar): cardinal;

/// s := 'un,deux'; CSVAdd(s,'trois') -> s='un,deux,trois'
procedure CSVAdd(var aCSV: AnsiString; const aValue: AnsiString);

/// s := 'un,deux'; CSVAddOnce(s,'deux') -> s='un,deux'
procedure CSVAddOnce(var aCSV: AnsiString; const aValue: AnsiString);

/// s := 'un,deux'; CSVAddOnce(s,'deux,trois') -> s='un,deux,trois'
procedure CSVAddOnceCSV(var aCSV: AnsiString; const aCSVValue: AnsiString);

/// CSVContains('un,deux,trois','deux')=true
function CSVContains(const aCSV, aValue: AnsiString; Sep: AnsiChar=','): boolean;

/// CSVIndexOf('un,deux,trois','deux')=1
function CSVIndexOf(const aCSV, aValue: AnsiString; Sep: AnsiChar=','): integer;

/// v := 'un,deux,trois','deux' -> CSVDelete(v,'deux')=true and v='un,trois'
function CSVDelete(var aCSV: AnsiString; const aValue: AnsiString; Sep: AnsiChar=','): boolean;

/// LastCSV('un,deux,trois')='trois'
function LastCSV(const aCSV: AnsiString): AnsiString;

/// GetStringIndexCSV(['un','deux','trois']) -> 'un,deux,trois'
function GetStringIndexCSV(const Values: array of AnsiString): AnsiString;

/// Result[n]=true if any CSV in Values[n]
// - leave previous value of Result[] if no match
procedure SetStringIndexCSV(const CSV: AnsiString; const Values: array of AnsiString;
  var Result: array of Boolean);

/// can be used in any TStrings.CustomSort() to class a list from "5.1"-like indexes
function SectionNumbersCompare(List: TStringList; Index1, Index2: Integer): Integer;

/// CSVToIntegers('1,2,3',Ints) -> Ints[]=1,2,3
procedure CSVToIntegers(const aCSV: AnsiString; var Integers: TIntegerDynArray);

var
  Hex2Dec:   array[AnsiChar] of byte; // for UrlDecode

/// convert a number stored in ascii in p^ to a cardinal value
// - return 0 if p^ is not a digit
// - stop at the first non-digit char in p^
// - if d is not nil, it will contain the next char to be parsed (the first one
// after the last digit converted)
function PCharToHex32(p: PAnsiChar; d: ppChar = nil): cardinal;

/// convert a number stored in ascii to a cardinal value
// - stop at the first non-digit char
// - don't raise any exception, but return zero if no digit is available
function HexToInt(const s: AnsiString): cardinal;

/// add a char to an AnsiString
// - this is faster than S := S+c (in which an AnsiString is allocated for c)
// - with FastMM4, performance is very good (ReallocMem is optimized)
procedure AddCharLS(var S: AnsiString; c: AnsiChar);

/// get an adress for a stub procedure
// function FastcodeGetAddress(AStub: Pointer): Pointer;

/// in-memory code redirection: Func will call RedirectFunc procedure/function
procedure Redirect(Func, RedirectFunc: Pointer);



implementation

{$ifdef NOENHANCEDRTL}
procedure TabledPCharCpy(src, dest: pAnsiChar; Count: integer; var Table: TNormTable); assembler;
// very fast copy with Table-case transform
// eax=src edx=dest ecx=Count
asm // generates a push ebp; mov esp,ebp
    or edx,edx
    push ebx
    push edi
    jz @z // avoid GPF
    mov edi,ecx
    xor ebx,ebx
    xor ecx,ecx
    cmp edi,8
    mov ebp,Table // generates mov ebp,[ebp+8]
    jae @loop
    jmp dword ptr [edi*4+@Table]
    nop; nop // align @Table
@Table:
    dd @0,@1,@2,@3,@4,@5,@6,@7
@loop:
    sub edi,8
    mov bl,[eax];     mov cl,[eax+1]  // use bl+cl: 2 chars each time (pairing on i686)
    mov bl,[ebx+ebp]; mov cl,[ecx+ebp]
    mov [edx],bl;     mov [edx+1],cl
    mov bl,[eax+2];   mov cl,[eax+3]
    mov bl,[ebx+ebp]; mov cl,[ecx+ebp]
    mov [edx+2],bl;   mov [edx+3],cl
    mov bl,[eax+4];   mov cl,[eax+5]
    mov bl,[ebx+ebp]; mov cl,[ecx+ebp]
    mov [edx+4],bl;   mov [edx+5],cl
    mov bl,[eax+6];   mov cl,[eax+7]
    mov bl,[ebx+ebp]; mov cl,[ecx+ebp]
    mov [edx+6],bl;   mov [edx+7],cl
    cmp edi,8
    lea eax,[eax+8]
    lea edx,[edx+8]
    jae @loop
    jmp dword ptr [edi*4+@Table]
@7: mov cl,[eax+6]; mov cl,[ecx+ebp]; mov [edx+6],cl
@6: mov bl,[eax+5]; mov bl,[ebx+ebp]; mov [edx+5],bl
@5: mov cl,[eax+4]; mov cl,[ecx+ebp]; mov [edx+4],cl
@4: mov bl,[eax+3]; mov bl,[ebx+ebp]; mov [edx+3],bl
@3: mov cl,[eax+2]; mov cl,[ecx+ebp]; mov [edx+2],cl
@2: mov bl,[eax+1]; mov bl,[ebx+ebp]; mov [edx+1],bl
@1: mov cl,[eax+0]; mov cl,[ecx+ebp]; mov [edx+0],cl
@0: mov byte ptr [edx+edi],0
@z: pop edi
    pop ebx
end;
{$endif}

function PosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
// Faster Equivalent of Delphi 7 StrUtils.PosEx
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx,[edx-4]     {Length(Str)}
  mov     ebx,[eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx        {Max Start Pos for Full Match}
  lea     edx,[edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi,[ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  movzx   ebx,[eax]       {Search Character}
@@Loop:                    {Compare 2 Characters per Loop}
  cmp     bl,[edx]
  jne     @@NotChar1
  mov     ebp, edi         {Remainder}
@@Char1Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax,[edx+ebp]
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  cmp     bl,[edx+1]
  jne     @@NotChar2
  mov     ebp, edi         {Remainder}
@@Char2Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax,[edx+ebp+1]
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ebp
  pop     edi
  jmp     @@CheckResult
@@NotChar2:
  lea     edx,[edx+2]
  cmp     edx, ecx         {Next Start Position <= Max Start Position}
  jle     @@Loop
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  movzx   eax,[eax]       {Search Character}
@@CharLoop:
  cmp     al,[edx]
  je      @@SetResult
  cmp     al,[edx+1]
  je      @@CheckResult
  lea     edx,[edx+2]
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check within AnsiString}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1
@@SetResult:
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax,[edx+ecx+1]
end;

function AnsiPosEx(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
// fast PosEx, with ignoreCase (use NormToUpper)
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  push    edi              {Save Remainder to Check = Length(SubStr) - 2}
  push    ecx              {Save Max Start Position}
  lea     edi, NormToUpper {Uppercase Lookup Table}
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [edi+ebx]   {Convert to Uppercase}
@@Loop:                    {Loop Comparing 2 Characters per Loop}
  movzx   eax, [edx]       {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  jne     @@NotChar1
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char1Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  movzx   eax, [edx+1]     {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     bl, al
  jne     @@NotChar2
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char2Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+2]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@CheckResult    {Check Match is within AnsiString Data}
@@NotChar2:
  add     edx,2
  cmp     edx, [esp]       {Compate to Max Start Position}
  jle     @@Loop           {Loop until Start Position > Max Start Position}
  pop     ecx              {Dump Start Position}
  pop     edi              {Dump Remainder to Check}
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  lea     esi, NormToUpper
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [esi+ebx]   {Convert to Uppercase}
@@CharLoop:
  movzx   eax, [edx]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@SetResult
  movzx   eax, [edx+1]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check Match is within AnsiString Data}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1           {OK - Adjust Result}
@@SetResult:               {Set Result Position}
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end;


function FastFindInteger(p: PInteger; Count: Cardinal; Value: integer): boolean;
// test if Value is present in the PInteger[0..n-1]
// eax=p edx=n ecx=value
asm
    test eax,eax
    jz @end
    cmp edx,8
    jae @s1
    jmp dword ptr [edx*4+@Table]
    nop // align @Table
@Table:
    dd @z, @1, @2, @3, @4, @5, @6, @7
@s1: // fast search by 8 integers (pipelined instructions)
    sub edx,8
    cmp [eax],ecx;    je @ok
    cmp [eax+4],ecx;  je @ok
    cmp [eax+8],ecx;  je @ok
    cmp [eax+12],ecx; je @ok
    cmp [eax+16],ecx; je @ok
    cmp [eax+20],ecx; je @ok
    cmp [eax+24],ecx; je @ok
    cmp [eax+28],ecx; je @ok
    cmp edx,8
    lea eax,[eax+32] // preserve flags during 'cmp edx,8' computation
@s2:jae @s1
    jmp dword ptr [edx*4+@Table]
@7: cmp [eax+24],ecx; je @ok
@6: cmp [eax+20],ecx; je @ok
@5: cmp [eax+16],ecx; je @ok
@4: cmp [eax+12],ecx; je @ok
@3: cmp [eax+8],ecx;  je @ok
@2: cmp [eax+4],ecx;  je @ok
@1: cmp [eax],ecx;    je @ok
@z: xor eax,eax
@end:
    ret
@ok:mov al,1
end;

function FastFindIntegerIndex(p: PInteger; Count: Cardinal; Value: integer): integer;
// eax=p edx=n ecx=value
asm
  push eax
  test eax,eax
  jz @z
  cmp edx,8
  jmp @s2
@ok0:              pop ecx; sub eax,ecx; shr eax,2; ret
@ok4:  add eax,4;  pop ecx; sub eax,ecx; shr eax,2; ret
@ok8:  add eax,8;  pop ecx; sub eax,ecx; shr eax,2; ret
@ok12: add eax,12; pop ecx; sub eax,ecx; shr eax,2; ret
@ok16: add eax,16; pop ecx; sub eax,ecx; shr eax,2; ret
@ok20: add eax,20; pop ecx; sub eax,ecx; shr eax,2; ret
@ok24: add eax,24; pop ecx; sub eax,ecx; shr eax,2; ret
@ok28: add eax,28; pop ecx; sub eax,ecx; shr eax,2; ret
@s1:
  sub edx,8
  cmp [eax],ecx;    je @ok0
  cmp [eax+4],ecx;  je @ok4
  cmp [eax+8],ecx;  je @ok8
  cmp [eax+12],ecx; je @ok12
  cmp [eax+16],ecx; je @ok16
  cmp [eax+20],ecx; je @ok20
  cmp [eax+24],ecx; je @ok24
  cmp [eax+28],ecx; je @ok28
  cmp edx,8
  lea eax,[eax+32]  // preserve flags during 'cmp edx,8' computation
@s2:
  jae @s1
  test edx,edx; jz @z // we need to find first -> no jmp [edx*4] trick :(
  cmp [eax],ecx;    je @ok0;  dec edx; jz @z
  cmp [eax+4],ecx;  je @ok4;  dec edx; jz @z
  cmp [eax+8],ecx;  je @ok8;  dec edx; jz @z
  cmp [eax+12],ecx; je @ok12; dec edx; jz @z
  cmp [eax+16],ecx; je @ok16; dec edx; jz @z
  cmp [eax+20],ecx; je @ok20; dec edx; jz @z
  cmp [eax+24],ecx; je @ok24
@z:
  pop ecx
  or eax,-1
end;

function AddInteger(var Res: TIntegerDynArray; var Count: integer; aValue: integer;
  DontDuplicate: boolean = false): integer;
// ex: AddInteger(IntArray,IntArrayCount,256);
begin
  if DontDuplicate then begin
    result := FastFindIntegerIndex(@Res[0],Count,aValue);
    if result>=0 then
      exit;
  end;
  if Count>=length(Res) then
    SetLength(Res,Count+200);
  Res[Count] := aValue;
  result := Count;
  inc(Count);
end;

function PCharToHex32(p: PAnsiChar; d: ppChar): cardinal;
var v0,v1: cardinal;
begin
  result := 0;
  if p<>nil then begin
    while p^=' ' do inc(p);
    repeat
      v0 := Hex2Dec[p[0]];
      if v0=255 then break; // not in '0'..'9','a'..'f'
      v1 := Hex2Dec[p[1]];
      inc(p);
      if v1=255 then begin
        result := (result shl 4)+v0; // only one char left
        break;
      end;
      v0 := v0 shl 4;
      result := result shl 8;
      inc(v0,v1);
      inc(p);
      inc(result,v0);
    until false;
  end;
  if d<>nil then
    d^ := p;
end;

function HexToInt(const s: AnsiString): cardinal;
begin
  result := PCharToHex32(pointer(s),nil);
end;


{ TIntegers }

function TIntegers.Add(aValue: integer; DontDuplicate: boolean): integer;
begin
  result := AddInteger(Value,Count,aValue,DontDuplicate);
end;

function TIntegers.Find(aValue: integer): boolean;
begin
  result := FastFindInteger(@Value[0],Count,aValue);
end;

function TIntegers.FindIndex(aValue: integer): integer;
begin
  result := FastFindIntegerIndex(@Value[0],Count,aValue);
end;

procedure TIntegers.Init(InitialCapacity: integer);
begin
  Count := 0;
  system.SetLength(Value,InitialCapacity);
end;

procedure TIntegers.SetLength;
begin
  system.SetLength(Value,Count);
end;


function AddString(var Res: TStringDynArray; var Count: integer; aValue: AnsiString): integer;
begin
  if Count>=length(Res) then
    SetLength(Res,Count+200);
  Res[Count] := aValue;
  result := Count;
  inc(Count);
end;

{ TAnsiStrings }

function TAnsiStrings.Add(const aValue: AnsiString; DontDuplicate: boolean = false;
  CaseSensitive: boolean=false): integer;
begin
  if DontDuplicate then begin
    result := FindIndex(aValue,CaseSensitive);
    if result>=0 then
      exit;
  end;
  result := AddString(Value,Count,aValue);
end;

procedure TAnsiStrings.Delete(aIndex: integer);
var i: integer;
begin
  if cardinal(aIndex)>=cardinal(Count) then exit;
  dec(Count);
  for i := aIndex to Count do // fast (no move, only lockinc), and avoid GPF
    Value[i] := Value[i+1];
end;

function TAnsiStrings.Find(const aValue: AnsiString; CaseSensitive: boolean=false): boolean;
begin
  result := FindIndex(aValue,CaseSensitive)>=0;
end;

function TAnsiStrings.FindIndex(const aValue: AnsiString; CaseSensitive: boolean=false): integer;
begin
  if CaseSensitive then begin
    for result := 0 to Count-1 do
      if Value[result]=aValue then
        exit;
  end else
    for result := 0 to Count-1 do
      if SameText(Value[result],aValue) then
        exit;
  result := -1;
end;

function UpperCase(const S: string): string;
var L: Integer;
begin
  L := length(S);
  result := '';
  if L=0 then
    exit;
  SetLength(result,L);
  TabledPCharCpy(pointer(S),Pointer(result),L,NormToUpper);
end;

function TAnsiStrings.FindValue(const aName: AnsiString; const aSep: AnsiString='='): AnsiString;
var i, L: integer;
    tmp: AnsiString;
begin
{$ifdef LVCL}
  tmp := UpperCase(aName)+aSep;
  L := length(tmp)+1;
{$else}
  i := length(aName);
  L := length(aSep);
  if (i=0) or (L=0) then begin
    result := '';
    exit;
  end;
  system.SetLength(tmp,i+L);
  TabledPCharCpy(pointer(aName),pointer(tmp),i,NormToUpper);
  move(pointer(aSep)^,pointer(integer(tmp)+i)^,L);
  inc(L,i+1);
{$endif}
  for i := 0 to Count-1 do
    if IdemPChar(pointer(Value[i]),pointer(tmp)) then begin
      while Value[i][L]=' ' do inc(L); // trim left
      result := copy(Value[i],L,maxInt);
      exit;
    end;
  result := '';
end;

procedure TAnsiStrings.Init(InitialCapacity: integer);
begin
  Count := 0;
  system.SetLength(Value,InitialCapacity);
end;

procedure TAnsiStrings.SetLength;
begin
  system.SetLength(Value,Count);
end;

function StringReplaceAll(const S, OldPattern, NewPattern: AnsiString): AnsiString;
// fast replacement of StringReplace(S, OldPattern, NewPattern,[rfReplaceAll]);
procedure Process(j: integer);
var i: integer;
begin
  Result := '';
  i := 1;
  repeat
    Result := Result+Copy(S,i,j-i)+NewPattern;
    i := j+length(OldPattern);
    j := PosEx(OldPattern, S, i);
    if j=0 then begin
      Result := Result+Copy(S, i, maxInt);
      break;
    end;
  until false;
end;
var j: integer;
begin
  j := Pos(OldPattern, S);
  if j=0 then
    result := S else
    Process(j);
end;

procedure ValAtPChar(pc: pAnsiChar; index: integer; charLimit: AnsiChar; var result: AnsiString);
var pdeb: pAnsiChar; // optimized asm
label s;
begin
  if pc=nil then
    exit;
  if index=0 then
    goto s; // goto is deprecated, but fast & easy in this code
  dec(index);
  while pc^<>#0 do begin
    if pc^=charLimit then begin
      if index=0 then begin
        inc(pc);
        if pc^=#10 then
          inc(pc); // ignore #10
s:      pdeb := pc;
        while (pc^<>#0) and (pc^<>charLimit) do
          inc(pc);
        SetString(result,pdeb,pc-pdeb);
        exit;
      end;
      dec(index);
    end;
    inc(pc);
  end;
end;

function ValAt(const value: AnsiString; index: integer; charLimit: AnsiChar = ','): AnsiString;
// ValAt('un,deux',1)='deux'
begin
  result := '';
  if index>=0 then
    ValAtPChar(pointer(value),index,charLimit,result);
end;

function IdemPChar(p, up: pAnsiChar): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
// eax=p edx=up
asm
  or eax,eax
  jz @e // P=nil -> false
  or edx,edx
  push ebx
  push esi
  jz @z // up=nil -> true
  mov esi,offset NormToUpper
  xor ebx,ebx
  xor ecx,ecx
@1:
  mov cl,[edx] // cl=up^
  mov bl,[eax] // bl=p^
  test cl,cl
  mov bl,[ebx+esi] // bl=NormToUpper[p^]
  jz @z // up^=#0 -> OK
  lea edx,[edx+1] // = inc edx without changing flags
  cmp bl,cl
  lea eax,[eax+1]
  je @1
  pop esi
  pop ebx
  xor eax,eax
@e:
  ret
@z:
  mov al,1 // up^=#0 -> OK
  pop esi
  pop ebx
end;

function GetStringIndex(const Values: array of AnsiString; const Value: AnsiString): integer;
// (['un','deux','trois'],'deux') -> 1
begin
  if Value<>'' then
  for result := 0 to length(Values)-1 do
    if SameText(Values[result],Value) then
      exit;
  result := -1;
end;

function GetNextItem(var P: PAnsiChar; Sep: AnsiChar = ','): AnsiString;
// return next CSV string in P, nil if no more
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
     P := S+1 else
     P := nil;
  end;
end;

function GetNextItemCardinal(var P: PAnsiChar): cardinal;
/// return the next value as an integer, 0 if not a positive integer
var c: cardinal;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^=' ' then repeat inc(P) until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

function SectionNumbersCompare(List: TStringList; Index1, Index2: Integer): Integer;
var P1,P2: PAnsiChar;
    V1,V2: integer;
begin
  P1 := pointer(List.Strings[Index1]);
  P2 := pointer(List.Strings[Index2]);
  repeat
    V1 := GetNextItemCardinal(P1);
    V2 := GetNextItemCardinal(P2);
    result := V1-V2;
    if result<>0 then
      exit;
    if (P1^<>'.') and (P2^<>'.') then
      break;
    if P1^='.' then
      inc(P1);
    if P2^='.' then
      inc(P2);
  until false;
  result := StrIComp(P1,P2);
end;

function GetNextItemTrimed(var P: PAnsiChar; Sep: AnsiChar = ','): AnsiString;
// return trim(next CSV string) from P, nil if no more
var S: PAnsiChar;
    L: cardinal;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    while P^=' ' do inc(P); // trim left
    L := S-P;
    while P[L-1]=' ' do dec(L); // trim right
    SetString(result,P,L);
    if S^<>#0 then
     P := S+1 else
     P := nil;
  end;
end;

function GetStringIndexCSV(const Values: array of AnsiString): AnsiString;
// ('un','deux','trois') -> 'un,deux,trois'
var n, i: integer;
begin
  n := length(Values);
  if n=0 then
    result := '' else begin
    result := Values[0];
    for i := 1 to n-1 do
      result := result+','+Values[i];
  end;
end;

procedure SetStringIndexCSV(const CSV: AnsiString; const Values: array of AnsiString;
  var Result: array of Boolean);
// Result[n]=true if any CSV in Values[n]
var i: integer;
    P: PAnsiChar;
begin
  if length(Values)<>length(Result) then exit;
  P := pointer(CSV);
  while P<>nil do begin
    i := GetStringIndex(Values,GetNextItemTrimed(P));
    if (i>=0) and (i<length(Result)) then
      Result[i] := true;
  end;
end;

function CSVIndexOf(const aCSV, aValue: AnsiString; Sep: AnsiChar=','): integer;
// CSVIndexOf('un,deux,trois','deux')=1
var P: PAnsiChar;
begin
  result := 0;
  P := pointer(aCSV);
  while P<>nil do
    if SameText(GetNextItem(P,Sep),aValue) then
      exit else
      inc(result);
  result := -1;
end;

function CSVDelete(var aCSV: AnsiString; const aValue: AnsiString; Sep: AnsiChar=','): boolean;
// v := 'un,deux,trois','deux' -> CSVDelete(v,'deux')=true and v='un,trois'
var B,P: PAnsiChar;
    i: integer;
begin
  P := pointer(aCSV);
  while P<>nil do begin
    B := P;
    if SameText(GetNextItem(P,Sep),aValue) then begin
      result := true;
      i := B-pointer(aCSV)+1;
      if P=nil then
        if i=1 then
          aCSV := '' else
          SetLength(aCSV,i-2) else
        delete(aCSV,i,P-B);
      exit;
    end;
  end;
  result := false;
end;

function CSVContains(const aCSV, aValue: AnsiString; Sep: AnsiChar=','): boolean;
// CSVContains('un,deux,trois','deux')=true
var P: PAnsiChar;
begin
  result := true;
  P := pointer(aCSV);
  while P<>nil do
    if SameText(GetNextItem(P,Sep),aValue) then
      exit;
  result := false;
end;

procedure CSVAdd(var aCSV: AnsiString; const aValue: AnsiString);
// s := 'un,deux'; CSVAdd(s,'trois') -> s='un,deux,trois'
begin
  if aCSV='' then
    aCSV := aValue else
    aCSV := aCSV+','+aValue;
end;

procedure CSVAddOnce(var aCSV: AnsiString; const aValue: AnsiString);
// s := 'un,deux'; CSVAddOnce(s,'deux') -> s='un,deux'
begin
  if aValue='' then exit;
  if aCSV='' then
    aCSV := aValue else
    if not CSVContains(aCSV,aValue) then
      aCSV := aCSV+','+aValue;
end;

procedure CSVAddOnceCSV(var aCSV: AnsiString; const aCSVValue: AnsiString);
// s := 'un,deux'; CSVAddOnce(s,'deux,trois') -> s='un,deux,trois'
var P: PAnsiChar;
begin
  P := pointer(aCSVValue);
  while P<>nil do
    CSVAddOnce(aCSV,GetNextItem(P));
end;

procedure CSVToIntegers(const aCSV: AnsiString; var Integers: TIntegerDynArray);
var P: PAnsiChar;
    n, V: integer;
begin
  P := Pointer(aCSV);
  SetLength(Integers,0);
  n := 0;
  repeat
    V := GetNextItemCardinal(P);
    if V=0 then
      break;
    Inc(n);
    SetLength(Integers,n);
    Integers[n-1] := V;
    if P=nil then
      break;
    while P^ in [' ',','] do inc(P);
  until False;
end;

function LastCSV(const aCSV: AnsiString): AnsiString;
// LastCSV('un,deux,trois')='trois'
var i: integer;
begin
  for i := length(aCSV) downto 1 do
  if aCSV[i]=',' then begin
    result := copy(aCSV,i+1,maxInt);
    exit;
  end;
  result := aCSV;
end;


type
  PJumpCode = ^TJumpCode;
  TJumpCode = packed record
    Code: Byte; // $e9  jmp Distance
    Distance: Integer;
  end;

function FastcodeGetAddress(AStub: Pointer): Pointer;
begin
  if PBYTE(AStub)^ = $E8 then begin
    Inc(Integer(AStub));
    Result := Pointer(Integer(AStub) + SizeOf(Pointer) + PInteger(AStub)^);
  end else begin
    halt; // error in code generated by compiler (no optimization) -> program halt at startup
    Result := nil;
  end;
end;

{$ifdef Win32}

procedure Redirect(Func, RedirectFunc: Pointer);
var NewJump: PJumpCode absolute Func;
    OldProtect: Cardinal;
begin
  if VirtualProtect(Func, 5, PAGE_EXECUTE_READWRITE, OldProtect) then begin
    NewJump.Code := $E9;
    NewJump.Distance := Integer(RedirectFunc) - Integer(Func) - 5;
    FlushInstructionCache(GetCurrentProcess, Func, 5);
    VirtualProtect(Func, 5, OldProtect, @OldProtect);
  end;
  assert(pByte(Func)^=$e9);
end;

{$else}

type
  PRedirectCode = ^TRedirectCode;
  TRedirectCode = packed record
    Code: Word;
    Address: PPointer;
  end;
  TRedirectInfo = record
    Func: Pointer;
    OrgCode: TJumpCode;
  end;

function InjectCode(Addr: Pointer; Code: Pointer; Size: Integer): Boolean;
var
  SysPageSize, PageSize, AlignedAddr: Integer;
begin
  Result := False;
  SysPageSize := getpagesize;
  PageSize := SysPageSize;
  AlignedAddr := Integer(Addr) and not (PageSize - 1);
  while Integer(Addr) + Size >= AlignedAddr + PageSize do
    Inc(PageSize, SysPageSize);
  if mprotect(Pointer(AlignedAddr), PageSize, PROT_READ or PROT_WRITE or PROT_EXEC) = 0 then
    try
      Move(Code^, Addr^, Size);
      Result := True;
    except
    end;
end;

procedure Redirect(Func, RedirectFunc: Pointer);
var JumpCode: TJumpCode;
begin
  if PRedirectCode(Func)^.Code = $25FF then
    // redirected (in package or shared object)
    Func := PRedirectCode(Func)^.Address^;
  JumpCode.Code := $e9;
  JumpCode.Distance := Integer(RedirectFunc) -
                       Integer(Func) -
                       SizeOf(TJumpCode);
  if not CompareMem(Func, @JumpCode, SizeOf(TJumpCode)) then
    InjectCode(Func, @JumpCode, SizeOf(TJumpCode));
  assert(pByte(Func)^=$e9);
end;

{$endif}

procedure AddCharLS(var S: AnsiString; c: AnsiChar);
// add a lonely char to an AnsiString (with slow ReallocMem)
// - this is faster than S := S+c (in which an AnsiString is allocated for c)
var L: integer;
begin
  L := integer(s);
  if L<>0 then
    L := pInteger(L-4)^; // L := length(S)
  SetLength(s,L+1); // with FastMM4, this SetLength() is almost immediate
  PAnsiChar(integer(s)+L)^ := c;
end;

{$ifdef NOENHANCEDRTL}
procedure InitAnsiConsts;
// init NormToUpper[] / NormToLower[] to WinAnsi code page,
// with only A..Z ('Å'->'A' e.g.)
{} const n2u: string[130]= '€‚ƒ„…†‡ˆ‰S‹ŒZ‘’“”•–—˜™S›ŒZY ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»'+
{}        '¼½¾¿AAAAAAÆCEEEEIIIIDNOOOOO×OUUUUYÞßAAAAAAÆCEEEEIIIIDNOOOOO÷OUUUUYÞY';
var i: cardinal;
    c: char;
begin // default settings, according to WinAnsi code page
  for i := 0 to 127 do begin
    NormToUpperByte[i] := i;
    NormToLowerByte[i] := i;
    c := n2u[i+1];
    NormToUpperByte[i+128] := ord(c);
    NormToLowerByte[i+128] := ord(c);
    if c in ['A'..'Z'] then
      inc(NormToLowerByte[i+128],32);
  end;
  for c := 'a' to 'z' do
    dec(NormToUpper[c],32);
  for c := 'A' to 'Z' do
    inc(NormToLower[c],32);
end;
{$endif}

var i: integer; // i is a register -> no global memory is allocated
    H2C: array[byte] of byte absolute Hex2Dec;
begin
  // init H2C[]
  FillChar(H2C,sizeof(H2C),$ff);
  for i := 0 to 9 do H2C[i+ord('0')] := i;
  for i := 10 to 15 do begin
    H2C[i+(ord('a')-10)] := i;
    H2C[i+(ord('A')-10)] := i;
  end;
{$ifdef NOENHANCEDRTL}
  InitAnsiConsts;
{$endif}
end.
