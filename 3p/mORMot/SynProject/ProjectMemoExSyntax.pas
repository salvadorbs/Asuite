/// Custom syntax highlighting for the MemoEx editor
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectMemoExSyntax;

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

interface

uses
  SysUtils,
  SynMemoEx;

type
  /// virtual class used for Syntax Highlighting
  TProjectSyntax = class
  public
    /// .ini line syntax highlighting
    class procedure IniGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .pro or .scr body line syntax highlighting
    class procedure BodyGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .pas code line syntax highlighting
    class procedure PasGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .dfm code line syntax highlighting
    class procedure DfmGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .c .h code line syntax highlighting
    class procedure CGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .cs code line syntax highlighting
    class procedure CSharpGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .xml .html code line syntax highlighting
    class procedure SGMLLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    /// .mod code line syntax highlighting
    class procedure Mod2GetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
  public
    class function GetHighliter(const Ext: string): TOnGetLineAttr;
  end;


const
  /// '@SRS@' or '@Claude Mench@' or '@EIA\Main.pas@' or '=[KnownIssue]'
  edStyleButton = 1;
  /// '%picturename.png'
  edStylePicture = 2;
  /// '[SDD-DI-4.1]' -> [SRS-DI-4.1]
  edStyleSection = 3;

  VALID_CODE_EXT: array[0..21] of string =
  ({0}'.INI','.CFG','.DOF', {1}'.PAS','.DPK','.PP','.DPR', {7} '.DFM',
   {2}'.C','.H','.CPP', {3}'.CS', {4}'.PRO','.SCR','.MAN',
   {5}'.HTM','.HTML','.XML','.XAML','.CONFIG', {6} '.MOD','.DEF');
  VALID_CODE_HIGHLIGHTER: array[0..high(VALID_CODE_EXT)] of integer =
  (0,0,0, 1,1,1,1,7, 2,2,2, 3, 4,4,4, 5,5,5,5,5, 6,6);



procedure ProgramMemoGetLineAttr(p: pChar; A: PLineAttr; const KeyWords: array of string);
// code syntax-highlighting

procedure TextMemoGetLineAttr(p: pChar; A: PLineAttr);
// normal .pro or .scr rtf text

implementation


uses
  Graphics;

const
  LISTINGCOLOR = clWhite-$080808; // very light gray

  KEYWORDCHARS: set of char =
    ['A'..'Z','a'..'z','0'..'9','_'];

  PASCALKEYWORDS: array[0..99] of string =
  ('ABSOLUTE', 'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM', 'ASSEMBLER',
   'AUTOMATED', 'BEGIN', 'CASE', 'CDECL', 'CLASS', 'CONST', 'CONSTRUCTOR',
   'DEFAULT', 'DESTRUCTOR', 'DISPID', 'DISPINTERFACE', 'DIV', 'DO',
   'DOWNTO', 'DYNAMIC', 'ELSE', 'END', 'EXCEPT', 'EXPORT', 'EXPORTS',
   'EXTERNAL', 'FAR', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FORWARD',
   'FUNCTION', 'GOTO', 'IF', 'IMPLEMENTATION', 'IN', 'INDEX', 'INHERITED',
   'INITIALIZATION', 'INLINE', 'INTERFACE', 'IS', 'LABEL', 'LIBRARY',
   'MESSAGE', 'MOD', 'NEAR', 'NIL', 'NODEFAULT', 'NOT', 'OBJECT',
   'OF', 'OR', 'OUT', 'OVERRIDE', 'PACKED', 'PASCAL', 'PRIVATE', 'PROCEDURE',
   'PROGRAM', 'PROPERTY', 'PROTECTED', 'PUBLIC', 'PUBLISHED', 'RAISE',
   'READ', 'READONLY', 'RECORD', 'REGISTER', 'REINTRODUCE', 'REPEAT', 'RESIDENT',
   'RESOURCESTRING', 'SAFECALL', 'SET', 'SHL', 'SHR', 'STDCALL', 'STORED',
   'STRING', 'STRINGRESOURCE', 'THEN', 'THREADVAR', 'TO', 'TRY', 'TYPE',
   'UNIT', 'UNTIL', 'USES', 'VAR', 'VARIANT', 'VIRTUAL', 'WHILE', 'WITH', 'WRITE',
   'WRITEONLY', 'XOR');

  DFMKEYWORDS: array[0..4] of string = (
    'END', 'FALSE', 'ITEM', 'OBJECT', 'TRUE');

  CKEYWORDS: array[0..47] of string = (
  'ASM', 'AUTO', 'BREAK', 'CASE', 'CATCH', 'CHAR', 'CLASS', 'CONST', 'CONTINUE',
  'DEFAULT', 'DELETE', 'DO', 'DOUBLE', 'ELSE', 'ENUM', 'EXTERN', 'FLOAT', 'FOR',
  'FRIEND', 'GOTO', 'IF', 'INLINE', 'INT', 'LONG', 'NEW', 'OPERATOR', 'PRIVATE',
  'PROTECTED', 'PUBLIC', 'REGISTER', 'RETURN', 'SHORT', 'SIGNED', 'SIZEOF',
  'STATIC', 'STRUCT', 'SWITCH', 'TEMPLATE', 'THIS', 'THROW', 'TRY', 'TYPEDEF',
  'UNION', 'UNSIGNED', 'VIRTUAL', 'VOID', 'VOLATILE', 'WHILE');

  CSHARPKEYWORDS : array[0..86] of string = (
  'ABSTRACT', 'AS', 'BASE', 'BOOL', 'BREAK', 'BY3', 'BYTE', 'CASE', 'CATCH', 'CHAR',
  'CHECKED', 'CLASS', 'CONST', 'CONTINUE', 'DECIMAL', 'DEFAULT', 'DELEGATE', 'DESCENDING',
  'DO', 'DOUBLE', 'ELSE', 'ENUM', 'EVENT', 'EXPLICIT', 'EXTERN', 'FALSE', 'FINALLY',
  'FIXED', 'FLOAT', 'FOR', 'FOREACH', 'FROM', 'GOTO', 'GROUP', 'IF', 'IMPLICIT',
  'IN', 'INT', 'INTERFACE', 'INTERNAL', 'INTO', 'IS', 'LOCK', 'LONG', 'NAMESPACE',
  'NEW', 'NULL', 'OBJECT', 'OPERATOR', 'ORDERBY', 'OUT', 'OVERRIDE', 'PARAMS',
  'PRIVATE', 'PROTECTED', 'PUBLIC', 'READONLY', 'REF', 'RETURN', 'SBYTE',
  'SEALED', 'SELECT', 'SHORT', 'SIZEOF', 'STACKALLOC', 'STATIC', 'STRING',
  'STRUCT', 'SWITCH', 'THIS', 'THROW', 'TRUE', 'TRY', 'TYPEOF', 'UINT', 'ULONG',
  'UNCHECKED', 'UNSAFE', 'USHORT', 'USING', 'VAR', 'VIRTUAL', 'VOID', 'VOLATILE',
  'WHERE', 'WHILE', 'YIELD');

  MODULA2KEYWORDS: array[0..68] of string = (
  'ABS', 'AND', 'ARRAY', 'BEGIN', 'BITSET', 'BOOLEAN', 'BY', 'CAP', 'CARDINAL',
  'CASE', 'CHAR', 'CHR', 'CONST', 'DEC', 'DEFINITION', 'DIV', 'DO', 'ELSE',
  'ELSIF', 'END', 'EXCL', 'EXIT', 'EXPORT', 'FALSE', 'FLOAT', 'FOR', 'FROM',
  'GOTO', 'HALT', 'HIGH', 'IF', 'IMPLEMENTATION', 'IMPORT', 'IN', 'INC', 'INCL',
  'INTEGER', 'LONGINT', 'LOOP', 'MAX', 'MIN', 'MOD', 'MODULE', 'NIL', 'NOT',
  'ODD', 'OF', 'OR', 'ORD', 'POINTER', 'PROC', 'PROCEDURE', 'REAL', 'RECORD',
  'RECORD', 'REPEAT', 'RETURN', 'SET', 'SIZE', 'THEN', 'TO', 'TRUE', 'TRUNC',
  'TYPE', 'UNTIL', 'VAL', 'VAR', 'WHILE', 'WITH');

procedure TabledPCharCpy(src, dest: pAnsiChar; Count: integer);
var i: integer;
begin
  for i := 0 to Count-1 do
    if src[i] in ['a'..'z'] then
      dest[i] := AnsiChar(ord(src[i])-32) else
      dest[i] := src[i];
end;

function IsKeyWord(const KeyWords: array of string; const aToken: String): Boolean;
// aToken must be already uppercase
var First, Last, I, Compare: Integer;
begin
  First := Low(Keywords);
  Last := High(Keywords);
  Result := True;
  while First<=Last do begin
    I := (First + Last) shr 1;
    Compare := CompareStr(Keywords[I],aToken);
    if Compare=0 then
      Exit else
    if Compare<0  then
      First := I+1 else
      Last := I-1;
  end;
  Result := False;
end;

procedure ProgramMemoGetLineAttr(p: pChar; A: PLineAttr; const KeyWords: array of string);
// code syntax-highlighting
var I, L, Col: Integer;
    Token: string;
    d: pChar;
begin
  if p=nil then exit;
  // background
  if p[1]='!' then
    Col := clYellow else // highlight line text
    Col := LISTINGCOLOR; // normal program line
  d := p;
  repeat
    A^.BC := Col;
    inc(A);
    inc(d)
  until d^=#0;
  dec(A,d-p);
  // foreground 
  while p^<>#0 do begin
    while not (p^ in ['A'..'Z','a'..'z',#0]) do begin
      case p^ of
       '/': if (pChar(p+1)^='/') then begin
              repeat
                A^.FC := clNavy;
                include(A^.Style,fsItalic);
                inc(A);
                inc(p);
              until p^=#0; // '//' = comment till end of line
              exit;
            end;
       '{': if @KeyWords=@PASCALKEYWORDS then begin
            repeat // {pascal comment on same line}
              A^.FC := clNavy;
              include(A^.Style,fsItalic);
              inc(A);
              inc(p);
              if p^=#0 then exit;
            until p^='}';
            A^.FC := clNavy;
            include(A^.Style,fsItalic);
          end;
       '(': if (pChar(p+1)^='*') then
            if (@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@MODULA2KEYWORDS) then begin
              repeat // limitation: can't handle multiple lines comments 
                A^.FC := clNavy;
                include(A^.Style,fsItalic);
                inc(A);
                inc(p);
                if p^=#0 then exit;
              until (p^=')') and (pChar(p-1)^='*');
              A^.FC := clNavy;
              include(A^.Style,fsItalic);
            end;
       '#': if (@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@DFMKEYWORDS) then
         A^.FC := clNavy; // next word in blue navy color
       '|': if @KeyWords=@MODULA2KEYWORDS then
         include(A^.Style,fsBold); 
       '0'..'9': A^.FC := clNavy;
       '"': if @Keywords=@MODULA2KEYWORDS then begin
         repeat // "string"
           A^.FC := clNavy;
           inc(A);
           inc(p);
           if p^=#0 then exit;
         until p^='"';
         A^.FC := clNavy;
       end;
       #39: if (@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@DFMKEYWORDS) then begin
         repeat // 'string'
           A^.FC := clNavy;
           inc(A);
           inc(p);
           if p^=#0 then exit;
         until p^=#39;
         A^.FC := clNavy;
       end;
       '$': begin
         repeat
           A^.FC := clNavy;
           inc(A);
           inc(p);
           if p^=#0 then exit;
         until not (p^ in ['$','0'..'9', 'A'..'F', 'a'..'f']);
         continue;
       end;
      end;
      inc(A);
      inc(p);
    end;
    if p^=#0 then exit;
    d := p;
    while p^ in KEYWORDCHARS do inc(p);
    L := p-d;
    if (L in [2..14]) then begin // maybe keyword
      SetLength(Token,L);
      if @Keywords=@MODULA2KEYWORDS then
        move(d^,pointer(Token)^,L) else // MODULA-2 keywords are always uppercase
        TabledPCharCpy(d,pointer(Token),L);
      if IsKeyWord(KeyWords,Token) then
        for i := 1 to L do begin
          include(A^.Style,fsBold);
          inc(A);
        end else
        inc(A,L);
    end else
      inc(A,L); // not a keyword: leave A^ like they were (normal text)
  end;
end;

const
  RTFEndToken: set of char = [#0,' '..#127]-['A'..'Z','a'..'z','0'..'9'];

procedure TextMemoGetLineAttr(p: pChar; A: PLineAttr);
// normal .pro or .scr rtf text
var b: PChar;
    Level, L: integer;
    Stack: array[0..15] of TLineAttr; // stack to handle { }
    token: string[15];
begin
  if p=nil then exit;
  Level := 0;
  Stack[0] := A^;
  repeat
    case p^ of
    #0: exit;
    '{': begin
      b := p;
      repeat inc(b) until b^ in [#0,'}'];
      if b^=#0 then // no closing '}' -> mark error
        Stack[Level].BC := clFuchsia else
      if Level<high(Stack) then begin
        inc(Level);
        Stack[Level] := Stack[Level-1];
      end;
    end;
    '}': if Level>0 then
      dec(Level);
    '\': begin
      b := p;
      repeat inc(b) until b^ in RTFEndToken;
      L := b-p-1;
      if L<=7 then begin
        SetString(token,p+1,L);
        if token='b' then
          include(Stack[Level].Style,fsBold) else
        if token='b0' then
          exclude(Stack[Level].Style,fsBold) else
        if token='i' then
          include(Stack[Level].Style,fsItalic) else
        if token='i0' then
          exclude(Stack[Level].Style,fsItalic) else
        if token='ul' then
          include(Stack[Level].Style,fsUnderline) else
        if token='ul0' then
          exclude(Stack[Level].Style,fsUnderline) else
        if token='strike' then
          include(Stack[Level].Style,fsStrikeOut) else
        if token='f1' then // font Courrier
          Stack[Level].FC := clDkGray;
      end;
    end;
    '@': begin
       b := p;
       repeat inc(p) until p^ in [#0,'@'];
       if p^=#0 then // no closing '@' -> just go to next char = b
         p := b else begin
         while b<=p do begin
           include(A^.Style,fsItalic);
           A^.FC := clMaroon;
           A^.ex_style := edStyleButton; // mark '@SRS@' 
           inc(b);
           inc(A);
         end;
         inc(P);
         continue;
       end;
    end; // '@'
    end;
    A^ := Stack[Level];
    inc(P);
    inc(A);
  until false;
end;

class procedure TProjectSyntax.BodyGetLineAttr(Sender: TObject; const Line: String;
  index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
var L: integer;
procedure LineColor(Color: TColor);
var i: integer;
begin
  for i := 0 to L-1 do
    Attrs[i].FC := Color;
end;
procedure LineBackColor(Color: TColor);
var i: integer;
begin
  for i := 0 to L-1 do
    Attrs[i].BC := Color;
end;
procedure LineBold;
var i: integer;
begin
  for i := 0 to L-1 do
    include(Attrs[i].Style,fsBold);
end;
procedure LineStyle(Style: byte);
var i: integer;
begin
  for i := 0 to L-1 do
    Attrs[i].ex_style := Style;
end;
begin
  L := length(Line);
  if L=0 then exit;
  case Line[1] of
    '=': if Line[2]='[' then begin
      LineColor(clPurple);
      LineBold;
      LineStyle(edStyleButton); // mark '[SRS]'
    end else
      TextMemoGetLineAttr(pointer(line),@Attrs[0]);
    '[': begin
      LineColor(clRed);
      LineBold;
      LineStyle(edStyleSection); // '[SDD-DI-4.1]' -> [SRS-DI-4.1]
    end;
    ';': LineColor(clOlive);  // comment
    ':': begin
      LineBold;            // title
      LineColor(clTeal);
    end;
    '!':
      case Line[2] of
      '$': begin // !$ = DFM
        if Line[3]='!' then
          LineBackColor(clYellow) else
          LineBackColor(LISTINGCOLOR);
        DfmGetLineAttr(Sender,Line,index,SelAttrs,Attrs);
      end;
      else ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],PASCALKEYWORDS);
      end;
    '&': ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],CKEYWORDS);
    '#': ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],CSHARPKEYWORDS);
    'µ': ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],MODULA2KEYWORDS);
//    '-': with Attrs[0] do begin include(Style,fsBold); FC := clMaroon; end;
    '$': begin // listing
      LineColor(clNavy);
      case Line[2] of
      '$': begin // $$ = XML/HTML
        if Line[3]='!' then
          LineBackColor(clYellow) else
          LineBackColor(LISTINGCOLOR);
        SGMLLineAttr(Sender,Line,index,SelAttrs,Attrs);
      end;
      '!': LineBackColor(clYellow); // highlight
      else LineBackColor(LISTINGCOLOR);
      end;
    end;
    '%': begin
      LineColor(clGreen);  // picture
      LineStyle(edStylePicture); // '%picturename.png';
    end;
    '|': begin
      LineColor(clMaroon); // table
      TextMemoGetLineAttr(pointer(line),@Attrs[0]);
    end;
    else TextMemoGetLineAttr(pointer(line),@Attrs[0]);
  end;
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

class function TProjectSyntax.GetHighliter(const Ext: string): TOnGetLineAttr;
var E: integer;
begin
  E := GetStringIndex(VALID_CODE_EXT,Ext);
  if E<0 then
    result := nil else
    case VALID_CODE_HIGHLIGHTER[E] of
    0: result := IniGetLineAttr;
    1: result := PasGetLineAttr;
    2: result := CGetLineAttr;
    3: result := CSharpGetLineAttr;
    4: result := BodyGetLineAttr;
    5: result := SGMLLineAttr;
    6: result := MOD2GetLineAttr;
    7: result := DfmGetLineAttr;
    else result := nil;
    end;
end;

class procedure TProjectSyntax.IniGetLineAttr(Sender: TObject;
  const Line: String; index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
var i: integer;
    A: TLineAttr;
begin
  if Line='' then exit;
  if Line[1]='[' then begin // [Section]
    for i := 0 to length(Line)-1 do
    with Attrs[i] do begin
      Style := [fsBold];
      FC := clRed;
    end;
    exit;
  end else
  if Line[1]=';' then begin // comment
    for i := 0 to length(Line)-1 do
      Attrs[i].FC := clOlive;
    exit;
  end;
  A := Attrs[0];
  A.FC := clTeal;
  for i := 1 to length(Line) do
    if Line[i]='=' then begin
      A.FC := clRed;
      include(A.Style,fsBold);
      Attrs[i-1] := A;
      exclude(A.Style,fsBold);
      A.FC := clNavy;
    end else
      Attrs[i-1] := A;
end;

class procedure TProjectSyntax.Mod2GetLineAttr(Sender: TObject;
  const Line: String; index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
begin
  ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],MODULA2KEYWORDS);
end;

class procedure TProjectSyntax.PasGetLineAttr(Sender: TObject;
  const Line: String; index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
begin
  ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],PASCALKEYWORDS);
end;


class procedure TProjectSyntax.CGetLineAttr(Sender: TObject; const Line: String;
  index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
begin
  ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],CKEYWORDS);
end;

class procedure TProjectSyntax.CSharpGetLineAttr(Sender: TObject;
  const Line: String; index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
begin
  ProgramMemoGetLineAttr(pointer(Line),@Attrs[0],CSHARPKEYWORDS);
end;

procedure MarkKeyWords(p: PAnsiChar; A: PLineAttr; const KeyWords: array of string);
var d: PAnsiChar;
    L, i: integer;
    Token: string;
begin
  repeat
    while not (p^ in KEYWORDCHARS) do begin
      if p^=#0 then
        exit;
      inc(A);
      inc(p);
    end;
    d := p;
    while p^ in KEYWORDCHARS do inc(p);
    L := p-d;
    if (L in [2..14]) then begin // maybe keyword
      SetLength(Token,L);
      TabledPCharCpy(d,pointer(Token),L);
      if IsKeyWord(KeyWords,Token) then
        for i := 1 to L do begin
          include(A^.Style,fsBold);
          inc(A);
        end else
        inc(A,L);
    end else
      inc(A,L); // not a keyword: leave A^ like they were (normal text)
  until p^=#0;
end;

function IsDfmHexa(p: PAnsiChar): boolean;
begin
  result := false;
  if (p[0]='!') and (p[1]='$') then inc(p,2); // allow !$ at the line beginning 
  while (p^=' ') do inc(p);
  while p^<>#0 do
    if p^ in ['0'..'9','A'..'Z','}'] then
      inc(p) else
      exit;
  result := true;
end;

class procedure TProjectSyntax.DfmGetLineAttr(Sender: TObject;
  const Line: String; index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
var i,j,L: integer;
begin
  if Line='' then exit;
  L := length(Line)-1;
  MarkKeyWords(pointer(Line),@Attrs[0],DFMKEYWORDS);
  i := pos(' = ',Line);
  if i>0 then begin
    Attrs[i].FC := clMaroon;
    for j := i+2 to L do
      Attrs[j].FC := clNavy;
  end else
    if IsDfmHexa(pointer(Line)) then begin
      for j := 0 to L do
        Attrs[j].FC := clGray;
      if Line[L+1]='}' then
        Attrs[L].FC := clNavy;
    end;
  if (Line[1]='!') and (Line[2]='$') then begin
    Attrs[0].FC := clNavy;
    Attrs[1].FC := clNavy;
  end;
end;

class procedure TProjectSyntax.SGMLLineAttr(Sender: TObject; const Line: String;
  index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
var i,j: integer;
    FC: TColor;
begin
  i := 0;
  FC := clWindowText;
  if line<>'' then
  repeat
    case line[i+1] of // i+1 for better code generation, and compatible with Attrs[]
      #0: break;
      '$': if i<2 then begin Attrs[i].FC := clNavy; inc(i); continue; end; 
      '<': FC := clPurple; //fc12
      '>': begin Attrs[i].FC := clPurple; inc(i); FC := clWindowText; continue; end;
      '&': FC := clGreen;  //fc10
      ';': if FC=clGreen then begin
        Attrs[i].FC := FC; inc(i); FC := clWindowText; continue; end;
      '=': if FC=clPurple then
        for j := i downto 0 do
          if line[j+1]=' ' then
            break else
            Attrs[j].FC := clRed; //fc6
      '"': if FC=clNavy then begin Attrs[i].FC := FC; inc(i); FC := clPurple; continue; end else
           if FC=clPurple then FC := clNavy;
    end; // case line[i+1]
    Attrs[i].FC := FC; inc(i);
  until false;
end;

end.
