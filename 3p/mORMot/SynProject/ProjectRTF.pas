/// text and RTF writing classes
// - this unit is part of SynProject, under GPL 3.0 license; version 1.17
unit ProjectRTF;

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

  Revision 1.13
  - TRTF is now a TClass, with virtual methods to allow not only RTF backend

  Revision 1.15
  - new THTML backend, to write... HTML content

*)

interface

{.$define DIRECTPREVIEW}
{ if defined, documents can be previewed then exported as PDF directly }

{$define DIRECTEXPORTTOWORD}
{ if defined, documents are directly converted to .doc }

uses
  {$ifdef DIRECTPREVIEW}mORMotReport,{$endif}
  SysUtils, ProjectCommons, Classes;

{
   TStringWriter: fast buffered output

  var s: string;
      c: TStringWriter; // automatic Garbage Collector
  begin
    s := c.Init.Add('<').Add(HexChars,4).Add('>').
      AddXML('Essai & Deux').Add('</').AddCardinal(100).Add('>').Data;
    //  s='<0123>Essai &amp; Deux</100>'

  TRTF: specialized RTF writer
}

type
  PStringWriter = ^TStringWriter;
  TStringWriter = object
  private
    // tmp variable used in Add()
    sLen: integer;
    fGrowby: integer;
    fData: string;
    procedure SetData(const Value: string);
    function GetData: string;
    // returns fData without SetLength(fData,len)
    function GetDataPointer: pointer;
  public
    len: integer;
  public
    function Init: PStringWriter; overload;
    function Init(GrowBySize: integer): PStringWriter; overload;
    function Init(const Args: array of const): PStringWriter; overload;
    procedure SaveToStream(aStream: TStream); // contains reset (len := 0)
    procedure SaveToFile(const aFileName: String);
    procedure SaveToWriter(const aWriter: TStringWriter);
    function GetPortion(aPos, aLen: integer): string;
    procedure MovePortion(sourcePos, destPos, aLen: integer);

    procedure Write(const buf; bufLen: integer); overload;
    procedure Write(Value: integer); overload;
    procedure Write(const s: string); overload;

    function Add(c: char): PStringWriter; overload;
    function Add(p: pChar; pLen: integer): PStringWriter; overload;
    function Add(const s: string): PStringWriter; overload;
    function Add(const s1,s2: string): PStringWriter; overload;
    function Add(const s: array of string): PStringWriter; overload;
    function Add(const Format: string; const Args: array of const): PStringWriter; overload;
    function AddWithoutPeriod(const s: string; FirstCharLower: boolean = false): PStringWriter; overload;
    function AddStringOfChar(ch: char; Count: integer): PStringWriter; overload;
    function RtfBackSlash(Text: string;
      trimLastBackSlash: boolean = false): PStringWriter; // '\' -> '\\'

    function AddArray(const Args: array of const): PStringWriter;
    function AddCopy(const s: string; Index, Count: Integer): PStringWriter;
    function AddPChar(p: pChar): PStringWriter; overload;
    function AddByte(value: byte): PStringWriter;
    function AddWord(value: word): PStringWriter;
    function AddInteger(const value: integer): PStringWriter;
    // use CardinalToStrBuf()
    function AddCardinal(value: cardinal): PStringWriter;
    // AddCardinal6('+',1234)=Add('+ 001234 ')
    function AddCardinal6(cmd: char; value: cardinal; withLineNumber: boolean): PStringWriter;
    function AddInt64(value: Int64): PStringWriter;
    function AddHex32(value: cardinal): PStringWriter;
    function AddHex64(value: Int64): PStringWriter;
    function AddHexBuffer(value: PByte; Count: integer): PStringWriter;
    function AddShort(const s: shortstring): PStringWriter; overload;
    function AddShort(const s1,s2: shortstring): PStringWriter; overload;
    function AddShort(const s: array of shortstring): PStringWriter; overload;
    // no Grow test: faster
    procedure AddShortNoGrow(const s: shortstring);
    function AddCRLF: PStringWriter;

    function IsLast(const s: string): boolean; // if last data was s -> true
    function DeleteLast(const s: string): boolean; // if last data was s -> delete
    function EnsureLast(const s: string): PStringWriter; // if last data is not s -> add(s)
    function RtfValid: boolean; // return true if count of { = count of }

    property Data: string read GetData write SetData;
    property DataPointer: pointer read GetDataPointer;
  end;

  TLastRTF = (lastRtfNone, lastRtfText, lastRtfTitle, lastRtfCode,
      lastRtfImage, lastRtfCols, lastRtfList);
  TTitleLevel = array[0..6] of integer;
  TProjectWriterClass = class of TProjectWriter;
  TSaveFormat = (fNoSave,fDoc,fPdf);

  TProjectLayout = record
    Page: record
      Width, Height: integer;
    end;
    Margin: record
      Left, Right, Top, Bottom: integer;
    end;
  end;

  TProjectWriter = class
  protected
    WR: TStringWriter;
    FontSize: integer; // font size
    fLast: TLastRTF;
    fLastWasRtfPage: boolean;
    fLandscape, fKeyWordsComment: boolean;
    // set by RtfCols():
    fCols: string; // string to be added before any row
    fColsCount: integer;
    procedure RtfKeywords(line: string; const KeyWords: array of string; aFontSize: integer = 80); virtual;
    procedure SetLast(const Value: TLastRTF); virtual; abstract;
    procedure SetLandscape(const Value: boolean); virtual;
    constructor InternalCreate;
  public
    Layout: TProjectLayout;
    Width: integer; // paragraph width in twips
    TitleWidth: integer;  // title indetation gap (default 0 twips)
    IndentWidth: integer; // list or title first line indent (default 240 twips)
    TitleLevel: TTitleLevel;
    TitleLevelCurrent: integer;
    LastTitleBookmark: string;
    PicturePath: string;
    TitlesList: TStringList; // <>nil -> RtfTitle() add one (TitleList.Free in Caller)
    TitleFlat: boolean; // true -> Titles are all numerical with no big sections
    FullTitleInTableOfContent: boolean; // true -> Title contains also \line...
    ListLine: boolean; // true -> \line, not \par in RtfList()
    FileName: TFileName;
    constructor Create(const aLayout: TProjectLayout;
      aDefFontSizeInPts: integer; // in points
      aCodePage: integer = 1252;
      aDefLang: integer = $0409; // french is $040c (1036)
      aLandscape: boolean = false; aCloseManualy: boolean = false;
      aTitleFlat: boolean = false; const aF0: string = 'Calibri';
      const aF1: string = 'Consolas'); virtual;
    class function CreateFrom(aParent: TProjectWriter): TProjectWriter;
    // if CloseManualy was true
    procedure InitClose; virtual; abstract;
    procedure SaveToFile(Format: TSaveFormat; OldWordOpen: boolean); virtual; abstract;
    function AddRtfContent(const s: string): TProjectWriter;  overload; virtual; abstract;
    function AddRtfContent(const fmt: string; const params: array of const): TProjectWriter; overload;
    function AddWithoutPeriod(const s: string; FirstCharLower: boolean = false): TProjectWriter; 
    // set Last := lastText -> update any pending {}
    function RtfText: TProjectWriter;
    procedure RtfList(line: string); virtual; abstract;
    function RtfLine: TProjectWriter; virtual; abstract;
    procedure RtfPage; virtual; abstract;
    function RtfPar: TProjectWriter; virtual; abstract;
    function RtfImage(const Image: string; // 'SAD-4.1-functions.png 1101x738 85%'
      const Caption: string = ''; WriteBinary: boolean = true;
      const RtfHead: string = '\li0\fi0\qc'): TProjectWriter; virtual; abstract;
    /// if line is displayed as code, add it and return true
    function RtfCode(const line: string): boolean;
    procedure RtfPascal(const line: string; aFontSize: integer = 80);
    procedure RtfDfm(const line: string; aFontSize: integer = 80);
    procedure RtfC(const line: string);
    procedure RtfCSharp(const line: string);
    procedure RtfListing(const line: string);
    procedure RtfSgml(const line: string);
    procedure RtfModula2(const line: string);
    procedure RtfColLine(const line: string); virtual; 
    procedure RtfCols(const ColXPos: array of integer; FullWidth: integer;
      VertCentered, withBorder: boolean;
      const RowFormat: string = ''); virtual; abstract;
    // RowFormat can be '\trkeep' e.g.
    procedure RtfColsPercent(ColWidth: array of integer;
      VertCentered, withBorder: boolean;
      NormalIndent: boolean = false;
      RowFormat: string = ''); virtual; 
    // left=top, middle=center, right=bottom
    // must have been created with VertCentered=true
    procedure RtfColVertAlign(ColIndex: Integer; Align: TAlignment; DrawBottomLine: boolean=False);
    procedure RtfRow(const Text: array of string; lastRow: boolean=false); // after RtfCols
      virtual; abstract;
    function RtfColsEnd: TProjectWriter; virtual; abstract;
    procedure RtfEndSection; virtual; abstract;
    function RtfParDefault: TProjectWriter; virtual; abstract;
    procedure RtfHeaderBegin(aFontSize: integer); virtual; abstract;
    procedure RtfHeaderEnd; virtual; abstract;
    procedure RtfFooterBegin(aFontSize: integer); virtual; abstract;
    procedure RtfFooterEnd; virtual; abstract;
    procedure RtfTitle(Title: string; LevelOffset: integer = 0;
      withNumbers: boolean = true; Bookmark: string = ''); // level-indentated
      virtual; abstract;
    function RtfBookMark(const Text, BookmarkName: string): string; // returns bookmarkreal
      virtual; abstract;
    function RtfLinkTo(const aBookName, aText: string): TProjectWriter;
      virtual; abstract;
    function RtfPageRefTo(aBookName: string; withLink: boolean; BookMarkAlreadyComputed: boolean): TProjectWriter;
      virtual; abstract;
    procedure RtfSubTitle(const Title: string); // Title is put without numbers
      virtual; abstract;
    function RtfFont(SizePercent: integer): TProjectWriter; // change font size % DefFontSize
      virtual; abstract;
    function RtfFontString(SizePercent: integer): string; // idem with string
      virtual; abstract;
    function RtfBig(const text: string): TProjectWriter; // \par + bold + 110% size + \par
      virtual; abstract;
    function RtfGoodSized(const Text: string): string; virtual; abstract;
    procedure SetInfo(const aTitle, aAuthor, aSubject, aManager, aCompany: string); virtual; abstract;
    procedure Clear;
    procedure SaveToWriter(aWriter: TProjectWriter);
    procedure MovePortion(sourcePos, destPos, aLen: integer);
    function Data: string;
    property Last: TLastRTF read fLast write SetLast;
    property Len: Integer read WR.Len;
    property ColsCount: integer read fColsCount;
    property Landscape: boolean read fLandscape write SetLandscape;
  end;

  TRTF = class(TProjectWriter)
  protected
    procedure SetLast(const Value: TLastRTF); override;
    procedure SetLandscape(const Value: boolean); override;
  public
    constructor Create(const aLayout: TProjectLayout;
      aDefFontSizeInPts: integer; // in points
      aCodePage: integer = 1252;
      aDefLang: integer = $0409; // french is $040c (1036)
      aLandscape: boolean = false; CloseManualy: boolean = false;
      aTitleFlat: boolean = false; const aF0: string = 'Calibri';
      const aF1: string = 'Consolas'); override;
    procedure InitClose; override; // if CloseManualy was true
    procedure SaveToFile(Format: TSaveFormat; OldWordOpen: boolean); override;

    function AddRtfContent(const s: string): TProjectWriter; override;
    procedure RtfList(line: string); override;
    procedure RtfPage; override;
    function RtfLine: TProjectWriter; override;
    function RtfPar: TProjectWriter; override;
    function RtfImage(const Image: string; // 'SAD-4.1-functions.png 1101x738 85%'
      const Caption: string = ''; WriteBinary: boolean = true;
      const RtfHead: string = '\li0\fi0\qc'): TProjectWriter; override;
    /// if line is displayed as code, add it and return true
    procedure RtfCols(const ColXPos: array of integer; FullWidth: integer;
      VertCentered, withBorder: boolean;
      const RowFormat: string = ''); override;
    procedure RtfRow(const Text: array of string; lastRow: boolean=false); // after RtfCols
      override;
    function RtfColsEnd: TProjectWriter; override;
    procedure RtfEndSection; override;
    function RtfParDefault: TProjectWriter; override;
    procedure RtfHeaderBegin(aFontSize: integer); override;
    procedure RtfHeaderEnd; override;
    procedure RtfFooterBegin(aFontSize: integer); override;
    procedure RtfFooterEnd; override;
    // level-indentated
    procedure RtfTitle(Title: string; LevelOffset: integer = 0;
      withNumbers: boolean = true; Bookmark: string = ''); override;
    // returns bookmarkreal
    function RtfBookMark(const Text, BookmarkName: string): string; override;
    function RtfLinkTo(const aBookName, aText: string): TProjectWriter; override;
    function RtfPageRefTo(aBookName: string; withLink: boolean;
      BookMarkAlreadyComputed: boolean): TProjectWriter; override;
    // Title is put without numbers
    procedure RtfSubTitle(const Title: string); override;
    // change font size % DefFontSize
    function RtfFont(SizePercent: integer): TProjectWriter; override;
    // idem with string
    function RtfFontString(SizePercent: integer): string; override;
    // \par + bold + 110% size + \par
    function RtfBig(const text: string): TProjectWriter; override;
    function RtfGoodSized(const Text: string): string; override;
    procedure SetInfo(const aTitle, aAuthor, aSubject, aManager, aCompany: string); override;
  end;

  THtmlTag = (hBold, hItalic, hUnderline, hCode, hBR, hBRList, hNavy, hNavyItalic,
    hNbsp, hPre, hAHRef, hTable, hTD, hTR, hP, hTitle, hHighlight, hLT, hGT);
  THtmlTags = set of THtmlTag;
  THtmlTagsSet = array[boolean,THtmlTag] of AnsiString;

  THTML = class(TProjectWriter)
  protected
    Level: integer;
    Current: THtmlTags;
    InTable, HasLT: boolean;
    Stack: array[0..20] of THtmlTags; // stack to handle { }
    procedure SetLast(const Value: TLastRTF); override;
    procedure SetLandscape(const Value: boolean); override;
    procedure WriteAsHtml(P: PAnsiChar);
  public
    constructor Create(const aLayout: TProjectLayout;
      aDefFontSizeInPts: integer; // in points
      aCodePage: integer = 1252;
      aDefLang: integer = $0409; // french is $040c (1036)
      aLandscape: boolean = false; aCloseManualy: boolean = false;
      aTitleFlat: boolean = false; const aF0: string = 'Calibri';
      const aF1: string = 'Consolas'); override;
    procedure InitClose; override; // if CloseManualy was true
    procedure SaveToFile(Format: TSaveFormat; OldWordOpen: boolean); override;

    function AddRtfContent(const s: string): TProjectWriter; override;
    procedure RtfList(line: string); override;
    procedure RtfPage; override;
    function RtfLine: TProjectWriter; override;
    function RtfPar: TProjectWriter; override;
    function RtfImage(const Image: string; // 'SAD-4.1-functions.png 1101x738 85%'
      const Caption: string = ''; WriteBinary: boolean = true;
      const RtfHead: string = '\li0\fi0\qc'): TProjectWriter; override;
    /// if line is displayed as code, add it and return true
    procedure RtfCols(const ColXPos: array of integer; FullWidth: integer;
      VertCentered, withBorder: boolean;
      const RowFormat: string = ''); override;
    procedure RtfRow(const Text: array of string; lastRow: boolean=false); // after RtfCols
      override;
    function RtfColsEnd: TProjectWriter; override;
    procedure RtfEndSection; override;
    function RtfParDefault: TProjectWriter; override;
    procedure RtfHeaderBegin(aFontSize: integer); override;
    procedure RtfHeaderEnd; override;
    procedure RtfFooterBegin(aFontSize: integer); override;
    procedure RtfFooterEnd; override;
    procedure RtfTitle(Title: string; LevelOffset: integer = 0;
      withNumbers: boolean = true; Bookmark: string = ''); // level-indentated
      override;
    function RtfBookMark(const Text, BookmarkName: string): string; // returns bookmarkreal
      override;
    function RtfLinkTo(const aBookName, aText: string): TProjectWriter;
      override;
    function RtfPageRefTo(aBookName: string; withLink: boolean; BookMarkAlreadyComputed: boolean): TProjectWriter;
      override;
    procedure RtfSubTitle(const Title: string); // Title is put without numbers
      override;
    function RtfFont(SizePercent: integer): TProjectWriter; // change font size % DefFontSize
      override;
    function RtfFontString(SizePercent: integer): string; // idem with string
      override;
    function RtfBig(const text: string): TProjectWriter; // \par + bold + 110% size + \par
      override;
    function RtfGoodSized(const Text: string): string; override;
    procedure SetInfo(const aTitle, aAuthor, aSubject, aManager, aCompany: string); override;
  end;

  THeapMemoryStream = class(TMemoryStream)
  // allocates memory from Delphi heap (FastMM4) and not windows.Global*()
  // and uses bigger growing size -> a lot faster
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  end;


function TrimLastPeriod(const s: string; FirstCharLower: boolean = false): string; // delete last '.'
function RtfField(const FieldName: string): string; // RtfField('PAGE')
function RtfBackSlash(const Text: string): string; // RtfBackSlash('C:\Dir\')='C:\\Dir\\'
function RtfBookMarkName(const Name: string): string; // bookmark name compatible
function RtfBookMark(const Text, BookMarkName: string): string; // bookmark some Text
function RtfLinkTo(const aBookName, aText: string): string;
function RtfPageRefTo(aBookName: string; withLink: boolean): string;
function MM2Inch(mm: integer): integer; // MM2Inch(210)=11905
function Hex32(const C: cardinal): string; // return the hex value of a cardinal
function BookMarkHash(const s: string): string;
function ImageSplit(Image: string; out aFileName, iWidth, iHeight: string;
  out w,h, percent, Ext: integer): boolean;

{$ifdef DIRECTEXPORTTOWORD}
function RtfToDoc(Format: TSaveFormat; RtfFileName: string; OldWordOpen: boolean): boolean; // RTF -> native DOC format
{$endif}


function IsKeyWord(const KeyWords: array of string; const aToken: String): Boolean;
// aToken must be already uppercase
// note that 'array of string' deals with Const - not TStringDynArray

function IsNumber(P: PAnsiChar): boolean;

const
  VALID_PICTURES_EXT: array[0..3] of string =
  ('.JPG','.JPEG','.PNG','.EMF');

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

  XMLKEYWORDS: array[0..0] of string = ('');

  RTFEndToken: set of AnsiChar = [#0..#254]-['A'..'Z','a'..'z','0'..'9'];
  HTML_TAGS: THtmlTagsSet = (
    ('<b>','<i>','<u>','<code>','<br>','<br>','<font color="navy">','<font color="navy"><i>',
      '&nbsp;','<pre>','<a href="%s">','<table>','<td>','<tr>','<p>','<h3>',
      '<span style="background-color:yellow;">','&lt;','&gt;'),
    ('</b>','</i>','</u>','</code>','','','</font>','</i></font>',
      '','</pre>','</a>','</table>','</td>','</tr>','</p>','</h3>'#13#10,'</span>','',''));

procedure CSVValuesAddToStringList(const aCSV: string; List: TStrings); overload;
// add all values in aCSV into List[]

procedure CSVValuesAddToStringList(P: PChar; List: TStrings); overload;
// add all CSV values in P into List[]



implementation

uses
  Windows,
{$ifdef DIRECTEXPORTTOWORD}
  ActiveX, ComObj, Variants,
{$endif}
  ProjectDiff; // for TMemoryMap


{ THeapMemoryStream = faster TMemoryStream using FastMM4 heap, not windows.GlobalAlloc() }

const
  MemoryDelta = $8000; // 32kb growing size Must be a power of 2

function THeapMemoryStream.Realloc(var NewCapacity: Integer): Pointer;
// allocates memory from Delphi heap (FastMM4) and not windows.Global*()
// and uses bigger growing size -> a lot faster
var i: integer;
begin
  if (NewCapacity > 0) then begin
    i := Seek(0,soFromCurrent); // no direct access to fSize -> use Seek() trick
    if NewCapacity=Seek(0,soFromEnd) then begin // avoid ReallocMem() if just truncate
      result := Memory;
      Seek(i,soFromBeginning);
      exit;
    end;
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
    Seek(i,soFromBeginning);
  end;
  Result := Memory;
  if NewCapacity <> Capacity then begin
    if NewCapacity = 0 then begin
      FreeMem(Memory);
      Result := nil;
    end else begin
      if Capacity = 0 then
        GetMem(Result, NewCapacity) else
        ReallocMem(Result, NewCapacity);
      if Result = nil then raise EStreamError.Create('THeapMemoryStream');
    end;
  end;
end;


{ TStringWriter }

function TStringWriter.Init: PStringWriter;
begin
  fGrowBy := 2048; // first FastMM4 fast (small block), and then medium block size
  len := 0;
  fData := '';
  result := @self;
end;

function TStringWriter.Init(const Args: array of const): PStringWriter;
begin
  Init;
  result := AddArray(Args);
end;

function TStringWriter.Init(GrowBySize: integer): PStringWriter;
begin
  if GrowBySize<512 then
    GrowBySize := 512; // to avoid bug in AddShort(s1,s2)
  fGrowBy := GrowBySize;
  len := 0;
  fData := '';
  result := @self;
end;

procedure TStringWriter.SaveToStream(aStream: TStream);
begin
  if len>0 then
    aStream.Write(fData[1],len);
end;

procedure TStringWriter.SaveToWriter(const aWriter: TStringWriter);
begin
  aWriter.Write(fData[1],len);
end;

procedure TStringWriter.SetData(const Value: string);
begin
  len := length(Value);
  fdata := Value;
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A else
    Result := B;
end;

function TStringWriter.Add(const s: string): PStringWriter;
begin
  result := @self;
  sLen := length(s);
  if sLen=0 then exit;
  if len+sLen>length(fData) then
    SetLength(fData,length(fData)+Max(sLen,fGrowBy));
  move(s[1],fData[len+1],sLen);
  inc(len,sLen);
end;

function TStringWriter.AddPChar(p: pChar): PStringWriter;
begin
  result := @self;
  sLen := StrLen(p);
  if sLen=0 then exit;
  if len+sLen>length(fData) then
    SetLength(fData,length(fData)+Max(sLen,fGrowBy));
  move(p^,fData[len+1],sLen);
  inc(len,sLen);
end;

function TStringWriter.AddStringOfChar(ch: char; Count: integer): PStringWriter;
begin
  result := @self;
  if Count<=0 then exit;
  if len+Count>length(fData) then
    SetLength(fData,length(fData)+Max(Count,fGrowBy));
  fillchar(fData[len+1],Count,ord(ch));
  inc(len,Count);
end;

function TStringWriter.RtfBackSlash(Text: string; trimLastBackSlash: boolean = false): PStringWriter;
var i,j: integer;
begin
  result := @self;
  if Text='' then exit;
  if trimLastBackSlash and (Text[length(Text)]='\') then
    SetLength(Text,length(Text)-1);
  i := pos('\',Text);
  if i=0 then begin
    Add(Text);
    exit;
  end;
  j := 1;
  repeat
    AddCopy(Text,j,i);
    Add('\');
    j := i+1;
    i := posEx('\',Text,j);
  until i=0;
  AddCopy(Text,j,maxInt);
end;

function TStringWriter.AddCopy(const s: string; Index, Count: Integer): PStringWriter;
begin
  result := @self;
  sLen := length(s)+1;
  if Index>=sLen then
    exit;
  if cardinal(Index)+cardinal(Count)>cardinal(sLen) then // cardinal: Count can be = maxInt
    Count := sLen-Index;
  if Count<=0 then exit;
  if len+Count>length(fData) then
    SetLength(fData,length(fData)+Max(Count,fGrowBy));
  move(s[Index],fData[len+1],Count);
  inc(len,Count);
end;

function TStringWriter.Add(c: char): PStringWriter;
begin
  inc(len);
  result := @self;
  if (pointer(fData)=nil) or (len>pInteger(cardinal(fData)-4)^) then
//  if len>length(fData) then
    SetLength(fData,length(fData)+fGrowBy);
  fData[len] := c;
end;

function TStringWriter.Add(const s1, s2: string): PStringWriter;
var L1, l2: integer;
begin
  L1 := length(s1);
  L2 := length(s2);
  sLen := len+L1;
  if sLen+L2>length(fData) then
    SetLength(fData,length(fData)+Max(L1+L2,fGrowBy));
  move(s1[1],fData[len+1],L1);
  move(s2[1],fData[sLen+1],L2);
  len := sLen+L2;
  result := @self;
end;

function TStringWriter.Add(const s: array of string): PStringWriter;
var i: integer;
begin
  for i := 0 to high(s) do
    Add(s[i]);
  result := @self;
end;

function TStringWriter.AddShort(const s1, s2: shortstring): PStringWriter;
begin
  sLen := len+ord(s1[0]);
  if sLen+ord(s2[0])>length(fData) then
    SetLength(fData,length(fData)+fGrowBy); // fGrowBy is always >255+255
  move(s1[1],fData[len+1],ord(s1[0]));
  move(s2[1],fData[sLen+1],ord(s2[0]));
  len := sLen+ord(s2[0]);
  result := @self;
end;

function TStringWriter.Add(p: pChar; pLen: integer): PStringWriter;
begin
  result := @self;
  if pLen>0 then begin
    if (pointer(fData)=nil) or (len+pLen>pInteger(cardinal(fData)-4)^) then
      SetLength(fData,length(fData)+Max(pLen,fGrowby));
    move(p^,fData[len+1],pLen);
    inc(len,pLen);
  end;
end;

function TStringWriter.AddArray(const Args: array of const): PStringWriter;
// with XML standard for Float values
var i: integer;
    tmp: shortstring;
begin
  for i := 0 to high(Args) do
  with Args[i] do
  case VType of
    vtChar:       Add(VChar);
    vtWideChar:   Add(string(widestring(VWideChar)));
    vtString:     AddShort(VString^);
    vtAnsiString: Add(string(VAnsiString));
    vtWideString: Add(string(VWideString));
    vtPChar:      AddPChar(VPChar);
    vtInteger:    begin str(VInteger,tmp); AddShort(tmp); end;
    vtPointer:    AddHex32(cardinal(VPointer));
    vtInt64:      AddInt64(VInt64^);
    vtExtended:   Add(@tmp[0],FloatToText(@tmp[0],VExtended^,fvExtended,ffGeneral,18,0));
    vtCurrency:   Add(@tmp[0],FloatToText(@tmp[0],VCurrency^,fvCurrency,ffGeneral,18,0));
  end;
  result := @self;
end;

const
  hexChars: array[0..15] of Char = '0123456789ABCDEF';
  
function Hex32ToPChar(dest: pChar; aValue: cardinal): pChar;
// group by byte (2 hex chars at once): faster and easier to read
begin
  case aValue of
    $0..$FF: begin
    dest[1] := HexChars[aValue and $F];
    dest[0] := HexChars[aValue shr 4];
    result := dest+2;
    end;
    $100..$FFFF: begin
    dest[3] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[2] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[1] := HexChars[aValue and $F];
    dest[0] := HexChars[aValue shr 4];
    result := dest+4;
    end;
    $10000..$FFFFFF: begin
    dest[5] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[4] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[3] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[2] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[1] := HexChars[aValue and $F];
    dest[0] := HexChars[aValue shr 4];
    result := dest+6;
    end;
    else begin //$1000000..$FFFFFFFF: begin
    dest[7] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[6] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[5] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[4] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[3] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[2] := HexChars[aValue and $F]; aValue := aValue shr 4;
    dest[1] := HexChars[aValue and $F];
    dest[0] := HexChars[aValue shr 4];
    result := dest+8;
    end;
  end;
end;

function Hex32(const C: cardinal): string;
// return the hex value of a cardinal
var tmp: array[0..7] of char;
begin
  SetString(result,tmp,Hex32ToPChar(tmp,C)-tmp);
end;

function Hex64ToPChar(dest: pChar; aValue: Int64): pChar;
function Write8(dest: pChar; aValue: Cardinal): pChar;
begin
  dest[7] := HexChars[aValue and $F]; aValue := aValue shr 4;
  dest[6] := HexChars[aValue and $F]; aValue := aValue shr 4;
  dest[5] := HexChars[aValue and $F]; aValue := aValue shr 4;
  dest[4] := HexChars[aValue and $F]; aValue := aValue shr 4;
  dest[3] := HexChars[aValue and $F]; aValue := aValue shr 4;
  dest[2] := HexChars[aValue and $F]; aValue := aValue shr 4;
  dest[1] := HexChars[aValue and $F];
  dest[0] := HexChars[aValue shr 4];
  result := dest+8;
end;
begin
  with Int64Rec(aValue) do
  if Hi<>0 then
    result := Write8(Hex32ToPChar(dest,Hi),Lo) else
    result := Hex32ToPChar(dest,Lo);
end;

function TStringWriter.Add(const Format: string; const Args: array of const): PStringWriter;
// all standard commands are supported, but the most usefull are optimized
var i,j,  c, n, L: integer;
    decim: shortstring;
begin
  result := @self;
  n := length(Args);
  L := length(Format);
  if (n=0) or (L=0) then exit;
  i := 1;
  c := 0;
  while (i<=L) do begin
    j := i;
    while (i<=L) and (Format[i]<>'%') do inc(i);
    case i-j of
      0: ;
      1:   Add(Format[j]);
      else AddCopy(Format,j,i-j);
    end;
    inc(i);
    if i>L then break;
    if (Format[i] in ['0'..'9']) and (i<L) and (Format[i+1]=':') then begin
      c := ord(Format[i])-48;  // Format('%d %d %d %0:d %d',[1,2,3,4]) = '1 2 3 1 2'
      inc(i,2);
      if i>L then break;
    end;
    if Format[i]='%' then         // Format('%%') = '%'
      Add('%') else  // Format('%.3d',[4]) = '004':
    if (Format[i]='.') and (i+2<=L) and (c<n) and (Format[i+1] in ['1'..'9'])
      and (Format[i+2] in ['d','x']) and (Args[c].VType=vtInteger) then begin
      if Format[i+2]='d' then
        str(Args[c].VInteger,decim) else
        decim[0] := chr(Hex32ToPChar(@decim[1],Args[c].VInteger)-@decim[1]);
      for j := length(decim) to ord(Format[i+1])-49 do
        Add('0');
      AddShort(decim);
      inc(c);
      inc(i,2);
    end else
    if c<n then begin
      with Args[c] do
      case Format[i] of
      's': case VType of
        vtString:     AddShort(VString^);
        vtAnsiString: Add(string(VAnsiString));
        vtWideString: Add(string(VWideString));
        vtPChar:      AddPChar(VPChar);
        vtChar:       Add(VChar);
        vtInteger:    AddCardinal(VInteger); // extension from std FormatBuf
      end;
      'd': case VType of
        vtInteger: AddInteger(VInteger);
        vtInt64:   AddInt64(VInt64^);
      end;
      'x': case VType of
        vtInteger: AddHex32(VInteger);
        vtInt64:   AddHex64(VInt64^);
      end;
      else begin // all other formats: use standard FormatBuf()
        j := i-1; // Format[j] -> '%'
        while (i<=L) and not(NormToUpper[Format[i]] in ['A'..'Z']) do
          inc(i); // Format[i] -> 'g'
        Add(@decim[0],FormatBuf(decim[0],255,Format[j],i-j+1,Args[c]));
      end;
      end;
      inc(c);
    end;
    inc(i);
  end;
end;

function TStringWriter.AddWithoutPeriod(const s: string; FirstCharLower: boolean = false): PStringWriter;
var i: integer;
begin
  result := @self;
  if s='' then exit;
  slen := length(s);
  if s[slen]='.' then
   dec(slen);
  Add(pointer(s),slen);
  if FirstCharLower then begin
    i := len-slen;
    fData[i+1] := NormToLower[fData[i+1]];
  end;
end;

function TStringWriter.AddShort(const s: array of shortstring): PStringWriter;
var i: integer;
begin
  for i := 0 to high(s) do
    AddShort(s[i]);
  result := @self;
end;

function TStringWriter.DeleteLast(const s: string): boolean;
begin
  result := IsLast(s);
  if result then
    dec(len,length(s));
end;

function TStringWriter.IsLast(const s: string): boolean;
var L: integer;
begin
  L := length(s);
  result := (L=0) or ((L<=len) and (copy(fData,len-L+1,L)=s));
end;

function TStringWriter.EnsureLast(const s: string): PStringWriter;
// if last data is not s -> add(s)
begin
  result := @self;
  if not IsLast(s) then
    Add(s);
end;

function TStringWriter.RtfValid: boolean;
var i, Len1,Len2: integer;
begin
  Len1 := 0;
  Len2 := 0;
  for i := 1 to len do
    case fData[i] of
    '{': if (i=1) or (fData[i-1]<>'\') then inc(Len1);
    '}': if (i=1) or (fData[i-1]<>'\') then inc(Len2);
    end;
  result := Len1=Len2;
end;

function TStringWriter.AddShort(const s: shortstring): PStringWriter;
begin
  result := @self;
  sLen := ord(s[0]);
  if sLen=0 then exit;
  if (pointer(fData)=nil) or (len+sLen>pInteger(cardinal(fData)-4)^) then
//  if len+sLen>length(fData) then
    SetLength(fData,length(fData)+fGrowBy); // fGrowBy is always >255
  move(s[1],fData[len+1],sLen);
  inc(len,sLen);
end;

function TStringWriter.GetData: string;
begin
  if len<>length(fData) then
    SetLength(fData,len);
  result := fData;
end;

function TStringWriter.GetDataPointer: pointer;
begin
  result := pointer(fData);
end;

function TStringWriter.AddCardinal(value: cardinal): PStringWriter;
var tmp: string[15];
begin
  str(value,tmp);
  result := AddShort(tmp);
end;

function TStringWriter.AddInt64(value: Int64): PStringWriter;
// with FastMM4: using Int64Str (JOH version) is 8x faster than str()
begin
  result := Add(IntToStr(value));
end;

function TStringWriter.AddHex32(value: cardinal): PStringWriter;
var tmp: array[0..7] of char;
begin
  result := Add(tmp,Hex32ToPChar(tmp,value)-@tmp[0]);
end;

function TStringWriter.AddHex64(value: Int64): PStringWriter;
var tmp: array[0..15] of char;
begin
  result := Add(tmp,Hex64ToPChar(tmp,value)-@tmp[0]);
end;

function TStringWriter.AddInteger(const value: integer): PStringWriter;
begin
  result := Add(IntToStr(value));
end;

procedure TStringWriter.AddShortNoGrow(const s: shortstring);
var n: integer;    // faster: no Grow test
begin
  n := ord(s[0]);
  move(s[1],fData[len+1],n);
  inc(len,n);
end;

function TStringWriter.AddCRLF: PStringWriter;
begin
  result := @self;
  if (pointer(fData)=nil) or (len+2>pInteger(cardinal(fData)-4)^) then
    SetLength(fData,length(fData)+fGrowBy); // fGrowBy is always >2
  pWord(@fData[len+1])^ := $0a0d;  // CRLF = #13#10
  inc(len,2);
end;


procedure StringWriterTest;
var s: string;
    c: TStringWriter; // automatic Garbage Collector
begin
  fillchar(c,sizeof(c),0);
  c.Init.Add('<').Add(HexChars,4).Add('>');
  s := c.Add('Essai %s numéro %g et',['d''un',3.14159]).
    AddShort('</').AddByte(100).Add('>').Data;
  assert(s='<0123>Essai d''un numéro 3'+DecimalSeparator+'14159 et</100>');
end;

function ModDiv32(const Dividend, Divisor: Cardinal; out Quotient: Cardinal): Cardinal;
{ Quotient := Dividend mod Divisor;
  Result :=   Dividend div Divisor; }
asm
  push ecx
  mov ecx,edx
  xor edx,edx
  div ecx
  pop ecx
  mov [ecx],edx
end;

function TStringWriter.AddByte(value: byte): PStringWriter;
var dest: array[0..3] of char;
    tmp: cardinal;
begin
  if value<10 then
    result := Add(chr(value+48)) else
    if value<100 then // 10..99
      result := Add(@TwoDigitLookup[value],2) else begin // 100..999
      dest[0] := chr(ModDiv32(value,100,tmp)+48); // tmp=value mod 100
      pWord(@dest[1])^ := pWord(@TwoDigitLookup[tmp])^;
      result := Add(dest,3);
    end;
end;

function TStringWriter.AddWord(value: word): PStringWriter;
var dest: array[0..4] of char;
    tmp, t: cardinal;
begin
  if value<10 then
    result := Add(chr(value+48)) else
    if value<100 then // 10..99
      result := Add(@TwoDigitLookup[value],2) else begin
      t := ModDiv32(value,100,tmp); // t=value/100 tmp=value mod 100
      if t<10 then begin // 100..999
        dest[0] := chr(t+48);
        pWord(@dest[1])^ := pWord(@TwoDigitLookup[tmp])^;
        result := Add(dest,3);
      end else
      if t<100 then begin // 1000..9999
        pWord(@dest[0])^ := pWord(@TwoDigitLookup[t])^;
        pWord(@dest[2])^ := pWord(@TwoDigitLookup[tmp])^;
        result := Add(dest,4);
      end else begin // 10000..99999
        dest[0] := chr(ModDiv32(t,100,t)+48); // t=(value/100) mod 100
        pWord(@dest[1])^ := pWord(@TwoDigitLookup[t])^;
        pWord(@dest[3])^ := pWord(@TwoDigitLookup[tmp])^;
        result := Add(dest,5);
      end;
  end;
end;

function TStringWriter.AddCardinal6(cmd: char; value: cardinal; withLineNumber: boolean): PStringWriter;
// AddCardinal6('+',1234)=Add('+ 001234 ')
type PInt6 = ^TInt6;
     TInt6 = packed record cm,W0,W1,W2: TTwoDigit; sp: char; end;
var t,tmp: cardinal;
begin
  result := @self;
  if (pointer(fData)=nil) or (len+sizeof(TInt6)>pInteger(cardinal(fData)-4)^) then
    SetLength(fData,length(fData)+fGrowby);
  with PInt6(@fData[len+1])^ do begin
    cm[1] := cmd;
    cm[2] := ' ';
    inc(len,2);
    if not withLineNumber then
      exit;
    t := ModDiv32(value,100,tmp); // t=value/100 tmp=value mod 100
    W0 := TwoDigitLookup[ModDiv32(t,100,t)];
    W1 := TwoDigitLookup[t];
    W2 := TwoDigitLookup[tmp];
    sp := ' ';
  end;
  inc(len,sizeof(TInt6)-2);
end;

procedure TStringWriter.Write(const buf; bufLen: integer);
begin
  if bufLen<=0 then exit;
  if (pointer(fData)=nil) or (len+bufLen>pInteger(cardinal(fData)-4)^) then
    SetLength(fData,length(fData)+Max(bufLen,fGrowby));
  move(buf,fData[len+1],bufLen);
  inc(len,bufLen);
end;

procedure TStringWriter.Write(Value: integer);
begin
  if (pointer(fData)=nil) or (len+4>pInteger(cardinal(fData)-4)^) then
    SetLength(fData,length(fData)+fGrowby);
  move(Value,fData[len+1],4);
  inc(len,4);
end;

procedure TStringWriter.Write(const s: string);
var P: PChar;
    bufLen: integer;
begin
  P := pointer(S);
  if P=nil then exit;
  bufLen := PInteger(@P[-4])^;
  if (pointer(fData)=nil) or (len+bufLen>pInteger(cardinal(fData)-4)^) then
    SetLength(fData,length(fData)+Max(bufLen,fGrowby));
  move(P^,fData[len+1],bufLen);
  inc(len,bufLen);
end;

{function RtfWrite(Src,Dst: PChar): PChar;
var I: integer;
begin
  if Src<>nil then
  repeat
    if Src^=#0 then break else
    if Src^='\' then begin
      pWord(Dst)^ := ord('\')+ord('\')shl 8;
      inc(Dst,2);
      inc(Src);
    end else
    if Src^<#128 then begin
      Dst^ := Src^;
      inc(Dst);
      inc(Src);
    end else begin
      i := ord(Src^);
      Dst[0] := '\';
      Dst[1] := '''';
      Dst[2] := hexChars[i shr 4];
      Dst[3] := hexChars[i and $F];
      inc(Dst,4);
      inc(Src);
    end;
  until false;
  result := Dst;
end;

function TRTF.Rtf(const s: string): TProjectWriter;
begin
  result := self;
  WR.sLen := length(s)*4; // max chars that may be added
  if WR.sLen=0 then exit;
  if WR.len+WR.sLen>length(WR.fData) then
    SetLength(WR.fData,length(WR.fData)+Max(WR.sLen,WR.fGrowBy));
  WR.len := RtfWrite(pointer(s),PChar(pointer(WR.fData))+WR.len)-PChar(pointer(WR.fData));
end;}

function ImageSplit(Image: string; out aFileName, iWidth, iHeight: string;
  out w,h, percent, Ext: integer): boolean;
var i,j,err: integer;
begin
  result := false;
  if Image='' then exit;
  h := 0;
  w := 0;
  if Image[1]='%' then delete(Image,1,1);
  percent := 100;
  j := length(Image);
  if Image[j]='%' then // image width in page width %
    repeat
      dec(j);
      if not (Image[j] in ['0'..'9']) then begin
        percent := StrToIntDef(copy(Image,j+1,length(Image)-j-1),100);
        SetLength(Image,j-1);
        break;
      end;
    until false;
  aFileName := '';
  for i := length(Image) downto 1 do
    if Image[i]=' ' then begin
      aFileName := copy(Image,1,i-1);
      delete(Image,1,i);
      j := pos('x',Image);
      if j<2 then exit;
      iWidth := copy(Image,1,j-1);
      val(iWidth,w,err);
      if err<>0 then exit;
      iHeight := copy(Image,j+1,10);
      val(iHeight,h,err);
      if err<>0 then exit;
      break;
    end;
  j := 0;
  for i := length(aFileName) downto 1 do
    if aFileName[i]='.' then begin
      j := i;
      break;
    end;
  if j=0 then exit;
  Ext := GetStringIndex(VALID_PICTURES_EXT,copy(aFileName,j,5));
  if Ext>=0 then
    result := true;
end;

function TRTF.RtfImage(const Image: string;
  const Caption: string = ''; WriteBinary: boolean = true;
  const RtfHead: string = '\li0\fi0\qc'): TProjectWriter;
// 'SAD-4.1-functions.png 1101x738 85%' (100% default width)
var Map: TMemoryMap;
    w,h, percent, Ext: integer;
    aFileName, iWidth, iHeight: string;
begin
  result := self;
  if ImageSplit(Image, aFileName, iWidth, iHeight, w,h,percent,ext) then
  if Map.DoMap(aFileName) or Map.DoMap(PicturePath+aFileName) then
  try
    // write to
    Last := lastRtfImage;
    WR.Add('{').Add(RtfHead).AddShort('{\pict\');
    case Ext of
    0,1: WR.AddShort('jpegblip\picw');
      2: WR.AddShort('pngblip\picw');
      3: WR.AddShort('emfblip\picw');
    end;
    WR.Add(iWidth).AddShort('\pich').Add(iHeight);
    percent := (Width*percent) div 100;
    h := (h*percent) div w;
    WR.AddShort('\picwgoal').AddInteger(percent).AddShort('\pichgoal').AddInteger(h);
    if WriteBinary then
      WR.AddShort('\bin').AddInteger(Map._size).Add(' ').Add(PAnsiChar(Map.buf),Map._size) else
      WR.Add(' ').AddHexBuffer(PByte(Map.buf), Map._size);
    WR.AddShort('}\line ');
    if Caption<>'' then
      WR.Add(Caption).AddShort('\par');
    WR.AddShort('}'#13);
  finally
    Map.UnMap;
  end;
end;

function TStringWriter.AddHexBuffer(value: PByte; Count: integer): PStringWriter;
var Dst: PChar;
    i: integer;
begin
  result := @self;
  sLen := Count*2; // chars count that are added
  if sLen=0 then exit;
  if len+sLen>length(fData) then
    SetLength(fData,length(fData)+Max(sLen,fGrowBy));
  Dst := pointer(fData);
  inc(Dst,len);
  inc(len,sLen);
  for i := 1 to Count do begin
    Dst[0] := hexChars[Value^ shr 4];
    Dst[1] := hexChars[Value^ and $F];
    inc(Value);
    inc(Dst,2);
  end;
end;

procedure TStringWriter.SaveToFile(const aFileName: String);
var F: TFileStream;
begin
  F := TFileStream.Create(aFileName,fmCreate);
  SaveToStream(F);
  F.Free;
end;


function TStringWriter.GetPortion(aPos, aLen: integer): string;
begin
  SetString(result,PChar(@fData[aPos]),aLen);
end;

procedure TStringWriter.MovePortion(sourcePos, destPos, aLen: integer);
var Part: string;
begin
  Part := GetPortion(sourcePos, aLen);
  Insert(Part,fData,destPos);
  if sourcePos>destPos then
    inc(sourcePos,aLen);
  delete(fData,sourcePos,aLen);
end;


constructor TRTF.Create(const aLayout: TProjectLayout;
  aDefFontSizeInPts, aCodePage, aDefLang: integer;
  aLandscape, CloseManualy, aTitleFlat: boolean; const aF0, aF1: string);
begin
  inherited;
  WR.AddShort('{\rtf1\ansi\ansicpg').AddWord(aCodePage).AddShort('\deff0\deffs').
  AddInteger(aDefFontSizeInPts).AddShort('\deflang').AddInteger(aDefLang).
  AddShort('{\fonttbl{\f0\fswiss\fcharset0 ').Add(aF0).
  AddShort(';}{\f1\fmodern\fcharset0 ').Add(aF1).Add(';}}'+
      '{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;'+
      '\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;'+
      '\red255\green255\blue150;\red255\green255\blue255;\red22\green43\blue90;'+
      '\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;'+
      '\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;'+
      '\red235\green235\blue230;}'+ // full default colortbl (for \highlight)
      '\viewkind4\uc1\paperw').
  AddInteger(Layout.Page.Width).AddShort('\paperh').AddInteger(Layout.Page.Height).
  AddShort('\margl').AddInteger(Layout.Margin.Left).
  AddShort('\margr').AddInteger(Layout.Margin.Right).
  AddShort('\margt').AddInteger(Layout.Margin.Top).
  AddShort('\margb').AddInteger(Layout.Margin.Bottom);
  if LandScape then
    WR.AddShort('\landscape');
(*  if aHeaderTop<>0 then
    AddShort('\headery').AddInteger(aHeaderTop);
  if aFooterBottom<>0 then
    AddShort('\footery').AddInteger(aFooterBottom);
  if Header<>'' then
    AddShort('{\header ').Add(Header).Add('}'); *)
  if not CloseManualy then
    InitClose;
end;

procedure TRTF.InitClose;
begin
  WR.AddShort(#13'{');
  RtfParDefault;
  Last := lastRtfNone;
end;

procedure TRTF.RtfPage;
begin
  RtfText;
  WR.AddShort(#13'\page');
  RtfParDefault;
  fLastWasRtfPage := true;
end;

function TRTF.RtfPar: TProjectWriter;
begin
  RtfText;
  WR.AddShort('\par'#13);
  result := Self;
end;

function TRTF.RtfLine: TProjectWriter;
begin
  RtfText;
  WR.AddShort('\line'#13);
  result := self;
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

procedure TRTF.SaveToFile(Format: TSaveFormat; OldWordOpen: boolean);
begin
  RtfText;
  WR.AddShort('}}');
  if Format=fNoSave then exit;
  WR.SaveToFile(FileName);
  if OldWordOpen then
    Format := fDoc;
{$ifdef DIRECTEXPORTTOWORD}
  if RtfToDoc(Format,FileName,OldWordOpen) then begin
    DeleteFile(pointer(FileName)); // delete .rtf file and open .doc
    if Format=fPdf then
      FileName := ChangeFileExt(FileName,'.pdf') else
      FileName := ChangeFileExt(FileName,'.doc');
  end;
{$endif}
end;

procedure TRTF.RtfCols(const ColXPos: array of integer; FullWidth: integer;
  VertCentered, withBorder: boolean;
  const RowFormat: string);
var i: integer;
    s: string;
begin
  if Last=lastRtfTitle then
    WR.AddShort('{\sb0\sa0\fs12\par}'); // increase gap between title and table
  if withBorder and (length(ColXPos)>0) and (Last<>LastRtfNone) then
    WR.AddShort('{\sb0\sa0\fs6\brdrb\brdrs\brdrcf9\ri').
      AddInteger(FullWidth-ColXPos[High(ColXPos)]).AddShort('\par}');
  Last := lastRtfCols;
  WR.AddShort(#13'{\li0\fi0\ql ');
  fColsCount := length(ColXPos);
  fCols := '\trowd\trgaph108';
  fCols := fCols+RowFormat;
  for i := 0 to fColsCount-1 do begin
    s := '\cellx'+IntToStr(ColXPos[i]);
    if VertCentered then
      s := '\clvertalc'+s;
    if withBorder then // border top+bottom only (more modern look)
      fCols := fCols+'\clbrdrt\brdrs\brdrcf9\clbrdrb\brdrs\brdrcf9\cf9'+s else
//      fCols := fCols+'\clbrdrt\brdrs\clbrdrl\brdrs\clbrdrb\brdrs\clbrdrr\brdrs'+s else
    fCols := fCols+s;
  end;
end;

procedure TRTF.RtfRow(const Text: array of string; lastRow: boolean);
var i: integer;
begin
  if length(Text)<>fColsCount then begin
    for i := 1 to length(Text) do // bad count -> direct write, without table
      WR.Add(Text[i]).Add(' ');
    WR.AddShort('\par');
  end else begin
    WR.Add(fCols);
    for i := 0 to fColsCount-1 do
      WR.AddShort('\intbl ').Add(Text[i]).AddShort('\cell');
    WR.AddShort('\row'#13);
  end;
  if lastRow then
    RtfColsEnd;
end;

function TRTF.RtfColsEnd: TProjectWriter;
begin
  result := self;
  if fColsCount=0 then exit;
  WR.AddShort('}'#13);
  Last := LastRtfNone;
  fColsCount := 0;
end;

function IsNumber(P: PAnsiChar): boolean;
begin
  if P=nil then begin
    result := false;
    exit;
  end;
  while P^ in ['0'..'9'] do
    inc(P);
  result := (P^=#0);
end;

procedure TRTF.RtfTitle(Title: string;
  LevelOffset: integer = 0; withNumbers: boolean = true;
  Bookmark: string = '');
// Title is indentated with level
var i: integer;
    Num: string;
    LastWasRtfPage: boolean;
begin
  if LevelOffset>0 then
    TitleLevelCurrent := LevelOffset else
    TitleLevelCurrent := 0;
  for i := 1 to length(Title) do
    if Title[i]<>' ' then begin
      inc(TitleLevelCurrent,i);
      break;
    end;
  if TitleLevelCurrent=0 then exit;
  // TitleLevelCurrent = 1..length(TitleLevel)
  LastWasRtfPage := fLastWasRtfPage;
  Last := lastRtfTitle;
  if TitleLevelCurrent>1 then
    Delete(Title,1,TitleLevelCurrent-1-LevelOffset);
  if TitleLevelCurrent>length(TitleLevel) then
    TitleLevelCurrent := length(TitleLevel);
  inc(TitleLevel[TitleLevelCurrent-1]);
  if not TitleFlat and (TitleLevel[0]=0) then // intro = first chapter = no number
    withNumbers := false;
  fillchar(TitleLevel[TitleLevelCurrent],length(TitleLevel)-TitleLevelCurrent,0);
  if not TitleFlat and (TitleLevelCurrent=1) then begin
    if not LastWasRtfPage then
      WR.AddShort('\page');
    WR.AddShort('{\sb2400\sa140\brdrb\brdrs\brdrcf9\brdrw60\brsp60\ql\b\cf9\shad');
    RtfFont(240);
  end else begin
    WR.AddShort('{\sb280\sa0\ql\b\cf9\fi-').AddInteger(IndentWidth).
    AddShort('\li').AddInteger(TitleWidth*pred(TitleLevelCurrent)+IndentWidth);
    if TitleFlat then
      RtfFont(Max(140-TitleLevelCurrent*10,100)) else
      RtfFont(Max(180-TitleLevelCurrent*20,100));
  end;
  if withNumbers then begin
    for i := 1 to TitleLevelCurrent do
      WR.AddWord(TitleLevel[i-1]).Add('.');
    WR.Add(' ');
  end;
  if Bookmark='' then
    if TitlesList<>nil then // force every title to have a bookmark
      Bookmark := RtfBookMark(Title,'TITLE_'+IntToStr(TitlesList.Count)) else
      WR.Add(Title) else // no Bookmark -> only title
    if Bookmark='- 'then
      WR.Add(Title) else // Bookmark='-' -> force only title
      LastTitleBookmark := RtfBookMark(Title,BookMark); // title+Bookmark
  WR.AddShort('\par}'#13);
  if TitleWidth<>0 then
    WR.AddShort('\li').AddInteger(TitleWidth*pred(TitleLevelCurrent)).Add(' ');
  if (TitlesList<>nil) and (BookMark<>'-') then begin
    // '1.2.3 Title|BookMark',pointer(3)
    if not FullTitleInTableOfContent then begin
      i := pos('\line',Title);
      if i>0 then
        SetLength(Title,i-1);
    end;
    i := pos('{',Title);
    if i>0 then
      SetLength(Title,i-1);
    if Title<>'' then begin
      if withNumbers then begin
        Num := '';
        for i := 1 to TitleLevelCurrent do
          Num := Num+IntToStr(TitleLevel[i-1])+'.';
        Title := Num+' '+Title;
      end;
      TitlesList.AddObject(Title+'|'+Bookmark,pointer(TitleLevelCurrent));
    end;
  end;
end;

function TRTF.RtfBookMark(const Text, BookmarkName: string): string;
// return real BookMarkName
begin
  if BookmarkName='' then begin
    result := '';
    WR.Add(Text);
  end else begin
    result := RtfBookMarkName(BookmarkName);
    WR.AddShort('{\*\bkmkstart ').Add(result).Add('}').
    Add(Text).
    AddShort('{\*\bkmkend ').Add(result).Add('}');
  end;
end;

procedure TRTF.RtfSubTitle(const Title: string);
// Title is put without numbers
begin
  RtfText.WR.AddShort('{\sb280\sa0\ql\fi-').AddInteger(IndentWidth);
  if TitleWidth<>0 then
    WR.AddShort('\li').AddInteger(TitleWidth*pred(TitleLevelCurrent)+IndentWidth);
  RtfFont(Max(160-TitleLevelCurrent*20,100));
  WR.Add(Title).AddShort('\par}'#13);
end;

function TRTF.RtfFont(SizePercent: integer): TProjectWriter;
// change font size % DefFontSize
begin
  WR.AddShort('\fs').AddWord((FontSize*SizePercent)div 100).Add(' ');
  result := self;
end;

function TRTF.RtfFontString(SizePercent: integer): string;
// change font size % DefFontSize - result in string
begin
  result := '\fs'+IntToStr((FontSize*SizePercent)div 100)+' ';
end;

procedure TRTF.SetLast(const Value: TLastRTF);
begin
  if ListLine then // true -> \line, not \par in RtfList()
    if (Value<>LastRtfList) and (fLast=LastRtfList) then
      WR.AddShort('\sa80\par}'#13);
  fLast := Value;
  if Value<>lastRtfNone then
    fLastWasRtfPage := false;
end;

procedure TRTF.RtfList(line: string);
var c: char;
begin
  if line='' then exit;
  if ListLine then // true -> \line, not \par in RtfList()
    if (fLast<>LastRtfList) then begin
      if WR.DeleteLast('\par'#13) then
        WR.AddShort('\sa40\par\sa80');
      WR.AddShort(#13'{\ql\sa0\sb0\fi-').AddInteger(IndentWidth).
      AddShort('\li').AddInteger(TitleWidth*TitleLevelCurrent+IndentWidth).
      Add(' ');
    end else begin
      WR.AddShort('\par'#13);
    end;
  Last := lastRtfList;
  c := line[1];
  repeat
    delete(line,1,1);
  until (line='') or (line[1]<>' ');
  if line='' then exit;
  WR.Add(c).AddShort('\tab ').Add(line);
  if not ListLine then // true -> \line, not \par in RtfList()
    WR.AddShort('\par'#13);
end;

function TRTF.RtfBig(const text: string): TProjectWriter;
// bold + 110% size + \par
begin
  RtfText.WR.Add('{\b\cf9');
  RtfFont(110);
  WR.Add(Text).AddShort('\par}');
  result := self;
end;

function TRTF.RtfGoodSized(const Text: string): string;
begin
  if (length(Text)>8) and not IdemPChar(pointer(Text),'DI-') then
    Result := '{'+RtfFontString(91)+Text+'}' else
    Result := Text; 
end;


{$ifdef DIRECTEXPORTTOWORD}
var
  CoInitialized: boolean = false;


function RtfToDoc(Format: TSaveFormat; RtfFileName: string; OldWordOpen: boolean): boolean;
// convert an RTF file to native DOC format, using installed Word exe
var vMSWord, vMSDoc: variant;
    ClassID: TCLSID;
    Unknown: IUnknown;
    Disp: IDispatch;
    Path, DocFileName, FN: string;
    i: integer;
Const wdOpenFormatRTF = 3;
      wdOpenFormatAuto = 0;
      wdExportFormatPDF = 17;
      msoEncodingAutoDetect = 50001;
begin
  result := false; // leave as RTF if conversion didn't succeed
{$WARN SYMBOL_PLATFORM OFF}
  if DebugHook<>0 then
    exit; // OLE automation usualy fails inside Delphi IDE
{$WARN SYMBOL_PLATFORM ON}
  // 1. init MSWord OLE communication
  if not CoInitialized then begin
    CoInitialize(nil);
    CoInitialized := true;
  end;
  try
    try
      ClassID := ProgIDToClassID('Word.Application');
      if not Succeeded(GetActiveObject(ClassID, nil, Unknown)) or // get active
         not Succeeded(Unknown.QueryInterface(IDispatch, Disp)) then
         if not Succeeded(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
        CLSCTX_LOCAL_SERVER, IDispatch, Disp)) then // create msWord instance
          exit; // no msWord -> leave as RTF
      vMSWord := Disp;
      while vMSWord.Templates.Count=0 do // Office 2010 fix
        // http://stackoverflow.com/questions/5913665/word-2010-automation-goto-bookmark
        Sleep(200);
      // 2. remote control of msWord to convert from .rtf to .doc
      Path := ExtractFilePath(RtfFileName);
      if Path='' then
        Path := GetCurrentDir else
        RtfFileName := ExtractFileName(RtfFileName);
      vMSWord.ChangeFileOpenDirectory(Path);
      // close any opened version
      for i := vMSWord.Documents.Count downto 1 do begin
        vMSDoc := vMSWord.Documents.Item(i);
        FN := ExtractFileName(vMSDoc.Name);
        if ValAt(FN,0,'.')=ValAt(RtfFileName,0,'.') then
          vMSDoc.Close(0,EmptyParam,EmptyParam);
      end;
      // invisible 'open' + 'save as' = convert .rtf to .doc
      if OldWordOpen then
        vMSDoc := vMSWord.Documents.Open(
          RtfFileName, //FileName,
          false) else
        vMSDoc := vMSWord.Documents.Open(
          RtfFileName, //FileName,
          false, //ConfirmConversions,
          false, //ReadOnly,
          false, //AddToRecentFiles,
          '', //PasswordDocument,
          '', //PasswordTemplate,
          true, //Revert,
          '', //WritePasswordDocument,
          '', //WritePasswordTemplate,
          wdOpenFormatRTF, //Format,
          msoEncodingAutoDetect, //Encoding,
          false //Visible,
          //OpenConflictDocument,
          //OpenAndRepair,
          //DocumentDirection,
          //NoEncodingDialog
      );
      try
        if Format=fPdf then begin
          DocFileName := ChangeFileExt(RtfFileName, '.pdf');
          vMSDoc.SaveAs(DocFileName, wdExportFormatPDF);
        end else begin
          DocFileName := ChangeFileExt(RtfFileName, '.doc');
          vMSDoc.SaveAs(DocFileName, wdOpenFormatAuto);
        end;
        result := true;
      finally
        vMSDoc.Close(0,EmptyParam,EmptyParam);
      end;
    finally
      vMSWord := unassigned;
    end;
  except
    on Exception do
      MessageBox(0,'Error during the document creation.'#13#13+
        'Did''nt you miss a \ or a } in your text?',nil,MB_ICONERROR);
  end;
end;
{$endif}

function RtfField(const FieldName: string): string;
// ex: RtfField('PAGE')
begin
  result := '{\field{\*\fldinst '+FieldName+'}}';
end;

// RtfBackSlash('C:\Dir\')='C:\\Dir\\'
function RtfBackSlash(const Text: string): string;
var i: integer;
begin
  result := Text;
  i := pos('\',Text);
  if i>0 then
  repeat
    insert('\',result,i); // result[i]='\\'
    i := posEx('\',result,i+2);
  until i=0;
end;

function BookMarkHash(const s: string): string;
begin
  result := Hex32(Adler32Asm(0,pointer(s),length(s)));
end;

var
  RtfBookMarkNameName,
  RtfBookMarkNameValue: string; // cache for RtfBookMarkName()

function RtfBookMarkName(const Name: string): string;
// bookmark name compatible with rtf (AlphaNumeric+'_')
var i: integer;
begin
  if Name=RtfBookMarkNameName then begin
    result := RtfBookMarkNameValue;
    exit;
  end;
  RtfBookMarkNameName := Name;
  result := UpperCase(Name);
  for i := 1 to length(result) do
    if not (result[i] in ['0'..'9','A'..'Z']) then
      result[i] := '_';
  RtfBookMarkNameValue := result;
end;

function RtfBookMark(const Text, BookMarkName: string): string;
// bookmark and write some rtf Text
var BookMark: string;
begin
  BookMark := RtfBookMarkName(BookMarkName);
  result :='{\*\bkmkstart '+BookMark+'}'+ Text
          +'{\*\bkmkend '+BookMark+'}';
end;

function RtfLinkTo(const aBookName, aText: string): string;
begin
  result := '{\field{\*\fldinst HYPERLINK \\l "'+
    RtfBookMarkName(aBookName)+'"}{\fldrslt '+aText+'}}';
end;

function TrimLastPeriod(const s: string; FirstCharLower: boolean = false): string;
begin
  if s='' then
    result := '' else begin
    if s[length(s)]<>'.' then
      result := s else
      result := copy(s,1,length(s)-1);
    if FirstCharLower then
      result[1] := NormToLower[result[1]];
  end;
end;

function MM2Inch(mm: integer): integer;
// MM2Inch(210)=11905 : A4 width paper size
begin
  result := ((1440*1000)*mm) div 25400;
end;

function RtfPageRefTo(aBookName: string; withLink: boolean): string;
begin
  if aBookName='' then begin
    result := '';
    exit;
  end;
  aBookName := RtfBookMarkName(aBookName);
  result := '{\field{\*\fldinst PAGEREF "'+aBookName+'"}}';
  if withLink then
    result := '{\field{\*\fldinst HYPERLINK \\l "'+aBookName+'"}{\fldrslt '+
      result+'}}';
end;

procedure CSVValuesAddToStringList(const aCSV: string; List: TStrings); overload;
// add all values in aCSV into List[]
var P: PChar;
    s: string;
begin
  P := pointer(aCSV);
  if List<>nil then
  while P<>nil do begin
   s := GetNextItem(P);
   if s<>'' then
     List.Add(s);
  end;
end;

procedure CSVValuesAddToStringList(P: PChar; List: TStrings); overload;
// add all CSV values in P into List[]
var s: string;
begin
  if List<>nil then
  while P<>nil do begin
   s := GetNextItem(P);
   if s<>'' then
     List.Add(s);
  end;
end;

procedure TRTF.RtfEndSection;
begin
  WR.AddShort('\sect}\endhere{');
end;

function TRTF.RtfParDefault: TProjectWriter;
begin
  WR.AddShort('\pard\plain\sb80\sa80\qj');
  if TitleWidth<>0 then
    WR.AddShort('\li').AddInteger(TitleWidth*pred(TitleLevelCurrent));
  result := RtfFont(100);
end;

procedure TRTF.RtfFooterBegin(aFontSize: integer);
begin
  WR.AddShort('{\footer\cf9 ');
  if aFontsize<>0 then
    RtfFont(aFontSize);
end;

procedure TRTF.RtfFooterEnd;
begin
  WR.Add('}');
  fLastWasRtfPage := true;
end;

procedure TRTF.RtfHeaderBegin(aFontSize: integer);
begin
  WR.AddShort('{\header\cf9 ');
  if aFontsize<>0 then
    RtfFont(aFontSize);
end;

procedure TRTF.RtfHeaderEnd;
begin
  WR.Add('}');
end;

function TRTF.RtfLinkTo(const aBookName, aText: string): TProjectWriter;
begin
  WR.AddShort('{\field{\*\fldinst HYPERLINK \\l "').
    Add(RtfBookMarkName(aBookName)).
    AddShort('"}{\fldrslt ').Add(aText).AddShort('}}');
  result := self;
end;

function TRTF.RtfPageRefTo(aBookName: string; withLink: boolean;
  BookMarkAlreadyComputed: boolean): TProjectWriter;
begin
  result := Self;
  if aBookName='' then
    exit;
  if not BookMarkAlreadyComputed then
    aBookName := RtfBookMarkName(aBookName);
  if withLink then
    WR.AddShort('{\field{\*\fldinst HYPERLINK \\l "').Add(aBookName).
    AddShort('"}{\fldrslt ');
  WR.AddShort('{\field{\*\fldinst PAGEREF "').Add(aBookName).AddShort('"}}');
  if withLink then
    WR.AddShort('}}');
end;

function TRTF.AddRtfContent(const s: string): TProjectWriter;
begin
  WR.Add(s);
  result := self;
end;

procedure TRTF.SetInfo(const aTitle, aAuthor, aSubject, aManager, aCompany: string);
begin
  WR.AddShort('{\info{\title ').Add(aTitle).
     AddShort('}{\author ').Add(aAuthor).
     AddShort('}{\operator ').Add(aAuthor).
     AddShort('}{\subject ').Add(aSubject).
     AddShort('}{\doccomm Document auto-generated by SynProject http://synopse.info').
     AddShort('}{\*\manager ').Add(aManager).
     AddShort('}{\*\company ').Add(aCompany).
     AddShort('}}');
end;

procedure TRTF.SetLandscape(const Value: boolean);
begin
  if Value=fLandscape then
    exit;
  inherited;
  if Value then begin
    WR.AddShort('\sect}'+
      '\sectd\ltrsect\lndscpsxn\pgwsxn').AddInteger(Layout.Page.Height). // switch paper size
      AddShort('\pghsxn').AddInteger(Layout.Page.Width).
      AddShort('\marglsxn').AddInteger(Layout.Margin.Left). // don't change margins
      AddShort('\margrsxn').AddInteger(Layout.Margin.Right).
      AddShort('\margtsxn').AddInteger(Layout.Margin.Top).
      AddShort('\margbsxn').AddInteger(Layout.Margin.Bottom).
      AddShort('\endhere{');
    Width := Layout.Page.Height-Layout.Margin.Left-Layout.Margin.Right;
  end else begin
    WR.AddShort('\sect}'+
      '\sectd\ltrsect\pghsxn').AddInteger(Layout.Page.Height). // switch paper size
      AddShort('\pgwsxn').AddInteger(Layout.Page.Width).
      AddShort('\marglsxn').AddInteger(Layout.Margin.Left).
      AddShort('\margrsxn').AddInteger(Layout.Margin.Right).
      AddShort('\margtsxn').AddInteger(Layout.Margin.Top).
      AddShort('\margbsxn').AddInteger(Layout.Margin.Bottom).
      AddShort('\endhere{');
    Width := Layout.Page.Width-Layout.Margin.Left-Layout.Margin.Right;
  end;
end;


{ TProjectWriter }

procedure TProjectWriter.Clear;
begin
  WR.len := 0;
end;

procedure TProjectWriter.SaveToWriter(aWriter: TProjectWriter);
begin
  WR.SaveToWriter(aWriter.WR);
end;

class function TProjectWriter.CreateFrom(
  aParent: TProjectWriter): TProjectWriter;
begin
  result := TProjectWriterClass(aParent.ClassType).InternalCreate;
  result.FontSize := aParent.FontSize;
end;

constructor TProjectWriter.InternalCreate;
begin
  WR.Init;
end;

function TProjectWriter.AddRtfContent(const fmt: string;
  const params: array of const): TProjectWriter;
begin
  result := AddRtfContent(format(fmt,params));
end;

function TProjectWriter.AddWithoutPeriod(const s: string;
  FirstCharLower: boolean): TProjectWriter;
var tmp: string;
    L: integer;
begin
  result := self;
  if s='' then
    exit;
  L := length(s);
  if s[L]<>'.' then
    tmp := s else
    if L=1 then
      exit else
      SetString(tmp,PChar(pointer(s)),L-1);
  if FirstCharLower then
    tmp[1] := NormToLower[tmp[1]] else
    tmp[1] := NormToUpper[tmp[1]];
  AddRtfContent(tmp);
end;

constructor TProjectWriter.Create(const aLayout: TProjectLayout;
  aDefFontSizeInPts, aCodePage, aDefLang: integer; aLandscape,
  aCloseManualy, aTitleFlat: boolean; const aF0, aF1: string);
begin
  InternalCreate;
  aDefFontSizeInPts := aDefFontSizeInPts*2;
  FontSize := aDefFontSizeInPts;
  fLandscape := aLandscape;
  Layout := aLayout;
  if LandScape then begin
    Layout.Page.Width := aLayout.Page.Height;
    Layout.Page.Height := aLayout.Page.Width;
  end;
  Width := Layout.Page.Width-Layout.Margin.Left-Layout.Margin.Right;
  TitleFlat := aTitleFlat;
  TitleWidth := 0;     // title indetation gap (default 0 twips)
  IndentWidth := 240;  // list or title first line indent (default 240 twips)
  if not TitleFlat then
    TitleLevel[0] := -1; // introduction = first chapter = no number
  // inherited should finish with:  if not CloseManualy then InitClose;
end;

procedure TProjectWriter.SetLandscape(const Value: boolean);
begin
  fLandscape := Value;
end;

procedure TProjectWriter.MovePortion(sourcePos, destPos, aLen: integer);
begin
  WR.MovePortion(sourcePos,destPos,aLen);
end;

function TProjectWriter.Data: string;
begin
  result := WR.Data;
end;

function TProjectWriter.RtfText: TProjectWriter;
begin
  Last := lastRtfText; // set Last -> update any pending {} (LastRtfList e.g.)
  fKeyWordsComment := false; // force end of comments
  result := self;
end;

procedure TProjectWriter.RtfColVertAlign(ColIndex: Integer;
  Align: TAlignment; DrawBottomLine: boolean);
var i, j: integer;
begin
  j := 1;
  for i := 0 to ColIndex do begin
   j := PosEx('\clvertal',fCols,j)+9;
   if j=9 then exit;
  end;
  case Align of // left=top, middle=center, right=bottom
    taLeftJustify:  fCols[j] := 't';
    taCenter:       fCols[j] := 'c';
    taRightJustify: fCols[j] := 'b';
  end;
  if DrawBottomLine then
    insert('\clbrdrb\brdrs\brdrcf9\brdrw24\brsp24',fCols,j-9) else
    if IdemPChar(@fCols[j-46],'\CLBRDR') then
      Delete(fCols,j-46,37);
end;

procedure TProjectWriter.RtfColsPercent(ColWidth: array of integer;
  VertCentered, withBorder, NormalIndent: boolean;
  RowFormat: string);
var x, w, i, ww: integer;
begin
  if NormalIndent then begin
    x := TitleWidth*pred(TitleLevelCurrent);
    ww := Width-x;
    if x<>0 then
      RowFormat := RowFormat+'\trleft'+IntToStr(x);
  end else begin
    x := 0;
    ww := Width;
  end;
  for i := 0 to high(ColWidth) do begin
    w := (ww*ColWidth[i]) div 100;
    inc(x,w);
    ColWidth[i] := x;
  end;
  RtfCols(ColWidth, ww, VertCentered, withBorder, RowFormat);
end;

procedure TProjectWriter.RtfKeywords(line: string; const KeyWords: array of string; aFontSize: integer = 80);
function GetLineContent(start,count: integer): string;
var k: Integer;
begin
  result := Copy(line,start,count);
  if @KeyWords=@PASCALKEYWORDS then // not already done before
    for k := length(result) downto 1 do
      if result[k] in ['{','}'] then
        insert('\',result,k);
end;
var s, comment: string;
    i,j,k, cf: integer;
    CString, PasString, yellow, navy: boolean;
function IsDfmHexa(p: PAnsiChar): boolean;
begin
  result := false;
  while (p^=' ') do inc(p);
  while p^<>#0 do
    if p^ in ['0'..'9','A'..'Z','}','\'] then
      inc(p) else
      exit;
  result := true;
end;
var IgnoreKeyWord: boolean;
begin
  if line='' then begin
    AddRtfContent('\line'#13);
    exit;
  end;
  CString := (@KeyWords=@MODULA2KEYWORDS) or (@KeyWords=@CKEYWORDS)
          or (@KeyWords=@CSHARPKEYWORDS);
  PasString := (@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@DFMKEYWORDS);
  for i := length(line) downto 1 do
    case line[i] of
    '{','}': if (@KeyWords<>@PASCALKEYWORDS) then insert('\',line,i);
    '\': if not IdemPChar(@PChar(pointer(line))[i],'LINE') then // '\line' is allowed
      insert('\',line,i);
    end;
  IgnoreKeyWord := (length(KeyWords)=0) or (@KeyWords=@XMLKEYWORDS);
  navy := false;
  if @KeyWords=@DFMKEYWORDS then begin
    i := pos(' = ',line);
    if i>0 then begin
      insert('\cf9 ',line,i+3); // right side in navy color
    end else
    if IsDfmHexa(pointer(line)) then begin
      IgnoreKeyWord := true;
      navy := true;
    end;
  end;
  if Last<>lastRtfCode then begin
    Last := lastRtfCode;
    AddRtfContent('{\sa0\sb80\f1\cbpat16\ql');
  end else
    AddRtfContent('{\sa0\sb0\f1\cbpat16\ql');
  RtfFont(aFontSize);
  yellow := line[1]='!';
  if yellow then
    i := 2 else
    i := 1;
  // write leading spaces before \highlight
  while line[i]=' ' do begin
    AddRtfContent(' ');
    inc(i);
  end;
  yellow := line[1]='!';
  if yellow then
    AddRtfContent('\highlight7 ');
  if navy then
    AddRtfContent('\cf9 ');
  // C-style '//' comment detection
  comment := '';
  if not IgnoreKeyWord and not (@KeyWords=@DFMKEYWORDS) then begin
    j := Pos('//',line);
    if j>0 then begin
      comment := GetLineContent(j,maxInt);
      SetLength(line,j-1);
      if i>length(line) then
        line := '';
    end;
  end;
  if line<>'' then
    if @KeyWords=@XMLKEYWORDS then begin
      cf := 0;
      repeat
        case line[i] of
        #0: break;
        '<': begin AddRtfContent('\cf12 '); cf := 12; end;
        '>': begin if cf<>12 then AddRtfContent('\cf12 ');
                   AddRtfContent('>\cf0 '); cf := 0; inc(i); continue; end;
        '&': begin AddRtfContent('\cf10 '); cf := 10; end;
        ';': if cf=10 then begin
               AddRtfContent(';\cf0 '); cf := 0; inc(i); continue; end;
        '"': if cf=9 then begin
               AddRtfContent('"\cf12 '); cf := 12; inc(i); continue; end else
             if cf=12 then begin
               AddRtfContent('"\cf9 '); cf := 9; inc(i); continue; end;
        end;
        AddRtfContent(line[i]);
        inc(i);
      until false;
    end else
    if IgnoreKeyWord then // for '$ Simple listing' or DFM hexa
      AddRtfContent(Copy(line,i,maxInt)) else
    repeat
      // write leading spaces
      while line[i]=' ' do begin
        AddRtfContent(line[i]);
        inc(i);
      end;
      // '(* Pascal or Modula-2 comment *)'
      if (Line[i]='(') and (Line[i+1]='*') and
         ((@KeyWords=@PASCALKEYWORDS) or (@KeyWords=@MODULA2KEYWORDS)) then begin
        k := PosEx('*)',line,i+2);
        if k>0 then begin
          inc(k,2);
          AddRtfContent('{\cf9\i '+GetLineContent(i,k-i)+'}');
          i := k;
          continue;
        end;
      end;
      // '{ pascal s }'
      if @KeyWords=@PASCALKEYWORDS then
      case Line[i] of
      '{': begin
        k := PosEx('}',line,i+1);
        if k>0 then begin
          inc(k);
          AddRtfContent('{\cf9\i\{'+GetLineContent(i+1,k-i-2)+'\}}');
          i := k;
          continue;
        end else begin
          AddRtfContent('\{');
          inc(i);
        end;
      end;
      '}': begin // a line with no beginning of } -> full line of comment
          AddRtfContent('\}');
          inc(i);
      end;
      end;
      // '/* C comment */'
      if ((@KeyWords=@CKEYWORDS)or(@KeyWords=@CSHARPKEYWORDS)) and
        (Line[i]='/') and (Line[i+1]='*') then begin
        k := PosEx('*/',line,i+2);
        if k>0 then begin
          inc(k,2);
          AddRtfContent('{\cf9\i '+Copy(line,i,k-i)+'}');
          i := k;
          continue;
        end;
      end;
      // get next word till end word char
      j := 0;
      k := i;
      while line[k]<>#0 do
        if (@KeyWords=@MODULA2KEYWORDS) and (line[k]='|') then begin
          j := k;
          break;
        end else
        if CString and (line[k]='"') then begin
          if (line[k+1]='"') then
            // ignore ""
            inc(k,2) else begin
            j := k;
            break;
          end;
        end else
        if line[k]='''' then begin
          if PasString and (line[k+1]='''') then
            // ignore pascal ''
            inc(k,2) else begin
            j := k;
            break;
          end;
        end else
        if not (line[k] in ['0'..'9','a'..'z','A'..'Z','_']) then begin
          // we reached a non identifier-valid char, i.e. an end word char
          j := k;
          break;
        end else
          inc(k);
      // write word as keyword if the case
      if j=0 then
        s := copy(line,i,maxInt) else
        s := copy(line,i,j-i);
      if s<>'' then
        if (CString or PasString) and IsNumber(pointer(s)) then
          // write const number in blue 
          AddRtfContent('{\cf9 '+s+'}') else
        if ((@KeyWords=@MODULA2KEYWORDS) and
              IsKeyWord(KeyWords,s)) or // .MOD keys are always uppercase
           ((@KeyWords<>@MODULA2KEYWORDS) and IsKeyWord(KeyWords,UpperCase(s))) then
          // write keyword in bold
          AddRtfContent('{\b '+s+'}') else
          // write normal word
          AddRtfContent(s);
      // handle end word char (in line[j])
      if j=0 then break;
      i := j+1;
      if (@KeyWords=@MODULA2KEYWORDS) and (line[j]='|') then
        AddRtfContent('{\b |}') else // put CASE | in bold
      if PasString and (line[j]='''') and (line[i]<>'''') then begin
        AddRtfContent('{\cf9 ''');
        while line[i]<>#0 do begin // put pascal 'string'
          if (@KeyWords=@PASCALKEYWORDS) and (line[i] in ['{','}']) then
            AddRtfContent('\');
          AddRtfContent(line[i]);
          inc(i);
          if line[i-1]='''' then break;
        end;
        AddRtfContent('}');     
        if line[i]=#0 then break;
      end else
      if CString and (line[j]='"') and (line[i]<>'"') then begin
        AddRtfContent('{\cf9 "');
        while line[i]<>#0 do begin // handle "string"
          AddRtfContent(line[i]);
          inc(i);
          if line[i-1]='"' then break;
        end;
        AddRtfContent('}');
        if line[i]=#0 then break;
      end else begin
        if (@KeyWords=@PASCALKEYWORDS) and (line[j] in ['{','}']) then
          AddRtfContent('\');
        AddRtfContent(line[j]); // write end word char
      end;
    until false;
  if comment<>'' then // write comment in italic
    AddRtfContent('{\cf9\i '+comment+'}');
  AddRtfContent('\par}'#13);
end;

function TProjectWriter.RtfCode(const line: string): boolean;
begin
  result := false;
  if line='' then
    exit;
  case line[1] of
    '!':
    case line[2] of
      '$': RtfDfm(copy(line,3,maxInt));
      else RtfPascal(copy(line,2,maxInt));
    end;
    '&': RtfC(copy(line,2,maxInt));
    '#': RtfCSharp(copy(line,2,maxInt));
    'µ': RtfModula2(copy(line,2,maxInt));
    '$': 
    case line[2] of
      '$': RtfSgml(copy(line,3,maxInt));
      else RtfListing(copy(line,2,maxInt));
    end;
  else exit;
  end;
  result := true;
end;

procedure TProjectWriter.RtfPascal(const line: string; aFontSize: integer = 80);
begin
  RtfKeywords(line, PASCALKEYWORDS, aFontSize);
end;

procedure TProjectWriter.RtfDfm(const line: string; aFontSize: integer);
begin
  RtfKeywords(line, DFMKEYWORDS, aFontSize);
end;

procedure TProjectWriter.RtfC(const line: string);
begin
  RtfKeywords(line, CKEYWORDS);
end;

procedure TProjectWriter.RtfCSharp(const line: string);
begin
  RtfKeywords(line, CSHARPKEYWORDS);
end;

procedure TProjectWriter.RtfModula2(const line: string);
begin
  RtfKeywords(line, MODULA2KEYWORDS);
end;

procedure TProjectWriter.RtfListing(const line: string);
begin
  RtfKeywords(line, []);
end;

procedure TProjectWriter.RtfSgml(const line: string);
begin
  RtfKeywords(line, XMLKEYWORDS);
end;

procedure TProjectWriter.RtfColLine(const line: string);
var P: PChar;
    withBorder, normalIndent: boolean;
    n, v, err: integer;
    Col: string;
    Cols: TIntegerDynArray;
    Colss: TStringDynArray;
begin
  if line='' then exit;
  if line[1]='%' then begin
     // |%=-40%30%30  = each column size -:no border =:no indent
     if fColsCount<>0 then // close any pending (and forgotten) table
       RtfColsEnd;
     P := @line[2];
     if P^=#0 then exit else // |% -> just close Table
     if P^='=' then begin //  = : no indent
       normalIndent := true;
       inc(P);
     end else
       normalIndent := false;
     if P^='-' then begin  // - : no border
       withBorder := false;
       inc(P);
     end else
       withBorder := true;
     SetLength(Cols,20);
     n := 0;
     repeat
       Col := GetNextItem(P,'%');
       if Col='' then break;
       val(Col,v,err);
       if err<>0 then continue;
       ProjectCommons.AddInteger(Cols,n,v);
     until false;
     SetLength(Cols,n);
     RtfColsPercent(Cols,true,withBorder,normalIndent,'\trkeep');
  end else begin // |text col 1|text col 2|text col 3
     SetLength(Colss,20);
     n := 0;
     P := pointer(line);
     repeat
       Col := GetNextItem(P,'|');
       ProjectCommons.AddString(Colss,n,Col);
     until P=nil;
     if n=0 then exit;
     SetLength(Colss,n);
     RtfRow(Colss);
  end;
end;


{ THTML }

function THTML.AddRtfContent(const s: string): TProjectWriter;
begin
  WriteAsHtml(pointer(s));
  result := self;
end;

constructor THTML.Create(const aLayout: TProjectLayout; aDefFontSizeInPts,
  aCodePage, aDefLang: integer; aLandscape, aCloseManualy,
  aTitleFlat: boolean; const aF0, aF1: string);
begin
  inherited;
  { TODO : multi-page layout }
end;

procedure THTML.InitClose;
begin
  inherited;
  { TODO : multi-page layout }
end;

function THTML.RtfBig(const text: string): TProjectWriter;
begin
  WR.Add(HTML_TAGS[False,hTitle]);
  AddRtfContent(text);
  WR.Add(HTML_TAGS[True,hTitle]).AddCRLF;
  result := self;
end;

function THTML.RtfBookMark(const Text, BookmarkName: string): string;
begin
  if BookmarkName='' then
    result := '' else begin
    result := RtfBookMarkName(BookmarkName);
    WR.AddShort('<a name="').Add(result).AddShort('">');
  end;
  AddRtfContent(Text);
end;

procedure THTML.RtfCols(const ColXPos: array of integer;
  FullWidth: integer; VertCentered, withBorder: boolean;
  const RowFormat: string);
begin
  fColsCount := length(ColXPos);
  WR.AddCRLF.Add(HTML_TAGS[False,hTable]);
  Last := lastRtfCols;
  { TODO: handle table cells format }
end;

function THTML.RtfColsEnd: TProjectWriter;
begin
  result := self;
  if fColsCount=0 then exit;
  WR.Add(HTML_TAGS[True,hTable]).AddCRLF;
  Last := LastRtfNone;
  fColsCount := 0;
end;

procedure THTML.RtfEndSection;
begin
  inherited;

end;

function THTML.RtfFont(SizePercent: integer): TProjectWriter;
begin

end;

function THTML.RtfFontString(SizePercent: integer): string;
begin

end;

procedure THTML.RtfFooterBegin(aFontSize: integer);
begin
  inherited;

end;

procedure THTML.RtfFooterEnd;
begin
  inherited;

end;

function THTML.RtfGoodSized(const Text: string): string;
begin

end;

procedure THTML.RtfHeaderBegin(aFontSize: integer);
begin
  inherited;

end;

procedure THTML.RtfHeaderEnd;
begin
  inherited;

end;

function THTML.RtfImage(const Image, Caption: string; WriteBinary: boolean;
  const RtfHead: string): TProjectWriter;
begin

end;

function THTML.RtfLine: TProjectWriter;
begin

end;

function THTML.RtfLinkTo(const aBookName, aText: string): TProjectWriter;
begin

end;

procedure THTML.RtfList(line: string);
begin
  inherited;

end;

procedure THTML.RtfPage;
begin
  inherited;

end;

function THTML.RtfPageRefTo(aBookName: string; withLink,
  BookMarkAlreadyComputed: boolean): TProjectWriter;
begin

end;

function THTML.RtfPar: TProjectWriter;
begin

end;

function THTML.RtfParDefault: TProjectWriter;
begin

end;

procedure THTML.RtfRow(const Text: array of string; lastRow: boolean);
var i: integer;
begin
  WR.Add(HTML_TAGS[False,hTR]);
  for i := 0 to high(Text) do begin
    WR.Add(HTML_TAGS[False,hTD]);
    AddRtfContent(Text[i]);
    WR.Add(HTML_TAGS[True,hTD]);
  end;
  WR.Add(HTML_TAGS[True,hTR]).AddCRLF;
  if lastRow then
    RtfColsEnd;
end;

procedure THTML.RtfSubTitle(const Title: string);
begin
  inherited;

end;

procedure THTML.RtfTitle(Title: string; LevelOffset: integer;
  withNumbers: boolean; Bookmark: string);
begin
  inherited;

end;

procedure THTML.SaveToFile(Format: TSaveFormat; OldWordOpen: boolean);
begin
  inherited;

end;

procedure THTML.SetInfo(const aTitle, aAuthor, aSubject, aManager,
  aCompany: string);
begin
  inherited;

end;

procedure THTML.SetLandscape(const Value: boolean);
begin
  inherited;

end;

procedure THTML.SetLast(const Value: TLastRTF);
begin
  inherited;
end;

procedure THTML.WriteAsHtml(P: PAnsiChar);
  procedure SetCurrent;
  var Old, New: THtmlTags;
      Tag: THtmlTag;
  begin
    New := Stack[Level];
    Old := Current;
    if New=Old then
      exit;
    for Tag := low(Tag) to high(Tag) do
      if (Tag in Old) and not (Tag in New) then
        WR.Add(HTML_TAGS[true,Tag]) else
      if not (Tag in Old) and (Tag in New) then
        WR.Add(HTML_TAGS[false,Tag]);
    Current := New;
  end;
var B: PAnsiChar;
    L: integer;
    token: AnsiString;
begin
  if P<>nil then
    while P^<>#0 do begin
      case P^ of
        '{': begin
          B := P;
          repeat inc(B) until B^ in [#0,'}'];
          if B^<>#0 then begin
            if Level<high(Stack) then begin
              inc(Level);
              Stack[Level] := Stack[Level-1];
            end;
          end;
        end;
        '}': if Level>0 then dec(Level);
        '<': begin
          SetCurrent;
          WR.Add(HTML_TAGS[false,hLT]);
        end;
        '>': begin
          SetCurrent;
          WR.Add(HTML_TAGS[false,hGT]);
        end;
        '\': begin
          B := P;
          repeat inc(B) until B^ in RTFEndToken;
          L := B-P-1;
          if L<=7 then begin
            if L>0 then begin
              SetString(Token,p+1,L);
              if token='b' then
                include(Stack[Level],hBold) else
              if token='b0' then
                exclude(Stack[Level],hBold) else
              if token='i' then
                include(Stack[Level],hItalic) else
              if token='i0' then
                exclude(Stack[Level],hItalic) else
              if token='ul' then
                include(Stack[Level],hUnderline) else
              if token='ul0' then
                exclude(Stack[Level],hUnderline) else
              if token='strike' then
                include(Stack[Level],hItalic) else
              if token='f1' then
                include(Stack[Level],hCode) else
              if token='line' then
                WR.Add(HTML_TAGS[false,hBR]) else
              if token='pard' then
                Stack[Level] := [] else
              if token='par' then begin // should not occur normaly
                SetCurrent;
                WR.Add(HTML_TAGS[false,hP]);
              end else begin
                WR.Add('???').Add(Token);
              end;
            end;
            inc(P,L+1);
            if P^<>'\' then inc(P); // handle \\ as \
            continue;
          end;
        end;
        else begin
          SetCurrent;
          WR.Add(P^);
        end;
      end;
      inc(P);
    end;
end;


initialization

finalization
{$ifdef DIRECTEXPORTTOWORD}
  if CoInitialized then
    CoUnInitialize;
{$endif}
end.


