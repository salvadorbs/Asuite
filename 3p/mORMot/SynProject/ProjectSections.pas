/// text ini-like file internal handling
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectSections;

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
  Contnrs,
  Classes,
  ProjectRTF;

type
  TSection = class
  private
    FCacheName, FCacheValue: string; // for GetValue()
    fLoadFileName: string;
    function GetValue(const aName: string): string;
    procedure SetValue(const aName, Value: string);
  public
    Lines: TStringList;
    SectionName: string; // 'SRS-4.2'
    SectionNameKind, // before the '*-' chars: 'SRS-4.2' -> 'SRS'
    SectionNameValue: string; // without the '*-' chars: 'SRS-4.2' -> '4.2'
    SectionNameValueWithDI: string; // SRS-DI-4.3 and DI-4.3 -> 'DI-4.3'
    Separator: string;
    HasBody, HasTitle: boolean;
    Level: integer;  // number of '.' in SectionName
    Owner: TSection; // usefull for sub-items (SRS-MENU01->DI-4.1, SRS-DI-4.8->DI-4.8)
    constructor Create(const SectionName: string); reintroduce;
    destructor Destroy; override;
    function ReadSection(P: PChar): PChar; // calls Lines.Clear
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string='');
    function GetValueIndex(const aName: string): integer;
    function AddCSVValue(const aName, aValue: string; noDuplicates: boolean=false;
      aValueIsCSV: boolean=false; Sep: AnsiChar=','): boolean; // aName=aValue1,aValue2..
    function CSVValueContains(const aName, aValue: string): boolean;
    procedure Clear;
    function ReadString(const aName, aDefault: string): string;
    function ReadInteger(const aName: string; aDefault: integer): integer;
    /// e.g. 'SWRS'
    function ItemName: string;
    function DocName: string;
    // [SRS] -> 'SWRS', [SRS-DI-4.7.1] -> 'SWRS # DI-4.7.1'
    function DisplayName(Doc: TSection): string;
    function Description: string;
    function ShortDescription(const layout: string): string;
    function PreparedBy: string;
    function RevisionDate: string;
    function Risk: string;
    function Hint: string; // for display
    function Root: TSection; // get Root item (Test-DI-4.3-03 -> DI-4.3)
    /// merge content from the supplied TSection
    procedure MergeFrom(aFrom: TSection);
    function SectionNameValueProtected: string; // '' if self=nil
    procedure SplitNameValue(LineIndex: integer; out Name, Value: string); overload;
    class procedure SplitNameValue(const Line: string; out Name, Value: string); overload;
    class procedure SplitSectionName(const SectionName: string;
      out SectionNameKind, // before the '*-' chars: 'SRS-4.2' -> 'SRS'
      SectionNameValue: string); // without the '*-' chars: 'SRS-4.2' -> '4.2'
    property Value[const aName: string]: string read GetValue write SetValue; default;
  end;

  TSectionList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSection;
    procedure SetItem(Index: Integer; const Value: TSection);
  public
    property Items[Index: Integer]: TSection read GetItem write SetItem; default;
  end;

  TSectionsStorage = class
  private
    ReaderSection: TSection;
    ReaderIndex: integer;
    ReaderValue: string;
    PushIndex: integer;
    ReaderSectionPushed: array[0..2] of TSection;
    ReaderIndexPushed: array[0..2] of integer;
    ReaderValuePushed: array[0..2] of string;
    //FCacheName: string;
    //FCacheValue: TSection;
    procedure ReaderNextLine(noIgnore: boolean = false);
    function GetSectionIndex(const aSection: string): integer;
    function GetSection(const aSection: string): TSection;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    Modified: boolean; // true if any call to WriteString() or [section] create
    Adler32AsCreated: cardinal;
    Sections: TSectionList;
    Header: TStringList;
    FileName: string;
    constructor Create(const aFileName: string = ''); reintroduce;
    procedure LoadFromMemory(P: PChar; PLen: integer = 0);
    destructor Destroy; override;
    function GetOrCreateSection(aSection: string; CreateIfNecessary: boolean): TSection;
    procedure WriteString(const aSection, aName, aValue: string);
    procedure WriteBody(const aSection, aBodyText: string);
    procedure WriteParams(const aSection, aBodyText: string);
    procedure WriteHeader(const aName, aValue: string);
    function ReadString(const aSection, aName, aDefault: string): string;
    function ReadHeader(const aName, aDefault: string): string; overload;
    function ReadHeader(const aName: string; aDefault: integer): integer; overload;
    procedure ReadStrings(const aSection: string; Dest: TStrings);
    function AddCSVValue(const aSection, aName, aValue: string; noDuplicates: boolean=false;
      aValueIsCSV: boolean=false; Sep: AnsiChar=','): boolean;
    procedure DeleteSection(const aSection: string);
    // ReadOpen('Section') + while not ReadEof do ReadLine/ReadNextNameValue
    procedure ReadOpen(const aSection: string; forNameValue: boolean);
    procedure ReadPush;
    procedure ReadPop;
    function ReadEof: boolean;
    function ReadLine(noIgnore: boolean = false): string; // next useful line (noIgnore -> with void or comment)
    function ReadLinePick: string; // get next useful line or '' if no more available
    function ReadNextNameValue(out Name, Value: string): boolean;
    /// merge header+[Sections] content from the supplied TSectionsStorage
    procedure MergeFrom(aFrom: TSectionsStorage);
    procedure SaveText(var W: TStringWriter);
    procedure SaveToFile(const aFileName: string = '');
    property Text: string read GetText write SetText;
    class function TrimBrackets(const s: string): string; // '[DI]' -> 'DI'
    property Section[const aSection: string]: TSection read GetSection; default;
  end;


function GetNextLine(d: pChar; out next: pChar): string; overload;
function GetNextLine(d: pChar): pChar; overload;
function GetNextLineBegin(d: pChar): pChar;
function GetNextLineRightTrim(d: pChar; out next: pChar): string;
function IgnoreLine(d: pChar): pChar;

function isTrue(const s: string): boolean;
// isTrue('yes')=isTrue('TRUE')=true


implementation

uses
  RTLConsts,
  ProjectDiff, // for TMemoryMap
  ProjectCommons;

function GetNextLineBegin(d: pChar): pChar;
begin
  if d^=#13 then inc(d);
  if d^=#10 then inc(d);
  result := d;
end;

function GetNextLine(d: pChar; out next: pChar): string;
begin
  next := d;
  d := GetNextLine(d);
  SetString(result,next,d-next);
  if d^=#13 then inc(d);
  if d^=#10 then inc(d);
  next := d;
end;

function IgnoreLine(d: pChar): pChar;
begin
  d := GetNextLine(d);
  if d^=#13 then inc(d);
  if d^=#10 then inc(d);
  result := d;
end;

function GetNextLineRightTrim(d: pChar; out next: pChar): string;
var e: PChar;
begin
  next := d;
  d := GetNextLine(d);
  e := d;
  while (e>next) and (e[-1]=' ') do
    dec(e); // trim right: no ' ' at the end of line
  SetString(result,next,e-next);
  next := GetNextLineBegin(d);
end;

function GetNextLine(d: pChar): pChar;
// stop on next #0,#10,#13
const EndChars: set of char = [#0,#10,#13];
begin
  while not (d^ in EndChars) do begin
    inc(d); if d^ in EndChars then break;
    inc(d); if d^ in EndChars then break;
    inc(d); if d^ in EndChars then break;
    inc(d); if d^ in EndChars then break;
    inc(d); if d^ in EndChars then break;
    inc(d); if d^ in EndChars then break;
  end;
  result := d;
end;

function isTrue(const s: string): boolean;
// isTrue('yes')=isTrue('TRUE')=true
begin
  result := (s='1') or IdemPChar(pointer(s),'YES') or IdemPChar(pointer(s),'TRUE');
end;

{ TSectionsStorage }

function TSectionsStorage.AddCSVValue(const aSection, aName, aValue: string;
  noDuplicates,aValueIsCSV: boolean; Sep: AnsiChar): boolean;
var Sub: TSection;
begin
  Sub := GetOrCreateSection(aSection,true);
  if Sub=nil then
    result := false else // unable to create section
    result := Sub.AddCSVValue(aName,aValue,noDuplicates,aValueIsCSV,Sep);
end;

constructor TSectionsStorage.Create(const aFileName: string);
var Map: TMemoryMap;
begin
  ReaderIndex := -1;
  Sections := TSectionList.Create; // FOwnsObjects := True
  FileName := aFileName;
  if aFileName<>'' then
    if Map.DoMap(aFileName) then begin
      LoadFromMemory(pointer(Map.buf),Map._size);
      Adler32AsCreated := Map.Adler32;
      Map.UnMap;
    end;
end;

procedure TSectionsStorage.DeleteSection(const aSection: string);
var i: integer;
begin
  i := GetSectionIndex(aSection);
  if i>=0 then
    Sections.Delete(i);
end;

destructor TSectionsStorage.Destroy;
begin
  Sections.Free; // auto-free Section[]
  Header.Free;
  inherited;
end;

function TSectionsStorage.GetOrCreateSection(aSection: string; CreateIfNecessary: boolean): TSection;
var i, L: integer;
begin
  result := nil;
  if self=nil then
    exit;
  L := length(aSection);
  while (L>0) and (aSection[L] in [']',' ']) do
    dec(L);
  if L=0 then
    exit else
    SetLength(aSection,L);
  i := GetSectionIndex(aSection);
  if i>=0 then begin
    result := Sections[i];
//    FCacheName := result.SectionName; FCacheValue := result;
    exit;
  end;
  if CreateIfNecessary then begin
    result := TSection.Create(aSection);
    Sections.Add(result);
//    FCacheName := result.SectionName; FCacheValue := result;
//  GetOrCreateSection() is called in LoadFromMemory
// -> Modified := true in WriteString/WriteBody only
  end else
    result := nil;
end;

function TSectionsStorage.GetSection(const aSection: string): TSection;
begin
  if self=nil then
    result := pointer(self) else
//  if aSection=FCacheName then result := FCacheValue else   // sometimes GPF
    result := GetOrCreateSection(aSection,false);
end;

function TSectionsStorage.GetSectionIndex(const aSection: string): integer;
begin
  for result := 0 to Sections.Count-1 do
    if SameText(Sections[result].SectionName,aSection) then
      exit;
  result := -1;
end;

function TSectionsStorage.GetText: string;
var W: TStringWriter;
begin
  SaveText(W);
  result := W.Data;
end;

procedure TSectionsStorage.LoadFromMemory(P: PChar; PLen: integer = 0);
var PEnd: PChar;
    line: string;
begin
  if P=nil then exit;
  // 1. initialize content
  Sections.Clear;
  FreeAndNil(Header);
  if PLen=0 then
    PLen := StrLen(P);
  PEnd := P+Plen;
  // 2. read header
  while (P<PEnd) and (P^<>'[') do begin
    line := GetNextLine(P,P);
    if line<>'' then begin
      if Header=nil then
        Header := TStringList.Create;
      Header.Add(line);
    end;
  end;
  // 3. read [Sections]
  if P<PEnd then
    repeat
      if P^='[' then
        P := GetOrCreateSection(GetNextLine(P+1,P), true).ReadSection(P) else
        P := IgnoreLine(P);
    until P>=PEnd;
end;

procedure TSectionsStorage.MergeFrom(aFrom: TSectionsStorage);
var i,j: integer;
begin
  if aFrom.Header<>nil then
    for i := 0 to aFrom.Header.Count-1 do begin
      j := Pos('=',aFrom.Header[i]);
      if j>0 then
        WriteHeader(copy(aFrom.Header[i],1,j-1),copy(aFrom.Header[i],j+1,maxInt));
    end;
  for i := 0 to Sections.Count-1 do
    with Sections[i] do
      MergeFrom(aFrom.Section[SectionName]);
end;

function TSectionsStorage.ReadEof: boolean;
begin
  if self=nil then
    result := true else
    result := (ReaderIndex<0);
end;

procedure TSectionsStorage.ReaderNextLine(noIgnore: boolean = false);
// inner go to next useful line (noIgnore -> with void or comment)
begin
  while not ReadEof do
    if ReaderIndex>=ReaderSection.Lines.Count then // eof?
      ReaderIndex := -1 else begin
      ReaderValue := ReaderSection.Lines[ReaderIndex];
      inc(ReaderIndex);
      if noIgnore or (ReaderValue='') or (ReaderValue[1]<>';') then exit;
    end;
end;

function TSectionsStorage.ReadHeader(const aName, aDefault: string): string;
var i: integer;
begin
  if (self=nil) or (Header=nil) then
    result := aDefault else begin
    i := Header.IndexOfName(aName);
    if i<0 then
      result := aDefault else
      result := Header.Values[aName];
  end;
end;

function TSectionsStorage.ReadHeader(const aName: string; aDefault: integer): integer;
var res: string;
begin
  res := ReadHeader(aName,IntToStr(aDefault));
  if not TryStrToInt(res,result) then
    result := aDefault;
end;

function TSectionsStorage.ReadLine(noIgnore: boolean = false): string;
// next useful line (noIgnore -> with void or comment)
begin
  if ReadEof then
    result := '' else begin
    result := ReaderValue;
    ReaderNextLine(noIgnore);
  end;
end;

function TSectionsStorage.ReadLinePick: string;
// pick next line, but no ReaderNextLine;
begin
  if ReadEof then
    result := '' else
    result := ReaderValue;
end;

function TSectionsStorage.ReadNextNameValue(out Name, Value: string): boolean;
var s: string;
    i: integer;
begin
  repeat
    result := not ReadEof;
    if not result then exit;
    s := ReadLine;
    if (s='') or (s[1]=':') then
      break; // void or title -> end of Name=Value pairs
    i := pos(ReaderSection.Separator,s);
    if i<=1 then
      break; // no more '=' -> end of Name=Value pairs
    Name := copy(s,1,i-1);
    Value := copy(s,i+1,maxInt);
    exit;
  until false;
  result := false;
  ReaderIndex := -1; // eof
end;

procedure TSectionsStorage.ReadOpen(const aSection: string; forNameValue: boolean);
begin
  if self=nil then
    exit;
  ReaderSection := Section[aSection];
  if ReaderSection<>nil then begin
    ReaderIndex := 0;
    if forNameValue then
      ReaderNextLine else
      repeat // forNameValue=false -> ignore all Name=Value pairs
        ReaderNextLine;
        if ReaderValue='' then break else
        if ReaderValue[1] in [':','='] then break else
        if ReaderValue[1]=';' then continue else
        if not (pos(ReaderSection.Separator,ReaderValue) in [2..48]) then break;
      until ReadEof;
  end else
    ReaderIndex := -1;
end;

procedure TSectionsStorage.ReadPop;
begin
  if PushIndex<=0 then
    exit;
  dec(PushIndex);
  if PushIndex<=high(ReaderSectionPushed) then begin
    ReaderSection := ReaderSectionPushed[PushIndex];
    ReaderIndex := ReaderIndexPushed[PushIndex];
    ReaderValue := ReaderValuePushed[PushIndex];
  end;
end;

procedure TSectionsStorage.ReadPush;
begin
  if PushIndex<=high(ReaderSectionPushed) then begin
    ReaderSectionPushed[PushIndex] := ReaderSection;
    ReaderIndexPushed[PushIndex] := ReaderIndex;
    ReaderValuePushed[PushIndex] := ReaderValue;
  end;
  inc(PushIndex);
end;

function TSectionsStorage.ReadString(const aSection, aName, aDefault: string): string;
begin
  result := Section[aSection].ReadString(aName,aDefault);
end;

procedure TSectionsStorage.ReadStrings(const aSection: string; Dest: TStrings);
begin
   Dest.BeginUpdate;
   Dest.Clear;
   ReadOpen(aSection,true);
   while not ReadEof do
     Dest.Add(ReadLine);
   Dest.EndUpdate;
end;

procedure TSectionsStorage.SaveText(var W: TStringWriter);
var i,j: integer;
begin
  W.Init;
  if self=nil then
    exit;
  if Header<>nil then begin
    for i := 0 to Header.Count-1 do
      W.Add(Header[i]).AddShort(#13#10);
    W.AddShort(#13#10);
  end;
  for i := 0 to Sections.Count-1 do
  with Sections[i] do begin
    W.Add('[').Add(SectionName).AddShort(']'#13#10);
    for j := 0 to Lines.Count-1 do
      W.Add(Lines[j]).AddShort(#13#10);
    W.AddShort(#13#10);
  end;
end;

procedure TSectionsStorage.SaveToFile(const aFileName: string = '');
var W: TStringWriter;
begin
  if self=nil then exit;
  SaveText(W);
  if aFileName='' then
    W.SaveToFile(FileName) else
    W.SaveToFile(aFileName);
end;

procedure TSectionsStorage.SetText(const Value: string);
begin
  if self=nil then exit;
  LoadFromMemory(pointer(Value),length(Value));
  Modified := true;
end;

class function TSectionsStorage.TrimBrackets(const s: string): string;
// '[DI]' -> 'DI'
var b,e,L: integer;
begin
  L := length(s);
  for b := 1 to L do
    if s[b]='[' then begin
      for e := L downto b+1 do
        if not (s[e] in [' ',']']) then begin
          result := copy(s,b+1,e-b);
          exit;
        end;
      break;
    end;
  result := s;
end;

procedure TSectionsStorage.WriteBody(const aSection, aBodyText: string);
var S: TSection;
    P: PChar;
begin
  S := GetOrCreateSection(aSection, true);
  if S=nil then exit; // invalid self or aSection name
  if S.HasBody then begin
    ReadOpen(aSection,false);
    if not ReadEof then
      while ReaderIndex<ReaderSection.Lines.Count do // delete existing body
        ReaderSection.Lines.Delete(ReaderIndex);
  end;
  S.Lines.Add('');
  P := pointer(aBodyText);
  if P<>nil then
  while P^<>#0 do
    S.Lines.Add(GetNextLine(P,P));
  Modified := true;
end;

procedure TSectionsStorage.WriteHeader(const aName, aValue: string);
var i: integer;
begin
  if Header=nil then
    Header := TStringList.Create;
  i := Header.IndexOfName(aName);
  if i<0 then
    Header.Add(aName+'='+aValue) else
    Header[i] := aName+'='+aValue;
end;

procedure TSectionsStorage.WriteParams(const aSection, aBodyText: string);
var S: TSection;
    i: integer;
    P: PChar;
begin
  S := GetOrCreateSection(aSection, true);
  if S=nil then exit; // invalid self or aSection name
  if S.HasBody then begin // delete all lines till body
    ReadOpen(aSection,false);
    if not ReadEof then
      for i := ReaderIndex-1 downto 0 do
        ReaderSection.Lines.Delete(i);
  end else
    S.Lines.Clear; // no body -> only params -> clear old params values
  P := pointer(aBodyText);
  i := 0;
  if P<>nil then
  while P^<>#0 do begin
    S.Lines.Insert(i,GetNextLine(P,P));
    inc(i);
  end;
end;

procedure TSectionsStorage.WriteString(const aSection, aName, aValue: string);
begin
  GetOrCreateSection(aSection, true)[aName] := aValue;
  Modified := true;
end;



{ TSection }

class procedure TSection.SplitSectionName(const SectionName: string;
  out SectionNameKind, // before the '*-' chars: 'SRS-4.2' -> 'SRS'
  SectionNameValue: string); // without the '*-' chars: 'SRS-4.2' -> '4.2'
var i: integer;
begin
  i := pos('-',SectionName);
  if i>0 then begin // without the '*-' chars -> 'DI-4.2' -> '4.2'
    SectionNameKind := copy(SectionName,1,i-1);
    SectionNameValue := copy(SectionName,i+1,20);
  end else begin
    SectionNameKind := SectionName;
    SectionNameValue := SectionName;
  end;
end;

constructor TSection.Create(const SectionName: string);
begin
  Lines := TStringList.Create;
  Separator := '=';
  self.SectionName := SectionName;
  SplitSectionName(SectionName,SectionNameKind,SectionNameValue);
  if SectionNameValue[1] in ['0'..'9'] then
    SectionNameValueWithDI := SectionName else  // DI-4.1 -> 'DI-4.1'
    SectionNameValueWithDI := SectionNameValue; // SRS-DI-4.1 -> 'DI-4.1'
end;

destructor TSection.Destroy;
begin
  Lines.Free;
end;

function TSection.GetValue(const aName: string): string;
var i: integer;
begin
  if Self=nil then
    result := '' else
  if aName=FCacheName then
    result := FCacheValue else begin
    i := GetValueIndex(aName);
    if i<0 then
      result := '' else
      result := copy(Lines[i],length(aName)+length(Separator)+1,maxInt);
    FCacheName := aName;
    FCacheValue := result;
  end;
end;

function TSection.ReadInteger(const aName: string; aDefault: integer): integer;
begin
  if not TryStrToInt(Value[aName],result) then
    result := aDefault;
end;

function TSection.ReadString(const aName, aDefault: string): string;
var i: integer;
begin
  if Self=nil then
    result := aDefault else
  if aName=FCacheName then
    result := FCacheValue else begin
    i := GetValueIndex(aName);
    if i<0 then
      result := aDefault else
      result := copy(Lines[i],length(aName)+length(Separator)+1,maxInt);
    FCacheName := aName;
    FCacheValue := result;
  end;
end;

procedure TSection.SetValue(const aName, Value: string);
var i: integer;
begin
  if Self=nil then exit;
  i := GetValueIndex(aName);
  if i<0 then
    Lines.Insert(0,aName+Separator+Value) else
    Lines[i] := aName+Separator+Value;
  FCacheName := aName;
  FCacheValue := Value;
end;

function TSection.GetValueIndex(const aName: string): integer;
// faster than TStrings.IndexOfName() implementation
var Search, Line: string;
begin
  if Self<>nil then begin
    Search := ProjectCommons.UpperCase(aName+Separator); // 'NAME='
    for result := 0 to Lines.Count-1 do begin
      Line := Lines.Strings[result];
      if Line='' then // no more Name=Value pairs after void line
        break else
      if IdemPChar(pointer(Line),pointer(Search)) then
        exit;
    end;
  end;
  result := -1;
end;

function TSection.ReadSection(P: PChar): PChar;
var s: string;
begin
  if self=nil then begin // go to next section if Lines[] not assigned
    if P<>nil then
      while not (P^ in [#0,'[']) do
        P := IgnoreLine(P);
    Result := P;
    exit;
  end;
  Lines.Clear;
  if P<>nil then begin
    // read lines
    while not (P^ in [#0,'[']) do begin
      s := GetNextLineRightTrim(P,P); // without ending ' '
      if not hasBody and (s<>'') and (s[1]<>';') then
        if s[1] in [':','='] then begin
          HasTitle := true;
          hasBody := true;
        end else
          case pos(Separator,s) of
            0,32..maxInt: hasBody := true;
          end;
      Lines.Add(s);
    end;
    // delete last void lines
    while (Lines.Count>0) and (Lines[Lines.Count-1]='') do
      Lines.Delete(Lines.Count-1);
  end;
  result := P;
end;

function TSection.ItemName: string;
begin
  result := ReadString('ItemName',SectionNameKind);
end;

function TSection.DocName: string;
begin
  result := ReadString('DocName',ItemName);
end;

function TSection.DisplayName(Doc: TSection): string;
// [SRS] -> 'SWRS', [SRS-DI-4.7.1] -> 'SWRS # DI-4.7.1'
begin
  result := Value['DisplayName'];
  if result='' then
    if Doc<>nil then
      result := Doc.ItemName+' # '+ SectionNameValue else
    if SectionNameValue<>SectionName then
      result := ItemName+' # '+ SectionNameValue else
      result := SectionName;
end;

function TSection.Description: string;
begin
  if self=nil then
    result := '' else begin
    result := ReadString('Description',Value['Ident']);
    if result='' then
      if Owner<>self then
        result := Owner.Description;
  end;
end;

function TSection.ShortDescription(const layout: string): string;
begin
  if self=nil then
    result := '' else begin
    result := Value['ShortName'];
    if result='' then
      result := Description else
      if (layout<>'') and (Owner<>nil) and (pos(':',result)=0) then
        result := layout+': '+result;
  end;
end;

function TSection.RevisionDate: string;
begin
  result := Value['RevisionDate'];
  if result='' then
    result := FormatDateTime('mmmm d, yyyy',now);
end;

function TSection.CSVValueContains(const aName, aValue: string): boolean;
begin
  result := CSVContains(Value[aName],aValue);
end;

function TSection.AddCSVValue(const aName, aValue: string; noDuplicates,aValueIsCSV: boolean;
  Sep: AnsiChar): boolean;
// aName=aValue1,aValue2.. true if added, false if already there
var V: string;
    P: PAnsiChar;
begin
  result := false;
  if (self=nil) or (aValue='') then exit;
  if aValueIsCSV then begin
    P := pointer(aValue);
    while P<>nil do
      if AddCSVValue(aName,GetNextItemTrimed(P),noDuplicates,false,Sep) then
        result := true;
    exit;
  end;
  V := Value[aName];
  if V='' then
    V := aValue else begin
    if noDuplicates and CSVContains(V,aValue,Sep) then
      exit; // already there -> exit
    V := V+','+aValue;
  end;
  Value[aName] := V;
  result := true;
end;

procedure TSection.Clear;
begin
  Lines.Clear;
  FCacheName := '';
  FCacheValue := '';
  HasBody := false;
  HasTitle := false;
end;

function TSection.Hint: string;
begin
  result := Value['Name'];
  if result<>'' then exit;
  result := Description;
end;

function TSection.Root: TSection;
// get Root item (Test-DI-4.3-03 -> DI-4.3)
begin
  if (self=nil) or (Owner=nil) or (Owner=self) then
    result := self else   // we are at the root level
    result := Owner.Root; // go up one level
end;

function TSection.Risk: string;
begin
  if self=nil then
    result := '' else begin
    result := Value['Risk'];
    if result='' then
      if Owner<>self then
        result := Owner.Risk;
  end;
end;

function TSection.SectionNameValueProtected: string;
begin
  if self=nil then
    result := '' else
    result := SectionNameValue;
end;

function TSection.PreparedBy: string;
begin
  if self=nil then
    result := '' else begin
    result := Value['PreparedBy'];
    if result<>'' then
      result := ValAt(result,0) else // return first name only
      if Owner<>self then
        result := Owner.PreparedBy;
  end;
end;

class procedure TSection.SplitNameValue(const Line: string; out Name, Value: string);
var i: integer;
begin
  i := pos('=',Line); // default Separator string is '='
  if i<=1 then begin
    Name := '';
    Value := '';
  end else begin
    Name := copy(Line,1,i-1);
    Value := copy(line,i+1,maxInt);
  end;
end;

procedure TSection.SplitNameValue(LineIndex: integer; out Name, Value: string);
begin
  SplitNameValue(Lines[LineIndex],Name,Value);
end;

procedure TSection.LoadFromFile(const FileName: string);
begin
  fLoadFileName := FileName;
  if FileExists(FileName) then
    Lines.LoadFromFile(FileName);
end;

procedure TSection.SaveToFile(const FileName: string);
begin
  if FileName='' then
    Lines.SaveToFile(fLoadFileName) else
    Lines.SaveToFile(FileName);
end;

procedure TSection.MergeFrom(aFrom: TSection);
var i: integer;
    aName, aValue: string;
begin
  if (self=nil) or (aFrom=nil) then
    exit;
  for i := 0 to aFrom.Lines.Count-1 do begin
    SplitNameValue(aFrom.Lines[i],aName,aValue);
    if aName<>'' then
      Value[aName] := aValue;
  end;
end;

{ TSectionList }

function TSectionList.GetItem(Index: Integer): TSection;
begin
  if cardinal(Index)>=cardinal(Count) then
    Error(@SListIndexError, Index);
  result := TSection(List^[Index]);
end;

procedure TSectionList.SetItem(Index: Integer; const Value: TSection);
begin
  Items[Index] := Value;
end;


end.
