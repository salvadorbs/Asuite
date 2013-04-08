/// binary Diff functions
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectDiff;

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

{$Q-,R-}  // Turn range checking and overflow checking off

uses
  Windows,
  SysUtils;

function DiffStreamCompress(const aNew, aOld: string;
  isEXE: boolean=false; BufSize: integer=2*1024*1024): string; overload;

function DiffStreamCompress(New, Old: PAnsiChar; NewSize, OldSize: integer; out Diff: PAnsiChar;
  isEXE: boolean=false; BufSize: integer=2*1024*1024): integer; overload;

function DiffStreamCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  isEXE: boolean=false; BufSize: integer=2*1024*1024): string; overload;

function DiffStreamExtract(const aDiff,aOld: string; out aOutUpd: string): string;
// return '' if OK, error message otherwize
// decompression don't use any RAM and is very fast


type
  TMemoryMap = object
  // use mapped files: very fast + avoid unnecessary disk access
  private
    _map: cardinal;
  public
    buf: PByteArray;
    _file,
    _size: integer;
    function DoMap(const aFileName: string): boolean; // buf -> mapped file
    function CopyToFile(const DestFileName: string): boolean;
    procedure UnMap;
    procedure ToHex(Dst: PAnsiChar);
    function GetMagicPatchOffset(MagicPatch: pointer; MagicLen: integer): PAnsiChar;
    function Adler32: cardinal;
  end;


function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;

function Adler32File(const aFileName: string): cardinal;
// fast Adler32-checksum calc of a file, using TMemoryMap - returns value

function FileToString(const FileName: string; out Text: AnsiString; PAdler: PCardinal=nil): boolean;
function StringToFile(const FileName: string; const Text: AnsiString): boolean;

function CopyFile(const source, dest: string): boolean;


implementation


function Max(a,b: integer): integer;
begin
  if a > b then
    result := a else
    result := b;
end;

function Min(a,b: integer): integer;
begin
  if a < b then
    result := a else
    result := b;
end;

{----------------------------------------------------------------}


function comp(a,b: pointer; len:integer):integer;
// eax = a, edx = b, ecx = len
asm // the 'rep cmpsb' version is slower on Intel Core CPU (not AMD)
     or ecx,ecx
     push ebx
     push ecx
     jz @ok
@1:  mov bx,[eax]
     lea eax,eax+2
     cmp bl,[edx]
     jne @ok
     dec ecx
     jz @ok
     cmp bh,[edx+1]
     lea edx,edx+2
     jne @ok
     dec ecx
     jnz @1
@ok: pop eax
     sub eax,ecx
     pop ebx
end;

function compReverse(var a,b; len:integer):integer; assembler;
asm
    or ecx,ecx
    push esi
    push edi
    mov esi,eax
    mov edi,edx
    mov eax,ecx
    jz @1
    std  // reverse order
    rep cmpsb
    je  @1 // if [esi]=[edi] till the end
    inc ecx
@1: sub eax,ecx
    cld
    pop edi
    pop esi
end;

procedure MoveWithOverlap(Src: pointer; Dst: pointer; Count: Integer);
// special tuned Move() routine, including data overlap bug correction
asm // eax=source edx=dest ecx=count
  push edx
  sub edx,eax
  cmp edx,ecx // avoid move error if dest and source overlaps
  pop edx     // restore original edx=dest
  ja System.Move // call FastMove() routine for normal code
  or ecx,ecx
  jz @@Exit
  push edi
  mov edi,edx // restore original edi=dest
@@overlap: // byte by byte slower but accurate move routine
  mov dl,[eax]
  inc eax
  mov [edi],dl
  inc edi
  dec ecx
  jnz @@overlap
  pop edi
@@Exit:
end;


{--------------------------------------------------------}

function WriteBufOfs(v: integer; SP: PAnsiChar): PAnsiChar;
begin
  if v<255 then begin
    SP^ := AnsiChar(v); // bufOfs = copied bytes length
    inc(SP);
  end else begin        // mark long sequence (>=255 length)
    SP^ := #255;        // bufOfs = copied bytes length
    inc(SP);
    dec(v,255);
    while v>=$ffff do begin // special encoding for length>=65535
      pWord(SP)^ := $ffff;  // bufOfs = copied bytes length = 65535
      inc(SP,2);
      dec(v,$ffff);
    end;
    pWord(SP)^ := v;    // bufOfs = copied bytes length = remaining
    inc(SP,2);
  end;
  result := SP;
end;

function WriteCurOfs(CurOfs,v: integer; SP: PAnsiChar): PAnsiChar;
begin
  if v<255 then begin   // short sequence (<255 length)
    SP^ := AnsiChar(v); // curlen = sequence length
    inc(SP);
  end else begin        // mark long sequence (>=255 length)
    SP^ := #255;        // curlen = sequence length
    inc(SP);
    dec(v,255);
    while v>=$ffff do begin // special encoding for length>=65535
      pWord(SP)^ := $ffff;  // curlen = sequence length = 65535
      inc(SP,2);
      dec(v,$ffff);
    end;
    pWord(SP)^ := v;        // curlen = sequence length = remaining
    inc(SP,2);
  end;
  pInteger(SP)^ := CurOfs;  // useofs = sequence position
  result := SP;
end;


const
  HTabBits = 18; // fits well with DiffStreamCompress(..,BufSize=2MB)
  HTabBitShr = 31-HTabBits;
  maxHTab = (1 shl HTabBits)-1; // =$3FFFF;
type
  PHTab = ^THTab;
  THTab = packed array[0..maxHTab] of integer; // HTabBits=18 -> 1MB of hash table

function DiffCompress(NewBuf, OldBuf, OutBuf, OutSpBuf: PAnsiChar;
   HList: PIntegerArray; NewBufSize, OldBufSize, MaxLevel: integer;
   HTab: PHTab): PAnsiChar;
type
  PInteger3 = ^TInteger3; // to optimize asm generated code below
  TInteger3 = packed record I0,I1,I2: integer; end;
var v, i, h, curofs, curlen, curlevel, templen, tempOfs, oldh,
    count: integer;
    SP, pInBuf, pOut: PAnsiChar;
    OldBufInc: integer;
begin
  // 1. fill HTab[] with hashes for all old data
  FillChar(HTab[0],Sizeof(HTab^),$FF); // hash=-1 by default
  pInBuf := OldBuf;
  oldh := maxHTab+1; // force calculate first hash
  for i := 0 to OldBufSize-3 do begin
    v := pInteger(pInBuf)^;
    inc(pInBuf);
    h := (v xor (v shr HTabBitShr)) and maxHTab;
    if h=oldh then continue;
    HList^[i] := HTab^[h];
    HTab^[h] := i;
    oldh := h;
  end;
  // 2. compression init
  if OldBufSize<=$ffff then
    OldBufInc := 2 else
    OldBufInc := 3;
  CurOfs := 0;
  pOut := OutBuf+7;
  SP := OutSpBuf;
  pInBuf := NewBuf;
  count := NewBufSize;
  // 3. main loop: identify longest sequences using hash, and store reference
  if count>=8 then
  repeat
    // hash 4 next bytes from NewBuf
    v := pInteger(NewBuf)^;
    TempOfs := HTab^[(v xor (v shr HTabBitShr)) and maxHTab];
    curlevel := MaxLevel;
    curlen := 0;
    if TempOfs<>-1 then
    // loop list of hash matchs
    repeat
      with PInteger3(OldBuf+TempOfs)^ do
      if (PInteger3(NewBuf)^.I0=I0) and (PInteger3(NewBuf)^.I1=I1) then begin // test 8 bytes
        TempLen := Comp(@PInteger3(NewBuf)^.I2,@I2,min(OldBufSize-TempOfs,Count)-8)+8;
        if TempLen>CurLen then begin // we find a longer sequence
          CurLen := TempLen;
          CurOfs := TempOfs;
        end;
      end;
      dec(CurLevel);
      TempOfs := HList^[TempOfs];
    until (CurLevel=0) or (TempOfs=-1);
    // Curlen = longest sequence length
    v := CurLen;
    if v<8 then begin // no long enough sequence found -> copy one byte
      dec(count);
      pOut^ := NewBuf^;
      inc(NewBuf);
      inc(pOut);
      if Count>8 then // >=8 may overflow
        continue else break;
    end;
    SP := WriteBufOfs(NewBuf-pInBuf,       // write bufofs = length of copied bytes
      WriteCurOfs(CurOfs,v,SP)+OldBufInc); // write curofs = sequence offset
    Inc(NewBuf,CurLen); // continue to search after the sequence
    dec(count,CurLen);
    pInBuf := NewBuf;
    if Count>8 then // >=8 may overflow
      continue else break;
  until false;
  // 4. write remaining bytes
  if count>0 then begin
    move(NewBuf^,pOut^,count);
    inc(pOut,count);
    inc(newBuf,count);
  end;
  SP := WriteBufOfs(NewBuf-pInBuf,WriteCurOfs(0,0,SP)+OldBufInc);
  // 5. write header
  pInteger(OutBuf)^ := pOut-OutBuf-7;
  h := SP-OutSpBuf;
  pInteger(OutBuf+3)^ := h;
  OutBuf[6] := AnsiChar(OldBufInc);
  // 6. copy commands
  move(OutSpBuf^,pOut^,h);
  result := pOut+h;
end;

function ExtractBuf(GoodCRC: cardinal; p: PAnsiChar; var aUpd, Diff: PAnsiChar; Old: PAnsiChar): string;
var obufsize, OSpBufSize:integer;
    pEnd: PAnsiChar;
    curlen,bufofs,useofs:integer;
    Buf: PAnsiChar;
    Upd: PAnsiChar;
    OldBuf16Bit: boolean;
begin
  // 1. decompression init
  Upd := aUpd;
  OBufSize := pCardinal(p)^ and $00ffffff; inc(p,3);
  OSpBufSize := pCardinal(p)^ and $00ffffff; inc(p,3);
  OldBuf16Bit := p^=#2; inc(p);
  Buf := p; inc(p,OBufSize);
  pEnd := p+OSpBufSize;
  // 2. main loop
  while p<pEnd do begin
    // curlen = sequence length
    curlen := ord(p^);
    inc(p);
    if curlen=255 then begin
      while pWord(p)^=$ffff do begin
        inc(curlen,$ffff);
        inc(p,2);
      end;
      inc(curlen,pWord(p)^);
      inc(p,2);
    end;
    // useofs = sequence position
    if OldBuf16Bit then begin
      useofs := pWord(p)^;
      inc(p,2);
    end else begin
      useofs := pCardinal(p)^ and $00ffffff;
      inc(p,3);
    end;
    // bufofs = copied bytes length
    bufofs := ord(p^);
    inc(p);
    if bufofs=255 then begin
      while pWord(p)^=$ffff do begin
        inc(bufofs,$ffff);
        inc(p,2);
      end;
      inc(bufofs,pWord(p)^);
      inc(p,2);
    end;
    // copy bytes
    if bufofs<>0 then begin
      move(Buf^,Upd^,bufofs);
      inc(Upd,bufofs);
      inc(Buf,bufofs);
    end;
    // copy sequence
    if curlen<>0 then begin
      MoveWithOverlap(Old+UseOfs,Upd,curlen); // a bit slower, but always working
//      Move(Old[UseOfs],Upd^,curlen);
      inc(Upd,curlen);
    end;
  end;
  // 3. result check
  Diff := p;
  if (p=pEnd) and
     (Adler32Asm(1,aUpd,Upd-aUpd)=GoodCRC) then // faster to make a whole CRC 
    result := '' else
    result := 'CRC Error 3 (extract)';
  aUpd := Upd;
end;

procedure WriteByte(var DiffP: PAnsiChar; A:Byte);
begin
  pByte(DiffP)^ := A;
  inc(DiffP);
end;

procedure WriteInt(var DiffP: PAnsiChar; A: Cardinal);
begin
  pCardinal(DiffP)^ := A;
  inc(DiffP,4);
end;

const
  FLAG_COPIED   = 0;
  FLAG_COMPRESS = 1;
  FLAG_BEGIN    = 2;
  FLAG_END      = 3;

function DiffStreamCompress(New, Old: PAnsiChar; NewSize, OldSize: integer; out Diff: PAnsiChar;
  isEXE: boolean=false; BufSize: integer=2*1024*1024): integer; overload;
var HTab: PHTab;
    DiffP: PAnsiChar;
    aSpBuf: PAnsiChar;
    BufRead, OldRead, maxLevel, LeftToCopy, NewSizeSave:integer;
    HList: PIntegerArray;
label CreateCopied;
begin
  // 1. special case
  NewSizeSave := NewSize;
  if OldSize=0 then begin // diff from nothing -> direct copy of whole block
CreateCopied:
    GetMem(Diff,NewSizeSave+17); // 17 = 4*Integer + 1*Byte
    DiffP := Diff;
    WriteInt(DiffP,NewSizeSave); // Destination Size = NewSize
    WriteInt(DiffP,0);       // OldRead=0
    WriteByte(DiffP,FLAG_COPIED); // block copied flag
    WriteInt(DiffP,NewSizeSave);
    WriteInt(DiffP,Adler32Asm(1, New, NewSizeSave));
    Move(New^,DiffP^,NewSizeSave); inc(DiffP,NewSizeSave);
    result := DiffP-Diff;
    assert(result=NewSizeSave+17);
    exit;
  end;

  // 2. compression init
  if BufSize>$ffffff then
    BufSize := $ffffff; // we store offsets with 3 bytes -> max 16MB
  GetMem(aSpBuf,BufSize); // compression temporary buffers
  GetMem(HList,BufSize*4);
  Getmem(HTab,SizeOf(HTab^));
  Getmem(Diff,Max(NewSize,OldSize)+4096); // Diff size max evalulation
  DiffP := Diff;

  // 3. first and last identical bytes
  WriteInt(DiffP,NewSize); // Destination Size = NewSize
  if Old<>nil then begin
    BufRead := Comp(New,Old,Min(NewSize,OldSize));   // test 1st same chars
    if BufRead>9 then begin // it happens very often
      WriteInt(DiffP,BufRead);  // blockSize = Size BufIdem
      WriteByte(DiffP,FLAG_BEGIN); {block idem begin flag}
      WriteInt(DiffP,Adler32Asm(1, New,Bufread));
      inc(New,BufRead);
      dec(NewSize,BufRead);
      inc(Old,BufRead);
      dec(oldSize,BufRead);
    end;                                             // test last same chars
    BufRead := CompReverse((New+NewSize-1)^,(Old+OldSize-1)^,Min(NewSize,OldSize));
    if BufRead>5 then begin
      if (NewSize=BufRead) then
        dec(BufRead); // avoid block overflow
      Dec(OldSize,BufRead);
      Dec(NewSize,BufRead);
      LeftToCopy := BufRead;
    end else LeftToCopy := 0;
  end else LeftToCopy := 0;

  // 4. main loop
  try
    repeat
      if BufSize>NewSize then
        BufRead := NewSize else
        BufRead := BufSize;
      Dec(NewSize,BufRead);
      if BufSize>OldSize then
        OldRead := OldSize else
        OldRead := BufSize;
      Dec(OldSize,OldRead);
      if (BufRead=0) and (LeftToCopy>0) then begin
        WriteInt(DiffP,LeftToCopy);  // blockSize = Size BufIdem
        WriteByte(DiffP,FLAG_END); // block idem end flag -> BufRead := 0 not necessary
        WriteInt(DiffP,Adler32Asm(1, New,LeftToCopy));
        break;
      end;
      WriteInt(DiffP,OldRead);
      If (BufRead<4) or (OldRead<4) or (BufRead div 4>OldRead) then begin
        WriteByte(DiffP,FLAG_COPIED); // block copied flag
        WriteInt(DiffP,BufRead);
        if bufread=0 then break;
        WriteInt(DiffP,Adler32Asm(1, New,Bufread));
        Move(New^,DiffP^,BufRead);
        inc(New,BufRead);
        inc(DiffP,BufRead);
      end else begin
        WriteByte(DiffP,FLAG_COMPRESS); // block compressed flag
        WriteInt(DiffP,Adler32Asm(1, New,Bufread));
        WriteInt(DiffP,Adler32Asm(1, Old,OldRead));
        if isExe then
          MaxLevel := 100 else // not faster with new hashing function
          MaxLevel := 500;
        DiffP := DiffCompress(New,Old,DiffP,aSpBuf,HList,BufRead,OldRead,MaxLevel,HTab);
        Inc(New,BufRead);
        Inc(Old,OldRead);
      end;
    until false;

  // 5. release temp memory 
  finally
    result := DiffP-Diff;
    Freemem(HTab);
    Freemem(HList);
    FreeMem(aSpBuf);
  end;
  if result>=NewSizeSave+17 then begin
    // diff didn't compress well -> store it (with 17 bytes overhead)
    FreeMem(Diff);
    goto CreateCopied;
  end;
end;

function DiffStreamCompress(const aNew, aOld: string;
  isEXE: boolean=false; BufSize: integer=2*1024*1024): string; overload;
begin
  if aOld=aNew then
    result := '=' else
    result := DiffStreamCompress(PAnsiChar(aNew),PAnsiChar(aOld),
      length(aNew),length(aOld),isExe,BufSize);
end;

function DiffStreamCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  isEXE: boolean=false; BufSize: integer=2*1024*1024): string; overload;
var Diff: PAnsiChar;
    DiffLen: integer;
begin
  DiffLen := DiffStreamCompress(New,Old,Newsize,OldSize,Diff,isExe,BufSize);
  SetString(result,Diff,DiffLen);
  Freemem(Diff);
end;

function DiffStreamExtract(const aDiff,aOld: string; out aOutUpd: string): string;
Var BufCRC: Cardinal;
   Code, Len, BufRead, OldRead:integer;
   Diff, Old, Upd: PAnsiChar;
begin
  Result := ''; // no error
  Diff := pointer(aDiff);
  if (Diff=nil) or (aDiff='=') then begin
    aOutUpd := aOld;
    exit;
  end;
  Old := pointer(aOld);
  Len := pInteger(Diff)^; inc(Diff,4);
  SetLength(aOutUpd,Len);
  Upd := PAnsiChar(aOutUpd);

  repeat
    OldRead := pInteger(Diff)^; inc(Diff,4);
    Code := ord(Diff^); inc(Diff);
    Case Code of
    FLAG_COPIED: begin // block copied flag - copy new from Diff
      BufRead := pInteger(Diff)^; inc(Diff,4);
      If BufRead=0 then break;
      If Adler32Asm(1,Diff+4,BufRead)<>pCardinal(Diff)^ then begin
        result := 'CRC Error 1 (copy source)';
        exit;
      end else inc(Diff,4);
      move(Diff^,Upd^,BufRead);
      if BufRead>=Len then exit; // if Old=nil -> only copy new
      inc(Diff,BufRead);
      inc(Upd,BufRead);
    end;
    FLAG_COMPRESS: begin // block compressed flag - extract Diff from Old
      BufCRC := pCardinal(Diff)^; inc(Diff,4);
      if Adler32Asm(1,Old,OldRead)<>pCardinal(Diff)^ then begin
        result := 'CRC Error 2 (extract source)';
        exit;
      end else inc(Diff,4);
      result := ExtractBuf(BufCRC,Diff,Upd,Diff,Old);
      if result<>'' then exit;
    end;
    FLAG_BEGIN: begin // block idem begin flag
      if Adler32Asm(1,Old,OldRead)<>pCardinal(Diff)^ then begin
        result := 'CRC Error 6 (begin source)';
        exit;
      end else inc(Diff,4);
      move(Old^,Upd^,OldRead);
      inc(Upd,OldRead);
    end;
    FLAG_END: begin // block idem end flag
      if Adler32Asm(1,Old,OldRead)<>pCardinal(Diff)^ then
        Result := 'CRC Error 7 (end source)';
      move(Old^,Upd^,OldRead);
      inc(Upd,OldRead);
      break;
    end;
    else begin
      result := 'CRC Error 4 (invalid flag source)';
      exit;
    end;
    end; // Case Code of
    inc(Old,OldRead);
  until false;
  if Upd-pointer(aOutUpd)<>Len then
    result := 'Dest Len Error 5';
end;



function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;
asm
	push      ebx
	push      esi
	push      edi
	mov       edi,eax
	shr       edi,16
	movzx     ebx,ax
	push      ebp
	mov       esi,edx
	test      esi,esi
	mov       ebp,ecx
	jne       @31
	mov       eax,1
	jmp       @32
@31:
	test      ebp,ebp
	jbe       @34
@33:
	cmp       ebp,5552
	jae        @35
	mov       eax,ebp
	jmp        @36
@35:
	mov       eax,5552
@36:
	sub       ebp,eax
	cmp       eax,16
	jl        @38
	xor       edx,edx
	xor       ecx,ecx
@39:
	sub       eax,16
	mov       dl,[esi]
	mov       cl,[esi+1]
	add       ebx,edx
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+2]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+3]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+4]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+5]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+6]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+7]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+8]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+9]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+10]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+11]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+12]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+13]
	add       edi,ebx
	add       ebx,ecx
	mov       dl,[esi+14]
	add       edi,ebx
	add       ebx,edx
	mov       cl,[esi+15]
	add       edi,ebx
	add       ebx,ecx
	cmp       eax,16
	lea       esi,[esi+16]
	lea       edi,[edi+ebx]
	jge       @39
@38:
	test      eax,eax
	je         @42
@43:
	xor       edx,edx
	mov       dl,[esi]
	add       ebx,edx
	dec       eax
	lea       esi,[esi+1]
  lea       edi,[edi+ebx]
	jg        @43
@42:
	mov       ecx,65521
	mov       eax,ebx
	xor       edx,edx
	div       ecx
	mov       ebx,edx
	mov       ecx,65521
	mov       eax,edi
	xor       edx,edx
	div       ecx
	test      ebp,ebp
	mov       edi,edx
	ja        @33
@34:
	mov       eax,edi
	shl       eax,16
	or        eax,ebx
@45:
@32:
	pop       ebp
	pop       edi
	pop       esi
	pop       ebx
end;



function Adler32File(const aFileName: string): cardinal;
var Map: TMemoryMap;
begin
  Map.DoMap(aFileName);
  result := Map.Adler32;
  Map.UnMap;
end;

function StringToFile(const FileName: string; const Text: AnsiString): boolean;
var f: integer;
begin
  result := false;
  f := FileCreate(FileName);
  if f>=0 then
  try
    if Text<>'' then
      FileWrite(f,Text[1],length(Text));
    result := true;
  finally
    FileClose(f);
  end;
end;

function FileToString(const FileName: string; out Text: AnsiString; PAdler: PCardinal=nil): boolean;
var Map: TMemoryMap;
begin
  if Map.DoMap(FileName) then begin
    SetString(Text,PAnsiChar(Map.buf),Map._size);
    if PAdler<>nil then
      PAdler^ := Adler32Asm(0,Map.buf,Map._size);
    Map.UnMap;
    result := true;
  end else begin
    Text := '';
    result := false;
  end;
end;


{ TMemoryMap }

function TMemoryMap.CopyToFile(const DestFileName: string): boolean;
var f: integer;
    FileTime: TFileTime;
begin
  result := false;
  if buf=nil then exit;
  f := FileCreate(DestFileName);
  if f<0 then exit;
  FileWrite(f,buf^,_size);
  SetEndOfFile(f);
  GetFileTime(_file,nil,nil,@FileTime);
  SetFileTime(f,nil,nil,@FileTime);
  FileClose(f);
  result := true;
end;

function TMemoryMap.DoMap(const aFileName: string): boolean;
begin
  _file := FileOpen(aFileName,fmOpenRead or fmShareDenyWrite);
  buf := nil;
  result := _file>=0;
  if not result then begin
    _size := 0;
    exit;
  end;
  _size := FileSeek(_file,0,2); // from end -> return File Size
  if _size=0 then begin
    FileClose(_file);
    _file := 0;
  end else begin
    FileSeek(_file,0,0); // from beginning
    _map := CreateFileMapping(_file, nil, PAGE_READONLY, 0, 0, nil);
    if _map<>0 then
      buf := MapViewOfFile(_map, FILE_MAP_READ, 0, 0, 0) else begin
      result := false;
      _size := 0;
    end;
  end;
end;

procedure TMemoryMap.UnMap;
begin
  if buf<>nil then begin
    UnmapViewOfFile(buf);
    CloseHandle(_map);
    buf := nil; // mark unmapped
  end;
  if _file<>0 then begin
    FileClose(_file);
    _file := 0;
  end;
end;

function TMemoryMap.GetMagicPatchOffset(MagicPatch: pointer; MagicLen: integer): PAnsiChar;
var i: integer;
begin
  result := pointer(buf);
  if result=nil then
    exit;
  for i := 0 to _size-MagicLen do
    if CompareMem(result,MagicPatch,MagicLen) then begin
      inc(result,Magiclen);
      exit;
    end else
      inc(result);
  result := nil;
end;

function TMemoryMap.Adler32: cardinal;
begin
  if buf=nil then
    result := 0 else
    result := Adler32Asm(0,buf,_size);
end;

function CopyFile(const source, dest: string): boolean;
// very fast File Copy using a memory mapped file
var Map: TMemoryMap;
begin
{$ifdef Win32}
  result := SameText(source,dest);
{$else}
  result := source=dest; // case-sensitive file names on linux
{$endif}
  if result then exit;  // same file? nothing to do!
  result := Map.DoMap(source);
  if result then
  try
    result := Map.CopyToFile(dest);
  finally
    Map.UnMap;
  end;
end;

const
  hexChars: array[0..15] of Char = '0123456789ABCDEF';

procedure TMemoryMap.ToHex(Dst: PAnsiChar);
var i,V: integer;
begin
  for i := 0 to _Size-1 do begin
    V := buf^[i];
    Dst^ := hexChars[V shr 4];   inc(Dst);
    Dst^ := hexChars[V and $F];  inc(Dst);
  end;
end;

end.
