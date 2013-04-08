/// File Versioning textual Diff core
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectDiffUnit;

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

(*
  Original Copyright Notice:
   
  Component         TDiff
  Version:          3.0
  Date:             November 2008
  Compilers:        Delphi 3 - Delphi 2009
  Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com
                    updated by Arnaud Bouchez http://synopse.info
  Copyright:        © 2001-2004 Angus Johnson
                    © 2008 Arnaud Bouchez

  Licence to use, terms and conditions:
                    The code in the TDiff component is released as freeware
                    provided you agree to the following terms & conditions:
                    1. the copyright notice, terms and conditions are
                    left unchanged
                    2. modifications to the code by other authors must be
                    clearly documented and accompanied by the modifier's name.
                    3. the TDiff component may be freely compiled into binary
                    format and no acknowledgement is required. However, a
                    discrete acknowledgement would be appreciated (eg. in a
                    program's 'About Box').

  Description:      Component to list differences between two integer/word/char
                    arrays using a "longest common subsequence" algorithm.
                    Typically, this component is used to diff 2 text files
                    once their individuals lines have been hashed.

  A.Bouchez notes:  This algorithm is less efficient for storing diff than
                    SynDiff (don't find any moved block e.g.), but is easier to
                    interpret from an human being point of view.
                    => use SynDiff for difference storage on disk
                    => after data reconstruction, use TDiff with hashed lines
                       for difference display on the UI

  Acknowledgements: The key algorithm in this component is based on:
                    "An O(ND) Difference Algorithm and its Variations"
                    By E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266
                    http://www.cs.arizona.edu/people/gene/
                    http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps


  History:
  13 December 2001 - Original Release
  24 February 2002 - OnProgress event added, improvements to code comments
  15 April 2003    - Bug fix in diff algorithm
  20 January 2004  - Minor bug fix, code and documentation improvements
  24 January 2004  - Fixed minor bug introduced in 20-Jan changes
                     Further code and documentation improvements
  November 2008    - Char+Word+Integer diffs by Arnaud Bouchez
                     Example of use: Char for AnsiString, Word for UnicodeString,
                     Integer for hashed lines (use CRC32 for best hashing)
                   - direct text diff function for human reading
                   - diff output format
                   - The code was rewritten deeply, for speed and memory usage:
                     there is not left much code from Angus here, only ideas
                   - modifications released under GPL 3.0 license 
*)


{.$DEFINE DIFF_CHARS}


interface

uses
  ProjectCommons, // for TIntegerDynArray
  SynZip,   // for crc32
  ProjectRTF,   // for TStringWriter
  SysUtils,
  Classes;

const
  //Maximum realistic deviation from centre diagonal vector ...
  MAX_DIAGONAL = $FFFFFF; // means 128KB*2 = 256KB max memory in diagVecB/F

type
  PDiagVectorArray = ^TDiagVectorArray;
  TDiagVectorArray = array [-MAX_DIAGONAL .. +MAX_DIAGONAL] of integer;

  PByteArray = ^TByteArray;
  TByteArray = packed array[0..0] of byte;
  PWordArray = ^TWordArray;
  TWordArray = packed array[0..0] of word;

  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PChangeRec =^TChangeRec;
  TChangeRec = record
    Kind     : TChangeKind; //(ckAdd, ckDelete, ckModify)
    x        : integer;     //Array1 offset (where to add, delete, modify)
    y        : integer;     //Array2 offset (what to add, modify)
    Range    : integer;     //range :-)
  end;

  TDiff = class(TComponent)
  private
    Array1, Array2: pointer;
    diagVecB,
    diagVecF: PDiagVectorArray; //forward and backward arrays
    ElemSize: integer; // in bytes: allowed values are 1, 2 or 4
    ArraySize1, ArraySize2: integer;
    MaxD: integer;
    fChangeCount: integer;
    fCancelled: boolean;

    function RecursiveDiff(bottom1, bottom2, top1, top2: integer): boolean;
  protected
    fLastAdd, fLastDel, fLastMod: TChangeRec;
    procedure NewAdd(bottom1,bottom2: integer);
    procedure NewMod(bottom1,bottom2: integer);
    procedure NewDel(bottom1: integer);
    procedure Add(bottom1,bottom2: integer);
    function  AddChange: PChangeRec;
    procedure Delete(bottom1: integer);
    procedure PushAdd;
    procedure PushDel;
    procedure PushMod;
  public
    Changes: array of TChangeRec;
    function Execute(aArray1, aArray2: pointer; aElemSize, aArrSize1, aArrSize2: integer): boolean;
    procedure Cancel; //allow cancelling prolonged comparisons from another thread
    property ChangeCount: integer read FChangeCount;
  end;


function CalculateDiff(const Text1, Text2: string): TDiff;
// hash lines and calculate diffs of Text1 and Text2

function TextToHash(P: PChar; var Hash: TIntegerDynArray): integer;
// crc32 of all lines in P^ into Hash[] - return Hash[] count
// crc32 is more precise than adler32 for small text lines
// no temporary string are created -> crc32 is computed on the fly in the PChar

function DiffText(const Old, New: string; withLineNumber: boolean): string; overload;
procedure DiffText(const Old, New: string; var WR: TStringWriter; withLineNumber: boolean); overload;
// return a human readable diff from versions



implementation

// Miscellaneous Functions

function min(a,b: integer): integer;
begin
  if a < b then
    result := a else
    result := b;
end;

function max(a,b: integer): integer;
begin
  if a > b then
    result := a else
    result := b;
end;


// TDiff Class

procedure TDiff.Cancel;
begin
  fCancelled := true;
end;

function TDiff.Execute(aArray1, aArray2: pointer; aElemSize, aArrSize1, aArrSize2: integer): boolean;
var IntArr_f, IntArr_b: PAnsiChar;
    bottom1, bottom2, top1, top2: integer;
begin
  result := false;
  fChangeCount := 0; // reset ChangeList
  fLastAdd.Kind := ckNone;
  fLastDel.Kind := ckNone;
  fLastMod.Kind := ckNone;

  if not assigned(aArray1) or not assigned(aArray2) or
     not (aElemSize in [1,2,4]) then exit;
  ArraySize1 := aArrSize1;
  ArraySize2 := aArrSize2;
  ElemSize := aElemSize;
  Array1 := aArray1;
  Array2 := aArray2;

  //MaxD == Maximum possible deviation from centre diagonal vector
  //which can't be more than the largest DiffArray (with upperlimit = MAX_DIAGONAL)
  MaxD := min(max(ArraySize1,ArraySize2), MAX_DIAGONAL);

  //allocate the vector memory (total 256KB max)
  GetMem(IntArr_f, sizeof(integer)*(MaxD*2+1));
  GetMem(IntArr_b, sizeof(integer)*(MaxD*2+1));
  try
    //Align the forward and backward diagonal vector arrays
    //with the memory which has just been allocated ...
    pointer(diagVecF) := IntArr_f - sizeof(integer)*(MAX_DIAGONAL-MaxD);
    pointer(diagVecB) := IntArr_b - sizeof(integer)*(MAX_DIAGONAL-MaxD);

    fCancelled := false;

    bottom1 := 1;
    bottom2 := 1;
    top1 := ArraySize1;
    top2 := ArraySize2;

    //ignore leading and trailing matches, we're only interested in diffs...
    //20-Jan-04: this code block was moved from RecursiveDiff()
    case ElemSize of // 3 different loops for higher speed
    1: begin
     while (bottom1 <= top1) and (bottom2 <= top2) and
      (PByteArray(Array1)^[bottom1-1]=PByteArray(Array2)^[bottom2-1]) do begin
      inc(bottom1);
      inc(bottom2);
    end;
    while (top1 > bottom1) and (top2 > bottom2) and
      (PByteArray(Array1)^[top1-1]=PByteArray(Array2)^[top2-1]) do begin
      dec(top1);
      dec(top2);
    end;
    end;
    2: begin
     while (bottom1 <= top1) and (bottom2 <= top2) and
      (PWordArray(Array1)^[bottom1-1]=PWordArray(Array2)^[bottom2-1]) do begin
      inc(bottom1);
      inc(bottom2);
    end;
    while (top1 > bottom1) and (top2 > bottom2) and
      (PWordArray(Array1)^[top1-1]=PWordArray(Array2)^[top2-1]) do begin
      dec(top1);
      dec(top2);
    end;
    end;
    4: begin
     while (bottom1 <= top1) and (bottom2 <= top2) and
      (PIntegerArray(Array1)^[bottom1-1]=PIntegerArray(Array2)^[bottom2-1]) do begin
      inc(bottom1);
      inc(bottom2);
    end;
    while (top1 > bottom1) and (top2 > bottom2) and
      (PIntegerArray(Array1)^[top1-1]=PIntegerArray(Array2)^[top2-1]) do begin
      dec(top1);
      dec(top2);
    end;
    end;
    end;

    //NOW DO IT HERE...
    if (bottom1 > top1) and (bottom2 > top2) then //24-Jan-04
      result := true else begin //ie identical arrays
      result := RecursiveDiff(bottom1-1, bottom2-1, top1, top2);
    end;

    //add remaining range buffers onto ChangeList...
    PushAdd;
    PushDel;
    if not result then
      fChangeCount := 0;
  finally
    FreeMem(IntArr_f);
    FreeMem(IntArr_b);
  end;
end;

function TDiff.AddChange: PChangeRec;
begin
  if fChangeCount=length(Changes) then
    SetLength(Changes,fChangeCount+length(Changes)shr 3+256);
  result := @Changes[fChangeCount];
  inc(fChangeCount);
end;

procedure TDiff.PushAdd;
begin
  PushMod;
  if fLastAdd.Kind<>ckAdd then exit; // no ADD pending to be written
  AddChange^ := fLastAdd;
  fLastAdd.Kind := ckNone;
end;

procedure TDiff.PushDel;
begin
  PushMod;
  if fLastDel.Kind<>ckDelete then exit; // no DEL pending to be written
  AddChange^ := fLastDel;
  fLastDel.Kind := ckNone;
end;

procedure TDiff.PushMod;
begin
  if fLastMod.Kind<>ckModify then exit; // no MOD pending to be written
  AddChange^ := fLastMod;
  fLastMod.Kind := ckNone;
end;

// 1. there can NEVER be concurrent fLastAdd and fLastDel record ranges
// 2. fLastMod is always pushed onto ChangeList before fLastAdd & fLastDel

procedure TDiff.Add(bottom1, bottom2: integer);
begin
  if fLastAdd.Kind=ckAdd then begin            //OTHER ADDS PENDING
    if (fLastAdd.x=bottom1) and
      (fLastAdd.y+fLastAdd.Range=bottom2) then
      inc(fLastAdd.Range) else begin //add in series
      PushAdd;
      NewAdd(bottom1,bottom2);
    end;   //add NOT in series
  end
  else if fLastDel.Kind=ckDelete then begin       //NO ADDS BUT DELETES PENDING
    if bottom1 = fLastDel.x then begin //add matches pending del so modify ...
      if (fLastMod.Kind=ckModify) and (fLastMod.x+fLastMod.Range-1 = bottom1) and
         (fLastMod.y+fLastMod.Range-1=bottom2) then
        inc(fLastMod.Range) else //modify in series
        NewMod(bottom1,bottom2);
      //start NEW modify
      if fLastDel.Range=1 then
        fLastDel.Kind := ckNone else begin    //decrement or remove existing del
        dec(fLastDel.Range);
        inc(fLastDel.x);
      end;
    end
    else begin  //add does NOT match pending del's
      PushDel;
      NewAdd(bottom1,bottom2);
    end;
  end
  else
    NewAdd(bottom1,bottom2);   //NO ADDS OR DELETES PENDING
end;

procedure TDiff.Delete(bottom1: integer);
begin
  if fLastDel.Kind=ckDelete then begin            //OTHER DELS PENDING
    if (fLastDel.x+fLastDel.Range=bottom1) then
      inc(fLastDel.Range) else begin          //del in series
      PushDel;
      NewDel(bottom1);
    end;      //del NOT in series
  end
  else if fLastAdd.Kind=ckAdd then begin       //NO DELS BUT ADDS PENDING
    if bottom1=fLastAdd.x then begin        //del matches pending add so modify ...
      if (fLastMod.Kind=ckModify) and (fLastMod.x+fLastMod.Range=bottom1) then
        inc(fLastMod.Range) else //mod in series
        NewMod(bottom1,fLastAdd.y);
      //start NEW modify ...
      if fLastAdd.Range=1 then
        fLastAdd.Kind := ckNone else begin //decrement or remove existing add
        dec(fLastAdd.Range);
        inc(fLastAdd.x);
        inc(fLastAdd.y);
      end;
    end
    else begin //del does NOT match pending add's
      PushAdd;
      NewDel(bottom1);
    end;
  end
  else
    NewDel(bottom1);    //NO ADDS OR DELETES PENDING
end;

procedure TDiff.NewAdd(bottom1, bottom2: integer);
begin
  fLastAdd.Kind := ckAdd;
  fLastAdd.x := bottom1;
  fLastAdd.y := bottom2;
  fLastAdd.Range := 1;
end;

procedure TDiff.NewDel(bottom1: integer);
begin
  fLastDel.Kind := ckDelete;
  fLastDel.x := bottom1;
  fLastDel.y := 0;
  fLastDel.Range := 1;
end;

procedure TDiff.NewMod(bottom1, bottom2: integer);
begin
  PushMod;
  fLastMod.Kind := ckModify;
  fLastMod.x := bottom1;
  fLastMod.y := bottom2;
  fLastMod.Range := 1;
end;


{
  ADDITIONAL NOTES:

  (1)  formula: curr2 := curr1 - bottom1 + bottom2 -k;
                (k = index of current diagonal vector)

       a r r 1
     \ • • o •
   a • \ • • •  given bottom1 = 0 and bottom2 = 0 and
   r • • \ • •  given that k = 0 and curr1 = 3, then
   r o • • + •  curr2 := 3 - 0 + 0 - 0; (ie 3)
   2 • • • • \

       a r r 1
     • • o • •
   a • • • • •  given bottom1 = 0 and bottom2 = 0 and
   r \ • • • •  given that k = -2 and curr1 = 2, then
   r • \ • • •
   2 o • + • •  curr2 := 2 - 0 + 0 - -2; (ie 4)
     • • • \ •
}

function TDiff.RecursiveDiff(bottom1, bottom2, top1, top2: integer): boolean;
var
  //Recursive functions should generally use the heap (rather than the stack)
  //for local vars. However, as the maximum depth of recursion here is
  //relatively small (<25), the risk of stack overflow is negligible.
  i, curr1, curr2, Delta, D, k: integer;
begin
    result := true;

    //check if one has no line -> just all additions or all deletions
    if (top1 = bottom1) then begin
      for i := bottom2 to top2-1 do
        Add(bottom1,i);
      exit;
    end else if (top2 = bottom2) then begin
      for i := bottom1 to top1-1 do
        Delete(i);
      exit;
    end;

    //Delta = offset of bottomright from topleft corner
    Delta := (top1 - bottom1) - (top2 - bottom2);
    //initialize the forward and backward diagonal vectors including the
    //outer bounds ...
    diagVecF[0] := bottom1;
    diagVecB[Delta] := top1;
    //24-Jan-04 ...
    diagVecF[top1 - bottom1] := top1;
    diagVecB[top1 - bottom1] := top1;
    //the following avoids the -(top2 - bottom2) vectors being assigned
    //invalid values. Also, the algorithm is a little faster than when
    //initialising the -(top2 - bottom2) vectors instead...
    if Delta > 0 then begin
      diagVecF[-(top2 - bottom2 +1)] := bottom1;
      diagVecB[-(top2 - bottom2 +1)] := bottom1;
    end;

    //nb: the 2 arrays of diagonal vectors diagVecF and diagVecB store the
    //current furthest forward and backward Array1 coords respectively
    //(something between bottom1 and top1). The Array2 coords can be derived
    //from these (see added comments (1) at the bottom of this unit).

    //When the forward and backward arrays cross over at some point the
    //curr1 and curr2 values represent a relative mid-point on the 'shortest
    //common sub-sequence' path. By recursively finding these points the
    //whole path can be constructed.

    //OUTER LOOP ...
    //MAKE INCREASING OSCILLATIONS ABOUT CENTRE DIAGONAL UNTIL A FORWARD
    //DIAGONAL VECTOR IS GREATER THAN OR EQUAL TO A BACKWARD DIAGONAL.
     for D := 1 to MaxD do begin
      if fCancelled then begin
        result := false;
        exit;
      end;

      //forward loop...............................................
      //nb: k == index of current diagonal vector and
      //    will oscillate (in increasing swings) between -MaxD and MaxD
      k := -D;
      //stop going outside the grid ...
      while k < -(top2 - bottom2) do
        inc(k, 2);

      i := min(D, top1 - bottom1 -1); //24-Jan-04
      //the following code is duplicated for each elementsize (1,2,4)
      //so that the generated asm for PInteger/Word/ByteArray() is faster
      //(on Delphi 7 at least)
      case ElemSize of

      4: begin // Integer loop for higher speed
      while (k <= i) do begin
        //derive curr1 from the larger of adjacent vectors...
        if (k = -D) or ((k < D) and (diagVecF[k-1]<diagVecF[k+1])) then
          curr1 := diagVecF[k+1] else
          curr1 := diagVecF[k-1]+1;
        //derive curr2 (see above) ...
        curr2 := curr1 - bottom1 + bottom2 -k;
        //while (curr1+1,curr2+1) match, increment them...
        while (curr1 < top1) and (curr2 < top2) and
          (PIntegerArray(Array1)^[curr1]=PIntegerArray(Array2)^[curr2]) do begin
          inc(curr1);
          inc(curr2);
        end;
        //update current vector ...
        diagVecF[k] := curr1;

        //check if a vector in diagVecF overlaps the corresp. diagVecB vector.
        //(If a crossover point found here then further recursion is required)
        if odd(Delta) and (k > -D+Delta) and (k < D+Delta) and
           (diagVecF[k]>=diagVecB[k]) then begin
          //find subsequent points by recursion ...

          //To avoid declaring 2 extra variables in this recursive function ..
          //Delta & k are simply reused to store the curr1 & curr2 values ...
          Delta := curr1; k := curr2;
          //ignore trailing matches in lower block ...
          while (curr1 > bottom1) and (curr2 > bottom2) and
            (PIntegerArray(Array1)^[curr1-1]=PIntegerArray(Array2)^[curr2-1]) do begin
            dec(curr1);
            dec(curr2);
          end;
          result := RecursiveDiff(bottom1,bottom2,curr1,curr2);
          //do recursion with the upper block...
          if not result then exit;
          //and again with the lower block (nb: Delta & k are stored curr1 & curr2)...
          result := RecursiveDiff(Delta,k,top1,top2);
          exit; //All done!!!
        end;
        inc(k,2);
      end;

      //backward loop..............................................
      //nb: k will oscillate (in increasing swings) between -MaxD and MaxD
      k := -D+Delta;
      //stop going outside grid and also ensure we remain within the diagVecB[]
      //and diagVecF[] array bounds.
      //nb: in the backward loop it is necessary to test the bottom left corner.
      while k < -(top2 - bottom2) do inc(k, 2);

      i := min(D+Delta, top1 - bottom1 -1); //24-Jan-04
      while (k <= i) do begin
        //derive curr1 from the adjacent vectors...
        if (k = D+Delta) or ((k > -D+Delta) and (diagVecB[k+1]>diagVecB[k-1])) then
          curr1 := diagVecB[k-1] else
          curr1 := diagVecB[k+1]-1;

        curr2 := curr1 - bottom1 + bottom2 -k;
        //if curr2 < bottom2 then break; //shouldn't be necessary and adds a 3% time penalty

        //slide up and left if possible ...
        while (curr1 > bottom1) and (curr2 > bottom2) and
          (PIntegerArray(Array1)^[curr1-1]=PIntegerArray(Array2)^[curr2-1]) do begin
          dec(curr1);
          dec(curr2);
        end;
        //update current vector ...
        diagVecB[k] := curr1;

        //check if a crossover point reached...
        if not odd(Delta) and (k >= -D) and (k <= D) and
           (diagVecF[k]>=diagVecB[k]) then begin
          if (bottom1+1 = top1) and (bottom2+1 = top2) then begin
            //ie smallest divisible unit
            //(nb: skAddDel could also have been skDelAdd)
            Add(bottom1,bottom2);
            Delete(top1-1);
          end else begin
            //otherwise process the lower block ...
            result := RecursiveDiff(bottom1, bottom2, curr1, curr2);
            if not result then exit;
            //strip leading matches in upper block ...
            while (curr1 < top1) and (curr2 < top2) and
              (PIntegerArray(Array1)^[curr1]=PIntegerArray(Array2)^[curr2]) do begin
              inc(curr1);
              inc(curr2);
            end;
            //and finally process the upper block ...
            result := RecursiveDiff(curr1, curr2, top1, top2);
          end;
          exit; //All done!!!
        end;
        inc(k,2);
      end;
      end;

      2: begin // Word loop for higher speed
      while (k <= i) do begin
        //derive curr1 from the larger of adjacent vectors...
        if (k = -D) or ((k < D) and (diagVecF[k-1]<diagVecF[k+1])) then
          curr1 := diagVecF[k+1] else
          curr1 := diagVecF[k-1]+1;
        //derive curr2 (see above) ...
        curr2 := curr1 - bottom1 + bottom2 -k;
        //while (curr1+1,curr2+1) match, increment them...
        while (curr1 < top1) and (curr2 < top2) and
          (PWordArray(Array1)^[curr1]=PWordArray(Array2)^[curr2]) do begin
          inc(curr1);
          inc(curr2);
        end;
        //update current vector ...
        diagVecF[k] := curr1;

        //check if a vector in diagVecF overlaps the corresp. diagVecB vector.
        //(If a crossover point found here then further recursion is required.)
        if odd(Delta) and (k > -D+Delta) and (k < D+Delta) and
           (diagVecF[k]>=diagVecB[k]) then begin
          //find subsequent points by recursion ...

          //To avoid declaring 2 extra variables in this recursive function ..
          //Delta & k are simply reused to store the curr1 & curr2 values ...
          Delta := curr1; k := curr2;
          //ignore trailing matches in lower block ...
          while (curr1 > bottom1) and (curr2 > bottom2) and
            (PWordArray(Array1)^[curr1-1]=PWordArray(Array2)^[curr2-1]) do begin
            dec(curr1);
            dec(curr2);
          end;
          result := RecursiveDiff(bottom1,bottom2,curr1,curr2);
          //do recursion with the upper block...
          if not result then exit;
          //and again with the lower block (nb: Delta & k are stored curr1 & curr2)...
          result := RecursiveDiff(Delta,k,top1,top2);
          exit; //All done!!!
        end;
        inc(k,2);
      end;

      //backward loop..............................................
      //nb: k will oscillate (in increasing swings) between -MaxD and MaxD
      k := -D+Delta;
      //stop going outside grid and also ensure we remain within the diagVecB[]
      //and diagVecF[] array bounds.
      //nb: in the backward loop it is necessary to test the bottom left corner.
      while k < -(top2 - bottom2) do inc(k, 2);

      i := min(D+Delta, top1 - bottom1 -1); //24-Jan-04
      while (k <= i) do begin
        //derive curr1 from the adjacent vectors...
        if (k = D+Delta) or ((k > -D+Delta) and (diagVecB[k+1]>diagVecB[k-1])) then
          curr1 := diagVecB[k-1] else
          curr1 := diagVecB[k+1]-1;

        curr2 := curr1 - bottom1 + bottom2 -k;
        //if curr2 < bottom2 then break; //shouldn't be necessary and adds a 3% time penalty

        //slide up and left if possible ...
        while (curr1 > bottom1) and (curr2 > bottom2) and
          (PWordArray(Array1)^[curr1-1]=PWordArray(Array2)^[curr2-1]) do begin
          dec(curr1);
          dec(curr2);
        end;
        //update current vector ...
        diagVecB[k] := curr1;

        //check if a crossover point reached...
        if not odd(Delta) and (k >= -D) and (k <= D) and
           (diagVecF[k]>=diagVecB[k]) then begin
          if (bottom1+1 = top1) and (bottom2+1 = top2) then begin
            //ie smallest divisible unit
            //(nb: skAddDel could also have been skDelAdd)
            Add(bottom1,bottom2);
            Delete(top1-1);
          end else begin
            //otherwise process the lower block ...
            result := RecursiveDiff(bottom1, bottom2, curr1, curr2);
            if not result then exit;
            //strip leading matches in upper block ...
            while (curr1 < top1) and (curr2 < top2) and
              (PWordArray(Array1)^[curr1]=PWordArray(Array2)^[curr2]) do begin
              inc(curr1);
              inc(curr2);
            end;
            //and finally process the upper block ...
            result := RecursiveDiff(curr1, curr2, top1, top2);
          end;
          exit; //All done!!!
        end;
        inc(k,2);
      end;
      end;

      1: begin // Char loop for higher speed
      while (k <= i) do begin
        //derive curr1 from the larger of adjacent vectors...
        if (k = -D) or ((k < D) and (diagVecF[k-1]<diagVecF[k+1])) then
          curr1 := diagVecF[k+1] else
          curr1 := diagVecF[k-1]+1;
        //derive curr2 (see above) ...
        curr2 := curr1 - bottom1 + bottom2 -k;
        //while (curr1+1,curr2+1) match, increment them...
        while (curr1 < top1) and (curr2 < top2) and
          (PByteArray(Array1)^[curr1]=PByteArray(Array2)^[curr2]) do begin
          inc(curr1);
          inc(curr2);
        end;
        //update current vector ...
        diagVecF[k] := curr1;

        //check if a vector in diagVecF overlaps the corresp. diagVecB vector.
        //(If a crossover point found here then further recursion is required.)
        if odd(Delta) and (k > -D+Delta) and (k < D+Delta) and
           (diagVecF[k]>=diagVecB[k]) then begin
          //find subsequent points by recursion ...

          //To avoid declaring 2 extra variables in this recursive function ..
          //Delta & k are simply reused to store the curr1 & curr2 values ...
          Delta := curr1; k := curr2;
          //ignore trailing matches in lower block ...
          while (curr1 > bottom1) and (curr2 > bottom2) and
            (PByteArray(Array1)^[curr1-1]=PByteArray(Array2)^[curr2-1]) do begin
            dec(curr1);
            dec(curr2);
          end;
          result := RecursiveDiff(bottom1,bottom2,curr1,curr2);
          //do recursion with the upper block...
          if not result then exit;
          //and again with the lower block (nb: Delta & k are stored curr1 & curr2)...
          result := RecursiveDiff(Delta,k,top1,top2);
          exit; //All done!!!
        end;
        inc(k,2);
      end;

      //backward loop..............................................
      //nb: k will oscillate (in increasing swings) between -MaxD and MaxD
      k := -D+Delta;
      //stop going outside grid and also ensure we remain within the diagVecB[]
      //and diagVecF[] array bounds.
      //nb: in the backward loop it is necessary to test the bottom left corner.
      while k < -(top2 - bottom2) do inc(k, 2);

      i := min(D+Delta, top1 - bottom1 -1); //24-Jan-04
      while (k <= i) do begin
        //derive curr1 from the adjacent vectors...
        if (k = D+Delta) or ((k > -D+Delta) and (diagVecB[k+1]>diagVecB[k-1])) then
          curr1 := diagVecB[k-1] else
          curr1 := diagVecB[k+1]-1;

        curr2 := curr1 - bottom1 + bottom2 -k;
        //if curr2 < bottom2 then break; //shouldn't be necessary and adds a 3% time penalty

        //slide up and left if possible ...
        while (curr1 > bottom1) and (curr2 > bottom2) and
          (PByteArray(Array1)^[curr1-1]=PByteArray(Array2)^[curr2-1]) do begin
          dec(curr1);
          dec(curr2);
        end;
        //update current vector ...
        diagVecB[k] := curr1;

        //check if a crossover point reached...
        if not odd(Delta) and (k >= -D) and (k <= D) and
           (diagVecF[k]>=diagVecB[k]) then begin
          if (bottom1+1 = top1) and (bottom2+1 = top2) then begin
            //ie smallest divisible unit
            //(nb: skAddDel could also have been skDelAdd)
            Add(bottom1,bottom2);
            Delete(top1-1);
          end else begin
            //otherwise process the lower block ...
            result := RecursiveDiff(bottom1, bottom2, curr1, curr2);
            if not result then exit;
            //strip leading matches in upper block ...
            while (curr1 < top1) and (curr2 < top2) and
              (PByteArray(Array1)^[curr1]=PByteArray(Array2)^[curr2]) do begin
              inc(curr1);
              inc(curr2);
            end;
            //and finally process the upper block ...
            result := RecursiveDiff(curr1, curr2, top1, top2);
          end;
          exit; //All done!!!
        end;
        inc(k,2);
      end;
      end;
      end; // case ElemSize of

    end;
    result := false;
end;

function CalculateDiff(const Text1, Text2: string): TDiff;
var Hash1, Hash2: TIntegerDynArray;
    n1,n2: integer;
begin
  n1 := TextToHash(pointer(Text1),Hash1);
  n2 := TextToHash(pointer(Text2),Hash2);
  result := TDiff.Create(nil);
  if not result.Execute(@Hash1[0],@Hash2[0],4,n1,n2) then
    FreeAndNil(result);
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

function GetNextLineBegin(d: pChar): pChar;
begin
  if d^=#13 then inc(d);
  if d^=#10 then inc(d);
  result := d;
end;

function IgnoreLine(d: pChar): pChar;
begin
  d := GetNextLine(d);
  if d^=#13 then inc(d);
  if d^=#10 then inc(d);
  result := d;
end;

function TextToHash(P: PChar; var Hash: TIntegerDynArray): integer;
// crc32 of all lines in P^ into Hash[] - return Hash[] count
// crc32 is more precise than adler32 for small text lines
// no temporary string are created -> crc32 is computed on the fly in the PChar
var B,E: PChar;
begin
  result := 0;
  if P<>nil then
  while P^<>#0 do begin
//    while P^=' ' do inc(P); // trim left
    B := P;
    P := GetNextLine(P);
    if result=length(Hash) then
      SetLength(Hash,result+1024);
    E := P; while (E>B) and (E[-1]=' ') do dec(E); // trim right
    if E=B then
      Hash[result] := -1 else
      Hash[result] := crc32(cardinal(-1),B,E-B);
    inc(result);
    P := GetNextLineBegin(P);
  end;
end;

function DiffText(const Old, New: string; withLineNumber: boolean): string;
// return a human readable diff from versions
var WR: TStringWriter;
begin
  WR.Init;
  DiffText(Old,New,WR,withLineNumber);
  result := WR.Data;
end;

function isBinary(const Text: string): boolean;
var i: integer;
begin
  result := true;
  for i := 0 to length(Text)-1 do
    if not (Text[i+1] in [#9,#13,#10,#26,' '..#255]) then
      exit;
  result := false;
end;

procedure DiffText(const Old, New: string; var WR: TStringWriter; withLineNumber: boolean); overload;
// return a human readable diff from versions
var Diff: TDiff;
    i,j,k,v: integer;
    P1,P2,B: PAnsiChar;
procedure WriteIdem;
var B: PAnsiChar;
begin
  P1 := IgnoreLine(P1);
  B := P2; P2 := IgnoreLine(P2);
  WR.AddCardinal6('=',k,withLineNumber).Add(B,P2-B);
  inc(j);
  inc(k);
end;
begin
  // 1. Calculate Diff
  if isBinary(Old) or isBinary(New) then
    exit; // only for Text documents
  Diff := CalculateDiff(Old, New); // do the diff list creation
  if Diff=nil then
    exit;
  // 2. write diff as text into WR
  j := 0;
  k := 0;
  P1 := pointer(Old);
  P2 := pointer(New);
  for i := 0 to Diff.ChangeCount-1 do
  with Diff.Changes[i] do begin
    //first add preceeding unmodified lines from x to j
    v := x-j;
    if (i>0) and (v>3) then begin // show up to 3 lines after(i>0) modif
      WriteIdem;
      WriteIdem;
      WriteIdem;
      dec(v,3);
    end;
    if v>3 then begin  // show 3 lines before modif
      WR.AddShort(' ...'#13#10);
      v := 3;
    end;
    inc(k, x - j-v);
    while j < x-v do begin
       P1 := IgnoreLine(P1);
       P2 := IgnoreLine(P2);
       inc(j);
    end;
    while j < x do
      WriteIdem;
    case Kind of
    ckAdd: begin
      for j := k to k+Range-1 do begin
        B := P2; P2 := IgnoreLine(P2);
        WR.AddCardinal6('+',j,withLineNumber).Add(B,P2-B);
      end;
      j := x;
      k := y+Range;
    end;
    ckModify: begin
      for j := 0 to Range-1 do begin
        B := P1; P1 := IgnoreLine(P1);
        WR.AddCardinal6('<',0,withLineNumber).Add(B,P1-B);
        B := P2; P2 := IgnoreLine(P2);
        WR.AddCardinal6('>',k+j,withLineNumber).Add(B,P2-B);
      end;
      j := x+Range;
      k := y+Range;
    end;
    ckDelete: begin
      for j := x to x+Range-1 do begin
        B := P1; P1 := IgnoreLine(P1);
        WR.AddCardinal6('-',0,withLineNumber).Add(B,P1-B);
      end;
      j := x+Range;
    end;
    end;
  end;
  WR.EnsureLast(#13#10);
  // 3. release Diff
  Diff.Free;
  if P1^=#0 then exit else WriteIdem;  // diff: show up to 3 remaining lines
  if P1^=#0 then exit else WriteIdem;
  if P1^=#0 then exit else WriteIdem;
  if P1^<>#0 then WR.EnsureLast(#13#10).AddShort(' ...'#13#10);
end;

end.
