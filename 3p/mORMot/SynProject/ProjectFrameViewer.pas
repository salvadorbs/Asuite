/// File Versioning file view frame
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectFrameViewer;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ProjectCommons,
  ProjectSections, ProjectVersioning, SynMemoEx;

type
  TDiffKind = (dkIdem, dkAdd, dkDelete, dkModify);

  TFrameViewer = class(TFrame)
    MemoEx: TMemoEx;
    FindDialog: TFindDialog;
    procedure MemoExPaintGutter(Sender: TObject; Canvas: TCanvas; const Rect: TRect);
    procedure MemoExKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FindDialogFind(Sender: TObject);
  private
    fExt,
    fFileName: string;
    fDiff: boolean;
    fInternalOnGetLineAttr: TOnGetLineAttr;
    procedure MemoExGetLineAttr(Sender: TObject; const Line: String;
      index: Integer; const SelAttrs: TSelAttrs; var Attrs: TLineAttrs);
    procedure SetDiff(const Value: boolean);
    procedure SetFileName(const Value: string);
  public
    Versions: TVersions;
    constructor Create(AOwner: TComponent; aVersions: TVersions;
      aVersion: PVersion; fillMemo: boolean); reintroduce;
    procedure GotoNextModif;
    property FileName: string read fFileName write SetFileName;
    property Diff: boolean read fDiff write SetDiff;
  end;

const
  DIFF_KIND_COLOR: array[TDiffKind] of TColor =
    (clWhite, $F0CCA8, $BB77FF, $6FFB8A);

/// if binary file -> format as 80 bytes per line viewable text
procedure BinToTextIfNecessary(var Text: string);

/// if binary file -> format as 40 bytes per line viewable hex text
procedure BinToHexIfNecessary(var Text: string);

procedure TabToSpace(P: PChar);

const
  BIN_MAX_SIZE = 256*1024; // truncate binary files under this size

implementation

{$R *.dfm}

uses
  ProjectMemoExSyntax, ProjectRTF, ProjectVersionMain;

{ TFrameViewer }

procedure TabToSpace(P: PChar);
begin
  if P<>nil then
  while P^<>#0 do begin
    if P^=#9 then
      P^ := ' ';
    inc(P);
  end;
end;

procedure BinToHexIfNecessary(var Text: string);
// if binary file -> format as 40 bytes per line viewable hex text
var L, i, j, n: integer;
    old: string;
    S,D: PChar;
begin
  L := length(Text);
  if L=0 then exit;
  for i := 0 to L-1 do
    if not (Text[i+1] in [#9,#13,#10,#26,' '..#255]) then begin // may be binary file
      if L>BIN_MAX_SIZE then begin
        L := BIN_MAX_SIZE;  // trunc too big file
        old := Copy(Text,1,L);
      end else
        old := Text;
      n := (L+39) div 40; // line count
      SetLength(Text,L*2+n*2);
      S := pointer(old);
      D := pointer(Text);
      for j := 0 to n-2 do begin
        BinToHex(S,D,40);
        inc(S,40);
        inc(D,80);
        pWord(D)^ := $0a0d;
        inc(D,2);
      end;
      n := L mod 40;
      if n>0 then begin
        BinToHex(S,D,n);
        inc(D,n);
        pWord(D)^ := $0a0d;
        inc(D,2);
      end;
      D^ := #0;
      exit;
    end;
  TabToSpace(pointer(Text));  
end;

procedure GoodWrite(S,D: PChar; n: integer);
var i: integer;
begin
  for i := 1 to n do begin
    if S^<' ' then
      D^ := ' ' else
      D^ := S^;
    inc(S);
    inc(D);
  end;
end;

procedure BinToTextIfNecessary(var Text: string);
// if binary file -> format as 80 bytes per line viewable text
var L, i, j, n: integer;
    old: string;
    S,D: PChar;
begin
  L := length(Text);
  if L=0 then exit;
  for i := 0 to L-1 do
    if not (Text[i+1] in [#9,#13,#10,#26,' '..#255]) then begin // may be binary file
      if L>BIN_MAX_SIZE then begin
        L := BIN_MAX_SIZE;  // trunc too big file
        old := Copy(Text,1,L);
      end else
        old := Text;
      n := (L+79) div 80; // line count
      SetLength(Text,L+n*2);
      S := pointer(old);
      D := pointer(Text);
      for j := 0 to n-2 do begin
        GoodWrite(S,D,80);
        inc(S,80);
        inc(D,80);
        pWord(D)^ := $0a0d;
        inc(D,2);
      end;
      n := L mod 80;
      if n>0 then begin
        GoodWrite(S,D,n);
        inc(D,n);
        pWord(D)^ := $0a0d;
        inc(D,2);
      end;
      D^ := #0;
      exit;
    end;
  TabToSpace(pointer(Text));  
end;

constructor TFrameViewer.Create(AOwner: TComponent; aVersions: TVersions;
  aVersion: PVersion; fillMemo: boolean);
var Text: string;
begin
  inherited Create(AOwner);
  if Screen.Fonts.IndexOf('Consolas')>=0 then
    MemoEx.Font.Name := 'Consolas';
  MemoEx.Font.Size := 9;
  Versions := aVersions;
  if (aVersion=nil) or (Versions=nil) then exit;
  FileName := Versions.FileNames.Value[aVersion^.FileName];
  MemoEx.GutterWidth := 40;
  MemoEx.OnPaintGutter := MemoExPaintGutter;
  if fillMemo then begin
    Text := Versions.GetVersionData(aVersion^.FileName,aVersion^.Commit);
    if not Assigned(fInternalOnGetLineAttr) then
      BinToTextIfNecessary(Text) else
      TabToSpace(pointer(Text));
    MemoEx.Lines.Text := Text;
    MemoEx.OnGetLineAttr := fInternalOnGetLineAttr;
  end else
    MemoEx.OnGetLineAttr := MemoExGetLineAttr;
end;

procedure LineBackColor(Color: TColor; var Attrs: TLineAttrs; L: integer);
var i: integer;
begin
  for i := 0 to L-1 do
    Attrs[i].BC := Color;
end;

procedure TFrameViewer.MemoExGetLineAttr(Sender: TObject;
  const Line: String; index: Integer; const SelAttrs: TSelAttrs;
  var Attrs: TLineAttrs);
var Kind: TDiffKind;
begin
  if Assigned(fInternalOnGetLineAttr) then // syntax highlighting of the text
    fInternalOnGetLineAttr(Sender,Line,Index,SelAttrs,Attrs);
  if Diff and (index>=0) and (index<MemoEx.Lines.Count) then begin
    Kind := TDiffKind(integer(MemoEx.Lines.Objects[index]) and 7);
    if Kind<>dkIdem then // modified? -> special background color
      LineBackColor(DIFF_KIND_COLOR[Kind],Attrs,length(Line));
  end;
end;

procedure TFrameViewer.MemoExPaintGutter(Sender: TObject; Canvas: TCanvas; const Rect: TRect);
var L: integer;
    M: TMemoEx absolute Sender;
    s: string;
    i, v: integer;
begin
  if M.GutterWidth=0 then exit;
  Canvas.Font.Color := clWhite;
  for L := 0 to M.VisibleRowCount-1 do begin
    v := M.TopRow+L;
    if v>=M.Lines.Count then exit;
    with M.Lines.Paragraphs[v]^ do
    if Diff and (FStrings[0]='') then // diff: don't show line number for void line
      continue else begin
      i := integer(FObject) shr 3;
      if i=0 then
        if Diff then
          continue else
          i := v+1;
      s := IntToStr(i);
      Canvas.TextOut(Rect.Right-Canvas.TextWidth(s)-8,Rect.Top+1+L*M.CellRect.Height,s);
    end;
  end;
end;

procedure TFrameViewer.GotoNextModif;
var i: integer;
    L: TEditorStrings;
    K1,K2: TDiffKind;
begin
  if not Diff then exit;
  L := MemoEx.Lines;
  if MemoEx.CaretY>=L.Count then exit;
  K1 := TDiffKind(integer(L.Paragraphs[MemoEx.CaretY]^.FObject) and 7);
  for i := MemoEx.CaretY+1 to L.Count-1 do begin // search current
    K2 := TDiffKind(integer(L.Paragraphs[i]^.FObject) and 7);
    if (K2<>dkIdem) and (K2<>K1) then begin
      MemoEx.TopRow := i-8;
      MemoEx.SetCaret(0,i);
      exit;
    end;
    K1 := K2;
  end;
  MemoEx.SetCaret(0,0); // not found -> will search from bottom down
end;

procedure TFrameViewer.MemoExKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var P: TWinControl;
    W: string;
begin
  case Key of
  VK_F3:
    if FindDialog.FindText<>'' then // F3 = Find Next
      FindDialog.OnFind(nil);
  VK_ESCAPE: begin // escape key -> hide code form
    P := Parent;
    while (P<>nil) and not P.InheritsFrom(TCustomForm) do
      P := P.Parent;
    if (P<>nil) and (P<>Application.MainForm) and P.InheritsFrom(TCustomForm) then
      if fsModal in TCustomForm(P).FormState then
        TCustomForm(P).Close else
        TCustomForm(P).Hide;
  end;
  VK_F7: begin
      GotoNextModif; // F7 for next modif
      Key := 0;
    end;
  ord('F'):
    if ssCtrl in Shift then begin
      W := MemoEx.GetWordOnCaret;
      if W<>'' then
        FindDialog.FindText := W;
      FindDialog.Execute; // Ctrl + F = find
    end;
{  ord('S'):
    if ssAlt in Shift then begin
      GotoNextModif; // Alt-S for next modif
      Key := 0;
    end; }
  end; // case Key of
end;

procedure TFrameViewer.SetDiff(const Value: boolean);
begin
  fDiff := Value;
  if fDiff then
    MemoEx.OnGetLineAttr := MemoExGetLineAttr else
    MemoEx.OnGetLineAttr := fInternalOnGetLineAttr;
end;


procedure TFrameViewer.FindDialogFind(Sender: TObject);
begin
  if not MemoEx.FindNext(FindDialog.FindText,not(frMatchCase in FindDialog.Options)) then
    FindDialog.CloseDialog;
end;

procedure TFrameViewer.SetFileName(const Value: string);
begin
  fFileName := Value;
  fExt := ExtractFileExt(fFileName);
  fInternalOnGetLineAttr := TProjectSyntax.GetHighliter(fExt);
end;

initialization
  assert(integer(high(TDiffKind))<8);
end.

