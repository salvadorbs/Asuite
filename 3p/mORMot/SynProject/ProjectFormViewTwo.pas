/// File Versioning two files diff view form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectFormViewTwo;

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
  ProjectVersioning, ProjectFrameViewer, ExtCtrls, SynMemoEx;

type
  TDiffCalc = object
    FirstDiffLineIndex,
    linesAdd, linesMod, linesDel: integer;
    procedure Execute(Text1, Text2: string; Memo1, Memo2: TMemoEx;
      ShowDiffsOnly: boolean);
  end;

  TFormViewTwo = class(TForm)
    PanelLeft: TPanel;
    Splitter1: TSplitter;
    PanelRight: TPanel;
    procedure FormResize(Sender: TObject);
    procedure SynchronizeOnScroll(Sender: TObject);
  public
    View1,
    View2: TFrameViewer;
    Diff: TDiffCalc; // filled by Load()
    procedure Load(aVersions: TVersions; aVersion1, aVersion2: PVersion; ShowDiffsOnly: boolean);
  end;

var
  FormViewTwo: TFormViewTwo;


implementation

{$R *.dfm}

uses
  ProjectCommons,
  ProjectSections,
  ProjectDiffUnit; // diff calculation, less efficient than SynDiff, but human-readable



{ TDiffCalc }

procedure TDiffCalc.Execute(Text1, Text2: string; // not const (BinToText...)
  Memo1, Memo2: TMemoEx; ShowDiffsOnly: boolean);
var Diff: TDiff;
    P1, P2: PChar;
    i,j,k, v: integer;
    M1, M2: TStrings;
    s: string;
procedure WriteIdem;
begin
  if M1=nil then
    P1 := IgnoreLine(P1) else
    M1.AddObject(GetNextLine(P1,P1),pointer(integer(dkIdem)+j shl 3));
  if M2=nil then
    P2 := IgnoreLine(P2) else
    M2.AddObject(GetNextLine(P2,P2),pointer(integer(dkIdem)+k shl 3));
  inc(j);
  inc(k);
end;
begin
  // 1. init;
  linesAdd := 0;
  linesMod := 0;
  linesDel := 0;
  FirstDiffLineIndex := 0;
  if (Memo1=nil) and (Memo2=nil) then exit;
  Memo1.BeginUpdate;
  Memo2.BeginUpdate;
  if Memo1=nil then
    M1 := nil else
    M1 := Memo1.Lines;
  if Memo2=nil then
    M2 := nil else
    M2 := Memo2.Lines;
  try
    // 2. if binary file -> format as 80 bytes per line viewable text
    BinToTextIfNecessary(Text1);
    BinToTextIfNecessary(Text2);
    // 3. calculate changes
    Diff := CalculateDiff(Text1,Text2);
    if Diff=nil then begin
      if M1<>nil then
        M1.Text := Text1;
      if M2<>nil then
        M2.Text := Text2;
      exit;
    end;
    // 4. write changes
    if M1<>nil then
      M1.Clear;
    if M2<>nil then
      M2.Clear;
    j := 0;
    k := 0;
    P1 := pointer(Text1);
    P2 := pointer(Text2);
    for i := 0 to Diff.ChangeCount-1 do
      with Diff.Changes[i] do begin
        //first add preceeding unmodified lines
        if ShowDiffsOnly then begin // add from j to x
          v := x-j;
          if (i>0) and (v>3) then begin // show up to 3 lines after(i>0) modif
            WriteIdem;
            WriteIdem;
            WriteIdem;
            dec(v,3);
          end;
          if v>3 then begin  // show 3 lines before modif
            if M2<>nil then M2.Add(' ...');
            v := 3;
          end;
          inc(k, x-j-v);
          while j < x-v do begin
             P1 := IgnoreLine(P1);
             P2 := IgnoreLine(P2);
             inc(j);
          end;
        end;
        while j < x do
          WriteIdem;
        if FirstDiffLineIndex=0 then
          FirstDiffLineIndex := k+1;
        case Kind of
        ckAdd: begin
          for j := k to k+Range-1 do begin
            if M1<>nil then
              M1.AddObject('',pointer(integer(dkAdd)));
            if M2=nil then
              P2 := IgnoreLine(P2) else
              M2.AddObject(GetNextLine(P2,P2),pointer(integer(dkAdd)+j shl 3));
          end;
          inc(linesAdd,Range);
          j := x;
          k := y+Range;
        end;
        ckModify: begin
          for j := 0 to Range-1 do begin
            s := GetNextLine(P1,P1);
            if M1<>nil then
              M1.AddObject(s,pointer(integer(dkModify)+(x+j)shl 3));
            if M2=nil then
              P2 := IgnoreLine(P2) else begin
              if ShowDiffsOnly then // diff: show original line before
                M2.AddObject(s,pointer(integer(dkModify)));
              M2.AddObject(GetNextLine(P2,P2),pointer(integer(dkModify)+(k+j)shl 3));
            end;
          end;
          inc(linesMod,Range);
          j := x+Range;
          k := y+Range;
        end;
        ckDelete: begin
          for j := x to x+Range-1 do begin
            s := GetNextLine(P1,P1);
            if M1<>nil then
              M1.AddObject(s,pointer(integer(dkDelete)+j shl 3));
            if M2<>nil then
              if (M1=nil) and ShowDiffsOnly then // diff: show deleted lines in red
                M2.AddObject(s,pointer(integer(dkDelete))) else
                M2.AddObject('',pointer(integer(dkDelete)));
          end;
          inc(linesDel,Range);
          j := x+Range;
        end;
        end;
      end;
    //add remaining unmodified lines...
    if ShowDiffsOnly then begin
      if P1^<>#0 then begin WriteIdem;  // diff: show up to 3 lines after
      if P1^<>#0 then begin WriteIdem;
      if P1^<>#0 then begin WriteIdem;
      if P1^<>#0 then M2.Add(' ...'); end; end; end;
    end else
      while P1^<>#0 do
        WriteIdem;
    Diff.Free;
  finally
    Memo2.EndUpdate;
    Memo1.EndUpdate;
  end;
end;

{ TFormViewTwo }

procedure TFormViewTwo.Load(aVersions: TVersions; aVersion1, aVersion2: PVersion; ShowDiffsOnly: boolean);
var s: string;
    M: TMonitor;
    v1, v2: string;
begin
  if not Visible then begin
    Show;
    if Screen.MonitorCount>1 then begin // put on other monitor
      if Screen.Monitors[0]=Monitor then
        M := Screen.Monitors[1] else
        M := Screen.Monitors[0];
      with M.WorkareaRect do // manual maximize
        SetBounds(Left,Top,Right-Left,Bottom-Top);
    end;
  end;
  View1.Free;
  View1 := TFrameViewer.Create(self,aVersions,aVersion1,false);
  View1.Parent := PanelLeft;
  View1.Align := alClient;
  View1.Name := 'View1';
  View1.Diff := true;
  View1.MemoEx.OnScroll := SynchronizeOnScroll;
  View2.Free;
  View2 := TFrameViewer.Create(self,aVersions,aVersion2,false);
  View2.Parent := PanelRight;
  View2.Align := alClient;
  View2.MemoEx.SetFocus;
  View2.Name := 'View2';
  View2.Diff := true;
  View2.MemoEx.OnScroll := SynchronizeOnScroll;
  if (aVersion1=nil) or (aVersion2=nil) then
    exit; // Diff.Execute() and Caption will be done manually
  v1 := aVersions.GetVersionData(aVersion1^.FileName,aVersion1^.Commit);
  v2 := aVersions.GetVersionData(aVersion2^.FileName,aVersion2^.Commit);
  Diff.Execute(v1, v2, View1.MemoEx, View2.MemoEx, ShowDiffsOnly);
  View1.GotoNextModif;
  if aVersion1^.FileName=aVersion2^.FileName then
    s := format('%s #%d vs #%d',[
      aVersions.FileNames.Value[aVersion1^.FileName],aVersion1^.Commit,aVersion2^.Commit]) else
    s := format('%s #%d vs %s #%d',[
      aVersions.FileNames.Value[aVersion1^.FileName],aVersion1^.Commit,
      aVersions.FileNames.Value[aVersion2^.FileName],aVersion2^.Commit]);
  Caption := format('%s: %d add, %d mod, %d del',[
      s,Diff.linesAdd,Diff.linesMod,Diff.linesDel]);
end;

procedure TFormViewTwo.FormResize(Sender: TObject);
begin
  PanelLeft.Width := ClientWidth div 2-Splitter1.Width;
end;

procedure TFormViewTwo.SynchronizeOnScroll(Sender: TObject);
var M: TMemoEx absolute Sender;
begin
  if not (Sender.InheritsFrom(TMemoEx)) then exit;
  if M=View1.MemoEx then
    View2.MemoEx.SetLeftTop(0,M.TopRow) else
  if M=View2.MemoEx then
    View1.MemoEx.SetLeftTop(0,M.TopRow);
end;


end.
