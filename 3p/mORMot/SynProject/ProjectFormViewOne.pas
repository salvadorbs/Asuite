/// File Versioning one file content view form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectFormViewOne;

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
  ProjectVersioning, ProjectFrameViewer, Menus, ExtCtrls;

type
  TFormViewOne = class(TForm)
    PopupMenu: TPopupMenu;
    Image: TImage;
  private
    { Private declarations }
    Current: PVersion;
    Versions: TVersions;
    Commit1, Commit2: integer;
    function NewMenu(const Name: string; Value: integer; Menu: TMenuItem): TMenuItem;
    procedure ViewCreate(aVersions: TVersions; aVersion: PVersion; fillMemo: boolean);
  public
    { Public declarations }
    View: TFrameViewer;
    procedure Load(aVersions: TVersions; aVersion: PVersion; aCommit2: integer=-1);
    procedure CompareMenu(Sender: TObject);
  end;

var
  FormViewOne: TFormViewOne;

implementation

{$R *.dfm}

uses
  ProjectCommons,
  ProjectRTF,
  ProjectDiff,
  ProjectFormViewTwo;

{ TFormViewOne }

procedure TFormViewOne.CompareMenu(Sender: TObject);
var Menu: TMenuItem absolute Sender;
    isDiff: boolean;
    FileName, Commit: integer;
    Diff: TDiffCalc;
begin
  if not Sender.InheritsFrom(TMenuItem) or (View=nil) then exit;
  isDiff := (Menu.Tag and (1 shl 16))<>0;
  Commit := Menu.Tag;
  if isDiff then
    Commit := Commit and pred(1 shl 16);
  dec(Commit);
  FileName := Current^.FileName;
  if Commit=-1 then
    Commit := Versions.PreviousCommit(FileName,Current^.Commit);
  if isDiff then begin
    if View=nil then begin
      ViewCreate(Versions, Current, false); // fillMemo=false
      Show;
    end;
    View.Diff := true;
    Diff.Execute(Versions.GetVersionData(FileName,Commit),
      Versions.GetVersionData(FileName,Current^.Commit),
      nil, View.MemoEx, true);
    Caption := format('%s diff vs #%d: %d add, %d mod, %d del',[
      Caption,Commit,Diff.linesAdd,Diff.linesMod,Diff.linesDel]);
  end else begin
    Hide;
    FormViewTwo.Load(Versions,Versions.GetVersion(FileName,Commit),Current,false);
  end;
end;

procedure TFormViewOne.ViewCreate(aVersions: TVersions; aVersion: PVersion; fillMemo: boolean);
var firstVisible: boolean;
    Ext, FN, tmp: string;
    M, W, H: integer;
begin
  firstVisible := View=nil;
  FreeAndNil(View);
  FN := aVersions.FileNames.Value[aVersion^.FileName];
  Ext := ExtractFileExt(FN);
  if (GetStringIndex(VALID_PICTURES_EXT, Ext)>=0)
    or SameText(Ext,'.BMP') or SameText(Ext,'.ICO') then begin
    // 1. display picture file
    SetLength(tmp,255);
    SetLength(tmp,GetTempPath(255,pointer(tmp)));
    tmp := tmp+ExtractFileName(FN); // use a temporary file to load pic contents
    if FileExists(tmp) and not DeleteFile(tmp) then exit;
    try
      StringToFile(tmp,aVersions.GetVersionData(aVersion^.FileName,aVersion^.Commit));
      Image.Picture.LoadFromFile(tmp);
    finally // on any load error -> exception -> delete tmp and display nothing
      DeleteFile(tmp);
    end;
    W := Image.Picture.Width;
    H := Image.Picture.Height;
    M := Monitor.Width div 2;
    if W>M then begin
      Image.Stretch := true;
      ClientWidth := M;
      ClientHeight := (M*H)div W;
    end else begin
      Image.Stretch := false;
      ClientWidth := W;
      ClientHeight := H;
    end;
    Image.Visible := true;
    Left := Monitor.Width-Width;
    Top := Monitor.WorkareaRect.Bottom-Height;
  end else begin
    // 2. display text content (using a TFrameViewer.MemoEx)
    Image.Visible := false;
    View := TFrameViewer.Create(self,aVersions,aVersion,true);
    View.Parent := self;
    View.Align := alClient;
    if firstVisible then
      ClientWidth := View.MemoEx.CellRect.Width*84+View.MemoEx.GutterWidth;
    if firstVisible then begin
      Top := 200;
      Left := Monitor.Width-Width;
      Height := Monitor.WorkareaRect.Bottom-Top;
    end;
  end;
end;

procedure TFormViewOne.Load(aVersions: TVersions; aVersion: PVersion; aCommit2: integer);
procedure CompareWith(Commit: integer);
begin
  NewMenu(format('Compare with %s',[aVersions.CommitToString(Commit)]),
    Commit+1,PopupMenu.Items).OnClick := CompareMenu;
end;
procedure DiffFrom(Commit: integer);
begin
  NewMenu(format('Diff from %s',[aVersions.CommitToString(Commit)]),
    (Commit+1)or(1 shl 16),PopupMenu.Items).OnClick := CompareMenu;
end;
var Menu: TMenuItem;
    i: integer;
    FN, C: integer;
begin
  if visible then begin // update only if visible
    ViewCreate(aVersions, aVersion, true); // fillMemo=true
    if View<>nil then
      View.MemoEx.PopupMenu := PopupMenu;
  end;
  FN := aVersion^.FileName;
  C := aVersion^.Commit;
  Caption := format('%s - #%d',[aVersions.FileNames.Value[FN],C]);
  Current := aVersion; // update PopupMenu (used also for caller)
  Versions := aVersions;
  Commit1 := aVersion^.Commit;
  Commit2 := Commit1;
  PopupMenu.Items.Clear;
  if View=nil then exit;
  if (aCommit2>=0) and (aCommit2<=aVersions.LastCommit) and (Commit1<>aCommit2) then begin
    Commit2 := aCommit2;
    CompareWith(-1);
    CompareWith(Commit2);
    DiffFrom(-1);
    DiffFrom(Commit2);
  end else begin
    CompareWith(-1);
    DiffFrom(-1);
  end;
  Menu := NewMenu(format('Compare #%d with',[C]),0,PopupMenu.Items);
  with Versions do
  for i := Count-1 downto 0 do
  with Values[i] do 
    if FileName=FN then
      if Commit=C then
        NewMenu('-',0,Menu) else
        NewMenu(CommitToString(Commit),Commit+1,Menu).OnClick := CompareMenu;
  if Menu.Count=1 then
    PopupMenu.Items.Clear; // no other version -> no popupmenu
end;

function TFormViewOne.NewMenu(const Name: string; Value: integer; Menu: TMenuItem): TMenuItem;
begin
  result := TMenuItem.Create(Owner);
  result.Caption := Name;
  result.Tag := Value;
  Menu.Add(result);
end;


end.
