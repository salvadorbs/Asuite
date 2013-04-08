/// File Versioning comparison form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectVersionCompare;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, ComCtrls, ExtCtrls, SynMemoEx, ProjectCommons,
  ProjectVersioning,
  ProjectEditor, ProjectFrameViewer, ProjectFormViewTwo;

type
  TProjectVersionCompareForm = class(TForm)
    PanelLeft: TPanel;
    PanelRight: TPanel;
    ListFilesCommit: TListView;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFilesCommitSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure SynchronizeOnScroll(Sender: TObject);
  public
    Backup,
    Versions: TVersions;
    Editor: TFrameEditor;
    EditorText: string;
    Title,
    FileName: string;
    FileNameIndex: integer; // in Backup.FileNames[]
    View1,
    View2: TFrameViewer;
    procedure OnEditorBtnHistoryClick(Sender: TObject);
    procedure Init(aVersions: TVersions; aEditor: TFrameEditor; const aFileName: string);
    procedure CompareUsing(const aTitle: string; Back: TVersions);
  end;

var
  ProjectVersionCompareForm: TProjectVersionCompareForm;

implementation

uses
  ProjectVersionMain;

{$R *.dfm}

{ TProjectVersionCompareForm }

procedure TProjectVersionCompareForm.CompareUsing(const aTitle: string; Back: TVersions);
var i: integer;
begin
  Backup := Back;
  FileNameIndex := Back.FileNames.FindIndex(FileName);
  if FileNameIndex<0 then exit;
  Title := StringReplaceAll(aTitle,'&','');
  Caption := ' '+Title;
  WindowState := wsMaximized;
  View1.Free;
  View1 := TFrameViewer.Create(self,Back,nil,false);
  View1.FileName := FileName;
  View1.Parent := PanelLeft;
  View1.Align := alClient;
  View1.Name := 'View1';
  View1.Diff := true;
  View1.MemoEx.OnScroll := SynchronizeOnScroll;
  // right = current Editor content
  View2.Free;
  View2 := TFrameViewer.Create(self,Back,nil,false);
  View2.FileName := FileName;
  View2.Parent := PanelRight;
  View2.Align := alClient;
  View2.Name := 'View2';
  View2.Diff := true;
  View2.MemoEx.OnScroll := SynchronizeOnScroll;
  MainForm.PagesLeft.CommitInit(ListFilesCommit);
  for i := Back.Count-1 downto 0 do
  with Back.Values[i] do
    if FileName=FileNameIndex then
      MainForm.PagesLeft.CommitAdd(Back,Commit,true,ListFilesCommit);
  MainForm.PagesLeft.CommitEnd(ListFilesCommit);
  ListFilesCommit.OnResize := nil; // FormResize;
  ShowModal;
end;

procedure TProjectVersionCompareForm.OnEditorBtnHistoryClick(Sender: TObject);
// just a TMenuItem with Tag = BakcupDir #
var Menu: TMenuItem absolute Sender;
    b, path, FN: string;
    Back: TVersions;
begin
  if not Sender.InheritsFrom(TMenuItem) then exit;
  if Menu.Tag=-1 then // Tag=-1 -> commits = Version
    Back := Versions else begin
    b := Versions.Params['BackupDir'+IntToStr(Menu.Tag)];
    // BackupDir0=Local backup,012345,D:\-=- Backup -=-\Synopse\
    FN := Versions.GetBackupFileName(b,path);
    if not FileExists(FN) then exit;
    Back := TVersions.Create(FN);
  end;
  try
    EditorText := Editor.Memo.Lines.Text;
    CompareUsing('Compare with '+Menu.Caption, Back);
  finally
    if Back<>Versions then
      Back.Free;
  end;
end;

procedure TProjectVersionCompareForm.FormResize(Sender: TObject);
var W: integer;
begin
  W := (ClientWidth-ListFilesCommit.Width)div 2;
  PanelLeft.Width := W;
  PanelRight.Width := W;
end;

procedure TProjectVersionCompareForm.SynchronizeOnScroll(Sender: TObject);
var M: TMemoEx absolute Sender;
begin
  if not (Sender.InheritsFrom(TMemoEx)) then exit;
  if M=View1.MemoEx then
    View2.MemoEx.SetLeftTop(0,M.TopRow) else
  if M=View2.MemoEx then
    View1.MemoEx.SetLeftTop(0,M.TopRow);
end;

procedure TProjectVersionCompareForm.Init(aVersions: TVersions;
  aEditor: TFrameEditor; const aFileName: string);
begin
  Versions := aVersions;
  Editor := aEditor;
  FileName := aFileName;
end;

procedure TProjectVersionCompareForm.FormShow(Sender: TObject);
begin
  if ListFilesCommit.Items.Count>0 then
    ListFilesCommit.ItemIndex := 0;
  View2.MemoEx.SetFocus; // must be after having been shown
end;

procedure TProjectVersionCompareForm.ListFilesCommitSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
var List: TListView absolute Sender;
    Diff: TDiffCalc;
begin
  if csDestroying in ComponentState then exit;
  if not Sender.InheritsFrom(TListView) or not Assigned(Backup) then exit;
  if List.SelCount>1 then begin  // maximum 1 selected item
    Item.Selected := false;
    exit;
  end;
  Diff.Execute(Backup.GetVersionData(FileNameIndex,integer(Item.Data)), EditorText,
    View1.MemoEx, View2.MemoEx, false);
  Caption := format(' %s - %s :  %d add, %d mod, %d del',[
    Title,FileName,Diff.linesAdd,Diff.linesMod,Diff.linesDel]);
  View2.GotoNextModif;
end;

end.
