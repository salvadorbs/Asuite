/// File Versioning version selection visual frame
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectVersionPages;

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
  Dialogs, ComCtrls, Menus,
  ProjectCommons, ProjectSections, ProjectVersioning,
  ExtCtrls, StdCtrls, Buttons, ProjectEditor;

type
  TFramePages = class(TFrame)
    Pages: TPageControl;
    PageCommit: TTabSheet;
    PageFile: TTabSheet;
    Splitter1: TSplitter;
    ListCommit: TListView;
    CommitRight: TPanel;
    ListCommitFiles: TListView;
    CommitRightTop: TPanel;
    CheckBoxOnlyModified: TCheckBox;
    LabelCommitRightTop: TLabel;
    BtnComitRightTop: TSpeedButton;
    ListFiles: TListView;
    Splitter2: TSplitter;
    ListFilesCommit: TListView;
    procedure CheckBoxOnlyModifiedClick(Sender: TObject);
    procedure BtnComitRightTopClick(Sender: TObject);
    procedure ListFilesCommitSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListCommitFilesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PagesChange(Sender: TObject);
  private
    { Private declarations }
    fVersions: TVersions;
    Col3,Col4,Col5,UpdateFilesDone: boolean;
    procedure OnCommitResize(Sender: TObject);
    procedure OnCommitSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure OnCommitCompare(Sender: TObject; Node1, Node2: TListItem; Data: Integer; var Compare: Integer);
    procedure OnCommitColumnClick(Sender: TObject; Column: TListColumn);
    procedure VersionsToCommit(Commit: TListView);
    procedure OnFilesSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure OnFilesCompare(Sender: TObject; Node1, Node2: TListItem; Data: Integer; var Compare: Integer);
    procedure FilesToList(Files: TIntegerDynArray; List: TListView);
    procedure FilesDblClick(Sender: TObject);
    procedure FilesInit(List: TListView);
    procedure FilesAdd(f: integer; List: TListView);
    procedure FilesEnd(List: TListView);
    function OtherVersion(aVersion: integer): integer; // either Commit1 or Commit2
    procedure UpdateFiles;
  public
    { Public declarations }
    Commit1, Commit2: integer;
    ListFilesCurrentFN: integer;
    constructor Create(AOwner: TComponent; aVersions: TVersions); reintroduce;
    procedure UpdateVersions(NewVersions: TVersions=nil); // reload from Versions
    procedure CommitInit(Commit: TListView);
    procedure CommitAdd(Versions: TVersions; C: integer; withDesc: boolean; Commit: TListView);
    procedure CommitEnd(Commit: TListView);
    property Versions: TVersions read fVersions;
  end;


procedure ShowVersions(aVersions: TVersions);


implementation

{$R *.dfm}

uses ProjectFormViewOne, ProjectFormViewTwo, ProjectVersionSCR, SynMemoEx;

{ TFramePages }

constructor TFramePages.Create(AOwner: TComponent; aVersions: TVersions);
begin
  inherited Create(AOwner);
  fVersions := aVersions;
end;

procedure TFramePages.FilesInit(List: TListView);
begin
  List.Items.BeginUpdate;
  List.Columns.Clear;
  with List.Columns.Add do begin
    Caption := 'Directory';
    Width := 90;
  end;
  with List.Columns.Add do
    Caption := 'File Name'; // with will be done in List.OnResize() below
  with List.Columns.Add do begin
    Caption := 'Date';
    Width := 120;
  end;
  List.Items.Clear;
  List.ReadOnly := true; // must be just after Items.Clear (bug on XP)
end;

procedure TFramePages.FilesAdd(f: integer; List: TListView);
var FN: string;
    V: PVersion;
begin
  if (f<0) or (f>=Versions.Count) then exit;
  V := @Versions.Values[f];
  FN := Versions.FileNames.Value[V^.FileName];
  with List.Items.Add do begin
    Caption := ExtractFilePath(FN);
    Data := V;
    SubItems.Add(ExtractFileName(FN));
    if V^.Age=1 then
      SubItems.Add('deleted') else
      SubItems.Add(DateTimeToStr(FileDateToDateTime(V^.Age)));
  end;
end;

procedure TFramePages.FilesEnd(List: TListView);
begin
  List.ViewStyle := vsReport;
  List.RowSelect := true;
  List.OnCompare := OnFilesCompare;
  List.SortType := stData;
  List.OnColumnClick := OnCommitColumnClick; // List.Tag = item sort column
  List.Tag := 0; // List.Tag = item sort column
  List.OnResize := OnCommitResize; // resize the 2nd column from the Client size
  List.AlphaSort;
  List.Items.EndUpdate;
  List.OnResize(List);
end;

procedure TFramePages.FilesToList(Files: TIntegerDynArray; List: TListView);
var i: integer;
begin
  if csDestroying in ComponentState then exit;
  FilesInit(List);
  for i := 0 to high(Files) do
    FilesAdd(Files[i],List);
  List.OnSelectItem := OnFilesSelect;
  List.OnDblClick := FilesDblClick;
  FilesEnd(List);
end;

procedure TFramePages.OnCommitColumnClick(Sender: TObject; Column: TListColumn);
var List: TListView absolute Sender;
    i: integer;
//    j, sel: integer;
//    Current: PVersion;
begin
  if not Sender.InheritsFrom(TListView) then exit;
  for i := 0 to List.Columns.Count-1 do
    if List.Column[i]=Column then begin
      List.Tag := i; // List.Tag = item sort column
      List.AlphaSort;
{      if List.ItemIndex>=0 then
        List.ViewOrigin := Point(0,-List.ItemIndex*List.Height);
{      sel := List.ItemIndex;
      if sel<0 then
        Current := nil else
        Current := List.Items[sel].Data;
      List.AlphaSort;
      if List<>nil then
        for j := 0 to List.Items.Count-1 do
          if List.Items[j].Data=Current then begin
             List.ItemIndex := j;
            exit;
          end;}
      exit;
    end;
end;

procedure TFramePages.OnCommitCompare(Sender: TObject;
  Node1, Node2: TListItem; Data: Integer; var Compare: Integer);
var List: TListView absolute Sender;
    D1,D2: TDateTime;
begin
  if (Self=nil) or not Sender.InheritsFrom(TListView) or (List.Columns.Count<5) then exit;
  case List.Tag of // item sort column #
  0: Compare := StrToIntDef(Node2.Caption,0)-StrToIntDef(Node1.Caption,0);
  1: Compare := CompareText(Node1.SubItems[0],Node2.SubItems[0]);
  2: if TryStrToDateTime(Node1.SubItems[1],D1) and
        TryStrToDateTime(Node2.SubItems[1],D2) then
       Compare := Round(D2-D1) else
       Compare := CompareText(Node1.SubItems[1],Node2.SubItems[1]);
  3: Compare := StrToIntDef(Node1.SubItems[2],0)-StrToIntDef(Node2.SubItems[2],0);
  4: Compare := CompareText(Node1.SubItems[3],Node2.SubItems[3]);
  end;
end;

procedure TFramePages.OnCommitResize(Sender: TObject);
// resize the 2nd column from the Client size
var List: TListView absolute Sender;
    i, W: integer;
begin
  if not Sender.InheritsFrom(TListView) then exit;
  W := 0;
  for i := 0 to List.Columns.Count-1 do
    if i<>1 then
      inc(W,List.Column[i].Width);
  List.Column[1].Width := List.ClientWidth-W;
end;

procedure TFramePages.OnCommitSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var List: TListView absolute Sender;
    Files: TIntegerDynArray;
    Title: string;
    i: integer;
    Item2: TListItem;
begin
  if csDestroying in ComponentState then exit;
  if not Sender.InheritsFrom(TListView) then exit;
  if List.SelCount>2 then begin  // maximum 2 selected items
    Item.Selected := false;
    exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    if Item=nil then
      Item := List.ItemFocused;
    Title := '';
    Commit1 := -1;
    Commit2 := -1;
    if (Item=nil) or (List.SelCount=0) then
      SetLength(Files,0)
    else begin
      if List.SelCount=1 then begin // 1 item select -> show files within
        CheckBoxOnlyModified.Enabled := true;
        Title := '#'+Item.Caption+' '+Item.SubItems[0];
        Commit1 := integer(Item.Data);
        Commit2 := Commit1;
        Versions.GetVersionValues(Commit1,Files,CheckBoxOnlyModified.Checked);
      end else begin
        CheckBoxOnlyModified.Enabled := false;
        for i := 0 to List.Items.Count-1 do begin // 2 items select -> show diff
          Item2 := List.Items[i];
          if Item2.Selected and (Item<>Item2) then begin
            Commit1 := integer(Item2.Data);
            Commit2 := integer(Item.Data);
            Versions.GetVersionValuesDiff(Commit1,Commit2,Files);
            Title := format('#%d vs #%d',[Commit2, Commit1]);
            break;
          end;
        end;
      end;
    end;
    LabelCommitRightTop.Caption := Title;
    FilesToList(Files,ListCommitFiles);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFramePages.OnFilesCompare(Sender: TObject; Node1,
  Node2: TListItem; Data: Integer; var Compare: Integer);
var List: TListView absolute Sender;
begin
  if (Self=nil) or not Sender.InheritsFrom(TListView) then exit;
  if (Node1=Node2) or (Node1.Data=Node2.Data) then
    Compare := 0 else
  if (Node1=nil) or (Node1.Data=nil) then
    Compare := 1 else
  if (Node2=nil) or (Node2.Data=nil) then
    Compare := -1 else
  case List.Tag of // item sort column #
  0: begin
    Compare := CompareText(Node1.Caption,Node2.Caption); // compare directory
    if Compare=0 then // in the same directory  -> compare filename
      Compare := CompareText(Node1.SubItems[0],Node2.SubItems[0]);
  end;
  1: Compare := CompareText(Node1.SubItems[0],Node2.SubItems[0]); // filename
  2: Compare := PVersion(Node1.Data)^.Age-PVersion(Node2.Data)^.Age; // Age
  else
     Compare := CompareText(Versions.FileName[PVersion(Node1.Data)^.FileName],
       Versions.FileName[PVersion(Node2.Data)^.FileName]);
  end;
end;

procedure TFramePages.OnFilesSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var List: TListView absolute Sender;
    Vers: PVersion;
    i: integer;
begin
  if csDestroying in ComponentState then exit;
  if not Sender.InheritsFrom(TListView) then exit;
  if (Item=nil) or not Selected or (Item.Data=nil) then exit;
  Vers := Item.Data;
  if List=ListFiles then begin
    CommitInit(ListFilesCommit);
    ListFilesCurrentFN := Vers^.FileName;
    for i := 0 to Versions.Count-1 do
    with Versions.Values[i] do
      if FileName=ListFilesCurrentFN then
        CommitAdd(Versions,Commit,true,ListFilesCommit);
    CommitEnd(ListFilesCommit);
    ListFilesCommit.ItemIndex := 0; // select most recent version
  end else begin
    FormViewOne.Load(Versions,Vers,OtherVersion(Vers^.Commit)); // update FormViewOne+PopupMenu
    List.PopupMenu := FormViewOne.PopupMenu;
  end;
end;

procedure TFramePages.UpdateFiles;
var i, FN: integer;
    Done: array of boolean;
begin
  if not UpdateFilesDone then
  try
    Screen.Cursor := crHourGlass;
    UpdateFilesDone := true;
    FilesInit(ListFiles);
    SetLength(Done,Versions.FileNames.Count); // will do fillchar()
    for i := 0 to Versions.Count-1 do begin
      FN := Versions.Values[i].FileName;
      if not Done[FN] then begin
        FilesAdd(i,ListFiles);
        Done[FN] := true;
      end;
    end;
    FilesEnd(ListFiles);
    ListFiles.OnSelectItem := OnFilesSelect;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFramePages.UpdateVersions(NewVersions: TVersions=nil);
begin
  if NewVersions<>nil then
    fVersions := NewVersions;
  VersionsToCommit(ListCommit);
  UpdateFilesDone := false;
  Pages.ActivePageIndex := 0; // select Commit
end;

procedure TFramePages.VersionsToCommit(Commit: TListView);
var i: integer;
begin
  CommitInit(Commit);
  for i := 0 to Versions.LastCommit do
    CommitAdd(Versions,i,true,Commit);
  Commit.MultiSelect := true;
  Commit.OnSelectItem := OnCommitSelect;
  CommitEnd(Commit);
end;

procedure TFramePages.CheckBoxOnlyModifiedClick(Sender: TObject);
begin
  if ListCommit.SelCount=1 then
    OnCommitSelect(ListCommit,ListCommit.ItemFocused,true);
end;

procedure TFramePages.BtnComitRightTopClick(Sender: TObject);
var List: TListView;
begin
  if Sender=BtnComitRightTop then
    List := ListCommit else
    List := ListFiles;
  if List.Width=0 then
    List.Width := 200 else
    List.Width := 0;
end;

procedure TFramePages.FilesDblClick(Sender: TObject);
var List: TListView absolute Sender;
    Item: TListItem;
    Vers: PVersion;
begin
  if (csDestroying in ComponentState) or not Sender.InheritsFrom(TListView) then exit;
  Item := List.ItemFocused;
  if Item=nil then exit;
  Vers := Item.Data;
  if Vers=nil then exit;
  if not FormViewOne.Visible then begin
    FormViewOne.Show;
    FormViewOne.Load(Versions,Vers,OtherVersion(Vers^.Commit)); // update
    if (FormViewOne.View<>nil) and (FormViewOne.View.MemoEx<>nil) then
      FormViewOne.View.MemoEx.SetFocus;
  end;
end;

function TFramePages.OtherVersion(aVersion: integer): integer;
// either Commit1 or Commit2
begin
  if aVersion<=Commit1 then
    result := Commit2 else
  if aVersion<=Commit2 then
    result := Commit1 else
    result := -1;
end;

procedure TFramePages.CommitInit(Commit: TListView);
begin
  Commit.Items.BeginUpdate;
  Commit.Columns.Clear;
  with Commit.Columns.Add do begin
    Caption := '#';
    Width := 30;
  end;
  with Commit.Columns.Add do
    Caption := 'Description'; // with will be done in Commit.OnResize() below
  with Commit.Columns.Add do begin
    Caption := 'Date';
    Width := 80;
  end;
  with Commit.Columns.Add do begin
    Caption := 'SCR';
    Width := 40;
  end;
  with Commit.Columns.Add do begin
    Caption := 'Request';
    Width := 60;
  end;
  Commit.Items.Clear;
  Commit.ReadOnly := true; // must be just after Items.Clear (bug on XP)
  Col3 := false;
  Col4 := false;
  Col5 := false;
end;

procedure TFramePages.CommitAdd(Versions: TVersions; C: integer; withDesc: boolean;
  Commit: TListView);
var Sec: TSection;
    s: string;
    Date: TDateTime;
    i: integer;
begin
  Sec := Versions.Commits[IntToHex(C,0)];
  if Sec<>nil then
  with Commit.Items.Add do begin
    Caption := IntToStr(C);
    Data := pointer(C);
    if withDesc then
      SubItems.Add(Sec['Description']) else
      SubItems.Add('');
    s := Sec['Date'];
    if s<>'' then begin
      Col3 := true;
      for i := 1 to length(s) do
        if s[i]=',' then s[i] := '.'; // force match DecimalSeparator='.'
      if TryStrToFloat(s,Double(Date)) then
        s := DateTimeToStr(Date);
    end;
    SubItems.Add(s); // always added, even if ''
    s := Sec['SCR'];
    if s<>'' then
      Col4 := true;
    SubItems.Add(s); // always added, even if ''
    if s<>'' then begin
      s := Versions.SCR[s]['Request'];
      if s<>'' then
        Col5 := true;
    end;
    SubItems.Add(s); // always added, even if ''
  end;
end;

procedure TFramePages.CommitEnd(Commit: TListView);
begin
  Commit.OnCompare := OnCommitCompare;
  Commit.RowSelect := true;
  Commit.ViewStyle := vsReport;
  Commit.SortType := stData;
  Commit.OnColumnClick := OnCommitColumnClick;
  Commit.OnResize := OnCommitResize;
  Commit.Tag := 0; // List.Tag = item sort column
  Commit.Items.EndUpdate;
  Commit.AlphaSort;
  if not Col3 then
    Commit.Columns[2].Width := 0; // hide not used columns
  if not Col4 then
    Commit.Columns[3].Width := 0; // hide not used columns
  if not Col5 then
    Commit.Columns[4].Width := 0; // hide not used columns
  Commit.OnResize(Commit);
end;

procedure TFramePages.ListFilesCommitSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var List: TListView absolute Sender; // ListFilesCurrentFN
    i,j: integer;
    Item2: TListItem;
begin
  if csDestroying in ComponentState then exit;
  if not Sender.InheritsFrom(TListView) or (Item=nil) or not Selected then exit;
  if List.SelCount>2 then begin  // maximum 2 selected items
    for i := List.Items.Count-1 downto 0 do begin // unselect last item
      Item2 := List.Items[i];
      if Item2.Selected and (Item2<>Item) then begin
        List.Items[i].Selected := false;
        break;
      end;
    end;
  end;
  case List.SelCount of
  1: begin // 1 item select -> show file of this version
    Commit1 := integer(Item.Data);
    Commit2 := Commit1;
    if not FormViewOne.Visible then
      FormViewOne.Show;
    FormViewOne.Load(Versions,Versions.GetVersion(ListFilesCurrentFN,Commit1),
      Commit2); // update FormViewOne+PopupMenu
    List.PopupMenu := FormViewOne.PopupMenu;
  end;
  2: begin  // 2 items select -> show diff between the 2 versions
    for i := 0 to List.Items.Count-1 do begin // 2 items select -> show diff
      Item2 := List.Items[i];
      if Item2.Selected and (Item<>Item2) then begin
        Commit1 := integer(Item2.Data);
        Commit2 := integer(Item.Data);
        if Commit1>Commit2 then begin
          j := Commit1;
          Commit1 := Commit2;
          Commit2 := j;
        end;
        FormViewTwo.Load(Versions,Versions.GetVersion(ListFilesCurrentFN,Commit1),
          Versions.GetVersion(ListFilesCurrentFN,Commit2),false);
//        FormViewTwo.View2.MemoEx.SetFocus;
        break;
      end;
    end;
  end;
  end;
end;

procedure TFramePages.ListCommitFilesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin // Ctrl+File click -> compare with previous version, if any
 if ssCtrl in Shift then
   if FormViewOne.PopupMenu.Items.Count>0 then // first popup menu item is Compare from previous
     FormViewOne.CompareMenu(FormViewOne.PopupMenu.Items[0]);
 // MouseDown is triggered AFTER OnSelect -> easier to implement
end;

procedure ShowVersions(aVersions: TVersions);
var F: TForm;
    P: TFramePages;
begin
  if aVersions=nil then exit;
  // hide view forms before F.ShowModal, otherwize they remain inaccessible
  if FormViewOne.Visible then
    FormViewOne.Hide;
  if FormViewTwo.Visible then
    FormViewTwo.Hide;
  // display form
  F := TForm.Create(Application);
  try
    F.Caption := ' '+aVersions.ReleaseName+' - '+aVersions.FileName;
    F.Font.Name := 'Tahoma';
    F.Left := 0;
    F.Width := F.Monitor.Width div 2;
    F.Height := (F.Monitor.Height*2)div 3;
    F.Top := (F.Monitor.Height-F.Height)div 2;
    P := TFramePages.Create(F,aVersions);
    P.Parent := F;
    P.Align := alClient;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TFramePages.PagesChange(Sender: TObject);
begin
  if Pages.ActivePageIndex=1 then
    UpdateFiles;
end;

end.
