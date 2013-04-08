/// PVCS Tracker connection login form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectTrackerLogin;

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
  Dialogs, StdCtrls, Buttons, ExtCtrls, ProjectTrkTool, ProjectCommons, ComCtrls,
  ProjectSections, CheckLst;

type
  TProjectTrackerLoginForm = class(TForm)
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    Pages: TPageControl;
    TabUser: TTabSheet;
    TabProject: TTabSheet;
    TabSCR: TTabSheet;
    UserName: TLabeledEdit;
    PassWord: TLabeledEdit;
    Label1: TLabel;
    Project: TListBox;
    Label2: TLabel;
    SCRList: TListBox;
    Label3: TLabel;
    TabSCRMulti: TTabSheet;
    Label4: TLabel;
    SCRListMulti: TCheckListBox;
    BtnSelAll: TButton;
    BtnSelNone: TButton;
    BtnSelOpen: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure SCRListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SCRListClick(Sender: TObject);
    procedure BtnSelAllClick(Sender: TObject);
    procedure SCRListMultiClickCheck(Sender: TObject);
  private
    OriginalProjectIndex: integer;
  public
    Tracker: TPVCSTracker;
    SelectedID: integer;
    procedure ShowLogin;
    procedure ShowSCRList;
    procedure ShowSCRListMulti(const Title: string);
  end;

var
  ProjectTrackerLoginForm: TProjectTrackerLoginForm = nil;

function TrackerLogin(forceLog: boolean): TProjectTrackerLoginForm;
procedure DeleteTempForm(F: TForm); // call after TrackerLogin()

procedure SCRImportOne(Tracker: TPVCSTracker; n: integer;
  out Description, Request: string; Body: PString=nil);

// show TProjectTrackerLoginForm to select entries to import
function SCRImport(SCR: TSectionsStorage; forceLog: boolean=false): boolean;


resourcestring
  sSelectEntriesToImport = 'Select entries to import';
  sLocalSCRN = 'BRS SCR #%d';

implementation

uses
  ProjectRTF;

{$R *.dfm}

procedure DeleteTempForm(F: TForm);
begin
  if F<>ProjectTrackerLoginForm then
    F.Free; // delete temp form created if ProjectTrackerLoginForm not available
end;

function TrackerLogin(forceLog: boolean): TProjectTrackerLoginForm;
begin
  if ProjectTrackerLoginForm=nil then // create temp form if not available
    result := TProjectTrackerLoginForm.Create(Application) else begin
    result := ProjectTrackerLoginForm;
    if not forceLog and (result.Tracker<>nil) and result.Tracker.Logged then
      exit;
  end;
  try
    result.ShowLogin;
    if result.ShowModal=mrOk then
      assert((result.Tracker<>nil)and(result.Tracker.Logged)) else begin
      DeleteTempForm(result);
      result := nil; // return nil of error login
    end;
  except
    on TPVCSTrackerException do begin
//      if result.Visible then result.Hide;
      DeleteTempForm(result);
      result := nil;
    end;
  end;
end;

function StringImport(const s: string): string;
var j: integer;
begin
  result := s;
  j := 1;
  while j<length(result) do begin
    case result[j] of
    '\': begin insert('\',result,j); inc(j); end;
    '[': result[j] := '(';
    ']': result[j] := ')';
    #10: case result[j+1] of
     ':','$',';','&': result[j] := ' ';
    end;
    end;
    inc(j);
  end;
  result := trim(result);
end;

procedure SCRImportOne(Tracker: TPVCSTracker; n: integer;
  out Description, Request: string; Body: PString=nil);
var WR: TStringWriter;
begin
  Tracker.RecordOpen(n);
  Description := StringImport(Tracker.FieldNameString['Title']);
  Request := Tracker.FieldNameString['CSDBase'];
  if Request='' then
    Request := format(sLocalSCRN,[n]);
  if Body=nil then
    exit; // creates body (=description+notes) on demand
  WR.Init.AddShort(':Description'#13#10'Submitted by ').
    Add(Tracker.FieldNameString['Submitter']).AddShort(' on ').
    Add(Tracker.FieldNameString['Submit Date']).AddCRLF.
    Add(StringImport(Tracker.RecordDescription)).AddCRLF;
  while Tracker.NextNote do begin // auto init if first time for this record
    WR.Add(':').RtfBackSlash(Tracker.NoteTitle).AddCRLF.
       Add(Tracker.NoteAuthor).AddShort(', on ').
       Add(DateTimeToStr(Tracker.NoteTime)).AddCRLF.
       Add(StringImport(Tracker.NoteData)).AddCRLF;
  end;
  Body^ := WR.Data;
end;

function SCRImport(SCR: TSectionsStorage; forceLog: boolean): boolean;
var F: TProjectTrackerLoginForm;
    i, n: integer;
    num: string;
    aDescription, aRequest, aBody: string;
begin
  result := false;
  if SCR=nil then exit;
  F := TrackerLogin(forceLog);
  if F<>nil then
  try
    F.ShowSCRListMulti(sSelectEntriesToImport);
    if F.ShowModal=mrOk then
    try
      Screen.Cursor := crHourGlass;
      for i := 0 to F.SCRListMulti.Count-1 do
      if F.SCRListMulti.Checked[i] then begin
        // 1. get checked item
        n := integer(F.SCRListMulti.Items.Objects[i]);
        num := IntToStr(n);
        if SCR[num]<>nil then
          exit; // don't add if already there
        // 2. update SCR with PVCS values
        SCRImportOne(F.Tracker,n,aDescription,aRequest,@aBody);
        with SCR.GetOrCreateSection(num,true) do begin
          Value['Request'] := aRequest;
          Value['ShortName'] := '';
          Value['Description'] := aDescription;
        end;
        SCR.WriteBody(num,aBody);
      end;
      result := true;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    DeleteTempForm(F);
  end;
end;

procedure TProjectTrackerLoginForm.FormShow(Sender: TObject);
begin
  SelectedID := 0;
  if Tracker=nil then
  try
    Tracker := TPVCSTracker.Create;
  except
    on TPVCSTrackerException do
      FreeAndNil(Tracker);
  end;
end;

procedure TProjectTrackerLoginForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Tracker);
end;

procedure TProjectTrackerLoginForm.BtnOkClick(Sender: TObject);
var i: integer;
begin
  try
  try
    Screen.Cursor := crHourGlass;
    if Tracker<>nil then
    case Pages.ActivePageIndex of
    0: begin
      Tracker.Login(UserName.Text,PassWord.Text);
      if Tracker.Logged then begin
        Pages.ActivePageIndex := 1;
        OriginalProjectIndex := Tracker.GetProjects(Project.Items);
        Project.ItemIndex := OriginalProjectIndex;
      end;
    end;
    1: begin
      i := Project.ItemIndex;
      if i>=0 then begin
        if i<>OriginalProjectIndex then // relog only if necessary
          Tracker.Login(UserName.Text,PassWord.Text,Project.Items[i]);
        if Tracker.Logged then
           exit; // leave ModalResult=mrOk
      end;
    end;
    2: begin
      SelectedID := SCRList.ItemIndex;
      if SelectedID>=0 then begin
        SelectedID := integer(SCRList.Items.Objects[SelectedID]);
        exit; // leave ModalResult=mrOk
      end;
    end;
    3: exit; // BtnOk.enabled=true only if something was checked -> direct exit
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  except
    on E: TPVCSTrackerException do
      ShowMessage(E.Message); // just show error message from Tracker
  end;
  ModalResult := mrNone; // don't close form
end;

procedure TProjectTrackerLoginForm.ShowSCRList;
begin
  BtnOK.Enabled := false;
  Pages.ActivePageIndex := 2;
  if (Tracker=nil) or not Tracker.Logged then
    exit;
  BtnOK.Caption := ValAt(BtnOK.Hint,0); // Hint='OK,Connect'
  Tracker.FillQuery(SCRList.Items);
end;

procedure TProjectTrackerLoginForm.ShowLogin;
begin
  Pages.ActivePageIndex := 0;
  BtnOK.Enabled := true;
  BtnOK.Caption := ValAt(BtnOK.Hint,1); // Hint='OK,Connect'
  Label3.Caption := Caption+' '+BtnOK.Caption;
end;

procedure TProjectTrackerLoginForm.SCRListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var List: TCustomListBox absolute Control;
begin
  with List.Canvas do begin
    FillRect(Rect);
    Font.Style := [fsBold];
    TextOut(Rect.Left+4,Rect.Top,'#'+IntToStr(integer(List.Items.Objects[Index])));
    Font.Style := [];
    TextOut(Rect.Left+40,Rect.Top,List.Items[Index]);
  end;
end;

procedure TProjectTrackerLoginForm.SCRListClick(Sender: TObject);
begin
  BtnOk.Enabled := SCRList.ItemIndex>=0;
end;

procedure TProjectTrackerLoginForm.ShowSCRListMulti(const Title: string);
begin
  Label4.Caption := Title;
  BtnOK.Enabled := false;
  Pages.ActivePageIndex := 3;
  if (Tracker=nil) or not Tracker.Logged then
    exit;
  BtnOK.Caption := ValAt(BtnOK.Hint,0); // Hint='OK,Connect'
  Tracker.FillQuery(SCRListMulti.Items);
end;

procedure TProjectTrackerLoginForm.BtnSelAllClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to SCRListMulti.Count-1 do
    if Sender=BtnSelAll then
      SCRListMulti.Checked[i] := true else
    if Sender=BtnSelNone then
      SCRListMulti.Checked[i] := false else
      SCRListMulti.Checked[i] := pos('(Open)',SCRListMulti.Items[i])>0;
  SCRListMultiClickCheck(nil);
end;

procedure TProjectTrackerLoginForm.SCRListMultiClickCheck(Sender: TObject);
var i: integer;
begin
  for i := 0 to SCRListMulti.Count-1 do
    if SCRListMulti.Checked[i] then begin
      BtnOk.Enabled := true;
      exit;
    end;
  BtnOk.Enabled := false;
end;

end.
