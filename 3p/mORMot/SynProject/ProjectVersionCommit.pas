/// File Versioning Commit form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectVersionCommit;

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
  Dialogs, StdCtrls, Buttons, CheckLst, ProjectCommons, ExtCtrls, ProjectVersioning;

type
  TProjectVersionCommitForm = class(TForm)
    CheckListBox: TCheckListBox;
    Label1: TLabel;
    BtnSelected: TBitBtn;
    BtnAll: TBitBtn;
    BtnCancel: TBitBtn;
    Description: TLabeledEdit;
    Label2: TLabel;
    Comments: TMemo;
    Label3: TLabel;
    SCR: TComboBox;
    PVCS: TCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnAllClick(Sender: TObject);
    procedure BtnSelectedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    Selected: TIntegerDynArray;
    procedure Init(const Title: string; Versions: TVersions);
  end;

var
  ProjectVersionCommitForm: TProjectVersionCommitForm;

implementation

uses
  ProjectVersionSCR, // for notVoid()
  ProjectSections;


{$R *.dfm}

procedure TProjectVersionCommitForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if byte(Shift)=0 then
  case Key of
  vk_F3: BtnSelected.Click;
  vk_F4: BtnAll.Click;
  end;
end;

procedure TProjectVersionCommitForm.BtnAllClick(Sender: TObject);
var i: integer;
begin
  SetLength(Selected,CheckListBox.Count);
  with CheckListBox.Items do
  for i := 0 to Count-1 do
    Selected[i] := integer(Objects[i]);
end;

procedure TProjectVersionCommitForm.BtnSelectedClick(Sender: TObject);
var n,i: integer;
begin
  SetLength(Selected,CheckListBox.Count);
  n := 0;
  with CheckListBox.Items do
  for i := 0 to Count-1 do
  if CheckListBox.Checked[i] then begin
    Selected[n] := integer(Objects[i]);
    inc(n);
  end;
  Setlength(Selected,n);
end;

procedure TProjectVersionCommitForm.FormShow(Sender: TObject);
begin
  Description.SetFocus;
end;

procedure TProjectVersionCommitForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not (ModalResult in [mrOK,mrAll]) then exit;
  if NotVoid(Description) then exit;
  CanClose := false;
end;

procedure TProjectVersionCommitForm.Init(const Title: string; Versions: TVersions);
var i, s, Value,Code: integer;
    sel, u, last: string;
    aSCR: TSectionsStorage;
    Sec: TSection;
begin
  Caption := ' '+Title;
  Description.Text := '';
  Comments.Clear;
  PVCS.Checked := isTrue(Versions.Params['CommitToPVCS']);
  sel := Versions.Params['LastUpdate']; // LastUpdate=012
  with CheckListBox.Items do begin
    Clear; // Update4=IFA Software;Synopse\IFA2;FilterDefault,*.sld,*.ias
    for i := 0 to 9 do begin
      u := Versions.Params['Update'+IntToStr(i)];
      if u='' then continue;
      AddObject(format('%s - %s',[ValAt(u,0,';'),ValAt(u,1,';')]),pointer(i));
      if pos(chr(i+48),sel)>0 then
        CheckListBox.Checked[Count-1] := true;
    end;
  end;
  with SCR.Items do begin
    Clear;
    aSCR := Versions.SCR;
    if aSCR<>nil then begin
      s := 0;
      last := Versions.Commits[Versions.Params['LastCommit']]['SCR'];
      AddObject('-=- None -=-',pointer(-1));
      for i := 0 to aSCR.Sections.Count-1 do begin
        Sec := aSCR.Sections[i];
        Val(Sec.SectionName,Value,Code);
        if Code<>0 then continue; // [section] must be numeric
        AddObject(format('%s - %s',[Sec.SectionName,Sec.ShortDescription('')]),
          pointer(Value));
        if Sec.SectionName=last then
          s := Count-1;
      end;
      SCR.ItemIndex := s;
    end;
  end;
end;

end.
