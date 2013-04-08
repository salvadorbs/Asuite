/// File Versioning Backup form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectVersionBackup;

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
  Dialogs, StdCtrls, Buttons, CheckLst, ProjectCommons;

type
  TProjectVersionBackupForm = class(TForm)
    CheckListBox: TCheckListBox;
    Label1: TLabel;
    BtnSelected: TBitBtn;
    BtnAll: TBitBtn;
    BtnCancel: TBitBtn;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnAllClick(Sender: TObject);
    procedure BtnSelectedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    Selected: TIntegerDynArray;
  end;

var
  ProjectVersionBackupForm: TProjectVersionBackupForm;

implementation

{$R *.dfm}

procedure TProjectVersionBackupForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if byte(Shift)=0 then
  case  Key of
  vk_F3: BtnSelected.Click;
  vk_F4: BtnAll.Click;
  end;
end;

procedure TProjectVersionBackupForm.BtnAllClick(Sender: TObject);
var i: integer;
begin
  SetLength(Selected,CheckListBox.Count);
  with CheckListBox.Items do
  for i := 0 to Count-1 do
    Selected[i] := integer(Objects[i]);
end;

procedure TProjectVersionBackupForm.BtnSelectedClick(Sender: TObject);
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

procedure TProjectVersionBackupForm.FormShow(Sender: TObject);
begin
  BtnSelected.SetFocus;
end;

end.
