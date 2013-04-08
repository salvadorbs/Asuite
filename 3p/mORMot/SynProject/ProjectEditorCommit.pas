/// Commit parameters setting form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectEditorCommit;

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
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  /// Commit parameters setting form
  TProjectEditorCommitForm = class(TForm)
    DisplayName: TLabeledEdit;
    Path: TLabeledEdit;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    FilterDefault: TCheckBox;
    Filters: TLabeledEdit;
    BtnPath: TButton;
    procedure BtnPathClick(Sender: TObject);
  private
    FDefPath: string;
  public
  end;

function EditorCommitForm(const Title, DefPath, FilterDefault: string; var Value: string): boolean;


implementation

uses
  ProjectCommons, ProjectRTF, ProjectFormDocWizard, ProjectVersioning;

{$R *.dfm}


function EditorCommitForm(const Title, DefPath, FilterDefault: string; var Value: string): boolean;
// Value = 'Display Name;Path;Filter,Filter,..'
var F: TProjectEditorCommitForm;
    P: PAnsiChar;
    v: string;
begin
  F := TProjectEditorCommitForm.Create(Application);
  try
    F.FDefPath := IncludeTrailingPathDelimiter(SysUtils.UpperCase(trim(DefPath)));
    F.Caption := ' '+Title;
    P := pointer(Value);
    F.DisplayName.Text := GetNextItem(P,';');
    F.Path.Text := GetNextItem(P,';');
    F.FilterDefault.Caption := sCommitFilterDefault;
    F.FilterDefault.Hint := FilterDefault;
    v := P;
    F.FilterDefault.Checked := CSVDelete(v,'FilterDefault');
    F.Filters.Text := v;
    result := F.ShowModal=mrOk;
    if not result then
      exit;
    Value := trim(F.DisplayName.Text)+';'+trim(F.Path.Text)+';';
    v := trim(F.Filters.Text);
    if F.FilterDefault.Checked then
      if v='' then
        v := 'FilterDefault' else
        v := 'FilterDefault,'+v;
    Value := Value+v;
  finally
    F.Free;
  end;
end;

procedure TProjectEditorCommitForm.BtnPathClick(Sender: TObject);
var Dir: string;
begin
  Dir := Path.Text;
  if not DirectoryExists(Dir) and DirectoryExists(FDefPath+Dir) then
    Dir := FDefPath+Dir;
  if SelectDirectory(Handle,Path.EditLabel.Caption,Dir) then begin
    if IdemPChar(pointer(Dir),pointer(FDefPath)) then
      Delete(Dir,1,length(FDefPath));
    Path.Text := Dir;
  end;
end;

end.
