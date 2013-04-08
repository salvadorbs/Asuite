/// Documentation editor Version number wizard form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectEditorRelease;

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
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  ProjectSections;

type
  TProjectEditorReleaseForm = class(TForm)
    Description: TLabeledEdit;
    BtnCurrent: TBitBtn;
    BtnCancel: TBitBtn;
    Version: TLabeledEdit;
    Date: TLabeledEdit;
    BtnToday: TButton;
    BtnNew: TBitBtn;
    procedure BtnCurrentClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnTodayClick(Sender: TObject);
  private
    Doc: TSection;
  public
    procedure Init(aDoc: TSection);
  end;


resourcestring
  sNewReleaseForDocumentN = 'New Release for the "%s" document';
  sDocumentVersionNumberN = '"%s" document version number (1.01 e.g.):';
  sErrorRevisionNotInUserN = 'The revision %s is not in use - please enter an existing one';
  sErrorRevisionUsedN = 'The revision %s is already used - please enter a new value';

  
implementation

uses
  ProjectCommons,
  ProjectFormDocWizard,
  ProjectVersionSCR;

{
Revision=0.1
RevisionDescription=Initial Version
RevisionDate=October 1, 2008
}

{$R *.dfm}

procedure TProjectEditorReleaseForm.Init(aDoc: TSection);
var Title: string;
begin
  if self=nil then exit;
  Doc := aDoc;
  Title := Doc.DisplayName(nil);
  Caption := ' '+format(sNewReleaseForDocumentN,[Title]);
  Version.EditLabel.Caption := format(sDocumentVersionNumberN,[Title]);
  Version.Text := aDoc['Revision'];
  Description.Text := aDoc['RevisionDescription'];
  Date.Text := aDoc['RevisionDate'];
end;

procedure TProjectEditorReleaseForm.BtnCurrentClick(Sender: TObject);
var i, num, first: integer;
    s,up: string;
    oldDate: boolean;
begin
  ModalResult := mrNone;
  if not notVoid(Version) or not notVoid(Description) then exit;
  up := uppercase(trim(Version.Text));
  if Sender=BtnCurrent then begin
    if not SameText(Doc['Revision'],up) then begin // check is current
      MessageErrFmt(sErrorRevisionNotInUserN,up);
      Version.SetFocus;
      exit;
    end;
    Doc['RevisionDescription'] := Description.Text;
    Doc['RevisionDate'] := Date.Text;
  end else
  if Sender=BtnNew then begin
    first := -1;
    num := -1;
    for i := 0 to Doc.Lines.Count-1 do begin
      s := Doc.Lines[i];
      if (s='') or (s[1]=':') then break;
      if IdemPChar(pointer(s),'REVISION=') then begin
        if first<0 then
          first := i;
        if IdemPChar(@s[length('REVISION=')+1],pointer(up)) then begin
          num := i;
          break;
        end;
      end;
    end;
    if num>=0 then begin // check if unique
      MessageErrFmt(sErrorRevisionUsedN,up);
      Version.SetFocus;
      exit;
    end;
    if Doc['RevisionDate']='' then begin
      Doc['RevisionDate'] := trim(Date.Text); // update old version date
      oldDate := true;
    end else
      oldDate := false;
    if first<0 then begin // no Revision= yet -> just add new
      Doc['Revision'] := up;
      Doc['RevisionDescription'] := trim(Description.Text);
      // RevisionDate was '' -> already set
    end else begin // add before any existing Revision=
      if oldDate then
        Doc.Lines.Insert(first,'RevisionDate=') else
        Doc.Lines.Insert(first,'RevisionDate='+trim(Date.Text));
      Doc.Lines.Insert(first,'RevisionDescription='+trim(Description.Text));
      Doc.Lines.Insert(first,'Revision='+up);
    end;
  end;
  ModalResult := mrOk;
end;


procedure TProjectEditorReleaseForm.FormShow(Sender: TObject);
begin
  if Version.Text='' then
    Version.SetFocus;
end;

procedure TProjectEditorReleaseForm.BtnTodayClick(Sender: TObject);
var s: string;
begin
  DateTimeToString(s,'mmmm d, yyyy',now);
  Date.Text := s;
end;

end.
