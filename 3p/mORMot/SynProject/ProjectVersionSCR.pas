/// File Versioning SCR Editor form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectVersionSCR;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, SynMemoEx,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ProjectFrameRisk;

type
  TProjectVersionSCRForm = class(TForm)
    Description: TLabeledEdit;
    ShortName: TLabeledEdit;
    Request: TLabeledEdit;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    Ident: TLabeledEdit;
    BtnTrackerImport: TBitBtn;
    procedure BtnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnTrackerImportClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FrameRisk: TFrameRisk;
    TrackNumber: integer;
    Risk,
    Body: string;
    procedure Init(const Title: string);
  end;

var
  ProjectVersionSCRForm: TProjectVersionSCRForm;

function NotVoid(Field: TComponent): boolean;

resourcestring
  SErrorFieldEmpty = 'This field must not be empty';


implementation

uses ProjectTrackerLogin;

{$R *.dfm}

procedure TProjectVersionSCRForm.Init(const Title: string);
begin
  Caption := ' '+Title;
  Description.Text := '';
  ShortName.Text := '';
  Request.Text := '';
  Ident.Text := '';
  TrackNumber := -1;
  Body := '';
  FrameRisk.Init('');
end;

function NotVoid(Field: TComponent): boolean;
begin
  if Field<>nil then
  if Field.InheritsFrom(TLabeledEdit) then begin
    result := TLabeledEdit(Field).Text<>'';
    if result then exit;
    MessageDlg(TLabeledEdit(Field).EditLabel.Caption+#13#13+
      SErrorFieldEmpty,mtError,[mbOk],0);
    TLabeledEdit(Field).SetFocus;
    exit;
  end else
  if Field.InheritsFrom(TMemoEx) then begin
    result := TMemoEx(Field).Lines.HasText;
    if result then exit;
    MessageDlg(SErrorFieldEmpty,mtError,[mbOk],0);
    TMemoEx(Field).SetFocus;
    exit;
  end;
  result := true;
end;

procedure TProjectVersionSCRForm.BtnOKClick(Sender: TObject);
begin
  if not TryStrToInt(Ident.Text,TrackNumber) then begin
    TrackNumber := -1;
    MessageDlg(Ident.EditLabel.Caption+' '+Ident.Hint,mtError,[mbOk],0);
    Ident.SetFocus;
  end else
  if (TrackNumber>=0) and notVoid(Description) and notVoid(Request) then begin
    Risk := FrameRisk.Risk;
    exit;
  end;
  ModalResult := mrNone;
end;


procedure TProjectVersionSCRForm.FormShow(Sender: TObject);
begin
  if TrackNumber<0 then
    Ident.SetFocus;
end;

procedure TProjectVersionSCRForm.FormCreate(Sender: TObject);
begin
  FrameRisk := TFrameRisk.Create(self);
  FrameRisk.Parent := self;
  FrameRisk.Left := Ident.Left;
  FrameRisk.Top := 216;
end;

procedure TProjectVersionSCRForm.BtnTrackerImportClick(Sender: TObject);
var F: TProjectTrackerLoginForm;
    aDescription, aRequest: string;
begin
  F := TrackerLogin(false);
  if F<>nil then
  try
    F.ShowSCRList;
    if (F.ShowModal=mrOk) and (F.SelectedID>0) then
    try
      Screen.Cursor := crHourGlass;
      SCRImportOne(F.Tracker,F.SelectedID,aDescription,aRequest,@Body);
      Ident.Text := IntToStr(F.SelectedID);
      Description.Text := aDescription;
      Request.Text := aRequest;
    finally
      Screen.Cursor := crDefault;
    end;
  finally
    DeleteTempForm(F);
  end;
end;

end.
