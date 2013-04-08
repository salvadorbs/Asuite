/// Risk assessment setting visual frame
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectFrameRisk;

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
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrameRisk = class(TFrame)
    GroupBoxRisk: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    LabeledEditEvaluatedBy: TLabeledEdit;
    LabeledEditJustif: TLabeledEdit;
    procedure LabeledEditEvaluatedByKeyPress(Sender: TObject; var Key: Char);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const RiskValue: string);
    function Risk: string;
  end;

implementation

{$R *.dfm}

uses
  ProjectTypes; // for RISKDEFS_ENGLISH[]

{ TFrameRisk }

constructor TFrameRisk.Create(AOwner: TComponent);
procedure One(const Risk: TRiskOneDef; L: TLabel; C: TComboBox);
begin
  with Risk do begin
    L.Caption := Name;
    C.Hint := Name+': '+Description;
    with C.Items do begin
      Clear;
      Add('0 - Not evaluated');
      Add('1 - '+Level[1]);
      Add('2 - '+Level[2]);
      Add('3 - '+Level[3]);
    end;
  end;
  C.ShowHint := true;
  C.ItemIndex := 0;
end;
begin
  inherited;
  One(RISKDEFS[0],Label1,ComboBox1);
  One(RISKDEFS[1],Label2,ComboBox2);
  One(RISKDEFS[2],Label3,ComboBox3);
end;

procedure TFrameRisk.Init(const RiskValue: string);
var R: TRisk;
begin
  fillchar(R,sizeof(R),0);
  R.FromString(RiskValue);
  LabeledEditEvaluatedBy.Text := R.EvaluatedBy;
  LabeledEditJustif.Text := R.Comment;
  ComboBox1.ItemIndex := R.Risk[0];
  ComboBox2.ItemIndex := R.Risk[1];
  ComboBox3.ItemIndex := R.Risk[2];
end;

function TFrameRisk.Risk: string;
begin
  result := format('%s,%s,%s,%s,%s',[
    ComboBox1.Text[1],ComboBox2.Text[1],ComboBox3.Text[1],
    trim(LabeledEditEvaluatedBy.Text),trim(LabeledEditJustif.Text)]);
  if result='0,0,0,,' then
    result := '';
end;

procedure TFrameRisk.LabeledEditEvaluatedByKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=',' then
   Key := '+';
end;

end.

