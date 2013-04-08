unit MemoFrm;

(*   My Secure NotePad
     =================
     version 1.0

  Very tiny (40KB using LVCL) secure notepad:
  it's a Safe/StrongBox/Coffer notepad and file cipher utility.
  Uses AES-256 and SHA-256 encryption from myCrypto library:
  it's top-secret enabled - cf. http://www.cnss.gov/Assets/pdf/cnssp_15_fs.pdf

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info

*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMemoForm = class(TForm)
    Memo: TMemo;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MemoForm: TMemoForm;

  
implementation

{$R *.dfm}

procedure TMemoForm.FormResize(Sender: TObject);
begin
  with ClientRect do // emulate Memo.Align := alClient
    Memo.SetBounds(Left,Top,Right-Left,Bottom-Top);
end;

procedure TMemoForm.FormShow(Sender: TObject);
begin
  Memo.SetFocus;
end;

procedure TMemoForm.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
  #1: Memo.SelectAll;
  #27: Close;
  end;
end;

end.
