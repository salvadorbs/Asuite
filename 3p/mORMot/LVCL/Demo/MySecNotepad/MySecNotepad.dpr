program MySecNotepad;

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

uses
  Forms,
  LoginFrm in 'LoginFrm.pas' {LoginForm},
  MemoFrm in 'MemoFrm.pas' {MemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TMemoForm, MemoForm);
  Application.Run;
end.
