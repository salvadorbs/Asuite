unit LoginFrm;

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
  This work is Copyright (C) 2008 Arnaud Bouchez - http://synopse.info

*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynCrypto;

type
  PMsg = ^TMsg;
  TMsg = array[0..12] of string;
  TLoginForm = class(TForm)
    L: TLabel;
    Pass: TEdit;
    Img: TImage;
    OK: TButton;
    Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Help: TButton;
    Label3: TLabel;
    Donate: TButton;
    procedure FormShow(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure PassKeyPress(Sender: TObject; var Key: Char);
    procedure HelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DonateClick(Sender: TObject);
  private
    { Déclarations privées }
    MSG: PMsg;
    procedure Add(const Filename: string);
    function MSGP(i: integer): PChar;
  public
    { Déclarations publiques }
    Files: TStringList;
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
  end;

var
  LoginForm: TLoginForm;

implementation

uses MemoFrm, ShellApi;

{$R *.dfm}

{$R WindowsXP.res} // copy it from the Delphi source directory

procedure TLoginForm.FormShow(Sender: TObject);
const
  MsgUS: TMsg =
  ('Notepad mode','Files to do: ','Enter Password:','Cancel',
   'Strong encryption with SHA-AES 256 bits', ' My Secure Notepad',
   'Wrong password for'#13'"%s"'#13#13' Please try again.',
   'Text has changed.'#13#13'Save the new version?',
   'The files are now uncrypted.'#13'You can work with them.'#13#13+
   'Do you want to crypt it again?'#13+
   'Choose Yes to cypher it or No to leave it clear.',
   'The files are now crypted.',
   #13#13'Keep your personnal data away from bad eyes '+
  'with strong encryption (industry security standard SHA-AES with 256 bits key)...'#13#13+
  'Use it simply to edit a secure Notepad File, or drag & drop some files '+
  'to cypher/uncypher them.'#13'You can also set files from the command line.'#13+
  'On Via C3/C7 proc, will use fast LibPadlock.dll if available.'#13#13+
  'This program is free and comes with NO WARRANTY.'#13+
  'If you use it, please consider making a small donation to me.'#13#13+
  '©2008-2013 Arnaud Bouchez - htp://synopse.info',
  'Is this program useful for you?'#13'Please consider making a small donation!',
  'US&item_name=Donate%20for%20My%20Secure%20Notepad'
   );
  MsgFR: TMsg =
  ('Mode Bloc-notes','Fichiers à traiter: ','Mot de passe:','Annuler',
   'Chiffrement fort en SHA-AES 256 bits', ' Mon Bloc-notes chiffré',
   'Mot de passe incorrect pour '#13'"%s"'#13#13' Essayez de nouveau.',
   'Le texte a été modifié.'#13#13'Sauver la nouvelle version?',
   'Les fichiers sont maintenant déchiffrés.'#13'Vous pouvez travailler avec.'#13#13+
   'Voulez-vous les chiffrer de nouveau?'#13+
   'Choisissez Oui pour les encoder, ou Non pour les laisser en clair.',
   'Les fichiers sont maintenant chiffrés.',
   #13#13'Garder vos données personnelles à l''écart des yeux indélicats '+
  'grâce au chiffrement fort (selon le standard SHA-AES à clef 256 bits)...'#13#13+
  'A utiliser comme un bloc-note sécurisé, ou bien en glissant/déplaçant '+
  'des fichiers ou répertoires pour les coder/décoder.'#13+
  'Vous pouvez aussi spécifier un fichier en ligne de commande.'#13+
  'Sur les Via C3/C7, les instructions rapides Padlock seront utilisées.'#13#13+
  'Ce programme est libre, d''utilisation gratuite, et SANS GARANTIE.'#13+
  'Si vous l''utilisez, merci de penser à faire un petit don d''encouragement.'#13#13+
  '©2008-2013 Arnaud Bouchez - htp://synopse.info',
  'Utilisez-vous ce programme?'#13'Merci de faire un petit don d''encouragement!',
  'FR&item_name=Don%20pour%20Mon%20Bloc-notes%20chiffre'
   );
var Loc: cardinal;
begin
  Loc := GetThreadLocale and $3ff;  // = SysLocale.PriLangID
  case Loc of
    LANG_FRENCH: MSG := @MsgFR;
    else MSG := @MsgUS;
  end;
  Pass.SetFocus;
  if Files.Count=0 then
    Label3.Caption := MSG[0] else
    Label3.Caption := MSG[1]+IntToStr(Files.Count);
  L.Caption := MSG[2];
  Cancel.Caption := MSG[3];
  Label2.Caption := MSG[4];
  Caption := MSG[5];
  Label1.Caption := Caption+Label1.Caption;
end;

procedure TLoginForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TLoginForm.OKClick(Sender: TObject);
var PassW, FileName, Data, NewData, FilePath: string;
    i,j, CryptFiles,TotalFiles: integer;
    Digest: TSHA256Digest;
    F: TFileStream;
    A: TAESFull;
    FT: TFileTime;
procedure NewFile;
begin
  if FileExists(FileName) then begin
    F := TFileStream.Create(FileName,fmOpenWrite);
    F.Size := 0;
  end else
    F := TFileStream.Create(FileName,fmCreate);
end;
procedure DoFile(Encrypt: boolean);
begin
  A.outStreamCreated := nil;
  F := TFileStream.Create(FileName,fmOpenRead);
  try
    GetFileTime(F.Handle,nil,nil,@FT);
    if A.EncodeDecode(Digest,256,F.Size,Encrypt,F,nil,nil,nil)<0 then
      MessageBox(Handle,PChar(Format(Msg[6],[FileName])),MsgP(4),0);
  finally
    F.Free;
  end;
end;
procedure WriteFile;
begin
  NewFile;
  with A.outStreamCreated do begin
    F.Write(Memory^,Size);
    Free;
  end;
  SetFileTime(F.Handle,nil,nil,@FT);
  F.Free;
end;
begin
  PassW := trim(Pass.Text);
  if PassW='' then exit;
  Hide;
  SHA256Weak(PassW,Digest);
  if Files.Count=0 then begin // Notepad version:
    FileName := ChangeFileExt(paramstr(0),'.crypt');
    if FileExists(FileName) then begin
      DoFile(false);
      if A.outStreamCreated=nil then begin
        Close;
        exit;
      end else
      with A.outStreamCreated do begin
        SetString(Data,PChar(Memory),Size);
        Free;
      end;
    end;
    with MemoForm do begin
      Memo.Text := Data;
      Caption := Label1.Caption;
      ShowModal;
      NewData := trim(Memo.Text);
      if (NewData<>Data) and (MessageBox(Handle,MSGP(7),MSGP(5),
        MB_ICONQUESTION or MB_YESNO)=IDYES) then begin
        NewFile;
        try
          A.EncodeDecode(Digest,256,length(NewData),true,nil,F,PChar(NewData),nil);
        finally
          F.Free;
        end;
      end;
    end;
  end else begin // multiple files:
    CryptFiles := 0;
    TotalFiles := Files.Count;
    for i := 0 to TotalFiles-1 do begin
      FileName := Files[i];
      if not FileExists(FileName) then begin
        dec(TotalFiles);
        Files[i] := '';
        continue;
      end;
      j := pos('.crypt',FileName);
      if j>0 then begin
        DoFile(false);
        if A.outStreamCreated=nil then begin
          dec(TotalFiles);
          Files[i] := '';
          continue;
        end;
        inc(CryptFiles);
        DeleteFile(FileName);
        SetLength(FileName,j-1);
        FilePath := extractFilePath(FileName);
        FileName := extractFileName(FileName);
        while FileExists(FilePath+FileName) do
          FileName := '~~'+FileName;
        FileName := FilePath+FileName;
        Files[i] := FileName;
        WriteFile;
      end;
    end;
    if TotalFiles>0 then
    if (CryptFiles=0) or (MessageBox(Handle,MSGP(8),
      MSGP(5),MB_ICONQUESTION or MB_YESNO)=IDYES) then begin
      for i := 0 to Files.Count-1 do begin
        FileName := Files[i];
        if (FileName='') or not FileExists(FileName) then continue;
        DoFile(true);
        assert(A.outStreamCreated<>nil);
        DeleteFile(FileName);
        repeat
          j := pos('~~',FileName);
          if j=0 then break else delete(FileName,j,2);
        until false;
        FileName := FileName+'.crypt';
        Writefile;
      end;
      if CryptFiles=0 then
        MessageBox(Handle,MSGP(9),MSGP(5),MB_ICONINFORMATION);
    end;
    Files.Clear;
  end;
  Close; // quit
end;

procedure TLoginForm.PassKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
  #13: OKClick(Sender);
  #27: Close;
  end;
end;

procedure TLoginForm.HelpClick(Sender: TObject);
begin
  MessageBox(Handle,PChar(Caption+MSG[10]),MSGP(5),MB_ICONINFORMATION);
end;

procedure TLoginForm.AcceptFiles(var msg: TMessage);
var i, nCount: integer;
    acFileName: array [0..255] of char;
begin
  nCount := DragQueryFile(msg.WParam, $FFFFFFFF, acFileName, 255 );
  for i := 0 to nCount-1 do begin
    DragQueryFile(msg.WParam, i, acFileName, 255);
    Add(acFileName);
  end;
  DragFinish(msg.WParam);
  FormShow(nil);
end;

procedure TLoginForm.FormCreate(Sender: TObject);
var i: integer;
begin
  Files := TStringList.Create;
  HandleNeeded;
  DragAcceptFiles(Handle, True); // accepting drag and drop files
  for i := 1 to paramcount do
     Files.Add(paramstr(i));
end;

procedure TLoginForm.FormDestroy(Sender: TObject);
begin
  Files.Free;
end;

procedure TLoginForm.Add(const Filename: string);
var SR: TSearchRec;
begin
  if DirectoryExists(FileName) then begin
    if FindFirst(FileName+'\*.*',faAnyFile,SR)<>0 then exit;
    repeat
      if (SR.Name<>'.') and (SR.Name<>'..') then
        Add(FileName+'\'+SR.Name);
    until FindNext(SR)<>0;
    FindClose(SR);
  end else
  if Files.IndexOf(FileName)<0 then
    Files.Add(FileName);
end;

procedure TLoginForm.DonateClick(Sender: TObject);
begin
  if MessageBox(Handle,MSGP(11),MSGP(5),MB_YESNO or MB_ICONQUESTION)=IDYES then
    ShellExecute(0,nil,PChar('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&'+
    'business=arnaud%40bouchez%2einfo&bn=PP%2dDonationsBF&no_shipping=0&'+
    'no_note=1&tax=0&currency_code=EUR&lc='+MSG[12]),nil,nil,SW_SHOWNORMAL);
end;

function TLoginForm.MSGP(i: integer): PChar;
begin
  result := PChar(MSG[i]);
end;

end.
