/// HunSpell spell checker
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectSpellCheck;

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
  Dialogs, SynMemoEx, StdCtrls, ProjectCommons, ProjectSections, ProjectParser;

type
  TPAnsiCharArray = packed array[0..(MaxLongint div SizeOf(PAnsiChar))-1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;
  THunHandle = type Pointer;
  THunGetListFunc = function (Handle: THunHandle; var List: PPAnsiCharArray;
      const Word: PAnsiChar): Integer; cdecl;

  {{ Simple Hunspell wrapper class
    - Hunspell is a spell checker and morphological analyzer library and program
    designed for languages with rich morphology and complex word compounding or
    character encoding }
  THunSpell = class
  protected
    fHunLib: HMODULE;
    fHunHandle: THunHandle;
    fDictionaryName: string;
    fPersoDic: TStringList;
    fPersoDicModified: boolean;
    fPersoDicFileName: TFileName;
    Hunspell_create: function (const affpath, dpath: PAnsiChar): THunHandle; cdecl;
    Hunspell_create_key: function (const affpath, dpath, key: PAnsiChar): THunHandle; cdecl;
    Hunspell_destroy: procedure (Handle: THunHandle); cdecl;
    Hunspell_spell: function (Handle: THunHandle; const Word: PAnsiChar): Integer; cdecl;
    Hunspell_get_dic_encoding: function (Handle: THunHandle): PAnsiChar; cdecl;
    Hunspell_suggest: THunGetListFunc;
    Hunspell_analyze: THunGetListFunc;
    Hunspell_stem: THunGetListFunc;
    Hunspell_add: function (Handle: THunHandle; const Word: PAnsiChar): Integer; cdecl;
    Hunspell_add_with_affix: function (Handle: THunHandle; const Word, Example: PAnsiChar): Integer; cdecl;
    Hunspell_remove: function (Handle: THunHandle; const Word: PAnsiChar): Integer; cdecl;
    Hunspell_free_list: procedure (Handle: THunHandle; var List: PPAnsiCharArray; Count: Integer); cdecl;
    function LoadEntryPoints: boolean;
    procedure CallListFunc(const Word: PAnsiChar; const Func: THunGetListFunc;
      Strings: TStrings);
  public
    /// load an embedded hunspell engine and dictionary
    // - e.g. call as THunSpell.Create('en_US')
    // - if no DictionaryName is supplied, 'en_US' is selected
    // - uncompress and load the hunspell.dll library
    // - uncompress and load DictionaryName.dic+DictionaryName.aff files
    constructor Create(DictionaryName: string='');
    /// release engine and memory
    destructor Destroy; override;
    /// true if the supplied word is correct
    function IsCorrect(const Word: AnsiString): boolean;
    /// fill the supplied Strings collection with suggestions for this mispelled word
    procedure Suggest(const Word: AnsiString; Strings: TStrings);
    /// fill the supplied Strings collection with the morphological analysis
    // of this known word
    procedure Analyze(const Word: AnsiString; Strings: TStrings);
    /// fill the supplied Strings collection with the stemming of this known word
    procedure Stem(const Word: AnsiString; Strings: TStrings);
    /// add a word to the custom dictionnary
    function AddCustom(const Word: AnsiString): boolean;
    /// update the personal dictionary if a new word was added
    procedure SaveCustomDicIfNecessary;
    /// the associated dictionnary name, e.g. 'en_US'
    property DictionaryName: string read fDictionaryName;
  end;

  /// Simple Hunspell speel-check form for our MemoEx editor
  // - the Hunspell library and the default en_US dictionary are embedded
  // as a .zip file into the main executable
  TSpellCheckForm = class(TForm)
    Label1: TLabel;
    Edit: TEdit;
    btnSkip: TButton;
    btnSkipAll: TButton;
    ListBox: TListBox;
    Label2: TLabel;
    btnAddDictionary: TButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure btnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    fHunSpell: THunSpell;
    ClickedButton: TObject;
  public
    /// launch the spell-check for the supplied MemoEx editor
    // - if a selection is first made, spell-check inside this selection
    // - if no selection is available, spell-check from the current cursor
    // position till the end of the document
    // - on any misspelled word, the corresponding word is selected in the editor,
    // and this form popup, in order to enable the user to correct this word
    // or add it to his/her personal dictionnary (.dic file in the executable folder)
    procedure SpellCheck(aMemo: TMemoEx);
    /// the associated Hunspell wrapper class and its dictionnary
    property HunSpell: THunSpell read fHunSpell;
  end;


var
  SpellCheckForm: TSpellCheckForm;



implementation

uses
  ProjectDiff,
  SynZip; // TZipRead to read embedded ProjectRes.zip

{$R *.dfm}

// {$R ProjectRes.RES} already done
// contains hunspell.dll and en_US.dic/.aff in the embedded ProjectRes.zip

{ THunSpell }

function THunSpell.AddCustom(const Word: AnsiString): boolean;
begin
  // warning: Hunspell_add() will modify the Word buffer!
  if (self=nil) or (fHunHandle=nil) or (Word='') then
    result := false else
    result := Hunspell_add(fHunHandle,pointer(Word))=0;
  if result then begin
    fPersoDic.Add(Word);
    fPersoDicModified := true;
  end;
end;

procedure THunSpell.Analyze(const Word: AnsiString; Strings: TStrings);
begin
  CallListFunc(pointer(Word),Hunspell_analyze,Strings);
end;

procedure THunSpell.CallListFunc(const Word: PAnsiChar;
  const Func: THunGetListFunc; Strings: TStrings);
var ListCount, i: integer;
    List: PPAnsiCharArray;
begin
  if (self=nil) or (fHunHandle=nil) or (Strings=nil) then
    exit;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    ListCount := Func(fHunHandle,List,Word);
    try
      for i := 0 to ListCount-1 do
        Strings.Add(List[i])
    finally
      Hunspell_free_list(fHunHandle, List, ListCount);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

constructor THunSpell.Create(DictionaryName: string='');
var Temp, HunSpell, Aff, Dic: TFileName;
    i: integer;
begin
  if DictionaryName='' then
    DictionaryName := 'en_US';
  Temp := GetSynopseCommonAppDataPath;
  HunSpell := Temp+'hunspell.dll';
  with TZipRead.Create(HInstance,'Zip','ZIP') do
  try
    Aff := DictionaryName+'.aff';
    if not FileExists(Temp+Aff) then
      StringToFile(Temp+Aff,UnZip(NameToIndex(Aff)));
    Dic := DictionaryName+'.dic';
    if not FileExists(Temp+Dic) then
      StringToFile(Temp+Dic,UnZip(NameToIndex(Dic)));
    if not FileExists(HunSpell) then
      StringToFile(HunSpell,UnZip(NameToIndex('hunspell.dll')));
  finally
    Free;
  end;
  fHunLib := SafeLoadLibrary(HunSpell);
  if fHunLib=0 then
    exit;
  if not LoadEntryPoints then begin
    FreeLibrary(fHunLib);
    fHunLib := 0;
    exit;
  end;
  fDictionaryName := DictionaryName;
  fHunHandle := Hunspell_create(pointer(Temp+Aff),pointer(Temp+Dic));
  if fHunHandle=nil then
    exit;
  fPersoDic := TStringList.Create;
  fPersoDic.Sorted := true; // will make .dic human review easier 
  fPersoDicFileName := ChangeFileExt(paramstr(0),'.dic');
  if FileExists(fPersoDicFileName) then begin
    fPersoDic.LoadFromFile(fPersoDicFileName);
    for i := 0 to fPersoDic.Count-1 do
      Hunspell_add(fHunHandle,pointer(fPersoDic[i]));
  end;
end;

destructor THunSpell.Destroy;
begin
  if fHunLib<>0 then begin
    if fHunHandle<>nil then begin
      Hunspell_destroy(fHunHandle);
      SaveCustomDicIfNecessary;
    end;
    FreeLibrary(fHunLib);
  end;
  fPersoDic.Free;
  inherited;
end;

function THunSpell.IsCorrect(const Word: AnsiString): boolean;
begin
  if (self=nil) or (fHunHandle=nil) then
    result := false else begin
    result := Hunspell_spell(fHunHandle,pointer(Word))<>0;
    if not result and (fPersoDic.IndexOf(Word)>=0) then
      result := true; // allow any case match for perso dictionnary
  end;
end;

function THunSpell.LoadEntryPoints: boolean;
begin
  Result := false;
  if fHunLib = 0 then exit;
  @Hunspell_create := GetProcAddress(fHunLib,'Hunspell_create');
  if not Assigned(Hunspell_create) then exit;
  @Hunspell_create_key := GetProcAddress(fHunLib,'Hunspell_create_key');
  if not Assigned(Hunspell_create_key) then exit;
  @Hunspell_destroy := GetProcAddress(fHunLib,'Hunspell_destroy');
  if not Assigned(Hunspell_destroy) then exit;
  @Hunspell_spell := GetProcAddress(fHunLib,'Hunspell_spell');
  if not Assigned(Hunspell_spell) then exit;
  @Hunspell_get_dic_encoding := GetProcAddress(fHunLib,'Hunspell_get_dic_encoding');
  if not Assigned(Hunspell_get_dic_encoding) then exit;
  @Hunspell_suggest := GetProcAddress(fHunLib,'Hunspell_suggest');
  if not Assigned(Hunspell_suggest) then exit;
  @Hunspell_analyze := GetProcAddress(fHunLib,'Hunspell_analyze');
  if not Assigned(Hunspell_analyze) then exit;
  @Hunspell_stem := GetProcAddress(fHunLib,'Hunspell_stem');
  if not Assigned(Hunspell_stem) then exit;
  @Hunspell_add := GetProcAddress(fHunLib,'Hunspell_add');
  if not Assigned(Hunspell_add) then exit;
  @Hunspell_add_with_affix := GetProcAddress(fHunLib,'Hunspell_add_with_affix');
  if not Assigned(Hunspell_add_with_affix) then exit;
  @Hunspell_remove := GetProcAddress(fHunLib,'Hunspell_remove');
  if not Assigned(Hunspell_remove) then exit;
  @Hunspell_free_list := GetProcAddress(fHunLib,'Hunspell_free_list');
  if Assigned(Hunspell_free_list) then
    result := true;
end;

procedure THunSpell.SaveCustomDicIfNecessary;
begin
  if fPersoDicModified then begin
    fPersoDic.SaveToFile(fPersoDicFileName);
    fPersoDicModified := false;
  end;
end;

procedure THunSpell.Stem(const Word: AnsiString; Strings: TStrings);
begin
  CallListFunc(pointer(Word),Hunspell_stem,Strings);
end;

procedure THunSpell.Suggest(const Word: AnsiString; Strings: TStrings);
begin
  CallListFunc(pointer(Word),Hunspell_suggest,Strings);
end;



{ TSpellCheckForm }

procedure TSpellCheckForm.FormCreate(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    fHunSpell := THunSpell.Create;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSpellCheckForm.FormDestroy(Sender: TObject);
begin
  fHunSpell.Free;
end;

procedure TSpellCheckForm.SpellCheck(aMemo: TMemoEx);
var IgnoredWords, ReplaceWords: TAnsiStrings;
procedure Check(Offset: integer; PBeg: PChar);
var Word, NewWord: string;
    PC: PChar;
    Beg: integer;
begin
  if PBeg=nil then
    exit;
  PC := PBeg;
  if Offset<=0 then
    Offset := 1;
  repeat
    repeat
      while not (NormToUpper[PC^] in ['A'..'Z','0'..'9']) do begin
        // go to next word begin
        if PC^=#10 then begin // handle special lines, cf. TProject.CreateRTFBody
          inc(PC);
          if (PWord(PC)^=ord('=')+ord('[')shl 8) or    // =[SoftwareHistory]
             (PWord(PC)^=ord('|')+ord('%')shl 8) then  // |%23...
            repeat inc(PC) until PC^<' ' else 
          if PC^='\' then begin // ignore \table or like
            if IdemPChar(PC,'\GRAPH') then begin 
              repeat // go to end of \graph block
                Word := GetNextLine(PC,PC);
                if PC=nil then exit;
              until (Word='') or (Word='\');
              dec(PC);
              continue;
            end else
              repeat inc(PC) until PC^<' '; // one \table line ignore
          end else
          if PC^='[' then begin
            repeat // whole ignore [Section] header
              Word := GetNextLine(PC,PC);
              if PC=nil then exit;
              // end of header: cf. TSectionsStorage.ReadOpen
            until (Word='') or (Word[1] in [':','=']) or
              not ((Word[1] in [';','[']) or (pos('=',Word) in [2..48]));
            dec(PC);
            continue;
          end else
          if PC^ in ['%',';','!','&','#','µ','$'] then
            // comment or %image.png or code -> ignore whole line
            repeat inc(PC) until PC^<' ' else
          dec(PC); // this was a "normal" line -> return to line beginning 
        end;
        if PC^=#0 then exit else inc(PC);
      end;
      if PC=PBeg then
        break else // avoid GPF
      if PWord(PC-2)^=ord('@')+ord('!')shl 8 then
          while (PC^>=' ') and (PC^<>'@') do inc(PC) else // ignore @!button@
      if PC[-1]='@' then begin
        if IdemPChar(PC,'HTTP://') then // reach @http://synopse.info end
          while PC^>=' ' do inc(PC) else
          while (PC^>=' ') and (PC^<>'@') do inc(PC); // ignore @button@
      end else
      if PC[-1]='\' then
        if PWord(PC)^=ord('f')+ord('1')shl 8 then
          while (PC^>=' ') and (PC^<>'}') do inc(PC) else // reach {\f1 ..} end
          while NormToUpper[PC^] in ['A'..'Z','0'..'9'] do inc(PC) // reach \rtf end
      else
        break;
      if PC^=#0 then exit;
    until false;
    Beg := PC-PBeg;
    while NormToUpper[PC^] in ['A'..'Z','0'..'9'] do inc(PC); // go to word end
    SetString(Word,PBeg+Beg,PC-PBeg-Beg);
    if not Hunspell.IsCorrect(Word) and
       not IgnoredWords.Find(Word) then begin
      aMemo.SelStart := Offset+Beg;
      aMemo.SelLength := length(Word);
      NewWord := ReplaceWords.FindValue(Word);
      if NewWord<>'' then begin
        inc(offset,length(NewWord)-length(Word));
        aMemo.SelText := NewWord; // replace All
        continue;
      end;
      Edit.Text := Word;
      Hunspell.Suggest(Word,ListBox.Items);
      ListBoxClick(nil);
      ShowModal;
      NewWord := trim(Edit.Text);
      if ClickedButton=btnAddDictionary then
        Hunspell.AddCustom(NewWord) else
      if ClickedButton=btnReplaceAll then begin
        inc(offset,length(NewWord)-length(Word));
        aMemo.SelText := NewWord;
        ReplaceWords.Add(Word+'='+NewWord);
      end else
      if ClickedButton=btnReplace then begin
        inc(offset,length(NewWord)-length(Word));
        aMemo.SelText := NewWord;
      end else
      if ClickedButton=btnSkipAll then
        IgnoredWords.Add(NewWord) else
      if ClickedButton=btnClose then
        break; // end spellcheck
    end;
  until PC=nil;
end;
var Text, Sel: string;
    offset: integer;
begin
  if (self=nil) or (fHunSpell.fHunHandle=nil) or (aMemo=nil) then
    exit; // avoid any GPF
  IgnoredWords.Init;
  ReplaceWords.Init;
  Sel := aMemo.SelText;
  if Sel<>'' then
    // spell-check inside this selection
    Check(aMemo.SelStart,pointer(Sel)) else begin
    // check until the end
    offset := aMemo.PosFromCaret(aMemo.CaretX,aMemo.CaretY)+2;
    Text := aMemo.Lines.Text;
    Sel := copy(Text,offset,maxInt);
    Check(offset,pointer(Sel));
  end;
  aMemo.SelLength := 0; // unshow selection to 
  HunSpell.SaveCustomDicIfNecessary;
end;

procedure TSpellCheckForm.ListBoxClick(Sender: TObject);
begin
  btnReplace.Enabled := ListBox.ItemIndex>=0;
  btnReplaceAll.Enabled := btnReplace.Enabled;
end;

procedure TSpellCheckForm.btnClick(Sender: TObject);
begin
  ClickedButton := Sender;
  if (Sender=btnReplace) or (Sender=btnReplaceAll) then
    if ListBox.ItemIndex>=0 then
      Edit.Text := ListBox.Items[ListBox.ItemIndex];
  Close;
end;

procedure TSpellCheckForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: btnClick(btnClose);
  end;
end;

end.
