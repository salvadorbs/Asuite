/// Documentation Editor main form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectEditMain;

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

{.$define AUTOBACKUPASZIP}
{ if defined, an automatic backup copy will be made in a .ZIP file for each day
  not to be used in global ProjectVersion (uses project differential backup) }

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SynMemoEx,
  ProjectCommons, ProjectEditor, ProjectTypes, ProjectSections, 
  StdCtrls, ExtCtrls, ComCtrls;

type
  TProMainForm = class(TForm)
    Sections: TListBox;
    Splitter2: TSplitter;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure SectionsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure WholeFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SectionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fData: TSectionsStorage;
    OldSectionIndex: integer;
    procedure DataToSections;
    procedure EditorsToData(const FocusSectionName: string);
    procedure SaveToFile(Sender: TObject);
  public
    Editor, Params: TFrameEditor;
    procedure InitEditor(const Title, ProFileName: string);
    procedure DataChangedInEditorSection(Sender: TObject; const newData: string);
    property Data: TSectionsStorage read FData;
  end;

var
  ProMainForm: TProMainForm;

resourcestring
  sWholeFile = 'Whole File';


  
implementation

{$R *.dfm}

uses
  ProjectDiff, // for Adler32Asm
{$ifdef AUTOBACKUPASZIP}
  ZipOnly,
{$endif}
  ProjectRTF, ProjectVersionMain;

procedure TProMainForm.DataToSections;
var i: integer;
begin
  if fData=nil then exit;
  with Sections.Items do begin
    BeginUpdate;
    Clear;
    Add('* '+sWholeFile);
    for i := 0 to fData.Sections.Count-1 do
      AddObject(fData.Sections[i].SectionName,fData.Sections[i]);
    EndUpdate;
  end;
end;

procedure TProMainForm.EditorsToData(const FocusSectionName: string);
var OldSec: TSection;
    paramsText, bodyText: string;
begin
  if fData=nil then exit;
  if OldSectionIndex>=0 then begin
    OldSec := TSection(Sections.Items.Objects[OldSectionIndex]);
    if OldSec=nil then begin // all Text
      if Editor.UpdateDataFromTextAllIfNecessary then
        DataToSections;
      if FocusSectionName<>'' then
        Sections.ItemIndex := Sections.Items.IndexOf(FocusSectionName);
    end else begin
      if Editor.Memo.Modified or Params.Memo.Modified then begin
        paramsText := trim(Params.Memo.Lines.Text);
        bodyText := trim(Editor.Memo.Lines.Text);
        if paramsText<>'' then begin // Params exists -> enforce separate from body
          if (bodyText<>'') and (bodyText[1]<>':') then
            paramsText := paramsText+#13#10;
        end else // paramsText='' -> put body after a CRLF if not title first
          if (bodyText<>'') and (bodyText[1]<>':') then
            paramsText := #13#10;
        OldSec.ReadSection(PChar(paramsText+bodyText));
        fData.Modified := true;
      end;
    end;
  end;
end;

procedure TProMainForm.FormCreate(Sender: TObject);
begin
  OldSectionIndex := -1;
  Editor := TFrameEditor.Create(Self);
  Editor.BtnSave.OnClick := SaveToFile;
  Editor.Name := 'Editor';
  Editor.Parent := Self;
  Editor.Align := alClient;
  Editor.Params := false;
  Editor.BtnTextAll.OnClick := WholeFileClick;
  Params := TFrameEditor.Create(Self);
  Params.Parent := Self;
  Params.Align := alRight;
  Params.Params := true;
  Params.Panel.Width := Max((ClientWidth -Sections.Width) div 3,300);
  Params.BtnTextAll.OnClick := WholeFileClick;
  Splitter1.Left := Params.Left;
  OnKeyDown := Editor.OnEscKeyDown; // Escape key will close form
end;

procedure TProMainForm.SectionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var index: integer;
    E: TFrameEditor;
    aText: string;
begin
  if Button=mbRight then begin
    index := Sections.ItemAtPos(Point(X,Y),true);
    if index<0 then exit;
    aText := fData[Sections.Items[index]].Lines.Text;
    if aText='' then exit;
    with CreateFormEditor(nil,E,false,
      ' ['+Sections.Items[index]+'] Section Read-Only View') do begin
      E.BtnWordWrap.Down := true;
      E.Memo.WordWrap := true;
      E.Memo.Lines.Text := aText;
      Width := Width-E.Sections.Width;
      E.Sections.Hide;
      E.ReadOnly := true;
      E.BtnReadOnly.Enabled := false;
      E.BtnSave.Enabled := false;
      E.BtnWizard.Enabled := false;
      OnClose := MainForm.FormClose; // Action := caFree
      Show; // show as Read-Only text
    end;
  end;
end;

procedure TProMainForm.SectionsClick(Sender: TObject);
var Sec: TSection;
    i, index: integer;
    SecName, s: string;
    M: TStrings;
begin
  if fData=nil then exit;
  index := Sections.ItemIndex;
  if index<0 then exit;
  if index=OldSectionIndex then exit;
  SecName := Sections.Items[index];
  EditorsToData(SecName); // if Modified in the Editors -> save to Data
  Sec := TSection(Sections.Items.Objects[index]);
  if Sec=nil then begin // first line: all Text
    Params.Memo.Lines.Clear;
    Params.Hide;
    Editor.OnDataChange := nil;
    Editor.Memo.Lines.Text := fData.Text;
    Editor.TextAll := true;
    if OldSectionIndex>=0 then begin
      Sec := TSection(Sections.Items.Objects[OldSectionIndex]);
      if Sec<>nil then begin
        i := Editor.Sections.Items.IndexOf(Sec.SectionName);
        if i>=0 then begin
          Editor.Sections.ItemIndex := i;
          Editor.SectionsClick(self); // Sender<>nil -> no search of MemoWordClickText
        end;
      end;
    end;
  end else begin // update Params+Editor with Sec content:
    Params.Memo.BeginUpdate; // M.BeginUpdate is not sufficient
    M := Params.Memo.Lines;
    M.Clear;
    with Sec.Lines do begin
      i := 0;
      while i<Count do begin
        s := Strings[i];
        if (s='') or (s[1]=':') then break;
        M.Add(s);
        inc(i);
      end;
    end;
    Params.Memo.EndUpdate;
    Params.Memo.Modified := false;
    Editor.OnDataChange := DataChangedInEditorSection;
    Editor.Memo.BeginUpdate;
    M := Editor.Memo.Lines;
    M.Clear;
    with Sec.Lines do
      while i<Count do begin
        s := Strings[i];
        if s<>'' then
          M.Add(s);
        inc(i);
      end;
    Editor.Memo.EndUpdate;
    Editor.Memo.Modified := false;
    Params.Show;
    if Params.Memo.Lines.Count=0 then
      Params.Width := 140 else
      if M.Count=0 then
        Params.Width := (ClientWidth-Sections.Width)-140 else
        Params.Width := (ClientWidth-Sections.Width)div 2;
      Editor.TextAll := false;
  end;
  OldSectionIndex := index;
end;

procedure TProMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if fData=nil then exit;
  EditorsToData(''); // if Modified in the Editors -> save to Data
  if fData.Modified then // if Data modified -> save to file
  case MessageDlg(SErrorDocModifiedAskSave,mtConfirmation,mbYesNoCancel,0) of
  mrYes:    SaveToFile(nil);
  mrCancel: CanClose := false;
  end;
end;

procedure TProMainForm.WholeFileClick(Sender: TObject);
begin
  Sections.ItemIndex := 0;
  SectionsClick(nil);
end;

procedure TProMainForm.FormShow(Sender: TObject);
begin
  Sections.ItemIndex := 0;
  SectionsClick(nil);
  Application.ProcessMessages;
  Editor.Memo.SetFocus;
end;

procedure TProMainForm.SaveToFile(Sender: TObject);
var Bak: string;
    W: TStringWriter;
    Adler32: cardinal;
{$ifdef AUTOBACKUPASZIP}
    SystemTime: TSystemTime;
    Zip: TZip;
    FileName: string;
{$endif}
begin
  if fData=nil then exit;
  EditorsToData(''); // if Modified in the Editors -> save to Data
  if not fData.Modified then exit;
  bak := ChangeFileExt(fData.FileName,'.~pro');
  fData.SaveText(W);
  Adler32 := Adler32Asm(0,W.DataPointer,W.len);
  if Adler32<>fData.Adler32AsCreated then begin // if something really changed :)
    fData.Adler32AsCreated := Adler32;
    DeleteFile(bak);
    RenameFile(fData.FileName,bak);
    W.SaveToFile(fData.FileName);
{$ifdef AUTOBACKUPASZIP}
    GetLocalTime(SystemTime);
    bak := ExtractFilePath(fData.FileName)+'Backup';
    if not DirectoryExists(bak) then
      mkDir(bak);
    FileName := ValAt(ExtractFileName(fData.FileName),0,'.');
    bak := format('%s\%s %d%.2d%.2d.zip',[bak,FileName,
      SystemTime.wYear,SystemTime.wMonth,SystemTime.wDay]);
    Zip := TZip.Create(bak);
    Zip.AddBuf(format('%s %.2dh%.2d%s',[ValAt(ExtractFileName(bak),0,'.'),
       SystemTime.wHour,SystemTime.wMinute,ExtractFileExt(fData.FileName)]),
      6,W.DataPointer,W.len);
    Zip.Free;
{$endif}
  end;
  fData.Modified := false;
  Editor.Memo.Modified := false;
  Params.Memo.Modified := false;
end;

procedure TProMainForm.DataChangedInEditorSection(Sender: TObject; const newData: string);
begin
  if fData=nil then exit;
  fData.LoadFromMemory(pointer(newData),length(newData));
  fData.Modified := true;
  DataToSections;
  Sections.ItemIndex := 0; // whole file edit
  SectionsClick(nil);
  if Sender<>nil then
    Editor.Memo.Command(ecEndDoc);
end;

procedure TProMainForm.InitEditor(const Title, ProFileName: string);
begin
  FreeAndNil(fData);
  fData := TSectionsStorage.Create(ProFileName); 
  Editor.Data := fData;
  Editor.Memo.RightMargin := StrToIntDef(
    fData.Section['Project'].Value['EditorRightMargin'],100);
  OldSectionIndex := -1; // force reload editor text
  Params.Data := fData;
  DataToSections;
  Caption := ' SynProject - '+Title;
end;

procedure TProMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fData);
end;

end.
