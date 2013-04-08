/// Link to Program visual form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.7
unit ProjectEditorProgram;

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
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Contnrs,
  ProjectCommons, ProjectTypes, ProjectSections,
  ProjectParser, PasDoc_Light, PasDoc_Items;

type
  /// Link to Program visual form
  TProjectEditorProgramForm = class(TForm)
    Panel1: TPanel;
    cbUnitName: TComboBox;
    lUnitName: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    pagTypes: TPageControl;
    PanelRight: TPanel;
    lstValues: TListBox;
    l1: TLabel;
    btnDeleteItem: TButton;
    btnClearList: TButton;
    cbSADSection: TComboBox;
    lSADSection: TLabel;
    leUnitDescription: TLabeledEdit;
    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnClearListClick(Sender: TObject);
    procedure cbSADSectionChange(Sender: TObject);
    procedure cbUnitNameChange(Sender: TObject);
    procedure lstValuesClick(Sender: TObject);
    procedure leUnitDescriptionExit(Sender: TObject);
  private
    SAD: TProjectBrowser;
    saeStorage: TStringList;
    Project: TProject;
    lists: array of TListBox;
    procedure lstObjectsClick(Sender: TObject);
    procedure lstObjectsDblClick(Sender: TObject);
    procedure SaeDescrExit(Sender: TObject);
    function GetSaeStorage(const OutputFileName: string): TSectionsStorage;
  public
  end;

/// display the "Link to Program" form and edit the supplied @button content@
// - return TRUE if the content was modified
function EditProgramForm(aProject: TProject; var ButtonContent: string): boolean;

/// display the Object browser form and edit the description for storage in the
// .sae external file
procedure EditSAEForm(aProject: TProject);


implementation

uses
  SynZipFiles;

{$R *.dfm}

resourcestring
  sErrorSaeFileN = 'File %s doesn''t exist yet.'#13#10#13#10+
    'Please launch first "Create external .sae file" menu item.';

procedure EditSAEForm(aProject: TProject);
var i,j: integer;
    Source, Merge: TSectionsStorage;
    Merged: TStringList;
    txt: string;
begin
  if aProject<>nil then
  with TProjectEditorProgramForm.Create(Application) do
  try
    Screen.Cursor := crHourGlass;
    Project := aProject;
    SAD := TProjectBrowser.Create(aProject);
    try
      if not FileExists(SAD.CacheNameSAE) then begin
        MessageDlg(format(sErrorSaeFileN,[SAD.CacheNameSAE]),mtError,[mbOk],0);
        exit;
      end;
      Screen.Cursor := crDefault;
      for i := 0 to high(Project.Parse) do
      with Project.Parse[i] do // edit external .sae only
      if isTrue(Value['ExternalDescription']) then
        cbSADSection.Items.AddObject(SectionName,Project.Parse[i]);
      saeStorage := TStringList.Create;
      if cbSADSection.Items.Count=0 then
        exit; // no valid .sae content
      PanelRight.Hide;
      btnOk.Enabled := false;
      if (ShowModal<>mrOk) or (saeStorage.Count=0) then
        exit;
      SAD.CloseCacheSAE(true,true);
      with TZip.Create(SAD.CacheNameSAE) do
      try
        Merged := TStringList.Create;
        try
          for i := 0 to Reader.Count-1 do begin
            j := saeStorage.IndexOf(Reader.Entry[i].ZipName);
            if j<0 then
              continue;
            Source := TSectionsStorage.Create;
            Source.LoadFromMemory(pointer(Reader.GetString(i)));
            Merge := TSectionsStorage(saeStorage.Objects[j]);
            Source.MergeFrom(Merge);
            Merged.AddObject(Reader.Entry[i].ZipName,Source);
            MarkDeleted(i); // must be done before any BeginWriter
          end;
          for i := 0 to Merged.Count-1 do begin
            txt := TSectionsStorage(Merged.Objects[i]).Text;
            AddBuf(Merged[i],6,pointer(txt),length(txt)); // do BeginWriter
          end;
        finally
          for i := 0 to Merged.Count-1 do
            Merged.Objects[i].Free;
          Merged.Free;
        end;
      finally
        Free;
      end;
    finally
      if saeStorage<>nil then begin
        for i := 0 to saeStorage.Count-1 do
          saeStorage.Objects[i].Free; // TSectionsStorage.Free
        saeStorage.Free;
      end;
      SAD.Free;
    end;
  finally
    Screen.Cursor := crDefault;
    Free;
  end;
end;

function EditProgramForm(aProject: TProject; var ButtonContent: string): boolean;
var i: integer;
    V,U: string;
    P,PV: PChar;
begin
  result := false;
  if aProject<>nil then
  with TProjectEditorProgramForm.Create(Application) do
  try
    Screen.Cursor := crHourGlass;
    Project := aProject;
    SAD := TProjectBrowser.Create(aProject);
    try
      P := pointer(ButtonContent);
      // 1. fill lstValues
      if (P<>nil) and (P^='!') then begin
        inc(P);
        V := GetNextItem(P,'!');
        PV := pointer(V);
        while PV<>nil do
          lstValues.Items.Add(GetNextItem(PV));
        V := GetNextItem(P,'\'); // 'PC\EIA\Test.pas' -> V='PC'
      end;
      // 2. fill cbSADSection
      for i := 0 to high(Project.Parse) do
      with Project.Parse[i] do begin
        cbSADSection.Items.AddObject(SectionName,Project.Parse[i]);
        if SameText(SectionNameValue,V) then begin
          SAD.FillUnits(Project.Parse[i],false);
          cbSADSection.ItemIndex := i;
        end;
      end;
      V := ExtractFileName(P);        // V = 'mORMot.pas'
      if cbSADSection.ItemIndex<0 then begin
        U := copy(V,1,length(V)-4);   // U = 'mORMot'
        // no section defined -> may be 'D:\Dev\Lib\SynPdf.pas' -> search by unit
        for i := 0 to high(Project.Parse) do begin
          SAD.FillUnits(Project.Parse[i],false);
          if SAD.Units.FindName(U)<>nil then begin
            cbSADSection.ItemIndex := i;
            break;
          end;
        end;
      end;
      // 3. fill cbUnitName
      for i := 0 to SAD.Units.Count-1 do
        with TPasUnit(SAD.Units[i]) do
          if isUnit then
            cbUnitName.Items.AddObject(Name+'.pas',SAD.Units[i]);
      i := cbUnitName.Items.IndexOf(V);
      if i>=0 then begin
        cbUnitName.ItemIndex := i;
        cbUnitNameChange(nil);
      end;
      // 4. display form, update button content if "Save" was clicked
      Screen.Cursor := crDefault;
      if ShowModal=mrOk then begin
        i := cbUnitName.ItemIndex;
        if i>=0 then begin
          if lstValues.Items.Count>0 then
            ButtonContent := '!'+lstValues.Items.CommaText else
            ButtonContent := '';
          ButtonContent := ButtonContent+'!'+
            TPasUnit(cbUnitName.Items.Objects[i]).OutputFileName;
          result := true; // mack ButtonContent was updated/saved
        end;
      end;
    finally
      SAD.Free;
    end;
  finally
    Project.Free;
    Screen.Cursor := crDefault;
    Free;
  end;
end;

procedure TProjectEditorProgramForm.btnDeleteItemClick(Sender: TObject);
var i: integer;
begin
  i := lstValues.ItemIndex;
  if i>=0 then
    lstValues.Items.Delete(i);
end;

procedure TProjectEditorProgramForm.btnClearListClick(Sender: TObject);
begin
  if Visible then
    lstValues.Items.Clear;
end;

procedure TProjectEditorProgramForm.cbSADSectionChange(Sender: TObject);
var i: integer;
    Parse: TSection;
begin
  btnClearListClick(nil); // change of this combobox will delete whole list
  cbUnitName.Clear;
  cbUnitNameChange(nil);
  i := cbSADSection.ItemIndex;
  if i<0 then
    exit;
  Parse := TSection(cbSADSection.Items.Objects[i]);
  try
    Screen.Cursor := crHourGlass;
    SAD.FillUnits(Parse,false);
    for i := 0 to SAD.Units.Count-1 do
      with TPasUnit(SAD.Units[i]) do
        if isUnit then
          cbUnitName.Items.AddObject(Name+'.pas',SAD.Units[i]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TProjectEditorProgramForm.cbUnitNameChange(Sender: TObject);
var lstCount: integer;
procedure Details(Items: TPasItems; const aCaption: string);
var P: TPasItem;
    PO: TPasCIO absolute P;
    lst: TLisTBox;
procedure Field(Fields: TPasItems);
var f: integer;
    m: TPasItem;
    N: string;
begin
  if (Fields=nil) or (Fields.Count=0) then
    exit;
  N := PO.Name+'.';
  for f := 0 to Fields.Count-1 do begin
    m := TPasItem(Fields[f]);
    lst.Items.AddObject(N+m.Name,m);
  end;
end;
var i, descrHeight: integer;
    TS: TTabSheet;
    descr, saeDescr: TMemo;
begin
  TS := TTabSheet.Create(self);
  TS.PageControl := pagTypes;
  TS.Caption := aCaption;
  lst := TListBox.Create(TS);
  lists[lstCount] := lst;
  inc(lstCount);
  lst.Parent := TS;
  lst.SetBounds(8,8,297,TS.ClientHeight-16);
  lst.Anchors := [akLeft,akTop,akBottom];
  lst.Items.BeginUpdate;
  try
    if (Items=nil) or (Items.Count=0) then
      exit;
    for i := 0 to Items.Count-1 do begin
      P := TPasItem(Items[i]);
{      if P.InheritsFrom(TPasConstant) and
         not TPasConstant(P).IsResourceString then
        exit; }
      lst.Items.AddObject(P.Name,P);
      if P.InheritsFrom(TPasCIO) then begin
        Field(PO.Fields);
        Field(PO.Methods);
        Field(PO.Properties);
      end;
    end;
    lst.ItemIndex := 0;
  finally
    lst.Items.EndUpdate;
    if lst.Items.Count=0 then
      TS.TabVisible := false;
  end;
  if lst.Items.Count=0 then
    exit;
  descrHeight := lst.Height;
  if not PanelRight.Visible then
    descrHeight := descrHeight shr 2;
  descr := TMemo.Create(TS);
  descr.Parent := TS;
  descr.SetBounds(306,8,TS.ClientWidth-(306+8),descrHeight);
  descr.Font := Font;
  descr.WordWrap := true;
  descr.ReadOnly := true;
  descr.ScrollBars := ssVertical;
  lst.Tag := integer(descr);
  if not PanelRight.Visible then begin
    descr.Anchors := [akRight,akLeft,akTop];
    saeDescr := TMemo.Create(TS);
    saeDescr.Parent := TS;
    saeDescr.SetBounds(descr.Left,descr.Top+descrHeight,descr.Width,lst.Height-descrHeight);
    saeDescr.Anchors := [akRight,akLeft,akTop,akBottom];
    saeDescr.Font := Font;
    saeDescr.WordWrap := true;
    saeDescr.ScrollBars := ssVertical;
    saeDescr.OnExit := SaeDescrExit;
    descr.Tag := Integer(saeDescr);
  end else begin
    descr.Anchors := [akRight,akLeft,akTop,akBottom];
    lst.OnDblClick := lstObjectsDblClick;
  end;
  lst.OnClick := lstObjectsClick;
  lstObjectsClick(lst);
end;
var i: integer;
    U: TPasUnit;
begin
  btnClearListClick(nil); // change of this combobox will delete whole list
  for i := pagTypes.PageCount-1 downto 0 do
    pagTypes.Pages[i].Free;
  Application.ProcessMessages;
  i := cbUnitName.ItemIndex;
  if i<0 then
    exit;
  U := TPasUnit(cbUnitName.Items.Objects[i]);
  leUnitDescription.Text := Trim(U.RawDescription);
  leUnitDescription.ReadOnly := PanelRight.Visible;
  SetLength(lists,5);
  lstCount := 0;
  Details(U.CIOs,'Objects'); // DO NOT translate these strings (used in .sae)
  Details(U.FuncsProcs,'Functions');
  Details(U.Types,'Types');
  Details(U.Constants,'Constants');
  Details(U.Variables,'Variables');
end;

procedure TProjectEditorProgramForm.lstObjectsClick(Sender: TObject);
var L: TListBox absolute Sender;
    i: integer;
    P: TPasItem;
    descr, saeDescr: TMemo;
    txt: string;
begin
  if not L.InheritsFrom(TListBox) then
    exit;
  i := L.ItemIndex;
  if i<0 then
    exit;
  descr := TMemo(L.Tag);
  if descr=nil then
    exit;
  P := TPasItem(L.Items.Objects[i]);
  txt := P.ExpandedDeclaration;
  if descr.Tag<>0 then begin
    descr.Lines.Text := txt;
    saeDescr := TMemo(descr.Tag);
    saeDescr.Lines.Text := P.RawDescriptionInfo.Content;
    saeDescr.Tag := integer(P);
  end else
    descr.Lines.Text := txt+#13#10#13#10+P.RawDescriptionInfo.Content;
end;

procedure TProjectEditorProgramForm.lstObjectsDblClick(Sender: TObject);
var L: TListBox absolute Sender;
    i: integer;
    V: string;
begin
  if not L.InheritsFrom(TListBox) then
    exit;
  i := L.ItemIndex;
  if i<0 then
    exit;
  V := L.Items[i];
  if lstValues.Items.IndexOf(V)>=0 then
    exit; // no duplicates
  lstValues.Items.Add(V);
end;

procedure TProjectEditorProgramForm.lstValuesClick(Sender: TObject);
var i,j: integer;
    V: string;
begin
  i := lstValues.ItemIndex;
  if i<0 then
    exit;
  V := lstValues.Items[i];
  for i := 0 to high(lists) do begin
    j := lists[i].Items.IndexOf(V);
    if j<0 then
      continue;
    pagTypes.ActivePageIndex := i;
    lists[i].ItemIndex := j;
    lstObjectsClick(lists[i]);
    break;
  end;
end;

function TProjectEditorProgramForm.GetSaeStorage(const OutputFileName: string): TSectionsStorage;
var i: Integer;
begin
  if saeStorage=nil then
    result := nil else begin
    i := saeStorage.IndexOf(OutputFileName);
    if i<0 then begin
      result := TSectionsStorage.Create;
      saeStorage.AddObject(OutputFileName,result);
    end else
      result := TSectionsStorage(saeStorage.Objects[i]);
  end;
end;

procedure TProjectEditorProgramForm.SaeDescrExit(Sender: TObject);
var saeDescr: TMemo absolute Sender;
    saeStore: TSectionsStorage;
    txt, section: string;
    i: Integer;
begin
  if (self=nil) or not Sender.InheritsFrom(TMemo) or PanelRight.Visible or
     (saeDescr.Tag=0) or not TPasItem(saeDescr.Tag).InheritsFrom(TPasItem) or
     (saeStorage=nil) or saeDescr.ReadOnly then
    exit;
  txt := trim(saeDescr.Text);
  with TPasItem(saeDescr.Tag) do
    if RawDescription<>txt then begin
      RawDescription := txt;
      saeStore := GetSaeStorage(
        TPasUnit(cbUnitName.Items.Objects[cbUnitName.ItemIndex]).OutputFileName);
      if saeStore=nil then
        exit;
      if pagTypes.ActivePageIndex=0 then begin // 'Objects' is first page
        section := lists[0].Items[lists[0].ItemIndex];
        i := Pos('.',section);
        if i=0 then
          saeStore.WriteString('Objects',Name,txt) else
          saeStore.WriteString(Copy(section,1,i-1),Copy(section,i+1,maxInt),txt);
      end  else
        saeStore.WriteString(pagTypes.ActivePage.Caption,Name,txt);
      btnOk.Enabled := true;
    end;
end;

procedure TProjectEditorProgramForm.leUnitDescriptionExit(Sender: TObject);
var txt: string;
begin
  if (cbUnitName.ItemIndex<0) or (saeStorage=nil) or leUnitDescription.ReadOnly then
    exit;
  txt := leUnitDescription.Text;
  with TPasUnit(cbUnitName.Items.Objects[cbUnitName.ItemIndex]) do
    if RawDescription<>txt then begin
      RawDescription := txt;
      GetSaeStorage(OutputFileName).WriteHeader('Description',txt);
      btnOk.Enabled := true;
    end;
end;


end.
