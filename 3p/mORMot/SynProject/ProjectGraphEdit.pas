/// GraphViz interactive diagram editor form
// - this unit is part of SynProject, under GPL 3.0 license; version 1.13
unit ProjectGraphEdit;

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

{.$define USEGDIPLUSFORIMAGES}
// if defined, GDI+ library will be used for reading jpeg and png images
// (requires Windows XP and later - or GdiPlus.dll in program folder)
// -> NOT defined because GDI+ is buggy for Bezier curves

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ProjectParser, ProjectSections,
  ComCtrls;

type
  TGraphEditForm = class(TForm)
    Image: TImage;
    Panel1: TPanel;
    EditName: TEdit;
    EditTitle: TEdit;
    Source: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnSave: TButton;
    btnClose: TButton;
    btnRefresh: TButton;
    Splitter1: TSplitter;
    btnFromCode: TButton;
    PopupMenuFromCode: TPopupMenu;
    FromCodeObjecthierarchy: TMenuItem;
    FromCodeSQLRecord: TMenuItem;
    Tree: TTreeView;
    FromCodeObjectHierarchyHide: TMenuItem;
    N1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FromCodeClick(Sender: TObject);
    procedure btnFromCodeClick(Sender: TObject);
    procedure TreeClick(Sender: TObject);
  private
    DotOK: boolean;
  public
    DotEngine: variant;
    SAD: TProjectBrowser;
  end;



implementation

uses
{$ifdef USEGDIPLUSFORIMAGES}
  SynGdiPlus,
{$endif}
  PasDoc_Items,
  PasDoc_HierarchyTree;

{$R *.dfm}


resourcestring
  sHlpGraphViz = 'Enter either standard dot lines (e.g. rankdir=LR;),'#13+
    'either "\From Text\To¤Text[\Label between both]"'#13+
    '"=From=From¤Text"    ¤=CRLF'#13+
    '"\One Text=Other text at same rank=3rd text at same rank"';
  sHlpGraphClasses = 'Select one or several classes to create the '+
    'corresponding hierarchy diagram';
  sHlpGraphRecord = 'Select one TSQLRecord child to create the corresponding '+
    'field layout structure';

procedure TGraphEditForm.FormShow(Sender: TObject);
begin
  Source.SetFocus;
  if not GetWingraphviz(DotEngine) then
    exit;
  Label3.Caption := sHlpGraphviz;
  DotOK := true;
  btnRefreshClick(nil);
  BtnFromCode.Enabled := SAD<>nil;
  if GetProcAddress(GetModuleHandle('kernel32'), 'GetLocaleInfoEx')=nil then
    DoubleBuffered := true; // DoubleBuffered is buggy under Vista/Seven
end;

procedure TGraphEditForm.FormDestroy(Sender: TObject);
begin
  DotEngine := null; // release memory
  SAD.Free;
end;

procedure TGraphEditForm.btnRefreshClick(Sender: TObject);
var Pic: TMetaFile;
    Img: Variant;
    DotContent: string;
{$ifdef USEGDIPLUSFORIMAGES}
    Bmp: TBitmap;
{$endif}
begin
  if not DotOK then exit;
  DotContent := WingraphvizFromText('test',Source.Lines);
  if not DotEngine.Validate(DotContent) then
    exit;
  Img := DotEngine.ToSVG(DotContent);
  Pic := SVGToEMF(Img,'',true);
  if Pic<>nil then
  try
{$ifdef USEGDIPLUSFORIMAGES}
    Bmp := LoadFrom(Pic); // Bezier curves are buggy
    try
      Image.Picture.Bitmap := Bmp;
    finally
      Bmp.Free;
    end;
{$else}
    Image.Picture.Assign(Pic);
{$endif}
  finally
    Pic.Free;
  end;
end;

procedure TGraphEditForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F9:     btnRefreshClick(nil);
    VK_ESCAPE: Close;
  end;
end;

procedure TGraphEditForm.FromCodeClick(Sender: TObject);
function FindNode(M: TTreeNode; aData: pointer): TTreeNode;
var i: integer;
begin
  if (M=nil) or (M.Data=aData) then
    result := M else begin
    for i := 0 to M.Count-1 do begin
      result := FindNode(M.Item[i],aData);
      if result<>nil then
        exit;
    end;
    result := nil;
  end;
end;
var i, p: integer;
    aCaption: string;
    Node: TPasItemNode;
    Root, M, T: TTreeNode;
begin
  if (SAD=nil) or not Sender.InheritsFrom(TMenuItem) then
    exit;
  if Sender=FromCodeObjectHierarchyHide then
    p := -1 else begin
    aCaption := TMenuItem(Sender).Hint;
    p := -1;
    with SAD.Project do
      for i := 0 to high(Parse) do
        if SameText(Parse[i].SectionNameValue,aCaption) then begin
          p := i;
          break;
        end;
  end;
  Root := nil;
  Tree.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    Tree.Items.Clear;
    Tree.Visible := p>=0;
    Source.Visible := p<0;
    Tree.Tag := p;
    if cardinal(p)>=cardinal(length(SAD.Project.Parse)) then begin
      Label3.Caption := sHlpGraphviz;
      exit;
    end;
    SAD.FillUnits(SAD.Project.Parse[p],false);
    SAD.CreateClassHierarchy;
    if TMenuItem(Sender).Parent=FromCodeSQLRecord then begin
      // per SAD-* module TSQLRecord diagrams
      Tree.MultiSelect := false;
      Root := Tree.Items.AddChild(nil,'TSQLRecord');
      Node := SAD.ClassHierarchy.FirstItem;
      while Node<>nil do begin
        if Node.HasParent('TSQLRecord') then begin
          if not SameText(Node.Parent.Name,'TSQLRecord') then
            M := FindNode(Root,Node.Parent) else
            M := nil;
          if M=nil then
            M := Root;
          Tree.Items.AddChild(M,Node.Name).Data := Node;
        end;
        Node := SAD.ClassHierarchy.NextItem(Node);
      end;
      Label3.Caption := sHlpGraphRecord;
    end else begin
      // per SAD-* module diagram of all classes
      Tree.MultiSelect := true;
      Root := Tree.Items.AddChild(nil,'TObject');
      Root.Data := SAD.ClassHierarchy.ItemOfName('TObject');
      Node := SAD.ClassHierarchy.FirstItem;
      while Node<>nil do begin
        if Node<>Root.Data then begin
          M := Root;
          if Node.Parent.Name<>'TObject' then
            for i := 0 to Tree.Items.Count-1 do begin
              T := FindNode(Tree.Items[i],Node.Parent);
              if T<>nil then begin
                M := T;
                break;
              end;
            end;
          Tree.Items.AddChild(M,Node.Name).Data := Node;
        end;
        Node := SAD.ClassHierarchy.NextItem(Node);
      end;
      Label3.Caption := sHlpGraphClasses;
    end;
    Root.Expand(true);
  finally
    Tree.Items.EndUpdate;
    Tree.Selected := Root;
    Screen.Cursor := crDefault;
  end;
end;

procedure TGraphEditForm.btnFromCodeClick(Sender: TObject);
function NewMenu(Menu: TMenuItem; const aCaption: string): TMenuItem;
begin
  result := TMenuItem.Create(self);
  result.Caption := aCaption;
  result.Hint := aCaption; // so no '&' auto adding
  result.OnClick := FromCodeClick;
  Menu.Add(result);
end;
var i: integer;
begin
  if (SAD<>nil) and (FromCodeObjectHierarchy.Count=0) then
    for i := 0 to high(SAD.Project.Parse) do
    with SAD.Project.Parse[i] do begin
      NewMenu(FromCodeObjectHierarchy,SectionNameValue);
      NewMenu(FromCodeSQLRecord,SectionNameValue);
    end;
  FromCodeObjectHierarchy.Enabled := FromCodeObjectHierarchy.Count>0;
  FromCodeSQLRecord.Enabled := FromCodeSQLRecord.Count>0;
  with ClientToScreen(btnFromCode.BoundsRect.TopLeft) do
    PopupMenuFromCode.Popup(X,Y+btnFromCode.Height);
end;

procedure TGraphEditForm.TreeClick(Sender: TObject);
var i, f: integer;
    Node, Sub: TPasItemNode;
    Item: TPasItem;
    Sel: TTreeNode;
    s,d: string;
function TableName(const ClassName: string): string;
begin
  if copy(ClassName,1,4)='TSQL' then
    if SameText(copy(ClassName,5,6),'Record') then
      result := copy(ClassName,11,maxInt) else
      result := copy(ClassName,5,maxInt) else
      result := copy(ClassName,2,maxInt);
end;
begin
  if not Tree.Visible then
    exit;
  if Tree.MultiSelect then begin
    // all classes diagram
    if Tree.SelectionCount=0 then
      exit;
    for i := 0 to Tree.SelectionCount-1 do begin
      Node := Tree.Selections[i].Data;
      if Node=nil then continue;
      Item := Node.Item;
      if (Item=nil) or not Item.InheritsFrom(TPasCio) then continue;
      with TPasCio(Item) do begin
        if Node.Parent=nil then
          s := s+'\'+Name+'\TObject'#13#10 else
          s := s+'\'+Name+'\'+Node.Parent.Name+#13#10;
        d := Name; // latest child class used for title 
      end;
      EditName.Text := 'Hier'+d;
      EditTitle.Text := d+' classes hierarchy';
     end;
  end else begin
    // TSQLRecord diagrams 
    Sel := Tree.Selected;
    if (Sel=nil) or (Sel.Data=nil) then
      exit;
    Node := Sel.Data;
    s := TableName(Node.Name);
    EditName.Text := 'DB'+s;
    EditTitle.Text := s+' Record Layout';
    s := 'rankdir=LR;'#13#10'node [shape=Mrecord];'#13#10'struct1 [label="ID : integer';
    f := 0;
    repeat
      Item := Node.Item;
      if (Item<>nil) and Item.InheritsFrom(TPasCio) then
        with TPasCio(Item) do
        for i := 0 to Properties.Count-1 do
          with TPasProperty(Properties.PasItemAt[i]) do
          if Visibility=viPublished then begin
            s := s+'|';
            if SAD<>nil then begin
              Sub := SAD.ClassHierarchy.ItemOfName(Proptype);
              if Sub.HasParent('TSQLRecord') then begin
                s := s+'<f'+IntToStr(f)+'>';
                d := d+'struct'+IntToStr(f+2)+' [label="'+TableName(Sub.Name)+
                  '"];'#13#10'struct1:f'+IntToStr(f)+' -> struct'+IntToStr(f+2)+
                  ';'#13#10;
                inc(f);
              end;
            end;
            s := s+Name+' : '+Proptype;
          end;
      Node := Node.Parent;
    until Node=nil;
    s := s+'"];'+#13#10+d;
  end;
  Source.Text := s;
  btnRefreshClick(nil);
end;

end.
