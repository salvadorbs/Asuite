{
Copyright (C) 2006-2008 Matteo Salvi of SalvadorSoftware

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit ScanFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees;

type
  TfrmScanFolder = class(TForm)
    btnScan: TButton;
    btnCancel: TButton;
    pnlScan: TPanel;
    edtFolderPath: TEdit;
    lbFolderPath: TLabel;
    btnBrowse: TButton;
    lxTypes: TListBox;
    lbTypes: TLabel;
    edtTypes: TEdit;
    btnTypesAdd: TButton;
    btnTypesReplace: TButton;
    btnTypesDelete: TButton;
    cbRetrieveInfo: TCheckBox;
    cbSubfolders: TCheckBox;
    lbExclude: TLabel;
    lxExclude: TListBox;
    edtExclude: TEdit;
    btnExcludeAdd: TButton;
    btnExcludeReplace: TButton;
    btnExcludeDelete: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnTypesAddClick(Sender: TObject);
    procedure btnTypesReplaceClick(Sender: TObject);
    procedure btnTypesDeleteClick(Sender: TObject);
    procedure btnExcludeAddClick(Sender: TObject);
    procedure btnExcludeReplaceClick(Sender: TObject);
    procedure btnExcludeDeleteClick(Sender: TObject);
    procedure TranslateForm(Lingua: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmScanFolder: TfrmScanFolder;

implementation

uses Main, CommonUtils;

{$R *.dfm}

procedure TfrmScanFolder.btnBrowseClick(Sender: TObject);
var
  TempPath : String;
begin
  with edtFolderPath do
  begin
    if DirectoryExists(Text) then
      TempPath := Text
    else
      TempPath := ApplicationPath;
    TempPath := BrowseForFolder('',TempPath) + '\';
    if (TempPath <> '\') then
      Text := TempPath;
  end;
end;

procedure TfrmScanFolder.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScanFolder.btnExcludeAddClick(Sender: TObject);
begin
  with lxExclude do
  begin
    if (count = 10) then
    begin
      ShowMessage(ArrayMessages[14]);
      Exit;
    end;
    if ((Items.Count) = 1) and (Items[0] = '') then
      Items.Delete(0);
    Items.Add(LowerCase(edtExclude.Text));
  end;
  edtExclude.Clear;
end;

procedure TfrmScanFolder.btnExcludeDeleteClick(Sender: TObject);
begin
  with lxExclude do
    if (ItemIndex <> -1) then
    begin
      Items.Delete(ItemIndex);
      ItemIndex := -1;
    end;
end;

procedure TfrmScanFolder.btnExcludeReplaceClick(Sender: TObject);
begin 
  with lxExclude do
    if (ItemIndex <> -1) and (Count > 0)  then
    begin
      Items.Delete(ItemIndex);
      Items.Insert(ItemIndex,LowerCase(edtExclude.Text));
      ItemIndex := -1;
    end;
end;

procedure TfrmScanFolder.btnScanClick(Sender: TObject);
var
  ChildNode : PVirtualNode;
  ChildNodeData, ParentNodeData : PTreeData;
  Dialog    : TForm;
  I         : Integer;
//  cTempo1, cTempo2 : Cardinal;
begin
  if edtFolderPath.Text = '' then
    Exit;
  with frmMain do
  begin
    //Save options
    ScanSettings.LastFolderPath := edtFolderPath.Text;
    ScanSettings.SubFolders     := cbSubfolders.Checked;
    ScanSettings.RetrieveInfo   := cbRetrieveInfo.Checked;
    ScanSettings.FileTypes.Clear;
    ScanSettings.ExcludeFiles.Clear;
    for I := 0 to lxTypes.Count - 1 do
      ScanSettings.FileTypes.Add(LowerCase(StringReplace(lxTypes.Items[I],'*','',[rfReplaceAll])));
    for I := 0 to lxExclude.Count - 1 do
      ScanSettings.ExcludeFiles.Add(LowerCase(lxExclude.Items[I]));
    //Run scan
    if Assigned(vstList.FocusedNode) then
    begin
      ParentNodeData := vstList.GetNodeData(vstList.FocusedNode);
      if (ParentNodeData.Tipo = 0) then
      begin
        ChildNode    := vstList.AddChild(vstList.FocusedNode);
        vstList.Expanded[vstList.FocusedNode] := true;
      end
      else
        ChildNode    := vstList.AddChild(vstList.NodeParent[vstList.FocusedNode]);
    end
    else
      ChildNode      := vstList.AddChild(nil);
//      cTempo1 := GetTickCount;
    Dialog := CreateDialogProgressBar(ArrayMessages[23],GetNumberSubFolders(edtFolderPath.Text + '\'));
    ChildNodeData            := vstList.GetNodeData(ChildNode);
    ChildNodeData.pNode      := ChildNode;
    ChildNodeData.Name       := ExtractDirectoryName(edtFolderPath.Text);
    ChildNodeData.Tipo       := 0;
    ChildNodeData.ImageIndex := 1;
    RunScanFolder(vstList,ImageList1,edtFolderPath.Text,ChildNode, Dialog);
    Dialog.Free;
  end;
  close;
//  cTempo2 := GetTickCount;
//  ShowMessageFmt('Caricato in %d ms',[cTempo2 - cTempo1]);
end;

procedure TfrmScanFolder.btnTypesAddClick(Sender: TObject);
begin
  with lxTypes do
  begin
    if (count = 10) then
    begin
      ShowMessage(ArrayMessages[14]);
      Exit;
    end;
    if ((Items.Count) = 1) and (Items[0] = '') then
      Items.Delete(0);
    Items.Add(LowerCase(edtTypes.Text));
  end;
  edtTypes.Clear;
end;

procedure TfrmScanFolder.btnTypesDeleteClick(Sender: TObject);
begin
  with lxTypes do
    if (ItemIndex <> -1) then
    begin
      Items.Delete(ItemIndex);
      ItemIndex := -1;
    end;
end;

procedure TfrmScanFolder.btnTypesReplaceClick(Sender: TObject);
begin 
  with lxTypes do
    if (ItemIndex <> -1) and (Count > 0)  then
    begin
      Items.Delete(ItemIndex);
      Items.Insert(ItemIndex,LowerCase(edtTypes.Text));
      ItemIndex := -1;
    end;
end;

procedure TfrmScanFolder.FormCreate(Sender: TObject);
var
I : Integer;
begin
  TranslateForm(LauncherOptions.LangName);
  if DirectoryExists(ScanSettings.LastFolderPath) then
    edtFolderPath.Text := ScanSettings.LastFolderPath
  else
    edtFolderPath.Text := ApplicationPath;
  cbSubfolders.Checked   := ScanSettings.SubFolders;
  cbRetrieveInfo.Checked := ScanSettings.RetrieveInfo;
  for I := 0 to ScanSettings.FileTypes.Count - 1 do
    if ScanSettings.FileTypes[I] <> '' then
      lxTypes.Items.Add(LowerCase(ScanSettings.FileTypes[I]));
  for I := 0 to ScanSettings.ExcludeFiles.Count - 1 do
    if ScanSettings.ExcludeFiles[I] <> '' then
      lxExclude.Items.Add(LowerCase(ScanSettings.ExcludeFiles[I]));
end;   

procedure TfrmScanFolder.TranslateForm(Lingua: string);
begin
  with frmMain.xmldTranslate.DocumentElement.ChildNodes['Form7'] do
  begin
    Caption              := ChildNodes['Form7Caption'].Text;
    lbFolderPath.Caption := ChildNodes['LabelFolderPath'].Text;
    btnBrowse.Caption    := ChildNodes['ButtonBrowse'].Text;
    cbSubfolders.Caption := ChildNodes['CheckboxSubFolders'].Text;
    lbTypes.Caption      := ChildNodes['LabelTypes'].Text;
    lbExclude.Caption    := ChildNodes['LabelExclude'].Text;
    btnTypesAdd.Caption  := ChildNodes['ButtonAdd'].Text;
    btnTypesReplace.Caption   := ChildNodes['ButtonReplace'].Text;
    btnTypesDelete.Caption    := ChildNodes['ButtonDelete'].Text;
    btnExcludeAdd.Caption     := ChildNodes['ButtonAdd'].Text;
    btnExcludeReplace.Caption := ChildNodes['ButtonReplace'].Text;
    btnExcludeDelete.Caption  := ChildNodes['ButtonDelete'].Text;
    cbRetrieveInfo.Caption    := ChildNodes['CheckboxRetrieveInfo'].Text;
    btnScan.Caption   := ChildNodes['ButtonScan'].Text;
    btnCancel.Caption := ChildNodes['ButtonCancel'].Text;
  end;
end;

end.
