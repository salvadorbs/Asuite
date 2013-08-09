{
Copyright (C) 2006-2013 Matteo Salvi of SalvadorSoftware

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
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, ComCtrls;

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
  private
    { Private declarations }
    procedure PopulateStringList(ListBox: TListBox; StringList: TStringList);
    procedure PopulateListBox(ListBox: TListBox; StringList: TStringList);
    procedure DoScanFolder(Sender: TBaseVirtualTree;FolderPath: string;
                           ParentNode: PVirtualNode;ProgressDialog: TForm);
    procedure RunScanFolder(Tree: TBaseVirtualTree;TempPath: string; ChildNode: PVirtualNode);
  public
    { Public declarations }
  end;

var
  frmScanFolder: TfrmScanFolder;

implementation

uses
  Main, ulCommonUtils, AppConfig, ulFileFolder, ulNodeDataTypes, ulAppConfig,
  ulEnumerations, ulTreeView, ulSysUtils, udImages;

{$R *.dfm}

procedure TfrmScanFolder.btnBrowseClick(Sender: TObject);
var
  TempPath : String;
begin
  if DirectoryExists(Text) then
    TempPath := IncludeTrailingBackslash(BrowseForFolder('', Text))
  else
    TempPath := IncludeTrailingBackslash(BrowseForFolder('', SUITE_WORKING_PATH));

  if (TempPath <> '\') then
    edtFolderPath.Text := TempPath;
end;

procedure TfrmScanFolder.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScanFolder.btnExcludeAddClick(Sender: TObject);
begin
  lxExclude.Items.Add(LowerCase(edtExclude.Text));
  edtExclude.Clear;
end;

procedure TfrmScanFolder.btnExcludeDeleteClick(Sender: TObject);
begin
  if (lxExclude.ItemIndex <> -1) then
  begin
    lxExclude.Items.Delete(lxExclude.ItemIndex);
    lxExclude.ItemIndex := -1;
  end;
end;

procedure TfrmScanFolder.PopulateListBox(ListBox: TListBox;
  StringList: TStringList);
var
  I: Integer;
begin
  ListBox.Clear;
  for I := 0 to StringList.Count - 1 do
    if StringList[I] <> '' then
      ListBox.Items.Add(LowerCase(StringList[I]));
end;

procedure TfrmScanFolder.PopulateStringList(ListBox: TListBox; StringList: TStringList);
var
  I: Integer;
begin
  StringList.Clear;
  for I := 0 to ListBox.Count - 1 do
    StringList.Add(LowerCase(ListBox.Items[I]));
end;

procedure TfrmScanFolder.DoScanFolder(Sender: TBaseVirtualTree;
  FolderPath: string; ParentNode: PVirtualNode;
  ProgressDialog: TForm);
var
  SearchRec     : TSearchRec;
  ChildNode     : PVirtualNode;
  ChildNodeData : TvCustomRealNodeData;
  TempFullName, TempShortName, PathTemp : String;
  ProgressBar   : TProgressBar;
begin
  ProgressBar := TProgressBar(ProgressDialog.FindComponent('Progress'));
  if FindFirst(FolderPath + '*.*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      TempFullName  := LowerCase(SearchRec.Name);
      TempShortName := TempFullName;
      //Absolute path->relative path
      PathTemp := AbsoluteToRelative(FolderPath + SearchRec.Name);
      //Add node in vstlist
      if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) and
         (((SearchRec.Attr and faDirectory) = (faDirectory)) or ((Config.ScanFolderFileTypes.IndexOf(ExtractFileExt(TempFullName)) <> -1))) then
      begin
        //Extract file name without extension (.ext)
        Delete(TempShortName,pos(ExtractFileExt(SearchRec.Name),SearchRec.Name),Length(SearchRec.Name));
        if (Config.ScanFolderExcludeNames.IndexOf(TempShortName) = -1) then
        begin
          if ((SearchRec.Attr and faDirectory) = (faDirectory)) and (Config.ScanFolderSubFolders) then
          begin
            ChildNode := Sender.AddChild(ParentNode, CreateNodeData(vtdtCategory));
            ChildNodeData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(ChildNode)).Data);
            //Dialog
            ProgressBar.Position := ProgressBar.Position + 1;
            ProgressDialog.Update;
            DoScanFolder(Sender, IncludeTrailingBackslash(FolderPath + SearchRec.Name), ChildNode, ProgressDialog);
          end
          else
            if (SearchRec.Attr <> faDirectory) and (Config.ScanFolderFileTypes.IndexOf(ExtractFileExt(TempFullName)) <> -1) then
            begin
              ChildNode := Sender.AddChild(ParentNode, CreateNodeData(vtdtFile));
              ChildNodeData := TvCustomRealNodeData(PBaseData(Sender.GetNodeData(ChildNode)).Data);
              if Assigned(ChildNodeData) then
                TvFileNodeData(ChildNodeData).PathExe := PathTemp;
            end;
          ChildNodeData.pNode := ChildNode;
          ChildNodeData.Name  := TempShortName;
          if (ChildNode.ChildCount = 0) And (ChildNodeData.DataType = vtdtCategory) then
            Sender.DeleteNode(ChildNode);
        end;
      end;
    until FindNext(SearchRec) <> 0;
  end;
  FindClose(SearchRec);
end;

procedure TfrmScanFolder.RunScanFolder(Tree: TBaseVirtualTree;TempPath: string; ChildNode: PVirtualNode);
var
  Dialog: TForm;
begin
  //Run scan
  Dialog := CreateDialogProgressBar(msgScanningProgress, GetNumberSubFolders(TempPath));
  Tree.BeginUpdate;
  try
    DoScanFolder(Tree, TempPath, ChildNode, Dialog);
  finally
    Tree.EndUpdate;
    Dialog.Free;
    Self.Close;
  end;
end;

procedure TfrmScanFolder.btnExcludeReplaceClick(Sender: TObject);
begin
  if (lxExclude.ItemIndex <> -1) and (lxExclude.Count > 0)  then
  begin
    lxExclude.Items.Delete(lxExclude.ItemIndex);
    lxExclude.Items.Insert(lxExclude.ItemIndex,LowerCase(edtExclude.Text));
    lxExclude.ItemIndex := -1;
  end;
end;

procedure TfrmScanFolder.btnScanClick(Sender: TObject);
var
  ChildNode : PVirtualNode;
  ChildNodeData : TvBaseNodeData;
  TempPath  : String;
begin
  if edtFolderPath.Text <> '' then
    TempPath := IncludeTrailingBackslash(edtFolderPath.Text);
  //Save options
  Config.ScanFolderLastPath   := TempPath;
  Config.ScanFolderSubFolders := cbSubfolders.Checked;
  PopulateStringList(lxTypes, Config.ScanFolderFileTypes);
  PopulateStringList(lxExclude, Config.ScanFolderExcludeNames);
  Config.Changed := True;
  //Check if user insert a valid path
  if TempPath = '' then
  begin
    ShowMessage(msgErrEmptyPath);
    Exit;
  end;
  if Not(DirectoryExists(TempPath)) then
  begin
    ShowMessage(msgFolderNotFound);
    Exit;
  end;
  //Add parent node
  ChildNode := AddNodeInVST(frmMain.vstList, frmMain.vstList.FocusedNode, vtdtCategory);
  ChildNodeData       := PBaseData(frmMain.vstList.GetNodeData(ChildNode)).Data;
  ChildNodeData.pNode := ChildNode;
  ChildNodeData.Name  := ExtractDirectoryName(TempPath);
  RunScanFolder(frmMain.vstList, TempPath, ChildNode);
end;

procedure TfrmScanFolder.btnTypesAddClick(Sender: TObject);
begin
  lxTypes.Items.Add(LowerCase(edtTypes.Text));
  edtTypes.Clear;
end;

procedure TfrmScanFolder.btnTypesDeleteClick(Sender: TObject);
begin
  if (lxTypes.ItemIndex <> -1) then
  begin
    lxTypes.Items.Delete(lxTypes.ItemIndex);
    lxTypes.ItemIndex := -1;
  end;
end;

procedure TfrmScanFolder.btnTypesReplaceClick(Sender: TObject);
begin
  if (lxTypes.ItemIndex <> -1) and (lxTypes.Count > 0)  then
  begin
    lxTypes.Items.Delete(lxTypes.ItemIndex);
    lxTypes.Items.Insert(lxTypes.ItemIndex,LowerCase(edtTypes.Text));
    lxTypes.ItemIndex := -1;
  end;
end;

procedure TfrmScanFolder.FormCreate(Sender: TObject);
begin
  if DirectoryExists(Config.ScanFolderLastPath) then
    edtFolderPath.Text := Config.ScanFolderLastPath
  else
    edtFolderPath.Text := SUITE_WORKING_PATH;
  cbSubfolders.Checked   := Config.ScanFolderSubFolders;
  PopulateListBox(lxTypes, Config.ScanFolderFileTypes);
  PopulateListBox(lxExclude, Config.ScanFolderExcludeNames);
end;

end.
