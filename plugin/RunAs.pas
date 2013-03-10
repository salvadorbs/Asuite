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

unit RunAs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees;

type
  TfrmRunAs = class(TForm)
    Panel1: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    btnOk: TButton;
    btnCancel: TButton;    
    procedure TranslateForm(Lingua:string);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunAs: TfrmRunAs;

implementation  

uses Main, CommonUtils;

{$R *.dfm}     

procedure TfrmRunAs.TranslateForm(Lingua:string);
begin
  with frmMain.xmldTranslate.DocumentElement.ChildNodes['Form11'] do
  begin
    Caption            := ChildNodes['Form11Caption'].Text;
    lbUsername.Caption := ChildNodes['LabelUsername'].Text;
    lbPassword.Caption := ChildNodes['LabelPassword'].Text;
    btnOk.Caption      := ChildNodes['ButtonOk'].Text;
    btnCancel.Caption  := ChildNodes['ButtonCancel'].Text;
  end;
end;

procedure TfrmRunAs.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRunAs.btnOkClick(Sender: TObject);
var
  Tree         : TBaseVirtualTree;
  NodeDataList : PTreeData;
  Node         : PVirtualNode;
begin
  if frmMain.pcList.ActivePageIndex = 0 then
    Tree := frmMain.vstList
  else
    Tree := frmMain.vstSearch;
  Node := Tree.GetFirstSelected;
  while Assigned(Node) do
  begin
    if (Tree = frmMain.vstList) then
      NodeDataList := frmMain.vstList.GetNodeData(Node)
    else
      NodeDataList := NodeDataXToNodeDataList(Node);
    if Assigned(NodeDataList) then
    begin
      //Run file
      if (NodeDataList.Tipo = 1) or (NodeDataList.Tipo = 2) then
      begin
        RunProcessAsUser(frmMain.vstList,NodeDataList,edtUsername.Text,edtPassword.Text);
        AddMRU(frmMain.vstList,frmMain.CoolTrayIcon1,NodeDataList.pNode,NodeDataList.DontInsertMRU);
        RunActionOnExe(NodeDataList);
      end;
    end;
    if Node <> Tree.GetNextSelected(node) then
      Node := Tree.GetNextSelected(node)
    else
      Node := nil;
  end;
  close;
end;

procedure TfrmRunAs.FormCreate(Sender: TObject);
begin
  TranslateForm(LauncherOptions.LangName);
end;

end.
