{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Forms.Dialog.BaseEntity;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Frame.BaseEntity, VirtualTrees, ButtonPanel;

type

  { TfrmDialogBase }

  TfrmDialogBase = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    pnlDialogPage: TPanel;
    vstCategory: TVirtualStringTree;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure SaveNodeData(Sender: TBaseVirtualTree; Node: PVirtualNode;
                           Data: Pointer; var Abort: Boolean);
    function GetNodeByFrameClass(Tree: TBaseVirtualTree; AFramePage: TPageFrameClass;
                                 Node: PVirtualNode = nil): PVirtualNode;
    procedure AdjustPanelSize;
  strict protected
    FCurrentPage: TfrmBaseEntityPage;
    FDefaultPage: TPageFrameClass;
    FFrameGeneral: PVirtualNode;
    function InternalLoadData: Boolean; virtual;
    function InternalSaveData: Boolean; virtual;
    function AddFrameNode(Tree: TBaseVirtualTree; Parent: PVirtualNode;
                          FramePage: TfrmBaseEntityPage): PVirtualNode;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property CurrentPage: TfrmBaseEntityPage read FCurrentPage write FCurrentPage;

    procedure ChangePage(NewPage: TfrmBaseEntityPage);
  end;

var
  frmDialogBase: TfrmDialogBase;

implementation

uses
  Kernel.Types, Kernel.Logger, SynLog, Kernel.Instance;

{$R *.lfm}

{ TfrmDialogBase }

function TfrmDialogBase.AddFrameNode(Tree: TBaseVirtualTree;
  Parent: PVirtualNode; FramePage: TfrmBaseEntityPage): PVirtualNode;
var
  NodeData: PFramesNodeData;
begin
  Result   := Tree.AddChild(Parent);
  NodeData := Tree.GetNodeData(Result);
  if Assigned(NodeData) then
  begin
    NodeData.Frame := FramePage;
    NodeData.Title := FramePage.Title;
    NodeData.ImageIndex := FramePage.ImageIndex;

    FramePage.Parent := Self.pnlDialogPage;
    FramePage.Visible := False;
    FramePage.Align := alClient;

    FramePage.LoadData;
  end;
end;

procedure TfrmDialogBase.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmDialogBase.btnOkClick(Sender: TObject);
begin
  //If IterateSubtree returns a value, something is wrong
  if Not Assigned(vstCategory.IterateSubtree(nil, SaveNodeData, nil)) then
  begin
    if InternalSaveData then
      ModalResult := mrOk;
  end
  else
    ModalResult := mrNone;
end;

procedure TfrmDialogBase.ChangePage(NewPage: TfrmBaseEntityPage);
begin
  if Assigned(FCurrentPage) then
  begin
    if FCurrentPage = NewPage then
      Exit
    else
     FCurrentPage.Visible := False;
  end;
  FCurrentPage := TfrmBaseEntityPage(NewPage);
  FCurrentPage.Visible := True;

  AdjustPanelSize;
end;

constructor TfrmDialogBase.Create(AOwner: TComponent);
var
  selNode: PVirtualNode;
begin
  inherited;
  ASuiteInstance.VSTEvents.SetupVSTDialogFrame(vstCategory);
  ButtonPanel1.OKButton.OnClick := btnOkClick;
  ButtonPanel1.CancelButton.OnClick := btnCancelClick;

  //Load frames
  Self.InternalLoadData;

  //Set default page
  if not Assigned(FDefaultPage) then
    selNode := FFrameGeneral
  else
    selNode := GetNodeByFrameClass(Self.vstCategory, FDefaultPage);

  //Select node (automatically open frame using vst's AddToSelection event)
  Self.vstCategory.FocusedNode := selNode;
  Self.vstCategory.Selected[selNode] := True;
  Self.vstCategory.FullExpand;
  Self.pnlDialogPage.TabOrder := 0;

  {$IFDEF UNIX}
  //Workaround for form glitch in GTK2
  BorderStyle := bsSizeable;
  {$ENDIF}
end;

procedure TfrmDialogBase.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    btnOkClick(Sender)
  else
    if Ord(Key) = VK_ESCAPE then
      btnCancelClick(Sender);
end;

function TfrmDialogBase.GetNodeByFrameClass(Tree: TBaseVirtualTree;
  AFramePage: TPageFrameClass; Node: PVirtualNode): PVirtualNode;
var
  NodeData: PFramesNodeData;
begin
  Result := nil;

  if Node = nil then
    Node := Tree.GetFirst;

  while Assigned(Node) do
  begin
    NodeData := Tree.GetNodeData(Node);

    if NodeData.Frame.ClassName = AFramePage.ClassName then
      Exit(Node);

    if Node.ChildCount > 0 then
      Result := GetNodeByFrameClass(Tree, AFramePage, Node.FirstChild);

    Node := Tree.GetNextSibling(Node);
  end;
end;

procedure TfrmDialogBase.AdjustPanelSize;
var
  NodeData: PFramesNodeData;
  Node: PVirtualNode;
  intWidth, intHeight: Integer;
begin
  intWidth := pnlDialogPage.Constraints.MinWidth;
  intHeight := pnlDialogPage.Constraints.MinHeight;
  Node := vstCategory.GetFirst;

  while Assigned(Node) do
  begin
    NodeData := vstCategory.GetNodeData(Node);

    if Assigned(NodeData) then
    begin
      if NodeData.Frame.Height > intHeight then
        intHeight := NodeData.Frame.Height;

      if NodeData.Frame.Width > intWidth then
        intWidth := NodeData.Frame.Width;
    end;

    Node := vstCategory.GetNext(Node);
  end;

  if pnlDialogPage.Height > intHeight then
    intHeight := pnlDialogPage.Height;

  if pnlDialogPage.Width > intWidth then
    intWidth := pnlDialogPage.Width;

  if intHeight > pnlDialogPage.Constraints.MinHeight then
    pnlDialogPage.Constraints.MinHeight := intHeight;

  if intWidth > pnlDialogPage.Constraints.MinWidth then
    pnlDialogPage.Constraints.MinWidth := intWidth;
end;

function TfrmDialogBase.InternalLoadData: Boolean;
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TfrmDialogBase.InternalLoadData', Self);
  Result := True;
end;

function TfrmDialogBase.InternalSaveData: Boolean;
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TfrmDialogBase.InternalSaveData', Self);
  Result := True;
end;

procedure TfrmDialogBase.SaveNodeData(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NodeData: PFramesNodeData;
begin
  //Call Frame's function SaveData
  NodeData := vstCategory.GetNodeData(Node);
  if Assigned(NodeData) then
    Abort := Not(TfrmBaseEntityPage(NodeData.Frame).SaveData);
end;

end.
