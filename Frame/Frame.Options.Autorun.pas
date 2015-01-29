{
Copyright (C) 2006-2015 Matteo Salvi

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

unit Frame.Options.Autorun;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons,
  DKLang, Frame.BaseEntity, Vcl.StdCtrls, VirtualTrees, Lists.Base;

type
  TfrmAutorunOptionsPage = class(TfrmBaseEntityPage)
    DKLanguageController1: TDKLanguageController;
    grpStartupOrderItems: TGroupBox;
    grpShutdownOrderItems: TGroupBox;
    chkStartup: TCheckBox;
    lblStartupInfo: TLabel;
    chkShutdown: TCheckBox;
    lblShutdownInfo: TLabel;
    vstStartupItems: TVirtualStringTree;
    vstShutdownItems: TVirtualStringTree;
    btnStartupUp: TBitBtn;
    btnStartupDelete: TBitBtn;
    btnStartupDown: TBitBtn;
    btnShutdownDelete: TBitBtn;
    btnShutdownDown: TBitBtn;
    btnShutdownUp: TBitBtn;
    procedure btnStartupUpClick(Sender: TObject);
    procedure btnShutdownUpClick(Sender: TObject);
    procedure btnStartupDeleteClick(Sender: TObject);
    procedure btnShutdownDeleteClick(Sender: TObject);
    procedure btnStartupDownClick(Sender: TObject);
    procedure btnShutdownDownClick(Sender: TObject);
  private
    { Private declarations }
    procedure MoveItemUp(const ATree: TBaseVirtualTree);
    procedure MoveItemDown(const ATree: TBaseVirtualTree);
    procedure RemoveItem(const ATree: TBaseVirtualTree);
    procedure SaveInAutorunItemList(const ATree: TBaseVirtualTree;const AutorunItemList: TBaseItemsList);
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmAutorunOptionsPage: TfrmAutorunOptionsPage;

implementation

uses
  NodeDataTypes.Custom, AppConfig.Main, Forms.Main, DataModules.Icons,
  VirtualTree.Methods, Kernel.Types, NodeDataTypes.Files, NodeDataTypes.Base,
  System.UITypes, Kernel.Enumerations, VirtualTree.Events;

{$R *.dfm}

{ TfrmItemsOptionsPage }

procedure TfrmAutorunOptionsPage.btnShutdownDeleteClick(Sender: TObject);
begin
  RemoveItem(vstShutdownItems);
end;

procedure TfrmAutorunOptionsPage.btnShutdownDownClick(Sender: TObject);
begin
  MoveItemDown(vstShutdownItems);
end;

procedure TfrmAutorunOptionsPage.btnShutdownUpClick(Sender: TObject);
begin
  MoveItemUp(vstShutdownItems);
end;

procedure TfrmAutorunOptionsPage.btnStartupDeleteClick(Sender: TObject);
begin
  RemoveItem(vstStartupItems);
end;

procedure TfrmAutorunOptionsPage.btnStartupDownClick(Sender: TObject);
begin
  MoveItemDown(vstStartupItems);
end;

procedure TfrmAutorunOptionsPage.btnStartupUpClick(Sender: TObject);
begin
  MoveItemUp(vstStartupItems);
end;

function TfrmAutorunOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('autorun');
end;

function TfrmAutorunOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgAutorun');
end;

function TfrmAutorunOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  TVirtualTreeEvents.Create.SetupVSTAutorun(vstStartupItems);
  TVirtualTreeEvents.Create.SetupVSTAutorun(vstShutdownItems);
  //Startup
  dmImages.DrawIconInBitmap(btnStartupUp.Glyph, Config.IconsManager.GetIconIndex('arrow_up'));
  dmImages.DrawIconInBitmap(btnStartupDelete.Glyph, Config.IconsManager.GetIconIndex('delete'));
  dmImages.DrawIconInBitmap(btnStartupDown.Glyph, Config.IconsManager.GetIconIndex('arrow_down'));
  //Shutdown
  dmImages.DrawIconInBitmap(btnShutdownUp.Glyph, Config.IconsManager.GetIconIndex('arrow_up'));
  dmImages.DrawIconInBitmap(btnShutdownDelete.Glyph, Config.IconsManager.GetIconIndex('delete'));
  dmImages.DrawIconInBitmap(btnShutdownDown.Glyph, Config.IconsManager.GetIconIndex('arrow_down'));
  //Autorun
  chkStartup.Checked  := Config.AutorunStartup;
  chkShutdown.Checked := Config.AutorunShutdown;
  //Populate lstStartUp and lstShutdown
  TVirtualTreeMethods.Create.PopulateVSTItemList(vstStartupItems, Config.ListManager.StartupItemList);
  TVirtualTreeMethods.Create.PopulateVSTItemList(vstShutdownItems, Config.ListManager.ShutdownItemList);
  vstStartupItems.Header.AutoFitColumns;
  vstShutdownItems.Header.AutoFitColumns;
end;

function TfrmAutorunOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  //Autorun
  Config.AutorunStartup  := chkStartup.Checked;
  Config.AutorunShutdown := chkShutdown.Checked;
  //Save Startup and Shutdown lists
  SaveInAutorunItemList(vstStartupItems, Config.ListManager.StartupItemList);
  SaveInAutorunItemList(vstShutdownItems, Config.ListManager.ShutdownItemList);
end;

procedure TfrmAutorunOptionsPage.MoveItemUp(const ATree: TBaseVirtualTree);
begin
  if Assigned(ATree.FocusedNode) then
    if ATree.GetFirst <> ATree.FocusedNode then
      ATree.MoveTo(ATree.FocusedNode, ATree.GetPrevious(ATree.FocusedNode), amInsertBefore, False);
end;

procedure TfrmAutorunOptionsPage.MoveItemDown(const ATree: TBaseVirtualTree);
begin
  if Assigned(ATree.FocusedNode) then
    if ATree.GetLast <> ATree.FocusedNode then
      ATree.MoveTo(ATree.FocusedNode, ATree.GetNext(ATree.FocusedNode), amInsertAfter, False);
end;

procedure TfrmAutorunOptionsPage.RemoveItem(const ATree: TBaseVirtualTree);
begin
  if (MessageDlg((DKLangConstW('msgConfirm')),mtWarning, [mbYes,mbNo], 0) = mrYes) then
    if Assigned(ATree.FocusedNode) then
      ATree.IsVisible[ATree.FocusedNode] := False;
end;

procedure TfrmAutorunOptionsPage.SaveInAutorunItemList(
  const ATree: TBaseVirtualTree; const AutorunItemList: TBaseItemsList);
var
  Node : PVirtualNode;
  NodeData: TvCustomRealNodeData;
begin
  AutorunItemList.Clear;
  Node := ATree.GetFirst;
  while Assigned(Node) do
  begin
    NodeData := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(Node, ATree));
    if Assigned(NodeData) then
    begin
      if ATree.IsVisible[Node] then
        NodeData.AutorunPos := AutorunItemList.AddItem(NodeData)
      else
        NodeData.Autorun := atNever;
      NodeData.Changed := True;
    end;
    Node := ATree.GetNext(Node);
  end;
end;

end.
