{
Copyright (C) 2006-2020 Matteo Salvi

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

unit Frame.Options.Hotkey;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DKLang, Frame.BaseEntity, VirtualTrees,
  Vcl.ComCtrls, Vcl.StdCtrls, Lists.Base, Vcl.Menus;

type
  TfrmHotkeyOptionsPage = class(TfrmBaseEntityPage)
    DKLanguageController1: TDKLanguageController;
    gbHotkey: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    cbHotKey: TCheckBox;
    hkWindow: THotKey;
    hkGraphicMenu: THotKey;
    grpOrderSoftware: TGroupBox;
    vstItems: TVirtualStringTree;
    pmHotkey: TPopupMenu;
    mniEditHotkey: TMenuItem;
    mniRemoveHotkey: TMenuItem;
    mniN1: TMenuItem;
    mniProperties: TMenuItem;
    procedure cbHotKeyClick(Sender: TObject);
    procedure hkWindowChange(Sender: TObject);
    procedure hkGraphicMenuChange(Sender: TObject);
    procedure hkWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure hkGraphicMenuMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mniEditHotkeyClick(Sender: TObject);
    procedure mniRemoveHotkeyClick(Sender: TObject);
    procedure mniPropertiesClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadGlyphs;
    procedure SaveInHotkeyItemList(const ATree: TBaseVirtualTree;const AItemList: TBaseItemsList);
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
  end;

var
  frmHotkeyOptionsPage: TfrmHotkeyOptionsPage;

implementation

uses
  AppConfig.Main, VirtualTree.Events, VirtualTree.Methods, NodeDataTypes.Custom,
  Forms.ShortcutGrabber;

{$R *.dfm}

{ TfrmHotkeyOptionsPage }

procedure TfrmHotkeyOptionsPage.cbHotKeyClick(Sender: TObject);
begin
  hkWindow.Enabled := cbHotKey.Checked;
  hkGraphicMenu.Enabled := cbHotKey.Checked;
end;

function TfrmHotkeyOptionsPage.GetImageIndex: Integer;
begin
  Result := Config.IconsManager.GetIconIndex('hotkey');
end;

function TfrmHotkeyOptionsPage.GetTitle: string;
begin
  Result := DKLangConstW('msgHotkey');
end;

procedure TfrmHotkeyOptionsPage.hkGraphicMenuChange(Sender: TObject);
begin
  if hkGraphicMenu.Modifiers = [] then
    hkGraphicMenu.Modifiers := [hkAlt];
end;

procedure TfrmHotkeyOptionsPage.hkGraphicMenuMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  hkGraphicMenu.HotKey := TShorcutGrabber.Execute(Self);
end;

procedure TfrmHotkeyOptionsPage.hkWindowChange(Sender: TObject);
begin
  if hkWindow.Modifiers = [] then
    hkWindow.Modifiers := [hkAlt];
end;

procedure TfrmHotkeyOptionsPage.hkWindowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  hkWindow.HotKey := TShorcutGrabber.Execute(Self);
end;

function TfrmHotkeyOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  TVirtualTreeEvents.Create.SetupVSTHotkey(vstItems);
  //Hot Keys
  cbHotKey.Checked := Config.HotKey;
  hkWindow.HotKey  := Config.WindowHotKey;
  hkGraphicMenu.HotKey := Config.MenuHotkey;
  //Populate VST with HotKeyItemList's items
  TVirtualTreeMethods.Create.PopulateVSTItemList(vstItems, Config.ListManager.HotKeyItemList);
  vstItems.Header.AutoFitColumns;
  //Enable/disable visual components
  cbHotKeyClick(Self);
  LoadGlyphs;
end;

function TfrmHotkeyOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;
  //Hot Keys
  Config.HotKey       := cbHotKey.Checked;
  Config.WindowHotKey := hkWindow.HotKey;
  Config.MenuHotKey   := hkGraphicMenu.HotKey;
  //Save vst items in HotKeyItemList
  SaveInHotkeyItemList(vstItems, Config.ListManager.HotKeyItemList);
end;

procedure TfrmHotkeyOptionsPage.LoadGlyphs;
begin
  mniRemoveHotkey.ImageIndex := Config.IconsManager.GetIconIndex('keyboard_delete');
  mniEditHotkey.ImageIndex   := Config.IconsManager.GetIconIndex('keyboard_edit');
  mniProperties.ImageIndex   := Config.IconsManager.GetIconIndex('property');
end;

procedure TfrmHotkeyOptionsPage.mniEditHotkeyClick(Sender: TObject);
var
  ShortCut: TShortCut;
  NodeData: TvCustomRealNodeData;
begin
  if Assigned(vstItems.FocusedNode) then
  begin
    NodeData := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(vstItems.FocusedNode, vstItems));
    if Assigned(NodeData) then
    begin
      ShortCut := TShorcutGrabber.Execute(Self);
      if (ShortCut <> 0) then
      begin
        NodeData.Hotkey  := ShortCut;
        NodeData.ActiveHotkey := True;
        NodeData.Changed := True;
      end;
    end;
  end;
end;

procedure TfrmHotkeyOptionsPage.mniPropertiesClick(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ShowItemProperty(Self, vstItems, vstItems.FocusedNode, False);
end;

procedure TfrmHotkeyOptionsPage.mniRemoveHotkeyClick(Sender: TObject);
begin
  if (MessageDlg((DKLangConstW('msgConfirm')),mtWarning, [mbYes,mbNo], 0) = mrYes) then
    if Assigned(vstItems.FocusedNode) then
      vstItems.IsVisible[vstItems.FocusedNode] := False;
end;

procedure TfrmHotkeyOptionsPage.SaveInHotkeyItemList(
  const ATree: TBaseVirtualTree; const AItemList: TBaseItemsList);
var
  Node : PVirtualNode;
  NodeData: TvCustomRealNodeData;
begin
  Node := ATree.GetFirst;
  while Assigned(Node) do
  begin
    NodeData := TvCustomRealNodeData(TVirtualTreeMethods.Create.GetNodeItemData(Node, ATree));
    if Assigned(NodeData) then
    begin
      NodeData.ActiveHotkey := ATree.IsVisible[Node];
      if NodeData.ActiveHotkey then
        AItemList.AddItem(NodeData)
      else
        AItemList.RemoveItem(NodeData);
      NodeData.Changed := True;
    end;
    Node := ATree.GetNext(Node);
  end;
end;

end.
