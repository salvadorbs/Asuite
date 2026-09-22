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

unit Frame.Options.Hotkey;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, SysUtils, Graphics,
  Controls, Dialogs, Frame.BaseEntity, VirtualTrees,
  ComCtrls, StdCtrls, Lists.Base, HotKeyEdit, Menus, ActnList, Classes;

type

  { TfrmHotkeyOptionsPage }

  TfrmHotkeyOptionsPage = class(TfrmBaseEntityPage)
    actProperties: TAction;
    actRemoveHotkey: TAction;
    actEditHotkey: TAction;
    ActionList1: TActionList;
    edtHotkeyCM: THotKeyEdit;
    edtHotkeyGM: THotKeyEdit;
    edtHotkeyMF: THotKeyEdit;
    
    gbHotkey: TGroupBox;
    cbHotKey: TCheckBox;
    grpOrderSoftware: TGroupBox;
    lblHotkeyCM: TLabel;
    lblHotkeyGM: TLabel;
    lblHotkeyWindow: TLabel;
    vstItems: TVirtualStringTree;
    pmHotkey: TPopupMenu;
    mniEditHotkey: TMenuItem;
    mniRemoveHotkey: TMenuItem;
    mniN1: TMenuItem;
    mniProperties: TMenuItem;
    procedure actEditHotkeyExecute(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actMenuItemUpdate(Sender: TObject);
    procedure actRemoveHotkeyExecute(Sender: TObject);
    procedure cbHotKeyClick(Sender: TObject);
    procedure edtHotkeyChange(Sender: TObject);
  private
    { Private declarations }
    FUpdating: Boolean;
    procedure LoadGlyphs;
    procedure SaveInHotkeyItemList(const ATree: TBaseVirtualTree;const AItemList: TBaseItemsList);
    { The desktop changed a hotkey: reload the editors without re-registering. }
    procedure HotkeysUpdated(Sender: TObject);
  strict protected
    function GetTitle: string; override;
    function GetImageIndex: Integer; override;
    function InternalLoadData: Boolean; override;
    function InternalSaveData: Boolean; override;
  public
    { Public declarations }
    destructor Destroy; override;
  end;

var
  frmHotkeyOptionsPage: TfrmHotkeyOptionsPage;

implementation

uses
  AppConfig.Main, VirtualTree.Methods, NodeDataTypes.Custom, VirtualTrees.Types,
  ShortcutGrabber, DataModules.Icons, Kernel.ResourceStrings,
  LCLProc, Kernel.Consts, Kernel.Manager, Utility.Misc, Kernel.Instance;

{$R *.lfm}

{ TfrmHotkeyOptionsPage }

destructor TfrmHotkeyOptionsPage.Destroy;
begin
  if Assigned(Config) then
    Config.OnHotkeysUpdated := nil;
  inherited Destroy;
end;

procedure TfrmHotkeyOptionsPage.HotkeysUpdated(Sender: TObject);
begin
  if not Visible then
    Exit;

  FUpdating := True;
  try
    edtHotkeyMF.Hotkey := TextToShortCut(Config.WindowHotKey);
    edtHotkeyGM.Hotkey := TextToShortCut(Config.GraphicMenuHotkey);
    edtHotkeyCM.Hotkey := TextToShortCut(Config.ClassicMenuHotkey);
  finally
    FUpdating := False;
  end;

  //The item shortcuts were updated in place; just repaint the list.
  vstItems.Repaint;
end;

procedure TfrmHotkeyOptionsPage.cbHotKeyClick(Sender: TObject);
begin
  edtHotkeyMF.Enabled := cbHotKey.Checked;
  edtHotkeyGM.Enabled := cbHotKey.Checked;
  edtHotkeyCM.Enabled := cbHotKey.Checked;
end;

procedure TfrmHotkeyOptionsPage.actEditHotkeyExecute(Sender: TObject);
var
  NewHotkey: TShortCut;
  NodeData: TvCustomRealNodeData;
begin
  if Assigned(vstItems.FocusedNode) then
  begin
    NodeData := TvCustomRealNodeData(TVirtualTreeMethods.GetNodeItemData(vstItems.FocusedNode, vstItems));
    if Assigned(NodeData) then
    begin
      NewHotkey := TfrmShortcutGrabber.Execute(Self, NodeData.Hotkey);
      if (NewHotkey <> 0) then
      begin
        NodeData.Hotkey  := NewHotkey;
        NodeData.Changed := True;

        //Avoid duplicates with the launcher hotkeys
        if (edtHotkeyMF.Hotkey = NewHotkey) then
          edtHotkeyMF.Hotkey := 0;

        if (edtHotkeyGM.Hotkey = NewHotkey) then
          edtHotkeyGM.Hotkey := 0;

        if (edtHotkeyCM.Hotkey = NewHotkey) then
          edtHotkeyCM.Hotkey := 0;
      end;
    end;
  end;
end;

procedure TfrmHotkeyOptionsPage.actPropertiesExecute(Sender: TObject);
begin
  TVirtualTreeMethods.ShowItemProperty(Self, vstItems, vstItems.FocusedNode, False);
end;

procedure TfrmHotkeyOptionsPage.actMenuItemUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := TVirtualTreeMethods.HasSelectedNodes(vstItems);
end;

procedure TfrmHotkeyOptionsPage.actRemoveHotkeyExecute(Sender: TObject);
begin
  if AskUserWarningMessage(msgConfirm, []) and Assigned(vstItems.FocusedNode) then
    vstItems.IsVisible[vstItems.FocusedNode] := False;
end;

procedure TfrmHotkeyOptionsPage.edtHotkeyChange(Sender: TObject);
var
  edtHotkey: THotKeyEdit;
begin
  if FUpdating or (not (Sender is THotKeyEdit)) then
    Exit;

  edtHotkey := THotKeyEdit(Sender);
  if edtHotkey.Hotkey = 0 then
    Exit;

  //Avoid duplicates between the three launcher hotkeys
  if (Sender <> edtHotkeyMF) and (edtHotkeyMF.Hotkey = edtHotkey.Hotkey) then
    edtHotkeyMF.Hotkey := 0;

  if (Sender <> edtHotkeyGM) and (edtHotkeyGM.Hotkey = edtHotkey.Hotkey) then
    edtHotkeyGM.Hotkey := 0;

  if (Sender <> edtHotkeyCM) and (edtHotkeyCM.Hotkey = edtHotkey.Hotkey) then
    edtHotkeyCM.Hotkey := 0;
end;

function TfrmHotkeyOptionsPage.GetImageIndex: Integer;
begin
  Result := ASuiteManager.IconsManager.GetIconIndex('hotkey');
end;

function TfrmHotkeyOptionsPage.GetTitle: string;
begin
  Result := msgHotkey;
end;

function TfrmHotkeyOptionsPage.InternalLoadData: Boolean;
begin
  Result := inherited;
  ASuiteInstance.VSTEvents.SetupVSTHotkey(vstItems);

  //Hot Keys
  cbHotKey.Checked := Config.HotKey;

  FUpdating := True;
  try
    edtHotkeyMF.Hotkey := TextToShortCut(Config.WindowHotKey);
    edtHotkeyGM.Hotkey := TextToShortCut(Config.GraphicMenuHotkey);
    edtHotkeyCM.Hotkey := TextToShortCut(Config.ClassicMenuHotkey);
  finally
    FUpdating := False;
  end;

  //Reload the editors if the desktop changes a hotkey while we are open
  Config.OnHotkeysUpdated := HotkeysUpdated;

  //Populate VST with HotKeyItemList's items
  TVirtualTreeMethods.PopulateVSTItemList(vstItems, ASuiteManager.ListManager.HotKeyItemList);
  vstItems.SortTree(0, VirtualTrees.Types.sdAscending);

  //Enable/disable visual components
  cbHotKeyClick(Self);
  LoadGlyphs;

  //Hide caret in hotkey control
  //HideCaret(edtHotkeyMF.Handle);
  //HideCaret(edtHotkeyGM.Handle);
  //HideCaret(edtHotkeyCM.Handle);

  edtHotkeyMF.Color := edtHotkeyMF.Color;
  edtHotkeyGM.Color := edtHotkeyGM.Color;
  edtHotkeyCM.Color := edtHotkeyCM.Color;
end;

function TfrmHotkeyOptionsPage.InternalSaveData: Boolean;
begin
  Result := inherited;

  //Hot Keys
  Config.HotKey       := cbHotKey.Checked;
  Config.WindowHotKey := ShortCutToText(edtHotkeyMF.Hotkey);
  Config.GraphicMenuHotkey := ShortCutToText(edtHotkeyGM.Hotkey);
  Config.ClassicMenuHotkey := ShortCutToText(edtHotkeyCM.Hotkey);

  //Save vst items in HotKeyItemList
  SaveInHotkeyItemList(vstItems, ASuiteManager.ListManager.HotKeyItemList);
end;

procedure TfrmHotkeyOptionsPage.LoadGlyphs;
begin
  edtHotkeyMF.RightButton.Images := dmImages.ilIcons;
  edtHotkeyMF.RightButton.ImagesWidth := ICON_SIZE_SMALL;
  edtHotkeyMF.ClearImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_delete');
  edtHotkeyMF.ChooseImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_add');

  edtHotkeyGM.RightButton.Images := dmImages.ilIcons;
  edtHotkeyGM.RightButton.ImagesWidth := ICON_SIZE_SMALL;
  edtHotkeyGM.ClearImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_delete');
  edtHotkeyGM.ChooseImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_add');

  edtHotkeyCM.RightButton.Images := dmImages.ilIcons;
  edtHotkeyCM.RightButton.ImagesWidth := ICON_SIZE_SMALL;
  edtHotkeyCM.ClearImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_delete');
  edtHotkeyCM.ChooseImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_add');

  pmHotkey.Images := dmImages.ilIcons;
  pmHotkey.ImagesWidth := ICON_SIZE_SMALL;

  mniRemoveHotkey.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('hotkey_delete');
  mniEditHotkey.ImageIndex   := ASuiteManager.IconsManager.GetIconIndex('hotkey_edit');
  mniProperties.ImageIndex   := ASuiteManager.IconsManager.GetIconIndex('property');
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
    NodeData := TvCustomRealNodeData(TVirtualTreeMethods.GetNodeItemData(Node, ATree));
    if Assigned(NodeData) then
    begin
      if ATree.IsVisible[Node] then
        AItemList.AddItem(NodeData)
      else
        AItemList.RemoveItem(NodeData);
      NodeData.Changed := True;
    end;
    Node := ATree.GetNext(Node);
  end;
end;

end.
