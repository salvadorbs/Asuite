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

unit DataModules.TrayMenu;

{$MODE DelphiUnicode}

{$I ASuite.inc}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, VirtualTrees, ImgList, Messages,
  Kernel.PopupMenu, Lists.Base, Kernel.Enumerations, LazMethodList;

type

  { TdmTrayMenu }

  TdmTrayMenu = class(TDataModule)
    tiTrayMenu: TTrayIcon;
    pmTrayicon: TPopupMenu;
    procedure DataModuleCreate(Sender: TObject);
    procedure tiTrayMenuMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tiTrayMenuDblClick(Sender: TObject);
    procedure ShowMainForm(Sender: TObject);
    procedure EjectDialog(Sender: TObject);
    procedure OpenFile(Sender: TObject);
  private
    { Private declarations }
    procedure CreateListItems(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure UpdateClassicMenu(Menu: TPopUpMenu);
    procedure CreateHeaderItems(Menu: TPopupMenu);
    procedure CreateFooterItems(Menu: TPopupMenu);
    procedure UpdateSpecialList(PopupMenu: TMenuItem;SList: TBaseItemsList;MaxItems: Integer);
    procedure MeasureCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);
    function  IsCaptionedSeparator(MenuItem: TMenuItem): Boolean;
    procedure CreateSpecialList(Menu: TPopupMenu; SList: TBaseItemsList;
                               MaxItems: Integer; SubMenuCaption: String = '');
    procedure AddItem(TargetItem, AMenuItem: TMenuItem);
    procedure DrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AState: LCLType.TOwnerDrawState);
    procedure DoTrayIconButtonClick(ASender: TObject; ATrayiconAction: TTrayiconActionClick);
    procedure PopulateDirectory(Sender: TObject);
    procedure SearchAddDirectory(AMI: TASMenuItem; FolderPath: string = '');
    procedure SearchAddFiles(AMI: TASMenuItem; FolderPath: string = '');
    procedure AddSub(MI: TMenuItem);
    procedure GetItemsIcons(Sender: TObject);
    procedure PopulateCategoryItems(Sender: TObject);
    procedure ShowPopupMenu(const APopupMenu: TPopupMenu);
    procedure RunFromTrayMenu(Sender: TObject);
  public
    { Public declarations }
    procedure ShowClassicMenu;
    procedure ShowGraphicMenu;
    procedure DoDrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      ACaption: string = '');
    procedure CreateSeparator(Menu: TPopupMenu;Text: String = '';ListMenuItem: TMenuItem = nil);
  end;

type
  TColorQuad = record
    Red, Green, Blue, Alpha: Byte;
  end;

{$IFDEF FASTHACK}
  { TESTED ONLY WITH DELPHI 5 ENTERPRISE. THE FIELD ORDER/SIZE MUST MATCH SO THAT
    FItems: TList AND FParent: TMenuItem ARE ACCESSIBLE. }
  TMenuItemPrivateHack = class(TComponent)
  private
{$HINTS OFF}
    FActionLink: TMenuActionLink;
    FCaption: TTranslateString;
    FBitmap: TBitmap;
    FGlyphShowMode: TGlyphShowMode;
    FHandle: HMenu;
    FHelpContext: THelpContext;
    FHint: String;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FItems: TList; // list of TMenuItem
    FMenu: TMenu;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
    FParent: TMenuItem;
    FMerged: TMenuItem;
    FMergedWith: TMenuItem;
    FMergedItems: TMergedMenuItems;
    FMenuItemHandlers: array[TMenuItemHandlerType] of TMethodList;
    FSubMenuImages: TCustomImageList;
    FSubMenuImagesWidth: Integer;
    FShortCut: TShortCut;
    FShortCutKey2: TShortCut;
    FGroupIndex: Byte;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShowAlwaysCheckable: boolean;
    FVisible: Boolean;
    FBitmapIsValid: Boolean;
    FAutoCheck: Boolean;
    FChecked: Boolean;
    FDefault: Boolean;
    FEnabled: Boolean;
{$HINTS ON}
  end;
{$ENDIF}

const
  FadeLineWidth = 32;

var
  dmTrayMenu: TdmTrayMenu;

implementation

uses
  DataModules.Icons, Forms.Main, AppConfig.Main, VirtualTree.Methods,
  Utility.System, Forms.GraphicMenu, Kernel.Types, NodeDataTypes.Files,
  NodeDataTypes.Custom, NodeDataTypes.Base, Kernel.Consts, Kernel.Logger,
  Utility.Misc, Utility.FileFolder, {$IFDEF Windows}Windows,{$ENDIF} Kernel.ResourceStrings;

{$R *.lfm}

procedure TdmTrayMenu.DataModuleCreate(Sender: TObject);
begin
  pmTrayicon.Images := dmImages.ilSmallIcons;
  tiTrayMenu.Hint := Format('%s %s (%s)',[APP_NAME, GetASuiteVersion(True),
                                          UpperCase(Config.Paths.SuiteDrive)]);
end;

procedure TdmTrayMenu.tiTrayMenuDblClick(Sender: TObject);
begin
  ShowMainForm(Sender);
end;

procedure TdmTrayMenu.tiTrayMenuMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Config.ASuiteState = lsStartUp) then
    Exit;
  case Button of
    TMouseButton.mbLeft   : DoTrayIconButtonClick(Sender, Config.ActionClickLeft);
    TMouseButton.mbMiddle : DoTrayIconButtonClick(Sender, Config.ActionClickMiddle);
    TMouseButton.mbRight  : DoTrayIconButtonClick(Sender, Config.ActionClickRight);
  end;
end;

procedure TdmTrayMenu.ShowMainForm(Sender: TObject);
begin
  frmMain.ShowMainForm(Sender);
end;

procedure TdmTrayMenu.ShowPopupMenu(const APopupMenu: TPopupMenu);
var
  CursorPoint: TPoint;
begin
  TASuiteLogger.Enter('ShowPopupMenu', Self);
  //Get Mouse coordinates
  GetCursorPos(CursorPoint);
  SetForegroundWindow(Application.Handle);
  Application.ProcessMessages;
  APopupMenu.AutoPopup := False;
  APopupMenu.PopupComponent := tiTrayMenu.Owner;
  //Show menu
  APopupMenu.Popup(CursorPoint.X, CursorPoint.Y);
  PostMessage(Application.Handle, WM_NULL, 0, 0);
end;

procedure TdmTrayMenu.SearchAddDirectory(AMI: TASMenuItem; FolderPath: string);
var
  SR    : TSearchRec;
  Found : Boolean;
  NMI   : TASMenuItem;
  sPath : string;
begin
  if AMI is TASMenuItem then
    sPath := TASMenuItem(AMI).Path
  else
    sPath := FolderPath;
  Found := FindFirst(sPath + '*',faDirectory + faReadOnly + faArchive,SR) = 0;
  try
    while Found do
    begin
      if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '..') then
      begin
        if AMI.Count > 0 then
          AMI.Items[0].Visible := False;
        //Create new menuitem and add base properties
        NMI             := TASMenuItem.Create(AMI);
        NMI.Path        := sPath + SR.Name + PathDelim;
        NMI.ImageIndex  := Config.IconsManager.GetPathIconIndex(Config.Paths.RelativeToAbsolute(CONST_PATH_FOLDERICON)); // folder image
        //Add item in traymenu
        AddItem(AMI, NMI);
        //If it is not '.', expand folder else add OnClick event to open folder
        if SR.Name <> '.' then
        begin
          NMI.Caption := SR.Name;
          NMI.OnClick := PopulateDirectory;
          AddSub(NMI);
        end
        else begin
          NMI.Caption := msgCMOpenFolder;
          NMI.OnClick := OpenFile;
          AMI.NewBottomLine;
        end;
      end;
      //Next folder
      Found := FindNext(SR) = 0;
    end;
  finally
    SysUtils.FindClose(SR);
  end;
end;

procedure TdmTrayMenu.SearchAddFiles(AMI: TASMenuItem; FolderPath: string);
var
  SR: TSearchRec;
  Found: Boolean;
  NMI: TASMenuItem;
  sPath : string;
begin
  if AMI is TASMenuItem then
    sPath := TASMenuItem(AMI).Path
  else
    sPath := FolderPath;
  Found := FindFirst(sPath + '*',faReadOnly + faArchive,SR) = 0;
  try
    if Found then
      AMI.NewBottomLine;

    while Found do
    begin
      //Create new menuitem and add base properties
      NMI             := TASMenuItem.Create(AMI);
      NMI.Caption     := SR.Name;
      NMI.Path        := sPath + SR.Name;
      NMI.ImageIndex  := Config.IconsManager.GetPathIconIndex(sPath + SR.Name);
      NMI.OnClick     := OpenFile;
      //Add item in traymenu
      AddItem(AMI, NMI);
      //Next file
      Found := FindNext(SR) = 0;
    end;
  finally
    SysUtils.FindClose(SR);
  end;
end;

procedure TdmTrayMenu.ShowClassicMenu;
begin
  TASuiteLogger.Enter('ShowClassicMenu', Self);
  //Populate classic menu at runtime
  UpdateClassicMenu(pmTrayicon);
  //Show classic menu
  ShowPopupMenu(pmTrayicon);
end;

procedure TdmTrayMenu.ShowGraphicMenu;
begin
  if Assigned(frmGraphicMenu) then
  begin
    if frmGraphicMenu.Visible then
      frmGraphicMenu.CloseMenu
    else
      frmGraphicMenu.OpenMenu;
  end;
end;

procedure TdmTrayMenu.CreateListItems(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  MenuItem : TASMenuItem;
  ItemNodeData : TvBaseNodeData;
  NodeData, ParentNodeData : PBaseData;
begin
  if Assigned(Node) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeDataEx(Node, Sender);
    ItemNodeData := NodeData.Data;
    //Create a menu item and add it in trayicon menu
    MenuItem := TASMenuItem.Create(Application.MainForm);
    if (Node.Parent <> Sender.RootNode) then
    begin
      ParentNodeData := TVirtualTreeMethods.Create.GetNodeDataEx(Node.Parent, Sender);
      AddItem(ParentNodeData.MenuItem, MenuItem);
    end
    else
      AddItem(pmTrayicon.Items, MenuItem);
    //Set MenuItem properties
    if (ItemNodeData.DataType = vtdtSeparator) then
      CreateSeparator(pmTrayicon, ItemNodeData.Name, MenuItem)
    else begin
      MenuItem.Caption    := ItemNodeData.Name;
      MenuItem.ImageIndex := ItemNodeData.Icon.ImageIndex;
      if (ItemNodeData.DataType = vtdtFile) then
      begin
        MenuItem.OnClick  := RunFromTrayMenu;
        TvFileNodeData(ItemNodeData).CheckPathFile;
        //If it is a Directory, add in Trayicon Menu its subfolders and its subfiles
        if Config.AutoExpansionFolder then
        begin
          if IsDirectory(TvFileNodeData(ItemNodeData).PathAbsoluteFile) then
          begin
            MenuItem.OnClick := PopulateDirectory;
            MenuItem.Path    := (TvFileNodeData(ItemNodeData)).PathAbsoluteFile;
            AddSub(MenuItem);
          end;
        end;
      end
      else begin
        if ItemNodeData.DataType = vtdtCategory then
        begin
          MenuItem.OnClick := PopulateCategoryItems;      
          AddSub(MenuItem);
        end;
      end;
    end;
    MenuItem.Data    := ItemNodeData;
    MenuItem.pNode   := ItemNodeData.pNode;
    MenuItem.Visible := not(ItemNodeData.HideFromMenu);
    //Set NodeData's MenuItem
    NodeData.MenuItem   := MenuItem;
  end;
end;

procedure TdmTrayMenu.GetItemsIcons(Sender: TObject);
var
  MenuItem  : TASMenuItem;
  I         : Integer;
begin
  if (Sender is TASMenuItem) then
  begin
    MenuItem := (Sender as TASMenuItem);
    for I := 0 to MenuItem.Count - 1 do
    begin
      if MenuItem.Items[I].ImageIndex = -1 then
        MenuItem.Items[I].ImageIndex := (MenuItem.Items[I] as TASMenuItem).Data.Icon.ImageIndex;
    end;
  end;
end;

procedure TdmTrayMenu.DoTrayIconButtonClick(ASender: TObject; ATrayiconAction: TTrayiconActionClick);
begin
  case ATrayiconAction of
    tcShowWindow: ShowMainForm(ASender);
    tcShowGraphicMenu: ShowGraphicMenu;
    tcShowClassicMenu: ShowClassicMenu;
  end;
end;

procedure TdmTrayMenu.UpdateClassicMenu(Menu: TPopUpMenu);
begin
  TASuiteLogger.Enter('UpdateClassicMenu', Self);

  Menu.Items.Clear;
  //Create MenuItems's TrayMenu
  //Header
  CreateHeaderItems(Menu);
  //MFU
  if (Config.MFU) and (Config.ListManager.MFUList.Count > 0) then
  begin
    if Config.SubMenuMFU then
    begin
      CreateSeparator(Menu);
      CreateSpecialList(Menu, Config.ListManager.MFUList, Config.MFUNumber, msgLongMFU);
    end
    else begin
      CreateSeparator(Menu, msgLongMFU);
      CreateSpecialList(Menu, Config.ListManager.MFUList, Config.MFUNumber);
    end;
  end;
  CreateSeparator(Menu,msgList);
  //List
  PopulateCategoryItems(nil);
  //MRU
  if (Config.MRU) and (Config.ListManager.MRUList.Count > 0) then
  begin
    if Config.SubMenuMRU then
    begin
      CreateSeparator(Menu);
      CreateSpecialList(Menu, Config.ListManager.MRUList, Config.MRUNumber, msgLongMRU);
    end
    else begin
      CreateSeparator(Menu,msgLongMRU);
      CreateSpecialList(Menu, Config.ListManager.MRUList, Config.MRUNumber,'');
    end;
  end;
  CreateSeparator(Menu);
  //Footer
  CreateFooterItems(Menu);
end;

procedure TdmTrayMenu.CreateHeaderItems(Menu: TPopupMenu);
var
  I : Integer;
  MenuItem : TMenuItem;
begin
  //Create header menu items and set its properties
  //Menu Items: Show Window, Options
  for I := 0 to 1 do
  begin
    MenuItem := TMenuItem.Create(Application.MainForm);
    Menu.Items.Add(MenuItem);
    case I of
      0:
        begin
          MenuItem.Caption := msgShowASuite;
          MenuItem.ImageIndex := Config.IconsManager.GetIconIndex('asuite');
          MenuItem.OnClick := ShowMainForm;
          MenuItem.Default := true;
        end;
      1:
        begin
          MenuItem.Caption := msgOpenOptions;
          MenuItem.ImageIndex := Config.IconsManager.GetIconIndex('options');
          MenuItem.OnClick := frmMain.miOptionsClick;
          MenuItem.Enabled := Not(Config.ReadOnlyMode);
        end;
    end;
  end;
end;

procedure TdmTrayMenu.AddSub(MI: TMenuItem);
var
  MISub: TMenuItem;
begin
  MISub := TMenuItem.Create(MI);
  with MISub do
  begin
    Caption := '(Folder empty)';
    Enabled := False;
    Hint := '';
    MI.Add(MISub);
  end;
end;

procedure TdmTrayMenu.AddItem(TargetItem, AMenuItem: TMenuItem);
begin
{$IFDEF FASTHACK}
  with TMenuItemPrivateHack(TargetItem) do
  begin
    if FItems = nil then FItems := TList.Create;
    FItems.Insert(FItems.Count, AMenuItem);
    TMenuItemPrivateHack(AMenuItem).FParent := TargetItem;
  end;
{$ELSE}
  TargetItem.Add(AMenuItem);
{$ENDIF}
end;

procedure TdmTrayMenu.CreateFooterItems(Menu: TPopupMenu);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  //Create footer menu items and set its properties
  //Menu Items: Safely remove hardware, Exit
  for I := 0 to 1 do
  begin
    MenuItem := TMenuItem.Create(Application.MainForm);
    Menu.Items.Add(MenuItem);
    case I of
      0:
        begin
          MenuItem.Caption := msgEjectHardware;
          MenuItem.OnClick := EjectDialog;
        end;
      1:
        begin
          MenuItem.Caption := msgExit;
          MenuItem.OnClick := frmMain.miExitClick;
        end;
    end;
  end;
end;

procedure TdmTrayMenu.UpdateSpecialList(PopupMenu: TMenuItem;SList: TBaseItemsList;MaxItems: Integer);
var
  NodeData : TvCustomRealNodeData;
  MenuItem : TASMenuItem;
  I, ItemCount : Integer;
begin
  //Set limit based on MaxItems or SList.Count
  if MaxItems < SList.Count then
    ItemCount := MaxItems
  else
    ItemCount := SList.Count;
  for I := 0 to ItemCount - 1 do
  begin
    if Assigned(SList[I]) then
    begin
      //Create MenuItem
      NodeData := SList[I];
      if Assigned(NodeData) then
      begin
        MenuItem := TASMenuItem.Create(Application.MainForm);
        //Set some properties
        MenuItem.Caption    := NodeData.Name;
        MenuItem.ImageIndex := NodeData.Icon.ImageIndex;
        MenuItem.Data       := NodeData;
        MenuItem.pNode      := NodeData.PNode;
        MenuItem.OnClick    := RunFromTrayMenu;
        PopupMenu.Add(MenuItem);
      end
      else
        SList.Delete(I);
    end;
  end;
end;

procedure TdmTrayMenu.DrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AState: LCLType.TOwnerDrawState);
begin
  DoDrawCaptionedSeparator(Sender, ACanvas, ARect);
end;

procedure TdmTrayMenu.DoDrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  ACaption: string);

  function RectWidth(const ARect: TRect): Integer;
  begin
    Result := ARect.Right - ARect.Left;
  end;

var
  TextArea, LineArea: TRect;
  Flags: Longint;
  LineCaption : string;  
  TextSpace, CaptionLineItemHeight: Cardinal;
begin
  CaptionLineItemHeight := 0;
  LineArea := ARect;
  TextArea := LineArea;
  Dec(TextArea.Bottom, 1);
  Flags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_CENTER;
  TextSpace := 0;
  if (Sender is TMenuItem) then
  begin
    CaptionLineItemHeight := Config.SmallHeightNode - 4;
    //Don't highlight menu item
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color  := clWindowText;
    if (Sender as TMenuItem).Hint <> '' then
    begin
      LineCaption := Format(' %s ', [(Sender as TMenuItem).Hint]);
      TextSpace   := 1;
    end;
  end
  else
    if (Sender is TBaseVirtualTree) then
    begin
      //Get Tree's Font
      ACanvas.Font.Assign(TBaseVirtualTree(Sender).Font);
      CaptionLineItemHeight := Config.SmallHeightNode;
      if ACaption <> '' then
      begin
        LineCaption := Format(' %s ', [ACaption]);
        CaptionLineItemHeight := ACanvas.TextHeight(ACaption);
        TextSpace   := 1;
      end;
    end;
  DrawTextW(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags or DT_CALCRECT);
  OffsetRect(TextArea, Round((RectWidth(LineArea) - RectWidth(TextArea)) / 2 - TextSpace), 0);
  Inc(ARect.Top, (CaptionLineItemHeight div 2) - 1);
  //Create first line
  Inc(ARect.Top);
  Inc(ARect.Bottom);
  DrawFadeLine(ACanvas, TextArea, ARect, clBtnShadow, FadeLineWidth, True);
  //Create second line
  Inc(ARect.Top);
  Inc(ARect.Bottom);
  DrawFadeLine(ACanvas, TextArea, ARect, clBtnHighlight, FadeLineWidth, True);
  DrawTextW(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags);
end;

procedure TdmTrayMenu.DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);

{
  Original from Delphi component BarMenus http://www.bluecave.net/products/barmenus/
  All Rights Reserved
}

function Min(Value1, Value2: Integer): Integer;
begin
  if Value1 > Value2 then
    Result := Value2
  else
    Result := Value1;
end;

function Max(Value1, Value2: Integer): Integer;
begin
  if Value1 < Value2 then
    Result := Value2
  else
    Result := Value1;
end;

function RGB(Red, Green, Blue: Byte; Alpha: Byte = $00): TColor;
begin
  Result := (Alpha shl 24) or (Blue shl 16) or (Green shl 8) or Red;
end;

var
  I, AToDiv2, ATo, AFrom, ATop, R1, G1, B1, R2, G2, B2: Integer;
  C: TColor;
begin
  AToDiv2 := ALineRect.Left - (ALineRect.Left - ALineRect.Right) div 2;
  ATop := Max(ALineRect.Top, AClipRect.Top);
  if AClip then
  begin
    ATo := Min(AToDiv2, AClipRect.Left) - 1;
    AFrom := Max(AToDiv2, AClipRect.Right);
  end else
  begin
    ATo := AToDiv2;
    AFrom := AToDiv2;
  end;
  AColor := ColorToRGB(AColor);
  R1 := TColorQuad(AColor).Red;
  G1 := TColorQuad(AColor).Green;
  B1 := TColorQuad(AColor).Blue;
  for I := ALineRect.Left to ATo do
  begin
    if I < (ALineRect.Left + AFadeWidth) then
    begin
      C := ACanvas.Pixels[I, ATop];
      R2 := TColorQuad(C).Red;
      G2 := TColorQuad(C).Green;
      B2 := TColorQuad(C).Blue;
      R2 := R2 + (((R1 - R2) * (I - ALineRect.Left)) div AFadeWidth);
      G2 := G2 + (((G1 - G2) * (I - ALineRect.Left)) div AFadeWidth);
      B2 := B2 + (((B1 - B2) * (I - ALineRect.Left)) div AFadeWidth);
      C := RGB(R2, G2, B2, 0);
      ACanvas.Pixels[I, ATop] := C;
    end else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
  for I := AFrom to ALineRect.Right do
  begin
    if I > (ALineRect.Right - AFadeWidth) then
    begin
      C := ACanvas.Pixels[I, ATop];
      R2 := TColorQuad(C).Red;
      G2 := TColorQuad(C).Green;
      B2 := TColorQuad(C).Blue;
      R2 := R2 + (((R1 - R2) * (ALineRect.Right - I)) div AFadeWidth);
      G2 := G2 + (((G1 - G2) * (ALineRect.Right - I)) div AFadeWidth);
      B2 := B2 + (((B1 - B2) * (ALineRect.Right - I)) div AFadeWidth);
      C := RGB(R2, G2, B2, 0);
      ACanvas.Pixels[I, ATop] := C;
    end else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
end;

procedure TdmTrayMenu.EjectDialog(Sender: TObject);
begin
  Utility.System.EjectDialog(Sender);
end;

procedure TdmTrayMenu.CreateSeparator(Menu: TPopupMenu;Text: String;ListMenuItem: TMenuItem);
var
  MenuItem: TMenuItem;
begin
  //If last MenuItem is a captioned separator (two separators in succession are useless)
  if IsCaptionedSeparator(Menu.Items[Menu.Items.Count - 1]) then
  begin
    //Then change last MenuItem.Hint to Text value
    Menu.Items[Menu.Items.Count - 1].Hint := Text;
  end
  else begin
    //Else create a new captioned separator
    if Assigned(ListMenuItem) then
      MenuItem := ListMenuItem
    else begin
      MenuItem := TMenuItem.Create(Application.MainForm);
      Menu.Items.Add(MenuItem);
    end;
    MenuItem.Enabled    := False;
    MenuItem.Caption    := '-';
    MenuItem.Hint       := Text;
    MenuItem.OnMeasureItem := MeasureCaptionedSeparator;
    MenuItem.OnDrawItem := DrawCaptionedSeparator;
  end;
end;

procedure TdmTrayMenu.CreateSpecialList(Menu: TPopupMenu;SList: TBaseItemsList;
                                        MaxItems: Integer; SubMenuCaption: String);
var
  MenuItem : TASMenuItem;
begin
  MenuItem := TASMenuItem.Create(Application.MainForm);
  Menu.Items.Add(MenuItem);
  if SubMenuCaption <> '' then
  begin
    //Yes submenu
    UpdateSpecialList(MenuItem, SList, MaxItems);
    MenuItem.Caption := SubMenuCaption;
  end
  else begin
    //No submenu
    UpdateSpecialList(Menu.Items[0].parent, SList, MaxItems);
    MenuItem.free;
  end;
end;

function TdmTrayMenu.IsCaptionedSeparator(MenuItem: TMenuItem): Boolean;
begin
  Result := (MenuItem.Name = '') and
            (MenuItem.Hint <> '') and
            (MenuItem.Enabled = false);
end;

procedure TdmTrayMenu.MeasureCaptionedSeparator(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
begin
  //Change separator's height
  Height := 15; //CaptionLineItemHeight + 1;
end;

procedure TdmTrayMenu.PopulateCategoryItems(Sender: TObject);
var
  Node: PVirtualNode;
begin
  //Get first node
  if Assigned(Sender) then
  begin
    Node := Config.MainTree.GetFirstChild(TASMenuItem(Sender).pNode);
    {$IFDEF FASTHACK}
    { allocate some space for the items TList. E.g. space for 4096 items should
      be enough. }
    TMenuItemPrivateHack(TASMenuItem(Sender)).FItems.Capacity := 4096;
    {$ENDIF}
    if TASMenuItem(Sender).Count > 0 then
      TASMenuItem(Sender).Items[0].Visible := False;
  end
  else
    Node := Config.MainTree.GetFirst;

  try
    //Iterate time (only child level)
    while Assigned(Node) do
    begin
      CreateListItems(Config.MainTree, Node);

      Node := Config.MainTree.GetNextSibling(Node);
    end;
  finally
    if Assigned(Sender) then
    begin
      TASMenuItem(Sender).OnClick := nil;
      {$IFDEF FASTHACK}
        { because fast hack does not rebuild the handle, we use the autolinereduction
          to do that. Add extract line here so it will rebuild the handle. Otherwise
          we don't see any items in the menu.. :) }
      TASMenuItem(Sender).AddSeparator;
      {$ENDIF}
    end;
  end;
end;

procedure TdmTrayMenu.PopulateDirectory(Sender: TObject);
var
  MI: TASMenuItem;
begin
  TASuiteLogger.Enter('PopulateDirectory', Self);

  MI := TASMenuItem(Sender);
  try
    {$IFDEF FASTHACK}
    { allocate some space for the items TList. E.g. space for 4096 items should
      be enough. }
    TMenuItemPrivateHack(MI).FItems.Capacity := 4096;
    {$ENDIF}
    MI.Path := IncludeTrailingBackslash(MI.Path);
    { first directories }
    SearchAddDirectory(MI);
    { then files }
    SearchAddFiles(MI);                ;

    //A Folder empty will have 3 child items (folder empty, "Open this folder" and a separator)
    //Change visibile property to false for last child (separator)
    if (MI.Count = 3) and (MI.Items[MI.Count - 1].IsLine) then
      MI.Items[MI.Count - 1].Visible := False;
  finally
    MI.OnClick := nil;
  end;
  {$IFDEF FASTHACK}
    { because fast hack does not rebuild the handle, we use the autolinereduction
      to do that. Add extract line here so it will rebuild the handle. Otherwise
      we don't see any items in the menu.. :) }
  MI.AddSeparator;
  {$ENDIF}
end;

procedure TdmTrayMenu.RunFromTrayMenu(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ExecuteNode(Config.MainTree, TASMenuItem(Sender).pNode, rmNormal, False);
end;

procedure TdmTrayMenu.OpenFile(Sender: TObject);
begin
   OpenDocument(PChar(TASMenuItem(Sender).Path));
end;

end.
