{
Copyright (C) 2006-2013 Matteo Salvi

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

unit udClassicMenu;

{$I ASuite.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, VirtualTrees, ulCommonClasses, ShellApi, Vcl.ImgList,
  Winapi.Messages, ulNodeDataTypes;

type
  TClassicMenu = class(TDataModule)
    tiTrayMenu: TTrayIcon;
    pmTrayicon: TPopupMenu;
    procedure DataModuleCreate(Sender: TObject);
    procedure tiTrayMenuMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tiTrayMenuDblClick(Sender: TObject);
    procedure ShowMainForm(Sender: TObject);
    procedure RunFromTrayMenu(Sender: TObject);
    procedure EjectDialog(Sender: TObject);
    procedure OpenFile(Sender: TObject);
    procedure GetItemsIcons(Sender: TObject);
  private
    { Private declarations }
    procedure CreateListItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                      Data: Pointer; var Abort: Boolean);
    procedure UpdateClassicMenu(Menu: TPopUpMenu);
    procedure CreateHeaderItems(Menu: TPopupMenu);
    procedure CreateFooterItems(Menu: TPopupMenu);
    procedure UpdateSpecialList(PopupMenu: TMenuItem;SList: TNodeDataList;MaxItems: Integer);
    procedure MeasureCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);
    procedure CreateSeparator(Menu: TPopupMenu;Text: String;ListMenuItem: TMenuItem = nil);
    function  IsCaptionedSeparator(MenuItem: TMenuItem): Boolean;
    procedure CreateSpecialList(Menu: TPopupMenu; SList: TNodeDataList;
                               MaxItems: Integer; SubMenuCaption: String);
    procedure AddSub(MI: TMenuItem);
    procedure AddItem(TargetItem, AMenuItem: TMenuItem);
    procedure PopulateDirectory(Sender: TObject);
    procedure DrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
  public
    { Public declarations }
    procedure ShowClassicMenu;
    procedure ShowGraphicMenu;
    procedure ShowTrayiconMenu; //In based of config, open ClassicMenu or GraphicMenu
    procedure DoDrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      ACaption: string = '');
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
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FDefault: Boolean;
    FAutoHotkeys: TMenuItemAutoFlag;
    FAutoLineReduction: TMenuItemAutoFlag;
    FRadioItem: Boolean;
    FVisible: Boolean;
    FGroupIndex: Byte;
    FImageIndex: TImageIndex;
    FActionLink: TMenuActionLink;
    FBreak: TMenuBreak;
    FBitmap: TBitmap;
    FCommand: Word;
    FHelpContext: THelpContext;
    FHint: string;
    FItems: TList;
    FShortCut: TShortCut;
    FParent: TMenuItem;
    FMerged: TMenuItem;
    FMergedWith: TMenuItem;
    FMenu: TMenu;
    FStreamedRebuild: Boolean;
    FImageChangeLink: TChangeLink;
    FSubMenuImages: TCustomImageList;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnAdvancedDrawItem: TAdvancedMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
    FAutoCheck: Boolean;
    FHandle: TMenuHandle;
{$HINTS ON}
  end;
{$ENDIF}

const
  CaptionLineItemHeight = 14;
  FadeLineWidth = 32;

var
  ClassicMenu: TClassicMenu;
  IsTrayMenuOpen: Boolean; //Used to prevent open more than one TrayMenu

implementation

uses
  AppConfig, udImages, Main, ulEnumerations, ulAppConfig, ulTreeView, ulSysUtils,
  ulCommonUtils, GraphicMenu;

{$R *.dfm}

procedure TClassicMenu.DataModuleCreate(Sender: TObject);
begin
  tiTrayMenu.Hint   := Format('%s %s %s (%s)',[APP_NAME,VERSION_COMPLETE,
                                               VERSION_PRERELEASE,UpperCase(SUITE_DRIVE)]);
  pmTrayicon.Images := ImagesDM.IcoImages;
end;

procedure TClassicMenu.tiTrayMenuDblClick(Sender: TObject);
begin
  if Config.ActionClickLeft = 0 then
    ShowMainForm(Sender);
end;

procedure TClassicMenu.tiTrayMenuMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Config.ASuiteState = asStartUp) or (IsTrayMenuOpen) then
    Exit;
  if (Button = mbLeft) then
  begin
    case Config.ActionClickLeft of
      1: ShowMainForm(Sender);
      2: ShowTrayiconMenu;
    end;
  end
  else
    if (Button = mbRight) then
    begin
      case Config.ActionClickRight of
        1: ShowMainForm(Sender);
        2: ShowTrayiconMenu;
      end;
    end;
end;

procedure TClassicMenu.ShowMainForm(Sender: TObject);
begin
  frmMain.ShowMainForm(Sender);
end;

procedure TClassicMenu.ShowTrayiconMenu;
begin
  //In based of config, open ClassicMenu or GraphicMenu
  if Config.UseClassicMenu then
    ShowClassicMenu
  else
    ShowGraphicMenu;
end;

procedure TClassicMenu.ShowClassicMenu;
var
  Point: TPoint;
begin
  IsTrayMenuOpen := True;
  //Get Mouse coordinates
  GetCursorPos(Point);
  //Classic Menu
  SetForegroundWindow(frmMain.Handle);
  //Populate classic menu at runtime
  UpdateClassicMenu(pmTrayicon);
  //Show classic menu
  pmTrayicon.Popup(Point.X, Point.Y);
  IsTrayMenuOpen := False;
end;

procedure TClassicMenu.ShowGraphicMenu;
begin
  if frmGraphicMenu.Visible then
    frmGraphicMenu.CloseMenu
  else
    frmGraphicMenu.OpenMenu;
end;

procedure TClassicMenu.CreateListItems(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                       Data: Pointer; var Abort: Boolean);
var
  MenuItem : TASMenuItem;
  NodeData, ParentNodeData : PBaseData;
begin
  if Assigned(Node) then
  begin
    NodeData := Sender.GetNodeData(Node);
    //Create a menu item and add it in trayicon menu
    MenuItem := TASMenuItem.Create(Application.MainForm);
    if (Node.Parent <> Sender.RootNode) then
    begin
      ParentNodeData := Sender.GetNodeData(Node.Parent);
      ParentNodeData.MenuItem.Add(MenuItem);
    end
    else
      pmTrayicon.Items.Add(MenuItem);
    //Set MenuItem properties
    if (NodeData.Data.DataType = vtdtSeparator) then
      CreateSeparator(pmTrayicon,NodeData.Data.Name,MenuItem)
    else begin
      MenuItem.Caption    := NodeData.Data.Name;
      if (NodeData.Data.DataType = vtdtFile) then
      begin
        MenuItem.OnClick  := RunFromTrayMenu;
        //If it is a Directory, add in Trayicon Menu its subfolders and its subfiles
        if DirectoryExists(TvFileNodeData(NodeData.Data).PathAbsoluteExe) then
        begin
          MenuItem.OnClick := populateDirectory;
          MenuItem.Hint    := (TvFileNodeData(NodeData.Data)).PathAbsoluteExe;
          AddSub(MenuItem);
        end;
      end
      else begin
        if NodeData.Data.DataType = vtdtCategory then
          MenuItem.OnClick := GetItemsIcons;
      end;
      MenuItem.ImageIndex := NodeData.Data.ImageIndex;
    end;
    MenuItem.Data    := NodeData.Data;
    MenuItem.pNode   := NodeData.Data.pNode;
    MenuItem.Visible := not(NodeData.Data.HideFromMenu);
    //Set NodeData's MenuItem
    NodeData.MenuItem   := MenuItem;
  end;
end;

procedure TClassicMenu.RunFromTrayMenu(Sender: TObject);
var
  NodeData    : TvBaseNodeData;
  ProcessInfo : TProcessInfo;
begin
  //From menu
  if (Sender is TASMenuItem) then
  begin
    NodeData := (Sender as TASMenuItem).Data;
    //Run file
    ProcessInfo.RunMode := rmNormal;
    if Assigned(NodeData) then
      TvFileNodeData(NodeData).Execute(frmMain.vstList, ProcessInfo)
    else
      ShowMessageFmt(msgErrRun, [StringReplace((Sender as TASMenuItem).Caption, '&', '', [])],True);
  end;
end;

procedure TClassicMenu.GetItemsIcons(Sender: TObject);
var
  MenuItem  : TASMenuItem;
  I         : Integer;
begin
  if (Sender is TASMenuItem) then
  begin
    MenuItem := (Sender as TASMenuItem);
    ImagesDM.GetChildNodesIcons(frmMain.vstList, nil, MenuItem.pNode, False);
    for I := 0 to MenuItem.Count - 1 do
    begin
      if MenuItem.Items[I].ImageIndex = -1 then
        MenuItem.Items[I].ImageIndex := (MenuItem.Items[I] as TASMenuItem).Data.ImageIndex;
    end;
  end;
end;

procedure TClassicMenu.UpdateClassicMenu(Menu: TPopUpMenu);
begin
  Menu.Items.Clear;
  //Create MenuItems's TrayMenu
  //Header
  CreateHeaderItems(Menu);
  //MFU
  if (Config.MFU) and (MFUList.Count > 0) then
  begin
    if Config.SubMenuMFU then
    begin
      CreateSeparator(Menu,'');
      CreateSpecialList(Menu,MFUList,Config.MFUNumber,msgLongMFU);
    end
    else begin
      CreateSeparator(Menu,msgLongMFU);
      CreateSpecialList(Menu,MFUList,Config.MFUNumber,'');
    end;
  end;
  CreateSeparator(Menu,msgList);
  //List
  frmMain.vstList.IterateSubtree(nil, CreateListItems, nil, [], False);
  //MRU
  if (Config.MRU) and (MRUList.Count > 0) then
  begin
    if Config.SubMenuMRU then
    begin
      CreateSeparator(Menu,'');
      CreateSpecialList(Menu,MRUList,Config.MRUNumber,msgLongMRU);
    end
    else begin
      CreateSeparator(Menu,msgLongMRU);
      CreateSpecialList(Menu,MRUList,Config.MRUNumber,'');
    end;
  end;
  CreateSeparator(Menu,'');
  //Footer
  CreateFooterItems(Menu);
end;

procedure TClassicMenu.CreateHeaderItems(Menu: TPopupMenu);
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
          MenuItem.ImageIndex := IMAGE_INDEX_ASuite;
          MenuItem.OnClick := ShowMainForm;
          MenuItem.Default := true;
        end;
      1:
        begin
          MenuItem.Caption := msgOpenOptions;
          MenuItem.ImageIndex := IMAGE_INDEX_Options;
          MenuItem.OnClick := frmMain.miOptionsClick;
          MenuItem.Enabled := Not(Config.ReadOnlyMode);
        end;
    end;
  end;
end;

procedure TClassicMenu.AddSub(MI: TMenuItem);
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

procedure TClassicMenu.AddItem(TargetItem, AMenuItem: TMenuItem);
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

procedure TClassicMenu.CreateFooterItems(Menu: TPopupMenu);
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

procedure TClassicMenu.UpdateSpecialList(PopupMenu: TMenuItem;SList: TNodeDataList;MaxItems: Integer);
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
        if Assigned(NodeData) and (NodeData.ImageIndex = -1) then
          NodeData.ImageIndex := ImagesDM.GetIconIndex(NodeData);
        MenuItem.ImageIndex := NodeData.ImageIndex;
        MenuItem.Data       := NodeData;
        MenuItem.OnClick    := ClassicMenu.RunFromTrayMenu;
        PopupMenu.Add(MenuItem);
      end
      else
        SList.Delete(I);
    end;
  end;
end;

procedure TClassicMenu.DrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
begin
  DoDrawCaptionedSeparator(Sender, ACanvas, ARect);
end;

procedure TClassicMenu.DoDrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  ACaption: string);

  function RectWidth(const ARect: TRect): Integer;
  begin
    Result := ARect.Right - ARect.Left;
  end;

var
  TextArea, LineArea: TRect;
  Flags: Longint;
  LineCaption : string;  
  TextSpace   : Cardinal;
begin
  LineArea := ARect;
  TextArea := LineArea;
  Dec(TextArea.Bottom, 1);
  Flags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_CENTER;
  TextSpace := 0;
  if (Sender is TMenuItem) then
  begin
    //Don't highlight menu item
    ACanvas.Brush.Color := clMenu;
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
      ACanvas.Font.Assign((Sender as TBaseVirtualTree).Font);
      if ACaption <> '' then
      begin
        LineCaption := Format(' %s ', [ACaption]);
        TextSpace   := 1;
      end;
    end;
  DrawText(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags or DT_CALCRECT);
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
  DrawText(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags);
end;

procedure TClassicMenu.DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);

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

procedure TClassicMenu.EjectDialog(Sender: TObject);
begin
  ulSysUtils.EjectDialog(Sender);
end;

procedure TClassicMenu.CreateSeparator(Menu: TPopupMenu;Text: String;ListMenuItem: TMenuItem = nil);
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
    MenuItem.Hint       := Text;
    MenuItem.OnMeasureItem := MeasureCaptionedSeparator;
    MenuItem.OnDrawItem := DrawCaptionedSeparator;
  end;
end;

procedure TClassicMenu.CreateSpecialList(Menu: TPopupMenu;SList: TNodeDataList;
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

function TClassicMenu.IsCaptionedSeparator(MenuItem: TMenuItem): Boolean;
begin
  Result := (MenuItem.Name = '') and
            (MenuItem.Hint <> '') and
            (MenuItem.Enabled = false);
end;

procedure TClassicMenu.MeasureCaptionedSeparator(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
begin
  //Change separator's height
  Height := CaptionLineItemHeight + 1;
end;

procedure TClassicMenu.PopulateDirectory(Sender: TObject);

  procedure SearchAddDirectory(AMI: TMenuItem);
  var
    SR    : TSearchRec;
    Found : Boolean;
    NMI   : TMenuItem;
  begin
    Found := FindFirst(AMI.Hint + '*',faDirectory + faReadOnly + faArchive,SR) = 0;
    try
      while Found do
      begin
        if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '..') then
        begin
          if AMI.Count > 0 then
            AMI.Items[0].Visible := False;
          //Create new menuitem and add base properties
          NMI             := TMenuItem.Create(AMI);
          NMI.Caption     := SR.Name;
          NMI.Hint        := AMI.Hint + SR.Name + PathDelim;
          NMI.ImageIndex  := ImagesDM.GetSimpleIconIndex(SUITE_SMALLICONS_PATH + FILEICON_Folder); // folder image
          //Set AutoHotkeys to maManual, speed up popup menu
          NMI.AutoHotkeys := maManual;
          //If it is not '.', expand folder else add OnClick event to open folder
          if NMI.Caption <> '.' then
            NMI.OnClick := PopulateDirectory
          else
            NMI.OnClick := OpenFile;
          //Add item in traymenu
          if NMI.Caption <> '.' then
            AddSub(NMI);
          AddItem(AMI, NMI);
        end;
        //Next folder
        Found := FindNext(SR) = 0;
      end;
    finally
      FindClose(SR);
    end;
  end;

  procedure SearchAddFiles(AMI: TMenuItem);
  var
    SR: TSearchRec;
    Found: Boolean;
    NMI: TMenuItem;
  begin
    Found := FindFirst(AMI.Hint + '*',faReadOnly + faArchive,SR) = 0;
    try
      if Found then
        AMI.NewBottomLine;
      while Found do
      begin
        //Create new menuitem and add base properties
        NMI             := TMenuItem.Create(AMI);
        NMI.Caption     := SR.Name;
        NMI.Hint        := AMI.Hint + SR.Name;
        NMI.ImageIndex  := ImagesDM.GetSimpleIconIndex(AMI.Hint + SR.Name);
          //Set AutoHotkeys to maManual, speed up popup menu
        NMI.AutoHotkeys := maManual;
        NMI.OnClick     := OpenFile;
        //Add item in traymenu
        AddItem(AMI, NMI);
        //Next file
        Found := FindNext(SR) = 0;
      end;
    finally
      FindClose(SR);
    end;
  end;

var
  MI: TMenuItem;

begin
  MI := TMenuItem(Sender);
  try
    {$IFDEF FASTHACK}
    { allocate some space for the items TList. E.g. space for 4096 items should
      be enough. }
    TMenuItemPrivateHack(MI).FItems.Capacity := 4096;
    {$ENDIF}
    MI.Hint := IncludeTrailingBackslash(MI.Hint);
    { first directories }
    SearchAddDirectory(MI);
    { then files }
    SearchAddFiles(MI);
  finally
    MI.OnClick := nil;
  end;
  {$IFDEF FASTHACK}
    { because fast hack does not rebuild the handle, we use the autolinereduction
      to do that. Add extract line here so it will rebuild the handle. Otherwise
      we don't see any items in the menu.. :) }
  MI.NewBottomLine;
  {$ENDIF}
end;

procedure TClassicMenu.OpenFile(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow, 'open', PChar(TMenuItem(Sender).Hint), nil,
               PChar(ExtractFileDir(TMenuItem(Sender).Hint)), SW_SHOW);
end;

end.
