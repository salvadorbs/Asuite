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

unit DataModules.TrayMenu;

{$MODE DelphiUnicode}

{$I ASuite.inc}

{$IFDEF LCLGTK3}
  {$LINKLIB libgdk-3.so.0}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, VirtualTrees, LazFileUtils, Process,
  Kernel.ASMenuItem, Lists.Base, Kernel.Enumerations
  {$IFDEF LINUX}
  , x, xlib

    {$IFDEF LCLGTK2}
  , gdk2, gdk2x
    {$ENDIF}

    {$IFDEF LCLGTK3}
  , LazGdk3, LazGLib2
    {$ENDIF}

    {$IFDEF QT}
  , qt5
    {$ENDIF}
  {$ENDIF};

type

  { TdmTrayMenu }

  TdmTrayMenu = class(TDataModule)
    tiTrayMenu: TTrayIcon;
    pmTrayicon: TPopupMenu;
    procedure DataModuleCreate(Sender: TObject);
    procedure tiTrayMenuDblClick(Sender: TObject);
    procedure ShowMainForm(Sender: TObject);
    procedure EjectDialog(Sender: TObject);
    procedure OpenFile(Sender: TObject);
    procedure tiTrayMenuMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    function GetTextWidth(const AText: string; AFont: Graphics.TFont): Integer;
    procedure CreateAndAddSeparator(var Menu: TPopUpMenu;Text: String = '');
  public
    { Public declarations }
    procedure ShowClassicMenu;
    procedure ShowGraphicMenu;
    procedure DoDrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      ACaption: string = '');
    function CreateSeparator(Menu: TPopupMenu;Text: String = ''): TMenuItem;
    procedure RefreshClassicMenu;

    //These functions come from the Tomboy-NG project https://github.com/tomboy-notes/tomboy-ng
    class function CheckGnomeExtras: Boolean;
    class function CheckSysTray: Boolean;
  end;

{$IFDEF LCLGTK3}
function gdk_x11_window_get_xid(AX11Window: PGdkWindow): guint32; cdecl; external;
function gdk_x11_display_get_xdisplay(AX11Display: PGdkDisplay): PDisplay; cdecl; external;
{$ENDIF}

type
  TColorQuad = record
    Red, Green, Blue, Alpha: Byte;
  end;

const
  FadeLineWidth = 32;

var
  dmTrayMenu: TdmTrayMenu;

implementation

uses
  DataModules.Icons, Forms.Main, AppConfig.Main, VirtualTree.Methods,
  Utility.System, Forms.GraphicMenu, Kernel.Types, NodeDataTypes.Files,
  NodeDataTypes.Custom, NodeDataTypes.Base, Kernel.Consts, Kernel.Logger,
  Utility.FileFolder, Kernel.ResourceStrings, Kernel.Instance, LazVersion,
  Kernel.Manager, mormot.core.log {$IFDEF MSWINDOWS} , Windows {$ENDIF};

{$R *.lfm}

procedure TdmTrayMenu.DataModuleCreate(Sender: TObject);
begin
  pmTrayicon.Images := dmImages.ilIcons;
  pmTrayicon.ImagesWidth := ICON_SIZE_SMALL;

  tiTrayMenu.Hint := Format('%s %s (%s)',[APP_NAME, TASuiteInstance.GetASuiteVersion(True),
                                          UpperCase(ASuiteInstance.Paths.SuiteDrive)]);
  {$IFNDEF MSWINDOWS}
  tiTrayMenu.PopUpMenu := pmTrayicon;
  {$ENDIF}
end;

procedure TdmTrayMenu.tiTrayMenuDblClick(Sender: TObject);
begin
  if (Config.ActionClickLeft = tcNone) then
    ShowMainForm(Sender);
end;

procedure TdmTrayMenu.ShowMainForm(Sender: TObject);
begin
  frmMain.ShowMainForm(Sender);
end;

procedure TdmTrayMenu.ShowPopupMenu(const APopupMenu: TPopupMenu);
var
  CursorPoint: TPoint;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TdmTrayMenu.ShowPopupMenu', Self);
  //Get Mouse coordinates
  GetCursorPos(CursorPoint);
  SetForegroundWindow(Application.Handle);
  Application.ProcessMessages;
  APopupMenu.AutoPopup := False;
  APopupMenu.PopupComponent := tiTrayMenu.Owner;
  //Show menu
  APopupMenu.Popup(CursorPoint.X, CursorPoint.Y);
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
        NMI.Path        := AppendPathDelim(sPath + SR.Name);
        NMI.ImageIndex  := ASuiteManager.IconsManager.GetPathIconIndex(ASuiteInstance.Paths.RelativeToAbsolute(CONST_PATH_FOLDERICON)); // folder image
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
      NMI.ImageIndex  := ASuiteManager.IconsManager.GetPathIconIndex(sPath + SR.Name);
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
  //Workaround for Lazarus bug https://bugs.freepascal.org/view.php?id=38849
  //QT5Trayicon's contextMenu broken after recreating popup's handle
  //So hide trayicon before PopUpMethod and after it, show again
  {$IFDEF QT}
  {$IF laz_fullversion<2020000}
  tiTrayMenu.Hide;
  {$ENDIF}
  {$ENDIF}

  //Populate classic menu at runtime
  {$IFDEF MSWINDOWS}
  UpdateClassicMenu(pmTrayicon);
  {$ENDIF}

  //Show classic menu
  ShowPopupMenu(pmTrayicon);

  {$IFDEF QT}
  {$IF laz_fullversion<2020000}
  tiTrayMenu.Show;
  {$ENDIF}
  {$ENDIF}
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
  try
    if Assigned(Node) then
    begin
      NodeData := TVirtualTreeMethods.GetNodeDataEx(Node, Sender);
      ItemNodeData := NodeData.Data;

      //Set MenuItem properties
      if (ItemNodeData.IsSeparatorItem) then
        MenuItem := TASMenuItem(CreateSeparator(pmTrayicon, ItemNodeData.Name))
      else begin
        MenuItem := TASMenuItem.Create(Application.MainForm);

        MenuItem.Caption    := ItemNodeData.Name;
        MenuItem.ImageIndex := ItemNodeData.Icon.ImageIndex;
        if (ItemNodeData.IsFileItem) then
        begin
          MenuItem.OnClick  := RunFromTrayMenu;
          TvFileNodeData(ItemNodeData).CheckPathFile;

          //If it is a Directory, add in Trayicon Menu its subfolders and its subfiles
          if Config.AutoExpansionFolder then
          begin
            if IsDirectory(TvFileNodeData(ItemNodeData).PathAbsoluteFile) then
            begin
              {$IFNDEF GTK}
              MenuItem.OnClick := PopulateDirectory;
              MenuItem.Path    := (TvFileNodeData(ItemNodeData)).PathAbsoluteFile;
              AddSub(MenuItem);
              {$ENDIF}
            end;
          end;
        end
        else begin
          if ItemNodeData.IsCategoryItem then
          begin
            {$IFDEF GTK}
            PopulateCategoryItems(MenuItem);
            {$ELSE}
            if Sender.HasChildren[Node] then
            begin
              MenuItem.OnClick := PopulateCategoryItems;
              AddSub(MenuItem);
            end;
            {$ENDIF}
          end;
        end;
      end;

      if Assigned(MenuItem) then
      begin
        MenuItem.Data    := ItemNodeData;
        MenuItem.pNode   := ItemNodeData.pNode;
        MenuItem.Visible := not(ItemNodeData.HideFromMenu);

        //Set NodeData's MenuItem
        NodeData.MenuItem   := MenuItem;

        if (Node.Parent <> Sender.RootNode) then
        begin
          ParentNodeData := TVirtualTreeMethods.GetNodeDataEx(Node.Parent, Sender);
          AddItem(ParentNodeData.MenuItem, MenuItem);
        end
        else
          AddItem(pmTrayicon.Items, MenuItem);
      end;
    end;
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
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
var
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TdmTrayMenu.UpdateClassicMenu', Self);

  try
    Menu.Items.Clear;
    //Create MenuItems's TrayMenu
    //Header
    CreateHeaderItems(Menu);
    //MFU
    if (Config.MFU) and (ASuiteManager.ListManager.MFUList.Count > 0) then
    begin
      if Config.SubMenuMFU then
      begin
        CreateAndAddSeparator(Menu);
        CreateSpecialList(Menu, ASuiteManager.ListManager.MFUList, Config.MFUNumber, msgLongMFU);
      end
      else begin
        CreateAndAddSeparator(Menu, msgLongMFU);
        CreateSpecialList(Menu, ASuiteManager.ListManager.MFUList, Config.MFUNumber);
      end;
    end;
    CreateAndAddSeparator(Menu, msgList);
    //List
    PopulateCategoryItems(nil);
    //MRU
    if (Config.MRU) and (ASuiteManager.ListManager.MRUList.Count > 0) then
    begin
      if Config.SubMenuMRU then
      begin
        CreateAndAddSeparator(Menu);
        CreateSpecialList(Menu, ASuiteManager.ListManager.MRUList, Config.MRUNumber, msgLongMRU);
      end
      else begin
        CreateAndAddSeparator(Menu, msgLongMRU);
        CreateSpecialList(Menu, ASuiteManager.ListManager.MRUList, Config.MRUNumber,'');
      end;
    end;
    CreateAndAddSeparator(Menu);
    //Footer
    CreateFooterItems(Menu);
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
  end;
end;

procedure TdmTrayMenu.CreateHeaderItems(Menu: TPopupMenu);
var
  I : Integer;
  MenuItem : TMenuItem;
begin
  try
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
            MenuItem.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('asuite');
            MenuItem.OnClick := ShowMainForm;
            MenuItem.Default := true;
          end;
        1:
          begin
            MenuItem.Caption := msgOpenOptions;
            MenuItem.ImageIndex := ASuiteManager.IconsManager.GetIconIndex('options');
            MenuItem.OnClick := frmMain.miOptionsClick;
          end;
      end;
    end;
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
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
  TargetItem.Add(AMenuItem);
end;

procedure TdmTrayMenu.CreateFooterItems(Menu: TPopupMenu);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  try
    //Create footer menu items and set its properties
    //Menu Items: Safely remove hardware, Exit
    for I := 0 to 1 do
    begin
      MenuItem := TMenuItem.Create(Application.MainForm);
      Menu.Items.Add(MenuItem);
      case I of
        0:
          begin
            {$IFDEF UNIX}
            MenuItem.Visible := False;
            {$ENDIF}
            MenuItem.Visible := not(Config.CMHideEjectMenuItem);
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
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
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

function TdmTrayMenu.GetTextWidth(const AText: string; AFont: Graphics.TFont
  ): Integer;
var
  bmp: Graphics.TBitmap; // Graphics.TBitmap, not Windows.TBitmap
begin
  Result := 0;
  bmp := Graphics.TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(AFont);
    Result := bmp.Canvas.TextWidth(AText);
  finally
    bmp.Free;
  end;
end;

procedure TdmTrayMenu.DoDrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  ACaption: string);

  function RectWidth(const ARect: TRect): Integer;
  begin
    Result := ARect.Right - ARect.Left;
  end;

var
  TextArea, LineArea: TRect;
  LineCaption : string;  
  TextSpace, CaptionLineItemHeight: Cardinal;
begin
  try
    CaptionLineItemHeight := 0;
    LineArea := ARect;
    TextArea := LineArea;
    Dec(TextArea.Bottom, 1);
    TextSpace := 0;
    if (Sender is TMenuItem) then
    begin
      CaptionLineItemHeight := ASuiteInstance.SmallHeightNode - 4;
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
        CaptionLineItemHeight := ASuiteInstance.SmallHeightNode;
        if ACaption <> '' then
        begin
          LineCaption := Format(' %s ', [ACaption]);
          CaptionLineItemHeight := ACanvas.TextHeight(ACaption);
          TextSpace   := 1;
        end;
      end;
    TextArea.Width := GetTextWidth(LineCaption, ACanvas.Font);
    OffsetRect(TextArea, Round((RectWidth(LineArea) - RectWidth(TextArea)) / 2 - TextSpace), 0);
    Inc(ARect.Top, (CaptionLineItemHeight div 2) + 1);
    //Create first line
    Inc(ARect.Top);
    Inc(ARect.Bottom);
    DrawFadeLine(ACanvas, TextArea, ARect, clBtnShadow, FadeLineWidth, True);
    //Create second line
    Inc(ARect.Top);
    Inc(ARect.Bottom);
    DrawFadeLine(ACanvas, TextArea, ARect, clBtnHighlight, FadeLineWidth, True);
    ACanvas.TextOut(TextArea.Left, TextArea.Top, LineCaption);
  except
    on E : Exception do
      TASuiteLogger.Exception(E);
  end;
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
  I, AToDiv2, ATo, AFrom, ATop: Integer;
  //, R1, G1, B1, R2, G2, B2: Integer;
  //C: TColor;
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
  //R1 := TColorQuad(AColor).Red;
  //G1 := TColorQuad(AColor).Green;
  //B1 := TColorQuad(AColor).Blue;
  for I := ALineRect.Left to ATo do
  begin
    //if I < (ALineRect.Left + AFadeWidth) then
    //begin
    //  C := ACanvas.Pixels[I, ATop];
    //  R2 := TColorQuad(C).Red;
    //  G2 := TColorQuad(C).Green;
    //  B2 := TColorQuad(C).Blue;
    //  R2 := R2 + (((R1 - R2) * (I - ALineRect.Left)) div AFadeWidth);
    //  G2 := G2 + (((G1 - G2) * (I - ALineRect.Left)) div AFadeWidth);
    //  B2 := B2 + (((B1 - B2) * (I - ALineRect.Left)) div AFadeWidth);
    //  C := RGB(R2, G2, B2, 0);
    //  ACanvas.Pixels[I, ATop] := C;
    //end else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
  for I := AFrom to ALineRect.Right do
  begin
    //if I > (ALineRect.Right - AFadeWidth) then
    //begin
    //  C := ACanvas.Pixels[I, ATop];
    //  R2 := TColorQuad(C).Red;
    //  G2 := TColorQuad(C).Green;
    //  B2 := TColorQuad(C).Blue;
    //  R2 := R2 + (((R1 - R2) * (ALineRect.Right - I)) div AFadeWidth);
    //  G2 := G2 + (((G1 - G2) * (ALineRect.Right - I)) div AFadeWidth);
    //  B2 := B2 + (((B1 - B2) * (ALineRect.Right - I)) div AFadeWidth);
    //  C := RGB(R2, G2, B2, 0);
    //  ACanvas.Pixels[I, ATop] := C;
    //end else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
end;

procedure TdmTrayMenu.EjectDialog(Sender: TObject);
begin
  Utility.System.EjectDialog(Sender);
end;

function TdmTrayMenu.CreateSeparator(Menu: TPopupMenu; Text: String): TMenuItem;
begin
  Result := nil;

  try
    //If last MenuItem is a captioned separator (two separators in succession are useless)
    if IsCaptionedSeparator(Menu.Items[Menu.Items.Count - 1]) then
    begin
      //Then change last MenuItem.Hint to Text value
      Menu.Items[Menu.Items.Count - 1].Hint := Text;
    end
    else begin
      Result := TASMenuItem.Create(Application.MainForm);

      Result.Enabled    := False;
      Result.Caption    := '-';
      Result.Hint       := Text;

      {$IFDEF MSWINDOWS}
      //OnDrawItem is a windows-only feature
      Result.OnMeasureItem := MeasureCaptionedSeparator;
      Result.OnDrawItem := DrawCaptionedSeparator;
      {$ENDIF}
    end;
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
  end;
end;

procedure TdmTrayMenu.RefreshClassicMenu;
begin
  UpdateClassicMenu(pmTrayicon);
end;

class function TdmTrayMenu.CheckGnomeExtras: Boolean;
var
  H : TLibHandle;

  function FindInStringList(aList: TStringList; aString: String): Integer;
  var
    I: Integer = 0;
  begin
    while I < aList.Count do
    begin
      if pos(aString, AList.Strings[I]) > 0 then
        Exit(I);

      Inc(I);
    end;

    Result := -1;
  end;

  function CheckPlugIn(PlugInName : string) : boolean;
  var
    AProcess: TProcess;
    List : TStringList = nil;
  begin
    result := false;

    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable:= 'gnome-extensions';
      AProcess.Parameters.Add('info');
      AProcess.Parameters.Add(PlugInName);

      AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];

      AProcess.Execute;

      if (AProcess.ExitStatus = 0) then
      begin
        List := TStringList.Create;
        List.LoadFromStream(AProcess.Output);

        if FindInStringList(List, 'ENABLED') > -1 then
          result := true;
      end;
    finally
      freeandnil(List);
      freeandnil(AProcess);
    end;
  end;

begin
  Result := false;

  H := LoadLibrary('libappindicator3.so.1');
  if H = NilHandle then
  begin
    TASuiteLogger.Info('Failed to Find libappindicator3, SysTray may not work.', []);
    exit(False);
  end;
  unloadLibrary(H);

  if CheckPlugIn('ubuntu-appindicators@ubuntu.com') or            // Ubuntu, Debian
     CheckPlugIn('appindicatorsupport@rgcjonas.gmail.com') then  // Fedora
    Result := True;

  if not Result then
    TASuiteLogger.Info('Failed to Find an enabled appindicator plugin, SysTray may not work.', []);
end;

class function TdmTrayMenu.CheckSysTray: Boolean;
{$IFDEF LINUX}
var
  A : TAtom;
  XDisplay: PDisplay;
{$ENDIF}
begin
  {$IFDEF LINUX}
    {$IFDEF LCLGTK2}
  XDisplay := gdk_display;
    {$ENDIF}

    {$IFDEF LCLQT5}
  XDisplay := QX11Info_display;
    {$ENDIF}

    {$IFDEF LCLGTK3}
  XDisplay := gdk_x11_display_get_xdisplay(gdk_window_get_display(gdk_get_default_root_window));
    {$ENDIF}

    A := XInternAtom(XDisplay, '_NET_SYSTEM_TRAY_S0', False);
    Result := (XGetSelectionOwner(XDisplay, A) <> 0);

  if not Result then
    Result := TdmTrayMenu.CheckGnomeExtras; // Thats libappindicator3 and an installed and enabled gnome-shell-extension-appindicator
  {$ELSE}
  //Windows is always true!
  Result := True;
  {$ENDIF}
end;

procedure TdmTrayMenu.CreateSpecialList(Menu: TPopupMenu;SList: TBaseItemsList;
                                        MaxItems: Integer; SubMenuCaption: String);
var
  ParentMenuItem : TMenuItem;
begin
  try
    if SubMenuCaption <> '' then
    begin
      //Yes submenu
      ParentMenuItem := TASMenuItem.Create(Application.MainForm);
      ParentMenuItem.Caption := SubMenuCaption;
      Menu.Items.Add(ParentMenuItem);
    end
    else begin
      //No submenu
      ParentMenuItem := Menu.Items[0].Parent;
    end;

    if Assigned(ParentMenuItem) then
      UpdateSpecialList(ParentMenuItem, SList, MaxItems);  
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
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
  Height := Round((Screen.PixelsPerInch / 96.0) * CAPTION_LINE_ITEM_HEIGHT);
end;

procedure TdmTrayMenu.PopulateCategoryItems(Sender: TObject);
var
  Node: PVirtualNode;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TdmTrayMenu.PopulateCategoryItems', Self);

  try
    //Get first node
    if Assigned(Sender) then
    begin
      Node := ASuiteInstance.MainTree.GetFirstChild(TASMenuItem(Sender).pNode);
      if TASMenuItem(Sender).Count > 0 then
        TASMenuItem(Sender).Items[0].Visible := False;
    end
    else
      Node := ASuiteInstance.MainTree.GetFirst;

    try
      //Iterate time (only child level)
      while Assigned(Node) do
      begin
        CreateListItems(ASuiteInstance.MainTree, Node);

        Node := ASuiteInstance.MainTree.GetNextSibling(Node);
      end;
    finally
      if Assigned(Sender) then
        TASMenuItem(Sender).OnClick := nil;
    end;    
  except
    on E: Exception do
      TASuiteLogger.Exception(E);
  end;
end;

procedure TdmTrayMenu.PopulateDirectory(Sender: TObject);
var
  MI: TASMenuItem;
  {%H-}log: ISynLog;
begin
  log := TASuiteLogger.Enter('TdmTrayMenu.PopulateDirectory', Self);

  MI := TASMenuItem(Sender);
  try
    MI.Path := AppendPathDelim(MI.Path);
    { first directories }
    SearchAddDirectory(MI);
    { then files }
    SearchAddFiles(MI);

    //A Folder empty will have 3 child items (folder empty, "Open this folder" and a separator)
    //Change visibile property to false for last child (separator)
    if (MI.Count = 3) and (MI.Items[MI.Count - 1].IsLine) then
      MI.Items[MI.Count - 1].Visible := False;
  finally
    MI.OnClick := nil;
  end;
end;

procedure TdmTrayMenu.RunFromTrayMenu(Sender: TObject);
begin
  TVirtualTreeMethods.ExecuteNode(ASuiteInstance.MainTree, TASMenuItem(Sender).pNode, rmNormal, False);
end;

procedure TdmTrayMenu.OpenFile(Sender: TObject);
begin
  RunFile(TASMenuItem(Sender).Path);
end;

procedure TdmTrayMenu.tiTrayMenuMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TdmTrayMenu.CreateAndAddSeparator(var Menu: TPopUpMenu; Text: String);
var
  MenuItem: TMenuItem;
begin
  MenuItem := CreateSeparator(Menu, Text);
  if Assigned(MenuItem) then
    Menu.Items.Add(MenuItem);
end;

end.
