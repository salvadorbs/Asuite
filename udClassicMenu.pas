{
Copyright (C) 2006-2009 Matteo Salvi and Shannara

Website: http://www.salvadorsoftware.com/

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

unit udClassicMenu;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ExtCtrls, VirtualTrees, ulCommonClasses, ShellApi;

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
    function  UpdateSpecialList(PopupMenu: TMenuItem;SList: TNodeDataList): Integer;
    procedure DrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);
    procedure CreateSeparator(Menu: TPopupMenu;Text: String;ListMenuItem: TMenuItem);
    function  IsCaptionedSeparator(MenuItem: TMenuItem): Boolean;
    function CreateSpecialList(Menu: TPopupMenu; SList: TNodeDataList;
                               SubMenuCaption: String): Integer;
  public
    { Public declarations }
    procedure ShowTrayiconMenu;
    procedure PopulateDirectory(Sender: TObject);
  end;

type
  TColorQuad = record
    Red, Green, Blue, Alpha: Byte;
  end;

const
  CaptionLineItemHeight = 14;
  FadeLineWidth = 32;

var
  ClassicMenu: TClassicMenu;

implementation

uses
  AppConfig, udImages, Main, ulNodeDataTypes, ulEnumerations, ulAppConfig,
  ulTreeView;

{$R *.dfm}

procedure TClassicMenu.DataModuleCreate(Sender: TObject);
begin
  tiTrayMenu.Hint   := APP_NAME + ' ' + VERSION_RELEASE + ' ' + VERSION_PRERELEASE + ' (' +
                       SUITE_DRIVE + ')';
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
  if frmMain.StartUpTime then
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
  //From CoolTrayicon source
  if Application.MainForm <> nil then
  begin
    // Restore the app, but don't automatically show its taskbar icon
    // Show application's TASKBAR icon (not the tray icon)
    ShowWindow(Application.Handle, SW_RESTORE);
    Application.Restore;
    // Show the form itself
    if Application.MainForm.WindowState = wsMinimized then
      Application.MainForm.WindowState := wsNormal;    // Override minimized state
    Application.MainForm.Visible := True;
    // Bring the main form (or its modal dialog) to the foreground
    SetForegroundWindow(Application.Handle);
  end;
end;

procedure TClassicMenu.ShowTrayiconMenu;
var
  Point: TPoint;
begin
  //Get Mouse coordinates
  GetCursorPos(Point);
  //Classic Menu
  SetForegroundWindow(frmMain.Handle);
  //Populate classic menu at runtime
  UpdateClassicMenu(pmTrayicon);
  //Show classic menu
  pmTrayicon.Popup(Point.X, Point.Y);
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
          MenuItem.Hint    := (TvFileNodeData(NodeData.Data)).PathAbsoluteExe;
          //Populate MenuItem with folder and files from folder path
          PopulateDirectory(MenuItem);
        end;
      end
      else begin
        if NodeData.Data.DataType = vtdtCategory then
          MenuItem.OnClick := GetItemsIcons;
      end;
      MenuItem.ImageIndex := NodeData.Data.ImageIndex;
    end;
    MenuItem.Data      := NodeData.Data;
    MenuItem.pNode     := NodeData.pNode;
    MenuItem.Visible   := not(NodeData.Data.HideFromMenu);
    //Set NodeData's MenuItem
    NodeData.MenuItem  := MenuItem;
  end;
end;

procedure TClassicMenu.RunFromTrayMenu(Sender: TObject);
var
  NodeData : TvBaseNodeData;
begin
  //From menu
  if (Sender is TASMenuItem) then
  begin
    NodeData := (Sender as TASMenuItem).Data;
    //Run file
    if Assigned(NodeData) then
      TvFileNodeData(NodeData).Execute(frmMain.vstList, false)
    else
      ShowMessageFmt(msgErrRun, [StringReplace((Sender as TASMenuItem).Caption, '&', '', [])]);
  end;
end;

procedure TClassicMenu.EjectDialog(Sender: TObject);
var
  WindowsPath : string;
begin
  //Call "Safe Remove hardware" Dialog
  WindowsPath := GetEnvironmentVariable('WinDir');
  if FileExists(PChar(WindowsPath + '\System32\Rundll32.exe')) then
  begin
    ShellExecute(0,'open',
                 PChar(WindowsPath + '\System32\Rundll32.exe'),
                 PChar('Shell32,Control_RunDLL hotplug.dll'),
                 PChar(WindowsPath + '\System32'),SW_SHOWNORMAL);
  end;
  //Close ASuite
  frmMain.miExitClick(Sender);
end;

procedure TClassicMenu.GetItemsIcons(Sender: TObject);
var
  MenuItem  : TASMenuItem;
  I         : Integer;
begin
  if (Sender is TASMenuItem) then
  begin
    MenuItem := (Sender as TASMenuItem);
    GetChildNodesIcons(frmMain.vstList, MenuItem.pNode);
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
  CreateSeparator(Menu,msgShortMFU,nil);
  //MFU
  if (Config.MFU) and (MFUList.Count > 0) then
  begin
    if Config.SubMenuMFU then
      CreateSpecialList(Menu,MFUList,msgLongMFU)
    else
      CreateSpecialList(Menu,MFUList,'')
  end;
  CreateSeparator(Menu,msgList,nil);
  //List
  frmMain.vstList.IterateSubtree(nil, CreateListItems, nil, [], False);
  CreateSeparator(Menu,msgShortMRU,nil);
  //MRU
  if (Config.MRU) and (MRUList.Count > 0) then
  begin
    if Config.SubMenuMRU then
      CreateSpecialList(Menu,MRUList,msgLongMRU)
    else
      CreateSpecialList(Menu,MRUList,'')
  end;
  CreateSeparator(Menu,'',nil);
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
          MenuItem.Caption := 'Show ASuite';
          MenuItem.ImageIndex := IMG_ASuite;
          MenuItem.OnClick := ShowMainForm;
          MenuItem.Default := true;
        end;
      1:
        begin
          MenuItem.Caption := 'Options...';
          MenuItem.ImageIndex := IMG_Options;
          MenuItem.OnClick := frmMain.miOptionsClick;
          MenuItem.Enabled := Not(Config.ReadOnlyMode);
        end;
    end;
  end;
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
          MenuItem.Caption := 'Safely remove hardware';
          MenuItem.OnClick := EjectDialog;
        end;
      1:
        begin
          MenuItem.Caption := 'Exit';
          MenuItem.OnClick := frmMain.miExitClick;
        end;
    end;
  end;
end;

function TClassicMenu.UpdateSpecialList(PopupMenu: TMenuItem;SList: TNodeDataList): Integer;
var
  NodeData  : TvBaseNodeData;
  I         : Integer;
  MenuItem  : TASMenuItem;
begin
  Result := 0;
  for I := 0 to SList.Count - 1 do
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
        Inc(Result);
      end
      else
        SList.Delete(I);
    end;
  end;
end;

procedure TClassicMenu.DrawCaptionedSeparator(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);

function RectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

var
  TextArea, LineArea: TRect;
  Flags: Longint;
  LineCaption : string;
begin
  //Don't highlight menu item
  ACanvas.Brush.Color := clMenu;
  ACanvas.Font.Color := clWindowText;
  LineArea := ARect;
  TextArea := LineArea;
  Dec(TextArea.Bottom, 1);
  Flags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_CENTER;
  if (Sender as TMenuItem).Hint <> '' then
    LineCaption := Format(' %s ', [(Sender as TMenuItem).Hint]);
  DrawText(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags or DT_CALCRECT);
  OffsetRect(TextArea, Round((RectWidth(LineArea) - RectWidth(TextArea)) / 2 - 0.5), 0);
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

procedure TClassicMenu.CreateSeparator(Menu: TPopupMenu;Text: String;ListMenuItem: TMenuItem);
var
  MenuItem: TMenuItem;
begin
  //If last MenuItem is a captioned separator
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
    MenuItem.OnDrawItem := DrawCaptionedSeparator;
  end;
end;

function TClassicMenu.CreateSpecialList(Menu: TPopupMenu;SList: TNodeDataList;
                                        SubMenuCaption: String): Integer;
var
  MenuItem : TASMenuItem;
begin
  MenuItem := TASMenuItem.Create(Application.MainForm);
  Menu.Items.Add(MenuItem);
  if SubMenuCaption <> '' then
  begin
    //Yes submenu
    Result := UpdateSpecialList(MenuItem,SList);
    MenuItem.Caption := SubMenuCaption;
  end
  else begin
    //No submenu
    Result := UpdateSpecialList(Menu.Items[0].parent,SList);
    MenuItem.free;
  end;
end;

function TClassicMenu.IsCaptionedSeparator(MenuItem: TMenuItem): Boolean;
begin
  Result := (MenuItem.Name = '') and
            (MenuItem.Hint <> '') and
            (MenuItem.Enabled = false);
end;

procedure TClassicMenu.PopulateDirectory(Sender: TObject);

  procedure SearchAddDirectory(AMI: TASMenuItem);
  var
    SR    : TSearchRec;
    Found : Boolean;
    NMI   : TASMenuItem;
    Attrs : integer;
  begin
    Found := FindFirst(AMI.Hint + '*',faDirectory + faReadOnly + faArchive,SR) = 0;
    try
      while Found do
      begin
        attrs := FileGetAttr(AMI.Hint + SR.Name + PathDelim);
        if ((Attrs and FILE_ATTRIBUTE_REPARSE_POINT) = 0) then
        if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '..') then
        begin
          //Create new menuitem and add base properties
          NMI             := TASMenuItem.Create(AMI);
          NMI.Caption     := SR.Name;
          NMI.Hint        := AMI.Hint + SR.Name + PathDelim;
          NMI.ImageIndex  := ImagesDM.GetSimpleIconIndex(SUITE_ICONS_PATH + FILEICON_Folder + EXT_ICO); // folder image
          //Add item in traymenu
          AMI.Add(NMI);
          //If it is not '.', expand folder else add OnClick event to open folder
          if NMI.Caption <> '.' then
            PopulateDirectory(NMI)
          else
            NMI.OnClick := OpenFile;
        end;
        //Next folder
        Found := FindNext(SR) = 0;
      end;
    finally
      FindClose(SR);
    end;
  end;

  procedure SearchAddFiles(AMI: TASMenuItem);
  var
    SR: TSearchRec;
    Found: Boolean;
    NMI: TASMenuItem;
  begin
    Found := FindFirst(AMI.Hint + '*',faReadOnly + faArchive,SR) = 0;
    try
      if Found then
        AMI.NewBottomLine;
      while Found do
      begin
        //Create new menuitem and add base properties
        NMI             := TASMenuItem.Create(AMI);
        NMI.Caption     := SR.Name;
        NMI.Hint        := AMI.Hint + SR.Name;
        NMI.ImageIndex  := ImagesDM.GetSimpleIconIndex(AMI.Hint + SR.Name);
        NMI.OnClick     := OpenFile;
        //Add item in traymenu
        AMI.Add(NMI);
        //Next file
        Found := FindNext(SR) = 0;
      end;
    finally
      FindClose(SR);
    end;
  end;

var
  MI: TASMenuItem;

begin
  MI := TASMenuItem(Sender);
  try
    MI.Hint := IncludeTrailingBackslash(MI.Hint);
    { first directories }
    SearchAddDirectory(MI);
    { then files }
    SearchAddFiles(MI);
  finally
    MI.OnClick := nil;
  end;
end;

procedure TClassicMenu.OpenFile(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow, 'open', PChar(TMenuItem(Sender).Hint), nil,
               PChar(ExtractFileDir(TMenuItem(Sender).Hint)), SW_SHOW);
end;

end.
