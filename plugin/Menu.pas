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

unit menu;

interface

uses Windows, Classes, Forms, StdCtrls, Buttons, ExtCtrls, ComCtrls, Messages,
	Jpeg, ShellAPI, Controls, Graphics, Dialogs, SysUtils, VirtualTrees, AppEvnts,
    ClassicSpeedButton, NGImages;

type
	TfrmMenu = class(TForm)
  	imgBackground: TImage;
  	imgClose: TImage;
  	imgDriveSpace: TImage;
    sbtnASuite: TClassicSpeedButton;
    imgASuite: TImage;
	  sbtnOptions: TClassicSpeedButton;
	  imgOptions: TImage;
	  imgDivider2: TImage;
	  sbtnSearch: TClassicSpeedButton;
	  imgSearch: TImage;
    sbtnAbout: TClassicSpeedButton;
    imgAbout: TImage;
  	lblDriveName: TLabel;
  	lblDriveSpace: TLabel;
  	sbtnScrollUp: TClassicSpeedButton;
  	sbtnScrollDown: TClassicSpeedButton;
   	tmrFader: TTimer;
    imgDragSpaceHidden: TImage;
    imgPersonalPicture: TImage;
    bvlPersonalPicture: TBevel;
    vstMenu: TVirtualStringTree;
    vstRecents: TVirtualStringTree;
    OpenDialog1: TOpenDialog;
    imgDivider1: TImage;
    ApplicationEvents1: TApplicationEvents;
    tmrWatchFocus: TTimer;
    imgDividerLong: TImage;
    imgEject: TImage;
    procedure imgPersonalPictureClick(Sender: TObject);
    procedure imgDragSpaceHiddenMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  	procedure FormCreate(Sender: TObject);
  	procedure imgCloseClick(Sender: TObject);
  	procedure OpenMenu;
  	procedure CloseMenu;
  	procedure tmrFaderTimer(Sender: TObject);
    procedure vstMenuMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure sbtnScrollUpClick(Sender: TObject);
    procedure sbtnScrollDownClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure tmrWatchFocusTimer(Sender: TObject);
    procedure sbtnASuiteClick(Sender: TObject);
    procedure sbtnOptionsClick(Sender: TObject);
    procedure sbtnSearchClick(Sender: TObject);
    procedure sbtnAboutClick(Sender: TObject);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstRecentsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure RunExeFromMenu(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vstMenuGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure imgEjectClick(Sender: TObject);
	protected
		procedure CreateParams(var Params: TCreateParams); override;
	private    
    { Private declarations }
    procedure TranslateForm(Lingua: string);
    procedure CopyImageInVst(Source:TImage;Dest:TVirtualStringTree);
    procedure PopulateMenuTree(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);
    procedure OpenFolder(FolderPath: String); 
  	procedure MenuButtonClick(Sender: TObject);
    procedure CreateMenuButton(ButtonIndex, InitialTop : Integer);
    procedure DeleteAllMenuButtons;
	public
    { Public declarations }
  end;

var
	frmMenu : TfrmMenu;
  Opening : Boolean;

implementation

{$R *.dfm}

uses Main, CommonUtils, Card, Option;

procedure TfrmMenu.TranslateForm(Lingua: string);
begin
  with frmMain.xmldTranslate.DocumentElement.ChildNodes['Form1'] do
  begin
    sbtnOptions.Caption := ChildNodes['DefaultMenuButtonOptions'].Text;
    sbtnSearch.Caption  := ChildNodes['DefaultMenuButtonSearch'].Text;
    sbtnAbout.Caption   := ChildNodes['DefaultMenuButtonAbout'].Text;
  end;
end;

procedure TfrmMenu.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
var
  Tree : TBaseVirtualTree;
begin
  if Msg.message = WM_MOUSELEAVE then
  begin
    Tree := nil;
    if vstRecents.Handle = Msg.hwnd then
      Tree := vstRecents
    else
      if vstMenu.Handle = Msg.hwnd then
        Tree := vstMenu;
    if Assigned(Tree) and Assigned(Tree.FocusedNode) then
    begin
      Tree.Selected[Tree.FocusedNode] := False;
      Tree.FocusedNode := nil;
    end;
  end;
end;

procedure TfrmMenu.CopyImageInVst(Source: TImage;Dest: TVirtualStringTree);
var
  RectSource, RectDest : TRect;
  bmpTempImage, bmpTempBG : TBitmap;
begin
  bmpTempImage := TBitmap.Create;
  bmpTempBG    := TBitmap.Create;
  try
    bmpTempImage.Height := Dest.Height;
    bmpTempImage.Width  := Dest.Width;
    //Set RectSource size
    RectSource.Left     := Dest.Left;
    RectSource.Top      := Dest.Top;
    RectSource.Right    := Dest.Left + Dest.Width;
    RectSource.Bottom   := Dest.Top + Dest.Height;
    //Set RectSource size
    RectDest.Left       := 0;
    RectDest.Top        := 0;
    RectDest.Right      := Dest.Width;
    RectDest.Bottom     := Dest.Height;
    //CopyRect in bmpTempImage and use it as background for Dest vst
    if (Source.Picture.graphic is TNGImage) then
    begin
      //Free bmpTempBG to avoid memory leak
      bmpTempBG.Free;
      bmpTempBG := (Source.Picture.Graphic as TNGImage).CopyBitmap;
    end
    else
      bmpTempBG.Assign(Source.Picture.Graphic);
    bmpTempImage.canvas.CopyRect(RectDest, bmpTempBG.Canvas, RectSource);
    Dest.Background.Bitmap := bmpTempImage;
    Dest.Background.Bitmap.Transparent := true;
  finally
    bmpTempImage.Free;
    bmpTempBG.Free;
  end;
end;

procedure TfrmMenu.CreateParams(var Params: TCreateParams);
// used to display a drop shadow on the menu on WinXP and up
const
	CS_DROPSHADOW = $00020000;
begin
	inherited;
	if ((Win32MajorVersion >= 5) and (Win32MinorVersion >= 1)) OR (Win32MajorVersion >= 6) then
		Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TFrmMenu.FormCreate(Sender: TObject);
begin
  //NodeDataSize
  vstMenu.NodeDataSize    := SizeOf(TTreeDataX);
  vstRecents.NodeDataSize := SizeOf(TTreeDataX);
  vstMenu.Images          := frmMain.ImageList1;
  vstRecents.Images       := frmMain.ImageList1;
  //Position
  Top  := Screen.WorkAreaRect.Bottom - Height;
  Left := Screen.WorkAreaRect.Right - Width;
end;

procedure TfrmMenu.FormShow(Sender: TObject);
var
  NodeDataMenu : PTreeDataX;
  I            : Integer;
  Node         : PVirtualNode;
  Drive        : Char;
  dblDriveSize : Double;
  dblDriveUsed : Double;
  NGImage      : TNGImage;
  ButtonsCount : Integer;
begin
  TranslateForm(LauncherOptions.LangName);
  if LauncherOptions.RefreshMenuTheme then
  begin
    // Check for personal picture
    if FileExists(RelativeToAbsolute(LauncherOptions.MenuPersonalPicture)) then
   		imgPersonalPicture.Picture.LoadFromFile(RelativeToAbsolute(LauncherOptions.MenuPersonalPicture))
    else
      if FileExists(PathTheme + LauncherOptions.MenuTheme + '\PersonalPicture.jpg') then
   		  imgPersonalPicture.Picture.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\PersonalPicture.jpg');
    if Not(DirectoryExists(PathTheme + LauncherOptions.MenuTheme)) then
      LauncherOptions.MenuTheme := 'Default';
  	//Load Theme
    //Load background.png (if it isn't exist, load background.jpg)
    if FileExists(PathTheme + LauncherOptions.MenuTheme + '\Theme\background.png') then
    begin
      NGImage := TNGImage.Create;
      try
        NGImage.BGColor := clFuchsia;
        NGImage.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\Theme\background.png');
        imgBackground.Picture.Assign(NGImage);
      finally
        NGImage.Free;
      end;
    end
    else
      if FileExists(PathTheme + LauncherOptions.MenuTheme + '\Theme\background.jpg') then
        imgBackground.Picture.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\Theme\background.jpg');
    imgDriveSpace.Picture.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\Theme\drive_space_slider.bmp');
    if FileExists(PathTheme + LauncherOptions.MenuTheme + '\Theme\divider.jpg') then
      imgDividerLong.Picture.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\Theme\divider.jpg');
   	//Load Icons
    if FileExists(PathTheme + LauncherOptions.MenuTheme + '\IconTheme\ASuite.ico') then
      imgASuite.Picture.Icon.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\IconTheme\ASuite.ico');
  	imgAbout.Picture.Icon.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\IconTheme\help.ico');
  	imgOptions.Picture.Icon.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\IconTheme\options.ico');
  	imgSearch.Picture.Icon.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\IconTheme\search.ico');
    LauncherOptions.RefreshMenuTheme := False;
  end;
  lblDriveName.Caption := ExtractFileDrive(ApplicationPath);
  ButtonsCount := 0;
  DeleteAllMenuButtons;
  for I := 0 to Length(LauncherOptions.TrayMenuButtons) - 1 do
  begin
    if LauncherOptions.TrayMenuButtons[I].Enable then
    begin
      CreateMenuButton(I,148 + (ButtonsCount * 30) + ((ButtonsCount + 1) * 2)); //30 is Button's height, 2 is the interval between buttons
      Inc(ButtonsCount);
    end;
  end;
  //Other Buttons (search and help) positions
  imgDivider2.Top := (148 + (ButtonsCount * 30) + ((ButtonsCount) * 2)) + 2;
  sbtnSearch.Top  := imgDivider2.Top + imgDivider2.Height + 2;
  imgSearch.Top   := imgDivider2.Top + imgDivider2.Height + 6;
  sbtnAbout.Top   := sbtnSearch.Top + sbtnSearch.Height + 2;
  imgAbout.Top    := sbtnSearch.Top + sbtnSearch.Height + 6;
  //Clear virtualtrees
  vstMenu.Clear;
  vstRecents.Clear;
  //Refresh Menu
  frmMain.vstList.IterateSubtree(nil, PopulateMenuTree, nil, [], False);
  //Refresh Recents
  if LauncherOptions.MRU then
  begin
    for I := 0 to MRUList.Count - 1 do
    begin
      if Assigned(MRUList[I]) then
      begin
        Node := vstRecents.AddChild(nil);
        NodeDataMenu := vstRecents.GetNodeData(Node);
        NodeDataMenu.pNodeList := MRUList[I];
        NodeDataMenu.pNodeX    := Node;
      end;
    end;
  end;
  //Position vstRecents, bvlDivider and vstMenu
  vstRecents.Visible     := (LauncherOptions.MRU) and (MRUList.Count > 0);
  vstRecents.Enabled     := (LauncherOptions.MRU) and (MRUList.Count > 0);
  imgDividerLong.Visible := (LauncherOptions.MRU) and (MRUList.Count > 0);
  imgDividerLong.Enabled := (LauncherOptions.MRU) and (MRUList.Count > 0);
  if (LauncherOptions.MRU) and (MRUList.Count > 0) then
  begin
    vstRecents.Height  := Integer(vstRecents.DefaultNodeHeight) * MRUList.Count;
    vstRecents.Top     := (sbtnScrollUp.Top + 460) - (vstRecents.Height); //460 = vstRecents.Top + Old vstRecents.Height
    imgDividerLong.Top := vstRecents.Top      - (imgDividerLong.Height);
    sbtnScrollDown.Top := imgDividerLong.Top  - (sbtnScrollDown.Height);
    vstMenu.Height     := (sbtnScrollDown.Top - (vstMenu.Top + vstMenu.Height + 4)) + vstMenu.Height;
  end
  else begin
    sbtnScrollDown.Top := 530 - sbtnScrollDown.Height;
    vstMenu.Height     := sbtnScrollDown.Top - (vstMenu.Top + 4);
  end;
  //Calculate and display the drive size
  Drive := ExtractFileDrive(ApplicationPath)[1];
  dblDriveSize := DiskSize(Ord(Drive) - 64);
  dblDriveUsed := dblDriveSize - DiskFree(Ord(Drive) - 64);
  imgDriveSpace.Width   := Round(dblDriveUsed/dblDriveSize * 131);
  lblDriveSpace.Caption := DiskFreeString(Drive, True) + ' free of ' + DiskSizeString(Drive, True);
  //Workaround for vst trasparent
  CopyImageInVst(imgBackground,vstMenu);
  CopyImageInVst(imgBackground,vstRecents);
end;

procedure TfrmMenu.sbtnASuiteClick(Sender: TObject);
begin
  frmMain.ShowMainForm(Sender);
  frmMain.pcList.ActivePageIndex := 0;
  frmMain.SetFocus;
end;

procedure TfrmMenu.MenuButtonClick(Sender: TObject);
begin
  OpenFolder(LauncherOptions.TrayMenuButtons[(Sender as TClassicSpeedButton).Tag].PathExe);
end; 

procedure TfrmMenu.DeleteAllMenuButtons;
var
  SpeedButton : TComponent;
  ButtonIcon  : TComponent;
  I           : Integer;
begin
  for I := 0 to 7 do
  begin
    SpeedButton := FindComponent('sbtnButton' + IntToStr(I));
    if (SpeedButton is TClassicSpeedButton) then
      SpeedButton.Free;
    ButtonIcon := FindComponent('imgButton' + IntToStr(I));
    if (ButtonIcon is TImage) then
      ButtonIcon.Free;
  end;
end;

procedure TfrmMenu.CreateMenuButton(ButtonIndex, InitialTop : Integer);
var
  SpeedButton : TClassicSpeedButton;
  ButtonIcon  : TImage;
  TempPath    : string;
begin
  //Create ButtonIcon and set some properties
  ButtonIcon := TImage.Create(Self);
  with ButtonIcon do
  begin
    Name    := 'imgButton' + IntToStr(ButtonIndex);
    //Coordinates and sizes
    Height  := 24;
    Width   := 24;
    Left    := 272;
    Top     := InitialTop + 4;
    Parent  := Self;
    //ToDo - Load custom icon or default icon (from theme)
    TempPath := RelativeToAbsolute(LauncherOptions.TrayMenuButtons[ButtonIndex].PathIcon);
    //If PathIcon exists load it else load asuite.ico from theme folders
    if FileExists(TempPath) then
      Picture.Icon.LoadFromFile(TempPath)
    else
      Picture.Icon.LoadFromFile(PathTheme + LauncherOptions.MenuTheme + '\IconTheme\asuite.ico');
  end;
  //Create SpeedButton and set some properties
  SpeedButton := TClassicSpeedButton.Create(Self);
  with SpeedButton do
  begin
    Name    := 'sbtnButton' + IntToStr(ButtonIndex);
    //Coordinates and sizes
    Height  := 30;
    Width   := 120;
    Left    := 269;
    Top     := InitialTop;
    Parent  := Self;
    //Graphic
    Flat    := True;
    Margin  := 33;
    ParentFont := False;
    Font.Color := clWhite;
    Font.Style := [fsBold];
    Caption := LauncherOptions.TrayMenuButtons[ButtonIndex].Name;
    //Properties to execute
    Tag     := ButtonIndex;
    OnClick := MenuButtonClick;
  end;
end;

procedure TfrmMenu.sbtnOptionsClick(Sender: TObject);
begin
  frmMain.miOptionsClick(Sender);
end;

procedure TfrmMenu.sbtnAboutClick(Sender: TObject);
begin
  if not IsFormOpen('frmCard') then
    Application.CreateForm(TfrmCard, frmCard);
  frmCard.show;
  frmCard.pcCard.ActivePageIndex := 0;
  frmCard.SetFocus;
end;

procedure TfrmMenu.sbtnScrollDownClick(Sender: TObject);
begin
  vstMenu.OffsetY := (vstMenu.OffsetY - 1 * 20);
end;

procedure TfrmMenu.sbtnScrollUpClick(Sender: TObject);
begin
  vstMenu.OffsetY := (vstMenu.OffsetY + 1 * 20);
end;

procedure TfrmMenu.sbtnSearchClick(Sender: TObject);
begin
  frmMain.ShowMainForm(Sender);
  frmMain.pcList.ActivePageIndex := 1;
  frmMain.SetFocus;
end;

procedure TfrmMenu.imgCloseClick(Sender: TObject);
begin
  frmMain.Hide;
  ShutdownTime := True;
  if IsFormOpen('frmCard') then
    frmCard.Close;
  frmMain.Close;
end;

procedure TfrmMenu.imgDragSpaceHiddenMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end;
end;

procedure TfrmMenu.imgEjectClick(Sender: TObject);
begin
  frmMain.EjectDialog(Sender);
end;

procedure TfrmMenu.imgPersonalPictureClick(Sender: TObject);
var
  TempString : string;
begin
  TempString := '';
  OpenDialog1.Filter     := 'Personal Picture [48x48] (*.jpg)|*.jpg';
  OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(LauncherOptions.MenuPersonalPicture));
  if OpenDialog1.Execute then
  begin
    TempString := OpenDialog1.FileName;
		imgPersonalPicture.Picture.LoadFromFile(TempString);
    LauncherOptions.MenuPersonalPicture := AbsoluteToRelative(TempString);
  end;
  SetCurrentDir(ApplicationPath);
end;

procedure TfrmMenu.vstMenuGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: WideString);
var
  NodeData: PTreeData;
begin
  NodeData := NodeDataXToNodeDataList(Node);
  if NodeData.PathExeError then
    HintText := ArrayMessages[1]
  else
    HintText := NodeData.Desc;
end;

procedure TfrmMenu.vstMenuMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Node : PVirtualNode;
begin
  FocusControl(Sender as TBaseVirtualTree);
  with (Sender as TBaseVirtualTree) do
  begin
    Node := GetNodeAt(X,Y);
    if Assigned(Node) then
    begin
      Selected[Node] := True;
      FocusedNode    := Node;
    end
    else begin
      Selected[FocusedNode] := False;
      FocusedNode    := nil;
    end;
  end;
  if (Sender = vstRecents) then
  begin
    vstMenu.Selected[vstMenu.FocusedNode] := False;
    vstMenu.FocusedNode := nil;
  end
  else begin
    vstRecents.Selected[vstRecents.FocusedNode] := False;
    vstRecents.FocusedNode := nil;
  end;
end;

procedure TfrmMenu.RunExeFromMenu(Sender: TObject);   
var
  NodeDataList : PTreeData;
  Node         : PVirtualNode;
begin
  if ClickOnButtonTree(vstMenu) then
    Exit;
  if (Sender = vstMenu) or (Sender = vstRecents) then
  begin
    Node := (Sender as TBaseVirtualTree).GetFirstSelected;
    if Assigned(Node) then
    begin
      NodeDataList := NodeDataXToNodeDataList(Node);
      if Assigned(NodeDataList) then
      begin
        //Run file
        if (NodeDataList.Tipo = 1) or (NodeDataList.Tipo = 2) then
        begin
          RunProcess(frmMain.vstList,NodeDataList,false);
          AddMRU(frmMain.vstList,frmMain.CoolTrayIcon1,NodeDataList.pNode,NodeDataList.DontInsertMRU);
          RunActionOnExe(NodeDataList);
          CloseMenu;
        end
        else
          if (NodeDataList.Tipo = 0) then
            with (Sender as TBaseVirtualTree) do
              Expanded[Node] := Not(Expanded[Node]);
      end;
    end;
  end
end;

procedure TfrmMenu.vstRecentsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : PTreeData;
begin
  NodeData := NodeDataXToNodeDataList(Node);
  if Assigned(NodeData) then
    ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmMenu.vstGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  NodeData : PTreeData;
begin
  NodeData := NodeDataXToNodeDataList(Node);
  if Assigned(NodeData) then
  begin
    CellText := StringReplace(NodeData.Name, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
    if CellText = '-' then
      CellText := StringReplace(CellText, '-', '-----------------------------------------', [rfIgnoreCase,rfReplaceAll]);
  end;
end;

procedure TfrmMenu.OpenFolder(FolderPath: String);
var
  ErrorCode : Integer;
begin
  ErrorCode := ShellExecute(GetDesktopWindow, 'open', PChar(RelativeToAbsolute(FolderPath)),
                            PChar(''),
                            PChar(ExtractFilePath(RelativeToAbsolute(FolderPath))), SW_SHOWDEFAULT);
  if ErrorCode <= 32 then
    ShowMessageFmt(ArrayMessages[11],['',SysErrorMessage(ErrorCode)]);
end;

procedure TfrmMenu.OpenMenu;
begin
  //Fade in now    
  Opening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Show;
  SetForegroundWindow(Handle);
  if Not(IsWindowVisible(frmMain.Handle)) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmMenu.CloseMenu;
begin
  //Fade in out
  Opening := False;
  tmrFader.Enabled:= True;
end;

procedure TfrmMenu.PopulateMenuTree(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                    Data: Pointer; var Abort: Boolean);
var
  NewNodeData : PTreeDataX;
  NodeData, ParentNodeData : PTreeData;
  NewNode     : PVirtualNode;
begin
  if Assigned(Node) then
  begin
    NodeData              := frmMain.vstList.GetNodeData(Node);
    if Not(NodeData.HideSoftwareMenu) then
    begin
      if (Node.Parent <> frmMain.vstList.RootNode) then
      begin
        ParentNodeData      := frmMain.vstList.GetNodeData(Node.Parent);
        NewNode             := vstMenu.AddChild(ParentNodeData.MenuNode);
      end
      else
        NewNode             := vstMenu.AddChild(nil);
      NewNodeData           := vstMenu.GetNodeData(NewNode);
      NodeData.MenuNode     := NewNode;
      //References
      NewNodeData.pNodeList := Node;
      NewNodeData.pNodeX    := NewNode;
    end;
  end;
end;

procedure TfrmMenu.tmrFaderTimer(Sender: TObject);
// adjust the alpha values to fade the menu in or out
begin
  if Opening then
  begin
    if (frmMenu.AlphaBlendValue < 225) and LauncherOptions.MenuFade then
   	  frmMenu.AlphaBlendValue := frmMenu.AlphaBlendValue + 30
    else begin
 	    frmMenu.AlphaBlendValue := 255;
   	  tmrFader.Enabled        := False;
    end;
  end
  else begin
    if (frmMenu.AlphaBlendValue > 30) and LauncherOptions.MenuFade then
      frmMenu.AlphaBlendValue := frmMenu.AlphaBlendValue - 30
    else begin
      frmMenu.AlphaBlendValue := 0;
      tmrFader.Enabled        := False;
      frmMenu.Hide;
    end;
  end;
end;

procedure TfrmMenu.tmrWatchFocusTimer(Sender: TObject);
begin
  //if menu lost its focus, it hide
  if (getFocus() = 0) then
    CloseMenu;
end;

end.
