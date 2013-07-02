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

unit GraphicMenu;

interface

uses
  Windows, Classes, Forms, StdCtrls, Buttons, ExtCtrls, ComCtrls, Messages,
	ShellAPI, Controls, Graphics, Dialogs, SysUtils, VirtualTrees, AppEvnts,
  Vcl.Imaging.pngimage, cySkinButton, IniFiles, ulCommonClasses, Vcl.Menus;

type
  TGraphicMenuElement = (
      //RightButtons
      gmbASuite,
      gmbOptions,
      gmbDocuments,
      gmbMusic,
      gmbPictures,
      gmbVideos,
      gmbExplore,
      gmbAbout,
      //Other buttons
      gmbEject,
      gmbExit,
      //Tabs
      gmbList,
      gmbMRU,
      gmbMFU
  );

  TButtonState = (
      bsNormal,
      bsHover,
      bsClicked
  );
  //Workaround for TPageControl without borders
  TPageControl = class(ComCtrls.TPageControl)
  private
    procedure TCMAdjustRect(var Msg: TMessage); message $1300 + 40; //TCM_ADJUSTRECT
  end;

	TfrmGraphicMenu = class(TForm)
  	imgDriveSpace: TImage;
	  imgDivider2: TImage;
  	lblDriveName: TLabel;
  	lblDriveSpace: TLabel;
   	tmrFader: TTimer;
    imgLogo: TImage;
    imgPersonalPicture: TImage;
    bvlPersonalPicture: TBevel;
    vstList: TVirtualStringTree;
    OpenDialog1: TOpenDialog;
    imgDivider1: TImage;
    ApplicationEvents1: TApplicationEvents;
    tmrWatchFocus: TTimer;
    sknbtnASuite: TcySkinButton;
    sknbtnOptions: TcySkinButton;
    sknbtnDocuments: TcySkinButton;
    sknbtnPictures: TcySkinButton;
    sknbtnAbout: TcySkinButton;
    sknbtnExplore: TcySkinButton;
    sknbtnVideos: TcySkinButton;
    sknbtnMusic: TcySkinButton;
    btnSearch: TButtonedEdit;
    sknbtnList: TcySkinButton;
    sknbtnRecents: TcySkinButton;
    sknbtnMFU: TcySkinButton;
    sknbtnEject: TcySkinButton;
    sknbtnExit: TcySkinButton;
    imgBackground: TImage;
    pgcTreeViews: TPageControl;
    tsList: TTabSheet;
    tsMRU: TTabSheet;
    tsMFU: TTabSheet;
    vstMostUsed: TVirtualStringTree;
    vstRecents: TVirtualStringTree;
    imgDriveBackground: TImage;
    pmWindow: TPopupMenu;
    miRunSelectedSw: TMenuItem;
    miRunAs: TMenuItem;
    miRunAsAdmin: TMenuItem;
    miOpenFolderSw: TMenuItem;
    N6: TMenuItem;
    miProperty2: TMenuItem;
    tsSearch: TTabSheet;
    vstSearch: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure tmrFaderTimer(Sender: TObject);
    procedure imgLogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenRightButton(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstGetImageLargeIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure vstListExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure sknbtnListClick(Sender: TObject);
    procedure sknbtnRecentsClick(Sender: TObject);
    procedure sknbtnMFUClick(Sender: TObject);
    procedure sknbtnEjectClick(Sender: TObject);
    procedure sknbtnExitClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure tmrWatchFocusTimer(Sender: TObject);
    procedure vstKeyPress(Sender: TObject; var Key: Char);
    procedure vstMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vstInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure vstNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure imgPersonalPictureClick(Sender: TObject);
    procedure miProperty2Click(Sender: TObject);
    procedure miRunSelectedSwClick(Sender: TObject);
    procedure miRunAsClick(Sender: TObject);
    procedure miRunAsAdminClick(Sender: TObject);
    procedure miOpenFolderSwClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSearchKeyPress(Sender: TObject; var Key: Char);
	private    
    { Private declarations }
    FOpening   : Boolean;
    FThemePath : string;
    procedure CopyImageInVst(Source:TImage;Page: TPageControl);
    procedure DrawButton(IniFile: TIniFile;Button: TcySkinButton;
                         ButtonType: TGraphicMenuElement);
    procedure DrawIconInPNGImage(IniFile: TIniFile;PNGImage: TPngImage;
                                 ButtonType: TGraphicMenuElement);
    procedure DrawTextInPNGImage(IniFile: TIniFile;ButtonState: TButtonState;
                                 PNGImage: TPngImage;ButtonType: TGraphicMenuElement; SpaceForIcon: Boolean = True);
    function GetButtonCaption(IniFile: TIniFile;ButtonType: TGraphicMenuElement): string;
    function GetButtonIconPath(IniFile: TIniFile;ButtonType: TGraphicMenuElement): string;
    function GetIniFileSection(ElementType: TGraphicMenuElement): string;
    procedure OpenFolder(FolderPath: string);
    function IsRightButton(ButtonType: TGraphicMenuElement): Boolean;
    procedure DrawIconAndTextInPNGImage(IniFile: TIniFile;
      ButtonState: TButtonState; PNGImage: TPngImage;
      ButtonType: TGraphicMenuElement);
    procedure PopulateMenuTree(Sender: TBaseVirtualTree; Node: PVirtualNode;
                               Data: Pointer; var Abort: Boolean);
    procedure PopulateSpecialTree(Tree: TBaseVirtualTree;SList: TNodeDataList;MaxItems: Integer);
    procedure DrawHardDiskSpace(IniFile: TIniFile; DriveBackGround, DriveSpace: TImage);
    function  GetActiveTree: TBaseVirtualTree;
    procedure AssignFontFromString(strfont: string;CompFont: TFont);
	public
    { Public declarations }
    procedure OpenMenu;
  	procedure CloseMenu;
  end;

var
	frmGraphicMenu : TfrmGraphicMenu;

const
  //Theme.ini structure
  //Sections
  INIFILE_SECTION_INFO         = 'info';
  INIFILE_SECTION_GENERAL      = 'general';
  INIFILE_SECTION_RIGHTBUTTONS = 'rightbuttons';
  INIFILE_SECTION_HARDDISK     = 'harddisk';
  INIFILE_SECTION_SEARCH       = 'search';
  INIFILE_SECTION_LIST         = 'list';
  INIFILE_SECTION_RECENTS      = 'recent';
  INIFILE_SECTION_MOSTUSED     = 'mostused';
  INIFILE_SECTION_EJECTBUTTON  = 'ejectbutton';
  INIFILE_SECTION_EXITBUTTON   = 'exitbutton';
  //Keys
  //Info
  INIFILE_KEY_NAME    = 'name';
  INIFILE_KEY_AUTHOR  = 'author';
  INIFILE_KEY_VERSION = 'version';
  INIFILE_KEY_URL     = 'url';
  //Images
  INIFILE_KEY_IMAGENORMAL  = 'image_normal';
  INIFILE_KEY_IMAGEHOVER   = 'image_hover';
  INIFILE_KEY_IMAGECLICKED = 'image_clicked';
  INIFILE_KEY_IMAGEBACKGROUND = 'image_background';
  INIFILE_KEY_IMAGELOGO    = 'image_logo';
  INIFILE_KEY_IMAGESPACE   = 'image_space';
  INIFILE_KEY_IMAGESEPARATOR = 'image_separator';
  //Fonts
  INIFILE_KEY_FONTNORMAL   = 'font_normal';
  INIFILE_KEY_FONTHOVER    = 'font_hover';
  INIFILE_KEY_FONTCLICKED  = 'font_clicked';
  INIFILE_KEY_FONT         = 'font'; //Generic font key
  //Icons
  INIFILE_KEY_ICONASUITE   = 'icon_asuite';
  INIFILE_KEY_ICONEXPLORE  = 'icon_explore';
  INIFILE_KEY_ICONDOCUMENT = 'icon_document';
  INIFILE_KEY_ICONMUSIC    = 'icon_music';
  INIFILE_KEY_ICONPICTURES = 'icon_pictures';
  INIFILE_KEY_ICONVIDEOS   = 'icon_videos';
  INIFILE_KEY_ICONOPTIONS  = 'icon_options';
  INIFILE_KEY_ICONHELP     = 'icon_help';
  INIFILE_KEY_ICON         = 'icon'; //Generic icon key

implementation

{$R *.dfm}

uses
  Main, Options, ulSysUtils, AppConfig, ulAppConfig, ulCommonUtils, About,
  udImages, ulNodeDataTypes, ulTreeView, ulEnumerations;

procedure TfrmGraphicMenu.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
var
  Tree : TBaseVirtualTree;
begin
  if Msg.message = WM_MOUSELEAVE then
  begin
    Tree := nil;
    if vstList.Handle = Msg.hwnd then
      Tree := vstList
    else
      if vstRecents.Handle = Msg.hwnd then
        Tree := vstRecents
      else
        if vstMostUsed.Handle = Msg.hwnd then
          Tree := vstMostUsed
        else
          if vstSearch.Handle = Msg.hwnd then
            Tree := vstSearch;
    if Assigned(Tree) and Assigned(Tree.FocusedNode) then
    begin
      Tree.Selected[Tree.FocusedNode] := False;
      Tree.FocusedNode := nil;
    end;
  end;
end;

procedure TfrmGraphicMenu.btnSearchClick(Sender: TObject);
begin
  pgcTreeViews.ActivePageIndex := PG_MENUSEARCH;
  frmMain.DoSearchItem(vstSearch,btnSearch.Text,IterateSubtreeProcs.GMFindNode);
end;

procedure TfrmGraphicMenu.btnSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    btnSearchClick(Sender);
end;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  FOpening := False;
  tmrFader.Enabled:= True;
end;

procedure TfrmGraphicMenu.AssignFontFromString(strfont: string;CompFont: TFont);
var
  vFont: TFont;
begin
  try
    vFont := StrToFont(strfont);
    CompFont.Assign(vFont);
  finally
    vFont.Free;
  end;
end;

procedure TfrmGraphicMenu.DrawHardDiskSpace(IniFile: TIniFile; DriveBackGround, DriveSpace: TImage);
var
  HDPath, HDSpacePath: string;
begin
  //Hard Disk Space
  HDPath := FThemePath + IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_IMAGEBACKGROUND, '');
  HDSpacePath := FThemePath + IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_IMAGESPACE, '');
  if FileExists(HDPath) then
    DriveBackGround.Picture.LoadFromFile(HDPath);
  if FileExists(HDSpacePath) then
    DriveSpace.Picture.LoadFromFile(HDSpacePath);
end;

procedure TfrmGraphicMenu.OpenFolder(FolderPath: string);
var
  ErrorCode: Integer;
begin
  ErrorCode := ShellExecute(GetDesktopWindow, 'open', PChar(FolderPath), PChar(''), PChar(FolderPath), SW_SHOWDEFAULT);
  if ErrorCode <= 32 then
    ShowMessageFmt(msgErrGeneric, ['', SysErrorMessage(ErrorCode)]);
end;

procedure TfrmGraphicMenu.CopyImageInVst(Source: TImage;Page: TPageControl);
var
  RectSource, RectDest : TRect;
  bmpTempImage, bmpTempBG : TBitmap;
begin
  bmpTempImage := TBitmap.Create;
  bmpTempBG    := TBitmap.Create;
  try
    bmpTempImage.Height := Page.Height;
    bmpTempImage.Width  := Page.Width;
    //Set RectSource size
    RectSource.Left     := Page.Left;
    RectSource.Top      := Page.Top;
    RectSource.Right    := Page.Left + Page.Width;
    RectSource.Bottom   := Page.Top + Page.Height;
    //Set RectDest size
    RectDest.Left       := 0;
    RectDest.Top        := 0;
    RectDest.Right      := Page.Width;
    RectDest.Bottom     := Page.Height;
    //CopyRect in bmpTempImage and use it as background for Dest vst
    bmpTempBG.Width := Source.Picture.Width;
    bmpTempBG.Height := Source.Picture.Height;
    bmpTempBG.Canvas.Draw(0, 0, Source.Picture.Graphic);
    bmpTempImage.canvas.CopyRect(RectDest, bmpTempBG.Canvas, RectSource);
    //Change vst's bitmapList
    vstList.Background.Bitmap     := bmpTempImage;
    vstRecents.Background.Bitmap  := bmpTempImage;
    vstMostUsed.Background.Bitmap := bmpTempImage;
  finally
    bmpTempImage.Free;
    bmpTempBG.Free;
  end;
end;

procedure TfrmGraphicMenu.DrawButton(IniFile: TIniFile;Button: TcySkinButton;
                                            ButtonType: TGraphicMenuElement);
var
  PNGImage_Normal, PNGImage_Hover, PNGImage_Clicked: TPngImage;
  Image_Normal, Image_Hover, Image_Clicked, IniFile_Section: string;
begin
  PNGImage_Normal  := TPngImage.Create;
  PNGImage_Hover   := TPngImage.Create;
  PNGImage_Clicked := TPngImage.Create;
  try
    IniFile_Section := GetIniFileSection(ButtonType);
    //Get images path
    Image_Normal  := FThemePath + IniFile.ReadString(IniFile_Section, INIFILE_KEY_IMAGENORMAL, '');
    Image_Hover   := FThemePath + IniFile.ReadString(IniFile_Section, INIFILE_KEY_IMAGEHOVER, '');
    Image_Clicked := FThemePath + IniFile.ReadString(IniFile_Section, INIFILE_KEY_IMAGECLICKED, '');
    //Load png button states
    if FileExists(Image_Normal) then
      PNGImage_Normal.LoadFromFile(Image_Normal);
    if FileExists(Image_Hover) then
      PNGImage_Hover.LoadFromFile(Image_Hover);
    if FileExists(Image_Clicked) then
      PNGImage_Clicked.LoadFromFile(Image_Clicked);
    //Draw caption and icon in PNGImage_*, if button is a RightButton
    if IsRightButton(ButtonType) then
    begin
      DrawIconAndTextInPNGImage(IniFile,bsNormal,PNGImage_Normal,ButtonType);
      DrawIconAndTextInPNGImage(IniFile,bsHover,PNGImage_Hover,ButtonType);
      DrawIconAndTextInPNGImage(IniFile,bsClicked,PNGImage_Clicked,ButtonType);
    end
    else
      if ButtonType in [gmbList, gmbMRU, gmbMFU] then
      begin
        DrawTextInPNGImage(IniFile,bsNormal,PNGImage_Normal,ButtonType,False);
        DrawTextInPNGImage(IniFile,bsHover,PNGImage_Hover,ButtonType,False);
        DrawTextInPNGImage(IniFile,bsClicked,PNGImage_Clicked,ButtonType,False);
      end;
    //Set Button's PicNormal, PicMouseOver and PicMouseDown
    if Assigned(PNGImage_Normal) then
      Button.PicNormal.Assign(PNGImage_Normal);
    if Assigned(PNGImage_Hover) then
      Button.PicMouseOver.Assign(PNGImage_Hover);
    if Assigned(PNGImage_Clicked) then
      Button.PicMouseDown.Assign(PNGImage_Clicked);
  finally
    PNGImage_Normal.Free;
    PNGImage_Hover.Free;
    PNGImage_Clicked.Free;
  end;
end;

procedure TfrmGraphicMenu.DrawIconAndTextInPNGImage(IniFile: TIniFile;
  ButtonState: TButtonState; PNGImage: TPngImage;
  ButtonType: TGraphicMenuElement);
begin
  DrawIconInPNGImage(IniFile,PNGImage,ButtonType);
  DrawTextInPNGImage(IniFile,ButtonState,PNGImage,ButtonType);
end;

procedure TfrmGraphicMenu.DrawIconInPNGImage(IniFile: TIniFile; PNGImage: TPngImage;
  ButtonType: TGraphicMenuElement);
var
  Icon : TIcon;
  IconPath, IniFile_Section : string;
begin
  if Not Assigned(PNGImage) then
    Exit;
  Icon := TIcon.Create;
  try
    //Get and draw icon
    IniFile_Section := GetIniFileSection(ButtonType);
    IconPath := GetButtonIconPath(IniFile, ButtonType);
    if FileExists(FThemePath + IconPath) then
    begin
      Icon.LoadFromFile(FThemePath + IconPath);
      PNGImage.Canvas.Lock;
      try
        PNGImage.Canvas.Draw(5, 3, Icon);
      finally
        PNGImage.Canvas.Unlock;
      end;
    end;
  finally
    Icon.Free;
  end;
end;

procedure TfrmGraphicMenu.DrawTextInPNGImage(IniFile: TIniFile;
  ButtonState: TButtonState; PNGImage: TPngImage;
  ButtonType: TGraphicMenuElement; SpaceForIcon: Boolean = True);
var
  TopText  : Integer;
  FontText : TFont;
  Caption, IniFile_Section : string;
begin
  if Not Assigned(PNGImage) then
    Exit;
  try
    IniFile_Section := GetIniFileSection(ButtonType);
    //Get font
    case ButtonState of
      bsNormal  : FontText := StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTNORMAL, ''));
      bsHover   : FontText := StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTHOVER, ''));
      bsClicked : FontText := StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTCLICKED, ''));
    end;
    //Get caption and draw it
    Caption  := GetButtonCaption(IniFile, ButtonType);
    if Caption <> '' then
    begin
      PNGImage.Canvas.Lock;
      try
        if Assigned(FontText) then
          PNGImage.Canvas.Font.Assign(FontText);
        PNGImage.Canvas.Brush.Style := bsClear;
        TopText := (PNGImage.Height - Abs(PNGImage.Canvas.Font.Height)) div 2;
        if SpaceForIcon then
          PNGImage.Canvas.TextOut(35, TopText - 1, Caption)
        else
          PNGImage.Canvas.TextOut(10, TopText - 1, Caption);
      finally
        PNGImage.Canvas.Unlock;
      end;
    end;
  finally
    FontText.Free;
  end;
end;

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
var
  IniFile : TIniFile;
  strFont : string;
  BackgroundPath, SeparatorPath, LogoPath, IconPath: string;
begin
  //NodeDataSize
  vstList.NodeDataSize     := SizeOf(TTreeDataX);
  vstRecents.NodeDataSize  := SizeOf(TTreeDataX);
  vstMostUsed.NodeDataSize := SizeOf(TTreeDataX);
  //Load theme
  FThemePath := IncludeTrailingBackslash(SUITE_MENUTHEMES_PATH + Config.GMTheme);
  IniFile := TIniFile.Create(FThemePath + THEME_INI);
  try
    //IniFile Section General
    //Background
    BackgroundPath := FThemePath + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEBACKGROUND, '');
    if FileExists(BackgroundPath) then
      imgBackground.Picture.LoadFromFile(BackgroundPath);
    //Logo
    LogoPath := FThemePath + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGELOGO, '');
    if FileExists(LogoPath) then
      imgLogo.Picture.LoadFromFile(LogoPath);
    //Separator
    SeparatorPath := FThemePath + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGESEPARATOR, '');
    imgDivider1.Picture.LoadFromFile(SeparatorPath);
    imgDivider2.Picture.LoadFromFile(SeparatorPath);
    //Tabs
    DrawButton(IniFile,sknbtnList,gmbList);
    DrawButton(IniFile,sknbtnRecents,gmbMRU);
    DrawButton(IniFile,sknbtnMFU,gmbMFU);
    //Right Buttons
    DrawButton(IniFile,sknbtnASuite,gmbASuite);
    DrawButton(IniFile,sknbtnOptions,gmbOptions);
    DrawButton(IniFile,sknbtnDocuments,gmbDocuments);
    DrawButton(IniFile,sknbtnMusic,gmbMusic);
    DrawButton(IniFile,sknbtnPictures,gmbPictures);
    DrawButton(IniFile,sknbtnVideos,gmbVideos);
    DrawButton(IniFile,sknbtnExplore,gmbExplore);
    DrawButton(IniFile,sknbtnAbout,gmbAbout);
    //Eject and Close Buttons
    DrawButton(IniFile,sknbtnEject,gmbEject);
    DrawButton(IniFile,sknbtnExit,gmbExit);
    //Search
    IconPath := FThemePath + IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_ICON, '');
    if FileExists(IconPath) then
      btnSearch.RightButton.ImageIndex := ImagesDM.GetSimpleIconIndex(IconPath);
    //Hard Disk
    DrawHardDiskSpace(IniFile,imgDriveBackground,imgDriveSpace);
    lblDriveName.Caption := UpperCase(ExtractFileDrive(SUITE_WORKING_PATH));
    strFont := IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_FONT, '');
    AssignFontFromString(strFont,lblDriveName.Font);
    AssignFontFromString(strFont,lblDriveSpace.Font);
    //VirtualTrees
    AssignFontFromString(IniFile.ReadString(INIFILE_SECTION_LIST, INIFILE_KEY_FONT, ''), vstList.Font);
    AssignFontFromString(IniFile.ReadString(INIFILE_SECTION_RECENTS, INIFILE_KEY_FONT, ''), vstRecents.Font);
    AssignFontFromString(IniFile.ReadString(INIFILE_SECTION_MOSTUSED, INIFILE_KEY_FONT, ''), vstMostUsed.Font);
    AssignFontFromString(IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_FONT, ''), vstSearch.Font);
  finally
    IniFile.Free;
  end;
  //Workaround for vst trasparent
  CopyImageInVst(imgBackground,pgcTreeViews);
  //Position
  Top  := Screen.WorkAreaRect.Bottom - Height;
  Left := Screen.WorkAreaRect.Right - Width;
end;

procedure TfrmGraphicMenu.FormShow(Sender: TObject);
var
  Drive        : Char;
  dblDriveSize : Double;
  dblDriveUsed : Double;
begin
  pgcTreeViews.ActivePageIndex := PG_MENULIST;
  //Clear virtualtrees
  vstList.Clear;
  vstRecents.Clear;
  vstMostUsed.Clear;
  //Refresh VirtualTrees
  frmMain.vstList.IterateSubtree(nil, PopulateMenuTree, nil, [], False);
  PopulateSpecialTree(vstRecents,MRUList,Config.MRUNumber);
  PopulateSpecialTree(vstMostUsed,MFUList,Config.MFUNumber);
  //Calculate and display the drive size
  Drive := ExtractFileDrive(SUITE_WORKING_PATH)[1];
  dblDriveSize := DiskSize(Ord(Drive) - 64);
  dblDriveUsed := dblDriveSize - DiskFree(Ord(Drive) - 64);
  imgDriveSpace.Width   := Round(dblDriveUsed/dblDriveSize * 131);
  lblDriveSpace.Caption := Format(msgGMHardDiskSpace,[DiskFreeString(Drive, True),DiskSizeString(Drive, True)])
end;

function TfrmGraphicMenu.GetActiveTree: TBaseVirtualTree;
begin
  case pgcTreeViews.ActivePageIndex of
    PG_MENULIST : Result := vstList;
    PG_MENUMRU  : Result := vstRecents;
    PG_MENUMFU  : Result := vstMostUsed;
  else
    Result := nil;
  end;
end;

function TfrmGraphicMenu.GetButtonCaption(IniFile: TIniFile;ButtonType: TGraphicMenuElement): string;
begin
  Result := '';
  case ButtonType of
    //Right buttons
    gmbASuite    : Result := msgGMASuite;
    gmbOptions   : Result := msgGMOptions;
    gmbDocuments : Result := msgGMDocuments;
    gmbMusic     : Result := msgGMMusic;
    gmbPictures  : Result := msgGMPictures;
    gmbVideos    : Result := msgGMVideos;
    gmbExplore   : Result := msgGMExplore;
    gmbAbout     : Result := msgGMAbout;
    //Tabs
    gmbList      : Result := msgList;
    gmbMRU       : Result := msgLongMRU;
    gmbMFU       : Result := msgLongMFU;
  end;
end;

function TfrmGraphicMenu.GetButtonIconPath(IniFile: TIniFile;
  ButtonType: TGraphicMenuElement): string;
begin
  Result := '';
  case ButtonType of
    gmbASuite    :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONASUITE, '');
    gmbOptions   :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONOPTIONS, '');
    gmbDocuments :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONDOCUMENT, '');
    gmbMusic     :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONMUSIC, '');
    gmbPictures  :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONPICTURES, '');
    gmbVideos    :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONVIDEOS, '');
    gmbExplore   :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONEXPLORE, '');
    gmbAbout     :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONHELP, '');
  end;
end;

function TfrmGraphicMenu.GetIniFileSection(ElementType: TGraphicMenuElement): string;
begin
  Result := '';
  //Right Buttons
  if IsRightButton(ElementType) then
    Result := INIFILE_SECTION_RIGHTBUTTONS else
  //Eject
  if ElementType in [gmbEject] then
    Result := INIFILE_SECTION_EJECTBUTTON else
  //Exit Button
  if ElementType in [gmbExit] then
    Result := INIFILE_SECTION_EXITBUTTON else
  //List Tab
  if ElementType in [gmbList] then
    Result := INIFILE_SECTION_LIST else
  //MRU Tab
  if ElementType in [gmbMRU] then
    Result := INIFILE_SECTION_RECENTS else
  //MFU Tab
  if ElementType in [gmbMFU] then
    Result := INIFILE_SECTION_MOSTUSED;
end;

procedure TfrmGraphicMenu.imgLogoMouseDown(Sender: TObject;
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

procedure TfrmGraphicMenu.imgPersonalPictureClick(Sender: TObject);
var
  TempString : string;
begin
  TempString := '';
  OpenDialog1.Filter     := 'Personal Picture [48x48] (*.jpg, *.png)|*.jpg;*.png';
  OpenDialog1.InitialDir := ExtractFileDir(RelativeToAbsolute(Config.GMPersonalPicture));
  if OpenDialog1.Execute then
  begin
    TempString := OpenDialog1.FileName;
		imgPersonalPicture.Picture.LoadFromFile(TempString);
    Config.GMPersonalPicture := AbsoluteToRelative(TempString);
  end;
  SetCurrentDir(SUITE_WORKING_PATH);
end;

function TfrmGraphicMenu.IsRightButton(ButtonType: TGraphicMenuElement): Boolean;
begin
  Result := False;
  if ButtonType in [gmbASuite,gmbOptions,gmbDocuments,gmbMusic,gmbPictures,
                    gmbVideos,gmbExplore,gmbAbout] then
    Result := True;
end;

procedure TfrmGraphicMenu.miOpenFolderSwClick(Sender: TObject);
begin
  frmMain.OpenFolder(GetActiveTree);
end;

procedure TfrmGraphicMenu.miProperty2Click(Sender: TObject);
begin
  frmMain.ShowItemProperty(GetActiveTree);
end;

procedure TfrmGraphicMenu.miRunAsAdminClick(Sender: TObject);
begin
  frmMain.RunAsAdmin(GetActiveTree);
end;

procedure TfrmGraphicMenu.miRunAsClick(Sender: TObject);
begin
  frmMain.RunAs(GetActiveTree);
end;

procedure TfrmGraphicMenu.miRunSelectedSwClick(Sender: TObject);
begin
  frmMain.RunNormalSw(GetActiveTree);
end;

procedure TfrmGraphicMenu.OpenRightButton(Sender: TObject);
begin
  if (Sender is TcySkinButton) then
  begin
    //Close Graphic Menu
    frmGraphicMenu.CloseMenu;
    if (Sender = sknbtnASuite) then
    begin
      frmMain.ShowMainForm(Sender);
      frmMain.pcList.ActivePageIndex := PG_LIST;
      frmMain.SetFocus;
    end;
    if (Sender = sknbtnOptions) then
      frmMain.miOptionsClick(Sender);
    if (Sender = sknbtnDocuments) then
      OpenFolder(Config.GMBtnDocuments);
    if (Sender = sknbtnMusic) then
      OpenFolder(Config.GMBtnMusic);
    if (Sender = sknbtnPictures) then
      OpenFolder(Config.GMBtnPictures);
    if (Sender = sknbtnVideos) then
      OpenFolder(Config.GMBtnVideos);
    if (Sender = sknbtnExplore) then
      OpenFolder(Config.GMBtnExplore);
    if (Sender = sknbtnAbout) then
    begin
      if not IsFormOpen('frmAbout') then
        Application.CreateForm(TfrmAbout, frmAbout);
      frmAbout.Show;
      frmAbout.SetFocus;
    end;
  end;
end;

procedure TfrmGraphicMenu.PopulateMenuTree(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  NewNodeData : PTreeDataX;
  NodeData, ParentNodeData : PBaseData;
  NewNode     : PVirtualNode;
begin
  if Assigned(Node) then
  begin
    NodeData := Sender.GetNodeData(Node);
    if Not(NodeData.Data.HideFromMenu) then
    begin
      if (Node.Parent <> Sender.RootNode) then
      begin
        ParentNodeData := Sender.GetNodeData(Node.Parent);
        NewNode        := vstList.AddChild(ParentNodeData.MenuNode);
      end
      else
        NewNode        := vstList.AddChild(nil);
      NewNodeData      := vstList.GetNodeData(NewNode);
      NodeData.MenuNode     := NewNode;
      //References
      NewNodeData.pNodeList := Node;
    end;
  end;
end;

procedure TfrmGraphicMenu.PopulateSpecialTree(Tree: TBaseVirtualTree;
  SList: TNodeDataList; MaxItems: Integer);
var
  NewNodeData : PTreeDataX;
  NewNode     : PVirtualNode;
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
      if Assigned(SList[I]) then
      begin
        NewNode     := Tree.AddChild(nil);
        NewNodeData := Tree.GetNodeData(NewNode);
        //References
        NewNodeData.pNodeList := TvCustomRealNodeData(SList[I]).pNode;
      end
      else
        SList.Delete(I);
    end;
  end;
  Tree.ValidateNode(Tree.RootNode,True);
  ImagesDM.GetChildNodesIcons(frmMain.vstList, Tree, Tree.RootNode, False, False);
end;

procedure TfrmGraphicMenu.sknbtnEjectClick(Sender: TObject);
begin
  EjectDialog(Sender);
end;

procedure TfrmGraphicMenu.sknbtnExitClick(Sender: TObject);
begin
  frmMain.miExitClick(Sender);
end;

procedure TfrmGraphicMenu.sknbtnListClick(Sender: TObject);
begin
  pgcTreeViews.ActivePageIndex := PG_MENULIST;
end;

procedure TfrmGraphicMenu.sknbtnMFUClick(Sender: TObject);
begin
  pgcTreeViews.ActivePageIndex := PG_MENUMFU;
end;

procedure TfrmGraphicMenu.sknbtnRecentsClick(Sender: TObject);
begin
  pgcTreeViews.ActivePageIndex := PG_MENUMRU;
end;

procedure TfrmGraphicMenu.OpenMenu;
begin
  //Fade in now
  FOpening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Self.Show;
  SetForegroundWindow(Self.Handle);
  if Not(IsWindowVisible(frmMain.Handle)) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmGraphicMenu.tmrFaderTimer(Sender: TObject);
begin
  if FOpening then
  begin
    if (Self.AlphaBlendValue < 225) and Config.GMFade then
   	  Self.AlphaBlendValue := Self.AlphaBlendValue + 30
    else begin
 	    Self.AlphaBlendValue := 255;
   	  tmrFader.Enabled     := False;
    end;
  end
  else begin
    if (Self.AlphaBlendValue > 30) and Config.GMFade then
      Self.AlphaBlendValue := Self.AlphaBlendValue - 30
    else begin
      Self.AlphaBlendValue := 0;
      tmrFader.Enabled     := False;
      Self.Hide;
    end;
  end;
end;

procedure TfrmGraphicMenu.tmrWatchFocusTimer(Sender: TObject);
begin
  //if menu lost its focus, it hide
  if (getFocus() = 0) then
    CloseMenu;
end;

procedure TfrmGraphicMenu.vstListExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
  NodeData : TvBaseNodeData;
begin
  NodeData := GetNodeDataSearch(Node,vstList,frmMain.vstList).Data;
  if NodeData.DataType = vtdtCategory then
    ImagesDM.GetChildNodesIcons(frmMain.vstList, Sender, Node);
end;

procedure TfrmGraphicMenu.vstNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  NodeData : PBaseData;
begin
  NodeData := GetNodeDataSearch(HitInfo.HitNode,vstList,frmMain.vstList);
  if NodeData.Data.DataType = vtdtCategory then
    Sender.Expanded[HitInfo.HitNode] := Not(Sender.Expanded[HitInfo.HitNode])
  else
    if NodeData.Data.DataType = vtdtFile then
    begin
      frmMain.RunDoubleClick(Sender);
      CloseMenu;
    end;
end;

procedure TfrmGraphicMenu.vstGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : TvBaseNodeData;
begin
  NodeData   := GetNodeDataSearch(Node,vstList,frmMain.vstList).Data;
  ImageIndex := NodeData.ImageIndex;
end;

procedure TfrmGraphicMenu.vstGetImageLargeIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : TvBaseNodeData;
begin
  NodeData   := GetNodeDataSearch(Node,Sender,frmMain.vstList).Data;
  ImageIndex := NodeData.ImageLargeIndex;
end;

procedure TfrmGraphicMenu.vstGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData : PBaseData;
  I   : Byte;
  str : string;
begin
  NodeData := GetNodeDataSearch(Node,vstList,frmMain.vstList);
  if Assigned(NodeData) then
  begin
    if NodeData.Data.DataType = vtdtSeparator then
    begin
      I := 50 - (Length(NodeData.Data.Name));
      str := '---------------------------';
      if I >= 10 then
        SetLength(str, I div 2)
      else
        SetLength(str, 5);
      CellText := str + ' ' + NodeData.Data.Name + ' ' + str;
    end
    else
      CellText := StringReplace(NodeData.Data.Name, '&&', '&', [rfIgnoreCase,rfReplaceAll]);
  end;
end;

procedure TfrmGraphicMenu.vstKeyPress(Sender: TObject; var Key: Char);
begin
  frmMain.vstListKeyPress(Sender, Key);
  CloseMenu;
end;

procedure TfrmGraphicMenu.vstMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Node : PVirtualNode;
begin
  if (Sender is TBaseVirtualTree) then
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
  end;
end;

procedure TfrmGraphicMenu.vstInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.NodeHeight := 36;
end;

{ TPageControl }

procedure TPageControl.TCMAdjustRect(var Msg: TMessage);
begin
  if Self.TabPosition = tpTop then
  begin
    PRect(Msg.LParam)^.Left  := 0;
    PRect(Msg.LParam)^.Right := Self.ClientWidth;
    Dec(PRect(Msg.LParam)^.Top, 2);
    PRect(Msg.LParam)^.Bottom := Self.ClientHeight;
  end
  else
    inherited;
end;

end.
