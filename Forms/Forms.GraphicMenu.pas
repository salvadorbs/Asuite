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

unit Forms.GraphicMenu;

interface

uses
  Windows, Classes, Forms, StdCtrls, Buttons, ExtCtrls, ComCtrls, Messages,
	ShellAPI, Controls, Graphics, Dialogs, SysUtils, VirtualTrees, AppEvnts,
  Vcl.Imaging.pngimage, cySkinButton, IniFiles, Lists.Manager, Vcl.Menus,
  DKLang, Lists.Base;

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
      bsClicked,
      bsDisabled
  );

  TVSTHelper = class helper for TBaseVirtualTree
  public
    procedure SetCurrentHotNode(const Value: PVirtualNode);
  end;

	TfrmGraphicMenu = class(TForm)
  	imgDriveSpace: TImage;
	  imgDivider2: TImage;
  	lblDriveName: TLabel;
  	lblDriveSpace: TLabel;
   	tmrFader: TTimer;
    imgLogo: TImage;
    imgPersonalPicture: TImage;
    vstList: TVirtualStringTree;
    OpenDialog1: TOpenDialog;
    imgDivider1: TImage;
    ApplicationEvents1: TApplicationEvents;
    sknbtnASuite: TcySkinButton;
    sknbtnOptions: TcySkinButton;
    sknbtnDocuments: TcySkinButton;
    sknbtnPictures: TcySkinButton;
    sknbtnAbout: TcySkinButton;
    sknbtnExplore: TcySkinButton;
    sknbtnVideos: TcySkinButton;
    sknbtnMusic: TcySkinButton;
    edtSearch: TButtonedEdit;
    sknbtnList: TcySkinButton;
    sknbtnRecents: TcySkinButton;
    sknbtnMFU: TcySkinButton;
    sknbtnEject: TcySkinButton;
    sknbtnExit: TcySkinButton;
    imgBackground: TImage;
    imgDriveBackground: TImage;
    pmWindow: TPopupMenu;
    miRunSelectedSw: TMenuItem;
    miRunAs: TMenuItem;
    miRunAsAdmin: TMenuItem;
    miOpenFolderSw: TMenuItem;
    N6: TMenuItem;
    miProperty2: TMenuItem;
    DKLanguageController1: TDKLanguageController;
    imgUserFrame: TImage;
    imgDragSpaceHidden: TImage;
    tmrCheckItems: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrFaderTimer(Sender: TObject);
    procedure imgLogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenRightButton(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sknbtnListClick(Sender: TObject);
    procedure sknbtnRecentsClick(Sender: TObject);
    procedure sknbtnMFUClick(Sender: TObject);
    procedure sknbtnEjectClick(Sender: TObject);
    procedure sknbtnExitClick(Sender: TObject);
    procedure imgPersonalPictureClick(Sender: TObject);
    procedure miProperty2Click(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pmWindowPopup(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure tmrCheckItemsTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
	private    
    { Private declarations }
    FOpening    : Boolean;
    FSearchIcon : Integer;
    FCancelIcon : Integer;
    procedure CopyImageInVst(Source:TImage; Tree: TVirtualStringTree);
    procedure CopySelectedRectInBitmap(Source:TImage;Comp: TControl;bmp: TBitmap);
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
    procedure DrawHardDiskSpace(IniFile: TIniFile; DriveBackGround, DriveSpace: TImage);
    procedure DrawEmptyButton(PNGImage: TPngImage; Button: TcySkinButton);
    procedure InternalLoadTheme;
    procedure DoClickOnTaskbar;
    procedure UpdateDriveStats;
    procedure CheckUserPicture;

    procedure PopulateListTree(const ATree: TVirtualStringTree);
    procedure PopulateSpecialTree(const ATree: TVirtualStringTree; AList: TBaseItemsList; MaxItems: Integer);
	public
    { Public declarations }
    procedure OpenMenu;
  	procedure CloseMenu;
    procedure LoadTheme;
  end;

var
	frmGraphicMenu : TfrmGraphicMenu;

  //TODO: Move these consts in GraphicMenu.Consts
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
  INIFILE_KEY_IMAGEUSERFRAME  = 'image_userframe';
  INIFILE_KEY_IMAGELOGO    = 'image_logo';
  INIFILE_KEY_IMAGESPACE   = 'image_space';
  INIFILE_KEY_IMAGESEPARATOR = 'image_separator';
  //Fonts
  INIFILE_KEY_FONTNORMAL   = 'font_normal';
  INIFILE_KEY_FONTHOVER    = 'font_hover';
  INIFILE_KEY_FONTCLICKED  = 'font_clicked';
  INIFILE_KEY_FONTDISABLED = 'font_disabled';
  INIFILE_KEY_FONTTREE     = 'font_tree';
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
  INIFILE_KEY_ICONSEARCH   = 'icon_search';
  INIFILE_KEY_ICONCANCEL   = 'icon_cancel';
  INIFILE_KEY_ICON         = 'icon'; //Generic icon key

implementation

{$R *.dfm}

uses
  Forms.Main, Utility.System, Kernel.Consts, AppConfig.Main, Utility.Conversions,
  Forms.About, NodeDataTypes.Base, Kernel.Enumerations, Forms.Options,
  Utility.Misc, VirtualTree.Events, VirtualTree.Methods, Kernel.Types,
  NodeDataTypes.Custom;

procedure TfrmGraphicMenu.ApplicationEvents1Deactivate(Sender: TObject);
begin
  //Check if GraphicMenu is not in opening state
  if Not(tmrFader.Enabled) then
    CloseMenu;
end;

procedure TfrmGraphicMenu.edtSearchChange(Sender: TObject);
var
  Node: PVirtualNode;
begin
  //Clear vstList
  vstList.Clear;
  if edtSearch.Text <> '' then
  begin
    edtSearch.RightButton.ImageIndex := FCancelIcon;
    //Do search
    //Change node height and imagelist
    TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, False);
    frmMain.DoSearchItem(vstList, edtSearch.Text, stName);
    vstList.SortTree(-1, sdAscending);
    //Get icons
    //TODO: Fix it
//    dmImages.GetChildNodesIcons(vstList, vstList.RootNode, isAny);
    //Set first node as HotNode
    Node := vstList.GetFirst;
    if Assigned(Node) then
      vstList.SetCurrentHotNode(Node);
  end
  else begin
    edtSearch.RightButton.ImageIndex := FSearchIcon;
    //Change node height and imagelist
    TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, True);
    PopulateListTree(vstList);
  end;
end;

procedure TfrmGraphicMenu.btnSearchClick(Sender: TObject);
begin
  edtSearch.Text := '';
end;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  FOpening := False;
  tmrFader.Enabled:= True;
end;

procedure TfrmGraphicMenu.LoadTheme;
begin
  sknbtnRecents.Enabled := Config.MRU;
  sknbtnMFU.Enabled := Config.MFU;
  //Load graphics
  InternalLoadTheme;
  //Set PopUpMenu's ImageIndexes
//  miRunSelectedSw.ImageIndex := Config.ASuiteIcons.PopupMenu.Run;
//  miProperty2.ImageIndex   := Config.ASuiteIcons.PopupMenu.Properties;
end;

procedure TfrmGraphicMenu.CheckUserPicture;
var
  sTempPath: string;
begin
  //User Picture
  if (Config.GMPersonalPicture = 'PersonalPicture.jpg') and (not FileExists(Config.GMPersonalPicture)) then
    sTempPath := Config.Paths.SuitePathCurrentTheme + Config.GMPersonalPicture
  else
    sTempPath := Config.Paths.RelativeToAbsolute(Config.GMPersonalPicture);
  if FileExists(sTempPath) then
    imgPersonalPicture.Picture.LoadFromFile(sTempPath);
  imgPersonalPicture.Visible := (FileExists(sTempPath));
  imgUserFrame.Visible := (FileExists(sTempPath));
end;

procedure TfrmGraphicMenu.UpdateDriveStats;
var
  dblDriveSize: Double;
  Drive: Char;
  dblDriveUsed: Double;
begin
  //Calculate and display the drive size
  Drive := Config.Paths.SuiteDrive[1];
  dblDriveSize := DiskSize(Ord(Drive) - 64);
  dblDriveUsed := dblDriveSize - DiskFree(Ord(Drive) - 64);
  imgDriveSpace.Width := Round(dblDriveUsed / dblDriveSize * 131);
  lblDriveSpace.Caption := Format(DKLangConstW('msgGMHardDiskSpace'), [DiskFreeString(Drive, True), DiskSizeString(Drive, True)]);
end;

procedure TfrmGraphicMenu.DoClickOnTaskbar;
var
  TrayHandle: THandle;
begin
  TrayHandle := FindWindow('Shell_TrayWnd', '');
  TrayHandle := FindWindowEx(TrayHandle, 0, 'TrayNotifyWnd', nil);
  TrayHandle := FindWindowEx(TrayHandle, 0, 'SysPager', nil);
  TrayHandle := FindWindowEx(TrayHandle, 0, 'ToolbarWindow32', nil);
  PostMessage(TrayHandle, WM_LBUTTONDOWN, MK_LBUTTON, 0);
  PostMessage(TrayHandle, WM_LBUTTONUP, MK_LBUTTON, 0);
end;

procedure TfrmGraphicMenu.InternalLoadTheme;
var
  BackgroundPath: string;
  sTempPath: string;
  IniFile: TIniFile;
  strFont: string;
begin
  //Load theme
  if FileExists(Config.Paths.SuitePathCurrentTheme + THEME_INI) then
  begin
    IniFile := TIniFile.Create(Config.Paths.SuitePathCurrentTheme + THEME_INI);
    try
      //IniFile Section General
      //Background
      BackgroundPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEBACKGROUND, '');
      if FileExists(BackgroundPath) then
        imgBackground.Picture.LoadFromFile(BackgroundPath);
      //User frame
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEUSERFRAME, '');
      if FileExists(sTempPath) then
        imgUserFrame.Picture.LoadFromFile(sTempPath);
      //Logo
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGELOGO, '');
      if FileExists(sTempPath) then
        imgLogo.Picture.LoadFromFile(sTempPath);
      //Separator
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGESEPARATOR, '');
      imgDivider1.Picture.LoadFromFile(sTempPath);
      imgDivider2.Picture.LoadFromFile(sTempPath);
      //Tabs
      DrawButton(IniFile, sknbtnList, gmbList);
      DrawButton(IniFile, sknbtnRecents, gmbMRU);
      DrawButton(IniFile, sknbtnMFU, gmbMFU);
      //Right Buttons
      DrawButton(IniFile, sknbtnASuite, gmbASuite);
      DrawButton(IniFile, sknbtnOptions, gmbOptions);
      DrawButton(IniFile, sknbtnDocuments, gmbDocuments);
      DrawButton(IniFile, sknbtnMusic, gmbMusic);
      DrawButton(IniFile, sknbtnPictures, gmbPictures);
      DrawButton(IniFile, sknbtnVideos, gmbVideos);
      DrawButton(IniFile, sknbtnExplore, gmbExplore);
      DrawButton(IniFile, sknbtnAbout, gmbAbout);
      //Eject and Close Buttons
      DrawButton(IniFile, sknbtnEject, gmbEject);
      DrawButton(IniFile, sknbtnExit, gmbExit);
      //Search
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_ICONSEARCH, '');
//      if FileExists(sTempPath) then
//        FSearchIcon := ImagesDM.GetSimpleIconIndex(sTempPath, True);
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_ICONCANCEL, '');
//      if FileExists(sTempPath) then
//        FCancelIcon := ImagesDM.GetSimpleIconIndex(sTempPath, True);
      edtSearch.RightButton.ImageIndex := FSearchIcon;
      //Hard Disk
      DrawHardDiskSpace(IniFile, imgDriveBackground, imgDriveSpace);
      lblDriveName.Caption := format(DKLangConstW('msgGMDriveName'), [UpperCase(Config.Paths.SuiteDrive)]);
      //Fonts
      strFont := IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_FONT, '');
      StrToFont(strFont, lblDriveName.Font);
      StrToFont(strFont, lblDriveSpace.Font);
      //VirtualTrees
      StrToFont(IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_FONTTREE, ''), vstList.Font);
      //Workaround for vst trasparent
      CopyImageInVst(imgBackground, vstList);
    finally
      IniFile.Free;
    end;
  end
  else
    ShowMessageFmtEx(DKLangConstW('msgErrNoThemeIni'), [Config.Paths.SuitePathCurrentTheme + THEME_INI], True);
end;

procedure TfrmGraphicMenu.DrawEmptyButton(PNGImage: TPngImage; Button: TcySkinButton);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    CopySelectedRectInBitmap(imgBackground, Button, bmp);
    PNGImage.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TfrmGraphicMenu.DrawHardDiskSpace(IniFile: TIniFile; DriveBackGround, DriveSpace: TImage);
var
  HDPath, HDSpacePath: string;
begin
  //Hard Disk Space
  HDPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_IMAGEBACKGROUND, '');
  HDSpacePath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_IMAGESPACE, '');
  if FileExists(HDPath) then
    DriveBackGround.Picture.LoadFromFile(HDPath);
  if FileExists(HDSpacePath) then
    DriveSpace.Picture.LoadFromFile(HDSpacePath);
end;

procedure TfrmGraphicMenu.OpenFolder(FolderPath: string);
var
  ErrorCode: Integer;
  sPath: string;
begin
  sPath := Config.Paths.RelativeToAbsolute(FolderPath);
  ErrorCode := ShellExecute(GetDesktopWindow, 'open', PChar(sPath), PChar(''), PChar(sPath), SW_SHOWDEFAULT);
  if ErrorCode <= 32 then
    ShowMessageFmtEx(DKLangConstW('msgErrGeneric'), ['', SysErrorMessage(ErrorCode)], True);
end;

procedure TfrmGraphicMenu.CopyImageInVst(Source: TImage; Tree: TVirtualStringTree);
var
  bmpTempImage : TBitmap;
begin
  bmpTempImage := TBitmap.Create;
  try
    CopySelectedRectInBitmap(Source, Tree, bmpTempImage);
    Tree.Background.Bitmap := bmpTempImage;
  finally
    bmpTempImage.Free;
  end;
end;

procedure TfrmGraphicMenu.CopySelectedRectInBitmap(Source: TImage;Comp: TControl;
  bmp: TBitmap);
var
  RectSource, RectDest : TRect;
  bmpTempBG : TBitmap;
begin
  if Assigned(bmp) then
  begin
    bmpTempBG    := TBitmap.Create;
    try
      bmp.Height := Comp.Height;
      bmp.Width  := Comp.Width;
      //Set RectSource size
      RectSource.Left     := Comp.Left;
      RectSource.Top      := Comp.Top;
      RectSource.Right    := Comp.Left + Comp.Width;
      RectSource.Bottom   := Comp.Top + Comp.Height;
      //Set RectDest size
      RectDest.Left       := 0;
      RectDest.Top        := 0;
      RectDest.Right      := Comp.Width;
      RectDest.Bottom     := Comp.Height;
      //CopyRect in bmpTempImage
      bmpTempBG.Width := Source.Picture.Width;
      bmpTempBG.Height := Source.Picture.Height;
      bmpTempBG.Canvas.Draw(0, 0, Source.Picture.Graphic);
      bmp.Canvas.CopyRect(RectDest, bmpTempBG.Canvas, RectSource);
    finally
      bmpTempBG.Free;
    end;
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
    Image_Normal  := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(IniFile_Section, INIFILE_KEY_IMAGENORMAL, '');
    Image_Hover   := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(IniFile_Section, INIFILE_KEY_IMAGEHOVER, '');
    Image_Clicked := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(IniFile_Section, INIFILE_KEY_IMAGECLICKED, '');
    //Load png button states
    //Normal state
    if FileExists(Image_Normal) then
      PNGImage_Normal.LoadFromFile(Image_Normal)
    else
      DrawEmptyButton(PNGImage_Normal, Button);
    //Hover state
    if FileExists(Image_Hover) then
      PNGImage_Hover.LoadFromFile(Image_Hover)
    else
      DrawEmptyButton(PNGImage_Hover, Button);
    //Clicked state
    if FileExists(Image_Clicked) then
      PNGImage_Clicked.LoadFromFile(Image_Clicked)
    else
      DrawEmptyButton(PNGImage_Clicked, Button);
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
        if Button.Enabled then
          DrawTextInPNGImage(IniFile,bsNormal,PNGImage_Normal,ButtonType,False)
        else
          DrawTextInPNGImage(IniFile,bsDisabled,PNGImage_Normal,ButtonType,False);
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
    if FileExists(Config.Paths.SuitePathCurrentTheme + IconPath) then
    begin
      Icon.LoadFromFile(Config.Paths.SuitePathCurrentTheme + IconPath);
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
  DrawRect, R: TRect;
  DrawFlags: Cardinal;
begin
  if Not Assigned(PNGImage) then
    Exit;
  FontText := TFont.Create;
  try
    IniFile_Section := GetIniFileSection(ButtonType);
    //Get font
    case ButtonState of
      bsNormal   : StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTNORMAL, ''), FontText);
      bsHover    : StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTHOVER, ''), FontText);
      bsClicked  : StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTCLICKED, ''), FontText);
      bsDisabled : StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTDISABLED, ''), FontText);
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
          PNGImage.Canvas.TextOut(35, TopText - 2, Caption)
        else begin
          //Draw caption in center
          SetRect(R, 0, 0, PNGImage.Width, PNGImage.Height);
          DrawRect  := R;
          DrawFlags := DT_END_ELLIPSIS or DT_NOPREFIX or DT_WORDBREAK or
            DT_EDITCONTROL or DT_CENTER;
          DrawText(PNGImage.Canvas.Handle, PChar(Caption), -1, DrawRect, DrawFlags or DT_CALCRECT);
          DrawRect.Right := R.Right;
          if DrawRect.Bottom < R.Bottom then
            OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
          else
            DrawRect.Bottom := R.Bottom;
          DrawTextEx(PNGImage.Canvas.Handle, PChar(Caption), -1, DrawRect, DrawFlags, nil);
        end;
      finally
        PNGImage.Canvas.Unlock;
      end;
    end;
  finally
    FontText.Free;
  end;
end;

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
begin
  FSearchIcon := -1;
  FCancelIcon := -1;
  TVirtualTreeEvents.Create.SetupVSTGraphicMenu(vstList, Self);
  //Load current theme
  LoadTheme;
//  //Position
//  if Config.GMPositionTop <> -1 then
//    Self.Top  := Config.GMPositionTop
//  else
//    Self.Top  := Screen.WorkAreaRect.Bottom - Height;
//  if Config.GMPositionLeft <> -1 then
//    Self.Left  := Config.GMPositionLeft
//  else
//    Self.Left  := Screen.WorkAreaRect.Right - Width;
end;

procedure TfrmGraphicMenu.FormDeactivate(Sender: TObject);
begin
  //if menu lost its focus, it hide
  CloseMenu;
//  //Save position - TOP
//  if Self.Top <> Config.GMPositionTop then
//  begin
//    Config.GMPositionTop := Self.Top;
//    Config.Changed := True;
//  end;
//  //Save position - LEFT
//  if Self.Left <> Config.GMPositionLeft then
//  begin
//    Config.GMPositionLeft := Self.Left;
//    Config.Changed := True;
//  end;
end;

procedure TfrmGraphicMenu.FormHide(Sender: TObject);
begin
  tmrCheckItems.Enabled := False;
end;

procedure TfrmGraphicMenu.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CurrentNode: PVirtualNode;
  NodeData: TvBaseNodeData;
begin
  CurrentNode := vstList.HotNode;
  case Ord(Key) of
    VK_UP:
      begin
        Key := 0;
        CurrentNode := vstList.GetPreviousVisible(vstList.HotNode);
        if Not Assigned(CurrentNode) then
          CurrentNode := vstList.GetLast;
      end;
    VK_DOWN:
      begin
        Key := 0;
        CurrentNode := vstList.GetNextVisible(vstList.HotNode);
        if Not Assigned(CurrentNode) then
          CurrentNode := vstList.GetFirst;
      end;
    VK_RETURN:
      begin
        Key := 0;
        if Assigned(CurrentNode) then
        begin
          NodeData := TVirtualTreeMethods.Create.GetNodeItemData(CurrentNode, vstList);
          if Assigned(NodeData) then
          begin
            //TODO: Fix it
//            if NodeData.DataType = vtdtFile then
//              frmMain.RunNormalSw(vstList)
//            else
//              if (NodeData.DataType = vtdtCategory) and (ssCtrl in Shift) then
//                frmMain.RunNormalSw(vstList);
            //TODO: If node is a category, expand it
          end;
        end;
      end;
    VK_LEFT:
      begin
        if Assigned(CurrentNode) then
          vstList.Expanded[CurrentNode] := False;
      end;
    VK_RIGHT:
      begin
        if Assigned(CurrentNode) then
          vstList.Expanded[CurrentNode] := True;
      end;
  end;
  if Assigned(CurrentNode) then
    vstList.SetCurrentHotNode(CurrentNode);
end;

procedure TfrmGraphicMenu.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = (VK_RETURN) then
    Key := #0;
end;

procedure TfrmGraphicMenu.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //Scroll vstList using WheelDelta
  vstList.OffsetY := vstList.OffsetY + WheelDelta;
end;

procedure TfrmGraphicMenu.FormShow(Sender: TObject);
begin
  CheckUserPicture;
  //Clear edtSearch and focus it
  edtSearch.Text := '';
  Self.FocusControl(edtSearch);
  //Change node height and imagelist
  TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, True);
  //Clear and populate virtualtree
  PopulateListTree(vstList);
  UpdateDriveStats;
  //Timer
  tmrCheckItems.Enabled := True;
end;

function TfrmGraphicMenu.GetButtonCaption(IniFile: TIniFile;ButtonType: TGraphicMenuElement): string;
begin
  Result := '';
  case ButtonType of
    //Right buttons
    gmbASuite    : Result := Format(DKLangConstW('msgGMShow'), [APP_NAME]);
    gmbOptions   : Result := DKLangConstW('msgGMOptions');
    gmbDocuments : Result := DKLangConstW('msgGMDocuments');
    gmbMusic     : Result := DKLangConstW('msgGMMusic');
    gmbPictures  : Result := DKLangConstW('msgGMPictures');
    gmbVideos    : Result := DKLangConstW('msgGMVideos');
    gmbExplore   : Result := DKLangConstW('msgGMExplore');
    gmbAbout     : Result := DKLangConstW('msgGMAbout');
    //Tabs
    gmbList      : Result := DKLangConstW('msgList');
    gmbMRU       : Result := DKLangConstW('msgLongMRU');
    gmbMFU       : Result := DKLangConstW('msgLongMFU');
  end;
end;

function TfrmGraphicMenu.GetButtonIconPath(IniFile: TIniFile;
  ButtonType: TGraphicMenuElement): string;
begin
  Result := '';
  case ButtonType of
    gmbASuite    :
      Result := IniFile.ReadString(INIFILE_SECTION_RIGHTBUTTONS, INIFILE_KEY_ICONASuite, '');
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
  OpenDialog1.Filter     := DKLangConstW('msgFilterPicture');
  OpenDialog1.InitialDir := ExtractFileDir(Config.Paths.RelativeToAbsolute(Config.GMPersonalPicture));
  if OpenDialog1.Execute then
  begin
    TempString := OpenDialog1.FileName;
		imgPersonalPicture.Picture.LoadFromFile(TempString);
    Config.GMPersonalPicture := Config.Paths.AbsoluteToRelative(TempString);
    Config.Changed := True;
  end;
end;

function TfrmGraphicMenu.IsRightButton(ButtonType: TGraphicMenuElement): Boolean;
begin
  Result := False;
  if ButtonType in [gmbASuite,gmbOptions,gmbDocuments,gmbMusic,gmbPictures,
                    gmbVideos,gmbExplore,gmbAbout] then
    Result := True;
end;

procedure TfrmGraphicMenu.miProperty2Click(Sender: TObject);
begin
  TVirtualTreeMethods.Create.ShowItemProperty(Self, vstList, vstList.FocusedNode, False);
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
      TfrmOptions.Execute(Self);
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
      //TODO: Make a function (see mainform, also)
      if not IsFormOpen('frmAbout') then
        Application.CreateForm(TfrmAbout, frmAbout);
      frmAbout.Show;
      frmAbout.SetFocus;
    end;
  end;
end;

procedure TfrmGraphicMenu.pmWindowPopup(Sender: TObject);
var
  NodeData : TvBaseNodeData;
begin
  if Assigned(vstList.FocusedNode) then
  begin
    NodeData := TVirtualTreeMethods.Create.GetNodeItemData(vstList.FocusedNode, vstList);
    miRunSelectedSw.Enabled := (NodeData.DataType <> vtdtSeparator);
    miRunAs.Enabled         := (NodeData.DataType <> vtdtSeparator);
    miRunAsAdmin.Enabled    := (NodeData.DataType <> vtdtSeparator);
    miOpenFolderSw.Enabled  := (NodeData.DataType in [vtdtFile,vtdtFolder]);
  end;
end;

procedure TfrmGraphicMenu.PopulateListTree(const ATree: TVirtualStringTree);
begin
  ATree.Clear;
  ATree.BeginUpdate;
  try
    //Populate and get icons from first level
    Config.MainTree.IterateSubtree(nil, TVirtualTreeMethods.Create.AddNodeInTreeFromMainTree, @ATree);
  finally
    ATree.EndUpdate;
    //Check nodes path
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(ATree);
  end;
end;

procedure TfrmGraphicMenu.PopulateSpecialTree(const ATree: TVirtualStringTree;
  AList: TBaseItemsList; MaxItems: Integer);
var
  NewNodeData  : PTreeDataX;
  NewNode      : PVirtualNode;
  I, ItemCount : Integer;
begin
  ATree.Clear;
  ATree.BeginUpdate;
  try
    //Change node height and imagelist
    TVirtualTreeMethods.Create.ChangeTreeIconSize(ATree, False);
    //Set limit based on MaxItems or AList.Count
    if MaxItems < AList.Count then
      ItemCount := MaxItems
    else
      ItemCount := AList.Count;
    for I := 0 to ItemCount - 1 do
    begin
      if Assigned(AList[I]) then
      begin
        //Create MenuItem
        if Assigned(AList[I]) then
        begin
          NewNode     := ATree.AddChild(nil);
          NewNodeData := ATree.GetNodeData(NewNode);
          //References
          NewNodeData.pNodeList := TvCustomRealNodeData(AList[I]).pNode;
        end
        else
          AList.Delete(I);
      end;
    end;
  finally
    ATree.EndUpdate;
    ATree.ValidateNode(ATree.RootNode, True);
    //Check nodes path
    //TODO: Fix it
//ImagesDM.GetChildNodesIcons(ATree, ATree.RootNode, isAny);
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(ATree);
  end;
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
  //Change node height and imagelist
  TVirtualTreeMethods.Create.ChangeTreeIconSize(vstList, True);
  PopulateListTree(vstList);
  edtSearch.Text := '';
end;

procedure TfrmGraphicMenu.sknbtnMFUClick(Sender: TObject);
begin
  PopulateSpecialTree(vstList, Config.ListManager.MFUList, Config.MFUNumber);
end;

procedure TfrmGraphicMenu.sknbtnRecentsClick(Sender: TObject);
begin
  PopulateSpecialTree(vstList, Config.ListManager.MRUList, Config.MRUNumber);
end;

procedure TfrmGraphicMenu.OpenMenu;
begin
  //Workaround: Avoid to open windows' context menu too
  DoClickOnTaskbar;
  //Fade in now
  FOpening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Self.Show;
  SetForegroundWindow(Self.Handle);
  if Not(IsWindowVisible(frmMain.Handle)) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmGraphicMenu.tmrCheckItemsTimer(Sender: TObject);
begin
  if Config.ASuiteState = lsNormal then
    TVirtualTreeMethods.Create.CheckVisibleNodePathExe(vstList);
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

{ TVSTHelper }

//TODO: Move this code in another unit (VirtualTree.Helper)

type
  THackOptions = Class(TCustomVirtualTreeOptions);

procedure TVSTHelper.SetCurrentHotNode(const Value: PVirtualNode);
var
  DoInvalidate: Boolean;
const
  MouseButtonDown = [tsLeftButtonDown, tsMiddleButtonDown, tsRightButtonDown];
begin
 if Self.FCurrentHotNode <> Value then
 begin
   DoInvalidate := (toHotTrack in THackOptions(Self.FOptions).PaintOptions) or (toCheckSupport in THackOptions(Self.FOptions).MiscOptions);
   DoHotChange(Self.FCurrentHotNode, Value);
   //Invalidate old Self.FCurrentHotNode
   if Assigned(Self.FCurrentHotNode) and DoInvalidate then
     InvalidateNode(Self.FCurrentHotNode);
   //Set new Self.FCurrentHotNode and invalidate it
   Self.FCurrentHotNode := Value;
   if Assigned(Self.FCurrentHotNode) and DoInvalidate then
     InvalidateNode(Self.FCurrentHotNode);
   //Scroll view
   if (Self.FUpdateCount = 0) and not (toDisableAutoscrollOnFocus in THackOptions(Self.FOptions).AutoOptions) then
     ScrollIntoView(Self.FCurrentHotNode, (toCenterScrollIntoView in THackOptions(Self.FOptions).SelectionOptions) and
        (MouseButtonDown * Self.FStates = []), not (toFullRowSelect in THackOptions(Self.FOptions).SelectionOptions) );
  end;
end;

end.
