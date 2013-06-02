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
  Vcl.Imaging.pngimage, cySkinButton, IniFiles;

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

	TfrmGraphicMenu = class(TForm)
  	imgDriveSpace: TImage;
	  imgDivider2: TImage;
  	lblDriveName: TLabel;
  	lblDriveSpace: TLabel;
   	tmrFader: TTimer;
    imgLogo: TImage;
    imgPersonalPicture: TImage;
    bvlPersonalPicture: TBevel;
    vstMenu: TVirtualStringTree;
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
    procedure FormCreate(Sender: TObject);
    procedure tmrFaderTimer(Sender: TObject);
    procedure imgLogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenRightButton(Sender: TObject);
	private    
    { Private declarations }
    FOpening   : Boolean;
    FThemePath : string;
    procedure CopyImageInVst(Source:TImage;Dest:TVirtualStringTree);
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
  //Fonts
  INIFILE_KEY_FONTNORMAL   = 'font_normal';
  INIFILE_KEY_FONTHOVER    = 'font_hover';
  INIFILE_KEY_FONTCLICKED  = 'font_clicked';
  INIFILE_KEY_FONT         = 'font'; //For treeviews
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

implementation

{$R *.dfm}

uses
  Main, Option, ulSysUtils, AppConfig, ulAppConfig, ulCommonUtils, About;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  FOpening := False;
  tmrFader.Enabled:= True;
end;

procedure TfrmGraphicMenu.OpenFolder(FolderPath: string);
var
  ErrorCode: Integer;
begin
  ErrorCode := ShellExecute(GetDesktopWindow, 'open', PChar(FolderPath), PChar(''), PChar(FolderPath), SW_SHOWDEFAULT);
  if ErrorCode <= 32 then
    ShowMessageFmt(msgErrGeneric, ['', SysErrorMessage(ErrorCode)]);
end;

procedure TfrmGraphicMenu.CopyImageInVst(Source: TImage;Dest: TVirtualStringTree);
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
    //TODO: Check this IF
    if (Source.Picture.graphic is TPNGImage) then
    begin
      bmpTempBG.Assign(Source.Picture.Graphic);
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
      PNGImage.Canvas.Draw(5, 3, Icon);
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
      if Assigned(FontText) then
        PNGImage.Canvas.Font.Assign(FontText);
      PNGImage.Canvas.Brush.Style := bsClear;
      TopText := (PNGImage.Height - Abs(PNGImage.Canvas.Font.Height)) div 2;
      if SpaceForIcon then
        PNGImage.Canvas.TextOut(35, TopText - 1, Caption)
      else
        PNGImage.Canvas.TextOut(10, TopText - 1, Caption);
    end;
  finally
    FontText.Free;
  end;
end;

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
  BackgroundPath, LogoPath: string;
begin
  FThemePath := IncludeTrailingBackslash(SUITE_MENUTHEMES_PATH + Config.GraphicMenuTheme);
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
  finally
    IniFile.Free;
  end;
  //Position
  Top  := Screen.WorkAreaRect.Bottom - Height;
  Left := Screen.WorkAreaRect.Right - Width;
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

function TfrmGraphicMenu.IsRightButton(ButtonType: TGraphicMenuElement): Boolean;
begin
  Result := False;
  if ButtonType in [gmbASuite,gmbOptions,gmbDocuments,gmbMusic,gmbPictures,
                    gmbVideos,gmbExplore,gmbAbout] then
    Result := True;
end;

procedure TfrmGraphicMenu.OpenRightButton(Sender: TObject);
begin
  if (Sender is TcySkinButton) then
  begin
    if (Sender = sknbtnASuite) then
    begin
      frmMain.ShowMainForm(Sender);
      frmMain.pcList.ActivePageIndex := 0;
      frmMain.SetFocus;
    end;
    if (Sender = sknbtnOptions) then
      frmMain.miOptionsClick(Sender);
    //TODO: Use Config.ButtonPATH*
    if (Sender = sknbtnDocuments) then
      OpenFolder('e:\documents');
    if (Sender = sknbtnMusic) then
      OpenFolder('e:\music');
    if (Sender = sknbtnPictures) then
      OpenFolder('e:\pictures');
    if (Sender = sknbtnVideos) then
      OpenFolder('e:\videos');
    if (Sender = sknbtnExplore) then
      OpenFolder('e:\');
    if (Sender = sknbtnAbout) then
    begin
      if not IsFormOpen('frmAbout') then
        Application.CreateForm(TfrmAbout, frmAbout);
      frmAbout.Show;
      frmAbout.SetFocus;
    end;
  end;
end;

procedure TfrmGraphicMenu.OpenMenu;
begin
  //Fade in now
  FOpening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Show;
  SetForegroundWindow(Handle);
  if Not(IsWindowVisible(frmMain.Handle)) then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmGraphicMenu.tmrFaderTimer(Sender: TObject);
begin
  if FOpening then
  begin
    if (Self.AlphaBlendValue < 225) and Config.GraphicMenuFade then
   	  Self.AlphaBlendValue := Self.AlphaBlendValue + 30
    else begin
 	    Self.AlphaBlendValue := 255;
   	  tmrFader.Enabled     := False;
    end;
  end
  else begin
    if (Self.AlphaBlendValue > 30) and Config.GraphicMenuFade then
      Self.AlphaBlendValue := Self.AlphaBlendValue - 30
    else begin
      Self.AlphaBlendValue := 0;
      tmrFader.Enabled     := False;
      Self.Hide;
    end;
  end;
end;

end.
