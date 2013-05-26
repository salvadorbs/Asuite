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
  TGraphicMenuButton = (
      gmbASuite,
      gmbOptions,
      gmbDocuments,
      gmbMusic,
      gmbPictures,
      gmbVideos,
      gmbExplore,
      gmbAbout
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
    procedure CopyImageInVst(Source:TImage;Dest:TVirtualStringTree);
    procedure DrawRightButton(IniFile: TIniFile;Button: TcySkinButton;
                                ButtonType: TGraphicMenuButton);
    procedure DrawAndIconTextInPNGImage(IniFile: TIniFile;ButtonState: TButtonState;
                                        PNGImage: TPngImage;ButtonType: TGraphicMenuButton);
    function GetButtonCaption(IniFile: TIniFile;ButtonType: TGraphicMenuButton): string;
    function GetButtonIconPath(IniFile: TIniFile;ButtonType: TGraphicMenuButton): string;
    procedure OpenFolder(FolderPath: string);
	public
    { Public declarations }
    procedure OpenMenu;
  	procedure CloseMenu;
  end;

var
	frmGraphicMenu : TfrmGraphicMenu;
  Opening   : Boolean;
  ThemePath : String;

implementation

{$R *.dfm}

uses
  Main, Option, ulSysUtils, AppConfig, ulAppConfig, ulCommonUtils, About;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  Opening := False;
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

procedure TfrmGraphicMenu.DrawAndIconTextInPNGImage(IniFile: TIniFile;ButtonState: TButtonState;
                                                    PNGImage: TPngImage;ButtonType: TGraphicMenuButton);
var
  TopText  : Integer;
  FontText : TFont;
  Icon     : TIcon;
  IconPath, Caption : string;
begin
  Icon := TIcon.Create;
  try
    //Get and draw icon
    IconPath := GetButtonIconPath(IniFile, ButtonType);
    if FileExists(ThemePath + IconPath) then
    begin
      Icon.LoadFromFile(ThemePath + IconPath);
      PNGImage.Canvas.Draw(5, 3, Icon);
    end;
    //Get font
    case ButtonState of
      bsNormal  : FontText := StrToFont(IniFile.ReadString('rightbuttons', 'font_normal', ''));
      bsHover   : FontText := StrToFont(IniFile.ReadString('rightbuttons', 'font_hover', ''));
      bsClicked : FontText := StrToFont(IniFile.ReadString('rightbuttons', 'font_clicked', ''));
    end;
    //Get caption and draw it
    Caption  := GetButtonCaption(IniFile, ButtonType);
    if Assigned(FontText) then
      PNGImage.Canvas.Font.Assign(FontText);
    PNGImage.Canvas.Brush.Style := bsClear;
    TopText := (PNGImage.Height - Abs(PNGImage.Canvas.Font.Height)) div 2;
    PNGImage.Canvas.TextOut(35, TopText - 1, Caption);
  finally
    FontText.Free;
    Icon.Free;
  end;
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

procedure TfrmGraphicMenu.DrawRightButton(IniFile: TIniFile;Button: TcySkinButton;
                                            ButtonType: TGraphicMenuButton);
var
  PNGImage_Normal, PNGImage_Hover, PNGImage_Clicked: TPngImage;
  Image_Normal, Image_Hover, Image_Clicked: string;
begin
  PNGImage_Normal  := TPngImage.Create;
  PNGImage_Hover   := TPngImage.Create;
  PNGImage_Clicked := TPngImage.Create;
  try
    //Get images path
    Image_Normal  := ThemePath + IniFile.ReadString('rightbuttons', 'image_normal', '');
    Image_Hover   := ThemePath + IniFile.ReadString('rightbuttons', 'image_hover', '');
    Image_Clicked := ThemePath + IniFile.ReadString('rightbuttons', 'image_clicked', '');
    //Load png button states
    if FileExists(Image_Normal) then
      PNGImage_Normal.LoadFromFile(Image_Normal);
    if FileExists(Image_Hover) then
      PNGImage_Hover.LoadFromFile(Image_Hover);
    if FileExists(Image_Clicked) then
      PNGImage_Clicked.LoadFromFile(Image_Clicked);
    //Draw caption and icon in PNGImage_*
    DrawAndIconTextInPNGImage(IniFile,bsNormal,PNGImage_Normal,ButtonType);
    DrawAndIconTextInPNGImage(IniFile,bsHover,PNGImage_Hover,ButtonType);
    DrawAndIconTextInPNGImage(IniFile,bsClicked,PNGImage_Clicked,ButtonType);
    //Set Button's PicNormal, PicMouseOver and PicMouseDown
    Button.PicNormal.Assign(PNGImage_Normal);
    Button.PicMouseOver.Assign(PNGImage_Hover);
    Button.PicMouseDown.Assign(PNGImage_Clicked);
  finally
    PNGImage_Normal.Free;
    PNGImage_Hover.Free;
    PNGImage_Clicked.Free;
  end;
end;

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
  BackgroundPath, LogoPath: string;
begin
  ThemePath := SUITE_MENUTHEMES_PATH + Config.GraphicMenuTheme + '\';
  IniFile := TIniFile.Create(ThemePath + THEME_INI);
  try
    //IniFile Section General
    //Background
    BackgroundPath := ThemePath + IniFile.ReadString('general', 'image_background', '');
    if FileExists(BackgroundPath) then
      imgBackground.Picture.LoadFromFile(BackgroundPath);
    //Logo
    LogoPath := ThemePath + IniFile.ReadString('general', 'image_logo', '');
    if FileExists(LogoPath) then
      imgLogo.Picture.LoadFromFile(LogoPath);
    //IniFile Section RightButtons
    //Draw Right Buttons
    DrawRightButton(IniFile,sknbtnASuite,gmbASuite);
    DrawRightButton(IniFile,sknbtnOptions,gmbOptions);
    DrawRightButton(IniFile,sknbtnDocuments,gmbDocuments);
    DrawRightButton(IniFile,sknbtnMusic,gmbMusic);
    DrawRightButton(IniFile,sknbtnPictures,gmbPictures);
    DrawRightButton(IniFile,sknbtnVideos,gmbVideos);
    DrawRightButton(IniFile,sknbtnExplore,gmbExplore);
    DrawRightButton(IniFile,sknbtnAbout,gmbAbout);
  finally
    IniFile.Free;
  end;
  //Position
  Top  := Screen.WorkAreaRect.Bottom - Height;
  Left := Screen.WorkAreaRect.Right - Width;
end;

function TfrmGraphicMenu.GetButtonCaption(IniFile: TIniFile;ButtonType: TGraphicMenuButton): string;
begin
  case ButtonType of
    gmbASuite    : Result := msgGMASuite;
    gmbOptions   : Result := msgGMOptions;
    gmbDocuments : Result := msgGMDocuments;
    gmbMusic     : Result := msgGMMusic;
    gmbPictures  : Result := msgGMPictures;
    gmbVideos    : Result := msgGMVideos;
    gmbExplore   : Result := msgGMExplore;
    gmbAbout     : Result := msgGMAbout;
  end;
end;

function TfrmGraphicMenu.GetButtonIconPath(IniFile: TIniFile;
  ButtonType: TGraphicMenuButton): string;
begin
  case ButtonType of
    gmbASuite    : Result := IniFile.ReadString('rightbuttons', 'icon_asuite', '');
    gmbOptions   : Result := IniFile.ReadString('rightbuttons', 'icon_options', '');
    gmbDocuments : Result := IniFile.ReadString('rightbuttons', 'icon_document', '');
    gmbMusic     : Result := IniFile.ReadString('rightbuttons', 'icon_music', '');
    gmbPictures  : Result := IniFile.ReadString('rightbuttons', 'icon_pictures', '');
    gmbVideos    : Result := IniFile.ReadString('rightbuttons', 'icon_videos', '');
    gmbExplore   : Result := IniFile.ReadString('rightbuttons', 'icon_explore', '');
    gmbAbout     : Result := IniFile.ReadString('rightbuttons', 'icon_help', '');
  end;
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
  Opening := True;
  tmrFader.Enabled:= True;
  //Show frmMenu
  Show;
  SetForegroundWindow(Handle);
  if Not(IsWindowVisible(frmMain.Handle)) then
    ShowWindow(Application.Handle, SW_HIDE);
end;
procedure TfrmGraphicMenu.tmrFaderTimer(Sender: TObject);
begin
  if Opening then
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
