{
Copyright (C) 2006-2015 Matteo Salvi

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

unit GraphicMenu.ThemeEngine;

interface

uses
  Classes, Kernel.Singleton, IniFiles, cySkinButton, Vcl.Imaging.PNGImage,
  ExtCtrls, Windows, Graphics, SysUtils, DKLang, VirtualTrees, Controls,
  Forms.GraphicMenu;

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

  TThemeEngine = class(TSingleton)
  private
    FGraphicMenu: TfrmGraphicMenu;
    FSearchIcon: Integer;
    FCancelIcon: Integer;

    //Get methods
    function GetButtonCaption(IniFile: TIniFile; ButtonType: TGraphicMenuElement): string;
    function GetButtonIconPath(IniFile: TIniFile; ButtonType: TGraphicMenuElement): string;
    function GetIniFileSection(ElementType: TGraphicMenuElement): string;

    //Draw methods
    procedure DrawEmptyButton(PNGImage: TPngImage; Button: TcySkinButton; imgBackground: TImage);
    procedure DrawIconInPNGImage(IniFile: TIniFile;PNGImage: TPngImage;
                                 ButtonType: TGraphicMenuElement);
    procedure DrawTextInPNGImage(IniFile: TIniFile;ButtonState: TButtonState;
                                 PNGImage: TPngImage;ButtonType: TGraphicMenuElement;
                                 SpaceForIcon: Boolean = True);
    procedure DrawIconAndTextInPNGImage(IniFile: TIniFile; ButtonState: TButtonState;
                                        PNGImage: TPngImage; ButtonType: TGraphicMenuElement);
    procedure DrawButton(IniFile: TIniFile;Button: TcySkinButton;
                         ButtonType: TGraphicMenuElement);
    procedure DrawHardDiskSpace(IniFile: TIniFile; DriveBackGround, DriveSpace: TImage);

    //Misc
    function IsRightButton(ButtonType: TGraphicMenuElement): Boolean;
    procedure CopyImageInVst(Source:TImage; Tree: TVirtualStringTree);
    procedure CopySelectedRectInBitmap(Source:TImage;Comp: TControl;bmp: TBitmap);
  public
    procedure Initialize; override;

    procedure LoadTheme;

    procedure SetupThemeEngine(AGraphicMenu: TfrmGraphicMenu);

    property SearchIcon: Integer read FSearchIcon write FSearchIcon;
    property CancelIcon: Integer read FCancelIcon write FCancelIcon;
  end;

implementation

uses
  Kernel.Consts, AppConfig.Main, Utility.Conversions, Utility.Misc,
  GraphicMenu.ThemeEngine.Consts;

{ TThemeEngineMethods }

procedure TThemeEngine.CopyImageInVst(Source: TImage;
  Tree: TVirtualStringTree);
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

procedure TThemeEngine.CopySelectedRectInBitmap(Source: TImage;
  Comp: TControl; bmp: TBitmap);
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

procedure TThemeEngine.DrawButton(IniFile: TIniFile;
  Button: TcySkinButton; ButtonType: TGraphicMenuElement);
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
      DrawEmptyButton(PNGImage_Normal, Button, FGraphicMenu.imgBackground);
    //Hover state
    if FileExists(Image_Hover) then
      PNGImage_Hover.LoadFromFile(Image_Hover)
    else
      DrawEmptyButton(PNGImage_Hover, Button, FGraphicMenu.imgBackground);
    //Clicked state
    if FileExists(Image_Clicked) then
      PNGImage_Clicked.LoadFromFile(Image_Clicked)
    else
      DrawEmptyButton(PNGImage_Clicked, Button, FGraphicMenu.imgBackground);
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

procedure TThemeEngine.DrawEmptyButton(PNGImage: TPngImage;
  Button: TcySkinButton; imgBackground: TImage);
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

procedure TThemeEngine.DrawHardDiskSpace(IniFile: TIniFile;
  DriveBackGround, DriveSpace: TImage);
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

procedure TThemeEngine.DrawIconAndTextInPNGImage(IniFile: TIniFile;
  ButtonState: TButtonState; PNGImage: TPngImage;
  ButtonType: TGraphicMenuElement);
begin
  DrawIconInPNGImage(IniFile, PNGImage, ButtonType);
  DrawTextInPNGImage(IniFile, ButtonState, PNGImage, ButtonType);
end;

procedure TThemeEngine.DrawIconInPNGImage(IniFile: TIniFile;
  PNGImage: TPngImage; ButtonType: TGraphicMenuElement);
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

procedure TThemeEngine.DrawTextInPNGImage(IniFile: TIniFile;
  ButtonState: TButtonState; PNGImage: TPngImage;
  ButtonType: TGraphicMenuElement; SpaceForIcon: Boolean);
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

function TThemeEngine.GetButtonCaption(IniFile: TIniFile;
  ButtonType: TGraphicMenuElement): string;
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

function TThemeEngine.GetButtonIconPath(IniFile: TIniFile;
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

function TThemeEngine.GetIniFileSection(
  ElementType: TGraphicMenuElement): string;
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

procedure TThemeEngine.LoadTheme;
var
  BackgroundPath: string;
  sTempPath: string;
  IniFile: TIniFile;
  strFont: string;
begin
  Assert(Assigned(FGraphicMenu), 'FGraphicMenu is not assigned!');

  //Load theme
  if FileExists(Config.Paths.SuitePathCurrentTheme + THEME_INI) then
  begin
    IniFile := TIniFile.Create(Config.Paths.SuitePathCurrentTheme + THEME_INI);
    try
      //IniFile Section General
      //Background
      BackgroundPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEBACKGROUND, '');
      if FileExists(BackgroundPath) then
        FGraphicMenu.imgBackground.Picture.LoadFromFile(BackgroundPath);
      //User frame
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEUSERFRAME, '');
      if FileExists(sTempPath) then
        FGraphicMenu.imgUserFrame.Picture.LoadFromFile(sTempPath);
      //Logo
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGELOGO, '');
      if FileExists(sTempPath) then
        FGraphicMenu.imgLogo.Picture.LoadFromFile(sTempPath);
      //Separator
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGESEPARATOR, '');
      FGraphicMenu.imgDivider1.Picture.LoadFromFile(sTempPath);
      FGraphicMenu.imgDivider2.Picture.LoadFromFile(sTempPath);
      //Tabs
      DrawButton(IniFile, FGraphicMenu.sknbtnList, gmbList);
      DrawButton(IniFile, FGraphicMenu.sknbtnRecents, gmbMRU);
      DrawButton(IniFile, FGraphicMenu.sknbtnMFU, gmbMFU);
      //Right Buttons
      DrawButton(IniFile, FGraphicMenu.sknbtnASuite, gmbASuite);
      DrawButton(IniFile, FGraphicMenu.sknbtnOptions, gmbOptions);
      DrawButton(IniFile, FGraphicMenu.sknbtnDocuments, gmbDocuments);
      DrawButton(IniFile, FGraphicMenu.sknbtnMusic, gmbMusic);
      DrawButton(IniFile, FGraphicMenu.sknbtnPictures, gmbPictures);
      DrawButton(IniFile, FGraphicMenu.sknbtnVideos, gmbVideos);
      DrawButton(IniFile, FGraphicMenu.sknbtnExplore, gmbExplore);
      DrawButton(IniFile, FGraphicMenu.sknbtnAbout, gmbAbout);
      //Eject and Close Buttons
      DrawButton(IniFile, FGraphicMenu.sknbtnEject, gmbEject);
      DrawButton(IniFile, FGraphicMenu.sknbtnExit, gmbExit);
      //Search
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_ICONSEARCH, '');
//      if FileExists(sTempPath) then
//        FSearchIcon := ImagesDM.GetSimpleIconIndex(sTempPath, True);
      sTempPath := Config.Paths.SuitePathCurrentTheme + IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_ICONCANCEL, '');
//      if FileExists(sTempPath) then
//        FCancelIcon := ImagesDM.GetSimpleIconIndex(sTempPath, True);
      FGraphicMenu.edtSearch.RightButton.ImageIndex := FSearchIcon;
      //Hard Disk
      DrawHardDiskSpace(IniFile, FGraphicMenu.imgDriveBackground, FGraphicMenu.imgDriveSpace);
      FGraphicMenu.lblDriveName.Caption := format(DKLangConstW('msgGMDriveName'), [UpperCase(Config.Paths.SuiteDrive)]);
      //Fonts
      strFont := IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_FONT, '');
      StrToFont(strFont, FGraphicMenu.lblDriveName.Font);
      StrToFont(strFont, FGraphicMenu.lblDriveSpace.Font);
      //VirtualTrees
      StrToFont(IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_FONTTREE, ''), FGraphicMenu.vstList.Font);
      //Workaround for vst trasparent
      CopyImageInVst(FGraphicMenu.imgBackground, FGraphicMenu.vstList);
    finally
      IniFile.Free;
    end;
  end
  else
    ShowMessageFmtEx(DKLangConstW('msgErrNoThemeIni'), [Config.Paths.SuitePathCurrentTheme + THEME_INI], True);
end;

procedure TThemeEngine.SetupThemeEngine(AGraphicMenu: TfrmGraphicMenu);
begin
  FGraphicMenu := AGraphicMenu;
end;

procedure TThemeEngine.Initialize;
begin
  inherited;
  FSearchIcon := -1;
  FCancelIcon := -1;
end;

function TThemeEngine.IsRightButton(
  ButtonType: TGraphicMenuElement): Boolean;
begin
  Result := False;
  if ButtonType in [gmbASuite,gmbOptions,gmbDocuments,gmbMusic,gmbPictures,
                    gmbVideos,gmbExplore,gmbAbout] then
    Result := True;
end;

end.
