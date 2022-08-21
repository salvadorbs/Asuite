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

unit GraphicMenu.ThemeEngine;

{$MODE DelphiUnicode}

interface

uses
  Classes, IniFiles, ExtCtrls, LCLIntf, LCLType,
  Graphics, SysUtils, VirtualTrees, Controls, BGRABitmap, Forms,
  BCImageButton, LazFileUtils;

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

  { TThemeEngine }

  TThemeEngine = class
  private
    FGraphicMenu: TForm;
    FSearchIcon: Integer;
    FCancelIcon: Integer;

    //Get methods
    function GetButtonCaption(IniFile: TIniFile; ButtonType: TGraphicMenuElement): string;
    function GetButtonIconPath(IniFile: TIniFile; ButtonType: TGraphicMenuElement): AnsiString;
    function GetIniFileSection(ElementType: TGraphicMenuElement): string;
    function GetPathFromIni(IniFile: TIniFile; Section, Key, DefaultValue: String; InitialPath: String = ''): AnsiString;

    //Draw methods
    procedure DrawEmptyButton(PNGImage: TBGRABitmap; Button: TBCCustomImageButton;
      Background: TBitmap; IsDPIChanged: Boolean);
    function DrawIconInPNGImage(IniFile: TIniFile; PNGImage: TBGRABitmap;
                                 ButtonType: TGraphicMenuElement): Integer;
    procedure DrawTextInPNGImage(IniFile: TIniFile; PNGImage: TBGRABitmap; ButtonType: TGraphicMenuElement; IconHeight: Integer);
    procedure DrawButton(IniFile: TIniFile;Button: TBCCustomImageButton;
                         ButtonType: TGraphicMenuElement);
    procedure DrawHardDiskSpace(IniFile: TIniFile; DriveBackGround, DriveSpace: TImage);

    //Misc
    function IsDPIChanged: Boolean;
    function IsRightButton(ButtonType: TGraphicMenuElement): Boolean;
    function IsTabElement(ButtonType: TGraphicMenuElement): Boolean;
    procedure CopyImageInVst(Source: Graphics.TBitmap; Tree: TVirtualStringTree;
      IsDPIChanged: Boolean);
    procedure CopySelectedRectInBitmap(Source: Graphics.TBitmap; Comp: TControl;
      bmp: Graphics.TBitmap; IsDPIChanged: Boolean);
  public
    constructor Create(AGraphicMenu: TForm);

    procedure LoadTheme;

    property SearchIcon: Integer read FSearchIcon write FSearchIcon;
    property CancelIcon: Integer read FCancelIcon write FCancelIcon;
  end;

implementation

uses
  Kernel.Consts, Utility.Conversions, Kernel.ResourceStrings, BGRABitmapTypes, Types,
  GraphicMenu.ThemeEngine.Consts, Kernel.Logger, Utility.Misc, Kernel.Instance,
  Kernel.Manager, mormot.core.log, Forms.GraphicMenu;

{ TThemeEngineMethods }

function TThemeEngine.GetPathFromIni(IniFile: TIniFile; Section, Key, DefaultValue: String; InitialPath: String = ''): AnsiString;
begin
  if InitialPath = '' then
    Result := ASuiteInstance.Paths.SuitePathCurrentTheme + IniFile.ReadString(Section, Key, DefaultValue)
  else
    Result := InitialPath + IniFile.ReadString(Section, Key, DefaultValue);

  ForcePathDelims(Result);
end;

procedure TThemeEngine.CopyImageInVst(Source: Graphics.TBitmap;
  Tree: TVirtualStringTree; IsDPIChanged: Boolean);
var
  bmpTempImage : Graphics.TBitmap;
begin
  bmpTempImage := Graphics.TBitmap.Create;
  try
    CopySelectedRectInBitmap(Source, Tree, bmpTempImage, IsDPIChanged);
    Tree.Background.Bitmap := bmpTempImage;
  finally
    bmpTempImage.Free;
  end;
end;

procedure TThemeEngine.CopySelectedRectInBitmap(Source: Graphics.TBitmap;
  Comp: TControl; bmp: Graphics.TBitmap; IsDPIChanged: Boolean);
var
  RectSource, RectDest : TRect;
  iLeft, iTop, iWidth, iHeight: Integer;
begin
  if Assigned(bmp) then
  begin
    //Get original (dpi 96) Comp size
    if IsDPIChanged then
    begin
      iLeft   := Comp.ScaleFormTo96(Comp.Left);
      iTop    := Comp.ScaleFormTo96(Comp.Top);
      iWidth  := Comp.ScaleFormTo96(Comp.Width);
      iHeight := Comp.ScaleFormTo96(Comp.Height);
    end
    else begin
      iLeft   := Comp.Left;
      iTop    := Comp.Top;
      iWidth  := Comp.Width;
      iHeight := Comp.Height;
    end;

    bmp.Height := iHeight;
    bmp.Width  := iWidth;

    //Set RectSource size
    RectSource.Left     := iLeft;
    RectSource.Top      := iTop;
    RectSource.Right    := iLeft + iWidth;
    RectSource.Bottom   := iTop + iHeight;

    //Set RectDest size
    RectDest.Left       := 0;
    RectDest.Top        := 0;
    RectDest.Right      := iWidth;
    RectDest.Bottom     := iHeight;

    bmp.Canvas.CopyRect(RectDest, Source.Canvas, RectSource);
  end;
end;

constructor TThemeEngine.Create(AGraphicMenu: TForm);
begin
  FSearchIcon := -1;
  FCancelIcon := -1;           
  FGraphicMenu := AGraphicMenu;
end;

procedure TThemeEngine.DrawButton(IniFile: TIniFile;
  Button: TBCCustomImageButton; ButtonType: TGraphicMenuElement);
var
  PNGButton: TBGRABitmap;
  strButtonFile, IniFile_Section: string;
  iIconHeight: Integer;
begin
  iIconHeight := -1;

  PNGButton := TBGRABitmap.Create;
  try
    IniFile_Section := GetIniFileSection(ButtonType);

    //Get images path
    strButtonFile := GetPathFromIni(IniFile, IniFile_Section, INIFILE_KEY_IMAGEBUTTON, '');

    //Load png button states
    if FileExists(strButtonFile) then
      PNGButton.LoadFromFile(strButtonFile)
    else
      DrawEmptyButton(PNGButton, Button, TfrmGraphicMenu(FGraphicMenu).imgBackground.Picture.Bitmap, IsDPIChanged);

    //Draw caption and icon in PNGImage_*, if button is a RightButton 
    if IsRightButton(ButtonType) then
      iIconHeight := DrawIconInPNGImage(IniFile, PNGButton, ButtonType);

    DrawTextInPNGImage(IniFile, PNGButton, ButtonType, iIconHeight);
  finally
    if Assigned(Button.BitmapOptions.Bitmap) then
      Button.BitmapOptions.Bitmap.Free;
    Button.BitmapOptions.Bitmap := PNGButton;
  end;
end;

procedure TThemeEngine.DrawEmptyButton(PNGImage: TBGRABitmap;
  Button: TBCCustomImageButton; Background: TBitmap; IsDPIChanged: Boolean);
var
  bmp: Graphics.TBitmap;
begin
  bmp := Graphics.TBitmap.Create;
  try
    CopySelectedRectInBitmap(Background, Button, bmp, IsDPIChanged);
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
  HDPath := GetPathFromIni(IniFile, INIFILE_SECTION_HARDDISK, INIFILE_KEY_IMAGEBACKGROUND, '');
  if FileExists(HDPath) then
    DriveBackGround.Picture.LoadFromFile(HDPath);

  HDSpacePath := GetPathFromIni(IniFile, INIFILE_SECTION_HARDDISK, INIFILE_KEY_IMAGESPACE, '');
  if FileExists(HDSpacePath) then
    DriveSpace.Picture.LoadFromFile(HDSpacePath);
end;

function TThemeEngine.IsDPIChanged: Boolean;
begin
  Result := TfrmGraphicMenu(FGraphicMenu).DesignTimePPI <>
            TfrmGraphicMenu(FGraphicMenu).PixelsPerInch;
end;

function TThemeEngine.DrawIconInPNGImage(IniFile: TIniFile;
  PNGImage: TBGRABitmap; ButtonType: TGraphicMenuElement): Integer;
var
  Icon : TBGRABitmap;
  IconPath: string;
  I, buttonHeight, iSpace: Integer;
begin
  Result := -1;
  if Not Assigned(PNGImage) then
    Exit;

  Icon := TBGRABitmap.Create;
  try
    //Get and draw icon
    IconPath := GetButtonIconPath(IniFile, ButtonType);
    if FileExists(ASuiteInstance.Paths.SuitePathCurrentTheme + IconPath) then
    begin
      Icon.LoadFromFile(ASuiteInstance.Paths.SuitePathCurrentTheme + IconPath);
      Result := Icon.Height;
      buttonHeight := (PNGImage.Height div 4);
      iSpace := (buttonHeight - Icon.Height) div 2;

      for I := 0 to 3 do
        PNGImage.BlendImage(5, iSpace + (buttonHeight * I), Icon, boTransparent);
    end;
  finally
    Icon.Free;
  end;
end;

procedure TThemeEngine.DrawTextInPNGImage(IniFile: TIniFile;
  PNGImage: TBGRABitmap; ButtonType: TGraphicMenuElement; IconHeight: Integer);
var
  ButtonHeight, I : Integer;
  FontNormal, FontHover, FontClicked : TFont;
  Caption, IniFile_Section : string;
  TextColor: TColor;

  procedure AssignFont(APNGImage: TBGRABitmap; AFont: TFont);
  begin
    APNGImage.FontAntialias := True;

    APNGImage.FontName := AFont.Name;
    APNGImage.FontStyle := AFont.Style;
    APNGImage.FontOrientation := AFont.Orientation;

    case AFont.Quality of
      fqNonAntialiased: APNGImage.FontQuality := fqSystem;
      fqAntialiased: APNGImage.FontQuality := fqFineAntialiasing;
      fqProof: APNGImage.FontQuality := fqFineClearTypeRGB;
      fqDefault, fqDraft, fqCleartype, fqCleartypeNatural: APNGImage.FontQuality :=
          fqSystemClearType;
    end;

    APNGImage.FontHeight := -AFont.Height;
    TextColor := AFont.Color;
  end;

begin
  if Not Assigned(PNGImage) then
    Exit;

  FontNormal := TFont.Create;
  FontHover := TFont.Create;
  FontClicked := TFont.Create;
  try
    IniFile_Section := GetIniFileSection(ButtonType);
    //Get font
    StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTNORMAL, 'default|0|#000000|0'), FontNormal);
    StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTHOVER, 'default|0|#000000|0'), FontHover);
    StrToFont(IniFile.ReadString(IniFile_Section, INIFILE_KEY_FONTCLICKED, 'default|0|#000000|0'), FontClicked);
    //Get caption and draw it
    Caption := GetButtonCaption(IniFile, ButtonType);
    if Caption <> '' then
    begin
      ButtonHeight := (PNGImage.Height div 4);
      for I := 0 to 3 do
      begin
        case TButtonState(I) of
          bsNormal: AssignFont(PNGImage, FontNormal);
          bsHover: AssignFont(PNGImage, FontHover);
          bsClicked: AssignFont(PNGImage, FontClicked);
          bsDisabled: AssignFont(PNGImage, FontNormal);
        end;

        if IconHeight <> -1 then
          PNGImage.TextRect(Rect(IconHeight + 10, (ButtonHeight * I), PNGImage.Width, (ButtonHeight * (I + 1))), Caption, taLeftJustify, tlCenter, TextColor)
        else
          PNGImage.TextRect(Rect(0, (ButtonHeight * I), PNGImage.Width, (ButtonHeight * (I + 1))), Caption, taCenter, tlCenter, TextColor)
      end;
    end;
  finally
    FontNormal.Free;
    FontHover.Free;
    FontClicked.Free;
  end;
end;

function TThemeEngine.GetButtonCaption(IniFile: TIniFile;
  ButtonType: TGraphicMenuElement): string;
begin
  Result := '';

  case ButtonType of
    //Right buttons
    gmbASuite    : Result := Format(msgGMShow, [APP_NAME]);
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

function TThemeEngine.GetButtonIconPath(IniFile: TIniFile;
  ButtonType: TGraphicMenuElement): AnsiString;
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

  ForcePathDelims(Result);
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
  {%H-}log: ISynLog;
begin
  Assert(Assigned(FGraphicMenu), 'FGraphicMenu is not assigned!');
  Assert((FGraphicMenu is TfrmGraphicMenu), 'FGraphicMenu is not a TfrmGraphicMenu!');
  log := TASuiteLogger.Enter('TThemeEngine.LoadTheme', Self);

  //Load theme
  if FileExists(ASuiteInstance.Paths.SuitePathCurrentTheme + THEME_INI) then
  begin
    TASuiteLogger.Info('Found theme.ini - Loading it', []);
    IniFile := TIniFile.Create(ASuiteInstance.Paths.SuitePathCurrentTheme + THEME_INI);
    try
      //IniFile Section General
      //Background
      BackgroundPath := GetPathFromIni(IniFile, INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEBACKGROUND, '');
      if FileExists(BackgroundPath) then
        TfrmGraphicMenu(FGraphicMenu).imgBackground.Picture.LoadFromFile(BackgroundPath);

      //User frame
      sTempPath := GetPathFromIni(IniFile, INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGEUSERFRAME, '');
      if FileExists(sTempPath) then
        TfrmGraphicMenu(FGraphicMenu).imgUserFrame.Picture.LoadFromFile(sTempPath);

      //Logo
      sTempPath := GetPathFromIni(IniFile, INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGELOGO, '');
      if FileExists(sTempPath) then
        TfrmGraphicMenu(FGraphicMenu).imgLogo.Picture.LoadFromFile(sTempPath);

      //Separator
      sTempPath := GetPathFromIni(IniFile, INIFILE_SECTION_GENERAL, INIFILE_KEY_IMAGESEPARATOR, '');
      TfrmGraphicMenu(FGraphicMenu).imgDivider1.Picture.LoadFromFile(sTempPath);
      TfrmGraphicMenu(FGraphicMenu).imgDivider2.Picture.LoadFromFile(sTempPath);

      //Tabs
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnList, gmbList);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnRecents, gmbMRU);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnMFU, gmbMFU);

      //Right Buttons
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnASuite, gmbASuite);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnOptions, gmbOptions);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnDocuments, gmbDocuments);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnMusic, gmbMusic);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnPictures, gmbPictures);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnVideos, gmbVideos);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnExplore, gmbExplore);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnAbout, gmbAbout);

      //Eject and Close Buttons
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnEject, gmbEject);
      DrawButton(IniFile, TfrmGraphicMenu(FGraphicMenu).sknbtnExit, gmbExit);

      //Search
      sTempPath := GetPathFromIni(IniFile, INIFILE_SECTION_SEARCH, INIFILE_KEY_ICONSEARCH, '');
      if FileExists(sTempPath) then
        FSearchIcon := ASuiteManager.IconsManager.GetPathIconIndex(sTempPath);

      sTempPath := GetPathFromIni(IniFile, INIFILE_SECTION_SEARCH, INIFILE_KEY_ICONCANCEL, '');
      if FileExists(sTempPath) then
        FCancelIcon := ASuiteManager.IconsManager.GetPathIconIndex(sTempPath);

      TfrmGraphicMenu(FGraphicMenu).edtSearch.RightButton.ImageIndex := FSearchIcon;
      StrToFont(IniFile.ReadString(INIFILE_SECTION_SEARCH, INIFILE_KEY_FONT, ''), TfrmGraphicMenu(FGraphicMenu).edtSearch.Font);

      //Hard Disk
      DrawHardDiskSpace(IniFile, TfrmGraphicMenu(FGraphicMenu).imgDriveBackground, TfrmGraphicMenu(FGraphicMenu).imgDriveSpace);
      TfrmGraphicMenu(FGraphicMenu).lblDriveName.Caption := format(msgGMDriveName, [UpperCase(ASuiteInstance.Paths.SuiteDrive)]);

      //Fonts
      strFont := IniFile.ReadString(INIFILE_SECTION_HARDDISK, INIFILE_KEY_FONT, '');
      StrToFont(strFont, TfrmGraphicMenu(FGraphicMenu).lblDriveName.Font);
      StrToFont(strFont, TfrmGraphicMenu(FGraphicMenu).lblDriveSpace.Font);

      //VirtualTrees
      StrToFont(IniFile.ReadString(INIFILE_SECTION_GENERAL, INIFILE_KEY_FONTTREE, ''), TfrmGraphicMenu(FGraphicMenu).vstList.Font);

      //Workaround for vst trasparent
      CopyImageInVst(TfrmGraphicMenu(FGraphicMenu).imgBackground.Picture.Bitmap, TfrmGraphicMenu(FGraphicMenu).vstList, IsDPIChanged);
    finally
      IniFile.Free;
    end;
  end
  else
    ShowMessageFmtEx(msgErrNoThemeIni, [ASuiteInstance.Paths.SuitePathCurrentTheme + THEME_INI], True);
end;

function TThemeEngine.IsRightButton(
  ButtonType: TGraphicMenuElement): Boolean;
begin
  Result := ButtonType in [gmbASuite,gmbOptions,gmbDocuments,gmbMusic,gmbPictures,
                    gmbVideos,gmbExplore,gmbAbout];
end;

function TThemeEngine.IsTabElement(ButtonType: TGraphicMenuElement): Boolean;
begin
  Result := ButtonType in [gmbList, gmbMRU, gmbMFU];
end;

end.
