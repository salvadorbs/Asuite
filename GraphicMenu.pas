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
	TfrmGraphicMenu = class(TForm)
  	imgDriveSpace: TImage;
	  imgDivider2: TImage;
  	lblDriveName: TLabel;
  	lblDriveSpace: TLabel;
   	tmrFader: TTimer;
    imgDragSpaceHidden: TImage;
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
    sknbtnMusic: TcySkinButton;
    sknbtnPictures: TcySkinButton;
    sknbtnVideo: TcySkinButton;
    sknbtnExplore: TcySkinButton;
    sknbtnAbout: TcySkinButton;
    btnSearch: TButtonedEdit;
    sknbtnList: TcySkinButton;
    sknbtnRecents: TcySkinButton;
    sknbtnMFU: TcySkinButton;
    sknbtnEject: TcySkinButton;
    sknbtnExit: TcySkinButton;
    imgBackground: TImage;
    procedure FormCreate(Sender: TObject);
    procedure tmrFaderTimer(Sender: TObject);
	private    
    { Private declarations }
    procedure CopyImageInVst(Source:TImage;Dest:TVirtualStringTree);
	public
    { Public declarations }
    procedure OpenMenu;
  	procedure CloseMenu;
  end;

var
	frmGraphicMenu : TfrmGraphicMenu;
  Opening : Boolean;

implementation

{$R *.dfm}

uses
  Main, Option, ulSysUtils, AppConfig, ulAppConfig;

procedure TfrmGraphicMenu.CloseMenu;
begin
  //Fade in out
  Opening := False;
  tmrFader.Enabled:= True;
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

procedure TfrmGraphicMenu.FormCreate(Sender: TObject);
begin
  //Position
  Top  := Screen.WorkAreaRect.Bottom - Height;
  Left := Screen.WorkAreaRect.Right - Width;
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
