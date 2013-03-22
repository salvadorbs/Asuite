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

unit ulCommonUtils;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Dialogs, ComCtrls, Clipbrd,
  AppConfig, DOM, StdCtrls;

{ Converters }
function RGBToHtml(iRGB: Cardinal): string;
function ColorToHtml(Color:TColor): string;
function HtmlToColor(Color: string): TColor;
function StrToFont(const s: string; Font: TFont): boolean;
function FontToStr(Font: TFont): string;

{ Forms }
function  CreateDialogProgressBar(DialogMsg: String;NumberFolders: Integer): TForm;
function  IsFormOpen(const FormName : string): Boolean;
procedure SetFormPosition(Form: TForm;ListFormLeft, ListFormTop: Integer);

{ XML }
function GetStrPropertyXML(Node : TDOMNode;Name: String;Default: String): String;
function GetIntPropertyXML(Node : TDOMNode;Name: String;Default: Integer): Integer;
function GetBoolPropertyXML(Node : TDOMNode;Name: String;Default: Boolean): Boolean;

{ Misc }
procedure CheckPropertyName(Edit: TEdit);
function  GetDateTime: String;
function  GetFirstFreeIndex(ArrayString: Array of WideString): Integer;
function  IfThen(AValue: Boolean; ATrue, AFalse: String): String;
function  IsFormatInClipBoard(format: Word): Boolean;

//type
//  TScanFolderSettings = record
//    LastFolderPath   : String;
//    SubFolders       : Boolean;
//    FileTypes        : TStringList;
//    ExcludeFiles     : TStringList;
//    RetrieveInfo     : Boolean;
//  end;

const
  MAX_PROFILE_PATH = 255;

implementation

uses
  ulStringUtils;

function RGBToHtml(iRGB: Cardinal): string;
begin
  Result:=Format('#%.2x%.2x%.2x',
                 [Byte(iRGB),          //GetRValue(vRGB)
                  Byte(iRGB shr 8),    //GetGValue(vRGB)
                  Byte(iRGB shr 16)]); //GetBValue(vRGB)
end;

function ColorToHtml(Color:TColor): string;
begin
  Result := RGBToHtml(ColorToRGB(Color));
end;

function HtmlToColor(Color: string): TColor;
begin
  Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4, 2) + Copy(Color, 2, 2));
end;

function StrToFont(const s: string; Font: TFont): boolean;
var
  afont : TFont;
  Strs : TStringList;
begin
  afont := TFont.Create;
  try
    afont.Assign(Font);
    Strs := TStringList.Create;
    try
      Strs.Text := StringReplace(s, '|', #10, [rfReplaceAll]);
      result    := Strs.Count = 4;
      if result then
      begin
        afont.Name  := Strs[0];
        afont.Size  := StrToInt(Strs[1]);
        afont.Color := HtmlToColor(Strs[2]);
        afont.Style := TFontStyles(Integer(StrToInt(Strs[3])));
      end;
      Font.Assign(afont);
    finally
      Strs.Free;
    end;
  finally
    afont.Free;
  end;
end;

function FontToStr(Font: TFont): string;
var
  sColor, sStyle : string;
begin
  sColor := ColorToHtml(Font.Color);
  sStyle := IntToStr(Integer(Font.Style));
  result := Font.Name +'|'+ IntToStr(Font.Size) + '|' + sColor + '|' + sStyle;
end;

function CreateDialogProgressBar(DialogMsg: String;NumberFolders: Integer): TForm;
var
  ProgressBar : TProgressBar;
begin
  //Create Dialog and ProgressBar (in Dialog)
  Result      := CreateMessageDialog(DialogMsg, mtInformation, []) ;
  ProgressBar := TProgressBar.Create(Result) ;
  Result.Caption := DialogMsg;
  Result.Height  := 100;
  //Set some progressbar's property
  with ProgressBar do
  begin
    Name  := 'Progress';
    Parent := Result;
    Max   := NumberFolders;
    Step  := 1;
    Top   := 50;
    Left  := 8;
    Width := Result.ClientWidth - 16;
  end;
  Result.Show;
end;

function IsFormOpen(const FormName : string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for i := Screen.FormCount - 1 DownTo 0 do
    if (Screen.Forms[i].Name = FormName) then
    begin
      Result := True;
      Break;
    end;
end;

procedure SetFormPosition(Form: TForm; ListFormLeft, ListFormTop: Integer);
begin
  if (ListFormTop <> -1) and (ListFormLeft <> -1) then
  begin
    if (ListFormTop + frmMainHeight) <= Screen.Height then
      Form.Top  := ListFormTop
    else
      Form.Top  := Screen.Height - Form.Height - 30;
    if (ListFormLeft + frmMainWidth) <= Screen.Width then
      Form.Left := ListFormLeft
    else
      Form.Left := Screen.Width - Form.Width;
    Form.Position := poDesigned;
  end
  else
    Form.Position := poDesktopCenter;
end;

function GetStrPropertyXML(Node : TDOMNode;Name: String;Default: String): String;
var
  PropertyNode: TDOMNode;
begin
  Result := Default;
  PropertyNode := Node.FindNode(Name);
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.TextContent <> '' then
      Result := PropertyNode.TextContent;
end;

function GetIntPropertyXML(Node : TDOMNode;Name: String;Default: Integer): Integer;
var
  PropertyNode: TDOMNode;
begin
  Result := Default;
  PropertyNode := Node.FindNode(Name);
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.TextContent <> '' then
      Result := StrToInt(PropertyNode.TextContent);
end;

function GetBoolPropertyXML(Node : TDOMNode;Name: String;Default: Boolean): Boolean;
var
  PropertyNode: TDOMNode;
begin
  Result := Default;
  PropertyNode := Node.FindNode(Name);
  //Check if PropertyNode exists
  if Assigned(PropertyNode) then
    if PropertyNode.TextContent <> '' then
      Result := ulStringUtils.StrToBool(Node.TextContent);
end;

procedure CheckPropertyName(Edit: TEdit);
begin
  // Check if inserted name is empty, then
  if (Trim(Edit.Text) = '') then
  begin
    ShowMessage(msgErrEmptyName);
    Edit.Color := clYellow;
  end;
end;

function GetDateTime: String;
begin
  DateTimeToString(Result, 'yyyy-mm-dd-hh-mm-ss',now);
end;

function GetFirstFreeIndex(ArrayString: Array of WideString): Integer;
begin
  Result := 0;
  while (ArrayString[Result] <> '') do
    Inc(Result);
end;

function IfThen(AValue: Boolean; ATrue, AFalse: String): String;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IsFormatInClipBoard(format: Word): Boolean;
var
  buf : array [0..60] of Char;
  n   : Integer;
  fmt : Word;
begin
  //Get clipboard format
  Result := False;
  for n := 0 to Clipboard.FormatCount - 1 do
  begin
    fmt := Clipboard.Formats[n];
    GetClipboardFormatName(fmt, buf, Pred(SizeOf(buf)));
    if fmt = format then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

end.
