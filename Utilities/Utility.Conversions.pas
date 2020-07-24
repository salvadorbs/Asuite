{
Copyright (C) 2006-2020 Matteo Salvi

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

unit Utility.Conversions;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms, Dialogs, Clipbrd,
  StdCtrls, TypInfo;

{ HTML }
function RGBToHtml(iRGB: Cardinal): string;
function ColorToHtml(Color:TColor): string;
function HtmlToColor(const Color: string): TColor;

{ Font }
procedure StrToFont(const s: string; AFont: TFont);
function FontToStr(Font: TFont): string;

{ String }
function  BoolToStr(Bool: Boolean): String;
function  StrToBool(Str: string): boolean;
procedure StrToStrings(Str, Sep: string; const List: TStrings);

implementation

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

function HtmlToColor(const Color: string): TColor;
begin
  Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4, 2) + Copy(Color, 2, 2));
end;

procedure StrToFont(const s: string; AFont: TFont);
var
  Strs : TStringList;
begin
  if Assigned(AFont) then
  begin
    Strs  := TStringList.Create;
    try
      Strs.Text := StringReplace(s, '|', #10, [rfReplaceAll]);
      if Strs.Count = 4 then
      begin
        AFont.Name  := Strs[0];
        AFont.Size  := StrToInt(Strs[1]);
        AFont.Color := HtmlToColor(Strs[2]);
        { TODO : Check  this code }
        AFont.Style := TFontStyles({byte}(StrToInt(Strs[3])));
      end;
    finally
      Strs.Free;
    end;
  end;
end;

function FontToStr(Font: TFont): string;
var
  sColor, sStyle : string;
begin
  sColor := ColorToHtml(Font.Color);  
  sStyle := GetEnumName(TypeInfo(TFontStyles), integer(Font.Style));
  result := Font.Name + '|' + IntToStr(Font.Size) + '|' + sColor + '|' + sStyle;
end;

 function BoolToStr(Bool: Boolean):String;
begin
  if Bool = true then
    result := '1'
  else
    result := '0';
end;

function StrToBool(Str: string): boolean;
begin
  if (Str = '1') then
    result := True
  else
    result := false;
end;

procedure StrToStrings(Str, Sep: string; const List: TStrings);
var
  I        : Integer;
  PieceStr : string;
begin
  Assert(Assigned(List));
  List.BeginUpdate;
  try
    List.Clear;
    I := Pos(Sep, Str);
    while I > 0 do
    begin
      //Copy first part of Str and add it in List
      PieceStr := Copy(Str, 1, I - 1);
      if (PieceStr <> '') then
        List.Add(PieceStr);
      //Delete first part in Str and begin again
      Delete(Str, 1, I);
      I := Pos(Sep, Str);
    end;
    //Last piece of string
    if Str <> '' then
      List.Add(Str);
  finally
    List.EndUpdate;
  end;
end;

end.
