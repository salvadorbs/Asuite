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

unit Utility.Conversions;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms, Dialogs, Clipbrd,
  TypInfo;

{ HTML }
function ColorToHtml(Color:TColor): string;
function HtmlToColor(const Color: string): TColor;

{ Font }
procedure StrToFont(const s: string; AFont: TFont);

implementation

uses
  BGRABitmapTypes;

function ColorToHtml(Color:TColor): string;
var
  p: TBGRAPixel;
begin
  p.FromColor(Color);
  Result := p.ToString;
end;

function HtmlToColor(const Color: string): TColor;
var
  p: TBGRAPixel;
begin
  p.FromString(Color);
  Result := p.ToColor;
end;

procedure StrToFont(const s: string; AFont: TFont);
var
  Strs : TStringList;
begin
  if s = '' then
    Exit;

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
        AFont.Style := TFontStyles({byte}(StrToInt(Strs[3])));
      end;
    finally
      Strs.Free;
    end;
  end;
end;

end.
