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

unit ulStringUtils;

{$MODE Delphi}

interface

uses Classes, StrUtils, SysUtils;

function StartIndex(SubStr, Str: String; Offset: Cardinal = 1): Cardinal;
function EndIndex(SubStr, Str: String; Offset: Cardinal = 1): Cardinal;

function GetURLFromFile(const Filename: string; var URL: string): boolean;
function GetURLFromStream(Stream: TStream; var URL: string): boolean;

function IndentLine(Val: WideString; Ch: Char; Len: Integer): WideString;

{ Conversions }
function  BoolToStr(Bool: Boolean):String;
function  StrToBool(Str: string): boolean;
procedure StrToStrings(Str, Sep: string; const List: TStrings);

implementation

function StartIndex(SubStr, Str: String; Offset: Cardinal = 1): Cardinal;
begin
  //Retrieve the index from where the string start, excluding the specified delimiter
  Result := PosEx(SubStr, Str, Offset);
  if (Result > 0) then
    Result := Result + Cardinal(Length(SubStr));
end;

function EndIndex(SubStr, Str: String; Offset: Cardinal = 1): Cardinal;
begin
  //Retrieve the index where the string ends, excluding the specified delimiter
  Result := PosEx(SubStr,Str,Offset);
end;

function IndentLine(Val: WideString; Ch: Char; Len: Integer): WideString;
begin
  Result := StringOfChar(Ch, Len) + Val;
end;

function GetURLFromFile(const Filename: string; var URL: string): boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetURLFromStream(Stream, URL);
  finally
    Stream.Free;
  end;
end;

function GetURLFromStream(Stream: TStream; var URL: string): boolean;
var
  URLfile: TStringList;
  i: integer;
  s: string;
  ch: PChar;
begin
  Result := False;
  URLfile := TStringList.Create;
  try
    URLFile.LoadFromStream(Stream);
    i := 0;
    while (i < URLFile.Count-1) do
    begin
      if (CompareText(URLFile[i], '[InternetShortcut]') = 0) then
      begin
        inc(i);
        while (i < URLFile.Count) do
        begin
          s := URLFile[i];
          ch := PChar(s);
          if (StrLIComp(ch, 'URL=', length('URL=')) = 0) then
          begin
            inc(ch, length('URL='));
            URL := ch;
            Exit(True);
          end else
            if (ch^ = '[') then
              Exit;
          inc(i);
        end;
      end;
      inc(i);
    end;
  finally
    URLFile.Free;
  end;
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
