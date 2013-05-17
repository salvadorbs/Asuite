{

    Description:
    Utilities for cyIEWrappers pack

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
unit cyIEUtils;

interface

Uses Classes, Graphics, Windows, SysUtils, Registry;

type
  // TWebBrowser print page Setup :
  TwbPageSetup = Record
    font: String;
    footer: String;
    header: String;
    margin_bottom: String;
    margin_left: String;
    margin_right: String;
    margin_top: String;
    Print_Background: String;
    Shrink_To_Fit: String;
  End;


function URLEncode(const S: string): string;
function MakeResourceURL(const ModuleName: string; const ResName: PChar; const ResType: PChar = nil): string; overload;
function MakeResourceURL(const Module: HMODULE; const ResName: PChar; const ResType: PChar = nil): string; overload;

function ColorToHtml(aColor: TColor): String;
function HtmlToColor(aHtmlColor: String): TColor;

{$IFDEF UNICODE}
function GetStreamEncoding(aStream: TStream): TEncoding;
function IsStreamEncodedWith(aStream: TStream; Encoding: TEncoding): Boolean;
{$ENDIF}

function AddHtmlUnicodePrefix(aHtml: String): String;
function RemoveHtmlUnicodePrefix(aHtml: String): String;

procedure GetPageSetupFromRegistry(var awbPageSetup: TwbPageSetup);
procedure SetPageSetupToRegistry(awbPageSetup: TwbPageSetup);

const
   IEBodyBorderless = 'none';
   IEBodySingleBorder = '';
   IEDesignModeOn = 'On';
   IEDesignModeOff = 'Off';

  // Html string encoding prefix for UTF-8, Unicode and Unicode big endian charset :
  {$IFDEF UNICODE}
  cHtmlUnicodePrefixChar = #$FEFF;
  {$ELSE}
  cHtmlUnicodePrefixChar = #$FE;
  {$ENDIF}

implementation

{
  The following 3 functions are taken from the DelphiDabbler article "How to
  create and use HTML resource files" at
  http://www.delphidabbler.com/articlesarticle=10
}

{ Do not use this function to URL encode query strings: see the more flexible
  version in the Code Snippets Database at
  http://www.delphidabbler.com/codesnip?action=named&routines=URLEncode }
function URLEncode(const S: string): string;
var I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    {$IFDEF UNICODE}
    if CharInSet(S[I], ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.']) then
    {$ELSE}
    if S[I] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.'] then
    {$ENDIF}
      Result := Result + S[I]
    else
      Result := Result + '%' + IntToHex(Ord(S[I]), 2);
  end;
end;

function MakeResourceURL(const ModuleName: string; const ResName: PChar;
  const ResType: PChar = nil): string; overload;

    function ResNameOrTypeToString(R: PChar): string;
    begin
      if HiWord(LongWord(R)) = 0
      then
        // high word = 0 => numeric resource id
        // numeric value is stored in low word
        Result := Format('#%d', [LoWord(LongWord(R))])
      else
        // high word <> 0 => string value
        // PChar is implicitly converted to string
        Result := R;
    end;

begin
  Assert(ModuleName <> '');
  Assert(Assigned(ResName));
  Result := 'res://' + URLEncode(ModuleName);
  if Assigned(ResType)
  then Result := Result + '/' + URLEncode(ResNameOrTypeToString(ResType));

  Result := Result + '/' + URLEncode(ResNameOrTypeToString(ResName));
end;

function MakeResourceURL(const Module: HMODULE; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
begin
  Result := MakeResourceURL(GetModuleName(Module), ResName, ResType);
end;





function ColorToHtml(aColor: TColor): String;
var RGBValue, r,g,b: Integer;
begin
  RGBValue := ColorToRGB(aColor);

  R := GetRValue(RGBValue);
  G := GetGValue(RGBValue);
  B := GetBValue(RGBValue);

  RESULT := '#' + IntToHex(R, 2) + IntToHex(G, 2) + IntToHex(B, 2);
end;

function HtmlToColor(aHtmlColor: String): TColor;
var
  R, G, B: Integer;
begin
  if aHtmlColor <> ''
  then begin
    try
      R := StrToInt('$' + Copy(aHtmlColor, 2, 2));
      G := StrToInt('$' + Copy(aHtmlColor, 4, 2));
      B := StrToInt('$' + Copy(aHtmlColor, 6, 2));
      RESULT := RGB(R, G, B);
    except
      RESULT := clNone;
    end;
  end
  else
    RESULT := clNone;
end;

{$IFDEF UNICODE}
function GetStreamEncoding(aStream: TStream): TEncoding;
var
  Bytes: TBytes;
  Size: Int64;
begin
  aStream.Seek(0, soFromBeginning);
  Size := aStream.Size;
  SetLength(Bytes, Size);
  aStream.ReadBuffer(Pointer(Bytes)^, Size);
  TEncoding.GetBufferEncoding(Bytes, RESULT);
end;
{$ENDIF}

{$IFDEF UNICODE}
function IsStreamEncodedWith(aStream: TStream; Encoding: TEncoding): Boolean;
var
  BOM, Bytes: TBytes;   // Encoding Byte Order Mark ...
  BOMSize: Integer;
begin
  RESULT := false;
  BOM := Encoding.GetPreamble;
  BOMSize := Length(BOM);

  if aStream.Size >= BOMSize
  then begin
    aStream.Seek(0, soFromBeginning);
    SetLength(Bytes, BOMSize);
    aStream.ReadBuffer(Pointer(Bytes)^, BOMSize);

    if CompareMem(@BOM[0], @Bytes[0], BOMSize)
    then RESULT := true;
  end;
end;
{$ENDIF}

function AddHtmlUnicodePrefix(aHtml: String): String;
begin
  Result := aHtml;

  if Result <> ''
  then
    if Result[1] <> cHtmlUnicodePrefixChar
    then Insert(cHtmlUnicodePrefixChar, Result, 1);
end;

function RemoveHtmlUnicodePrefix(aHtml: String): String;
begin
  Result := aHtml;

  if Result <> ''
  then
    if Result[1] = cHtmlUnicodePrefixChar
    then Delete(Result, 1, 1);
end;

procedure GetPageSetupFromRegistry(var awbPageSetup: TwbPageSetup);
var Reg: TRegistry;
begin
  Reg := TRegistry.Create;

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Internet Explorer\PageSetup', True);
    awbPageSetup.font             := Reg.ReadString('font');
    awbPageSetup.footer           := Reg.ReadString('footer');
    awbPageSetup.header           := Reg.ReadString('header');
    awbPageSetup.margin_bottom    := Reg.ReadString('margin_bottom');
    awbPageSetup.margin_left      := Reg.ReadString('margin_left');
    awbPageSetup.margin_right     := Reg.ReadString('margin_right');
    awbPageSetup.margin_top       := Reg.ReadString('margin_top');
    awbPageSetup.Print_Background := Reg.ReadString('Print_Background');
    awbPageSetup.Shrink_To_Fit    := Reg.ReadString('Shrink_To_Fit');
    Reg.CloseKey;
  finally
    // Deixar mostrar o erro ...
  end;

  Reg.Free;
end;

procedure SetPageSetupToRegistry(awbPageSetup: TwbPageSetup);
var Reg: TRegistry;
begin
  Reg := TRegistry.Create;

  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Internet Explorer\PageSetup', True);
    Reg.WriteString('font', awbPageSetup.font);
    Reg.WriteString('footer', awbPageSetup.footer);
    Reg.WriteString('header', awbPageSetup.header);
    Reg.WriteString('margin_bottom', awbPageSetup.margin_bottom);
    Reg.WriteString('margin_left', awbPageSetup.margin_left);
    Reg.WriteString('margin_right', awbPageSetup.margin_right);
    Reg.WriteString('margin_top', awbPageSetup.margin_top);
    Reg.WriteString('Print_Background', awbPageSetup.Print_Background);
    Reg.WriteString('Shrink_To_Fit', awbPageSetup.Shrink_To_Fit);
    Reg.CloseKey;
  finally
    // Deixar mostrar o erro ...
  end;

  Reg.Free;
end;

end.
