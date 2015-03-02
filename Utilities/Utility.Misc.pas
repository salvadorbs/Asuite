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

unit Utility.Misc;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Dialogs, ComCtrls, Clipbrd,
  Kernel.Consts, StdCtrls, XMLIntf, System.UITypes, DKLang, Menus;

{ Forms }
function  IsFormOpen(const FormName : string): Boolean;
procedure SetFormPosition(Form: TForm; ListFormLeft, ListFormTop: Integer);

{ Misc }
function CheckPropertyName(Edit: TEdit): Boolean;
function  GetCheckedMenuItem(PopupMenu: TPopupMenu): TMenuItem;
function GetASuiteVersion(ASimpleFormat: Boolean): string;
function  IsFormatInClipBoard(format: Word): Boolean;
function RemoveQuotes(const S: string; const QuoteChar: Char): string;
function RemoveAllQuotes(const S: string): string;
procedure ShowMessageEx(const Msg: string; Error: boolean=false);
procedure ShowMessageFmtEx(const Msg: string; Params: array of const; Error: boolean=false);

{ Stats }
function  DiskFloatToString(Number: double;Units: Boolean): string;
function  DiskFreeString(Drive: Char;Units: Boolean): string;
function  DiskSizeString(Drive: Char;Units: Boolean): string;
function  DiskUsedString(Drive: Char;Units: Boolean): string;

{ Integer }
function  CompareInteger(int1, int2: Integer): Integer;

{ HotKey }
function  GetHotKeyCode(AShortcut: TShortcut) : Word;
function  GetHotKeyMod(AShortcut: TShortcut) : Integer;

implementation

uses
  Registry, SynTaskDialog, PJVersionInfo, AppConfig.Main, Kernel.Logger;

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

function CheckPropertyName(Edit: TEdit): Boolean;
begin
  Result := True;
  // Check if inserted name is empty, then
  if (Trim(Edit.Text) = '') then
  begin
    ShowMessageEx(DKLangConstW('msgErrEmptyName'),true);
    Edit.Color := clYellow;
    Result := False;
  end;
end;

function GetCheckedMenuItem(PopupMenu: TPopupMenu): TMenuItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PopupMenu.Items.Count - 1 do
    if PopupMenu.Items[I].Checked then
      Result := PopupMenu.Items[I];
end;

function GetASuiteVersion(ASimpleFormat: Boolean): string;
var
  VersionInfo: TPJVersionInfo;
begin
  VersionInfo := TPJVersionInfo.Create(nil);
  try
    VersionInfo.FileName := Config.Paths.SuiteFullFileName;
    if ASimpleFormat then
    begin
      //Version format "Major.Minor Beta"
      Result := Format('%d.%d', [VersionInfo.FileVersionNumber.V1,
                                 VersionInfo.FileVersionNumber.V2]);
    end
    else begin
      //Version format "Major.Minor.Revision.Build Beta"
      Result := Format('%d.%d.%d.%d', [VersionInfo.FileVersionNumber.V1,
                                       VersionInfo.FileVersionNumber.V2,
                                       VersionInfo.FileVersionNumber.V3,
                                       VersionInfo.FileVersionNumber.V4]);
    end;
    if VERSION_PRERELEASE <> '' then
      Result := Result + ' ' + VERSION_PRERELEASE;
  finally
    VersionInfo.Free;
  end;
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

function RemoveQuotes(const S: string; const QuoteChar: Char): string;
var
  Len: Integer;
begin
  Result := S;

  Len := Length(Result);

  if (Len < 2) then Exit;                    //Quoted text must have at least 2 chars
  if (Result[1] <> QuoteChar) then Exit;     //Text is not quoted
  if (Result[Len] <> QuoteChar) then Exit;   //Text is not quoted

  System.Delete(Result, Len, 1);
  System.Delete(Result, 1, 1);

  Result := StringReplace(Result, QuoteChar + QuoteChar, QuoteChar, [rfReplaceAll]);
end;

function RemoveAllQuotes(const S: string): string;
begin
  Result := RemoveQuotes(S, '''');
  Result := RemoveQuotes(Result, '"');
end;

procedure ShowMessageEx(const Msg: string; Error: boolean=false);
const
  IconError: array[boolean] of TTaskDialogIcon = (tiInformation, tiError);
var
  Task: TTaskDialog;
begin
  Task.Content := Msg;
  Task.Execute([cbOK],mrOk,[],IconError[Error]);
end;

procedure ShowMessageFmtEx(const Msg: string; Params: array of const; Error: boolean=false);
const
  IconError: array[boolean] of TTaskDialogIcon = (tiInformation, tiError);
var
  Task: TTaskDialog;
begin
  Task.Content := Format(Msg, Params);
  Task.Execute([cbOK],mrOk,[],IconError[Error]);

  if Error then
    TASuiteLogger.Error(Msg, Params);
end;

{ Stats }

function DiskFloatToString(Number: Double;Units: Boolean): string;
var
  TypeSpace : String;
begin
  if Number >= 1024 then
  begin
    //KiloBytes
    Number    := Number / (1024);
    TypeSpace := ' KB';
    if Number >= 1024 then
    begin
      //MegaBytes
      Number    := Number / (1024);
      TypeSpace := ' MB';
      if Number >= 1024 then
      begin
        //GigaBytes
        Number    := Number / (1024);
        TypeSpace := ' GB';
      end;
    end;
  end;
  Result := FormatFloat('0.00',Number);
  if Units then
    Result := Result + TypeSpace;
end;

function DiskFreeString(Drive: Char;Units: Boolean): string;
var
  Free : Double;
begin
  Free   := DiskFree(Ord(Drive) - 64);
  Result := DiskFloatToString(Free,Units);
end;

function DiskSizeString(Drive: Char;Units: Boolean): string;
var
  Size : Double;
begin
  Size   := DiskSize(Ord(Drive) - 64);
  Result := DiskFloatToString(Size,Units);
end;

function DiskUsedString(Drive: Char;Units: Boolean): string;
var
  Free : Double;
  Size : Double;
begin
  Free   := (DiskFree(Ord(Drive) - 64));
  Size   := (DiskSize(Ord(Drive) - 64));
  Result := DiskFloatToString(Size - Free,Units);
end;

function CompareInteger(int1, int2: Integer): Integer;
begin
  //Result
  //1 = int1 is bigger
  //0  = Same
  //-1  = int2 is bigger
  if int1 < int2 then
    Result := -1
  else
    if int1 > int2 then
      Result := 1
    else
      Result := 0;
end;

Function GetHotKeyCode(AShortcut: TShortcut): Word;
var
  Shift: TShiftState;
begin
  Result := 0;
  ShortCutToKey(AShortcut, Result, Shift);
end;

Function GetHotKeyMod(AShortcut: TShortcut): Integer;
var
  Shift: TShiftState;
  Key: Word;
begin
  Result := 0;
  ShortCutToKey(AShortcut, Key, Shift);
  if (ssShift in Shift) then
  Result := Result or MOD_SHIFT;
  if (ssAlt in Shift) then
  Result := Result or MOD_ALT;
  if (ssCtrl in Shift) then
  Result := Result or MOD_CONTROL;
end;

end.
