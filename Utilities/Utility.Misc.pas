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

unit Utility.Misc;

{$MODE DelphiUnicode}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms, Dialogs, ComCtrls, Clipbrd,
  UITypes, Menus;

{ Forms }
procedure SetFormPositionFromConfig(AForm: TForm);

{ Misc }
function GetCheckedMenuItem(PopupMenu: TPopupMenu): TMenuItem;
function IsFormatInClipBoard(format: Word): Boolean;
function RemoveAllQuotes(const S: string): string;

{ Dialogs }
procedure ShowMessageEx(const Msg: string; Error: boolean = False);
procedure ShowMessageFmtEx(const Msg: string; Params: array of const; Error: boolean = False);
function AskUserWarningMessage(const AMsg: string; Params: array of const): Boolean;
function AskUserCloseApp: Boolean;

{ Stats }
function  DiskFloatToString(Size: Int64; Units: Boolean): string;
function  GetDiskFreeSpace(const Path : String; out FreeSize, TotalSize : Int64) : Boolean;

{ Integer }
function  CompareInteger(int1, int2: Integer): Integer;

{ HotKey }
function  GetHotKeyCode(AShortcut: TShortcut) : Word;
function  GetHotKeyMod(AShortcut: TShortcut) : Integer;

implementation

uses
  AppConfig.Main, Kernel.Logger, LCLProc, Kernel.ResourceStrings
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}
  , BaseUnix, Unix
  {$ENDIF};

procedure SetFormPositionFromConfig(AForm: TForm);
begin
  if Assigned(AForm) then
  begin
    AForm.Position := poMainFormCenter;
    if Assigned(Config) and (not Config.DialogCenterMF) then
      AForm.Position := poScreenCenter;
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

function IsFormatInClipBoard(format: Word): Boolean;
var
  I: Integer;
begin
  //Get clipboard format
  Result := False;
  for I := 0 to Clipboard.FormatCount - 1 do
  begin
    if Clipboard.Formats[I] = format then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function RemoveAllQuotes(const S: string): string;
begin
  Result := AnsiDequotedStr(S, '''');
  Result := AnsiDequotedStr(Result, '"');
end;

procedure ShowMessageEx(const Msg: string; Error: boolean = False);
begin
  ShowMessageFmtEx(Msg, [], error);
end;

procedure ShowMessageFmtEx(const Msg: string; Params: array of const; Error: boolean = False);
const
  DlgType: array[boolean] of TMsgDlgType = (mtInformation, mtError);
begin
  MessageDlg(Format(Msg, Params), DlgType[Error], [mbOK], 0);

  if Error then
    TASuiteLogger.Error(Msg, Params);
end;

function AskUserWarningMessage(const AMsg: string; Params: array of const): Boolean;
begin
  //Returns true if user clicks on mbYes
  Result := (MessageDlg(Format(AMsg, Params), mtWarning, [mbYes, mbNo], 0) = mrYes);
end;

function AskUserCloseApp: Boolean;
begin
  if Config.ConfirmMsgCloseApp then
    Result := AskUserWarningMessage(msgConfirm, [])
  else
    Result := True;
end;

function DiskFloatToString(Size: Int64; Units: Boolean): string;
var
  TypeSpace : String;
  dblSize: Double;
begin
  //KiloBytes
  dblSize := Size;
  if dblSize >= 1024 then
  begin
    dblSize    := dblSize / (1024);
    TypeSpace := ' KB';

    //MegaBytes
    if dblSize >= 1024 then
    begin
      dblSize    := dblSize / (1024);
      TypeSpace := ' MB';

      //GigaBytes
      if dblSize >= 1024 then
      begin
        dblSize    := dblSize / (1024);
        TypeSpace := ' GB';
      end;
    end;
  end;

  Result := FormatFloat('0.00',dblSize);

  if Units then
    Result := Result + TypeSpace;
end;

//Warning: Code taken from DoubleCMD source (unit uOSUtils.pas)
function GetDiskFreeSpace(const Path : String; out FreeSize, TotalSize : Int64) : Boolean;
{$IFDEF UNIX}
var
  sbfs: TStatFS;
begin
  Result:= (fpStatFS(PAnsiChar(Path), @sbfs) = 0);
  if not Result then Exit;
  FreeSize := (Int64(sbfs.bavail) * sbfs.bsize);
  TotalSize := (Int64(sbfs.blocks) * sbfs.bsize);
end;
{$ELSE}
begin
  FreeSize := 0;
  TotalSize := 0;
  Result:= GetDiskFreeSpaceExW(PWideChar(Path), FreeSize, TotalSize, nil);
end;
{$ENDIF}

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

function GetHotKeyCode(AShortcut: TShortcut): Word;
var
  Shift: TShiftState;
begin
  Result := 0;
  ShortCutToKey(AShortcut, Result, Shift);
end;

function GetHotKeyMod(AShortcut: TShortcut): Integer;
var
  Shift: TShiftState;
  Key: Word;
const
  MOD_ALT = 1;
  MOD_CONTROL = 2;
  MOD_SHIFT = 4;
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
