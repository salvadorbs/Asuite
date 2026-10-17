unit Tests.MiscUtils;

{$mode objfpc}{$H+}

{ Behaviour tests for the side-effect-free helpers in Utility.Misc.

  String/integer formatting, hotkey bit operations, disk size formatting and
  the menu helper run without any application state. The dialog procedures
  (ShowMessageEx/ShowMessageFmtEx/AskUserWarningMessage/AskUserCloseApp)
  are deliberately excluded: they are modal. }

interface

uses
  Classes, SysUtils, Menus, Clipbrd, LCLProc, fpcunit, testregistry,
  Utility.Misc;

type
  TTestMiscUtils = class(TTestCase)
  published
    procedure TestRemoveAllQuotes;
    procedure TestDoubleAmpersands;
    procedure TestCompareInteger;
    procedure TestGetHotKeyCode;
    procedure TestGetHotKeyMod;
    procedure TestDiskFloatToString;
    procedure TestGetCheckedMenuItem;
    procedure TestIsFormatInClipBoardAbsent;
    procedure TestGetDiskFreeSpace;
  end;

implementation

procedure TTestMiscUtils.TestRemoveAllQuotes;
begin
  AssertEquals('double quotes stripped', 'quoted',
    UTF8Encode(RemoveAllQuotes('"quoted"')));
  AssertEquals('single quotes stripped', 'quoted',
    UTF8Encode(RemoveAllQuotes('''quoted''')));
  AssertEquals('plain text untouched', 'plain',
    UTF8Encode(RemoveAllQuotes('plain')));
  AssertEquals('empty stays empty', '', UTF8Encode(RemoveAllQuotes('')));
end;

procedure TTestMiscUtils.TestDoubleAmpersands;
begin
  AssertEquals('ampersand doubled', 'a&&b', UTF8Encode(DoubleAmpersands('a&b')));
  AssertEquals('plain text untouched', 'abc', UTF8Encode(DoubleAmpersands('abc')));
end;

procedure TTestMiscUtils.TestCompareInteger;
begin
  AssertTrue('less', CompareInteger(1, 2) < 0);
  AssertTrue('greater', CompareInteger(2, 1) > 0);
  AssertEquals('equal', 0, CompareInteger(5, 5));
end;

procedure TTestMiscUtils.TestGetHotKeyCode;
begin
  AssertEquals('plain key', Ord('A'), GetHotKeyCode(TextToShortCut('A')));
  AssertEquals('key with modifiers', Ord('A'),
    GetHotKeyCode(TextToShortCut('Ctrl+A')));
end;

procedure TTestMiscUtils.TestGetHotKeyMod;
begin
  AssertEquals('no modifiers', 0, GetHotKeyMod(TextToShortCut('A')));
  // MOD_CONTROL = 2, MOD_SHIFT = 4, MOD_ALT = 1
  AssertEquals('ctrl', 2, GetHotKeyMod(TextToShortCut('Ctrl+A')));
  AssertEquals('ctrl+shift', 6, GetHotKeyMod(TextToShortCut('Ctrl+Shift+B')));
end;

procedure TTestMiscUtils.TestDiskFloatToString;
var
  OldSeparator: Char;
begin
  // FormatFloat follows the locale: pin the separator for determinism
  OldSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    AssertEquals('bytes', '0.00', UTF8Encode(DiskFloatToString(0, True)));
    AssertEquals('under one KB', '512.00', UTF8Encode(DiskFloatToString(512, True)));
    AssertEquals('one KB', '1.00 KB', UTF8Encode(DiskFloatToString(1024, True)));
    AssertEquals('fractional KB', '1.50 KB', UTF8Encode(DiskFloatToString(1536, True)));
    AssertEquals('one MB', '1.00 MB', UTF8Encode(DiskFloatToString(1048576, True)));
    AssertEquals('one GB', '1.00 GB', UTF8Encode(DiskFloatToString(1073741824, True)));
    AssertEquals('units optional', '2.00', UTF8Encode(DiskFloatToString(2048, False)));
  finally
    DefaultFormatSettings.DecimalSeparator := OldSeparator;
  end;
end;

procedure TTestMiscUtils.TestGetCheckedMenuItem;
var
  Popup: TPopupMenu;
  First, Second: TMenuItem;
begin
  Popup := TPopupMenu.Create(nil);
  try
    AssertTrue('empty menu finds nothing', GetCheckedMenuItem(Popup) = nil);

    First := TMenuItem.Create(Popup);
    First.Caption := 'first';
    Popup.Items.Add(First);
    Second := TMenuItem.Create(Popup);
    Second.Caption := 'second';
    Popup.Items.Add(Second);
    AssertTrue('unchecked finds nothing', GetCheckedMenuItem(Popup) = nil);

    First.Checked := True;
    AssertTrue('checked item found', GetCheckedMenuItem(Popup) = First);

    Second.Checked := True;
    AssertTrue('last checked wins', GetCheckedMenuItem(Popup) = Second);
  finally
    Popup.Free;
  end;
end;

procedure TTestMiscUtils.TestIsFormatInClipBoardAbsent;
var
  NeverUsed: Word;
begin
  // A freshly registered format can never be on the clipboard: read-only
  // probe, deterministic on any display server. The explicit narrowing is
  // harmless: only presence is asserted, never the value.
  NeverUsed := Word(RegisterClipboardFormat('ASuiteTestNeverUsedFormat'));
  AssertFalse('absent format not found', IsFormatInClipBoard(NeverUsed));
end;

procedure TTestMiscUtils.TestGetDiskFreeSpace;
var
  FreeSize, TotalSize: Int64;
begin
  AssertTrue('temp dir queried', GetDiskFreeSpace(UTF8Decode(GetTempDir(False)),
    FreeSize, TotalSize));
  AssertTrue('free space reported', FreeSize > 0);
  AssertTrue('total covers free', TotalSize >= FreeSize);
end;

initialization
  RegisterTest(TTestMiscUtils);

end.
