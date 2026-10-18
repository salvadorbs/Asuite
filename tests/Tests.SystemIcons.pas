unit Tests.SystemIcons;

{$mode objfpc}{$H+}

{ Regression tests for the platform system-icon extractor
  (ASuiteComps, SystemIcons.Platform).

  TestMissingThemeIconDoesNotCrash pins a crash in the Qt widgetset. LCL
  declares QIcon_fromTheme with the fallback icon defaulting to nil, while
  the C binding dereferences that handle unconditionally and hands the
  resulting (null) QIcon& to QIcon::fromTheme, which reads it as soon as the
  theme lookup misses. Every icon name absent from the current icon theme -
  including any name on a platform with no theme at all, such as the Qt
  "offscreen" plugin - therefore segfaulted.

  A .desktop file whose Icon= points at a name that cannot exist forces that
  code path deterministically on every Unix widgetset. Windows does not read
  .desktop files and simply falls back to the default shell icon, so the same
  test stays safe there and on GTK, where a missing icon degrades to an empty
  bitmap. }

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry, BGRABitmap,
  SystemIcons.Platform;

type

  { TTestSystemIcons }

  TTestSystemIcons = class(TTestCase)
  private
    function WriteDesktopIconFile(const AIconName: string): string;
  published
    procedure TestMissingThemeIconDoesNotCrash;
    procedure TestUnknownExtensionDoesNotCrash;
  end;

implementation

function TTestSystemIcons.WriteDesktopIconFile(const AIconName: string): string;
var
  Desktop: TStringList;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'asuite_systemicons_regression.desktop';
  Desktop := TStringList.Create;
  try
    Desktop.Add('[Desktop Entry]');
    Desktop.Add('Type=Application');
    Desktop.Add('Name=ASuite system icon regression');
    Desktop.Add('Icon=' + AIconName);
    Desktop.SaveToFile(Result);
  finally
    Desktop.Free;
  end;
end;

procedure TTestSystemIcons.TestMissingThemeIconDoesNotCrash;
const
  MISSING_ICON = 'asuite-nonexistent-icon-name-regression';
var
  DesktopFile: string;
  Icon: TBGRABitmap;
begin
  DesktopFile := WriteDesktopIconFile(MISSING_ICON);
  try
    Icon := TSystemIconHelper.GetSystemIcon(DesktopFile, 32, nil);
    try
      AssertNotNull('system icon extractor must never return nil', Icon);
    finally
      Icon.Free;
    end;
  finally
    SysUtils.DeleteFile(DesktopFile);
  end;
end;

procedure TTestSystemIcons.TestUnknownExtensionDoesNotCrash;
var
  Icon: TBGRABitmap;
begin
  // An extension no MIME table knows about resolves to a generic theme icon,
  // which may or may not be present depending on the installed theme.
  Icon := TSystemIconHelper.GetSystemIcon(GetTempDir(False) +
    'asuite_unknown_extension_for_icons.zzzq', 32, nil);
  try
    AssertNotNull('system icon extractor must never return nil', Icon);
  finally
    Icon.Free;
  end;
end;

initialization
  RegisterTest(TTestSystemIcons);

end.
