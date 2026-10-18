unit Tests.SystemUtils;

{$mode objfpc}{$H+}

{ Behaviour tests for the side-effect-free helpers in Utility.System plus
  the hotkey code tables in Utility.XML.

  IsValidURLProtocol, ExtractDirectoryName, ConvertWindowStateToSWOptions
  and the hotkey tables are pure. IsPathExists resolves through
  ASuiteInstance.Paths (which exists in the test binary) and IsExecutableFile
  probes real files, so those tests use temp files. Launching processes
  (RunFile/CreateProcessEx), the registry startup helpers and the eject
  dialog are deliberately excluded. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  LCLIntf, LCLType, process,
  Utility.System, Utility.XML
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}
  ;

type
  TTestSystemUtils = class(TTestCase)
  published
    procedure TestIsValidURLProtocolTable;
    procedure TestIsValidURLProtocolRejects;
    procedure TestIsPathExists;
    procedure TestIsExecutableFile;
    procedure TestExtractDirectoryName;
    procedure TestConvertWindowStateToSWOptions;
    procedure TestGetHotKeyCodeTable;
    procedure TestGetHotKeyModTable;
  end;

implementation

procedure TTestSystemUtils.TestIsValidURLProtocolTable;
const
  Protocols: array[1..12] of string = (
    'ftp://x', 'http://x', 'gopher://x', 'mailto:a@b', 'news:x', 'nntp://x',
    'telnet://x', 'wais://x', 'file://x', 'prospero://x', 'https://x',
    'steam://x');
var
  I: Integer;
begin
  for I := Low(Protocols) to High(Protocols) do
    AssertTrue('protocol accepted: ' + Protocols[I],
      IsValidURLProtocol(UTF8Decode(Protocols[I])));
end;

procedure TTestSystemUtils.TestIsValidURLProtocolRejects;
begin
  AssertFalse('plain path rejected', IsValidURLProtocol('/tmp/x'));
  AssertFalse('drive path rejected', IsValidURLProtocol('C:\foo'));
  AssertFalse('empty rejected', IsValidURLProtocol(''));
  AssertFalse('prefix match required', IsValidURLProtocol('xhttp://y'));
  AssertFalse('unknown scheme rejected', IsValidURLProtocol('httpx://y'));
end;

procedure TTestSystemUtils.TestIsPathExists;
var
  Tmp: string;
  FS: TFileStream;
begin
  Tmp := GetTempDir(False) + 'asuite_test_exists.tmp';
  FS := TFileStream.Create(Tmp, fmCreate);
  try
    AssertTrue('existing file', IsPathExists(UTF8Decode(Tmp)));
    AssertTrue('existing directory', IsPathExists(UTF8Decode(GetTempDir(False))));
    AssertTrue('url protocol', IsPathExists('https://example.com/x'));
    // The 'shell:AppsFolder' special case in IsPathExists compares the
    // CONVERTED path, but RelativeToAbsolute expands it first, so the
    // literal never matches: documents current behaviour, not intent
    AssertFalse('shell special case never matches', IsPathExists('shell:AppsFolder'));
    AssertFalse('missing path', IsPathExists(UTF8Decode(Tmp + '.missing')));
    AssertFalse('empty path', IsPathExists(''));
  finally
    FS.Free;
    DeleteFile(Tmp);
  end;
end;

procedure TTestSystemUtils.TestIsExecutableFile;
{$IFDEF UNIX}
var
  Tmp: string;
  FS: TFileStream;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Tmp := GetTempDir(False) + 'asuite_test_exec.tmp';
  FS := TFileStream.Create(Tmp, fmCreate);
  FS.Free;
  try
    AssertFalse('plain file is not executable', IsExecutableFile(UTF8Decode(Tmp)));
    AssertEquals('chmod succeeds', 0, fpchmod(PChar(Tmp),
      S_IRWXU or S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH));
    AssertTrue('executable bit detected', IsExecutableFile(UTF8Decode(Tmp)));
  finally
    DeleteFile(Tmp);
  end;
  {$ELSE}
  AssertTrue('exe extension', IsExecutableFile('x.exe'));
  AssertTrue('bat extension', IsExecutableFile('x.bat'));
  AssertTrue('cmd extension', IsExecutableFile('x.cmd'));
  AssertFalse('txt extension', IsExecutableFile('x.txt'));
  {$ENDIF}
end;

procedure TTestSystemUtils.TestExtractDirectoryName;
begin
  AssertEquals('file name of path', 'c', UTF8Encode(ExtractDirectoryName('/a/b/c')));
  AssertEquals('trailing delimiter ignored', 'b', UTF8Encode(ExtractDirectoryName('/a/b/')));
  AssertEquals('bare name survives', 'plain', UTF8Encode(ExtractDirectoryName('plain')));
end;

procedure TTestSystemUtils.TestConvertWindowStateToSWOptions;
begin
  AssertTrue('default', ConvertWindowStateToSWOptions(SW_SHOWDEFAULT) = swoShowDefault);
  AssertTrue('minimized', ConvertWindowStateToSWOptions(SW_SHOWMINNOACTIVE) = swoshowMinNOActive);
  AssertTrue('maximized', ConvertWindowStateToSWOptions(SW_SHOWMAXIMIZED) = swoShowMaximized);
  AssertTrue('unknown maps to none', ConvertWindowStateToSWOptions(9999) = swoNone);
end;

procedure TTestSystemUtils.TestGetHotKeyCodeTable;
begin
  AssertEquals('code 0 is A', VK_A, GetHotKeyCode(0));
  AssertEquals('code 1 is B', VK_B, GetHotKeyCode(1));
  AssertEquals('unknown code maps to zero', 0, GetHotKeyCode(9999));
end;

procedure TTestSystemUtils.TestGetHotKeyModTable;
begin
  AssertTrue('no modifier', GetHotKeyMod(-1) = []);
  AssertTrue('alt', GetHotKeyMod(0) = [ssAlt]);
  AssertTrue('ctrl', GetHotKeyMod(1) = [ssCtrl]);
  AssertTrue('shift', GetHotKeyMod(2) = [ssShift]);
  AssertTrue('ctrl+alt', GetHotKeyMod(3) = ([ssCtrl, ssAlt]));
  AssertTrue('all three', GetHotKeyMod(6) = ([ssShift, ssCtrl, ssAlt]));
  AssertTrue('unknown maps to none', GetHotKeyMod(99) = []);
end;

initialization
  RegisterTest(TTestSystemUtils);

end.
