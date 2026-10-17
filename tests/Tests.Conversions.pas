unit Tests.Conversions;

{$mode objfpc}{$H+}

{ GUI-suite tests for Utility.Conversions: color <-> HTML round-trips and
  the 'Name|Size|Color|Style' font serialization used by the options. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Graphics,
  Utility.Conversions;

type
  TTestConversions = class(TTestCase)
  published
    procedure TestColorRoundTrip;
    procedure TestKnownCssName;
    procedure TestFontParse;
    procedure TestFontEmptyLeavesUnchanged;
    procedure TestFontWrongFieldCountLeavesUnchanged;
    procedure TestInvalidInputIsSafe;
  end;

implementation

procedure TTestConversions.TestColorRoundTrip;
const
  Colors: array[0..9] of TColor = (
    clRed, clLime, clBlue, clWhite, clBlack,
    clYellow, clAqua, clFuchsia, clGray, TColor($123456));
var
  I: Integer;
begin
  for I := Low(Colors) to High(Colors) do
    AssertTrue('Round trip ' + IntToStr(I),
      HtmlToColor(ColorToHtml(Colors[I])) = Colors[I]);
end;

procedure TTestConversions.TestKnownCssName;
begin
  AssertTrue('Non empty', ColorToHtml(clRed) <> '');
  AssertTrue('Named color', HtmlToColor('red') = clRed);
end;

procedure TTestConversions.TestFontParse;
var
  F: TFont;
begin
  F := TFont.Create;
  try
    StrToFont('Arial|12|' + ColorToHtml(clRed) + '|1', F);
    AssertTrue('Name', F.Name = 'Arial');
    AssertEquals('Size', 12, F.Size);
    AssertTrue('Color', F.Color = clRed);
    AssertTrue('Style', F.Style = TFontStyles([fsBold]));
  finally
    F.Free;
  end;
end;

procedure TTestConversions.TestFontEmptyLeavesUnchanged;
var
  F: TFont;
begin
  F := TFont.Create;
  try
    F.Name := 'Sentinel';
    StrToFont('', F);
    AssertTrue('Unchanged', F.Name = 'Sentinel');
  finally
    F.Free;
  end;
end;

procedure TTestConversions.TestFontWrongFieldCountLeavesUnchanged;
var
  F: TFont;
begin
  F := TFont.Create;
  try
    F.Name := 'Sentinel';
    StrToFont('Arial|12', F);
    AssertTrue('Unchanged', F.Name = 'Sentinel');
  finally
    F.Free;
  end;
end;

procedure TTestConversions.TestInvalidInputIsSafe;
begin
  // Neither must raise; the color falls back to the BGRA default.
  HtmlToColor('not-a-color');
  StrToFont('Arial|12|red|1', nil);
end;

initialization
  RegisterTest(TTestConversions);

end.
