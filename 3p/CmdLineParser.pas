unit CmdLineParser;

interface

uses
  SysUtils, CmdLineHelper;

function GetCmdLineSwitch(const ASwitch: string; const IgnoreCase: Boolean = True): Boolean;
function GetCmdLineSwitchValue(out AValue: string; const ASwitch: string; const IgnoreCase: Boolean = True): Boolean;

implementation

function GetCmdLineSwitch(const ASwitch: string; const IgnoreCase: Boolean = True): Boolean;
begin
  Result := FindCmdLineSwitch(ASwitch, ['-', '/'], IgnoreCase);
end;

{**************************************************************************
* NAME:    FindCmdLineSwitchValue
* DESC:    Kommandozeilenparser für Optionen der Form:    ['-'|'/']Name[':'|'=']Wert
*
* Beispiel:
*   Kommandozeile "-a:WertA /b:WertB -c=WertC -d=WertD
*    FindCmdLineSwitchValue(Value,'a',False) ==> Result=True, Value=WertA
*    FindCmdLineSwitchValue(Value,'A',False) ==> Result=False
*    FindCmdLineSwitchValue(Value,'A',True) ==> Result=False, Value=WertA
* PARAMS:  out AValue: string; const ASwitch: string; const IgnoreCase: Boolean = True
* RESULT:  Boolean
* CREATED: 14-03-2003/haide
* CHANGED:
*************************************************************************}

function GetCmdLineSwitchValue(out AValue: string; const ASwitch: string; const IgnoreCase: Boolean = True): Boolean;
const
  CompareFunction   : array[Boolean]

  of function(const s1, s2: string): Integer = (CompareStr, CompareText);
  var
    iCmdLine, iSplit: Integer;
    s, sName, sValue: string;
  begin
    Result := False;

    for iCmdLine := 1 to GetParamCount do
    begin
      s := GetParamStr(iCmdLine);

      if not (s[1] in ['-', '/']) then
        Continue;

      Delete(s, 1, 1);
      iSplit := Pos(':', s);
      if iSplit = 0 then
        iSplit := Pos('=', s);

      if iSplit = 0 then
        Continue;

      sName := Copy(s, 1, iSplit - 1);
      sValue := Copy(s, iSplit + 1, 666);

      if CompareFunction[IgnoreCase](ASwitch, sName) = 0 then
      begin
        AValue := sValue;
        Result := True;
        Break;
      end;
    end;
  end;

end.

