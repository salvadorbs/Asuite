{
Copyright (C) 2006-2020 Matteo Salvi

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

unit AppConfig.PathVars;

{$MODE DelphiUnicode}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, RegExpr;

type
  TVarsList = TFastHashMap<string, string>;
  TKeyValue = TPair<string, string>;

  { TPathVars }

  TPathVars = class
  private
    FItems: TVarsList;
    FRegExp: String;
    function ReplaceVar(APath: String; AVarKey: String; AVarKeyQuoted: String): String;
    function KeyToValue(const AVarKey: String): String;
  public
    constructor Create(const ARegExp: String);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: String; const AValue: String);

    function ExpandVars(const Str: string): string;
    function DeExpandVars(const APath: string; AKey: String): string;

    property RegExp: String read FRegExp write FRegExp;
  end;

implementation

{ TPathVars }

constructor TPathVars.Create(const ARegExp: String);
begin
  FItems  := TVarsList.Create;
  FRegExp := ARegExp;
end;

destructor TPathVars.Destroy;
begin
  inherited Destroy;

  FItems.Clear;
  FreeAndNil(FItems);
end;

procedure TPathVars.Clear;
begin
  FItems.Clear;
end;

procedure TPathVars.Add(const AKey: String; const AValue: String);
begin
  if not(FItems.ContainsValue(AKey)) then
    FItems.Add(AKey, AValue);
end;

function TPathVars.KeyToValue(const AVarKey: String): String;
var
  Item: TKeyValue;
begin
  Result := '';

  Assert(Assigned(FItems));

  for Item in FItems do
  begin
    if Item.Key = UpperCase(AVarKey) then
    begin
      Result := Item.Value;
      break;
    end;
  end;
end;

function TPathVars.ReplaceVar(APath: String; AVarKey: String; AVarKeyQuoted: String): String;
var
  VarValue: String;
begin
  Result := APath;

  VarValue := KeyToValue(AVarKey);
  if VarValue <> '' then
    Result := StringReplace(APath, AVarKeyQuoted, VarValue, [rfIgnoreCase]);
end;

function TPathVars.ExpandVars(const Str: string): string;
var
  RegexObj: TRegExpr;
begin
  //Return param str as Result, if after regex, we have no replace vars
  Result := Str;

  RegexObj := TRegExpr.Create;
  try
    RegexObj.Expression  := FRegExp;
    RegexObj.InputString := Str;

    //Execute RegExp
    if RegexObj.Exec then
    begin
      //Match[0] -> Full Match (ex. "%ProgramFiles%")
      //Match[x] -> Group x (ex. "ProgramFiles")
      Result := ReplaceVar(Result, RegexObj.Match[1], RegexObj.Match[0]);

      //Find other env vars
      while RegexObj.ExecNext do
        Result := ReplaceVar(Result, RegexObj.Match[1], RegexObj.Match[0]);
    end;
  finally
    RegexObj.Free;
  end;
end;

function TPathVars.DeExpandVars(const APath: string; AKey: String): string;
begin
  Result := StringReplace(APath, KeyToValue(AKey), AnsiQuotedStr(LowerCase(AKey), '%'), [rfIgnoreCase, rfReplaceAll]);
end;

end.

