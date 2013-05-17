{   Unit cyDateUtils

    Description:
    Unit with Date functions.

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

unit cyDateUtils;

interface

uses
  {$IFDEF UNICODE}
  SysUtils, DateUtils;
  {$ELSE}
  SysUtils, Controls, DateUtils;
  {$ENDIF}

    // *** Basic *** //
    function  LongDayName(aDate: TDate): String;
    function LongMonthName(aDate: TDate): String;
    function ShortYearOf(aDate: TDate): byte;

    // *** Conversions *** //
    function DateToStrYYYYMMDD(aDate: TDate): String;
    function StrYYYYMMDDToDate(aStr: String): TDate;

    function SecondsToMinutes(Seconds: Integer): Double;
    function MinutesToSeconds(Minutes: Double): Integer;

    function MinutesToHours(Minutes: Integer): Double;
    function HoursToMinutes(Hours: Double): Integer;

    function ShortTimeStringToTime(ShortTimeStr: String; const ShortTimeFormat: String = 'hh:nn:ss'): TDateTime;

    // *** Operations *** //
    procedure AddMonths(var aMonth, aYear: Word; Months: Integer);
    function MergeDateWithTime(aDate: TDate; aTime: TDateTime): TDateTime;
    // Error on DateUtils.MinutesBetween(Time1, Time2);
    // between '08:05' and '09:00' returns 54 mn instead of 55 mn !
    function GetMinutesBetween(DateTime1, DateTime2: TDateTime): Int64; overload;
    function GetMinutesBetween(From_ShortTimeStr, To_ShortTimeStr: String; const ShortTimeFormat: String = 'hh:nn:ss'): Int64; overload;
    function GetSecondsBetween(DateTime1, DateTime2: TDateTime): Int64; overload;
    function IntersectPeriods(Period1Begin, Period1End, Period2Begin, Period2End: TDateTime; var RsltBegin: TDateTime; RsltEnd: TDateTime): Boolean; overload;
    function IntersectPeriods(Period1Begin, Period1End, Period2Begin, Period2End: TDateTime): Boolean; overload;
    function TryToEncodeDate(Year, Month, Day: Integer; var RsltDate: TDateTime): Boolean;

     // *** Others *** //

implementation

var
  DefaultFormatSettings: TFormatSettings;

function LongDayName(aDate: TDate): String;
begin
  {$IFDEF DCC} // Delphi XE2/XE3 Win 32/64
  RESULT := DefaultFormatSettings.LongDayNames[DayOfWeek(aDate)];
  {$ELSE} // Delphi other
  RESULT := LongDayNames[DayOfWeek(aDate)];
  {$ENDIF DCC}
end;

function LongMonthName(aDate: TDate): String;
begin
  {$IFDEF DCC} // Delphi XE2/XE3 Win 32/64
  RESULT := DefaultFormatSettings.LongMonthNames[MonthOf(aDate)];
  {$ELSE} // Delphi other
  RESULT := LongMonthNames[MonthOf(aDate)];
  {$ENDIF DCC}
end;

function ShortYearOf(aDate: TDate): byte;
var aYear : Word;
begin
  aYear := YearOf(aDate);
  RESULT := aYear - ((aYear div 100) * 100);
end;

function DateToStrYYYYMMDD(aDate: TDate): String;
var Y1, M1, D1 : Word;
begin
  DecodeDate(aDate, Y1, M1, D1);
  RESULT := FormatFloat('0000', Y1) + FormatFloat('00', M1) + FormatFloat('00', D1);
end;

function StrYYYYMMDDToDate(aStr: String): TDate;
var
  strY, strM, strD : String;
  aY, aM, aD: Integer;
begin
  strY := Copy(aStr, 1, 4);
  if not TryStrToInt(strY, aY) then aY := 1900;
  strM := Copy(aStr, 5, 2);
  if not TryStrToInt(strM, aM) then aM := 1;
  strD := Copy(aStr, 7, 2);
  if not TryStrToInt(strD, aD) then aD := 1;

  RESULT := EncodeDate(aY, aM, aD);
end;

function SecondsToMinutes(Seconds: Integer): Double;
begin
  Result := (Seconds div 60) + ( (Seconds mod 60)/100 );
end;

function MinutesToSeconds(Minutes: Double): Integer;
begin
  Result := 60 * (Round(Minutes * 100) div 100) + Round(Minutes * 100) - ((Round(Minutes * 100) div 100) * 100);
end;

function MinutesToHours(Minutes: Integer): Double;
begin
  Result := (Minutes div 60) + ( (Minutes mod 60)/100 );
end;

function HoursToMinutes(Hours: Double): Integer;
begin
  Result := 60 * (Round(Hours * 100) div 100) + Round(Hours * 100) - ((Round(Hours * 100) div 100) * 100);
end;

procedure AddMonths(var aMonth, aYear: Word; Months: Integer);
begin
  aMonth := aMonth + Months;

  while aMonth > 12 do
  begin
    aYear := aYear + 1;
    aMonth := aMonth - 12;
  end;

  while aMonth < 1 do
  begin
    aYear := aYear - 1;
    aMonth := aMonth + 12;
  end;
end;

function MergeDateWithTime(aDate: TDate; aTime: TDateTime): TDateTime;
var
  Ano, Mes, Dia: Word;
  Hora, Minutos, segundos, MSegundos: Word;
begin
  DecodeDate(aDate, Ano, Mes, Dia);
  DecodeTime(aTime, Hora, Minutos, segundos, MSegundos);
  RESULT := EncodeDateTime(Ano, Mes, Dia, Hora, Minutos, segundos, MSegundos);
end;

function ShortTimeStringToTime(ShortTimeStr: String; const ShortTimeFormat: String = 'hh:nn:ss'): TDateTime;
var
  i: Integer;
  fs: TFormatSettings;
begin
  Result := 0;
  if ShortTimeStr = '' then Exit;

  // Determine time separator :
  for i := 1 to length(ShortTimeFormat) do
    if not (ShortTimeFormat[i] in ['h', 'n', 's', 'H', 'N', 'S']) then
    begin
      fs.TimeSeparator := ShortTimeFormat[i];
      Break;
    end;

  try
    fs.ShortTimeFormat := ShortTimeFormat;
    Result := StrToTime(ShortTimeStr, fs);
  finally

  end;
end;

function GetMinutesBetween(DateTime1, DateTime2: TDateTime): Int64;
var Value: Extended;
begin
  Result := 0;

  if DateTime1 > DateTime2
  then Value := DateTime1 - DateTime2
  else Value := DateTime2 - DateTime1;

  // Exemple between '08:05' and '09:00': Result := 0,10069444444 * 1440 = 144,9999999936 minutes
  Result := Round(Value * 1440);              // 1 day = 1440 minutes
end;

function GetMinutesBetween(From_ShortTimeStr, To_ShortTimeStr: String; const ShortTimeFormat: String = 'hh:nn:ss'): Int64;
var Time1, Time2: TDateTime;
begin
  Time1 := ShortTimeStringToTime(From_ShortTimeStr, ShortTimeFormat);
  Time2 := ShortTimeStringToTime(To_ShortTimeStr, ShortTimeFormat);

  if Time1 > Time2 then   // Add one day to Time2 :
    Time2 := Time2 + 1;

  Result := GetMinutesBetween(Time1, Time2);
end;

function GetSecondsBetween(DateTime1, DateTime2: TDateTime): Int64;
var Value: Extended;
begin
  Result := 0;

  if DateTime1 > DateTime2
  then Value := DateTime1 - DateTime2
  else Value := DateTime2 - DateTime1;

  // Exemple between '08:05' and '09:00': Result := 0,10069444444 * 1440 = 8699,999999616 seconds
  Result := Round(Value * 86400);              // 1 day = 86400 seconds
end;

function IntersectPeriods(Period1Begin, Period1End, Period2Begin, Period2End: TDateTime; var RsltBegin: TDateTime; RsltEnd: TDateTime): Boolean;
begin
  Result := false;
  if Period1Begin > Period2Begin
  then RsltBegin := Period1Begin
  else RsltBegin := Period2Begin;

  if Period1End < Period2End
  then RsltEnd := Period1End
  else RsltEnd := Period2End;

  Result := RsltEnd >= RsltBegin;
end;

function IntersectPeriods(Period1Begin, Period1End, Period2Begin, Period2End: TDateTime): Boolean;
var IntersectBegin, IntersectEnd: TDateTime;
begin
  Result := false;
  if Period1Begin > Period2Begin
  then IntersectBegin := Period1Begin
  else IntersectBegin := Period2Begin;

  if Period1End < Period2End
  then IntersectEnd := Period1End
  else IntersectEnd := Period2End;

  Result := IntersectEnd >= IntersectBegin;
end;

function TryToEncodeDate(Year, Month, Day: Integer; var RsltDate: TDateTime): Boolean;
begin
  Result := false;

  if (Year >= 0) and (Month > 0) and (Day > 0) then
    Result := TryEncodeDate(Year, Month, Day, RsltDate);
end;

initialization

  {$IFDEF DCC} // Delphi XE2/XE3 Win 32/64
  DefaultFormatSettings := TFormatSettings.Create;
  {$ENDIF DCC}


end.
