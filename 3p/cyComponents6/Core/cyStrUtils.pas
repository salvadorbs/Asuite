{   Unit cyStrUtils

    Description:
    Unit with string functions.

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

unit cyStrUtils;

interface

uses
  {$IFDEF UNICODE}
  SysUtils, Character;
  {$ELSE}
  SysUtils;
  {$ENDIF}

type
  TStrLocateOption = (strloCaseInsensitive, strloPartialKey);
  TStrLocateOptions = set of TStrLocateOption;
  TStringRead = (srFromLeft, srFromRight);
  TStringReads = Set of TStringRead;
  TCaseSensitive = (csCaseSensitive, csCaseNotSensitive);
  TWordsOption = (woOnlyFirstWord, woOnlyFirstCar);
  TWordsOptions = Set of TWordsOption;
  TCarType = (ctAlphabeticUppercase, ctAlphabeticLowercase, ctNumeric, ctOther);
  TCarTypes = Set of TCarType;

{$IFDEF UNICODE}
  TUnicodeCategories = Set of TUnicodeCategory;
{$ENDIF}

const
  CarTypeAlphabetic = [ctAlphabeticUppercase, ctAlphabeticLowercase];

{$IFDEF UNICODE}
  CharCategoriesNumbers = [TUnicodeCategory.ucDecimalNumber];
  CharCategoriesLetters = [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucUppercaseLetter];
  CharCategoriesLetters_Space = [TUnicodeCategory.ucLowercaseLetter, TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucSpaceSeparator];
{$ENDIF}

// *** Char funtions ***//
function Char_GetType(aChar: Char): TCarType;

// *** SubString functions *** //
  { English: Substring definition:
    A string can be defined by substrings that are separated by a specified caracter.
    We admit that if a string = '', it has a single substring element with '' value.
    So, Substring count is 1 or superior.
    The first Substring element on a string has index 1, the second has index 2 and so on.

    Exemple (';' is the substring separator) :
    Exemple: Str = ''                 1 empty substring
    Exemple: Str = 'xxx'              1 substring with 'xxx' value
    Exemple: Str = ';'                2 empty substrings
    Exemple: Str = 'xxx;'             2 substrings ('xxx' and '')
    Exemple: Str = ';xxx'             2 substrings ('' and 'xxx')
    Exemple: Str = 'xxx;yyy'          2 substrings ('xxx' and 'yyy')
  }

  { Français: Definition d' une sous-chaine, appelé élément:
    Une chaine de caractères peut être définit de plusieurs éléments séparés par un caractère définit.
    On admet que si une string = '', celle-ci contient un seul élément appelé subString de valeur ''.
    Donc, le nombre d' éléments ne peut être inférieur à 1.
    Le 1er élément dans une string a pour Index 1, le second a pour Index 2 etc ...

    Exemple (lorsque ';' est le séparateurs des éléments) :
    Si Str = ''                 on a 1 élément vide
    Si Str = 'xxx'              on a 1 élément de valeur 'xxx'
    Si Str = ';'                on a 2 éléments vide
    Si Str = 'xxx;'             on a 2 éléments  de valeur 'xxx' et ''
    Si Str = ';xxx'             on a 2 éléments  de valeur '' et 'xxx'
    Si Str = 'xxx;yyy'          on a 2 éléments  de valeur 'xxx' et 'yyy'
  }

  function SubString_Count(Str: String; Separator: Char): Integer;
  // English: Retrieve the number of substring
  // Français: La fonction renvoie le nombre d' éléments dans Str

  function SubString_AtPos(Str: String; Separator: Char; SubStringIndex: Word): Integer;
  // English: Retrieve the substring (specified by its index) position on the string
  // Français: Renvoie la position du 1er caractère de l' élément d' indexe SubStringIndex

  function SubString_Get(Str: String; Separator: Char; SubStringIndex: Word): ShortString;
  // English: Retrieve substring element by index
  // Français: Renvoie l' élément d' indexe SubStringIndex

  function SubString_Length(Str: String; Separator: Char; SubStringIndex: Word): Integer;
  // English: Retrieve the number of caracters of the substring specified by SubStringIndex
  // Français: Renvoie le nombre de caractères de l' élément d' indexe SubStringIndex

  procedure SubString_Add(var Str: String; Separator: Char; Value: String);
  // English: Add a substring at the end. For the first substring, use str := 'Value'
  // Français: Ajoute un élément à la fin de la string. Pour le 1er élément, utilisez Str := 'exemple'

  procedure SubString_Insert(var Str: String; Separator: Char; SubStringIndex: Word; Value: String);
  // English: Insert a substring at SubStringIndex position
  // Français: Insère un élément à la position SubStringIndex

  procedure SubString_Edit(Var Str: String; Separator: Char; SubStringIndex: Word; NewValue: String);
  // English: Modify/initialize the specified substring at SubStringIndex position (don' t need to already exists)
  // Français: Modifie/initialise l' élément définit par SubStringIndex.

  function SubString_Remove(var Str: string; Separator: Char; SubStringIndex: Word): Boolean;
  // English: Delete the specified substring
  // Français: Élimine l' élément à l' index SubStringIndex

  function SubString_Locate(Str: string; Separator: Char; SubString: String; Options: TStrLocateOptions): Integer;
  // English: Retrieve SubString index
  // Français: Renvoie la position de l' élément spécifié

  function SubString_Ribbon(Str: string; Separator: Char; Current: Word; MoveBy: Integer): Integer; overload;
  // English: Retrieve substring index from a current substring and moving by MoveBy.
  // If there's no more substrings, it continues to move from the first one.
  // Français: Renvoie la position d' un élément par rapport à un autre en ce déplaceant de "MoveBy" éléments.
  // Si on se retrouve à la fin, on revient au début et vice versa.

  function SubString_Ribbon(Str: string; Separator: Char; Current: String; MoveBy: Integer): String; overload;
  // English: Like prior but SubString can be specified by its value
  // Français: comme la fonction antérieure, si ce n' est que l' élément est cette fois définit par sa valeur



// *** String functions *** //
  function String_Quote(Str: String): String;

  function String_GetCar(Str: String; Position: Word; ReturnCarIfNotExists: Char): Char;
  // English: Returns caracter at specified position
  // Français: Renvoie le caractère à la position "Position"

  function String_ExtractCars(fromStr: String; CarTypes: TCarTypes; IncludeCars, ExcludeCars: String): String; {$IFDEF UNICODE} overload; {$ENDIF}

  {$IFDEF UNICODE}
  function String_ExtractCars(Str: String; CharCategories: TUnicodeCategories): String; overload;
  // English: Returns caracters of specified categories (numbers, letters etc ...)
  // Français: Renvoie les types de caractères spécifié avec CharCategories (nombres, lettres etc ...)
  {$ENDIF}

  function String_GetWord(Str: String; StringRead: TStringRead): String;
  // English: Returns a string with any word found on the string
  // Français: Renvoie une string qui peut être convertie en type Word si le résultat de la fonction n' est pas vide

  function String_GetInteger(Str: String; StringRead: TStringRead): String;
  // English: Returns a string with any integer found on the string
  // Français: Renvoie une string qui peut être convertie en type Integer si le résultat de la fonction n' est pas vide

  function String_ToInt(Str: String): Integer;
  // English: Always convert a string to an integer, returns 0 if not valid integer
  // Français: Converti une String en Integer si possible, sinon renvoie 0

  function String_Uppercase(Str: String; Options: TWordsOptions) : String;
  // English: Uppercase string by Options
  // Français: Renvoie la string en majuscule selon Options (1ère lettre de string, 1ère lettre de chaque mot ou toute la string)

  function String_Lowercase(Str: String; Options: TWordsOptions) : String;
  // English: Lowercase string by Options
  // Français: Renvoie la string en minuscule selon Options (1ère lettre de string, 1ère lettre de chaque mot ou toute la string)

  function String_Reverse(Str: String): String;
  // English: Revert a string, String_Reverse('Hello ') returns 'olleH'
  // Français: Renvoie l' ordre des caratères inversé de la string

  function String_Pos(SubStr: String; Str: String; fromPos: Integer; CaseSensitive: TCaseSensitive): Integer; overload;
  // English: Retrieve subString position in string since fromPos position
// Français: Renvoie la position d' une substring depuis une certaine position

  function String_Pos(SubStr: String; Str: String; StringRead: TStringRead; Occurrence: Word; CaseSensitive: TCaseSensitive): Integer; overload;
  // English: Retrieve subString position
  // Français: Renvoie la position d' une substring selon son ocurrence

  function String_Copy(Str: String; fromIndex: Integer; toIndex: Integer): String; overload;
  // English: Copy caracters in a string from fromIndex to toIndex
  // Français: Renvoie la copie d' une string selon la position du 1er et dernier caractère

  function String_Copy(Str: String; StringRead: TStringRead; UntilFind: String; _Inclusive: Boolean): String; overload;
  // English: Copy caraters in a string until UntilFind found
  // Français: Renvoie la copie d' une string tant qu' elle ne trouve pas UntilFind

  function String_Copy(Str: String; Between1: String; Between1MustExist: Boolean; Between2: String; Between2MustExist: Boolean; CaseSensitive: TCaseSensitive): String; overload;
  // English: Retrieve caracters between 2 strings
  // Français: Renvoie les caractères d' uyne string entre 2 substrings.

  function String_Delete(Str: String; fromIndex: Integer; toIndex: Integer): String; overload;
  // English: Returns a string cutted from fromIndex to to Index
  // Français: Élimine les caractères d' une string entre la position du 1er et dernier caractère spécifié

  function String_Delete(Str: String; delStr: String; CaseSensitive: TCaseSensitive): String; overload;
  // English: Returns a string Removing substrings specified in delStr
  // Français: Renvoie la string après avoir retiré toute SubString specifiée avec delStr

  function String_BoundsCut(Str: String; CutCar: Char; Bounds: TStringReads): String;
  // English: Remove specified caracter from string bounds
  // Français: Permet de retirer un caractère tant que la string commence ou fini para celui-ci.

  function String_BoundsAdd(Str: String; AddCar: Char; ReturnLength: Integer): String;
  // English: Add specified caracter until string length is equal to ReturnLength
  // Français: Permet d' ajouter un caractère au début ou/et à la fin d' une string jusqu' à ce que la string se retrouve de la taille de "ReturnLength" caractères

  function String_Add(Str: String; StringRead: TStringRead; aCar: Char; ReturnLength: Integer) : String;
  // English: Add specified caracter at the beginning/end until string length is equal to ReturnLength
  // Français: Permet d' ajouter un caractère au début ou à la fin d' une string jusqu' à ce que la string se retrouve de la taille de "ReturnLength" caractères

  function String_End(Str: String; Cars: Word): String;
  // English: Get last string caracters
  // Français: Renvoie les derniers caractères d' une string

  function String_Subst(OldStr: String; NewStr: String; Str: String; CaseSensitive: TCaseSensitive = csCaseSensitive; AlwaysFindFromBeginning: Boolean = false): String;
  // English: Like StringReplace() but with AlwaysFindFromBeginning parameter
  // Français: Fonction identique à StringReplace.
  // Permet cependant de toujours remplacer en recherchant depuis le début de la string. Utile lorsque New est contenu par OldStr

  function String_SubstCar(Str: String; Old, New: Char): String;
  // English: Replace a caracter by another
  // Français: Remplace un caractère par un autre dans une string

  function String_Count(Str:String; SubStr:String; CaseSenSitive: TCaseSensitive) : Integer;
  // English: Retrieve the number of occurrences of SubStr
  // Français: Renvoie le nombre d' occurences d' une SubString

  function String_SameCars(Str1, Str2: String; StopCount_IfDiferent: Boolean; CaseSensitive: TCaseSensitive): Integer;
  // English: Retrieve number of identical caracters at the same position
  // Français: Compte le nombre de caractères identiques à la même position entre 2 strings

  function String_IsNumbers(Str: String) : Boolean;
  // English: Returns True if the string contains only numeric caracters
  // Français: Permet de savoir si la string ne possède que des caractères de type chiffre.

  function SearchPos(SubStr: String; Str: String; MaxErrors: Integer): Integer;
  // English: Search a string into another if diference tolerence
  // Français: Recherche une chaîne de caractères à l' intérieur d' une autre avec une tolérence de diferences

  function StringToCsvCell(aStr: String): String;


implementation

function Char_GetType(aChar: Char): TCarType;
begin
  Result := ctOther;
  if aChar in ['a'..'z', 'é', 'è', 'ê', 'ë', 'á', 'à', 'â', 'ä', 'ã', 'í', 'ì', 'î', 'ï', 'ó', 'ò', 'ô', 'ö', 'õ', 'ú', 'ù', 'û', 'ü'] then
    Result := ctAlphabeticLowercase
  else
    if aChar in ['A'..'Z', 'É', 'È', 'Ê', 'Ë', 'Á', 'À', 'Â', 'Ä', 'Ã', 'Í', 'Ì', 'Î', 'Ï', 'Ó', 'Ò', 'Ô', 'Ö', 'Õ', 'Ú', 'Ù', 'Û', 'Ü'] then
      Result := ctAlphabeticUppercase
    else
      if aChar in ['0'..'9'] then
        Result := ctNumeric;
end;

function SubString_Count(Str: String; Separator: Char): Integer;
var i, nbCars: Integer;
begin
  Result := 1;
  nbCars := length(Str);

  for i := 1 to nbCars do
    if Str[i] = Separator
    then Result := Result + 1;
end;

function SubString_AtPos(Str: String; Separator: Char; SubStringIndex: Word): Integer;
var nbCars, curSubStringIndex, i: Integer;
begin
  Result := 0;
  if SubStringIndex = 0 then raise ERangeError.Create('SubStringIndex = 0!');

  nbCars := length(Str);
  curSubStringIndex := 1;
  i := 0;

  while (curSubStringIndex <> SubStringIndex) and (i < nbCars) do
  begin
    i := i + 1;

    if Str[i] = Separator
    then curSubStringIndex := curSubStringIndex + 1;
  end;

  if curSubStringIndex = SubStringIndex
  then Result := i + 1;    // 'Sauter' le separateur ou i = 0...
end;

function SubString_Get(Str: String; Separator: Char; SubStringIndex: Word): ShortString;
var nbCars, i: Integer;
begin
  Result := '';
  i := SubString_AtPos(Str, Separator, SubStringIndex);

  if i <> 0      // Substring exists
  then begin
    nbCars := length(Str);

    while i <= nbCars do
    begin
      if Str[i] <> Separator
      then Result := Result + Str[i]
      else i := nbCars;

      i := i + 1;
    end;
  end;
end;

function SubString_Length(Str: String; Separator: Char; SubStringIndex: Word): Integer;
begin
  Result := Length(SubString_Get(Str, Separator, SubStringIndex));
end;

procedure SubString_Add(var Str: String; Separator: Char; Value: String);
begin
  Str := Str + Separator + Value;
end;

procedure SubString_Insert(var Str: String; Separator: Char; SubStringIndex: Word; Value: String);
var i, SubStrCount: Integer;
begin
  SubStrCount := SubString_Count(Str, Separator);

  if (SubStringIndex > 0) and (SubStringIndex <= SubStrCount)
  then begin
    i := SubString_AtPos(Str, Separator, SubStringIndex);
    Insert(Value + Separator, Str, i);
  end
  else
    raise ERangeError.CreateFmt('%d is not within the valid range of %d..%d', [SubStringIndex, 1, SubStrCount]);
end;

procedure SubString_Edit(var Str: string; Separator: Char; SubStringIndex: Word; NewValue: string);
var i, SubStrCount, nbCars: Integer;
begin
  i := SubString_AtPos(Str, Separator, SubStringIndex);

  if i = 0          // Substring does not exists ...
  then begin
    SubStrCount := SubString_Count(Str, Separator);

    while SubStrCount < SubStringIndex do
    begin
      Str := Str + Separator;          // Add empty substring
      SubStrCount := SubStrCount + 1;
    end;

    Str := Str + NewValue;
  end
  else begin
    nbCars := length(Str);
    // Remove current  value :
    while i <= nbCars do
      if Str[i] <> Separator
      then begin
        Delete(Str, i, 1);
        nbCars := nbCars - 1;
      end
      else
        nbCars := 0;   // End of the substring

    // Insert new substring value :
    Insert(NewValue, Str, i);
  end;
end;

function SubString_Remove(var Str: string; Separator: Char; SubStringIndex: Word): Boolean;
var i, nbCars: Integer;
begin
  Result := false;
  i := SubString_AtPos(Str, Separator, SubStringIndex);

  if i <> 0
  then begin
    Result := True;
    nbCars := length(Str);
    // Remove current value :
    while i <= nbCars do
    begin
      if Str[i] = Separator
      then nbCars := 0             // end of the substring
      else nbCars := nbCars - 1;

      Delete(Str, i, 1);
    end;
  end;
end;

function SubString_Locate(Str: string; Separator: Char; SubString: String; Options: TStrLocateOptions): Integer;
var
  SubStrCount, CurSubStrIndex: Integer;
  FindPartialKey, FindCaseInsensitive: Boolean;
begin
  Result := 0;
  CurSubStrIndex := 1;
  SubStrCount := SubString_Count(Str, Separator);
  FindPartialKey := strloPartialKey in Options;
  FindCaseInsensitive := strloCaseInsensitive in Options;

  if FindCaseInsensitive
  then begin
    Str := AnsiUpperCase(Str);
    SubString := AnsiUpperCase(SubString);
  end;

  while (Result = 0) and (CurSubStrIndex <= SubStrCount) do
  begin
    if FindPartialKey
    then begin
      if Pos(SubString, SubString_Get(Str, Separator, CurSubStrIndex)) = 1
      then Result := CurSubStrIndex;
    end
    else
      if SubString = SubString_Get(Str, Separator, CurSubStrIndex)
      then Result := CurSubStrIndex;

    inc(CurSubStrIndex, 1);
  end;
end;

function SubString_Ribbon(Str: string; Separator: Char; Current: Word; MoveBy: Integer): Integer;
var Count: Integer;
begin
  Count := SubString_Count(Str, Separator);

  Result := Current + MoveBy;

  if Result > 0
  then begin
    while Result > Count do
      Result := Result - Count;
  end
  else begin
    while Result <= 0 do
      Result := Result + Count;
  end;
end;

function SubString_Ribbon(Str: string; Separator: Char; Current: String; MoveBy: Integer): String;
var SubStringIndex: Integer;
begin
  SubStringIndex := SUBSTRING_LOCATE(Str, Separator, Current, []);

  if SubStringIndex = 0
  then SubStringIndex := 1
  else SubStringIndex := SubString_Ribbon(Str, Separator, SubStringIndex, MoveBy);

  Result := SUBSTRING_GET(Str, Separator, SubStringIndex);
end;

function String_Quote(Str: String): String;
begin
  Result := '''' + Str + '''';
end;

function String_ExtractCars(fromStr: String; CarTypes: TCarTypes; IncludeCars, ExcludeCars: String): String;

        function IncludeCar(aCar: Char): Boolean;
        var c: Integer;
        begin
          Result := false;
          for c := 1 to length(IncludeCars) do
            if IncludeCars[c] = aCar then
            begin
              Result := true;
              Break;
            end;
        end;

        function ExcludeCar(aCar: Char): Boolean;
        var c: Integer;
        begin
          Result := false;
          for c := 1 to length(ExcludeCars) do
            if ExcludeCars[c] = aCar then
            begin
              Result := true;
              Break;
            end;
        end;


var
  i: Integer;
  AddCar: Boolean;
begin
  Result := '';

  for i := 1 to length(fromStr) do
  begin
    AddCar := false;

    if IncludeCar(fromStr[i]) then
      AddCar := true
    else
      if not ExcludeCar(fromStr[i]) then
        if Char_GetType(fromStr[i]) in CarTypes then
          AddCar := true;

    if AddCar then
      Result := Result + fromStr[i];
  end;
end;

{$IFDEF UNICODE}
function String_ExtractCars(Str: String; CharCategories: TUnicodeCategories): String;
var
  I: Integer;
  UnicodeCategory: TUnicodeCategory;
begin
  Result := '';

  for i := 1 to Length(Str) do
  begin
    UnicodeCategory := GetUnicodeCategory(Str, I);
    if UnicodeCategory in CharCategories then
      Result := Result + Str[I];
  end;
end;
{$ENDIF}

function String_GetCar(Str: String; Position: Word; ReturnCarIfNotExists: Char): Char;
begin
  if Position in [1..Length(Str)] // Returns false if Length(Str) = 0
  then Result := Str[Position]
  else Result := ReturnCarIfNotExists;
end;

function String_ToInt(Str: String): Integer;
begin
  if not TryStrToInt(Str, Result)
  then Result := 0;
end;

function String_Uppercase(Str: String; Options: TWordsOptions) : String;
var
  i: Integer;
  b: Boolean;
begin
  if Str = ''
  then
    Result := ''
  else
    if woOnlyFirstCar in Options     // Word first letter
    then begin
      Str := AnsiLowerCase(Str);

      if woOnlyFirstWord in Options
      then
        Result := AnsiUpperCase(Str[1]) + Copy(Str, 2, Length(Str) -1)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then Result := Result + AnsiUpperCase(Str[i])
          else Result := Result + Str[i];

          b := Str[i] = ' ';
        end;
      end;
    end
    else begin                       // All word
      Str := AnsiLowerCase(Str);

      if not (woOnlyFirstWord in Options)
      then
        Result := AnsiUpperCase(Str)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then begin
            Result := Result + AnsiUpperCase(Str[i]);
            b := Str[i] <> ' ';
          end
          else
            Result := Result + Str[i];
        end;
      end
    end;
end;

function String_Lowercase(Str : String; Options: TWordsOptions): String;
var
  i: Integer;
  b: Boolean;
begin
  if Str = ''
  then
    Result := ''
  else
    if woOnlyFirstCar in Options     // Word first letter
    then begin
      if woOnlyFirstWord in Options
      then
        Result := AnsiLowerCase(Str[1]) + Copy(Str, 2, Length(Str) -1)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then Result := Result + AnsiLowerCase(Str[i])
          else Result := Result + Str[i];

          b := Str[i] = ' ';
        end;
      end;
    end
    else begin                       // All word
      if not (woOnlyFirstWord in Options)
      then
        Result := AnsiLowerCase(Str)
      else begin
        b := True;
        Result := '';

        for i := 1 to Length(Str) do
        begin
          if b
          then begin
            Result := Result + AnsiLowerCase(Str[i]);
            b := Str[i] <> ' ';
          end
          else
            Result := Result + Str[i];
        end;
      end
    end;
end;

function String_Reverse(Str: String): String;
var i: Integer;
begin
  Result := '';

  for i := 1 to Length(Str) do
    Result := Str[i] + Result;
end;

function String_Pos(SubStr: String; Str: String; fromPos: Integer; CaseSensitive: TCaseSensitive): Integer;
begin
  if fromPos < 1 then fromPos := 1;   // From first char ...

  if CaseSensitive = csCaseNotSensitive
  then begin
    SubStr := AnsiUpperCase(SubStr);
    Str := AnsiUpperCase(Str);
  end;

  // Remove the beginning :
  Delete(Str, 1, fromPos - 1);

  // Get "relative" position :
  Result := pos(SubStr, Str);

  // Get absolute position :
  if Result <> 0
  then Inc(Result, fromPos - 1);
end;

function String_Pos(SubStr: String; Str: String; StringRead: TStringRead; Occurrence: Word; CaseSensitive: TCaseSensitive): Integer;
var CurCar, LengthStr, LengthSubStr, FoundCount: Integer;
begin
  Result := 0;

  if CaseSensitive = csCaseNotSensitive
  then begin
    Str  := AnsiUpperCase(Str);
    SubStr := AnsiUpperCase(SubStr);
  end;

  if (Occurrence > 0) and (SubStr <> '')
  then begin
    if StringRead = srFromRight
    then begin
      SubStr := String_Reverse(SubStr);
      Str  := String_Reverse(Str);
    end;

    FoundCount   := 0;
    CurCar       := 1;
    LengthStr    := Length(Str);
    LengthSubStr := Length(SubStr);

    while CurCar <= LengthStr do
    begin
      if Copy(Str, CurCar, LengthSubStr) = SubStr
      then begin
        FoundCount := FoundCount + 1;

        if FoundCount = Occurrence
        then begin
          if StringRead = srFromLeft
          then Result := CurCar
          else Result := LengthStr - CurCar + (2 - LengthSubStr);   // Calc correct position because of String reverse()

          CurCar := LengthStr;
        end
        else
          CurCar := CurCar + LengthSubStr;
      end
      else
        CurCar := CurCar + 1;
    end;
  end;
end;

function String_Copy(Str: String; fromIndex: Integer; toIndex: Integer): String;
begin
  Result := Copy(Str, fromIndex, (toIndex - fromIndex + 1));
end;

function String_Copy(Str: String; StringRead: TStringRead; UntilFind: String; _Inclusive: Boolean): String;
var x: Integer;
begin
  Result := '';

  if Pos(UntilFind, Str) > 0
  then
    if StringRead = srFromLeft
    then begin
      x := Pos(UntilFind, Str);
      if _Inclusive
      then Inc(x, Length(UntilFind));
      Result := Copy(Str, 1, x - 1);
    end
    else begin
      x := String_Pos(UntilFind, Str, srFromRight, 1, csCaseSensitive);
      if not _Inclusive
      then Inc(x, Length(UntilFind));
      Result := Copy(Str, x, Length(Str));
    end;
end;

function String_Copy(Str: String; Between1: String; Between1MustExist: Boolean; Between2: String; Between2MustExist: Boolean; CaseSensitive: TCaseSensitive): String;
var
  posStr1, posStr2, StartPos, EndPos: Integer;
  WorkingStr: String;
begin
  Result := '';

  if Str <> ''
  then begin
    if CaseSensitive = csCaseNotSensitive
    then begin
      Between1 := AnsiUpperCase(Between1);
      Between2 := AnsiUpperCase(Between2);
      WorkingStr := AnsiUpperCase(Str);
    end
    else
      WorkingStr := Str;

    StartPos := 0;
    EndPos   := 0;
    posStr1  := pos(Between1, WorkingStr);

    if Between1 = Between2    // Locate the 2nd occurrence :
    then posStr2 := String_Pos(Between2, WorkingStr, srFromLeft, 2, csCaseNotSensitive)
    else posStr2 := String_Pos(Between2, WorkingStr, posStr1 + length(Between1), csCaseNotSensitive);

    if posStr1 = 0
    then begin
      if not Between1MustExist
      then StartPos := 1;
    end
    else
      StartPos := posStr1 + length(Between1);

    if posStr2 = 0
    then begin
      if not Between2MustExist
      then EndPos := Length(Str);
    end
    else
      EndPos := posStr2 - 1;

    if (StartPos <> 0) and (EndPos <> 0)
    then Result := String_Copy(Str, StartPos, EndPos);
  end;
end;

function String_BoundsCut(Str: String; CutCar: Char; Bounds: TStringReads): String;
var
  Cont: Boolean;
  NbCars: Integer;
begin
  if srFromLeft in Bounds
  then begin
    Cont := true;
    NbCars := Length(Str);
    while (Cont) and (NbCars > 0) do
      if Str[1] = CutCar
      then begin
        Delete(Str, 1, 1);
        NbCars := nbCars - 1;
      end
      else
        Cont := false;
  end;

  if srFromRight in Bounds
  then begin
    Cont := true;
    NbCars := Length(Str);
    while (Cont) and (NbCars > 0) do
      if Str[NbCars] = CutCar
      then begin
        Delete(Str, NbCars, 1);
        NbCars := nbCars - 1;
      end
      else
        Cont := false;
  end;

  Result := Str;
end;

function String_BoundsAdd(Str: String; AddCar: Char; ReturnLength: Integer): String;
var
  Orig, i, AddCarCount: Integer;
  ToTheRight: Boolean;
begin
  ToTheRight := True;
  Orig      := Length(Str);
  AddCarCount    := ReturnLength - Orig;
  Result    := Str;

  for i := 1 to AddCarCount do
  begin
    if ToTheRight
    then Result := Result + AddCar
    else Result := AddCar + Result;

    ToTheRight := not ToTheRight;
  end;
end;

function String_GetWord(Str: String; StringRead: TStringRead): String;
var
  I: Integer;
  Cont: Boolean;
begin
  Cont   := True;
  Result := '';

  if StringRead = srFromLeft
  then begin
     for i := 1 to Length(Str) do
       if (Cont) and (Str[i] in ['0'..'9'])
       then Result := Result + Str[i]
       else Cont := Result = '';
  end
  else begin
     for i := Length(Str) downto 1 do
       if (Cont) and (Str[i] in ['0'..'9'])
       then Result := Str[i] + Result
       else Cont := Result = '';
  end;
end;

function String_GetInteger(Str: String; StringRead: TStringRead): String;
var
  I: Integer;
  Cont: Boolean;
begin
  Cont   := True;
  Result := '';

  if StringRead = srFromLeft
  then begin
     for i := 1 to Length(Str) do
       if (Cont) and (Str[i] in ['-', '0'..'9'])
       then begin
         if Str[i] = '-'
         then begin
           if (Result = '') and (i < length(Str))
           then begin
             if Str[i+1] in ['0'..'9']  // Next car is a number !!!
             then Result := '-';
           end
           else
             Cont := false;
         end
         else
           Result := Result + Str[i];
       end
       else
         Cont := Result = '';
  end
  else begin
     for i := Length(Str) downto 1 do
       if (Cont) and (Str[i] in ['-', '0'..'9'])
       then begin
         if Str[i] = '-'
         then begin
           if Result <> ''
           then begin
             Result := '-' + Result;
             Cont := false;
           end;
         end
         else
           Result := Str[i] + Result;
       end
       else
         Cont := Result = '';
  end;
end;

function String_Add(Str: String; StringRead: TStringRead; aCar: Char; ReturnLength: Integer) : String;
var NbCars: Integer;
begin
  NbCars := Length(Str);

  while NbCars < ReturnLength do
  begin
    if StringRead = srFromRight
    then Str := Str + aCar
    else Str := aCar + Str;

    NbCars := NbCars + 1;
  end;

  Result := Str;
end;

function String_End(Str: String; Cars: Word): String;
begin
  Result := Copy(Str, Length(Str) - Cars + 1, Cars);
end;

function String_Delete(Str: String; fromIndex: Integer; toIndex: Integer): String;
begin
  Result := Copy(Str, 1, fromIndex - 1) + Copy(Str, toIndex + 1, Length(Str) - toIndex);
end;

function String_Delete(Str: String; delStr: String; CaseSensitive: TCaseSensitive): String;
begin
  Result := String_Subst(delStr, '', Str, CaseSensitive, false);
end;

function String_Subst(OldStr: String; NewStr: String; Str: String; CaseSensitive: TCaseSensitive = csCaseSensitive; AlwaysFindFromBeginning: Boolean = false): String;
var AnsiUpperCaseNewStr: String;
    LengthStr, LengthOldStr, lengthNewStr: Integer;
    SearchUntilCar, i, f: Integer;
    Match: Boolean;
begin
  Result := Str;
  if OldStr = '' then Exit;
  if OldStr = NewStr then Exit;

  LengthStr := Length(Str);
  LengthOldStr := Length(OldStr);
  lengthNewStr := Length(NewStr);

  if CaseSensitive = csCaseNotSensitive then
  begin
    Str := AnsiUpperCase(Str);
    OldStr := AnsiUpperCase(OldStr);

    AnsiUpperCaseNewStr := AnsiUpperCase(NewStr);

    if AlwaysFindFromBeginning then
      AlwaysFindFromBeginning := Pos(OldStr, AnsiUpperCaseNewStr) = 0;
  end
  else
    if AlwaysFindFromBeginning then
      AlwaysFindFromBeginning := Pos(OldStr, NewStr) = 0;


  i := 1;
  SearchUntilCar := (LengthStr - LengthOldStr) + 1;
  while i <= SearchUntilCar do
  begin
    if Str[i] = OldStr[1] then
    begin
      Match := true;
      for f := 2 to LengthOldStr do     // Search OldStr into Str ...
        if Str[f+i-1] <> OldStr[f] then
        begin
          Match := false;
          Break;
        end;

      if Match then
      begin
        // Replace into Result and Str :
        Delete(Result, i, LengthOldStr);
        Delete(Str, i, LengthOldStr);

        if lengthNewStr <> 0 then
        begin
          Insert(NewStr, Result, i);

          if CaseSensitive = csCaseNotSensitive
          then Insert(AnsiUpperCaseNewStr, Str, i)
          else Insert(NewStr, Str, i);
        end;

        SearchUntilCar := SearchUntilCar + lengthNewStr - LengthOldStr;

        if AlwaysFindFromBeginning
        then i := i - LengthOldStr       // Go back
        else i := i + lengthNewStr - 1;  // Need to put cursor on last replaced char

        if i < 0 then i := 0;
      end;
    end;

    inc(i);
  end;
end;

{ Old
function String_Subst(OldStr: String; New: String; Str: String; CaseSensitive: TCaseSensitive = csCaseSensitive; AlwaysFindFromBeginning: Boolean = true): String;
var Tex, Ant : String;
    FoundPos  : Integer;
    NewContemOldStr: Boolean;
begin
  if CaseSensitive = csCaseSensitive
  then begin
    Tex := Str;
    Ant := OldStr;
    NewContemOldStr := Pos(OldStr, New) > 0;
  end
  else begin
    Tex := AnsiUpperCase(Str);
    Ant := AnsiUpperCase(OldStr);
    NewContemOldStr := Pos(Ant, AnsiUpperCase(New)) > 0;
  end;

  FoundPos := Pos(Ant, Tex);

  if (NewContemOldStr) or (AlwaysFindFromBeginning)
  then begin
    // *** New string contains oldStr and AlwaysFindFromBeginning defined *** //
    Result := '';
    while FoundPos > 0 do
    begin
      // Passar o que está antes da posição + New:
      Result := Result + Copy(Str, 1, FoundPos - 1) + New;

      // Remover até a posição de Ant, remover também Ant :
      Delete(Str, 1, FoundPos + length(Ant) - 1);
      Delete(Tex,   1, FoundPos + length(Ant) - 1);

      FoundPos := Pos(Ant, Tex);
    end;

    Result := Result + Str;   // Inserir o que sobrar ...
  end
  else begin
    // *** New string does not contains OldStr *** //
    while FoundPos > 0 do
    begin
      Delete(Str, FoundPos, Length(OldStr)); // Remove OldStr ...
      Insert(New, Str, FoundPos);           // Insert New ...

      if CaseSensitive = csCaseNotSensitive
      then Tex := AnsiUpperCase(Str)
      else Tex := Str;

      FoundPos := Pos(Ant, Tex);             // Find next ...
    end;

    Result := Str;
  end;
end;   }

function String_SubstCar(Str: String; Old, New: Char): String;
var
  LengthStr, i: Integer;
begin
  Result := Str;
  LengthStr := length(Str);

  for i := 1 to LengthStr do
    if Result[i] = Old then
      Result[i] := New;
end;

function String_Count(Str:String; SubStr:String; CaseSensitive: TCaseSensitive) : Integer;
var i, j, L_Str, L_SubStr: Integer;

   function SubStr_Na_Pos(_P: Integer): Boolean;
   begin
     Result := Copy(Str, _P, L_SubStr) = SubStr;
   end;

begin
  Result := 0;

  if SubStr <> ''
  then begin
    if CaseSensitive = csCaseNotSensitive
    then begin
      Str  := AnsiUpperCase(Str);
      SubStr := AnsiUpperCase(SubStr);
    end;

    L_Str    := Length(Str);
    L_SubStr := Length(SubStr);
    i := 1;

    while i <= L_Str do
       if SubStr_Na_Pos(i)
       then begin
         Result := Result + 1;
         i := i + L_SubStr;
       end
       else
         i := i + 1;
  end;
end;

function String_SameCars(Str1, Str2: String; StopCount_IfDiferent: Boolean; CaseSensitive: TCaseSensitive): Integer;
var i, MaxCars: Integer;
begin
  Result := 0;

  if CaseSensitive = csCaseNotSensitive
  then begin
    Str1 := AnsiUpperCase(Str1);
    Str2 := AnsiUpperCase(Str2);
  end;

  if Length(Str1) > Length(Str2)
  then MaxCars := Length(Str2)
  else MaxCars := Length(Str1);

  for i := 1 to MaxCars do
    if Str1[i] = Str2[i]
    then
      Result := Result + 1
    else
      if StopCount_IfDiferent
      then Break;
end;

function String_IsNumbers(Str: String) : Boolean;
var i: Integer;
begin
  Result := true;

  if Str <> ''
  then begin
    for i := 1 to Length(Str) do
      if not (Str[i] in ['0'..'9'])
      then Result := false;
  end
  else
    Result := false;
end;

function SearchPos(SubStr: String; Str: String; MaxErrors: Integer): Integer;
var
  i, p, LengthSubStr: Integer;
  ErrorCount: Integer;
begin
  Result := 0;
  LengthSubStr := Length(SubStr);

  // Navigate on Str searching for SubStr :
  for i := 1 to (Length(Str) - LengthSubStr) + 1 do
  begin
    ErrorCount := 0;

    // Compare all SubStr chars :
    for p := 1 to LengthSubStr do
      if SubStr[p] <> Str[i + p - 1] then
      begin
        Inc(ErrorCount);
        if ErrorCount > MaxErrors then
          Break;
      end;

    if ErrorCount <= MaxErrors then
    begin
      Result := i;
      MaxErrors := ErrorCount-1; // Try to locate with less errors ...
    end;
  end;
end;

function StringToCsvCell(aStr: String): String;
var AddQuote: Boolean;
begin
  Result := aStr;
  AddQuote := pos(';', aStr) <> 0;
  if not AddQuote then AddQuote := pos('"', aStr) <> 0;

  Result := String_Subst('"', '""', Result, csCaseSensitive, false);

  if AddQuote then
    Result := '"' + Result + '"';
end;

end.
