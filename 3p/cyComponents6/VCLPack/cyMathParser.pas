{ The Initial Developer is "Barbichette" and of the Original Code
  can be found here: http://www.delphifr.com//code.aspx?ID=45846

  His work was inspired from an "Oniria" source that can be found here: http://www.delphifr.com/codes/CALCULATRICE-CHAIN%20ES-MATHEMATIQUES_45537.aspx



  The method used to parse is called "reverse Polish notation" :
  http://en.wikipedia.org/wiki/Reverse_Polish_notation

  From Wikipedia:
  "Reverse Polish notation (or RPN) is a mathematical notation wherein every operator follows all of its operands, in contrast to Polish notation,
  which puts the operator in the prefix position. It is also known as Postfix notation and is parenthesis-free as long as operator arities are fixed.
  The description "Polish" refers to the nationality of logician Jan ?ukasiewicz, who invented (prefix) Polish notation in the 1920s."

  Exemple:

  Infix notation |       RPN
  -------------------------------
      2+2*3      |    2 2 3 * +
      (2+2)*3    |    2 2 + 3 *
      2*2+3      |    2 2 x 3 +
}

{   Component(s):
    tcyMathParser

    Description:
    Mathematical parser component that can evaluate an expression using common operators, some functions and variables

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

unit cyMathParser;

interface

uses classes, windows, Math, SysUtils, StrUtils;

const
  cNoError = 0;
  // Internal error:
  cInternalError = 1;
  // Expression errors:
  cErrorInvalidCar = 2;
  cErrorUnknownName = 3;
  cErrorInvalidFloat = 4;
  cErrorOperator = 5;
  cErrorFxNeedLeftParentese = 6;
  cErrorNotEnoughArgs = 7;
  cErrorSeparatorNeedArgument = 8;
  cErrorMissingLeftParenteses = 9;
  cErrorMissingRightParenteses = 10;
  cErrorLeftParentese = 11;
  cErrorRightParentese = 12;
  cErrorSeparator = 13;
  cErrorOperatorNeedArgument = 14;
  // Calc errors:
  cCalcError = 100;    // From here, this is a calculation error ...
  cErrorDivByZero = 101;
  cErrorPower = 102;
  cErrorFxInvalidValue = 103;

type
  TLognFirstArg = (lognBase, lognNumber);

  TTypeStack = (tsNotDef, tsValue, tsOperator, tsFunction, tsLeftParenthese, tsRightParenthese, tsSeparator, tsVariable);

  // Operation identifier:
  TOperationID = (opNone,
           // Operators
           OpPower, OpMultiply, OpDivide, OpAdd, OpSubstract, OpMod, OpNeg,
           // Functions
           OpCos, OpSin, OpTan, OpLog, OpLn, OpASin, OpACos, OpATan, OpExp, OpSqrt, OpSqr, OpLogN,OpInt,OpFrac,OpAbs,
           OpCeil, OpFloor, OpLdexp, OpLnXP1, OpMax, OpMin, OpRoundTo, OpSign, OpSum, OpCustom);

  // Operators and functions information :
  TOperationInfo = record
    Priority: byte;
    Arguments: Integer;
    Name: String;
    ID: TOperationID;
  end;

  TStackInfo = record
    Value: Extended;
    OperationID: TOperationID;
    TypeStack: TTypeStack;
    Name: string;             // Variable, operator or function name
    ArgumentsCount: Integer;  // Number of arguments for functions with no fixed arguments
  end;

  TStack = class
  private
    fCount: Integer;
  protected
    fList: array of TStackInfo;
    function GetStackInfo(Index: Integer): TStackInfo;
    procedure SetStackInfo(Index: Integer; StackInfo: TStackInfo);
  public
    constructor create;
    property Count: Integer read fCount;
    property StackInfo[Index: Integer]: TStackInfo read GetStackInfo write SetStackInfo; default;
    function Add(StackInfo: TStackInfo): Integer;
    function Insert(StackInfo: TStackInfo; Index: Integer): Integer;
    procedure Delete(Index: Integer);
    function DeleteLast: TStackInfo;
    function Last: TStackInfo;
    procedure Clear;
  end;

  function ReturnTStackInfo(Value: Extended; TypeStack: TTypeStack; OperationID: TOperationID = opNone; Name: string = ''): TStackInfo;


type
  TVariables = class
  private
    fCount: Integer;
    fNames: Array of string;
    fValues: Array of Extended;
    function Add(Name: String; Value: Extended): Integer;
    procedure Delete(Name: String);
  protected
    procedure Clear;
    function GetName(Index: Integer): String;
    function GetValue(Index: Integer): Extended; overload;
  public
    constructor create;
    property Count: Integer read fCount;
    property Names[Index: Integer]: String read GetName;
    property Values[Index: Integer]: Extended read GetValue;
    function GetIndex(Name: String): Integer;
    function GetValue(Name: String; var Value: Extended): boolean; overload;
    procedure SetValue(Name: String; Value: Extended);
  end;

  TProcOnCustomOperationParse = procedure (Sender: TObject; Name: String; const Arguments: Array of Extended; var Rslt: Extended) of object;

  TcyMathParser = class(TComponent)
  private
    // Operators and functions rules and specifications :
    OperationsInfo: array of TOperationInfo;
    //
    fLastError: integer;
    fLastErrorBeforeParse: Integer;
    fResultStack: TStack;
    fPrefixStack: TStack;
    fExpression: String;
    fOnCustomOperationParse: TProcOnCustomOperationParse;
    FLognFirstArg: TLognFirstArg;
    procedure InfixToPreFix(infix: TStack; Prefix: TStack);
    procedure StrToInfixStack(aExpression: String; aStack: TStack);
    function ValidateInfixStack(aStack: TStack):Boolean;
    function GetParserResult: Extended;
    procedure SetExpression(const Value: String);
    function GetOperation(Index: Integer): TOperationInfo;
    function GetOperationCount: Integer;
  protected
  public
    Variables: TVariables;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Parse: Boolean;
    function GetLastError: Integer;
    function GetLastErrorString: String;
    function ValidVariableName(Name: string): Boolean;
    function GetOperationIndex(OperationName: String): Integer;
    function GetOperationInfo(OperationName: string): TOperationInfo;
    function RenameOperation(CurrentName, NewName: String): Boolean;
    function AddCustomOperation(Name: String; Arguments: Integer; Priority: Byte = 0): Boolean;
    property Expression: String read fExpression write SetExpression;
    property ParserResult: Extended read GetParserResult;
    property Operations[Index: Integer]: TOperationInfo read GetOperation;
    property OperationsCount: Integer read GetOperationCount;
  published
    property LognFirstArg: TLognFirstArg read FLognFirstArg write FLognFirstArg default lognBase;
    property OnCustomOperationParse: TProcOnCustomOperationParse read fOnCustomOperationParse write fOnCustomOperationParse;
  end;

function GetErrorString(ErrorCode: integer): String;

implementation

function ReturnTStackInfo(Value: Extended; TypeStack: TTypeStack; OperationID: TOperationID = opNone; Name: string = ''): TStackInfo;
begin
  Result.Value := Value;
  Result.OperationID := OperationID;
  Result.TypeStack := TypeStack;
  Result.Name := Name;
end;

constructor TStack.Create;
begin
  inherited;
  fCount := 0;
  SetLength(fList, 0);
end;

function TStack.GetStackInfo(Index: Integer): TStackInfo;
begin
  Result := fList[Index];
end;

procedure TStack.SetStackInfo(Index: Integer; StackInfo: TStackInfo);
begin
  fList[Index] := StackInfo;
end;

function TStack.Add(StackInfo: TStackInfo): Integer;
begin
  inc(fCount);
  Setlength(fList, fCount);
  fList[fCount - 1] := StackInfo;
  Result := fCount - 1;
end;

function TStack.Insert(StackInfo: TStackInfo; Index: Integer): Integer;
var i: Integer;
begin
  if Index > fCount then Index := fCount;
  if Index < 0 then Index := 0;

  Setlength(fList, fCount + 1);
  i:= fCount - 1;

  while i >= Index do
  begin
    fList[i+1] := fList[i];
    dec(i);
  end;
  fList[Index] := StackInfo;
  inc(fCount);
  Result := Index;
end;

procedure TStack.Delete(Index: Integer);
begin
  dec(fCount);
  while Index < fCount do
  begin
    fList[Index] := fList[Index+1];
    inc(Index);
  end;
  Setlength(fList, fCount);
end;

function TStack.DeleteLast: TStackInfo;
begin
  Result := fList[fCount-1];
  Delete(fCount-1);
end;

function TStack.Last: TStackInfo;
begin
  Result := fList[fCount-1];
end;

procedure TStack.Clear;
begin
  fCount := 0;
  Setlength(fList, 0);
end;

constructor TVariables.Create;
begin
  Clear;
end;

procedure TVariables.Clear;
begin
  fCount := 0;
  setlength(fNames, 0);
  setlength(fValues, 0);
end;

function TVariables.GetIndex(Name: String): Integer;
var i: Integer;
begin
  Result := -1;
  Name := AnsiLowerCase(Name);

  for i:= 0 to fCount - 1 do
    if fNames[i] = Name
    then begin
      Result := i;
      exit;
    end;
end;

function TVariables.GetValue(Name: string; var Value: Extended): Boolean;
var i: Integer;
begin
  Result:= false;
  Name := AnsiLowerCase(Name);
  i := GetIndex(Name);

  if i<> -1
  then begin
    Value := fValues[i];
    Result := True;
  end;
end;

procedure TVariables.SetValue(Name: String; Value: Extended);
var
 i: Integer;
begin
  Name := AnsiLowerCase(Name);
  i := GetIndex(Name);

  if i = -1
  then Add(Name, Value)
  else fValues[i] := value;
end;

function TVariables.Add(Name: String; Value: Extended): Integer;
begin
  Name := AnsiLowerCase(Name);
  Result := GetIndex(Name);

  if Result = -1
  then begin
    inc(fCount);
    setlength(fNames, fCount);
    setlength(fValues, fCount);
    fNames[fCount-1] := Name;
    fValues[fCount-1] := Value;
    Result := fCount-1;
  end;
end;

procedure TVariables.Delete(Name: String);
var
 i: Integer;
begin
  Name := AnsiLowerCase(Name);
  i := GetIndex(Name);
  if i = -1 then exit;
  dec(fCount);
  move(fNames[i + 1], fNames[i], (fCount - i - 1) * sizeof(string));
  move(fValues[i + 1], fValues[i], (fCount-i-1) * sizeof(extended));
  setlength(fNames, fCount);
  setlength(fValues, fCount);
end;

function TVariables.GetName(Index: Integer): String;
begin
  Result := fNames[Index];
end;

function TVariables.GetValue(Index: Integer): Extended;
begin
  Result := fValues[Index];
end;

constructor TcyMathParser.Create(AOwner: TComponent);
begin
  inherited;
  fLognFirstArg := lognBase;  // Same as Delphi Math unit
  fExpression := '';
  fLastError := cNoError;
  fLastErrorBeforeParse := cNoError;
  fResultStack := TStack.Create;
  fPrefixStack := TStack.Create;
  Variables := TVariables.Create;
  //Variables.Add('pi', 3.1415926535897932385);

  SetLength(OperationsInfo, 32);

   // None
  OperationsInfo[ 0].Priority := 0; OperationsInfo[ 0].Arguments :=  0; OperationsInfo[ 0].Name := '';        OperationsInfo[ 0].ID := opNone;      // opNone
   // Operators
  OperationsInfo[ 1].Priority := 3; OperationsInfo[ 1].Arguments :=  2; OperationsInfo[ 1].Name := '^';       OperationsInfo[ 1].ID := OpPower;     // OpPower
  OperationsInfo[ 2].Priority := 2; OperationsInfo[ 2].Arguments :=  2; OperationsInfo[ 2].Name := '*';       OperationsInfo[ 2].ID := OpMultiply;  // OpMultiply
  OperationsInfo[ 3].Priority := 2; OperationsInfo[ 3].Arguments :=  2; OperationsInfo[ 3].Name := '/';       OperationsInfo[ 3].ID := OpDivide;    // OpDivide
  OperationsInfo[ 4].Priority := 1; OperationsInfo[ 4].Arguments :=  2; OperationsInfo[ 4].Name := '+';       OperationsInfo[ 4].ID := OpAdd;       // OpAdd
  OperationsInfo[ 5].Priority := 1; OperationsInfo[ 5].Arguments :=  2; OperationsInfo[ 5].Name := '-';       OperationsInfo[ 5].ID := OpSubstract; // OpSubstract
  OperationsInfo[ 6].Priority := 2; OperationsInfo[ 6].Arguments :=  2; OperationsInfo[ 6].Name := '%';       OperationsInfo[ 6].ID := OpMod;       // OpMod (5 mod 3 = 2)
  // Internal functions
  OperationsInfo[ 7].Priority := 4; OperationsInfo[ 7].Arguments :=  1; OperationsInfo[ 7].Name := 'neg';     OperationsInfo[ 7].ID := OpNeg;       // OpNeg (negative value, used for diferenciate with substract operator)
  // Functions
  OperationsInfo[ 8].Priority := 0; OperationsInfo[ 8].Arguments :=  1; OperationsInfo[ 8].Name := 'cos';     OperationsInfo[ 8].ID := OpCos;       // OpCos
  OperationsInfo[ 9].Priority := 0; OperationsInfo[ 9].Arguments :=  1; OperationsInfo[ 9].Name := 'sin';     OperationsInfo[ 9].ID := OpSin;       // OpSin
  OperationsInfo[10].Priority := 0; OperationsInfo[10].Arguments :=  1; OperationsInfo[10].Name := 'tan';     OperationsInfo[10].ID := OpTan;       // OpTan
  OperationsInfo[11].Priority := 0; OperationsInfo[11].Arguments :=  1; OperationsInfo[11].Name := 'log';     OperationsInfo[11].ID := OpLog;       // OpLog
  OperationsInfo[12].Priority := 0; OperationsInfo[12].Arguments :=  1; OperationsInfo[12].Name := 'ln';      OperationsInfo[12].ID := OpLn;        // OpLn
  OperationsInfo[13].Priority := 0; OperationsInfo[13].Arguments :=  1; OperationsInfo[13].Name := 'asin';    OperationsInfo[13].ID := OpASin;      // OpASin
  OperationsInfo[14].Priority := 0; OperationsInfo[14].Arguments :=  1; OperationsInfo[14].Name := 'acos';    OperationsInfo[14].ID := OpACos;      // OpACos
  OperationsInfo[15].Priority := 0; OperationsInfo[15].Arguments :=  1; OperationsInfo[15].Name := 'atan';    OperationsInfo[15].ID := OpATan;      // OpATan
  OperationsInfo[16].Priority := 0; OperationsInfo[16].Arguments :=  1; OperationsInfo[16].Name := 'exp';     OperationsInfo[16].ID := OpExp;       // OpExp
  OperationsInfo[17].Priority := 0; OperationsInfo[17].Arguments :=  1; OperationsInfo[17].Name := 'sqrt';    OperationsInfo[17].ID := OpSqrt;      // OpSqrt
  OperationsInfo[18].Priority := 0; OperationsInfo[18].Arguments :=  1; OperationsInfo[18].Name := 'sqr';     OperationsInfo[18].ID := OpSqr;       // OpSqr
  OperationsInfo[19].Priority := 0; OperationsInfo[19].Arguments :=  2; OperationsInfo[19].Name := 'logn';    OperationsInfo[19].ID := OpLogN;      // OpLogN
  OperationsInfo[20].Priority := 0; OperationsInfo[20].Arguments :=  1; OperationsInfo[20].Name := 'int';     OperationsInfo[20].ID := OpInt;       // OpInt
  OperationsInfo[21].Priority := 0; OperationsInfo[21].Arguments :=  1; OperationsInfo[21].Name := 'frac';    OperationsInfo[21].ID := OpFrac;      // OpFrac
  OperationsInfo[22].Priority := 0; OperationsInfo[22].Arguments :=  1; OperationsInfo[22].Name := 'abs';     OperationsInfo[22].ID := OpAbs;       // OpAbs
  OperationsInfo[23].Priority := 0; OperationsInfo[23].Arguments :=  1; OperationsInfo[23].Name := 'ceil';    OperationsInfo[23].ID := OpCeil;      // OpCeil
  OperationsInfo[24].Priority := 0; OperationsInfo[24].Arguments :=  1; OperationsInfo[24].Name := 'floor';   OperationsInfo[24].ID := OpFloor;     // OpFloor
  OperationsInfo[25].Priority := 0; OperationsInfo[25].Arguments :=  2; OperationsInfo[25].Name := 'ldexp';   OperationsInfo[25].ID := OpLdexp;     // OpLdexp
  OperationsInfo[26].Priority := 0; OperationsInfo[26].Arguments :=  1; OperationsInfo[26].Name := 'lnxp1';   OperationsInfo[26].ID := OpLnXP1;     // OpLnXP1
  OperationsInfo[27].Priority := 0; OperationsInfo[27].Arguments := -1; OperationsInfo[27].Name := 'max';     OperationsInfo[27].ID := OpMax;       // OpMax
  OperationsInfo[28].Priority := 0; OperationsInfo[28].Arguments := -1; OperationsInfo[28].Name := 'min';     OperationsInfo[28].ID := OpMin;       // OpMin
  OperationsInfo[29].Priority := 0; OperationsInfo[29].Arguments :=  2; OperationsInfo[29].Name := 'roundto'; OperationsInfo[29].ID := OpRoundTo;   // OpRoundTo
  OperationsInfo[30].Priority := 0; OperationsInfo[30].Arguments :=  1; OperationsInfo[30].Name := 'sign';    OperationsInfo[30].ID := OpSign;      // OpSign
  OperationsInfo[31].Priority := 0; OperationsInfo[31].Arguments := -1; OperationsInfo[31].Name := 'sum';     OperationsInfo[31].ID := OpSum;       // OpSum
end;

destructor TcyMathParser.Destroy;
begin
  fResultStack.Free;
  fPrefixStack.Free;
  Variables.Free;
  inherited;
end;

function TcyMathParser.GetOperation(Index: Integer): TOperationInfo;
begin
  Result := OperationsInfo[Index];
end;

function TcyMathParser.GetOperationCount: Integer;
begin
  Result := Length(OperationsInfo);
end;

function TcyMathParser.GetOperationIndex(OperationName: String): Integer;
var i: Integer;
begin
  Result := -1;
  OperationName := AnsiLowerCase(OperationName);

  for i:= 0 to length(OperationsInfo)-1 do
    if OperationsInfo[i].Name = OperationName
    then begin
      Result := i;
      break;
    end;
end;

function TcyMathParser.GetOperationInfo(OperationName: string): TOperationInfo;
var i: Integer;
begin
  Result := OperationsInfo[0];  // None

 for i:= 1 to length(OperationsInfo)-1 do
  if OperationsInfo[i].Name = OperationName
  then begin
    Result := OperationsInfo[i];
    break;
  end;
end;

function TcyMathParser.RenameOperation(CurrentName, NewName: String): Boolean;
var i: Integer;
begin
  Result := false;
  if NewName = '' then Exit;
  Newname := AnsiLowerCase(NewName);
  if not ValidVariableName(NewName) then Exit;

  i := GetOperationIndex(AnsiLowerCase(CurrentName));

  if i <> -1
  then begin
    OperationsInfo[i].Name := NewName;
    Result := true;
  end;
end;

function TcyMathParser.AddCustomOperation(Name: String; Arguments: Integer; Priority: Byte): Boolean;
var l: Integer;
begin
  Result := false;
  if Name = '' then Exit;
  Name := AnsiLowerCase(Name);
  if not ValidVariableName(Name) then Exit;

  l := Length(OperationsInfo);
  SetLength(OperationsInfo, l+1);
  OperationsInfo[l].Priority := Priority;
  OperationsInfo[l].Arguments := Arguments;
  OperationsInfo[l].Name := Name;
  OperationsInfo[l].ID := OpCustom;
  Result := true;
end;

// Determine if variable Name is defined with 'a'..'z', '_' and does not enter in conflict with function Names:
function TcyMathParser.ValidVariableName(Name: string): Boolean;
var
 i: Integer;
begin
  Result:= false;
  Name := trim(AnsiLowerCase(Name));
  if (Name = '') or (Name = 'e') then exit;        // ex: 5E3 = 5 * 10*10*10
  if GetOperationIndex(Name) <> -1 then exit;
  if not (Name[1] in ['_', 'a'..'z']) then exit;

  for i:= 2 to length(Name) do
    if not (Name[i] in ['_', 'a'..'z', '0'..'9'])
    then exit;

  Result:= True;
end;

procedure TcyMathParser.SetExpression(const Value: String);
var InfixStack: TStack;
begin
  fExpression := Value;
  fLastError := cNoError;
  fLastErrorBeforeParse := cNoError;
  fPrefixStack.Clear;
  fResultStack.Clear;
  if fExpression = '' then exit;

  // Get infix stack :
  InfixStack := TStack.create;
  StrToInfixStack(fExpression, InfixStack);

  if fLastError = cNoError then
    if ValidateInfixStack(InfixStack) then
      InfixToPreFix(InfixStack, fPrefixStack);

  fLastErrorBeforeParse := fLastError;

  InfixStack.Free;
end;

// Convert infix notation to stack infix notation :
procedure TcyMathParser.StrToInfixStack(aExpression: String; aStack: TStack);
var
  i, j, lengthExpr: integer;
  s: string;
  v: Extended;
  OpIndex: Integer;
  fs: TFormatSettings;
begin
  aStack.Clear;
  aExpression := AnsiLowerCase(aExpression);
  lengthExpr := length(aExpression);
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, fs);
  i := 1;

  while i <= lengthExpr do
    case aExpression[i] of
      '(','{','[':
        begin
          aStack.Add(ReturnTStackInfo(0, tsLeftParenthese, opNone, aExpression[i]));
          inc(i);
        end;

      ')','}',']':
        begin
          aStack.Add(ReturnTStackInfo(0, tsRightParenthese, opNone, aExpression[i]));
          inc(i);
        end;

      'a'..'z', '_':  // Functions and variables must begin with a letter or with '_'
        begin
          s := '';
          for j := i to lengthExpr do
            if aExpression[j] in ['a'..'z', '_', '0'..'9', ' ']
            then begin
              // case of the function "E": (Exemple: 5E3 = 5 * 10*10*10), must be followed by a number :
              if (s = 'e') and (aExpression[j] in ['0'..'9', ' '])
              then begin
                s := '';
                inc(i);  // Return to next car after "E"
                // E must be replaced by *10^
                aStack.Add(ReturnTStackInfo(0, tsOperator, OpMultiply, '*'));
                aStack.Add(ReturnTStackInfo(10, tsValue));
                aStack.Add(ReturnTStackInfo(0, tsOperator, OpPower, '^'));
                Break;
              end
              else
                if aExpression[j] <> ' '
                then s := s + aExpression[j]
                else Break;
            end
            else
              break;

          if s <> ''
          then begin
            i := j;
            OpIndex := GetOperationIndex(s);

            if OpIndex = -1   // It' s a variable
            then
              aStack.Add(ReturnTStackInfo(0, tsVariable, opNone, s))
            else
              if OperationsInfo[OpIndex].Priority <> 0 // Operators
              then aStack.Add(ReturnTStackInfo(0, tsOperator, OperationsInfo[OpIndex].ID, OperationsInfo[OpIndex].Name))
              else aStack.Add(ReturnTStackInfo(0, tsFunction, OperationsInfo[OpIndex].ID, OperationsInfo[OpIndex].Name));
          end;
        end;

      '0'..'9', '.', ',':
        begin
          s:= '';
          for j := i to lengthExpr do
            if aExpression[j] in ['0'..'9']
            then
              s := s + aExpression[j]
            else
              if aExpression[j] in ['.', ',']
              then s := s + fs.DecimalSeparator
              else break;

          i := j;

          if not TryStrToFloat(s, V)
          then begin
            fLastError := cErrorInvalidFloat;
            Exit;
          end;

          aStack.Add(ReturnTStackInfo(v, tsValue));
        end;

      ';':
        begin
          aStack.Add(ReturnTStackInfo(0, TsSeparator));
          inc(i);
        end;

      '-', '+', '/', '*', '^', '%':
        begin
          aStack.Add(ReturnTStackInfo(0, tsOperator, GetOperationInfo(aExpression[i]).ID, aExpression[i]));
          inc(i);
        end;

      // Space, just ignore
      ' ':
        Inc(i);

      else begin
        fLastError := cErrorInvalidCar;
        exit;
      end;
    end;
end;

function TcyMathParser.ValidateInfixStack(aStack: TStack): Boolean;
var
  LeftParentesesCount, RightParentesesCount: Integer;
  i: Integer;

  j, c, NbArguments: Integer;
begin
  LeftParentesesCount := 0;
  RightParentesesCount := 0;

  i := 0;
  while (fLastError = cNoError) and (i <= aStack.Count-1) do   // Note that aStack.Count can change!
    case aStack[i].TypeStack of
      tsLeftParenthese:
      begin
        // *** Check for invalid position *** //
        if i > 0
        then begin
          // Need multiply operator ?
          if aStack[i-1].TypeStack in [tsValue, TsVariable, tsRightParenthese]
          then begin
            aStack.Insert(ReturnTStackInfo(0, tsOperator, OpMultiply, '*'), i);
            Continue; // Will process this new stack
          end;
        end;

        if (i = aStack.Count - 1)
        then fLastError := cErrorMissingRightParenteses;

        inc(LeftParentesesCount);
        inc(i);
      end;

      tsRightParenthese:
      begin
        // *** Check for invalid position *** //
        if i > 0
        then begin
         if aStack[i-1].TypeStack in [tsFunction, tsOperator, TsSeparator]
         then fLastError := cErrorRightParentese;
        end;

        inc(RightParentesesCount);
        inc(i);

        if (fLastError = cNoError) and (RightParentesesCount > LeftParentesesCount)
        then fLastError := cErrorMissingLeftParenteses;
      end;

      tsValue, TsVariable:
      begin
        // *** Check for invalid position *** //
        if i > 0
        then begin
          // Need multiply operator ?
          if aStack[i-1].TypeStack in [tsValue, TsVariable, tsRightParenthese]
          then begin
            aStack.Insert(ReturnTStackInfo(0, tsOperator, OpMultiply, '*'), i);
            Continue; // Will process this new stack
          end;

          if aStack[i-1].TypeStack = tsFunction
          then fLastError := cErrorFxNeedLeftParentese;
        end;

        inc(i);
      end;

      tsFunction:
      begin
        // *** Check for invalid position *** //
        if i > 0
        then begin
          // Need multiply operator ?
          if aStack[i-1].TypeStack in [tsValue, TsVariable, tsRightParenthese]
          then begin
            aStack.Insert(ReturnTStackInfo(0, tsOperator, OpMultiply, '*'), i);
            Continue; // Will process this new stack
          end;
        end;

        if (i = aStack.Count - 1)
        then fLastError := cErrorFxNeedLeftParentese;

        inc(i);
      end;

      tsSeparator:
      begin
        // *** Check for invalid use *** //
        if i = 0
        then
          fLastError := cErrorSeparator
        else
          if (i = aStack.Count - 1)
          then
            fLastError := cErrorSeparatorNeedArgument
          else
            case aStack[i-1].TypeStack of
              tsFunction: fLastError := cErrorFxNeedLeftParentese;
              tsSeparator: fLastError := cErrorSeparatorNeedArgument;
              tsOperator, tsLeftParenthese: fLastError := cErrorSeparator;
            end;

        inc(i);
      end;

      tsOperator:
      begin
        // *** Check for invalid use *** //
        if i = 0
        then begin
          case aStack[0].OperationID of
            OpAdd:
              begin
                aStack.Delete(0);
                Dec(i);
              end;

            OpSubstract:
              begin
                aStack.fList[0].Name := 'neg';
                aStack.fList[0].OperationID := OpNeg;
              end

            else
              fLastError := cErrorOperator;
          end;
        end
        else
          if (i = aStack.Count - 1)
          then
            fLastError := cErrorOperatorNeedArgument
          else
            case aStack[i-1].TypeStack of
              tsFunction: fLastError := cErrorFxNeedLeftParentese;
              tsOperator, tsLeftParenthese, tsSeparator:  // excluding opNeg that is handled upper ...
                case aStack[i].OperationID of     // Check current operation
                  OpSubstract:
                    if (aStack[i-1].TypeStack = tsOperator) and (aStack[i-1].OperationID = OpNeg)
                    then begin
                      // opSubstract nullify opNeg :
                      aStack.Delete(i-1);
                      Dec(i);

                      aStack.Delete(i);
                      Dec(i);
                    end
                    else begin
                      aStack.fList[i].Name := 'neg';
                      aStack.fList[i].OperationID := OpNeg;
                    end;

                  OpAdd:
                    begin
                      aStack.Delete(i);
                      Dec(i);
                    end;

                  else   // like opMultiply etc ...
                    fLastError := cErrorOperator;
                end;
            end;

        inc(i);
      end;
    end;

  // Handle functions with undefined operands number :
  i:= 0;
  while (fLastError = cNoError) and (i < aStack.Count) do
  begin
    if (aStack[i].TypeStack = tsFunction) and (GetOperationInfo(aStack[i].Name).Arguments = -1)
    then begin
      c := 1;
      NbArguments := 1;
      j := i + 2;

      while (j < aStack.Count) and (c > 0) do
      begin
        case aStack[j].TypeStack of
          tsSeparator: if c = 1 then inc(NbArguments);
          tsLeftParenthese:  Inc(c);
          tsRightParenthese: dec(c);
        end;
        inc(j);
      end;

      aStack.fList[i].ArgumentsCount := NbArguments;        // Store the number of arguments
    end;

    inc(i);
  end;

  if (fLastError = cNoError) and (LeftParentesesCount <> RightParentesesCount)
  then
    if LeftParentesesCount > RightParentesesCount
    then fLastError := cErrorMissingRightParenteses
    else fLastError := cErrorMissingLeftParenteses;

  Result := fLastError = cNoError;
end;

procedure TcyMathParser.InfixToPreFix(Infix: TStack; Prefix: TStack);
var
  TmpStack: TStack;
  i: Integer;
  Current: TStackInfo;
begin
  TmpStack := TStack.Create;

  for i:= 0 to Infix.Count-1 do
  begin
    Current := Infix[i];

    case Current.TypeStack of
      tsValue, TsVariable:
        Prefix.Add(Current);

      tsFunction:
        TmpStack.Add(Current);

      tsLeftParenthese:
        TmpStack.Add(Current);

      tsRightParenthese:  // end of previous argument or group of arguments
        begin
          while TmpStack.Count <> 0 do
            if TmpStack.Last.TypeStack <> tsLeftParenthese
            then Prefix.Add(TmpStack.DeleteLast)
            else Break;

          TmpStack.DeleteLast;

          if TmpStack.Count <> 0 then
            if TmpStack.Last.TypeStack = tsFunction then
              Prefix.Add(TmpStack.DeleteLast);
        end;

      tsSeparator:        // end of previous argument
        begin
          while TmpStack.Count <> 0 do
            if TmpStack.Last.TypeStack <> tsLeftParenthese
            then Prefix.Add(TmpStack.DeleteLast)
            else Break;
        end;

      tsOperator:
        begin
          while (TmpStack.Count > 0) do
            if (TmpStack.Last.TypeStack = tsOperator) and (GetOperationInfo(Current.Name).Priority <= GetOperationInfo(TmpStack.Last.Name).Priority)
            then Prefix.Add(TmpStack.DeleteLast)
            else Break;

         TmpStack.Add(Current);
        end;
    end;
  end;

  while TmpStack.Count > 0 do
    Prefix.Add(TmpStack.DeleteLast);

  TmpStack.Free;
end;

function TcyMathParser.Parse: Boolean;

    function ExtendedMod(x, y: Extended): Extended;
    begin
      Result := x - int(x / y) * y;
    end;

var
  Current: TStackInfo;
  i, j, Arguments: Integer;
  aValue: Extended;
  Values: array of Extended;

        procedure ApplyOperation;
        var v: Integer;
        begin
          try
            case Current.OperationID of
              opNone: ;

              // Operators :
              OpPower    : if (frac(Values[0]) <> 0) and (Values[1] < 0)
                           then fLastError := cErrorPower
                           else fResultStack.Add(ReturnTStackInfo(power(Values[1], Values[0]), tsValue));

              OpMultiply : fResultStack.Add(ReturnTStackInfo(Values[1] * Values[0], tsValue));

              OpDivide   : if Values[0] = 0
                           then fLastError := cErrorDivByZero
                           else fResultStack.Add(ReturnTStackInfo(Values[1] / Values[0], tsValue));

              OpAdd      : fResultStack.Add(ReturnTStackInfo(Values[1] + Values[0], tsValue));

              OpSubstract: fResultStack.Add(ReturnTStackInfo(Values[1] - Values[0], tsValue));

              OpNeg      : fResultStack.Add(ReturnTStackInfo(-Values[0], tsValue));

              opMod      : if Values[0] = 0
                           then fLastError := cErrorDivByZero
                           else fResultStack.Add( ReturnTStackInfo(ExtendedMod(Values[1], Values[0]), tsValue) );

              // Functions :
              OpCos      : fResultStack.Add(ReturnTStackInfo(cos(Values[0]), tsValue));

              OpSin      : fResultStack.Add(ReturnTStackInfo(sin(Values[0]), tsValue));

              OpTan      : fResultStack.Add(ReturnTStackInfo(tan(Values[0]), tsValue));

              OpLog      : if Values[0] <= 0
                           then fLastError := cErrorFxInvalidValue
                           else fResultStack.Add(ReturnTStackInfo(log10(Values[0]), tsValue));

              OpLn       : if Values[0] <= 0
                           then fLastError := cErrorFxInvalidValue
                           else fResultStack.Add(ReturnTStackInfo(ln(Values[0]), tsValue));

              OpASin     : if (Values[0] < -1) or (Values[0] > 1)
                           then fLastError := cErrorFxInvalidValue
                           else fResultStack.Add(ReturnTStackInfo(arcsin(Values[0]), tsValue));

              OpACos     : if (Values[0] < -1) or (Values[0] > 1)
                           then fLastError := cErrorFxInvalidValue
                           else fResultStack.Add(ReturnTStackInfo(arccos(Values[0]), tsValue));

              OpATan     : fResultStack.Add(ReturnTStackInfo(arctan(Values[0]), tsValue));

              OpExp      : fResultStack.Add(ReturnTStackInfo(exp(Values[0]), tsValue));

              OpSqrt     : if Values[0] < 0
                           then fLastError := cErrorFxInvalidValue
                           else fResultStack.Add(ReturnTStackInfo(sqrt(Values[0]), tsValue));

              OpSqr      : fResultStack.Add(ReturnTStackInfo(sqr(Values[0]), tsValue));

              OpInt      : fResultStack.Add(ReturnTStackInfo(int(Values[0]), tsValue));

              OpFrac     : fResultStack.Add(ReturnTStackInfo(frac(Values[0]), tsValue));

              OpAbs      : fResultStack.Add(ReturnTStackInfo(abs(Values[0]), tsValue));

              OpLogN     : if (Values[1] <= 0) or (Values[0] <= 0) or (Log2(Values[1]) = 0)
                           then
                             fLastError := cErrorFxInvalidValue
                           else
                             if fLognFirstArg = lognBase
                             then fResultStack.Add(ReturnTStackInfo(logn(Values[1], Values[0]), tsValue))
                             else fResultStack.Add(ReturnTStackInfo(logn(Values[0], Values[1]), tsValue));

              OpCeil     : fResultStack.Add(ReturnTStackInfo(Ceil(Values[0]), tsValue));

              OpFloor    : fResultStack.Add(ReturnTStackInfo(Floor(Values[0]), tsValue));

              OpLdexp    : fResultStack.Add(ReturnTStackInfo(Ldexp(Values[1], round(Values[0])), tsValue));

              OpLnXP1    : if Values[0] <= -1
                           then fLastError := cErrorFxInvalidValue
                           else fResultStack.Add(ReturnTStackInfo(LnXP1(Values[0]), tsValue));

              OpMax      : begin
                             aValue := Values[0];
                             for v := 0 to Arguments-1 do
                               if Values[v] > aValue
                               then aValue := Values[v];

                             fResultStack.Add(ReturnTStackInfo(aValue, tsValue));
                           end;

              OpMin      : begin
                             aValue := Values[0];
                             for v := 0 to Arguments-1 do
                               if Values[v] < aValue
                               then aValue := Values[v];

                             fResultStack.Add(ReturnTStackInfo(aValue, tsValue));
                           end;

              OpRoundTo  : fResultStack.Add(ReturnTStackInfo(RoundTo(Values[1], round(Values[0])), tsValue));

              OpSign     : fResultStack.Add(ReturnTStackInfo(Sign(Values[0]), tsValue));

              OpSum      : begin
                             aValue := 0;
                             for v := 0 to Arguments-1 do
                               aValue := aValue + Values[v];

                             fResultStack.Add(ReturnTStackInfo(aValue, tsValue));
                           end;

              OpCustom   : begin
                             aValue := 0;
                             if Assigned(fOnCustomOperationParse)
                             then fOnCustomOperationParse(Self, Current.Name, Values, aValue);

                             fResultStack.Add(ReturnTStackInfo(aValue, tsValue));
                           end;
            end;
          except
            on EInvalidOp do fLastError := cCalcError;
          end;
        end;

begin
  fResultStack.Clear;
  fLastError := fLastErrorBeforeParse;

  i := 0;
  while (fLastError = cNoError) and (i < fPrefixStack.Count) do
  begin
    Current := fPrefixStack[i];
    inc(i);

    case Current.TypeStack of
      tsValue :
        fResultStack.Add(Current);

      tsVariable:
        begin
         if not Variables.GetValue(Current.Name, aValue)
         then fLastError := cErrorUnknownName
         else fResultStack.Add(ReturnTStackInfo(aValue, tsValue));
        end;

      tsOperator, tsFunction:
        begin
          Arguments := GetOperationInfo(Current.Name).Arguments;

          // Functions with undefined arguments :
          if Arguments = -1
          then Arguments := Current.ArgumentsCount;

          if fResultStack.Count >= Arguments // Suficient arguments/operands?
          then begin
            SetLength(Values, Arguments);

            // Store needed arguments/operands in array:
            for j := 0 to Arguments - 1 do
              Values[j] := fResultStack.DeleteLast.Value;

            // Make the calc :
            ApplyOperation;
          end
          else
            fLastError := cErrorNotEnoughArgs;
        end;
    end;
  end;

  // All stacks parsed ?
  if fResultStack.Count > 1 then
    fLastError := cInternalError;

  Result := fLastError = cNoError;
end;

function TcyMathParser.GetParserResult: Extended;
begin
  if fResultStack.Count > 0
  then Result := fResultStack[0].Value   // Retrieve topmost stack
  else Result := 0;
end;

function TcyMathParser.GetLastError: Integer;
begin
  Result := fLastError;
end;

function TcyMathParser.GetLastErrorString: String;
begin
  Result := GetErrorString(fLastError);
end;

function GetErrorString(ErrorCode: integer): String;
begin
  case ErrorCode of
    cNoError:                     Result := '';

    cInternalError:               Result := 'Cannot parse';

    cErrorInvalidCar:             Result := 'Invalid car';
    cErrorUnknownName:            Result := 'Unknown function or variable';
    cErrorInvalidFloat:           Result := 'Invalid float number';
    cErrorOperator:               Result := 'Operator cannot be placed here';
    cErrorFxNeedLeftParentese:    Result := 'A function must be followed by left parentese';
    cErrorNotEnoughArgs:          Result := 'Not enough arguments or operands';
    cErrorSeparatorNeedArgument:  Result := 'Missing argument after separator';
    cErrorMissingLeftParenteses:  Result := 'Missing at least one left parentese';
    cErrorMissingRightParenteses: Result := 'Missing at least one right parentese';
    cErrorLeftParentese:          Result := 'Left parentese cannot be placed here';
    cErrorRightParentese:         Result := 'Right parentese cannot be placed here';
    cErrorSeparator:              Result := 'Separator cannot be placed here';
    cErrorOperatorNeedArgument:   Result := 'Operator must be followed by argument';

    cCalcError:                   Result := 'Invalid operation';
    cErrorDivByZero:              Result := 'Division by zero';
    cErrorPower:                  Result := 'Invalid use of power function';
    cErrorFxInvalidValue:         Result := 'Invalid parameter value for function';
  end;
end;

end.
