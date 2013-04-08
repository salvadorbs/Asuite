{
  @cvs($Date: 2006-11-15 07:12:34 +0100 (¶ro, 15 lis 2006) $)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  @abstract(string vector --- based on TStringList)
  The string vector is based on TStringList and simply exports
  a few extra functions - I did this so I didn't have to change
  so much old code, this has only little additional
  functionality
}
unit PasDoc_StringVector;

interface
uses
  Classes;
   
type
  TIterateFunc = function(const AString: string): string;
  TStringVector = class(TStringList)
  public
    { This is the same thing as Items[0] }
    function FirstName: string;
    
    procedure LoadFromTextFileAdd(const AFilename: string); overload;
    procedure LoadFromTextFileAdd(var ATextFile: TextFile); overload;
    procedure RemoveAllNamesCI(const AName: string);
    function ExistsNameCI(const AName: string): boolean;
    function IsEmpty: boolean;
    procedure Iterate(const AItFunc: TIterateFunc);
    procedure AddNotExisting(const AString: string);
    
    { This loads our contents (i.e. Count and Items[] values)
      from a stream using the binary format
      - SizeOf(Count) bytes for Count
      - then each string is loaded using 
        @link(TSerializable.LoadStringFromStream). 

      This is better than simply loading/saving our Text value,
      by @code(Text := TSerializable.LoadStringFromStream(Stream)),
      because when such loading splits multiline strings,
      e.g. if Items[0] = 'foo' + LineEnding + 'bar', 
      then after you do Text := 'foo' + LineEnding + 'bar'
      you get two items: Items[0] = 'foo' and Items[1] = 'bar'. }
    procedure LoadFromBinaryStream(Stream: TStream);
    
    { This saves our contents in a format readable by 
      @link(LoadFromBinaryStream). }
    procedure SaveToBinaryStream(Stream: TStream);
  end;

function NewStringVector: TStringVector;
function StringVectorIsNilOrEmpty(const AOV: TStringVector): boolean;

implementation
uses
  SysUtils, PasDoc_Serialize;

function StringVectorIsNilOrEmpty(const AOV: TStringVector): boolean;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

function NewStringVector: TStringVector;
begin
  Result := TStringVector.Create;
  Result.Duplicates := dupIgnore;
end;

{ TStringVector }

procedure TStringVector.AddNotExisting(const AString: string);
begin
  if IndexOf(AString) < 0 then 
    Add(AString);
end;

function TStringVector.ExistsNameCI(const AName: string): boolean;
var i: Integer;
begin
  Result := True;
  for i := Count - 1 downto 0 do
    if SameText(Get(i),AName) then
      exit;
  Result := false;
end;

function TStringVector.FirstName: string;
begin
  if Count > 0 then
    Result := Get(0) else
    Result := '';
end;

function TStringVector.IsEmpty: boolean;
begin
  Result := Count = 0;
end;

procedure TStringVector.Iterate(const AItFunc: TIterateFunc);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    Strings[i] := AItFunc(Strings[i]);
end;

procedure TStringVector.LoadFromTextFileAdd(const AFilename: string);
var
  LCurrent: string;
begin
  LCurrent := Text;
  LoadFromFile(AFilename);
  Add(LCurrent);
end;

procedure TStringVector.LoadFromTextFileAdd(var ATextFile: TextFile);
var S: string;
begin
  while not Eof(ATextFile) do
  begin
    Readln(ATextFile, S);
    S := Trim(S);
    if S <> '' then Append(S);
  end;
end;

procedure TStringVector.RemoveAllNamesCI(const AName: string);
var
  i: Integer;
  LName: string;
begin
  LName := LowerCase(AName);
  for i := Count - 1 downto 0 do
    if LowerCase(Get(i)) = LName then
      Delete(i);
end;

procedure TStringVector.LoadFromBinaryStream(Stream: TStream);
var i: Integer;
begin
  Clear;
  for i := 0 to TSerializable.LoadIntegerFromStream(Stream) - 1 do
    Append(TSerializable.LoadStringFromStream(Stream));
end;

procedure TStringVector.SaveToBinaryStream(Stream: TStream);
var i: Integer;
begin
  TSerializable.SaveIntegerToStream(Count, Stream);
  for i := 0 to Count - 1 do
    TSerializable.SaveStringToStream(Strings[i], Stream);
end;

end.
