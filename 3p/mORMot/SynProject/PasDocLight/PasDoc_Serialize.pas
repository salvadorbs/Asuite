unit PasDoc_Serialize;

interface
uses
  Classes,
  SysUtils;

type
  TSerializable = class;
  TSerializableClass = class of TSerializable;

  TSerializable = class
  private
    FWasDeserialized: boolean;
  protected
    procedure Serialize(const ADestination: TStream); virtual;
    procedure Deserialize(const ASource: TStream); virtual;
  public  
    class function LoadStringFromStream(const ASource: TStream): string;
    class procedure SaveStringToStream(const AValue: string; const ADestination: TStream);
    class function LoadDoubleFromStream(const ASource: TStream): double;
    class procedure SaveDoubleToStream(const AValue: double; const ADestination: TStream);
    class function LoadIntegerFromStream(const ASource: TStream): Integer;
    class procedure SaveIntegerToStream(const AValue: Integer; const ADestination: TStream);

    constructor Create; virtual;
    class procedure SerializeObject(const AObject: TSerializable; const ADestination: TStream);
    class function DeserializeObject(const ASource: TStream): TSerializable;
    class procedure Register(const AClass: TSerializableClass);
    procedure SerializeToFile(const AFileName: string);
    class function DeserializeFromFile(const AFileName: string): TSerializable;
    class function DeserializeFromStream(AStream: TStream): TSerializable;
    property WasDeserialized: boolean read FWasDeserialized;
  end;

  ESerializedException = class(Exception);

implementation

uses
  PasDoc_Utils;
  
var
  GClassNames: TStringList;

{ TSerializable }

constructor TSerializable.Create;
begin
  inherited;
end;

procedure TSerializable.Serialize(const ADestination: TStream); 
begin
end;

procedure TSerializable.Deserialize(const ASource: TStream);
begin
  FWasDeserialized := True;                 
end;

class function TSerializable.DeserializeFromFile(
  const AFileName: string): TSerializable;
var LF: THeapMemoryStream;
begin
  LF := THeapMemoryStream.Create;
  LF.LoadFromFile(AFileName);
  try
    Result := DeserializeObject(LF);
  finally
    LF.Free;
  end;
end;

class function TSerializable.DeserializeFromStream(AStream: TStream): TSerializable;
begin
  Result := DeserializeObject(AStream);
end;

class function TSerializable.DeserializeObject(
  const ASource: TStream): TSerializable;
var S: string;
    L: integer;
    LClass: TSerializableClass;
    Idx: Integer;
begin
  L := 0;
  ASource.Read(L, 1);
  Setlength(S,L);
  ASource.Read(S[1], L);
  Idx := GClassNames.IndexOf(S);
  if Idx<0 then begin
    raise ESerializedException.CreateFmt('Tried loading unknown class %s', [S]);
  end else begin
    LClass := TSerializableClass(GClassNames.Objects[Idx]);
    Result := LClass.Create;
    Result.Deserialize(ASource);
  end;
end;

class function TSerializable.LoadIntegerFromStream(
  const ASource: TStream): Integer;
begin
  ASource.Read(Result, SizeOf(Result));
end;

class function TSerializable.LoadDoubleFromStream(
  const ASource: TStream): double;
begin
  ASource.Read(Result, SizeOf(Result));
end;

class function TSerializable.LoadStringFromStream(
  const ASource: TStream): string;
var L: Integer;
begin
  ASource.Read(L, SizeOf(L));
  SetLength(Result, L);
  ASource.Read(Pointer(Result)^, L);
end;

class procedure TSerializable.Register(const AClass: TSerializableClass);
begin
  GClassNames.AddObject(AClass.ClassName, TObject(AClass));
end;

class procedure TSerializable.SaveIntegerToStream(
  const AValue: Integer; const ADestination: TStream);
begin
  ADestination.Write(AValue, SizeOf(AValue));
end;

class procedure TSerializable.SaveDoubleToStream(const AValue: double;
  const ADestination: TStream);
begin
  ADestination.Write(AValue, SizeOf(AValue));
end;

class procedure TSerializable.SaveStringToStream(const AValue: string;
  const ADestination: TStream);
var L: Integer;
begin
  L := Length(AValue);
  ADestination.Write(L, SizeOf(L));
  ADestination.Write(Pointer(AValue)^, L);
end;

class procedure TSerializable.SerializeObject(const AObject: TSerializable;
  const ADestination: TStream);
var
  S: shortstring;
begin
  S := AObject.ClassName;
  if GClassNames.IndexOf(S)<0 then
    raise ESerializedException.CreateFmt('Tried saving unregistered class %s', [S]);
  ADestination.Write(S[0], Byte(S[0])+1);
  AObject.Serialize(ADestination);
end;

procedure TSerializable.SerializeToFile(const AFileName: string);
var
  LF: THeapMemoryStream;
begin
  LF := THeapMemoryStream.Create;
  try
    SerializeObject(Self, LF);
    LF.SaveToFile(AFileName);
  finally
    LF.Free;
  end;
end;



initialization
  GClassNames := TStringList.Create;
  GClassNames.Sorted := true; // faster IndexOf()
finalization
  GClassNames.Free;
end.
 