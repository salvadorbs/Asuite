/// UML diagrams rendering
// - this unit is part of SynProject, under GPL 3.0 license; version 1.18
unit ProjectDiagrams;

interface

uses
  Graphics,
  SysUtils,
  Classes;

/// convert a text sequence into an UML-like vectorial diagram
// - returns nil if the supplied content is incorrect
// - text syntax is pretty easy:
// $A=Alice                          // optional participant naming and ordering
// $B=Bob
// $A->B:synchronous call
// $A->>A:asynchronous arrow
// $B-->A:dotted open arrow
// $A->:Authentication Request
// $alt:successful case              // alternative group
// $ B->A:Authentication Accepted    // internal lines shall be indented
// $:on failure                      // same level group will be
// $ B->A:authentication Failure
// $ opt:                            // nested group
// $  loop:10000 times
// $   A->B:DNS attack
// $:on timeout
// $ B->A:please retry
// $A->+Service:DoQuery()            // + - notifies lifeline activation
// $+Service->Domain:PrepareQuery()
// $Domain-->TQuery:Prepare()
// $-Domain-->Service
// $+Service->Domain:CommitQuery()
// $*TQuery                         // * destroy a participant
function UMLSequenceToMetaFile(const Content: string): TMetafile;


{
C=Customer
O=Order
M=Menu Manager
?>>C:hunger
 >>O:<<create>
loop:until complete
 >:Add item
  >M:Check Available
 <:a Callback
 =:a self message
 >:return
ref:complete Order And Pay
M>?:Stock update


W=Website
H=Warehouse
B=Banck
? "Customer Order" W
alt:deliver to home
 W processPayment H <<return>>
  "validate card" B <<return>>
 mailToHome H <<return>>
:collect from store
 W mailToStore H <<return>>

C=Customer
O=Order
M=Menu Manager
? hunger C
C <<create>> O
loop:until complete
 C "Add item" O
 O "Check Available" M <<return>>
  M "a Callback" O "<<callback return>>"
  O "a self message" O
ref:Complete Order and Pay
M "Stock update" ?

}
implementation

type
  TUMLSequenceLineStyle = (slsSynchCall, slsAsynchCall, slsSynchReturn);
  TUMLSequence = class
  protected
    function ParticipantIndex(const aName, aIdent: string;
      CreateIfNotExisting: boolean=true): integer;
  public
    Participant: array of record
      Name: string;
      Ident: string;
    end;
    Items: array of record
      Style: TUMLSequenceLineStyle;
      FromParticipant: integer;
      ToParticipant: integer;
    end;
    procedure ParseLine(Line: string);
    function RenderContent: TMetaFile;
  end;


function UMLSequenceToMetaFile(const Content: string): TMetafile;
var Lines: TStringList;
    i: integer;
begin
  with TUMLSequence.Create do
  try
    Lines := TStringList.Create;
    try
      Lines.Text := Content;
      for i := 0 to Lines.Count-1 do
        ParseLine(Lines[i]);
    finally
      Lines.Free;
    end;
    result := RenderContent;
  finally
    Free;
  end;
end;


{ TUMLSequence }

procedure TUMLSequence.ParseLine(Line: string);
var i,j: integer;
    activation: (aNone, aActivate, aDisactivate, aDestroy);
    fromPart, toPart: string;
    style: TUMLSequenceLineStyle;
begin
  if (Line='') or (Line[1]=';') then
    exit;
  // A=Alice  or  Alice=
  for i := 1 to length(Line) do
    if Line[i]='=' then begin
      ParticipantIndex(Copy(Line,1,i-1),copy(Line,i+1,1000));
      exit;
    end else
    if not (Line[i] in ['A'..'Z','a'..'z','0'..'9','_']) then
      break;
  case Line[1] of
  '+': activation := aActivate;
  '-': activation := aDisactivate;
  '*': activation := aDestroy;
  else activation := aNone;
  end;
  if activation<>aNone then
    delete(Line,1,1);
  // A->B  or  Alice-->Bob
  for i := 1 to length(Line) do
    if Line[i]=':' then break else
    if Line[i]='-' then begin
      fromPart := copy(line,1,i-1);
      j := i+1;
      if j>=length(Line) then
        break;
      if line[j]='>' then
        if line[j+1]='>' then begin
          style := slsAsynchCall;
          inc(j,2);
        end else begin
          style := slsSynchCall;
          inc(j);
        end else
      if (line[j]='-') and (line[j+1]='>') then begin
        style := slsSynchReturn;
        inc(j,2);
      end else
       break;
      // TO BE DONE
      exit;
    end;

end;

function TUMLSequence.ParticipantIndex(const aName, aIdent: string;
  CreateIfNotExisting: boolean): integer;
begin
  for result := 0 to high(Participant) do
    if Participant[result].Name=aName then
      exit;
  if CreateIfNotExisting then begin
    result := Length(Participant);
    SetLength(Participant,result+1);
    Participant[result].Name := aName;
    if aIdent='' then
      Participant[result].Ident := aName else
      Participant[result].Ident := aIdent;
  end else
    result := -1;
end;

function TUMLSequence.RenderContent: TMetaFile;
begin

end;

end.
