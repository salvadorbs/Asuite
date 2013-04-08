unit StdCtrls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL StdCtrls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TButton+TCheckBox+TEdit+TLabel+TMemo
   - for TMemo: use global Text property, as there's no Lines[] property;
     don't set anything in Lines property in IDE 
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup
     (no Anchor, e.g.)

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are Copyright (C) 2001 Paul Toth. http://tothpaul.free.fr
  All Rights Reserved.

  Some modifications by Leonid Glazyrin, Feb 2012 <leonid.glazyrin@gmail.com>

  * TCheckBox: new properties: 'Alignment', 'State', 'AllowGrayed' (3-states style)
  * TRadioButton (auto-switching and groups not implemented)
  * TGroupBox
  * TListBox, TComboBox (and parent TCustomBox)
  
}

{$WARNINGS OFF}
{$HINTS ON}

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, Graphics;

type
  TLabel = class(TGraphicControl)
  protected
    procedure SetCaption(const Value: string);
  public
    procedure Paint; override;
    property Caption: string read fCaption write SetCaption;
  end;

  TButton = class(TWinControl)
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEdit = class(TWinControl)
  private
    fText: string;
    fPassWordChar: char;
    fReadOnly: boolean;
  protected
    CreateFlags: cardinal;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    procedure CreateHandle; override;
    function GetText: string;
    procedure SetText(const Value: string); override;
  public
    procedure SetReadOnly(Value: Boolean);
    procedure SelectAll;
    procedure AddLine(const s: string; AddEOLN: boolean = true);
    procedure AddLineFmt(const s: string; const Args: array of const; AddEOLN: boolean = true);
    property Text: string read GetText write SetText;
    property ReadOnly: boolean read fReadOnly write SetReadOnly;
  end;

  TMemo = class(TEdit)
  protected
    procedure CreateHandle; override;
  end;

  TAlignment = (taRightJustify, taLeftJustify); // default = 0 = taRightJustify
  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCheckBox = class(TWinControl)
  private
    fState: TCheckBoxState;
    fAllowGrayed: boolean;
    fAlignment: TAlignment;
    procedure SetChecked(const Value: boolean);
    function  GetChecked: boolean;
    procedure SetState(const Value: TCheckBoxState);
    procedure SetAllowGrayed(Value: boolean);
    function  GetState: TCheckBoxState;
  protected
    procedure CreateHandle; override;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
  public
    property Checked: boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read GetState write SetState;
    property AllowGrayed: boolean read fAllowGrayed write SetAllowGrayed;
  end;

  TRadioButton = class(TCheckBox) //should be vice versa
  protected
    procedure CreateHandle; override;
  end;

  TGroupBox = class(TWinControl)
  protected
    procedure CreateHandle; override;
  end;

  TCustomBox = class;

  TBoxStrings = class(TPersistent)
  protected
    fBox: TCustomBox;
    fStrings: TStringList;
    procedure SetStrings(StringList: TStrings);
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    function GetItem(n: Integer): string;
  public
    constructor Create(Box: TCustomBox);
    destructor  Destroy; override;
    function  Add(const S: String): Integer;
    procedure Clear;
    property Strings: TStrings read fStrings write SetStrings;
    property Items[n: integer]: string read GetItem; default;
  end;

  TCustomBox = class(TWinControl)
  protected
    fWinClass: string;
    fCreateParams: cardinal;
    fAddLineMsg:   cardinal;
    fResetMsg:     cardinal;
    fGetIndexMsg:  cardinal;
    fGetCountMsg:  cardinal;
    fDropDownHeight: integer;
    fItems: TBoxStrings;
    fItemIndex: Integer;
    fSorted: Boolean;
    function  GetItems: TStrings;
    procedure SetItems(AItems: TStrings);
    function  GetCount: Integer; virtual;
    function  GetItemIndex: Integer; virtual;
    procedure SetItemIndex(const Value: Integer); virtual;
    procedure CreateHandle; override;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    function  SubProperty(const Name: string): TPersistent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
    property Items: TBoxStrings read fItems;
    property ItemCount: Integer read GetCount;
    property ItemStrings: TStrings read GetItems write SetItems;
  end;

  TComboBoxStyle =
    (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable);

  TComboBox = class(TCustomBox)
  protected
    fText: string;
    fStyle: TComboBoxStyle;
    fOnChange:  TNotifyEvent;
    procedure WMCommand(var msg: TWMCommand); message WM_COMMAND;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    function GetText: string;
    procedure SetText(const Value: string); override;
    procedure CreateHandle; override;
    function GetDroppedDown: Boolean;
    procedure SetDroppedDown(Value: Boolean);
  public
    property Text: string read GetText write SetText;
    property OnChange:  TNotifyEvent read fOnChange write fOnChange;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
  end;

  TListBox = class(TCustomBox)
    procedure CreateHandle; override;
  end;


implementation


{ TButton }

constructor TButton.Create(AOwner:TComponent);
begin
  inherited;
  fTransparent := True; // do not use color
end;

procedure TButton.CreateHandle;
begin
  Handle := CreateWindow(
   'BUTTON',
   pointer(fCaption),
   WS_CHILD or WS_CLIPCHILDREN or WS_TABSTOP,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
end;


{ TEdit }

procedure TEdit.CreateHandle;
begin
  Color := clWhite;
  if CreateFlags=0 then begin
   CreateFlags := WS_CHILD or WS_CLIPCHILDREN or WS_TABSTOP
     or ES_AUTOHSCROLL or ES_AUTOVSCROLL;
   if fPassWordChar='*' then
     CreateFlags := CreateFlags or ES_PASSWORD;
  end;
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT or WS_EX_CLIENTEDGE,
   'EDIT',
   pointer(fCaption),
   CreateFlags,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  SendMessage(Handle,WM_SETTEXT,0,integer(fText));
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
  if fReadOnly then
    SendMessage(fHandle, EM_SETREADONLY, 1, 0);
end;

function TEdit.GetText: string;
var i: integer;
begin
  i := SendMessage(fHandle,WM_GETTEXTLENGTH,0,0);
  if i=0 then
    result := '' else begin
    SetLength(result,i);
    SetLength(result,SendMessage(fHandle,WM_GETTEXT,i+1,integer(result)));
  end;
end;

procedure TEdit.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..1] of PChar=(
    'PasswordChar','ReadOnly');
var tmp: string;
begin
  case StringIndex(Name,Properties) of
  0: begin
    tmp := Reader.StringProperty;
    if tmp<>'' then
      fPassWordChar := tmp[1];
  end;
  1: fReadOnly := Reader.BooleanProperty;
  else inherited;
  end;
end;

procedure TEdit.SelectAll;
begin
  if fHandle<>0 then
    SendMessage(fHandle, EM_SETSEL, 0, -1);
end;

procedure TEdit.SetReadOnly(Value: Boolean);
begin
  HandleNeeded;
  if fHandle<>0 then
    SendMessage(fHandle, EM_SETREADONLY, Ord(Value), 0);
  fReadOnly := Value;
end;

procedure TEdit.SetText(const Value: string);
begin
  fText := Value;
  if fHandle<>0 then
    SendMessage(fHandle,WM_SETTEXT,0,integer(Value));
end;

procedure TEdit.AddLine(const s: string; AddEOLN: boolean=true);
var len: integer;
    str: string;
begin
  HandleNeeded;
  len := SendMessage(fHandle, WM_GETTEXTLENGTH, 0, 0);
  if AddEOLN and (len>0) then
    str := #13#10+s else
    str := s;
  SendMessage(fHandle, EM_SETSEL, len, len);
  SendMessage(fHandle, EM_REPLACESEL, 0, integer(str));
end;

procedure TEdit.AddLineFmt(const s: string; const Args: array of const; AddEOLN: boolean = true);
begin
  AddLine(Format(s, Args), AddEOLN);
end;


{ TMemo }

procedure TMemo.CreateHandle;
begin
  CreateFlags := WS_VISIBLE or
    WS_CHILD or ES_MULTILINE or ES_WANTRETURN or ES_AUTOVSCROLL or WS_VSCROLL;
  inherited;
end;


{ TCheckBox }

const
  AlignStyle: array [TAlignment] of Cardinal = (0, BS_LEFTTEXT);
  GrayStyle: array [Boolean] of Cardinal = (BS_AUTOCHECKBOX, BS_AUTO3STATE);

procedure TCheckBox.CreateHandle;
begin
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT,
   'BUTTON',
   pointer(fCaption),
   WS_VISIBLE or WS_CHILD or WS_TABSTOP or GrayStyle[fAllowGrayed] or AlignStyle[fAlignment],
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  State := fState;
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
end;

procedure TRadioButton.CreateHandle;
begin
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT,
   'BUTTON',
   pointer(fCaption),
   WS_VISIBLE or WS_CHILD or BS_RADIOBUTTON or WS_TABSTOP or AlignStyle[fAlignment],
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  SendMessage(Handle, BM_SETCHECK, integer(fState), 0);
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
end;

function TCheckBox.GetChecked: boolean;
begin
  result := Boolean(GetState);
end;

function TCheckBox.GetState: TCheckBoxState;
begin
  result := TCheckBoxState(SendMessage(Handle,BM_GETCHECK,0,0));
end;

procedure TCheckBox.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..3] of PChar=(
    'Checked', 'Alignment', 'State', 'AllowGrayed');
var b: Boolean;
begin
  case StringIndex(Name,Properties) of
    0 : begin
          b := Reader.BooleanProperty;
          if fState<>cbGrayed then
            fState := TCheckBoxState(b);
        end;
    1: Reader.IdentProperty(fAlignment,TypeInfo(TAlignment));
    2: Reader.IdentProperty(fState,TypeInfo(TCheckBoxState));
    3: fAllowGrayed := Reader.BooleanProperty;
    else inherited;
  end;
end;

procedure TCheckBox.SetChecked(const Value: boolean);
begin
  SetState(TCheckBoxState(Value));
end;

procedure TCheckBox.SetState(const Value: TCheckBoxState);
begin
  if self=nil then exit;
  HandleNeeded;
  if (Value = cbGrayed) and not fAllowGrayed then
  begin
    SetAllowGrayed(True); // need to set Grayed state
    SendMessage(Handle, BM_SETCHECK, integer(Value), 0);
    SetAllowGrayed(False); // style switches back, but state persists!
  end
  else
    SendMessage(Handle, BM_SETCHECK, integer(Value), 0);
end;

procedure TCheckBox.SetAllowGrayed(Value: boolean);
var i: Integer;
const StyleMask = $F;
begin
  if fAllowGrayed = Value then Exit;
  fAllowGrayed := Value;
  i:= GetWindowLong(fHandle, GWL_STYLE);
  //if (i and StyleMask)<>GrayStyle[fAllowGrayed] then //no need to check
  // switch between 2-state and 3-state CheckBox autoswitching style
  SetWindowLong(fHandle, GWL_STYLE, i and not StyleMask or GrayStyle[fAllowGrayed]);
end;


{ TGroupBox }

procedure TGroupBox.CreateHandle;
begin
  Color:= Parent.Color;
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT,
   'BUTTON',
   pointer(fCaption),
   WS_VISIBLE or WS_CHILD or BS_GROUPBOX or WS_TABSTOP,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
end;


{ TLabel }

procedure TLabel.Paint;
var R: TRect;
begin
  R := ClientRect;
  with Canvas do begin
    Font.Assign(Self.Font);
    if fTransparent then
      Brush.Style := bsClear else begin
      Brush.Color := Parent.Color;
      FillRect(R);
    end;
    if fVisible then begin
      PrepareText;
      DrawText(Handle, pointer(fCaption), length(fCaption), R,
        DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
    end;
  end;
end;

procedure TLabel.SetCaption(const Value: string);
var Redraw: boolean;
begin
  Redraw := (fCaption<>Value);
  fCaption := Value;
  if Redraw then // avoid flicker
    Invalidate;
end;


{ TBoxStrings }

constructor TBoxStrings.Create(Box: TCustomBox);
begin
  inherited Create;
  fStrings:= TStringList.Create;
  fBox:= Box;
end;

destructor TBoxStrings.Destroy;
begin
  fStrings.Free;
  inherited;
end;

function TBoxStrings.Add(const S: String): Integer;
begin
  fStrings.Add(S);
  result := SendMessage(fBox.Handle, fBox.fAddLineMsg, 0, integer(S));
  if result<0 then
    raise Exception.Create('InsertLineError');
end;

procedure TBoxStrings.Clear;
begin
  fStrings.Clear;
  SendMessage(fBox.Handle, fBox.fResetMsg, 0, 0);
end;

procedure TBoxStrings.SetStrings(StringList: TStrings);
// seems to be very unoptimal, but there's no TStringList.Assign !
var i: Integer;
begin
  if StringList = fStrings then Exit;
  Clear;
  for i := 0 to StringList.Count-1 do
    Add(StringList[i]);
end;

function TBoxStrings.GetItem(n: Integer): string;
begin
  Result := fStrings[n];
end;

procedure TBoxStrings.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..0] of PChar=('Strings');
begin
  case StringIndex(Name,Properties) of
    0 : Reader.ReadStrings(fStrings);
    else inherited;
  end;
end;


{ TCustomBox }

constructor TCustomBox.Create(AOwner: TComponent);
begin
  inherited;
  fItems := TBoxStrings.Create(Self);
  fItemIndex:= -1;
end;

procedure TCustomBox.CreateHandle;
  var i: Integer;
begin
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT,
   PAnsiChar(fWinClass),
   pointer(fCaption),
   fCreateParams,
   fLeft,fTop,fWidth,fHeight+fDropDownHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  for i:= 0 to fItems.Strings.Count - 1 do
    SendMessage(FHandle, fAddLineMsg, 0, integer(fItems.Strings[i]));
  SetItemIndex(fItemIndex);
  if not fVisible then
    ShowWindow(FHandle, SW_HIDE);
end;

//TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable);
const ComboBoxStyleFlag: array[TComboBoxStyle] of Cardinal =
  (CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST, CBS_OWNERDRAWFIXED, CBS_OWNERDRAWVARIABLE);

procedure TComboBox.CreateHandle;
begin
  fWinClass := 'COMBOBOX';
  fCreateParams := WS_VISIBLE or WS_CHILD or WS_TABSTOP or WS_VSCROLL or LBS_NOINTEGRALHEIGHT;
  fAddLineMsg := CB_ADDSTRING;
  fResetMsg := CB_RESETCONTENT;
  fGetIndexMsg:= CB_GETCURSEL;
  fGetCountMsg:= CB_GETCOUNT;
  if fSorted then
    fCreateParams := fCreateParams or CBS_SORT;
  fCreateParams := fCreateParams or ComboBoxStyleFlag[fStyle];
  if fStyle<>csSimple then
    fDropDownHeight:= 400; // maybe, not the best solution?!?
  Color:= Parent.Color;
  inherited;
  SendMessage(fHandle, WM_SETTEXT, 0, integer(fText));
end;

procedure TListBox.CreateHandle;
begin
  fWinClass := 'LISTBOX';
  fCreateParams := WS_VISIBLE or WS_CHILD or WS_TABSTOP or WS_VSCROLL
                or WS_BORDER or LBS_HASSTRINGS or LBS_NOINTEGRALHEIGHT;
  fAddLineMsg := LB_ADDSTRING;
  fResetMsg := LB_RESETCONTENT;
  fGetIndexMsg:= LB_GETCURSEL;
  fGetCountMsg:= LB_GETCOUNT;
  if fSorted then
    fCreateParams := fCreateParams or LBS_SORT;
  Color:= clWhite;
  inherited;
end;

procedure TComboBox.WMCommand(var msg: TWMCommand);
begin
  inherited;
  case msg.NotifyCode of
    CBN_SELCHANGE:
      if Assigned(fOnChange) then
        fOnChange(Self);
  end;
end;

destructor TCustomBox.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TCustomBox.GetItems: TStrings;
begin
  Result:= fItems.Strings;
end;

procedure TCustomBox.SetItems(AItems: TStrings);
begin
  fItems.Strings:= AItems;
end;

function TCustomBox.GetCount: Integer;
begin
  Result := SendMessage(FHandle, fGetCountMsg, 0, 0);
  Assert(Result = fItems.Strings.Count);
end;

function TCustomBox.GetItemIndex: Integer;
begin
  Result := SendMessage(FHandle, fGetIndexMsg, 0, 0);
end;

procedure TCustomBox.SetItemIndex(const Value: Integer);
begin
  fItemIndex := Value;
  if GetItemIndex<>Value then
    SendMessage(FHandle, CB_SETCURSEL, Value, 0);
end;

procedure TCustomBox.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..1] of PChar=('ItemIndex', 'Sorted');
begin
  case StringIndex(Name,Properties) of
    0 : fItemIndex := Reader.IntegerProperty;
    1 : fSorted := Reader.BooleanProperty;
    else inherited;
  end;
end;

procedure TComboBox.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..1] of PChar=('OnChange', 'Style');
begin
  case StringIndex(Name,Properties) of
    0 : TMethod(fOnChange) := FindMethod(Reader);
    1 : Reader.IdentProperty(fStyle, TypeInfo(TComboBoxStyle));
    else inherited;
  end;
end;

function TCustomBox.SubProperty(const Name:string): TPersistent;
const
  TCustomBoxSubProperties:array[0..0] of PChar=('Items');
begin
  case StringIndex(Name, TCustomBoxSubProperties) of
   0 : result := fItems;
   else result := inherited SubProperty(Name);
  end;
end;

function TComboBox.GetText: string;
var i: integer;
begin
  i := SendMessage(fHandle,WM_GETTEXTLENGTH,0,0);
  if i=0 then
    result := '' else begin
    SetLength(result,i);
    SetLength(result,SendMessage(fHandle,WM_GETTEXT,i+1,integer(result)));
  end;
end;

procedure TComboBox.SetText(const Value: string);
begin
  fText := Value;
  if fHandle<>0 then
    SendMessage(fHandle,WM_SETTEXT,0,integer(Value));
end;

function TComboBox.GetDroppedDown: Boolean;
begin
  Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0));
end;

procedure TComboBox.SetDroppedDown(Value: Boolean);
var
  R: TRect;
begin
  SendMessage(Handle, CB_SHOWDROPDOWN, ord(Value), 0);
  R := ClientRect;
  InvalidateRect(Handle, @R, True);
end;

const
  Classes: array[0..8] of TPersistentClass =
    (TLabel, TButton, TEdit, TCheckBox, TRadioButton, TGroupBox, TMemo,
     TComboBox, TListBox);

initialization
  RegisterClasses(Classes);
end.
