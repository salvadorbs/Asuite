unit Controls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL Controls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TControl+TCustomControl+TGraphicControl+TWinControl
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup

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

  * New TWinControl.Enabled property. Both Enabled and Visible loads from DFM
}

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics;

type
  TShiftState = set of (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TKeyPressEvent = procedure (Sender: TObject; var Key: Char) of object;
  TKeyEvent = procedure (Sender: TObject; var Key: Word; Shift: TShiftState) of object;
  TMouseEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TControl = class(TComponent)
  private
    EOnClick: TNotifyEvent;
  protected
    procedure ReadProperty(const Name:string; Reader: TReader); override;
    function FindMethod(Reader: TReader): TMethod;
  public
    Tag: integer;
    property OnClick: TNotifyEvent read EOnClick write EOnClick;
  end;

  TWinControl = class;

  TCustomControlClass = class of TCustomControl;

  TCustomControl = class(TControl)
  private
    fParent: TWinControl;
    fCanvas: TCanvas;
    fFont: TFont;
    fColor: integer;
    fParentFont: boolean;
    procedure SetVisible(const Value: boolean);
  protected
    EOnPaint: TNotifyEvent;
    EOnShow: TNotifyEvent;
    fCaption: string;
    fLeft, fTop: integer;
    fWidth, fHeight: integer;
    fVisible,
    fTransparent: boolean;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    function SubProperty(const Name: string): TPersistent; override;
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetCanvas: TCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Invalidate;
    function ClientRect: TRect; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    property Font: TFont read GetFont write SetFont;
    property Canvas: TCanvas read GetCanvas;
    property Parent: TWinControl read fParent;
    property Color: integer read fColor write fColor;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    property Left: integer read fLeft;
    property Top: integer read fTop;
    property Transparent: boolean read fTransparent write fTransparent;
    property Caption: string read fCaption;
    property Visible: boolean read fVisible write SetVisible;
  end;

  TGraphicControl = class(TCustomControl)
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TControlStyle = set of (csAcceptsControl, csCaptureMouse, csClickEvents,
    csFramed, csSetCaption, csOpaque, cdDoubleClicks);
  TControlState = set of (csLButtonDown, csClicked, csPalette, csReadingState,
    csAlignmentNeeded, csFocusing, csCreating, csPaintCopy, csCustomPaint,
    csDestroyingHandle, csDocking);
  TBorderStyle = (bsNone,bsSingle);

  TWinControlClass = class of TWinControl;

  TWinControl = class(TCustomControl)
  private
    fGraphics: TList;
    fEnabled: Boolean;
    procedure WMSize(var Msg: TWMSize); message wm_size;
    procedure WMLButtonDown(var msg: TWMLButtonDown); message wm_lbuttondown;
    procedure WMLButtonUp(var msg: TWMLButtonUp); message wm_lbuttonup;
    procedure WMDblClick(var msg: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var msg: TWMMouse); message wm_rbuttondown;
    procedure WMRButtonUp(var msg: TWMMouse); message wm_rbuttonup;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message wm_erasebkgnd;
    procedure WMDestroy(var Msg: TWMDestroy); message wm_destroy;
    procedure WMPaint(var Msg: TWMPaint); message wm_paint;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMKeyDown(var Msg: TWMKey); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TWMKey); message WM_KEYUP;
    procedure SetCaption(const Value: string);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    procedure DefaultHandler(var Message); override;
  protected
    EOnKeyPress: TKeyPressEvent;
    EOnKeyDown: TKeyEvent;
    EOnKeyUp: TKeyEvent;
    EOnMouseDown: TMouseEvent;
    EOnMouseUp: TMouseEvent;
    EOnDblClick: TNotifyEvent;
    fHandle: THandle;
    fOldProc: integer;
    fTabOrder: integer;
    fControlStyle: TControlStyle;
    fControlState: TControlState;
    fControls: TList;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    procedure SetParentComponent(Value: TComponent); override;
    function GetParentComponent: TComponent; override;
    procedure HandleNeeded;
    procedure CreateHandle; virtual; abstract;
    procedure SetHandle(Value: THandle);
    procedure SetText(const Value: string); virtual;
    procedure AddChild(AChild: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ClientRect: TRect; override;
    procedure Paint; override;
    procedure Show; override;
    procedure Hide; override;
    procedure SetFocus;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    property Handle: THandle read fHandle write SetHandle;
    property Caption: string read fCaption write SetCaption;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Controls: TList read fControls;
    property OnKeyPress: TKeyPressEvent read EOnKeyPress write EOnKeyPress;
    property OnKeyDown: TKeyEvent read EOnKeyDown write EOnKeyDown;
    property OnKeyUp: TKeyEvent read EOnKeyUp write EOnKeyUp;
    property OnMouseDown: TMouseEvent read EOnMouseDown write EOnMouseDown;
    property OnMouseUp: TMouseEvent read EOnMouseUp write EOnMouseUp;
    property OnDblClick: TNotifyEvent read EOnDblClick write EOnDblClick;
  end;


implementation

uses Forms;

function KeysToShiftState(Keys: Word): TShiftState;
begin
  byte(Result) := 0;
  if Keys and MK_SHIFT<>0 then Include(Result, ssShift);
  if Keys and MK_CONTROL<>0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON<>0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON<>0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON<>0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function KeyDataToShiftState(KeyData: Longint): TShiftState;
const
  AltMask = $20000000;
begin
  byte(Result) := 0;
  if GetKeyState(VK_SHIFT)<0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL)<0 then Include(Result, ssCtrl);
  if KeyData and AltMask<>0 then Include(Result, ssAlt);
end;

function GetParentForm(Control: TCustomControl): TCustomForm;
begin
  while Control.FParent<>nil do
    Control := Control.Parent;
  if Control.InheritsFrom(TCustomForm) then
    Result := TCustomForm(Control) else
    Result := nil;
end;

function WndProc(Hwnd,Msg,wParam,lParam: integer): integer; stdcall;
var obj: TObject;
    dsp: TMessage;
begin
  obj := TObject(GetWindowLong(HWnd,GWL_USERDATA)); // faster than GetProp()
  if not Assigned(obj) then
    result := DefWindowProc(HWnd,Msg,wParam,lParam) else begin
    dsp.msg := msg;
    dsp.wParam := WParam;
    dsp.lParam := lParam;
    dsp.result := 0;
    obj.Dispatch(dsp);
    result := dsp.result;
  end;
end;

procedure TControl.ReadProperty(const Name: string; Reader:TReader);
const
  TControlProperties:array[0..1] of PChar=(
   'OnClick','Tag'
  );
begin
  case StringIndex(Name,TControlProperties) of
    0 : TMethod(EOnClick) := FindMethod(Reader);
    1 : Tag := Reader.IntegerProperty;
    else inherited;
  end;
end;

function TControl.FindMethod(Reader: TReader): TMethod;
var AComponent: TComponent;
    Name: shortstring;
begin
  if Reader.ReadValueType in [vaString,vaIdent] then begin
    Name := Reader.ReadShortString;
    AComponent := self;
    while AComponent<>nil do begin
      result.Data := AComponent;
      result.Code := AComponent.MethodAddress(Name);
      if result.Code<>nil then
        exit;
      AComponent := AComponent.Owner;
    end;
  end;
  raise EClassesError.Create('method?');
end;

constructor TCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  fParentFont := true; // default value
  fVisible := true;
end;

destructor TCustomControl.Destroy;
begin
  fFont.Free;
  fCanvas.Free;
  inherited;
end;

function TCustomControl.GetCanvas: TCanvas;
begin
  if fCanvas=nil then
    fCanvas := TCanvas.Create;
  result := fCanvas;
end;

procedure TCustomControl.ReadProperty(const Name: string; Reader: TReader);
const
  TWinControlProperties: array[0..10] of PChar=(
   'Left','Top',
   'Width','Height',
   'Color',
   'Transparent',
   'Caption',
   'OnPaint',
   'ParentFont',
   'OnShow',
   'Visible'
  );
begin
  case StringIndex(Name,TWinControlProperties) of
    0 : fLeft := Reader.IntegerProperty;
    1 : fTop := Reader.IntegerProperty;
    2 : fWidth := Reader.IntegerProperty;
    3 : fHeight := Reader.IntegerProperty;
    4 : fColor := Reader.ColorProperty;
    5 : fTransparent := Reader.BooleanProperty;
    6 : fCaption := Reader.StringProperty;
    7 : TMethod(EOnPaint) := FindMethod(Reader);
    8 : fParentFont := Reader.BooleanProperty;
    9 : TMethod(EOnShow) := FindMethod(Reader);
    10: fVisible := Reader.BooleanProperty;
    else inherited;
  end;
end;

function TCustomControl.SubProperty(const Name:string): TPersistent;
const
  TControlSubProperties:array[0..0] of PChar=(
   'Font'
  );
begin
  case StringIndex(Name,TControlSubProperties) of
   0 : begin
     if fFont=nil then
       fFont := TFont.Create;
     result := fFont;
   end;
   else result := nil;
  end;
end;

function TCustomControl.GetFont: TFont;
begin
  if fFont=nil then begin
    if fParentFont and (Parent<>nil) then begin
      result := Parent.Font;
      exit;
    end;
    fFont := TFont.Create;
  end;
  result := fFont;
end;

procedure TCustomControl.SetFont(Value: TFont);
begin
  fParentFont := false; // to create a custom font
  Font.Assign(Value);
end;

procedure TCustomControl.Paint;
begin
  if not fTransparent then
   with Canvas do
    if Assigned(EOnPaint) then EOnPaint(Self);
end;

function TCustomControl.ClientRect: TRect;
begin
  result.Left := fLeft;
  result.Right := result.Left+fWidth;
  result.Top := fTop;
  result.Bottom := result.Top+fHeight;
end;

procedure TCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  fLeft := aLeft;
  fTop := aTop;
  fWidth := aWidth;
  fHeight := aHeight;
end;

procedure TCustomControl.Invalidate;
var R: TRect;
begin
  if Parent=nil then
    exit;
  R := ClientRect;
  InvalidateRect(Parent.Handle,@R,false);
end;

constructor TGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  fVisible := true; // default value (TWinControl.Show expects fVisible=false)
end;

procedure TGraphicControl.SetParentComponent(Value: TComponent);
begin
  while (Value<>nil) and not Value.InheritsFrom(TWinControl) do
    Value := Value.ParentComponent;
  if Value<>nil then begin
    fParent := TWinControl(Value);
    Font.Assign(fParent.Font);
    fParent.fGraphics.Add(Self);
  end;
end;

constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited;
  fControls := TList.Create;
  fGraphics := TList.Create;
  fEnabled:= True;
end;

destructor TWinControl.Destroy;
begin
  if fColor<>0 then
    DeleteObject(fColor);
  Handle := 0;
  fControls.Free;
  fGraphics.Free;
  inherited;
end;

procedure TWinControl.ReadProperty(const Name:string; Reader:TReader);
const
  TWinControlProperties:array[0..8] of PChar=(
   'Text',
   'TabOrder',
   'OnKeyPress', 'OnKeyDown', 'OnKeyUp',
   'OnMouseDown', 'OnMouseUp', 'OnDblClick',
   'Enabled'
  );
begin
  case StringIndex(Name,TWinControlProperties) of
    0 : SetText(Reader.StringProperty);
    1 : fTabOrder := Reader.IntegerProperty;
    2 : TMethod(EOnKeyPress)  := FindMethod(Reader);
    3 : TMethod(EOnKeyDown)   := FindMethod(Reader);
    4 : TMethod(EOnKeyUp)     := FindMethod(Reader);
    5 : TMethod(EOnMouseDown) := FindMethod(Reader);
    6 : TMethod(EOnMouseUp)   := FindMethod(Reader);
    7 : TMethod(EOnDblClick)  := FindMethod(Reader);
    8 : Enabled := Reader.BooleanProperty;
   else inherited;
  end;
end;

procedure TWinControl.SetParentComponent(Value:TComponent);
begin
  while (Value<>nil) and not(Value.InheritsFrom(TWinControl)) do
    Value := Value.ParentComponent;
  if Value<>nil then begin
    fParent := TWinControl(Value);
    Canvas.Font.Assign(fParent.Font);
    fParent.AddChild(Self);
  end;
end;

function TWinControl.GetParentComponent:TComponent;
begin
  result := fParent;
end;

procedure TWinControl.HandleNeeded;
begin
  if self=nil then exit;
  if fParent<>nil then
    fParent.HandleNeeded;
  if fHandle=0 then
    CreateHandle;
end;

procedure TWinControl.SetHandle(Value: THandle);
begin
  if fHandle<>0 then begin
    SetWindowLong(fHandle,GWL_WNDPROC,fOldProc);
    DestroyWindow(fHandle);
  end;
  fHandle := Value;
  if fHandle<>0 then begin
    fOldProc := GetWindowLong(fHandle,GWL_WNDPROC);
    SetWindowLong(fHandle,GWL_USERDATA,integer(self)); // faster than SetProp()
    SetWindowLong(fHandle,GWL_WNDPROC,integer(@WndProc));
    SendMessage(fHandle,WM_SETFONT,integer(Font.Handle),0);
    SetEnabled(FEnabled);
  end;
end;

procedure TWinControl.SetText(const Value:string);
begin
  fCaption := Value;
end;

procedure TWinControl.AddChild(AChild:TWinControl);
begin
  fControls.Add(AChild);
end;

procedure TWinControl.WMSize(var Msg: TWMSize);
begin
  inherited;
  fWidth := msg.Width;
  fHeight := msg.Height;
end;

procedure TWinControl.WMLButtonDown(var msg: TWMLButtonDown);
begin
  inherited;
  Include(fControlState,csClicked);
  if Assigned(EOnMouseDown) then
    EOnMouseDown(Self, mbLeft, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMLButtonUp(var msg: TWMLButtonUp);
begin
  inherited;
  if Assigned(EOnMouseUp) then
    EOnMouseUp(Self, mbLeft, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);

  //if csClicked in fControlState then begin
    if Assigned(EOnClick) then
      if (cardinal(msg.XPos)<=cardinal(fWidth)) and
         (cardinal(msg.YPos)<=cardinal(fHeight)) then
        EOnClick(Self);
    Exclude(fControlState,csClicked);
  //end;
end;

procedure TWinControl.WMDblClick(var msg: TWMMouse);
begin
  inherited;
  if Assigned(EOnDblClick) then
    EOnDblClick(Self);
end;

procedure TWinControl.WMRButtonDown(var msg: TWMRButtonDown);
begin
  inherited;
  if Assigned(EOnMouseDown) then
    EOnMouseDown(Self, mbRight, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMRButtonUp(var msg: TWMRButtonUp);
begin
  inherited;
  if Assigned(EOnMouseUp) then
    EOnMouseUp(Self, mbRight, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMChar(var Msg: TWMChar);
var Key: Char; Form: TCustomForm;
begin
  inherited;
  Key := chr(Msg.CharCode);

  Form := GetParentForm(Self);
  if (Form<>nil) and (Form<>Self) and Form.KeyPreview and Assigned(Form.EOnKeyPress) then
  begin
    Form.EOnKeyPress(Self, Key);
    if Key = #0 then Exit;
  end;

  if Assigned(EOnKeyPress) then
    EOnKeyPress(Self, Key);
end;

procedure TWinControl.WMKeyDown(var Msg: TWMKey);
var Key: word; Shift: TShiftState; Form: TCustomForm;
begin
  inherited;
  Key := Msg.CharCode;
  Shift := KeyDataToShiftState(Msg.KeyData);

  Form := GetParentForm(Self);
  if (Form<>nil) and (Form<>Self) and Form.KeyPreview and Assigned(Form.EOnKeyDown) then
  begin
    Form.EOnKeyDown(Self, Key, Shift);
    if Key = 0 then Exit;
  end;

  if Assigned(EOnKeyDown) then
    EOnKeyDown(Self, Key, Shift);
end;

procedure TWinControl.WMKeyUp(var Msg: TWMKey);
var Key: word; Shift: TShiftState; Form: TCustomForm;
begin
  inherited;
  Key := Msg.CharCode;
  Shift := KeyDataToShiftState(Msg.KeyData);

  Form := GetParentForm(Self);
  if (Form<>nil) and (Form<>Self) and Form.KeyPreview and Assigned(Form.EOnKeyUp) then
  begin
    Form.EOnKeyUp(Self, Key, Shift);
    if key = 0 then Exit;
  end;

  if Assigned(EOnKeyUp) then
    EOnKeyUp(Self, Key, Shift);
end;

procedure TWinControl.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  msg.result := 1;
  if not fTransparent then
  with Canvas do begin
    Handle := Msg.DC;
    Brush.Color := Self.Color;
    FillRect(Rect(0,0,fWidth,fHeight));
  end;
end;

procedure TWinControl.WMPaint(var Msg: TWMPaint);
begin
  with Canvas do begin
    Handle := Msg.DC;
    if Handle=0 then
      Handle := GetDC(self.fHandle);
    Paint;
    if Msg.DC=0 then
      ReleaseDC(self.fHandle,Handle);
  end;
  inherited;
end;

procedure TWinControl.WMDestroy(var Msg: TWMDestroy);
begin
  inherited;
  PostQuitMessage(0);
end;

procedure TWinControl.DefaultHandler(var Message);
begin
  with TMessage(Message) do
    result := CallWindowProc(pointer(fOldProc),fHandle,Msg,wParam,lParam)
end;

procedure TWinControl.Paint;
var H, i: integer;
begin
  inherited;
  if fGraphics.Count=0 then exit;
  H := Self.Canvas.Handle;
  for i := 0 to fGraphics.Count-1 do
    with TGraphicControl(fGraphics.List[i]) do begin
      Canvas.Handle := H;
      Paint;
    end;
end;

procedure TWinControl.SetFocus;
begin
  Windows.SetFocus(fHandle);
end;

procedure TWinControl.SetCaption(const Value: string);
begin
  fCaption := Value;
  if fHandle<>0 then
    SendMessage(fHandle,WM_SETTEXT,0,integer(Value));
end;

procedure TWinControl.Hide;
begin
  HandleNeeded;
  fVisible := false;
  ShowWindow(fHandle,SW_HIDE);
end;

procedure TWinControl.Show;
var i: integer;
begin
  HandleNeeded;
  for i := 0 to fControls.Count-1 do
    with TWinControl(fControls.List[i]) do
      if Visible then
        Show;
  ShowWindow(fHandle,SW_SHOW);
  if Assigned(EOnShow) then
    EOnShow(Self);
end;

function TWinControl.ClientRect: TRect;
begin
  GetClientRect(Handle,result);
end;

procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited;
  MoveWindow(Handle,ALeft,Atop,AWidth,AHeight,false);
end;

function TWinControl.GetEnabled: boolean;
begin
  if fHandle<>0 then // enabled by default
    fEnabled := IsWindowEnabled(fHandle);
  result := fEnabled;
end;

procedure TWinControl.SetEnabled(const Value: boolean);
begin
  if GetEnabled = Value then Exit;
  FEnabled := Value;
  if fHandle<>0 then
  begin
    EnableWindow(fHandle, Value);
    Invalidate; // necessary for Graphic controls
  end;
end;

procedure TCustomControl.Hide;
begin
  fVisible := false;
end;

procedure TCustomControl.Show;
begin
  fVisible := true;
end;

procedure TCustomControl.SetVisible(const Value: boolean);
begin
  if Value=fVisible then
    exit;
  fVisible := Value;
  if Value then
    Show else
    Hide;
end;

end.
