unit Forms;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL Forms.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TApplication + TForm
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup
     (no Anchor property, e.g.)
   - MinimizeToTray property for easy tray icon implementation (server-aware)

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

}


interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, StdCtrls;


const
  WM_TRAYICON = WM_USER +1;

type
  TFormBorderStyle =
    (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);
  TPosition =
    (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly,
     poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);
  TWindowState =
    (wsNormal, wsMinimized, wsMaximized);
  TFormStyle =
    (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop);

 TCustomForm = class(TWinControl)
 private
   EOnResize,
   EOnDestroy: TNotifyEvent;
   fPixelsPerInch: integer;
   fTextHeight: integer;
   fOldCreateOrder: boolean;
   fBorderStyle: TFormBorderStyle;
   fPosition: TPosition;
   fClosed,
   fIsTray: boolean;
   fWindowState: TWindowState;
   fFormStyle: TFormStyle;
   fKeyPreview: boolean;
   procedure WMSize(var Msg: TWMSize); message wm_size;
   procedure WMTray(var Msg: TMsg); message WM_TRAYICON;
   procedure WMClose(var Msg); message WM_CLOSE;
   procedure SetWindowState(const Value: TWindowState);
 protected
   procedure Load;
   procedure Read(Reader:TReader);
   procedure ReadProperty(const Name: string; Reader: TReader); override;
   procedure CreateHandle; override;
 public
   MinimizeToTray: boolean;
   constructor Create(AOwner: TComponent); override;
   procedure Show; override;
   procedure ShowModal;
   procedure Refresh;
   procedure Close;
   destructor Destroy; override;
   property WindowState: TWindowState read fWindowState write SetWindowState;
   property KeyPreview: boolean read fKeyPreview write fKeyPreview;
 end;

{$M+} //  we use fields like Form1.Button1, so we need to publish them with $M+
 TForm = class(TCustomForm)
 end;
{$M-}

 TApplication = class(TComponent)
 private
   fNonClientMetrics: TNonClientMetrics;
   fIconHandle: THandle;
 public
   fMainForm: TCustomForm;
   fTerminated: boolean; // public so that can be used/modified from caller
   destructor Destroy; override;
   procedure Initialize;
   procedure CreateForm(InstanceClass: TComponentClass; var Reference);
   procedure Run;
   procedure ProcessMessages;
   procedure Terminate;
   procedure ShowException(E: Exception);
   property IconHandle: THandle read fIconHandle;
   property MainForm: TCustomForm read fMainForm;
 end;

var
  Application: TApplication;


implementation


constructor TCustomForm.Create(AOwner:TComponent);
begin
  inherited;
  fBorderStyle := bsSizeable;
  fPosition := poDefault;
end;

procedure TCustomForm.Close;
begin
  if self=Application.fMainForm then begin
    Free;
    Application.Terminate;
  end else
    fClosed := true;
end;

destructor TCustomForm.Destroy;
begin
  if Assigned(EOnDestroy) then
    EOnDestroy(Self);
  DestroyWindow(fHandle);
  inherited;
end;

procedure TCustomForm.Load;
var Reader: TReader;
begin
  Reader := TReader.Create(ClassName);
  try
    if Reader.Size<>0 then
      if Reader.ReadInteger=ord('T')+ord('P')shl 8+ord('F')shl 16+ord('0')shl 24 then
        Read(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TCustomForm.Read(Reader:TReader);
var
  Flags: TFilerFlags;
  Child: integer;
  sClass, sName: string;
begin
  Reader.ReadPrefix(Flags,Child);
  sClass := Reader.ReadString;
  sName  := Reader.ReadString;
  if not (ffInherited in Flags) then
    fCompName := sName;
  ReadProperties(Reader);
  if Reader.Size-Reader.Position<>0 then
    raise Exception.CreateFmt('ResSize %s',[sName]);
end;

procedure TCustomForm.ReadProperty(const Name: string; Reader: TReader);
const
  TCustomFormProperties: array[0..12] of PChar = (
    'PixelsPerInch',
    'TextHeight',
    'OldCreateOrder',
    'ClientWidth','ClientHeight',
    'BorderStyle',
    'Position',
    'WindowState',
    'OnCreate', 'OnDestroy', 'OnResize',
    'FormStyle',
    'KeyPreview'
  );
var EOnCreate: TNotifyEvent;
begin
  case StringIndex(Name,TCustomFormProperties) of
    0: fPixelsPerInch := Reader.IntegerProperty;
    1: fTextHeight := Reader.IntegerProperty;
    2: fOldCreateOrder := Reader.BooleanProperty;
    3: with Application.fNonClientMetrics do
         fWidth := Reader.IntegerProperty+iBorderWidth*2;
    4: with Application.fNonClientMetrics do
         fHeight := Reader.IntegerProperty+iCaptionHeight+iBorderWidth*4;
    5: Reader.IdentProperty(fBorderStyle,TypeInfo(TFormBorderStyle));
    6: Reader.IdentProperty(fPosition,TypeInfo(TPosition));
    7: Reader.IdentProperty(fWindowState,TypeInfo(TWindowState));
    8: begin
      TMethod(EOnCreate) := FindMethod(Reader);
      if Assigned(EOnCreate) then
        EOnCreate(self); // we need this event triggered now
    end;
    9: TMethod(EOnDestroy) := FindMethod(Reader);
    10: TMethod(EOnResize) := FindMethod(Reader);
    11: Reader.IdentProperty(fFormStyle,TypeInfo(TFormStyle));
    12: fKeyPreview := Reader.BooleanProperty;
   else inherited;
  end;
end;

procedure TCustomForm.Show;
var i: integer;
begin // don't call inherited Show, which doesn't handle WindowState properly
  HandleNeeded;
  for i := 0 to fControls.Count-1 do
    with TWinControl(fControls.List[i]) do
      if Visible then 
        Show;
  WindowState := fWindowState;
  if Assigned(EOnShow) then
    EOnShow(Self);
end;

procedure TCustomForm.CreateHandle;
var
  style, exstyle: cardinal;
begin
  case fBorderStyle of
    bsNone    : Style := WS_POPUP;
    bsSingle  : Style := WS_CAPTION or WS_BORDER or WS_SYSMENU or WS_MINIMIZEBOX;
    bsSizeable: Style := WS_OVERLAPPEDWINDOW;
    bsDialog  : Style := WS_DLGFRAME; // or WS_SYSMENU;
    else        Style := 0;
  end;
  case fBorderStyle of
    bsDialog      : exstyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
    bsToolWindow,
    bsSizeToolWin : ExStyle := WS_EX_TOOLWINDOW;
    else            exstyle := 0;
  end;
  case fPosition of
    poDefault: begin
      fLeft := integer(CW_USEDEFAULT);
      fTop := integer(CW_USEDEFAULT);
    end;
    poScreenCenter, poDesktopCenter: begin
      fLeft := (GetSystemMetrics(SM_CXSCREEN)-fWidth) div 2;
      fTop := (GetSystemMetrics(SM_CYSCREEN)-fHeight) div 2;
    end;
  end;
  Style := style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Handle := CreateWindowEx(
   exstyle,
   'LFORM',
   pointer(fCaption),
   style,
   fLeft,fTop,fWidth,fHeight,
   0,0,
   hInstance,
   nil
  );
  case fFormStyle of  // no MDI support yet
    fsStayOnTop: SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
  end;
end;

function Shell_NotifyIcon(dwMessage: DWORD; lpData: pointer): BOOL; stdcall;
  external 'shell32.dll' name 'Shell_NotifyIconA';

procedure TCustomForm.WMSize(var Msg: TWMSize);
const
  NIF_MESSAGE = $00000001;
  NIF_ICON    = $00000002;
  NIF_TIP     = $00000004;
  NIM_ADD     = $00000000;
  NIM_MODIFY  = $00000001;
  NIM_DELETE  = $00000002;
var Info: record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..63] of AnsiChar;
  end;
begin
  inherited; // L_G: otherwise fHeight/fWidth remains unchaged and WMEraseBkGnd don't fill larger area
  if MinimizeToTray then
  case Msg.SizeType of
   SIZEICONIC: begin
     ShowWindow(Handle, SW_HIDE);
     fillchar(Info,SizeOf(Info),0);
     Info.cbSize := SizeOf(Info);
     Info.Wnd    := Handle;
     Info.uID    := 1;
     StrLCopy(Info.szTip,pointer(Caption),63);
     Info.hIcon  := Application.IconHandle;
     Info.uCallbackMessage := WM_TRAYICON;
     Info.uFlags := NIF_TIP or NIF_ICON or NIF_MESSAGE;
     Shell_NotifyIcon(NIM_ADD,@Info);
     fIsTray := true;
     fVisible := false;
     fWindowState := wsMinimized;
   end;
   SIZE_RESTORED:
     if fIsTray then begin
       Info.cbSize := SizeOf(Info);
       Info.Wnd    := Handle;
       Info.uID    := 1;
       Shell_NotifyIcon(NIM_DELETE,@Info);
       fIsTray := false;
       PostMessage(Handle,WM_TRAYICON,0,0); // will call SetForegroundWindow()
     end;
  end;
  if not fIsTray and Assigned(EOnResize) then
     EOnResize(self);
end;

procedure TCustomForm.WMTray(var Msg: TMsg);
begin
  if Msg.wParam=WM_LBUTTONDOWN then begin
    ShowWindow(Handle,SW_RESTORE);
    fVisible := true;
    fWindowState := wsNormal;
  end else
  if Msg.wParam=0 then
    SetForegroundWindow(Handle);
end;


destructor TApplication.Destroy;
begin
  UnregisterClass('LFORM',hInstance);
  inherited;
end;

procedure TApplication.Initialize;
var WndClass: TWndClass;
begin
  fTerminated := False;
  fIconHandle := LoadIcon(hInstance,'MAINICON');
  FillChar(WndClass,SizeOf(WndClass),0);
  WndClass.hInstance := hInstance; // hInstance in System (D2) or SysInit (D5) :(
  with WndClass do begin
    Style := CS_VREDRAW or CS_HREDRAW;
    lpfnWndProc := @DefWindowProc;
    hIcon := fIconHandle;
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := 0; // GetStockObject(LTGRAY_BRUSH); --> WM_ERASBKGND
    lpszClassName := 'LFORM';
  end;
  RegisterClass(WndClass);
  fNonClientMetrics.cbSize := sizeof(fNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @fNonClientMetrics, 0);
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
var Instance: TComponent;
begin
  Instance := TComponent(InstanceClass.NewInstance);
  try
    TComponent(Reference) := Instance;
    Instance.Create(nil);
    if Instance.InheritsFrom(TCustomForm) then begin
      TCustomForm(Instance).Load;
      if fMainForm=nil then // Main Form is the first initialized form
        fMainForm := TCustomForm(Instance);
    end;
  except
    TComponent(Reference) := nil;
    raise;
  end;
end;

procedure TApplication.Run;
begin
  if fMainForm=nil then exit;
  fMainForm.Show;
  repeat
    ProcessMessages;
    if fTerminated then break;
    WaitMessage;
  until fTerminated;
end;

procedure TApplication.ProcessMessages;
var msg: TMsg;
begin
  while PeekMessage(msg,0,0,0,PM_REMOVE) do
    if (Msg.Message=WM_QUIT) and (self<>nil) then
      fTerminated := True else begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end;

procedure TApplication.Terminate;
begin
  fTerminated := True;
end;

procedure TCustomForm.Refresh;
begin
  HandleNeeded;
  ShowWindow(Handle,SW_HIDE);
  Application.ProcessMessages;
  ShowWindow(Handle,SW_NORMAL);
  Application.ProcessMessages;
end;

procedure TCustomForm.ShowModal;
begin
  HandleNeeded;
  Show;
  repeat
    Application.ProcessMessages;
    if fClosed then break;
    WaitMessage;
  until fClosed;
  fClosed := false;
  Hide;
end;

procedure TCustomForm.WMClose;
begin
  Close;
end;

procedure TCustomForm.SetWindowState(const Value: TWindowState);
const
  ShowCommands: array[TWindowState] of integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);
begin
  fWindowState := Value;
  ShowWindow(fHandle,ShowCommands[fWindowState]);
end;

procedure TApplication.ShowException(E: Exception);
var H: integer;
begin
  if Application.fMainForm=nil then
    H := 0 else
    H := Application.fMainForm.Handle;
  MessageBox(H,pointer(E.Message),nil,MB_OK or MB_ICONERROR);
end;

initialization
  Application := TApplication.Create(nil);

finalization
  Application.Free;
end.