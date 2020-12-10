{%MainUnit ../hotkeyCaptureList.pas}

uses
  X, XLib, Gdk2, Gdk2x, Gtk2Proc, KeySym;

type

  { TGTK2HotkeyManager }

  TGTK2HotkeyManager = class(TBaseHotkeyManager)
  private
    FRoot: PGdkWindow;
    FDisplay: PDisplay;
  protected
    function DoRegister(Shortcut: TShortCut): Boolean; override;
    function DoUnregister(Shortcut: TShortCut): Boolean; override;
  public
    constructor Create; override;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; override;
  end;       

  THotkeyCaptureImpl = TGTK2HotkeyManager;

{
  X Key Modifiers:

  Mask        | Value | Key
  ------------+-------+------------
  ShiftMask   |     1 | Shift
  LockMask    |     2 | Caps Lock
  ControlMask |     4 | Ctrl
  Mod1Mask    |     8 | Alt
  Mod2Mask    |    16 | Num Lock
  Mod3Mask    |    32 | Scroll Lock
  Mod4Mask    |    64 | Windows
}

const
  AltMask = Mod1Mask;
  SuperMask = Mod4Mask;
  ModifiersMask = ShiftMask or AltMask or ControlMask or SuperMask;
  CapLock = LockMask;
  NumLock = Mod2Mask;
  NotLock = Integer(not (CapLock or NumLock));

function ShiftToMod(ShiftState: TShiftState): Integer;
begin
  Result := 0;
  if ssShift in ShiftState then
    Result := Result or ShiftMask;
  if ssAlt in ShiftState then
    Result := Result or AltMask;
  if ssCtrl in ShiftState then
    Result := Result or ControlMask;
  if (ssSuper in ShiftState) or (ssMeta in ShiftState) then
    Result := Result or SuperMask;
end;

function ModToShift(Modifiers: Integer): TShiftState;
begin
  Result := [];
  if ShiftMask and Modifiers > 0 then
    Include(Result, ssShift);
  if AltMask and Modifiers > 0 then
    Include(Result, ssAlt);
  if ControlMask and Modifiers > 0 then
    Include(Result, ssCtrl);
  if (SuperMask and Modifiers > 0) then
    Include(Result, ssMeta);
end;

function KeyToSym(Key: Word): TKeySym;
begin
  case Key of
    VK_TAB: Result := XK_TAB;
    VK_CLEAR: Result := XK_CLEAR;
    VK_RETURN: Result := XK_RETURN;
    VK_MENU: Result := XK_MENU;
    VK_ESCAPE: Result := XK_ESCAPE;
    VK_PAUSE: Result := XK_PAUSE;
    VK_SPACE: Result := XK_SPACE;
    VK_PRIOR: Result := XK_PRIOR;
    VK_NEXT: Result := XK_NEXT;
    VK_END: Result := XK_END;
    VK_HOME: Result := XK_HOME;
    VK_LEFT: Result := XK_LEFT;
    VK_UP: Result := XK_UP;
    VK_RIGHT: Result := XK_RIGHT;
    VK_DOWN: Result := XK_DOWN;
    VK_SELECT: Result := XK_SELECT;
    VK_EXECUTE: Result := XK_EXECUTE;
    VK_SNAPSHOT: Result := XK_PRINT;
    VK_INSERT: Result := XK_INSERT;
    VK_DELETE: Result := XK_DELETE;
    VK_HELP: Result := XK_HELP;
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;
    VK_A: Result := XK_A;
    VK_B: Result := XK_B;
    VK_C: Result := XK_C;
    VK_D: Result := XK_D;
    VK_E: Result := XK_E;
    VK_F: Result := XK_F;
    VK_G: Result := XK_G;
    VK_H: Result := XK_H;
    VK_I: Result := XK_I;
    VK_J: Result := XK_J;
    VK_K: Result := XK_K;
    VK_L: Result := XK_L;
    VK_M: Result := XK_M;
    VK_N: Result := XK_N;
    VK_O: Result := XK_O;
    VK_P: Result := XK_P;
    VK_Q: Result := XK_Q;
    VK_R: Result := XK_R;
    VK_S: Result := XK_S;
    VK_T: Result := XK_T;
    VK_U: Result := XK_U;
    VK_V: Result := XK_V;
    VK_W: Result := XK_W;
    VK_X: Result := XK_X;
    VK_Y: Result := XK_Y;
    VK_Z: Result := XK_Z;
    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_MULTIPLY;
    VK_ADD: Result := XK_KP_ADD;
    VK_SEPARATOR: Result := XK_KP_SEPARATOR;
    VK_SUBTRACT: Result := XK_KP_SUBTRACT;
    VK_DECIMAL: Result := XK_KP_DECIMAL;
    VK_DIVIDE: Result := XK_KP_DIVIDE;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_LCL_EQUAL: Result := XK_EQUAL;
    VK_LCL_COMMA: Result := XK_COMMA;
    VK_LCL_POINT: Result := XK_PERIOD;
    VK_LCL_SLASH: Result := XK_SLASH;
    VK_LCL_SEMI_COMMA: Result := XK_SEMICOLON;
    VK_LCL_MINUS: Result := XK_MINUS;
    VK_LCL_OPEN_BRAKET: Result := XK_BRACKETLEFT;
    VK_LCL_CLOSE_BRAKET: Result := XK_BRACKETRIGHT;
    VK_LCL_BACKSLASH: Result := XK_BACKSLASH;
    VK_LCL_TILDE: Result := XK_GRAVE;
    VK_LCL_QUOTE: Result := XK_SINGLELOWQUOTEMARK;
  else
    Result := 0;
  end;
end;

function SymToKey(Sym: TKeySym): Word;
begin
  case Sym of
    XK_TAB: Result := VK_TAB;
    XK_CLEAR: Result := VK_CLEAR;
    XK_RETURN: Result := VK_RETURN;
    XK_MENU: Result := VK_MENU;
    XK_ESCAPE: Result := VK_ESCAPE;
    XK_PAUSE: Result := VK_PAUSE;
    XK_SPACE: Result := VK_SPACE;
    XK_PRIOR: Result := VK_PRIOR;
    XK_NEXT: Result := VK_NEXT;
    XK_END: Result := VK_END;
    XK_HOME: Result := VK_HOME;
    XK_LEFT: Result := VK_LEFT;
    XK_UP: Result := VK_UP;
    XK_RIGHT: Result := VK_RIGHT;
    XK_DOWN: Result := VK_DOWN;
    XK_SELECT: Result := VK_SELECT;
    XK_EXECUTE: Result := VK_EXECUTE;
    XK_PRINT: Result := VK_SNAPSHOT;
    XK_INSERT: Result := VK_INSERT;
    XK_DELETE: Result := VK_DELETE;
    XK_HELP: Result := VK_HELP;
    XK_0: Result := VK_0;
    XK_1: Result := VK_1;
    XK_2: Result := VK_2;
    XK_3: Result := VK_3;
    XK_4: Result := VK_4;
    XK_5: Result := VK_5;
    XK_6: Result := VK_6;
    XK_7: Result := VK_7;
    XK_8: Result := VK_8;
    XK_9: Result := VK_9;
    XK_A: Result := VK_A;
    XK_B: Result := VK_B;
    XK_C: Result := VK_C;
    XK_D: Result := VK_D;
    XK_E: Result := VK_E;
    XK_F: Result := VK_F;
    XK_G: Result := VK_G;
    XK_H: Result := VK_H;
    XK_I: Result := VK_I;
    XK_J: Result := VK_J;
    XK_K: Result := VK_K;
    XK_L: Result := VK_L;
    XK_M: Result := VK_M;
    XK_N: Result := VK_N;
    XK_O: Result := VK_O;
    XK_P: Result := VK_P;
    XK_Q: Result := VK_Q;
    XK_R: Result := VK_R;
    XK_S: Result := VK_S;
    XK_T: Result := VK_T;
    XK_U: Result := VK_U;
    XK_V: Result := VK_V;
    XK_W: Result := VK_W;
    XK_X: Result := VK_X;
    XK_Y: Result := VK_Y;
    XK_Z: Result := VK_Z;
    XK_KP_0: Result := VK_NUMPAD0;
    XK_KP_1: Result := VK_NUMPAD1;
    XK_KP_2: Result := VK_NUMPAD2;
    XK_KP_3: Result := VK_NUMPAD3;
    XK_KP_4: Result := VK_NUMPAD4;
    XK_KP_5: Result := VK_NUMPAD5;
    XK_KP_6: Result := VK_NUMPAD6;
    XK_KP_7: Result := VK_NUMPAD7;
    XK_KP_8: Result := VK_NUMPAD8;
    XK_KP_9: Result := VK_NUMPAD9;
    XK_KP_MULTIPLY: Result := VK_MULTIPLY;
    XK_KP_ADD: Result := VK_ADD;
    XK_KP_SEPARATOR: Result := VK_SEPARATOR;
    XK_KP_SUBTRACT: Result := VK_SUBTRACT;
    XK_KP_DECIMAL: Result := VK_DECIMAL;
    XK_KP_DIVIDE: Result := VK_DIVIDE;
    XK_F1: Result := VK_F1;
    XK_F2: Result := VK_F2;
    XK_F3: Result := VK_F3;
    XK_F4: Result := VK_F4;
    XK_F5: Result := VK_F5;
    XK_F6: Result := VK_F6;
    XK_F7: Result := VK_F7;
    XK_F8: Result := VK_F8;
    XK_F9: Result := VK_F9;
    XK_F10: Result := VK_F10;
    XK_F11: Result := VK_F11;
    XK_F12: Result := VK_F12;
    XK_EQUAL: Result := VK_LCL_EQUAL;
    XK_COMMA: Result := VK_LCL_COMMA;
    XK_PERIOD: Result := VK_LCL_POINT;
    XK_SLASH: Result := VK_LCL_SLASH;
    XK_SEMICOLON: Result := VK_LCL_SEMI_COMMA;
    XK_MINUS: Result := VK_LCL_MINUS;
    XK_BRACKETLEFT: Result := VK_LCL_OPEN_BRAKET;
    XK_BRACKETRIGHT: Result := VK_LCL_CLOSE_BRAKET;
    XK_BACKSLASH: Result := VK_LCL_BACKSLASH;
    XK_GRAVE: Result := VK_LCL_TILDE;
    XK_SINGLELOWQUOTEMARK: Result := VK_LCL_QUOTE;
  else
    Result := 0;
  end;
end;

function FilterKeys(AnyEvent: PXAnyEvent; Event: PGdkEvent; Data: Pointer): TGdkFilterReturn; cdecl;
var
  Capture: TGTK2HotkeyManager absolute Data;
  KeyEvent: PXKeyEvent absolute AnyEvent;
  Sym: TKeySym;
  Key: Word;
  ShiftState: TShiftState;
  H: TShortcutEx;
  I: Integer;
begin
  if AnyEvent._type <> KeyPress then
    Exit(GDK_FILTER_CONTINUE);

  Sym := XKeycodeToKeysym(Capture.FDisplay, KeyEvent.keycode, 0);
  Key := SymToKey(Sym);
  ShiftState := ModToShift(KeyEvent.state);
  I := Capture.FindHotkey(Key, ShiftState);

  if I > -1 then
  begin
    H := Capture[I];
    if Assigned(H.Notify) then
      H.Notify(Capture, Key, ShiftState);

    Result := GDK_FILTER_REMOVE;
  end
  else
    Result := GDK_FILTER_CONTINUE;
end;

function TGTK2HotkeyManager.DoRegister(Shortcut: TShortCut): Boolean;

  procedure CaptureKey(Display: PDisplay; KeyCode: LongWord; Modifier: LongWord; Window: TWindow);
  begin
    { Capture keys without cap or num lock }
    XGrabKey(Display, KeyCode, Modifier and NotLock, Window, 1, GrabModeAsync, GrabModeAsync);
    { Capture keys with cap lock }
    XGrabKey(Display, KeyCode, Modifier or CapLock, Window, 1, GrabModeAsync, GrabModeAsync);
    { Capture keys with num lock }
    XGrabKey(Display, KeyCode, Modifier or NumLock, Window, 1, GrabModeAsync, GrabModeAsync);
    { Capture keys with cap or num lock }
    XGrabKey(Display, KeyCode, Modifier or CapLock or NumLock, Window, 1, GrabModeAsync, GrabModeAsync);
  end;

var
  Modifier: LongWord;
  KeySym, ShiftSym: TKeySym;
  KeyCode: LongWord;
  Window: TWindow;
  Key: Word;
  ShiftState: TShiftState;
begin
  ShortCutToKey(Shortcut, Key, ShiftState);

  Result := Key <> 0;
  if (Result) then
  begin
    Modifier := ShiftToMod(ShiftState);
    KeySym := KeyToSym(Key);
    KeyCode := XKeysymToKeycode(FDisplay, KeySym);
    Window := gdk_x11_drawable_get_xid(FRoot);
    CaptureKey(FDisplay, KeyCode, Modifier, Window);
    ShiftSym := XKeycodeToKeysym(FDisplay, KeyCode, 1);

    if KeySym <> ShiftSym then
    begin
      KeyCode := XKeysymToKeycode(FDisplay, ShiftSym);
      CaptureKey(FDisplay, KeyCode, Modifier, Window);
    end;

    if Count = 0 then
      gdk_window_add_filter(FRoot, @FilterKeys, Self)
  end;
end;

function TGTK2HotkeyManager.DoUnregister(Shortcut: TShortCut): Boolean;

  procedure ReleaseKey(Display: PDisplay; KeyCode: LongWord; Modifier: LongWord; Window: TWindow);
  begin
    { See comments in CaptureKey }
    XUngrabKey(Display, KeyCode, Modifier and NotLock, Window);
    XUngrabKey(Display, KeyCode, Modifier or CapLock, Window);
    XUngrabKey(Display, KeyCode, Modifier or NumLock, Window);
    XUngrabKey(Display, KeyCode, Modifier or CapLock or NumLock, Window);
  end;

var
  Modifier: LongWord;
  KeySym, ShiftSym: TKeySym;
  KeyCode: LongWord;
  Window: TWindow;
  Key: Word;
  ShiftState: TShiftState;
begin
  ShortCutToKey(Shortcut, Key, ShiftState);

  Result := Key <> 0;
  if (Result) then
  begin
    Modifier := ShiftToMod(ShiftState);
    KeySym := KeyToSym(Key);
    KeyCode := XKeysymToKeycode(FDisplay, KeySym);
    Window := gdk_x11_drawable_get_xid(FRoot);
    ReleaseKey(FDisplay, KeyCode, Modifier, Window);
    ShiftSym := XKeycodeToKeysym(FDisplay, KeyCode, 1);

    if KeySym <> ShiftSym then
    begin
      KeyCode := XKeysymToKeycode(FDisplay, ShiftSym);
      ReleaseKey(FDisplay, KeyCode, Modifier, Window);
    end;

    if Count = 0 then
      gdk_window_remove_filter(FRoot, @FilterKeys, Self);
  end;
end;

constructor TGTK2HotkeyManager.Create;
begin
  inherited Create;

  FRoot := gdk_get_default_root_window;
  FDisplay := GDK_WINDOW_XDISPLAY(FRoot);
end;

function TGTK2HotkeyManager.IsHotkeyAvailable(Shortcut: TShortCut): Boolean;
begin
  //TODO: Sob :(
  Result := True;
end;

function HotkeyCapture: TBaseHotkeyManager;
begin
  if InternalCapture = nil then
    InternalCapture := THotkeyCaptureImpl.Create;

  Result := TBaseHotkeyManager(InternalCapture);
end;

