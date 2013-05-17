{   Component(s):
    TcyDDECmd

    Description:
    Handle DDE command (open specified files extension registered on Windows)

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

unit cyDDECmd;

interface

uses Classes, Windows, Messages, sysUtils, Registry, ShlOBJ;

type
  TRegisterOptions = class(TPersistent)
  private
    FExtensionDescription: String;
    FFileExtension: String;
    FExtensionKey: String;
    FContextMenuCaption: String;
    FShellNotify: Boolean;
    procedure SetFileExtension(const Value: String);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property ContextMenuCaption: String read FContextMenuCaption write FContextMenuCaption;
    property ExtensionKey: String read FExtensionKey write FExtensionKey;
    property ExtensionDescription: String read FExtensionDescription write FExtensionDescription;
    property FileExtension: String read FFileExtension write SetFileExtension;
    property ShellNotify: Boolean read FShellNotify write FShellNotify default true;
  end;

  TProcAcceptCommand = procedure (Sender: TObject; var Accept: Boolean) of object;
  TProcReceiveCommand = procedure (Sender: TObject; Command: String) of object;

  TcyDDECmd = class(TComponent)
  private
    // Hidden window handle in order to receive message:
    FHWnd: HWND;
    fddeexecTopic: String;
    fddeexecApplication: String;
    fConnected: Boolean;
    fOnConnect: TNotifyEvent;
    fOnDisconnect: TNotifyEvent;
    fOnReceiveCommand: TProcReceiveCommand;
    fBeforeConnect: TProcAcceptCommand;
    FRegisterOptions: TRegisterOptions;
    procedure SetRegisterOptions(const Value: TRegisterOptions);
  protected
    procedure WndMethod(var Msg: TMessage); // virtual;
    function IsDDEAppAndTopic(const Msg: TMessage): Boolean;
    function DDEConnect(const Msg: TMessage): Boolean;
    function GetDDECmd(const Msg: TMessage): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: HWND read FHWnd;
    // Open cmd :
    function IsRegisteredOpenCmd: Boolean;
    function RegisterOpenCmd(IncludeDDE: Boolean): Boolean;
    // print cmd :
    function IsRegisteredPrintCmd: Boolean;
    function RegisterPrintCmd(IncludeDDE: Boolean): Boolean;
    // For all :
    function UnRegisterCmds: Boolean;  // No need of right admin ...
  published
    property ddeexecApplication: String read fddeexecApplication write fddeexecApplication;
    property ddeexecTopic: String read fddeexecTopic write fddeexecTopic;
    property Connected: Boolean read fConnected;
    property BeforeConnect: TProcAcceptCommand read fBeforeConnect write fBeforeConnect;
    property RegisterOptions: TRegisterOptions read FRegisterOptions write SetRegisterOptions;
    property OnConnect: TNotifyEvent read fOnConnect write fOnConnect;
    property OnDisconnect: TNotifyEvent read fOnDisconnect write fOnDisconnect;
    property OnReceiveCommand: TProcReceiveCommand read fOnReceiveCommand write fOnReceiveCommand;
  end;

implementation

{ TRegisterOptions }
constructor TRegisterOptions.Create(AOwner: TComponent);
begin
  FContextMenuCaption := 'Open with [your application name ...]';
  FExtensionKey := '[AppExtensionKey]';
  FExtensionDescription := '[Your extension description]';
  FFileExtension := '.[your extension]';
  FShellNotify := true;
end;

procedure TRegisterOptions.SetFileExtension(const Value: String);
begin
  FFileExtension := Value;

  if FFileExtension <> '' then
    if FFileExtension[1] <> '.' then FFileExtension := '.' + FFileExtension;
end;

{ TcyDDECmd }
constructor TcyDDECmd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fConnected := false;
  // Create a window to process windows messages with WndMethod() :
  FHWnd := AllocateHWnd(WndMethod);
  FRegisterOptions := TRegisterOptions.Create(self);
end;

destructor TcyDDECmd.Destroy;
begin
  FRegisterOptions.Free;
  DeallocateHWnd(FHWnd);  // Delete window handle ...
  inherited;
end;

procedure TcyDDECmd.SetRegisterOptions(const Value: TRegisterOptions);
begin
  FRegisterOptions.Assign(Value);
end;

// Process windows messages :
procedure TcyDDECmd.WndMethod(var Msg: TMessage);
var
  StrCmd: String;
  Handled, Accept: Boolean;
begin
  Handled := false;

  if not (csDesigning in ComponentState) then
    if Msg.Msg = WM_DDE_INITIATE
    then begin
      Handled := true;
      if IsDDEAppAndTopic(Msg)                    // DDE cmd for this application
      then begin
        Accept := true;
        if Assigned(fBeforeConnect) then                 // Developper confirmation :
          fBeforeConnect(self, Accept);

        if Accept
        then begin
          fConnected := DDEConnect(Msg);

          if fConnected then
            if Assigned(fOnConnect) then
              fOnConnect(self);
        end;
      end;
    end
    else
      if Msg.Msg = WM_DDE_EXECUTE
      then begin
        Handled := true;
        if fConnected
        then begin
          StrCmd := GetDDECmd(Msg);  // Will send confirmation message ...

          if Assigned(fOnReceiveCommand) then
            fOnReceiveCommand(Self, StrCmd);
        end;
      end
      else
        if Msg.Msg = WM_DDE_TERMINATE
        then begin
          Handled := true;
          if fConnected
          then begin
            // Send end communication confirmation :
            PostMessage(Msg.wParam, WM_DDE_TERMINATE, FHWnd, 0);
            fConnected := false;
            if Assigned(fOnDisconnect) then
              fOnDisconnect(self);
          end;
        end;

  if not Handled then
    Msg.Result := DefWindowProc(FHWnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

function TcyDDECmd.IsDDEAppAndTopic(const Msg: TMessage): Boolean;
var
  MsgAppDDE, MsgTopicDDE: Word;
  OurApp, OurTopic: Word;
  p: PWord;
begin
  Result := true;
  // Retrieve address from lParam :
  p := PWord(@Msg.lParam);

  // Application atom number :
  MsgAppDDE := Word(p^);
  Inc(p);
  // Topic atom number :
  MsgTopicDDE := Word(p^);

  // Test if the message is for us:
  if fddeexecApplication <> ''
  then begin
    OurApp := GlobalFindAtom(PChar(fddeexecApplication));
    Result := OurApp = MsgAppDDE;
//    ShowMessage(intToStr(OurApp) +'[ddeexecApplication]' + intToStr(MsgAppDDE));
  end;

  if Result and (fddeexecTopic <> '')
  then begin
    OurTopic := GlobalFindAtom(PChar(fddeexecTopic));
    Result := OurTopic = MsgTopicDDE;
//    ShowMessage(intToStr(OurTopic) +'[ddeexecTopic]' + intToStr(MsgTopicDDE));
  end;
end;

function TcyDDECmd.DDEConnect(const Msg: TMessage): Boolean;
var
  OurApp, OurTopic: Word;
  lTmpParam: LongWord;
begin
  // Add atom to global atom table for the response :
  OurApp := GlobalAddAtom(PChar(fddeexecApplication));
  OurTopic := GlobalAddAtom(PChar(fddeexecTopic));

  // Put information to lParam :
  lTmpParam := PackDDElParam(WM_DDE_ACK, OurApp, OurTopic);

  // Send message to confirm communication :
  Result := Boolean(SendMessage(Msg.wParam, WM_DDE_ACK, FHWnd, lTmpParam));

  // Delete atoms :
  GlobalDeleteAtom(OurApp);
  GlobalDeleteAtom(OurTopic);
end;

function TcyDDECmd.GetDDECmd(const Msg: TMessage): String;
var
  lpGlobal: Pointer;
  lpBuffer: String;
  lTmpParam: LongWord;
begin
  lpGlobal := GlobalLock(Msg.lParam);
  SetLength(lpBuffer, StrLen(PChar(lpGlobal)));
  // Copy from pointer :
  StrCopy(PChar(lpBuffer),PChar(lpGlobal));
  Result := lpBuffer;
  GlobalUnlock(Msg.lParam);

  // Send message to confirm communication :
  lTmpParam := PackDDElParam(WM_DDE_ACK, $8000, Msg.lParam);
  PostMessage(Msg.wParam, WM_DDE_ACK, FHWnd, Msg.lParam);
end;

function TcyDDECmd.IsRegisteredOpenCmd: Boolean;
var
  Reg: TRegistry;
  aKey: String;
begin
  Result := false;
  if FRegisterOptions.FExtensionKey = '' then Exit;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    aKey := FRegisterOptions.FExtensionKey + '\shell\open';
    if aKey[1] <> '\' then aKey := '\' + aKey;
    Result := Reg.OpenKeyReadOnly(aKey);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function TcyDDECmd.RegisterOpenCmd(IncludeDDE: Boolean): Boolean;
var Reg: TRegistry;

    function WriteKey(aKey, aName, aValue: String): Boolean;
    begin
      Result := false;
      if aKey[1] <> '\' then aKey := '\' + aKey;

      if Reg.OpenKey(aKey, true)
      then begin
        Reg.WriteString(aName, aValue);
        Reg.CloseKey;
        Result := True;
      end;
    end;

begin
  Result := false;
  if FRegisterOptions.FFileExtension = '' then Exit;
  if FRegisterOptions.FExtensionKey = '' then Exit;

  Reg := TRegistry.Create;

  try
    Result := true;
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Application display name on "Open with ..." context menu :
    // if not WriteKey(FRegisterOptions.FExtensionKey + '\Local Settings\Software\Microsoft\Windows\Shell\MuiCache', ParamStr(0), 'Teste ...') then Result := false;

    // May need right admin:
    if not WriteKey(FRegisterOptions.FFileExtension,                                     '', FRegisterOptions.FExtensionKey) then Result := false;

    if not WriteKey(FRegisterOptions.FExtensionKey,                                      '', FRegisterOptions.FExtensionDescription) then Result := false;
    if not WriteKey(FRegisterOptions.FExtensionKey + '\DefaultIcon',                     '', ParamStr(0) + ',0') then Result := false;
    if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\open',                      '', FRegisterOptions.FContextMenuCaption) then Result := false;
    if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\open\command',              '', '"' + paramStr(0) + '"' + ' "%1"') then Result := false;

    if IncludeDDE
    then begin
      if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\open\ddeexec',              '', 'open"%1"') then Result := false;
      if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\open\ddeexec\Application',  '', fddeexecApplication) then Result := false;
      if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\open\ddeexec\Topic',        '', fddeexecTopic) then Result := false;
    end;
  finally
    Reg.Free;
  end;

  if Result and FRegisterOptions.FShellNotify then
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST,nil,nil);
end;

function TcyDDECmd.UnRegisterCmds: Boolean;
var
  Reg: TRegistry;

    procedure RemoveName(aKey, aName: String);
    begin
      if aKey[1] <> '\' then aKey := '\' + aKey;
      if Reg.OpenKey(aKey, True)
      then begin
        Reg.DeleteValue(aName);
        Reg.CloseKey;
      end;
    end;

    procedure RemoveKey(aKey: String);
    begin
      if aKey[1] <> '\' then aKey := '\' + aKey;
      Reg.DeleteKey(aKey);
    end;


begin
  Result := false;
  Reg := TRegistry.Create;
  if FRegisterOptions.FFileExtension = '' then Exit;
  if FRegisterOptions.FExtensionKey = '' then Exit;


  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    RemoveName(FRegisterOptions.FFileExtension, '');
    RemoveKey(FRegisterOptions.FExtensionKey);
    Result := true;
  finally
    Reg.Free;
  end;

  if FRegisterOptions.FShellNotify then
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST,nil,nil);
end;

function TcyDDECmd.IsRegisteredPrintCmd: Boolean;
var
  Reg: TRegistry;
  aKey: String;
begin
  Result := false;
  if FRegisterOptions.FExtensionKey = '' then Exit;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    aKey := FRegisterOptions.FExtensionKey + '\shell\print';
    if aKey[1] <> '\' then aKey := '\' + aKey;
    Result := Reg.OpenKeyReadOnly(aKey);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function TcyDDECmd.RegisterPrintCmd(IncludeDDE: Boolean): Boolean;
var Reg: TRegistry;

    function WriteKey(aKey, aName, aValue: String): Boolean;
    begin
      Result := false;
      if aKey[1] <> '\' then aKey := '\' + aKey;

      if Reg.OpenKey(aKey, true)
      then begin
        Reg.WriteString(aName, aValue);
        Reg.CloseKey;
        Result := True;
      end;
    end;

begin
  Result := false;
  if FRegisterOptions.FFileExtension = '' then Exit;
  if FRegisterOptions.FExtensionKey = '' then Exit;

  Reg := TRegistry.Create;

  try
    Result := true;
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // May need right admin:
    if not WriteKey(FRegisterOptions.FFileExtension,                                     '', FRegisterOptions.FExtensionKey) then Result := false;

    if not WriteKey(FRegisterOptions.FExtensionKey,                                      '', FRegisterOptions.FExtensionDescription) then Result := false;
//    if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\print',                      '', FRegisterOptions.FContextMenuCaption) then Result := false;
    if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\print\command',              '', '"' + paramStr(0) + '" print "%1"') then Result := false;

    if IncludeDDE
    then begin
      if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\print\ddeexec',              '', 'print"%1"') then Result := false;
      if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\print\ddeexec\Application',  '', fddeexecApplication) then Result := false;
      if not WriteKey(FRegisterOptions.FExtensionKey + '\shell\print\ddeexec\Topic',        '', fddeexecTopic) then Result := false;
    end;
  finally
    Reg.Free;
  end;

  if Result and FRegisterOptions.FShellNotify then
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST,nil,nil);
end;

end.
