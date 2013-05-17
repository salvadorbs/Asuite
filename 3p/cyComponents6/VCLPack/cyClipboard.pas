{   Component(s):
    tcyClipboard

    Description:
    Component that analyze and retrieve clipboard contents.

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

unit cyClipboard;

interface

uses Classes, Windows, SysUtils, Graphics, clipbrd, Messages, ShellAPI;

type
  TClipbrdFormat = Word;

  TcyTmpClipbrd = class(TClipboard)    // In order to access protected methods ...
  end;

  TcyClipboard = class(TComponent)
  private
    // Hidden window handle in order to receive message:
    FHWnd: HWND;
    fBookmarks: Array of Cardinal;
    fBookmarkCount: Integer;
    FOnClipboardContentChanged: TNotifyEvent;
    function GeTClipbrdFormat(Index: Integer): TClipbrdFormat;
    function GeTClipbrdFormatName(Index: Integer): String;
    function GeTClipbrdFormatCount: Integer;
    function GeTClipbrdFormatHandle(Index: Integer): Cardinal;
  protected
    PassMessageToWnd: Hwnd;
    procedure WndMethod(var Msg: TMessage); virtual;
  public
    PictureFormats: array of TClipbrdFormat;
    AnsiStringFormats: array of TClipbrdFormat;
    WideStringFormats: array of TClipbrdFormat;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function GetclipboardFormatName(aFormat: TClipbrdFormat): String;
    function FindFormat(aFormat: TClipbrdFormat): Boolean;
    function FindAnyFormat(PreferencialOrderFormats: Array Of TClipbrdFormat): TClipbrdFormat;
    function FindPictureFormat: TClipbrdFormat;
    function FindWideStringFormat: TClipbrdFormat;
    function FindAnsiStringFormat: TClipbrdFormat;
    property FormatCount: Integer read GeTClipbrdFormatCount;
    property Formats[Index: Integer]: TClipbrdFormat read GeTClipbrdFormat;
    property FormatsName[Index: Integer]: String read GeTClipbrdFormatName;
    property FormatsHandle[Index: Integer]: Cardinal read GeTClipbrdFormatHandle;

    // Clipboard read/write persistent:
    procedure AssignClipboardTo(Dest: TPersistent);
    procedure AssignToClipboard(Source: TPersistent); overload;
    procedure AssignToClipboard(Source: String); overload;

    // Bookmarks, allows to know if clipboard content on a specified format was changed with isBookmarked() since last time we call Bookmark():
    procedure BookmarkContent(aFormat: TClipbrdFormat);
    function isContentBookmarked(aFormat: TClipbrdFormat): Boolean;
    procedure ClearContentBookmarks;

    // Get content generics by type:
    function GetContentAsAnsiString(aFormat: TClipbrdFormat; var AnsiStr: AnsiString): Boolean;
    function GetContentAsWideString(aFormat: TClipbrdFormat; var Str: WideString): Boolean;
    function GetContentAsPicture(aFormat: TClipbrdFormat; aPicture: TPicture): Boolean;

    // Get content by format:
    function GetContentHDROP(aList: TStrings): Boolean;
  published
    property OnClipboardContentChanged: TNotifyEvent read FOnClipboardContentChanged write FOnClipboardContentChanged;
  end;

const
  CF_HTML = 49339; // 49372;  // Html clipboard format
  MsgResultOk = 99;           // Arbitrary value, only can' t be 0!

implementation

{ TcyClipboard }
constructor TcyClipboard.Create(AOwner: TComponent);
begin
  inherited;
  fBookmarkCount := 0;
  SetLength(fBookmarks, fBookmarkCount);

  SetLength(PictureFormats, 2);
  PictureFormats[0] := CF_BITMAP;
  PictureFormats[1] := CF_PICTURE;

  SetLength(AnsiStringFormats, 3);
  AnsiStringFormats[0] := CF_TEXT;
  AnsiStringFormats[1] := CF_OEMTEXT;
  AnsiStringFormats[3] := CF_LOCALE;

  SetLength(WideStringFormats, 1);
  WideStringFormats[0] := CF_UNICODETEXT;

  // Create a window to process windows messages with WndMethod() :
  FHWnd := AllocateHWnd(WndMethod);

  // Be notified when clipboard changes :
  PassMessageToWnd := SetClipboardViewer(FHWnd);
end;

destructor TcyClipboard.Destroy;
begin
  // Unregister clipboard changes notify:
  ChangeClipboardChain(FHWnd, PassMessageToWnd);

  // Delete window handle :
  DeallocateHWnd(FHWnd);

  inherited;
end;

procedure TcyClipboard.WndMethod(var Msg: TMessage);
var Handled: Boolean;
begin
  Handled := false;

  // In order to handle change in clipboard chain :
  if Msg.Msg = WM_CHANGECBCHAIN then
  begin
    Handled := true;
    // Next viewer was removed: record following viewer as next
    if HWND(Msg.WParam) = PassMessageToWnd
    then
      PassMessageToWnd := HWND(Msg.LParam)
    else
      // No changes for me, pass message on to next viewer :
      if PassMessageToWnd <> 0 then
        SendMessage(PassMessageToWnd, WM_CHANGECBCHAIN, Msg.WParam, Msg.LParam);
  end;

  // Clipboard content changed :
  if Msg.Msg = WM_DRAWCLIPBOARD then
  begin
    Handled := true;
    ClearContentBookmarks;

    // Pass message to the next viewer in clipboard chain, if any :
    if PassMessageToWnd <> 0 then
      SendMessage(PassMessageToWnd, WM_DRAWCLIPBOARD, Msg.WParam, Msg.LParam);

    if Assigned(fOnClipboardContentChanged) then
      fOnClipboardContentChanged(Self);
  end;

  if Handled
  then Msg.Result := MsgResultOk
  else Msg.Result := DefWindowProc(fHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TcyClipboard.Clear;
begin
  Clipboard.Clear;
end;

function TcyClipboard.GetclipboardFormatName(aFormat: TClipbrdFormat): String;
var buf: Array [0..99] of Char;
begin
  Result := '';

  if Windows.GetclipboardFormatName(aFormat, buf, Pred(Sizeof(buf))) <> 0
  then
    Result := StrPas(buf)
  else
    case aFormat of
      CF_TEXT: Result := 'CF_TEXT';
      CF_BITMAP: Result := 'CF_BITMAP';
      CF_METAFILEPICT: Result := 'CF_METAFILEPICT';
      CF_SYLK: Result := 'CF_SYLK';
      CF_DIF: Result := 'CF_DIF';
      CF_TIFF: Result := 'CF_TIFF';
      CF_OEMTEXT: Result := 'CF_OEMTEXT';
      CF_DIB: Result := 'CF_DIB';
      CF_PALETTE: Result := 'CF_PALETTE';
      CF_PENDATA: Result := 'CF_PENDATA';
      CF_RIFF: Result := 'CF_RIFF';
      CF_WAVE: Result := 'CF_WAVE';
      CF_UNICODETEXT: Result := 'CF_UNICODETEXT';
      CF_ENHMETAFILE: Result := 'CF_ENHMETAFILE';
      CF_HDROP: Result := 'CF_HDROP';
      CF_LOCALE: Result := 'CF_LOCALE';
//      CF_MAX: Result := 'CF_MAX';
      // CF_DIBV5: Result := 'CF_DIBV5';
      CF_MAX_XP: Result := 'CF_MAX_XP';
      CF_OWNERDISPLAY: Result := 'CF_OWNERDISPLAY';
      CF_DSPTEXT: Result := 'CF_DSPTEXT';
      CF_DSPBITMAP: Result := 'CF_DSPBITMAP';
      CF_DSPMETAFILEPICT: Result := 'CF_DSPMETAFILEPICT';
      CF_DSPENHMETAFILE: Result := 'CF_DSPENHMETAFILE';
      CF_PRIVATEFIRST: Result := 'CF_PRIVATEFIRST';
      CF_PRIVATELAST: Result := 'CF_PRIVATELAST';
      CF_GDIOBJFIRST: Result := 'CF_GDIOBJFIRST';
      CF_GDIOBJLAST: Result := 'CF_GDIOBJLAST';
    end;
end;

function TcyClipboard.FindFormat(aFormat: TClipbrdFormat): Boolean;
var f: Integer;
begin
  Result := false;

  with Clipboard do
  begin
    Open;

    try
      for f := 0 to FormatCount-1 do
      begin
        if Formats[f] = aFormat
        then begin
          Result := true;
          Break;
        end;
      end;
    finally
      Close;
    end;
  end;
end;

function TcyClipboard.FindAnyFormat(PreferencialOrderFormats: Array Of TClipbrdFormat): TClipbrdFormat;
var o, f: Integer;
begin
  Result := 0;

  with Clipboard do
  begin
    Open;

    try
      // Try to locate any format in array "Formats" in preferencial order:
      for o := 0 to length(PreferencialOrderFormats) do
      begin
        for f := 0 to FormatCount-1 do
        begin
          if Formats[f] = PreferencialOrderFormats[o]
          then begin
            Result := PreferencialOrderFormats[o];
            Break;
          end;
        end;

        if Result <> 0 then Break;
      end;
    finally
      Close;
    end;
  end;
end;

function TcyClipboard.FindPictureFormat: TClipbrdFormat;
begin
  Result := FindAnyFormat(PictureFormats);
end;

function TcyClipboard.FindWideStringFormat: TClipbrdFormat;
begin
  Result := FindAnyFormat(WideStringFormats);
end;

function TcyClipboard.FindAnsiStringFormat: TClipbrdFormat;
begin
  Result := FindAnyFormat(AnsiStringFormats);
end;

function TcyClipboard.GeTClipbrdFormatCount: Integer;
begin
  Result := 0;

  with Clipboard do
  begin
    Open;

    try
      Result := FormatCount;
    finally
      Close;
    end;
  end;
end;

function TcyClipboard.GeTClipbrdFormatHandle(Index: Integer): cardinal;
begin
  Result := Clipboard.GetAsHandle( GeTClipbrdFormat(Index) );
end;

function TcyClipboard.GeTClipbrdFormat(Index: Integer): TClipbrdFormat;
begin
  Result := 0;
  with Clipboard do
  begin
    Open;

    try
      Result := Formats[Index];
    finally
      Close;
    end;
  end;
end;

function TcyClipboard.GeTClipbrdFormatName(Index: Integer): String;
begin
  Result := GetclipboardFormatName( GeTClipbrdFormat(Index) );
end;

procedure TcyClipboard.AssignClipboardTo(Dest: TPersistent);
begin
  TcyTmpClipbrd(Clipboard).AssignTo(Dest);
end;

procedure TcyClipboard.AssignToClipboard(Source: TPersistent);
begin
  Clipboard.Assign(Source);
end;

procedure TcyClipboard.AssignToClipboard(Source: String);
begin
  Clipboard.AsText := Source;
end;

procedure TcyClipboard.BookmarkContent(aFormat: TClipbrdFormat);
begin
  inc(fBookmarkCount, 1);
  SetLength(fBookmarks, fBookmarkCount);
  fBookmarks[fBookmarkCount-1] := Clipboard.GetAsHandle(aFormat);
end;

function TcyClipboard.isContentBookmarked(aFormat: TClipbrdFormat): Boolean;
var
  i: Integer;
  aHandle: Cardinal;
begin
  Result := false;
  aHandle := Clipboard.GetAsHandle(aFormat);

  for i := 0 to fBookmarkCount-1 do
    if fBookmarks[i] = aHandle
    then begin
      Result := true;
      Break;
    end;
end;

procedure TcyClipboard.ClearContentBookmarks;
begin
  fBookmarkCount := 0;
  SetLength(fBookmarks, fBookmarkCount);
end;

function TcyClipboard.GetContentAsAnsiString(aFormat: TClipbrdFormat; var AnsiStr: AnsiString): Boolean;
var
  DataHandle: THandle;
  DataPtr: PAnsiString;
begin
  Result := false;
  AnsiStr := '';
  Clipboard.Open;
  DataHandle := GetClipboardData(aFormat);
  if DataHandle <> 0
  then
    try
      DataPtr := GlobalLock(DataHandle);
      AnsiStr := PAnsiChar(DataPtr);
      Result := AnsiStr <> '';
    finally
      GlobalUnLock(DataHandle);
    end;

  Clipboard.Close;
end;

function TcyClipboard.GetContentAsWideString(aFormat: TClipbrdFormat; var Str: WideString): Boolean;
var
  DataHandle: THandle;
  DataPtr: PWideChar;
begin
  Result := false;
  Str := '';
  Clipboard.Open;
  DataHandle := GetClipboardData(aFormat);
  if DataHandle <> 0
  then
    try
      DataPtr := GlobalLock(DataHandle);
      Str := DataPtr;
      Result := Str <> '';
    finally
      GlobalUnLock(DataHandle);
    end;

  Clipboard.Close;
end;

function TcyClipboard.GetContentAsPicture(aFormat: TClipbrdFormat; aPicture: TPicture): Boolean;
var
  DataHandle: THandle;
  FPalette: HPalette;
begin
  Result := false;

  if TPicture.SupportsClipboardFormat(aFormat)
  then begin
    Clipboard.Open;
    DataHandle  := GetClipboardData(aFormat);
    FPalette := GetClipboardData(CF_PALETTE);
    aPicture.LoadFromClipboardFormat(aFormat, DataHandle, FPalette);
    Result := true;
    Clipboard.Close;
  end;
end;

function TcyClipboard.GetContentHDROP(aList: TStrings): Boolean;
var
  FileOrDir: String;
  DataHandle: THandle;
  DataPtr: Pointer;
  Buffer : array[0..MAX_PATH] of Char;
  Count, i: Integer;
begin
  Result := false;
  aList.Clear;

  Clipboard.Open;
  DataHandle := GetClipboardData(CF_HDROP);
  if DataHandle <> 0
  then
    try
      DataPtr := PChar(GlobalLock(DataHandle));
      Count := DragQueryFile(HDROP(DataPtr), $FFFFFFFF, nil, 0);

      for i:= 0 to Count-1 do
      begin
        DragQueryFile(HDROP(DataPtr), i, @Buffer, SizeOf(Buffer));

        if strlen(PChar(@Buffer)) > 0
        then begin
          FileOrDir := PChar(@Buffer);
          aList.Add(FileOrDir);
        end;
      end;

      Result := True;
    finally
      GlobalUnlock(DataHandle);
    end;

  Clipboard.Close;
end;

end.
