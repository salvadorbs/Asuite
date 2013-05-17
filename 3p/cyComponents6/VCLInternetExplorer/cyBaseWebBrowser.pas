{* The code of this unit accompanies the article "How to load and save documents
 * in TWebBrowser in a Delphi-like way" which can be found at
 * http://www.delphidabbler.com/articles.php?article=14.
 *
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
 * The Original Code is "TWebBrowser IO Wrapper Class".
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2004 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Mauricio Julio:
 *     Fixed InternalSaveBodyHTMLToStream and InternalSaveBodyTextToStream for
 *     for Unicode Delphis.
 *     Renamed as TcyBaseWebBrowser
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
 * ***** END LICENSE BLOCK *****
}




{   Component(s):
    tcyBaseWebBrowser

    Description:
    Base for Internet Explorer Wrapper components

    Author: Mauricio
    mail: mauricio_box@yahoo.com

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    Updates are avaible in at adress https://sourceforge.net/projects/tcycomponents/

    Copyrights: See the above license
}

unit cyBaseWebBrowser;

interface

uses
  Classes, Graphics, SHDocVw, SysUtils, StrUtils, Windows, ActiveX, Forms, MSHTML, Variants, OleServer;

type
  TTextContent = (tcDocument, tcBodyHtml, tcBodyAsText);
  TBodyBorderStyle = (bbsNone, bbsSingle);

  TcyBaseWebBrowser = class(TWebBrowser)
  private
    FDesignMode: Boolean;
    FAsynchMode: Boolean;
    FNavigating: Boolean;
    function GetBusy: Boolean;
    procedure SetAsynchMode(const Value: Boolean);
    procedure SetDesignMode(const Value: Boolean);
    function GetCharset: String;
    procedure SetCharset(const Value: String);
  protected
    procedure InvokeEvent(DispID: TDispID; var Params: TDispParams); override;
    procedure cyInvokeEvent(DispID: TDispID; var Params: TVariantArray); virtual;
    function InternalLoadBodyTextFromStream(const Stream: TStream): Boolean;
    function InternalLoadBodyHtmlFromStream(const Stream: TStream): Boolean;
    function InternalLoadDocumentFromStream(const Stream: TStream): boolean;
    function InternalSaveBodyHTMLToStream(const Stream: TStream): boolean;
    function InternalSaveBodyTextToStream(const Stream: TStream): boolean;
    function InternalSaveDocumentToStream(const Stream: TStream): boolean;
    procedure ApplyDesignModeToDocument;
    property AsynchMode: Boolean read FAsynchMode write SetAsynchMode default false;
    property Busy: Boolean read GetBusy;        // TwebBrowser.Busy seems not working (not set to false when document completed)
    property Charset: String read GetCharset write SetCharset;
    property DesignMode: Boolean read FDesignMode write SetDesignMode default false;
    property Navigating: Boolean read FNavigating default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    // Herited from TWebBrowser :
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; const Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; const Flags: OleVariant; const TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; const Flags: OleVariant;
                       const TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; const Flags: OleVariant; const TargetFrameName: OleVariant;
                       var PostData: OleVariant; const Headers: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant;
                        var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant;
                        var PostData: OleVariant; var Headers: OleVariant); overload;
    // TcyBaseWebBrowser :
    function ExecCommand(CmdID: WideString; ShowUI: Boolean; Value: OleVariant): Boolean;
    function GetIEHandle(const aClassName: string = 'Internet Explorer_Server'): cardinal;
    function GetIHTMLDocument2(var aIHTMLDocument2: IHTMLDocument2): Boolean;
    function GetSelectionObject(var aIHTMLSelectionObject: IHTMLSelectionObject): Boolean;
    function LoadFromStream(const Stream: TStream; const TextContent: TTextContent = tcDocument): boolean;
    function LoadFromString(aString: string; const TextContent: TTextContent = tcDocument): boolean; overload;
    function LoadFromString(aString: string; StringCharEncoding: TEncoding; const TextContent: TTextContent = tcDocument): boolean; overload;
    function LoadFromFile(const FileName: string; const TextContent: TTextContent = tcDocument): boolean;
    function NavigateToLocalFile(const FileName: string): Boolean;
    procedure NavigateToResource(const Module: HMODULE; const ResName: PChar; const ResType: PChar = nil); overload;
    procedure NavigateToResource(const ModuleName: string; const ResName: PChar; const ResType: PChar = nil); overload;
    procedure NavigateToURL(const URL: String);
    function SaveToStream(const Stream: TStream; const TextContent: TTextContent = tcDocument): boolean;
    function SaveToString(const TextContent: TTextContent = tcDocument): string;
    procedure SaveToFile(const FileName: string; const TextContent: TTextContent = tcDocument); overload;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding; const TextContent: TTextContent = tcDocument); overload;
    function SetFocusToDoc: Boolean;
    procedure WaitForCompleteState;  // Wait navigation done ...
  end;

implementation

uses cyIEUtils;

{ tcyBaseWebBrowser }
constructor TcyBaseWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FAsynchMode := false;
  FDesignMode := false;
  FNavigating := false;
end;

procedure TcyBaseWebBrowser.Loaded;
begin
  inherited;

  if FDesignMode
  then
    if not (csDesigning in Owner.ComponentState)
    then begin
      NavigateToURL('about:blank');
      ApplyDesignModeToDocument;
    end;
end;

destructor TcyBaseWebBrowser.Destroy;
begin
  //
  inherited;
end;

function TcyBaseWebBrowser.GetIHTMLDocument2(var aIHTMLDocument2: IHTMLDocument2): Boolean;
begin
  RESULT := false;
  if Assigned(Document)
  then
    if Document.QueryInterface(IHTMLDocument2, aIHTMLDocument2) = S_OK
    then RESULT := true;
end;

function TcyBaseWebBrowser.GetSelectionObject(var aIHTMLSelectionObject: IHTMLSelectionObject): Boolean;
var Doc: IHTMLDocument2;
begin
  RESULT := false;

  if GetIHTMLDocument2(Doc)
  then begin
    aIHTMLSelectionObject := Doc.selection;
    RESULT := true;
  end;
end;

procedure TcyBaseWebBrowser.Navigate(const URL: WideString; const Flags,
  TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, EmptyParam);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate(const URL: WideString; const Flags,
  TargetFrameName: OleVariant; var PostData: OleVariant; const Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate(const URL: WideString; const Flags, TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, EmptyParam, EmptyParam);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate(const URL: WideString);
begin
  DefaultInterface.Navigate(URL, EmptyParam, EmptyParam, EmptyParam, EmptyParam);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate(const URL: WideString; const Flags: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, EmptyParam, EmptyParam, EmptyParam);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate2(var URL, Flags, TargetFrameName,
  PostData: OleVariant);
begin
  // Inherited;
  Inherited Navigate2(URL, Flags, TargetFrameName, PostData);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate2(var URL, Flags, TargetFrameName,
  PostData, Headers: OleVariant);
begin
  // Inherited;
  Inherited Navigate2(URL, Flags, TargetFrameName, PostData, Headers);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate2(var URL, Flags,
  TargetFrameName: OleVariant);
begin
  // Inherited;
  Inherited Navigate2(URL, Flags, TargetFrameName);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate2(var URL: OleVariant);
begin
  // Inherited;
  Inherited Navigate2(URL);

  if not FAsynchMode
  then WaitForCompleteState;
end;

procedure TcyBaseWebBrowser.Navigate2(var URL, Flags: OleVariant);
begin
  // Inherited;
  Inherited Navigate2(URL, Flags);

  if not FAsynchMode
  then WaitForCompleteState;
end;

function TcyBaseWebBrowser.NavigateToLocalFile(const FileName: string): Boolean;
begin
  RESULT := FileExists(FileName);
  if RESULT
  then NavigateToURL('file://' + FileName);
end;

procedure TcyBaseWebBrowser.NavigateToResource(const Module: HMODULE; const ResName, ResType: PChar);
var URL: string;
begin
  URL := MakeResourceURL(Module, ResName, ResType);
  NavigateToURL(URL);
end;

procedure TcyBaseWebBrowser.NavigateToResource(const ModuleName: string; const ResName, ResType: PChar);
begin
  NavigateToURL(MakeResourceURL(ModuleName, ResName, ResType));
end;

procedure TcyBaseWebBrowser.NavigateToURL(const URL: String);
var Flags: OleVariant;  // flags that determine action
begin
  // Don't record in history :
  Flags := navNoHistory;
  if AnsiStartsText('res://', URL) or AnsiStartsText('file://', URL)
    or AnsiStartsText('about:', URL) or AnsiStartsText('javascript:', URL)
    or AnsiStartsText('mailto:', URL) then
    // don't use cache for local files
    Flags := Flags or navNoReadFromCache or navNoWriteToCache;
  // Do the navigation :
  Navigate(URL, Flags);
end;

function TcyBaseWebBrowser.InternalLoadDocumentFromStream(const Stream: TStream): Boolean;
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  RESULT := false;
  if Assigned(Document)
  then
    // Get IPersistStreamInit interface on document object
    if Document.QueryInterface(IPersistStreamInit, PersistStreamInit) = S_OK
    then begin
      // Clear document
      if PersistStreamInit.InitNew = S_OK
      then begin
        // Get IStream interface on stream
        StreamAdapter:= TStreamAdapter.Create(Stream);
        // Load data from Stream into WebBrowser
        PersistStreamInit.Load(StreamAdapter);
        RESULT := true;
      end;
    end;
end;

function TcyBaseWebBrowser.InternalLoadBodyTextFromStream(const Stream: TStream): Boolean;
var
  Doc: IHTMLDocument2;
  BodyElement: IHTMLElement;
begin
  RESULT := false;
  if GetIHTMLDocument2(Doc)
  then begin
    BodyElement := Doc.body;
    if Assigned(BodyElement)
    then begin
      BodyElement.innerText := TStringStream(Stream).DataString;
      RESULT := true;
    end;
  end;
end;

function TcyBaseWebBrowser.InternalLoadBodyHtmlFromStream(const Stream: TStream): Boolean;
var
  Doc: IHTMLDocument2;
  BodyElement: IHTMLElement;
begin
  RESULT := false;
  if GetIHTMLDocument2(Doc)
  then begin
    BodyElement := Doc.body;
    if Assigned(BodyElement)
    then begin
      BodyElement.innerHTML := TStringStream(Stream).DataString;
      RESULT := true;
    end;
  end;
end;

function TcyBaseWebBrowser.InternalSaveBodyHTMLToStream(const Stream: TStream): boolean;
var
  {$IFDEF UNICODE}
  Bytes: TBytes;
  {$ENDIF}
  HTMLStr: string;
  Doc: IHTMLDocument2;
  BodyElement: IHTMLElement;
begin
  RESULT := false;
  if GetIHTMLDocument2(Doc)
  then begin
    BodyElement := Doc.body;
    if Assigned(BodyElement)
    then begin
      HTMLStr := BodyElement.innerHTML;
      {$IFDEF UNICODE}
      Bytes := BytesOf(HTMLStr);
      Stream.Write(Bytes[0], Length(Bytes));
      {$ELSE}
      Stream.WriteBuffer(HTMLStr[1], Length(HTMLStr));
      {$ENDIF}
      RESULT := true;
    end;
  end;
end;

function TcyBaseWebBrowser.InternalSaveBodyTextToStream(const Stream: TStream): boolean;
var
  {$IFDEF UNICODE}
  Bytes: TBytes;
  {$ENDIF}
  TextStr: string;
  Doc: IHTMLDocument2;
  BodyElement: IHTMLElement;
begin
  RESULT := false;
  if GetIHTMLDocument2(Doc)
  then begin
    BodyElement := Doc.body;
    if Assigned(BodyElement)
    then begin
      TextStr := BodyElement.innerText;
      {$IFDEF UNICODE}
      Bytes := BytesOf(TextStr);
      Stream.Write(Bytes[0], Length(Bytes));
      {$ELSE}
      Stream.WriteBuffer(TextStr[1], Length(TextStr));
      {$ENDIF}
      RESULT := true;
    end;
  end;
end;

// If charset is defined on the html, it will save the document with the specified charset.
// Otherwise, it will save the document with file encoding from last open file.
function TcyBaseWebBrowser.InternalSaveDocumentToStream(const Stream: TStream): boolean;
var
  StreamAdapter: IStream;
  PersistStreamInit: IPersistStreamInit;
begin
  RESULT := false;
  if Assigned(Document)
  then
    if Document.QueryInterface(IPersistStreamInit, PersistStreamInit) = S_OK
    then begin
      StreamAdapter := TStreamAdapter.Create(Stream);
      PersistStreamInit.Save(StreamAdapter, True);
      RESULT := true;
    end;
end;

procedure TcyBaseWebBrowser.InvokeEvent(DispID: TDispID; var Params: TDispParams);
var
  vVarArray: TVariantArray;
  vPVarArgIn: PVariantArg;
  i, vFistArrItem, vLastArrItem: integer;
begin
  SetLength(vVarArray, Params.cArgs); // set our array to appropriate length

  // array boundaries
  vFistArrItem := Low(vVarArray);
  vLastArrItem := High(vVarArray);

  if Params.cNamedArgs > 0 then
    // Copy over data from Params in NamedArgs order
    for i := vFistArrItem to vLastArrItem do
    begin
      vPVarArgIn := @Params.rgvarg[i];
      vVarArray[Params.rgdispidNamedArgs[i]] := POleVariant(vPVarArgIn)^;
    end
  else
    // Copy over data from Params in reverse order
    for i := vFistArrItem to vLastArrItem do
    begin
      vPVarArgIn := @Params.rgvarg[i];
      vVarArray[vLastArrItem - i] := POleVariant(vPVarArgIn)^;
    end;

  cyInvokeEvent(DispID, vVarArray);
  SetLength(vVarArray, 0);

  inherited;
end;

procedure TcyBaseWebBrowser.cyInvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    250:                     // OnBeforeNavigate2 ...
      FNavigating := true; // Cannot be called here because OnBeforeNavigate2 called several times for URLs ...

    252:                     // OnNavigateComplete2 called for each frame completion ...
      begin
        // DefaultInterface is "parent" IWebBrowser :
        // vVarArray[0] => IDispatch !!!
        // Not working with non local html: if (IDispatch(Params[0]) as IWebBrowser2) = IWebBrowser2(DefaultInterface) then
          FNavigating := false;
      end;


    271:                     // OnNavigateError ...
      FNavigating := false;
  end;
end;

function TcyBaseWebBrowser.LoadFromStream(const Stream: TStream; const TextContent: TTextContent = tcDocument): boolean;
begin
  case TextContent of
    tcDocument:
    begin
      if not Assigned(Document)
      then NavigateToURL('about:blank');

      RESULT := InternalLoadDocumentFromStream(Stream);
    end;

    tcBodyHtml:
      RESULT := InternalLoadBodyHTMLFromStream(Stream);

    tcBodyAsText:
      RESULT := InternalLoadBodyTextFromStream(Stream);
  end;

  if not FAsynchMode
  then WaitForCompleteState;
end;

function TcyBaseWebBrowser.LoadFromString(aString: string; const TextContent: TTextContent = tcDocument): boolean;
var
  aCharset: String;
  StringStream: TStringStream;
  aEncoding: TEncoding;
begin
  RESULT := false;

  if TextContent = tcDocument
  then begin
    aEncoding := TEncoding.Default;
    aCharset := lowercase(Charset);

    if aCharset = 'utf-8'
    then begin
      aEncoding := TEncoding.UTF8;
      aString := AddHtmlUnicodePrefix(aString);
    end;

    if aCharset = 'unicode'
    then begin
      aEncoding := TEncoding.Unicode;
      aString := AddHtmlUnicodePrefix(aString);
    end;

    if aCharset = 'unicodefeff'
    then begin
      aEncoding := TEncoding.BigEndianUnicode;
      aString := AddHtmlUnicodePrefix(aString);
    end;
  end
  else
    aEncoding := TEncoding.Default;

  try
    StringStream := TStringStream.Create(aString, aEncoding);
    RESULT := LoadFromStream(StringStream, TextContent);
  finally
    StringStream.Free;
  end;
end;

function TcyBaseWebBrowser.LoadFromString(aString: string; StringCharEncoding: TEncoding; const TextContent: TTextContent = tcDocument): boolean;
var
  StringStream: TStringStream;
begin
  RESULT := false;

  if TextContent = tcDocument
  then begin
    // Apply charset to document :
    if StringCharEncoding = TEncoding.UTF8
    then begin
// no more needed using AddHtmlUnicodePrefix        CharSet := 'utf-8';
      aString := AddHtmlUnicodePrefix(aString);
    end;

    if StringCharEncoding = TEncoding.Unicode
    then begin
// no more needed using AddHtmlUnicodePrefix              CharSet := 'unicode';
      aString := AddHtmlUnicodePrefix(aString);
    end;

    if StringCharEncoding = TEncoding.BigEndianUnicode
    then begin
// no more needed using AddHtmlUnicodePrefix              CharSet := 'unicodefeff';
      aString := AddHtmlUnicodePrefix(aString);
    end;
  end;

  try
    StringStream := TStringStream.Create(aString, StringCharEncoding);
    RESULT := LoadFromStream(StringStream, TextContent);
  finally
    StringStream.Free;
  end;
end;

function TcyBaseWebBrowser.LoadFromFile(const FileName: string; const TextContent: TTextContent = tcDocument): boolean;
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    RESULT := LoadFromStream(FileStream, TextContent);
  finally
    FileStream.Free;
  end;
end;

function TcyBaseWebBrowser.SaveToStream(const Stream: TStream; const TextContent: TTextContent= tcDocument): boolean;
begin
  WaitForCompleteState;

  RESULT := false;
  if Assigned(Document)
  then
    case TextContent of
      tcDocument:   RESULT := InternalSaveDocumentToStream(Stream);
      tcBodyHtml:  RESULT := InternalSaveBodyHTMLToStream(Stream);
      tcBodyAsText: RESULT := InternalSaveBodyTextToStream(Stream);
    end;
end;

function TcyBaseWebBrowser.SaveToString(const TextContent: TTextContent = tcDocument): string;
var
  FDocumentEncoding: TEncoding;
  MemoryStream: TMemoryStream;
  StringStream: TStringStream;
begin
  RESULT := '';
  MemoryStream := TMemoryStream.Create;

  try
    if SaveToStream(MemoryStream, TextContent)
    then begin
      if TextContent = tcDocument
      then FDocumentEncoding := GetStreamEncoding(MemoryStream)
      else FDocumentEncoding := TEncoding.Default;

      try
        StringStream := TStringStream.Create('', FDocumentEncoding, true);
        MemoryStream.Seek(0, soFromBeginning);
        StringStream.CopyFrom(MemoryStream, MemoryStream.Size);
        Result := StringStream.DataString;

        // Remove any encoding prefix from html source :
        if TextContent = tcDocument
        then Result := RemoveHtmlUnicodePrefix(Result);
      finally
        StringStream.Free;
      end;
    end;
  finally
    MemoryStream.Free;
  end;
end;

procedure TcyBaseWebBrowser.SaveToFile(const FileName: string; const TextContent: TTextContent = tcDocument);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream, TextContent);
  finally
    FileStream.Free;
  end;
end;

procedure TcyBaseWebBrowser.SaveToFile(const FileName: string; Encoding: TEncoding; const TextContent: TTextContent = tcDocument);
var Lines: TStrings;
begin
  Lines := TStringList.Create;

  try
    Lines.Text := SaveToString(TextContent);
    Lines.SaveToFile(FileName, Encoding);
  finally
    Lines.Free;
  end;
end;

function TcyBaseWebBrowser.GetBusy: Boolean;
var _ReadyState: Cardinal;
begin
  _ReadyState := ReadyState;

  RESULT := _ReadyState < READYSTATE_INTERACTIVE;

{  RESULT := (_ReadyState <> READYSTATE_COMPLETE)   // Browse mode
              and (_ReadyState <> READYSTATE_INTERACTIVE);  // Editor mode ...  }
end;

function TcyBaseWebBrowser.GetCharset: String;
var Doc: IHTMLDocument2;
begin
  // Can be: "utf-8", "unicode", "unicodeFEFF" (unicode big endian) or something like "windows-1252" that is ANSI ...
  RESULT := '';

  if GetIHTMLDocument2(Doc)
  then RESULT := Doc.charset;
end;

procedure TcyBaseWebBrowser.SetAsynchMode(const Value: Boolean);
begin
  FAsynchMode := Value;
end;

procedure TcyBaseWebBrowser.SetCharset(const Value: String);
var Doc: IHTMLDocument2;
begin
  if GetIHTMLDocument2(Doc)
  then Doc.charset := Value;
end;

procedure TcyBaseWebBrowser.SetDesignMode(const Value: Boolean);
begin
  if FDesignMode <> Value
  then begin
    FDesignMode := Value;
    ApplyDesignModeToDocument;
  end;
end;

procedure TcyBaseWebBrowser.ApplyDesignModeToDocument;
var Doc: IHTMLDocument2;
begin
  if GetIHTMLDocument2(Doc)
  then
    if FDesignMode
    then Doc.designMode := IEDesignModeOn
    else Doc.DesignMode := IEDesignModeOff;
end;

procedure TcyBaseWebBrowser.WaitForCompleteState;

    procedure Wait_for_a_while(const ADelay: Cardinal);
    var StartTC: Cardinal;
    begin
      StartTC := Windows.GetTickCount;
      repeat
        Forms.Application.ProcessMessages;  // Cannot use this one ...
      until Int64(Windows.GetTickCount) - Int64(StartTC) >= ADelay;
    end;

begin
  while (Busy) and (not Forms.Application.Terminated) do
    Wait_for_a_while(10);
end;

function TcyBaseWebBrowser.ExecCommand(CmdID: WideString; ShowUI: Boolean; Value: OleVariant): Boolean;
var Doc: IHTMLDocument2;
begin
  Result := false;

  if GetIHTMLDocument2(Doc)
  then
    try
      Result := Doc.execCommand(CmdID, ShowUI, Value);
    except
    end
end;

function TcyBaseWebBrowser.GetIEHandle(const aClassName: string = 'Internet Explorer_Server'): Cardinal;
var
  szClass: array [0..255] of char;
  hwndChild, hwndTmp: Cardinal;
begin
  Result := 0;
  hwndTmp := Handle;

  while hwndTmp <> 0 do
  begin
    hwndChild := GetWindow(hwndTmp, GW_CHILD);
    GetClassName(hwndChild, szClass, SizeOf(szClass));

    if string(szClass) = aClassName
    then begin
      Result := hwndChild;
      hwndTmp := 0;
    end
    else
      hwndTmp := hwndChild;
  end;
end;

function TcyBaseWebBrowser.SetFocusToDoc: Boolean;
var Doc: IHTMLDocument2;
begin
  Result := false;

  if GetIHTMLDocument2(Doc)
  then Doc.ParentWindow.Focus;
end;

end.
