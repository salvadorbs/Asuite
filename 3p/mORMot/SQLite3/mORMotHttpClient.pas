/// HTTP/1.1 RESTFUL JSON Client classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotHttpClient;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2013 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2013
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****



      HTTP/1.1 RESTFUL JSON Client for mORMot
    ******************************************

   - use internaly the JSON format for content communication
   - can be called by any JSON-aware AJAX application
   - can optionaly compress the returned data to optimize Internet bandwidth
   - speed is very high: more than 20MB/sec R/W localy on a 1.8GHz Sempron,
     i.e. 400Mb/sec of duplex raw IP data, with about 200 µs only elapsed
     by request (direct call is 50 µs, so bottle neck is the Win32 API),
     i.e. 5000 requests per second, with 113 result rows (i.e. 4803 bytes
     of JSON data each)... try to find a faster JSON HTTP server! ;)

    Initial version: 2009 May, by Arnaud Bouchez

    Version 1.1
      - code rewrite for FPC and Delphi 2009/2010 compilation

    Version 1.3 - January 22, 2010
      - some small fixes and multi-compiler enhancements

    Version 1.4 - February 08, 2010
      - whole Synopse SQLite3 database framework released under the GNU Lesser
        General Public License version 3, instead of generic "Public Domain"
      - HTTP/1.1 RESTFUL JSON Client and Server split into two units
        (SQLite3HttpClient and SQLite3HttpServer)

    Version 1.5 - February 12, 2010
      - test HTTP connection in both KeepAlive and with new connection for
        each request (an issue with no KeepAlive connections was detected)

    Version 1.13
      - now can compress its content using deflate or faster SynLZ algorithm:
        by default, the SynLZ algorithm will be used between a Delphi Client
        and Server over HTTP/1.1 - there will be no speed penalty on the
        server side, whereas deflate would use much more CPU
      - can make TCP/IP stream not HTTP compliant (against antivirus slowdown)
      - new TSQLite3HttpClientWinINet class, using WinINet API (very slow)
      - new TSQLite3HttpClientWinHTTP class, using WinHTTP API (fast and stable):
        this class should be considered to be used instead of TSQLite3HttpClient
        for any HTTP/1.1 client connection over a network - it is therefore
        the default TSQLite3HttpClient class since this 1.13 revision

    Version 1.16
      - fixed GPF issue at closing
      - fixed unnecessary dual URL signing (when authentication actived)

    Version 1.17
      - added optional aProxyName, aProxyByPass parameters to
        TSQLite3HttpClientWinGeneric / TSQLite3HttpClientWinINet and
        TSQLite3HttpClientWinHTTP constructors
		
    Version 1.18
	   - unit SQLite3HttpClient.pas renamed mORMotHttpClient.pas
	   - classes TSQLite3HttpClient* renamed as TSQLHttpClient*
     - fixed TSQLHttpClientGeneric.InternalURI() method to raise an explicit
       exception on connection error (as expected by TSQLRestClientURI.URI)
     - TSQLHttpClient* classes will now handle properly reconnection in case
       of connection break via overriden InternalCheckOpen/InternalClose methods
     - introducing TSQLHttpClientGeneric.Compression property to set the handled
       compression schemes at runtime

}

interface

{.$define USETCPPREFIX}
{ if defined, a prefix will be added to the TCP/IP stream so that it won't be
  valid HTTP content any more: it could increase the client/server speed with
  some anti-virus software, but the remote access won't work any more with
  Internet Browsers nor AJAX applications
  - not defined by default - should be set globally to the project conditionals }

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 WITHLOG

uses
{$ifdef MSWINDOWS}
  Windows,
  {$define USEWININET}
{$else}
  {$undef USEWININET}
{$endif}
  SysUtils,
  SynZip,
  SynLZ,
  SynCrtSock,
  SynCommons,
  mORMot;

type
  /// available compression algorithms for transmission
  // - SynLZ is faster then Deflate, but not standard: use hcSynLZ for Delphi
  // clients, but hcDeflate for AJAX or any HTTP clients
  // - with hcSynLZ, the 440 KB JSON for TTestClientServerAccess._TSQLHttpClient
  // is compressed into 106 KB with no speed penalty (it's even a bit faster)
  // whereas deflate, hcDeflate with its level set to 1 (fastest), is 25 % slower
  TSQLHttpCompression = (hcSynLZ, hcDeflate);

  /// set of available compressions schemes
  TSQLHttpCompressions = set of TSQLHttpCompression;

  /// Generic HTTP/1.1 RESTFUL JSON mORMot Client class
  TSQLHttpClientGeneric = class(TSQLRestClientURI)
  private
    fKeepAliveMS: cardinal;
    fCompression: TSQLHttpCompressions;
    procedure SetCompression(Value: TSQLHttpCompressions);
    procedure SetKeepAliveMS(Value: cardinal);
  protected
    /// process low-level HTTP/1.1 request
    // - call by URI() public method
    // - returns 200,202,204 if OK, http status error otherwize in result.Lo
    // - returns Server-InternalState in result.Hi
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; virtual; abstract;
    /// method calling the RESTful server fServer via HTTP/1.1
    // - calls the InternalRequest() protected method
    function InternalURI(const url, method: RawUTF8; Resp: PRawUTF8=nil;
      Head: PRawUTF8=nil; SendData: PRawUTF8=nil): Int64Rec; override;
  public
    /// the time (in milliseconds) to keep the connection alive with the
    // TSQLHttpServer
    // - default is 20000, i.e. 20 seconds
    property KeepAliveMS: cardinal read fKeepAliveMS write SetKeepAliveMS;
    /// the compression algorithms usable with this client
    // - default is [], i.e. no compression
    // - for a Delphi client, 
    property Compression: TSQLHttpCompressions read fCompression write SetCompression;
  end;

  /// HTTP/1.1 RESTFUL JSON mORMot Client class using SynCrtSock / WinSock
  // - will give the best performance on a local computer, but has been found
  // out to be slower over a network
  // - is not able to use secure HTTPS protocol
  TSQLHttpClientWinSock = class(TSQLHttpClientGeneric)
  protected
    /// internal HTTP/1.1 compatible client
    fSocket: THttpClientSocket;
    /// connection parameters as set by Create()
    fServer, fPort: AnsiString;
    /// call fSocket.Request()
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; override;
    /// overriden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// overriden protected method to close HTTP connection
    procedure InternalClose; override;
  public
    /// connect to TSQLHttpServer on aServer:aPort
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel); reintroduce;
    /// internal HTTP/1.1 compatible client
    property Socket: THttpClientSocket read fSocket;
  end;

{$ifdef USEWININET}
  /// HTTP/1.1 RESTFUL JSON mORMot Client abstract class using either WinINet
  // either TWinHTTP API
  // - not to be called directly, but via TSQLHttpClientWinINet or (even
  // better) TSQLHttpClientWinHTTP overriden classes 
  TSQLHttpClientWinGeneric = class(TSQLHttpClientGeneric)
  protected
    fWinAPI: TWinHttpAPI;
    fServer, fPort, fProxyName, fProxyByPass: AnsiString;
    fHttps: boolean;
    /// call fWinAPI.Request()
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; override;
    /// overriden protected method to close HTTP connection
    procedure InternalClose; override;
    /// overriden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// create a fWinAPI instance if needed
    procedure InternalCreate; virtual; abstract;
  public
    /// connect to TSQLHttpServer on aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      aHttps: boolean=false; const aProxyName: AnsiString='';
      const aProxyByPass: AnsiString=''); reintroduce;
    /// internal class instance used for the connection
    // - will return either a TWinINet, either a TWinHTTP class instance
    property WinAPI: TWinHttpAPI read fWinAPI;
  end;

  /// HTTP/1.1 RESTFUL JSON mORMot Client class using WinINet API
  // - this class is 15/20 times slower than TSQLHttpClient using SynCrtSock
  // on a local machine, but was found to be faster throughout local networks
  // - this class is able to connect via the secure HTTPS protocol
  // - it will retrieve by default the Internet Explorer proxy settings, and 
  // display some error messages or authentification dialog on screen
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinINet API should not be used from a service
  // - is implemented by creating a TWinINet internal class instance
  TSQLHttpClientWinINet = class(TSQLHttpClientWinGeneric)
  protected
    procedure InternalCreate; override;
  end;

  {{ HTTP/1.1 RESTFUL JSON Client class using WinHTTP API
   - has a common behavior as THttpClientSocket() but seems to be faster
     over a network and is able to retrieve the current proxy settings
     (if available) and handle secure HTTPS connection - so it seems to be used
     in your client programs: TSQLHttpClient will therefore map to this class
   - WinHTTP does not share directly any proxy settings with Internet Explorer.
     The default WinHTTP proxy configuration is set by either
     proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
     netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
     you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
     the current user's proxy settings for Internet Explorer (under 64 bit
     Vista/Seven, to configure applications using the 32 bit WinHttp settings,
     call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
   - you can optionaly specify manual Proxy settings at constructor level
   - by design, the WinHTTP API can be used from a service or a server
   - is implemented by creating a TWinHTTP internal class instance }
  TSQLHttpClientWinHTTP = class(TSQLHttpClientWinGeneric)
  protected
    procedure InternalCreate; override;
  end;

  /// HTTP/1.1 RESTFUL JSON default mORMot Client class
  // - under Windows, map the TSQLHttpClientWinHTTP class 
  TSQLHttpClient = TSQLHttpClientWinHTTP;
{$else}
  /// HTTP/1.1 RESTFUL JSON deault mORMot Client class 
  // - not unders Windows, map the WinSock implementation class
  TSQLHttpClient = TSQLHttpClientWinSock;
{$endif}


implementation


{ TSQLHttpClientGeneric }

function TSQLHttpClientGeneric.InternalURI(const url, method: RawUTF8;
  Resp, Head, SendData: PRawUTF8): Int64Rec;
var Headers, Content, ContentType: RawUTF8;
    P: PUTF8Char;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
{$ifdef WITHLOG}
  Log := TSQLLog.Enter(self);
{$endif}
  if InternalCheckOpen then begin
    if SendData<>nil then
      Content := SendData^;
    if Head<>nil then begin
      Headers := Head^;
      P := pointer(Headers);
      if IdemPChar(P,'CONTENT-TYPE:') then begin
        inc(P,14);
        if Content<>'' then begin
          ContentType := GetMimeContentType(pointer(Content),Length(Content));
          if ContentType='application/octet-stream' then
            ContentType := '';
        end;
        if ContentType='' then
          ContentType := GetNextLine(P,P);
        Headers := ''; // header is processed -> no need to send Content-Type twice
      end;
    end;
    if ContentType='' then
      ContentType := JSON_CONTENT_TYPE;
    result := InternalRequest(url,method,Headers,Content,ContentType);
    if Resp<>nil then
      Resp^ := Content;
    if Head<>nil then
      Head^ := Headers;
  end else
    Int64(result) := HTML_NOTIMPLEMENTED; // 501
{$ifdef WITHLOG}
  Log.Log(sllClient,'% % result.Lo=% .Hi=%',[method,url,result.Lo,result.Hi],self);
{$endif}
end;

procedure TSQLHttpClientGeneric.SetCompression(Value: TSQLHttpCompressions);
begin
  fCompression := Value;
  InternalClose; // force re-create connection at next requet
end;

procedure TSQLHttpClientGeneric.SetKeepAliveMS(Value: cardinal);
begin
  fKeepAliveMS := Value;
  InternalClose; // force re-create connection at next requet
end;


{ TSQLHttpClientWinSock }

constructor TSQLHttpClientWinSock.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel);
begin
  inherited Create(aModel);
  fServer := aServer;
  fPort := aPort;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
end;

function TSQLHttpClientWinSock.InternalCheckOpen: boolean;
begin
  if fSocket<>nil then begin
    result := true;
    exit;
  end;
  try
    fSocket := THttpClientSocket.Open(fServer,fPort,cslTCP,1000); // 1 sec timeout
    {$ifdef USETCPPREFIX}
    fSocket.TCPPrefix := 'magic';
    {$endif}
    // note that first registered algo (i.e. hcSynLZ) will be the prefered one 
    if hcSynLz in Compression then
      // SynLZ is very fast and efficient, perfect for a Delphi Client
      fSocket.RegisterCompress(CompressSynLZ);
    if hcDeflate in Compression then
      // standard (slower) AJAX/HTTP zip/deflate compression
      fSocket.RegisterCompress(CompressDeflate);
    result := true;
  except
    on Exception do begin
      FreeAndNil(fSocket);
      result := false;
    end;
  end;
end;

procedure TSQLHttpClientWinSock.InternalClose;
begin
  try
    FreeAndNil(fSocket);
  except
    ; // ignore any error here
  end;
end;

function TSQLHttpClientWinSock.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
begin
  result.Lo := fSocket.Request(url,method,KeepAliveMS,Header,Data,DataType,false);
  result.Hi := GetCardinal(pointer(fSocket.HeaderValue('Server-InternalState')));
  Header := fSocket.HeaderGetText;
  Data := fSocket.Content;
end;


{$ifdef USEWININET}

{ TSQLHttpClientWinGeneric }

constructor TSQLHttpClientWinGeneric.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel; aHttps: boolean; const aProxyName, aProxyByPass: AnsiString);
begin
  inherited Create(aModel);
  fServer := aServer;
  fPort := aPort;
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
end;

function TSQLHttpClientWinGeneric.InternalCheckOpen: boolean;
begin
  result := false;
  if fWinAPI=nil then
  try
    InternalCreate;
    if fWinAPI<>nil then begin
      // note that first registered algo (i.e. hcSynLZ) will be the prefered one 
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fWinAPI.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP zip/deflate compression
        fWinAPI.RegisterCompress(CompressDeflate);
      result := true;
    end;
  except
    on Exception do
      fWinAPI := nil;
  end else
    result := true;
end;

procedure TSQLHttpClientWinGeneric.InternalClose;
begin
  FreeAndNil(fWinAPI);
end;

function TSQLHttpClientWinGeneric.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
var OutHeader, OutData: RawByteString;
begin
  if fWinAPI=nil then
    result.Lo := HTML_NOTIMPLEMENTED else begin
    result.Lo := fWinAPI.Request(url,method,KeepAliveMS,Header,Data,DataType,
      OutHeader,OutData);
    result.Hi := GetCardinal(pointer(
      FindIniNameValue(pointer(OutHeader),'SERVER-INTERNALSTATE: ')));
    Header := OutHeader;
    Data := OutData;
  end;
end;


{ TSQLHttpClientWinINet }

procedure TSQLHttpClientWinINet.InternalCreate;
begin
  fWinAPI := TWinINet.Create(fServer,fPort,fHttps,fProxyName,fProxyByPass);
end;


{ TSQLHttpClientWinHTTP }

procedure TSQLHttpClientWinHTTP.InternalCreate;
begin
  fWinAPI := TWinHTTP.Create(fServer,fPort,fHttps,fProxyName,fProxyByPass);
end;

{$endif}

end.
