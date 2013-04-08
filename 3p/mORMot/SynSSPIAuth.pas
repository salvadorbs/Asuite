/// low level access to Windows Authentication for the Win32 platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSSPIAuth;
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

  The Initial Developer of the Original Code is Chaa.

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


  This unit has been contributed by Chaa.
  See http://synopse.info/forum/viewtopic.php?pid=5619#p5619

  Thanks A LOT for this great contribution to the framework!


  Version 1.18
  - initial release, with code submitted by Chaa


}

interface

uses
    SysUtils, SynCommons;

{$I Synopse.inc} // define HASINLINE

type
    /// Windows Authentication context handle
    TSecHandle = record
      dwLower: PtrInt;
      dwUpper: PtrInt;
    end;
    PSecHandle = ^TSecHandle;

    /// Windows Authentication context
    TSecContext = record
      CredHandle: TSecHandle;
      CtxHandle: TSecHandle;
      ID: Cardinal;
      Created: Cardinal;
    end;
    PSecContext = ^TSecContext;

    /// dynamic array of Windows Authentication contexts
    // - used to hold information between calls to ServerSSPIAuth
    TSecContexts = array of TSecContext;

/// Sets aSecHandle fields to empty state
procedure InvalidateSecContext(var aSecContext: TSecContext);{$ifdef HASINLINE}inline;{$endif}

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again width data, returned from servsr
function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data recieved from client
// - aOutData contains data that must be sent to client
// - if function returns True, server must send aOutData to client
// and call function again width data, returned from client
function ServerSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previos success call to ServerSSPIAuth
// - aUserName contains authenticated user name
procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);

/// Free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);


implementation

uses
    Windows;

type
  TSecBuffer = record
    cbBuffer: Cardinal;   // Size of the buffer, in bytes
    BufferType: Cardinal; // Type of the buffer
    pvBuffer: Pointer;    // Pointer to the buffer
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: Cardinal;  // Version number
    cBuffers: Cardinal;   // Number of buffers
    pBuffers: PSecBuffer; // Pointer to array of buffers
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TSecPkgInfoW = record
    fCapabilities: Cardinal; // Capability bitmask
    wVersion: Word;       // Version of driver
    wRPCID: Word;         // ID for RPC Runtime
    cbMaxToken: Cardinal; // Size of authentication token (max)
    Name: PWideChar;      // Text name
    Comment: PWideChar;   // Comment
  end;
  PSecPkgInfoW = ^TSecPkgInfoW;

const
  SECBUFFER_VERSION = 0;
  SECBUFFER_TOKEN = 2;
  SECPKG_CRED_INBOUND  = $00000001;
  SECPKG_CRED_OUTBOUND = $00000002;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ISC_REQ_CONNECTION = $00000800;
  ASC_REQ_ALLOCATE_MEMORY = $00000100;
  ASC_REQ_CONNECTION = $00000800;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_COMPLETE_NEEDED = $00090313;
  SEC_I_COMPLETE_AND_CONTINUE = $00090314;
  secur32 = 'secur32.dll';
  SecPkgName = 'Negotiate';

function QuerySecurityPackageInfoW(pszPackageName: PWideChar;
  var ppPackageInfo: PSecPkgInfoW): Integer; stdcall;
  external secur32 name 'QuerySecurityPackageInfoW';

function AcquireCredentialsHandleW(pszPrincipal, pszPackage: PWideChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
  pGetKeyFn: Pointer; pvGetKeyArgument: Pointer; phCredential: PSecHandle;
  var ptsExpiry: LARGE_INTEGER): Integer; stdcall;
  external secur32 name 'AcquireCredentialsHandleW';

function InitializeSecurityContextW(phCredential: PSecHandle; phContext: PSecHandle;
  pszTargetName: PWideChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PSecHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; var ptsExpiry: LARGE_INTEGER): Integer; stdcall;
  external secur32 name 'InitializeSecurityContextW';

function AcceptSecurityContext(phCredential: PSecHandle; phContext: PSecHandle;
  pInput: PSecBufferDesc; fContextReq, TargetDataRep: Cardinal;
  phNewContext: PSecHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  var ptsExpiry: LARGE_INTEGER): Integer; stdcall;
  external secur32 name 'AcceptSecurityContext';

function CompleteAuthToken(phContext: PSecHandle; pToken: PSecBufferDesc): Integer; stdcall;
  external secur32 name 'CompleteAuthToken';

function QuerySecurityContextToken(phContext: PSecHandle; var Token: THandle): Integer; stdcall;
  external secur32 name 'QuerySecurityContextToken';

function FreeContextBuffer(pvContextBuffer: Pointer): Integer; stdcall;
  external secur32 name 'FreeContextBuffer';

function DeleteSecurityContext(phContext: PSecHandle): Integer; stdcall;
  external secur32 name 'DeleteSecurityContext';

function FreeCredentialsHandle(phCredential: PSecHandle): Integer; stdcall;
  external secur32 name 'FreeCredentialsHandle';

procedure InvalidateSecContext(var aSecContext: TSecContext);
begin
    aSecContext.CredHandle.dwLower := -1;
    aSecContext.CredHandle.dwUpper := -1;
    aSecContext.CtxHandle.dwLower := -1;
    aSecContext.CtxHandle.dwUpper := -1;
    aSecContext.ID := 0;
    aSecContext.Created := 0;
end;

function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;
var
    InBuf: TSecBuffer;
    InDesc: TSecBufferDesc;
    InDescPtr: PSecBufferDesc;
    SecPkgInfo: PSecPkgInfoW;
    Expiry: LARGE_INTEGER;
    LInCtxPtr: PSecHandle;
    OutBuf: TSecBuffer;
    OutDesc: TSecBufferDesc;
    CtxAttr: Cardinal;
    Status: Integer;
begin
    InBuf.BufferType := SECBUFFER_TOKEN;
    InBuf.cbBuffer := Length(aInData);
    InBuf.pvBuffer := PByte(aInData);

    if (aSecContext.CredHandle.dwLower = -1) and (aSecContext.CredHandle.dwUpper = -1) then
    begin
        aSecContext.Created := GetTickCount();

        if QuerySecurityPackageInfoW(SecPkgName, SecPkgInfo) <> 0 then
            RaiseLastOSError();
        try
            if AcquireCredentialsHandleW(nil, SecPkgInfo.Name, SECPKG_CRED_OUTBOUND, nil, nil, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
                RaiseLastOSError();
        finally
            FreeContextBuffer(SecPkgInfo);
        end;

        InDescPtr := nil;
        LInCtxPtr := nil;
    end
    else
    begin
        InDesc.ulVersion := SECBUFFER_VERSION;
        InDesc.cBuffers := 1;
        InDesc.pBuffers := @InBuf;

        InDescPtr := @InDesc;
        LInCtxPtr := @aSecContext.CtxHandle;
    end;

    OutBuf.BufferType := SECBUFFER_TOKEN;
    OutBuf.cbBuffer := 0;
    OutBuf.pvBuffer := nil;
    OutDesc.ulVersion := SECBUFFER_VERSION;
    OutDesc.cBuffers := 1;
    OutDesc.pBuffers := @OutBuf;

    Status := InitializeSecurityContextW(@aSecContext.CredHandle, LInCtxPtr, nil,
        ISC_REQ_CONNECTION or ISC_REQ_ALLOCATE_MEMORY,
        0, 0, InDescPtr, 0, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);

    if (Status = SEC_I_COMPLETE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    begin
        Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
    end;

    if Status < 0 then
        RaiseLastOSError();

    SetString(aOutData,PAnsiChar(OutBuf.pvBuffer),OutBuf.cbBuffer);
    FreeContextBuffer(OutBuf.pvBuffer);

    Result := Status = SEC_I_CONTINUE_NEEDED;
end;

function ServerSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;
var
    InBuf: TSecBuffer;
    InDesc: TSecBufferDesc;
    SecPkgInfo: PSecPkgInfoW;
    Expiry: LARGE_INTEGER;
    LInCtxPtr: PSecHandle;
    OutBuf: TSecBuffer;
    OutDesc: TSecBufferDesc;
    CtxAttr: Cardinal;
    Status: Integer;
begin
    InBuf.BufferType := SECBUFFER_TOKEN;
    InBuf.cbBuffer := Length(aInData);
    InBuf.pvBuffer := PByte(aInData);
    InDesc.ulVersion := SECBUFFER_VERSION;
    InDesc.cBuffers := 1;
    InDesc.pBuffers := @InBuf;

    if (aSecContext.CredHandle.dwLower = -1) and (aSecContext.CredHandle.dwUpper = -1) then
    begin
        aSecContext.Created := GetTickCount();

        if QuerySecurityPackageInfoW(SecPkgName, SecPkgInfo) <> 0 then
            RaiseLastOSError();
        try
            if AcquireCredentialsHandleW(nil, SecPkgInfo.Name, SECPKG_CRED_INBOUND, nil, nil, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
                RaiseLastOSError();
        finally
            FreeContextBuffer(SecPkgInfo);
        end;

        LInCtxPtr := nil;
    end
    else
    begin
        LInCtxPtr := @aSecContext.CtxHandle;
    end;

    OutBuf.BufferType := SECBUFFER_TOKEN;
    OutBuf.cbBuffer := 0;
    OutBuf.pvBuffer := nil;
    OutDesc.ulVersion := SECBUFFER_VERSION;
    OutDesc.cBuffers := 1;
    OutDesc.pBuffers := @OutBuf;

    Status := AcceptSecurityContext(@aSecContext.CredHandle, LInCtxPtr, @InDesc,
        ASC_REQ_CONNECTION or ASC_REQ_ALLOCATE_MEMORY,
        0, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);

    if (Status = SEC_I_COMPLETE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    begin
        Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
    end;

    if Status < 0 then
        RaiseLastOSError();

    SetLength(aOutData, OutBuf.cbBuffer);
    Move(PByte(OutBuf.pvBuffer)^, PByte(aOutData)^, OutBuf.cbBuffer);
    FreeContextBuffer(OutBuf.pvBuffer);

    Result := Status = SEC_I_CONTINUE_NEEDED;
end;

procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);
var
    UserToken: THandle;
    UserInfo: PSIDAndAttributes;
    Size: Cardinal;
    NameBuf: array [0..255] of Char;
    NameLen: Cardinal;
    DomainBuf: array [0..255] of Char;
    DomainLen: Cardinal;
    NameType: Cardinal;
begin
    aUserName := '';
    if QuerySecurityContextToken(@aSecContext.CtxHandle, UserToken) <> 0 then
        RaiseLastOSError();
    try
        Size := 0;
        GetTokenInformation(UserToken, TokenUser, nil, 0, Size);
        UserInfo := AllocMem(Size);
        try
            if not GetTokenInformation(Cardinal(UserToken), Windows.TokenUser, UserInfo, Size, Size) then
                RaiseLastOSError();
            FillChar(NameBuf[0], SizeOf(NameBuf), 0);
            NameLen := 256;
            FillChar(DomainBuf[0], SizeOf(DomainBuf), 0);
            DomainLen := 256;
            if not LookupAccountSid(nil, UserInfo.Sid, NameBuf, NameLen, DomainBuf, DomainLen, NameType) then
                RaiseLastOSError();
            if NameType = SidTypeUser then
            begin
                aUserName := FormatUTF8('%\%', [DomainBuf, NameBuf]);
            end;
        finally
            FreeMem(UserInfo);
        end;
    finally
        CloseHandle(UserToken);
    end;
end;

procedure FreeSecContext(var aSecContext: TSecContext);
begin
    if (aSecContext.CtxHandle.dwLower <> -1) or (aSecContext.CtxHandle.dwUpper <> -1) then
    begin
        DeleteSecurityContext(@aSecContext.CtxHandle);
        aSecContext.CtxHandle.dwLower := -1;
        aSecContext.CtxHandle.dwUpper := -1;
    end;
    if (aSecContext.CredHandle.dwLower <> -1) or (aSecContext.CredHandle.dwUpper <> -1) then
    begin
        FreeCredentialsHandle(@aSecContext.CredHandle);
        aSecContext.CredHandle.dwLower := -1;
        aSecContext.CredHandle.dwUpper := -1;
    end;
end;

end.
