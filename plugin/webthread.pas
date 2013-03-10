{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    ***
    June 2007
    This code was modified by El Salvador <salvadorbs@gmail.com> for ASuite
    ***
}

unit WebThread;

interface

uses
  Windows, Classes, SysUtils, WinInet;

type
  PUpdateRec = ^TUpdateRec;
  TUpdateRec = packed record
    Name        : string;
    Url         : string;
    Filename    : string;
    Downloaded  : Boolean;
  end;

  TWTMessageReason = (wumrConnectSuccess,
    wumrConnectError,
    wumrRetrieveStart,
    wumrRetrieveProgress,
    wumrRetrieveSuccess,
    wumrRetrieveError,
    wumrDisconnect,
    wumrUnknownError);

  TWTStatus = (wtStopped, wtConnecting, wtDownloading, wtStopping, wtChangingFile);

  TWTProgressEvent = procedure(Sender: TObject; MsgCode: TWTMessageReason; Msg:
    string; TotalSize, BytesRead: Int64) of object;

  TWTErrorEvent = procedure(Sender: TObject; ErrorMsg: string; MsgCode: TWTMessageReason) of object;

  TWebThread = class(TThread)
  private
    { Private declarations }
    fStatus: TWTStatus;
    fAgent: PChar;
    fInternetHandle: HINTERNET;
    fFileHandle: HINTERNET;
    fFilesList: TList;
    fRemoteBase: string;
    fMsg: string;
    fTotalSize: cardinal;
    fBytesRead: cardinal;
    fCurrentIndex: integer;
    fMsgCode: TWTMessageReason;
    fErrorText: string;
    fOnMessage: TWTProgressEvent;
    fOnStatusChange: TNotifyEvent;
    fOnError: TWTErrorEvent;
    procedure SetMessage(MsgCode: TWTMessageReason; Msg: string);
    procedure AlertMainThread;
    procedure Error;
    procedure StatusChange;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    property Agent: PChar read fAgent write fAgent;
    property Status: TWTStatus read fStatus;
    property CurrentIndex: integer read fCurrentIndex;
    property Files: TList read fFilesList write fFilesList;
    property RemoteBase: string read fRemoteBase write fRemoteBase;
    property OnMessage: TWTProgressEvent read fOnMessage write fOnMessage;
    property OnStatusChange: TNotifyEvent read fOnStatusChange write fOnStatusChange;
    property OnError: TWTErrorEvent read fOnError write fOnError;
    property LastMessage: TWTMessageReason read fMsgCode;
    property ErrorText: string read FErrorText write FErrorText;
  end;

implementation

{ TWebThread }

procedure TWebThread.AlertMainThread;
begin
  if Assigned(fOnMessage) then
    fOnMessage(Self, fMsgCode, fMsg, fTotalSize, fBytesRead);
end;

procedure TWebThread.Error;
begin
  FStatus := wtStopped;
  if Assigned(FOnError) then
    FOnError(Self, FErrorText, fMsgCode);
end;

procedure TWebThread.StatusChange;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

constructor TWebThread.Create(CreateSuspended: boolean);
begin
  inherited;

  fFilesList := TList.Create;
end;

destructor TWebThread.Destroy;
begin
  fFilesList.Clear;
  fFilesList.Free;

  inherited;
end;

procedure TWebThread.Execute;
var
  Buffer: array[0..4095] of Char;
  CharsRead: cardinal;
  FileSize: integer;
  DownFile: string;
  TempPath, TempFile: array[0..MAX_PATH] of Char;
  hFile: cardinal;
  I: integer;
begin
  FStatus := wtStopped;
  try
    fInternetHandle := InternetOpen(fAgent,INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
    if fInternetHandle = nil then
    begin                            
      ErrorText := 'Could not initialize network';
      SetMessage(wumrConnectError, ErrorText);
      Synchronize(Error);
      Exit;
    end
    else begin
      SetMessage(wumrConnectSuccess, 'Connected');
      Synchronize(AlertMainThread);
    end;
    for I := 0 to fFilesList.Count - 1 do begin
      with PUpdateRec(fFilesList[I])^ do
      begin
        Downloaded := False;
        if Pos('http://', LowerCase(Url)) = 1 then
          DownFile := Url
        else
          DownFile := fRemoteBase + Url;
      end;
      fTotalSize    := 0;
      fBytesRead    := 0;
      fCurrentIndex := I;
      FStatus       := wtConnecting; 
      Synchronize(StatusChange);
      fFileHandle   := InternetOpenUrl(fInternetHandle, PChar(DownFile), nil, 0,
                                       {INTERNET_FLAG_PRAGMA_NOCACHE or }
                                       INTERNET_FLAG_RELOAD, 0);
      if fFileHandle = nil then
      begin       
        ErrorText := 'Could not start transferring remote file';
        SetMessage(wumrRetrieveError, ErrorText);
        Synchronize(Error);
        Continue;
      end
      else begin
        FStatus := wtDownloading;
        Synchronize(StatusChange);
        SetMessage(wumrRetrieveStart, 'Start transferring');
        Synchronize(AlertMainThread);
      end;
      FileSize := InternetSetFilePointer(fFileHandle, 0, nil, FILE_END, 0);
      if FileSize = -1 then
      begin   
        ErrorText := Format('File %s does not exist', [DownFile]);
        SetMessage(wumrRetrieveError, ErrorText);
        Synchronize(Error);
        InternetCloseHandle(fFileHandle);
        Continue;
      end
      else begin
        fTotalSize := FileSize;
        SetMessage(wumrRetrieveStart, Format('Downloading %s', [PUpdateRec(fFilesList[I])^.Name]));
        InternetSetFilePointer(fFileHandle, 0, nil, FILE_BEGIN, 0);
      end;
      Synchronize(AlertMainThread);
      // Get a temporary filename and create a file
      GetTempPath(SizeOf(TempPath), TempPath);
      GetTempFileName(TempPath, '~wu', 0, TempFile);
      // Transfer the file
      hFile := FileCreate(TempFile);
      FillChar(Buffer, SizeOf(Buffer), 0);
      repeat
        InternetReadFile(fFileHandle, @Buffer, SizeOf(Buffer), CharsRead);
        FileWrite(hFile, Buffer, CharsRead);
        fBytesRead := fBytesRead + CharsRead;
        SetMessage(wumrRetrieveProgress, '');
        Synchronize(AlertMainThread);
      until CharsRead = 0;
      FileClose(hFile);
      if GetLastError <> 0 then
      begin        
        ErrorText := 'Error retrieving remote file: ' + SysErrorMessage(GetLastError);
        SetMessage(wumrRetrieveError, ErrorText);
        DeleteFile(TempFile);
        Synchronize(Error);
        InternetCloseHandle(fFileHandle);
        Continue;
      end
      else begin
        FStatus := wtStopping;
        Synchronize(StatusChange);
        SetMessage(wumrRetrieveSuccess, 'Transfer completed');
        Synchronize(AlertMainThread);
        DeleteFile(PUpdateRec(fFilesList[I])^.Filename);
        MoveFile(TempFile, PChar(PUpdateRec(fFilesList[I])^.Filename));
        PUpdateRec(fFilesList[I]).Downloaded := True;
      end;
      InternetCloseHandle(fFileHandle);
      FStatus := wtChangingFile;  
      Synchronize(StatusChange);
    end;
    InternetCloseHandle(fInternetHandle);

    SetMessage(wumrDisconnect, 'Disconnected');
    Synchronize(AlertMainThread);
    if fFilesList.Count > 1 then
    begin
      FStatus := wtStopped;
      Synchronize(StatusChange);
    end;
  except
    ErrorText := SysErrorMessage(GetLastError);
    SetMessage(wumrUnknownError, ErrorText);
    Synchronize(Error);
  end;
end;

procedure TWebThread.SetMessage(MsgCode: TWTMessageReason; Msg: string);
begin
  fMsg := Msg;
  fMsgCode := MsgCode;
end;

end.