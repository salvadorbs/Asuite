{
Copyright (C) 2006-2008 Matteo Salvi of SalvadorSoftware

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit Update;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, IniFiles, ExtCtrls, ShellApi, WebThread;

type
  TfrmUpdate = class(TForm)
    gbDownloadInfo: TGroupBox;
    gbServerInfo: TGroupBox;
    lbTotalSize: TLabel;
    lbSpeed: TLabel;
    lbBytesReceived: TLabel;
    lbServerInfo: TLabel;
    pbDownload: TProgressBar;
    btnCancel: TButton;
    lbSpeed2: TLabel;
    lbBytesReceived2: TLabel;
    lbTotalSize2: TLabel;
    Timer1: TTimer;
    lbFileName: TLabel;
    lbFileName2: TLabel;
    function  GetUpdaterVersion(DosApp: String): String;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure TranslateForm(Lingua: string);
  private
    { Private declarations }
    function  CompareInt(int1, int2: Integer): Integer;
    function  CompareVersionNumbers(CurrentVersion, LastVersion: String;
                                    ASuite: Boolean): Integer;
  public
    procedure wThreadProgress(Sender: TObject; MsgCode: TWTMessageReason;
                              Msg: string; TotalSize, BytesRead: Int64);
    procedure wThreadError(Sender: TObject; ErrorMsg: string;
                           MsgCode: TWTMessageReason);
    procedure wThreadTerminate(Sender: TObject);
    procedure wThread2Terminate(Sender: TObject);
    procedure wThread2StatusChange(Sender: TObject);
    { Public declarations }
  end;

var
  frmUpdate  : TfrmUpdate;
  Time       : Integer;
  TimeActive : Boolean;
  MessageDownload : String;
    wThread, wThread2 : TWebThread;

implementation

uses Main, CommonUtils, Card;

{$R *.dfm}

procedure TfrmUpdate.TranslateForm(Lingua: string);
begin
  with frmMain.xmldTranslate.DocumentElement.ChildNodes['Form8'] do
  begin
    gbServerInfo.Caption   := ChildNodes['GroupboxCurrentTask'].Text;
    MessageDownload        := ChildNodes['MessageDownload'].Text;
    gbDownloadInfo.Caption := ChildNodes['GroupboxDownloadInfo'].Text;
    lbFileName.Caption     := ChildNodes['LabelFileName'].Text;
    lbSpeed.Caption        := ChildNodes['LabelSpeed'].Text;
    lbTotalSize.Caption    := ChildNodes['LabelTotalSize'].Text;
    lbBytesReceived.Caption := ChildNodes['LabelBytesReceived'].Text;
    btnCancel.Caption      := ChildNodes['ButtonCancel'].Text;
  end;
end;

procedure TfrmUpdate.wThread2StatusChange(Sender: TObject);
begin
  if (Sender as TWebThread).Status = wtChangingFile then
    Time := 0;
end;

procedure TfrmUpdate.wThread2Terminate(Sender: TObject);
var
  I : Integer;
  ErrorCode : Integer;
  IniFile   : TIniFile;
  UpdaterFileName : String;
begin
  for I := 0 to wThread2.Files.Count - 1 do
  begin
    if (pos('ASuite Install',PUpdateRec(wThread2.Files[I]).Name) = 1) and
       (FileExists(PUpdateRec(wThread2.Files[I]).Filename)) and
       (PUpdateRec(wThread2.Files[I]).Downloaded) then
    begin
      //Execute ASuite Updater to update old ASuite to new version
      IniFile := TIniFile.Create(ApplicationPath + UpdateName);
      UpdaterFileName := IniFile.ReadString('Updater', 'UpdaterFileName', '');
      IniFile.Free;
      ErrorCode := ShellExecute(GetDesktopWindow, 'open', PChar(UpdaterFileName),
                                nil, PChar(ApplicationPath), SW_NORMAL);
      if ErrorCode <= 32 then
        ShowMessageFmt(ArrayMessages[9],[UpdaterFileName]);
      //Close ASuite
      frmMain.Hide;
      ShutdownTime := True;
      if IsFormOpen('frmCard') then
        frmCard.Close;
      frmMain.Close;
    end;
    Dispose(PUpdateRec(wThread2.Files[I]));
  end;
end;

procedure TfrmUpdate.wThreadError(Sender: TObject; ErrorMsg: string;
  MsgCode: TWTMessageReason);
begin
  MessageDlg(ErrorMsg,mtError,[mbOK],0);
  //Close;
end;

procedure TfrmUpdate.wThreadProgress(Sender: TObject; MsgCode: TWTMessageReason;
  Msg: string; TotalSize, BytesRead: Int64);
begin
  //Update pbDownload
  pbDownload.Max      := TotalSize;
  pbDownload.Position := BytesRead;
  //Update labels
  if TotalSize >= 1024 then
    lbTotalSize2.Caption := IntToStr(TotalSize div 1024) + ' KB'
  else
    lbTotalSize2.Caption := IntToStr(TotalSize) + ' B';
  if BytesRead >= 1024 then
    lbBytesReceived2.Caption := IntToStr(BytesRead div 1024) + ' KB'
  else
    lbBytesReceived2.Caption := IntToStr(BytesRead) + ' B';
  if TotalSize <> 0 then
    Timer1.Enabled := True;
  if Time <> 0 then
    lbSpeed2.Caption  := IntToStr((BytesRead div Time) div 1024) + ' KB/s'
  else
    lbSpeed2.Caption  := IntToStr((BytesRead div 1) div 1024) + ' KB/s';
  if (Sender as TWebThread).Files.Count > 0 then
    with (Sender as TWebThread) do
    begin
      lbFileName2.Caption  := PUpdateRec(Files[CurrentIndex]).FileName;
      lbServerInfo.Caption := Format(MessageDownload,[PUpdateRec(Files[
        CurrentIndex]).Name]);
    end;
end;

procedure TfrmUpdate.wThreadTerminate(Sender: TObject);
var
  IniFile : TIniFile;
  NewVersion, NewVersion2, CurrentVersion, UpdaterName : string;
  UpdaterFile, ASuiteUpdateFile: PUpdateRec;
begin
  TimeActive     := False;
  if wThread.Files.Count > 0 then
  begin
    //Read update.ini and write value CurrentVersion (for ASuite Updater)
    if FileExists(ApplicationPath + UpdateName) and (PUpdateRec(wThread.Files[0]).Downloaded) then
    begin
      IniFile := TIniFile.Create(ApplicationPath + UpdateName);
      try
        IniFile.WriteString('ASuite', 'CurrentVersion', ReleaseVersion);
        NewVersion2    := IniFile.ReadString('ASuite', 'LastVersion', '');
        if (NewVersion2 = '') then
        begin
          ShowMessage(ArrayMessages[1]);
          btnCancelClick(Sender);
          Exit;
        end;
        //If ASuite is an old version, get new version and ASuite Updater
        if (CompareVersionNumbers(ReleaseVersion,NewVersion2, True) = 1) and
          (MessageDlg(ArrayMessages[24],mtConfirmation, mbOKCancel, 0) = mrOk) then
        begin
          TimeActive := True;
          //Create second webthread
          wThread2 := TWebThread.Create(True);
          with wThread2 do
          begin
            Agent            := 'ASuite WebUpdater';
            OnTerminate      := wThread2Terminate;
            OnMessage        := wThreadProgress;
            OnError          := wThreadError;
            OnStatusChange   := wThread2StatusChange;
            FreeOnTerminate  := True;
          end;
          //Download Updater
          //If don't exits (or it is an old version), get ASuite Updater
          UpdaterName    := IniFile.ReadString('Updater', 'UpdaterFileName', '');
          NewVersion     := IniFile.ReadString('Updater', 'LastVersion', '');
          CurrentVersion := GetUpdaterVersion(UpdaterName + ' -version');
          if (Not FileExists(UpdaterName)) or
             (CompareVersionNumbers(CurrentVersion, NewVersion, False) = 1) then
          begin
            TimeActive  := True;
            //Create and add new PUpdateRec in wThread2
            UpdaterFile := New(PUpdateRec);
            with UpdaterFile^ do
            begin
              Name     := 'ASuite Updater ' + CurrentVersion;
              Filename := UpdaterName;
              Url      := IniFile.ReadString('Updater', 'DownloadUrl', '');
            end;
            wThread2.Files.Add(UpdaterFile);
          end;
          //Download ASuite
          //Create and add new PUpdateRec in wThread2
          ASuiteUpdateFile := New(PUpdateRec);
          with ASuiteUpdateFile^ do
          begin
            Name     := 'ASuite Install ' + NewVersion2;
            FileName := IniFile.ReadString('ASuite', 'UpdateFileName', '');
            Url      := IniFile.ReadString('ASuite', 'DownloadUrl', '');
          end;
          wThread2.Files.Add(ASuiteUpdateFile);
          wThread2.Resume;
        end
        else begin
          if CompareVersionNumbers(ReleaseVersion,NewVersion2, True) <> 1 then
            ShowMessage(ArrayMessages[25]);
          btnCancelClick(Sender);
        end;
      finally
        IniFile.Free;
      end;
    end;
    Dispose(PUpdateRec(wThread.Files[0]));
  end;
end;

function TfrmUpdate.GetUpdaterVersion(DosApp: String): String;
const
  ReadBuffer = 2400;
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PChar;
  BytesRead: DWord;
  Apprunning: DWord;
begin
  Result := '';
  with Security do
  begin
    nlength := SizeOf(TSecurityAttributes);
    binherithandle := True;
    lpsecuritydescriptor := nil;
  end;
  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then
  begin
    Buffer := AllocMem(ReadBuffer + 1);
    FillChar(Start, Sizeof(Start), #0);
    start.cb          := SizeOf(start);
    start.hStdOutput  := WritePipe;
    start.hStdInput   := ReadPipe;
    start.dwFlags     := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    start.wShowWindow := SW_HIDE;
    if CreateProcess(nil, PChar(DosApp), @Security, @Security, True,
      NORMAL_PRIORITY_CLASS, nil, nil, start, ProcessInfo) then
    begin
      repeat
        Apprunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
        Application.ProcessMessages;
      until (Apprunning <> WAIT_TIMEOUT);
      repeat
        BytesRead := 0;
        ReadFile(ReadPipe, Buffer[0], ReadBuffer, BytesRead, nil);
        Buffer[BytesRead] := #0;
        OemToAnsi(Buffer, Buffer);
        Result := string(Buffer);
      until (BytesRead < ReadBuffer);
    end;
    FreeMem(Buffer);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ReadPipe);
    CloseHandle(WritePipe);
  end;
end;

procedure TfrmUpdate.Timer1Timer(Sender: TObject);
begin
  if TimeActive then
    Inc(Time)
  else
    Time := 0;
end;

procedure TfrmUpdate.FormCreate(Sender: TObject);
var
  UpdateFileIni: PUpdateRec;
begin
  TranslateForm(LauncherOptions.LangName);
  //Get update.ini
  lbServerInfo.Caption := Format(MessageDownload,[UpdateName]);
  //Create PUpdateRec
  UpdateFileIni := New(PUpdateRec);
  with UpdateFileIni^ do
  begin
    Name     := 'Update File';
    Filename := UpdateName;
    Url      := UpdateUrl + UpdateName;
  end;
  //Create first webthread
  wThread := TWebThread.Create(True);
  with wThread do
  begin
    Files.Add(UpdateFileIni);
    Agent            := 'ASuite WebUpdater';
    OnTerminate      := wThreadTerminate;
    OnMessage        := wThreadProgress;
    OnError          := wThreadError;
    FreeOnTerminate  := True;
    TimeActive       := True;
    Resume;
  end;
end;

procedure TfrmUpdate.btnCancelClick(Sender: TObject);
begin
  if Assigned(wThread) then
  begin
    if (wThread.Status <> wtStopped) then
    begin
      wThread.Terminate;
    end
    else
      wThread.Suspend;
  end;
  if Assigned(wThread2) then
  begin
    if (wThread2.Status <> wtStopped) then
    begin
      wThread2.Terminate;
    end
    else
      wThread2.Suspend;
  end;
  close;
end;   

function TfrmUpdate.CompareInt(int1, int2: Integer): Integer;
begin
  if int1 < int2 then
    Result := -1
  else
    if int1 > int2 then
      Result := 1
    else
      Result := 0;
end;

function TfrmUpdate.CompareVersionNumbers(CurrentVersion, LastVersion: String;
                                          ASuite: Boolean): Integer;
var
  N1, N2: Integer;

  function GetNextNumber(var Version: String): Integer;
  var
    P: Integer;
    S: string;
  begin
    P := Pos('.', Version);
    if P > 0 then
    begin
      S := Copy(Version, 1, P - 1);
      Version := Copy(Version, P + 1, Length(Version) - P);
    end
    else begin
      S := Version;
      Version := '';
    end;
    if (S = '') then
      Result := -1
    else
      try
        Result := StrToInt(S);
      except
        Result := -1;
      end;
  end;

begin
  repeat
    //Get next version number
    N1 := GetNextNumber(CurrentVersion);
    N2 := GetNextNumber(LastVersion);
    //Compare numbers
    Result := CompareInt(N2, N1);
  until ((CurrentVersion = '') and (LastVersion = '')) or
        ((Result = 1) or (Result = -1));
  //Check beta version in ASuite (if CurrentVersion is equal to LastVersion)
  if ((PreReleaseVersion) <> '') and (ASuite) and (Result = 0) then
    Result := 1;
end;

end.
