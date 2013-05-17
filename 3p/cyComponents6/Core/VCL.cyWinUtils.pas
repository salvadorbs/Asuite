{   Unit VCL.cyWinUtils

    Description:
    Unit with windows functions.

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

unit VCL.cyWinUtils;

{$I cyCompilerDefines.inc}

interface

uses Windows, Forms, Messages, WinSpool, classes, SysUtils, ShellAPI, ShlObj, ComObj, ActiveX, cyStrUtils, VCL.cySysUtils;

type
  TWindowsVersion = (wvUnknown, wvWin31, wvWin95, wvWin98, wvWinMe, wvWinNt3, wvWinNt4, wvWin2000, wvWinXP, wvWinVista,wvWin7, wvWin8, wvWin8_Or_Upper);


  function ShellGetExtensionName(FileName: String): String;
  // Get file extension description

  function ShellGetIconIndex(FileName: String): Integer;
  // Volta o indexo do icon do ficheiro que se vê no Windows Explorer ...

  function ShellGetIconHandle(FileName: String): HIcon;
  // Get file icon handle

  procedure ShellThreadCopy(App_Handle: THandle; fromFile: string; toFile: string);
  // Copy file using Windows Explorer dialog

  procedure ShellThreadMove(App_Handle: THandle; fromFile: string; toFile: string);
  // Move file using Windows Explorer dialog

  procedure ShellRenameDir(DirFrom, DirTo: string);
  // Rename directory using Windows Explorer API

  function ShellExecute(Operation, FileName, Parameters, Directory: String; ShowCmd: Integer): Cardinal; overload;
  // Like ShellExecute but without PCHar()

  procedure ShellExecute(ExeFilename, Parameters, ApplicationName, ApplicationClass: String; Restore: Boolean); overload;
  // Run exe with restore option

  procedure ShellExecuteAsModal(ExeFilename, ApplicationName, Directory: String);
  // ShellExecute and wait until exe closed

  procedure ShellExecuteExAsAdmin(hWnd: HWND; Filename: string; Parameters: string);
  // ShellExecute with Admin rights (Windows dialog with confirmation appears) ...

  function ShellExecuteEx(aFileName: string; const Parameters: string = ''; const Directory: string = ''; const WaitCloseCompletion: boolean = false): Boolean;

  procedure RestoreAndSetForegroundWindow(Hnd: Integer);
  // Restaura o programa especificado.

  function RemoveDuplicatedPathDelimiter(Str: String): String;

  function FileTimeToDateTime(_FT: TFileTime): TDateTime;

  function GetModificationDate(Filename: String): TDateTime;

  function GetCreationDate(Filename: String): TDateTime;

  function GetLastAccessDate(Filename: String): TDateTime;

  function FileDelete(Filename: String): Boolean;
  // Delete any file (ReadOnly or not)

  function FileIsOpen(Filename: string): boolean;

  procedure FilesDelete(FromDirectory: String; Filter: ShortString);

  function DirectoryDelete(Directory: String): Boolean;

  function GetPrinters(PrintersList: TStrings): Integer;

  procedure SetDefaultPrinter(PrinterName: String);

  procedure ShowDefaultPrinterWindowProperties(FormParent_Handle: Integer);

  function WinToDosPath(WinPathName: String): String;

  function DosToWinPath(DosPathName: String) : String;

  function GetWindowsVersion: TWindowsVersion;

  function NTSetPrivilege(sPrivilege: string; bEnabled: Boolean): Boolean;

  procedure WindowsShutDown(Restart: boolean);

  procedure CreateShortCut(FileSrc, Parametres, FileLnk, Description, DossierDeTravail, FileIcon: string; NumIcone: integer);

  procedure GetWindowsFonts(FontsList: TStrings);


implementation

function ShellGetExtensionName(FileName: String): String;
var
  {$IFDEF DELPHI2009_OR_ABOVE}
  FileInfo: _SHFileInfoW;
  {$ELSE}
  FileInfo: _SHFileInfoA;
  {$ENDIF}
  ImageListHandle: THandle;
begin
  ImageListHandle := SHGetFileInfo(PChar(FileName),   // Path: PChar
                                   0,                 // dwFileAttibutes: Cardinal
                                   FileInfo,          // Var Psfi: _SHFileInfoA
                                   SizeOf(FileInfo),  // cbFileInfo: Cardinal
                                   SHGFI_TYPENAME);   // uFlags: Cardinal

  RESULT := FileInfo.szTypeName;
end;

function ShellGetIconIndex(FileName: String): Integer;
var
  {$IFDEF DELPHI2009_OR_ABOVE}
  FileInfo: _SHFileInfoW;
  {$ELSE}
  FileInfo: _SHFileInfoA;
  {$ENDIF}
  ImageListHandle: THandle;
begin
  ImageListHandle := SHGetFileInfo(PChar(FileName),         // Path: PChar
                                   0,                       // dwFileAttibutes: Cardinal
                                   FileInfo,                // Var Psfi: _SHFileInfoA
                                   SizeOf(FileInfo),        // cbFileInfo: Cardinal
                                   SHGFI_SYSICONINDEX	);  // uFlags: Cardinal

  RESULT := FileInfo.iIcon;
end;

function ShellGetIconHandle(FileName: String): HIcon;
var
  {$IFDEF DELPHI2009_OR_ABOVE}
  FileInfo: _SHFileInfoW;
  {$ELSE}
  FileInfo: _SHFileInfoA;
  {$ENDIF}
  ImageListHandle: THandle;
begin
  ImageListHandle := SHGetFileInfo(PChar(FileName),   // Path: PChar
                                   0,                         // dwFileAttibutes: Cardinal
                                   FileInfo,                  // Var Psfi: _SHFileInfoA
                                   SizeOf(FileInfo),          // cbFileInfo: Cardinal
                                   SHGFI_ICON);    // uFlags: Cardinal

  RESULT := FileInfo.hIcon;
end;

procedure ShellThreadCopy(App_Handle: THandle; fromFile: string; toFile: string);
var
  {$IFDEF DELPHI2009_OR_ABOVE}
  shellinfo: TSHFileOpStructW;
  {$ELSE}
  shellinfo: TSHFileOpStructA;
  {$ENDIF}
begin
  with shellinfo do
  begin
    wnd   := App_Handle;
    wFunc := FO_COPY;
    pFrom := PChar(fromFile);
    pTo   := PChar(toFile);
  end;

  SHFileOperation(shellinfo);
end;

procedure ShellThreadMove(App_Handle: THandle; fromFile: string; toFile: string);
var 
  {$IFDEF DELPHI2009_OR_ABOVE}
  shellinfo: TSHFileOpStructW;
  {$ELSE}
  shellinfo: TSHFileOpStructA;
  {$ENDIF}
begin
  with shellinfo do
  begin
    wnd   := App_Handle;
    wFunc := FO_MOVE;
    pFrom := PChar(fromFile);
    pTo   := PChar(toFile);
  end;

  SHFileOperation(shellinfo);
end;

procedure ShellRenameDir(DirFrom, DirTo: string);
var
  shellinfo: TSHFileOpStruct;
begin
  with shellinfo do
  begin
    Wnd    := 0;
    wFunc  := FO_RENAME;
    pFrom  := PChar(DirFrom);
    pTo    := PChar(DirTo);
    fFlags := FOF_FILESONLY or FOF_ALLOWUNDO or
              FOF_SILENT or FOF_NOCONFIRMATION;
  end;

  SHFileOperation(shellinfo);
end;

function ShellExecute(Operation, FileName, Parameters, Directory: String; ShowCmd: Integer): Cardinal;
var POperation, PFilename, PParameters, PDirectory: PChar;
begin
  POperation := PChar(Operation);
  PFilename := PChar(FileName);
  PParameters := PChar(Parameters);
  PDirectory := PChar(Directory);
  RESULT := ShellAPI.ShellExecute(0, POperation, PFilename, PParameters, PDirectory, ShowCmd);
end;

procedure ShellExecute(ExeFilename, Parameters, ApplicationName, ApplicationClass: String; Restore: Boolean);
var
  HInstancia: Integer;
  PApplicationName, PApplicationClass: PChar;
begin
  if Restore then
  begin
    if Length(ApplicationName) = 0
    then PApplicationName := nil
    else PApplicationName := PChar(ApplicationName);

    PApplicationClass := PChar(ApplicationClass);
    HInstancia := FindWindow(PApplicationClass, PApplicationName);
  end
  else
    HInstancia := 0;

  if HInstancia <> 0
  then RestoreAndSetForegroundWindow(HInstancia)
  else ShellExecute('Open', ExeFilename, Parameters, '', SW_RESTORE);
end;

procedure ShellExecuteAsModal(ExeFilename, ApplicationName, Directory: String);
var
 StartupInfo: Tstartupinfo;
 ProcessInfo: TprocessInformation;
 PExeFilename, PApplicationName, PDirectory: PChar;
begin
  PExeFilename := PChar(ExeFilename);
  PApplicationName := PChar(ApplicationName);
  PDirectory := PChar(Directory);

  FillChar(startupinfo,sizeof(Tstartupinfo),0);
  StartupInfo.cb:=sizeof(Tstartupinfo);

  if CreateProcess(PApplicationName, PExeFilename, nil, nil, false, normal_priority_class, nil, PDirectory, Startupinfo, Processinfo) then
    WaitForSingleObject(ProcessInfo.hprocess, infinite);

  Closehandle(ProcessInfo.hprocess);
end;

procedure ShellExecuteExAsAdmin(hWnd: HWND; Filename: string; Parameters: string);
var
  sei: TShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := sizeof(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(Filename);
  sei.lpParameters := PChar(Parameters);
  sei.nShow := SW_SHOWNORMAL;
  if not ShellAPI.ShellExecuteEx(@sei) then
    RaiseLastOSError;
end;

function ShellExecuteEx(aFileName: string; const Parameters: string = ''; const Directory: string = ''; const WaitCloseCompletion: boolean = false): Boolean;
 var
   SEInfo: TShellExecuteInfo;
   ExitCode: DWORD;
   Wait: Boolean;
begin
   FillChar(SEInfo, SizeOf(SEInfo), 0) ;
   SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
   with SEInfo do
   begin
     fMask := SEE_MASK_NOCLOSEPROCESS;
     Wnd := Application.Handle;
     lpFile := PChar(aFileName);
     lpParameters := PChar(Parameters);
     lpDirectory := PChar(Directory);
     nShow := SW_SHOWNORMAL;
   end;

   Result := ShellAPI.ShellExecuteEx(@SEInfo);

   if Result and WaitCloseCompletion then
     repeat
       Application.ProcessMessages;
       GetExitCodeProcess(SEInfo.hProcess, ExitCode);
       Wait := WaitForSingleObject(SEInfo.hProcess, 10000) <> ExitCode;   // Wait max. 10 seconds
     until not Wait;
end;

procedure RestoreAndSetForegroundWindow(Hnd: Integer);
begin
  if IsIconic(Hnd) then
    ShowWindow(Hnd, Sw_Restore);

  SetForegroundWindow(Hnd);
end;

function RemoveDuplicatedPathDelimiter(Str: String): String;
var i   : Integer;
begin
  // Prefix :
  if Copy(Str, 1, 2) = '\\'         // Network path ...
  then Result := '\'
  else Result := '';

  i := Pos('\\', Str);

  while i > 0 do
  begin
    Delete(Str, i, 1);
    i := Pos('\\', Str);
  end;

  Result := Result + Str;
end;

function FileTimeToDateTime(_FT: TFileTime): TDateTime;
var _ST: SYSTEMTIME;
begin
  FileTimeToSystemTime(_FT, _ST);
  Result := SystemTimeToDateTime(_ST);
end;

function GetModificationDate(Filename: String): TDateTime;
var
  SRec: TSearchRec;
begin
  if FindFirst(Filename, FaAnyFile, SRec) = 0
  then Result := FileTimeToDateTime(SRec.FindData.ftLastWriteTime)
  else Result := 0;

  FindClose(SRec);
end;

function GetCreationDate(Filename: String): TDateTime;
var SRec: TSearchRec;
begin
  if FindFirst(Filename, FaAnyFile, SRec) = 0
  then Result := FileTimeToDateTime(SRec.FindData.ftCreationTime)
  else Result := 0;

  FindClose(SRec);
end;

function GetLastAccessDate(Filename: String): TDateTime;
var SRec: TSearchRec;
begin
  if FindFirst(Filename, FaAnyFile, SRec) = 0
  then Result := FileTimeToDateTime(SRec.FindData.ftLastAccessTime)
  else Result := 0;

  FindClose(SRec);
end;

function FileDelete(Filename: String): Boolean;
begin
  Result := false;

  if FileExists(Filename) then
  begin
    SetFileAttributes(PChar(Filename), 0); // Remove ReadOnly ...
    Result := DeleteFile(Filename);
  end;
end;

function FileIsOpen(Filename: string): boolean;
var
  HFileRes: HFILE;
begin
  Result := false;
  if not FileExists(Filename) then
    Exit;

  HFileRes := CreateFile(PChar(Filename)
    ,GENERIC_READ or GENERIC_WRITE
    ,0
    ,nil
    ,OPEN_EXISTING
    ,FILE_ATTRIBUTE_NORMAL
    ,0);

  Result := (HFileRes = INVALID_HANDLE_VALUE);

  if not(Result) then
    CloseHandle(HFileRes);
end;

procedure FilesDelete(FromDirectory: String; Filter: ShortString);
var SRec: TSearchRec;
begin
  if FromDirectory <> '' then
    if FromDirectory[length(FromDirectory)] <> '\' then
      FromDirectory := FromDirectory + '\';

  if FindFirst(FromDirectory + Filter, faAnyFile, SRec) = 0
  then
    repeat
       if not IsFolder(SRec) then
         FileDelete(FromDirectory + SRec.Name);
    until FindNext(SRec) <> 0;

  FindClose(SRec);
end;

function DirectoryDelete(Directory: String): Boolean;
var
  SR: TSearchRec;
begin
  if Directory <> '' then
    if Directory[length(Directory)] <> '\' then
      Directory := Directory + '\';

  // Remover pastas e ficheiros da pasta :
  if FindFirst(Directory + '*', faAnyFile, SR) = 0
  then
    repeat
       if IsFolder(SR)
       then begin
         if (SR.Name + '.')[1] <> '.' then          // Hidden System folder !
           DirectoryDelete(Directory + SR.Name);
       end
       else
         FileDelete(Directory + SR.Name);
    until FindNext(SR) <> 0;

  FindClose(SR);
  Result := RemoveDir(Directory);
end;

function GetPrinters(PrintersList: TStrings): Integer;
var
  I: Integer;
  ByteCnt : DWORD; //Nb d'octets à réserver pour récupérer le tableau de structures
  StructCnt: DWORD; //Nb de structures récupérees = nb imprimantes
  p : pointer; //pointe sur le tableau de structures retourné par EnumPrinters
  PrinterInfo: PPrinterInfo2; //Pointeur de structures de type _Printer_Info_2
  szCurrentPrinter: PChar;
begin
  Result := -1;
  PrintersList.Clear;
  //initialisation des paramètres
  ByteCnt := 0;
  StructCnt := 0;
  PrinterInfo:=nil;
  p:=nil;

  GetMem(szCurrentPrinter,SizeOf(Char) * 256);                            // Reserver espace memoire ...
  GetProfileString('Windows', 'DEVICE', '', szCurrentPrinter, 254);       // Nome+Info de l' imprimante actuelle ...

  //Cette fonction retourne un pointeur sur les structures des imprimantes
  //Le While permet d'effectuer une réservation suffisante pour le tableau de structures
  while not EnumPrinters(PRINTER_ENUM_LOCAL, nil, 2, p, ByteCnt, ByteCnt,StructCnt) Do
    if (GetLastError = ERROR_INSUFFICIENT_BUFFER)
    then p := AllocMem(ByteCnt);

  //Maintenant il ne reste plus qu'à lire le contenu des structures du tableau
  for I := 0 to StructCnt-1 do //On lit chaques structures propre à chaque imprimante
  begin
    //On se déplace dans le tableau de structures
    PrinterInfo := PPrinterInfo2(LongInt(P)+I*sizeof(_Printer_Info_2));
    PrintersList.add(PrinterInfo.pPrinterName);

    if PrinterInfo.pPrinterName = String_copy(szCurrentPrinter, srFromLeft, ',', False) then
      Result := i;
//    Memo1.lines.add(#9+'Port          ->   '+PrinterInfo.pPortName);
//    Memo1.lines.add(#9+'Pilote        ->   '+PrinterInfo.pDriverName);
//    Memo1.lines.add(#9+'Commentaire   ->   '+PrinterInfo.pComment);
  end;

  FreeMem(szCurrentPrinter, SizeOf(Char) * 256) ;                         // Libérer espace mémoire ...
end;

procedure SetDefaultPrinter(PrinterName: String);
var szPrinterName, szIniInfo, szSection: PChar;
    Arr_Tmp: Array[0..64] of Char;
begin
  try
    GetMem(szPrinterName,SizeOf(Char) * 256);                            // Reserver espace memoire ...
    GetMem(szIniInfo,SizeOf(Char) * 256);
    GetMem(szSection,10) ;

    StrPCopy(szPrinterName, PrinterName);
    GetProfileString('DEVICES', szPrinterName, nil, szIniInfo, 254) ;    // Recuperer info sur l' imprimante ...

    if szIniInfo^ <> #0 then                                                 // Si info trouvée ...
    begin
      StrCat(szPrinterName,',') ;
      StrCat(szPrinterName,szIniInfo) ;
      WriteProfileString('Windows','DEVICE',szPrinterName) ;             // Changer l' imprimante ...

      // Informer les applications du changement :
      StrCopy(Arr_Tmp, 'windows');
      SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, LongInt(@Arr_Tmp));

      // StrCopy(szSection,'Windows') ;
      // Ne fonctionne pas avec Win98/ Me : PostMessage(HWND_BROADCAST,WM_WININICHANGE,0,LongInt(szSection)) ;
    end;

    FreeMem(szPrinterName,SizeOf(Char) * 256) ;                          // Libérer espace mémoire ...
    FreeMem(szIniInfo,SizeOf(Char) * 256) ;
    FreeMem(szSection,10) ;
  except
//    on E: EOutOfMemory do ShowMessage(E.Message) ;                       // Pas d' espace mémoire ...
//    on E: EInvalidPointer do ShowMessage(E.Message) ;                    // Erreur de pointeur ...
  end;
end;

procedure ShowDefaultPrinterWindowProperties(FormParent_Handle: Integer);
var szCurrentPrinter: PChar;
    Impressora: String;
    {$IFDEF DCC} // Delphi XE2/XE3 Win 32/64
    HPrt: NativeUInt;
    {$ELSE}
    HPrt: Cardinal;
    {$ENDIF}
begin
  GetMem(szCurrentPrinter,SizeOf(Char) * 256);                            // Reserver espace memoire ...
  GetProfileString('Windows', 'DEVICE', '', szCurrentPrinter, 254);       // Nome+Info de l' imprimante actuelle ...

  Impressora := String_copy(szCurrentPrinter, srFromLeft, ',', False);
  FreeMem(szCurrentPrinter, SizeOf(Char) * 256) ;                         // Libérer espace mémoire ...

  OpenPrinter(PChar(Impressora), HPrt, Nil);   // Handle da impressora
  PrinterProperties(FormParent_Handle, HPrt); // Mostrar janela de config
end;

function WinToDosPath(WinPathName: String): String;
var aTmp: array[0..MAX_PATH] of char;    // MAX_PATH = 260 ...
begin
  if GetShortPathName(PChar(WinPathName),aTmp,Sizeof(aTmp)-1)=0
  then Result := ''
  else Result := StrPas(aTmp);
end;

function DosToWinPath(DosPathName: String) : String;
var aInfo: TSHFileInfo;
    FileDrive: ShortString;
begin
  RESULT := '';

  if DosPathName <> '' then
  begin
    FileDrive := ExtractFileDrive(DosPathName) + '\';

    // Retirer la barre à la fin :
    if DosPathName[length(DosPathName)] = '\'
    then DosPathName := Copy(DosPathName, 1, length(DosPathName) - 1);

    while (Length(DosPathName) > Length(FileDrive))
      and (SHGetFileInfo(PChar(DosPathName),0,aInfo,Sizeof(aInfo),SHGFI_DISPLAYNAME)<>0) do
    begin
      if RESULT = ''
      then Result := String(aInfo.szDisplayName)
      else Result := String(aInfo.szDisplayName) + '\' + RESULT;

      DosPathName := ExtractFileDir(DosPathName);    // Rechercher le nom long du repertoire contenant le fichier/repertoire actuel ...
    end;

    RESULT := FileDrive + RESULT;
  end;
end;

function GetWindowsVersion: TWindowsVersion;
var
  VersionInfo: TOSVersionInfo;
begin
  RESULT := wvUnknown;

  // charger info
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  // en fonction de la version
  case VersionInfo.dwPlatformId of
      // win 3.1
      VER_PLATFORM_WIN32S:
        RESULT := wvWin31;

      // win 95 / 98 / me
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          case VersionInfo.dwMinorVersion of
             // win 95
              0: RESULT := wvWin95;
             // win 98
             10: RESULT := wvWin98;
             // win millenium
             90: RESULT := wvWinMe;
          end;
        end;

      // win nt, 2000, XP ...
      VER_PLATFORM_WIN32_NT :
          case VersionInfo.dwMajorVersion of
             // win nt 3
             3: RESULT := wvWinNt3;
             // win nt4
             4: RESULT := wvWinNt4;
             // win 2000 et xp
             5: begin
                  case VersionInfo.dwMinorVersion of
                       // win 2000
                       0: RESULT := wvWin2000;
                       // win xp
                       1: RESULT := wvWinXP;
                  end;
                end;
             // win Vista
             6: RESULT := wvWinVista;
             7: RESULT := wvWin7;
             8: RESULT := wvWin8;
             else
                RESULT := wvWin8_Or_Upper;
          end;
  end;
end;

function NTSetPrivilege(sPrivilege: string; bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin
  Result := True;
  // Only for Windows NT/2000/XP and later.
  if not (Win32Platform = VER_PLATFORM_WIN32_NT) then Exit;
  Result := False;

  // obtain the processes token
  if OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken)
  then begin
    try
      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValue(nil, PChar(sPrivilege), TokenPriv.Privileges[0].Luid)
      then begin
        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege
        AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
          PrevTokenPriv, ReturnLength);
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
//  if not Result then
//    raise Exception.Create(SysErrorMessage(GetLastError));
end;

procedure WindowsShutDown(Restart: boolean);
var
  osVer: OSVERSIONINFO;
begin
  try
    NTSetPrivilege(SE_SHUTDOWN_NAME, true);
  finally
     Application.ProcessMessages;

     if Restart
     then ExitWindowsEx(EWX_REBOOT or EWX_FORCEIFHUNG, 0)
     else begin
       osVer.dwOSVersionInfoSize := Sizeof(osVer);
       GetVersionEx(osVer);
       if osVer.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS
       then ExitWindowsEx(EWX_SHUTDOWN or EWX_FORCEIFHUNG , 0)
       else ExitWindowsEx(EWX_POWEROFF or EWX_FORCEIFHUNG , 0);
     end;
  end;
end;

procedure CreateShortCut(FileSrc, Parametres, FileLnk, Description, DossierDeTravail, FileIcon: string; NumIcone: integer);
var ShellLink: IShellLink;
begin
  if AnsiUpperCase(extractFileExt(FileLnk)) <> '.LNK'
  then FileLnk := FileLnk + '.lnk';

  ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
  ShellLink.SetDescription(PChar(Description));
  ShellLink.SetPath(PChar(FileSrc));
  ShellLink.SetArguments(PChar(Parametres));
  ShellLink.SetWorkingDirectory(PChar(DossierDeTravail));
  ShellLink.SetShowCmd(SW_SHOW);

  if FileIcon <> '' then    ShellLink.SetIconLocation(PChar(FileIcon), NumIcone);  (ShellLink as IpersistFile).Save(StringToOleStr(FileLnk), true);
end;

procedure GetWindowsFonts(FontsList: TStrings);
var
  DC: HDC;

    function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
        FontType: Integer; Data: Pointer): Integer; stdcall;
    begin
      TStrings(Data).Add(LogFont.lfFaceName);
      Result := 1;
    end;

begin
  FontsList.Clear;
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontsList));
  ReleaseDC(0, DC);
end;

end.
