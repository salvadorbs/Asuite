(*
  @abstract(Regroupe quelques routines pour la detection de l'OS, CPU et autres
  informations sur le systeme en général. Plus quelques fonctions utiles pour
  une application. (retrouver le dossier de l'application, dossier temporaire de l'OS etc...))

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  @bold(Historique) : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSceneStrConsts @br

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZSystem;

//==============================================================================
{$mode objfpc}{$H+}
{$i bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, fileutil, LazUtf8, lazfileutils, DateUtils,
  LCLIntf, LCLType
  {$IFDEF Windows}
   ,Windows, GetText
  {$ENDIF}
  {$IFDEF UNIX}
   ,BaseUnix, Unix, Unixutil, linux, users
    {$IFDEF X11_SUPPORT}
     ,xlib
    {$ENDIF}
    {$IFDEF LCLCarbon}
     ,MacOSAll
    {$ENDIF}
  {$ENDIF};

//==============================================================================

ResourceString
  rsIndexOutOfRange = 'Index hors limite';
  rsListOrArrayIsEmpty = 'La liste ou le tableau est vide';
  rsOutOfMemory ='Pas assez de mémoire disponible';
  rsTooLarge = 'Trop grand';

  rsAnimPowerInfosMsg = 'TBZAnimationController' + LineEnding +
                        'l''animation de type amPower à besoins d''paramètre supplémentaires dans la variables ExtraParams.' + LineEnding +
                        'Ce paramètre doit désigner la valeur de l''exposant. En son absence celui-ci est par défaut 2';

  cUnknownArchiveVersion = 'Version d''archive inconnue : ';

const
 { Langue utilisée par défaut }
 cDefaultLanguage : String = 'FR';

Type
  { Décrit les informations sur le systeme d'exploitation }
  TBZPlatformInfo = record
    Major: DWORD;
    Minor: DWORD;
    Revision: DWORD;
    Version: string;
    PlatformId   :DWORD;
    ID: string;
    CodeName: string;
    Description: string;
    ProductBuildVersion: string;
  end;

Type
  { Enumération des types et version de systemes d'exploitation }
  TBZPlatformVersion = (
      pvUnknown,
      pvWin95,
      pvWin98,
      pvWinME,
      pvWinNT3,
      pvWinNT4,
      pvWin2000,
      pvWinXP,
      pvWin2003,
      pvWinVista,
      pvWinSeven,
      pvWin2008,
      pvWin8,
      pvWin10,
      pvUNIXArc,
      pvUNIXDebian,
      pvUNIXopenSUSE,
      pvUNIXFedora,
      pvUNIXGentoo,
      pvUNIXMandriva,
      pvUNIXRedHat,
      pvUNIXTurboUNIX,
      pvUNIXUbuntu,
      pvUNIXXandros,
      pvUNIXOracle,
      pvAppleMacOSX
    );
  //TBZPlatformVersionSet = set of TBZPlatformVersion;

Type
  { Décrit les capacités de l'affichage }
  TBZDeviceCapabilities = record
    Xdpi, Ydpi: integer; //< Nombre de pixel logique par pouce.
    Depth: integer;      //< profondeur de couleur (bit).
    NumColors: integer;  //< Nombre d'entrées dans la table des couleurs de l'appareil.
  end;

//==============================================================================

Type
  { Definition des technologies spécifiques supportées par le CPU }
  TBZCPUFeaturesSet = (cf3DNow,  //< Extension EDX bit 31
					   cf3DNowExt, //< Extension EDX bit 30
					   cfMMX,      //< EDX bit 23 + Extension EDX bit 23
					   cfEMMX,     //< Extension EDX bit 22
					   cfSSE,      //< EDX bit 25
					   cfSSE2,     //< EDX bit 26
					   cfSSE3,     //< ECX bit 0
					   cfSSSE3,    //< ECX bit 9
					   cfSSE41,    //< ECX bit 19
					   cfSSE42,    //< ECX bit 20
					   cfSSE4A,    //< Extension ECX bit 20
					   cfAVX,      //< ECX bit 28
					   //cfAVX2,
					   cfFMA3,     //< ECX bit 12
					   cfFMA4,     //< Extension ECX bit 16
					   cfNX,       //< Extension EDX bit 20
					   cfAES       //< ECX bit 25
                     );
   { Extensions supportées par le CPU }
   TBZCPUFeatures = set of TBZCPUFeaturesSet;

   { Informations générales sur le CPU  }
   TBZCPUInfos = record
     Vendor  : String;
     BrandName : String;
     //FamillyAsString : String;
     Features  : TBZCPUFeatures;
     FeaturesAsString : String;
     Signature : Integer;
     Speed : Integer;
     LogicalProcessors : byte;
     ProcessorType:Integer;
     Familly:integer;
     Model:Integer;
     ExtModel:Integer;
     ExtFamilly:Integer;
     Stepping:Integer;
   end;

  { Informations sur l'utilisation de la mémoire }
  TBZMemoryStatus = record
    TotalPhys: Int64;
    AvailPhys: Int64;
    TotalPageFile: Int64;
    AvailPageFile: Int64;
    TotalVirtual: Int64;
    AvailVirtual: Int64;
    //AvailExtendedVirtual: Int64;
  end;

function FormatByteSize(ABytes: Int64): String;

{ Retourne la vitesse approximative du processeur }
function CPU_Speed: double;
{ Renvoie @TRUE si l'instruction RDRand est supporté. INTEL CPU seulement }
function CPU_Support_RDRAND:Boolean;
{ Renvoie @TRUE si l'instruction RDSeed est supporté. INTEL CPU seulement }
function CPU_Support_RDSEED:Boolean;
{ Retourne le nombre de processeur configurer par le systeme }
function GetProcessorCount: Cardinal;
{ Retourne si des des instructions spécifiques sont supportées par le processeur }
function CPU_HasFeature(const InstructionSet: TBZCPUFeaturesSet): Boolean;

{ Recupere la valeur du timer interne du CPU (RTDSC) }
function GetClockCycleTickCount: Int64;
{ Recupere la valeur du timer interne }
procedure QueryPerformanceCounter(var val: Int64);
{ Recupere la fréquence du timer interne }
function QueryPerformanceFrequency(var val: Int64): Boolean;

{ Retourne les infos sur l'OS }
function GetPlatformInfo: TBZPlatformInfo;
{ Retrourne la version de l'OS }
function GetplatformVersion : TBZPlatformVersion;
{ Retrourne la version de l'OS  sous forme de chaine de caratères }
function GetplatformAsString : string;
function GetplatformVersionAsString : string;

{ Retourne les carateristique d'affichage de l'appareil  cf : TBZDeviceCapabilities }
function GetDeviceCapabilities: TBZDeviceCapabilities;
{ Retourne le nombre de bit pour l'affichage }
function GetCurrentColorDepth: Integer;
{ Retourne la largeur de l'affichage en DPI }
function GetDeviceLogicalPixelsX: Integer;
{ Retourne les carateristique d'affichage de l'appareil sous forme de chaine de caratères }
function GetDeviceCapabilitiesAsString: String;

{ Retourne des informations sur la mémoire. cf : TBZMemoryStatus. Seulement valable pour WINDOWS actuellement }
function GetMemoryStatus : TBZMemoryStatus;
{ Retourne le nom de fichier de l'application en cours }
function GetApplicationFileName : string;
{ Retourne le dossier de l'application en cours }
function GetApplicationPath : string;
{ Retourne le répertoire temporaire de l'OS }
function GetTempFolderPath : string;

{ Ouvre un fichier HTML ou une Url dans le navigateur par defaut }
procedure ShowHTMLUrl(Url: string);

{ Retourne la langue utilisée par le système}
function GetSystemLanguage: string;
{ Retourne le format de la decimal du systeme }
function GetDecimalSeparator: Char;
{ Definit le format décimal "." ou "," habituellement}
procedure SetDecimalSeparator(AValue: Char);

{ Retourne la valeur de la variable d'environement passé en paramètre sous forme de chaine de caractères}
function GetEnvironmentVariable(const Name: string): string;

{ Change les slashs et backslash et inversement suivant l'OS }
function FixPathDelimiter(S: string):String;

{ Efface les slashs et backslash d'un nom de fichier }
function RemoveFileNamePathDelimiter(S : String):String;

{ Vérifie si un dossier à les droits en écriture ou non }
function IsDirectoryWriteable(const AName: string): Boolean;

{ Retourne la taille du disque courrant }
function GetCurrentDiskSize : String;
{ Retourne l'espace libre du disque courrant }
function GetCurrentFreeDiskSpace : String;

{ Retourne le nom de l'utilisateur en cours }
function GetCurrentUserName: string;

//Function GetFileVersion: String;
//Function GetProductVersion: String;

{ Retourne la date de compilation de l'application }
Function GetCompiledDate: String;
{ Retourne l'heure de compilation de l'application }
Function GetCompiledTime: String;
{ Retourne la version de FPC qui a servi à compilé l'application }
Function GetCompilerInfo: String;
{ Retourne la version de la LCL utilisée }
Function GetLCLVersion: String;
{ Retourne le widget utilisé pour l'interface graphique de l'application }
function GetWidgetSet: string;

{ Retourne l'état des boutons de la souris n'importe ou (même en dehors d'une fenêtre) comme GetCursorPos}
function GetMouseButtonState: TShiftState;

//==============================================================================
Var
  { Variable globale de type TBZCPUInfos pour stocker les informations du processeur . Variable Initialisée automatiquement }
  BZCPUInfos : TBZCPUInfos;

//==============================================================================

implementation

uses
   resreader, resource, versiontypes, versionresource, LCLVersion, InterfaceBase, LCLPlatformDef,
   forms
   //BZUtils
   {$IFDEF WINDOWS}
    ,ShellApi,
    JwaWinBase,{, JwaWinNt}
    winpeimagereader {need this for reading exe info}
   {$ENDIF}
   {$IFDEF UNIX}
     ,LCLProc,
     elfreader {needed for reading ELF executables}
   {$ENDIF}
   {$IFDEF DARWIN}
     ,XMLRead,
     DOM,
     machoreader {needed for reading MACH-O executables}
   {$ENDIF};

Const
  WIDGETSET_GTK        = 'GTK widget';
  WIDGETSET_GTK2       = 'GTK 2 widget';
  WIDGETSET_WIN        = 'Win32/Win64 widget';
  WIDGETSET_WINCE      = 'WinCE widget';
  WIDGETSET_CARBON     = 'Carbon widget';
  WIDGETSET_QT         = 'QT widget';
  WIDGETSET_fpGUI      = 'fpGUI widget';
  WIDGETSET_OTHER      = 'Other gui';

//==============================================================================

{$IFDEF UNIX}
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}

{$IFDEF USE_ASM_OPTIMIZATIONS}
{$IFDEF CPU32}
function GetClockCycleTickCount: Int64; assembler; register;
asm
  DB $0F,$31
end;
{$ENDIF}
{$IFDEF CPU64}
function GetClockCycleTickCount: Int64; assembler; register;
asm
   RDTSC //dw 310Fh // rdtsc
   shl rdx, 32
   or rax, rdx
end;
{$ENDIF}
{$ELSE}
function GetClockCycleTickCount: Int64;
begin
  result:=getTickCount64;
end;
{$ENDIF}

//asm
//{$ifdef CPU64}
//  XOR rax, rax
//  CPUID
//  RDTSC  //Get the CPU's time stamp counter.
//  mov [Result], RAX
//{$else}
//  XOR eax, eax
//  CPUID
//  RDTSC  //Get the CPU's time stamp counter.
//  mov [Result], eax
//{$endif}
//end;

function CPU_Speed: double;
{$IFDEF USE_ASM_OPTIMIZATIONS}
const
  DelayTime = 500; // measure time in ms
var
  TimerHi, TimerLo: DWOrd;
  PriorityClass, Priority: Integer;
begin
  {$IFDEF WINDOWS}
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetthreadPriority(GetCurrentthread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetthreadPriority(GetCurrentthread, thread_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  {$ENDIF}

  asm
    dw 310Fh // rdtsc
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  Sleep(DelayTime);
  asm
    dw 310Fh // rdtsc
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;

  {$IFDEF WINDOWS}
  SetthreadPriority(GetCurrentthread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  Result := TimerLo / (1000.0 * DelayTime);
  {$ENDIF}
end;
{$ELSE}
const
  DelayTime = 200;
var
  x, y: UInt64;
  {$IFDEF WINDOWS}PriorityClass, Priority: Integer;{$ENDIF}
begin

  {$IFDEF WINDOWS}
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority      := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  {$ENDIF}

  x := GetClockCycleTickCount;
  Sleep(DelayTime);
  y := GetClockCycleTickCount;

  {$IFDEF WINDOWS}
  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  {$ENDIF}

  Result := ((y - x) and $FFFFFFFF) / (1000 * DelayTime);
end;
{$ENDIF}

function GetProcessorCount: Cardinal;
{$IFDEF WINDOWS}
var
  lpSysInfo: TSystemInfo;
begin
  //lpSysInfo := nil;
  GetSystemInfo({%H-}lpSysInfo);
  Result := lpSysInfo.dwNumberOfProcessors;
end;
{$ELSE}
begin
  //Result := 1;
  result := sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ENDIF}

var
  vBZStartTime : TDateTime;
{$IFDEF WINDOWS}
  vLastTime: TDateTime;
  vDeltaMilliSecond: TDateTime;
{$ENDIF}

function CPUID_Available: Boolean;  assembler;
asm
{$IFDEF CPU64}
  //POP RDX
  MOV       EDX,False
  PUSHFQ
  POP       RAX
  MOV       ECX,EAX
  XOR       EAX,$00200000
  PUSH      RAX
  POPFQ
  PUSHFQ
  POP       RAX
  XOR       ECX,EAX
  JZ        @1
  MOV  EDX,True
  @1: PUSH RAX
  POPFQ
  MOV       EAX,EDX
  //PUSH RDX
{$ELSE}
  MOV       EDX,False
  PUSHFD
  POP       EAX
  MOV       ECX,EAX
  XOR       EAX,$00200000
  PUSH      EAX
  POPFD
  PUSHFD
  POP       EAX
  XOR       ECX,EAX
  JZ        @1
  MOV       EDX,True
  @1: PUSH      EAX
  POPFD
  MOV       EAX,EDX
{$ENDIF}

end;

function CPU_getLargestStandardFunction:integer;  assembler;
asm
{$IFDEF cpu64}
  PUSH      RBX
  MOV       EAX,0
  CPUID
  POP RBX
{$ELSE}
  PUSH      EBX
  MOV       EAX,0
  CPUID
  POP EBX
{$ENDIF}
end;

function CPU_Signature: Integer;   assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       EAX,1
  CPUID
  POP       RBX
{$ELSE}
  PUSH      EBX
  MOV       EAX,1
  CPUID
  POP       EBX
{$ENDIF}
end;

function CPU_FeaturesEDX: Integer;  assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       EAX,1
  CPUID
  POP       RBX
  MOV       EAX,EDX
{$ELSE}
  PUSH      EBX
  MOV       EAX,1
  CPUID
  POP       EBX
  MOV       EAX,EDX
{$ENDIF}

end;

function CPU_FeaturesECX: Integer; assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       EAX,1
  CPUID
  POP       RBX
  MOV       EAX,ECX
{$ELSE}
  PUSH      EBX
  MOV       EAX,1
  CPUID
  POP       EBX
  MOV       EAX,EDX
{$ENDIF}
end;

{$IFDEF CPU64}
function CPU_DetectFeaturesEDX(B:Byte):boolean;  assembler;
asm
  PUSH      RBX
  MOV       R8B,CL
  MOV       EAX,1
  CPUID
  MOV       EAX,EDX
  MOV       CL,R8B
  SHR       EAX,CL
  AND       EAX,1
  POP       RBX
end;
{$ELSE}
function CPU_DetectFeaturesEDX(B:Byte):boolean;
begin
  Result := false;
End;

{asm
  PUSH      EBX
  MOV       BL,CL
  MOV       EAX,1
  CPUID
  MOV       EAX,EDX
  MOV       CL,BL
  SHR       EAX,CL
  AND       EAX,1
  POP       EBX
end; }
{$ENDIF}


function CPU_DetectFeaturesECX(B:Byte):boolean; assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       R8B,CL
  MOV       EAX,1
  CPUID
  MOV       EAX,ECX
  MOV       CL,R8B
  SHR       EAX,CL
  AND       EAX,1
  POP       RBX
{$ELSE}
  PUSH      EBX
  MOV       BL ,CL
  MOV       EAX,1
  CPUID
  MOV       EAX,ECX
  MOV       CL,BL
  SHR       EAX,CL
  AND       EAX,1
  POP       EBX
{$ENDIF}
end;

function CPU_ExtensionsAvailable: Boolean;  assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       @Result, True
  MOV       EAX, $80000000
  CPUID
  CMP       EAX, $80000000
  JBE       @NOEXTENSION
  JMP       @EXIT
  @NOEXTENSION:
  MOV       @Result, False
  @EXIT:
  POP       RBX
{$ELSE}
  PUSH      EBX
  MOV       @Result, True
  MOV       EAX, $80000000
  CPUID
  CMP       EAX, $80000000
  JBE       @NOEXTENSION
  JMP       @EXIT
  @NOEXTENSION:
  MOV       @Result, False
  @EXIT:
  POP       EBX
{$ENDIF}
end;

function CPU_DetectExtensionFeaturesECX(B:Byte):boolean;  assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       R8B,CL
  MOV       EAX,$80000001
  CPUID
  MOV       EAX,ECX
  MOV       CL,R8B
  SHR       EAX,CL
  AND       EAX,1
  POP       RBX
{$ELSE}
  PUSH      EBX
  MOV       BL ,CL
  MOV       EAX,$80000001
  CPUID
  MOV       EAX,ECX
  MOV       CL,BL
  SHR       EAX,CL
  AND       EAX,1
  POP       EBX
{$ENDIF}
end;

function CPU_DetectExtensionFeaturesEDX(B:Byte):boolean;  assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       R8B,CL
  MOV       EAX,$80000001
  CPUID
  MOV       EAX,EDX
  MOV       CL,R8B
  SHR       EAX,CL
  AND       EAX,1
  POP       RBX
{$ELSE}
  PUSH      EBX
  MOV       BL,CL
  MOV       EAX,$80000001
  CPUID
  MOV       EAX,EDX
  MOV       CL,BL
  SHR       EAX,CL
  AND       EAX,1
  POP       EBX
{$ENDIF}
end;

function CPU_FeaturesBitsEBX: Integer;  assembler;
asm
{$IFDEF CPU64}
        PUSH      RBX
        MOV       EAX, $00000007
        CPUID
        MOV       EAX,EBX
        POP       RBX
{$ELSE}
        PUSH      EBX
        MOV       EAX, $80000001
        CPUID
        POP       EBX
        MOV       EAX,EDX
{$ENDIF}
end;

function CPU_LogicalProcessorCount: Integer;  assembler;
asm
{$IFDEF CPU64}
  PUSH      RBX
  MOV       EAX, 1
  CPUID
  AND       EBX, 00FF0000h
  MOV       EAX, EBX
  SHR       EAX, 16
  POP       RBX
{$ELSE}
  PUSH      EBX
  MOV       EAX, 1
  CPUID
  AND       EBX, 00FF0000h
  MOV       EAX, EBX
  SHR       EAX, 16
  POP       EBX
{$ENDIF}
end;

function CPU_Brand: String;
var s:array[0..48] of ansichar;
begin
  {$IFDEF CPU64}
  //fillchar(s{%H-},sizeof(s),0);
  asm
    PUSH      RBX
    mov eax,080000000h
    CPUID
    cmp eax,080000004h
    jb @@endbrandstr
    //get first name part
    mov eax,080000002h
    CPUID
    mov longword(s[0]),eax
    mov longword(s[4]),ebx
    mov longword(s[8]),ecx
    mov longword(s[12]),edx
    //get second name part
    mov eax,080000003h
    CPUID
    mov longword(s[16]),eax
    mov longword(s[20]),ebx
    mov longword(s[24]),ecx
    mov longword(s[28]),edx
    //get third name part
    mov eax,080000004h
    CPUID
    mov longword(s[32]),eax
    mov longword(s[36]),ebx
    mov longword(s[40]),ecx
    mov longword(s[44]),edx
  @@endbrandstr:
    POP       RBX
  end;
  {$ENDIF}
  {$IFDEF CPU32}
  asm
    //check if necessary extended CPUID calls are
    //supported, if not return null string
    push ebx
    mov eax,080000000h
    CPUID
    cmp eax,080000004h
    jb @@endbrandstr
    //get first name part
    mov eax,080000002h
    CPUID
    mov DWord(s[0]),eax
    mov DWord(s[4]),ebx
    mov DWord(s[8]),ecx
    mov DWord(s[12]),edx
    //get second name part
    mov eax,080000003h
    CPUID
    mov DWord(s[16]),eax
    mov DWord(s[20]),ebx
    mov DWord(s[24]),ecx
    mov DWord(s[28]),edx
    //get third name part
    mov eax,080000004h
    CPUID
    mov DWord(s[32]),eax
    mov DWord(s[36]),ebx
    mov DWord(s[40]),ecx
    mov DWord(s[44]),edx
  @@endbrandstr:
    pop ebx
  end;
  {$ENDIF}
  result:=string(s);
end;

function CPU_VendorID: String;
var
  s : array[0..11] of ansichar;
begin
  {$IFDEF CPU64}
  asm
    PUSH      RBX
    mov eax,080000000h
    CPUID
    mov longword(s[0]),ebx
    mov longword(s[4]),edx
    mov longword(s[8]),ecx
    POP       RBX
  end;
  {$ENDIF}
  {$IFDEF CPU32}
   asm
    push ebx
    mov eax, 080000000h
    CPUID
    mov DWord(s[0]),ebx
    mov DWord(s[4]),edx
    mov DWord(s[8]),ecx
    pop ebx
  end;
  {$ENDIF}
  result:= string(s);
end;


function CPU_FeaturesAsString : String;
begin
  result:='';
  if not CPUID_Available then Exit;
  {ciMMX  ,  ciEMMX,  ciSSE   , ciSSE2  , ci3DNow ,  ci3DNowExt}
  if CPU_HasFeature(cf3DNow)  then result:=result+'3DNow';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cf3DNowExt) then result:=result+'3DNowExt';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfMMX) then result:=result+'MMX';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfEMMX) then result:=result+'EMMX';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSE) then result:=result+'SSE';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSE2) then result:=result+'SSE2';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSE3) then result:=result+'SSE3';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSSE3) then result:=result+'SSSE3';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSE41) then result:=result+'SSE4.1';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSE42) then result:=result+'SSE4.2';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfSSE4A) then result:=result+'SSE4A';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfAVX) then result:=result+'AVX';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfFMA3) then result:=result+'FMA3';
  if (result<>'') then result:=result+' / ';
  if CPU_HasFeature(cfFMA4) then result:=result+'FMA4';
end;

function CPU_StepId:Integer;
begin
  result:= CPU_Signature And $0000000F;// and $0F;
end;

function CPU_Model:Integer;
begin
  result:= (CPU_Signature  And $000000F0) Shr 4;    //shr 4 and $0F;
end;

function CPU_ExtModel:Integer;
begin
 // if CPU_Model >= 15 then
    result:= (CPU_Signature And $000F0000) Shr 16;    //shr 16 and $0F;
 // else
 //  result:=0;
end;

function CPU_Familly:Integer;
begin
  result:= (CPU_Signature  And $00000F00) Shr 8;          //  shr 8 and $0F;
end;

function CPU_ExtFamilly:Integer;
begin
  if CPU_Familly >= 15 then
    result:= (CPU_Signature And $0FF00000) Shr 20 //shr 20 and $FF
  else
    result:=0;
end;

function CPU_Type:Integer;
begin
  result:= (CPU_Signature And $00003000) Shr 12; // shr 12 and $03;
end;

function CPU_HasFeature(const InstructionSet: TBZCPUFeaturesSet): Boolean;
// Must be implemented for each target CPU on which specific functions rely
begin
  Result := False;
  if not CPUID_Available then Exit;                   // no CPUID available
  if CPU_Signature shr 8 and $0F < 5 then Exit;       // not a Pentium class

  case InstructionSet of
    cf3DNow :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesEDX(30)) then Exit;
       end;
    cf3DNowExt :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesEDX(31)) then Exit;
       end;
    cfEMMX:
      begin
        // check for SSE, necessary for Intel CPUs because they don't implement the
        // extended info
        if (not(CPU_DetectFeaturesEDX(25)) and
          (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesEDX(22))) then Exit;
      end;
     cfMMX :
       begin
         if (not CPU_DetectFeaturesEDX(23)) then exit;//CPU_DetectExtensionFeaturesEDX(23)) then Exit;     // (not CPU_ExtensionsAvailable or (
       end;
     cfSSE :
       begin
          if (not CPU_DetectFeaturesEDX(25)) then exit;
       end;
     cfSSE2 :
       begin
         if (not CPU_DetectFeaturesEDX(26)) then exit;
       end;
     cfSSE3 :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesECX(0)) then exit;
       end;
     cfSSSE3 :
       begin
         if (not CPU_ExtensionsAvailable) or(not CPU_DetectExtensionFeaturesECX(9)) then exit;
       end;
     cfSSE41 :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesECX(19)) then exit;
       end;
     cfSSE42 :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesECX(20)) then exit;
       end;
     cfSSE4A :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesECX(6)) then exit;
       end;
     cfAVX:
       begin
         if (not CPU_ExtensionsAvailable) or not(CPU_DetectFeaturesECX(28)) then exit;
       end;
     cfFMA3 :
       begin
         if (not CPU_ExtensionsAvailable) or(not CPU_DetectExtensionFeaturesECX(12)) then exit;
       end;
     cfFMA4 :
       begin
         if (not CPU_ExtensionsAvailable) or (not CPU_DetectExtensionFeaturesECX(16)) then exit;
       end;
  else
      Exit; // return -> instruction set not supported
    end;

  Result := True;
end;

function CPU_Support_RDRAND:Boolean;
begin
 result:=false;
 if not CPUID_Available then Exit;
 result:= CPU_DetectFeaturesEDX(30);// ((CPU_FeaturesEDX and $40000000) =  $40000000);
end;

function CPU_Support_RDSEED:Boolean;
begin
 result:=false;
 if not CPUID_Available then Exit;
 result:= ((CPU_FeaturesBitsEBX and $40000) =  $40000);
end;

procedure getCPUInfos(var ret :TBZCPUInfos);
var
 // ret:TBZCPUInfos;
  CPUFeaturesData : TBZCPUFeatures;
  i : TBZCPUFeaturesSet;
begin
  CPUFeaturesData := [];

  if CPUID_Available then
  begin
    for I := Low(TBZCPUFeaturesSet) to High(TBZCPUFeaturesSet) do
    if CPU_HasFeature(I) then CPUFeaturesData := CPUFeaturesData + [I];
    with Ret do
    begin
       Vendor  := CPU_VendorID;
       BrandName := CPU_Brand;
       //FamillyAsString := '-';//getCPUFamillyAsString;
       Features  := CPUFeaturesData;
       FeaturesAsString := CPU_FeaturesAsString;
       Signature := CPU_Signature;
       ProcessorType := CPU_Type;
       Model:=CPU_Model;
       Familly:=CPU_Familly;
       ExtModel:=CPU_ExtModel;
       ExtFamilly:=CPU_ExtFamilly;
       Stepping :=CPU_StepID;
       Speed := round(CPU_Speed) ;
       LogicalProcessors :=CPU_LogicalProcessorCount;
    end;
  end
  else
  begin
    with Ret do
    begin
       Vendor  := 'n/a';
       BrandName := 'n/a';
       Features  := CPUFeaturesData;
       Signature := 0;
       ProcessorType := 0;
       Model:= 0;
       Familly:= 0;
       ExtModel:= 0;
       ExtFamilly:= 0;
       Stepping := 0;
       Speed :=round(CPU_Speed);
       LogicalProcessors :=1;
    end;
  end;
 // result:=ret;
end;

{ Returns time in milisecond from application start.}
function BZStartTime: Double;
{$IFDEF WINDOWS}
var
  SystemTime: TSystemTime;
begin
  GetLocalTime({%H-}SystemTime);
  with SystemTime do
    Result :=(wHour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             wMinute * (SecsPerMin * MSecsPerSec) +
             wSecond * MSecsPerSec +
             wMilliSeconds) - vBZStartTime;
  // Hack to fix time precession
  if Result - vLastTime = 0 then
  begin
    Result := Result + vDeltaMilliSecond;
    vDeltaMilliSecond := vDeltaMilliSecond + 0.1;
  end
  else begin
    vLastTime := Result;
    vDeltaMilliSecond := 0.1;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  Result := tz.tv_sec - vBZStartTime;
  Result := Result * 1000000;
  Result := Result + tz.tv_usec;
// Delphi for UNIX variant (for future ;)
//var
//  T: TTime_T;
//  TV: TTimeVal;
//  UT: TUnixTime;
//begin
//  gettimeofday(TV, nil);
//  T := TV.tv_sec;
//  localtime_r(@T, UT);
//  with UT do
//    Result := (tm_hour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
//             tm_min * (SecsPerMin * MSecsPerSec) +
//             tm_sec * MSecsPerSec +
//             tv_usec div 1000) - vGLSStartTime;
end;
{$ENDIF}

{$IFDEF UNIX}
var
  vProgStartSecond: int64;

procedure Init_vProgStartSecond;
//var
//  tz: timeval;
//begin
//  fpgettimeofday(@tz, nil);
//  val := tz.tv_sec - vProgStartSecond;
//  val := val * 1000000;
//  val := val + tz.tv_usec;
//end;

var
  tp: timespec; // timespec record tv_sec seconds tv_nsec nanoseconds
begin
  //clock_gettime(CLOCK_MONOTONIC, @tp); // orig
  clock_gettime(CLOCK_MONOTONIC_RAW, @tp);
  // multiplier les secondes par 1000 pour pouvoir y ajouter les millisec,
  // multiplier les secondes par 1000000 pour pouvoir y ajouter les microsec,
  // multiplier les secondes par 1000000000 pour pouvoir y ajouter les nanosec
  // du coup le "result" sera le nombre de milli-secondes depuis le démarrage de la machine
  vProgStartSecond  := (Int64(tp.tv_sec) *  1000000) + int64(tp.tv_nsec); //
end;
{$ENDIF}

procedure QueryPerformanceCounter(var val: Int64);
{$IFDEF WINDOWS}
begin
  Windows.QueryPerformanceCounter(val);
end;
{$ENDIF}
{$IFDEF UNIX}
// NE FONCTIONNE PAS ET FAIT RALENTIR LE TBZCADENCER !!! A VOIR POURQUOI
//var
//  tp: timespec; // timespec record tv_sec seconds tv_nsec nanoseconds
//begin
//  clock_gettime(CLOCK_MONOTONIC, @tp); // orig
//  //clock_gettime(CLOCK_MONOTONIC_RAW, @tp);
//  val := ((tp.tv_sec - vProgStartSecond) * 1000000) + tp.tv_nsec; // valeur en milli secondes
//end;

var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  val := tz.tv_sec - vProgStartSecond;
  val := val * 1000000;
  val := val + tz.tv_usec;
end;

{$ENDIF}

function QueryPerformanceFrequency(var val: Int64): Boolean;
{$IFDEF WINDOWS}
begin
  Result := Boolean(Windows.QueryPerformanceFrequency(val));
end;
{$ENDIF}
{$IFDEF UNIX}
// https://stackoverflow.com/questions/13927317/what-is-free-pascals-equivalent-of-delphis-tstopwatch
//Var
//   r : TBaseMesure;
//begin
//FIsHighResolution := (clock_getres(CLOCK_MONOTONIC,@r) = 0);
//    FIsHighResolution := FIsHighResolution and (r.tv_nsec <> 0);
//    if (r.tv_nsec <> 0) then
//      FFrequency := C_BILLION div r.tv_nsec;
//end;

begin
  val := 1000000;
  Result := True;
end;
{$ENDIF}

function GetPlatformInfo: TBZPlatformInfo;
var
  {$IFDEF WINDOWS}
  OSVersionInfo : windows.TOSVersionInfo;
  //LPOSVERSIONINFOA; //
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFNDEF DARWIN}
      ReleseList: TStringList;
    {$ENDIF}
    str: String;
    {$IFDEF DARWIN}
    Documento: TXMLDocument;
    Child: TDOMNode;
    i:integer;
    {$ENDIF}
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  With Result do
  begin

    OSVersionInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);

    if not windows.GetVersionEx(OSVersionInfo) then {%H-}Exit;

    Minor := OSVersionInfo.DwMinorVersion;
    Major := OSVersionInfo.DwMajorVersion;
    Revision := OSVersionInfo.dwBuildNumber;
    PlatformId := OSVersionInfo.dwPlatformId;
    Version :=  InttoStr(OSVersionInfo.DwMajorVersion)+'.'+InttoStr(OSVersionInfo.DwMinorVersion)
                +' Build : '+InttoStr(OSVersionInfo.dwBuildNumber);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  {$IFNDEF DARWIN}
  ReleseList := TStringList.Create;
  With Result do
  begin
    ID := '';
    Version := '';
    CodeName := '';
    Description := '';
    Major:=0;
    Minor:=0;
    Revision:=0;;
    PlatformId   :=0;
    ProductBuildVersion:='';
  End;
  with Result,ReleseList do
  begin
    if FileExists('/etc/lsb-release')  then
      LoadFromFile('/etc/lsb-release')
    else Exit;

    ID := Values['DISTRIB_ID'];
    Version := Values['DISTRIB_RELEASE'];
    CodeName := Values['DISTRIB_CODENAME'];
    Description := Values['DISTRIB_DESCRIPTION'];
    Destroy;
  end;
  {$ELSE}
  if FileExists('System/Library/CoreServices/ServerVersion.plist')  then
    ReadXMLFile(Documento, 'System/Library/CoreServices/ServerVersion.plist')
  else Exit;
  Child := Documento.DocumentElement.FirstChild;

  if Assigned(Child) then
  begin
    with Child.ChildNodes do
    try
      for i := 0 to (Count - 1) do
      begin
        if Item[i].FirstChild.NodeValue='ProductBuildVersion' then
          Result.ProductBuildVersion:=Item[i].NextSibling.FirstChild.NodeValue;
        if Item[i].FirstChild.NodeValue='ProductName' then
          Result.ID:=Item[i].NextSibling.FirstChild.NodeValue;
        if Item[i].FirstChild.NodeValue='ProductVersion' then
          Result.Version:=Item[i].NextSibling.FirstChild.NodeValue;
      end;
    finally
      Free;
    end;
  end;
  {$ENDIF}
  //Major.Minor.Revision
  str:=Result.Version;
  if str='' then Exit;
  Result.Major:=StrtoInt( Utf8Copy(str, 1, Utf8Pos('.',str)-1) );
  Utf8Delete(str, 1, Utf8Pos('.', str) );

  //10.04
  if Utf8Pos('.', str) = 0 then
  begin
    Result.Minor:=StrtoInt( Utf8Copy(str, 1, Utf8Length(str)) );
    Result.Revision:=0;
  end
  else
  //10.6.5
  begin
     Result.Minor:=StrtoInt( Utf8Copy(str, 1, Utf8Pos('.',str)-1) );
     Utf8Delete(str, 1, Utf8Pos('.', str) );
     Result.Revision:=StrtoInt( Utf8Copy(str, 1, Utf8Length(str)) );
  end;
  {$ENDIF}
end;

function GetplatformVersion : TBZPlatformVersion;
{$IFDEF Unix}
var
  i: integer;
const
VersStr : array[TBZPlatformVersion] of string = (
  '',  '',  '',  '',  '',  '',
  '',  '',  '',  '',  '',  '', '',
  '',
  'Arc',
  'Debian',
  'openSUSE',
  'Fedora',
  'Gentoo',
  'Mandriva',
  'RedHat',
  'TurboUNIX',
  'Ubuntu',
  'Xandros',
  'Oracle',
  'Mac OS X'
  );
{$ENDIF}
begin
  Result := pvUnknown;
  {$IFDEF WINDOWS}
  with GetPlatformInfo do
  begin
        if Version='' then Exit;
        case Major of
          0..2: Result := pvUnknown;
          3:  Result := pvWinNT3;              // Windows NT 3
          4:  case Minor of
                0: if PlatformId = VER_PLATFORM_WIN32_NT
                   then Result := pvWinNT4     // Windows NT 4
                   else Result := pvWin95;     // Windows 95
                10: Result := pvWin98;         // Windows 98
                90: Result := pvWinME;         // Windows ME
              end;
          5:  case Minor of
                0: Result := pvWin2000;         // Windows 2000
                1: Result := pvWinXP;          // Windows XP
                2: Result := pvWin2003;        // Windows 2003
              end;
          6:  case Minor of
                0: Result := pvWinVista;         // Windows Vista
                1: Result := pvWinSeven;          // Windows Seven
                2: Result := pvWin2008;        // Windows 2008
                3..4: Result := pvUnknown;
              end;
          7..8:  Result := pvWin8;
          9..10:  Result := pvWin10;
        end;
   end;
  {$ENDIF}
  {$IFDEF UNIX}
  with GetPlatformInfo do
  begin
    if Version='' then Exit;
    For i:= 13 to Length(VersStr)-1 do
     if ID=VersStr[TBZPlatformVersion(i)] then
       Result := TBZPlatformVersion(i);
  end;
  {$ENDIF}
end;

function GetplatformAsString : string;
const
  VersStr : array[TBZPlatformVersion] of string = (
    'Inconnu',
    'Windows 95',
    'Windows 98',
    'Windows ME',
    'Windows NT 3',
    'Windows NT 4',
    'Windows 2000',
    'Windows XP',
    'Windows 2003',
    'Windows Vista',
    'Windows Seven',
    'Windows 2008',
    'Windows 8',
    'Windows 10',

    'UNIX Arc',
    'UNIX Debian',
    'UNIX openSUSE',
    'UNIX Fedora',
    'UNIX Gentoo',
    'UNIX Mandriva',
    'UNIX RedHat',
    'UNIX TurboUNIX',
    'UNIX Ubuntu',
    'UNIX Xandros',
    'UNIX Oracle',
    'Apple MacOSX');
begin
  Result := VersStr[GetplatformVersion];
end;

function GetplatformVersionAsString : string;
begin
  Result := GetplatformAsString + ' ( Version : '+ GetPlatformInfo.Version +' )';
end;

function GetDeviceCapabilities: TBZDeviceCapabilities;
{$IFDEF WINDOWS}
var
  Device: HDC;
begin
  Device := GetDC(0);
  try
    result.Xdpi := GetDeviceCaps(Device, LOGPIXELSX);
    result.Ydpi := GetDeviceCaps(Device, LOGPIXELSY);
    result.Depth := GetDeviceCaps(Device, BITSPIXEL);
    result.NumColors := GetDeviceCaps(Device, NUMCOLORS);
  finally
    ReleaseDC(0, Device);
  end;
end;
{$ELSE}
{$IFDEF X11_SUPPORT}
var
  dpy: PDisplay;
begin
  dpy := XOpenDisplay(nil);
  Result.Depth := DefaultDepth(dpy, DefaultScreen(dpy));
  XCloseDisplay(dpy);
  Result.Xdpi := 96;
  Result.Ydpi := 96;
  Result.NumColors := 1;
end;
{$ELSE}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
end;
{$ENDIF}

{$ENDIF}

function GetDeviceCapabilitiesAsString: String;
Var
  s:String;
begin
  s:='';
  with GetDeviceCapabilities() do
  begin
    s:=  'Resolution : '+ Inttostr(Screen.Width)+'x'+Inttostr(Screen.Height)+#13#10;
    s:=s+'DPI        : '+ Inttostr(Xdpi) +'x'+inttostr(Ydpi)+#13#10;
    s:=s+'Format     : '+ Inttostr(Depth)+' Bits'+#13#10;
    //s:=s+'Couleurs : '+ Inttostr(NumColors);
  end;
  result:=s;
end;

function GetDeviceLogicalPixelsX: Integer;
begin
  result := GetDeviceCapabilities.Xdpi;
end;

function GetCurrentColorDepth: Integer;
begin
  result := GetDeviceCapabilities.Depth;
end;

{$IFDEF WINDOWS}
type
  MEMORYSTATUSEX = record
     dwLength : DWORD;
     dwMemoryLoad : DWORD;
     ullTotalPhys : uint64;
     ullAvailPhys : uint64;
     ullTotalPageFile : uint64;
     ullAvailPageFile : uint64;
     ullTotalVirtual : uint64;
     ullAvailVirtual : uint64;
     ullAvailExtendedVirtual : uint64;
  end;

function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): BOOL; stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';
{$ENDIF}

function GetMemoryStatus: TBZMemoryStatus;
{$IFDEF WINDOWS}
//type
//  TGlobalMemoryStatusEx = procedure(var lpBuffer: TMemoryStatusEx); stdcall;
var
  ms : TMemoryStatus;
  MS_Ex: MemoryStatusEx;
begin
  //Result.dwLength := SizeOf(Result);
  If GetplatformVersion in [pvUnknown, pvWin95, pvWin98, pvWinME, pvWinNT3, pvWinNT4, pvWin2000 ] then
  Begin
    ms.dwLength := SizeOf(ms);
    GlobalMemoryStatus({%H-}ms);
    //Result.dwMemoryLoad := ms.dwMemoryLoad;
    Result.TotalPhys := ms.dwTotalPhys;
    Result.AvailPhys := ms.dwAvailPhys;
    Result.TotalPageFile := ms.dwTotalPageFile;
    Result.AvailPageFile := ms.dwAvailPageFile;
    Result.TotalVirtual := ms.dwTotalVirtual;
    Result.AvailVirtual := ms.dwAvailVirtual;
  End
  Else
  Begin
    FillChar({%H-}MS_Ex, SizeOf(MemoryStatusEx), 0);
    MS_Ex.dwLength := SizeOf(MemoryStatusEx);
    if GlobalMemoryStatusEx(MS_Ex) then
    begin
      Result.TotalPhys := MS_Ex.ullTotalPhys;
      Result.AvailPhys := MS_Ex.ullAvailPhys;
      Result.TotalPageFile := MS_Ex.ullTotalPageFile;
      Result.AvailPageFile := MS_Ex.ullAvailPageFile;
      Result.TotalVirtual := MS_Ex.ullTotalVirtual;
      Result.AvailVirtual := MS_Ex.ullAvailVirtual;
    End;
  End;
End;
{$ELSE}
(* -----><------
procedure Split(const Delimiter: Char; Input: string; const Strings: TStrings);
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

procedure GetMemory(var st: TStringList);
var
  s1, s2: string;
  stTmp: TStringList;
begin
  st.Clear;
  st.LoadFromFile('/proc/meminfo');
  if st.Count > 0 then
  begin
    stTmp:= TStringList.Create;
    Split(' ', st[0], stTmp);
    s1 := stTmp[1];
    stTmp.Clear;
    Split(' ', st[1], stTmp);
    s2 := stTmp[1];
    stTmp.Free;
    st.Clear;
    st.Add(s1); // total
    st.Add(s2); // free
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  infototal, infolibre : double;
  stList: TStringList;
  s1 : string = ' Mémoire physique disponible : ';
  s2 : string = ' octets sur ';
  s3 : string = ' octets totaux, soit ';
  s4 : string = ' % libre. ';
begin
  stList:= TStringList.Create;
  GetMemory(stList);
  if stList.Count > 0 then
  begin
    infototal := StrToFloat(stList[0]);
    infolibre := StrToFloat(stList[1]);
    stb.SimpleText := s1 + Format('%.0n', [infolibre]) + s2 + Format('%.0n', [infototal])
      + s3 + FloatToStrF(((100 * infolibre) / infototal), ffFixed, 3, 0) + s4;
  end;
  stList.Free;
end;
*)
Begin
  {$MESSAGE Warn 'Needs to be implemented'}
end;

{$ENDIF}

function GetApplicationFileName : string;
var
  path: UTF8String;
begin
  path := ExtractFileName(ParamStrUTF8(0));
  result:=path;
end;

function GetApplicationPath : string;
//var
//  path: UTF8String;
//begin
//  path := ExtractFilePath(ParamStrUTF8(0));
//  path := IncludeTrailingPathDelimiter(path);
//  result:=path;
//end;
begin
  {$ifdef darwin}
  Result := Copy(Application.ExeName, 1, Pos(ApplicationName + '.app', Application.ExeName) - 1);
  {$else}
  Result  :=  Application.Location;
  {$endif}
  Result :=  IncludeTrailingPathDelimiter(Result);
end;

function GetTempFolderPath : string;
{$IFDEF WINDOWS}
var lng: DWORD; thePath: string;
begin
  SetLength(thePath{%H-}, MAX_PATH);
  lng := GetTempPath(MAX_PATH, PChar(thePath));
  SetLength(thePath, lng);
  result := thePath;
end;
{$ELSE}
begin
  result:=sysutils.GetTempDir;
end;
{$ENDIF}

procedure ShowHTMLUrl(Url: string);
begin
{$IFDEF WINDOWS}
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOW);
{$ENDIF}
{$IFDEF UNIX}
  fpSystem(PChar('env xdg-open ' + Url));
{$ENDIF}
end;

function GetSystemLanguage: string;
// *** methode independante de la plateform pour lire la langue de l'interface de l'utilisateur ***
// *** platform-independent method to read the language of the user interface ***
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF LCLCarbon}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
    {$IFDEF LINUX}
    fbl := Copy(GetEnvironmentVariableUTF8('LC_CTYPE'), 1, 2);
    if fbl='' then fbl := Copy(GetEnvironmentVariableUTF8('LANG'), 1, 2);
    if fbl='' then fbl:= cDefaultLanguage;
    {$ELSE}
      fbl := '';
      l := '';
      GetLanguageIDs(l, fbl);
    {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;


function GetDecimalSeparator: Char;
begin
  Result :=DefaultFormatSettings.DecimalSeparator;
end;

procedure SetDecimalSeparator(AValue: Char);
begin
  DefaultFormatSettings.DecimalSeparator := AValue;
end;

function GetEnvironmentVariable(const Name: string): string;
begin
  Result := GetEnvironmentVariableUTF8(Name);
end;

function FixPathDelimiter(S: string):String;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
  begin
    if (Result[I] = '/') or (Result[I] = '\') then
      Result[I] := PathDelim;
  End;
end;

function RemoveFileNamePathDelimiter(S : String) : String;
var
  I: Integer;
  Ext : String;
begin
  Ext := ExtractFileExt(S);
  S := ExtractFileNameWithoutExt(S);
  Result := '';
  for I := 1 to Length(S) do
  begin
    if (S[I] <> '/') or (S[I] <> '\') then Result := Result + S[I];
  End;
  Result := Result + Ext;
end;

function IsDirectoryWriteable(const AName: string): Boolean;
var
  LFileName: String;
{$IFDEF MSWINDOWS}
  LHandle: THandle;
{$ENDIF}
begin
  LFileName := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
{$IFDEF MSWINDOWS}
  LHandle := CreateFile(PChar(LFileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := LHandle <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(LHandle);
{$ELSE}
  Result := fpAccess(PChar(LFileName), W_OK) <> 0;
{$ENDIF}
end;

// fonction sortie de BZUtils pour eviter une double référence dans l'implementation
function FormatByteSize(ABytes: Int64): String;
const
  suffix : array[0..6] of String = ('b', 'Kb', 'Mb', 'Go', 'To', 'Po', 'Eo');
var
  l : Integer;
  fr : Double;
begin
  l := 0;
  fr := ABytes;
  while fr >= 1024 do
  begin
    inc(l);
    fr := fr / 1024;
  end;
  if fr >= 1000 then   // ensures eg. 1022 MB will show 0.99 GB
  begin
    inc(l);
    fr := fr / 1024;
  end;
  if l > High(suffix) then
    Result := rsTooLarge
  else
    Result := Format('%f %s', [fr, suffix[l]]);
end;

function GetCurrentFreeDiskSpace : String;
begin
  Result := FormatByteSize(DiskFree(0));
end;

function GetCurrentDiskSize : String;
begin
  Result := FormatByteSize(DiskSize(0));
end;

// http://forum.lazarus.freepascal.org/index.php/topic,23171.msg138057.html#msg138057
function GetCurrentUserName: string;
{$IFDEF WINDOWS}
const
  MaxLen = 256;
var
  Len: DWORD;
  WS: WideString;
  Res: windows.BOOL;
{$ENDIF}
begin
  Result := '';
  {$IFDEF UNIX}
  {$IF (DEFINED(LINUX)) OR (DEFINED(FREEBSD))}
   //GetUsername in unit Users, fpgetuid in unit BaseUnix
  Result := SysToUtf8(GetUserName(fpgetuid));
  {$ELSE Linux/BSD}
  Result := GetEnvironmentVariableUtf8('USER');
  {$ENDIF UNIX}
  {$ELSE}
  {$IFDEF WINDOWS}
  Len := MaxLen;
  {$IFnDEF WINCE}
  if Win32MajorVersion <= 4 then
  begin
    SetLength(Result,MaxLen);
    Res := Windows.GetuserName(@Result[1], Len);
    if Res then
    begin
      SetLength(Result,Len-1);
      Result := SysToUtf8(Result);
    end
    else SetLength(Result,0);
  end
  else
  {$ENDIF NOT WINCE}
  begin
    SetLength(WS{%H-}, MaxLen-1);
    Res := Windows.GetUserNameW(@WS[1], Len);
    if Res then
    begin
      SetLength(WS, Len - 1);
      Result := Utf16ToUtf8(WS);
    end
    else SetLength(Result,0);
  end;
  {$ENDIF WINDOWS}
  {$ENDIF UNIX}
end;

function GetWidgetSet: string;
begin
  Result := LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
//  case WidgetSet.LCLPlatform of
//(*    lpGtk,
//        lpGtk2,
//        lpGtk3,
//        lpWin32,
//        lpWinCE,
//        lpCarbon,
//        lpQT,
//        lpQt5,
//        lpfpGUI,
//        lpNoGUI,
//        lpCocoa,
//        lpCustomDrawn,
//        lpMUI *)
//    lpGtk:   Result := WIDGETSET_GTK;
//    lpGtk2:  Result := WIDGETSET_GTK2;
//    lpWin32: Result := WIDGETSET_WIN;
//    lpWinCE: Result := WIDGETSET_WINCE;
//    lpCarbon:Result := WIDGETSET_CARBON;
//    lpQT:    Result := WIDGETSET_QT;
//    lpfpGUI: Result := WIDGETSET_fpGUI;
//  else
//    Result:=WIDGETSET_OTHER;
//  end;
end;

Function GetCompilerInfo: String;
begin
  Result := 'FPC '+{$I %FPCVERSION%};
end;

Function GetLCLVersion: String;
begin
  Result := 'LCL '+lcl_version;
end;

Function GetCompiledDate: String;
Begin
  Result:= {$I %DATE%};
End;

Function GetCompiledTime: String;
Begin
  Result:= {$I %TIME%};
End;

function GetMouseButtonState: TShiftState;
begin
  Result := [];
  if (GetKeyState(VK_LBUTTON) and $8000) <> 0 then
    Include(Result, ssLeft);
  if (GetKeyState(VK_MBUTTON) and $8000) <> 0 then
    Include(Result, ssMiddle);
  if (GetKeyState(VK_RBUTTON) and $8000) <> 0 then
    Include(Result, ssRight);
end;

//==============================================================================

initialization
   vBZStartTime := BZStartTime;
  {$IFDEF UNIX}
    Init_vProgStartSecond;
  {$ENDIF}

  //BZCpuInfos:=
  getCPUInfos(BZCpuInfos);

//------------------------------------------------------------------------------
finalization

//==============================================================================

end.
