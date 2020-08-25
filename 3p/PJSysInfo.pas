{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2001-2020, Peter Johnson (@delphidabbler).
 *
 * $Rev: 2023 $
 * $Date: 2020-03-17 05:00:45 +0000 (Tue, 17 Mar 2020) $
 *
 * This unit contains various static classes, constants, type definitions and
 * global variables for use in providing information about the host computer and
 * operating system.
 *
 * NOTES
 *
 *  1: When compiled with old versions of Delphi that do not support setting
 *     registry access flags via the TRegistry object, some of this code may not
 *     work correctly when running on 64 bit Windows.
 *
 *  2: The code has been tested with the Delphi 64 bit compiler (introduced
 *     in Delphi XE2) and functions correctly.
 *
 *  3: When run on operating systems up to and including Windows 8 running the
 *     host program in compatibility mode causes some variables and TPJOSInfo
 *     methods to be "spoofed" into returning information about the emulated
 *     OS. When run on Windows 8.1 and later details of the actual host
 *     operating system are always returned and the emulated OS is ignored.
 *
 * ACKNOWLEDGEMENTS
 *
 * Thanks to the following who have contributed to this project:
 *
 *   - Guillermo Fazzolari (bug fix in v2.0.1)
 *
 *   - Laurent Pierre (Many PRODUCT_* constants and suggested GetProductInfo API
 *     code used in v3.0 and later)
 *
 *   - Rich Habedank (bug fix in r228 and testing of bug fixes reported as
 *     issues #31 (https://sourceforge.net/p/ddablib/tickets/31/) and #33
 *     (https://sourceforge.net/p/ddablib/tickets/33/)
 *
 * The project also draws on the work of:
 *
 *   - Achim Kalwa <delphi@achim-kalwa.de> who translated the versionhelpers.h
 *     header into Pascal. Some of the IsReallyWindowsXXXXOrGreater methods of
 *     TPJOSInfo and the TestWindowsVersion routine are based closely on his
 *     work.
 *
 *   - Brendan grant for his ideas presented in the Code Project article at
 *     http://bit.ly/1mDKTu3
 *
 *   - Kendall Sullivan for the code on which TPJComputerInfo.IsAdmin is based.
 *     See http://edn.embarcadero.com/article/26752.
 *
 *   - norgepaul for the code on which TPJComputerInfo.IsUACActive is based. See
 *     his answer on Stack Overflow at http://tinyurl.com/avlztmg.
 *
 * ***** END LICENSE BLOCK *****
}


unit PJSysInfo;


// Define DEBUG whenever debugging.
// *** IMPORTANT: Ensure that DEBUG is NOT defined in production code.
{.$DEFINE DEBUG}

// Define DEBUG_NEW_API if debugging on Windows Vista to Windows 8 in order to
// check that the new version API used for Windows 8.1 and later is working.
// This will cause the new API to be used for Windows Vista and later instead
// of only Windows 8.1 and later.
// *** IMPORTANT: Ensure that DEBUG_NEW_API is NOT defined in production code.
{.$DEFINE DEBUG_NEW_API}


// Conditional defines
// ===================

// Assume all required facilities available
{$DEFINE REGACCESSFLAGS}      // TRegistry access flags available
{$DEFINE WARNDIRS}            // $WARN compiler directives available
{$DEFINE EXCLUDETRAILING}     // SysUtils.ExcludeTrailingPathDelimiter available
{$UNDEF RTLNAMESPACES}        // No support for RTL namespaces in unit names
{$UNDEF HASUNIT64}            // UInt64 type not defined
{$UNDEF INLINEMETHODS}        // No support for inline methods

// Undefine facilities not available in earlier compilers
// Note: Delphi 1 to 3 is not included since the code will not compile on these
// compilers
{$IFDEF VER120} // Delphi 4
  {$UNDEF REGACCESSFLAGS}
  {$UNDEF WARNDIRS}
  {$UNDEF EXCLUDETRAILING}
{$ENDIF}
{$IFDEF VER130} // Delphi 5
  {$UNDEF REGACCESSFLAGS}
  {$UNDEF WARNDIRS}
  {$UNDEF EXCLUDETRAILING}  // ** fix by Rich Habedank
{$ENDIF}
{$IFDEF VER140} // Delphi 6
  {$UNDEF WARNDIRS}
{$ENDIF}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNAMESPACES}
  {$IFEND}
  {$IF CompilerVersion >= 17.0} // Delphi 2005 and later
    {$DEFINE INLINEMETHODS}
  {$IFEND}
  {$IF Declared(UInt64)}
    {$DEFINE HASUINT64}
  {$IFEND}
{$ENDIF}

// Switch off "unsafe" warnings for this unit
{$IFDEF WARNDIRS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
{$ENDIF}

// Switch on range checking when debugging. In production code it's the user's
// choice whether to use range checking or not
{$IFDEF DEBUG}
  {$RANGECHECKS ON}
{$ENDIF}


interface


uses
  // Delphi
  {$IFNDEF RTLNAMESPACES}
  SysUtils, Classes, Windows;
  {$ELSE}
  System.SysUtils, System.Classes, Winapi.Windows;
  {$ENDIF}


type
  // Windows types not defined in all supported Delphi VCLs

  // ANSI versions of the Win API OSVERSIONINFOEX structure and pointers
  _OSVERSIONINFOEXA = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of AnsiChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  POSVersionInfoExA = ^TOSVersionInfoExA;

  // Unicode versions of the Win API OSVERSIONINFOEX structure and pointers
  _OSVERSIONINFOEXW = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of WideChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  POSVersionInfoExW = ^TOSVersionInfoExW;

  // Default version of the Win API OSVERSIONINFOEX structure.
  // UNICODE is defined when the Unicode API is used, so we use this to decide
  // which structure to use as default.
  {$IFDEF UNICODE}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXW;
  OSVERSIONINFOEX = OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExW;
  POSVersionInfoEx = POSVersionInfoExW;
  {$ELSE}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
  OSVERSIONINFOEX = OSVERSIONINFOEXA;
  TOSVersionInfoEx = TOSVersionInfoExA;
  POSVersionInfoEx = POSVersionInfoExA;
  {$ENDIF}

const

  // Windows constants possibly not defined in all supported Delphi VCLs

  // Conditional consts used in VerSetConditionMask calls
  VER_EQUAL         = 1; // current value = specified value.
  VER_GREATER       = 2; // current value > specified value.
  VER_GREATER_EQUAL = 3; // current value >= specified value.
  VER_LESS          = 4; // current value < specified value.
  VER_LESS_EQUAL    = 5; // current value <= specified value.

  // Platform ID defines
  // these are not included in Windows unit of all supported Delphis
  VER_BUILDNUMBER       = $00000004;
  VER_MAJORVERSION      = $00000002;
  VER_MINORVERSION      = $00000001;
  VER_PLATFORMID        = $00000008;
  VER_SERVICEPACKMAJOR  = $00000020;
  VER_SERVICEPACKMINOR  = $00000010;
  VER_SUITENAME         = $00000040;
  VER_PRODUCT_TYPE      = $00000080;

  // Constants from sdkddkver.h
  _WIN32_WINNT_NT4          = $0400; // Windows NT 4
  _WIN32_WINNT_WIN2K        = $0500; // Windows 2000
  _WIN32_WINNT_WINXP        = $0501; // Windows XP
  _WIN32_WINNT_WS03         = $0502; // Windows Server 2003
  _WIN32_WINNT_WIN6         = $0600; // Windows Vista
  _WIN32_WINNT_VISTA        = $0600; // Windows Vista
  _WIN32_WINNT_WS08         = $0600; // Windows Server 2008
  _WIN32_WINNT_LONGHORN     = $0600; // Windows Vista
  _WIN32_WINNT_WIN7         = $0601; // Windows 7
  _WIN32_WINNT_WIN8         = $0602; // Windows 8
  _WIN32_WINNT_WINBLUE      = $0603; // Windows 8.1
  _WIN32_WINNT_WINTHRESHOLD = $0A00; // Windows 10
  _WIN32_WINNT_WIN10        = $0A00; // Windows 10


  // These Windows-defined constants are required for use with TOSVersionInfoEx
  // NT Product types
  VER_NT_WORKSTATION                          = 1;
  VER_NT_DOMAIN_CONTROLLER                    = 2;
  VER_NT_SERVER                               = 3;
  // Mask representing NT product suites
  VER_SUITE_SMALLBUSINESS                     = $00000001;
  VER_SUITE_ENTERPRISE                        = $00000002;
  VER_SUITE_BACKOFFICE                        = $00000004;
  VER_SUITE_COMMUNICATIONS                    = $00000008;
  VER_SUITE_TERMINAL                          = $00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED          = $00000020;
  VER_SUITE_EMBEDDEDNT                        = $00000040;
  VER_SUITE_DATACENTER                        = $00000080;
  VER_SUITE_SINGLEUSERTS                      = $00000100;
  VER_SUITE_PERSONAL                          = $00000200;
  VER_SUITE_SERVERAPPLIANCE                   = $00000400;
  VER_SUITE_BLADE                             = VER_SUITE_SERVERAPPLIANCE;
  VER_SUITE_EMBEDDED_RESTRICTED               = $00000800;
  VER_SUITE_SECURITY_APPLIANCE                = $00001000;
  VER_SUITE_STORAGE_SERVER                    = $00002000;
  VER_SUITE_COMPUTE_SERVER                    = $00004000;
  VER_SUITE_WH_SERVER                         = $00008000;

  // These Windows-defined constants are required for use with the
  // GetProductInfo API call used with Windows Vista and later
  // ** Thanks to Laurent Pierre for providing these definitions.
  // ** Additional definitions were obtained from
  //    http://msdn.microsoft.com/en-us/library/ms724358
  PRODUCT_BUSINESS                            = $00000006;
  PRODUCT_BUSINESS_N                          = $00000010;
  PRODUCT_CLUSTER_SERVER                      = $00000012;
  PRODUCT_CLUSTER_SERVER_V                    = $00000040;
  PRODUCT_CORE                                = $00000065;
  PRODUCT_CORE_N                              = $00000062;
  PRODUCT_CORE_COUNTRYSPECIFIC                = $00000063;
  PRODUCT_CORE_SINGLELANGUAGE                 = $00000064;
  PRODUCT_DATACENTER_EVALUATION_SERVER        = $00000050;
  PRODUCT_DATACENTER_SERVER                   = $00000008;
  PRODUCT_DATACENTER_SERVER_CORE              = $0000000C;
  PRODUCT_DATACENTER_SERVER_CORE_V            = $00000027;
  PRODUCT_DATACENTER_SERVER_V                 = $00000025;
  PRODUCT_EDUCATION                           = $00000079;
  PRODUCT_EDUCATION_N                         = $0000007A;
  PRODUCT_ENTERPRISE                          = $00000004;
  PRODUCT_ENTERPRISE_E                        = $00000046;
  PRODUCT_ENTERPRISE_N_EVALUATION             = $00000054;
  PRODUCT_ENTERPRISE_N                        = $0000001B;
  PRODUCT_ENTERPRISE_EVALUATION               = $00000048;
  PRODUCT_ENTERPRISE_SERVER                   = $0000000A;
  PRODUCT_ENTERPRISE_SERVER_CORE              = $0000000E;
  PRODUCT_ENTERPRISE_SERVER_CORE_V            = $00000029;
  PRODUCT_ENTERPRISE_SERVER_IA64              = $0000000F;
  PRODUCT_ENTERPRISE_SERVER_V                 = $00000026;
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT       = $0000003B;
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL       = $0000003C;
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC    = $0000003D;
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC    = $0000003E;
  PRODUCT_HOME_BASIC                          = $00000002;
  PRODUCT_HOME_BASIC_E                        = $00000043;
  PRODUCT_HOME_BASIC_N                        = $00000005;
  PRODUCT_HOME_PREMIUM                        = $00000003;
  PRODUCT_HOME_PREMIUM_E                      = $00000044;
  PRODUCT_HOME_PREMIUM_N                      = $0000001A;
  PRODUCT_HOME_PREMIUM_SERVER                 = $00000022;
  PRODUCT_HOME_SERVER                         = $00000013;
  PRODUCT_HYPERV                              = $0000002A;
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT    = $0000001E;
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING     = $00000020;
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY      = $0000001F;
  PRODUCT_MOBILE_CORE                         = $00000068;
  PRODUCT_MOBILE_ENTERPRISE                   = $00000085;
  PRODUCT_MULTIPOINT_STANDARD_SERVER          = $0000004C;
  PRODUCT_MULTIPOINT_PREMIUM_SERVER           = $0000004D;
  PRODUCT_PROFESSIONAL                        = $00000030;
  PRODUCT_PROFESSIONAL_E                      = $00000045;
  PRODUCT_PROFESSIONAL_N                      = $00000031;
  PRODUCT_PROFESSIONAL_WMC                    = $00000067;
  PRODUCT_SB_SOLUTION_SERVER                  = $00000032;
  PRODUCT_SB_SOLUTION_SERVER_EM               = $00000036;
  PRODUCT_SERVER_FOR_SB_SOLUTIONS             = $00000033;
  PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM          = $00000037;
  PRODUCT_SERVER_FOR_SMALLBUSINESS            = $00000018;
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V          = $00000023;
  PRODUCT_SERVER_FOUNDATION                   = $00000021;
  PRODUCT_SMALLBUSINESS_SERVER                = $00000009;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM        = $00000019;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE   = $0000003F;
  PRODUCT_SOLUTION_EMBEDDEDSERVER             = $00000038;
  PRODUCT_STANDARD_EVALUATION_SERVER          = $0000004F;
  PRODUCT_STANDARD_SERVER                     = $00000007;
  PRODUCT_STANDARD_SERVER_CORE                = $0000000D;
  PRODUCT_STANDARD_SERVER_V                   = $00000024;
  PRODUCT_STANDARD_SERVER_CORE_V              = $00000028;
  PRODUCT_STANDARD_SERVER_SOLUTIONS           = $00000034;
  PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE      = $00000035;
  PRODUCT_STARTER                             = $0000000B;
  PRODUCT_STARTER_E                           = $00000042;
  PRODUCT_STARTER_N                           = $0000002F;
  PRODUCT_STORAGE_ENTERPRISE_SERVER           = $00000017;
  PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE      = $0000002E;
  PRODUCT_STORAGE_EXPRESS_SERVER              = $00000014;
  PRODUCT_STORAGE_EXPRESS_SERVER_CORE         = $0000002B;
  PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER  = $00000060;
  PRODUCT_STORAGE_STANDARD_SERVER             = $00000015;
  PRODUCT_STORAGE_STANDARD_SERVER_CORE        = $0000002C;
  PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER = $0000005F;
  PRODUCT_STORAGE_WORKGROUP_SERVER            = $00000016;
  PRODUCT_STORAGE_WORKGROUP_SERVER_CORE       = $0000002D;
  PRODUCT_UNDEFINED                           = $00000000;
  PRODUCT_ULTIMATE                            = $00000001;
  PRODUCT_ULTIMATE_E                          = $00000047;
  PRODUCT_ULTIMATE_N                          = $0000001C;
  PRODUCT_WEB_SERVER                          = $00000011;
  PRODUCT_WEB_SERVER_CORE                     = $0000001D;
  PRODUCT_UNLICENSED                          = $ABCDABCD;

  // These constants are required for use with GetSystemMetrics to detect
  // certain editions. GetSystemMetrics returns non-zero when passed these flags
  // if the associated edition is present.
  // Obtained from http://msdn.microsoft.com/en-us/library/ms724385
  SM_TABLETPC       = 86;     // Detects XP Tablet Edition
  SM_MEDIACENTER    = 87;     // Detects XP Media Center Edition
  SM_STARTER        = 88;     // Detects XP Starter Edition
  SM_SERVERR2       = 89;     // Detects Windows Server 2003 R2
  SM_REMOTESESSION  = $1000;  // Detects a remote terminal server session

  // These constants are required when examining the
  // TSystemInfo.wProcessorArchitecture member.
  // Only constants marked * are defined in the MS 2008 SDK
  PROCESSOR_ARCHITECTURE_UNKNOWN    = $FFFF; // Unknown architecture.
  PROCESSOR_ARCHITECTURE_INTEL          = 0; // x86 *
  PROCESSOR_ARCHITECTURE_MIPS           = 1; // MIPS architecture
  PROCESSOR_ARCHITECTURE_ALPHA          = 2; // Alpha architecture
  PROCESSOR_ARCHITECTURE_PPC            = 3; // PPC architecture
  PROCESSOR_ARCHITECTURE_SHX            = 4; // SHX architecture
  PROCESSOR_ARCHITECTURE_ARM            = 5; // ARM  architecture
  PROCESSOR_ARCHITECTURE_IA64           = 6; // Intel Itanium Processor Family *
  PROCESSOR_ARCHITECTURE_ALPHA64        = 7; // Alpha64 architecture
  PROCESSOR_ARCHITECTURE_MSIL           = 8; // MSIL architecture
  PROCESSOR_ARCHITECTURE_AMD64          = 9; // x64 (AMD or Intel) *
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10; // IA32 on Win64 architecture

  // These constants are provided in case the obsolete
  // TSystemInfo.dwProcessorType needs to be used.
  // Constants marked Windows CE are only used on Windows mobile and are only
  // provided here for completeness.
  // Only constants marked * are defined in MS SDK 6.1
  PROCESSOR_INTEL_386     = 386;   // Intel i386 processor *
  PROCESSOR_INTEL_486     = 486;   // Intel i486 processor *
  PROCESSOR_INTEL_PENTIUM = 586;   // Intel Pentium processor *
  PROCESSOR_INTEL_IA64    = 2200;  // Intel IA64 processor *
  PROCESSOR_AMD_X8664     = 8664;  // AMD X86 64 processor *
  PROCESSOR_MIPS_R4000    = 4000;  // MIPS R4000, R4101, R3910 processor
  PROCESSOR_ALPHA_21064   = 21064; // Alpha 210 64 processor
  PROCESSOR_PPC_601       = 601;   // PPC 601 processor
  PROCESSOR_PPC_603       = 603;   // PPC 603 processor
  PROCESSOR_PPC_604       = 604;   // PPC 604 processor
  PROCESSOR_PPC_620       = 620;   // PPC 620 processor
  PROCESSOR_HITACHI_SH3   = 10003; // Hitachi SH3 processor (Windows CE)
  PROCESSOR_HITACHI_SH3E  = 10004; // Hitachi SH3E processor (Windows CE)
  PROCESSOR_HITACHI_SH4   = 10005; // Hitachi SH4 processor (Windows CE)
  PROCESSOR_MOTOROLA_821  = 821;   // Motorola 821 processor (Windows CE)
  PROCESSOR_SHx_SH3       = 103;   // SHx SH3 processor (Windows CE)
  PROCESSOR_SHx_SH4       = 104;   // SHx SH4 processor (Windows CE)
  PROCESSOR_STRONGARM     = 2577;  // StrongARM processor (Windows CE)
  PROCESSOR_ARM720        = 1824;  // ARM 720 processor (Windows CE)
  PROCESSOR_ARM820        = 2080;  // ARM 820 processor (Windows CE)
  PROCESSOR_ARM920        = 2336;  // ARM 920 processor (Windows CE)
  PROCESSOR_ARM_7TDMI     = 70001; // ARM 7TDMI processor (Windows CE)
  PROCESSOR_OPTIL         = $494F; // MSIL processor


type
  ///  <summary>Enumeration of OS platforms.</summary>
  TPJOSPlatform = (
    ospWinNT,               // Windows NT platform
    ospWin9x,               // Windows 9x platform
    ospWin32s               // Win32s platform
  );

type
  ///  <summary>Enumeration identifying OS product.</summary>
  ///  <remarks>New values are always appended to the end of the enumeration so
  ///  as not to destroy any existing code that depends on the ordinal value of
  ///  the existing values.</remarks>
  TPJOSProduct = (
    osUnknownWinNT,         // Unknown Windows NT OS
    osWinNT,                // Windows NT (up to v4)
    osWin2K,                // Windows 2000
    osWinXP,                // Windows XP
    osUnknownWin9x,         // Unknown Windows 9x OS
    osWin95,                // Windows 95
    osWin98,                // Windows 98
    osWinMe,                // Windows Me
    osUnknownWin32s,        // Unknown OS running Win32s
    osWinSvr2003,           // Windows Server 2003
    osUnknown,              // Completely unknown Windows
    osWinVista,             // Windows Vista
    osWinSvr2003R2,         // Windows Server 2003 R2
    osWinSvr2008,           // Windows Server 2008
    osWinLater,             // A later version of Windows than v6.1
    osWin7,                 // Windows 7
    osWinSvr2008R2,         // Windows Server 2008 R2
    osWin8,                 // Windows 8
    osWinSvr2012,           // Windows Server 2012
    osWin8Point1,           // Windows 8.1
    osWinSvr2012R2,         // Windows Server 2012 R2
    osWin10,                // Windows 10
    osWin10Svr,             // Windows 2016 Server
    osWinSvr2019            // Windows 2019 Server
  );
  
type
  ///  <summary>Enumeration identifying processor architecture.</summary>
  TPJProcessorArchitecture = (
    paUnknown,              // Unknown architecture
    paX64,                  // X64 (AMD or Intel)
    paIA64,                 // Intel Itanium processor family (IPF)
    paX86                   // Intel 32 bit
  );

type
  ///  <summary>Enumeration identifying system boot modes.</summary>
  TPJBootMode = (
    bmUnknown,              // Unknown boot mode
    bmNormal,               // Normal boot
    bmSafeMode,             // Booted in safe mode
    bmSafeModeNetwork       // Booted in safe node with networking
  );

type
  ///  <summary>Class of exception raised by code in this unit.</summary>
  EPJSysInfo = class(Exception);

type
  ///  <summary>Static class that provides operating system version information.
  ///  </summary>
  TPJOSInfo = class(TObject)
  private
    ///  <summary>Gets description of OS product edition from value returned
    ///  from GetProductInfo API.</summary>
    class function EditionFromProductInfo: string;

    ///  <summary>Checks if a given suite is installed on an NT system.
    ///  </summary>
    ///  <param name="Suite">Integer [in] One of the VER_SUITE_* flags.</param>
    ///  <returns>True if suite is installed, False if not installed or not an
    ///  NT platform OS.</returns>
    class function CheckSuite(const Suite: Integer): Boolean;

    ///  <summary>Gets product edition from registry.</summary>
    ///  <remarks>Needed to get edition for NT4 pre SP6.</remarks>
    class function EditionFromReg: string;

    ///  <summary>Checks registry to see if NT4 Service Pack 6a is installed.
    ///  </summary>
    class function IsNT4SP6a: Boolean;

    ///  <summary>Gets code describing product type from registry.</summary>
    ///  <remarks>Used to get product type for NT4 SP5 and earlier.</remarks>
    class function ProductTypeFromReg: string;

    ///  <summary>Checks if the underlying operating system either has the given
    ///  major and minor version number and service pack major version numbers
    ///  or is a later version.</summary>
    ///  <remarks>
    ///  <para>MajorVersion version must be greater than or equal to 5,
    ///  otherwise the method always returns False.</para>
    ///  <para>This method is immune to spoofing: it always returns information
    ///  about the actual operating system.</para>
    ///  </remarks>
    class function IsReallyWindowsVersionOrGreater(MajorVersion, MinorVersion,
      ServicePackMajor: Word): Boolean;

  public

    ///  <summary>Checks if the OS can be "spoofed" by specifying a
    ///  compatibility mode for the program.</summary>
    ///  <remarks>When this method returns True public methods of TPJOSInfo
    ///  will return the details of OS emulated by the compatibility mode OS
    ///  instead of the actual OS, unless the method is documented to the
    ///  contrary. When False is returned the reported OS is the real underlying
    ///  OS and any compatibility mode is ignored.</remarks>
    class function CanSpoof: Boolean;

    ///  <summary>Checks if the OS is on the Windows 9x platform.</summary>
    class function IsWin9x: Boolean;

    ///  <summary>Checks if the OS is on the Windows NT platform.</summary>
    class function IsWinNT: Boolean;

    ///  <summary>Checks if the program is hosted on Win32s.</summary>
    ///  <remarks>This is unlikely to ever return True since Delphi does not run
    ///  on Win32s.</remarks>
    class function IsWin32s: Boolean;

    ///  <summary>Checks if a 32 bit program is running under WOW64 on a 64 bit
    ///  operating system.</summary>
    class function IsWow64: Boolean;

    ///  <summary>Checks if the program is running on a server operating system.
    ///  </summary>
    ///  <remarks>Use IsWindowsServer in preference.</remarks>
    class function IsServer: Boolean;

    ///  <summary>Checks if Windows Media Center is installed.</summary>
    class function IsMediaCenter: Boolean;

    ///  <summary>Checks if the program is running on a tablet PC OS.</summary>
    class function IsTabletPC: Boolean;

    ///  <summary>Checks if the program is running under Windows Terminal Server
    ///  as a client session.</summary>
    class function IsRemoteSession: Boolean;

    ///  <summary>Checks of the host operating system has pen extensions
    ///  installed.</summary>
    class function HasPenExtensions: Boolean;

    ///  <summary>Returns the host OS platform identifier.</summary>
    class function Platform: TPJOSPlatform;

    ///  <summary>Returns the host OS product identifier.</summary>
    class function Product: TPJOSProduct;

    ///  <summary>Returns the product name of the host OS.</summary>
    class function ProductName: string;

    ///  <summary>Returns the major version number of the host OS.</summary>
    class function MajorVersion: Integer;

    ///  <summary>Returns the minor version number of the host OS.</summary>
    class function MinorVersion: Integer;

    ///  <summary>Returns the host OS's build number.</summary>
    ///  <remarks>A return value of 0 indicates that the build number can't be
    ///  found.</remarks>
    class function BuildNumber: Integer;

    ///  <summary>Returns the name of any installed OS service pack.</summary>
    class function ServicePack: string;

    ///  <summary>Returns the name of any installed OS service pack along with
    ///  other similar, detectable, updates.</summary>
    ///  <remarks>
    ///  <para>Windows has added significant OS updates that bump the build
    ///  number but do not declare themselves as service packs: e.g. the Windows
    ///  10 TH2 update, aka Version 1511.</para>
    ///  <para>This method is used to report such updates in addition to
    ///  updates that declare themselves as service packs, while the ServicePack
    ///  method only reports declared 'official' service packs.</para>
    ///  </remarks>
    class function ServicePackEx: string;

    ///  <summary>Returns the major version number of any NT platform service
    ///  pack.</summary>
    ///  <remarks>0 is returned in no service pack is installed, if the host OS
    ///  is not on the NT platform.</remarks>
    class function ServicePackMajor: Integer;

    ///  <summary>Returns the minor version number of any NT platform service
    ///  pack.</summary>
    ///  <remarks>Invalid is ServicePackMinor returns 0.</remarks>
    class function ServicePackMinor: Integer;

    ///  <summary>Returns the product edition for an NT platform OS.</summary>
    ///  <remarks>The empty string is returned if the OS is not on the NT
    ///  platform.</remarks>
    class function Edition: string;

    ///  <summary>Returns a full description of the host OS.</summary>
    class function Description: string;

    ///  <summary>Returns the Windows product ID of the host OS.</summary>
    class function ProductID: string;

    ///  <summary>Organisation to which Windows is registered, if any.</summary>
    class function RegisteredOrganisation: string;

    ///  <summary>Owner to which Windows is registered.</summary>
    class function RegisteredOwner: string;

    ///  <summary>Date the operating system was installed.</summary>
    ///  <remarks>If this information is not available then <c>0.0</c> is
    ///  returned (i.e. 1899/12/30).</remarks>
    class function InstallationDate: TDateTime;

    ///  <summary>Checks whether the OS is Windows 2000 or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force.</remarks>
    class function IsReallyWindows2000OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 2000 Service Pack 1 or
    ///  greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force.</remarks>
    class function IsReallyWindows2000SP1OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 2000 Service Pack 2 or
    ///  greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force.</remarks>
    class function IsReallyWindows2000SP2OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 2000 Service Pack 3 or
    ///  greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force.</remarks>
    class function IsReallyWindows2000SP3OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 2000 Service Pack 4 or
    ///  greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force.</remarks>
    class function IsReallyWindows2000SP4OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows XP or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsXPOrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows XP Service Pack 1 or greater.
    ///  </summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsXPSP1OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows XP Service Pack 2 or greater.
    ///  </summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsXPSP2OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows XP Service Pack 3 or greater.
    ///  </summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsXPSP3OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows Vista or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsVistaOrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows Vista Service Pack 1 or
    ///  greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsVistaSP1OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows Vista Service Pack 2 or
    ///  greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindowsVistaSP2OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 7 or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindows7OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 7 Service Pack 1 or greater.
    ///  </summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindows7SP1OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 8 or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindows8OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 8.1 or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force or whether a suitable
    ///  manifest file is present.</remarks>
    class function IsReallyWindows8Point1OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks whether the OS is Windows 10 or greater.</summary>
    ///  <remarks>This method always returns information about the true OS,
    ///  regardless of any compatibility mode in force, but DOES require that
    ///  the correct manifest file is present.</remarks>
    class function IsReallyWindows10OrGreater: Boolean;
      {$IFDEF INLINEMETHODS}inline;{$ENDIF}

    ///  <summary>Checks if the OS is a server version.</summary>
    ///  <remarks>
    ///  <para>For Windows 2000 and later the result always relates to the
    ///  actual OS, regardless of any compatibility mode in force. For versions
    ///  prior to Windows 2000 this method will take note of compatibility modes
    ///  and returns the same value as TPJOSInfo.IsServer.</para>
    ///  <para>WARNING: For Windows 10 this method is likely to succeed only if
    ///  the application is correctly manifested.</para>
    class function IsWindowsServer: Boolean;
  end;

type
  ///  <summary>Static class that provides information about the host computer.
  ///  </summary>
  TPJComputerInfo = class(TObject)
  public
    ///  <summary>Returns name of host computer.</summary>
    class function ComputerName: string;

    ///  <summary>Returns name of currently logged on user.</summary>
    class function UserName: string;

    ///  <summary>Returns processor architecture of host computer.</summary>
    class function Processor: TPJProcessorArchitecture;

    ///  <summary>Returns number of processors (or cores) in the host computer.
    ///  </summary>
    class function ProcessorCount: Cardinal;

    ///  <summary>Returns the identifier of the computer's processor.</summary>
    ///  <remarks>On multi-processor systems this is the identifier of the 1st
    ///  processor.</remarks>
    class function ProcessorIdentifier: string;

    ///  <summary>Returns the name of the computer's processor.</summary>
    ///  <remarks>On multi-processor systems this is the name of the 1st
    ///  processor.</remarks>
    class function ProcessorName: string;

    ///  <summary>Returns the speed of the computer's processor in MHz.
    ///  </summary>
    ///  <remarks>
    ///  <para>On multi-processor systems this is the speed of the 1st
    ///  processor.</para>
    ///  <para>0 is returned if the information is not a available.</para>
    ///  </remarks>
    class function ProcessorSpeedMHz: Cardinal;

    ///  <summary>Checks if the host computer has a 64 bit processor.</summary>
    class function Is64Bit: Boolean;

    ///  <summary>Checks if a network is present on host computer.</summary>
    class function IsNetworkPresent: Boolean;

    ///  <summary>Returns the OS mode used when host computer was last booted.
    ///  </summary>
    class function BootMode: TPJBootMode;

    ///  <summary>Checks if the current user has administrator privileges.
    ///  </summary>
    ///  <remarks>
    ///  <para>Always returns True on the Windows 9x platform.</para>
    ///  <para>WARNING: True is also returned when running in Windows 9x
    ///  compatibility mode on a Windows NT platform system, regardless of
    ///  whether the user has admin privileges or not.</para>
    ///  <para>Based on code at http://edn.embarcadero.com/article/26752</para>
    ///  </remarks>
    class function IsAdmin: Boolean;

    ///  <summary>Checks if UAC is active on the computer.</summary>
    ///  <remarks>
    ///  <para>UAC requires Windows Vista or later. Returns False on earlier
    ///  operating systems.</para>
    ///  <para>WARNING: False is also returned when running in Windows XP or
    ///  earlier compatibility mode on Windows Vista or later, regardless of
    ///  whether UAC is enabled or not.</para>
    ///  <para>Based on code on Stack Overflow, answer by norgepaul, at
    ///  http://tinyurl.com/avlztmg</para>
    ///  </remarks>
    class function IsUACActive: Boolean;

    ///  <summary>Returns the name of the computer's BIOS vendor.</summary>
    class function BiosVendor: string;

    ///  <summary>Returns the name of the computer's manufacturer.</summary>
    class function SystemManufacturer: string;

    ///  <summary>Returns the computer's product or model name.</summary>
    class function SystemProductName: string;

  end;

type
  ///  <summary>Static class that provides the paths of the system's standard
  ///  folders.</summary>
  TPJSystemFolders = class(TObject)
  public
    ///  <summary>Returns the fully qualified name of the Common Files folder.
    ///  </summary>
    class function CommonFiles: string;

    ///  <summary>Returns the fully qualified name of the Common Files x86
    ///  folder.</summary>
    ///  <remarks>
    ///  <para>Returns the empty string on 32 bit Windows since there is no
    ///  such folder.</para>
    ///  <para>This folder is used common files for 32 bit programs on 64 bit
    ///  Windows systems.</para>
    ///  </remarks>
    class function CommonFilesX86: string;

    ///  <summary>Returns the fully qualified name of the Common Files folder
    ///  according to whether the host program and operating system are 32 or 64
    ///  bit.</summary>
    ///  <remarks>For a 32 bit program on 32 bit Windows or 64 bit program on
    ///  64 bit windows the return value is the same as CommonFiles. For a 32
    ///  bit program running on 64 bit Windows the return value is the same as
    ///  CommonFilesX86.</remarks>
    class function CommonFilesRedirect: string;

    ///  <summary>Returns the fully qualified name of the Program Files folder.
    ///  </summary>
    class function ProgramFiles: string;

    ///  <summary>Returns the fully qualified name of the Program Files x86
    ///  folder, if present.</summary>
    ///  <remarks>
    ///  <para>Returns the empty string on 32 bit Windows since there is no
    ///  such folder.</para>
    ///  <para>This folder is used to install 32 bit programs on 64 bit
    ///  Windows systems.</para>
    ///  </remarks>
    class function ProgramFilesX86: string;

    ///  <summary>Returns the fully qualified name of the Program Files folder
    ///  according to whether the host program and operating system are 32 or 64
    ///  bit.</summary>
    ///  <remarks>For a 32 bit program on 32 bit Windows or 64 bit program on
    ///  64 bit windows the return value is the same as ProgramFiles. For a 32
    ///  bit program running on 64 bit Windows the return value is the same as
    ///  ProgramFilesX86.</remarks>
    class function ProgramFilesRedirect: string;

    ///  <summary>Returns the fully qualified name of the Windows folder.
    ///  </summary>
    class function Windows: string;

    ///  <summary>Returns the fully qualified name of the Windows System folder.
    ///  </summary>
    class function System: string;

    ///  <summary>Returns the fully qualified name of the folder used to store
    ///  shared 32 bit code on 64 bit Windows.</summary>
    class function SystemWow64: string;

    ///  <summary>Returns the fully qualified name of the system's temporary
    ///  folder.</summary>
    class function Temp: string;
  end;

var
  // Global variables providing extended information about the OS version

  // The following five variables are analogues of the similarly named variables
  // (without the "Ex" appendix) from SysUtils. If the OS is spoofed and
  // TOSInfo.CanSpoof = False then these variable will reflect the values from
  // the true OS, whereas their SysUtils equivalents will have the spoof values.
  // When TOSInfo.CanSpoof = True then both sets of variables will have the
  // same value.

  // OS platform: one of VER_PLATFORM_WIN32_NT, VER_PLATFORM_WIN32_WINDOWS or
  // (very unlikely) VER_PLATFORM_WIN32s.
  Win32PlatformEx: Integer = 0;
  // Major version number of OS.
  Win32MajorVersionEx: LongWord = 0;
  // Minor version number of OS.
  Win32MinorVersionEx: LongWord = 0;
  // OS Build number.
  Win32BuildNumberEx: Integer = 0;
  // Description of any OS service pack.
  Win32CSDVersionEx: string = '';

  // Flag that indicates if extended version information is available.
  Win32HaveExInfo: Boolean = False;
  // Major version number of the latest Service Pack installed on the system. If
  // no service pack has been installed the value is 0. Invalid if
  // Win32HaveExInfo is False.
  Win32ServicePackMajor: Word = 0;
  // Minor version number of the latest Service Pack installed on the system.
  // Invalid if Win32HaveExInfo is False.
  Win32ServicePackMinor: Word = 0;
  // Bit flags that identify the product suites available on the system. Value
  // is a combination of the VER_SUITE_* flags defined above. Invalid if
  // Win32HaveExInfo is False.
  Win32SuiteMask: Integer = 0;
  // Additional information about the system. Value is one of the VER_NT_* flags
  // defined above. Invalid if Win32HaveExInfo is False.
  Win32ProductType: Integer = 0;

  // Flag that indicates if product information is available on the OS, i.e. if
  // the GetProductInfo API function is available.
  Win32HaveProductInfo: Boolean = False;
  // Product info for the operating system. Set to 0 if Win32HaveProductInfo
  // is False.
  Win32ProductInfo: LongWord = 0;


implementation


uses
  // Delphi
  {$IFNDEF RTLNAMESPACES}
  Registry, Nb30;
  {$ELSE}
  System.Win.Registry, Winapi.Nb30;
  {$ENDIF}


resourcestring
  // Error messages
  sUnknownPlatform = 'Unrecognised operating system platform';
  sUnknownProduct = 'Unrecognised operating system product';
  sBadRegType =  'Unsupported registry type';
  sBadRegIntType = 'Integer value expected in registry';
  sBadProcHandle = 'Bad process handle';


{$IFNDEF HASUINT64}
// Defined a fake UInt64 of correct size for used with compilers that don't
// define the type.
type
  UInt64 = Int64;
{$ENDIF}


const
  // Map of product codes per GetProductInfo API to product names
  // ** Laurent Pierre supplied original code on which this map is based
  //    It has been modified and extended using MSDN documentation at
  //    http://msdn.microsoft.com/en-us/library/ms724358
  cProductMap: array[1..87] of record
    Id: Cardinal; // product ID
    Name: string; // product name
  end = (
    (Id: PRODUCT_BUSINESS;
      Name: 'Business';),
    (Id: PRODUCT_BUSINESS_N;
      Name: 'Business N';),
    (Id: PRODUCT_CLUSTER_SERVER;
      Name: 'Cluster Server / HPC';),
    (Id: PRODUCT_CLUSTER_SERVER_V;
      Name: 'Server Hyper Core V';),
    (Id: PRODUCT_CORE;
      Name: 'Core / Home';),
    (Id: PRODUCT_CORE_N;
      Name: 'Core N or Home N';),
    (Id: PRODUCT_CORE_COUNTRYSPECIFIC;
      Name: 'Core / Home China';),
    (Id: PRODUCT_CORE_SINGLELANGUAGE;
      Name: 'Core / Home Single Language';),
    (Id: PRODUCT_MOBILE_CORE;
      Name: 'Mobile'),
    (Id: PRODUCT_MOBILE_ENTERPRISE;
      Name: 'Mobile Enterprise'),
    (Id: PRODUCT_EDUCATION;
      Name: 'Education'),
    (Id: PRODUCT_EDUCATION_N;
      Name: 'Education N'),
    (Id: PRODUCT_DATACENTER_EVALUATION_SERVER;
      Name: 'Server Datacenter (evaluation installation)';),
    (Id: PRODUCT_DATACENTER_SERVER;
      Name: 'Server Datacenter (full installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_CORE;
      Name: 'Server Datacenter (core installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_CORE_V;
      Name: 'Server Datacenter without Hyper-V (core installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_V;
      Name: 'Server Datacenter without Hyper-V (full installation)';),
    (Id: PRODUCT_ENTERPRISE;
      Name: 'Enterprise';),
    (Id: PRODUCT_ENTERPRISE_E;
      Name: 'Enterprise E';),
    (Id: PRODUCT_ENTERPRISE_N_EVALUATION;
      Name: 'Enterprise N (evaluation installation)';),
    (Id: PRODUCT_ENTERPRISE_N;
      Name: 'Enterprise N';),
    (Id: PRODUCT_ENTERPRISE_EVALUATION;
      Name: 'Server Enterprise (evaluation installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER;
      Name: 'Server Enterprise (full installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_CORE;
      Name: 'Server Enterprise (core installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_CORE_V;
      Name: 'Server Enterprise without Hyper-V (core installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_IA64;
      Name: 'Server Enterprise for Itanium-based Systems';),
    (Id: PRODUCT_ENTERPRISE_SERVER_V;
      Name: 'Server Enterprise without Hyper-V (full installation)';),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT;
      Name: 'Windows Essential Server Solution Management'),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL;
      Name: 'Windows Essential Server Solution Additional'),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC;
      Name: 'Windows Essential Server Solution Management SVC'),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC;
      Name: 'Windows Essential Server Solution Additional SVC'),
    (Id: PRODUCT_HOME_BASIC;
      Name: 'Home Basic';),
    (Id: PRODUCT_HOME_BASIC_E;
      Name: 'Home Basic E';),
    (Id: PRODUCT_HOME_BASIC_N;
      Name: 'Home Basic N';),
    (Id: PRODUCT_HOME_PREMIUM;
      Name: 'Home Premium';),
    (Id: PRODUCT_HOME_PREMIUM_E;
      Name: 'Home Premium E';),
    (Id: PRODUCT_HOME_PREMIUM_N;
      Name: 'Home Premium N';),
    (Id: PRODUCT_HOME_PREMIUM_SERVER;
      Name: 'Home Server';),
    (Id: PRODUCT_HOME_SERVER;
      Name: 'Home Storage Server';),
    (Id: PRODUCT_HYPERV;
      Name: 'Hyper-V Server'),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT;
      Name: 'Essential Business Server Management Server';),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING;
      Name: 'Essential Business Server Messaging Server';),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY;
      Name: 'Essential Business Server Security Server';),
    (Id: PRODUCT_MULTIPOINT_STANDARD_SERVER;
      Name: 'MultiPoint Server Standard (full installation)';),
    (Id: PRODUCT_MULTIPOINT_PREMIUM_SERVER;
      Name: 'MultiPoint Server Premium (full installation)';),
    (Id: PRODUCT_PROFESSIONAL;
      Name: 'Professional';),
    (Id: PRODUCT_PROFESSIONAL_E;
      Name: 'Professional E';),
    (Id: PRODUCT_PROFESSIONAL_N;
      Name: 'Professional N';),
    (Id: PRODUCT_PROFESSIONAL_WMC;
      Name: 'Professional with Media Center';),
    (Id: PRODUCT_SB_SOLUTION_SERVER_EM;
      Name: 'Server For SB Solutions EM';),
    (Id: PRODUCT_SERVER_FOR_SB_SOLUTIONS;
      Name: 'Server For SB Solutions';),
    (Id: PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM;
      Name: 'Server For SB Solutions EM';),
    (Id: PRODUCT_SERVER_FOR_SMALLBUSINESS;
      Name: 'Server for Essential Server Solutions';),
    (Id: PRODUCT_SERVER_FOR_SMALLBUSINESS_V;
      Name: 'Server 2008 without Hyper-V for Essential Server Solutions';),
    (Id: PRODUCT_SERVER_FOUNDATION;
      Name: 'Server Foundation';),
    (Id: PRODUCT_SB_SOLUTION_SERVER;
      Name: 'Small Business Server Essentials';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER;
      Name: 'Small Business Server';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER_PREMIUM;
      Name: 'Small Business Server Premium';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE;
      Name: 'Small Business Server Premium (core installation)';),
    (Id: PRODUCT_SOLUTION_EMBEDDEDSERVER;
      Name: 'Windows MultiPoint Server';),
    (Id: PRODUCT_STANDARD_EVALUATION_SERVER;
      Name: 'Server Standard (evaluation installation)';),
    (Id: PRODUCT_STANDARD_SERVER;
      Name: 'Server Standard';),
    (Id: PRODUCT_STANDARD_SERVER_CORE;
      Name: 'Server Standard (core installation)';),
    (Id: PRODUCT_STANDARD_SERVER_CORE_V;
      Name: 'Server Standard without Hyper-V (core installation)';),
    (Id: PRODUCT_STANDARD_SERVER_V;
      Name: 'Server Standard without Hyper-V (full installation)';),
    (Id: PRODUCT_STANDARD_SERVER_SOLUTIONS;
      Name: 'Server Solutions Premium';),
    (Id: PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE;
      Name: 'Server Solutions Premium (core installation)';),
    (Id: PRODUCT_STARTER;
      Name: 'Starter';),
    (Id: PRODUCT_STARTER_E;
      Name: 'Starter E';),
    (Id: PRODUCT_STARTER_N;
      Name: 'Starter N';),
    (Id: PRODUCT_STORAGE_ENTERPRISE_SERVER;
      Name: 'Storage Server Enterprise';),
    (Id: PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE;
      Name: 'Storage Server Enterprise (core installation)';),
    (Id: PRODUCT_STORAGE_EXPRESS_SERVER;
      Name: 'Storage Server Express';),
    (Id: PRODUCT_STORAGE_EXPRESS_SERVER_CORE;
      Name: 'Storage Server Express (core installation)';),
    (Id: PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER;
      Name: 'Storage Server Standard (evaluation installation)';),
    (Id: PRODUCT_STORAGE_STANDARD_SERVER;
      Name: 'Storage Server Standard';),
    (Id: PRODUCT_STORAGE_STANDARD_SERVER_CORE;
      Name: 'Storage Server Standard (core installation)';),
    (Id: PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER;
      Name: 'Storage Server Workgroup (evaluation installation)';),
    (Id: PRODUCT_STORAGE_WORKGROUP_SERVER;
      Name: 'Storage Server Workgroup';),
    (Id: PRODUCT_STORAGE_WORKGROUP_SERVER_CORE;
      Name: 'Storage Server Workgroup (core installation)';),
    (Id: PRODUCT_UNDEFINED;
      Name: 'An unknown product';),
    (Id: PRODUCT_ULTIMATE;
      Name: 'Ultimate';),
    (Id: PRODUCT_ULTIMATE_E;
      Name: 'Ultimate E';),
    (Id: PRODUCT_ULTIMATE_N;
      Name: 'Ultimate N';),
    (Id: PRODUCT_WEB_SERVER;
      Name: 'Web Server (full installation)';),
    (Id: PRODUCT_WEB_SERVER_CORE;
      Name: 'Web Server (core installation)';),
    (Id: Cardinal(PRODUCT_UNLICENSED);
      Name: 'Unlicensed product';)
  );

const
  // Array of "current version" registry sub-keys that vary with platform.
  // "False" value is for Window 9x and "True" value is for Windows NT.
  CurrentVersionRegKeys: array[Boolean] of string = (
    'Software\Microsoft\Windows\CurrentVersion',
    'Software\Microsoft\Windows NT\CurrentVersion'
  );

const
  // Known windows build numbers.
  // Sources:
  //   https://en.wikipedia.org/wiki/Windows_NT
  //   https://en.wikipedia.org/wiki/Windows_10_version_history
  //   https://en.wikipedia.org/wiki/Windows_Server_2019
  //   https://en.wikipedia.org/wiki/Windows_Server_2016
  //   https://en.wikipedia.org/wiki/Windows_Server_2016
  //   https://tinyurl.com/y8tfadm2

  // for Vista and Win 7 we have to add service pack number to these values to
  // get actual build number

  WinVistaBaseBuild = 6000;
  Win7BaseBuild = 7600;

  // for Win 8 onwards we just use the build numbers as is

  Win8Build = 9200;             // Build number used for all Win 8/Svr 2012
  Win8Point1Build = 9600;       // Build number used for all Win 8.1/Svr 2012 R2

  Win10TH1Build = 10240;        // Windows 10 TH1 - version 1507 (1st release)
  Win10TH2Build = 10586;        // Windows 10 TH2 - version 1511
  Win10RS1Build = 14393;        // Windows 10 RS1 - version 1607
  Win10RS2Build = 15063;        // Windows 10 RS2 - version 1703
  Win10RS3Build = 16299;        // Windows 10 RS3 - version 1709
  Win10RS4Build = 17134;        // Windows 10 RS4 - version 1803
  Win10RS5Build = 17763;        // Windows 10 RS5 - version 1809
  Win1019H1Build = 18362;       // Windows 10 19H1 - version 1903
  Win1019H2Build = 18363;       // Windows 10 19H2 - version 1909
  Win1020H1Build = 19041;       // Windows 10 20H2 - version 2004

  Win2016TP1Build = 9841;       // Win 2016 Server Technical Preview 1
  Win2016TP2Build = 10074;      // Win 2016 Server Technical Preview 2
  Win2016TP3Build = 10514;      // Win 2016 Server Technical Preview 3
  Win2016TP4Build = 10586;      // Win 2016 Server Technical Preview 4
  Win2016TP5Build = 14300;      // Win 2016 Server Technical Preview 5
  Win2016RTMBuild = 14393;      // Win 2016 Server Release To Manufacturing
  Win2016v1709Build = 16299;    // Win Server 2016 version 1709
  Win2016v1803Build = 17134;    // Win Server 2016 version 1803

  Win2019IP180320Build = 17623; // Win Server 2019 Insider Preview Build 17623
  Win2019IP180324Build = 17627; // Win Server 2019 Insider Preview Build 17627
  Win2019IP180515Build = 17666; // Win Server 2019 Insider Preview Build 17666
  Win2019IP180619Build = 17692; // Win Server 2019 Insider Preview Build 17692
  Win2019IP180710Build = 17709; // Win Server 2019 Insider Preview Build 17709
  Win2019IP180716Build = 17713; // Win Server 2019 Insider Preview Build 17713
  Win2019IP180731Build = 17723; // Win Server 2019 Insider Preview Build 17723
  Win2019IP180814Build = 17733; // Win Server 2019 Insider Preview Build 17733
  Win2019IP180821Build = 17738; // Win Server 2019 Insider Preview Build 17738
  Win2019IP180828Build = 17744; // Win Server 2019 Insider Preview Build 17744
  Win2019v1809Build = 17763;    // Win Server 2019 version 1809
  Win2019v1903Build = 18362;    // Win Server 2019 version 1903
  Win2019v1909Build = 18363;    // Win Server 2019 version 1909

  // Last build number of Windows 2016 Server - after this build number are
  // Windows 2019 Server
  Win2016LastBuild = Win2016v1803Build;

type
  // Function type of the GetNativeSystemInfo and GetSystemInfo functions
  TGetSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
  // Function type of the VerSetConditionMask API function
  TVerSetConditionMask = function(dwlConditionMask: UInt64;
    dwTypeBitMask: LongWord; dwConditionMask: Byte): UInt64; stdcall;
  // Function type of the VerifyVersionInfo API function
  TVerifyVersionInfo = function(lpVersionInfo: POSVersionInfoEx;
    dwTypeMask: LongWord; dwlConditionMask: UInt64): LongBool; stdcall;

var
  // Function used to get system info: initialised to GetNativeSystemInfo API
  // function if available, otherwise set to GetSystemInfo API function.
  GetSystemInfoFn: TGetSystemInfo;

  // Function used to specify conditional to use in tests in VerifyVersionInfo:
  // initialised to VerSetConditionMask API function if available, undefined
  // otherwise.
  VerSetConditionMask: TVerSetConditionMask;

  // Function used to query operating system version: initialised to
  // VerifyVersionInfo API function if available, undefined otherwise.
  VerifyVersionInfo: TVerifyVersionInfo;

var
  // Internal variables recording version information.
  // When using the GetVersionEx API function to get version information these
  // variables have the same value as the similarly named Win32XXX function in
  // SysUtils. When the old API function aren't being used these value *may*
  // vary from the SysUtils versions.
  InternalPlatform: Integer = 0;
  InternalMajorVersion: LongWord = 0;
  InternalMinorVersion: LongWord = 0;
  InternalBuildNumber: Integer = 0;
  InternalCSDVersion: string = '';
  // Internal variable recording processor architecture information
  InternalProcessorArchitecture: Word = 0;
  // Internal variable recording additional update information.
  // ** This was added because Windows 10 TH2 doesn't declare itself as a
  //    service pack, but is a significant update.
  // ** At present this variable is only used for Windows 10.
  InternalExtraUpdateInfo: string = '';

// Flag required when opening registry with specified access flags
{$IFDEF REGACCESSFLAGS}
const
  KEY_WOW64_64KEY = $0100;  // registry access flag not defined in all Delphis
{$ENDIF}

// Tests Windows version (major, minor, service pack major & service pack minor)
// against the given values using the given comparison condition and return
// True if the given version matches the current one or False if not
// Assumes VerifyVersionInfo & VerSetConditionMask APIs functions are available
// Adapted from code from VersionHelpers.pas
// by Achim Kalwa <delphi@achim-kalwa.de> 2014-01-05
function TestWindowsVersion(wMajorVersion, wMinorVersion,
  wServicePackMajor, wServicePackMinor: Word; Condition: Byte): Boolean;
var
  OSVI: TOSVersionInfoEx;
  POSVI: POSVersionInfoEx;
  ConditionalMask: UInt64;
begin
  Assert(Assigned(VerSetConditionMask) and Assigned(VerifyVersionInfo));
  FillChar(OSVI, SizeOf(OSVI), 0);
  OSVI.dwOSVersionInfoSize := SizeOf(OSVI);
  OSVI.dwMajorVersion := wMajorVersion;
  OSVI.dwMinorVersion := wMinorVersion;
  OSVI.wServicePackMajor := wServicePackMajor;
  OSVI.wServicePackMinor := wServicePackMinor;
  POSVI := @OSVI;
  ConditionalMask :=
    VerSetConditionMask(
      VerSetConditionMask(
        VerSetConditionMask(
          VerSetConditionMask(
            0,
            VER_MAJORVERSION,
            Condition
          ),
          VER_MINORVERSION,
          Condition
        ),
        VER_SERVICEPACKMAJOR,
        Condition
      ),
      VER_SERVICEPACKMINOR,
      Condition
    );
  Result := VerifyVersionInfo(
    POSVI,
    VER_MAJORVERSION or VER_MINORVERSION
      or VER_SERVICEPACKMAJOR or VER_SERVICEPACKMINOR,
    ConditionalMask
  );
end;

// Checks if given build number matches that of the current OS.
// Assumes VerifyVersionInfo & VerSetConditionMask APIs functions are available
function IsBuildNumber(BuildNumber: DWORD): Boolean;
var
  OSVI: TOSVersionInfoEx;
  POSVI: POSVersionInfoEx;
  ConditionalMask: UInt64;
begin
  Assert(Assigned(VerSetConditionMask) and Assigned(VerifyVersionInfo));
  FillChar(OSVI, SizeOf(OSVI), 0);
  OSVI.dwOSVersionInfoSize := SizeOf(OSVI);
  OSVI.dwBuildNumber := BuildNumber;
  POSVI := @OSVI;
  ConditionalMask := VerSetConditionMask(0, VER_BUILDNUMBER, VER_EQUAL);
  Result := VerifyVersionInfo(POSVI, VER_BUILDNUMBER, ConditionalMask);
end;

// Checks if the OS has the given product type.
// Assumes VerifyVersionInfo & VerSetConditionMask APIs functions are available
function IsWindowsProductType(ProductType: Byte): Boolean;
var
  ConditionalMask: UInt64;
  OSVI: TOSVersionInfoEx;
  POSVI: POSVersionInfoEx;
begin
  FillChar(OSVI, SizeOf(OSVI), 0);
  OSVI.dwOSVersionInfoSize := SizeOf(OSVI);
  OSVI.wProductType := ProductType;
  POSVI := @OSVI;
  ConditionalMask := VerSetConditionMask(0, VER_PRODUCT_TYPE, VER_EQUAL);
  Result := VerifyVersionInfo(POSVI, VER_PRODUCT_TYPE, ConditionalMask);
end;

// Checks if we are to use the GetVersionEx API function to get version
// information. (GetVersionEx was deprecated in Windows 8.1).
function UseGetVersionAPI: Boolean;

  // Checks if the current OS major and minor version is strictly less than the
  // given major and minor version numbers
  function TestOSLT(Major, Minor: LongWord): Boolean;
  begin
    Result := not Assigned(VerSetConditionMask)
      or not Assigned(VerifyVersionInfo)
      or TestWindowsVersion(Major, Minor, 0, 0, VER_LESS);
  end;

begin
  {$IFNDEF DEBUG_NEW_API}
  // Production code uses GetVersionEx if OS earlier than Windows 8.0
  Result := TestOSLT(6, 2);
  {$ELSE}
  // Debug code uses GetVersionEx if OS earlier than Windows Vista
  Result := TestOSLT(6, 0);
  {$ENDIF}
end;

// Gets Windows version by probing for possible versions
procedure NewGetVersion(out Major, Minor: LongWord; out SPMajor, SPMinor: Word);
begin
  Major := 6;   // lowest version to use this code has major version 6
  Minor := High(Word);
  SPMajor := High(Word);
  SPMinor := High(Word);
  while TestWindowsVersion(Major, Minor, SPMajor, SPMinor, VER_GREATER) do
    Inc(Major);
  Minor := 0;
  while TestWindowsVersion(Major, Minor, SPMajor, SPMinor, VER_GREATER) do
    Inc(Minor);
  SPMajor := 0;
  while TestWindowsVersion(Major, Minor, SPMajor, SPMinor, VER_GREATER) do
    Inc(SPMajor);
  SPMinor := 0;
  while TestWindowsVersion(Major, Minor, SPMajor, SPMinor, VER_GREATER) do
    Inc(SPMinor);
end;

// Loads a function from the OS kernel.
function LoadKernelFunc(const FuncName: string): Pointer;
const
  cKernel = 'kernel32.dll'; // kernel DLL
begin
  Result := GetProcAddress(GetModuleHandle(cKernel), PChar(FuncName));
end;

{$IFNDEF EXCLUDETRAILING}
// Removes any trailing '\' from given directory or path. Used for versions of
// Delphi that don't implement this routine in SysUtils.
function ExcludeTrailingPathDelimiter(const DirOrPath: string) : string;
begin
  Result := DirOrPath;
  while (Result <> '') and (Result[Length(Result)] = '\') do
    Result := Copy(Result, 1, Length(Result) - 1);
end;
{$ENDIF}

// Returns the value of the given environment variable.
function GetEnvVar(const VarName: string): string;
var
  BufSize: Integer;
begin
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end
  else
    Result := '';
end;

// Checks if host OS is Windows 2000 or earlier, including any Win9x OS.
// This is a helper function for RegCreate and RegOpenKeyReadOnly and avoids
// using TPJOSInfo to ensure that an infinite loop is not set up with TPJOSInfo
// calling back into RegCreate.
function IsWin2000OrEarlier: Boolean;
begin
  // NOTE: all Win9x OSs have InternalMajorVersion < 5, so we don't need to
  // check platform.
  Result := (InternalMajorVersion < 5) or
    ((InternalMajorVersion = 5) and (InternalMinorVersion = 0));
end;

// Creates a read only TRegistry instance. On versions of Delphi or OSs that
// don't support passing access flags to TRegistry constructor, registry is
// opened normally for read/write access.
function RegCreate: TRegistry;
begin
  {$IFDEF REGACCESSFLAGS}
  //! Fix for issue #14 (https://sourceforge.net/p/ddablib/tickets/14/)
  //! suggested by Steffen Schaff.
  //! Later modified to allow for fact that Windows 2000 fails if
  //! KEY_WOW64_64KEY is used.
  if IsWin2000OrEarlier then
    Result := TRegistry.Create
  else
    Result := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  {$ELSE}
  Result := TRegistry.Create;
  {$ENDIF}
end;

// Uses registry object to open a key as read only. On versions of Delphi that
// can't open keys as read only the key is opened normally.
function RegOpenKeyReadOnly(const Reg: TRegistry; const Key: string): Boolean;
begin
  {$IFDEF REGACCESSFLAGS}
  //! Fix for problem with OpenKeyReadOnly on 64 bit Windows requires Reg has
  //! (KEY_READ or KEY_WOW64_64KEY) access flags.
  //! Even though these flags aren't provided on Windows 2000 and earlier, the
  //! following code should still work
  if IsWin2000OrEarlier then
    Result := Reg.OpenKeyReadOnly(Key)
  else
    Result := Reg.OpenKey(Key, False);
  {$ELSE}
  // Can't fix Win 64 problem since this version of Delphi does not support
  // customisation of registry access flags.
  Result := Reg.OpenKeyReadOnly(Key);
  {$ENDIF}
end;

// Gets a string value from the given registry sub-key and value within the
// given root key (hive).
function GetRegistryString(const RootKey: HKEY;
  const SubKey, Name: string): string;
var
  Reg: TRegistry;          // registry access object
  ValueInfo: TRegDataInfo; // info about registry value
begin
  Result := '';
  // Open registry at required root key
  Reg := RegCreate;
  try
    Reg.RootKey := RootKey;
    // Open registry key and check value exists
    if RegOpenKeyReadOnly(Reg, SubKey) and Reg.ValueExists(Name) then
    begin
      // Check if registry value is string or integer
      Reg.GetDataInfo(Name, ValueInfo);
      case ValueInfo.RegData of
        rdString, rdExpandString:
          // string value: just return it
          Result := Reg.ReadString(Name);
        rdInteger:
          // integer value: convert to string
          Result := IntToStr(Reg.ReadInteger(Name));
        else
          // unsupported value: raise exception
          raise EPJSysInfo.Create(sBadRegType);
      end;
    end;
  finally
    // Close registry
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function GetRegistryInt(const RootKey: HKEY; const SubKey, Name: string):
  Integer;
var
  Reg: TRegistry;          // registry access object
  ValueInfo: TRegDataInfo; // info about registry value
begin
  Result := 0;
  // Open registry at required root key
  Reg := RegCreate;
  try
    Reg.RootKey := RootKey;
    if RegOpenKeyReadOnly(Reg, SubKey) and Reg.ValueExists(Name) then
    begin
      // Check if registry value is integer
      Reg.GetDataInfo(Name, ValueInfo);
      if ValueInfo.RegData <> rdInteger then
        raise EPJSysInfo.Create(sBadRegIntType);
      Result := Reg.ReadInteger(Name);
    end;
  finally
    // Close registry
    Reg.CloseKey;
    Reg.Free;
  end;
end;

// Gets string info for given value from Windows current version key in
// registry.
function GetCurrentVersionRegStr(ValName: string): string;
const
  // required registry string
  cWdwCurrentVer = '\Software\Microsoft\Windows\CurrentVersion';
begin
  Result := GetRegistryString(HKEY_LOCAL_MACHINE, cWdwCurrentVer, ValName);
end;

// Initialise global variables with extended OS version information if possible.
procedure InitPlatformIdEx;

type
  // Function type of the GetProductInfo API function
  TGetProductInfo = function(OSMajor, OSMinor, SPMajor, SPMinor: DWORD;
    out ProductType: DWORD): BOOL; stdcall;
  // Function type of the GetVersionEx API function
  TGetVersionEx = function(var lpVersionInformation: TOSVersionInfoEx): BOOL;
    stdcall;
var
  OSVI: TOSVersionInfoEx;           // extended OS version info structure
  GetVersionEx: TGetVersionEx;      // pointer to GetVersionEx API function
  GetProductInfo: TGetProductInfo;  // pointer to GetProductInfo API function
  SI: TSystemInfo;                  // structure from GetSystemInfo API call

  // Return name of Windows Server 2019 insider preview release for given build
  // number. Build must be a valid insider preview release number
  function Win2019IPExtra(const Build: Integer): string;
  begin
    Result := Format('Insider Preview Build %d', [Build]);
  end;

begin
  // Load version query functions used externally to this routine
  VerSetConditionMask := LoadKernelFunc('VerSetConditionMask');
  {$IFDEF UNICODE}
  VerifyVersionInfo := LoadKernelFunc('VerifyVersionInfoW');
  {$ELSE}
  VerifyVersionInfo := LoadKernelFunc('VerifyVersionInfoA');
  {$ENDIF}

  if not UseGetVersionAPI then
  begin
    // Not using GetVersion and GetVersionEx functions to get version info
    InternalMajorVersion := 0;
    InternalMinorVersion := 0;
    InternalBuildNumber := 0;
    InternalCSDVersion := '';
    Win32ServicePackMajor := 0;
    Win32ServicePackMinor := 0;
    // we don't use suite mask any more!
    Win32SuiteMask := 0;
    // platform for all OSs tested for this way is always NT: the NewGetVersion
    // calls below indirectly call VerifyVersionInfo API, which is only defined
    // for Windows 2000 and later.
    InternalPlatform := VER_PLATFORM_WIN32_NT;
    Win32HaveExInfo := True;
    NewGetVersion(
      InternalMajorVersion, InternalMinorVersion,
      Win32ServicePackMajor, Win32ServicePackMinor
    );
    // Test possible product types to see which one we have
    if IsWindowsProductType(VER_NT_WORKSTATION) then
      Win32ProductType := VER_NT_WORKSTATION
    else if IsWindowsProductType(VER_NT_DOMAIN_CONTROLLER) then
      Win32ProductType := VER_NT_DOMAIN_CONTROLLER
    else if IsWindowsProductType(VER_NT_SERVER) then
      Win32ProductType := VER_NT_SERVER
    else
      Win32ProductType := 0;
    // NOTE: It's going to be very slow to test for all possible build numbers,
    // so I've narrowed the search down using the information at
    // http://en.wikipedia.org/wiki/Windows_NT
    case InternalMajorVersion of
      6:
      begin
        case InternalMinorVersion of
          0:
            // Vista
            InternalBuildNumber := WinVistaBaseBuild + Win32ServicePackMajor;
          1:
            // Windows 7
            InternalBuildNumber := Win7BaseBuild + Win32ServicePackMajor;
          2:
            // Windows 8 (no known SPs)
            if Win32ServicePackMajor = 0 then
              InternalBuildNumber := Win8Build;
          3:
            // Windows 8.1 (no known SPs)
            if Win32ServicePackMajor = 0 then
              InternalBuildNumber := Win8Point1Build;
          4:
            if (Win32ProductType = VER_NT_DOMAIN_CONTROLLER)
              or (Win32ProductType = VER_NT_SERVER) then
            begin
              // Windows 2016 Server tech preview 1
              InternalBuildNumber := Win2016TP1Build;
              InternalExtraUpdateInfo := 'Technical Preview 6';
            end;
        end;
        if Win32ServicePackMajor > 0 then
          // ** Tried to read this info from registry, but for some weird
          //    reason the required value is reported as non-existant by
          //    TRegistry, even though it is present in registry.
          // ** Seems there is some kind of regitry "spoofing" going on (see
          //    below.
          InternalCSDVersion := Format(
            'Service Pack %d', [Win32ServicePackMajor]
          );
      end;
      10:
      begin
        case InternalMinorVersion of
          0:
          if (Win32ProductType <> VER_NT_DOMAIN_CONTROLLER)
            and (Win32ProductType <> VER_NT_SERVER) then
          begin
            if IsBuildNumber(Win10TH1Build) then
            begin
              InternalBuildNumber := Win10TH1Build;
              InternalExtraUpdateInfo := 'Version 1507';  // 1st Win 10 version
            end
            else if IsBuildNumber(Win10TH2Build) then
            begin
              InternalBuildNumber := Win10TH2Build;
              InternalExtraUpdateInfo := 'Version 1511: November Update';
            end
            else if IsBuildNumber(Win10RS1Build) then
            begin
              InternalBuildNumber := Win10RS1Build;
              InternalExtraUpdateInfo := 'Version 1607: Anniversary Update';
            end
            else if IsBuildNumber(Win10RS2Build) then
            begin
              InternalBuildNumber := Win10RS2Build;
              InternalExtraUpdateInfo := 'Version 1703: Creators Update';
            end
            else if IsBuildNumber(Win10RS3Build) then
            begin
              InternalBuildNumber := Win10RS3Build;
              InternalExtraUpdateInfo := 'Version 1709: Fall Creators Update';
            end
            else if IsBuildNumber(Win10RS4Build) then
            begin
              InternalBuildNumber := Win10RS4Build;
              InternalExtraUpdateInfo := 'Version 1803: April 2018 Update';
            end
            else if IsBuildNumber(Win10RS5Build) then
            begin
              InternalBuildNumber := Win10RS5Build;
              InternalExtraUpdateInfo := 'Version 1809: October 2018 Update';
            end
            else if IsBuildNumber(Win1019H1Build) then
            begin
              InternalBuildNumber := Win1019H1Build;
              InternalExtraUpdateInfo := 'Version 1903: May 2019 Update';
            end
            else if IsBuildNumber(Win1019H2Build) then
            begin
              InternalBuildNumber := Win1019H2Build;
              InternalExtraUpdateInfo := 'Version 1909: Novermber 2019 Update';
            end
            else if IsBuildNumber(Win1020H1Build) then
            begin
              InternalBuildNumber := Win1020H1Build;
              // TODO: Add marketing name below once known
              InternalExtraUpdateInfo := 'Version 2004';
            end
          end
          else
          begin
            // Check for Win Server 2016 echnical previews.
            // We don't check for TP1 // here because that reported version 6.4,
            // not version 10!
            if IsBuildNumber(Win2016TP2Build) then
            begin
              InternalBuildNumber := Win2016TP2Build;
              InternalExtraUpdateInfo := 'Technical Preview 2';
            end
            else if IsBuildNumber(Win2016TP3Build) then
            begin
              InternalBuildNumber := Win2016TP3Build;
              InternalExtraUpdateInfo := 'Technical Preview 3';
            end
            else if IsBuildNumber(Win2016TP4Build) then
            begin
              InternalBuildNumber := Win2016TP4Build;
              InternalExtraUpdateInfo := 'Technical Preview 4';
            end
            else if IsBuildNumber(Win2016TP5Build) then
            begin
              InternalBuildNumber := Win2016TP5Build;
              InternalExtraUpdateInfo := 'Technical Preview 5';
            end
            else if IsBuildNumber(Win2016RTMBuild) then
            begin
              InternalBuildNumber := Win2016RTMBuild;
              InternalExtraUpdateInfo := 'Version 1607';
            end
            else if IsBuildNumber(Win2016v1709Build) then
            begin
              InternalBuildNumber := Win2016v1709Build;
              InternalExtraUpdateInfo := 'Version 1709';
            end
            else if IsBuildNumber(Win2016v1803Build) then
            begin
              InternalBuildNumber := Win2016v1803Build;
              InternalExtraUpdateInfo := 'Version 1803';
            end
            else if IsBuildNumber(Win2019IP180320Build) then
            begin
              InternalBuildNumber := Win2019IP180320Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180320Build);
            end
            else if IsBuildNumber(Win2019IP180324Build) then
            begin
              InternalBuildNumber := Win2019IP180324Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180324Build);
            end
            else if IsBuildNumber(Win2019IP180515Build) then
            begin
              InternalBuildNumber := Win2019IP180515Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180515Build);
            end
            else if IsBuildNumber(Win2019IP180619Build) then
            begin
              InternalBuildNumber := Win2019IP180619Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180619Build);
            end
            else if IsBuildNumber(Win2019IP180710Build) then
            begin
              InternalBuildNumber := Win2019IP180710Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180710Build);
            end
            else if IsBuildNumber(Win2019IP180716Build) then
            begin
              InternalBuildNumber := Win2019IP180716Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180716Build);
            end
            else if IsBuildNumber(Win2019IP180716Build) then
            begin
              InternalBuildNumber := Win2019IP180716Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180716Build);
            end
            else if IsBuildNumber(Win2019IP180731Build) then
            begin
              InternalBuildNumber := Win2019IP180731Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180731Build);
            end
            else if IsBuildNumber(Win2019IP180814Build) then
            begin
              InternalBuildNumber := Win2019IP180814Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180814Build);
            end
            else if IsBuildNumber(Win2019IP180821Build) then
            begin
              InternalBuildNumber := Win2019IP180821Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180821Build);
            end
            else if IsBuildNumber(Win2019IP180828Build) then
            begin
              InternalBuildNumber := Win2019IP180828Build;
              InternalExtraUpdateInfo := Win2019IPExtra(Win2019IP180828Build);
            end
            else if IsBuildNumber(Win2019v1809Build) then
            begin
              InternalBuildNumber := Win2019v1809Build;
              InternalExtraUpdateInfo := 'Version 1809';
            end
            else if IsBuildNumber(Win2019v1903Build) then
            begin
              InternalBuildNumber := Win2019v1903Build;
              InternalExtraUpdateInfo := 'Version 1903';
            end
            else if IsBuildNumber(Win2019v1909Build) then
            begin
              InternalBuildNumber := Win2019v1909Build;
              InternalExtraUpdateInfo := 'Version 1909';
            end
          end;
        end;
      end;
    end;

    // ** If InternalBuildNumber is 0 when we get here then we failed to get it
    //    We no longer look in registry as of SVN commit r2001, because this can
    //    get spoofed. E.g. when running on Windows 10 TH2 registry call is
    //    returning build number of 7600 even though regedit reveals it to be
    //    10586 !
    //    So we must now consider a build number of 0 as indicating an unknown
    //    build number.
    //    But note that some users report that their registry is returning
    //    correct value. I really hate Windows!!!
    // ** Seems like more registry spoofing (see above).

  end
  else
  begin
    // Get internal OS version information from SysUtils.Win32XXX routines,
    // which in turn gets it from GetVersion or GetVersionEx API call in
    // SysUtils.
    InternalPlatform := Win32Platform;
    InternalMajorVersion := Win32MajorVersion;
    InternalMinorVersion := Win32MinorVersion;
    InternalBuildNumber := Win32BuildNumber;
    InternalCSDVersion := Win32CSDVersion;
    // Try to get extended information
    {$IFDEF UNICODE}
    GetVersionEx := LoadKernelFunc('GetVersionExW');
    {$ELSE}
    GetVersionEx := LoadKernelFunc('GetVersionExA');
    {$ENDIF}
    FillChar(OSVI, SizeOf(OSVI), 0);
    OSVI.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
    Win32HaveExInfo := GetVersionEx(OSVI);
    if Win32HaveExInfo then
    begin
      // We have extended info: store details in global vars
      Win32ServicePackMajor := OSVI.wServicePackMajor;
      Win32ServicePackMinor := OSVI.wServicePackMinor;
      Win32SuiteMask := OSVI.wSuiteMask;
      Win32ProductType := OSVI.wProductType;
    end;
  end;

  Win32PlatformEx := InternalPlatform;
  Win32MajorVersionEx := InternalMajorVersion;
  Win32MinorVersionEx := InternalMinorVersion;
  Win32BuildNumberEx := InternalBuildNumber;
  Win32CSDVersionEx := InternalCSDVersion;

  // Try to get product info (API introduced with Windows Vista)
  GetProductInfo := LoadKernelFunc('GetProductInfo');
  Win32HaveProductInfo := Assigned(GetProductInfo);
  if Win32HaveProductInfo then
  begin
    if not GetProductInfo(
      InternalMajorVersion, InternalMinorVersion,
      Win32ServicePackMajor, Win32ServicePackMinor,
      Win32ProductInfo
    ) then
      Win32ProductInfo := PRODUCT_UNDEFINED;
  end
  else
    Win32ProductInfo := PRODUCT_UNDEFINED;

  // Set GetSystemInfoFn to GetNativeSystemInfo() API if available, otherwise
  // use GetSystemInfo().
  GetSystemInfoFn := LoadKernelFunc('GetNativeSystemInfo');
  if not Assigned(GetSystemInfoFn) then
    GetSystemInfoFn := GetSystemInfo;
  GetSystemInfoFn(SI);
  // Get processor architecture
  InternalProcessorArchitecture := SI.wProcessorArchitecture;
end;

{ TPJOSInfo }

class function TPJOSInfo.BuildNumber: Integer;
begin
  Result := InternalBuildNumber;
end;

class function TPJOSInfo.CanSpoof: Boolean;
begin
  Result := UseGetVersionAPI;
end;

class function TPJOSInfo.CheckSuite(const Suite: Integer): Boolean;
begin
  Result := Win32SuiteMask and Suite <> 0;
end;

class function TPJOSInfo.Description: string;

  // Adds a non-empty string to end of result, preceded by space.
  procedure AppendToResult(const Str: string);
  begin
    if Str <> '' then
      Result := Result + ' ' + Str;
  end;

begin
  // Start with product name
  Result := ProductName;
  case Platform of
    ospWinNT:
    begin
      // We have an NT OS
      // append any product type
      if Product = osWinNT then
      begin
        // For NT3/4 append version number after product
        AppendToResult(Format('%d.%d', [MajorVersion, MinorVersion]));
        AppendToResult(Edition);
        AppendToResult(ServicePackEx);  // does nothing if no service pack etc
        AppendToResult(Format('(Build %d)', [BuildNumber]));
      end
      else
      begin
        // Windows 2000 and later: don't include version number
        AppendToResult(Edition);
        AppendToResult(ServicePackEx);  // does nothing if no service pack
        AppendToResult(Format('(Build %d)', [BuildNumber]));
      end;
    end;
    ospWin9x:
      // We have a Win 95 line OS: append service pack
      AppendToResult(ServicePack);
  end;
end;

class function TPJOSInfo.Edition: string;
begin
  // This method is based on sample C++ code from MSDN
  Result := '';
  case Product of
    osWinVista, osWinSvr2008,
    osWin7, osWinSvr2008R2,
    osWin8, osWinSvr2012,
    osWin8Point1, osWinSvr2012R2,
    osWin10, osWin10Svr, osWinSvr2019:
    begin
      // For v6.0 and later we ignore the suite mask and use the new
      // PRODUCT_ flags from the GetProductInfo() function to determine the
      // edition
      Result := EditionFromProductInfo;
      // append 64-bit if 64 bit system
      if InternalProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
        Result := Result + ' (64-bit)';
      // can detect 32-bit if required by checking if
      // InternalProcessorArchitecture = PROCESSOR_ARCHITECTURE_INTEL
    end;
    osWinSvr2003, osWinSvr2003R2:
    begin
      // We check different processor architectures and act accordingly
      // This code closely based on MS's sample code found at
      // http://msdn2.microsoft.com/en-us/library/ms724429
      if InternalProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64 then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Edition for Itanium-based Systems'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise Edition for Itanium-based Systems';
      end
      else if InternalProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter x64 Edition'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise x64 Edition'
        else
          Result := 'Standard x64 Edition';
      end
      else
      begin
        if CheckSuite(VER_SUITE_WH_SERVER) then
          Result := 'Home Server'
        else if CheckSuite(VER_SUITE_COMPUTE_SERVER) then
          Result := 'Compute Cluster Edition'
        else if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Edition'
        else if CheckSuite(VER_SUITE_BLADE) then
          Result := 'Web Edition'
        else if CheckSuite(VER_SUITE_STORAGE_SERVER) then
          Result := 'Storage Server'
        // According to MSDN we can't rely on VER_SUITE_SMALLBUSINESS since it
        // is not removed when upgrading to standard or enterprises editions.
        // When installing Small Business edition both VER_SUITE_SMALLBUSINESS
        // and VER_SUITE_SMALLBUSINESS_RESTRICTED are set. When installing
        // standard edition VER_SUITE_SMALLBUSINESS_RESTRICTED gets unset while
        // VER_SUITE_SMALLBUSINESS remains. So, we first check for the
        // Enterprise edition and exclude Small Business if we find that.
        // Since there is no flag for Standard Edition we check for both
        // VER_SUITE_SMALLBUSINESS and VER_SUITE_SMALLBUSINESS_RESTRICTED and
        // assume Small Business if we find both, otherwise we assume Standard
        // edition.
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise Edition'
        else if CheckSuite(VER_SUITE_SMALLBUSINESS) and
          CheckSuite(VER_SUITE_SMALLBUSINESS_RESTRICTED) then
          Result := 'Small Business Edition'
        else
          Result := 'Standard Edition';
      end;
    end;
    osWinXP:
    begin
      if GetSystemMetrics(SM_STARTER) <> 0 then
        Result := 'Starter Edition'
      else if (InternalMajorVersion = 5) and (InternalMinorVersion = 2) and
        not IsServer and  // XP Pro 64 has version 5.2 not 5.1!
        (InternalProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
        Result := 'Professional x64 Edition'
      else if CheckSuite(VER_SUITE_PERSONAL) then
        Result := 'Home Edition'
      else
        Result := 'Professional';
    end;
    osWin2K:
    begin
      if IsServer then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Server'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Advanced Server'
        else
          Result := 'Server';
      end
      else
        Result := 'Professional';
    end;
    osWinNT:
    begin
      if Win32HaveExInfo then
      begin
        // This is NT SP6 or later: got info from OS
        if IsServer then
        begin
          if CheckSuite(VER_SUITE_ENTERPRISE) then
            Result := 'Enterprise Edition'
          else
            Result := 'Server';
        end
        else
          Result := 'Workstation'
      end
      else
        // NT before SP6: we read required info from registry
        Result := EditionFromReg;
    end;
  end;
end;

class function TPJOSInfo.EditionFromProductInfo: string;
var
  Idx: Integer; // loops through entries in cProductMap[]
begin
  Result := '';
  for Idx := Low(cProductMap) to High(cProductMap) do
  begin
    if cProductMap[Idx].Id = Win32ProductInfo then
    begin
      Result := cProductMap[Idx].Name;
      Exit;
    end;
  end;
end;

class function TPJOSInfo.EditionFromReg: string;
var
  EditionCode: string;  // OS product edition code stored in registry
begin
  EditionCode := ProductTypeFromReg;
  if CompareText(EditionCode, 'WINNT') = 0 then
    Result := 'WorkStation'
  else if CompareText(EditionCode, 'LANMANNT') = 0 then
    Result := 'Server'
  else if CompareText(EditionCode, 'SERVERNT') = 0 then
    Result := 'Advanced Server';
  Result := Result + Format(
    ' %d.%d', [InternalMajorVersion, InternalMinorVersion]
  );
end;

class function TPJOSInfo.HasPenExtensions: Boolean;
begin
  Result := GetSystemMetrics(SM_PENWINDOWS) <> 0;
end;

class function TPJOSInfo.InstallationDate: TDateTime;
var
  DateStr: string;
  UnixDate: LongWord;
const
  UnixStartDate: TDateTime = 25569.0; // 1970/01/01
begin
  DateStr := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'InstallDate'
  );
  Result := 0.0;
  if DateStr = '' then
    Exit;
  UnixDate := StrToIntDef(DateStr, 0);
  if UnixDate = 0 then
    Exit;
  Result := (UnixDate / 86400) + UnixStartDate
end;

class function TPJOSInfo.IsMediaCenter: Boolean;
begin
  Result := GetSystemMetrics(SM_MEDIACENTER) <> 0;
end;

class function TPJOSInfo.IsNT4SP6a: Boolean;
var
  Reg: TRegistry; // registry access object
begin
  if (Product = osWinNT)
    and (InternalMajorVersion = 4)
    and (CompareText(InternalCSDVersion, 'Service Pack 6') = 0) then
  begin
    // System is reporting NT4 SP6
    // we have SP 6a if particular registry key exists
    Reg := RegCreate;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Result := Reg.KeyExists(
        'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009'
      );
    finally
      Reg.Free;
    end;
  end
  else
    // System not reporting NT4 SP6, so not SP6a!
    Result := False;
end;

class function TPJOSInfo.IsReallyWindows2000OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN2K), LoByte(_WIN32_WINNT_WIN2K), 0
  );
end;

class function TPJOSInfo.IsReallyWindows2000SP1OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN2K), LoByte(_WIN32_WINNT_WIN2K), 1
  );
end;

class function TPJOSInfo.IsReallyWindows2000SP2OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN2K), LoByte(_WIN32_WINNT_WIN2K), 2
  );
end;

class function TPJOSInfo.IsReallyWindows2000SP3OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN2K), LoByte(_WIN32_WINNT_WIN2K), 3
  );
end;

class function TPJOSInfo.IsReallyWindows2000SP4OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN2K), LoByte(_WIN32_WINNT_WIN2K), 4
  );
end;

class function TPJOSInfo.IsReallyWindows7OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN7), LoByte(_WIN32_WINNT_WIN7), 0
  );
end;

class function TPJOSInfo.IsReallyWindows7SP1OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN7), LoByte(_WIN32_WINNT_WIN7), 1
  );
end;

class function TPJOSInfo.IsReallyWindows8OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN8), LoByte(_WIN32_WINNT_WIN8), 0
  );
end;

class function TPJOSInfo.IsReallyWindows8Point1OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WINBLUE), LoByte(_WIN32_WINNT_WINBLUE), 0
  );
end;

class function TPJOSInfo.IsReallyWindows10OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WIN10), LoByte(_WIN32_WINNT_WIN10), 0
  );
end;

class function TPJOSInfo.IsReallyWindowsVersionOrGreater(MajorVersion,
  MinorVersion, ServicePackMajor: Word): Boolean;
begin
  Assert(MajorVersion >= HiByte(_WIN32_WINNT_WIN2K));
  if Assigned(VerSetConditionMask) and Assigned(VerifyVersionInfo) then
    Result := TestWindowsVersion(
      MajorVersion, MinorVersion, ServicePackMajor, 0, VER_GREATER_EQUAL
    )
  else
    Result := False;
end;

class function TPJOSInfo.IsReallyWindowsVistaOrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_VISTA), LoByte(_WIN32_WINNT_VISTA), 0
  );
end;

class function TPJOSInfo.IsReallyWindowsVistaSP1OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_VISTA), LoByte(_WIN32_WINNT_VISTA), 1
  );
end;

class function TPJOSInfo.IsReallyWindowsVistaSP2OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_VISTA), LoByte(_WIN32_WINNT_VISTA), 2
  );
end;

class function TPJOSInfo.IsReallyWindowsXPOrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WINXP), LoByte(_WIN32_WINNT_WINXP), 0
  );
end;

class function TPJOSInfo.IsReallyWindowsXPSP1OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WINXP), LoByte(_WIN32_WINNT_WINXP), 1
  );
end;

class function TPJOSInfo.IsReallyWindowsXPSP2OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WINXP), LoByte(_WIN32_WINNT_WINXP), 2
  );
end;

class function TPJOSInfo.IsReallyWindowsXPSP3OrGreater: Boolean;
begin
  Result := IsReallyWindowsVersionOrGreater(
    HiByte(_WIN32_WINNT_WINXP), LoByte(_WIN32_WINNT_WINXP), 3
  );
end;

class function TPJOSInfo.IsRemoteSession: Boolean;
begin
  Result := GetSystemMetrics(SM_REMOTESESSION) <> 0;
end;

class function TPJOSInfo.IsServer: Boolean;
begin
  if InternalPlatform <> VER_PLATFORM_WIN32_NT then
    // Not WinNT platform => can't be a server
    Result := False
  else if Win32HaveExInfo then
    // Check product type from extended OS info
    Result := (Win32ProductType = VER_NT_DOMAIN_CONTROLLER)
      or (Win32ProductType = VER_NT_SERVER)
  else
    // Check product type stored in registry
    Result := CompareText(ProductTypeFromReg, 'WINNT') <> 0;;
end;

class function TPJOSInfo.IsTabletPC: Boolean;
begin
  Result := GetSystemMetrics(SM_TABLETPC) <> 0;
end;

class function TPJOSInfo.IsWin32s: Boolean;
begin
  Result := Platform = ospWin32s;
end;

class function TPJOSInfo.IsWin9x: Boolean;
begin
  Result := Platform = ospWin9x;
end;

class function TPJOSInfo.IsWindowsServer: Boolean;
var
  OSVI: TOSVersionInfoEx;
  ConditionMask: UInt64;
begin
  if Assigned(VerSetConditionMask) and Assigned(VerifyVersionInfo) then
  begin
    FillChar(OSVI, SizeOf(OSVI), 0);
    OSVI.dwOSVersionInfoSize := SizeOf(OSVI);
    OSVI.wProductType := VER_NT_WORKSTATION;
    ConditionMask := VerSetConditionMask(0, VER_PRODUCT_TYPE, VER_EQUAL);
    Result := not VerifyVersionInfo(@OSVI, VER_PRODUCT_TYPE, ConditionMask);
  end
  else
    Result := IsServer;
end;

class function TPJOSInfo.IsWinNT: Boolean;
begin
  Result := Platform = ospWinNT;
end;

class function TPJOSInfo.IsWow64: Boolean;
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: THandle;
    var Res: BOOL
  ): BOOL; stdcall;
var
  IsWow64Result: BOOL;              // result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := LoadKernelFunc('IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create(sBadProcHandle);
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
end;

class function TPJOSInfo.MajorVersion: Integer;
begin
  Result := InternalMajorVersion;
end;

class function TPJOSInfo.MinorVersion: Integer;
begin
  Result := InternalMinorVersion;
end;

class function TPJOSInfo.Platform: TPJOSPlatform;
begin
  case InternalPlatform of
    VER_PLATFORM_WIN32_NT: Result := ospWinNT;
    VER_PLATFORM_WIN32_WINDOWS: Result := ospWin9x;
    VER_PLATFORM_WIN32s: Result := ospWin32s;
    else raise EPJSysInfo.Create(sUnknownPlatform);
  end;
end;

class function TPJOSInfo.Product: TPJOSProduct;
begin
  Result := osUnknown;
  case Platform of
    ospWin9x:
    begin
      // Win 9x platform: only major version is 4
      Result := osUnknownWin9x;
      case InternalMajorVersion of
        4:
        begin
          case InternalMinorVersion of
            0: Result := osWin95;
            10: Result := osWin98;
            90: Result := osWinMe;
          end;
        end;
      end;
    end;
    ospWinNT:
    begin
      // NT platform OS
      Result := osUnknownWinNT;
      case InternalMajorVersion of
        3, 4:
        begin
          // NT 3 or 4
          case InternalMinorVersion of
            0: Result := osWinNT;
          end;
        end;
        5:
        begin
          // Windows 2000 or XP
          case InternalMinorVersion of
            0:
              Result := osWin2K;
            1:
              Result := osWinXP;
            2:
            begin
              if GetSystemMetrics(SM_SERVERR2) <> 0 then
                Result := osWinSvr2003R2
              else
              begin
                if not IsServer and
                  (InternalProcessorArchitecture
                    = PROCESSOR_ARCHITECTURE_AMD64) then
                  Result := osWinXP // XP Pro X64
                else
                  Result := osWinSvr2003
              end
            end;
          end;
        end;
        6:
        begin
          case InternalMinorVersion of
            0:
              if not IsServer then
                Result := osWinVista
              else
                Result := osWinSvr2008;
            1:
              if not IsServer then
                Result := osWin7
              else
                Result := osWinSvr2008R2;
            2:
              if not IsServer then
                Result := osWin8
              else
                Result := osWinSvr2012;
            3:
              // NOTE: Version 6.3 may only be reported by Windows if the
              // application is "manifested" for Windows 8.1. See
              // http://bit.ly/MJSO8Q. Getting the OS via VerifyVersionInfo
              // instead of GetVersion or GetVersionEx should work round this
              // for Windows 8.1 (i.e. version 6.3).
              if not IsServer then
                Result := osWin8Point1
              else
                Result := osWinSvr2012R2;
            4:
              // Version 6.4 was used for Windows 2016 server tech preview 1.
              // This version *may* only be detected by Windows if the
              // application is "manifested" for the correct Windows version.
              // See http://bit.ly/MJSO8Q.
              if IsServer then
                Result := osWin10Svr;
            else
              // Higher minor version: must be an unknown later OS
              Result := osWinLater
          end;
        end;
        10:
        begin
          // NOTE: Version 10 and later may only be reported by Windows if the
          // application is "manifested" for the correct Windows version. See
          // http://bit.ly/MJSO8Q. Previously, getting the OS from
          // VerifyVersionInfo instead of GetVersion or GetVersionEx worked
          // round this, but MS deprecated this in Windows 10, reverting
          // VerifyVersionInfo to work like GetVersion. WHY????!!!!
          case InternalMinorVersion of
            0:
              if not IsServer then
                Result := osWin10
              else
                if InternalBuildNumber <= Win2016LastBuild then
                  Result := osWin10Svr
                else
                  Result := osWinSvr2019;
          end;
        end;
        else
          // Higher major version: must be an unknown later OS
          Result := osWinLater;
      end;
    end;
    ospWin32s:
      // Windows 32s: probably won't ever get this
      Result := osUnknownWin32s;
  end;
end;

class function TPJOSInfo.ProductID: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'ProductID'
  );
end;

class function TPJOSInfo.ProductName: string;
begin
  case Product of
    osUnknownWinNT, osUnknownWin9x, osUnknownWin32s: Result := '';
    osWinNT: Result := 'Windows NT';
    osWin2K: Result := 'Windows 2000';
    osWinXP: Result := 'Windows XP';
    osWinVista: Result := 'Windows Vista';
    osWinSvr2008: Result := 'Windows Server 2008';
    osWin95: Result := 'Windows 95';
    osWin98: Result := 'Windows 98';
    osWinMe: Result := 'Windows Me';
    osWinSvr2003: Result := 'Windows Server 2003';
    osWinSvr2003R2: Result := 'Windows Server 2003 R2';
    osWinLater: Result := Format(
      'Windows Version %d.%d', [InternalMajorVersion, InternalMinorVersion]
    );
    osWin7: Result := 'Windows 7';
    osWinSvr2008R2: Result := 'Windows Server 2008 R2';
    osWin8: Result := 'Windows 8';
    osWinSvr2012: Result := 'Windows Server 2012';
    osWin8Point1: Result := 'Windows 8.1';
    osWinSvr2012R2: Result := 'Windows Server 2012 R2';
    osWin10: Result := 'Windows 10';
    osWin10Svr: Result := 'Windows Server 2016';
    osWinSvr2019: Result := 'Windows Server 2019';
    else
      raise EPJSysInfo.Create(sUnknownProduct);
  end;
end;

class function TPJOSInfo.ProductTypeFromReg: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\ProductOptions',
    'ProductType'
  );
end;

class function TPJOSInfo.RegisteredOrganisation: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'RegisteredOrganization'
  );
end;

class function TPJOSInfo.RegisteredOwner: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'RegisteredOwner'
  );
end;

class function TPJOSInfo.ServicePack: string;
begin
  // Assume no service pack
  Result := '';
  case Platform of
    ospWin9x:
      // On the Windows 9x platform we decode the service pack info
      if InternalCSDVersion <> '' then
      begin
        case Product of
          osWin95:
            {$IFDEF UNICODE}
            if CharInSet(InternalCSDVersion[1], ['B', 'b', 'C', 'c']) then
            {$ELSE}
            if InternalCSDVersion[1] in ['B', 'b', 'C', 'c'] then
            {$ENDIF}
              Result := 'OSR2';
          osWin98:
            {$IFDEF UNICODE}
            if CharInSet(InternalCSDVersion[1], ['A', 'a']) then
            {$ELSE}
            if InternalCSDVersion[1] in ['A', 'a'] then
            {$ENDIF}
              Result := 'SE';
        end;
      end;
    ospWinNT:
      // On Windows NT we return service pack string, unless NT4 SP6 when we
      // need to check whether actually SP6 or SP6a
      if IsNT4SP6a then
        Result := 'Service Pack 6a' // do not localize
      else
        Result := InternalCSDVersion;
  end;
end;

class function TPJOSInfo.ServicePackEx: string;
begin
  Result := ServicePack;
  if Result = '' then
    Result := InternalExtraUpdateInfo
  else
    Result := Result + ', ' + InternalExtraUpdateInfo;
end;

class function TPJOSInfo.ServicePackMajor: Integer;
begin
  Result := Win32ServicePackMajor;
end;

class function TPJOSInfo.ServicePackMinor: Integer;
begin
  Result := Win32ServicePackMinor;
end;

{ TPJComputerInfo }

class function TPJComputerInfo.BiosVendor: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\',
    'BIOSVendor'
  );
end;

class function TPJComputerInfo.BootMode: TPJBootMode;
begin
  case GetSystemMetrics(SM_CLEANBOOT) of
    0: Result := bmNormal;
    1: Result := bmSafeMode;
    2: Result := bmSafeModeNetwork;
    else Result := bmUnknown;
  end;
end;

class function TPJComputerInfo.ComputerName: string;
var
  PComputerName:  // buffer for name returned from API
    array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;    // size of name buffer
begin
  Size := MAX_COMPUTERNAME_LENGTH;
  if GetComputerName(PComputerName, Size) then
    Result := PComputerName
  else
    Result := '';
end;

class function TPJComputerInfo.Is64Bit: Boolean;
begin
  Result := Processor in [paX64, paIA64];
end;

class function TPJComputerInfo.IsAdmin: Boolean;
const
  // SID related constants
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  AccessToken: THandle;           // process access token
  TokenGroupsInfo: PTokenGroups;  // token groups
  InfoBufferSize: DWORD;          // token info buffer size
  AdmininstratorsSID: PSID;       // administrators SID
  I: Integer;                     // loops thru token groups
  Success: BOOL;                  // API function success results
begin
  if not TPJOSInfo.IsWinNT then
  begin
    // Admin mode is a foreign concept to Windows 9x - everyone is an admin
    Result := True;
    Exit;
  end;
  Result := False;
  Success := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, AccessToken);
  if not Success then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      Success := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, AccessToken);
  end;
  if Success then
  begin
    GetMem(TokenGroupsInfo, 1024);
    Success := GetTokenInformation(
      AccessToken, TokenGroups, TokenGroupsInfo, 1024, InfoBufferSize
    );
    CloseHandle(AccessToken);
    if Success then
    begin
      AllocateAndInitializeSid(
        SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID,
        DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, AdmininstratorsSID
      );
      {$IFOPT R+}
        {$DEFINE RANGECHECKSWEREON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGECHECKSWEREON}
      {$ENDIF}
      for I := 0 to TokenGroupsInfo.GroupCount - 1 do
        if EqualSid(AdmininstratorsSID, TokenGroupsInfo.Groups[I].Sid) then
        begin
          Result := True;
          Break;
        end;
      {$IFDEF RANGECHECKSWEREON}
        {$R+}
      {$ENDIF}
      FreeSid(AdmininstratorsSID);
    end;
    if Assigned(TokenGroupsInfo) then
      FreeMem(TokenGroupsInfo);
  end;
end;

class function TPJComputerInfo.IsNetworkPresent: Boolean;
begin
  Result := GetSystemMetrics(SM_NETWORK) and 1 = 1;
end;

class function TPJComputerInfo.IsUACActive: Boolean;
var
  Reg: TRegistry;   // registry access object
begin
  Result := False;
  if not TPJOSInfo.IsWinNT or (TPJOSInfo.MajorVersion < 6) then
    // UAC only available on Vista or later
    Exit;
  Reg := RegCreate;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if RegOpenKeyReadOnly(
      Reg, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System'
    ) then
      Result := Reg.ValueExists('EnableLUA') and Reg.ReadBool('EnableLUA');
  finally
    Reg.Free;
  end;
end;

class function TPJComputerInfo.Processor: TPJProcessorArchitecture;
begin
  case InternalProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL: Result := paX86;
    PROCESSOR_ARCHITECTURE_AMD64: Result := paX64;
    PROCESSOR_ARCHITECTURE_IA64:  Result := paIA64;
    else Result := paUnknown;
  end;
end;

class function TPJComputerInfo.ProcessorCount: Cardinal;
var
  SI: TSystemInfo;  // contains system information
begin
  GetSystemInfoFn(SI);
  Result := SI.dwNumberOfProcessors;
end;

class function TPJComputerInfo.ProcessorIdentifier: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\CentralProcessor\0\',
    'Identifier'
  );
end;

class function TPJComputerInfo.ProcessorName: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\CentralProcessor\0\',
    'ProcessorNameString'
  );
end;

class function TPJComputerInfo.ProcessorSpeedMHz: Cardinal;
begin
  Result := Cardinal(
    GetRegistryInt(
      HKEY_LOCAL_MACHINE,
      'HARDWARE\DESCRIPTION\System\CentralProcessor\0\',
      '~MHz'
    )
  );
end;

class function TPJComputerInfo.SystemManufacturer: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\',
    'SystemManufacturer'
  );
end;

class function TPJComputerInfo.SystemProductName: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\',
    'SystemProductName'
  );
end;

class function TPJComputerInfo.UserName: string;
const
  UNLEN = 256;  // max size of user name buffer (per MS SDK docs)
var
  PUserName: array[0..UNLEN] of Char; // buffer for name returned from API
  Size: DWORD;                        // size of name buffer
begin
  Size := UNLEN;
  if GetUserName(PUserName, Size) then
    Result := PUserName
  else
    Result := '';
end;

{ TPJSystemFolders }

class function TPJSystemFolders.CommonFiles: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('CommonFilesDir')
  );
end;

class function TPJSystemFolders.CommonFilesRedirect: string;
begin
  Result := GetEnvVar('COMMONPROGRAMFILES');
end;

class function TPJSystemFolders.CommonFilesX86: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('CommonFilesDir (x86)')
  );
end;

class function TPJSystemFolders.ProgramFiles: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('ProgramFilesDir')
  );
end;

class function TPJSystemFolders.ProgramFilesRedirect: string;
begin
  Result := GetEnvVar('PROGRAMFILES');
end;

class function TPJSystemFolders.ProgramFilesX86: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('ProgramFilesDir (x86)')
  );
end;

class function TPJSystemFolders.System: string;
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetSystemDirectory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder)
  else
    Result := '';
end;

class function TPJSystemFolders.SystemWow64: string;
type
  // type of GetSystemWow64DirectoryFn API function
  TGetSystemWow64Directory = function(lpBuffer: PChar; uSize: UINT): UINT;
    stdcall;
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
  GetSystemWow64Directory: TGetSystemWow64Directory;  // API function
begin
  Result := '';
  {$IFDEF UNICODE}
  GetSystemWow64Directory := LoadKernelFunc('GetSystemWow64DirectoryW');
  {$ELSE}
  GetSystemWow64Directory := LoadKernelFunc('GetSystemWow64DirectoryA');
  {$ENDIF}
  if not Assigned(GetSystemWow64Directory) then
    Exit;
  if GetSystemWow64Directory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder);
end;

class function TPJSystemFolders.Temp: string;
var
  PathBuf: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetTempPath(MAX_PATH, PathBuf) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PathBuf)
  else
    Result := '';
end;

class function TPJSystemFolders.Windows: string;
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetWindowsDirectory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder)
  else
    Result := '';
end;

initialization

// Initialize global variables from extended OS and product info
InitPlatformIdEx;

end.

