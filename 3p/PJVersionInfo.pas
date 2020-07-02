{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev: 1515 $
 * $Date: 2014-01-11 02:36:28 +0000 (Sat, 11 Jan 2014) $
 *
 * Version Information Component. The component reads version information from
 * executable files.
}


unit PJVersionInfo;

// Determine if certain features are supported by compiler
// * Supports_Assert          - Defined if assertions supported (all compilers
//                              except Delphi 2).
// * Supports_ResourceString  - Defined if resourcestring keyword supported (all
//                              compilers except Delphi 2).
// * Supports_AdvancedRecords - Defined if advanced records with record methods,
//                              operator overloads etc. supported (Delphi 2006
//                              and later).
// * Supports_RTLNameSpaces   - Defined if Delphi RTL / VCL unit references
//                              should be qualified with namespaces.
{$DEFINE Supports_Assert}
{$DEFINE Supports_ResourceString}
{$UNDEF Supports_AdvancedRecords}
{$UNDEF Supports_RTLNameSpaces}
  {$UNDEF Supports_Assert}
  {$UNDEF Supports_ResourceString}
// Switch off unsafe code warnings if switch supported
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE Supports_RTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 18.0} // Delphi 2006 and later
    {$DEFINE Supports_AdvancedRecords}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}

interface


uses
  // Delphi
  {$IFDEF Supports_RTLNameSpaces}
  Winapi.Windows, System.Classes;
  {$ELSE}
  Windows, Classes;
  {$ENDIF}


type

  {
  TPJVersionNumber:
    Record that encapsulates version numbers.
  }
  TPJVersionNumber = record
    V1: Word;   // Major version number
    V2: Word;   // Minor version number
    V3: Word;   // Revision version number
    V4: Word;   // Build number
    {$IFDEF Supports_AdvancedRecords}
    class operator Implicit(Ver: TPJVersionNumber): string;
      {Operator overload that performs implicit conversion of TPJVersionNumber
      to string as dotted quad.
        @param Ver [in] Version number to be converted.
        @return Version number as dotted quad.
      }
    class operator LessThanOrEqual(Ver1, Ver2: TPJVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      less than or equal to the second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 <= Ver2, False otherwise.
      }
    class operator LessThan(Ver1, Ver2: TPJVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      less than second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 < Ver2, False otherwise.
      }
    class operator GreaterThan(Ver1, Ver2: TPJVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      greater than second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 > Ver2, False otherwise.
      }
    class operator GreaterThanOrEqual(Ver1, Ver2: TPJVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check if first is
      greater than or equal to the second.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 >= Ver2, False otherwise.
      }
    class operator Equal(Ver1, Ver2: TPJVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check for
      equality.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 = Ver2, False otherwise.
      }
    class operator NotEqual(Ver1, Ver2: TPJVersionNumber): Boolean;
      {Operator overload that compares two version numbers to check for
      inequality.
        @param Ver1 [in] First version number.
        @param Ver2 [in] Second version number.
        @return True if Ver1 <> Ver2, False otherwise.
      }
    {$ENDIF}
  end;

  {
  TPJVersionInfo:
    Component that accesses the version information embedded in an executable
    file and exposes the information as properties. Supports multi-lingual
    version iformation resources.
  }
  TPJVersionInfo = class(TComponent)
  private // properties
    fFileName: string;
    fHaveInfo: Boolean;
    fNumTranslations: Integer;
    fCurrentTranslation: Integer;
    fFixedFileInfo: TVSFixedFileInfo;
    procedure SetFileName(AName: string);
    function GetProductVersionNumber: TPJVersionNumber;
    function GetFileVersionNumber: TPJVersionNumber;
    function GetLanguage: string;
    function GetCharSet: string;
    function GetCharSetCode: WORD;
    function GetLanguageCode: WORD;
    function GetCurrentTranslation: Integer;
    procedure SetCurrentTranslation(const Value: Integer);
    function GetStringFileInfo(const Name: string): string;
    function GetStringFileInfoByIdx(Index: Integer): string;
    function GetFixedFileInfoItemByIdx(Index: Integer): DWORD;
  private
    fPInfoBuffer: PChar;      // Pointer to info buffer
    fPTransBuffer: Pointer;   // Pointer to translation buffer
    procedure GetInfoBuffer(Len: DWORD);
      {Creates an info buffer of required size.
        @param Len [in] Required buffer size in characters.
      }
    procedure GetTransBuffer(Len: UINT);
      {Creates a translation table buffer of required size.
        @param Required buffer size in bytes.
      }
    function GetTransStr: string;
      {Encodes information about the current translation in a string.
        @return Required translation information.
      }
  protected
    procedure ClearProperties; virtual;
      {Forces properties to return cleared values.
      }
    procedure ReadVersionInfo; virtual;
      {Reads version info from file named by FileName property.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Sets default values.
        @param AOwner [in] Component that owns this one. May be nil.
      }
    destructor Destroy; override;
      {Object destructor. Frees allocated memory.
      }
    property HaveInfo: Boolean
      read fHaveInfo;
      {Property true if file version info for the file named by the FileName
      property has been successfully read}
    property FixedFileInfo: TVSFixedFileInfo
      read fFixedFileInfo;
      {Exposes the whole fixed file info record. Following properties expose
      the various fields of it}
    property FileVersionNumber: TPJVersionNumber
      read GetFileVersionNumber;
      {Version number of file in numeric format. From fixed file info}
    property ProductVersionNumber: TPJVersionNumber
      read GetProductVersionNumber;
      {Version number of product in numeric format. From fixed file info}
    property FileOS: DWORD  index 0
      read GetFixedFileInfoItemByIdx;
      {Code describing operating system to be used by file, From fixed file
      info}
    property FileType: DWORD index 1
      read GetFixedFileInfoItemByIdx;
      {Code descibing type of file. From fixed file info}
    property FileSubType: DWORD index 2
      read GetFixedFileInfoItemByIdx;
      {Code describing sub-type of file - only used for certain values of
      FileType property. From fixed file info}
    property FileFlagsMask: DWORD index 3
      read GetFixedFileInfoItemByIdx;
      {Code describing which FileFlags are valid. From fixed file info}
    property FileFlags: DWORD index 4
      read GetFixedFileInfoItemByIdx;
      {Flags describing file state. From fixed file info}
    property Comments: string index 0
      read GetStringFileInfoByIdx;
      {String file info property giving user defined comments in current
      translation}
    property CompanyName: string index 1
      read GetStringFileInfoByIdx;
      {String file info property giving name of company in current translation}
    property FileDescription: string index 2
      read GetStringFileInfoByIdx;
      {String file info property giving description of file in current
      translation}
    property FileVersion: string index 3
      read GetStringFileInfoByIdx;
      {String file info property giving version number of file in string format
      in current translation}
    property InternalName: string index 4
      read GetStringFileInfoByIdx;
      {String file info property giving internal name of file in current
      translation}
    property LegalCopyright: string index 5
      read GetStringFileInfoByIdx;
      {String file info property giving copyright message in current
      translation}
    property LegalTrademarks: string index 6
      read GetStringFileInfoByIdx;
      {String file info property giving trademark info in current translation}
    property OriginalFileName: string index 7
      read GetStringFileInfoByIdx;
      {String file info property giving original name of file in current
      translation}
    property PrivateBuild: string index 8
      read GetStringFileInfoByIdx;
      {String file info property giving information about a private build of
      file in current translation}
    property ProductName: string index 9
      read GetStringFileInfoByIdx;
      {String file info property giving name of product in current translation}
    property ProductVersion: string index 10
      read GetStringFileInfoByIdx;
      {String file info property giving version number of product in string
      format in current translation}
    property SpecialBuild: string index 11
      read GetStringFileInfoByIdx;
      {String file info property giving information about a special build of
      file in current translation}
    property StringFileInfo[const Name: string]: string
      read GetStringFileInfo;
      {Value of named string file info item in current translation. This
      property can access both standard and custom string info}
    property Language: string
      read GetLanguage;
      {Name of language in use in current translation}
    property CharSet: string
      read GetCharSet;
      {Name of character set in use in current translation}
    property LanguageCode: WORD
      read GetLanguageCode;
      {Code of laguage in use in current translation}
    property CharSetCode: WORD
      read GetCharSetCode;
      {Code of character set in use in current translation}
    property NumTranslations: Integer
      read fNumTranslations;
      {The number of difference translations (ie languages and char sets) in
      the version information}
    property CurrentTranslation: Integer
      read GetCurrentTranslation write SetCurrentTranslation;
      {Zero-based index of the current translation: this is 0 when a file is
      first accessed. Set to a value in range 0..NumTranslations-1 to access
      other translations. All string info, language and char set properties
      return information for the current translation}
  published
    property FileName: string read fFileName write SetFileName;
      {Name of file containing version information. If set to '' (default) the
      version information comes from the containing executable file}
  end;

function VerNumToStr(const Ver: TPJVersionNumber): string;
  {Converts a version number to its string representation as a dotted quad.
    @param Ver [in] Version number to be converted.
    @return Version number as dotted quad.
  }

function CompareVerNums(const Ver1, Ver2: TPJVersionNumber): Integer;
  {Compares two version numbers and returns a value indicating if the first is
  less than, equal to or greater than the second.
    @param Ver1 [in] First version number to compare.
    @param Ver2 [in] Second version number to compare.
    @return 0 if Ver1 = Ver2, -ve if Ver1 < Ver2, +ve if Ver1 > Ver2.
  }

procedure Register;
  {Registers this component.
  }


implementation


uses
  {$IFDEF Supports_RTLNameSpaces}
  System.SysUtils;
  {$ELSE}
  // Delphi
  SysUtils;
  {$ENDIF}


procedure Register;
  {Registers this component.
  }
begin
  RegisterComponents('DelphiDabbler', [TPJVersionInfo]);
end;

type
  // ANSI version of CPINFOEX: provides information about a code page
  _cpinfoexA = packed record
    MaxCharSize: UINT;
      {max length in bytes of a character in the code page}
    DefaultChar: array[0..MAX_DEFAULTCHAR-1] of Byte;
      {default character used to translate strings to the specific code page}
    LeadByte: array[0..MAX_LEADBYTES-1] of Byte;
      {fixed-length array of lead byte ranges: all elements null if none}
    UnicodeDefaultChar: WideChar;
      {unicode default char used in translations from the specific code page}
    CodePage: UINT;
      {code page value}
    CodePageName: array[0..MAX_PATH-1] of AnsiChar;
      {full localised name of the code page}
  end;
  CPINFOEXA = _cpinfoexA;
  PCPInfoExA = ^CPINFOEXA;
  TCPInfoExA = CPINFOEXA;

  // Unicode version of CPINFOEX: provides information about a code page
  _cpinfoexW = packed record
    MaxCharSize: UINT;
      {max length in bytes of a character in the code page}
    DefaultChar: array[0..MAX_DEFAULTCHAR-1] of Byte;
      {default character used to translate strings to the specific code page}
    LeadByte: array[0..MAX_LEADBYTES-1] of Byte;
      {fixed-length array of lead byte ranges: all elements null if none}
    UnicodeDefaultChar: WideChar;
      {unicode default char used in translations from the specific code page}
    CodePage: UINT;
      {code page value}
    CodePageName: array[0..MAX_PATH-1] of WideChar;
      {full localised name of the code page}
  end;
  CPINFOEXW = _cpinfoexW;
  PCPInfoExW = ^CPINFOEXW;
  TCPInfoExW = CPINFOEXW;

  // Set TCPInfoEx etc to required ANSI or Unicode version of structure
  {$IFDEF UNICODE}
  TCPInfoEx = TCPInfoExW;
  PCPInfoEx = PCPInfoExW;
  {$ELSE}
  TCPInfoEx = TCPInfoExA;
  PCPInfoEx = PCPInfoExA;
  {$ENDIF}
  CPINFOEX = TCPInfoEx;

var
  // Pointer to Windows API GetCPInfoEx function if it exists or to GetCPInfoAlt
  // otherwise
  GetCPInfoExFn: function (CodePage: UINT; dwFlags: DWORD;
    var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;

const
  // Import name of GetCPInfoEx. Unicode and ANSI versions.
  {$IFDEF UNICODE}
  cGetCPInfoEx = 'GetCPInfoExW';
  {$ELSE}
  cGetCPInfoEx = 'GetCPInfoExA';
  {$ENDIF}

function GetCPInfoAlt(CodePage: UINT; dwFlags: DWORD;
  var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;
  {Local implementation of GetCPInfoEx, for use on OSs that don't support
  GetCPInfoEx. Calls older GetCPInfo API function and calculates members of
  TCPInfoEx not provided by GetCPInfo.
    @param CodePage [in] Code page for which information is required.
    @param dwFlags [in] Reserved. Must be 0.
    @param lpCPInfoEx [in/out] Structure that receives information about the
      code page.
    @return True on success, False on error.
  }
  // ---------------------------------------------------------------------------
  procedure CopyByteArray(const Src: array of Byte; var Dest: array of Byte);
    {Makes a copy of a byte array.
      @param Src [in] Byte array to be copied.
      @param Dest [in/out] In: Array to receive copy: must be same size as Src.
        Out: Receives copy of Src.
    }
  var
    Idx: Integer; // loops thru array
  begin
    {$IFDEF Supports_Assert}
    Assert((Low(Src) = Low(Dest)) and (High(Src) = High(Dest)));
    {$ENDIF}
    for Idx := Low(Src) to High(Src) do
      Dest[Idx] := Src[Idx];
  end;
  // ---------------------------------------------------------------------------
{$IFDEF Supports_ResourceString}
resourcestring
{$ELSE}
const
{$ENDIF}
  sCodePage = 'Code Page %d'; // description of code page if OS doesn't provide
var
  OldInfo: TCPInfo; // old style code page info structure for Win95/NT4
begin
  // We haven't got GetCPInfoEx: use old GetCPInfo to get some info
  Result := GetCPInfo(CodePage, OldInfo);
  if Result then
  begin
    // We update TCPInfoEx structure from old style structure and calculate
    // additional info
    // copy over from old style TCPInfo structure
    lpCPInfoEx.MaxCharSize := OldInfo.MaxCharSize;
    CopyByteArray(OldInfo.DefaultChar, lpCPInfoEx.DefaultChar);
    CopyByteArray(OldInfo.LeadByte, lpCPInfoEx.LeadByte);
    // no new default char
    lpCPInfoEx.UnicodeDefaultChar := #0;
    // store reference to code page
    lpCPInfoEx.CodePage := CodePage;
    // description is simply "Code Page NNN"
    StrPLCopy(
      lpCPInfoEx.CodePageName,
      Format(sCodePage, [CodePage]),
      SizeOf(lpCPInfoEx.CodePageName)
    );
  end;
end;

function VerNumToStr(const Ver: TPJVersionNumber): string;
  {Converts a version number to its string representation as a dotted quad.
    @param Ver [in] Version number to be converted.
    @return Version number as dotted quad.
  }
begin
  Result := Format('%d.%d.%d.%d', [Ver.V1, Ver.V2, Ver.V3, Ver.V4]);
end;

function CompareVerNums(const Ver1, Ver2: TPJVersionNumber): Integer;
  {Compares two version numbers and returns a value indicating if the first is
  less than, equal to or greater than the second.
    @param Ver1 [in] First version number to compare.
    @param Ver2 [in] Second version number to compare.
    @return 0 if Ver1 = Ver2, -ve if Ver1 < Ver2, +ve if Ver1 > Ver2.
  }
begin
  Result := Ver1.V1 - Ver2.V1;
  if Result <> 0 then
    Exit;
  Result := Ver1.V2 - Ver2.V2;
  if Result <> 0 then
    Exit;
  Result := Ver1.V3 - Ver2.V3;
  if Result <> 0 then
    Exit;
  Result := Ver1.V4 - Ver2.V4;
end;

type
  {
  TTransRec:
    Record of language code and char set codes that are returned from version
    information.
  }
  TTransRec = packed record
    Lang: Word;       // language code
    CharSet: Word;    // character set code
  end;
  {
  TTransRecs:
    Type used to type cast translation data into an array of translation
    records.
  }
  TTransRecs = array[0..1000] of TTransRec;
  {
  PTransRecs:
    Pointer to an array of translation records.
  }
  PTransRecs = ^TTransRecs;


{ TPJVersionInfo }

procedure TPJVersionInfo.ClearProperties;
  {Forces properties to return cleared values.
  }
begin
  // Record that we haven't read ver info: this effectively clears properties
  // since each property read access method checks this flag before returning
  // result
  fHaveInfo := False;
end;

constructor TPJVersionInfo.Create(AOwner: TComponent);
  {Object constructor. Sets default values.
    @param AOwner [in] Component that owns this one. May be nil.
  }
begin
  inherited Create(AOwner);
  // Default is no file name - refers to executable file for application
  FileName := '';
end;

destructor TPJVersionInfo.Destroy;
  {Object destructor. Frees allocated memory.
  }
begin
  // Ensure that info buffer is freed if allocated
  if fPInfoBuffer <> nil then
    StrDispose(fPInfoBuffer);
  // Ensure that translation buffer is free if allocated
  if fPTransBuffer <> nil then
    FreeMem(fPTransBuffer);
  inherited Destroy;
end;

function TPJVersionInfo.GetCharSet: string;
  {Read accessor for CharSet property:
    @return String describing character set if version info is available or
      empty string if not.
  }
var
  Info: TCPInfoEx;  // receives code page info
  CP: Word;         // code page
{$IFDEF Supports_ResourceString}
resourcestring
{$ELSE}
const
{$ENDIF}
  // Special code page messages
  sUnknownCP = '%d (Unknown Code Page)';  // unknown
  // Messages for pages API can't return (managed apps only)
  sUTF16LE  = '%d (Unicode UTF-16, little endian byte order)';
  sUTF16BE  = '%d (Unicode UTF-16, big endian byte order)';
  sUTF32LE  = '%d (Unicode UTF-32, little endian byte order)';
  sUTF32BE  = '%d (Unicode UTF-32, big endian byte order)';
begin
  Result := '';
  if fHaveInfo then
  begin
    CP := GetCharSetCode;
    case CP of
      // Check for char codes only available in managed apps (API call won't
      // find them)
      1200:   Result := Format(sUTF16LE, [CP]);
      1201:   Result := Format(sUTF16BE, [CP]);
      12000:  Result := Format(sUTF32LE, [CP]);
      12001:  Result := Format(sUTF32BE, [CP]);
      else
      begin
        // Not a known problem code page: get it from OS
        if GetCPInfoExFn(CP, 0, Info) then
          Result := Info.CodePageName
        else
          // Give up: can't find it
          Result := Format(sUnknownCP, [CP]);
      end;
    end;
  end;
end;

function TPJVersionInfo.GetCharSetCode: WORD;
  {Read accessor for CharSetCode property.
    @return Char set code for current translation or 0 if there is no
      translation or there is no version info.
  }
begin
  if fHaveInfo and (GetCurrentTranslation >= 0) then
    Result := PTransRecs(fPTransBuffer)^[GetCurrentTranslation].CharSet
  else
    Result := 0;
end;

function TPJVersionInfo.GetCurrentTranslation: Integer;
  {Read accessor for CurrentTranslation property.
    @return Index to current translation if version info is available or -1 if
      not.
  }
begin
  if fHaveInfo then
    Result := fCurrentTranslation
  else
    Result := -1;
end;

function TPJVersionInfo.GetFileVersionNumber: TPJVersionNumber;
  {Read accessor for FileVersionNumber property.
    @return Record containing version information. If there is no version info
      then all fields will be zero.
  }
begin
  Result.V1 := HiWord(fFixedFileInfo.dwFileVersionMS);
  Result.V2 := LoWord(fFixedFileInfo.dwFileVersionMS);
  Result.V3 := HiWord(fFixedFileInfo.dwFileVersionLS);
  Result.V4 := LoWord(fFixedFileInfo.dwFileVersionLS);
end;

function TPJVersionInfo.GetFixedFileInfoItemByIdx(Index: Integer): DWORD;
  {Read accessor method for various DWORD fields of the fixed file information
  record accessed by index.
  NOTE: This is a fix for C++ Builder. Delphi is able to access the fields of
  the TVSFixedFileInfo record directly in the read clause of the property
  declaration but this is not possible in C++ Builder.
    @param Index [in] Index of required property.
    @return Required DWORD value.
  }
begin
  case Index of
    0:  Result := fFixedFileInfo.dwFileOS;
    1:  Result := fFixedFileInfo.dwFileType;
    2:  Result := fFixedFileInfo.dwFileSubType;
    3:  Result := fFixedFileInfo.dwFileFlagsMask;
    4:  Result := fFixedFileInfo.dwFileFlags;
    else Result := 0;
  end;
end;

procedure TPJVersionInfo.GetInfoBuffer(Len: DWORD);
  {Creates an info buffer of required size.
    @param Len [in] Required buffer size in characters.
  }
begin
  // Clear any existing buffer
  if fPInfoBuffer <> nil then
    StrDispose(fPInfoBuffer);
  // Create the new one
  fPInfoBuffer := StrAlloc(Len);
end;

function TPJVersionInfo.GetLanguage: string;
  {Read accessor for Language property
    @return String describing language or empty string if no version info
      available.
  }
const
  cBufSize = 256;   // size of buffer
var
  Buf: array[0..Pred(cBufSize)] of Char; // stores langauge string from API call
begin
  // Assume failure
  Result := '';
  // Try to get language name from Win API if we have ver info
  if fHaveInfo and
    (VerLanguageName(GetLanguageCode, Buf, Pred(cBufSize)) > 0) then
    Result := Buf;
end;

function TPJVersionInfo.GetLanguageCode: WORD;
  {Read accessor for LanguageCode property
    @return Language code for current translation or 0 if there is no
      translation or there is no version info.
  }
begin
  if fHaveInfo and (GetCurrentTranslation >= 0) then
    Result := PTransRecs(fPTransBuffer)^[GetCurrentTranslation].Lang
  else
    Result := 0;
end;

function TPJVersionInfo.GetProductVersionNumber: TPJVersionNumber;
  {Read accessor for ProductVersionNumber property.
    @return Record containing version information. If there is no version info
      then all fields will be zero.
  }
begin
  Result.V1 := HiWord(fFixedFileInfo.dwProductVersionMS);
  Result.V2 := LoWord(fFixedFileInfo.dwProductVersionMS);
  Result.V3 := HiWord(fFixedFileInfo.dwProductVersionLS);
  Result.V4 := LoWord(fFixedFileInfo.dwProductVersionLS);
end;

function TPJVersionInfo.GetStringFileInfo(const Name: string): string;
  {Read accessor for StringFileInfo array property.
    @param Name [in] Name of required string information.
    @return String associated Name or empty string if there is no version info.
  }
var
  CommandBuf: array[0..255] of char;  // buffer to build API call command str
  Ptr: Pointer;                       // pointer to result of API call
  Len: UINT;                          // length of structure returned from API
begin
  // Set default failure result to empty string
  Result := '';
  // Check if we have valid information recorded in info buffer - exit if not
  if fHaveInfo then
  begin
    // Build API call command string for reading string file info:
    //   this uses info string + language and character set
    StrPCopy(CommandBuf, '\StringFileInfo\' + GetTransStr + '\' + Name);
    // Call API to get required string and return it if successful
    if VerQueryValue(fPInfoBuffer, CommandBuf, Ptr, Len) then
      Result := PChar(Ptr);
  end;
end;

function TPJVersionInfo.GetStringFileInfoByIdx(Index: Integer): string;
  {Read accessor for all string file info properties.
    @param Index [in] Index of required property.
    @return Appropriate string value of the indexed property or empty string if
      property has no value or there is no version info.
  }
const
  cNames: array[0..11] of string =
    ('Comments', 'CompanyName', 'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFileName',
    'PrivateBuild', 'ProductName', 'ProductVersion', 'SpecialBuild');
    {names of predefined string file info strings}
begin
  Result := GetStringFileInfo(cNames[Index]);
end;

procedure TPJVersionInfo.GetTransBuffer(Len: UINT);
  {Creates a translation table buffer of required size.
    @param Required buffer size in bytes.
  }
begin
  // Clear any existing buffer
  if fPTransBuffer <> nil then
    FreeMem(fPTransBuffer);
  // Create the new one
  GetMem(fPTransBuffer, Len);
end;

function TPJVersionInfo.GetTransStr: string;
  {Encodes information about the current translation in a string.
    @return Required translation information.
  }
var
  TransRec: TTransRec;  // translation record in array of translations
begin
  if GetCurrentTranslation >= 0 then
  begin
    // There is a valid current translation: return hex string related to it
    TransRec := PTransRecs(fPTransBuffer)^[GetCurrentTranslation];
    Result := Format('%4.4x%4.4x', [TransRec.Lang, TransRec.CharSet]);
  end
  else
    // No valid translation string: return empty string
    Result := '';
end;

procedure TPJVersionInfo.ReadVersionInfo;
  {Reads version info from file named by FileName property.
  }
var
  Len: UINT;        // length of structs returned from API calls
  Ptr: Pointer;     // points to version info structures
  InfoSize: DWORD;  // size of info buffer
  Dummy: DWORD;     // stores 0 in call to GetFileVersionInfoSize
begin
  // Record default value of HaveInfo property - no info read
  fHaveInfo := False;
  // Store zeros in fixed file info structure: this is used when no info
  FillChar(fFixedFileInfo, SizeOf(fFixedFileInfo), 0);
  // Set NumTranslations property to 0: this is value if no info
  fNumTranslations := 0;
  // Record required size of version info buffer
  InfoSize := GetFileVersionInfoSize(PChar(fFileName), Dummy);
  // Check that there was no error
  if InfoSize > 0 then
  begin
    // Found info size OK
    // Ensure we have a sufficiently large buffer allocated
    GetInfoBuffer(InfoSize);
    // Read file version info into storage and check success
    if GetFileVersionInfo(PChar(fFileName), Dummy, InfoSize, fPInfoBuffer) then
    begin
      // Success: we've read file version info to storage OK
      fHaveInfo := True;
      // Get fixed file info & copy to own storage
      VerQueryValue(fPInfoBuffer, '\', Ptr, Len);
      fFixedFileInfo := PVSFixedFileInfo(Ptr)^;
      // Get first translation table info from API
      VerQueryValue(fPInfoBuffer, '\VarFileInfo\Translation', Ptr, Len);
      // Ptr is to block of translation records each of size Len:
      // work out number of translations
      fNumTranslations := Len div SizeOf(TTransRec);
      // store translation array in a buffer
      GetTransBuffer(Len);
      Move(Ptr^, fPTransBuffer^, Len);
      // make first translation in block current one (-1 if no translations)
      SetCurrentTranslation(0);   // adjusts value to -1 if no translations
    end;
  end;
end;

procedure TPJVersionInfo.SetCurrentTranslation(const Value: Integer);
  {Write acceesor method CurrentTranslation property
    @param Index of required translation. If Value is out of range then the
      property is set to -1 to indicate no translation.
  }
begin
  if (Value >= 0) and (Value < NumTranslations) then
    fCurrentTranslation := Value
  else
    fCurrentTranslation := -1
end;

procedure TPJVersionInfo.SetFileName(AName: string);
  {Write accessor for FileName property. Action at design time and run time is
  different. At design time we simply record the property value while at run
  time we store the value and read any version information from the file.
    @param AName [in] New value of FileName property. If '' then property is set
      to the name of the program's executable file.
  }
begin
  if csDesigning in ComponentState then
    // We are designing, simply record the required name
    fFileName := AName
  else
  begin
    // It's run-time
    // use Application exec file name if name is ''
    if AName = '' then
      fFileName := ParamStr(0)
    else
      fFileName := AName;
    // clear all properties and read file version info for new file
    ClearProperties;
    ReadVersionInfo;
  end;
end;

{$IFDEF Supports_AdvancedRecords}

{ TPJVersionNumber }

class operator TPJVersionNumber.Equal(Ver1, Ver2: TPJVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check for equality.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 = Ver2, False otherwise.
  }
begin
  Result := CompareVerNums(Ver1, Ver2) = 0;
end;

class operator TPJVersionNumber.GreaterThan(Ver1,
  Ver2: TPJVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  greater than second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 > Ver2, False otherwise.
  }
begin
  Result := CompareVerNums(Ver1, Ver2) > 0;
end;

class operator TPJVersionNumber.GreaterThanOrEqual(Ver1,
  Ver2: TPJVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is
  greater than or equal to the second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 >= Ver2, False otherwise.
  }
begin
  Result := CompareVerNums(Ver1, Ver2) >= 0;
end;

class operator TPJVersionNumber.Implicit(Ver: TPJVersionNumber): string;
  {Operator overload that performs implicit conversion of TPJVersionNumber to
  string as dotted quad.
    @param Ver [in] Version number to be converted.
    @return Version number as dotted quad.
  }
begin
  Result := VerNumToStr(Ver);
end;

class operator TPJVersionNumber.LessThan(Ver1, Ver2: TPJVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is less
  than second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 < Ver2, False otherwise.
  }
begin
  Result := CompareVerNums(Ver1, Ver2) < 0;
end;

class operator TPJVersionNumber.LessThanOrEqual(Ver1,
  Ver2: TPJVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check if first is less
  than or equal to the second.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 <= Ver2, False otherwise.
  }
begin
  Result := CompareVerNums(Ver1, Ver2) <= 0;
end;

class operator TPJVersionNumber.NotEqual(Ver1, Ver2: TPJVersionNumber): Boolean;
  {Operator overload that compares two version numbers to check for inequality.
    @param Ver1 [in] First version number.
    @param Ver2 [in] Second version number.
    @return True if Ver1 <> Ver2, False otherwise.
  }
begin
  Result := CompareVerNums(Ver1, Ver2) <> 0;
end;

{$ENDIF}


initialization

// Get reference to GetCPInfoEx function
GetCPInfoExFn := GetProcAddress(GetModuleHandle('Kernel32.dll'), cGetCPInfoEx);
if not Assigned(GetCPInfoExFn) then
  GetCPInfoExFn := GetCPInfoAlt;

end.

