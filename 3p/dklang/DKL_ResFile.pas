///*********************************************************************************************************************
///  $Id$
///---------------------------------------------------------------------------------------------------------------------
///  DKLang Localization Package
///  Copyright 2002-2009 DK Software, http://www.dk-soft.org
///*********************************************************************************************************************
///
/// The contents of this package are subject to the Mozilla Public License
/// Version 1.1 (the "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
///
/// Alternatively, you may redistribute this library, use and/or modify it under the
/// terms of the GNU Lesser General Public License as published by the Free Software
/// Foundation; either version 2.1 of the License, or (at your option) any later
/// version. You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/
///
/// Software distributed under the License is distributed on an "AS IS" basis,
/// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
/// specific language governing rights and limitations under the License.
///
/// The initial developer of the original code is Dmitry Kann, http://www.dk-soft.org/
///
/// Upgraded to Delphi 2009 by Bruce J. Miller, rules-of-thumb.com Dec 2008
///
///**********************************************************************************************************************
// Routines and classes to handle .res resource files.
// WARNING: TDKLang_ResFile only handles 32-bit resource files and does not support 16-bit ones!
//
unit DKL_ResFile;

interface
uses Windows, SysUtils, Classes, Contnrs;

type

   //===================================================================================================================
   // Resource entry header
   //===================================================================================================================

  PResResourceEntryHeader = ^TResResourceEntryHeader;
  TResResourceEntryHeader = packed record
    iDataSize:   Integer; // Data size in bytes
    iHeaderSize: Integer; // Header size in bytes
  end;

   //===================================================================================================================
   // Resource entry properties
   //===================================================================================================================

  PResResourceEntryProps = ^TResResourceEntryProps;
  TResResourceEntryProps = packed record
    cDataVersion:     Cardinal;
    wMemoryFlags:     Word;
    wLanguage:        LANGID;
    cVersion:         Cardinal;
    cCharacteristics: Cardinal;
  end;

   //===================================================================================================================
   // .res file handler
   //===================================================================================================================

  TDKLang_ResEntry = class;

  TDKLang_ResFile = class(TObject)
  private
     // Entry list
    FEntries: TObjectList;
     // Loads .res file contents from the stream
    procedure LoadFromStream(Stream: TStream);
     // Saves .res file contents into the stream
    procedure SaveToStream(Stream: TStream);
    function GetEntries(Index: Integer): TDKLang_ResEntry;
    function GetEntryCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
     // Adds an entry and returns its index
    function  AddEntry(Item: TDKLang_ResEntry): Integer;
     // Deletes the entry
    procedure DeleteEntry(Index: Integer);
     // Removes the entry and returns index it had before the deletion
    function  RemoveEntry(Item: TDKLang_ResEntry): Integer;
     // Clears the entry list
    procedure ClearEntries;
     // Loads .res file contents from the file
    procedure LoadFromFile(const wsFileName: UnicodeString);
     // Saves .res file contents into the file
    procedure SaveToFile(const wsFileName: UnicodeString);
     // Tries to find an entry by its type and name. Returns nil if not found
    function  FindEntry(const wsType, wsName: UnicodeString): TDKLang_ResEntry;
     // Props
     // -- Entry count
    property EntryCount: Integer read GetEntryCount;
     // -- Entries by index
    property Entries[Index: Integer]: TDKLang_ResEntry read GetEntries; default;
  end;

   //===================================================================================================================
   // Single resource entry
   //===================================================================================================================

  TDKLang_ResEntry = class(TObject)
  private
     // Prop storage
    FCharacteristics: Cardinal;
    FDataVersion: Cardinal;
    FLanguage: LANGID;
    FMemoryFlags: Word;
    FName: UnicodeString;
    FRawData: RawByteString;
    FResType: UnicodeString;
    FVersion: Cardinal;
  public
     // Stores resource entry into the stream
    procedure SaveToStream(Stream: TStream);
     // Creates and returns an exact copy of the entry
    function  Clone: TDKLang_ResEntry;
     // Props
     // -- Characteristics
    property Characteristics: Cardinal read FCharacteristics write FCharacteristics;
     // -- Data version
    property DataVersion: Cardinal read FDataVersion write FDataVersion;
     // -- Language
    property Language: LANGID read FLanguage write FLanguage;
     // -- Memory flags
    property MemoryFlags: Word read FMemoryFlags write FMemoryFlags;
     // -- Entry name
    property Name: UnicodeString read FName write FName;
     // -- Raw (unparsed) entry data
    property RawData: RawByteString read FRawData write FRawData;
     // -- Entry resource type
    property ResType: UnicodeString read FResType write FResType;
     // -- Version
    property Version: Cardinal read FVersion write FVersion;
  end;

implementation //=======================================================================================================
uses Dialogs;

   //===================================================================================================================
   // TDKLang_ResFile
   //===================================================================================================================

  function TDKLang_ResFile.AddEntry(Item: TDKLang_ResEntry): Integer;
  begin
    Result := FEntries.Add(Item);
  end;

  procedure TDKLang_ResFile.ClearEntries;
  begin
    FEntries.Clear;
  end;

  constructor TDKLang_ResFile.Create;
  begin
    inherited Create;
    FEntries := TObjectList.Create(True);
  end;

  procedure TDKLang_ResFile.DeleteEntry(Index: Integer);
  begin
    FEntries.Delete(Index);
  end;

  destructor TDKLang_ResFile.Destroy;
  begin
    FEntries.Free;
    inherited Destroy;
  end;

  function TDKLang_ResFile.FindEntry(const wsType, wsName: UnicodeString): TDKLang_ResEntry;
  var i: Integer;
  begin
    for i := 0 to EntryCount-1 do begin
      Result := Entries[i];
      if SameText(Result.ResType, wsType) and SameText(Result.Name, wsName) then Exit;
    end;
    Result := nil;
  end;

  function TDKLang_ResFile.GetEntries(Index: Integer): TDKLang_ResEntry;
  begin
    Result := TDKLang_ResEntry(FEntries[Index]);
  end;

  function TDKLang_ResFile.GetEntryCount: Integer;
  begin
    Result := FEntries.Count;
  end;

  procedure TDKLang_ResFile.LoadFromFile(const wsFileName: UnicodeString);
  var Stream: TStream;
  begin
    Stream := TFileStream.Create(wsFileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;

  procedure TDKLang_ResFile.LoadFromStream(Stream: TStream);
  var
    pBuffer, pData: PByte;
    iBufferSize, iBytesLeft, iBlockSize: Integer;
    Header: TResResourceEntryHeader;

     // Retrieves a string or numeric identifier from the data and shifts the pointer appropriately
    function RetrieveIdentifier(var p: PByte): UnicodeString;
    begin
       // Numeric ID
      if PWord(p)^=$ffff then begin
        Inc(p, SizeOf(Word));
        Result := IntToStr(PWord(p)^);
        Inc(p, SizeOf(Word))
       // A  string name
      end else begin
        Result := UnicodeString(PWideChar(p));
        Inc(p, (Length(Result)+1)*SizeOf(WideChar));
      end;
    end;

     // Processes a resource entry
    procedure ProcessResourceEntry;
    var
      p: PByte;
      sRawData: RawByteString;
      wsName, wsType: UnicodeString;
      EntryProps: TResResourceEntryProps;
      Entry: TDKLang_ResEntry;
    begin
      p := pData;
       // Skip the header
      Inc(p, SizeOf(Header));
       // Retrieve resource type and name
      wsType := RetrieveIdentifier(p);
      wsName := RetrieveIdentifier(p);
       // Skip the dummy 32-bit indicator entry
      if (wsType<>'0') or (wsName<>'0') then begin
         // Align the pointer to a 4-byte boundary
        if (Integer(p) mod 4)<>0 then Inc(p, 4-Integer(p) mod 4);
         // Read entry properties
        Move(p^, EntryProps, SizeOf(EntryProps));
         // Create an entry
        Entry := TDKLang_ResEntry.Create;
        try
          Entry.ResType         := wsType;
          Entry.Name            := wsName;
          Entry.DataVersion     := EntryProps.cDataVersion;
          Entry.MemoryFlags     := EntryProps.wMemoryFlags;
          Entry.Language        := EntryProps.wLanguage;
          Entry.Version         := EntryProps.cVersion;
          Entry.Characteristics := EntryProps.cCharacteristics;
          SetString(sRawData, PAnsiChar(Integer(pData)+Header.iHeaderSize), Header.iDataSize);  // surprized SetString works, but it does
          Entry.RawData         := sRawData;

           // Register the entry in the list
          AddEntry(Entry);
        except
          Entry.Free;
          raise;
        end;
      end;
    end;

  begin
     // Clear the entry list
    ClearEntries;
     // Allocate the buffer
    iBufferSize := Stream.Size;
    GetMem (pBuffer, iBufferSize);
    try
       // Read the entire file into the buffer
      Stream.ReadBuffer(pBuffer^, iBufferSize);
       // Scan the buffer
      iBytesLeft := iBufferSize;
      pData := pBuffer;
      while iBytesLeft>=SizeOf(Header) do begin
         // Read the header
        Move(pData^, Header, SizeOf(Header));
         // Process the entry
        ProcessResourceEntry;
         // Shift pointers
        iBlockSize := ((Header.iDataSize+Header.iHeaderSize+3) div 4)*4;
        Inc(pData,      iBlockSize);
        Dec(iBytesLeft, iBlockSize);
      end;
    finally
      FreeMem(pBuffer);
    end;
  end;

  function TDKLang_ResFile.RemoveEntry(Item: TDKLang_ResEntry): Integer;
  begin
    Result := FEntries.Remove(Item);
  end;

  procedure TDKLang_ResFile.SaveToFile(const wsFileName: UnicodeString);
  var Stream: TStream;
  begin
    Stream := TFileStream.Create(wsFileName, fmCreate);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  end;

  procedure TDKLang_ResFile.SaveToStream(Stream: TStream);
  var
    REIndicator: TDKLang_ResEntry;
    i: Integer;
  begin
     // Write dummy 32-bit resource indicator
    REIndicator := TDKLang_ResEntry.Create;
    try
      REIndicator.ResType := '0';
      REIndicator.Name    := '0';
      REIndicator.SaveToStream(Stream);
    finally
      REIndicator.Free;
    end;
     // Write real entries
    for i := 0 to EntryCount-1 do Entries[i].SaveToStream(Stream);
  end;

   //===================================================================================================================
   // TDKLang_ResEntry
   //===================================================================================================================

  function TDKLang_ResEntry.Clone: TDKLang_ResEntry;
  begin
    Result := TDKLang_ResEntry.Create;
    try
      Result.Characteristics := Characteristics;
      Result.DataVersion     := DataVersion;
      Result.Language        := Language;
      Result.MemoryFlags     := MemoryFlags;
      Result.Name            := Name;
      Result.RawData         := RawData;
      Result.ResType         := ResType;
      Result.Version         := Version;
    except
      Result.Free;
      raise;
    end;
  end;

  procedure TDKLang_ResEntry.SaveToStream(Stream: TStream);
  var
    msHeaderBlock: TMemoryStream;
    Header: TResResourceEntryHeader;
    Props: TResResourceEntryProps;

     // Writes a numeric or string identifier into the stream
    procedure WriteIdentifier(const wsID: UnicodeString; Stream: TStream);
    var
      iNumericID: Integer;
      w: Word;
    begin
      iNumericID := StrToIntDef(wsID, -1);
       // string ID
      if iNumericID<0 then
        Stream.WriteBuffer(wsID[1], (Length(wsID)+1)*SizeOf(WideChar))
       // Numeric ID
      else begin
        w := $ffff;
        Stream.WriteBuffer(w, SizeOf(w));
        w := iNumericID;
        Stream.WriteBuffer(w, SizeOf(w));
      end;
    end;

     // Aligns the stream position to a 4-byte boundary 
    procedure AlignStream4(Stream: TStream);
    const IZero: Integer = 0;
    var iMod: Integer;
    begin
      iMod := Stream.Position mod 4;
      if iMod>0 then Stream.WriteBuffer(IZero, 4-iMod);
    end;

  begin
     // Prepare header block
    msHeaderBlock := TMemoryStream.Create;
    try
       // Write type and name identifiers
      WriteIdentifier(ResType, msHeaderBlock);
      WriteIdentifier(Name, msHeaderBlock);
       // Align the stream pointer
      AlignStream4(msHeaderBlock);
       // Fill properties record
      Props.cDataVersion     := DataVersion;
      Props.wMemoryFlags     := MemoryFlags;
      Props.wLanguage        := Language;
      Props.cVersion         := Version;
      Props.cCharacteristics := Characteristics;
       // Write properties
      msHeaderBlock.WriteBuffer(Props, SizeOf(Props));
       // Fill header record
      Header.iDataSize   := ((Length(RawData)+3) div 4)*4;
      Header.iHeaderSize := msHeaderBlock.Size+SizeOf(Header);
       // Put the header record
      Stream.WriteBuffer(Header, SizeOf(Header));
       // Put the header block
      Stream.CopyFrom(msHeaderBlock, 0);
       // Put entry data
      Stream.WriteBuffer(RawData[1], Length(RawData));
       // Align the stream pointer
      AlignStream4(Stream);
    finally
      msHeaderBlock.Free;
    end;
  end;

end.
