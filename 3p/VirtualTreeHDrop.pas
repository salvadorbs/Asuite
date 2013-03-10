{
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

Alternatively, you may redistribute this library, use and/or modify it under the terms of the
GNU Lesser General Public License as published by the Free Software Foundation;
either version 2.1 of the License, or (at your option) any later version.
You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.

The Original Code is MPDataObject 2.0.0

The initial developer of this code is Jim Kueneman <jimdk@mindspring.com>
Special thanks to the following in no particular order for their help/support/code
   Danijel Malik, Robert Lee, Werner Lehmann, Alexey Torgashin, Milan Vandrovec
}

unit VirtualTreeHDrop;

interface

uses
  Windows, SysUtils, Classes, ActiveX, ShlObj, win32int, InterfaceBase;

const
  CFSTR_LOGICALPERFORMEDDROPEFFECT = 'Logical Performed DropEffect';
  CFSTR_PREFERREDDROPEFFECT = 'Preferred DropEffect';
  CFSTR_PERFORMEDDROPEFFECT = 'Performed DropEffect';
  CFSTR_PASTESUCCEEDED = 'Paste Succeeded';
  CFSTR_INDRAGLOOP = 'InShellDragLoop';
  CFSTR_SHELLIDLISTOFFSET = 'Shell Object Offsets';

type
  TClipboardFormat = class
  public
    function GetFormatEtc: TFormatEtc; virtual;
    function LoadFromClipboard: Boolean; virtual;
    function LoadFromDataObject(DataObject: IDataObject): Boolean; virtual; abstract;
    function SaveToClipboard: Boolean; virtual;
    function SaveToDataObject(DataObject: IDataObject): Boolean; virtual; abstract;
  end;
   
// Simpifies dealing with the CF_HDROP format
  THDrop = class(TClipboardFormat)
  private
    procedure SetDropFiles(const Value: PDropFiles);
    function GetHDropStruct: THandle;
  protected
    FDropFiles: PDropFiles;
    FStructureSize: integer;
    FFileCount: integer;

    procedure AllocStructure(Size: integer);
    function CalculateDropFileStructureSizeA(Value: PDropFiles): integer;
    function CalculateDropFileStructureSizeW(Value: PDropFiles): integer;
    function FileCountA: Integer;
    function FileCountW: Integer;
    function FileNameA(Index: integer): string;
    function FileNameW(Index: integer): WideString;
    procedure FreeStructure; // Frees memory allocated
  public
    destructor Destroy; override;
    function AssignFromClipboard: Boolean;
    function LoadFromClipboard: Boolean; override;
    function LoadFromDataObject(DataObject: IDataObject): Boolean; override;
    function FileCount: integer;
    function FileName(Index: integer): WideString;
    function GetFormatEtc: TFormatEtc; override;
    procedure AssignFilesA(FileList: TStringList);
    {$IFDEF TNTSUPPORT}
    procedure AssignFilesW(FileList: TWideStrings);
    procedure FileNamesW(FileList: TWideStrings);
    {$ENDIF}
    procedure FileNamesA(FileList: TStrings);

    property HDropStruct: THandle read GetHDropStruct;
    function SaveToClipboard: Boolean; override;
    function SaveToDataObject(DataObject: IDataObject): Boolean; override;
    property StructureSize: integer read FStructureSize;
    property DropFiles: PDropFiles read FDropFiles write SetDropFiles;
  end;

{ TODO : Check this code - Useless? *Lazarus Porting* }
//var
//  CF_SHELLIDLIST,
//  CF_LOGICALPERFORMEDDROPEFFECT,
//  CF_PREFERREDDROPEFFECT,
//  CF_FILECONTENTS,
//  CF_FILEDESCRIPTORA,
//  CF_FILEDESCRIPTORW: TClipFormat;

implementation

{ TClipboardFormat }

function TClipboardFormat.GetFormatEtc: TFormatEtc;
begin
  FillChar(Result, SizeOf(Result), #0);
end;

function TClipboardFormat.LoadFromClipboard: Boolean;
begin
  Result := False;
end;

function TClipboardFormat.SaveToClipboard: Boolean;
begin
  Result := False;
end;


{ THDrop }

procedure THDrop.AllocStructure(Size: integer);
begin
  FreeStructure;
  GetMem(FDropFiles, Size);
  FStructureSize := Size;
  FillChar(FDropFiles^, Size, #0);
end;

procedure THDrop.AssignFilesA(FileList: TStringList);
var
  i: Integer;
  Size: integer;
  Path: PAnsiChar;
begin
  if Assigned(FileList) then
  begin
    FreeStructure;
    Size := 0;
    for i := 0 to FileList.Count - 1 do
      Inc(Size, Length(FileList[i]) + SizeOf(Char)); // add spot for the null
    Inc(Size, SizeOf(TDropFiles));
    Inc(Size, SizeOf(Char)); // room for the terminating null
    AllocStructure(Size);
    DropFiles.pFiles := SizeOf(TDropFiles);
    DropFiles.pt.x := 0;
    DropFiles.pt.y := 0;
    DropFiles.fNC := False;
    DropFiles.fWide := False;  // Don't support wide char let NT convert it
    Path := PAnsiChar(FDropFiles) + FDropFiles.pFiles;
    for i := 0 to FileList.Count - 1 do
    begin
      MoveMemory(Path, Pointer(FileList[i]), Length(FileList[i]));
      Inc(Path, Length(FileList[i]) + 1); // skip over the single null #0
    end
  end
end;

function THDrop.AssignFromClipboard: Boolean;
var
  Handle: THandle;
  Ptr: PDropFiles;
begin
  Result := False;
  Handle := 0;
  OpenClipboard(TWin32WidgetSet(WidgetSet).AppHandle);
  try
    Handle := GetClipboardData(CF_HDROP);
    if Handle <> 0 then
    begin
      Ptr := GlobalLock(Handle);
      if Assigned(Ptr) then
      begin
        DropFiles := Ptr;
        Result := True;
      end;
    end;
  finally
    CloseClipboard;
    GlobalUnLock(Handle);
  end;
end;

function THDrop.CalculateDropFileStructureSizeA(
  Value: PDropFiles): integer;
var
  Head: PAnsiChar;
  Len: integer;
begin
  if Assigned(Value) then
  begin
    Result := Value^.pFiles;
    Head := PAnsiChar( Value) + Value^.pFiles;
    Len := lstrlenA(Head);
    while Len > 0 do
    begin
      Result := Result + Len + 1;
      Head := Head + Len + 1;
       Len := lstrlenA(Head);
    end;
    Inc(Result, 1); // Add second null
  end else
    Result := 0
end;

function THDrop.CalculateDropFileStructureSizeW(
  Value: PDropFiles): integer;
var
  Head: PAnsiChar;
  Len: integer;
begin
  if Assigned(Value) then
  begin
    Result := Value^.pFiles;
    Head := PAnsiChar( Value) + Value^.pFiles;
    Len := 2 * (lstrlenW(PWideChar( Head)));
    while Len > 0 do
    begin
      Result := Result + Len + 2;
      Head := Head + Len + 2;
       Len := 2 * (lstrlenW(PWideChar( Head)));
    end;
    Inc(Result, 2); // Add second null
  end else
    Result := 0
end;

destructor THDrop.Destroy;
begin
  FreeStructure;
  inherited;
end;

function THDrop.FileCount: integer;
begin
  if Assigned(DropFiles) then
  begin
    if FFileCount = 0 then
    begin
      if DropFiles.fWide then
        Result := FileCountW
      else
        Result := FileCountA;
       FFileCount := Result;
    end;
  end else
   FFileCount := 0;
  Result := FFileCount
end;

function THDrop.FileCountA: Integer;
var
  Head: PAnsiChar;
  Len: integer;
begin
  Result := 0;
  if Assigned(DropFiles) then
  begin
    Head := PAnsiChar( DropFiles) + DropFiles^.pFiles;
    Len := lstrlenA(Head);
    while Len > 0 do
    begin
      Head := Head + Len + 1;
      Inc(Result);
      Len := lstrlenA(Head);
    end
  end
end;

function THDrop.FileCountW: Integer;
var
  Head: PAnsiChar;
  Len: integer;
begin
  Result := 0;
  if Assigned(DropFiles) then
  begin
    Head := PAnsiChar( DropFiles) + DropFiles^.pFiles;
    Len := 2 * (lstrlenW(PWideChar( Head)));
    while Len > 0 do
    begin
      Head := Head + Len + 2;
      Inc(Result);
      Len := 2 * (lstrlenW(PWideChar( Head)));
    end
  end;
end;

function THDrop.FileName(Index: integer): WideString;
begin
  if Assigned(DropFiles) then
  begin
    if DropFiles.fWide then
      Result := FileNameW(Index)
    else
      Result := FileNameA(Index)
  end
end;

function THDrop.FileNameA(Index: integer): string;
var
  Head: PAnsiChar;
  PathNameCount: integer;
  Done: Boolean;
  Len: integer;
begin
  PathNameCount := 0;
  Done := False;
  if Assigned(DropFiles) then
  begin
    Head := PAnsiChar( DropFiles) + DropFiles^.pFiles;
    Len := lstrlenA(Head);
    while (not Done) and (PathNameCount < FileCount) do
    begin
      if PathNameCount = Index then
      begin
        SetLength(Result, Len + 1);
        CopyMemory(@Result[1], Head, Len + 1); // Include the NULL
        Done := True;
      end;
      Head := Head + Len + 1;
      Inc(PathNameCount);
      Len := lstrlenA(Head);
    end
  end
end;

{$IFDEF TNTSUPPORT}
procedure THDrop.AssignFilesW(FileList: TWideStrings);
var
  i: Integer;
  Size: integer;
  Path: PAnsiChar;
  ByteSize: Integer;
begin
  if Assigned(FileList) then
  begin
    FreeStructure;
    Size := 0;
    if UnicodeStringLists then
      ByteSize := 2
    else
      ByteSize := 1;
    for i := 0 to FileList.Count - 1 do
      Inc(Size, (Length(FileList[i])+1)*(SizeOf(Char)*ByteSize)); // add spot for the null
    Inc(Size, SizeOf(TDropFiles));
    Inc(Size, SizeOf(Char)*2); // room for the terminating null
    AllocStructure(Size);
    DropFiles.pFiles := SizeOf(TDropFiles);
    DropFiles.pt.x := 0;
    DropFiles.pt.y := 0;
    DropFiles.fNC := False;
    DropFiles.fWide := UnicodeStringLists;
    Path := PAnsiChar(FDropFiles) + FDropFiles.pFiles;
    for i := 0 to FileList.Count - 1 do
    begin
      MoveMemory(Path, Pointer(FileList[i]), Length(FileList[i])*ByteSize);
      Inc(Path, (Length(FileList[i]) + 1)*ByteSize); // skip over the single null #0
    end
  end
end;

procedure THDrop.FileNamesW(FileList: TWideStrings);
var
  i: integer;
begin
  if Assigned(FileList) then
  begin
    for i := 0 to FileCount - 1 do
      FileList.Add(FileNameW(i));
  end;
end;
{$ENDIF}

procedure THDrop.FileNamesA(FileList: TStrings);
var
  i: integer;
begin
  if Assigned(FileList) then
  begin
    for i := 0 to FileCount - 1 do
      FileList.Add(FileName(i));
  end;
end;

function THDrop.FileNameW(Index: integer): WideString;
var
  Head: PAnsiChar;
  PathNameCount: integer;
  Done: Boolean;
  Len: integer;
begin
  PathNameCount := 0;
  Done := False;
  if Assigned(DropFiles) then
  begin
    Head := PAnsiChar( DropFiles) + DropFiles^.pFiles;
    Len := 2 * (lstrlenW(PWideChar( Head)));
    while (not Done) and (PathNameCount < FileCount) do
    begin
      if PathNameCount = Index then
      begin
        SetLength(Result, (Len + 1) div 2);
        CopyMemory(@Result[1], Head, Len + 2); // Include the NULL
        Done := True;
      end;
      Head := Head + Len + 2;
      Inc(PathNameCount);
      Len := 2 * (lstrlenW(PWideChar( Head)));
    end
  end
end;

procedure THDrop.FreeStructure;
begin
  FFileCount := 0;
  if Assigned(FDropFiles) then
    FreeMem(FDropFiles, FStructureSize);
  FDropFiles := nil;
  FStructureSize := 0;
end;

function THDrop.GetFormatEtc: TFormatEtc;
begin
  Result.cfFormat := CF_HDROP; // This guy is always registered for all applications
  Result.ptd := nil;
  Result.dwAspect := DVASPECT_CONTENT;
  Result.lindex := -1;
  Result.tymed := TYMED_HGLOBAL
end;

function THDrop.GetHDropStruct: THandle;
var
  Files: PDropFiles;
begin
  Result := GlobalAlloc(GHND, StructureSize);
  Files := GlobalLock(Result);
  try
    MoveMemory(Files, FDropFiles, StructureSize);
  finally
    GlobalUnlock(Result)
  end;
end;

function THDrop.LoadFromClipboard: Boolean;
var
  Handle: THandle;
  Ptr: PDropFiles;
begin
  Result := False;
  Handle := 0;
  OpenClipboard(TWin32WidgetSet(WidgetSet).AppHandle);
  try
    Handle := GetClipboardData(CF_HDROP);
    if Handle <> 0 then
    begin
      Ptr := GlobalLock(Handle);
      if Assigned(Ptr) then
      begin
        DropFiles := Ptr;
        Result := True;
      end;
    end;
  finally
    CloseClipboard;
    GlobalUnLock(Handle);
  end;
end;

function THDrop.LoadFromDataObject(DataObject: IDataObject): Boolean;
var
  Medium: TStgMedium;
  Files: PDropFiles;
begin
  Result := False;
  if Assigned(DataObject) then
  begin
    if Succeeded(DataObject.GetData(GetFormatEtc, Medium)) then
    try
      Files := GlobalLock(Medium.hGlobal);
      try
        DropFiles := Files
      finally
        GlobalUnlock(Medium.hGlobal)
      end
    finally
      ReleaseStgMedium(Medium)
    end;
    Result := Assigned(DropFiles)
  end
end;

function THDrop.SaveToClipboard: Boolean;
begin
  Result := False;
  OpenClipboard(TWin32WidgetSet(WidgetSet).AppHandle);
  try
    SetClipboardData(CF_HDROP, HDropStruct)
  finally
    CloseClipboard;
  end;
end;

function THDrop.SaveToDataObject(DataObject: IDataObject): Boolean;
var
  Medium: TStgMedium;
begin
  Result := False;
  FillChar(Medium, SizeOf(Medium), #0);
  Medium.tymed := TYMED_HGLOBAL;
  Medium.hGlobal := HDropStruct;
  // Give the block to the DataObject
  if Succeeded(DataObject.SetData(GetFormatEtc, Medium, True)) then
    Result := True
  else
    GlobalFree(Medium.hGlobal)
end;

procedure THDrop.SetDropFiles(const Value: PDropFiles);
begin
  FreeStructure;
  if Assigned(Value) then
  begin
    if Value.fWide then
      FStructureSize := CalculateDropFileStructureSizeW(Value)
    else
      FStructureSize := CalculateDropFileStructureSizeA(Value);
    AllocStructure(StructureSize);
    CopyMemory(FDropFiles, Value, StructureSize);
  end;
end;

{ TODO : Check this code - Useless? *Lazarus Porting* }
//initialization
//  CF_SHELLIDLIST := RegisterClipboardFormat(CFSTR_SHELLIDLIST);
//  CF_LOGICALPERFORMEDDROPEFFECT := RegisterClipboardFormat(CFSTR_LOGICALPERFORMEDDROPEFFECT);
//  CF_PREFERREDDROPEFFECT := RegisterClipboardFormat(CFSTR_PREFERREDDROPEFFECT);
//  CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
//  CF_FILEDESCRIPTORA := RegisterClipboardFormat(CFSTR_FILEDESCRIPTORA);
//  CF_FILEDESCRIPTORW := RegisterClipboardFormat(CFSTR_FILEDESCRIPTORW);

end.
