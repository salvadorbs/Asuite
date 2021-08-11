{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Thread.FindFiles;

{$MODE delphiunicode}

interface

uses
  Classes, SysUtils, FileUtil, mormot.core.log, StrUtils;

type
  TFileFoundEvent = procedure(AFilePath: String) of Object;
  TSearchEndEvent = procedure(ATotalFiles: Integer; ATime: Cardinal) of Object;

  { TFindFiles }

  TFindFiles = class(TThread)
  private
    FDirectory: String;
    FOnFileFound: TFileFoundEvent;
    FOnSearchEnd: TSearchEndEvent;
    FSearchCriteriaFilename: TStringList;
    FSearchExcludeFilename: TStringList;
    FAbort: Boolean;

    function FindMatchText(Strings: TStrings; const Str: string): Integer;
  protected
    procedure Execute; override;
  public
    Constructor Create;
    destructor Destroy; override;

    procedure Stop;

    property Directory: String read FDirectory write FDirectory;
    property SearchCriteriaFilename: TStringList read FSearchCriteriaFilename write FSearchCriteriaFilename;
    property SearchExcludeFilename: TStringList read FSearchExcludeFilename write FSearchExcludeFilename;

    property OnFileFound: TFileFoundEvent read FOnFileFound write FOnFileFound;
    property OnSearchEnd: TSearchEndEvent read FOnSearchEnd write FOnSearchEnd;
  end;

implementation

uses
  Utility.FileFolder, Kernel.Consts, Kernel.Logger;

{ TFindFiles }

procedure TFindFiles.Execute;
var
  foundList: TStringList;
  intFoundFiles, I: Integer;
  StartTime: Cardinal;
  sPath, sFileExt, sShortName: String;
begin
  TASuiteLogger.Info('Start scanning folders to search files (path = %s)', [FDirectory]);

  if FDirectory = '' then
    Exit;

  intFoundFiles := 0;

  StartTime := GetTickCount64;
  try
    foundList := FindAllFiles(FDirectory, '*', True, faAnyFile);

    for I := 0 to foundList.Count - 1 do
    begin
      if FAbort then
        break;

      sPath := foundList[I];
      sFileExt := ExtractFileExtEx(sPath);
      sShortName := ExtractFileName(sPath);

      if (sPath <> '') and (FindMatchText(FSearchExcludeFilename, sShortName) = -1) and
         ((SearchCriteriaFilename.IndexOf(sFileExt) <> -1) or (SearchCriteriaFilename.IndexOf(EXT_PATH_MASK + sFileExt) <> -1)) then
      begin
        Inc(intFoundFiles);

        if Assigned(FOnFileFound) then
          FOnFileFound(sPath);
      end;
    end;
  finally
    if Assigned(FOnSearchEnd) then
      FOnSearchEnd(intFoundFiles, GetTickCount64 - StartTime);

    foundList.Free;
  end;
end;

function TFindFiles.FindMatchText(Strings: TStrings;
  const Str: string): Integer;
begin
  for Result := 0 to Strings.Count-1 do
    if ContainsText(Str, Strings[Result]) then
      exit;

  Result := -1;
end;

constructor TFindFiles.Create;
begin
  FreeOnTerminate := True;
  inherited Create(True);

  FDirectory := '';
  FSearchCriteriaFilename := TStringList.Create;
  FSearchExcludeFilename := TStringList.Create;
  FAbort := False;
end;

destructor TFindFiles.Destroy;
begin
  FSearchCriteriaFilename.Free;
  FSearchExcludeFilename.Free;

  inherited Destroy;
end;

procedure TFindFiles.Stop;
begin
  FAbort := True;
end;

end.

