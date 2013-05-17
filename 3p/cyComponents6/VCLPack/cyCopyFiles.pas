{   Component(s):
    tcyCopyFiles

    Description:
    Component to copy files from a directory to another.

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


unit cyCopyFiles;

{$I ..\Core\cyCompilerDefines.inc}

interface

Uses Classes, StrUtils, SysUtils, Windows, ComCtrls, cySearchFiles;

type  
  TCopyFileResult = (cfCreate, cfOverwrite, cfNoNeed, cfForceDirectoryError, cfCopyError);

  TCopyFileExists = (feDoNothing, feCopy, feCopyIfModified, feCopyIfMoreRecent);
  TCopyFileNotExists = (fnDoNothing, fnCopy, fnCopyForceDir);

  TDestinationOptions=class(TPersistent)
  private
    FResetAttributes: boolean;
    FIfFileExists: TCopyFileExists;
    FIfFileNotExists: TCopyFileNotExists;
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property ResetAttributes: boolean read FResetAttributes write FResetAttributes;
    property IfFileExists: TCopyFileExists read FIfFileExists write FIfFileExists;
    property IfFileNotExists: TCopyFileNotExists read FIfFileNotExists write FIfFileNotExists;
  end;

  TProcCustomCopyFileEvent = procedure (Sender: TObject; var CopyFileResult: TCopyFileResult) of object;
  TProcOnCopyFileProgressEvent = procedure (Sender: TObject; FileBytes, TransferedBytes: LARGE_INTEGER; PercentDone: Int64) of object;
  TProcOnCustomSetFileDestination = procedure (Sender: TObject; var FileName: String) of object;

  TcyCopyFiles = class(TcyCustomSearchFiles)
  private
    // Internal variables for runtime //
    FCreateSubDirsBeforeCopy: boolean;
    FDestinationExe: String;  // Destination path when we call execute ...
    FSourceExe: String;       // Source path when we call execute ...
    //
    FDestinationPath: String;
    FFileSource: String;
    FFileDestination: String;
    FCancelCurrentFile: boolean;
    FBeforeCopyFile: TNotifyEvent;
    FAfterCopyFile: TNotifyEvent;
    FOnCopyFileFailed: TNotifyEvent;
    FCustomCopyFile: TProcCustomCopyFileEvent;
    FDestinationOptions: TDestinationOptions;
    FCurrentFileProcess: TCopyFileResult;
    FOnCopyFileProgress: TProcOnCopyFileProgressEvent;
    FCopyFilesCount: Integer;
    FCopyFailsCount: Integer;
    FOnCustomSetFileDestination: TProcOnCustomSetFileDestination;
    procedure SetDestinationOptions(const Value: TDestinationOptions);
    procedure DoCopyFile;
    procedure DoCopyFileEx;
  protected
    procedure SearchRecAcceptedFile(var DoFindNext: boolean); override;
    function SearchRecValidateFolder: boolean; override;
    procedure SearchRecExitFolder; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean; override;
    procedure DefaultCopyFile;
    property CancelCurrentFile: Boolean read FCancelCurrentFile write FCancelCurrentFile;
    property FileSource: String read FFileSource;
    property FileDestination: String read FFileDestination;
    property CurrentFileProcess: TCopyFileResult read FCurrentFileProcess;
    property CopyFilesCount: Integer read FCopyFilesCount;
    property CopyFailsCount: Integer read FCopyFailsCount;
    property ActiveSearchRec;
    property Aborted;
    property CurrentFileName;
    property CurrentDirectory;
    property MatchedDirectories;
    property MatchedFiles;
    property SearchState;
  published
    property FileAttributes;
    property FromPath;
    property MaskInclude;
    property MaskExclude;
    property Options;
    property SubDirectories;
    property OnAbort;
    property OnPause;
    property OnResume;
    property OnValidateFile;
    property OnValidateDirectory;
    property OnExitDirectory;
    property OnTerminate;
    property DestinationOptions: TDestinationOptions read FDestinationOptions write SetDestinationOptions;
    property DestinationPath: String read FDestinationPath write FDestinationPath;
    property BeforeCopyFile: TNotifyEvent read FBeforeCopyFile write FBeforeCopyFile;
    property CustomCopyFile: TProcCustomCopyFileEvent read FCustomCopyFile write FCustomCopyFile;
    property AfterCopyFile: TNotifyEvent read FAfterCopyFile write FAfterCopyFile;
    property OnCustomSetFileDestination: TProcOnCustomSetFileDestination read FOnCustomSetFileDestination write FOnCustomSetFileDestination;
    property OnCopyFileFailed: TNotifyEvent read FOnCopyFileFailed write FOnCopyFileFailed;
    // Only called for Win 2000/Nt/Xp and later :
    property OnCopyFileProgress: TProcOnCopyFileProgressEvent read FOnCopyFileProgress write FOnCopyFileProgress;
  end;

var FCancCopyFileEx: boolean;     // if we initialize with = false, the function will not work !

// Functions to use without TcyCopyFiles :
function cyCopyFile(FromFile, ToFile: String; FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists;
                   ResetAttr: boolean): TCopyFileResult;

function cyCopyFileEx(FromFile, ToFile: String; FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists;
                      ResetAttr: boolean; aProgressBar: TProgressBar): TCopyFileResult;

function cyCopyFilesEx(SourcePath, DestinationPath, IncludeFilters, ExcludeFilters: String;
                     ArchiveFiles, ReadOnlyFiles, HiddenFiles, SystemFiles: TcyFileAttributeMode;
                     FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists;
                     SubFolders, ResetAttributes: Boolean): Integer;

implementation

function DetermineCopyFile(FromFile, ToFile: String; FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists): TCopyFileResult;
begin
  RESULT := cfNoNeed;

  if SysUtils.FileExists(ToFile)
  then begin
    case FileExists of
      feCopy:
       RESULT := cfOverwrite;

      feCopyIfModified:
        if FileAge(FromFile) <> FileAge(ToFile)
        then RESULT := cfOverwrite;

      feCopyIfMoreRecent:
        if FileAge(FromFile) > FileAge(ToFile)
        then RESULT := cfOverwrite;
    end;
  end
  else
    if FileNotExists <> fnDoNothing
    then RESULT := cfCreate;
end;

function PrepareCopyFile(FromFile, ToFile: String; FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists): TCopyFileResult;
var
  ToFileExists, ToFileResetAttr: boolean;
  ToFileAttr: Integer;
begin
  RESULT := cfNoNeed;
  ToFileExists := SysUtils.FileExists(ToFile);

  if ToFileExists
  then begin
    case FileExists of
      feCopy:
       RESULT := cfOverwrite;

      feCopyIfModified:
        if FileAge(FromFile) <> FileAge(ToFile)
        then RESULT := cfOverwrite;

      feCopyIfMoreRecent:
        if FileAge(FromFile) > FileAge(ToFile)
        then RESULT := cfOverwrite;
    end;

    if RESULT = cfOverwrite
    then begin
      ToFileAttr :=FileGetAttr(ToFile);
      ToFileResetAttr := ToFileAttr = faAnyFile;
      if not ToFileResetAttr
      then ToFileResetAttr := (ToFileAttr and SysUtils.faReadOnly) <> 0;
      if not ToFileResetAttr
      then ToFileResetAttr := (ToFileAttr and faHidden) <> 0;

      if ToFileResetAttr
      then FileSetAttr(ToFile, 0);   // In order to let overwite file ...
    end;
  end
  else begin
    case FileNotExists of
      fnCopy:
        RESULT := cfCreate;   // May not copy if destination directory not exists ...

      fnCopyForceDir:
        if DirectoryExists(ExtractFileDir(ToFile))
        then
          RESULT := cfCreate
        else
          if ForceDirectories(ExtractFileDir(ToFile))
          then RESULT := cfCreate
          else RESULT := cfForceDirectoryError;

//      fnDoNothing ;
    end;
  end;
end;

function cyCopyFile(FromFile, ToFile: String; FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists;
                   ResetAttr: boolean): TCopyFileResult;
begin
  RESULT := PrepareCopyFile(FromFile, ToFile, FileExists, FileNotExists);

  if RESULT in [cfCreate, cfOverwrite]
  then
    try
      //
      if Windows.CopyFile(PChar(FromFile), PChar(ToFile), false)
      then begin
        if ResetAttr
        then FileSetAttr(ToFile, 0);
      end
      else
        RESULT := cfCopyError;
    except
      RESULT := cfCopyError;
    end;
end;

function cyCopyFileEx(FromFile, ToFile: String; FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists;
                      ResetAttr: boolean; aProgressBar: TProgressBar): TCopyFileResult;

    function CopyProgress(
      TotalFileSize: LARGE_INTEGER;                      // Total file size in bytes
      TotalBytesTransferred: LARGE_INTEGER;              // Total file size transferred in bytes
      StreamSize: LARGE_INTEGER;                         // Total stream size in bytes
      StreamBytesTransferred: LARGE_INTEGER;             // Total stream size transferred in bytes
      dwStreamNumber: DWord;                             // Stream number
      dwCallbackReason: DWord;                           // Call back reason
      hSourceFile: THandle;                              // FromFile file handle
      hDestinationFile: THandle;                         // ToFile file handle
      ProgressBar: TProgressBar): DWord; stdcall;        // Allow specify TProgressBar
    var
      PercDone: Int64;
    begin
      if ProgressBar <> nil
      then begin
        if TotalFileSize.QuadPart = 0
        then PercDone := 0
        else PercDone := TotalBytesTransferred.QuadPart * 100 div TotalFileSize.QuadPart;

        ProgressBar.Position := PercDone;
      end;

      // Continue with process?
      Result := PROGRESS_CONTINUE;
    end;

begin
  FCancCopyFileEx := false;
  RESULT := PrepareCopyFile(FromFile, ToFile, FileExists, FileNotExists);

  if RESULT in [cfCreate, cfOverwrite]
  then
    try
      {$IFDEF DELPHI2009_OR_ABOVE}
      if Windows.CopyFileExW(PChar(FromFile), PChar(ToFile), @CopyProgress, aProgressBar, @FCancCopyFileEx, 0)
      {$ELSE}
      if Windows.CopyFileExW(PAnsiChar(FromFile), PAnsiChar(ToFile), @CopyProgress, aProgressBar, @FCancCopyFileEx, 0)
      {$ENDIF}
      then begin
        if ResetAttr
        then FileSetAttr(ToFile, 0);
      end
      else
        RESULT := cfCopyError;
    except
      RESULT := cfCopyError;
    end;
end;

function cyCopyFilesEx(SourcePath, DestinationPath, IncludeFilters, ExcludeFilters: String;
                     ArchiveFiles, ReadOnlyFiles, HiddenFiles, SystemFiles: TcyFileAttributeMode;
                     FileExists: TCopyFileExists; FileNotExists: TCopyFileNotExists;
                     SubFolders, ResetAttributes: Boolean): Integer;
var aCopyFiles: TcyCopyFiles;
begin
  RESULT := 0;
  aCopyFiles := TcyCopyFiles.Create(nil);
  aCopyFiles.FromPath := SourcePath;
  aCopyFiles.SubDirectories := SubFolders;
  aCopyFiles.MaskInclude.Delimiter := ';';
  aCopyFiles.MaskInclude.DelimitedText := IncludeFilters;
  aCopyFiles.MaskExclude.Delimiter := ';';
  aCopyFiles.MaskExclude.DelimitedText := ExcludeFilters;
  aCopyFiles.FileAttributes.Archive := ArchiveFiles;
  aCopyFiles.FileAttributes.Hidden := HiddenFiles;
  aCopyFiles.FileAttributes.ReadOnly := ReadOnlyFiles;
  aCopyFiles.FileAttributes.System := SystemFiles;
  aCopyFiles.DestinationPath := DestinationPath;
  aCopyFiles.DestinationOptions.IfFileExists := FileExists;
  aCopyFiles.DestinationOptions.IfFileNotExists := FileNotExists;
  aCopyFiles.DestinationOptions.ResetAttributes := ResetAttributes;

  if aCopyFiles.Execute then
    RESULT := aCopyFiles.CopyFilesCount;

  aCopyFiles.Free;
end;

{ TDestinationOptions }
constructor TDestinationOptions.Create(AOwner: TComponent);
begin
  FResetAttributes := false;
  FIfFileExists := feCopyIfMoreRecent;
  FIfFileNotExists := fnCopy;
end;

{ TcyCopyFiles }
constructor TcyCopyFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDestinationOptions := TDestinationOptions.Create(self);
end;

destructor TcyCopyFiles.Destroy;
begin
  FDestinationOptions.Free;
  inherited Destroy;
end;

procedure TcyCopyFiles.SetDestinationOptions(const Value: TDestinationOptions);
begin
  FDestinationOptions := Value;
end;

function TcyCopyFiles.Execute: boolean;
begin
  FDestinationExe := FDestinationPath;
  if RightStr(FDestinationExe, 1) <> '\' then FDestinationExe := FDestinationExe + '\';

  FSourceExe := FromPath;
  if RightStr(FSourceExe, 1) <> '\' then FSourceExe := FSourceExe + '\';

  FCopyFilesCount := 0;
  FCopyFailsCount := 0;
  FCreateSubDirsBeforeCopy := false;

  if not DirectoryExists(FDestinationPath)
  then ForceDirectories(FDestinationPath);      

  RESULT := Inherited Execute;
end;

function TcyCopyFiles.SearchRecValidateFolder: boolean;
begin
  RESULT := Inherited SearchRecValidateFolder;
  if RESULT
  then FCreateSubDirsBeforeCopy := true;      
end;

procedure TcyCopyFiles.SearchRecExitFolder;
begin
  Inherited;
  FCreateSubDirsBeforeCopy := false;  // for the case of no file matched on folder ...
end;

procedure TcyCopyFiles.SearchRecAcceptedFile(var DoFindNext: boolean);
var
  DefaultCopy: boolean;
  fnTmp : TCopyFileNotExists;
begin
  DoFindNext := true;
  FFileSource := CurrentDirectory + CurrentFileName;
  FFileDestination := FileSource;
  Delete(FFileDestination, 1, Length(FSourceExe));
  FFileDestination := FDestinationExe + FFileDestination;

  // Let change FFileDestination:
  if Assigned(FOnCustomSetFileDestination)
  then FOnCustomSetFileDestination(Self, FFileDestination);

  FCancelCurrentFile := false;
  DefaultCopy := not Assigned(FCustomCopyFile);

  if DefaultCopy
  then begin
    FCurrentFileProcess := cfNoNeed;
    // Avoid the use of DirectoryExists() for all files in the same folder :
    if FDestinationOptions.FIfFileNotExists = fnCopyForceDir
    then begin
      fnTmp := fnCopy;

      if FCreateSubDirsBeforeCopy
      then begin
        FCreateSubDirsBeforeCopy := false;

        if not DirectoryExists(ExtractFileDir(FFileDestination))
        then
          if not ForceDirectories(ExtractFileDir(FFileDestination))
          then FCurrentFileProcess := cfForceDirectoryError;
      end;
    end
    else
      fnTmp := FDestinationOptions.FIfFileNotExists;

    if FCurrentFileProcess <> cfForceDirectoryError
    then
      FCurrentFileProcess := PrepareCopyFile(FFileSource, FFileDestination,
                               FDestinationOptions.FIfFileExists, fnTmp);

    if FCurrentFileProcess in [cfCreate, cfOverwrite]
    then begin
      if Assigned(FBeforeCopyFile)
      then FBeforeCopyFile(Self);

      DefaultCopyFile;     
    end;
  end
  else begin
    FCurrentFileProcess := DetermineCopyFile(FFileSource, FFileDestination,
                             FDestinationOptions.FIfFileExists, FDestinationOptions.FIfFileNotExists);

    if FCurrentFileProcess in [cfCreate, cfOverwrite]
    then begin
      if Assigned(FBeforeCopyFile)
      then FBeforeCopyFile(Self);

      FCustomCopyFile(Self, FCurrentFileProcess);
    end;
  end;

  if (SearchState = ssPausing) and (FCurrentFileProcess = cfCopyError) // The user pause process and the file copy has failed ...
  then begin
    DoFindNext := false;        // Copy the same file after resuming ...
  end
  else
    case FCurrentFileProcess of
      // cfNoNeed: ;

      cfCreate, cfOverwrite:
      begin
        inc(FCopyFilesCount, 1);
        if Assigned(FAfterCopyFile)
        then FAfterCopyFile(Self);
      end;

      cfForceDirectoryError, cfCopyError:
      begin
        inc(FCopyFailsCount, 1);
        if Assigned(FOnCopyFileFailed)
        then FOnCopyFileFailed(Self);
      end;
    end;
end;

procedure TcyCopyFiles.DefaultCopyFile;
var
  VersionInfo: TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize:=SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);

  if VersionInfo.dwPlatformId >= VER_PLATFORM_WIN32_NT // Win 2000/Nt/Xp and later
  then DoCopyFileEx
  else DoCopyFile;
end;

procedure TcyCopyFiles.DoCopyFile;
begin
  try
    if Windows.CopyFile(PChar(FFileSource), PChar(FFileDestination), false)
    then begin
      if FDestinationOptions.FResetAttributes
      then FileSetAttr(FFileDestination, 0);
    end
    else
      FCurrentFileProcess := cfCopyError;
  except
    FCurrentFileProcess := cfCopyError;
  end;
end;

procedure TcyCopyFiles.DoCopyFileEx;

    function DoCopyProgress(
      TotalFileSize: LARGE_INTEGER;
      TotalBytesTransferred: LARGE_INTEGER;
      StreamSize: LARGE_INTEGER;
      StreamBytesTransferred: LARGE_INTEGER;
      dwStreamNumber: DWord;
      dwCallbackReason: DWord;
      hSourceFile: THandle;
      hDestinationFile: THandle;
      lpData: Pointer): DWord; stdcall;             // Pointer to TcyCopyFiles ...
    var
      PercentDone: Int64;
    begin
      with TcyCopyFiles(lpData) do
      begin
        if Assigned(FOnCopyFileProgress)
        then begin
          if TotalFileSize.QuadPart = 0
          then PercentDone := 0
          else PercentDone := TotalBytesTransferred.QuadPart * 100 div TotalFileSize.QuadPart;

          FOnCopyFileProgress(TcyCopyFiles(lpData), TotalFileSize, TotalBytesTransferred, PercentDone);
        end;


{  Impossible to resume during CopyFileEx!
   To let user cancel the current file, use TcyCopyFiles.CancelCurrentFile := true;

        case SearchState of
          ssAborting:
            Result := PROGRESS_CANCEL;

          ssPausing:
            Result := PROGRESS_STOP;  // Unfortunately, the file don't continue on resume ...

          else
            Result := PROGRESS_CONTINUE;
        end;                               }

        case SearchState of
          ssAborting, sspausing:
            Result := PROGRESS_CANCEL;  // Abort CopyFileEx ...

          else
            Result := PROGRESS_CONTINUE;
        end;
      end;
    end;

begin
  try
    {$IFDEF DELPHI2009_OR_ABOVE}
    if Windows.CopyFileExW(PChar(FFileSource), PChar(FFileDestination), @DoCopyProgress, Self, @FCancelCurrentFile, 0)
    {$ELSE}
    if Windows.CopyFileEx(PAnsiChar(FFileSource), PAnsiChar(FFileDestination), @DoCopyProgress, Self, @FCancelCurrentFile, 0)
    {$ENDIF}
    then begin
      if FDestinationOptions.FResetAttributes
      then FileSetAttr(FFileDestination, 0);
    end
    else
      FCurrentFileProcess := cfCopyError;
  except
    FCurrentFileProcess := cfCopyError;
  end;
end;

end.
