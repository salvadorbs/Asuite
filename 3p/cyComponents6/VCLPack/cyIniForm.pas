{   Component(s):
    tcyIniForm

    Description:
    Non-visual component that allow to load and save the position and size of the forms

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

unit cyIniForm;

interface

uses Classes, Windows, Messages, Controls, Forms, SysUtils, Inifiles, Registry, SHFolder;

type
  TcyTempForm = class(TCustomForm)  // In order to access to TCustomForm.Position property
  end;

  RAttributes = Record
    Top: Integer;
    Left: Integer;
    Width: Integer;
    Height: Integer;
    Visible: Boolean;
    State: Integer;
  end;

  TProcOnReadFile = procedure (Sender: TObject; IniFile: TInifile; FileVersion: String) of object;
  TProcOnReadRegistry = procedure (Sender: TObject; Registry: TRegistry; RegistryVersion: String) of object;
  TProcOnWriteFile = procedure (Sender: TObject; IniFile: TInifile) of object;
  TProcOnWriteRegistry = procedure (Sender: TObject; Registry: TRegistry) of object;
  TAttribute = (atPosition, atSize, atVisible, atMinimized, atMaximized);
  TAttributes = Set of TAttribute;
  TMode = (mFile, mRegistry, mBoth);
  TIniDirectory = (idProgramLocation, idCommonAppData, idLocalAppData);
  TRegRoot = (rrKEY_CLASSES_ROOT, rrKEY_CURRENT_USER, rrKEY_LOCAL_MACHINE, rrKEY_USERS, rrKEY_CURRENT_CONFIG);

  TcyIniForm = class(TComponent)
  private
    FFormHandle: HWND;
    FIniCustomfile: String;
    FAutoLoad: Boolean;
    FAutoSave: Boolean;
    FAttributes: TAttributes;
    FIniCustomSection: String;
    FRegRoot: TRegRoot;
    FRegCustomKey: String;
    FMode: TMode;
    FOnCustomSaveToFile: TProcOnWriteFile;
    FOnCustomLoadFromFile: TProcOnReadFile;
    FOnCustomSaveToRegistry: TProcOnWriteRegistry;
    FOnCustomLoadFromRegistry: TProcOnReadRegistry;
    FStoreVersion: String;
    FOnNotLoadFromRegistry: TProcOnReadRegistry;
    FOnNotLoadFromFile: TProcOnReadFile;
    fIniDirectory: TIniDirectory;
    fIniSubDirs: String;
    procedure SetIniCustomfile(AValue: String);
    procedure SetAutoLoad(AValue: Boolean);
    procedure SetAutoSave(AValue: Boolean);
    procedure SetAttributes(const Value: TAttributes);
    function LoadAttributesFromFile(OwnerControl: TWinControl): Boolean;
    function LoadAttributesFromRegistry(OwnerControl: TWinControl): Boolean;
    function SaveAttributesToFile(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
    function SaveAttributesToRegistry(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
    procedure DeleteAttributesFromFile(OwnerControl: TComponent);
    procedure DeleteAttributesFromRegistry(OwnerControl: TComponent);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    // procedure BeforeDestruction; override; Can' t be used because its called after receiving owner free notification ...
    function GetFile: String;
    function GetRootKey: HKey;
    function LoadDefinitions: Boolean;
    function SaveDefinitions: Boolean;
    procedure DeleteDefinitions;
  published
    property Attributes: TAttributes read FAttributes write SetAttributes default [atPosition, atSize];
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad default True;
    property AutoSave: Boolean read FAutoSave write SetAutoSave default True;
    property IniCustomfile: String read FIniCustomfile write SetIniCustomfile;
    property IniCustomSection: String read FIniCustomSection write FIniCustomSection;
    property IniDirectory: TIniDirectory read fIniDirectory write fIniDirectory default idProgramLocation;
    property IniSubDirs: String read fIniSubDirs write fIniSubDirs;
    property Mode: TMode read FMode write FMode default mFile;
    property RegRoot: TRegRoot read FRegRoot write FRegRoot;
    property RegCustomKey: String read FRegCustomKey write FRegCustomKey;
    property StoreVersion: String read FStoreVersion write FStoreVersion;
    property OnCustomLoadFromFile: TProcOnReadFile read FOnCustomLoadFromFile write FOnCustomLoadFromFile;
    property OnCustomSaveToFile: TProcOnWriteFile read FOnCustomSaveToFile write FOnCustomSaveToFile;
    property OnCustomLoadFromRegistry: TProcOnReadRegistry read FOnCustomLoadFromRegistry write FOnCustomLoadFromRegistry;
    property OnCustomSaveToRegistry: TProcOnWriteRegistry read FOnCustomSaveToRegistry write FOnCustomSaveToRegistry;
    property OnNotLoadFromFile: TProcOnReadFile read FOnNotLoadFromFile write FOnNotLoadFromFile;
    property OnNotLoadFromRegistry: TProcOnReadRegistry read FOnNotLoadFromRegistry write FOnNotLoadFromRegistry;
  end;

const
  cNormal = 0;
  cMaximized = 1;
  cMinimized = 2;


implementation

constructor TcyIniForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if AOwner is TForm
  then FFormHandle := TForm(AOwner).Handle    // Avoid rare error reading Handle property on save ...
  else FFormHandle := 0;

  FAttributes := [atPosition, atSize, atMaximized];
  FAutoLoad := True;
  FAutoSave := True;
  FIniCustomfile := '';
  FIniCustomSection := '';
  fIniDirectory := idProgramLocation;
  fIniSubDirs := '';
  FMode := mFile;
  FRegRoot := rrKEY_CURRENT_USER;
  FRegCustomKey := '';
  FStoreVersion := '';
end;

procedure TcyIniForm.Loaded;
begin
  Inherited;

  if not (csDesigning in ComponentState)
  then begin
    if (Owner <> Nil) and (FAutoSave)
    then Owner.FreeNotification(self);  // Notify owner destruction

    if FAutoLoad
    then begin
      if atPosition in FAttributes
      then
        if Owner is TCustomForm    // Set position property if needed ...
        then
          with TcyTempForm(Owner) do
          begin
            if Position in [poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter]
            then Position := poDesigned;
          end;

      LoadDefinitions;
    end;
  end;
end;

procedure TcyIniForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if not (csDesigning in ComponentState)
  then
    if csDestroying in Owner.ComponentState
    then
      if FAutoSave
      then begin
        FAutoSave := false;     // Avoid to be called twice ...
        SaveDefinitions;
      end;

{ This code don't work at 100% because components can be already free.
  if not (csDesigning in ComponentState)
  then
    if AComponent = Owner
    then
      if (FAutoSave) and (Operation = opRemove)
      then SaveAttributes;
 }
  inherited Notification(AComponent, Operation);
end;

function TcyIniForm.LoadDefinitions: Boolean;
begin
  RESULT := false;

  if Owner <> nil
  then
    if Owner is TWinControl
    then begin
      if (not RESULT) and (FMode <> mRegistry)
      then RESULT := LoadAttributesFromFile(TWinControl(Owner));

      if (not RESULT) and (FMode <> mFile)        // Don' t need to load twice !!!
      then RESULT := LoadAttributesFromRegistry(TWinControl(Owner));
    end;
end;

function TcyIniForm.GetFile: String;

    function GetIniDirectory: String;

        function GetSpecialFolderPath(folder: integer) : string;
        const
          SHGFP_TYPE_CURRENT = 0;
        var
          path: array [0..MAX_PATH] of char;
        begin
          if SHGetFolderPath(0, folder, 0, SHGFP_TYPE_CURRENT, @path[0]) = 0
          then Result := path
          else Result := '';
        end;

    begin
      case fIniDirectory of
        idProgramLocation: Result := ExtractFileDir(ParamStr(0));                // Executable
        idCommonAppData:   Result := GetSpecialFolderPath(CSIDL_COMMON_APPDATA); // All users application data
        idLocalAppData:    Result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA);  // Current user application data
      end;
    end;

    function CheckPathDelimiters(Str: String): String;
    var i: Integer;
    begin
      // Initialization :
      if Copy(Str, 1, 2) = '\\'     // Network path ...
      then Result := '\'
      else Result := '';

      // Remove duplicated path delimiters :
      i := Pos('\\', Str);

      while i > 0 do
      begin
        Delete(Str, i, 1);
        i := Pos('\\', Str);
      end;

      Result := Result + Str;
    end;

begin
  Result := '';

  if FIniCustomfile = ''
  then begin
    Result := GetIniDirectory;
    // Add subDir and default filename :
    Result := Result + '\' + fIniSubDirs + '\' + ExtractFileName( ChangeFileExt(ParamStr(0), '.ini') );
    Result := CheckPathDelimiters(Result);
  end
  else begin
    if not DirectoryExists(ExtractFileDir(FIniCustomfile))   // Only filename?
    then begin
      Result := GetIniDirectory;
      // Add subDir and defined filename :
      Result := Result + '\' + fIniSubDirs + '\' + FIniCustomfile;
      Result := CheckPathDelimiters(Result);
    end
    else
      Result := FIniCustomfile;
  end;
end;

function TcyIniForm.LoadAttributesFromFile(OwnerControl: TWinControl): Boolean;
var 
  StrFile, FileVersion, StrSection, Str: String;
  ValidVersion: Boolean;
  i: Integer;
  IniF : TIniFile;
begin
  RESULT := false;
  ValidVersion := false;
  FileVersion := '';
  StrFile := GetFile;

  with OwnerControl do
    try
      IniF := TIniFile.Create(StrFile);

      if FIniCustomSection = ''
      then StrSection := Name
      else StrSection := FIniCustomSection;

      // Read stored file version :
      if IniF.ValueExists(StrSection, 'VERSION')
      then begin
        FileVersion  := IniF.ReadString(StrSection, 'VERSION', '');
        ValidVersion := FStoreVersion = FileVersion;
      end;

      if ValidVersion
      then begin
        if atSize in FAttributes
        then begin
          i := IniF.ReadInteger(StrSection, 'WIDTH', 0);
          if i > 0 then Width := i;

          i := IniF.ReadInteger(StrSection, 'HEIGHT', 0);
          if i > 0 then Height := i;
        end;

        if atPosition in FAttributes
        then begin
          if IniF.ValueExists(StrSection, 'TOP') and IniF.ValueExists(StrSection, 'LEFT')
          then begin
            i := IniF.ReadInteger(StrSection, 'TOP', 0);
            Top := i;

            i := IniF.ReadInteger(StrSection, 'LEFT', 0);
            Left := i;
          end
          else begin
            // Center owner control in the screen:
            Left := (Screen.Width div 2) -  (Width div 2);
            Top  := (Screen.Height div 2) -  (Height div 2);
          end;
        end;

        if atVisible in FAttributes
        then begin
          Str := IniF.ReadString(StrSection, 'VISIBLE', '');
          Visible := Str <> 'N';
        end;

        if OwnerControl is TCustomForm
        then begin
          i := IniF.ReadInteger(StrSection, 'STATE', cNormal);

          if (atMaximized in FAttributes) and (i = cMaximized)
          then TCustomForm(Self.Owner).WindowState := wsMaximized;

          if (atMinimized in FAttributes) and (i = cMinimized)
          then TCustomForm(Self.Owner).WindowState := wsMinimized;
        end;

        RESULT := true;
      end
      else
        if Assigned(FOnNotLoadFromFile)
        then FOnNotLoadFromFile(Self, IniF, FileVersion);

      if Assigned(FOnCustomLoadFromFile)
      then FOnCustomLoadFromFile(Self, IniF, FileVersion);
    finally
      IniF.free;
    end;
end;

function TcyIniForm.GetRootKey: HKey;
begin
  case FRegRoot of
    rrKEY_CLASSES_ROOT:   RESULT := HKEY_CLASSES_ROOT;
    rrKEY_CURRENT_USER:   RESULT := HKEY_CURRENT_USER;
    rrKEY_LOCAL_MACHINE:  RESULT := HKEY_LOCAL_MACHINE;
    rrKEY_USERS:          RESULT := HKEY_USERS;
    rrKEY_CURRENT_CONFIG: RESULT := HKEY_CURRENT_CONFIG;
  end;
end;

function TcyIniForm.LoadAttributesFromRegistry(OwnerControl: TWinControl): Boolean;
var
  ValidVersion: Boolean;
  Registro: TRegistry;
  RegistryVersion, aKey: String;
  i: Integer;
begin
  RESULT := false;
  ValidVersion := false;
  RegistryVersion := '';
  Registro := TRegistry.Create;

  with OwnerControl do
    try
      Registro.RootKey := GetRootKey;

      if FRegCustomKey <> ''
      then begin
        aKey := FRegCustomKey;
        if aKey[1] <> '\' then aKey := '\' + aKey;
      end
      else
        aKey := '\software\' + ExtractFileName(ChangeFileExt(ParamStr(0), '')) + '\' + OwnerControl.Name;

      if Registro.OpenKeyReadOnly(aKey)
      then begin
        // Read stored file version :
        if Registro.ValueExists('VERSION')
        then begin
          RegistryVersion := Registro.ReadString('VERSION');
          ValidVersion := FStoreVersion = RegistryVersion;
        end;

        if ValidVersion
        then begin
          if atSize in FAttributes
          then
            if Registro.ValueExists('WIDTH') and Registro.ValueExists('HEIGHT')
            then begin
              i := Registro.ReadInteger('WIDTH');
              if i > 0 then Width := i;

              i := Registro.ReadInteger('HEIGHT');
              if i > 0 then Height := i;
            end;

          if atPosition in FAttributes
          then begin
            if Registro.ValueExists('TOP') and Registro.ValueExists('LEFT')
            then begin
              i := Registro.ReadInteger('TOP');
              Top := i;

              i := Registro.ReadInteger('LEFT');
              Left := i;
            end
            else begin
              // Center owner control in the screen:
              Left := (Screen.Width div 2) -  (Width div 2);
              Top  := (Screen.Height div 2) -  (Height div 2);
            end;
          end;

          if atVisible in FAttributes
          then
            if Registro.ValueExists('VISIBLE')
            then Visible := Registro.ReadBool('VISIBLE');

          if OwnerControl is TCustomForm
          then
            if Registro.ValueExists('STATE')
            then begin
              i := Registro.ReadInteger('STATE');

              if (atMaximized in FAttributes) and (i = cMaximized)
              then TCustomForm(Self.Owner).WindowState := wsMaximized;

              if (atMinimized in FAttributes) and (i = cMinimized)
              then TCustomForm(Self.Owner).WindowState := wsMinimized;
            end;

          RESULT := true;
        end
        else
          if Assigned(FOnNotLoadFromRegistry)
          then FOnNotLoadFromRegistry(Self, Registro, RegistryVersion);

        if Assigned(FOnCustomLoadFromRegistry)
        then FOnCustomLoadFromRegistry(Self, Registro, RegistryVersion);

        Registro.CloseKey;
      end;
    finally
      Registro.Free;
    end;
end;

function TcyIniForm.SaveDefinitions: Boolean;
var
  RAttr: RAttributes;
  WinPlacement: TWindowPlacement;
  RectNormalPos: TRect;
begin
  RESULT := false;

  if Owner <> nil
  then
    if Owner is TWinControl
    then begin
      RAttr.Visible := TWinControl(Owner).Visible;
      RAttr.State := cNormal;

      if atMinimized in FAttributes
      then
        if IsIconic(TWinControl(Owner).Handle)    // Don't work on Vista
          or IsIconic(Application.Handle)
        then RAttr.State := cMinimized;

      if atMaximized in FAttributes
      then
        // Avoid rare error reading Handle property :
        if FFormHandle <> 0
        then
          if IsZoomed(FFormHandle)
          then RAttr.State := cMaximized;

      if RAttr.State <> cNormal
      then begin
        WinPlacement.length := SizeOf(WinPlacement);
        FillChar(WinPlacement, SizeOf(WinPlacement), #0);

        try
          if GetWindowPlacement(TWinControl(Owner).Handle, @WinPlacement)
          then RectNormalPos := WinPlacement.rcNormalPosition;  // Position independently of form state
        except
          RectNormalPos := TWinControl(Owner).BoundsRect;
        end;
      end
      else
        RectNormalPos := TWinControl(Owner).BoundsRect;

      RAttr.Top := RectNormalPos.Top;
      RAttr.Left := RectNormalPos.Left;
      RAttr.Width := RectNormalPos.Right-RectNormalPos.Left;
      RAttr.Height := RectNormalPos.Bottom-RectNormalPos.Top;

      if FMode <> mRegistry
      then RESULT := RESULT or SaveAttributesToFile(TWinControl(Owner), RAttr);

      if FMode <> mFile
      then RESULT := RESULT or SaveAttributesToRegistry(TWinControl(Owner), RAttr);
    end;
end;

function TcyIniForm.SaveAttributesToFile(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
var
  StrFile, StrSection: String;
  IniF : TIniFile;
begin
  RESULT := false;
  StrFile := GetFile;

  try
    IniF := TIniFile.Create(StrFile);

    try       // This try avoid write error (read only ini file for exemple) occured when only using try finally statement ...

      // Create directory if not exists :
      if not DirectoryExists(ExtractFileDir(StrFile))
      then ForceDirectories(ExtractFileDir(StrFile));

      if FIniCustomSection = ''
      then StrSection := OwnerControl.Name
      else StrSection := FIniCustomSection;

      IniF.WriteString(StrSection, 'VERSION', FStoreVersion);

      if atPosition in FAttributes
      then begin
        IniF.WriteInteger(StrSection, 'TOP', RAttr.Top);
        IniF.WriteInteger(StrSection, 'LEFT', RAttr.Left);
      end;

      if atSize  in FAttributes
      then begin
        IniF.WriteInteger(StrSection, 'WIDTH', RAttr.Width);
        IniF.WriteInteger(StrSection, 'HEIGHT', RAttr.Height);
      end;

      if atVisible in FAttributes
      then
        if RAttr.Visible
        then IniF.WriteString(StrSection, 'VISIBLE', 'Y')
        else IniF.WriteString(StrSection, 'VISIBLE', 'N');

      if (atMinimized in FAttributes) or (atMaximized in FAttributes)
      then IniF.WriteInteger(StrSection, 'STATE', RAttr.State);

      if Assigned(FOnCustomSaveToFile)
      then FOnCustomSaveToFile(Self, IniF);

      RESULT := true;
    except

    end;
  finally
    IniF.free;  
  end;
end;

function TcyIniForm.SaveAttributesToRegistry(OwnerControl: TWinControl; RAttr: RAttributes): Boolean;
var
  Registro: TRegistry;
  aKey: String;
begin
  RESULT := false;
  Registro := TRegistry.Create;

  try
    Registro.RootKey := GetRootKey;

    if FRegCustomKey <> ''
    then begin
      aKey := FRegCustomKey;
      if aKey[1] <> '\' then aKey := '\' + aKey;
    end
    else
      aKey := '\software\' + ExtractFileName(ChangeFileExt(ParamStr(0), '')) + '\' + OwnerControl.Name;

    if Registro.OpenKey(aKey, true)
    then begin
      Registro.WriteString('VERSION', FStoreVersion);

      if atPosition in FAttributes
      then begin
        Registro.WriteInteger('TOP', RAttr.Top);
        Registro.WriteInteger('LEFT', RAttr.Left);
      end;

      if atSize  in FAttributes
      then begin
        Registro.WriteInteger('WIDTH', RAttr.Width);
        Registro.WriteInteger('HEIGHT', RAttr.Height);
      end;

      if atVisible in FAttributes
      then Registro.WriteBool('VISIBLE', RAttr.Visible);

      if (atMinimized in FAttributes) or (atMaximized in FAttributes)
      then Registro.WriteInteger('STATE', RAttr.State);

      if Assigned(FOnCustomSaveToRegistry)
      then FOnCustomSaveToRegistry(Self, Registro);

      Registro.CloseKey;
      RESULT := true;
    end;
  finally
    Registro.Free;
  end;
end;

procedure TcyIniForm.DeleteDefinitions;
begin
  if Owner = nil then Exit;

  if FMode <> mRegistry
  then DeleteAttributesFromFile(Owner);

  if FMode <> mFile
  then DeleteAttributesFromRegistry(Owner);
end;

procedure TcyIniForm.DeleteAttributesFromFile(OwnerControl: TComponent);
var StrFile, StrSection: String;
begin
  StrFile := GetFile;
  if FIniCustomSection = ''
  then StrSection := OwnerControl.Name
  else StrSection := FIniCustomSection;

  if FileExists(StrFile)
  then
    with TIniFile.Create(StrFile) do
      try
        EraseSection(StrSection);
      finally
        free;
      end;
end;

procedure TcyIniForm.DeleteAttributesFromRegistry(OwnerControl: TComponent);
var aKey: String;
begin
  with TRegistry.Create do
    try
      RootKey := GetRootKey;

      if FRegCustomKey <> ''
      then begin
        aKey := FRegCustomKey;
        if aKey[1] <> '\' then aKey := '\' + aKey;
      end
      else
        aKey := '\software\' + ExtractFileName(ChangeFileExt(ParamStr(0), ''));

      DeleteKey(aKey);
    finally
      Free;
    end;
end;

procedure TcyIniForm.SetIniCustomfile(AValue: String);
begin
  if AValue <> FIniCustomfile
  then FIniCustomfile := AValue;
end;

procedure TcyIniForm.SetAttributes(const Value: TAttributes);
begin
  FAttributes := Value;
end;

procedure TcyIniForm.SetAutoLoad(AValue: Boolean);
begin
  if AValue <> FAutoLoad
  then FAutoLoad := AValue;
end;

procedure TcyIniForm.SetAutoSave(AValue: Boolean);
begin
  if AValue <> FAutoSave
  then FAutoSave := AValue;  
end;

end.
