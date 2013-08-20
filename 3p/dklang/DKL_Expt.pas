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
// Declarations of the core IDE integration component - DKLang Expert
//
unit DKL_Expt;

interface
uses Classes, ToolsAPI, DesignEditors;

   // Creates DKLang expert instance
  function DKLang_CreateExpert: IOTAWizard;

type
   // TDKLanguageController component editor
  TDKLangControllerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

const
  SDKLExpt_ConstantResFileEnding     = '.dkl_const.res';

resourcestring
  SDKLExptErr_CannotObtainNTAIntf    = 'Cannot obtain INTAServices interface';
  SDKLExptErr_CannotObtainOTAIntf    = 'Cannot obtain IOTAServices interface';
  SDKLExptErr_CannotObtainModSvcIntf = 'Cannot obtain IOTAModuleServices interface';
  SDKLExptErr_CannotFindProjectMenu  = 'Cannot locate ''ProjectMenu'' submenu item';
  SDKLExptErr_CannotFindProject      = 'No active project found';
  SDKLExptErr_CannotSaveLangSource   = 'Failed to update project language source. Check whether project is open and active';

  SDKLExptMsg_LCsUpdated             = '%d language controllers have updated the project language source.';

  SDKLExptMenuItem_EditConstants     = 'Edit pro&ject constants...';
  SDKLExptMenuItem_UpdateLangSource  = 'Update project lan&guage source';

implementation //=======================================================================================================
uses
  SysUtils, Windows, Registry, Menus, Graphics, Dialogs, DesignIntf, TypInfo, Forms, RTLConsts, 
  DKLang, DKL_ConstEditor, DKL_ResFile;


   // Returns the current active project, if any; raises an exception otherwise
  function GetActualProject: IOTAProject;
  begin
    Result := GetActiveProject;
    if Result=nil then DKLangError(SDKLExptErr_CannotFindProject);
  end;

   // Stores the LSObject's language source data in the current project's language source file. Returns True if
   //   succeeded
  function UpdateProjectLangSource(LSObject: IDKLang_LanguageSourceObject): Boolean;
  var Proj: IOTAProject;
  begin
     // If a project is open
    Proj := GetActiveProject;
    Result := Proj<>nil;
    if Result then UpdateLangSourceFile(ChangeFileExt(Proj.FileName, '.'+SDKLang_LangSourceExtension), LSObject, []);
  end;

   // Finds first TDKLanguageController instance among components owned by RootComp, if any, and calls
   //   UpdateProjectLangSource(). Returns True if succeeded
  function LC_UpdateProjectLangSource(RootComp: TComponent): Boolean;
  var
    i: Integer;
    LC: TDKLanguageController;
  begin
    Result := False;
    if RootComp<>nil then
      for i := 0 to RootComp.ComponentCount-1 do
         // If found
        if RootComp.Components[i] is TDKLanguageController then begin
          LC := TDKLanguageController(RootComp.Components[i]);
          if dklcoAutoSaveLangSource in LC.Options then begin
            UpdateProjectLangSource(LC);
            Result := True;
          end;
          Break;
        end;
  end;

type
  TDKLang_Expert = class;

   //===================================================================================================================
   // TDKLang_FormNotifier
   //===================================================================================================================

  TDKLang_FormNotifier = class(TNotifierObject, IOTANotifier, IOTAFormNotifier)
  private
     // The module with which the notifier is associated
    FModule: IOTAModule;
     // IOTANotifier
    procedure Destroyed;
     // IOTAFormNotifier
    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle; const OldName, NewName: UnicodeString);
  public
    constructor Create(AModule: IOTAModule);
  end;

   //===================================================================================================================
   // TDKLang_OTAIDENotifier
   //===================================================================================================================

  TDKLang_OTAIDENotifier = class(TNotifierObject, IOTAIDENotifier)
  private
     // Expert owner
    FExpert: TDKLang_Expert;
     // IOTAIDENotifier
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: UnicodeString; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure AfterCompile(Succeeded: Boolean);
  public
    constructor Create(AExpert: TDKLang_Expert);
  end;

   //===================================================================================================================
   // TDKLang_Expert
   //===================================================================================================================

  TDKLang_Expert = class(TNotifierObject, IOTAWizard)
  private
     // IDE interface
    FNTAServices: INTAServices;
    FOTAServices: IOTAServices;
    FModServices: IOTAModuleServices;
     // OTA notifier index
    FOTANotifierIndex: Integer;
     // Menu item owner
    FMenuOwner: TComponent;
     // Menu items
    FItem_EditConstants: TMenuItem;
    FItem_UpdateLangSource: TMenuItem;
     // Adds and returns a menu item
    function  NewMenuItem(const wsCaption: UnicodeString; Menu: TMenuItem; AOnClick: TNotifyEvent): TMenuItem;
     // Menu item click events
    procedure ItemClick_EditConstants(Sender: TObject);
    procedure ItemClick_UpdateLangSource(Sender: TObject);
     // Invokes the constant editor for editing constant data in the project resources. Returns True if user saved the
     //   changes
    function  EditConstantsResource: Boolean;
     // Callback function for obtaining current language ID
    function  GetLangIDCallback: LANGID;
     // IOTAWizard
    function  GetIDString: UnicodeString;
    function  GetName: UnicodeString;
    function  GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
  end;

   //===================================================================================================================
   // TDKLang_FormNotifier
   //===================================================================================================================

  procedure TDKLang_FormNotifier.ComponentRenamed(ComponentHandle: TOTAHandle; const OldName, NewName: UnicodeString);
  begin
    { stub }
  end;

  constructor TDKLang_FormNotifier.Create(AModule: IOTAModule);
  begin
    inherited Create;
    FModule := AModule;
  end;

  procedure TDKLang_FormNotifier.Destroyed;
  begin
    FModule := nil;
  end;

  procedure TDKLang_FormNotifier.FormActivated;
  begin
    { stub }
  end;

  procedure TDKLang_FormNotifier.FormSaving;
  var
    i: Integer;
    NTAFormEditor: INTAFormEditor;
  begin
    if FModule=nil then Exit;
     // Find the FormEditor interface for the module
    for i := 0 to FModule.ModuleFileCount-1 do
      if Supports(FModule.ModuleFileEditors[i], INTAFormEditor, NTAFormEditor) then begin
        LC_UpdateProjectLangSource(NTAFormEditor.FormDesigner.Root);
        Break;
      end;
  end;

   //===================================================================================================================
   // TDKLang_OTAIDENotifier
   //===================================================================================================================

  procedure TDKLang_OTAIDENotifier.AfterCompile(Succeeded: Boolean);
  begin
    { stub }
  end;

  procedure TDKLang_OTAIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
  begin
    { stub }
  end;

  constructor TDKLang_OTAIDENotifier.Create(AExpert: TDKLang_Expert);
  begin
    inherited Create;
    FExpert := AExpert;
  end;

  procedure TDKLang_OTAIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: UnicodeString; var Cancel: Boolean);
  var
    Module: IOTAModule;
    OTAFormEditor: IOTAFormEditor;
    i: Integer;
  begin
    if NotifyCode=ofnFileOpened then begin
       // Find the module by file name and install the notifier on IOTAFormEditor interface
      Module := FExpert.FModServices.FindModule(FileName);
      if Module<>nil then
        for i := 0 to Module.ModuleFileCount-1 do
          if Supports(Module.ModuleFileEditors[i], IOTAFormEditor, OTAFormEditor) then
            OTAFormEditor.AddNotifier(TDKLang_FormNotifier.Create(Module));
    end;
  end;

   //===================================================================================================================
   // TDKLang_Expert
   //===================================================================================================================

  constructor TDKLang_Expert.Create;
  var
    mi, miTest: TMenuItem;
    i: Integer;
  begin
    inherited Create;
     // Obtain needed IDE interfaces
    if not Supports(BorlandIDEServices, INTAServices,       FNTAServices) then DKLangError(SDKLExptErr_CannotObtainNTAIntf);
    if not Supports(BorlandIDEServices, IOTAServices,       FOTAServices) then DKLangError(SDKLExptErr_CannotObtainOTAIntf);
    if not Supports(BorlandIDEServices, IOTAModuleServices, FModServices) then DKLangError(SDKLExptErr_CannotObtainModSvcIntf);
     // Register OTA services notifier
    FOTANotifierIndex := FOTAServices.AddNotifier(TDKLang_OTAIDENotifier.Create(Self));
     // Find 'Project' menu
    mi := nil;
    for i := 0 to FNTAServices.MainMenu.Items.Count-1 do begin
      miTest := FNTAServices.MainMenu.Items[i];
      if SameText(miTest.Name, 'ProjectMenu') then begin
        mi := miTest;
        Break;
      end;
    end;
    if mi=nil then DKLangError(SDKLExptErr_CannotFindProjectMenu);
     // Create a dummy menu item owner component
    FMenuOwner := TComponent.Create(nil);
     // Insert a separator
    NewMenuItem('-', mi, nil);
     // Create menu items
    FItem_EditConstants    := NewMenuItem(SDKLExptMenuItem_EditConstants,    mi, ItemClick_EditConstants);
    FItem_UpdateLangSource := NewMenuItem(SDKLExptMenuItem_UpdateLangSource, mi, ItemClick_UpdateLangSource);
     // Set the designtime flag
    IsDesignTime := True;
  end;

  destructor TDKLang_Expert.Destroy;
  begin
     // Clear the designtime flag
    IsDesignTime := False;
     // Remove menu items
    FMenuOwner.Free;
     // Release the OTA notifier
    if FOTAServices<>nil then FOTAServices.RemoveNotifier(FOTANotifierIndex);
    inherited Destroy;
  end;

  function TDKLang_Expert.EditConstantsResource: Boolean;
  var
    wsResFileName: UnicodeString;
    ResFile: TDKLang_ResFile;
    ConstantResEntry: TDKLang_ResEntry;
    Consts: TDKLang_Constants;
    bErase: Boolean;

     // Returns file name for constant resource file
    function GetResFileName: UnicodeString;
    begin
      Result := ChangeFileExt(GetActualProject.FileName, SDKLExpt_ConstantResFileEnding)
    end;

  begin
     // Determine the constant resource file name
    wsResFileName := GetResFileName;
     // Create the resource file
    ResFile := TDKLang_ResFile.Create;
    try
       // Load the resource file if it exists
      if FileExists(wsResFileName) then ResFile.LoadFromFile(wsResFileName);
       // Create constant list object
      Consts := TDKLang_Constants.Create(GetLangIDCallback);
      try
         // Try to find the constant resource entry
        ConstantResEntry := ResFile.FindEntry(IntToStr(Integer(RT_RCDATA)), SDKLang_ConstResourceName);
         // If constant resource exists, load the constant list from it
        if ConstantResEntry<>nil then
          Consts.AsRawString := ConstantResEntry.RawData;
        bErase := ConstantResEntry<>nil;
        Result := EditConstants(Consts, bErase);
         // If changes made
        if Result then
           // If user clicked 'Erase'
          if bErase then begin
            if ConstantResEntry<>nil then ResFile.RemoveEntry(ConstantResEntry);
           // Else save the constants back to the resources
          end else begin
             // Create an entry if it didn't exist
            if ConstantResEntry=nil then begin
              ConstantResEntry := TDKLang_ResEntry.Create;
              try
                ConstantResEntry.ResType := IntToStr(Integer(RT_RCDATA));
                ConstantResEntry.Name    := SDKLang_ConstResourceName;
                ResFile.AddEntry(ConstantResEntry);
              except
                ConstantResEntry.Free;
                raise;
              end;
            end;
             // Update the data
            ConstantResEntry.RawData := Consts.AsRawString;
             // Save the resource file
            ResFile.SaveToFile(wsResFileName);
             // Update the project language source file if needed
            if Consts.AutoSaveLangSource and not UpdateProjectLangSource(Consts) then DKLangError(SDKLExptErr_CannotSaveLangSource);
          end;
      finally
        Consts.Free;
      end;
    finally
      ResFile.Free;
    end;
  end;

  procedure TDKLang_Expert.Execute;
  begin
    { stub }
  end;

  function TDKLang_Expert.GetIDString: UnicodeString;
  begin
    Result := 'DKSoftware.DKLang_IDE_Expert';
  end;

  function TDKLang_Expert.GetLangIDCallback: LANGID;
  begin
    Result := GetThreadLocale; // Implicit LCID to LANGID conversion 
  end;

  function TDKLang_Expert.GetName: UnicodeString;
  begin
    Result := 'DKLang IDE Expert';
  end;

  function TDKLang_Expert.GetState: TWizardState;
  begin
    Result := [wsEnabled];
  end;

  procedure TDKLang_Expert.ItemClick_EditConstants(Sender: TObject);
  begin
    EditConstantsResource;
  end;

  procedure TDKLang_Expert.ItemClick_UpdateLangSource(Sender: TObject);
  var
    Proj: IOTAProject;
    i, iMod, iLCUpdated: Integer;
    ModuleInfo: IOTAModuleInfo;
    Module: IOTAModule;
    NTAFormEditor: INTAFormEditor;
  begin
    iLCUpdated := 0;
     // Iterate through project modules to discover form editors
    Proj := GetActualProject;
    for iMod := 0 to Proj.GetModuleCount-1 do begin
      ModuleInfo := Proj.GetModule(iMod);
      if (ModuleInfo.ModuleType=omtForm) and (ModuleInfo.FormName<>'') then begin
        Module := ModuleInfo.OpenModule;
        if Module<>nil then
          for i := 0 to Module.ModuleFileCount-1 do
            if Supports(Module.ModuleFileEditors[i], INTAFormEditor, NTAFormEditor) then begin
              if LC_UpdateProjectLangSource(NTAFormEditor.FormDesigner.Root) then Inc(iLCUpdated);
              Break;
            end;
      end;
    end;
     // Show info
    ShowMessage(Format(SDKLExptMsg_LCsUpdated, [iLCUpdated]));
  end;

  function TDKLang_Expert.NewMenuItem(const wsCaption: UnicodeString; Menu: TMenuItem; AOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(FMenuOwner);
    with Result do begin
      Caption      := wsCaption;
      OnClick      := AOnClick;
    end;
    Menu.Add(Result);
  end;

   //===================================================================================================================
   // TDKLangControllerEditor
   //===================================================================================================================

  procedure TDKLangControllerEditor.ExecuteVerb(Index: Integer);
  var LC: TDKLanguageController;
  begin
    LC := Component as TDKLanguageController;
    case Index of
       // Save the controller's data into project language source file
      0: if not UpdateProjectLangSource(LC) then DKLangError(SDKLExptErr_CannotSaveLangSource);
       // Save the controller's data into selected language source file
      1: 
        with TSaveDialog.Create(nil) do
          try
            DefaultExt := SDKLang_LangSourceExtension;
            Filter     := 'Language source files (*.'+SDKLang_LangSourceExtension+')|*.'+SDKLang_LangSourceExtension+'|All files (*.*)|*.*';
            Options    := [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt, ofPathMustExist];
            Title      := 'Select a language source file';
            if Execute then UpdateLangSourceFile(FileName, LC, []);
          finally
            Free;
          end;
    end;
  end;

  function TDKLangControllerEditor.GetVerb(Index: Integer): UnicodeString;
  begin
    case Index of
      0:   Result := 'Save data to pro&ject language source';
      1:   Result := 'Save data as lan&guage source...';
      else Result := '';
    end;
  end;

  function TDKLangControllerEditor.GetVerbCount: Integer;
  begin
    Result := 2;
  end;

   //===================================================================================================================

  function DKLang_CreateExpert: IOTAWizard;
  begin
    Result := TDKLang_Expert.Create;
  end;

end.
