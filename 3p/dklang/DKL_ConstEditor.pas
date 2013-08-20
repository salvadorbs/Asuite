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
// Designtime project constant editor dialog declaration
//
unit DKL_ConstEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DKLang,
  StdCtrls, Grids;

type
  TdDKL_ConstEditor = class(TForm)
    bCancel: TButton;
    bErase: TButton;
    bLoad: TButton;
    bOK: TButton;
    bSave: TButton;
    cbSaveToLangSource: TCheckBox;
    gMain: TStringGrid;
    lCount: TLabel;
    lDeleteHint: TLabel;
    procedure bEraseClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure gMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure gMainSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
  private
     // The constants being edited
    FConsts: TDKLang_Constants;
     // True if the constants are to be erased from the project resources
    FErase: Boolean;
     // Initializes the dialog
    procedure InitializeDialog(AConsts: TDKLang_Constants; bEraseAllowed: Boolean);
     // Updates the count info
    procedure UpdateCount;
     // Storing/restoring the settings
    procedure SaveSettings;
    procedure LoadSettings;
     // Updates gMain columns' widths so that they both fit into the client area
    procedure UpdateGridColumnWidths;
     // Raises an exception if entry (constant) index is not valid
    procedure CheckEntryIndexValidity(iIndex: Integer);
     // Ensures a virtual row is available at the end of the table
    procedure EnsureVirtualRowExists;
     // Returns True if the specified row has neither name nor value
    function  IsRowEmpty(iRow: Integer): Boolean;
     // Raises an exception if constant names are not valid (includes uniqueness checking)
    procedure CheckNamesValid;
     // Deletes the specified entry
    procedure DeleteEntry(iIndex: Integer);
     // Prop handlers
    function  GetEntryCount: Integer;
    function  GetEntryNames(Index: Integer): UnicodeString;
    function  GetEntryValues(Index: Integer; bEncoded: Boolean): UnicodeString;
    procedure SetEntryCount(iCount: Integer);
    procedure SetEntryNames(Index: Integer; const wsValue: UnicodeString);
    procedure SetEntryValues(Index: Integer; bEncoded: Boolean; const wsValue: UnicodeString);
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    procedure Resize; override;
  public
     // Props
     // -- Entry (constant) count
    property EntryCount: Integer read GetEntryCount write SetEntryCount;
     // -- Constant names by index
    property EntryNames[Index: Integer]: UnicodeString read GetEntryNames write SetEntryNames;
     // -- Constant names by index. If bEncoded=True, the constant value is represented 'encoded', with no literal
     //    control chars; if bEncoded=False, the value is represented 'as is', with linebreaks, tabs, etc. in it
    property EntryValues[Index: Integer; bEncoded: Boolean]: UnicodeString read GetEntryValues write SetEntryValues;
  end;

const
  SRegKey_DKLangConstEditor = 'Software\DKSoftware\DKLang\ConstEditor';

   // Show constant editor dialog
   //   AConsts       - The constants being edited
   //   bEraseAllowed - Entry: is erase allowed (ie constant resource exists); return: True if user has pressed Erase
   //                   button
  function EditConstants(AConsts: TDKLang_Constants; var bEraseAllowed: Boolean): Boolean;

implementation
{$R *.dfm}
uses Registry;

const
   // gMain's column indexes
  IColIdx_Name  = 0;
  IColIdx_Value = 1;

  function EditConstants(AConsts: TDKLang_Constants; var bEraseAllowed: Boolean): Boolean;
  begin
    with TdDKL_ConstEditor.Create(Application) do
      try
        InitializeDialog(AConsts, bEraseAllowed);
        Result := ShowModal=mrOK;
        bEraseAllowed := FErase;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdDKL_ConstEditor
   //===================================================================================================================

  procedure TdDKL_ConstEditor.bEraseClick(Sender: TObject);
  begin
    if Application.MessageBox('Are you sure you want to delete the constants from project resources?', 'Confirm', MB_ICONEXCLAMATION or MB_OKCANCEL)=IDOK then begin
      FErase := True;
      ModalResult := mrOK;
    end;
  end;

  procedure TdDKL_ConstEditor.bLoadClick(Sender: TObject);

    procedure DoLoad(const wsFileName: UnicodeString);
    var
      SL: TStringList;
      i: Integer;
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(wsFileName);
        EntryCount := SL.Count;
        for i := 0 to SL.Count-1 do begin
          EntryNames [i]       := SL.Names[i];
          EntryValues[i, True] := SL.ValueFromIndex[i]; // Assume the value is already encoded in the file
        end;
      finally
        SL.Free;
      end;
    end;

  begin
    with TOpenDialog.Create(Self) do
      try
        DefaultExt := 'txt';
        Filter     := 'All files (*.*)|*.*';
        Options    := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        Title      := 'Select a text file to load from';
        if Execute then DoLoad(FileName);
      finally
        Free;
      end;
  end;

  procedure TdDKL_ConstEditor.bOKClick(Sender: TObject);
  var i: Integer;
  begin
     // Check that all names are valid
    CheckNamesValid;
     // Copy the constans from the editor back into FConsts
    FConsts.Clear;
    FConsts.AutoSaveLangSource := cbSaveToLangSource.Checked;
    for i := 0 to EntryCount-1 do FConsts.Add(EntryNames[i], EntryValues[i, False], []);
    ModalResult := mrOK;
  end;

  procedure TdDKL_ConstEditor.bSaveClick(Sender: TObject);

    procedure DoSave(const wsFileName: UnicodeString);
    var
      SL: TStringList;
      i: Integer;
    begin
      SL := TStringList.Create;
      try
        for i := 0 to EntryCount-1 do SL.Add(EntryNames[i]+'='+EntryValues[i, True]);
        SL.SaveToFile(wsFileName);
      finally
        SL.Free;
      end;
    end;

  begin
    with TSaveDialog.Create(Self) do
      try
        DefaultExt := 'txt';
        Filter     := 'All files (*.*)|*.*';
        Options    := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        Title      := 'Select a text file to save to';
        if Execute then DoSave(FileName);
      finally
        Free;
      end;
  end;

  procedure TdDKL_ConstEditor.CheckEntryIndexValidity(iIndex: Integer);
  begin
    if (iIndex<0) or (iIndex>=EntryCount) then DKLangError('Invalid entry index (%d)', [iIndex]);
  end;

  procedure TdDKL_ConstEditor.CheckNamesValid;
  var
    SL: TStringList;
    ws: UnicodeString;
    i: Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Sorted := True;
      for i := 0 to EntryCount-1 do begin
        ws := EntryNames[i];
        if ws='' then DKLangError('Constant name cannot be empty');
        if not IsValidIdent(ws) then DKLangError('Invalid constant name: "%s"', [ws]);
        if SL.IndexOf(ws)<0 then SL.Add(ws) else DKLangError('Duplicate constant name: "%s"', [ws]);
      end;
    finally
      SL.Free;
    end;
  end;

  procedure TdDKL_ConstEditor.DeleteEntry(iIndex: Integer);
  var i: Integer;
  begin
    CheckEntryIndexValidity(iIndex);
     // Shift the grid contents
    for i := iIndex to EntryCount-2 do begin
      EntryNames [i]       := EntryNames [i+1];
      EntryValues[i, True] := EntryValues[i+1, True];
    end;
     // Remove the last row
    EntryCount := EntryCount-1;  
  end;

  procedure TdDKL_ConstEditor.DoClose(var Action: TCloseAction);
  begin
    inherited DoClose(Action);
    SaveSettings;
  end;

  procedure TdDKL_ConstEditor.DoShow;
  begin
    inherited DoShow;
    LoadSettings;
  end;

  procedure TdDKL_ConstEditor.EnsureVirtualRowExists;
  var i: Integer;
  begin
     // Determine the index of last non-empty row
    i := gMain.RowCount-1;
    while (i>0) and IsRowEmpty(i) do Dec(i);
     // Set the number of rows
    EntryCount := i;
  end;

  function TdDKL_ConstEditor.GetEntryCount: Integer;
  begin
    Result := gMain.RowCount-2; // One for the header, one more for the virtual row
  end;

  function TdDKL_ConstEditor.GetEntryNames(Index: Integer): UnicodeString;
  begin
    CheckEntryIndexValidity(Index);
    Result := Trim(gMain.Cells[IColIdx_Name, Index+1]); // One more row to skip the header
  end;

  function TdDKL_ConstEditor.GetEntryValues(Index: Integer; bEncoded: Boolean): UnicodeString;
  begin
    CheckEntryIndexValidity(Index);
    Result := Trim(gMain.Cells[IColIdx_Value, Index+1]); // One more row to skip the header
    if not bEncoded then Result := DecodeControlChars(Result);
  end;

  procedure TdDKL_ConstEditor.gMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if (Key=VK_DELETE) and (Shift=[ssCtrl]) and (gMain.Row<gMain.RowCount-1) then begin      
      DeleteEntry(gMain.Row-1);
      Key := 0;
    end;
  end;

  procedure TdDKL_ConstEditor.gMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
     // Believe mouse up is linked to column resizing...
    UpdateGridColumnWidths;
  end;

  procedure TdDKL_ConstEditor.gMainSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
  begin
    EnsureVirtualRowExists;
    CheckNamesValid;
    UpdateCount;
  end;

  procedure TdDKL_ConstEditor.InitializeDialog(AConsts: TDKLang_Constants; bEraseAllowed: Boolean);
  var i: Integer;
  begin
    FConsts                    := AConsts;
    cbSaveToLangSource.Checked := FConsts.AutoSaveLangSource;
    bErase.Enabled             := bEraseAllowed;
    FErase                     := False;
     // Setup the editor
    gMain.Cells[IColIdx_Name,  0] := 'Constant name';
    gMain.Cells[IColIdx_Value, 0] := 'Constant value';
     // Copy the constans into the editor
    EntryCount := FConsts.Count;
    for i := 0 to FConsts.Count-1 do begin
      EntryNames [i]        := FConsts[i].wsName;
      EntryValues[i, False] := FConsts[i].wsValue;
    end;
     // Update count info
    UpdateCount;
  end;

  function TdDKL_ConstEditor.IsRowEmpty(iRow: Integer): Boolean;
  begin
    Result := (Trim(gMain.Cells[IColIdx_Name, iRow])='') and (Trim(gMain.Cells[IColIdx_Value, iRow])='');
  end;

  procedure TdDKL_ConstEditor.LoadSettings;
  var
    rif: TRegIniFile;
    rBounds: TRect;
  begin
    rif := TRegIniFile.Create(SRegKey_DKLangConstEditor);
    try
       // Restore form bounds
      rBounds := Rect(
        rif.ReadInteger('', 'Left',   MaxInt),
        rif.ReadInteger('', 'Top',    MaxInt),
        rif.ReadInteger('', 'Right',  MaxInt),
        rif.ReadInteger('', 'Bottom', MaxInt));
       // If all the coords are valid
      if (rBounds.Left<MaxInt) and (rBounds.Top<MaxInt) and (rBounds.Right<MaxInt) and (rBounds.Bottom<MaxInt) then
        BoundsRect := rBounds;
       // Load other settings
      gMain.ColWidths[IColIdx_Name] := rif.ReadInteger('', 'NameColWidth', gMain.ClientWidth div 2);
      UpdateGridColumnWidths;
    finally
      rif.Free;
    end;
  end;

  procedure TdDKL_ConstEditor.Resize;
  begin
    inherited Resize;
    UpdateGridColumnWidths;
  end;

  procedure TdDKL_ConstEditor.SaveSettings;
  var
    rif: TRegIniFile;
    rBounds: TRect;
  begin
    rif := TRegIniFile.Create(SRegKey_DKLangConstEditor);
    try
       // Store form bounds
      rBounds := BoundsRect;
      rif.WriteInteger('', 'Left',         rBounds.Left);
      rif.WriteInteger('', 'Top',          rBounds.Top);
      rif.WriteInteger('', 'Right',        rBounds.Right);
      rif.WriteInteger('', 'Bottom',       rBounds.Bottom);
       // Store other settings
      rif.WriteInteger('', 'NameColWidth', gMain.ColWidths[IColIdx_Name]);
    finally
      rif.Free;
    end;
  end;

  procedure TdDKL_ConstEditor.SetEntryCount(iCount: Integer);
  begin
    gMain.RowCount := iCount+2; // One for the header, one more for the virtual row
     // Cleanup the virtual row
    gMain.Cells[IColIdx_Name,  iCount+1] := '';
    gMain.Cells[IColIdx_Value, iCount+1] := '';
  end;

  procedure TdDKL_ConstEditor.SetEntryNames(Index: Integer; const wsValue: UnicodeString);
  begin
    CheckEntryIndexValidity(Index);
    gMain.Cells[IColIdx_Name, Index+1] := wsValue; // One more row to skip the header
  end;

  procedure TdDKL_ConstEditor.SetEntryValues(Index: Integer; bEncoded: Boolean; const wsValue: UnicodeString);
  var ws: UnicodeString;
  begin
    CheckEntryIndexValidity(Index);
    ws := wsValue;
    if not bEncoded then ws := EncodeControlChars(ws);
    gMain.Cells[IColIdx_Value, Index+1] := ws; // One more row to skip the header
  end;

  procedure TdDKL_ConstEditor.UpdateCount;
  begin
    lCount.Caption := Format('%d constants', [EntryCount]);
  end;

  procedure TdDKL_ConstEditor.UpdateGridColumnWidths;
  var iwClient, iwName: Integer;
  begin
    iwClient := gMain.ClientWidth;
    iwName   := gMain.ColWidths[IColIdx_Name];
     // Do not allow columns be narrower than 20 pixels
    if iwName<20 then iwName := 20
    else if iwName>iwClient-20 then iwName := iwClient-22;
     // Update column widths
    gMain.ColWidths[IColIdx_Name]  := iwName;
    gMain.ColWidths[IColIdx_Value] := iwClient-iwName-2;
  end;

end.
