{   * ***** BEGIN LICENSE BLOCK *****
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

unit cyDBRegister;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses
  Classes,

  {$IFDEF DELPHI6_OR_ABOVE}
  DesignEditors, DesignIntf,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}

  // Visual components
  cyDBLabel, cyDBHotLabel, cyDBEdit, cyDBLed, cyDBAdvLed, cyDBRadioButton, cyDBSpeedButton, cyDBAdvSpeedButton, cyDBSimpleGauge, cyDBGrid,
  cyDBAdvGrid,

  // Non visual components
  cyBookmarks, cyFieldLink,


  Menus;



type
  TcyDefaultEditor = class(TDefaultEditor) // class(TComponentEditor)
  public
    { Public declarations }
    procedure Edit; override;  // On double click the component ...
    procedure ExecuteVerb(Index: Integer); override;  // Called when MenuItem was clicked by developper ...
    function GetVerb(Index: Integer): string; override;  // Inform Delphi the captions to appear in the context menu.
    function GetVerbCount: Integer; override;  // Inform Delphi how many items we want to add to the context menu ...
//    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;  // before appearing the contextMenu, we can Show/hide menuItems but we can't destroy them ...
  end;

{  TcyDBGridEditor = class(TcyDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;  }

procedure Register;

implementation

uses cyformAbout, cyDsnResource;

procedure TcyDefaultEditor.Edit;
var Handled: Boolean;
begin
  Handled := false;

  if not Handled
  then Inherited Edit;
end;

procedure TcyDefaultEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1   // Last one ...
  then begin
    CindyDesignAboutForm;
    // Tell to the IDE that something changed!
    //  Designer.Modified;
  end
  else
    Inherited ExecuteVerb(Index);
end;

function TcyDefaultEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1   // Last one ...
  then Result := RSAboutName
  else Result := Inherited GetVerb(Index);
end;

function TcyDefaultEditor.GetVerbCount: Integer;
begin
  Result := Inherited GetVerbCount + 1;  // We add one MenuItem ...
end;

{procedure TcyDefaultEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
begin
  case Index of
    0: AItem.Enabled := true;
  end;

  // Tell to the IDE that something changed!
  Designer.Modified;
end;}

{procedure TcyDBGridEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1   // Last one ...
  then begin
    // Edit columns :

  end
  else
    Inherited ExecuteVerb(Index);
end;

function TcyDBGridEditor.GetVerb(Index: Integer): string;
begin
  if Index <> GetVerbCount - 1   // Last one ...
  then Result := 'Columns ...'
  else Result := Inherited GetVerb(Index);
end;

function TcyDBGridEditor.GetVerbCount: Integer;
begin
  Result := Inherited GetVerbCount + 1;  // We add one MenuItem ...
end;            }

procedure Register;
begin
  // Visual components :
  RegisterComponents(RSVCLDBCompsPalette, [
    TcyDBLabel,
    TcyDBHotLabel,
    TcyDBEdit,
    TcyDBLed,
    TcyDBAdvLed,
    TcyDBRadioButton,
    TcyDBSpeedButton,
    TcyDBAdvSpeedButton,
    TcyDBSimpleGauge,
    TcyDBGrid,
    TcyDBAdvGrid
    ]);

    RegisterComponentEditor(TcyDBLabel, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBHotLabel, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBEdit, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBLed, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBAdvLed, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBRadioButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBSpeedButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBAdvSpeedButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyDBSimpleGauge, TcyDefaultEditor);
//    RegisterComponentEditor(TcyDBGrid, TcyDefaultEditor);
//    RegisterComponentEditor(TcyDBAdvGrid, TcyDefaultEditor);

  // Register Non-Visual components :
  RegisterComponents(RSVCLDBCompsPalette, [
    TcyBookmarks,
    TcyFieldLink
    ]);

    RegisterComponentEditor(TcyBookmarks, TcyDefaultEditor);
    RegisterComponentEditor(TcyFieldLink, TcyDefaultEditor);
end;

end.
