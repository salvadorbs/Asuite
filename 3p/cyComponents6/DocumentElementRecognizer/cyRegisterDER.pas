unit cyRegisterDER;

{$I ..\Core\cyCompilerDefines.inc}

interface

Uses
  Classes,

  {$IFDEF DELPHI6_OR_ABOVE}
  DesignEditors, DesignIntf,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}

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

  procedure Register;

implementation

uses cyformAbout, cyDsnResource, cyDocER;

procedure TcyDefaultEditor.Edit;
begin
  Inherited Edit;
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

procedure Register;
begin
  RegisterComponents(RSDERPalette, [TcyDocER]);
  RegisterComponentEditor(TcyDocER, TcyDefaultEditor);
end;

end.
