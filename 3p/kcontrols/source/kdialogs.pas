{ @abstract(This unit contains all dialogs supplied with KControls.)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(18 Sep 2009)
  @lastmod(14 Oct 2009)

  This unit implements all dialogs supplied with KControls Development Suite.

  Copyright © 2009 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KDialogs;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, Controls, Forms, KControls, KPrintPreview, KPrintSetup;

type
  { @abstract(Encapsulates the print preview dialog) }
  TKPrintPreviewDialog = class(TComponent)
  private
    FControl: TKCustomControl;
    FPrintPreviewForm: TKPrintPreviewForm;
    function GetPrintPreviewForm: TKPrintPreviewForm;
  public
    { Creates the instance. Assigns default values to properties. }
    constructor Create(AOwner: TComponent); override;
    { Shows the dialog. }
    procedure Show;
    { Shows the dialog as modal dialog. }
    function Execute: Boolean;
    { Specifies the associated preview form. }
    property PrintPreviewForm: TKPrintPreviewForm read GetPrintPreviewForm;
  published
    { Specifies the associated control. }
    property Control: TKCustomControl read FControl write FControl;
  end;

  { @abstract(Encapsulates the print preview dialog) }
  TKPrintSetupDialog = class(TComponent)
  private
    FControl: TKCustomControl;
    FPrintSetupForm: TKPrintSetupForm;
    FPreviewDialog: TKPrintPreviewDialog;
    FSelAvail: Boolean;
  public
    { Creates the instance. Assigns default values to properties. }
    constructor Create(AOwner: TComponent); override;
    { Shows the dialog as modal dialog. }
    function Execute: Boolean;
  published
    { Specifies the associated control. }
    property Control: TKCustomControl read FControl write FControl;
    { Specifies the preview dialog for the Preview... button.
      If not specified, the print setup dialog creates a new one. }
    property PreviewDialog: TKPrintPreviewDialog read FPreviewDialog write FPreviewDialog;
    { If True, the Selection Only option will be checked (if selection is available
      for the control). }
    property SelAvail: Boolean read FSelAvail write FSelAvail default True;
  end;

implementation

{ TKPrintPreviewDialog }

constructor TKPrintPreviewDialog.Create(AOwner: TComponent);
begin
  inherited;
  FPrintPreviewForm := nil;
  FControl := nil;
end;

function TKPrintPreviewDialog.Execute;
begin
  PrintPreviewForm.Preview.Control := FControl;
  PrintPreviewForm.ShowModal;
  Result := True;
end;

function TKPrintPreviewDialog.GetPrintPreviewForm: TKPrintPreviewForm;
begin
  if not Assigned(FPrintPreviewForm) then
    FPrintPreviewForm := TKPrintPreviewForm.Create(Self);
  Result := FPrintPreviewForm;
end;

procedure TKPrintPreviewDialog.Show;
begin
  PrintPreviewForm.Preview.Control := FControl;
  PrintPreviewForm.Show;
end;

{ TKPrintSetupDialog }

constructor TKPrintSetupDialog.Create(AOwner: TComponent);
begin
  inherited;
  FControl := nil;
  FPrintSetupForm := nil;
  FPreviewDialog := nil;
  FSelAvail := True;
end;

function TKPrintSetupDialog.Execute: Boolean;
begin
  if Assigned(FControl) then
  begin
    if not Assigned(FPrintSetupForm) then
      FPrintSetupForm := TKPrintSetupForm.Create(Self);
    FPrintSetupForm.PageSetup := FControl.PageSetup;
    if Assigned(FPreviewDialog) then
      FPrintSetupForm.PreviewForm := FPreviewDialog.PrintPreviewForm;
    FPrintSetupForm.SelAvail := FSelAvail;
    Result := FPrintSetupForm.ShowModal = mrOk;
  end else
    Result := False;
end;

end.
