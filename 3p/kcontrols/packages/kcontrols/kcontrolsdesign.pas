unit kcontrolsdesign;

{$include ..\..\source\kcontrols.inc}

interface

procedure Register;

implementation

{$IFNDEF FPC}
  {$R *.dcr}
{$ENDIF}

uses
  Classes, KControls, KDialogs, KGrids, KHexEditor
{$IFDEF TKDBGRID_USE}
  , KDBGrids
{$ENDIF}
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TK', [
    TKGrid, 
{$IFDEF TKDBGRID_USE}
    TKDBGrid,
{$ENDIF}
    TKHexEditor, 
    TKPrintPreview,
    TKPrintSetupDialog,
    TKPrintPreviewDialog
  ]);
end;

{$IFDEF FPC}
initialization
  {$i kcontrolsdesign.lrs}
{$ENDIF}
end.
