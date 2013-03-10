{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit KControlsLaz;

interface

uses
  KFunctions, KGraphics, KControls, KDialogs, KEditCommon, KGrids, KHexEditor, 
  KIcon, KPrintPreview, KPrintSetup, KWideWinProcs, kcontrolsdesign, KDBGrids, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('kcontrolsdesign', @kcontrolsdesign.Register);
end;

initialization
  RegisterPackage('KControlsLaz', @Register);
end.
