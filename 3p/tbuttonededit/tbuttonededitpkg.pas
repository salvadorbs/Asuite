{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TButtonedEditpkg; 

interface

uses
  ButtonedEdit, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ButtonedEdit', @ButtonedEdit.Register); 
end; 

initialization
  RegisterPackage('TButtonedEditpkg', @Register); 
end.
