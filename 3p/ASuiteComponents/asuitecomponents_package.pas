{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit asuitecomponents_package;

interface

uses
  ASuiteControls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ASuiteControls', @ASuiteControls.Register);
end;

initialization
  RegisterPackage('asuitecomponents_package', @Register);
end.
