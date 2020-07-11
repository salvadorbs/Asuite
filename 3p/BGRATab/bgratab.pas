{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BGRATab;

{$warn 5023 off : no warning about unused units}
interface

uses
  BCImageTab, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BCImageTab', @BCImageTab.Register);
end;

initialization
  RegisterPackage('BGRATab', @Register);
end.
