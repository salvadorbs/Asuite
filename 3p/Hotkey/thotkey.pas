{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit THotKey;

{$warn 5023 off : no warning about unused units}
interface

uses
  HotKey, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('HotKey', @HotKey.Register);
end;

initialization
  RegisterPackage('THotKey', @Register);
end.
