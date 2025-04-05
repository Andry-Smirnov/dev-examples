{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazXProj;

interface

uses
  LazXProj_Intf, NewLazXProj, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazXProj_Intf', @LazXProj_Intf.Register);
end;

initialization
  RegisterPackage('LazXProj', @Register);
end.
