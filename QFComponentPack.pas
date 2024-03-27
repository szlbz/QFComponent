{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit QFComponentPack;

{$warn 5023 off : no warning about unused units}
interface

uses
  QFComponent, QFRichEdit, setfontcolor, SetFontSize, SetImage, SetTable, 
  Tools, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('QFComponent', @QFComponent.Register);
end;

initialization
  RegisterPackage('QFComponentPack', @Register);
end.
