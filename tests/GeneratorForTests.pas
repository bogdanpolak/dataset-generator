unit GeneratorForTests;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB,
  Comp.Generator.DataSetCode;

type
  TDSGeneratorUnderTest = class(TDSGenerator)
  public
    function TestGenCodeLineFieldDefAdd(fld: TField): string;
    function TestGenCodeLineSetFieldValue(fld: TField): string;
    function TestFormatLongStringLiterals(const Literal: string): string;
  end;

implementation

function TDSGeneratorUnderTest.TestFormatLongStringLiterals(
  const Literal: string): string;
begin
  Result := FormatLongStringLiterals(Literal);
end;

function TDSGeneratorUnderTest.TestGenCodeLineFieldDefAdd(fld: TField): string;
begin
  Result := GenCodeLineFieldDefAdd(fld);
end;

function TDSGeneratorUnderTest.TestGenCodeLineSetFieldValue(
  fld: TField): string;
begin
  Result := GenCodeLineSetFieldValue(fld);
end;

end.
