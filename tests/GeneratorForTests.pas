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
    function TestGenUnitHeader: string;
    function TestGenUnitFooter: string;
    function TestGenerateOneAppend: string;
    function TestGenerateAppendsBlock: string;
    function TestGenFunction: string;
  end;

implementation

function TDSGeneratorUnderTest.TestFormatLongStringLiterals(
  const Literal: string): string;
begin
  Result := FormatLongStringLiterals(Literal);
end;

function TDSGeneratorUnderTest.TestGenCodeLineFieldDefAdd(fld: TField): string;
begin
  Result := GenerateLine_FieldDefAdd(fld);
end;

function TDSGeneratorUnderTest.TestGenCodeLineSetFieldValue(
  fld: TField): string;
begin
  Result := GenerateLine_SetFieldValue(fld);
end;

function TDSGeneratorUnderTest.TestGenerateOneAppend: string;
begin
  Result := GenerateOneAppend;
end;

function TDSGeneratorUnderTest.TestGenerateAppendsBlock: string;
begin
  Result := GenerateAppendsBlock;
end;

function TDSGeneratorUnderTest.TestGenUnitHeader: string;
begin
  Result := GenerateUnitHeader;
end;

function TDSGeneratorUnderTest.TestGenFunction: string;
begin
  Result := GenerateFunction;
end;

function TDSGeneratorUnderTest.TestGenUnitFooter: string;
begin
  Result := GenerateUnitFooter;
end;

end.
