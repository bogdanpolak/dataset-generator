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
    function TestGenUnitHeader(const aUnitName: string): string;
    function TestGenUnitFooter(): string;
    function TestGenerateOneAppend(aFields: TFields): string;
    function TestGenerateAppendsBlock(aDataSet: TDataSet): string;
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

function TDSGeneratorUnderTest.TestGenerateOneAppend(aFields: TFields): string;
begin
  Result := GenerateOneAppend(aFields);
end;

function TDSGeneratorUnderTest.TestGenerateAppendsBlock(aDataSet: TDataSet): string;
begin
  Result := GenerateAppendsBlock(aDataSet);
end;

function TDSGeneratorUnderTest.TestGenUnitHeader(
  const aUnitName: string): string;
begin
  Result := GenerateUnitHeader(aUnitName);
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
