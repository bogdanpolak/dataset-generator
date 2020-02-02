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
    // ----
    function _FormatLongStringLiterals(const Literal: string): string;
    // ----
    function _GenerateLine_FieldDefAdd(fld: TField): string;
    function _GenerateLine_SetFieldValue(fld: TField): string;
    function _GenerateUnitHeader: string;
    function _GenerateUnitFooter: string;
    function _GenerateOneAppend: string;
    function _GenerateAppendsBlock: string;
    function _GenerateFunction: string;
  end;

implementation

function TDSGeneratorUnderTest._FormatLongStringLiterals(
  const Literal: string): string;
begin
  Result := FormatLongStringLiterals(Literal);
end;

function TDSGeneratorUnderTest._GenerateLine_FieldDefAdd(fld: TField): string;
begin
  Result := GenerateLine_FieldDefAdd(fld);
end;

function TDSGeneratorUnderTest._GenerateLine_SetFieldValue(
  fld: TField): string;
begin
  Result := GenerateLine_SetFieldValue(fld);
end;

function TDSGeneratorUnderTest._GenerateUnitHeader: string;
begin
  Result := GenerateUnitHeader;
end;

function TDSGeneratorUnderTest._GenerateUnitFooter: string;
begin
  Result := GenerateUnitFooter;
end;

function TDSGeneratorUnderTest._GenerateOneAppend: string;
begin
  Result := GenerateOneAppend;
end;

function TDSGeneratorUnderTest._GenerateAppendsBlock: string;
begin
  Result := GenerateAppendsBlock;
end;

function TDSGeneratorUnderTest._GenerateFunction: string;
begin
  Result := GenerateFunction;
end;

end.
