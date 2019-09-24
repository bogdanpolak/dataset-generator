unit Test.CodeWithAppendData;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode,
  Test.Common;

{$M+}

type

  [TestFixture]
  TTestCodeWithAppendData = class(TTestGenerate)
  private
    procedure AreEqual_TextTemplate_And_GeneratedCode
      (const TextTemplate: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure TestLongStringLiterals_iss002;
    // -------------
    procedure TestOneBCDField_iss001;
    procedure TestOneBCDField_DifferentFieldName;
    // -------------
    procedure TestOneIntegerField;
    procedure TestOneWideStringField;
    procedure TestOneDateTimeField_DateOnly;
    procedure TestOneDateTimeField_DateTime;
    // -------------
    procedure Test_IndentationText_BCDField;
    procedure Test_Indentation_MultilineTextValue;
    procedure Test_Indentation_Empty;
    procedure Test_Indentation_1Space;
    // -------------
    procedure TestSample1;
  end;

implementation

uses
  System.Variants,
  Data.FmtBcd;

// -----------------------------------------------------------------------
// Utils section
// -----------------------------------------------------------------------

procedure TTestCodeWithAppendData.AreEqual_TextTemplate_And_GeneratedCode
  (const TextTemplate: string);
var
  sExpected: string;
  sActual: string;
begin
  sExpected := ReplaceArrowsAndDiamonds(TextTemplate);
  Self.GenerateDataSetCode.DataSet := Self.mockDataSet;
  Self.GenerateDataSetCode.Execute;
  sActual := Self.GenerateDataSetCode.CodeWithAppendData.Text;
  Assert.AreEqual(sExpected, sActual);
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TTestCodeWithAppendData.Setup;
begin
  Common_Setup;
end;

procedure TTestCodeWithAppendData.TearDown;
begin
  Common_TearDown;
end;

// -----------------------------------------------------------------------
// Templates
// -----------------------------------------------------------------------

const
  CodeTemplateOnePrecisionField =
  (* *) '◇ds := TFDMemTable.Create(AOwner);→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇with FieldDefs.AddFieldDef do begin→' +
  (* *) '◇◇◇Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;→' +
  (* *) '◇◇end;→' +
  (* *) '◇◇CreateDataSet;→' +
  (* *) '◇end;→' +
  (* *) '{$REGION ''Append data to MemTable''}→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''%s'').Value := %s;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '{$ENDREGION}→';

const
  CodeTemplateOneField =
  (* *) '◇ds := TFDMemTable.Create(AOwner);→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇FieldDefs.Add(''f1'', %s);→' +
  (* *) '◇◇CreateDataSet;→' +
  (* *) '◇end;→' +
  (* *) '{$REGION ''Append data to MemTable''}→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''f1'').Value := %s;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '{$ENDREGION}→';


  // -----------------------------------------------------------------------
  // Tests for: Registered issues (bugs)
  // -----------------------------------------------------------------------
{$REGION 'Registered issues (bugs)'}

procedure TTestCodeWithAppendData.TestLongStringLiterals_iss002;
var
  aExpectedCode: string;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftWideString, 300);
    CreateDataSet;
    AppendRecord(['Covers Dependency Injection, you''ll learn about' +
      ' Constructor Injection, Property Injection, and Method Injection' +
      ' and about the right and wrong way to use it']);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField,
    ['ftWideString, 300',
    '→◇◇◇' + QuotedStr
    ('Covers Dependency Injection, you''ll learn about Constructor Injecti') +
    '+→◇◇◇' + QuotedStr
    ('on, Property Injection, and Method Injection and about the right and') +
    '+→◇◇◇' + QuotedStr(' wrong way to use it')]);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: One BCD field with one value
// -----------------------------------------------------------------------
{$REGION 'One BCD field with one value'}

procedure TTestCodeWithAppendData.TestOneBCDField_DifferentFieldName;
var
  sExpected: string;
begin
  with mockDataSet do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := 'abc123';
      DataType := ftBcd;
      Precision := 8;
      Size := 2;
    end;
    CreateDataSet;
    AppendRecord([1.01]);
    First;
  end;
  sExpected := Format(CodeTemplateOnePrecisionField,
    ['abc123', 'ftBCD', 8, 2, 'abc123', '1.01']);
  AreEqual_TextTemplate_And_GeneratedCode(sExpected);
end;

procedure TTestCodeWithAppendData.TestOneBCDField_iss001;
var
  sExpected: string;
begin
  with mockDataSet do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := 'f1';
      DataType := ftBcd;
      Precision := 10;
      Size := 4;
    end;
    CreateDataSet;
    AppendRecord([16.25]);
    First;
  end;
  sExpected := Format(CodeTemplateOnePrecisionField,
    ['f1', 'ftBCD', 10, 4, 'f1', '16.25']);
  AreEqual_TextTemplate_And_GeneratedCode(sExpected);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------
{$REGION 'One DB field with one value'}

procedure TTestCodeWithAppendData.TestOneDateTimeField_DateOnly;
var
  aExpectedCode: string;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01)]);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField,
    ['ftDateTime', 'EncodeDate(2019,7,1)']);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

procedure TTestCodeWithAppendData.TestOneDateTimeField_DateTime;
var
  aExpectedCode: string;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01) + EncodeTime(15, 07, 30, 500)]);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField,
    ['ftDateTime', 'EncodeDate(2019,7,1)+EncodeTime(15,7,30,500)']);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

procedure TTestCodeWithAppendData.TestOneIntegerField;
var
  aExpectedCode: string;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField, ['ftInteger', '1']);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

procedure TTestCodeWithAppendData.TestOneWideStringField;
var
  aExpectedCode: string;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftWideString, 20);
    CreateDataSet;
    AppendRecord(['Alice has a cat']);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField,
    ['ftWideString, 20', QuotedStr('Alice has a cat')]);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------
{$REGION 'property IndentationText'}

procedure TTestCodeWithAppendData.Test_Indentation_1Space;
var
  aExpectedCode: string;
begin
  GenerateDataSetCode.IndentationText := ' ';
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField, ['ftInteger', '1']);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

procedure TTestCodeWithAppendData.Test_Indentation_Empty;
var
  aExpectedCode: string;
begin
  GenerateDataSetCode.IndentationText := '';
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  aExpectedCode := Format(CodeTemplateOneField, ['ftInteger', '1']);
  AreEqual_TextTemplate_And_GeneratedCode(aExpectedCode);
end;

procedure TTestCodeWithAppendData.Test_Indentation_MultilineTextValue;
var
  FieldDefsParams: string;
  FieldValue: string;
  sExpected: string;
begin
  GenerateDataSetCode.IndentationText := '  ';
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftWideString, 300);
    CreateDataSet;
    AppendRecord(['Covers Dependency Injection, you''ll learn about' +
      ' Constructor Injection, Property Injection, and Method Injection' +
      ' and about the right and wrong way to use it']);
    First;
  end;
  FieldDefsParams := 'ftWideString, 300';
  FieldValue := '→◇◇◇' + QuotedStr
    ('Covers Dependency Injection, you''ll learn about Constructor Injecti') +
    '+→◇◇◇' + QuotedStr
    ('on, Property Injection, and Method Injection and about the right and') +
    '+→◇◇◇' + QuotedStr(' wrong way to use it');
  sExpected := Format(CodeTemplateOneField, [FieldDefsParams, FieldValue]);
  AreEqual_TextTemplate_And_GeneratedCode(sExpected);
end;

procedure TTestCodeWithAppendData.Test_IndentationText_BCDField;
var
  sExpected: string;
begin
  with mockDataSet do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := 'xyz123';
      DataType := ftBcd;
      Precision := 8;
      Size := 2;
    end;
    CreateDataSet;
    AppendRecord([1.01]);
    First;
  end;
  sExpected := Format(CodeTemplateOnePrecisionField,
    ['xyz123', 'ftBCD', 8, 2, 'xyz123', '1.01']);
  GenerateDataSetCode.IndentationText := '  ';
  AreEqual_TextTemplate_And_GeneratedCode(sExpected);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: Sample1
// -----------------------------------------------------------------------
{$REGION 'Sample1 : dataset with 4 fields and 2 rows containing NULL values'}

procedure TTestCodeWithAppendData.TestSample1;
var
  expectedCode: string;
begin
  expectedCode :=
  (* *) '◇ds := TFDMemTable.Create(AOwner);→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇FieldDefs.Add(''id'', ftInteger);→' +
  (* *) '◇◇FieldDefs.Add(''text1'', ftWideString, 30);→' +
  (* *) '◇◇FieldDefs.Add(''date1'', ftDate);→' +
  (* *) '◇◇FieldDefs.Add(''float1'', ftFloat);→' +
  (* *) '◇◇FieldDefs.Add(''currency1'', ftCurrency);→' +
  (* *) '◇◇CreateDataSet;→' +
  (* *) '◇end;→' +
  (* *) '{$REGION ''Append data to MemTable''}→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''id'').Value := 1;→' +
  (* *) '◇◇FieldByName(''text1'').Value := ''Ala ma kota'';→' +
  (* *) '◇◇FieldByName(''date1'').Value := EncodeDate(2019,9,16);→' +
  (* *) '◇◇FieldByName(''float1'').Value := 1.2;→' +
  (* *) '◇◇FieldByName(''currency1'').Value := 1200;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''id'').Value := 2;→' +
  (* *) '◇◇FieldByName(''text1'').Value := ''Ala ma kota'';→' +
  (* *) '◇◇FieldByName(''currency1'').Value := 950;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '{$ENDREGION}→';
  with mockDataSet do
  begin
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    FieldDefs.Add('date1', ftDate);
    FieldDefs.Add('float1', ftFloat);
    FieldDefs.Add('currency1', ftCurrency);
    CreateDataSet;
    AppendRecord([1, 'Ala ma kota', EncodeDate(2019, 09, 16), 1.2, 1200]);
    AppendRecord([2, 'Ala ma kota', System.Variants.Null, Null, 950]);
    First;
  end;
  AreEqual_TextTemplate_And_GeneratedCode(expectedCode);
end;

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(TTestCodeWithAppendData);

end.
