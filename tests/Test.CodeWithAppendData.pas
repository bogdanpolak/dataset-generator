unit Test.CodeWithAppendData;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode;

{$M+}

type

  [TestFixture]
  TestGenerateAppends = class(TObject)
  private
    fGenerator: TDSGenerator;
    fOwner: TComponent;
    fExpectedCode: TStringList;
    function ReplaceArrowsAndDiamonds(const s: String): string;
    procedure Assert_AreCodesEqual(const expectedCode: string;
      const actualCode: string);
    function GivenSampleDataSetWithTwoRows(aOwner: TComponent): TDataSet;
    function Given_DataSet_With300String(aOwner: TComponent): TDataSet;
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

function TestGenerateAppends.ReplaceArrowsAndDiamonds(const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', fGenerator.IndentationText,
    [rfReplaceAll])
end;

procedure TestGenerateAppends.Assert_AreCodesEqual(const expectedCode: string;
  const actualCode: string);
begin
  Assert.AreEqual(expectedCode, actualCode);
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Setup;
begin
  fGenerator := TDSGenerator.Create(nil);
  fOwner := TComponent.Create(nil);
  fExpectedCode := TStringList.Create;
end;

procedure TestGenerateAppends.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
  fExpectedCode.Free;
end;

// -----------------------------------------------------------------------
// Templates
// -----------------------------------------------------------------------

const
  CodeTemplateOnePrecisionField =
  (* *) '{$REGION ''Append data''}→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''%s'').Value := %s;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '{$ENDREGION}→';

const
  CodeTemplateOneField =
  (* *) '{$REGION ''Append data''}→' +
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

function TestGenerateAppends.Given_DataSet_With300String(aOwner: TComponent)
  : TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(fOwner);
  with ds do
  begin
    FieldDefs.Add('f1', ftWideString, 300);
    CreateDataSet;
    AppendRecord(['Covers Dependency Injection, you''ll learn about' +
      ' Constructor Injection, Property Injection, and Method Injection' +
      ' and about the right and wrong way to use it']);
    First;
  end;
  Result := ds;
end;

procedure TestGenerateAppends.TestLongStringLiterals_iss002;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := Given_DataSet_With300String(fOwner);

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['→◇◇◇''Covers Dependency Injection, you''''ll learn about Constructor Injecti''+→'
    + '◇◇◇''on, Property Injection, and Method Injection and about the right and''+→'
    + '◇◇◇'' wrong way to use it''']));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

// -----------------------------------------------------------------------
// Tests for: One BCD field with one value
// -----------------------------------------------------------------------

procedure TestGenerateAppends.TestOneBCDField_DifferentFieldName;
var
  sExpected: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
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

  fGenerator.Execute;

  sExpected := ReplaceArrowsAndDiamonds(Format(CodeTemplateOnePrecisionField,
    ['abc123', '1.01']));
  Assert_AreCodesEqual(sExpected, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.TestOneBCDField_iss001;
var
  sExpected: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
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

  fGenerator.Execute;

  sExpected := ReplaceArrowsAndDiamonds(Format(CodeTemplateOnePrecisionField,
    ['f1', '16.25']));
  Assert_AreCodesEqual(sExpected, fGenerator.CodeWithAppendData.Text);
end;

// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------

procedure TestGenerateAppends.TestOneDateTimeField_DateOnly;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01)]);
    First;
  end;

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['EncodeDate(2019,7,1)']));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.TestOneDateTimeField_DateTime;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01) + EncodeTime(15, 07, 30, 500)]);
    First;
  end;

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['EncodeDate(2019,7,1)+EncodeTime(15,7,30,500)']));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.TestOneIntegerField;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds
    (Format(CodeTemplateOneField, ['1']));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.TestOneWideStringField;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftWideString, 20);
    CreateDataSet;
    AppendRecord(['Alice has a cat']);
    First;
  end;

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    [QuotedStr('Alice has a cat')]));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Test_Indentation_1Space;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  fGenerator.IndentationText := ' ';

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds
    (Format(CodeTemplateOneField, ['1']));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.Test_Indentation_Empty;
var
  aExpectedCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  fGenerator.IndentationText := '';

  fGenerator.Execute;

  aExpectedCode := ReplaceArrowsAndDiamonds
    (Format(CodeTemplateOneField, ['1']));
  Assert_AreCodesEqual(aExpectedCode, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.Test_Indentation_MultilineTextValue;
var
  sExpected: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('f1', ftWideString, 300);
    CreateDataSet;
    AppendRecord(['Covers Dependency Injection, you''ll learn about' +
      ' Constructor Injection, Property Injection, and Method Injection' +
      ' and about the right and wrong way to use it']);
    First;
  end;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;

  sExpected := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['→◇◇◇' + QuotedStr
    ('Covers Dependency Injection, you''ll learn about Constructor Injecti') +
    '+→◇◇◇' + QuotedStr
    ('on, Property Injection, and Method Injection and about the right and') +
    '+→◇◇◇' + QuotedStr(' wrong way to use it')]));
  Assert_AreCodesEqual(sExpected, fGenerator.CodeWithAppendData.Text);
end;

procedure TestGenerateAppends.Test_IndentationText_BCDField;
var
  sExpected: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
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
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;

  sExpected := ReplaceArrowsAndDiamonds(Format(CodeTemplateOnePrecisionField,
    ['xyz123', '1.01']));
  Assert_AreCodesEqual(sExpected, fGenerator.CodeWithAppendData.Text);
end;

// -----------------------------------------------------------------------
// Tests for: Sample1
// -----------------------------------------------------------------------

function TestGenerateAppends.GivenSampleDataSetWithTwoRows(aOwner: TComponent)
  : TDataSet;
var
  memTable: TFDMemTable;
begin
  memTable := TFDMemTable.Create(aOwner);
  with memTable do
  begin
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    FieldDefs.Add('date1', ftDate);
    FieldDefs.Add('float1', ftFloat);
    FieldDefs.Add('currency1', ftCurrency);
    CreateDataSet;
    AppendRecord([1, 'Alice has a cat', EncodeDate(2019, 09, 16), 1.2, 1200]);
    AppendRecord([2, 'Eva has a dog', System.Variants.Null, Null, 950]);
    First;
  end;
  Result := memTable;
end;

procedure TestGenerateAppends.TestSample1;
var
  expectedCode: string;
begin
  fGenerator.DataSet := GivenSampleDataSetWithTwoRows(fOwner);

  fGenerator.Execute;

  expectedCode := ReplaceArrowsAndDiamonds(
  (* *) '{$REGION ''Append data''}→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''id'').Value := 1;→' +
  (* *) '◇◇FieldByName(''text1'').Value := ''Alice has a cat'';→' +
  (* *) '◇◇FieldByName(''date1'').Value := EncodeDate(2019,9,16);→' +
  (* *) '◇◇FieldByName(''float1'').Value := 1.2;→' +
  (* *) '◇◇FieldByName(''currency1'').Value := 1200;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇Append;→' +
  (* *) '◇◇FieldByName(''id'').Value := 2;→' +
  (* *) '◇◇FieldByName(''text1'').Value := ''Eva has a dog'';→' +
  (* *) '◇◇FieldByName(''currency1'').Value := 950;→' +
  (* *) '◇◇Post;→' +
  (* *) '◇end;→' +
  (* *) '{$ENDREGION}→');

  Assert_AreCodesEqual(expectedCode, fGenerator.CodeWithAppendData.Text);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerateAppends);

end.
