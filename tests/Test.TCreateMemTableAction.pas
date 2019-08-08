unit Test.TCreateMemTableAction;

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
  TGenCodeDataSetMock = class(TObject)
  private
    GenerateDataSetCode: TGenerateDataSetCode;
    mockDataSet: TFDMemTable;
    function ReplaceArrowsToEndOfLines(const s: String): string;
    function GenerateCode(ds: TDataSet): string;
    procedure AssertOneFieldTemplateToMock(const FieldDefsParams: string;
      const FieldValue: string);
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
    procedure TestHeader_OneLine;
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

function TGenCodeDataSetMock.GenerateCode(ds: TDataSet): string;
begin
  GenerateDataSetCode.DataSet := ds;
  GenerateDataSetCode.Execute;
  Result := GenerateDataSetCode.Code.Text;
end;

function TGenCodeDataSetMock.ReplaceArrowsToEndOfLines(const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll])
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TGenCodeDataSetMock.Setup;
begin
  GenerateDataSetCode := TGenerateDataSetCode.Create(nil);
  mockDataSet := TFDMemTable.Create(nil);
end;

procedure TGenCodeDataSetMock.TearDown;
begin
  FreeAndNil(GenerateDataSetCode);
  FreeAndNil(mockDataSet);
end;

// -----------------------------------------------------------------------
// Templates
// -----------------------------------------------------------------------

const
  CodeTemplateOnePrecisionField =
  (* *) 'ds := TFDMemTable.Create(AOwner);→' +
  (* *) 'with ds do→' +
  (* *) 'begin→' +
  (* *) '  with FieldDefs.AddFieldDef do begin→' +
  (* *) '    Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;→' +
  (* *) '  end;→' +
  (* *) '  CreateDataSet;→' +
  (* *) 'end;→' +
  (* *) 'with ds do→' +
  (* *) 'begin→' +
  (* *) '  Append;→' +
  (* *) '    FieldByName(''%s'').Value := %s;→' +
  (* *) '  Post;→' +
  (* *) 'end;→';

const
  CodeTemplateOneField =
  (* *) 'ds := TFDMemTable.Create(AOwner);→' +
  (* *) 'with ds do→' +
  (* *) 'begin→' +
  (* *) '  FieldDefs.Add(''f1'', %s);→' +
  (* *) '  CreateDataSet;→' +
  (* *) 'end;→' +
  (* *) 'with ds do→' +
  (* *) 'begin→' +
  (* *) '  Append;→' +
  (* *) '    FieldByName(''f1'').Value := %s;→' +
  (* *) '  Post;→' +
  (* *) 'end;→';

procedure TGenCodeDataSetMock.AssertOneFieldTemplateToMock(const FieldDefsParams
  : string; const FieldValue: string);
var
  sExpected: string;
  aActual: string;
begin
  sExpected := ReplaceArrowsToEndOfLines(Format(CodeTemplateOneField,
    [FieldDefsParams, FieldValue]));
  aActual := GenerateCode(mockDataSet);
  Assert.AreEqual(sExpected, aActual);
end;

// -----------------------------------------------------------------------
// Tests for: Registered issues (bugs)
// -----------------------------------------------------------------------
{$REGION 'Registered issues (bugs)'}

procedure TGenCodeDataSetMock.TestLongStringLiterals_iss002;
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
  AssertOneFieldTemplateToMock('ftWideString, 300',
    '→      ' + QuotedStr
    ('Covers Dependency Injection, you''ll learn about Constructor Injecti') +
    '+→' + '      ' + QuotedStr
    ('on, Property Injection, and Method Injection and about the right and') +
    '+→' + '      ' + QuotedStr(' wrong way to use it'));
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: One BCD field with one value
// -----------------------------------------------------------------------
{$REGION 'One BCD field with one value'}

procedure TGenCodeDataSetMock.TestOneBCDField_DifferentFieldName;
var
  sExpected: string;
  sActual: string;
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
  sExpected := ReplaceArrowsToEndOfLines(Format(CodeTemplateOnePrecisionField,
    ['abc123', 'ftBCD', 8, 2, 'abc123', '1.01']));
  sActual := GenerateCode(mockDataSet);
  Assert.AreEqual(sExpected, sActual);
end;

procedure TGenCodeDataSetMock.TestOneBCDField_iss001;
var
  sExpected: string;
  sActual: string;
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
  sExpected := ReplaceArrowsToEndOfLines(Format(CodeTemplateOnePrecisionField,
    ['f1', 'ftBCD', 10, 4, 'f1', '16.25']));
  sActual := GenerateCode(mockDataSet);
  Assert.AreEqual(sExpected, sActual);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------
{$REGION 'One DB field with one value'}

procedure TGenCodeDataSetMock.TestOneDateTimeField_DateOnly;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01)]);
    First;
  end;
  AssertOneFieldTemplateToMock('ftDateTime', 'EncodeDate(2019,7,1)');
end;

procedure TGenCodeDataSetMock.TestOneDateTimeField_DateTime;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01) + EncodeTime(15, 07, 30, 500)]);
    First;
  end;
  AssertOneFieldTemplateToMock('ftDateTime',
    'EncodeDate(2019,7,1)+EncodeTime(15,7,30,500)');
end;

procedure TGenCodeDataSetMock.TestOneIntegerField;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  AssertOneFieldTemplateToMock('ftInteger', '1');
end;

procedure TGenCodeDataSetMock.TestOneWideStringField;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftWideString, 20);
    CreateDataSet;
    AppendRecord(['Alice has a cat']);
    First;
  end;
  AssertOneFieldTemplateToMock('ftWideString, 20',
    QuotedStr('Alice has a cat'));
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: component header and footer
// -----------------------------------------------------------------------
{$REGION 'Component header and footer'}

procedure TGenCodeDataSetMock.TestHeader_OneLine;
var
  Line1: string;
  FieldDefsParams: string;
  FieldValue: AnsiChar;
  sExpected: string;
  aActual: string;
begin
  Line1 := '// Test coments';
  GenerateDataSetCode.Header.Add(Line1);
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  FieldDefsParams := 'ftInteger';
  FieldValue := '1';
  sExpected := ReplaceArrowsToEndOfLines
    (Line1 + '→' + Format(CodeTemplateOneField, [FieldDefsParams, FieldValue]));
  aActual := GenerateCode(mockDataSet);
  Assert.AreEqual(sExpected, aActual);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: Sample1
// -----------------------------------------------------------------------
{$REGION 'Sample1 : dataset with 4 fields and 2 rows containing NULL values'}

procedure TGenCodeDataSetMock.TestSample1;
var
  expectedCode: string;
  actualCode: string;
begin
  expectedCode := ReplaceArrowsToEndOfLines(
    (* *) 'ds := TFDMemTable.Create(AOwner);→' +
    (* *) 'with ds do→' +
    (* *) 'begin→' +
    (* *) '  FieldDefs.Add(''id'', ftInteger);→' +
    (* *) '  FieldDefs.Add(''text1'', ftWideString, 30);→' +
    (* *) '  FieldDefs.Add(''date1'', ftDate);→' +
    (* *) '  FieldDefs.Add(''float1'', ftFloat);→' +
    (* *) '  FieldDefs.Add(''currency1'', ftCurrency);→' +
    (* *) '  CreateDataSet;→' +
    (* *) 'end;→' +
    (* *) 'with ds do→' +
    (* *) 'begin→' +
    (* *) '  Append;→' +
    (* *) '    FieldByName(''id'').Value := 1;→' +
    (* *) '    FieldByName(''text1'').Value := ''Ala ma kota'';→' +
    (* *) '    FieldByName(''date1'').Value := EncodeDate(2019,9,16);→' +
    (* *) '    FieldByName(''float1'').Value := 1.2;→' +
    (* *) '    FieldByName(''currency1'').Value := 1200;→' +
    (* *) '  Post;→' +
    (* *) '  Append;→' +
    (* *) '    FieldByName(''id'').Value := 2;→' +
    (* *) '    FieldByName(''text1'').Value := ''Ala ma kota'';→' +
    (* *) '    FieldByName(''currency1'').Value := 950;→' +
    (* *) '  Post;→' +
    (* *) 'end;→');
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
  actualCode := GenerateCode(mockDataSet);
  Assert.AreEqual(expectedCode, actualCode);
end;

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(TGenCodeDataSetMock);

end.
