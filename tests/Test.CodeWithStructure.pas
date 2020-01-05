unit Test.CodeWithStructure;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Math,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode,
  GeneratorForTests, Helper.DUnitAssert;

{$M+}

type

  [TestFixture]
  TestGenerateStructure = class(TObject)
  private
    fGenerator: TDSGeneratorUnderTest;
    fOwner: TComponent;
    fExpectedCode: TStringList;
    procedure AreCodesEqual(expectedCode: TStrings; actualCode: TStrings);
    function GivenSampleDataSetWithTwoRows(aOwner: TComponent): TDataSet;
    function ReplaceArrowsAndDiamonds(const s: String): string;
    function GivenField(aOwner: TComponent; const fieldName: string;
      fieldType: TFieldType; size: integer = 0): TField;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure GenFieldDef_Integer;
    procedure GenFieldDef_WideString;
    procedure GenFieldDef_Date;
    procedure GenFieldDef_DateTime;
    procedure GenFieldDef_BCD;
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
// Templates
// -----------------------------------------------------------------------

const
  CodeTemplateOneBcdField =
  (* *) '◇ds := TFDMemTable.Create(AOwner);→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇with FieldDefs.AddFieldDef do begin→' +
  (* *) '◇◇◇Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;→' +
  (* *) '◇◇end;→' +
  (* *) '◇◇CreateDataSet;→' +
  (* *) '◇end;→';

  CodeTemplateOneField =
  (* *) '◇ds := TFDMemTable.Create(AOwner);→' +
  (* *) '◇with ds do→' +
  (* *) '◇begin→' +
  (* *) '◇◇FieldDefs.Add(''f1'', %s);→' +
  (* *) '◇◇CreateDataSet;→' +
  (* *) '◇end;→';


  // -----------------------------------------------------------------------
  // Utils section
  // -----------------------------------------------------------------------

function TestGenerateStructure.ReplaceArrowsAndDiamonds
  (const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', fGenerator.IndentationText,
    [rfReplaceAll])
end;

procedure TestGenerateStructure.AreCodesEqual(expectedCode: TStrings;
  actualCode: TStrings);
begin
  Assert.AreEqual(expectedCode.Text, actualCode.Text);
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGenerateStructure.Setup;
begin
  fGenerator := TDSGeneratorUnderTest.Create(nil);
  fOwner := TComponent.Create(nil);
  fExpectedCode := TStringList.Create;
end;

procedure TestGenerateStructure.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
  fExpectedCode.Free;
end;


// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------

function TestGenerateStructure.GivenField(aOwner: TComponent;
  const fieldName: string; fieldType: TFieldType; size: integer = 0): TField;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  ds.FieldDefs.Add(fieldName, fieldType, size);
  ds.CreateDataSet;
  Result := ds.Fields[0];
end;

procedure TestGenerateStructure.GenFieldDef_Date;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Birthday', ftDate);

  actualCode := fGenerator.TestGenCodeLineFieldDefAdd(fld);

  Assert.AreEqual('FieldDefs.Add(''Birthday'', ftDate);', actualCode);
end;

procedure TestGenerateStructure.GenFieldDef_DateTime;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Created', ftDateTime);

  actualCode := fGenerator.TestGenCodeLineFieldDefAdd(fld);

  Assert.AreEqual('FieldDefs.Add(''Created'', ftDateTime);', actualCode);
end;

procedure TestGenerateStructure.GenFieldDef_Integer;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Rating', ftInteger);

  actualCode := fGenerator.TestGenCodeLineFieldDefAdd(fld);

  Assert.AreEqual('FieldDefs.Add(''Rating'', ftInteger);', actualCode);
end;

procedure TestGenerateStructure.GenFieldDef_WideString;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Description', ftWideString, 30);

  actualCode := fGenerator.TestGenCodeLineFieldDefAdd(fld);

  Assert.AreEqual('FieldDefs.Add(''Description'', ftWideString, 30);',
    actualCode);
end;

procedure TestGenerateStructure.GenFieldDef_BCD;
var
  ds: TFDMemTable;
  fld: TField;
  actualCode: string;
begin
  ds := TFDMemTable.Create(fOwner);
  with ds.FieldDefs.AddFieldDef do
  begin
    Name := 'Price';
    DataType := ftBcd;
    Precision := 10;
    size := 4;
  end;
  ds.CreateDataSet;
  fld := ds.Fields[0];

  actualCode := fGenerator.TestGenCodeLineFieldDefAdd(fld);

  Assert.AreMemosEqual( //.
    'with FieldDefs.AddFieldDef do begin'#13 +
    '      Name := ''Price'';  DataType := ftBCD;  Precision := 10;  Size := 4;'#13
    + '    end;', //.
    actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------

procedure TestGenerateStructure.Test_Indentation_1Space;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftInteger', '1']));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TestGenerateStructure.Test_Indentation_Empty;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftInteger', '1']));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TestGenerateStructure.Test_Indentation_MultilineTextValue;
var
  aExpectedFieldValue: string;
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

  aExpectedFieldValue := '→◇◇◇' +
    QuotedStr(
    'Covers Dependency Injection, you''ll learn about Constructor Injecti') +
    '+→◇◇◇' + QuotedStr
    ('on, Property Injection, and Method Injection and about the right and') +
    '+→◇◇◇' + QuotedStr(' wrong way to use it');
  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftWideString, 300', aExpectedFieldValue]));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TestGenerateStructure.Test_IndentationText_BCDField;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := 'xyz123';
      DataType := ftBcd;
      Precision := 8;
      size := 2;
    end;
    CreateDataSet;
    AppendRecord([1.01]);
    First;
  end;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneBcdField,
    ['xyz123', 'ftBCD', 8, 2]));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

// -----------------------------------------------------------------------
// Tests for: Sample1
// -----------------------------------------------------------------------

function TestGenerateStructure.GivenSampleDataSetWithTwoRows(aOwner: TComponent)
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

procedure TestGenerateStructure.TestSample1;
begin
  fGenerator.DataSet := GivenSampleDataSetWithTwoRows(fOwner);

  fGenerator.Execute;

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(
    (* *) '◇ds := TFDMemTable.Create(AOwner);→' +
    (* *) '◇with ds do→' +
    (* *) '◇begin→' +
    (* *) '◇◇FieldDefs.Add(''id'', ftInteger);→' +
    (* *) '◇◇FieldDefs.Add(''text1'', ftWideString, 30);→' +
    (* *) '◇◇FieldDefs.Add(''date1'', ftDate);→' +
    (* *) '◇◇FieldDefs.Add(''float1'', ftFloat);→' +
    (* *) '◇◇FieldDefs.Add(''currency1'', ftCurrency);→' +
    (* *) '◇◇CreateDataSet;→' +
    (* *) '◇end;→');
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerateStructure);

end.
