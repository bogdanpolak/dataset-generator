unit Test.GenerateAppends;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode,
  GeneratorForTests,
  Helper.DUnitAssert;

{$M+}

type

  [TestFixture]
  TestGenerateAppends = class(TObject)
  private
    fGenerator: TDSGeneratorUnderTest;
    fOwner: TComponent;
    function GivenField(const fieldName: string; fieldType: TFieldType;
      size: integer = 0): TField;
    function GivenSampleDataSetWithTwoRows(aOwner: TComponent): TDataSet;
    function Given_DataSet_With300String(aOwner: TComponent): TDataSet;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure TestOneIntegerField;
    procedure TestOneWideStringField;
    procedure TestOneDateTimeField_DateOnly;
    procedure TestOneDateTimeField_DateTime;
    procedure TestOneWideStringField_WithLongValue;
    procedure TestOneBCDField_DifferentFieldName;
    // -------------
    procedure TestLongStringLiterals_iss002;
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
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Setup;
begin
  fGenerator := TDSGeneratorUnderTest.Create(nil);
  fOwner := TComponent.Create(nil);
end;

procedure TestGenerateAppends.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
end;

// -----------------------------------------------------------------------
// Tests for: Generation of one FieldByName
// -----------------------------------------------------------------------

function TestGenerateAppends.GivenField(const fieldName: string;
  fieldType: TFieldType; size: integer = 0): TField;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(fOwner);
  ds.FieldDefs.Add(fieldName, fieldType, size);
  ds.CreateDataSet;
  Result := ds.Fields[0];
end;

procedure TestGenerateAppends.TestOneIntegerField;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField('Level', ftInteger);
  fld.DataSet.AppendRecord([1]);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual('FieldByName(''Level'').Value := 1;', actualCode);
end;

procedure TestGenerateAppends.TestOneDateTimeField_DateOnly;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField('Birthday', ftDate);
  fld.DataSet.AppendRecord([EncodeDate(2019, 07, 01)]);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual('FieldByName(''Birthday'').Value := EncodeDate(2019,7,1);',
    actualCode);
end;

procedure TestGenerateAppends.TestOneDateTimeField_DateTime;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField('ChangeDate', ftDateTime);
  fld.DataSet.AppendRecord( //.
    [EncodeDate(2019, 07, 01) + EncodeTime(15, 07, 30, 500)]);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual( //.
    'FieldByName(''ChangeDate'').Value := EncodeDate(2019,7,1)+EncodeTime(15,7,30,500);',
    actualCode);
end;

procedure TestGenerateAppends.TestOneWideStringField;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField('ChangeDate', ftWideString, 30);
  fld.DataSet.AppendRecord(['Alice has a cat']);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual( //.
    'FieldByName(''ChangeDate'').Value := ''Alice has a cat'';', //.
    actualCode);
end;

procedure TestGenerateAppends.TestOneWideStringField_WithLongValue;
var
  longText: string;
  actualCode: string;
begin
  longText := 'Covers Dependency Injection, you''ll learn about' +
    ' Constructor Injection, Property Injection, and Method Injection' +
    ' and about the right and wrong way to use it';

  actualCode := fGenerator.TestFormatLongStringLiterals(longText);

  Assert.AreMemosEqual( //.
    #13 //.
    + '      Covers Dependency Injection, you''ll learn about Constructor Injection''+'#13
    + '      '', Property Injection, and Method Injection and about the right and w''+'#13
    + '      ''rong way to use it'#13, actualCode);
end;

procedure TestGenerateAppends.TestOneBCDField_DifferentFieldName;
var
  ds: TFDMemTable;
  fld: TField;
  actualCode: string;
begin
  ds := TFDMemTable.Create(fOwner);
  with ds.FieldDefs.AddFieldDef do
  begin
    Name := 'abc123';
    DataType := ftBcd;
    Precision := 8;
    size := 2;
  end;
  ds.CreateDataSet;
  ds.AppendRecord([1.01]);
  fld := ds.Fields[0];

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual('FieldByName(''abc123'').Value := 1.01;', actualCode);
end;

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
  actualCode: string;
begin
  fGenerator.DataSet := Given_DataSet_With300String(fOwner);

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual( //.
    '{$REGION ''Append data''}'#13 //.
    + '  with ds do'#13 //.
    + '  begin'#13 //.
    + '    Append;'#13 //
    + '    FieldByName(''f1'').Value := '#13 //.
    + '      ''Covers Dependency Injection, you''''ll learn about Constructor Injecti''+'#13
    + '      ''on, Property Injection, and Method Injection and about the right and''+'#13
    + '      '' wrong way to use it'';'#13 //.
    + '    Post;'#13 //.
    + '  end;'#13 //.
    + '{$ENDREGION}'#13, actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Test_Indentation_1Space;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('Stage', ftInteger);
    CreateDataSet;
    AppendRecord([5]);
    First;
  end;
  fGenerator.IndentationText := ' ';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual(
    (* *) '{$REGION ''Append data''}'#13 +
    (* *) ' with ds do'#13 +
    (* *) ' begin'#13 +
    (* *) '  Append;'#13 +
    (* *) '  FieldByName(''Stage'').Value := 5;'#13 +
    (* *) '  Post;'#13 +
    (* *) ' end;'#13 +
    (* *) '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.Test_Indentation_Empty;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('Degree', ftInteger);
    CreateDataSet;
    AppendRecord([7]);
    First;
  end;
  fGenerator.IndentationText := '';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual(
    (* *) '{$REGION ''Append data''}'#13 +
    (* *) 'with ds do'#13 +
    (* *) 'begin'#13 +
    (* *) 'Append;'#13 +
    (* *) 'FieldByName(''Degree'').Value := 7;'#13 +
    (* *) 'Post;'#13 +
    (* *) 'end;'#13 +
    (* *) '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.Test_Indentation_MultilineTextValue;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('LongDescription', ftWideString, 300);
    CreateDataSet;
    AppendRecord(['Covers Dependency Injection, you''ll learn about' +
      ' Constructor Injection, Property Injection, and Method Injection' +
      ' and about the right and wrong way to use it']);
    First;
  end;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual( //.
    '{$REGION ''Append data''}'#13 //.
    + '  with ds do'#13 //.
    + '  begin'#13 //.
    + '    Append;'#13 //.
    + '    FieldByName(''LongDescription'').Value := '#13 //.
    + '      ''Covers Dependency Injection, you''''ll learn about Constructor Injecti''+'#13
    + '      ''on, Property Injection, and Method Injection and about the right and''+'#13
    + '      '' wrong way to use it'';'#13 //.
    + '    Post;'#13 //.
    + '  end;'#13 //.
    + '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.Test_IndentationText_BCDField;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := 'Price';
      DataType := ftBcd;
      Precision := 8;
      size := 2;
    end;
    CreateDataSet;
    AppendRecord([870.99]);
    First;
  end;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual(
    (* *) '{$REGION ''Append data''}'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    Append;'#13 +
    (* *) '    FieldByName(''Price'').Value := 870.99;'#13 +
    (* *) '    Post;'#13 +
    (* *) '  end;'#13 +
    (* *) '{$ENDREGION}'#13, actualCode);
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
  actualCode: string;
begin
  fGenerator.DataSet := GivenSampleDataSetWithTwoRows(fOwner);

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual(
    (* *) '{$REGION ''Append data''}'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    Append;'#13 +
    (* *) '    FieldByName(''id'').Value := 1;'#13 +
    (* *) '    FieldByName(''text1'').Value := ''Alice has a cat'';'#13 +
    (* *) '    FieldByName(''date1'').Value := EncodeDate(2019,9,16);'#13 +
    (* *) '    FieldByName(''float1'').Value := 1.2;'#13 +
    (* *) '    FieldByName(''currency1'').Value := 1200;'#13 +
    (* *) '    Post;'#13 +
    (* *) '  end;'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    Append;'#13 +
    (* *) '    FieldByName(''id'').Value := 2;'#13 +
    (* *) '    FieldByName(''text1'').Value := ''Eva has a dog'';'#13 +
    (* *) '    FieldByName(''currency1'').Value := 950;'#13 +
    (* *) '    Post;'#13 +
    (* *) '  end;'#13 +
    (* *) '{$ENDREGION}'#13, actualCode);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerateAppends);

end.
