unit Test.GenerateStructure;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Math,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode,
  GeneratorForTests,
  Helper.DUnitAssert;

{$M+}

type

  [TestFixture]
  TestGenerateStructure = class(TObject)
  private
    fGenerator: TDSGeneratorUnderTest;
    fOwner: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure GenFieldDef_Integer;
    procedure GenFieldDef_WideString;
    procedure GenFieldDef_Date;
    procedure GenFieldDef_DateTime;
    procedure GenFieldDef_BCD;
    // -------------
    procedure GenStructure_WithMultipleFields;
    // -------------
    procedure GenWithNoIndentation;
    procedure GenWithIndentation_OneSpace;
    procedure GenWithIndentation_BCDField;
  end;

implementation

uses
  System.Variants,
  Data.FmtBcd;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGenerateStructure.Setup;
begin
  fGenerator := TDSGeneratorUnderTest.Create(nil);
  fOwner := TComponent.Create(nil);
end;

procedure TestGenerateStructure.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
end;

// -----------------------------------------------------------------------
// Dataset factories
// -----------------------------------------------------------------------

function GivenField(aOwner: TComponent; const fieldName: string;
  fieldType: TFieldType; size: integer = 0): TField;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  ds.FieldDefs.Add(fieldName, fieldType, size);
  ds.CreateDataSet;
  Result := ds.Fields[0];
end;

function GivenDataSet_Sample_WithTwoRows(aOwner: TComponent): TDataSet;
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

// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------

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

  Assert.AreMemosEqual(
    (* *) 'with FieldDefs.AddFieldDef do begin'#13 +
    (* *) '      Name := ''Price'';  DataType := ftBCD;  Precision := 10;  Size := 4;'#13
    (* *) + '    end;', actualCode);
end;

// -----------------------------------------------------------------------
// Test: Dataset structure generation with multiple diffrent fields
// -----------------------------------------------------------------------

procedure TestGenerateStructure.GenStructure_WithMultipleFields;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_Sample_WithTwoRows(fOwner);

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithStructure.Text;

  Assert.AreMemosEqual(
    (* *) '  ds := TFDMemTable.Create(AOwner);'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    FieldDefs.Add(''id'', ftInteger);'#13 +
    (* *) '    FieldDefs.Add(''text1'', ftWideString, 30);'#13 +
    (* *) '    FieldDefs.Add(''date1'', ftDate);'#13 +
    (* *) '    FieldDefs.Add(''float1'', ftFloat);'#13 +
    (* *) '    FieldDefs.Add(''currency1'', ftCurrency);'#13 +
    (* *) '    CreateDataSet;'#13 +
    (* *) '  end;'#13, actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------

procedure TestGenerateStructure.GenWithIndentation_OneSpace;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('Points', ftInteger);
    CreateDataSet;
  end;
  fGenerator.IndentationText := ' ';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithStructure.Text;

  Assert.AreMemosEqual(
    (* *) ' ds := TFDMemTable.Create(AOwner);'#13 +
    (* *) ' with ds do'#13 +
    (* *) ' begin'#13 +
    (* *) '  FieldDefs.Add(''Points'', ftInteger);'#13 +
    (* *) '  CreateDataSet;'#13 +
    (* *) ' end;'#13, actualCode);
end;

procedure TestGenerateStructure.GenWithNoIndentation;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    FieldDefs.Add('Points', ftInteger);
    CreateDataSet;
    AppendRecord([1]);
    First;
  end;
  fGenerator.IndentationText := '';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithStructure.Text;

  Assert.AreMemosEqual(
    (* *) 'ds := TFDMemTable.Create(AOwner);'#13 +
    (* *) 'with ds do'#13 +
    (* *) 'begin'#13 +
    (* *) 'FieldDefs.Add(''Points'', ftInteger);'#13 +
    (* *) 'CreateDataSet;'#13 +
    (* *) 'end;'#13, actualCode);
end;

procedure TestGenerateStructure.GenWithIndentation_BCDField;
var
  actualCode: string;
begin
  fGenerator.DataSet := TFDMemTable.Create(fOwner);
  with fGenerator.DataSet as TFDMemTable do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := 'Bugdet';
      DataType := ftBcd;
      Precision := 11;
      size := 3;
    end;
    CreateDataSet;
  end;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithStructure.Text;

  Assert.AreMemosEqual(
    (* *) '  ds := TFDMemTable.Create(AOwner);'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    with FieldDefs.AddFieldDef do begin'#13 +
    (* *) '      Name := ''Bugdet'';  DataType := ftBCD;  Precision := 11;  Size := 3;'#13
    (* *) + '    end;'#13 +
    (* *) '    CreateDataSet;'#13 +
    (* *) '  end;'#13, actualCode);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerateStructure);

end.
