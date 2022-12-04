unit Test.GenerateStructure;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.Math,
  Data.DB,
  FireDAC.Comp.Client,
  MemoryDataSetGenerator,
  Helper.DUnitAssert;

{$M+}

type

  [TestFixture]
  TestGenerateStructure = class(TObject)
  private
    fGenerator: TDSGenerator;
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
    procedure Execute_DataSetType_CDS;
    procedure Execute_WithMultipleFields;
    // -------------
    procedure Execute_WithNoIndentation;
    procedure Execute_WithOneSpaceIndentation;
    procedure Execute_DefaultIndentation_BCDField;
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
  fGenerator := TDSGenerator.Create(nil);
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

function GivenDataSet_WithField(aOwner: TComponent; const fieldName: string;
  fieldType: TFieldType; size: integer = 0): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  ds.FieldDefs.Add(fieldName, fieldType, size);
  ds.CreateDataSet;
  Result := ds;
end;

function GivenDataSet_WithBcdField(
  aOwner: TComponent;
  const aFieldName: string;
  const aPrecision: integer;
  const aDecimalLen: integer): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds.FieldDefs.AddFieldDef do
  begin
    Name := aFieldName;
    DataType := ftBcd;
    Precision := aPrecision;
    size := aDecimalLen;
  end;
  ds.CreateDataSet;
  Result := ds;
end;

function GivenDataSet_WithInteger(aOwner: TComponent; const aFieldName: string)
  : TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  ds.FieldDefs.Add(aFieldName, ftInteger);
  ds.CreateDataSet;
  Result := ds;
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

function GivenDataSet_WithBCD_11_3(aOwner: TComponent; const aFieldName: string)
  : TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    with FieldDefs.AddFieldDef do
    begin
      Name := aFieldName;
      DataType := ftBcd;
      Precision := 11;
      size := 3;
    end;
    CreateDataSet;
  end;
  Result := ds;
end;

// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------

procedure TestGenerateStructure.GenFieldDef_Date;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSet_WithField(fOwner, 'Birthday', ftDate);

  code := TStructureBlockGenerator.Generate(ds,'  ');

  Assert.AreMemosEqual(
    { } '  with ds do'#13 +
    { } '  begin'#13 +
    { } '    FieldDefs.Add(''Birthday'', ftDate);'#13 +
    { } '    CreateDataSet;'#13 +
    { } '  end;', code);
end;

procedure TestGenerateStructure.GenFieldDef_DateTime;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSet_WithField(fOwner, 'Created', ftDateTime);

  code := TStructureBlockGenerator.Generate(ds,'·');

  Assert.AreMemosEqual(
    { } '·with ds do'#13 +
    { } '·begin'#13 +
    { } '··FieldDefs.Add(''Created'', ftDateTime);'#13 +
    { } '··CreateDataSet;'#13 +
    { } '·end;', code);
end;

procedure TestGenerateStructure.GenFieldDef_Integer;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSet_WithField(fOwner, 'Rating', ftInteger);

  code := TStructureBlockGenerator.Generate(ds,'  ');

  Assert.AreMemosEqual(
    { } '  with ds do'#13 +
    { } '  begin'#13 +
    { } '    FieldDefs.Add(''Rating'', ftInteger);'#13 +
    { } '    CreateDataSet;'#13 +
    { } '  end;', code);
end;

procedure TestGenerateStructure.GenFieldDef_WideString;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSet_WithField(fOwner, 'Description', ftWideString, 30);

  code := TStructureBlockGenerator.Generate(ds,'  ');

  Assert.AreMemosEqual(
    { } '  with ds do'#13 +
    { } '  begin'#13 +
    { } '    FieldDefs.Add(''Description'', ftWideString, 30);'#13 +
    { } '    CreateDataSet;'#13 +
    { } '  end;', code);
end;

procedure TestGenerateStructure.GenFieldDef_BCD;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSet_WithBcdField(fOwner, 'Price', 10, 4);

  code := TStructureBlockGenerator.Generate(ds,'·');

  Assert.AreMemosEqual(
    { } '·with ds do'#13 +
    { } '·begin'#13 +
    { } '··with FieldDefs.AddFieldDef do begin'#13 +
    { } '···Name := ''Price'';  DataType := ftBCD;  Precision := 10;  Size := 4;'#13
    +
    { } '··end;'#13 +
    { } '··CreateDataSet;'#13 +
    { } '·end;', code);
end;

// -----------------------------------------------------------------------
// Test: Dataset structure generation with multiple diffrent fields
// -----------------------------------------------------------------------

procedure TestGenerateStructure.Execute_DataSetType_CDS;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithInteger(fOwner, 'Group');
  fGenerator.GeneratorMode := genStructure;
  fGenerator.DataSetType := dstClientDataSet;

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    FieldDefs.Add(''Group'', ftInteger);'#13 +
    (* *) '    CreateDataSet;'#13 +
    (* *) '  end;'#13, actualCode);
end;

procedure TestGenerateStructure.Execute_WithMultipleFields;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_Sample_WithTwoRows(fOwner);
  fGenerator.GeneratorMode := genStructure;

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
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

procedure TestGenerateStructure.Execute_WithOneSpaceIndentation;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithInteger(fOwner, 'Points');
  fGenerator.GeneratorMode := genStructure;
  fGenerator.IndentationText := ' ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    (* *) ' with ds do'#13 +
    (* *) ' begin'#13 +
    (* *) '  FieldDefs.Add(''Points'', ftInteger);'#13 +
    (* *) '  CreateDataSet;'#13 +
    (* *) ' end;'#13, actualCode);
end;

procedure TestGenerateStructure.Execute_WithNoIndentation;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithInteger(fOwner, 'Points');
  fGenerator.GeneratorMode := genStructure;
  fGenerator.IndentationText := '';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    (* *) 'with ds do'#13 +
    (* *) 'begin'#13 +
    (* *) 'FieldDefs.Add(''Points'', ftInteger);'#13 +
    (* *) 'CreateDataSet;'#13 +
    (* *) 'end;'#13, actualCode);
end;

procedure TestGenerateStructure.Execute_DefaultIndentation_BCDField;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithBCD_11_3(fOwner, 'Bugdet');
  fGenerator.GeneratorMode := genStructure;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
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
