unit Test.GenerateAppends;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
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
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure GenFieldByName_Integer;
    procedure GenFieldByName_WideString;
    procedure GenFieldByName_Date;
    procedure GenFieldByName_DateTime;
    procedure GenFieldByName_BCDField;
    // -------------
    procedure GenLongLiterals;
    // -------------
    procedure Iss002_GenLongStringLiterals_NewLines;
    // -------------
    procedure GenIndentation_Empty;
    procedure GenIndentation_OneSpace;
    procedure GenIndentation_LongLiteral;
    // -------------
    procedure GenSampleDataset_Appends;
    procedure GenSampleDataset_OnelineAppends;
    // -------------
    procedure GenMultipleRowDataset_MaxRows_Zero;
    procedure GenMultipleRowDataset_MaxRows_2;
    // -------------
    procedure GenMultipleRowDataset_PersistDatasetPosition;
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

function GivenDataSet_With300String(aOwner: TComponent;
  const aFieldName: string): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add(aFieldName, ftWideString, 300);
    CreateDataSet;
    AppendRecord(['Covers Dependency Injection, you''ll learn about' +
      ' Constructor Injection, Property Injection, and Method Injection' +
      ' and about the right and wrong way to use it']);
    First;
  end;
  Result := ds;
end;

function GivenDataSet_WithInteger(aOwner: TComponent; const aFieldName: string)
  : TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add(aFieldName, ftInteger);
    CreateDataSet;
    AppendRecord([5]);
    First;
  end;
  Result := ds;
end;

function Given_ID_Text_DataSet(aOwner: TComponent;
  aData: TArray < TArray < Variant >> ): TDataSet;
var
  ds: TFDMemTable;
  idxRow: integer;
  idxField: integer;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add('ID', ftInteger);
    FieldDefs.Add('Text', ftWideString, 30);
    CreateDataSet;
  end;
  for idxRow := 0 to High(aData) do
  begin
    ds.Append;
    for idxField := 0 to High(aData[idxRow]) do
      ds.Fields[idxField].Value := aData[idxRow][idxField];
    ds.Post;
  end;
  ds.First;
  Result := ds;
end;

function GivenSampleDataSet(aOwner: TComponent;
  aData: TArray < TArray < Variant >> ): TDataSet;
var
  ds: TFDMemTable;
  idxRow: integer;
  idxField: integer;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    FieldDefs.Add('date1', ftDate);
    FieldDefs.Add('float1', ftFloat);
    FieldDefs.Add('currency1', ftCurrency);
    CreateDataSet;
  end;
  for idxRow := 0 to High(aData) do
  begin
    ds.Append;
    for idxField := 0 to High(aData[idxRow]) do
      ds.Fields[idxField].Value := aData[idxRow][idxField];
    ds.Post;
  end;
  ds.First;
  Result := ds;
end;

// -----------------------------------------------------------------------
// Tests for: Generation of one FieldByName
// -----------------------------------------------------------------------

procedure TestGenerateAppends.GenFieldByName_Integer;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Level', ftInteger);
  fld.DataSet.AppendRecord([1]);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual('FieldByName(''Level'').Value := 1;', actualCode);
end;

procedure TestGenerateAppends.GenFieldByName_Date;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'Birthday', ftDate);
  fld.DataSet.AppendRecord([EncodeDate(2019, 07, 01)]);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual('FieldByName(''Birthday'').Value := EncodeDate(2019,7,1);',
    actualCode);
end;

procedure TestGenerateAppends.GenFieldByName_DateTime;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'ChangeDate', ftDateTime);
  fld.DataSet.AppendRecord( //.
    [EncodeDate(2019, 07, 01) + EncodeTime(15, 07, 30, 500)]);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual( //.
    'FieldByName(''ChangeDate'').Value := EncodeDate(2019,7,1)+EncodeTime(15,7,30,500);',
    actualCode);
end;

procedure TestGenerateAppends.GenFieldByName_WideString;
var
  fld: TField;
  actualCode: string;
begin
  fld := GivenField(fOwner, 'ChangeDate', ftWideString, 30);
  fld.DataSet.AppendRecord(['Alice has a cat']);

  actualCode := fGenerator.TestGenCodeLineSetFieldValue(fld);

  Assert.AreEqual( //.
    'FieldByName(''ChangeDate'').Value := ''Alice has a cat'';', //.
    actualCode);
end;

procedure TestGenerateAppends.GenFieldByName_BCDField;
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
// Tests: Format code with long string literals
// -----------------------------------------------------------------------

procedure TestGenerateAppends.GenLongLiterals;
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

// -----------------------------------------------------------------------
// Tests for: Registered issues (bugs)
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Iss002_GenLongStringLiterals_NewLines;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_With300String(fOwner, 'Info');
  fGenerator.GeneratorMode := genAppend;

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''Info'').Value := '#13 +
    {} '      ''Covers Dependency Injection, you''''ll learn about Constructor Injecti''+'#13
    {} + '      ''on, Property Injection, and Method Injection and about the right and''+'#13
    {} + '      '' wrong way to use it'';'#13 +
    {} '    Post;'#13 +
    {} '  end;'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13, actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------

procedure TestGenerateAppends.GenIndentation_OneSpace;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithInteger(fOwner, 'Stage');
  fGenerator.GeneratorMode := genAppend;
  fGenerator.IndentationText := ' ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} ' with ds do'#13 +
    {} ' begin'#13 +
    {} '  Append;'#13 +
    {} '  FieldByName(''Stage'').Value := 5;'#13 +
    {} '  Post;'#13 +
    {} ' end;'#13 +
    {} ' ds.First;'#13 +
    {} '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.GenIndentation_Empty;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithInteger(fOwner, 'Degree');
  fGenerator.GeneratorMode := genAppend;

  fGenerator.IndentationText := '';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} 'with ds do'#13 +
    {} 'begin'#13 +
    {} 'Append;'#13 +
    {} 'FieldByName(''Degree'').Value := 5;'#13 +
    {} 'Post;'#13 +
    {} 'end;'#13 +
    {} 'ds.First;'#13 +
    {} '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.GenIndentation_LongLiteral;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_With300String(fOwner, 'LongDescription');
  fGenerator.GeneratorMode := genAppend;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual( //.
    {} '{$REGION ''Append data''}'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''LongDescription'').Value := '#13 +
    {} '      ''Covers Dependency Injection, you''''ll learn about Constructor Injecti''+'#13
    {} + '      ''on, Property Injection, and Method Injection and about the right and''+'#13
    {} + '      '' wrong way to use it'';'#13 +
    {} '    Post;'#13 +
    {} '  end;'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13, actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: Sample1
// -----------------------------------------------------------------------

procedure TestGenerateAppends.GenSampleDataset_Appends;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenSampleDataSet(fOwner, [
    {} [1, 'Alice has a cat', EncodeDate(2019, 09, 16), 1.2, 1200],
    {} [2, 'Eva has a dog', System.Variants.Null, Null, 950]]);
  fGenerator.GeneratorMode := genAppend;

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''id'').Value := 1;'#13 +
    {} '    FieldByName(''text1'').Value := ''Alice has a cat'';'#13 +
    {} '    FieldByName(''date1'').Value := EncodeDate(2019,9,16);'#13 +
    {} '    FieldByName(''float1'').Value := 1.2;'#13 +
    {} '    FieldByName(''currency1'').Value := 1200;'#13 +
    {} '    Post;'#13 +
    {} '  end;'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''id'').Value := 2;'#13 +
    {} '    FieldByName(''text1'').Value := ''Eva has a dog'';'#13 +
    {} '    FieldByName(''currency1'').Value := 950;'#13 +
    {} '    Post;'#13 +
    {} '  end;'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.GenSampleDataset_OnelineAppends;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenSampleDataSet(fOwner, [
    {} [1, 'Alice has a cat', EncodeDate(2019, 09, 16), 1.2, 1200],
    {} [2, 'Eva has a dog', System.Variants.Null, Null, 950]]);
  fGenerator.GeneratorMode := genAppend;
  fGenerator.AppendMode := amSinglelineAppends;

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} '  ds.AppendRecord([1, ''Alice has a cat'',' +
    ' EncodeDate(2019,9,16), 1.2, 1200]);'#13 +
    {} '  ds.AppendRecord([2, ''Eva has a dog'', Null, Null, 950]);'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13, actualCode);
end;

procedure TestGenerateAppends.GenMultipleRowDataset_MaxRows_Zero;
begin
  fGenerator.DataSet := Given_ID_Text_DataSet(fOwner,
    [[1, 'FirstRow'], [2, 'MiddleRow'], [3, 'ThirdRow'], [4, 'FourthRow'],
    [5, 'FifthRow'], [6, 'LastRow']]);
  fGenerator.AppendMode := amSinglelineAppends;
  fGenerator.GeneratorMode := genAppend;

  fGenerator.MaxRows := 0;
  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} '  ds.AppendRecord([1, ''FirstRow'']);'#13 +
    {} '  ds.AppendRecord([2, ''MiddleRow'']);'#13 +
    {} '  ds.AppendRecord([3, ''ThirdRow'']);'#13 +
    {} '  ds.AppendRecord([4, ''FourthRow'']);'#13 +
    {} '  ds.AppendRecord([5, ''FifthRow'']);'#13 +
    {} '  ds.AppendRecord([6, ''LastRow'']);'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13, fGenerator.Code.Text);
end;

procedure TestGenerateAppends.GenMultipleRowDataset_MaxRows_2;
begin
  fGenerator.DataSet := Given_ID_Text_DataSet(fOwner,
    [[1, 'FirstRow'], [2, 'MiddleRow'], [3, 'ThirdRow'], [4, 'FourthRow'],
    [5, 'FifthRow'], [6, 'LastRow']]);
  fGenerator.AppendMode := amSinglelineAppends;
  fGenerator.GeneratorMode := genAppend;

  fGenerator.MaxRows := 2;
  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} '{$REGION ''Append data''}'#13 +
    {} '  ds.AppendRecord([1, ''FirstRow'']);'#13 +
    {} '  ds.AppendRecord([2, ''MiddleRow'']);'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13, fGenerator.Code.Text);
end;

// -----------------------------------------------------------------------
// Bug proofs
// -----------------------------------------------------------------------

// Bug: #35 - Generator is not persisting dataset position

procedure TestGenerateAppends.GenMultipleRowDataset_PersistDatasetPosition;
begin
  fGenerator.DataSet := GivenSampleDataSet(fOwner,
    [[1, 'FirstRow'], [2, 'MiddleRow'], [3, 'aRow'], [4, 'LastRow']]);
  fGenerator.DataSet.RecNo := 3;

  fGenerator.TestGenerateAppendsBlock;

  Assert.AreEqual(3, fGenerator.DataSet.RecNo);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerateAppends);

end.
