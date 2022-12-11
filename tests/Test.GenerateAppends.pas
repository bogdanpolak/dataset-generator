unit Test.GenerateAppends;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Data.DB,
  FireDAC.Comp.Client,
  MemoryDataSetGenerator,
  Helper.DUnitAssert;

{$M+}

type

  [TestFixture]
  TestGenerateAppends = class(TObject)
  private const
    DefaultRightMargin = 101;
  private
    fGenerator: TDSGenerator;
    fOwner: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure GenDataWith_1xInteger;
    procedure GenDataWith_1xWideString;
    procedure GenDataWith_1xDate;
    procedure GenDataWith_1xDateTime;
    procedure GenDataWith_1xBCDField;
    procedure GenDataWith_1xWithLongLitteral;
    procedure GenDataWith_1xBlob;
    procedure GenDataWith_1x40BytesBlob;
    procedure GenDataWith_1x50BytesBlob;
    // -------------
    // -------------
    procedure Iss002_GenLongStringLiterals_NewLines;
    // -------------
    procedure GenIndentation_Empty;
    procedure GenIndentation_OneSpace;
    procedure GenIndentation_LongLiteral;
    // -------------
    procedure GenWrappedString_WithMargin47;
    procedure GenString_WithMargin63;
    // -------------
    procedure GenSampleDataset_Appends;
    procedure GenSampleDataset_OnelineAppends;
    // -------------
    procedure GenMultipleRowDataset_MaxRows_Zero;
    procedure GenMultipleRowDataset_MaxRows_2;
    // -------------
    procedure GenAppendRows_OneRow;
    procedure GenAppendRows_ThreeRows;
    // -------------
    procedure GenMultipleRowDataset_PersistDatasetPosition;
  end;

implementation

uses
  System.Variants,
  System.Math,
  Data.FmtBcd;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Setup;
begin
  fGenerator := TDSGenerator.Create(nil);
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

const
  STR_300chars = 'Covers Dependency Injection, you''ll learn about' +
    ' Constructor Injection, Property Injection, and Method Injection' +
    ' and about the right and wrong way to use it';

function GivenDataSetWithField(
  const aOwner: TComponent;
  const aFieldName: string;
  aFieldType: TFieldType;
  aFieldSize: integer = 0): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  ds.FieldDefs.Add(aFieldName, aFieldType, aFieldSize);
  ds.CreateDataSet;
  Result := ds;
end;

function GivenDataSet_WithString(aOwner: TComponent; const aFieldName: string;
  const aDataValue: string): TDataSet;
var
  ds: TFDMemTable;
  fieldSize: integer;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    fieldSize := IFThen(aDataValue.Length<100,100,aDataValue.Length+1);
    FieldDefs.Add(aFieldName, ftWideString, fieldSize);
    CreateDataSet;
    AppendRecord([aDataValue]);
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

procedure SetBlobFieldValue(
  const aBlobField: TBlobField;
  const aHexValues: string);
var
  isEditable: Boolean;
  idx: Integer;
  bytes: TBytes;
  size: Integer;
begin
  if (((aHexValues.Length + 1) mod 3) > 0) then
    raise Exception.Create(Format('Hex Valuse have to be in format' +
      ' `00 00 00`, and actual is %s', [aHexValues]));
  size := (aHexValues.Length + 1) div 3;
  SetLength(bytes, size);
  for idx := 0 to size-1 do
  begin
    bytes[idx] := StrToInt('$' + aHexValues.Substring(idx * 3, 2));
  end;
  isEditable := aBlobField.DataSet.State in [dsEdit, dsInsert];
  if not isEditable then
  begin
    aBlobField.DataSet.Edit;
  end;
  aBlobField.Value := bytes;
  if not isEditable then
    aBlobField.DataSet.Post;
end;

// -----------------------------------------------------------------------
// Tests for: Generation of one FieldByName
// -----------------------------------------------------------------------

procedure TestGenerateAppends.GenDataWith_1xInteger;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSetWithField(fOwner, 'Level', ftInteger);
  ds.AppendRecord([1]);

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    70, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''Level'').Value := 1;'#13 +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1xDate;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSetWithField(fOwner, 'Birthday', ftDate);
  ds.AppendRecord([EncodeDate(2019, 07, 01)]);

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    DefaultRightMargin, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''Birthday'').Value := EncodeDate(2019,7,1);'#13+
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1xDateTime;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSetWithField(fOwner, 'ChangeDate', ftDateTime);
  ds.AppendRecord([EncodeDate(2019, 07, 01) + EncodeTime(15, 07, 30, 500)]);

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    DefaultRightMargin, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''ChangeDate'').Value := EncodeDate(2019,7,1)+EncodeTime(15,7,30,500);'#13
    +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1xWideString;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSetWithField(fOwner, 'MessageText', ftWideString, 30);
  ds.AppendRecord(['Alice has a ♥ cat']);

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    DefaultRightMargin, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''MessageText'').Value := ''Alice has a ♥ cat'';'#13 +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1xBCDField;
var
  ds: TFDMemTable;
  code: string;
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

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    DefaultRightMargin, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''abc123'').Value := 1.01;'#13 +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1xBlob;
var
  ds: TDataSet;
  code: string;
begin
  // https://cryptii.com/pipes/binary-to-base64
  // Bytes: A0 01 02 03 04 05 06 07
  // Base64: oAECAwQFBgc=
  ds := GivenDataSetWithField(fOwner, 'BinaryData', ftBlob);
  SetBlobFieldValue(ds.FieldByName('BinaryData') as TBlobField,
    'A0 01 02 03 04 05 06 07');

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    DefaultRightMargin, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''BinaryData'').Value := ''oAECAwQFBgc='';'#13 +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1x40BytesBlob;
var
  ds: TDataSet;
  code: string;
begin
  // https://cryptii.com/pipes/binary-to-base64
  // Bytes: A0 (39x..) 01
  // Base64: oAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQ==
  ds := GivenDataSetWithField(fOwner, 'BinaryData', ftBlob);
  SetBlobFieldValue(ds.FieldByName('BinaryData') as TBlobField,
    'A0'+DupeString(' 01',39));

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends, 70, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·(ds.FieldByName(''BinaryData'') as TBlobField).Value := '#13+
    { } '··Base64Decode('#13 +
    { } '···''oAEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQ=='');'#13 +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1x50BytesBlob;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSetWithField(fOwner, 'BinaryData', ftBlob);
  SetBlobFieldValue(ds.FieldByName('BinaryData') as TBlobField,
    '02'+DupeString(' 02',49));

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends, 60, '  ');

  Assert.AreMemosEqual(
    { } '  ds.Append;'#13 +
    { } '  ds.FieldByName(''BinaryData'').Value := '#13 +
    { } '    ''AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC'' +'#13 +
    { } '    ''AgICAgICAgICAgI='';'#13 +
    { } '  ds.Post;'#13 +
    { } '  ds.First;'#13, code);
end;

procedure TestGenerateAppends.GenDataWith_1xWithLongLitteral;
var
  ds: TDataSet;
  code: string;
begin
  ds := GivenDataSetWithField(fOwner, 'Quotation', ftWideString, 1000);
  ds.AppendRecord([
  'Here we have very long text: Lorem ipsum dolor sit amet, consectetur adipiscing elit.']);

  code := TDataBlockGenerator.Generate(ds, amMultilineAppends,
    45, '·');

  Assert.AreMemosEqual(
    { } '·ds.Append;'#13 +
    { } '·ds.FieldByName(''Quotation'').Value := '#13 +
    { } '··''Here we have very long text: Lorem ipsum ''+'#13 +
    { } '··''dolor sit amet, consectetur adipiscing ''+'#13 +
    { } '··''elit.'';'#13 +
    { } '·ds.Post;'#13 +
    { } '·ds.First;'#13, code);
end;

// -----------------------------------------------------------------------
// Tests for: Registered issues (bugs)
// -----------------------------------------------------------------------

procedure TestGenerateAppends.Iss002_GenLongStringLiterals_NewLines;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithString(fOwner, 'Info', STR_300chars);
  fGenerator.GeneratorMode := genAppend;

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual_FullReport(
    {} '  ds.Append;'#13
    {} + '  ds.FieldByName(''Info'').Value := '#13
    {} + '    ''Covers Dependency Injection, you''''ll learn about Constructor ''+'#13
    {} + '    ''Injection, Property Injection, and Method Injection and about the ''+'#13
    {} + '    ''right and wrong way to use it'';'#13
    {} + '  ds.Post;'#13
    {} + '  ds.First;'#13, actualCode);
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
    {} ' ds.Append;'#13 +
    {} ' ds.FieldByName(''Stage'').Value := 5;'#13 +
    {} ' ds.Post;'#13 +
    {} ' ds.First;'#13, actualCode);
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
    {} 'ds.Append;'#13 +
    {} 'ds.FieldByName(''Degree'').Value := 5;'#13 +
    {} 'ds.Post;'#13 +
    {} 'ds.First;'#13, actualCode);
end;

procedure TestGenerateAppends.GenIndentation_LongLiteral;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_WithString(fOwner, 'LongDescription',
    STR_300chars);
  fGenerator.GeneratorMode := genAppend;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '  ds.Append;'#13
    {} + '  ds.FieldByName(''LongDescription'').Value := '#13
    {} + '    ''Covers Dependency Injection, you''''ll learn about Constructor ''+'#13
    {} + '    ''Injection, Property Injection, and Method Injection and about the ''+'#13
    {} + '    ''right and wrong way to use it'';'#13
    {} + '  ds.Post;'#13
    {} + '  ds.First;'#13, actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: literals with RightMargin
// -----------------------------------------------------------------------

//  ---------1---------2---------3---------4---------5
//  12345678901234567890123456789012345678901234567890
//  ---------.---------.---------.---------.------|
//   ds.Append;
//   ds.FieldByName('Poem').Value :=
//     '#Lorem ipsum dolor sit amet, consectetur '+
//     'adipiscing elit. Suspendisse in '+
//     'vestibulum ante.';
//   ds.Post;
//   ds.First;

procedure TestGenerateAppends.GenWrappedString_WithMargin47;
var
  actualCode: string;
begin
  fGenerator.RightMargin := 47;
  fGenerator.DataSet := GivenDataSet_WithString(fOwner, 'Poem',
    '#Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse '+
    'in vestibulum ante.');
  fGenerator.GeneratorMode := genAppend;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '  ds.Append;'#13
    {} + '  ds.FieldByName(''Poem'').Value := '#13
    {} + '    ''#Lorem ipsum dolor sit amet, consectetur ''+'#13
    {} + '    ''adipiscing elit. Suspendisse in ''+'#13
    {} + '    ''vestibulum ante.'';'#13
    {} + '  ds.Post;'#13
    {} + '  ds.First;'#13, actualCode);
end;

//  ---------1---------2---------3---------4---------5---------6---
//  123456789012345678901234567890123456789012345678901234567890123
//  ---------.---------.---------.---------.------|
//   ds.Append;
//   ds.FieldByName('Poem').Value := 'Lorem ipsum dolor sit amet';

procedure TestGenerateAppends.GenString_WithMargin63;
var
  actualCode: string;
begin
  fGenerator.RightMargin := 63;
  fGenerator.DataSet := GivenDataSet_WithString(fOwner, 'Poem',
    'Lorem ipsum dolor sit amet');
  fGenerator.GeneratorMode := genAppend;
  fGenerator.IndentationText := '  ';

  fGenerator.Execute;
  actualCode := fGenerator.Code.Text;

  Assert.AreMemosEqual(
    {} '  ds.Append;'#13
    {} + '  ds.FieldByName(''Poem'').Value := ''Lorem ipsum dolor sit amet'';'#13
    {} + '  ds.Post;'#13
    {} + '  ds.First;'#13, actualCode);
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
    {} '  ds.Append;'#13 +
    {} '  ds.FieldByName(''id'').Value := 1;'#13 +
    {} '  ds.FieldByName(''text1'').Value := ''Alice has a cat'';'#13 +
    {} '  ds.FieldByName(''date1'').Value := EncodeDate(2019,9,16);'#13 +
    {} '  ds.FieldByName(''float1'').Value := 1.2;'#13 +
    {} '  ds.FieldByName(''currency1'').Value := 1200;'#13 +
    {} '  ds.Post;'#13 +
    {} '  ds.Append;'#13 +
    {} '  ds.FieldByName(''id'').Value := 2;'#13 +
    {} '  ds.FieldByName(''text1'').Value := ''Eva has a dog'';'#13 +
    {} '  ds.FieldByName(''currency1'').Value := 950;'#13 +
    {} '  ds.Post;'#13 +
    {} '  ds.First;'#13, actualCode);
end;

// -----------------------------------------------------------------------
// Tests for: Single Line Appends (AppendMode = amSinglelineAppends)
// -----------------------------------------------------------------------

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
    {} '  ds.AppendRecord([1, ''Alice has a cat'', EncodeDate(2019,9,16), 1.2, 1200]);'#13
    {} + '  ds.AppendRecord([2, ''Eva has a dog'', Null, Null, 950]);'#13
    {} + '  ds.First;'#13, actualCode);
end;

procedure TestGenerateAppends.GenMultipleRowDataset_MaxRows_Zero;
begin
  fGenerator.DataSet := Given_ID_Text_DataSet(fOwner,
    [[1, 'FirstRow'], [2, 'MiddleRow'], [3, 'ThirdRow'], [4, 'FourthRow'],
    [5, 'FifthRow'], [6, 'LastRow']]);
  fGenerator.AppendMode := amSinglelineAppends;
  fGenerator.GeneratorMode := genAppend;

  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} '  ds.AppendRecord([1, ''FirstRow'']);'#13 +
    {} '  ds.AppendRecord([2, ''MiddleRow'']);'#13 +
    {} '  ds.AppendRecord([3, ''ThirdRow'']);'#13 +
    {} '  ds.AppendRecord([4, ''FourthRow'']);'#13 +
    {} '  ds.AppendRecord([5, ''FifthRow'']);'#13 +
    {} '  ds.AppendRecord([6, ''LastRow'']);'#13 +
    {} '  ds.First;'#13, fGenerator.Code.Text);
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
    {} '  ds.AppendRecord([1, ''FirstRow'']);'#13 +
    {} '  ds.AppendRecord([2, ''MiddleRow'']);'#13 +
    {} '  ds.First;'#13, fGenerator.Code.Text);
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

  fGenerator.Execute;

  Assert.AreEqual(3, fGenerator.DataSet.RecNo);
end;

// -----------------------------------------------------------------------
// Tests for: Append Rows (AppendMode = amAppendRows)
// -----------------------------------------------------------------------

procedure TestGenerateAppends.GenAppendRows_OneRow;
begin
  fGenerator.DataSet := Given_ID_Text_DataSet(fOwner,[[1, 'FirstRow']]);
  fGenerator.AppendMode := amAppendRows;
  fGenerator.GeneratorMode := genAppend;

  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} '  ds.AppendRows(['#13 +
    {} '    [1, ''FirstRow'']'#13 +
    {} '  ]);'#13 +
    {} '  ds.First;'#13, fGenerator.Code.Text);
end;

procedure TestGenerateAppends.GenAppendRows_ThreeRows;
begin
  fGenerator.DataSet := Given_ID_Text_DataSet(fOwner,[
    [1, 'FirstRow'],[2, 'MiddleRow'], [3, 'LastRow']]);
  fGenerator.AppendMode := amAppendRows;
  fGenerator.GeneratorMode := genAppend;

  fGenerator.Execute;

  Assert.AreMemosEqual(
    {} '  ds.AppendRows(['#13 +
    {} '    [1, ''FirstRow''],'#13 +
    {} '    [2, ''MiddleRow''],'#13 +
    {} '    [3, ''LastRow'']'#13 +
    {} '  ]);'#13 +
    {} '  ds.First;'#13, fGenerator.Code.Text);
end;

initialization

TDUnitX.RegisterTestFixture(TestGenerateAppends);

end.
