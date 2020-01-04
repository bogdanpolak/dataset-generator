unit Test.CodeWithStructure;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode;

{$M+}

type

  [TestFixture]
  TTestCodeWithStructure = class(TObject)
  private
    fGenerator: TGenerateDataSetCode;
    fOwner: TComponent;
    fExpectedCode: TStringList;
    procedure AreCodesEqual(expectedCode: TStrings; actualCode: TStrings);
    function GivenSampleDataSetWithTwoRows(aOwner: TComponent): TDataSet;
    function ReplaceArrowsAndDiamonds(const s: String): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    // -------------
    procedure TestOneBCDField_DifferentFieldName;
    // -------------
    procedure TestOneIntegerField;
    procedure TestOneWideStringField;
    procedure TestOneDateTimeField_DateOnly;
    procedure TestOneDateTimeField_DateTime;
    procedure TestOneBCDField;
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

const
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

function TTestCodeWithStructure.ReplaceArrowsAndDiamonds(const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', fGenerator.IndentationText,
    [rfReplaceAll])
end;

procedure TTestCodeWithStructure.AreCodesEqual(expectedCode: TStrings;
  actualCode: TStrings);
begin
  Assert.AreEqual(expectedCode.Text, actualCode.Text);
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TTestCodeWithStructure.Setup;
begin
  fGenerator := TGenerateDataSetCode.Create(nil);
  fOwner := TComponent.Create(nil);
  fExpectedCode := TStringList.Create;
end;

procedure TTestCodeWithStructure.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
  fExpectedCode.Free;
end;


// -----------------------------------------------------------------------
// Tests for: One DB field with one value
// -----------------------------------------------------------------------
{$REGION 'One DB field with one value'}

procedure TTestCodeWithStructure.TestOneDateTimeField_DateOnly;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftDateTime', 'EncodeDate(2019,7,1)']));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TTestCodeWithStructure.TestOneDateTimeField_DateTime;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftDateTime', 'EncodeDate(2019,7,1)+EncodeTime(15,7,30,500)']));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TTestCodeWithStructure.TestOneIntegerField;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftInteger', '1']));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TTestCodeWithStructure.TestOneWideStringField;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneField,
    ['ftWideString, 20', QuotedStr('Alice has a cat')]));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TTestCodeWithStructure.TestOneBCDField;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneBcdField,
    ['f1', 'ftBCD', 10, 4]));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

procedure TTestCodeWithStructure.TestOneBCDField_DifferentFieldName;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneBcdField,
    ['abc123', 'ftBCD', 8, 2]));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: property IndentationText
// -----------------------------------------------------------------------
{$REGION 'property IndentationText'}

procedure TTestCodeWithStructure.Test_Indentation_1Space;
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

procedure TTestCodeWithStructure.Test_Indentation_Empty;
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

procedure TTestCodeWithStructure.Test_Indentation_MultilineTextValue;
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

procedure TTestCodeWithStructure.Test_IndentationText_BCDField;
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

  fExpectedCode.Text := ReplaceArrowsAndDiamonds(Format(CodeTemplateOneBcdField,
    ['xyz123', 'ftBCD', 8, 2]));
  AreCodesEqual(fExpectedCode, fGenerator.CodeWithStructure);
end;

{$ENDREGION}
// -----------------------------------------------------------------------
// Tests for: Sample1
// -----------------------------------------------------------------------
{$REGION 'Sample1 : dataset with 4 fields and 2 rows containing NULL values'}

function TTestCodeWithStructure.GivenSampleDataSetWithTwoRows
  (aOwner: TComponent): TDataSet;
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
    AppendRecord([1, 'Alice has a cat', EncodeDate(2019, 09, 16),
      1.2, 1200]);
    AppendRecord([2, 'Eva has a dog', System.Variants.Null, Null, 950]);
    First;
  end;
  Result := memTable;
end;

procedure TTestCodeWithStructure.TestSample1;
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

{$ENDREGION}

initialization

TDUnitX.RegisterTestFixture(TTestCodeWithStructure);

end.
