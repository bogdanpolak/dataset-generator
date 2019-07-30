unit Test.TCreateMemTableAction;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Action.CreateMemTable;

type

  [TestFixture]
  TGenCodeDataSetMock = class(TObject)
  private
    actCreateMemTable: TCreateMemTableAction;
    mockDataSet: TFDMemTable;
    function ReplaceArrowsToEndOfLines(const s: String): string;
    function GenerateCode(ds: TDataSet): string;
    procedure AssertAreEqualOneFieldTemplateToMock(const fldType: string;
      fldSize: integer; const fldValue: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestOneIntegerField;
    [Test]
    procedure TestOneWideStringField;
    [Test]
    procedure TestOneDateTimeField_DateOnly;
    [Test]
    procedure TestOneDateTimeField_DateTime;
    [Test]
    procedure TestSample1;
  end;

implementation

uses
  System.Variants;

// -----------------------------------------------------------------------
// Utils section
// -----------------------------------------------------------------------

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

procedure TGenCodeDataSetMock.AssertAreEqualOneFieldTemplateToMock
  (const fldType: string; fldSize: integer; const fldValue: string);
var
  ft: string;
  sExpected: string;
  aActual: string;
begin
  if fldSize > 0 then
    ft := fldType + ', ' + fldSize.ToString
  else
    ft := fldType;
  sExpected := ReplaceArrowsToEndOfLines(Format(CodeTemplateOneField,
    [ft, fldValue]));
  aActual := GenerateCode(mockDataSet);
  Assert.AreEqual(sExpected, aActual);
end;

function TGenCodeDataSetMock.GenerateCode(ds: TDataSet): string;
begin
  actCreateMemTable.GenerateCode(ds);
  Result := actCreateMemTable.Code.Text;
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
  actCreateMemTable := TCreateMemTableAction.Create(nil);
  mockDataSet := TFDMemTable.Create(nil);
end;

procedure TGenCodeDataSetMock.TearDown;
begin
  FreeAndNil(actCreateMemTable);
  FreeAndNil(mockDataSet);
end;

// -----------------------------------------------------------------------
// Test section
// -----------------------------------------------------------------------

procedure TGenCodeDataSetMock.TestOneDateTimeField_DateOnly;
begin
  with mockDataSet do
  begin
    FieldDefs.Add('f1', ftDateTime);
    CreateDataSet;
    AppendRecord([EncodeDate(2019, 07, 01)]);
    First;
  end;
  AssertAreEqualOneFieldTemplateToMock('ftDateTime', 0, 'EncodeDate(2019,7,1)');
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
  AssertAreEqualOneFieldTemplateToMock('ftDateTime', 0,
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
  AssertAreEqualOneFieldTemplateToMock('ftInteger', 0, '1');
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
  AssertAreEqualOneFieldTemplateToMock('ftWideString', 20,
    QuotedStr('Alice has a cat'));
end;

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

initialization

TDUnitX.RegisterTestFixture(TGenCodeDataSetMock);

end.
