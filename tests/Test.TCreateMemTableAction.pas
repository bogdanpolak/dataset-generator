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
  TCreateMockTest = class(TObject)
  private
    actCreateMemTable: TCreateMemTableAction;
    mockDataSet: TFDMemTable;
    function CreateDataSetSample1(Owner: TComponent): TDataSet;
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
    procedure TestSample1;
  end;

implementation

uses
  System.Variants;

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

procedure TCreateMockTest.AssertAreEqualOneFieldTemplateToMock
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

function TCreateMockTest.CreateDataSetSample1(Owner: TComponent): TDataSet;
begin
  Result := TFDMemTable.Create(Owner);
  with Result as TFDMemTable do
  begin
    // FieldDefs.Add('bDelete', ftBoolean);
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
end;

function TCreateMockTest.GenerateCode(ds: TDataSet): string;
begin
  actCreateMemTable.GenerateCode(ds);
  Result := actCreateMemTable.Code.Text;
end;

function TCreateMockTest.ReplaceArrowsToEndOfLines(const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll])
end;

procedure TCreateMockTest.Setup;
begin
  actCreateMemTable := TCreateMemTableAction.Create(nil);
  mockDataSet := TFDMemTable.Create(nil);
end;

procedure TCreateMockTest.TearDown;
begin
  FreeAndNil(actCreateMemTable);
  FreeAndNil(mockDataSet);
end;

procedure TCreateMockTest.TestOneIntegerField;
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

procedure TCreateMockTest.TestOneWideStringField;
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

procedure TCreateMockTest.TestSample1;
begin
  actCreateMemTable.GenerateCode(CreateDataSetSample1(actCreateMemTable));
  Assert.AreEqual(ReplaceArrowsToEndOfLines(
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
    (* *) 'end;→'), actCreateMemTable.Code.Text);
end;

initialization

TDUnitX.RegisterTestFixture(TCreateMockTest);

end.
