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

// -----------------------------------------------------------------------
// Utils section
// -----------------------------------------------------------------------

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

procedure TGenCodeDataSetMock.TestOneIntegerField;
begin
end;

procedure TGenCodeDataSetMock.TestOneWideStringField;
begin
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
