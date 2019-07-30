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
begin
end;

initialization

TDUnitX.RegisterTestFixture(TGenCodeDataSetMock);

end.
