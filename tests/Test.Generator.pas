unit Test.Generator;

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
  TestDSGenerator = class(TObject)
  private
    fGenerator: TDSGeneratorUnderTest;
    fOwner: TComponent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure GenerateHistoricalEvents;
  end;

implementation

uses
  System.Variants,
  Data.FmtBcd;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestDSGenerator.Setup;
begin
  fGenerator := TDSGeneratorUnderTest.Create(nil);
  fOwner := TComponent.Create(nil);
end;

procedure TestDSGenerator.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
end;

// -----------------------------------------------------------------------
// Dataset factories
// -----------------------------------------------------------------------

function GivenDataSet_HistoricalEvents(aOwner: TComponent): TDataSet;
var
  memTable: TFDMemTable;
begin
  memTable := TFDMemTable.Create(aOwner);
  with memTable do
  begin
    FieldDefs.Add('EventID', ftInteger);
    FieldDefs.Add('Event', ftWideString, 50);
    FieldDefs.Add('Date', ftDate);
    FieldDefs.Add('Expirence', ftFloat);
    FieldDefs.Add('Income', ftCurrency);
    CreateDataSet;
    AppendRecord([1, 'Liberation of Poland', EncodeDate(1989, 06, 04),
      1.2, 120]);
    AppendRecord([2, 'Battle of Vienna', EncodeDate(1683, 09, 12),
      System.Variants.Null, Null]);
    First;
  end;
  Result := memTable;
end;

// -----------------------------------------------------------------------
// Tests for: Generate Sample historical events code
// -----------------------------------------------------------------------

procedure TestDSGenerator.GenerateHistoricalEvents;
var
  actualCode: string;
begin
  fGenerator.DataSet := GivenDataSet_HistoricalEvents(fOwner);

  fGenerator.Execute;
  actualCode := fGenerator.CodeWithStructure.Text +
    fGenerator.CodeWithAppendData.Text;

  Assert.AreMemosEqual(
    (* *) '  ds := TFDMemTable.Create(AOwner);'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    FieldDefs.Add(''EventID'', ftInteger);'#13 +
    (* *) '    FieldDefs.Add(''Event'', ftWideString, 50);'#13 +
    (* *) '    FieldDefs.Add(''Date'', ftDate);'#13 +
    (* *) '    FieldDefs.Add(''Expirence'', ftFloat);'#13 +
    (* *) '    FieldDefs.Add(''Income'', ftCurrency);'#13 +
    (* *) '    CreateDataSet;'#13 +
    (* *) '  end;'#13 +
    (* *) '{$REGION ''Append data''}'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    Append;'#13 +
    (* *) '    FieldByName(''EventID'').Value := 1;'#13 +
    (* *) '    FieldByName(''Event'').Value := ''Liberation of Poland'';'#13 +
    (* *) '    FieldByName(''Date'').Value := EncodeDate(1989,6,4);'#13 +
    (* *) '    FieldByName(''Expirence'').Value := 1.2;'#13 +
    (* *) '    FieldByName(''Income'').Value := 120;'#13 +
    (* *) '    Post;'#13 +
    (* *) '  end;'#13 +
    (* *) '  with ds do'#13 +
    (* *) '  begin'#13 +
    (* *) '    Append;'#13 +
    (* *) '    FieldByName(''EventID'').Value := 2;'#13 +
    (* *) '    FieldByName(''Event'').Value := ''Battle of Vienna'';'#13 +
    (* *) '    FieldByName(''Date'').Value := EncodeDate(1683,9,12);'#13 +
    (* *) '    Post;'#13 +
    (* *) '  end;'#13 +
    (* *) '{$ENDREGION}'#13, actualCode);
end;

end.
