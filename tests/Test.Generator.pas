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
    fStringStream: TStringStream;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure GenerateUnit_NilDataSet;
    procedure Generate_UnitHeader;
    procedure Generate_HistoricalEvents;
    procedure GenerateToStream_StringDataSet;
    procedure GenerateToFile_UnitName;
    procedure GenerateUnit_Header;
    procedure GenerateUnit_Header_ClientDataSet;
    procedure GenerateUnit_Footer;
    procedure GenerateFunction;
    procedure GenerateFunction_ClientDataSet;
  end;

implementation

uses
  System.Variants,
  System.IOUtils,
  Data.FmtBcd;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

function GetFirstLineFromMemo(const sMemoText: string): string;
var
  i1: Integer;
  i2: Integer;
  idx: Integer;
begin
  i1 := sMemoText.IndexOf(#10);
  i2 := sMemoText.IndexOf(#13);
  if i1 = -1 then
    idx := i2
  else if i2 = -1 then
    idx := i1
  else
    idx := Min(i1, i2);
  if idx = -1 then
    Result := ''
  else
    Result := sMemoText.Substring(0, idx);
end;

// -----------------------------------------------------------------------
// Setup and TearDown section
// -----------------------------------------------------------------------

procedure TestDSGenerator.Setup;
begin
  fGenerator := TDSGeneratorUnderTest.Create(nil);
  fOwner := TComponent.Create(nil);
  fStringStream := TStringStream.Create('', TEncoding.UTF8);
end;

procedure TestDSGenerator.TearDown;
begin
  fGenerator.Free;
  fOwner.Free;
  fStringStream.Free;
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

function GivenDataSet_MiniHistoricalEvents(aOwner: TComponent): TDataSet;
var
  memTable: TFDMemTable;
begin
  memTable := TFDMemTable.Create(aOwner);
  with memTable do
  begin
    FieldDefs.Add('EventID', ftInteger);
    FieldDefs.Add('Event', ftWideString, 50);
    FieldDefs.Add('Date', ftDate);
    CreateDataSet;
    AppendRecord([1, 'Liberation of Poland', EncodeDate(1989, 06, 04)]);
    AppendRecord([2, 'Battle of Vienna', EncodeDate(1683, 09, 12)]);
    First;
  end;
  Result := memTable;
end;

function GivenDataSet_WithString(aOwner: TComponent; const aFieldName: string;
  const aDataValue: string): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add(aFieldName, ftWideString, 30);
    CreateDataSet;
    AppendRecord([aDataValue]);
    First;
  end;
  Result := ds;
end;

// -----------------------------------------------------------------------
// Tests for: Generate Header / Unit / Function
// -----------------------------------------------------------------------

procedure TestDSGenerator.GenerateUnit_NilDataSet;
begin
  TDSGenerator.GenerateAsString(nil);

  Assert.Pass;
end;

procedure TestDSGenerator.Generate_UnitHeader;
var
  actualCode: string;
begin
  actualCode := fGenerator.TestGenUnitHeader('Fake.HistoricalEvents');

  Assert.AreMemosEqual(
    {} 'unit Fake.HistoricalEvents;'#13 +
    {} #13 +
    {} 'interface'#13 +
    {} #13 +
    {} 'uses'#13 +
    {} '  System.Classes,'#13 +
    {} '  System.SysUtils,'#13 +
    {} '  System.Variants,'#13 +
    {} '  Data.DB,'#13 +
    {} '  FireDAC.Comp.Client;'#13 +
    {} #13 +
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13
    {} + ''#13 +
    {} 'implementation'#13 +
    {} ''#13, actualCode);
end;

procedure TestDSGenerator.Generate_HistoricalEvents;
var
  ds: TDataSet;
  actualCode: string;
begin
  ds := GivenDataSet_HistoricalEvents(fOwner);

  actualCode := TDSGenerator.GenerateAsString(ds);

  Assert.AreMemosEqual(
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} 'var'#13 +
    {} '  ds: TFDMemTable;'#13 +
    {} 'begin'#13 +
    {} '  ds := TFDMemTable.Create(AOwner);'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    FieldDefs.Add(''EventID'', ftInteger);'#13 +
    {} '    FieldDefs.Add(''Event'', ftWideString, 50);'#13 +
    {} '    FieldDefs.Add(''Date'', ftDate);'#13 +
    {} '    FieldDefs.Add(''Expirence'', ftFloat);'#13 +
    {} '    FieldDefs.Add(''Income'', ftCurrency);'#13 +
    {} '    CreateDataSet;'#13 +
    {} '  end;'#13 +
    {} '{$REGION ''Append data''}'#13 +
    {} '  with ds do'#13#10 +
    {} '  begin'#13#10 +
    {} '    Append;'#13#10 +
    {} '    FieldByName(''EventID'').Value := 1;'#13#10 +
    {} '    FieldByName(''Event'').Value := ''Liberation of Poland'';'#13#10 +
    {} '    FieldByName(''Date'').Value := EncodeDate(1989,6,4);'#13#10 +
    {} '    FieldByName(''Expirence'').Value := 1.2;'#13#10 +
    {} '    FieldByName(''Income'').Value := 120;'#13#10 +
    {} '    Post;'#13#10 +
    {} '  end;'#13#10 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''EventID'').Value := 2;'#13 +
    {} '    FieldByName(''Event'').Value := ''Battle of Vienna'';'#13 +
    {} '    FieldByName(''Date'').Value := EncodeDate(1683,9,12);'#13 +
    {} '    Post;'#13 +
    {} '  end;'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13 +
    {} '  Result := ds;'#13 +
    {} 'end;'#13, actualCode);
end;

procedure TestDSGenerator.GenerateToStream_StringDataSet;
var
  ds: TDataSet;
  actualCode: string;
begin
  ds := GivenDataSet_WithString(fOwner, 'CyrlicText',
    'Все люди рождаются свободными');

  TDSGenerator.GenerateAndSaveToStream(ds, fStringStream);
  actualCode := fStringStream.DataString;
  Assert.AreMemosEqual(
    {} 'unit uSampleDataSet;'#13 +
    {} #13 +
    {} 'interface'#13 +
    {} #13 +
    {} 'uses'#13 +
    {} '  System.Classes,'#13 +
    {} '  System.SysUtils,'#13 +
    {} '  System.Variants,'#13 +
    {} '  Data.DB,'#13 +
    {} '  FireDAC.Comp.Client;'#13 +
    {} #13 +
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} #13 +
    {} 'implementation'#13 +
    {} #13 +
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} 'var'#13 +
    {} '  ds: TFDMemTable;'#13 +
    {} 'begin'#13 +
    {} '  ds := TFDMemTable.Create(AOwner);'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    FieldDefs.Add(''CyrlicText'', ftWideString, 30);'#13 +
    {} '    CreateDataSet;'#13 +
    {} '  end;'#13 +
    {} '{$REGION ''Append data''}'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''CyrlicText'').Value := ''Все люди рождаются свободными'';'#13
    {} + '    Post;'#13 +
    {} '  end;'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13 +
    {} '  Result := ds;'#13 +
    {} 'end;'#13 +
    {} #13 +
    {} 'end.'#13, actualCode);
end;

procedure TestDSGenerator.GenerateToFile_UnitName;
var
  aFileName: string;
  actualLine: string;
begin
  aFileName := System.IOUtils.TPath.GetTempPath + 'FakeDataSet.Historical.pas';

  TDSGenerator.GenerateAndSaveToFile(nil, aFileName);
  actualLine := GetFirstLineFromMemo(TFile.ReadAllText(aFileName));

  Assert.AreEqual('unit FakeDataSet.Historical;', actualLine);
end;

procedure TestDSGenerator.GenerateUnit_Header;
var
  actualCode: string;
begin
  actualCode := fGenerator.TestGenUnitHeader('Unit1');
  Assert.AreMemosEqual(
    {} 'unit Unit1;'#13 +
    {} #13 +
    {} 'interface'#13 +
    {} #13 +
    {} 'uses'#13 +
    {} '  System.Classes,'#13 +
    {} '  System.SysUtils,'#13 +
    {} '  System.Variants,'#13 +
    {} '  Data.DB,'#13 +
    {} '  FireDAC.Comp.Client;'#13 +
    {} #13 +
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} #13 +
    {} 'implementation'#13 +
    {} #13, actualCode);
  // MidasLib, Datasnap.DBClient
end;

procedure TestDSGenerator.GenerateUnit_Header_ClientDataSet;
var
  actualCode: string;
begin
  fGenerator.DataSetType := dstClientDataSet;
  actualCode := fGenerator.TestGenUnitHeader('Unit1');
  Assert.AreMemosEqual(
    {} 'unit Unit1;'#13 +
    {} #13 +
    {} 'interface'#13 +
    {} #13 +
    {} 'uses'#13 +
    {} '  System.Classes,'#13 +
    {} '  System.SysUtils,'#13 +
    {} '  System.Variants,'#13 +
    {} '  Data.DB,'#13 +
    {} '  Datasnap.DBClient;'#13 +
    {} '  MidasLib;'#13 +
    {} #13 +
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} #13 +
    {} 'implementation'#13 +
    {} #13, actualCode);
  //
end;

procedure TestDSGenerator.GenerateUnit_Footer;
var
  actualCode: string;
begin
  actualCode := fGenerator.TestGenUnitFooter;
  Assert.AreMemosEqual(
    {} #13 +
    {} 'end.'#13, actualCode);
end;

procedure TestDSGenerator.GenerateFunction;
var
  actualCode: string;
begin
  fGenerator.dataSet := GivenDataSet_WithString(fOwner, 'CyrlicText',
    'Все люди рождаются свободными');

  actualCode := fGenerator.TestGenFunction;

  Assert.AreMemosEqual(
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} 'var'#13 +
    {} '  ds: TFDMemTable;'#13 +
    {} 'begin'#13 +
    {} '  ds := TFDMemTable.Create(AOwner);'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    FieldDefs.Add(''CyrlicText'', ftWideString, 30);'#13 +
    {} '    CreateDataSet;'#13 +
    {} '  end;'#13 +
    {} '{$REGION ''Append data''}'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    Append;'#13 +
    {} '    FieldByName(''CyrlicText'').Value :=' +
    ' ''Все люди рождаются свободными'';'#13 +
    {} '    Post;'#13 +
    {} '  end;'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13 +
    {} '  Result := ds;'#13 +
    {} 'end;'#13, actualCode);
end;

procedure TestDSGenerator.GenerateFunction_ClientDataSet;
var
  actualCode: string;
begin
  fGenerator.dataSet := GivenDataSet_MiniHistoricalEvents(fOwner);
  fGenerator.DataSetType := dstClientDataSet;
  fGenerator.AppendMode := amSinglelineAppends;

  actualCode := fGenerator.TestGenFunction;

  Assert.AreMemosEqual(
    {} 'function GivenDataSet (aOwner: TComponent): TDataSet;'#13 +
    {} 'var'#13 +
    {} '  ds: TClientDataSet;'#13 +
    {} 'begin'#13 +
    {} '  ds := TClientDataSet.Create(AOwner);'#13 +
    {} '  with ds do'#13 +
    {} '  begin'#13 +
    {} '    FieldDefs.Add(''EventID'', ftInteger);'#13 +
    {} '    FieldDefs.Add(''Event'', ftWideString, 50);'#13 +
    {} '    FieldDefs.Add(''Date'', ftDate);'#13 +
    {} '    CreateDataSet;'#13 +
    {} '  end;'#13 +
    {} '{$REGION ''Append data''}'#13 +
    {} '  ds.AppendRecord([1, ''Liberation of Poland'',' +
    ' EncodeDate(1989,6,4)]);'#13 +
    {} '  ds.AppendRecord([2, ''Battle of Vienna'',' +
    ' EncodeDate(1683,9,12)]);'#13 +
    {} '  ds.First;'#13 +
    {} '{$ENDREGION}'#13 +
    {} '  Result := ds;'#13 +
    {} 'end;'#13, actualCode);
end;

end.
