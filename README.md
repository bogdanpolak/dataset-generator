# DataSet to Delphi Code

---------------------------------------------------------------
PLAN

1) TDSGenerator component usage
1) Methods:
   - `GenerateAsString`
   - `GenerateAsArray`
   - `GenerateAndSaveToStream / GenerateAndSaveToFile`
   - `GenerateAndSaveToClipboard`
1) Options
   - `IdentationText`
   - `GenerateCode`
   - `AppendMode`
   - `GenerateDataSet`
1) Component Class `TDSGenerator`
1) Update project sample

--------------------------------------------------------------

## Overview

DataSet Generator is a component that generates a Delphi code using any dataset provided as an input parameter. Component output is a function which creates and populates in-memory dataset according to structure and content of the provided dataset. The main reason for building this component was ability to generate quickly fake datasets for unit testing purposes.

## Generator usage

To create a fake dataset developer needs to include unit `Comp.Generator.DataSetCode.pas` in uses section:

```pas
uses
  Comp.Generator.DataSetCode;
```

Finds a production dataset which need to be faked and call any of generator's class methods like `GenerateAndSaveToFile`:

```pas
begin
  aDataSet := fDataSetFactory.ConstructSelectDataset(
    SQL_SELECT_CustomerOrders_FromOneMonth, 
    [aYear, aMonth] );
  // ------------------------------------
  // injected generator call:
  TDSGenerator.GenerateAndSaveToFile(
    aDataSet,
    'Fake.CustomerOrders.pas');
  // ------------------------------------
  fOrdersView.SetMasterDataset(aDataSet);
end;
```

Using generated method `CreateDataSet` developer is able to write unit test for the class `TOrdersView`:

```pas
procedure TestOrdersView.Setup;
begin
  fOwner := TComponent.Create;
  fOrdersView := TOrdersView.Create(
    TMock<IOrdersModel>.Create);
  // ...
end;

procedure TestOrdersView.Test_CalculateCurrentImpact;
begin
  fOrdersView.SetMasterDataset(
    CreateDataSet(fOwner));
  
  actual := fOrdersView.GentCurrentImpact;

  Assert.AreEqual(25.5, actual, 0.000001);
end;
```

## Sample - component usage

```pas
function CreateDataSet (AOwner: TComponent): Data.DB.TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(AOwner);
  with ds do
  begin
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    FieldDefs.Add('date1', ftDate);
    FieldDefs.Add('float1', ftFloat);
    FieldDefs.Add('currency1', ftCurrency);
    CreateDataSet;
    AppendRecord([1, 'Ala ma kota', EncodeDate(2019, 09, 16), 1.2, 1200]);
    AppendRecord([2, 'Ala ma kota', System.Variants.Null, Null, 950]);
  end;
  ds.First;
  Result := ds;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Generator: TGenerateDataSetCode;
begin
  Generator := TGenerateDataSetCode.Create(Self);
  Generator.DataSet := CreateDataSet(Generator);
  GenerateDataSetCode.Execute;
  Memo1.Lines := GenerateDataSetCode.Code;
  Generator.Free;
end;
```

Event `Button1Click` will generate the following code in the Memo1 control:

```pas
ds := TFDMemTable.Create(AOwner);
with ds do
begin
  FieldDefs.Add('id', ftInteger);
  FieldDefs.Add('text1', ftWideString, 30);
  FieldDefs.Add('date1', ftDate);
  FieldDefs.Add('float1', ftFloat);
  FieldDefs.Add('currency1', ftCurrency);
  CreateDataSet;
end;
with ds do
begin
  Append;
  FieldByName('id').Value := 1;
  FieldByName('text1').Value := 'Ala ma kota';
  FieldByName('date1').Value := EncodeDate(2019,9,16);
  FieldByName('float1').Value := 1.2;
  FieldByName('currency1').Value := 1200;
  Post;
end;
with ds do
begin
  Append;
  FieldByName('id').Value := 2;
  FieldByName('text1').Value := 'Ala ma kota';
  FieldByName('currency1').Value := 950;
  Post;
end;
```

## Testing with DataSet Generator

TBD: Describe usage scenario (unit tests)

## Fakes vs mocks

TBD:

article: https://blog.pragmatists.com/test-doubles-fakes-mocks-and-stubs-1a7491dfa3da

Fakes are objects that have working implementations, but not same as production one. Usually they take some shortcut and have simplified version of production code.

Mocks are objects that register calls they receive.
In test assertion we can verify on Mocks that all expected actions were performed.

We use mocks when we don’t want to invoke production code or when there is no easy way to verify, that intended code was executed. There is no return value and no easy way to check system state change. An example can be a functionality that calls e-mail sending service.

We don’t want to send e-mails each time we run a test. Moreover, it is not easy to verify in tests that a right email was send. Only thing we can do is to verify the outputs of the functionality that is exercised in our test. In other worlds, verify that e-mail sending service was called.