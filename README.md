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

DataSet Generator is used to create a fake in-memory dataset to bypass usage of SQL datasets in a test code. This scenario is quite straightforward: 

**Step 1.** Create a new fake factory:

Find a production dataset which need to be faked and run generator, the easiest way is to call one of its class methods:

```pas
uses
  Comp.Generator.DataSetCode;
```
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

**Step 2)** Use this fake:

In the sample code above was used a view object: `fOrdersView` of the class `TOrdersView`. This view class uses a dataset injected via method `SetMasterDataset` to calculate a date (period of the year) impact on a total monthly sale value. Because above code is dependent on a SQL connection and a SQL database (dataset provided by `fDataSetFactory`) you need a fake to put a method `TOrdersView.GetCurrentImpact` into unit test harness:

```pas
procedure TestOrdersView.Setup;
begin
  fOwner := TComponent.Create;
  fOrderModelMock := TMock<IOrdersModel>.Create;
  fOrdersView := TOrdersView.Create(fOrderModelMock);
  // ...
end;

procedure TestOrdersView.Test_CalculateCurrentImpact;
var
  aDataSet: TDataSet;
  actualImpact: Extended;
begin
  aDataSet := CreateDataSet(fOwner);
  fOrdersView.SetMasterDataset(aDataSet);
  
  actualImpact := fOrdersView.GetCurrentImpact;

  Assert.AreEqual(0.054, actualImpact, 0.0000001);
end;
```

## Sample generated fake factory

```pas
function CreateDataSet (aOwner: TComponent): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(AOwner);
  with ds do
  begin
    FieldDefs.Add('Id', ftInteger);
    FieldDefs.Add('Name', ftWideString, 30);
    FieldDefs.Add('RegistrationDate', ftDate);
    FieldDefs.Add('Balance', ftFloat);
    FieldDefs.Add('Budget', ftCurrency);
    CreateDataSet;
  end;
{$REGION ''Append data''}
  ds.AppendRecord([1, 'Team integration', EncodeDate(2019, 09, 16), 1.2, 1200]);
  ds.AppendRecord([2, 'Progress retrospective', Null, Null, 950]);
  ds.First;
{$ENDREGION}
  Result := ds;
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