# DataSet to Delphi Code

## Overview

Generate Delphi source code creating mock dataset (TFDMemTable) based on any TDataSet.

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

Event `Button1Click` will generate followin code in the Memo1 control:

```text
ds := TFDMemTable.Create(AOwner);
with ds do
begin
  FieldDefs.Add(''id'', ftInteger);
  FieldDefs.Add(''text1'', ftWideString, 30);
  FieldDefs.Add(''date1'', ftDate);
  FieldDefs.Add(''float1'', ftFloat);
  FieldDefs.Add(''currency1'', ftCurrency);
  CreateDataSet;
end;
with ds do
begin
  Append;
  FieldByName(''id'').Value := 1;
  FieldByName(''text1'').Value := ''Ala ma kota'';
  FieldByName(''date1'').Value := EncodeDate(2019,9,16);
  FieldByName(''float1'').Value := 1.2;
  FieldByName(''currency1'').Value := 1200;
  Post;
end;
with ds do
begin
  Append;
  FieldByName(''id'').Value := 2;
  FieldByName(''text1'').Value := ''Ala ma kota'';
  FieldByName(''currency1'').Value := 950;
  Post;
end;
```