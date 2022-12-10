unit DataSet.Promotions;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Data.DB;

function CreatePromotionsDataSet(aOwner: TComponent): TDataSet;

implementation

uses
  FireDAC.Comp.Client;

function CreatePromotionsDataSet(aOwner: TComponent): TDataSet;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(aOwner);
  with ds do
  begin
    FieldDefs.Add('Year', ftInteger);
    FieldDefs.Add('Month', ftInteger);
    FieldDefs.Add('CategoryIds', ftString, 50);
    CreateDataSet;
  end;
  ds.AppendRecord([1998, 5, '1,7,8']);
  ds.AppendRecord([1998, 4, '1,7,8']);
  ds.AppendRecord([1998, 3, '1,7,8']);
  ds.AppendRecord([1998, 2, '1,7,8']);
  ds.AppendRecord([1998, 1, '8']);
  ds.First;
  Result := ds;
end;

end.
