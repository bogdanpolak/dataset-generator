unit Data.DataModule1;

interface

uses
  System.SysUtils,
  System.Math,
  System.StrUtils,
  System.Classes,
  System.Variants,
  System.DateUtils,
  Data.DB,
  Spring,
  Spring.Collections,
  {FireDAC}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLiteWrapper.Stat;

type
  TEmployee = class
    EmployeeId: Integer;
    FirstName: string;
    LastName: string;
    FullName: string;
    Title: string;
    HireDate: TDateTime;
    Country: string;
    City: string;
  end;

  TEmployeeScore = record
    OrderCount: Integer;
    OrderValues: TArray<Currency>;
  end;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    fdqDetailsInMonth: TFDQuery;
  public
    constructor Create(aOwner: TComponent); override;
    procedure Connect();
    function IsConnected(): boolean;
    function GetActiveMonths: IEnumerable<string>;
    function GetDataSet_DetailsInMonth(
      const aEmployeeId: Integer;
      const aYear: Word;
      const aMonth: Word): TDataSet;
    function GetEmployees: IEnumerable<TEmployee>;
    function CalculateMonthlyScore(
      const aEmployeeId: Integer;
      const aYear: Word;
      const aMonth: Word): TEmployeeScore;
  private
    function GetDetailsItemTotal(const aDetailsDataSet: TDataSet): Currency;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

constructor TDataModule1.Create(aOwner: TComponent);
begin
  inherited;
  Assert(FDConnection1.Connected = False,
    'Error! Connection to database was active before opening');
end;

procedure TDataModule1.Connect();
begin
  FDConnection1.Open();
end;

function IffString(
  condition: boolean;
  const textTrue, textFalse: string): string;
begin
  if condition then
    Result := textTrue
  else
    Result := textFalse;
end;

function TDataModule1.IsConnected: boolean;
begin
  Result := FDConnection1.Connected;
end;

function TDataModule1.GetActiveMonths: IEnumerable<string>;
var
  varMinDate: Variant;
  varMaxDate: Variant;
  aDate: TDateTime;
  aEndDate: TDateTime;
  months: IList<string>;
begin
  months := TCollections.CreateList<string>();
  varMinDate := FDConnection1.ExecSQLScalar
    ('SELECT Min(OrderDate) FROM {id Orders}');
  varMaxDate := FDConnection1.ExecSQLScalar
    ('SELECT Max(OrderDate) FROM {id Orders}');
  if varMinDate = System.Variants.Null then
    Exit;
  if varMaxDate = System.Variants.Null then
    Exit;
  aDate := RecodeDay(VarToDateTime(varMinDate), 1);
  aEndDate := VarToDateTime(varMaxDate);
  while aDate <= aEndDate do
  begin
    months.Add(FormatDateTime('yyyy-mm', aDate));
    aDate := IncMonth(aDate, 1);
  end;
  months.Reverse;
  Result := months;
end;

function TDataModule1.GetDataSet_DetailsInMonth(
  const aEmployeeId: Integer;
  const aYear: Word;
  const aMonth: Word): TDataSet;
var
  sql: string;
begin
  sql := 'SELECT O.EmployeeId, {Year(O.OrderDate)} Year' +
    '   , {Month(O.OrderDate)} Month, OD.OrderId, OD.ProductId' +
    '   , P.CategoryId, OD.UnitPrice, OD.Quantity, OD.Discount' +
    ' FROM Orders O' +
    ' INNER JOIN {id Order Details} OD ON O.OrderId = OD.OrderId' +
    ' INNER JOIN Products P ON P.ProductId = OD.ProductId' +
    ' WHERE EmployeeId = :EmployeeId and Year = :Year and Month = :Month' +
    ' ORDER BY OD.OrderID';
  fdqDetailsInMonth.Open(sql, [aEmployeeId, aYear, aMonth]);
  Result := fdqDetailsInMonth;
end;

function TDataModule1.GetEmployees: IEnumerable<TEmployee>;
var
  ds: TDataSet;
  employee: TEmployee;
  employees: IList<TEmployee>;
begin
  employees := TCollections.CreateObjectList<TEmployee>;
  FDConnection1.ExecSQL('SELECT * FROM Employees', ds);
  try
    while not ds.Eof do
    begin
      employee := TEmployee.Create;
      with employee do
      begin
        EmployeeId := ds.FieldByName('EmployeeId').AsInteger;
        FirstName := ds.FieldByName('FirstName').AsString;
        LastName := ds.FieldByName('LastName').AsString;
        FullName := FirstName + ' ' + LastName;
        Title := ds.FieldByName('Title').AsString;
        HireDate := ds.FieldByName('HireDate').AsDateTime;
        Country := ds.FieldByName('Country').AsString;
        City := ds.FieldByName('City').AsString;
      end;
      employees.Add(employee);
      ds.Next;
    end;
    Result := employees;
  finally
    ds.Free;
  end;
end;

function TDataModule1.CalculateMonthlyScore(
  const aEmployeeId: Integer;
  const aYear: Word;
  const aMonth: Word): TEmployeeScore;
var
  detailsDataSet: TDataSet;
  totalOrderValue: Currency;
  itemTotal: Currency;
  orderId: Integer;
  currentOrderId: Integer;
  scores: IList<Currency>;
begin
  detailsDataSet := GetDataSet_DetailsInMonth(aEmployeeId, aYear, aMonth);
  totalOrderValue := 0;
  currentOrderId := 0;
  scores := TCollections.CreateList<Currency>();
  while not(detailsDataSet.Eof) do
  begin
    orderId := detailsDataSet.FieldByName('OrderId').AsInteger;
    if orderId <> currentOrderId then
    begin
      if (totalOrderValue > 0) then
      begin
        scores.Add(totalOrderValue);
        totalOrderValue := 0;
      end;
      currentOrderId := orderId;
    end;
    itemTotal := GetDetailsItemTotal(detailsDataSet);
    totalOrderValue := totalOrderValue + itemTotal;
    detailsDataSet.Next;
  end;
  Result.OrderValues := scores.ToArray;
  Result.OrderCount := scores.Count;
end;

function TDataModule1.GetDetailsItemTotal(const aDetailsDataSet: TDataSet)
  : Currency;
var
  unitPrice: Currency;
  quantity: Integer;
  discount: Double;
begin
  unitPrice := aDetailsDataSet.FieldByName('UnitPrice').AsCurrency;
  quantity := aDetailsDataSet.FieldByName('Quantity').AsInteger;
  discount := aDetailsDataSet.FieldByName('Discount').AsFloat;
  Result := RoundTo(unitPrice * quantity * (1 - discount), -2);
end;

end.
