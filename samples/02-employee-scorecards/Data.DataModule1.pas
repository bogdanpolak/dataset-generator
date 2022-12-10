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

  TEmployeeScore = class
    EmployeeId: Integer;
    OrderCount: Integer;
    TotalOrdersSum: Currency;
    ScoreValue: Currency;
    Stars: Integer;
    constructor Create();
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
    procedure CalculateMonthlyScore(
      const aEmployeeId: Integer;
      const aYear: Word;
      const aMonth: Word;
      const score: TEmployeeScore);
  private
    dsPromotions: TDataSet;
    function GetDetailsItemTotal(const aDetailsDataSet: TDataSet): Currency;
    function PromotionModifier(
      const aYear, aMonth: Word;
      const aDetailsDataSet: TDataSet): double;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  DataSet.Promotions,
  Scorecards.Utils;

constructor TDataModule1.Create(aOwner: TComponent);
begin
  inherited;
  Assert(FDConnection1.Connected = False,
    'Error! Connection to database was active before opening');
  dsPromotions := CreatePromotionsDataSet(self);
end;

procedure TDataModule1.Connect();
begin
  FDConnection1.Open();
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

procedure TDataModule1.CalculateMonthlyScore(
  const aEmployeeId: Integer;
  const aYear: Word;
  const aMonth: Word;
  const score: TEmployeeScore);
var
  detailsDataSet: TDataSet;
  orderId: Integer;
  orderTotal: Currency;
  orderScore: Currency;
  modifier: double;
  total: Currency;
  totalScore: Currency;
  orders: Integer;
begin
  detailsDataSet := GetDataSet_DetailsInMonth(aEmployeeId, aYear, aMonth);
  orders := 0;
  total := 0;
  totalScore := 0;
  while not(detailsDataSet.Eof) do
  begin
    orderId := detailsDataSet.FieldByName('OrderId').AsInteger;
    orderTotal := 0;
    orderScore := 0;
    while not(detailsDataSet.Eof) and
      (orderId = detailsDataSet.FieldByName('OrderId').AsInteger) do
    begin
      modifier := PromotionModifier(aYear, aMonth, detailsDataSet);
      orderTotal := orderTotal + GetDetailsItemTotal(detailsDataSet);
      orderScore := orderScore + modifier * GetDetailsItemTotal(detailsDataSet);
      detailsDataSet.Next;
    end;
    inc(orders);
    total := total + orderTotal;
    totalScore := totalScore + orderScore;
  end;
  with score do begin
    EmployeeId := aEmployeeId;
    OrderCount := orders;
    TotalOrdersSum := total;
    ScoreValue := totalScore;
    Stars := -1;
  end;
end;

function TDataModule1.GetDetailsItemTotal(const aDetailsDataSet: TDataSet)
  : Currency;
var
  unitPrice: Currency;
  quantity: Integer;
  discount: double;
begin
  unitPrice := aDetailsDataSet.FieldByName('UnitPrice').AsCurrency;
  quantity := aDetailsDataSet.FieldByName('Quantity').AsInteger;
  discount := aDetailsDataSet.FieldByName('Discount').AsFloat;
  Result := RoundTo(unitPrice * quantity * (1 - discount), -2);
end;

function ExtractCategoryIds(const aPromotedCategories: string): TArray<Integer>;
var
  arr: TArray<string>;
  idx: Integer;
begin
  if aPromotedCategories = '' then
    Exit(nil);
  arr := aPromotedCategories.Split([',']);
  SetLength(Result, Length(arr));
  for idx := 0 to High(arr) do
    Result[idx] := StrToInt(arr[idx]);
end;

function TDataModule1.PromotionModifier(
  const aYear: Word;
  const aMonth: Word;
  const aDetailsDataSet: TDataSet): double;
var
  categoryId: Integer;
  hasPromotion: boolean;
  promotedCategories: string;
  promoCategoryIds: TArray<Integer>;
begin
  hasPromotion := dsPromotions.Locate('Year;Month',
    VarArrayOf([aYear, aMonth]), []);
  if hasPromotion then
  begin
    promotedCategories := dsPromotions.FieldByName('CategoryIds').AsString;
    promoCategoryIds := ExtractCategoryIds(promotedCategories)
  end
  else
    promoCategoryIds := nil;
  categoryId := aDetailsDataSet.FieldByName('CategoryId').AsInteger;
  if TUtils.IsIntArrayContains(promoCategoryIds,categoryId) then
    Result := 2
  else
    Result := 1;
end;

{ TEmployeeScore }

constructor TEmployeeScore.Create;
begin
  Stars := -1;
end;

end.
