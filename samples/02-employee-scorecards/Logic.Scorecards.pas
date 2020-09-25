unit Logic.Scorecards;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.DateUtils,
  System.Math,
  Data.DB,
  Spring.Collections,
  {-}
  Data.DataModule1;

type
  TEmployeeScore = class
    fEmployeeId: Integer;
    fMonth: TDateTime;
    fEmployeeName: String;
    fOrderCount: Integer;
    fOrderValues: string;
    fMaxScore: Integer;
    fFinalScore: Integer;
  end;

  TScorecards = record
  private
    fYear: Integer;
    fMonth: Integer;
    fEmployeeScores: IDictionary<Integer, TEmployeeScore>;
    procedure FillUsingOrders(const dsOrders: TDataSet);
    procedure FillUsingDetails(const dsDetails: TDataSet);
    procedure FillUsingEmployee(const dsEmployee: TDataSet);
  public
    constructor Create(aYear: word; aMonth: word);
    function GenerateData(const aDataModule: TDataModule1)
        : IReadOnlyCollection<TEmployeeScore>;
  end;

implementation

procedure TScorecards.FillUsingOrders(const dsOrders: TDataSet);
var
  employeeId: Integer;
  employeeScore: TEmployeeScore;
begin
  dsOrders.First;
  while not dsOrders.Eof do
  begin
    employeeId := dsOrders.FieldByName('EmployeeID').AsInteger;
    if fEmployeeScores.TryGetValue(employeeId, employeeScore) then
    begin
      employeeScore.fOrderCount := employeeScore.fOrderCount + 1;
    end
    else
    begin
      employeeScore := TEmployeeScore.Create;
      employeeScore.fEmployeeId := employeeId;
      employeeScore.fOrderCount := 1;
      fEmployeeScores.Add(employeeId, employeeScore);
    end;
    dsOrders.Next;
  end;
end;

procedure TScorecards.FillUsingEmployee(const dsEmployee: TDataSet);
var
  employeeId: Integer;
  Scorecards: TEmployeeScore;
begin
  dsEmployee.First;
  while not dsEmployee.Eof do
  begin
    employeeId := dsEmployee.FieldByName('EmployeeId').AsInteger;
    if fEmployeeScores.TryGetValue(employeeId, Scorecards) then
      Scorecards.fEmployeeName := dsEmployee.FieldByName('FullName').AsString;
    dsEmployee.Next;
  end;
end;

function CountOrderScore(const aTotalOrderValue: Currency): Integer;
begin
  Result := 0;
end;

procedure TScorecards.FillUsingDetails(const dsDetails: TDataSet);
var
  employeeId: Integer;
  currentOrderID: Integer;
  unitPrice: Currency;
  quantity: Integer;
  discount: Double;
  orderID: Integer;
  strOrderValues: String;
  itemTotal: Currency;
  totalOrderValue: Currency;
  score: TEmployeeScore;
begin
  for employeeId in fEmployeeScores.Keys do
  begin
    dsDetails.Locate('EmployeeId', employeeId, []);
    currentOrderID := 0;
    totalOrderValue := 0;
    strOrderValues := '';
    while not(dsDetails.Eof) and
        (dsDetails.FieldByName('EmployeeId').AsInteger = employeeId) do
    begin
      orderID := dsDetails.FieldByName('OrderId').AsInteger;
      unitPrice := dsDetails.FieldByName('UnitPrice').AsCurrency;
      quantity := dsDetails.FieldByName('Quantity').AsInteger;
      discount := dsDetails.FieldByName('Discount').AsFloat;
      itemTotal := RoundTo(unitPrice * quantity * (1 - discount), -2);
      if orderID <> currentOrderID then
      begin
        if (totalOrderValue > 0) then
        begin
          strOrderValues := strOrderValues + '| ' + CurrToStrF(totalOrderValue,
              ffFixed, 2);
          // CountOrderScore(totalOrderValue);
          totalOrderValue := 0;
        end;
        currentOrderID := orderID;
      end;
      totalOrderValue := totalOrderValue + itemTotal;
      dsDetails.Next;
    end;
    if (Length(strOrderValues) > 0) and (fEmployeeScores.TryGetValue(employeeId, score))
    then
    begin
      score.fOrderValues := strOrderValues.Substring(2);
    end;
  end;
end;

constructor TScorecards.Create(aYear: word; aMonth: word);
begin
  fEmployeeScores := Spring.Collections.TCollections.CreateDictionary<Integer,
      TEmployeeScore>([doOwnsValues]);
  fYear := aYear;
  fMonth := aMonth;
end;

function TScorecards.GenerateData(const aDataModule: TDataModule1)
    : IReadOnlyCollection<TEmployeeScore>;
begin
  FillUsingOrders(aDataModule.GetDataSet_OrdersInMonth(fYear, fMonth));
  aDataModule.fdqEmployees.Open();
  FillUsingEmployee(aDataModule.fdqEmployees);
  FillUsingDetails(aDataModule.GetDataSet_DetailsInMonth(fYear, fMonth));
  Result := fEmployeeScores.Values;
end;

end.
