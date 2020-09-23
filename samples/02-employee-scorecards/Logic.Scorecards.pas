unit Logic.Scorecards;

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  Spring.Collections,
  Data.DB,
  {-}
  Data.DataModule1;

type
  TEmployeeScore = class
    fEmployeeId: Integer;
    fMonth: TDateTime;
    fEmployeeName: String;
    fOrderCount: Integer;
    fMaxScore: Integer;
    fFinalScore: Integer;
  end;

  TScorecards = record
  private
    fYear: Integer;
    fMonth: Integer;
    procedure FillUsingOrders(const dsOrders: TDataSet);
    procedure FillUsingDetails(const dsDetails: TDataSet);
  public
    fEmployeeScores: IDictionary<Integer, TEmployeeScore>;
    constructor Create(aYear: word; aMonth: word);
  end;

implementation

type
  TDataSetHelper = class helper for TDataSet
  public
    procedure WhileNotEof(proc: TProc);
  end;

procedure TDataSetHelper.WhileNotEof(proc: TProc);
var
  Bookmark: TBookmark;
begin
  Bookmark := self.GetBookmark;
  try
  finally
    if self.BookmarkValid(Bookmark) then
      self.GotoBookmark(Bookmark);
    self.FreeBookmark(Bookmark);
  end;
end;

procedure TScorecards.FillUsingOrders(const dsOrders: TDataSet);
var
  employeeId: Integer;
  employeeScore: TEmployeeScore;
begin
  dsOrders.First;
  while not dsOrders.Eof do
  begin
    employeeId := dsOrders.FieldByName('EmployeeID').AsInteger;
    if fEmployeeScores.TryGetValue( employeeId, employeeScore) then
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

procedure TScorecards.FillUsingDetails(const dsDetails: TDataSet);
begin

end;

constructor TScorecards.Create(aYear: word; aMonth: word);
begin
  fEmployeeScores := Spring.Collections.TCollections.CreateDictionary<Integer,
      TEmployeeScore>([doOwnsValues]);
  fYear := aYear;
  fMonth := aMonth;
  FillUsingOrders(DataModule1.GetDataSet_OrdersInMonth(aYear,aMonth));
  FillUsingDetails(DataModule1.GetDataSet_DetailsInMonth(aYear,aMonth));
end;

end.
