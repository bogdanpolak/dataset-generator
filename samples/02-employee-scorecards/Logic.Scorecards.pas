unit Logic.Scorecards;

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  Spring.Collections,
  {-}
  Data.DataModule1;

type
  TEmployeeScore = class

  end;

  TScorecards = record
  private
    fYear: Integer;
    fMonth: Integer;
  public
    fEmployeeScores: IList<TEmployeeScore>;
    constructor Create(aYear: word; aMonth: word);
  end;

implementation


constructor TScorecards.Create(aYear: word; aMonth: word);
begin
  fEmployeeScores := Spring.Collections.TCollections.CreateList<TEmployeeScore>( True);
  fYear := aYear;
  fMonth := aMonth;
end;

end.
