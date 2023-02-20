unit Scorecards.Utils;

interface

function Iff(
  aCondition: boolean;
  const aTrueResult, aFalseResult: string): string; overload;
function Iff(
  aCondition: boolean;
  const aTrueResult, aFalseResult: integer): integer; overload;

type
  TUtils = class
    class function Iff<T>(
      aCondition: boolean;
      const aTrueResult, aFalseResult: T): T; overload;
    class function IsIntArrayContains(
      const aArray: TArray<integer>;
      const aValue: integer): boolean; static;
  end;

implementation

function Iff(
  aCondition: boolean;
  const aTrueResult, aFalseResult: string): string;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

function Iff(
  aCondition: boolean;
  const aTrueResult, aFalseResult: integer): integer; overload;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

{ TUtils }

class function TUtils.Iff<T>(
  aCondition: boolean;
  const aTrueResult, aFalseResult: T): T;
begin
  if aCondition then
    Result := aTrueResult
  else
    Result := aFalseResult;
end;

class function TUtils.IsIntArrayContains(
  const aArray: TArray<integer>;
  const aValue: integer): boolean;
var
  idx: integer;
begin
  for idx := 0 to High(aArray) do
    if aArray[idx] = aValue then
      Exit(true);
  Result := False;
end;

end.
