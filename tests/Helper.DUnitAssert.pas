unit Helper.DUnitAssert;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  DUnitX.TestFramework;

type
  DUnitAssertHelper = class helper for Assert
  private
    class procedure DoAreMemosEqual(const expectedStrings: string;
      const actualStrings: string; isFullReport: boolean = False); static;
  public
    class procedure AreMemosEqual(const expectedStrings: string;
      const actualStrings: string); static;
    class procedure AreMemosEqual_FullReport(const expectedStrings,
      actualStrings: string); static;
  end;

implementation

function FindDiffrence(const s1: string; const s2: string): integer;
var
  j: integer;
begin
  if s1 = s2 then
    Exit(0);
  for j := 1 to Min(s1.Length, s2.Length) do
    if s1[j] <> s2[j] then
      Exit(j);
  Result := Min(s1.Length, s2.Length);
end;

class procedure DUnitAssertHelper.DoAreMemosEqual(const expectedStrings: string;
  const actualStrings: string; isFullReport: boolean = False);
var
  slActual: TStringList;
  slExpected: TStringList;
  i: integer;
  aPos: integer;
begin
  slActual := TStringList.Create;
  slExpected := TStringList.Create;
  try
    slActual.Text := actualStrings;
    slExpected.Text := expectedStrings;
    if slExpected.Count <> slActual.Count then
    begin
      if isFullReport then
        Assert.AreEqual(slExpected.Count, slActual.Count,
          {} '(diffrent number of lines)' + sLineBreak +
          {}'[ expected ]' + sLineBreak +
          {} slExpected.Text +
          {}'[ actual ]' + sLineBreak +
          {} slActual.Text)
      else
        Assert.AreEqual(slExpected.Count, slActual.Count,
          '(diffrent number of lines)');
    end;
    for i := 0 to slExpected.Count - 1 do
      if slExpected[i] <> slActual[i] then
      begin
        aPos := FindDiffrence(slExpected[i], slActual[i]);
        Assert.Fail(Format('in line: %d at pos: %d, expected |%s| is not equal'
          + ' to actual |%s|', [i + 1, aPos, slExpected[i], slActual[i]]));
      end;
      Assert.Pass;
  finally
    slActual.Free;
    slExpected.Free;
  end;
end;

class procedure DUnitAssertHelper.AreMemosEqual(const expectedStrings: string;
  const actualStrings: string);
begin
  DoAreMemosEqual(expectedStrings, actualStrings, False);
end;

class procedure DUnitAssertHelper.AreMemosEqual_FullReport(const expectedStrings
  : string; const actualStrings: string);
begin
  DoAreMemosEqual(expectedStrings, actualStrings, True);
end;

end.
