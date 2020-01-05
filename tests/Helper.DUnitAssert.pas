unit Helper.DUnitAssert;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  DUnitX.TestFramework;

type
  TAssertHelper = class helper for Assert
    class procedure AreMemosEqual(const actualStrings: string;
      const expectedStrings: string);
  end;

implementation

function FindDiffrence(const s1: string; const s2: string): integer;
var
  j: integer;
begin
  if s1=s2 then
     Exit(0);
  for j := 1 to Min(s1.Length,s2.Length) do
    if s1[j] <> s2[j] then
      Exit(j);
end;

class procedure TAssertHelper.AreMemosEqual(const actualStrings,
  expectedStrings: string);
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
    Assert.IsTrue(slExpected.Count = slActual.Count,
      Format('Diffrent number of lines, expected %d is not equal to actual %d',
      [slExpected.Count, slActual.Count]));
    for i := 0 to slExpected.Count - 1 do
      if slExpected[i] <> slActual[i] then
      begin
        aPos := FindDiffrence(slExpected[i], slActual[i]);
        Assert.Fail
          (Format('in line: %d at pos: %d, expected |%s| is not equal to actual |%s|',
          [i + 1, aPos, slExpected[i], slActual[i]]));
      end;
  finally
    slActual.Free;
    slExpected.Free;
  end;
end;

end.
