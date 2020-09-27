unit Test.TextWrapper;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils;

{$M+}

type

  [TestFixture]
  TTestTextWrapper = class(TObject)
  private
    fText: string;
  public
  published
    procedure NoWrapText;
    procedure Wrap_2Lines;
  end;

implementation

uses
  Comp.Generator.DataSetCode;

//  ---------1---------2---------3---------4---------5---------6
//  123456789012345678901234567890123456789012345678901234567890
//  Lorem ipsum dolor sit amet, consectetur adipiscing elit.

procedure TTestTextWrapper.NoWrapText;
var
  actual: TArray<string>;
begin
  fText := 'Lorem ipsum ...';
  actual := TTextWrapper.WrapTextWholeWords(fText, 20);
  Assert.AreEqual(1, Length(actual));
  Assert.AreEqual(fText, actual[0]);
end;

procedure TTestTextWrapper.Wrap_2Lines;
var
  actual: TArray<string>;
begin
  fText := 'Lorem ipsum dolor sit amet,';
  actual := TTextWrapper.WrapTextWholeWords(fText, 20);
  Assert.AreEqual(2, Length(actual));
  Assert.AreEqual('Lorem ipsum dolor ', actual[0]);
  Assert.AreEqual('sit amet,', actual[1]);
end;

end.
