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
  end;

implementation

uses
  Comp.Generator.DataSetCode;

procedure TTestTextWrapper.NoWrapText;
var
  actual: TArray<string>;
begin
  fText := 'Lorem ipsum ...';
  actual := TTextWrapper.WrapTextWholeWords(fText,20);
  Assert.AreEqual(1,Length(actual));
  Assert.AreEqual(fText,actual[0]);
end;

end.
