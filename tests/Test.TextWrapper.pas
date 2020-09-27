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
  public
  published
    procedure Case01;
  end;

implementation

procedure TTestTextWrapper.Case01;
begin
  Assert.Fail();
end;

end.
