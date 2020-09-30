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
    procedure Wrap_3Lines;
    procedure Wrap_4Lines;
    procedure Wrap_LongContinousText;
    [Test]
    [TestCase('Dot','.')]
    [TestCase('Comma',',','_')]
    [TestCase('ExclamationMark','!')]
    [TestCase('QuestionMark','?')]
    [TestCase('Semicolon',';')]
    [TestCase('Colon',':')]
    [TestCase('Hyphen','-')]
    procedure Wrap_SeparatedBy(const separator: char);
  end;

implementation

uses
  Comp.Generator.DataSetCode;

procedure AssertStringArrays(const expected: TArray<string>;
  const actual: TArray<string>);
var
  i: Integer;
begin
  Assert.AreEqual(Length(expected), Length(actual), 'Incorrect array sizes');
  for i := 0 to Length(actual) - 1 do
    Assert.AreEqual(expected[i], actual[i], Format('Line %d', [i + 1]));
end;

// ---------1---------2---------3---------4---------5---------6
// 123456789012345678901234567890123456789012345678901234567890
// Lorem ipsum dolor sit amet, consectetur adipiscing elit.

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
  AssertStringArrays(['Lorem ipsum dolor ', 'sit amet,'], actual);
end;

procedure TTestTextWrapper.Wrap_3Lines;
var
  actual: TArray<string>;
begin
  fText := 'Lorem ipsum dolor sit amet,';
  actual := TTextWrapper.WrapTextWholeWords(fText, 13);
  AssertStringArrays(['Lorem ipsum ', 'dolor sit ', 'amet,'], actual);
end;

procedure TTestTextWrapper.Wrap_4Lines;
var
  actual: TArray<string>;
begin
  fText := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit.';
  actual := TTextWrapper.WrapTextWholeWords(fText, 20);
  AssertStringArrays(['Lorem ipsum dolor ', 'sit amet, ', 'consectetur ',
    'adipiscing elit.'], actual);
end;

procedure TTestTextWrapper.Wrap_LongContinousText;
var
  actual: TArray<string>;
begin
  fText := 'LoremIpsumDolorSit';
  actual := TTextWrapper.WrapTextWholeWords(fText, 10);
  AssertStringArrays(['LoremIpsum', 'DolorSit'], actual);
end;

procedure TTestTextWrapper.Wrap_SeparatedBy(const separator: char);
var
  actual: TArray<string>;
begin
  fText := 'Lorem'+separator+'Ipsum';
  actual := TTextWrapper.WrapTextWholeWords(fText, 8);
  AssertStringArrays(['Lorem'+separator, 'Ipsum'], actual);
end;

end.
