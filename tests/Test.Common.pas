unit Test.Common;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Comp.Generator.DataSetCode;

type
  TTestGenerate = class(TObject)
  private
  protected
    GenerateDataSetCode: TGenerateDataSetCode;
    mockDataSet: TFDMemTable;
    function ReplaceArrowsAndDiamonds(const s: String): string;
    procedure Common_Setup;
    procedure Common_TearDown;
  end;


implementation

function TTestGenerate.ReplaceArrowsAndDiamonds(const s: String): string;
begin
  Result := StringReplace(s, '→', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '◇', GenerateDataSetCode.IndentationText,
    [rfReplaceAll])
end;

procedure TTestGenerate.Common_Setup;
begin
  GenerateDataSetCode := TGenerateDataSetCode.Create(nil);
  mockDataSet := TFDMemTable.Create(nil);
end;

procedure TTestGenerate.Common_TearDown;
begin
  FreeAndNil(GenerateDataSetCode);
  FreeAndNil(mockDataSet);
end;

end.
