unit Action.CreateMemTable;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  TCreateMemTableAction = class (TComponent)
  private
    FCode: TStrings;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy;
    function CreateFDMemTable (dataSet: TDataSet): TFDMemTable;
    procedure GenerateCode (dataSet: TDataSet);
    property Code: TStrings read FCode write FCode;
  end;

implementation


constructor TCreateMemTableAction.Create(AOwner: TComponent);
begin
  inherited;
  FCode := TStringList.Create;
end;

destructor TCreateMemTableAction.Destroy;
begin
  FCode.Free;
end;

function TCreateMemTableAction.CreateFDMemTable (dataSet:TDataSet): TFDMemTable;
begin
  Result := TFDMemTable.Create(Self);
  with Result do
  begin
    FieldDefs.Add('id', ftInteger);
    FieldDefs.Add('text1', ftWideString, 30);
    CreateDataSet;
    InsertRecord([1, 'Ala ma kota']);
  end;
end;

procedure TCreateMemTableAction.GenerateCode(dataSet: TDataSet);
var
  sl: TStringList;
begin
  With Code do begin
    Clear;
    Add('ds := TFDMemTable.Create(AOwner);');
    Add('with ds do');
    Add('begin');
    Add('  FieldDefs.Add(''id'', ftInteger);');
    Add('  FieldDefs.Add(''text1'', ftWideString, 30);');
    Add('  CreateDataSet;');
    Add('end;');
  end;
end;

end.
