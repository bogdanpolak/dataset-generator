unit Action.CreateMemTable;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  TCreateMemTableAction = class(TComponent)
  private
    FCode: TStrings;
    function GenCodeLineFieldDefAdd(fld: TField): string;
    function GenCodeLineSetFieldValue(fld: TField): string;
    procedure GenCodeCreateMockTableWithStructure(dataSet: TDataSet);
    procedure GenCodeAppendDataToMockTable(dataSet: TDataSet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateFDMemTable(dataSet: TDataSet): TFDMemTable;
    procedure GenerateCode(dataSet: TDataSet);
    property Code: TStrings read FCode write FCode;
  end;

implementation

uses
  System.Rtti;

constructor TCreateMemTableAction.Create(AOwner: TComponent);
begin
  inherited;
  FCode := TStringList.Create;
end;

destructor TCreateMemTableAction.Destroy;
begin
  FCode.Free;
  inherited;
end;

function TCreateMemTableAction.CreateFDMemTable(dataSet: TDataSet): TFDMemTable;
var
  fld: TField;
begin
  dataSet.Open;
  Result := TFDMemTable.Create(Self);
  for fld in dataSet.Fields do
  begin
    if fld.DataType in [ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint,
      ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime] then
      Result.FieldDefs.Add(fld.FieldName, fld.DataType)
    else if (fld.DataType in [ftString, ftWideString]) and (fld.Size > 9999)
    then
      Result.FieldDefs.Add(fld.FieldName, fld.DataType, 100)
    else if (fld.DataType in [ftString, ftWideString]) then
      Result.FieldDefs.Add(fld.FieldName, fld.DataType, fld.Size)
    else
      Result.FieldDefs.Add(fld.FieldName, fld.DataType, fld.DataSize);
  end;
  Result.CreateDataSet;
  dataSet.DisableControls;
  dataSet.Open;
  dataSet.First;
  while not dataSet.Eof do
  begin
    Result.Insert;
    for fld in dataSet.Fields do
    begin
      Result.FieldByName(fld.FieldName).Value :=
        dataSet.FieldByName(fld.FieldName).Value;
    end;
    Result.Post;
    dataSet.Next;
  end;
  Result.First;
  dataSet.EnableControls;
end;

function FieldTypeToString(ft: TFieldType): string;
begin
  Result := System.Rtti.TRttiEnumerationType.GetName(ft);
end;

function TCreateMemTableAction.GenCodeLineFieldDefAdd(fld: TField): string;
begin
  if fld.DataType in [ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint,
    ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime] then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ');'
  else if (fld.DataType in [ftString, ftWideString]) and (fld.Size > 9999) then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', 100);'
  else if (fld.DataType in [ftString, ftWideString]) then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', ' + fld.Size.ToString + ');'
  else
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', ' + fld.Size.ToString + ');';
end;

function FloatToCode(val: Extended): string;
begin
  Result := FloatToStr(val);
  Result := StringReplace(Result, ',', '.', []);
end;

function DateToCode(dt: TDateTime): string;
var
  y, m, d: word;
begin
  DecodeDate(dt, y, m, d);
  Result := Format('EncodeDate(%d,%d,%d)', [y, m, d]);
end;

function TimeToCode(dt: TDateTime): string;
var
  h, min, s, ms: word;
begin
  DecodeTime(dt, h, min, s, ms);
  Result := Format('EncodeTime(%d,%d,%d,%d)', [h, min, s, ms]);
end;

function DateTimeToCode(dt: TDateTime): string;
var
  h, min, s, ms: word;
begin
  Result := DateToCode(dt);
  if Frac(dt) > 0 then
    Result := Result + '+' + TimeToCode(dt);
end;

function TCreateMemTableAction.GenCodeLineSetFieldValue(fld: TField): string;
var
  sByNameValue: string;
begin
  Result := '';
  if not(fld.IsNull) then
  begin
    sByNameValue := 'FieldByName(' + QuotedStr(fld.FieldName) + ').Value';
    case fld.DataType of
      ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
        Result := sByNameValue + ' := ' + fld.AsString + ';';
      ftBoolean:
        Result := sByNameValue + ' := ' + BoolToStr(fld.AsBoolean, true) + ';';
      ftFloat, ftCurrency:
        Result := sByNameValue + ' := ' + FloatToCode(fld.AsExtended) + ';';
      ftDate:
        Result := sByNameValue + ' := ' + DateToCode(fld.AsDateTime) + ';';
      ftTime:
        Result := sByNameValue + ' := ' + TimeToCode(fld.AsDateTime) + ';';
      ftDateTime:
        Result := sByNameValue + ' := ' + DateTimeToCode(fld.AsDateTime) + ';';
      ftString, ftWideString:
        Result := sByNameValue + ' := ' + QuotedStr(fld.Value) + ';';
    end;
  end;
end;

procedure TCreateMemTableAction.GenCodeAppendDataToMockTable(dataSet: TDataSet);
var
  fld: TField;
  s1: string;
begin
  dataSet.DisableControls;
  dataSet.Open;
  dataSet.First;
  Code.Add('with ds do');
  Code.Add('begin');
  while not dataSet.Eof do
  begin
    with Code do
    begin
      Add('  Append;');
      for fld in dataSet.Fields do
      begin
        s1 := GenCodeLineSetFieldValue(fld);
        if s1 <> '' then
          Add('    ' + s1);
      end;
      Add('  Post;');
    end;
    dataSet.Next;
  end;
  Code.Add('end;');
  dataSet.EnableControls;
end;

procedure TCreateMemTableAction.GenCodeCreateMockTableWithStructure(dataSet: TDataSet);
var
  fld: TField;
begin
  with Code do
  begin
    Clear;
    Add('ds := TFDMemTable.Create(AOwner);');
    Add('with ds do');
    Add('begin');
    for fld in dataSet.Fields do
      Add('  ' + GenCodeLineFieldDefAdd(fld));
    Add('  CreateDataSet;');
    Add('end;');
  end;
end;

procedure TCreateMemTableAction.GenerateCode(dataSet: TDataSet);
begin
  GenCodeCreateMockTableWithStructure(dataSet);
  GenCodeAppendDataToMockTable(dataSet);
end;

end.
