unit Comp.GenerateDataSetCode;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  TGenerateDataSetCode = class(TComponent)
  private
    FCode: TStrings;
    FDataSet: TDataSet;
    procedure Guard;
    function GenCodeLineFieldDefAdd(fld: TField): string;
    function GenCodeLineSetFieldValue(fld: TField): string;
    procedure GenCodeCreateMockTableWithStructure(dataSet: TDataSet);
    procedure GenCodeAppendDataToMockTable(dataSet: TDataSet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property Code: TStrings read FCode;
    class function GenerateAsString (ds: TDataSet): string;
    class function GenerateAsArray (ds: TDataSet): TStringDynArray;
  end;

implementation

uses
  System.Rtti;

constructor TGenerateDataSetCode.Create(AOwner: TComponent);
begin
  inherited;
  FCode := TStringList.Create;
end;

destructor TGenerateDataSetCode.Destroy;
begin
  FCode.Free;
  inherited;
end;

function FieldTypeToString(ft: TFieldType): string;
begin
  Result := System.Rtti.TRttiEnumerationType.GetName(ft);
end;

function TGenerateDataSetCode.GenCodeLineFieldDefAdd(fld: TField): string;
begin
   (*
  ---------------------------------------------------------------------------
  [Doc]
  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval, // 38..41
    ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream, //42..48
    ftTimeStampOffset, ftObject, ftSingle); //49..51
  ---------------------------------------------------------------------------
  *)
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
begin
  Result := DateToCode(dt);
  if Frac(dt) > 0 then
    Result := Result + '+' + TimeToCode(dt);
end;

function TGenerateDataSetCode.GenCodeLineSetFieldValue(fld: TField): string;
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

class function TGenerateDataSetCode.GenerateAsString(ds: TDataSet): string;
var
  gen: TGenerateDataSetCode;
begin
  gen := TGenerateDataSetCode.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := gen.Code.Text;
  finally
    gen.Free;
  end;
end;

class function TGenerateDataSetCode.GenerateAsArray(
  ds: TDataSet): TStringDynArray;
var
  gen: TGenerateDataSetCode;
begin
  gen := TGenerateDataSetCode.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := gen.Code.ToStringArray;
  finally
    gen.Free;
  end;
end;

procedure TGenerateDataSetCode.Guard;
begin
  Assert( DataSet<>nil, 'Property DataSet not assigned!');
end;

procedure TGenerateDataSetCode.GenCodeAppendDataToMockTable(dataSet: TDataSet);
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

procedure TGenerateDataSetCode.GenCodeCreateMockTableWithStructure(dataSet: TDataSet);
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

procedure TGenerateDataSetCode.Execute;
begin
  Guard;
  GenCodeCreateMockTableWithStructure(dataSet);
  GenCodeAppendDataToMockTable(dataSet);
end;

end.
