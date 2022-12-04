{ * ------------------------------------------------------------------------
  * ♥
  * ♥ Memory DataSet Generator
  * ♥ creates ClientDataSet or FDMemTable with the data
  * ♥
  * Component: TDSGenerator
  * Project: https://github.com/bogdanpolak/dataset-generator
  * ------------------------------------------------------------------------ }

unit MemoryDataSetGenerator;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  TGeneratorMode = (genStructure, genAppend, genFunction, genUnit);
  TDataSetType = (dstFDMemTable, dstClientDataSet);
  TAppendMode = (amMultilineAppends, amSinglelineAppends, amAppendRows);

  TDSGenerator = class(TComponent)
  public const
    Version = '1.5';
  private
    fCode: TStrings;
    fDataSet: TDataSet;
    fIndentationText: String;
    fGeneratorMode: TGeneratorMode;
    fDataSetType: TDataSetType;
    fAppendMode: TAppendMode;
    fNameOfUnit: string;
    fMaxRows: integer;
    fRightMargin: integer;
    function GenerateAll(aMode: TGeneratorMode): string;
    class function GenetateUnit(
      ds: TDataSet;
      const aUnitName: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    class function GenerateAsString(ds: TDataSet): string;
    class function GenerateAsArray(ds: TDataSet): TArray<String>;
    class procedure GenerateAndSaveToStream(
      ds: TDataSet;
      aStream: TStream);
    class procedure GenerateAndSaveToFile(
      ds: TDataSet;
      const aFileName: string);
    class procedure GenerateAndSaveClipboard(ds: TDataSet);
  published
    property DataSet: TDataSet read fDataSet write fDataSet;
    property Code: TStrings read fCode;
    property IndentationText: String read fIndentationText
      write fIndentationText;
    property GeneratorMode: TGeneratorMode read fGeneratorMode
      write fGeneratorMode;
    property DataSetType: TDataSetType read fDataSetType write fDataSetType;
    property AppendMode: TAppendMode read fAppendMode write fAppendMode;
    property NameOfUnit: string read fNameOfUnit write fNameOfUnit;
    property MaxRows: integer read fMaxRows write fMaxRows;
    property RightMargin: integer read fRightMargin write fRightMargin;
  end;

  TCodeSegmentsGenerator = class
  public
    class function GenerateFunction(
      const aDataSet: TDataSet;
      const aDataSetType: TDataSetType;
      const aAppendMode: TAppendMode;
      const aRightMargin: integer;
      const aIndentation: string;
      const aMaxRows: integer = 10): string;
    class function GenerateUnitHeader(
      const aDataSetType: TDataSetType;
      const aNameOfUnit: string;
      const aIndentation: string): string; static;
    class function GenerateUnitFooter: string; static;
  end;

  TStructureBlockGenerator = class
  public
    class function Generate(
      const aDataSet: TDataSet;
      const aIndentation: string): string;
  private
    class function GetDataFieldPrecision(fld: TField): integer; static;
    class function GenerateFieldDefAdd(
      const fld: TField;
      const aIndentation: string): string;
  end;

  TDataBlockGenerator = class
  private
    fAppendMode: TAppendMode;
    fRightMargin: integer;
    fIndentation: string;
    fMaxRows: integer;
  public
    constructor Create(
      const aAppendMode: TAppendMode;
      const aRightMargin: integer;
      const aIndentation: string;
      const aMaxRows: integer);
    class function Generate(
      const aDataSet: TDataSet;
      const aAppendMode: TAppendMode;
      const aRightMargin: integer;
      const aIndentation: string;
      const aMaxRows: integer = 10): string;
  private
    function DoGenerateDataBlock(const aDataSet: TDataSet): string;
    function GenerateDataWithManyAppends(const aDataSet: TDataSet): string;
    function GenerateDataWithAppendRows(const aDataSet: TDataSet): string;
    function GenerateOneMultilineAppend(const aDataSet: TDataSet): string;
    function TryGenerateFieldByName(
      const fld: TField;
      out line: string): boolean;
    function FormatLongStringLiteral(
      const aLiteral: string;
      const aFistLineStartAt: integer): string;
    function GenerateArrayWithValues(const aFields: TFields): string;
  end;

  TTextWrapper = class
    class function WrapTextWholeWords(
      const aText: string;
      aMaxWidth: integer): TArray<string>;
  end;

implementation

uses
  System.StrUtils,
  System.Rtti,
  Vcl.Clipbrd;

constructor TDSGenerator.Create(AOwner: TComponent);
begin
  inherited;
  // --------------------------------
  // Default options
  fGeneratorMode := genFunction;
  fDataSetType := dstFDMemTable;
  fAppendMode := amMultilineAppends;
  fIndentationText := '  ';
  fNameOfUnit := 'uSampleDataSet';
  fMaxRows := 100;
  fRightMargin := 76;
  // --------------------------------
  fCode := TStringList.Create;
end;

destructor TDSGenerator.Destroy;
begin
  fCode.Free;
  inherited;
end;

function TDSGenerator.GenerateAll(aMode: TGeneratorMode): string;
begin
  case aMode of
    genStructure:
      Result := TStructureBlockGenerator.Generate(fDataSet, fIndentationText);
    genAppend:
      Result := IfThen(fDataSet = nil, '',
        TDataBlockGenerator.Generate(fDataSet, fAppendMode, fRightMargin,
        fIndentationText, fMaxRows));
    genUnit:
      Result := TCodeSegmentsGenerator.GenerateUnitHeader(fDataSetType,
        fNameOfUnit, fIndentationText) + TCodeSegmentsGenerator.GenerateFunction
        (fDataSet, fDataSetType, fAppendMode, fRightMargin, fIndentationText,
        fMaxRows) + TCodeSegmentsGenerator.GenerateUnitFooter();
    genFunction:
      Result := TCodeSegmentsGenerator.GenerateFunction(fDataSet, fDataSetType,
        fAppendMode, fRightMargin, fIndentationText, fMaxRows);
  else
    Result := '// Unsupported generator mode';
  end;
end;

procedure TDSGenerator.Execute;
begin
  fCode.Text := GenerateAll(fGeneratorMode);
end;

class function TDSGenerator.GenerateAsString(ds: TDataSet): string;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := gen.Code.Text;
  finally
    gen.Free;
  end;
end;

function TStringsToArray(sl: TStrings): TArray<String>;
var
  i: integer;
begin
  SetLength(Result, sl.Count);
  for i := 0 to sl.Count - 1 do
    Result[i] := sl[i];
end;

class function TDSGenerator.GenerateAsArray(ds: TDataSet): TArray<String>;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := TStringsToArray(gen.Code);
  finally
    gen.Free;
  end;
end;

class function TDSGenerator.GenetateUnit(
  ds: TDataSet;
  const aUnitName: string): string;
var
  aGenerator: TDSGenerator;
begin
  aGenerator := TDSGenerator.Create(nil);
  try
    aGenerator.DataSet := ds;
    aGenerator.NameOfUnit := aUnitName;
    Result := aGenerator.GenerateAll(genUnit);
  finally
    aGenerator.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveToStream(
  ds: TDataSet;
  aStream: TStream);
var
  sCode: Utf8String;
begin
  sCode := Utf8String(GenetateUnit(ds, 'uSampleDataSet'));
  aStream.Write(sCode[1], Length(sCode));
end;

function GetUnitName_FromFilePath(aFilePath: string): string;
var
  aUnitName: string;
  aExtentionLen: integer;
begin
  aUnitName := ExtractFileName(aFilePath);
  aExtentionLen := Length(ExtractFileExt(aFilePath));
  if aExtentionLen > 0 then
    Result := aUnitName.Substring(0, aUnitName.Length - aExtentionLen)
  else
    Result := aUnitName;
end;

class procedure TDSGenerator.GenerateAndSaveToFile(
  ds: TDataSet;
  const aFileName: string);
var
  fs: TFileStream;
  sCode: Utf8String;
  aUnitName: string;
begin
  aUnitName := GetUnitName_FromFilePath(aFileName);
  sCode := Utf8String(GenetateUnit(ds, aUnitName));
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    {
      aFilePreamble := TEncoding.UTF8.GetPreamble;
      aStream.Write(aFilePreamble[0], Length(aFilePreamble));
    }
    fs.Write(sCode[1], Length(sCode));
  finally
    fs.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveClipboard(ds: TDataSet);
var
  aGenerator: TDSGenerator;
begin
  aGenerator := TDSGenerator.Create(nil);
  try
    aGenerator.DataSet := ds;
    Clipboard.AsText := TCodeSegmentsGenerator.GenerateFunction
      (aGenerator.DataSet, aGenerator.DataSetType, aGenerator.AppendMode,
      aGenerator.RightMargin, aGenerator.IndentationText, aGenerator.MaxRows);
  finally
    aGenerator.Free;
  end;
end;


{ TCodeSegmentsGenerator }

class function TCodeSegmentsGenerator.GenerateUnitHeader(
  const aDataSetType: TDataSetType;
  const aNameOfUnit: string;
  const aIndentation: string): string;
var
  dataSetUnits: string;
begin
  case aDataSetType of
    dstFDMemTable:
      dataSetUnits := aIndentation + 'FireDAC.Comp.Client;';
    dstClientDataSet:
      dataSetUnits :=
      { } aIndentation + 'Datasnap.DBClient;'#13#10 +
      { } aIndentation + 'MidasLib;';
  end;
  Result :=
  { } 'unit ' + aNameOfUnit + ';' + sLineBreak +
  { } sLineBreak +
  { } 'interface' + sLineBreak +
  { } sLineBreak +
  { } 'uses' + sLineBreak +
  { } aIndentation + 'System.Classes,' + sLineBreak +
  { } aIndentation + 'System.SysUtils,' + sLineBreak +
  { } aIndentation + 'System.Variants,' + sLineBreak +
  { } aIndentation + 'Data.DB,' + sLineBreak +
  { } dataSetUnits + sLineBreak +
  { } sLineBreak +
  { } 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  { } sLineBreak +
  { } 'implementation' + sLineBreak +
  { } sLineBreak;
end;

class function TCodeSegmentsGenerator.GenerateFunction(
  const aDataSet: TDataSet;
  const aDataSetType: TDataSetType;
  const aAppendMode: TAppendMode;
  const aRightMargin: integer;
  const aIndentation: string;
  const aMaxRows: integer = 10): string;
var
  dsc: string;
  body: string;
begin
  dsc := IfThen(aDataSetType = dstFDMemTable, 'TFDMemTable', 'TClientDataSet');
  body := TStructureBlockGenerator.Generate(aDataSet, aIndentation) +
    TDataBlockGenerator.Generate(aDataSet, aAppendMode, aRightMargin,
    aIndentation, aMaxRows);
  Result :=
  { } 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  { } 'var' + sLineBreak +
  { } aIndentation + 'ds: ' + dsc + ';' + sLineBreak +
  { } 'begin' + sLineBreak +
  { } aIndentation + 'ds := ' + dsc + '.Create(aOwner);' + sLineBreak +
  { } body +
  { } aIndentation + 'Result := ds;' + sLineBreak +
  { } 'end;' + sLineBreak;
end;

class function TCodeSegmentsGenerator.GenerateUnitFooter(): string;
begin
  Result := sLineBreak + 'end.' + sLineBreak;
end;


{ TFieldGeneratorUtils }

function FieldTypeToString(ft: TFieldType): string;
begin
  Result := System.Rtti.TRttiEnumerationType.GetName(ft);
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


{ TStructureBlockGenerator }

class function TStructureBlockGenerator.Generate(
  const aDataSet: TDataSet;
  const aIndentation: string): string;
var
  fld: TField;
  fieldDefinitions: string;
begin
  fieldDefinitions := '';
  if (aDataSet <> nil) then
    for fld in aDataSet.Fields do
      fieldDefinitions := fieldDefinitions + DupeString(aIndentation, 2) +
        GenerateFieldDefAdd(fld, aIndentation) + sLineBreak;
  Result :=
  { } aIndentation + 'with ds do' + sLineBreak +
  { } aIndentation + 'begin' + sLineBreak +
  { } fieldDefinitions +
  { } DupeString(aIndentation, 2) + 'CreateDataSet;' + sLineBreak +
  { } aIndentation + 'end;' + sLineBreak
end;

class function TStructureBlockGenerator.GenerateFieldDefAdd(
  const fld: TField;
  const aIndentation: string): string;
begin
  (* -----------------------------------------------------------------------
    [Doc]
    TFieldType = ( ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd,
    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval,
    ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream,
    ftTimeStampOffset, ftObject, ftSingle);
    ------------------------------------------------------------------------- *)
  if fld.DataType in [ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint,
    ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime] then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ');'
  else if (fld.DataType in [ftBCD, ftFMTBcd]) then
    Result := 'with FieldDefs.AddFieldDef do begin' + sLineBreak +
      DupeString(aIndentation, 3) +
      Format('Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;',
      [fld.FieldName, FieldTypeToString(fld.DataType),
      GetDataFieldPrecision(fld), fld.Size]) + sLineBreak +
      DupeString(aIndentation, 2) + 'end;'
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

class function TStructureBlockGenerator.GetDataFieldPrecision
  (fld: TField): integer;
begin
  System.Assert((fld is TBCDField) or (fld is TFMTBCDField) or
    (fld is TFloatField));
  if fld is TBCDField then
    Result := (fld as TBCDField).Precision
  else if fld is TFMTBCDField then
    Result := (fld as TFMTBCDField).Precision
  else
    Result := (fld as TFloatField).Precision
end;

{ TDataBlockGenerator }

constructor TDataBlockGenerator.Create(
  const aAppendMode: TAppendMode;
  const aRightMargin: integer;
  const aIndentation: string;
  const aMaxRows: integer);
begin
  fAppendMode := aAppendMode;
  fRightMargin := aRightMargin;
  fIndentation := aIndentation;
  fMaxRows := aMaxRows;
end;

class function TDataBlockGenerator.Generate(
  const aDataSet: TDataSet;
  const aAppendMode: TAppendMode;
  const aRightMargin: integer;
  const aIndentation: string;
  const aMaxRows: integer = 10): string;
var
  dataBlockGenerator: TDataBlockGenerator;
begin
  dataBlockGenerator := TDataBlockGenerator.Create(aAppendMode, aRightMargin,
    aIndentation, aMaxRows);
  try
    Result := dataBlockGenerator.DoGenerateDataBlock(aDataSet);
  finally
    dataBlockGenerator.Free;
  end;
end;

function TDataBlockGenerator.DoGenerateDataBlock(const aDataSet
  : TDataSet): string;
var
  bookmark: TBookmark;
  dataCode: string;
begin
  if (aDataSet = nil) or (aDataSet.Fields.Count = 0) then
    exit('');
  aDataSet.DisableControls;
  try
    aDataSet.Active := true;
    bookmark := aDataSet.GetBookmark;
    try
      if fAppendMode = amAppendRows then
      begin
        dataCode := GenerateDataWithAppendRows(aDataSet);
      end
      else
      begin
        dataCode := GenerateDataWithManyAppends(aDataSet);
      end;
    finally
      aDataSet.GotoBookmark(bookmark);
      aDataSet.FreeBookmark(bookmark);
    end;
  finally
    aDataSet.EnableControls;
  end;
  Result := dataCode + fIndentation + 'ds.First;' + sLineBreak;
end;

function TDataBlockGenerator.GenerateDataWithManyAppends(const aDataSet
  : TDataSet): string;
var
  sb: TStringBuilder;
  rowCounter: integer;
begin
  aDataSet.First;
  sb := TStringBuilder.Create();
  rowCounter := fMaxRows;
  try
    while not aDataSet.Eof and (rowCounter > 0) do
    begin
      case fAppendMode of
        amMultilineAppends:
          sb.Append(GenerateOneMultilineAppend(aDataSet));
        amSinglelineAppends:
          sb.Append(fIndentation + 'ds.AppendRecord(' + GenerateArrayWithValues
            (aDataSet.Fields) + ');' + sLineBreak);
      end;
      dec(rowCounter);
      aDataSet.Next;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TDataBlockGenerator.GenerateDataWithAppendRows(const aDataSet
  : TDataSet): string;
var
  sb: TStringBuilder;
  rowCounter: integer;
  row: string;
  rows: string;
begin
  aDataSet.First;
  rowCounter := fMaxRows;
  sb := TStringBuilder.Create;
  try
    while not aDataSet.Eof and (rowCounter > 0) do
    begin
      row := GenerateArrayWithValues(aDataSet.Fields);
      aDataSet.Next;
      sb.Append(DupeString(fIndentation, 2) + row + IfThen(not aDataSet.Eof,
        ',') + sLineBreak);
      dec(rowCounter);
    end;
    rows := sb.ToString;
    Result := fIndentation + 'ds.AppendRows([' + sLineBreak + rows +
      fIndentation + ']);' + sLineBreak;
  finally
    sb.Free;
  end;
end;

function TDataBlockGenerator.GenerateOneMultilineAppend(const aDataSet
  : TDataSet): string;
var
  fld: TField;
  line: string;
begin
  Result := fIndentation + 'ds.Append;' + sLineBreak;
  for fld in aDataSet.Fields do
  begin
    if TryGenerateFieldByName(fld, line) then
      Result := Result + line + sLineBreak;
  end;
  Result := Result + fIndentation + 'ds.Post;' + sLineBreak;
end;

function TDataBlockGenerator.TryGenerateFieldByName(
  const fld: TField;
  out line: string): boolean;
var
  linePattern: string;
  value: string;
begin
  Result := False;
  line := '';
  if fld.IsNull then
    exit;
  linePattern := fIndentation + 'ds.FieldByName(' + QuotedStr(fld.FieldName) +
    ').Value := %s;';
  case fld.DataType of
    ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
      value := fld.AsString;
    ftBoolean:
      value := BoolToStr(fld.AsBoolean, true);
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      value := FloatToCode(fld.AsExtended);
    ftDate:
      value := DateToCode(fld.AsDateTime);
    ftTime:
      value := TimeToCode(fld.AsDateTime);
    ftDateTime:
      value := DateTimeToCode(fld.AsDateTime);
    ftString, ftWideString:
      value := FormatLongStringLiteral(fld.value, Length(linePattern) - 2);
  else
    value := '';
  end;
  if value = '' then
    exit;
  line := Format(linePattern, [value]);
  Result := true;
end;

function TDataBlockGenerator.FormatLongStringLiteral(
  const aLiteral: string;
  const aFistLineStartAt: integer): string;
var
  s: string;
  lines: TArray<string>;
  i: integer;
  sb: TStringBuilder;
begin
  s := QuotedStr(aLiteral);
  if aFistLineStartAt + Length(s) <= fRightMargin then
  begin
    Result := QuotedStr(aLiteral);
  end
  else
  begin
    lines := TTextWrapper.WrapTextWholeWords(s,
      fRightMargin - 2 * Length(fIndentation) - 1);
    sb := TStringBuilder.Create;
    try
      sb.Append(sLineBreak);
      for i := 0 to High(lines) do
        sb.Append(DupeString(fIndentation, 2) + IfThen(i > 0, '''') + lines[i] +
          IfThen(i < High(lines), '''+' + sLineBreak, ''));
      Result := sb.ToString();
    finally
      sb.Free;
    end;
  end
end;

function TDataBlockGenerator.GenerateArrayWithValues(const aFields
  : TFields): string;
var
  fld: TField;
  value: string;
begin
  Result := '';
  for fld in aFields do
  begin
    if fld.IsNull then
      value := 'Null'
    else
      case fld.DataType of
        ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
          value := fld.AsString;
        ftBoolean:
          value := BoolToStr(fld.AsBoolean, true);
        ftFloat, ftCurrency, ftBCD, ftFMTBcd:
          value := FloatToCode(fld.AsExtended);
        ftDate:
          value := DateToCode(fld.AsDateTime);
        ftTime:
          value := TimeToCode(fld.AsDateTime);
        ftDateTime:
          value := DateTimeToCode(fld.AsDateTime);
        ftString, ftWideString:
          value := QuotedStr(fld.value);
      else
        value := 'Null'
      end;
    Result := IfThen(Result = '', value, Result + ', ' + value);
  end;
  Result := '[' + Result + ']';
end;

{ TTextWrapper }

class function TTextWrapper.WrapTextWholeWords(
  const aText: string;
  aMaxWidth: integer): TArray<string>;
var
  i: integer;
  j: integer;
  Count: integer;
begin
  i := 0;
  j := aMaxWidth;
  Count := 0;
  Result := [];
  while j < aText.Length do
  begin
    while (j > i) and not(CharInSet(aText[j], [' ', '.', ',', '!', '?', ':',
      ';', '-'])) do
      dec(j);
    if j = i then
      j := i + aMaxWidth;
    SetLength(Result, Count + 1);
    Result[Count] := aText.Substring(i, j - i);
    i := j;
    j := j + aMaxWidth;
    Count := Count + 1;
  end;
  SetLength(Result, Count + 1);
  Result[Count] := aText.Substring(i);
end;

end.
