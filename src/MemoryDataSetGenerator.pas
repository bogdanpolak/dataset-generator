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

  TInternalGenerator = class
    class function GenerateFieldDefAdd(
      const fld: TField;
      const aIndentation: string): string;
    class function GenerateFieldByName(
      fld: TField;
      const aIndentation: string;
      const aRightMargin: integer;
      out line: string): boolean; static;
    class function GetDataFieldPrecision(fld: TField): integer; static;
    class function FormatLongStringLiteral(
      const aLiteral: string;
      const aFistLineStartAt: integer;
      const aRightMargin: integer;
      const aIndentation: string): string;
    class function GenerateDataBlock(
      const aDataSet: TDataSet;
      const aAppendMode: TAppendMode;
      const aMaxRows: integer;
      const aRightMargin: integer;
      const aIndentation: string): string; static;
    class function GenerateFunction(
      const aDataSet: TDataSet;
      const aDataSetType: TDataSetType;
      const aAppendMode: TAppendMode;
      const aMaxRows: integer;
      const aRightMargin: integer;
      const aIndentation: string): string; static;
    class function GenerateStructure(
      const aDataSet: TDataSet;
      const aDataSetType: TDataSetType;
      const aIndentation: string): string;
    class function GenerateOneAppend(
      const aAppendMode: TAppendMode;
      const aFields: TFields;
      const aIndentation: string;
      const aRightMargin: integer): string;
    class function GenerateOneAppend_Multiline(
      const aFields: TFields;
      const aIndentation: string;
      const aRightMargin: integer): string; static;
    class function GenerateSingleLine_ValuesArray(const aFields: TFields)
      : string; static;
    class function GenerateUnitFooter: string; static;
    class function GenerateUnitHeader(
      const aDataSetType: TDataSetType;
      const aNameOfUnit: string;
      const aIndentation: string): string; static;
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

function TDSGenerator.GenerateAll(aMode: TGeneratorMode): string;
begin
  case aMode of
    genStructure:
      Result := TInternalGenerator.GenerateStructure(fDataSet, fDataSetType,
        fIndentationText);
    genAppend:
      Result := IfThen(fDataSet = nil, '',
        TInternalGenerator.GenerateDataBlock(fDataSet, fAppendMode, fMaxRows,
        fRightMargin, fIndentationText));
    genUnit:
      Result := TInternalGenerator.GenerateUnitHeader(fDataSetType, fNameOfUnit,
        fIndentationText) + TInternalGenerator.GenerateFunction(fDataSet,
        fDataSetType, fAppendMode, fMaxRows, fRightMargin, fIndentationText) +
        TInternalGenerator.GenerateUnitFooter();
    genFunction:
      Result := TInternalGenerator.GenerateFunction(fDataSet, fDataSetType,
        fAppendMode, fMaxRows, fRightMargin, fIndentationText);
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
    Clipboard.AsText := TInternalGenerator.GenerateFunction(aGenerator.DataSet,
      aGenerator.DataSetType, aGenerator.AppendMode, aGenerator.MaxRows,
      aGenerator.RightMargin, aGenerator.IndentationText);
  finally
    aGenerator.Free;
  end;
end;

{ TFieldGenerator }

class function TInternalGenerator.GenerateFieldDefAdd(
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

class function TInternalGenerator.GetDataFieldPrecision(fld: TField): integer;
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

class function TInternalGenerator.GenerateFieldByName(
  fld: TField;
  const aIndentation: string;
  const aRightMargin: integer;
  out line: string): boolean;
var
  linePattern: string;
  value: string;
begin
  Result := False;
  line := '';
  if fld.IsNull then
    exit;
  linePattern := aIndentation + 'ds.FieldByName(' + QuotedStr(fld.FieldName) +
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
      value := FormatLongStringLiteral(fld.value, Length(linePattern) - 2,
        aRightMargin, aIndentation);
  else
    value := '';
  end;
  if value = '' then
    exit;
  line := Format(linePattern, [value]);
  Result := true;
end;

class function TInternalGenerator.FormatLongStringLiteral(
  const aLiteral: string;
  const aFistLineStartAt: integer;
  const aRightMargin: integer;
  const aIndentation: string): string;
var
  s: string;
  lines: TArray<string>;
  i: integer;
  sb: TStringBuilder;
begin
  s := QuotedStr(aLiteral);
  if aFistLineStartAt + Length(s) <= aRightMargin then
  begin
    Result := QuotedStr(aLiteral);
  end
  else
  begin
    lines := TTextWrapper.WrapTextWholeWords(s,
      aRightMargin - 2 * Length(aIndentation) - 1);
    sb := TStringBuilder.Create;
    try
      sb.Append(sLineBreak);
      for i := 0 to High(lines) do
        sb.Append(DupeString(aIndentation, 2) + IfThen(i > 0, '''') + lines[i] +
          IfThen(i < High(lines), '''+' + sLineBreak, ''));
      Result := sb.ToString();
    finally
      sb.Free;
    end;
  end
end;

class function TInternalGenerator.GenerateOneAppend(
  const aAppendMode: TAppendMode;
  const aFields: TFields;
  const aIndentation: string;
  const aRightMargin: integer): string;
begin
  case aAppendMode of
    amMultilineAppends:
      Result := GenerateOneAppend_Multiline(aFields, aIndentation,
        aRightMargin);
    amSinglelineAppends:
      Result := aIndentation + 'ds.AppendRecord(' +
        GenerateSingleLine_ValuesArray(aFields) + ');' + sLineBreak;
  else
    Result := '';
  end;
end;

class function TInternalGenerator.GenerateOneAppend_Multiline(
  const aFields: TFields;
  const aIndentation: string;
  const aRightMargin: integer): string;
var
  fld: TField;
  line: string;
  sl: TStringList;
begin

  sl := TStringList.Create;
  try
    sl.Add(aIndentation + 'ds.Append;');
    for fld in aFields do
    begin
      if TInternalGenerator.GenerateFieldByName(fld, aIndentation, aRightMargin,
        line) then
        sl.Add(line);
    end;
    sl.Add(aIndentation + 'ds.Post;');
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

class function TInternalGenerator.GenerateSingleLine_ValuesArray
  (const aFields: TFields): string;
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

class function TInternalGenerator.GenerateDataBlock(
  const aDataSet: TDataSet;
  const aAppendMode: TAppendMode;
  const aMaxRows: integer;
  const aRightMargin: integer;
  const aIndentation: string): string;
var
  sb: TStringBuilder;
  bookmark: TBookmark;
  rowCounter: integer;
  valuesArray: string;
  dataAppendCode: string;
begin
  if (aDataSet = nil) or (aDataSet.Fields.Count = 0) then
    exit('');
  if aMaxRows = 0 then
    rowCounter := MaxInt
  else
    rowCounter := aMaxRows;
  sb := TStringBuilder.Create();
  try
    aDataSet.DisableControls;
    try
      aDataSet.Active := true;
      bookmark := aDataSet.GetBookmark;
      try
        aDataSet.First;
        if aAppendMode = amAppendRows then
        begin
          sb.Append(aIndentation + 'ds.AppendRows([' + sLineBreak);
          while not aDataSet.Eof and (rowCounter > 0) do
          begin
            valuesArray := GenerateSingleLine_ValuesArray(aDataSet.Fields);
            aDataSet.Next;
            sb.Append(DupeString(aIndentation, 2) + valuesArray +
              IfThen(not aDataSet.Eof, ',') + sLineBreak);
            dec(rowCounter);
          end;
          sb.Append(aIndentation + ']);' + sLineBreak);
        end
        else
        begin
          while not aDataSet.Eof and (rowCounter > 0) do
          begin
            sb.Append(GenerateOneAppend(aAppendMode, aDataSet.Fields,
              aIndentation, aRightMargin));
            dec(rowCounter);
            aDataSet.Next;
          end;
        end;
      finally
        aDataSet.GotoBookmark(bookmark);
        aDataSet.FreeBookmark(bookmark);
      end;
    finally
      aDataSet.EnableControls;
    end;
    dataAppendCode := sb.ToString;
  finally
    sb.Free;
  end;

  Result :=
  { } dataAppendCode +
  { } aIndentation + 'ds.First;' + sLineBreak;
end;

class function TInternalGenerator.GenerateUnitHeader(
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

class function TInternalGenerator.GenerateFunction(
  const aDataSet: TDataSet;
  const aDataSetType: TDataSetType;
  const aAppendMode: TAppendMode;
  const aMaxRows: integer;
  const aRightMargin: integer;
  const aIndentation: string): string;
var
  aClassName: string;
begin
  case aDataSetType of
    dstFDMemTable:
      aClassName := 'TFDMemTable';
    dstClientDataSet:
      aClassName := 'TClientDataSet';
  end;
  Result :=
  { } 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  { } 'var' + sLineBreak +
  { } '  ds: ' + aClassName + ';' + sLineBreak +
  { } 'begin' + sLineBreak +
  { } GenerateStructure(aDataSet, aDataSetType, aIndentation) +
  { } GenerateDataBlock(aDataSet, aAppendMode, aMaxRows, aRightMargin,
    aIndentation) +
  { } '  Result := ds;' + sLineBreak +
  { } 'end;' + sLineBreak;
end;

class function TInternalGenerator.GenerateStructure(
  const aDataSet: TDataSet;
  const aDataSetType: TDataSetType;
  const aIndentation: string): string;
var
  fld: TField;
  dataSetCreateCode: string;
  fieldDefinitions: string;
begin
  case aDataSetType of
    dstFDMemTable:
      dataSetCreateCode := 'TFDMemTable.Create(AOwner)';
    dstClientDataSet:
      dataSetCreateCode := 'TClientDataSet.Create(AOwner)';
  end;
  fieldDefinitions := '';
  if (aDataSet <> nil) then
    for fld in aDataSet.Fields do
      fieldDefinitions := fieldDefinitions + DupeString(aIndentation, 2) +
        GenerateFieldDefAdd(fld, aIndentation) + sLineBreak;
  Result :=
  { } aIndentation + 'ds := ' + dataSetCreateCode + ';' + sLineBreak +
  { } aIndentation + 'with ds do' + sLineBreak +
  { } aIndentation + 'begin' + sLineBreak +
  { } fieldDefinitions +
  { } DupeString(aIndentation, 2) + 'CreateDataSet;' + sLineBreak +
  { } aIndentation + 'end;' + sLineBreak
end;

class function TInternalGenerator.GenerateUnitFooter(): string;
begin
  Result := sLineBreak + 'end.' + sLineBreak;
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
